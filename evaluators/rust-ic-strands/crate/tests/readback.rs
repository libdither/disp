//! Reading the net back keeps its meaning: at any clock of a run, the root's term, reduced by the
//! independent oracle, is the final answer; and a picked computation, followed through rewrites
//! and through hand-overs that renumber the net (as from a GPU), still reduces to what it did.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ca_lattice::rules::Tag;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::readback::{self, Fate, Reader, Selection, NOWHERE};
use std::collections::HashMap;
use std::rc::Rc;

fn load(t: &Term, p: Params) -> Option<Lattice> {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    Lattice::load(p, net, out).ok()
}

fn nf(t: Term) -> Option<String> { oracle::nf(t, &mut Fuel(1_000_000)).ok().map(|t| oracle::show(&t)) }

/// Corpus terms that take a while, and two that share a lot.
fn terms() -> Vec<(String, Term)> {
    let mut rng = Lcg(20260730);
    let mut v: Vec<(String, Term)> = (0..60).map(|i| (format!("corpus {i}"), rng.rand_term(4 + i % 3))).collect();
    for (name, n) in [("share-tower", 3), ("s-rule", 0), ("disp-t", 0), ("convoy", 3)] {
        v.push((name.into(), rust_ic_mesh::term::workload(name, n).unwrap()));
    }
    v
}

fn p() -> Params { Params { w: 40, h: 40, depth: 8, ..latest() } }

/// The readback text read again: shared parts are bound where first read.
fn parse(s: &str) -> Option<Term> {
    fn go(b: &[char], i: &mut usize, labels: &mut HashMap<String, Term>) -> Option<Term> {
        let c = *b.get(*i)?;
        *i += 1;
        let eat = |i: &mut usize, want: char| (b.get(*i) == Some(&want)).then(|| *i += 1);
        Some(match c {
            'L' => Term::L,
            'S' => { eat(i, '(')?; let x = go(b, i, labels)?; eat(i, ')')?; Term::S(Rc::new(x)) }
            'F' | '@' => {
                eat(i, '(')?;
                let x = go(b, i, labels)?;
                eat(i, ',')?;
                let y = go(b, i, labels)?;
                eat(i, ')')?;
                if c == 'F' { Term::F(Rc::new(x), Rc::new(y)) } else { Term::Ap(Rc::new(x), Rc::new(y)) }
            }
            '#' => {
                let start = *i;
                while b.get(*i).is_some_and(|c| c.is_ascii_digit()) { *i += 1; }
                let n: String = b[start..*i].iter().collect();
                if eat(i, '=').is_some() {
                    let x = go(b, i, labels)?;
                    labels.insert(n, x.clone());
                    x
                } else {
                    labels.get(&n)?.clone()
                }
            }
            _ => return None,
        })
    }
    let b: Vec<char> = s.chars().collect();
    let mut i = 0;
    let t = go(&b, &mut i, &mut HashMap::new())?;
    (i == b.len()).then_some(t)
}

fn out_id(l: &Lattice) -> u32 { let (s, k) = l.find_out().unwrap(); l.sids[s as usize * l.ks + k] }

#[test]
fn the_root_reads_back_as_the_answer_all_along() {
    let (mut runs, mut pending) = (0, 0);
    for (name, t) in terms() {
        let Some(want) = nf(t.clone()) else { continue };
        let Some(mut l) = load(&t, p()) else { continue };
        runs += 1;
        for clock in 0.. {
            if clock % 12 == 0 || l.readback().is_some() {
                let r = Reader::new(&l.shadow).meaning(out_id(&l));
                let got = readback::term(&r).unwrap_or_else(|| panic!("{name}, clock {clock}: the root does not read"));
                assert_eq!(nf(got).as_ref(), Some(&want), "{name}, clock {clock}: the root means something else");
                let (text, at) = readback::text(&r, usize::MAX);
                assert_eq!(parse(&text).and_then(nf).as_ref(), Some(&want), "{name}, clock {clock}: {text}");
                assert_eq!(at.len(), text.chars().filter(|c| "LSF@<~#…?".contains(*c)).count(), "{name}: a node with no agent");
                // Every node is computed by an agent of the root's subtree, which holds no garbage.
                let sub = readback::subtree(&l.shadow, out_id(&l));
                assert!(at.iter().all(|a| *a == NOWHERE || sub.contains(a)), "{name}, clock {clock}: a node outside the subtree");
                let seat = readback::seats(&l);
                let mut marks = vec![];
                l.garbage(&mut marks);
                assert!(sub.iter().all(|&id| seat[id as usize] != NOWHERE && marks[seat[id as usize] as usize] == 0), "{name}: garbage in the root's subtree");
                if l.readback().is_some() { break; }
                pending += 1;
            }
            l.chip_clock();
            assert!(clock < 100_000, "{name} did not finish");
        }
        // A budget cuts the term short, deepest parts first.
        let r = Reader::new(&l.shadow).meaning(out_id(&l));
        let (short, _) = readback::text(&r, 3);
        assert!(short.chars().filter(|c| "LSF".contains(*c)).count() <= 3, "{name}: {short}");
    }
    assert!(runs >= 40 && pending >= 200, "only {runs} runs and {pending} looks before the answer");
}

/// The run handed to a fresh lattice, as a GPU hands it back: the same state, every agent renumbered.
fn hand_back(l: &Lattice, t: &Term) -> Lattice {
    let mut b = load(t, p()).unwrap();
    b.put_sites(&l.held_sites());
    b.stats = l.stats.clone();
    b
}

/// Pick, one selection each, every computation and value whose meaning the oracle can finish,
/// then look again every `gap` clocks, handing the run back every `every` looks. Whatever a pick
/// follows must reduce to what it did. Returns the meanings checked, the roots found again after
/// a hand-back, and what became of the roots that moved on.
fn follow_checked(gap: usize, every: usize) -> (usize, usize, HashMap<Fate, usize>) {
    let mut fates: HashMap<Fate, usize> = HashMap::new();
    let (mut checked, mut across) = (0, 0);
    for (name, t) in terms() {
        let Some(mut l) = load(&t, p()) else { continue };
        for _ in 0..20 { l.chip_clock(); }
        if l.readback().is_some() { continue; }
        let mut picks: Vec<(Selection, String)> = vec![];
        for id in 0..l.shadow.agents.len() as u32 {
            let Some(a) = &l.shadow.agents[id as usize] else { continue };
            if matches!(a.tag, Tag::Out | Tag::Eps | Tag::Unp | Tag::Pair) { continue; }
            let Some(want) = readback::term(&Reader::new(&l.shadow).meaning(id)).and_then(nf) else { continue };
            let mut s = Selection::default();
            assert!(s.toggle(&l.shadow, id));
            s.follow(&l, false, true);
            picks.push((s, want));
        }
        for round in 0..60 {
            for _ in 0..gap { l.chip_clock(); }
            let renumbered = round % every == every - 1;
            if renumbered { l = hand_back(&l, &t); }
            for (s, want) in &mut picks {
                let before = s.roots.len();
                for e in s.follow(&l, renumbered, true) { *fates.entry(e.fate).or_insert(0) += 1; }
                if renumbered { across += s.roots.len().min(before); }
                for r in &s.roots {
                    let Some(got) = readback::term(&r.meaning(&l.shadow)).and_then(nf) else { continue };
                    assert_eq!(&got, want, "{name}, round {round}: a picked {:?} means something else", r.tag);
                    checked += 1;
                }
            }
            if l.readback().is_some() { break; }
        }
    }
    (checked, across, fates)
}

#[test]
fn picked_computations_keep_their_meaning() {
    let (checked, across, fates) = follow_checked(5, 4);
    let n = |f| fates.get(&f).copied().unwrap_or(0);
    assert!(checked > 2000 && across > 200, "{checked} meanings checked, {across} roots found again after a hand-back");
    assert!(n(Fate::Rewritten) > 20 && n(Fate::Forced) > 5 && n(Fate::Consumed) > 20, "{fates:?}");
    // Handed back after every 40 clocks, as a GPU's stretches are: nothing found again means something else.
    let (checked, across, _) = follow_checked(40, 1);
    assert!(checked > 500 && across > 500, "{checked} meanings checked, {across} roots found again after a hand-back");
}

/// After a hand-back, most picks are found again as the same agents a CPU run that kept its
/// numbering follows, on terms of a few hundred agents.
#[test]
fn picks_are_found_again_after_a_hand_back() {
    let mut rng = Lcg(77);
    let (mut runs, mut same, mut missed, mut other) = (0, 0, 0, 0);
    for _ in 0..400 {
        let t = rng.rand_term(9);
        let Some(mut a) = load(&t, p()) else { continue };
        for _ in 0..100 { a.chip_clock(); }
        if a.readback().is_some() || a.shadow.live_count() < 100 { continue; }
        runs += 1;
        let mut b = hand_back(&a, &t);
        let seat = readback::seats(&a);
        let (mut cpu, mut gpu) = (vec![], vec![]);
        for id in (0..a.shadow.agents.len() as u32).filter(|&id| a.shadow.agents[id as usize].as_ref().is_some_and(|x| x.tag != Tag::Eps)) {
            let (mut c, mut g) = (Selection::default(), Selection::default());
            c.toggle(&a.shadow, id);
            g.toggle(&b.shadow, b.sids[seat[id as usize] as usize]);
            g.follow(&b, false, true);
            cpu.push(c);
            gpu.push(g);
        }
        for _ in 0..4 {
            for _ in 0..50 { a.chip_clock(); }
            b = hand_back(&a, &t);
            let seat = readback::seats(&a);
            for (c, g) in cpu.iter_mut().zip(&mut gpu) {
                c.follow(&a, false, false);
                g.follow(&b, true, true);
                let want: Vec<u32> = c.roots.iter().map(|r| b.sids[seat[r.id as usize] as usize]).collect();
                for r in &g.roots { if want.contains(&r.id) { same += 1 } else { other += 1 } }
                missed += want.iter().filter(|w| !g.roots.iter().any(|r| r.id == **w)).count();
                // Go on from the truth, so one miss does not count again.
                *g = Selection::default();
                for &w in &want { g.toggle(&b.shadow, w); }
                g.follow(&b, false, true);
            }
            if a.readback().is_some() { break; }
        }
        if runs == 4 { break; }
    }
    assert!(runs == 4 && same > 4 * (missed + other), "{runs} runs: {same} found again, {missed} missed, {other} others");
}

/// The wiring handed to the player's drawings is the abstract net's: every agent on the lattice once,
/// in its seat, each wire seen the same from both ends, and an agent keeps its id as it moves.
#[test]
fn the_wiring_is_the_nets() {
    for (name, t) in terms().into_iter().take(12) {
        let Some(mut l) = load(&t, p()) else { continue };
        let mut was: HashMap<u32, u32> = HashMap::new();
        for clock in 0..300 {
            let w = readback::wires(&l);
            let rec: HashMap<u32, &[u32]> = w.chunks(6).map(|r| (r[0], r)).collect();
            assert_eq!(rec.len() * 6, w.len(), "{name}: an agent twice");
            assert_eq!(rec.len(), l.shadow.live_count(), "{name}, clock {clock}: agents missing");
            for r in rec.values() {
                assert_eq!(l.sids[r[1] as usize], r[0], "{name}: not in its seat");
                assert_eq!(l.tags[r[1] as usize] as u32, r[2], "{name}: another tag");
                for (q, &far) in r[3..].iter().enumerate() {
                    if far == NOWHERE { continue; }
                    assert_eq!(rec[&(far / 4)][3 + (far % 4) as usize], r[0] * 4 + q as u32, "{name}: a wire seen differently from its ends");
                }
            }
            // Ids already seen still hold agents of the same kind, unless a rewrite used them up.
            for r in rec.values() { if let Some(&tag) = was.get(&r[0]) { assert_eq!(tag, r[2], "{name}: an id changed hands"); } }
            was = rec.values().map(|r| (r[0], r[2])).collect();
            if l.readback().is_some() { break; }
            l.chip_clock();
        }
    }
}
