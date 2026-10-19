//! Keeping history on the lattice (resid.rs, rules.rs `RESID_RULES`): run to the oracle's answer, then
//! a click on the root runs it back to the very tree loaded with nothing else left, and run again and
//! back the same. Forgetting what the applies drop: back the term with holes, any trees in them giving
//! the same answer. With the history erased as it is made, nothing left but the answer.

use tree_net::oracle::{self, f2, Fuel, Lcg, Term};
use tree_net::rules::Tag;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::resid::{self, Edit, Tree};

/// Random terms (applications all through), random trees applied to one or two random trees, and
/// tuples of random terms, each finishing under the oracle.
fn corpus(n: usize) -> Vec<(Term, Term)> {
    let mut rng = Lcg(20260730);
    let mut terms = vec![];
    for i in 0..n {
        let t = match i % 3 {
            0 => rng.rand_term(3 + (i % 4) as u32),
            1 => {
                let mut tree = |d: u32| loop { if let Ok(t) = oracle::nf(rng.rand_term(d), &mut Fuel(5_000)) { return t; } };
                let f = tree(3 + (i % 4) as u32);
                (0..1 + i % 2).fold(f, |g, j| oracle::ap(g, tree(2 + ((i + j) % 3) as u32)))
            }
            _ => f2(rng.rand_term(3), f2(rng.rand_term(4), Term::L)),
        };
        if let Ok(want) = oracle::nf(t.clone(), &mut Fuel(20_000)) { terms.push((t, want)); }
    }
    terms
}

fn params(keep: bool, seed: u64) -> Params { Params { w: 40, h: 40, depth: 6, seed, resid: if keep { 1 } else { 2 }, ..latest() } }

/// Fully lazy: no duplicator copies before it is asked, no S rule starts (b c) before it is read.
fn lazy(keep: bool, seed: u64) -> Params { Params { dn_room: 0, fork: false, ..params(keep, seed) } }

fn load(t: &Term, p: Params) -> Lattice {
    let (net, out, parts) = resid::net(t, p.resid == 1);
    let side = ((net.live_count() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut l = Lattice::load(Params { w: side, h: side, ..p }, net, out).expect("load");
    l.resid = parts;
    l
}

fn show(t: &Term) -> String { oracle::show(t) }

/// The agents left that are not a tree's or the root.
fn others(l: &Lattice) -> Vec<Tag> {
    l.shadow.agents.iter().flatten().map(|a| a.tag).filter(|t| !matches!(t, Tag::L | Tag::S | Tag::F | Tag::Out | Tag::Hole)).collect()
}

fn answer(l: &Lattice) -> Option<String> { l.read_answer().and_then(|a| a.plain()).map(|a| show(&a)) }

/// A click on the root, then the run on to rest.
fn click(l: &mut Lattice, e: Edit) -> bool {
    l.edit(e);
    l.run(l.stats.proposals + 40_000_000)
}

#[test]
fn forward_then_back() { round_trips(params) }

#[test]
fn forward_then_back_lazy() { round_trips(lazy) }

fn round_trips(params: fn(bool, u64) -> Params) {
    let (mut fwd, mut all, mut n) = (0u64, 0u64, 0);
    for (i, (t, want)) in corpus(90).iter().enumerate() {
        let mut l = load(t, params(true, i as u64 + 1));
        if i % 10 == 0 { l.check_every = 1; }
        assert!(l.run(20_000_000), "term {i}: no answer");
        assert_eq!(answer(&l), Some(show(want)), "term {i}: WRONG ANSWER for {}", show(t));
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        fwd += l.stats.fires;
        for round in 0..2 {
            assert!(click(&mut l, Edit::Back), "term {i}: not back: {}", show(t));
            assert_eq!(l.read_term().as_ref(), Some(&l.resid.term), "term {i}: not back as loaded: {}", show(t));
            l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
            l.check_invariants().unwrap_or_else(|e| panic!("term {i}: {e}"));
            assert!(others(&l).is_empty(), "term {i}: left over {:?}", others(&l));
            if round == 0 {
                assert!(click(&mut l, Edit::Go), "term {i}: no answer run again");
                assert_eq!(answer(&l), Some(show(want)), "term {i}: WRONG ANSWER run again");
            }
        }
        all += l.stats.fires;
        n += 1;
    }
    eprintln!("{n} terms: rewrites forward {fwd}, forward, back, again and back {all}");
}

#[test]
fn forgetting() {
    let (mut holes, mut agents) = (0, 0);
    for (i, (t, want)) in corpus(60).iter().enumerate() {
        let mut l = load(t, params(true, i as u64 + 1));
        l.edit(Edit::Forget(true));
        assert!(l.run(20_000_000), "term {i}: no answer");
        assert_eq!(answer(&l), Some(show(want)), "term {i}: WRONG ANSWER forgetting");
        assert!(click(&mut l, Edit::Back), "term {i}: not back forgetting: {}", show(t));
        let got = l.read_term().unwrap();
        assert!(got.within(&l.resid.term), "term {i}: back {} is not the term with holes", got.show());
        assert!(others(&l).is_empty(), "term {i}: left over {:?}", others(&l));
        for hole in [Term::L, Term::S(Term::L.into())] {
            assert_eq!(oracle::nf(got.unquote(&hole), &mut Fuel(1_000_000)).ok().map(|a| show(&a)), Some(show(want)), "term {i}: the holes matter");
        }
        l.edit(Edit::Forget(false));
        assert!(click(&mut l, Edit::Go), "term {i}: no answer from the holes");
        assert_eq!(answer(&l), Some(show(want)), "term {i}: WRONG ANSWER from the holes");
        assert!(click(&mut l, Edit::Back), "term {i}: not back again");
        assert_eq!(l.read_term().as_ref(), Some(&got), "term {i}: back again not the same holes");
        holes += got.holes();
        agents += size(&l.resid.term);
    }
    eprintln!("forgetting: {holes} holes over {agents} agents of the terms as loaded");
}

fn size(t: &Tree) -> usize {
    match t { Tree::L | Tree::Hole => 1, Tree::S(a) => 1 + size(a), Tree::F(a, b) => 1 + size(a) + size(b) }
}

#[test]
fn history_erased() {
    for (i, (t, want)) in corpus(60).iter().enumerate() {
        let mut l = load(t, params(false, i as u64 + 1));
        assert!(l.run(20_000_000), "term {i}: no answer");
        assert_eq!(l.readback().map(|a| show(&a)), Some(show(want)), "term {i}: WRONG ANSWER");
        let stop = l.stats.proposals + 4_000_000;
        while l.stats.proposals < stop && !others(&l).is_empty() { l.run_on(l.stats.proposals + 50_000); }
        assert!(others(&l).is_empty(), "term {i}: left over {:?}", others(&l));
    }
}
