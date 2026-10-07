//! A superposed run against its universes run one at a time. For each line `name|term` of a file
//! (the term in `term::parse_sup`'s syntax, or `fib:&1{2,3}`): rewrites on the abstract net to
//! quiescence, and clocks and rewrites to the answer on the lattice (the current design, lattices
//! sized as the player sizes them), for the superposed term and for each of its distinct universes
//! alone; every answer checked against the oracle universe by universe. Also where the lattice
//! run forks: the rewrites before the first rule that makes two computations of one reading a
//! superposition (A·Sup, T1·Sup, Sel·Sup), how many fire, and how many superpositions are copied
//! (Dn·Sup of another label).
//! A line may end in `|want`, the answer searched for (a term, or `bits:n` for any number tree of
//! value n, programs.disp's binary numbers): the universes giving it are listed, as a search over
//! superposed candidates reads them off.
//!   strands-sup <file> [--seeds N] [--no-lattice] [--clocks MAX]

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Term};
use rust_ca_lattice::rules::{all_rules, rule, Tag, When, N_RULES, RULES};
use rust_ca_lattice::sup::{self, STerm};
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};

/// Rewrites to quiescence on the abstract net, those of each rule, and the answer.
fn abstract_run(t: &STerm) -> (u64, Vec<u64>, Option<STerm>) {
    let mut n = Net::new();
    let root = sup::build(&mut n, t);
    let (_, out) = n.drive(root);
    let mut fired = vec![0u64; N_RULES];
    n.reduce_listed(u64::MAX, |r| fired[all_rules().position(|x| std::ptr::eq(x, r)).unwrap()] += 1);
    let answer = sup::read(&n, n.get(out).ports[0]);
    (n.ints, fired, answer)
}

/// Whether a rule forks: makes two computations of one reading a superposition.
fn forks(i: usize) -> bool { let r = rule(i); r.producer == Tag::Sup && matches!(r.consumer, Tag::A | Tag::T1 | Tag::Sel) }
/// Whether a rule copies a superposition (a duplicator of another label).
fn copies(i: usize) -> bool { let r = rule(i); r.producer == Tag::Sup && r.consumer == Tag::Dn && r.when == When::Differ }

struct Run { clocks: f64, fires: u64, first_fork: Option<u64>, forks: u64, copies: u64, side: u32 }

/// The lattice run to the answer: the current design on a lattice sized as the player sizes it
/// (side ⌈√agents · 7⌉ + 16, 8 layers, ×1.4 until the drawing fits).
fn lattice_run(t: &STerm, want: &[(u32, Term)], seed: u64, max_clocks: f64, trace: bool) -> Result<Run, String> {
    let agents = rust_ic_strands::share::net_sup(t, 0).0.live_count();
    let mut side = ((agents as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut l = None;
    for _ in 0..6 {
        let (net, out) = rust_ic_strands::share::net_sup(t, 0);
        match Lattice::load(Params { w: side, h: side, seed, ..latest() }, net, out) {
            Ok(x) => { l = Some(x); break; }
            Err(_) => side = (side as f64 * 1.4).ceil() as u32,
        }
    }
    let mut l = l.ok_or("does not fit")?;
    if trace { l.trace_start(); }
    while !l.answered() && l.stats.clocks < max_clocks {
        let next = l.stats.proposals + 20 * l.live().len().max(1) as u64;
        l.run(next);
    }
    let answer = l.read_answer().ok_or_else(|| format!("unfinished at {} clocks", l.stats.clocks))?;
    sup::check(t, want, &answer)?;
    let fork: Vec<u64> = l.trace.fires.iter().enumerate().filter(|(_, f)| forks(f.1)).map(|(i, _)| i as u64).collect();
    let copied = l.trace.fires.iter().filter(|f| copies(f.1)).count() as u64;
    Ok(Run { clocks: l.stats.clocks, fires: l.stats.fires, first_fork: fork.first().copied(), forks: fork.len() as u64, copies: copied, side })
}

/// A number tree's value (programs.disp's binary numbers): b0 = L, b1 = S(L), and a pair its low
/// half then its high half.
fn bits(t: &Term) -> Option<u128> {
    fn go(t: &Term) -> Option<(u128, u32)> {
        match t {
            Term::L => Some((0, 1)),
            Term::S(x) if **x == Term::L => Some((1, 1)),
            Term::F(lo, hi) => { let (a, w) = go(lo)?; let (b, v) = go(hi)?; (w + v <= 120).then(|| (a + (b << w), w + v)) }
            _ => None,
        }
    }
    go(t).map(|x| x.0)
}

fn k(x: f64) -> String { if x >= 9_950.0 { format!("{:.1}k", x / 1000.0) } else { format!("{x:.0}") } }

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (mut file, mut seeds, mut lattice, mut max_clocks) = (None, 2u64, true, 3_000_000f64);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--seeds" => seeds = it.next().unwrap().parse().unwrap(),
            "--no-lattice" => lattice = false,
            "--clocks" => max_clocks = it.next().unwrap().parse().unwrap(),
            _ => file = Some(a.clone()),
        }
    }
    let text = std::fs::read_to_string(file.expect("a file of name|term lines")).expect("read the file");
    for line in text.lines().map(str::trim).filter(|l| !l.is_empty() && !l.starts_with('#')) {
        let (name, rest) = line.split_once('|').expect("name|term");
        let (src, wanted) = rest.split_once('|').map_or((rest, None), |(s, w)| (s, Some(w)));
        let t = match src.split_once(':') {
            Some((w, arg)) if term::workload_sup(w, arg).is_some() => term::workload_sup(w, arg).unwrap(),
            _ => term::parse_sup(src),
        }.unwrap_or_else(|e| panic!("{name}: {e}"));
        let labels = t.labels();
        let want = sup::oracle_answers(&t, 2_000_000_000).unwrap_or_else(|| panic!("{name}: the oracle ran out of fuel"));
        let mut distinct: Vec<Term> = vec![];
        for (_, u) in t.universes(&labels) { if !distinct.contains(&u) { distinct.push(u); } }
        let t0 = std::time::Instant::now();
        let (ints, fired, answer) = abstract_run(&t);
        sup::check(&t, &want, &answer.expect("an answer")).unwrap_or_else(|e| panic!("{name}: abstract net: {e}"));
        let each: Vec<u64> = distinct.iter().map(|u| abstract_run(&u.into()).0).collect();
        let sum: u64 = each.iter().sum();
        let sup_rules: u64 = fired[RULES.len()..].iter().sum();
        let (nforks, ncopies): (u64, u64) = ((0..N_RULES).filter(|&i| forks(i)).map(|i| fired[i]).sum(), (0..N_RULES).filter(|&i| copies(i)).map(|i| fired[i]).sum());
        let answers = sup::collapse(&want, &labels);
        let shown = sup::show(&answers);
        println!("{name}: {} universes, {} distinct terms; answers {}", 1u32 << labels.len(), distinct.len(), if shown.len() > 100 { format!("{}…", &shown[..100]) } else { shown });
        println!("  abstract net: {ints} rewrites superposed ({sup_rules} by superposition rules: {nforks} forks, {ncopies} copies), {sum} for the terms one by one ({}), shared {:.0}% of the smallest",
            each.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" + "),
            100.0 * (sum as f64 - ints as f64) / (*each.iter().min().unwrap() as f64));
        if let Some(w) = wanted {
            let hit = |t: &Term| match w.strip_prefix("bits:") {
                Some(n) => bits(t).is_some_and(|v| v.to_string() == n),
                None => *t == term::parse(w).expect("a wanted term"),
            };
            let found: Vec<String> = want.iter().filter(|(_, t)| hit(t))
                .map(|(u, _)| labels.iter().enumerate().map(|(i, l)| format!("&{l}={}", u >> i & 1)).collect::<Vec<_>>().join(" ")).collect();
            let terms: Vec<String> = t.universes(&labels).iter().zip(&want).filter(|(_, (_, a))| hit(a)).map(|((_, u), _)| oracle::show(u)).collect::<std::collections::BTreeSet<_>>().into_iter().collect();
            println!("  wanted {w} in {} of {} universes: {}", found.len(), want.len(), if found.len() <= 8 { found.join(", ") } else { format!("{}, …", found[..8].join(", ")) });
            for u in terms { println!("    {}", if u.len() > 160 { format!("{}…", &u[..160]) } else { u }); }
            if want.iter().all(|(_, t)| bits(t).is_some()) {
                println!("  as numbers: {}", want.iter().map(|(u, t)| format!("{u}: {}", bits(t).unwrap())).collect::<Vec<_>>().join(", "));
            }
        }
        if !lattice { continue; }
        for seed in 1..=seeds {
            let s = lattice_run(&t, &want, seed, max_clocks, true).unwrap_or_else(|e| panic!("{name}: lattice seed {seed}: {e}"));
            let alone: Vec<Run> = distinct.iter().map(|u| {
                let u: STerm = u.into();
                let w = sup::oracle_answers(&u, 2_000_000_000).unwrap();
                lattice_run(&u, &w, seed, max_clocks, false).unwrap_or_else(|e| panic!("{name}: lattice seed {seed}, alone: {e}"))
            }).collect();
            let (csum, cmax) = (alone.iter().map(|r| r.clocks).sum::<f64>(), alone.iter().map(|r| r.clocks).fold(0.0, f64::max));
            let fsum: u64 = alone.iter().map(|r| r.fires).sum();
            println!("  lattice seed {seed} (side {}): {} clocks, {} rewrites superposed; one by one {} clocks in all (longest {}), {} rewrites; first fork after {} of its rewrites, {} forks, {} copies",
                s.side, k(s.clocks), k(s.fires as f64), k(csum), k(cmax), k(fsum as f64), s.first_fork.map_or("-".into(), |f| f.to_string()), s.forks, s.copies);
        }
        eprintln!("{name}: {:.1}s", t0.elapsed().as_secs_f64());
    }
}
