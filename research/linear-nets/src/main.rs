//! EXPERIMENTAL (research/LINEAR_TREE_NETS.md): runs disp calls and corpus terms on the
//! tree-calculus net as it is and with the redesigns of `net.rs`, and prints the work.
//!   linear-nets [--calls FILE] [--lambada] [--corpus N] [--shapes N] [--modes m,m,...]
//!               [--sched ideal|all] [--no-fork] [--check-strands] [--rules]
//! FILE lines are `name|head|arg,arg|answer` in ternary (emit.ts); `--lambada` adds the lambada
//! benchmarks, `--corpus` random terms, `--shapes` random code rich in the views' shapes; all are
//! checked against the eager answer or the oracle. Modes are `+`-joined words: `base` (the 26
//! rules), `views` (movement agents in code), `nobc` (views without B and C), `w`, `nok` (no K views:
//! K shapes stay plain forks), `pp` (views only along the principal path), `hints` (plain trees whose
//! forks carry their shape as a latent label an apply reads), `rom-code` (code read from the table),
//! `rom-all` (inputs too), and the table's native rules: `native` (an S rule peeking at its sides),
//! `thru` (also through copies), `apply` (an apply reads table code and fires its rule in one step),
//! `triage` (+ a triage reading its arms from the table), `head` (+ T1 reading a table head), with
//! ablations `noK`, `noroute`; e.g. `views+w+rom-code`, `head+rom-all`. The first mode is the baseline
//! of the geometric means printed at the end. Environment: CHECK=1 checks every invariant every round;
//! LOCAL=1 prints the pairs one rewrite made together and the table size; REUSE=1 prints each piece of
//! code's uses, copies and most uses close together; CHAIN=1 (2: K too) fires routing chains for free.

mod net;
mod run;

use net::{Net, Origin, Shape};
use run::Ledger;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use std::rc::Rc;

struct Case { name: String, head: Term, args: Vec<Term>, want: Option<Term> }

#[derive(Clone, Copy)]
struct Mode { shape: Shape, rom_code: bool, rom_data: bool, native: u8, opts: u8, hints: bool }

fn mode(m: &str) -> Mode {
    let has = |k: &str| m.split('+').any(|x| x == k);
    Mode {
        shape: Shape { views: has("views"), w: has("w"), bc: !has("nobc"), pp: has("pp"), nok: has("nok") },
        rom_code: has("rom-code") || has("rom-all"),
        rom_data: has("rom-all"),
        opts: (has("noK") as u8) | (has("noroute") as u8) << 1,
        hints: has("hints"),
        native: if has("head") { 5 } else if has("triage") { 4 } else if has("apply") { 3 } else if has("thru") { 2 } else if has("native") { 1 } else { 0 },
    }
}

fn load(c: &Case, m: Mode) -> (Net, u32, usize) {
    let mut net = Net::new();
    net.hints = m.hints;
    let plain = Shape { views: false, w: false, bc: false, pp: false, nok: false };
    let head = net.value(&c.head, Origin::Code, m.shape, m.rom_code);
    let args: Vec<u32> = c.args.iter().map(|a| net.value(a, Origin::Data, plain, m.rom_data)).collect();
    let root = net.call(head, &args);
    let out = net.drive(root);
    net.native = m.native;
    net.hints = m.hints;
    net.opts = m.opts;
    net.check = std::env::var("CHECK").is_ok();
    let agents = net.live;
    net.made = 0;
    (net, out, agents)
}

fn pct(x: u64, of: u64) -> String { if of == 0 { "-".into() } else { format!("{:.0}", 100.0 * x as f64 / of as f64) } }

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (mut shapes, mut fork) = (0u32, true);
    let (mut calls, mut lambada, mut corpus, mut modes, mut sched, mut check, mut rules) = (None, false, 0u32, "base".to_string(), "ideal".to_string(), false, false);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--calls" => calls = it.next().cloned(),
            "--lambada" => lambada = true,
            "--corpus" => corpus = it.next().unwrap().parse().unwrap(),
            "--shapes" => shapes = it.next().unwrap().parse().unwrap(),
            "--modes" => modes = it.next().unwrap().clone(),
            "--sched" => sched = it.next().unwrap().clone(),
            "--check-strands" => check = true,
            "--rules" => rules = true,
            "--no-fork" => fork = false,
            x => panic!("unknown argument {x}"),
        }
    }
    let mut cases: Vec<Case> = vec![];
    if let Some(f) = calls {
        for line in std::fs::read_to_string(f).expect("calls file").lines().filter(|l| !l.trim().is_empty()) {
            let p: Vec<&str> = line.split('|').collect();
            let t = |s: &str| rust_ic_mesh::term::ternary(s).expect("ternary");
            cases.push(Case { name: p[0].into(), head: t(p[1]), args: p[2].split(',').filter(|s| !s.is_empty()).map(t).collect(), want: Some(t(p[3])) });
        }
    }
    if lambada {
        for (w, n) in [("fib", 2u64), ("fib", 3), ("fib", 4), ("exp", 1), ("sort", 2), ("sort", 3)] {
            let Some(Term::Ap(h, a)) = rust_ic_mesh::term::workload(w, n) else { unreachable!() };
            let want = oracle::nf(Term::Ap(h.clone(), a.clone()), &mut Fuel(50_000_000)).ok();
            cases.push(Case { name: format!("lambada {w}({n})"), head: (*h).clone(), args: vec![(*a).clone()], want });
        }
    }
    if corpus > 0 {
        // Random terms as the abstract tier's differential makes them, split at their outermost
        // call into a head and arguments when those are values.
        let mut rng = Lcg(12345);
        let mut i = 0;
        while (cases.iter().filter(|c| c.name.starts_with("rand")).count() as u32) < corpus && i < 100_000 {
            let t = rng.rand_term(3 + (i % 4));
            i += 1;
            let Ok(want) = oracle::nf(t.clone(), &mut Fuel(5_000)) else { continue };
            let mut spine = vec![];
            let mut h = t.clone();
            while let Term::Ap(f, x) = h.clone() { spine.insert(0, (*x).clone()); h = (*f).clone(); }
            // Values only: every head and argument normalized first (they are leaves and
            // applications of leaves in this corpus), so a term is code applied to data.
            let Ok(hv) = oracle::nf(h, &mut Fuel(5_000)) else { continue };
            let Ok(av) = spine.into_iter().map(|x| oracle::nf(x, &mut Fuel(5_000))).collect::<Result<Vec<_>, _>>() else { continue };
            if av.is_empty() { continue; }
            cases.push(Case { name: format!("rand{i}"), head: hv, args: av, want: Some(want) });
        }
    }
    if shapes > 0 {
        // Random code rich in the shapes the views stand for (K, S, B, C, I, W), applied to
        // random values: the views' rules must agree with the oracle, triage on code included.
        fn code(rng: &mut Lcg, d: u32) -> Term {
            use oracle::{f2, s};
            if d == 0 { return if rng.next() < 0.5 { Term::L } else { s(Term::L) }; }
            let r = rng.next();
            if r < 0.15 { f2(Term::L, code(rng, d - 1)) }
            else if r < 0.35 { f2(s(f2(Term::L, code(rng, d - 1))), code(rng, d - 1)) }
            else if r < 0.5 { f2(s(code(rng, d - 1)), f2(Term::L, code(rng, d - 1))) }
            else if r < 0.6 { f2(s(code(rng, d - 1)), code(rng, d - 1)) }
            else if r < 0.65 { let x = code(rng, d - 1); f2(s(x.clone()), x) }
            else if r < 0.7 { f2(f2(Term::L, Term::L), Term::L) }
            else if r < 0.8 { f2(f2(code(rng, d - 1), code(rng, d - 1)), code(rng, d - 1)) }
            else if r < 0.9 { s(code(rng, d - 1)) }
            else { Term::L }
        }
        let mut rng = Lcg(777);
        let mut i = 0;
        while (cases.iter().filter(|c| c.name.starts_with("shape")).count() as u32) < shapes && i < 1_000_000 {
            i += 1;
            let h = code(&mut rng, 2 + i % 3);
            let args: Vec<Term> = (0..1 + i % 3).map(|_| code(&mut rng, 1 + i % 2)).collect();
            let t = args.iter().fold(h.clone(), |f, x| oracle::ap(f, x.clone()));
            let Ok(want) = oracle::nf(t, &mut Fuel(20_000)) else { continue };
            cases.push(Case { name: format!("shape{i}"), head: h, args, want: Some(want) });
        }
    }
    println!("{:<44} {:<18} {:>6} {:>8} {:>6} {:>4} {:>4} {:>14} {:>14} {:>5} {:>6} {:>6} {:>6} {:>5}",
        "case", "mode", "load/rom", "rewrites", "rounds", "Dn%", "Eps%", "Dn c/d/b", "Eps c/d/b", "coll", "unfold", "peak", "S:BCW?", "ok");
    let mut totals: Vec<(String, u64, u64, u64, u64)> = vec![];
    for c in &cases {
        for m in modes.split(',') {
            let md = mode(m);
            let (mut net, out, agents) = load(c, md);
            let mut ledger = Ledger::default();
            let done = if sched == "all" { run::all(&mut net, 50_000_000, &mut ledger) } else { run::ideal(&mut net, out, fork, 10_000_000, &mut ledger) };
            let got = net.readback(net.get(out).ports[0]);
            let ok = done && got.is_some() && (c.want.is_none() || got == c.want);
            let dn = ledger.consumer("Dn");
            let ep = ledger.consumer("Eps");
            let total = ledger.total();
            let s = ledger.s_shapes;
                    println!("{:<44} {:<18} {:>6} {:>8} {:>6} {:>4} {:>4} {:>14} {:>14} {:>5} {:>6} {:>6} {:>6} {:>5}",
                &c.name[..c.name.len().min(44)], m, format!("{agents}/{}", net.rom.nodes.len()), total, ledger.rounds,
                pct(dn.iter().sum(), total), pct(ep.iter().sum::<u64>() + ledger.collect.iter().sum::<u64>(), total),
                format!("{}/{}/{}", dn[0], dn[1], dn[2]), format!("{}/{}/{}", ep[0], ep[1], ep[2]),
                ledger.collect.iter().sum::<u64>(), net.unfolds, net.peak,
                format!("{}/{}/{}/{}", s[0], s[1], s[2], s[3]), if ok { "ok" } else if !done { "UNDONE" } else { "WRONG" });
            if rules {
                for (k, v) in &ledger.by_rule { println!("    {k:<10} code {:>7} data {:>7} built {:>7}", v[0], v[1], v[2]); }
                println!("    collect dn->wire {} app {} unp {}; agents made {}", ledger.collect[0], ledger.collect[1], ledger.collect[2], net.made);
            }
            totals.push((m.to_string(), total, ledger.rounds, net.unfolds, net.made));
            if std::env::var("REUSE").is_ok() {
                // Per piece of code: uses in all, and the most uses within any window of w rounds.
                let mut by: std::collections::HashMap<u32, Vec<u64>> = std::collections::HashMap::new();
                for &(id, r) in &ledger.uses { by.entry(id).or_default().push(r); }
                let total: usize = by.values().map(|v| v.len()).sum();
                let peak = |w: u64| -> usize {
                    by.values().map(|v| {
                        let mut v = v.clone(); v.sort();
                        let (mut best, mut j) = (0, 0);
                        for i in 0..v.len() { while v[i] - v[j] >= w { j += 1; } best = best.max(i - j + 1); }
                        best
                    }).sum()
                };
                println!("    reuse: {} pieces, {} uses, {} copies made; most uses at once (window 1/4/16/64 rounds): {} / {} / {} / {}",
                    by.len(), total, ledger.copies, peak(1), peak(4), peak(16), peak(64));
            }
            if std::env::var("LOCAL").is_ok() { println!("    pairs {} together {} ({}%), table {} entries", ledger.pairs, ledger.together, pct(ledger.together, ledger.pairs), net.rom.nodes.len()); }
            if check && m == "base" && sched != "all" {
                // The same run on rust-ic-strands' own idealised machine must do the same work.
                let mut t = c.args.iter().fold(c.head.clone(), |f, x| Term::Ap(Rc::new(f), Rc::new(x.clone())));
                if let Term::Ap(..) = t {} else { t = c.head.clone(); }
                let mut bn = rust_ca_lattice::net::Net::new();
                let root = bn.build(&t);
                bn.drive(root);
                let mut want: Vec<bool> = bn.agents.iter().map(|a| matches!(a, Some(a) if matches!(a.tag, rust_ca_lattice::rules::Tag::Nrm | rust_ca_lattice::rules::Tag::Out | rust_ca_lattice::rules::Tag::Eps))).collect();
                let r = rust_ic_strands::lattice::memo::ideal_depth(&mut bn, &mut want, true, 10_000_000);
                println!("    strands ideal_depth: {:?} (ours {} rewrites, {} rounds)", r, total, ledger.rounds);
            }
        }
    }
    // Geometric means of each mode's work and rounds against the first mode's, over every case.
    let ms: Vec<&str> = modes.split(',').collect();
    if ms.len() > 1 {
        let n = ms.len();
        println!("\ngeometric mean against {} over {} cases:", ms[0], cases.len());
        for (j, m) in ms.iter().enumerate() {
            let (mut lw, mut lr, mut lu, mut lm, mut k) = (0.0, 0.0, 0.0, 0.0, 0);
            for i in 0..cases.len() {
                let (b, x) = (&totals[i * n], &totals[i * n + j]);
                if b.1 == 0 || x.1 == 0 { continue; }
                lw += (x.1 as f64 / b.1 as f64).ln();
                lr += ((x.2.max(1)) as f64 / (b.2.max(1)) as f64).ln();
                lu += ((x.1 + x.3) as f64 / (b.1 + b.3) as f64).ln();
                lm += (x.4.max(1) as f64 / b.4.max(1) as f64).ln();
                k += 1;
            }
            let g = |x: f64| (x / k as f64).exp();
            println!("  {m:<18} rewrites ×{:.3}  rounds ×{:.3}  rewrites+unfolds ×{:.3}  agents made ×{:.3}", g(lw), g(lr), g(lu), g(lm));
        }
    }
}
