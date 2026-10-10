//! Movement agents (`views=1`, rules.rs `VIEW_RULES`) on the strand lattice, in the current design:
//! random terms, code applied to data, code rich in the K, S, B, C, I and W shapes (also with every
//! value in views, so triage and dispatch read them as data) and disp programs, each run to the
//! oracle's answer with the lattice projecting exactly onto the abstract net, some re-checking every
//! invariant after every move; every view rule fires; the root read back means the answer all along;
//! garbage holding views is collected; a saved state put back runs on exactly; the GPU takes them,
//! and what takes no views refuses them.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ca_lattice::rules::{Tag, N_RULES, VIEW_RULES};
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::{readback, share, tables};

fn views() -> Params { let mut p = latest(); p.set("views", "1").unwrap(); p }

fn load(t: &Term, p: Params) -> Option<Lattice> {
    let (net, out) = share::net_for(&t.into(), &p).ok()?;
    Lattice::load(p, net, out).ok()
}

/// `t` with every value in it in views, the arguments too, so readers meet views as data.
fn load_every(t: &Term, p: Params) -> Option<Lattice> {
    let mut net = Net::new();
    let root = net.build_every_view(t);
    let (_, out) = net.drive(root);
    Lattice::load(p, net, out).ok()
}

/// `load` on a lattice sized as the player sizes it: side ⌈√agents · 7⌉ + 16, ×1.4 until the drawing fits.
fn fit(t: &Term, p: Params) -> Lattice {
    let (net, _) = share::net_for(&t.into(), &p).unwrap();
    let mut side = ((net.agents.len() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    loop {
        if let Some(l) = load(t, Params { w: side, h: side, ..p }) { return l; }
        side = (side as f64 * 1.4).ceil() as u32;
    }
}

fn nf(t: &Term) -> Option<String> { oracle::nf(t.clone(), &mut Fuel(2_000_000)).ok().map(|t| oracle::show(&t)) }

/// Random terms, and the same split into a head and arguments worked out first (code applied to data).
fn corpus(n: u32) -> Vec<Term> {
    let mut rng = Lcg(20261010);
    let mut out = vec![];
    for i in 0..n {
        let t = rng.rand_term(3 + (i % 4));
        if oracle::nf(t.clone(), &mut Fuel(5_000)).is_err() { continue; }
        let (mut h, mut args) = (t.clone(), vec![]);
        while let Term::Ap(f, x) = h.clone() { args.insert(0, (*x).clone()); h = (*f).clone(); }
        out.push(t);
        let Ok(h) = oracle::nf(h, &mut Fuel(5_000)) else { continue };
        let Ok(args) = args.into_iter().map(|a| oracle::nf(a, &mut Fuel(5_000))).collect::<Result<Vec<_>, _>>() else { continue };
        if !args.is_empty() { out.push(args.into_iter().fold(h, ap)); }
    }
    out
}

/// Random code rich in K, S, B, C, I and W shapes and triage, applied to random values (themselves
/// rich in those shapes) and to applications still to be worked out.
fn shapes(n: usize) -> Vec<Term> {
    fn code(rng: &mut Lcg, d: u32) -> Term {
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
    let mut out = vec![];
    let mut i = 0;
    while out.len() < n && i < 1_000_000 {
        i += 1;
        let h = code(&mut rng, 2 + i % 3);
        let t = (0..1 + i % 3).fold(h, |f, _| {
            let x = if rng.next() < 0.25 { ap(code(&mut rng, 1), code(&mut rng, 2)) } else { code(&mut rng, 1 + i % 3) };
            ap(f, x)
        });
        if oracle::nf(t.clone(), &mut Fuel(20_000)).is_ok() { out.push(t); }
    }
    out
}

/// Each view read by each reader: applied, copied, erased, triaged as a fork's head, dispatched on and
/// normalized (to be loaded with every value in views).
fn every_reader() -> Vec<Term> {
    let l = || Term::L;
    let views = [f2(l(), s(l())), f2(s(l()), s(l())), f2(s(f2(l(), l())), l()), f2(s(s(l())), f2(l(), l())), f2(f2(l(), l()), l()), f2(s(s(l())), s(l()))];
    views.iter().flat_map(|v| [
        ap(v.clone(), s(l())),
        ap(f2(s(l()), s(l())), v.clone()),
        ap(f2(l(), l()), v.clone()),
        ap(ap(s(v.clone()), l()), s(l())),
        ap(f2(f2(s(l()), l()), l()), v.clone()),
        ap(f2(l(), v.clone()), l()),
    ]).collect()
}

/// A disp program from the player's compiled list, applied to these arguments (ternary).
fn disp(name: &str, args: &[&str]) -> Term {
    let src = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).expect("player/programs.js");
    let at = src.find(&format!("\"name\": \"{name}\"")).unwrap_or_else(|| panic!("no program {name}"));
    let tree = src[at..].split("\"tree\": \"").nth(1).unwrap().split('"').next().unwrap();
    let t = |x: &str| rust_ic_mesh::term::ternary(x).unwrap();
    args.iter().fold(t(tree), |f, x| ap(f, t(x)))
}
fn nat(n: usize) -> String { "20".repeat(n) + "0" }

/// Run to the answer; the answer and whether the projection is exact.
fn run(l: &mut Lattice) -> (Option<String>, Result<(), String>) {
    l.run(200_000_000);
    (l.readback().map(|t| oracle::show(&t)), l.check_projection())
}

#[test]
fn view_runs_agree_with_the_oracle() {
    let mut fired = vec![0u64; VIEW_RULES.len()];
    let (mut ran, mut checked) = (0, 0);
    let cases: Vec<Term> = corpus(120).into_iter().chain(shapes(160)).collect();
    let targeted = every_reader();
    for (i, t) in cases.iter().chain(&targeted).enumerate() {
        let want = nf(t).unwrap();
        // Every eighth on a small lattice, re-checking every invariant after every move; every third
        // with every value in views.
        let small = i % 8 == 0;
        let p = if small { Params { w: 16, h: 16, depth: 4, ..views() } } else { Params { w: 48, h: 48, depth: 6, ..views() } };
        let p = Params { seed: i as u64 + 1, ..p };
        let every = i % 3 == 1 || i >= cases.len();
        let Some(mut l) = (if every { load_every(t, p) } else { load(t, p) }) else { assert!(small, "term {i} does not fit"); continue };
        if small { l.check_every = 1; checked += 1; }
        l.trace_start();
        let (got, proj) = run(&mut l);
        assert_eq!(got.as_deref(), Some(want.as_str()), "term {i}: {}", oracle::show(t));
        proj.unwrap_or_else(|e| panic!("term {i}: {e}"));
        for f in &l.trace.fires { if f.1 >= N_RULES { fired[f.1 - N_RULES] += 1; } }
        ran += 1;
    }
    println!("{ran} runs, {checked} checked after every move");
    assert!(checked >= 20, "only {checked} terms fit the small lattice");
    for (r, &n) in VIEW_RULES.iter().zip(&fired) {
        println!("  {}·{}: {n}", r.consumer.name(), r.producer.name());
        assert!(n > 0, "{}·{} never fired on the lattice", r.consumer.name(), r.producer.name());
    }
}

#[test]
fn disp_programs_and_benchmarks_with_views() {
    // Built in each run's own thread (terms are not `Send`), every run at once.
    let cases: [(&str, fn() -> Term); 6] = [
        ("add 2 3", || disp("add", &[&nat(2), &nat(3)])),
        ("fib 3", || disp("fib", &[&nat(3)])),
        ("is_even 3", || disp("is_even", &[&nat(3)])),
        ("mul 2 2", || disp("mul", &[&nat(2), &nat(2)])),
        ("fib:1", || rust_ic_mesh::term::workload("fib", 1).unwrap()),
        ("sort:1", || rust_ic_mesh::term::workload("sort", 1).unwrap()),
    ];
    let lines: Vec<String> = std::thread::scope(|sc| {
        let runs: Vec<_> = cases.iter().enumerate().flat_map(|(i, &(name, make))| [1, 2].map(|seed| sc.spawn(move || {
            let t = make();
            let want = nf(&t).unwrap();
            let mut l = fit(&t, Params { seed, ..views() });
            // Every invariant re-checked every few hundred moves (after every move would take minutes here).
            if i == 0 { l.check_every = 397; }
            let (got, proj) = run(&mut l);
            assert_eq!(got.as_deref(), Some(want.as_str()), "{name}, seed {seed}");
            proj.unwrap_or_else(|e| panic!("{name}: {e}"));
            let net = share::net_for(&(&t).into(), &views()).unwrap().0;
            format!("{name} seed {seed}: {:.0} clocks, {} rewrites, {} agents at load, {} of them views", l.stats.clocks, l.stats.fires,
                net.live_count(), net.agents.iter().flatten().filter(|a| a.tag.is_view()).count())
        }))).collect();
        runs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    for line in lines { println!("{line}"); }
}

/// Run on past the answer until only the answer is left, every invariant re-checked after every
/// move: garbage holding views is collected too.
#[test]
fn garbage_is_collected_with_views() {
    let (mut erased, mut runs) = (0, 0);
    for (i, t) in shapes(200).iter().enumerate() {
        let p = Params { w: 16, h: 16, depth: 4, seed: i as u64 + 1, ..views() };
        let Some(mut l) = (if i % 2 == 0 { load(t, p) } else { load_every(t, p) }) else { continue };
        l.check_every = 1;
        l.trace_start();
        let (got, _) = run(&mut l);
        let mut marks = vec![];
        for _ in 0..3000 { if l.garbage(&mut marks) == 0 { break; } let next = l.stats.proposals + 1; l.run_on(next); }
        assert_eq!(l.garbage(&mut marks), 0, "term {i}: garbage left");
        assert_eq!(l.readback().map(|t| oracle::show(&t)), got, "term {i}");
        assert_eq!(got, nf(t), "term {i}");
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        erased += l.trace.fires.iter().filter(|f| f.1 >= N_RULES && rust_ca_lattice::rules::rule(f.1).consumer == Tag::Eps).count();
        runs += 1;
    }
    println!("{runs} runs cleaned up, {erased} views erased");
    assert!(erased > 0, "no view was erased");
}

#[test]
fn the_root_reads_back_as_the_answer_all_along() {
    let (mut looks, mut read) = (0, 0);
    for (i, t) in shapes(40).iter().chain(&corpus(20)).enumerate() {
        let want = nf(t).unwrap();
        let p = Params { w: 40, h: 40, depth: 8, seed: i as u64 + 1, ..views() };
        let Some(mut l) = (if i % 2 == 0 { load(t, p) } else { load_every(t, p) }) else { continue };
        let mut c = 0;
        while !l.answered() && c < 50_000 {
            if c % 12 == 0 {
                let (s, k) = l.find_out().unwrap();
                let r = readback::Reader::new(&l.shadow).meaning(l.sids[s as usize * l.ks + k]);
                looks += 1;
                if let Some(now) = readback::term(&r) {
                    read += 1;
                    assert_eq!(nf(&now).as_deref(), Some(want.as_str()), "term {i}, clock {c}: {}", readback::text(&r, 400).0);
                }
            }
            l.chip_clock();
            c += 1;
        }
    }
    assert!(read * 10 >= looks * 9, "only {read} of {looks} looks read the whole term");
}

#[test]
fn a_saved_state_keeps_its_views() {
    let mut tried = 0;
    for (i, t) in shapes(80).iter().enumerate() {
        let Some(mut l) = load(t, Params { w: 40, h: 40, depth: 6, ..views() }) else { continue };
        for _ in 0..12 { l.chip_clock(); }
        let has_views = |l: &Lattice| l.shadow.agents.iter().flatten().any(|a| a.tag.is_view());
        if l.answered() || !has_views(&l) { continue; }
        let (held, stats) = (l.held_sites(), l.stats.clone());
        let seen: Vec<_> = (0..120).map(|_| { l.chip_clock(); l.state_words() }).collect();
        l.put_sites(&held);
        l.stats = stats;
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        assert!(has_views(&l), "term {i}: the views were lost");
        for (c, w) in seen.iter().enumerate() {
            l.chip_clock();
            assert!(l.state_words() == *w, "term {i}: clock {} differs after the state was put back", 13 + c);
        }
        let (got, proj) = run(&mut l);
        assert_eq!(got, nf(t), "term {i}");
        proj.unwrap();
        tried += 1;
        if tried == 4 { return; }
    }
    panic!("only {tried} terms ran long enough to save");
}

#[test]
fn what_takes_no_views_refuses_them() {
    let t = disp("add", &[&nat(1), &nat(1)]);
    let l = fit(&t, views());
    assert!(tables::gpu_refuses(&l).is_none() && tables::gpu_unfit(&views()).is_none(), "the GPU runs movement agents");
    assert!(tables::wgsl_for(&views()).contains("const NRULES: u32 = 62u;"));
    assert!(share::net_for(&rust_ic_mesh::term::parse_sup("@(&1{L,S(L)},L)").unwrap(), &views()).is_err());
    let mut shared = views();
    shared.set("share", "8").unwrap();
    assert!(share::net_for(&(&t).into(), &shared).is_err());
    let mut memo = views();
    memo.set("memo", "2").unwrap();
    let (net, out) = share::net_for(&(&t).into(), &memo).unwrap();
    assert!(Lattice::load(Params { w: 128, h: 128, ..memo }, net, out).is_err());
    // Off, nothing changes: no views.
    let (net, _) = share::net_for(&(&t).into(), &latest()).unwrap();
    assert!(!net.agents.iter().flatten().any(|a| a.tag.is_view()));
}
