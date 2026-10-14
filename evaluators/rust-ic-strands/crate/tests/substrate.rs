//! The substrate (substrate.rs): the strand lattice with every rewrite recorded and undoable, K and
//! triage keeping what they drop. Forward it gives the oracle's answers; with the drive turned
//! round it runs back; the ledger balances.

use rust_ca_lattice::net::{Agent, Net};
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ic_strands::lattice::{latest, Lattice, Params};

fn corpus(n: usize) -> Vec<(Term, String)> {
    let mut rng = Lcg(20260730);
    (0..n).filter_map(|i| {
        let t = rng.rand_term(3 + (i % 4) as u32);
        oracle::nf(t.clone(), &mut Fuel(5_000)).ok().map(|w| (t, oracle::show(&w)))
    }).collect()
}

fn phys(seed: u64) -> Params { Params { w: 48, h: 48, depth: 6, phys: true, seed, ..latest() } }

fn load(t: &Term, p: Params) -> (Lattice, Vec<Option<Agent>>) {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    let start = net.agents.clone();
    (Lattice::load(p, net, out).expect("load"), start)
}

/// Clocks until `done`, at most `max`.
fn until(l: &mut Lattice, max: u64, done: impl Fn(&Lattice) -> bool) -> bool {
    let stop = l.stats.proposals + max;
    while l.stats.proposals < stop {
        if done(l) { return true; }
        l.run_on(l.stats.proposals + 2_000);
    }
    done(l)
}

#[test]
fn forward_gives_the_oracles_answers() {
    let (mut undone, mut held) = (0, 0);
    for (i, (t, want)) in corpus(120).iter().enumerate() {
        let (mut l, _) = load(t, phys(i as u64 + 1));
        if i % 10 == 0 { l.check_every = 1; }
        assert!(l.run(20_000_000), "term {i} did not finish: {}", oracle::show(t));
        assert_eq!(l.readback().map(|t| oracle::show(&t)).as_deref(), Some(want.as_str()), "term {i}: WRONG ANSWER");
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        l.check_invariants().unwrap_or_else(|e| panic!("term {i}: {e}"));
        undone += l.stats.unfires;
        held += l.erasers();
    }
    eprintln!("{undone} rewrites undone on the way, {held} erasers left holding garbage");
}

/// Forward to the answer, then the drive turned round: every rewrite is undone and the net is the
/// loaded one again, every agent with its id, wired as loaded.
#[test]
fn back_to_the_start() {
    let mut report = vec![];
    for (i, (t, _)) in corpus(40).iter().enumerate() {
        let (mut l, start) = load(t, phys(i as u64 + 1));
        assert!(l.run(20_000_000), "term {i} did not finish");
        let made = l.records_kept();
        l.set_drive(-6.0);
        let back = until(&mut l, 40_000_000, |l| l.same_net(&start));
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        report.push(format!("term {i}: {made} records, {} left, {} undone, back: {back}", l.records_kept(), l.stats.unfires));
        if !back { for r in l.records_report() { eprintln!("  {r:?}"); } }
        assert!(back, "term {i}: not back at the start ({} of {made} records left)", l.records_kept());
    }
    eprintln!("{}", report.join("\n"));
}

/// Fuel spent is the agents added plus the heat; with nothing erased or forgotten, the heat is the
/// drive's alone, ε for every step forward not undone.
#[test]
fn the_ledger_balances() {
    for (i, (t, _)) in corpus(30).iter().enumerate() {
        let (mut l, _) = load(t, phys(i as u64 + 1));
        l.run(20_000_000);
        let g = &l.sub.ledger;
        assert_eq!((g.erased, g.forgot), (0, 0), "term {i}: nothing erased or forgotten");
        assert!((g.heat() - l.p.drive * (g.forward - g.back) as f64).abs() < 1e-6, "term {i}");
        assert_eq!(g.back, l.stats.unfires, "term {i}");
        assert_eq!(g.forward - g.back, l.records_kept() as u64, "term {i}: a record for every step forward not undone");
    }
}

/// A run put back to a saved state and run on repeats itself exactly, ids and records included,
/// with the drive changed on the way as the schedule says.
#[test]
fn saved_states_repeat_the_run() {
    for (i, (t, _)) in corpus(12).iter().enumerate() {
        let (mut l, _) = load(t, phys(i as u64 + 1));
        l.schedule = vec![(0.0, [l.p.drive, l.p.erase, l.p.forget, l.p.bond])];
        for _ in 0..40 { l.chip_clock(); }
        let saved = l.snap();
        for _ in 0..30 { l.chip_clock(); }
        l.reset_schedule([-6.0, 0.0, 0.0, 1.0]);
        for _ in 0..60 { l.chip_clock(); }
        let (words, net, ledger) = (l.state_words(), l.shadow.agents.clone(), format!("{:?}", l.sub.ledger));
        l.put_snap(&saved);
        assert_eq!(l.p.drive, phys(1).drive, "term {i}: the drive at the saved clock");
        for _ in 0..90 { l.chip_clock(); }
        assert_eq!(l.state_words(), words, "term {i}: the lattice");
        assert!(l.same_net(&net), "term {i}: the abstract net");
        assert_eq!(format!("{:?}", l.sub.ledger), ledger, "term {i}: the ledger");
    }
}
