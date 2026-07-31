//! The cross-backend signal-plane gate (AC_IDEA step 0): three implementations of one
//! heat fixpoint — the in-word worklist wave, union-find cable components, and the
//! dense iterative recompute — must each independently satisfy the substrate contract
//! (quiesce, never answer wrongly, project at seed-free rest, survive a kick), and any
//! answer produced under any backend is the oracle's. Heat timing is just another
//! schedule dimension, so per-term completion may differ between backends (instant
//! cables move more matter sooner); the floors below pin each backend's aggregate.

use rust_ca_lattice::cascade_run::{
    check_projection, check_reciprocity, load_net, load_net_tree, Discipline, Runner,
};
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ca_lattice::signal::SignalBackend;

const BUDGET: u64 = 4_000_000;

fn backends() -> [SignalBackend; 2] {
    [SignalBackend::components(), SignalBackend::dense()]
}

fn frontier_terms() -> Vec<(&'static str, Term)> {
    use rust_ca_lattice::oracle::{ap, f2, s};
    vec![
        ("identity", ap(Term::L, Term::L)),
        ("k-combinator", ap(ap(oracle::k(), s(Term::L)), Term::L)),
        ("fork-dispatch", ap(f2(Term::L, Term::L), Term::L)),
        ("s-rule-sharing", ap(f2(s(Term::L), s(Term::L)), Term::L)),
        ("k-chain", oracle::chain_k(2)),
        ("disp-t", oracle::disp_t()),
    ]
}

fn soak_slice(count: u32) -> Vec<(String, Term, Discipline, bool)> {
    let mut rng = Lcg(20260730);
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    let mut out = vec![];
    for i in 0..count {
        let term = rng.rand_term(3 + (i % 4));
        if oracle::nf(term.clone(), &mut Fuel(5_000)).is_err() {
            continue;
        }
        out.push((
            format!("term {i}"),
            term,
            disciplines[((i / 8) % 4) as usize],
            (i / 4) % 2 == 1,
        ));
    }
    out
}

fn run_term(
    term: &Term,
    discipline: Discipline,
    tree: bool,
    backend: SignalBackend,
) -> (Runner, u32) {
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, out) = shadow.drive(root);
    let grid = if tree {
        load_net_tree(&shadow, Topo::Full3D).expect("loads")
    } else {
        load_net(&shadow, Topo::Full3D).expect("loads")
    };
    let mut r = Runner::new(grid, shadow, discipline);
    r.signals = backend;
    (r, out)
}

/// Every backend independently satisfies the whole substrate contract on the frontier
/// corpus and a soak slice, and its aggregate completion is floored per backend.
///
/// Measured 2026-07-31: all three backends complete 28/54 with ZERO per-case verdict
/// flips — completion is per-case ROBUST across heat semantics, even though worklist
/// heat over-approximates demand (a raised bit persists until its route is rewired)
/// while derivational heat is exact and instant (a cable cools the moment its source
/// disconnects, and heats whole). Flips remain report-only if they ever appear (heat
/// timing is a schedule dimension; chaotic-margin discipline pins aggregates only).
#[test]
fn backends_uphold_the_contract() {
    let mut verdicts: Vec<(String, Vec<(String, bool)>)> = vec![];
    for backend in [
        SignalBackend::worklist(),
        SignalBackend::components(),
        SignalBackend::dense(),
    ] {
        let name = backend.name();
        let mut cases: Vec<(String, Term, Discipline, bool)> = frontier_terms()
            .into_iter()
            .map(|(n, t)| (n.to_string(), t, Discipline::Fifo, false))
            .collect();
        cases.extend(soak_slice(48));
        let total = cases.len();
        let mut complete = 0u32;
        let mut per_case: Vec<(String, bool)> = vec![];
        for (case, term, discipline, tree) in cases {
            let want =
                oracle::show(&oracle::nf(term.clone(), &mut Fuel(100_000)).expect("oracle nf"));
            let (mut r, out) = run_term(&term, discipline, tree, backend.clone());
            assert!(r.run(BUDGET), "[{name}] {case}: did not quiesce");
            let seed_free = r.grid.seed_sids.is_empty();
            if seed_free {
                check_reciprocity(&r.grid)
                    .unwrap_or_else(|e| panic!("[{name}] {case}: reciprocity: {e}"));
                check_projection(&r.grid, &r.shadow)
                    .unwrap_or_else(|e| panic!("[{name}] {case}: projection: {e}"));
            }
            let mut done = false;
            if let Some(got) = r.shadow.readback(r.shadow.get(out).ports[0]) {
                assert_eq!(oracle::show(&got), want, "[{name}] {case}: WRONG ANSWER");
                if seed_free {
                    complete += 1;
                    done = true;
                }
            }
            per_case.push((case, done));
        }
        println!("backend {name}: {complete}/{total} complete");
        assert!(complete >= 26, "backend {name} completion floor: {complete} < 26");
        verdicts.push((name.to_string(), per_case));
    }
    // Report (never pin) per-case flips between backends.
    let base = &verdicts[0];
    for (name, cases) in &verdicts[1..] {
        let flips: Vec<&str> = base
            .1
            .iter()
            .zip(cases)
            .filter(|(a, b)| a.1 != b.1)
            .map(|(a, _)| a.0.as_str())
            .collect();
        println!(
            "verdict flips {} vs {}: {} ({})",
            base.0,
            name,
            flips.len(),
            if flips.is_empty() { "none".to_string() } else { flips.join(", ") }
        );
    }
}

/// The kick invariant under derivational backends: after quiescence, waking the whole
/// grid moves nothing. This is THE test of the rebuild-diff wake chain — a hole in
/// "sync wakes what newly heated" shows up here as post-kick progress.
#[test]
fn kick_holds_under_derivational_backends() {
    use rust_ca_lattice::cascade_run::Event;
    for backend in backends() {
        let name = backend.name();
        let mut cases: Vec<(String, Term, Discipline, bool)> = frontier_terms()
            .into_iter()
            .map(|(n, t)| (n.to_string(), t, Discipline::Fifo, false))
            .collect();
        cases.extend(soak_slice(24));
        for (case, term, discipline, tree) in cases {
            let (mut r, _out) = run_term(&term, discipline, tree, backend.clone());
            assert!(r.run(BUDGET), "[{name}] {case}: did not quiesce");
            let counts = |r: &Runner| {
                let (mut docks, mut retracts) = (0u64, 0u64);
                for e in &r.events {
                    match e {
                        Event::Dock(..) => docks += 1,
                        Event::Retract(..) => retracts += 1,
                        _ => {}
                    }
                }
                (r.grid.rewrites, r.grid.transport, docks, retracts)
            };
            let before = counts(&r);
            r.kick();
            assert!(r.run(BUDGET), "[{name}] {case}: kicked run did not re-quiesce");
            assert_eq!(before, counts(&r), "[{name}] {case}: progress after kick — lost wake");
        }
    }
}

/// One term, all three backends, all four disciplines: any produced answer equals the
/// oracle's (the cross-backend agreement is through the oracle, the only fixed point
/// all schedules share).
#[test]
fn all_backends_all_disciplines_agree_through_the_oracle() {
    use rust_ca_lattice::oracle::{ap, f2, s};
    let term = ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L));
    let want = oracle::show(&oracle::nf(term.clone(), &mut Fuel(100_000)).expect("nf"));
    for backend in [
        SignalBackend::worklist(),
        SignalBackend::components(),
        SignalBackend::dense(),
    ] {
        for discipline in [
            Discipline::Fifo,
            Discipline::Lifo,
            Discipline::Random(7),
            Discipline::AddressOrdered,
        ] {
            let (mut r, out) = run_term(&term, discipline, false, backend.clone());
            assert!(r.run(BUDGET), "{}/{discipline:?}: quiesce", backend.name());
            if let Some(got) = r.shadow.readback(r.shadow.get(out).ports[0]) {
                assert_eq!(
                    oracle::show(&got),
                    want,
                    "{}/{discipline:?}: wrong answer",
                    backend.name()
                );
            }
        }
    }
}
