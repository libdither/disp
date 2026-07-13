//! Stage-2 differential: reduction ON THE LATTICE vs the independent oracle, with the
//! projection invariant asserted along the way — now run on BOTH topologies (bilayer and
//! full 3D) with the polymer moves (retract, kink-flip) in the tick.
//!
//! The gate discipline (the project's differential-oracle discipline at substrate level):
//! CORRECTNESS is absolute — zero wrong normal forms, zero invariant violations, bit
//! determinism, on every topology. LIVENESS (reaching NF under the plain sequential
//! schedule, still with no fields) is MEASURED and reported; only catastrophic regressions
//! fail the suite.

use rust_ca_lattice::lattice::{Topo, TOPOS};
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ca_lattice::scheduler::{run_term, run_term_with, CheckLevel, FireMode, Outcome, Sim};
use rust_ca_lattice::transitions::plan_retract;

fn oracle_nf(t: &Term) -> Option<String> {
    oracle::nf(t.clone(), &mut Fuel(20_000)).ok().map(|w| oracle::show(&w))
}

/// Correctness gate: if the lattice finishes, the result must match the oracle exactly.
/// Completion itself is asserted only when `must_complete` (verified micro-pins).
fn check_pin(t: Term, topo: Topo, must_complete: bool) -> Outcome {
    let want = oracle_nf(&t).expect("pin diverged in oracle");
    let out = run_term(&t, topo, 50_000, CheckLevel::Every);
    if out.done {
        assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
            "wrong NF for {} on {}", oracle::show(&t), topo.name());
    } else {
        assert!(!must_complete, "must-complete pin stuck on {}: {}", topo.name(), oracle::show(&t));
        println!("[{}] pin stuck (liveness residue, correctness intact): {}", topo.name(), oracle::show(&t));
    }
    out
}

#[test]
fn pins_must_complete() {
    // Verified to reach NF under the bare sequential schedule, per topology. Bilayer
    // currently wedges at fire seams (splices + trails exhaust the single overflow plane;
    // the measured residue the fields address), so its verified set is smaller.
    for topo in TOPOS {
        let out = check_pin(ap(s(Term::L), Term::L), topo, true); // stem application
        assert!(out.transport > 0, "reel-in should have happened");
    }
    let t3 = Topo::Full3D;
    check_pin(ap(f2(Term::L, Term::L), Term::L), t3, true); // fork dispatch → w
    check_pin(oracle::chain_k(1), t3, true);
    check_pin(ap(ap(oracle::k(), Term::L), s(Term::L)), t3, true); // K erases
    check_pin(ap(f2(s(Term::L), Term::L), Term::L), t3, true); // S-rule shares
}

#[test]
fn pins_status() {
    // The rung-2 (2D) residue pins: correctness gated absolutely; completion printed so
    // regressions and recoveries stay visible in the test log.
    for topo in TOPOS {
        for (name, t) in [
            ("kargs", ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L)))),
            ("chain4", oracle::chain_k(4)),
            ("disp", oracle::disp_t()),
            ("share", ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L))),
        ] {
            let out = check_pin(t, topo, false);
            println!("[{}] {name}: done={} ticks={} ints={} transport={}",
                topo.name(), out.done, out.ticks, out.ints, out.transport);
        }
    }
}

#[test]
fn determinism() {
    for topo in TOPOS {
        let t = oracle::chain_k(3);
        let a = run_term(&t, topo, 50_000, CheckLevel::Tick);
        let b = run_term(&t, topo, 50_000, CheckLevel::Tick);
        assert_eq!((a.done, a.ticks, a.ints, a.transport, a.peak_agents),
                   (b.done, b.ticks, b.ints, b.transport, b.peak_agents),
            "sequential schedule must be bit-deterministic on {}", topo.name());
    }
}

fn differential(topo: Topo) {
    let mut rng = Lcg(999);
    let (mut pass, mut stuck, mut cap, mut skip) = (0u32, 0u32, 0u32, 0u32);
    let (mut ints, mut transport) = (0u64, 0u64);
    let n_terms = 400;
    for i in 0..n_terms {
        let term = rng.rand_term(3 + (i % 3));
        let Some(want) = oracle_nf(&term) else { skip += 1; continue };
        let check = if i % 8 == 0 { CheckLevel::Every } else { CheckLevel::Tick };
        let out = run_term(&term, topo, 20_000, check);
        if out.done {
            assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                "WRONG NF on {} ({})", oracle::show(&term), topo.name());
            pass += 1;
            ints += out.ints;
            transport += out.transport;
        } else if out.stuck { stuck += 1; } else { cap += 1; }
    }
    println!("stage2 [{}]: {pass} pass, {stuck} stuck, {cap} tick-capped, {skip} skipped", topo.name());
    println!("  ledger over passes: interactions={ints} transport={transport}");
    assert_eq!(cap, 0, "tick budget should never bind on this corpus (monotone schedule)");
    // Liveness is a measurement; only catastrophic regressions fail the suite. The floors
    // pin the measured baselines (bilayer 179/400, full3d 201/400 under the bare schedule).
    let denom = pass + stuck;
    let floor_ok = match topo {
        Topo::Bilayer => pass * 5 >= denom * 2, // ≥40%
        Topo::Full3D => pass * 2 >= denom,      // ≥50%
    };
    assert!(denom > 0 && floor_ok, "liveness collapsed on {}: {pass}/{denom}", topo.name());
}

#[test]
fn differential_bilayer() { differential(Topo::Bilayer); }

#[test]
fn differential_full3d() { differential(Topo::Full3D); }

/// The stamp planner (precomputed-workshop fire): correctness is gated absolutely on the
/// same corpus; liveness per mode is measured and printed. No floor yet — the numbers ARE
/// the experiment.
#[test]
fn differential_fire_modes() {
    for topo in TOPOS {
        for (mode, label) in [(FireMode::Stamp, "stamp"), (FireMode::StampThenSearch, "stamp+search")] {
            let mut rng = Lcg(999);
            let (mut pass, mut stuck, mut cap, mut skip) = (0u32, 0u32, 0u32, 0u32);
            for i in 0..400 {
                let term = rng.rand_term(3 + (i % 3));
                let Some(want) = oracle_nf(&term) else { skip += 1; continue };
                let check = if i % 8 == 0 { CheckLevel::Every } else { CheckLevel::Tick };
                let out = run_term_with(&term, topo, mode, 20_000, check);
                if out.done {
                    assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                        "WRONG NF on {} ({}, {label})", oracle::show(&term), topo.name());
                    pass += 1;
                } else if out.stuck { stuck += 1; } else { cap += 1; }
            }
            println!("stage2 [{} · {label}]: {pass} pass, {stuck} stuck, {cap} tick-capped, {skip} skipped", topo.name());
            assert_eq!(cap, 0, "tick budget bound on {} ({label})", topo.name());
            assert!(pass > 0, "no term completed on {} ({label})", topo.name());
        }
    }
}

#[test]
fn polymer_tidy() {
    // After quiescence (plus the tick's own retract fixpoint), no retractable U-turn may
    // remain anywhere, and the projection must hold — the polymer moves change geometry,
    // never connectivity.
    for topo in TOPOS {
        let term = ap(s(Term::L), Term::L); // completes on every topology
        let mut sim = Sim::load(&term, topo);
        for _ in 0..50_000 {
            if sim.shadow.all_active_pairs().is_empty() { break; }
            if sim.tick(CheckLevel::Every) == 0 { break; }
        }
        // let any post-NF tidying settle (flips can expose one more retraction)
        for _ in 0..1_000 { if sim.tick(CheckLevel::Every) == 0 { break; } }
        sim.grid.check_projection(&sim.shadow);
        assert!(sim.shadow.all_active_pairs().is_empty(), "stem-app should complete on {}", topo.name());
        for p in sim.grid.wire_positions() {
            assert!(plan_retract(&sim.grid, p).is_none(), "retractable U left at {p:?} on {}", topo.name());
        }
        println!("[{}] polymer_tidy: strands at quiescence = {}", topo.name(), sim.grid.total_strands());
    }
}
