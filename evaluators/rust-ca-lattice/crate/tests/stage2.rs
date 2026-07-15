//! Lattice differential against the independent oracle on both Bilayer and Full3D, with
//! projection checks and all active geometry mechanisms enabled.
//!
//! Correctness requires zero wrong normal forms, zero invariant violations, and deterministic
//! results. Liveness is measured for the generated corpus and required for the explicit
//! must-complete pins.

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
    // Terms whose completion is part of the current liveness contract. Bilayer has the
    // smaller set because its two planes provide less routing freedom.
    for topo in TOPOS {
        let out = check_pin(ap(s(Term::L), Term::L), topo, true); // stem application
        assert!(out.transport > 0, "reel-in should have happened");
        check_pin(ap(f2(Term::L, Term::L), Term::L), topo, true); // fork dispatch → w
    }
    let t3 = Topo::Full3D;
    check_pin(oracle::chain_k(1), t3, true);
    check_pin(ap(ap(oracle::k(), Term::L), s(Term::L)), t3, true); // K erases
    check_pin(ap(f2(s(Term::L), Term::L), Term::L), t3, true); // S-rule shares
    // Pressure, chain transport, dispatch, and deep-copy coverage.
    check_pin(ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))), t3, true); // kargs
    check_pin(oracle::chain_k(2), t3, true);
    check_pin(oracle::chain_k(3), t3, true);
    check_pin(ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)), t3, true); // selF end to end
    check_pin(ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)), t3, true); // share: δ deep-copies a fork
    check_pin(oracle::disp_t(), t3, true); // A → T1 → Sel end to end
    // Deep-spine tension and retraction coverage.
    check_pin(oracle::chain_k(4), t3, true);
    // Chain transport on the constrained Bilayer topology.
    check_pin(oracle::chain_k(2), Topo::Bilayer, true);
}

#[test]
fn pins_status() {
    // Additional topology status cases: correctness remains mandatory; completion is
    // reported without making these cases part of the liveness gate.
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
        let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_term(&term, topo, 20_000, check)
        })).unwrap_or_else(|failure| {
            eprintln!("stage2 run panicked on corpus term {i} ({}): {}",
                topo.name(), oracle::show(&term));
            std::panic::resume_unwind(failure)
        });
        if out.done {
            assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                "WRONG NF on {} ({})", oracle::show(&term), topo.name());
            pass += 1;
            ints += out.ints;
            transport += out.transport;
        } else if out.stuck { stuck += 1; } else { cap += 1; }
    }
    println!("lattice [{}]: {pass} pass, {stuck} stuck, {cap} tick-capped, {skip} skipped", topo.name());
    println!("  ledger over passes: interactions={ints} transport={transport}");
    // The schedule is nonmonotone: χ-licensed occupant steps and slides can keep ticks
    // busy. The quiet streak plus shadow-progress drought must catch every non-completing
    // run long before the budget binds.
    assert_eq!(cap, 0, "tick budget should never bind on this corpus (stall detection covers churn)");
    // Liveness is a measurement; only catastrophic regressions fail the suite. The floors
    // pin the measured baselines with slack (bilayer 271/400, full3d 366/400 under the
    // sequential schedule with ψ/χ and the tension survey).
    let denom = pass + stuck;
    let floor_ok = match topo {
        Topo::Bilayer => pass * 5 >= denom * 3, // ≥60%
        Topo::Full3D => pass * 5 >= denom * 4,  // ≥80%
    };
    assert!(denom > 0 && floor_ok, "liveness collapsed on {}: {pass}/{denom}", topo.name());
}

#[test]
fn differential_bilayer() { differential(Topo::Bilayer); }

#[test]
fn differential_full3d() { differential(Topo::Full3D); }

/// The stamp planner (precomputed-workshop fire): correctness is gated absolutely on the
/// same corpus; liveness per mode is measured and printed. No floor yet — the numbers ARE
/// the experiment. Ignored by default (eight extra corpus runs ≈ most of the suite's
/// wall-clock); the commit gate is `cargo test --release -- --include-ignored`, and the
/// iteration loop is `probe-corpus`.
#[test]
#[ignore = "8 extra corpus runs; commit gate only (--include-ignored)"]
fn differential_fire_modes() {
    for topo in TOPOS {
        for (mode, label) in [(FireMode::Stamp, "stamp"), (FireMode::StampThenSearch, "stamp+search"), (FireMode::Grow, "grow"), (FireMode::GrowThenSearch, "grow+search")] {
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
            println!("lattice [{} · {label}]: {pass} pass, {stuck} stuck, {cap} tick-capped, {skip} skipped", topo.name());
            assert_eq!(cap, 0, "tick budget bound on {} ({label}) — stall detection should cover churn", topo.name());
            assert!(pass > 0, "no term completed on {} ({label})", topo.name());
        }
    }
}

#[test]
fn survey_oracle() {
    // The tension survey must converge, on static geometry, to the whole-wire truth:
    // for each strand side, exactly the set of outbound travel directions between this
    // strand and the far port (None where the chain does not reach a port). Checked at
    // load and again mid-run, on both topologies — the flip rule's inputs are only as
    // sound as this census.
    use rust_ca_lattice::lattice::{step, Cell, Pos, Strand};
    for topo in TOPOS {
        for (label, term, ticks) in [
            ("load", ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)), 0u32),
            ("mid-run", oracle::chain_k(3), 60),
        ] {
            let mut sim = Sim::load(&term, topo);
            for _ in 0..ticks {
                if sim.shadow.all_active_pairs().is_empty() { break; }
                sim.tick(CheckLevel::Tick);
            }
            for _ in 0..5_000 { if sim.grid.survey_step() == 0 { break; } }
            assert_eq!(sim.grid.survey_step(), 0, "survey did not converge ({label}, {})", topo.name());
            let strands: Vec<(Pos, Strand)> = sim.grid.cells.iter().filter_map(|(p, c)| match c {
                Cell::Wire(w) => Some(w.iter().map(|s| (*p, s)).collect::<Vec<_>>()),
                _ => None,
            }).flatten().collect();
            for (p, st) in strands {
                for (i, f) in [st.a, st.b].into_iter().enumerate() {
                    let (mut dist, mut cur, mut face) = ([0u8; 6], p, f);
                    let mut hop = 0u32;
                    let mut ok = false;
                    for _ in 0..100_000 {
                        let q = step(cur, face);
                        match sim.grid.cells.get(&q) {
                            Some(Cell::Agent(ag)) => { ok = ag.port_at(face.opp()).is_some(); break; }
                            Some(Cell::Wire(w)) => match w.with_he(face.opp()) {
                                Some(t) => {
                                    let h = t.other(face.opp());
                                    hop += 1;
                                    if dist[h as usize] == 0 { dist[h as usize] = hop.min(255) as u8; }
                                    cur = q; face = h;
                                }
                                None => break,
                            },
                            _ => break,
                        }
                    }
                    let want = if ok { Some(dist) } else { None };
                    assert_eq!(st.survey[i], want,
                        "survey mismatch at {p:?} side {i} ({label}, {})", topo.name());
                }
            }
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
