//! Stage-2 differential: reduction ON THE LATTICE vs the independent oracle, with the
//! projection invariant asserted along the way.
//!
//! The gate discipline (the project's differential-oracle discipline at substrate level):
//! CORRECTNESS is absolute — zero wrong normal forms, zero invariant violations, bit
//! determinism. LIVENESS (reaching NF under the plain sequential schedule, which has no
//! relaxation policies yet: no straighten, no drift, no pressure) is MEASURED and
//! reported; only catastrophic regressions fail the suite. The stuck residue is pocket
//! congestion around the reduction focus — LOCAL_CA_DESIGN.md §13's liveness question,
//! which the policy rungs address on top of this sound core.

use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ca_lattice::scheduler::{run_term, CheckLevel, Outcome};

fn oracle_nf(t: &Term) -> Option<String> {
    oracle::nf(t.clone(), &mut Fuel(20_000)).ok().map(|w| oracle::show(&w))
}

/// Correctness gate: if the lattice finishes, the result must match the oracle exactly.
/// Completion itself is asserted only when `must_complete` (verified micro-pins).
fn check_pin(t: Term, must_complete: bool) -> Outcome {
    let want = oracle_nf(&t).expect("pin diverged in oracle");
    let out = run_term(&t, 50_000, CheckLevel::Every);
    if out.done {
        assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
            "wrong NF for {}", oracle::show(&t));
    } else {
        assert!(!must_complete, "must-complete pin stuck: {}", oracle::show(&t));
        println!("pin stuck (liveness residue, correctness intact): {}", oracle::show(&t));
    }
    out
}

#[test]
fn pins_must_complete() {
    // Verified to reach NF under the bare sequential schedule.
    let out = check_pin(ap(f2(Term::L, Term::L), Term::L), true); // fork dispatch → w
    assert!(out.transport > 0, "reel-in should have happened");
    check_pin(oracle::chain_k(1), true);
    check_pin(ap(s(Term::L), Term::L), true); // stem application
}

#[test]
fn pins_correctness_only() {
    // Denser terms currently pocket-jam under the bare schedule (measured residue; the
    // §13 liveness lab lives above this substrate). Correctness still gated.
    check_pin(ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))), false);
    check_pin(oracle::chain_k(4), false);
    check_pin(oracle::disp_t(), false);
    check_pin(ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)), false);
}

#[test]
fn determinism() {
    let t = oracle::chain_k(3);
    let a = run_term(&t, 50_000, CheckLevel::Tick);
    let b = run_term(&t, 50_000, CheckLevel::Tick);
    assert_eq!((a.done, a.ticks, a.ints, a.transport, a.peak_agents),
               (b.done, b.ticks, b.ints, b.transport, b.peak_agents),
        "sequential schedule must be bit-deterministic");
}

#[test]
fn differential_lattice() {
    let mut rng = Lcg(999);
    let (mut pass, mut stuck, mut cap, mut skip) = (0u32, 0u32, 0u32, 0u32);
    let (mut ints, mut transport) = (0u64, 0u64);
    let n_terms = 400;
    for i in 0..n_terms {
        let term = rng.rand_term(3 + (i % 3));
        let Some(want) = oracle_nf(&term) else { skip += 1; continue };
        let check = if i % 8 == 0 { CheckLevel::Every } else { CheckLevel::Tick };
        let out = run_term(&term, 20_000, check);
        if out.done {
            assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                "WRONG NF on {}", oracle::show(&term));
            pass += 1;
            ints += out.ints;
            transport += out.transport;
        } else if out.stuck { stuck += 1; } else { cap += 1; }
    }
    println!("stage2 lattice differential: {pass} pass, {stuck} stuck, {cap} tick-capped, {skip} skipped");
    println!("  ledger over passes: interactions={ints} transport={transport}");
    assert_eq!(cap, 0, "tick budget should never bind on this corpus (monotone schedule)");
    // Liveness is a measurement; only catastrophic regressions fail the suite.
    let denom = pass + stuck;
    assert!(denom > 0 && pass * 2 >= denom, "liveness collapsed: {pass}/{denom}");
}
