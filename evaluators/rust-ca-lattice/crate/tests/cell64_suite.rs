//! The Cell64 suite gate: every scenario in the `Must` tier must pass under several fair
//! activation seeds; `Example` tier scenarios are expected to pass too — they are the
//! current reduction frontier and their failure marks the next work item.

use rust_ca_lattice::suite::{self, Scenario, Tier};

const SWEEP_CAP: u64 = 400;

fn check(sc: &Scenario, seed: u64) {
    let (mut grid, mut shadow) = suite::build(sc);
    let outcome = suite::run(sc, &mut grid, &mut shadow, seed, SWEEP_CAP, &mut |_, _, _| {});
    assert!(
        outcome.pass,
        "suite scenario {} (seed {seed:#x}) failed: {}",
        sc.key,
        outcome.fail.as_deref().unwrap_or("unknown"),
    );
}

#[test]
fn must_scenarios_pass_under_adversarial_seeds() {
    for sc in suite::scenarios() {
        if sc.tier != Tier::Must { continue; }
        for seed in [0, 1, 0x9e37_79b9_7f4a_7c15, u64::MAX] {
            check(&sc, seed);
        }
    }
}

#[test]
fn example_scenarios_pass_under_adversarial_seeds() {
    for sc in suite::scenarios() {
        if sc.tier != Tier::Example { continue; }
        for seed in [0, 1, 0x9e37_79b9_7f4a_7c15, u64::MAX] {
            check(&sc, seed);
        }
    }
}
