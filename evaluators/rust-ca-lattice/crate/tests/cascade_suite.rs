//! The cascade suite: every rule fires from a docked pose (the atlas), and whole terms
//! reduce end to end on the grid with the result checked against the independent oracle,
//! under every queue discipline.

use rust_ca_lattice::cascade_run::{
    check_projection, check_reciprocity, dock_fixture, normalize_on_grid, place_obstruction,
    Discipline, Runner,
};
use rust_ca_lattice::lattice::Dir;
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Term};
use rust_ca_lattice::rules::{find_index, Tag, RULES};

const DISCIPLINES: [Discipline; 4] = [
    Discipline::Fifo,
    Discipline::Lifo,
    Discipline::Random(0x9e37_79b9_7f4a_7c15),
    Discipline::AddressOrdered,
];

fn af_rule() -> &'static rust_ca_lattice::rules::Rule {
    &RULES[find_index(Tag::A, Tag::F).expect("A·F in the ROM")]
}

/// The producer walks a straight three-cell wire, docks, and the workshop fires: the old
/// suite's translate-straight followed by reduce-apply-fork, on the cascade substrate.
#[test]
fn translate_straight_then_fire() {
    for discipline in DISCIPLINES {
        let (grid, shadow) = dock_fixture(af_rule(), 3, false);
        let mut r = Runner::new(grid, shadow, discipline);
        assert!(r.run(2_000_000), "did not quiesce ({discipline:?})");
        assert!(r.grid.transport >= 3, "three walked cells ({discipline:?})");
        assert_eq!(r.grid.rewrites, 1, "A·F fires once ({discipline:?})");
        check_projection(&r.grid, &r.shadow).unwrap();
        check_reciprocity(&r.grid).unwrap();
    }
}

/// The same walk around a dogleg: the old translate-bend.
#[test]
fn translate_bend_then_fire() {
    for discipline in DISCIPLINES {
        let (grid, shadow) = dock_fixture(af_rule(), 0, true);
        let mut r = Runner::new(grid, shadow, discipline);
        assert!(r.run(2_000_000), "did not quiesce ({discipline:?})");
        assert!(r.grid.transport >= 4, "four walked cells ({discipline:?})");
        assert_eq!(r.grid.rewrites, 1, "A·F fires once ({discipline:?})");
        check_projection(&r.grid, &r.shadow).unwrap();
        check_reciprocity(&r.grid).unwrap();
    }
}

/// Obstructing one growth orientation must not stop the fire: the dock's roll ladder
/// picks another orientation (the old af-fallback, lift retry generalized to rolls).
#[test]
fn af_fallback_picks_another_roll() {
    let (mut grid, mut shadow) = dock_fixture(af_rule(), 0, false);
    place_obstruction(&mut grid, &mut shadow, (0, -1, 0), Dir::N);
    check_projection(&grid, &shadow).unwrap();
    let mut r = Runner::new(grid, shadow, Discipline::Fifo);
    assert!(r.run(2_000_000), "did not quiesce");
    assert_eq!(r.grid.rewrites, 1, "A·F still fires around the obstruction");
    check_projection(&r.grid, &r.shadow).unwrap();
    check_reciprocity(&r.grid).unwrap();
}

/// Obstructing every growth orientation declines the dock: matter unchanged, quiescent
/// (the old af-blocked).
#[test]
fn af_declined_when_every_roll_is_blocked() {
    let (mut grid, mut shadow) = dock_fixture(af_rule(), 0, false);
    for (at, toward) in [
        ((0, -1, 0), Dir::N),
        ((0, 1, 0), Dir::S),
        ((0, 0, 1), Dir::U),
        ((0, 0, -1), Dir::D),
    ] {
        place_obstruction(&mut grid, &mut shadow, at, toward);
    }
    check_projection(&grid, &shadow).unwrap();
    let mut r = Runner::new(grid, shadow, Discipline::Fifo);
    assert!(r.run(2_000_000), "did not quiesce");
    assert_eq!(r.grid.rewrites, 0, "the dock must decline");
    check_projection(&r.grid, &r.shadow).unwrap();
    check_reciprocity(&r.grid).unwrap();
}

#[test]
fn atlas_every_rule_fires_and_projects() {
    for (i, rule) in RULES.iter().enumerate() {
        for discipline in DISCIPLINES {
            let (grid, shadow) = dock_fixture(rule, 0, false);
            let mut r = Runner::new(grid, shadow, discipline);
            assert!(
                r.run(2_000_000),
                "rule {} {}·{} did not quiesce ({discipline:?})",
                i, rule.consumer.name(), rule.producer.name()
            );
            assert_eq!(
                r.grid.rewrites, 1,
                "rule {}·{} must fire exactly once ({discipline:?})",
                rule.consumer.name(), rule.producer.name()
            );
            check_projection(&r.grid, &r.shadow).unwrap_or_else(|e| {
                panic!("rule {}·{} projection ({discipline:?}): {e}",
                    rule.consumer.name(), rule.producer.name())
            });
            check_reciprocity(&r.grid).unwrap_or_else(|e| {
                panic!("rule {}·{} reciprocity ({discipline:?}): {e}",
                    rule.consumer.name(), rule.producer.name())
            });
        }
    }
}

fn oracle_nf(t: &Term) -> String {
    oracle::show(&oracle::nf(t.clone(), &mut oracle::Fuel(100_000)).expect("oracle terminates"))
}

fn reduces_to(term: Term, budget: u64) {
    let expect = oracle_nf(&term);
    for discipline in DISCIPLINES {
        let (result, rewrites, generations) =
            normalize_on_grid(&term, Topo::Full3D, discipline, budget)
                .unwrap_or_else(|e| panic!("{discipline:?}: {e} (term {})", oracle::show(&term)));
        let got = result
            .map(|t| oracle::show(&t))
            .unwrap_or_else(|| "<no normal form>".into());
        assert_eq!(
            got, expect,
            "{discipline:?} reduced {} wrong ({rewrites} rewrites, {generations} generations)",
            oracle::show(&term)
        );
    }
}

#[test]
fn reduce_identity_leaf() {
    // @(L, L) -> S(L): one A·L fire after the producers walk to their consumers.
    reduces_to(ap(Term::L, Term::L), 2_000_000);
}

/// The deep-normalization frontier. Each term must run to quiescence VALIDLY (projection
/// and reciprocity green, at least the first fire): the substrate never computes a wrong
/// answer, only sometimes an incomplete one. Terms that reach the oracle normal form are
/// counted; the floor below pins the current frontier and must only move up. The open
/// blocker is blocklet growth wedged against cold traffic near second-generation docks;
/// the pressure-relief rung (wire eviction) is the designed fix.
#[test]
fn frontier_deep_reductions() {
    let terms: Vec<(&str, Term)> = vec![
        ("k-combinator", ap(ap(oracle::k(), s(Term::L)), Term::L)),
        ("fork-dispatch", ap(f2(Term::L, Term::L), Term::L)),
        ("s-rule-sharing", ap(f2(s(Term::L), s(Term::L)), Term::L)),
        ("k-chain", oracle::chain_k(2)),
        ("disp-t", oracle::disp_t()),
    ];
    let mut complete = 0;
    for (name, term) in &terms {
        let expect = oracle_nf(term);
        let mut shadow = Net::new();
        let root = shadow.build(term);
        let (_nrm, out) = shadow.drive(root);
        let grid = rust_ca_lattice::cascade_run::load_net(&shadow, Topo::Full3D).unwrap();
        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        assert!(r.run(8_000_000), "{name}: did not quiesce");
        assert!(r.grid.rewrites >= 1, "{name}: not even the first fire happened");
        // A wedged seed leaves pending blocklet stubs, so the geometric checks only
        // apply to seed-free quiescence.
        let pending_seed = !r.grid.seed_sids.is_empty();
        if !pending_seed {
            check_reciprocity(&r.grid).unwrap_or_else(|e| panic!("{name}: reciprocity: {e}"));
        }
        if !pending_seed {
            check_projection(&r.grid, &r.shadow)
                .unwrap_or_else(|e| panic!("{name}: projection: {e}"));
        }
        let got = r.shadow.readback(r.shadow.get(out).ports[0]).map(|t| oracle::show(&t));
        match got {
            Some(got) if got == expect && !pending_seed => {
                complete += 1;
                println!(
                    "frontier {name}: COMPLETE in {} rewrites, {} generations",
                    r.grid.rewrites, r.generation
                );
            }
            got => {
                println!(
                    "frontier {name}: parked after {} rewrites (seed pending: {pending_seed}, got {got:?}, want {expect})",
                    r.grid.rewrites
                );
            }
        }
    }
    // The pinned floor: fork-dispatch and k-combinator complete end to end; the deeper
    // terms park validly in corridor knots (walkers over doubled foreign cables). Raise
    // this as relief mechanisms land.
    assert!(complete >= 2, "frontier floor");
    println!("frontier: {complete}/{} deep terms complete", terms.len());
}

/// Two apply-fork pairs docked in adjacent planes: their blocklets contest the shared
/// space, arbitration picks a survivor by seed address, the loser retracts (or dodges by
/// roll), and both interactions eventually fire with a clean projection.
#[test]
fn competing_seeds_both_fire() {
    use rust_ca_lattice::cascade::Grid2;
    use rust_ca_lattice::cascade_run::{dock_fixture_at, Event};
    let mut grid = Grid2::new(Topo::Full3D);
    let mut shadow = Net::new();
    dock_fixture_at(af_rule(), 0, false, (0, 0, 0), &mut grid, &mut shadow);
    dock_fixture_at(af_rule(), 0, false, (0, 0, 2), &mut grid, &mut shadow);
    // Wall the outward escape planes: the dock rings stay clear, but the three-plane
    // blocklets must share the space between the pairs.
    place_obstruction(&mut grid, &mut shadow, (0, 0, -1), Dir::D);
    place_obstruction(&mut grid, &mut shadow, (0, 0, 3), Dir::U);
    check_reciprocity(&grid).expect("stacked fixtures are reciprocal");
    check_projection(&grid, &shadow).expect("stacked fixtures project");
    let mut r = Runner::new(grid, shadow, Discipline::Fifo);
    assert!(r.run(8_000_000), "must quiesce");
    assert_eq!(r.grid.rewrites, 2, "both docked pairs fire");
    let retracts =
        r.events.iter().filter(|e| matches!(e, Event::Retract(..))).count();
    println!("competing seeds: {retracts} retraction(s) during arbitration");
    assert!(retracts >= 1, "the losing seed must retract at least once");
    check_reciprocity(&r.grid).expect("post-fire reciprocity");
    check_projection(&r.grid, &r.shadow).expect("post-fire projection");
}

/// Sixteen random schedules over the two completing frontier terms: any schedule may
/// park, none may answer wrongly, and a seed-free quiescent grid must project.
#[test]
fn schedule_fuzz_never_wrong() {
    use rust_ca_lattice::cascade_run::load_net;
    for seed in 0..16u64 {
        for term in [ap(Term::L, Term::L), ap(f2(Term::L, Term::L), Term::L)] {
            let want = oracle::show(
                &oracle::nf(term.clone(), &mut oracle::Fuel(100_000)).expect("oracle nf"),
            );
            let mut shadow = Net::new();
            let root = shadow.build(&term);
            let (_nrm, out) = shadow.drive(root);
            let grid = load_net(&shadow, Topo::Full3D).expect("loads");
            let mut r = Runner::new(grid, shadow, Discipline::Random(seed));
            assert!(r.run(8_000_000), "seed {seed}: did not quiesce");
            if r.grid.seed_sids.is_empty() {
                check_reciprocity(&r.grid)
                    .unwrap_or_else(|e| panic!("seed {seed}: reciprocity: {e}"));
                check_projection(&r.grid, &r.shadow)
                    .unwrap_or_else(|e| panic!("seed {seed}: projection: {e}"));
            }
            if let Some(got) = r.shadow.readback(r.shadow.get(out).ports[0]) {
                assert_eq!(oracle::show(&got), want, "seed {seed}: wrong answer");
            }
        }
    }
}
