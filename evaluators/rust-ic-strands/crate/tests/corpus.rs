//! The cascade's soak corpus on the strand lattice, in the configurations the README
//! reports. Every run must finish with the oracle's answer and project exactly onto the
//! abstract net; a few runs re-check every invariant after every single move.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ic_strands::lattice::{Lattice, Params};

fn corpus() -> Vec<(Term, String)> {
    let mut rng = Lcg(20260730);
    (0..160).filter_map(|i| {
        let t = rng.rand_term(3 + (i % 4));
        oracle::nf(t.clone(), &mut Fuel(5_000)).ok().map(|w| (t, oracle::show(&w)))
    }).collect()
}

fn run(t: &Term, p: Params, check_every: u64) -> (bool, Option<String>, Result<(), String>) {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    let mut l = Lattice::load(p, net, out).expect("load");
    l.check_every = check_every;
    let done = l.run(50_000_000);
    (done, l.readback().map(|t| oracle::show(&t)), l.check_projection())
}

fn all_finish(p: Params) {
    for (i, (t, want)) in corpus().iter().enumerate() {
        let (done, got, proj) = run(t, Params { seed: i as u64 + 1, ..p }, 0);
        assert!(done, "term {i} did not finish: {}", oracle::show(t));
        assert_eq!(got.as_deref(), Some(want.as_str()), "term {i}: WRONG ANSWER");
        proj.unwrap_or_else(|e| panic!("term {i}: {e}"));
    }
}

fn base() -> Params { Params { w: 48, h: 48, temp: 2.0, ..Params::default() } }

#[test]
fn one_agent_per_site_in_3d() {
    all_finish(Params { depth: 6, k: 1, lanes: 3, lazy: true, ..base() });
}

#[test]
fn block_rewrites_in_3d() {
    all_finish(Params { depth: 6, k: 2, lanes: 2, block: true, lazy: true, ..base() });
}

/// The chip schedule: disjoint 2×2×2 blocks each making one move per clock, demand pulses,
/// duplicators collected by erasers.
#[test]
fn margolus_blocks_with_pulses_in_3d() {
    all_finish(Params { depth: 6, k: 2, lanes: 3, block: true, lazy: true, pulse: true, margolus: true, gc: true, agent_turns: 0.8, ..base() });
}

/// Erasers collecting duplicators, with every invariant re-checked after every move.
#[test]
fn collection_keeps_the_projection_exact() {
    let t = rust_ic_mesh::term::workload("share-tower", 3).unwrap();
    let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000)).unwrap());
    for margolus in [false, true] {
        let p = Params { w: 24, h: 24, depth: 4, k: 2, lanes: 3, block: true, lazy: true, pulse: true, gc: true, margolus,
                         swap: 1.0, agent_turns: 0.8, ..base() };
        let mut net = Net::new();
        let root = net.build(&t);
        let (_, out) = net.drive(root);
        let mut l = Lattice::load(p, net, out).expect("load");
        l.check_every = 1;
        assert!(l.run(50_000_000), "margolus {margolus}");
        assert!(l.stats.collected > 0, "nothing collected");
        assert_eq!(l.readback().map(|t| oracle::show(&t)).as_deref(), Some(want.as_str()));
        l.check_projection().unwrap();
    }
}

#[test]
fn one_agent_per_site_in_2d_eager() {
    all_finish(Params { k: 1, lanes: 4, ..base() });
}

/// Every invariant (mutual pairing, strands seen from both sides, every live port wired)
/// after every move, on terms that exercise spills, cross-link rewrites and block rewrites.
#[test]
fn invariants_hold_after_every_move() {
    let terms = ["@(@(S(L),S(L)),L)", "@(F(S(L),S(L)),L)", "@(@(F(F(L,S(L)),L),S(L)),F(L,L))"];
    let configs = [
        Params { k: 2, lanes: 3, ..base() },
        Params { k: 1, lanes: 4, ..base() },
        Params { k: 2, lanes: 2, block: true, lazy: true, ..base() },
        Params { depth: 4, k: 1, lanes: 3, lazy: true, w: 24, h: 24, ..base() },
        Params { depth: 4, k: 2, lanes: 3, block: true, lazy: true, pulse: true, swap: 1.0, agent_turns: 0.8, w: 16, h: 16, ..base() },
        Params { depth: 4, k: 2, lanes: 3, block: true, lazy: true, pulse: true, margolus: true, swap: 1.0, w: 16, h: 16, ..base() },
    ];
    for src in terms {
        let t = rust_ic_mesh::term::parse(src).unwrap();
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(10_000)).unwrap());
        for p in configs {
            let (done, got, proj) = run(&t, p, 1);
            assert!(done, "{src} {p:?}");
            assert_eq!(got.as_deref(), Some(want.as_str()), "{src}");
            proj.unwrap();
        }
    }
}
