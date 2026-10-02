//! Random terms against the independent oracle, every rewrite shadow-checked, plus the
//! named workloads at small sizes. Must complete; must match.

use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ic_mesh::mesh::Config;
use rust_ic_mesh::{run, term};

#[test]
fn random_terms_match_the_oracle() {
    let mut rng = Lcg(7);
    let cfg = Config { w: 32, h: 32, ..Config::default() };
    let mut checked = 0;
    for i in 0..1500 {
        let t = rng.rand_term(2 + i % 5);
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(20_000)) else { continue };
        let rep = run::run(&t, cfg, true, 5_000_000).unwrap();
        assert_eq!(rep.answer, Some(oracle::show(&want)), "term {i}: {}", oracle::show(&t));
        checked += 1;
    }
    assert!(checked > 1000);
}

#[test]
fn workloads_match_the_oracle() {
    let cfg = Config { w: 96, h: 96, ..Config::default() };
    for (name, _, n) in term::WORKLOADS {
        let n = (*n).min(if *name == "sort" { 2 } else { 3 });
        let t = term::workload(name, n).unwrap();
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000_000)).unwrap());
        let rep = run::run(&t, cfg, true, 20_000_000).unwrap();
        assert_eq!(rep.outcome, "done", "{name}:{n}");
        assert_eq!(rep.answer, Some(want), "{name}:{n}");
    }
}

#[test]
fn shapes_and_speculation_do_not_change_answers() {
    for (name, n) in [("fib", 2), ("sort", 2), ("share-tower", 5)] {
        let t = term::workload(name, n).unwrap();
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000_000)).unwrap());
        for (k, fifo, ev) in [(6, 1, 1), (8, 2, 1), (16, 4, 1), (8, 4, 3), (12, 1, 2)] {
            let levels: &[u32] = if k == 8 && fifo == 2 { &[0, 6, 3, 1] } else { &[0, 5] };
            for &speculate in levels {
                let cfg = Config { w: 64, h: 64, k, fifo, events_per_tick: ev, speculate, ..Config::default() };
                let rep = run::run(&t, cfg, true, 20_000_000).unwrap();
                assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "{name}:{n} k={k} fifo={fifo} ev={ev} spec={speculate}");
            }
        }
    }
}

#[test]
fn speculative_random_terms_match_the_oracle() {
    let mut rng = Lcg(99);
    let cfg = Config { w: 32, h: 32, speculate: 1, ..Config::default() };
    for i in 0..600 {
        let t = rng.rand_term(2 + i % 5);
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(20_000)) else { continue };
        let rep = run::run(&t, cfg, true, 5_000_000).unwrap();
        assert_eq!(rep.answer, Some(oracle::show(&want)), "term {i}: {}", oracle::show(&t));
    }
}

/// The cascade's deep-reduction frontier, verbatim: every term completes, with the same
/// number of interactions the abstract net needs.
#[test]
fn cascade_frontier_completes() {
    use rust_ca_lattice::oracle::{ap, f2, s, Term};
    let terms = [
        ("k-combinator", ap(ap(oracle::k(), s(Term::L)), Term::L), 9),
        ("fork-dispatch", ap(f2(Term::L, Term::L), Term::L), 6),
        ("s-rule-sharing", ap(f2(s(Term::L), s(Term::L)), Term::L), 13),
        ("k-chain", oracle::chain_k(2), 15),
        ("disp-t", oracle::disp_t(), 18),
    ];
    for (name, t, fires) in terms {
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000)).unwrap());
        let rep = run::run(&t, Config { w: 16, h: 16, ..Config::default() }, true, 1_000_000).unwrap();
        assert_eq!(rep.answer, Some(want), "{name}");
        assert_eq!(rep.mesh.stats.fires, fires, "{name}");
    }
}
