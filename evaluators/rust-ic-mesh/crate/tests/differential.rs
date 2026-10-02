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
        let n = (*n).min(3);
        let t = term::workload(name, n).unwrap();
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000_000)).unwrap());
        let rep = run::run(&t, cfg, true, 20_000_000).unwrap();
        assert_eq!(rep.outcome, "done", "{name}:{n}");
        assert_eq!(rep.answer, Some(want), "{name}:{n}");
    }
}

#[test]
fn router_shapes_do_not_change_answers() {
    let t = term::workload("fib", 3).unwrap();
    let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000_000)).unwrap());
    for (k, fifo, ev) in [(6, 1, 1), (8, 2, 1), (16, 4, 1), (8, 4, 3), (12, 1, 2)] {
        let cfg = Config { w: 40, h: 40, k, fifo, events_per_tick: ev, ..Config::default() };
        let rep = run::run(&t, cfg, true, 20_000_000).unwrap();
        assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "k={k} fifo={fifo} ev={ev}");
    }
}
