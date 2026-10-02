//! The cascade lattice's soak corpus, unchanged (same seed, same depths), on the mesh.
//! The cascade completes 130 of these 160; the mesh must complete every one the oracle
//! can, with every rewrite cross-checked against the abstract net and the final mesh
//! projected onto it. A tight-grid pass then squeezes each term until it runs out of room:
//! running out of space is allowed, a wrong answer or a silent stall is not.

use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ic_mesh::mesh::Config;
use rust_ic_mesh::run;

fn corpus() -> Vec<(u32, Term, String)> {
    let mut rng = Lcg(20260730);
    let mut out = vec![];
    for i in 0..160 {
        let term = rng.rand_term(3 + (i % 4));
        if let Ok(w) = oracle::nf(term.clone(), &mut Fuel(5_000)) {
            out.push((i, term, oracle::show(&w)));
        }
    }
    out
}

#[test]
fn soak_completes_every_term() {
    let corpus = corpus();
    let cfg = Config { w: 48, h: 48, ..Config::default() };
    for (i, term, want) in &corpus {
        let rep = run::run(term, cfg, true, 10_000_000)
            .unwrap_or_else(|e| panic!("term {i} {}: {e}", oracle::show(term)));
        assert_eq!(rep.outcome, "done", "term {i} {}", oracle::show(term));
        assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "term {i}: WRONG ANSWER");
    }
    println!("soak: {} of {} oracle-normalizing terms complete", corpus.len(), corpus.len());
}

#[test]
fn tight_grids_never_lie() {
    let small: &[u32] = &[1, 2, 3, 5];
    let mut terms: Vec<(String, Term, String, &[u32])> = corpus().into_iter().step_by(3)
        .map(|(i, t, w)| (format!("term {i}"), t, w, small)).collect();
    for (name, n) in [("fib", 4), ("sort", 3), ("exp", 2)] {
        let t = rust_ic_mesh::term::workload(name, n).unwrap();
        let w = oracle::show(&oracle::nf(t.clone(), &mut Fuel(100_000_000)).unwrap());
        terms.push((format!("{name}:{n}"), t, w, &[12, 16, 24, 40]));
    }
    let (mut done, mut oom) = (0, 0);
    for (name, term, want, sides) in &terms {
        for &side in *sides {
            let cfg = Config { w: side, h: side, k: 6, init_fill: 6, ..Config::default() };
            let Ok(rep) = run::run(term, cfg, true, 5_000_000) else { continue };
            match rep.outcome {
                "done" => {
                    assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "{name} on {side}x{side}: WRONG ANSWER");
                    done += 1;
                }
                "out-of-space" => oom += 1,
                other => panic!("{name} on {side}x{side}: {other}"),
            }
        }
    }
    println!("tight grids: {done} complete, {oom} out of space");
    assert!(done > 0 && oom > 0, "the squeeze should exercise both outcomes");
}

/// Hardware-sized queues: a 24-flit outbox and a 4-entry event queue per tile carry the
/// whole corpus to the end, even under full speculation (queue-sweep finds the first
/// deadlock at 16/2). Pins the sizing the README quotes.
#[test]
fn hardware_sized_queues_finish_the_corpus() {
    let cfg = Config { w: 48, h: 48, speculate: 1, outbox_cap: 24, events_cap: 4, ..Config::default() };
    for (i, term, want) in &corpus() {
        let rep = run::run(term, cfg, false, 10_000_000).unwrap();
        assert_eq!(rep.outcome, "done", "term {i}");
        assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "term {i}");
    }
}
