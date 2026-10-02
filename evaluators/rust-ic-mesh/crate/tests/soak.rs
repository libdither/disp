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
    let corpus = corpus();
    let (mut done, mut oom) = (0, 0);
    for (i, term, want) in corpus.iter().step_by(3) {
        for side in [1u32, 2, 3] {
            let cfg = Config { w: side, h: side, k: 6, init_fill: 6, ..Config::default() };
            let Ok(rep) = run::run(term, cfg, true, 2_000_000) else { continue };
            match rep.outcome {
                "done" => {
                    assert_eq!(rep.answer.as_deref(), Some(want.as_str()), "term {i} on {side}x{side}: WRONG ANSWER");
                    done += 1;
                }
                "out-of-space" => oom += 1,
                other => panic!("term {i} on {side}x{side}: {other}"),
            }
        }
    }
    println!("tight grids: {done} complete, {oom} out of space");
    assert!(done > 0 && oom > 0, "the squeeze should exercise both outcomes");
}
