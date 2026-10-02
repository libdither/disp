//! The numbers in the README: every run checked against the oracle, printed as markdown.
//!
//!   mesh-bench [--quick]

use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ic_mesh::mesh::Config;
use rust_ic_mesh::{run, term};
use std::time::Instant;

fn main() {
    let quick = std::env::args().any(|a| a == "--quick");
    let mut rng = Lcg(20260730);
    let (mut done, mut total, mut fires, mut ticks) = (0, 0, 0u64, 0u64);
    for i in 0..160 {
        let t = rng.rand_term(3 + (i % 4));
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(5_000)) else { continue };
        total += 1;
        let rep = run::run(&t, Config { w: 48, h: 48, ..Config::default() }, true, 10_000_000).unwrap();
        if rep.answer == Some(oracle::show(&want)) { done += 1; }
        fires += rep.mesh.stats.fires;
        ticks += rep.mesh.tick;
    }
    println!("cascade soak corpus: {done} of {total} complete and correct ({fires} rewrites, {ticks} ticks in all)\n");

    println!("| program | speculation | rewrites | ticks | rewrites/tick | hops/rewrite | in own tile | peak live | wall |");
    println!("|---|---|---|---|---|---|---|---|---|");
    let mut cases = vec![("disp-t", 0), ("share-tower", 6), ("discard-tree", 6), ("fib", 4), ("exp", 2), ("sort", 3), ("size-self", 0)];
    if !quick { cases.extend([("fib", 6), ("sort", 5)]); }
    for (name, n) in cases {
        let t = term::workload(name, n).unwrap();
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(2_000_000_000)).unwrap());
        for spec in [0, 4, 1] {
            let side = 96;
            let t0 = Instant::now();
            let rep = run::run(&t, Config { w: side, h: side, speculate: spec, ..Config::default() }, false, 200_000_000).unwrap();
            let dt = t0.elapsed().as_secs_f64();
            let s = &rep.mesh.stats;
            let ok = rep.answer.as_deref() == Some(want.as_str());
            let label = if n == 0 { name.to_string() } else { format!("{name}({n})") };
            let spec_s = if spec == 0 { "off".to_string() } else { format!("≥{spec} free") };
            println!("| {label} | {spec_s} | {} | {} | {:.2} | {:.1} | {:.0}% | {} | {:.2}s{} |",
                s.fires, rep.mesh.tick, s.fires as f64 / rep.mesh.tick as f64,
                s.hops as f64 / s.fires.max(1) as f64, 100.0 * s.local_fires as f64 / s.fires.max(1) as f64,
                s.peak_live, dt, if ok { "" } else { " WRONG" });
        }
    }
}
