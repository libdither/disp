//! How small can a tile's queues be? Runs the soak corpus and the benchmark programs with
//! bounded outbox and event queues, and counts answers, out-of-space and deadlocks.

use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ic_mesh::mesh::Config;
use rust_ic_mesh::{run, term};

fn main() {
    let mut terms = vec![];
    let mut rng = Lcg(20260730);
    for i in 0..160 {
        let t = rng.rand_term(3 + (i % 4));
        if let Ok(w) = oracle::nf(t.clone(), &mut Fuel(5_000)) { terms.push((t, oracle::show(&w))); }
    }
    for (name, n) in [("fib", 3), ("sort", 2), ("exp", 2), ("size-self", 0)] {
        let t = term::workload(name, n).unwrap();
        let w = oracle::show(&oracle::nf(t.clone(), &mut Fuel(2_000_000_000)).unwrap());
        terms.push((t, w));
    }
    println!("| outbox | event queue | speculation | done | wrong | out of space | deadlock | max ticks |");
    println!("|---|---|---|---|---|---|---|---|");
    for (ob, ev) in [(0, 0), (64, 16), (32, 8), (24, 4), (16, 2), (13, 1)] {
        for spec in [0, 1] {
            let (mut done, mut wrong, mut oom, mut dead, mut ticks) = (0, 0, 0, 0, 0u64);
            for (t, want) in &terms {
                let cfg = Config { w: 48, h: 48, speculate: spec, outbox_cap: ob, events_cap: ev, ..Config::default() };
                let rep = run::run(t, cfg, false, 30_000_000).unwrap();
                ticks = ticks.max(rep.mesh.tick);
                match rep.outcome {
                    "done" if rep.answer.as_deref() == Some(want.as_str()) => done += 1,
                    "done" => wrong += 1,
                    "out-of-space" => oom += 1,
                    "deadlock" => dead += 1,
                    other => panic!("{other}"),
                }
            }
            println!("| {} | {} | {} | {done} | {wrong} | {oom} | {dead} | {ticks} |",
                if ob == 0 { "∞".into() } else { ob.to_string() }, if ev == 0 { "∞".into() } else { ev.to_string() },
                if spec == 0 { "off" } else { "≥1 free" });
        }
    }
}
