//! The cascade's soak corpus under a list of lattice configurations: how many finish and how
//! many clocks they take.
//!   strands-sweep "k=8 lanes=4" "k=6 lanes=3 temp=1.0" ...

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ic_strands::lattice::{Lattice, Params};

fn parse(spec: &str) -> Params {
    let mut p = Params::default();
    for kv in spec.split_whitespace() {
        let (k, v) = kv.split_once('=').expect("key=value");
        match k {
            "k" => p.k = v.parse().unwrap(),
            "lanes" => p.lanes = v.parse().unwrap(),
            "grid" => { p.w = v.parse().unwrap(); p.h = p.w; }
            "depth" => p.depth = v.parse().unwrap(),
            "temp" => p.temp = v.parse().unwrap(),
            "crowd" => p.crowd = v.parse().unwrap(),
            "repel" => p.repel = v.parse().unwrap(),
            "press" => p.pressure = v.parse().unwrap(),
            "peak" => p.pressure_peak = v.parse().unwrap(),
            "wp" => p.w_principal = v.parse().unwrap(),
            "wa" => p.w_aux = v.parse().unwrap(),
            "hop" => p.p_hop = v.parse().unwrap(),
            "fill" => p.init_fill = v.parse().unwrap(),
            "spread" => p.spread = v.parse().unwrap(),
            "block" => p.block = v == "1",
            "lazy" => p.lazy = v == "1",
            "idle" => p.idle_tension = v.parse().unwrap(),
            "active" => p.active = v.parse().unwrap(),
            "swap" => p.swap = v.parse().unwrap(),
            "pulse" => p.pulse = v == "1",
            "margolus" => p.margolus = v == "1",
            "gc" => p.gc = v == "1",
            "agents" => p.agent_turns = v.parse().unwrap(),
            _ => panic!("unknown key {k}"),
        }
    }
    p
}

fn main() {
    let mut rng = Lcg(20260730);
    let mut corpus = vec![];
    for i in 0..160 {
        let t = rng.rand_term(3 + (i % 4));
        if let Ok(w) = oracle::nf(t.clone(), &mut Fuel(5_000)) { corpus.push((t, oracle::show(&w))); }
    }
    println!("| config | done | wrong | stuck | unloadable | median clocks | p90 | fires | sec |");
    println!("|---|---|---|---|---|---|---|---|---|");
    let budget: u64 = std::env::var("BUDGET").ok().and_then(|v| v.parse().ok()).unwrap_or(3_000_000);
    for spec in std::env::args().skip(1) {
        let p = parse(&spec);
        let t0 = std::time::Instant::now();
        let (mut done, mut wrong, mut stuck, mut clocks, mut fires, mut unloadable) = (0, 0, 0, vec![], 0u64, 0);
        for (i, (t, want)) in corpus.iter().enumerate() {
            let mut net = Net::new();
            let root = net.build(t);
            let (_, out) = net.drive(root);
            let Ok(mut l) = Lattice::load(Params { seed: i as u64 + 1, ..p }, net, out) else { unloadable += 1; continue };
            let fin = l.run(budget);
            fires += l.stats.fires;
            if !fin { stuck += 1; continue; }
            if l.readback().map(|t| oracle::show(&t)).as_deref() == Some(want.as_str()) && l.check_projection().is_ok() {
                done += 1;
                clocks.push(l.stats.clocks);
            } else {
                wrong += 1;
            }
        }
        clocks.sort_by(f64::total_cmp);
        let q = |f: f64| clocks.get(((clocks.len() as f64 * f) as usize).min(clocks.len().saturating_sub(1))).copied().unwrap_or(0.0);
        println!("| {spec} | {done} | {wrong} | {stuck} | {unloadable} | {:.0} | {:.0} | {fires} | {:.1} |", q(0.5), q(0.9), t0.elapsed().as_secs_f64());
    }
}
