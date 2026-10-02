//! Run terms on the strand lattice and compare with the oracle.
//!   strands-run <term | workload[:n]> [--k K] [--lanes L] [--grid N] [--3d] [--temp T] [--seed S]

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel};
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{Lattice, Params};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut p = Params::default();
    let mut src = None;
    let mut it = args.iter();
    let mut budget = 200_000_000u64;
    let mut check = 0u64;
    let mut progress = 0u64;
    while let Some(a) = it.next() {
        match a.as_str() {
            "--k" => p.k = it.next().unwrap().parse().unwrap(),
            "--lanes" => p.lanes = it.next().unwrap().parse().unwrap(),
            "--grid" => {
                let v = it.next().unwrap();
                let (w, h) = v.split_once('x').unwrap_or((v, v));
                p.w = w.parse().unwrap();
                p.h = h.parse().unwrap();
            }
            "--depth" => p.depth = it.next().unwrap().parse().unwrap(),
            "--temp" => p.temp = it.next().unwrap().parse().unwrap(),
            "--crowd" => p.crowd = it.next().unwrap().parse().unwrap(),
            "--press" => p.pressure = it.next().unwrap().parse().unwrap(),
            "--wp" => p.w_principal = it.next().unwrap().parse().unwrap(),
            "--fill" => p.init_fill = it.next().unwrap().parse().unwrap(),
            "--seed" => p.seed = it.next().unwrap().parse().unwrap(),
            "--budget" => budget = it.next().unwrap().parse().unwrap(),
            "--check" => check = it.next().unwrap().parse().unwrap(),
            "--progress" => progress = it.next().unwrap().parse().unwrap(),
            "--spread" => p.spread = it.next().unwrap().parse().unwrap(),
            "--block" => p.block = true,
            "--lazy" => p.lazy = true,
            "--idle" => p.idle_tension = it.next().unwrap().parse().unwrap(),
            "--active" => p.active = it.next().unwrap().parse().unwrap(),
            "--swap" => p.swap = it.next().unwrap().parse().unwrap(),
            "--repel" => p.repel = it.next().unwrap().parse().unwrap(),
            _ => src = Some(a.clone()),
        }
    }
    let src = src.expect("term");
    let t = match src.split_once(':') {
        Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
        _ => term::workload(&src, 0).unwrap_or_else(|| term::parse(&src).expect("bad term")),
    };
    let want = oracle::nf(t.clone(), &mut Fuel(100_000_000)).ok().map(|w| oracle::show(&w));
    let mut net = Net::new();
    let root = net.build(&t);
    let (_, out) = net.drive(root);
    let mut l = Lattice::load(p, net, out).expect("load");
    l.check_every = check;
    let t0 = std::time::Instant::now();
    let done = if progress == 0 { l.run(budget) } else {
        let mut fin = false;
        while l.stats.proposals < budget && !fin {
            let next = (l.stats.proposals + progress).min(budget);
            fin = l.run(next);
            let (lens, agents) = l.demand_report();
            println!("sweeps {:>9.0}  fires {:>6}  agents {:>5}  strands {:>5}  wanted {:>3} principal lengths {:?}",
                l.stats.sweeps, l.stats.fires, agents, l.stats.strands, lens.len(), &lens[..lens.len().min(12)]);
        }
        fin
    };
    let dt = t0.elapsed().as_secs_f64();
    let ans = l.readback().map(|t| oracle::show(&t));
    let proj = l.check_projection();
    let s = &l.stats;
    println!("{} — answer {} (want {}) projection {:?}", if done { "DONE" } else { "UNFINISHED" },
        ans.as_deref().unwrap_or("-"), want.as_deref().unwrap_or("?"), proj.err());
    println!("sweeps {:.0}  walker steps ok {} / no seat {} / no lane {} / energy {}", s.sweeps, s.walk_ok, s.walk_fail[0], s.walk_fail[1], s.walk_fail[2]);
    println!("proposals {}  fires {} (blocked {})  swaps {}  hops {}  folds {}  flips {}  strands {} (peak {})  fullest site {}  {:.2}s",
        s.proposals, s.fires, s.blocked_fires, s.swaps, s.hops, s.folds, s.flips, s.strands, s.peak_strands, s.peak_site, dt);
    let blocked: Vec<String> = s.blocked_rule.iter().enumerate().filter(|(_, &n)| n > 0)
        .map(|(i, n)| format!("{}·{} {n}", rust_ca_lattice::rules::RULES[i].consumer.name(), rust_ca_lattice::rules::RULES[i].producer.name())).collect();
    println!("blocked by lanes {}; by rule: {}", s.blocked_lanes, blocked.join(", "));
}
