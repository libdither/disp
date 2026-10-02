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
    let mut profile = false;
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
            "--wa" => p.w_aux = it.next().unwrap().parse().unwrap(),
            "--fill" => p.init_fill = it.next().unwrap().parse().unwrap(),
            "--seed" => p.seed = it.next().unwrap().parse().unwrap(),
            "--budget" => budget = it.next().unwrap().parse().unwrap(),
            "--check" => check = it.next().unwrap().parse().unwrap(),
            "--progress" => progress = it.next().unwrap().parse().unwrap(),
            "--spread" => p.spread = it.next().unwrap().parse().unwrap(),
            "--profile" => profile = true,
            "--pulse" => p.pulse = true,
            "--phop" => p.p_hop = it.next().unwrap().parse().unwrap(),
            "--margolus" => p.margolus = true,
            "--gc" => p.gc = true,
            "--block" => p.block = true,
            "--lazy" => p.lazy = true,
            "--idle" => p.idle_tension = it.next().unwrap().parse().unwrap(),
            "--active" => p.active = it.next().unwrap().parse().unwrap(),
            "--swap" => p.swap = it.next().unwrap().parse().unwrap(),
            "--agents" => p.agent_turns = it.next().unwrap().parse().unwrap(),
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
    let done = if profile {
        // Time-averaged count of wanted consumers in each waiting state, sampled four times a
        // clock, plus the share of clocks where some consumer had its partner in reach.
        let (mut acc, mut lens, mut ready, mut fin) = ([0f64; 5], [0f64; 2], 0f64, false);
        while l.stats.proposals < budget && !fin {
            let c0 = l.stats.clocks;
            fin = l.run((l.stats.proposals + (l.live_sites() / 4).max(1) as u64).min(budget));
            let dc = l.stats.clocks - c0;
            let (n, len) = l.wait_profile();
            for i in 0..5 { acc[i] += n[i] as f64 * dc; }
            for i in 0..2 { lens[i] += len[i] as f64 * dc; }
            if n[0] > 0 { ready += dc; }
        }
        let c = l.stats.clocks.max(1e-9);
        println!("wanted consumers on average: in reach {:.2}  walking to partner {:.2} (length {:.1})  carrying demand {:.2} (length {:.1})  waiting on a computation {:.2}  erasers parked {:.1}",
            acc[0] / c, acc[1] / c, lens[0] / acc[1].max(1e-9), acc[2] / c, lens[1] / acc[2].max(1e-9), acc[3] / c, acc[4] / c);
        println!("clocks per rewrite {:.1}; some pair in reach during {:.0}% of clocks", c / l.stats.fires.max(1) as f64, 100.0 * ready / c);
        fin
    } else if progress == 0 { l.run(budget) } else {
        let mut fin = false;
        while l.stats.proposals < budget && !fin {
            let next = (l.stats.proposals + progress).min(budget);
            fin = l.run(next);
            let (lens, agents) = l.demand_report();
            println!("clocks {:>9.0}  fires {:>6}  agents {:>5}  strands {:>5}  wanted {:>3} principal lengths {:?}",
                l.stats.clocks, l.stats.fires, agents, l.stats.strands, lens.len(), &lens[..lens.len().min(12)]);
        }
        fin
    };
    let dt = t0.elapsed().as_secs_f64();
    let ans = l.readback().map(|t| oracle::show(&t));
    let proj = l.check_projection();
    let s = &l.stats;
    println!("{} — answer {} (want {}) projection {:?}", if done { "DONE" } else { "UNFINISHED" },
        ans.as_deref().unwrap_or("-"), want.as_deref().unwrap_or("?"), proj.err());
    println!("clocks {:.0}  walker steps ok {} / no seat {} / no lane {} / energy {}  demand by pulse {}  duplicators collected {}", s.clocks, s.walk_ok, s.walk_fail[0], s.walk_fail[1], s.walk_fail[2], s.pulses, s.collected);
    println!("proposals {}  fires {} (blocked {})  swaps {}  hops {}  folds {}  flips {}  strands {} (peak {})  peak live sites {}  fullest site {}  {:.2}s",
        s.proposals, s.fires, s.blocked_fires, s.swaps, s.hops, s.folds, s.flips, s.strands, s.peak_strands, s.peak_live, s.peak_site, dt);
    let blocked: Vec<String> = s.blocked_rule.iter().enumerate().filter(|(_, &n)| n > 0)
        .map(|(i, n)| format!("{}·{} {n}", rust_ca_lattice::rules::RULES[i].consumer.name(), rust_ca_lattice::rules::RULES[i].producer.name())).collect();
    println!("blocked by lanes {}; by rule: {}", s.blocked_lanes, blocked.join(", "));
}
