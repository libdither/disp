//! Run one term on the mesh and print what it cost.
//!
//!   mesh-run <term | workload[:n]> [--grid WxH] [--k K] [--fifo B] [--no-check]
//!
//! A term is `@(F(L,L),L)` notation or ternary; a workload is one of term::WORKLOADS,
//! e.g. `fib:6`. The answer is always compared with the independent oracle.

use rust_ic_mesh::mesh::{Config, KIND_NAMES};
use rust_ic_mesh::{run, term};
use rust_ca_lattice::oracle::{self, Fuel};
use std::time::Instant;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut cfg = Config::default();
    let mut check = true;
    let mut src = None;
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--grid" => {
                let v = it.next().expect("--grid WxH");
                let (w, h) = v.split_once('x').expect("--grid WxH");
                cfg.w = w.parse().unwrap();
                cfg.h = h.parse().unwrap();
            }
            "--k" => cfg.k = it.next().unwrap().parse().unwrap(),
            "--fifo" => cfg.fifo = it.next().unwrap().parse().unwrap(),
            "--fill" => cfg.init_fill = it.next().unwrap().parse().unwrap(),
            "--no-check" => check = false,
            "--dump" => {}
            _ => src = Some(a.clone()),
        }
    }
    let src = src.expect("usage: mesh-run <term | workload[:n]>");
    let t = match src.split_once(':') {
        Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
        _ => term::workload(&src, 0).unwrap_or_else(|| term::parse(&src).expect("bad term")),
    };
    let want = oracle::nf(t.clone(), &mut Fuel(200_000_000)).ok().map(|w| oracle::show(&w));
    let t0 = Instant::now();
    if args.iter().any(|a| a == "--dump") {
        let mut m = run::load(&t, cfg, true).unwrap();
        m.run(50_000_000);
        for l in m.dump_waiting() { println!("{l}"); }
        if let Err(e) = m.check_projection() { println!("projection: {e}"); }
        return;
    }
    let rep = run::run(&t, cfg, check, 50_000_000).unwrap_or_else(|e| panic!("{e}"));
    let dt = t0.elapsed().as_secs_f64();
    let s = &rep.mesh.stats;
    let verdict = match (&rep.answer, &want) {
        (Some(a), Some(w)) if a == w => "MATCHES ORACLE",
        (Some(_), Some(_)) => "WRONG ANSWER",
        (Some(_), None) => "oracle out of fuel",
        (None, _) => "no answer",
    };
    println!("outcome {} — {verdict}", rep.outcome);
    if let Some(a) = &rep.answer {
        let shown = if a.len() > 200 { format!("{}…", &a[..200]) } else { a.clone() };
        println!("answer  {shown}");
    }
    println!("grid {}x{} k={} fifo={}  ticks {}  fires {}  ({:.2} fires/tick)",
        cfg.w, cfg.h, cfg.k, cfg.fifo, s.ticks, s.fires, s.fires as f64 / s.ticks.max(1) as f64);
    println!("hops {}  ({:.1}/fire)  events {}  local fires {:.0}%",
        s.hops, s.hops as f64 / s.fires.max(1) as f64, s.events, 100.0 * s.local_fires as f64 / s.fires.max(1) as f64);
    let sent: Vec<String> = KIND_NAMES.iter().zip(s.sent.iter()).map(|(k, n)| format!("{k} {n}")).collect();
    println!("sent    {}", sent.join(", "));
    println!("peak live {}  inds {} (end {})  reserved {}  in flight {}  outbox {}  event queue {}  reserve hops {}",
        s.peak_live, s.peak_inds, s.inds, s.peak_reserved, s.peak_in_flight, s.max_outbox, s.max_events, s.max_reserve_hops);
    println!("wall {:.3}s  ({:.1} M hops/s, {:.2} M fires/s)", dt, s.hops as f64 / dt / 1e6, s.fires as f64 / dt / 1e6);
}
