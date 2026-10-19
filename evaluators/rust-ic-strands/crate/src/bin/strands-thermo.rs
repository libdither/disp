//! The lattice run thermodynamically (thermo.rs) over drives and temperatures: per pair, over seeds,
//! the clocks to a two-way program's answer, and per rewrite kept at the answer (in energy units) the
//! fuel brought in, the heat the bath took, and what undos borrowed from the bath.
//!   strands-thermo [--seeds N] [--work 'add 0 1'] [--drive 4,8,16] [--temp 0.5,0.75,1] [--events MAX] [k=v]…
//! The work is twoway.rs `parse_run`'s; settings are `Params::set` keys on top of `thermal`. A run
//! still short of the answer after MAX events counts as jammed. Each run prints a line on stderr as it ends.

use rust_ic_strands::lattice::{thermal, Lattice, Params};
use rust_ic_strands::twoway;
use std::sync::atomic::{AtomicUsize, Ordering};

struct Out { clocks: f64, done: bool, kept: f64, fuel: f64, heat: f64, borrowed: f64, secs: f64 }

fn run(work: &str, drive: f64, temp: f64, extra: &[String], seed: u64, max: u64) -> Out {
    let (prog, x) = twoway::parse_run(work).unwrap_or_else(|e| panic!("{e}"));
    let want = prog.run(&x, &mut 1_000_000);
    let (net, out) = twoway::net(&prog, &x);
    let side = ((net.live_count() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut p = Params { w: side, h: side, seed, two: true, lazy: false, drive, temp, ..thermal() };
    for a in extra { let (k, v) = a.split_once('=').expect("settings are key=value"); p.set(k, v).unwrap(); }
    let t0 = std::time::Instant::now();
    let mut l = Lattice::load(p, net, out).expect("load");
    let done = l.run_thermo(max);
    if done { assert_eq!(twoway::read(&l.shadow, out).map(|(_, y)| y), want, "{work}: wrong answer"); }
    let g = &l.th.ledger;
    Out { clocks: l.stats.clocks, done, kept: l.records_kept() as f64, fuel: g.fuel as f64 / 4.0, heat: g.to_bath as f64 / 4.0,
          borrowed: g.borrowed as f64 / 4.0, secs: t0.elapsed().as_secs_f64() }
}

fn main() {
    let list = |s: &str| s.split(',').map(|v| v.parse::<f64>().unwrap()).collect::<Vec<_>>();
    let (mut seeds, mut work, mut drives, mut temps, mut max) = (4u64, "add 0 1".to_string(), vec![4.0, 8.0, 16.0], vec![0.5, 0.75, 1.0], 400_000_000u64);
    let mut extra = vec![];
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--seeds" => seeds = it.next().unwrap().parse().unwrap(),
            "--work" => work = it.next().unwrap().clone(),
            "--drive" => drives = list(it.next().unwrap()),
            "--temp" => temps = list(it.next().unwrap()),
            "--events" => max = it.next().unwrap().parse().unwrap(),
            kv if kv.contains('=') => extra.push(kv.to_string()),
            w => panic!("unknown argument {w}"),
        }
    }
    let jobs: Vec<(f64, f64, u64)> = drives.iter().flat_map(|&d| temps.iter().flat_map(move |&t| (1..=seeds).map(move |s| (d, t, s)))).collect();
    let (next, done) = (AtomicUsize::new(0), AtomicUsize::new(0));
    let results = std::sync::Mutex::new((0..jobs.len()).map(|_| None).collect::<Vec<Option<Out>>>());
    std::thread::scope(|sc| {
        for _ in 0..std::thread::available_parallelism().map_or(8, |n| n.get()) {
            sc.spawn(|| loop {
                let i = next.fetch_add(1, Ordering::Relaxed);
                let Some(&(d, t, s)) = jobs.get(i) else { break };
                let o = run(&work, d, t, &extra, s, max);
                let k = done.fetch_add(1, Ordering::Relaxed) + 1;
                eprintln!("[{k}/{}] drive {d} temp {t} seed {s}: {} {:.0} clocks, {:.1}s", jobs.len(), if o.done { "answer at" } else { "jammed after" }, o.clocks, o.secs);
                results.lock().unwrap()[i] = Some(o);
            });
        }
    });
    let results = results.into_inner().unwrap();
    println!("{work}, per rewrite kept: fuel in, heat out, borrowed from the bath (energy units)");
    println!("{:>6} {:>6} {:>7} {:>9} {:>8} {:>8} {:>9} {:>6}", "drive", "temp", "answer", "clocks", "fuel", "heat", "borrowed", "secs");
    for &d in &drives {
        for &t in &temps {
            let rs: Vec<&Out> = jobs.iter().zip(&results).filter(|((d2, t2, _), _)| (*d2, *t2) == (d, t)).map(|(_, r)| r.as_ref().unwrap()).collect();
            let ok: Vec<&&Out> = rs.iter().filter(|r| r.done).collect();
            let mean = |f: &dyn Fn(&Out) -> f64| if ok.is_empty() { f64::NAN } else { ok.iter().map(|r| f(r)).sum::<f64>() / ok.len() as f64 };
            println!("{:>6} {:>6} {:>7} {:>9.0} {:>8.1} {:>8.1} {:>9.1} {:>6.1}", d, t, format!("{}/{}", ok.len(), rs.len()), mean(&|r| r.clocks),
                     mean(&|r| r.fuel / r.kept), mean(&|r| r.heat / r.kept), mean(&|r| r.borrowed / r.kept), rs.iter().map(|r| r.secs).sum::<f64>() / rs.len() as f64);
        }
    }
}
