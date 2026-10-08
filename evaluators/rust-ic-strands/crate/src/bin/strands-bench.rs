//! Benchmark runs for `programs/bench.ts`: each line `name|term` (or `name|fib:2`) of a file runs to the answer on the
//! current design (`latest`) on a lattice sized as the player sizes it, one JSON line a seed:
//! side, agents at load, clocks, rewrites, peak strands and sites, wall time, the answer in ternary.
//!   strands-bench <file> [--seeds N | --seed S] [--clocks MAX] [--rules] [--ideal] [key=value ...]
//! `--rules` also counts the rewrites by consumer (Dn copies, Eps erases, …). `--ideal` runs the
//! idealised lazy machine instead (memo.rs: no lattice, every wanted rewrite at once): its work
//! (rewrites and collections) and depth (rounds), in no time.

use rust_ca_lattice::rules::rule;
use rust_ca_lattice::sup::STerm;
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};

/// Ternary preorder: 0 a leaf, 1 a stem, 2 a fork.
fn ternary(t: &STerm, out: &mut String) {
    let mut stack = vec![t];
    while let Some(t) = stack.pop() {
        match t {
            STerm::L => out.push('0'),
            STerm::S(x) => { out.push('1'); stack.push(x); }
            STerm::F(x, y) => { out.push('2'); stack.push(y); stack.push(x); }
            _ => { out.push('?'); }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (mut file, mut seeds, mut max_clocks, mut by_rule, mut sets, mut ideal) = (None, vec![1u64, 2], 5_000_000f64, false, vec![], false);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--seeds" => seeds = (1..=it.next().unwrap().parse().unwrap()).collect(),
            "--seed" => seeds = vec![it.next().unwrap().parse().unwrap()],
            "--clocks" => max_clocks = it.next().unwrap().parse().unwrap(),
            "--rules" => by_rule = true,
            "--ideal" => ideal = true,
            _ if a.contains('=') => sets.push(a.clone()),
            _ => file = Some(a.clone()),
        }
    }
    let text = std::fs::read_to_string(file.expect("a file of name|term lines")).expect("read the file");
    for line in text.lines().map(str::trim).filter(|l| !l.is_empty() && !l.starts_with('#')) {
        let (name, src) = line.split_once('|').expect("name|term");
        let t = match src.split_once(':') {
            Some((w, arg)) if term::workload_sup(w, arg).is_some() => term::workload_sup(w, arg).unwrap(),
            _ => term::parse_sup(src),
        }.unwrap_or_else(|e| panic!("{name}: {e}"));
        if ideal {
            // The idealised lazy machine (memo.rs): demand at once, every wanted rewrite at once.
            use rust_ca_lattice::rules::Tag;
            let (mut net, _) = rust_ic_strands::share::net_sup(&t, 0);
            let agents = net.live_count();
            let mut want: Vec<bool> = net.agents.iter().map(|a| matches!(a, Some(a) if matches!(a.tag, Tag::Nrm | Tag::Out | Tag::Eps))).collect();
            let r = rust_ic_strands::lattice::memo::ideal_depth(&mut net, &mut want, latest().fork, 10_000_000);
            let mut answer = String::new();
            let o = net.agents.iter().position(|a| matches!(a, Some(a) if a.tag == Tag::Out)).unwrap() as u32;
            if let Some(a) = net.readback(net.get(o).ports[0]) { ternary(&(&a).into(), &mut answer); }
            let (work, depth) = r.unwrap_or((0, 0));
            println!("{{\"name\":\"{name}\",\"agents\":{agents},\"done\":{},\"rewrites\":{work},\"depth\":{depth},\"rules\":{{}},\"answer\":\"{answer}\"}}", r.is_some() && !answer.is_empty());
            continue;
        }
        for &seed in &seeds {
            let mut p = Params { seed, ..latest() };
            for kv in &sets { let (k, v) = kv.split_once('=').unwrap(); p.set(k, v).unwrap_or_else(|e| panic!("{e}")); }
            // As the player: side ⌈√agents · 7⌉ + 16 (8 for one layer), ×1.4 until the drawing fits.
            let agents = rust_ic_strands::share::net_sup(&t, 0).0.live_count();
            let mut side = ((agents as f64).sqrt() * if p.depth > 1 { 7.0 } else { 8.0 }).ceil() as u32 + 16;
            let mut l = None;
            for _ in 0..6 {
                let (net, out) = rust_ic_strands::share::net_sup(&t, 0);
                match Lattice::load(Params { w: side, h: side, ..p.clone() }, net, out) {
                    Ok(x) => { l = Some(x); break; }
                    Err(_) => side = (side as f64 * 1.4).ceil() as u32,
                }
            }
            let Some(mut l) = l else { println!("{{\"name\":\"{name}\",\"seed\":{seed},\"error\":\"does not fit\"}}"); continue };
            if by_rule { l.trace_start(); }
            let t0 = std::time::Instant::now();
            while !l.answered() && l.stats.clocks < max_clocks {
                let next = l.stats.proposals + 20 * l.live().len().max(1) as u64;
                l.run(next);
            }
            let wall = t0.elapsed().as_secs_f64();
            let mut answer = String::new();
            if let Some(a) = l.read_answer() { ternary(&a, &mut answer); }
            let s = &l.stats;
            let mut counts = std::collections::BTreeMap::new();
            for f in &l.trace.fires { *counts.entry(rule(f.1).consumer.name()).or_insert(0u64) += 1; }
            let rules = counts.iter().map(|(k, n)| format!("\"{k}\":{n}")).collect::<Vec<_>>().join(",");
            println!("{{\"name\":\"{name}\",\"seed\":{seed},\"side\":{side},\"agents\":{agents},\"done\":{},\"clocks\":{:.0},\"rewrites\":{},\"collected\":{},\"peak_strands\":{},\"peak_sites\":{},\"wall\":{wall:.3},\"rules\":{{{rules}}},\"answer\":\"{answer}\"}}",
                !answer.is_empty(), s.clocks, s.fires, s.collected, s.peak_strands, s.peak_live);
        }
    }
}
