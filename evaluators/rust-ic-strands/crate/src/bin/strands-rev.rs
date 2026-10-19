//! Reversible rewriting (src/rev.rs) on one term: run forward to the answer, report the history and the
//! residue it leaves, then undo every event (in random order among those that can be undone) and check
//! the net is back as loaded.
//!   strands-rev TERM|@FILE [--seed S] [--keep] [--no-collect] [--residue] [--rounds]
//! --keep: erasers keep values whole instead of taking them apart (as events); --no-collect: no un-copy
//! or dead-work collection; --residue: what the net holds besides the answer; --rounds: run by rounds,
//! every rewrite there is at once, and say how many rounds and how many events the widest had.
use tree_net::net::Net;
use tree_net::oracle::{self, Fuel, Lcg};
use tree_net::rules::Tag;
use tree_net::term;
use rust_ic_strands::rev::Rev;
use std::collections::BTreeMap;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let src = args.iter().find(|a| !a.starts_with("--")).expect("a term").clone();
    let seed: i64 = args.iter().position(|a| a == "--seed").map(|i| args[i + 1].parse().unwrap()).unwrap_or(1);
    let flag = |f: &str| args.iter().any(|a| a == f);
    let src = match src.strip_prefix('@') { Some(p) => std::fs::read_to_string(p).expect("term file").trim().to_string(), None => src };
    let t = match src.split_once(':') {
        Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
        _ => term::workload(&src, 0).unwrap_or_else(|| term::parse(&src).expect("bad term")),
    };
    let want = oracle::nf(t.clone(), &mut Fuel(100_000_000)).ok();
    let (net, _) = rust_ic_strands::load::net(&t);
    let loaded = Net { agents: net.agents.clone(), ..Net::new() };
    let at_load = loaded.live_count();
    let mut r = Rev::new(net, true);
    r.erase = !flag("--keep");
    r.collect = !flag("--no-collect");
    let t0 = std::time::Instant::now();
    let n = if flag("--rounds") {
        let (mut n, mut widest) = (0, 0);
        loop { let k = r.round(); if k == 0 { break; } n += k; widest = widest.max(k); }
        println!("rounds: {}, the widest {widest} events", r.rounds.len());
        n
    } else { r.run(50_000_000) };
    let ok = r.answer() == want && want.is_some();
    println!("forward: {n} rewrites, answer {} ({:.2}s)", if ok { "right" } else { "WRONG or none" }, t0.elapsed().as_secs_f64());
    // The history: events, what they consumed and made, seams; the residue: what the answer no longer reaches.
    let (mut gone, mut made, mut seams) = (0, 0, 0);
    let mut by_rule: BTreeMap<String, usize> = BTreeMap::new();
    for e in r.events.iter().flatten() {
        gone += e.gone.len();
        made += e.made.len();
        seams += e.made.iter().filter(|&&m| r.net.agents[m as usize].as_ref().is_some_and(|a| a.tag == Tag::Seam)).count();
        *by_rule.entry(e.kind.name()).or_default() += 1;
    }
    let live = r.net.live_count();
    let reached = r.reached();
    let answer_agents = reached.iter().filter(|&&x| x).count();
    let erasers = r.net.agents.iter().flatten().filter(|a| a.tag == Tag::Eps).count();
    println!("history: {} events, consuming {gone} agents and making {made} (seams {seams})", r.live_events());
    println!("net at the end: {live} agents (loaded {at_load}): the answer and root {answer_agents}, residue {} under {erasers} erasers, seams {}",
        live - answer_agents, r.net.agents.iter().flatten().filter(|a| a.tag == Tag::Seam).count());
    if flag("--residue") {
        let mut tags: BTreeMap<String, usize> = BTreeMap::new();
        for (i, a) in r.net.agents.iter().enumerate() { if let Some(a) = a { if !reached[i] { *tags.entry(format!("{}{}", a.tag.name(), if r.want[i] { "*" } else { "" })).or_default() += 1; } } }
        println!("residue by tag (* wanted): {tags:?}");
        for (i, a) in r.net.agents.iter().enumerate() {
            let Some(a) = a else { continue };
            if a.tag != Tag::Eps { continue; }
            let (far, s) = r.follow((i as u32, 0));
            println!("  eraser {i} holds {:?} via {} seams", far.map(|(b, q)| (r.net.agents[b as usize].as_ref().unwrap().tag.name(), q)), s.len());
        }
    }
    println!("events by rule: {}", by_rule.iter().map(|(k, v)| format!("{k} {v}")).collect::<Vec<_>>().join(", "));
    // Back.
    let mut rng = Lcg(seed);
    let t1 = std::time::Instant::now();
    let mut undone = 0;
    while r.undo_any(|n| ((rng.next() * n as f64) as usize).min(n - 1)).is_some() { undone += 1; }
    let back = (0..loaded.agents.len().max(r.net.agents.len())).all(|i| {
        let (x, y) = (loaded.agents.get(i).and_then(|x| x.as_ref()), r.net.agents.get(i).and_then(|y| y.as_ref()));
        match (x, y) { (None, None) => true, (Some(x), Some(y)) => x.tag == y.tag && x.label == y.label && x.ports == y.ports, _ => false }
    });
    println!("backward: {undone} events undone, {} left; the net {} as loaded ({:.2}s)", r.live_events(), if back { "is exactly" } else { "is NOT" }, t1.elapsed().as_secs_f64());
}
