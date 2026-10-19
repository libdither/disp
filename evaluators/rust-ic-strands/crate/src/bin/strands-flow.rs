//! How matter flows: each workload under each variant of the settings, over seeds, from load to the
//! answer, its garbage gone, and the answer contracted (cooling, as the player does it):
//!   strands-flow [--seeds N] [--cool C] [--threads T] [--work W]… [--vary 'k=v k=v']…
//! A workload is a lambada workload (`fib:1`, `sort:1`), a disp call (`disp:add 3 4`, from
//! player/programs.js) or a two-way program (`two:add 5 6`, twoway.rs `parse_run`). Each variant
//! is `key=value` settings (lattice.rs `Params::set`) on top of the current design (`latest`).
//! Per workload and variant, means over the seeds: clocks to the answer, then to its garbage gone,
//! then to contracted (the player's rule: no shorter wire for 300 clocks at the coldest), the
//! strands and sites per agent left, rewrites blocked for room, and runs that jammed (no rewrite
//! for 30,000 clocks) or gave a wrong answer. `FIXED=1` cools for all C clocks instead; `CLEAN=1` traces the garbage, and `CENSUS=1` with it says
//! what the garbage is and how soon an erasure wave along its wires could clear it.

use rust_ca_lattice::oracle;
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::twoway;
use std::sync::atomic::{AtomicUsize, Ordering};

/// disp's `name args` as a term, from player/programs.js.
fn disp_call(name: &str, nums: &[usize]) -> String {
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).expect("programs.js");
    let at = js.find(&format!("\"name\": \"{name}\"")).unwrap_or_else(|| panic!("no program {name}"));
    let entry = &js[at..at + js[at..].find('}').unwrap()];
    let i = entry.find("\"tree\": \"").unwrap() + 9;
    let tree = &entry[i..i + entry[i..].find('"').unwrap()];
    std::iter::once(tree.to_string()).chain(nums.iter().map(|&n| "20".repeat(n) + "0")).collect::<Vec<_>>().join(" ")
}

#[derive(Clone, Default, Debug)]
struct Out { shape: [f64; 8], answer: f64, clean: f64, contract: f64, strands: f64, sites: f64, agents: f64, blocked: f64, jam: bool, wrong: bool, secs: f64 }

/// What the garbage is: agents by tag (wanted ones in brackets), what each eraser reads and how far, and
/// garbage agents facing each other principal to principal.
fn census(l: &Lattice, marks: &[u8]) {
    use rust_ic_strands::lattice::tag_of;
    use rust_ca_lattice::rules::Tag;
    let mut by: std::collections::BTreeMap<String, (usize, usize)> = Default::default();
    let mut eps: std::collections::BTreeMap<String, (usize, usize)> = Default::default();
    let mut facing: std::collections::BTreeMap<String, usize> = Default::default();
    for &s in l.live() {
        for k in 0..l.p.k {
            let i = s as usize * l.ks + k;
            if marks[i] == 0 { continue; }
            let t = tag_of(l.tags[i]);
            let e = by.entry(t.name().to_string()).or_default();
            e.0 += 1;
            if l.want[i] { e.1 += 1; }
            let (d, kd, q, len) = l.follow(s, (k * l.ar) as u8);
            let td = tag_of(l.tags[d as usize * l.ks + kd]);
            if t == Tag::Eps {
                let e = eps.entry(format!("{}.{q}", td.name())).or_default();
                e.0 += 1;
                e.1 += len;
            } else if q == 0 && (s, k) < (d, kd) {
                *facing.entry(format!("{}-{}{}", t.name(), td.name(), if l.want[i] || l.want[d as usize * l.ks + kd] { " wanted" } else { "" })).or_default() += 1;
            }
        }
    }
    eprintln!("    garbage (wanted): {}", by.iter().map(|(t, (n, w))| format!("{t} {n} ({w})")).collect::<Vec<_>>().join(", "));
    eprintln!("    erasers read (mean strands): {}", eps.iter().map(|(t, (n, len))| format!("{t} {n} ({:.1})", *len as f64 / *n as f64)).collect::<Vec<_>>().join(", "));
    eprintln!("    facing: {}", facing.iter().map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", "));
    // An erasure wave's clocks: one a strand along the wires and one an agent, from every eraser at once;
    // a duplicator or unpair passes it on to its input once both outputs are reached.
    let at: std::collections::HashMap<u32, (u32, usize)> = l.live().iter().flat_map(|&s| (0..l.p.k).map(move |k| (s, k)))
        .filter(|&(s, k)| marks[s as usize * l.ks + k] != 0).map(|(s, k)| (l.sids[s as usize * l.ks + k], (s, k))).collect();
    let mut heap = std::collections::BinaryHeap::new();
    let mut best: std::collections::HashMap<u32, (usize, usize)> = Default::default();
    let mut half: std::collections::HashMap<u32, usize> = Default::default();
    let reach = |s: u32, k: usize, p: usize, t: usize, d: usize, heap: &mut std::collections::BinaryHeap<std::cmp::Reverse<(usize, usize, u32, usize)>>| {
        let (s2, k2, q2, len) = l.follow(s, (k * l.ar + p) as u8);
        heap.push(std::cmp::Reverse((t + len + 1, d + 1, l.sids[s2 as usize * l.ks + k2], q2)));
    };
    for (&_id, &(s, k)) in &at { if tag_of(l.tags[s as usize * l.ks + k]) == Tag::Eps { reach(s, k, 0, 0, 0, &mut heap); } }
    while let Some(std::cmp::Reverse((t, d, id, q))) = heap.pop() {
        let Some(&(s, k)) = at.get(&id) else { continue };
        let tg = tag_of(l.tags[s as usize * l.ks + k]);
        let ins: Vec<usize> = match tg {
            _ if tg.is_producer() && q == 0 => (1..tg.arity()).collect(),
            Tag::A | Tag::T1 | Tag::Sel if q == 2 => vec![0, 1],
            Tag::Dn | Tag::Unp if q == 9 => vec![0],
            Tag::Dn | Tag::Unp if q > 0 && q != 9 => match half.insert(id, t) { Some(t0) if !best.contains_key(&id) => { let t = t.max(t0); best.insert(id, (t, d)); heap.push(std::cmp::Reverse((t, d, id, 9))); continue } _ => continue },
            _ => continue,
        };
        if q != 9 && best.contains_key(&id) { continue; }
        best.entry(id).or_insert((t, d));
        for p in ins { reach(s, k, p, t, d, &mut heap); }
    }
    let n = at.values().filter(|&&(s, k)| tag_of(l.tags[s as usize * l.ks + k]) != Tag::Eps).count();
    // What the agents it does not reach have on their principal port.
    let mut miss: std::collections::BTreeMap<String, usize> = Default::default();
    for (&id, &(s, k)) in &at {
        if best.contains_key(&id) || tag_of(l.tags[s as usize * l.ks + k]) == Tag::Eps { continue; }
        let (s2, k2, q2, _) = l.follow(s, (k * l.ar) as u8);
        let j = s2 as usize * l.ks + k2;
        *miss.entry(format!("{}.0-{}.{q2}{}", tag_of(l.tags[s as usize * l.ks + k]).name(), tag_of(l.tags[j]).name(), if marks[j] == 0 { " live" } else { "" })).or_default() += 1;
    }
    let mut miss: Vec<_> = miss.into_iter().collect();
    miss.sort_by_key(|m| std::cmp::Reverse(m.1));
    eprintln!("    unreached by what their principal reads: {}", miss.iter().take(12).map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", "));
    // Duplicators and unpairs the wave reached on one output: what reads the other.
    let mut halfway: std::collections::BTreeMap<String, usize> = Default::default();
    for (&id, _) in half.iter().filter(|(id, _)| !best.contains_key(id)) {
        let (s, k) = at[&id];
        for q in 1..3 {
            let (s2, k2, q2, len) = l.follow(s, (k * l.ar + q) as u8);
            let j = s2 as usize * l.ks + k2;
            let reached = best.contains_key(&l.sids[j]) || tag_of(l.tags[j]) == Tag::Eps;
            *halfway.entry(format!("{}{}.{q2}{}", if reached { "reached " } else { "" }, tag_of(l.tags[j]).name(), if len > 1 { " far" } else { "" })).or_default() += 1;
        }
    }
    eprintln!("    half-reached duplicators: {}, their outputs: {}", half.len() - half.keys().filter(|id| best.contains_key(id)).count(), halfway.iter().map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", "));
    // Walk up from each unreached agent (to what reads its output) to the first agent that is not a plain
    // value or a duplicator: what holds the garbage the wave does not reach.
    let mut tops: std::collections::BTreeMap<String, usize> = Default::default();
    for (&id, &(s0, k0)) in &at {
        if best.contains_key(&id) || tag_of(l.tags[s0 as usize * l.ks + k0]) == Tag::Eps { continue; }
        let (mut s, mut k, mut seen) = (s0, k0, std::collections::HashSet::new());
        let top = loop {
            if !seen.insert((s, k)) { break "cycle".to_string(); }
            let tg = tag_of(l.tags[s as usize * l.ks + k]);
            let out = if tg.is_producer() { 0 } else if tg == Tag::Dn { 1 } else { 2 };
            let (s2, k2, q2, _) = l.follow(s, (k * l.ar + out) as u8);
            let t2 = tag_of(l.tags[s2 as usize * l.ks + k2]);
            if !(t2.is_producer() && q2 > 0 || t2 == Tag::Dn && q2 == 0) {
                let j = s2 as usize * l.ks + k2;
                break format!("{}.{q2}{}{}", t2.name(), if l.want[j] { " wanted" } else { "" }, if best.contains_key(&l.sids[j]) { " reached" } else { "" });
            }
            (s, k) = (s2, k2);
        };
        *tops.entry(top).or_default() += 1;
    }
    eprintln!("    unreached garbage hangs from: {}", tops.iter().map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", "));
    eprintln!("    wave: reaches {} of {n}, the last at clock {} (depth {} agents)", best.len(), best.values().map(|b| b.0).max().unwrap_or(0), best.values().map(|b| b.1).max().unwrap_or(0));
}

/// The run: load, run to the answer, run its garbage away, cool it till it is done contracting.
fn run(work: &str, vary: &str, seed: u64, cool: f64) -> Out {
    let mut p = Params { seed, ..latest() };
    let two = work.strip_prefix("two:");
    if two.is_some() { p.set("two", "1").unwrap(); }
    for kv in vary.split_whitespace() { let (k, v) = kv.split_once('=').expect("key=value"); p.set(k, v).unwrap_or_else(|e| panic!("{e}")); }
    let (net, out, want_tc, want_two) = if let Some(src) = two {
        let (prog, x) = twoway::parse_run(src).unwrap_or_else(|e| panic!("{e}"));
        let (net, out) = twoway::net(&prog, &x);
        (net, out, None, prog.run(&x, &mut 1_000_000))
    } else {
        let src = match work.strip_prefix("disp:") {
            Some(call) => { let mut w = call.split_whitespace(); let name = w.next().unwrap(); disp_call(name, &w.map(|n| n.parse().unwrap()).collect::<Vec<_>>()) }
            None => work.to_string(),
        };
        let t = match src.split_once(':') {
            Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
            _ => term::parse_sup(&src).expect("bad term").plain().expect("no superpositions"),
        };
        let want = oracle::nf(t.clone(), &mut oracle::Fuel(100_000_000)).ok();
        let (net, out) = rust_ic_strands::share::net_for(&(&t).into(), &p).unwrap_or_else(|e| panic!("{e}"));
        (net, out, want, None)
    };
    let agents = net.live_count();
    let side = ((agents as f64).sqrt() * if p.depth > 1 { 7.0 } else { 8.0 }).ceil() as u32 + 16;
    let mut l = Lattice::load(Params { w: side, h: side, ..p }, net, out).unwrap_or_else(|e| panic!("{work}: {e}"));
    let t0 = std::time::Instant::now();
    let mut o = Out::default();
    // To the answer, or a jam.
    let (mut last, mut since) = (0, 0.0);
    let done = loop {
        if l.run(l.stats.proposals + 4 * l.live_sites().max(64) as u64) { break true; }
        if l.stats.fires != last { last = l.stats.fires; since = l.stats.clocks; }
        if l.stats.clocks - since > 30_000.0 || l.stats.clocks > 3_000_000.0 { break false; }
    };
    o.answer = l.stats.clocks;
    o.blocked = l.stats.blocked_fires as f64;
    o.jam = !done;
    if done {
        o.wrong = match two { Some(_) => twoway::read(&l.shadow, out).map(|(_, y)| y) != want_two, None => l.readback() != want_tc };
        // Its garbage away (tree calculus; a two-way run leaves none).
        let (c0, mut marks) = (l.stats.clocks, vec![]);
        let trace = std::env::var("CLEAN").is_ok();
        let mut next = c0;
        while l.p.gc && l.garbage(&mut marks) > 0 && l.stats.clocks - c0 < 200_000.0 {
            if trace && l.stats.clocks >= next {
                let erasers = l.shadow.agents.iter().flatten().filter(|a| a.tag == rust_ca_lattice::rules::Tag::Eps).count();
                eprintln!("{work} seed {seed} clean +{:.0}: {} agents of garbage, {erasers} erasers, {} sites", l.stats.clocks - c0, l.garbage(&mut marks), l.live_sites());
                if std::env::var("CENSUS").is_ok() { census(&l, &marks); }
                next += 250.0;
            }
            let n = l.stats.proposals + l.live_sites() as u64;
            l.run_on(n);
        }
        o.clean = l.stats.clocks - c0;
        // Contracted, by the player's rule.
        let c1 = l.stats.clocks;
        l.cool();
        let mut shortest = (l.stats.strands, c1);
        while l.stats.clocks - c1 < cool {
            let n = l.stats.proposals + l.live_sites() as u64;
            l.run_on(n);
            if l.stats.strands < shortest.0 { shortest = (l.stats.strands, l.stats.clocks); }
            else if l.stats.clocks - shortest.1 >= 300.0 && std::env::var("FIXED").is_err() { break; }
        }
        o.contract = shortest.1 - c1;
        let agents = l.shadow.agents.iter().flatten().count() as f64;
        o.agents = agents;
        o.strands = l.stats.strands as f64 / agents;
        o.sites = l.live_sites() as f64 / agents;
        // The shape: wires by strands (0, 1, 2, 3-4, 5+), sites by agents (2, 1, 0: wire only).
        for &x in l.live() {
            let n = (0..l.ks).filter(|&k| l.tags[x as usize * l.ks + k] != 0).count();
            o.shape[5 + (2 - n.min(2))] += 1.0;
            for k in 0..l.ks {
                let t = l.tags[x as usize * l.ks + k];
                if t == 0 { continue; }
                for q in 0..rust_ic_strands::lattice::tag_of(t).arity() {
                    let len = l.follow(x, (k * l.ar + q) as u8).3;
                    o.shape[match len { 0 => 0, 1 => 1, 2 => 2, 3 | 4 => 3, _ => 4 }] += 0.5;
                    if q == 0 && len >= 3 && std::env::var("PROBE").is_ok() {
                        eprintln!("{work} seed {seed}: {} with a principal wire of {len}: {}", rust_ic_strands::lattice::tag_of(t).name(), l.probe_step(x, k));
                    }
                }
            }
        }
    }
    o.secs = t0.elapsed().as_secs_f64();
    o
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (mut seeds, mut cool, mut threads) = (4u64, 4000.0, std::thread::available_parallelism().map_or(8, |n| n.get()));
    let (mut works, mut varies) = (vec![], vec![]);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--seeds" => seeds = it.next().unwrap().parse().unwrap(),
            "--cool" => cool = it.next().unwrap().parse().unwrap(),
            "--threads" => threads = it.next().unwrap().parse().unwrap(),
            "--work" => works.push(it.next().unwrap().clone()),
            "--vary" => varies.push(it.next().unwrap().clone()),
            w => panic!("unknown argument {w}"),
        }
    }
    if works.is_empty() { works = ["fib:1", "sort:1", "disp:add 3 4", "two:add 5 6", "two:mirror [swap; flip]"].map(String::from).to_vec(); }
    if varies.is_empty() { varies.push(String::new()); }
    let jobs: Vec<(usize, usize, u64)> = (0..works.len()).flat_map(|w| (0..varies.len()).flat_map(move |v| (1..=seeds).map(move |s| (w, v, s)))).collect();
    let next = AtomicUsize::new(0);
    let results = std::sync::Mutex::new(vec![None; jobs.len()]);
    std::thread::scope(|sc| {
        for _ in 0..threads {
            sc.spawn(|| loop {
                let i = next.fetch_add(1, Ordering::Relaxed);
                let Some(&(w, v, s)) = jobs.get(i) else { break };
                let o = run(&works[w], &varies[v], s, cool);
                results.lock().unwrap()[i] = Some(o);
            });
        }
    });
    let results = results.into_inner().unwrap();
    println!("{:28} {:24} {:>9} {:>7} {:>8} {:>7} {:>6} {:>7} {:>6} {:>5}", "workload", "variant", "answer", "clean", "contract", "str/ag", "sit/ag", "blocked", "jam", "secs");
    for (w, work) in works.iter().enumerate() {
        for (v, vary) in varies.iter().enumerate() {
            let rs: Vec<&Out> = jobs.iter().zip(&results).filter(|((w2, v2, _), _)| (*w2, *v2) == (w, v)).map(|(_, r)| r.as_ref().unwrap()).collect();
            let ok: Vec<&&Out> = rs.iter().filter(|r| !r.jam).collect();
            let mean = |f: &dyn Fn(&Out) -> f64| if ok.is_empty() { f64::NAN } else { ok.iter().map(|r| f(r)).sum::<f64>() / ok.len() as f64 };
            let wrong = rs.iter().filter(|r| r.wrong).count();
            println!("{:28} {:24} {:>9.0} {:>7.0} {:>8.0} {:>7.3} {:>6.3} {:>7.0} {:>6} {:>5.1}{}", work, if vary.is_empty() { "latest" } else { vary }, mean(&|r| r.answer),
                     mean(&|r| r.clean), mean(&|r| r.contract), mean(&|r| r.strands), mean(&|r| r.sites), mean(&|r| r.blocked),
                     format!("{}/{}", rs.len() - ok.len(), rs.len()), rs.iter().map(|r| r.secs).sum::<f64>() / rs.len() as f64,
                     if wrong > 0 { format!("  {wrong} WRONG") } else { String::new() });
            if std::env::var("SHAPE").is_ok() {
                let sh: Vec<String> = (0..8).map(|i| format!("{:.0}", mean(&|r| r.shape[i]))).collect();
                println!("    wires by strands 0/1/2/3-4/5+: {}   sites with 2/1/0 agents: {}", sh[..5].join("/"), sh[5..].join("/"));
            }
        }
    }
}
