//! Run terms on the strand lattice and compare with the oracle.
//!   strands-run <term | workload[:n] | @file> [--k K] [--lanes L] [--grid N | --fit] [--temp T] [--seed S] [--share N]
//! `--fit` sizes the grid as the player does. `--clean N --cool M` runs on until only the answer is
//! left, then cools for M clocks and reports how fast and how far the answer contracted; each
//! `--vary 'key=value ...'` cools again from the same state with those settings.
//! A term may hold superpositions (`&1{a,b}`, `fib:&1{2,3}`); its answer is then checked universe by
//! universe and shown collapsed (rust-ca-lattice sup.rs).

use rust_ca_lattice::sup::{self, STerm};
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
    let mut clean = 0f64;
    let mut mix = 0f64;
    let mut fit = false;
    let mut cool = 0f64;
    let mut vary = vec![];
    let mut sets = vec![];
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
            "--clean" => clean = it.next().unwrap().parse().unwrap(),
            "--pulse" => p.pulse = true,
            "--phop" => p.p_hop = it.next().unwrap().parse().unwrap(),
            "--margolus" => p.margolus = true,
            "--chip" => p = rust_ic_strands::lattice::Params { w: p.w, h: p.h, seed: p.seed, ..rust_ic_strands::lattice::chip() },
            "--latest" => p = rust_ic_strands::lattice::Params { w: p.w, h: p.h, seed: p.seed, ..rust_ic_strands::lattice::latest() },
            "--block-moves" => p.block_moves = true,
            "--block-side" => p.block_side = it.next().unwrap().parse().unwrap(),
            "--gc" => p.gc = true,
            "--link" => p.link_crowd = it.next().unwrap().parse().unwrap(),
            "--idle-crowd" => p.idle_crowd = it.next().unwrap().parse().unwrap(),
            "--board" => p.board_crowd = it.next().unwrap().parse().unwrap(),
            "--pairs" => p.pairs = it.next().unwrap().parse().unwrap(),
            "--block" => p.block = true,
            "--lazy" => p.lazy = true,
            "--idle" => p.idle_tension = it.next().unwrap().parse().unwrap(),
            "--active" => p.active = it.next().unwrap().parse().unwrap(),
            "--swap" => p.swap = it.next().unwrap().parse().unwrap(),
            "--agents" => p.agent_turns = it.next().unwrap().parse().unwrap(),
            "--repel" => p.repel = it.next().unwrap().parse().unwrap(),
            "--field" => p.set("field", &it.next().unwrap()).unwrap_or_else(|e| panic!("{e}")),
            "--calls" => p.calls = true,
            "--fork" => p.fork = true,
            "--share" => p.share = it.next().unwrap().parse().unwrap(),
            "--demand" => { p.calls = true; p.set("field", rust_ic_strands::lattice::DEMAND).unwrap(); }
            "--mix" => mix = it.next().unwrap().parse().unwrap(),
            "--fit" => fit = true,
            "--cool" => cool = it.next().unwrap().parse().unwrap(),
            "--vary" => vary.push(it.next().unwrap().clone()),
            _ if a.contains('=') => sets.push(a.clone()),
            _ => src = Some(a.clone()),
        }
    }
    // key=value settings (`Params::set`) go last, so they change whatever the flags chose.
    for kv in &sets { let (k, v) = kv.split_once('=').unwrap(); p.set(k, v).unwrap_or_else(|e| panic!("{e}")); }
    let src = src.expect("term");
    let src = match src.strip_prefix('@') { Some(path) => std::fs::read_to_string(path).expect("term file").trim().to_string(), None => src };
    let t: STerm = match src.split_once(':') {
        Some((name, n)) if n.starts_with('&') && term::workload_sup(name, n).is_some() => term::workload_sup(name, n).unwrap().expect("bad superposition"),
        Some((name, n)) if term::workload(name, 0).is_some() => (&term::workload(name, n.parse().unwrap()).unwrap()).into(),
        _ => term::workload(&src, 0).map(|t| (&t).into()).unwrap_or_else(|| term::parse_sup(&src).expect("bad term")),
    };
    let labels = t.labels();
    let want = sup::oracle_answers(&t, 100_000_000).map(|u| sup::show(&sup::collapse(&u, &labels)));
    let (net, out) = rust_ic_strands::share::net_sup(&t, p.share);
    let agents = net.live_count();
    let mut l = if fit {
        // As the player sizes it: side ceil(√agents · 7, or 8 in 2D) + 16, growing ×1.4 until the drawing fits.
        let mut side = ((net.agents.len() as f64).sqrt() * if p.depth > 1 { 7.0 } else { 8.0 }).ceil() as u32 + 16;
        let mut net = Some(net);
        loop {
            let (net, out) = net.take().map_or_else(|| rust_ic_strands::share::net_sup(&t, p.share), |n| (n, out));
            match Lattice::load(Params { w: side, h: side, ..p }, net, out) {
                Ok(l) => break l,
                Err(e) if e.starts_with("the drawing") => side = (side as f64 * 1.4).ceil() as u32,
                Err(e) => panic!("{e}"),
            }
        }
    } else { Lattice::load(p, net, out).expect("load") };
    println!("loaded {agents} agents on {}x{}x{}", l.p.w, l.p.h, l.p.depth);
    l.check_every = check;
    if let Ok(path) = std::env::var("VECTORS") {
        let mut w = std::io::BufWriter::new(std::fs::File::create(path).expect("vectors file"));
        // Header: the lattice's size.
        std::io::Write::write_all(&mut w, &[&b"H"[..], &l.p.w.to_le_bytes(), &l.p.h.to_le_bytes(), &l.p.depth.to_le_bytes()].concat()).expect("write vectors");
        l.vectors = Some(Box::new(w));
        l.vector_sample = std::env::var("VECTOR_SAMPLE").ok().map_or(100, |v| v.parse().unwrap());
    }
    if let Ok(path) = std::env::var("DUMPS") {
        let mut w = std::io::BufWriter::new(std::fs::File::create(path).expect("dumps file"));
        let seed = (l.p.seed as u32).wrapping_mul(0x9E37_79B9);
        std::io::Write::write_all(&mut w, &[&b"H"[..], &l.p.w.to_le_bytes(), &l.p.h.to_le_bytes(), &l.p.depth.to_le_bytes(), &seed.to_le_bytes()].concat()).expect("write dumps");
        l.dumps = Some(Box::new(w));
        l.dump_state();
    }
    let trace = std::env::var("TRACE").ok();
    if trace.is_some() { l.trace_start(); }
    let t0 = std::time::Instant::now();
    let done = if profile {
        // Time-averaged count of wanted consumers in each waiting state, sampled four times a
        // clock, plus the share of clocks where some consumer had its partner in reach.
        let (mut acc, mut lens, mut ready, mut fin, mut crowd) = ([0f64; 5], [0f64; 2], 0f64, false, [0usize; 8]);
        let mut boards = [0u64; 8];
        while l.stats.proposals < budget && !fin {
            let c0 = l.stats.clocks;
            fin = l.run((l.stats.proposals + (l.live_sites() / 4).max(1) as u64).min(budget));
            let dc = l.stats.clocks - c0;
            let (n, len) = l.wait_profile();
            for i in 0..5 { acc[i] += n[i] as f64 * dc; }
            for i in 0..2 { lens[i] += len[i] as f64 * dc; }
            if n[0] > 0 { ready += dc; }
            let b = l.board_stats();
            for i in 0..8 { boards[i] += b[i]; }
            let c = l.crowding();
            for i in 0..8 { crowd[i] += c[i]; }
        }
        let c = l.stats.clocks.max(1e-9);
        println!("wanted consumers on average: in reach {:.2}  walking to partner {:.2} (length {:.1})  carrying demand {:.2} (length {:.1})  waiting on a computation {:.2}  erasers parked {:.1}",
            acc[0] / c, acc[1] / c, lens[0] / acc[1].max(1e-9), acc[2] / c, lens[1] / acc[2].max(1e-9), acc[3] / c, acc[4] / c);
        println!("clocks per rewrite {:.1}; some pair in reach during {:.0}% of clocks", c / l.stats.fires.max(1) as f64, 100.0 * ready / c);
        println!("crowding: {:.0}% of the sites walkers head into are full, against {:.0}% of all sites holding agents; walker steps into full sites that went through as exchanges: {} of {}",
            100.0 * crowd[0] as f64 / crowd[1].max(1) as f64, 100.0 * crowd[2] as f64 / crowd[3].max(1) as f64, l.stats.walk_swap, l.stats.walk_fail[0]);
        let pairs = boards[..6].iter().sum::<u64>().max(1) as f64;
        println!("switchboards: {:.1} ends in use per live site; pairings: straight through {:.0}%, straight to another lane {:.0}%, corner {:.0}%, U-turn {:.0}%, port to strand {:.0}%, port to port {:.0}%",
            boards[7] as f64 / boards[6].max(1) as f64, 100.0 * boards[0] as f64 / pairs, 100.0 * boards[1] as f64 / pairs, 100.0 * boards[2] as f64 / pairs,
            100.0 * boards[3] as f64 / pairs, 100.0 * boards[4] as f64 / pairs, 100.0 * boards[5] as f64 / pairs);
        let h = l.pairs_hist.borrow();
        let tot = h.iter().sum::<u64>().max(1) as f64;
        println!("pairings per live site: {}", h.iter().enumerate().filter(|(_, &n)| n > 0).map(|(i, &n)| format!("{i}: {:.3}%", 100.0 * n as f64 / tot)).collect::<Vec<_>>().join(", "));
        let full = crowd[0].max(1) as f64;
        println!("full sites walkers head into hold: two idle agents {:.0}%, one idle {:.0}%, no idle {:.0}%; the walker's own partner {:.0}%",
            100.0 * crowd[4] as f64 / full, 100.0 * crowd[5] as f64 / full, 100.0 * crowd[6] as f64 / full, 100.0 * crowd[7] as f64 / full);
        fin
    } else if mix > 0.0 {
        // How mixed the root's segments are (readback.rs `mixing`), sampled every `mix` clocks.
        let (mut fin, mut sum, mut n, mut right) = (false, [0f64; 4], 0, 0f64);
        while l.stats.proposals < budget && !fin {
            let c0 = l.stats.clocks;
            while l.stats.clocks - c0 < mix && !fin { fin = l.run((l.stats.proposals + l.live_sites() as u64).min(budget)); }
            for (a, m) in sum.iter_mut().zip(rust_ic_strands::readback::mixing(&l, &[1, 2, 3, u32::MAX])) { *a += m; }
            let (r, of) = l.labels_right();
            right += r as f64 / of.max(1) as f64;
            n += 1;
        }
        println!("mixing at segment depth 1, 2, 3, all: {}; tree labels right {:.0}%", sum.iter().map(|a| format!("{:.3}", a / n.max(1) as f64)).collect::<Vec<_>>().join(" "),
            100.0 * right / n.max(1) as f64);
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
    if let Some(path) = trace {
        // One line per rewrite: clock, rule, site, when its pair first existed and the rewrite that
        // made it (-1: from the start or a collection), when its consumer was first wanted, and
        // how often it found no room.
        let tr = &l.trace;
        let mut out = String::from("clock,rule,site,active,cause,wanted,blocked,strands,sites\n");
        for &(c, ri, site, cs, ps) in &tr.fires {
            let (a, cause) = tr.active.get(&(cs, ps)).copied().unwrap_or((f64::NAN, -2));
            let w = tr.wanted.get(&cs).copied().unwrap_or(f64::NAN);
            let r = rust_ca_lattice::rules::rule(ri);
            let (len, d) = tr.apart.get(&(cs, ps)).map(|&(l, d)| (l as i64, d as i64)).unwrap_or((-1, -1));
            out += &format!("{c},{}·{},{site},{a},{cause},{w},{},{len},{d}\n", r.consumer.name(), r.producer.name(), tr.blocked.get(&cs).copied().unwrap_or(0));
        }
        std::fs::write(&path, out).expect("write trace");
    }
    if done && clean > 0.0 {
        // Keep running so erasers collect what the answer no longer needs.
        let (c0, mut marks) = (l.stats.clocks, vec![]);
        let mut left = l.garbage(&mut marks);
        println!("garbage when the answer is in: {left} agents");
        while left > 0 && l.stats.clocks - c0 < clean {
            let next = l.stats.proposals + l.live_sites() as u64;
            l.run_on(next);
            left = l.garbage(&mut marks);
        }
        println!("after {:.0} more clocks: {left} agents of garbage left", l.stats.clocks - c0);
        if cool > 0.0 && left == 0 { contract(&mut l, cool, &vary); }
    }
    let ans = l.read_answer().map(|a| sup::show(&if labels.is_empty() { a } else { sup::collapse(&a.universes(&labels), &labels) }));
    let proj = l.check_projection();
    let s = &l.stats;
    println!("{} — answer {} (want {}) projection {:?}", if done { "DONE" } else { "UNFINISHED" },
        ans.as_deref().unwrap_or("-"), want.as_deref().unwrap_or("?"), proj.err());
    println!("clocks {:.0}  walker steps ok {} / no seat {} / no lane {} / energy {}  demand by pulse {}  collected {} ({} dead computations)  refused for a full switchboard {}  turns dropped for stale reads {}", s.clocks, s.walk_ok, s.walk_fail[0], s.walk_fail[1], s.walk_fail[2], s.pulses, s.collected, s.dead, s.capped, s.stale);
    println!("proposals {}  fires {} (blocked {})  swaps {}  hops {}  folds {}  flips {}  strands {} (peak {})  peak live sites {}  fullest site {}  {:.2}s",
        s.proposals, s.fires, s.blocked_fires, s.swaps, s.hops, s.folds, s.flips, s.strands, s.peak_strands, s.peak_live, s.peak_site, dt);
    if l.fields.on {
        println!("fields: {} bits a site; on average {:.0} sites a clock hold a field, against {} holding anything at the end", l.fields.bits(),
            l.fields.support as f64 / l.stats.clocks.max(1.0), l.live().len());
    }
    let mut left = std::collections::BTreeMap::new();
    for &site in l.live() {
        for k in 0..l.ks {
            let t = l.tag(site, k);
            if t != 0 { *left.entry(rust_ic_strands::lattice::tag_of(t).name()).or_insert(0) += 1; }
        }
    }
    println!("left on the lattice: {}", left.iter().map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", "));
    if std::env::var("PIECES").is_ok() {
        // Connected pieces of the abstract net that remain, with their agents and readers.
        let n = l.shadow.agents.len();
        let mut seen = vec![false; n];
        let mut pieces = std::collections::BTreeMap::new();
        for start in 0..n {
            if seen[start] || l.shadow.agents[start].is_none() { continue; }
            let (mut stack, mut tags) = (vec![start], std::collections::BTreeMap::new());
            seen[start] = true;
            while let Some(i) = stack.pop() {
                let a = l.shadow.agents[i].as_ref().unwrap();
                *tags.entry(a.tag.name()).or_insert(0) += 1;
                for p in a.ports.iter().flatten() {
                    if !seen[p.0 as usize] { seen[p.0 as usize] = true; stack.push(p.0 as usize); }
                }
            }
            let key = tags.iter().map(|(t, n)| format!("{t} {n}")).collect::<Vec<_>>().join(", ");
            *pieces.entry(key).or_insert(0) += 1;
        }
        for (k, c) in pieces { println!("  {c} × [{k}]"); }
    }
    let blocked: Vec<String> = s.blocked_rule.iter().enumerate().filter(|(_, &n)| n > 0)
        .map(|(i, n)| format!("{}·{} {n}", rust_ca_lattice::rules::rule(i).consumer.name(), rust_ca_lattice::rules::rule(i).producer.name())).collect();
    println!("blocked by lanes {}; by rule: {}", s.blocked_lanes, blocked.join(", "));
}

/// Cool for `clocks` clocks from the state at hand, once with each set of `key=value` settings
/// (`vary`; none: as configured), and report how the answer contracted: when the player would call
/// it done (at the coldest, and the wire no shorter for 300 clocks; also for 100 and 200, and when
/// nothing has moved for 16 clocks), when the wire first got within 5% and 1% of its length then and
/// as short as the first setting got it, and where it ended. `CURVE=n` prints the strands every n
/// clocks; a `row` line per setting has the numbers tab-separated.
fn contract(l: &mut Lattice, clocks: f64, vary: &[String]) {
    assert!(l.p.margolus, "contraction is measured in chip clocks");
    let (held, stats, p, temp) = (l.held_sites(), l.stats.clone(), l.p, l.p.temp);
    let want = l.readback().map(|t| rust_ca_lattice::oracle::show(&t));
    let curve = std::env::var("CURVE").ok().map(|v| v.parse::<f64>().expect("CURVE=clocks"));
    let none = [String::new()];
    let mut reference = None;
    for v in if vary.is_empty() { &none[..] } else { vary } {
        l.put_sites(&held);
        l.stats = stats.clone();
        l.cooling = None;
        l.p = p;
        for kv in v.split_whitespace() { let (k, x) = kv.split_once('=').expect("key=value"); l.p.set(k, x).unwrap_or_else(|e| panic!("{e}")); }
        l.p.temp = temp;
        l.e = rust_ic_strands::lattice::Energy::new(&l.p);
        let (c0, s0, live0, t0) = (l.stats.clocks, l.stats.strands, l.live_sites(), std::time::Instant::now());
        l.cool();
        let moves = |l: &Lattice| l.stats.hops + l.stats.swaps + l.stats.folds + l.stats.flips;
        let mut seen = vec![(l.stats.strands, l.live_sites())];
        // Done by the player's rule: (clock, strands, sites, seconds); by 100 and 200 clocks, and by
        // 16 clocks of nothing moving: (clock, strands).
        let (mut shortest, mut done, mut windows, mut still, mut quiet) = ((u64::MAX, 0f64), None, [None; 2], 0, None);
        while l.stats.clocks - c0 < clocks {
            let m0 = moves(l);
            l.run_on(l.stats.proposals + 1);
            seen.push((l.stats.strands, l.live_sites()));
            let c = l.stats.clocks - c0;
            if curve.is_some_and(|k| c % k == 0.0) { println!("curve [{v}] {c} {} {} {:.3}", l.stats.strands, l.live_sites(), l.p.temp); }
            if l.p.temp > l.p.cool_min + 1e-9 { continue; }
            if l.stats.strands < shortest.0 { shortest = (l.stats.strands, c); }
            else if done.is_none() && c - shortest.1 >= 300.0 { done = Some((c, l.stats.strands, l.live_sites(), t0.elapsed().as_secs_f64())); }
            for (x, w) in windows.iter_mut().zip([100.0, 200.0]) { if x.is_none() && c - shortest.1 >= w { *x = Some((c, l.stats.strands)); } }
            still = if moves(l) == m0 { still + 1 } else { 0 };
            if quiet.is_none() && still >= 16 { quiet = Some((c, l.stats.strands)); }
        }
        let secs = t0.elapsed().as_secs_f64();
        if std::env::var("WIRES").is_ok() { wires(l); }
        let (end, live) = *seen.last().unwrap();
        let (c, n, sites, dsecs) = done.unwrap_or((-1.0, end, live, secs));
        let first = |x: f64| seen.iter().position(|&(m, _)| m as f64 <= x).map_or(-1, |c| c as i64);
        let reference = *reference.get_or_insert(n) as f64;
        let right = l.readback().map(|t| rust_ca_lattice::oracle::show(&t)) == want;
        let at = |x: Option<(f64, u64)>| x.map_or("-1\t0".to_string(), |(c, n)| format!("{c:.0}\t{n}"));
        println!("contract [{v}]: from {s0} strands {live0} sites; done at clock {c:.0} with {n} strands {sites} sites ({dsecs:.3}s); within 5% at {}, 1% at {}, \
            as short as the first setting at {}; done by 100, 200 clocks, 16 still: {}; after {clocks:.0} clocks {end} strands {live} sites{}",
            first(n as f64 * 1.05), first(n as f64 * 1.01), first(reference), [windows[0], windows[1], quiet].map(at).join(" ").replace('\t', "/"),
            if right { "" } else { "; ANSWER CHANGED" });
        println!("row\t{v}\t{}\t{s0}\t{live0}\t{c:.0}\t{n}\t{sites}\t{}\t{}\t{}\t{end}\t{live}\t{dsecs:.4}\t{secs:.4}\t{right}\t{}", p.seed,
            first(n as f64 * 1.05), first(n as f64 * 1.01), first(reference), [windows[0], windows[1], quiet].map(at).join("\t"));
    }
}

/// Every agent left and its wires: where each leads and through which sites.
fn wires(l: &Lattice) {
    let xyz = |s: u32| (s % l.p.w, (s / l.p.w) % l.p.h, s / (l.p.w * l.p.h));
    let ports = 3 * l.ks;
    for &s in l.live() {
        for k in 0..l.ks {
            let t = l.tag(s, k);
            if t == 0 { continue; }
            let tg = rust_ic_strands::lattice::tag_of(t);
            let mut line = format!("  {} {:?} pairs {}:", tg.name(), xyz(s), l.pairs(s));
            for q in 0..tg.arity() {
                let (mut x, mut m, mut path) = (s, l.mate_of(s, (k * 3 + q) as u8), vec![]);
                while m != rust_ic_strands::lattice::NONE && m as usize >= ports {
                    let (f, i) = ((m as usize - ports) / l.p.lanes, (m as usize - ports) % l.p.lanes);
                    x = l.nb(x, f);
                    path.push(xyz(x));
                    m = l.mate_of(x, (ports + (f ^ 1) * l.p.lanes + i) as u8);
                }
                let (k2, q2) = (m as usize / 3, m as usize % 3);
                line += &format!(" [{q}] -> {}.{q2} via {} {:?}", rust_ic_strands::lattice::tag_of(l.tag(x, k2)).name(), path.len(), path);
            }
            println!("{line}");
        }
    }
}
