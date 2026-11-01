//! Run a disp program call or a term on the substrate (lattice.rs `Params::phys`, substrate.rs):
//! forward to the answer, then, with `--back`, the drive turned round until the net is the loaded
//! one again.
//!   strands-phys 'add 1 1' [key=value …] [--grid N] [--back D] [--budget P] [--seed S]
//! A call names a program of player/programs.js with literal arguments (numbers, or lists of them);
//! anything else is read as a term. key=value sets a parameter (`Params::set`) over the current
//! design with the substrate on.

use tree_net::sup::STerm;
use tree_net::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};

/// A disp program call as the player builds it (rev.html `direct`), as a term; else None.
fn call(src: &str) -> Option<String> {
    let (name, args) = src.trim().split_once(char::is_whitespace)?;
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).ok()?;
    let at = js.find(&format!("\"name\": \"{name}\""))?;
    let entry = &js[at..at + js[at..].find('}')?];
    let field = |k: &str| entry.find(&format!("\"{k}\": ")).map(|i| { let v = &entry[i + k.len() + 4..]; v.trim_start_matches('"').split(['"', ',', '\n']).next().unwrap().to_string() });
    let tree = field("tree")?;
    let nums: Vec<u128> = args.split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()).collect();
    let nat = |n: u128| "20".repeat(n as usize) + "0";
    let mut out = vec![tree];
    if field("kind").as_deref() == Some("bits") {
        let k = match field("width") { Some(w) => w.parse::<u32>().ok()?.trailing_zeros(), None => { let m = *nums.iter().max()?; (0..).find(|&k: &u32| m < 1u128 << (1u32 << k)).unwrap() } };
        fn bits(n: u128, k: u32) -> String { if k == 0 { return if n & 1 == 1 { "10".into() } else { "0".into() }; } let w = 1u128 << (1u32 << (k - 1)); format!("2{}{}", bits(n % w, k - 1), bits(n / w, k - 1)) }
        out.extend(nums.iter().map(|&n| bits(n, k)));
    } else if args.contains('[') {
        out.push(args.trim().trim_matches(['[', ']']).split(',').filter(|s| !s.trim().is_empty()).rev()
            .fold("0".to_string(), |tail, x| format!("2{}{tail}", nat(x.trim().parse().unwrap()))));
    } else {
        out.extend(nums.iter().map(|&n| nat(n)));
    }
    Some(out.join(" "))
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut p = Params { phys: true, ..latest() };
    let (mut grid, mut back, mut budget, mut back_bond) = (0u32, None, 400_000_000u64, None);
    let mut src = None;
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--grid" => grid = it.next().unwrap().parse().unwrap(),
            "--back" => back = Some(it.next().unwrap().parse::<f64>().unwrap()),
            "--back-bond" => back_bond = Some(it.next().unwrap().parse::<f64>().unwrap()),
            "--budget" => budget = it.next().unwrap().parse().unwrap(),
            "--seed" => p.seed = it.next().unwrap().parse().unwrap(),
            kv if kv.contains('=') => { let (k, v) = kv.split_once('=').unwrap(); p.set(k, v).unwrap(); }
            s => src = Some(s.to_string()),
        }
    }
    let src = src.expect("a program call or a term");
    let text = call(&src).unwrap_or(src.clone());
    let t: STerm = term::parse_sup(&text).unwrap_or_else(|e| panic!("{e}"));
    let (net, out) = rust_ic_strands::load::net_for(&t, &p).unwrap();
    let n = net.live_count();
    let side = if grid > 0 { grid } else { ((n as f64).sqrt() * if p.depth > 1 { 7.0 } else { 8.0 }).ceil() as u32 + 16 };
    p.w = side;
    p.h = side;
    let start = net.agents.clone();
    let mut l = Lattice::load(p, net, out).unwrap_or_else(|e| panic!("{e}"));
    let t0 = std::time::Instant::now();
    let done = l.run(budget);
    let answer = l.readback().map(|a| tree_net::oracle::show(&a)).unwrap_or_default();
    let g = &l.sub.ledger;
    println!("{src}: {} agents loaded on {side}x{side}x{}, {}", n, l.p.depth, if done { "answered" } else { "NOT answered" });
    println!("  read {:?}, abstract net quiet: {}", l.read_answer().map(|a| tree_net::sup::show(&a).chars().take(80).collect::<String>()), l.shadow.active_pair().is_none());
    println!("  answer {}", answer.chars().take(80).collect::<String>());
    println!("  {:.0} clocks, {} rewrites ({} undone on the way), {} un-copies, {:.1}s", l.stats.clocks, l.stats.fires, l.stats.unfires, g.uncopied, t0.elapsed().as_secs_f64());
    println!("  records kept {}, erasers {}, agents now {}, heat {:.0} (drive {:.0}, erased {}, forgotten {})", l.records_kept(), l.erasers(), l.shadow.live_count(), g.heat(), g.drive, g.erased, g.forgot);
    {
        let mut by: std::collections::BTreeMap<String, (usize, usize)> = Default::default();
        for &s in l.live() { for k in 0..l.ks { let t = l.tags[s as usize * l.ks + k]; if t != 0 { let e = by.entry(rust_ic_strands::lattice::tag_of(t).name().to_string()).or_default(); e.0 += 1; e.1 += l.want[s as usize * l.ks + k] as usize; } } }
        println!("  agents by kind (wanted): {}", by.iter().map(|(k, (n, w))| format!("{k} {n} ({w})")).collect::<Vec<_>>().join(", "));
        let mut kinds: std::collections::BTreeMap<u16, usize> = Default::default();
        for r in l.sub.records.iter().flatten() { *kinds.entry(r.kind).or_default() += 1; }
        println!("  records by kind: {}", kinds.iter().map(|(k, n)| format!("{} {n}", if *k < 1000 { tree_net::rules::rule(*k as usize).consumer.name().to_string() + "·" + tree_net::rules::rule(*k as usize).producer.name() } else { format!("#{k}") })).collect::<Vec<_>>().join(", "));
    }
    if let Some(d) = back {
        if let Some(b) = back_bond { l.p.bond = b; }
        l.set_drive(d);
        let (c0, t1) = (l.stats.clocks, std::time::Instant::now());
        let stop = l.stats.proposals + budget;
        let mut at = false;
        while l.stats.proposals < stop {
            if l.same_net(&start) { at = true; break; }
            l.run_on(l.stats.proposals + 20_000);
        }
        println!("  back (drive {d}): {} after {:.0} clocks, {} undone in all ({} tries too far apart, {} without room), {} forward again, records left {}, {:.1}s", if at { "at the start" } else { "NOT at the start" },
                 l.stats.clocks - c0, l.stats.unfires, l.stats.undo_far, l.stats.undo_full, l.sub.ledger.forward, l.records_kept(), t1.elapsed().as_secs_f64());
        if !at {
            let rep = l.records_report();
            let ok: Vec<_> = rep.iter().filter(|r| r.2).collect();
            let spread = |v: &Vec<[i64; 3]>| v.iter().flat_map(|a| v.iter().map(move |b| (0..3).map(|i| (a[i] - b[i]).abs()).sum::<i64>())).max().unwrap_or(0);
            let mut hist = [0usize; 6];
            for r in &ok { hist[(spread(&r.3) as usize).min(5)] += 1; }
            println!("    {} records left, {} undoable as things are; those by how far apart what they need is (0..5+): {hist:?}", rep.len(), ok.len());
            let mut kinds: std::collections::BTreeMap<u16, usize> = Default::default();
            for r in &ok { *kinds.entry(r.1).or_default() += 1; }
            println!("    undoable by kind: {kinds:?}");
            for r in ok.iter().take(8) { println!("    {r:?}"); }
        }
    }
}
