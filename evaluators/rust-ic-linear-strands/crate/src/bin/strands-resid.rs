//! Run a disp program call or a term on the lattice keeping history (lattice.rs `Params::resid`,
//! resid.rs): forward to the answer, then a click on the root and back to the term as loaded.
//!   strands-resid 'add 2 3' [key=value …] [--grid N] [--forgetting] [--erase] [--budget P] [--seed S]
//! A call names a program of player/programs.js with literal arguments (numbers, or lists of them);
//! anything else is read as a term `f a₁ … aₙ`. With `--forgetting` what the applies drop is
//! forgotten (the term comes back with holes); with `--erase` the history is erased as it is made.

use tree_net::oracle;
use tree_net::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::resid::{self, Edit};

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
    let mut p = Params { resid: 1, ..latest() };
    let (mut grid, mut budget, mut src, mut forgetting) = (0u32, 400_000_000u64, None, false);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--grid" => grid = it.next().unwrap().parse().unwrap(),
            "--budget" => budget = it.next().unwrap().parse().unwrap(),
            "--seed" => p.seed = it.next().unwrap().parse().unwrap(),
            "--erase" => p.resid = 2,
            "--forgetting" => forgetting = true,
            kv if kv.contains('=') => { let (k, v) = kv.split_once('=').unwrap(); p.set(k, v).unwrap(); }
            s => src = Some(s.to_string()),
        }
    }
    let src = src.expect("a program call or a term");
    assert!(p.resid > 0, "strands-resid keeps the history (resid=1) or erases it (--erase); for neither, strands-phys phys=0");
    let text = call(&src).unwrap_or(src.clone());
    let t = term::parse_sup(&text).unwrap_or_else(|e| panic!("{e}")).plain().expect("no superpositions");
    let want = oracle::nf(t.clone(), &mut oracle::Fuel(100_000_000)).ok();
    let (net, out, parts) = resid::net(&t, p.resid == 1);
    let n = net.live_count();
    let side = if grid > 0 { grid } else { ((n as f64).sqrt() * if p.depth > 1 { 7.0 } else { 8.0 }).ceil() as u32 + 16 };
    p.w = side;
    p.h = side;
    let mut l = Lattice::load(p, net, out).unwrap_or_else(|e| panic!("{e}"));
    l.resid = parts;
    if forgetting { l.edit(Edit::Forget(true)); }
    let t0 = std::time::Instant::now();
    let done = l.run(budget);
    let answer = l.read_answer().and_then(|a| a.plain());
    println!("{src}: {n} agents loaded on {side}x{side}x{}, {}", l.p.depth, if done { "answered" } else { "NOT answered" });
    println!("  answer {} ({})", answer.as_ref().map_or("-".into(), oracle::show).chars().take(80).collect::<String>(),
             if answer == want { "the oracle's" } else { "NOT the oracle's" });
    println!("  forward: {:.0} clocks, {} rewrites, {} moves, {:.1}s; history {} agents", l.stats.clocks, l.stats.fires, l.stats.collected, t0.elapsed().as_secs_f64(), l.resid_history());
    println!("  agents now {}, rewrites blocked for room {}, live sites {}", l.shadow.live_count(), l.stats.blocked_fires, l.live().len());
    let blocked = |l: &Lattice, before: &[u64]| {
        if std::env::var("RESID_BLOCKED").is_err() { return; }
        let mut by: Vec<(u64, String)> = l.stats.blocked_rule.iter().enumerate().map(|(i, &n)| (n - before.get(i).copied().unwrap_or(0), i)).filter(|x| x.0 > 0)
            .map(|(n, i)| { let r = tree_net::rules::rule(i); (n, format!("{}·{} ({} fresh)", r.consumer.name(), r.producer.name(), r.fresh.len())) }).collect();
        by.sort_by(|a, b| b.0.cmp(&a.0));
        println!("  blocked by rule: {:?}", &by[..by.len().min(12)]);
    };
    blocked(&l, &[]);
    let before = l.stats.blocked_rule.clone();
    if p.resid != 1 { return; }
    let (c0, f0, m0, t1) = (l.stats.clocks, l.stats.fires, l.stats.collected, std::time::Instant::now());
    l.edit(Edit::Back);
    let back = l.run(l.stats.proposals + budget);
    let got = l.read_term();
    let shape = match &got {
        Some(t) if *t == l.resid.term => "as loaded".to_string(),
        Some(t) if t.within(&l.resid.term) => format!("as loaded but for {} holes", t.holes()),
        _ => "NOT as loaded".into(),
    };
    use tree_net::rules::Tag;
    let left = l.shadow.agents.iter().flatten().filter(|a| !matches!(a.tag, Tag::L | Tag::S | Tag::F | Tag::Out | Tag::Hole)).count();
    println!("  back: {}, {:.0} clocks, {} rewrites, {} moves, {:.1}s; the term {shape}, {left} other agents left",
             if back { "done" } else { "NOT done" }, l.stats.clocks - c0, l.stats.fires - f0, l.stats.collected - m0, t1.elapsed().as_secs_f64());
    blocked(&l, &before);
    if !back {
        // What is left, and which of it touches a value or another walker without going on.
        let mut by = std::collections::BTreeMap::new();
        for a in l.shadow.agents.iter().flatten() { *by.entry(a.tag.name()).or_insert(0) += 1; }
        println!("  left: {by:?}");
        let mut meet = std::collections::BTreeMap::new();
        for a in l.shadow.agents.iter().flatten() {
            if !a.tag.is_consumer() { continue; }
            if let Some((b, q)) = a.ports[0] { *meet.entry(format!("{}.0·{}.{q}", a.tag.name(), l.shadow.agents[b as usize].as_ref().unwrap().tag.name())).or_insert(0) += 1; }
        }
        println!("  consumers read: {meet:?}");
    }
}
