//! Run a two-way program on the strand lattice (twoway.rs, `Params::two`) beside the same work in tree
//! calculus on the plain lattice:
//!   strands-two add A B [key=value …] [--seed S] [--profile]
//! `add A B` runs twoway.rs `add` on (A, B) and back, and disp's `add A B` (player/programs.js) plain.
//! `--profile`: for the two-way run, each rule's rewrites, how long its pairs waited between becoming
//! ready and firing, and how far apart they were then.

use rust_ca_lattice::oracle;
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::twoway::{self, nat, Prog, Val};

/// disp's `name args` as a term, from player/programs.js.
fn call(name: &str, nums: &[usize]) -> String {
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).expect("programs.js");
    let at = js.find(&format!("\"name\": \"{name}\"")).expect("no such program");
    let entry = &js[at..at + js[at..].find('}').unwrap()];
    let i = entry.find("\"tree\": \"").unwrap() + 9;
    let tree = &entry[i..i + entry[i..].find('"').unwrap()];
    std::iter::once(tree.to_string()).chain(nums.iter().map(|&n| "20".repeat(n) + "0")).collect::<Vec<_>>().join(" ")
}

fn side(n: usize) -> u32 { ((n as f64).sqrt() * 7.0).ceil() as u32 + 16 }

/// Runs p on x: clocks, rewrites, agents at the start and at their peak, and whether it gave `want`.
fn two(p: &Prog, x: &Val, want: &Val, base: &Params, profile: bool) -> String {
    let (net, out) = twoway::net(p, x);
    let n = net.live_count();
    let w = side(n);
    let mut l = Lattice::load(Params { w, h: w, ..*base }, net, out).unwrap_or_else(|e| panic!("{e}"));
    if profile { l.trace_start(); }
    let t = std::time::Instant::now();
    l.run(400_000_000);
    if profile {
        let mut by: std::collections::BTreeMap<String, (u64, f64, f64)> = Default::default();
        for &(clock, ri, _, c, p) in &l.trace.fires {
            let r = rust_ca_lattice::rules::rule(ri);
            let e = by.entry(format!("{}·{}", r.consumer.name(), r.producer.name())).or_default();
            e.0 += 1;
            if let Some(&(made, _)) = l.trace.active.get(&(c, p)) { e.1 += clock - made; }
            if let Some(&(strands, _)) = l.trace.apart.get(&(c, p)) { e.2 += strands as f64; }
        }
        let mut rows: Vec<_> = by.into_iter().collect();
        rows.sort_by(|a, b| (b.1 .0 as f64 * b.1 .1).total_cmp(&(a.1 .0 as f64 * a.1 .1)));
        println!("  {:14} {:>6} {:>10} {:>10}", "rule", "fires", "mean wait", "mean apart");
        for (name, (n, wait, apart)) in rows.iter().take(14) {
            println!("  {name:14} {n:>6} {:>10.1} {:>10.1}", wait / *n as f64, apart / *n as f64);
        }
    }
    let ok = twoway::leftovers(&l.shadow).is_empty() && twoway::read(&l.shadow, out).is_some_and(|(_, y)| y == *want);
    format!("{:>8.0} clocks {:>6} rewrites, {:>5} agents loaded, {:>5} sites at most, {:.1}s{}", l.stats.clocks, l.stats.fires, n,
            l.stats.peak_live, t.elapsed().as_secs_f64(), if ok { "" } else { "  WRONG" })
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut base = Params { two: true, lazy: false, ..latest() };
    let mut plain = latest();
    let mut words = vec![];
    let mut profile = false;
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--profile" => profile = true,
            "--seed" => { let s = it.next().unwrap().parse().unwrap(); base.seed = s; plain.seed = s; }
            kv if kv.contains('=') => { let (k, v) = kv.split_once('=').unwrap(); base.set(k, v).unwrap(); plain.set(k, v).unwrap(); }
            w => words.push(w.to_string()),
        }
    }
    match words.iter().map(String::as_str).collect::<Vec<_>>()[..] {
        ["add", a, b] => {
            let (a, b): (usize, usize) = (a.parse().unwrap(), b.parse().unwrap());
            let (x, y) = (Val::f(nat(a), nat(b)), Val::f(nat(a), nat(a + b)));
            println!("two-way add {a} {b}:  {}", two(&twoway::add(), &x, &y, &base, profile));
            println!("  and back (sub):  {}", two(&twoway::add().dagger().unwrap(), &y, &x, &base, false));
            let src = call("add", &[a, b]);
            let t = term::parse_sup(&src).unwrap().plain().unwrap();
            let want = oracle::nf(t.clone(), &mut oracle::Fuel(10_000_000)).ok();
            let (net, out) = rust_ic_strands::share::net_for(&(&t).into(), &plain).unwrap();
            let n = net.live_count();
            let w = side(n);
            let mut l = Lattice::load(Params { w, h: w, ..plain }, net, out).unwrap();
            let t0 = std::time::Instant::now();
            let done = l.run(400_000_000);
            let ok = done && l.readback() == want;
            println!("tree calculus add {a} {b}: {:>8.0} clocks {:>6} rewrites, {:>5} agents loaded, {:>5} sites at most, {:.1}s{}", l.stats.clocks,
                     l.stats.fires, n, l.stats.peak_live, t0.elapsed().as_secs_f64(), if ok { "" } else { "  WRONG" });
        }
        _ => eprintln!("usage: strands-two add A B [key=value …] [--seed S]"),
    }
}
