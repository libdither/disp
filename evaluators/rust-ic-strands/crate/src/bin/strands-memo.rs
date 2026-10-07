//! Memo radius (README "Memo radius"): how many equal computations a run has close together, and
//! what merging them would save.
//!   strands-memo TERM [--every N] [--seed S] [--ideal] [--keys] [key=value...]
//!   strands-memo TERM --run [--seed S] [--budget PROPOSALS] memo=R [key=value...]
//! `--run` just runs to the answer (`memo=R` merging, Params::memo) and prints one line of counts.
//! TERM as `strands-run` takes it; the lattice is sized as the player sizes it and runs `latest`.
//! Every N clocks it reads every output the answer depends on as a term (duplicators transparent)
//! and prints, for each radius, how many computations (P, A, T1, Sel) and values (L, S, F) have an
//! equal twin no farther away that a merge could drop (`memo::plan`). `--keys` also counts twins
//! whose function and argument are equal once worked out (what a memo table keyed by apply(f, x)
//! would catch); `--ideal` estimates the work a merge saves, as the idealised lazy machine's work
//! left from the net as it is, with and without the merges (`memo::remaining`).

use rust_ca_lattice::oracle::{self, Fuel, Term};
use rust_ic_mesh::term;
use rust_ic_strands::lattice::memo::{self, Head, Terms};
use rust_ic_strands::lattice::{latest, Lattice, Params};

const RADII: [u32; 6] = [1, 2, 4, 8, 16, u32::MAX];

fn parse(src: &str) -> Term {
    match src.split_once(':') {
        Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
        _ => term::workload(src, 0).unwrap_or_else(|| term::parse(src).expect("bad term")),
    }
}

/// Load as the player does: side ceil(√agents · 7) + 16, growing ×1.4 until the drawing fits.
pub fn load(t: &Term, p: Params) -> Lattice {
    let agents = rust_ic_strands::share::net(t, p.share).0.live_count();
    let mut side = ((agents as f64).sqrt() * 7.0).ceil() as u32 + 16;
    loop {
        let (net, out) = rust_ic_strands::share::net(t, p.share);
        match Lattice::load(Params { w: side, h: side, ..p }, net, out) {
            Ok(l) => return l,
            Err(e) if e.starts_with("the drawing") => side = (side as f64 * 1.4).ceil() as u32,
            Err(e) => panic!("{e}"),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let (mut every, mut ideal, mut keys, mut src, mut run, mut budget) = (500f64, false, false, None, false, 4_000_000_000u64);
    let mut p = latest();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--every" => every = it.next().unwrap().parse().unwrap(),
            "--run" => run = true,
            "--budget" => budget = it.next().unwrap().parse().unwrap(),
            "--seed" => p.seed = it.next().unwrap().parse().unwrap(),
            "--ideal" => ideal = true,
            "--keys" => keys = true,
            _ if a.contains('=') => { let (k, v) = a.split_once('=').unwrap(); p.set(k, v).unwrap_or_else(|e| panic!("{e}")); }
            _ => src = Some(a.clone()),
        }
    }
    let t = parse(&src.expect("term"));
    let want = oracle::nf(t.clone(), &mut Fuel(100_000_000)).ok().map(|w| oracle::show(&w));
    let mut l = load(&t, p);
    println!("loaded {} agents on {}x{}x{}", l.shadow.live_count(), l.p.w, l.p.h, l.p.depth);
    if run {
        let done = l.run(budget);
        let ans = l.readback().map(|t| oracle::show(&t));
        let s = &l.stats;
        println!("memo={} seed={} {} clocks={:.0} rewrites={} collected={} peak_strands={} peak_sites={} merged={} unmerged={} projection={:?}",
            l.p.memo, l.p.seed, if !done { "UNFINISHED" } else if ans == want { "right" } else { "WRONG" }, s.clocks, s.fires, s.collected,
            s.peak_strands, s.peak_live, s.merged, s.unmerged, l.check_projection().err());
        return;
    }
    let mut terms = Terms::default();
    let (mut done, mut next) = (false, every);
    let r = |r: u32| if r == u32::MAX { "∞".to_string() } else { r.to_string() };
    println!("clock | comps (in groups) values | droppable comps at r = {} | values | {}{}", RADII.map(r).join(" "),
        if keys { "by memo key | " } else { "" }, if ideal { "ideal work left, saved at each r" } else { "" });
    let mut sums = vec![0f64; 4 * RADII.len()];
    let mut samples = 0;
    while !done {
        while l.stats.clocks < next && !done { done = l.run(l.stats.proposals + 1); }
        next += every;
        let heads = memo::heads(&l, &mut terms);
        let (comps, values): (Vec<Head>, Vec<Head>) = heads.iter().partition(|h| memo::computes(h.tag));
        let exact = |h: &Head| Some(h.term);
        let grouped = {
            let mut n = std::collections::HashMap::new();
            for h in &comps { *n.entry(h.term).or_insert(0) += 1; }
            comps.iter().filter(|h| n[&h.term] > 1).count()
        };
        let drop_c: Vec<usize> = RADII.iter().map(|&r| memo::plan(&l, &terms, &comps, &exact, r).len()).collect();
        let drop_v: Vec<usize> = RADII.iter().map(|&r| memo::plan(&l, &terms, &values, &exact, r).len()).collect();
        let mut line = format!("{:>7.0} | {:>5} ({:>4}) {:>5} | {:?} | {:?}", l.stats.clocks, comps.len(), grouped, values.len(), drop_c, drop_v);
        for (i, &x) in drop_c.iter().enumerate() { sums[i] += x as f64; }
        if keys {
            let mut k = std::collections::HashMap::new();
            for h in &comps { if let Some(x) = terms.key(h.term, 200_000) { k.insert(h.id, x); } }
            let by_key = |h: &Head| k.get(&h.id).copied();
            let drop_k: Vec<usize> = RADII.iter().map(|&r| memo::plan(&l, &terms, &comps, &by_key, r).len()).collect();
            for (i, &x) in drop_k.iter().enumerate() { sums[RADII.len() + i] += x as f64; }
            line += &format!(" | {drop_k:?}");
        }
        if ideal {
            let base = memo::remaining(&l, None, 1_000_000);
            let saved: Vec<String> = RADII.iter().map(|&r| {
                let plan = memo::plan(&l, &terms, &comps, &exact, r);
                match (base, memo::remaining(&l, Some(&plan), 1_000_000)) {
                    (Some(b), Some(m)) => { (b as i64 - m as i64).to_string() }
                    _ => "?".into(),
                }
            }).collect();
            for (i, s) in saved.iter().enumerate() { if let Ok(x) = s.parse::<f64>() { sums[2 * RADII.len() + i] += x; } }
            if let Some(b) = base { sums[3 * RADII.len()] += b as f64; }
            line += &format!(" | {} [{}]", base.map_or("?".into(), |b| b.to_string()), saved.join(", "));
        }
        println!("{line}");
        samples += 1;
    }
    let ans = l.readback().map(|t| oracle::show(&t));
    println!("{} at clock {:.0}, {} rewrites; answer {}", if ans == want { "right" } else { "WRONG" }, l.stats.clocks, l.stats.fires, ans.is_some());
    let n = samples.max(1) as f64;
    let mean = |a: &[f64]| a.iter().map(|x| format!("{:.1}", x / n)).collect::<Vec<_>>().join(" ");
    println!("mean droppable comps at r = {}: {}", RADII.map(r).join(" "), mean(&sums[..RADII.len()]));
    if keys { println!("mean by memo key: {}", mean(&sums[RADII.len()..2 * RADII.len()])); }
    if ideal { println!("mean ideal work left {:.0}, saved: {}", sums[3 * RADII.len()] / n, mean(&sums[2 * RADII.len()..3 * RADII.len()])); }
}
