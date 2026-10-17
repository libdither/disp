//! The chip's block schedule on a GPU, checked bit for bit against the simulator.
//!   strands-gpu --vectors FILE...                                   replay recorded turns and blocks (hw/validate.sh)
//!   strands-gpu TERM --grid N [--depth D] [--seed S] --check [--batch B] [--clocks C]   simulator and GPU in lockstep
//!   strands-gpu TERM --grid N [--depth D] [--seed S] [--batch B] [--clocks C]  the GPU alone
//! `--dense` runs every block and site each clock instead of only the blocks holding something;
//! `--narrow` (with `--check`) runs the turns in one workgroup, each invocation taking many blocks;
//! `--bench` runs exactly `--clocks` clocks without looking for the answer and times the GPU alone;
//! `--latest` runs the current design (lattice.rs `latest`: the chip's schedule with the demand field
//! and forking S rules);
//! `key=value` changes a parameter of the chip's configuration (lattice.rs `Params::set`; `views=1` loads
//! the code's shapes as movement agents; `--every-view` loads every value so, arguments too, so that
//! readers meet views as data, and with `--check` counts the movement agents' rules that fired).
//! TERM may be a workload, a term, or `@FILE` holding one; or `two:RUN`, a two-way program on a value as
//! twoway.rs `parse_run` reads it (`two:add 2 3`, `'two:swap; swap @ (△, △△)'`), run with 4 ports a slot.

use rust_ca_lattice::oracle::{self, Fuel};
use rust_ca_lattice::rules::{rule, N_RULES};
use rust_ic_mesh::term;
use rust_ic_strands::gpu::{Gpu, Grid, REC, SITE, TALLY};
use rust_ic_strands::lattice::{chip, latest, tag_of, Lattice, Params, SiteBytes, OPS};
use rust_ic_strands::twoway;
use std::collections::BTreeMap;

/// Bytes of a recorded site (the chip's).
const SB: usize = 4 * SITE;

fn end(m: u8) -> String { if m == 255 { "-".into() } else { m.to_string() } }
fn tag(t: u8) -> String { if t == 0 { "-".into() } else { tag_of(t).name().into() } }

/// A site's bytes, decoded.
fn show(l: SiteBytes, b: &[u8]) -> String {
    let mates: String = (0..l.ends).filter(|&e| b[e] != 255).map(|e| format!(" {e}:{}", b[e])).collect();
    let (t, w) = (l.tags, l.want);
    format!("mates{mates} | tags {} {} {} | want {} {} {} | pulse {}", tag(b[t]), tag(b[t + 1]), tag(b[t + 2]), b[w], b[w + 1], b[w + 2], end(b[SiteBytes::PULSE]))
}
/// The fields two sites differ in, as `field want→got`.
fn diff(l: SiteBytes, a: &[u8], b: &[u8]) -> String {
    (0..4 * l.words).filter(|&i| a[i] != b[i]).map(|i| match i {
        _ if i < l.ends => format!("end {i} {}→{}", end(a[i]), end(b[i])),
        _ if (l.tags..l.tags + 3).contains(&i) => format!("tag {} {}→{}", i - l.tags, tag(a[i]), tag(b[i])),
        _ if (l.want..l.want + 3).contains(&i) => format!("want {} {}→{}", i - l.want, a[i], b[i]),
        _ => format!("pulse {}→{}", end(a[i]), end(b[i])),
    }).collect::<Vec<_>>().join(", ")
}
fn bytes(w: &[u32]) -> Vec<u8> { w.iter().flat_map(|x| x.to_le_bytes()).collect() }


/// The simulator's counts in the order of the GPU's tally (tiles.wgsl `T_*`).
const COUNTED: [&str; TALLY] = ["turns", "rewrites", "rewrites without room", "steps", "exchanges", "folds", "flips", "collected",
    "walker steps", "walker steps without a seat", "pulses delivered"];
fn counted(l: &Lattice) -> [u64; TALLY] {
    let x = &l.stats;
    [x.proposals, x.fires, x.blocked_fires, x.hops, x.swaps, x.folds, x.flips, x.collected, x.walk_ok, x.walk_fail[0], x.pulses]
}

/// One recorded turn ('T') or block ('B'), as lattice.rs `margolus_clock` writes it.
struct Rec { kind: u8, clock: u32, corner: [i32; 3], dims: [u32; 3], valid: u8, pos: u8, taken: u8, before: Vec<u8>, after: Vec<u8>, touched: u8, stale: u8, op: u8 }

fn parse(data: &[u8]) -> Vec<Rec> {
    let (mut at, mut dims, mut out) = (0, [0u32; 3], vec![]);
    let word = |s: &[u8]| u32::from_le_bytes(s[..4].try_into().unwrap());
    while at < data.len() {
        let kind = data[at];
        let n = match kind { b'H' => 12, b'T' => 19 + 16 * SB + 3, b'B' => 17 + 16 * SB, _ => { eprintln!("unknown record {kind} at byte {at}"); break } };
        if at + 1 + n > data.len() { eprintln!("truncated record at byte {at}"); break; }
        let r = &data[at + 1..at + 1 + n];
        at += 1 + n;
        if kind == b'H' { dims = [word(&r[0..]), word(&r[4..]), word(&r[8..])]; continue; }
        let (pos, taken, s) = if kind == b'T' { (r[17], r[18], 19) } else { (0, 0, 17) };
        let tail = &r[s + 16 * SB..];
        out.push(Rec { kind, clock: word(r), corner: [0, 1, 2].map(|i| word(&r[4 + 4 * i..]) as i32), dims, valid: r[16], pos, taken,
            before: r[s..s + 8 * SB].to_vec(), after: r[s + 8 * SB..s + 16 * SB].to_vec(),
            touched: tail.first().copied().unwrap_or(0), stale: tail.get(1).copied().unwrap_or(0), op: tail.get(2).copied().unwrap_or(0) });
    }
    out
}

/// Replay every record of every file through the GPU and report as hw/sim/tb_block.cpp does.
fn vectors(gpu: &Gpu, files: &[String]) -> bool {
    let mut all_ok = true;
    for f in files {
        let t0 = std::time::Instant::now();
        let recs = parse(&std::fs::read(f).unwrap_or_else(|e| panic!("{f}: {e}")));
        let mut words = Vec::with_capacity(recs.len() * REC);
        for r in &recs {
            words.extend([(r.kind == b'B') as u32, r.clock]);
            words.extend(r.corner.map(|c| c as u32));
            words.extend(r.dims);
            words.extend([r.valid as u32, r.pos as u32, r.taken as u32]);
            words.extend(r.before.chunks(4).map(|c| u32::from_le_bytes(c.try_into().unwrap())));
            words.extend([0, 0]);
        }
        gpu.replay(&mut words);
        let (mut by, mut bad, mut shown) = (BTreeMap::<&str, (u64, u64)>::new(), 0, 0);
        for (i, r) in recs.iter().enumerate() {
            let g = &words[i * REC..(i + 1) * REC];
            let got = bytes(&g[11..91]);
            let mut why = String::new();
            for q in 0..8 {
                let (a, b) = (&r.after[q * SB..(q + 1) * SB], &got[q * SB..(q + 1) * SB]);
                if a != b { why += &format!("  site {q}: {}\n    want {}\n    got  {}\n", diff(gpu.site, a, b), show(gpu.site, a), show(gpu.site, b)); }
            }
            if r.kind == b'T' {
                if g[91] != r.touched as u32 { why += &format!("  touched want {:02x} got {:02x}\n", r.touched, g[91]); }
                if g[92] != r.stale as u32 { why += &format!("  stale want {} got {}\n", r.stale, g[92]); }
            }
            let kind = if r.kind == b'B' { "block" } else { OPS.get(r.op as usize).map_or("?", |&o| if o.is_empty() { "none" } else { o }) };
            let e = by.entry(kind).or_default();
            e.0 += 1;
            if !why.is_empty() {
                e.1 += 1;
                bad += 1;
                if shown < 5 {
                    shown += 1;
                    println!("MISMATCH record {} ({kind}) clock {} corner {},{},{} pos {} taken {:02x}\n{why}", i + 1, r.clock, r.corner[0], r.corner[1], r.corner[2], r.pos, r.taken);
                    for q in 0..8 { println!("  before {q}: {}", show(gpu.site, &r.before[q * SB..(q + 1) * SB])); }
                }
            }
        }
        println!("== {f}");
        for (k, (n, m)) in &by { println!("{k:<14} {n:>9} records {m:>6} mismatches"); }
        println!("{} records, {bad} mismatches ({:.2}s)", recs.len(), t0.elapsed().as_secs_f64());
        all_ok &= bad == 0;
    }
    all_ok
}

/// The first few sites two states differ in, with their coordinates.
fn compare(p: &Params, l: SiteBytes, want: &[u32], got: &[u32]) -> Option<String> {
    if want == got { return None; }
    let (a, b, n) = (bytes(want), bytes(got), 4 * l.words);
    let differ: Vec<usize> = (0..want.len() / l.words).filter(|&s| a[s * n..(s + 1) * n] != b[s * n..(s + 1) * n]).collect();
    let mut r = format!("{} sites differ", differ.len());
    for &s in differ.iter().take(4) {
        let (x, y, z) = (s as u32 % p.w, s as u32 / p.w % p.h, s as u32 / (p.w * p.h));
        let (sa, sb) = (&a[s * n..(s + 1) * n], &b[s * n..(s + 1) * n]);
        r += &format!("\n  site ({x}, {y}, {z}): {}\n    cpu {}\n    gpu {}", diff(l, sa, sb), show(l, sa), show(l, sb));
    }
    Some(r)
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let start = |p: &Params| { let g = Gpu::new(p).unwrap_or_else(|e| { eprintln!("{e}"); std::process::exit(2) }); println!("GPU: {}", g.name); g };
    if args.first().is_some_and(|a| a == "--vectors") { std::process::exit(if vectors(&start(&chip()), &args[1..]) { 0 } else { 1 }); }
    let mut p = chip();
    let (mut src, mut check, mut dense, mut narrow, mut bench, mut batch, mut max, mut every) = (None, false, false, false, false, None, 1_000_000u64, false);
    let mut it = args.iter();
    while let Some(a) = it.next() {
        let mut val = || it.next().expect("a value").clone();
        match a.as_str() {
            "--grid" => {
                let v = val();
                let (w, h) = v.split_once('x').unwrap_or((&v, &v));
                (p.w, p.h) = (w.parse().unwrap(), h.parse().unwrap());
            }
            "--depth" => p.depth = val().parse().unwrap(),
            "--seed" => p.seed = val().parse().unwrap(),
            "--batch" => batch = Some(val().parse::<usize>().unwrap()),
            "--clocks" => max = val().parse().unwrap(),
            "--check" => check = true,
            "--dense" => dense = true,
            "--narrow" => narrow = true,
            "--bench" => bench = true,
            "--every-view" => every = true,
            "--latest" => p = Params { w: p.w, h: p.h, depth: p.depth, seed: p.seed, ..latest() },
            _ => match a.split_once('=') {
                Some((k, v)) => p.set(k, v).unwrap_or_else(|e| panic!("{e}")),
                None => src = Some(a.clone()),
            },
        }
    }
    let src = src.expect("term");
    // A two-way program runs eagerly with 4 ports a slot, its answer the program handed back and the result.
    let two = src.strip_prefix("two:").map(|r| twoway::parse_run(r).unwrap_or_else(|e| panic!("{e}")));
    if two.is_some() { p.set("two", "1").unwrap(); }
    let gpu = start(&p);
    // `@FILE` holds the term; `@(f,x)` is a term itself.
    let src = match src.strip_prefix('@').filter(|path| !path.starts_with('(')) { Some(path) => std::fs::read_to_string(path).expect("term file").trim().to_string(), None => src };
    let t = match src.split_once(':') {
        _ if two.is_some() => oracle::Term::L,
        Some((name, n)) if term::workload(name, 0).is_some() => term::workload(name, n.parse().unwrap()).unwrap(),
        _ => term::workload(&src, 0).unwrap_or_else(|| term::parse(&src).expect("bad term")),
    };
    let want = match &two {
        Some((prog, x)) => prog.run(x, &mut 1_000_000).map(|y| y.show()),
        None => oracle::nf(t.clone(), &mut Fuel(100_000_000)).ok().map(|w| oracle::show(&w)),
    };
    let (net, out) = if let Some((prog, x)) = &two { twoway::net(prog, x) } else if every {
        let mut net = rust_ca_lattice::net::Net::new();
        let root = net.build_every_view(&t);
        let (_, out) = net.drive(root);
        (net, out)
    } else { rust_ic_strands::share::net_for(&(&t).into(), &p).unwrap_or_else(|e| panic!("{e}")) };
    let mut l = Lattice::load(p, net, out).expect("load");
    let show_ans = |l: &Lattice| if two.is_some() { twoway::answer(&l.shadow).map_or("-".into(), |(_, y)| y.show()) }
                                 else { l.readback().map(|t| oracle::show(&t)).unwrap_or("-".into()) };
    let answered = |l: &Lattice| if two.is_some() { twoway::answer(&l.shadow).is_some() } else { l.readback().is_some() };
    let layout = l.site_bytes();
    let want = want.as_deref().unwrap_or("?");
    assert!(!dense || !l.fields.on, "--dense keeps no field");
    // The simulator's demand field, a value a site, if it keeps one.
    let field = |l: &Lattice| l.fields.on.then(|| (0..l.sites()).map(|s| l.fields.value(s)).collect::<Vec<u8>>());
    let t0 = std::time::Instant::now();
    if check {
        // Compared after every batch (one clock unless --batch).
        let batch = batch.unwrap_or(1);
        let mut grid = Grid::new(&gpu, p.w, p.h, p.depth, batch);
        if p.views || every { l.trace_start(); }
        let state = l.state_words();
        grid.upload(&state, field(&l).as_deref());
        if let Some(d) = compare(&p, layout, &state, &grid.download()) { println!("upload differs: {d}"); std::process::exit(1); }
        let mut c = 0;
        while c < max && !(answered(&l) || l.shadow.active_pair().is_none()) {
            let n = (batch as u64).min(max - c);
            let before = counted(&l);
            l.fire_log.clear();
            grid.busy_hint = if narrow { 0 } else { l.live().len() as u32 };
            for _ in 0..n { l.chip_clock(); }
            grid.run(c, p.seed, n as usize, dense, false);
            c += n;
            if let Some(d) = compare(&p, layout, &l.state_words(), &grid.download()) {
                println!("clock {c} differs (after {} matching): {d}", c - n);
                std::process::exit(1);
            }
            if let Some(want) = field(&l) {
                let got = grid.download_field();
                let off: Vec<u32> = (0..l.sites()).filter(|&s| want[s as usize] != got[s as usize]).collect();
                if !off.is_empty() {
                    let at = off.iter().take(4).map(|&s| format!("{s} cpu {} gpu {}", want[s as usize], got[s as usize])).collect::<Vec<_>>().join(", ");
                    println!("clock {c}: the field differs at {} sites (after {} matching): {at}", off.len(), c - n);
                    std::process::exit(1);
                }
            }
            let (got, mut fires) = grid.take_tally();
            let want = counted(&l);
            let off: Vec<String> = (0..TALLY).filter(|&i| got[i] as u64 != want[i] - before[i])
                .map(|i| format!("{} cpu {} gpu {}", COUNTED[i], want[i] - before[i], got[i])).collect();
            let mut cpu_fires = l.fire_log.clone();
            cpu_fires.sort_unstable();
            fires.sort_unstable();
            if !off.is_empty() || cpu_fires != fires {
                println!("clocks {} to {c}: the counts differ: {}{}", c - n, off.join(", "), if cpu_fires != fires { format!(" (rewrites at cpu {cpu_fires:?} gpu {fires:?})") } else { String::new() });
                std::process::exit(1);
            }
        }
        let done = answered(&l) || l.shadow.active_pair().is_none();
        println!("{} — answer {} (want {want})", if done { "DONE" } else { "UNFINISHED" }, show_ans(&l));
        println!("{c} clocks matched on all {} sites{}, and so did every count ({} rewrites, {} steps)  {:.2}s", l.sites(),
            if l.fields.on { " and their fields" } else { "" }, l.stats.fires, l.stats.hops, t0.elapsed().as_secs_f64());
        if l.trace.on {
            let mut by = BTreeMap::new();
            for f in l.trace.fires.iter().filter(|f| f.1 >= N_RULES) { let r = rule(f.1); *by.entry(format!("{}·{}", r.consumer.name(), r.producer.name())).or_insert(0) += 1; }
            println!("movement agents' rules: {}", by.iter().map(|(k, n)| format!("{k} {n}")).collect::<Vec<_>>().join(", "));
        }
        return;
    }
    let batch = batch.unwrap_or(64);
    let mut grid = Grid::new(&gpu, p.w, p.h, p.depth, batch);
    grid.upload(&l.state_words(), field(&l).as_deref());
    if bench {
        // The GPU raises its clock only under load: a first pass warms it, the second is timed.
        for c in (0..max.min(4096)).step_by(batch) { grid.run(c, p.seed, batch.min((max.min(4096) - c) as usize), dense, false); }
        grid.take_tally();
        grid.upload(&l.state_words(), field(&l).as_deref());
        let t0 = std::time::Instant::now();
        let mut c = 0u64;
        while c < max {
            let n = (batch as u64).min(max - c);
            grid.run(c, p.seed, n as usize, dense, false);
            c += n;
        }
        let (tally, _) = grid.take_tally();
        let dt = t0.elapsed().as_secs_f64();
        println!("{c} clocks in {dt:.3}s: {:.1} µs a clock, {:.0} clocks/s ({} rewrites, {} turns)", dt * 1e6 / c as f64, c as f64 / dt, tally[1], tally[0]);
        return;
    }
    // The answer is looked for after every batch, among the sites holding anything (`--dense`:
    // everywhere); a site that held something at the last look and is not among them is empty now.
    let (empty, sw) = (layout.empty(), layout.words);
    let (mut c, mut done, mut looking) = (0u64, answered(&l), 0f64);
    let mut prev: std::collections::HashSet<u32> = l.live().iter().copied().collect();
    while c < max && !done {
        let n = (batch as u64).min(max - c);
        grid.run(c, p.seed, n as usize, dense, !dense);
        c += n;
        let t1 = std::time::Instant::now();
        if dense { l.set_state_words(&grid.download()); } else {
            let (idx, words, _) = grid.gathered();
            let now: std::collections::HashSet<u32> = idx.iter().copied().collect();
            for &s in prev.difference(&now) { l.set_site_words(s, &empty); }
            for (j, &s) in idx.iter().enumerate() { l.set_site_words(s, &words[j * sw..(j + 1) * sw]); }
            prev = now;
        }
        // The abstract net is what a two-way run is read from: rebuilt from the sites.
        if two.is_some() { l.adopt(); }
        done = answered(&l);
        looking += t1.elapsed().as_secs_f64();
    }
    let dt = t0.elapsed().as_secs_f64();
    let (tally, _) = grid.take_tally();
    println!("{} — answer {} (want {want})", if done { "DONE" } else { "UNFINISHED" }, show_ans(&l));
    println!("{}", (0..TALLY).map(|i| format!("{} {}", COUNTED[i], tally[i])).collect::<Vec<_>>().join(", "));
    println!("clocks {c} (checked every {batch})  {} sites, {} holding anything at the end  {dt:.2}s ({looking:.2}s looking for the answer)  {:.0} clocks/s",
        l.sites(), l.live().len(), c as f64 / dt.max(1e-9));
}
