//! Parallel corpus probe with selectable projection-check frequency and topology. Wrong
//! normal forms and projection failures still panic with the corpus index and term. The
//! default uses end-level checks and a shorter drought window for fast liveness iteration;
//! the commit gate stays the full suite:
//! `cargo test --release -- --include-ignored`.
//!
//! Usage: probe-corpus [drought=800] [mode=search] [check=end] [topology=both]. Modes:
//! search, stamp, stamp+search, grow, grow+search, all. Checks: end, tick, every, mixed
//! (`Every` for each eighth corpus term, `Tick` for the rest, matching the stage-2 gate).
//! Topologies: both, bilayer, full3d. Terms are Rc-based (not Send), so workers share only
//! an atomic corpus index and regenerate the requested deterministic prefix locally.
use rust_ca_lattice::lattice::{Topo, TOPOS};
use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ca_lattice::scheduler::{run_term_opts, CheckLevel, FireMode};
use std::sync::{
    atomic::{AtomicU32, Ordering},
    mpsc, Arc,
};
use std::thread;

fn main() {
    let drought: u64 = std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(800);
    let mode_arg = std::env::args().nth(2).unwrap_or_else(|| "search".into());
    let check_arg = std::env::args().nth(3).unwrap_or_else(|| "end".into());
    let (check, mixed_check) = match check_arg.as_str() {
        "end" => (CheckLevel::End, false),
        "tick" => (CheckLevel::Tick, false),
        "every" => (CheckLevel::Every, false),
        "mixed" => (CheckLevel::Tick, true),
        other => panic!("unknown check frequency {other}"),
    };
    let topo_arg = std::env::args().nth(4).unwrap_or_else(|| "both".into());
    let topologies: Vec<Topo> = match topo_arg.as_str() {
        "both" => TOPOS.to_vec(),
        "bilayer" => vec![Topo::Bilayer],
        "full3d" => vec![Topo::Full3D],
        other => panic!("unknown topology {other}"),
    };
    let modes: Vec<(FireMode, &str)> = match mode_arg.as_str() {
        "search" => vec![(FireMode::Search, "search")],
        "stamp" => vec![(FireMode::Stamp, "stamp")],
        "stamp+search" => vec![(FireMode::StampThenSearch, "stamp+search")],
        "grow" => vec![(FireMode::Grow, "grow")],
        "grow+search" => vec![(FireMode::GrowThenSearch, "grow+search")],
        "all" => vec![
            (FireMode::Search, "search"), (FireMode::Stamp, "stamp"),
            (FireMode::StampThenSearch, "stamp+search"), (FireMode::Grow, "grow"),
            (FireMode::GrowThenSearch, "grow+search"),
        ],
        other => panic!("unknown mode {other}"),
    };
    let workers = thread::available_parallelism().map(|n| n.get().saturating_sub(2).max(1)).unwrap_or(8) as u32;
    for (mode, label) in modes { for &topo in &topologies {
        let (tx, rx) = mpsc::channel::<(u32, bool, bool)>();
        let cursor = Arc::new(AtomicU32::new(0));
        let mut handles = vec![];
        for _ in 0..workers {
            let tx = tx.clone();
            let cursor = Arc::clone(&cursor);
            handles.push(thread::spawn(move || {
                let n_terms = 400;
                loop {
                    let i = cursor.fetch_add(1, Ordering::Relaxed);
                    if i >= n_terms { break; }
                    let mut rng = Lcg(999);
                    let term = (0..=i).map(|j| rng.rand_term(3 + (j % 3))).last().unwrap();
                    let Ok(nf) = oracle::nf(term.clone(), &mut Fuel(20_000)) else { continue };
                    let want = oracle::show(&nf);
                    let term_check = if mixed_check && i % 8 == 0 {
                        CheckLevel::Every
                    } else {
                        check
                    };
                    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        run_term_opts(&term, topo, mode, 20_000, term_check, drought)
                    })).unwrap_or_else(|failure| {
                        eprintln!("run panicked on corpus term {i} ({} · {label} · {term_check:?}): {}",
                            topo.name(), oracle::show(&term));
                        std::panic::resume_unwind(failure)
                    });
                    if out.done {
                        assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                            "WRONG NF on term {i} ({})", topo.name());
                    }
                    tx.send((i, out.done, out.stuck)).unwrap();
                }
            }));
        }
        drop(tx);
        let mut results: Vec<(u32, bool, bool)> = rx.iter().collect();
        for h in handles { h.join().unwrap(); }
        results.sort();
        let pass = results.iter().filter(|(_, done, _)| *done).count();
        let stuck: Vec<u32> = results.iter()
            .filter(|(_, done, is_stuck)| !done && *is_stuck)
            .map(|(i, _, _)| *i).collect();
        let capped: Vec<u32> = results.iter()
            .filter(|(_, done, is_stuck)| !done && !is_stuck)
            .map(|(i, _, _)| *i).collect();
        let skipped = 400 - results.len();
        println!("[{} · {label}] {pass} pass, {} stuck, {} capped, {skipped} skipped (drought {drought})",
            topo.name(), stuck.len(), capped.len());
        println!("  stuck: {stuck:?}");
        assert!(capped.is_empty(), "tick-capped corpus terms: {capped:?}");
        let denom = pass + stuck.len();
        let floor_ok = match topo {
            Topo::Bilayer => pass * 5 >= denom * 3,
            Topo::Full3D => pass * 5 >= denom * 4,
        };
        assert!(denom > 0 && floor_ok, "liveness collapsed on {}: {pass}/{denom}", topo.name());
    } }
}
