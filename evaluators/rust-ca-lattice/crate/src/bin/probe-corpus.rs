//! Fast iteration probe: the two search-mode differentials, PARALLEL over terms, with
//! End-level checks and a shorter drought window. Wrong normal forms still panic — the
//! probe trades stall-detection patience and per-transition projection asserts for
//! wall-clock, never correctness. The commit gate stays the full suite:
//! `cargo test --release -- --include-ignored`.
//!
//! Usage: probe-corpus [drought=800] [mode=search] — prints pass counts and the stuck
//! term indices, so two runs can be diffed term by term. Modes: search, stamp,
//! stamp+search, grow, grow+search, all. Terms are Rc-based (not Send), so every worker
//! regenerates the deterministic corpus and runs its own residue class.
use rust_ca_lattice::lattice::TOPOS;
use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ca_lattice::scheduler::{run_term_opts, CheckLevel, FireMode};
use std::sync::mpsc;
use std::thread;

fn main() {
    let drought: u64 = std::env::args().nth(1).and_then(|s| s.parse().ok()).unwrap_or(800);
    let mode_arg = std::env::args().nth(2).unwrap_or_else(|| "search".into());
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
    for (mode, label) in modes { for topo in TOPOS {
        let (tx, rx) = mpsc::channel::<(u32, bool)>();
        let mut handles = vec![];
        for w in 0..workers {
            let tx = tx.clone();
            handles.push(thread::spawn(move || {
                let mut rng = Lcg(999);
                let n_terms = 400;
                for i in 0..n_terms {
                    let term = rng.rand_term(3 + (i % 3));
                    if i % workers != w { continue; }
                    let Ok(nf) = oracle::nf(term.clone(), &mut Fuel(20_000)) else { continue };
                    let want = oracle::show(&nf);
                    let out = run_term_opts(&term, topo, mode, 20_000, CheckLevel::End, drought);
                    if out.done {
                        assert_eq!(out.result.as_ref().map(oracle::show).as_deref(), Some(want.as_str()),
                            "WRONG NF on term {i} ({})", topo.name());
                    }
                    tx.send((i, out.done)).unwrap();
                }
            }));
        }
        drop(tx);
        let mut results: Vec<(u32, bool)> = rx.iter().collect();
        for h in handles { h.join().unwrap(); }
        results.sort();
        let pass = results.iter().filter(|(_, d)| *d).count();
        let stuck: Vec<u32> = results.iter().filter(|(_, d)| !d).map(|(i, _)| *i).collect();
        println!("[{} · {label}] {pass} pass, {} stuck (drought {drought})", topo.name(), stuck.len());
        println!("  stuck: {stuck:?}");
    } }
}
