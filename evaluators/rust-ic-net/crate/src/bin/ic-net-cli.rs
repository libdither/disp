// Native CLI for the rust-ic-net PARALLEL reducer — a cold-subprocess benchmark
// contestant (M2d). The parallel path uses real OS threads, which wasm32 cannot host, so
// this native bin is how a host actually drives it. Mirrors bench/disp-cli.ts: normal form
// on stdout, `reduce_ms=… evaluator=…` on stderr (reduce time excludes process + arena
// setup, matching disp-cli's "session created before t0").
//
//   ic-net-cli -threads N -budget B -ternary t0 t1 …   # left-fold apply the terms
//   ic-net-cli -threads N -wide DEPTH CHAIN            # 2^DEPTH independent not^CHAIN chains
//   ic-net-cli -tiled -threads N …                     # E3 tiled drain (tiled.rs); prints
//                                                      # same/cross-tile metrics on stderr
//   ic-net-cli -rc …                                   # wire-RC cancellation (rc.rs),
//                                                      # sequential; counters on stderr
//
// E1 instrumentation (SPATIAL_IC.md §10; trace.rs):
//   ic-net-cli -trace out.bin [-trace-limit N] [-nodes N] [-vars N] [-file terms.txt] t0 …
// Sequential only; writes the rung C event log to out.bin, a JSON sidecar (histograms,
// counts) to out.bin.meta.json. `-file` reads whitespace-separated ternary terms (argv
// caps out near 128 KB per arg; elaborated terms are bigger). `-nodes`/`-vars` size the
// arenas in cells for every mode (default 2^21, max 2^28 — the port encoding's index
// width); tiled runs of near-arena-filling loads want headroom, since only stripes above
// the loaded prefix have bump room until freed cells accumulate.

use std::io::Write;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut threads = 1usize;
    let mut budget: i64 = 8_000_000_000;
    let mut tiled = false;
    let mut rc = false;
    let mut wide: Option<(usize, usize)> = None;
    let mut trace: Option<String> = None;
    let mut trace_limit: u64 = 200_000_000;
    let mut node_cap: usize = 1 << 21;
    let mut var_cap: usize = 1 << 21;
    let mut terms: Vec<Vec<u8>> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-threads" => {
                i += 1;
                threads = args[i].parse().unwrap_or(1);
            }
            "-budget" => {
                i += 1;
                budget = args[i].parse().unwrap_or(budget);
            }
            "-tiled" => {
                tiled = true;
            }
            "-rc" => {
                rc = true;
            }
            "-wide" => {
                let d = args[i + 1].parse().unwrap_or(0);
                let ch = args[i + 2].parse().unwrap_or(0);
                wide = Some((d, ch));
                i += 2;
            }
            "-trace" => {
                i += 1;
                trace = Some(args[i].clone());
            }
            "-trace-limit" => {
                i += 1;
                trace_limit = args[i].parse().unwrap_or(trace_limit);
            }
            "-nodes" => {
                i += 1;
                node_cap = args[i].parse().unwrap_or(node_cap);
            }
            "-vars" => {
                i += 1;
                var_cap = args[i].parse().unwrap_or(var_cap);
            }
            "-file" => {
                i += 1;
                let body = std::fs::read_to_string(&args[i]).unwrap_or_else(|e| {
                    eprintln!("ic-net-cli: cannot read {}: {e}", args[i]);
                    std::process::exit(2);
                });
                terms.extend(body.split_whitespace().map(|t| t.as_bytes().to_vec()));
            }
            "-ternary" => {}
            a => terms.push(a.as_bytes().to_vec()),
        }
        i += 1;
    }
    assert!(node_cap <= 1 << 28 && var_cap <= 1 << 28, "arena cap exceeds the 28-bit port index");

    if let Some(path) = trace {
        if threads > 1 || tiled || rc {
            eprintln!("ic-net-cli: -trace is sequential-only and exclusive of -tiled/-rc");
            std::process::exit(2);
        }
        if terms.is_empty() {
            eprintln!("ic-net-cli: no ternary terms given (positional or -file)");
            std::process::exit(2);
        }
        match rust_ic_net::reduce_fold_traced(&terms, budget, &path, node_cap, var_cap, trace_limit) {
            Err(e) => {
                eprintln!("ic-net-cli: trace io error: {e}");
                std::process::exit(2);
            }
            Ok(None) => {
                eprintln!("reduce_ms=NaN evaluator=ic-net-trace (budget exhausted)");
                std::process::exit(1);
            }
            Ok(Some((nf, ms, interactions, rep))) => {
                println!("{nf}");
                eprintln!(
                    "reduce_ms={ms:.3} evaluator=ic-net-trace interactions={interactions} events={} peak_nodes={}",
                    rep.events, rep.peak_nodes
                );
                let hist = |h: &[u64; 33]| {
                    let inner = h.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(",");
                    format!("[{inner}]")
                };
                let meta = format!(
                    "{{\"interactions\":{interactions},\"events\":{},\"peak_nodes\":{},\"reduce_ms\":{ms:.3},\"pop_hist\":{},\"var_hist\":{}}}\n",
                    rep.events,
                    rep.peak_nodes,
                    hist(&rep.pop_hist),
                    hist(&rep.var_hist)
                );
                if let Err(e) = std::fs::write(format!("{path}.meta.json"), meta) {
                    eprintln!("ic-net-cli: meta write failed: {e}");
                    std::process::exit(2);
                }
            }
        }
        let _ = std::io::stdout().flush();
        return;
    }

    if wide.is_none() && terms.is_empty() {
        eprintln!("ic-net-cli: no ternary terms (or -wide D CHAIN) given");
        std::process::exit(2);
    }

    // Wire-RC cancellation (rc.rs): sequential fold only; stderr carries the counters.
    if rc {
        if threads > 1 || tiled {
            eprintln!("ic-net-cli: -rc is sequential-only (cancellation state is unsynchronized)");
            std::process::exit(2);
        }
        if terms.is_empty() {
            eprintln!("ic-net-cli: -rc needs ternary terms (positional or -file)");
            std::process::exit(2);
        }
        let (nf, ms, interactions, rep) =
            rust_ic_net::reduce_fold_rc(&terms, budget, node_cap, var_cap);
        let counters = format!(
            "interactions={interactions} peak_nodes={} cancels_at_park={} cancels_at_copy={} cancels_at_eps={}",
            rep.peak_nodes, rep.cancels_at_park, rep.cancels_at_copy, rep.cancels_at_eps
        );
        match nf {
            Some(nf) => {
                println!("{nf}");
                eprintln!("reduce_ms={ms:.3} evaluator=ic-net-rc {counters}");
            }
            None => {
                eprintln!("reduce_ms=NaN evaluator=ic-net-rc (budget exhausted) {counters}");
                std::process::exit(1);
            }
        }
        let _ = std::io::stdout().flush();
        return;
    }

    // E3 tiled drain: same stdout contract; stderr additionally carries the tiling
    // metrics (same/cross-tile routing = the live coarse Rent readout, SPATIAL_IC.md 10).
    if tiled {
        let result = match wide {
            Some((d, ch)) => rust_ic_net::reduce_wide_tiled(d, ch, threads, budget, node_cap, var_cap),
            None => rust_ic_net::reduce_fold_tiled(&terms, threads, budget, node_cap, var_cap),
        };
        match result {
            Some((nf, ms, interactions, st)) => {
                println!("{nf}");
                eprintln!(
                    "reduce_ms={ms:.3} evaluator=ic-net-tiled-{threads}t interactions={interactions} \
                     same_tile={} cross_tile={} cross_frac={:.4} inbox_pushes={} steals={} donated={} \
                     births_hinted={} births_foreign={} births_default={} alloc_fallback={}",
                    st.same_tile,
                    st.cross_tile,
                    st.cross_frac(),
                    st.inbox_pushes,
                    st.steals,
                    st.donated,
                    st.births_hinted,
                    st.births_foreign,
                    st.births_default,
                    st.alloc_fallback
                );
            }
            None => {
                eprintln!("reduce_ms=NaN evaluator=ic-net-tiled-{threads}t (budget exhausted)");
                std::process::exit(1);
            }
        }
        let _ = std::io::stdout().flush();
        return;
    }

    let result = match wide {
        Some((d, ch)) => rust_ic_net::reduce_wide_timed(d, ch, threads, budget, node_cap, var_cap),
        None => rust_ic_net::reduce_fold_timed(&terms, threads, budget, node_cap, var_cap),
    };

    match result {
        Some((nf, ms, interactions, peak)) => {
            println!("{nf}");
            eprintln!(
                "reduce_ms={ms:.3} evaluator=ic-net-{threads}t interactions={interactions} peak_nodes={peak}"
            );
        }
        None => {
            eprintln!("reduce_ms=NaN evaluator=ic-net-{threads}t (budget exhausted)");
            std::process::exit(1);
        }
    }
    let _ = std::io::stdout().flush();
}
