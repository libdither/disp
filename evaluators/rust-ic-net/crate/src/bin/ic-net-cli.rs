// Native CLI for the rust-ic-net PARALLEL reducer — a cold-subprocess benchmark
// contestant (M2d). The parallel path uses real OS threads, which wasm32 cannot host, so
// this native bin is how a host actually drives it. Mirrors bench/disp-cli.ts: normal form
// on stdout, `reduce_ms=… evaluator=…` on stderr (reduce time excludes process + arena
// setup, matching disp-cli's "session created before t0").
//
//   ic-net-cli -threads N -budget B -ternary t0 t1 …   # left-fold apply the terms
//   ic-net-cli -threads N -wide DEPTH CHAIN            # 2^DEPTH independent not^CHAIN chains

use std::io::Write;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut threads = 1usize;
    let mut budget: i64 = 8_000_000_000;
    let mut wide: Option<(usize, usize)> = None;
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
            "-wide" => {
                let d = args[i + 1].parse().unwrap_or(0);
                let ch = args[i + 2].parse().unwrap_or(0);
                wide = Some((d, ch));
                i += 2;
            }
            "-ternary" => {}
            a => terms.push(a.as_bytes().to_vec()),
        }
        i += 1;
    }

    let result = match wide {
        Some((d, ch)) => rust_ic_net::reduce_wide_timed(d, ch, threads, budget),
        None => {
            if terms.is_empty() {
                eprintln!("ic-net-cli: no ternary terms (or -wide D CHAIN) given");
                std::process::exit(2);
            }
            rust_ic_net::reduce_fold_timed(&terms, threads, budget)
        }
    };

    match result {
        Some((nf, ms, interactions)) => {
            println!("{nf}");
            eprintln!("reduce_ms={ms:.3} evaluator=ic-net-{threads}t interactions={interactions}");
        }
        None => {
            eprintln!("reduce_ms=NaN evaluator=ic-net-{threads}t (budget exhausted)");
            std::process::exit(1);
        }
    }
    let _ = std::io::stdout().flush();
}
