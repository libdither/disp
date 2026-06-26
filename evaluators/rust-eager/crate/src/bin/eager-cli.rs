// Native CLI for rust-eager — the fair native-subprocess baseline for the rust-ic-net
// benchmark (M2d). Same I/O contract as bench/disp-cli.ts and ic-net-cli: normal form on
// stdout, `reduce_ms=… evaluator=…` on stderr (reduce time excludes arena setup). Running
// rust-eager natively here (vs its in-process wasm backend) isolates the wasm-vs-native
// confound from ic-net's parallel-scaling, so a head-to-head measures the net design.
//
//   eager-cli -budget B -ternary t0 t1 …   # left-fold apply the terms, force NF

use std::io::Write;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut budget: i64 = 8_000_000_000;
    let mut wide: Option<(usize, usize)> = None;
    let mut terms: Vec<Vec<u8>> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
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
        Some((d, ch)) => rust_eager::reduce_wide_timed(d, ch, budget),
        None => {
            if terms.is_empty() {
                eprintln!("eager-cli: no ternary terms (or -wide D CHAIN) given");
                std::process::exit(2);
            }
            rust_eager::reduce_fold_timed(&terms, budget)
        }
    };
    match result {
        Some((nf, ms, interactions)) => {
            println!("{nf}");
            eprintln!("reduce_ms={ms:.3} evaluator=rust-eager-native interactions={interactions}");
        }
        None => {
            eprintln!("reduce_ms=NaN evaluator=rust-eager-native (budget exhausted)");
            std::process::exit(1);
        }
    }
    let _ = std::io::stdout().flush();
}
