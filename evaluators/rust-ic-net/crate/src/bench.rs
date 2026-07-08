//! Native CLI / benchmark entry points (M2d). The parallel reducer (`parallel.rs`) has no
//! in-process host — crossbeam/threads don't build on wasm, that needs napi — so a thin
//! native bin (`src/bin/ic-net-cli.rs`) drives these as a cold-subprocess benchmark
//! contestant, mirroring bench/disp-cli.ts: NF on stdout, `reduce_ms` on stderr, the timer
//! excluding the (fixed) arena allocation. `reduce_fold_timed` / `reduce_wide_timed` return
//! `(nf, reduce_ms, interactions)`, or None on budget exhaustion. The whole module is
//! `#[cfg]`-gated to non-wasm targets (see lib.rs).

use crate::net::{Ctx, Net, Worker};
use crate::port::*;
use std::sync::atomic::Ordering;

fn build_not(c: &mut Ctx) -> u32 {
    let sl = c.stem(pack(L, 0)); // S(L) = true
    let fll = c.fork(pack(L, 0), pack(L, 0)); // F(L,L)
    let inner = c.fork(sl, fll);
    c.fork(inner, pack(L, 0)) // F(.., L) = not
}
/// A balanced depth-`d` fork tree whose every leaf is `not^chain false` — a workload of
/// 2^d INDEPENDENT reduction chains (the parallel frontier the demand-spine programs lack).
fn build_wide(c: &mut Ctx, d: usize, chain: usize) -> u32 {
    if d == 0 {
        let mut e = pack(L, 0); // false
        for _ in 0..chain {
            let not = build_not(c);
            e = c.susp(not, e);
        }
        e
    } else {
        let l = build_wide(c, d - 1, chain);
        let r = build_wide(c, d - 1, chain);
        c.fork(l, r)
    }
}
/// Full NF of a root, sequential (`threads<=1`) or parallel, then serialize to ternary.
fn force_emit(net: &Net, h: u32, threads: usize, budget: i64) -> Option<String> {
    let nf = if threads <= 1 {
        let mut w = Worker::new();
        let mut c = net.ctx(&mut w);
        let mut b = budget;
        c.full_nf(h, &mut b)?
    } else {
        net.full_nf_parallel(h, threads, budget)?
    };
    let mut out = Vec::new();
    let mut w = Worker::new();
    net.ctx(&mut w).emit(nf, &mut out);
    String::from_utf8(out).ok()
}
/// Left-fold apply over ternary `terms`, force full NF, serialize — the batch.ts contract.
pub fn reduce_fold_timed(terms: &[Vec<u8>], threads: usize, budget: i64) -> Option<(String, f64, u64)> {
    let net = Net::new(NODE_CAP, VAR_CAP);
    let t0 = std::time::Instant::now();
    let h = {
        let mut w = Worker::new();
        let mut c = net.ctx(&mut w);
        let mut acc: Option<u32> = None;
        for t in terms {
            let mut i = 0usize;
            let term = c.parse(t, &mut i);
            acc = Some(acc.map_or(term, |f| c.susp(f, term)));
        }
        acc?
    };
    let nf = force_emit(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed)))
}
/// E1 trace run summary (SPATIAL_IC.md §10; see trace.rs for the semantics).
pub struct TraceReport {
    pub events: u64,
    pub pop_hist: [u64; 33],
    pub var_hist: [u64; 33],
    pub peak_nodes: u64,
}

/// `reduce_fold_timed` with E1 instrumentation: sequential only, custom arena sizes
/// (checker-shaped terms outgrow the default 2M-cell arena), the rung C event log
/// streamed to `trace_path`, rung A histograms returned in the report.
pub fn reduce_fold_traced(
    terms: &[Vec<u8>],
    budget: i64,
    trace_path: &str,
    node_cap: usize,
    var_cap: usize,
    trace_limit: u64,
) -> std::io::Result<Option<(String, f64, u64, TraceReport)>> {
    let net = Net::new(node_cap, var_cap);
    let mut w = Worker::new();
    w.tracer = Some(Box::new(crate::trace::Tracer::new(trace_path, node_cap, var_cap, trace_limit)?));
    let t0 = std::time::Instant::now();
    let nf = {
        let mut c = net.ctx(&mut w);
        let mut acc: Option<u32> = None;
        for t in terms {
            let mut i = 0usize;
            let term = c.parse(t, &mut i);
            acc = Some(acc.map_or(term, |f| c.susp(f, term)));
        }
        let Some(h) = acc else { return Ok(None) };
        let mut b = budget;
        let Some(nf) = c.full_nf(h, &mut b) else { return Ok(None) };
        let mut out = Vec::new();
        c.emit(nf, &mut out);
        String::from_utf8(out).expect("ternary is ascii")
    };
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    let (events, pop_hist, var_hist) = w.tracer.take().unwrap().finish();
    let report = TraceReport {
        events,
        pop_hist,
        var_hist,
        peak_nodes: net.node_top.load(Ordering::Relaxed) as u64,
    };
    Ok(Some((nf, ms, net.interactions.load(Ordering::Relaxed), report)))
}

/// Build + reduce the wide independent-chain workload (the parallelism benchmark).
pub fn reduce_wide_timed(depth: usize, chain: usize, threads: usize, budget: i64) -> Option<(String, f64, u64)> {
    let net = Net::new(NODE_CAP, VAR_CAP);
    let t0 = std::time::Instant::now();
    let h = {
        let mut w = Worker::new();
        let mut c = net.ctx(&mut w);
        build_wide(&mut c, depth, chain)
    };
    let nf = force_emit(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed)))
}
