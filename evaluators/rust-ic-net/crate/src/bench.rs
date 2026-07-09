//! Native CLI / benchmark entry points (M2d). The parallel reducer (`parallel.rs`) has no
//! in-process host — crossbeam/threads don't build on wasm, that needs napi — so a thin
//! native bin (`src/bin/ic-net-cli.rs`) drives these as a cold-subprocess benchmark
//! contestant, mirroring bench/disp-cli.ts: NF on stdout, `reduce_ms` on stderr, the timer
//! excluding the (fixed) arena allocation. `reduce_fold_timed` / `reduce_wide_timed` return
//! `(nf, reduce_ms, interactions)`, or None on budget exhaustion. The whole module is
//! `#[cfg]`-gated to non-wasm targets (see lib.rs).

use crate::net::{Ctx, Net, Worker};
use crate::port::*;
use crate::tiled::{TileShared, TileState, TiledStats};
use std::sync::atomic::Ordering;
use std::sync::Arc;

fn build_not(c: &mut Ctx) -> u32 {
    let sl = c.stem(pack(L, 0)); // S(L) = true
    let fll = c.fork(pack(L, 0), pack(L, 0)); // F(L,L)
    let inner = c.fork(sl, fll);
    c.fork(inner, pack(L, 0)) // F(.., L) = not
}
/// A balanced depth-`d` fork tree whose every leaf is `not^chain false` — a workload of
/// 2^d INDEPENDENT reduction chains (the parallel frontier the demand-spine programs lack).
/// Crate-visible: the tiled race-detector test (tests.rs) reduces the same build.
pub(crate) fn build_wide(c: &mut Ctx, d: usize, chain: usize) -> u32 {
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
/// Tile-aware `build_wide` (SPATIAL_IC.md 12.1). Same term, but each of the `2^d`
/// independent chains is allocated whole into a distinct tile (round-robin), so the layout
/// starts partitioned along the natural parallel-frontier boundary instead of being loaded
/// contiguously into one stripe and split by the drain afterward. The caller's worker must
/// carry a `TileState` (the loader builds the tiling before load); this function drives
/// `ts.tile` so plain `alloc` (own-tile) places each subtree where we want it.
///
/// Returns `(root, leftmost_chain_tile)`: a spine fork is placed in the tile of its left
/// subtree, keeping it next to real work; the chains (which dominate the node count) drive
/// the split.
fn build_wide_tiled(c: &mut Ctx, d: usize, chain: usize, next: &mut usize, tiles: usize) -> (u32, usize) {
    if d == 0 {
        let t = *next % tiles;
        *next += 1;
        set_loader_tile(c, t);
        let mut e = pack(L, 0); // false
        for _ in 0..chain {
            let not = build_not(c);
            e = c.susp(not, e);
        }
        (e, t)
    } else {
        let (l, lt) = build_wide_tiled(c, d - 1, chain, next, tiles);
        let (r, _rt) = build_wide_tiled(c, d - 1, chain, next, tiles);
        set_loader_tile(c, lt);
        (c.fork(l, r), lt)
    }
}

/// Point the loader worker's current-tile at `t`, so its next plain allocations land in
/// tile `t`'s stripe (`alloc` -> `alloc_in_tile(own)`; net.rs). Only valid while the worker
/// carries a loader `TileState`.
fn set_loader_tile(c: &mut Ctx, t: usize) {
    c.w.tiled.as_mut().expect("set_loader_tile: loader has no tiling").tile = t;
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
    emit_string(net, nf)
}
/// Full NF via the E3 tiled drain (tiled.rs), then serialize; also returns the tiling
/// metrics (same/cross-tile routing, births, steals).
fn force_emit_tiled(net: &Net, h: u32, threads: usize, budget: i64) -> Option<(String, TiledStats)> {
    let (nf, stats) = net.full_nf_tiled(h, threads, budget)?;
    Some((emit_string(net, nf)?, stats))
}
/// `force_emit_tiled` over a tiling that already governed the load (tile-aware loaders):
/// drives `full_nf_tiled_with` so the drain reuses the load-time stripe partition instead
/// of rebuilding one from node addresses.
fn force_emit_tiled_with(net: &Net, shared: Arc<TileShared>, h: u32, budget: i64) -> Option<(String, TiledStats)> {
    let (nf, stats) = net.full_nf_tiled_with(shared, h, budget)?;
    Some((emit_string(net, nf)?, stats))
}
fn emit_string(net: &Net, nf: u32) -> Option<String> {
    let mut out = Vec::new();
    let mut w = Worker::new();
    net.ctx(&mut w).emit(nf, &mut out);
    String::from_utf8(out).ok()
}
/// Left-fold apply ternary `terms` into a fresh worker's net; None when `terms` is empty.
fn load_fold(net: &Net, terms: &[Vec<u8>]) -> Option<u32> {
    let mut w = Worker::new();
    let mut c = net.ctx(&mut w);
    let mut acc: Option<u32> = None;
    for t in terms {
        let mut i = 0usize;
        let term = c.parse(t, &mut i);
        acc = Some(acc.map_or(term, |f| c.susp(f, term)));
    }
    acc
}
/// Tile-aware `load_fold` (SPATIAL_IC.md 12.1): parse round-robin across tiles
/// (`parse_tiled`), so a big folded term is spread over the stripes at construction. The
/// worker must carry a loader `TileState`; a shared round-robin cursor is threaded across
/// terms and the fold's own susp nodes. Byte-identical tree to `load_fold`.
fn load_fold_tiled(c: &mut Ctx, terms: &[Vec<u8>], rr: &mut usize, tiles: usize) -> Option<u32> {
    let mut acc: Option<u32> = None;
    for t in terms {
        let mut i = 0usize;
        let term = c.parse_tiled(t, &mut i, rr, tiles);
        acc = Some(acc.map_or(term, |f| {
            let tt = *rr % tiles;
            *rr += 1;
            c.w.tiled.as_mut().expect("load_fold_tiled: loader has no tiling").tile = tt;
            c.susp(f, term)
        }));
    }
    acc
}
/// Left-fold apply over ternary `terms`, force full NF, serialize — the batch.ts contract.
/// `node_cap`/`var_cap` size the arenas (the CLI's `-nodes`/`-vars`; NODE_CAP default).
/// The fourth field is peak node cells (the bump high-water), for peak comparisons
/// against the rc/tiled modes.
pub fn reduce_fold_timed(
    terms: &[Vec<u8>],
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, u64)> {
    let net = Net::new(node_cap, var_cap);
    let t0 = std::time::Instant::now();
    let h = load_fold(&net, terms)?;
    let nf = force_emit(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    let peak = net.node_top.load(Ordering::Relaxed) as u64;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), peak))
}
/// `reduce_fold_timed` on the tiled drain; the fourth field is the tiling metrics.
pub fn reduce_fold_tiled(
    terms: &[Vec<u8>],
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, TiledStats)> {
    let net = Net::new(node_cap, var_cap);
    let t0 = std::time::Instant::now();
    let h = load_fold(&net, terms)?;
    let (nf, stats) = force_emit_tiled(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), stats))
}
/// Wire-RC run summary (rc.rs): cancellation counters + the peak-cell readout the
/// mechanism exists to lower.
pub struct RcReport {
    pub cancels_at_eps: u64,
    pub cancels_at_park: u64,
    pub cancels_at_copy: u64,
    pub peak_nodes: u64,
}

/// `reduce_fold_timed` with wire-RC cancellation on (rc.rs): sequential only, like the
/// traced entry point. The NF is None on budget exhaustion, but the report (counters,
/// peak) comes back either way: at-exhaustion counters are the diagnostic for whether
/// cancellation is firing on a workload too big to finish.
pub fn reduce_fold_rc(
    terms: &[Vec<u8>],
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> (Option<String>, f64, u64, RcReport) {
    let net = Net::new(node_cap, var_cap);
    let mut w = Worker::new();
    w.rc = Some(Box::new(crate::rc::RcState::new(node_cap, var_cap)));
    let t0 = std::time::Instant::now();
    let nf = {
        let mut c = net.ctx(&mut w);
        let mut acc: Option<u32> = None;
        for t in terms {
            let mut i = 0usize;
            let term = c.parse(t, &mut i);
            acc = Some(acc.map_or(term, |f| c.susp(f, term)));
        }
        acc.and_then(|h| {
            let mut b = budget;
            let nf = c.full_nf(h, &mut b)?;
            let mut out = Vec::new();
            c.emit(nf, &mut out);
            Some(String::from_utf8(out).expect("ternary is ascii"))
        })
    };
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    let rc = w.rc.take().unwrap();
    let report = RcReport {
        cancels_at_eps: rc.cancels_at_eps,
        cancels_at_park: rc.cancels_at_park,
        cancels_at_copy: rc.cancels_at_copy,
        peak_nodes: net.node_top.load(Ordering::Relaxed) as u64,
    };
    (nf, ms, net.interactions.load(Ordering::Relaxed), report)
}

/// `reduce_fold_tiled` with TILE-AWARE loading (SPATIAL_IC.md 12.1): parse round-robin
/// across tiles at construction. For a demand-spine term (fib) the spine drives its own
/// consumer-tile placement anyway, so this is mostly a non-regression check; the point of
/// the round-robin is to not concentrate the initial term in one stripe.
pub fn reduce_fold_tiled_aware(
    terms: &[Vec<u8>],
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, TiledStats)> {
    let net = Net::new(node_cap, var_cap);
    let tiles = threads.max(1);
    let t0 = std::time::Instant::now();
    let shared = Arc::new(TileShared::new(&net, tiles));
    let h = {
        let mut w = Worker::new();
        w.tiled = Some(Box::new(TileState::new(Arc::clone(&shared), 0)));
        let mut c = net.ctx(&mut w);
        let mut rr = 0usize;
        load_fold_tiled(&mut c, terms, &mut rr, tiles)?
    };
    let (nf, stats) = force_emit_tiled_with(&net, Arc::clone(&shared), h, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), stats))
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
pub fn reduce_wide_timed(
    depth: usize,
    chain: usize,
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, u64)> {
    let net = Net::new(node_cap, var_cap);
    let t0 = std::time::Instant::now();
    let h = {
        let mut w = Worker::new();
        let mut c = net.ctx(&mut w);
        build_wide(&mut c, depth, chain)
    };
    let nf = force_emit(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    let peak = net.node_top.load(Ordering::Relaxed) as u64;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), peak))
}
/// `reduce_wide_timed` on the tiled drain; the fourth field is the tiling metrics.
pub fn reduce_wide_tiled(
    depth: usize,
    chain: usize,
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, TiledStats)> {
    let net = Net::new(node_cap, var_cap);
    let t0 = std::time::Instant::now();
    let h = {
        let mut w = Worker::new();
        let mut c = net.ctx(&mut w);
        build_wide(&mut c, depth, chain)
    };
    let (nf, stats) = force_emit_tiled(&net, h, threads, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), stats))
}
/// `reduce_wide_tiled` with TILE-AWARE loading (SPATIAL_IC.md 12.1): create the tiling
/// before load and build each independent chain into its own tile, so the initial layout
/// is already partitioned. Same NF and interaction count as the naive-load tiled path (only
/// node addresses differ); the tiling metrics should show a lower cross-tile fraction.
pub fn reduce_wide_tiled_aware(
    depth: usize,
    chain: usize,
    threads: usize,
    budget: i64,
    node_cap: usize,
    var_cap: usize,
) -> Option<(String, f64, u64, TiledStats)> {
    let net = Net::new(node_cap, var_cap);
    let tiles = threads.max(1);
    let t0 = std::time::Instant::now();
    // Tiling built BEFORE load; the loader worker carries a TileState so `alloc` targets a
    // chosen tile. `TileShared::new` reads node_top (0 here), so each tile's cursor starts
    // at its stripe head; the load then advances each tile's cursor past its own prefix.
    let shared = Arc::new(TileShared::new(&net, tiles));
    let h = {
        let mut w = Worker::new();
        w.tiled = Some(Box::new(TileState::new(Arc::clone(&shared), 0)));
        let mut c = net.ctx(&mut w);
        let mut next = 0usize;
        let (root, _) = build_wide_tiled(&mut c, depth, chain, &mut next, tiles);
        root
    };
    let (nf, stats) = force_emit_tiled_with(&net, Arc::clone(&shared), h, budget)?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((nf, ms, net.interactions.load(Ordering::Relaxed), stats))
}
