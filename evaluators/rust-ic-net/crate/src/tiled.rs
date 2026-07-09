//! E3 tiled drain skeleton (research/interaction-combinator/SPATIAL_IC.md 12.1):
//! placement at birth, not migration.
//!
//! The node arena is partitioned into T contiguous stripes (tiles), one per worker
//! thread. A pointer arena cannot cheaply move a node, but nets churn: every rewrite
//! kills two agents and births up to four, and the births are placeable. So tension
//! becomes an allocation policy:
//!
//! - Placement at birth: a rule allocating a new agent passes the port at the other end
//!   of the new agent's principal wire as a hint (`alloc_to`, reduce.rs). When the hint
//!   is a node-bearing port the birth goes to that node's tile (the tile where the
//!   interaction will fire, since redexes route by consumer); a var hint gets one
//!   advisory peek at its substitution cell (rule code reads aux ports before resolving
//!   them, so most hints arrive as vars); a still-unresolved or nullary hint falls back
//!   to the firing worker's own tile. The population relocates by generational
//!   turnover, zero copies moved.
//! - Redex routing: `link` detecting an active pair delivers it to the tile owning the
//!   consumer node (nullary consumers like eps ride the producer's tile). Same-tile is
//!   the fast path (plain push onto the local bag); cross-tile goes through the target
//!   tile's inbox (a mutex vec is fine for the skeleton).
//! - Work stealing is demoted to idle fallback: a worker drains its own bag, then its
//!   inbox, and only then steals from other tiles' inboxes, nearest ring neighbors
//!   first. Busy workers donate their bag's oldest half to their own inbox when some
//!   tile is idle, so a private frontier is reachable by thieves. TODO: the tile
//!   topology is a 1D ring here; a mesh shape (and preferring boundary redexes when
//!   stealing) is the next rung.
//! - Metrics are the point: same-tile vs cross-tile redex counts are a live coarse Rent
//!   readout (SPATIAL_IC.md 10), plus inbox pushes, steals, donations, and
//!   birth-placement counters.
//!
//! Measured limits of the skeleton (wide-10-200 + fib-14, 2026-07-08): a load that fills
//! the low stripes leaves them no bump room, so wide-arity births aimed there miss until
//! same-arity frees accumulate (`alloc_fallback` counts the misses; near-full arenas
//! want `-nodes` headroom, and per-worker freelists strand freed cells when the spine
//! migrates). And routing by birth tile means work born under one worker stays with it,
//! so tiled scaling on low-cross workloads rides donations + steals rather than the
//! router. Both are placement-policy findings the counters exist to surface, not
//! correctness issues.
//!
//! Correctness rests on the same backbone as the work-stealing drain (parallel.rs):
//! single-principal-port ownership means the worker firing a redex exclusively owns both
//! agents' cells, and the var exchange (swap/AcqRel) stays the only cross-thread
//! rendezvous inside the rules. Tiling adds one new handoff, the inbox: pushing a redex
//! there transfers ownership of both agents to whoever pops it (safe because each
//! agent's single principal port is inside the redex being handed over, so no other
//! path reaches their cells), and the inbox mutex release/acquire publishes the
//! producer's relaxed cell writes to the taker, exactly as the deque steal does in the
//! work-stealing drain. The sequential path is untouched: every tiled branch sits
//! behind `Worker::tiled == None`, the same never-taken-branch pattern as the tracer.

use crate::net::{Ctx, Net};
#[cfg(not(target_arch = "wasm32"))]
use crate::net::Worker;
use crate::port::*;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

/// Per-(worker, tile) bump lease size in cells. Smaller than the untiled NODE_REGION:
/// each worker may hold one lease per target tile, so T*T leases can be outstanding and
/// big leases would strand too much of a tight arena.
pub(crate) const TILE_REGION: u32 = 256;

/// Local-bag size above which a busy worker donates its oldest half to its own inbox
/// when some tile is idle (see the drain loop). Keeps the hot tail local while making
/// the surplus stealable; low-cross-tile workloads otherwise strand their whole
/// frontier in one worker's private bag. Low on purpose: the bag is drained LIFO
/// (depth-first), so it stays about as deep as the term tree even when each deferred
/// entry is a fat independent subtree.
const SPILL_MIN: usize = 4;

/// One cache line per tile cursor so neighboring tiles' leases do not false-share.
#[repr(align(64))]
pub(crate) struct PaddedCursor(pub(crate) AtomicU32);

/// Shared tiling state: the stripe map, per-tile bump cursors, per-tile inboxes, and the
/// stat sinks workers flush into on exit. One per tiled run, behind an Arc.
pub(crate) struct TileShared {
    pub(crate) tiles: usize,
    stripe: u32,             // node-arena stripe width (whole arena / tiles)
    ends: Vec<u32>,          // per-tile stripe end (bump limit)
    cursors: Vec<PaddedCursor>, // per-tile bump cursor (starts above the loaded prefix)
    pub(crate) inboxes: Vec<Mutex<Vec<(u32, u32)>>>,
    // stat sinks (summed from per-worker locals at thread exit)
    pub(crate) same_tile: AtomicU64,
    pub(crate) cross_tile: AtomicU64,
    pub(crate) inbox_pushes: AtomicU64,
    pub(crate) steals: AtomicU64,
    pub(crate) donated: AtomicU64,
    pub(crate) births_hinted: AtomicU64,
    pub(crate) births_foreign: AtomicU64,
    pub(crate) births_default: AtomicU64,
    pub(crate) alloc_fallback: AtomicU64,
}

impl TileShared {
    /// Partition the whole node arena `[0, cap)` into `tiles` stripes. Nodes already
    /// loaded (below `node_top`) fall into whichever stripe covers their base, so the
    /// initial net is spread across tiles by address; each tile's bump cursor starts at
    /// the loaded prefix's high-water mark clamped into its stripe.
    pub(crate) fn new(net: &Net, tiles: usize) -> Self {
        let cap = net.nodes.len() as u32;
        let top0 = net.node_top.load(Ordering::Relaxed).min(cap);
        let stripe = cap.div_ceil(tiles as u32).max(1);
        let ends: Vec<u32> = (0..tiles).map(|t| (((t as u32) + 1) * stripe).min(cap)).collect();
        let cursors = (0..tiles)
            .map(|t| {
                let start = (t as u32) * stripe;
                PaddedCursor(AtomicU32::new(top0.clamp(start, ends[t])))
            })
            .collect();
        TileShared {
            tiles,
            stripe,
            ends,
            cursors,
            inboxes: (0..tiles).map(|_| Mutex::new(Vec::new())).collect(),
            same_tile: AtomicU64::new(0),
            cross_tile: AtomicU64::new(0),
            inbox_pushes: AtomicU64::new(0),
            steals: AtomicU64::new(0),
            donated: AtomicU64::new(0),
            births_hinted: AtomicU64::new(0),
            births_foreign: AtomicU64::new(0),
            births_default: AtomicU64::new(0),
            alloc_fallback: AtomicU64::new(0),
        }
    }

    /// The tile owning a node base. A division for the skeleton; a shift once stripes
    /// are forced to powers of two.
    #[inline]
    pub(crate) fn tile_of(&self, base: u32) -> usize {
        ((base / self.stripe) as usize).min(self.tiles - 1)
    }

    /// Lease a bump region from tile `t`'s cursor (one atomic per ~TILE_REGION allocs,
    /// the same pattern as the global cursor lease). None when the stripe is exhausted
    /// or the tail is smaller than `size`.
    fn lease(&self, t: usize, size: u32) -> Option<(u32, u32)> {
        if self.cursors[t].0.load(Ordering::Relaxed) >= self.ends[t] {
            return None;
        }
        let s = self.cursors[t].0.fetch_add(TILE_REGION, Ordering::Relaxed);
        if s >= self.ends[t] {
            return None;
        }
        let e = s.saturating_add(TILE_REGION).min(self.ends[t]);
        if e - s < size {
            return None;
        }
        Some((s, e))
    }
}

/// Per-worker tiling state, boxed into `Worker::tiled`. `None` in every non-tiled run;
/// all tiled behavior in net.rs branches on it.
pub(crate) struct TileState {
    pub(crate) shared: Arc<TileShared>,
    pub(crate) tile: usize,
    /// Per-target-tile cached bump lease (next, end). A worker births into foreign
    /// tiles, so it keeps one small lease per tile.
    rgn: Vec<(u32, u32)>,
    /// Per-home-tile intrusive free lists by arity. A freed node stays keyed to the
    /// stripe it lives in, so reuse preserves placement. Worker-local: only the worker
    /// that freed a node reuses it (same as the untiled free lists).
    free_heads: Vec<[u32; 5]>,
    // per-worker stat counters (flushed to TileShared at thread exit)
    pub(crate) same_tile: u64,
    pub(crate) cross_tile: u64,
    pub(crate) births_hinted: u64,
    pub(crate) births_foreign: u64,
    pub(crate) births_default: u64,
    pub(crate) alloc_fallback: u64,
}

/// Free-list end marker, mirrored from net.rs (no real base equals u32::MAX).
const NULL_FREE: u32 = u32::MAX;

impl TileState {
    pub(crate) fn new(shared: Arc<TileShared>, tile: usize) -> Self {
        let tiles = shared.tiles;
        TileState {
            shared,
            tile,
            rgn: vec![(0, 0); tiles],
            free_heads: vec![[NULL_FREE; 5]; tiles],
            same_tile: 0,
            cross_tile: 0,
            births_hinted: 0,
            births_foreign: 0,
            births_default: 0,
            alloc_fallback: 0,
        }
    }
    pub(crate) fn flush_stats(&self) {
        let s = &self.shared;
        s.same_tile.fetch_add(self.same_tile, Ordering::Relaxed);
        s.cross_tile.fetch_add(self.cross_tile, Ordering::Relaxed);
        s.births_hinted.fetch_add(self.births_hinted, Ordering::Relaxed);
        s.births_foreign.fetch_add(self.births_foreign, Ordering::Relaxed);
        s.births_default.fetch_add(self.births_default, Ordering::Relaxed);
        s.alloc_fallback.fetch_add(self.alloc_fallback, Ordering::Relaxed);
    }
}

/// Run summary for a tiled drain. `cross_frac` is the live coarse Rent readout.
#[derive(Default, Clone, Copy)]
pub struct TiledStats {
    pub same_tile: u64,
    pub cross_tile: u64,
    pub inbox_pushes: u64,
    pub steals: u64,
    pub donated: u64,
    pub births_hinted: u64,
    pub births_foreign: u64,
    pub births_default: u64,
    pub alloc_fallback: u64,
}
impl TiledStats {
    pub fn cross_frac(&self) -> f64 {
        let total = self.same_tile + self.cross_tile;
        if total == 0 {
            0.0
        } else {
            self.cross_tile as f64 / total as f64
        }
    }
    fn from_shared(s: &TileShared) -> Self {
        TiledStats {
            same_tile: s.same_tile.load(Ordering::Relaxed),
            cross_tile: s.cross_tile.load(Ordering::Relaxed),
            inbox_pushes: s.inbox_pushes.load(Ordering::Relaxed),
            steals: s.steals.load(Ordering::Relaxed),
            donated: s.donated.load(Ordering::Relaxed),
            births_hinted: s.births_hinted.load(Ordering::Relaxed),
            births_foreign: s.births_foreign.load(Ordering::Relaxed),
            births_default: s.births_default.load(Ordering::Relaxed),
            alloc_fallback: s.alloc_fallback.load(Ordering::Relaxed),
        }
    }
}

/// The tile a detected redex should fire in: the consumer node's tile; a nullary
/// consumer (eps) rides the producer; two nullary agents fire anywhere (`fallback`).
#[inline]
pub(crate) fn route_of(shared: &TileShared, a: u32, b: u32, fallback: usize) -> usize {
    let (c, p) = if is_producer(tag(a)) { (b, a) } else { (a, b) };
    if arity(tag(c)) > 0 {
        shared.tile_of(val(c) as u32)
    } else if arity(tag(p)) > 0 {
        shared.tile_of(val(p) as u32)
    } else {
        fallback
    }
}

impl<'a> Ctx<'a> {
    /// Tiled birth: freelist of cells homed in the wanted tile, then that tile's bump
    /// stripe, then fall back to the worker's own tile and around the ring (counted:
    /// a fallback is a placement miss). Panics only when every stripe is exhausted.
    pub(crate) fn alloc_in_tile(&mut self, want: usize, aux: &[u32]) -> u32 {
        let net = self.net;
        let size = aux.len();
        let ts = self.w.tiled.as_mut().expect("alloc_in_tile: tiled state missing");
        let tiles = ts.shared.tiles;
        let own = ts.tile;
        let mut base: Option<u32> = None;
        for k in 0..=tiles {
            // candidate order: wanted tile, own tile, then the ring from own
            let t = match k {
                0 => want,
                1 => own,
                _ => (own + k - 1) % tiles,
            };
            if k >= 1 && t == want {
                continue; // already tried as the wanted tile
            }
            if k >= 2 && t == own {
                continue; // already tried as the own-tile fallback
            }
            let head = ts.free_heads[t][size];
            if head != NULL_FREE {
                // pop: the freed node's first cell holds the next-free base (net.rs).
                ts.free_heads[t][size] = net.nodes[head as usize].load(Ordering::Relaxed);
                base = Some(head);
            } else {
                let (next, end) = ts.rgn[t];
                if next + (size as u32) <= end {
                    ts.rgn[t] = (next + size as u32, end);
                    base = Some(next);
                } else if let Some((s, e)) = ts.shared.lease(t, size as u32) {
                    ts.rgn[t] = (s + size as u32, e);
                    base = Some(s);
                }
            }
            if base.is_some() {
                if k > 0 {
                    ts.alloc_fallback += 1;
                }
                break;
            }
        }
        let Some(b) = base else {
            panic!("ic-net: tiled node arena overflow");
        };
        for (i, &v) in aux.iter().enumerate() {
            net.nodes[(b + i as u32) as usize].store(v, Ordering::Relaxed);
        }
        b
    }

    /// Tiled free: key the intrusive free list by the node's home tile so reuse
    /// preserves placement. Same mechanism as the untiled list (next-free link stored
    /// in the freed node's first cell), just one list per (tile, arity).
    pub(crate) fn free_node_tiled(&mut self, base: u32, ar: usize) {
        let net = self.net;
        let ts = self.w.tiled.as_mut().expect("free_node_tiled: tiled state missing");
        let t = ts.shared.tile_of(base);
        net.nodes[base as usize].store(ts.free_heads[t][ar], Ordering::Relaxed);
        ts.free_heads[t][ar] = base;
    }

    /// Tiled redex delivery (see the module header): same-tile to the local bag,
    /// cross-tile through the target tile's inbox. Pushing to a foreign inbox transfers
    /// ownership of both agents to the taker; the mutex release/acquire publishes our
    /// relaxed cell writes, so the taker reads fully initialized nodes.
    pub(crate) fn route_redex(&mut self, a: u32, b: u32) {
        let (route, own) = {
            let ts = self.w.tiled.as_ref().expect("route_redex: tiled state missing");
            (route_of(&ts.shared, a, b, ts.tile), ts.tile)
        };
        if route == own {
            self.w.tiled.as_mut().unwrap().same_tile += 1;
            self.w.bag.push((a, b));
        } else {
            let ts = self.w.tiled.as_mut().unwrap();
            ts.cross_tile += 1;
            ts.shared.inbox_pushes.fetch_add(1, Ordering::Relaxed);
            ts.shared.inboxes[route].lock().unwrap().push((a, b));
        }
    }
}

// The drain itself needs real threads, which wasm32 cannot host; the wasm build (the
// sequential oracle) compiles the types above (they ride Worker) but never the driver.
#[cfg(not(target_arch = "wasm32"))]
impl Net {
    /// Reduce `h` to full NF with `threads` tile-pinned workers sharing this net.
    /// Returns the NF root and the tiling metrics, or None on budget exhaustion.
    pub(crate) fn full_nf_tiled(
        &self,
        h: u32,
        threads: usize,
        budget: i64,
    ) -> Option<(u32, TiledStats)> {
        use crossbeam_utils::CachePadded;
        use std::sync::atomic::{AtomicBool, AtomicI64, AtomicUsize};

        let tiles = threads.max(1);
        // Seed with an untiled worker (loader semantics unchanged), then route its bag.
        let mut setup = Worker::new();
        let v = {
            let mut c = self.ctx(&mut setup);
            let v = c.new_var();
            let n = c.alloc(&[pack(VAR, v)]);
            c.link(pack(N, n), h);
            v
        };
        let shared = Arc::new(TileShared::new(self, tiles));
        if setup.bag.is_empty() {
            return Some((self.resolve(pack(VAR, v)), TiledStats::default()));
        }
        for (x, y) in setup.bag.drain(..) {
            let route = route_of(&shared, x, y, 0);
            shared.inbox_pushes.fetch_add(1, Ordering::Relaxed);
            shared.inboxes[route].lock().unwrap().push((x, y));
        }

        const BUDGET_LEASE: i64 = 1 << 16;
        let active = CachePadded::new(AtomicUsize::new(tiles));
        let pool = CachePadded::new(AtomicI64::new(budget));
        let exhausted = CachePadded::new(AtomicBool::new(false));
        let active = &active;
        let pool = &pool;
        let exhausted = &exhausted;
        let shared_ref = &shared;

        std::thread::scope(|s| {
            for t in 0..tiles {
                let shared = Arc::clone(shared_ref);
                s.spawn(move || {
                    let mut w = Worker::new();
                    w.tiled = Some(Box::new(TileState::new(Arc::clone(&shared), t)));
                    let backoff = crossbeam_utils::Backoff::new();
                    let mut local_budget: i64 = 0;
                    let mut local_int: u64 = 0;
                    let mut local_steals: u64 = 0;
                    let mut idle = false;
                    'outer: loop {
                        // 1. drain the local bag; same-tile products land right back on
                        // it (route_redex), cross-tile ones leave through the inboxes.
                        while let Some((x, y)) = w.bag.pop() {
                            if local_budget <= 0 {
                                let got = pool.fetch_sub(BUDGET_LEASE, Ordering::Relaxed);
                                if got <= 0 {
                                    exhausted.store(true, Ordering::Relaxed);
                                    break;
                                }
                                local_budget = got.min(BUDGET_LEASE);
                            }
                            local_budget -= 1;
                            local_int += 1;
                            let mut c = self.ctx(&mut w);
                            c.interact(x, y);
                            // Donate when a tile sits idle and our bag has surplus:
                            // move the oldest half into our own inbox, where the steal
                            // path can reach it (the private bag is otherwise
                            // untouchable, and a low-cross-tile workload would strand
                            // its whole frontier here). Handing the redexes over
                            // transfers ownership of both agents of each to whichever
                            // worker pops them, exactly like a cross-tile push; the
                            // empty-inbox gate bounds churn, try_lock keeps it
                            // non-blocking.
                            if w.bag.len() >= SPILL_MIN && active.load(Ordering::Relaxed) < tiles {
                                if let Ok(mut ib) = shared.inboxes[t].try_lock() {
                                    if ib.is_empty() {
                                        let half = w.bag.len() / 2;
                                        ib.extend(w.bag.drain(..half));
                                        shared.donated.fetch_add(half as u64, Ordering::Relaxed);
                                    }
                                }
                            }
                        }
                        if exhausted.load(Ordering::Relaxed) {
                            break;
                        }
                        // 2. own inbox: take everything other tiles routed to us.
                        {
                            let mut ib = shared.inboxes[t].lock().unwrap();
                            if !ib.is_empty() {
                                w.bag.append(&mut ib);
                                drop(ib);
                                if idle {
                                    active.fetch_add(1, Ordering::AcqRel);
                                    idle = false;
                                }
                                backoff.reset();
                                continue;
                            }
                        }
                        // 3. idle fallback: steal from other tiles' inboxes, nearest
                        // ring neighbors first. Takes the older half (the tail grows;
                        // the front is the oldest). TODO: mesh-shaped neighborhoods and
                        // preferring boundary redexes are the next rung.
                        for d in 1..tiles {
                            for t2 in [(t + d) % tiles, (t + tiles - d) % tiles] {
                                if t2 == t {
                                    continue;
                                }
                                let mut ib = shared.inboxes[t2].lock().unwrap();
                                if ib.is_empty() {
                                    continue;
                                }
                                let take = ib.len().div_ceil(2);
                                w.bag.extend(ib.drain(..take));
                                drop(ib);
                                local_steals += take as u64;
                                if idle {
                                    active.fetch_add(1, Ordering::AcqRel);
                                    idle = false;
                                }
                                backoff.reset();
                                continue 'outer;
                            }
                        }
                        // 4. nothing anywhere: announce idle; quiesce when all are idle.
                        if !idle {
                            active.fetch_sub(1, Ordering::AcqRel);
                            idle = true;
                        }
                        if active.load(Ordering::Acquire) == 0 {
                            break;
                        }
                        backoff.snooze();
                    }
                    if !idle {
                        active.fetch_sub(1, Ordering::AcqRel); // exhausted path
                    }
                    self.interactions.fetch_add(local_int, Ordering::Relaxed);
                    shared.steals.fetch_add(local_steals, Ordering::Relaxed);
                    w.tiled.as_ref().unwrap().flush_stats();
                });
            }
        });
        if exhausted.load(Ordering::Relaxed) {
            return None;
        }
        Some((self.resolve(pack(VAR, v)), TiledStats::from_shared(&shared)))
    }
}
