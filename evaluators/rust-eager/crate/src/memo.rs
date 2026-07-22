//! The eager apply memo. `apply(f,x)=r` is a pure function of `(f,x)` (hash-consing makes
//! `(f,x)` a complete key), so the memo is a PURE cache: any contents are
//! correctness-preserving — only hit rate (speed) and footprint differ.
//!
//! Backed by an `FxHashMap` with a **generational** shed policy: `clear` (called per-file
//! by the test harness, or under memory pressure) RETAINS the kernel-keyed entries — those
//! whose key node ids are both below the node high-water captured at the first clear — and
//! drops only the file-local ones. The kernel is interned first, so its nodes are a low-id
//! prefix; keeping just that small set preserves cross-file reuse of the (expensive) kernel
//! reductions while keeping the map small and cache-hot.
//!
//! This beat both a hand-rolled CLOCK and quick_cache's S3-FIFO in benchmarks (see the
//! preceding `bench(rust-eager)` commit): a *big* cross-file cache is slower — hot-path
//! `get` cache-misses on a millions-entry map outweigh the re-reductions saved. Keeping
//! only the kernel-keyed set is the win, and it uses the low-id structure no off-the-shelf
//! cache can see.

use rustc_hash::FxHashMap;

pub(crate) struct Memo {
    map: FxHashMap<(u32, u32), (u32, u32)>, // (f,x) -> (result, recompute cost in fork-dispatches)
    /// Node high-water captured at the first `clear` (after the kernel is interned). Keys
    /// entirely below it are the shared base; `0` = not yet captured.
    baseline: u32,
    /// Optional hard cap (`tc_set_memo_limit`): a safety net for long-lived sessions that
    /// never call `clear` — over it, shed (retaining the kernel base). `usize::MAX` = off.
    limit: usize,
}

impl Memo {
    pub(crate) fn new() -> Self {
        Memo { map: FxHashMap::default(), baseline: 0, limit: usize::MAX }
    }

    #[inline]
    pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
        self.map.get(&(f, x)).map(|&(r, _)| r)
    }

    #[inline]
    pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32, cost: u32) {
        self.map.insert((f, x), (r, cost));
        if self.map.len() > self.limit {
            self.shed();
        }
    }

    /// Drop file-local entries, keep the kernel-keyed base. The first call records the
    /// current node high-water as the baseline (keeping everything interned so far); later
    /// calls retain only keys below it.
    pub(crate) fn clear(&mut self, node_hw: u32) {
        if self.baseline == 0 {
            self.baseline = node_hw;
            return;
        }
        self.shed();
    }

    /// Cap the memo (0 = unbounded). The hard-cap path for long sessions; the test harness
    /// leaves it unbounded and relies on the per-file `clear`.
    pub(crate) fn set_limit(&mut self, n: usize) {
        self.limit = if n == 0 { usize::MAX } else { n };
    }

    /// Iterate entries as (f, x, result, cost) — snapshot persistence.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (u32, u32, u32, u32)> + '_ {
        self.map.iter().map(|(&(f, x), &(r, c))| (f, x, r, c))
    }

    /// Keep only entries all of whose ids (`f`, `x`, AND the result `r`) are still live, per
    /// the caller's predicate — for `Arena::end_scope`, which frees nodes a stale memo entry
    /// would otherwise point at. Pure cache, so dropping a still-valid-but-now-uncached entry
    /// only costs a re-reduction.
    pub(crate) fn retain(&mut self, live: impl Fn(u32) -> bool) {
        self.map.retain(|&(f, x), &mut (r, _)| live(f) && live(x) && live(r));
        self.map.shrink_to_fit();
    }

    /// Shed file-local entries (or, before a baseline exists, everything).
    fn shed(&mut self) {
        if self.baseline == 0 {
            self.map.clear();
        } else {
            let b = self.baseline;
            self.map.retain(|&(f, x), _| f < b && x < b);
        }
    }
}
