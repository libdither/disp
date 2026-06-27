//! The eager apply memo, behind a backend abstraction so eviction policies can be
//! compared via cargo features. `apply(f,x)=r` is a pure function (hash-consing makes
//! `(f,x)` a complete key), so the memo is a PURE cache: ANY contents are
//! correctness-preserving — only hit rate (speed) and footprint differ. Exactly one
//! backend compiles:
//!
//! - default (no feature): `FxHashMap` + wholesale clear at `memo_limit`. Unbounded by
//!   default; the test harness bounds it by clearing per file. `clear` keeps the map
//!   SMALL and cache-hot — empirically the fastest (eviction caches that keep a big map
//!   pay more in hot-path cache misses than they save in avoided re-reductions).
//! - `memo-watermark`: `FxHashMap`, but `clear` RETAINS the kernel-keyed entries (both
//!   key ids below the node high-water captured at the first clear) and drops only
//!   file-local ones — small map AND cross-file kernel reuse. Uses the low-id structure
//!   (kernel interned first) that off-the-shelf caches can't.
//! - `memo-quickcache`: `quick_cache::unsync` (S3-FIFO), fixed cap, self-evicting,
//!   FxHash (no ahash → wasm-clean). `clear` no-op (the harness clear keeps reuse).
//! - `memo-clock`: a hand-rolled CLOCK (ring `Vec` + ref bits), fixed cap. `clear` no-op.
//!
//! `clear(node_hw)` is threaded the current node count so the watermark backend can
//! capture its baseline; the others ignore it. Eviction is order-based and FxHash is
//! seedless, so every backend is deterministic (byte-identical NFs; only the reported
//! interaction count shifts, as it already does under per-file clears).

#[cfg(any(feature = "memo-quickcache", feature = "memo-clock"))]
pub(crate) const MEMO_CAP: usize = 4_000_000;

// ── default: FxHashMap + wholesale clear at limit (the original behavior) ──
#[cfg(not(any(feature = "memo-quickcache", feature = "memo-clock", feature = "memo-watermark")))]
mod imp {
    use rustc_hash::FxHashMap;
    pub(crate) struct Memo {
        map: FxHashMap<(u32, u32), u32>,
        limit: usize,
    }
    impl Memo {
        pub(crate) fn new() -> Self {
            Memo { map: FxHashMap::default(), limit: usize::MAX }
        }
        #[inline]
        pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
            self.map.get(&(f, x)).copied()
        }
        #[inline]
        pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32) {
            self.map.insert((f, x), r);
            if self.map.len() > self.limit {
                self.map.clear();
            }
        }
        pub(crate) fn clear(&mut self, _node_hw: u32) {
            self.map.clear();
            self.map.shrink_to_fit();
        }
        pub(crate) fn set_limit(&mut self, n: usize) {
            self.limit = if n == 0 { usize::MAX } else { n };
        }
    }
}

// ── memo-watermark: FxHashMap, clear keeps kernel-keyed entries (low-id generational) ──
#[cfg(all(feature = "memo-watermark", not(any(feature = "memo-quickcache", feature = "memo-clock"))))]
mod imp {
    use rustc_hash::FxHashMap;
    pub(crate) struct Memo {
        map: FxHashMap<(u32, u32), u32>,
        baseline: u32, // node high-water at first clear; keys entirely below it are the shared (kernel) base
    }
    impl Memo {
        pub(crate) fn new() -> Self {
            Memo { map: FxHashMap::default(), baseline: 0 }
        }
        #[inline]
        pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
            self.map.get(&(f, x)).copied()
        }
        #[inline]
        pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32) {
            self.map.insert((f, x), r);
        }
        pub(crate) fn clear(&mut self, node_hw: u32) {
            // First clear (after the kernel is interned): record the baseline, keep all.
            if self.baseline == 0 {
                self.baseline = node_hw;
                return;
            }
            // Per-file clears: drop file-local entries, KEEP kernel-keyed reductions (both
            // inputs below the baseline ⇒ a shared reduction any file may recompute).
            let b = self.baseline;
            self.map.retain(|&(f, x), _| f < b && x < b);
        }
        pub(crate) fn set_limit(&mut self, _n: usize) {}
    }
}

// ── memo-quickcache: quick_cache::unsync (S3-FIFO) ──
#[cfg(feature = "memo-quickcache")]
mod imp {
    use super::MEMO_CAP;
    use quick_cache::{unsync::Cache, UnitWeighter};
    use rustc_hash::FxBuildHasher;
    pub(crate) struct Memo {
        c: Cache<(u32, u32), u32, UnitWeighter, FxBuildHasher>,
    }
    impl Memo {
        pub(crate) fn new() -> Self {
            Memo {
                c: Cache::with(
                    MEMO_CAP,
                    MEMO_CAP as u64,
                    UnitWeighter,
                    FxBuildHasher::default(),
                    Default::default(),
                ),
            }
        }
        #[inline]
        pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
            self.c.get(&(f, x)).copied()
        }
        #[inline]
        pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32) {
            self.c.insert((f, x), r);
        }
        pub(crate) fn clear(&mut self, _node_hw: u32) {} // self-bounding; no-op keeps reuse
        pub(crate) fn set_limit(&mut self, _n: usize) {}
    }
}

// ── memo-clock: hand-rolled CLOCK / second-chance over a ring Vec ──
#[cfg(all(feature = "memo-clock", not(feature = "memo-quickcache")))]
mod imp {
    use super::MEMO_CAP;
    use rustc_hash::FxHashMap;
    struct Slot {
        key: (u32, u32),
        val: u32,
        refed: bool,
    }
    pub(crate) struct Memo {
        idx: FxHashMap<(u32, u32), u32>,
        slots: Vec<Slot>,
        hand: usize,
    }
    impl Memo {
        pub(crate) fn new() -> Self {
            Memo { idx: FxHashMap::default(), slots: Vec::new(), hand: 0 }
        }
        #[inline]
        pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
            if let Some(&i) = self.idx.get(&(f, x)) {
                let s = &mut self.slots[i as usize];
                s.refed = true;
                Some(s.val)
            } else {
                None
            }
        }
        #[inline]
        pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32) {
            let key = (f, x);
            if let Some(&i) = self.idx.get(&key) {
                self.slots[i as usize].val = r;
                return;
            }
            if self.slots.len() < MEMO_CAP {
                let i = self.slots.len() as u32;
                self.slots.push(Slot { key, val: r, refed: false });
                self.idx.insert(key, i);
                return;
            }
            while self.slots[self.hand].refed {
                self.slots[self.hand].refed = false;
                self.hand = (self.hand + 1) % MEMO_CAP;
            }
            let victim = self.hand;
            let old_key = self.slots[victim].key;
            self.idx.remove(&old_key);
            self.slots[victim] = Slot { key, val: r, refed: false };
            self.idx.insert(key, victim as u32);
            self.hand = (self.hand + 1) % MEMO_CAP;
        }
        pub(crate) fn clear(&mut self, _node_hw: u32) {}
        pub(crate) fn set_limit(&mut self, _n: usize) {}
    }
}

pub(crate) use imp::Memo;
