//! The eager apply memo. `apply(f,x)=r` is a pure function of `(f,x)` (hash-consing makes
//! `(f,x)` a complete key), so the memo is a PURE cache: any contents are
//! correctness-preserving — only hit rate (speed) and footprint differ.
//!
//! By default it is a bounded direct-mapped cache (`Cache`): one slot per hashed key, a
//! new fact overwriting the old. Most hits are on recent facts, and a growing hash map
//! spends its time on DRAM misses in probing and rehashing; the fixed cache keeps the
//! recent facts at a fraction of the memory and runs the kernel suite ~20% faster for ~3%
//! more steps (re-reductions of evicted facts). `set_limit` resizes it.
//!
//! A session that will save a snapshot (`record_costs`) instead keeps EVERY fact in a hash
//! map with its cold cost and fresh-forest parent, which persistence and `cold_equiv` need.
//! Its `clear` (called per file by the test harness) RETAINS the kernel-keyed entries —
//! those whose key node ids are both below the node high-water captured at the first clear —
//! and drops the file-local ones: the kernel is interned first, so its nodes are a low-id
//! prefix, and keeping that set preserves cross-file reuse of the expensive kernel facts.

use rustc_hash::FxHashMap;

type Map<V> = FxHashMap<(u32, u32), V>;
type Costed = (u32, u32, u32, u32);

/// Direct-mapped cache: `(f, x)` hashes to one slot, and a new fact overwrites whatever
/// sat there. Key `(0, 0)` marks an empty slot (id 0 is the never-interned sentinel).
pub(crate) struct Cache {
    slots: Vec<[u32; 3]>,
    shift: u32,
    len: usize,
}

impl Cache {
    fn new(bits: u32) -> Self {
        // vec![0; n] is calloc'd: untouched pages cost nothing, so small sessions stay small.
        Cache { slots: vec![[0; 3]; 1 << bits], shift: 64 - bits, len: 0 }
    }

    #[inline]
    fn slot(&self, f: u32, x: u32) -> usize {
        (((f as u64) << 32 | x as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15) >> self.shift) as usize
    }

    #[inline]
    fn get(&self, f: u32, x: u32) -> Option<u32> {
        let [sf, sx, r] = self.slots[self.slot(f, x)];
        (sf == f && sx == x).then_some(r)
    }

    #[inline]
    fn put(&mut self, f: u32, x: u32, r: u32) {
        let i = self.slot(f, x);
        if self.slots[i][0] == 0 {
            self.len += 1;
        }
        self.slots[i] = [f, x, r];
    }
}

/// Default cache size: 2^23 slots natively (96 MB of address space, touched only as it
/// fills); 2^20 (12 MB) in wasm, whose linear memory is paid for up front.
#[cfg(not(target_arch = "wasm32"))]
const CACHE_BITS: u32 = 23;
#[cfg(target_arch = "wasm32")]
const CACHE_BITS: u32 = 20;

/// Lean (a bounded result cache) or costed (every fact, with cost and parent), chosen once
/// per session — see the module doc.
pub(crate) enum Entries {
    Lean(Cache),
    Costed(Map<Costed>),
}

pub(crate) struct Memo {
    entries: Entries,
    /// Node high-water captured at the first `clear` (after the kernel is interned). Keys
    /// entirely below it are the shared base; `0` = not yet captured.
    baseline: u32,
}

impl Memo {
    pub(crate) fn new() -> Self {
        Memo { entries: Entries::Lean(Cache::new(CACHE_BITS)), baseline: 0 }
    }

    /// Keep every fact with its cold cost and parent from now on (needed to save a
    /// snapshot). Entries already cached are dropped: their costs were never recorded.
    pub(crate) fn record_costs(&mut self) {
        if let Entries::Lean(_) = self.entries {
            self.entries = Entries::Costed(Map::default());
        }
    }

    pub(crate) fn records_costs(&self) -> bool {
        matches!(self.entries, Entries::Costed(_))
    }

    #[inline]
    pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
        match &self.entries {
            Entries::Lean(c) => c.get(f, x),
            Entries::Costed(m) => m.get(&(f, x)).map(|e| e.0),
        }
    }

    #[inline]
    pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32, cost: u32, pf: u32, px: u32) {
        match &mut self.entries {
            Entries::Lean(c) => c.put(f, x, r),
            Entries::Costed(m) => {
                m.insert((f, x), (r, cost, pf, px));
            }
        }
    }

    /// Shed file-local facts, keep the kernel-keyed base. The first call records the
    /// current node high-water as the baseline (keeping everything interned so far); later
    /// calls retain only keys below it. The lean cache is bounded already, so it keeps all.
    pub(crate) fn clear(&mut self, node_hw: u32) {
        if self.baseline == 0 {
            self.baseline = node_hw;
            return;
        }
        let b = self.baseline;
        if let Entries::Costed(m) = &mut self.entries {
            m.retain(|&(f, x), _| f < b && x < b);
        }
    }

    /// Size the lean cache to about `n` slots (rounded up to a power of two; 0 = default).
    /// Its contents are dropped. A costed memo keeps every fact regardless.
    pub(crate) fn set_limit(&mut self, n: usize) {
        if let Entries::Lean(_) = self.entries {
            let bits = if n == 0 { CACHE_BITS } else { n.next_power_of_two().trailing_zeros().clamp(10, 30) };
            self.entries = Entries::Lean(Cache::new(bits));
        }
    }

    pub(crate) fn len(&self) -> usize {
        match &self.entries {
            Entries::Lean(c) => c.len,
            Entries::Costed(m) => m.len(),
        }
    }

    /// Iterate entries as (f, x, result, cost, parent f, parent x) — snapshot persistence.
    /// Lean entries report cost 0 and no parent.
    pub(crate) fn iter(&self) -> Box<dyn Iterator<Item = (u32, u32, u32, u32, u32, u32)> + '_> {
        match &self.entries {
            Entries::Lean(c) => Box::new(c.slots.iter().filter(|s| s[0] != 0).map(|&[f, x, r]| (f, x, r, 0, 0, 0))),
            Entries::Costed(m) => Box::new(m.iter().map(|(&(f, x), &(r, c, pf, px))| (f, x, r, c, pf, px))),
        }
    }

    /// Keep only entries all of whose ids (`f`, `x`, AND the result `r`) are still live, per
    /// the caller's predicate — for `Arena::end_scope`, which frees nodes a stale memo entry
    /// would otherwise point at. Pure cache, so dropping a still-valid-but-now-uncached entry
    /// only costs a re-reduction.
    pub(crate) fn retain(&mut self, live: impl Fn(u32) -> bool) {
        match &mut self.entries {
            Entries::Lean(c) => {
                for s in c.slots.iter_mut() {
                    if s[0] != 0 && !(live(s[0]) && live(s[1]) && live(s[2])) {
                        *s = [0; 3];
                        c.len -= 1;
                    }
                }
            }
            Entries::Costed(m) => {
                m.retain(|&(f, x), e| {
                    if !(live(f) && live(x) && live(e.0)) {
                        return false;
                    }
                    // A parent whose node was reclaimed is no longer a persisted fact; demote
                    // the child to a fresh-forest root so its slot-parent never dangles at save.
                    if e.2 != 0 && !(live(e.2) && live(e.3)) {
                        e.2 = 0;
                        e.3 = 0;
                    }
                    true
                });
                m.shrink_to_fit();
            }
        }
    }
}
