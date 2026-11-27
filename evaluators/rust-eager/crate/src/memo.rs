//! The eager apply memo. `apply(f,x)=r` is a pure function of `(f,x)` (hash-consing makes
//! `(f,x)` a complete key), so the memo is a PURE cache: any contents are
//! correctness-preserving — only hit rate (speed) and footprint differ.
//!
//! Backed by `FxHashMap`s with a **generational** shed policy: `clear` (called per-file
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
//!
//! Two knobs:
//! - **Lean vs costed entries.** An entry is just the result unless the session will save
//!   a snapshot (`record_costs`), which needs each fact's cold cost and fresh-forest parent.
//!   Lean entries halve the memo (12 bytes a slot instead of 24); it is most of the arena.
//! - **Bounded size** (`set_limit`). Off by default. When on, entries go to a young
//!   generation; when it fills, it becomes the old one and the previous old one is dropped.
//!   A hit in the old generation moves the entry back to the young one. Most hits are on
//!   recent entries, so this keeps far more of the memo's value than dropping everything.

use rustc_hash::FxHashMap;

/// What a memo slot stores for `(f, x)`.
pub(crate) trait Entry: Copy {
    fn new(r: u32, cost: u32, pf: u32, px: u32) -> Self;
    fn result(self) -> u32;
    /// (result, cold cost in fork-dispatches, parent f, parent x)
    fn parts(self) -> (u32, u32, u32, u32);
    fn clear_parent(&mut self);
}
impl Entry for u32 {
    #[inline]
    fn new(r: u32, _: u32, _: u32, _: u32) -> Self {
        r
    }
    #[inline]
    fn result(self) -> u32 {
        self
    }
    fn parts(self) -> (u32, u32, u32, u32) {
        (self, 0, 0, 0)
    }
    fn clear_parent(&mut self) {}
}
impl Entry for (u32, u32, u32, u32) {
    #[inline]
    fn new(r: u32, cost: u32, pf: u32, px: u32) -> Self {
        (r, cost, pf, px)
    }
    #[inline]
    fn result(self) -> u32 {
        self.0
    }
    fn parts(self) -> (u32, u32, u32, u32) {
        self
    }
    fn clear_parent(&mut self) {
        self.2 = 0;
        self.3 = 0;
    }
}

type Map<V> = FxHashMap<(u32, u32), V>;

pub(crate) struct Table<V: Entry> {
    young: Map<V>,
    /// The previous generation; always empty while the memo is unbounded.
    old: Map<V>,
    /// Young-generation size that triggers a turnover (`usize::MAX` = unbounded).
    gen: usize,
}

impl<V: Entry> Table<V> {
    fn new() -> Self {
        Table { young: Map::default(), old: Map::default(), gen: usize::MAX }
    }

    #[inline]
    fn get(&mut self, f: u32, x: u32) -> Option<u32> {
        if let Some(&e) = self.young.get(&(f, x)) {
            return Some(e.result());
        }
        if self.old.is_empty() {
            return None;
        }
        let e = self.old.remove(&(f, x))?;
        self.put((f, x), e);
        Some(e.result())
    }

    #[inline]
    fn put(&mut self, k: (u32, u32), e: V) {
        self.young.insert(k, e);
        if self.young.len() >= self.gen {
            self.old = std::mem::take(&mut self.young);
            self.young.reserve(self.gen);
        }
    }

    fn len(&self) -> usize {
        self.young.len() + self.old.len()
    }

    fn retain(&mut self, keep: impl Fn(u32, u32) -> bool) {
        self.young.retain(|&(f, x), _| keep(f, x));
        self.old.retain(|&(f, x), _| keep(f, x));
    }
}

/// Lean (result only) or costed entries, chosen once per session — see the module doc.
pub(crate) enum Entries {
    Lean(Table<u32>),
    Costed(Table<(u32, u32, u32, u32)>),
}

pub(crate) struct Memo {
    entries: Entries,
    /// Node high-water captured at the first `clear` (after the kernel is interned). Keys
    /// entirely below it are the shared base; `0` = not yet captured.
    baseline: u32,
}

macro_rules! each {
    ($self:expr, $t:ident => $body:expr) => {
        match &mut $self.entries {
            Entries::Lean($t) => $body,
            Entries::Costed($t) => $body,
        }
    };
}

impl Memo {
    pub(crate) fn new() -> Self {
        Memo { entries: Entries::Lean(Table::new()), baseline: 0 }
    }

    /// Keep each entry's cold cost and parent from now on (needed to save a snapshot).
    /// Entries already stored are dropped: their costs were never recorded.
    pub(crate) fn record_costs(&mut self) {
        if let Entries::Lean(t) = &self.entries {
            let mut c = Table::new();
            c.gen = t.gen;
            self.entries = Entries::Costed(c);
        }
    }

    pub(crate) fn records_costs(&self) -> bool {
        matches!(self.entries, Entries::Costed(_))
    }

    #[inline]
    pub(crate) fn get(&mut self, f: u32, x: u32) -> Option<u32> {
        each!(self, t => t.get(f, x))
    }

    #[inline]
    pub(crate) fn insert(&mut self, f: u32, x: u32, r: u32, cost: u32, pf: u32, px: u32) {
        each!(self, t => t.put((f, x), Entry::new(r, cost, pf, px)))
    }

    /// Drop file-local entries, keep the kernel-keyed base. The first call records the
    /// current node high-water as the baseline (keeping everything interned so far); later
    /// calls retain only keys below it.
    pub(crate) fn clear(&mut self, node_hw: u32) {
        if self.baseline == 0 {
            self.baseline = node_hw;
            return;
        }
        let b = self.baseline;
        each!(self, t => t.retain(|f, x| f < b && x < b));
    }

    /// Bound the memo at about `n` entries (0 = unbounded): two generations of `n / 2`.
    pub(crate) fn set_limit(&mut self, n: usize) {
        let gen = if n == 0 { usize::MAX } else { (n / 2).max(1) };
        each!(self, t => {
            t.gen = gen;
            if t.young.len() >= gen {
                t.old = std::mem::take(&mut t.young);
            }
        })
    }

    pub(crate) fn len(&self) -> usize {
        match &self.entries {
            Entries::Lean(t) => t.len(),
            Entries::Costed(t) => t.len(),
        }
    }

    /// Iterate entries as (f, x, result, cost, parent f, parent x) — snapshot persistence.
    /// Lean entries report cost 0 and no parent.
    pub(crate) fn iter(&self) -> Box<dyn Iterator<Item = (u32, u32, u32, u32, u32, u32)> + '_> {
        fn go<V: Entry>(t: &Table<V>) -> impl Iterator<Item = (u32, u32, u32, u32, u32, u32)> + '_ {
            t.young.iter().chain(t.old.iter()).map(|(&(f, x), &e)| {
                let (r, c, pf, px) = e.parts();
                (f, x, r, c, pf, px)
            })
        }
        match &self.entries {
            Entries::Lean(t) => Box::new(go(t)),
            Entries::Costed(t) => Box::new(go(t)),
        }
    }

    /// Keep only entries all of whose ids (`f`, `x`, AND the result `r`) are still live, per
    /// the caller's predicate — for `Arena::end_scope`, which frees nodes a stale memo entry
    /// would otherwise point at. Pure cache, so dropping a still-valid-but-now-uncached entry
    /// only costs a re-reduction.
    pub(crate) fn retain(&mut self, live: impl Fn(u32) -> bool) {
        fn go<V: Entry>(m: &mut Map<V>, live: &impl Fn(u32) -> bool) {
            m.retain(|&(f, x), e| {
                let (r, _, pf, px) = e.parts();
                if !(live(f) && live(x) && live(r)) {
                    return false;
                }
                // A parent whose node was reclaimed is no longer a persisted fact; demote the
                // child to a fresh-forest root so its slot-parent never dangles at save.
                if pf != 0 && !(live(pf) && live(px)) {
                    e.clear_parent();
                }
                true
            });
            m.shrink_to_fit();
        }
        each!(self, t => {
            go(&mut t.young, &live);
            go(&mut t.old, &live);
        })
    }
}
