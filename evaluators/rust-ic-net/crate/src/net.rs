//! The arena, the exchange linker, and the term algebra. `Net` is the shared atomic arena
//! (one per session, borrowed `&self` by all workers); `Worker` is per-thread local state
//! (redex bag + free lists + bump regions); `Ctx` pairs them so the rule kernel (see
//! `reduce`) needs no extra threading. The node cells are OWNED by the firing worker
//! (single-principal invariant ⇒ no contention, Relaxed); only the `vars` substitution
//! cells are the cross-thread rendezvous — the exchange linker (`swap`/AcqRel, no CAS).

use crate::port::*;
use crate::tiled::TileState;
use crate::trace::{dist_bucket, Tracer};
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

/// Intrusive free-list "empty" sentinel. No real base / var index equals `u32::MAX` (the
/// arena caps at 2^21), so it is unambiguous as the end-of-chain / empty marker. The
/// next-free link lives in the freed cell itself, so the per-arity free lists need no side
/// `Vec` (no heap, no bounds check, no length tracking on the hot alloc/free path).
const NULL_FREE: u32 = u32::MAX;

/// The SHARED net state — one per session, borrowed `&self` by all workers. Fixed atomic
/// arena (no realloc under sharing) + atomic bump cursors + atomic counters.
pub(crate) struct Net {
    /// Node cells: a node at base `b`, arity `k`, owns `nodes[b..b+k]`. OWNED by the
    /// firing worker (single-principal invariant) ⇒ accessed Relaxed; the `vars` AcqRel
    /// rendezvous provides cross-thread ordering. `AtomicU32` for Rust-soundness only.
    pub(crate) nodes: Box<[AtomicU32]>,
    /// Substitution cells — the exchange linker's rendezvous and the SOLE cross-thread
    /// shared mutable state. `swap`/AcqRel (no CAS). `vars[v]` = a parked Port or NUL.
    pub(crate) vars: Box<[AtomicU32]>,
    pub(crate) node_top: AtomicU32, // bump cursor into `nodes` (high-water = peak live cells)
    pub(crate) var_top: AtomicU32,  // bump cursor into `vars`
    pub(crate) interactions: AtomicU64, // reduction counter (budget unit)
    pub(crate) dnp: AtomicU64,          // δⁿ ⊗ P firings (shared suspensions)
}

impl Net {
    pub(crate) fn new(node_cap: usize, var_cap: usize) -> Self {
        Net {
            nodes: (0..node_cap).map(|_| AtomicU32::new(NUL)).collect(),
            vars: (0..var_cap).map(|_| AtomicU32::new(NUL)).collect(),
            node_top: AtomicU32::new(0),
            var_top: AtomicU32::new(0),
            interactions: AtomicU64::new(0),
            dnp: AtomicU64::new(0),
        }
    }
    #[inline]
    pub(crate) fn ctx<'a>(&'a self, w: &'a mut Worker) -> Ctx<'a> {
        Ctx { net: self, w }
    }
    /// Follow a wire through resolved substitution cells to a non-var port. The one
    /// canonical wire-walk — `Ctx::resolve` and the parallel readback both delegate here.
    #[inline]
    pub(crate) fn resolve(&self, mut p: u32) -> u32 {
        while is_var(p) {
            let nx = self.vars[val(p)].load(Ordering::Acquire);
            if nx == NUL {
                return p;
            }
            p = nx;
        }
        p
    }
}

/// Per-thread reduction context: a local redex bag + free lists. One per worker thread;
/// its free-list bases index into a SPECIFIC `Net`'s arena, so it is paired with a `Net`
/// via `Ctx` (never shared across nets).
pub(crate) struct Worker {
    /// Scratch the rule kernel pushes new active pairs onto. The sequential driver
    /// (`Ctx::drain`) pops it directly; the parallel driver (`parallel.rs`) flushes it
    /// into a per-worker crossbeam work-stealing deque after each interaction — `bag`
    /// itself stays a plain `Vec` in both modes.
    pub(crate) bag: Vec<(u32, u32)>,
    pub(crate) free_heads: [u32; 5], // intrusive free-list head per arity (free-on-consume);
    // the next-free base is stored IN the freed node's first cell (NULL_FREE = empty).
    pub(crate) free_var_head: u32, // intrusive free-list head for wire cells (next link in the cell)
    pub(crate) node_rgn: (u32, u32), // (next, end) of this worker's leased node region (Stage 3)
    pub(crate) var_rgn: (u32, u32),  // (next, end) of this worker's leased var region
    /// E1 instrumentation (trace.rs). `None` in every normal run — the hot paths pay one
    /// never-taken branch. Sequential-only (the parallel drain never sets it).
    pub(crate) tracer: Option<Box<Tracer>>,
    /// E3 tiling (tiled.rs). `None` in every non-tiled run (the sequential drain, the
    /// work-stealing drain, the wasm oracle): alloc/free/link then behave exactly as
    /// before, paying one never-taken branch, the same pattern as the tracer. Set only
    /// by the tiled drain's workers; never combined with `tracer`.
    pub(crate) tiled: Option<Box<TileState>>,
}
impl Worker {
    pub(crate) fn new() -> Self {
        Worker {
            bag: Vec::new(),
            free_heads: [NULL_FREE; 5],
            free_var_head: NULL_FREE,
            node_rgn: (0, 0),
            var_rgn: (0, 0),
            tracer: None,
            tiled: None,
        }
    }
}

/// A worker operating on a shared net — the rule kernel lives here so call sites need no
/// extra threading: `self.net` is the shared arena, `self.w` the local bag/free-lists.
pub(crate) struct Ctx<'a> {
    pub(crate) net: &'a Net,
    pub(crate) w: &'a mut Worker,
}

impl<'a> Ctx<'a> {
    // ── cell access (owned nodes: Relaxed) ──
    #[inline]
    pub(crate) fn nd(&self, i: u32) -> u32 {
        self.net.nodes[i as usize].load(Ordering::Relaxed)
    }
    #[inline]
    pub(crate) fn set_nd(&self, i: u32, v: u32) {
        self.net.nodes[i as usize].store(v, Ordering::Relaxed);
    }

    /// Allocate a node with the given aux ports; reuses a freed same-arity node else bumps.
    /// In a tiled run an unhinted alloc births into the firing worker's own tile.
    #[inline]
    pub(crate) fn alloc(&mut self, aux: &[u32]) -> u32 {
        if self.w.tiled.is_some() {
            let ts = self.w.tiled.as_mut().unwrap();
            ts.births_default += 1;
            let own = ts.tile;
            return self.alloc_in_tile(own, aux);
        }
        let size = aux.len();
        let base = if self.w.free_heads[size] != NULL_FREE {
            // pop: the freed node's first cell holds the next-free base of this arity.
            let b = self.w.free_heads[size];
            self.w.free_heads[size] = self.nd(b);
            b
        } else {
            // bump within the local region; lease a fresh region when `size` won't fit.
            if self.w.node_rgn.0 + size as u32 > self.w.node_rgn.1 {
                let start = self.net.node_top.fetch_add(NODE_REGION, Ordering::Relaxed);
                assert!(
                    (start as usize) + NODE_REGION as usize <= self.net.nodes.len(),
                    "ic-net: node arena overflow"
                );
                self.w.node_rgn = (start, start + NODE_REGION);
            }
            let b = self.w.node_rgn.0;
            self.w.node_rgn.0 += size as u32;
            b
        };
        for (i, &v) in aux.iter().enumerate() {
            self.set_nd(base + i as u32, v);
        }
        if let Some(t) = self.w.tracer.as_mut() {
            t.node_birth[base as usize] = t.cur;
        }
        base
    }
    /// Placement at birth (SPATIAL_IC.md 12.1): allocate a node whose principal wire is
    /// known to end at `hint`. In a tiled run a node-bearing hint places the birth in
    /// that node's tile (where the future interaction fires); a var or nullary hint
    /// falls back to the worker's own tile. In a non-tiled run this is exactly `alloc`.
    #[inline]
    pub(crate) fn alloc_to(&mut self, hint: u32, aux: &[u32]) -> u32 {
        if self.w.tiled.is_some() {
            // A var hint names an unresolved wire; peek its cell one hop (rule code
            // reads aux ports long before it resolves them, so most hints arrive as
            // vars). Safe: our own occurrence of the var is still unlinked, so the
            // cell cannot have been resolved-and-freed; a racy or NUL read only costs
            // placement quality (the hint is advisory), never correctness.
            let hint =
                if is_var(hint) { self.net.vars[val(hint)].load(Ordering::Relaxed) } else { hint };
            let ts = self.w.tiled.as_mut().unwrap();
            if !is_var(hint) && arity(tag(hint)) > 0 {
                let t = ts.shared.tile_of(val(hint) as u32);
                ts.births_hinted += 1;
                if t != ts.tile {
                    ts.births_foreign += 1;
                }
                return self.alloc_in_tile(t, aux);
            }
            ts.births_default += 1;
            let own = ts.tile;
            return self.alloc_in_tile(own, aux);
        }
        self.alloc(aux)
    }
    /// Return a consumed node's cells to the free list for reuse (`ar` = its arity).
    /// Tiled runs key the free list by the node's home tile, so reuse preserves
    /// placement (a freed cell only ever hosts a birth aimed at its own stripe).
    #[inline]
    pub(crate) fn free_node(&mut self, base: u32, ar: usize) {
        if ar > 0 {
            if self.w.tiled.is_some() {
                self.free_node_tiled(base, ar);
                return;
            }
            // push: stash the old head in this node's first cell, then point the head here.
            self.set_nd(base, self.w.free_heads[ar]);
            self.w.free_heads[ar] = base;
        }
    }
    /// A fresh substitution cell (wire); reuses a freed cell when available.
    #[inline]
    pub(crate) fn new_var(&mut self) -> u32 {
        if self.w.free_var_head != NULL_FREE {
            // pop: the freed cell holds the next-free var index; reset it to NUL for reuse.
            let v = self.w.free_var_head;
            self.w.free_var_head = self.net.vars[v as usize].load(Ordering::Relaxed);
            self.net.vars[v as usize].store(NUL, Ordering::Relaxed);
            return v;
        }
        if self.w.var_rgn.0 >= self.w.var_rgn.1 {
            let start = self.net.var_top.fetch_add(VAR_REGION, Ordering::Relaxed);
            assert!(
                (start as usize) + VAR_REGION as usize <= self.net.vars.len(),
                "ic-net: var arena overflow"
            );
            self.w.var_rgn = (start, start + VAR_REGION);
        }
        let v = self.w.var_rgn.0;
        self.w.var_rgn.0 += 1;
        v
    }

    #[inline]
    pub(crate) fn leaf(&self) -> u32 {
        pack(L, 0)
    }
    #[inline]
    pub(crate) fn stem(&mut self, x: u32) -> u32 {
        let b = self.alloc(&[x]);
        pack(S, b)
    }
    #[inline]
    pub(crate) fn fork(&mut self, l: u32, r: u32) -> u32 {
        let b = self.alloc(&[l, r]);
        pack(F, b)
    }
    #[inline]
    pub(crate) fn susp(&mut self, f: u32, a: u32) -> u32 {
        let b = self.alloc(&[f, a]);
        pack(P, b)
    }

    /// The universal connect. Exchange-based (M2a): when an end is a `Var`, atomically
    /// `swap` our partner into its cell; if the far end already deposited a partner the
    /// second arrival wins, frees the spent cell, and links the two (chaining onward).
    /// Two principals form an active pair → the local bag.
    pub(crate) fn link(&mut self, mut a: u32, mut b: u32) {
        if self.w.tracer.is_some() {
            // Traced path: both ports enter from the current interaction's rule code.
            let cur = self.w.tracer.as_ref().unwrap().cur;
            return self.link_prov(a, cur, b, cur);
        }
        loop {
            if !is_var(a) && !is_var(b) {
                // Redex detected. Tiled runs route it to the tile owning the consumer
                // node (tiled.rs); everything else pushes to the local bag as before.
                if self.w.tiled.is_some() {
                    self.route_redex(a, b);
                } else {
                    self.w.bag.push((a, b));
                }
                return;
            }
            if !is_var(a) {
                std::mem::swap(&mut a, &mut b);
            }
            let v = val(a);
            let prev = self.net.vars[v].swap(b, Ordering::AcqRel);
            if prev == NUL {
                return; // parked b; the far end will find it
            }
            // intrusive free: the var is fully resolved (both occurrences arrived) and now
            // exclusively ours — stash the old head in the dead cell, point the head here.
            self.net.vars[v].store(self.w.free_var_head, Ordering::Relaxed);
            self.w.free_var_head = v as u32;
            a = prev;
            // loop with (prev, b)
        }
    }

    /// `link` with flow provenance (trace.rs): same control flow, plus each side carries
    /// the id of the interaction that last moved it — rule code passes `cur`, a port
    /// swapped out of a var cell carries that cell's recorded parker (`var_writer`).
    /// Sequential-only: the read-around-swap of `var_writer` is unsynchronized.
    fn link_prov(&mut self, mut a: u32, mut pa: u32, mut b: u32, mut pb: u32) {
        loop {
            if !is_var(a) && !is_var(b) {
                self.w.bag.push((a, b));
                let t = self.w.tracer.as_mut().unwrap();
                t.prov_bag.push((pa, pb));
                return;
            }
            if !is_var(a) {
                std::mem::swap(&mut a, &mut b);
                std::mem::swap(&mut pa, &mut pb);
            }
            let v = val(a);
            let prev = self.net.vars[v].swap(b, Ordering::AcqRel);
            let t = self.w.tracer.as_mut().unwrap();
            if prev == NUL {
                t.var_writer[v] = pb;
                return; // parked b; the far end will find it (and read its parker)
            }
            let pprev = t.var_writer[v];
            if is_var(prev) {
                // rung A: var-var chain step distance in the var arena.
                let d = (val(prev) as i64 - v as i64).unsigned_abs();
                t.var_hist[dist_bucket(d)] += 1;
            }
            self.net.vars[v].store(self.w.free_var_head, Ordering::Relaxed);
            self.w.free_var_head = v as u32;
            a = prev;
            pa = pprev;
            // loop with (prev, b)
        }
    }

    /// Follow a wire to a non-var port (delegates to the canonical `Net::resolve`).
    #[inline]
    pub(crate) fn resolve(&self, p: u32) -> u32 {
        self.net.resolve(p)
    }
}
