//! The arena, the exchange linker, and the term algebra. `Net` is the shared atomic arena
//! (one per session, borrowed `&self` by all workers); `Worker` is per-thread local state
//! (redex bag + free lists + bump regions); `Ctx` pairs them so the rule kernel (see
//! `reduce`) needs no extra threading. The node cells are OWNED by the firing worker
//! (single-principal invariant ⇒ no contention, Relaxed); only the `vars` substitution
//! cells are the cross-thread rendezvous — the exchange linker (`swap`/AcqRel, no CAS).

use crate::port::*;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

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
    pub(crate) free: [Vec<u32>; 5], // reusable node bases by arity (free-on-consume)
    pub(crate) free_vars: Vec<u32>, // reusable wire cells
    pub(crate) node_rgn: (u32, u32), // (next, end) of this worker's leased node region (Stage 3)
    pub(crate) var_rgn: (u32, u32),  // (next, end) of this worker's leased var region
}
impl Worker {
    pub(crate) fn new() -> Self {
        Worker {
            bag: Vec::new(),
            free: Default::default(),
            free_vars: Vec::new(),
            node_rgn: (0, 0),
            var_rgn: (0, 0),
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
    #[inline]
    pub(crate) fn alloc(&mut self, aux: &[u32]) -> u32 {
        let size = aux.len();
        let base = if let Some(b) = self.w.free[size].pop() {
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
        base
    }
    /// Return a consumed node's cells to the free list for reuse (`ar` = its arity).
    #[inline]
    pub(crate) fn free_node(&mut self, base: u32, ar: usize) {
        if ar > 0 {
            self.w.free[ar].push(base);
        }
    }
    /// A fresh substitution cell (wire); reuses a freed cell when available.
    #[inline]
    pub(crate) fn new_var(&mut self) -> u32 {
        if let Some(v) = self.w.free_vars.pop() {
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
        loop {
            if !is_var(a) && !is_var(b) {
                self.w.bag.push((a, b));
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
            self.w.free_vars.push(v as u32);
            a = prev;
            // loop with (prev, b)
        }
    }

    /// Follow a wire to a non-var port (delegates to the canonical `Net::resolve`).
    #[inline]
    pub(crate) fn resolve(&self, p: u32) -> u32 {
        self.net.resolve(p)
    }
}
