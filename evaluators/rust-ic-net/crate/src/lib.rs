//! rust-ic-net — a MATERIALIZED interaction-net evaluator for disp tree calculus.
//!
//! **M0 + M1 + M2a/b (sequential, parallel-ready).** Agents and ports are REAL arena
//! nodes; active pairs sit in a schedulable redex bag. Differentially gated against
//! `rust-eager` (strong confluence ⇒ identical NFs). Design:
//! research/interaction-combinator/RUST_IC_NET_DESIGN.md; calculus: tc-net.typ.
//! - **M0:** the full rule kernel with δˢ (structural copy).
//! - **M1:** the S-rule spawns δⁿ (demand-before-copy) — a duplicated SUSPENSION's work
//!   runs at most once (Theorem 6). **M1b:** free-on-consume GC (linear net ⇒ each agent
//!   consumed once; peak arena tracks the live working set).
//! - **M2a:** atomic exchange linker (`vars` = `AtomicU32`, `swap`/AcqRel, no CAS).
//! - **M2b:** split into a SHARED `Net` (atomic FIXED arena — `nodes`/`vars` `AtomicU32`,
//!   atomic bump cursors, atomic counters) and a per-thread `Worker` (local redex bag +
//!   free lists), carried together by a `Ctx{net, w}`. The node cells are OWNED by the
//!   firing worker (single-principal invariant ⇒ no contention, Relaxed); only `vars`
//!   is the cross-thread rendezvous (AcqRel). This is the substrate M2c parallelizes:
//!   N workers share one `&Net`, each with its own `Worker`. The arena is fixed (no
//!   realloc under `&self`); overflow traps (catchable, `panic=abort`).
#![allow(clippy::missing_safety_doc, dead_code)]

use std::cell::RefCell;
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};

// ── port encoding ────────────────────────────────────────────────────────────
// tag in the low 4 bits; value (node base index / var index) in the high 28 bits.
const TAG_BITS: u32 = 4;
const TAG_MASK: u32 = 0xF;

const NUL: u32 = 0; // empty slot / null (pack(NUL,0) == 0); no real port equals 0
const L: u32 = 1; // leaf  (nullary producer)
const S: u32 = 2; // stem  (1 aux: x)
const F: u32 = 3; // fork  (2 aux: l, r)
const P: u32 = 4; // suspended application (2 aux: f, a)
const A: u32 = 5; // apply (2 aux: arg, res)
const T1: u32 = 6; // first-level dispatch (3 aux: b, c, res)
const T2: u32 = 7; // second-level dispatch (4 aux: w, x, b, res)
const DS: u32 = 8; // structural duplicator δˢ (2 aux: l, r) — copies syntax (Prop 5)
const EPS: u32 = 9; // eraser ε (nullary consumer)
const N: u32 = 10; // recursive normalizer (1 aux: res)
const VAR: u32 = 11; // a substitution-cell wire end (value = vars index)
const DN: u32 = 12; // need duplicator δⁿ (2 aux: l, r) — demand-before-copy (Thm 6)

/// Default fixed-arena sizes (cells). The bump allocator + free-on-consume keep the live
/// set tiny (benchmarks peak ~64K cells), so this is generous headroom; overflow traps.
const NODE_CAP: usize = 1 << 21; // 2M cells (8 MiB)
const VAR_CAP: usize = 1 << 21;

#[inline]
fn pack(tag: u32, val: u32) -> u32 {
    (val << TAG_BITS) | tag
}
#[inline]
fn tag(p: u32) -> u32 {
    p & TAG_MASK
}
#[inline]
fn val(p: u32) -> usize {
    (p >> TAG_BITS) as usize
}
#[inline]
fn is_var(p: u32) -> bool {
    tag(p) == VAR
}
#[inline]
fn is_producer(t: u32) -> bool {
    (L..=P).contains(&t)
}
/// Number of auxiliary cells an agent of this kind occupies (0 = nullary / no node).
#[inline]
fn arity(t: u32) -> usize {
    match t {
        S | N => 1,
        F | P | A | DS | DN => 2,
        T1 => 3,
        T2 => 4,
        _ => 0, // L, EPS, VAR, NUL — nullary, no allocated node
    }
}

/// The SHARED net state — one per session, borrowed `&self` by all workers. Fixed atomic
/// arena (no realloc under sharing) + atomic bump cursors + atomic counters.
struct Net {
    /// Node cells: a node at base `b`, arity `k`, owns `nodes[b..b+k]`. OWNED by the
    /// firing worker (single-principal invariant) ⇒ accessed Relaxed; the `vars` AcqRel
    /// rendezvous provides cross-thread ordering. `AtomicU32` for Rust-soundness only.
    nodes: Box<[AtomicU32]>,
    /// Substitution cells — the exchange linker's rendezvous and the SOLE cross-thread
    /// shared mutable state. `swap`/AcqRel (no CAS). `vars[v]` = a parked Port or NUL.
    vars: Box<[AtomicU32]>,
    node_top: AtomicU32, // bump cursor into `nodes` (high-water = peak live cells)
    var_top: AtomicU32,  // bump cursor into `vars`
    interactions: AtomicU64, // reduction counter (budget unit)
    dnp: AtomicU64,          // δⁿ ⊗ P firings (shared suspensions)
}

impl Net {
    fn new(node_cap: usize, var_cap: usize) -> Self {
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
    fn ctx<'a>(&'a self, w: &'a mut Worker) -> Ctx<'a> {
        Ctx { net: self, w }
    }
}

/// Per-thread reduction context: a local redex bag + free lists. One per worker thread;
/// its free-list bases index into a SPECIFIC `Net`'s arena, so it is paired with a `Net`
/// via `Ctx` (never shared across nets).
struct Worker {
    bag: Vec<(u32, u32)>, // local active-pair stack (M2c: crossbeam work-stealing deque)
    free: [Vec<u32>; 5],  // reusable node bases by arity (free-on-consume)
    free_vars: Vec<u32>,  // reusable wire cells
}
impl Worker {
    fn new() -> Self {
        Worker { bag: Vec::new(), free: Default::default(), free_vars: Vec::new() }
    }
}

/// A worker operating on a shared net — the rule kernel lives here so call sites need no
/// extra threading: `self.net` is the shared arena, `self.w` the local bag/free-lists.
struct Ctx<'a> {
    net: &'a Net,
    w: &'a mut Worker,
}

impl<'a> Ctx<'a> {
    // ── cell access (owned nodes: Relaxed) ──
    #[inline]
    fn nd(&self, i: u32) -> u32 {
        self.net.nodes[i as usize].load(Ordering::Relaxed)
    }
    #[inline]
    fn set_nd(&self, i: u32, v: u32) {
        self.net.nodes[i as usize].store(v, Ordering::Relaxed);
    }

    /// Allocate a node with the given aux ports; reuses a freed same-arity node else bumps.
    #[inline]
    fn alloc(&mut self, aux: &[u32]) -> u32 {
        let size = aux.len();
        let base = if let Some(b) = self.w.free[size].pop() {
            b
        } else {
            let b = self.net.node_top.fetch_add(size as u32, Ordering::Relaxed);
            assert!((b as usize) + size <= self.net.nodes.len(), "ic-net: node arena overflow");
            b
        };
        for (i, &v) in aux.iter().enumerate() {
            self.set_nd(base + i as u32, v);
        }
        base
    }
    /// Return a consumed node's cells to the free list for reuse (`ar` = its arity).
    #[inline]
    fn free_node(&mut self, base: u32, ar: usize) {
        if ar > 0 {
            self.w.free[ar].push(base);
        }
    }
    /// A fresh substitution cell (wire); reuses a freed cell when available.
    #[inline]
    fn new_var(&mut self) -> u32 {
        if let Some(v) = self.w.free_vars.pop() {
            self.net.vars[v as usize].store(NUL, Ordering::Relaxed);
            return v;
        }
        let v = self.net.var_top.fetch_add(1, Ordering::Relaxed);
        assert!((v as usize) < self.net.vars.len(), "ic-net: var arena overflow");
        v
    }

    #[inline]
    fn leaf(&self) -> u32 {
        pack(L, 0)
    }
    #[inline]
    fn stem(&mut self, x: u32) -> u32 {
        let b = self.alloc(&[x]);
        pack(S, b)
    }
    #[inline]
    fn fork(&mut self, l: u32, r: u32) -> u32 {
        let b = self.alloc(&[l, r]);
        pack(F, b)
    }
    #[inline]
    fn susp(&mut self, f: u32, a: u32) -> u32 {
        let b = self.alloc(&[f, a]);
        pack(P, b)
    }

    /// The universal connect. Exchange-based (M2a): when an end is a `Var`, atomically
    /// `swap` our partner into its cell; if the far end already deposited a partner the
    /// second arrival wins, frees the spent cell, and links the two (chaining onward).
    /// Two principals form an active pair → the local bag.
    fn link(&mut self, mut a: u32, mut b: u32) {
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

    /// Drive the local bag to quiescence or until the budget runs out (returns false).
    fn drain(&mut self, budget: &mut i64) -> bool {
        while let Some((x, y)) = self.w.bag.pop() {
            if *budget <= 0 {
                return false;
            }
            *budget -= 1;
            self.net.interactions.fetch_add(1, Ordering::Relaxed);
            self.interact(x, y);
        }
        true
    }

    /// One interaction (consumer ⊗ producer). tc-net.typ §Demand/Dispatch/Sharing.
    fn interact(&mut self, x: u32, y: u32) {
        let (c, p) = if is_producer(tag(x)) { (y, x) } else { (x, y) };
        let ct = tag(c);
        let pt = tag(p);
        if !is_producer(pt) {
            return; // consumer ⊗ consumer — degenerate; ignore (Eps⊗Eps vanishes)
        }
        let cb = val(c) as u32;
        let pb = val(p) as u32;
        match ct {
            // ── A (apply) ──
            A => {
                let arg = self.nd(cb);
                let res = self.nd(cb + 1);
                match pt {
                    L => {
                        let s = self.stem(arg);
                        self.link(res, s);
                    }
                    S => {
                        let x = self.nd(pb);
                        let f = self.fork(x, arg);
                        self.link(res, f);
                    }
                    F => {
                        let l = self.nd(pb);
                        let r = self.nd(pb + 1);
                        let t = self.alloc(&[r, arg, res]);
                        self.link(pack(T1, t), l);
                    }
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        let a2 = self.alloc(&[arg, res]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), pack(A, a2));
                    }
                    _ => {}
                }
            }
            // ── T1 (first-level dispatch on a of △ a b c) ──
            T1 => {
                let b = self.nd(cb);
                let carg = self.nd(cb + 1);
                let res = self.nd(cb + 2);
                match pt {
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), c);
                    }
                    L => {
                        // K rule: △△ b c → b ; erase c
                        self.link(res, b);
                        self.link(pack(EPS, 0), carg);
                    }
                    S => {
                        // S rule: △(△x) b c → (x c)(b c). Sole δ site — spawns δⁿ.
                        let x = self.nd(pb);
                        let c1 = self.new_var();
                        let c2 = self.new_var();
                        let u = self.new_var();
                        let w = self.new_var();
                        let d = self.alloc(&[pack(VAR, c1), pack(VAR, c2)]);
                        self.link(pack(DN, d), carg);
                        let a1 = self.alloc(&[pack(VAR, c1), pack(VAR, u)]);
                        self.link(pack(A, a1), x);
                        let a2 = self.alloc(&[pack(VAR, c2), pack(VAR, w)]);
                        self.link(pack(A, a2), b);
                        let a3 = self.alloc(&[pack(VAR, w), res]);
                        self.link(pack(VAR, u), pack(A, a3));
                    }
                    F => {
                        // triage: △(△w x) b c → T2(w,x,b,res) faces c
                        let w = self.nd(pb);
                        let x = self.nd(pb + 1);
                        let t = self.alloc(&[w, x, b, res]);
                        self.link(pack(T2, t), carg);
                    }
                    _ => {}
                }
            }
            // ── T2 (second-level dispatch on z of △(△w x) y z) ──
            T2 => {
                let w = self.nd(cb);
                let x = self.nd(cb + 1);
                let b = self.nd(cb + 2);
                let res = self.nd(cb + 3);
                match pt {
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), c);
                    }
                    L => {
                        self.link(res, w);
                        self.link(pack(EPS, 0), x);
                        self.link(pack(EPS, 0), b);
                    }
                    S => {
                        let u = self.nd(pb);
                        let a1 = self.alloc(&[u, res]);
                        self.link(pack(A, a1), x);
                        self.link(pack(EPS, 0), w);
                        self.link(pack(EPS, 0), b);
                    }
                    F => {
                        let u = self.nd(pb);
                        let v = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc(&[u, pack(VAR, t)]);
                        self.link(pack(A, a1), b);
                        let a2 = self.alloc(&[v, res]);
                        self.link(pack(VAR, t), pack(A, a2));
                        self.link(pack(EPS, 0), w);
                        self.link(pack(EPS, 0), x);
                    }
                    _ => {}
                }
            }
            // ── δ (duplicate): δˢ/δⁿ share constructor rules (children inherit `sp`);
            //    differ only on P — δˢ copies the suspension as syntax, δⁿ forces it once
            //    (via an A) and parks on the result wire (Theorem 6). ──
            DS | DN => {
                let dl = self.nd(cb);
                let dr = self.nd(cb + 1);
                let sp = ct;
                match pt {
                    L => {
                        self.link(dl, pack(L, 0));
                        self.link(dr, pack(L, 0));
                    }
                    S => {
                        let x = self.nd(pb);
                        let a = self.new_var();
                        let b = self.new_var();
                        let d = self.alloc(&[pack(VAR, a), pack(VAR, b)]);
                        self.link(pack(sp, d), x);
                        let s1 = self.alloc(&[pack(VAR, a)]);
                        let s2 = self.alloc(&[pack(VAR, b)]);
                        self.link(dl, pack(S, s1));
                        self.link(dr, pack(S, s2));
                    }
                    F => {
                        let l = self.nd(pb);
                        let r = self.nd(pb + 1);
                        let (ll, lr, rl, rr) =
                            (self.new_var(), self.new_var(), self.new_var(), self.new_var());
                        let da = self.alloc(&[pack(VAR, ll), pack(VAR, lr)]);
                        self.link(pack(sp, da), l);
                        let db = self.alloc(&[pack(VAR, rl), pack(VAR, rr)]);
                        self.link(pack(sp, db), r);
                        let f1 = self.alloc(&[pack(VAR, ll), pack(VAR, rl)]);
                        let f2 = self.alloc(&[pack(VAR, lr), pack(VAR, rr)]);
                        self.link(dl, pack(F, f1));
                        self.link(dr, pack(F, f2));
                    }
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        if sp == DN {
                            // δⁿ: demand-before-copy — force `f a` once, park on the wire.
                            self.net.dnp.fetch_add(1, Ordering::Relaxed);
                            let t = self.new_var();
                            let a1 = self.alloc(&[pa, pack(VAR, t)]);
                            self.link(pack(A, a1), pf);
                            self.link(pack(VAR, t), c);
                        } else {
                            // δˢ: structural copy of the unevaluated suspension.
                            let (fl, fr, al, ar) =
                                (self.new_var(), self.new_var(), self.new_var(), self.new_var());
                            let df = self.alloc(&[pack(VAR, fl), pack(VAR, fr)]);
                            self.link(pack(sp, df), pf);
                            let da = self.alloc(&[pack(VAR, al), pack(VAR, ar)]);
                            self.link(pack(sp, da), pa);
                            let p1 = self.alloc(&[pack(VAR, fl), pack(VAR, al)]);
                            let p2 = self.alloc(&[pack(VAR, fr), pack(VAR, ar)]);
                            self.link(dl, pack(P, p1));
                            self.link(dr, pack(P, p2));
                        }
                    }
                    _ => {}
                }
            }
            // ── ε (erase; recurse into children, P erased unevaluated) ──
            EPS => match pt {
                L => {}
                S => {
                    let x = self.nd(pb);
                    self.link(pack(EPS, 0), x);
                }
                F | P => {
                    let a = self.nd(pb);
                    let b = self.nd(pb + 1);
                    self.link(pack(EPS, 0), a);
                    self.link(pack(EPS, 0), b);
                }
                _ => {}
            },
            // ── N (recursive normalizer → full NF) ──
            N => {
                let res = self.nd(cb);
                match pt {
                    L => self.link(res, pack(L, 0)),
                    S => {
                        let x = self.nd(pb);
                        let vx = self.new_var();
                        let nx = self.alloc(&[pack(VAR, vx)]);
                        self.link(pack(N, nx), x);
                        let s2 = self.alloc(&[pack(VAR, vx)]);
                        self.link(res, pack(S, s2));
                    }
                    F => {
                        let l = self.nd(pb);
                        let r = self.nd(pb + 1);
                        let vl = self.new_var();
                        let vr = self.new_var();
                        let nl = self.alloc(&[pack(VAR, vl)]);
                        self.link(pack(N, nl), l);
                        let nr = self.alloc(&[pack(VAR, vr)]);
                        self.link(pack(N, nr), r);
                        let f2 = self.alloc(&[pack(VAR, vl), pack(VAR, vr)]);
                        self.link(res, pack(F, f2));
                    }
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), c);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        // Free-on-consume: both agents are spent — reclaim their cells, EXCEPT the
        // demand/park rules (T1/T2/N/δⁿ ⊗ P) reuse the consumer (it re-aimed at the wire).
        let reuse_c = pt == P && (ct == T1 || ct == T2 || ct == N || ct == DN);
        self.free_node(pb, arity(pt));
        if !reuse_c {
            self.free_node(cb, arity(ct));
        }
    }

    /// Follow a wire through resolved substitution cells to a non-var port.
    #[inline]
    fn resolve(&self, mut p: u32) -> u32 {
        while is_var(p) {
            let nx = self.net.vars[val(p)].load(Ordering::Acquire);
            if nx == NUL {
                return p;
            }
            p = nx;
        }
        p
    }

    /// Reduce `h` to full normal form by connecting an `N` normalizer and draining.
    fn full_nf(&mut self, h: u32, budget: &mut i64) -> Option<u32> {
        let v = self.new_var();
        let n = self.alloc(&[pack(VAR, v)]);
        self.link(pack(N, n), h);
        if !self.drain(budget) {
            return None;
        }
        Some(self.resolve(pack(VAR, v)))
    }

    /// Encode an already-normalized producer tree to ternary (iterative).
    fn emit(&self, root: u32, out: &mut Vec<u8>) {
        let mut stack = vec![root];
        while let Some(p) = stack.pop() {
            let p = self.resolve(p);
            match tag(p) {
                L => out.push(b'0'),
                S => {
                    out.push(b'1');
                    stack.push(self.nd(val(p) as u32));
                }
                F => {
                    out.push(b'2');
                    stack.push(self.nd(val(p) as u32 + 1)); // right popped after left
                    stack.push(self.nd(val(p) as u32));
                }
                _ => out.push(b'0'), // unreachable after full_nf; defensive
            }
        }
    }

    /// Structural NF equality (no hash-cons ⇒ demand-then-compare).
    fn equal(&mut self, a: u32, b: u32, budget: &mut i64) -> Option<bool> {
        let na = self.full_nf(a, budget)?;
        let nb = self.full_nf(b, budget)?;
        let mut stack = vec![(na, nb)];
        while let Some((x, y)) = stack.pop() {
            let x = self.resolve(x);
            let y = self.resolve(y);
            match (tag(x), tag(y)) {
                (L, L) => {}
                (S, S) => stack.push((self.nd(val(x) as u32), self.nd(val(y) as u32))),
                (F, F) => {
                    stack.push((self.nd(val(x) as u32), self.nd(val(y) as u32)));
                    stack.push((self.nd(val(x) as u32 + 1), self.nd(val(y) as u32 + 1)));
                }
                _ => return Some(false),
            }
        }
        Some(true)
    }

    // ── ternary codec (preorder: leaf "0", stem "1"+child, fork "2"+l+r) ──
    fn parse(&mut self, s: &[u8], i: &mut usize) -> u32 {
        enum Frame {
            Stem,
            ForkL,
            ForkR(u32),
        }
        let mut stack: Vec<Frame> = Vec::new();
        loop {
            let mut value = loop {
                if *i >= s.len() {
                    break self.leaf();
                }
                let ch = s[*i];
                *i += 1;
                match ch {
                    b'1' => stack.push(Frame::Stem),
                    b'2' => stack.push(Frame::ForkL),
                    _ => break self.leaf(),
                }
            };
            loop {
                match stack.pop() {
                    None => return value,
                    Some(Frame::Stem) => value = self.stem(value),
                    Some(Frame::ForkL) => {
                        stack.push(Frame::ForkR(value));
                        break;
                    }
                    Some(Frame::ForkR(left)) => value = self.fork(left, value),
                }
            }
        }
    }
}

// ── M2c: native parallel drain ──────────────────────────────────────────────
// A simple lock-free scheduler: every redex goes to a shared crossbeam `Injector`; N
// worker threads steal, interact, and flush their output back. Termination via an
// `in_flight` counter (incremented BEFORE a producer's redexes are pushed, decremented
// AFTER each is processed) — 0 means done. Not work-stealing-optimal (all contention on
// the injector; per-worker deques are a later perf knob), but CORRECT and genuinely
// parallel. Strong confluence (Theorem 2) means the NF AND the interaction COUNT are
// identical to sequential regardless of thread count — exactly the race-detector gate.
// Cross-thread publishing: a stealing worker sees the producer's node-cell writes via the
// injector push/steal (release/acquire) + the `vars` AcqRel; owned-only (the atomic
// `node_top` bump hands each worker a disjoint region) means no two workers race a cell.
#[cfg(not(target_arch = "wasm32"))]
impl Net {
    fn resolve_ro(&self, mut p: u32) -> u32 {
        while is_var(p) {
            let nx = self.vars[val(p)].load(Ordering::Acquire);
            if nx == NUL {
                return p;
            }
            p = nx;
        }
        p
    }
    /// Reduce `h` to full NF with `threads` workers sharing this net. NF root, or None on
    /// budget exhaustion.
    fn full_nf_parallel(&self, h: u32, threads: usize, budget: i64) -> Option<u32> {
        use crossbeam_deque::{Injector, Steal};
        use std::sync::atomic::{AtomicBool, AtomicI64, AtomicUsize};
        let mut setup = Worker::new();
        let v = {
            let mut c = self.ctx(&mut setup);
            let v = c.new_var();
            let n = c.alloc(&[pack(VAR, v)]);
            c.link(pack(N, n), h);
            v
        };
        let injector = Injector::new();
        let mut seeded = 0usize;
        for r in setup.bag.drain(..) {
            injector.push(r);
            seeded += 1;
        }
        if seeded == 0 {
            return Some(self.resolve_ro(pack(VAR, v)));
        }
        let in_flight = AtomicUsize::new(seeded);
        let budget = AtomicI64::new(budget);
        let exhausted = AtomicBool::new(false);
        let injector = &injector;
        let in_flight = &in_flight;
        let budget = &budget;
        let exhausted = &exhausted;
        std::thread::scope(|s| {
            for _ in 0..threads {
                s.spawn(move || {
                    let mut w = Worker::new();
                    let backoff = crossbeam_utils::Backoff::new();
                    loop {
                        let stolen = loop {
                            match injector.steal() {
                                Steal::Success(r) => break Some(r),
                                Steal::Empty => break None,
                                Steal::Retry => {}
                            }
                        };
                        match stolen {
                            Some((x, y)) => {
                                backoff.reset();
                                if budget.fetch_sub(1, Ordering::Relaxed) <= 0 {
                                    exhausted.store(true, Ordering::Relaxed);
                                    in_flight.fetch_sub(1, Ordering::AcqRel);
                                    continue;
                                }
                                self.interactions.fetch_add(1, Ordering::Relaxed);
                                {
                                    let mut c = self.ctx(&mut w);
                                    c.interact(x, y);
                                }
                                let produced: Vec<(u32, u32)> = w.bag.drain(..).collect();
                                in_flight.fetch_add(produced.len(), Ordering::AcqRel);
                                for nr in produced {
                                    injector.push(nr);
                                }
                                in_flight.fetch_sub(1, Ordering::AcqRel);
                            }
                            None => {
                                if in_flight.load(Ordering::Acquire) == 0 {
                                    break;
                                }
                                backoff.snooze();
                            }
                        }
                    }
                });
            }
        });
        if exhausted.load(Ordering::Relaxed) {
            return None;
        }
        Some(self.resolve_ro(pack(VAR, v)))
    }
}

// One Net + one Worker per WASM instance (the host calls on a single thread). The Net is
// interior-mutable (atomics), so a plain `&` suffices; the Worker is RefCell (briefly
// borrowed per op — never nested, so no double-borrow).
thread_local! {
    static NET: Net = Net::new(NODE_CAP, VAR_CAP);
    static WK: RefCell<Worker> = RefCell::new(Worker::new());
}
#[inline]
fn with<T>(f: impl FnOnce(&mut Ctx) -> T) -> T {
    NET.with(|net| {
        WK.with(|wk| {
            let mut w = wk.borrow_mut();
            let mut ctx = net.ctx(&mut w);
            f(&mut ctx)
        })
    })
}
#[inline]
fn pack_ptr(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

// ── term algebra (Session.leaf / stem / fork) ───────────────────────────────
#[no_mangle]
pub extern "C" fn tc_leaf() -> u32 {
    pack(L, 0)
}
#[no_mangle]
pub extern "C" fn tc_stem(child: u32) -> u32 {
    with(|c| c.stem(child))
}
#[no_mangle]
pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 {
    with(|c| c.fork(left, right))
}
/// **LAZY** apply: build `P(f,x)` in O(1); reduction is deferred to dump/equal.
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, _budget: u32) -> u32 {
    with(|c| c.susp(f, x))
}

// ── bulk / interchange (Session.loadTernary / dumpTernary) ──────────────────
#[no_mangle]
pub extern "C" fn tc_alloc(len: u32) -> u32 {
    let mut buf = vec![0u8; len as usize];
    let ptr = buf.as_mut_ptr() as u32;
    std::mem::forget(buf);
    ptr
}
#[no_mangle]
pub unsafe extern "C" fn tc_free(ptr: u32, len: u32) {
    if ptr != 0 {
        drop(Vec::from_raw_parts(ptr as *mut u8, len as usize, len as usize));
    }
}
#[no_mangle]
pub unsafe extern "C" fn tc_load_ternary(ptr: u32, len: u32) -> u32 {
    let bytes = std::slice::from_raw_parts(ptr as *const u8, len as usize);
    with(|c| {
        let mut i = 0usize;
        c.parse(bytes, &mut i)
    })
}
/// Force full NF, serialize to ternary, return `(ptr<<32)|len`; `(u32::MAX<<32)` on exhaust.
#[no_mangle]
pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 {
    with(|c| {
        let mut b = budget as i64;
        match c.full_nf(h, &mut b) {
            None => pack_ptr(u32::MAX, 0),
            Some(nf) => {
                let mut out: Vec<u8> = Vec::new();
                c.emit(nf, &mut out);
                let len = out.len() as u32;
                let ptr = out.as_ptr() as u32;
                std::mem::forget(out);
                pack_ptr(ptr, len)
            }
        }
    })
}

// ── observations ────────────────────────────────────────────────────────────
/// NF equality: `0`=false `1`=true `2`=budget exhausted.
#[no_mangle]
pub extern "C" fn tc_equal(a: u32, b: u32, budget: u32) -> u32 {
    with(|c| {
        let mut bud = budget as i64;
        match c.equal(a, b, &mut bud) {
            Some(true) => 1,
            Some(false) => 0,
            None => 2,
        }
    })
}
/// Total interactions consumed this session.
#[no_mangle]
pub extern "C" fn tc_interactions() -> u64 {
    NET.with(|n| n.interactions.load(Ordering::Relaxed))
}
/// Peak node cells used (the bump high-water — materialized-net proof + footprint).
#[no_mangle]
pub extern "C" fn tc_nodes() -> u64 {
    NET.with(|n| n.node_top.load(Ordering::Relaxed) as u64)
}
/// δⁿ ⊗ P firings = how many duplicated suspensions were shared.
#[no_mangle]
pub extern "C" fn tc_dnp() -> u64 {
    NET.with(|n| n.dnp.load(Ordering::Relaxed))
}

#[cfg(test)]
mod tests {
    use super::*;

    // A net + worker for a test, small fixed arena.
    fn net() -> Net {
        Net::new(1 << 16, 1 << 16)
    }
    fn build_not(c: &mut Ctx) -> u32 {
        let sl = c.stem(pack(L, 0)); // S(L) = true
        let fll = c.fork(pack(L, 0), pack(L, 0)); // F(L,L)
        let inner = c.fork(sl, fll); // F(S(L), F(L,L))
        c.fork(inner, pack(L, 0)) // F(.., L) = not
    }
    fn nf_string(c: &mut Ctx, h: u32) -> String {
        let mut b = 100_000_000i64;
        let nf = c.full_nf(h, &mut b).expect("nf");
        let mut out = Vec::new();
        c.emit(nf, &mut out);
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn not_false_is_true() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let not = build_not(&mut c);
        let app = c.susp(not, pack(L, 0));
        assert_eq!(nf_string(&mut c, app), "10"); // S(L) = true
    }

    #[test]
    fn not_true_is_false() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let not = build_not(&mut c);
        let t = c.stem(pack(L, 0));
        let app = c.susp(not, t);
        assert_eq!(nf_string(&mut c, app), "0"); // L = false
    }

    #[test]
    fn k_discards_second() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let k = c.stem(pack(L, 0));
        let x = c.fork(pack(L, 0), pack(L, 0));
        let y = c.stem(pack(L, 0));
        let kx = c.susp(k, x);
        let kxy = c.susp(kx, y);
        assert_eq!(nf_string(&mut c, kxy), "200"); // F(L,L)
    }

    #[test]
    fn ternary_roundtrip_via_parse() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let s = b"210200";
        let mut i = 0usize;
        let h = c.parse(s, &mut i);
        assert_eq!(i, s.len());
        let mut b = 1_000_000i64;
        let nf = c.full_nf(h, &mut b).unwrap();
        let mut out = Vec::new();
        c.emit(nf, &mut out);
        assert_eq!(&out, s);
    }

    // S-rule (T1 ⊗ S → δⁿ): △(△L) L (Stem(L)) → "210110".
    #[test]
    fn s_rule_dup() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let op = {
            let sl = c.stem(pack(L, 0));
            c.fork(sl, pack(L, 0))
        };
        let arg = c.stem(pack(L, 0));
        let app = c.susp(op, arg);
        assert_eq!(nf_string(&mut c, app), "210110");
    }

    // δⁿ ⊗ P (parking): duplicate a SUSPENSION (not false); δⁿ⊗P must fire (dnp ≥ 1).
    #[test]
    fn dn_parks_on_suspension() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let not = build_not(&mut c);
        let susp = c.susp(not, pack(L, 0)); // (not false) reduces to true
        let op = {
            let sl = c.stem(pack(L, 0));
            c.fork(sl, pack(L, 0))
        };
        let app = c.susp(op, susp);
        assert_eq!(nf_string(&mut c, app), "210110");
        assert!(n.dnp.load(Ordering::Relaxed) >= 1, "δⁿ⊗P should fire");
    }

    // Theorem-6 work-sharing: duplicate one expensive suspension; δⁿ < δˢ interactions.
    fn dup_cost(species: u32, k: usize) -> u64 {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let mut e = pack(L, 0);
        for _ in 0..k {
            let not = build_not(&mut c); // FRESH not per level — a linear chain
            e = c.susp(not, e);
        }
        let (lv, rv) = (c.new_var(), c.new_var());
        let d = c.alloc(&[pack(VAR, lv), pack(VAR, rv)]);
        c.link(pack(species, d), e);
        for v in [lv, rv] {
            let res = c.new_var();
            let nn = c.alloc(&[pack(VAR, res)]);
            c.link(pack(N, nn), pack(VAR, v));
        }
        let mut b = 1_000_000_000i64;
        c.drain(&mut b);
        n.interactions.load(Ordering::Relaxed)
    }

    #[test]
    fn dn_shares_what_ds_redoes() {
        let dn = dup_cost(DN, 12);
        let ds = dup_cost(DS, 12);
        // (Measured k=12: δⁿ≈111 vs δˢ≈327 interactions, ~2.95×.)
        assert!(dn < ds, "δⁿ ({dn}) should do strictly less work than δˢ ({ds})");
    }

    #[test]
    fn equal_nf() {
        let n = net();
        let mut w = Worker::new();
        let mut c = n.ctx(&mut w);
        let not = build_not(&mut c);
        let nf_app = c.susp(not, pack(L, 0)); // → true
        let tru = c.stem(pack(L, 0));
        let mut b = 1_000_000i64;
        assert_eq!(c.equal(nf_app, tru, &mut b), Some(true));
        let mut b2 = 1_000_000i64;
        assert_eq!(c.equal(nf_app, pack(L, 0), &mut b2), Some(false));
    }

    // M2c race detector: a balanced fork tree of independent not^4-chains has many
    // independent N redexes; reducing with 1 vs 4 threads must give the SAME NF and the
    // SAME interaction count (strong confluence). Run many times — concurrency bugs flake.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn parallel_matches_sequential() {
        fn build(c: &mut Ctx, depth: usize) -> u32 {
            if depth == 0 {
                let mut e = pack(L, 0); // false
                for _ in 0..4 {
                    let not = build_not(c);
                    e = c.susp(not, e); // not^4 false
                }
                e
            } else {
                let l = build(c, depth - 1);
                let r = build(c, depth - 1);
                c.fork(l, r)
            }
        }
        for _ in 0..20 {
            let nseq = Net::new(1 << 20, 1 << 20);
            let (seq_nf, seq_int) = {
                let mut w = Worker::new();
                let mut c = nseq.ctx(&mut w);
                let h = build(&mut c, 6);
                let mut b = 1_000_000_000i64;
                let nf = c.full_nf(h, &mut b).expect("seq nf");
                let mut o = Vec::new();
                c.emit(nf, &mut o);
                (o, nseq.interactions.load(Ordering::Relaxed))
            };
            let npar = Net::new(1 << 20, 1 << 20);
            let h = {
                let mut w = Worker::new();
                let mut c = npar.ctx(&mut w);
                build(&mut c, 6)
            };
            let nf = npar.full_nf_parallel(h, 4, 1_000_000_000).expect("par nf");
            let par_nf = {
                let mut w = Worker::new();
                let c = npar.ctx(&mut w);
                let mut o = Vec::new();
                c.emit(nf, &mut o);
                o
            };
            let par_int = npar.interactions.load(Ordering::Relaxed);
            assert_eq!(seq_nf, par_nf, "parallel NF must equal sequential");
            assert_eq!(seq_int, par_int, "interaction count deterministic across threads");
        }
    }

}
