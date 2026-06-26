//! rust-ic-net — a MATERIALIZED interaction-net evaluator for disp tree calculus.
//!
//! **M0 + M1 (sequential).** Unlike `rust-eager` (where the net collapses to a hash-consed
//! tree reducer — consumers are control flow, δˢ is interning), here the agents and
//! ports are REAL arena nodes and active pairs sit in a schedulable redex bag. This is
//! the substrate M2 parallelizes; M0/M1 validate the rules + scheduler single-threaded,
//! gated differentially against `rust-eager` (strong confluence ⇒ identical NFs).
//! Design: research/interaction-combinator/RUST_IC_NET_DESIGN.md; calculus: tc-net.typ.
//! - **M0:** the full rule kernel with δˢ (structural copy). Correct, but re-reduces
//!   shared subterms (Prop 5), so sharing-heavy programs allocate exponentially.
//! - **M1:** the S-rule spawns δⁿ (demand-before-copy) — a duplicated SUSPENSION's work
//!   runs at most once (Theorem 6; validated by `dn_shares_what_ds_redoes`). δⁿ shares
//!   re-*reduction*, NOT structure (identical constructors are a hash-consing matter, not
//!   δⁿ's — so it's a no-op where the duplicated value is already a constructor).
//! - **M1b (GC):** free-on-consume — an interaction consumes its two agents, so their
//!   cells return to per-arity free lists for reuse (and resolved wire cells too). Peak
//!   `nodes.len()` then tracks the LIVE working set, not cumulative allocs (measured:
//!   fib(8) 7.6M→64K nodes, ~119×). This is COMPLETE for the eager net because it is
//!   LINEAR — every agent is consumed exactly once. **Requirement:** terms must be linear
//!   (each node referenced once); sharing in a net goes through δ, NOT host handle reuse —
//!   `loadTernary`/the fold provide this. Refcounting for the lazy parked-δⁿ leak (which
//!   eager M1 doesn't have — the forcing `A` always fires) + atomics stay M2.
//!
//! Representation (option (d), IVM/Vine-style, 32-bit ports for M0 so a handle = a port):
//! - A `Port` is a `u32`: low 4 bits tag, high 28 bits value (node base index, or var
//!   index). The PRINCIPAL port is implicit (it's whoever references the node), so a
//!   node stores only its auxiliary ports — kind lives in the referencing port's tag.
//! - Producers `L S F P` (≤2 aux); consumers `A T1 T2 Ds Eps N`. Nullary `L`/`Eps` need
//!   no allocation (`pack(L,0)`/`pack(EPS,0)`). δⁿ + reachability GC + atomics are M1/M2.
//! - The linker is exchange-based on a separate `vars` substitution buffer (no CAS).
//! - M0 GC = grow-until-dispose: `alloc` only bumps, nothing is freed (the per-call
//!   Session arena is dropped wholesale). δⁿ/free-lists/atomics arrive at M1/M2.
#![allow(clippy::missing_safety_doc, dead_code)]

use std::cell::RefCell;
use std::sync::atomic::{AtomicU32, Ordering};

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

/// Budget exhaustion sentinel, threaded as a bool out of `drain`.
struct Net {
    /// Flat cell store: a node at base `b` of arity `k` owns `nodes[b .. b+k]` (its
    /// auxiliary ports). Append-only in M0 (grow-until-dispose).
    nodes: Vec<u32>,
    /// Substitution cells — the exchange linker's rendezvous and the SOLE cross-thread
    /// shared state (node cells are owned by the firing worker). `vars[v]` = a parked Port
    /// or `NUL`; a wire end is `pack(VAR, v)`. Atomic (`swap`, AcqRel) so the linker is
    /// lock-free under M2 parallelism; single-threaded (M2a) the ops degrade to plain.
    vars: Vec<AtomicU32>,
    /// Active pairs (principal ⊗ principal), the schedulable work bag (a LIFO stack for
    /// M0; M2 makes it a per-worker work-stealing deque).
    redexes: Vec<(u32, u32)>,
    /// Reduction counter = interactions fired (the budget unit; reported via stats).
    interactions: u64,
    /// δⁿ ⊗ P firings — how often a duplicated SUSPENSION was shared (vs reduced-then-copied).
    /// Zero means the eval order reduced every duplicated value before the S-rule saw it,
    /// so δⁿ degenerates to δˢ (the win needs demand-driven scheduling — see the M1 note).
    dnp: u64,
    /// Free lists of reusable node bases, indexed by arity (1..=4; 0 unused). Linear
    /// reclamation: an interaction consumes its two agents, so their cells return here for
    /// reuse — peak `nodes.len()` then tracks the LIVE working set, not cumulative allocs.
    free: [Vec<u32>; 5],
    /// Free list of reusable substitution cells (a wire freed when both ends resolve).
    free_vars: Vec<u32>,
}

impl Net {
    fn new() -> Self {
        Net {
            nodes: Vec::with_capacity(1 << 16),
            vars: Vec::with_capacity(1 << 16),
            redexes: Vec::new(),
            interactions: 0,
            dnp: 0,
            free: Default::default(),
            free_vars: Vec::new(),
        }
    }

    /// Allocate a node with the given auxiliary ports; returns its base cell index.
    /// Reuses a freed same-arity node when available (free-on-consume), else bumps.
    #[inline]
    fn alloc(&mut self, aux: &[u32]) -> u32 {
        let size = aux.len();
        if let Some(base) = self.free[size].pop() {
            self.nodes[base as usize..base as usize + size].copy_from_slice(aux);
            return base;
        }
        let base = self.nodes.len() as u32;
        self.nodes.extend_from_slice(aux);
        base
    }
    /// Return a consumed node's cells to the free list for reuse (`ar` = its arity).
    #[inline]
    fn free_node(&mut self, base: u32, ar: usize) {
        if ar > 0 {
            self.free[ar].push(base);
        }
    }
    /// A fresh substitution cell (wire); reuses a freed cell when available.
    #[inline]
    fn new_var(&mut self) -> u32 {
        if let Some(v) = self.free_vars.pop() {
            self.vars[v as usize].store(NUL, Ordering::Relaxed);
            return v;
        }
        let v = self.vars.len() as u32;
        self.vars.push(AtomicU32::new(NUL));
        v
    }

    // term builders (Session.leaf/stem/fork + parse) — produce a producer Port.
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

    /// The universal connect. Exchange-based: when an end is a `Var`, swap our partner
    /// into its cell; if the far end already deposited a partner, the second arrival
    /// "wins" and we link the two partners (chaining through resolved vars). When both
    /// ends are principals, they form an active pair → the redex bag. Iterative (the
    /// chain resolves without native recursion).
    fn link(&mut self, mut a: u32, mut b: u32) {
        loop {
            if !is_var(a) && !is_var(b) {
                self.redexes.push((a, b));
                return;
            }
            if !is_var(a) {
                std::mem::swap(&mut a, &mut b);
            }
            // a is a Var: the lock-free rendezvous. Atomically deposit our partner `b` and
            // read what was there. NUL ⇒ we parked b (the far end will find it on its own
            // swap). Otherwise the far end already deposited `prev`; we won, the var is now
            // spent (freed; AcqRel publishes our subgraph / acquires theirs), and we link
            // the two partners — chaining if `prev` is itself a var.
            let v = val(a);
            let prev = self.vars[v].swap(b, Ordering::AcqRel);
            if prev == NUL {
                return;
            }
            self.free_vars.push(v as u32);
            a = prev;
            // loop with (prev, b)
        }
    }

    /// Drive the redex bag to quiescence (full reduction of all demanded work) or until
    /// the budget is exhausted. Returns false on exhaustion.
    fn drain(&mut self, budget: &mut i64) -> bool {
        while let Some((x, y)) = self.redexes.pop() {
            if *budget <= 0 {
                return false;
            }
            *budget -= 1;
            self.interactions += 1;
            self.interact(x, y);
        }
        true
    }

    /// One interaction. Every well-formed active pair is consumer ⊗ producer; we orient
    /// to `(c = consumer, p = producer)` and apply the rule (tc-net.typ §Demand/Dispatch/
    /// Sharing). The S-rule (`T1 ⊗ S`) is the sole δ-spawner.
    fn interact(&mut self, x: u32, y: u32) {
        // Orient: producer in `p`, the other in `c`.
        let (c, p) = if is_producer(tag(x)) { (y, x) } else { (x, y) };
        let ct = tag(c);
        let pt = tag(p);
        if !is_producer(pt) {
            // consumer ⊗ consumer — shouldn't arise in well-formed tree-calc reduction
            // (every value/wire has one producer end). Eps ⊗ Eps vanishes; else ignore.
            return;
        }
        let cb = val(c) as u32;
        let pb = val(p) as u32;
        match ct {
            // ── A (apply) ──────────────────────────────────────────────────────
            A => {
                let arg = self.nodes[cb as usize];
                let res = self.nodes[cb as usize + 1];
                match pt {
                    L => {
                        // A ⊗ L: build stem △arg
                        let s = self.stem(arg);
                        self.link(res, s);
                    }
                    S => {
                        // A ⊗ S(x): build fork △x arg
                        let x = self.nodes[pb as usize];
                        let f = self.fork(x, arg);
                        self.link(res, f);
                    }
                    F => {
                        // A ⊗ F(l,r): enter first-level dispatch, T1(b=r,c=arg,res) faces l
                        let l = self.nodes[pb as usize];
                        let r = self.nodes[pb as usize + 1];
                        let t = self.alloc(&[r, arg, res]);
                        self.link(pack(T1, t), l);
                    }
                    P => {
                        // A ⊗ P(f,a): associate — demand (f a) first, then apply arg
                        let pf = self.nodes[pb as usize];
                        let pa = self.nodes[pb as usize + 1];
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        let a2 = self.alloc(&[arg, res]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), pack(A, a2));
                    }
                    _ => {}
                }
            }
            // ── T1 (first-level dispatch on a of △ a b c) ──────────────────────
            T1 => {
                let b = self.nodes[cb as usize];
                let carg = self.nodes[cb as usize + 1];
                let res = self.nodes[cb as usize + 2];
                match pt {
                    P => {
                        // demand the discriminator; T1 re-aims at the result wire
                        let pf = self.nodes[pb as usize];
                        let pa = self.nodes[pb as usize + 1];
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
                        // S rule: △(△x) b c → (x c)(b c). The SOLE δ site — it spawns δⁿ
                        // (need-sharing) so the duplicated argument `c` is reduced at most
                        // once across both uses (Theorem 6). δˢ is reserved for an explicit
                        // reflective syntax-copy (no such site yet).
                        let x = self.nodes[pb as usize];
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
                        let w = self.nodes[pb as usize];
                        let x = self.nodes[pb as usize + 1];
                        let t = self.alloc(&[w, x, b, res]);
                        self.link(pack(T2, t), carg);
                    }
                    _ => {}
                }
            }
            // ── T2 (second-level dispatch on z of △(△w x) y z) ─────────────────
            T2 => {
                let w = self.nodes[cb as usize];
                let x = self.nodes[cb as usize + 1];
                let b = self.nodes[cb as usize + 2];
                let res = self.nodes[cb as usize + 3];
                match pt {
                    P => {
                        let pf = self.nodes[pb as usize];
                        let pa = self.nodes[pb as usize + 1];
                        let t = self.new_var();
                        let a1 = self.alloc(&[pa, pack(VAR, t)]);
                        self.link(pack(A, a1), pf);
                        self.link(pack(VAR, t), c);
                    }
                    L => {
                        // triage leaf → w ; erase x, b
                        self.link(res, w);
                        self.link(pack(EPS, 0), x);
                        self.link(pack(EPS, 0), b);
                    }
                    S => {
                        // triage stem → x u ; erase w, b
                        let u = self.nodes[pb as usize];
                        let a1 = self.alloc(&[u, res]);
                        self.link(pack(A, a1), x);
                        self.link(pack(EPS, 0), w);
                        self.link(pack(EPS, 0), b);
                    }
                    F => {
                        // triage fork → (b u) v ; erase w, x
                        let u = self.nodes[pb as usize];
                        let v = self.nodes[pb as usize + 1];
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
            // ── δ (duplicate). δˢ and δⁿ share the constructor rules — copy one
            //    WHNF constructor, children inherit the SPECIES (`sp`), so the copy
            //    wave propagates. They differ ONLY on `P`: δˢ copies the suspension as
            //    syntax (redoes its work per copy, tc-net.typ Prop 5); δⁿ DEMANDS it
            //    (an `A` forces `f a` once) and PARKS itself on the result wire, so the
            //    shared computation runs at most once (Theorem 6, demand-before-copy).
            DS | DN => {
                let dl = self.nodes[cb as usize];
                let dr = self.nodes[cb as usize + 1];
                let sp = ct; // children inherit this duplicator's species
                match pt {
                    L => {
                        self.link(dl, pack(L, 0));
                        self.link(dr, pack(L, 0));
                    }
                    S => {
                        let x = self.nodes[pb as usize];
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
                        let l = self.nodes[pb as usize];
                        let r = self.nodes[pb as usize + 1];
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
                        let pf = self.nodes[pb as usize];
                        let pa = self.nodes[pb as usize + 1];
                        if sp == DN {
                            // δⁿ: demand-before-copy — force `f a` once (via an A), then
                            // park this δⁿ on the result wire (its l/r preserved). When a
                            // constructor arrives the δⁿ ⊗ ctor rule copies ONE level,
                            // children re-parked behind fresh δⁿ. Shared work runs once.
                            self.dnp += 1;
                            let t = self.new_var();
                            let a1 = self.alloc(&[pa, pack(VAR, t)]);
                            self.link(pack(A, a1), pf);
                            self.link(pack(VAR, t), c);
                        } else {
                            // δˢ: structural copy of the unevaluated suspension
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
            // ── ε (erase; recurse into children, P erased unevaluated) ─────────
            EPS => match pt {
                L => {}
                S => {
                    let x = self.nodes[pb as usize];
                    self.link(pack(EPS, 0), x);
                }
                F | P => {
                    let a = self.nodes[pb as usize];
                    let b = self.nodes[pb as usize + 1];
                    self.link(pack(EPS, 0), a);
                    self.link(pack(EPS, 0), b);
                }
                _ => {}
            },
            // ── N (recursive normalizer → full NF; tc-net.typ §Full Normalization) ──
            N => {
                let res = self.nodes[cb as usize];
                match pt {
                    L => self.link(res, pack(L, 0)),
                    S => {
                        let x = self.nodes[pb as usize];
                        let vx = self.new_var();
                        let nx = self.alloc(&[pack(VAR, vx)]);
                        self.link(pack(N, nx), x);
                        let s2 = self.alloc(&[pack(VAR, vx)]);
                        self.link(res, pack(S, s2));
                    }
                    F => {
                        let l = self.nodes[pb as usize];
                        let r = self.nodes[pb as usize + 1];
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
                        // demand (f a), then keep normalizing the result
                        let pf = self.nodes[pb as usize];
                        let pa = self.nodes[pb as usize + 1];
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
        // Linear reclamation (free-on-consume): both agents of the active pair are spent,
        // so return their cells to the free list for reuse — EXCEPT the demand/park rules
        // (T1/T2/N/δⁿ ⊗ P) REUSE the consumer (it re-aimed at the result wire), so it must
        // stay live (it is freed later, when it fires against the forced constructor).
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
            let nx = self.vars[val(p)].load(Ordering::Acquire);
            if nx == NUL {
                return p;
            }
            p = nx;
        }
        p
    }

    /// Reduce `h` to full normal form (a Susp-free producer tree) by connecting an `N`
    /// normalizer and draining. Returns the NF root Port, or `None` on budget exhaustion.
    fn full_nf(&mut self, h: u32, budget: &mut i64) -> Option<u32> {
        let v = self.new_var();
        let n = self.alloc(&[pack(VAR, v)]);
        self.link(pack(N, n), h);
        if !self.drain(budget) {
            return None;
        }
        Some(self.resolve(pack(VAR, v)))
    }

    /// Encode an already-normalized producer tree to ternary (iterative; deep results
    /// don't recurse). `root` must be Susp-free (call `full_nf` first).
    fn emit(&self, root: u32, out: &mut Vec<u8>) {
        let mut stack = vec![root];
        while let Some(p) = stack.pop() {
            let p = self.resolve(p);
            match tag(p) {
                L => out.push(b'0'),
                S => {
                    out.push(b'1');
                    stack.push(self.nodes[val(p)]);
                }
                F => {
                    out.push(b'2');
                    stack.push(self.nodes[val(p) + 1]); // right popped after left
                    stack.push(self.nodes[val(p)]);
                }
                _ => out.push(b'0'), // unreachable after full_nf; defensive
            }
        }
    }

    /// Structural NF equality (no hash-cons ⇒ demand-then-compare). Iterative.
    fn equal(&mut self, a: u32, b: u32, budget: &mut i64) -> Option<bool> {
        let na = self.full_nf(a, budget)?;
        let nb = self.full_nf(b, budget)?;
        let mut stack = vec![(na, nb)];
        while let Some((x, y)) = stack.pop() {
            let x = self.resolve(x);
            let y = self.resolve(y);
            match (tag(x), tag(y)) {
                (L, L) => {}
                (S, S) => stack.push((self.nodes[val(x)], self.nodes[val(y)])),
                (F, F) => {
                    stack.push((self.nodes[val(x)], self.nodes[val(y)]));
                    stack.push((self.nodes[val(x) + 1], self.nodes[val(y) + 1]));
                }
                _ => return Some(false),
            }
        }
        Some(true)
    }

    // ── ternary codec (preorder: leaf "0", stem "1"+child, fork "2"+l+r) ──
    fn parse(&mut self, s: &[u8], i: &mut usize) -> u32 {
        // iterative preorder build (no native recursion on input depth)
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

thread_local! {
    static NET: RefCell<Net> = RefCell::new(Net::new());
}
#[inline]
fn with<T>(f: impl FnOnce(&mut Net) -> T) -> T {
    NET.with(|n| f(&mut n.borrow_mut()))
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
    with(|n| n.stem(child))
}
#[no_mangle]
pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 {
    with(|n| n.fork(left, right))
}

/// **LAZY** apply: build the suspended application `P(f,x)` in O(1); reduction is
/// deferred to the forcing observations (`dumpTernary`/`equal`). Budget unused.
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, _budget: u32) -> u32 {
    with(|n| n.susp(f, x))
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
    with(|n| {
        let mut i = 0usize;
        n.parse(bytes, &mut i)
    })
}
/// Force full NF, serialize to ternary, return `(ptr<<32)|len`. On exhaustion returns
/// `(u32::MAX<<32)` (the host checks the ptr sentinel).
#[no_mangle]
pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 {
    with(|n| {
        let mut b = budget as i64;
        match n.full_nf(h, &mut b) {
            None => pack_ptr(u32::MAX, 0),
            Some(nf) => {
                let mut out: Vec<u8> = Vec::new();
                n.emit(nf, &mut out);
                let len = out.len() as u32;
                let ptr = out.as_ptr() as u32;
                std::mem::forget(out);
                pack_ptr(ptr, len)
            }
        }
    })
}

// ── observations ────────────────────────────────────────────────────────────
/// NF equality: `0`=false `1`=true `2`=budget exhausted (host checks `> 1`).
#[no_mangle]
pub extern "C" fn tc_equal(a: u32, b: u32, budget: u32) -> u32 {
    with(|n| {
        let mut bud = budget as i64;
        match n.equal(a, b, &mut bud) {
            Some(true) => 1,
            Some(false) => 0,
            None => 2,
        }
    })
}
/// Total interactions consumed this session (the backend-declared budget unit).
#[no_mangle]
pub extern "C" fn tc_interactions() -> u64 {
    with(|n| n.interactions)
}
/// Live node-cell count (proof the net is materialized, not a recompiled tree reducer).
#[no_mangle]
pub extern "C" fn tc_nodes() -> u64 {
    with(|n| n.nodes.len() as u64)
}
/// δⁿ ⊗ P firings = how many duplicated suspensions were shared (vs reduced-then-copied).
#[no_mangle]
pub extern "C" fn tc_dnp() -> u64 {
    with(|n| n.dnp)
}

#[cfg(test)]
mod tests {
    use super::*;

    // tc-net.typ §Worked Example: false = △ = L; true = △△ = S(L);
    //   not = △ (△ (△△) (△△△)) △ = F(F(S(L), F(L,L)), L)
    fn build_not(n: &mut Net) -> u32 {
        let sl = n.stem(pack(L, 0)); // S(L) = true
        let fll = n.fork(pack(L, 0), pack(L, 0)); // F(L,L)
        let inner = n.fork(sl, fll); // F(S(L), F(L,L))
        n.fork(inner, pack(L, 0)) // F(.., L) = not
    }

    fn nf_string(n: &mut Net, h: u32) -> String {
        let mut b = 100_000_000i64;
        let nf = n.full_nf(h, &mut b).expect("nf");
        let mut out = Vec::new();
        n.emit(nf, &mut out);
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn not_false_is_true() {
        let mut n = Net::new();
        let not = build_not(&mut n);
        let app = n.susp(not, pack(L, 0)); // not false
        assert_eq!(nf_string(&mut n, app), "10"); // S(L) = true
    }

    #[test]
    fn not_true_is_false() {
        let mut n = Net::new();
        let not = build_not(&mut n);
        let t = n.stem(pack(L, 0));
        let app = n.susp(not, t); // not true
        assert_eq!(nf_string(&mut n, app), "0"); // L = false
    }

    // K x y = x ;  K = △△ = S(L)
    #[test]
    fn k_discards_second() {
        let mut n = Net::new();
        let k = n.stem(pack(L, 0)); // K
        let x = n.fork(pack(L, 0), pack(L, 0)); // some value F(L,L)
        let y = n.stem(pack(L, 0));
        let kx = n.susp(k, x);
        let kxy = n.susp(kx, y); // (K x) y → x
        assert_eq!(nf_string(&mut n, kxy), "200"); // F(L,L)
    }

    // S K K x = x  (identity);  S = △(△(△△△))... build via combinators is verbose,
    // so test the dispatch directly: △(△△) drops to the S-rule.
    #[test]
    fn ternary_roundtrip_via_parse() {
        let mut n = Net::new();
        let s = b"210200"; // F(S(L), F(L,L))
        let mut i = 0usize;
        let h = n.parse(s, &mut i);
        assert_eq!(i, s.len());
        // already NF (no P); dump must round-trip
        let mut b = 1_000_000i64;
        let nf = n.full_nf(h, &mut b).unwrap();
        let mut out = Vec::new();
        n.emit(nf, &mut out);
        assert_eq!(&out, s);
    }

    // Exercises the S-rule (T1 ⊗ S → δⁿ): △(△a) b c → (a c)(b c).
    //   operator = Fork(Stem(L), L) = △(△△)△ ; arg c = Stem(L) = △△
    //   → (L·c)(L·c) = Stem(Stem(L)) applied to itself = Fork(Stem(L), Stem(Stem(L)))
    //   ternary "2" + "10" + "110" = "210110".
    #[test]
    fn s_rule_dup() {
        let mut n = Net::new();
        let op = {
            let sl = n.stem(pack(L, 0));
            n.fork(sl, pack(L, 0))
        };
        let c = n.stem(pack(L, 0));
        let app = n.susp(op, c);
        assert_eq!(nf_string(&mut n, app), "210110");
    }

    // δⁿ ⊗ P (parking): duplicate a SUSPENSION. △(△L) L (not false) → (L c)(L c) where
    // c = `not false` is an unreduced P. δⁿ must force c ONCE (dnp ≥ 1) and share it;
    // c → true = S(L), so (L·true)(L·true) = Fork(S(L), S(S(L))) = "210110".
    #[test]
    fn dn_parks_on_suspension() {
        let mut n = Net::new();
        let not = build_not(&mut n);
        let c = n.susp(not, pack(L, 0)); // (not false) — a suspension reducing to true
        let op = {
            let sl = n.stem(pack(L, 0));
            n.fork(sl, pack(L, 0))
        }; // Fork(Stem(L), L) = △(△L)·
        let app = n.susp(op, c);
        assert_eq!(nf_string(&mut n, app), "210110");
        assert!(n.dnp >= 1, "δⁿ⊗P should fire when a suspension is duplicated");
    }

    // The Theorem-6 work-sharing win, directly: duplicate one EXPENSIVE suspension and
    // normalize both copies. δⁿ forces the shared chain once; δˢ copies it as syntax and
    // forces both copies, doing ~2× the reduction. (This is the win the lambda benchmarks
    // don't show — they duplicate already-reduced constructors, a hash-consing matter.)
    fn dup_cost(species: u32, k: usize) -> u64 {
        let mut n = Net::new();
        let mut e = pack(L, 0); // false
        for _ in 0..k {
            let not = build_not(&mut n); // FRESH not per level — a linear chain (a node
            e = n.susp(not, e); // is consumed once; sharing in a net goes through δ, not reuse)
        }
        let (lv, rv) = (n.new_var(), n.new_var());
        let d = n.alloc(&[pack(VAR, lv), pack(VAR, rv)]);
        n.link(pack(species, d), e); // δ duplicates the suspension
        for v in [lv, rv] {
            let res = n.new_var();
            let nn = n.alloc(&[pack(VAR, res)]);
            n.link(pack(N, nn), pack(VAR, v)); // normalize each copy to full NF
        }
        let mut b = 1_000_000_000i64;
        n.drain(&mut b);
        n.interactions
    }

    #[test]
    fn dn_shares_what_ds_redoes() {
        let dn = dup_cost(DN, 12);
        let ds = dup_cost(DS, 12);
        // δⁿ shares the shared chain's reduction; δˢ re-reduces it per copy (Prop 5).
        // (Measured k=12: δⁿ=111 vs δˢ=327 interactions, ~2.95×.)
        assert!(dn < ds, "δⁿ ({dn}) should do strictly less work than δˢ ({ds})");
    }

    #[test]
    fn equal_nf() {
        let mut n = Net::new();
        let not = build_not(&mut n);
        let nf_app = n.susp(not, pack(L, 0)); // not false → true
        let tru = n.stem(pack(L, 0)); // true
        let mut b = 1_000_000i64;
        assert_eq!(n.equal(nf_app, tru, &mut b), Some(true));
        let fls = pack(L, 0);
        let mut b2 = 1_000_000i64;
        assert_eq!(n.equal(nf_app, fls, &mut b2), Some(false));
    }
}
