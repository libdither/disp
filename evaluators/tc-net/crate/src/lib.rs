//! Rooted TC-Net: a sequential evaluator for disp tree calculus.
//!
//! **M0 + M1 (sequential, hash-consed reducer strategy — see TC_NET_PLAN.md
//! §Architecture / Implementation strategy).** With hash-consing the formal net
//! collapses to a tree reducer: producers (`Leaf`/`Stem`/`Fork`/`Susp`=`P`) are
//! hash-consed nodes; the consumers (`A`/`T₁`/`T₂`/`δ`/`ε`) are the control flow
//! of the reduction functions, not stored agents. `δˢ` (structural copy) = sharing
//! a hash-cons reference (free); `δⁿ` at-most-once = the `forced` memo cell on a
//! shared `Susp` node. Materialized agents + parked duplicators + reachability GC
//! are deferred to M2 (parallel), where they are actually required.
//!
//! - **M0 (eager):** `reduce` fully normalizes `apply(f,a)` to a value. Validated
//!   by the `#[cfg(test)]` worked examples (tc-net.typ §Worked Example).
//! - **M1 (lazy / call-by-need):** the live `tc_apply` returns `Susp(f,a)` (O(1));
//!   `force` drives WHNF on demand, memoizing each visited `Susp` (`δⁿ`
//!   at-most-once). `tc_dump_ternary`/`tc_classify`/`tc_equal` force as needed.
//!
//! FFI: numbers-only except the one ternary byte-copy per term (EVALUATOR_PLAN
//! §4.2). Handles are `u32` arena indices, opaque to the host. A WASM instance owns
//! exactly one arena; `dispose()` drops the instance, so the arena vanishes
//! wholesale (grow-until-dispose absorbs the per-session laziness leak —
//! tc-net.typ §Costs of δⁿ). Budget exhaustion returns `u32::MAX`, never a trap
//! (`panic = "abort"`).
// `reduce`/`dispatch_eager`/`memo` are the M0 eager reference, exercised by the
// cargo tests but not by the lazy `tc_apply` the wasm build ships — hence dead in
// release.
#![allow(clippy::missing_safety_doc, dead_code)]

use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;

// Deterministic hasher (fixed keys, no entropy) — wasm32-unknown-unknown has no
// randomness source, so the std `RandomState` default would not link cleanly.
type Map<K, V> = HashMap<K, V, BuildHasherDefault<DefaultHasher>>;

/// A producer node (tc-net.typ §Agents). `Leaf`/`Stem`/`Fork` are constructors;
/// `Susp(f,a)` is the suspended application `P(f,a)` — the call-by-need parking
/// point. Consumers are reduction steps over these, not stored kinds.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Node {
    Leaf,
    Stem(u32),
    Fork(u32, u32),
    Susp(u32, u32),
}

const LEAF_ID: u32 = 0;
/// `forced[id]` sentinel: this `Susp` has not been forced yet.
const NOT_FORCED: u32 = u32::MAX;

/// Budget exhaustion, threaded as `Err` through the recursive reducers so the
/// boundary can return the `u32::MAX` sentinel instead of trapping.
struct Exhausted;
type R = Result<u32, Exhausted>;

/// One session's arena (a WASM instance owns exactly one). Handles are indices
/// into `nodes`. `intern` is the hash-cons table (structural node → id), so `δˢ`
/// copy is reference sharing and identical `Susp`s share one `forced` memo cell.
struct Arena {
    nodes: Vec<Node>,
    intern: Map<Node, u32>,
    /// Per-node `Susp` WHNF memo (`δⁿ` at-most-once); `NOT_FORCED` until forced.
    /// Parallel to `nodes`; unused for constructor nodes.
    forced: Vec<u32>,
    /// Eager apply memo (M0): `apply(f,a)` is a pure function of `(f,a)`, and
    /// hash-consing makes `(f,a)` a complete key (the eager backend's `applyMemo`).
    memo: Map<(u32, u32), u32>,
    /// Interaction counter (the backend-declared budget unit; reported via stats).
    interactions: u64,
}

impl Arena {
    fn new() -> Self {
        let mut a = Arena {
            nodes: Vec::with_capacity(1 << 16),
            intern: Map::default(),
            forced: Vec::with_capacity(1 << 16),
            memo: Map::default(),
            interactions: 0,
        };
        let id = a.mk(Node::Leaf);
        debug_assert_eq!(id, LEAF_ID);
        a
    }

    /// Intern a node (hash-cons): structurally-identical nodes share one id.
    #[inline]
    fn mk(&mut self, n: Node) -> u32 {
        if let Some(&id) = self.intern.get(&n) {
            return id;
        }
        let id = self.nodes.len() as u32;
        self.nodes.push(n);
        self.forced.push(NOT_FORCED);
        self.intern.insert(n, id);
        id
    }

    #[inline]
    fn node(&self, id: u32) -> Node {
        self.nodes[id as usize]
    }
    #[inline]
    fn stem(&mut self, c: u32) -> u32 {
        self.mk(Node::Stem(c))
    }
    #[inline]
    fn fork(&mut self, l: u32, r: u32) -> u32 {
        self.mk(Node::Fork(l, r))
    }
    #[inline]
    fn susp(&mut self, f: u32, a: u32) -> u32 {
        self.mk(Node::Susp(f, a))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // M0 — eager reducer: `reduce(f, a)` fully normalizes `apply(f, a)`.
    // In pure tree calculus every Leaf/Stem/Fork is already a value, so this maps
    // (NF, NF) → NF. (Used by the cargo tests; tc_apply ships the lazy core.)
    // ─────────────────────────────────────────────────────────────────────────

    fn reduce(&mut self, f: u32, x: u32, budget: &mut i64) -> R {
        if let Some(&r) = self.memo.get(&(f, x)) {
            return Ok(r);
        }
        if *budget <= 0 {
            return Err(Exhausted);
        }
        *budget -= 1;
        self.interactions += 1;
        let r = match self.node(f) {
            Node::Leaf => self.stem(x),       // A ⊗ L: △ x
            Node::Stem(a) => self.fork(a, x), // A ⊗ S: △ a x
            Node::Fork(a, b) => self.dispatch_eager(a, b, x, budget)?, // A ⊗ F: T₁
            Node::Susp(f2, a2) => {
                // M0 never builds Susp, but stay total: force then re-apply.
                let fv = self.force(f2, a2, f, budget)?;
                return self.reduce(fv, x, budget);
            }
        };
        self.memo.insert((f, x), r);
        Ok(r)
    }

    /// T₁/T₂ dispatch on `△ a b x` (eager). `a` is the operator, `b` the second
    /// argument, `x` the third (the one that triggered reduction).
    fn dispatch_eager(&mut self, a: u32, b: u32, x: u32, budget: &mut i64) -> R {
        match self.node(a) {
            // K: △ △ b x → b  (x discarded, never reduced)
            Node::Leaf => Ok(b),
            // S: △ (△ c) b x → (c x)(b x)
            Node::Stem(c) => {
                let cx = self.reduce(c, x, budget)?;
                // lazy discard: if cx = K(v) = △ △ v, then (cx)(bx) = v without bx.
                if let Node::Fork(cl, cr) = self.node(cx) {
                    if matches!(self.node(cl), Node::Leaf) {
                        return Ok(cr);
                    }
                }
                let bx = self.reduce(b, x, budget)?;
                self.reduce(cx, bx, budget)
            }
            // triage: △ (△ w u) b x → dispatch on x  (w = a.left, u = a.right)
            Node::Fork(w, u) => match self.node(x) {
                Node::Leaf => Ok(w),                       // T₂ ⊗ L → w
                Node::Stem(v) => self.reduce(u, v, budget), // T₂ ⊗ S → u v
                Node::Fork(s, t) => {
                    // T₂ ⊗ F → (b s) t
                    let bs = self.reduce(b, s, budget)?;
                    self.reduce(bs, t, budget)
                }
                Node::Susp(f2, a2) => {
                    let xv = self.force(f2, a2, x, budget)?;
                    self.dispatch_eager(a, b, xv, budget)
                }
            },
            Node::Susp(f2, a2) => {
                let av = self.force(f2, a2, a, budget)?;
                self.dispatch_eager(av, b, x, budget)
            }
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // M1 — lazy call-by-need. `tc_apply` returns `Susp(f,a)`; `force` drives WHNF
    // on demand and memoizes every visited Susp (δⁿ at-most-once, Theorem 6).
    // ─────────────────────────────────────────────────────────────────────────

    /// Force `Susp(f0, a0)` (whose own id is `self_id`) to WHNF — a Leaf/Stem/Fork
    /// whose children may still be suspended. Memoizes the whole demand chain.
    fn force(&mut self, f0: u32, a0: u32, self_id: u32, budget: &mut i64) -> R {
        // Already forced?
        let fc = self.forced[self_id as usize];
        if fc != NOT_FORCED {
            return Ok(fc);
        }
        let mut chain: Vec<u32> = Vec::new();
        let mut h = self_id;
        let mut f = f0;
        let mut a = a0;
        let result = loop {
            // `h` is a not-yet-forced Susp(f, a). Take one WHNF step of apply(f,a).
            chain.push(h);
            if *budget <= 0 {
                return Err(Exhausted);
            }
            *budget -= 1;
            self.interactions += 1;
            let next = self.step_lazy(f, a, budget)?;
            match self.node(next) {
                Node::Susp(nf, na) => {
                    let nfc = self.forced[next as usize];
                    if nfc != NOT_FORCED {
                        break nfc; // already-forced Susp: its WHNF ends the chain
                    }
                    h = next;
                    f = nf;
                    a = na;
                }
                _ => break next, // constructor: WHNF reached
            }
        };
        for &s in &chain {
            self.forced[s as usize] = result;
        }
        Ok(result)
    }

    /// One head-reduction step of `apply(f, a)` in the lazy world: force the
    /// operator to WHNF, then build the result with suspended (lazy) children.
    fn step_lazy(&mut self, f: u32, a: u32, budget: &mut i64) -> R {
        let wf = self.whnf(f, budget)?;
        match self.node(wf) {
            Node::Leaf => Ok(self.stem(a)),    // A ⊗ L: △ a  (a stays lazy)
            Node::Stem(c) => Ok(self.fork(c, a)), // A ⊗ S: △ c a
            Node::Fork(p, b) => self.dispatch_lazy(p, b, a, budget), // A ⊗ F: T₁
            Node::Susp(..) => unreachable!("whnf returns a constructor"),
        }
    }

    /// T₁/T₂ dispatch on `△ p b x` (lazy): force the operator (and, for triage,
    /// the discriminant) to WHNF; emit suspensions for sub-applications.
    fn dispatch_lazy(&mut self, p: u32, b: u32, x: u32, budget: &mut i64) -> R {
        let wp = self.whnf(p, budget)?;
        match self.node(wp) {
            // K: → b  (b not forced — pure call-by-need discard)
            Node::Leaf => Ok(b),
            // S: → (c x)(b x), every application suspended
            Node::Stem(c) => {
                let cx = self.susp(c, x);
                let bx = self.susp(b, x);
                Ok(self.susp(cx, bx))
            }
            // triage on x  (w = p.left, u = p.right)
            Node::Fork(w, u) => {
                let wx = self.whnf(x, budget)?;
                match self.node(wx) {
                    Node::Leaf => Ok(w),
                    Node::Stem(v) => Ok(self.susp(u, v)),
                    Node::Fork(s, t) => {
                        let bs = self.susp(b, s);
                        Ok(self.susp(bs, t))
                    }
                    Node::Susp(..) => unreachable!("whnf returns a constructor"),
                }
            }
            Node::Susp(..) => unreachable!("whnf returns a constructor"),
        }
    }

    /// Force `h` to WHNF (a Leaf/Stem/Fork). Non-Susp passes through; this is the
    /// only place demand-driven reduction runs under M1.
    fn whnf(&mut self, h: u32, budget: &mut i64) -> R {
        match self.node(h) {
            Node::Susp(f, a) => self.force(f, a, h, budget),
            _ => Ok(h),
        }
    }

    /// Full normal form: WHNF, then recursively normalize children. (`dump`/`equal`
    /// need full NF; the core is weak-head — tc-net.typ §Full Normalization.)
    fn nf(&mut self, h: u32, budget: &mut i64) -> R {
        let w = self.whnf(h, budget)?;
        match self.node(w) {
            Node::Leaf => Ok(w),
            Node::Stem(c) => {
                let c2 = self.nf(c, budget)?;
                Ok(self.stem(c2))
            }
            Node::Fork(l, r) => {
                let l2 = self.nf(l, budget)?;
                let r2 = self.nf(r, budget)?;
                Ok(self.fork(l2, r2))
            }
            Node::Susp(..) => unreachable!("whnf returns a constructor"),
        }
    }

    /// Decidable structural equality of normal forms — demand-then-compare, with
    /// the hash-cons id shortcut and short-circuit on first difference (decision 1).
    fn equal(&mut self, a: u32, b: u32, budget: &mut i64) -> Result<bool, Exhausted> {
        if a == b {
            return Ok(true); // hash-cons identity (incl. shared susps)
        }
        let wa = self.whnf(a, budget)?;
        let wb = self.whnf(b, budget)?;
        if wa == wb {
            return Ok(true);
        }
        match (self.node(wa), self.node(wb)) {
            (Node::Leaf, Node::Leaf) => Ok(true),
            (Node::Stem(x), Node::Stem(y)) => self.equal(x, y, budget),
            (Node::Fork(x1, x2), Node::Fork(y1, y2)) => {
                Ok(self.equal(x1, y1, budget)? && self.equal(x2, y2, budget)?)
            }
            _ => Ok(false),
        }
    }

    // ── ternary codec (preorder arity: leaf "0", stem "1"+child, fork "2"+l+r) ──
    fn parse(&mut self, s: &[u8], i: &mut usize) -> u32 {
        if *i >= s.len() {
            return LEAF_ID;
        }
        let c = s[*i];
        *i += 1;
        match c {
            b'0' => LEAF_ID,
            b'1' => {
                let c0 = self.parse(s, i);
                self.stem(c0)
            }
            b'2' => {
                let l = self.parse(s, i);
                let r = self.parse(s, i);
                self.fork(l, r)
            }
            _ => LEAF_ID,
        }
    }

    /// Encode an already-normalized value to ternary. `h` must be Susp-free
    /// (call `nf` first).
    fn encode(&self, h: u32, out: &mut Vec<u8>) {
        match self.node(h) {
            Node::Leaf => out.push(b'0'),
            Node::Stem(c) => {
                out.push(b'1');
                self.encode(c, out);
            }
            Node::Fork(l, r) => {
                out.push(b'2');
                self.encode(l, out);
                self.encode(r, out);
            }
            Node::Susp(..) => out.push(b'0'), // unreachable after nf; defensive
        }
    }
}

thread_local! {
    static ARENA: RefCell<Arena> = RefCell::new(Arena::new());
}

#[inline]
fn with<T>(f: impl FnOnce(&mut Arena) -> T) -> T {
    ARENA.with(|a| f(&mut a.borrow_mut()))
}

#[inline]
fn pack(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

// ── term algebra (Session.leaf / stem / fork) ───────────────────────────────
#[no_mangle]
pub extern "C" fn tc_leaf() -> u32 {
    LEAF_ID
}
#[no_mangle]
pub extern "C" fn tc_stem(child: u32) -> u32 {
    with(|a| a.stem(child))
}
#[no_mangle]
pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 {
    with(|a| a.fork(left, right))
}

// ── computation (Session.apply) — LAZY: build the suspended application ───────
/// `apply f x = Susp(f, x)`, reduced on demand by the forcing observations.
/// O(1); `budget` is unused here (forcing is where reduction — and exhaustion —
/// happens). Returns `u32::MAX` only on overflow, which cannot occur for a build.
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, _budget: u32) -> u32 {
    with(|a| a.susp(f, x))
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
    with(|a| {
        let mut i = 0usize;
        a.parse(bytes, &mut i)
    })
}
/// Force full NF, serialize to ternary, return `(ptr << 32) | len`. On budget
/// exhaustion returns `(u32::MAX << 32)` (ptr = `u32::MAX`, len = 0) — the host
/// checks the ptr sentinel.
#[no_mangle]
pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 {
    with(|a| {
        let mut b = budget as i64;
        let nf = match a.nf(h, &mut b) {
            Ok(n) => n,
            Err(_) => return pack(u32::MAX, 0),
        };
        let mut out: Vec<u8> = Vec::new();
        a.encode(nf, &mut out);
        let len = out.len() as u32;
        let ptr = out.as_ptr() as u32;
        std::mem::forget(out);
        pack(ptr, len)
    })
}

// ── observations ────────────────────────────────────────────────────────────
/// WHNF classify: `0`=leaf `1`=stem `2`=fork; `u32::MAX` on exhaustion. Forces.
#[no_mangle]
pub extern "C" fn tc_classify(h: u32, budget: u32) -> u32 {
    with(|a| {
        let mut b = budget as i64;
        match a.whnf(h, &mut b) {
            Ok(w) => match a.node(w) {
                Node::Leaf => 0,
                Node::Stem(_) => 1,
                Node::Fork(..) => 2,
                Node::Susp(..) => u32::MAX,
            },
            Err(_) => u32::MAX,
        }
    })
}
/// Child of a WHNF node. Assumes a preceding `tc_classify` already forced `h`
/// (the host calls classify first); if not, forces with a large budget.
#[no_mangle]
pub extern "C" fn tc_child0(h: u32) -> u32 {
    with(|a| {
        let mut b = i64::MAX;
        let w = a.whnf(h, &mut b).unwrap_or(LEAF_ID);
        match a.node(w) {
            Node::Stem(c) => c,
            Node::Fork(l, _) => l,
            _ => LEAF_ID,
        }
    })
}
#[no_mangle]
pub extern "C" fn tc_child1(h: u32) -> u32 {
    with(|a| {
        let mut b = i64::MAX;
        let w = a.whnf(h, &mut b).unwrap_or(LEAF_ID);
        match a.node(w) {
            Node::Fork(_, r) => r,
            _ => LEAF_ID,
        }
    })
}
/// NF equality: `0`=false `1`=true `2`=budget exhausted (host checks `> 1`).
#[no_mangle]
pub extern "C" fn tc_equal(a_h: u32, b_h: u32, budget: u32) -> u32 {
    with(|a| {
        let mut b = budget as i64;
        match a.equal(a_h, b_h, &mut b) {
            Ok(true) => 1,
            Ok(false) => 0,
            Err(_) => 2,
        }
    })
}
/// Total interactions consumed this session (the backend-declared budget unit).
#[no_mangle]
pub extern "C" fn tc_interactions() -> u64 {
    with(|a| a.interactions)
}

#[cfg(test)]
mod tests {
    use super::*;

    // tc-net.typ §Worked Example boolean negation.
    //   false = △ = L;  true = △△ = S(L)
    //   not = △ (△ (△△) (△△△)) △ = F(F(S(L), F(L,L)), L)
    fn build_not(a: &mut Arena) -> u32 {
        let l = LEAF_ID;
        let sl = a.stem(l); // S(L) = true
        let fll = a.fork(l, l); // F(L,L)
        let inner = a.fork(sl, fll); // F(S(L), F(L,L))
        a.fork(inner, l) // F(.., L) = not
    }

    #[test]
    fn eager_not_false_is_true() {
        let mut a = Arena::new();
        let not = build_not(&mut a);
        let f = LEAF_ID; // false
        let mut budget = 1_000_000i64;
        let r = a.reduce(not, f, &mut budget).ok().unwrap();
        let expect_true = a.stem(LEAF_ID);
        assert_eq!(r, expect_true, "not false = true");
    }

    #[test]
    fn eager_not_true_is_false() {
        let mut a = Arena::new();
        let not = build_not(&mut a);
        let t = a.stem(LEAF_ID); // true
        let mut budget = 1_000_000i64;
        let r = a.reduce(not, t, &mut budget).ok().unwrap();
        assert_eq!(r, LEAF_ID, "not true = false");
    }

    // Same examples through the LAZY core (tc_apply = Susp, then nf forces).
    #[test]
    fn lazy_not_false_is_true() {
        let mut a = Arena::new();
        let not = build_not(&mut a);
        let app = a.susp(not, LEAF_ID); // apply(not, false), suspended
        let mut budget = 1_000_000i64;
        let r = a.nf(app, &mut budget).ok().unwrap();
        let expect_true = a.stem(LEAF_ID);
        assert_eq!(r, expect_true);
    }

    #[test]
    fn lazy_not_true_is_false() {
        let mut a = Arena::new();
        let not = build_not(&mut a);
        let t = a.stem(LEAF_ID);
        let app = a.susp(not, t);
        let mut budget = 1_000_000i64;
        let r = a.nf(app, &mut budget).ok().unwrap();
        assert_eq!(r, LEAF_ID);
    }

    // S/K combinators: K x y = x ;  S K K x = x  (identity).
    fn k(a: &mut Arena) -> u32 {
        // K = △△ = S(L)
        a.stem(LEAF_ID)
    }
    #[test]
    fn lazy_k_discards_second() {
        let mut a = Arena::new();
        let kk = k(&mut a);
        let x = a.fork(LEAF_ID, LEAF_ID); // some value
        let y = a.stem(LEAF_ID);
        let kx = a.susp(kk, x); // K x
        let kxy = a.susp(kx, y); // (K x) y → x
        let mut budget = 1_000_000i64;
        let r = a.nf(kxy, &mut budget).ok().unwrap();
        let xnf = a.nf(x, &mut budget).ok().unwrap();
        assert_eq!(r, xnf, "K x y = x");
    }

    #[test]
    fn ternary_roundtrip() {
        let mut a = Arena::new();
        // F(S(L), F(L,L)) = 2 1 0 2 0 0
        let s = b"210200";
        let mut i = 0usize;
        let h = a.parse(s, &mut i);
        assert_eq!(i, s.len());
        let mut out = Vec::new();
        a.encode(h, &mut out);
        assert_eq!(&out, s);
    }

    // Eager and lazy must agree on NF (confluence) for a few applications.
    #[test]
    fn eager_lazy_agree() {
        let mut a = Arena::new();
        let not = build_not(&mut a);
        for arg in [LEAF_ID] {
            let t = a.stem(LEAF_ID);
            for x in [arg, t] {
                let mut b1 = 1_000_000i64;
                let eager = a.reduce(not, x, &mut b1).ok().unwrap();
                let app = a.susp(not, x);
                let mut b2 = 1_000_000i64;
                let lazy = a.nf(app, &mut b2).ok().unwrap();
                assert_eq!(eager, lazy, "eager vs lazy NF disagree");
            }
        }
    }
}
