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
// The M1 lazy reducer (`tc_apply_lazy` / `force` / `step_lazy` / `dispatch_lazy`)
// is the work-sharing path — reachable from the forcing observations but not the
// eager `tc_apply` Session path, so parts read as dead in a pure-eager release.
#![allow(clippy::missing_safety_doc, dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};

// A fast, deterministic, dependency-free hasher (FxHash-style: rotate-xor-multiply
// per word). Two reasons over std's default: wasm32-unknown-unknown has no entropy
// for SipHash's `RandomState`, and SipHash is ~3-5× slower than this on the small
// (u32 / `Node`) keys the `intern` + `memo` tables hash on *every* reduction — the
// hottest structures in the evaluator.
#[derive(Default)]
struct FxHasher(u64);
const FX_K: u64 = 0x51_7c_c1_b7_27_22_0a_95;
impl FxHasher {
    #[inline]
    fn add(&mut self, i: u64) {
        self.0 = (self.0.rotate_left(5) ^ i).wrapping_mul(FX_K);
    }
}
impl Hasher for FxHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }
    // Fallback (e.g. the enum discriminant); the typed paths below cover the keys.
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        for chunk in bytes.chunks(8) {
            let mut b = [0u8; 8];
            b[..chunk.len()].copy_from_slice(chunk);
            self.add(u64::from_le_bytes(b));
        }
    }
    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.add(i as u64)
    }
    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.add(i)
    }
    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.add(i as u64)
    }
}
type Map<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

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

/// LEAF is handle 1, not 0: handle 0 is a reserved null sentinel so that NO valid
/// handle is JS-falsy. The host elaborator (`src/compile.ts`) was written against
/// object handles (always truthy) and tests them with `entry?.tree ? …` /
/// truthiness; a `0` LEAF handle would be misread as "absent" → spurious
/// "unresolved free variable". Reserving 0 makes every real handle ≥ 1 (truthy),
/// fixing all such checks at once without touching the shared host.
const LEAF_ID: u32 = 1;

/// Budget exhaustion, threaded as `Err` through the recursive reducers so the
/// boundary can return the `u32::MAX` sentinel instead of trapping.
struct Exhausted;
type R = Result<u32, Exhausted>;

/// Continuation frame for the iterative eager reducer (a port of `src/core/tree.ts`
/// `apply`). Native recursion would overflow on the kernel's deep reduction spines
/// (tens of millions deep), so the spine lives on an explicit `Vec`.
enum Cont {
    /// result → `apply(result, arg)`
    ApplyTo(u32),
    /// result → `apply(func, result)`
    ApplyResultTo(u32),
    /// memoize `apply(f, x) = result`
    Memo(u32, u32),
    /// S-rule tail: `result` is `c x`; carry `(b, orig_x)` to finish `(c x)(b x)`
    SAfterCx(u32, u32),
}

/// One session's arena (a WASM instance owns exactly one). Handles are indices
/// into `nodes`. `intern` is the hash-cons table (structural node → id), so `δˢ`
/// copy is reference sharing and identical `Susp`s share one `forced` memo cell.
struct Arena {
    nodes: Vec<Node>,
    intern: Map<Node, u32>,
    /// `Susp` WHNF memo (`δⁿ` at-most-once): susp id → its forced WHNF. SPARSE —
    /// only forced susps appear (absent = not yet forced), so eager mode (which
    /// never forces) keeps it empty and `mk` does zero per-node bookkeeping.
    forced: Map<u32, u32>,
    /// Eager apply memo (M0): `apply(f,a)` is a pure function of `(f,a)`, and
    /// hash-consing makes `(f,a)` a complete key (the eager backend's `applyMemo`).
    memo: Map<(u32, u32), u32>,
    /// Reduction counter = fork-dispatches (the budget unit; eager-`steps`-
    /// equivalent; reported via stats). Leaf/stem builds and the tree_eq fast-path
    /// are O(1) and uncounted, matching src/core/tree.ts.
    interactions: u64,
    /// The `tree_eq` definition's handle, registered via `recognizeNative` (0 =
    /// unset). When set, `apply(tree_eq, a) b` is intercepted as the O(1)
    /// hash-cons structural compare (the two-stage susp scheme of the eager
    /// backend), so conversion checking is cheap — without it, in-language
    /// `tree_eq` makes kernel verification blow the host's apply budget.
    tree_eq_id: u32,
    /// Scott `TT`/`FF` (the fast-path results), built once at session init.
    tt: u32,
    ff: u32,
    /// Cap on `memo` entries; over it the eager apply memo is cleared (pure cache —
    /// correctness-preserving, just re-reduces). Default unbounded (`usize::MAX`);
    /// lower via `tc_set_memo_limit` to trade speed for a smaller footprint on
    /// long-lived sessions. Per-file sessions are bounded by `dispose()` regardless.
    memo_limit: usize,
}

impl Arena {
    fn new() -> Self {
        let mut a = Arena {
            nodes: Vec::with_capacity(1 << 16),
            intern: Map::default(),
            forced: Map::default(),
            memo: Map::default(),
            interactions: 0,
            tree_eq_id: 0,
            tt: 0,
            ff: 0,
            memo_limit: usize::MAX,
        };
        // index 0: reserved null sentinel (never interned, never returned) so no
        // valid handle is JS-falsy — see LEAF_ID.
        a.nodes.push(Node::Leaf);
        let id = a.mk(Node::Leaf); // real LEAF interned at index 1
        debug_assert_eq!(id, LEAF_ID);
        // Scott TT/FF — the exact trees the prelude's TT/FF compile to (matching
        // src/core/tree.ts SCOTT_TT/FF): TT = △(△△), FF = △(△(△(△△)△)).
        let l = LEAF_ID;
        let k = a.stem(l); // K = △△
        a.tt = a.fork(l, k); // TT = fork(L, K)
        let ll = a.fork(l, l);
        let i = a.fork(ll, l); // I = fork(fork(L,L), L)
        let ki = a.fork(l, i);
        a.ff = a.fork(l, ki); // FF = fork(L, fork(L, I))
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

    fn reduce(&mut self, f0: u32, x0: u32, budget: &mut i64) -> R {
        let mut stack: Vec<Cont> = Vec::new();
        let mut cur_f = f0;
        let mut cur_x = x0;
        'outer: loop {
            if *budget <= 0 {
                return Err(Exhausted);
            }
            // Produce a `result` value, or `continue 'outer` after re-aiming cur_f/cur_x.
            let result: u32;
            if self.tree_eq_id != 0 && cur_f == self.tree_eq_id {
                // tree_eq fast-path stage 1: apply(tree_eq, x) ⇒ susp(tree_eq, x).
                // O(1); not budget- or interaction-charged (matches eager's fast-path).
                result = self.susp(cur_f, cur_x);
            } else {
                result = match self.node(cur_f) {
                // Leaf/Stem are O(1) constructor builds (argument accumulation, NOT
                // reductions) — neither budget- nor interaction-charged, matching
                // src/core/tree.ts (only fork dispatch counts). Charging them burns
                // the host's per-call budget ~3× faster → spurious verify exhaustion.
                Node::Leaf => self.stem(cur_x), // A ⊗ L: △ x
                Node::Stem(a) => self.fork(a, cur_x), // A ⊗ S: △ a x
                Node::Susp(sf, sa) => {
                    if self.tree_eq_id != 0 && sf == self.tree_eq_id {
                        // tree_eq fast-path stage 2: apply(susp(tree_eq, a), x) ⇒
                        // a == x ? TT : FF (O(1) hash-cons structural compare).
                        if self.equal(sa, cur_x, budget)? {
                            self.tt
                        } else {
                            self.ff
                        }
                    } else {
                        // Operator suspension: force then re-dispatch (eager never
                        // builds these, but lazy-path nodes may flow in; stay total).
                        cur_f = self.force(sf, sa, cur_f, budget)?;
                        continue 'outer;
                    }
                }
                Node::Fork(a, b) => {
                    if let Some(&r) = self.memo.get(&(cur_f, cur_x)) {
                        r
                    } else if let Node::Susp(sf, sa) = self.node(a) {
                        let av = self.force(sf, sa, a, budget)?;
                        cur_f = self.fork(av, b);
                        continue 'outer;
                    } else {
                        *budget -= 1;
                        self.interactions += 1;
                        match self.node(a) {
                            // K: △ △ b x → b  (x discarded, never reduced). No memo
                            // frame — the result is constant in x, caching pollutes.
                            Node::Leaf => b,
                            // S: △ (△ c) b x → (c x)(b x). Compute c x first.
                            Node::Stem(c) => {
                                stack.push(Cont::Memo(cur_f, cur_x));
                                stack.push(Cont::SAfterCx(b, cur_x));
                                cur_f = c; // cur_x unchanged
                                continue 'outer;
                            }
                            // triage: △ (△ w u) b x → dispatch on x (w=a.left, u=a.right)
                            Node::Fork(w, u) => {
                                stack.push(Cont::Memo(cur_f, cur_x));
                                if let Node::Susp(sf, sa) = self.node(cur_x) {
                                    cur_x = self.force(sf, sa, cur_x, budget)?;
                                }
                                match self.node(cur_x) {
                                    Node::Leaf => w, // T₂ ⊗ L → w
                                    Node::Stem(v) => {
                                        cur_f = u; // T₂ ⊗ S → u v
                                        cur_x = v;
                                        continue 'outer;
                                    }
                                    Node::Fork(s, t) => {
                                        // T₂ ⊗ F → (b s) t
                                        stack.push(Cont::ApplyTo(t));
                                        cur_f = b;
                                        cur_x = s;
                                        continue 'outer;
                                    }
                                    Node::Susp(..) => unreachable!("forced above"),
                                }
                            }
                            Node::Susp(..) => unreachable!("handled above"),
                        }
                    }
                }
                };
            }
            // deliver(result): pop continuations until one re-aims the reducer.
            let mut res = result;
            loop {
                match stack.pop() {
                    None => return Ok(res),
                    Some(Cont::ApplyTo(arg)) => {
                        cur_f = res;
                        cur_x = arg;
                        continue 'outer;
                    }
                    Some(Cont::ApplyResultTo(func)) => {
                        cur_f = func;
                        cur_x = res;
                        continue 'outer;
                    }
                    Some(Cont::Memo(sf, sx)) => {
                        self.memo.insert((sf, sx), res);
                        // Cap the pure-cache memo (correctness-preserving — re-reduces).
                        if self.memo.len() > self.memo_limit {
                            self.memo.clear();
                        }
                        // keep popping
                    }
                    Some(Cont::SAfterCx(sb, sox)) => {
                        // res = c x. apply(res, b x): if res = K(cr) = △ △ cr, the
                        // result is cr without computing b x (lazy discard).
                        if let Node::Fork(cl, cr) = self.node(res) {
                            if matches!(self.node(cl), Node::Leaf) {
                                res = cr;
                                continue;
                            }
                        }
                        // general: compute b x, then apply res to it.
                        stack.push(Cont::ApplyResultTo(res));
                        cur_f = sb;
                        cur_x = sox;
                        continue 'outer;
                    }
                }
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
        if let Some(&fc) = self.forced.get(&self_id) {
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
                    if let Some(&nfc) = self.forced.get(&next) {
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
            self.forced.insert(s, result);
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
    /// Iterative (work-stack of pairs) so it costs O(1) native stack regardless of
    /// tree depth — lets the wasm shadow stack stay small (see .cargo/config.toml).
    fn equal(&mut self, a0: u32, b0: u32, budget: &mut i64) -> Result<bool, Exhausted> {
        let mut stack: Vec<(u32, u32)> = vec![(a0, b0)];
        while let Some((a, b)) = stack.pop() {
            if a == b {
                continue; // hash-cons identity (incl. shared susps)
            }
            let wa = self.whnf(a, budget)?;
            let wb = self.whnf(b, budget)?;
            if wa == wb {
                continue;
            }
            match (self.node(wa), self.node(wb)) {
                (Node::Leaf, Node::Leaf) => {}
                (Node::Stem(x), Node::Stem(y)) => stack.push((x, y)),
                (Node::Fork(x1, x2), Node::Fork(y1, y2)) => {
                    stack.push((x1, y1));
                    stack.push((x2, y2));
                }
                _ => return Ok(false), // short-circuit on first difference
            }
        }
        Ok(true)
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

    /// Force `h` to full NF and emit ternary directly — WITHOUT materializing the
    /// NF tree (a big result streams to `out` instead of being built as nodes;
    /// iterative, so deep results don't recurse). For eager handles `whnf` is the
    /// identity, so this is an iterative encode; lazy handles force as they descend.
    fn dump_emit(&mut self, h: u32, out: &mut Vec<u8>, budget: &mut i64) -> Result<(), Exhausted> {
        let mut stack: Vec<u32> = vec![h];
        while let Some(n) = stack.pop() {
            let w = self.whnf(n, budget)?;
            match self.node(w) {
                Node::Leaf => out.push(b'0'),
                Node::Stem(c) => {
                    out.push(b'1');
                    stack.push(c);
                }
                Node::Fork(l, r) => {
                    out.push(b'2');
                    stack.push(r); // pushed first ⇒ popped after the left child
                    stack.push(l);
                }
                Node::Susp(..) => unreachable!("whnf returns a constructor"),
            }
        }
        Ok(())
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

// ── computation (Session.apply) ─────────────────────────────────────────────
/// **EAGER (M0)** — fully normalize `apply(f, x)`. This is the elaboration
/// conformance mode: disp's elaborator is eager-normative (EVALUATOR_PLAN
/// decision 7), so the full-`lib/tests` gate needs eager reduction (a lazy
/// `tc_apply` defers redexes the elaborator's host-side observations assume are
/// already reduced). Returns `u32::MAX` on budget exhaustion.
///
/// The **M1 lazy** core (`tc_apply_lazy` → `Susp`, forced by the observations) is
/// the work-sharing path, validated separately by the reduction differential; it
/// is NOT the Session `apply`, because the elaborator can't consume a lazy one yet
/// (the decision-7 crux — retiring it needs demand-driven compile-time
/// normalization, out of scope here).
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, budget: u32) -> u32 {
    with(|a| {
        let mut b = budget as i64;
        match a.reduce(f, x, &mut b) {
            Ok(r) => r,
            Err(_) => u32::MAX,
        }
    })
}

/// M1 lazy apply: `apply f x = Susp(f, x)`, reduced on demand by the forcing
/// observations (`dump`/`classify`/`equal`). The work-sharing path (δⁿ
/// at-most-once); exposed for the reduction differential + laziness benchmark.
#[no_mangle]
pub extern "C" fn tc_apply_lazy(f: u32, x: u32, _budget: u32) -> u32 {
    with(|a| a.susp(f, x))
}

/// Register the `tree_eq` definition's handle (Session.recognizeNative) so
/// `apply(tree_eq, a) b` fast-paths to the O(1) hash-cons compare. Idempotent.
#[no_mangle]
pub extern "C" fn tc_recognize_tree_eq(handle: u32) {
    with(|a| a.tree_eq_id = handle);
}

// ── memory knobs (run at smaller footprints) ────────────────────────────────
/// Cap the eager apply memo at `n` entries (0 = unbounded); over the cap the memo
/// is cleared. Pure cache → correctness-preserving (just re-reduces). Trades speed
/// for a smaller footprint on long-lived sessions.
#[no_mangle]
pub extern "C" fn tc_set_memo_limit(n: u32) {
    with(|a| a.memo_limit = if n == 0 { usize::MAX } else { n as usize });
}
/// Drop all caches (apply memo + susp WHNF memo) and release their backing memory
/// to relieve pressure. Correctness-preserving; the node arena (live trees stay
/// reachable via handles) is untouched — only re-derivable cache is freed.
#[no_mangle]
pub extern "C" fn tc_clear_caches() {
    with(|a| {
        a.memo.clear();
        a.memo.shrink_to_fit();
        a.forced.clear();
        a.forced.shrink_to_fit();
    });
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
        let mut out: Vec<u8> = Vec::new();
        if a.dump_emit(h, &mut out, &mut b).is_err() {
            return pack(u32::MAX, 0);
        }
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
