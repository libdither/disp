//! The reducers. Two strategies over the hash-consed arena:
//!
//! - **M0 (eager):** `reduce(f,a)` fully normalizes `apply(f,a)` to a value — the
//!   elaboration-conformance mode (disp's elaborator is eager-normative). An iterative
//!   explicit-stack machine (a port of `src/core/tree.ts apply`): native recursion would
//!   overflow on the kernel's deep reduction spines (tens of millions deep).
//! - **M1 (lazy / call-by-need):** `force` drives WHNF on demand, memoizing every
//!   visited `Susp` (`δⁿ` at-most-once, Theorem 6). Off the production Session path
//!   (which wires eager `tc_apply`); kept + validated so M2 inherits a live M1 core.

use crate::arena::{Arena, Exhausted, Node, R};

/// Continuation frame for the iterative eager reducer — the reduction spine lives on
/// this explicit `Vec` rather than the native stack.
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

impl Arena {
    // ── M0 — eager: `reduce(f, a)` fully normalizes `apply(f, a)`. In pure tree
    // calculus every Leaf/Stem/Fork is already a value, so this maps (NF, NF) → NF.
    pub(crate) fn reduce(&mut self, f0: u32, x0: u32, budget: &mut i64) -> R {
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

    // ── M1 — lazy call-by-need. `tc_apply` returns `Susp(f,a)`; `force` drives WHNF
    // on demand and memoizes every visited Susp (δⁿ at-most-once, Theorem 6).

    /// Force `Susp(f0, a0)` (whose own id is `self_id`) to WHNF — a Leaf/Stem/Fork whose
    /// children may still be suspended. Memoizes the whole demand chain.
    pub(crate) fn force(&mut self, f0: u32, a0: u32, self_id: u32, budget: &mut i64) -> R {
        if let Some(&fc) = self.forced.get(&self_id) {
            return Ok(fc); // already forced
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

    /// One head-reduction step of `apply(f, a)` in the lazy world: force the operator to
    /// WHNF, then build the result with suspended (lazy) children.
    fn step_lazy(&mut self, f: u32, a: u32, budget: &mut i64) -> R {
        // tree_eq fast-path (mirror the eager `reduce` stage 2): when the operator is the
        // partial application `susp(tree_eq, sa)`, the saturated call `tree_eq sa a` is
        // the O(1) hash-cons compare — WITHOUT this the lazy path forces the in-language
        // tree_eq (O(size)) and blows kernel-verify budget. (Stage 1, `apply(tree_eq, x)
        // = susp(tree_eq, x)`, is what lazy apply already builds, so only stage 2 here.)
        if self.tree_eq_id != 0 {
            if let Node::Susp(sf, sa) = self.node(f) {
                if sf == self.tree_eq_id {
                    return Ok(if self.equal(sa, a, budget)? { self.tt } else { self.ff });
                }
            }
        }
        let wf = self.whnf(f, budget)?;
        match self.node(wf) {
            Node::Leaf => Ok(self.stem(a)),    // A ⊗ L: △ a  (a stays lazy)
            Node::Stem(c) => Ok(self.fork(c, a)), // A ⊗ S: △ c a
            Node::Fork(p, b) => self.dispatch_lazy(p, b, a, budget), // A ⊗ F: T₁
            Node::Susp(..) => unreachable!("whnf returns a constructor"),
        }
    }

    /// T₁/T₂ dispatch on `△ p b x` (lazy): force the operator (and, for triage, the
    /// discriminant) to WHNF; emit suspensions for sub-applications.
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

    /// Force `h` to WHNF (a Leaf/Stem/Fork). Non-Susp passes through; the only place
    /// demand-driven reduction runs under M1.
    pub(crate) fn whnf(&mut self, h: u32, budget: &mut i64) -> R {
        match self.node(h) {
            Node::Susp(f, a) => self.force(f, a, h, budget),
            _ => Ok(h),
        }
    }

    /// Full normal form: WHNF, then recursively normalize children. (`dump`/`equal` need
    /// full NF; the core is weak-head — tc-net.typ §Full Normalization.)
    pub(crate) fn nf(&mut self, h: u32, budget: &mut i64) -> R {
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

    /// Decidable structural equality of normal forms — demand-then-compare, with the
    /// hash-cons id shortcut and short-circuit on first difference (decision 1).
    /// Iterative (work-stack of pairs) so it costs O(1) native stack regardless of tree
    /// depth — lets the wasm shadow stack stay small (see .cargo/config.toml).
    pub(crate) fn equal(&mut self, a0: u32, b0: u32, budget: &mut i64) -> Result<bool, Exhausted> {
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
}
