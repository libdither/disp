//! The reduction kernel: one `interact` (consumer ⊗ producer) per active pair, the
//! sequential `drain` driver, and the forcing observations `full_nf` / `equal`. The rule
//! table is tc-net.typ §Demand/Application, §Dispatch, §Sharing — δˢ/δⁿ share the
//! constructor copy rules and differ only on `P` (δⁿ demands-before-copy, Theorem 6).
//! No hash-consing, so equality is demand-then-compare (full NF, then structural walk).

use crate::net::Ctx;
use crate::port::*;
use std::sync::atomic::Ordering;

impl<'a> Ctx<'a> {
    /// Drive the local bag to quiescence or until the budget runs out (returns false).
    pub(crate) fn drain(&mut self, budget: &mut i64) -> bool {
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
    pub(crate) fn interact(&mut self, x: u32, y: u32) {
        let (c, p) = if is_producer(tag(x)) { (y, x) } else { (x, y) };
        let ct = tag(c);
        let pt = tag(p);
        if !is_producer(pt) {
            // consumer ⊗ consumer cannot arise in a well-formed net: every consumer's lone
            // principal faces a producer. Assert it in debug/test builds (a violation means
            // a malformed net, not a no-op to swallow); drop the redex in release.
            debug_assert!(false, "ic-net: unexpected consumer⊗consumer redex (tags {ct}/{pt})");
            return;
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

    /// Reduce `h` to full normal form by connecting an `N` normalizer and draining.
    pub(crate) fn full_nf(&mut self, h: u32, budget: &mut i64) -> Option<u32> {
        let v = self.new_var();
        let n = self.alloc(&[pack(VAR, v)]);
        self.link(pack(N, n), h);
        if !self.drain(budget) {
            return None;
        }
        Some(self.resolve(pack(VAR, v)))
    }

    /// Structural NF equality (no hash-cons ⇒ demand-then-compare).
    pub(crate) fn equal(&mut self, a: u32, b: u32, budget: &mut i64) -> Option<bool> {
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
}
