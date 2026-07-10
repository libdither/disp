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
    /// The interaction counter is accumulated LOCALLY and flushed ONCE at the end — the
    /// sequential analogue of the parallel drain's thread-local `local_int` (parallel.rs).
    /// (Was a `lock xadd` per interaction; the counter is a stat, not a synchronization point.)
    pub(crate) fn drain(&mut self, budget: &mut i64) -> bool {
        let mut n: u64 = 0;
        let ok = loop {
            let Some((x, y)) = self.w.bag.pop() else { break true };
            if let Some(t) = self.w.tracer.as_mut() {
                t.pending = t.prov_bag.pop().expect("trace: prov_bag desync");
            }
            if *budget <= 0 {
                break false;
            }
            *budget -= 1;
            n += 1;
            self.interact(x, y);
        };
        if let Some(t) = self.w.tracer.as_mut() {
            t.cur = 0; // links outside a firing (readback, next load) are the loader's
        }
        self.net.interactions.fetch_add(n, Ordering::Relaxed);
        ok
    }

    /// One interaction (consumer ⊗ producer). tc-net.typ §Demand/Dispatch/Sharing.
    pub(crate) fn interact(&mut self, x: u32, y: u32) {
        let (c, p) = if is_producer(tag(x)) { (y, x) } else { (x, y) };
        let ct = tag(c);
        let pt = tag(p);
        if self.w.tracer.is_some() {
            self.trace_interaction(c, p);
        }
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
            // Each alloc passes the port its new agent's principal will link to as a
            // birth-placement hint (alloc_to, a no-op outside tiled runs).
            A => {
                let arg = self.nd(cb);
                let res = self.nd(cb + 1);
                match pt {
                    L => {
                        let s = self.alloc_to(res, &[arg]);
                        self.link(res, pack(S, s));
                    }
                    S => {
                        let x = self.nd(pb);
                        let f = self.alloc_to(res, &[x, arg]);
                        self.link(res, pack(F, f));
                    }
                    F => {
                        let l = self.nd(pb);
                        let r = self.nd(pb + 1);
                        let t = self.alloc_to(l, &[r, arg, res]);
                        self.link(pack(T1, t), l);
                    }
                    P => {
                        let pf = self.nd(pb);
                        let pa = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc_to(pf, &[pa, pack(VAR, t)]);
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
                        let a1 = self.alloc_to(pf, &[pa, pack(VAR, t)]);
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
                        let d = self.alloc_to(carg, &[pack(VAR, c1), pack(VAR, c2)]);
                        self.link(pack(DN, d), carg);
                        let a1 = self.alloc_to(x, &[pack(VAR, c1), pack(VAR, u)]);
                        self.link(pack(A, a1), x);
                        let a2 = self.alloc_to(b, &[pack(VAR, c2), pack(VAR, w)]);
                        self.link(pack(A, a2), b);
                        let a3 = self.alloc(&[pack(VAR, w), res]);
                        self.link(pack(VAR, u), pack(A, a3));
                    }
                    F => {
                        // triage: △(△w x) b c → T2(w,x,b,res) faces c
                        let w = self.nd(pb);
                        let x = self.nd(pb + 1);
                        let t = self.alloc_to(carg, &[w, x, b, res]);
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
                        let a1 = self.alloc_to(pf, &[pa, pack(VAR, t)]);
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
                        let a1 = self.alloc_to(x, &[u, res]);
                        self.link(pack(A, a1), x);
                        self.link(pack(EPS, 0), w);
                        self.link(pack(EPS, 0), b);
                    }
                    F => {
                        let u = self.nd(pb);
                        let v = self.nd(pb + 1);
                        let t = self.new_var();
                        let a1 = self.alloc_to(b, &[u, pack(VAR, t)]);
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
                // Wire-RC (rc.rs): this duplicator just fired, so it is not parked; and if
                // both its outputs are already discarded (a parked ε in each), forward the
                // producer to ε instead of copying or demanding it — for δⁿ ⊗ P that
                // erases the suspension unevaluated, killing the whole future demand
                // chain. The producer node stays alive for the ε ⊗ producer redex.
                if self.w.rc.is_some() {
                    self.w.rc.as_mut().unwrap().dn_park[cb as usize] = crate::rc::RC_NULL;
                    if self.rc_dead(dl) && self.rc_dead(dr) {
                        {
                            let rc = self.w.rc.as_mut().unwrap();
                            if pt == P {
                                rc.cancels_at_park += 1;
                            } else {
                                rc.cancels_at_copy += 1;
                            }
                        }
                        self.rc_free_output(dl);
                        self.rc_free_output(dr);
                        self.link(pack(EPS, 0), p);
                        self.free_node(cb, 2);
                        return;
                    }
                }
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
                        let d = self.alloc_to(x, &[pack(VAR, a), pack(VAR, b)]);
                        self.link(pack(sp, d), x);
                        let s1 = self.alloc_to(dl, &[pack(VAR, a)]);
                        let s2 = self.alloc_to(dr, &[pack(VAR, b)]);
                        self.link(dl, pack(S, s1));
                        self.link(dr, pack(S, s2));
                    }
                    F => {
                        let l = self.nd(pb);
                        let r = self.nd(pb + 1);
                        let (ll, lr, rl, rr) =
                            (self.new_var(), self.new_var(), self.new_var(), self.new_var());
                        let da = self.alloc_to(l, &[pack(VAR, ll), pack(VAR, lr)]);
                        self.link(pack(sp, da), l);
                        let db = self.alloc_to(r, &[pack(VAR, rl), pack(VAR, rr)]);
                        self.link(pack(sp, db), r);
                        let f1 = self.alloc_to(dl, &[pack(VAR, ll), pack(VAR, rl)]);
                        let f2 = self.alloc_to(dr, &[pack(VAR, lr), pack(VAR, rr)]);
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
                            let a1 = self.alloc_to(pf, &[pa, pack(VAR, t)]);
                            self.link(pack(A, a1), pf);
                            self.link(pack(VAR, t), c);
                        } else {
                            // δˢ: structural copy of the unevaluated suspension.
                            let (fl, fr, al, ar) =
                                (self.new_var(), self.new_var(), self.new_var(), self.new_var());
                            let df = self.alloc_to(pf, &[pack(VAR, fl), pack(VAR, fr)]);
                            self.link(pack(sp, df), pf);
                            let da = self.alloc_to(pa, &[pack(VAR, al), pack(VAR, ar)]);
                            self.link(pack(sp, da), pa);
                            let p1 = self.alloc_to(dl, &[pack(VAR, fl), pack(VAR, al)]);
                            let p2 = self.alloc_to(dr, &[pack(VAR, fr), pack(VAR, ar)]);
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

    /// E1 rung C (trace.rs): record this interaction's DAG node. In-edges: the flow
    /// provenance of both redex sides (who last moved them into contact, popped off
    /// `prov_bag` by `drain`) plus the birth of each node-bearing consumed agent (who
    /// allocated the cells being read). Read BEFORE the rule body frees/reuses the nodes.
    /// Also rung A: the allocation-order distance between the two consumed nodes.
    #[cold]
    fn trace_interaction(&mut self, c: u32, p: u32) {
        let t = self.w.tracer.as_mut().unwrap();
        let (pa, pb) = t.pending;
        let mut edges = [0u32; 4];
        let mut n = 0usize;
        let push = |edges: &mut [u32; 4], n: &mut usize, e: u32| {
            if !edges[..*n].contains(&e) {
                edges[*n] = e;
                *n += 1;
            }
        };
        push(&mut edges, &mut n, pa);
        push(&mut edges, &mut n, pb);
        let (ct, pt) = (tag(c), tag(p));
        let (ca, pr) = (arity(ct), arity(pt));
        if ca > 0 {
            push(&mut edges, &mut n, t.node_birth[val(c)]);
        }
        if pr > 0 {
            push(&mut edges, &mut n, t.node_birth[val(p)]);
        }
        if ca > 0 && pr > 0 {
            let d = (val(c) as i64 - val(p) as i64).unsigned_abs();
            t.pop_hist[crate::trace::dist_bucket(d)] += 1;
        }
        t.event(ct, pt, &edges[..n]);
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
