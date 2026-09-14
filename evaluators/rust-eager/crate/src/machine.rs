//! lib/machine.disp's `m_advance`, natively — the second native after `tree_eq`. A port of
//! `src/core/tree.ts stepMachine`: decode the state into registers and a frame vector, run
//! `k` dispatches by the machine's table, re-encode through the hash-consed arena, so the
//! result is the handle the disp definition would build. It forces a suspension only where
//! the disp code triages, stores the raw handles the disp code stores, and charges neither
//! budget nor interactions for the dispatches themselves (the count is the definition's).
//!
//! The encoding is machine.disp's over the lib's literal trees: `Done v = pair "Done" v`,
//! `Run = pair "Run" (pair (pair f x) stack)`, frames `pair "To" arg` / `pair "Res" func` /
//! `pair "S" (pair b x)`, a list nil = leaf / cons = fork, a string a list of char-code Nats.

use crate::arena::{Arena, Exhausted, Node, LEAF_ID};

enum Frame {
    To(u32),
    Res(u32),
    S(u32, u32),
}

struct Regs {
    f: u32,
    x: u32,
    frames: Vec<Frame>,
    done: Option<u32>,
}

struct Tags {
    done: u32,
    run: u32,
    to: u32,
    res: u32,
    s: u32,
}

impl Arena {
    /// Register `m_advance`'s handle (Session.recognizeNative). Idempotent.
    pub(crate) fn recognize_machine(&mut self, handle: u32) {
        self.machine_id = handle;
    }

    /// The lib's string literal tree (src/elab/literals.ts stringToTree).
    pub(crate) fn string_tree(&mut self, s: &str) -> u32 {
        let mut r = LEAF_ID;
        for c in s.chars().rev() {
            let mut n = LEAF_ID;
            for _ in 0..(c as u32) {
                n = self.fork(LEAF_ID, n);
            }
            r = self.fork(n, r);
        }
        r
    }

    // Re-interned per call rather than cached: a scoped reclamation (`end_scope`) may free
    // and reuse the ids of trees built inside a scope, and a stale tag id must never match.
    fn machine_tags(&mut self) -> Tags {
        Tags {
            done: self.string_tree("Done"),
            run: self.string_tree("Run"),
            to: self.string_tree("To"),
            res: self.string_tree("Res"),
            s: self.string_tree("S"),
        }
    }

    fn fork_right(&self, id: u32) -> u32 {
        match self.node(id) {
            Node::Fork(_, r) => r,
            _ => unreachable!("fork_right on a non-fork"),
        }
    }

    /// `k_shaped v` on an already-WHNF `v`: a fork whose left child triages to the leaf.
    fn k_shaped(&mut self, v: u32, budget: &mut i64) -> Result<bool, Exhausted> {
        if let Node::Fork(l, _) = self.node(v) {
            let ls = self.whnf(l, budget)?;
            return Ok(matches!(self.node(ls), Node::Leaf));
        }
        Ok(false)
    }

    /// m_deliver: pop frames until an application is pending; the S tail keeps the discard.
    fn m_deliver(&mut self, r: &mut Regs, mut v: u32, budget: &mut i64) -> Result<(), Exhausted> {
        loop {
            match r.frames.pop() {
                None => {
                    r.done = Some(v);
                    return Ok(());
                }
                Some(Frame::To(arg)) => {
                    r.f = v;
                    r.x = arg;
                    return Ok(());
                }
                Some(Frame::Res(func)) => {
                    r.f = func;
                    r.x = v;
                    return Ok(());
                }
                Some(Frame::S(b, x)) => {
                    let vs = self.whnf(v, budget)?;
                    if self.k_shaped(vs, budget)? {
                        v = self.fork_right(vs);
                        continue;
                    }
                    let bs = self.whnf(b, budget)?;
                    if self.k_shaped(bs, budget)? {
                        r.f = v;
                        r.x = self.fork_right(bs);
                        return Ok(());
                    }
                    r.frames.push(Frame::Res(v));
                    r.f = b;
                    r.x = x;
                    return Ok(());
                }
            }
        }
    }

    /// m_settle: leaf and stem heads only build, for free, until a fork head or Done.
    fn m_settle(&mut self, r: &mut Regs, budget: &mut i64) -> Result<(), Exhausted> {
        while r.done.is_none() {
            let fs = self.whnf(r.f, budget)?;
            match self.node(fs) {
                Node::Leaf => {
                    let s = self.stem(r.x);
                    self.m_deliver(r, s, budget)?;
                }
                Node::Stem(c) => {
                    let k = self.fork(c, r.x);
                    self.m_deliver(r, k, budget)?;
                }
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    /// m_step: one K, S or triage dispatch on a fork head, then its delivery.
    fn m_step(&mut self, r: &mut Regs, budget: &mut i64) -> Result<(), Exhausted> {
        self.m_settle(r, budget)?;
        if r.done.is_some() {
            return Ok(());
        }
        let fs = self.whnf(r.f, budget)?;
        let (a, b) = match self.node(fs) {
            Node::Fork(a, b) => (a, b),
            _ => unreachable!("settled on a fork head"),
        };
        let av = self.whnf(a, budget)?;
        match self.node(av) {
            Node::Leaf => self.m_deliver(r, b, budget),
            Node::Stem(c) => {
                let cs = self.whnf(c, budget)?;
                if self.k_shaped(cs, budget)? {
                    let v = self.fork_right(cs);
                    let vs = self.whnf(v, budget)?;
                    if self.k_shaped(vs, budget)? {
                        let w = self.fork_right(vs);
                        return self.m_deliver(r, w, budget);
                    }
                    let bs = self.whnf(b, budget)?;
                    if self.k_shaped(bs, budget)? {
                        r.f = v;
                        r.x = self.fork_right(bs);
                        return Ok(());
                    }
                    r.frames.push(Frame::Res(v));
                    r.f = b;
                    return Ok(());
                }
                r.frames.push(Frame::S(b, r.x));
                r.f = c;
                Ok(())
            }
            Node::Fork(w, u) => {
                let xs = self.whnf(r.x, budget)?;
                match self.node(xs) {
                    Node::Leaf => self.m_deliver(r, w, budget),
                    Node::Stem(v) => {
                        r.f = u;
                        r.x = v;
                        Ok(())
                    }
                    Node::Fork(s, t) => {
                        r.frames.push(Frame::To(t));
                        r.f = b;
                        r.x = s;
                        Ok(())
                    }
                    Node::Susp(..) => unreachable!("whnf returns a constructor"),
                }
            }
            Node::Susp(..) => unreachable!("whnf returns a constructor"),
        }
    }

    /// `m_advance state k`: `Some(handle)` of the resulting machine, or `None` when `state`
    /// is not a state the machine built (the caller forces the partial and runs the
    /// definition, so the intercept stays transparent).
    pub(crate) fn step_machine(&mut self, state: u32, k: u32, budget: &mut i64) -> Result<Option<u32>, Exhausted> {
        let tags = self.machine_tags();
        let mut kk = self.whnf(k, budget)?; // is_zero steps
        if matches!(self.node(kk), Node::Leaf) {
            return Ok(Some(state));
        }
        let m = self.whnf(state, budget)?; // m_finished m
        let (mtag, inner) = match self.node(m) {
            Node::Fork(l, r) => (l, r),
            _ => return Ok(None),
        };
        if mtag == tags.done {
            return Ok(Some(state));
        }
        if mtag != tags.run {
            return Ok(None);
        }
        let (app, mut list) = match self.node(inner) {
            Node::Fork(l, r) => (l, r),
            _ => return Ok(None),
        };
        let (f, x) = match self.node(app) {
            Node::Fork(l, r) => (l, r),
            _ => return Ok(None),
        };
        let mut frames: Vec<Frame> = Vec::new();
        loop {
            match self.node(list) {
                Node::Leaf => break,
                Node::Fork(fr, rest) => {
                    let (t, payload) = match self.node(fr) {
                        Node::Fork(l, r) => (l, r),
                        _ => return Ok(None),
                    };
                    if t == tags.to {
                        frames.push(Frame::To(payload));
                    } else if t == tags.res {
                        frames.push(Frame::Res(payload));
                    } else if t == tags.s {
                        match self.node(payload) {
                            Node::Fork(b, xx) => frames.push(Frame::S(b, xx)),
                            _ => return Ok(None),
                        }
                    } else {
                        return Ok(None);
                    }
                    list = rest;
                }
                _ => return Ok(None),
            }
        }
        frames.reverse(); // list head = top of stack = last element
        let mut r = Regs { f, x, frames, done: None };
        // m_advance: is_zero steps, m_finished m, then m_step and nat_pred
        while !matches!(self.node(kk), Node::Leaf) {
            if r.done.is_some() {
                break;
            }
            self.m_step(&mut r, budget)?;
            let pred = match self.node(kk) {
                Node::Stem(c) => c,
                Node::Fork(_, rr) => rr,
                _ => unreachable!("a WHNF non-leaf"),
            };
            kk = self.whnf(pred, budget)?;
        }
        if let Some(v) = r.done {
            return Ok(Some(self.fork(tags.done, v)));
        }
        let mut stack = LEAF_ID;
        for fr in r.frames.iter() {
            let node = match *fr {
                Frame::To(a) => self.fork(tags.to, a),
                Frame::Res(a) => self.fork(tags.res, a),
                Frame::S(b, xx) => {
                    let p = self.fork(b, xx);
                    self.fork(tags.s, p)
                }
            };
            stack = self.fork(node, stack);
        }
        let app = self.fork(r.f, r.x);
        let inner = self.fork(app, stack);
        Ok(Some(self.fork(tags.run, inner)))
    }
}
