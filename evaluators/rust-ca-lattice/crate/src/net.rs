//! The abstract interaction-net engine, driven ENTIRELY by the rule ROM (rules.rs): one
//! generic `fire` interprets templates; there is no per-rule code. Tests validate this
//! engine against the independent oracle, and the lattice co-maintains it as the shadow
//! net for projection checks (CASCADE_CELL_DESIGN.md §9).
//!
//! Thanks to the ≤3-port lowered alphabet, ports are a fixed `[Option<Ref>; 3]`.

use crate::rules::{find_labelled, End, Rule, Tag};

pub type Ref = (u32, u8); // (agent id, port)

#[derive(Clone, Debug)]
pub struct Agent {
    pub tag: Tag,
    /// A superposition's label, or a duplicator's (rules.rs `Lab`); 0 for everything else.
    pub label: u8,
    pub ports: [Option<Ref>; 3],
}

#[derive(Default)]
pub struct Net {
    pub agents: Vec<Option<Agent>>, // tombstoned arena; ids are indices
    pub ints: u64,                  // interaction count (the work ledger)
    /// Each fixed unit's code, by its number (rules.rs `UNIT_RULES`), as `Tree`.
    pub units: Vec<Tree>,
    /// The tree each word holds.
    pub words: std::collections::HashMap<u32, Tree>,
}

/// A value tree as bytes, root first (`L`, `S`, `F`): plain data, so a net can cross threads.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tree(pub Box<[u8]>);

impl Tree {
    pub fn of(t: &crate::oracle::Term) -> Tree { Tree::within(t, usize::MAX).expect("a tree is a value") }
    /// t as a tree, if it is a value of at most n nodes.
    pub fn within(t: &crate::oracle::Term, n: usize) -> Option<Tree> {
        fn go(t: &crate::oracle::Term, v: &mut Vec<u8>, n: usize) -> Option<()> {
            use crate::oracle::Term::*;
            if v.len() >= n { return None; }
            match t { L => v.push(b'L'), S(a) => { v.push(b'S'); go(a, v, n)? } F(a, b) => { v.push(b'F'); go(a, v, n)?; go(b, v, n)? } Ap(..) => return None }
            Some(())
        }
        let mut v = vec![];
        go(t, &mut v, n)?;
        Some(Tree(v.into()))
    }
    pub fn term(&self) -> crate::oracle::Term {
        fn go(b: &[u8], i: &mut usize) -> crate::oracle::Term {
            use crate::oracle::Term::*;
            let c = b[*i];
            *i += 1;
            match c { b'L' => L, b'S' => S(std::rc::Rc::new(go(b, i))), _ => { let a = go(b, i); F(std::rc::Rc::new(a), std::rc::Rc::new(go(b, i))) } }
        }
        go(&self.0, &mut 0)
    }
    /// A word's label: the shape of its tree.
    pub fn shape(&self) -> u8 {
        match self.0[0] { b'L' => crate::rules::SHAPE_L, b'S' => crate::rules::SHAPE_S, _ => crate::rules::SHAPE_F }
    }
    /// Its children, for spelling it out.
    pub fn children(&self) -> Vec<Tree> {
        fn end(b: &[u8], i: usize) -> usize { match b[i] { b'L' => i + 1, b'S' => end(b, i + 1), _ => end(b, end(b, i + 1)) } }
        let b = &self.0;
        match b[0] {
            b'L' => vec![],
            b'S' => vec![Tree(b[1..].into())],
            _ => { let m = end(b, 1); vec![Tree(b[1..m].into()), Tree(b[m..].into())] }
        }
    }
}

impl Net {
    pub fn new() -> Self { Self::default() }

    pub fn mk(&mut self, tag: Tag) -> u32 { self.mk_labelled(tag, 0) }
    pub fn mk_labelled(&mut self, tag: Tag, label: u8) -> u32 {
        self.agents.push(Some(Agent { tag, label, ports: [None; 3] }));
        (self.agents.len() - 1) as u32
    }
    pub fn get(&self, id: u32) -> &Agent { self.agents[id as usize].as_ref().expect("live agent") }
    fn get_mut(&mut self, id: u32) -> &mut Agent { self.agents[id as usize].as_mut().expect("live agent") }

    pub fn link(&mut self, a: u32, i: u8, b: u32, j: u8) {
        self.get_mut(a).ports[i as usize] = Some((b, j));
        self.get_mut(b).ports[j as usize] = Some((a, i));
    }
    fn attach(&mut self, a: u32, i: u8, target: Ref) {
        self.get_mut(a).ports[i as usize] = Some(target);
        self.get_mut(target.0).ports[target.1 as usize] = Some((a, i));
    }
    fn fuse(&mut self, t1: Ref, t2: Ref) {
        self.get_mut(t1.0).ports[t1.1 as usize] = Some(t2);
        self.get_mut(t2.0).ports[t2.1 as usize] = Some(t1);
    }

    pub fn live_count(&self) -> usize { self.agents.iter().flatten().count() }

    /// A word holding t.
    pub fn word(&mut self, t: Tree) -> u32 {
        let w = self.mk_labelled(Tag::Word, t.shape());
        self.words.insert(w, t);
        w
    }

    /// The value a source holds, if it is all values: trees, words and units' code.
    pub fn value(&self, r: Option<Ref>) -> Option<crate::oracle::Term> {
        use crate::oracle::Term;
        use std::rc::Rc;
        let (id, 0) = r? else { return None };
        let a = self.agents.get(id as usize)?.as_ref()?;
        match a.tag {
            Tag::L => Some(Term::L),
            Tag::S => Some(Term::S(Rc::new(self.value(a.ports[1])?))),
            Tag::F => Some(Term::F(Rc::new(self.value(a.ports[1])?), Rc::new(self.value(a.ports[2])?))),
            Tag::Word => self.words.get(&id).map(Tree::term),
            Tag::Ref => self.units.get(a.label as usize).map(Tree::term),
            _ => None,
        }
    }

    /// Wire each request whose arguments are values to an idle unit of its number, as the lattice
    /// does where the two meet. Returns how many docked.
    pub fn dock(&mut self) -> usize {
        let mut n = 0;
        for r in 0..self.agents.len() as u32 {
            let Some(a) = &self.agents[r as usize] else { continue };
            if a.tag != Tag::Req || a.ports[0].is_some() { continue; }
            let (label, pair) = (a.label, self.get(a.ports[1].expect("a request holds its arguments").0).ports);
            if self.value(pair[1]).is_none() || self.value(pair[2]).is_none() { continue; }
            let idle = (0..self.agents.len() as u32).find(|&u| matches!(&self.agents[u as usize], Some(x) if x.tag == Tag::Unit && x.label == label && x.ports[0].is_none()));
            if let Some(u) = idle { self.link(r, 0, u, 0); n += 1; }
        }
        n
    }

    /// `build`, every subtree equal to a unit's code (`units`) loaded as a `Ref` to it.
    pub fn build_units(&mut self, t: &crate::oracle::Term) -> u32 {
        use crate::oracle::Term::*;
        let longest = self.units.iter().map(|c| c.0.len()).max().unwrap_or(0);
        if let Some(tree) = Tree::within(t, longest) {
            if let Some(u) = self.units.iter().position(|c| *c == tree) { return self.mk_labelled(Tag::Ref, u as u8); }
        }
        let (tag, kids): (Tag, Vec<&crate::oracle::Term>) = match t {
            L => (Tag::L, vec![]),
            S(a) => (Tag::S, vec![&**a]),
            F(a, b) => (Tag::F, vec![&**a, &**b]),
            Ap(f, x) => (Tag::P, vec![&**f, &**x]),
        };
        let a = self.mk(tag);
        for (q, k) in kids.into_iter().enumerate() {
            let b = self.build_units(k);
            self.link(a, q as u8 + 1, b, 0);
        }
        a
    }

    /// Build a term as a producer tree (applications become inert `P` suspensions);
    /// returns the root id whose port 0 is the output.
    pub fn build(&mut self, t: &crate::oracle::Term) -> u32 {
        use crate::oracle::Term::*;
        match t {
            L => self.mk(Tag::L),
            S(a) => { let s = self.mk(Tag::S); let c = self.build(a); self.link(s, 1, c, 0); s }
            F(a, b) => {
                let f = self.mk(Tag::F);
                let l = self.build(a); let r = self.build(b);
                self.link(f, 1, l, 0); self.link(f, 2, r, 0); f
            }
            Ap(f, x) => {
                let p = self.mk(Tag::P);
                let ff = self.build(f); let xx = self.build(x);
                self.link(p, 1, ff, 0); self.link(p, 2, xx, 0); p
            }
        }
    }

    /// `build`, with the code in movement agents (rules.rs `VIEW_RULES`): every plain value something is
    /// applied to (the loaded program, each application's head) has its shapes loaded as views
    /// (`view_shape`); arguments are data and stay trees, as does whatever the run builds. A K shape
    /// stays a plain fork: numbers and lists are K shapes all the way down, so a K view would mostly
    /// label data, and code inside a K keeps its own views.
    pub fn build_views(&mut self, t: &crate::oracle::Term) -> u32 {
        use crate::oracle::Term::*;
        fn value(t: &crate::oracle::Term) -> bool { match t { L => true, S(a) => value(a), F(a, b) => value(a) && value(b), Ap(..) => false } }
        fn go(net: &mut Net, t: &crate::oracle::Term, code: bool) -> u32 {
            let (tag, kids): (Tag, Vec<(&crate::oracle::Term, bool)>) = match t {
                Ap(f, x) => (Tag::P, vec![(f, value(f)), (x, false)]),
                F(a, b) if code && **a == L => (Tag::F, vec![(a, true), (b, true)]),
                _ if code => { let (tag, kids) = view_shape(t); (tag, kids.into_iter().map(|k| (k, true)).collect()) }
                L => (Tag::L, vec![]),
                S(a) => (Tag::S, vec![(a, false)]),
                F(a, b) => (Tag::F, vec![(a, false), (b, false)]),
            };
            let a = net.mk(tag);
            for (q, (k, code)) in kids.into_iter().enumerate() {
                let b = go(net, k, code);
                net.link(a, q as u8 + 1, b, 0);
            }
            a
        }
        go(self, t, false)
    }

    /// `build`, every value in movement agents, arguments included: so readers meet views as data too
    /// (the tests' and the GPU check's way to make every view rule fire).
    pub fn build_every_view(&mut self, t: &crate::oracle::Term) -> u32 {
        use crate::oracle::Term::*;
        let (tag, kids): (Tag, Vec<&crate::oracle::Term>) = match t { Ap(f, x) => (Tag::P, vec![&**f, &**x]), _ => view_shape(t) };
        let a = self.mk(tag);
        for (q, k) in kids.into_iter().enumerate() {
            let b = self.build_every_view(k);
            self.link(a, q as u8 + 1, b, 0);
        }
        a
    }

    /// Wrap a built term for normalization: Nrm drives it, Out holds the result.
    /// Returns (nrm, out).
    pub fn drive(&mut self, root: u32) -> (u32, u32) {
        let nrm = self.mk(Tag::Nrm);
        let out = self.mk(Tag::Out);
        self.link(nrm, 0, root, 0);
        self.link(nrm, 1, out, 0);
        (nrm, out)
    }

    /// Find one active pair (consumer id, producer id): a producer whose principal faces a
    /// consumer's principal.
    pub fn active_pair(&self) -> Option<(u32, u32)> {
        for (id, a) in self.agents.iter().enumerate() {
            let Some(a) = a else { continue };
            if !a.tag.is_producer() { continue; }
            let Some((nid, np)) = a.ports[0] else { continue };
            if np != 0 { continue; }
            let n = self.get(nid);
            if n.tag.is_consumer() { return Some((nid, id as u32)); }
        }
        None
    }

    pub fn all_active_pairs(&self) -> Vec<(u32, u32)> {
        let mut out = vec![];
        for (id, a) in self.agents.iter().enumerate() {
            let Some(a) = a else { continue };
            if !a.tag.is_producer() { continue; }
            let Some((nid, np)) = a.ports[0] else { continue };
            if np != 0 { continue; }
            if self.get(nid).tag.is_consumer() { out.push((nid, id as u32)); }
        }
        out
    }

    /// Apply one interaction. Generic over the ROM. Returns the fresh agent ids in
    /// template order (the lattice uses this to sync its shadow ids).
    pub fn fire(&mut self, consumer: u32, producer: u32) -> (&'static Rule, Vec<u32>) {
        let (c, p) = (self.get(consumer), self.get(producer));
        let (ct, pt, cl, pl) = (c.tag, p.tag, c.label, p.label);
        let rule = find_labelled(ct, cl, pt, pl).unwrap_or_else(|| panic!(
            "no rule for {}·{} — reachable-plane invariant violated", ct.name(), pt.name()));
        self.ints += 1;

        // Capture the external far-ends of every aux port BEFORE deleting the pair.
        let cx: Vec<Option<Ref>> = (0..3).map(|i| self.get(consumer).ports[i]).collect();
        let px: Vec<Option<Ref>> = (0..3).map(|i| self.get(producer).ports[i]).collect();
        // What fresh words hold: the producer's tree whole (a copy) or its children (spelled out),
        // or a unit's answer, worked out from the arguments the request's pair holds.
        let held = match pt {
            Tag::Word => self.words.remove(&producer),
            Tag::Ref => self.units.get(pl as usize).cloned(),
            Tag::Unit => {
                let pair = self.get(cx[1].expect("a request holds its arguments").0);
                let arg = |q: usize| self.value(pair.ports[q]).expect("a unit answers only values");
                let call = crate::oracle::ap(crate::oracle::ap(self.units[pl as usize].term(), arg(1)), arg(2));
                Some(Tree::of(&crate::oracle::nf(call, &mut crate::oracle::Fuel(1 << 32)).expect("a unit's answer within its fuel")))
            }
            _ => None,
        };
        self.agents[consumer as usize] = None;
        self.agents[producer as usize] = None;

        let fresh: Vec<u32> = rule.fresh.iter().enumerate().map(|(k, t)| self.mk_labelled(*t, rule.label(k, cl, pl))).collect();
        if let Some(t) = held {
            let words: Vec<u32> = fresh.iter().copied().filter(|&x| self.get(x).tag == Tag::Word).collect();
            let whole = matches!(ct, Tag::Dn | Tag::Nrm | Tag::Req);
            let parts = if whole { vec![t; words.len()] } else { t.children() };
            for (w, t) in words.into_iter().zip(parts) {
                self.get_mut(w).label = t.shape();
                self.words.insert(w, t);
            }
        }

        // A wire endpoint resolves to a fresh port or to an external target. An external
        // target may itself point back INTO the dying pair (a direct aux–aux wire between
        // the two); those chase through the rule's own matching until they exit.
        #[derive(Clone, Copy)]
        enum R { Fresh(u32, u8), Ext(Ref) }
        let partner_of = |e: End| -> End {
            for (a, b) in rule.wires {
                if *a == e { return *b; }
                if *b == e { return *a; }
            }
            unreachable!("validated ROM: every live port is wired")
        };
        let resolve = |start: End| -> R {
            let mut e = start;
            let mut steps = 0;
            loop {
                steps += 1;
                assert!(steps < 64, "wire chase did not exit the dying pair (vicious circle)");
                match e {
                    End::Fresh(k, p) => return R::Fresh(fresh[k as usize], p),
                    End::CAux(i) => {
                        let t = cx[i as usize].expect("closed net: aux wired");
                        if t.0 == consumer { e = partner_of(End::CAux(t.1)); }
                        else if t.0 == producer { e = partner_of(End::PAux(t.1)); }
                        else { return R::Ext(t); }
                    }
                    End::PAux(j) => {
                        let t = px[j as usize].expect("closed net: aux wired");
                        if t.0 == consumer { e = partner_of(End::CAux(t.1)); }
                        else if t.0 == producer { e = partner_of(End::PAux(t.1)); }
                        else { return R::Ext(t); }
                    }
                }
            }
        };

        for (a, b) in rule.wires {
            match (resolve(*a), resolve(*b)) {
                (R::Fresh(x, i), R::Fresh(y, j)) => self.link(x, i, y, j),
                (R::Fresh(x, i), R::Ext(t)) | (R::Ext(t), R::Fresh(x, i)) => self.attach(x, i, t),
                (R::Ext(t1), R::Ext(t2)) => self.fuse(t1, t2),
            }
        }
        (rule, fresh)
    }

    /// Fire until no active pair remains. Returns true when quiescent within budget.
    pub fn reduce(&mut self, budget: u64) -> bool {
        for _ in 0..budget {
            match self.active_pair() {
                Some((c, p)) => { self.fire(c, p); }
                None => return true,
            }
        }
        self.active_pair().is_none()
    }

    /// `reduce`, keeping a list of the agents a rewrite made or rewired instead of looking at every
    /// agent for each one, and telling `each` every rule it fires. Every order fires the same number
    /// of rewrites (interaction nets are strongly confluent), so `ints` is the same as `reduce` gives.
    pub fn reduce_listed(&mut self, budget: u64, mut each: impl FnMut(&'static Rule)) -> bool {
        let mut todo: Vec<u32> = (0..self.agents.len() as u32).rev().collect();
        let start = self.ints;
        while let Some(id) = todo.pop() {
            let Some(a) = &self.agents[id as usize] else { continue };
            let Some((b, 0)) = a.ports[0] else { continue };
            let bt = self.get(b).tag;
            let (c, p) = if a.tag.is_consumer() && bt.is_producer() { (id, b) } else if a.tag.is_producer() && bt.is_consumer() { (b, id) } else { continue };
            if self.ints - start == budget { return false; }
            let near: Vec<u32> = [c, p].iter().flat_map(|&x| self.get(x).ports).flatten().map(|r| r.0).filter(|&x| x != c && x != p).collect();
            let (rule, fresh) = self.fire(c, p);
            each(rule);
            todo.extend(near);
            todo.extend(fresh);
        }
        true
    }

    /// Read the value tree hanging off a ref (an Out's port-0 far end at quiescence).
    pub fn readback(&self, r: Option<Ref>) -> Option<crate::oracle::Term> {
        use crate::oracle::Term;
        let (id, _) = r?;
        let a = self.agents.get(id as usize)?.as_ref()?;
        match a.tag {
            Tag::L => Some(Term::L),
            Tag::S => Some(Term::S(std::rc::Rc::new(self.readback(a.ports[1])?))),
            Tag::F => Some(Term::F(
                std::rc::Rc::new(self.readback(a.ports[1])?),
                std::rc::Rc::new(self.readback(a.ports[2])?),
            )),
            Tag::Word => self.words.get(&id).map(Tree::term),
            _ => None, // a leftover consumer/suspension: not a normal form
        }
    }
}

/// The agent a node of loaded code becomes, and its children: a view where its shape is one (K before
/// the S shapes, B before C, both halves equal before a plain S rule), else its tree node.
pub fn view_shape(t: &crate::oracle::Term) -> (Tag, Vec<&crate::oracle::Term>) {
    use crate::oracle::Term::*;
    match t {
        L => (Tag::L, vec![]),
        S(a) => (Tag::S, vec![&**a]),
        F(a, b) => match (&**a, &**b) {
            (L, y) => (Tag::K, vec![y]),
            (S(s), g) => match (&**s, g) {
                (F(l, f), g) if **l == L => (Tag::B, vec![&**f, g]),
                (s, F(l, g)) if **l == L => (Tag::C, vec![s, &**g]),
                (s, b) if s == b => (Tag::W, vec![s]),
                (s, b) => (Tag::Sx, vec![s, b]),
            },
            (F(l, r), L) if **l == L && **r == L => (Tag::I, vec![]),
            (a, b) => (Tag::F, vec![a, b]),
        },
        Ap(..) => unreachable!("code is a value"),
    }
}

/// One-call convenience for the differentials: build, drive, reduce, read back.
pub fn normalize(term: &crate::oracle::Term, budget: u64) -> (Option<crate::oracle::Term>, bool, u64) {
    let mut net = Net::new();
    let root = net.build(term);
    let (_nrm, out) = net.drive(root);
    let done = net.reduce(budget);
    let res = net.readback(net.get(out).ports[0]);
    (res, done, net.ints)
}
