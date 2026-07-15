//! The abstract interaction-net engine, driven ENTIRELY by the rule ROM (rules.rs): one
//! generic `fire` interprets templates; there is no per-rule code. Tests validate this
//! engine against the independent oracle, and the lattice co-maintains it as the shadow
//! net for projection checks (LOCAL_CA_DESIGN.md §10).
//!
//! Thanks to the ≤3-port lowered alphabet, ports are a fixed `[Option<Ref>; 3]`.

use crate::rules::{find, End, Rule, Tag};

pub type Ref = (u32, u8); // (agent id, port)

#[derive(Clone, Debug)]
pub struct Agent {
    pub tag: Tag,
    pub ports: [Option<Ref>; 3],
}

#[derive(Default)]
pub struct Net {
    pub agents: Vec<Option<Agent>>, // tombstoned arena; ids are indices
    pub ints: u64,                  // interaction count (the work ledger)
}

impl Net {
    pub fn new() -> Self { Self::default() }

    pub fn mk(&mut self, tag: Tag) -> u32 {
        self.agents.push(Some(Agent { tag, ports: [None; 3] }));
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
        let (ct, pt) = (self.get(consumer).tag, self.get(producer).tag);
        let rule = find(ct, pt).unwrap_or_else(|| panic!(
            "no rule for {}·{} — reachable-plane invariant violated", ct.name(), pt.name()));
        self.ints += 1;

        // Capture the external far-ends of every aux port BEFORE deleting the pair.
        let cx: Vec<Option<Ref>> = (0..3).map(|i| self.get(consumer).ports[i]).collect();
        let px: Vec<Option<Ref>> = (0..3).map(|i| self.get(producer).ports[i]).collect();
        self.agents[consumer as usize] = None;
        self.agents[producer as usize] = None;

        let fresh: Vec<u32> = rule.fresh.iter().map(|t| self.mk(*t)).collect();

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
            _ => None, // a leftover consumer/suspension: not a normal form
        }
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
