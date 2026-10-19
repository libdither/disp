//! The substrate (`Params::phys`): the strand lattice run as physics, every rewrite reversible
//! while it is remembered.
//!
//! - **A rewrite keeps a record** of what it used up: the agents as they were, and for a rule that
//!   joins two wires into one (`Unp·Pair`, an un-copy) the two ends it joined. The agents it made carry
//!   the record's number (`made_by`), and the ends of a joined wire carry a mark. (The records sit in
//!   one table under their numbers; nothing looks one up but a move at a site holding an agent that
//!   carries its number.)
//! - **A rewrite can be undone** while everything it made is still there and unused (no agent
//!   consumed, no later join on any of their ports) and lies in one 2×2 square, together with the
//!   near end of each wire it joined: what it made goes, the agents it used come back with their ids,
//!   wired as they were. A kept record ties what it made together (`Params::bond`), so it stays close
//!   enough to be undone. Undoing climbs the drive: a step forward is taken with chance
//!   min(1, e^(ε/T)) and a step back with min(1, e^(-ε/T)) (`Params::drive`), so with ε < 0 the run
//!   goes backward.
//! - **Erasing is a decision.** An eraser takes a value apart as before, but every step of that is a
//!   recorded rewrite like any other, as are the collections (an un-copy, a computation nobody wants,
//!   an unpair both of whose halves are erased): what was taken apart is in the records. At a leaf it
//!   stops. Taking the leaf is the decision (`Params::erase`, a chance each time it tries): it keeps no
//!   record, the leaf is gone for good, and so is every record that only it could have led back
//!   through.
//! - **Forgetting.** A record can also just be forgotten (`Params::forget`, a chance a turn for an
//!   agent that carries one), and so is every record only it could have led back to: what the
//!   forgotten rewrite used up can never come back, so neither can what made that.
//! - **Heat** (`Ledger`): the drive's ε a step forward (less a step back), and a unit for every record
//!   forgotten and every leaf erased. Agents are made from fuel and give it back when unmade, so the
//!   fuel spent is the agents added plus the heat.

use super::{code, Energy, FieldSnap, Lattice, Params, Region, Stats, NONE};
use tree_net::net::{Agent, Ref};
use tree_net::rules::{rule, End, Tag};
use std::collections::HashMap;

/// A record's kind for an un-copy: this plus the copy the holder or eraser was on.
pub const UNCOPY: u16 = 1000;
/// A record's kind for a computation nobody wants, collected by an eraser on its result: erasers go
/// onto its two inputs.
pub const DEAD: u16 = 1010;
/// A record's kind for an unpair erasers read both halves of: one eraser goes onto its input.
pub const UNPAIRED: u16 = 1011;

/// What a rewrite used up and joined, kept so that it can be undone.
#[derive(Clone, Debug)]
pub struct Record {
    /// The rule (`rules::rule`'s number), `UNCOPY` plus the copy erased, `DEAD` or `UNPAIRED`.
    pub kind: u16,
    /// The agents it used up, as they were.
    pub gone: Vec<Was>,
    /// Wires between the agents it used up: (agent, port) pairs, indices into `gone`.
    pub inner: Vec<((u8, u8), (u8, u8))>,
    /// The agents it made: ids `made..made + n`.
    pub made: u32,
    pub n: u8,
    /// Ports of what it made that took over a used-up agent's wire: (made, port) ↔ (gone, port).
    /// Every other port of what it made is wired to another of them.
    pub outer: Vec<((u8, u8), (u8, u8))>,
    /// The wires it joined.
    pub joined: Vec<Joined>,
}

/// A wire a rewrite joined: its two ends, and the ports of the used-up agents that held them (a's,
/// then b's). Undone, the wire is cut at a's site.
#[derive(Clone, Copy, Debug)]
pub struct Joined { pub a: Ref, pub b: Ref, pub pa: (u8, u8), pub pb: (u8, u8) }

/// An agent as it was when a rewrite used it up.
#[derive(Clone, Debug)]
pub struct Was { pub id: u32, pub tag: Tag, pub sup: u8, pub want: bool, pub marks: Vec<(u8, Vec<u64>)> }

/// The energy account.
#[derive(Clone, Debug, Default)]
pub struct Ledger {
    /// Agents when loaded.
    pub loaded: u64,
    /// Heat from the drive: ε a rewrite forward, less ε one undone.
    pub drive: f64,
    /// Leaves erased for good: a unit of heat each.
    pub erased: u64,
    /// Records forgotten: a unit of heat each.
    pub forgot: u64,
    /// Rewrites made (recorded or not) and undone; un-copies kept now.
    pub forward: u64,
    pub back: u64,
    pub uncopied: u64,
}

impl Ledger {
    pub fn heat(&self) -> f64 { self.drive + (self.erased + self.forgot) as f64 }
}

/// The substrate's state beside the lattice's.
#[derive(Default, Clone)]
pub struct Sub {
    /// Records by number, from 1; None once undone or forgotten.
    pub records: Vec<Option<Record>>,
    /// By agent id: the record of the rewrite that made it (0: none).
    pub made_by: Vec<u64>,
    /// Ports that are ends of joined wires, and the records that joined them (latest last).
    pub marks: HashMap<(u32, u8), Vec<u64>>,
    pub ledger: Ledger,
    /// By agent id: the site it was last put in.
    pub seat_of: Vec<u32>,
    /// By site: records of rewrites there that left nothing behind (no agent made, no wire joined, as when
    /// a two-way `Eat` eats a leaf), which no agent's number leads back to, so a turn there looks for them.
    pub bare: HashMap<u32, Vec<u64>>,
}

impl Sub {
    pub(super) fn seat(&mut self, id: u32, s: u32) {
        if self.seat_of.len() <= id as usize { self.seat_of.resize(id as usize + 1, u32::MAX); }
        self.seat_of[id as usize] = s;
    }
}

/// How an undo went: done, not done because what it needs is not in one 2×2 square, or not done.
enum Undo { Done, Far, No }

/// Where a wire of a rebuild ends: a port of a new agent, or an end that stays (region site, end).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum T { New(usize, usize), Stay(usize, u8) }

/// A rebuild of part of a region: agents that go, ends of strands that come free (and the links
/// they free), wires cut in two (an end of each side, both relinked), agents to make and the wires
/// to lay.
#[derive(Default)]
struct Rebuild {
    dying: Vec<(usize, usize)>,
    clear: Vec<(u32, u8)>,
    cut: Vec<(u32, u8)>,
    freed: Vec<(u32, usize)>,
    new: Vec<Was>,
    links: Vec<(T, T)>,
}

impl Lattice {
    /// The live record of what made agent `id`, if any.
    fn maker(&self, id: u32) -> Option<u64> {
        let e = *self.sub.made_by.get(id as usize)?;
        (e != 0 && self.sub.records.get(e as usize).is_some_and(|r| r.is_some())).then_some(e)
    }
    fn alive(&self, id: u32) -> bool { self.shadow.agents.get(id as usize).is_some_and(|a| a.is_some()) }
    fn marked(&self, id: u32) -> bool { (0..self.ar as u8).any(|q| self.sub.marks.get(&(id, q)).is_some_and(|v| !v.is_empty())) }

    /// Agent `id` (at seat i) as it is, its marks taken off the ports.
    fn was(&mut self, id: u32, i: usize) -> Was {
        let a = self.shadow.get(id);
        let (tag, sup) = (a.tag, a.label);
        let marks = (0..self.ar as u8).filter_map(|q| self.sub.marks.remove(&(id, q)).filter(|v| !v.is_empty()).map(|v| (q, v))).collect();
        Was { id, tag, sup, want: self.want[i], marks }
    }

    /// The agents a kept record ties agent `id` to: the others its maker made, and the near ends of
    /// the other wires joined by the latest join on one of its ports.
    fn bonded(&self, id: u32) -> Vec<u32> {
        let mut out = vec![];
        if let Some(e) = self.maker(id) {
            let r = self.sub.records[e as usize].as_ref().unwrap();
            out.extend((r.made..r.made + r.n as u32).filter(|&x| x != id));
        }
        for q in 0..self.ar as u8 {
            let Some(&e) = self.sub.marks.get(&(id, q)).and_then(|v| v.last()) else { continue };
            let Some(Some(r)) = self.sub.records.get(e as usize) else { continue };
            if r.joined.iter().any(|j| j.a == (id, q)) { out.extend(r.joined.iter().map(|j| j.a.0).filter(|&x| x != id)); }
        }
        out
    }

    /// The bond energy's change when agent k of s steps to t: `Params::bond` for every site it moves
    /// away from an agent it is tied to, less for every site it moves toward one.
    pub(super) fn bond_de(&self, s: u32, k: usize, t: u32) -> i32 {
        let id = self.sids[s as usize * self.ks + k];
        let (a, b) = (self.xyz(s), self.xyz(t));
        let d = |x: [i64; 3], y: [i64; 3]| (0..3).map(|i| (x[i] - y[i]).abs()).sum::<i64>();
        let mut de = 0;
        for x in self.bonded(id) {
            let Some(&at) = self.sub.seat_of.get(x as usize) else { continue };
            if at == u32::MAX || !self.alive(x) { continue; }
            let c = self.xyz(at);
            de += (d(b, c) - d(a, c)) as i32;
        }
        de * self.e.bond
    }

    /// Chance a step forward is taken (`forward`) or a step back, by the drive.
    pub(super) fn drive_allows(&mut self, forward: bool) -> bool {
        let x = if forward { self.p.drive } else { -self.p.drive } / self.p.temp.max(1e-9);
        x >= 0.0 || self.unit() < x.exp()
    }

    /// Just before the abstract net fires consumer `c` (seat ic) and producer `p` (seat ip) by rule
    /// ri: the two as they are, and the wires the rule will join (both ends outside the pair).
    pub(super) fn sub_before(&mut self, ri: usize, c: u32, ic: usize, p: u32, ip: usize) -> (Vec<Was>, Vec<Joined>) {
        let r = rule(ri);
        let far = |l: &Self, e: End| -> Option<(Ref, (u8, u8))> {
            let (who, i, q) = match e { End::CAux(i) => (c, 0u8, i), End::PAux(j) => (p, 1u8, j), End::Fresh(..) => return None };
            let t = l.shadow.get(who).ports[q as usize]?;
            (t.0 != c && t.0 != p).then_some((t, (i, q)))
        };
        let mut joined = vec![];
        for &(x, y) in r.wires {
            if let (Some((a, pa)), Some((b, pb))) = (far(self, x), far(self, y)) { joined.push(Joined { a, b, pa, pb }); }
        }
        (vec![self.was(c, ic), self.was(p, ip)], joined)
    }

    /// Just after: keep the record and number what it made; an eraser taking a leaf instead counts the
    /// heat and forgets whatever could only have led back through the two.
    pub(super) fn sub_after(&mut self, ri: usize, gone: Vec<Was>, joined: Vec<Joined>, fresh: &[u32], site: u32) {
        let r = rule(ri);
        let top = fresh.iter().copied().max().map_or(0, |m| m as usize + 1).max(self.shadow.agents.len());
        if self.sub.made_by.len() < top { self.sub.made_by.resize(top, 0); }
        self.sub.ledger.forward += 1;
        if (r.consumer, r.producer) == (Tag::Eps, Tag::L) {
            self.sub.ledger.erased += 1;
            for &x in fresh { self.sub.made_by[x as usize] = 0; }
            self.forget_gone(&gone);
            return;
        }
        let e = self.sub.records.len().max(1) as u64;
        if self.sub.records.is_empty() { self.sub.records.push(None); }
        for &x in fresh { self.sub.made_by[x as usize] = e; }
        for j in &joined { for end in [j.a, j.b] { self.sub.marks.entry((end.0, end.1)).or_default().push(e); } }
        if fresh.is_empty() && joined.is_empty() { self.sub.bare.entry(site).or_default().push(e); }
        let made = fresh.first().copied().unwrap_or(0);
        debug_assert!(fresh.iter().enumerate().all(|(i, &x)| x == made + i as u32), "fresh agents numbered in a row");
        let outer = r.wires.iter().filter_map(|&(x, y)| match (x, y) {
            (End::Fresh(f, q), End::CAux(i)) | (End::CAux(i), End::Fresh(f, q)) => Some(((f, q), (0, i))),
            (End::Fresh(f, q), End::PAux(j)) | (End::PAux(j), End::Fresh(f, q)) => Some(((f, q), (1, j))),
            _ => None,
        }).collect();
        self.sub.records.push(Some(Record { kind: ri as u16, gone, inner: vec![((0, 0), (1, 0))], made, n: fresh.len() as u8, outer, joined }));
        self.sub.ledger.drive += self.p.drive;
    }

    /// Before an un-copy at a duplicator (id d at seat id_) whose copy q a holder or eraser (h at
    /// seat ih) is on: the record, kept once it is done (`sub_collected`).
    pub(super) fn sub_uncopy(&mut self, d: u32, id_: usize, h: u32, ih: usize, q: usize) -> Record {
        let dn = self.shadow.get(d);
        let (a, b) = (dn.ports[0].expect("wired"), dn.ports[3 - q].expect("wired"));
        let gone = vec![self.was(d, id_), self.was(h, ih)];
        Record { kind: UNCOPY + q as u16, gone, inner: vec![((0, q as u8), (1, 0))], made: 0, n: 0, outer: vec![],
                 joined: vec![Joined { a, b, pa: (0, 0), pb: (0, 3 - q as u8) }] }
    }
    /// Keep a collection's record (an un-copy, or a holder collecting something nobody wants), what
    /// it made being `fresh`.
    pub(super) fn sub_collected(&mut self, mut rec: Record, fresh: &[u32]) {
        if self.sub.records.is_empty() { self.sub.records.push(None); }
        let e = self.sub.records.len() as u64;
        for j in &rec.joined { for end in [j.a, j.b] { self.sub.marks.entry((end.0, end.1)).or_default().push(e); } }
        let top = fresh.iter().copied().max().map_or(0, |m| m as usize + 1).max(self.shadow.agents.len());
        if self.sub.made_by.len() < top { self.sub.made_by.resize(top, 0); }
        for &x in fresh { self.sub.made_by[x as usize] = e; }
        rec.made = fresh.first().copied().unwrap_or(0);
        rec.n = fresh.len() as u8;
        if rec.kind >= UNCOPY && rec.kind < DEAD { self.sub.ledger.uncopied += 1; }
        self.sub.records.push(Some(rec));
        self.sub.ledger.forward += 1;
        self.sub.ledger.drive += self.p.drive;
    }
    /// Before an eraser at seat ih (id h) collects the computation (id c, seat ic) whose result it
    /// reads: the record. What it makes is an eraser on each of the computation's two inputs.
    pub(super) fn sub_dead(&mut self, c: u32, ic: usize, h: u32, ih: usize) -> Record {
        let gone = vec![self.was(c, ic), self.was(h, ih)];
        Record { kind: DEAD, gone, inner: vec![((0, 2), (1, 0))], made: 0, n: 0, outer: vec![((0, 0), (0, 0)), ((1, 0), (0, 1))], joined: vec![] }
    }
    /// Before erasers on both halves of an unpair (u at iu; h1 on half q at i1, h2 on the other at i2)
    /// collect it: the record. What it makes is one eraser on its input.
    pub(super) fn sub_unpaired(&mut self, u: u32, iu: usize, h1: u32, i1: usize, h2: u32, i2: usize, q: usize) -> Record {
        let gone = vec![self.was(u, iu), self.was(h1, i1), self.was(h2, i2)];
        Record { kind: UNPAIRED, gone, inner: vec![((0, q as u8), (1, 0)), ((0, 3 - q as u8), (2, 0))], made: 0, n: 0, outer: vec![((0, 0), (0, 0))], joined: vec![] }
    }

    /// These agents are gone for good: forget the records of what made them and of the joins on
    /// their ports.
    fn forget_gone(&mut self, gone: &[Was]) {
        let mut todo: Vec<u64> = vec![];
        for w in gone {
            if let Some(e) = self.maker(w.id) { todo.push(e); }
            for (_, v) in &w.marks { todo.extend(v.iter().copied()); }
        }
        for e in todo { self.forget(e); }
    }

    /// Forget record e, and every record only it could have led back to. Each is a unit of heat.
    pub fn forget(&mut self, e: u64) {
        let mut todo = vec![e];
        while let Some(e) = todo.pop() {
            let Some(rec) = self.sub.records.get_mut(e as usize).and_then(|r| r.take()) else { continue };
            self.sub.ledger.forgot += 1;
            for w in &rec.gone {
                if let Some(m) = self.maker(w.id) { todo.push(m); }
                for (_, v) in &w.marks { todo.extend(v.iter().copied()); }
            }
            for j in &rec.joined {
                for end in [j.a, j.b] {
                    if let Some(v) = self.sub.marks.get_mut(&(end.0, end.1)) {
                        v.retain(|&x| x != e);
                        if v.is_empty() { self.sub.marks.remove(&(end.0, end.1)); }
                    }
                }
            }
        }
    }

    /// The substrate's part of a turn at s: an agent here may forget what made it, and a rewrite one
    /// of them took part in may be undone. True if the turn is used up.
    pub(super) fn sub_turn(&mut self, s: u32) -> bool {
        let here: Vec<u32> = (0..self.p.k).filter(|&k| self.tag(s, k) != 0).map(|k| self.sids[s as usize * self.ks + k]).collect();
        if here.is_empty() && !self.sub.bare.contains_key(&s) { return false; }
        if !here.is_empty() && self.p.forget > 0.0 && self.unit() < self.p.forget {
            let id = here[self.below(here.len())];
            if let Some(e) = self.maker(id) { self.forget(e); }
        }
        if !self.drive_allows(false) { return false; }
        let mut tries: Vec<u64> = vec![];
        if let Some(v) = self.sub.bare.get_mut(&s) {
            let records = &self.sub.records;
            v.retain(|&e| records.get(e as usize).is_some_and(|r| r.is_some()));
            tries.extend(v.iter().copied());
            if v.is_empty() { self.sub.bare.remove(&s); }
        }
        for &id in &here {
            if let Some(e) = self.maker(id) { tries.push(e); }
            for q in 0..self.ar as u8 {
                if let Some(&e) = self.sub.marks.get(&(id, q)).and_then(|v| v.last()) {
                    if self.sub.records.get(e as usize).is_some_and(|r| r.is_some()) { tries.push(e); }
                }
            }
        }
        tries.sort_unstable();
        tries.dedup();
        let mut far = None;
        while !tries.is_empty() {
            let e = tries.swap_remove(self.below(tries.len()));
            match self.undo(s, e) {
                Undo::Done => return true,
                Undo::Far => far = far.or(Some(e)),
                Undo::No => {}
            }
            if self.stale.get() { return true; }
        }
        // What one of them needs is apart: as a reader walks to what it reads going forward, the agent
        // here steps toward the others.
        far.is_some_and(|e| self.gather(s, e))
    }

    /// The agents undoing record e needs to see: what it made, and the near end of each wire it joined.
    fn needs(&self, e: u64) -> Vec<u32> {
        let Some(Some(r)) = self.sub.records.get(e as usize) else { return vec![] };
        (r.made..r.made + r.n as u32).chain(r.joined.iter().map(|j| j.a.0)).collect()
    }

    /// An agent at s that undoing record e needs steps one site toward the middle of the others it
    /// needs, if the energy allows.
    fn gather(&mut self, s: u32, e: u64) -> bool {
        let need = self.needs(e);
        let Some(k) = (0..self.p.k).find(|&k| self.tag(s, k) != 0 && need.contains(&self.sids[s as usize * self.ks + k])) else { return false };
        let id = self.sids[s as usize * self.ks + k];
        let others: Vec<[i64; 3]> = need.iter().filter(|&&x| x != id).filter_map(|&x| self.sub.seat_of.get(x as usize).copied()).filter(|&t| t != u32::MAX).map(|t| self.xyz(t)).collect();
        if others.is_empty() { return false; }
        let a = self.xyz(s);
        let mid: Vec<f64> = (0..3).map(|i| others.iter().map(|o| o[i] as f64).sum::<f64>() / others.len() as f64 - a[i] as f64).collect();
        let i = (0..3).max_by(|&x, &y| mid[x].abs().total_cmp(&mid[y].abs())).unwrap();
        if mid[i].abs() < 0.5 { return false; }
        let f = 2 * i + (mid[i] < 0.0) as usize;
        if f >= self.faces { return false; }
        self.hop(s, k, f, true)
    }

    /// Whether record e could be undone as things are: what it made is all there and unused, and
    /// the wires it joined still join the same ends with nothing joined on them since.
    pub fn undoable(&self, e: u64) -> bool {
        let Some(Some(rec)) = self.sub.records.get(e as usize) else { return false };
        (rec.made..rec.made + rec.n as u32).all(|x| self.alive(x) && !self.marked(x))
            && rec.joined.iter().all(|j| self.alive(j.a.0) && self.alive(j.b.0) && self.shadow.get(j.a.0).ports[j.a.1 as usize] == Some(j.b)
                && [j.a, j.b].iter().all(|end| self.sub.marks.get(&(end.0, end.1)).and_then(|v| v.last()) == Some(&e)))
            && rec.gone.iter().all(|w| !self.alive(w.id))
    }

    /// Undo record e, if it can be undone in a 2×2 square at s.
    fn undo(&mut self, s: u32, e: u64) -> Undo {
        if !self.undoable(e) { return Undo::No; }
        let rec = self.sub.records[e as usize].clone().unwrap();
        let products: Vec<u32> = (rec.made..rec.made + rec.n as u32).collect();
        let mut wanted: Vec<u32> = products.clone();
        wanted.extend(rec.joined.iter().map(|j| j.a.0));
        // The whole 2×2×2 block the turn is in, where moves are confined to blocks; else the 2×2 squares at s.
        let regions = match self.block_region(s) { Some(r) => vec![r], None => self.regions(s, s) };
        if self.stale.get() { return Undo::No; }
        let (mut near, mut full) = (false, false);
        'region: for region in regions {
            // The seat of every agent the undo has to see, in this square.
            let mut seat: HashMap<u32, (usize, usize)> = HashMap::new();
            for (l, &x) in region.sites.iter().enumerate() {
                if x == u32::MAX { continue; }
                for k in 0..self.p.k { if self.tag(x, k) != 0 { let id = self.sids[x as usize * self.ks + k]; if wanted.contains(&id) { seat.insert(id, (l, k)); } } }
            }
            if !wanted.iter().all(|id| seat.contains_key(id)) { continue; }
            near = true;
            let mut plan = Rebuild { new: rec.gone.clone(), ..Default::default() };
            for &((a, p), (b, q)) in &rec.inner { plan.links.push((T::New(a as usize, p as usize), T::New(b as usize, q as usize))); }
            // Shadow wiring to make: (gone index, port) ↔ an outside ref, or another gone port.
            let mut outside: Vec<((u8, u8), Ref)> = vec![];
            let mut loops: Vec<((u8, u8), (u8, u8))> = vec![];
            if !products.is_empty() {
                let lhs = |f: usize, q: usize| -> Option<(u8, u8)> { rec.outer.iter().find(|(m, _)| (m.0 as usize, m.1 as usize) == (f, q)).map(|(_, g)| *g) };
                let mut seen: Vec<(u32, u8)> = vec![];
                for (f, &x) in products.iter().enumerate() {
                    let (l, k) = seat[&x];
                    let site = region.sites[l];
                    for q in 0..self.shadow.get(x).tag.arity() {
                        if seen.contains(&(x, q as u8)) { continue; }
                        let far = self.shadow.get(x).ports[q].expect("wired");
                        if let Some(g) = products.iter().position(|&y| y == far.0) {
                            seen.push((far.0, far.1));
                            // A wire between two of the products: it goes, strands and all.
                            let Some(path) = self.path(site, self.ae(k, q), &region) else { continue 'region };
                            for (x0, e0, f0) in path { plan.clear.push((x0, e0)); if let Some(f0) = f0 { plan.freed.push((x0, f0)); } }
                            match (lhs(f, q), lhs(g, far.1 as usize)) {
                                (Some(a), Some(b)) => { plan.links.push((T::New(a.0 as usize, a.1 as usize), T::New(b.0 as usize, b.1 as usize))); loops.push((a, b)); }
                                (None, None) => {}
                                _ => continue 'region,
                            }
                        } else {
                            let Some(a) = lhs(f, q) else { continue 'region };
                            plan.links.push((T::New(a.0 as usize, a.1 as usize), T::Stay(l, self.mate_of(site, self.ae(k, q)))));
                            outside.push((a, far));
                        }
                    }
                    plan.dying.push((l, k));
                }
            }
            for j in &rec.joined {
                let (l, k) = seat[&j.a.0];
                let site = region.sites[l];
                let ea = self.ae(k, j.a.1 as usize);
                let m = self.mate_of(site, ea);
                if [ea, m].iter().any(|&x| plan.clear.contains(&(site, x)) || plan.cut.contains(&(site, x))) { continue 'region; }
                plan.cut.push((site, ea));
                plan.links.push((T::New(j.pa.0 as usize, j.pa.1 as usize), T::Stay(l, ea)));
                plan.links.push((T::New(j.pb.0 as usize, j.pb.1 as usize), T::Stay(l, m)));
                outside.push((j.pa, j.a));
                outside.push((j.pb, j.b));
            }
            if !self.rebuild(&region, plan) { full = true; continue; }
            // The abstract net: what was made goes, what was used up comes back, wired as it was.
            for &x in &products { self.shadow.agents[x as usize] = None; }
            for w in &rec.gone { self.shadow.agents[w.id as usize] = Some(Agent { tag: w.tag, label: w.sup, ports: [None; 4] }); }
            let id = |g: (u8, u8)| rec.gone[g.0 as usize].id;
            for &(a, b) in &rec.inner { self.shadow.link(id(a), a.1, id(b), b.1); }
            for &(a, b) in &loops { self.shadow.link(id(a), a.1, id(b), b.1); }
            for &(a, far) in &outside { self.shadow.link(id(a), a.1, far.0, far.1); }
            for w in &rec.gone { for (q, v) in &w.marks { self.sub.marks.insert((w.id, *q), v.clone()); } }
            for j in &rec.joined {
                for end in [j.a, j.b] {
                    if let Some(v) = self.sub.marks.get_mut(&(end.0, end.1)) {
                        v.pop();
                        if v.is_empty() { self.sub.marks.remove(&(end.0, end.1)); }
                    }
                }
            }
            self.sub.records[e as usize] = None;
            self.sub.ledger.back += 1;
            self.sub.ledger.drive -= self.p.drive;
            if rec.kind >= UNCOPY && rec.kind < DEAD { self.sub.ledger.uncopied -= 1; }
            self.stats.unfires += 1;
            self.last_op = "undo";
            let at = wanted.first().map_or(s, |w| region.sites[seat[w].0]);
            self.note([super::EV_UNDO, s, at, code(rec.gone[0].tag) as u32, code(rec.gone[1].tag) as u32, rec.kind as u32, 0, 0]);
            if self.fire_log.len() < 4096 { self.fire_log.push(s | 1 << 31); }
            return Undo::Done;
        }
        if !near { self.stats.undo_far += 1; return Undo::Far; }
        if full { self.stats.undo_full += 1; }
        Undo::No
    }

    /// The current block (`Lattice::fence`) as a region, s first, with a route between any two of its
    /// sites (x, then y, then z); None without blocks, or if a move this clock has changed one of them.
    fn block_region(&self, s: u32) -> Option<Region> {
        let c = self.fence?;
        let mut sites = vec![s];
        for q in 0..8 { if let Some(x) = self.block_site(c, q) { if x != s { sites.push(x); } } }
        if !sites.iter().all(|&x| self.inside(x)) { return None; }
        let n = sites.len();
        let mut route = vec![vec![vec![]; n]; n];
        for a in 0..n {
            for b in 0..n {
                let (mut at, to) = (self.xyz(sites[a]), self.xyz(sites[b]));
                let mut x = sites[a];
                for i in 0..3 {
                    while at[i] != to[i] {
                        let f = 2 * i + (to[i] < at[i]) as usize;
                        route[a][b].push((x, f));
                        x = self.nb(x, f);
                        at[i] += if to[i] < at[i] { -1 } else { 1 };
                    }
                }
            }
        }
        Some(Region { sites, route })
    }

    /// The wire from end e of site s, strand by strand, to the agent port at its other end: each
    /// end it passes (site, end, the face of the strand leaving there if it is one going on). None
    /// if it leaves the region.
    fn path(&self, mut s: u32, e: u8, region: &Region) -> Option<Vec<(u32, u8, Option<usize>)>> {
        let mut out = vec![];
        let mut m = self.mate_of(s, e);
        while self.is_strand(m) {
            let (f, i) = (self.face(m), self.lane(m));
            out.push((s, m, Some(f)));
            s = self.nb(s, f);
            if !region.sites.contains(&s) { return None; }
            let back = self.se(f ^ 1, i);
            out.push((s, back, None));
            m = self.mate_of(s, back);
        }
        Some(out)
    }

    /// Seat the plan's new agents in the region's free slots and the dying agents' slots, and lay
    /// its wires along the region's routes, if the lanes and the switchboards have room. All or
    /// nothing.
    fn rebuild(&mut self, region: &Region, plan: Rebuild) -> bool {
        let nloc = region.sites.len();
        let n = plan.new.len();
        let link_key = |site: u32, f: usize, nb: u32| if site < nb { (site, f) } else { (nb, f ^ 1) };
        let mut slots_at: Vec<Vec<usize>> = region.sites.iter().map(|&x| {
            if x == u32::MAX { vec![] } else { (0..self.p.k).filter(|&k| self.tag(x, k) == 0).collect() }
        }).collect();
        for &(l, k) in &plan.dying { slots_at[l].insert(0, k); }
        let mut avail: HashMap<(u32, usize), usize> = HashMap::new();
        for a in 0..nloc {
            for b in 0..nloc {
                for &(x, f) in &region.route[a][b] {
                    let y = self.nb(x, f);
                    avail.entry(link_key(x, f, y)).or_insert_with(|| (0..self.p.lanes).filter(|&i| self.mate_of(x, self.se(f, i)) == NONE).count());
                }
            }
        }
        for &(x, f) in &plan.freed {
            let y = self.nb(x, f);
            let key = link_key(x, f, y);
            let free = (0..self.p.lanes).filter(|&i| self.mate_of(x, self.se(f, i)) == NONE).count();
            *avail.entry(key).or_insert(free) += 1;
        }
        // Pairings each site keeps once the dying agents and cleared ends are gone.
        let mut lost: Vec<(u32, u8, u8)> = vec![];
        let mut lose = |l: &Self, x: u32, e: u8| { let m = l.mate_of(x, e); if m != NONE && !lost.contains(&(x, e.min(m), e.max(m))) { lost.push((x, e.min(m), e.max(m))); } };
        for &(l, k) in &plan.dying { for q in 0..self.ar { lose(self, region.sites[l], self.ae(k, q)); } }
        for &(x, e) in plan.clear.iter().chain(&plan.cut) { lose(self, x, e); }
        let left: Vec<usize> = region.sites.iter().map(|&x| if x == u32::MAX || self.p.pairs == 0 { 0 } else { self.pairs(x) - lost.iter().filter(|l| l.0 == x).count() }).collect();
        let at = |t: T, loc: &[usize]| match t { T::New(i, _) => loc[i], T::Stay(l, _) => l };
        let need_of = |loc: &[usize]| -> HashMap<(u32, usize), usize> {
            let mut need = HashMap::new();
            for &(a, b) in &plan.links {
                let (la, lb) = (at(a, loc), at(b, loc));
                if la == lb { continue; }
                for &(x, f) in &region.route[la][lb] { *need.entry(link_key(x, f, self.nb(x, f))).or_insert(0) += 1; }
            }
            need
        };
        let fits = |loc: &[usize]| -> bool {
            if self.p.pairs == 0 { return true; }
            let mut add = vec![0usize; nloc];
            for &(a, b) in &plan.links {
                let (la, lb) = (at(a, loc), at(b, loc));
                for &(x, _) in &region.route[la][lb] { add[region.sites.iter().position(|&y| y == x).unwrap()] += 1; }
                add[lb] += 1;
            }
            (0..nloc).all(|l| left[l] + add[l] <= self.p.pairs)
        };
        let mut best: Option<(usize, Vec<usize>)> = None;
        let mut loc = vec![0usize; n];
        let mut used = vec![0usize; nloc];
        fn search(i: usize, n: usize, nloc: usize, loc: &mut Vec<usize>, used: &mut Vec<usize>, slots_at: &[Vec<usize>],
                  check: &dyn Fn(&[usize]) -> Option<usize>, best: &mut Option<(usize, Vec<usize>)>) {
            if i == n {
                if let Some(cost) = check(loc) { if best.as_ref().map_or(true, |(c, _)| cost < *c) { *best = Some((cost, loc.clone())); } }
                return;
            }
            for l in 0..nloc {
                if used[l] < slots_at[l].len() {
                    used[l] += 1;
                    loc[i] = l;
                    search(i + 1, n, nloc, loc, used, slots_at, check, best);
                    used[l] -= 1;
                    if best.as_ref().is_some_and(|(c, _)| *c == 0) { return; }
                }
            }
        }
        let check = |loc: &[usize]| -> Option<usize> {
            let need = need_of(loc);
            (need.iter().all(|(k, &v)| v <= *avail.get(k).unwrap_or(&0)) && fits(loc)).then(|| need.values().sum())
        };
        search(0, n, nloc, &mut loc, &mut used, &slots_at, &check, &mut best);
        let Some((_, loc)) = best else { return false };
        let need = need_of(&loc);
        // Clear, remove, place, link.
        for &(x, e) in &plan.clear { self.set(x, e, NONE); }
        for _ in &plan.freed { self.strand_count(-1); }
        for &(l, k) in &plan.dying {
            let x = region.sites[l];
            for q in 0..self.ar { self.set(x, self.ae(k, q), NONE); }
            self.remove(x, k);
        }
        let mut lanes: HashMap<(u32, usize), Vec<usize>> = HashMap::new();
        for (&(x, f), &v) in &need { lanes.insert((x, f), self.free_lanes(x, f, v).expect("checked")); }
        let mut taken = vec![0usize; nloc];
        let seats: Vec<(usize, usize)> = loc.iter().map(|&l| { let k = slots_at[l][taken[l]]; taken[l] += 1; (l, k) }).collect();
        for (w, &(l, k)) in plan.new.iter().zip(&seats) {
            let x = region.sites[l];
            self.place(x, k, code(w.tag), w.id);
            let i = x as usize * self.ks + k;
            self.sup_label[i] = w.sup;
            self.want[i] = w.want;
        }
        for &(a, b) in &plan.links {
            let end_at = |t: T| match t { T::New(i, q) => (seats[i].0, (seats[i].1 * self.ar + q) as u8), T::Stay(l, m) => (l, m) };
            let ((la, ea), (lb, eb)) = (end_at(a), end_at(b));
            if la == lb { self.link(region.sites[la], ea, eb); continue; }
            let mut cur = ea;
            let mut site = region.sites[la];
            for &(x, f) in &region.route[la][lb] {
                let y = self.nb(x, f);
                let i = lanes.get_mut(&link_key(x, f, y)).unwrap().pop().unwrap();
                self.link(x, cur, self.se(f, i));
                self.strand_count(1);
                site = y;
                cur = self.se(f ^ 1, i);
            }
            self.link(site, cur, eb);
        }
        for &x in &region.sites { if x != u32::MAX { self.refresh(x); } }
        true
    }

    /// Seats of agents a kept record ties together, two by two: each agent a rewrite made with the next
    /// it made, and the near ends of the wires one join made (the player draws them).
    pub fn bonds(&self) -> Vec<u32> {
        let seat = |x: u32| -> Option<u32> {
            let s = *self.sub.seat_of.get(x as usize)?;
            if s == u32::MAX { return None; }
            (0..self.ks).find(|&k| self.tags[s as usize * self.ks + k] != 0 && self.sids[s as usize * self.ks + k] == x).map(|k| s * self.ks as u32 + k as u32)
        };
        let mut out = vec![];
        for r in self.sub.records.iter().flatten() {
            let ids: Vec<u32> = (r.made..r.made + r.n as u32).chain(r.joined.iter().map(|j| j.a.0)).filter(|&x| self.alive(x)).collect();
            for w in ids.windows(2) { if let (Some(a), Some(b)) = (seat(w[0]), seat(w[1])) { out.extend([a, b]); } }
        }
        out
    }

    /// Records kept now.
    pub fn records_kept(&self) -> usize { self.sub.records.iter().filter(|r| r.is_some()).count() }
    /// Erasers now: each holds what is left of something dropped, a leaf once it is down to one.
    pub fn erasers(&self) -> usize { self.live().iter().map(|&s| (0..self.ks).filter(|&k| self.tag(s, k) == code(Tag::Eps)).count()).sum() }

    /// For each record kept: its kind, whether it could be undone as things are, and where what it
    /// made (or the near end of each wire it joined) sits, as lattice coordinates.
    pub fn records_report(&self) -> Vec<(u64, u16, bool, Vec<[i64; 3]>)> {
        let mut at: HashMap<u32, u32> = HashMap::new();
        for &s in self.live() { for k in 0..self.ks { if self.tag(s, k) != 0 { at.insert(self.sids[s as usize * self.ks + k], s); } } }
        self.sub.records.iter().enumerate().filter_map(|(e, r)| {
            let r = r.as_ref()?;
            let ids: Vec<u32> = (r.made..r.made + r.n as u32).chain(r.joined.iter().map(|j| j.a.0)).collect();
            Some((e as u64, r.kind, self.undoable(e as u64), ids.iter().filter_map(|i| at.get(i).map(|&s| self.xyz(s))).collect()))
        }).collect()
    }

    /// Whether the abstract net is exactly `net` (same agents by id, tags, labels and wires).
    pub fn same_net(&self, net: &[Option<Agent>]) -> bool {
        let n = self.shadow.agents.len().max(net.len());
        (0..n).all(|i| match (self.shadow.agents.get(i).and_then(|a| a.as_ref()), net.get(i).and_then(|a| a.as_ref())) {
            (None, None) => true,
            (Some(a), Some(b)) => a.tag == b.tag && a.label == b.label && a.ports == b.ports,
            _ => false,
        })
    }
}

/// The whole run as it is at a clock, ids and records included, to put back exactly (the player's
/// saved states, under the substrate: putting sites back as `Lattice::put_sites` does numbers agents
/// afresh, which the records would not survive).
pub struct Snap {
    p: Params,
    stats: Stats,
    cooling: Option<(f64, f64)>,
    rng: u64,
    next_pulse: f64,
    sites: Vec<u32>,
    mate: Vec<u8>,
    seats: Vec<(u8, u32, bool, u8)>,
    pulse: Vec<u8>,
    press: Vec<u8>,
    shadow: Vec<Option<Agent>>,
    ints: u64,
    born: Vec<u64>,
    sub: Sub,
    fields: FieldSnap,
}

impl Snap {
    /// About how many bytes it takes.
    pub fn bytes(&self) -> usize {
        self.mate.len() + self.seats.len() * 12 + self.sites.len() * 6 + self.shadow.len() * 32 + self.born.len() * 8
            + self.sub.made_by.len() * 8 + self.sub.seat_of.len() * 4 + self.sub.records.iter().flatten().map(|r| 64 + r.gone.len() * 48).sum::<usize>()
            + self.sub.marks.len() * 40
    }
}

impl Lattice {
    pub fn snap(&self) -> Snap {
        let sites = self.live.clone();
        let mut mate = Vec::with_capacity(sites.len() * self.ends);
        let mut seats = Vec::with_capacity(sites.len() * self.ks);
        for &x in &sites {
            mate.extend_from_slice(&self.mate[x as usize * self.ends..(x as usize + 1) * self.ends]);
            for k in 0..self.ks { let i = x as usize * self.ks + k; seats.push((self.tags[i], self.sids[i], self.want[i], self.sup_label[i])); }
        }
        Snap { p: self.p, stats: self.stats.clone(), cooling: self.cooling, rng: self.rng, next_pulse: self.next_pulse,
               pulse: sites.iter().map(|&x| self.pulse_at[x as usize]).collect(), press: sites.iter().map(|&x| self.press[x as usize]).collect(),
               sites, mate, seats, shadow: self.shadow.agents.clone(), ints: self.shadow.ints, born: self.born.clone(), sub: self.sub.clone(),
               fields: self.fields.snap() }
    }

    /// Become exactly the run `snap` was, then take the settings the schedule says for its clock.
    pub fn put_snap(&mut self, v: &Snap) {
        for x in self.live.clone() {
            let (m, a) = (x as usize * self.ends, x as usize * self.ks);
            self.mate[m..m + self.ends].fill(NONE);
            for i in a..a + self.ks { self.tags[i] = 0; self.sids[i] = u32::MAX; self.want[i] = false; self.sup_label[i] = 0; }
            self.pulse_at[x as usize] = NONE;
            self.press[x as usize] = 0;
            self.occ[x as usize] = 0;
            self.in_agents[x as usize] = false;
            self.refresh(x);
        }
        for (j, &x) in v.sites.iter().enumerate() {
            let (m, a) = (x as usize * self.ends, x as usize * self.ks);
            self.mate[m..m + self.ends].copy_from_slice(&v.mate[j * self.ends..(j + 1) * self.ends]);
            let mut occ = 0;
            for k in 0..self.ks {
                let (t, sid, w, sl) = v.seats[j * self.ks + k];
                self.tags[a + k] = t; self.sids[a + k] = sid; self.want[a + k] = w; self.sup_label[a + k] = sl;
                occ += (t != 0) as u8;
            }
            self.occ[x as usize] = occ;
            self.in_agents[x as usize] = occ > 0;
            self.pulse_at[x as usize] = v.pulse[j];
            self.press[x as usize] = v.press[j];
            self.refresh(x);
        }
        self.agent_sites = v.sites.iter().copied().filter(|&x| self.occ[x as usize] > 0).collect();
        self.pulse_sites = v.sites.iter().copied().filter(|&x| self.pulse_at[x as usize] != NONE).collect();
        self.shadow.agents = v.shadow.clone();
        self.shadow.ints = v.ints;
        self.born = v.born.clone();
        self.sub = v.sub.clone();
        self.fields.put(&v.fields);
        self.stats = v.stats.clone();
        self.cooling = v.cooling;
        self.rng = v.rng;
        self.next_pulse = v.next_pulse;
        self.p = v.p;
        self.e = Energy::new(&self.p);
        self.events.clear();
        self.fire_log.clear();
        if let Some(&(_, x)) = self.schedule.iter().rev().find(|e| e.0 <= self.stats.clocks) { self.take(x); }
    }

    fn take(&mut self, [drive, erase, forget, bond]: [f64; 4]) {
        self.p.erase = erase;
        self.p.forget = forget;
        self.p.bond = bond;
        self.p.drive = f64::NAN;
        self.set_drive(drive);
    }

    /// Change the substrate's settings from this clock on: what the schedule had for later clocks goes.
    pub fn reset_schedule(&mut self, x: [f64; 4]) {
        let c = self.stats.clocks;
        self.schedule.retain(|e| e.0 < c);
        self.schedule.push((c, x));
        self.take(x);
    }

    /// As a clock starts: the settings the schedule has for it.
    pub(super) fn scheduled(&mut self) {
        let c = self.stats.clocks;
        if let Some(&(_, x)) = self.schedule.iter().find(|e| e.0 == c) {
            if [self.p.drive, self.p.erase, self.p.forget, self.p.bond] != x { self.take(x); }
        }
    }
}
