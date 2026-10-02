//! The protocol a tile runs. Every handler is a method on [`Tile`], which holds mutable
//! references to one tile's slots and queues and read-only views of its neighbourhood's
//! free-space field, and nothing else. So "a tile only ever changes itself and talks by
//! messages" is not a convention here: there is no way to write a handler that breaks it.
//! That is the hardware contract, and it is what lets the mesh step tiles in parallel and
//! still get the same answer, tick for tick, as on one thread.

use crate::mesh::*;
use crate::polarity::{is_source, role, Role};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::rules::{find_index, End, Tag, RULES};
use std::collections::VecDeque;

/// What a tile's handlers produced this tick beyond the tile itself: counter deltas, free
/// space that changed, and fires for the recorder. Merged after the phase in any order.
#[derive(Default)]
pub(crate) struct Out {
    pub fires: u64,
    pub cancels: u64,
    pub direct_ships: u64,
    pub events: u64,
    pub local_fires: u64,
    pub sent: [u64; N_KINDS],
    pub live: i64,
    pub inds: i64,
    pub reserved: i64,
    pub in_flight: i64,
    pub parked: i64,
    pub max_outbox: u64,
    pub max_events: u64,
    pub max_reserve_hops: u64,
    pub phi_dirty: Vec<u32>,
    pub record: bool,
    pub rec_fires: Vec<[u32; 2]>,
}

/// A producer that has reached the consumer which wants it.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Docked {
    pub tag: u8,
    pub aux: [u32; 2],
    pub sid: u32,
}

fn rule_index(consumer: u8, producer: u8) -> usize {
    let (ct, pt) = (tag_of(consumer), tag_of(producer));
    find_index(ct, pt).unwrap_or_else(|| panic!("no rule {}·{}", ct.name(), pt.name()))
}

/// Readers that are hungry from birth: the normalizer drives the answer, the root holds it,
/// and an eraser only ever drops.
#[inline]
pub(crate) fn eager(t: Tag) -> bool { matches!(t, Tag::Nrm | Tag::Eps | Tag::Out) }

/// One tile, as its handlers see it.
pub(crate) struct Tile<'a> {
    pub c: u32,
    pub k: u32,
    pub speculate: u32,
    pub slots: &'a mut [Slot],
    pub free: &'a mut u8,
    pub rr: &'a mut u8,
    pub outbox: &'a mut VecDeque<Flit>,
    pub events: &'a mut VecDeque<Ev>,
    pub parked: &'a mut Vec<Flit>,
    pub nb: &'a [u32; 4],
    pub phi: &'a [[u8; FIELDS]],
    pub shadow: Option<&'a mut Net>,
    pub out: &'a mut Out,
}

struct Plan {
    /// Result ports that stay behind as indirections.
    linger: u8,
    direct: u8,
    slots: usize,
    /// The fresh agent the consumer's own slot hosts, with its port permutation
    /// (`usize::MAX`: whichever fresh agent comes first, unpermuted).
    host: Option<(usize, u8)>,
}

impl Plan {
    fn hosts(&self) -> usize { self.host.is_some() as usize }
}

impl<'a> Tile<'a> {
    /// One tick of the tile: advance the router's round-robin pointer and handle up to `n`
    /// queued events.
    pub fn run(&mut self, n: u32) {
        *self.rr = if *self.rr == 4 { 0 } else { *self.rr + 1 };
        for _ in 0..n {
            let Some(e) = self.events.pop_front() else { break };
            self.out.events += 1;
            match e {
                Ev::Msg(f) => {
                    self.out.in_flight -= 1;
                    self.handle(f);
                }
                Ev::Activate(s) => self.activate(s),
            }
        }
    }

    fn emit(&mut self, f: Flit) {
        self.out.sent[f.kind as usize] += 1;
        self.out.in_flight += 1;
        if a_cell(f.dst) == self.c {
            self.push_event(Ev::Msg(f));
        } else {
            self.outbox.push_back(f);
            self.out.max_outbox = self.out.max_outbox.max(self.outbox.len() as u64);
        }
    }

    pub fn push_event(&mut self, e: Ev) {
        self.events.push_back(e);
        self.out.max_events = self.out.max_events.max(self.events.len() as u64);
    }

    // ---- slot bookkeeping -------------------------------------------------------------

    fn set_free(&mut self, s: u32) {
        let was = self.slots[s as usize].kind;
        if was == FREE { return; }
        if is_agent(was) { self.out.live -= 1; }
        if was == IND { self.out.inds -= 1; }
        if was == RESV { self.out.reserved -= 1; }
        self.slots[s as usize] = Slot::EMPTY;
        *self.free += 1;
        self.out.phi_dirty.push(self.c);
        self.wake_parked();
    }

    /// Parked reservations retry as ordinary events, never inline: a wake can happen in the
    /// middle of a rewrite that is about to use the slot that just came free.
    pub fn wake_parked(&mut self) {
        while let Some(f) = self.parked.pop() {
            self.out.parked -= 1;
            self.push_event(Ev::Msg(f));
        }
    }

    pub fn place(&mut self, s: u32, slot: Slot) {
        let was = self.slots[s as usize].kind;
        debug_assert!(was == FREE || was == RESV, "placing over a live slot");
        if was == FREE {
            *self.free -= 1;
            self.out.phi_dirty.push(self.c);
        }
        if was == RESV { self.out.reserved -= 1; }
        if is_agent(slot.kind) { self.out.live += 1; }
        if slot.kind == IND { self.out.inds += 1; }
        if slot.kind == RESV { self.out.reserved += 1; }
        self.slots[s as usize] = slot;
    }

    /// Reserve `n` free slots of this tile, returning their mask.
    fn reserve_local(&mut self, n: u32) -> u16 {
        let mut mask = 0u16;
        let mut got = 0;
        for s in 0..self.k {
            if got == n { break; }
            if self.slots[s as usize].kind == FREE {
                self.place(s, Slot { kind: RESV, ..Slot::EMPTY });
                mask |= 1 << s;
                got += 1;
            }
        }
        debug_assert_eq!(got, n);
        mask
    }

    // ---- handlers ---------------------------------------------------------------------

    fn handle(&mut self, f: Flit) {
        match f.kind {
            PULL => self.on_pull(f),
            SHIP => self.on_ship(f),
            MOVED => self.on_moved(f),
            SPAWN => self.on_spawn(f),
            DROP => self.on_drop(f),
            RESERVE => self.on_reserve(f),
            GRANT => self.on_grant(f),
            RELEASE => self.on_release(f),
            k => panic!("unknown message kind {k}"),
        }
    }

    /// A reader starts reading: pull its source, or (an eraser) drop it.
    pub fn activate(&mut self, s: u32) {
        let slot = self.slots[s as usize];
        if slot.st != IDLE || !is_agent(slot.kind) { return; }
        let t = tag_of(slot.kind);
        let r = slot.p[0];
        if t == Tag::Eps {
            let mut f = Flit::new(DROP, r);
            f.sid = slot.sid;
            self.set_free(s);
            self.emit(f);
            return;
        }
        self.slots[s as usize].st = AWAIT;
        let mut f = Flit::new(PULL, r);
        f.x = addr(self.c, s, 0) | if t == Tag::Out { 0 } else { SHIPPABLE };
        self.emit(f);
    }

    fn speculative(&self) -> bool { self.speculate > 0 && *self.free as u32 >= self.speculate }

    /// A consumer just took a slot: start it if it is wanted, cancel it if nobody can read
    /// it, and otherwise let it wait for demand.
    fn settle(&mut self, s: u32) {
        let sl = self.slots[s as usize];
        let t = tag_of(sl.kind);
        let (mut any_sub, mut all_dead, mut n) = (false, true, 0);
        for k in (1..t.arity()).filter(|&k| is_source(t, k)) {
            n += 1;
            any_sub |= is_sub(sl.p[k]);
            all_dead &= is_dead(sl.p[k]);
        }
        if eager(t) || any_sub || self.speculative() {
            self.activate(s);
        } else if n > 0 && all_dead {
            self.cancel(s);
        }
    }

    fn on_pull(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        let slot = self.slots[s as usize];
        let k = logical(slot.perm, a_port(f.dst));
        match slot.kind {
            IND => {
                assert!(slot.st & (1 << k) != 0, "pull at a spent indirection port");
                let mut m = Flit::new(MOVED, f.x & ADDR);
                m.x = slot.p[k];
                self.emit(m);
                self.spend_ind(s, k);
            }
            RESV => {
                assert_eq!(slot.p[k], NONE, "two readers pulled one reserved source");
                self.slots[s as usize].p[k] = f.x;
            }
            kind if is_agent(kind) => {
                let t = tag_of(kind);
                assert!(is_source(t, k), "pull at sink port {k} of {}", t.name());
                if t.is_producer() && f.x & SHIPPABLE != 0 {
                    self.ship(s, f.x & ADDR);
                } else if t.is_producer() {
                    let mut m = Flit::new(MOVED, f.x & ADDR);
                    m.x = f.dst;
                    m.n = VALUE;
                    self.emit(m);
                } else {
                    assert_eq!(slot.p[k], NONE, "two readers subscribed to one source");
                    self.slots[s as usize].p[k] = f.x;
                    self.activate(s);
                }
            }
            _ => panic!("pull at a free slot {:#x}", f.dst),
        }
    }

    fn spend_ind(&mut self, s: u32, k: usize) {
        let sl = &mut self.slots[s as usize];
        sl.st &= !(1 << k);
        sl.p[k] = NONE;
        if sl.st == 0 { self.set_free(s); }
    }

    fn ship(&mut self, s: u32, to: u32) {
        let slot = self.slots[s as usize];
        let mut m = Flit::new(SHIP, to);
        m.tag = slot.kind;
        m.x = slot.p[1];
        m.y = slot.p[2];
        m.sid = slot.sid;
        self.set_free(s);
        self.emit(m);
    }

    fn on_moved(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        assert_eq!(self.slots[s as usize].st, AWAIT, "moved to a reader that was not waiting");
        self.slots[s as usize].p[0] = f.x;
        match f.n {
            PRESUB => {}
            VALUE => self.slots[s as usize].st = HOLD,
            _ => {
                self.slots[s as usize].st = IDLE;
                self.activate(s);
            }
        }
    }

    fn on_spawn(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        let pend = self.slots[s as usize];
        assert_eq!(pend.kind, RESV, "spawn into an unreserved slot");
        let t = tag_of(f.tag);
        let mut slot = Slot { kind: f.tag, sid: f.sid, ..Slot::EMPTY };
        let fields = [f.x, f.y, f.z];
        for p in 0..3 {
            if p < t.arity() && is_source(t, p) && pend.p[p] != NONE {
                assert_eq!(fields[p], NONE, "two readers for one fresh source");
                slot.p[p] = pend.p[p];
            } else {
                assert!(pend.p[p] == NONE, "early reader at a sink");
                slot.p[p] = fields[p];
            }
        }
        self.place(s, slot);
        if t.is_producer() {
            if is_dead(slot.p[0]) {
                self.erase(s, dead_sid(slot.p[0]));
            } else if slot.p[0] != NONE {
                self.ship(s, slot.p[0] & ADDR);
            }
        } else {
            self.settle(s);
        }
    }

    /// The reader of the source at `f.dst` is gone.
    fn on_drop(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        let slot = self.slots[s as usize];
        let k = logical(slot.perm, a_port(f.dst));
        match slot.kind {
            IND => {
                assert!(slot.st & (1 << k) != 0, "drop at a spent indirection port");
                let mut m = Flit::new(DROP, slot.p[k]);
                m.sid = f.sid;
                self.spend_ind(s, k);
                self.emit(m);
            }
            RESV => {
                assert_eq!(slot.p[k], NONE);
                self.slots[s as usize].p[k] = dead(f.sid);
            }
            kind if is_agent(kind) => {
                let t = tag_of(kind);
                assert!(is_source(t, k), "drop at sink port {k} of {}", t.name());
                if t.is_producer() {
                    self.erase(s, f.sid);
                } else {
                    assert_eq!(slot.p[k], NONE, "drop at a source somebody else reads");
                    self.slots[s as usize].p[k] = dead(f.sid);
                    if slot.st == IDLE { self.settle(s); }
                }
            }
            _ => panic!("drop at a free slot {:#x}", f.dst),
        }
    }

    fn record_fire(&mut self, ri: usize) {
        self.out.fires += 1;
        if self.out.record { self.out.rec_fires.push([self.c, ri as u32]); }
    }

    /// The eraser rule, run where the producer stands: it dies, and its children are
    /// dropped in turn.
    fn erase(&mut self, s: u32, eps_sid: u32) {
        let sl = self.slots[s as usize];
        let ri = find_index(Tag::Eps, tag_of(sl.kind)).expect("an eraser rule for every producer");
        let rule = &RULES[ri];
        self.record_fire(ri);
        self.out.local_fires += 1;
        let fresh = match self.shadow.as_deref_mut() {
            Some(net) => {
                assert_eq!(net.get(eps_sid).ports[0], Some((sl.sid, 0)), "erased a pair the abstract net does not have");
                net.fire(eps_sid, sl.sid).1
            }
            None => vec![NONE; rule.fresh.len()],
        };
        self.set_free(s);
        for (a, b) in rule.wires {
            let ((End::Fresh(e, 0), End::PAux(j)) | (End::PAux(j), End::Fresh(e, 0))) = (*a, *b) else {
                unreachable!("eraser rules hand each child to a fresh eraser")
            };
            let mut m = Flit::new(DROP, sl.p[j as usize]);
            m.sid = fresh[e as usize];
            self.emit(m);
        }
    }

    /// A computation nobody will read: it dies without running, and drops its inputs.
    fn cancel(&mut self, s: u32) {
        let sl = self.slots[s as usize];
        let t = tag_of(sl.kind);
        self.out.cancels += 1;
        let mut erasers = [NONE; 3];
        if let Some(net) = self.shadow.as_deref_mut() {
            let a = net.get(sl.sid).clone();
            for k in 0..t.arity() {
                let (other, q) = a.ports[k].expect("closed net");
                if is_source(t, k) {
                    assert_eq!(net.get(other).tag, Tag::Eps, "cancelled a computation somebody reads");
                    net.agents[other as usize] = None;
                } else {
                    let e = net.mk(Tag::Eps);
                    net.link(e, 0, other, q);
                    erasers[k] = e;
                }
            }
            net.agents[sl.sid as usize] = None;
        }
        self.set_free(s);
        for k in 0..t.arity() {
            if !is_source(t, k) {
                let mut m = Flit::new(DROP, sl.p[k]);
                m.sid = erasers[k];
                self.emit(m);
            }
        }
    }

    /// Send a reservation one step down the free-space field, or park it until the field
    /// says there is room somewhere.
    pub fn route_reserve(&mut self, mut f: Flit) {
        let field = f.n as usize - 1;
        self.out.in_flight += 1;
        let mut best = (INF, NONE);
        if self.phi[self.c as usize][field] != INF {
            for &nb in self.nb {
                if nb != NONE {
                    let v = self.phi[nb as usize][field];
                    if v < best.0 { best = (v, nb); }
                }
            }
        }
        if best.1 == NONE {
            self.out.parked += 1;
            self.parked.push(f);
            return;
        }
        f.dst = addr(best.1, 0, 0);
        self.outbox.push_back(f);
    }

    fn on_reserve(&mut self, f: Flit) {
        self.out.max_reserve_hops = self.out.max_reserve_hops.max(f.hops as u64);
        if *self.free >= f.n {
            let mask = self.reserve_local(f.n as u32);
            let mut g = Flit::new(GRANT, f.x);
            g.x = self.c;
            g.y = mask as u32;
            self.emit(g);
        } else {
            self.route_reserve(f);
        }
    }

    fn on_grant(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        let sl = self.slots[s as usize];
        assert_eq!(sl.st, DOCKED, "grant to a consumer that was not waiting for room");
        let docked = Docked { tag: sl.dock_tag, aux: [sl.p[0], sl.dock], sid: sl.psid };
        self.fire(s, docked, Some((f.x, f.y as u16)));
    }

    fn on_release(&mut self, f: Flit) {
        for s in 0..self.k {
            if f.y & (1 << s) != 0 {
                let sl = self.slots[s as usize];
                assert!(sl.kind == RESV && sl.p == [NONE; 3], "release of a used reservation");
                self.set_free(s);
            }
        }
    }

    /// A producer arrived. Fire at once if this tile has room for the slots the rule needs;
    /// otherwise dock it here, hold what room there is, and look for the rest.
    fn on_ship(&mut self, f: Flit) {
        let s = a_slot(f.dst);
        let cs = self.slots[s as usize];
        assert_eq!(cs.st, AWAIT, "ship arrived at a consumer that did not ask");
        let docked = Docked { tag: f.tag, aux: [f.x, f.y], sid: f.sid };
        let plan = self.plan(s, &cs, &docked);
        let short = plan.slots.saturating_sub(plan.hosts() + *self.free as usize);
        if short == 0 {
            self.fire(s, docked, None);
            return;
        }
        let local = *self.free as u32;
        let hold = if local > 0 { self.reserve_local(local) } else { 0 };
        let sl = &mut self.slots[s as usize];
        sl.st = DOCKED;
        sl.hold = hold;
        sl.p[0] = f.x;
        sl.dock = f.y;
        sl.dock_tag = f.tag;
        sl.psid = f.sid;
        let mut r = Flit::new(RESERVE, addr(self.c, 0, 0));
        r.n = short as u8;
        r.x = addr(self.c, s, 0);
        self.out.sent[RESERVE as usize] += 1;
        self.route_reserve(r);
    }

    /// What a rewrite will need: which fresh producers go straight to a waiting reader
    /// (they never take a slot), how many slots the rest take, which of the consumer's
    /// result ports have no reader yet, and what becomes of the consumer's own slot: it
    /// hosts the fresh agent that answers its one unread result (so the reader's address
    /// stays good), or any fresh agent if every result is accounted for, or else lingers
    /// as indirections.
    fn plan(&self, s: u32, cs: &Slot, d: &Docked) -> Plan {
        let ct = tag_of(cs.kind);
        let rule = &RULES[rule_index(cs.kind, d.tag)];
        let mut linger = 0u8;
        for i in 1..ct.arity() {
            if !is_source(ct, i) || cs.p[i] != NONE { continue; }
            let me = addr(self.c, s, physical(cs.perm, i));
            let read_inside = (1..ct.arity()).any(|j| !is_source(ct, j) && cs.p[j] == me) || d.aux.contains(&me);
            if !read_inside { linger |= 1 << i; }
        }
        let mut direct = 0u8;
        let mut heir = None;
        for &(a, b) in rule.wires {
            if let (End::Fresh(k, p), End::CAux(i)) | (End::CAux(i), End::Fresh(k, p)) = (a, b) {
                let reader = cs.p[i as usize];
                let t = rule.fresh[k as usize];
                if p == 0 && t.is_producer() && is_sub(reader) && reader & SHIPPABLE != 0 {
                    direct |= 1 << k;
                }
                if linger == 1 << i && is_source(t, p as usize) {
                    heir = Some((k as usize, perm_to(p as usize, physical(cs.perm, i as usize) as usize)));
                }
            }
        }
        let slots = rule.fresh.len() - direct.count_ones() as usize;
        let host = if heir.is_some() { heir } else if linger == 0 && slots > 0 { Some((usize::MAX, 0)) } else { None };
        Plan { linger: if heir.is_some() { 0 } else { linger }, direct, slots, host }
    }

    /// The rewrite. Fresh agents take the consumer's own slot (when it is free to go), the
    /// slots it held while docked, other free slots of this tile, then the granted remote
    /// room. Everything else this changes travels as messages.
    fn fire(&mut self, s: u32, d: Docked, grant: Option<(u32, u16)>) {
        let c = self.c;
        let cs = self.slots[s as usize];
        let ct = tag_of(cs.kind);
        let ri = rule_index(cs.kind, d.tag);
        let rule = &RULES[ri];
        self.record_fire(ri);

        let fresh_ids = match self.shadow.as_deref_mut() {
            Some(net) => {
                assert_eq!(net.get(cs.sid).ports[0], Some((d.sid, 0)), "fired a pair the abstract net does not have");
                assert_eq!(net.get(d.sid).tag, tag_of(d.tag));
                net.fire(cs.sid, d.sid).1
            }
            None => vec![NONE; rule.fresh.len()],
        };

        let plan = self.plan(s, &cs, &d);
        let n = rule.fresh.len();
        let mut tgt = [NONE; 6];
        let mut fperm = [0u8; 6];
        let mut placed = 0;
        if let Some((k, perm)) = plan.host {
            if k != usize::MAX {
                tgt[k] = addr(c, s, 0);
                fperm[k] = perm;
                placed += 1;
            }
        }
        let mut order = [0usize; 6];
        let mut n_order = 0;
        for k in 0..n {
            if plan.direct & (1 << k) == 0 && tgt[k] == NONE {
                order[n_order] = k;
                n_order += 1;
            }
        }
        let mut next = 0;
        let mut put = |a: u32| {
            if next == n_order { return false; }
            tgt[order[next]] = a;
            next += 1;
            true
        };
        if plan.host == Some((usize::MAX, 0)) && put(addr(c, s, 0)) { placed += 1; }
        let mut spare_hold = cs.hold;
        for b in 0..self.k {
            if placed < plan.slots && cs.hold & (1 << b) != 0 && put(addr(c, b, 0)) {
                spare_hold &= !(1 << b);
                placed += 1;
            }
        }
        for b in 0..self.k {
            if placed == plan.slots { break; }
            if b != s && self.slots[b as usize].kind == FREE && put(addr(c, b, 0)) { placed += 1; }
        }
        let mut unused = 0u16;
        if let Some((gc, gmask)) = grant {
            unused = gmask;
            for b in 0..self.k {
                if placed < plan.slots && gmask & (1 << b) != 0 && put(addr(gc, b, 0)) {
                    unused &= !(1 << b);
                    placed += 1;
                }
            }
        }
        assert_eq!(placed, plan.slots, "not enough room to fire");
        if tgt[..n].iter().all(|&a| a == NONE || a_cell(a) == c) { self.out.local_fires += 1; }

        let mut fresh = [Slot::EMPTY; 6];
        for (k, t) in rule.fresh.iter().enumerate() {
            fresh[k] = Slot { kind: code(*t), perm: fperm[k], sid: fresh_ids[k], ..Slot::EMPTY };
        }
        let aux = [NONE, d.aux[0], d.aux[1]];
        let own = |r: u32| r != NONE && a_cell(r) == c && a_slot(r) == s && is_source(ct, logical(cs.perm, a_port(r)));
        let have = |e: End| -> u32 {
            match e {
                End::Fresh(k, p) => tgt[k as usize] | physical(fperm[k as usize], p as usize),
                End::CAux(i) => cs.p[i as usize],
                End::PAux(j) => aux[j as usize],
            }
        };
        let partner = |e: End| -> End {
            for (a, b) in rule.wires {
                if *a == e { return *b; }
                if *b == e { return *a; }
            }
            unreachable!("validated ROM")
        };
        // A reference into the dying consumer's own result port chases through the rule.
        // Fresh addresses never chase: one of them may be the consumer's reused slot.
        let resolve = |mut e: End| -> End {
            for _ in 0..16 {
                if matches!(e, End::Fresh(..)) || !own(have(e)) { return e; }
                e = partner(End::CAux(logical(cs.perm, a_port(have(e))) as u8));
            }
            panic!("vicious circle inside a rewrite")
        };
        let read_inside = |i: usize| -> bool {
            let me = addr(c, s, physical(cs.perm, i));
            (1..ct.arity()).any(|j| !is_source(ct, j) && cs.p[j] == me) || aux.contains(&me)
        };

        let mut ind = [NONE; 3];
        let mut out: [Flit; 4] = [Flit::new(0, NONE); 4];
        let mut n_out = 0;
        let mut ships: [(usize, u32); 2] = [(0, NONE); 2];
        let mut n_ships = 0;
        for &(a, b) in rule.wires {
            let (h, nd) = if role(rule, a) == Role::Have { (a, b) } else { (b, a) };
            if let End::CAux(i) = nd {
                if read_inside(i as usize) { continue; }
            }
            let h = resolve(h);
            let src = have(h);
            match nd {
                End::Fresh(k, p) => fresh[k as usize].p[p as usize] = src,
                End::CAux(i) => {
                    let reader = cs.p[i as usize];
                    if is_dead(reader) {
                        let mut f = Flit::new(DROP, src);
                        f.sid = dead_sid(reader);
                        out[n_out] = f;
                        n_out += 1;
                    } else if is_sub(reader) {
                        match h {
                            End::Fresh(k, 0) if plan.direct & (1 << k) != 0 => {
                                ships[n_ships] = (k as usize, reader & ADDR);
                                n_ships += 1;
                            }
                            End::Fresh(k, p) if p > 0 => {
                                // A fresh computation: subscribe the reader on its behalf.
                                fresh[k as usize].p[p as usize] = reader;
                                let mut f = Flit::new(MOVED, reader & ADDR);
                                f.x = src;
                                f.n = PRESUB;
                                out[n_out] = f;
                                n_out += 1;
                            }
                            _ => {
                                let mut f = Flit::new(MOVED, reader & ADDR);
                                f.x = src;
                                out[n_out] = f;
                                n_out += 1;
                            }
                        }
                    } else if plan.linger & (1 << i) != 0 {
                        ind[i as usize] = src;
                    }
                }
                End::PAux(_) => unreachable!("producer aux ports are sinks"),
            }
        }

        // The consumer's slot dies, lingers as indirections, or hosts a fresh agent.
        self.set_free(s);
        if plan.linger != 0 {
            self.place(s, Slot { kind: IND, st: plan.linger, perm: cs.perm, p: ind, ..Slot::EMPTY });
        }
        for &(k, to) in &ships[..n_ships] {
            let mut f = Flit::new(SHIP, to);
            f.tag = fresh[k].kind;
            f.x = fresh[k].p[1];
            f.y = fresh[k].p[2];
            f.sid = fresh[k].sid;
            self.out.direct_ships += 1;
            self.emit(f);
        }
        for f in &out[..n_out] { self.emit(*f); }
        for (k, t) in rule.fresh.iter().enumerate() {
            let a = tgt[k];
            if a == NONE { continue; }
            if a_cell(a) == c {
                self.place(a_slot(a), fresh[k]);
                if t.is_consumer() {
                    let wanted = eager(*t) || (1..t.arity()).any(|p| is_source(*t, p) && is_sub(fresh[k].p[p]));
                    if wanted || self.speculative() { self.push_event(Ev::Activate(a_slot(a))); }
                }
            } else {
                let mut f = Flit::new(SPAWN, a);
                f.tag = fresh[k].kind;
                f.x = fresh[k].p[0];
                f.y = fresh[k].p[1];
                f.z = fresh[k].p[2];
                f.sid = fresh[k].sid;
                self.emit(f);
            }
        }
        for b in 0..self.k {
            if spare_hold & (1 << b) != 0 { self.set_free(b); }
        }
        if unused != 0 {
            let (gc, _) = grant.unwrap();
            let mut f = Flit::new(RELEASE, addr(gc, 0, 0));
            f.y = unused as u32;
            self.emit(f);
        }
    }
}
