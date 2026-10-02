//! The mesh: a W×H grid of tiles. Each tile holds K agent slots, a five-port router, and a
//! protocol engine that handles one event per tick. Wires are not matter: a wire is a
//! reference held by its sink, naming the source's (tile, slot, port) address, and every
//! source has exactly one reader.
//!
//! Every handler below reads and writes only its own tile and emits messages; nothing
//! reaches into a neighbour. That is the hardware contract, and it is also why the
//! simulation is deterministic whatever order tiles are visited in.
//!
//! Evaluation is demand-driven. A consumer starts only when someone subscribes to its
//! result (the normalizer and the root are always hungry); erasure is a `Drop` that runs
//! the eraser rule in place at a producer, or cancels a computation nobody will read.
//!
//! Messages:
//! - `Pull`    a reader asks a source for its value. A producer ships itself to the reader;
//!             a pending computation records the reader and starts; an indirection hands
//!             its target over and dies.
//! - `Ship`    a producer reaches the consumer that wants it: the rewrite fires (or docks
//!             while room is found).
//! - `Moved`   a subscribed reader learns its source's new address.
//! - `Spawn`   a fresh agent lands in a slot reserved for it in another tile.
//! - `Drop`    the reader of a source is gone.
//! - `Reserve` a docked consumer looks for room, following the free-space field;
//!             `Grant` answers, `Release` returns room a rewrite did not use.

use crate::polarity::{is_source, role, Role};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::rules::{find_index, End, Tag, ALL_TAGS, RULES};
use std::collections::VecDeque;

pub const NONE: u32 = u32::MAX;
pub const K_MAX: u32 = 16;

// Slot kinds: 0 free, 1..=13 the agent tags (ALL_TAGS order), then two plumbing kinds.
pub const FREE: u8 = 0;
pub const IND: u8 = 14;
pub const RESV: u8 = 15;

// Reader phases (consumers and Out).
pub const IDLE: u8 = 0;
pub const AWAIT_MOVED: u8 = 2;
pub const AWAIT_SHIP: u8 = 3;
pub const HOLD: u8 = 4;
/// Holding the producer it pulled, waiting for room to fire.
pub const DOCKED: u8 = 5;

// Message kinds.
pub const PULL: u8 = 0;
pub const SHIP: u8 = 1;
pub const MOVED: u8 = 2;
pub const SPAWN: u8 = 3;
pub const RESERVE: u8 = 4;
pub const GRANT: u8 = 5;
pub const RELEASE: u8 = 6;
pub const DROP: u8 = 7;
pub const N_KINDS: usize = 8;
pub const KIND_NAMES: [&str; N_KINDS] = ["pull", "ship", "moved", "spawn", "reserve", "grant", "release", "drop"];

/// `Moved` flag: the reader is already subscribed at the new source.
const PRESUB: u8 = 1;

// Flags on the value a source port keeps for its reader. Addresses fit in 30 bits.
const ADDR: u32 = 0x3FFF_FFFF;
/// The reader is a consumer, so a producer may be shipped straight to it.
const SHIPPABLE: u32 = 1 << 30;
/// The reader dropped this source; the low bits keep the eraser's shadow id.
const DEAD: u32 = 1 << 31;
#[inline] fn is_dead(v: u32) -> bool { v != NONE && v & DEAD != 0 }
#[inline] fn is_sub(v: u32) -> bool { v != NONE && v & DEAD == 0 }
#[inline] fn dead(eps_sid: u32) -> u32 { DEAD | (eps_sid & 0x3FFF_FFFF) }
#[inline] fn dead_sid(v: u32) -> u32 { if v & 0x3FFF_FFFF == 0x3FFF_FFFF { NONE } else { v & 0x3FFF_FFFF } }

// Router ports: four neighbours and the local ejection port.
pub const EAST: usize = 0;
pub const WEST: usize = 1;
pub const SOUTH: usize = 2;
pub const NORTH: usize = 3;
pub const EJECT: usize = 4;

/// The free-space field keeps one distance per room size: field n-1 points at the nearest
/// tile with at least n free slots.
pub const FIELDS: usize = 6;
pub const INF: u8 = u8::MAX;

#[inline] pub fn addr(cell: u32, slot: u32, port: u32) -> u32 { (cell << 6) | (slot << 2) | port }
#[inline] pub fn a_cell(a: u32) -> u32 { (a & ADDR) >> 6 }
#[inline] pub fn a_slot(a: u32) -> u32 { (a >> 2) & 15 }
#[inline] pub fn a_port(a: u32) -> usize { (a & 3) as usize }

#[inline] pub fn code(t: Tag) -> u8 { ALL_TAGS.iter().position(|x| *x == t).unwrap() as u8 + 1 }
#[inline] pub fn tag_of(kind: u8) -> Tag { ALL_TAGS[kind as usize - 1] }
#[inline] pub fn is_agent(kind: u8) -> bool { (1..=13).contains(&kind) }

/// Readers that are hungry from birth: the normalizer drives the answer, the root holds
/// it, and an eraser only ever drops.
#[inline] fn eager(t: Tag) -> bool { matches!(t, Tag::Nrm | Tag::Eps | Tag::Out) }

/// One agent slot. For a sink port `p[i]` is the reference it holds; for a source port it
/// is the reader that subscribed (or NONE, or DEAD). An indirection keeps its targets in
/// the ports named by `st` (a bitmask); a reserved slot keeps readers that arrived before
/// the agent did. A docked consumer keeps the producer it pulled in `p[0]`, `dock` and
/// `dock_tag`, and the local slots it is holding in `hold`.
#[derive(Clone, Copy, Debug)]
pub struct Slot {
    pub kind: u8,
    pub st: u8,
    pub dock_tag: u8,
    pub hold: u16,
    pub p: [u32; 3],
    pub dock: u32,
    /// Shadow ids (checking only): this agent, and a docked producer.
    pub sid: u32,
    pub psid: u32,
}

impl Slot {
    pub const EMPTY: Slot =
        Slot { kind: FREE, st: 0, dock_tag: 0, hold: 0, p: [NONE; 3], dock: NONE, sid: NONE, psid: NONE };
}

/// A message, one flit. Fields are interpreted per kind (see the module docs).
#[derive(Clone, Copy, Debug)]
pub struct Flit {
    pub kind: u8,
    pub tag: u8,
    pub n: u8,
    pub hops: u8,
    pub dst: u32,
    pub x: u32,
    pub y: u32,
    pub z: u32,
    pub sid: u32,
}

impl Flit {
    fn new(kind: u8, dst: u32) -> Flit {
        Flit { kind, tag: 0, n: 0, hops: 0, dst, x: NONE, y: NONE, z: NONE, sid: NONE }
    }
}

/// A producer that has reached the consumer which wants it.
#[derive(Clone, Copy, Debug)]
struct Docked {
    tag: u8,
    aux: [u32; 2],
    sid: u32,
}

fn rule_index(consumer: u8, producer: u8) -> usize {
    let (ct, pt) = (tag_of(consumer), tag_of(producer));
    find_index(ct, pt).unwrap_or_else(|| panic!("no rule {}·{}", ct.name(), pt.name()))
}

#[derive(Clone, Copy, Debug)]
enum Ev {
    Msg(Flit),
    Activate(u32),
}

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub w: u32,
    pub h: u32,
    /// Agent slots per tile (6..=16: a tile must hold one whole rewrite).
    pub k: u32,
    /// Router input buffer depth, in flits.
    pub fifo: u32,
    /// Protocol events a tile handles per tick.
    pub events_per_tick: u32,
    /// Agents per tile when the initial term is laid out.
    pub init_fill: u32,
}

impl Default for Config {
    fn default() -> Self { Config { w: 64, h: 64, k: 8, fifo: 4, events_per_tick: 1, init_fill: 3 } }
}

#[derive(Clone, Debug, Default)]
pub struct Stats {
    pub ticks: u64,
    /// Rule-ROM interactions, wherever they ran (a consumer's tile, or in place by a drop).
    pub fires: u64,
    pub cancels: u64,
    pub direct_ships: u64,
    pub events: u64,
    pub hops: u64,
    pub sent: [u64; N_KINDS],
    pub live: u64,
    pub peak_live: u64,
    pub inds: u64,
    pub peak_inds: u64,
    pub reserved: u64,
    pub peak_reserved: u64,
    pub in_flight: u64,
    pub peak_in_flight: u64,
    pub max_outbox: u64,
    pub max_events: u64,
    pub max_reserve_hops: u64,
    pub parked_reserves: u64,
    pub local_fires: u64,
}

/// Per-tick activity, recorded only when the player asks for it.
#[derive(Default)]
pub struct Recorder {
    /// (from tile, direction | kind << 4 | tag << 8)
    pub moves: Vec<[u32; 2]>,
    /// (tile, rule index)
    pub fires: Vec<[u32; 2]>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Outcome {
    /// The root holds a whole constructor tree.
    Done,
    /// Quiescent with consumers waiting for room that never came.
    OutOfSpace,
    /// Quiescent without an answer.
    Stuck,
    /// Tick budget exhausted.
    Running,
}

impl Outcome {
    pub fn name(self) -> &'static str {
        match self {
            Outcome::Done => "done",
            Outcome::OutOfSpace => "out-of-space",
            Outcome::Stuck => "stuck",
            Outcome::Running => "running",
        }
    }
}

pub struct Mesh {
    pub cfg: Config,
    pub slots: Vec<Slot>,
    pub free: Vec<u8>,
    pub phi: Vec<[u8; FIELDS]>,
    fifo_buf: Vec<Flit>,
    fifo_head: Vec<u8>,
    pub fifo_len: Vec<u8>,
    outbox: Vec<VecDeque<Flit>>,
    events: Vec<VecDeque<Ev>>,
    parked: Vec<Vec<Flit>>,
    rr: Vec<u8>,
    active: Vec<u32>,
    stamp: Vec<u32>,
    epoch: u32,
    phi_dirty: Vec<u32>,
    phi_stamp: Vec<u32>,
    phi_epoch: u32,
    moves: Vec<(u32, usize, usize)>,
    pub tick: u64,
    pub stats: Stats,
    pub shadow: Option<Net>,
    pub rec: Option<Recorder>,
    pub out_addr: u32,
}

impl Mesh {
    pub fn new(cfg: Config) -> Mesh {
        assert!(cfg.k >= 6 && cfg.k <= K_MAX, "a tile holds 6..=16 slots");
        assert!(cfg.fifo >= 1 && cfg.fifo <= 255);
        assert!((cfg.w as u64) * (cfg.h as u64) < 1 << 24, "addresses are 30 bits");
        let cells = (cfg.w * cfg.h) as usize;
        Mesh {
            cfg,
            slots: vec![Slot::EMPTY; cells * cfg.k as usize],
            free: vec![cfg.k as u8; cells],
            phi: vec![[0; FIELDS]; cells],
            fifo_buf: vec![Flit::new(0, 0); cells * 4 * cfg.fifo as usize],
            fifo_head: vec![0; cells * 4],
            fifo_len: vec![0; cells * 4],
            outbox: (0..cells).map(|_| VecDeque::new()).collect(),
            events: (0..cells).map(|_| VecDeque::new()).collect(),
            parked: (0..cells).map(|_| Vec::new()).collect(),
            rr: vec![0; cells],
            active: vec![],
            stamp: vec![0; cells],
            epoch: 1,
            phi_dirty: vec![],
            phi_stamp: vec![0; cells],
            phi_epoch: 1,
            moves: vec![],
            tick: 0,
            stats: Stats::default(),
            shadow: None,
            rec: None,
            out_addr: NONE,
        }
    }

    #[inline] pub fn cells(&self) -> u32 { self.cfg.w * self.cfg.h }
    #[inline] fn si(&self, cell: u32, slot: u32) -> usize { (cell * self.cfg.k + slot) as usize }
    #[inline] pub fn slot_at(&self, a: u32) -> &Slot { &self.slots[self.si(a_cell(a), a_slot(a))] }

    #[inline]
    fn neighbor(&self, c: u32, d: usize) -> Option<u32> {
        let (x, y, w, h) = (c % self.cfg.w, c / self.cfg.w, self.cfg.w, self.cfg.h);
        match d {
            EAST if x + 1 < w => Some(c + 1),
            WEST if x > 0 => Some(c - 1),
            SOUTH if y + 1 < h => Some(c + w),
            NORTH if y > 0 => Some(c - w),
            _ => None,
        }
    }

    /// Dimension-order routing: X first, then Y. Deadlock-free on a mesh.
    #[inline]
    fn route(&self, c: u32, dst_cell: u32) -> usize {
        let (x, y) = (c % self.cfg.w, c / self.cfg.w);
        let (dx, dy) = (dst_cell % self.cfg.w, dst_cell / self.cfg.w);
        if dx > x { EAST } else if dx < x { WEST } else if dy > y { SOUTH } else if dy < y { NORTH } else { EJECT }
    }

    fn touch(&mut self, c: u32) {
        if self.stamp[c as usize] != self.epoch {
            self.stamp[c as usize] = self.epoch;
            self.active.push(c);
        }
    }

    fn dirty_phi(&mut self, c: u32) {
        if self.phi_stamp[c as usize] != self.phi_epoch {
            self.phi_stamp[c as usize] = self.phi_epoch;
            self.phi_dirty.push(c);
        }
    }

    fn emit(&mut self, c: u32, f: Flit) {
        self.stats.sent[f.kind as usize] += 1;
        self.stats.in_flight += 1;
        self.stats.peak_in_flight = self.stats.peak_in_flight.max(self.stats.in_flight);
        if a_cell(f.dst) == c {
            self.push_event(c, Ev::Msg(f));
        } else {
            let ob = &mut self.outbox[c as usize];
            ob.push_back(f);
            self.stats.max_outbox = self.stats.max_outbox.max(ob.len() as u64);
            self.touch(c);
        }
    }

    fn push_event(&mut self, c: u32, e: Ev) {
        let q = &mut self.events[c as usize];
        q.push_back(e);
        self.stats.max_events = self.stats.max_events.max(q.len() as u64);
        self.touch(c);
    }

    // ---- slot bookkeeping -------------------------------------------------------------

    fn set_free(&mut self, c: u32, s: u32) {
        let i = self.si(c, s);
        let was = self.slots[i].kind;
        if was == FREE { return; }
        if is_agent(was) { self.stats.live -= 1; }
        if was == IND { self.stats.inds -= 1; }
        if was == RESV { self.stats.reserved -= 1; }
        self.slots[i] = Slot::EMPTY;
        self.free[c as usize] += 1;
        self.dirty_phi(c);
        if !self.parked[c as usize].is_empty() { self.wake_parked(c); }
    }

    /// Parked reservations retry as ordinary events, never inline: a wake can happen in the
    /// middle of a rewrite that is about to use the slot that just came free.
    fn wake_parked(&mut self, c: u32) {
        let parked = std::mem::take(&mut self.parked[c as usize]);
        for f in parked {
            self.stats.parked_reserves -= 1;
            self.push_event(c, Ev::Msg(f));
        }
    }

    fn place(&mut self, c: u32, s: u32, slot: Slot) {
        let i = self.si(c, s);
        let was = self.slots[i].kind;
        debug_assert!(was == FREE || was == RESV, "placing over a live slot");
        if was == FREE {
            self.free[c as usize] -= 1;
            self.dirty_phi(c);
        }
        if was == RESV { self.stats.reserved -= 1; }
        if is_agent(slot.kind) {
            self.stats.live += 1;
            self.stats.peak_live = self.stats.peak_live.max(self.stats.live);
        }
        if slot.kind == IND {
            self.stats.inds += 1;
            self.stats.peak_inds = self.stats.peak_inds.max(self.stats.inds);
        }
        if slot.kind == RESV {
            self.stats.reserved += 1;
            self.stats.peak_reserved = self.stats.peak_reserved.max(self.stats.reserved);
        }
        self.slots[i] = slot;
    }

    /// Reserve `n` free slots of tile `c`, returning their mask.
    fn reserve_local(&mut self, c: u32, n: u32) -> u16 {
        let mut mask = 0u16;
        let mut got = 0;
        for s in 0..self.cfg.k {
            if got == n { break; }
            if self.slots[self.si(c, s)].kind == FREE {
                self.place(c, s, Slot { kind: RESV, ..Slot::EMPTY });
                mask |= 1 << s;
                got += 1;
            }
        }
        debug_assert_eq!(got, n);
        mask
    }

    // ---- loading ------------------------------------------------------------------------

    /// Lay out an abstract net: depth-first from `Out`, along a square spiral from the
    /// grid's centre, `init_fill` agents per tile. With `check`, the net stays on as the
    /// shadow: every interaction is replayed on it and must be one it has.
    pub fn load(cfg: Config, net: Net, out: u32, check: bool) -> Result<Mesh, String> {
        let mut m = Mesh::new(cfg);
        let n = net.agents.len();
        let mut order = Vec::with_capacity(n);
        let mut seen = vec![false; n];
        let mut stack = vec![out];
        while let Some(id) = stack.pop() {
            if seen[id as usize] { continue; }
            seen[id as usize] = true;
            order.push(id);
            let a = net.get(id);
            for p in (0..a.tag.arity()).rev() {
                if let Some((b, _)) = a.ports[p] {
                    if !seen[b as usize] { stack.push(b); }
                }
            }
        }
        let spiral = m.spiral();
        let fill = cfg.init_fill.clamp(1, cfg.k) as usize;
        if order.len() > spiral.len() * fill {
            return Err(format!("{} agents do not fit {} tiles at {fill} per tile", order.len(), spiral.len()));
        }
        let mut at = vec![NONE; n];
        for (i, &id) in order.iter().enumerate() {
            at[id as usize] = addr(spiral[i / fill], (i % fill) as u32, 0);
        }
        for &id in &order {
            let a = net.get(id);
            let mut slot = Slot { kind: code(a.tag), sid: id, ..Slot::EMPTY };
            for p in 0..a.tag.arity() {
                if !is_source(a.tag, p) {
                    let (b, q) = a.ports[p].ok_or("open port in the initial net")?;
                    slot.p[p] = at[b as usize] | q as u32;
                }
            }
            let base = at[id as usize];
            m.place(a_cell(base), a_slot(base), slot);
        }
        m.out_addr = at[out as usize];
        m.init_phi();
        for &id in &order {
            if eager(net.get(id).tag) {
                let base = at[id as usize];
                m.push_event(a_cell(base), Ev::Activate(a_slot(base)));
            }
        }
        if check { m.shadow = Some(net); }
        m.phi_dirty.clear();
        m.epoch += 1;
        let start = std::mem::take(&mut m.active);
        for c in start { m.touch(c); }
        Ok(m)
    }

    /// Tiles in order of a square spiral out from the centre.
    pub fn spiral(&self) -> Vec<u32> {
        let (w, h) = (self.cfg.w as i64, self.cfg.h as i64);
        let (mut x, mut y) = ((w - 1) / 2, (h - 1) / 2);
        let mut out = Vec::with_capacity((w * h) as usize);
        let (mut dx, mut dy, mut len) = (1i64, 0i64, 1i64);
        while (out.len() as i64) < w * h {
            for _ in 0..2 {
                for _ in 0..len {
                    if x >= 0 && x < w && y >= 0 && y < h { out.push((y * w + x) as u32); }
                    x += dx;
                    y += dy;
                }
                (dx, dy) = (-dy, dx);
            }
            len += 1;
        }
        out
    }

    fn init_phi(&mut self) {
        let cells = self.cells();
        let cap = self.phi_cap();
        for f in 0..FIELDS {
            let mut q = VecDeque::new();
            for c in 0..cells {
                if self.free[c as usize] as usize > f {
                    self.phi[c as usize][f] = 0;
                    q.push_back(c);
                } else {
                    self.phi[c as usize][f] = INF;
                }
            }
            while let Some(c) = q.pop_front() {
                let d = self.phi[c as usize][f];
                for dir in 0..4 {
                    if let Some(nb) = self.neighbor(c, dir) {
                        if self.phi[nb as usize][f] == INF && d + 1 < cap {
                            self.phi[nb as usize][f] = d + 1;
                            q.push_back(nb);
                        }
                    }
                }
            }
        }
    }

    fn phi_cap(&self) -> u8 { (self.cfg.w + self.cfg.h).min(250) as u8 }

    // ---- the tick -------------------------------------------------------------------------

    pub fn quiescent(&self) -> bool { self.active.is_empty() }

    /// One clock tick: every router moves at most one flit per output port, then every
    /// tile handles up to `events_per_tick` protocol events, then the free-space field
    /// relaxes one step.
    pub fn step(&mut self) {
        self.tick += 1;
        self.stats.ticks = self.tick;
        if let Some(r) = self.rec.as_mut() {
            r.moves.clear();
            r.fires.clear();
        }
        let cur = std::mem::take(&mut self.active);
        self.epoch += 1;
        let b = self.cfg.fifo as usize;

        // Switch allocation, from start-of-tick state only.
        let mut moves = std::mem::take(&mut self.moves);
        moves.clear();
        for &c in &cur {
            let start = self.rr[c as usize] as usize;
            let mut used = [false; 5];
            for j in 0..5 {
                let i = (start + j) % 5;
                let dst = if i < 4 {
                    let q = (c * 4) as usize + i;
                    if self.fifo_len[q] == 0 { continue; }
                    self.fifo_buf[q * b + self.fifo_head[q] as usize].dst
                } else {
                    match self.outbox[c as usize].front() { Some(f) => f.dst, None => continue }
                };
                let o = self.route(c, a_cell(dst));
                if used[o] { continue; }
                if o != EJECT {
                    let nb = self.neighbor(c, o).expect("XY routing stays on the grid");
                    if self.fifo_len[(nb * 4) as usize + (o ^ 1)] as usize >= b { continue; }
                }
                used[o] = true;
                moves.push((c, i, o));
            }
            self.rr[c as usize] = ((start + 1) % 5) as u8;
        }

        // Link traversal.
        for &(c, i, o) in &moves {
            let mut f = if i < 4 {
                let q = (c * 4) as usize + i;
                let f = self.fifo_buf[q * b + self.fifo_head[q] as usize];
                self.fifo_head[q] = ((self.fifo_head[q] as usize + 1) % b) as u8;
                self.fifo_len[q] -= 1;
                f
            } else {
                self.outbox[c as usize].pop_front().unwrap()
            };
            if o == EJECT {
                self.push_event(c, Ev::Msg(f));
            } else {
                let nb = self.neighbor(c, o).unwrap();
                let q = (nb * 4) as usize + (o ^ 1);
                let tail = (self.fifo_head[q] as usize + self.fifo_len[q] as usize) % b;
                f.hops = f.hops.saturating_add(1);
                self.fifo_buf[q * b + tail] = f;
                self.fifo_len[q] += 1;
                self.stats.hops += 1;
                if let Some(r) = self.rec.as_mut() {
                    r.moves.push([c, o as u32 | (f.kind as u32) << 4 | (f.tag as u32) << 8]);
                }
                self.touch(nb);
            }
        }
        self.moves = moves;

        // Protocol engines.
        for &c in &cur {
            for _ in 0..self.cfg.events_per_tick {
                let Some(e) = self.events[c as usize].pop_front() else { break };
                self.stats.events += 1;
                match e {
                    Ev::Msg(f) => {
                        self.stats.in_flight -= 1;
                        self.handle(c, f);
                    }
                    Ev::Activate(s) => self.activate(c, s),
                }
            }
        }
        for &c in &cur {
            let ci = c as usize;
            let busy = !self.events[ci].is_empty()
                || !self.outbox[ci].is_empty()
                || self.fifo_len[ci * 4..ci * 4 + 4].iter().any(|&l| l > 0);
            if busy { self.touch(c); }
        }

        // The free-space field relaxes one step per tick, as the hardware would.
        let dirty = std::mem::take(&mut self.phi_dirty);
        self.phi_epoch += 1;
        let cap = self.phi_cap();
        for &c in &dirty {
            let old = self.phi[c as usize];
            let mut new = [INF; FIELDS];
            for f in 0..FIELDS {
                if self.free[c as usize] as usize > f {
                    new[f] = 0;
                } else {
                    let mut best = INF;
                    for d in 0..4 {
                        if let Some(nb) = self.neighbor(c, d) { best = best.min(self.phi[nb as usize][f]); }
                    }
                    new[f] = if best >= cap - 1 { INF } else { best + 1 };
                }
            }
            if new != old {
                self.phi[c as usize] = new;
                for d in 0..4 {
                    if let Some(nb) = self.neighbor(c, d) { self.dirty_phi(nb); }
                }
                if (0..FIELDS).any(|f| new[f] < old[f]) && !self.parked[c as usize].is_empty() {
                    self.wake_parked(c);
                }
            }
        }
    }

    /// Run until quiescent or until the clock reads `max_ticks`.
    pub fn run(&mut self, max_ticks: u64) -> Outcome {
        while !self.quiescent() {
            if self.tick >= max_ticks { return Outcome::Running; }
            self.step();
        }
        self.outcome()
    }

    pub fn outcome(&self) -> Outcome {
        if !self.quiescent() { return Outcome::Running; }
        if self.readback().is_some() { return Outcome::Done; }
        if self.stats.parked_reserves > 0 { Outcome::OutOfSpace } else { Outcome::Stuck }
    }

    // ---- protocol handlers (tile-local) ---------------------------------------------------

    fn handle(&mut self, c: u32, f: Flit) {
        match f.kind {
            PULL => self.on_pull(c, f),
            SHIP => self.on_ship(c, f),
            MOVED => self.on_moved(c, f),
            SPAWN => self.on_spawn(c, f),
            DROP => self.on_drop(c, f),
            RESERVE => self.on_reserve(c, f),
            GRANT => self.on_grant(c, f),
            RELEASE => self.on_release(c, f),
            k => panic!("unknown message kind {k}"),
        }
    }

    /// A reader starts reading: pull its source, or (an eraser) drop it.
    fn activate(&mut self, c: u32, s: u32) {
        let i = self.si(c, s);
        let slot = self.slots[i];
        if slot.st != IDLE || !is_agent(slot.kind) { return; }
        let t = tag_of(slot.kind);
        let r = slot.p[0];
        if t == Tag::Eps {
            let mut f = Flit::new(DROP, r);
            f.sid = slot.sid;
            self.set_free(c, s);
            self.emit(c, f);
            return;
        }
        if t == Tag::Out && a_port(r) == 0 {
            self.slots[i].st = HOLD;
            return;
        }
        self.slots[i].st = if a_port(r) == 0 { AWAIT_SHIP } else { AWAIT_MOVED };
        let mut f = Flit::new(PULL, r);
        f.x = addr(c, s, 0) | if t == Tag::Out { 0 } else { SHIPPABLE };
        self.emit(c, f);
    }

    /// A consumer just took a slot: start it if it is wanted, cancel it if nobody can read
    /// it, and otherwise let it wait for demand.
    fn settle(&mut self, c: u32, s: u32) {
        let sl = self.slots[self.si(c, s)];
        let t = tag_of(sl.kind);
        let outs = (1..t.arity()).filter(|&k| is_source(t, k));
        let (mut any_sub, mut all_dead, mut n) = (false, true, 0);
        for k in outs {
            n += 1;
            any_sub |= is_sub(sl.p[k]);
            all_dead &= is_dead(sl.p[k]);
        }
        if eager(t) || any_sub {
            self.activate(c, s);
        } else if n > 0 && all_dead {
            self.cancel(c, s);
        }
    }

    fn on_pull(&mut self, c: u32, f: Flit) {
        let (s, k) = (a_slot(f.dst), a_port(f.dst));
        let i = self.si(c, s);
        let slot = self.slots[i];
        match slot.kind {
            IND => {
                assert!(slot.st & (1 << k) != 0, "pull at a spent indirection port");
                let mut m = Flit::new(MOVED, f.x & ADDR);
                m.x = slot.p[k];
                self.emit(c, m);
                self.spend_ind(c, s, k);
            }
            RESV => {
                assert_eq!(slot.p[k], NONE, "two readers pulled one reserved source");
                self.slots[i].p[k] = f.x;
            }
            kind if is_agent(kind) => {
                let t = tag_of(kind);
                assert!(is_source(t, k), "pull at sink port {k} of {}", t.name());
                if t.is_producer() {
                    self.ship(c, s, f.x & ADDR);
                } else {
                    assert_eq!(slot.p[k], NONE, "two readers subscribed to one source");
                    self.slots[i].p[k] = f.x;
                    self.activate(c, s);
                }
            }
            _ => panic!("pull at a free slot {:#x}", f.dst),
        }
    }

    fn spend_ind(&mut self, c: u32, s: u32, k: usize) {
        let i = self.si(c, s);
        let sl = &mut self.slots[i];
        sl.st &= !(1 << k);
        sl.p[k] = NONE;
        if sl.st == 0 { self.set_free(c, s); }
    }

    fn ship(&mut self, c: u32, s: u32, to: u32) {
        let slot = self.slots[self.si(c, s)];
        let mut m = Flit::new(SHIP, to);
        m.tag = slot.kind;
        m.x = slot.p[1];
        m.y = slot.p[2];
        m.sid = slot.sid;
        self.set_free(c, s);
        self.emit(c, m);
    }

    fn on_moved(&mut self, c: u32, f: Flit) {
        let s = a_slot(f.dst);
        let i = self.si(c, s);
        assert_eq!(self.slots[i].st, AWAIT_MOVED, "moved to a reader that was not waiting");
        self.slots[i].p[0] = f.x;
        if f.n != PRESUB {
            self.slots[i].st = IDLE;
            self.activate(c, s);
        }
    }

    fn on_spawn(&mut self, c: u32, f: Flit) {
        let s = a_slot(f.dst);
        let i = self.si(c, s);
        let pend = self.slots[i];
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
        self.place(c, s, slot);
        if t.is_producer() {
            if is_dead(slot.p[0]) {
                self.erase(c, s, dead_sid(slot.p[0]));
            } else if slot.p[0] != NONE {
                self.ship(c, s, slot.p[0] & ADDR);
            }
        } else {
            self.settle(c, s);
        }
    }

    /// The reader of the source at `f.dst` is gone.
    fn on_drop(&mut self, c: u32, f: Flit) {
        let (s, k) = (a_slot(f.dst), a_port(f.dst));
        let i = self.si(c, s);
        let slot = self.slots[i];
        match slot.kind {
            IND => {
                assert!(slot.st & (1 << k) != 0, "drop at a spent indirection port");
                let mut m = Flit::new(DROP, slot.p[k]);
                m.sid = f.sid;
                self.spend_ind(c, s, k);
                self.emit(c, m);
            }
            RESV => {
                assert_eq!(slot.p[k], NONE);
                self.slots[i].p[k] = dead(f.sid);
            }
            kind if is_agent(kind) => {
                let t = tag_of(kind);
                assert!(is_source(t, k), "drop at sink port {k} of {}", t.name());
                if t.is_producer() {
                    self.erase(c, s, f.sid);
                } else {
                    assert_eq!(slot.p[k], NONE, "drop at a source somebody else reads");
                    self.slots[i].p[k] = dead(f.sid);
                    if slot.st == IDLE { self.settle(c, s); }
                }
            }
            _ => panic!("drop at a free slot {:#x}", f.dst),
        }
    }

    /// The eraser rule, run where the producer stands: it dies, and its children are
    /// dropped in turn.
    fn erase(&mut self, c: u32, s: u32, eps_sid: u32) {
        let sl = self.slots[self.si(c, s)];
        let t = tag_of(sl.kind);
        let ri = find_index(Tag::Eps, t).expect("an eraser rule for every producer");
        let rule = &RULES[ri];
        self.stats.fires += 1;
        self.stats.local_fires += 1;
        if let Some(r) = self.rec.as_mut() { r.fires.push([c, ri as u32]); }
        let fresh = match self.shadow.as_mut() {
            Some(net) => {
                assert_eq!(net.get(eps_sid).ports[0], Some((sl.sid, 0)), "erased a pair the abstract net does not have");
                net.fire(eps_sid, sl.sid).1
            }
            None => vec![NONE; rule.fresh.len()],
        };
        self.set_free(c, s);
        for (a, b) in rule.wires {
            let ((End::Fresh(e, 0), End::PAux(j)) | (End::PAux(j), End::Fresh(e, 0))) = (*a, *b) else {
                unreachable!("eraser rules hand each child to a fresh eraser")
            };
            let mut m = Flit::new(DROP, sl.p[j as usize]);
            m.sid = fresh[e as usize];
            self.emit(c, m);
        }
    }

    /// A computation nobody will read: it dies without running, and drops its inputs.
    fn cancel(&mut self, c: u32, s: u32) {
        let sl = self.slots[self.si(c, s)];
        let t = tag_of(sl.kind);
        self.stats.cancels += 1;
        let mut erasers = [NONE; 3];
        if let Some(net) = self.shadow.as_mut() {
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
        self.set_free(c, s);
        for k in 0..t.arity() {
            if !is_source(t, k) {
                let mut m = Flit::new(DROP, sl.p[k]);
                m.sid = erasers[k];
                self.emit(c, m);
            }
        }
    }

    fn route_reserve(&mut self, c: u32, mut f: Flit) {
        let field = f.n as usize - 1;
        self.stats.in_flight += 1;
        let mut best = (INF, NONE);
        if self.phi[c as usize][field] != INF {
            for d in 0..4 {
                if let Some(nb) = self.neighbor(c, d) {
                    let v = self.phi[nb as usize][field];
                    if v < best.0 { best = (v, nb); }
                }
            }
        }
        if best.1 == NONE {
            self.stats.parked_reserves += 1;
            self.parked[c as usize].push(f);
            return;
        }
        f.dst = addr(best.1, 0, 0);
        self.outbox[c as usize].push_back(f);
        self.touch(c);
    }

    fn on_reserve(&mut self, c: u32, f: Flit) {
        self.stats.max_reserve_hops = self.stats.max_reserve_hops.max(f.hops as u64);
        if self.free[c as usize] >= f.n {
            let mask = self.reserve_local(c, f.n as u32);
            let mut g = Flit::new(GRANT, f.x);
            g.x = c;
            g.y = mask as u32;
            self.emit(c, g);
        } else {
            self.route_reserve(c, f);
        }
    }

    fn on_grant(&mut self, c: u32, f: Flit) {
        let s = a_slot(f.dst);
        let sl = self.slots[self.si(c, s)];
        assert_eq!(sl.st, DOCKED, "grant to a consumer that was not waiting for room");
        let docked = Docked { tag: sl.dock_tag, aux: [sl.p[0], sl.dock], sid: sl.psid };
        self.fire(c, s, docked, Some((f.x, f.y as u16)));
    }

    fn on_release(&mut self, c: u32, f: Flit) {
        for s in 0..self.cfg.k {
            if f.y & (1 << s) != 0 {
                let sl = self.slots[self.si(c, s)];
                assert!(sl.kind == RESV && sl.p == [NONE; 3], "release of a used reservation");
                self.set_free(c, s);
            }
        }
    }

    /// A producer arrived. Fire at once if this tile has room for the slots the rule needs;
    /// otherwise dock it here, hold what room there is, and look for the rest.
    fn on_ship(&mut self, c: u32, f: Flit) {
        let s = a_slot(f.dst);
        let i = self.si(c, s);
        let cs = self.slots[i];
        assert!(cs.st == AWAIT_SHIP || cs.st == AWAIT_MOVED, "ship arrived at a consumer that did not ask");
        let docked = Docked { tag: f.tag, aux: [f.x, f.y], sid: f.sid };
        let plan = self.plan(c, s, &cs, &docked);
        let short = plan.slots.saturating_sub(plan.reuse as usize + self.free[c as usize] as usize);
        if short == 0 {
            self.fire(c, s, docked, None);
            return;
        }
        let local = self.free[c as usize] as u32;
        let hold = if local > 0 { self.reserve_local(c, local) } else { 0 };
        let sl = &mut self.slots[i];
        sl.st = DOCKED;
        sl.hold = hold;
        sl.p[0] = f.x;
        sl.dock = f.y;
        sl.dock_tag = f.tag;
        sl.psid = f.sid;
        let mut r = Flit::new(RESERVE, addr(c, 0, 0));
        r.n = short as u8;
        r.x = addr(c, s, 0);
        self.stats.sent[RESERVE as usize] += 1;
        self.route_reserve(c, r);
    }

    /// What a rewrite will need: which fresh producers go straight to a waiting reader
    /// (they never take a slot), how many slots the rest take, which of the consumer's
    /// result ports must linger as indirections, and so whether its own slot is reusable.
    fn plan(&self, c: u32, s: u32, cs: &Slot, d: &Docked) -> Plan {
        let ct = tag_of(cs.kind);
        let rule = &RULES[rule_index(cs.kind, d.tag)];
        let mut linger = 0u8;
        for i in 1..ct.arity() {
            if !is_source(ct, i) || cs.p[i] != NONE { continue; }
            let me = addr(c, s, i as u32);
            let read_inside = (1..ct.arity()).any(|j| !is_source(ct, j) && cs.p[j] == me) || d.aux.contains(&me);
            if !read_inside { linger |= 1 << i; }
        }
        let mut direct = 0u8;
        for &(a, b) in rule.wires {
            if let (End::Fresh(k, 0), End::CAux(i)) | (End::CAux(i), End::Fresh(k, 0)) = (a, b) {
                let sub = cs.p[i as usize];
                if rule.fresh[k as usize].is_producer() && is_sub(sub) && sub & SHIPPABLE != 0 {
                    direct |= 1 << k;
                }
            }
        }
        let slots = rule.fresh.len() - direct.count_ones() as usize;
        Plan { linger, direct, slots, reuse: linger == 0 && slots > 0 }
    }

    /// The rewrite. Fresh agents take the consumer's own slot (when it is free to go), the
    /// slots it held while docked, other free slots of this tile, then the granted remote
    /// room. Everything else this changes travels as messages.
    fn fire(&mut self, c: u32, s: u32, d: Docked, grant: Option<(u32, u16)>) {
        let ci = self.si(c, s);
        let cs = self.slots[ci];
        let ct = tag_of(cs.kind);
        let ri = rule_index(cs.kind, d.tag);
        let rule = &RULES[ri];
        self.stats.fires += 1;
        if let Some(r) = self.rec.as_mut() { r.fires.push([c, ri as u32]); }

        let fresh_ids = match self.shadow.as_mut() {
            Some(net) => {
                assert_eq!(net.get(cs.sid).ports[0], Some((d.sid, 0)), "fired a pair the abstract net does not have");
                assert_eq!(net.get(d.sid).tag, tag_of(d.tag));
                net.fire(cs.sid, d.sid).1
            }
            None => vec![NONE; rule.fresh.len()],
        };

        let plan = self.plan(c, s, &cs, &d);
        let n = rule.fresh.len();
        let mut tgt = [NONE; 6];
        let mut places = (0..n).filter(|&k| plan.direct & (1 << k) == 0);
        let mut put = |a: u32| match places.next() {
            Some(k) => { tgt[k] = a; true }
            None => false,
        };
        let mut placed = 0;
        if plan.reuse && put(addr(c, s, 0)) { placed += 1; }
        let mut spare_hold = cs.hold;
        for b in 0..self.cfg.k {
            if placed < plan.slots && cs.hold & (1 << b) != 0 && put(addr(c, b, 0)) {
                spare_hold &= !(1 << b);
                placed += 1;
            }
        }
        for b in 0..self.cfg.k {
            if placed == plan.slots { break; }
            if b != s && self.slots[self.si(c, b)].kind == FREE && put(addr(c, b, 0)) { placed += 1; }
        }
        let mut unused = 0u16;
        if let Some((gc, gmask)) = grant {
            unused = gmask;
            for b in 0..self.cfg.k {
                if placed < plan.slots && gmask & (1 << b) != 0 && put(addr(gc, b, 0)) {
                    unused &= !(1 << b);
                    placed += 1;
                }
            }
        }
        assert_eq!(placed, plan.slots, "not enough room to fire");
        if tgt[..n].iter().all(|&a| a == NONE || a_cell(a) == c) { self.stats.local_fires += 1; }

        let mut fresh = [Slot::EMPTY; 6];
        for (k, t) in rule.fresh.iter().enumerate() {
            fresh[k] = Slot { kind: code(*t), sid: fresh_ids[k], ..Slot::EMPTY };
        }
        let aux = [NONE, d.aux[0], d.aux[1]];
        let own = |r: u32| r != NONE && a_cell(r) == c && a_slot(r) == s && is_source(ct, a_port(r));
        let have = |e: End| -> u32 {
            match e {
                End::Fresh(k, p) => tgt[k as usize] | p as u32,
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
                e = partner(End::CAux(a_port(have(e)) as u8));
            }
            panic!("vicious circle inside a rewrite")
        };
        let read_inside = |i: usize| -> bool {
            let me = addr(c, s, i as u32);
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
                    } else {
                        ind[i as usize] = src;
                    }
                }
                End::PAux(_) => unreachable!("producer aux ports are sinks"),
            }
        }

        // The consumer's slot dies, lingers as indirections, or hosts a fresh agent.
        self.set_free(c, s);
        if plan.linger != 0 {
            self.place(c, s, Slot { kind: IND, st: plan.linger, p: ind, ..Slot::EMPTY });
        }
        for &(k, to) in &ships[..n_ships] {
            let mut f = Flit::new(SHIP, to);
            f.tag = fresh[k].kind;
            f.x = fresh[k].p[1];
            f.y = fresh[k].p[2];
            f.sid = fresh[k].sid;
            self.stats.direct_ships += 1;
            self.emit(c, f);
        }
        for f in &out[..n_out] { self.emit(c, *f); }
        for (k, t) in rule.fresh.iter().enumerate() {
            let a = tgt[k];
            if a == NONE { continue; }
            if a_cell(a) == c {
                self.place(c, a_slot(a), fresh[k]);
                if t.is_consumer() {
                    let wanted = eager(*t) || (1..t.arity()).any(|p| is_source(*t, p) && is_sub(fresh[k].p[p]));
                    if wanted { self.push_event(c, Ev::Activate(a_slot(a))); }
                }
            } else {
                let mut f = Flit::new(SPAWN, a);
                f.tag = fresh[k].kind;
                f.x = fresh[k].p[0];
                f.y = fresh[k].p[1];
                f.z = fresh[k].p[2];
                f.sid = fresh[k].sid;
                self.emit(c, f);
            }
        }
        for b in 0..self.cfg.k {
            if spare_hold & (1 << b) != 0 { self.set_free(c, b); }
        }
        if unused != 0 {
            let (gc, _) = grant.unwrap();
            let mut f = Flit::new(RELEASE, addr(gc, 0, 0));
            f.y = unused as u32;
            self.emit(c, f);
        }
    }

    // ---- reading the answer ---------------------------------------------------------------

    /// Follow indirections to the source a reference really names.
    pub fn follow(&self, mut r: u32) -> u32 {
        for _ in 0..10_000_000 {
            let sl = self.slot_at(r);
            if sl.kind != IND { return r; }
            r = sl.p[a_port(r)];
        }
        panic!("indirection cycle")
    }

    /// The value the root holds, if it is a whole constructor tree.
    pub fn readback(&self) -> Option<rust_ca_lattice::oracle::Term> {
        use rust_ca_lattice::oracle::Term;
        use std::rc::Rc;
        if self.out_addr == NONE { return None; }
        let out = *self.slot_at(self.out_addr);
        if out.st != HOLD { return None; }
        fn go(m: &Mesh, r: u32, memo: &mut std::collections::HashMap<u32, Rc<Term>>) -> Option<Rc<Term>> {
            let r = m.follow(r);
            if let Some(t) = memo.get(&r) { return Some(t.clone()); }
            let sl = *m.slot_at(r);
            if !is_agent(sl.kind) || a_port(r) != 0 { return None; }
            let t = match tag_of(sl.kind) {
                Tag::L => Rc::new(Term::L),
                Tag::S => Rc::new(Term::S(go(m, sl.p[1], memo)?)),
                Tag::F => Rc::new(Term::F(go(m, sl.p[1], memo)?, go(m, sl.p[2], memo)?)),
                _ => return None,
            };
            memo.insert(r, t.clone());
            Some(t)
        }
        go(self, out.p[0], &mut std::collections::HashMap::new()).map(|t| (*t).clone())
    }

    /// At quiescence the mesh must be exactly the abstract net: the same agents, every
    /// sink's reference (through indirections) naming the port the abstract net links it
    /// to, and every dropped result port faced by the eraser that dropped it.
    pub fn check_projection(&self) -> Result<(), String> {
        let Some(net) = self.shadow.as_ref() else { return Ok(()) };
        let mut live = 0;
        for (i, sl) in self.slots.iter().enumerate() {
            if sl.kind == RESV { return Err(format!("slot {i} still reserved at quiescence")); }
            if !is_agent(sl.kind) { continue; }
            live += 1;
            let t = tag_of(sl.kind);
            if sl.st == DOCKED { return Err(format!("slot {i} ({}) docked at quiescence", t.name())); }
            let a = net.agents.get(sl.sid as usize).and_then(|a| a.as_ref())
                .ok_or_else(|| format!("slot {i} holds agent {} the abstract net lost", sl.sid))?;
            if a.tag != t { return Err(format!("slot {i}: tag {} vs abstract {}", t.name(), a.tag.name())); }
            for p in 0..t.arity() {
                if is_source(t, p) {
                    if is_dead(sl.p[p]) {
                        live += 1;
                        let e = dead_sid(sl.p[p]);
                        let ea = net.agents.get(e as usize).and_then(|a| a.as_ref());
                        if ea.map(|a| (a.tag, a.ports[0])) != Some((Tag::Eps, Some((sl.sid, p as u8)))) {
                            return Err(format!("slot {i} port {p}: dropped, but eraser {e} does not face it"));
                        }
                    }
                    continue;
                }
                let r = self.follow(sl.p[p]);
                let tsl = self.slot_at(r);
                if !is_agent(tsl.kind) { return Err(format!("slot {i} port {p} references a non-agent")); }
                if a.ports[p] != Some((tsl.sid, a_port(r) as u8)) {
                    return Err(format!("slot {i} ({}) port {p}: mesh {:?} vs abstract {:?}",
                        t.name(), (tsl.sid, a_port(r)), a.ports[p]));
                }
            }
        }
        if live != net.live_count() {
            return Err(format!("{live} agents on the mesh, {} in the abstract net", net.live_count()));
        }
        Ok(())
    }

    /// Every consumer still on the mesh, and what its reference really names.
    pub fn dump_waiting(&self) -> Vec<String> {
        let mut out = vec![];
        for (i, sl) in self.slots.iter().enumerate() {
            if !is_agent(sl.kind) || !tag_of(sl.kind).is_consumer() { continue; }
            let r = sl.p[0];
            let what = (r != NONE).then(|| { let a = self.follow(r); let x = self.slot_at(a); (x.kind, x.st, a_port(a)) });
            out.push(format!("slot {i} tile {} {} st={} p={:x?} -> {what:?}",
                i as u32 / self.cfg.k, tag_of(sl.kind).name(), sl.st, sl.p));
        }
        out.push(format!("parked reserves {}", self.stats.parked_reserves));
        out
    }
}

struct Plan {
    linger: u8,
    direct: u8,
    slots: usize,
    reuse: bool,
}
