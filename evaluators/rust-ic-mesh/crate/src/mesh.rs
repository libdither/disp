//! The mesh: a W×H grid of tiles. Each tile holds K agent slots, a five-port router, and a
//! protocol engine (`tile.rs`) that handles one event per tick. Wires are not matter: a
//! wire is a reference held by its sink, naming the source's (tile, slot, port) address,
//! and every source has exactly one reader.
//!
//! A tick has five phases, each touching state in a way that cannot race:
//! 1. decide: every router picks which flits move, from start-of-tick state only;
//! 2. pop: each chosen flit leaves its queue (a queue is popped only by its own tile);
//! 3. push: each flit enters the next tile's input buffer (each buffer has one writer);
//! 4. events: each tile runs its protocol on itself alone;
//! 5. the free-space field relaxes one step.
//! So the native build runs tiles on all cores and gets the same machine, tick for tick,
//! as the one-thread browser build.

use crate::polarity::is_source;
use crate::tile::{Out, Tile};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::rules::{Tag, ALL_TAGS};
use std::collections::VecDeque;

pub const NONE: u32 = u32::MAX;
pub const K_MAX: u32 = 16;

// Slot kinds: 0 free, 1..=13 the agent tags (ALL_TAGS order; `Sup`, the 14th, never loads),
// then two plumbing kinds.
pub const FREE: u8 = 0;
pub const IND: u8 = 14;
pub const RESV: u8 = 15;

// Reader phases (consumers and Out).
pub const IDLE: u8 = 0;
/// Pulled its source; a `Ship` or a `Moved` will answer.
pub const AWAIT: u8 = 2;
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

/// `Moved` flags: the reader is already subscribed at the new source; the new source is a
/// value the reader should hold rather than pull (only the root reads that way).
pub(crate) const PRESUB: u8 = 1;
pub(crate) const VALUE: u8 = 2;

// Flags on the value a source port keeps for its reader. Addresses fit in 30 bits.
pub(crate) const ADDR: u32 = 0x3FFF_FFFF;
/// The reader is a consumer, so a producer may be shipped straight to it.
pub(crate) const SHIPPABLE: u32 = 1 << 30;
/// The reader dropped this source; the low bits keep the eraser's shadow id.
pub(crate) const DEAD: u32 = 1 << 31;
#[inline] pub(crate) fn is_dead(v: u32) -> bool { v != NONE && v & DEAD != 0 }
#[inline] pub(crate) fn is_sub(v: u32) -> bool { v != NONE && v & DEAD == 0 }
#[inline] pub(crate) fn dead(eps_sid: u32) -> u32 { DEAD | (eps_sid & 0x3FFF_FFFF) }
#[inline] pub(crate) fn dead_sid(v: u32) -> u32 { if v & 0x3FFF_FFFF == 0x3FFF_FFFF { NONE } else { v & 0x3FFF_FFFF } }

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

/// Port permutations: an agent's logical port q sits at physical port PERMS[perm][q]. A
/// fresh agent that inherits a dying consumer's slot is permuted so its result lands on the
/// physical port the consumer's reader already names. Everything else is the identity.
pub const PERMS: [[u8; 3]; 6] = [[0, 1, 2], [2, 1, 0], [1, 0, 2], [0, 2, 1], [1, 2, 0], [2, 0, 1]];
#[inline] pub fn logical(perm: u8, phys: usize) -> usize {
    PERMS[perm as usize].iter().position(|&x| x as usize == phys).unwrap()
}
#[inline] pub fn physical(perm: u8, q: usize) -> u32 { PERMS[perm as usize][q] as u32 }
/// The permutation that puts logical port `q` on physical port `phys` (a transposition).
pub(crate) fn perm_to(q: usize, phys: usize) -> u8 {
    (0..6u8).find(|&p| PERMS[p as usize][q] as usize == phys && PERMS[p as usize].iter().enumerate()
        .all(|(l, &x)| l == q || l == phys || x as usize == l)).unwrap()
}

/// One agent slot. For a sink port `p[i]` is the reference it holds; for a source port it
/// is the reader that subscribed (or NONE, or DEAD). An indirection keeps its targets in
/// the ports named by `st` (a bitmask); a reserved slot keeps readers that arrived before
/// the agent did. A docked consumer keeps the producer it pulled in `p[0]`, `dock` and
/// `dock_tag`, and the local slots it is holding in `hold`.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Slot {
    pub kind: u8,
    pub st: u8,
    pub perm: u8,
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
        Slot { kind: FREE, st: 0, perm: 0, dock_tag: 0, hold: 0, p: [NONE; 3], dock: NONE, sid: NONE, psid: NONE };
}

/// A message, one flit. Fields are interpreted per kind (see `tile.rs`).
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
    pub(crate) fn new(kind: u8, dst: u32) -> Flit {
        Flit { kind, tag: 0, n: 0, hops: 0, dst, x: NONE, y: NONE, z: NONE, sid: NONE }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Ev {
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
    /// Speculation: a computation nobody has asked for yet starts anyway when its tile has
    /// at least this many free slots (0 = never; demand only).
    pub speculate: u32,
    /// Hardware queue sizes (0 = unbounded). A tile starts an event only when its outbox
    /// has room for everything one event can send ([`BURST`]); its router delivers to it
    /// only while its event queue is below `events_cap`.
    pub outbox_cap: u32,
    pub events_cap: u32,
}

/// The most messages one event can put in a tile's outbox (a rewrite: two direct ships,
/// two moves or drops, six spawns and a release, rounded up).
pub const BURST: u32 = 12;

impl Default for Config {
    fn default() -> Self {
        Config { w: 64, h: 64, k: 8, fifo: 4, events_per_tick: 1, init_fill: 3, speculate: 0, outbox_cap: 0, events_cap: 0 }
    }
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
    /// Fires per rule, in rule-table order.
    pub rule_fires: [u64; 26],
}

impl Stats {
    fn absorb(&mut self, o: &Out) {
        self.fires += o.fires;
        self.cancels += o.cancels;
        self.direct_ships += o.direct_ships;
        self.events += o.events;
        self.local_fires += o.local_fires;
        for k in 0..N_KINDS { self.sent[k] += o.sent[k]; }
        for r in 0..26 { self.rule_fires[r] += o.rule_fires[r]; }
        let add = |x: &mut u64, d: i64| *x = (*x as i64 + d) as u64;
        add(&mut self.live, o.live);
        add(&mut self.inds, o.inds);
        add(&mut self.reserved, o.reserved);
        add(&mut self.in_flight, o.in_flight);
        add(&mut self.parked_reserves, o.parked);
        self.max_outbox = self.max_outbox.max(o.max_outbox);
        self.max_events = self.max_events.max(o.max_events);
        self.max_reserve_hops = self.max_reserve_hops.max(o.max_reserve_hops);
    }
    fn peaks(&mut self) {
        self.peak_live = self.peak_live.max(self.live);
        self.peak_inds = self.peak_inds.max(self.inds);
        self.peak_reserved = self.peak_reserved.max(self.reserved);
        self.peak_in_flight = self.peak_in_flight.max(self.in_flight);
    }
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
    /// Work is pending but nothing has moved for a long time: full queues waiting on each other.
    Deadlock,
    /// Tick budget exhausted.
    Running,
}

impl Outcome {
    pub fn name(self) -> &'static str {
        match self {
            Outcome::Done => "done",
            Outcome::OutOfSpace => "out-of-space",
            Outcome::Stuck => "stuck",
            Outcome::Deadlock => "deadlock",
            Outcome::Running => "running",
        }
    }
}

/// Raw views of the per-tile state, so phases can hand disjoint tiles to different
/// threads. Each phase's comment says why its accesses are disjoint.
#[derive(Clone, Copy)]
struct Raw {
    slots: *mut Slot,
    free: *mut u8,
    rr: *mut u8,
    outbox: *mut VecDeque<Flit>,
    events: *mut VecDeque<Ev>,
    parked: *mut Vec<Flit>,
    fifo_buf: *mut Flit,
    fifo_head: *mut u8,
    fifo_len: *mut u8,
    k: u32,
    b: usize,
}
unsafe impl Send for Raw {}
unsafe impl Sync for Raw {}

/// Run `f` over chunks of `items`: on all cores when `parallel`, else as one chunk.
fn chunked<T: Sync, R: Send>(items: &[T], parallel: bool, f: impl Fn(&[T]) -> R + Sync + Send) -> Vec<R> {
    #[cfg(not(target_arch = "wasm32"))]
    if parallel {
        use rayon::prelude::*;
        let chunk = (items.len() / (4 * rayon::current_num_threads())).max(64);
        return items.par_chunks(chunk).map(f).collect();
    }
    let _ = parallel;
    vec![f(items)]
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
    spare: Vec<u32>,
    stamp: Vec<u32>,
    epoch: u32,
    phi_dirty: Vec<u32>,
    phi_stamp: Vec<u32>,
    phi_epoch: u32,
    /// Scratch for the serial tick, kept to avoid reallocating every tick.
    moves: Vec<(u32, u8, u8)>,
    sends: Vec<(u32, u8, Flit)>,
    /// Precomputed geometry: each tile's (x, y), and its neighbour in each direction.
    xy: Vec<(u16, u16)>,
    nb: Vec<[u32; 4]>,
    pub tick: u64,
    pub stats: Stats,
    pub shadow: Option<Net>,
    pub rec: Option<Recorder>,
    pub out_addr: u32,
    /// Consecutive ticks with work pending and nothing moving.
    pub stalled: u32,
    /// Step tiles on all cores when this many or more are active (and nothing is being
    /// shadow-checked, which needs one global order). `usize::MAX` keeps it on one thread.
    pub par_min: usize,
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
            spare: vec![],
            stamp: vec![0; cells],
            epoch: 1,
            phi_dirty: vec![],
            phi_stamp: vec![0; cells],
            phi_epoch: 1,
            moves: vec![],
            sends: vec![],
            xy: (0..cells as u32).map(|c| ((c % cfg.w) as u16, (c / cfg.w) as u16)).collect(),
            nb: (0..cells as u32).map(|c| {
                let (x, y, w, h) = (c % cfg.w, c / cfg.w, cfg.w, cfg.h);
                [if x + 1 < w { c + 1 } else { NONE }, if x > 0 { c - 1 } else { NONE },
                 if y + 1 < h { c + w } else { NONE }, if y > 0 { c - w } else { NONE }]
            }).collect(),
            tick: 0,
            stats: Stats::default(),
            shadow: None,
            rec: None,
            out_addr: NONE,
            par_min: 4096,
            stalled: 0,
        }
    }

    #[inline] pub fn cells(&self) -> u32 { self.cfg.w * self.cfg.h }
    #[inline] fn si(&self, cell: u32, slot: u32) -> usize { (cell * self.cfg.k + slot) as usize }
    #[inline] pub fn slot_at(&self, a: u32) -> &Slot { &self.slots[self.si(a_cell(a), a_slot(a))] }

    /// Dimension-order routing: X first, then Y. Deadlock-free on a mesh.
    #[inline]
    fn route(&self, c: u32, dst_cell: u32) -> usize {
        let ((x, y), (dx, dy)) = (self.xy[c as usize], self.xy[dst_cell as usize]);
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

    fn raw(&mut self) -> Raw {
        Raw {
            slots: self.slots.as_mut_ptr(),
            free: self.free.as_mut_ptr(),
            rr: self.rr.as_mut_ptr(),
            outbox: self.outbox.as_mut_ptr(),
            events: self.events.as_mut_ptr(),
            parked: self.parked.as_mut_ptr(),
            fifo_buf: self.fifo_buf.as_mut_ptr(),
            fifo_head: self.fifo_head.as_mut_ptr(),
            fifo_len: self.fifo_len.as_mut_ptr(),
            k: self.cfg.k,
            b: self.cfg.fifo as usize,
        }
    }

    /// Fold a phase's per-tile output into the machine.
    fn absorb(&mut self, o: Out) {
        self.stats.absorb(&o);
        for &c in &o.phi_dirty { self.dirty_phi(c); }
        if let Some(r) = self.rec.as_mut() { r.fires.extend_from_slice(&o.rec_fires); }
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
            if a.tag == Tag::Sup || a.label != 0 { return Err("the mesh runs no superpositions".into()); }
            let mut slot = Slot { kind: code(a.tag), sid: id, ..Slot::EMPTY };
            for p in 0..a.tag.arity() {
                if !is_source(a.tag, p) {
                    let (b, q) = a.ports[p].ok_or("open port in the initial net")?;
                    slot.p[p] = at[b as usize] | q as u32;
                }
            }
            let base = at[id as usize];
            let i = m.si(a_cell(base), a_slot(base));
            m.slots[i] = slot;
            m.free[a_cell(base) as usize] -= 1;
            m.stats.live += 1;
        }
        m.stats.peaks();
        m.out_addr = at[out as usize];
        m.init_phi();
        for &id in &order {
            if crate::tile::eager(net.get(id).tag) {
                let base = at[id as usize];
                m.events[a_cell(base) as usize].push_back(Ev::Activate(a_slot(base)));
                m.touch(a_cell(base));
            }
        }
        if check { m.shadow = Some(net); }
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
                for &nb in &self.nb[c as usize] {
                    if nb != NONE && self.phi[nb as usize][f] == INF && d + 1 < cap {
                        self.phi[nb as usize][f] = d + 1;
                        q.push_back(nb);
                    }
                }
            }
        }
    }

    fn phi_cap(&self) -> u8 { (self.cfg.w + self.cfg.h).min(250) as u8 }

    // ---- the tick -------------------------------------------------------------------------

    pub fn quiescent(&self) -> bool { self.active.is_empty() }

    pub fn step(&mut self) {
        self.tick += 1;
        self.stats.ticks = self.tick;
        if let Some(r) = self.rec.as_mut() {
            r.moves.clear();
            r.fires.clear();
        }
        let mut cur = std::mem::take(&mut self.spare);
        cur.clear();
        std::mem::swap(&mut cur, &mut self.active);
        self.epoch += 1;
        let before = (self.stats.events, self.stats.hops);
        if cur.len() >= self.par_min && self.shadow.is_none() {
            // Contiguous stretches of the grid per thread keep threads off each other's lines.
            cur.sort_unstable();
            self.phases_parallel(&cur);
        } else {
            self.phases_serial(&cur);
        }
        for &c in &cur {
            let ci = c as usize;
            if !self.events[ci].is_empty() || !self.outbox[ci].is_empty()
                || self.fifo_len[ci * 4..ci * 4 + 4].iter().any(|&l| l > 0) {
                self.touch(c);
            }
        }
        let moved = (self.stats.events, self.stats.hops) != before || !self.phi_dirty.is_empty();
        self.stalled = if moved || self.active.is_empty() { 0 } else { self.stalled + 1 };
        self.spare = cur;
        self.stats.peaks();
        self.relax_field();
    }

    /// Phase 1 for one tile: which flits its router moves this tick, from start-of-tick
    /// state only.
    #[inline]
    fn decide(&self, c: u32, moves: &mut Vec<(u32, u8, u8)>) {
        let b = self.cfg.fifo as usize;
        let start = self.rr[c as usize] as usize;
        let mut used = [false; 5];
        for j in 0..5 {
            let i = if start + j >= 5 { start + j - 5 } else { start + j };
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
                let nb = self.nb[c as usize][o];
                if self.fifo_len[(nb * 4) as usize + (o ^ 1)] as usize >= b { continue; }
            } else if self.cfg.events_cap > 0 && self.events[c as usize].len() >= self.cfg.events_cap as usize {
                continue;
            }
            used[o] = true;
            moves.push((c, i as u8, o as u8));
        }
    }

    fn phases_serial(&mut self, cur: &[u32]) {
        let mut moves = std::mem::take(&mut self.moves);
        moves.clear();
        for &c in cur { self.decide(c, &mut moves); }
        let raw = self.raw();
        let mut out = Out { record: self.rec.is_some(), ..Out::default() };
        let mut sends = std::mem::take(&mut self.sends);
        sends.clear();
        for &m in &moves { unsafe { raw.pop(&self.nb, m, &mut sends, &mut out) }; }
        for &(nb, o, f) in &sends {
            unsafe { raw.push(nb, o, f) };
            self.stats.hops += 1;
            if let Some(r) = self.rec.as_mut() {
                r.moves.push([self.nb[nb as usize][o as usize ^ 1], o as u32 | (f.kind as u32) << 4 | (f.tag as u32) << 8]);
            }
            self.touch(nb);
        }
        let (phi, cfg) = (&self.phi, self.cfg);
        let mut shadow = self.shadow.as_mut();
        for &c in cur {
            let mut t = unsafe { raw.tile(c, cfg, &self.nb, phi, shadow.as_deref_mut(), &mut out) };
            t.run(cfg.events_per_tick);
        }
        self.absorb(out);
        self.moves = moves;
        self.sends = sends;
    }

    /// The same phases on all cores. Each phase's accesses are disjoint: decide only reads;
    /// each move pops a different queue (an input buffer or the outbox of the tile that owns
    /// it, and a tile ejects at most once, into its own events); each input buffer is pushed
    /// by exactly one neighbour through one port; and each tile's events touch only itself.
    fn phases_parallel(&mut self, cur: &[u32]) {
        let record = self.rec.is_some();
        let this = &*self;
        let decided: Vec<Vec<(u32, u8, u8)>> = chunked(cur, true, |tiles| {
            let mut moves = Vec::with_capacity(tiles.len() * 2);
            for &c in tiles { this.decide(c, &mut moves); }
            moves
        });
        let raw = self.raw();
        let nb_tab = &self.nb;
        let popped: Vec<(Vec<(u32, u8, Flit)>, Out)> = chunked(&decided, true, |groups| {
            let raw = &raw;
            let (mut sends, mut out) = (vec![], Out::default());
            for moves in groups {
                for &m in moves { unsafe { raw.pop(nb_tab, m, &mut sends, &mut out) }; }
            }
            (sends, out)
        });
        let pushed: Vec<(Vec<u32>, Vec<[u32; 2]>)> = chunked(&popped, true, |groups| {
            let raw = &raw;
            let (mut touched, mut rec) = (vec![], vec![]);
            for (sends, _) in groups {
                for &(nb, o, f) in sends {
                    unsafe { raw.push(nb, o, f) };
                    touched.push(nb);
                    if record { rec.push([nb_tab[nb as usize][o as usize ^ 1], o as u32 | (f.kind as u32) << 4 | (f.tag as u32) << 8]); }
                }
            }
            (touched, rec)
        });
        let (phi, cfg) = (&self.phi, self.cfg);
        let outs: Vec<Out> = chunked(cur, true, |tiles| {
            let raw = &raw;
            let mut out = Out { record, ..Out::default() };
            for &c in tiles {
                let mut t = unsafe { raw.tile(c, cfg, nb_tab, phi, None, &mut out) };
                t.run(cfg.events_per_tick);
            }
            out
        });
        for (_, o) in popped { self.absorb(o); }
        for o in outs { self.absorb(o); }
        for (touched, rec) in pushed {
            self.stats.hops += touched.len() as u64;
            for c in touched { self.touch(c); }
            if let Some(r) = self.rec.as_mut() { r.moves.extend_from_slice(&rec); }
        }
    }

    /// The free-space field relaxes one step per tick, as the hardware would.
    fn relax_field(&mut self) {
        if self.phi_dirty.is_empty() { return; }
        let dirty = std::mem::take(&mut self.phi_dirty);
        self.phi_epoch += 1;
        let cap = self.phi_cap();
        // Every tile reads last tick's values (as simultaneous hardware would), so the
        // result does not depend on the order tiles are visited in.
        let mut updates = Vec::with_capacity(dirty.len());
        for &c in &dirty {
            let mut new = [INF; FIELDS];
            for f in 0..FIELDS {
                if self.free[c as usize] as usize > f {
                    new[f] = 0;
                } else {
                    let mut best = INF;
                    for &nb in &self.nb[c as usize] {
                        if nb != NONE { best = best.min(self.phi[nb as usize][f]); }
                    }
                    new[f] = if best >= cap - 1 { INF } else { best + 1 };
                }
            }
            if new != self.phi[c as usize] { updates.push((c, new)); }
        }
        for (c, new) in updates {
            let old = std::mem::replace(&mut self.phi[c as usize], new);
            for nb in self.nb[c as usize] {
                if nb != NONE { self.dirty_phi(nb); }
            }
            if (0..FIELDS).any(|f| new[f] < old[f]) && !self.parked[c as usize].is_empty() {
                let mut out = Out::default();
                let raw = self.raw();
                let (phi, cfg) = (&self.phi, self.cfg);
                unsafe { raw.tile(c, cfg, &self.nb, phi, None, &mut out) }.wake_parked();
                self.absorb(out);
                self.touch(c);
            }
        }
    }

    /// Run until quiescent or until the clock reads `max_ticks`.
    pub fn run(&mut self, max_ticks: u64) -> Outcome {
        while !self.quiescent() {
            if self.tick >= max_ticks || self.stalled >= 256 { return self.outcome(); }
            self.step();
        }
        self.outcome()
    }

    pub fn outcome(&self) -> Outcome {
        if self.stalled >= 256 { return Outcome::Deadlock; }
        if !self.quiescent() { return Outcome::Running; }
        if self.readback().is_some() { return Outcome::Done; }
        if self.stats.parked_reserves > 0 { Outcome::OutOfSpace } else { Outcome::Stuck }
    }

    // ---- reading the answer ---------------------------------------------------------------

    /// Follow indirections to the source a reference really names.
    pub fn follow(&self, mut r: u32) -> u32 {
        for _ in 0..10_000_000 {
            let sl = self.slot_at(r);
            if sl.kind != IND { return r; }
            r = sl.p[logical(sl.perm, a_port(r))];
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
            if !is_agent(sl.kind) || logical(sl.perm, a_port(r)) != 0 { return None; }
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
                let q = logical(tsl.perm, a_port(r));
                if a.ports[p] != Some((tsl.sid, q as u8)) {
                    return Err(format!("slot {i} ({}) port {p}: mesh {:?} vs abstract {:?}",
                        t.name(), (tsl.sid, q), a.ports[p]));
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
            let what = (r != NONE).then(|| { let a = self.follow(r); let x = self.slot_at(a); (x.kind, x.st, logical(x.perm, a_port(a))) });
            out.push(format!("slot {i} tile {} {} st={} p={:x?} -> {what:?}",
                i as u32 / self.cfg.k, tag_of(sl.kind).name(), sl.st, sl.p));
        }
        out.push(format!("parked reserves {}", self.stats.parked_reserves));
        out
    }
}

impl Raw {
    /// Phase 2 for one move: the flit leaves its queue; an ejected flit joins its tile's
    /// events, any other is queued to be pushed next door.
    #[inline]
    unsafe fn pop(&self, nb: &[[u32; 4]], (c, i, o): (u32, u8, u8), sends: &mut Vec<(u32, u8, Flit)>, out: &mut Out) {
        let b = self.b;
        let f = if i < 4 {
            let q = (c * 4) as usize + i as usize;
            let head = self.fifo_head.add(q);
            let f = *self.fifo_buf.add(q * b + *head as usize);
            *head = if *head as usize + 1 == b { 0 } else { *head + 1 };
            *self.fifo_len.add(q) -= 1;
            f
        } else {
            (*self.outbox.add(c as usize)).pop_front().unwrap()
        };
        if o as usize == EJECT {
            let ev = &mut *self.events.add(c as usize);
            ev.push_back(Ev::Msg(f));
            out.max_events = out.max_events.max(ev.len() as u64);
        } else {
            sends.push((nb[c as usize][o as usize], o, f));
        }
    }

    /// Phase 3 for one flit: it enters the input buffer of tile `nb` facing direction `o`.
    #[inline]
    unsafe fn push(&self, nb: u32, o: u8, mut f: Flit) {
        let b = self.b;
        let q = (nb * 4) as usize + (o as usize ^ 1);
        let mut tail = *self.fifo_head.add(q) as usize + *self.fifo_len.add(q) as usize;
        if tail >= b { tail -= b; }
        f.hops = f.hops.saturating_add(1);
        *self.fifo_buf.add(q * b + tail) = f;
        *self.fifo_len.add(q) += 1;
    }

    /// One tile's view. Safety: the caller hands out each tile at most once at a time.
    #[allow(clippy::too_many_arguments)]
    unsafe fn tile<'a>(&self, c: u32, cfg: Config, nb: &'a [[u32; 4]], phi: &'a [[u8; FIELDS]],
                       shadow: Option<&'a mut Net>, out: &'a mut Out) -> Tile<'a> {
        let ci = c as usize;
        Tile {
            c,
            k: self.k,
            speculate: cfg.speculate,
            outbox_cap: cfg.outbox_cap,
            slots: std::slice::from_raw_parts_mut(self.slots.add(ci * self.k as usize), self.k as usize),
            free: &mut *self.free.add(ci),
            rr: &mut *self.rr.add(ci),
            outbox: &mut *self.outbox.add(ci),
            events: &mut *self.events.add(ci),
            parked: &mut *self.parked.add(ci),
            nb: &nb[ci],
            phi,
            shadow,
            out,
        }
    }
}
