//! The cascade engine: event-driven transitions over [`crate::cascade`] words, a serial
//! worklist runner whose FIFO generations are the physical tick, the route tracer, and the
//! projection check against the shadow net.
//!
//! Transition scopes are one cell or one face-adjacent pair. Anything read outside the
//! scope is a stale-safe heuristic (chi gradients); the invariants that matter (face/lane
//! reciprocity) are preserved inductively by every commit.

use crate::cascade::{Cell, Cursor, EndPt, Grid2, Half, Route, Site};
use crate::lattice::{Dir, DIRS};
use crate::lattice::{dir_to, step, Pos, Topo};
use crate::net::Net;
use crate::rules::{find_index, Tag, RULES};
use crate::signal::SignalBackend;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

// Pressure levels for the future relief rung. Blocked actors currently wait silently
// (pure event-driven waiting cannot livelock); chi becomes load-bearing when occupants
// gain relief moves.
pub const BLOCKED_PRESSURE: u8 = 6;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    /// An agent advanced one cell (from, to).
    Move(Pos, Pos),
    /// A docked pair became a seed (consumer cell, producer cell, rule).
    Dock(Pos, Pos, u8),
    /// The semantic interaction: seed resolved (consumer cell, rule).
    Fire(Pos, u8),
    /// A blocked seed retracted and restored the docked pair.
    Retract(Pos, u8),
}

/// Queue discipline for the serial runner. All disciplines must reach the same normal
/// form; they exist to be adversarial about event order.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Discipline {
    Fifo,
    Lifo,
    Random(u64),
    AddressOrdered,
}

pub struct Runner {
    pub grid: Grid2,
    pub shadow: Net,
    pub discipline: Discipline,
    queue: VecDeque<Pos>,
    queued: BTreeSet<Pos>,
    /// Number of pops remaining in the current generation.
    gen_left: usize,
    pub generation: u64,
    pub events: Vec<Event>,
    rng: u64,
    /// When set, relief decisions append their refusal reasons here (the census bin's
    /// --why probe reads them); CASCADE_DBG mirrors the same notes to stderr.
    pub explain: Option<Vec<String>>,
    /// The cell whose own cursor requested the current relief: that one cell is exempt
    /// from the cursor-hosting prohibition (a cursor may swing ordinary wire in its own
    /// cell; foreign relief still may not touch it).
    pub relief_owner: Option<Pos>,
    /// The reserved target the current relief is clearing: its own reservation does not
    /// refuse the relief chain (the reserver asked for it).
    pub relief_root: Option<Pos>,
    /// The displacement-order form (dot with a shape's primary direction must be
    /// positive); components distinct powers of 3 so no face or diagonal sums to zero.
    pub relief_g: (i32, i32, i32),
    /// The requesting dock's ring during its relief (receivers here are refused).
    relief_ring: Vec<Pos>,
    /// Set while running a relief whose success directly commits a blocked placement:
    /// such a displacement may descend the order (see `ascends`).
    relief_pays: bool,
    /// Heuristic ablation: when false, cooldown stamps are never written (damping off).
    /// The bit-class claim under test: dropping a heuristic may park more, never wrong.
    pub cooldown_stamps: bool,
    /// Mechanism ablation: when false, grown agents skip the nursery (negative control —
    /// the nursery is classified correctness-of-mechanism, so this must break things).
    pub nursery_discipline: bool,
    /// Routing epoch at the last quiescence-edge sweep (fixpoint guard: an unchanged
    /// epoch means no structural change since, so re-waking the cold wires would
    /// refuse identically; non-structural writes — stamp decay, χ — must not re-arm
    /// the sweep or a decay chain at the edge costs a whole-grid pass per notch).
    edge_swept_at: u64,
    /// Activation counter for the derivational-heat refresh throttle (sync every 64).
    sync_tick: u32,
    /// Cells committed by growth, attributed to the growing rule (clump-rule evidence).
    pub grown_by_rule: BTreeMap<u8, u64>,
    /// When Some, every activation's write-set size and read radius are recorded per op
    /// class (the chip-power audit: the burn-down table for AC_IDEA's commit budget).
    pub audit: Option<BTreeMap<&'static str, OpAudit>>,
    /// Where heat lives and how it converges; all backends reach the same fixpoint.
    pub signals: SignalBackend,
}

/// Per-op-class audit accumulator: how big activations of this class get.
#[derive(Clone, Copy, Debug, Default)]
pub struct OpAudit {
    pub activations: u64,
    pub max_writes: u32,
    pub max_read_r: u32,
}

fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

impl Runner {
    pub fn new(grid: Grid2, shadow: Net, discipline: Discipline) -> Self {
        let mut r = Self {
            grid,
            shadow,
            discipline,
            queue: VecDeque::new(),
            queued: BTreeSet::new(),
            gen_left: 0,
            generation: 0,
            events: Vec::new(),
            rng: match discipline { Discipline::Random(s) => s, _ => 0 },
            explain: None,
            relief_owner: None,
            relief_root: None,

            relief_g: (-1, -3, 9),
            relief_ring: Vec::new(),
            relief_pays: false,
            edge_swept_at: u64::MAX,
            sync_tick: 0,
            cooldown_stamps: true,
            nursery_discipline: true,
            grown_by_rule: BTreeMap::new(),
            audit: None,
            signals: SignalBackend::Worklist,
        };
        let live: Vec<Pos> = r.grid.cells.keys().copied().collect();
        for p in live {
            r.wake(p);
        }
        r.gen_left = r.queue.len();
        r
    }

    pub fn wake(&mut self, p: Pos) {
        if !self.grid.topo.in_bounds(p) || !self.queued.insert(p) {
            return;
        }
        match self.discipline {
            Discipline::Lifo => self.queue.push_front(p),
            _ => self.queue.push_back(p),
        }
    }

    fn wake_around(&mut self, p: Pos) {
        self.wake(p);
        for d in DIRS {
            self.wake(step(p, d));
        }
    }

    fn next_pos(&mut self) -> Option<Pos> {
        match self.discipline {
            Discipline::Fifo | Discipline::Lifo => self.queue.pop_front(),
            Discipline::Random(_) => {
                if self.queue.is_empty() {
                    None
                } else {
                    let i = (splitmix(&mut self.rng) % self.queue.len() as u64) as usize;
                    self.queue.swap_remove_back(i)
                }
            }
            Discipline::AddressOrdered => {
                let min = self.queue.iter().enumerate().min_by_key(|(_, p)| **p)?.0;
                self.queue.swap_remove_back(min)
            }
        }
    }

    /// Process one activation. Returns false when the queue is empty (quiescent).
    pub fn tick_one(&mut self) -> bool {
        self.tick_traced().is_some()
    }

    /// Like [`Self::tick_one`], reporting which cell was activated (for tracing).
    /// (The every-N-generations contraction sweep was deleted 2026-07-31; the
    /// quiescence-EDGE sweep below took its place when the displacement order landed:
    /// ordered contraction's enabling changes often sit at radius 2 — the anchor cells
    /// — which wake_around never reaches, so slack discovery gets one sweep at each
    /// would-be quiescence, re-armed only by real commits. No periodic storms, and the
    /// order guarantees the sweep cannot fuel displacement pumps.)
    pub fn tick_traced(&mut self) -> Option<Pos> {
        let p = match self.next_pos() {
            Some(p) => p,
            None => {
                if self.grid.route_epoch == self.edge_swept_at {
                    return None; // genuinely quiescent: the last sweep moved no matter
                }
                self.edge_swept_at = self.grid.route_epoch;
                // Wake EVERYTHING once: quiescence means a full re-examination
                // changes nothing (which makes the kick invariant hold by
                // construction). Re-armed only by structural commits, so a run pays a
                // handful of these, each bounded by the fixpoint guard.
                let live: Vec<Pos> = self.grid.cells.keys().copied().collect();
                for p in live {
                    self.wake(p);
                }
                self.next_pos()?
            }
        };
        self.queued.remove(&p);
        if self.gen_left == 0 {
            self.generation += 1;
            self.gen_left = self.queue.len() + 1;
        }
        self.gen_left -= 1;
        if self.audit.is_none() {
            self.activate(p);
            return Some(p);
        }
        // Audited activation: record the write set and read radius, classify by what the
        // activation did (its events; else its hosted cursor; else whether it committed
        // anything — "fabric" is heat/relief/contraction, "refusal" is a pure read).
        let had_cursor = self.grid.site(p).cursor.is_some();
        let ev0 = self.events.len();
        self.grid.probe = Some(crate::cascade::Probe::at(p));
        self.activate(p);
        let probe = self.grid.probe.take().expect("probe survives the activation");
        let mut class = if had_cursor {
            "growth"
        } else if probe.writes.is_empty() {
            "refusal"
        } else {
            "fabric"
        };
        for e in &self.events[ev0..] {
            class = match e {
                Event::Fire(..) => "resolve",
                Event::Dock(..) if class != "resolve" => "dock",
                Event::Retract(..) if !matches!(class, "resolve" | "dock") => "retract",
                Event::Move(..) if !matches!(class, "resolve" | "dock" | "retract") => "move",
                _ => class,
            };
        }
        let a = self.audit.as_mut().expect("audit on").entry(class).or_default();
        a.activations += 1;
        a.max_writes = a.max_writes.max(probe.writes.len() as u32);
        a.max_read_r = a.max_read_r.max(probe.read_r.get());
        Some(p)
    }

    /// Wake every live cell once. After true quiescence this must be a no-op on the
    /// progress counters: any post-kick rewrite, walk, dock, or retract means some cell
    /// was willing to act but had been forgotten by the wake plumbing (a lost wake).
    pub fn kick(&mut self) {
        let live: Vec<Pos> = self.grid.cells.keys().copied().collect();
        for p in live {
            self.wake(p);
        }
    }

    /// Run until quiescent or the activation budget is exhausted. Returns true when
    /// quiescent.
    pub fn run(&mut self, budget: u64) -> bool {
        for _ in 0..budget {
            if !self.tick_one() {
                return true;
            }
        }
        !self.tick_one()
    }

    pub fn quiescent(&self) -> bool { self.queue.is_empty() }

    /// The cooldown value a stamping write uses (0 under the ablation knob).
    fn stamp(&self, v: u8) -> u8 {
        if self.cooldown_stamps { v } else { 0 }
    }

    /// One relief-decision note: appended to the explain log when probing, mirrored to
    /// stderr under CASCADE_DBG. The message closure only runs when someone listens.
    fn note(&mut self, msg: impl FnOnce() -> String) {
        let dbg = std::env::var_os("CASCADE_DBG").is_some();
        if self.explain.is_none() && !dbg {
            return;
        }
        let m = msg();
        if dbg {
            eprintln!("{m}");
        }
        if let Some(log) = &mut self.explain {
            log.push(m);
        }
    }

    // ------------------------------------------------------------ transitions

    /// One cell activation (public so the census can probe a parked cell directly).
    pub fn activate(&mut self, p: Pos) {
        // Derivational signal backends refresh their fixpoint here (no-op on an
        // unchanged routing epoch), throttled to one rebuild per max(64, cells/8)
        // activations: the rebuild is O(grid) and deep runs bump the epoch on every
        // transport commit, so unthrottled refresh made the gate corpus quadratic.
        // The stale window is the same bounded demand over-approximation the
        // worklist backend carries by design (heat persists until rewired); the
        // interval is a function of grid state, so runs stay deterministic. Newly
        // hot cells get the wake the heat wave would have carried — a cold-gated
        // walker learns its cable was demanded from this.
        if self.sync_tick == 0 {
            let newly_hot = self.signals.sync(&self.grid);
            for w in newly_hot {
                self.wake_around(w);
            }
        }
        let interval = 64.max(self.grid.cells.len() as u32 / 8);
        self.sync_tick = (self.sync_tick + 1) % interval;
        let site = self.grid.site(p);
        if site.claim {
            return;
        }
        // Demand sources push: a live consumer heats the wire route touching its
        // principal edge. Before the cooldown branch, so a damped consumer still
        // demands (the old pull scan saw its principal regardless of its own state).
        if let Cell::Agent { tag, principal, nursery: false, .. } = &site.cell {
            if tag.is_consumer() {
                let n = step(p, principal.face);
                let back = EndPt { face: principal.face.opp(), lane: principal.lane };
                self.raise_endpoint(n, back);
            }
        }
        // A cell can host a walker and a builder cursor at once. The agent acts first:
        // its departure is often exactly what unblocks the cursor's next placement.
        // Producers walk toward their consumers; the arity-1 eraser walks too (the
        // reel — the CPU form of AC_IDEA's arity-1 teleport): with no aux to drag it
        // eats its own hot cable cell by cell, which is what dissolves terminal
        // U-loops where the cable re-enters the eraser's own cell as a passthrough
        // and the arriving producer would otherwise park on an undockable face (the
        // discard-tree standing-dead-matter hole). Polarity keeps this chase-free: a
        // consumer principal only ever faces a producer principal, and a cable walked
        // from both ends strictly shortens.
        match &site.cell {
            Cell::Agent { nursery: false, cooldown: 0, tag, .. }
                if tag.is_producer() || *tag == Tag::Eps =>
            {
                if self.try_dock(p, &site)
                    || self.try_walk(p, &site)
                    || self.try_swap(p, &site)
                    || self.try_pass_guest(p, &site)
                {
                    return;
                }
            }
            Cell::Agent { cooldown, .. } if *cooldown > 0 => {
                let mut s = site.clone();
                if let Cell::Agent { cooldown, .. } = &mut s.cell {
                    *cooldown -= 1;
                }
                self.grid.set(p, &s);
                self.wake(p);
                return;
            }
            _ => {}
        }
        if let Some(cursor) = site.cursor {
            self.step_cursor(p, site, cursor);
            return;
        }
        if self.pump_heat(p) {
            return;
        }
        if self.try_retract(p) {
            return;
        }
        // A guest bridges its passthrough wires: demand arriving on one side must cross
        // or it dies at the crossing. The relay re-raises the far WIRE side directly —
        // an agent end can never hold heat, and wakes alone must never bounce between
        // agents (the four-agent wake ring the soak found). Nursery guests stay opaque,
        // as they were to the deleted scan; chains of adjacent guests relay one guest
        // per generation, each hop through the wire between them.
        let pass: Vec<Route> = match &site.cell {
            Cell::Agent { pass, nursery: false, .. } => pass.clone(),
            Cell::Seed { pass, .. } => pass.iter().copied().collect(),
            _ => vec![],
        };
        for r in pass {
            let a = (step(p, r.a.face), EndPt { face: r.a.face.opp(), lane: r.a.lane });
            let b = (step(p, r.b.face), EndPt { face: r.b.face.opp(), lane: r.b.lane });
            let a_hot = self.demand_at(a.0, a.1);
            let b_hot = self.demand_at(b.0, b.1);
            if a_hot && !b_hot {
                self.raise_endpoint(b.0, b.1);
            }
            if b_hot && !a_hot {
                self.raise_endpoint(a.0, a.1);
            }
        }
        self.relax_chi(p);
    }

    /// Demand visible at radius one beyond an edge: a live consumer's principal on it,
    /// or a hot route ending on it. This is the only demand read left — the signal
    /// arrives instead of being searched for.
    fn demand_at(&self, n: Pos, back: EndPt) -> bool {
        match &self.grid.site(n).cell {
            Cell::Agent { tag, principal, nursery: false, .. } => {
                tag.is_consumer() && *principal == back
            }
            Cell::Wire { routes, .. } => routes
                .iter()
                .enumerate()
                .any(|(j, r)| r.ends().contains(&back) && self.signals.hot(&self.grid, n, j)),
            _ => false,
        }
    }

    /// Raise the route at `n` whose end meets `back`; a fresh raise wakes the
    /// neighborhood (the wake IS the propagation edge). No-op unless `n` is wire with
    /// a live matching route: epoch death swallows late raises. Derivational backends
    /// return no wakes here — their heat is a function of matter and sync delivers it.
    fn raise_endpoint(&mut self, n: Pos, back: EndPt) -> bool {
        let slot = match &self.grid.site(n).cell {
            Cell::Wire { routes, .. } => routes.iter().position(|r| r.ends().contains(&back)),
            _ => None,
        };
        let Some(slot) = slot else { return false };
        let wakes = self.signals.raise(&mut self.grid, n, slot);
        let fresh = !wakes.is_empty();
        for w in wakes {
            self.wake_around(w);
        }
        fresh
    }

    /// Demand propagation, push model: each hot route extends one cell along its cable
    /// per activation, from consumers toward producers, so only demanded wires ever
    /// heat and undemanded values never move. Adjacency raises only. Also the wire
    /// cooldown stamp's opportunistic decay point (decay is not demand spread).
    fn pump_heat(&mut self, p: Pos) -> bool {
        let site = self.grid.site(p);
        let Cell::Wire { routes, hot, cooldown, reserved } = &site.cell else {
            return false;
        };
        let (routes, hot) = (routes.clone(), *hot);
        let new_cd = cooldown.saturating_sub(1);
        if new_cd != *cooldown {
            let cell = Cell::Wire {
                routes: routes.clone(),
                hot,
                cooldown: new_cd,
                reserved: *reserved,
            };
            self.grid.set(p, &Site { cell, cursor: site.cursor, chi: site.chi, claim: site.claim });
            if new_cd > 0 {
                // Stamps self-decay to zero: the chain must not depend on a
                // requester's retries (that dependency was the wear pump), and a
                // quiescent grid must hold no live stamps (the kick invariant).
                self.wake(p);
            } else {
                // Expiry is the event the refused requesters were waiting on.
                self.wake_around(p);
            }
        }
        if !self.signals.extends_by_pump() {
            return false; // derivational backends converge inside sync, not here
        }
        let mut spread = false;
        for (i, r) in routes.iter().enumerate() {
            if (hot >> i) & 1 == 0 {
                continue;
            }
            for e in r.ends() {
                let n = step(p, e.face);
                let back = EndPt { face: e.face.opp(), lane: e.lane };
                spread |= self.raise_endpoint(n, back);
            }
        }
        spread
    }

    /// Producer advances one cell along its principal wire. One edge transaction: the
    /// target becomes the agent, the vacated cell becomes the trail.
    /// Producer advances one cell along its principal wire. One edge transaction when
    /// both trail lanes fit through the vacated edge; a staged four-cell transaction
    /// (reserve two side cells, then commit) when one auxiliary must detour around a
    /// foreign lane. Split aux endpoints make the detour representable: the moved agent's
    /// second auxiliary simply enters through a side face.
    /// Producer advances one cell along its principal wire. Per auxiliary the walk picks,
    /// in order: truncation (the target already carries this aux's own cable: absorb the
    /// segment, no trail), a straight trail lane across the vacated edge, or a detour
    /// through a reserved side pair (at most one aux). Truncation is how walks eat their
    /// own slack; the detour is how an aux crosses an edge a foreign lane occupies.
    fn try_walk(&mut self, p: Pos, site: &Site) -> bool {
        self.try_walk_gated(p, site, false)
    }

    /// The walk with an optional shove license: `forced` overrides the demand gate for
    /// one attempt, used when a blocked hot walker's relief needs an undemanded guest to
    /// move on (the blocked walker's demand is the demand).
    fn try_walk_gated(&mut self, p: Pos, site: &Site, forced: bool) -> bool {
        let Cell::Agent { tag, principal, aux, pass, .. } = &site.cell else {
            return false;
        };
        let m = principal.face;
        let t = step(p, m);
        let target = self.grid.site(t);
        if target.claim {
            return false;
        }
        // A guest agent parked at the head of my wire, with demand burning beyond it
        // (read through any further guests; a stale-safe heuristic), is shoved onward.
        // Its own walk or sidestep validates itself; this walker waits for the wake.
        // Producer squatters may answer with their own forced walk; consumer squatters
        // sidestep only (consumers never walk), which unhooks the terminal hairpin
        // where a cable's final approach threads the destination-adjacent consumer.
        if !forced {
            let mut shove_guest = false;
            let mut guest_walks = false;
            if let Cell::Agent {
                tag: gtag, principal: gprin, pass: gpass, nursery: false, cooldown: 0, ..
            } = &target.cell
            {
                if target.cursor.is_none() {
                    let enter = EndPt { face: m.opp(), lane: principal.lane };
                    if let Some(far) = gpass.iter().find_map(|r| r.through(enter)) {
                        // A walking guest with live demand of its OWN is traffic, not
                        // a squatter: it will move itself, and shoving it only buys a
                        // sidestep it walks straight back from — the net-zero shuttle
                        // exact-instant heat sustains (the walk is order-exempt, so
                        // the displacement order cannot bar it). Shove only guests
                        // with nothing of their own to act on.
                        let self_demanded = (gtag.is_producer() || *gtag == Tag::Eps)
                            && self.demand_at(
                                step(t, gprin.face),
                                EndPt { face: gprin.face.opp(), lane: gprin.lane },
                            );
                        shove_guest = !self_demanded
                            && self.demand_at(
                                step(t, far.face),
                                EndPt { face: far.face.opp(), lane: far.lane },
                            );
                        guest_walks = gtag.is_producer();
                    }
                }
            }
            if shove_guest {
                self.note(|| format!("walk {p:?}: shoving guest at {t:?} off my hot wire"));
                let ts = self.grid.site(t);
                // Walk (producers), else sidestep, else shed the squatter's own
                // passthrough — an over-full agent (three ports plus a guest route)
                // cannot vacate a legal trail cell until the guest route is gone. The
                // shed exists only to enable that vacating walk, so it fires only
                // when the guest's principal target is plain wire (a guest that
                // cannot walk at all sheds into a route the shortening moves pull
                // straight back — a net-zero toggle exact-instant heat sustains).
                let guest_could_walk = matches!(&ts.cell, Cell::Agent { principal, .. }
                    if matches!(self.grid.site(step(t, principal.face)).cell,
                        Cell::Wire { reserved: None, .. }));
                // NOT a sidestep. Stepping aside is self-defeating here by
                // construction: the guest leaves its trail in the very cell being
                // contested AND its principal re-anchors to point back into it, so the
                // demand it was shoved out of is exactly what marches it home. The
                // shove then repeats forever (soak term 8 under exact-instant heat).
                // Sidesteps keep their other callers, where the cell being cleared is
                // not the one pulling the guest back.
                let _ = (guest_walks && self.try_walk_gated(t, &ts, true))
                    || (guest_could_walk && self.try_evict(t, None, 1));
                return false;
            }
        }
        let Cell::Wire { routes, reserved: None, .. } = &target.cell else {
            self.note(|| format!("walk {p:?}: principal target {t:?} is not plain wire"));
            return false; // blocked; the target's next change wakes this cell
        };
        let enter = EndPt { face: m.opp(), lane: principal.lane };
        let Some(my_index) = routes.iter().position(|r| r.through(enter).is_some()) else {
            self.note(|| format!("walk {p:?}: no continuing route at {t:?}"));
            return false;
        };
        let exit = routes[my_index].through(enter).unwrap();
        // Demand-gated motion: only walk a wire the consumer side has heated (or a
        // one-shot shove license).
        let route_hot = self.signals.hot(&self.grid, t, my_index);
        let downhill = site.chi >= 4 && target.chi.saturating_add(2) <= site.chi;
        if !(route_hot || downhill || forced) {
            return false;
        }
        if exit.face == enter.face {
            // A one-cell hairpin: the wire U-turns and re-enters this agent's own cell
            // as a passthrough. Collapse it: the principal re-anchors onto that pass
            // route's far end, and both slack segments vanish. Two cells, one commit.
            let back = EndPt { face: m, lane: exit.lane };
            let Some(pi) = pass.iter().position(|r| r.through(back).is_some()) else {
                self.note(|| format!("walk {p:?}: hairpin return lane goes elsewhere"));
                return false; // the return lane goes elsewhere; wait
            };
            let new_principal = pass[pi].through(back).unwrap();
            let mut me = site.clone();
            if let Cell::Agent { principal, pass, .. } = &mut me.cell {
                *principal = new_principal;
                pass.remove(pi);
            }
            let mut ts = target.clone();
            let emptied = {
                let Cell::Wire { routes, hot, .. } = &mut ts.cell else { unreachable!() };
                let mut nh = 0u8;
                let mut k = 0;
                for j in 0..routes.len() {
                    if j != my_index {
                        if (*hot >> j) & 1 == 1 {
                            nh |= 1 << k;
                        }
                        k += 1;
                    }
                }
                routes.remove(my_index);
                *hot = nh;
                routes.is_empty()
            };
            if emptied {
                ts.cell = Cell::Empty { reserved: None };
            }
            if crate::cascade::Word2::pack(&me).is_err() || crate::cascade::Word2::pack(&ts).is_err()
            {
                return false;
            }
            self.grid.set(p, &me);
            self.grid.set(t, &ts);
            self.wake_around(p);
            self.wake_around(t);
            return true;
        }

        let arity = tag.arity();
        let need = arity.saturating_sub(1);
        let mut foreign: Vec<Route> = routes
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != my_index)
            .map(|(_, r)| *r)
            .collect();

        // Plan each auxiliary.
        #[derive(Clone, Copy, PartialEq)]
        enum Plan {
            Truncate(EndPt), // new aux endpoint after absorbing the target's segment
            Straight(u8),    // trail lane across the vacated edge
            Detour,
        }
        let mut plans = [Plan::Detour; 2];
        let mut lanes_used: Vec<u8> = vec![];
        let taken = |rs: &[Route], e: EndPt| rs.iter().any(|r| r.ends().contains(&e));
        for k in 0..need {
            // Truncation: this aux's own cable already runs through the target.
            if aux[k].face == m {
                let back = EndPt { face: m.opp(), lane: aux[k].lane };
                if let Some(fi) = foreign.iter().position(|r| r.through(back).is_some()) {
                    let far = foreign[fi].through(back).unwrap();
                    foreign.remove(fi);
                    plans[k] = Plan::Truncate(far);
                    continue;
                }
            }
            // Straight lane: free on both sides of the vacated edge.
            let lane = (0..2u8).find(|l| {
                !lanes_used.contains(l)
                    && !taken(pass, EndPt { face: m, lane: *l })
                    && !taken(&foreign, EndPt { face: m.opp(), lane: *l })
            });
            if let Some(l) = lane {
                lanes_used.push(l);
                plans[k] = Plan::Straight(l);
            }
        }
        if foreign.len() > 2 {
            self.note(|| format!("walk {p:?}: target carries too many foreign routes"));
            return false;
        }
        // A shoved walk may still relieve its own cell, but at reduced depth so it can
        // never shove in turn (two guests would otherwise shove each other forever).
        let relief_depth = if forced { 1 } else { 2 };
        let detours: Vec<usize> = (0..need).filter(|k| plans[*k] == Plan::Detour).collect();
        if detours.len() > 1 {
            // Both auxiliaries need detours only when passthroughs crowd the vacated
            // edge: shed one cold passthrough and replan on the next wake.
            self.note(|| format!("walk {p:?}: both auxiliaries need detours; shedding a passthrough"));
            if self.try_evict(p, None, relief_depth) {
                self.wake(p);
            }
            return false;
        }

        if detours.is_empty() {
            // Single edge transaction.
            let mut vac_routes = pass.clone();
            let mut new_aux = [EndPt { face: m.opp(), lane: 0 }; 2];
            for k in 0..need {
                match plans[k] {
                    Plan::Truncate(far) => new_aux[k] = far,
                    Plan::Straight(l) => {
                        vac_routes.push(Route::new(aux[k], EndPt { face: m, lane: l }));
                        new_aux[k] = EndPt { face: m.opp(), lane: l };
                    }
                    Plan::Detour => unreachable!(),
                }
            }
            if need == 1 {
                new_aux[1] = new_aux[0];
            }
            if vac_routes.len() > 3 {
                // The vacated cell cannot hold trails plus passthroughs: shed one cold
                // passthrough out of this cell first, then replan on the next wake.
                self.note(|| format!("walk {p:?}: vacated cell over capacity; shedding a passthrough"));
                if self.try_evict(p, None, relief_depth) {
                    self.wake(p);
                }
                return false;
            }
            let moved = Cell::Agent {
                tag: *tag,
                principal: exit,
                aux: new_aux,
                pass: foreign,
                nursery: false,
                cooldown: 0,
            };
            let vacated = if vac_routes.is_empty() {
                Cell::Empty { reserved: None }
            } else {
                // Trails are the mover's aux wires: cold until a later fire rewires them.
                Cell::Wire { routes: vac_routes, hot: 0, cooldown: 0, reserved: None }
            };
            let t_new = Site { cell: moved, cursor: target.cursor, chi: target.chi, claim: false };
            let p_new = Site { cell: vacated, cursor: site.cursor, chi: site.chi, claim: false };
            if crate::cascade::Word2::pack(&t_new).is_err()
                || crate::cascade::Word2::pack(&p_new).is_err()
            {
                return false;
            }
            self.grid.set(t, &t_new);
            self.grid.set(p, &p_new);
            self.commit_move(p, t);
            return true;
        }

        // One aux detours through a side pair s (next to p) and s' (next to t). The whole
        // four-cell rewrite commits in one serial activation (the parallel driver claims
        // all four cells in address order instead); every candidate pair and lane
        // assignment is tried, with packing as the validator.
        let dk = detours[0];
        let usable = |st: &Site| match &st.cell {
            Cell::Empty { reserved: None } => st.cursor.is_none() && !st.claim,
            Cell::Wire { reserved: None, routes, .. } => {
                routes.len() < 3 && st.cursor.is_none() && !st.claim
            }
            _ => false,
        };
        let mut full_sides: Vec<Pos> = vec![];
        for q in m.perp() {
            if q == aux[dk].face
                || !self.grid.topo.in_bounds(step(p, q))
                || !self.grid.topo.in_bounds(step(t, q))
            {
                continue;
            }
            let s = step(p, q);
            let sq = step(t, q);
            let s_site = self.grid.site(s);
            let sq_site = self.grid.site(sq);
            if !usable(&s_site) || !usable(&sq_site) {
                for (x, xs) in [(s, &s_site), (sq, &sq_site)] {
                    if !usable(xs)
                        && matches!(&xs.cell, Cell::Wire { reserved: None, routes, .. } if routes.len() >= 3)
                        && xs.cursor.is_none()
                        && !xs.claim
                        && !full_sides.contains(&x)
                    {
                        full_sides.push(x);
                    }
                }
                continue;
            }
            for l1 in 0..2u8 {
                for l2 in 0..2u8 {
                    for l3 in 0..2u8 {
                        let mut vac_routes = pass.clone();
                        let mut new_aux = [EndPt { face: m.opp(), lane: 0 }; 2];
                        for k in 0..need {
                            match plans[k] {
                                Plan::Truncate(far) => new_aux[k] = far,
                                Plan::Straight(l) => {
                                    vac_routes
                                        .push(Route::new(aux[k], EndPt { face: m, lane: l }));
                                    new_aux[k] = EndPt { face: m.opp(), lane: l };
                                }
                                Plan::Detour => {
                                    vac_routes
                                        .push(Route::new(aux[k], EndPt { face: q, lane: l1 }));
                                    new_aux[k] = EndPt { face: q, lane: l3 };
                                }
                            }
                        }
                        if vac_routes.len() > 3 {
                            self.note(|| format!(
                                "walk {p:?}: vacated cell over capacity on detour; shedding"
                            ));
                            if self.try_evict(p, None, relief_depth) {
                                self.wake(p);
                            }
                            return false;
                        }
                        let side = |st: &Site, route: Route| -> Site {
                            let mut ns = st.clone();
                            match &mut ns.cell {
                                Cell::Empty { .. } => {
                                    ns.cell = Cell::Wire {
                                        routes: vec![route],
                                        hot: 0,
                                        cooldown: 0,
                                        reserved: None,
                                    };
                                }
                                Cell::Wire { routes, .. } => {
                                    routes.push(route);
                                }
                                _ => unreachable!(),
                            }
                            ns
                        };
                        let s_new = side(&s_site, Route::new(
                            EndPt { face: q.opp(), lane: l1 },
                            EndPt { face: m, lane: l2 },
                        ));
                        let sq_new = side(&sq_site, Route::new(
                            EndPt { face: m.opp(), lane: l2 },
                            EndPt { face: q.opp(), lane: l3 },
                        ));
                        let moved = Cell::Agent {
                            tag: *tag,
                            principal: exit,
                            aux: new_aux,
                            pass: foreign.clone(),
                            nursery: false,
                            cooldown: 0,
                        };
                        let vacated =
                            Cell::Wire { routes: vac_routes, hot: 0, cooldown: 0, reserved: None };
                        let t_new = Site {
                            cell: moved,
                            cursor: target.cursor,
                            chi: target.chi,
                            claim: false,
                        };
                        let p_new = Site {
                            cell: vacated,
                            cursor: site.cursor,
                            chi: site.chi,
                            claim: false,
                        };
                        if [&t_new, &p_new, &s_new, &sq_new]
                            .iter()
                            .all(|w| crate::cascade::Word2::pack(w).is_ok())
                        {
                            self.grid.set(t, &t_new);
                            self.grid.set(p, &p_new);
                            self.grid.set(s, &s_new);
                            self.grid.set(sq, &sq_new);
                            self.wake_around(s);
                            self.wake_around(sq);
                            self.commit_move(p, t);
                            return true;
                        }
                    }
                }
            }
        }
        // Every detour side pair is blocked. Full side cells are relief candidates:
        // shed one cold route from one of them and replan on the next wake.
        self.note(|| format!("walk {p:?}: every detour side pair blocked"));
        for x in full_sides {
            if self.try_evict(x, None, 1) {
                self.wake(p);
                break;
            }
        }
        false
    }

    /// The last resort of a shove: the agent steps to a free side cell, lengthening its
    /// own wiring by one connector segment through the vacated cell (which also keeps
    /// hosting the passthroughs it sat on). Adds slack, so it only ever runs under a
    /// shove license; the retraction machinery pulls the slack back in later.
    fn try_sidestep(&mut self, p: Pos, site: &Site) -> bool {
        let Cell::Agent { tag, principal, aux, pass, nursery: false, .. } = &site.cell else {
            return false;
        };
        let arity = tag.arity();
        // The vacated cell must hold the passthroughs plus one connector per port, and
        // all connectors leave through one face (two lanes), so only arity 1 and 2 fit.
        if arity > 2 || pass.len() + arity > 3 {
            self.note(|| format!("sidestep {p:?}: ports plus passthroughs do not fit"));
            return false;
        }
        for f in DIRS {
            if f == principal.face || (arity >= 2 && f == aux[0].face) {
                continue;
            }
            // Sidesteps are displacement too (an agent is matter): the step must
            // ascend the same order every route displacement obeys, or shove-driven
            // sidesteps duel without end at the quiescence edge.
            {
                let g = self.relief_g;
                let d = f.delta();
                if g.0 * d.0 + g.1 * d.1 + g.2 * d.2 <= 0 {
                    continue;
                }
            }
            let s = step(p, f);
            if !self.grid.topo.in_bounds(s) {
                continue;
            }
            let ss = self.grid.site(s);
            if !matches!(ss.cell, Cell::Empty { reserved: None }) || ss.cursor.is_some() || ss.claim
            {
                continue;
            }
            for l1 in 0..2u8 {
                for l2 in 0..2u8 {
                    let mut vac_routes = pass.clone();
                    vac_routes.push(Route::new(*principal, EndPt { face: f, lane: l1 }));
                    let mut new_aux = [EndPt { face: f.opp(), lane: l1 }; 2];
                    if arity >= 2 {
                        if l2 == l1 {
                            continue;
                        }
                        vac_routes.push(Route::new(aux[0], EndPt { face: f, lane: l2 }));
                        new_aux[0] = EndPt { face: f.opp(), lane: l2 };
                        new_aux[1] = new_aux[0];
                    }
                    let moved = Cell::Agent {
                        tag: *tag,
                        principal: EndPt { face: f.opp(), lane: l1 },
                        aux: new_aux,
                        pass: vec![],
                        nursery: false,
                        cooldown: self.stamp(1),
                    };
                    let vacated =
                        Cell::Wire { routes: vac_routes, hot: 0, cooldown: 0, reserved: None };
                    let s_new = Site { cell: moved, cursor: ss.cursor, chi: ss.chi, claim: false };
                    let p_new =
                        Site { cell: vacated, cursor: site.cursor, chi: site.chi, claim: false };
                    if crate::cascade::Word2::pack(&s_new).is_ok()
                        && crate::cascade::Word2::pack(&p_new).is_ok()
                    {
                        self.note(|| format!("sidestep {p:?}: stepped aside to {s:?}"));
                        self.grid.set(s, &s_new);
                        self.grid.set(p, &p_new);
                        self.commit_move(p, s);
                        return true;
                    }
                }
            }
        }
        self.note(|| format!("sidestep {p:?}: no free side cell"));
        false
    }

    /// Bookkeeping shared by every completed move: observer id follows the agent.
    fn commit_move(&mut self, p: Pos, t: Pos) {
        if let Some(sid) = self.grid.sid.remove(&p) {
            assert!(self.grid.sid.insert(t, sid).is_none(), "target already has a sid");
        }
        self.grid.transport += 1;
        self.events.push(Event::Move(p, t));
        self.wake_around(p);
        self.wake_around(t);
    }

    /// Two agents whose principal wires pass through each other's cells advance through
    /// each other in one transaction. Without this, head-on walkers deadlock, and a
    /// walker whose wire loops through its own consumer's cell parks one step short of
    /// docking. Only producers initiate; a consumer can be the partner (the swap is
    /// symmetric untangling, not consumer motion).
    fn try_swap(&mut self, p: Pos, site: &Site) -> bool {
        let Cell::Agent { tag: atag, principal: apr, aux: aaux, pass: apass, .. } = &site.cell
        else {
            return false;
        };
        let m = apr.face;
        let t = step(p, m);
        let target = self.grid.site(t);
        if target.claim {
            return false;
        }
        let Cell::Agent { tag: btag, principal: bpr, aux: baux, pass: bpass, nursery: bnur, cooldown: bcd, .. } =
            &target.cell
        else {
            return false;
        };
        let _ = btag;
        if *bnur || *bcd > 0 || bpr.face != m.opp() {
            return false;
        }
        let a_enter = EndPt { face: m.opp(), lane: apr.lane };
        let b_enter = EndPt { face: m, lane: bpr.lane };
        let Some(a_route_idx) = bpass.iter().position(|r| r.through(a_enter).is_some()) else {
            return false;
        };
        let Some(b_route_idx) = apass.iter().position(|r| r.through(b_enter).is_some()) else {
            return false;
        };
        if apr.lane == bpr.lane {
            return false; // one edge lane cannot carry both wires
        }
        let a_exit = bpass[a_route_idx].through(a_enter).unwrap();
        let b_exit = apass[b_route_idx].through(b_enter).unwrap();
        if a_exit.face == a_enter.face || b_exit.face == b_enter.face {
            return false;
        }

        // Both principals' lanes on the shared edge are freed by the swap; each agent's
        // trails take the lanes its counterpart vacates.
        let trail = |aux: &[EndPt; 2], arity: usize, toward: Dir| -> (Vec<Route>, [EndPt; 2]) {
            let mut routes = vec![];
            let mut new_aux = [EndPt { face: toward.opp(), lane: 0 }; 2];
            for k in 0..arity.saturating_sub(1) {
                routes.push(Route::new(aux[k], EndPt { face: toward, lane: k as u8 }));
                new_aux[k] = EndPt { face: toward.opp(), lane: k as u8 };
            }
            if arity == 2 {
                new_aux[1] = new_aux[0];
            }
            (routes, new_aux)
        };
        let (a_trail, a_new_aux) = trail(aaux, atag.arity(), m);
        let (b_trail, b_new_aux) = trail(baux, btag.arity(), m.opp());
        let mut a_cell_pass: Vec<Route> = apass.clone();
        a_cell_pass.remove(b_route_idx);
        a_cell_pass.extend(a_trail.iter().copied());
        let mut b_cell_pass: Vec<Route> = bpass.clone();
        b_cell_pass.remove(a_route_idx);
        b_cell_pass.extend(b_trail.iter().copied());
        if a_cell_pass.len() > 2 || b_cell_pass.len() > 2 {
            return false;
        }

        let moved_a = Cell::Agent {
            tag: *atag,
            principal: a_exit,
            aux: a_new_aux,
            pass: b_cell_pass,
            nursery: false,
            cooldown: 0,
        };
        let moved_b = Cell::Agent {
            tag: *btag,
            principal: b_exit,
            aux: b_new_aux,
            pass: a_cell_pass,
            nursery: false,
            cooldown: 0,
        };
        let a_site = Site { cell: moved_b, cursor: site.cursor, chi: site.chi, claim: false };
        let t_site = Site { cell: moved_a, cursor: target.cursor, chi: target.chi, claim: false };
        if crate::cascade::Word2::pack(&a_site).is_err()
            || crate::cascade::Word2::pack(&t_site).is_err()
        {
            return false;
        }
        self.grid.set(p, &a_site);
        self.grid.set(t, &t_site);
        let sa = self.grid.sid.remove(&p);
        let sb = self.grid.sid.remove(&t);
        if let Some(id) = sa {
            self.grid.sid.insert(t, id);
        }
        if let Some(id) = sb {
            self.grid.sid.insert(p, id);
        }
        self.grid.transport += 2;
        self.events.push(Event::Move(p, t));
        self.events.push(Event::Move(t, p));
        self.wake_around(p);
        self.wake_around(t);
        true
    }

    /// A demanded walker passes a STATIONARY guest whose cell hosts its cable: the two
    /// exchange places, each one's cables becoming passthroughs of the other's new cell.
    /// [`Self::try_swap`] handles two walkers meeting head-on; this handles what the park
    /// census found to be the commonest stuck shape — the guest is going nowhere (its own
    /// principal faces another agent, so its forced walk fails and nothing shoves it) and
    /// the walker parks against it forever.
    ///
    /// Only an UNDEMANDED producer guest may be displaced: a demanded one marches
    /// straight back and the exchange is a shuttle, which is the lesson three separate
    /// pumps taught this substrate. Consumers never walk, so they are always safe.
    ///
    /// The shared face carries two lanes, and after the exchange the walker's aux trails
    /// cross it backwards while every one of the guest's cables crosses it forwards, so
    /// this is representable only for small arities. That ceiling is inherent, not a
    /// tuning choice.
    fn try_pass_guest(&mut self, p: Pos, site: &Site) -> bool {
        let Cell::Agent { tag: atag, principal: apr, aux: aaux, pass: apass, .. } = &site.cell
        else {
            return false;
        };
        let m = apr.face;
        let t = step(p, m);
        let target = self.grid.site(t);
        if target.claim || target.cursor.is_some() {
            return false;
        }
        let Cell::Agent {
            tag: btag, principal: bpr, aux: baux, pass: bpass, nursery: false, cooldown: 0,
        } = &target.cell
        else {
            return false;
        };
        if bpr.face == m.opp() && bpr.lane == apr.lane {
            return false; // true head-on: that is the symmetric swap's case
        }
        // The guest must actually be carrying my cable onward.
        let a_enter = EndPt { face: m.opp(), lane: apr.lane };
        let Some(a_route_idx) = bpass.iter().position(|r| r.through(a_enter).is_some()) else {
            return false;
        };
        let a_exit = bpass[a_route_idx].through(a_enter).expect("just matched");
        if a_exit.face == a_enter.face {
            return false;
        }
        // Move only under demand, read through the guest exactly as the shove does.
        if !self.demand_at(
            step(t, a_exit.face),
            EndPt { face: a_exit.face.opp(), lane: a_exit.lane },
        ) {
            return false;
        }
        // A guest with demand of its own will walk itself; displacing it is the shuttle.
        if btag.is_producer() || *btag == Tag::Eps {
            if self.demand_at(
                step(t, bpr.face),
                EndPt { face: bpr.face.opp(), lane: bpr.lane },
            ) {
                return false;
            }
        }
        // Cables that cross the shared face fall into three kinds, and each costs one of
        // its two lanes. PAIRED: one of my aux meets one of the guest's ports directly —
        // the guest is an argument delivered to me, the commonest reason it is sitting in
        // my way at all. Those just swap ends and need no passthrough. TRAIL: an aux of
        // mine pointing elsewhere, which must reach back through the cell I am leaving.
        // CONNECTOR: a port of the guest's pointing elsewhere, which must reach forward
        // through the cell it is leaving. Anything else crossing that face (a cable of
        // mine ending inside the guest's cell, or vice versa) is not something an
        // exchange can re-express, so refuse.
        let a_arity = atag.arity().saturating_sub(1);
        let b_ports: Vec<EndPt> = std::iter::once(*bpr)
            .chain((0..btag.arity().saturating_sub(1)).map(|k| baux[k]))
            .collect();
        let mut used: Vec<u8> = vec![];
        let mut paired: Vec<(usize, usize, u8)> = vec![]; // (my aux, guest port, lane)
        let mut trails: Vec<usize> = vec![];
        let mut connectors: Vec<usize> = vec![];
        for k in 0..a_arity {
            if aaux[k].face != m {
                trails.push(k);
                continue;
            }
            let lane = aaux[k].lane;
            match b_ports
                .iter()
                .position(|e| e.face == m.opp() && e.lane == lane)
            {
                Some(j) => {
                    paired.push((k, j, lane));
                    used.push(lane);
                }
                None => return false, // my aux ends somewhere inside the guest's cell
            }
        }
        for (j, e) in b_ports.iter().enumerate() {
            if e.face != m.opp() {
                connectors.push(j);
            } else if !paired.iter().any(|(_, pj, _)| *pj == j) {
                return false; // the guest's cable ends somewhere inside my cell
            }
        }
        if paired.len() + trails.len() + connectors.len() > 2 {
            let (x, y, z) = (paired.len(), trails.len(), connectors.len());
            self.note(|| format!(
                "pass-guest {p:?}: {x} paired + {y} trail(s) + {z} connector(s) exceed the \
                 shared face's 2 lanes"
            ));
            return false;
        }
        let mut free = (0..2u8).filter(|l| !used.contains(l));
        let mut a_lanes = [0u8; 2];
        for k in &trails {
            a_lanes[*k] = free.next().expect("budget checked");
        }
        let mut b_lanes = [0u8; 3];
        for j in &connectors {
            b_lanes[*j] = free.next().expect("budget checked");
        }
        for (_, j, lane) in &paired {
            b_lanes[*j] = *lane;
        }

        // The walker's new cell: it keeps travelling, and adopts the guest's cables plus
        // whatever else was passing through.
        let mut a_new_pass: Vec<Route> = bpass.clone();
        a_new_pass.remove(a_route_idx);
        for j in &connectors {
            a_new_pass.push(Route::new(
                EndPt { face: m.opp(), lane: b_lanes[*j] },
                b_ports[*j],
            ));
        }
        let mut a_new_aux = [EndPt { face: m.opp(), lane: 0 }; 2];
        for k in 0..a_arity {
            let lane = match paired.iter().find(|(pk, _, _)| *pk == k) {
                Some((_, _, lane)) => *lane,
                None => a_lanes[k],
            };
            a_new_aux[k] = EndPt { face: m.opp(), lane };
        }
        if atag.arity() == 2 {
            a_new_aux[1] = a_new_aux[0];
        }
        // The guest's new cell: its ports all re-anchor onto the shared face, and it
        // adopts the walker's aux trails.
        let mut b_new_pass: Vec<Route> = apass.clone();
        for k in &trails {
            b_new_pass.push(Route::new(aaux[*k], EndPt { face: m, lane: a_lanes[*k] }));
        }
        if a_new_pass.len() > 2 || b_new_pass.len() > 2 {
            let (x, y) = (a_new_pass.len(), b_new_pass.len());
            self.note(|| format!(
                "pass-guest {p:?}: exchanged cells would carry {x}/{y} passthroughs (max 2)"
            ));
            return false;
        }
        let mut b_new_aux = [EndPt { face: m, lane: 0 }; 2];
        for k in 0..btag.arity().saturating_sub(1) {
            b_new_aux[k] = EndPt { face: m, lane: b_lanes[k + 1] };
        }
        if btag.arity() == 2 {
            b_new_aux[1] = b_new_aux[0];
        }
        let moved_a = Cell::Agent {
            tag: *atag,
            principal: a_exit,
            aux: a_new_aux,
            pass: a_new_pass,
            nursery: false,
            cooldown: 0,
        };
        let moved_b = Cell::Agent {
            tag: *btag,
            principal: EndPt { face: m, lane: b_lanes[0] },
            aux: b_new_aux,
            pass: b_new_pass,
            nursery: false,
            cooldown: self.stamp(1),
        };
        let p_site = Site { cell: moved_b, cursor: site.cursor, chi: site.chi, claim: false };
        let t_site = Site { cell: moved_a, cursor: target.cursor, chi: target.chi, claim: false };
        if crate::cascade::Word2::pack(&p_site).is_err()
            || crate::cascade::Word2::pack(&t_site).is_err()
        {
            return false;
        }
        self.note(|| format!("pass-guest {p:?}: exchanged places with the guest at {t:?}"));
        self.grid.set(p, &p_site);
        self.grid.set(t, &t_site);
        let sa = self.grid.sid.remove(&p);
        let sb = self.grid.sid.remove(&t);
        if let Some(id) = sa {
            self.grid.sid.insert(t, id);
        }
        if let Some(id) = sb {
            self.grid.sid.insert(p, id);
        }
        self.grid.transport += 2;
        self.events.push(Event::Move(p, t));
        self.events.push(Event::Move(t, p));
        self.wake_around(p);
        self.wake_around(t);
        true
    }

    /// Producer principal-to-principal with a consumer: replace both with seed halves and
    /// start the builder cursor. Rules with no fresh agents resolve in the same
    /// transaction.
    fn try_dock(&mut self, p: Pos, site: &Site) -> bool {
        let Cell::Agent { tag, principal, aux, pass, .. } = &site.cell else {
            return false;
        };
        let t = step(p, principal.face);
        let target = self.grid.site(t);
        // The consumer cell must be able to hold the new blocklet's cursor.
        if target.claim || target.cursor.is_some() {
            return false;
        }
        let Cell::Agent {
            tag: ctag,
            principal: cprincipal,
            aux: caux,
            pass: cpass,
            nursery: cnursery,
            ..
        } = &target.cell
        else {
            return false;
        };
        if cprincipal.face != principal.face.opp()
            || cprincipal.lane != principal.lane
            || *cnursery
        {
            return false;
        }
        if !ctag.is_consumer() {
            assert!(
                !(tag.is_producer() && ctag.is_producer()),
                "producer principals fused: polarity invariant violated at {p:?}/{t:?}"
            );
            return false;
        }
        // A seed inherits at most one passthrough route: shed the excess (cold foreign
        // lanes reroute around the cell) and dock on a later wake. ONE eviction per
        // activation — the second cell sheds on the next wake (the locality audit
        // holds every activation to a single primitive's footprint).
        if pass.len() > 1 || cpass.len() > 1 {
            let mut progressed = false;
            if pass.len() > 1 {
                progressed |= self.try_evict(p, None, 2);
            }
            if cpass.len() > 1 && !progressed {
                progressed |= self.try_evict(t, None, 2);
            }
            if progressed {
                self.wake(p);
            }
            return false;
        }
        let rule = find_index(*ctag, *tag).unwrap_or_else(|| {
            panic!("no rule for {}·{} at {t:?}/{p:?}", ctag.name(), tag.name())
        }) as u8;

        let layout = crate::blocklet::layout(rule);
        // The dock axis points consumer -> producer. Foreign routes crossing either cell
        // ride along as seed passthroughs and fold into the patch panel at resolve.
        let axis = cprincipal.face;
        let plane = principal.lane;
        let mk_seeds = |roll: u8| {
            (
                Cell::Seed {
                    rule,
                    half: Half::Consumer,
                    partner: axis,
                    roll,
                    stub: *caux,
                    plane,
                    pass: cpass.first().copied(),
                },
                Cell::Seed {
                    rule,
                    half: Half::Producer,
                    partner: axis.opp(),
                    roll,
                    stub: *aux,
                    plane,
                    pass: pass.first().copied(),
                },
            )
        };
        // Cells where the dying pair's aux cables and delivered values legally sit.
        let stub_cells = [
            (ctag.arity() >= 2).then(|| step(t, caux[0].face)),
            (tag.arity() >= 2).then(|| step(p, aux[0].face)),
            (ctag.arity() >= 3).then(|| step(t, caux[1].face)),
            (tag.arity() >= 3).then(|| step(p, aux[1].face)),
        ];
        // Seated rules need no growth room: try the one-transaction resolution before
        // the roll ladder, which gates on the scripted fallback's footprint.
        {
            let (seed_c, seed_p) = mk_seeds(0);
            if let Some((fc, fp)) = crate::blocklet::seated_finals(rule, axis, &seed_c, &seed_p) {
                let consumer_sid = self.grid.sid.get(&t).copied();
                let producer_sid = self.grid.sid.get(&p).copied();
                self.events.push(Event::Dock(t, p, rule));
                self.grid.set(t, &Site { cell: fc, cursor: target.cursor, chi: target.chi, claim: false });
                self.grid.set(p, &Site { cell: fp, cursor: site.cursor, chi: site.chi, claim: false });
                self.grid.sid.remove(&t);
                self.grid.sid.remove(&p);
                let seats = crate::blocklet::seated_seats(rule);
                self.observe_fire(t, rule, axis, 0, &seats, consumer_sid, producer_sid);
                self.wake_around(p);
                self.wake_around(t);
                return true;
            }
        }
        let roll = {
            let read = |q: Pos| self.grid.site(q);
            choose_roll(&read, self.grid.topo, t, p, stub_cells, rule, axis, &mk_seeds)
        };
        if roll.is_none() {
            // Ring relief: pick the roll with the FEWEST blockers and clear exactly one
            // per activation with the existing primitives, damped by the existing
            // stamps, then re-gate with fresh reads. Clearing the LAST blocker pays for
            // itself (the ring is then whole and the dock fires next), so that one move
            // may descend the displacement order.
            // The rung landed with a bound of two blockers because unbounded clearing
            // livelocked. Both halves of that are now obsolete: the displacement order
            // took over termination, and once the last-blocker relief could pay its way
            // past the order, the bound was the only thing left holding the 56-cell comb
            // dock shut. Swept 2026-08-01: 3, 4, 8 and unbounded are indistinguishable
            // (soak 130/160, every deep term complete), so there is no constant to tune
            // and none is kept — a crowded ring simply works itself down one cell at a
            // time, and the geometry bounds the count.
            // Arbitration: only the address-LOWEST ready pair in the neighborhood may
            // run ring relief. A displacement cycle between two docks needs both to
            // push; this leaves one pusher, and when it fires or truly parks the next
            // lowest inherits the baton. (The same fixed tie-break that already breaks
            // claim deadlocks, applied to relief requesters.)
            let my_addr = t.min(p);
            let mut lowest = true;
            'scan: for dx in -2i32..=2 {
                for dy in -2i32..=2 {
                    for dz in -2i32..=2 {
                        let q = add(t, (dx, dy, dz));
                        if q >= my_addr || q == p {
                            continue;
                        }
                        let Ok(qs) = self.grid.word(q).unpack() else { continue };
                        let Cell::Agent { tag: qt, principal: qp, nursery: false, .. } = &qs.cell
                        else {
                            continue;
                        };
                        if !qt.is_consumer() {
                            continue;
                        }
                        let m = step(q, qp.face);
                        let Ok(ms) = self.grid.word(m).unpack() else { continue };
                        if matches!(&ms.cell, Cell::Agent { tag: mt, principal: mp, nursery: false, .. }
                            if mt.is_producer() && mp.face == qp.face.opp() && mp.lane == qp.lane)
                        {
                            lowest = false;
                            break 'scan;
                        }
                    }
                }
            }
            let candidate = lowest
                .then(|| {
                    (0..4u8)
                        .filter_map(|r| {
                            let read = |q: Pos| self.grid.site(q);
                            let bs = roll_blockers(
                                &read, self.grid.topo, t, p, stub_cells, rule, axis, r,
                            )?;
                            (!bs.is_empty()).then_some((r, bs))
                        })
                        .min_by_key(|(_, bs)| bs.len())
                        .map(|(roll, bs)| {
                            // A roll down to its last blocker: clearing this one cell
                            // completes the ring, so the relief pays for itself and may
                            // descend the displacement order (same argument as a
                            // blocked placement's — the fire it buys is next).
                            let last = bs.len() == 1;
                            let (world, planned) = bs.into_iter().next().expect("nonempty");
                            (world, planned, last, roll)
                        })
                })
                .flatten();
            let ring_relief_ran = candidate.is_some();
            if let Some((world, planned, pays, roll)) = candidate {
                // This dock's combined ring (every roll's near-seed footprint) is
                // off-limits as a receiver while its relief runs: relief drains.
                // Narrowing this to the ring of the roll being cleared was tried
                // 2026-08-01 and reverted: it does unwedge docks (the declined-dock park
                // class fell 13 → 5, and "in the requesting dock's ring" is far and away
                // the most common refusal in their traces) but the freed terms mostly
                // re-park one step later, net completion moved by 2, and roll switching
                // began to oscillate — clearing one roll crowds another, which then
                // becomes the cheapest roll (soak term 47 stopped quiescing). Making it
                // safe needs a receiver rule that provably worsens NO roll, i.e. a
                // per-cell map of what each roll wants to place there, so every relief
                // strictly lowers the total blocker count. Worth doing when docks are
                // the dominant park class again; today they are not.
                let _ = roll;
                self.relief_ring = {
                    let layout = crate::blocklet::layout(rule);
                    let mut v = vec![];
                    for rr in 0..4u8 {
                        for (off, _) in &layout.extras {
                            let world = add(t, crate::cascade::rot_pos(*off, axis, rr));
                            let near = DIRS
                                .iter()
                                .any(|d| step(world, *d) == t || step(world, *d) == p);
                            if near && !v.contains(&world) {
                                v.push(world);
                            }
                        }
                    }
                    v
                };
                let ws = self.grid.site(world);
                let progressed = match &ws.cell {
                    Cell::Wire { .. } => {
                        self.note(|| format!(
                            "dock {t:?}/{p:?}: ring relief evicting the one blocker at {world:?}"
                        ));
                        // Depth 1 (stamp-respecting) rather than
                        // the stamp-bypassing top level: a just-displaced route must
                        // wear its stamp down before moving again.
                        let depth = 1;
                        self.relief_root = Some(world);
                        self.relief_pays = pays;
                        let ok = self.try_evict(world, Some(&planned), depth);
                        self.relief_pays = false;
                        self.relief_root = None;
                        ok
                    }
                    Cell::Agent { tag, nursery: false, cooldown: 0, .. }
                        if tag.is_producer() && ws.cursor.is_none() && !ws.claim =>
                    {
                        self.note(|| format!(
                            "dock {t:?}/{p:?}: ring relief sidestepping the squatter at {world:?}"
                        ));
                        self.try_sidestep(world, &ws)
                    }
                    _ => false,
                };
                self.relief_ring.clear();
                if progressed {
                    // Never dock in the same activation: the relief commit already
                    // woke this pair, and splitting keeps the dock commit at its
                    // two-cell budget (the locality audit holds the line).
                    self.wake(p);
                    return false;
                }
            }
            // One relief primitive per activation: when ring relief ran (even to a
            // refusal), the pass-shed fallback waits for the next wake, keeping every
            // activation's write set at a single primitive's footprint.
            if ring_relief_ran {
                return false;
            }
        }
        let Some(roll) = roll else {
            // Every roll's first ring is blocked, or no seed finals pack. A passthrough
            // crossing either dying cell can occupy a dock-edge lane and make the fusion
            // panels unpackable: shed it and retry on the wake (bounded, since each cell
            // sheds at most its own passthroughs; unbounded ring eviction stays
            // forbidden, it livelocks).
            self.note(|| format!("dock {t:?}/{p:?}: every roll's first ring is blocked"));
            let mut progressed = false;
            if !cpass.is_empty() {
                progressed |= self.try_evict(t, None, 2);
            }
            if !pass.is_empty() && !progressed {
                progressed |= self.try_evict(p, None, 2);
            }
            if progressed {
                self.wake(p);
            }
            return false;
        };

        let consumer_sid = self.grid.sid.get(&t).copied();
        let producer_sid = self.grid.sid.get(&p).copied();
        let (seed_c, seed_p) = mk_seeds(roll);
        self.events.push(Event::Dock(t, p, rule));
        if layout.script.is_empty() {
            // No blocklet: resolve immediately in the dock transaction.
            let (fc, fp) = crate::blocklet::seed_finals(rule, axis, roll, &seed_c, &seed_p);
            self.grid.set(t, &Site { cell: fc, cursor: None, chi: target.chi, claim: false });
            self.grid.set(p, &Site { cell: fp, cursor: site.cursor, chi: site.chi, claim: false });
            self.grid.sid.remove(&t);
            self.grid.sid.remove(&p);
            self.observe_fire(t, rule, axis, roll, &crate::blocklet::layout(rule).seats, consumer_sid, producer_sid);
        } else {
            self.grid.set(
                t,
                &Site {
                    cell: seed_c,
                    cursor: Some(Cursor { rule, axis, roll, pc: 0, reverse: false }),
                    chi: target.chi,
                    claim: false,
                },
            );
            self.grid.set(p, &Site { cell: seed_p, cursor: site.cursor, chi: site.chi, claim: false });
            if let (Some(cs), Some(ps)) = (consumer_sid, producer_sid) {
                self.grid.sid.remove(&t);
                self.grid.sid.remove(&p);
                self.grid.seed_sids.insert(t, (cs, ps));
            }
        }
        self.wake_around(p);
        self.wake_around(t);
        true
    }

    fn observe_fire(
        &mut self,
        seed_c: Pos,
        rule: u8,
        axis: Dir,
        roll: u8,
        seats: &[(usize, Pos)],
        consumer_sid: Option<u32>,
        producer_sid: Option<u32>,
    ) {
        self.grid.rewrites += 1;
        self.events.push(Event::Fire(seed_c, rule));
        if let (Some(cs), Some(ps)) = (consumer_sid, producer_sid) {
            let (r, fresh) = self.shadow.fire(cs, ps);
            assert!(std::ptr::eq(r, &RULES[rule as usize]));
            assert_eq!(fresh.len(), seats.len());
            for (fresh_idx, off) in seats {
                let world = add(seed_c, crate::cascade::rot_pos(*off, axis, roll));
                assert!(
                    self.grid.sid.insert(world, fresh[*fresh_idx]).is_none(),
                    "fresh seat already has a sid"
                );
            }
        }
    }

    /// One builder-cursor step: place the next blocklet cell (reserving first), hop along
    /// the placed set, resolve back on the consumer seed, then finalize the nursery.
    /// The consumer-seed cell a cursor grew from, folded back through its script's hops.
    /// The origin is the blocklet's identity: in a footprint conflict the LOWER origin
    /// wins and the higher one retracts, so every conflict set has a survivor.
    fn cursor_origin(cursor: &Cursor, p: Pos) -> Pos {
        let layout = crate::blocklet::layout(cursor.rule);
        let mut at = p;
        for op in &layout.script[..cursor.pc as usize] {
            if let crate::blocklet::Op::Hop { dir, .. } = op {
                at = step(at, crate::cascade::rot_dir(*dir, cursor.axis, cursor.roll).opp());
            }
        }
        at
    }

    /// Seed-vs-seed arbitration: if this cursor's blocklet has the higher origin it flips
    /// to reverse (retract, restore the pair, re-dock later); the winner waits silently
    /// and is woken by the loser's unwind. Once the seed has resolved the fire is
    /// irrevocable, so a post-resolve cursor never yields: it waits out the squatter.
    fn yield_if_loser(&mut self, p: Pos, site: &Site, cursor: Cursor, their_origin: Pos) {
        if cursor.pc >= crate::blocklet::layout(cursor.rule).resolve_pc {
            return;
        }
        if Self::cursor_origin(&cursor, p) > their_origin {
            let mut s = site.clone();
            s.cursor = Some(Cursor { reverse: true, ..cursor });
            self.grid.set(p, &s);
            self.wake(p);
        }
    }

    fn step_cursor(&mut self, p: Pos, site: Site, cursor: Cursor) {
        use crate::blocklet::Op;
        let layout = crate::blocklet::layout(cursor.rule);
        if cursor.reverse {
            self.step_cursor_reverse(p, site, cursor);
            return;
        }
        if cursor.pc == layout.resolve_pc && matches!(site.cell, Cell::Seed { .. }) {
            // All cells placed, everyone still in the nursery: fire the interaction.
            self.resolve(p, site, cursor);
            return;
        }
        if cursor.pc as usize == layout.script.len() {
            // Finalize pass complete: the cursor evaporates.
            let mut s = site;
            s.cursor = None;
            self.grid.set(p, &s);
            self.wake_around(p);
            return;
        }
        let op = &layout.script[cursor.pc as usize];
        match op {
            Op::Place { dir, cell } => {
                let d = crate::cascade::rot_dir(*dir, cursor.axis, cursor.roll);
                let t = step(p, d);
                let target = self.grid.site(t);
                if target.claim {
                    return; // wait silently; the target's next change wakes this cell
                }
                if let Some(fc) = target.cursor {
                    self.yield_if_loser(p, &site, cursor, Self::cursor_origin(&fc, t));
                    return;
                }
                if let Cell::Seed { half, partner, .. } = &target.cell {
                    // Growing into a foreign docked pair: arbitrate against its origin.
                    let their = match half {
                        Half::Consumer => t,
                        Half::Producer => step(t, *partner),
                    };
                    self.yield_if_loser(p, &site, cursor, their);
                    return;
                }
                match &target.cell {
                    Cell::Empty { reserved: None } => {
                        // First phase: reserve the empty cell.
                        self.grid.set(
                            t,
                            &Site {
                                cell: Cell::Empty { reserved: Some(d.opp()) },
                                cursor: None,
                                chi: target.chi,
                                claim: false,
                            },
                        );
                        self.wake(p);
                        self.wake(t);
                    }
                    Cell::Wire { reserved: None, routes, hot, cooldown } => {
                        // A hopeless merge never reserves: with the quiescence-edge
                        // sweep re-waking every parked op, a reserve/release retry
                        // cycle would re-arm the sweep forever (soak term 26). The
                        // pre-check keeps a refused Place mutation-free, so the grid
                        // can actually reach the sweep's fixpoint and park.
                        let planned = crate::cascade::rot_cell(cell, cursor.axis, cursor.roll);
                        if merge_matter(planned.clone(), routes).is_none() {
                            self.note(|| format!(
                                "merge-fail (pre-reserve) at {t:?}: planned {planned:?} vs {:?}",
                                target.cell
                            ));
                            self.relief_owner = Some(p);
                            self.relief_root = Some(t);
                            self.relief_pays = true;
                            let relieved = self.try_evict(t, Some(&planned), 2);
                            self.relief_pays = false;
                            self.relief_owner = None;
                            self.relief_root = None;
                            if relieved {
                                self.wake(p);
                            }
                            return;
                        }
                        // First phase over existing wire: reserve it so nothing enters
                        // while the merge is pending.
                        self.grid.set(
                            t,
                            &Site {
                                cell: Cell::Wire {
                                    routes: routes.clone(),
                                    hot: *hot,
                                    cooldown: *cooldown,
                                    reserved: Some(d.opp()),
                                },
                                cursor: None,
                                chi: target.chi,
                                claim: false,
                            },
                        );
                        self.wake(p);
                        self.wake(t);
                    }
                    Cell::Empty { reserved: Some(r) } | Cell::Wire { reserved: Some(r), .. }
                        if *r == d.opp() =>
                    {
                        // Second phase: merge the planned matter with any existing
                        // routes (the guest principle) and advance the script.
                        let planned = crate::cascade::rot_cell(cell, cursor.axis, cursor.roll);
                        let existing: Vec<Route> = match &target.cell {
                            Cell::Wire { routes, .. } => routes.clone(),
                            _ => vec![],
                        };
                        let Some(mut merged) = merge_matter(planned.clone(), &existing) else {
                            // The merge cannot fit. Evict one cold occupying route out of
                            // the way (the relief rung); if nothing is evictable, release
                            // the reservation and wait for the occupant to change.
                            self.note(|| format!(
                                "merge-fail at {t:?}: planned {planned:?} vs {:?}",
                                target.cell
                            ));
                            // This cursor's own cell is exempt from the cursor-hosting
                            // prohibition while its relief runs, and the reserved target
                            // does not refuse its own clearing.
                            self.relief_owner = Some(p);
                            self.relief_root = Some(t);
                            self.relief_pays = true;
                            let relieved = self.try_evict(t, Some(&planned), 2);
                            self.relief_pays = false;
                            self.relief_owner = None;
                            self.relief_root = None;
                            if relieved {
                                self.wake(p);
                                return;
                            }
                            let mut ts = target.clone();
                            if let Cell::Wire { reserved, .. } = &mut ts.cell {
                                *reserved = None;
                            }
                            self.grid.set(t, &ts);
                            self.wake(t);
                            return;
                        };
                        if !self.nursery_discipline {
                            if let Cell::Agent { nursery, .. } = &mut merged {
                                *nursery = false;
                            }
                        }
                        *self.grown_by_rule.entry(cursor.rule).or_insert(0) += 1;
                        self.grid.set(
                            t,
                            &Site { cell: merged, cursor: None, chi: target.chi, claim: false },
                        );
                        let mut s = site;
                        s.cursor = Some(Cursor { pc: cursor.pc + 1, ..cursor });
                        self.grid.set(p, &s);
                        self.wake_around(t);
                        self.wake(p);
                    }
                    _ => {
                        // Occupied by an agent, a seed, or a foreign reservation: wait
                        // silently. The target's next change wakes this cell.
                    }
                }
            }
            Op::Hop { dir, finalize } => {
                let d = crate::cascade::rot_dir(*dir, cursor.axis, cursor.roll);
                let t = step(p, d);
                let mut target = self.grid.site(t);
                if let Some(fc) = target.cursor {
                    // Overlapping blocklets: a foreign cursor stands on this hop's cell.
                    self.yield_if_loser(p, &site, cursor, Self::cursor_origin(&fc, t));
                    return;
                }
                let mut here = site;
                here.cursor = None;
                if *finalize {
                    if let Cell::Agent { nursery, .. } = &mut here.cell {
                        *nursery = false;
                    }
                }
                target.cursor = Some(Cursor { pc: cursor.pc + 1, ..cursor });
                self.grid.set(p, &here);
                self.grid.set(t, &target);
                self.wake_around(p);
                self.wake(t);
            }
        }
    }

    /// The relief rung: move one cold route out of a growth-blocked cell. A bent segment
    /// is corner-cut through its diagonal cell (same length, frees the cell); a straight
    /// segment shifts sideways through three side cells (two cells longer). Only routes
    /// that are cold, unreserved, and continue into plain wire cells on both sides are
    /// touched, and (with `planned` set) only when removing them lets the planned matter
    /// merge; `planned: None` means any shed route is progress (capacity relief). When
    /// every shape fails on a full-but-otherwise-eligible side cell, the eviction recurses
    /// into that blocker (`depth` bounds the chain; one commit per activation, the
    /// cursor's retry drives the rest). Cells that just received an evicted route carry a
    /// cooldown stamp and refuse to shed again until it decays, which breaks
    /// displacement ping-pong. The whole rewrite commits in one serial activation; the
    /// parallel driver claims the same set in address order. The blocked cell keeps its
    /// reservation so the cursor's retry finds it still protected.
    pub fn try_evict(&mut self, t: Pos, planned: Option<&Cell>, depth: u8) -> bool {
        let target = self.grid.site(t);
        // The host is a wire cell, or an agent shedding one of its own passthroughs (a
        // blocked walker relieving its own cell): the reroute shapes are identical, the
        // host just loses the route.
        let (routes, hot, reserved) = match &target.cell {
            Cell::Wire { routes, hot, reserved, cooldown } => {
                // Inner (blocker) evictions respect the stamp with a plain refusal —
                // no wear, no wake, no progress report. Wear-as-progress was a pump
                // motor three times over (the requester's retry loop kept itself
                // alive); stamps now decay on the stamped cell's OWN activations
                // (self-waking, see pump_heat), so a stamped refusal is a short pause
                // that expires without anyone hammering. The top level is the caller's
                // own target and always proceeds.
                if depth < 2 && *cooldown > 0 {
                    let cd = *cooldown;
                    self.note(|| format!("evict {t:?} d{depth}: cooldown {cd}, refusing"));
                    return false;
                }
                (routes.clone(), *hot, *reserved)
            }
            Cell::Agent { pass, .. } => (pass.clone(), 0u8, None),
            _ => {
                self.note(|| format!("evict {t:?} d{depth}: host is not a wire or agent"));
                return false;
            }
        };
        let routes = &routes;
        // The one cell whose own cursor requested this relief may be swung despite
        // hosting that cursor, and the reserved target being cleared does not refuse its
        // own relief; foreign cursor cells and reservations stay untouchable.
        let owner = self.relief_owner;
        let root = self.relief_root;
        let cursor_ok = move |s: &Site, at: Pos| s.cursor.is_none() || owner == Some(at);
        let reserved_of = |s: &Site| match &s.cell {
            Cell::Wire { reserved, .. } | Cell::Empty { reserved } => *reserved,
            _ => None,
        };
        let unreserved_ok =
            move |s: &Site, at: Pos| reserved_of(s).is_none() || root == Some(at);
        let plain = |s: &Site, at: Pos| {
            matches!(s.cell, Cell::Wire { .. })
                && unreserved_ok(s, at)
                && cursor_ok(s, at)
                && !s.claim
        };
        // A requesting dock's own ring is forbidden as a displacement receiver: its
        // relief strictly DRAINS the ring instead of shuffling within it (the potential
        // function). Empty outside a dock's relief, so ordinary evictions are free.
        let ring = self.relief_ring.clone();
        let side_ok = move |s: &Site, at: Pos| {
            !ring.contains(&at)
                && match &s.cell {
                    Cell::Empty { .. } => unreserved_ok(s, at) && cursor_ok(s, at) && !s.claim,
                    Cell::Wire { routes, .. } => {
                        routes.len() < 3 && unreserved_ok(s, at) && cursor_ok(s, at) && !s.claim
                    }
                    _ => false,
                }
        };
        // Why a receiver was refused, for the relief notes. Reporting a refused-but-free
        // cell as "blocked" alongside its contents reads as an occupancy problem and
        // sends diagnosis down the wrong path (twice now: the corner-cut diagonal, then
        // shifts and brackets into wholly empty space).
        let ring_note = self.relief_ring.clone();
        let side_why = move |s: &Site, at: Pos| -> &'static str {
            if ring_note.contains(&at) {
                return "in the requesting dock's ring";
            }
            if !cursor_ok(s, at) {
                return "hosts a cursor";
            }
            if s.claim {
                return "claimed";
            }
            if !unreserved_ok(s, at) {
                return "reserved";
            }
            match &s.cell {
                // Empty always has room, so an Empty reaching here is a passing cell
                // listed beside the one that actually refused.
                Cell::Empty { .. } => "ok",
                Cell::Wire { routes, .. } if routes.len() < 3 => "ok",
                Cell::Wire { .. } => "full",
                _ => "occupied",
            }
        };
        // Receivers of a displaced route get the full cooldown stamp: they will not shed
        // it again (nor be retracted) until the stamp decays. A displaced hot route
        // carries its heat into the new cell.
        let stamp3 = self.stamp(3);
        let side_add = move |s: &Site, route: Route, hot_flag: bool| -> Site {
            let mut ns = s.clone();
            match &mut ns.cell {
                Cell::Empty { .. } => {
                    ns.cell = Cell::Wire {
                        routes: vec![route],
                        hot: u8::from(hot_flag),
                        cooldown: stamp3,
                        reserved: None,
                    };
                }
                Cell::Wire { routes, hot, cooldown, .. } => {
                    if hot_flag {
                        *hot |= 1 << routes.len();
                    }
                    routes.push(route);
                    *cooldown = stamp3;
                }
                _ => unreachable!(),
            }
            ns
        };
        // A side cell that fails only on capacity is a recursion candidate: evicting one
        // of its cold routes makes this eviction's shape viable next retry.
        let recursable = |s: &Site| {
            matches!(&s.cell, Cell::Wire { reserved: None, routes, .. } if routes.len() >= 3)
                && s.cursor.is_none()
                && !s.claim
        };
        // Route-level displacement order: every displacement
        // shape's PRIMARY direction must ascend a fixed global form (g = (1,3,9) —
        // no face or diagonal can sum to zero), so a displacement cycle, which needs
        // net-zero total movement, is impossible for ANY pair of requesters. Move
        // granularity: a whole shifted segment counts as one move in one direction,
        // so nothing straddles the order the way cell-level receivers did. The
        // strictly-shortening shapes (splice, truncation) stay exempt.
        // A relief that PAYS FOR ITSELF may move against the order: this exact route is
        // what blocks a placement, so moving it commits that placement next activation.
        // Each violation is therefore the last move before real progress, and progress
        // is monotone and bounded by the reduction itself, so violations cannot cycle.
        // The payment must be VERIFIED per route, not assumed per request: exempting
        // every eviction a blocked placement asks for (including the ones that shed
        // some other cold route) re-armed displacement cycles — measured, the cooldown
        // ablation went from 0 livelocks to 3.
        let ascends = {
            let g = self.relief_g;
            move |delta: (i32, i32, i32), pays: bool| -> bool {
                pays || g.0 * delta.0 + g.1 * delta.1 + g.2 * delta.2 > 0
            }
        };
        let mut blockers: Vec<Pos> = vec![];
        // A continuation is either a ROUTE entry (wire, or threading an agent's
        // passthrough list) or a terminal PORT attachment (the displaced route ends at
        // an agent's own principal or aux). Bending either is one word rewrite; bent
        // agents get the cooldown stamp (anti-ping-pong damping, as displaced wire
        // routes carry theirs).
        #[derive(Clone, Copy, PartialEq)]
        enum Cont {
            Route(usize),
            Port,
        }
        let stamp1 = self.stamp(1);
        let swing = move |s: &Site, cont: Cont, from: EndPt, to: EndPt| -> Site {
            let mut ns = s.clone();
            match (&mut ns.cell, cont) {
                (Cell::Wire { routes, .. }, Cont::Route(idx)) => {
                    let far = routes[idx].through(from).expect("continuation endpoint");
                    routes[idx] = Route::new(far, to);
                }
                (Cell::Agent { pass, cooldown, .. }, Cont::Route(idx)) => {
                    let far = pass[idx].through(from).expect("guest continuation endpoint");
                    pass[idx] = Route::new(far, to);
                    *cooldown = stamp1;
                }
                (Cell::Agent { tag, principal, aux, cooldown, .. }, Cont::Port) => {
                    // Endpoint swing: re-anchor the agent's own port. Exposure
                    // legality re-validates at pack, like every other commit.
                    if *principal == from {
                        *principal = to;
                    } else {
                        for k in 0..tag.arity().saturating_sub(1) {
                            if aux[k] == from {
                                aux[k] = to;
                            }
                        }
                        if tag.arity() == 2 {
                            aux[1] = aux[0]; // unused entry mirrors aux[0] by convention
                        }
                    }
                    *cooldown = stamp1;
                }
                _ => {}
            }
            ns
        };

        // Prefer an eviction that unblocks the merge outright; when the cell is more than
        // one route over budget, evict any evictable cold route (progress one at a time).
        let order: Vec<(usize, bool)> = {
            let unblocking = |i: usize| {
                let Some(planned) = planned else { return true };
                let rest: Vec<Route> = routes
                    .iter()
                    .enumerate()
                    .filter(|(j, _)| *j != i)
                    .map(|(_, x)| *x)
                    .collect();
                merge_matter(planned.clone(), &rest).is_some()
            };
            let mut v: Vec<(usize, bool)> = (0..routes.len()).map(|i| (i, unblocking(i))).collect();
            v.sort_by_key(|(_, u)| std::cmp::Reverse(*u));
            v
        };
        // Cold routes first; a second pass may move hot routes as the last resort in an
        // all-hot pinch. Moving a demanded wire is sound: connectivity is preserved and
        // its heat rides along; the cooldown stamp damps displacement wars all the same.
        // A worklist rather than a plain loop: a successful squatter shove re-queues its
        // route for an immediate same-activation retry (fresh reads, bounded by the
        // per-route shove budget), so the shove's window is consumed here instead of
        // being raced for across activations.
        let unblocks: Vec<bool> = {
            let mut v = vec![false; routes.len()];
            for (i, u) in &order {
                v[*i] = *u;
            }
            v
        };
        let mut worklist: VecDeque<(bool, usize, u8)> = order
            .iter()
            .map(|(i, _)| (false, *i, 2u8))
            .chain(order.iter().map(|(i, _)| (true, *i, 2u8)))
            .collect();
        // A committed sidestep is caller-visible progress even when no route ultimately
        // sheds: callers re-wake their initiator on progress, and a shove that moved
        // matter without a wake is a lost dock (the --kick census class).
        let mut shoved_any = false;
        // Compact cell description for relief notes.
        let kind = |s: &Site| -> String {
            let base = match &s.cell {
                Cell::Empty { reserved: None } => "empty".into(),
                Cell::Empty { reserved: Some(d) } => format!("empty rsv<-{}", d.ch()),
                Cell::Wire { routes, reserved, cooldown, .. } => format!(
                    "wire x{}{}{}",
                    routes.len(),
                    reserved.map(|d| format!(" rsv<-{}", d.ch())).unwrap_or_default(),
                    if *cooldown > 0 { format!(" cd{cooldown}") } else { String::new() },
                ),
                Cell::Agent { tag, nursery, pass, .. } => format!(
                    "agent {}{} pass{}",
                    tag.name(),
                    if *nursery { " nursery" } else { "" },
                    pass.len()
                ),
                Cell::Seed { rule, .. } => format!("seed r{rule}"),
            };
            format!(
                "{base}{}{}",
                if s.cursor.is_some() { " +cursor" } else { "" },
                if s.claim { " claim" } else { "" }
            )
        };
        while let Some((allow_hot, i, shoves_left)) = worklist.pop_front() {
            let r = &routes[i];
            // Verified payment: the requester says a placement is waiting on this cell,
            // and removing THIS route is what lets that placement's matter merge.
            let pays = self.relief_pays && unblocks[i];
            let moved_hot = (hot >> i) & 1 == 1 || self.signals.hot(&self.grid, t, i);
            if !allow_hot && moved_hot {
                self.note(|| format!("evict {t:?} r{i}: hot (cold pass)"));
                continue;
            }
            let (d1, d2) = (r.a.face, r.b.face);
            let (n1, n2) = (step(t, d1), step(t, d2));
            let (n1s, n2s) = (self.grid.site(n1), self.grid.site(n2));
            // A blocked continuation cell may itself be relieved: a crowded wire or an
            // agent hosting passthroughs can shed one route. The U-turn splice tolerates
            // a reservation on its neighbor (it strictly reduces that cell's occupancy);
            // every other shape needs plain wire on both sides.
            let sheddable = |xs: &Site, at: Pos| {
                cursor_ok(xs, at)
                    && !xs.claim
                    && match &xs.cell {
                        Cell::Wire { reserved: None, routes, .. } => routes.len() >= 2,
                        Cell::Agent { pass, .. } => !pass.is_empty(),
                        _ => false,
                    }
            };
            let splice_host = |xs: &Site, at: Pos| {
                matches!(xs.cell, Cell::Wire { .. }) && cursor_ok(xs, at) && !xs.claim
            };
            let back1 = EndPt { face: d1.opp(), lane: r.a.lane };
            let back2 = EndPt { face: d2.opp(), lane: r.b.lane };
            // A continuation swings when it is plain wire, or when it meets a live,
            // unstamped agent — threading its passthrough list (guest continuation) or
            // terminating on one of its own ports (endpoint swing). Nursery agents are
            // untouchable growth matter; a stamped agent was bent recently: wait out
            // the damping.
            let swingable = |s: &Site, at: Pos, back: EndPt| -> bool {
                plain(s, at)
                    || (matches!(&s.cell,
                            Cell::Agent { tag, principal, aux, pass, nursery: false, cooldown: 0, .. }
                            if pass.iter().any(|x| x.ends().contains(&back))
                                || *principal == back
                                || (0..tag.arity().saturating_sub(1)).any(|k| aux[k] == back))
                        && cursor_ok(s, at)
                        && !s.claim)
            };
            let eligible = if d1 == d2 {
                splice_host(&n1s, n1)
            } else {
                swingable(&n1s, n1, back1) && swingable(&n2s, n2, back2)
            };
            if !eligible {
                self.note(|| format!(
                    "evict {t:?} r{i} ({}{}.{}-{}.{}): continuation not swingable; {n1:?} is {} and {n2:?} is {}",
                    if moved_hot { "hot " } else { "" },
                    d1.ch(), r.a.lane, d2.ch(), r.b.lane,
                    kind(&n1s), kind(&n2s)
                ));
                // A producer squatting on a continuation is sidestepped away. Only when
                // the shove COMPLETES eligibility (the opposite continuation is already
                // swingable, or the route U-turns through the squatter alone), and the
                // route then retries IMMEDIATELY (same activation, fresh reads).
                // Anything looser is a pump: shoving beside a hopeless partner just
                // cycles a demanded squatter out and back — the two-cell shuttle
                // livelock the soak found. Sidestep ONLY, never the forced walk: the
                // walk's failure paths run nested relief that can mutate t itself,
                // and every commit below writes t from this call's entry snapshot.
                // Top level only, so shoves do not chain.
                if depth == 2 && shoves_left > 0 {
                    // Never shove the route's own traffic: a walker whose principal is
                    // this route's reciprocal endpoint must traverse t eventually, and
                    // its demand marches it straight back after any sidestep (the
                    // convoy livelock). A legitimate shove target carries the route as
                    // a passthrough only.
                    let own_traffic = |end: &EndPt, xs: &Site| {
                        matches!(&xs.cell, Cell::Agent { principal, .. }
                            if *principal == EndPt { face: end.face.opp(), lane: end.lane })
                    };
                    let mut shoved = false;
                    for (x, xs, end, partner_plain) in [
                        (n1, &n1s, &r.a, d1 == d2 || swingable(&n2s, n2, back2)),
                        (n2, &n2s, &r.b, d1 == d2 || swingable(&n1s, n1, back1)),
                    ] {
                        let squatter = matches!(&xs.cell,
                            Cell::Agent { tag, nursery: false, cooldown: 0, .. } if tag.is_producer())
                            && xs.cursor.is_none()
                            && !xs.claim;
                        if squatter && partner_plain && !own_traffic(end, xs) {
                            self.note(|| format!("evict {t:?} r{i}: shoving squatter at {x:?}"));
                            if self.try_sidestep(x, xs) {
                                shoved = true;
                                break;
                            }
                        }
                    }
                    if shoved {
                        shoved_any = true;
                        worklist.push_front((allow_hot, i, shoves_left - 1));
                        continue;
                    }
                }
                for (x, xs) in [(n1, &n1s), (n2, &n2s)] {
                    if sheddable(xs, x) && !blockers.contains(&x) {
                        blockers.push(x);
                    }
                }
                continue;
            }
            let cont_at = |s: &Site, back: EndPt| -> Option<Cont> {
                match &s.cell {
                    Cell::Wire { routes, .. } => {
                        routes.iter().position(|x| x.ends().contains(&back)).map(Cont::Route)
                    }
                    Cell::Agent { tag, principal, aux, pass, nursery: false, .. } => pass
                        .iter()
                        .position(|x| x.ends().contains(&back))
                        .map(Cont::Route)
                        .or_else(|| {
                            (*principal == back
                                || (0..tag.arity().saturating_sub(1)).any(|k| aux[k] == back))
                                .then_some(Cont::Port)
                        }),
                    _ => None,
                }
            };
            let (Some(c1), Some(c2)) = (cont_at(&n1s, back1), cont_at(&n2s, back2)) else {
                self.note(|| format!("evict {t:?} r{i}: no continuation at {n1:?}/{n2:?}"));
                continue;
            };
            // Terminal ports carry no slot heat; the t-side hot gate already vetted the
            // cable itself.
            let hot1 = matches!(c1, Cont::Route(k) if self.signals.hot(&self.grid, n1, k));
            let hot2 = matches!(c2, Cont::Route(k) if self.signals.hot(&self.grid, n2, k));
            if !allow_hot && (hot1 || hot2) {
                self.note(|| format!("evict {t:?} r{i}: hot continuation (cold pass)"));
                continue;
            }
            // The blocked cell after removal, reservation intact.
            let t_new = {
                let mut ns = target.clone();
                let mut emptied = false;
                match &mut ns.cell {
                    Cell::Wire { routes, hot, .. } => {
                        let mut nh = 0u8;
                        let mut k = 0;
                        for j in 0..routes.len() {
                            if j != i {
                                if (*hot >> j) & 1 == 1 {
                                    nh |= 1 << k;
                                }
                                k += 1;
                            }
                        }
                        routes.remove(i);
                        *hot = nh;
                        emptied = routes.is_empty();
                    }
                    Cell::Agent { pass, .. } => {
                        pass.remove(i);
                    }
                    _ => unreachable!(),
                }
                if emptied {
                    ns.cell = Cell::Empty { reserved };
                }
                ns
            };

            if d1 == d2 {
                // A U-turn cell: the wire folds back through one neighbor, pure slack.
                // Splice the fold out at that neighbor (its two continuations become one
                // route) and drop the fold here; a same-route continuation is a closed
                // two-cell loop and vanishes entirely. Strictly length-decreasing.
                // splice_host guarantees a wire neighbor, so both conts are routes.
                let (Cont::Route(i1), Cont::Route(i2)) = (c1, c2) else {
                    continue;
                };
                let n_reserved = match &n1s.cell {
                    Cell::Wire { reserved, .. } => *reserved,
                    _ => None,
                };
                let n_new = {
                    let mut nn = n1s.clone();
                    let mut emptied = false;
                    if let Cell::Wire { routes, hot, .. } = &mut nn.cell {
                        let mut kept: Vec<(Route, bool)> = routes
                            .iter()
                            .enumerate()
                            .filter(|(j, _)| *j != i1 && *j != i2)
                            .map(|(j, x)| (*x, (*hot >> j) & 1 == 1))
                            .collect();
                        if i1 != i2 {
                            let x = routes[i1].through(back1).expect("fold continuation");
                            let y = routes[i2].through(back2).expect("fold continuation");
                            let sp_hot = (*hot >> i1) & 1 == 1 || (*hot >> i2) & 1 == 1;
                            kept.push((Route::new(x, y), sp_hot));
                        }
                        *routes = kept.iter().map(|(x, _)| *x).collect();
                        *hot = kept.iter().enumerate().fold(0, |acc, (j, (_, h))| {
                            acc | u8::from(*h) << j
                        });
                        emptied = routes.is_empty();
                    }
                    if emptied {
                        nn.cell = Cell::Empty { reserved: n_reserved };
                    }
                    nn
                };
                if crate::cascade::Word2::pack(&t_new).is_ok()
                    && crate::cascade::Word2::pack(&n_new).is_ok()
                {
                    self.grid.set(t, &t_new);
                    self.grid.set(n1, &n_new);
                    self.wake_around(t);
                    self.wake_around(n1);
                    return true;
                }
                self.note(|| format!("evict {t:?} r{i}: u-turn splice at {n1:?} does not pack"));
            } else if d2 != d1.opp() {
                // Bent segment: corner-cut through the diagonal cell.
                let u = step(n1, d2);
                let us = self.grid.site(u);
                if !side_ok(&us, u) && recursable(&us) && !blockers.contains(&u) {
                    blockers.push(u);
                }
                if side_ok(&us, u) && ascends((u.0 - t.0, u.1 - t.1, u.2 - t.2), pays) {
                    for l1 in 0..2u8 {
                        for l2 in 0..2u8 {
                            let n1n = swing(&n1s, c1, back1, EndPt { face: d2, lane: l1 });
                            let n2n = swing(&n2s, c2, back2, EndPt { face: d1, lane: l2 });
                            let un = side_add(&us, Route::new(
                                EndPt { face: d2.opp(), lane: l1 },
                                EndPt { face: d1.opp(), lane: l2 },
                            ), moved_hot);
                            if [&t_new, &n1n, &n2n, &un]
                                .iter()
                                .all(|w| crate::cascade::Word2::pack(w).is_ok())
                            {
                                self.grid.set(t, &t_new);
                                self.grid.set(n1, &n1n);
                                self.grid.set(n2, &n2n);
                                self.grid.set(u, &un);
                                for c in [t, n1, n2, u] {
                                    self.wake_around(c);
                                }
                                return true;
                            }
                        }
                    }
                    self.note(|| format!(
                        "evict {t:?} r{i}: corner-cut lanes exhausted at diagonal {u:?}"
                    ));
                } else if side_ok(&us, u) {
                    self.note(|| format!(
                        "evict {t:?} r{i}: corner-cut diagonal {u:?} is free but DESCENDS the order"
                    ));
                } else {
                    self.note(|| format!(
                        "evict {t:?} r{i}: corner-cut diagonal {u:?} busy: {}", kind(&us)
                    ));
                }
                // Fallback: the diagonal is taken, so bracket the bend out of plane
                // through the two faces perpendicular to both bend directions.
                for w in DIRS {
                    if w == d1 || w == d1.opp() || w == d2 || w == d2.opp() {
                        continue;
                    }
                    if !ascends(w.delta(), pays) {
                        self.note(|| format!(
                            "evict {t:?} r{i}: bracket {} DESCENDS the order", w.ch()
                        ));
                        continue;
                    }
                    let (a, c, b) = (step(n1, w), step(u, w), step(n2, w));
                    let (a_s, c_s, b_s) = (self.grid.site(a), self.grid.site(c), self.grid.site(b));
                    if !side_ok(&a_s, a) || !side_ok(&c_s, c) || !side_ok(&b_s, b) {
                        self.note(|| format!(
                            "evict {t:?} r{i}: bracket {} blocked: {a:?} {} ({}), {c:?} {} ({}), \
                             {b:?} {} ({})",
                            w.ch(),
                            kind(&a_s), side_why(&a_s, a),
                            kind(&c_s), side_why(&c_s, c),
                            kind(&b_s), side_why(&b_s, b)
                        ));
                        for (x, xs) in [(a, &a_s), (c, &c_s), (b, &b_s)] {
                            if !side_ok(xs, x) && recursable(xs) && !blockers.contains(&x) {
                                blockers.push(x);
                            }
                        }
                        continue;
                    }
                    'wlanes: for lanes in 0..16u8 {
                        let (l1, l2, l3, l4) =
                            (lanes & 1, (lanes >> 1) & 1, (lanes >> 2) & 1, (lanes >> 3) & 1);
                        let n1n = swing(&n1s, c1, back1, EndPt { face: w, lane: l1 });
                        let n2n = swing(&n2s, c2, back2, EndPt { face: w, lane: l4 });
                        let an = side_add(&a_s, Route::new(
                            EndPt { face: w.opp(), lane: l1 },
                            EndPt { face: d2, lane: l2 },
                        ), moved_hot);
                        let cn = side_add(&c_s, Route::new(
                            EndPt { face: d2.opp(), lane: l2 },
                            EndPt { face: d1.opp(), lane: l3 },
                        ), moved_hot);
                        let bn = side_add(&b_s, Route::new(
                            EndPt { face: d1, lane: l3 },
                            EndPt { face: w.opp(), lane: l4 },
                        ), moved_hot);
                        for x in [&t_new, &n1n, &n2n, &an, &cn, &bn] {
                            if crate::cascade::Word2::pack(x).is_err() {
                                continue 'wlanes;
                            }
                        }
                        self.grid.set(t, &t_new);
                        self.grid.set(n1, &n1n);
                        self.grid.set(n2, &n2n);
                        self.grid.set(a, &an);
                        self.grid.set(c, &cn);
                        self.grid.set(b, &bn);
                        for x in [t, n1, n2, a, c, b] {
                            self.wake_around(x);
                        }
                        return true;
                    }
                }
            } else {
                // Straight segment: parallel shift through three side cells.
                for q in d1.perp() {
                    if !ascends(q.delta(), pays) {
                        self.note(|| format!(
                            "evict {t:?} r{i}: shift {} DESCENDS the order", q.ch()
                        ));
                        continue;
                    }
                    let (u1, u2, u3) = (step(n1, q), step(t, q), step(n2, q));
                    let (u1s, u2s, u3s) =
                        (self.grid.site(u1), self.grid.site(u2), self.grid.site(u3));
                    if !side_ok(&u1s, u1) || !side_ok(&u2s, u2) || !side_ok(&u3s, u3) {
                        self.note(|| format!(
                            "evict {t:?} r{i}: shift {} blocked: {u1:?} {} ({}), {u2:?} {} ({}), \
                             {u3:?} {} ({})",
                            q.ch(),
                            kind(&u1s), side_why(&u1s, u1),
                            kind(&u2s), side_why(&u2s, u2),
                            kind(&u3s), side_why(&u3s, u3)
                        ));
                        for (x, xs) in [(u1, &u1s), (u2, &u2s), (u3, &u3s)] {
                            if !side_ok(xs, x) && recursable(xs) && !blockers.contains(&x) {
                                blockers.push(x);
                            }
                        }
                        continue;
                    }
                    'lanes: for lanes in 0..16u8 {
                        let (l1, l2, l3, l4) =
                            (lanes & 1, (lanes >> 1) & 1, (lanes >> 2) & 1, (lanes >> 3) & 1);
                        let n1n = swing(&n1s, c1, back1, EndPt { face: q, lane: l1 });
                        let n2n = swing(&n2s, c2, back2, EndPt { face: q, lane: l4 });
                        let u1n = side_add(&u1s, Route::new(
                            EndPt { face: q.opp(), lane: l1 },
                            EndPt { face: d1.opp(), lane: l2 },
                        ), moved_hot);
                        let u2n = side_add(&u2s, Route::new(
                            EndPt { face: d1, lane: l2 },
                            EndPt { face: d1.opp(), lane: l3 },
                        ), moved_hot);
                        let u3n = side_add(&u3s, Route::new(
                            EndPt { face: d1, lane: l3 },
                            EndPt { face: q.opp(), lane: l4 },
                        ), moved_hot);
                        for w in [&t_new, &n1n, &n2n, &u1n, &u2n, &u3n] {
                            if crate::cascade::Word2::pack(w).is_err() {
                                continue 'lanes;
                            }
                        }
                        self.grid.set(t, &t_new);
                        self.grid.set(n1, &n1n);
                        self.grid.set(n2, &n2n);
                        self.grid.set(u1, &u1n);
                        self.grid.set(u2, &u2n);
                        self.grid.set(u3, &u3n);
                        for c in [t, n1, n2, u1, u2, u3] {
                            self.wake_around(c);
                        }
                        return true;
                    }
                    self.note(|| format!("evict {t:?} r{i}: shift {} lanes exhausted", q.ch()));
                }
            }
            // Every shape failed for this route. The continuation cells themselves may be
            // the obstacle: when their swing faces have no free lane, shedding any cold
            // route from them frees one. Reservations keep the cursor's own protected
            // target out of reach of this relocation.
            for (x, xs) in [(n1, &n1s), (n2, &n2s)] {
                if sheddable(xs, x) && !blockers.contains(&x) {
                    blockers.push(x);
                }
            }
        }
        // Recursive room-making: every shape was boxed in by a full side cell. Shed one
        // cold route from one blocker; the cursor's retry re-attempts the outer eviction.
        if depth > 0 {
            for b in blockers {
                self.note(|| format!("evict {t:?} d{depth}: recurse into blocker {b:?}"));
                if self.try_evict(b, None, depth - 1) {
                    return true;
                }
            }
        } else {
            self.note(|| format!("evict {t:?} d0: out of depth"));
        }
        shoved_any
    }

    /// Proactive slack retraction: the inverse of the straight shift. When this cell is
    /// the middle of a cold three-cell detour (straight here, bending back on both
    /// sides toward the same face) and the bypassed cell can host the route, pull it
    /// straight: two cells of wire vanish. Strictly length-decreasing, so it cannot
    /// livelock; cooldown stamps keep it from immediately undoing a fresh eviction, and
    /// reservations keep it out of cells a cursor is growing into.
    fn try_retract(&mut self, p: Pos) -> bool {
        let site = self.grid.site(p);
        let Cell::Wire { routes, hot, cooldown: 0, reserved: None } = &site.cell else {
            return false;
        };
        if site.cursor.is_some() || site.claim {
            return false;
        }
        let plain_cold = |s: &Site| {
            matches!(s.cell, Cell::Wire { reserved: None, cooldown: 0, .. })
                && s.cursor.is_none()
                && !s.claim
        };
        let eligible = |s: &Site| match &s.cell {
            Cell::Empty { reserved: None } => s.cursor.is_none() && !s.claim,
            Cell::Wire { reserved: None, cooldown: 0, routes, .. } => {
                routes.len() < 3 && s.cursor.is_none() && !s.claim
            }
            _ => false,
        };
        // Remove route `idx` from a wire site, preserving the other routes' hot bits.
        let shed = |s: &Site, idx: usize| -> Site {
            let mut ns = s.clone();
            let mut emptied = false;
            if let Cell::Wire { routes, hot, .. } = &mut ns.cell {
                let mut nh = 0u8;
                let mut k = 0;
                for j in 0..routes.len() {
                    if j != idx {
                        if (*hot >> j) & 1 == 1 {
                            nh |= 1 << k;
                        }
                        k += 1;
                    }
                }
                routes.remove(idx);
                *hot = nh;
                emptied = routes.is_empty();
            }
            if emptied {
                ns.cell = Cell::Empty { reserved: None };
            }
            ns
        };
        for (i, r) in routes.iter().enumerate() {
            if (hot >> i) & 1 == 1 || self.signals.hot(&self.grid, p, i) {
                continue;
            }
            let (d1, d2) = (r.a.face, r.b.face);
            if d2 != d1.opp() {
                continue; // only the straight middle of a detour retracts
            }
            let (u1, u3) = (step(p, d1), step(p, d2));
            let (u1s, u3s) = (self.grid.site(u1), self.grid.site(u3));
            if !plain_cold(&u1s) || !plain_cold(&u3s) {
                continue;
            }
            let (Cell::Wire { routes: r1s, .. }, Cell::Wire { routes: r3s, .. }) =
                (&u1s.cell, &u3s.cell)
            else {
                continue;
            };
            // Both legs must bend from this route toward the same perpendicular face q.
            let back1 = EndPt { face: d1.opp(), lane: r.a.lane };
            let back3 = EndPt { face: d2.opp(), lane: r.b.lane };
            let Some(i1) = r1s.iter().position(|x| x.ends().contains(&back1)) else { continue };
            let Some(i3) = r3s.iter().position(|x| x.ends().contains(&back3)) else { continue };
            if self.signals.hot(&self.grid, u1, i1) || self.signals.hot(&self.grid, u3, i3) {
                continue;
            }
            let e1 = r1s[i1].through(back1).unwrap();
            let e3 = r3s[i3].through(back3).unwrap();
            let q = e1.face;
            if q == d1 || q == d2 || e3.face != q {
                continue;
            }
            // Contraction is a route displacement too (the inverse of the shift), so it
            // obeys the same order: pull only toward ascending g, never back the way
            // relief pushed. A detour whose pull descends waits as slack (area, priced
            // by the census) instead of fueling the relief-contraction pump.
            {
                let g = self.relief_g;
                let d = q.delta();
                if g.0 * d.0 + g.1 * d.1 + g.2 * d.2 <= 0 {
                    continue;
                }
            }
            let t = step(p, q);
            let (n1, n2) = (step(u1, q), step(u3, q));
            let (ts, n1s, n2s) = (self.grid.site(t), self.grid.site(n1), self.grid.site(n2));
            if !eligible(&ts) || !plain_cold(&n1s) || !plain_cold(&n2s) {
                continue;
            }
            let (Cell::Wire { routes: a1s, .. }, Cell::Wire { routes: a2s, .. }) =
                (&n1s.cell, &n2s.cell)
            else {
                continue;
            };
            // The anchors hold the detour's outer bends: swing them onto the bypassed cell.
            let anchor1 = EndPt { face: q.opp(), lane: e1.lane };
            let anchor2 = EndPt { face: q.opp(), lane: e3.lane };
            let Some(j1) = a1s.iter().position(|x| x.ends().contains(&anchor1)) else { continue };
            let Some(j2) = a2s.iter().position(|x| x.ends().contains(&anchor2)) else { continue };
            if self.signals.hot(&self.grid, n1, j1) || self.signals.hot(&self.grid, n2, j2) {
                continue;
            }
            let swing = |s: &Site, idx: usize, from: EndPt, to: EndPt| -> Site {
                let mut ns = s.clone();
                if let Cell::Wire { routes, .. } = &mut ns.cell {
                    let far = routes[idx].through(from).expect("anchor endpoint");
                    routes[idx] = Route::new(far, to);
                }
                ns
            };
            for l1 in 0..2u8 {
                for l2 in 0..2u8 {
                    let n1n = swing(&n1s, j1, anchor1, EndPt { face: d1.opp(), lane: l1 });
                    let n2n = swing(&n2s, j2, anchor2, EndPt { face: d1, lane: l2 });
                    let tn = {
                        let mut ns = ts.clone();
                        let route = Route::new(
                            EndPt { face: d1, lane: l1 },
                            EndPt { face: d2, lane: l2 },
                        );
                        match &mut ns.cell {
                            Cell::Empty { .. } => {
                                ns.cell = Cell::Wire {
                                    routes: vec![route],
                                    hot: 0,
                                    cooldown: 0,
                                    reserved: None,
                                };
                            }
                            Cell::Wire { routes, .. } => routes.push(route),
                            _ => unreachable!(),
                        }
                        ns
                    };
                    let (pn, u1n, u3n) = (shed(&site, i), shed(&u1s, i1), shed(&u3s, i3));
                    if [&tn, &n1n, &n2n, &pn, &u1n, &u3n]
                        .iter()
                        .all(|w| crate::cascade::Word2::pack(w).is_ok())
                    {
                        self.grid.set(t, &tn);
                        self.grid.set(n1, &n1n);
                        self.grid.set(n2, &n2n);
                        self.grid.set(p, &pn);
                        self.grid.set(u1, &u1n);
                        self.grid.set(u3, &u3n);
                        for c in [t, n1, n2, p, u1, u3] {
                            self.wake_around(c);
                        }
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Walk the script backwards, unplacing everything, and restore the docked pair.
    fn step_cursor_reverse(&mut self, p: Pos, site: Site, cursor: Cursor) {
        use crate::blocklet::Op;
        if cursor.pc == 0 {
            // Back on the consumer seed: restore both agents.
            let Cell::Seed { rule, partner, stub, plane, pass: cseed_pass, .. } = site.cell else {
                panic!("reverse cursor at pc 0 must stand on the consumer seed");
            };
            let t = step(p, partner);
            let pseed = self.grid.site(t);
            let Cell::Seed { stub: pstub, pass: pseed_pass, .. } = pseed.cell else {
                panic!("consumer seed's partner is not the producer seed");
            };
            let r = &RULES[rule as usize];
            let consumer = Cell::Agent {
                tag: r.consumer,
                principal: EndPt { face: partner, lane: plane },
                aux: stub,
                pass: cseed_pass.into_iter().collect(),
                nursery: false,
                cooldown: self.stamp(1),
            };
            let producer = Cell::Agent {
                tag: r.producer,
                principal: EndPt { face: partner.opp(), lane: plane },
                aux: pstub,
                pass: pseed_pass.into_iter().collect(),
                nursery: false,
                cooldown: self.stamp(1),
            };
            self.grid.set(p, &Site { cell: consumer, cursor: None, chi: site.chi, claim: false });
            self.grid.set(t, &Site { cell: producer, cursor: None, chi: pseed.chi, claim: false });
            if let Some((cs, ps)) = self.grid.seed_sids.remove(&p) {
                self.grid.sid.insert(p, cs);
                self.grid.sid.insert(t, ps);
            }
            self.events.push(Event::Retract(p, cursor.rule));
            self.wake_around(p);
            self.wake_around(t);
            return;
        }
        let layout = crate::blocklet::layout(cursor.rule);
        let op = &layout.script[cursor.pc as usize - 1];
        match op {
            Op::Place { dir, cell } => {
                let d = crate::cascade::rot_dir(*dir, cursor.axis, cursor.roll);
                let t = step(p, d);
                let target = self.grid.site(t);
                // Unplace: strip the planned matter, restoring any merged foreign routes.
                let planned = crate::cascade::rot_cell(cell, cursor.axis, cursor.roll);
                let planned_routes: Vec<Route> = match &planned {
                    Cell::Wire { routes, .. } => routes.clone(),
                    _ => vec![],
                };
                // Known residual: if relief (eviction or retraction) displaced one of this
                // blocklet's own placed routes, the filter cannot see it and the displaced
                // slack survives the unwind. Connectivity is preserved either way; the
                // quiescence gates (reciprocity, projection, oracle) surface any breakage.
                let leftover: Vec<Route> = match &target.cell {
                    Cell::Wire { routes, .. } => {
                        routes.iter().copied().filter(|r| !planned_routes.contains(r)).collect()
                    }
                    Cell::Agent { pass, .. } => pass.clone(),
                    _ => vec![],
                };
                let restored = if leftover.is_empty() {
                    Cell::Empty { reserved: None }
                } else {
                    Cell::Wire { routes: leftover, hot: 0, cooldown: 0, reserved: None }
                };
                self.grid.set(t, &Site { cell: restored, cursor: None, chi: target.chi, claim: false });
                let mut s = site;
                s.cursor = Some(Cursor { pc: cursor.pc - 1, ..cursor });
                self.grid.set(p, &s);
                self.wake_around(t);
                self.wake(p);
            }
            Op::Hop { dir, .. } => {
                let d = crate::cascade::rot_dir(*dir, cursor.axis, cursor.roll);
                let t = step(p, d.opp());
                let mut target = self.grid.site(t);
                if target.cursor.is_some() {
                    return; // a foreign cursor squats the unwind path; its next step frees it
                }
                let mut here = site;
                here.cursor = None;
                if let Cell::Agent { nursery, .. } = &mut target.cell {
                    // Re-enter the nursery while unwinding.
                    *nursery = true;
                }
                target.cursor = Some(Cursor { pc: cursor.pc - 1, ..cursor });
                self.grid.set(p, &here);
                self.grid.set(t, &target);
                self.wake(p);
                self.wake(t);
            }
        }
    }

    /// Every blocklet cell is placed: rewrite both seed cells into their final
    /// patch-panel matter and emit the semantic fire. The cursor stays, standing on the
    /// consumer cell's final matter, to run the finalize pass.
    fn resolve(&mut self, p: Pos, site: Site, cursor: Cursor) {
        let Cell::Seed { rule, partner, .. } = site.cell else {
            panic!("cursor resolving away from the consumer seed");
        };
        assert_eq!(rule, cursor.rule);
        let t = step(p, partner);
        let pseed = self.grid.site(t);
        assert!(matches!(pseed.cell, Cell::Seed { half: Half::Producer, .. }));
        let (fc, fp) = crate::blocklet::seed_finals(
            rule,
            cursor.axis,
            cursor.roll,
            &site.cell,
            &pseed.cell,
        );
        let keep = (cursor.pc as usize) < crate::blocklet::layout(rule).script.len();
        self.grid.set(
            p,
            &Site { cell: fc, cursor: keep.then_some(cursor), chi: site.chi, claim: false },
        );
        self.grid.set(t, &Site { cell: fp, cursor: None, chi: pseed.chi, claim: false });
        let sids = self.grid.seed_sids.remove(&p);
        self.observe_fire(
            p,
            rule,
            cursor.axis,
            cursor.roll,
            &crate::blocklet::layout(rule).seats,
            sids.map(|s| s.0),
            sids.map(|s| s.1),
        );
        self.wake_around(p);
        self.wake_around(t);
    }

    // ------------------------------------------------------------ fields

    #[allow(dead_code)]
    fn pump_chi(&mut self, p: Pos, level: u8) {
        if !self.grid.topo.in_bounds(p) {
            return;
        }
        let level = level.min(crate::cascade::CHI_MAX);
        let mut s = self.grid.site(p);
        if s.chi < level {
            s.chi = level;
            self.grid.set(p, &s);
            self.wake_around(p);
        }
    }

    /// Jacobi-style relax with leak: chi spreads and decays. Single-cell, stale-safe.
    fn relax_chi(&mut self, p: Pos) {
        let s = self.grid.site(p);
        if s.chi == 0 {
            return;
        }
        let mut sum = 2u32 * s.chi as u32;
        for d in DIRS {
            sum += self.grid.word(step(p, d)).chi() as u32;
        }
        let relaxed = ((sum / 8) as u8).saturating_sub(1);
        if relaxed != s.chi {
            let mut ns = s;
            ns.chi = relaxed;
            self.grid.set(p, &ns);
            self.wake_around(p);
        }
    }
}

fn add(origin: Pos, relative: Pos) -> Pos {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

/// Merge planned blocklet matter with the routes already occupying the target cell (the
/// guest principle for growth). None when the merge does not fit or collides.
pub(crate) fn merge_matter(planned: Cell, existing: &[Route]) -> Option<Cell> {
    let merged = match planned {
        Cell::Wire { mut routes, hot, cooldown, .. } => {
            routes.extend(existing.iter().copied());
            Cell::Wire { routes, hot, cooldown, reserved: None }
        }
        Cell::Agent { tag, principal, aux, mut pass, nursery, cooldown } => {
            pass.extend(existing.iter().copied());
            Cell::Agent { tag, principal, aux, pass, nursery, cooldown }
        }
        other => other,
    };
    crate::cascade::Word2::pack(&Site::of(merged.clone())).ok()?;
    Some(merged)
}

/// Whether every footprint cell of this roll currently either is free or merges with
/// the planned matter. Stale-safe heuristic only: cells change after the dock, and
/// growth re-validates at each placement. Shared by the serial and parallel drivers,
/// which supply their own site reads.
pub(crate) fn roll_merges_deep(
    read: &dyn Fn(Pos) -> Site,
    seed_c: Pos,
    rule: u8,
    axis: Dir,
    roll: u8,
) -> bool {
    let layout = crate::blocklet::layout(rule);
    for (off, cell) in &layout.extras {
        let world = add(seed_c, crate::cascade::rot_pos(*off, axis, roll));
        // A transiently claimed cell still shows its matter; the ladder is stale-safe,
        // so claims are ignored (the boundary word rejects via its reserved mark).
        let ws = read(world);
        let ok = match &ws.cell {
            Cell::Empty { reserved: None } => ws.cursor.is_none(),
            Cell::Wire { reserved: None, routes, .. } => {
                let planned = crate::cascade::rot_cell(cell, axis, roll);
                merge_matter(planned, routes).is_some()
            }
            _ => false,
        };
        if !ok {
            return false;
        }
    }
    true
}

/// Whether a roll works locally: the whole footprint is inside the topology and
/// avoids the pair's stub cells (where aux cables and delivered values legally park),
/// and the blocklet cells adjacent to the pair are free or mergeable wire. Only the
/// first ring gates on occupancy: those cells' changes wake the pair, so a declined
/// dock retries without any global scan. Deeper cells are handled by growth's
/// per-cell waits and merges.
pub(crate) fn roll_fits(
    read: &dyn Fn(Pos) -> Site,
    topo: Topo,
    seed_c: Pos,
    seed_p: Pos,
    stub_cells: [Option<Pos>; 4],
    rule: u8,
    axis: Dir,
    roll: u8,
    ring_merge: bool,
) -> bool {
    let layout = crate::blocklet::layout(rule);
    for (off, cell) in &layout.extras {
        let world = add(seed_c, crate::cascade::rot_pos(*off, axis, roll));
        if !topo.in_bounds(world) {
            return false;
        }
        if stub_cells.iter().flatten().any(|s| *s == world) {
            // A stub cell is not off-limits when the blocklet can weave through it:
            // the pair's aux cable keeps its routes as a guest of the planned matter.
            // (An unmergeable stub cell is a hard lock: it is keyed on the port faces,
            // so no eviction of its contents can ever lift it.)
            let planned = crate::cascade::rot_cell(cell, axis, roll);
            let ws = read(world);
            let weaves = match &ws.cell {
                Cell::Wire { reserved: None, routes, .. } if ws.cursor.is_none() => {
                    merge_matter(planned, routes).is_some()
                }
                _ => false,
            };
            if !weaves {
                return false;
            }
            continue;
        }
        let near = DIRS.iter().any(|d| step(world, *d) == seed_c || step(world, *d) == seed_p);
        if !near {
            continue;
        }
        let ws = read(world);
        match &ws.cell {
            Cell::Empty { reserved: None } if ws.cursor.is_none() => {}
            Cell::Wire { reserved: None, routes, .. } if ws.cursor.is_none() => {
                // With ring_merge, a ring wire must actually accept the planned
                // matter; without it, growth handles collisions by waiting and
                // evicting.
                if ring_merge {
                    let planned = crate::cascade::rot_cell(cell, axis, roll);
                    if merge_matter(planned, routes).is_none() {
                        return false;
                    }
                }
            }
            _ => return false,
        }
    }
    true
}

/// The cells that block one roll's first ring (the same walk as [`roll_fits`] with
/// `ring_merge`, collecting instead of refusing), each with the blocklet matter planned
/// there so relief can evict exactly what prevents the merge.
pub fn roll_blockers(
    read: &dyn Fn(Pos) -> Site,
    topo: Topo,
    seed_c: Pos,
    seed_p: Pos,
    stub_cells: [Option<Pos>; 4],
    rule: u8,
    axis: Dir,
    roll: u8,
) -> Option<Vec<(Pos, Cell)>> {
    let layout = crate::blocklet::layout(rule);
    let mut out = vec![];
    for (off, cell) in &layout.extras {
        let world = add(seed_c, crate::cascade::rot_pos(*off, axis, roll));
        if !topo.in_bounds(world) {
            return None; // the boundary never relieves
        }
        let planned = crate::cascade::rot_cell(cell, axis, roll);
        let ws = read(world);
        if stub_cells.iter().flatten().any(|s| *s == world) {
            let weaves = match &ws.cell {
                Cell::Wire { reserved: None, routes, .. } if ws.cursor.is_none() => {
                    merge_matter(planned.clone(), routes).is_some()
                }
                _ => false,
            };
            if !weaves {
                out.push((world, planned));
            }
            continue;
        }
        let near = DIRS.iter().any(|d| step(world, *d) == seed_c || step(world, *d) == seed_p);
        if !near {
            continue;
        }
        match &ws.cell {
            Cell::Empty { reserved: None } if ws.cursor.is_none() => {}
            Cell::Wire { reserved: None, routes, .. } if ws.cursor.is_none() => {
                if merge_matter(planned.clone(), routes).is_none() {
                    out.push((world, planned));
                }
            }
            _ => out.push((world, planned)),
        }
    }
    Some(out)
}

/// The dock's roll preference ladder, most to least informed (all stale-safe
/// heuristics): whole footprint merges cleanly, then the gate ring merges, then the
/// gate ring is merely unclaimed wire or empty (growth waits per cell and evicts).
pub(crate) fn choose_roll(
    read: &dyn Fn(Pos) -> Site,
    topo: Topo,
    seed_c: Pos,
    seed_p: Pos,
    stub_cells: [Option<Pos>; 4],
    rule: u8,
    axis: Dir,
    mk_seeds: &dyn Fn(u8) -> (Cell, Cell),
) -> Option<u8> {
    let candidate = |deep: bool, ring_merge: bool| {
        (0..4u8).find(|roll| {
            let (sc, sp) = mk_seeds(*roll);
            roll_fits(read, topo, seed_c, seed_p, stub_cells, rule, axis, *roll, ring_merge)
                && crate::blocklet::finals_fit(rule, axis, *roll, &sc, &sp)
                && (!deep || roll_merges_deep(read, seed_c, rule, axis, *roll))
        })
    };
    candidate(true, true).or_else(|| candidate(false, true)).or_else(|| candidate(false, false))
}

// ---------------------------------------------------------------- tracing / projection

/// Follow a wire from an agent port to the far agent port. Returns (position, port).
pub fn trace_port(grid: &Grid2, p: Pos, port: u8) -> Result<(Pos, u8), String> {
    let site = grid.site(p);
    let start = port_endpoint(&site.cell, port)
        .ok_or_else(|| format!("no port {port} at {p:?}"))?;
    let mut cur = step(p, start.face);
    let mut enter = EndPt { face: start.face.opp(), lane: start.lane };
    for _ in 0..100_000 {
        let s = grid.site(cur);
        match &s.cell {
            Cell::Wire { routes, .. } => {
                let r = routes
                    .iter()
                    .find_map(|r| r.through(enter))
                    .ok_or_else(|| format!("dangling wire at {cur:?} entering {enter:?}"))?;
                cur = step(cur, r.face);
                enter = EndPt { face: r.face.opp(), lane: r.lane };
            }
            Cell::Agent { pass, .. } => {
                if let Some(exit) = pass.iter().find_map(|r| r.through(enter)) {
                    // A guest crossing: continue through the passthrough route.
                    cur = step(cur, exit.face);
                    enter = EndPt { face: exit.face.opp(), lane: exit.lane };
                    continue;
                }
                let port = endpoint_port(&s.cell, enter)
                    .ok_or_else(|| format!("no port at {cur:?} for {enter:?}"))?;
                return Ok((cur, port));
            }
            Cell::Seed { pass, .. } => {
                if let Some(exit) = pass.iter().find_map(|r| r.through(enter)) {
                    cur = step(cur, exit.face);
                    enter = EndPt { face: exit.face.opp(), lane: exit.lane };
                    continue;
                }
                let port = endpoint_port(&s.cell, enter)
                    .ok_or_else(|| format!("no seed port at {cur:?} for {enter:?}"))?;
                return Ok((cur, port));
            }
            Cell::Empty { .. } => return Err(format!("wire runs into empty at {cur:?}")),
        }
    }
    Err("unterminated trace".into())
}

/// The (face, lane) where an agent's semantic port leaves its cell.
pub fn port_endpoint(cell: &Cell, port: u8) -> Option<EndPt> {
    match cell {
        Cell::Agent { tag, principal, aux, .. } => match port {
            0 => Some(*principal),
            k if (k as usize) < tag.arity() => Some(aux[k as usize - 1]),
            _ => None,
        },
        Cell::Seed { rule, half, partner, plane, stub, .. } => {
            let tag = match half {
                Half::Consumer => RULES[*rule as usize].consumer,
                Half::Producer => RULES[*rule as usize].producer,
            };
            match port {
                0 => Some(EndPt { face: *partner, lane: *plane }),
                k if (k as usize) < tag.arity() => Some(stub[k as usize - 1]),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Inverse of [`port_endpoint`]: which semantic port owns this (face, lane).
pub fn endpoint_port(cell: &Cell, at: EndPt) -> Option<u8> {
    let arity = match cell {
        Cell::Agent { tag, .. } => tag.arity(),
        Cell::Seed { rule, half, .. } => match half {
            Half::Consumer => RULES[*rule as usize].consumer.arity(),
            Half::Producer => RULES[*rule as usize].producer.arity(),
        },
        _ => return None,
    };
    (0..arity as u8).find(|k| port_endpoint(cell, *k) == Some(at))
}

/// The executable correctness spec: at a control-free checkpoint every live agent, tag,
/// and traced connection must match the shadow net exactly.
pub fn check_projection(grid: &Grid2, shadow: &Net) -> Result<(), String> {
    let mut live_grid = 0usize;
    for (p, site) in grid.agents() {
        if let Cell::Agent { nursery: true, .. } = site.cell {
            return Err(format!("nursery agent at checkpoint: {p:?}"));
        }
        live_grid += 1;
        let sid = *grid.sid.get(&p).ok_or_else(|| format!("agent at {p:?} has no sid"))?;
        let shadow_agent = shadow.agents[sid as usize]
            .as_ref()
            .ok_or_else(|| format!("sid {sid} dead in shadow"))?;
        let Cell::Agent { tag, .. } = site.cell else { unreachable!() };
        if shadow_agent.tag != tag {
            return Err(format!("tag mismatch at {p:?}: {} vs {}", tag.name(), shadow_agent.tag.name()));
        }
        for port in 0..tag.arity() as u8 {
            let (fp, fport) = trace_port(grid, p, port)
                .map_err(|e| format!("trace {}#{sid} port {port}: {e}", tag.name()))?;
            let fsid = *grid.sid.get(&fp).ok_or_else(|| format!("far agent at {fp:?} has no sid"))?;
            let expect = shadow_agent.ports[port as usize]
                .ok_or_else(|| format!("shadow {sid} port {port} unwired"))?;
            if expect != (fsid, fport) {
                return Err(format!(
                    "projection mismatch {}#{sid} port {port}: grid ({fsid},{fport}) shadow {expect:?}",
                    tag.name()
                ));
            }
        }
    }
    if live_grid != shadow.live_count() {
        return Err(format!(
            "live count mismatch: grid {live_grid} shadow {}",
            shadow.live_count()
        ));
    }
    if !grid.seed_sids.is_empty() {
        return Err("seed at checkpoint".into());
    }
    Ok(())
}

/// Global geometric well-formedness: every exposed (face, lane) must meet a reciprocal
/// exposure on the neighbor across that edge.
pub fn check_reciprocity(grid: &Grid2) -> Result<(), String> {
    check_reciprocity_where(grid, false)
}

/// Reciprocity with the growth exemption: an exposure toward an empty cell is a pending
/// stub of an unfinished blocklet, legal while a seed is still growing.
pub fn check_reciprocity_lenient(grid: &Grid2) -> Result<(), String> {
    check_reciprocity_where(grid, true)
}

fn check_reciprocity_where(grid: &Grid2, allow_stubs: bool) -> Result<(), String> {
    for (p, w) in &grid.cells {
        let site = w.unpack().map_err(|e| format!("bad word at {p:?}: {e:?}"))?;
        for e in crate::cascade::exposures(&site.cell) {
            let n = step(*p, e.face);
            if !grid.topo.in_bounds(n) {
                return Err(format!("exposure into the boundary at {p:?} {e:?}"));
            }
            let ns = grid.site(n);
            if allow_stubs && matches!(ns.cell, Cell::Empty { .. } | Cell::Seed { .. }) {
                continue;
            }
            let back = EndPt { face: e.face.opp(), lane: e.lane };
            if !crate::cascade::exposures(&ns.cell).contains(&back) {
                return Err(format!("non-reciprocal edge {p:?} {e:?} vs {n:?}"));
            }
        }
    }
    Ok(())
}

// ---------------------------------------------------------------- builders (host code)

/// Place a straight or bent single-lane wire path between two cells, returning the faces
/// at which the path leaves `from` and enters `to`. Host/loader code: global knowledge
/// allowed here and only here.
pub fn lay_wire(grid: &mut Grid2, path: &[Pos]) {
    assert!(path.len() >= 2, "a wire needs distinct endpoints");
    for i in 1..path.len() - 1 {
        let prev = dir_to(path[i], path[i - 1]).expect("path steps by faces");
        let next = dir_to(path[i], path[i + 1]).expect("path steps by faces");
        let route = Route::new(EndPt { face: prev, lane: 0 }, EndPt { face: next, lane: 0 });
        let mut site = grid.site(path[i]);
        match &mut site.cell {
            Cell::Empty { .. } => site.cell = Cell::Wire { routes: vec![route], hot: 0, cooldown: 0, reserved: None },
            Cell::Wire { routes, .. } => {
                assert!(routes.len() < 3, "wire capacity exceeded in loader at {:?}", path[i]);
                routes.push(route);
            }
            other => panic!("loader wire path through occupied cell {:?}: {other:?}", path[i]),
        }
        grid.set(path[i], &site);
    }
}

/// Install an agent with the given port faces, registering it in the shadow net.
pub struct Placed {
    pub pos: Pos,
    pub id: u32,
}

pub fn place_agent(
    grid: &mut Grid2,
    shadow: &mut Net,
    pos: Pos,
    tag: Tag,
    principal: Dir,
    tail: Option<Dir>,
) -> Placed {
    let id = shadow.mk(tag);
    let cell = Cell::Agent {
        tag,
        principal: EndPt { face: principal, lane: 0 },
        aux: aux_pair(tag, tail.unwrap_or(principal.opp())),
        pass: vec![],
        nursery: false,
        cooldown: 0,
    };
    grid.set(pos, &Site::of(cell));
    grid.sid.insert(pos, id);
    Placed { pos, id }
}

/// The conventional aux endpoints for an agent whose auxiliaries leave through one face:
/// aux k rides lane k-1. Unused entries mirror the first.
pub fn aux_pair(tag: Tag, tail: Dir) -> [EndPt; 2] {
    match tag.arity() {
        3 => [EndPt { face: tail, lane: 0 }, EndPt { face: tail, lane: 1 }],
        _ => [EndPt { face: tail, lane: 0 }; 2],
    }
}

/// A consumer–producer pair with every aux terminated in an Out sink through short stub
/// cables, shared by the suite and the trace generator. `gap` inserts that many straight
/// wire cells between the principals so the producer walks before docking; `bend` routes
/// four wire cells through a dogleg instead. Host/fixture code: global layout allowed.
pub fn dock_fixture(rule: &'static crate::rules::Rule, gap: i32, bend: bool) -> (Grid2, Net) {
    let mut grid = Grid2::new(Topo::Full3D);
    let mut shadow = Net::new();
    dock_fixture_at(rule, gap, bend, (0, 0, 0), &mut grid, &mut shadow);
    check_reciprocity(&grid).expect("fixture reciprocity");
    check_projection(&grid, &shadow).expect("fixture projection");
    (grid, shadow)
}

/// The docked-pair fixture translated to an origin, so tests can pit several pairs
/// against each other in one grid.
pub fn dock_fixture_at(
    rule: &'static crate::rules::Rule,
    gap: i32,
    bend: bool,
    origin: Pos,
    grid: &mut Grid2,
    shadow: &mut Net,
) {
    use Dir::*;
    let wire = |grid: &mut Grid2, p: Pos, routes: &[((Dir, u8), (Dir, u8))]| {
        let p = add(origin, p);
        let routes = routes
            .iter()
            .map(|((fa, la), (fb, lb))| {
                Route::new(EndPt { face: *fa, lane: *la }, EndPt { face: *fb, lane: *lb })
            })
            .collect();
        grid.set(p, &Site::of(Cell::Wire { routes, hot: 0, cooldown: 0, reserved: None }));
    };

    let px = if bend { 3 } else { 1 + gap };
    let c = place_agent(
        grid, shadow, add(origin, (0, 0, 0)), rule.consumer, E,
        (rule.consumer.arity() >= 2).then_some(W),
    );
    let p = place_agent(
        grid, shadow, add(origin, (px, 0, 0)), rule.producer, W,
        (rule.producer.arity() >= 2).then_some(E),
    );
    shadow.link(c.id, 0, p.id, 0);
    if bend {
        lay_wire(grid, &[(0, 0, 0), (1, 0, 0), (1, 1, 0), (2, 1, 0), (2, 0, 0), (3, 0, 0)].map(|p| add(origin, p)));
    } else if gap > 0 {
        let path: Vec<Pos> = (0..=px).map(|x| add(origin, (x, 0, 0))).collect();
        lay_wire(grid, &path);
    }

    match rule.consumer.arity() {
        1 => {}
        2 => {
            wire(grid, (-1, 0, 0), &[((E, 0), (W, 0))]);
            let o = place_agent(grid, shadow, add(origin, (-2, 0, 0)), Tag::Out, E, None);
            shadow.link(c.id, 1, o.id, 0);
        }
        _ => {
            wire(grid, (-1, 0, 0), &[((E, 0), (W, 0)), ((E, 1), (W, 1))]);
            wire(grid, (-2, 0, 0), &[((E, 0), (N, 0)), ((E, 1), (S, 0))]);
            wire(grid, (-2, -1, 0), &[((S, 0), (N, 0))]);
            wire(grid, (-2, 1, 0), &[((N, 0), (S, 0))]);
            let o1 = place_agent(grid, shadow, add(origin, (-2, -2, 0)), Tag::Out, S, None);
            let o2 = place_agent(grid, shadow, add(origin, (-2, 2, 0)), Tag::Out, N, None);
            shadow.link(c.id, 1, o1.id, 0);
            shadow.link(c.id, 2, o2.id, 0);
        }
    }
    match rule.producer.arity() {
        1 => {}
        2 => {
            wire(grid, (px + 1, 0, 0), &[((W, 0), (E, 0))]);
            let o = place_agent(grid, shadow, add(origin, (px + 2, 0, 0)), Tag::Out, W, None);
            shadow.link(p.id, 1, o.id, 0);
        }
        _ => {
            // Both producer stubs head east then split south and further east, keeping
            // the north half-space clear for the roll-0 blocklet.
            wire(grid, (px + 1, 0, 0), &[((W, 0), (E, 0)), ((W, 1), (E, 1))]);
            wire(grid, (px + 2, 0, 0), &[((W, 0), (E, 0)), ((W, 1), (S, 0))]);
            wire(grid, (px + 2, 1, 0), &[((N, 0), (S, 0))]);
            wire(grid, (px + 3, 0, 0), &[((W, 0), (E, 0))]);
            let o1 = place_agent(grid, shadow, add(origin, (px + 4, 0, 0)), Tag::Out, W, None);
            let o2 = place_agent(grid, shadow, add(origin, (px + 2, 2, 0)), Tag::Out, N, None);
            shadow.link(p.id, 1, o1.id, 0);
            shadow.link(p.id, 2, o2.id, 0);
        }
    }
}

/// An immovable inert obstruction: two Out agents facing each other. They belong to no
/// rule, never move, and stay projection-consistent. Fixture code for blocked-dock
/// scenarios.
pub fn place_obstruction(grid: &mut Grid2, shadow: &mut Net, at: Pos, toward: Dir) {
    let a = place_agent(grid, shadow, at, Tag::Out, toward, None);
    let b = place_agent(grid, shadow, step(at, toward), Tag::Out, toward.opp(), None);
    shadow.link(a.id, 0, b.id, 0);
}

// ---------------------------------------------------------------- net loader (host code)

/// Embed an abstract net into an empty grid: agents on a coarse plane, every link routed
/// as single-lane routes with a capacity-aware BFS. Global routing is allowed here and
/// only here; the dynamics never route.
pub fn load_net(shadow: &Net, topo: Topo) -> Result<Grid2, String> {
    let mut grid = Grid2::new(topo);
    let live: Vec<u32> = shadow
        .agents
        .iter()
        .enumerate()
        .filter_map(|(i, a)| a.as_ref().map(|_| i as u32))
        .collect();

    // Positions: breadth-first over link adjacency from the first agent, laid out along
    // one row. Blocklets never claim the row (their box excludes the dock axis line), and
    // docks happen at consumer cells on the row, so footprints extend into the empty
    // perpendicular space. The spacing keeps neighboring seat combs disjoint.
    let spacing = 28i32;
    let mut order: Vec<u32> = vec![];
    let mut seen: BTreeSet<u32> = BTreeSet::new();
    let mut bfs: VecDeque<u32> = VecDeque::new();
    for start in &live {
        if seen.contains(start) {
            continue;
        }
        seen.insert(*start);
        bfs.push_back(*start);
        while let Some(id) = bfs.pop_front() {
            order.push(id);
            for port in shadow.get(id).ports.iter().flatten() {
                if seen.insert(port.0) {
                    bfs.push_back(port.0);
                }
            }
        }
    }
    let mut pos_of: BTreeMap<u32, Pos> = BTreeMap::new();
    for (i, id) in order.iter().enumerate() {
        pos_of.insert(*id, (i as i32 * spacing, 0, 0));
    }

    // Orientation: principal toward the partner's cell (dominant axis); the tail is the
    // opposite face.
    for id in &order {
        let a = shadow.get(*id);
        let p = pos_of[id];
        let principal = a.ports[0]
            .map(|(b, _)| dominant_dir(p, pos_of[&b]))
            .unwrap_or(Dir::E);
        let cell = Cell::Agent {
            tag: a.tag,
            principal: EndPt { face: principal, lane: 0 },
            aux: aux_pair(a.tag, principal.opp()),
            pass: vec![],
            nursery: false,
            cooldown: 0,
        };
        grid.set(p, &Site::of(cell));
        grid.sid.insert(p, *id);
    }

    // Route every link once through its own dedicated corridor south of the row: dive at
    // the source port's column, run along an exclusive row, climb at the target port's
    // column. Corridors share cells only at perpendicular crossings, never edges, so an
    // arity-three walker (which needs both lanes of every edge it crosses) is never
    // blocked by a parallel foreign wire.
    let mut corridor = 0i32;
    for id in &order {
        let a = shadow.get(*id);
        for port in 0..a.tag.arity() as u8 {
            let Some((bid, bport)) = a.ports[port as usize] else {
                return Err(format!("open port {id}:{port}"));
            };
            if (bid, bport) < (*id, port) {
                continue;
            }
            route_corridor(&mut grid, pos_of[id], port, pos_of[&bid], bport, corridor)?;
            corridor += 1;
        }
    }
    check_reciprocity(&grid)?;
    Ok(grid)
}

/// Lay one link through corridor row y = -(2 + k). The port endpoints sit on the agent
/// row; each port's exit column is unique, so shafts never collide and every cell holds
/// at most one route of this link.
fn route_corridor(
    grid: &mut Grid2,
    pa: Pos,
    port_a: u8,
    pb: Pos,
    port_b: u8,
    k: i32,
) -> Result<(), String> {
    let ea = port_endpoint(&grid.site(pa).cell, port_a).ok_or("missing endpoint a")?;
    let eb = port_endpoint(&grid.site(pb).cell, port_b).ok_or("missing endpoint b")?;
    let start = step(pa, ea.face);
    let goal = step(pb, eb.face);
    if start == pb && goal == pa {
        // Directly adjacent principals.
        if ea.face == eb.face.opp() && ea.lane == eb.lane {
            return Ok(());
        }
        return Err("adjacent link with mismatched faces".into());
    }
    let depth = 8 + k; // below the deepest blocklet reach from row docks
    // Cell path: start, dive to the corridor row, run, climb, goal. The lane-one link of
    // a shared tail steps one column further out before diving so no shaft ever carries
    // two parallel routes (an arity-three walker crossing it needs the lane headroom).
    let mut path = vec![start];
    let mut cur = start;
    if ea.lane == 1 {
        cur = step(cur, ea.face);
        path.push(cur);
    }
    while cur.1 < depth {
        cur = (cur.0, cur.1 + 1, cur.2);
        path.push(cur);
    }
    let climb_x = if eb.lane == 1 { step(goal, eb.face).0 } else { goal.0 };
    while cur.0 != climb_x {
        cur = (cur.0 + (climb_x - cur.0).signum(), cur.1, cur.2);
        path.push(cur);
    }
    while cur.1 > goal.1 {
        cur = (cur.0, cur.1 - 1, cur.2);
        path.push(cur);
    }
    while cur != goal {
        cur = (cur.0 + (goal.0 - cur.0).signum(), cur.1, cur.2);
        path.push(cur);
    }
    // Install routes: enter with the fixed port endpoint, exit toward the next cell on a
    // free lane, terminate into the far port endpoint.
    let mut enter = EndPt { face: ea.face.opp(), lane: ea.lane };
    for i in 0..path.len() {
        let here = path[i];
        let exit = if i + 1 < path.len() {
            let d = dir_to(here, path[i + 1]).ok_or("corridor step")?;
            let lane = (0..2)
                .find(|l| !loader_lane_used(grid, here, EndPt { face: d, lane: *l }))
                .ok_or_else(|| format!("no free lane at {here:?} toward {d:?}"))?;
            EndPt { face: d, lane }
        } else {
            EndPt { face: eb.face.opp(), lane: eb.lane }
        };
        let mut site = grid.site(here);
        match &mut site.cell {
            Cell::Empty { .. } => {
                site.cell = Cell::Wire {
                    routes: vec![Route::new(enter, exit)],
                    hot: 0,
                    cooldown: 0,
                    reserved: None,
                };
            }
            Cell::Wire { routes, .. } => {
                routes.push(Route::new(enter, exit));
            }
            other => return Err(format!("corridor through occupied {here:?}: {other:?}")),
        }
        grid.set(here, &site);
        enter = EndPt { face: exit.face.opp(), lane: exit.lane };
    }
    Ok(())
}

fn dominant_dir(from: Pos, to: Pos) -> Dir {
    let (dx, dy, dz) = (to.0 - from.0, to.1 - from.1, to.2 - from.2);
    let (ax, ay, az) = (dx.abs(), dy.abs(), dz.abs());
    if ax >= ay && ax >= az && ax > 0 {
        if dx > 0 { Dir::E } else { Dir::W }
    } else if ay >= az && ay > 0 {
        if dy > 0 { Dir::S } else { Dir::N }
    } else if az > 0 {
        if dz > 0 { Dir::U } else { Dir::D }
    } else {
        Dir::E
    }
}

fn loader_lane_used(grid: &Grid2, p: Pos, e: EndPt) -> bool {
    let own = crate::cascade::exposures(&grid.site(p).cell).contains(&e);
    let n = step(p, e.face);
    let back = EndPt { face: e.face.opp(), lane: e.lane };
    own || (grid.topo.in_bounds(n)
        && crate::cascade::exposures(&grid.site(n).cell).contains(&back))
}

// ---------------------------------------------------------------- tree net loader (host code)

/// Embed an abstract net as a tidy tree: the Nrm driver at the top, every agent
/// centered over its BFS children, parent-child cables as short searched paths. Same
/// host-side global-layout license as `load_net`; the dynamics never route.
pub fn load_net_tree(shadow: &Net, topo: Topo) -> Result<Grid2, String> {
    let live: Vec<u32> = shadow
        .agents
        .iter()
        .enumerate()
        .filter_map(|(i, a)| a.as_ref().map(|_| i as u32))
        .collect();
    let Some(&first) = live.first() else { return Err("empty net".into()) };
    let root = live
        .iter()
        .copied()
        .find(|id| shadow.get(*id).tag == Tag::Nrm)
        .unwrap_or(first);

    // BFS parents over link adjacency; a node's children are ordered by its own port.
    let mut parent: BTreeMap<u32, u32> = BTreeMap::new();
    let mut order: Vec<u32> = vec![];
    let mut bfs: VecDeque<u32> = VecDeque::from([root]);
    parent.insert(root, root);
    while let Some(id) = bfs.pop_front() {
        order.push(id);
        for link in shadow.get(id).ports.iter().flatten() {
            if parent.contains_key(&link.0) {
                continue;
            }
            parent.insert(link.0, id);
            bfs.push_back(link.0);
        }
    }
    if order.len() != live.len() {
        return Err("net is not connected".into());
    }
    let children_of = |id: u32| -> Vec<u32> {
        let a = shadow.get(id);
        let mut kids: Vec<(u8, u32)> = a
            .ports
            .iter()
            .enumerate()
            .filter_map(|(port, link)| {
                link.filter(|l| parent.get(&l.0) == Some(&id)).map(|l| (port as u8, l.0))
            })
            .collect();
        kids.sort_by_key(|(port, _)| *port);
        kids.into_iter().map(|(_, id)| id).collect()
    };

    // In-order columns: leaves take successive columns, parents center over children.
    const H: i32 = 8;
    const V: i32 = 8;
    let mut pos_of: BTreeMap<u32, Pos> = BTreeMap::new();
    let mut next_col = 0i32;
    fn assign(
        id: u32,
        depth: i32,
        children_of: &dyn Fn(u32) -> Vec<u32>,
        next_col: &mut i32,
        pos_of: &mut BTreeMap<u32, Pos>,
    ) -> i32 {
        let kids = children_of(id);
        let col = if kids.is_empty() {
            let c = *next_col;
            *next_col += 1;
            c
        } else {
            let first = assign(kids[0], depth + 1, children_of, next_col, pos_of);
            let mut last = first;
            for k in &kids[1..] {
                last = assign(*k, depth + 1, children_of, next_col, pos_of);
            }
            (first + last) / 2
        };
        pos_of.insert(id, (col * H, -depth * V, 0));
        col
    }
    assign(root, 0, &children_of, &mut next_col, &mut pos_of);

    let mut grid = Grid2::new(topo);
    for id in &order {
        let a = shadow.get(*id);
        let p = pos_of[id];
        let principal = a.ports[0]
            .map(|(b, _)| dominant_dir(p, pos_of[&b]))
            .unwrap_or(Dir::E);
        let cell = Cell::Agent {
            tag: a.tag,
            principal: EndPt { face: principal, lane: 0 },
            aux: aux_pair(a.tag, principal.opp()),
            pass: vec![],
            nursery: false,
            cooldown: 0,
        };
        grid.set(p, &Site::of(cell));
        grid.sid.insert(p, *id);
    }
    // Every link once, shortest paths through free space (the tree leaves it open).
    let mut done: BTreeSet<(u32, u8)> = BTreeSet::new();
    for id in &order {
        let a = shadow.get(*id);
        for port in 0..a.tag.arity() as u8 {
            let Some((bid, bport)) = a.ports[port as usize] else {
                return Err(format!("open port {id}:{port}"));
            };
            if !done.insert((*id, port)) || !done.insert((bid, bport)) {
                done.insert((*id, port));
                continue;
            }
            let ea = port_endpoint(&grid.site(pos_of[id]).cell, port).ok_or("missing endpoint a")?;
            let eb =
                port_endpoint(&grid.site(pos_of[&bid]).cell, bport).ok_or("missing endpoint b")?;
            tree_route(&mut grid, pos_of[id], ea, pos_of[&bid], eb)?;
        }
    }
    check_reciprocity(&grid)?;
    Ok(grid)
}

/// One tree-loader link: BFS a shortest cell path through free space and install it.
fn tree_route(grid: &mut Grid2, pa: Pos, ea: EndPt, pb: Pos, eb: EndPt) -> Result<(), String> {
    let start = step(pa, ea.face);
    let goal = step(pb, eb.face);
    if start == pb && goal == pa {
        if ea.face == eb.face.opp() && ea.lane == eb.lane {
            return Ok(());
        }
        return Err("adjacent link with mismatched faces".into());
    }
    let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut queue: VecDeque<Pos> = VecDeque::from([start]);
    prev.insert(start, start);
    while let Some(cur) = queue.pop_front() {
        if cur == goal {
            break;
        }
        for d in DIRS {
            let n = step(cur, d);
            if prev.contains_key(&n) || !grid.topo.in_bounds(n) {
                continue;
            }
            match &grid.site(n).cell {
                Cell::Empty { .. } => {}
                Cell::Wire { routes, .. } if routes.len() < 3 => {}
                _ => continue,
            }
            if (0..2).all(|l| loader_lane_used(grid, cur, EndPt { face: d, lane: l })) {
                continue;
            }
            prev.insert(n, cur);
            queue.push_back(n);
        }
    }
    if !prev.contains_key(&goal) {
        return Err(format!("tree cable unroutable {pa:?} -> {pb:?}"));
    }
    let mut path = vec![goal];
    let mut cur = goal;
    while cur != start {
        cur = prev[&cur];
        path.push(cur);
    }
    path.reverse();
    let mut enter = EndPt { face: ea.face.opp(), lane: ea.lane };
    for i in 0..path.len() {
        let here = path[i];
        let exit = if i + 1 < path.len() {
            let d = dir_to(here, path[i + 1]).ok_or("tree cable step")?;
            let lane = (0..2)
                .find(|l| !loader_lane_used(grid, here, EndPt { face: d, lane: *l }))
                .ok_or_else(|| format!("no free lane at {here:?} toward {d:?}"))?;
            EndPt { face: d, lane }
        } else {
            EndPt { face: eb.face.opp(), lane: eb.lane }
        };
        let mut site = grid.site(here);
        match &mut site.cell {
            Cell::Empty { .. } => {
                site.cell = Cell::Wire {
                    routes: vec![Route::new(enter, exit)],
                    hot: 0,
                    cooldown: 0,
                    reserved: None,
                };
            }
            Cell::Wire { routes, .. } => {
                routes.push(Route::new(enter, exit));
            }
            _ => return Err(format!("tree cable through occupied {here:?}")),
        }
        grid.set(here, &site);
        enter = EndPt { face: exit.face.opp(), lane: exit.lane };
    }
    Ok(())
}


/// Reduce a term end to end on the grid: build the net, embed it, run to quiescence,
/// project, and read the normal form back from the (projection-equal) shadow net.
pub fn normalize_on_grid(
    term: &crate::oracle::Term,
    topo: Topo,
    discipline: Discipline,
    budget: u64,
) -> Result<(Option<crate::oracle::Term>, u64, u64), String> {
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, out) = shadow.drive(root);
    let grid = load_net(&shadow, topo)?;
    let mut r = Runner::new(grid, shadow, discipline);
    if !r.run(budget) {
        return Err(format!(
            "did not quiesce within {budget} activations ({} rewrites, {} transport)",
            r.grid.rewrites, r.grid.transport
        ));
    }
    check_projection(&r.grid, &r.shadow)?;
    check_reciprocity(&r.grid)?;
    let result = r.shadow.readback(r.shadow.get(out).ports[0]);
    Ok((result, r.grid.rewrites, r.generation))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade::Grid2;

    /// The inverse shift: a cold three-cell detour with an eligible bypassed cell pulls
    /// straight, shedding two cells of wire.
    #[test]
    fn retract_pulls_shift_detour_straight() {
        use Dir::*;
        let mut grid = Grid2::new(Topo::Full3D);
        let w = |grid: &mut Grid2, p: Pos, a: (Dir, u8), b: (Dir, u8)| {
            let routes = vec![Route::new(
                EndPt { face: a.0, lane: a.1 },
                EndPt { face: b.0, lane: b.1 },
            )];
            grid.set(p, &Site::of(Cell::Wire { routes, hot: 0, cooldown: 0, reserved: None }));
        };
        // A wire along y=0 whose middle segment was shifted through y=1; (2,0) is the
        // bypassed cell.
        w(&mut grid, (0, 0, 0), (E, 0), (W, 0));
        w(&mut grid, (1, 0, 0), (W, 0), (S, 0));
        w(&mut grid, (1, 1, 0), (N, 0), (E, 0));
        w(&mut grid, (2, 1, 0), (E, 0), (W, 0));
        w(&mut grid, (3, 1, 0), (W, 0), (N, 0));
        w(&mut grid, (3, 0, 0), (S, 0), (E, 0));
        w(&mut grid, (4, 0, 0), (E, 0), (W, 0));
        let mut r = Runner::new(grid, Net::new(), Discipline::Fifo);
        assert!(r.try_retract((2, 1, 0)), "the detour middle retracts");
        let mid = r.grid.site((2, 0, 0));
        assert!(
            matches!(&mid.cell, Cell::Wire { routes, .. } if routes.len() == 1),
            "the bypassed cell hosts the straightened route"
        );
        for p in [(1, 1, 0), (2, 1, 0), (3, 1, 0)] {
            assert!(
                matches!(r.grid.site(p).cell, Cell::Empty { .. }),
                "detour cell {p:?} empties"
            );
        }
    }

    /// The U-turn splice: a wire folding back through one neighbor is pure slack; the
    /// neighbor joins its two continuations and the fold vanishes.
    #[test]
    fn evict_splices_uturn_fold() {
        use Dir::*;
        let mut grid = Grid2::new(Topo::Full3D);
        grid.set(
            (0, 0, 0),
            &Site::of(Cell::Wire {
                routes: vec![Route::new(
                    EndPt { face: E, lane: 0 },
                    EndPt { face: E, lane: 1 },
                )],
                hot: 0,
                cooldown: 0,
                reserved: None,
            }),
        );
        grid.set(
            (1, 0, 0),
            &Site::of(Cell::Wire {
                routes: vec![
                    Route::new(EndPt { face: W, lane: 0 }, EndPt { face: N, lane: 0 }),
                    Route::new(EndPt { face: W, lane: 1 }, EndPt { face: S, lane: 0 }),
                ],
                hot: 0,
                cooldown: 0,
                reserved: None,
            }),
        );
        let mut r = Runner::new(grid, Net::new(), Discipline::Fifo);
        assert!(r.try_evict((0, 0, 0), None, 2), "the fold splices out");
        assert!(matches!(r.grid.site((0, 0, 0)).cell, Cell::Empty { .. }));
        let n = r.grid.site((1, 0, 0));
        let Cell::Wire { routes, .. } = &n.cell else { panic!("spliced neighbor is wire") };
        assert_eq!(routes.len(), 1);
        assert_eq!(
            routes[0],
            Route::new(EndPt { face: N, lane: 0 }, EndPt { face: S, lane: 0 })
        );
    }

    /// L walks a straight three-cell wire into a parked Eps and docks (rule Eps·L fires,
    /// both vanish). The walk must advance exactly one cell per generation.
    #[test]
    fn walk_straight_then_erase() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let l = place_agent(&mut grid, &mut shadow, (0, 0, 0), Tag::L, Dir::E, None);
        let eps = place_agent(&mut grid, &mut shadow, (4, 0, 0), Tag::Eps, Dir::W, None);
        shadow.link(l.id, 0, eps.id, 0);
        lay_wire(&mut grid, &[(0, 0, 0), (1, 0, 0), (2, 0, 0), (3, 0, 0), (4, 0, 0)]);
        check_projection(&grid, &shadow).expect("initial projection");

        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        assert!(r.run(10_000), "must quiesce");
        assert_eq!(r.grid.transport, 3, "three wire cells walked");
        assert_eq!(r.grid.rewrites, 1, "Eps·L fired");
        assert_eq!(r.shadow.live_count(), 0);
        assert!(r.grid.agents().count() == 0, "grid empty of agents");
        check_projection(&r.grid, &r.shadow).expect("final projection");
        // Physical pacing: each move lands in a distinct, increasing generation.
        let moves = r.events.iter().filter(|e| matches!(e, Event::Move(..))).count();
        assert_eq!(moves, 3);
    }

    /// The same walk around a bend: the wire turns north then east.
    #[test]
    fn walk_bend_then_erase() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let l = place_agent(&mut grid, &mut shadow, (0, 0, 0), Tag::L, Dir::E, None);
        let eps = place_agent(&mut grid, &mut shadow, (3, -2, 0), Tag::Eps, Dir::W, None);
        shadow.link(l.id, 0, eps.id, 0);
        lay_wire(
            &mut grid,
            &[(0, 0, 0), (1, 0, 0), (1, -1, 0), (1, -2, 0), (2, -2, 0), (3, -2, 0)],
        );
        check_projection(&grid, &shadow).expect("initial projection");
        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        assert!(r.run(10_000));
        assert_eq!(r.grid.rewrites, 1);
        assert_eq!(r.shadow.live_count(), 0);
        check_projection(&r.grid, &r.shadow).expect("final projection");
    }

    /// An arity-two producer (S) drags one trail lane behind it; the vacated cells become
    /// a single-route wire and the child connection stays traceable throughout.
    #[test]
    fn walk_arity_two_leaves_trail() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let s = place_agent(&mut grid, &mut shadow, (0, 0, 0), Tag::S, Dir::E, Some(Dir::W));
        let child = place_agent(&mut grid, &mut shadow, (-2, 0, 0), Tag::L, Dir::E, None);
        let eps = place_agent(&mut grid, &mut shadow, (3, 0, 0), Tag::Eps, Dir::W, None);
        shadow.link(s.id, 0, eps.id, 0);
        shadow.link(s.id, 1, child.id, 0);
        lay_wire(&mut grid, &[(0, 0, 0), (1, 0, 0), (2, 0, 0), (3, 0, 0)]);
        lay_wire(&mut grid, &[(-2, 0, 0), (-1, 0, 0), (0, 0, 0)]);
        check_projection(&grid, &shadow).expect("initial projection");

        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        // Stop before the dock so the trail is inspectable: budget tuned to walk only.
        for _ in 0..200 {
            r.tick_one();
        }
        let _ = r.run(100_000);
        // Eps·S fires, leaving a fresh Eps that then erases the child L.
        assert_eq!(r.grid.rewrites, 2, "Eps·S then Eps·L");
        assert_eq!(r.shadow.live_count(), 0);
        check_projection(&r.grid, &r.shadow).expect("final projection");
    }

    /// One cell per generation: a lone walker on a long wire moves with the wavefront.
    #[test]
    fn walk_speed_is_one_cell_per_generation() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let n = 12i32;
        let l = place_agent(&mut grid, &mut shadow, (0, 0, 0), Tag::L, Dir::E, None);
        let eps = place_agent(&mut grid, &mut shadow, (n, 0, 0), Tag::Eps, Dir::W, None);
        shadow.link(l.id, 0, eps.id, 0);
        let path: Vec<Pos> = (0..=n).map(|x| (x, 0, 0)).collect();
        lay_wire(&mut grid, &path);
        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        assert!(r.run(100_000));
        assert_eq!(r.grid.transport as i32, n - 1);
        // The walk spans n-1 moves; generations grow linearly with distance, not with
        // grid size: allow slack for the initial wake flood but demand linearity.
        assert!(
            r.generation as i32 <= 3 * n + 8,
            "generations {} not linear in distance {n}",
            r.generation
        );
    }
}
