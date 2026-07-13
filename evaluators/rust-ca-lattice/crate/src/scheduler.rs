//! The scheduler: the sequential deterministic schedule over the transitions and both
//! topologies, now carrying the two neighbor-local fields — the tick runs a ψ heat sweep,
//! fires (atomic or dock/grow), walker-demanded clears, reels, χ-driven shoves and
//! decongestion/evaporation, and a retract fixpoint, then one χ Jacobi step. Rung 3 adds
//! parallel and async-fuzz schedules over the SAME transitions — a schedule only ever
//! chooses WHICH enabled transitions run and in what order; it can never affect what a
//! transition does (footprints are the transition's own contract).

use crate::lattice::{embed, step, Grid, Pos, Topo};
use crate::net::Net;
use crate::oracle::Term;
use crate::transitions::{
    apply_fire, apply_flip, apply_grow_dock, apply_reel, apply_retract, apply_shove,
    apply_slide, grow_step, plan_fire, plan_fire_stamp, plan_flip, plan_grow_dock,
    plan_reel, plan_retract, plan_shove, plan_slide, plan_slide_ex, reel_blocked,
    DockPlan, FirePlan, GrowStep,
};

/// Which fire planner the schedule uses. `Search` is the in-board backtracking planner;
/// `Stamp` is the precomputed-workshop pattern (purely local at fire time: freeness reads
/// only, waits when the pattern does not fit); `StampThenSearch` stamps when it fits and
/// falls back to the search when it does not.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FireMode { Search, Stamp, StampThenSearch, Grow, GrowThenSearch }

/// Ablation switch for the SHOVE phase (measurement knob).
pub const SHOVE_ON: bool = true;

/// χ level at which a count-1 wire cell EVAPORATES: its cold strand relocates through a
/// strictly-empty trail into strictly cooler cells. Crowded cells (count ≥ 2) decongest
/// at χ ≥ 1 already; the walker-blocking MATS are count-1 — invisible to crowding, yet
/// exactly what fire boards and relaxed clears die on — so under strong pressure they
/// thin from the boundary inward.
pub const CHI_EVAP: u8 = 3;

/// Stall window on SHADOW progress (fires, docks, grow steps). The field dynamics keep a
/// tick busy — shoves, decongestion slides, detour reels — even when the computation is
/// going nowhere, so "a full tick applied nothing" no longer detects every stall. The
/// longest legitimate fire-free stretch on a completing term is a few hundred ticks of
/// pure transport (deep chains); a drought past this window means the polymer moves are
/// churning without ever opening a fire.
pub const PROGRESS_DROUGHT: u64 = 2_000;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CheckLevel {
    /// Projection invariant asserted after EVERY transition (the per-step §10 check).
    Every,
    /// Asserted once per tick.
    Tick,
    /// Asserted at quiescence only.
    End,
}

/// What happened during a tick — observer data for tracing/visualization; the dynamics
/// never read it.
enum FireAction { Atomic(FirePlan), Dock(DockPlan) }

#[derive(Clone, Debug)]
pub enum Event {
    Fire { cpos: Pos, ppos: Pos, rule: (&'static str, &'static str), fresh: Vec<Pos> },
    Reel { from: Pos, to: Pos, sid: u32 },
    Retract { a: Pos, b: Pos },
    Slide { at: Pos },
    Dock { cpos: Pos, ppos: Pos, rule: (&'static str, &'static str) },
    Grow { at: Pos },
    Abort { at: Pos },
    Shove { from: Pos, to: Pos },
    Flip { from: Pos, to: Pos },
}

pub struct Sim {
    pub grid: Grid,
    pub shadow: Net,
    /// Events of the most recent tick.
    pub events: Vec<Event>,
    pub fire_mode: FireMode,
}

impl Sim {
    pub fn load(term: &Term, topo: Topo) -> Sim {
        let mut shadow = Net::new();
        let root = shadow.build(term);
        shadow.drive(root);
        let grid = embed(&shadow, topo);
        Sim { grid, shadow, events: vec![], fire_mode: FireMode::Search }
    }

    fn plan_fire_mode(&self, p: Pos) -> Option<FireAction> {
        use FireAction::*;
        match self.fire_mode {
            FireMode::Search => plan_fire(&self.grid, p).map(Atomic),
            FireMode::Stamp => plan_fire_stamp(&self.grid, p).map(Atomic),
            FireMode::StampThenSearch => plan_fire_stamp(&self.grid, p).map(Atomic)
                .or_else(|| plan_fire(&self.grid, p).map(Atomic)),
            // Grow: stamp when the whole layout is already open (the atomic degenerate
            // case of extrusion), else dock and unfold.
            FireMode::Grow => plan_fire_stamp(&self.grid, p).map(Atomic)
                .or_else(|| plan_grow_dock(&self.grid, p).map(Dock)),
            // hybrid: stamp (clean), then the search (it copes with clutter atomically),
            // then dock-and-grow as the patient last resort for what search cannot fit
            FireMode::GrowThenSearch => plan_fire_stamp(&self.grid, p).map(Atomic)
                .or_else(|| plan_fire(&self.grid, p).map(Atomic))
                .or_else(|| plan_grow_dock(&self.grid, p).map(Dock)),
        }
    }

    /// One sequential tick: fire every enabled pair (rescanning after each, since a fire
    /// invalidates positions); CLEAR one blocking strand for each demanded walker stuck at
    /// a shared cell (the walker-demanded excluded-volume SLIDE); give every producer one
    /// reel step; then run RETRACT to fixpoint (each strictly shortens wire, so this
    /// terminates). Returns the number of transitions applied. Fully deterministic
    /// (BTreeMap coordinate order).
    pub fn tick(&mut self, check: CheckLevel) -> usize {
        self.events.clear();
        let mut applied = 0;
        // ψ: one demand sweep (monotone, so heated strands count as progress and the
        // phase provably quiesces once every live wire is hot)
        applied += self.grid.heat_step();
        // the tension survey: converging sweeps count as progress (they quiesce on
        // static geometry), which keeps the quiet-streak detector honest while fronts
        // are still traveling long wires
        applied += self.grid.survey_step();
        // χ sources collected as the tick runs; the field updates at the end
        let mut pumps: Vec<Pos> = vec![];
        loop {
            let positions: Vec<Pos> = self.grid.agents().map(|(p, _)| p).collect();
            let mut fired = false;
            for p in positions {
                match self.plan_fire_mode(p) {
                    Some(FireAction::Atomic(plan)) => {
                        self.events.push(Event::Fire {
                            cpos: plan.cpos, ppos: plan.ppos,
                            rule: (plan.rule.consumer.name(), plan.rule.producer.name()),
                            fresh: plan.fresh_cells.clone(),
                        });
                        apply_fire(&mut self.grid, &mut self.shadow, &plan);
                        applied += 1;
                        fired = true;
                        if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                        break;
                    }
                    Some(FireAction::Dock(plan)) => {
                        self.events.push(Event::Dock {
                            cpos: plan.cpos, ppos: plan.ppos,
                            rule: (plan.rule.consumer.name(), plan.rule.producer.name()),
                        });
                        apply_grow_dock(&mut self.grid, plan);
                        applied += 1;
                        fired = true;
                        break;
                    }
                    None => {}
                }
            }
            if !fired { break; }
        }
        // FIRE RIGHT OF WAY: an adjacent, facing, rule-licensed pair that no planner can
        // place is the most demanded thing in the system, and it previously had no
        // eviction mechanism at all (walkers did — the CLEAR phase). Each stuck pair
        // gets one licensed eviction per tick: slide one strand out of the prime ring
        // around the dying cells, hot strands included — a wire can be hot here purely
        // because a consumer waits at one end for a value that only this fire creates
        // (a circular wait no anonymous mechanism can break, since hot is immune to the
        // field). The pair also pumps while unfireable, so χ keeps tension out of the
        // room and thins the cold clutter.
        for (cpos, a) in self.grid.agents().map(|(p, a)| (p, a.clone())).collect::<Vec<_>>() {
            if !a.tag.is_consumer() || a.nascent { continue; }
            let Some(d) = a.faces[0] else { continue };
            let ppos = step(cpos, d);
            let Some(b) = self.grid.agent(ppos) else { continue };
            if !b.tag.is_producer() || b.nascent || b.faces[0] != Some(d.opp()) { continue; }
            if self.plan_fire_mode(cpos).is_some() { continue; }
            pumps.push(cpos);
            pumps.push(ppos);
            let mut ring: Vec<Pos> = vec![];
            for base in [cpos, ppos] {
                for dd in crate::lattice::DIRS {
                    let c = step(base, dd);
                    if c != cpos && c != ppos && !ring.contains(&c) { ring.push(c); }
                }
            }
            'evict: for c in ring {
                let Some(w) = self.grid.wire(c) else { continue };
                for s in w.iter().collect::<Vec<_>>() {
                    if let Some(plan) = plan_slide_ex(&self.grid, c, s, &[], false, true) {
                        self.events.push(Event::Slide { at: plan.p });
                        apply_slide(&mut self.grid, &plan);
                        applied += 1;
                        if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                        break 'evict;
                    }
                }
            }
        }
        for d in self.grid.seed_drivers() {
            match grow_step(&mut self.grid, &mut self.shadow, d) {
                Some(GrowStep::Placed(at)) | Some(GrowStep::Slid(at)) => {
                    self.events.push(Event::Grow { at });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                Some(GrowStep::Aborted) => {
                    self.events.push(Event::Abort { at: d });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                Some(GrowStep::Waiting(cell)) => { pumps.push(cell); }
                None => {}
            }
        }
        // squatted reservations pump wherever they stand
        for (c, _) in self.grid.reserved.clone() {
            if !self.grid.is_empty(c) { pumps.push(c); }
        }
        let positions: Vec<Pos> = self.grid.agents().map(|(p, _)| p).collect();
        for apos in &positions {
            let Some((npos, blockers)) = reel_blocked(&self.grid, *apos) else { continue };
            let mut cleared = false;
            for s in &blockers {
                if let Some(plan) = plan_slide_ex(&self.grid, npos, *s, &[], false, true) {
                    self.events.push(Event::Slide { at: plan.p });
                    apply_slide(&mut self.grid, &plan);
                    applied += 1;
                    cleared = true;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                    break;
                }
            }
            if cleared { continue; }
            let mut loosened = false;
            // Loosen the knot one level: the blocker could not slide because its own
            // endpoint cells are congested — slide a strand out of a SHARED endpoint cell
            // instead (never into npos, which must stay passable). Restricting to shared
            // cells keeps the excess-occupancy measure strictly decreasing, so loosening
            // cannot churn forever.
            'loosen: for s in &blockers {
                for c in [step(npos, s.a), step(npos, s.b)] {
                    let Some(wc) = self.grid.wire(c) else { continue };
                    if wc.count() < 2 { continue; }
                    for t in wc.iter().collect::<Vec<_>>() {
                        if let Some(plan) = plan_slide_ex(&self.grid, c, t, &[npos], false, true) {
                            self.events.push(Event::Slide { at: plan.p });
                            apply_slide(&mut self.grid, &plan);
                            applied += 1;
                            loosened = true;
                            if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                            break 'loosen;
                        }
                    }
                }
            }
            if loosened { continue; }
            // last resort before pumping: reroute MY OWN strand around the stack — the
            // walker's principal face is dynamic, so its wire can detour through open
            // space and the walker follows it (the shared cell loses my strand instead
            // of the blocker's)
            if let Some(a) = self.grid.agent(*apos) {
                if let Some(d) = a.faces[0] {
                    if let Some(mine) = self.grid.wire(npos).and_then(|w| w.with_he(d.opp())) {
                        if let Some(plan) = plan_slide_ex(&self.grid, npos, mine, &[], false, true) {
                            self.events.push(Event::Slide { at: plan.p });
                            apply_slide(&mut self.grid, &plan);
                            applied += 1;
                            if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                            continue;
                        }
                    }
                }
            }
            // a hopeless knot: nothing here can slide — this is exactly what pressure
            // exists for
            pumps.push(npos);
        }
        for p in positions {
            if let Some(plan) = plan_reel(&self.grid, p) {
                let sid = self.grid.agent(plan.apos).map(|a| a.sid).unwrap_or(0);
                self.events.push(Event::Reel { from: plan.apos, to: plan.npos, sid });
                apply_reel(&mut self.grid, &plan);
                applied += 1;
                if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
            } else if let Some(a) = self.grid.agent(p) {
                // demanded (hot adjacent strand) but unable to move: congestion around the
                // walker itself (aux re-anchoring failed or the shared cell would not
                // clear) — pump so χ evicts the hemming bystanders
                if a.tag.is_producer() && !a.nascent {
                    if let Some(d) = a.faces[0] {
                        let q = step(p, d);
                        if !self.grid.reserved.contains_key(&q) {
                            if let Some(w) = self.grid.wire(q) {
                                if let Some(m) = w.with_he(d.opp()) { if m.hot { pumps.push(p); } }
                            }
                        }
                    }
                }
            }
        }
        // TENSION: survey-guided bend-shifts — the bond's own upkeep, so it outranks the
        // anonymous field below and yields to everything demanded above. Every wire is
        // eligible, hot included: shortening the demanded wire is the most valuable
        // shortening there is, and the demand-licensed phases already ran this tick.
        for p in self.grid.wire_positions() {
            let Some(w) = self.grid.wire(p) else { continue };
            for s in w.iter().collect::<Vec<_>>() {
                if let Some(plan) = plan_flip(&self.grid, p, s) {
                    self.events.push(Event::Flip { from: plan.p, to: plan.npos });
                    apply_flip(&mut self.grid, &plan);
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                    break; // one shift per cell per tick
                }
            }
        }
        // SHOVE: χ-driven descent for parked agents (hot walkers and docked pairs immune)
        let positions: Vec<Pos> = if SHOVE_ON { self.grid.agents().map(|(p, _)| p).collect() } else { vec![] };
        for p in positions {
            if let Some(plan) = plan_shove(&self.grid, p) {
                self.events.push(Event::Shove { from: plan.apos, to: plan.npos });
                apply_shove(&mut self.grid, &plan);
                applied += 1;
                if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
            }
        }
        // pressure-demanded DECONGESTION: under χ, crowded wire cells thin themselves —
        // cold strands slide out into open space (hot strands are being walked: immune).
        // Strictly decongesting (empty trails only), so it cannot churn; this is what
        // dissolves the saturated pockets that block both walkers and slides. Under
        // STRONG pressure (χ ≥ CHI_EVAP) count-1 cells evaporate too, gated strictly
        // downhill in χ — without the gate the mat just circulates under the pump.
        if SHOVE_ON {
            for p in self.grid.wire_positions() {
                let here = self.grid.chi_at(p);
                if here < 1 { continue; }
                let Some(w) = self.grid.wire(p) else { continue };
                let crowded = w.count() >= 2;
                if !crowded && (w.count() != 1 || here < CHI_EVAP) { continue; }
                for s in w.iter().collect::<Vec<_>>() {
                    if s.hot { continue; }
                    if let Some(plan) = plan_slide(&self.grid, p, s, &[], false) {
                        if !crowded && plan.strands.iter().any(|(c, _)| self.grid.chi_at(*c) >= here) { continue; }
                        self.events.push(Event::Slide { at: plan.p });
                        apply_slide(&mut self.grid, &plan);
                        applied += 1;
                        if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                        break;
                    }
                }
            }
        }
        loop {
            let mut any = false;
            for p in self.grid.wire_positions() {
                if let Some(plan) = plan_retract(&self.grid, p) {
                    self.events.push(Event::Retract { a: plan.c1, b: plan.c2 });
                    apply_retract(&mut self.grid, &plan);
                    applied += 1;
                    any = true;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                    break; // the board changed: rescan
                }
            }
            if !any { break; }
        }
        self.grid.chi_step(&pumps);
        applied
    }
}

pub struct Outcome {
    pub done: bool,
    pub stuck: bool,
    pub ticks: u64,
    pub ints: u64,
    pub transport: u64,
    pub peak_agents: usize,
    pub result: Option<Term>,
}

/// Reduce a term on the lattice to quiescence (or a tick budget / a hard stall).
/// `done` means the SHADOW has no active pairs left, i.e. normal form reached; readback
/// works regardless of leftover wire slack. `stuck` means pairs remain and EITHER a full
/// tick applied nothing (the deterministic schedule can then never progress) OR no
/// shadow progress happened for `PROGRESS_DROUGHT` ticks (the field dynamics churn
/// without opening a fire) — the honest liveness residue, reported, never asserted away.
pub fn run_term(term: &Term, topo: Topo, max_ticks: u64, check: CheckLevel) -> Outcome {
    run_term_with(term, topo, FireMode::Search, max_ticks, check)
}

pub fn run_term_with(term: &Term, topo: Topo, mode: FireMode, max_ticks: u64, check: CheckLevel) -> Outcome {
    let mut sim = Sim::load(term, topo);
    sim.fire_mode = mode;
    let mut peak = sim.grid.agent_count();
    let mut ticks = 0;
    let mut zero_streak = 0u32;
    let mut last_ints = sim.shadow.ints;
    let mut last_progress = 0u64;
    let (done, stuck) = loop {
        if ticks >= max_ticks { break (false, false); }
        if sim.shadow.all_active_pairs().is_empty() && !sim.grid.has_seeds() { break (true, false); }
        let n = sim.tick(check);
        ticks += 1;
        peak = peak.max(sim.grid.agent_count());
        if check == CheckLevel::Tick && !sim.grid.has_seeds() { sim.grid.check_projection(&sim.shadow); }
        // χ needs a few quiet ticks to build a gradient before a shove can fire, so a
        // stall is only real after a grace window of zero-applied ticks
        if n == 0 { zero_streak += 1; if zero_streak > 48 { break (false, true); } }
        else { zero_streak = 0; }
        // Shadow ints is THE progress signal: a fire, atomic or at grow completion.
        // Docks, grow placements, and aborts are preparation that can cycle
        // (dock→place→abort→redock), so none of them may reset the window.
        if sim.shadow.ints > last_ints { last_ints = sim.shadow.ints; last_progress = ticks; }
        if ticks - last_progress > PROGRESS_DROUGHT { break (false, true); }
    };
    if (check != CheckLevel::Every || done) && !sim.grid.has_seeds() { sim.grid.check_projection(&sim.shadow); }
    let result = if done {
        let lattice_rb = sim.grid.readback();
        // The lattice and shadow readbacks must agree exactly; both feed the differential.
        let out_sid = sim.shadow.agents.iter().enumerate()
            .find(|(_, a)| a.as_ref().is_some_and(|a| a.tag == crate::rules::Tag::Out))
            .map(|(i, _)| i as u32).expect("out lives");
        let shadow_rb = sim.shadow.readback(sim.shadow.get(out_sid).ports[0]);
        assert_eq!(
            lattice_rb.as_ref().map(crate::oracle::show),
            shadow_rb.as_ref().map(crate::oracle::show),
            "lattice and shadow readbacks disagree"
        );
        lattice_rb
    } else { None };
    Outcome { done, stuck, ticks, ints: sim.shadow.ints, transport: sim.grid.transport, peak_agents: peak, result }
}
