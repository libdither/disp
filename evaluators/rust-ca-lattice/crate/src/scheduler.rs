//! Deterministic sequential scheduler combining strict seven-site kernels with declared
//! host-planned bounded-footprint transitions. Each tick advances ψ, the tension survey,
//! local star/bulge protocols, host-planned fire/grow and demanded reel, χ-licensed
//! occupant agent-step/reshape, and retract, then performs χ/σ Jacobi sweeps. Requesters only emit
//! pressure; they never select or displace a foreign payload. The host-planned phases are
//! explicitly nonlocal and remain outside the `next_site` cellular rule.

use crate::lattice::{embed, step, Grid, Pos, Topo};
use crate::local::advance_local_motion;
use crate::net::Net;
use crate::oracle::Term;
use crate::transitions::{
    apply_agent_step, apply_fire, apply_flip, apply_grow_dock, apply_reel, apply_retract,
    apply_slide, grow_step, plan_chi_flip, plan_fire, plan_fire_stamp,
    plan_agent_step, plan_flip, plan_grow_dock, plan_reel, plan_retract, plan_slide,
    reel_blocked, DockPlan, FirePlan, GrowStep,
};

/// Which host fire planner gets the immediate attempt. `Search` performs in-board
/// backtracking; `Stamp` checks one precomputed bounded footprint; `StampThenSearch`
/// tries that footprint before the search. Every mode retains the same pressure-only
/// blocked-pair response and its damped grow fallback after a prolonged wait.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FireMode { Search, Stamp, StampThenSearch, Grow, GrowThenSearch }

/// Configuration switch for host-planned, χ-licensed occupant motion: agent step,
/// χ-flip, and slide.
pub const AGENT_STEP_ON: bool = true;

/// Standing `σ` level at each live consumer. Its local shell parks calm stars near a
/// reaction room without authorizing pressure motion; explicit χ pumps remain separate.
pub const STANDING_SHELL: u8 = 60;


/// Stall window on semantic shadow progress. Only a completed atomic fire or grow commits
/// an interaction; docks, placements, aborts, field motion, and detour reels may cycle
/// without changing the shadow. The window admits long transport-only stretches on deep
/// chains while still detecting that drought.
pub const PROGRESS_DROUGHT: u64 = 2_000;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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
    Slide { at: Pos, why: &'static str },
    Dock { cpos: Pos, ppos: Pos, rule: (&'static str, &'static str) },
    Grow { at: Pos },
    Abort { at: Pos },
    AgentStep { from: Pos, to: Pos },
    Flip { from: Pos, to: Pos, hot: bool },
    Approach { from: Pos, to: Pos },
    /// Observer-only rising edge for a strict seven-site, center-emitting pressure source.
    LocalPressure { at: Pos, level: u8 },
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
            // Stamp first, then bounded search, then dock-and-grow when neither atomic
            // layout fits.
            FireMode::GrowThenSearch => plan_fire_stamp(&self.grid, p).map(Atomic)
                .or_else(|| plan_fire(&self.grid, p).map(Atomic))
                .or_else(|| plan_grow_dock(&self.grid, p).map(Dock)),
        }
    }

    /// One mixed sequential tick. Local kernels read frozen center-plus-six-neighbor
    /// snapshots; host-planned bounded-footprint transitions execute in deterministic
    /// coordinate order.
    /// Blocked actors add χ sources only, and every clearance transition is initiated by
    /// the pressured occupant.  Returns the number of control or payload transitions.
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
        // Strict-local star translation and pressure bulging. Every site reads one
        // frozen seven-cell view and writes only itself; handshake ticks leave payload
        // geometry untouched, and the final center writes commit synchronously. A star
        // consumes its principal forwarder; auxiliary rerouting changes total length by
        // zero or ±1 according to arity/mode. A pressured switchbox may add two units of
        // slack to move one of its own strands aside.
        let local = advance_local_motion(&mut self.grid);
        applied += local.changes;
        for &(at, level) in &local.pressure_sources {
            if self.grid.chi_at(at) < level {
                self.events.push(Event::LocalPressure { at, level });
            }
        }
        for (from, to) in local.approaches {
            self.events.push(Event::Approach { from, to });
        }
        for at in local.bulges {
            self.events.push(Event::Slide { at, why: "bulge-local" });
        }
        if check == CheckLevel::Every && !self.grid.has_seeds() {
            self.grid.check_projection(&self.shadow);
        }
        // χ sources collected as the tick runs; the field updates at the end.
        let mut pumps = local.pressure_sources;
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
        // An unfireable adjacent pair may request room, but it may not select or move a
        // bystander.  It raises χ at its own two cells; each obstructing payload responds
        // later through its own pressure transition. This ownership boundary excludes
        // board scans that select a foreign wire for displacement.
        for (cpos, a) in self.grid.agents().map(|(p, a)| (p, a.clone())).collect::<Vec<_>>() {
            if !a.tag.is_consumer() || a.nascent { continue; }
            let Some(d) = a.faces[0] else { continue };
            let ppos = step(cpos, d);
            let Some(b) = self.grid.agent(ppos) else { continue };
            if !b.tag.is_producer() || b.nascent || b.faces[0] != Some(d.opp()) { continue; }
            if self.plan_fire_mode(cpos).is_some() { continue; }
            // Age only controls damped dock retries.  It grants no displacement right.
            let age = {
                if let Some(crate::lattice::Cell::Agent(a)) = self.grid.cells.get_mut(&cpos) {
                    a.frustration = a.frustration.saturating_add(1);
                    a.frustration
                } else { 0 }
            };
            pumps.push((cpos, 250));
            pumps.push((ppos, 250));
            if age >= 32 && age % 64 == 32 {
                if let Some(plan) = plan_grow_dock(&self.grid, cpos) {
                    self.events.push(Event::Dock {
                        cpos: plan.cpos, ppos: plan.ppos,
                        rule: (plan.rule.consumer.name(), plan.rule.producer.name()),
                    });
                    apply_grow_dock(&mut self.grid, plan);
                    applied += 1;
                    continue;
                }
            }
        }
        for d in self.grid.seed_drivers() {
            match grow_step(&mut self.grid, &mut self.shadow, d) {
                Some(GrowStep::Placed(at)) => {
                    self.events.push(Event::Grow { at });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                Some(GrowStep::Aborted) => {
                    self.events.push(Event::Abort { at: d });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                Some(GrowStep::Waiting(cell)) => { pumps.push((cell, 250)); }
                None => {}
            }
        }
        // The claimed room remains a pressure source until the seed completes or aborts.
        // This keeps every occupied reservation responsive during the multi-tick unfold;
        // the field grants no requester a direct write to that payload.
        for (c, _) in self.grid.reserved.clone() {
            if !self.grid.is_empty(c) { pumps.push((c, 250)); }
        }
        let positions: Vec<Pos> = self.grid.agents().map(|(p, _)| p).collect();
        for apos in &positions {
            // A blocked walker likewise emits only pressure.  In particular it never
            // chooses a blocker strand, a shared endpoint, or even an alternate route for
            // the blocker.  The target cell's occupants own every subsequent move.
            if let Some(npos) = reel_blocked(&self.grid, *apos) { pumps.push((npos, 250)); }
        }
        for p in positions {
            if let Some(plan) = plan_reel(&self.grid, p) {
                let sid = self.grid.agent(plan.apos).map(|a| a.sid).unwrap_or(0);
                self.events.push(Event::Reel { from: plan.apos, to: plan.npos, sid });
                apply_reel(&mut self.grid, &plan);
                applied += 1;
                if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
            } else {
                // demanded (hot adjacent strand) but unable to move: congestion around
                // the walker itself (aux re-anchoring failed or the shared cell would
                // not clear) — pump so each hemming occupant can yield under its own χ
                if let Some(a) = self.grid.agent(p) {
                    if a.tag.is_producer() && !a.nascent {
                        if let Some(d) = a.faces[0] {
                            let q = step(p, d);
                            if !self.grid.reserved.contains_key(&q) {
                                if let Some(w) = self.grid.wire(q) {
                                    if let Some(m) = w.with_he(d.opp()) { if m.hot { pumps.push((p, 250)); } }
                                }
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
                    self.events.push(Event::Flip { from: plan.p, to: plan.npos, hot: plan.s.hot });
                    apply_flip(&mut self.grid, &plan);
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                    break; // one shift per cell per tick
                }
            }
        }
        // AGENT STEP: χ-licensed occupant descent for parked agents (hot walkers and docked pairs immune)
        let positions: Vec<Pos> = if AGENT_STEP_ON { self.grid.agents().map(|(p, _)| p).collect() } else { vec![] };
        for p in positions {
            if let Some(plan) = plan_agent_step(&self.grid, p) {
                self.events.push(Event::AgentStep { from: plan.apos, to: plan.npos });
                apply_agent_step(&mut self.grid, &plan);
                applied += 1;
                if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
            }
        }
        // χ-licensed, occupant-owned DECONGESTION and EVAPORATION: in frustration halos, crowded
        // wire cells thin themselves and count-1 cells slide one of their own strands
        // through strictly-downhill empty trails.  Hot strands are eligible because the
        // occupied cell—not the requester—initiates the move. Jurisdiction is by FIELD,
        // not by value: χ carries frustration only (spreaders act on it, tension keeps
        // out of it), while σ carries standing shells and is read only by approach.
        // Separate fields keep reaction-zone parking from authorizing clearance moves.
        if AGENT_STEP_ON {
            for p in self.grid.wire_positions() {
                let here = self.grid.chi_at(p);
                if here < 1 { continue; }
                let Some(w) = self.grid.wire(p) else { continue };
                let crowded = w.count() >= 2;
                let strands: Vec<_> = w.iter().collect();
                let mut moved = false;
                // pressure micro-flip first: a bend chooses a one-cell downhill hop,
                // supplying re-tie space for the host-planned agent step
                for s in strands.iter().copied() {
                    if let Some(plan) = plan_chi_flip(&self.grid, p, s) {
                        self.events.push(Event::Flip { from: plan.p, to: plan.npos, hot: plan.s.hot });
                        apply_flip(&mut self.grid, &plan);
                        applied += 1;
                        moved = true;
                        if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                        break;
                    }
                }
                if moved { continue; }
                // count-1 evaporation engages under STRONG pressure only (crowding
                // decongests from χ ≥ 1)
                if !crowded && (strands.len() != 1 || here < 3) { continue; }
                for s in strands {
                    if s.cooldown > 0 { continue; }
                    if let Some(plan) = plan_slide(&self.grid, p, s, &[]) {
                        if !crowded && plan.strands.iter().any(|(c, _)| self.grid.chi_at(*c) >= here) { continue; }
                        self.events.push(Event::Slide { at: plan.p, why: "decongest" });
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
        // the standing shells (σ, a separate field): every live consumer marks its
        // reaction room, and approaching values park at the rim
        let shells: Vec<(Pos, u8)> = self.grid.agents()
            .filter(|(_, a)| a.tag.is_consumer() && !a.nascent)
            .map(|(p, _)| (p, STANDING_SHELL)).collect();
        self.grid.sigma_step(&shells);
        self.grid.chi_step(&pumps);
        self.grid.cooldown_step();
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
/// works regardless of leftover wire slack. `stuck` means pairs remain and EITHER 49
/// consecutive ticks applied nothing OR no shadow progress happened for
/// `PROGRESS_DROUGHT` ticks (the field dynamics churn without opening a fire) — the honest
/// liveness residue, reported, never asserted away.
pub fn run_term(term: &Term, topo: Topo, max_ticks: u64, check: CheckLevel) -> Outcome {
    run_term_with(term, topo, FireMode::Search, max_ticks, check)
}

pub fn run_term_with(term: &Term, topo: Topo, mode: FireMode, max_ticks: u64, check: CheckLevel) -> Outcome {
    run_term_opts(term, topo, mode, max_ticks, check, PROGRESS_DROUGHT)
}

/// `run_term_with` with an explicit drought window. The suite always uses
/// `PROGRESS_DROUGHT`; iteration probes may shorten it to trade a little stall-detection
/// patience for wall-clock (a stuck term burns the whole window before it is called).
pub fn run_term_opts(term: &Term, topo: Topo, mode: FireMode, max_ticks: u64, check: CheckLevel, drought: u64) -> Outcome {
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
        // χ needs a few quiet ticks to build a gradient before an agent step can fire, so a
        // stall is only real after a grace window of zero-applied ticks
        if n == 0 { zero_streak += 1; if zero_streak > 48 { break (false, true); } }
        else { zero_streak = 0; }
        // Shadow ints is THE progress signal: a fire, atomic or at grow completion.
        // Docks, grow placements, and aborts are preparation that can cycle
        // (dock→place→abort→redock), so none of them may reset the window.
        if sim.shadow.ints > last_ints { last_ints = sim.shadow.ints; last_progress = ticks; }
        if ticks - last_progress > drought { break (false, true); }
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
