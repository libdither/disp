//! The scheduler: rung 2 ships the sequential deterministic schedule, now over four
//! transitions (fire, reel, retract, kink-flip) and both topologies. Rung 3 adds parallel
//! and async-fuzz schedules over the SAME transitions — a schedule only ever chooses WHICH
//! enabled transitions run and in what order; it can never affect what a transition does
//! (footprints are the transition's own contract).

use crate::lattice::{embed, step, Grid, Pos, Topo};
use crate::net::Net;
use crate::oracle::Term;
use crate::transitions::{
    apply_fire, apply_grow_dock, apply_reel, apply_retract, apply_slide,
    grow_step, plan_fire, plan_fire_stamp, plan_grow_dock, plan_reel, plan_retract,
    plan_slide, reel_blocked, DockPlan, FirePlan, GrowStep,
};

/// Which fire planner the schedule uses. `Search` is the in-board backtracking planner;
/// `Stamp` is the precomputed-workshop pattern (purely local at fire time: freeness reads
/// only, waits when the pattern does not fit); `StampThenSearch` stamps when it fits and
/// falls back to the search when it does not.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FireMode { Search, Stamp, StampThenSearch, Grow, GrowThenSearch }

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
        for d in self.grid.seed_drivers() {
            match grow_step(&mut self.grid, &mut self.shadow, d) {
                Some(GrowStep::Placed(at)) | Some(GrowStep::Slid(at)) => {
                    self.events.push(Event::Grow { at });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                Some(GrowStep::Aborted) => {
                    self.events.push(Event::Grow { at: d });
                    applied += 1;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                }
                _ => {}
            }
        }
        let positions: Vec<Pos> = self.grid.agents().map(|(p, _)| p).collect();
        for apos in &positions {
            let Some((npos, blockers)) = reel_blocked(&self.grid, *apos) else { continue };
            let mut cleared = false;
            for s in &blockers {
                if let Some(plan) = plan_slide(&self.grid, npos, *s, &[], false) {
                    self.events.push(Event::Slide { at: plan.p });
                    apply_slide(&mut self.grid, &plan);
                    applied += 1;
                    cleared = true;
                    if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                    break;
                }
            }
            if cleared { continue; }
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
                        if let Some(plan) = plan_slide(&self.grid, c, t, &[npos], false) {
                            self.events.push(Event::Slide { at: plan.p });
                            apply_slide(&mut self.grid, &plan);
                            applied += 1;
                            if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
                            break 'loosen;
                        }
                    }
                }
            }
        }
        for p in positions {
            if let Some(plan) = plan_reel(&self.grid, p) {
                let sid = self.grid.agent(plan.apos).map(|a| a.sid).unwrap_or(0);
                self.events.push(Event::Reel { from: plan.apos, to: plan.npos, sid });
                apply_reel(&mut self.grid, &plan);
                applied += 1;
                if check == CheckLevel::Every && !self.grid.has_seeds() { self.grid.check_projection(&self.shadow); }
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
/// works regardless of leftover wire slack. `stuck` means a full tick applied nothing
/// while pairs remain — the deterministic schedule can then never progress (the honest
/// liveness residue, reported, never asserted away).
pub fn run_term(term: &Term, topo: Topo, max_ticks: u64, check: CheckLevel) -> Outcome {
    run_term_with(term, topo, FireMode::Search, max_ticks, check)
}

pub fn run_term_with(term: &Term, topo: Topo, mode: FireMode, max_ticks: u64, check: CheckLevel) -> Outcome {
    let mut sim = Sim::load(term, topo);
    sim.fire_mode = mode;
    let mut peak = sim.grid.agent_count();
    let mut ticks = 0;
    let (done, stuck) = loop {
        if ticks >= max_ticks { break (false, false); }
        if sim.shadow.all_active_pairs().is_empty() && !sim.grid.has_seeds() { break (true, false); }
        let n = sim.tick(check);
        ticks += 1;
        peak = peak.max(sim.grid.agent_count());
        if check == CheckLevel::Tick && !sim.grid.has_seeds() { sim.grid.check_projection(&sim.shadow); }
        if n == 0 { break (false, true); }
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
