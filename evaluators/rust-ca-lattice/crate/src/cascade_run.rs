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
    pub fn tick_traced(&mut self) -> Option<Pos> {
        let p = self.next_pos()?;
        self.queued.remove(&p);
        if self.gen_left == 0 {
            self.generation += 1;
            self.gen_left = self.queue.len() + 1;
        }
        self.gen_left -= 1;
        self.activate(p);
        Some(p)
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

    // ------------------------------------------------------------ transitions

    fn activate(&mut self, p: Pos) {
        let site = self.grid.site(p);
        if site.claim {
            return;
        }
        // A cell can host a walker and a builder cursor at once. The agent acts first:
        // its departure is often exactly what unblocks the cursor's next placement.
        match &site.cell {
            Cell::Agent { nursery: false, cooldown: 0, tag, .. } if tag.is_producer() => {
                if self.try_dock(p, &site) || self.try_walk(p, &site) || self.try_swap(p, &site) {
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
        if self.heat(p) {
            return;
        }
        // A guest bridges its passthrough wires: a demand wave arriving on one side must
        // wake the other or it dies at the crossing (heat sees through guests, so wakes
        // must too). The relay is directional and stops once both sides agree, which
        // keeps adjacent guests from waking each other forever.
        let pass: Vec<Route> = match &site.cell {
            Cell::Agent { pass, .. } => pass.clone(),
            Cell::Seed { pass, .. } => pass.iter().copied().collect(),
            _ => vec![],
        };
        for r in pass {
            let read = |q: Pos| self.grid.word(q);
            let a = (step(p, r.a.face), EndPt { face: r.a.face.opp(), lane: r.a.lane });
            let b = (step(p, r.b.face), EndPt { face: r.b.face.opp(), lane: r.b.lane });
            let a_hot = hot_beyond(&read, a.0, a.1);
            let b_hot = hot_beyond(&read, b.0, b.1);
            if a_hot && !b_hot {
                self.wake(b.0);
            }
            if b_hot && !a_hot {
                self.wake(a.0);
            }
        }
        self.relax_chi(p);
    }

    /// Demand propagation: a route heats when either end meets a live consumer's
    /// principal or the hot continuation of the same wire, seen through any guests
    /// squatting on it. The wave travels one cell per generation from consumers toward
    /// producers; only hot wires are walked, so undemanded values stay parked and never
    /// clog the fabric. Single-cell, stale-safe.
    fn heat(&mut self, p: Pos) -> bool {
        let site = self.grid.site(p);
        let Cell::Wire { routes, hot, cooldown, reserved } = &site.cell else {
            return false;
        };
        let mut new_hot = *hot;
        for (i, r) in routes.iter().enumerate() {
            if (new_hot >> i) & 1 == 1 {
                continue;
            }
            for e in r.ends() {
                let n = step(p, e.face);
                let back = EndPt { face: e.face.opp(), lane: e.lane };
                let heats = {
                    let read = |q: Pos| self.grid.word(q);
                    hot_beyond(&read, n, back)
                };
                if heats {
                    new_hot |= 1 << i;
                    break;
                }
            }
        }
        if new_hot == *hot {
            return false;
        }
        let cell = Cell::Wire {
            routes: routes.clone(),
            hot: new_hot,
            cooldown: *cooldown,
            reserved: *reserved,
        };
        self.grid.set(p, &Site { cell, cursor: site.cursor, chi: site.chi, claim: site.claim });
        self.wake_around(p);
        true
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
        let Cell::Agent { tag, principal, aux, pass, .. } = &site.cell else {
            return false;
        };
        let m = principal.face;
        let t = step(p, m);
        let target = self.grid.site(t);
        if target.claim {
            return false;
        }
        let Cell::Wire { routes, hot: whot, reserved: None, .. } = &target.cell else {
            return false; // blocked; the target's next change wakes this cell
        };
        let enter = EndPt { face: m.opp(), lane: principal.lane };
        let Some(my_index) = routes.iter().position(|r| r.through(enter).is_some()) else {
            return false;
        };
        let exit = routes[my_index].through(enter).unwrap();
        // Demand-gated motion: only walk a wire the consumer side has heated.
        let route_hot = (whot >> my_index) & 1 == 1;
        let downhill = site.chi >= 4 && target.chi.saturating_add(2) <= site.chi;
        if !(route_hot || downhill) {
            return false;
        }
        if exit.face == enter.face {
            // A one-cell hairpin: the wire U-turns and re-enters this agent's own cell
            // as a passthrough. Collapse it: the principal re-anchors onto that pass
            // route's far end, and both slack segments vanish. Two cells, one commit.
            let back = EndPt { face: m, lane: exit.lane };
            let Some(pi) = pass.iter().position(|r| r.through(back).is_some()) else {
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
            return false;
        }
        let detours: Vec<usize> = (0..need).filter(|k| plans[*k] == Plan::Detour).collect();
        if detours.len() > 1 {
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
        // A seed inherits at most one passthrough route.
        if pass.len() > 1 || cpass.len() > 1 {
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
        // Roll preference ladder, most to least informed (all stale-safe heuristics):
        // whole footprint merges cleanly, then the gate ring merges, then the gate ring
        // is merely unclaimed wire or empty (growth waits per cell and evicts).
        let candidate = |deep: bool, ring_merge: bool| {
            (0..4u8).find(|roll| {
                let (sc, sp) = mk_seeds(*roll);
                self.roll_fits(t, p, stub_cells, rule, axis, *roll, ring_merge)
                    && crate::blocklet::finals_fit(rule, axis, *roll, &sc, &sp)
                    && (!deep || self.roll_merges_deep(t, rule, axis, *roll))
            })
        };
        let roll = candidate(true, true)
            .or_else(|| candidate(false, true))
            .or_else(|| candidate(false, false));
        let Some(roll) = roll else {
            // Every roll's first ring is blocked. The blocking cells are adjacent to the
            // pair, so their next change wakes it; wait silently until then.
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
            self.observe_fire(t, p, rule, axis, roll, consumer_sid, producer_sid);
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

    /// Whether every footprint cell of this roll currently either is free or merges with
    /// the planned matter. Stale-safe heuristic only: cells change after the dock, and
    /// growth re-validates at each placement.
    fn roll_merges_deep(&self, seed_c: Pos, rule: u8, axis: Dir, roll: u8) -> bool {
        let layout = crate::blocklet::layout(rule);
        for (off, cell) in &layout.extras {
            let world = add(seed_c, crate::cascade::rot_pos(*off, axis, roll));
            let ws = self.grid.site(world);
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
    fn roll_fits(
        &self,
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
            if !self.grid.topo.in_bounds(world) {
                return false;
            }
            if stub_cells.iter().flatten().any(|s| *s == world) {
                return false;
            }
            let near = DIRS.iter().any(|d| step(world, *d) == seed_c || step(world, *d) == seed_p);
            if !near {
                continue;
            }
            let ws = self.grid.site(world);
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

    fn observe_fire(
        &mut self,
        seed_c: Pos,
        _seed_p: Pos,
        rule: u8,
        axis: Dir,
        roll: u8,
        consumer_sid: Option<u32>,
        producer_sid: Option<u32>,
    ) {
        self.grid.rewrites += 1;
        self.events.push(Event::Fire(seed_c, rule));
        if let (Some(cs), Some(ps)) = (consumer_sid, producer_sid) {
            let (r, fresh) = self.shadow.fire(cs, ps);
            assert!(std::ptr::eq(r, &RULES[rule as usize]));
            let layout = crate::blocklet::layout(rule);
            assert_eq!(fresh.len(), layout.seats.len());
            for (fresh_idx, off) in &layout.seats {
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
                if target.claim || target.cursor.is_some() {
                    return; // wait silently; the target's next change wakes this cell
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
                        let Some(merged) = merge_matter(planned.clone(), &existing) else {
                            // The merge cannot fit. Evict one cold occupying route out of
                            // the way (the relief rung); if nothing is evictable, release
                            // the reservation and wait for the occupant to change.
                            if std::env::var_os("CASCADE_DBG").is_some() {
                                eprintln!("merge-fail at {t:?}: planned {planned:?} vs {:?}", target.cell);
                            }
                            if self.try_evict(t, &planned) {
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
                assert!(target.cursor.is_none(), "two cursors in one blocklet");
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
    /// touched, and only when removing them lets the planned matter merge. The whole
    /// rewrite commits in one serial activation; the parallel driver claims the same set
    /// in address order. The blocked cell keeps its reservation so the cursor's retry
    /// finds it still protected.
    fn try_evict(&mut self, t: Pos, planned: &Cell) -> bool {
        let target = self.grid.site(t);
        let Cell::Wire { routes, hot, reserved, .. } = &target.cell else {
            return false;
        };
        let reserved = *reserved;
        let plain = |s: &Site| {
            matches!(s.cell, Cell::Wire { reserved: None, .. }) && s.cursor.is_none() && !s.claim
        };
        let side_ok = |s: &Site| match &s.cell {
            Cell::Empty { reserved: None } => s.cursor.is_none() && !s.claim,
            Cell::Wire { reserved: None, routes, .. } => {
                routes.len() < 3 && s.cursor.is_none() && !s.claim
            }
            _ => false,
        };
        let side_add = |s: &Site, route: Route| -> Site {
            let mut ns = s.clone();
            match &mut ns.cell {
                Cell::Empty { .. } => {
                    ns.cell = Cell::Wire { routes: vec![route], hot: 0, cooldown: 0, reserved: None };
                }
                Cell::Wire { routes, .. } => routes.push(route),
                _ => unreachable!(),
            }
            ns
        };
        let swing = |s: &Site, idx: usize, from: EndPt, to: EndPt| -> Site {
            let mut ns = s.clone();
            if let Cell::Wire { routes, .. } = &mut ns.cell {
                let far = routes[idx].through(from).expect("continuation endpoint");
                routes[idx] = Route::new(far, to);
            }
            ns
        };

        // Prefer an eviction that unblocks the merge outright; when the cell is more than
        // one route over budget, evict any evictable cold route (progress one at a time).
        let order: Vec<(usize, bool)> = {
            let unblocking = |i: usize| {
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
        for (i, _unblocks) in order {
            let r = &routes[i];
            if (hot >> i) & 1 == 1 {
                if std::env::var_os("CASCADE_DBG").is_some() { eprintln!("evict {t:?} r{i}: hot"); } continue;
            }
            let (d1, d2) = (r.a.face, r.b.face);
            let (n1, n2) = (step(t, d1), step(t, d2));
            let (n1s, n2s) = (self.grid.site(n1), self.grid.site(n2));
            let dbg = std::env::var_os("CASCADE_DBG").is_some();
            if !plain(&n1s) || !plain(&n2s) {
                if dbg { eprintln!("evict {t:?} r{i}: neighbor not plain wire"); }
                continue;
            }
            let (Cell::Wire { routes: r1s, hot: h1, .. }, Cell::Wire { routes: r2s, hot: h2, .. }) =
                (&n1s.cell, &n2s.cell)
            else {
                continue;
            };
            let back1 = EndPt { face: d1.opp(), lane: r.a.lane };
            let back2 = EndPt { face: d2.opp(), lane: r.b.lane };
            let Some(i1) = r1s.iter().position(|x| x.ends().contains(&back1)) else {
                if dbg { eprintln!("evict {t:?} r{i}: no continuation n1"); }
                continue;
            };
            let Some(i2) = r2s.iter().position(|x| x.ends().contains(&back2)) else {
                if dbg { eprintln!("evict {t:?} r{i}: no continuation n2"); }
                continue;
            };
            if (h1 >> i1) & 1 == 1 || (h2 >> i2) & 1 == 1 {
                if dbg { eprintln!("evict {t:?} r{i}: hot continuation"); }
                continue;
            }
            // The blocked cell after removal, reservation intact.
            let t_new = {
                let mut ns = target.clone();
                let mut emptied = false;
                if let Cell::Wire { routes, hot, .. } = &mut ns.cell {
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
                if emptied {
                    ns.cell = Cell::Empty { reserved };
                }
                ns
            };

            if d2 != d1.opp() {
                // Bent segment: corner-cut through the diagonal cell.
                let u = step(n1, d2);
                let us = self.grid.site(u);
                if side_ok(&us) {
                    for l1 in 0..2u8 {
                        for l2 in 0..2u8 {
                            let n1n = swing(&n1s, i1, back1, EndPt { face: d2, lane: l1 });
                            let n2n = swing(&n2s, i2, back2, EndPt { face: d1, lane: l2 });
                            let un = side_add(&us, Route::new(
                                EndPt { face: d2.opp(), lane: l1 },
                                EndPt { face: d1.opp(), lane: l2 },
                            ));
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
                }
                // Fallback: the diagonal is taken, so bracket the bend out of plane
                // through the two faces perpendicular to both bend directions.
                for w in DIRS {
                    if w == d1 || w == d1.opp() || w == d2 || w == d2.opp() {
                        continue;
                    }
                    let (a, c, b) = (step(n1, w), step(u, w), step(n2, w));
                    let (a_s, c_s, b_s) = (self.grid.site(a), self.grid.site(c), self.grid.site(b));
                    if !side_ok(&a_s) || !side_ok(&c_s) || !side_ok(&b_s) {
                        continue;
                    }
                    'wlanes: for lanes in 0..16u8 {
                        let (l1, l2, l3, l4) =
                            (lanes & 1, (lanes >> 1) & 1, (lanes >> 2) & 1, (lanes >> 3) & 1);
                        let n1n = swing(&n1s, i1, back1, EndPt { face: w, lane: l1 });
                        let n2n = swing(&n2s, i2, back2, EndPt { face: w, lane: l4 });
                        let an = side_add(&a_s, Route::new(
                            EndPt { face: w.opp(), lane: l1 },
                            EndPt { face: d2, lane: l2 },
                        ));
                        let cn = side_add(&c_s, Route::new(
                            EndPt { face: d2.opp(), lane: l2 },
                            EndPt { face: d1.opp(), lane: l3 },
                        ));
                        let bn = side_add(&b_s, Route::new(
                            EndPt { face: d1, lane: l3 },
                            EndPt { face: w.opp(), lane: l4 },
                        ));
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
                    let (u1, u2, u3) = (step(n1, q), step(t, q), step(n2, q));
                    let (u1s, u2s, u3s) =
                        (self.grid.site(u1), self.grid.site(u2), self.grid.site(u3));
                    if !side_ok(&u1s) || !side_ok(&u2s) || !side_ok(&u3s) {
                        continue;
                    }
                    'lanes: for lanes in 0..16u8 {
                        let (l1, l2, l3, l4) =
                            (lanes & 1, (lanes >> 1) & 1, (lanes >> 2) & 1, (lanes >> 3) & 1);
                        let n1n = swing(&n1s, i1, back1, EndPt { face: q, lane: l1 });
                        let n2n = swing(&n2s, i2, back2, EndPt { face: q, lane: l4 });
                        let u1n = side_add(&u1s, Route::new(
                            EndPt { face: q.opp(), lane: l1 },
                            EndPt { face: d1.opp(), lane: l2 },
                        ));
                        let u2n = side_add(&u2s, Route::new(
                            EndPt { face: d1, lane: l2 },
                            EndPt { face: d1.opp(), lane: l3 },
                        ));
                        let u3n = side_add(&u3s, Route::new(
                            EndPt { face: d1, lane: l3 },
                            EndPt { face: q.opp(), lane: l4 },
                        ));
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
                cooldown: 1,
            };
            let producer = Cell::Agent {
                tag: r.producer,
                principal: EndPt { face: partner.opp(), lane: plane },
                aux: pstub,
                pass: pseed_pass.into_iter().collect(),
                nursery: false,
                cooldown: 1,
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
            t,
            rule,
            cursor.axis,
            cursor.roll,
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

/// Whether demand exists just beyond a wire endpoint: a live consumer's principal, or a
/// hot continuation of the same wire, looked at through any chain of guests (or a seed
/// passthrough) squatting on it. Stale-safe read; the hop bound limits how far one
/// activation sees, not correctness.
pub(crate) fn hot_beyond(
    read: &dyn Fn(Pos) -> crate::cascade::Word2,
    mut n: Pos,
    mut back: EndPt,
) -> bool {
    for _ in 0..5 {
        let Ok(ns) = read(n).unpack() else { return false };
        match &ns.cell {
            Cell::Agent { tag, principal, nursery: false, pass, .. } => {
                if tag.is_consumer() && *principal == back {
                    return true;
                }
                let Some(exit) = pass.iter().find_map(|r| r.through(back)) else {
                    return false;
                };
                n = step(n, exit.face);
                back = EndPt { face: exit.face.opp(), lane: exit.lane };
            }
            Cell::Seed { pass, .. } => {
                let Some(exit) = pass.and_then(|r| r.through(back)) else { return false };
                n = step(n, exit.face);
                back = EndPt { face: exit.face.opp(), lane: exit.lane };
            }
            Cell::Wire { routes, hot, .. } => {
                return routes
                    .iter()
                    .enumerate()
                    .any(|(j, r2)| r2.ends().contains(&back) && (hot >> j) & 1 == 1);
            }
            _ => return false,
        }
    }
    false
}

/// Merge planned blocklet matter with the routes already occupying the target cell (the
/// guest principle for growth). None when the merge does not fit or collides.
fn merge_matter(planned: Cell, existing: &[Route]) -> Option<Cell> {
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
    use Dir::*;
    let mut grid = Grid2::new(Topo::Full3D);
    let mut shadow = Net::new();
    let wire = |grid: &mut Grid2, p: Pos, routes: &[((Dir, u8), (Dir, u8))]| {
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
        &mut grid, &mut shadow, (0, 0, 0), rule.consumer, E,
        (rule.consumer.arity() >= 2).then_some(W),
    );
    let p = place_agent(
        &mut grid, &mut shadow, (px, 0, 0), rule.producer, W,
        (rule.producer.arity() >= 2).then_some(E),
    );
    shadow.link(c.id, 0, p.id, 0);
    if bend {
        lay_wire(&mut grid, &[(0, 0, 0), (1, 0, 0), (1, 1, 0), (2, 1, 0), (2, 0, 0), (3, 0, 0)]);
    } else if gap > 0 {
        let path: Vec<Pos> = (0..=px).map(|x| (x, 0, 0)).collect();
        lay_wire(&mut grid, &path);
    }

    match rule.consumer.arity() {
        1 => {}
        2 => {
            wire(&mut grid, (-1, 0, 0), &[((E, 0), (W, 0))]);
            let o = place_agent(&mut grid, &mut shadow, (-2, 0, 0), Tag::Out, E, None);
            shadow.link(c.id, 1, o.id, 0);
        }
        _ => {
            wire(&mut grid, (-1, 0, 0), &[((E, 0), (W, 0)), ((E, 1), (W, 1))]);
            wire(&mut grid, (-2, 0, 0), &[((E, 0), (N, 0)), ((E, 1), (S, 0))]);
            wire(&mut grid, (-2, -1, 0), &[((S, 0), (N, 0))]);
            wire(&mut grid, (-2, 1, 0), &[((N, 0), (S, 0))]);
            let o1 = place_agent(&mut grid, &mut shadow, (-2, -2, 0), Tag::Out, S, None);
            let o2 = place_agent(&mut grid, &mut shadow, (-2, 2, 0), Tag::Out, N, None);
            shadow.link(c.id, 1, o1.id, 0);
            shadow.link(c.id, 2, o2.id, 0);
        }
    }
    match rule.producer.arity() {
        1 => {}
        2 => {
            wire(&mut grid, (px + 1, 0, 0), &[((W, 0), (E, 0))]);
            let o = place_agent(&mut grid, &mut shadow, (px + 2, 0, 0), Tag::Out, W, None);
            shadow.link(p.id, 1, o.id, 0);
        }
        _ => {
            // Both producer stubs head east then split south and further east, keeping
            // the north half-space clear for the roll-0 blocklet.
            wire(&mut grid, (px + 1, 0, 0), &[((W, 0), (E, 0)), ((W, 1), (E, 1))]);
            wire(&mut grid, (px + 2, 0, 0), &[((W, 0), (E, 0)), ((W, 1), (S, 0))]);
            wire(&mut grid, (px + 2, 1, 0), &[((N, 0), (S, 0))]);
            wire(&mut grid, (px + 3, 0, 0), &[((W, 0), (E, 0))]);
            let o1 = place_agent(&mut grid, &mut shadow, (px + 4, 0, 0), Tag::Out, W, None);
            let o2 = place_agent(&mut grid, &mut shadow, (px + 2, 2, 0), Tag::Out, N, None);
            shadow.link(p.id, 1, o1.id, 0);
            shadow.link(p.id, 2, o2.id, 0);
        }
    }
    check_reciprocity(&grid).expect("fixture reciprocity");
    check_projection(&grid, &shadow).expect("fixture projection");
    (grid, shadow)
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
