//! Rung 2 dynamics: the footprint-atomic transitions.
//!
//! Every transition is two phases with one contract:
//!   `plan_*`  — reads ONLY cells inside a bounded neighborhood, decides feasibility, and
//!               returns a Plan carrying its exact FOOTPRINT (the cell set it may write).
//!   `apply_*` — executes the plan; every mutation is asserted to stay inside the footprint.
//!
//! This is the machine-checked half of the rung-3 ordering-resilience story: transitions
//! with disjoint footprints commute because they factorize the state; a scheduler then
//! only ever varies ORDER. The semantic half is strong confluence (tc-net.typ Thm 2)
//! carried by the projection invariant, asserted per transition in checked runs.
//!
//! FIRE (rule 1): an adjacent, mutually-facing consumer·producer pair rewrites by the ROM
//! template. The two dying cells become SPLICE HUBS: an external aux wire either lands
//! directly on the fresh agent that takes over the cell (half-edge-aligned, zero rewiring)
//! or continues through the vacated cell as a strand. The created-short lemma is what
//! makes this bounded: no template wires outside the pair + O(1) fresh agents, so the
//! 12-cell neighborhood suffices or the fire WAITS (space is a liveness concern, never a
//! soundness one).
//!
//! REEL (rule 2, Walk): a producer steps one cell along its principal wire, consuming one
//! strand — and ONLY when the wire's far end is a consumer's principal port, i.e. a fire
//! awaits (§5.2 "whose far end is demanded"). A value whose principal ends at a parent's
//! aux port stays parked: walking there wins nothing, and such walkers were measured to
//! squat on crossings and deadlock the crossed wire's owner. ε never walks; its victim
//! walks into it (§9's parked death pulse). Producer-only walking toward stationary
//! consumers is also the termination argument: walked principals only ever shrink.
//! Aux wires each lengthen by one bend through the vacated cell (per-edge layers let both
//! bends share the departure edge), and reeling onto a crossing tucks the crossed strand
//! behind the mover (§4.4).

use crate::lattice::{dir_to, he_opp, step, AgentCell, Cell, Dir, Grid, He, Pos, Strand, DIRS, WIRE_CAP};
use crate::net::Net;
use crate::rules::{find, End, Rule, Tag};
use std::collections::{BTreeMap, BTreeSet};

// =========================================================================================
// FIRE
// =========================================================================================

#[derive(Debug)]
pub struct FirePlan {
    pub cpos: Pos,
    pub ppos: Pos,
    pub csid: u32,
    pub psid: u32,
    pub rule: &'static Rule,
    pub fresh_cells: Vec<Pos>,
    pub fresh_faces: Vec<[Option<He>; 3]>,
    pub fresh_tucks: Vec<[Option<Strand>; 2]>,
    /// New strand elements: splices through the vacated cells, links between fresh agents,
    /// and any dying agent's tuck that stays behind as a plain strand.
    pub strands: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

/// Planner working state over the bounded board.
#[derive(Clone)]
struct BoardState {
    /// Cells agents may be placed on (empty in the grid, or a vacated dying cell with no
    /// leftover anchors).
    placeable: BTreeSet<Pos>,
    /// Cells strands may pass through, with their BASE occupancy (count, half-edges) as it
    /// will be after the pair vacates (dying agents' ports gone, their tucks kept).
    wireable: BTreeMap<Pos, (usize, Vec<He>)>,
    /// Half-edge view of cells OUTSIDE the plan (external wires, bystander agents), for
    /// edge-capacity checks. Read-only.
    outside: BTreeMap<Pos, Vec<He>>,
    /// Plan-side additions.
    agent_at: BTreeMap<Pos, usize>,
    added: BTreeMap<Pos, Vec<Strand>>,
    faces: Vec<[Option<He>; 3]>,
    tucks: Vec<[Option<Strand>; 2]>,
    /// First pass refuses to stack strands (a via is a future roadblock); second pass
    /// permits them. §4.4's face-uniqueness lives on as this preference.
    avoid_stacking: bool,
}

impl BoardState {
    fn hes_at(&self, p: Pos) -> Vec<He> {
        let mut v = vec![];
        if let Some(k) = self.agent_at.get(&p) {
            v.extend(self.faces[*k].iter().flatten().copied());
            for t in self.tucks[*k].iter().flatten() { v.extend(t.hes()); }
        } else if let Some((_, base)) = self.wireable.get(&p) {
            v.extend(base.iter().copied());
        } else if let Some(out) = self.outside.get(&p) {
            v.extend(out.iter().copied());
        }
        if let Some(add) = self.added.get(&p) {
            for s in add { v.extend(s.hes()); }
        }
        v
    }
    fn strand_fits(&self, p: Pos, s: Strand) -> bool {
        if self.agent_at.contains_key(&p) { return false; }
        let Some((base_n, _)) = self.wireable.get(&p) else { return false };
        let extra = self.added.get(&p).map_or(0, |v| v.len());
        if base_n + extra >= WIRE_CAP { return false; }
        if self.avoid_stacking && base_n + extra >= 1 { return false; }
        let used = self.hes_at(p);
        !used.contains(&s.a) && !used.contains(&s.b)
    }
    fn add_strand(&mut self, p: Pos, s: Strand) { self.added.entry(p).or_default().push(s); }
    /// Lowest free layer on the edge (p, d), consulting plan state on both sides.
    fn edge_layer(&self, p: Pos, d: Dir) -> Option<u8> {
        let q = step(p, d);
        (0..2u8).find(|l| !self.hes_at(p).contains(&(d, *l)) && !self.hes_at(q).contains(&(d.opp(), *l)))
    }
    fn face_free(&self, k: usize, h: He) -> bool {
        if self.faces[k].iter().flatten().any(|f| *f == h) { return false; }
        if self.tucks[k].iter().flatten().any(|t| t.contains(h)) { return false; }
        true
    }
}

/// One endpoint of a wire the planner must realize.
#[derive(Clone, Copy, Debug)]
enum PEnd {
    /// Port `port` of fresh agent `k` (half-edge picked by the router unless pre-fixed).
    Agent { k: usize, port: usize },
    /// An external wire arriving into `cell` through half-edge `he` (a dying cell's anchor).
    Half { cell: Pos, he: He },
}

/// Is the pair at (cpos → producer at ppos) an enabled fire? Returns the full plan or None
/// (not adjacent-facing, or no feasible local layout — the pair waits).
pub fn plan_fire(grid: &Grid, cpos: Pos) -> Option<FirePlan> {
    let cons = grid.agent(cpos)?;
    if !cons.tag.is_consumer() { return None; }
    let (d0, _l0) = cons.faces[0]?;
    let ppos = step(cpos, d0);
    let prod = grid.agent(ppos)?;
    if !prod.tag.is_producer() { return None; }
    if prod.faces[0].map(|h| h.0) != Some(d0.opp()) { return None; }
    // The principal wire must be the DIRECT edge between them (same layer both sides).
    if he_opp(cons.face_of(0)) != prod.face_of(0) { return None; }
    let rule = find(cons.tag, prod.tag)
        .unwrap_or_else(|| panic!("adjacent facing {}·{} has no rule", cons.tag.name(), prod.tag.name()));

    // ---- the board: the pair + its 8-ring (12 cells) ----
    let mut board: BTreeSet<Pos> = BTreeSet::new();
    for base in [cpos, ppos] {
        for dx in -2..=2 { for dy in -2..=2 { board.insert((base.0 + dx, base.1 + dy)); } }
    }

    let mut placeable: BTreeSet<Pos> = BTreeSet::new();
    let mut wireable: BTreeMap<Pos, (usize, Vec<He>)> = BTreeMap::new();
    let mut outside: BTreeMap<Pos, Vec<He>> = BTreeMap::new();
    for &p in &board {
        if p == cpos || p == ppos { continue; }
        match grid.cells.get(&p) {
            None => { placeable.insert(p); wireable.insert(p, (0, vec![])); }
            Some(Cell::Wire(w)) => { wireable.insert(p, (w.count(), w.used_hes())); }
            Some(Cell::Agent(a)) => { outside.insert(p, a.used_hes()); }
        }
    }
    // Ring-adjacent cells outside the board still bound edge capacity; collect their view.
    for &p in &board {
        for d in DIRS {
            let q = step(p, d);
            if !board.contains(&q) && !outside.contains_key(&q) {
                outside.insert(q, grid.used_hes(q));
            }
        }
    }
    let ctucks = cons.tucks;
    let ptucks = prod.tucks;

    // ---- anchors: each dying cell's aux half-edges and their rule partners ----
    let partner_of = |e: End| -> End {
        for (a, b) in rule.wires { if *a == e { return *b; } if *b == e { return *a; } }
        unreachable!("validated ROM")
    };
    struct Anchor { cell: Pos, he: He, own: End, partner: End }
    let mut anchors: Vec<Anchor> = vec![];
    for i in 1..cons.tag.arity() {
        anchors.push(Anchor { cell: cpos, he: cons.face_of(i), own: End::CAux(i as u8), partner: partner_of(End::CAux(i as u8)) });
    }
    for j in 1..prod.tag.arity() {
        anchors.push(Anchor { cell: ppos, he: prod.face_of(j), own: End::PAux(j as u8), partner: partner_of(End::PAux(j as u8)) });
    }

    // ---- residency: fresh k may take over a dying cell iff it partners ALL that cell's
    // anchors (those ports then inherit the anchor half-edges with zero rewiring) ----
    let resident_for = |cell: Pos, tucks: [Option<Strand>; 2]| -> Option<usize> {
        let mine: Vec<&Anchor> = anchors.iter().filter(|a| a.cell == cell).collect();
        if mine.is_empty() { return None; }
        let mut k: Option<usize> = None;
        for a in &mine {
            match a.partner {
                End::Fresh(f, _) => {
                    if k.is_some_and(|x| x != f as usize) { return None; }
                    k = Some(f as usize);
                }
                _ => return None,
            }
        }
        let k = k?;
        for t in tucks.iter().flatten() {
            for a in &mine { if t.contains(a.he) { return None; } }
        }
        Some(k)
    };
    let mut resident: BTreeMap<Pos, usize> = BTreeMap::new();
    if let Some(k) = resident_for(cpos, ctucks) { resident.insert(cpos, k); }
    if let Some(k) = resident_for(ppos, ptucks) {
        if resident.get(&cpos) != Some(&k) { resident.insert(ppos, k); }
    }

    // Vacated cells not taken by a resident become wireable (baseline = their tuck); those
    // with leftover anchors must stay wire-only (a squatter would strand the anchors).
    for (cell, tucks) in [(cpos, ctucks), (ppos, ptucks)] {
        if !resident.contains_key(&cell) {
            let n = tucks.iter().flatten().count();
            let f: Vec<He> = tucks.iter().flatten().flat_map(|t| t.hes()).collect();
            wireable.insert(cell, (n, f));
            // Placeable only when nothing must pass through it: leftover anchors need
            // splice room, and leftover tucks stay behind as strands.
            if !anchors.iter().any(|a| a.cell == cell) && n == 0 { placeable.insert(cell); }
        }
    }

    let nf = rule.fresh.len();
    let mut st = BoardState {
        placeable,
        wireable,
        outside,
        agent_at: BTreeMap::new(),
        added: BTreeMap::new(),
        faces: vec![[None; 3]; nf],
        tucks: vec![[None; 2]; nf],
        avoid_stacking: true,
    };
    let mut fresh_cells: Vec<Option<Pos>> = vec![None; nf];
    let mut done_wires: BTreeSet<usize> = BTreeSet::new();

    // Seat residents: anchor-partnered ports inherit the anchor half-edges verbatim.
    for (&cell, &k) in &resident {
        fresh_cells[k] = Some(cell);
        st.agent_at.insert(cell, k);
        st.placeable.remove(&cell);
        st.wireable.remove(&cell);
        st.tucks[k] = if cell == cpos { ctucks } else { ptucks };
        for a in anchors.iter().filter(|a| a.cell == cell) {
            let End::Fresh(_, p) = a.partner else { unreachable!() };
            if st.faces[k][p as usize].is_some() { return None; }
            st.faces[k][p as usize] = Some(a.he);
            let widx = rule.wires.iter().position(|(x, y)| *x == a.own || *y == a.own).unwrap();
            done_wires.insert(widx);
        }
    }

    // Place the remaining fresh agents: prefer cells adjacent to already-placed partners
    // AND to the hub cells their anchor wires must splice through (adjacency to the hub is
    // most of routability), then closest to the pair. Deterministic ordering throughout.
    let partners_of = |k: usize| -> Vec<usize> {
        let mut v = vec![];
        for (a, b) in rule.wires {
            if let (End::Fresh(x, _), End::Fresh(y, _)) = (a, b) {
                if *x as usize == k { v.push(*y as usize); }
                if *y as usize == k { v.push(*x as usize); }
            }
        }
        v
    };
    let hubs_of = |k: usize| -> Vec<Pos> {
        let mut v = vec![];
        for (a, b) in rule.wires {
            let hub = |e: &End| match e { End::CAux(_) => Some(cpos), End::PAux(_) => Some(ppos), _ => None };
            if let End::Fresh(x, _) = a { if *x as usize == k { v.extend(hub(b)); } }
            if let End::Fresh(y, _) = b { if *y as usize == k { v.extend(hub(a)); } }
        }
        v
    };
    // Placement + routing with bounded BACKTRACKING: a placement that suits early wires
    // can seal a hub against a later one (measured: T1 seated west of cpos walled off the
    // arg splice), so on routing failure the next placement is tried, deterministically,
    // up to a fixed attempt budget. The budget keeps plan_fire O(1) per call; exhausting
    // it means the fire waits.
    let to_pend = |e: End| -> PEnd {
        match e {
            End::Fresh(k, p) => PEnd::Agent { k: k as usize, port: p as usize },
            End::CAux(i) => PEnd::Half { cell: cpos, he: anchors.iter().find(|x| x.own == End::CAux(i)).unwrap().he },
            End::PAux(j) => PEnd::Half { cell: ppos, he: anchors.iter().find(|x| x.own == End::PAux(j)).unwrap().he },
        }
    };
    // Most-constrained wires first: anchor endpoints have FIXED half-edges at the hub
    // cells with limited approach capacity, so they route before the freely-placeable
    // fresh-fresh links (first-fit in template order let the free wires eat the hub ring
    // and strand the anchors — the measured T1·S failure).
    let mut wire_order: Vec<usize> = (0..rule.wires.len()).filter(|w| !done_wires.contains(w)).collect();
    let constrainedness = |w: usize| -> i32 {
        let (a, b) = &rule.wires[w];
        let halfs = [a, b].iter().filter(|e| !matches!(e, End::Fresh(_, _))).count() as i32;
        -halfs // more Half endpoints = smaller key = routed earlier
    };
    wire_order.sort_by_key(|w| (constrainedness(*w), *w));
    let last_fail: std::cell::Cell<i32> = std::cell::Cell::new(-1);
    let route_all = |st: &mut BoardState, cells: &[Pos]| -> bool {
        let trace = std::env::var("TR_TRACE").is_ok();
        if trace { eprintln!("  attempt: cells={cells:?} order={wire_order:?}"); }
        for &widx in &wire_order {
            let (a, b) = &rule.wires[widx];
            let routed = route_wire(st, cells, to_pend(*a), to_pend(*b), &board) || {
                st.avoid_stacking = false;
                let r = route_wire(st, cells, to_pend(*a), to_pend(*b), &board);
                st.avoid_stacking = true;
                r
            };
            if !routed {
                if trace { eprintln!("    wire {widx} {:?}<->{:?} FAILED", rule.wires[widx].0, rule.wires[widx].1); }
                last_fail.set(widx as i32);
                return false;
            }
            if trace { eprintln!("    wire {widx} ok"); }
        }
        for k in 0..nf {
            for p in 0..rule.fresh[k].arity() {
                if st.faces[k][p].is_none() { last_fail.set(100 + k as i32); return false; }
            }
        }
        true
    };

    fn solve(
        st: &BoardState,
        cells: &mut Vec<Option<Pos>>,
        k: usize,
        nf: usize,
        attempts: &mut u32,
        partners_of: &dyn Fn(usize) -> Vec<usize>,
        hubs_of: &dyn Fn(usize) -> Vec<Pos>,
        pair: (Pos, Pos),
        route_all: &dyn Fn(&mut BoardState, &[Pos]) -> bool,
    ) -> Option<(BoardState, Vec<Pos>)> {
        if *attempts == 0 { return None; }
        if k == nf {
            *attempts -= 1;
            let flat: Vec<Pos> = cells.iter().map(|c| c.unwrap()).collect();
            let mut st2 = st.clone();
            if route_all(&mut st2, &flat) { return Some((st2, flat)); }
            return None;
        }
        if cells[k].is_some() {
            return solve(st, cells, k + 1, nf, attempts, partners_of, hubs_of, pair, route_all);
        }
        // Candidates sorted by total wire distance: for each of k's wires, the Manhattan
        // distance from the candidate to the wire's other endpoint (hub cell, or partner's
        // cell when already placed). A smooth gradient beats adjacency-or-nothing: with no
        // hub-adjacent cell free, adjacency scoring degenerated to lexicographic ties and
        // clustered every fresh agent on one side, stranding the cross wires.
        let mut cands: Vec<(i32, i32, Pos)> = st.placeable.iter().map(|&cand| {
            let mut wire_dist = 0i32;
            for m in partners_of(k) {
                if let Some(mc) = cells[m] {
                    wire_dist += (cand.0 - mc.0).abs() + (cand.1 - mc.1).abs();
                }
            }
            for h in hubs_of(k) {
                wire_dist += (cand.0 - h.0).abs() + (cand.1 - h.1).abs();
            }
            let dist = (cand.0 - pair.0 .0).abs() + (cand.1 - pair.0 .1).abs()
                + (cand.0 - pair.1 .0).abs() + (cand.1 - pair.1 .1).abs();
            (wire_dist, dist, cand)
        }).collect();
        cands.sort();
        // Branch over the top few per level, so the attempt budget explores ALTERNATIVE
        // early placements instead of exhausting itself at the deepest level.
        cands.truncate(3);
        for (_, _, cell) in cands {
            let mut st2 = st.clone();
            st2.agent_at.insert(cell, k);
            st2.placeable.remove(&cell);
            st2.wireable.remove(&cell);
            cells[k] = Some(cell);
            if let Some(r) = solve(&st2, cells, k + 1, nf, attempts, partners_of, hubs_of, pair, route_all) {
                return Some(r);
            }
            cells[k] = None;
            if *attempts == 0 { return None; }
        }
        None
    }

    let mut attempts: u32 = 96;
    let Some((st_final, fresh_cells)) = solve(
        &st, &mut fresh_cells, 0, nf, &mut attempts,
        &partners_of, &hubs_of, (cpos, ppos), &route_all,
    ) else {
        if std::env::var("TR_DEBUG").is_ok() {
            eprintln!("fire {}·{}: no feasible placement+routing in {} attempts (last failing wire: {})",
                rule.consumer.name(), rule.producer.name(), 64, last_fail.get());
        }
        return None;
    };
    let st = st_final;

    // Assemble: leftover dying-cell tucks that nobody inherited become plain strands.
    let mut strands: Vec<(Pos, Strand)> = vec![];
    for (cell, tucks) in [(cpos, ctucks), (ppos, ptucks)] {
        if !resident.contains_key(&cell) {
            for t in tucks.iter().flatten() { strands.push((cell, *t)); }
        }
    }
    for (p, v) in &st.added { for s in v { strands.push((*p, *s)); } }

    let mut footprint: BTreeSet<Pos> = BTreeSet::new();
    footprint.insert(cpos);
    footprint.insert(ppos);
    for (p, _) in &strands { footprint.insert(*p); }
    for p in &fresh_cells { footprint.insert(*p); }

    Some(FirePlan {
        cpos, ppos,
        csid: cons.sid, psid: prod.sid,
        rule,
        fresh_cells,
        fresh_faces: st.faces,
        fresh_tucks: st.tucks,
        strands,
        footprint,
    })
}

/// Realize one wire between two endpoints as ≤4 strand elements inside the board.
/// Deterministic first-fit DFS with per-edge layer selection. Mutates `st` on success.
fn route_wire(
    st: &mut BoardState,
    cells: &[Pos],
    a: PEnd,
    b: PEnd,
    board: &BTreeSet<Pos>,
) -> bool {
    // Zero-strand case: two agent ports directly facing across one edge.
    if let (PEnd::Agent { k: ka, port: pa }, PEnd::Agent { k: kb, port: pb }) = (a, b) {
        let (ca, cb) = (cells[ka], cells[kb]);
        if let Some(d) = dir_to(ca, cb) {
            if let Some(l) = st.edge_layer(ca, d) {
                if st.face_free(ka, (d, l)) && st.face_free(kb, (d.opp(), l)) {
                    st.faces[ka][pa] = Some((d, l));
                    st.faces[kb][pb] = Some((d.opp(), l));
                    return true;
                }
            }
        }
    }
    // Entry states: (first strand cell, its entry half-edge, deferred A-side face setter).
    let starts: Vec<(Pos, He, Option<(usize, usize, He)>)> = match a {
        PEnd::Agent { k, port } => {
            let ca = cells[k];
            let mut v = vec![];
            for d in DIRS {
                let n = step(ca, d);
                if !board.contains(&n) { continue; }
                let Some(l) = st.edge_layer(ca, d) else { continue };
                if !st.face_free(k, (d, l)) { continue; }
                v.push((n, (d.opp(), l), Some((k, port, (d, l)))));
            }
            v
        }
        PEnd::Half { cell, he } => vec![(cell, he, None)],
    };

    for (w1, enter, aface) in starts {
        if !board.contains(&w1) { continue; }
        let mut trail: Vec<(Pos, He)> = vec![(w1, enter)];
        if dfs_wire(st, cells, b, &mut trail, board) {
            if let Some((k, port, h)) = aface { st.faces[k][port] = Some(h); }
            return true;
        }
    }
    false
}

fn dfs_wire(
    st: &mut BoardState,
    cells: &[Pos],
    b: PEnd,
    trail: &mut Vec<(Pos, He)>,
    board: &BTreeSet<Pos>,
) -> bool {
    let (cur, enter) = *trail.last().unwrap();

    // Commit helper: lay the whole trail given the final strand at `cur`.
    let commit = |st: &mut BoardState, trail: &[(Pos, He)], last: Strand| -> bool {
        let mut staged: Vec<(Pos, Strand)> = vec![(trail.last().unwrap().0, last)];
        for i in (0..trail.len() - 1).rev() {
            let (p, ein) = trail[i];
            let nxt = trail[i + 1];
            let d = dir_to(p, nxt.0).unwrap();
            let l = nxt.1 .1;
            let s = Strand::new(ein, (d, l));
            if !st.strand_fits(p, s) { return false; }
            staged.push((p, s));
        }
        for (p, s) in staged { st.add_strand(p, s); }
        true
    };

    // Try to TERMINATE here.
    match b {
        PEnd::Agent { k, port } => {
            let cb = cells[k];
            if let Some(out) = dir_to(cur, cb) {
                if let Some(l) = st.edge_layer(cur, out) {
                    let h = (out, l);
                    if h != enter && st.face_free(k, (out.opp(), l)) {
                        let s = Strand::new(enter, h);
                        if st.strand_fits(cur, s) && commit(st, trail, s) {
                            st.faces[k][port] = Some((out.opp(), l));
                            return true;
                        }
                    }
                }
            }
        }
        PEnd::Half { cell, he } => {
            if cur == cell && enter != he {
                let s = Strand::new(enter, he);
                if st.strand_fits(cur, s) && commit(st, trail, s) { return true; }
            }
        }
    }
    if trail.len() >= 6 { return false; }
    // Extend the trail.
    for d in DIRS {
        let n = step(cur, d);
        if !board.contains(&n) { continue; }
        if trail.iter().any(|(p, _)| *p == n) { continue; }
        if st.agent_at.contains_key(&n) || !st.wireable.contains_key(&n) { continue; }
        let Some(l) = st.edge_layer(cur, d) else { continue };
        let h = (d, l);
        if h == enter { continue; }
        let s = Strand::new(enter, h);
        if !st.strand_fits(cur, s) { continue; }
        trail.push((n, (d.opp(), l)));
        if dfs_wire(st, cells, b, trail, board) { return true; }
        trail.pop();
    }
    false
}

/// Execute a fire plan. The shadow fires the SAME rule; fresh sids sync in template order.
pub fn apply_fire(grid: &mut Grid, shadow: &mut Net, plan: &FirePlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");

    let (rule, fresh_sids) = shadow.fire(plan.csid, plan.psid);
    assert!(std::ptr::eq(rule, plan.rule), "shadow fired a different rule");

    guard(plan.cpos); guard(plan.ppos);
    grid.cells.remove(&plan.cpos);
    grid.cells.remove(&plan.ppos);
    for (p, s) in &plan.strands { guard(*p); grid.add_strand(*p, *s); }
    for k in 0..plan.fresh_cells.len() {
        guard(plan.fresh_cells[k]);
        grid.put_agent(plan.fresh_cells[k], AgentCell {
            tag: plan.rule.fresh[k],
            faces: plan.fresh_faces[k],
            tucks: plan.fresh_tucks[k],
            sid: fresh_sids[k],
        });
    }
}

// =========================================================================================
// REEL
// =========================================================================================

#[derive(Debug)]
pub struct ReelPlan {
    pub apos: Pos,
    pub npos: Pos,
    pub consumed: Strand,
    pub new_faces: [Option<He>; 3],
    pub new_tucks: [Option<Strand>; 2],
    /// Strands written behind the mover: the vacated cell's aux bends and the carried-over
    /// old tuck.
    pub writes: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

/// Walk one cell along the principal wire (see module doc for the polarity and the
/// fire-awaits gate).
pub fn plan_reel(grid: &Grid, apos: Pos) -> Option<ReelPlan> {
    let ag = grid.agent(apos)?;
    if !ag.tag.is_producer() { return None; }
    let h0 = ag.faces[0]?;
    let d = h0.0;
    let npos = step(apos, d);
    let w = grid.wire(npos)?; // agent-adjacent (fire's business) or empty (impossible): not a reel
    let (far_pos, far_port) = grid.trace(apos, h0);
    let far = grid.agent(far_pos).expect("trace ends at an agent");
    if far_port != 0 || !far.tag.is_consumer() { return None; } // no fire awaits: stay parked
    let mine = w.with_he(he_opp(h0))?;
    let f = mine.other(he_opp(h0));

    // Strands remaining at npos after consuming mine: the prospective tuck.
    let leftover: Vec<Strand> = w.iter().filter(|s| *s != mine).collect();
    if leftover.len() > 2 { return None; } // at most two strands tuck behind an agent
    let mut new_tucks: [Option<Strand>; 2] = [None; 2];
    for (i, s) in leftover.iter().enumerate() { new_tucks[i] = Some(*s); }

    // Plan-aware bookkeeping: apos is being vacated (old tuck stays, bends accrue), npos
    // is being occupied (principal at f, tuck half-edges persist, aux faces accrue),
    // detour cells accrue written strands on top of the live grid.
    let mut writes: Vec<(Pos, Strand)> = vec![];
    for t in ag.tucks.iter().flatten() { writes.push((apos, *t)); }
    let mut new_faces: [Option<He>; 3] = [None; 3];
    new_faces[0] = Some(f);
    let mut npos_used: Vec<He> = vec![f];
    for t in new_tucks.iter().flatten() { npos_used.extend(t.hes()); }
    if npos_used[1..].contains(&f) { return None; }
    let mut footprint: BTreeSet<Pos> = [apos, npos].into_iter().collect();

    // Helpers over (grid + plan-so-far). `used` gives every occupied half-edge at a cell.
    fn used(grid: &Grid, apos: Pos, npos: Pos, writes: &[(Pos, Strand)], npos_used: &[He], p: Pos) -> Vec<He> {
        if p == npos { return npos_used.to_vec(); }
        let mut v = if p == apos { vec![] } else { grid.used_hes(p) };
        for (q, s) in writes { if *q == p { v.extend(s.hes()); } }
        v
    }
    fn count(grid: &Grid, apos: Pos, writes: &[(Pos, Strand)], p: Pos) -> usize {
        let base = if p == apos { 0 } else { grid.strand_count(p) };
        base + writes.iter().filter(|(q, _)| *q == p).count()
    }
    fn edge_layer(grid: &Grid, apos: Pos, npos: Pos, writes: &[(Pos, Strand)], npos_used: &[He], p: Pos, dd: Dir) -> Option<u8> {
        let q = step(p, dd);
        (0..2u8).find(|l| {
            !used(grid, apos, npos, writes, npos_used, p).contains(&(dd, *l))
                && !used(grid, apos, npos, writes, npos_used, q).contains(&(dd.opp(), *l))
        })
    }
    // A strand may be laid at p (given plan state) if the cell is apos, empty, or a wire
    // cell with room, and its half-edges are free there.
    fn lay_ok(grid: &Grid, apos: Pos, npos: Pos, writes: &[(Pos, Strand)], npos_used: &[He], p: Pos, s: Strand) -> bool {
        if p == npos { return false; }
        if p != apos && !grid.is_empty(p) && grid.wire(p).is_none() { return false; }
        if count(grid, apos, writes, p) >= WIRE_CAP { return false; }
        let u = used(grid, apos, npos, writes, npos_used, p);
        !u.contains(&s.a) && !u.contains(&s.b)
    }

    for port in 1..ag.tag.arity() {
        let g = ag.face_of(port);
        let mut placed = false;
        // Straight-behind: bend at apos, re-enter npos across the departure edge.
        if let Some(l) = edge_layer(grid, apos, npos, &writes, &npos_used, apos, d) {
            let bend = Strand::new(g, (d, l));
            let nface = (d.opp(), l);
            if lay_ok(grid, apos, npos, &writes, &npos_used, apos, bend) && !npos_used.contains(&nface) {
                writes.push((apos, bend));
                npos_used.push(nface);
                new_faces[port] = Some(nface);
                placed = true;
            }
        }
        // Corner detour: apos --e--> k1 --d--> k2 --opp(e)--> npos. Two extra cells; with
        // per-edge layers this is the rare fallback for a congested departure edge (e.g.
        // a foreign wire running parallel to the walk claims a layer of that edge).
        if !placed {
            for e in d.perp() {
                let (k1, k2) = (step(apos, e), step(step(apos, e), d));
                let Some(l1) = edge_layer(grid, apos, npos, &writes, &npos_used, apos, e) else { continue };
                let s0 = Strand::new(g, (e, l1));
                if !lay_ok(grid, apos, npos, &writes, &npos_used, apos, s0) { continue; }
                let mut w2 = writes.clone();
                w2.push((apos, s0));
                let Some(l2) = edge_layer(grid, apos, npos, &w2, &npos_used, k1, d) else { continue };
                let s1 = Strand::new((e.opp(), l1), (d, l2));
                if !lay_ok(grid, apos, npos, &w2, &npos_used, k1, s1) { continue; }
                w2.push((k1, s1));
                let Some(l3) = edge_layer(grid, apos, npos, &w2, &npos_used, k2, e.opp()) else { continue };
                let s2 = Strand::new((d.opp(), l2), (e.opp(), l3));
                if !lay_ok(grid, apos, npos, &w2, &npos_used, k2, s2) { continue; }
                let nface = (e, l3);
                if npos_used.contains(&nface) { continue; }
                w2.push((k2, s2));
                writes = w2;
                npos_used.push(nface);
                footprint.insert(k1);
                footprint.insert(k2);
                new_faces[port] = Some(nface);
                placed = true;
                break;
            }
        }
        if !placed { return None; }
    }

    Some(ReelPlan {
        apos, npos,
        consumed: mine,
        new_faces,
        new_tucks,
        writes,
        footprint,
    })
}

pub fn apply_reel(grid: &mut Grid, plan: &ReelPlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    let Some(Cell::Agent(ag)) = grid.cells.get(&plan.apos).cloned() else { panic!("reel: no agent") };

    guard(plan.apos);
    grid.cells.remove(&plan.apos);
    for (p, s) in &plan.writes { guard(*p); grid.add_strand(*p, *s); }
    guard(plan.npos);
    grid.remove_strand(plan.npos, plan.consumed);
    for t in plan.new_tucks.iter().flatten() { grid.remove_strand(plan.npos, *t); }
    grid.put_agent(plan.npos, AgentCell {
        tag: ag.tag,
        faces: plan.new_faces,
        tucks: plan.new_tucks,
        sid: ag.sid,
    });
    grid.transport += 1;
}
