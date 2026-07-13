//! Rung 2 dynamics on the lifted lattice: the footprint-atomic transitions.
//!
//! Every transition is two phases with one contract:
//!   `plan_*`  — reads ONLY cells inside a bounded neighborhood, decides feasibility, and
//!               returns a Plan carrying its exact FOOTPRINT (the cell set it may write).
//!   `apply_*` — executes the plan; every mutation is asserted to stay inside the footprint.
//!
//! Four transitions:
//!
//! FIRE (rule 1): an adjacent, mutually-facing consumer·producer pair rewrites by the ROM
//! template. The two dying cells become splice hubs; fresh agents are placed and their
//! template wires routed inside a bounded board with backtracking. (This in-board search is
//! the one non-CA-shaped planner left; the geometric-fire redesign replaces it.)
//!
//! REEL (rule 2, Walk): a producer steps one cell along its principal wire, consuming one
//! strand — only when the far end is a consumer's principal (a fire awaits). On the lifted
//! lattice there is NO coexistence: a walker whose next cell carries any foreign strand
//! WAITS there instead of tucking; kink-flips (and later the fields) are the decongestants.
//! Aux wires bend through the vacated cell; when the departure edge is taken, the bend
//! detours around it through any of the FOUR perpendicular directions (in bilayer that
//! includes over-the-top through z=1, the chip picture).
//!
//! RETRACT (polymer move 1): a width-1 U-turn annihilates — two adjacent elbow strands
//! drop out and their flank cells connect directly. Wire length −2. The discrete
//! curve-shortening step; the only wire-length sink besides reel itself.
//!
//! SLIDE (polymer move 2): a strand relocates OUT of its cell — its chain neighbors
//! reconnect through any short route avoiding it. The excluded-volume move, scheduled
//! walker-demanded (a blocked walker asks the strand in its way to slide). The kink-flip
//! of polymer dynamics is its 1-cell route; the over-the-top bulge its 3-cell route.
//!
//! RETRACT and SLIDE read and write only a small bounded cell set and project to
//! IDENTITY: they move wire geometry, never connectivity, so `check_projection` holds
//! verbatim across them. This is the machine-checked half of the rung-3
//! ordering-resilience story; the semantic half is strong confluence (tc-net.typ Thm 2)
//! carried by the projection invariant, asserted per transition in checked runs.

use crate::lattice::{dir_to, manhattan, step, AgentCell, Cell, Dir, Grid, He, Pos, Strand, DIRS, WIRE_CAP};
use crate::net::Net;
use crate::rules::{find, End, Rule};
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
    /// New strand elements: splices through the vacated cells and links between fresh agents.
    pub strands: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

/// Planner working state over the bounded board.
#[derive(Clone)]
struct BoardState {
    /// Cells agents may be placed on (empty in the grid, or a vacated dying cell with no
    /// leftover anchors).
    placeable: BTreeSet<Pos>,
    /// Cells strands may pass through, with their BASE occupancy (count, faces) as it
    /// will be after the pair vacates.
    wireable: BTreeMap<Pos, (usize, Vec<He>)>,
    /// Face view of cells OUTSIDE the plan (external wires, bystander agents), for
    /// edge-capacity checks. Read-only.
    outside: BTreeMap<Pos, Vec<He>>,
    /// Plan-side additions.
    agent_at: BTreeMap<Pos, usize>,
    added: BTreeMap<Pos, Vec<Strand>>,
    faces: Vec<[Option<He>; 3]>,
    /// First pass refuses to stack strands (a shared cell is a future roadblock for
    /// walkers); second pass permits them. Face-uniqueness lives on as this preference.
    avoid_stacking: bool,
}

impl BoardState {
    fn hes_at(&self, p: Pos) -> Vec<He> {
        let mut v = vec![];
        if let Some(k) = self.agent_at.get(&p) {
            v.extend(self.faces[*k].iter().flatten().copied());
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
    /// Is the edge (p, d) free on both faces, consulting plan state on both sides?
    fn edge_free(&self, p: Pos, d: Dir) -> bool {
        let q = step(p, d);
        !self.hes_at(p).contains(&d) && !self.hes_at(q).contains(&d.opp())
    }
    fn face_free(&self, k: usize, h: He) -> bool {
        !self.faces[k].iter().flatten().any(|f| *f == h)
    }
}

/// One endpoint of a wire the planner must realize.
#[derive(Clone, Copy, Debug)]
enum PEnd {
    /// Port `port` of fresh agent `k` (face picked by the router unless pre-fixed).
    Agent { k: usize, port: usize },
    /// An external wire arriving into `cell` through face `he` (a dying cell's anchor).
    Half { cell: Pos, he: He },
}

/// Is the pair at (cpos → producer at ppos) an enabled fire? Returns the full plan or None
/// (not adjacent-facing, or no feasible local layout — the pair waits).
pub fn plan_fire(grid: &Grid, cpos: Pos) -> Option<FirePlan> {
    let cons = grid.agent(cpos)?;
    if !cons.tag.is_consumer() { return None; }
    let d0 = cons.faces[0]?;
    let ppos = step(cpos, d0);
    let prod = grid.agent(ppos)?;
    if !prod.tag.is_producer() { return None; }
    // The principal wire must be the direct edge between them.
    if prod.faces[0] != Some(d0.opp()) { return None; }
    let rule = find(cons.tag, prod.tag)
        .unwrap_or_else(|| panic!("adjacent facing {}·{} has no rule", cons.tag.name(), prod.tag.name()));

    // ---- the board: everything within Chebyshev distance 2 of the pair, in bounds ----
    let board = make_board(grid, cpos, ppos);

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

    // ---- anchors: each dying cell's aux faces and their rule partners ----
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
    // anchors (those ports then inherit the anchor faces with zero rewiring) ----
    let resident_for = |cell: Pos| -> Option<usize> {
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
        k
    };
    let mut resident: BTreeMap<Pos, usize> = BTreeMap::new();
    if let Some(k) = resident_for(cpos) { resident.insert(cpos, k); }
    if let Some(k) = resident_for(ppos) {
        if resident.get(&cpos) != Some(&k) { resident.insert(ppos, k); }
    }

    // Vacated cells not taken by a resident become wireable; those with leftover anchors
    // must stay wire-only (a squatter would strand the anchors).
    for cell in [cpos, ppos] {
        if !resident.contains_key(&cell) {
            wireable.insert(cell, (0, vec![]));
            if !anchors.iter().any(|a| a.cell == cell) { placeable.insert(cell); }
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
        avoid_stacking: true,
    };
    let mut fresh_cells: Vec<Option<Pos>> = vec![None; nf];
    let mut done_wires: BTreeSet<usize> = BTreeSet::new();

    // Seat residents: anchor-partnered ports inherit the anchor faces verbatim.
    for (&cell, &k) in &resident {
        fresh_cells[k] = Some(cell);
        st.agent_at.insert(cell, k);
        st.placeable.remove(&cell);
        st.wireable.remove(&cell);
        for a in anchors.iter().filter(|a| a.cell == cell) {
            let End::Fresh(_, p) = a.partner else { unreachable!() };
            if st.faces[k][p as usize].is_some() { return None; }
            st.faces[k][p as usize] = Some(a.he);
            let widx = rule.wires.iter().position(|(x, y)| *x == a.own || *y == a.own).unwrap();
            done_wires.insert(widx);
        }
    }

    // Place the remaining fresh agents (backtracking, deterministic) and route the wires
    // most-constrained-first. See the rung-2 findings in the README: greedy first-fit
    // seals its own hubs.
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
    let to_pend = |e: End| -> PEnd {
        match e {
            End::Fresh(k, p) => PEnd::Agent { k: k as usize, port: p as usize },
            End::CAux(i) => PEnd::Half { cell: cpos, he: anchors.iter().find(|x| x.own == End::CAux(i)).unwrap().he },
            End::PAux(j) => PEnd::Half { cell: ppos, he: anchors.iter().find(|x| x.own == End::PAux(j)).unwrap().he },
        }
    };
    let mut wire_order: Vec<usize> = (0..rule.wires.len()).filter(|w| !done_wires.contains(w)).collect();
    let constrainedness = |w: usize| -> i32 {
        let (a, b) = &rule.wires[w];
        let halfs = [a, b].iter().filter(|e| !matches!(e, End::Fresh(_, _))).count() as i32;
        -halfs // more Half endpoints = smaller key = routed earlier
    };
    wire_order.sort_by_key(|w| (constrainedness(*w), *w));
    let route_all = |st: &mut BoardState, cells: &[Pos]| -> bool {
        for &widx in &wire_order {
            let (a, b) = &rule.wires[widx];
            let routed = route_wire(st, cells, to_pend(*a), to_pend(*b), &board) || {
                st.avoid_stacking = false;
                let r = route_wire(st, cells, to_pend(*a), to_pend(*b), &board);
                st.avoid_stacking = true;
                r
            };
            if !routed { return false; }
        }
        for k in 0..nf {
            for p in 0..rule.fresh[k].arity() {
                if st.faces[k][p].is_none() { return false; }
            }
        }
        true
    };

    #[allow(clippy::too_many_arguments)]
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
        // Candidates sorted by total wire distance (a smooth gradient beats
        // adjacency-or-nothing; see the rung-2 findings).
        let mut cands: Vec<(i32, i32, Pos)> = st.placeable.iter().map(|&cand| {
            let mut wire_dist = 0i32;
            for m in partners_of(k) {
                if let Some(mc) = cells[m] { wire_dist += manhattan(cand, mc); }
            }
            for h in hubs_of(k) { wire_dist += manhattan(cand, h); }
            let dist = manhattan(cand, pair.0) + manhattan(cand, pair.1);
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
    let (st, fresh_cells) = solve(
        &st, &mut fresh_cells, 0, nf, &mut attempts,
        &partners_of, &hubs_of, (cpos, ppos), &route_all,
    )?;

    let mut strands: Vec<(Pos, Strand)> = vec![];
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
        strands,
        footprint,
    })
}

/// Realize one wire between two endpoints as a short strand chain inside the board.
/// Deterministic first-fit DFS with per-edge exclusivity. Mutates `st` on success.
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
            if st.edge_free(ca, d) && st.face_free(ka, d) && st.face_free(kb, d.opp()) {
                st.faces[ka][pa] = Some(d);
                st.faces[kb][pb] = Some(d.opp());
                return true;
            }
        }
    }
    // Entry states: (first strand cell, its entry face, deferred A-side face setter).
    let starts: Vec<(Pos, He, Option<(usize, usize, He)>)> = match a {
        PEnd::Agent { k, port } => {
            let ca = cells[k];
            let mut v = vec![];
            for d in DIRS {
                let n = step(ca, d);
                if !board.contains(&n) { continue; }
                if !st.edge_free(ca, d) { continue; }
                if !st.face_free(k, d) { continue; }
                v.push((n, d.opp(), Some((k, port, d))));
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
            let s = Strand::new(ein, d);
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
                if out != enter && st.edge_free(cur, out) && st.face_free(k, out.opp()) {
                    let s = Strand::new(enter, out);
                    if st.strand_fits(cur, s) && commit(st, trail, s) {
                        st.faces[k][port] = Some(out.opp());
                        return true;
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
        if d == enter { continue; }
        if !st.edge_free(cur, d) { continue; }
        let s = Strand::new(enter, d);
        if !st.strand_fits(cur, s) { continue; }
        trail.push((n, d.opp()));
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
    /// Strands written behind the mover: the vacated cell's aux bends and their detours.
    pub writes: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

/// The bounded board shared by FIRE and REEL planning: everything within Chebyshev
/// distance 2 of the two focus cells, clipped to the topology.
fn make_board(grid: &Grid, a: Pos, b: Pos) -> BTreeSet<Pos> {
    let mut board: BTreeSet<Pos> = BTreeSet::new();
    for base in [a, b] {
        for dx in -2..=2 { for dy in -2..=2 { for dz in -2..=2 {
            let p = (base.0 + dx, base.1 + dy, base.2 + dz);
            if grid.topo.in_bounds(p) { board.insert(p); }
        }}}
    }
    board
}

/// Walk one cell along the principal wire (see the module doc for the polarity and the
/// fire-awaits gate). Each aux wire is re-anchored by the SAME bounded router fire uses:
/// from its arrival face at the vacated cell to any free face of the mover — the
/// straight-behind bend and the corner detour are just its 1-cell and 3-cell routes.
pub fn plan_reel(grid: &Grid, apos: Pos) -> Option<ReelPlan> {
    let ag = grid.agent(apos)?;
    if !ag.tag.is_producer() { return None; }
    let d = ag.faces[0]?;
    let npos = step(apos, d);
    let w = grid.wire(npos)?; // agent-adjacent is fire's business; empty is impossible (parity)
    let (far_pos, far_port) = grid.trace(apos, d);
    let far = grid.agent(far_pos).expect("trace ends at an agent");
    if far_port != 0 || !far.tag.is_consumer() { return None; } // no fire awaits: stay parked
    let mine = w.with_he(d.opp())?;
    let f = mine.other(d.opp());
    // No coexistence on the lifted lattice: a shared cell makes the walker WAIT (the old
    // tuck). The CLEAR phase (flip/bypass) moves the blocking strand out first.
    if w.count() > 1 { return None; }

    let board = make_board(grid, apos, npos);
    let mut wireable: BTreeMap<Pos, (usize, Vec<He>)> = BTreeMap::new();
    let mut outside: BTreeMap<Pos, Vec<He>> = BTreeMap::new();
    for &p in &board {
        if p == npos { continue; }
        if p == apos { wireable.insert(p, (0, vec![])); continue; } // vacated
        match grid.cells.get(&p) {
            None => { wireable.insert(p, (0, vec![])); }
            Some(Cell::Wire(wc)) => { wireable.insert(p, (wc.count(), wc.used_hes())); }
            Some(Cell::Agent(a)) => { outside.insert(p, a.used_hes()); }
        }
    }
    for &p in &board {
        for dd in DIRS {
            let q = step(p, dd);
            if !board.contains(&q) && !outside.contains_key(&q) {
                outside.insert(q, grid.used_hes(q));
            }
        }
    }
    // One pseudo-agent: the mover, seated at npos, principal face preset. The consumed
    // strand is excluded from npos's view (it is gone by the time the writes land).
    let mut faces0: [Option<He>; 3] = [None; 3];
    faces0[0] = Some(f);
    let mut st = BoardState {
        placeable: BTreeSet::new(),
        wireable,
        outside,
        agent_at: [(npos, 0usize)].into_iter().collect(),
        added: BTreeMap::new(),
        faces: vec![faces0],
        avoid_stacking: true,
    };
    let cells = [npos];
    for port in 1..ag.tag.arity() {
        let g = ag.face_of(port);
        let (a_end, b_end) = (PEnd::Agent { k: 0, port }, PEnd::Half { cell: apos, he: g });
        let routed = route_wire(&mut st, &cells, a_end, b_end, &board) || {
            st.avoid_stacking = false;
            let r = route_wire(&mut st, &cells, a_end, b_end, &board);
            st.avoid_stacking = true;
            r
        };
        if !routed { return None; } // the mover waits this tick
    }

    let mut writes: Vec<(Pos, Strand)> = vec![];
    for (p, v) in &st.added { for s in v { writes.push((*p, *s)); } }
    let mut footprint: BTreeSet<Pos> = [apos, npos].into_iter().collect();
    for (p, _) in &writes { footprint.insert(*p); }

    Some(ReelPlan { apos, npos, consumed: mine, new_faces: st.faces[0], writes, footprint })
}

pub fn apply_reel(grid: &mut Grid, plan: &ReelPlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    let Some(Cell::Agent(ag)) = grid.cells.get(&plan.apos).cloned() else { panic!("reel: no agent") };

    guard(plan.apos);
    grid.cells.remove(&plan.apos);
    for (p, s) in &plan.writes { guard(*p); grid.add_strand(*p, *s); }
    guard(plan.npos);
    grid.remove_strand(plan.npos, plan.consumed);
    grid.put_agent(plan.npos, AgentCell {
        tag: ag.tag,
        faces: plan.new_faces,
        sid: ag.sid,
    });
    grid.transport += 1;
}

// =========================================================================================
// POLYMER MOVES: RETRACT + KINK-FLIP
// =========================================================================================

/// Repoint whichever attachment (agent port or strand half) occupies face `from` at `p`
/// to face `to`. The caller has verified `to` is free at `p`.
fn rewire_face(grid: &mut Grid, p: Pos, from: He, to: He) {
    match grid.cells.get_mut(&p) {
        Some(Cell::Agent(a)) => {
            let i = a.port_at(from).unwrap_or_else(|| panic!("no port at {from:?} on {p:?}"));
            a.faces[i] = Some(to);
        }
        Some(Cell::Wire(w)) => {
            let slot = w.strands.iter_mut().flatten().find(|s| s.contains(from))
                .unwrap_or_else(|| panic!("no strand half {from:?} at {p:?}"));
            *slot = if slot.a == from { Strand::new(to, slot.b) } else { Strand::new(slot.a, to) };
        }
        None => panic!("rewire on empty cell {p:?}"),
    }
}

#[derive(Debug)]
pub struct RetractPlan {
    pub c1: Pos,
    pub c2: Pos,
    pub f1: Pos,
    pub f2: Pos,
    pub s1: Strand,
    pub s2: Strand,
    pub d: Dir,
    pub e: Dir,
    pub footprint: BTreeSet<Pos>,
}

/// RETRACT: a width-1 U-turn annihilates. `c1` holds elbow (e, d), its d-neighbor `c2`
/// holds the partner elbow (d.opp, e), and the flanks f1 = c1+e, f2 = c2+e reconnect
/// across their (necessarily adjacent) shared edge. Wire length −2; projects to identity.
///
///      f1  f2            f1─f2
///       │   │      ⇒
///      c1 ─ c2            ·  ·
pub fn plan_retract(grid: &Grid, c1: Pos) -> Option<RetractPlan> {
    let w1 = grid.wire(c1)?;
    for s1 in w1.iter() {
        for (e, d) in [(s1.a, s1.b), (s1.b, s1.a)] {
            if e == d.opp() { continue; }              // straight strand: no elbow
            let c2 = step(c1, d);
            let Some(w2) = grid.wire(c2) else { continue };
            let Some(s2) = w2.with_he(d.opp()) else { continue };
            if s2.other(d.opp()) != e { continue; }    // partner elbow must exit the same side
            let (f1, f2) = (step(c1, e), step(c2, e));
            // The new direct edge f1—f2 must be free on both faces.
            if grid.used_hes(f1).contains(&d) || grid.used_hes(f2).contains(&d.opp()) { continue; }
            return Some(RetractPlan {
                c1, c2, f1, f2, s1, s2, d, e,
                footprint: [c1, c2, f1, f2].into_iter().collect(),
            });
        }
    }
    None
}

pub fn apply_retract(grid: &mut Grid, plan: &RetractPlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    guard(plan.c1); grid.remove_strand(plan.c1, plan.s1);
    guard(plan.c2); grid.remove_strand(plan.c2, plan.s2);
    guard(plan.f1); rewire_face(grid, plan.f1, plan.e.opp(), plan.d);
    guard(plan.f2); rewire_face(grid, plan.f2, plan.e.opp(), plan.d.opp());
}

#[derive(Debug)]
pub struct SlidePlan {
    pub p: Pos,
    pub s: Strand,
    pub na: Pos,
    pub nb: Pos,
    pub na_to: He,
    pub nb_to: He,
    pub strands: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

/// SLIDE: reroute the strand `s` OUT of cell `p` — its wire's chain neighbors na = p+s.a
/// and nb = p+s.b reconnect through the SHORTEST path (bounded BFS, deterministic
/// tie-break) that avoids `p` and every cell in `avoid` (in bilayer, typically over the
/// top through z=1). This is the excluded-volume move: two wires cannot share a cell
/// forever, so the one in a walker's way slides around. The kink-flip of polymer dynamics
/// is its 1-cell route and the straight bulge its 3-cell route; congested neighborhoods
/// get whatever short route exists. Length changes by (route − 1), slack the RETRACT move
/// reclaims later; projects to identity.
///
///      na ─ p ─ nb          na   ·   nb          (side view: the strand now rides the
///                      ⇒     └ ─ ─ ─ ┘            next plane; p is free for the walker)
pub fn plan_slide(grid: &Grid, p: Pos, s: Strand, avoid: &[Pos]) -> Option<SlidePlan> {
    let (na, nb) = (step(p, s.a), step(p, s.b));
    let board = make_board(grid, p, p);
    let blocked = |c: Pos| c == p || c == na || c == nb || avoid.contains(&c);
    // Trails run through EMPTY cells only. This is load-bearing for termination: a slide
    // then always moves a strand out of a shared cell into fresh cells, so total excess
    // occupancy (Σ max(0, strands−1)) strictly decreases and slide churn cannot cycle.
    // A permissive variant (threading through occupied cells) was measured to let the
    // scheduler shuffle strands between wire cells forever.
    // BFS keyed by cell (each trail cell carries exactly one new strand): shortest reroute
    // first, BTreeMap coordinate order as the deterministic tie-break.
    let mut seen: BTreeMap<Pos, (He, Pos)> = BTreeMap::new(); // cell -> (entry face, parent)
    let mut queue: std::collections::VecDeque<Pos> = Default::default();
    for da in DIRS {
        if grid.used_hes(na).contains(&da) { continue; }
        let t0 = step(na, da);
        if blocked(t0) || !board.contains(&t0) || !grid.is_empty(t0) || seen.contains_key(&t0) { continue; }
        seen.insert(t0, (da.opp(), t0)); // self-parent marks a root
        queue.push_back(t0);
    }
    let mut goal: Option<Pos> = None;
    let mut depth = 0usize;
    'bfs: while !queue.is_empty() && depth < 7 {
        depth += 1;
        for _ in 0..queue.len() {
            let cur = queue.pop_front().unwrap();
            let enter = seen[&cur].0;
            // Terminate: connect to nb through a free face.
            if let Some(out) = dir_to(cur, nb) {
                if out != enter && !grid.used_hes(nb).contains(&out.opp()) {
                    goal = Some(cur);
                    break 'bfs;
                }
            }
            for d in DIRS {
                if d == enter { continue; }
                let n = step(cur, d);
                if blocked(n) || !board.contains(&n) || !grid.is_empty(n) || seen.contains_key(&n) { continue; }
                seen.insert(n, (d.opp(), cur));
                queue.push_back(n);
            }
        }
    }
    let mut cur = goal?;
    let mut trail: Vec<(Pos, He)> = vec![(cur, seen[&cur].0)];
    while seen[&cur].1 != cur { cur = seen[&cur].1; trail.push((cur, seen[&cur].0)); }
    trail.reverse();
    // Assemble: strand chain along the trail, final leg into nb.
    let mut strands: Vec<(Pos, Strand)> = vec![];
    for i in 0..trail.len() {
        let (c, enter) = trail[i];
        let out = if i + 1 < trail.len() { dir_to(c, trail[i + 1].0).unwrap() }
                  else { dir_to(c, nb).unwrap() };
        strands.push((c, Strand::new(enter, out)));
    }
    let na_to = dir_to(na, trail[0].0).unwrap();
    let nb_to = dir_to(nb, trail.last().unwrap().0).unwrap();
    let mut footprint: BTreeSet<Pos> = [p, na, nb].into_iter().collect();
    for (c, _) in &strands { footprint.insert(*c); }
    Some(SlidePlan { p, s, na, nb, na_to, nb_to, strands, footprint })
}

pub fn apply_slide(grid: &mut Grid, plan: &SlidePlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    guard(plan.p); grid.remove_strand(plan.p, plan.s);
    guard(plan.na); rewire_face(grid, plan.na, plan.s.a.opp(), plan.na_to);
    guard(plan.nb); rewire_face(grid, plan.nb, plan.s.b.opp(), plan.nb_to);
    for (c, s) in &plan.strands { guard(*c); grid.add_strand(*c, *s); }
}

/// Is this producer a demanded walker blocked by a shared cell on its principal?
/// Returns the blocked cell and the foreign strands there (the CLEAR phase's targets).
pub fn reel_blocked(grid: &Grid, apos: Pos) -> Option<(Pos, Vec<Strand>)> {
    let ag = grid.agent(apos)?;
    if !ag.tag.is_producer() { return None; }
    let d = ag.faces[0]?;
    let npos = step(apos, d);
    let w = grid.wire(npos)?;
    if w.count() <= 1 { return None; }
    let (far_pos, far_port) = grid.trace(apos, d);
    let far = grid.agent(far_pos).expect("trace ends at an agent");
    if far_port != 0 || !far.tag.is_consumer() { return None; }
    let mine = w.with_he(d.opp())?;
    Some((npos, w.iter().filter(|s| *s != mine).collect()))
}
