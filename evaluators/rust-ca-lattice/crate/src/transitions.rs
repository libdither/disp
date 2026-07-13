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

use crate::lattice::{dir_to, manhattan, step, AgentCell, Cell, Dir, Grid, GrowItem, GrowPlan, He, Pos, SeedCell, Strand, DIRS, WIRE_CAP};
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
    /// Direction order for the wire router. Fire uses the standard order; REEL puts the
    /// vertical directions first so aux TRAILS ride the overflow plane instead of
    /// cluttering the reaction plane (trails were measured to be what makes fixed fire
    /// layouts unfittable and what forces search fires into stacking).
    dirs: [Dir; 6],
}

const DIRS_STD: [Dir; 6] = [Dir::N, Dir::E, Dir::S, Dir::W, Dir::U, Dir::D];
/// Trails sink into the basement: on full 3D, z<0 is uncontested space (fires spread
/// in-plane and detours prefer upward), so reel trails routed downward stay out of
/// everyone's way. Bilayer has no basement, so it keeps the standard order. A blanket
/// up-first order was measured to HURT (trails then contend with the overflow plane that
/// detours and fires need).
const DIRS_DOWN_FIRST: [Dir; 6] = [Dir::D, Dir::N, Dir::E, Dir::S, Dir::W, Dir::U];

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
    if !cons.tag.is_consumer() || cons.nascent { return None; }
    let d0 = cons.faces[0]?;
    let ppos = step(cpos, d0);
    let prod = grid.agent(ppos)?;
    if !prod.tag.is_producer() || prod.nascent { return None; }
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
        if grid.reserved.contains_key(&p) { outside.insert(p, grid.used_hes(p)); continue; }
        match grid.cells.get(&p) {
            None => { placeable.insert(p); wireable.insert(p, (0, vec![])); }
            Some(Cell::Wire(w)) => { wireable.insert(p, (w.count(), w.used_hes())); }
            Some(Cell::Agent(a)) => { outside.insert(p, a.used_hes()); }
            Some(Cell::Seed(s)) => { outside.insert(p, s.faces.iter().flatten().copied().collect()); }
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
        dirs: DIRS_STD,
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
            for d in st.dirs {
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
    for d in st.dirs {
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

// -----------------------------------------------------------------------------------------
// STAMP FIRE: the same layout problem plan_fire solves, but solved ONCE per
// (rule, dock axis, anchor faces, topology, plane) on an EMPTY synthetic neighborhood — a
// Platonic workshop — with the anchors' far cells blocked (in reality the external wires
// live there). The cached result is then applied to the live lattice as a FIXED PATTERN:
// about a dozen freeness reads, no search, no router. Two structural consequences:
// (a) fire enabledness becomes purely local (read the fixed cells, stamp or wait), and
// (b) templates are always stack-free (pass-1 on an empty board never shares a cell), so
// stamp fires cannot mint the shared-cell walker-blockers that search-mode fires create
// when their router falls back to stacking. Ports make this possible: faces are dynamic
// per agent, so the template is keyed by the ACTUAL anchor faces at dock time and needs
// no rotation or adaptation at apply time.
// -----------------------------------------------------------------------------------------

#[derive(Clone)]
struct FireTemplate {
    fresh_cells: Vec<Pos>,
    fresh_faces: Vec<[Option<He>; 3]>,
    strands: Vec<(Pos, Strand)>,
}
type StampKey = (Tag, Tag, Dir, [Option<He>; 3], [Option<He>; 3], i32, bool);
static STAMPS: std::sync::OnceLock<std::sync::Mutex<BTreeMap<StampKey, Option<FireTemplate>>>> =
    std::sync::OnceLock::new();

/// Solve the rule's layout in the empty workshop: the pair at the origin with its REAL
/// faces, anchor far-cells blocked by inert markers, everything else free.
fn synth_template(topo: crate::lattice::Topo, cons: &AgentCell, prod: &AgentCell, d0: Dir, zs: i32) -> Option<FireTemplate> {
    let a = (0, 0, zs);
    let b = step(a, d0);
    let mut g = Grid::new(topo);
    g.cells.insert(a, Cell::Agent(AgentCell { tag: cons.tag, faces: cons.faces, sid: 0, nascent: false }));
    g.cells.insert(b, Cell::Agent(AgentCell { tag: prod.tag, faces: prod.faces, sid: 1, nascent: false }));
    let mut blk = 10u32;
    for (cell, ag) in [(a, cons), (b, prod)] {
        for i in 1..ag.tag.arity() {
            let far = step(cell, ag.face_of(i));
            let mut faces = [None; 3];
            faces[0] = Some(ag.face_of(i).opp());
            g.cells.insert(far, Cell::Agent(AgentCell { tag: Tag::Out, faces, sid: blk, nascent: false }));
            blk += 1;
        }
    }
    let plan = plan_fire(&g, a)?;
    Some(FireTemplate { fresh_cells: plan.fresh_cells, fresh_faces: plan.fresh_faces, strands: plan.strands })
}

/// The stamp planner. Same enabledness as `plan_fire`; the layout comes from the cached
/// workshop template, offset to the live pair, and validated by freeness reads only.
pub fn plan_fire_stamp(grid: &Grid, cpos: Pos) -> Option<FirePlan> {
    let cons = grid.agent(cpos)?;
    if !cons.tag.is_consumer() || cons.nascent { return None; }
    let d0 = cons.faces[0]?;
    let ppos = step(cpos, d0);
    let prod = grid.agent(ppos)?;
    if !prod.tag.is_producer() || prod.nascent { return None; }
    if prod.faces[0] != Some(d0.opp()) { return None; }
    let bilayer = matches!(grid.topo, crate::lattice::Topo::Bilayer);
    let zs = if bilayer { cpos.2 } else { 0 };
    let key: StampKey = (cons.tag, prod.tag, d0, cons.faces, prod.faces, zs, bilayer);
    let tpl = {
        let m = STAMPS.get_or_init(|| std::sync::Mutex::new(BTreeMap::new()));
        let mut m = m.lock().unwrap();
        m.entry(key).or_insert_with(|| synth_template(grid.topo, cons, prod, d0, zs)).clone()
    }?;
    let off = (cpos.0, cpos.1, cpos.2 - zs);
    let mv = |p: Pos| (p.0 + off.0, p.1 + off.1, p.2 + off.2);

    let rule = find(cons.tag, prod.tag).unwrap();
    let mut footprint: BTreeSet<Pos> = [cpos, ppos].into_iter().collect();
    let mut fresh_cells = Vec::with_capacity(tpl.fresh_cells.len());
    for p in &tpl.fresh_cells {
        let q = mv(*p);
        if q != cpos && q != ppos && !grid.is_open(q) { return None; }
        footprint.insert(q);
        fresh_cells.push(q);
    }
    let mut strands = Vec::with_capacity(tpl.strands.len());
    for (p, s) in &tpl.strands {
        let q = mv(*p);
        if q != cpos && q != ppos && !grid.is_open(q) { return None; }
        footprint.insert(q);
        strands.push((q, *s));
    }
    Some(FirePlan {
        cpos, ppos,
        csid: cons.sid, psid: prod.sid,
        rule,
        fresh_cells,
        fresh_faces: tpl.fresh_faces.clone(),
        strands,
        footprint,
    })
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
            nascent: false,
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
    /// Truncation re-anchors ("walks eat slack"): aux-wire strands REMOVED because the
    /// wire's next chain cell already sits adjacent to the mover's new seat.
    pub removes: Vec<(Pos, Strand)>,
    /// (cell, from-face, to-face): the surviving neighbor half repointed at the mover.
    pub repoints: Vec<(Pos, He, He)>,
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
/// fire-awaits gate). Each aux wire is re-anchored TRUNCATION-FIRST ("walks eat slack"):
/// when the wire's next chain cell already sits adjacent to the mover's new seat with the
/// meeting edge free, the vacated-side strand is deleted and the neighbor half repointed
/// straight at the mover — net wire length FALLS by one where the extension bend would
/// grow it by one, so a walker consumes its own drag staircase instead of paving the
/// lattice with trail mats (the measured mat source). Otherwise the SAME bounded router
/// fire uses extends the wire: from its arrival face at the vacated cell to any free face
/// of the mover — the straight-behind bend and the corner detour are its 1-cell and
/// 3-cell routes.
pub fn plan_reel(grid: &Grid, apos: Pos) -> Option<ReelPlan> {
    let ag = grid.agent(apos)?;
    if !ag.tag.is_producer() || ag.nascent { return None; }
    let d = ag.faces[0]?;
    let npos = step(apos, d);
    if grid.reserved.contains_key(&npos) { return None; } // a seed claimed it: park, the strand will be slid
    let w = grid.wire(npos)?; // agent-adjacent is fire's business; empty is impossible (parity)
    let mine = w.with_he(d.opp())?;
    // ψ replaces the whole-wire trace: walk only when the adjacent strand of MY principal
    // carries demand (heat pumped by the far consumer's principal, propagated one cell
    // per tick). A single neighbor-local bit read.
    if !mine.hot { return None; }
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
        if grid.reserved.contains_key(&p) { outside.insert(p, grid.used_hes(p)); continue; }
        match grid.cells.get(&p) {
            None => { wireable.insert(p, (0, vec![])); }
            Some(Cell::Wire(wc)) => { wireable.insert(p, (wc.count(), wc.used_hes())); }
            Some(Cell::Agent(a)) => { outside.insert(p, a.used_hes()); }
            Some(Cell::Seed(s)) => { outside.insert(p, s.faces.iter().flatten().copied().collect()); }
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
        dirs: match grid.topo { crate::lattice::Topo::Full3D => DIRS_DOWN_FIRST, _ => DIRS_STD },
    };
    let cells = [npos];
    let mut removes: Vec<(Pos, Strand)> = vec![];
    let mut repoints: Vec<(Pos, He, He)> = vec![];
    for port in 1..ag.tag.arity() {
        let g = ag.face_of(port);
        // Truncation first: walk the aux chain apos —g— c1 —e1— c2 —e2— … a few cells; at
        // the FIRST chain cell whose far continuation u sits adjacent to the new seat with
        // both meeting faces free in PLAN state (earlier ports may have claimed them),
        // drop every strand walked over and repoint u's half at the mover. Re-anchoring
        // then needs ZERO new wire — which is exactly what survives in a matted board
        // where the extension router has nowhere to lay a detour.
        let trunc = (|| -> Option<(Vec<(Pos, Strand)>, Pos, He, He)> {
            let mut chain: Vec<(Pos, Strand)> = vec![];
            let (mut cell, mut face) = (apos, g);
            for _ in 0..3 {
                let t = step(cell, face);
                if !board.contains(&t) || grid.reserved.contains_key(&t) { return None; }
                let s_t = grid.wire(t)?.with_he(face.opp())?;
                let e = s_t.other(face.opp());
                let u = step(t, e);
                // u == npos is impossible while npos holds only `mine` (parity);
                // u == apos is a self-loop back into the mover — leave it alone.
                if u == npos || u == apos { return None; }
                chain.push((t, s_t));
                let u_in_chain = chain.iter().any(|(c, _)| *c == u);
                if !u_in_chain && !grid.reserved.contains_key(&u)
                    && !matches!(grid.cells.get(&u), Some(Cell::Seed(_)))
                    && grid.agent(u).is_none_or(|a| !a.nascent)
                {
                    if let Some(to) = dir_to(u, npos) {
                        if st.face_free(0, to.opp()) && !st.hes_at(u).contains(&to) {
                            return Some((chain, u, e.opp(), to));
                        }
                    }
                }
                (cell, face) = (t, e);
            }
            None
        })();
        if let Some((chain, u, from, to)) = trunc {
            removes.extend(chain);
            repoints.push((u, from, to));
            st.faces[0][port] = Some(to.opp());
            // publish the claimed u-face into the plan state so later ports see it
            if let Some((_, base)) = st.wireable.get_mut(&u) { base.push(to); }
            else if let Some(out) = st.outside.get_mut(&u) { out.push(to); }
            continue;
        }
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
    for (p, _) in &removes { footprint.insert(*p); }
    for (p, _, _) in &repoints { footprint.insert(*p); }

    Some(ReelPlan { apos, npos, consumed: mine, new_faces: st.faces[0], writes, removes, repoints, footprint })
}

pub fn apply_reel(grid: &mut Grid, plan: &ReelPlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    let Some(Cell::Agent(ag)) = grid.cells.get(&plan.apos).cloned() else { panic!("reel: no agent") };

    guard(plan.apos);
    grid.cells.remove(&plan.apos);
    for (p, s) in &plan.removes { guard(*p); grid.remove_strand(*p, *s); }
    for (p, s) in &plan.writes { guard(*p); grid.add_strand(*p, *s); }
    for (p, from, to) in &plan.repoints { guard(*p); rewire_face(grid, *p, *from, *to); }
    guard(plan.npos);
    grid.remove_strand(plan.npos, plan.consumed);
    grid.put_agent(plan.npos, AgentCell {
        tag: ag.tag,
        faces: plan.new_faces,
        sid: ag.sid,
        nascent: false,
    });
    grid.transport += 1;
}

// =========================================================================================
// SHOVE: the χ-driven agent step — the mover for PARKED agents, which neither fires nor
// reels nor slides can touch. An agent on high pressure steps to its lowest-pressure open
// neighbor (steepest descent, fixed tie order, hysteresis so it cannot dither), and every
// wire re-anchors through the same bounded router reel uses; nothing is consumed, so the
// move projects to identity. Hot walkers are immune (they are delivering — demand
// outranks pressure, and immunity kills the reel-forward/shove-back livelock). Docked
// pairs are immune (never separate an enabled fire). Out is immune (the pad is fixed).
// =========================================================================================

#[derive(Debug)]
pub struct ShovePlan {
    pub apos: Pos,
    pub npos: Pos,
    pub new_faces: [Option<He>; 3],
    pub writes: Vec<(Pos, Strand)>,
    pub footprint: BTreeSet<Pos>,
}

pub fn plan_shove(grid: &Grid, apos: Pos) -> Option<ShovePlan> {
    let ag = grid.agent(apos)?;
    if ag.nascent || ag.tag == Tag::Out { return None; }
    let here = grid.chi_at(apos);
    if here < 4 { return None; }
    if let Some(d0) = ag.faces[0] {
        let q = step(apos, d0);
        // docked pair: leave it to fire
        if let Some(b) = grid.agent(q) {
            if b.faces[0] == Some(d0.opp())
                && (ag.tag.is_consumer() && b.tag.is_producer()
                    || ag.tag.is_producer() && b.tag.is_consumer()) { return None; }
        }
        // hot walker: demand outranks pressure
        if let Some(w) = grid.wire(q) {
            if let Some(m) = w.with_he(d0.opp()) { if m.hot { return None; } }
        }
    }
    // steepest descent into an open cell
    let mut best: Option<(u8, Pos)> = None;
    for d in DIRS {
        let n = step(apos, d);
        if !grid.is_open(n) { continue; }
        let cn = grid.chi_at(n);
        if best.is_none_or(|(bc, _)| cn < bc) { best = Some((cn, n)); }
    }
    let (cn, npos) = best?;
    if here < cn.saturating_add(2) { return None; }

    // Re-anchor EVERY port from its face at the vacated cell to the agent's new seat.
    let board = make_board(grid, apos, npos);
    let mut wireable: BTreeMap<Pos, (usize, Vec<He>)> = BTreeMap::new();
    let mut outside: BTreeMap<Pos, Vec<He>> = BTreeMap::new();
    for &p in &board {
        if p == npos { continue; }
        if p == apos { wireable.insert(p, (0, vec![])); continue; }
        if grid.reserved.contains_key(&p) { outside.insert(p, grid.used_hes(p)); continue; }
        match grid.cells.get(&p) {
            None => { wireable.insert(p, (0, vec![])); }
            Some(Cell::Wire(wc)) => { wireable.insert(p, (wc.count(), wc.used_hes())); }
            Some(Cell::Agent(a)) => { outside.insert(p, a.used_hes()); }
            Some(Cell::Seed(s)) => { outside.insert(p, s.faces.iter().flatten().copied().collect()); }
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
    let mut st = BoardState {
        placeable: BTreeSet::new(),
        wireable,
        outside,
        agent_at: [(npos, 0usize)].into_iter().collect(),
        added: BTreeMap::new(),
        faces: vec![[None; 3]],
        avoid_stacking: true,
        dirs: DIRS_STD,
    };
    let cells = [npos];
    for port in 0..ag.tag.arity() {
        let (a_end, b_end) = (PEnd::Agent { k: 0, port }, PEnd::Half { cell: apos, he: ag.face_of(port) });
        let routed = route_wire(&mut st, &cells, a_end, b_end, &board) || {
            st.avoid_stacking = false;
            let r = route_wire(&mut st, &cells, a_end, b_end, &board);
            st.avoid_stacking = true;
            r
        };
        if !routed { return None; }
    }
    let mut writes: Vec<(Pos, Strand)> = vec![];
    for (p, v) in &st.added { for s in v { writes.push((*p, *s)); } }
    let mut footprint: BTreeSet<Pos> = [apos, npos].into_iter().collect();
    for (p, _) in &writes { footprint.insert(*p); }
    Some(ShovePlan { apos, npos, new_faces: st.faces[0], writes, footprint })
}

pub fn apply_shove(grid: &mut Grid, plan: &ShovePlan) {
    let guard = |p: Pos| assert!(plan.footprint.contains(&p), "write outside footprint: {p:?}");
    let Some(Cell::Agent(ag)) = grid.cells.get(&plan.apos).cloned() else { panic!("shove: no agent") };
    guard(plan.apos);
    grid.cells.remove(&plan.apos);
    for (p, s) in &plan.writes { guard(*p); grid.add_strand(*p, *s); }
    guard(plan.npos);
    grid.put_agent(plan.npos, AgentCell {
        tag: ag.tag,
        faces: plan.new_faces,
        sid: ag.sid,
        nascent: false,
    });
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
        Some(Cell::Seed(_)) => panic!("rewire on a seed cell {p:?}"),
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
/// ```text
///      f1  f2            f1─f2
///       │   │      ⇒
///      c1 ─ c2            ·  ·
/// ```
pub fn plan_retract(grid: &Grid, c1: Pos) -> Option<RetractPlan> {
    let w1 = grid.wire(c1)?;
    // Seed territory is untouchable: reserved cells hold half-wired template strands whose
    // geometry LOOKS retractable but whose parity is pending.
    let shielded = |p: Pos| grid.reserved.contains_key(&p)
        || matches!(grid.cells.get(&p), Some(Cell::Seed(_)));
    if shielded(c1) { return None; }
    for s1 in w1.iter() {
        for (e, d) in [(s1.a, s1.b), (s1.b, s1.a)] {
            if e == d.opp() { continue; }              // straight strand: no elbow
            let c2 = step(c1, d);
            let Some(w2) = grid.wire(c2) else { continue };
            let Some(s2) = w2.with_he(d.opp()) else { continue };
            if s2.other(d.opp()) != e { continue; }    // partner elbow must exit the same side
            if shielded(c2) { continue; }
            let (f1, f2) = (step(c1, e), step(c2, e));
            // A seed's anchor faces are frozen and its territory shielded: never rewire a
            // flank that is a seed cell or reserved.
            if shielded(f1) || shielded(f2) { continue; }
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
/// ```text
///      na ─ p ─ nb          na   ·   nb          (side view: the strand now rides the
///                      ⇒     └ ─ ─ ─ ┘            next plane; p is free for the walker)
/// ```
pub fn plan_slide(grid: &Grid, p: Pos, s: Strand, avoid: &[Pos], from_seed: bool) -> Option<SlidePlan> {
    plan_slide_ex(grid, p, s, avoid, from_seed, false)
}

/// `relax`: permits a SECOND pass threading count-1 wire cells (short trails only) when
/// no empty route exists — for demanded clearings in wire mats. Ambient decongestion must
/// NOT relax (measured: it shuffles strands forever).
pub fn plan_slide_ex(grid: &Grid, p: Pos, s: Strand, avoid: &[Pos], from_seed: bool, relax: bool) -> Option<SlidePlan> {
    let (na, nb) = (step(p, s.a), step(p, s.b));
    // Seed territory is untouchable from outside; the owning seed may slide SQUATTERS out
    // of its own reserved cells (pre-dock wires), whose parity is real.
    if !from_seed && grid.reserved.contains_key(&p) { return None; }
    let shielded = |q: Pos| grid.reserved.contains_key(&q)
        || matches!(grid.cells.get(&q), Some(Cell::Seed(_)));
    if shielded(na) || shielded(nb) { return None; }
    let board = make_board(grid, p, p);
    let blocked = |c: Pos| c == p || c == na || c == nb || avoid.contains(&c);
    // Trails prefer EMPTY cells (strictly decongesting: total excess occupancy falls, so
    // churn cannot cycle). When the region is a MAT of count-1 wire cells — no empty
    // roots anywhere, the measured chain-knot class — a second RELAXED pass may thread
    // through count-1 wire cells whose faces fit. Relaxation trades the monotone measure
    // for reach; call sites are χ- or walker-demanded and the run-level stall grace
    // bounds any residual ping-pong.
    let mut goal: Option<Pos> = None;
    let mut seen: BTreeMap<Pos, (He, Pos)> = BTreeMap::new(); // cell -> (entry face, parent)
    for relaxed in [false, true] {
        if relaxed && !relax { break; }
        // May a trail occupy `c`, entering via face `f_in`?
        let cell_ok = |c: Pos, f_in: He| -> bool {
            if !grid.topo.in_bounds(c) || grid.reserved.contains_key(&c) { return false; }
            match grid.cells.get(&c) {
                None => true,
                Some(Cell::Wire(w)) => relaxed && w.count() == 1 && !w.used_hes().contains(&f_in),
                _ => false,
            }
        };
        let exit_ok = |c: Pos, out: He| -> bool {
            match grid.cells.get(&c) {
                Some(Cell::Wire(w)) => !w.used_hes().contains(&out),
                _ => true,
            }
        };
        seen.clear();
        let mut queue: std::collections::VecDeque<Pos> = Default::default();
        for da in DIRS {
            if grid.used_hes(na).contains(&da) { continue; }
            let t0 = step(na, da);
            if blocked(t0) || !board.contains(&t0) || !cell_ok(t0, da.opp()) || seen.contains_key(&t0) { continue; }
            seen.insert(t0, (da.opp(), t0)); // self-parent marks a root
            queue.push_back(t0);
        }
        let mut depth = 0usize;
        // Both passes stay inside the radius-2 board; the relaxed cap governs how deep a
        // DEMANDED clearing may thread a mat (measured on the selF knot: 4 dies one hop
        // short of the empty plane above; ambient moves never take this pass).
        let cap = if relaxed { 6 } else { 7 };
        'bfs: while !queue.is_empty() && depth < cap {
            depth += 1;
            for _ in 0..queue.len() {
                let cur = queue.pop_front().unwrap();
                let enter = seen[&cur].0;
                // Terminate: connect to nb through a free face.
                if let Some(out) = dir_to(cur, nb) {
                    if out != enter && exit_ok(cur, out) && !grid.used_hes(nb).contains(&out.opp()) {
                        goal = Some(cur);
                        break 'bfs;
                    }
                }
                for d in DIRS {
                    if d == enter { continue; }
                    let n = step(cur, d);
                    if blocked(n) || !board.contains(&n) || seen.contains_key(&n) { continue; }
                    if !exit_ok(cur, d) || !cell_ok(n, d.opp()) { continue; }
                    seen.insert(n, (d.opp(), cur));
                    queue.push_back(n);
                }
            }
        }
        if goal.is_some() { break; }
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
    if !ag.tag.is_producer() || ag.nascent { return None; }
    let d = ag.faces[0]?;
    let npos = step(apos, d);
    if grid.reserved.contains_key(&npos) { return None; } // seed territory: park
    let w = grid.wire(npos)?;
    if w.count() <= 1 { return None; }
    let mine = w.with_he(d.opp())?;
    if !mine.hot { return None; } // ψ: only demanded walkers ask for clearing
    Some((npos, w.iter().filter(|s| *s != mine).collect()))
}

// =========================================================================================
// GROW FIRE: dock-and-extrude. Where the atomic stamp requires its whole layout free NOW,
// the grow dock RESERVES the layout's cells (free or not) and unfolds one cell per tick as
// they open: reservations keep everyone else out (so claimed cells only get freer), squat-
// ting wires are slid out on demand, and the two dying cells finalize last (the driver
// replaces itself at the end). The shadow fires at DOCK — the linearization point — so
// mid-unfold lattice states are checked at seed-free quiescence, and nascent products are
// fenced (no fires, no reels) until their complex completes.
// =========================================================================================

#[derive(Debug)]
pub struct DockPlan {
    pub cpos: Pos,
    pub ppos: Pos,
    pub rule: &'static Rule,
    pub cfaces: [Option<He>; 3],
    pub pfaces: [Option<He>; 3],
    pub items: Vec<GrowItem>,
    pub fresh_tags: Vec<Tag>,
    pub fresh_faces: Vec<[Option<He>; 3]>,
    pub cells: Vec<Pos>,
}

/// How many consecutive ticks a seed may WAIT before it aborts: emitted geometry is
/// deleted (it is shielded, so nothing external references it), the pair is restored, and
/// other planners get their chance. Possible only because the shadow fires at COMPLETION,
/// not at dock — mid-unfold products are pure geometry.
const SEED_PATIENCE: u32 = 24;

/// Dockability: the same template the stamp uses, but the outside cells need only be
/// UNCLAIMED and agent-free — occupied wire cells are acceptable squatters (the unfold
/// slides them out); agents are not (they only move by reeling, on their own schedule).
pub fn plan_grow_dock(grid: &Grid, cpos: Pos) -> Option<DockPlan> {
    let cons = grid.agent(cpos)?;
    if !cons.tag.is_consumer() || cons.nascent { return None; }
    let d0 = cons.faces[0]?;
    let ppos = step(cpos, d0);
    let prod = grid.agent(ppos)?;
    if !prod.tag.is_producer() || prod.nascent { return None; }
    if prod.faces[0] != Some(d0.opp()) { return None; }
    let bilayer = matches!(grid.topo, crate::lattice::Topo::Bilayer);
    let zs = if bilayer { cpos.2 } else { 0 };
    let key: StampKey = (cons.tag, prod.tag, d0, cons.faces, prod.faces, zs, bilayer);
    let tpl = {
        let m = STAMPS.get_or_init(|| std::sync::Mutex::new(BTreeMap::new()));
        let mut m = m.lock().unwrap();
        m.entry(key).or_insert_with(|| synth_template(grid.topo, cons, prod, d0, zs)).clone()
    }?;
    let off = (cpos.0, cpos.1, cpos.2 - zs);
    let mv = |p: Pos| (p.0 + off.0, p.1 + off.1, p.2 + off.2);
    let rule = find(cons.tag, prod.tag).unwrap();

    // Outside cells: reservable iff in bounds and not claimed by another seed or a seed
    // cell itself. Squatters of every kind are allowed: wires get slid by the unfold,
    // agents get pushed by χ+SHOVE (the reservation itself is a pressure source while
    // occupied), and the patience/abort valve bounds the worst case.
    let mut cells: Vec<Pos> = vec![];
    let mut outside_ok = |q: Pos| -> bool {
        if q == cpos || q == ppos { return true; }
        if !grid.topo.in_bounds(q) || grid.reserved.contains_key(&q) { return false; }
        if matches!(grid.cells.get(&q), Some(Cell::Seed(_))) { return false; }
        if !cells.contains(&q) { cells.push(q); }
        true
    };
    for p in &tpl.fresh_cells { if !outside_ok(mv(*p)) { return None; } }
    for (p, _) in &tpl.strands { if !outside_ok(mv(*p)) { return None; } }

    // Items: outside agents, outside strand-cells, then the pair's finals (B before A).
    let mut items: Vec<GrowItem> = vec![];
    let mut by_cell: BTreeMap<Pos, Vec<Strand>> = BTreeMap::new();
    for (p, s) in &tpl.strands { by_cell.entry(mv(*p)).or_default().push(*s); }
    for (k, p) in tpl.fresh_cells.iter().enumerate() {
        let q = mv(*p);
        if q != cpos && q != ppos { items.push(GrowItem::Agent { cell: q, k }); }
    }
    for (cell, strands) in &by_cell {
        if *cell != cpos && *cell != ppos { items.push(GrowItem::Strands { cell: *cell, strands: strands.clone() }); }
    }
    for final_cell in [ppos, cpos] {
        if let Some(k) = tpl.fresh_cells.iter().position(|p| mv(*p) == final_cell) {
            items.push(GrowItem::Agent { cell: final_cell, k });
        } else {
            items.push(GrowItem::Strands {
                cell: final_cell,
                strands: by_cell.get(&final_cell).cloned().unwrap_or_default(),
            });
        }
    }
    Some(DockPlan {
        cpos, ppos, rule,
        cfaces: cons.faces, pfaces: prod.faces,
        items,
        fresh_tags: rule.fresh.to_vec(),
        fresh_faces: tpl.fresh_faces.clone(),
        cells,
    })
}

/// Reserve the layout and become seeds. The shadow is NOT fired here: the linearization
/// point is COMPLETION (the driver's final step), which is what makes seeds abortable.
pub fn apply_grow_dock(grid: &mut Grid, plan: DockPlan) {
    let ca = grid.agent(plan.cpos).unwrap();
    let pa = grid.agent(plan.ppos).unwrap();
    let (csid, psid) = (ca.sid, pa.sid);
    let (ctag, ptag) = (ca.tag, pa.tag);
    for c in &plan.cells { grid.reserved.insert(*c, plan.cpos); }
    let gp = std::rc::Rc::new(GrowPlan {
        rule: plan.rule,
        a: plan.cpos, b: plan.ppos,
        items: plan.items,
        fresh_tags: plan.fresh_tags,
        fresh_faces: plan.fresh_faces,
        fresh_sids: vec![csid, psid],   // the PAIR's sids until completion assigns products
        cells: plan.cells,
    });
    grid.cells.insert(plan.cpos, Cell::Seed(SeedCell { plan: gp.clone(), step: 0, driver: true, faces: plan.cfaces, tag: ctag, stall: 0 }));
    grid.cells.insert(plan.ppos, Cell::Seed(SeedCell { plan: gp, step: 0, driver: false, faces: plan.pfaces, tag: ptag, stall: 0 }));
    grid.seed_count += 1;
}

pub enum GrowStep { Placed(Pos), Slid(Pos), Waiting(Pos), Aborted }

/// One unfold step for the seed driven at `driver`. Emits the next item if its cell is
/// open, slides a squatting strand out of the way if not, waits briefly, and ABORTS after
/// SEED_PATIENCE consecutive waits (delete the shielded emissions, restore the pair).
/// The final step fires the shadow: the linearization point.
pub fn grow_step(grid: &mut Grid, shadow: &mut Net, driver: Pos) -> Option<GrowStep> {
    let (plan, step) = match grid.cells.get(&driver) {
        Some(Cell::Seed(s)) if s.driver => (s.plan.clone(), s.step),
        _ => return None,
    };
    let item = plan.items.get(step).expect("driver outlived its plan");
    let (csid, psid) = (plan.fresh_sids[0], plan.fresh_sids[1]);
    let (cell, write): (Pos, Box<dyn FnOnce(&mut Grid)>) = match item {
        GrowItem::Agent { cell, k } => {
            let (cell, k) = (*cell, *k);
            let (tag, faces) = (plan.fresh_tags[k], plan.fresh_faces[k]);
            (cell, Box::new(move |g: &mut Grid| {
                // sid is a placeholder until completion fires the shadow
                g.put_agent(cell, AgentCell { tag, faces, sid: u32::MAX, nascent: true });
            }))
        }
        GrowItem::Strands { cell, strands } => {
            let (cell, strands) = (*cell, strands.clone());
            (cell, Box::new(move |g: &mut Grid| {
                for s in strands { g.add_strand(cell, s); }
            }))
        }
    };
    let is_final = cell == plan.a || cell == plan.b;
    if is_final {
        // our own seed cell: replace it with its final content
        grid.cells.remove(&cell);
        write(grid);
        if cell == plan.a {
            // the driver just replaced itself: the unfold is complete. Fire the shadow
            // (the linearization point) and hand every product its real sid.
            let (rule, fresh_sids) = shadow.fire(csid, psid);
            assert!(std::ptr::eq(rule, plan.rule), "shadow fired a different rule");
            for it in &plan.items {
                if let GrowItem::Agent { cell, k } = it {
                    if let Some(Cell::Agent(ag)) = grid.cells.get_mut(cell) {
                        ag.sid = fresh_sids[*k];
                        ag.nascent = false;
                    }
                }
            }
            for c in plan.cells.iter().chain([&plan.a, &plan.b]) {
                grid.reserved.remove(c);
            }
            grid.seed_count -= 1;
        } else if let Some(Cell::Seed(s)) = grid.cells.get_mut(&driver) {
            s.step = step + 1;
        }
        return Some(GrowStep::Placed(cell));
    }
    if grid.is_empty(cell) {
        write(grid);
        // the reservation STAYS until finalize: it shields the emitted (still dangling)
        // template content from retract/slide/clear until the whole complex is wired
        if let Some(Cell::Seed(s)) = grid.cells.get_mut(&driver) { s.step = step + 1; s.stall = 0; }
        return Some(GrowStep::Placed(cell));
    }
    // A squatter: wires get slid out (reservations stop new ones arriving); anything else
    // waits (agents move only by reeling).
    if let Some(w) = grid.wire(cell) {
        for s in w.iter().collect::<Vec<_>>() {
            if let Some(sl) = plan_slide(grid, cell, s, &[], true) {
                apply_slide(grid, &sl);
                if let Some(Cell::Seed(sc)) = grid.cells.get_mut(&driver) { sc.stall = 0; }
                return Some(GrowStep::Slid(cell));
            }
        }
    }
    // Stall bookkeeping and the abort valve.
    let stall = {
        let Some(Cell::Seed(sc)) = grid.cells.get_mut(&driver) else { unreachable!() };
        sc.stall += 1;
        sc.stall
    };
    if stall > SEED_PATIENCE {
        // Delete every emission so far (all shielded: nothing external references them),
        // restore the original pair, release the claims. The pair is enabled again and
        // other planners get their chance.
        let (ctag, cfaces) = match grid.cells.get(&plan.a) {
            Some(Cell::Seed(s)) => (s.tag, s.faces), _ => unreachable!(),
        };
        let (ptag, pfaces) = match grid.cells.get(&plan.b) {
            Some(Cell::Seed(s)) => (s.tag, s.faces), _ => unreachable!(),
        };
        for it in &plan.items[..step] {
            match it {
                GrowItem::Agent { cell, .. } => { grid.cells.remove(cell); }
                GrowItem::Strands { cell, strands } => {
                    for s in strands { grid.remove_strand(*cell, *s); }
                }
            }
        }
        for c in plan.cells.iter().chain([&plan.a, &plan.b]) { grid.reserved.remove(c); }
        grid.cells.insert(plan.a, Cell::Agent(AgentCell { tag: ctag, faces: cfaces, sid: csid, nascent: false }));
        grid.cells.insert(plan.b, Cell::Agent(AgentCell { tag: ptag, faces: pfaces, sid: psid, nascent: false }));
        grid.seed_count -= 1;
        return Some(GrowStep::Aborted);
    }
    Some(GrowStep::Waiting(cell))
}
