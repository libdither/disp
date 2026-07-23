//! Blocklet compiler: one small fixed patch per rule, grown at runtime by the builder
//! cursor. This replaces the searched workshop ROM. The construction is deterministic:
//! fresh agents sit on a comb north of the seed axis, every port hands over to a fixed
//! start cell, and each rule wire is BFS-routed once through a small box at first use.
//! Boundary splices are not part of the patch: the seed cells resolve into patch-panel
//! routes from the dying agents' runtime stub faces to fixed canonical exits.
//!
//! Canonical frame: consumer seed at (0,0,0), producer seed at (1,0,0), dock axis = E.
//! Runtime rotation is `cascade::rot_*` with (axis, roll).

use crate::cascade::{exposures, rot_endpt, Cell, EndPt, Route, Site, Word2};
use crate::lattice::{Dir, DIRS};
use crate::lattice::{dir_to, step, Pos};
use crate::rules::{End, RULES};
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::sync::OnceLock;

#[derive(Clone, Debug)]
pub enum Op {
    /// Reserve then write `cell` (canonical frame) one face away from the cursor.
    Place { dir: Dir, cell: Cell },
    /// Move the cursor onto an already-placed blocklet cell; `finalize` clears the
    /// nursery flag of the cell being left (its last visit).
    Hop { dir: Dir, finalize: bool },
}

#[derive(Clone, Debug)]
pub struct Layout {
    /// Canonical offsets and final matter of every non-seed blocklet cell.
    pub extras: Vec<(Pos, Cell)>,
    pub script: Vec<Op>,
    /// The pc at which the cursor, back on the consumer seed, resolves the fire. Ops
    /// before it place; ops after it are the finalize pass over the placed cells.
    pub resolve_pc: u16,
    /// Fresh-agent seats: (index into rule.fresh, canonical offset).
    pub seats: Vec<(usize, Pos)>,
    /// Canonical exit endpoint on the consumer seed for aux i+1 (None if not routed).
    pub c_exits: Vec<Option<EndPt>>,
    /// Same for the producer seed.
    pub p_exits: Vec<Option<EndPt>>,
    /// Direct consumer-aux to producer-aux wires carried on the seed-seed edge; the
    /// vector index is the edge lane (aux indices are 1-based).
    pub cp_wires: Vec<(u8, u8)>,
    /// When the whole rule fits inside the two seed cells, the one-transaction
    /// resolution; the scripted layout above stays as the fallback for fire states
    /// where the seated finals do not pack (inherited passthrough overflow).
    pub seated: Option<Seated>,
}

/// One endpoint source inside a seated resolution, resolved to a concrete (face, lane)
/// at fire time from the dying pair's runtime stubs and the freed dock edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EndSrc {
    /// The consumer's aux stub i (1-based).
    CStub(u8),
    /// The producer's aux stub j (1-based).
    PStub(u8),
    /// The seed-seed dock edge, lane l: faces the axis from the consumer cell and its
    /// opposite from the producer cell (both lanes are free once the principals died).
    Axis(u8),
}

/// A whole-rule embedding inside the two seed cells: fresh agents seated in the dying
/// pair's own cells with ports sourced from the stubs and the dock edge; leftover
/// connections become panel routes (or passthroughs under a seated agent). No extras,
/// no script, one dock-and-resolve transaction.
#[derive(Clone, Debug)]
pub struct Seated {
    /// Fresh agent in the consumer cell: (index into rule.fresh, per-port sources).
    pub c_agent: Option<(usize, Vec<EndSrc>)>,
    pub p_agent: Option<(usize, Vec<EndSrc>)>,
    /// Routes carried by each cell besides the agent (its passthroughs, or the whole
    /// panel when no agent sits there).
    pub c_routes: Vec<(EndSrc, EndSrc)>,
    pub p_routes: Vec<(EndSrc, EndSrc)>,
}

const SEED_C: Pos = (0, 0, 0);
const SEED_P: Pos = (1, 0, 0);

/// Seats on a spacing-2 comb north of the pair: the near ring above the seeds stays
/// free for the pair's own traffic (near-field seating wedges docks); compactness
/// comes from the packing router instead.
fn seat_pos(slot: usize) -> Pos { (2 + 2 * slot as i32, -2, 0) }
fn tail_pos(slot: usize) -> Pos { (2 + 2 * slot as i32, -3, 0) }

/// Assign fresh agents to comb slots: agents with the most wires into the boundary or
/// already-seated agents sit closest to the seed, which keeps routes short.
fn seat_slots(rule: &crate::rules::Rule) -> Vec<usize> {
    let n = rule.fresh.len();
    let mut slot_of = vec![usize::MAX; n];
    let mut seated: BTreeSet<usize> = BTreeSet::new();
    for next_slot in 0..n {
        let score = |k: usize| -> usize {
            rule.wires
                .iter()
                .flat_map(|(a, b)| [(a, b), (b, a)])
                .filter(|(a, b)| {
                    matches!(a, End::Fresh(kk, _) if *kk as usize == k)
                        && match b {
                            End::CAux(_) | End::PAux(_) => true,
                            End::Fresh(other, _) => seated.contains(&(*other as usize)),
                        }
                })
                .count()
        };
        let pick = (0..n)
            .filter(|k| !seated.contains(k))
            .max_by_key(|k| (score(*k), std::cmp::Reverse(*k)))
            .expect("unseated agent remains");
        slot_of[pick] = next_slot;
        seated.insert(pick);
    }
    slot_of
}

/// Where a semantic endpoint hands over to free routing: (cell, fixed entering endpoint).
/// Boundary auxes exit their seed cell through fixed canonical faces; fresh ports fan out
/// of the seat through the breakout (port 0) or the tail cell's z-layers (ports 1, 2).
fn channel(end: End, slots: &[usize]) -> (Pos, EndPt) {
    match end {
        End::CAux(1) => ((0, -1, 0), EndPt { face: Dir::S, lane: 0 }),
        End::CAux(2) => ((0, 0, 1), EndPt { face: Dir::D, lane: 0 }),
        End::PAux(1) => ((1, -1, 0), EndPt { face: Dir::S, lane: 0 }),
        End::PAux(2) => ((1, 1, 0), EndPt { face: Dir::N, lane: 0 }),
        End::Fresh(k, p) => {
            let x = seat_pos(slots[k as usize]).0;
            match p {
                0 => ((x, -1, 0), EndPt { face: Dir::N, lane: 0 }),
                1 => ((x, -3, 1), EndPt { face: Dir::D, lane: 0 }),
                2 => ((x, -3, -1), EndPt { face: Dir::U, lane: 0 }),
                _ => unreachable!("port index"),
            }
        }
        End::CAux(_) | End::PAux(_) => unreachable!("aux index"),
    }
}

/// The canonical seed-cell exit endpoint for a routed boundary aux.
fn seed_exit(end: End) -> EndPt {
    match end {
        End::CAux(1) => EndPt { face: Dir::N, lane: 0 },
        End::CAux(2) => EndPt { face: Dir::U, lane: 0 },
        End::PAux(1) => EndPt { face: Dir::N, lane: 0 },
        End::PAux(2) => EndPt { face: Dir::S, lane: 0 },
        _ => unreachable!("not a boundary end"),
    }
}

const BOX_LO: Pos = (-2, -6, -1);
const BOX_HI: Pos = (20, 1, 1);

fn in_box(p: Pos) -> bool {
    (BOX_LO.0..=BOX_HI.0).contains(&p.0)
        && (BOX_LO.1..=BOX_HI.1).contains(&p.1)
        && (BOX_LO.2..=BOX_HI.2).contains(&p.2)
        // The dock axis row is roll-invariant and usually occupied by the docked pair's
        // own trailing cables; the blocklet must never claim it.
        && !(p.1 == 0 && p.2 == 0)
}

#[derive(Clone, Debug)]
enum Draft {
    Agent(Cell),
    Wire(Vec<Route>),
}

struct Compiler {
    cells: BTreeMap<Pos, Draft>,
    /// Edge lanes in use, keyed by (cell, face) on the side that claimed them.
    edges: BTreeMap<(Pos, Dir), u8>,
    name: String,
}

impl Compiler {
    fn lane_used(&self, p: Pos, e: EndPt) -> bool {
        let far = (step(p, e.face), e.face.opp());
        self.edges.get(&(p, e.face)).is_some_and(|m| m & (1 << e.lane) != 0)
            || self.edges.get(&far).is_some_and(|m| m & (1 << e.lane) != 0)
    }
    fn mark_lane(&mut self, p: Pos, e: EndPt) {
        *self.edges.entry((p, e.face)).or_default() |= 1 << e.lane;
    }
    fn free_lane(&self, p: Pos, face: Dir) -> Option<u8> {
        (0..2).find(|l| !self.lane_used(p, EndPt { face, lane: *l }))
    }
    fn route_room(&self, p: Pos) -> bool {
        match self.cells.get(&p) {
            None => true,
            Some(Draft::Wire(rs)) => rs.len() < 3,
            Some(Draft::Agent(_)) => false,
        }
    }
    fn add_route(&mut self, p: Pos, r: Route) {
        self.mark_lane(p, r.a);
        self.mark_lane(p, r.b);
        match self.cells.entry(p).or_insert_with(|| Draft::Wire(vec![])) {
            Draft::Wire(rs) => {
                rs.push(r);
                assert!(rs.len() <= 3, "{}: route capacity exceeded at {p:?}", self.name);
            }
            Draft::Agent(_) => panic!("{}: route through agent cell {p:?}", self.name),
        }
    }

    /// BFS a cell path from `a` (entering at `ea`) to `b` (terminating at `eb`), then
    /// install one route per path cell. Cell-count-greedy: reusing a cell that already
    /// carries routes costs nothing, opening a new cell costs one, so wires pack into
    /// shared corridors (0-1 BFS; deterministic: DIRS order, first best cost wins).
    fn route_wire(&mut self, (a, ea): (Pos, EndPt), (b, eb): (Pos, EndPt)) {
        if a == b {
            assert_ne!(ea, eb, "{}: degenerate channel", self.name);
            self.add_route(a, Route::new(ea, eb));
            return;
        }
        assert!(self.route_room(a), "{}: endpoint cell {a:?} full", self.name);
        assert!(self.route_room(b), "{}: endpoint cell {b:?} full", self.name);
        let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
        let mut cost: BTreeMap<Pos, u32> = BTreeMap::new();
        let mut queue = VecDeque::from([a]);
        prev.insert(a, a);
        cost.insert(a, 0);
        while let Some(cur) = queue.pop_front() {
            if cur == b {
                break;
            }
            let base = cost[&cur];
            for d in DIRS {
                let n = step(cur, d);
                if !in_box(n) || n == SEED_C || n == SEED_P || prev.contains_key(&n) {
                    continue;
                }
                if self.free_lane(cur, d).is_none() || !self.route_room(n) {
                    continue;
                }
                // Uniform-cost search keeps corridors thin and spread out: packed
                // corridors measured worse on the frontier (footprint shape, not cell
                // count, drives dock contention).
                let c = base + 1;
                if cost.get(&n).is_some_and(|old| *old <= c) {
                    continue;
                }
                prev.insert(n, cur);
                cost.insert(n, c);
                queue.push_back(n);
            }
        }
        assert!(prev.contains_key(&b), "{}: unroutable wire {a:?} -> {b:?}", self.name);
        let mut path = vec![b];
        let mut cur = b;
        while cur != a {
            cur = prev[&cur];
            path.push(cur);
        }
        path.reverse();
        let mut enter = ea;
        for i in 0..path.len() {
            let exit = if i + 1 < path.len() {
                let d = dir_to(path[i], path[i + 1]).expect("path steps by faces");
                let lane = self.free_lane(path[i], d).expect("lane checked during search");
                EndPt { face: d, lane }
            } else {
                eb
            };
            self.add_route(path[i], Route::new(enter, exit));
            enter = EndPt { face: exit.face.opp(), lane: exit.lane };
        }
    }
}

fn compile(rule_idx: u8) -> Layout {
    let rule = &RULES[rule_idx as usize];
    let name = format!("{}·{}", rule.consumer.name(), rule.producer.name());
    let mut c = Compiler { cells: BTreeMap::new(), edges: BTreeMap::new(), name };

    // Seats and tail fan-outs for every fresh agent.
    let slots = seat_slots(rule);
    let mut seats = vec![];
    for (k, tag) in rule.fresh.iter().enumerate() {
        let seat = seat_pos(slots[k]);
        seats.push((k, seat));
        let arity = tag.arity();
        let cell = Cell::Agent {
            tag: *tag,
            principal: EndPt { face: Dir::S, lane: 0 },
            aux: match arity {
                3 => [EndPt { face: Dir::N, lane: 0 }, EndPt { face: Dir::N, lane: 1 }],
                _ => [EndPt { face: Dir::N, lane: 0 }; 2],
            },
            pass: vec![],
            nursery: true,
            cooldown: 0,
        };
        for e in exposures(&cell) {
            c.mark_lane(seat, e);
        }
        c.cells.insert(seat, Draft::Agent(cell));
        if arity >= 2 {
            let t = tail_pos(slots[k]);
            c.add_route(t, Route::new(
                EndPt { face: Dir::S, lane: 0 },
                EndPt { face: Dir::U, lane: 0 },
            ));
            if arity == 3 {
                c.add_route(t, Route::new(
                    EndPt { face: Dir::S, lane: 1 },
                    EndPt { face: Dir::D, lane: 0 },
                ));
            }
        }
    }

    // Partition the rule wires: direct consumer-producer fusions ride the seed-seed edge;
    // everything else is routed through the fabric.
    let mut routed = vec![];
    let mut cp_wires = vec![];
    for (e1, e2) in rule.wires {
        match (e1, e2) {
            (End::CAux(i), End::PAux(j)) | (End::PAux(j), End::CAux(i)) => {
                cp_wires.push((*i, *j));
            }
            _ => routed.push((*e1, *e2)),
        }
    }
    assert!(cp_wires.len() <= 2, "{}: seed-seed edge has two lanes", c.name);

    // Register every routed channel's fixed handover before routing so no path steals it,
    // and record which boundary auxes use their canonical seed exits.
    let mut c_exits = vec![None; rule.consumer.arity().saturating_sub(1)];
    let mut p_exits = vec![None; rule.producer.arity().saturating_sub(1)];
    for (e1, e2) in &routed {
        for e in [*e1, *e2] {
            match e {
                End::CAux(i) => c_exits[i as usize - 1] = Some(seed_exit(e)),
                End::PAux(j) => p_exits[j as usize - 1] = Some(seed_exit(e)),
                End::Fresh(..) => {}
            }
            let (cell, enter) = channel(e, &slots);
            c.mark_lane(cell, enter);
        }
    }

    // Longest channels first: long routes get first pick of the open fabric.
    routed.sort_by_key(|(e1, e2)| {
        let (a, _) = channel(*e1, &slots);
        let (b, _) = channel(*e2, &slots);
        std::cmp::Reverse((a.0 - b.0).abs() + (a.1 - b.1).abs() + (a.2 - b.2).abs())
    });
    for (e1, e2) in &routed {
        c.route_wire(channel(*e1, &slots), channel(*e2, &slots));
    }

    // Materialize extras; every blocklet cell must be a packable site on its own.
    let extras: Vec<(Pos, Cell)> = c
        .cells
        .iter()
        .map(|(p, d)| {
            let cell = match d {
                Draft::Agent(cell) => cell.clone(),
                Draft::Wire(routes) => {
                    let mut rs = routes.clone();
                    rs.sort_by_key(|r| (r.a.key(), r.b.key()));
                    Cell::Wire { routes: rs, hot: 0, cooldown: 0, reserved: None }
                }
            };
            Word2::pack(&Site::of(cell.clone()))
                .unwrap_or_else(|e| panic!("{}: invalid blocklet cell at {p:?}: {e:?}", c.name));
            (*p, cell)
        })
        .collect();

    let (script, resolve_pc) = build_script(&c.name, &extras);
    assert!(script.len() <= 0x1ff, "{}: script pc overflow", c.name);

    let seated = try_seat(rule);
    Layout { extras, script, resolve_pc, seats, c_exits, p_exits, cp_wires, seated }
}
/// cells and realize every rule wire as a same-cell port/stub hookup, an edge-lane
/// crossing, or a panel route. Capacities are conservative so a runtime inherited
/// passthrough usually still packs; when it does not, the dock falls back to the
/// scripted layout.
fn try_seat(rule: &crate::rules::Rule) -> Option<Seated> {
    let n = rule.fresh.len();
    if n == 0 || n > 2 {
        return None;
    }
    let assigns: Vec<(Option<usize>, Option<usize>)> = match n {
        1 => vec![(Some(0), None), (None, Some(0))],
        _ => vec![(Some(0), Some(1)), (Some(1), Some(0))],
    };
    'assign: for (ca, pa) in assigns {
        let mut c_ports: Vec<Option<EndSrc>> =
            ca.map(|k| vec![None; rule.fresh[k].arity()]).unwrap_or_default();
        let mut p_ports: Vec<Option<EndSrc>> =
            pa.map(|k| vec![None; rule.fresh[k].arity()]).unwrap_or_default();
        let mut c_routes: Vec<(EndSrc, EndSrc)> = vec![];
        let mut p_routes: Vec<(EndSrc, EndSrc)> = vec![];
        let mut lanes = 0u8;
        // Where an End lives (0 = consumer cell, 1 = producer cell) and what it is
        // there: a port slot of the seated agent, or a stub source.
        enum Sided {
            Port(u8, u8),  // (cell, port index)
            Stub(u8, EndSrc),
        }
        let side_of = |e: &End| -> Option<Sided> {
            Some(match e {
                End::CAux(i) => Sided::Stub(0, EndSrc::CStub(*i)),
                End::PAux(j) => Sided::Stub(1, EndSrc::PStub(*j)),
                End::Fresh(k, p) => {
                    let cell = if ca == Some(*k as usize) {
                        0
                    } else if pa == Some(*k as usize) {
                        1
                    } else {
                        return None;
                    };
                    Sided::Port(cell, *p)
                }
            })
        };
        for (e1, e2) in rule.wires {
            let (Some(s1), Some(s2)) = (side_of(e1), side_of(e2)) else { continue 'assign };
            let mut hook = |cell: u8, port: u8, src: EndSrc| {
                let slot = if cell == 0 {
                    &mut c_ports[port as usize]
                } else {
                    &mut p_ports[port as usize]
                };
                assert!(slot.is_none(), "rule wiring is a perfect matching");
                *slot = Some(src);
            };
            match (s1, s2) {
                (Sided::Port(cell1, port1), Sided::Port(cell2, port2)) => {
                    if cell1 == cell2 {
                        // A wire between two ports of one seated agent has no cell to
                        // carry it.
                        continue 'assign;
                    }
                    if lanes >= 2 {
                        continue 'assign;
                    }
                    hook(cell1, port1, EndSrc::Axis(lanes));
                    hook(cell2, port2, EndSrc::Axis(lanes));
                    lanes += 1;
                }
                (Sided::Port(cell, port), Sided::Stub(scell, src))
                | (Sided::Stub(scell, src), Sided::Port(cell, port)) => {
                    if cell == scell {
                        hook(cell, port, src);
                    } else {
                        if lanes >= 2 {
                            continue 'assign;
                        }
                        hook(cell, port, EndSrc::Axis(lanes));
                        let routes = if scell == 0 { &mut c_routes } else { &mut p_routes };
                        routes.push((src, EndSrc::Axis(lanes)));
                        lanes += 1;
                    }
                }
                (Sided::Stub(cell1, src1), Sided::Stub(cell2, src2)) => {
                    if cell1 == cell2 {
                        let routes = if cell1 == 0 { &mut c_routes } else { &mut p_routes };
                        routes.push((src1, src2));
                    } else {
                        if lanes >= 2 {
                            continue 'assign;
                        }
                        let (cr, pr) = if cell1 == 0 { (src1, src2) } else { (src2, src1) };
                        c_routes.push((cr, EndSrc::Axis(lanes)));
                        p_routes.push((pr, EndSrc::Axis(lanes)));
                        lanes += 1;
                    }
                }
            }
        }
        // Capacity, leaving headroom for one inherited runtime passthrough per cell:
        // an agent carries at most two passthroughs, a panel at most three routes.
        let fits = |agent: bool, routes: &[(EndSrc, EndSrc)]| {
            routes.len() <= if agent { 1 } else { 2 }
        };
        if !fits(ca.is_some(), &c_routes) || !fits(pa.is_some(), &p_routes) {
            continue 'assign;
        }
        let bind = |k: Option<usize>, ports: Vec<Option<EndSrc>>| {
            k.map(|k| {
                let ports: Vec<EndSrc> =
                    ports.into_iter().map(|p| p.expect("every port wired")).collect();
                (k, ports)
            })
        };
        return Some(Seated {
            c_agent: bind(ca, c_ports),
            p_agent: bind(pa, p_ports),
            c_routes,
            p_routes,
        });
    }
    None
}

/// Attachment point key. The dock edge is one physical channel: the consumer's
/// (axis, l) and the producer's (axis.opp, l) are the same point. Key kind 3 is a
/// synthetic point per fresh-agent port (seated verification only).
type Pt = (u8, u8, u8);

fn pt_key(axis: Dir, consumer_side: bool, e: EndPt) -> Pt {
    let edge = if consumer_side { axis } else { axis.opp() };
    if e.face == edge {
        (2, 0, e.lane)
    } else {
        (consumer_side as u8, e.face.code(), e.lane)
    }
}

struct Uf {
    parent: Vec<u16>,
}

impl Uf {
    fn new(n: usize) -> Self {
        Self { parent: (0..n as u16).collect() }
    }
    fn find(&mut self, mut x: u16) -> u16 {
        while self.parent[x as usize] != x {
            self.parent[x as usize] = self.parent[x as usize];
            x = self.parent[x as usize];
        }
        x
    }
    fn union(&mut self, a: u16, b: u16) {
        let (ra, rb) = (self.find(a), self.find(b));
        if ra != rb {
            self.parent[ra as usize] = rb;
        }
    }
}

fn routes_of(cell: &Cell) -> &[Route] {
    match cell {
        Cell::Wire { routes, .. } => routes,
        Cell::Agent { pass, .. } => pass,
        _ => &[],
    }
}

/// A fresh agent installed in one of the two final cells: (index into rule.fresh,
/// true = consumer cell). The verifier checks its ports against the actual cell.
pub type VisibleFresh = (usize, bool);

/// Whether the final cells splice the dying pair's cables exactly as the rule demands.
/// Cables are followed from the stubs through any seed passthroughs to where they leave
/// the pair; those external ends (plus foreign passthrough ends and seated fresh ports)
/// are the markers. Intended connectivity — pre-fire cables merged pairwise by the
/// rule's wires, foreign cables untouched — must equal physical connectivity in the
/// finals (routes + shared edge lanes). Anything else must be declined, never
/// installed: a cable resting on the dock edge facing a non-partner cannot be spliced
/// in two lanes. Scripted layouts are trusted beyond the canonical exits
/// (atlas-tested); this guards the runtime-variable part, the seed finals.
pub fn finals_correct(
    rule: u8,
    axis: Dir,
    seed_c: &Cell,
    seed_p: &Cell,
    fc: &Cell,
    fp: &Cell,
    visible: &[VisibleFresh],
) -> bool {
    let r = &RULES[rule as usize];
    let (
        Cell::Seed { stub: cstub, pass: cpass, .. },
        Cell::Seed { stub: pstub, pass: ppass, .. },
    ) = (seed_c, seed_p)
    else {
        panic!("finals_correct expects two seed cells");
    };
    let carity = r.consumer.arity().saturating_sub(1);
    let parity = r.producer.arity().saturating_sub(1);
    let mut ids: BTreeMap<Pt, u16> = BTreeMap::new();
    let mut uf = Uf::new(96);
    let pid = |ids: &mut BTreeMap<Pt, u16>, k: Pt| -> u16 {
        let n = ids.len() as u16;
        *ids.entry(k).or_insert(n)
    };
    let key_of = |ids: &mut BTreeMap<Pt, u16>, side: bool, e: EndPt| -> u16 {
        let k = pt_key(axis, side, e);
        pid(ids, k)
    };
    // Cables: passthroughs chain attachment points; rule wires merge cable classes
    // (fresh ports ride synthetic points, so a wire to a fresh port merges its cable
    // with that port's class).
    for (side, pass) in [(true, cpass), (false, ppass)] {
        for route in pass.iter() {
            let a = key_of(&mut ids, side, route.a);
            let b = key_of(&mut ids, side, route.b);
            uf.union(a, b);
        }
    }
    let stub_key = |ids: &mut BTreeMap<Pt, u16>, side: bool, idx: usize| -> u16 {
        let e = if side { cstub[idx] } else { pstub[idx] };
        key_of(ids, side, e)
    };
    for (e1, e2) in r.wires {
        let mut rep = |e: &End| -> Option<u16> {
            Some(match e {
                End::CAux(i) => stub_key(&mut ids, true, *i as usize - 1),
                End::PAux(j) => stub_key(&mut ids, false, *j as usize - 1),
                End::Fresh(k, p) => pid(&mut ids, (3, *k, *p)),
            })
        };
        if let (Some(a), Some(b)) = (rep(e1), rep(e2)) {
            uf.union(a, b);
        }
    }
    // Where a stub's cable leaves the pair: chase through the dead edge and any
    // passthrough it re-enters; None means the cable is internal (ends at the
    // partner's own stub or the dead principal edge) and needs no marker.
    let chase_ext = |mut side: bool, mut e: EndPt| -> Option<(bool, EndPt)> {
        for _ in 0..4 {
            let edge = if side { axis } else { axis.opp() };
            if e.face != edge {
                return Some((side, e));
            }
            let other = !side;
            let far = EndPt { face: edge, lane: e.lane };
            let pass = if other { cpass } else { ppass };
            e = pass.as_ref()?.through(far)?;
            side = other;
        }
        None
    };
    // Markers: (intended class, final attachment). Stub markers attach where the cable
    // leaves the pair; foreign passthroughs (cable classes holding no stub) attach at
    // their external ends; fresh ports at the seated agent's port endpoint.
    let mut marks: Vec<(u16, bool, EndPt)> = vec![];
    let mut stub_roots: BTreeMap<u16, ()> = BTreeMap::new();
    for side in [true, false] {
        let arity = if side { carity } else { parity };
        for idx in 0..arity {
            let e = if side { cstub[idx] } else { pstub[idx] };
            let root = uf.find(key_of(&mut ids, side, e));
            stub_roots.insert(root, ());
            if let Some((cell, ext)) = chase_ext(side, e) {
                marks.push((root, cell, ext));
            }
        }
    }
    for (side, pass) in [(true, cpass), (false, ppass)] {
        for route in pass.iter() {
            for e in route.ends() {
                if e.face != if side { axis } else { axis.opp() } {
                    let root = uf.find(key_of(&mut ids, side, e));
                    if !stub_roots.contains_key(&root) {
                        marks.push((root, side, e));
                    }
                }
            }
        }
    }
    // Physical classes from the finals' routes.
    let mut pids: BTreeMap<Pt, u16> = BTreeMap::new();
    let mut puf = Uf::new(96);
    for (side, cell) in [(true, fc), (false, fp)] {
        for route in routes_of(cell) {
            let a = key_of(&mut pids, side, route.a);
            let b = key_of(&mut pids, side, route.b);
            puf.union(a, b);
        }
    }
    let ex_c = exposures(fc);
    let ex_p = exposures(fp);
    let mut groups: BTreeMap<u16, Vec<u16>> = BTreeMap::new();
    let mut physical_of = |side: bool, e: EndPt| -> Option<u16> {
        let ex = if side { &ex_c } else { &ex_p };
        if !ex.contains(&e) {
            return None; // a cable end was dropped: cut, never acceptable
        }
        let k = pt_key(axis, side, e);
        let id = pid(&mut pids, k);
        Some(puf.find(id))
    };
    for (root, side, e) in marks {
        let Some(class) = physical_of(side, e) else { return false };
        groups.entry(root).or_default().push(class);
    }
    // Visible fresh ports must exist with the planned tag and join their wire's class.
    for (k, side) in visible {
        let (cell, ex) = if *side { (fc, &ex_c) } else { (fp, &ex_p) };
        let Cell::Agent { tag, principal, aux, .. } = cell else { return false };
        if *tag != r.fresh[*k] {
            return false;
        }
        for p in 0..tag.arity() {
            let e = if p == 0 { *principal } else { aux[p - 1] };
            if !ex.contains(&e) {
                return false;
            }
            let key = pid(&mut pids, pt_key(axis, *side, e));
            let root = uf.find(pid(&mut ids, (3, *k as u8, p as u8)));
            groups.entry(root).or_default().push(puf.find(key));
        }
    }
    // Partition equality: one intended class = one physical class, and no sharing.
    let mut owners: BTreeMap<u16, u16> = BTreeMap::new();
    for (root, classes) in &groups {
        let first = classes[0];
        if classes.iter().any(|c| *c != first) {
            return false; // a cable got split
        }
        if owners.insert(first, *root).is_some() {
            return false; // two cables got merged
        }
    }
    true
}

/// Fresh agents installed in the two final cells, for the verifier: the seated agents,
/// or none (pure scripted layouts are checked at the boundary only).
fn visible_fresh(rule: u8) -> Vec<VisibleFresh> {
    let l = layout(rule);
    let mut v = vec![];
    if let Some(s) = &l.seated {
        if let Some((k, _)) = &s.c_agent {
            v.push((*k, true));
        }
        if let Some((k, _)) = &s.p_agent {
            v.push((*k, false));
        }
    }
    v
}

/// The seated resolution's final matter for the two seed cells, or None when this rule
/// has no seated plan or the plan does not pack against the runtime stubs and
/// inherited passthroughs (the caller falls back to the scripted layout).
pub fn seated_finals(rule: u8, axis: Dir, seed_c: &Cell, seed_p: &Cell) -> Option<(Cell, Cell)> {
    let seated = layout(rule).seated.as_ref()?;
    let (
        Cell::Seed { stub: cstub, pass: cpass, .. },
        Cell::Seed { stub: pstub, pass: ppass, .. },
    ) = (seed_c, seed_p)
    else {
        panic!("seated_finals expects two seed cells");
    };
    let r = &RULES[rule as usize];
    // A runtime stub can land on the dock edge and collide with a compiled lane choice,
    // so both lane permutations are tried before giving up.
    for swap in [false, true] {
        let resolve = |consumer_cell: bool, s: EndSrc| -> EndPt {
            match s {
                EndSrc::CStub(i) => cstub[i as usize - 1],
                EndSrc::PStub(j) => pstub[j as usize - 1],
                EndSrc::Axis(l) => EndPt {
                    face: if consumer_cell { axis } else { axis.opp() },
                    lane: if swap { 1 - l } else { l },
                },
            }
        };
        let build = |consumer_cell: bool,
                     agent: &Option<(usize, Vec<EndSrc>)>,
                     routes: &[(EndSrc, EndSrc)],
                     inherited: &Option<Route>|
         -> Cell {
            let mut rs: Vec<Route> = routes
                .iter()
                .map(|(a, b)| Route::new(resolve(consumer_cell, *a), resolve(consumer_cell, *b)))
                .collect();
            rs.extend(inherited.iter().copied());
            match agent {
                Some((k, ports)) => {
                    let tag = r.fresh[*k];
                    let principal = resolve(consumer_cell, ports[0]);
                    let mut aux = [principal; 2];
                    for (i, p) in ports.iter().enumerate().skip(1) {
                        aux[i - 1] = resolve(consumer_cell, *p);
                    }
                    if tag.arity() == 2 {
                        aux[1] = aux[0];
                    }
                    Cell::Agent { tag, principal, aux, pass: rs, nursery: false, cooldown: 0 }
                }
                None if rs.is_empty() => Cell::Empty { reserved: None },
                None => Cell::Wire { routes: rs, hot: 0, cooldown: 0, reserved: None },
            }
        };
        let fc = build(true, &seated.c_agent, &seated.c_routes, cpass);
        let fp = build(false, &seated.p_agent, &seated.p_routes, ppass);
        if Word2::pack(&Site::of(fc.clone())).is_ok()
            && Word2::pack(&Site::of(fp.clone())).is_ok()
            && finals_correct(rule, axis, seed_c, seed_p, &fc, &fp, &visible_fresh(rule))
        {
            return Some((fc, fp));
        }
    }
    None
}

/// The seat offsets of a seated resolution, for observer id binding.
pub fn seated_seats(rule: u8) -> Vec<(usize, Pos)> {
    let Some(seated) = layout(rule).seated.as_ref() else { return vec![] };
    let mut out = vec![];
    if let Some((k, _)) = &seated.c_agent {
        out.push((*k, SEED_C));
    }
    if let Some((k, _)) = &seated.p_agent {
        out.push((*k, SEED_P));
    }
    out
}

/// Two-phase cursor program. Phase one places every cell (all agents in the nursery) and
/// returns to the consumer seed, where the resolve fires the interaction while every
/// fresh agent is still seated. Phase two revisits only the agent-bearing branches
/// (wire-only twigs need no finalize) and clears the nurseries. The producer seed is
/// walkable but never placed.
fn build_script(name: &str, extras: &[(Pos, Cell)]) -> (Vec<Op>, u16) {
    let map: BTreeMap<Pos, &Cell> = extras.iter().map(|(p, c)| (*p, c)).collect();
    let mut ops = vec![];

    let bridge = |map: &BTreeMap<Pos, &Cell>, ops: &mut Vec<Op>, visited: &mut BTreeSet<Pos>, place: bool| {
        walk(SEED_C, map, visited, ops, place);
        if DIRS.iter().any(|d| {
            let n = step(SEED_P, *d);
            !visited.contains(&n) && map.contains_key(&n)
        }) {
            let d = dir_to(SEED_C, SEED_P).expect("adjacent seeds");
            ops.push(Op::Hop { dir: d, finalize: false });
            walk(SEED_P, map, visited, ops, place);
            ops.push(Op::Hop { dir: d.opp(), finalize: false });
        }
    };

    let mut visited: BTreeSet<Pos> = [SEED_C, SEED_P].into();
    bridge(&map, &mut ops, &mut visited, true);
    for (p, _) in extras {
        assert!(visited.contains(p), "{name}: blocklet cell {p:?} unreachable");
    }
    let resolve_pc = ops.len() as u16;

    if extras.iter().any(|(_, c)| matches!(c, Cell::Agent { .. })) {
        // The finalize pass's map: agents plus the corridors that reach them.
        let mut parent: BTreeMap<Pos, Pos> = BTreeMap::new();
        let mut q = VecDeque::from([SEED_C, SEED_P]);
        parent.insert(SEED_C, SEED_C);
        parent.insert(SEED_P, SEED_C);
        while let Some(cur) = q.pop_front() {
            for d in DIRS {
                let n = step(cur, d);
                if parent.contains_key(&n) || (n != SEED_P && !map.contains_key(&n)) {
                    continue;
                }
                parent.insert(n, cur);
                q.push_back(n);
            }
        }
        let mut keep: BTreeSet<Pos> = BTreeSet::new();
        for (p, cell) in &map {
            if !matches!(cell, Cell::Agent { .. }) {
                continue;
            }
            let mut cur = *p;
            while keep.insert(cur) {
                cur = parent[&cur];
                if cur == SEED_C {
                    break;
                }
            }
        }
        let fmap: BTreeMap<Pos, &Cell> =
            map.iter().filter(|(p, _)| keep.contains(*p)).map(|(p, c)| (*p, *c)).collect();
        let mut visited: BTreeSet<Pos> = [SEED_C, SEED_P].into();
        bridge(&fmap, &mut ops, &mut visited, false);
    }
    (ops, resolve_pc)
}

fn walk(
    cur: Pos,
    map: &BTreeMap<Pos, &Cell>,
    visited: &mut BTreeSet<Pos>,
    ops: &mut Vec<Op>,
    place: bool,
) {
    for d in DIRS {
        let n = step(cur, d);
        if visited.contains(&n) {
            continue;
        }
        let Some(cell) = map.get(&n) else { continue };
        visited.insert(n);
        let is_agent = matches!(cell, Cell::Agent { .. });
        let has_children = DIRS.iter().any(|dd| {
            let nn = step(n, *dd);
            !visited.contains(&nn) && map.contains_key(&nn)
        });
        if place {
            ops.push(Op::Place { dir: d, cell: (*cell).clone() });
            if has_children {
                ops.push(Op::Hop { dir: d, finalize: false });
                walk(n, map, visited, ops, place);
                ops.push(Op::Hop { dir: d.opp(), finalize: false });
            }
        } else {
            // Finalize pass: enter everything, clear the nursery when leaving agents.
            ops.push(Op::Hop { dir: d, finalize: false });
            walk(n, map, visited, ops, place);
            ops.push(Op::Hop { dir: d.opp(), finalize: is_agent });
        }
    }
}

static LAYOUTS: OnceLock<Vec<Layout>> = OnceLock::new();

pub fn layout(rule: u8) -> &'static Layout {
    &LAYOUTS.get_or_init(|| (0..RULES.len() as u8).map(compile).collect())[rule as usize]
}

/// Whether both seed finals (including inherited passthroughs) are valid packable cells.
/// Used as the dock-time roll filter.
pub fn finals_fit(rule: u8, axis: Dir, roll: u8, seed_c: &Cell, seed_p: &Cell) -> bool {
    let (fc, fp) = seed_finals(rule, axis, roll, seed_c, seed_p);
    Word2::pack(&Site::of(fc.clone())).is_ok()
        && Word2::pack(&Site::of(fp.clone())).is_ok()
        && finals_correct(rule, axis, seed_c, seed_p, &fc, &fp, &visible_fresh(rule))
}

/// The final matter of the two seed cells at resolve: patch-panel routes from the runtime
/// stub faces to the canonical exits (rotated), the direct seed-seed fusion wires, and
/// any inherited passthrough routes.
pub fn seed_finals(rule: u8, axis: Dir, roll: u8, seed_c: &Cell, seed_p: &Cell) -> (Cell, Cell) {
    let l = layout(rule);
    let (
        Cell::Seed { stub: cstub, pass: cpass, .. },
        Cell::Seed { stub: pstub, pass: ppass, .. },
    ) = (seed_c, seed_p)
    else {
        panic!("seed_finals expects two seed cells");
    };
    let mut c_routes = vec![];
    let mut p_routes = vec![];
    for (i, exit) in l.c_exits.iter().enumerate() {
        if let Some(exit) = exit {
            c_routes.push(Route::new(cstub[i], rot_endpt(*exit, axis, roll)));
        }
    }
    for (j, exit) in l.p_exits.iter().enumerate() {
        if let Some(exit) = exit {
            p_routes.push(Route::new(pstub[j], rot_endpt(*exit, axis, roll)));
        }
    }
    // Fusion wires splice the dying pair's cables. A stub can sit on the dock edge
    // itself (a seated neighbor's cable hairpinning through the pair), so each cable
    // end is first CHASED through the dead edge and any passthrough it re-enters, and
    // the splice composes at the true resting ends: same cell means one direct route
    // (the fire-time hairpin collapse), different cells ride a free edge lane.
    let mut cpass_v: Vec<Route> = cpass.iter().copied().collect();
    let mut ppass_v: Vec<Route> = ppass.iter().copied().collect();
    let mut chase = |mut cell: u8, mut e: EndPt| -> (u8, EndPt) {
        loop {
            let edge_face = if cell == 0 { axis } else { axis.opp() };
            if e.face != edge_face {
                return (cell, e);
            }
            let other = 1 - cell;
            let far = EndPt { face: if other == 0 { axis } else { axis.opp() }, lane: e.lane };
            let pass = if other == 0 { &mut cpass_v } else { &mut ppass_v };
            let Some(k) = pass.iter().position(|r| r.through(far).is_some()) else {
                // Rests on the dead edge with no continuation: leave it; the splice
                // will use this lane directly.
                return (cell, e);
            };
            e = pass[k].through(far).unwrap();
            pass.remove(k);
            cell = other;
        }
    };
    let mut splices: Vec<((u8, EndPt), (u8, EndPt))> = vec![];
    for (ci, pj) in l.cp_wires.iter() {
        let a = chase(0, cstub[*ci as usize - 1]);
        let b = chase(1, pstub[*pj as usize - 1]);
        splices.push((a, b));
    }
    let mut c_extra: Vec<Route> = vec![];
    let mut p_extra: Vec<Route> = vec![];
    // Edge lanes still occupied by surviving passthroughs are not free for splices.
    let mut lane_used = [false; 2];
    for r in cpass_v.iter() {
        for e in r.ends() {
            if e.face == axis {
                lane_used[e.lane as usize] = true;
            }
        }
    }
    for r in ppass_v.iter() {
        for e in r.ends() {
            if e.face == axis.opp() {
                lane_used[e.lane as usize] = true;
            }
        }
    }
    for ((ca, ea), (cb, eb)) in splices {
        if ca == cb {
            if ea == eb {
                continue; // the cable loops onto itself: a closed cycle, vanish
            }
            let side = if ca == 0 { &mut c_extra } else { &mut p_extra };
            side.push(Route::new(ea, eb));
            continue;
        }
        // Cross-cell: ride a free edge lane; an end already resting on the edge keeps
        // its own lane and needs no route.
        let (ce, pe) = if ca == 0 { (ea, eb) } else { (eb, ea) };
        let lane = if ce.face == axis {
            ce.lane
        } else if pe.face == axis.opp() {
            pe.lane
        } else {
            match (0..2u8).find(|l| !lane_used[*l as usize]) {
                Some(l) => l,
                None => {
                    // No free edge lane: poison the consumer panel with duplicate
                    // endpoints so it cannot pack and the dock declines.
                    let poison =
                        Route::new(EndPt { face: axis, lane: 0 }, EndPt { face: axis, lane: 1 });
                    c_extra.push(poison);
                    c_extra.push(poison);
                    continue;
                }
            }
        };
        lane_used[lane as usize] = true;
        let c_end = EndPt { face: axis, lane };
        let p_end = EndPt { face: axis.opp(), lane };
        if ce != c_end {
            c_extra.push(Route::new(ce, c_end));
        }
        if pe != p_end {
            p_extra.push(Route::new(pe, p_end));
        }
    }
    c_routes.extend(c_extra);
    p_routes.extend(p_extra);
    c_routes.extend(cpass_v);
    p_routes.extend(ppass_v);
    let finish = |routes: Vec<Route>| {
        if routes.is_empty() {
            Cell::Empty { reserved: None }
        } else {
            Cell::Wire { routes, hot: 0, cooldown: 0, reserved: None }
        }
    };
    (finish(c_routes), finish(p_routes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade::Half;

    fn seed(rule: u8, half: Half, partner: Dir, stub: [EndPt; 2], pass: Option<Route>) -> Cell {
        Cell::Seed { rule, half, partner, roll: 0, stub, plane: 0, pass }
    }

    fn pt(face: Dir, lane: u8) -> EndPt {
        EndPt { face, lane }
    }

    fn wire(routes: Vec<Route>) -> Cell {
        Cell::Wire { routes, hot: 0, cooldown: 0, reserved: None }
    }

    // Rule 12 = Unp·Pair: pure double fusion (C1-P1, C2-P2), the two-cp-wire rule.
    const FUSION: u8 = 12;

    #[test]
    fn verifier_accepts_straight_fusion_and_rejects_swapped() {
        let axis = Dir::E;
        let sc = seed(FUSION, Half::Consumer, axis, [pt(Dir::N, 0), pt(Dir::S, 0)], None);
        let sp = seed(FUSION, Half::Producer, axis.opp(), [pt(Dir::N, 0), pt(Dir::S, 0)], None);
        let fc = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::E, 0)), Route::new(pt(Dir::S, 0), pt(Dir::E, 1))]);
        let good = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::W, 0)), Route::new(pt(Dir::S, 0), pt(Dir::W, 1))]);
        assert!(finals_correct(FUSION, axis, &sc, &sp, &fc, &good, &[]));
        let swapped = wire(vec![Route::new(pt(Dir::S, 0), pt(Dir::W, 0)), Route::new(pt(Dir::N, 0), pt(Dir::W, 1))]);
        assert!(!finals_correct(FUSION, axis, &sc, &sp, &fc, &swapped, &[]));
    }

    #[test]
    fn verifier_internal_wire_can_be_absorbed_but_not_split() {
        let axis = Dir::E;
        // C1 rests on the edge facing P2: an internal wire. The rule re-pairs
        // crosswise, so C2's and P1's cables must join through the freed channel.
        let sc = seed(FUSION, Half::Consumer, axis, [pt(Dir::E, 1), pt(Dir::S, 0)], None);
        let sp = seed(FUSION, Half::Producer, axis.opp(), [pt(Dir::N, 0), pt(Dir::W, 1)], None);
        let fc = wire(vec![Route::new(pt(Dir::S, 0), pt(Dir::E, 1))]);
        let fp = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::W, 1))]);
        assert!(finals_correct(FUSION, axis, &sc, &sp, &fc, &fp, &[]));
        // Split: P1 routed to the other lane, C2 left facing the dangling channel.
        let fp_split = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::W, 0))]);
        assert!(!finals_correct(FUSION, axis, &sc, &sp, &fc, &fp_split, &[]));
    }

    #[test]
    fn verifier_foreign_passthrough_must_survive() {
        let axis = Dir::E;
        let guest = Route::new(pt(Dir::N, 1), pt(Dir::D, 0));
        let sc = seed(FUSION, Half::Consumer, axis, [pt(Dir::N, 0), pt(Dir::S, 0)], Some(guest));
        let sp = seed(FUSION, Half::Producer, axis.opp(), [pt(Dir::N, 0), pt(Dir::S, 0)], None);
        let fc = wire(vec![
            Route::new(pt(Dir::N, 0), pt(Dir::E, 0)),
            Route::new(pt(Dir::S, 0), pt(Dir::E, 1)),
            guest,
        ]);
        let fp = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::W, 0)), Route::new(pt(Dir::S, 0), pt(Dir::W, 1))]);
        assert!(finals_correct(FUSION, axis, &sc, &sp, &fc, &fp, &[]));
        let fc_cut = wire(vec![Route::new(pt(Dir::N, 0), pt(Dir::E, 0)), Route::new(pt(Dir::S, 0), pt(Dir::E, 1))]);
        assert!(!finals_correct(FUSION, axis, &sc, &sp, &fc_cut, &fp, &[]));
    }

    #[test]
    fn verifier_chases_stub_through_partner_passthrough() {
        let axis = Dir::E;
        // C1's cable hairpins: it crosses the edge and exits the producer cell at U.
        let hairpin = Route::new(pt(Dir::W, 1), pt(Dir::U, 0));
        let sc = seed(FUSION, Half::Consumer, axis, [pt(Dir::E, 1), pt(Dir::S, 0)], None);
        let sp = seed(FUSION, Half::Producer, axis.opp(), [pt(Dir::N, 0), pt(Dir::S, 0)], Some(hairpin));
        let (fc, fp) = seed_finals(FUSION, axis, 0, &sc, &sp);
        assert!(finals_correct(FUSION, axis, &sc, &sp, &fc, &fp, &[]));
    }

    #[test]
    fn verifier_seated_port_swaps_are_caught() {
        let rule = 0; // A·L: fresh S, (X0,1)-C1, (X0,0)-C2
        let axis = Dir::E;
        let sc = seed(rule, Half::Consumer, axis, [pt(Dir::N, 0), pt(Dir::S, 0)], None);
        let sp = seed(rule, Half::Producer, axis.opp(), [pt(Dir::N, 0), pt(Dir::N, 0)], None);
        let (fc, fp) = seated_finals(rule, axis, &sc, &sp).expect("seated resolves");
        assert!(finals_correct(rule, axis, &sc, &sp, &fc, &fp, &visible_fresh(rule)));
        // Miswire: the S's principal and aux endpoints swapped.
        let bad = Cell::Agent {
            tag: crate::rules::Tag::S,
            principal: pt(Dir::N, 0),
            aux: [pt(Dir::S, 0); 2],
            pass: vec![],
            nursery: false,
            cooldown: 0,
        };
        assert!(!finals_correct(rule, axis, &sc, &sp, &bad, &fp, &visible_fresh(rule)));
    }

    #[test]
    fn seated_finals_correct_for_representative_orientations() {
        // Every seated rule, every axis, stubs fanned over the off-edge faces: any
        // accepted resolution must verify (the function declines by returning None).
        for i in 0..RULES.len() as u8 {
            let r = &RULES[i as usize];
            if layout(i).seated.is_none() {
                continue;
            }
            for axis in [Dir::E, Dir::N, Dir::U] {
                let stub_of = |faces: &[Dir]| match faces.len() {
                    0 => [pt(axis, 0); 2],
                    1 => [pt(faces[0], 0); 2],
                    _ => [pt(faces[0], 0), pt(faces[1], 0)],
                };
                let off: Vec<Dir> =
                    DIRS.into_iter().filter(|d| *d != axis && *d != axis.opp()).collect();
                let cs: Vec<Dir> = off.iter().copied().take(r.consumer.arity() - 1).collect();
                let ps: Vec<Dir> = off.iter().copied().take(r.producer.arity() - 1).collect();
                let sc = seed(i, Half::Consumer, axis, stub_of(&cs), None);
                let sp = seed(i, Half::Producer, axis.opp(), stub_of(&ps), None);
                if let Some((fc, fp)) = seated_finals(i, axis, &sc, &sp) {
                    assert!(
                        finals_correct(i, axis, &sc, &sp, &fc, &fp, &visible_fresh(i)),
                        "rule {i} axis {axis:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn all_rules_compile_and_stay_small() {
        let mut worst = 0;
        for i in 0..RULES.len() as u8 {
            let l = layout(i);
            let r = &RULES[i as usize];
            assert_eq!(l.seats.len(), r.fresh.len(), "{}·{}", r.consumer.name(), r.producer.name());
            worst = worst.max(l.extras.len());
        }
        assert!(worst < 63, "blocklets must stay under the v1 workshop cap, worst {worst}");
    }

    #[test]
    fn pure_fusion_rules_have_no_script() {
        for i in 0..RULES.len() as u8 {
            let l = layout(i);
            let r = &RULES[i as usize];
            if r.fresh.is_empty() {
                assert!(l.script.is_empty(), "{}·{}", r.consumer.name(), r.producer.name());
            }
        }
    }

    #[test]
    fn scripts_finalize_every_agent_and_return() {
        for i in 0..RULES.len() as u8 {
            let l = layout(i);
            // Simulate the cursor walk over canonical offsets.
            let mut at = SEED_C;
            let mut placed: BTreeSet<Pos> = [SEED_C, SEED_P].into();
            let mut finals = 0usize;
            for (pc, op) in l.script.iter().enumerate() {
                match op {
                    Op::Place { dir, .. } => {
                        assert!(pc < l.resolve_pc as usize, "rule {i}: place after resolve");
                        let t = step(at, *dir);
                        assert!(!placed.contains(&t), "rule {i}: double place at {t:?}");
                        placed.insert(t);
                    }
                    Op::Hop { dir, finalize } => {
                        let t = step(at, *dir);
                        assert!(placed.contains(&t), "rule {i}: hop onto unplaced {t:?}");
                        if *finalize {
                            assert!(pc >= l.resolve_pc as usize, "rule {i}: finalize before resolve");
                            finals += 1;
                        }
                        at = t;
                    }
                }
                if pc + 1 == l.resolve_pc as usize {
                    // The resolve happens standing on the consumer seed... after the op
                    // at resolve_pc - 1 the cursor must be home.
                }
            }
            assert_eq!(at, SEED_C, "rule {i}: cursor must end on the consumer seed");
            let agents = l.extras.iter().filter(|(_, c)| matches!(c, Cell::Agent { .. })).count();
            assert_eq!(finals, agents, "rule {i}: every agent finalized exactly once");
            assert_eq!(placed.len(), l.extras.len() + 2, "rule {i}: every extra placed");
        }
    }

    #[test]
    fn seed_finals_pack_for_every_stub_orientation() {
        // For each rule, place stubs on representative faces and check both finals pack.
        for i in 0..RULES.len() as u8 {
            let r = &RULES[i as usize];
            for axis in [Dir::E, Dir::N, Dir::U] {
                for roll in 0..4u8 {
                    let cstub_face = DIRS
                        .into_iter()
                        .find(|f| {
                            *f != axis
                                && layout(i).c_exits.iter().flatten().all(|e| {
                                    crate::cascade::rot_dir(e.face, axis, roll) != *f
                                })
                        })
                        .unwrap();
                    let pstub_face = DIRS
                        .into_iter()
                        .find(|f| {
                            *f != axis.opp()
                                && layout(i).p_exits.iter().flatten().all(|e| {
                                    crate::cascade::rot_dir(e.face, axis, roll) != *f
                                })
                        })
                        .unwrap();
                    let stub_of = |f: Dir, arity: usize| match arity {
                        3 => [EndPt { face: f, lane: 0 }, EndPt { face: f, lane: 1 }],
                        _ => [EndPt { face: f, lane: 0 }; 2],
                    };
                    let sc = Cell::Seed {
                        rule: i,
                        half: Half::Consumer,
                        partner: axis,
                        roll,
                        stub: stub_of(cstub_face, r.consumer.arity()),
                        plane: 0,
                        pass: None,
                    };
                    let sp = Cell::Seed {
                        rule: i,
                        half: Half::Producer,
                        partner: axis.opp(),
                        roll,
                        stub: stub_of(pstub_face, r.producer.arity()),
                        plane: 0,
                        pass: None,
                    };
                    let (fc, fp) = seed_finals(i, axis, roll, &sc, &sp);
                    Word2::pack(&Site::of(fc)).expect("consumer final packs");
                    Word2::pack(&Site::of(fp)).expect("producer final packs");
                }
            }
        }
    }
}

#[cfg(test)]
mod size_probe {
    use super::*;
    #[test]
    fn print_sizes() {
        for i in 0..RULES.len() as u8 {
            let l = layout(i);
            let r = &RULES[i as usize];
            println!("{:>10} extras {:>3} script {:>3}", format!("{}·{}", r.consumer.name(), r.producer.name()), l.extras.len(), l.script.len());
        }
    }
}
