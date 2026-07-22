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
}

const SEED_C: Pos = (0, 0, 0);
const SEED_P: Pos = (1, 0, 0);

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
    /// install one route per path cell. Deterministic: DIRS order, first discovery wins.
    fn route_wire(&mut self, (a, ea): (Pos, EndPt), (b, eb): (Pos, EndPt)) {
        if a == b {
            assert_ne!(ea, eb, "{}: degenerate channel", self.name);
            self.add_route(a, Route::new(ea, eb));
            return;
        }
        let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
        let mut queue = VecDeque::from([a]);
        prev.insert(a, a);
        'search: while let Some(cur) = queue.pop_front() {
            for d in DIRS {
                let n = step(cur, d);
                if !in_box(n) || n == SEED_C || n == SEED_P || prev.contains_key(&n) {
                    continue;
                }
                if self.free_lane(cur, d).is_none() || !self.route_room(n) {
                    continue;
                }
                prev.insert(n, cur);
                if n == b {
                    break 'search;
                }
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

    Layout { extras, script, resolve_pc, seats, c_exits, p_exits, cp_wires }
}

/// Two-phase cursor program. Phase one places every cell (all agents in the nursery) and
/// returns to the consumer seed, where the resolve fires the interaction while every
/// fresh agent is still seated. Phase two revisits the placed cells and finalizes the
/// agents. The producer seed is walkable but never placed.
fn build_script(name: &str, extras: &[(Pos, Cell)]) -> (Vec<Op>, u16) {
    let map: BTreeMap<Pos, &Cell> = extras.iter().map(|(p, c)| (*p, c)).collect();
    let mut ops = vec![];

    let bridge = |ops: &mut Vec<Op>, visited: &mut BTreeSet<Pos>, place: bool| {
        let pass = |ops: &mut Vec<Op>, visited: &mut BTreeSet<Pos>| {
            walk(SEED_C, &map, visited, ops, place);
            if DIRS.iter().any(|d| {
                let n = step(SEED_P, *d);
                !visited.contains(&n) && map.contains_key(&n)
            }) {
                let d = dir_to(SEED_C, SEED_P).expect("adjacent seeds");
                ops.push(Op::Hop { dir: d, finalize: false });
                walk(SEED_P, &map, visited, ops, place);
                ops.push(Op::Hop { dir: d.opp(), finalize: false });
            }
        };
        pass(ops, visited)
    };

    let mut visited: BTreeSet<Pos> = [SEED_C, SEED_P].into();
    bridge(&mut ops, &mut visited, true);
    for (p, _) in extras {
        assert!(visited.contains(p), "{name}: blocklet cell {p:?} unreachable");
    }
    let resolve_pc = ops.len() as u16;

    if extras.iter().any(|(_, c)| matches!(c, Cell::Agent { .. })) {
        let mut visited: BTreeSet<Pos> = [SEED_C, SEED_P].into();
        bridge(&mut ops, &mut visited, false);
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
    Word2::pack(&Site::of(fc)).is_ok() && Word2::pack(&Site::of(fp)).is_ok()
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
    for (lane, (ci, pj)) in l.cp_wires.iter().enumerate() {
        c_routes.push(Route::new(
            cstub[*ci as usize - 1],
            EndPt { face: axis, lane: lane as u8 },
        ));
        p_routes.push(Route::new(
            pstub[*pj as usize - 1],
            EndPt { face: axis.opp(), lane: lane as u8 },
        ));
    }

    // Inherited passthroughs join the panel; an over-capacity result fails to pack and
    // is rejected by finals_fit at dock time.
    c_routes.extend(cpass.iter().copied());
    p_routes.extend(ppass.iter().copied());
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
