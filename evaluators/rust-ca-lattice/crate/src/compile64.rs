//! Compiled face-relative rewrite workshops for the complete interaction ROM.
//!
//! A·F keeps its hand-crafted workshop ([`apply_fork_workshop`]). Every other rule uses a
//! canonical searched patch per handedness, cached on first use and rotated into the
//! requested `(axis, side, lift)` orientation. The live request/place protocol in
//! [`packed_local`](crate::packed_local) is generic over the workshops produced here.
//!
//! Workshop invariants the protocol relies on:
//!
//! - slot 0 is the consumer (driver) cell, and the request-tree BFS tries the principal
//!   axis first so the docked producer is always slot 1;
//! - the patch is connected through face adjacency and every word packs into one `u64`;
//! - a dying arity-three agent's cell becomes a boundary `Zip` (trunk outward, branches
//!   into the patch; runtime sets twist from the dying agent's aux flip), an arity-two
//!   agent's cell becomes a one-lane boundary `Link`, and an arity-one cell becomes
//!   routing space or `Empty`;
//! - fresh arity-three agents sit adjacent to their own zipper (trunk toward the agent;
//!   branch i carries semantic port i+1, twist false), fresh arity-two agents get a tail
//!   link, and every net of the rule's wiring permutation is a face-reciprocal one-lane
//!   route between those port cells.
//!
//! The search is fully deterministic: a one-sided seat library north of the fixed docked
//! pair, boundary/zipper branch faces from ordered candidate lists, a seat assignment
//! ranked by a Manhattan lower bound, and a per-net Dijkstra router over z ∈ {0, 1} (the
//! bilayer layers) with rip-up retries over a few net orders. A candidate is accepted only
//! when it packs, connects, fits the 63-slot budget, and — checked with the real fixture —
//! projects exactly onto the fired shadow net. The mirror canonical is the y-reflection of
//! the searched one: it keeps z ∈ {0, 1} (bilayer-safe fallback requests) and puts the
//! fallback region on the opposite side of the pair. Behind an arity-one producer, the two
//! canonical cells along the axis are additional obstacles: in every real context the
//! producer arrived from behind, leaving guest-crossing residue there.

use crate::cell64::{
    CellWord, Control, DecodedCell, Dir, FacePair, LaneMask, Matter, Pull, RewriteRole, U3,
    DIRS,
};
use crate::lattice::{dir_to, manhattan, step, Pos, Topo};
use crate::rewrite64::{
    apply_compiled_patch, apply_fork_workshop, cold_link, docked_fixture64, RewritePatch64,
    Workshop64, WorkshopSlot64, APPLY_FORK_RULE,
};
use crate::rules::{End, Rule, Tag, RULES};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, VecDeque};
use std::sync::OnceLock;

/// Canonical patch cache: (ROM rule index, left-handed) → workshop in the canonical
/// orientation (axis E, side N, lift U for right-handed targets; lift D otherwise).
static CANONICAL: OnceLock<BTreeMap<(u8, bool), Workshop64>> = OnceLock::new();

/// The workshop for `rule` (a ROM index) at face-relative orientation `(axis, side, lift)`.
pub fn workshop_for(rule: u8, axis: Dir, side: Dir, lift: Dir) -> Option<Workshop64> {
    if !axis.perp().contains(&side) { return None; }
    if rule == crate::rewrite64::APPLY_FORK_RULE {
        return apply_fork_workshop(axis, side, lift, false, false);
    }
    let canonical = CANONICAL.get_or_init(compile_all);
    let workshop = canonical.get(&(rule, left_handed(axis, side, lift)))?;
    Some(rotate(workshop, axis, side, lift))
}

/// Signed volume of the orientation frame: ±1. Canonical E,N,U is left-handed (−1).
fn left_handed(axis: Dir, side: Dir, lift: Dir) -> bool {
    frame_handedness(axis, side, lift) < 0
}

fn frame_handedness(axis: Dir, side: Dir, lift: Dir) -> i32 {
    let a = dir_vec(axis);
    let s = dir_vec(side);
    let cross = (
        a.1 * s.2 - a.2 * s.1,
        a.2 * s.0 - a.0 * s.2,
        a.0 * s.1 - a.1 * s.0,
    );
    let l = dir_vec(lift);
    cross.0 * l.0 + cross.1 * l.1 + cross.2 * l.2
}

/// Lattice direction as a coordinate vector: E +x, N −y, U +z.
pub fn dir_vec(d: Dir) -> (i32, i32, i32) {
    match d {
        Dir::E => (1, 0, 0),
        Dir::W => (-1, 0, 0),
        Dir::N => (0, -1, 0),
        Dir::S => (0, 1, 0),
        Dir::U => (0, 0, 1),
        Dir::D => (0, 0, -1),
    }
}

pub fn vec_dir(v: (i32, i32, i32)) -> Option<Dir> {
    Some(match v {
        (1, 0, 0) => Dir::E,
        (-1, 0, 0) => Dir::W,
        (0, -1, 0) => Dir::N,
        (0, 1, 0) => Dir::S,
        (0, 0, 1) => Dir::U,
        (0, 0, -1) => Dir::D,
        _ => return None,
    })
}

// -------------------------------------------------------------------------------------------
// Rotation between orientation frames.
//
// A frame is the orthonormal basis (axis, side, lift) with the same handedness as the
// canonical basis (E, N, canonical_lift). Coordinates are converted by expanding a vector
// in the canonical basis and recombining in the target basis; handedness equality keeps
// the map a proper rotation, so faces stay faces and adjacency is preserved.
// -------------------------------------------------------------------------------------------

fn frame_rotate_vec(
    v: (i32, i32, i32),
    axis: Dir,
    side: Dir,
    lift: Dir,
    canonical_lift: Dir,
) -> (i32, i32, i32) {
    let cz = if canonical_lift == Dir::U { 1 } else { -1 };
    // Coordinates of v in the canonical basis (E, N, (0,0,cz)).
    let (x, y, z) = (v.0, -v.1, cz * v.2);
    let (a, s, l) = (dir_vec(axis), dir_vec(side), dir_vec(lift));
    (
        x * a.0 + y * s.0 + z * l.0,
        x * a.1 + y * s.1 + z * l.1,
        x * a.2 + y * s.2 + z * l.2,
    )
}

fn rotate_matter(matter: Matter, rd: &dyn Fn(Dir) -> Dir) -> Matter {
    match matter {
        Matter::Empty => Matter::Empty,
        Matter::Agent { tag, principal, tail, aux_flip } => Matter::Agent {
            tag,
            principal: rd(principal),
            tail: tail.map(rd),
            aux_flip,
        },
        Matter::Link { ends, lanes, twist, hot, cooldown, pull } => Matter::Link {
            ends: FacePair::new(rd(ends.a), rd(ends.b))
                .expect("rotation preserves distinct faces"),
            lanes,
            twist,
            hot,
            cooldown,
            pull,
        },
        Matter::Zip { trunk, branches, twist, hot, cooldown, pull } => Matter::Zip {
            trunk: rd(trunk),
            // Branch order is semantic (branch i carries port i+1): rotate each face but
            // never reorder, and never touch the twist.
            branches: [rd(branches[0]), rd(branches[1])],
            twist,
            hot,
            cooldown,
            pull,
        },
        Matter::Cross { routes, hot, cooldown, pull } => Matter::Cross {
            routes: routes.map(|route| {
                FacePair::new(rd(route.a), rd(route.b))
                    .expect("rotation preserves distinct faces")
            }),
            hot,
            cooldown,
            pull,
        },
        // Guests never appear in workshop matter; the arms exist for exhaustive rotation.
        Matter::GuestZip { tag, plane, trunk, branches, twist, deep } => Matter::GuestZip {
            tag,
            plane,
            trunk: rd(trunk),
            branches: [rd(branches[0]), rd(branches[1])],
            twist,
            deep,
        },
        Matter::GuestLink { tag, principal, plane, ends, twist } => Matter::GuestLink {
            tag,
            principal: rd(principal),
            plane,
            ends: FacePair::new(rd(ends.a), rd(ends.b))
                .expect("rotation preserves distinct faces"),
            twist,
        },
    }
}

/// Rotate a workshop expressed in the canonical (E, N, `canonical_lift`) frame into the
/// `(axis, side, lift)` frame. Slot order, roles, and fresh indices are untouched.
fn rotate_into(
    workshop: &Workshop64,
    axis: Dir,
    side: Dir,
    lift: Dir,
    canonical_lift: Dir,
) -> Workshop64 {
    let rd = |d: Dir| {
        vec_dir(frame_rotate_vec(dir_vec(d), axis, side, lift, canonical_lift))
            .expect("frame rotation maps a face to a face")
    };
    let rp = |p: Pos| frame_rotate_vec(p, axis, side, lift, canonical_lift);
    let slots = workshop
        .slots
        .iter()
        .map(|slot| WorkshopSlot64 {
            at: rp(slot.at),
            matter: rotate_matter(slot.matter, &rd),
            parent: slot.parent.map(rd),
            children: slot.children.iter().map(|d| rd(*d)).collect(),
            role: slot.role,
            fresh: slot.fresh,
        })
        .collect();
    Workshop64 { slots }
}

/// Rotate a canonical workshop into the requested orientation frame.
fn rotate(workshop: &Workshop64, axis: Dir, side: Dir, lift: Dir) -> Workshop64 {
    let canonical_lift = if frame_handedness(axis, side, lift) < 0 { Dir::U } else { Dir::D };
    rotate_into(workshop, axis, side, lift, canonical_lift)
}

// -------------------------------------------------------------------------------------------
// The canonical search.
//
// Fixed frame: consumer at (0,0,0) principal E, producer at (1,0,0) principal W, exactly
// the pose `docked_fixture64` builds. The patch avoids every fixture-occupied cell, so the
// live protocol's emptiness requirement holds by construction.
// -------------------------------------------------------------------------------------------

const CONSUMER: Pos = (0, 0, 0);
const PRODUCER: Pos = (1, 0, 0);
const AXIS: Dir = Dir::E;

/// Router region: one-sided (north of the pair) and inside the two bilayer layers.
///
/// The one-sided layout is what makes the live side iteration meaningful: in side-in-plane
/// frames the preferred and fallback lifts put the patch on OPPOSITE world sides of the
/// driver, so a blocked first request retries into a genuinely different region. It also
/// keeps every patch cell at canonical y ≤ 0, so the side=E/W frames a driver picks for a
/// bent in-plane tail never place a slot on the tail-cable cell, and z ∈ {0, 1} keeps
/// side-in-plane requests in bounds on the bilayer topology.
const REGION_LO: Pos = (-6, -10, 0);
const REGION_HI: Pos = (10, 0, 1);

fn in_region(p: Pos) -> bool {
    p.0 >= REGION_LO.0 && p.0 <= REGION_HI.0
        && p.1 >= REGION_LO.1 && p.1 <= REGION_HI.1
        && p.2 >= REGION_LO.2 && p.2 <= REGION_HI.2
}

fn cold_zip(trunk: Dir, branches: [Dir; 2]) -> Matter {
    Matter::Zip {
        trunk,
        branches,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    }
}

/// Final matter of a dying agent's cell: arity three → boundary zipper, arity two →
/// one-lane boundary link, arity one → free (routing space or emptiness).
fn boundary_matter(tag: Tag, trunk: Dir, branches: &[Dir]) -> Matter {
    match tag.arity() {
        1 => Matter::Empty,
        2 => cold_link(trunk, branches[0]),
        3 => cold_zip(trunk, [branches[0], branches[1]]),
        _ => unreachable!("the lowered alphabet caps arity at three"),
    }
}

/// A fresh-agent seat: the agent cell, its principal face, and its tail face (the zipper
/// of an arity-three seat sits one step along the tail face, trunk pointing back).
#[derive(Clone, Copy)]
struct Seat {
    pos: Pos,
    principal: Dir,
    tail: Dir,
}

/// Deterministic seat library, all north of the pair (the patch is one-sided). Each seat
/// is a horizontal domino with its zipper in-row to the west, so the odd rows hold every
/// fixed cell and the even rows (y = −1, −3, −5) stay pure routing corridors.
/// Principals face U: principal nets start in the z = 1 overpass layer, leaving the z = 0
/// corridors to the boundary and zipper nets. Every position is empty in every docked
/// fixture (fixture cables live at y = 0 and x ∈ {−2, −1, 3}, with pads only at (±2
/// off-axis) at x ∈ {−2, 3} — those rules simply cannot use the (−1,−2) seat).
const SEAT_LIBRARY: [Seat; 16] = [
    Seat { pos: (1, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (0, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (-1, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (2, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (3, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (5, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (0, -4, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (-2, -4, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (2, -4, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (4, -4, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (-3, -2, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (6, -4, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (-1, -6, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (1, -6, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (3, -6, 0), principal: Dir::U, tail: Dir::W },
    Seat { pos: (5, -6, 0), principal: Dir::U, tail: Dir::W },
];

/// Ordered branch-face candidates for a fresh agent's zipper. The pair order is semantic
/// (branch[0] carries port 1); any geometrically valid pair works, so this list is purely
/// a search heuristic. U-first: the z = 1 overpass layer is the roomy one, and a domino
/// zipper's U endpoint always has open lateral neighbors. Faces equal to the trunk are
/// filtered per zipper.
const ZIP_BRANCHES: &[[Dir; 2]] = &[
    [Dir::U, Dir::N], [Dir::N, Dir::U],
    [Dir::U, Dir::S], [Dir::S, Dir::U],
    [Dir::U, Dir::W], [Dir::W, Dir::U],
    [Dir::N, Dir::S], [Dir::S, Dir::N],
    [Dir::N, Dir::W], [Dir::W, Dir::N], [Dir::S, Dir::W], [Dir::W, Dir::S],
    [Dir::N, Dir::D], [Dir::D, Dir::N], [Dir::S, Dir::D], [Dir::D, Dir::S],
    [Dir::U, Dir::D], [Dir::D, Dir::U], [Dir::W, Dir::D], [Dir::D, Dir::W],
    [Dir::N, Dir::E], [Dir::E, Dir::N], [Dir::S, Dir::E], [Dir::E, Dir::S],
    [Dir::U, Dir::E], [Dir::E, Dir::U], [Dir::W, Dir::E], [Dir::E, Dir::W],
    [Dir::D, Dir::E], [Dir::E, Dir::D],
];

/// Ordered branch candidates for a boundary zipper (trunk W for the consumer, E for the
/// producer; branches point into the one-sided patch — overpass first, north corridor
/// second — and never along the axis toward the partner).
const ZIP_BOUNDARY_BRANCHES: &[[Dir; 2]] = &[
    [Dir::U, Dir::N], [Dir::N, Dir::U],
    [Dir::N, Dir::D], [Dir::D, Dir::N], [Dir::U, Dir::D], [Dir::D, Dir::U],
    [Dir::N, Dir::S], [Dir::S, Dir::N],
    [Dir::S, Dir::U], [Dir::U, Dir::S], [Dir::S, Dir::D], [Dir::D, Dir::S],
];

/// Ordered face candidates for a boundary one-lane link (arity-two dying agent).
const LINK_BOUNDARY_FACES: &[Dir] = &[Dir::U, Dir::N, Dir::D, Dir::S];

fn boundary_variants(arity: usize) -> Vec<Vec<Dir>> {
    match arity {
        1 => vec![vec![]],
        2 => LINK_BOUNDARY_FACES.iter().map(|&f| vec![f]).collect(),
        3 => ZIP_BOUNDARY_BRANCHES.iter().map(|pair| pair.to_vec()).collect(),
        _ => unreachable!(),
    }
}

/// Cells the docked fixture occupies outside the pair itself: patch cells must avoid all
/// of them so every non-pair slot accepts the live request (pre-matter Empty). An arity-one
/// producer has no tail cable in the fixture, but in every real context it ARRIVED from
/// behind — leaving guest-crossing residue (one-lane elbows and infrastructure zippers) on
/// the canonical axis behind it — so (2,0,0) and (3,0,0) are obstacles there too.
fn fixture_obstacles(rule: &Rule) -> BTreeSet<Pos> {
    let (grid, _, _) = docked_fixture64(rule.consumer, rule.producer, Topo::Full3D);
    let mut obstacles: BTreeSet<Pos> = grid
        .cells
        .keys()
        .copied()
        .filter(|p| *p != CONSUMER && *p != PRODUCER)
        .collect();
    if rule.producer.arity() == 1 {
        obstacles.insert(step(PRODUCER, AXIS));
        obstacles.insert(step(step(PRODUCER, AXIS), AXIS));
    }
    obstacles
}

/// One endpoint of a rule wire, resolved to a patch cell and a face.
#[derive(Clone, Copy, Debug)]
struct Terminal {
    cell: Pos,
    face: Dir,
}

type Net = (Terminal, Terminal);

struct Placement {
    /// Boundary cells, fresh seats, and fresh zippers (fixed matter of the patch).
    fixed: BTreeMap<Pos, Matter>,
    /// One net per rule wire, in rule order.
    nets: Vec<Net>,
    /// Fresh index → seat cell (the `fresh` field of its workshop slot).
    seats: Vec<Pos>,
}

/// Approximate endpoint cell for the seat-assignment lower bound (seat cells stand in for
/// all of a fresh agent's ports; boundary ports stand in for the pair cells).
fn approx_cell(rule: &Rule, end: End, seats: &[usize]) -> Pos {
    match end {
        End::CAux(_) => CONSUMER,
        End::PAux(_) => PRODUCER,
        End::Fresh(k, 0) => SEAT_LIBRARY[seats[k as usize]].pos,
        End::Fresh(k, _) => {
            let seat = SEAT_LIBRARY[seats[k as usize]];
            if rule.fresh[k as usize].arity() == 3 {
                step(seat.pos, seat.tail) // the zipper cell
            } else {
                seat.pos
            }
        }
    }
}

/// Sum of Manhattan distances between wire endpoints: the routing lower bound used to
/// rank seat assignments (fewer, shorter nets first).
fn seat_lower_bound(rule: &Rule, seats: &[usize]) -> i32 {
    rule.wires
        .iter()
        .map(|&(a, b)| manhattan(approx_cell(rule, a, seats), approx_cell(rule, b, seats)))
        .sum()
}

/// Every injective assignment of fresh agents to library seats, best lower bound first,
/// capped at sixteen. Seats are never offered when they collide with this rule's fixture
/// obstacles (their own cell, or the zipper cell an arity-three fresh would need) or with
/// an already-chosen seat's domino (seat cells and arity-three zipper cells are mutually
/// exclusive). Ties break by assignment order, so the result is deterministic.
fn seat_assignments(rule: &Rule, obstacles: &BTreeSet<Pos>) -> Vec<Vec<usize>> {
    if rule.fresh.is_empty() { return vec![vec![]]; }
    let allowed: Vec<Vec<usize>> = rule
        .fresh
        .iter()
        .map(|tag| {
            (0..SEAT_LIBRARY.len())
                .filter(|&i| {
                    let seat = SEAT_LIBRARY[i];
                    !obstacles.contains(&seat.pos)
                        && (tag.arity() != 3
                            || !obstacles.contains(&step(seat.pos, seat.tail)))
                })
                .collect()
        })
        .collect();
    let mut scored: Vec<(i32, Vec<usize>)> = Vec::new();
    let mut current: Vec<usize> = Vec::with_capacity(rule.fresh.len());
    let mut used = [false; SEAT_LIBRARY.len()];
    let mut taken: BTreeSet<Pos> = BTreeSet::new(); // seat and zipper cells of the dominoes
    fn enumerate(
        rule: &Rule,
        allowed: &[Vec<usize>],
        used: &mut [bool; SEAT_LIBRARY.len()],
        taken: &mut BTreeSet<Pos>,
        current: &mut Vec<usize>,
        scored: &mut Vec<(i32, Vec<usize>)>,
    ) {
        let k = current.len();
        if k == allowed.len() {
            scored.push((seat_lower_bound(rule, current), current.clone()));
            return;
        }
        for &i in &allowed[k] {
            if used[i] { continue; }
            let seat = SEAT_LIBRARY[i];
            if taken.contains(&seat.pos) { continue; }
            let zip = (rule.fresh[k].arity() == 3).then(|| step(seat.pos, seat.tail));
            if zip.is_some_and(|z| taken.contains(&z)) { continue; }
            used[i] = true;
            taken.insert(seat.pos);
            if let Some(z) = zip { taken.insert(z); }
            current.push(i);
            enumerate(rule, allowed, used, taken, current, scored);
            current.pop();
            taken.remove(&seat.pos);
            if let Some(z) = zip { taken.remove(&z); }
            used[i] = false;
        }
    }
    enumerate(rule, &allowed, &mut used, &mut taken, &mut current, &mut scored);
    scored.sort();
    scored.truncate(160);
    scored.into_iter().map(|(_, seats)| seats).collect()
}

/// Lay out one placement variant: boundary matter, seats, zippers (first-fit branches),
/// and the netlist with its direct/single-cell degenerate cases validated. Returns None
/// on any structural conflict so the caller advances to the next variant.
fn place(
    rule: &Rule,
    consumer_branches: &[Dir],
    producer_branches: &[Dir],
    seat_ids: &[usize],
    obstacles: &BTreeSet<Pos>,
) -> Option<Placement> {
    let mut fixed: BTreeMap<Pos, Matter> = BTreeMap::new();

    // The dying pair's boundary matter.
    fixed.insert(CONSUMER, boundary_matter(rule.consumer, Dir::W, consumer_branches));
    fixed.insert(PRODUCER, boundary_matter(rule.producer, Dir::E, producer_branches));

    // "Likely" route-start cells claimed by boundary and seat terminals. Zipper branches
    // steer clear of them; the net builder below owns them per net and turns a same-net
    // coincidence into a zero/one-cell route instead of a conflict.
    let mut likely_starts: BTreeSet<Pos> = BTreeSet::new();
    for &face in consumer_branches { likely_starts.insert(step(CONSUMER, face)); }
    for &face in producer_branches { likely_starts.insert(step(PRODUCER, face)); }

    // Fresh seats and their own terminal faces.
    let mut seats: Vec<Pos> = Vec::with_capacity(rule.fresh.len());
    let mut zippers: Vec<Option<(Pos, [Dir; 2])>> = Vec::with_capacity(rule.fresh.len());
    for (k, &tag) in rule.fresh.iter().enumerate() {
        let seat = SEAT_LIBRARY[seat_ids[k]];
        if obstacles.contains(&seat.pos) || fixed.contains_key(&seat.pos) {
            return None;
        }
        fixed.insert(seat.pos, Matter::Agent {
            tag,
            principal: seat.principal,
            tail: (tag.arity() > 1).then_some(seat.tail),
            aux_flip: false,
        });
        seats.push(seat.pos);
        likely_starts.insert(step(seat.pos, seat.principal));
        if tag.arity() == 2 {
            likely_starts.insert(step(seat.pos, seat.tail));
        }
        zippers.push(None);
    }

    // Fresh arity-three zippers: first-fit branch faces whose route starts are free.
    // Branch endpoints must dodge every zipper cell of this assignment, including the
    // ones not placed yet.
    let zip_cells: BTreeSet<Pos> = rule
        .fresh
        .iter()
        .enumerate()
        .filter(|(_, tag)| tag.arity() == 3)
        .map(|(k, _)| {
            let seat = SEAT_LIBRARY[seat_ids[k]];
            step(seat.pos, seat.tail)
        })
        .collect();
    for (k, &tag) in rule.fresh.iter().enumerate() {
        if tag.arity() != 3 { continue; }
        let seat = SEAT_LIBRARY[seat_ids[k]];
        let zip_at = step(seat.pos, seat.tail);
        let trunk = seat.tail.opp();
        if obstacles.contains(&zip_at) || fixed.contains_key(&zip_at) { return None; }
        let branches = ZIP_BRANCHES.iter().copied().find(|pair| {
            if pair.contains(&trunk) { return false; }
            pair.iter().all(|&face| {
                let at = step(zip_at, face);
                in_region(at)
                    && !obstacles.contains(&at)
                    && !fixed.contains_key(&at)
                    && !zip_cells.contains(&at)
                    && !likely_starts.contains(&at)
            })
        });
        let Some(branches) = branches else { return None };
        for &face in &branches { likely_starts.insert(step(zip_at, face)); }
        fixed.insert(zip_at, cold_zip(trunk, branches));
        zippers[k] = Some((zip_at, branches));
    }

    // Resolve the rule's wiring permutation into concrete terminal pairs.
    let terminal = |end: End| -> Terminal {
        match end {
            End::CAux(i) => Terminal { cell: CONSUMER, face: consumer_branches[i as usize - 1] },
            End::PAux(j) => Terminal { cell: PRODUCER, face: producer_branches[j as usize - 1] },
            End::Fresh(k, 0) => {
                let seat = SEAT_LIBRARY[seat_ids[k as usize]];
                Terminal { cell: seat.pos, face: seat.principal }
            }
            End::Fresh(k, p) => {
                let seat = SEAT_LIBRARY[seat_ids[k as usize]];
                match seat_agent_arity(rule, k) {
                    2 => Terminal { cell: seat.pos, face: seat.tail },
                    3 => {
                        let (zip_at, branches) = zippers[k as usize].expect("zipper placed");
                        Terminal { cell: zip_at, face: branches[p as usize - 1] }
                    }
                    _ => unreachable!("arity-one fresh agents have no aux ports"),
                }
            }
        }
    };
    let mut nets: Vec<Net> = Vec::with_capacity(rule.wires.len());
    let mut starts: BTreeMap<Pos, usize> = BTreeMap::new();
    for (index, &(a, b)) in rule.wires.iter().enumerate() {
        let (ta, tb) = (terminal(a), terminal(b));
        let (sa, sb) = (step(ta.cell, ta.face), step(tb.cell, tb.face));
        if sa == tb.cell {
            // Direct face-reciprocal adjacency: no link cells at all.
            if sb != ta.cell {
                return None;
            }
        } else if sa == sb {
            // Both endpoints meet in one shared link cell.
            if fixed.contains_key(&sa) || obstacles.contains(&sa) || !in_region(sa) {
                return None;
            }
            if starts.insert(sa, index).is_some() { return None; }
        } else {
            for start in [sa, sb] {
                if fixed.contains_key(&start) || obstacles.contains(&start)
                    || !in_region(start)
                {
                    return None;
                }
                if starts.insert(start, index).is_some() { return None; }
            }
        }
        nets.push((ta, tb));
    }
    Some(Placement { fixed, nets, seats })
}

fn seat_agent_arity(rule: &Rule, k: u8) -> usize {
    rule.fresh[k as usize].arity()
}

/// Dijkstra route from `start` to `end` through free in-region cells, minimizing total
/// cost: 1 per cell plus the negotiated-congestion surcharge for cells other nets hold.
/// Returns the full cell path (both endpoints included); every cell becomes a one-lane
/// link. Deterministic via the (cost, position) heap order.
fn route(
    start: Pos,
    end: Pos,
    blocked: &BTreeSet<Pos>,
    surcharge: &BTreeMap<Pos, u32>,
) -> Option<Vec<Pos>> {
    let mut dist: BTreeMap<Pos, u32> = BTreeMap::new();
    let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut heap: BinaryHeap<std::cmp::Reverse<(u32, Pos)>> = BinaryHeap::new();
    dist.insert(start, 0);
    heap.push(std::cmp::Reverse((0, start)));
    let mut expansions = 0u32;
    while let Some(std::cmp::Reverse((cost, p))) = heap.pop() {
        if dist.get(&p) != Some(&cost) { continue; }
        if p == end {
            let mut path = vec![p];
            while let Some(&q) = prev.get(path.last().unwrap()) { path.push(q); }
            path.reverse();
            return Some(path);
        }
        expansions += 1;
        if expansions > 100_000 { return None; }
        for d in DIRS {
            let q = step(p, d);
            if !in_region(q) || (blocked.contains(&q) && q != end) { continue; }
            let next = cost + 1 + surcharge.get(&q).copied().unwrap_or(0);
            if dist.get(&q).is_none_or(|&old| next < old) {
                dist.insert(q, next);
                prev.insert(q, p);
                heap.push(std::cmp::Reverse((next, q)));
            }
        }
    }
    None
}

/// Deterministic net orders: span-descending/ascending, rule order and its reverse, plus
/// a few splitmix shuffles seeded per placement — rip-up by permutation.
fn net_orders(nets: &[Net], seed: u64) -> Vec<Vec<usize>> {
    let identity: Vec<usize> = (0..nets.len()).collect();
    let mut by_span = identity.clone();
    by_span.sort_by_key(|&i| {
        let (a, b) = nets[i];
        std::cmp::Reverse(manhattan(
            step(a.cell, a.face),
            step(b.cell, b.face),
        ))
    });
    let mut by_span_asc = identity.clone();
    by_span_asc.sort_by_key(|&i| {
        let (a, b) = nets[i];
        manhattan(step(a.cell, a.face), step(b.cell, b.face))
    });
    let reversed: Vec<usize> = identity.iter().rev().copied().collect();
    let mut orders = vec![by_span, by_span_asc, identity, reversed];
    let mut random = seed
        .wrapping_mul(0x9e37_79b9_7f4a_7c15)
        .wrapping_add(0xd1b5_4a32_d192_ed03);
    for _ in 0..8 {
        let mut order: Vec<usize> = (0..nets.len()).collect();
        for i in (1..order.len()).rev() {
            random ^= random << 13;
            random ^= random >> 7;
            random ^= random << 17;
            let j = (random % (i as u64 + 1)) as usize;
            order.swap(i, j);
        }
        orders.push(order);
    }
    orders
}

/// Why a candidate failed, for the honest exhaustion report.
#[derive(Clone, Copy, Debug)]
enum Fail {
    Route,
    TooManySlots,
    Disconnected,
    Pack,
    Projection,
}

/// Route every net of a placement and assemble the workshop. Routing uses negotiated
/// congestion: nets may share cells at a surcharge; each iteration rips out and re-routes
/// every net against the current occupancy, and the surcharge on contended cells grows
/// until the wiring deconflicts. Accepted only when the patch packs, connects, fits the
/// slot budget, and projects exactly onto the fired shadow net (the real fixture is the
/// acceptance oracle).
fn build_candidate(
    rule_index: usize,
    placed: &Placement,
    order: &[usize],
    obstacles: &BTreeSet<Pos>,
) -> Result<Workshop64, Fail> {
    // Route-start cells of every net, for ownership during routing.
    let start_of = |net: &Net| -> (Pos, Pos) {
        (step(net.0.cell, net.0.face), step(net.1.cell, net.1.face))
    };

    // Hard blockers shared by every net: obstacles, fixed matter, and other nets' starts.
    let starts: Vec<(Pos, Pos)> = placed.nets.iter().map(start_of).collect();
    let hard = |skip: usize| -> BTreeSet<Pos> {
        let mut blocked = obstacles.clone();
        blocked.extend(placed.fixed.keys().copied());
        for (other, &(sa, sb)) in starts.iter().enumerate() {
            if other == skip { continue; }
            blocked.insert(sa);
            blocked.insert(sb);
        }
        blocked
    };

    let mut paths: Vec<Option<Vec<Pos>>> = vec![None; placed.nets.len()];
    let mut singles: Vec<(Pos, FacePair)> = vec![];
    let mut occupancy: BTreeMap<Pos, u32> = BTreeMap::new();
    let mut history: BTreeMap<Pos, u32> = BTreeMap::new();

    // Committed cells first: single-cell routes and direct adjacencies need no search.
    for (index, net) in placed.nets.iter().enumerate() {
        let (sa, sb) = starts[index];
        if sa == net.1.cell {
            paths[index] = Some(vec![]); // direct face-reciprocal adjacency
        } else if sa == sb {
            let a = dir_to(sa, net.0.cell).ok_or(Fail::Route)?;
            let b = dir_to(sa, net.1.cell).ok_or(Fail::Route)?;
            singles.push((sa, FacePair::new(a, b).ok_or(Fail::Route)?));
            *occupancy.entry(sa).or_insert(0) += 1;
            paths[index] = Some(vec![sa]);
        }
    }

    for iteration in 0..24 {
        for &index in order {
            let net = placed.nets[index];
            let (sa, sb) = starts[index];
            if sa == net.1.cell || sa == sb { continue; }
            if let Some(old) = paths[index].take() {
                for cell in old {
                    occupancy.entry(cell).and_modify(|n| *n -= 1);
                }
            }
            let surcharge: BTreeMap<Pos, u32> = occupancy
                .iter()
                .map(|(&cell, &count)| {
                    (cell, count * 2 + history.get(&cell).copied().unwrap_or(0))
                })
                .collect();
            let Some(path) = route(sa, sb, &hard(index), &surcharge) else {
                return Err(Fail::Route);
            };
            for &cell in &path { *occupancy.entry(cell).or_insert(0) += 1; }
            paths[index] = Some(path);
        }
        let contended: Vec<Pos> = occupancy
            .iter()
            .filter(|(_, &count)| count > 1)
            .map(|(&cell, _)| cell)
            .collect();
        if contended.is_empty() { break; }
        for cell in contended { *history.entry(cell).or_insert(0) += 2; }
        if iteration == 23 {
            return Err(Fail::Route);
        }
    }

    // Materialize every route as one-lane links, checking face reciprocity per cell.
    let mut matter = placed.fixed.clone();
    for (sa, ends) in singles {
        if matter.insert(sa, cold_link(ends.a, ends.b)).is_some() {
            return Err(Fail::Route);
        }
    }
    for (index, path) in paths.iter().enumerate() {
        let net = placed.nets[index];
        let Some(path) = path else { continue };
        let sa = step(net.0.cell, net.0.face);
        if sa == net.1.cell { continue; }
        for (i, &p) in path.iter().enumerate() {
            let prev = if i == 0 { net.0.cell } else { path[i - 1] };
            let next = if i + 1 == path.len() { net.1.cell } else { path[i + 1] };
            let a = dir_to(p, prev).ok_or(Fail::Route)?;
            let b = dir_to(p, next).ok_or(Fail::Route)?;
            let ends = FacePair::new(a, b).ok_or(Fail::Route)?;
            if matter.insert(p, cold_link(ends.a, ends.b)).is_some() {
                return Err(Fail::Route);
            }
        }
    }

    let workshop = build_workshop(&matter, &placed.seats)?;
    if workshop.slots.iter().any(|slot| !packs(slot.matter)) { return Err(Fail::Pack); }
    if !projection_valid(rule_index, &workshop) { return Err(Fail::Projection); }
    Ok(workshop)
}

fn packs(matter: Matter) -> bool {
    CellWord::pack(DecodedCell { matter, control: Control::Idle, chi: 0, sigma: 0 }).is_ok()
}

/// Give the connected patch its deterministic rooted request tree, exactly like the
/// hand-crafted workshop: slot zero is the driver, and trying the principal axis first
/// makes the docked producer slot one.
fn build_workshop(
    matter: &BTreeMap<Pos, Matter>,
    seats: &[Pos],
) -> Result<Workshop64, Fail> {
    if matter.len() > 63 {
        return Err(Fail::TooManySlots);
    }
    let mut order = vec![CONSUMER];
    let mut parents: BTreeMap<Pos, Dir> = BTreeMap::new();
    let mut queue = VecDeque::from([CONSUMER]);
    while let Some(parent) = queue.pop_front() {
        for face in std::iter::once(AXIS).chain(DIRS) {
            let child = step(parent, face);
            if !matter.contains_key(&child) || order.contains(&child) { continue; }
            order.push(child);
            parents.insert(child, face.opp());
            queue.push_back(child);
        }
    }
    if order.len() != matter.len() { return Err(Fail::Disconnected); }
    if order.get(1) != Some(&PRODUCER) { return Err(Fail::Disconnected); }

    let slots = order
        .iter()
        .enumerate()
        .map(|(index, at)| {
            let final_matter = matter[at];
            let children = order
                .iter()
                .filter_map(|candidate| {
                    (parents.get(candidate).is_some_and(|parent_face| {
                        step(*candidate, *parent_face) == *at
                    }))
                    .then(|| dir_to(*at, *candidate))
                    .flatten()
                })
                .collect();
            let fresh = seats.iter().position(|seat| seat == at).map(|k| k as u8);
            let role = if index == 0 {
                RewriteRole::Driver
            } else if *at == PRODUCER || matches!(final_matter, Matter::Zip { .. }) {
                RewriteRole::Boundary
            } else if matches!(final_matter, Matter::Agent { .. }) {
                RewriteRole::Seat
            } else {
                RewriteRole::Wire
            };
            WorkshopSlot64 {
                at: *at,
                matter: final_matter,
                parent: parents.get(at).copied(),
                children,
                role,
                fresh,
            }
        })
        .collect();
    Ok(Workshop64 { slots })
}

/// The acceptance oracle: fire the rule in the real docked fixture, stamp the patch in
/// with `apply_compiled_patch`, and require the packed lattice to project exactly onto
/// the shadow net. Runs under `catch_unwind` so a bad candidate just falls through.
fn projection_valid(rule_index: usize, workshop: &Workshop64) -> bool {
    let rule = &RULES[rule_index];
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let (mut grid, mut shadow, _) =
            docked_fixture64(rule.consumer, rule.producer, Topo::Full3D);
        let patch = canonical_patch(workshop);
        apply_compiled_patch(&mut grid, &mut shadow, &patch);
        grid.check_projection(&shadow);
    }))
    .is_ok()
}

/// The canonical (axis E) patch form of a workshop, as `apply_compiled_patch` consumes it.
fn canonical_patch(workshop: &Workshop64) -> RewritePatch64 {
    let matter = workshop.slots.iter().map(|slot| (slot.at, slot.matter)).collect();
    let mut fresh: Vec<(u8, Pos)> = workshop
        .slots
        .iter()
        .filter_map(|slot| slot.fresh.map(|index| (index, slot.at)))
        .collect();
    fresh.sort();
    RewritePatch64 {
        consumer: CONSUMER,
        producer: PRODUCER,
        fresh: fresh.into_iter().map(|(_, at)| at).collect(),
        matter,
        axis: AXIS,
        side: Dir::N,
        lift: Dir::U,
    }
}

/// Search one canonical (E, N, U) workshop for a ROM rule. Deterministic: same variant
/// order every process. Panics with the rule name if the bounded search space is
/// exhausted — a missing workshop is a hard compiler bug, not a None.
fn search_workshop(rule_index: usize, rule: &'static Rule) -> Workshop64 {
    let obstacles = fixture_obstacles(rule);
    let consumer_variants = boundary_variants(rule.consumer.arity());
    let producer_variants = boundary_variants(rule.producer.arity());
    let assignments = seat_assignments(rule, &obstacles);
    let mut attempts = 0u32;
    let mut failures: BTreeMap<&'static str, u32> = BTreeMap::new();
    fn tally(failures: &mut BTreeMap<&'static str, u32>, reason: Fail) {
        let key = match reason {
            Fail::Route => "route",
            Fail::TooManySlots => "too-many-slots",
            Fail::Disconnected => "disconnected",
            Fail::Pack => "pack",
            Fail::Projection => "projection",
        };
        *failures.entry(key).or_insert(0) += 1;
    }
    for consumer_branches in &consumer_variants {
        for producer_branches in &producer_variants {
            for seats in &assignments {
                let Some(placed) =
                    place(rule, consumer_branches, producer_branches, seats, &obstacles)
                else {
                    *failures.entry("place").or_insert(0) += 1;
                    continue;
                };
                for order in net_orders(&placed.nets, attempts as u64 + rule_index as u64) {
                    attempts += 1;
                    if attempts > 20000 { break; }
                    match build_candidate(rule_index, &placed, &order, &obstacles) {
                        Ok(workshop) => return workshop,
                        Err(reason) => tally(&mut failures, reason),
                    }
                }
            }
        }
    }
    panic!(
        "compile64: no valid workshop for ROM rule {rule_index} ({}·{}) after {attempts} attempts; failures: {failures:?}",
        rule.consumer.name(),
        rule.producer.name(),
    );
}

/// The mirror canonical: reflect the searched workshop across the axis plane (y → −y;
/// N ↔ S, every other face unchanged). Unlike a z-flip this stays inside z ∈ {0, 1} — so
/// the side-in-plane fallback request is in bounds on the bilayer too — and it puts the
/// fallback region on the OPPOSITE side of the pair from the preferred one, which is the
/// whole point of the protocol's lift retry. Slot order, roles, fresh indices, branch
/// order, and twists are untouched.
fn mirror_canonical(workshop: &Workshop64) -> Workshop64 {
    let fd = |d: Dir| if d == Dir::N { Dir::S } else if d == Dir::S { Dir::N } else { d };
    let flip_matter = |matter: Matter| -> Matter {
        match matter {
            Matter::Empty => Matter::Empty,
            Matter::Agent { tag, principal, tail, aux_flip } => Matter::Agent {
                tag,
                principal: fd(principal),
                tail: tail.map(fd),
                aux_flip,
            },
            Matter::Link { ends, lanes, twist, hot, cooldown, pull } => Matter::Link {
                ends: FacePair::new(fd(ends.a), fd(ends.b)).expect("reflection keeps faces distinct"),
                lanes,
                twist,
                hot,
                cooldown,
                pull,
            },
            Matter::Zip { trunk, branches, twist, hot, cooldown, pull } => Matter::Zip {
                trunk: fd(trunk),
                branches: [fd(branches[0]), fd(branches[1])],
                twist,
                hot,
                cooldown,
                pull,
            },
            Matter::Cross { routes, hot, cooldown, pull } => Matter::Cross {
                routes: routes.map(|route| {
                    FacePair::new(fd(route.a), fd(route.b)).expect("reflection keeps faces distinct")
                }),
                hot,
                cooldown,
                pull,
            },
            Matter::GuestZip { tag, plane, trunk, branches, twist, deep } => Matter::GuestZip {
                tag,
                plane,
                trunk: fd(trunk),
                branches: [fd(branches[0]), fd(branches[1])],
                twist,
                deep,
            },
            Matter::GuestLink { tag, principal, plane, ends, twist } => Matter::GuestLink {
                tag,
                principal: fd(principal),
                plane,
                ends: FacePair::new(fd(ends.a), fd(ends.b))
                    .expect("reflection keeps faces distinct"),
                twist,
            },
        }
    };
    let slots = workshop
        .slots
        .iter()
        .map(|slot| WorkshopSlot64 {
            at: (slot.at.0, -slot.at.1, slot.at.2),
            matter: flip_matter(slot.matter),
            parent: slot.parent.map(fd),
            children: slot.children.iter().map(|d| fd(*d)).collect(),
            role: slot.role,
            fresh: slot.fresh,
        })
        .collect();
    Workshop64 { slots }
}

/// Compile the canonical patches for every ROM rule and both handednesses. The mirror
/// canonical is the y-reflection of the searched one, so both frames always exist together.
fn compile_all() -> BTreeMap<(u8, bool), Workshop64> {
    let mut map = BTreeMap::new();
    for (index, rule) in RULES.iter().enumerate() {
        if index == APPLY_FORK_RULE as usize { continue; } // hand-crafted workshop
        let workshop = search_workshop(index, rule);
        let mirror = mirror_canonical(&workshop);
        map.insert((index as u8, true), workshop);
        map.insert((index as u8, false), mirror);
    }
    map
}
