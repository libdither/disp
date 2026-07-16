//! Face-relative rewrite workshops for the packed cable substrate.
//!
//! The first compiled workshop is A·F. It is deliberately expressed as ordinary final
//! matter rather than a host route search: the same sixteen relative roles are requested
//! and placed by the local rewrite protocol.

use crate::cell64::{
    DecodedCell, Dir, FacePair, LaneCount, LaneMask, Matter, Pull, RewriteRole, U3,
    DIRS,
};
use crate::lattice::{dir_to, step, Pos, Topo};
use crate::net::Net;
use crate::rules::Tag;
use crate::substrate::Grid64;
use std::collections::BTreeMap;
use std::collections::VecDeque;

pub const APPLY_FORK_RULE: u8 = 2;

#[derive(Clone, Debug)]
pub struct WorkshopSlot64 {
    /// Offset from the consumer/driver cell.
    pub at: Pos,
    pub matter: Matter,
    /// Face pointing from this slot toward its request-tree parent.
    pub parent: Option<Dir>,
    /// Faces pointing from this slot toward its request-tree children.
    pub children: Vec<Dir>,
    pub role: RewriteRole,
    pub fresh: Option<u8>,
}

#[derive(Clone, Debug)]
pub struct Workshop64 {
    pub slots: Vec<WorkshopSlot64>,
}

impl Workshop64 {
    pub fn slot(&self, slot: u8) -> Option<&WorkshopSlot64> {
        self.slots.get(slot as usize)
    }

    pub fn child(&self, slot: u8, face: Dir) -> Option<u8> {
        let parent = self.slot(slot)?;
        if !parent.children.contains(&face) { return None; }
        self.slots.iter().position(|candidate| {
            candidate.at == step(parent.at, face) && candidate.parent == Some(face.opp())
        }).map(|index| index as u8)
    }

    pub fn parent(&self, slot: u8) -> Option<(u8, Dir)> {
        let child = self.slot(slot)?;
        let face = child.parent?;
        let parent_at = step(child.at, face);
        self.slots.iter().position(|candidate| candidate.at == parent_at)
            .map(|index| (index as u8, face))
    }
}

#[derive(Clone, Debug)]
pub struct RewritePatch64 {
    pub consumer: Pos,
    pub producer: Pos,
    pub fresh: Vec<Pos>,
    pub matter: BTreeMap<Pos, Matter>,
    pub axis: Dir,
    pub side: Dir,
    pub lift: Dir,
}

fn offset(mut p: Pos, d: Dir, n: usize) -> Pos {
    for _ in 0..n { p = step(p, d); }
    p
}

fn lift_pair(axis: Dir, side: Dir) -> Option<[Dir; 2]> {
    if side == axis || side == axis.opp() { return None; }
    let remaining: Vec<Dir> = DIRS.into_iter().filter(|d| {
        *d != axis && *d != axis.opp() && *d != side && *d != side.opp()
    }).collect();
    (remaining.len() == 2).then(|| [remaining[0], remaining[1]])
}

pub fn rewrite_orientation(axis: Dir, side: u8, lift: bool) -> Option<(Dir, Dir)> {
    let side_dir = *axis.perp().get(side as usize)?;
    let lifts = lift_pair(axis, side_dir)?;
    Some((side_dir, lifts[lift as usize]))
}

pub fn rewrite_orientation_code(axis: Dir, side: Dir, lift: Dir) -> Option<(u8, bool)> {
    let side_index = axis.perp().iter().position(|candidate| *candidate == side)? as u8;
    let lifts = lift_pair(axis, side)?;
    let lift_index = lifts.iter().position(|candidate| *candidate == lift)?;
    Some((side_index, lift_index != 0))
}

fn cold_link(a: Dir, b: Dir) -> Matter {
    Matter::Link {
        ends: FacePair::new(a, b).expect("rewrite link needs distinct faces"),
        lanes: LaneCount::One,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    }
}

fn add(origin: Pos, relative: Pos) -> Pos {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

/// The face-relative A·F ROM entry. It is independent of lattice coordinates and is used
/// both by the static geometry checker and by every local participant in the live protocol.
pub fn apply_fork_workshop(
    axis: Dir,
    side: Dir,
    lift: Dir,
    consumer_flip: bool,
    producer_flip: bool,
) -> Option<Workshop64> {
    if !axis.perp().contains(&side) || !lift_pair(axis, side)?.contains(&lift) {
        return None;
    }
    let consumer = (0, 0, 0);
    let producer = step(consumer, axis);
    let down = side.opp();
    let t1 = offset(producer, side, 2);
    let t1_zip = step(t1, axis.opp());
    let pair = offset(consumer, down, 2);
    let pair_zip = step(pair, side);

    let mut matter = BTreeMap::new();
    matter.insert(consumer, Matter::Zip {
        trunk: axis.opp(), branches: [down, side], twist: consumer_flip,
        hot: LaneMask::new(0).unwrap(), cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    matter.insert(producer, Matter::Zip {
        trunk: axis, branches: [side, down], twist: producer_flip,
        hot: LaneMask::new(0).unwrap(), cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    matter.insert(t1, Matter::Agent {
        tag: Tag::T1, principal: down, tail: Some(axis.opp()), aux_flip: false,
    });
    matter.insert(t1_zip, Matter::Zip {
        trunk: axis, branches: [axis.opp(), down], twist: false,
        hot: LaneMask::new(0).unwrap(), cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    matter.insert(pair, Matter::Agent {
        tag: Tag::Pair, principal: axis.opp(), tail: Some(side), aux_flip: false,
    });
    matter.insert(pair_zip, Matter::Zip {
        trunk: down, branches: [axis, side], twist: false,
        hot: LaneMask::new(0).unwrap(), cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });

    // Exterior connections. C.1—Pair.2 is the direct edge from the consumer zipper
    // to the Pair zipper, so it needs no intermediate link cell.
    matter.insert(step(consumer, side), cold_link(side, down));
    matter.insert(step(producer, side), cold_link(side, down));
    matter.insert(step(producer, down), cold_link(side, axis.opp()));

    // T1.args—Pair.principal rises one layer, passes the old consumer cable, and returns.
    let internal = [
        step(t1_zip, axis.opp()),
        step(step(t1_zip, axis.opp()), down),
        step(step(step(t1_zip, axis.opp()), down), lift),
        step(step(step(step(t1_zip, axis.opp()), down), lift), down),
        step(step(step(step(step(t1_zip, axis.opp()), down), lift), down), down),
        step(
            step(step(step(step(step(t1_zip, axis.opp()), down), lift), down), down),
            lift.opp(),
        ),
        step(pair, axis.opp()),
    ];
    let mut full_path = vec![t1_zip];
    full_path.extend(internal);
    full_path.push(pair);
    for i in 1..full_path.len() - 1 {
        let p = full_path[i];
        let link = cold_link(dir_to(p, full_path[i - 1])?, dir_to(p, full_path[i + 1])?);
        if matter.insert(p, link).is_some() { return None; }
    }
    if matter.len() != 16 { return None; }

    // Give the connected workshop a deterministic rooted request tree. Slot zero is always
    // the driver. Trying `axis` first makes the docked producer slot one in every rotation.
    let mut order = vec![consumer];
    let mut parents = BTreeMap::new();
    let mut queue = VecDeque::from([consumer]);
    while let Some(parent) = queue.pop_front() {
        for face in std::iter::once(axis).chain(DIRS) {
            let child = step(parent, face);
            if !matter.contains_key(&child) || order.contains(&child) { continue; }
            order.push(child);
            parents.insert(child, face.opp());
            queue.push_back(child);
        }
    }
    if order.len() != matter.len() { return None; }

    let slots = order.iter().enumerate().map(|(index, at)| {
        let final_matter = matter[at];
        let children = order.iter().filter_map(|candidate| {
            (parents.get(candidate).is_some_and(|parent_face| {
                step(*candidate, *parent_face) == *at
            })).then(|| dir_to(*at, *candidate)).flatten()
        }).collect();
        let fresh = if *at == t1 { Some(0) } else if *at == pair { Some(1) } else { None };
        let role = if index == 0 {
            RewriteRole::Driver
        } else if *at == producer || matches!(final_matter, Matter::Zip { .. }) {
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
    }).collect();
    Some(Workshop64 { slots })
}

/// Compile the sixteen-cell A·F butterfly around an already docked straight pair.
///
/// Preconditions are intentionally geometric and local:
///
/// - A and F occupy adjacent principal cells;
/// - both cable tails point axially outward; and
/// - one perpendicular butterfly plus its one-layer internal overpass is empty.
pub fn plan_apply_fork(grid: &Grid64, consumer: Pos) -> Option<RewritePatch64> {
    let Matter::Agent {
        tag: Tag::A,
        principal: axis,
        tail: Some(consumer_tail),
        aux_flip: consumer_flip,
    } = grid.matter(consumer)? else { return None };
    let producer = step(consumer, axis);
    let Matter::Agent {
        tag: Tag::F,
        principal,
        tail: Some(producer_tail),
        aux_flip: producer_flip,
    } = grid.matter(producer)? else { return None };
    if principal != axis.opp() || consumer_tail != axis.opp() || producer_tail != axis {
        return None;
    }
    if grid.control(consumer)? != crate::cell64::Control::Idle
        || grid.control(producer)? != crate::cell64::Control::Idle
    {
        return None;
    }

    for side in axis.perp() {
        for lift in lift_pair(axis, side)? {
            let workshop = apply_fork_workshop(
                axis, side, lift, consumer_flip, producer_flip,
            )?;
            let matter = workshop.slots.iter().map(|slot| {
                (add(consumer, slot.at), slot.matter)
            }).collect::<BTreeMap<_, _>>();
            let open = matter.keys().all(|at| {
                *at == consumer || *at == producer || grid.is_empty(*at)
            });
            if !open { continue; }
            let fresh = workshop.slots.iter().filter_map(|slot| {
                slot.fresh.map(|index| (index, add(consumer, slot.at)))
            }).collect::<BTreeMap<_, _>>().into_values().collect();
            return Some(RewritePatch64 {
                consumer, producer, fresh, matter, axis, side, lift,
            });
        }
    }
    None
}

/// Atomic materialization is used only as a compiler/projection test for the workshop.
/// The evaluator path uses the per-cell rewrite roles instead.
pub fn apply_compiled_patch(
    grid: &mut Grid64,
    shadow: &mut Net,
    patch: &RewritePatch64,
) -> Vec<u32> {
    let consumer_sid = grid.observer_sid.remove(&patch.consumer).expect("consumer sid");
    let producer_sid = grid.observer_sid.remove(&patch.producer).expect("producer sid");
    let (_, fresh_sids) = shadow.fire(consumer_sid, producer_sid);
    assert_eq!(fresh_sids.len(), patch.fresh.len());
    for (&p, &matter) in &patch.matter {
        let old = grid.decoded(p).unwrap_or_default();
        grid.set(p, DecodedCell { matter, control: crate::cell64::Control::Idle, ..old });
    }
    for (&p, &sid) in patch.fresh.iter().zip(&fresh_sids) {
        assert!(grid.observer_sid.insert(p, sid).is_none());
    }
    fresh_sids
}

fn put_demo(grid: &mut Grid64, p: Pos, matter: Matter) {
    grid.set(p, DecodedCell { matter, ..DecodedCell::default() });
}

fn attach_demo_out(
    shadow: &mut Net,
    grid: &mut Grid64,
    owner: u32,
    port: usize,
    zip: Pos,
    face: Dir,
    distance: usize,
) {
    let out = shadow.mk(Tag::Out);
    shadow.link(owner, port as u8, out, 0);
    let mut path = vec![zip];
    for n in 1..distance { path.push(offset(zip, face, n)); }
    let out_pos = offset(zip, face, distance);
    path.push(out_pos);
    for i in 1..path.len() - 1 {
        put_demo(
            grid,
            path[i],
            cold_link(
                dir_to(path[i], path[i - 1]).unwrap(),
                dir_to(path[i], path[i + 1]).unwrap(),
            ),
        );
    }
    put_demo(grid, out_pos, Matter::Agent {
        tag: Tag::Out,
        principal: face.opp(),
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert(out_pos, out);
}

/// Closed A·F fixture used by the activation trace and locality tests. Every auxiliary
/// cable ends at a private inert pad, so projection can be checked at either protocol end.
pub fn apply_fork_fixture64(topo: Topo) -> (Grid64, Net, Pos) {
    let mut shadow = Net::new();
    let consumer = shadow.mk(Tag::A);
    let producer = shadow.mk(Tag::F);
    shadow.link(consumer, 0, producer, 0);
    let c = (0, 0, 0);
    let p = (1, 0, 0);
    let mut grid = Grid64::new(topo);
    put_demo(&mut grid, c, Matter::Agent {
        tag: Tag::A,
        principal: Dir::E,
        tail: Some(Dir::W),
        aux_flip: false,
    });
    put_demo(&mut grid, p, Matter::Agent {
        tag: Tag::F,
        principal: Dir::W,
        tail: Some(Dir::E),
        aux_flip: false,
    });
    grid.observer_sid.insert(c, consumer);
    grid.observer_sid.insert(p, producer);

    for (at, ends) in [
        ((-1, 0, 0), FacePair::new(Dir::E, Dir::W).unwrap()),
        ((2, 0, 0), FacePair::new(Dir::E, Dir::W).unwrap()),
    ] {
        put_demo(&mut grid, at, Matter::Link {
            ends,
            lanes: LaneCount::Two,
            twist: false,
            hot: LaneMask::new(0).unwrap(),
            cooldown: U3::new(0).unwrap(),
            pull: [Pull::None; 2],
        });
    }
    let consumer_zip = (-2, 0, 0);
    let producer_zip = (3, 0, 0);
    put_demo(&mut grid, consumer_zip, Matter::Zip {
        trunk: Dir::E,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    put_demo(&mut grid, producer_zip, Matter::Zip {
        trunk: Dir::W,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, consumer, 1, consumer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, consumer, 2, consumer_zip, Dir::S, 2);
    attach_demo_out(&mut shadow, &mut grid, producer, 1, producer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, producer, 2, producer_zip, Dir::S, 2);
    grid.check_projection(&shadow);
    (grid, shadow, c)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell64::{CellWord, Control};
    use crate::lattice::Topo;
    use std::collections::BTreeSet;

    fn positions(origin: Pos, workshop: &Workshop64) -> BTreeSet<Pos> {
        workshop.slots.iter().map(|slot| add(origin, slot.at)).collect()
    }

    fn occupied_matter(grid: &Grid64) -> BTreeMap<Pos, Matter> {
        grid.cells.iter().filter_map(|(&at, word)| {
            let matter = matter_shape(word.unpack().unwrap().matter);
            (matter != Matter::Empty).then_some((at, matter))
        }).collect()
    }

    fn matter_shape(matter: Matter) -> Matter {
        match matter {
            Matter::Link { ends, lanes, twist, .. } => Matter::Link {
                ends,
                lanes,
                twist,
                hot: LaneMask::new(0).unwrap(),
                cooldown: U3::new(0).unwrap(),
                pull: [Pull::None; 2],
            },
            Matter::Zip { trunk, branches, twist, .. } => Matter::Zip {
                trunk,
                branches,
                twist,
                hot: LaneMask::new(0).unwrap(),
                cooldown: U3::new(0).unwrap(),
                pull: [Pull::None; 2],
            },
            Matter::Cross { routes, .. } => Matter::Cross {
                routes,
                hot: LaneMask::new(0).unwrap(),
                cooldown: U3::new(0).unwrap(),
                pull: [Pull::None; 2],
            },
            other => other,
        }
    }

    fn run_local(
        grid: &mut Grid64,
        shadow: &mut Net,
        seed: u64,
    ) -> u8 {
        let mut peak_chi = 0;
        for round in 0..2_000 {
            crate::packed_local::sweep_permuted(grid, shadow, seed, round);
            peak_chi = peak_chi.max(
                grid.cells.values().map(|word| word.unpack().unwrap().chi).max().unwrap_or(0),
            );
            if !grid.has_protocol() { return peak_chi; }
        }
        panic!("local request did not settle");
    }

    fn workshop_is_placed(
        grid: &Grid64,
        origin: Pos,
        workshop: &Workshop64,
    ) -> bool {
        workshop.slots.iter().all(|slot| {
            grid.matter(add(origin, slot.at)).map(matter_shape)
                == Some(matter_shape(slot.matter))
        })
    }

    fn run_until_placed(
        grid: &mut Grid64,
        shadow: &mut Net,
        origin: Pos,
        workshop: &Workshop64,
        seed: u64,
    ) {
        for round in 0..2_000 {
            for at in crate::packed_local::activation_order(grid, seed, round) {
                crate::packed_local::activate_with_shadow(grid, shadow, at);
                if grid.rewrites == 1 && workshop_is_placed(grid, origin, workshop) {
                    return;
                }
            }
        }
        panic!("workshop never finished its placement wave");
    }

    #[test]
    fn apply_fork_compiles_to_sixteen_cells_and_projects() {
        let (mut grid, mut shadow, c) = apply_fork_fixture64(Topo::Bilayer);
        let patch = plan_apply_fork(&grid, c).expect("compact A·F workshop");
        assert_eq!(patch.matter.len(), 16);
        assert_eq!(patch.fresh.len(), 2);
        assert!(patch.matter.values().all(|matter| !matches!(matter, Matter::Cross { .. })));
        apply_compiled_patch(&mut grid, &mut shadow, &patch);
        grid.check_projection(&shadow);
        assert_eq!(shadow.ints, 1);
    }

    #[test]
    fn blocked_butterfly_is_not_planned_or_overwritten() {
        let (mut grid, _, c) = apply_fork_fixture64(Topo::Bilayer);
        let open_patch = plan_apply_fork(&grid, c).unwrap();
        let blocked = *open_patch.matter.keys().find(|p| **p != c && **p != open_patch.producer).unwrap();
        let marker = Matter::Agent { tag: Tag::Out, principal: Dir::U, tail: None, aux_flip: false };
        put_demo(&mut grid, blocked, marker);
        assert!(plan_apply_fork(&grid, c).is_none());
        assert_eq!(grid.matter(blocked), Some(marker));
    }

    #[test]
    fn all_compiled_words_still_fit_the_single_u64() {
        let (grid, _, c) = apply_fork_fixture64(Topo::Bilayer);
        let patch = plan_apply_fork(&grid, c).unwrap();
        for matter in patch.matter.values().copied() {
            assert!(CellWord::pack(DecodedCell { matter, control: Control::Idle, chi: 255, sigma: 255 }).is_ok());
        }
    }

    #[test]
    fn apply_fork_places_exactly_sixteen_cells_by_local_random_order_activations() {
        for seed in [0, 1, 0x9e37_79b9_7f4a_7c15, u64::MAX] {
            let (mut grid, mut shadow, c) = apply_fork_fixture64(Topo::Bilayer);
            let workshop = apply_fork_workshop(
                Dir::E,
                Dir::N,
                Dir::U,
                false,
                false,
            ).unwrap();
            run_until_placed(&mut grid, &mut shadow, c, &workshop, seed);
            for slot in &workshop.slots {
                assert_eq!(
                    grid.matter(add(c, slot.at)).map(matter_shape),
                    Some(matter_shape(slot.matter)),
                    "seed {seed:#x} placed the wrong matter at slot {:?}",
                    slot.at,
                );
            }
            assert_eq!(workshop.slots.len(), 16);
            assert_eq!(grid.rewrites, 1, "seed {seed:#x} did not place exactly once");
            run_local(&mut grid, &mut shadow, seed ^ 0xd1b5_4a32_d192_ed03);
            assert!(!grid.has_protocol(), "seed {seed:#x} left rewrite roles behind");
            grid.check_projection(&shadow);
            assert_eq!(shadow.ints, 1);
        }
    }

    #[test]
    fn blocked_preferred_lift_retries_the_opposite_direction() {
        for seed in [0, 1, 0x9e37_79b9_7f4a_7c15, u64::MAX] {
            let (mut grid, mut shadow, c) = apply_fork_fixture64(Topo::Full3D);
            let preferred = apply_fork_workshop(
                Dir::E,
                Dir::N,
                Dir::U,
                false,
                false,
            ).unwrap();
            let fallback = apply_fork_workshop(
                Dir::E,
                Dir::N,
                Dir::D,
                false,
                false,
            ).unwrap();
            let preferred_positions = positions(c, &preferred);
            let fallback_positions = positions(c, &fallback);
            let blocked = *preferred_positions.difference(&fallback_positions)
                .find(|at| grid.is_empty(**at))
                .expect("preferred lift needs a private probe cell");
            let marker = Matter::Agent {
                tag: Tag::Out,
                principal: Dir::U,
                tail: None,
                aux_flip: false,
            };
            put_demo(&mut grid, blocked, marker);

            run_until_placed(&mut grid, &mut shadow, c, &fallback, seed);

            assert_eq!(grid.rewrites, 1, "seed {seed:#x} did not use its fallback");
            assert_eq!(shadow.ints, 1);
            assert_eq!(grid.matter(blocked), Some(marker));
            for slot in &fallback.slots {
                assert_eq!(
                    grid.matter(add(c, slot.at)).map(matter_shape),
                    Some(matter_shape(slot.matter)),
                    "seed {seed:#x} did not place the down-lift workshop at {:?}",
                    slot.at,
                );
            }
            run_local(&mut grid, &mut shadow, seed ^ 0xd1b5_4a32_d192_ed03);
            assert!(!grid.has_protocol());
        }
    }

    #[test]
    fn both_lifts_blocked_clear_the_request_without_changing_matter() {
        let (mut grid, mut shadow, c) = apply_fork_fixture64(Topo::Full3D);
        let preferred = apply_fork_workshop(
            Dir::E,
            Dir::N,
            Dir::U,
            false,
            false,
        ).unwrap();
        let fallback = apply_fork_workshop(
            Dir::E,
            Dir::N,
            Dir::D,
            false,
            false,
        ).unwrap();
        let preferred_positions = positions(c, &preferred);
        let fallback_positions = positions(c, &fallback);
        let block_preferred = *preferred_positions.difference(&fallback_positions)
            .find(|at| grid.is_empty(**at))
            .expect("preferred lift needs a private probe cell");
        let block_fallback = *fallback_positions.difference(&preferred_positions)
            .find(|at| grid.is_empty(**at))
            .expect("fallback lift needs a private probe cell");
        let marker = Matter::Agent {
            tag: Tag::Out,
            principal: Dir::U,
            tail: None,
            aux_flip: false,
        };
        put_demo(&mut grid, block_preferred, marker);
        put_demo(&mut grid, block_fallback, marker);
        let before = occupied_matter(&grid);

        let peak_chi = run_local(&mut grid, &mut shadow, 0xfeed_face);

        assert_eq!(occupied_matter(&grid), before);
        assert_eq!(grid.rewrites, 0);
        assert_eq!(shadow.ints, 0);
        assert!(!grid.has_protocol());
        assert!(peak_chi > 0, "declined requests did not emit local obstruction pressure");
        assert!(matches!(grid.matter(c), Some(Matter::Agent { tag: Tag::A, .. })));
        assert!(matches!(grid.matter(step(c, Dir::E)), Some(Matter::Agent { tag: Tag::F, .. })));
    }
}
