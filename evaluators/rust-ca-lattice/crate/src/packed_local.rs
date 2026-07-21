//! Delay-insensitive, center-write-only updates over [`CellWord`](crate::cell64::CellWord).
//!
//! `update_cell` mutates one word and receives immutable copies of its six current neighbors.
//! There is no frozen global tick: the runner may activate sites in any fair order. Persistent
//! persistent local handshakes make translation and rewrite insensitive to that order.

use crate::cell64::{
    CellWord, Control, DecodedCell, Dir, FacePair, LaneCount, LaneMask, LineRole, Matter,
    Phase, Pull, RewritePhase, RewriteRole, RuleId, U3, U6, DIRS,
};
use crate::lattice::{step, Pos};
use crate::net::Net;
use crate::rewrite64::{
    rewrite_orientation, Workshop64,
};
use crate::substrate::{Grid64, BOUNDARY};

const STANDING_SHELL: u8 = 60;
const BLOCKED_PRESSURE: u8 = 32;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct UpdateEffect {
    /// Set only when the old agent cell converts into its cable trail. Observer metadata
    /// follows this edge; the dynamics never reads the effect.
    pub observer_move: Option<Dir>,
    /// A semantic interaction becomes irrevocable exactly when the rewrite driver begins
    /// the outward placement wave. The scheduler applies this observer event; cells never
    /// inspect it.
    pub rewrite_fire: Option<RewriteFire>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RewriteFire {
    pub rule: RuleId,
    pub axis: Dir,
    pub side: u8,
    pub lift: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct MoveSpec {
    toward: Dir,
    exit: Dir,
    old_tail: Dir,
    epoch: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct RewriteSpec {
    rule: RuleId,
    axis: Dir,
    side: u8,
    lift: bool,
    fallback: bool,
}

fn site(word: CellWord) -> Option<DecodedCell> {
    (word != BOUNDARY).then(|| word.unpack().expect("neighbor word must be canonical"))
}

fn neighbor(adjacent: &[CellWord; 6], d: Dir) -> Option<DecodedCell> {
    site(adjacent[d as usize])
}

fn move_control(role: LineRole, phase: Phase, spec: MoveSpec) -> Control {
    Control::Translate {
        role,
        phase,
        toward: spec.toward,
        exit: spec.exit,
        old_tail: spec.old_tail,
        epoch: spec.epoch,
    }
}

fn as_move(control: Control, role: LineRole, phase: Phase, spec: MoveSpec) -> bool {
    control == move_control(role, phase, spec)
}

fn matter_has_channel(matter: Matter, face: Dir, lane: u8) -> bool {
    match matter {
        Matter::Empty => false,
        Matter::Agent { tag, principal, tail, .. } => {
            (face == principal && lane == 0)
                || (Some(face) == tail && lane < if tag.arity() == 3 { 2 } else { 1 })
        }
        Matter::Link { ends, lanes, .. } => {
            ends.contains(face) && lane < if lanes == LaneCount::Two { 2 } else { 1 }
        }
        Matter::Zip { trunk, branches, .. } => {
            (face == trunk && lane < 2) || (branches.contains(&face) && lane == 0)
        }
        Matter::Cross { routes, .. } => lane == 0 && routes.iter().any(|route| route.contains(face)),
        // A guest exposes the same channels projection sees: its ports plus the live
        // foreign through-channels (an arity-one rider's consumed lane exposes nothing).
        Matter::GuestZip { tag, plane, trunk, branches, twist, deep } => {
            let ridden = branches[(plane ^ twist as u8) as usize];
            let foreign = branches[(plane ^ twist as u8 ^ 1) as usize];
            (face == trunk && lane == plane ^ 1)
                || (face == foreign && lane == 0)
                || if deep {
                    (face == ridden && lane == 0)
                        || (face == trunk && lane == plane && tag.arity() == 2)
                } else {
                    (face == trunk && lane == plane)
                        || (face == ridden && lane == 0 && tag.arity() == 2)
                }
        }
        Matter::GuestLink { tag, principal, plane, ends, twist } => {
            let entry = ends.other(principal).expect("guest rides one end");
            (face == principal && (lane == plane || lane == plane ^ 1))
                || (face == entry
                    && (lane == plane ^ twist as u8 ^ 1
                        || (lane == plane ^ twist as u8 && tag.arity() == 2)))
        }
    }
}

fn hot_at(matter: Matter, face: Dir, lane: u8) -> bool {
    match matter {
        Matter::Agent { tag, principal, .. } => tag.is_consumer() && face == principal && lane == 0,
        Matter::Link { ends, lanes, twist, hot, .. } => {
            if !ends.contains(face) || lane >= if lanes == LaneCount::Two { 2 } else { 1 } {
                return false;
            }
            let base_lane = if lanes == LaneCount::Two && face == ends.b && twist { lane ^ 1 } else { lane };
            hot.get() & (1 << base_lane) != 0
        }
        Matter::Zip { trunk, branches, twist, hot, .. } => {
            let base_lane = if face == trunk && lane < 2 {
                Some(lane)
            } else if lane == 0 {
                branches.iter().position(|branch| *branch == face)
                    .map(|branch| branch as u8 ^ twist as u8)
            } else {
                None
            };
            base_lane.is_some_and(|lane| hot.get() & (1 << lane) != 0)
        }
        Matter::Empty => false,
        Matter::Cross { routes, hot, .. } => routes.iter().position(|route| route.contains(face))
            .is_some_and(|route| lane == 0 && hot.get() & (1 << route) != 0),
        // Guests are inert: they carry no heat of their own.
        Matter::GuestZip { .. } | Matter::GuestLink { .. } => false,
    }
}

fn reciprocal_hot(adjacent: &[CellWord; 6], face: Dir, lane: u8) -> bool {
    neighbor(adjacent, face).is_some_and(|n| hot_at(n.matter, face.opp(), lane))
}

fn settle_matter(matter: Matter, adjacent: &[CellWord; 6]) -> Matter {
    match matter {
        Matter::Link { ends, lanes, twist, hot, cooldown, pull } => {
            let lane_count = if lanes == LaneCount::Two { 2 } else { 1 };
            let mut next_hot = hot.get();
            for base_lane in 0..lane_count {
                let at_a = reciprocal_hot(adjacent, ends.a, base_lane);
                let lane_at_b = if lanes == LaneCount::Two && twist { base_lane ^ 1 } else { base_lane };
                let at_b = reciprocal_hot(adjacent, ends.b, lane_at_b);
                if at_a || at_b { next_hot |= 1 << base_lane; }
            }
            Matter::Link {
                ends, lanes, twist,
                hot: LaneMask::new(next_hot).unwrap(),
                cooldown: U3::new(cooldown.get().saturating_sub(1)).unwrap(),
                pull,
            }
        }
        Matter::Zip { trunk, branches, twist, hot, cooldown, pull } => {
            let mut next_hot = hot.get();
            for base_lane in 0..2 {
                let branch = base_lane ^ twist as u8;
                if reciprocal_hot(adjacent, trunk, base_lane)
                    || reciprocal_hot(adjacent, branches[branch as usize], 0)
                {
                    next_hot |= 1 << base_lane;
                }
            }
            Matter::Zip {
                trunk, branches, twist,
                hot: LaneMask::new(next_hot).unwrap(),
                cooldown: U3::new(cooldown.get().saturating_sub(1)).unwrap(),
                pull,
            }
        }
        Matter::Cross { routes, hot, cooldown, pull } => {
            let mut next_hot = hot.get();
            for (i, route) in routes.into_iter().enumerate() {
                if reciprocal_hot(adjacent, route.a, 0) || reciprocal_hot(adjacent, route.b, 0) {
                    next_hot |= 1 << i;
                }
            }
            Matter::Cross {
                routes,
                hot: LaneMask::new(next_hot).unwrap(),
                cooldown: U3::new(cooldown.get().saturating_sub(1)).unwrap(),
                pull,
            }
        }
        other => other,
    }
}

fn relaxed_field(center: u8, adjacent: &[CellWord; 6], source: u8, sigma: bool) -> u8 {
    let sum = 2u32 * center as u32 + adjacent.iter().map(|word| {
        site(*word).map_or(0, |cell| if sigma { cell.sigma } else { cell.chi }) as u32
    }).sum::<u32>();
    ((sum / 8).saturating_sub(1).min(255) as u8).max(source)
}

fn target_link(matter: Matter, entered: Dir) -> Option<(Dir, bool)> {
    let Matter::Link { ends, lanes: LaneCount::One, hot, .. } = matter else { return None };
    Some((ends.other(entered)?, hot.get() & 1 != 0))
}

/// A producer that may advance along its principal wire: a plain agent, or an agent
/// already riding a zipper/cable as a guest. `exposed` is the lane its principal wire
/// rides on its principal face (zero for a plain agent, `plane` for a guest).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Rider {
    tag: crate::rules::Tag,
    toward: Dir,
    exposed: u8,
    tail: Option<Dir>,
}

fn rider_of(matter: Matter) -> Option<Rider> {
    match matter {
        Matter::Agent { tag, principal, tail, .. } if tag.is_producer() => {
            Some(Rider { tag, toward: principal, exposed: 0, tail })
        }
        Matter::GuestZip { tag, plane, trunk, branches, twist, deep } if tag.is_producer() => {
            let ridden = branches[(plane ^ twist as u8) as usize];
            // `deep` riders entered through the trunk and exit through the branch.
            let (principal, entry) = if deep { (ridden, trunk) } else { (trunk, ridden) };
            Some(Rider {
                tag,
                toward: principal,
                exposed: plane,
                tail: (tag.arity() == 2).then_some(entry),
            })
        }
        Matter::GuestLink { tag, principal, plane, ends, .. } if tag.is_producer() => {
            Some(Rider {
                tag,
                toward: principal,
                exposed: plane,
                tail: (tag.arity() == 2)
                    .then(|| ends.other(principal).expect("guest rides one end")),
            })
        }
        _ => None,
    }
}

/// A crossing target for the guest acquire: a zipper entered through a branch or through
/// its trunk, or a two-lane trunk link. Returns the face the rider exits through (its new
/// principal face). Arity-three riders are never offered one.
fn crossing_target(matter: Matter, entered: Dir, exposed: u8) -> Option<Dir> {
    match matter {
        Matter::Zip { trunk, branches, twist, .. } => {
            if branches.contains(&entered) {
                Some(trunk)
            } else if trunk == entered {
                Some(branches[(exposed ^ twist as u8) as usize])
            } else {
                None
            }
        }
        Matter::Link { ends, lanes: LaneCount::Two, .. } => ends.other(entered),
        _ => None,
    }
}

/// The demand hot bit for a crossing offer: the target's heat at the entered face on the
/// lane the rider's wire rides there (zip branches are single-lane).
fn crossing_hot(matter: Matter, entered: Dir, exposed: u8) -> bool {
    let lane = match matter {
        Matter::Zip { trunk, branches, .. } => {
            if branches.contains(&entered) { 0 } else { debug_assert!(trunk == entered); exposed }
        }
        _ => exposed,
    };
    hot_at(matter, entered, lane)
}

fn source_offer(center: DecodedCell, adjacent: &[CellWord; 6]) -> Option<Control> {
    let rider = rider_of(center.matter)?;
    let toward = rider.toward;
    let target = neighbor(adjacent, toward)?;
    if target.control != Control::Idle { return None; }
    let (exit, hot) = match target_link(target.matter, toward.opp()) {
        Some((exit, hot)) => (exit, hot),
        None => {
            if rider.tag.arity() > 2 { return None; }
            // Crossing targets: a zip entered through a branch or through its trunk, or a
            // two-lane trunk link. An arity-one rider crosses cables and zip trunks only
            // while riding lane one: the vacated cell reverts to a lane-zero elbow on the
            // foreign lane and the live guest is the lane-index adapter, so a lane-zero
            // rider would leave the elbow facing the wrong lane (LOCAL_CA_DESIGN §7.3).
            let via_trunk = match target.matter {
                Matter::Zip { trunk, branches, .. } => {
                    if branches.contains(&toward.opp()) { false }
                    else if trunk == toward.opp() { true }
                    else { return None; }
                }
                Matter::Link { lanes: LaneCount::Two, .. } => true,
                _ => return None,
            };
            if rider.tag.arity() == 1 && via_trunk && rider.exposed != 1 { return None; }
            let exit = crossing_target(target.matter, toward.opp(), rider.exposed)?;
            (exit, crossing_hot(target.matter, toward.opp(), rider.exposed))
        }
    };
    let old_tail = rider.tail.unwrap_or(toward); // equality is the arity-one sentinel
    if let Some(tail) = rider.tail {
        let rear = neighbor(adjacent, tail)?;
        if rear.control != Control::Idle
            || !matter_has_channel(rear.matter, tail.opp(), 0)
            || (rider.tag.arity() == 3 && !matter_has_channel(rear.matter, tail.opp(), 1))
        {
            return None;
        }
    }
    let calm = center.chi == 0 && center.sigma == 0 && target.chi == 0 && target.sigma == 0;
    let downhill = center.chi >= 4 && target.chi.saturating_add(2) <= center.chi;
    if !(hot || calm || downhill) { return None; }
    Some(move_control(LineRole::Source, Phase::Offer, MoveSpec {
        toward, exit, old_tail, epoch: false,
    }))
}

fn incoming_offer(center: DecodedCell, adjacent: &[CellWord; 6]) -> Option<Control> {
    if center.control != Control::Idle { return None; }
    // Target claims outrank tail claims if an adversarial malformed patch offers both.
    for to_source in DIRS {
        let Some(source) = neighbor(adjacent, to_source) else { continue };
        let Control::Translate {
            role: LineRole::Source, phase: Phase::Offer,
            toward, exit, old_tail, epoch,
        } = source.control else { continue };
        let Some(rider) = rider_of(source.matter) else { continue };
        if toward != to_source.opp() || rider.toward != toward { continue; }
        let target_matches = target_link(center.matter, to_source)
            .is_some_and(|(actual_exit, _)| actual_exit == exit)
            || crossing_target(center.matter, to_source, rider.exposed) == Some(exit);
        if target_matches {
            return Some(move_control(LineRole::Target, Phase::Ack, MoveSpec {
                toward, exit, old_tail, epoch,
            }));
        }
    }
    for to_source in DIRS {
        let Some(source) = neighbor(adjacent, to_source) else { continue };
        let Control::Translate {
            role: LineRole::Source, phase: Phase::Offer,
            toward, exit, old_tail, epoch,
        } = source.control else { continue };
        let Some(rider) = rider_of(source.matter) else { continue };
        let Some(tail) = rider.tail else { continue };
        if old_tail != to_source.opp() || tail != old_tail { continue; }
        if matter_has_channel(center.matter, to_source, 0)
            && (rider.tag.arity() != 3 || matter_has_channel(center.matter, to_source, 1))
        {
            return Some(move_control(LineRole::Tail, Phase::Ack, MoveSpec {
                toward, exit, old_tail, epoch,
            }));
        }
    }
    None
}

fn source_next(
    center: DecodedCell,
    adjacent: &[CellWord; 6],
    phase: Phase,
    spec: MoveSpec,
) -> (Matter, Control, UpdateEffect) {
    match phase {
        Phase::Offer => {
            let Some(rider) = rider_of(center.matter) else {
                return (center.matter, move_control(LineRole::Source, Phase::Abort, spec), UpdateEffect::default());
            };
            let geometry_valid = rider.toward == spec.toward
                && rider.tail.unwrap_or(spec.toward) == spec.old_tail
                && neighbor(adjacent, spec.toward).is_some_and(|target| {
                    target_link(target.matter, spec.toward.opp())
                        .is_some_and(|(exit, _)| exit == spec.exit)
                        || crossing_target(target.matter, spec.toward.opp(), rider.exposed)
                            == Some(spec.exit)
                });
            if !geometry_valid {
                return (center.matter, move_control(LineRole::Source, Phase::Abort, spec), UpdateEffect::default());
            }
            let target_ready = neighbor(adjacent, spec.toward)
                .is_some_and(|target| as_move(target.control, LineRole::Target, Phase::Ack, spec));
            let tail_ready = rider.tag.arity() == 1 || neighbor(adjacent, spec.old_tail)
                .is_some_and(|tail| as_move(tail.control, LineRole::Tail, Phase::Ack, spec));
            if target_ready && tail_ready {
                (center.matter, move_control(LineRole::Source, Phase::Commit, spec), UpdateEffect::default())
            } else {
                (center.matter, move_control(LineRole::Source, Phase::Offer, spec), UpdateEffect::default())
            }
        }
        Phase::Commit => {
            let Some(rider) = rider_of(center.matter) else {
                return (center.matter, move_control(LineRole::Source, Phase::Abort, spec), UpdateEffect::default());
            };
            let target_committed = neighbor(adjacent, spec.toward)
                .is_some_and(|target| as_move(target.control, LineRole::Target, Phase::Commit, spec));
            let tail_committed = rider.tag.arity() == 1 || neighbor(adjacent, spec.old_tail)
                .is_some_and(|tail| as_move(tail.control, LineRole::Tail, Phase::Commit, spec));
            if !(target_committed && tail_committed) {
                return (center.matter, move_control(LineRole::Source, Phase::Commit, spec), UpdateEffect::default());
            }
            // The vacated cell: a plain agent leaves emptiness (arity one) or a trailing
            // link; a vacated guest leaves a one-lane elbow on the foreign lane (arity
            // one — the consumed principal wire vanishes cell by cell behind the rider)
            // or the original zip/cable restored (arity two — the rider's principal lane
            // becomes its aux lane and the foreign lane keeps its identity).
            let matter = match center.matter {
                Matter::Agent { tag, .. } => match tag.arity() {
                    1 => Matter::Empty,
                    2 | 3 => Matter::Link {
                        ends: FacePair::new(spec.old_tail, spec.toward).expect("agent faces are distinct"),
                        lanes: if tag.arity() == 3 { LaneCount::Two } else { LaneCount::One },
                        twist: false,
                        hot: LaneMask::new(0).unwrap(),
                        cooldown: U3::new(1).unwrap(),
                        pull: [Pull::None; 2],
                    },
                    _ => unreachable!(),
                },
                Matter::GuestZip { tag, plane, trunk, branches, twist, .. } => {
                    match tag.arity() {
                        1 => Matter::Link {
                            ends: FacePair::new(
                                branches[(plane ^ twist as u8 ^ 1) as usize],
                                trunk,
                            ).expect("zipper faces are distinct"),
                            lanes: LaneCount::One,
                            twist: false,
                            hot: LaneMask::new(0).unwrap(),
                            cooldown: U3::new(1).unwrap(),
                            pull: [Pull::None; 2],
                        },
                        _ => Matter::Zip {
                            trunk,
                            branches,
                            twist,
                            hot: LaneMask::new(0).unwrap(),
                            cooldown: U3::new(1).unwrap(),
                            pull: [Pull::None; 2],
                        },
                    }
                }
                Matter::GuestLink { tag, ends, twist, .. } => match tag.arity() {
                    1 => Matter::Link {
                        ends,
                        lanes: LaneCount::One,
                        twist: false,
                        hot: LaneMask::new(0).unwrap(),
                        cooldown: U3::new(1).unwrap(),
                        pull: [Pull::None; 2],
                    },
                    _ => Matter::Link {
                        ends,
                        lanes: LaneCount::Two,
                        twist,
                        hot: LaneMask::new(0).unwrap(),
                        cooldown: U3::new(1).unwrap(),
                        pull: [Pull::None; 2],
                    },
                },
                _ => unreachable!("rider matter is an agent or a guest"),
            };
            (matter, move_control(LineRole::Source, Phase::Done, spec), UpdateEffect {
                observer_move: Some(spec.toward),
                ..UpdateEffect::default()
            })
        }
        Phase::Done => {
            let target_done = neighbor(adjacent, spec.toward)
                .is_some_and(|target| as_move(target.control, LineRole::Target, Phase::Done, spec));
            let tail_done = spec.old_tail == spec.toward || neighbor(adjacent, spec.old_tail)
                .is_some_and(|tail| as_move(tail.control, LineRole::Tail, Phase::Done, spec));
            let control = if target_done && tail_done { Control::Idle }
                else { move_control(LineRole::Source, Phase::Done, spec) };
            (center.matter, control, UpdateEffect::default())
        }
        Phase::Abort => (center.matter, Control::Idle, UpdateEffect::default()),
        Phase::Ack => (center.matter, move_control(LineRole::Source, Phase::Abort, spec), UpdateEffect::default()),
    }
}

fn target_next(
    center: DecodedCell,
    adjacent: &[CellWord; 6],
    phase: Phase,
    spec: MoveSpec,
) -> (Matter, Control) {
    let to_source = spec.toward.opp();
    let source = neighbor(adjacent, to_source);
    match phase {
        Phase::Ack => {
            if source.is_some_and(|s| as_move(s.control, LineRole::Source, Phase::Offer, spec)) {
                return (center.matter, move_control(LineRole::Target, Phase::Ack, spec));
            }
            if let Some(source) = source.filter(|s| as_move(s.control, LineRole::Source, Phase::Commit, spec)) {
                let Some(rider) = rider_of(source.matter) else {
                    return (center.matter, move_control(LineRole::Target, Phase::Abort, spec));
                };
                let aux_flip = match source.matter {
                    Matter::Agent { aux_flip, .. } => aux_flip,
                    _ => false,
                };
                let commit = move_control(LineRole::Target, Phase::Commit, spec);
                // One-lane link: the rider steps off its ride (or the wire) as a plain
                // agent — the existing fast path.
                if target_link(center.matter, to_source).is_some_and(|(exit, _)| exit == spec.exit) {
                    return (Matter::Agent {
                        tag: rider.tag,
                        principal: spec.exit,
                        tail: (rider.tag.arity() > 1).then_some(to_source),
                        aux_flip,
                    }, commit);
                }
                // Crossing targets: the rider keeps riding — the cell becomes a guest
                // hosting the agent and the underlying crossing geometry at once.
                match center.matter {
                    Matter::Zip { trunk, branches, twist, .. } => {
                        if branches.contains(&to_source) && trunk == spec.exit {
                            let branch = branches.iter()
                                .position(|branch| *branch == to_source)
                                .expect("entered branch") as u8;
                            return (Matter::GuestZip {
                                tag: rider.tag,
                                plane: branch ^ twist as u8,
                                trunk,
                                branches,
                                twist,
                                deep: false,
                            }, commit);
                        }
                        if trunk == to_source
                            && branches[(rider.exposed ^ twist as u8) as usize] == spec.exit
                        {
                            return (Matter::GuestZip {
                                tag: rider.tag,
                                plane: rider.exposed,
                                trunk,
                                branches,
                                twist,
                                deep: true,
                            }, commit);
                        }
                        (center.matter, move_control(LineRole::Target, Phase::Abort, spec))
                    }
                    Matter::Link { ends, lanes: LaneCount::Two, twist, .. } => {
                        if ends.other(to_source) != Some(spec.exit) {
                            return (center.matter, move_control(LineRole::Target, Phase::Abort, spec));
                        }
                        (Matter::GuestLink {
                            tag: rider.tag,
                            principal: spec.exit,
                            plane: rider.exposed ^ twist as u8,
                            ends,
                            twist,
                        }, commit)
                    }
                    _ => (center.matter, move_control(LineRole::Target, Phase::Abort, spec)),
                }
            } else {
                (center.matter, move_control(LineRole::Target, Phase::Abort, spec))
            }
        }
        Phase::Commit => {
            if source.is_some_and(|s| as_move(s.control, LineRole::Source, Phase::Done, spec)) {
                (center.matter, move_control(LineRole::Target, Phase::Done, spec))
            } else {
                (center.matter, move_control(LineRole::Target, Phase::Commit, spec))
            }
        }
        Phase::Done => {
            let clear = source.is_some_and(|s| s.control == Control::Idle);
            (center.matter, if clear { Control::Idle } else { move_control(LineRole::Target, Phase::Done, spec) })
        }
        Phase::Abort => {
            let clear = source.is_none_or(|s| !matches!(s.control, Control::Translate { role: LineRole::Source, .. }));
            (center.matter, if clear { Control::Idle } else { move_control(LineRole::Target, Phase::Abort, spec) })
        }
        _ => (center.matter, move_control(LineRole::Target, Phase::Abort, spec)),
    }
}

fn tail_next(
    center: DecodedCell,
    adjacent: &[CellWord; 6],
    phase: Phase,
    spec: MoveSpec,
) -> (Matter, Control) {
    let to_source = spec.old_tail.opp();
    let source = neighbor(adjacent, to_source);
    match phase {
        Phase::Ack => {
            if source.is_some_and(|s| as_move(s.control, LineRole::Source, Phase::Offer, spec)) {
                (center.matter, move_control(LineRole::Tail, Phase::Ack, spec))
            } else if source.is_some_and(|s| as_move(s.control, LineRole::Source, Phase::Commit, spec)) {
                (center.matter, move_control(LineRole::Tail, Phase::Commit, spec))
            } else {
                (center.matter, move_control(LineRole::Tail, Phase::Abort, spec))
            }
        }
        Phase::Commit => {
            if source.is_some_and(|s| as_move(s.control, LineRole::Source, Phase::Done, spec)) {
                (center.matter, move_control(LineRole::Tail, Phase::Done, spec))
            } else {
                (center.matter, move_control(LineRole::Tail, Phase::Commit, spec))
            }
        }
        Phase::Done => {
            let clear = source.is_some_and(|s| s.control == Control::Idle);
            (center.matter, if clear { Control::Idle } else { move_control(LineRole::Tail, Phase::Done, spec) })
        }
        Phase::Abort => {
            let clear = source.is_none_or(|s| !matches!(s.control, Control::Translate { role: LineRole::Source, .. }));
            (center.matter, if clear { Control::Idle } else { move_control(LineRole::Tail, Phase::Abort, spec) })
        }
        _ => (center.matter, move_control(LineRole::Tail, Phase::Abort, spec)),
    }
}

fn rewrite_control(
    role: RewriteRole,
    phase: RewritePhase,
    slot: u8,
    spec: RewriteSpec,
) -> Control {
    Control::Rewrite {
        role,
        phase,
        rule: spec.rule,
        axis: spec.axis,
        side: spec.side,
        lift: spec.lift,
        slot: U6::new(slot).expect("workshop slot exceeds packed address"),
        fallback: spec.fallback,
    }
}

fn workshop(spec: RewriteSpec) -> Option<Workshop64> {
    let (side, lift) = rewrite_orientation(spec.axis, spec.side, spec.lift)?;
    crate::compile64::workshop_for(spec.rule.get(), spec.axis, side, lift)
}

fn rewrite_phase(
    control: Control,
    expected_role: RewriteRole,
    expected_slot: u8,
    spec: RewriteSpec,
) -> Option<RewritePhase> {
    let Control::Rewrite {
        role, phase, rule, axis, side, lift, slot, fallback,
    } = control else {
        return None;
    };
    (role == expected_role
        && slot.get() == expected_slot
        && rule == spec.rule
        && axis == spec.axis
        && side == spec.side
        && lift == spec.lift
        && fallback == spec.fallback)
        .then_some(phase)
}

/// The faces of a boundary slot's final matter that carry the patch's internal routes.
/// The trunk/outward face is excluded because placement adapts it to the dying agent's
/// actual tail face; the route faces are fixed by the workshop and must not collide with it.
fn boundary_route_faces(matter: &Matter, outward: Dir) -> Vec<Dir> {
    match matter {
        Matter::Zip { trunk, branches, .. } if *trunk == outward => branches.to_vec(),
        Matter::Link { ends, .. } if ends.contains(outward) => {
            ends.other(outward).into_iter().collect()
        }
        _ => vec![],
    }
}

/// True when a dying agent with tail face `tail` can become the boundary of `slot_matter`:
/// its tail face must not be one of the fixed route faces, and the cell holding its tail
/// cable (`tail_offset` from the driver cell) must not be a workshop slot.
fn boundary_compatible(
    workshop: &Workshop64,
    slot_matter: &Matter,
    outward: Dir,
    tail: Option<Dir>,
    tail_offset: Pos,
) -> bool {
    let Some(tail) = tail else { return true };
    if boundary_route_faces(slot_matter, outward).contains(&tail) { return false; }
    !workshop.slots.iter().any(|slot| slot.at == tail_offset)
}

fn can_accept_rewrite(slot: u8, center: DecodedCell, spec: RewriteSpec) -> bool {
    if center.control != Control::Idle { return false; }
    let Some(rule) = crate::rules::RULES.get(spec.rule.get() as usize) else { return false };
    match slot {
        // Slot zero is the driver and is never requested by a parent.
        0 => false,
        // Trying the principal axis first in the workshop BFS makes the producer slot one.
        1 => {
            let producer = rule.producer;
            let Matter::Agent { tag, principal, tail, .. } = center.matter else { return false };
            if tag != producer || principal != spec.axis.opp() { return false; }
            // Docked pose: a tail exactly when arity > 1, never pointing at the consumer.
            if tail.is_some() != (producer.arity() > 1) || tail == Some(spec.axis.opp()) {
                return false;
            }
            let Some(workshop) = workshop(spec) else { return false };
            let Some(layout) = workshop.slot(1) else { return false };
            let tail_offset = step(step((0, 0, 0), spec.axis), tail.unwrap_or(spec.axis));
            boundary_compatible(&workshop, &layout.matter, spec.axis, tail, tail_offset)
        }
        _ => center.matter == Matter::Empty,
    }
}

fn incoming_rewrite(center: DecodedCell, adjacent: &[CellWord; 6]) -> Option<Control> {
    if center.control != Control::Idle { return None; }
    for to_parent in DIRS {
        let Some(parent) = neighbor(adjacent, to_parent) else { continue };
        let Control::Rewrite {
            role,
            phase: RewritePhase::Request,
            rule,
            axis,
            side,
            lift,
            slot,
            fallback,
        } = parent.control else { continue };
        let spec = RewriteSpec { rule, axis, side, lift, fallback };
        let Some(workshop) = workshop(spec) else { continue };
        if workshop.slot(slot.get()).is_none_or(|layout| layout.role != role) { continue; }
        let Some(child_slot) = workshop.child(slot.get(), to_parent.opp()) else { continue };
        if !can_accept_rewrite(child_slot, center, spec) { continue; }
        let role = workshop.slot(child_slot).expect("request-tree child slot").role;
        return Some(rewrite_control(role, RewritePhase::Request, child_slot, spec));
    }
    None
}

fn start_rewrite(center: DecodedCell, adjacent: &[CellWord; 6]) -> Option<Control> {
    let Matter::Agent { tag: consumer, principal: axis, tail, .. } = center.matter else {
        return None;
    };
    if !consumer.is_consumer() { return None; }
    // Docked pose: a tail exactly when arity > 1, never pointing at the producer. The tail
    // need not exit opposite the principal — workshops adapt their boundary to it.
    if tail.is_some() != (consumer.arity() > 1) || tail == Some(axis) { return None; }
    let producer_cell = neighbor(adjacent, axis)?;
    if producer_cell.control != Control::Idle { return None; }
    let Matter::Agent { tag: producer, principal, tail: producer_tail, .. } =
        producer_cell.matter else { return None };
    if !producer.is_producer() || principal != axis.opp() { return None; }
    if producer_tail.is_some() != (producer.arity() > 1) || producer_tail == Some(axis.opp()) {
        return None;
    }
    let rule = RuleId::new(crate::rules::find_index(consumer, producer)? as u8)?;
    // The first side whose boundaries are compatible with both tail cables wins; later
    // lifts fall back through the usual decline path.
    for side in 0..4 {
        let spec = RewriteSpec { rule, axis, side, lift: false, fallback: false };
        let Some(workshop) = workshop(spec) else { continue };
        let consumer_ok = boundary_compatible(
            &workshop,
            &workshop.slot(0)?.matter,
            axis.opp(),
            tail,
            step((0, 0, 0), tail.unwrap_or(axis.opp())),
        );
        let producer_ok = boundary_compatible(
            &workshop,
            &workshop.slot(1)?.matter,
            axis,
            producer_tail,
            step(step((0, 0, 0), axis), producer_tail.unwrap_or(axis)),
        );
        if consumer_ok && producer_ok {
            return Some(rewrite_control(
                RewriteRole::Driver,
                RewritePhase::Request,
                0,
                spec,
            ));
        }
    }
    None
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ResponseState { Waiting, Ready, Blocked }

fn child_responses(
    workshop: &Workshop64,
    slot: u8,
    spec: RewriteSpec,
    adjacent: &[CellWord; 6],
) -> ResponseState {
    let mut state = ResponseState::Ready;
    for face in &workshop.slot(slot).expect("valid workshop slot").children {
        let child_slot = workshop.child(slot, *face).expect("request-tree child");
        let child_layout = workshop.slot(child_slot).unwrap();
        let Some(child) = neighbor(adjacent, *face) else { return ResponseState::Blocked };
        match rewrite_phase(child.control, child_layout.role, child_slot, spec) {
            Some(RewritePhase::Ready) => {}
            Some(RewritePhase::Request) => state = ResponseState::Waiting,
            Some(RewritePhase::Blocked)
            | Some(RewritePhase::Place)
            | Some(RewritePhase::Placed) => {
                return ResponseState::Blocked;
            }
            None if can_accept_rewrite(child_slot, child, spec) => {
                state = ResponseState::Waiting;
            }
            None => return ResponseState::Blocked,
        }
    }
    state
}

fn children_in_phase(
    workshop: &Workshop64,
    slot: u8,
    spec: RewriteSpec,
    adjacent: &[CellWord; 6],
    phase: RewritePhase,
) -> bool {
    workshop.slot(slot).expect("valid workshop slot").children.iter().all(|face| {
        let child_slot = workshop.child(slot, *face).unwrap();
        let child_layout = workshop.slot(child_slot).unwrap();
        neighbor(adjacent, *face).is_some_and(|child| {
            rewrite_phase(child.control, child_layout.role, child_slot, spec) == Some(phase)
        })
    })
}

fn children_clear(
    workshop: &Workshop64,
    slot: u8,
    spec: RewriteSpec,
    adjacent: &[CellWord; 6],
) -> bool {
    workshop.slot(slot).expect("valid workshop slot").children.iter().all(|face| {
        let child_slot = workshop.child(slot, *face).unwrap();
        let child_layout = workshop.slot(child_slot).unwrap();
        // An out-of-topology face holds no cell: it can never accept, so it counts as
        // clear rather than wedging the decline wave (its parent declines via
        // `child_responses`, which treats the same face as Blocked).
        neighbor(adjacent, *face).is_none_or(|child| {
            rewrite_phase(child.control, child_layout.role, child_slot, spec).is_none()
        })
    })
}

fn parent_phase(
    workshop: &Workshop64,
    slot: u8,
    spec: RewriteSpec,
    adjacent: &[CellWord; 6],
) -> Option<RewritePhase> {
    let (parent_slot, face) = workshop.parent(slot)?;
    let parent_layout = workshop.slot(parent_slot)?;
    let parent = neighbor(adjacent, face)?;
    rewrite_phase(parent.control, parent_layout.role, parent_slot, spec)
}

fn final_rewrite_matter(center: DecodedCell, slot: u8, workshop: &Workshop64, axis: Dir) -> Matter {
    let mut matter = workshop.slot(slot).expect("valid workshop slot").matter;
    if slot <= 1 {
        // The two dying agents' cells become lane-preserving boundary matter. The trunk or
        // outward end adapts to the dying agent's actual tail face (straight or bent); the
        // lane order follows its aux flip.
        let Matter::Agent { aux_flip, tail, .. } = center.matter else {
            panic!("rewrite boundary lost its docked agent before placement")
        };
        let outward = if slot == 0 { axis.opp() } else { axis };
        match &mut matter {
            Matter::Zip { trunk, twist, .. } => {
                if *trunk == outward {
                    if let Some(tail) = tail { *trunk = tail; }
                    *twist = aux_flip;
                }
            }
            Matter::Link { ends, lanes, twist, .. } if ends.contains(outward) => {
                let branch = ends.other(outward).expect("boundary link outward end");
                if let Some(tail) = tail {
                    *ends = FacePair::new(tail, branch).expect("boundary link needs distinct faces");
                }
                if *lanes == LaneCount::Two { *twist = aux_flip; }
            }
            _ => {}
        }
    }
    matter
}

fn rewrite_next(
    center: DecodedCell,
    adjacent: &[CellWord; 6],
    role: RewriteRole,
    phase: RewritePhase,
    slot: u8,
    spec: RewriteSpec,
) -> (Matter, Control, UpdateEffect) {
    let Some(workshop) = workshop(spec) else {
        return (center.matter, Control::Idle, UpdateEffect::default());
    };
    let Some(layout) = workshop.slot(slot) else {
        return (center.matter, Control::Idle, UpdateEffect::default());
    };
    if layout.role != role {
        return (center.matter, Control::Idle, UpdateEffect::default());
    }

    if role == RewriteRole::Driver {
        return match phase {
            RewritePhase::Request => match child_responses(&workshop, slot, spec, adjacent) {
                ResponseState::Waiting => (
                    center.matter,
                    rewrite_control(role, RewritePhase::Request, slot, spec),
                    UpdateEffect::default(),
                ),
                ResponseState::Blocked => (
                    center.matter,
                    rewrite_control(role, RewritePhase::Blocked, slot, spec),
                    UpdateEffect::default(),
                ),
                ResponseState::Ready => (
                    final_rewrite_matter(center, slot, &workshop, spec.axis),
                    rewrite_control(role, RewritePhase::Place, slot, spec),
                    UpdateEffect {
                        rewrite_fire: Some(RewriteFire {
                            rule: spec.rule,
                            axis: spec.axis,
                            side: spec.side,
                            lift: spec.lift,
                        }),
                        ..UpdateEffect::default()
                    },
                ),
            },
            RewritePhase::Blocked if !children_clear(&workshop, slot, spec, adjacent) => (
                center.matter,
                rewrite_control(role, RewritePhase::Blocked, slot, spec),
                UpdateEffect::default(),
            ),
            RewritePhase::Blocked if !spec.fallback => {
                let retry = RewriteSpec {
                    lift: !spec.lift,
                    fallback: true,
                    ..spec
                };
                (
                    center.matter,
                    rewrite_control(role, RewritePhase::Request, slot, retry),
                    UpdateEffect::default(),
                )
            }
            RewritePhase::Blocked => (
                center.matter,
                Control::Idle,
                UpdateEffect::default(),
            ),
            RewritePhase::Place
                if children_in_phase(
                    &workshop,
                    slot,
                    spec,
                    adjacent,
                    RewritePhase::Placed,
                ) =>
            {
                (center.matter, Control::Idle, UpdateEffect::default())
            }
            RewritePhase::Place => (
                center.matter,
                rewrite_control(role, RewritePhase::Place, slot, spec),
                UpdateEffect::default(),
            ),
            RewritePhase::Ready | RewritePhase::Placed => (
                center.matter,
                rewrite_control(role, RewritePhase::Blocked, slot, spec),
                UpdateEffect::default(),
            ),
        };
    }

    match phase {
        RewritePhase::Request => {
            let parent = parent_phase(&workshop, slot, spec, adjacent);
            if parent == Some(RewritePhase::Blocked) {
                return (
                    center.matter,
                    rewrite_control(role, RewritePhase::Blocked, slot, spec),
                    UpdateEffect::default(),
                );
            }
            if !matches!(
                parent,
                Some(RewritePhase::Request | RewritePhase::Ready)
            ) {
                return (
                    center.matter,
                    rewrite_control(role, RewritePhase::Blocked, slot, spec),
                    UpdateEffect::default(),
                );
            }
            match child_responses(&workshop, slot, spec, adjacent) {
                ResponseState::Waiting => (
                    center.matter,
                    rewrite_control(role, RewritePhase::Request, slot, spec),
                    UpdateEffect::default(),
                ),
                ResponseState::Ready => (
                    center.matter,
                    rewrite_control(role, RewritePhase::Ready, slot, spec),
                    UpdateEffect::default(),
                ),
                ResponseState::Blocked => (
                    center.matter,
                    rewrite_control(role, RewritePhase::Blocked, slot, spec),
                    UpdateEffect::default(),
                ),
            }
        }
        RewritePhase::Ready => match parent_phase(&workshop, slot, spec, adjacent) {
            Some(RewritePhase::Request | RewritePhase::Ready) => (
                center.matter,
                rewrite_control(role, RewritePhase::Ready, slot, spec),
                UpdateEffect::default(),
            ),
            Some(RewritePhase::Place) => (
                final_rewrite_matter(center, slot, &workshop, spec.axis),
                rewrite_control(role, RewritePhase::Place, slot, spec),
                UpdateEffect::default(),
            ),
            Some(RewritePhase::Blocked) | None => (
                center.matter,
                rewrite_control(role, RewritePhase::Blocked, slot, spec),
                UpdateEffect::default(),
            ),
            Some(RewritePhase::Placed) => (
                center.matter,
                rewrite_control(role, RewritePhase::Blocked, slot, spec),
                UpdateEffect::default(),
            ),
        },
        RewritePhase::Blocked => {
            let parent = parent_phase(&workshop, slot, spec, adjacent);
            if matches!(parent, Some(RewritePhase::Blocked) | None)
                && children_clear(&workshop, slot, spec, adjacent)
            {
                (center.matter, Control::Idle, UpdateEffect::default())
            } else {
                (
                    center.matter,
                    rewrite_control(role, RewritePhase::Blocked, slot, spec),
                    UpdateEffect::default(),
                )
            }
        }
        RewritePhase::Place => {
            if children_in_phase(
                &workshop,
                slot,
                spec,
                adjacent,
                RewritePhase::Placed,
            ) {
                (
                    center.matter,
                    rewrite_control(role, RewritePhase::Placed, slot, spec),
                    UpdateEffect::default(),
                )
            } else {
                (
                    center.matter,
                    rewrite_control(role, RewritePhase::Place, slot, spec),
                    UpdateEffect::default(),
                )
            }
        }
        RewritePhase::Placed => {
            let control = match parent_phase(&workshop, slot, spec, adjacent) {
                Some(RewritePhase::Place) => {
                    rewrite_control(role, RewritePhase::Placed, slot, spec)
                }
                Some(RewritePhase::Placed) | None => Control::Idle,
                _ => rewrite_control(role, RewritePhase::Placed, slot, spec),
            };
            (center.matter, control, UpdateEffect::default())
        }
    }
}

/// Mutate one center from its current six immutable face neighbors.
pub fn update_cell(center: &mut CellWord, adjacent: &[CellWord; 6]) -> UpdateEffect {
    assert_ne!(*center, BOUNDARY, "the boundary sentinel is not a mutable site");
    let current = center.unpack().expect("center word must be canonical");
    let (matter, control, effect) = match current.control {
        Control::Translate { role, phase, toward, exit, old_tail, epoch } => {
            let spec = MoveSpec { toward, exit, old_tail, epoch };
            match role {
                LineRole::Source => source_next(current, adjacent, phase, spec),
                LineRole::Target => {
                    let (matter, control) = target_next(current, adjacent, phase, spec);
                    (matter, control, UpdateEffect::default())
                }
                LineRole::Tail => {
                    let (matter, control) = tail_next(current, adjacent, phase, spec);
                    (matter, control, UpdateEffect::default())
                }
            }
        }
        Control::Rewrite { role, phase, rule, axis, side, lift, slot, fallback } => {
            rewrite_next(
                current,
                adjacent,
                role,
                phase,
                slot.get(),
                RewriteSpec { rule, axis, side, lift, fallback },
            )
        }
        Control::Idle => {
            let control = incoming_rewrite(current, adjacent)
                .or_else(|| start_rewrite(current, adjacent))
                .or_else(|| incoming_offer(current, adjacent))
                .or_else(|| source_offer(current, adjacent))
                .unwrap_or(Control::Idle);
            let matter = if control == Control::Idle { settle_matter(current.matter, adjacent) }
                else { current.matter };
            (matter, control, UpdateEffect::default())
        }
        // Cable and pressure roles are represented now and receive their local transition
        // tables next. Until then they persist rather than being discarded by an unrelated
        // activation.
        other => (current.matter, other, UpdateEffect::default()),
    };

    let sigma_source = matches!(matter, Matter::Agent { tag, .. } if tag.is_consumer())
        .then_some(STANDING_SHELL).unwrap_or(0);
    let blocked_source = matches!(matter, Matter::Agent { tag, principal, .. } if tag.is_producer()
        && neighbor(adjacent, principal).is_some_and(|n| !matches!(n.matter, Matter::Link { lanes: LaneCount::One, .. } | Matter::Agent { .. })))
        .then_some(BLOCKED_PRESSURE).unwrap_or(0);
    let rewrite_refusal_source = matches!(
        current.control,
        Control::Rewrite { phase: RewritePhase::Request, .. }
    ) && matches!(
        control,
        Control::Rewrite { phase: RewritePhase::Blocked, .. }
    );
    let next = DecodedCell {
        matter,
        control,
        chi: relaxed_field(
            current.chi,
            adjacent,
            blocked_source.max(if rewrite_refusal_source { BLOCKED_PRESSURE } else { 0 }),
            false,
        ),
        sigma: relaxed_field(current.sigma, adjacent, sigma_source, true),
    };
    *center = CellWord::pack(next).expect("local rule emitted an invalid cell");
    effect
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SweepResult {
    pub activations: usize,
    pub changes: usize,
    pub moves: Vec<(Pos, Pos)>,
    pub rewrites: Vec<Pos>,
}

fn splitmix64(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

pub fn activate(grid: &mut Grid64, p: Pos) -> Option<(CellWord, CellWord, UpdateEffect)> {
    if !grid.topo.in_bounds(p) { return None; }
    let before = grid.word(p);
    let adjacent = grid.neighborhood(p);
    let mut after = before;
    let effect = update_cell(&mut after, &adjacent);
    if after == before { return None; }
    grid.set_word(p, after);
    if let Some(toward) = effect.observer_move {
        let target = step(p, toward);
        let sid = grid.observer_sid.remove(&p).expect("moving source has no observer sid");
        assert!(grid.observer_sid.insert(target, sid).is_none(), "moving target already has a sid");
        grid.transport += 1;
    }
    Some((before, after, effect))
}

fn add(origin: Pos, relative: Pos) -> Pos {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

fn observe_rewrite(grid: &mut Grid64, shadow: &mut Net, driver: Pos, event: RewriteFire) {
    let consumer_sid = grid.observer_sid.remove(&driver)
        .expect("rewrite driver has no observer sid");
    let producer_pos = step(driver, event.axis);
    let producer_sid = grid.observer_sid.remove(&producer_pos)
        .expect("rewrite producer has no observer sid");
    let (rule, fresh_sids) = shadow.fire(consumer_sid, producer_sid);
    assert!(std::ptr::eq(rule, &crate::rules::RULES[event.rule.get() as usize]));

    let (side, lift) = rewrite_orientation(event.axis, event.side, event.lift)
        .expect("placed rewrite has invalid orientation");
    let workshop = crate::compile64::workshop_for(event.rule.get(), event.axis, side, lift)
        .expect("placed rewrite has invalid workshop");
    assert_eq!(fresh_sids.len(), rule.fresh.len());
    for slot in &workshop.slots {
        let Some(fresh) = slot.fresh else { continue };
        let sid = fresh_sids[fresh as usize];
        let seat = add(driver, slot.at);
        assert!(grid.observer_sid.insert(seat, sid).is_none(), "rewrite seat already has a sid");
    }
    grid.rewrites += 1;
}

/// Activate one site and immediately apply any semantic observer event to the shadow net.
pub fn activate_with_shadow(
    grid: &mut Grid64,
    shadow: &mut Net,
    p: Pos,
) -> Option<(CellWord, CellWord, UpdateEffect)> {
    let result = activate(grid, p)?;
    if let Some(event) = result.2.rewrite_fire {
        observe_rewrite(grid, shadow, p, event);
    }
    Some(result)
}

/// The scheduler's deterministic random order for one fair round. Exposing the order lets
/// trace tools record every individual activation without duplicating scheduler logic.
pub fn activation_order(grid: &Grid64, seed: u64, round: u64) -> Vec<Pos> {
    let mut sites = grid.positions_with_halo();
    let mut random = seed ^ round.wrapping_mul(0xd1b5_4a32_d192_ed03)
        ^ (sites.len() as u64).wrapping_mul(0xa24b_aed4_963e_e407);
    for i in (1..sites.len()).rev() {
        let j = (splitmix64(&mut random) % (i as u64 + 1)) as usize;
        sites.swap(i, j);
    }
    sites
}

/// One fair live-read round: every currently active site and its one-cell halo is activated
/// once in a deterministic pseudo-random order. Later activations observe earlier writes.
pub fn sweep_permuted(
    grid: &mut Grid64,
    shadow: &mut Net,
    seed: u64,
    round: u64,
) -> SweepResult {
    let sites = activation_order(grid, seed, round);
    let mut result = SweepResult { activations: sites.len(), ..SweepResult::default() };
    for p in sites {
        if let Some((_, _, effect)) = activate_with_shadow(grid, shadow, p) {
            result.changes += 1;
            if let Some(toward) = effect.observer_move { result.moves.push((p, step(p, toward))); }
            if effect.rewrite_fire.is_some() { result.rewrites.push(p); }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cell64::Matter;
    use crate::net::Net;
    use crate::oracle::{ap, f2, Term};
    use crate::substrate::embed64;
    use crate::lattice::Topo;

    fn word(matter: Matter) -> CellWord {
        CellWord::pack(DecodedCell { matter, ..DecodedCell::default() }).unwrap()
    }

    #[test]
    fn arity_three_translation_leaves_one_double_cable_and_no_square() {
        let source_pos = (0, 0, 0);
        let target_pos = (1, 0, 0);
        let tail_pos = (-1, 0, 0);
        let mut grid = Grid64::new(Topo::Full3D);
        grid.set_word(source_pos, word(Matter::Agent {
            tag: crate::rules::Tag::F,
            principal: Dir::E,
            tail: Some(Dir::W),
            aux_flip: true,
        }));
        grid.observer_sid.insert(source_pos, 7);
        grid.set_word(target_pos, word(Matter::Link {
            ends: FacePair::new(Dir::E, Dir::W).unwrap(), lanes: LaneCount::One,
            twist: false, hot: LaneMask::new(1).unwrap(), cooldown: U3::new(0).unwrap(),
            pull: [Pull::None; 2],
        }));
        grid.set_word(tail_pos, word(Matter::Zip {
            trunk: Dir::E, branches: [Dir::N, Dir::S], twist: false,
            hot: LaneMask::new(0).unwrap(), cooldown: U3::new(0).unwrap(),
            pull: [Pull::None; 2],
        }));

        for p in [source_pos, target_pos, tail_pos, source_pos, target_pos, tail_pos,
                  source_pos, target_pos, tail_pos, source_pos, target_pos, tail_pos] {
            activate(&mut grid, p);
        }

        assert_eq!(grid.transport, 1);
        assert_eq!(grid.observer_sid.get(&target_pos), Some(&7));
        assert!(!grid.observer_sid.contains_key(&source_pos));
        assert!(matches!(grid.matter(source_pos), Some(Matter::Link {
            lanes: LaneCount::Two, ends, ..
        }) if ends == FacePair::new(Dir::E, Dir::W).unwrap()));
        assert!(matches!(grid.matter(target_pos), Some(Matter::Agent {
            tag: crate::rules::Tag::F,
            principal: Dir::E,
            tail: Some(Dir::W),
            aux_flip: true,
        })));
        // No diagonal participant was created.
        assert_eq!(grid.matter((0, 1, 0)), Some(Matter::Empty));
        assert_eq!(grid.matter((1, 1, 0)), Some(Matter::Empty));
    }

    #[test]
    fn live_read_permutations_preserve_projection_after_settling() {
        for seed in [0, 1, 0x9e37_79b9_7f4a_7c15, u64::MAX] {
            let term = ap(f2(Term::L, Term::L), Term::L);
            let mut net = Net::new();
            let root = net.build(&term);
            net.drive(root);
            let mut grid = embed64(&net, Topo::Full3D);
            let mut last_transport = 0;
            let mut idle_rounds = 0;
            for round in 0..2_000 {
                sweep_permuted(&mut grid, &mut net, seed, round);
                if !grid.has_protocol() {
                    grid.check_projection(&net);
                    if grid.transport == last_transport { idle_rounds += 1; } else { idle_rounds = 0; }
                    last_transport = grid.transport;
                    if idle_rounds >= 2 { break; }
                }
            }
            assert!(grid.transport > 0, "seed {seed:#x} never completed a local translation");
            assert!(!grid.has_protocol(), "seed {seed:#x} left a stuck translation role");
            grid.check_projection(&net);
        }
    }
}
