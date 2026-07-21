//! The Cell64 suite: minimal scenarios the packed substrate must always complete (the `Must`
//! tier) plus example reductions (`Example`). Shared by the integration gate
//! (`tests/cell64_suite.rs`) and the trace generator (`src/bin/dump-suite.rs`), whose bundle
//! is replayed by `research/interaction-combinator/lattice_player.html`.
//!
//! Every scenario runs the real local rule under fair deterministic random live-read
//! activation. Translation is projection identity; only the A·F workshop performs a semantic
//! interaction today, so reduction examples are built around it.

use crate::cell64::{
    CellWord, DecodedCell, Dir, FacePair, LaneCount, LaneMask, Matter, Pull, U3, DIRS,
};
use crate::lattice::{step, Pos, Topo};
use crate::net::Net;
use crate::oracle::{ap, f2, Term};
use crate::packed_local::{activation_order, activate_with_shadow, RewriteFire};
use crate::rewrite64::{
    apply_fork_fixture64, apply_fork_workshop, attach_demo_out, cold_link, put_demo,
};
use crate::rules::Tag;
use crate::substrate::{embed64, Grid64};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Tier {
    Must,
    Example,
}

impl Tier {
    pub fn key(self) -> &'static str {
        match self {
            Self::Must => "must",
            Self::Example => "example",
        }
    }
}

/// When a scenario stops (two settling rounds are appended afterwards regardless).
#[derive(Clone, Copy, Debug)]
pub enum Stop {
    /// This many completed translations, then no remaining control roles.
    Transport(u64),
    /// This many completed rewrite placements, then no remaining control roles.
    Rewrites(u64),
    /// A rewrite request rose and fully declined without placing.
    Declined,
    /// This many consecutive sweep ends with zero word changes and no control roles.
    Quiet(u32),
}

#[derive(Clone, Copy, Debug)]
pub struct Expect {
    pub transport_min: u64,
    pub rewrites: u64,
    pub ints: u64,
    pub controls_clear: bool,
    pub matter_unchanged: bool,
    pub chi_seen: bool,
    pub frontier_min: usize,
}

#[derive(Clone, Copy, Debug)]
enum Setup {
    Translate { bend: bool },
    ApplyFork { block_preferred: bool, block_fallback: bool },
    Docked { consumer: Tag, producer: Tag },
    Reduce,
    Embed,
    Cascade,
    CascadeEps,
    CascadeStem,
}

#[derive(Clone, Copy, Debug)]
pub struct Scenario {
    pub key: &'static str,
    pub tier: Tier,
    pub topo: Topo,
    pub note: &'static str,
    pub term: &'static str,
    pub oracle: &'static str,
    pub stop: Stop,
    pub expect: Expect,
    setup: Setup,
}

pub fn scenarios() -> Vec<Scenario> {
    let mut scenarios = vec![
        Scenario {
            key: "translate-straight",
            tier: Tier::Must,
            topo: Topo::Bilayer,
            note: "An arity-three producer walks three straight one-lane links, one face per \
                translation handshake; its vacated cells become one continuous two-lane cable \
                back to the zipper.",
            term: "F.p —3 links— Out.p",
            oracle: "unchanged net (translation is projection identity)",
            stop: Stop::Transport(3),
            expect: Expect {
                transport_min: 3, rewrites: 0, ints: 0,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::Translate { bend: false },
        },
        Scenario {
            key: "translate-bend",
            tier: Tier::Must,
            topo: Topo::Bilayer,
            note: "The same three-face walk around a bend: the agent's principal and tail \
                faces rotate per hop (E → N → E), and the trailing two-lane cable turns \
                with it.",
            term: "F.p —bent links— Out.p",
            oracle: "unchanged net (translation is projection identity)",
            stop: Stop::Transport(3),
            expect: Expect {
                transport_min: 3, rewrites: 0, ints: 0,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::Translate { bend: true },
        },
        Scenario {
            key: "af-preferred",
            tier: Tier::Must,
            topo: Topo::Bilayer,
            note: "A docked A·F pair requests the preferred U lift and places its 16-cell \
                T1 + Pair workshop, one activation at a time.",
            term: "A.p ↔ F.p",
            oracle: "T1 + Pair",
            stop: Stop::Rewrites(1),
            expect: Expect {
                transport_min: 0, rewrites: 1, ints: 1,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::ApplyFork { block_preferred: false, block_fallback: false },
        },
        Scenario {
            key: "af-fallback",
            tier: Tier::Must,
            topo: Topo::Full3D,
            note: "An inert reciprocal blocker declines the preferred U lift. The request \
                clears without changing matter, retries the D lift once, and places the same \
                16-cell result.",
            term: "A.p ↔ F.p",
            oracle: "T1 + Pair",
            stop: Stop::Rewrites(1),
            expect: Expect {
                transport_min: 0, rewrites: 1, ints: 1,
                controls_clear: true, matter_unchanged: false, chi_seen: true, frontier_min: 0,
            },
            setup: Setup::ApplyFork { block_preferred: true, block_fallback: false },
        },
        Scenario {
            key: "af-blocked",
            tier: Tier::Must,
            topo: Topo::Full3D,
            note: "Inert reciprocal blockers decline both lifts. Both request trees clear, \
                obstruction pressure is emitted, and the pair, its cables, and every blocker \
                remain unchanged.",
            term: "A.p ↔ F.p",
            oracle: "unchanged active pair",
            stop: Stop::Declined,
            expect: Expect {
                transport_min: 0, rewrites: 0, ints: 0,
                controls_clear: true, matter_unchanged: true, chi_seen: true, frontier_min: 1,
            },
            setup: Setup::ApplyFork { block_preferred: true, block_fallback: true },
        },
    ];
    // The rule atlas: every ROM interaction fires its workshop from a docked pair.
    for (index, rule) in crate::rules::RULES.iter().enumerate() {
        if index == crate::rewrite64::APPLY_FORK_RULE as usize { continue; }
        let name = format!("{}·{}", rule.consumer.name(), rule.producer.name());
        let fresh = if rule.fresh.is_empty() {
            "no fresh agents (fusion or erasure)".to_string()
        } else {
            rule.fresh.iter().map(|tag| tag.name()).collect::<Vec<_>>().join(" + ")
        };
        scenarios.push(Scenario {
            key: Box::leak(
                format!("rule-{}", name.to_lowercase().replace('·', "-")).into_boxed_str(),
            ),
            tier: Tier::Must,
            topo: Topo::Full3D,
            note: Box::leak(
                format!("Rule atlas: a docked {name} pair requests and places its workshop → {fresh}.")
                    .into_boxed_str(),
            ),
            term: Box::leak(format!("{name} docked").into_boxed_str()),
            oracle: Box::leak(fresh.into_boxed_str()),
            stop: Stop::Rewrites(1),
            expect: Expect {
                transport_min: 0, rewrites: 1, ints: 1,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::Docked { consumer: rule.consumer, producer: rule.producer },
        });
    }
    scenarios.extend([
        Scenario {
            key: "reduce-apply-fork",
            tier: Tier::Example,
            topo: Topo::Bilayer,
            note: "Reduction end to end: F walks three faces along its principal wire to dock \
                with A, then the A·F workshop fires and places T1 + Pair.",
            term: "A.p —3 links— F.p",
            oracle: "T1 + Pair",
            stop: Stop::Rewrites(1),
            expect: Expect {
                transport_min: 3, rewrites: 1, ints: 1,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::Reduce,
        },
        Scenario {
            key: "cascade-apply-fork",
            tier: Tier::Example,
            topo: Topo::Bilayer,
            note: "Multi-fire cascade: F walks to A and the A·F workshop places T1 + Pair; \
                the L behind F's aux1 zipper branch then crosses the old zipper, the \
                two-lane trunk, and the new boundary zipper as a guest rider (the foreign \
                lane projecting intact throughout) to dock with T1, and T1·L fires. The \
                chain stalls at Unp·Pair: the Pair is arity three and can never ride.",
            term: "A.p —3 links— F.p; F.aux1—L.p, F.aux2—L.p behind the zipper branches",
            oracle: "T1 + Pair, then T1·L, then the Unp·Pair frontier",
            stop: Stop::Quiet(8),
            expect: Expect {
                transport_min: 8, rewrites: 2, ints: 2,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 1,
            },
            setup: Setup::Cascade,
        },
        Scenario {
            key: "cascade-eps-fork",
            tier: Tier::Example,
            topo: Topo::Full3D,
            note: "The first complete multi-fire reduction to normal form: F walks to the \
                arity-one Eps driver and Eps·F places two fresh erasers, one per aux lane. \
                Each L behind F's zipper branches guest-rides the cable run to its eraser \
                (aux1's rider on lane one, aux2's following the reverted elbows on lane \
                zero) and Eps·L fires twice. Nothing remains: every agent is erased.",
            term: "Eps.p —3 links— F.p; F.aux1—L.p, F.aux2—L.p behind the zipper branches",
            oracle: "empty net (Eps·F, then Eps·L twice)",
            stop: Stop::Quiet(8),
            expect: Expect {
                transport_min: 8, rewrites: 3, ints: 3,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 0,
            },
            setup: Setup::CascadeEps,
        },
        Scenario {
            key: "cascade-fork-stem",
            tier: Tier::Example,
            topo: Topo::Bilayer,
            note: "The S-rule through the 63-slot workshop: A·F places T1 + Pair, the S(L) \
                behind F's aux1 branch guest-rides with arity-two lane restoration to dock \
                with T1, and T1·S fires its 63-cell workshop around the A·F residue. The \
                chain then stalls: Unp·Pair and the blocked leaf behind aux2 mark the \
                arity-three crossing frontier.",
            term: "A.p —3 links— F.p; F.aux1—S.p—L, F.aux2—L.p behind the zipper branches",
            oracle: "T1 + Pair, then T1·S (Unp, Dn, A, A, A), then the frontier",
            stop: Stop::Quiet(8),
            expect: Expect {
                transport_min: 8, rewrites: 2, ints: 2,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 1,
            },
            setup: Setup::CascadeStem,
        },
        Scenario {
            key: "embed-term",
            tier: Tier::Example,
            topo: Topo::Full3D,
            note: "The embedder lays out @(F(L,L),L) driven by Nrm. Producers translate, \
                Nrm forces the suspension (Nrm·P fires once, cleanly), and the run stalls \
                at the next workshop availability — the real reduction frontier.",
            term: "@(F(L,L),L) under Nrm",
            oracle: "Nrm·P fires; frontier: A·F workshop availability",
            stop: Stop::Quiet(8),
            expect: Expect {
                transport_min: 1, rewrites: 1, ints: 1,
                controls_clear: true, matter_unchanged: false, chi_seen: false, frontier_min: 1,
            },
            setup: Setup::Embed,
        },
    ]);
    scenarios
}

/// One recorded center activation, reported to the trace callback right after it happens.
/// `changed` lists every cell whose trace-visible state differs from the previous frame:
/// the activated cell when its word changed, plus cells whose observer sid changed.
#[derive(Clone, Debug)]
pub struct Activation {
    pub tick: u64,
    pub round: u64,
    pub index: usize,
    pub sweep: usize,
    pub at: Pos,
    pub before: CellWord,
    pub after: CellWord,
    pub fire: Option<RewriteFire>,
    pub changed: Vec<Pos>,
}

#[derive(Clone, Debug)]
pub struct Outcome {
    pub pass: bool,
    pub fail: Option<String>,
    pub ticks: u64,
    pub sweeps: u64,
    pub transport: u64,
    pub rewrites: u64,
    pub ints: u64,
    pub frontier: usize,
}

fn matter_shape(matter: Matter) -> Matter {
    match matter {
        Matter::Link { ends, lanes, twist, .. } => Matter::Link {
            ends, lanes, twist,
            hot: LaneMask::new(0).unwrap(),
            cooldown: U3::new(0).unwrap(),
            pull: [Pull::None; 2],
        },
        Matter::Zip { trunk, branches, twist, .. } => Matter::Zip {
            trunk, branches, twist,
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

fn occupied_matter(grid: &Grid64) -> BTreeMap<Pos, Matter> {
    grid.cells.iter().filter_map(|(&at, word)| {
        let matter = matter_shape(word.unpack().unwrap().matter);
        (matter != Matter::Empty).then_some((at, matter))
    }).collect()
}

/// An inert reciprocal Out pair occupying one cell that only `lift` (and not
/// `opposite_lift`) requests, so the preferred direction declines. Shared by the
/// fallback/blocked scenarios and by `dump-packed`.
pub fn add_lift_blocker(
    grid: &mut Grid64,
    shadow: &mut Net,
    driver: Pos,
    lift: Dir,
    opposite_lift: Dir,
) {
    let target = apply_fork_workshop(Dir::E, Dir::N, lift, false, false).unwrap();
    let other = apply_fork_workshop(Dir::E, Dir::N, opposite_lift, false, false).unwrap();
    let target_positions = target.slots.iter().map(|slot| add(driver, slot.at))
        .collect::<BTreeSet<_>>();
    let other_positions = other.slots.iter().map(|slot| add(driver, slot.at))
        .collect::<BTreeSet<_>>();
    let blocked = *target_positions.difference(&other_positions)
        .find(|at| grid.is_empty(**at))
        .expect("lift needs a private request cell");
    let forbidden = target_positions.union(&other_positions).copied().collect::<BTreeSet<_>>();
    let (face, buddy) = DIRS.into_iter().find_map(|face| {
        let buddy = step(blocked, face);
        (grid.is_empty(buddy) && !forbidden.contains(&buddy)).then_some((face, buddy))
    }).expect("lift blocker needs one adjacent private cell");

    let blocker_sid = shadow.mk(Tag::Out);
    let buddy_sid = shadow.mk(Tag::Out);
    shadow.link(blocker_sid, 0, buddy_sid, 0);
    grid.set(blocked, DecodedCell {
        matter: Matter::Agent {
            tag: Tag::Out,
            principal: face,
            tail: None,
            aux_flip: false,
        },
        ..DecodedCell::default()
    });
    grid.set(buddy, DecodedCell {
        matter: Matter::Agent {
            tag: Tag::Out,
            principal: face.opp(),
            tail: None,
            aux_flip: false,
        },
        ..DecodedCell::default()
    });
    grid.observer_sid.insert(blocked, blocker_sid);
    grid.observer_sid.insert(buddy, buddy_sid);
}

fn add(origin: Pos, relative: Pos) -> Pos {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

/// An arity-three F producer with a three-link principal wire (straight or bent) ending at
/// an inert Out pad, and both aux lanes zipped behind it to private pads.
fn translate_fixture(topo: Topo, bend: bool) -> (Grid64, Net) {
    let mut shadow = Net::new();
    let f = shadow.mk(Tag::F);
    let mut grid = Grid64::new(topo);
    let at = (0, 0, 0);
    put_demo(&mut grid, at, Matter::Agent {
        tag: Tag::F,
        principal: Dir::E,
        tail: Some(Dir::W),
        aux_flip: false,
    });
    grid.observer_sid.insert(at, f);

    let (links, pad) = if bend {
        (
            vec![
                ((1, 0, 0), Dir::W, Dir::N),
                ((1, -1, 0), Dir::S, Dir::E),
                ((2, -1, 0), Dir::W, Dir::E),
            ],
            (3, -1, 0),
        )
    } else {
        (
            vec![
                ((1, 0, 0), Dir::W, Dir::E),
                ((2, 0, 0), Dir::W, Dir::E),
                ((3, 0, 0), Dir::W, Dir::E),
            ],
            (4, 0, 0),
        )
    };
    for (at, a, b) in links {
        put_demo(&mut grid, at, cold_link(a, b));
    }
    let out0 = shadow.mk(Tag::Out);
    shadow.link(f, 0, out0, 0);
    put_demo(&mut grid, pad, Matter::Agent {
        tag: Tag::Out,
        principal: Dir::W,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert(pad, out0);

    let zip = (-1, 0, 0);
    put_demo(&mut grid, zip, Matter::Zip {
        trunk: Dir::E,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, f, 1, zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, f, 2, zip, Dir::S, 2);
    grid.check_projection(&shadow);
    (grid, shadow)
}

/// A dockable pair at distance three: A and F face each other along a straight one-lane
/// principal wire, each dragging its two-lane aux cable to a private zipper.
fn reduce_fixture(topo: Topo) -> (Grid64, Net) {
    let mut shadow = Net::new();
    let consumer = shadow.mk(Tag::A);
    let producer = shadow.mk(Tag::F);
    shadow.link(consumer, 0, producer, 0);
    let mut grid = Grid64::new(topo);
    let c = (0, 0, 0);
    let p = (4, 0, 0);
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

    for at in [(1, 0, 0), (2, 0, 0), (3, 0, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::W, Dir::E));
    }

    let cable = Matter::Link {
        ends: FacePair::new(Dir::E, Dir::W).unwrap(),
        lanes: LaneCount::Two,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    };
    put_demo(&mut grid, (-1, 0, 0), cable);
    let consumer_zip = (-2, 0, 0);
    put_demo(&mut grid, consumer_zip, Matter::Zip {
        trunk: Dir::E,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, consumer, 1, consumer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, consumer, 2, consumer_zip, Dir::S, 2);

    put_demo(&mut grid, (5, 0, 0), cable);
    let producer_zip = (6, 0, 0);
    put_demo(&mut grid, producer_zip, Matter::Zip {
        trunk: Dir::W,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, producer, 1, producer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, producer, 2, producer_zip, Dir::S, 2);
    grid.check_projection(&shadow);
    (grid, shadow)
}

/// The cascade fixture: `reduce_fixture` with real L producers behind both of F's zipper
/// branches (one-lane wires from the branches to L agents facing back toward the zip) and
/// A's auxes on private Out pads. F carries `aux_flip`, so its aux1 lane — the wire the
/// T1 boundary zipper later splices to T1's principal — is the physical lane one the
/// phase-one lane adapter requires of an arity-one rider (LOCAL_CA_DESIGN §7.3). The L
/// behind aux2 starts two links farther back, so aux1's rider always wins its zipper.
fn cascade_fixture(topo: Topo) -> (Grid64, Net) {
    let mut shadow = Net::new();
    let consumer = shadow.mk(Tag::A);
    let producer = shadow.mk(Tag::F);
    let l1 = shadow.mk(Tag::L);
    let l2 = shadow.mk(Tag::L);
    shadow.link(consumer, 0, producer, 0);
    shadow.link(producer, 1, l1, 0);
    shadow.link(producer, 2, l2, 0);
    let mut grid = Grid64::new(topo);
    let c = (0, 0, 0);
    let p = (4, 0, 0);
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
        aux_flip: true,
    });
    grid.observer_sid.insert(c, consumer);
    grid.observer_sid.insert(p, producer);

    for at in [(1, 0, 0), (2, 0, 0), (3, 0, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::W, Dir::E));
    }

    let cable = Matter::Link {
        ends: FacePair::new(Dir::E, Dir::W).unwrap(),
        lanes: LaneCount::Two,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    };
    put_demo(&mut grid, (-1, 0, 0), cable);
    let consumer_zip = (-2, 0, 0);
    put_demo(&mut grid, consumer_zip, Matter::Zip {
        trunk: Dir::E,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, consumer, 1, consumer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, consumer, 2, consumer_zip, Dir::S, 2);

    put_demo(&mut grid, (5, 0, 0), cable);
    let producer_zip = (6, 0, 0);
    put_demo(&mut grid, producer_zip, Matter::Zip {
        trunk: Dir::W,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    // F.aux1 (physical lane one) exits branch S: one link to the L facing back.
    put_demo(&mut grid, (6, 1, 0), cold_link(Dir::N, Dir::S));
    put_demo(&mut grid, (6, 2, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::N,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((6, 2, 0), l1);
    // F.aux2 (physical lane zero) exits branch N: three links to its L facing back.
    for at in [(6, -1, 0), (6, -2, 0), (6, -3, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::N, Dir::S));
    }
    put_demo(&mut grid, (6, -4, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::S,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((6, -4, 0), l2);
    grid.check_projection(&shadow);
    (grid, shadow)
}

/// The producer side of the eps/stem cascades: F at `p` facing W with `aux_flip` (aux1 on
/// physical lane one), its two-lane tail cable and zipper, and child wires laid along the
/// branch faces. Returns the zipper position.
fn cascade_producer_side(grid: &mut Grid64, p: Pos) -> Pos {
    put_demo(grid, p, Matter::Agent {
        tag: Tag::F,
        principal: Dir::W,
        tail: Some(Dir::E),
        aux_flip: true,
    });
    let cable = Matter::Link {
        ends: FacePair::new(Dir::E, Dir::W).unwrap(),
        lanes: LaneCount::Two,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    };
    put_demo(grid, step(p, Dir::E), cable);
    let zip = step(step(p, Dir::E), Dir::E);
    put_demo(grid, zip, Matter::Zip {
        trunk: Dir::W,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    zip
}

/// The erasure cascade: an arity-one Eps driver at the origin facing E, F three one-lane
/// links away (aux1 on lane one, aux2 on lane zero, as in `cascade_fixture`), and real L
/// producers behind both zipper branches on one-lane wires facing back toward the zip.
/// Eps·F places one fresh eraser per lane; each L rides to its eraser and erases, so the
/// lane-one rider must win the zipper: it starts one link out, the lane-zero rider three.
fn cascade_eps_fixture(topo: Topo) -> (Grid64, Net) {
    let mut shadow = Net::new();
    let consumer = shadow.mk(Tag::Eps);
    let producer = shadow.mk(Tag::F);
    let l1 = shadow.mk(Tag::L);
    let l2 = shadow.mk(Tag::L);
    shadow.link(consumer, 0, producer, 0);
    shadow.link(producer, 1, l1, 0);
    shadow.link(producer, 2, l2, 0);
    let mut grid = Grid64::new(topo);
    let c = (0, 0, 0);
    let p = (4, 0, 0);
    put_demo(&mut grid, c, Matter::Agent {
        tag: Tag::Eps,
        principal: Dir::E,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert(c, consumer);
    let zip = cascade_producer_side(&mut grid, p);
    grid.observer_sid.insert(p, producer);
    for at in [(1, 0, 0), (2, 0, 0), (3, 0, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::W, Dir::E));
    }
    // F.aux1 (physical lane one) exits branch S: one link to the L facing back.
    put_demo(&mut grid, (zip.0, 1, 0), cold_link(Dir::N, Dir::S));
    put_demo(&mut grid, (zip.0, 2, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::N,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((zip.0, 2, 0), l1);
    // F.aux2 (physical lane zero) exits branch N: three links to its L facing back.
    for at in [(zip.0, -1, 0), (zip.0, -2, 0), (zip.0, -3, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::N, Dir::S));
    }
    put_demo(&mut grid, (zip.0, -4, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::S,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((zip.0, -4, 0), l2);
    grid.check_projection(&shadow);
    (grid, shadow)
}

/// The stem cascade: `cascade_fixture` with F's aux1 branch leading to a real S(L)
/// producer — an S facing the branch wire with its own arity-two tail wire to an L behind
/// it — and aux2 to a plain L. A and F start docked: with a three-link walk, F's stretched
/// tail cable lands exactly on the T1·S workshop's preferred-lift region (its east arm
/// reaches the old principal wire row) and the A·F residue sits in the fallback region,
/// so the 63-cell patch can only ever place here, where the producer infrastructure is
/// one cable cell deep. Link budgets keep the ride order deterministic across seeds: S
/// rides first (one link), aux2's L second (two links — it then wedges itself on the
/// restored zipper as a lane-zero guest), and S's tail L last (three links), so the fresh
/// A2 of T1·S never gets a visitor.
fn cascade_stem_fixture(topo: Topo) -> (Grid64, Net) {
    let mut shadow = Net::new();
    let consumer = shadow.mk(Tag::A);
    let producer = shadow.mk(Tag::F);
    let s = shadow.mk(Tag::S);
    let l = shadow.mk(Tag::L);
    let l2 = shadow.mk(Tag::L);
    shadow.link(consumer, 0, producer, 0);
    shadow.link(producer, 1, s, 0);
    shadow.link(s, 1, l, 0);
    shadow.link(producer, 2, l2, 0);
    let mut grid = Grid64::new(topo);
    let c = (0, 0, 0);
    let p = (1, 0, 0);
    put_demo(&mut grid, c, Matter::Agent {
        tag: Tag::A,
        principal: Dir::E,
        tail: Some(Dir::W),
        aux_flip: false,
    });
    grid.observer_sid.insert(c, consumer);
    let zip = cascade_producer_side(&mut grid, p);
    grid.observer_sid.insert(p, producer);

    let cable = Matter::Link {
        ends: FacePair::new(Dir::E, Dir::W).unwrap(),
        lanes: LaneCount::Two,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    };
    put_demo(&mut grid, (-1, 0, 0), cable);
    let consumer_zip = (-2, 0, 0);
    put_demo(&mut grid, consumer_zip, Matter::Zip {
        trunk: Dir::E,
        branches: [Dir::N, Dir::S],
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    });
    attach_demo_out(&mut shadow, &mut grid, consumer, 1, consumer_zip, Dir::N, 2);
    attach_demo_out(&mut shadow, &mut grid, consumer, 2, consumer_zip, Dir::S, 2);

    // F.aux1 (physical lane one) exits branch S: one link to the S facing back, which
    // drags its own arity-two tail wire (three links) to an L.
    put_demo(&mut grid, (zip.0, 1, 0), cold_link(Dir::N, Dir::S));
    put_demo(&mut grid, (zip.0, 2, 0), Matter::Agent {
        tag: Tag::S,
        principal: Dir::N,
        tail: Some(Dir::S),
        aux_flip: false,
    });
    grid.observer_sid.insert((zip.0, 2, 0), s);
    for at in [(zip.0, 3, 0), (zip.0, 4, 0), (zip.0, 5, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::N, Dir::S));
    }
    put_demo(&mut grid, (zip.0, 6, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::N,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((zip.0, 6, 0), l);
    // F.aux2 (physical lane zero) exits branch N: two links to its L facing back.
    for at in [(zip.0, -1, 0), (zip.0, -2, 0)] {
        put_demo(&mut grid, at, cold_link(Dir::N, Dir::S));
    }
    put_demo(&mut grid, (zip.0, -3, 0), Matter::Agent {
        tag: Tag::L,
        principal: Dir::S,
        tail: None,
        aux_flip: false,
    });
    grid.observer_sid.insert((zip.0, -3, 0), l2);
    grid.check_projection(&shadow);
    (grid, shadow)
}

/// Build the initial state of a scenario. Builders check projection at construction.
pub fn build(sc: &Scenario) -> (Grid64, Net) {
    match sc.setup {
        Setup::Translate { bend } => translate_fixture(sc.topo, bend),
        Setup::ApplyFork { block_preferred, block_fallback } => {
            let (mut grid, mut shadow, driver) = apply_fork_fixture64(sc.topo);
            if block_preferred { add_lift_blocker(&mut grid, &mut shadow, driver, Dir::U, Dir::D); }
            if block_fallback { add_lift_blocker(&mut grid, &mut shadow, driver, Dir::D, Dir::U); }
            (grid, shadow)
        }
        Setup::Reduce => reduce_fixture(sc.topo),
        Setup::Cascade => cascade_fixture(sc.topo),
        Setup::CascadeEps => cascade_eps_fixture(sc.topo),
        Setup::CascadeStem => cascade_stem_fixture(sc.topo),
        Setup::Docked { consumer, producer } => {
            let (grid, shadow, _) = crate::rewrite64::docked_fixture64(consumer, producer, sc.topo);
            (grid, shadow)
        }
        Setup::Embed => {
            let mut shadow = Net::new();
            let root = shadow.build(&ap(f2(Term::L, Term::L), Term::L));
            shadow.drive(root);
            (embed64(&shadow, sc.topo), shadow)
        }
    }
}

/// Run one scenario under fair deterministic random live-read activation. `on_activation`
/// observes the grid after every single center update (the trace generator serializes one
/// frame per call). The run stops once the scenario's stop condition has held and the
/// lattice has then stayed completely quiet — no word changes, no control roles — for
/// three consecutive sweeps, so projections are checked at a genuine checkpoint.
pub fn run(
    sc: &Scenario,
    grid: &mut Grid64,
    shadow: &mut Net,
    seed: u64,
    sweep_cap: u64,
    on_activation: &mut dyn FnMut(&Grid64, &Net, &Activation),
) -> Outcome {
    let before_matter = sc.expect.matter_unchanged.then(|| occupied_matter(grid));
    let mut tick = 0u64;
    let mut peak_chi = 0u8;
    let mut saw_protocol = false;
    // `armed` becomes true once the scenario's stop condition first holds; the run then
    // continues until the lattice is quiet, so late translations can finish and fields can
    // relax into the recorded tail.
    let mut armed = matches!(sc.stop, Stop::Quiet(_));
    let mut quiet = 0u32;
    let quiet_target = match sc.stop {
        Stop::Quiet(k) => k,
        _ => 3,
    };
    let mut done = false;
    let mut rounds_run = 0u64;

    let mut activate_one = |grid: &mut Grid64,
                            shadow: &mut Net,
                            tick: &mut u64,
                            peak_chi: &mut u8,
                            round: u64,
                            index: usize,
                            sweep: usize,
                            at: Pos| {
        *tick += 1;
        let before = grid.word(at);
        let before_observers = grid.observer_sid.clone();
        let result = activate_with_shadow(grid, shadow, at);
        let fire = result.and_then(|(_, _, e)| e.rewrite_fire);
        if let Some(cell) = grid.decoded(at) { *peak_chi = (*peak_chi).max(cell.chi); }
        let mut changed = Vec::new();
        if before != grid.word(at) { changed.push(at); }
        for p in before_observers.keys().chain(grid.observer_sid.keys()) {
            if before_observers.get(p) != grid.observer_sid.get(p) && !changed.contains(p) {
                changed.push(*p);
            }
        }
        let activation = Activation {
            tick: *tick, round, index, sweep, at,
            before, after: grid.word(at), fire, changed,
        };
        on_activation(grid, shadow, &activation);
        result.is_some()
    };

    for round in 0..sweep_cap {
        rounds_run = round + 1;
        let order = activation_order(grid, seed, round);
        let sweep = order.len();
        let mut changes = 0usize;
        for (index, at) in order.into_iter().enumerate() {
            let changed = activate_one(
                grid, shadow, &mut tick, &mut peak_chi, round, index, sweep, at,
            );
            saw_protocol |= grid.has_protocol();
            if changed { changes += 1; }
            let newly_armed = match sc.stop {
                Stop::Transport(n) => grid.transport >= n && !grid.has_protocol(),
                Stop::Rewrites(n) => grid.rewrites >= n && !grid.has_protocol(),
                Stop::Declined => saw_protocol && grid.rewrites == 0 && !grid.has_protocol(),
                Stop::Quiet(_) => true,
            };
            armed |= newly_armed;
            // A declined rewrite is re-requested by the still-docked driver on its next
            // activation (the liveness retry against clearable blockage), so the decline
            // invariant is checked at the exact instant the request tree has fully cleared.
            if newly_armed && matches!(sc.stop, Stop::Declined) {
                done = true;
                break;
            }
        }
        if done { break; }
        quiet = if armed && changes == 0 && !grid.has_protocol() { quiet + 1 } else { 0 };
        if quiet >= quiet_target { done = true; break; }
    }
    evaluate(sc, grid, shadow, before_matter, peak_chi, done, tick, rounds_run)
}

fn evaluate(
    sc: &Scenario,
    grid: &Grid64,
    shadow: &Net,
    before_matter: Option<BTreeMap<Pos, Matter>>,
    peak_chi: u8,
    done: bool,
    ticks: u64,
    sweeps: u64,
) -> Outcome {
    let mut problems = vec![];
    if !done {
        problems.push("stop condition not reached within the sweep cap".to_string());
    }
    if grid.transport < sc.expect.transport_min {
        problems.push(format!("transport {} < expected {}", grid.transport, sc.expect.transport_min));
    }
    if grid.rewrites != sc.expect.rewrites {
        problems.push(format!("rewrites {} != expected {}", grid.rewrites, sc.expect.rewrites));
    }
    if shadow.ints != sc.expect.ints {
        problems.push(format!("interactions {} != expected {}", shadow.ints, sc.expect.ints));
    }
    if sc.expect.controls_clear && grid.has_protocol() {
        problems.push("control roles left behind".to_string());
    }
    if let Some(before) = before_matter {
        let after = occupied_matter(grid);
        if after != before {
            problems.push("matter changed in a matter-unchanged scenario".to_string());
        }
    }
    if sc.expect.chi_seen && peak_chi == 0 {
        problems.push("no obstruction pressure was emitted".to_string());
    }
    let frontier = shadow.all_active_pairs().len();
    if frontier < sc.expect.frontier_min {
        problems.push(format!("frontier pairs {} < expected {}", frontier, sc.expect.frontier_min));
    }
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| grid.check_projection(shadow)))
        .unwrap_or_else(|_| problems.push("projection mismatch".to_string()));
    Outcome {
        pass: problems.is_empty(),
        fail: (!problems.is_empty()).then(|| problems.join("; ")),
        ticks,
        sweeps,
        transport: grid.transport,
        rewrites: grid.rewrites,
        ints: shadow.ints,
        frontier,
    }
}
