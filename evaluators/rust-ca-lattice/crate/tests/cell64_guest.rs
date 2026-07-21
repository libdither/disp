//! Guest-rider cable crossings (LOCAL_CA_DESIGN §7.3): arity-one/two producers translate
//! through zipper cells and two-lane cable trunk cells along their principal wire while
//! the foreign lane sharing those cells projects undisturbed — codec, projection, and
//! refusal coverage for the packed substrate's cable-shift phase 1.

use rust_ca_lattice::cell64::{
    CellWord, DecodedCell, Dir, FacePair, LaneCount, LaneMask, Matter, PackError, Pull, U3,
};
use rust_ca_lattice::lattice::{Pos, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::packed_local::{activation_order, activate_with_shadow};
use rust_ca_lattice::rewrite64::{cold_link, put_demo};
use rust_ca_lattice::rules::Tag;
use rust_ca_lattice::substrate::Grid64;

fn cold_cable(a: Dir, b: Dir) -> Matter {
    Matter::Link {
        ends: FacePair::new(a, b).unwrap(),
        lanes: LaneCount::Two,
        twist: false,
        hot: LaneMask::new(0).unwrap(),
        cooldown: U3::new(0).unwrap(),
        pull: [Pull::None; 2],
    }
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

fn agent(tag: Tag, principal: Dir, tail: Option<Dir>) -> Matter {
    Matter::Agent { tag, principal, tail, aux_flip: false }
}

fn out(grid: &mut Grid64, shadow: &mut Net, at: Pos, principal: Dir) -> u32 {
    let sid = shadow.mk(Tag::Out);
    put_demo(grid, at, agent(Tag::Out, principal, None));
    grid.observer_sid.insert(at, sid);
    sid
}

// -------------------------------------------------------------------------------------
// 1. Codec
// -------------------------------------------------------------------------------------

#[test]
fn guest_codec_roundtrips_and_rejects() {
    let cases = [
        Matter::GuestZip {
            tag: Tag::L, plane: 0, trunk: Dir::E, branches: [Dir::N, Dir::S],
            twist: false, deep: false,
        },
        Matter::GuestZip {
            tag: Tag::S, plane: 1, trunk: Dir::W, branches: [Dir::U, Dir::E],
            twist: true, deep: true,
        },
        Matter::GuestLink {
            tag: Tag::S, principal: Dir::W, plane: 1,
            ends: FacePair::new(Dir::E, Dir::W).unwrap(), twist: true,
        },
        Matter::GuestLink {
            tag: Tag::L, principal: Dir::D, plane: 0,
            ends: FacePair::new(Dir::D, Dir::N).unwrap(), twist: false,
        },
    ];
    for matter in cases {
        let decoded = DecodedCell { matter, chi: 0x5a, sigma: 0xa5, ..DecodedCell::default() };
        let word = CellWord::pack(decoded).expect("guest packs");
        assert_eq!(word.unpack().unwrap(), decoded, "guest roundtrip {matter:?}");
    }

    let guest_zip = Matter::GuestZip {
        tag: Tag::F, plane: 0, trunk: Dir::E, branches: [Dir::N, Dir::S],
        twist: false, deep: false,
    };
    assert_eq!(
        CellWord::pack(DecodedCell { matter: guest_zip, ..DecodedCell::default() }),
        Err(PackError::InvalidAgentGeometry),
        "arity-three tags never ride",
    );
    let bad_plane = Matter::GuestZip {
        tag: Tag::L, plane: 2, trunk: Dir::E, branches: [Dir::N, Dir::S],
        twist: false, deep: false,
    };
    assert_eq!(
        CellWord::pack(DecodedCell { matter: bad_plane, ..DecodedCell::default() }),
        Err(PackError::InvalidAgentGeometry),
        "plane is one bit",
    );
    let bad_geometry = Matter::GuestZip {
        tag: Tag::L, plane: 0, trunk: Dir::E, branches: [Dir::E, Dir::S],
        twist: false, deep: false,
    };
    assert_eq!(
        CellWord::pack(DecodedCell { matter: bad_geometry, ..DecodedCell::default() }),
        Err(PackError::InvalidZipperGeometry),
        "zipper geometry must decode",
    );
    let outside_ends = Matter::GuestLink {
        tag: Tag::L, principal: Dir::N, plane: 0,
        ends: FacePair::new(Dir::E, Dir::W).unwrap(), twist: false,
    };
    assert_eq!(
        CellWord::pack(DecodedCell { matter: outside_ends, ..DecodedCell::default() }),
        Err(PackError::InvalidAgentGeometry),
        "the ridden end must be one of the link's ends",
    );
    let packed_guest_link = Matter::GuestLink {
        tag: Tag::Pair, principal: Dir::E, plane: 0,
        ends: FacePair::new(Dir::E, Dir::W).unwrap(), twist: false,
    };
    assert_eq!(
        CellWord::pack(DecodedCell { matter: packed_guest_link, ..DecodedCell::default() }),
        Err(PackError::InvalidAgentGeometry),
        "arity-three tags never ride cables",
    );
}

// -------------------------------------------------------------------------------------
// Crossing fixtures: producer — zip — cable — zip — one-lane pad, with a foreign Out—Out
// wire sharing the cable run on lane zero (the rider always takes lane one).
// -------------------------------------------------------------------------------------

/// The shared run: zipper `z1` (branches `[foreign, rider]`), one two-lane cable cell,
/// zipper `z2` (branches `[foreign, rider]`), one one-lane link to the rider's pad.
/// `producer` sits behind `z1`'s rider branch on a one-lane wire. Returns the producer's
/// home position and sid, plus the two foreign pad sids.
fn crossing_fixture(
    topo: Topo,
    producer: Tag,
) -> (Grid64, Net, Pos, u32, u32, u32) {
    let mut shadow = Net::new();
    let mut grid = Grid64::new(topo);
    let p_sid = shadow.mk(producer);

    // Rider wire, west to east: producer — link — z1 — cable — z2 — link — pad.
    put_demo(&mut grid, (1, 0, 0), cold_link(Dir::W, Dir::E));
    let z1: Pos = (2, 0, 0);
    put_demo(&mut grid, z1, cold_zip(Dir::E, [Dir::N, Dir::W]));
    put_demo(&mut grid, (3, 0, 0), cold_cable(Dir::E, Dir::W));
    let z2: Pos = (4, 0, 0);
    put_demo(&mut grid, z2, cold_zip(Dir::W, [Dir::N, Dir::E]));
    put_demo(&mut grid, (5, 0, 0), cold_link(Dir::W, Dir::E));
    let pad = out(&mut grid, &mut shadow, (6, 0, 0), Dir::W);
    shadow.link(p_sid, 0, pad, 0);

    let home = (0, 0, 0);
    put_demo(&mut grid, home, agent(producer, Dir::E, (producer.arity() > 1).then_some(Dir::W)));
    grid.observer_sid.insert(home, p_sid);

    // Arity-two aux wire to its own pad.
    if producer.arity() == 2 {
        put_demo(&mut grid, (-1, 0, 0), cold_link(Dir::E, Dir::W));
        let aux = out(&mut grid, &mut shadow, (-2, 0, 0), Dir::E);
        shadow.link(p_sid, 1, aux, 0);
    }

    // Foreign wire on lane zero: out pads behind both zippers' north branches.
    put_demo(&mut grid, (2, -1, 0), cold_link(Dir::S, Dir::N));
    let foreign_a = out(&mut grid, &mut shadow, (2, -2, 0), Dir::S);
    put_demo(&mut grid, (4, -1, 0), cold_link(Dir::S, Dir::N));
    let foreign_b = out(&mut grid, &mut shadow, (4, -2, 0), Dir::S);
    shadow.link(foreign_a, 0, foreign_b, 0);

    grid.check_projection(&shadow);
    (grid, shadow, home, p_sid, foreign_a, foreign_b)
}

/// Run fair live-read sweeps (seed 0) until the lattice stays quiet for `settle`
/// consecutive sweep ends. Projection is checked at every control-free sweep end, so the
/// foreign lane must project intact after every single crossing, not only at the end.
fn run_to_quiet(grid: &mut Grid64, shadow: &mut Net, settle: u32) {
    let mut quiet = 0;
    for round in 0..2_000u64 {
        for at in activation_order(grid, 0, round) {
            activate_with_shadow(grid, shadow, at);
        }
        if !grid.has_protocol() {
            grid.check_projection(shadow);
            quiet += 1;
            if quiet >= settle { return; }
        } else {
            quiet = 0;
        }
    }
    panic!("crossing fixture never quiesced");
}

fn guest_at(grid: &Grid64, p: Pos) -> bool {
    matches!(
        grid.matter(p),
        Some(Matter::GuestZip { .. } | Matter::GuestLink { .. })
    )
}

#[test]
fn arity_one_producer_crosses_zip_cable_zip() {
    let (mut grid, mut shadow, home, l, foreign_a, foreign_b) =
        crossing_fixture(Topo::Bilayer, Tag::L);
    run_to_quiet(&mut grid, &mut shadow, 4);

    // The L's sid rode to the far pad side: five translation hops, no protocol left.
    let (&pos, _) = grid.observer_sid.iter().find(|(_, sid)| **sid == l)
        .expect("rider sid survived");
    assert_eq!(pos, (5, 0, 0), "rider reached the pad-side link");
    assert!(grid.transport >= 5, "transport {} covers the five hops", grid.transport);
    assert!(!grid.has_protocol());
    assert_eq!(grid.matter(home), Some(Matter::Empty), "the origin was consumed");
    // Both zippers and the cable reverted to one-lane elbows on the foreign lane.
    for at in [(2, 0, 0), (3, 0, 0), (4, 0, 0)] {
        match grid.matter(at) {
            Some(Matter::Link { lanes: LaneCount::One, .. }) => {}
            other => panic!("cell {at:?} did not revert to a one-lane elbow: {other:?}"),
        }
    }
    // The foreign wire still projects pad-to-pad.
    let a = grid.observer_sid.iter().find(|(_, sid)| **sid == foreign_a).unwrap().0;
    let b = grid.observer_sid.iter().find(|(_, sid)| **sid == foreign_b).unwrap().0;
    assert_eq!(grid.trace(*a, 0), Some((*b, 0)), "foreign lane intact");
    grid.check_projection(&shadow);
}

#[test]
fn arity_two_producer_crosses_zip_cable_zip() {
    let (mut grid, mut shadow, _home, s, _, _) = crossing_fixture(Topo::Bilayer, Tag::S);
    run_to_quiet(&mut grid, &mut shadow, 4);

    let (&pos, _) = grid.observer_sid.iter().find(|(_, sid)| **sid == s)
        .expect("rider sid survived");
    assert_eq!(pos, (5, 0, 0), "rider reached the pad-side link");
    assert!(grid.transport >= 5, "transport {} covers the five hops", grid.transport);
    assert!(!grid.has_protocol());
    // Arity two restores the original crossing matter: both zippers and the cable are
    // back exactly as built, with the rider's aux lane following through the same cells.
    assert!(matches!(grid.matter((2, 0, 0)), Some(Matter::Zip { .. })));
    assert!(matches!(
        grid.matter((3, 0, 0)),
        Some(Matter::Link { lanes: LaneCount::Two, .. })
    ));
    assert!(matches!(grid.matter((4, 0, 0)), Some(Matter::Zip { .. })));
    grid.check_projection(&shadow);
}

#[test]
fn arity_three_producer_is_never_offered_a_crossing() {
    let mut shadow = Net::new();
    let mut grid = Grid64::new(Topo::Bilayer);
    let f = shadow.mk(Tag::F);
    // F's principal faces a zipper branch (a crossing target); its two-lane aux tail is
    // zipped to private pads behind it. The zipper's trunk carries F's principal wire on
    // lane zero and a foreign wire on lane one through a cable to a second zipper.
    let home = (0, 0, 0);
    put_demo(&mut grid, home, agent(Tag::F, Dir::E, Some(Dir::W)));
    grid.observer_sid.insert(home, f);
    let zip: Pos = (1, 0, 0);
    put_demo(&mut grid, zip, cold_zip(Dir::N, [Dir::W, Dir::E]));
    put_demo(&mut grid, (2, 0, 0), cold_link(Dir::W, Dir::E));
    let foreign_b = out(&mut grid, &mut shadow, (3, 0, 0), Dir::W);
    put_demo(&mut grid, (1, -1, 0), cold_cable(Dir::N, Dir::S));
    let zip2: Pos = (1, -2, 0);
    put_demo(&mut grid, zip2, cold_zip(Dir::S, [Dir::W, Dir::E]));
    put_demo(&mut grid, (0, -2, 0), cold_link(Dir::E, Dir::W));
    let pad = out(&mut grid, &mut shadow, (-1, -2, 0), Dir::E);
    shadow.link(f, 0, pad, 0);
    put_demo(&mut grid, (2, -2, 0), cold_link(Dir::W, Dir::E));
    let foreign_a = out(&mut grid, &mut shadow, (3, -2, 0), Dir::W);
    shadow.link(foreign_a, 0, foreign_b, 0);

    // F's aux tail.
    put_demo(&mut grid, (-1, 0, 0), cold_cable(Dir::E, Dir::W));
    let tail_zip: Pos = (-2, 0, 0);
    put_demo(&mut grid, tail_zip, cold_zip(Dir::E, [Dir::N, Dir::S]));
    rust_ca_lattice::rewrite64::attach_demo_out(
        &mut shadow, &mut grid, f, 1, tail_zip, Dir::N, 2,
    );
    rust_ca_lattice::rewrite64::attach_demo_out(
        &mut shadow, &mut grid, f, 2, tail_zip, Dir::S, 2,
    );
    grid.check_projection(&shadow);
    let before: Vec<_> = grid.cells.iter().map(|(p, w)| (*p, *w)).collect();

    for round in 0..64u64 {
        for at in activation_order(&grid, 0, round) {
            activate_with_shadow(&mut grid, &mut shadow, at);
        }
        assert!(!grid.has_protocol(), "arity-three crossing protocol stuck at round {round}");
        assert!(
            !grid.cells.keys().any(|p| guest_at(&grid, *p)),
            "an arity-three rider materialized at round {round}",
        );
    }
    assert_eq!(grid.transport, 0, "the fork never moved");
    assert_eq!(grid.matter(home), Some(agent(Tag::F, Dir::E, Some(Dir::W))));
    // Only χ/σ fields may have drifted; every matter value is exactly as built.
    for (p, word) in &before {
        assert_eq!(
            grid.matter(*p),
            Some(word.unpack().unwrap().matter),
            "matter drifted at {p:?}",
        );
    }
    grid.check_projection(&shadow);
}
