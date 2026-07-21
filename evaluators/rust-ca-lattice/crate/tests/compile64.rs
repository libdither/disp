//! Validation for the compiled rewrite workshops: every ROM rule gets a structurally
//! sound face-relative workshop that (b) implements its interaction exactly when stamped
//! into the docked fixture and (c) is driven to completion by the live request/place
//! protocol. Rule A·F validates the hand-crafted workshop through the same checks.

use rust_ca_lattice::cell64::{CellWord, Control, DecodedCell, Dir, Matter, RewriteRole};
use rust_ca_lattice::compile64::{dir_vec, vec_dir, workshop_for};
use rust_ca_lattice::lattice::{step, Pos, Topo};
use rust_ca_lattice::packed_local::{activation_order, activate_with_shadow, sweep_permuted};
use rust_ca_lattice::rewrite64::{
    apply_compiled_patch, docked_fixture64, RewritePatch64, Workshop64,
};
use rust_ca_lattice::rules::RULES;
use std::collections::{BTreeSet, VecDeque};

const CONSUMER: Pos = (0, 0, 0);
const PRODUCER: Pos = (1, 0, 0);

fn rule_name(index: usize) -> String {
    let rule = &RULES[index];
    format!("{index} {}·{}", rule.consumer.name(), rule.producer.name())
}

/// The canonical (axis E) patch form of a workshop, as `apply_compiled_patch` consumes it.
fn canonical_patch(workshop: &Workshop64, side: Dir, lift: Dir) -> RewritePatch64 {
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
        axis: Dir::E,
        side,
        lift,
    }
}

/// Structural invariants the live protocol relies on, checked per workshop.
fn check_structure(label: &str, workshop: &Workshop64, producer: Pos) {
    assert!(workshop.slots.len() <= 63, "{label}: {} slots exceed U6", workshop.slots.len());
    let mut positions = BTreeSet::new();
    for slot in &workshop.slots {
        assert!(positions.insert(slot.at), "{label}: duplicate slot position {:?}", slot.at);
        assert!(
            CellWord::pack(DecodedCell {
                matter: slot.matter,
                control: Control::Idle,
                chi: 0,
                sigma: 0,
            })
            .is_ok(),
            "{label}: slot matter does not pack at {:?}",
            slot.at,
        );
    }
    // Slot zero is the driver on the consumer cell; the docked producer is slot one.
    assert_eq!(workshop.slots[0].at, CONSUMER, "{label}: slot 0 is not the consumer");
    assert_eq!(workshop.slots[0].role, RewriteRole::Driver, "{label}: slot 0 not Driver");
    assert_eq!(workshop.slots[1].at, producer, "{label}: slot 1 is not the producer");
    assert_eq!(workshop.slots[1].role, RewriteRole::Boundary, "{label}: slot 1 not Boundary");
    assert_eq!(workshop.slots[0].parent, None, "{label}: driver has a parent");

    // The patch is connected through face adjacency.
    let mut seen = BTreeSet::from([CONSUMER]);
    let mut queue = VecDeque::from([CONSUMER]);
    while let Some(at) = queue.pop_front() {
        for d in rust_ca_lattice::cell64::DIRS {
            let next = step(at, d);
            if positions.contains(&next) && seen.insert(next) { queue.push_back(next); }
        }
    }
    assert_eq!(seen.len(), workshop.slots.len(), "{label}: patch is disconnected");

    // The request tree is mutually consistent in both directions.
    for index in 0..workshop.slots.len() as u8 {
        let slot = workshop.slot(index).unwrap();
        if let Some(face) = slot.parent {
            let (parent, parent_face) = workshop.parent(index)
                .unwrap_or_else(|| panic!("{label}: parent of slot {index} not found"));
            assert_eq!(parent_face, face, "{label}: parent face mismatch at slot {index}");
            let parent_slot = workshop.slot(parent).unwrap();
            assert_eq!(parent_slot.at, step(slot.at, face));
            assert!(
                parent_slot.children.contains(&face.opp()),
                "{label}: slot {index} missing from its parent's children",
            );
        }
        for &face in &slot.children {
            let child = workshop.child(index, face)
                .unwrap_or_else(|| panic!("{label}: child of slot {index} on {face:?} missing"));
            let child_slot = workshop.slot(child).unwrap();
            assert_eq!(child_slot.at, step(slot.at, face));
            assert_eq!(
                child_slot.parent,
                Some(face.opp()),
                "{label}: child slot {child} does not point back at slot {index}",
            );
        }
    }

    // Fresh seats are distinct.
    let mut fresh: Vec<u8> = workshop.slots.iter().filter_map(|slot| slot.fresh).collect();
    let count = fresh.len();
    fresh.sort();
    fresh.dedup();
    assert_eq!(fresh.len(), count, "{label}: duplicate fresh seat");
}

/// (a) Every rule compiles in both canonical frames and satisfies the structural invariants.
#[test]
fn every_rule_compiles_in_both_canonical_frames() {
    assert_eq!(RULES.len(), 26);
    for index in 0..RULES.len() {
        for lift in [Dir::U, Dir::D] {
            let label = format!("rule {} lift {lift:?}", rule_name(index));
            let workshop = workshop_for(index as u8, Dir::E, Dir::N, lift)
                .unwrap_or_else(|| panic!("{label}: no workshop"));
            check_structure(&label, &workshop, PRODUCER);
            let fresh = workshop.slots.iter().filter_map(|slot| slot.fresh).count();
            assert_eq!(fresh, RULES[index].fresh.len(), "{label}: fresh seat count");
        }
    }
}

/// (b) Stamped into the docked fixture, the patch implements the rule exactly: the packed
/// lattice projects onto the fired shadow net.
#[test]
fn every_workshop_projects_onto_the_fired_shadow() {
    for index in 0..RULES.len() {
        let rule = &RULES[index];
        for lift in [Dir::U, Dir::D] {
            let label = format!("rule {} lift {lift:?}", rule_name(index));
            let workshop = workshop_for(index as u8, Dir::E, Dir::N, lift)
                .unwrap_or_else(|| panic!("{label}: no workshop"));
            let (mut grid, mut shadow, _) =
                docked_fixture64(rule.consumer, rule.producer, Topo::Full3D);
            let patch = canonical_patch(&workshop, Dir::N, lift);
            apply_compiled_patch(&mut grid, &mut shadow, &patch);
            assert_eq!(shadow.ints, 1, "{label}: shadow did not fire exactly once");
            grid.check_projection(&shadow);
        }
    }
}

/// (c) The live request/place protocol drives every workshop to completion under fair
/// activation, and the settled lattice still projects. Fresh producers may lawfully
/// translate along their new principal wires after the placement wave (a projection-
/// identity motion), so settling runs until the lattice is quiet, not for a fixed count.
#[test]
fn live_protocol_places_every_workshop() {
    for index in 0..RULES.len() {
        let rule = &RULES[index];
        let label = format!("rule {}", rule_name(index));
        let (mut grid, mut shadow, _) =
            docked_fixture64(rule.consumer, rule.producer, Topo::Full3D);
        let mut placed = false;
        for round in 0..500 {
            for at in activation_order(&grid, 0, round) {
                activate_with_shadow(&mut grid, &mut shadow, at);
            }
            if grid.rewrites == 1 && !grid.has_protocol() {
                placed = true;
                break;
            }
        }
        assert!(placed, "{label}: workshop never finished placing");
        // Settle: post-placement translations are bounded; the lattice must go quiet.
        let mut quiet_rounds = 0;
        for round in 500..700 {
            sweep_permuted(&mut grid, &mut shadow, 0, round);
            if !grid.has_protocol() {
                quiet_rounds += 1;
                if quiet_rounds == 2 { break; }
            } else {
                quiet_rounds = 0;
            }
        }
        assert_eq!(shadow.ints, 1, "{label}: shadow interaction count");
        assert_eq!(grid.rewrites, 1, "{label}: rewrite count");
        assert!(!grid.has_protocol(), "{label}: protocol roles left behind");
        grid.check_projection(&shadow);
    }
}

// --------------------------------------------------------------------------------------------
// (d) Rotation smoke tests: workshops requested in non-canonical frames are exactly the
// canonical workshop rotated by the frame's signed permutation matrix. The rotation under
// test is re-derived here independently.
// --------------------------------------------------------------------------------------------

fn handedness(axis: Dir, side: Dir, lift: Dir) -> i32 {
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

/// Independent frame rotation: expand in the canonical basis (E, N, canonical lift),
/// recombine in the target basis.
fn test_rotate_vec(v: (i32, i32, i32), axis: Dir, side: Dir, lift: Dir) -> (i32, i32, i32) {
    let cz = if handedness(axis, side, lift) < 0 { 1 } else { -1 };
    let (x, y, z) = (v.0, -v.1, cz * v.2);
    let (a, s, l) = (dir_vec(axis), dir_vec(side), dir_vec(lift));
    (
        x * a.0 + y * s.0 + z * l.0,
        x * a.1 + y * s.1 + z * l.1,
        x * a.2 + y * s.2 + z * l.2,
    )
}

fn test_rotate_dir(d: Dir, axis: Dir, side: Dir, lift: Dir) -> Dir {
    vec_dir(test_rotate_vec(dir_vec(d), axis, side, lift)).expect("face maps to face")
}

fn test_rotate_matter(matter: Matter, axis: Dir, side: Dir, lift: Dir) -> Matter {
    let rd = |d: Dir| test_rotate_dir(d, axis, side, lift);
    match matter {
        Matter::Empty => Matter::Empty,
        Matter::Agent { tag, principal, tail, aux_flip } => Matter::Agent {
            tag,
            principal: rd(principal),
            tail: tail.map(rd),
            aux_flip,
        },
        Matter::Link { ends, lanes, twist, hot, cooldown, pull } => Matter::Link {
            ends: rust_ca_lattice::cell64::FacePair::new(rd(ends.a), rd(ends.b)).unwrap(),
            lanes,
            twist,
            hot,
            cooldown,
            pull,
        },
        Matter::Zip { trunk, branches, twist, hot, cooldown, pull } => Matter::Zip {
            trunk: rd(trunk),
            branches: [rd(branches[0]), rd(branches[1])],
            twist,
            hot,
            cooldown,
            pull,
        },
        Matter::Cross { routes, hot, cooldown, pull } => Matter::Cross {
            routes: routes
                .map(|r| rust_ca_lattice::cell64::FacePair::new(rd(r.a), rd(r.b)).unwrap()),
            hot,
            cooldown,
            pull,
        },
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
            ends: rust_ca_lattice::cell64::FacePair::new(rd(ends.a), rd(ends.b)).unwrap(),
            twist,
        },
    }
}

#[test]
fn rotated_workshops_match_an_independent_rotation() {
    // Rule 2 (A·F) is the hand-crafted workshop: it rebuilds its request tree per
    // orientation, so slot order legitimately differs across frames. It is covered by the
    // structural, projection, and live tests instead.
    for index in [0, 5, 10, 12, 17, 25] {
        for (axis, side, lift) in [
            (Dir::E, Dir::N, Dir::U), // identity on the searched canonical
            (Dir::E, Dir::N, Dir::D), // the mirror canonical itself
            (Dir::E, Dir::S, Dir::U),
            (Dir::W, Dir::N, Dir::D),
        ] {
            let label = format!("rule {} frame {axis:?}{side:?}{lift:?}", rule_name(index));
            let canonical = workshop_for(index as u8, Dir::E, Dir::N, Dir::U).unwrap();
            let mirror_canonical = workshop_for(index as u8, Dir::E, Dir::N, Dir::D).unwrap();
            let rotated = workshop_for(index as u8, axis, side, lift)
                .unwrap_or_else(|| panic!("{label}: no workshop"));
            // The producer cell sits one axis step from the driver in every frame.
            assert_eq!(
                rotated.slots[1].at,
                step(CONSUMER, axis),
                "{label}: producer not on the axis",
            );
            assert_eq!(rotated.slots[0].at, CONSUMER, "{label}: driver moved");
            // Pick the canonical of matching handedness and rotate it independently.
            let source = if handedness(axis, side, lift) < 0 {
                &canonical
            } else {
                &mirror_canonical
            };
            assert_eq!(rotated.slots.len(), source.slots.len(), "{label}: slot count");
            for (got, want) in rotated.slots.iter().zip(source.slots.iter()) {
                assert_eq!(
                    got.at,
                    test_rotate_vec(want.at, axis, side, lift),
                    "{label}: slot position",
                );
                assert_eq!(
                    got.matter,
                    test_rotate_matter(want.matter, axis, side, lift),
                    "{label}: slot matter at {:?}",
                    want.at,
                );
                assert_eq!(
                    got.parent,
                    want.parent.map(|d| test_rotate_dir(d, axis, side, lift)),
                    "{label}: parent face",
                );
                let want_children: Vec<Dir> =
                    want.children.iter().map(|&d| test_rotate_dir(d, axis, side, lift)).collect();
                assert_eq!(got.children, want_children, "{label}: children faces");
                assert_eq!(got.role, want.role, "{label}: role");
                assert_eq!(got.fresh, want.fresh, "{label}: fresh index");
            }
            // The rotated workshop is itself structurally sound (packing, connectivity,
            // request-tree consistency) in its own frame.
            check_structure(&label, &rotated, step(CONSUMER, axis));
        }
    }
}

/// Compile-time and slot-count report (run with --nocapture).
#[test]
fn workshop_compile_report() {
    let start = std::time::Instant::now();
    let mut rows = vec![];
    for index in 0..RULES.len() {
        let workshop = workshop_for(index as u8, Dir::E, Dir::N, Dir::U).unwrap();
        rows.push(format!(
            "rule {:2} {}·{}: {} slots",
            index,
            RULES[index].consumer.name(),
            RULES[index].producer.name(),
            workshop.slots.len(),
        ));
    }
    eprintln!("compile_all wall time: {:?}", start.elapsed());
    for row in rows { eprintln!("{row}"); }
}
