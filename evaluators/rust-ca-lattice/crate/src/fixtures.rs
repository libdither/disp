//! Closed, hand-laid evaluator fixtures used by trace tooling.
//!
//! The rule atlas is a spatial catalogue of the complete interaction ROM. Every row is
//! an independent active pair whose principal edge contains exactly one wire cell. Each
//! auxiliary port runs outward through one wire cell to its own inert `Out` pad, so all
//! ports are closed without creating another reducible pair. The rows are deliberately
//! far apart: they exercise the ordinary scheduler and fire planner without sharing
//! geometry or workshop space.

use crate::lattice::{AgentCell, Dir, Grid, Pos, Strand, Topo};
use crate::net::Net;
use crate::rules::{Tag, RULES};
use crate::scheduler::{FireMode, Sim, DEFAULT_SCHEDULE_SEED};

/// Distance between atlas rows. The largest ROM template uses six fresh agents; this
/// margin leaves each pair a private bilayer workshop during host-planned materialization.
pub const RULE_ATLAS_ROW_PITCH: i32 = 16;

/// Stable observer metadata for one atlas row. These coordinates describe frame zero;
/// later movement and the fire event remain discoverable through the agents' shadow ids.
#[derive(Clone, Copy, Debug)]
pub struct RuleAtlasEntry {
    pub consumer: Tag,
    pub producer: Tag,
    pub consumer_sid: u32,
    pub producer_sid: u32,
    pub consumer_pos: Pos,
    pub principal_wire_pos: Pos,
    pub producer_pos: Pos,
}

fn agent(tag: Tag, sid: u32, faces: [Option<Dir>; 3]) -> AgentCell {
    AgentCell {
        tag,
        faces,
        sid,
        nascent: false,
        frustration: 0,
    }
}

fn faces(tag: Tag, principal: Dir, aux1: Dir, aux2: Dir) -> [Option<Dir>; 3] {
    let mut out = [None; 3];
    out[0] = Some(principal);
    if tag.arity() > 1 {
        out[1] = Some(aux1);
    }
    if tag.arity() > 2 {
        out[2] = Some(aux2);
    }
    out
}

/// Attach one auxiliary port to a private inert pad through one straight wire cell.
fn terminate_aux(
    shadow: &mut Net,
    grid: &mut Grid,
    owner_sid: u32,
    port: u8,
    wire_pos: Pos,
    pad_pos: Pos,
    owner_face: Dir,
) {
    let pad_sid = shadow.mk(Tag::Out);
    shadow.link(owner_sid, port, pad_sid, 0);
    grid.add_strand(wire_pos, Strand::new(owner_face.opp(), owner_face));
    grid.put_agent(
        pad_pos,
        agent(Tag::Out, pad_sid, [Some(owner_face.opp()), None, None]),
    );
}

/// Construct all 26 ROM interactions as independent spatial rows.
///
/// Row geometry on the compute plane (`C` and `P` are the dying consumer/producer):
///
/// ```text
///                          Out
///                           |
///                          wire
///                           |
/// Out--wire--C--wire--P--wire--Out
///            |    ^
///          wire   exactly one cell
///            |
///           Out
/// ```
///
/// The producer's east auxiliary is the back route for a one-face westward approach;
/// its north auxiliary is the swept-square route for arity-three producers. This makes
/// the initial geometry directly admissible to the normal local star protocol.
pub fn rule_atlas(topo: Topo) -> (Sim, Vec<RuleAtlasEntry>) {
    let mut shadow = Net::new();
    let mut grid = Grid::new(topo);
    let mut entries = Vec::with_capacity(RULES.len());

    for (row, rule) in RULES.iter().enumerate() {
        let y = row as i32 * RULE_ATLAS_ROW_PITCH;
        let consumer_pos = (0, y, 0);
        let principal_wire_pos = (1, y, 0);
        let producer_pos = (2, y, 0);

        let consumer_sid = shadow.mk(rule.consumer);
        let producer_sid = shadow.mk(rule.producer);
        shadow.link(consumer_sid, 0, producer_sid, 0);

        grid.put_agent(
            consumer_pos,
            agent(
                rule.consumer,
                consumer_sid,
                faces(rule.consumer, Dir::E, Dir::W, Dir::S),
            ),
        );
        grid.add_strand(principal_wire_pos, Strand::new(Dir::W, Dir::E));
        grid.put_agent(
            producer_pos,
            agent(
                rule.producer,
                producer_sid,
                faces(rule.producer, Dir::W, Dir::E, Dir::N),
            ),
        );

        if rule.consumer.arity() > 1 {
            terminate_aux(
                &mut shadow,
                &mut grid,
                consumer_sid,
                1,
                (-1, y, 0),
                (-2, y, 0),
                Dir::W,
            );
        }
        if rule.consumer.arity() > 2 {
            terminate_aux(
                &mut shadow,
                &mut grid,
                consumer_sid,
                2,
                (0, y + 1, 0),
                (0, y + 2, 0),
                Dir::S,
            );
        }
        if rule.producer.arity() > 1 {
            terminate_aux(
                &mut shadow,
                &mut grid,
                producer_sid,
                1,
                (3, y, 0),
                (4, y, 0),
                Dir::E,
            );
        }
        if rule.producer.arity() > 2 {
            terminate_aux(
                &mut shadow,
                &mut grid,
                producer_sid,
                2,
                (2, y - 1, 0),
                (2, y - 2, 0),
                Dir::N,
            );
        }

        entries.push(RuleAtlasEntry {
            consumer: rule.consumer,
            producer: rule.producer,
            consumer_sid,
            producer_sid,
            consumer_pos,
            principal_wire_pos,
            producer_pos,
        });
    }

    grid.check_projection(&shadow);
    assert_eq!(shadow.all_active_pairs().len(), RULES.len());

    let sim = Sim {
        grid,
        shadow,
        events: vec![],
        fire_mode: FireMode::Search,
        schedule_seed: DEFAULT_SCHEDULE_SEED,
        schedule_tick: 0,
    };
    (sim, entries)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::{CheckLevel, Event};
    use std::collections::BTreeSet;

    #[test]
    fn atlas_has_one_principal_wire_cell_per_rule() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            let (sim, entries) = rule_atlas(topo);
            assert_eq!(entries.len(), 26);
            for entry in entries {
                assert_eq!(
                    sim.grid.try_trace(entry.consumer_pos, Dir::E),
                    Some((entry.producer_pos, 0)),
                );
                assert_eq!(sim.grid.wire(entry.principal_wire_pos).unwrap().count(), 1);
            }
        }
    }

    #[test]
    fn atlas_fires_every_rom_rule_through_the_scheduler() {
        let (mut sim, _) = rule_atlas(Topo::Bilayer);
        let want: BTreeSet<_> = RULES
            .iter()
            .map(|rule| (rule.consumer.name(), rule.producer.name()))
            .collect();
        let mut got = BTreeSet::new();

        for _ in 0..80 {
            sim.tick(CheckLevel::Tick);
            for event in &sim.events {
                if let Event::Fire { rule, .. } = event {
                    got.insert(*rule);
                }
            }
            if got == want {
                break;
            }
        }

        assert_eq!(got, want);
    }
}
