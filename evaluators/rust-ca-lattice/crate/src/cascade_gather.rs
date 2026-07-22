//! The gather driver: the GPU/shader lowering of the cascade movement layer. Time is a
//! repeating six-phase domino schedule: phase (axis, parity) partitions the lattice into
//! disjoint adjacent pairs along that axis, and every pair (plus every single cell, for
//! the heat wave) is rewritten by one pure function of its own words. No claim bits, no
//! queues, no races: within a phase all writes go to a double buffer, exactly the shape
//! of a compute-shader dispatch per phase.
//!
//! The same pair-decision function as the threaded driver ([`crate::cascade_par::decide`])
//! runs here, so the two lowerings cannot drift.

use crate::cascade::{Grid2, Word2};
use crate::cascade_par::decide;
use crate::lattice::Dir;
use crate::lattice::{step, Pos, Topo};
use std::collections::BTreeMap;

pub struct GatherGrid {
    pub cells: BTreeMap<Pos, Word2>,
    pub topo: Topo,
    pub fires: u64,
    pub moves: u64,
}

const PHASES: [Dir; 3] = [Dir::E, Dir::S, Dir::U];

impl GatherGrid {
    pub fn from_grid(grid: &Grid2) -> Self {
        Self { cells: grid.cells.clone(), topo: grid.topo, fires: 0, moves: 0 }
    }

    pub fn to_grid(&self) -> Grid2 {
        let mut g = Grid2::new(self.topo);
        g.cells = self.cells.clone();
        g
    }

    fn read(&self, p: Pos) -> Word2 {
        if self.topo.in_bounds(p) {
            self.cells.get(&p).copied().unwrap_or(Word2::EMPTY)
        } else {
            Word2::pack(&crate::cascade::Site {
                cell: crate::cascade::Cell::Empty { reserved: Some(Dir::U) },
                cursor: None,
                chi: 0,
                claim: true,
            })
            .expect("boundary word")
        }
    }

    /// One phase: every domino (initiator at parity `par` along `axis`, partner one step
    /// along `axis`) evaluates the pair transition; every cell evaluates the single-cell
    /// heat transition. All reads come from the front buffer; the phase is one gather.
    pub fn phase(&mut self, axis: Dir, par: i32) -> usize {
        let read = |q: Pos| self.read(q);
        let mut writes: Vec<(Pos, Word2)> = vec![];
        let mut fires = 0u64;
        let mut moves = 0u64;
        let mut changed = 0usize;
        for (p, w) in &self.cells {
            let along = match axis {
                Dir::E => p.0,
                Dir::S => p.1,
                _ => p.2,
            };
            let is_initiator = along.rem_euclid(2) == par;
            // Pair transition for initiators whose partner lies one step along the axis.
            let t = step(*p, axis);
            let frozen = if is_initiator {
                vec![(*p, *w), (t, self.read(t))]
            } else {
                vec![(*p, *w)]
            };
            if let Some(tx) = decide(&read, *p, &frozen) {
                // The pair transition only fits this phase when its writes stay inside
                // the domino; a walker facing another axis simply waits for its phase.
                let inside = tx.writes.iter().all(|(q, _)| *q == *p || (is_initiator && *q == t));
                if inside {
                    if tx.fired {
                        fires += 1;
                    }
                    if tx.moved {
                        moves += 1;
                    }
                    writes.extend(tx.writes);
                }
            }
        }
        for (p, w) in writes {
            changed += 1;
            if w == Word2::EMPTY {
                self.cells.remove(&p);
            } else {
                self.cells.insert(p, w);
            }
        }
        self.fires += fires;
        self.moves += moves;
        changed
    }

    /// Run full six-phase rounds until a whole round changes nothing.
    pub fn run(&mut self, max_rounds: usize) -> bool {
        for _ in 0..max_rounds {
            let mut changed = 0;
            for axis in PHASES {
                for par in 0..2 {
                    changed += self.phase(axis, par);
                }
            }
            if changed == 0 {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade_run::{check_reciprocity, lay_wire, place_agent};
    use crate::net::Net;
    use crate::rules::Tag;

    /// The deterministic domino schedule completes the erasure lanes and agrees with
    /// itself run to run (pure function of the initial state).
    #[test]
    fn gather_erasure_lanes_deterministic() {
        let build = || {
            let mut grid = Grid2::new(Topo::Full3D);
            let mut shadow = Net::new();
            for lane in 0..16 {
                let y = lane * 3;
                let l = place_agent(&mut grid, &mut shadow, (0, y, 0), Tag::L, Dir::E, None);
                let e = place_agent(&mut grid, &mut shadow, (11, y, 0), Tag::Eps, Dir::W, None);
                shadow.link(l.id, 0, e.id, 0);
                lay_wire(&mut grid, &(0..=11).map(|x| (x, y, 0)).collect::<Vec<_>>());
            }
            grid
        };
        let mut a = GatherGrid::from_grid(&build());
        assert!(a.run(10_000), "must quiesce");
        assert_eq!(a.fires, 16, "all lanes erase");
        assert_eq!(a.to_grid().agents().count(), 0);
        check_reciprocity(&a.to_grid()).unwrap();

        let mut b = GatherGrid::from_grid(&build());
        assert!(b.run(10_000));
        assert_eq!(a.cells, b.cells, "gather runs are bit-identical");
    }

    /// Bent wires exercise phase changes: the walker advances on the phase matching each
    /// leg of its route.
    #[test]
    fn gather_walks_bends() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let l = place_agent(&mut grid, &mut shadow, (0, 0, 0), Tag::L, Dir::E, None);
        let e = place_agent(&mut grid, &mut shadow, (4, 3, 0), Tag::Eps, Dir::N, None);
        shadow.link(l.id, 0, e.id, 0);
        lay_wire(&mut grid, &[
            (0, 0, 0), (1, 0, 0), (2, 0, 0), (3, 0, 0), (4, 0, 0), (4, 1, 0), (4, 2, 0), (4, 3, 0),
        ]);
        let mut g = GatherGrid::from_grid(&grid);
        assert!(g.run(10_000));
        assert_eq!(g.fires, 1);
        assert_eq!(g.to_grid().agents().count(), 0);
    }
}
