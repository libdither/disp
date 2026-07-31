//! The signal plane (AC_IDEA): monotone facts that ride beside matter. Heat is
//! raise-only — a raise can never be undone, only die with its route when the route is
//! rewired — so raises commute and no signal operation ever needs mutual exclusion.
//! Queries may err stale-cold (a raise not yet served) but never stale-hot.
//!
//! Backends are runtime-selectable like queue disciplines: all must reach the same
//! fixpoint, differing only in where signal state lives and how fast it converges.
//! `Worklist` stores heat in the wire words themselves (the chip's hot latches); the
//! runner migrates bits when routes move, which is that backend's epoch death. The
//! union-find (cable components) and dense-bitmap (per-generation recompute) backends
//! land next; components make demand exact through guest chains, where the in-word
//! backend relays one guest per generation.

use crate::cascade::{Cell, Grid2};
use crate::lattice::Pos;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum SignalBackend {
    #[default]
    Worklist,
}

impl SignalBackend {
    /// May err stale-cold, never stale-hot.
    pub fn hot(&self, grid: &Grid2, p: Pos, slot: usize) -> bool {
        match self {
            SignalBackend::Worklist => match grid.site(p).cell {
                Cell::Wire { hot, .. } => (hot >> slot) & 1 == 1,
                _ => false,
            },
        }
    }

    /// Monotone raise. Returns true when the slot was newly heated (the caller wakes
    /// the neighborhood then: a fresh raise IS the propagation edge). Raising a
    /// non-wire or out-of-range slot is a no-op: the route died before the raise
    /// arrived, and its signal dies with it.
    pub fn raise(&self, grid: &mut Grid2, p: Pos, slot: usize) -> bool {
        match self {
            SignalBackend::Worklist => {
                if !grid.topo.in_bounds(p) {
                    return false;
                }
                let mut site = grid.site(p);
                let Cell::Wire { routes, hot, .. } = &mut site.cell else {
                    return false;
                };
                if slot >= routes.len() || (*hot >> slot) & 1 == 1 {
                    return false;
                }
                *hot |= 1 << slot;
                grid.set(p, &site);
                true
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade::{EndPt, Route, Site};
    use crate::lattice::{Dir, Topo};

    /// A three-route wire cell fixture.
    fn wire_grid() -> (Grid2, Pos) {
        let mut g = Grid2::new(Topo::Full3D);
        let p = (0, 0, 0);
        let r0 = Route::new(EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::E, lane: 0 });
        let r1 = Route::new(EndPt { face: Dir::W, lane: 1 }, EndPt { face: Dir::E, lane: 1 });
        let r2 = Route::new(EndPt { face: Dir::N, lane: 0 }, EndPt { face: Dir::S, lane: 0 });
        g.set(p, &Site::of(Cell::Wire { routes: vec![r0, r1, r2], hot: 0, cooldown: 0, reserved: None }));
        (g, p)
    }

    /// The platform contract: raises commute and are idempotent — ANY order and ANY
    /// duplication of the same raise set reaches the identical signal state. This is
    /// what licenses running raises without mutual exclusion on every platform, and
    /// every future backend must pass it unchanged.
    #[test]
    fn raises_commute_and_are_idempotent() {
        let backend = SignalBackend::Worklist;
        let orders: [&[usize]; 6] =
            [&[0, 1, 2], &[2, 1, 0], &[1, 0, 2, 1, 0], &[2, 2, 2, 0, 1], &[0, 0, 1, 2, 2, 1], &[1, 2, 0]];
        let mut states = vec![];
        for order in orders {
            let (mut g, p) = wire_grid();
            for &slot in order {
                backend.raise(&mut g, p, slot);
            }
            states.push((0..3).map(|s| backend.hot(&g, p, s)).collect::<Vec<_>>());
        }
        for s in &states {
            assert_eq!(s, &states[0], "raise order changed the fixpoint");
            assert_eq!(s, &vec![true, true, true]);
        }
    }

    #[test]
    fn raise_reports_freshness_once() {
        let backend = SignalBackend::Worklist;
        let (mut g, p) = wire_grid();
        assert!(backend.raise(&mut g, p, 1), "first raise is fresh");
        assert!(!backend.raise(&mut g, p, 1), "second raise is a no-op");
        assert!(backend.hot(&g, p, 1));
        assert!(!backend.hot(&g, p, 0));
    }

    /// Raising a dead or never-alive route is a silent no-op (epoch death), and a
    /// query on it errs cold.
    #[test]
    fn dead_routes_swallow_raises() {
        let backend = SignalBackend::Worklist;
        let (mut g, p) = wire_grid();
        assert!(!backend.raise(&mut g, p, 3), "out-of-range slot");
        assert!(!backend.raise(&mut g, (5, 5, 5), 0), "empty cell");
        assert!(!backend.hot(&g, (5, 5, 5), 0));
    }
}
