//! The signal plane (AC_IDEA): monotone facts that ride beside matter. Heat is
//! raise-only — a raise can never be undone, only die with its route when the route is
//! rewired — so raises commute and no signal operation ever needs mutual exclusion.
//! Queries may err stale-cold (a raise not yet served) but never stale-hot.
//!
//! Under the push model heat is the least fixpoint of routing plus demand sources (live
//! consumer principals), which licenses two kinds of backend, runtime-selectable like
//! queue disciplines and all reaching the same fixpoint:
//!
//! - `Worklist`: heat lives in the wire words (the chip's hot latches) and creeps one
//!   cell per activation; the runner migrates bits when routes move (epoch death).
//! - `Components` / `Dense`: heat is DERIVED from matter whenever the grid's
//!   `route_epoch` moves — components by union-find over route reciprocity (the model
//!   of the unclocked fabric: a cable heats in one instant, and passthrough routes are
//!   just more component edges, so guest chains are exact), dense by iterating the
//!   local rule to fixpoint (the GPU-shaped recompute). Two independent
//!   implementations of one fixpoint: divergence is a bug, not a mode.
//!
//! The derived recompute is a global, off-fabric scan — the software stand-in for
//! physics that is instantaneous on silicon. It never runs on the fabric's budget, so
//! the locality audit measures fabric ops under `Worklist` only.

use crate::cascade::{Cell, EndPt, Grid2, Route};
use crate::lattice::{step, Pos};
use std::collections::BTreeMap;

/// The route list a cell contributes to the signal fixpoint. Nursery guests are opaque
/// (their passthroughs join no cable until finalize), matching what the walkers see.
fn cell_routes(cell: &Cell) -> Vec<Route> {
    match cell {
        Cell::Wire { routes, .. } => routes.clone(),
        Cell::Agent { pass, nursery: false, .. } => pass.clone(),
        Cell::Seed { pass, .. } => pass.iter().copied().collect(),
        _ => vec![],
    }
}

/// A live consumer's principal endpoint, the demand source.
fn consumer_principal(cell: &Cell) -> Option<EndPt> {
    match cell {
        Cell::Agent { tag, principal, nursery: false, .. } if tag.is_consumer() => {
            Some(*principal)
        }
        _ => None,
    }
}

#[derive(Clone, Debug, Default)]
pub struct DerivedHeat {
    built_at: Option<u64>,
    /// Per-cell hot bitmaps (slot i of that cell's own route list), wire and pass alike.
    hot: BTreeMap<Pos, u8>,
}

#[derive(Clone, Debug, Default)]
pub enum SignalBackend {
    #[default]
    Worklist,
    Components(DerivedHeat),
    Dense(DerivedHeat),
}

impl SignalBackend {
    pub fn worklist() -> Self {
        SignalBackend::Worklist
    }
    pub fn components() -> Self {
        SignalBackend::Components(DerivedHeat::default())
    }
    pub fn dense() -> Self {
        SignalBackend::Dense(DerivedHeat::default())
    }

    /// Bring a derivational backend up to date with the grid's routing structure.
    /// Returns the cells whose heat newly appeared — the runner must wake around each,
    /// because a fresh fixpoint IS the signal arriving (a walker ten cells down a
    /// just-demanded cable learns of it from exactly this wake). Cheap no-op when the
    /// epoch is unchanged, and always a no-op for `Worklist`.
    pub fn sync(&mut self, grid: &Grid2) -> Vec<Pos> {
        let (state, fixpoint): (&mut DerivedHeat, fn(&Grid2) -> BTreeMap<Pos, u8>) = match self {
            SignalBackend::Worklist => return vec![],
            SignalBackend::Components(s) => (s, components_fixpoint),
            SignalBackend::Dense(s) => (s, dense_fixpoint),
        };
        if state.built_at == Some(grid.route_epoch) {
            return vec![];
        }
        let new = fixpoint(grid);
        let mut wakes = vec![];
        for (p, bits) in &new {
            let old = state.hot.get(p).copied().unwrap_or(0);
            if bits & !old != 0 {
                wakes.push(*p);
            }
        }
        state.hot = new;
        state.built_at = Some(grid.route_epoch);
        wakes
    }

    /// May err stale-cold, never stale-hot (derivational backends: fresh as of the last
    /// sync; within one activation's own writes they err on the pre-write topology,
    /// the same stale-safe window every heuristic read already lives with).
    pub fn hot(&self, grid: &Grid2, p: Pos, slot: usize) -> bool {
        match self {
            SignalBackend::Worklist => match grid.site(p).cell {
                Cell::Wire { hot, .. } => (hot >> slot) & 1 == 1,
                _ => false,
            },
            SignalBackend::Components(s) | SignalBackend::Dense(s) => {
                s.hot.get(&p).is_some_and(|b| (b >> slot) & 1 == 1)
            }
        }
    }

    /// Monotone raise; returns the cells the runner must wake around. For `Worklist`
    /// this heats one route slot (epoch death swallows raises on dead routes). For the
    /// derivational backends it is a no-op: heat is a function of matter, every change
    /// of matter bumps `route_epoch`, and sync's diff delivers the wakes.
    pub fn raise(&mut self, grid: &mut Grid2, p: Pos, slot: usize) -> Vec<Pos> {
        match self {
            SignalBackend::Worklist => {
                if !grid.topo.in_bounds(p) {
                    return vec![];
                }
                let mut site = grid.site(p);
                let Cell::Wire { routes, hot, .. } = &mut site.cell else {
                    return vec![];
                };
                if slot >= routes.len() || (*hot >> slot) & 1 == 1 {
                    return vec![];
                }
                *hot |= 1 << slot;
                grid.set(p, &site);
                vec![p]
            }
            SignalBackend::Components(_) | SignalBackend::Dense(_) => vec![],
        }
    }

    /// Whether heat creeps by per-cell extension (the worklist wave). Derivational
    /// backends converge inside sync instead, so their wires skip the pump.
    pub fn extends_by_pump(&self) -> bool {
        matches!(self, SignalBackend::Worklist)
    }

    pub fn name(&self) -> &'static str {
        match self {
            SignalBackend::Worklist => "worklist",
            SignalBackend::Components(_) => "components",
            SignalBackend::Dense(_) => "dense",
        }
    }
}

/// Union-find over route reciprocity: nodes are every route (wire, non-nursery agent
/// pass, seed pass), edges join reciprocal endpoints across a face, and a component is
/// hot iff any member endpoint meets a live consumer's principal.
fn components_fixpoint(grid: &Grid2) -> BTreeMap<Pos, u8> {
    let mut idx: BTreeMap<(Pos, u8), u32> = BTreeMap::new();
    let mut nodes: Vec<(Pos, u8, Route)> = vec![];
    let mut sites: BTreeMap<Pos, Cell> = BTreeMap::new();
    for (p, w) in &grid.cells {
        let site = w.unpack().expect("stored word must be canonical");
        for (i, r) in cell_routes(&site.cell).into_iter().enumerate() {
            idx.insert((*p, i as u8), nodes.len() as u32);
            nodes.push((*p, i as u8, r));
        }
        sites.insert(*p, site.cell);
    }
    let mut parent: Vec<u32> = (0..nodes.len() as u32).collect();
    fn find(parent: &mut [u32], mut x: u32) -> u32 {
        while parent[x as usize] != x {
            parent[x as usize] = parent[parent[x as usize] as usize];
            x = parent[x as usize];
        }
        x
    }
    let mut source = vec![false; nodes.len()];
    for k in 0..nodes.len() {
        let (p, _slot, r) = nodes[k];
        for e in r.ends() {
            let n = step(p, e.face);
            let back = EndPt { face: e.face.opp(), lane: e.lane };
            let Some(ncell) = sites.get(&n) else { continue };
            if consumer_principal(ncell) == Some(back) {
                source[k] = true;
                continue;
            }
            let joined = cell_routes(ncell)
                .iter()
                .position(|r2| r2.ends().contains(&back))
                .and_then(|j| idx.get(&(n, j as u8)).copied());
            if let Some(m) = joined {
                let (a, b) = (find(&mut parent, k as u32), find(&mut parent, m));
                if a != b {
                    parent[a as usize] = b;
                }
            }
        }
    }
    let mut hot_root = vec![false; nodes.len()];
    for k in 0..nodes.len() {
        if source[k] {
            let r = find(&mut parent, k as u32);
            hot_root[r as usize] = true;
        }
    }
    let mut map: BTreeMap<Pos, u8> = BTreeMap::new();
    for k in 0..nodes.len() {
        let (p, slot, _) = nodes[k];
        if hot_root[find(&mut parent, k as u32) as usize] {
            *map.entry(p).or_insert(0) |= 1 << slot;
        }
    }
    map
}

/// The same fixpoint as a breadth-first flood from the demand sources: seed every
/// route whose end meets a live consumer's principal, then spread across reciprocal
/// endpoints. This is the wavefront the GPU lowering runs as dense per-generation
/// passes, collapsed to convergence; it deliberately shares no machinery with the
/// component scan.
fn dense_fixpoint(grid: &Grid2) -> BTreeMap<Pos, u8> {
    let mut routes_at: BTreeMap<Pos, Vec<Route>> = BTreeMap::new();
    let mut consumers: BTreeMap<Pos, EndPt> = BTreeMap::new();
    for (p, w) in &grid.cells {
        let site = w.unpack().expect("stored word must be canonical");
        let rs = cell_routes(&site.cell);
        if !rs.is_empty() {
            routes_at.insert(*p, rs);
        }
        if let Some(pr) = consumer_principal(&site.cell) {
            consumers.insert(*p, pr);
        }
    }
    let mut hot: BTreeMap<Pos, u8> = BTreeMap::new();
    let mut frontier: Vec<(Pos, usize)> = vec![];
    for (p, rs) in &routes_at {
        for (i, r) in rs.iter().enumerate() {
            let sourced = r.ends().iter().any(|e| {
                let n = step(*p, e.face);
                consumers.get(&n) == Some(&EndPt { face: e.face.opp(), lane: e.lane })
            });
            if sourced {
                frontier.push((*p, i));
            }
        }
    }
    while let Some((p, i)) = frontier.pop() {
        let bits = hot.entry(p).or_insert(0);
        if *bits >> i & 1 == 1 {
            continue;
        }
        *bits |= 1 << i;
        let r = routes_at[&p][i];
        for e in r.ends() {
            let n = step(p, e.face);
            let back = EndPt { face: e.face.opp(), lane: e.lane };
            if let Some(nrs) = routes_at.get(&n) {
                for (j, r2) in nrs.iter().enumerate() {
                    if r2.ends().contains(&back) && hot.get(&n).copied().unwrap_or(0) >> j & 1 == 0
                    {
                        frontier.push((n, j));
                    }
                }
            }
        }
    }
    hot
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade::Site;
    use crate::lattice::{Dir, Topo};
    use crate::rules::Tag;

    fn wire(a: (Dir, u8), b: (Dir, u8)) -> Cell {
        Cell::Wire {
            routes: vec![Route::new(
                EndPt { face: a.0, lane: a.1 },
                EndPt { face: b.0, lane: b.1 },
            )],
            hot: 0,
            cooldown: 0,
            reserved: None,
        }
    }
    fn consumer(principal: Dir) -> Cell {
        Cell::Agent {
            tag: Tag::Nrm,
            principal: EndPt { face: principal, lane: 0 },
            aux: [EndPt { face: principal.opp(), lane: 0 }; 2],
            pass: vec![],
            nursery: false,
            cooldown: 0,
        }
    }

    /// consumer(E-facing) | wire | wire | wire: the whole cable heats in ONE sync under
    /// both derivational backends, and rewiring it away goes cold at the next sync.
    #[test]
    fn derivational_cable_heats_instantly_and_dies_with_its_route() {
        for mut backend in [SignalBackend::components(), SignalBackend::dense()] {
            let mut g = Grid2::new(Topo::Full3D);
            g.set((0, 0, 0), &Site::of(consumer(Dir::E)));
            for x in 1..=3 {
                g.set((x, 0, 0), &Site::of(wire((Dir::W, 0), (Dir::E, 0))));
            }
            let wakes = backend.sync(&g);
            assert_eq!(wakes.len(), 3, "{}: whole cable newly hot", backend.name());
            for x in 1..=3 {
                assert!(backend.hot(&g, (x, 0, 0), 0), "{} x={x}", backend.name());
            }
            assert!(backend.sync(&g).is_empty(), "unchanged epoch resyncs nothing");
            // Sever the middle: the far segment is no longer connected to demand.
            g.set((2, 0, 0), &Site::empty());
            let wakes = backend.sync(&g);
            assert!(wakes.is_empty(), "going cold wakes nothing");
            assert!(backend.hot(&g, (1, 0, 0), 0), "near segment still touches demand");
            assert!(!backend.hot(&g, (3, 0, 0), 0), "severed segment died (epoch death)");
        }
    }

    /// consumer | wire | guest(pass) | wire: demand crosses the guest in the SAME sync
    /// (passthroughs are component edges) — the exactness the worklist wave lacks. A
    /// nursery guest stays opaque.
    #[test]
    fn derivational_heat_crosses_guests_but_not_nurseries() {
        for nursery in [false, true] {
            for mut backend in [SignalBackend::components(), SignalBackend::dense()] {
                let mut g = Grid2::new(Topo::Full3D);
                g.set((0, 0, 0), &Site::of(consumer(Dir::E)));
                g.set((1, 0, 0), &Site::of(wire((Dir::W, 0), (Dir::E, 0))));
                g.set(
                    (2, 0, 0),
                    &Site::of(Cell::Agent {
                        tag: Tag::S,
                        principal: EndPt { face: Dir::U, lane: 0 },
                        aux: [EndPt { face: Dir::D, lane: 0 }; 2],
                        pass: vec![Route::new(
                            EndPt { face: Dir::W, lane: 0 },
                            EndPt { face: Dir::E, lane: 0 },
                        )],
                        nursery,
                        cooldown: 0,
                    }),
                );
                g.set((3, 0, 0), &Site::of(wire((Dir::W, 0), (Dir::E, 0))));
                backend.sync(&g);
                assert!(backend.hot(&g, (1, 0, 0), 0), "{}: near wire", backend.name());
                assert_eq!(
                    backend.hot(&g, (3, 0, 0), 0),
                    !nursery,
                    "{}: far wire hot iff the guest is out of the nursery",
                    backend.name()
                );
            }
        }
    }

    /// The two independent fixpoint implementations agree on a randomized soup of
    /// cells (whatever packs is fair game) — divergence is a bug, not a mode.
    #[test]
    fn components_and_dense_agree() {
        use crate::oracle::Lcg;
        let mut rng = Lcg(777);
        for _ in 0..40 {
            let mut g = Grid2::new(Topo::Full3D);
            for _ in 0..24 {
                let p = (
                    (rng.next() * 5.0) as i32,
                    (rng.next() * 3.0) as i32,
                    (rng.next() * 2.0) as i32,
                );
                let cell = match (rng.next() * 4.0) as u32 {
                    0 => consumer([Dir::E, Dir::W, Dir::N][(rng.next() * 3.0) as usize]),
                    1 => wire(
                        ([Dir::W, Dir::N, Dir::U][(rng.next() * 3.0) as usize], 0),
                        ([Dir::E, Dir::S][(rng.next() * 2.0) as usize], (rng.next() * 2.0) as u8),
                    ),
                    _ => wire((Dir::W, (rng.next() * 2.0) as u8), (Dir::E, (rng.next() * 2.0) as u8)),
                };
                if crate::cascade::Word2::pack(&Site::of(cell.clone())).is_ok() {
                    g.set(p, &Site::of(cell));
                }
            }
            assert_eq!(
                components_fixpoint(&g),
                dense_fixpoint(&g),
                "fixpoint implementations diverged"
            );
        }
    }

    /// The worklist platform contract: raises commute, are idempotent, and die with
    /// their routes. (Derivational backends have no raise state at all.)
    #[test]
    fn worklist_raises_commute_and_are_idempotent() {
        let mut backend = SignalBackend::worklist();
        let orders: [&[usize]; 4] = [&[0, 1], &[1, 0], &[1, 1, 0, 0], &[0, 1, 0, 1]];
        let mut states = vec![];
        for order in orders {
            let mut g = Grid2::new(Topo::Full3D);
            let p = (0, 0, 0);
            g.set(
                p,
                &Site::of(Cell::Wire {
                    routes: vec![
                        Route::new(EndPt { face: Dir::W, lane: 0 }, EndPt { face: Dir::E, lane: 0 }),
                        Route::new(EndPt { face: Dir::W, lane: 1 }, EndPt { face: Dir::E, lane: 1 }),
                    ],
                    hot: 0,
                    cooldown: 0,
                    reserved: None,
                }),
            );
            for &slot in order {
                backend.raise(&mut g, p, slot);
            }
            states.push((0..2).map(|s| backend.hot(&g, p, s)).collect::<Vec<_>>());
        }
        for s in &states {
            assert_eq!(s, &states[0], "raise order changed the fixpoint");
            assert_eq!(s, &vec![true, true]);
        }
        // Dead routes swallow raises.
        let mut g = Grid2::new(Topo::Full3D);
        assert!(backend.raise(&mut g, (9, 9, 9), 0).is_empty());
        assert!(!backend.hot(&g, (9, 9, 9), 0));
    }
}
