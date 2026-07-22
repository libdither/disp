//! The parallel cascade driver: N threads edit one shared backing array of `AtomicU64`
//! cell words. Mutual exclusion is the word's claim bit, acquired by compare-and-swap in
//! address order over a transition's write set, so claims are deadlock-free and contention
//! exists only where two wake fronts touch the same cells: two cascades meeting.
//!
//! Scope: the high-frequency transitions run fully parallel (heat waves, walks including
//! truncation, swaps, and fusion docks whose write set is the pair). Blocklet growth is
//! rare by comparison and stays on the serial runner; `run_movement` is the parallel
//! phase a full evaluator would interleave between fires.

use crate::cascade::{Cell, EndPt, Route, Site, Word2};
use crate::lattice::DIRS;
use crate::lattice::{step, Pos, Topo};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Mutex;

/// Dense atomic backing array over a bounding box. Out-of-box reads act as boundary.
pub struct AtomicGrid {
    words: Vec<AtomicU64>,
    lo: Pos,
    dims: (i32, i32, i32),
    pub topo: Topo,
}

impl AtomicGrid {
    /// Build from a serial grid, with margin for walks and trails.
    pub fn from_grid(grid: &crate::cascade::Grid2, margin: i32) -> Self {
        let mut lo = (i32::MAX, i32::MAX, i32::MAX);
        let mut hi = (i32::MIN, i32::MIN, i32::MIN);
        for p in grid.cells.keys() {
            lo = (lo.0.min(p.0), lo.1.min(p.1), lo.2.min(p.2));
            hi = (hi.0.max(p.0), hi.1.max(p.1), hi.2.max(p.2));
        }
        let lo = (lo.0 - margin, lo.1 - margin, lo.2 - margin);
        let hi = (hi.0 + margin, hi.1 + margin, hi.2 + margin);
        let dims = (hi.0 - lo.0 + 1, hi.1 - lo.1 + 1, hi.2 - lo.2 + 1);
        let n = (dims.0 as usize) * (dims.1 as usize) * (dims.2 as usize);
        let words: Vec<AtomicU64> = (0..n).map(|_| AtomicU64::new(0)).collect();
        let this = Self { words, lo, dims, topo: grid.topo };
        for (p, w) in &grid.cells {
            this.words[this.index(*p).unwrap()].store(w.0, Ordering::Relaxed);
        }
        this
    }

    pub fn index(&self, p: Pos) -> Option<usize> {
        let (x, y, z) = (p.0 - self.lo.0, p.1 - self.lo.1, p.2 - self.lo.2);
        if x < 0 || y < 0 || z < 0 || x >= self.dims.0 || y >= self.dims.1 || z >= self.dims.2 {
            return None;
        }
        Some(((z * self.dims.1 + y) * self.dims.0 + x) as usize)
    }

    pub fn in_bounds(&self, p: Pos) -> bool {
        self.topo.in_bounds(p) && self.index(p).is_some()
    }

    pub fn load(&self, p: Pos) -> Word2 {
        match self.index(p) {
            Some(i) if self.topo.in_bounds(p) => Word2(self.words[i].load(Ordering::Acquire)),
            // Outside: a permanently claimed boundary word nothing can enter or claim.
            _ => Word2::pack(&Site {
                cell: Cell::Empty { reserved: Some(crate::lattice::Dir::U) },
                cursor: None,
                chi: 0,
                claim: true,
            })
            .expect("boundary word"),
        }
    }

    /// Freeze one cell: CAS the claim bit onto the exact unclaimed word we read.
    fn claim(&self, p: Pos) -> Option<Word2> {
        let i = self.index(p)?;
        let w = Word2(self.words[i].load(Ordering::Acquire));
        if w.claimed() {
            return None;
        }
        self.words[i]
            .compare_exchange(w.0, w.with_claim(true).0, Ordering::AcqRel, Ordering::Acquire)
            .ok()
            .map(|_| w)
    }

    /// Publish a claimed cell's new word (claim cleared).
    fn publish(&self, p: Pos, w: Word2) {
        let i = self.index(p).expect("published cell in bounds");
        debug_assert!(!w.claimed());
        self.words[i].store(w.0, Ordering::Release);
    }

    /// Copy back into a serial grid for checking.
    pub fn to_grid(&self) -> crate::cascade::Grid2 {
        let mut g = crate::cascade::Grid2::new(self.topo);
        for z in 0..self.dims.2 {
            for y in 0..self.dims.1 {
                for x in 0..self.dims.0 {
                    let p = (self.lo.0 + x, self.lo.1 + y, self.lo.2 + z);
                    let w = Word2(self.words[self.index(p).unwrap()].load(Ordering::Relaxed));
                    if w != Word2::EMPTY {
                        g.cells.insert(p, w);
                    }
                }
            }
        }
        g
    }
}

pub struct ParStats {
    /// Committed transitions.
    pub commits: AtomicUsize,
    /// Claim attempts that lost to another thread: exactly "two cascades meeting".
    pub conflicts: AtomicUsize,
    /// Fires committed (fusion docks).
    pub fires: AtomicUsize,
    /// Completed moves.
    pub moves: AtomicUsize,
}

impl ParStats {
    fn new() -> Self {
        Self {
            commits: AtomicUsize::new(0),
            conflicts: AtomicUsize::new(0),
            fires: AtomicUsize::new(0),
            moves: AtomicUsize::new(0),
        }
    }
}

/// One planned transition: the frozen read set (claimed words) and the replacement words.
pub(crate) struct Tx {
    pub(crate) writes: Vec<(Pos, Word2)>,
    pub(crate) wake: Vec<Pos>,
    pub(crate) fired: bool,
    pub(crate) moved: bool,
}

/// Decide a transition for the cell at `p` given its frozen word and free (unclaimed,
/// racy) neighbor reads. Returns the ADDITIONAL cells that must be claimed; the final
/// decision re-runs on the frozen set.
fn plan(grid: &AtomicGrid, p: Pos, w: Word2) -> Option<Vec<Pos>> {
    let site = w.unpack().ok()?;
    match &site.cell {
        Cell::Wire { .. } => Some(vec![]), // heat: single-cell
        Cell::Agent { tag, principal, nursery: false, cooldown: 0, .. }
            if tag.is_producer() =>
        {
            let _ = grid;
            Some(vec![step(p, principal.face)])
        }
        _ => None,
    }
}

/// The frozen-set transition: given claimed words for `p` and (optionally) its partner,
/// produce the replacement words. This mirrors the serial rules for heat, walk
/// (straight/truncate), swap, and fusion docks (rules with no blocklet script).
pub(crate) fn decide(read: &dyn Fn(Pos) -> Word2, p: Pos, frozen: &[(Pos, Word2)]) -> Option<Tx> {
    let word_of = |q: Pos| frozen.iter().find(|(fp, _)| *fp == q).map(|(_, w)| *w);
    let me = word_of(p)?;
    let site = me.unpack().ok()?;
    match &site.cell {
        Cell::Wire { routes, hot, cooldown, reserved } => {
            // Heat: racy neighbor reads are fine (monotone hint).
            let mut new_hot = *hot;
            for (i, r) in routes.iter().enumerate() {
                if (new_hot >> i) & 1 == 1 {
                    continue;
                }
                for e in r.ends() {
                    let n = step(p, e.face);
                    let back = EndPt { face: e.face.opp(), lane: e.lane };
                    let heats = match read(n).unpack() {
                        Ok(ns) => match &ns.cell {
                            Cell::Agent { tag, principal, nursery: false, .. } => {
                                tag.is_consumer() && *principal == back
                            }
                            Cell::Wire { routes: nr, hot: nh, .. } => nr
                                .iter()
                                .enumerate()
                                .any(|(j, r2)| r2.ends().contains(&back) && (nh >> j) & 1 == 1),
                            _ => false,
                        },
                        Err(_) => false,
                    };
                    if heats {
                        new_hot |= 1 << i;
                        break;
                    }
                }
            }
            if new_hot == *hot {
                return None;
            }
            let cell = Cell::Wire {
                routes: routes.clone(),
                hot: new_hot,
                cooldown: *cooldown,
                reserved: *reserved,
            };
            let out = Word2::pack(&Site { cell, cursor: site.cursor, chi: site.chi, claim: false })
                .ok()?;
            Some(Tx {
                writes: vec![(p, out)],
                wake: wake_set(&[p]),
                fired: false,
                moved: false,
            })
        }
        Cell::Agent { tag, principal, aux, pass, nursery: false, cooldown: 0 }
            if tag.is_producer() =>
        {
            let m = principal.face;
            let t = step(p, m);
            let tw = word_of(t)?;
            let target = tw.unpack().ok()?;
            // Fusion dock: consumer principal facing ours, rule with no blocklet.
            if let Cell::Agent {
                tag: ctag,
                principal: cpr,
                aux: caux,
                pass: cpass,
                nursery: false,
                ..
            } = &target.cell
            {
                if ctag.is_consumer()
                    && cpr.face == m.opp()
                    && cpr.lane == principal.lane
                    && target.cursor.is_none()
                {
                    let rule = crate::rules::find_index(*ctag, *tag)? as u8;
                    if !crate::blocklet::layout(rule).script.is_empty()
                        || pass.len() > 1
                        || cpass.len() > 1
                    {
                        return None; // growth stays on the serial runner
                    }
                    let seed_c = Cell::Seed {
                        rule,
                        half: crate::cascade::Half::Consumer,
                        partner: m.opp(),
                        roll: 0,
                        stub: *caux,
                        plane: principal.lane,
                        pass: cpass.first().copied(),
                    };
                    let seed_p = Cell::Seed {
                        rule,
                        half: crate::cascade::Half::Producer,
                        partner: m,
                        roll: 0,
                        stub: *aux,
                        plane: principal.lane,
                        pass: pass.first().copied(),
                    };
                    let (fc, fp) =
                        crate::blocklet::seed_finals(rule, m.opp(), 0, &seed_c, &seed_p);
                    let cw = Word2::pack(&Site {
                        cell: fc,
                        cursor: target.cursor,
                        chi: target.chi,
                        claim: false,
                    })
                    .ok()?;
                    let pw = Word2::pack(&Site {
                        cell: fp,
                        cursor: site.cursor,
                        chi: site.chi,
                        claim: false,
                    })
                    .ok()?;
                    return Some(Tx {
                        writes: vec![(t, cw), (p, pw)],
                        wake: wake_set(&[p, t]),
                        fired: true,
                        moved: false,
                    });
                }
            }
            // Swap.
            if let Cell::Agent {
                tag: btag,
                principal: bpr,
                aux: baux,
                pass: bpass,
                nursery: false,
                cooldown: 0,
            } = &target.cell
            {
                if btag.is_producer() && bpr.face == m.opp() && bpr.lane != principal.lane {
                    let a_enter = EndPt { face: m.opp(), lane: principal.lane };
                    let b_enter = EndPt { face: m, lane: bpr.lane };
                    let ar = bpass.iter().position(|r| r.through(a_enter).is_some());
                    let br = pass.iter().position(|r| r.through(b_enter).is_some());
                    if let (Some(ar), Some(br)) = (ar, br) {
                        let a_exit = bpass[ar].through(a_enter).unwrap();
                        let b_exit = pass[br].through(b_enter).unwrap();
                        let trail = |aux: &[EndPt; 2], arity: usize, toward: crate::lattice::Dir| {
                            let mut routes = vec![];
                            let mut na = [EndPt { face: toward.opp(), lane: 0 }; 2];
                            for k in 0..arity.saturating_sub(1) {
                                routes.push(Route::new(
                                    aux[k],
                                    EndPt { face: toward, lane: k as u8 },
                                ));
                                na[k] = EndPt { face: toward.opp(), lane: k as u8 };
                            }
                            if arity == 2 {
                                na[1] = na[0];
                            }
                            (routes, na)
                        };
                        let (atr, ana) = trail(aux, tag.arity(), m);
                        let (btr, bna) = trail(baux, btag.arity(), m.opp());
                        let mut a_cell: Vec<Route> = pass.clone();
                        a_cell.remove(br);
                        a_cell.extend(atr);
                        let mut b_cell: Vec<Route> = bpass.clone();
                        b_cell.remove(ar);
                        b_cell.extend(btr);
                        if a_cell.len() <= 2 && b_cell.len() <= 2 {
                            let moved_a = Cell::Agent {
                                tag: *tag,
                                principal: a_exit,
                                aux: ana,
                                pass: b_cell,
                                nursery: false,
                                cooldown: 0,
                            };
                            let moved_b = Cell::Agent {
                                tag: *btag,
                                principal: b_exit,
                                aux: bna,
                                pass: a_cell,
                                nursery: false,
                                cooldown: 0,
                            };
                            let pw = Word2::pack(&Site {
                                cell: moved_b,
                                cursor: site.cursor,
                                chi: site.chi,
                                claim: false,
                            });
                            let tw2 = Word2::pack(&Site {
                                cell: moved_a,
                                cursor: target.cursor,
                                chi: target.chi,
                                claim: false,
                            });
                            if let (Ok(pw), Ok(tw2)) = (pw, tw2) {
                                return Some(Tx {
                                    writes: vec![(p, pw), (t, tw2)],
                                    wake: wake_set(&[p, t]),
                                    fired: false,
                                    moved: true,
                                });
                            }
                        }
                    }
                }
                return None;
            }
            // Walk (straight lanes and truncation; detours stay serial).
            let Cell::Wire { routes, hot: whot, reserved: None, .. } = &target.cell else {
                return None;
            };
            if target.cursor.is_some() {
                return None;
            }
            let enter = EndPt { face: m.opp(), lane: principal.lane };
            let my_index = routes.iter().position(|r| r.through(enter).is_some())?;
            let exit = routes[my_index].through(enter).unwrap();
            if exit.face == enter.face || (whot >> my_index) & 1 == 0 {
                return None;
            }
            let mut foreign: Vec<Route> = routes
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != my_index)
                .map(|(_, r)| *r)
                .collect();
            if foreign.len() > 2 {
                return None;
            }
            let need = tag.arity().saturating_sub(1);
            let taken = |rs: &[Route], e: EndPt| rs.iter().any(|r| r.ends().contains(&e));
            let mut vac_routes = pass.clone();
            let mut new_aux = [EndPt { face: m.opp(), lane: 0 }; 2];
            let mut lanes: Vec<u8> = vec![];
            for k in 0..need {
                if aux[k].face == m {
                    let back = EndPt { face: m.opp(), lane: aux[k].lane };
                    if let Some(fi) = foreign.iter().position(|r| r.through(back).is_some()) {
                        new_aux[k] = foreign[fi].through(back).unwrap();
                        foreign.remove(fi);
                        continue;
                    }
                }
                let lane = (0..2u8).find(|l| {
                    !lanes.contains(l)
                        && !taken(pass, EndPt { face: m, lane: *l })
                        && !taken(&foreign, EndPt { face: m.opp(), lane: *l })
                })?;
                lanes.push(lane);
                vac_routes.push(Route::new(aux[k], EndPt { face: m, lane }));
                new_aux[k] = EndPt { face: m.opp(), lane };
            }
            if need == 1 {
                new_aux[1] = new_aux[0];
            }
            if vac_routes.len() > 3 {
                return None;
            }
            let moved = Cell::Agent {
                tag: *tag,
                principal: exit,
                aux: new_aux,
                pass: foreign,
                nursery: false,
                cooldown: 0,
            };
            let vacated = if vac_routes.is_empty() {
                Cell::Empty { reserved: None }
            } else {
                Cell::Wire { routes: vac_routes, hot: 0, cooldown: 0, reserved: None }
            };
            let tw2 = Word2::pack(&Site {
                cell: moved,
                cursor: target.cursor,
                chi: target.chi,
                claim: false,
            })
            .ok()?;
            let pw = Word2::pack(&Site {
                cell: vacated,
                cursor: site.cursor,
                chi: site.chi,
                claim: false,
            })
            .ok()?;
            Some(Tx {
                writes: vec![(t, tw2), (p, pw)],
                wake: wake_set(&[p, t]),
                fired: false,
                moved: true,
            })
        }
        _ => None,
    }
}

fn wake_set(cells: &[Pos]) -> Vec<Pos> {
    let mut out = vec![];
    for c in cells {
        out.push(*c);
        for d in DIRS {
            out.push(step(*c, d));
        }
    }
    out
}

/// Run the movement layer to quiescence with `threads` workers. Returns statistics.
pub fn run_movement(grid: &AtomicGrid, seeds: Vec<Pos>, threads: usize) -> ParStats {
    let stats = ParStats::new();
    let queues: Vec<Mutex<VecDeque<Pos>>> =
        (0..threads).map(|_| Mutex::new(VecDeque::new())).collect();
    // Live work counter: incremented on push, decremented after processing.
    let pending = AtomicUsize::new(0);
    let shard = |p: Pos| {
        let h = (p.0 as i64 * 73856093 ^ p.1 as i64 * 19349663 ^ p.2 as i64 * 83492791) as usize;
        h % queues.len()
    };
    for p in seeds {
        pending.fetch_add(1, Ordering::Relaxed);
        queues[shard(p)].lock().unwrap().push_back(p);
    }
    std::thread::scope(|scope| {
        for me in 0..threads {
            let queues = &queues;
            let pending = &pending;
            let stats = &stats;
            scope.spawn(move || {
                let mut backoff = 0u32;
                loop {
                    // Pop from my shard, then steal.
                    let mut item = queues[me].lock().unwrap().pop_front();
                    if item.is_none() {
                        for other in 0..queues.len() {
                            if other == me {
                                continue;
                            }
                            item = queues[other].lock().unwrap().pop_front();
                            if item.is_some() {
                                break;
                            }
                        }
                    }
                    let Some(p) = item else {
                        if pending.load(Ordering::Acquire) == 0 {
                            return;
                        }
                        backoff = (backoff + 1).min(6);
                        std::thread::yield_now();
                        for _ in 0..(1 << backoff) {
                            std::hint::spin_loop();
                        }
                        continue;
                    };
                    backoff = 0;
                    // Optimistic plan on a racy read, then claim, then decide on the
                    // frozen words.
                    'attempt: {
                        let w = grid.load(p);
                        if w.claimed() || w == Word2::EMPTY {
                            break 'attempt;
                        }
                        let Some(partners) = plan(grid, p, w) else { break 'attempt };
                        let mut set: Vec<Pos> = vec![p];
                        set.extend(partners);
                        set.sort();
                        set.dedup();
                        set.retain(|q| grid.in_bounds(*q));
                        // Address-ordered claims.
                        let mut frozen: Vec<(Pos, Word2)> = vec![];
                        for q in &set {
                            match grid.claim(*q) {
                                Some(wq) => frozen.push((*q, wq)),
                                None => {
                                    stats.conflicts.fetch_add(1, Ordering::Relaxed);
                                    for (fq, fw) in &frozen {
                                        grid.publish(*fq, *fw);
                                    }
                                    // Requeue and retry later.
                                    pending.fetch_add(1, Ordering::Relaxed);
                                    queues[shard(p)].lock().unwrap().push_back(p);
                                    break 'attempt;
                                }
                            }
                        }
                        match decide(&(|q| grid.load(q)), p, &frozen) {
                            Some(tx) => {
                                for (q, nw) in &tx.writes {
                                    grid.publish(*q, *nw);
                                }
                                // Release any claimed cell not written.
                                for (fq, fw) in &frozen {
                                    if !tx.writes.iter().any(|(wq, _)| wq == fq) {
                                        grid.publish(*fq, *fw);
                                    }
                                }
                                stats.commits.fetch_add(1, Ordering::Relaxed);
                                if tx.fired {
                                    stats.fires.fetch_add(1, Ordering::Relaxed);
                                }
                                if tx.moved {
                                    stats.moves.fetch_add(1, Ordering::Relaxed);
                                }
                                for wq in tx.wake {
                                    if grid.in_bounds(wq) {
                                        pending.fetch_add(1, Ordering::Relaxed);
                                        queues[shard(wq)].lock().unwrap().push_back(wq);
                                    }
                                }
                            }
                            None => {
                                for (fq, fw) in &frozen {
                                    grid.publish(*fq, *fw);
                                }
                            }
                        }
                    }
                    pending.fetch_sub(1, Ordering::AcqRel);
                }
            });
        }
    });
    stats
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cascade::Grid2;
    use crate::cascade_run::{check_projection, check_reciprocity, lay_wire, place_agent};
    use crate::net::Net;
    use crate::rules::Tag;
    use crate::lattice::Dir;

    /// Many disjoint erasure lanes: L walks a wire into a waiting eraser and both vanish.
    /// All lanes must complete under any thread count with no invariant damage.
    fn lanes_fixture(lanes: i32, length: i32) -> (Grid2, Net) {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        for lane in 0..lanes {
            let y = lane * 3;
            let l = place_agent(&mut grid, &mut shadow, (0, y, 0), Tag::L, Dir::E, None);
            let e = place_agent(&mut grid, &mut shadow, (length, y, 0), Tag::Eps, Dir::W, None);
            shadow.link(l.id, 0, e.id, 0);
            let path: Vec<Pos> = (0..=length).map(|x| (x, y, 0)).collect();
            lay_wire(&mut grid, &path);
        }
        (grid, shadow)
    }

    #[test]
    fn parallel_erasure_lanes_complete() {
        for threads in [1, 2, 4, 8] {
            let (grid, mut shadow) = lanes_fixture(32, 12);
            let seeds: Vec<Pos> = grid.cells.keys().copied().collect();
            let agrid = AtomicGrid::from_grid(&grid, 8);
            let stats = run_movement(&agrid, seeds, threads);
            let after = agrid.to_grid();
            assert_eq!(
                after.agents().count(),
                0,
                "{threads} threads: all lanes must erase (fires {})",
                stats.fires.load(Ordering::Relaxed)
            );
            assert_eq!(stats.fires.load(Ordering::Relaxed), 32, "{threads} threads");
            check_reciprocity(&after).unwrap();
            // The shadow agrees once the same fires are applied.
            while shadow.active_pair().is_some() {
                let (c, p) = shadow.active_pair().unwrap();
                shadow.fire(c, p);
            }
            assert_eq!(shadow.live_count(), 0);
            println!(
                "threads {threads}: commits {} conflicts {} moves {}",
                stats.commits.load(Ordering::Relaxed),
                stats.conflicts.load(Ordering::Relaxed),
                stats.moves.load(Ordering::Relaxed),
            );
        }
    }

    /// Crossing traffic: walkers whose wires pass through shared cells, so claims do
    /// collide; the result must still be exact.
    #[test]
    fn parallel_crossing_traffic_is_exact() {
        let mut grid = Grid2::new(Topo::Full3D);
        let mut shadow = Net::new();
        let n = 8i32;
        // Horizontal erasure lanes.
        for k in 0..n {
            let y = k * 2;
            let l = place_agent(&mut grid, &mut shadow, (0, y, 0), Tag::L, Dir::E, None);
            let e = place_agent(&mut grid, &mut shadow, (20, y, 0), Tag::Eps, Dir::W, None);
            shadow.link(l.id, 0, e.id, 0);
            lay_wire(&mut grid, &(0..=20).map(|x| (x, y, 0)).collect::<Vec<_>>());
        }
        // Vertical erasure lanes crossing all horizontal ones.
        for k in 0..n {
            let x = 3 + k * 2;
            let l = place_agent(&mut grid, &mut shadow, (x, -2, 0), Tag::L, Dir::S, None);
            let e = place_agent(&mut grid, &mut shadow, (x, n * 2, 0), Tag::Eps, Dir::N, None);
            shadow.link(l.id, 0, e.id, 0);
            lay_wire(&mut grid, &(-2..=n * 2).map(|y| (x, y, 0)).collect::<Vec<_>>());
        }
        check_reciprocity(&grid).unwrap();
        check_projection(&grid, &shadow).unwrap();
        let seeds: Vec<Pos> = grid.cells.keys().copied().collect();
        let agrid = AtomicGrid::from_grid(&grid, 8);
        let stats = run_movement(&agrid, seeds, 8);
        let after = agrid.to_grid();
        assert_eq!(after.agents().count(), 0, "all walkers must erase");
        assert_eq!(stats.fires.load(Ordering::Relaxed), 2 * n as usize);
        check_reciprocity(&after).unwrap();
        println!(
            "crossing: commits {} conflicts {}",
            stats.commits.load(Ordering::Relaxed),
            stats.conflicts.load(Ordering::Relaxed),
        );
    }
}
