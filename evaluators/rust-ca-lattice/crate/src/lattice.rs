//! Rung 2 state, after the TOPO LIFT: cells live at (x, y, z) with SIX faces.
//!
//! The 2D prototype carried per-face wire layers, via cells, and strands tucked behind
//! agents — three mechanisms all simulating a z-coordinate the cells were denied. Lifting
//! z into the position deletes all three: a crossing is two wires at different z, a walker
//! passes a crossing through ordinary cells, and an agent cell holds ONLY its agent. One
//! attachment per face (exclusive faces) suffices in 3D — six faces cover ≤3 ports or up
//! to `WIRE_CAP` strands. The 2D finding that exclusive faces deadlock walkers (3 ports +
//! 2 tuck faces exceed 4) was a symptom of the missing dimension, not a case for layers.
//! Where the old model tucked a crossing behind a mover, the lifted model has the mover
//! WAIT at a shared cell instead; kink-flips (transitions.rs) and later the fields are
//! the decongestants.
//!
//! Two topologies, selectable per Grid and validated differentially (tests/stage2.rs):
//! - `Bilayer` (z ∈ {0,1}) — the 2.5D chip: a compute plane plus one routing plane above.
//!   The honest worst case that maps onto today's metal-stack silicon.
//! - `Full3D` (z unbounded) — the general substrate; crossings can always route around.
//!
//! Everything else keeps rung 2's contracts: NO ids in the dynamics (`sid` is observer
//! state for the shadow projection and readback only), connectivity purely positional
//! (follow half-edges cell to cell), the loader is host code (global routing allowed HERE
//! and only here), and `check_projection` is the executable correctness spec
//! (EMBEDDING_THEOREM.md §4).
//!
//! State budget stays §3-honest and got smaller: agent = tag(4b) + 3 faces(3b each);
//! wire = ≤3 strands × 6b. No layer bits, no tuck slots.

use crate::net::Net;
use crate::rules::Tag;
use std::collections::BTreeMap;

pub type Pos = (i32, i32, i32);

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Dir { N, E, S, W, U, D }
pub const DIRS: [Dir; 6] = [Dir::N, Dir::E, Dir::S, Dir::W, Dir::U, Dir::D];

impl Dir {
    pub fn delta(self) -> (i32, i32, i32) {
        match self {
            Dir::N => (0, -1, 0),
            Dir::E => (1, 0, 0),
            Dir::S => (0, 1, 0),
            Dir::W => (-1, 0, 0),
            Dir::U => (0, 0, 1),
            Dir::D => (0, 0, -1),
        }
    }
    pub fn opp(self) -> Dir {
        match self {
            Dir::N => Dir::S, Dir::S => Dir::N,
            Dir::E => Dir::W, Dir::W => Dir::E,
            Dir::U => Dir::D, Dir::D => Dir::U,
        }
    }
    /// The four directions orthogonal to this one.
    pub fn perp(self) -> [Dir; 4] {
        match self {
            Dir::N | Dir::S => [Dir::E, Dir::W, Dir::U, Dir::D],
            Dir::E | Dir::W => [Dir::N, Dir::S, Dir::U, Dir::D],
            Dir::U | Dir::D => [Dir::N, Dir::E, Dir::S, Dir::W],
        }
    }
    pub fn ch(self) -> char {
        match self { Dir::N => 'N', Dir::E => 'E', Dir::S => 'S', Dir::W => 'W', Dir::U => 'U', Dir::D => 'D' }
    }
}

pub fn step(p: Pos, d: Dir) -> Pos { let (dx, dy, dz) = d.delta(); (p.0 + dx, p.1 + dy, p.2 + dz) }
pub fn dir_to(a: Pos, b: Pos) -> Option<Dir> { DIRS.into_iter().find(|d| step(a, *d) == b) }
pub fn manhattan(a: Pos, b: Pos) -> i32 { (a.0 - b.0).abs() + (a.1 - b.1).abs() + (a.2 - b.2).abs() }

/// A cell attachment point is simply a FACE. With six faces and no coexistence states,
/// one attachment per face is enough; the wire-layer bit of the 2D model is gone.
pub type He = Dir;
pub fn he_opp(h: He) -> He { h.opp() }

/// The lattice topology. Dynamics are identical across topologies; only which cells exist
/// differs, so the differential suite runs the same corpus on each.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Topo { Bilayer, Full3D }
pub const TOPOS: [Topo; 2] = [Topo::Bilayer, Topo::Full3D];

impl Topo {
    pub fn in_bounds(self, p: Pos) -> bool {
        match self { Topo::Bilayer => p.2 == 0 || p.2 == 1, Topo::Full3D => true }
    }
    pub fn name(self) -> &'static str {
        match self { Topo::Bilayer => "bilayer", Topo::Full3D => "full3d" }
    }
}

/// A wire element in one cell: passes through the cell between two distinct faces.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Strand { pub a: He, pub b: He }
impl Strand {
    pub fn new(a: He, b: He) -> Strand { assert!(a != b, "strand needs two distinct faces"); Strand { a, b } }
    pub fn contains(self, h: He) -> bool { self.a == h || self.b == h }
    pub fn other(self, h: He) -> He { if self.a == h { self.b } else { self.a } }
    pub fn hes(self) -> [He; 2] { [self.a, self.b] }
}

#[derive(Clone, Debug)]
pub struct AgentCell {
    pub tag: Tag,
    /// Port i attaches at face `faces[i]`. Distinct across live ports.
    pub faces: [Option<He>; 3],
    /// Shadow-net agent id. Observer state only: no transition's enabledness or effect
    /// reads it.
    pub sid: u32,
    /// True while this agent belongs to a still-unfolding seed complex: some of its wires
    /// dangle until siblings land, so fires and reels must leave it alone. Cleared when
    /// the seed finalizes.
    pub nascent: bool,
}

impl AgentCell {
    pub fn face_of(&self, port: usize) -> He { self.faces[port].expect("live port has a face") }
    pub fn port_at(&self, h: He) -> Option<usize> {
        (0..self.tag.arity()).find(|i| self.faces[*i] == Some(h))
    }
    pub fn used_hes(&self) -> Vec<He> {
        (0..self.tag.arity()).filter_map(|i| self.faces[i]).collect()
    }
    fn assert_coherent(&self) {
        let f = self.used_hes();
        for i in 0..f.len() { for j in i + 1..f.len() {
            assert!(f[i] != f[j], "agent {} face clash: {:?}", self.tag.name(), self);
        }}
        for i in 0..self.tag.arity() { assert!(self.faces[i].is_some(), "unwired port {i} on {}", self.tag.name()); }
    }
}

pub const WIRE_CAP: usize = 3; // strands per cell (6 of 6 faces)

#[derive(Clone, Debug, Default)]
pub struct WireCell { pub strands: [Option<Strand>; WIRE_CAP] }
impl WireCell {
    pub fn iter(&self) -> impl Iterator<Item = Strand> + '_ { self.strands.iter().flatten().copied() }
    pub fn count(&self) -> usize { self.strands.iter().flatten().count() }
    pub fn used_hes(&self) -> Vec<He> { self.iter().flat_map(|s| s.hes()).collect() }
    pub fn with_he(&self, h: He) -> Option<Strand> { self.iter().find(|s| s.contains(h)) }
}

/// One emission of a growing seed: a fresh agent, or one cell's worth of strands.
#[derive(Clone, Debug)]
pub enum GrowItem {
    Agent { cell: Pos, k: usize },
    Strands { cell: Pos, strands: Vec<Strand> },
}

/// The unfold script a docked pair executes: the stamp template cut into per-cell
/// emissions (outside cells first, then the producer cell's final content, then the
/// consumer cell's — the driver replaces itself last). Immutable; shared by the two
/// seed cells via Rc.
#[derive(Debug)]
pub struct GrowPlan {
    pub rule: &'static crate::rules::Rule,
    pub a: Pos,
    pub b: Pos,
    pub items: Vec<GrowItem>,
    pub fresh_tags: Vec<Tag>,
    pub fresh_faces: Vec<[Option<He>; 3]>,
    pub fresh_sids: Vec<u32>,
    /// Every cell the plan will write outside the pair (the reservation set).
    pub cells: Vec<Pos>,
}

/// A dying pair mid-unfold. The driver (consumer side) executes the plan; the partner
/// (producer side) just holds its cell and anchors until its final content lands.
/// `faces` keeps the dying agent's faces: the external anchors stay physically here.
#[derive(Clone, Debug)]
pub struct SeedCell {
    pub plan: std::rc::Rc<GrowPlan>,
    pub step: usize,
    pub driver: bool,
    pub faces: [Option<He>; 3],
    /// The dying agent's tag, kept so an aborted seed can restore the pair verbatim.
    pub tag: Tag,
    /// Consecutive waiting ticks (driver only); past SEED_PATIENCE the seed aborts.
    pub stall: u32,
}

#[derive(Clone, Debug)]
pub enum Cell { Agent(AgentCell), Wire(WireCell), Seed(SeedCell) }

pub struct Grid {
    pub cells: BTreeMap<Pos, Cell>,
    pub transport: u64, // reel count (the transport grade of the ledger)
    pub topo: Topo,
    /// Cells claimed by growing seeds (cell -> driver). A reservation keeps everyone ELSE
    /// out (fires, reel trails, slide destinations) so a claimed cell can only get freer;
    /// the seed emits into it once it is empty.
    pub reserved: BTreeMap<Pos, Pos>,
    pub seed_count: usize,
}

impl Grid {
    pub fn new(topo: Topo) -> Grid { Grid { cells: BTreeMap::new(), transport: 0, topo, reserved: BTreeMap::new(), seed_count: 0 } }

    pub fn agent(&self, p: Pos) -> Option<&AgentCell> {
        match self.cells.get(&p) { Some(Cell::Agent(a)) => Some(a), _ => None }
    }
    pub fn wire(&self, p: Pos) -> Option<&WireCell> {
        match self.cells.get(&p) { Some(Cell::Wire(w)) => Some(w), _ => None }
    }
    pub fn is_empty(&self, p: Pos) -> bool { !self.cells.contains_key(&p) }

    /// All faces in use at a cell (agent ports, strand ends, or a seed's held anchors).
    pub fn used_hes(&self, p: Pos) -> Vec<He> {
        match self.cells.get(&p) {
            None => vec![],
            Some(Cell::Agent(a)) => a.used_hes(),
            Some(Cell::Wire(w)) => w.used_hes(),
            Some(Cell::Seed(s)) => s.faces.iter().flatten().copied().collect(),
        }
    }
    /// Empty, in bounds, and not claimed by a seed: the only cells anyone may grow into.
    pub fn is_open(&self, p: Pos) -> bool {
        self.topo.in_bounds(p) && self.is_empty(p) && !self.reserved.contains_key(&p)
    }
    pub fn has_seeds(&self) -> bool { self.seed_count > 0 }
    pub fn seed_drivers(&self) -> Vec<Pos> {
        self.cells.iter().filter_map(|(p, c)| match c {
            Cell::Seed(s) if s.driver => Some(*p), _ => None,
        }).collect()
    }
    pub fn strand_count(&self, p: Pos) -> usize {
        match self.cells.get(&p) { Some(Cell::Wire(w)) => w.count(), _ => 0 }
    }

    pub fn strand_fits(&self, p: Pos, s: Strand) -> bool {
        if !self.topo.in_bounds(p) { return false; }
        match self.cells.get(&p) {
            None => true,
            Some(Cell::Wire(w)) => {
                w.count() < WIRE_CAP && !w.used_hes().iter().any(|h| s.contains(*h))
            }
            // no coexistence on the lifted lattice; seeds hold their cells exclusively
            Some(Cell::Agent(_)) | Some(Cell::Seed(_)) => false,
        }
    }
    pub fn add_strand(&mut self, p: Pos, s: Strand) {
        assert!(self.strand_fits(p, s), "strand does not fit at {p:?}");
        match self.cells.entry(p).or_insert_with(|| Cell::Wire(WireCell::default())) {
            Cell::Wire(w) => {
                let slot = w.strands.iter_mut().find(|x| x.is_none()).unwrap();
                *slot = Some(s);
            }
            Cell::Agent(_) | Cell::Seed(_) => unreachable!(),
        }
    }
    pub fn remove_strand(&mut self, p: Pos, s: Strand) {
        let Some(Cell::Wire(w)) = self.cells.get_mut(&p) else { panic!("no wire at {p:?}") };
        let slot = w.strands.iter_mut().find(|x| **x == Some(s)).expect("strand present");
        *slot = None;
        if w.count() == 0 { self.cells.remove(&p); }
    }
    pub fn put_agent(&mut self, p: Pos, a: AgentCell) {
        a.assert_coherent();
        assert!(self.topo.in_bounds(p), "agent out of bounds at {p:?}");
        assert!(self.is_empty(p), "placing agent on non-empty cell {p:?}");
        self.cells.insert(p, Cell::Agent(a));
    }

    /// Is the edge leaving `p` in direction `d` free on both faces (and in bounds)?
    pub fn edge_free(&self, p: Pos, d: Dir) -> bool {
        let q = step(p, d);
        self.topo.in_bounds(q)
            && !self.used_hes(p).contains(&d)
            && !self.used_hes(q).contains(&d.opp())
    }

    /// Follow the wire leaving `pos` through face `out` to the far agent port.
    /// Strands pass through; an agent port terminates. Returns None on anything a live
    /// unfold makes temporarily untraceable: a seed cell, a dangling half-edge, a missing
    /// port. Callers that gate on the far end (reel, clear) PARK on None.
    pub fn try_trace(&self, pos: Pos, out: He) -> Option<(Pos, usize)> {
        let (mut cur, mut h) = (pos, out);
        for _ in 0..1_000_000 {
            let next = step(cur, h);
            let enter = he_opp(h);
            match self.cells.get(&next) {
                Some(Cell::Agent(a)) => return a.port_at(enter).map(|port| (next, port)),
                Some(Cell::Wire(w)) => {
                    let s = w.with_he(enter)?;
                    cur = next; h = s.other(enter);
                }
                Some(Cell::Seed(_)) => return None,
                None => return None,
            }
        }
        None
    }
    /// Panicking form for the checker and readback, which only run at seed-free points.
    pub fn trace(&self, pos: Pos, out: He) -> (Pos, usize) {
        self.try_trace(pos, out)
            .unwrap_or_else(|| panic!("broken wire tracing from {pos:?} out {out:?}"))
    }

    pub fn agents(&self) -> impl Iterator<Item = (Pos, &AgentCell)> {
        self.cells.iter().filter_map(|(p, c)| match c { Cell::Agent(a) => Some((*p, a)), _ => None })
    }
    pub fn agent_count(&self) -> usize { self.agents().count() }
    pub fn find_tag(&self, tag: Tag) -> Option<Pos> {
        self.agents().find(|(_, a)| a.tag == tag).map(|(p, _)| p)
    }
    pub fn wire_positions(&self) -> Vec<Pos> {
        self.cells.iter().filter_map(|(p, c)| match c { Cell::Wire(_) => Some(*p), _ => None }).collect()
    }
    pub fn total_strands(&self) -> usize {
        self.cells.values().map(|c| match c { Cell::Wire(w) => w.count(), _ => 0 }).sum()
    }

    /// THE correctness spec, executable: the lattice projects exactly onto the shadow net.
    /// Every lattice agent's every port, traced along strands, lands where the shadow says;
    /// agent multisets agree; every occupied face pairs with its neighbor's opposite face.
    pub fn check_projection(&self, shadow: &Net) {
        assert!(self.seed_count == 0, "projection is only defined at seed-free points");
        // face parity
        for (p, _) in self.cells.iter() {
            for h in self.used_hes(*p) {
                let q = step(*p, h);
                assert!(self.topo.in_bounds(q), "face {h:?} at {p:?} points out of bounds");
                assert!(self.used_hes(q).contains(&he_opp(h)),
                    "face {h:?} at {p:?} has no partner at {q:?}");
            }
        }
        let mut seen = 0usize;
        for (pos, ac) in self.agents() {
            let sa = shadow.agents.get(ac.sid as usize)
                .and_then(|x| x.as_ref())
                .unwrap_or_else(|| panic!("lattice agent at {pos:?} has dead sid {}", ac.sid));
            assert_eq!(sa.tag, ac.tag, "tag mismatch at {pos:?}");
            seen += 1;
            for i in 0..ac.tag.arity() {
                let (fpos, fport) = self.trace(pos, ac.face_of(i));
                let fac = self.agent(fpos).expect("trace ends at agent");
                let want = sa.ports[i].unwrap_or_else(|| panic!("shadow port {i} of sid {} unwired", ac.sid));
                assert_eq!((fac.sid, fport as u8), want,
                    "projection mismatch: {}·port{} at {pos:?} traces to sid {} port {}, shadow says {:?}",
                    ac.tag.name(), i, fac.sid, fport, want);
            }
        }
        assert_eq!(seen, shadow.live_count(), "agent count drifted from shadow");
    }

    /// Read back the value tree hanging off the Out pad (lattice-side; the differential
    /// compares this against BOTH the shadow readback and the oracle NF).
    pub fn readback(&self) -> Option<crate::oracle::Term> {
        let out = self.find_tag(Tag::Out)?;
        let (vpos, vport) = self.trace(out, self.agent(out)?.face_of(0));
        if vport != 0 { return None; }
        self.read_value(vpos)
    }
    fn read_value(&self, pos: Pos) -> Option<crate::oracle::Term> {
        use crate::oracle::Term;
        let a = self.agent(pos)?;
        match a.tag {
            Tag::L => Some(Term::L),
            Tag::S => {
                let (c, p) = self.trace(pos, a.face_of(1));
                if p != 0 { return None; }
                Some(Term::S(std::rc::Rc::new(self.read_value(c)?)))
            }
            Tag::F => {
                let (l, lp) = self.trace(pos, a.face_of(1));
                let (r, rp) = self.trace(pos, a.face_of(2));
                if lp != 0 || rp != 0 { return None; }
                Some(Term::F(std::rc::Rc::new(self.read_value(l)?), std::rc::Rc::new(self.read_value(r)?)))
            }
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------------------
// The loader: place the shadow net's agents on the z=0 plane and route every link.
// Host code: global search is permitted HERE (and only here). Routing may use z freely,
// so plane crossings dip through the upper layer(s) and the initial embedding needs no
// crossings-in-a-cell at all.
// ---------------------------------------------------------------------------------------

pub fn embed(shadow: &Net, topo: Topo) -> Grid {
    for spacing in [4, 6, 8, 12] {
        if let Some(g) = try_embed(shadow, topo, spacing) { return g; }
    }
    panic!("embed failed at every spacing");
}

fn try_embed(shadow: &Net, topo: Topo, spacing: i32) -> Option<Grid> {
    // Positions: DFS from Out, children averaged, leaves on fresh columns (the JS layout).
    let out = shadow.agents.iter().enumerate()
        .find(|(_, a)| a.as_ref().is_some_and(|a| a.tag == Tag::Out)).map(|(i, _)| i as u32)?;
    let mut posn: BTreeMap<u32, Pos> = BTreeMap::new();
    let mut col = 0i32;
    fn dfs(shadow: &Net, id: u32, depth: i32, spacing: i32, col: &mut i32, posn: &mut BTreeMap<u32, Pos>) -> i32 {
        if let Some(p) = posn.get(&id) { return p.0; }
        posn.insert(id, (i32::MIN, depth * spacing, 0)); // reserve to break cycles
        let a = shadow.get(id);
        let kids: Vec<u32> = (0..a.tag.arity())
            .filter_map(|i| a.ports[i]).map(|(b, _)| b)
            .filter(|b| !posn.contains_key(b)).collect();
        let mut sx = 0i32; let mut n = 0i32;
        for k in &kids { sx += dfs(shadow, *k, depth + 1, spacing, col, posn); n += 1; }
        let x = if n > 0 { sx / n } else { *col += spacing; *col };
        posn.insert(id, (x, depth * spacing, 0));
        x
    }
    dfs(shadow, out, 0, spacing, &mut col, &mut posn);
    for (id, a) in shadow.agents.iter().enumerate() {
        if a.is_some() && !posn.contains_key(&(id as u32)) {
            col += spacing;
            posn.insert(id as u32, (col, -spacing, 0));
        }
    }
    // Nudge collisions apart deterministically.
    let mut taken: BTreeMap<Pos, u32> = BTreeMap::new();
    let ids: Vec<u32> = posn.keys().copied().collect();
    for id in ids {
        let mut q = posn[&id];
        while taken.contains_key(&q) { q.0 += 1; }
        taken.insert(q, id);
        posn.insert(id, q);
    }

    let mut grid = Grid::new(topo);
    for (id, p) in &posn {
        let tag = shadow.get(*id).tag;
        grid.cells.insert(*p, Cell::Agent(AgentCell { tag, faces: [None; 3], sid: *id, nascent: false }));
    }

    // Route each abstract link once (canonical endpoint order).
    let mut links: Vec<((u32, usize), (u32, usize))> = vec![];
    for (id, a) in shadow.agents.iter().enumerate() {
        let Some(a) = a else { continue };
        for i in 0..a.tag.arity() {
            let (b, j) = a.ports[i].expect("closed net");
            if (id as u32, i as u8) < (b, j) { links.push(((id as u32, i), (b, j as usize))); }
        }
    }
    let dbg = std::env::var("EMBED_DEBUG").is_ok();
    for ((aid, ai), (bid, bi)) in links {
        let (pa, pb) = (posn[&aid], posn[&bid]);
        let Some(path) = route_bfs(&grid, pa, pb) else {
            if dbg { eprintln!("embed: no route {aid}.{ai}@{pa:?} -> {bid}.{bi}@{pb:?} (spacing {spacing})"); }
            return None;
        };
        let first = *path.first().unwrap_or(&pb);
        let last = *path.last().unwrap_or(&pa);
        let fa = dir_to(pa, first)?;
        let fb = dir_to(pb, last)?;
        if grid.used_hes(pa).contains(&fa) || grid.used_hes(pb).contains(&fb) {
            if dbg { eprintln!("embed: face clash {aid}.{ai}@{pa:?}({fa:?}) / {bid}.{bi}@{pb:?}({fb:?})"); }
            return None;
        }
        set_face(&mut grid, pa, ai, fa);
        set_face(&mut grid, pb, bi, fb);
        let chain: Vec<Pos> = [vec![pa], path.clone(), vec![pb]].concat();
        for w in 1..chain.len() - 1 {
            let din = dir_to(chain[w], chain[w - 1])?;
            let dout = dir_to(chain[w], chain[w + 1])?;
            let s = Strand::new(din, dout);
            if !grid.strand_fits(chain[w], s) {
                if dbg { eprintln!("embed: strand misfit at {:?} for link {aid}.{ai}->{bid}.{bi}", chain[w]); }
                return None;
            }
            grid.add_strand(chain[w], s);
        }
    }
    grid.check_projection(shadow);
    Some(grid)
}

fn set_face(grid: &mut Grid, p: Pos, port: usize, h: He) {
    match grid.cells.get_mut(&p) {
        Some(Cell::Agent(a)) => a.faces[port] = Some(h),
        _ => panic!("no agent at {p:?}"),
    }
}

/// Route from a to b through empty in-bounds cells only. Uniform-cost search with a heavy
/// penalty for cells hugging OTHER agents' port rings — a shortest path that grazes an
/// agent seals its faces and strands its later links. Deterministic (cost, pos) ordering.
/// Returns interior cells (empty when adjacent).
fn route_bfs(grid: &Grid, a: Pos, b: Pos) -> Option<Vec<Pos>> {
    let a_used = grid.used_hes(a);
    let b_used = grid.used_hes(b);
    if let Some(d) = dir_to(a, b) {
        if !a_used.contains(&d) && !b_used.contains(&d.opp()) { return Some(vec![]); }
    }
    let ring_penalty = |n: Pos| -> u32 {
        for d in DIRS {
            let q = step(n, d);
            if q == a || q == b { continue; }
            if matches!(grid.cells.get(&q), Some(Cell::Agent(_))) { return 100; }
        }
        0
    };
    use std::cmp::Reverse;
    use std::collections::BinaryHeap;
    let mut dist: BTreeMap<Pos, u32> = BTreeMap::new();
    let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut heap: BinaryHeap<Reverse<(u32, Pos)>> = BinaryHeap::new();
    for d in DIRS {
        if a_used.contains(&d) { continue; }
        let n = step(a, d);
        if n == b || !grid.topo.in_bounds(n) || !grid.is_empty(n) { continue; }
        let c = 1 + ring_penalty(n);
        if dist.get(&n).is_none_or(|old| c < *old) {
            dist.insert(n, c);
            prev.insert(n, a);
            heap.push(Reverse((c, n)));
        }
    }
    let span = manhattan(a, b);
    let mut expansions = 0u32;
    while let Some(Reverse((c, cur))) = heap.pop() {
        if dist.get(&cur) != Some(&c) { continue; }
        expansions += 1;
        if expansions > 400_000 { return None; }
        for d in DIRS {
            let n = step(cur, d);
            if n == b {
                if b_used.contains(&d.opp()) { continue; }
                let mut path = vec![cur];
                let mut x = cur;
                while prev[&x] != a { x = prev[&x]; path.push(x); }
                path.reverse();
                return Some(path);
            }
            if !grid.topo.in_bounds(n) || !grid.is_empty(n) { continue; }
            if manhattan(n, a) > 4 * (span + 8) { continue; }
            let nc = c + 1 + ring_penalty(n);
            if dist.get(&n).is_none_or(|old| nc < *old) {
                dist.insert(n, nc);
                prev.insert(n, cur);
                heap.push(Reverse((nc, n)));
            }
        }
    }
    None
}
