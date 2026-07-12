//! Rung 2 state: the lattice. Cells hold EITHER an agent (≤3 ports, plus at most one
//! tucked strand passing behind it) OR up to three wire strands. There are NO wire objects
//! and NO ids in the dynamics: connectivity is purely positional (follow half-edges cell to
//! cell), the cell-native lesson from ca_substrate_viz carried over. Each `AgentCell`
//! carries a shadow-net id (`sid`) — OBSERVER state for the projection check and readback;
//! no transition's enabledness or effect reads it.
//!
//! WIRE LAYERS. Every face carries up to TWO wire layers (LOCAL_CA_DESIGN.md §2/§3: the
//! face map stores "port index or wire layer A or B"), so a cell attachment is a HALF-EDGE
//! `(Dir, layer)`, and an edge between neighbors carries up to two independent wires.
//! This is load-bearing for liveness, found the hard way: a first cut made faces exclusive
//! (one port OR one strand per face) and every 3-port walker deadlocked at the first via
//! it met — 3 port faces + 2 tuck faces exceed 4 exclusive faces, so crossings were
//! impassable. With per-edge layers a port and a passing strand share a face the way a
//! standard cell's pin coexists with a routing track on the metal layer above it. The
//! §4.4 face-uniqueness survives as a PREFERENCE in the planners (spread before stacking),
//! not an invariant.
//!
//! State budget stays §3-honest: agent = tag(4b) + 3 half-edges(3b each) + tuck(6b);
//! wire = ≤3 strands × 6b. A cell is a few tens of bits.
//!
//! The loader (`embed`) is host code and may be as global as it likes (§12 scope note);
//! the SUBSTRATE transitions in transitions.rs are the ones bound by locality.
//!
//! The projection invariant (EMBEDDING_THEOREM.md §4) is `check_projection`: trace every
//! agent port along strands to its far agent port and compare with the shadow net's
//! adjacency exactly, plus half-edge parity (every occupied half-edge pairs with its
//! neighbor's opposite half-edge on the same layer).

use crate::net::Net;
use crate::rules::Tag;
use std::collections::BTreeMap;

pub type Pos = (i32, i32);
pub type Layer = u8; // 0 | 1

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Dir { N, E, S, W }
pub const DIRS: [Dir; 4] = [Dir::N, Dir::E, Dir::S, Dir::W];

impl Dir {
    pub fn delta(self) -> (i32, i32) {
        match self { Dir::N => (0, -1), Dir::E => (1, 0), Dir::S => (0, 1), Dir::W => (-1, 0) }
    }
    pub fn opp(self) -> Dir {
        match self { Dir::N => Dir::S, Dir::E => Dir::W, Dir::S => Dir::N, Dir::W => Dir::E }
    }
    pub fn perp(self) -> [Dir; 2] {
        match self { Dir::N | Dir::S => [Dir::E, Dir::W], Dir::E | Dir::W => [Dir::N, Dir::S] }
    }
}
pub fn step(p: Pos, d: Dir) -> Pos { let (dx, dy) = d.delta(); (p.0 + dx, p.1 + dy) }
pub fn dir_to(a: Pos, b: Pos) -> Option<Dir> { DIRS.into_iter().find(|d| step(a, *d) == b) }

/// A cell attachment point: a face plus a wire layer on that face.
pub type He = (Dir, Layer);
pub fn he_opp(h: He) -> He { (h.0.opp(), h.1) }

/// A wire element in one cell: passes through the cell between two distinct half-edges.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Strand { pub a: He, pub b: He }
impl Strand {
    pub fn new(a: He, b: He) -> Strand { assert!(a != b, "strand needs two distinct half-edges"); Strand { a, b } }
    pub fn contains(self, h: He) -> bool { self.a == h || self.b == h }
    pub fn other(self, h: He) -> He { if self.a == h { self.b } else { self.a } }
    pub fn hes(self) -> [He; 2] { [self.a, self.b] }
}

#[derive(Clone, Debug)]
pub struct AgentCell {
    pub tag: Tag,
    /// Port i attaches at half-edge `faces[i]`. Distinct across live ports and the tuck.
    pub faces: [Option<He>; 3],
    /// Up to two foreign strands passing behind the agent (a value sitting on crossings,
    /// §4.4; 3 ports + 4 tuck half-edges = 7 of 8 half-edges, still §3-bounded).
    pub tucks: [Option<Strand>; 2],
    /// Shadow-net agent id. Observer state only.
    pub sid: u32,
}

impl AgentCell {
    pub fn face_of(&self, port: usize) -> He { self.faces[port].expect("live port has a half-edge") }
    pub fn port_at(&self, h: He) -> Option<usize> {
        (0..self.tag.arity()).find(|i| self.faces[*i] == Some(h))
    }
    pub fn used_hes(&self) -> Vec<He> {
        let mut v: Vec<He> = (0..self.tag.arity()).filter_map(|i| self.faces[i]).collect();
        for t in self.tucks.iter().flatten() { v.extend(t.hes()); }
        v
    }
    fn assert_coherent(&self) {
        let f = self.used_hes();
        for i in 0..f.len() { for j in i + 1..f.len() {
            assert!(f[i] != f[j], "agent {} half-edge clash: {:?}", self.tag.name(), self);
        }}
        for i in 0..self.tag.arity() { assert!(self.faces[i].is_some(), "unwired port {i} on {}", self.tag.name()); }
    }
}

pub const WIRE_CAP: usize = 3; // strands per cell (6 of 8 half-edges)

#[derive(Clone, Debug, Default)]
pub struct WireCell { pub strands: [Option<Strand>; WIRE_CAP] }
impl WireCell {
    pub fn iter(&self) -> impl Iterator<Item = Strand> + '_ { self.strands.iter().flatten().copied() }
    pub fn count(&self) -> usize { self.strands.iter().flatten().count() }
    pub fn used_hes(&self) -> Vec<He> { self.iter().flat_map(|s| s.hes()).collect() }
    pub fn with_he(&self, h: He) -> Option<Strand> { self.iter().find(|s| s.contains(h)) }
}

#[derive(Clone, Debug)]
pub enum Cell { Agent(AgentCell), Wire(WireCell) }

#[derive(Default)]
pub struct Grid {
    pub cells: BTreeMap<Pos, Cell>,
    pub transport: u64, // reel count (the transport grade of the ledger)
}

impl Grid {
    pub fn agent(&self, p: Pos) -> Option<&AgentCell> {
        match self.cells.get(&p) { Some(Cell::Agent(a)) => Some(a), _ => None }
    }
    pub fn wire(&self, p: Pos) -> Option<&WireCell> {
        match self.cells.get(&p) { Some(Cell::Wire(w)) => Some(w), _ => None }
    }
    pub fn is_empty(&self, p: Pos) -> bool { !self.cells.contains_key(&p) }

    /// All half-edges in use at a cell (agent ports + tuck + strands).
    pub fn used_hes(&self, p: Pos) -> Vec<He> {
        match self.cells.get(&p) {
            None => vec![],
            Some(Cell::Agent(a)) => a.used_hes(),
            Some(Cell::Wire(w)) => w.used_hes(),
        }
    }
    pub fn strand_count(&self, p: Pos) -> usize {
        match self.cells.get(&p) {
            Some(Cell::Wire(w)) => w.count(),
            Some(Cell::Agent(a)) => a.tucks.iter().flatten().count(),
            None => 0,
        }
    }

    pub fn strand_fits(&self, p: Pos, s: Strand) -> bool {
        match self.cells.get(&p) {
            None => true,
            Some(Cell::Wire(w)) => {
                w.count() < WIRE_CAP && !w.used_hes().iter().any(|h| s.contains(*h))
            }
            Some(Cell::Agent(_)) => false, // tucks arise only from a REEL stepping over a strand
        }
    }
    pub fn add_strand(&mut self, p: Pos, s: Strand) {
        assert!(self.strand_fits(p, s), "strand does not fit at {p:?}");
        match self.cells.entry(p).or_insert_with(|| Cell::Wire(WireCell::default())) {
            Cell::Wire(w) => {
                let slot = w.strands.iter_mut().find(|x| x.is_none()).unwrap();
                *slot = Some(s);
            }
            Cell::Agent(_) => unreachable!(),
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
        assert!(self.is_empty(p), "placing agent on non-empty cell {p:?}");
        self.cells.insert(p, Cell::Agent(a));
    }

    /// Lowest free layer on the edge leaving `p` in direction `d` (both endpoints
    /// consulted), or None when the edge already carries two wires.
    pub fn edge_free_layer(&self, p: Pos, d: Dir) -> Option<Layer> {
        let q = step(p, d);
        'l: for l in 0..2u8 {
            if self.used_hes(p).contains(&(d, l)) { continue 'l; }
            if self.used_hes(q).contains(&(d.opp(), l)) { continue 'l; }
            return Some(l);
        }
        None
    }

    /// Follow the wire leaving `pos` through half-edge `out` to the far agent port.
    /// Strands (and tucks) pass through; an agent port terminates.
    pub fn trace(&self, pos: Pos, out: He) -> (Pos, usize) {
        let (mut cur, mut h) = (pos, out);
        for _ in 0..1_000_000 {
            let next = step(cur, h.0);
            let enter = he_opp(h);
            match self.cells.get(&next) {
                Some(Cell::Agent(a)) => {
                    if let Some(t) = a.tucks.iter().flatten().find(|t| t.contains(enter)) {
                        cur = next; h = t.other(enter); continue;
                    }
                    let port = a.port_at(enter)
                        .unwrap_or_else(|| panic!("wire enters {next:?} at {enter:?} but no port there"));
                    return (next, port);
                }
                Some(Cell::Wire(w)) => {
                    let s = w.with_he(enter)
                        .unwrap_or_else(|| panic!("wire enters {next:?} at {enter:?} but no strand there"));
                    cur = next; h = s.other(enter);
                }
                None => panic!("dangling wire: {cur:?} -> {next:?} is empty"),
            }
        }
        panic!("trace did not terminate (wire cycle?)");
    }

    pub fn agents(&self) -> impl Iterator<Item = (Pos, &AgentCell)> {
        self.cells.iter().filter_map(|(p, c)| match c { Cell::Agent(a) => Some((*p, a)), _ => None })
    }
    pub fn agent_count(&self) -> usize { self.agents().count() }
    pub fn find_tag(&self, tag: Tag) -> Option<Pos> {
        self.agents().find(|(_, a)| a.tag == tag).map(|(p, _)| p)
    }

    /// THE correctness spec, executable: the lattice projects exactly onto the shadow net.
    /// Every lattice agent's every port, traced along strands, lands where the shadow says;
    /// agent multisets agree; every occupied half-edge pairs with its neighbor's.
    pub fn check_projection(&self, shadow: &Net) {
        // half-edge parity
        for (p, _) in self.cells.iter() {
            for h in self.used_hes(*p) {
                let q = step(*p, h.0);
                assert!(self.used_hes(q).contains(&he_opp(h)),
                    "half-edge {h:?} at {p:?} has no partner at {q:?}");
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
// The loader: place the shadow net's agents on the lattice and route every link.
// Host code: global search is permitted HERE (and only here). The loader lays everything
// on layer 0 through empty cells, so the initial embedding is via-free by construction.
// ---------------------------------------------------------------------------------------

pub fn embed(shadow: &Net) -> Grid {
    for spacing in [4, 6, 8, 12] {
        if let Some(g) = try_embed(shadow, spacing) { return g; }
    }
    panic!("embed failed at every spacing");
}

fn try_embed(shadow: &Net, spacing: i32) -> Option<Grid> {
    // Positions: DFS from Out, children averaged, leaves on fresh columns (the JS layout).
    let out = shadow.agents.iter().enumerate()
        .find(|(_, a)| a.as_ref().is_some_and(|a| a.tag == Tag::Out)).map(|(i, _)| i as u32)?;
    let mut posn: BTreeMap<u32, Pos> = BTreeMap::new();
    let mut col = 0i32;
    fn dfs(shadow: &Net, id: u32, depth: i32, spacing: i32, col: &mut i32, posn: &mut BTreeMap<u32, Pos>) -> i32 {
        if let Some(p) = posn.get(&id) { return p.0; }
        posn.insert(id, (i32::MIN, depth * spacing)); // reserve to break cycles
        let a = shadow.get(id);
        let kids: Vec<u32> = (0..a.tag.arity())
            .filter_map(|i| a.ports[i]).map(|(b, _)| b)
            .filter(|b| !posn.contains_key(b)).collect();
        let mut sx = 0i32; let mut n = 0i32;
        for k in &kids { sx += dfs(shadow, *k, depth + 1, spacing, col, posn); n += 1; }
        let x = if n > 0 { sx / n } else { *col += spacing; *col };
        posn.insert(id, (x, depth * spacing));
        x
    }
    dfs(shadow, out, 0, spacing, &mut col, &mut posn);
    for (id, a) in shadow.agents.iter().enumerate() {
        if a.is_some() && !posn.contains_key(&(id as u32)) {
            col += spacing;
            posn.insert(id as u32, (col, -spacing));
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

    let mut grid = Grid::default();
    for (id, p) in &posn {
        let tag = shadow.get(*id).tag;
        grid.cells.insert(*p, Cell::Agent(AgentCell { tag, faces: [None; 3], tucks: [None; 2], sid: *id }));
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
        if grid.used_hes(pa).iter().any(|h| h.0 == fa) || grid.used_hes(pb).iter().any(|h| h.0 == fb) {
            if dbg { eprintln!("embed: face clash {aid}.{ai}@{pa:?}({fa:?}) / {bid}.{bi}@{pb:?}({fb:?})"); }
            return None;
        }
        set_face(&mut grid, pa, ai, (fa, 0));
        set_face(&mut grid, pb, bi, (fb, 0));
        let chain: Vec<Pos> = [vec![pa], path.clone(), vec![pb]].concat();
        for w in 1..chain.len() - 1 {
            let din = dir_to(chain[w], chain[w - 1])?;
            let dout = dir_to(chain[w], chain[w + 1])?;
            let s = Strand::new((din, 0), (dout, 0));
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

/// Route from a to b through empty cells only. Uniform-cost search with a heavy penalty
/// for cells hugging OTHER agents' port rings — a shortest path that grazes an agent seals
/// its faces and strands its later links. Deterministic (cost, pos) ordering. Returns
/// interior cells (empty when adjacent).
fn route_bfs(grid: &Grid, a: Pos, b: Pos) -> Option<Vec<Pos>> {
    let a_used: Vec<Dir> = grid.used_hes(a).iter().map(|h| h.0).collect();
    let b_used: Vec<Dir> = grid.used_hes(b).iter().map(|h| h.0).collect();
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
        if n == b || !grid.is_empty(n) { continue; }
        let c = 1 + ring_penalty(n);
        if dist.get(&n).is_none_or(|old| c < *old) {
            dist.insert(n, c);
            prev.insert(n, a);
            heap.push(Reverse((c, n)));
        }
    }
    let span = (a.0 - b.0).abs() + (a.1 - b.1).abs();
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
            if !grid.is_empty(n) { continue; }
            if (n.0 - a.0).abs() + (n.1 - a.1).abs() > 4 * (span + 8) { continue; }
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
