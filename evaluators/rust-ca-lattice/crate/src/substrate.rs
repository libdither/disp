//! Packed, single-route lattice substrate.
//!
//! This is the replacement geometry. Every real site is one [`CellWord`]; stable agent ids
//! are observer-only sidecar data. A wire site contains one exclusive route, while arity-three
//! agents expose both auxiliaries through one two-lane tail terminated by a [`Matter::Zip`].

use crate::cell64::{
    CellWord, Control, DecodedCell, Dir, FacePair, LaneCount, LaneMask, Matter, Pull, U3,
    DIRS,
};
use crate::lattice::{dir_to, manhattan, step, Pos, Topo};
use crate::net::Net;
use crate::oracle::Term;
use crate::rules::Tag;
use std::collections::{BTreeMap, BinaryHeap};

/// A non-state sentinel supplied for a missing neighbor at a finite topology boundary.
/// Control kinds 6 and 7 are deliberately outside the valid packed alphabet.
pub const BOUNDARY: CellWord = CellWord(0b111u64 << 20);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Channel {
    pub face: Dir,
    pub lane: u8,
}

/// Sparse storage is only an implementation optimization: absent entries are exactly the
/// all-zero empty word. Every present entry is still one complete 64-bit site state.
#[derive(Clone, Debug)]
pub struct Grid64 {
    pub cells: BTreeMap<Pos, CellWord>,
    pub topo: Topo,
    pub transport: u64,
    pub rewrites: u64,
    /// Stable abstract-net id for visualization/projection. Never exposed to `next_cell`.
    pub observer_sid: BTreeMap<Pos, u32>,
}

impl Grid64 {
    pub fn new(topo: Topo) -> Self {
        Self {
            cells: BTreeMap::new(),
            topo,
            transport: 0,
            rewrites: 0,
            observer_sid: BTreeMap::new(),
        }
    }

    pub fn word(&self, p: Pos) -> CellWord {
        if self.topo.in_bounds(p) { self.cells.get(&p).copied().unwrap_or_default() } else { BOUNDARY }
    }

    pub fn decoded(&self, p: Pos) -> Option<DecodedCell> {
        if !self.topo.in_bounds(p) { return None; }
        Some(self.word(p).unpack().expect("stored CellWord must be canonical"))
    }

    pub fn matter(&self, p: Pos) -> Option<Matter> { self.decoded(p).map(|c| c.matter) }

    pub fn control(&self, p: Pos) -> Option<Control> { self.decoded(p).map(|c| c.control) }

    pub fn is_empty(&self, p: Pos) -> bool {
        self.decoded(p).is_some_and(|c| c.matter == Matter::Empty && c.control == Control::Idle)
    }

    pub fn set(&mut self, p: Pos, cell: DecodedCell) {
        assert!(self.topo.in_bounds(p), "write outside topology at {p:?}");
        let word = CellWord::pack(cell).expect("attempted to store an invalid packed cell");
        if word == CellWord::default() { self.cells.remove(&p); } else { self.cells.insert(p, word); }
    }

    pub fn set_word(&mut self, p: Pos, word: CellWord) {
        assert!(self.topo.in_bounds(p), "write outside topology at {p:?}");
        word.unpack().expect("attempted to store an invalid packed word");
        if word == CellWord::default() { self.cells.remove(&p); } else { self.cells.insert(p, word); }
    }

    pub fn neighborhood(&self, p: Pos) -> [CellWord; 6] {
        DIRS.map(|d| self.word(step(p, d)))
    }

    pub fn positions_with_halo(&self) -> Vec<Pos> {
        let mut sites: std::collections::BTreeSet<Pos> = self.cells.keys().copied().collect();
        for p in self.cells.keys().copied().collect::<Vec<_>>() {
            for d in DIRS {
                let q = step(p, d);
                if self.topo.in_bounds(q) { sites.insert(q); }
            }
        }
        sites.into_iter().collect()
    }

    pub fn agents(&self) -> impl Iterator<Item = (Pos, Tag)> + '_ {
        self.cells.iter().filter_map(|(p, w)| match w.unpack().ok()?.matter {
            Matter::Agent { tag, .. } => Some((*p, tag)),
            Matter::GuestZip { tag, .. } | Matter::GuestLink { tag, .. } => Some((*p, tag)),
            _ => None,
        })
    }

    pub fn agent_count(&self) -> usize { self.agents().count() }

    pub fn has_protocol(&self) -> bool {
        self.cells.values().any(|w| w.unpack().is_ok_and(|c| c.control != Control::Idle))
    }

    pub fn channel_at_port(&self, pos: Pos, port: usize) -> Option<Channel> {
        match self.matter(pos)? {
            Matter::Agent { tag, principal, tail, aux_flip } => {
                if port >= tag.arity() { return None; }
                match port {
                    0 => Some(Channel { face: principal, lane: 0 }),
                    1 if tag.arity() == 2 => Some(Channel { face: tail?, lane: 0 }),
                    1 | 2 if tag.arity() == 3 => {
                        let semantic_lane = (port - 1) as u8;
                        Some(Channel { face: tail?, lane: semantic_lane ^ aux_flip as u8 })
                    }
                    _ => None,
                }
            }
            Matter::GuestZip { tag, plane, trunk, branches, twist, deep } => {
                if port >= tag.arity() { return None; }
                let ridden = branches[(plane ^ twist as u8) as usize];
                match (port, deep) {
                    (0, false) => Some(Channel { face: trunk, lane: plane }),
                    (0, true) => Some(Channel { face: ridden, lane: 0 }),
                    (1, false) => Some(Channel { face: ridden, lane: 0 }),
                    (1, true) => Some(Channel { face: trunk, lane: plane }),
                    _ => None,
                }
            }
            Matter::GuestLink { tag, principal, plane, ends, twist } => {
                if port >= tag.arity() { return None; }
                match port {
                    0 => Some(Channel { face: principal, lane: plane }),
                    1 => Some(Channel {
                        face: ends.other(principal)?,
                        lane: plane ^ twist as u8,
                    }),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn port_at_channel(&self, pos: Pos, channel: Channel) -> Option<usize> {
        match self.matter(pos)? {
            Matter::Agent { tag, principal, tail, aux_flip } => {
                if channel.face == principal && channel.lane == 0 { return Some(0); }
                if Some(channel.face) != tail { return None; }
                match tag.arity() {
                    2 if channel.lane == 0 => Some(1),
                    3 if channel.lane < 2 => Some((channel.lane ^ aux_flip as u8) as usize + 1),
                    _ => None,
                }
            }
            Matter::GuestZip { tag, plane, trunk, branches, twist, deep } => {
                let ridden = branches[(plane ^ twist as u8) as usize];
                let principal = if deep {
                    Channel { face: ridden, lane: 0 }
                } else {
                    Channel { face: trunk, lane: plane }
                };
                if channel == principal { return Some(0); }
                if tag.arity() != 2 { return None; }
                let tail = if deep {
                    Channel { face: trunk, lane: plane }
                } else {
                    Channel { face: ridden, lane: 0 }
                };
                (channel == tail).then_some(1)
            }
            Matter::GuestLink { tag, principal, plane, ends, twist } => {
                if channel.face == principal && channel.lane == plane { return Some(0); }
                if tag.arity() != 2 { return None; }
                (Some(channel.face) == ends.other(principal)
                    && channel.lane == plane ^ twist as u8)
                    .then_some(1)
            }
            _ => None,
        }
    }

    pub fn channels(&self, p: Pos) -> Vec<Channel> {
        match self.matter(p) {
            Some(Matter::Empty) | None => vec![],
            Some(Matter::Agent { tag, .. }) => (0..tag.arity())
                .filter_map(|port| self.channel_at_port(p, port)).collect(),
            // A guest exposes its agent ports plus the foreign through-channels; a channel
            // an arity-one rider consumed vanishes outright (it is no port and no route).
            Some(Matter::GuestZip { tag, plane, trunk, branches, twist, .. }) => {
                let foreign = branches[(plane ^ twist as u8 ^ 1) as usize];
                let mut channels: Vec<Channel> = (0..tag.arity())
                    .filter_map(|port| self.channel_at_port(p, port))
                    .collect();
                channels.push(Channel { face: trunk, lane: plane ^ 1 });
                channels.push(Channel { face: foreign, lane: 0 });
                channels
            }
            Some(Matter::GuestLink { tag, principal, plane, ends, twist }) => {
                let entry = ends.other(principal).expect("guest rides one end");
                let mut channels: Vec<Channel> = (0..tag.arity())
                    .filter_map(|port| self.channel_at_port(p, port))
                    .collect();
                channels.push(Channel { face: principal, lane: plane ^ 1 });
                channels.push(Channel { face: entry, lane: plane ^ twist as u8 ^ 1 });
                channels
            }
            Some(Matter::Link { ends, lanes, .. }) => {
                let n = if lanes == LaneCount::One { 1 } else { 2 };
                [ends.a, ends.b].into_iter().flat_map(|face| {
                    (0..n).map(move |lane| Channel { face, lane })
                }).collect()
            }
            Some(Matter::Zip { trunk, branches, .. }) => vec![
                Channel { face: trunk, lane: 0 }, Channel { face: trunk, lane: 1 },
                Channel { face: branches[0], lane: 0 },
                Channel { face: branches[1], lane: 0 },
            ],
            Some(Matter::Cross { routes, .. }) => routes.into_iter().flat_map(|route| [
                Channel { face: route.a, lane: 0 }, Channel { face: route.b, lane: 0 },
            ]).collect(),
        }
    }

    fn through(&self, p: Pos, entered: Channel) -> Option<Channel> {
        match self.matter(p)? {
            Matter::Empty | Matter::Agent { .. } => None,
            Matter::GuestZip { plane, trunk, branches, twist, deep, .. } => {
                // Zip semantics on the underlying geometry, except the agent's own
                // channels: principal and (arity two) entry are port endpoints, and an
                // arity-one rider's entry channel is dead.
                let ridden = branches[(plane ^ twist as u8) as usize];
                let foreign = branches[(plane ^ twist as u8 ^ 1) as usize];
                let agent_channels = if deep {
                    [Channel { face: ridden, lane: 0 }, Channel { face: trunk, lane: plane }]
                } else {
                    [Channel { face: trunk, lane: plane }, Channel { face: ridden, lane: 0 }]
                };
                if agent_channels.contains(&entered) {
                    return None;
                }
                if entered.face == trunk && entered.lane == plane ^ 1 {
                    Some(Channel { face: foreign, lane: 0 })
                } else if entered.face == foreign && entered.lane == 0 {
                    Some(Channel { face: trunk, lane: plane ^ 1 })
                } else {
                    None
                }
            }
            Matter::GuestLink { principal, plane, ends, twist, .. } => {
                // Passthrough only: the foreign lane crosses; the principal channel and
                // the agent's entry lane are endpoints (dead entry for arity one).
                let entry = ends.other(principal)?;
                let foreign_entry = plane ^ twist as u8 ^ 1;
                if entered.face == entry && entered.lane == foreign_entry {
                    Some(Channel { face: principal, lane: plane ^ 1 })
                } else if entered.face == principal && entered.lane == plane ^ 1 {
                    Some(Channel { face: entry, lane: foreign_entry })
                } else {
                    None
                }
            }
            Matter::Link { ends, lanes, twist, .. } => {
                let max_lane = if lanes == LaneCount::One { 1 } else { 2 };
                if entered.lane >= max_lane { return None; }
                let face = ends.other(entered.face)?;
                let lane = if lanes == LaneCount::Two && twist { entered.lane ^ 1 } else { entered.lane };
                Some(Channel { face, lane })
            }
            Matter::Zip { trunk, branches, twist, .. } => {
                if entered.face == trunk && entered.lane < 2 {
                    let branch = entered.lane ^ twist as u8;
                    Some(Channel { face: branches[branch as usize], lane: 0 })
                } else if entered.lane == 0 {
                    let branch = branches.iter().position(|f| *f == entered.face)? as u8;
                    Some(Channel { face: trunk, lane: branch ^ twist as u8 })
                } else {
                    None
                }
            }
            Matter::Cross { routes, .. } => {
                let route = routes.into_iter().find(|route| route.contains(entered.face))?;
                Some(Channel { face: route.other(entered.face)?, lane: 0 })
            }
        }
    }

    pub fn trace(&self, pos: Pos, port: usize) -> Option<(Pos, usize)> {
        let mut channel = self.channel_at_port(pos, port)?;
        let mut cur = pos;
        for _ in 0..1_000_000 {
            let next = step(cur, channel.face);
            if !self.topo.in_bounds(next) { return None; }
            let entered = Channel { face: channel.face.opp(), lane: channel.lane };
            if let Some(port) = self.port_at_channel(next, entered) { return Some((next, port)); }
            channel = self.through(next, entered)?;
            cur = next;
        }
        None
    }

    pub fn check_projection(&self, shadow: &Net) {
        assert!(!self.has_protocol(), "projection checkpoint contains an active protocol");
        for (&p, word) in &self.cells {
            word.unpack().expect("noncanonical word in grid");
            for channel in self.channels(p) {
                let q = step(p, channel.face);
                assert!(self.topo.in_bounds(q), "channel {channel:?} at {p:?} exits topology");
                assert!(self.channels(q).contains(&Channel {
                    face: channel.face.opp(), lane: channel.lane,
                }), "channel {channel:?} at {p:?} has no reciprocal endpoint at {q:?}");
            }
        }

        let mut seen = 0;
        for (pos, tag) in self.agents() {
            let sid = *self.observer_sid.get(&pos)
                .unwrap_or_else(|| panic!("agent {tag:?} at {pos:?} has no observer sid"));
            let abstract_agent = shadow.agents.get(sid as usize).and_then(|a| a.as_ref())
                .unwrap_or_else(|| panic!("agent {tag:?} at {pos:?} has dead sid {sid}"));
            assert_eq!(tag, abstract_agent.tag, "tag mismatch at {pos:?}");
            seen += 1;
            for port in 0..tag.arity() {
                let (far_pos, far_port) = self.trace(pos, port)
                    .unwrap_or_else(|| panic!("broken trace from {tag:?} at {pos:?} port {port}"));
                let far_sid = *self.observer_sid.get(&far_pos).expect("trace ended without sid");
                assert_eq!(abstract_agent.ports[port], Some((far_sid, far_port as u8)),
                    "projection mismatch at sid {sid} port {port}");
            }
        }
        assert_eq!(seen, shadow.live_count(), "packed agent count drifted from shadow");
    }

    pub fn readback(&self) -> Option<Term> {
        let out = self.agents().find(|(_, tag)| *tag == Tag::Out)?.0;
        let (value, port) = self.trace(out, 0)?;
        (port == 0).then(|| self.read_value(value)).flatten()
    }

    fn read_value(&self, pos: Pos) -> Option<Term> {
        let Matter::Agent { tag, .. } = self.matter(pos)? else { return None };
        match tag {
            Tag::L => Some(Term::L),
            Tag::S => {
                let (child, port) = self.trace(pos, 1)?;
                if port != 0 { return None; }
                Some(Term::S(std::rc::Rc::new(self.read_value(child)?)))
            }
            Tag::F => {
                let (left, lp) = self.trace(pos, 1)?;
                let (right, rp) = self.trace(pos, 2)?;
                if lp != 0 || rp != 0 { return None; }
                Some(Term::F(
                    std::rc::Rc::new(self.read_value(left)?),
                    std::rc::Rc::new(self.read_value(right)?),
                ))
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
struct Terminal { cell: Pos, face: Dir }

pub fn embed64(shadow: &Net, topo: Topo) -> Grid64 {
    for spacing in [8, 12, 16, 24] {
        if let Some(grid) = try_embed64(shadow, topo, spacing) { return grid; }
    }
    panic!("packed embed failed at every spacing")
}

fn try_embed64(shadow: &Net, topo: Topo, spacing: i32) -> Option<Grid64> {
    let positions = layout_agents(shadow, spacing)?;
    let mut grid = Grid64::new(topo);
    let mut terminals: BTreeMap<(u32, usize), Terminal> = BTreeMap::new();

    for (&sid, &p) in &positions {
        let tag = shadow.get(sid).tag;
        let tail = (tag.arity() > 1).then_some(Dir::S);
        grid.set(p, DecodedCell {
            matter: Matter::Agent { tag, principal: Dir::N, tail, aux_flip: false },
            ..DecodedCell::default()
        });
        grid.observer_sid.insert(p, sid);
        terminals.insert((sid, 0), Terminal { cell: p, face: Dir::N });

        match tag.arity() {
            1 => {}
            2 => { terminals.insert((sid, 1), Terminal { cell: p, face: Dir::S }); }
            3 => {
                let zip = step(p, Dir::S);
                if !grid.is_empty(zip) { return None; }
                grid.set(zip, DecodedCell {
                    matter: Matter::Zip {
                        trunk: Dir::N,
                        branches: [Dir::E, Dir::W],
                        twist: false,
                        hot: LaneMask::new(0).unwrap(),
                        cooldown: U3::new(0).unwrap(),
                        pull: [Pull::None; 2],
                    },
                    ..DecodedCell::default()
                });
                terminals.insert((sid, 1), Terminal { cell: zip, face: Dir::E });
                terminals.insert((sid, 2), Terminal { cell: zip, face: Dir::W });
            }
            _ => unreachable!(),
        }
    }

    let mut links = vec![];
    for (sid, agent) in shadow.agents.iter().enumerate() {
        let Some(agent) = agent else { continue };
        for port in 0..agent.tag.arity() {
            let (far_sid, far_port) = agent.ports[port]?;
            if (sid as u32, port as u8) < (far_sid, far_port) {
                links.push(((sid as u32, port), (far_sid, far_port as usize)));
            }
        }
    }
    // Longer routes first reduces late paths sealing the only bilayer crossing corridor.
    links.sort_by_key(|((a, _), (b, _))| std::cmp::Reverse(manhattan(positions[a], positions[b])));

    for (a, b) in links {
        let start = terminals[&a];
        let end = terminals[&b];
        let path = route_terminals(&grid, start, end)?;
        for (i, &p) in path.iter().enumerate() {
            let prev = if i == 0 { start.cell } else { path[i - 1] };
            let next = if i + 1 == path.len() { end.cell } else { path[i + 1] };
            let ends = FacePair::new(dir_to(p, prev)?, dir_to(p, next)?)?;
            grid.set(p, DecodedCell {
                matter: Matter::Link {
                    ends,
                    lanes: LaneCount::One,
                    twist: false,
                    hot: LaneMask::new(0).unwrap(),
                    cooldown: U3::new(0).unwrap(),
                    pull: [Pull::None; 2],
                },
                ..DecodedCell::default()
            });
        }
    }
    grid.check_projection(shadow);
    Some(grid)
}

fn layout_agents(shadow: &Net, spacing: i32) -> Option<BTreeMap<u32, Pos>> {
    let out = shadow.agents.iter().enumerate()
        .find(|(_, a)| a.as_ref().is_some_and(|a| a.tag == Tag::Out))?.0 as u32;
    let mut positions = BTreeMap::new();
    let mut column = 0;
    fn visit(
        shadow: &Net,
        sid: u32,
        depth: i32,
        spacing: i32,
        column: &mut i32,
        positions: &mut BTreeMap<u32, Pos>,
    ) -> i32 {
        if let Some(p) = positions.get(&sid) { return p.0; }
        positions.insert(sid, (i32::MIN, depth * spacing, 0));
        let agent = shadow.get(sid);
        let children: Vec<u32> = (0..agent.tag.arity())
            .filter_map(|port| agent.ports[port].map(|p| p.0))
            .filter(|child| !positions.contains_key(child)).collect();
        let mut sum = 0;
        for child in &children { sum += visit(shadow, *child, depth + 1, spacing, column, positions); }
        let x = if children.is_empty() { *column += spacing; *column } else { sum / children.len() as i32 };
        positions.insert(sid, (x, depth * spacing, 0));
        x
    }
    visit(shadow, out, 0, spacing, &mut column, &mut positions);
    for (sid, agent) in shadow.agents.iter().enumerate() {
        if agent.is_some() && !positions.contains_key(&(sid as u32)) {
            column += spacing;
            positions.insert(sid as u32, (column, -spacing, 0));
        }
    }
    let mut occupied = BTreeMap::new();
    for sid in positions.keys().copied().collect::<Vec<_>>() {
        let mut p = positions[&sid];
        while occupied.contains_key(&p) || occupied.contains_key(&step(p, Dir::S)) {
            p.0 += 2;
        }
        occupied.insert(p, sid);
        positions.insert(sid, p);
    }
    Some(positions)
}

fn route_terminals(grid: &Grid64, start: Terminal, end: Terminal) -> Option<Vec<Pos>> {
    if step(start.cell, start.face) == end.cell && end.face == start.face.opp() { return Some(vec![]); }
    let first = step(start.cell, start.face);
    let last = step(end.cell, end.face);
    if first == last {
        let a = dir_to(first, start.cell)?;
        let b = dir_to(first, end.cell)?;
        return (a != b && grid.is_empty(first)).then_some(vec![first]);
    }
    if !grid.is_empty(first) || !grid.is_empty(last) { return None; }

    let span = manhattan(start.cell, end.cell);
    let mut distances: BTreeMap<Pos, u32> = BTreeMap::new();
    let mut previous: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut heap = BinaryHeap::new();
    distances.insert(first, 0);
    heap.push(std::cmp::Reverse((0u32, first)));
    let mut expansions = 0;
    while let Some(std::cmp::Reverse((cost, p))) = heap.pop() {
        if distances.get(&p) != Some(&cost) { continue; }
        if p == last {
            let mut path = vec![p];
            while path.last().copied() != Some(first) {
                path.push(previous[path.last().unwrap()]);
            }
            path.reverse();
            return Some(path);
        }
        expansions += 1;
        if expansions > 1_000_000 { return None; }
        for d in DIRS {
            let q = step(p, d);
            if !grid.topo.in_bounds(q) || (!grid.is_empty(q) && q != last) { continue; }
            if manhattan(q, start.cell) > 5 * (span + 12) { continue; }
            let neighbor_agent_penalty = DIRS.into_iter().any(|f| {
                matches!(grid.matter(step(q, f)), Some(Matter::Agent { .. } | Matter::Zip { .. }))
                    && step(q, f) != start.cell && step(q, f) != end.cell
            }) as u32 * 100;
            let next_cost = cost + 1 + neighbor_agent_penalty;
            if distances.get(&q).is_none_or(|old| next_cost < *old) {
                distances.insert(q, next_cost);
                previous.insert(q, p);
                heap.push(std::cmp::Reverse((next_cost, q)));
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::oracle::{ap, f2, s};

    fn embedded(term: Term, topo: Topo) -> (Grid64, Net) {
        let mut net = Net::new();
        let root = net.build(&term);
        net.drive(root);
        (embed64(&net, topo), net)
    }

    #[test]
    fn packed_loader_projects_on_both_topologies() {
        for topo in [Topo::Bilayer, Topo::Full3D] {
            for term in [
                ap(s(Term::L), Term::L),
                ap(f2(Term::L, s(Term::L)), f2(Term::L, Term::L)),
                crate::oracle::disp_t(),
            ] {
                let (grid, net) = embedded(term, topo);
                grid.check_projection(&net);
                assert!(grid.cells.values().all(|word| word.unpack().is_ok()));
            }
        }
    }

    #[test]
    fn every_arity_three_agent_has_one_tail_and_one_zipper() {
        let (grid, _) = embedded(ap(f2(Term::L, Term::L), Term::L), Topo::Bilayer);
        for (p, tag) in grid.agents().filter(|(_, tag)| tag.arity() == 3) {
            let Matter::Agent { tail: Some(tail), .. } = grid.matter(p).unwrap() else { unreachable!() };
            let zip = step(p, tail);
            let Matter::Zip { trunk, .. } = grid.matter(zip).unwrap() else {
                panic!("arity-three {tag:?} at {p:?} has no zipper")
            };
            assert_eq!(trunk, tail.opp());
        }
    }

    #[test]
    fn loader_uses_links_and_zippers_without_crossings() {
        let (grid, _) = embedded(crate::oracle::disp_t(), Topo::Full3D);
        for word in grid.cells.values() {
            assert!(!matches!(word.unpack().unwrap().matter, Matter::Cross { .. }));
        }
    }
}
