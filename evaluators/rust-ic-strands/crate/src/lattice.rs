//! Strands on links. Sites of a 2D or 3D grid hold up to K agents; each link between two
//! neighbouring sites carries up to L numbered strands. Strand i of a link is one piece of
//! wire, seen from both of its sites, so a site never needs to name anything elsewhere.
//! Inside a site a switchboard (`mate`) pairs up the ends present: agent ports and strand
//! ends. A wire is therefore a path: agent port, switchboard, strand, switchboard, ...,
//! agent port. Crossing is free: two wires just pass through the same switchboard.
//!
//! Every move reads and writes at most a 2×2 block of sites:
//! - hop: an agent steps to a neighbouring site, eating the strand it walked along and
//!   dragging its other wires one strand longer;
//! - fold: a wire that leaves a site and comes straight back (a U-turn) snaps shut;
//! - flip: a wire's corner turns to the other side of its 2×2 square;
//! - fire: a rewrite happens inside one site once both reactants are in it.
//! Moves are proposed at random and accepted by an energy (wire tension, heavier on
//! principal wires so reactants find each other, plus crowding pressure), Metropolis-style.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::rules::{find_index, End, Tag, ALL_TAGS, RULES};

pub const NONE: u8 = u8::MAX;
/// Ports per agent slot.
pub const ARITY: usize = 3;

#[derive(Clone, Copy, Debug)]
pub struct Params {
    pub w: u32,
    pub h: u32,
    /// 1 for a 2D sheet, more for a 3D block.
    pub depth: u32,
    /// Agents per site.
    pub k: usize,
    /// Strands per link.
    pub lanes: usize,
    /// Energy per strand of a wire, seen from the agent port that would drag it.
    pub w_principal: f64,
    pub w_aux: f64,
    /// Energy per extra agent sharing a site.
    pub crowd: f64,
    /// Short-range repulsion: energy per pair of agents in neighbouring sites.
    pub repel: f64,
    /// Energy per unit of pressure. A rewrite with no room raises its site's pressure to
    /// `pressure_peak`; pressure spreads to neighbours one unit weaker and fades, and agents
    /// drift down it, clearing room around the reaction.
    pub pressure: f64,
    pub pressure_peak: u8,
    pub temp: f64,
    /// Chance a proposal is an agent hop rather than a strand move.
    pub p_hop: f64,
    pub init_fill: usize,
    /// Scale the initial drawing by this, leaving room around every agent.
    pub spread: usize,
    /// Rewrites stay inside a 2×2 block (otherwise: a site and its four neighbours).
    pub block: bool,
    /// Only wanted consumers react. The normalizer, the root and erasers are wanted; a
    /// wanted consumer that reaches the output of another consumer makes it wanted; a fresh
    /// consumer is wanted when its output feeds a wanted reader.
    pub lazy: bool,
    /// In lazy mode, how strongly agents nobody wants feel wire tension (1 = like everyone).
    pub idle_tension: f64,
    pub seed: u64,
}

impl Default for Params {
    fn default() -> Self {
        Params { w: 32, h: 32, depth: 1, k: 8, lanes: 4, w_principal: 3.0, w_aux: 1.0, crowd: 0.5, repel: 0.0, pressure: 0.0, pressure_peak: 6, temp: 0.6,
                 p_hop: 0.5, init_fill: 1, spread: 2, block: false, lazy: false, idle_tension: 1.0, seed: 1 }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Stats {
    pub proposals: u64,
    pub hops: u64,
    pub folds: u64,
    pub flips: u64,
    pub fires: u64,
    pub blocked_fires: u64,
    pub blocked_lanes: u64,
    /// Blocked fires per rule.
    pub blocked_rule: [u64; 26],
    pub strands: u64,
    pub peak_strands: u64,
    pub peak_site: u64,
    /// Physical time: one unit is every live site having proposed once on average.
    pub sweeps: f64,
}

#[inline] fn code(t: Tag) -> u8 { ALL_TAGS.iter().position(|x| *x == t).unwrap() as u8 + 1 }
#[inline] pub fn tag_of(c: u8) -> Tag { ALL_TAGS[c as usize - 1] }

/// Sites a rewrite may place fresh agents in (index 0 is the consumer's), and the fixed
/// route of links between each pair of them.
struct Region {
    sites: Vec<u32>,
    route: Vec<Vec<Vec<(u32, usize)>>>,
}

pub struct Lattice {
    pub p: Params,
    pub faces: usize,
    pub ends: usize,
    /// site*k + slot: 0 empty, else the agent's tag code.
    pub tags: Vec<u8>,
    pub sids: Vec<u32>,
    pub want: Vec<bool>,
    /// site*ends + end: the end it is paired with in the same site, or NONE.
    pub mate: Vec<u8>,
    pub occ: Vec<u8>,
    pub press: Vec<u8>,
    nb: Vec<[u32; 6]>,
    live: Vec<u32>,
    live_pos: Vec<u32>,
    rng: u64,
    pub shadow: Net,
    pub stats: Stats,
    pub out: (u32, usize),
    pub last_op: &'static str,
    infect: std::cell::Cell<Option<(u32, usize)>>,
    /// Check every invariant every this many proposals (0: never).
    pub check_every: u64,
}

impl Lattice {
    pub fn sites(&self) -> u32 { self.p.w * self.p.h * self.p.depth }
    #[inline] fn ae(&self, k: usize, p: usize) -> u8 { (k * ARITY + p) as u8 }
    #[inline] fn se(&self, f: usize, i: usize) -> u8 { (ARITY * self.p.k + f * self.p.lanes + i) as u8 }
    #[inline] fn is_strand(&self, e: u8) -> bool { e != NONE && e as usize >= ARITY * self.p.k }
    #[inline] fn face(&self, e: u8) -> usize { (e as usize - ARITY * self.p.k) / self.p.lanes }
    #[inline] fn lane(&self, e: u8) -> usize { (e as usize - ARITY * self.p.k) % self.p.lanes }
    #[inline] pub fn mate_of(&self, s: u32, e: u8) -> u8 { self.mate[s as usize * self.ends + e as usize] }
    #[inline] fn set(&mut self, s: u32, e: u8, m: u8) { self.mate[s as usize * self.ends + e as usize] = m; }
    #[inline] fn link(&mut self, s: u32, a: u8, b: u8) {
        debug_assert!(a != NONE && b != NONE && a != b, "linking {a} to {b} at site {s}");
        self.set(s, a, b);
        self.set(s, b, a);
    }
    #[inline] pub fn nb(&self, s: u32, f: usize) -> u32 { self.nb[s as usize][f] }
    #[inline] pub fn tag(&self, s: u32, k: usize) -> u8 { self.tags[s as usize * self.p.k + k] }

    fn rand(&mut self) -> u64 {
        self.rng ^= self.rng << 13;
        self.rng ^= self.rng >> 7;
        self.rng ^= self.rng << 17;
        self.rng
    }
    fn unit(&mut self) -> f64 { (self.rand() >> 11) as f64 / (1u64 << 53) as f64 }
    fn below(&mut self, n: usize) -> usize { (self.rand() % n as u64) as usize }

    pub fn new(p: Params) -> Lattice {
        let faces = if p.depth > 1 { 6 } else { 4 };
        let ends = ARITY * p.k + faces * p.lanes;
        assert!(ends < 255, "a site's switchboard has fewer than 255 ends");
        let n = (p.w * p.h * p.depth) as usize;
        let nb = (0..n as u32).map(|s| {
            let (x, y, z) = (s % p.w, (s / p.w) % p.h, s / (p.w * p.h));
            let at = |x: u32, y: u32, z: u32| z * p.w * p.h + y * p.w + x;
            let mut r = [u32::MAX; 6];
            if x + 1 < p.w { r[0] = at(x + 1, y, z); }
            if x > 0 { r[1] = at(x - 1, y, z); }
            if y + 1 < p.h { r[2] = at(x, y + 1, z); }
            if y > 0 { r[3] = at(x, y - 1, z); }
            if z + 1 < p.depth { r[4] = at(x, y, z + 1); }
            if z > 0 { r[5] = at(x, y, z - 1); }
            r
        }).collect();
        Lattice {
            p, faces, ends,
            tags: vec![0; n * p.k],
            sids: vec![u32::MAX; n * p.k],
            want: vec![false; n * p.k],
            mate: vec![NONE; n * ends],
            occ: vec![0; n],
            press: vec![0; n],
            nb,
            live: vec![],
            live_pos: vec![u32::MAX; n],
            rng: p.seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1,
            shadow: Net::new(),
            stats: Stats::default(),
            out: (u32::MAX, 0),
            last_op: "",
            infect: std::cell::Cell::new(None),
            check_every: 0,
        }
    }

    /// Keep the list of sites worth proposing moves at: those holding an agent or a strand.
    fn refresh(&mut self, s: u32) {
        let base = s as usize * self.ends;
        let used = self.occ[s as usize] > 0 || self.mate[base + ARITY * self.p.k..base + self.ends].iter().any(|&m| m != NONE);
        let pos = self.live_pos[s as usize];
        if used && pos == u32::MAX {
            self.live_pos[s as usize] = self.live.len() as u32;
            self.live.push(s);
        } else if !used && pos != u32::MAX {
            let last = *self.live.last().unwrap();
            self.live.swap_remove(pos as usize);
            if last != s { self.live_pos[last as usize] = pos; }
            self.live_pos[s as usize] = u32::MAX;
        }
    }

    fn free_slot(&self, s: u32) -> Option<usize> { (0..self.p.k).find(|&k| self.tag(s, k) == 0) }

    fn free_lanes(&self, s: u32, f: usize, n: usize) -> Option<Vec<usize>> {
        let v: Vec<usize> = (0..self.p.lanes).filter(|&i| self.mate_of(s, self.se(f, i)) == NONE).take(n).collect();
        (v.len() == n).then_some(v)
    }

    fn place(&mut self, s: u32, k: usize, tag: u8, sid: u32) {
        let t = tag_of(tag);
        self.want[s as usize * self.p.k + k] = matches!(t, Tag::Nrm | Tag::Out | Tag::Eps);
        self.tags[s as usize * self.p.k + k] = tag;
        self.sids[s as usize * self.p.k + k] = sid;
        self.occ[s as usize] += 1;
        self.stats.peak_site = self.stats.peak_site.max(self.occ[s as usize] as u64);
    }

    fn remove(&mut self, s: u32, k: usize) {
        self.tags[s as usize * self.p.k + k] = 0;
        self.sids[s as usize * self.p.k + k] = u32::MAX;
        self.occ[s as usize] -= 1;
    }

    fn strand_count(&mut self, d: i64) {
        self.stats.strands = (self.stats.strands as i64 + d) as u64;
        self.stats.peak_strands = self.stats.peak_strands.max(self.stats.strands);
    }

    // ---- loading ------------------------------------------------------------------------

    /// Lay out an abstract net as an HV drawing of its tree (from `Out`): a node's lighter
    /// subtree goes directly below it, its heavier one to the right of that, so every wire
    /// is a straight run of links and no two wires share a link. Coordinates are scaled by
    /// `spread` to leave room around every agent.
    pub fn load(p: Params, net: Net, out: u32) -> Result<Lattice, String> {
        let mut l = Lattice::new(p);
        let n = net.agents.len();
        // Children of each node in the tree hanging from `out`, found depth first.
        let mut parent = vec![u32::MAX; n];
        let mut order = vec![];
        let mut stack = vec![out];
        parent[out as usize] = out;
        while let Some(id) = stack.pop() {
            order.push(id);
            let a = net.get(id);
            for q in 0..a.tag.arity() {
                let (b, _) = a.ports[q].ok_or("open port")?;
                if parent[b as usize] == u32::MAX { parent[b as usize] = id; stack.push(b); }
            }
        }
        let kids = |id: u32| -> Vec<u32> {
            let a = net.get(id);
            (0..a.tag.arity()).filter_map(|q| a.ports[q].map(|(b, _)| b)).filter(|&b| parent[b as usize] == id && b != id).collect()
        };
        // Box sizes bottom up, then positions top down (an HV drawing). A node's children sit
        // directly below it and directly to its right; with two, either the right one starts
        // past the lower one's box (side by side) or the lower one starts under the right
        // one's box (stacked). Each node picks whichever keeps its box squarest.
        let mut size = vec![(0i64, 0i64); n];
        // (below child, right child, stacked)
        let mut arr = vec![(u32::MAX, u32::MAX, false); n];
        let score = |(w, h): (i64, i64)| (w.max(h), w * h);
        for &id in order.iter().rev() {
            let k = kids(id);
            let combine = |below: Option<u32>, right: Option<u32>, stacked: bool| -> (i64, i64) {
                let (bw, bh) = below.map_or((0, 0), |b| size[b as usize]);
                let (rw, rh) = right.map_or((0, 0), |r| size[r as usize]);
                if stacked {
                    ((1 + rw).max(bw).max(1), rh.max(1) + bh)
                } else {
                    (bw.max(1) + rw, (1 + bh).max(rh).max(1))
                }
            };
            let mut options = vec![];
            match k.len() {
                0 => options.push((None, None, false)),
                1 => { options.push((Some(k[0]), None, false)); options.push((None, Some(k[0]), false)); }
                _ => for (b, r) in [(k[0], k[1]), (k[1], k[0])] {
                    options.push((Some(b), Some(r), false));
                    options.push((Some(b), Some(r), true));
                },
            }
            let best = options.into_iter().min_by_key(|&(b, r, st)| score(combine(b, r, st))).unwrap();
            size[id as usize] = combine(best.0, best.1, best.2);
            arr[id as usize] = (best.0.unwrap_or(u32::MAX), best.1.unwrap_or(u32::MAX), best.2);
        }
        let mut pos = vec![(0i64, 0i64); n];
        for &id in &order {
            let (x, y) = pos[id as usize];
            let (below, right, stacked) = arr[id as usize];
            if stacked {
                let rh = size[right as usize].1.max(1);
                pos[right as usize] = (x + 1, y);
                pos[below as usize] = (x, y + rh);
            } else {
                let bw = if below == u32::MAX { 1 } else { size[below as usize].0.max(1) };
                if below != u32::MAX { pos[below as usize] = (x, y + 1); }
                if right != u32::MAX { pos[right as usize] = (x + bw, y); }
            }
        }
        let spread = p.spread.max(1) as i64;
        let (bw, bh) = size[out as usize];
        let (gw, gh) = (p.w as i64, p.h as i64);
        if bw * spread > gw || bh * spread > gh {
            return Err(format!("the drawing is {}x{} sites, the grid {}x{}", bw * spread, bh * spread, gw, gh));
        }
        let (ox, oy) = ((gw - bw * spread) / 2, (gh - bh * spread) / 2);
        let z = (p.depth / 2) as i64;
        let mut at = vec![(u32::MAX, 0usize); n];
        for &id in &order {
            let (x, y) = pos[id as usize];
            let s = (z * gw * gh + (oy + y * spread) * gw + ox + x * spread) as u32;
            let k = l.free_slot(s).ok_or("two agents drawn on one site")?;
            l.place(s, k, code(net.get(id).tag), id);
            at[id as usize] = (s, k);
        }
        l.out = at[out as usize];
        for &id in &order {
            let a = net.get(id);
            for q in 0..a.tag.arity() {
                let (b, r) = a.ports[q].ok_or("open port")?;
                if (b, r as usize) < (id, q) { continue; }
                let (sa, ka) = at[id as usize];
                let (sb, kb) = at[b as usize];
                let (ea, eb) = (l.ae(ka, q), l.ae(kb, r as usize));
                if sa == sb { l.link(sa, ea, eb); } else { l.route(sa, ea, sb, eb)?; }
            }
        }
        for s in 0..l.sites() { l.refresh(s); }
        l.shadow = net;
        l.check_invariants().map_err(|e| format!("after loading: {e}"))?;
        Ok(l)
    }

    /// Shortest path of links with a free lane, from site a to site b; lay strands on it.
    fn route(&mut self, a: u32, ea: u8, b: u32, eb: u8) -> Result<(), String> {
        let n = self.sites() as usize;
        let mut prev = vec![u32::MAX; n];
        let mut q = std::collections::VecDeque::new();
        prev[a as usize] = a;
        q.push_back(a);
        while let Some(s) = q.pop_front() {
            if s == b { break; }
            for f in 0..self.faces {
                let t = self.nb(s, f);
                if t == u32::MAX || prev[t as usize] != u32::MAX { continue; }
                if self.free_lanes(s, f, 1).is_none() { continue; }
                prev[t as usize] = s;
                q.push_back(t);
            }
        }
        if prev[b as usize] == u32::MAX { return Err("no route with free lanes".into()); }
        let mut path = vec![b];
        while *path.last().unwrap() != a { path.push(prev[*path.last().unwrap() as usize]); }
        path.reverse();
        let mut cur = ea;
        for w in path.windows(2) {
            let (s, t) = (w[0], w[1]);
            let f = (0..self.faces).find(|&f| self.nb(s, f) == t).unwrap();
            let i = self.free_lanes(s, f, 1).unwrap()[0];
            self.link(s, cur, self.se(f, i));
            self.strand_count(1);
            cur = self.se(f ^ 1, i);
        }
        self.link(b, cur, eb);
        Ok(())
    }

    // ---- reading the net -------------------------------------------------------------------

    /// Follow a wire from an agent port to the agent port at its other end.
    pub fn follow(&self, mut s: u32, e: u8) -> (u32, usize, usize, usize) {
        let mut m = self.mate_of(s, e);
        let mut len = 0;
        while self.is_strand(m) {
            let f = self.face(m);
            let i = self.lane(m);
            s = self.nb(s, f);
            m = self.mate_of(s, self.se(f ^ 1, i));
            len += 1;
        }
        (s, m as usize / ARITY, m as usize % ARITY, len)
    }

    /// The root moves like any agent; find it.
    pub fn find_out(&self) -> Option<(u32, usize)> {
        let out = code(Tag::Out);
        self.live.iter().find_map(|&s| (0..self.p.k).find(|&k| self.tag(s, k) == out).map(|k| (s, k)))
    }

    pub fn readback(&self) -> Option<rust_ca_lattice::oracle::Term> {
        use rust_ca_lattice::oracle::Term;
        use std::rc::Rc;
        fn go(l: &Lattice, s: u32, k: usize, depth: u32) -> Option<Rc<Term>> {
            if depth > 100_000 { return None; }
            let t = l.tag(s, k);
            if t == 0 { return None; }
            let child = |q: usize| { let (s2, k2, p2, _) = l.follow(s, l.ae(k, q)); if p2 != 0 { None } else { go(l, s2, k2, depth + 1) } };
            Some(Rc::new(match tag_of(t) {
                Tag::L => Term::L,
                Tag::S => Term::S(child(1)?),
                Tag::F => Term::F(child(1)?, child(2)?),
                _ => return None,
            }))
        }
        let (s, k) = self.find_out()?;
        let (s2, k2, p2, _) = self.follow(s, self.ae(k, 0));
        if p2 != 0 { return None; }
        go(self, s2, k2, 0).map(|t| (*t).clone())
    }

    /// The lattice must be the abstract net: same agents, every wire joining the ports the
    /// abstract net joins.
    pub fn check_projection(&self) -> Result<(), String> {
        let mut live = 0;
        for s in 0..self.sites() {
            for k in 0..self.p.k {
                let t = self.tag(s, k);
                if t == 0 { continue; }
                live += 1;
                let sid = self.sids[s as usize * self.p.k + k];
                let a = self.shadow.agents.get(sid as usize).and_then(|a| a.as_ref()).ok_or("lost agent")?;
                if a.tag != tag_of(t) { return Err("tag mismatch".into()); }
                for q in 0..a.tag.arity() {
                    let (s2, k2, p2, _) = self.follow(s, self.ae(k, q));
                    let sid2 = self.sids[s2 as usize * self.p.k + k2];
                    if a.ports[q] != Some((sid2, p2 as u8)) {
                        return Err(format!("{} port {q}: lattice {:?} vs abstract {:?}", a.tag.name(), (sid2, p2), a.ports[q]));
                    }
                }
            }
        }
        if live != self.shadow.live_count() { return Err(format!("{live} agents vs {}", self.shadow.live_count())); }
        Ok(())
    }

    /// Every pairing is mutual, and every strand is used from both of its sites or neither.
    pub fn check_invariants(&self) -> Result<(), String> {
        for s in 0..self.sites() {
            for e in 0..self.ends as u8 {
                let m = self.mate_of(s, e);
                if m == NONE { continue; }
                if self.mate_of(s, m) != e { return Err(format!("site {s}: {e}->{m} not mutual")); }
                if !self.is_strand(e) {
                    let k = e as usize / ARITY;
                    let t = self.tag(s, k);
                    if t == 0 || e as usize % ARITY >= tag_of(t).arity() { return Err(format!("site {s}: dead port {e} paired")); }
                } else {
                    let t = self.nb(s, self.face(e));
                    if t == u32::MAX { return Err(format!("site {s}: strand off the edge")); }
                    if self.mate_of(t, self.se(self.face(e) ^ 1, self.lane(e))) == NONE { return Err(format!("site {s}: strand {e} used on one side only")); }
                }
            }
            for k in 0..self.p.k {
                let t = self.tag(s, k);
                if t == 0 { continue; }
                for q in 0..tag_of(t).arity() {
                    if self.mate_of(s, self.ae(k, q)) == NONE { return Err(format!("site {s}: {} port {q} unpaired", tag_of(t).name())); }
                }
            }
        }
        Ok(())
    }

    /// For each wanted consumer, its principal wire's length; and how many agents there are.
    pub fn demand_report(&self) -> (Vec<usize>, usize) {
        let mut lens = vec![];
        let mut agents = 0;
        for &s in &self.live {
            for k in 0..self.p.k {
                let t = self.tag(s, k);
                if t == 0 { continue; }
                agents += 1;
                if tag_of(t).is_consumer() && self.want[s as usize * self.p.k + k] {
                    let (s2, k2, p2, len) = self.follow(s, self.ae(k, 0));
                    let t2 = self.tag(s2, k2);
                    if std::env::var("WHO").is_ok() {
                        eprintln!("  {} wanted -> {}.{} (wanted {}) len {len}  occ here {} there {}", tag_of(t).name(),
                            if t2 == 0 { "?" } else { tag_of(t2).name() }, p2, self.want[s2 as usize * self.p.k + k2],
                            self.occ[s as usize], self.occ[s2 as usize]);
                    }
                    lens.push(len);
                }
            }
        }
        (lens, agents)
    }

    /// Average strands per wire end over all agent ports (a measure of how stretched wires are).
    pub fn wire_length(&self) -> (u64, u64) {
        let (mut total, mut ends) = (0, 0);
        for s in &self.live {
            for k in 0..self.p.k {
                let t = self.tag(*s, k);
                if t == 0 { continue; }
                for q in 0..tag_of(t).arity() {
                    total += self.follow(*s, self.ae(k, q)).3 as u64;
                    ends += 1;
                }
            }
        }
        (total, ends)
    }

    // ---- moves ------------------------------------------------------------------------------

    fn accept(&mut self, de: f64) -> bool { de <= 0.0 || self.unit() < (-de / self.p.temp).exp() }

    /// Agent k of site s steps across face f.
    fn hop(&mut self, s: u32, k: usize, f: usize) -> bool {
        let t = self.nb(s, f);
        if t == u32::MAX { return false; }
        let Some(k2) = self.free_slot(t) else { return false };
        let a = tag_of(self.tag(s, k)).arity();
        // Where each port's wire goes after the step.
        #[derive(Clone, Copy)]
        enum Plan { SelfLoop(usize), Through(usize), Drag }
        let mut plan = [Plan::Drag; ARITY];
        let mut need = 0;
        let mut de = 0.0;
        let scale = if self.p.lazy && !self.want[s as usize * self.p.k + k] { self.p.idle_tension } else { 1.0 };
        for q in 0..a {
            let m = self.mate_of(s, self.ae(k, q));
            let w = scale * if q == 0 { self.p.w_principal } else { self.p.w_aux };
            if !self.is_strand(m) && m as usize / ARITY == k {
                plan[q] = Plan::SelfLoop(m as usize % ARITY);
            } else if self.is_strand(m) && self.face(m) == f {
                plan[q] = Plan::Through(self.lane(m));
                de -= w;
            } else {
                need += 1;
                de += w;
            }
        }
        // Dragged wires may reuse the strands this step frees (they are relinked after the
        // freed ones are cleared).
        let mut lanes: Vec<usize> = (0..a).filter_map(|q| match plan[q] { Plan::Through(i) => Some(i), _ => None }).collect();
        lanes.extend((0..self.p.lanes).filter(|&i| self.mate_of(s, self.se(f, i)) == NONE));
        if lanes.len() < need { return false; }
        lanes.truncate(need);
        de += self.p.crowd * (self.occ[t as usize] as f64 - (self.occ[s as usize] as f64 - 1.0));
        if self.p.pressure != 0.0 {
            de += self.p.pressure * (self.press[t as usize] as f64 - self.press[s as usize] as f64);
        }
        if self.p.repel != 0.0 {
            // Agents next door to t, after leaving s (which is next door to t), minus those next to s.
            let around = |x: u32| (0..self.faces).map(|g| self.nb(x, g)).filter(|&y| y != u32::MAX).map(|y| self.occ[y as usize] as f64).sum::<f64>();
            de += 2.0 * self.p.repel * (around(t) - 1.0 - around(s));
        }
        if !self.accept(de) { return false; }

        // Where the through-wires land inside t (a wire may come back to another port of the
        // same agent: that becomes a loop in t).
        let mut target = [NONE; ARITY];
        for q in 0..a {
            if let Plan::Through(i) = plan[q] {
                let mt = self.mate_of(t, self.se(f ^ 1, i));
                let back = self.is_strand(mt) && self.face(mt) == (f ^ 1) && {
                    let j = self.lane(mt);
                    (0..a).find(|&r| matches!(plan[r], Plan::Through(x) if x == j))
                }.is_some();
                target[q] = if back {
                    let j = self.lane(mt);
                    let r = (0..a).find(|&r| matches!(plan[r], Plan::Through(x) if x == j)).unwrap();
                    self.ae(k2, r)
                } else { mt };
            }
        }
        let (tag, sid, want) = (self.tag(s, k), self.sids[s as usize * self.p.k + k], self.want[s as usize * self.p.k + k]);
        let mut li = 0;
        let mut news = vec![];
        for q in 0..a {
            match plan[q] {
                Plan::Through(i) => {
                    self.set(s, self.se(f, i), NONE);
                    self.set(t, self.se(f ^ 1, i), NONE);
                    self.strand_count(-1);
                }
                Plan::Drag => {
                    let m = self.mate_of(s, self.ae(k, q));
                    let j = lanes[li];
                    li += 1;
                    news.push((q, m, j));
                }
                Plan::SelfLoop(_) => {}
            }
        }
        for q in 0..a { self.set(s, self.ae(k, q), NONE); }
        self.remove(s, k);
        self.place(t, k2, tag, sid);
        self.want[t as usize * self.p.k + k2] = want;
        for (q, m, j) in news {
            self.link(s, m, self.se(f, j));
            self.link(t, self.se(f ^ 1, j), self.ae(k2, q));
            self.strand_count(1);
        }
        for q in 0..a {
            match plan[q] {
                Plan::SelfLoop(r) => self.link(t, self.ae(k2, q), self.ae(k2, r)),
                Plan::Through(_) => {
                    let m = target[q];
                    self.link(t, self.ae(k2, q), m);
                }
                Plan::Drag => {}
            }
        }
        self.stats.hops += 1;
        self.last_op = "hop";
        self.refresh(s);
        self.refresh(t);
        true
    }

    /// A wire leaving s on face f lane i and coming straight back on lane j snaps shut.
    fn fold(&mut self, s: u32, e: u8) -> bool {
        let (f, i) = (self.face(e), self.lane(e));
        let t = self.nb(s, f);
        let mt = self.mate_of(t, self.se(f ^ 1, i));
        if !self.is_strand(mt) || self.face(mt) != (f ^ 1) { return false; }
        let j = self.lane(mt);
        let (a, b) = (self.mate_of(s, self.se(f, i)), self.mate_of(s, self.se(f, j)));
        if a == self.se(f, j) { return false; }
        for x in [self.se(f, i), self.se(f, j)] { self.set(s, x, NONE); }
        for x in [self.se(f ^ 1, i), self.se(f ^ 1, j)] { self.set(t, x, NONE); }
        self.link(s, a, b);
        self.strand_count(-2);
        self.stats.folds += 1;
        self.last_op = "fold";
        self.refresh(s);
        self.refresh(t);
        true
    }

    /// A wire turning a corner at s (in on face f1, out on face f2, perpendicular) moves to
    /// the opposite corner of the 2×2 square.
    fn flip(&mut self, s: u32, e: u8) -> bool {
        let m = self.mate_of(s, e);
        if !self.is_strand(m) { return false; }
        let (f1, i, f2, j) = (self.face(e), self.lane(e), self.face(m), self.lane(m));
        if f1 / 2 == f2 / 2 { return false; }
        let (x, z) = (self.nb(s, f1), self.nb(s, f2));
        if x == u32::MAX || z == u32::MAX { return false; }
        let w = self.nb(x, f2);
        if w == u32::MAX || w != self.nb(z, f1) { return false; }
        let Some(c) = self.free_lanes(x, f2, 1) else { return false };
        // W is X + f2 = Z + f1, so the step from W to Z is -f1.
        let Some(d) = self.free_lanes(w, f1 ^ 1, 1) else { return false };
        let (c, d) = (c[0], d[0]);
        if !self.accept(0.0) { return false; }
        let a = self.mate_of(x, self.se(f1 ^ 1, i));
        let g = self.mate_of(z, self.se(f2 ^ 1, j));
        self.set(s, e, NONE);
        self.set(s, m, NONE);
        self.set(x, self.se(f1 ^ 1, i), NONE);
        self.set(z, self.se(f2 ^ 1, j), NONE);
        self.link(x, a, self.se(f2, c));
        self.link(w, self.se(f2 ^ 1, c), self.se(f1 ^ 1, d));
        self.link(z, self.se(f1, d), g);
        self.stats.flips += 1;
        self.last_op = "flip";
        for y in [s, x, z, w] { self.refresh(y); }
        true
    }

    /// A consumer in site s whose principal meets a producer's principal, in s or across a
    /// single strand. Returns the consumer slot, the producer slot, and the face the producer
    /// is across (None: same site).
    fn active_pair(&self, s: u32) -> Option<(usize, usize, Option<usize>)> {
        for k in 0..self.p.k {
            let t = self.tag(s, k);
            if t == 0 || !tag_of(t).is_consumer() { continue; }
            if self.p.lazy && !self.want[s as usize * self.p.k + k] { continue; }
            let m = self.mate_of(s, self.ae(k, 0));
            if m == NONE { continue; }
            let (site, m, face) = if self.is_strand(m) {
                let f = self.face(m);
                let n = self.nb(s, f);
                (n, self.mate_of(n, self.se(f ^ 1, self.lane(m))), Some(f))
            } else { (s, m, None) };
            if self.is_strand(m) || m == NONE { continue; }
            let (k2, q) = (m as usize / ARITY, m as usize % ARITY);
            let t2 = self.tag(site, k2);
            if q == 0 && t2 != 0 && tag_of(t2).is_producer() { return Some((k, k2, face)); }
            // A wanted reader touching a pending computation's output wants it in turn (an
            // eraser does not: what it touches is garbage).
            if self.p.lazy && q > 0 && t2 != 0 && tag_of(t2).is_consumer() && tag_of(t) != Tag::Eps {
                self.infect.set(Some((site, k2)));
            }
        }
        None
    }

    /// The rewrite, in site s. Fresh agents take the pair's slots and the site's free slots;
    /// any that do not fit spill into neighbouring sites, wired back across the shared
    /// links. All or nothing: if the room or the lanes are not there, nothing changes.
    fn fire(&mut self, s: u32, kc: usize, kp: usize, via: Option<usize>) -> bool {
        // The producer's site and location: here (0), or across face f (f + 1).
        let (sp, ploc) = match via { None => (s, 0), Some(f) => (self.nb(s, f), f + 1) };
        let (ct, pt) = (tag_of(self.tag(s, kc)), tag_of(self.tag(sp, kp)));
        let ri = find_index(ct, pt).unwrap_or_else(|| panic!("no rule {}·{}", ct.name(), pt.name()));
        let rule = &RULES[ri];
        let n = rule.fresh.len();

        // Each dying aux port is joined to an end in this site (or to another dying aux port)
        // and, by the rule, to a fresh port or dying aux port. Walk each path to its two
        // terminals: fresh ports and ends that stay.
        let mut dying_mate = [[(0usize, NONE); ARITY]; 2];
        for (side, (site, loc, k, t)) in [(s, 0, kc, ct), (sp, ploc, kp, pt)].into_iter().enumerate() {
            for q in 1..t.arity() { dying_mate[side][q] = (loc, self.mate_of(site, self.ae(k, q))); }
        }
        let as_dying = |(loc, m): (usize, u8)| -> Option<End> {
            if self.is_strand(m) { return None; }
            let (k, q) = (m as usize / ARITY, m as usize % ARITY);
            if loc == 0 && k == kc && q > 0 { Some(End::CAux(q as u8)) }
            else if loc == ploc && k == kp && q > 0 { Some(End::PAux(q as u8)) } else { None }
        };
        let partner = |e: End| -> End {
            for (a, b) in rule.wires { if *a == e { return *b; } if *b == e { return *a; } }
            unreachable!()
        };
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        enum T { Fresh(usize, usize), Stay(usize, u8) }
        let walk = |mut e: End| -> T {
            for _ in 0..32 {
                match e {
                    End::Fresh(f, q) => return T::Fresh(f as usize, q as usize),
                    End::CAux(i) | End::PAux(i) => {
                        let side = matches!(e, End::PAux(_)) as usize;
                        let m = dying_mate[side][i as usize];
                        match as_dying(m) { Some(d) => e = partner(d), None => return T::Stay(m.0, m.1) }
                    }
                }
            }
            panic!("vicious circle")
        };
        let mut links: Vec<(T, T)> = vec![];
        for &(a, b) in rule.wires {
            let (ta, tb) = (walk(a), walk(b));
            if !links.iter().any(|&(x, y)| (x, y) == (ta, tb) || (x, y) == (tb, ta)) { links.push((ta, tb)); }
        }
        // Where fresh agents may sit: a small region of sites with fixed routes between them.
        // Plus-shaped: this site and its neighbours (a route between two neighbours passes
        // through here). Block: a 2×2 square holding both reactants, so that every move of
        // the machine, rewrites included, is a function of one 2×2 block.
        let principal_lane = via.map(|f| (f, self.lane(self.mate_of(s, self.ae(kc, 0)))));
        let regions = self.regions(s, sp);
        let link_key = |site: u32, f: usize, nb: u32| if site < nb { (site, f) } else { (nb, f ^ 1) };
        let mut chosen = None;
        for region in regions {
            let nloc = region.sites.len();
            let Some(ploc) = region.sites.iter().position(|&x| x == sp) else { continue };
            let mut slots_at: Vec<Vec<usize>> = region.sites.iter().map(|&x| {
                if x == u32::MAX { vec![] } else { (0..self.p.k).filter(|&k| self.tag(x, k) == 0).collect() }
            }).collect();
            slots_at[0].insert(0, kc);
            slots_at[ploc].insert(0, kp);
            // Free lanes per link of the region (the strand between the pair comes free).
            let mut avail: std::collections::HashMap<(u32, usize), usize> = std::collections::HashMap::new();
            for a in 0..nloc {
                for b in 0..nloc {
                    for &(x, f) in &region.route[a][b] {
                        let y = self.nb(x, f);
                        let key = link_key(x, f, y);
                        avail.entry(key).or_insert_with(|| (0..self.p.lanes).filter(|&i| self.mate_of(x, self.se(f, i)) == NONE).count());
                    }
                }
            }
            if let Some((f, _)) = principal_lane {
                let key = link_key(s, f, sp);
                *avail.entry(key).or_insert(0) += 1;
            }
            let at = |t: T, loc: &[usize]| match t { T::Fresh(f, _) => loc[f], T::Stay(l, _) => if l == 0 { 0 } else { ploc } };
            let need_of = |loc: &[usize]| -> std::collections::HashMap<(u32, usize), usize> {
                let mut need = std::collections::HashMap::new();
                for &(a, b) in &links {
                    let (la, lb) = (at(a, loc), at(b, loc));
                    if la == lb { continue; }
                    for &(x, f) in &region.route[la][lb] {
                        *need.entry(link_key(x, f, self.nb(x, f))).or_insert(0) += 1;
                    }
                }
                need
            };
            let check = |loc: &[usize]| -> Option<usize> {
                let need = need_of(loc);
                need.iter().all(|(k, &v)| v <= *avail.get(k).unwrap_or(&0)).then(|| need.values().sum())
            };
            let mut best: Option<(usize, Vec<usize>)> = None;
            let mut loc = vec![0usize; n];
            let mut used = vec![0usize; nloc];
            fn search(i: usize, n: usize, nloc: usize, loc: &mut Vec<usize>, used: &mut Vec<usize>, slots_at: &[Vec<usize>],
                      check: &dyn Fn(&[usize]) -> Option<usize>, best: &mut Option<(usize, Vec<usize>)>) {
                if i == n {
                    if let Some(cost) = check(loc) {
                        if best.as_ref().map_or(true, |(c, _)| cost < *c) { *best = Some((cost, loc.clone())); }
                    }
                    return;
                }
                for l in 0..nloc {
                    if used[l] < slots_at[l].len() {
                        used[l] += 1;
                        loc[i] = l;
                        search(i + 1, n, nloc, loc, used, slots_at, check, best);
                        used[l] -= 1;
                        if best.as_ref().map_or(false, |(c, _)| *c == 0) { return; }
                    }
                }
            }
            search(0, n, nloc, &mut loc, &mut used, &slots_at, &check, &mut best);
            if let Some((_, loc)) = best {
                let need = need_of(&loc);
                chosen = Some((region, slots_at, loc, need, ploc));
                break;
            }
        }
        let Some((region, slots_at, loc, need, ploc)) = chosen else {
            self.press[s as usize] = self.p.pressure_peak;
            self.stats.blocked_fires += 1;
            self.stats.blocked_rule[ri] += 1;
            return false;
        };
        let mut taken = vec![0usize; region.sites.len()];
        let seats: Vec<(usize, u32, usize)> = loc.iter().map(|&l| {
            let k = slots_at[l][taken[l]];
            taken[l] += 1;
            (l, region.sites[l], k)
        }).collect();
        if let Some((f, i)) = principal_lane {
            self.set(s, self.se(f, i), NONE);
            self.set(sp, self.se(f ^ 1, i), NONE);
            self.strand_count(-1);
        }
        let mut lanes: std::collections::HashMap<(u32, usize), Vec<usize>> = std::collections::HashMap::new();
        for (&(x, f), &v) in &need {
            lanes.insert((x, f), self.free_lanes(x, f, v).expect("checked"));
        }

        let (csid, psid) = (self.sids[s as usize * self.p.k + kc], self.sids[sp as usize * self.p.k + kp]);
        assert_eq!(self.shadow.get(csid).ports[0], Some((psid, 0)), "fired a pair the abstract net does not have");
        let fresh_sids = self.shadow.fire(csid, psid).1;
        for (site, k) in [(s, kc), (sp, kp)] {
            for q in 0..ARITY { self.set(site, self.ae(k, q), NONE); }
            self.remove(site, k);
        }
        // Which fresh consumers are wanted: those whose output feeds the dying consumer's
        // reader (wanted, or it would not have fired) or a wanted fresh consumer.
        let mut wanted = [false; 6];
        for (f, t) in rule.fresh.iter().enumerate() { wanted[f] = matches!(t, Tag::Nrm | Tag::Eps); }
        loop {
            let mut changed = false;
            for &(a, b) in rule.wires {
                for (x, y) in [(a, b), (b, a)] {
                    let End::Fresh(f, p) = x else { continue };
                    let tf = rule.fresh[f as usize];
                    if wanted[f as usize] || !tf.is_consumer() || p == 0 || !crate::polarity_is_source(tf, p as usize) { continue; }
                    let feeds = match y {
                        End::CAux(i) => crate::polarity_is_source(ct, i as usize),
                        End::Fresh(g, 0) => rule.fresh[g as usize].is_consumer() && wanted[g as usize],
                        _ => false,
                    };
                    if feeds { wanted[f as usize] = true; changed = true; }
                }
            }
            if !changed { break; }
        }
        for (f, t) in rule.fresh.iter().enumerate() {
            let (_, site, k) = seats[f];
            self.place(site, k, code(*t), fresh_sids[f]);
            if self.p.lazy { self.want[site as usize * self.p.k + k] |= wanted[f]; }
        }
        for (a, b) in links {
            let end_at = |t: T| match t {
                T::Fresh(f, q) => (seats[f].0, (seats[f].2 * ARITY + q) as u8),
                T::Stay(l, m) => (if l == 0 { 0 } else { ploc }, m),
            };
            let ((la, ea), (lb, eb)) = (end_at(a), end_at(b));
            if la == lb {
                self.link(region.sites[la], ea, eb);
                continue;
            }
            let mut cur = ea;
            let mut site = region.sites[la];
            for &(x, f) in &region.route[la][lb] {
                debug_assert_eq!(x, site);
                let y = self.nb(x, f);
                let key = link_key(x, f, y);
                let i = lanes.get_mut(&key).unwrap().pop().unwrap();
                self.link(x, cur, self.se(f, i));
                self.strand_count(1);
                site = y;
                cur = self.se(f ^ 1, i);
            }
            self.link(site, cur, eb);
        }
        self.stats.fires += 1;
        self.last_op = "fire";
        for &x in &region.sites { if x != u32::MAX { self.refresh(x); } }
        true
    }

    /// The regions a rewrite at s (producer at sp) may use, in the order to try them.
    fn regions(&mut self, s: u32, sp: u32) -> Vec<Region> {
        let mut out = vec![];
        if !self.p.block {
            let mut sites = vec![s];
            for f in 0..self.faces { sites.push(self.nb(s, f)); }
            let n = sites.len();
            let mut route = vec![vec![vec![]; n]; n];
            for f in 0..self.faces {
                if sites[f + 1] == u32::MAX { continue; }
                route[0][f + 1] = vec![(s, f)];
                route[f + 1][0] = vec![(sites[f + 1], f ^ 1)];
                for g in 0..self.faces {
                    if g != f && sites[g + 1] != u32::MAX { route[f + 1][g + 1] = vec![(sites[f + 1], f ^ 1), (s, g)]; }
                }
            }
            out.push(Region { sites, route });
            return out;
        }
        // 2×2 blocks containing s and sp: corners h (= s), hx, hy, d.
        let mut orient = vec![];
        for fx in [0usize, 1] { for fy in [2usize, 3] { orient.push((fx, fy)); } }
        let start = self.below(4);
        for j in 0..4 {
            let (fx, fy) = orient[(start + j) % 4];
            let (hx, hy) = (self.nb(s, fx), self.nb(s, fy));
            if hx == u32::MAX || hy == u32::MAX { continue; }
            let d = self.nb(hx, fy);
            if sp != s && sp != hx && sp != hy { continue; }
            let sites = vec![s, hx, hy, d];
            let step = |a: u32, f: usize| (a, f);
            let mut route = vec![vec![vec![]; 4]; 4];
            route[0][1] = vec![step(s, fx)];
            route[0][2] = vec![step(s, fy)];
            route[1][3] = vec![step(hx, fy)];
            route[2][3] = vec![step(hy, fx)];
            route[0][3] = vec![step(s, fx), step(hx, fy)];
            route[1][2] = vec![step(hx, fx ^ 1), step(s, fy)];
            for a in 0..4 {
                for b in 0..4 {
                    if a < b {
                        let fwd = route[a][b].clone();
                        let mut back = vec![];
                        let mut x = sites[b];
                        for &(_, f) in fwd.iter().rev() {
                            back.push((x, f ^ 1));
                            x = self.nb(x, f ^ 1);
                        }
                        route[b][a] = back;
                    }
                }
            }
            out.push(Region { sites, route });
        }
        out
    }

    // ---- the dynamics -----------------------------------------------------------------------

    /// One proposal at a random live site.
    pub fn propose(&mut self) {
        self.stats.proposals += 1;
        if self.live.is_empty() { return; }
        let pick = self.below(self.live.len());
        let s = self.live[pick];
        if self.p.pressure != 0.0 {
            // Pressure fades, and arrives from neighbours one unit weaker.
            let from_nb = (0..self.faces).map(|f| self.nb(s, f)).filter(|&t| t != u32::MAX)
                .map(|t| self.press[t as usize]).max().unwrap_or(0).saturating_sub(1);
            for f in 0..self.faces {
                let t = self.nb(s, f);
                if t != u32::MAX && self.press[t as usize] > 1 {
                    let nbp = (0..self.faces).map(|g| self.nb(t, g)).filter(|&u| u != u32::MAX)
                        .map(|u| self.press[u as usize]).max().unwrap_or(0).saturating_sub(1);
                    self.press[t as usize] = self.press[t as usize].saturating_sub(1).max(nbp);
                }
            }
            self.press[s as usize] = self.press[s as usize].saturating_sub(1).max(from_nb);
        }
        self.stats.sweeps += 1.0 / self.live.len() as f64;
        if let Some((kc, kp, via)) = self.active_pair(s) {
            if self.fire(s, kc, kp, via) { return; }
        }
        if let Some((site, k)) = self.infect.take() { self.want[site as usize * self.p.k + k] = true; }
        if self.unit() < self.p.p_hop && self.occ[s as usize] > 0 {
            let ks: Vec<usize> = (0..self.p.k).filter(|&k| self.tag(s, k) != 0).collect();
            let k = ks[self.below(ks.len())];
            // Mostly step along the principal wire; sometimes anywhere.
            let m = self.mate_of(s, self.ae(k, 0));
            let f = if self.is_strand(m) && self.unit() < 0.7 { self.face(m) } else { self.below(self.faces) };
            self.hop(s, k, f);
        } else {
            let used: Vec<u8> = (0..self.faces * self.p.lanes).map(|x| (ARITY * self.p.k + x) as u8)
                .filter(|&e| self.mate_of(s, e) != NONE).collect();
            if used.is_empty() { return; }
            let e = used[self.below(used.len())];
            if !self.fold(s, e) { self.flip(s, e); }
        }
    }

    /// Run until the abstract net has no active pair left, or the budget runs out. Returns
    /// whether it finished.
    pub fn run(&mut self, max_proposals: u64) -> bool {
        let mut next_check = 0;
        while self.stats.proposals < max_proposals {
            if self.stats.proposals >= next_check {
                if self.p.lazy && self.readback().is_some() { return true; }
                if self.shadow.active_pair().is_none() { return true; }
                next_check = self.stats.proposals + 50 * self.live.len().max(1) as u64;
            }
            self.propose();
            if self.check_every > 0 && self.stats.proposals % self.check_every == 0 {
                if let Err(e) = self.check_invariants() { panic!("after proposal {} ({}): {e}", self.stats.proposals, self.last_op); }
            }
        }
        (self.p.lazy && self.readback().is_some()) || self.shadow.active_pair().is_none()
    }
}
