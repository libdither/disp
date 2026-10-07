//! Reading the net back, for the player: what the computation on an agent's outputs means right
//! now, as a term, and which agents compute it; and agents picked in the player, followed as they
//! move and rewrite. Only reads the lattice: nothing here changes how a run goes.
//!
//! Each output means a term, by the rules (rules.rs):
//! - values are trees, `L`, `S(x)`, `F(x,y)`; a suspension `P` is the application `@(f,x)`;
//! - an apply's result is `@(operator, argument)`;
//! - a triage on `a` with arms `⟨b,c⟩` is `@(F(a,b),c)`, as `A·F` made it from `@(F(a,b),c)`;
//! - a dispatch on `z` with arms `⟨w,⟨x,b⟩⟩` is `@(F(F(w,x),b),z)`, as `T1·F` made it;
//! - an unpair's outputs are its pair's parts, and a normalizer's result is its input;
//! - a duplicator's outputs are both its input, read once and shared.

use crate::lattice::Lattice;
use crate::polarity_is_source as is_source;
use rust_ca_lattice::net::{Agent, Net, Ref};
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::rules::Tag;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

/// No agent, or no seat.
pub const NOWHERE: u32 = u32::MAX;
/// How deep a term is read; anything deeper is cut.
const DEEPEST: usize = 2000;

/// One node of what an output means, and the agent whose output it is (NOWHERE for the forks a
/// triage or a dispatch only implies).
pub struct Node {
    pub at: u32,
    pub kind: Kind,
}
pub type Rd = Rc<Node>;

pub enum Kind {
    Leaf,
    Stem(Rd),
    Fork(Rd, Rd),
    Apply(Rd, Rd),
    /// The arms of a pending triage or dispatch.
    Pair(Rd, Rd),
    /// A value duplicators hand to several readers, labelled by the first duplicator copying it.
    Shared(u32, Rd),
    /// What an eraser erases.
    Erased(Rd),
    /// Not read: too deep, or not a well-formed piece of net.
    Cut,
}

fn node(at: u32, kind: Kind) -> Rd { Rc::new(Node { at, kind }) }

fn parts(r: &Rd) -> Option<(Rd, Rd)> {
    match &r.kind {
        Kind::Pair(a, b) => Some((a.clone(), b.clone())),
        Kind::Shared(_, x) => parts(x),
        _ => None,
    }
}

fn children(r: &Rd) -> Vec<&Rd> {
    match &r.kind {
        Kind::Stem(x) | Kind::Erased(x) | Kind::Shared(_, x) => vec![x],
        Kind::Fork(x, y) | Kind::Apply(x, y) | Kind::Pair(x, y) => vec![x, y],
        Kind::Leaf | Kind::Cut => vec![],
    }
}

/// Reads outputs of a net back as terms, reading what a duplicator shares once.
pub struct Reader<'a> {
    net: &'a Net,
    shared: HashMap<u32, Rd>,
    pairs: HashMap<u32, Rd>,
    on_path: Vec<bool>,
    depth: usize,
}

impl<'a> Reader<'a> {
    pub fn new(net: &'a Net) -> Self {
        Reader { net, shared: HashMap::new(), pairs: HashMap::new(), on_path: vec![false; net.agents.len()], depth: 0 }
    }

    fn agent(&self, id: u32) -> Option<&'a Agent> { self.net.agents.get(id as usize)?.as_ref() }

    /// What an agent's outputs mean: its value or result; for an unpair the pair it splits, for
    /// an eraser what it erases, and for the root the answer so far.
    pub fn meaning(&mut self, id: u32) -> Rd {
        let Some(a) = self.agent(id) else { return node(id, Kind::Cut) };
        match a.tag {
            Tag::Out | Tag::Unp => self.source(a.ports[0]),
            Tag::Eps => { let x = self.source(a.ports[0]); node(id, Kind::Erased(x)) }
            t => self.source(Some((id, (0..3).find(|&q| is_source(t, q)).unwrap() as u8))),
        }
    }

    /// What an output port carries.
    pub fn source(&mut self, r: Option<Ref>) -> Rd {
        let Some((id, p)) = r else { return node(NOWHERE, Kind::Cut) };
        let Some(a) = self.agent(id) else { return node(id, Kind::Cut) };
        if !is_source(a.tag, p as usize) { return node(id, Kind::Cut); }
        if a.tag == Tag::Dn && self.shared.contains_key(&id) { return self.shared[&id].clone(); }
        if a.tag == Tag::Unp && self.pairs.contains_key(&id) { return self.project(id, p); }
        if self.depth >= DEEPEST || self.on_path[id as usize] { return node(id, Kind::Cut); }
        self.on_path[id as usize] = true;
        self.depth += 1;
        let ports = a.ports;
        let r = match a.tag {
            Tag::L => node(id, Kind::Leaf),
            Tag::S => { let x = self.source(ports[1]); node(id, Kind::Stem(x)) }
            Tag::F => { let x = self.source(ports[1]); let y = self.source(ports[2]); node(id, Kind::Fork(x, y)) }
            Tag::P => { let f = self.source(ports[1]); let x = self.source(ports[2]); node(id, Kind::Apply(f, x)) }
            Tag::Pair => { let x = self.source(ports[1]); let y = self.source(ports[2]); node(id, Kind::Pair(x, y)) }
            Tag::A => { let f = self.source(ports[0]); let x = self.source(ports[1]); node(id, Kind::Apply(f, x)) }
            Tag::T1 => {
                let a = self.source(ports[0]);
                match parts(&self.source(ports[1])) {
                    Some((b, c)) => node(id, Kind::Apply(node(NOWHERE, Kind::Fork(a, b)), c)),
                    None => node(id, Kind::Cut),
                }
            }
            Tag::Sel => {
                let z = self.source(ports[0]);
                match parts(&self.source(ports[1])).and_then(|(w, rest)| Some((w, parts(&rest)?))) {
                    Some((w, (x, b))) => node(id, Kind::Apply(node(NOWHERE, Kind::Fork(node(NOWHERE, Kind::Fork(w, x)), b)), z)),
                    None => node(id, Kind::Cut),
                }
            }
            Tag::Nrm => self.source(ports[0]),
            Tag::Dn => {
                let x = self.source(ports[0]);
                let shared = match &x.kind { Kind::Shared(l, y) => node(id, Kind::Shared(*l, y.clone())), _ => node(id, Kind::Shared(id, x)) };
                self.shared.insert(id, shared.clone());
                shared
            }
            Tag::Unp => {
                let pair = self.source(ports[0]);
                self.pairs.insert(id, pair);
                self.project(id, p)
            }
            Tag::Eps | Tag::Out => node(id, Kind::Cut),
        };
        self.depth -= 1;
        self.on_path[id as usize] = false;
        r
    }

    fn project(&self, unp: u32, p: u8) -> Rd {
        match parts(&self.pairs[&unp]) {
            Some((x, y)) => if p == 1 { x } else { y },
            None => node(unp, Kind::Cut),
        }
    }
}

/// The term an output means, when all of it was read and it is not a pair.
pub fn term(r: &Rd) -> Option<Term> {
    fn go(r: &Rd, memo: &mut HashMap<u32, Rc<Term>>) -> Option<Rc<Term>> {
        Some(match &r.kind {
            Kind::Leaf => Rc::new(Term::L),
            Kind::Stem(x) => Rc::new(Term::S(go(x, memo)?)),
            Kind::Fork(x, y) => Rc::new(Term::F(go(x, memo)?, go(y, memo)?)),
            Kind::Apply(f, x) => Rc::new(Term::Ap(go(f, memo)?, go(x, memo)?)),
            Kind::Shared(l, x) => {
                if let Some(t) = memo.get(l) { return Some(t.clone()); }
                let t = go(x, memo)?;
                memo.insert(*l, t.clone());
                t
            }
            Kind::Pair(..) | Kind::Erased(_) | Kind::Cut => return None,
        })
    }
    go(r, &mut HashMap::new()).map(|t| (*t).clone())
}

/// The term as text: `L`, `S(x)`, `F(x,y)` and `@(f,x)` as the oracle prints them, `<x,y>` for a
/// pair, `~(x)` for what an eraser erases, `#n=x` for a value shared by duplicators where it is
/// first read and `#n` where it is read again, `…` for whatever lies past the first `budget` nodes,
/// breadth first, and `?` for what could not be read. Also the agent of each node (each `#n=`,
/// `#n`, `…` and `?` too), in the order the text names them.
pub fn text(r: &Rd, budget: usize) -> (String, Vec<u32>) {
    let key = |r: &Rd| Rc::as_ptr(r) as usize;
    let mut kept = HashSet::new();
    let mut opened = HashSet::new();
    let mut queue = VecDeque::from([r.clone()]);
    while let Some(n) = queue.pop_front() {
        if let Kind::Shared(l, x) = &n.kind {
            if opened.insert(*l) { queue.push_back(x.clone()); }
            continue;
        }
        if kept.len() >= budget { break; }
        kept.insert(key(&n));
        queue.extend(children(&n).into_iter().cloned());
    }
    fn count(r: &Rd, kept: &HashSet<usize>, uses: &mut HashMap<u32, usize>) {
        if let Kind::Shared(l, x) = &r.kind {
            let n = uses.entry(*l).or_insert(0);
            *n += 1;
            if *n == 1 { count(x, kept, uses); }
        } else if kept.contains(&(Rc::as_ptr(r) as usize)) {
            for c in children(r) { count(c, kept, uses); }
        }
    }
    let mut uses = HashMap::new();
    count(r, &kept, &mut uses);
    struct Out<'a> { s: String, at: Vec<u32>, kept: &'a HashSet<usize>, uses: &'a HashMap<u32, usize>, labels: HashMap<u32, usize> }
    fn put(r: &Rd, o: &mut Out) {
        if let Kind::Shared(l, x) = &r.kind {
            if o.uses[l] < 2 { return put(x, o); }
            o.at.push(r.at);
            if let Some(k) = o.labels.get(l) { o.s.push_str(&format!("#{k}")); return; }
            let k = o.labels.len() + 1;
            o.labels.insert(*l, k);
            o.s.push_str(&format!("#{k}="));
            return put(x, o);
        }
        o.at.push(r.at);
        if !o.kept.contains(&(Rc::as_ptr(r) as usize)) { o.s.push('…'); return; }
        let (open, close) = match &r.kind {
            Kind::Leaf => { o.s.push('L'); return; }
            Kind::Cut => { o.s.push('?'); return; }
            Kind::Stem(_) => ("S(", ")"),
            Kind::Fork(..) => ("F(", ")"),
            Kind::Apply(..) => ("@(", ")"),
            Kind::Pair(..) => ("<", ">"),
            Kind::Erased(_) => ("~(", ")"),
            Kind::Shared(..) => unreachable!(),
        };
        o.s.push_str(open);
        for (i, c) in children(r).into_iter().enumerate() {
            if i > 0 { o.s.push(','); }
            put(c, o);
        }
        o.s.push_str(close);
    }
    let mut o = Out { s: String::new(), at: vec![], kept: &kept, uses: &uses, labels: HashMap::new() };
    put(r, &mut o);
    (o.s, o.at)
}

/// The agents an agent's outputs depend on: it, and whatever feeds its inputs, recursively.
pub fn subtree(net: &Net, root: u32) -> Vec<u32> {
    let mut seen = HashSet::new();
    let mut stack = vec![root];
    let mut out = vec![];
    while let Some(id) = stack.pop() {
        let Some(Some(a)) = net.agents.get(id as usize) else { continue };
        if !seen.insert(id) { continue; }
        out.push(id);
        for q in 0..a.tag.arity() {
            if let (false, Some((b, _))) = (is_source(a.tag, q), a.ports[q]) { stack.push(b); }
        }
    }
    out
}

/// Every agent on the lattice and what each of its ports is wired to, for the player's drawings of
/// the net: [id, seat, tag, far end of port 0, of port 1, of port 2] for each, a far end as
/// id × 4 + port (NOWHERE if unwired). An id stays an agent's as it moves, until the net is renumbered.
pub fn wires(l: &Lattice) -> Vec<u32> {
    let mut v = vec![];
    for &s in l.live() {
        for k in 0..l.ks {
            let i = s as usize * l.ks + k;
            if l.tags[i] == 0 { continue; }
            let Some(a) = live(&l.shadow, l.sids[i]) else { continue };
            v.extend([l.sids[i], i as u32, l.tags[i] as u32]);
            v.extend(a.ports.iter().map(|r| r.map_or(NOWHERE, |(b, p)| b * 4 + p as u32)));
        }
    }
    v
}

/// Applications and the arms of a choice: their inputs start segments of their own.
pub fn splits(t: Tag) -> bool { matches!(t, Tag::P | Tag::Pair | Tag::A | Tag::T1 | Tag::Sel) }

/// The segments of the term the root computes, as `player/net.js` `segment` cuts them: a spanning
/// tree over inputs from the root, first visit wins, in which every application's inputs each
/// start a segment and every other agent's stay in its own. Each agent's segment by id (NOWHERE:
/// garbage), and each segment's parent (NOWHERE for the root's) and depth.
pub fn segments(net: &Net) -> (Vec<u32>, Vec<(u32, u32)>) {
    let n = net.agents.len();
    let mut seg = vec![NOWHERE; n];
    let mut segs: Vec<(u32, u32)> = vec![];
    let mut stack: Vec<(u32, u32)> = (0..n as u32).rev().filter(|&i| live(net, i).is_some_and(|a| a.tag == Tag::Out)).map(|i| (i, NOWHERE)).collect();
    while let Some((i, from)) = stack.pop() {
        if seg[i as usize] != NOWHERE { continue; }
        let Some(a) = live(net, i) else { continue };
        seg[i as usize] = match from {
            NOWHERE => { segs.push((NOWHERE, 0)); segs.len() as u32 - 1 }
            p if splits(live(net, p).unwrap().tag) => { let g = seg[p as usize]; segs.push((g, segs[g as usize].1 + 1)); segs.len() as u32 - 1 }
            p => seg[p as usize],
        };
        let kids: Vec<u32> = (0..a.tag.arity()).filter(|&q| !is_source(a.tag, q)).filter_map(|q| a.ports[q].map(|r| r.0)).collect();
        for &c in kids.iter().rev() { if seg[c as usize] == NOWHERE { stack.push((c, i)); } }
    }
    (seg, segs)
}

/// How mixed the segments are on the lattice, for each depth in `depths` (segments cut at that
/// depth, deeper ones counted as their ancestor there): of the pairs of agents in one site or in
/// neighbouring sites, the share in different segments, over that share if the same agents were
/// scattered at random (1: as mixed as chance, 0: every segment apart). Garbage is left out.
pub fn mixing(l: &Lattice, depths: &[u32]) -> Vec<f64> {
    let (seg, segs) = segments(&l.shadow);
    let at = |s: u32, k: usize| { let i = s as usize * l.ks + k; if l.tags[i] == 0 { NOWHERE } else { seg.get(l.sids[i] as usize).copied().unwrap_or(NOWHERE) } };
    depths.iter().map(|&d| {
        let cut = |mut g: u32| { while segs[g as usize].1 > d { g = segs[g as usize].0; } g };
        let (mut pairs, mut differ, mut count) = (0u64, 0u64, std::collections::HashMap::<u32, u64>::new());
        for &s in l.live() {
            for k in 0..l.ks {
                let g = at(s, k);
                if g == NOWHERE { continue; }
                let g = cut(g);
                *count.entry(g).or_insert(0) += 1;
                let mut meet = |h: u32| { if h != NOWHERE { pairs += 1; differ += (cut(h) != g) as u64; } };
                for k2 in k + 1..l.ks { meet(at(s, k2)); }
                for f in (0..l.faces).step_by(2) {
                    let t = l.nb(s, f);
                    if t != u32::MAX { for k2 in 0..l.ks { meet(at(t, k2)); } }
                }
            }
        }
        let n = count.values().sum::<u64>() as f64;
        let same = count.values().map(|&c| (c * c.saturating_sub(1)) as f64).sum::<f64>() / (n * (n - 1.0)).max(1.0);
        if pairs == 0 || same >= 1.0 { 0.0 } else { differ as f64 / pairs as f64 / (1.0 - same) }
    }).collect()
}

/// Where each agent of the abstract net sits on the lattice (site × slots + slot), by its id.
pub fn seats(l: &Lattice) -> Vec<u32> {
    let mut at = vec![NOWHERE; l.shadow.agents.len()];
    for &s in l.live() {
        for k in 0..l.ks {
            let i = s as usize * l.ks + k;
            if l.tags[i] != 0 && (l.sids[i] as usize) < at.len() { at[l.sids[i] as usize] = i as u32; }
        }
    }
    at
}

/// Agents picked in the player, each standing for the computation on its outputs (or on one of
/// them), followed as agents move and rewrite.
#[derive(Default)]
pub struct Selection {
    pub roots: Vec<Root>,
    /// The last look, kept when a GPU may hand back the run renumbered.
    last: Option<Look>,
}

/// The net as it was, where its agents sat and when.
struct Look {
    agents: Vec<Option<Agent>>,
    seats: Vec<u32>,
    clocks: f64,
}

#[derive(Clone, Debug)]
pub struct Root {
    pub id: u32,
    pub tag: Tag,
    /// The output it stands for, or None for all of them.
    pub out: Option<u8>,
    /// Its ports at the last look, and the tags of the agents there.
    ports: [Option<Ref>; 3],
    next: [Option<Tag>; 3],
    /// What reads on from its readers that only pass its value on (duplicators, normalizers and
    /// pairs), in case both are rewritten between looks.
    beyond: Vec<Ref>,
}

/// What became of a root since the last look.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Fate {
    /// Rewritten: the computation on its output goes on in another agent.
    Rewritten,
    /// A suspended application was forced: it goes on in the apply that computes it.
    Forced,
    /// What it made was read by a rewrite or erased, so there is nothing left to follow.
    Consumed,
    /// Not found again after a GPU handed back the run renumbered.
    Lost,
}

/// A root that is not where it was: its tag, what became of it, and the other agent involved
/// (what it became, or what consumed it).
#[derive(Clone, Copy, Debug)]
pub struct Report {
    pub was: Tag,
    pub fate: Fate,
    pub other: Option<Tag>,
}

fn live(net: &Net, id: u32) -> Option<&Agent> { net.agents.get(id as usize)?.as_ref() }

fn outputs(tag: Tag, out: Option<u8>) -> Vec<usize> {
    match out { Some(p) => vec![p as usize], None => (0..3).filter(|&q| is_source(tag, q)).collect() }
}

fn root(net: &Net, id: u32, out: Option<u8>) -> Option<Root> {
    let a = live(net, id)?;
    let beyond = outputs(a.tag, out).into_iter().filter_map(|q| a.ports[q]).flat_map(|(b, f)| {
        let Some(b) = live(net, b) else { return vec![] };
        match b.tag {
            Tag::Dn | Tag::Nrm => (1..3).filter(|&p| is_source(b.tag, p)).filter_map(|p| b.ports[p]).collect(),
            // A pair's part goes on to whatever reads that part of the unpair splitting it.
            Tag::Pair => b.ports[0].and_then(|(u, _)| live(net, u)).filter(|u| u.tag == Tag::Unp).and_then(|u| u.ports[f as usize]).into_iter().collect(),
            _ => vec![],
        }
    }).collect();
    Some(Root { id, tag: a.tag, out, ports: a.ports, next: a.ports.map(|p| p.and_then(|(b, _)| live(net, b)).map(|b| b.tag)), beyond })
}

impl Root {
    /// What it stands for now.
    pub fn meaning(&self, net: &Net) -> Rd {
        let mut r = Reader::new(net);
        match self.out { Some(p) => r.source(Some((self.id, p))), None => r.meaning(self.id) }
    }
}

impl Selection {
    /// Remember the net as it is, as every agent is about to be numbered afresh (a saved state put
    /// back in its place), so the roots are found again by `rematch`.
    pub fn remember(&mut self, l: &Lattice) {
        if !self.roots.is_empty() { self.last = Some(Look { agents: l.shadow.agents.clone(), seats: seats(l), clocks: l.stats.clocks }); }
    }

    /// Pick an agent, or let it go if it is picked. Returns whether it is picked now.
    pub fn toggle(&mut self, net: &Net, id: u32) -> bool {
        if let Some(i) = self.roots.iter().position(|r| r.id == id) { self.roots.remove(i); return false; }
        match root(net, id, None) { Some(r) => { self.roots.push(r); true } None => false }
    }

    /// Find the roots again. A root rewritten since the last look goes on as whatever now feeds
    /// the readers of its outputs (or, when they only passed it on and were rewritten too, their
    /// readers), a forced suspension as the apply computing it; one consumed is dropped.
    /// `renumbered`: a GPU handed back the run since the last look, so every agent has a new id
    /// and is found again by `rematch`; one not found is lost. `keep`: remember this look, as such
    /// a hand-back may come next. Returns what happened to roots not where they were.
    pub fn follow(&mut self, l: &Lattice, renumbered: bool, keep: bool) -> Vec<Report> {
        let net = &l.shadow;
        let map = if renumbered { self.last.as_ref().map(|old| rematch(old, l)) } else { None };
        let now = |id: u32| -> Option<u32> {
            match &map {
                Some(m) => m.get(id as usize).copied().filter(|&n| n != NOWHERE),
                None if renumbered => None,
                None => live(net, id).map(|_| id),
            }
        };
        let mut roots: Vec<Root> = vec![];
        let mut reports = vec![];
        for r in &self.roots {
            if renumbered && map.is_none() { reports.push(Report { was: r.tag, fate: Fate::Lost, other: None }); continue; }
            if let Some(n) = now(r.id).filter(|&n| live(net, n).map(|a| a.tag) == Some(r.tag)) {
                roots.extend(root(net, n, r.out));
                continue;
            }
            let outs = outputs(r.tag, r.out);
            let now_read = |(reader, port): Ref| live(net, now(reader)?)?.ports[port as usize];
            let mut next: Vec<Ref> = outs.iter().filter_map(|&q| now_read(r.ports[q]?)).collect();
            let mut fate = Fate::Rewritten;
            if next.is_empty() && r.tag == Tag::P {
                let forced = r.ports[1].and_then(now_read);
                if let Some((a, 0)) = forced.filter(|&(a, _)| live(net, a).map(|a| a.tag) == Some(Tag::A)) { next.push((a, 2)); fate = Fate::Forced; }
            }
            if next.is_empty() { next = r.beyond.iter().filter_map(|&b| now_read(b)).collect(); }
            let became: Vec<Root> = next.iter().filter_map(|&(id, p)| root(net, id, Some(p))).collect();
            match became.first() {
                Some(b) => reports.push(Report { was: r.tag, fate, other: Some(b.tag) }),
                None if renumbered => reports.push(Report { was: r.tag, fate: Fate::Lost, other: None }),
                None => reports.push(Report { was: r.tag, fate: Fate::Consumed, other: outs.first().and_then(|&q| r.next[q]) }),
            }
            roots.extend(became);
        }
        let mut seen = HashSet::new();
        roots.retain(|r| seen.insert((r.id, r.out)));
        self.roots = roots;
        self.last = keep.then(|| Look { agents: net.agents.clone(), seats: seats(l), clocks: l.stats.clocks });
        reports
    }
}

/// Which new agent each old agent is, after a GPU handed back the run with every agent
/// renumbered. Agents are told apart by their neighbourhoods (tags and ports out to a few wires),
/// not by seats, as look-alikes move into each other's seats. Matched: the root; then, widest
/// first, agents whose neighbourhood out to 8, 6, 4 or 3 wires is like no other agent's in either
/// net; and from each match, every agent joined to it by the same ports with its neighbours' tags
/// unchanged. Below 8 wires, and when spreading, a match must also lie within reach: an agent
/// moves at most one site a clock.
fn rematch(old: &Look, l: &Lattice) -> Vec<u32> {
    let new = &l.shadow.agents;
    let near = (neighbourhood(&old.agents, 1), neighbourhood(new, 1));
    let seat = seats(l);
    let xyz = |s: u32| { let (w, h, s) = (l.p.w as i64, l.p.h as i64, (s as usize / l.ks) as i64); [s % w, s / w % h, s / (w * h)] };
    let reach = (l.stats.clocks - old.clocks).abs() as i64 + 2;
    let within = |o: u32, n: u32| match (old.seats.get(o as usize), seat.get(n as usize)) {
        (Some(&a), Some(&b)) if a != NOWHERE && b != NOWHERE => { let (a, b) = (xyz(a), xyz(b)); (0..3).map(|i| (a[i] - b[i]).abs()).sum::<i64>() <= reach }
        _ => false,
    };
    let (mut map, mut back) = (vec![NOWHERE; old.agents.len()], vec![NOWHERE; new.len()]);
    let mut stack = vec![];
    if let (Some(o), Some((s, k))) = (old.agents.iter().position(|a| matches!(a, Some(a) if a.tag == Tag::Out)), l.find_out()) {
        let n = l.sids[s as usize * l.ks + k];
        (map[o], back[n as usize]) = (n, o as u32);
        stack.push((o as u32, n));
    }
    for r in [8, 6, 4, 3] {
        let far = (neighbourhood(&old.agents, r), neighbourhood(new, r));
        let mut alike: HashMap<u64, [(u32, u32); 2]> = HashMap::new();
        for (side, h) in [&far.0, &far.1].into_iter().enumerate() {
            for (id, &x) in h.iter().enumerate().filter(|&(_, &x)| x != 0) {
                let e = &mut alike.entry(x).or_insert([(0, NOWHERE); 2])[side];
                *e = (e.0 + 1, id as u32);
            }
        }
        for [(n0, o), (n1, n)] in alike.into_values() {
            if (n0, n1) == (1, 1) && map[o as usize] == NOWHERE && back[n as usize] == NOWHERE && (r == 8 || within(o, n)) {
                (map[o as usize], back[n as usize]) = (n, o);
                stack.push((o, n));
            }
        }
        while let Some((o, n)) = stack.pop() {
            let (a, b) = (old.agents[o as usize].as_ref().unwrap().ports, new[n as usize].as_ref().unwrap().ports);
            for q in 0..3 {
                let (Some((po, pp)), Some((pn, pq))) = (a[q], b[q]) else { continue };
                if pp == pq && near.0[po as usize] == near.1[pn as usize] && map[po as usize] == NOWHERE && back[pn as usize] == NOWHERE && within(po, pn) {
                    (map[po as usize], back[pn as usize]) = (pn, po);
                    stack.push((po, pn));
                }
            }
        }
    }
    map
}

/// Each agent's neighbourhood out to `r` wires (its tag, and its partners' ports and
/// neighbourhoods out to r - 1), hashed; 0 for the dead.
fn neighbourhood(v: &[Option<Agent>], r: usize) -> Vec<u64> {
    let mix = |mut x: u64| { x ^= x >> 30; x = x.wrapping_mul(0xbf58476d1ce4e5b9); x ^= x >> 27; x = x.wrapping_mul(0x94d049bb133111eb); x ^ x >> 31 };
    let mut h: Vec<u64> = v.iter().map(|a| a.as_ref().map_or(0, |a| mix(a.tag as u64 + 1))).collect();
    for _ in 0..r {
        h = v.iter().enumerate().map(|(i, a)| {
            let Some(a) = a else { return 0 };
            a.ports.iter().enumerate().fold(h[i], |x, (q, p)| match p {
                Some((b, bp)) => mix(x ^ mix(((q as u64) << 8 | *bp as u64) + 1).wrapping_add(h[*b as usize].rotate_left(q as u32 * 21 + 7))),
                None => x,
            })
        }).collect();
    }
    h
}
