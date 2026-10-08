//! Memoization as a radius (README "Memo radius"): equal computations closer than r sites on the
//! lattice are merged, one computing and the other reading a copy of its result through a fresh
//! duplicator; farther ones are both computed. Equal means equal terms as `readback` reads them,
//! duplicators transparent, so twins at different stages of evaluation are missed (or, with
//! `Params::memo_names`, equal names fixed when agents are made, `Names`). The detector is central
//! (it hashes the whole abstract net) and the merge is surgery on the lattice, kept inside one
//! 2×2×2 block with `Params::memo_local` but still not a chip's move, so only the CPU runs it.

use super::{code, Lattice, ARITY, GARBAGE, NONE};
use crate::polarity_is_source as is_source;
use rust_ca_lattice::net::{Net, Ref};
use rust_ca_lattice::oracle::{self, Fuel, Term};
use rust_ca_lattice::rules::Tag;
use std::collections::HashMap;
use std::rc::Rc;

pub const LEAF: u8 = 0;
pub const STEM: u8 = 1;
pub const FORK: u8 = 2;
pub const APPLY: u8 = 3;
pub const PAIR: u8 = 4;
/// Not read: a cycle, a missing wire, or a triage without its arms. Every cut is unlike any other.
pub const CUT: u8 = 5;

const UNSEEN: u32 = u32::MAX;
const BUSY: u32 = u32::MAX - 1;

/// Terms interned: each distinct term one number, so equal terms are equal numbers.
#[derive(Default)]
pub struct Terms {
    /// Each term's kind and parts.
    pub nodes: Vec<(u8, u32, u32)>,
    ids: HashMap<(u8, u32, u32), u32>,
    size: Vec<u64>,
    /// The memo key of a term (`key`), once worked out.
    keys: HashMap<u32, u32>,
}

impl Terms {
    pub fn intern(&mut self, n: (u8, u32, u32)) -> u32 {
        let (nodes, size) = (&mut self.nodes, &mut self.size);
        let mut add = || {
            let s = match n.0 { LEAF | CUT => 1, STEM => 1 + size[n.1 as usize], _ => (1 + size[n.1 as usize]).saturating_add(size[n.2 as usize]) };
            nodes.push(n);
            size.push(s);
            nodes.len() as u32 - 1
        };
        if n.0 == CUT { return add(); }
        *self.ids.entry(n).or_insert_with(add)
    }
    fn cut(&mut self) -> u32 { self.intern((CUT, 0, 0)) }
    /// Nodes in the term written out as a tree (saturating): a strict part is always smaller.
    pub fn size(&self, t: u32) -> u64 { self.size[t as usize] }
    pub fn has_cut(&self, t: u32) -> bool {
        let mut stack = vec![t];
        let mut seen = std::collections::HashSet::new();
        while let Some(x) = stack.pop() {
            if !seen.insert(x) { continue; }
            let (k, a, b) = self.nodes[x as usize];
            match k { CUT => return true, LEAF => {} STEM => stack.push(a), _ => { stack.push(a); stack.push(b); } }
        }
        false
    }

    /// What every output of the net means, by agent and port (UNSEEN where nothing comes out).
    pub fn of_net(&mut self, net: &Net) -> Vec<[u32; 3]> {
        let mut at = vec![[UNSEEN; 3]; net.agents.len()];
        for (id, a) in net.agents.iter().enumerate() {
            let Some(a) = a else { continue };
            for p in 0..3 { if is_source(a.tag, p) { self.source(net, &mut at, (id as u32, p as u8)); } }
        }
        at
    }

    /// What output `r` carries, reading what feeds it first (no recursion: inputs can run deep).
    fn source(&mut self, net: &Net, at: &mut [[u32; 3]], r: Ref) -> u32 {
        let mut stack = vec![r];
        while let Some(&(id, p)) = stack.last() {
            let (i, q) = (id as usize, p as usize);
            let state = at[i][q];
            if state != UNSEEN && state != BUSY { stack.pop(); continue; }
            let Some(a) = net.agents[i].as_ref().filter(|a| is_source(a.tag, q)) else { at[i][q] = self.cut(); stack.pop(); continue };
            let inputs: &[usize] = match a.tag {
                Tag::L => &[],
                Tag::S => &[1],
                Tag::F | Tag::P | Tag::Pair => &[1, 2],
                Tag::A | Tag::T1 | Tag::Sel => &[0, 1],
                Tag::Sup => &[],
                _ => &[0],
            };
            if state == UNSEEN {
                at[i][q] = BUSY;
                let mut waiting = false;
                for &x in inputs {
                    if let Some((b, bp)) = a.ports[x] { if at[b as usize][bp as usize] == UNSEEN { stack.push((b, bp)); waiting = true; } }
                }
                if waiting { continue; }
            }
            stack.pop();
            let mut v = [0u32; 3];
            for &x in inputs {
                v[x] = match a.ports[x] { Some((b, bp)) if at[b as usize][bp as usize] < BUSY => at[b as usize][bp as usize], _ => self.cut() };
            }
            let pair = |t: &Terms, x: u32| { let n = t.nodes[x as usize]; (n.0 == PAIR).then_some((n.1, n.2)) };
            at[i][q] = match a.tag {
                Tag::L => self.intern((LEAF, 0, 0)),
                Tag::S => self.intern((STEM, v[1], 0)),
                Tag::F => self.intern((FORK, v[1], v[2])),
                Tag::P => self.intern((APPLY, v[1], v[2])),
                Tag::Pair => self.intern((PAIR, v[1], v[2])),
                Tag::A => self.intern((APPLY, v[0], v[1])),
                Tag::T1 => match pair(self, v[1]) {
                    Some((b, c)) => { let f = self.intern((FORK, v[0], b)); self.intern((APPLY, f, c)) }
                    None => self.cut(),
                },
                Tag::Sel => match pair(self, v[1]).and_then(|(w, rest)| Some((w, pair(self, rest)?))) {
                    Some((w, (x, b))) => { let wx = self.intern((FORK, w, x)); let f = self.intern((FORK, wx, b)); self.intern((APPLY, f, v[0])) }
                    None => self.cut(),
                },
                // A superposition, and a duplicator copying for one, mean different things in different
                // universes: never merged.
                Tag::Sup => self.cut(),
                Tag::Dn if a.label != 0 => self.cut(),
                Tag::Nrm | Tag::Dn => v[0],
                Tag::Unp => match pair(self, v[0]) { Some((x, y)) => if q == 1 { x } else { y }, None => self.cut() },
                Tag::Eps | Tag::Out => self.cut(),
            };
        }
        at[r.0 as usize][r.1 as usize]
    }

    /// The term as the oracle's, if it has no cut.
    pub fn term(&self, t: u32, memo: &mut HashMap<u32, Rc<Term>>) -> Option<Rc<Term>> {
        if let Some(x) = memo.get(&t) { return Some(x.clone()); }
        let (k, a, b) = self.nodes[t as usize];
        let x = Rc::new(match k {
            LEAF => Term::L,
            STEM => Term::S(self.term(a, memo)?),
            FORK => Term::F(self.term(a, memo)?, self.term(b, memo)?),
            APPLY => Term::Ap(self.term(a, memo)?, self.term(b, memo)?),
            _ => return None,
        });
        memo.insert(t, x.clone());
        Some(x)
    }
    fn of_term(&mut self, t: &Term) -> u32 {
        match t {
            Term::L => self.intern((LEAF, 0, 0)),
            Term::S(a) => { let a = self.of_term(a); self.intern((STEM, a, 0)) }
            Term::F(a, b) => { let a = self.of_term(a); let b = self.of_term(b); self.intern((FORK, a, b)) }
            Term::Ap(a, b) => { let a = self.of_term(a); let b = self.of_term(b); self.intern((APPLY, a, b)) }
        }
    }

    /// What a memo table keyed by `apply(f, x)` would see: an application with its function and
    /// argument worked out to values (as the eager evaluator's memo does), anything else worked
    /// out. None if a part has a cut or takes more than `fuel` steps.
    pub fn key(&mut self, t: u32, fuel: u64) -> Option<u32> {
        if let Some(&k) = self.keys.get(&t) { return (k != UNSEEN).then_some(k); }
        let nf = |me: &mut Terms, x: u32| -> Option<u32> {
            let tm = me.term(x, &mut HashMap::new())?;
            let v = oracle::nf((*tm).clone(), &mut Fuel(fuel)).ok()?;
            Some(me.of_term(&v))
        };
        let (k, a, b) = self.nodes[t as usize];
        let r = if k == APPLY { nf(self, a).zip(nf(self, b)).map(|(a, b)| self.intern((APPLY, a, b))) } else { nf(self, t) };
        self.keys.insert(t, r.unwrap_or(UNSEEN));
        r
    }
}

/// Names fixed when an agent is first seen (`Params::memo_names`): a hash of its tag and of its
/// inputs' names then, duplicators and normalizers transparent. A rewrite keeps a computation's
/// value, so a name goes on meaning what it meant, and unlike a term read back it is never worked out
/// again when an input is evaluated. A chip would make them in each rewrite, from the names the dying
/// pair keeps for its inputs.
#[derive(Default)]
pub struct Names(HashMap<u32, u64>);

fn mix(mut x: u64) -> u64 { x ^= x >> 30; x = x.wrapping_mul(0xbf58476d1ce4e5b9); x ^= x >> 27; x = x.wrapping_mul(0x94d049bb133111eb); x ^ x >> 31 }

impl Names {
    fn source(&mut self, net: &Net, r: Ref) -> u64 {
        let a = net.get(r.0);
        match a.tag {
            Tag::Dn | Tag::Nrm => self.source(net, a.ports[0].unwrap()),
            Tag::Unp => mix(self.source(net, a.ports[0].unwrap()) ^ (0x5500 + r.1 as u64)),
            _ => self.of(net, r.0),
        }
    }
    /// The name of an agent heading a computation or a value.
    pub fn of(&mut self, net: &Net, id: u32) -> u64 {
        if let Some(&n) = self.0.get(&id) { return n; }
        // A cycle back here meets a name no other agent has.
        self.0.insert(id, mix(0xC1C1E ^ (id as u64) << 20 ^ self.0.len() as u64));
        let a = net.get(id);
        let ins: &[usize] = match a.tag { Tag::L => &[], Tag::S => &[1], Tag::F | Tag::P | Tag::Pair => &[1, 2], _ => &[0, 1] };
        // A suspension and an apply are the same application at two stages.
        let kind = if a.tag == Tag::A { Tag::P } else { a.tag };
        let n = ins.iter().fold(mix(kind as u64 + 1), |h, &q| mix(h.rotate_left(17) ^ self.source(net, a.ports[q].unwrap())));
        self.0.insert(id, n);
        n
    }
}

/// The output port of an agent heading a computation or a value.
pub fn out_port(t: Tag) -> Option<u8> {
    match t {
        Tag::L | Tag::S | Tag::F | Tag::P => Some(0),
        Tag::A | Tag::T1 | Tag::Sel => Some(2),
        _ => None,
    }
}
pub fn computes(t: Tag) -> bool { matches!(t, Tag::P | Tag::A | Tag::T1 | Tag::Sel) }

/// An agent the answer depends on that heads a computation (P, A, T1, Sel) or a value (L, S,
/// F), the term on its output, and its site.
#[derive(Clone, Copy, Debug)]
pub struct Head {
    pub id: u32,
    pub tag: Tag,
    pub term: u32,
    /// Its seat (site × slots + slot), and its site.
    pub seat: u32,
    pub site: u32,
    pub want: bool,
}

/// Every head the root's answer depends on (garbage left out), with its term.
pub fn heads(l: &Lattice, terms: &mut Terms) -> Vec<Head> {
    let net = &l.shadow;
    let Some(out) = net.agents.iter().position(|a| matches!(a, Some(a) if a.tag == Tag::Out)) else { return vec![] };
    let at = terms.of_net(net);
    let seat = crate::readback::seats(l);
    crate::readback::subtree(net, out as u32).into_iter().filter_map(|id| {
        let a = net.agents[id as usize].as_ref()?;
        let q = out_port(a.tag)?;
        let s = *seat.get(id as usize)?;
        (s != u32::MAX).then(|| Head { id, tag: a.tag, term: at[id as usize][q as usize], seat: s, site: s / l.ks as u32, want: l.want[s as usize] })
    }).collect()
}

/// Sites apart along the lattice's links.
pub fn apart(l: &Lattice, a: u32, b: u32) -> u32 {
    let xyz = |s: u32| [s % l.p.w, s / l.p.w % l.p.h, s / (l.p.w * l.p.h)];
    let (a, b) = (xyz(a), xyz(b));
    (0..3).map(|i| a[i].abs_diff(b[i])).sum()
}

/// Which head to keep of two equal ones: one already running before a suspension, then a wanted
/// one, then the older.
fn rank(h: &Head) -> (u8, u8, u32) { (if h.tag == Tag::P { 1 } else { 0 }, !h.want as u8, h.id) }

/// The merges a central detector makes at radius r: (kept, dropped) pairs of these heads at most r
/// sites apart with equal `key` (a term; None: never merged). Outer ones first, and nothing inside
/// a dropped one is used again.
pub fn plan(l: &Lattice, terms: &Terms, heads: &[Head], key: &dyn Fn(&Head) -> Option<u32>, r: u32) -> Vec<(Head, Head)> {
    let mut groups: HashMap<u32, Vec<Head>> = HashMap::new();
    for h in heads { if let Some(k) = key(h) { groups.entry(k).or_default().push(*h); } }
    let mut groups: Vec<Vec<Head>> = groups.into_values().filter(|g| g.len() > 1).collect();
    groups.sort_by_key(|g| std::cmp::Reverse(terms.size(g[0].term)));
    let mut gone = std::collections::HashSet::new();
    let mut out = vec![];
    for mut g in groups {
        g.sort_by_key(rank);
        let mut kept: Vec<Head> = vec![];
        for h in g {
            if gone.contains(&h.id) { continue; }
            match kept.iter().filter(|k| apart(l, k.site, h.site) <= r).min_by_key(|k| apart(l, k.site, h.site)) {
                Some(k) => {
                    for x in crate::readback::subtree(&l.shadow, h.id) { gone.insert(x); }
                    out.push((*k, h));
                }
                None => kept.push(h),
            }
        }
    }
    out
}

/// Drop computation `d` for a copy of `k`'s output, in the abstract net: a fresh duplicator on
/// k's output hands one copy to k's reader and one to d's, and erasers take d's inputs. Returns
/// the duplicator and the erasers.
pub fn merge_net(net: &mut Net, k: u32, d: u32) -> (u32, [u32; 2]) {
    let (kt, dt) = (net.get(k).tag, net.get(d).tag);
    let (kq, dq) = (out_port(kt).unwrap(), out_port(dt).unwrap());
    let (kr, dr) = (net.get(k).ports[kq as usize].expect("wired"), net.get(d).ports[dq as usize].expect("wired"));
    let inputs: Vec<Ref> = (0..3).filter(|&q| q != dq as usize).map(|q| net.get(d).ports[q].expect("wired")).collect();
    net.agents[d as usize] = None;
    let dn = net.mk(Tag::Dn);
    net.link(dn, 0, k, kq);
    net.link(dn, 1, kr.0, kr.1);
    net.link(dn, 2, dr.0, dr.1);
    let mut eps = [0; 2];
    for (e, (b, bp)) in eps.iter_mut().zip(inputs) { *e = net.mk(Tag::Eps); net.link(*e, 0, b, bp); }
    (dn, eps)
}

/// The idealised lazy machine (README "Fork"): demand arrives at once, every wanted rewrite fires
/// at once, and erasers collect as on the lattice. `want` is by agent id. Rewrites and collections
/// until the root reads back as a value, or None if it gets stuck or runs past `rounds`.
pub fn ideal(net: &mut Net, want: &mut Vec<bool>, fork: bool, rounds: u64) -> Option<u64> { ideal_depth(net, want, fork, rounds).map(|w| w.0) }

/// `ideal`, also giving the rounds it took: the depth of the computation.
pub fn ideal_depth(net: &mut Net, want: &mut Vec<bool>, fork: bool, rounds: u64) -> Option<(u64, u64)> {
    let out = net.agents.iter().position(|a| matches!(a, Some(a) if a.tag == Tag::Out))? as u32;
    let mut work = 0;
    let mut live: Vec<u32> = (0..net.agents.len() as u32).filter(|&i| net.agents[i as usize].is_some()).collect();
    let tag = |net: &Net, i: u32| net.agents[i as usize].as_ref().map(|a| a.tag);
    for round in 0..rounds {
        if net.readback(net.get(out).ports[0]).is_some() { return Some((work, round)); }
        want.resize(net.agents.len(), false);
        let mut stack: Vec<u32> = live.iter().copied().filter(|&i| want[i as usize] && tag(net, i).is_some_and(|t| t.is_consumer() && t != Tag::Eps)).collect();
        while let Some(c) = stack.pop() {
            let Some((b, q)) = net.agents[c as usize].as_ref().and_then(|a| a.ports[0]) else { continue };
            let bt = tag(net, b).unwrap();
            if q > 0 && bt.is_consumer() && is_source(bt, q as usize) && !want[b as usize] { want[b as usize] = true; stack.push(b); }
        }
        let mut acted = false;
        for i in 0..live.len() {
            let c = live[i];
            let Some(a) = net.agents[c as usize].as_ref() else { continue };
            if !a.tag.is_consumer() || !want[c as usize] { continue; }
            let Some((b, q)) = a.ports[0] else { continue };
            let bt = tag(net, b).unwrap();
            if q == 0 && bt.is_producer() {
                let (rule, fresh) = net.fire(c, b);
                let w = super::fresh_wanted_in(rule, fork);
                want.resize(net.agents.len(), false);
                for (f, &x) in fresh.iter().enumerate() { want[x as usize] = w[f]; }
                live.extend(fresh);
            } else if a.tag == Tag::Eps && collect_net(net, c, b, q, want, &mut live) {
            } else { continue; }
            work += 1;
            acted = true;
        }
        live.retain(|&i| net.agents[i as usize].is_some());
        if !acted { return None; }
    }
    None
}

/// Eraser e reads output q of b: collect b as `Lattice::collect` does. Whether it did.
fn collect_net(net: &mut Net, e: u32, b: u32, q: u8, want: &mut Vec<bool>, live: &mut Vec<u32>) -> bool {
    let a = net.get(b).clone();
    let mut fresh_eps = |net: &mut Net, r: Ref, want: &mut Vec<bool>| {
        let x = net.mk(Tag::Eps);
        net.link(x, 0, r.0, r.1);
        want.resize(net.agents.len(), false);
        want[x as usize] = true;
        live.push(x);
    };
    match a.tag {
        Tag::Dn if q > 0 => {
            let (src, dst) = (a.ports[0].unwrap(), a.ports[3 - q as usize].unwrap());
            if src == (b, 3 - q) { return false; }
            net.agents[e as usize] = None;
            net.agents[b as usize] = None;
            net.link(src.0, src.1, dst.0, dst.1);
        }
        Tag::A | Tag::T1 | Tag::Sel if q == 2 => {
            net.agents[e as usize] = None;
            net.agents[b as usize] = None;
            fresh_eps(net, a.ports[0].unwrap(), want);
            fresh_eps(net, a.ports[1].unwrap(), want);
        }
        Tag::Unp if q > 0 => {
            let (o, _) = a.ports[3 - q as usize].unwrap();
            if o == e || net.get(o).tag != Tag::Eps { return false; }
            for x in [e, o, b] { net.agents[x as usize] = None; }
            fresh_eps(net, a.ports[0].unwrap(), want);
        }
        _ => return false,
    }
    true
}

/// Why two equal applications (P or A) are equal, input by input: `d` when they read the two copies
/// of one duplicator, `s` when their inputs lead back through duplicators to one output, `x` when
/// they are equal values built apart; `-` for triages and dispatches.
pub fn kinship(net: &Net, a: u32, b: u32) -> String {
    let ins = |t: Tag| match t { Tag::P => Some([1, 2]), Tag::A => Some([0, 1]), _ => None };
    let (Some(ia), Some(ib)) = (ins(net.get(a).tag), ins(net.get(b).tag)) else { return "-".into() };
    let root = |mut r: Ref| { while net.get(r.0).tag == Tag::Dn { r = net.get(r.0).ports[0].unwrap(); } r };
    (0..2).map(|i| {
        let (ra, rb) = (net.get(a).ports[ia[i]].unwrap(), net.get(b).ports[ib[i]].unwrap());
        if ra.0 == rb.0 && net.get(ra.0).tag == Tag::Dn { 'd' } else if root(ra) == root(rb) { 's' } else { 'x' }
    }).collect()
}

/// The lattice's wanted bits by agent id.
pub fn wanted(l: &Lattice) -> Vec<bool> {
    let mut w = vec![false; l.shadow.agents.len()];
    for &s in l.live() {
        for k in 0..l.ks {
            let i = s as usize * l.ks + k;
            if l.tags[i] != 0 && (l.sids[i] as usize) < w.len() { w[l.sids[i] as usize] = l.want[i]; }
        }
    }
    w
}

/// The work the idealised machine still has to do from the lattice's net as it is, after the
/// merges in `plan` (None: none).
pub fn remaining(l: &Lattice, merges: Option<&[(Head, Head)]>, rounds: u64) -> Option<u64> {
    let mut net = Net { agents: l.shadow.agents.clone(), ints: l.shadow.ints };
    let mut want = wanted(l);
    for &(k, d) in merges.unwrap_or(&[]) {
        if net.agents[d.id as usize].is_none() || net.agents[k.id as usize].is_none() { continue; }
        let (dn, eps) = merge_net(&mut net, k.id, d.id);
        want.resize(net.agents.len(), false);
        want[dn as usize] = d.want;
        for e in eps { want[e as usize] = true; }
        if d.want && k.tag.is_consumer() { want[k.id as usize] = true; }
    }
    ideal(&mut net, &mut want, l.p.fork, rounds)
}

/// Seats and wire paths for a merge, planned before anything changes: the pairings each site gains
/// or loses, and the slots and lanes already spoken for.
#[derive(Clone, Default)]
struct Planner {
    pairs: HashMap<u32, i32>,
    slots: std::collections::HashSet<(u32, usize)>,
    freed: Vec<(u32, usize)>,
    lanes: std::collections::HashSet<(u32, usize, usize)>,
    /// The corner of the 2×2×2 block everything must stay in, if any (`Params::memo_local`).
    fence: Option<[i64; 3]>,
}

/// A wire's path: each link as (site, face, lane), from the first site on.
type Path = Vec<(u32, usize, usize)>;

/// The corner of the 2×2×2 block holding site s, the lattice cut into blocks at offset o.
pub fn corner(l: &Lattice, s: u32, o: [i64; 3]) -> [i64; 3] { let x = l.xyz(s); [0, 1, 2].map(|i| x[i] - (x[i] + o[i]) % 2) }

impl Planner {
    fn inside(&self, l: &Lattice, s: u32) -> bool {
        let Some(c) = self.fence else { return true };
        let x = l.xyz(s);
        (0..3).all(|i| x[i] >= c[i] && x[i] < c[i] + 2)
    }
    fn room(&self, l: &Lattice, s: u32, n: i32) -> bool {
        l.p.pairs == 0 || l.pairs(s) as i32 + self.pairs.get(&s).copied().unwrap_or(0) + n <= l.p.pairs as i32
    }
    fn add(&mut self, s: u32, n: i32) { *self.pairs.entry(s).or_insert(0) += n; }
    fn slot(&self, l: &Lattice, s: u32) -> Option<usize> {
        (0..l.p.k).find(|&k| (l.tag(s, k) == 0 || self.freed.contains(&(s, k))) && !self.slots.contains(&(s, k)))
    }
    fn lane(&self, l: &Lattice, s: u32, f: usize) -> Option<usize> {
        (0..l.p.lanes).find(|&i| l.mate_of(s, l.se(f, i)) == NONE && !self.lanes.contains(&(s, f, i)))
    }
    /// A wire from site a to site b along links with a free lane, through sites with room for one
    /// more pairing, at most `reach` links long; its pairings and lanes are spoken for.
    fn wire(&mut self, l: &Lattice, a: u32, b: u32, reach: u32) -> Option<Path> {
        if a == b {
            if !self.room(l, a, 1) { return None; }
            self.add(a, 1);
            return Some(vec![]);
        }
        if !self.room(l, a, 1) || !self.room(l, b, 1) { return None; }
        let mut prev: HashMap<u32, (u32, usize)> = HashMap::from([(a, (a, 0))]);
        let mut front = vec![a];
        for _ in 0..reach {
            let mut next = vec![];
            for &s in &front {
                for f in 0..l.faces {
                    let t = l.nb(s, f);
                    if t == u32::MAX || prev.contains_key(&t) || !self.inside(l, t) || self.lane(l, s, f).is_none() || t != b && !self.room(l, t, 1) { continue; }
                    prev.insert(t, (s, f));
                    next.push(t);
                }
            }
            if prev.contains_key(&b) { break; }
            front = next;
        }
        if !prev.contains_key(&b) { return None; }
        let mut links = vec![];
        let mut t = b;
        while t != a { let (s, f) = prev[&t]; links.push((s, f)); t = s; }
        links.reverse();
        let path: Path = links.into_iter().map(|(s, f)| {
            let i = self.lane(l, s, f).unwrap();
            self.lanes.insert((s, f, i));
            self.lanes.insert((l.nb(s, f), f ^ 1, i));
            (s, f, i)
        }).collect();
        for &(s, _, _) in &path { self.add(s, 1); }
        self.add(b, 1);
        Some(path)
    }
    /// A seat for a fresh agent with one wire to each of these sites, in the first of them or next
    /// to it: the seat, and each wire's path from it.
    fn seat(&mut self, l: &Lattice, ends: &[u32], reach: u32) -> Option<((u32, usize), Vec<Path>)> {
        let a = ends[0];
        for s in std::iter::once(a).chain((0..l.faces).map(|f| l.nb(a, f)).filter(|&t| t != u32::MAX)) {
            if !self.inside(l, s) { continue; }
            let Some(k) = self.slot(l, s) else { continue };
            let mut p = self.clone();
            p.slots.insert((s, k));
            let paths: Option<Vec<Path>> = ends.iter().map(|&b| p.wire(l, s, b, reach)).collect();
            if let Some(paths) = paths { *self = p; return Some(((s, k), paths)); }
        }
        None
    }
}

impl Lattice {
    /// The central detector's turn (`Params::memo`): equal computations at most `memo` sites apart
    /// are merged where there is room. With `check_every` set, every invariant is checked after
    /// every merge.
    pub(super) fn memo_turn(&mut self) {
        let mut terms = Terms::default();
        let comps: Vec<Head> = heads(self, &mut terms).into_iter().filter(|h| computes(h.tag)).collect();
        let log = std::env::var("MEMO_LOG").is_ok();
        // Block-local: only twins in one 2×2×2 block of the lattice cut at this clock's offset.
        let g = super::hash(u32::MAX, self.tick());
        let o = [g as i64 & 1, g as i64 >> 8 & 1, if self.p.depth > 1 { g as i64 >> 16 & 1 } else { 0 }];
        // Equal terms, or with `memo_names` equal names.
        let mut keys: HashMap<(u64, [i64; 3]), u32> = HashMap::new();
        let mut key: HashMap<u32, u32> = HashMap::new();
        for h in &comps {
            let c = if self.p.memo_local { corner(self, h.site, o) } else { [0; 3] };
            let x = if self.p.memo_names { self.names.of(&self.shadow, h.id) } else { h.term as u64 };
            let n = keys.len() as u32;
            key.insert(h.id, *keys.entry((x, c)).or_insert(n));
        }
        let r = if self.p.memo_local { 3 } else { self.p.memo };
        for (k, d) in plan(self, &terms, &comps, &|h| key.get(&h.id).copied(), r) {
            if log {
                eprintln!("merge clock {:.0} kept {} dropped {} apart {} size {} inputs {}", self.stats.clocks, k.tag.name(), d.tag.name(),
                    apart(self, k.site, d.site), terms.size(k.term), kinship(&self.shadow, k.id, d.id));
            }
            let fence = self.p.memo_local.then(|| corner(self, d.site, o));
            if self.merge(k, d, fence) { self.stats.merged += 1; } else { self.stats.unmerged += 1; }
            if self.check_every > 0 {
                self.check_invariants().unwrap_or_else(|e| panic!("after a merge: {e}"));
                self.check_projection().unwrap_or_else(|e| panic!("after a merge: {e}"));
            }
        }
    }

    /// Drop computation d for a copy of k's output (`merge_net`) on the lattice: a fresh duplicator
    /// beside k takes k's output and hands one copy to k's reader and the other, along a new wire,
    /// to d's; erasers in or beside d's site take d's inputs. With a fence, all of it stays inside
    /// that 2×2×2 block. Nothing changes if there is no room.
    fn merge(&mut self, k: Head, d: Head, fence: Option<[i64; 3]>) -> bool {
        let ks = self.ks;
        if self.sids[k.seat as usize] != k.id || self.sids[d.seat as usize] != d.id { return false; }
        let ((sk, kk), (sd, kd)) = ((k.site, k.seat as usize % ks), (d.site, d.seat as usize % ks));
        let (kq, dq) = (out_port(k.tag).unwrap() as usize, out_port(d.tag).unwrap() as usize);
        let kout = self.ae(kk, kq);
        let mk = self.mate_of(sk, kout);
        let md: Vec<u8> = (0..ARITY).map(|q| self.mate_of(sd, self.ae(kd, q))).collect();
        let ins: Vec<usize> = (0..ARITY).filter(|&q| q != dq).collect();
        let reach = apart(self, sk, sd) + 8;
        let mut pl = Planner { fence, ..Planner::default() };
        pl.add(sd, -(ARITY as i32));
        pl.add(sk, -1);
        pl.freed.push((sd, kd));
        let Some((dn_at, dn_paths)) = pl.seat(self, &[sk, sk, sd], reach) else { return false };
        let Some((e0_at, e0_path)) = pl.seat(self, &[sd], 2) else { return false };
        let Some((e1_at, e1_path)) = pl.seat(self, &[sd], 2) else { return false };
        for q in 0..ARITY { let m = md[q]; self.set(sd, self.ae(kd, q), NONE); self.set(sd, m, NONE); }
        self.remove(sd, kd);
        self.set(sk, kout, NONE);
        self.set(sk, mk, NONE);
        let (dn, eps) = merge_net(&mut self.shadow, k.id, d.id);
        let label = self.label[k.seat as usize];
        self.place(dn_at.0, dn_at.1, code(Tag::Dn), dn);
        let i = dn_at.0 as usize * ks + dn_at.1;
        self.want[i] = d.want;
        self.label[i] = label;
        if d.want && k.tag.is_consumer() { self.want[k.seat as usize] = true; }
        for (&(s, kx), e) in [e0_at, e1_at].iter().zip(eps) {
            self.place(s, kx, code(Tag::Eps), e);
            self.label[s as usize * ks + kx] = GARBAGE;
        }
        let mut sites = vec![sk, sd, dn_at.0, e0_at.0, e1_at.0];
        let wires = [(dn_at, 0, &dn_paths[0], sk, kout), (dn_at, 1, &dn_paths[1], sk, mk), (dn_at, 2, &dn_paths[2], sd, md[dq]),
                     (e0_at, 0, &e0_path[0], sd, md[ins[0]]), (e1_at, 0, &e1_path[0], sd, md[ins[1]])];
        for ((s, kx), q, path, b, eb) in wires {
            let mut cur = self.ae(kx, q);
            let mut at = s;
            for &(x, f, i) in path {
                self.link(x, cur, self.se(f, i));
                self.strand_count(1);
                cur = self.se(f ^ 1, i);
                at = self.nb(x, f);
                sites.push(x);
            }
            debug_assert_eq!(at, b, "a merge's wire ends where it was planned to");
            self.link(b, cur, eb);
        }
        for s in sites { self.refresh(s); }
        self.settle_pulses();
        true
    }
}
