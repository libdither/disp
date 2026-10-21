//! Interaction systems over any signature: kinds of agent, each with its number of auxiliary ports, and
//! perhaps an eraser, whose rules are fixed (it puts an eraser on each auxiliary port of what it meets,
//! and two erasers vanish). The right sides a pair of kinds may have, what a step by each costs to undo,
//! and systems built from a choice of right sides.

use crate::live::{Piece, System};
use std::collections::HashMap;

pub struct Sig {
    pub name: &'static str,
    /// Auxiliary ports per kind; the eraser, if any, is the last kind.
    pub arity: Vec<u8>,
    pub eraser: Option<u8>,
}

impl Sig {
    pub fn new(name: &'static str, arity: &[u8], eraser: bool) -> Sig {
        let mut arity = arity.to_vec();
        let e = eraser.then(|| {
            arity.push(0);
            arity.len() as u8 - 1
        });
        Sig { name, arity, eraser: e }
    }
    pub fn kinds(&self) -> u8 {
        self.arity.len() as u8
    }
    pub fn ports(&self, k: u8) -> usize {
        1 + self.arity[k as usize] as usize
    }
    pub fn letter(&self, k: u8) -> char {
        if Some(k) == self.eraser { 'E' } else { self.name.as_bytes()[k as usize] as char }
    }
    /// The pairs that get a chosen rule: every two kinds, the eraser aside.
    pub fn pairs(&self) -> Vec<(u8, u8)> {
        let n = self.kinds() - self.eraser.is_some() as u8;
        (0..n).flat_map(|a| (a..n).map(move |b| (a, b))).collect()
    }
    pub fn text(&self, p: &Piece) -> String {
        let kinds: String = p.kinds.iter().map(|&k| self.letter(k)).collect();
        let wires: Vec<String> = p.partner.iter().map(|q| q.to_string()).collect();
        format!("{} {}", if kinds.is_empty() { "-" } else { &kinds }, wires.join(","))
    }
}

fn offsets(sig: &Sig, p: &Piece) -> Vec<usize> {
    let mut o = Vec::with_capacity(p.kinds.len());
    let mut at = p.free;
    for &k in &p.kinds {
        o.push(at);
        at += sig.ports(k);
    }
    o
}

/// The piece walked from its free ports in turn, numbering agents as they are met: the same for two
/// pieces that differ only in how their agents are numbered. None if some agent cannot be reached.
pub fn canon(sig: &Sig, p: &Piece) -> Option<Vec<u8>> {
    let off = offsets(sig, p);
    let mut owner = vec![(0usize, 0usize); p.partner.len()];
    for (i, &k) in p.kinds.iter().enumerate() {
        for q in 0..sig.ports(k) {
            owner[off[i] + q] = (i, q);
        }
    }
    let mut num = vec![u8::MAX; p.kinds.len()];
    let mut order: Vec<usize> = vec![];
    let mut out = Vec::with_capacity(2 * p.partner.len());
    let name = |q: usize, out: &mut Vec<u8>, num: &mut Vec<u8>, order: &mut Vec<usize>| {
        if q < p.free {
            out.push(q as u8);
        } else {
            let (ag, local) = owner[q];
            if num[ag] == u8::MAX {
                num[ag] = order.len() as u8;
                order.push(ag);
            }
            out.push(32 + p.kinds[ag] * 4 + local as u8);
            out.push(num[ag]);
        }
    };
    for f in 0..p.free {
        name(p.partner[f] as usize, &mut out, &mut num, &mut order);
    }
    let mut i = 0;
    while i < order.len() {
        let ag = order[i];
        i += 1;
        for q in 0..sig.ports(p.kinds[ag]) {
            name(p.partner[off[ag] + q] as usize, &mut out, &mut num, &mut order);
        }
    }
    (order.len() == p.kinds.len()).then_some(out)
}

/// The piece with its free ports renamed: free port f becomes perm[f].
pub fn rename_free(p: &Piece, perm: &[usize]) -> Piece {
    let map = |q: usize| if q < p.free { perm[q] } else { q };
    let mut partner = vec![0u8; p.partner.len()];
    for (q, &r) in p.partner.iter().enumerate() {
        partner[map(q)] = map(r as usize) as u8;
    }
    Piece { kinds: p.kinds.clone(), partner, free: p.free }
}

/// No two of the piece's agents meet at their principal ports.
pub fn quiet(sig: &Sig, p: &Piece) -> bool {
    let off = offsets(sig, p);
    off.iter().all(|&o| !off.contains(&(p.partner[o] as usize)))
}

/// Which of the piece's ports are principal.
pub fn principal(sig: &Sig, p: &Piece) -> Vec<bool> {
    let mut v = vec![false; p.partner.len()];
    for o in offsets(sig, p) {
        v[o] = true;
    }
    v
}

/// The parts of a piece, counting free ports as nodes, and how many of them have no agent (a bare wire,
/// which a footprint must mark on the wire itself).
pub fn parts(sig: &Sig, p: &Piece) -> (u32, u32) {
    let nodes = p.free + p.kinds.len();
    let mut node_of = (0..p.free).collect::<Vec<_>>();
    for (i, &k) in p.kinds.iter().enumerate() {
        node_of.extend(std::iter::repeat(p.free + i).take(sig.ports(k)));
    }
    let mut root: Vec<usize> = (0..nodes).collect();
    fn find(r: &mut [usize], mut x: usize) -> usize {
        while r[x] != x {
            r[x] = r[r[x]];
            x = r[x];
        }
        x
    }
    for (q, &r) in p.partner.iter().enumerate() {
        let (a, b) = (find(&mut root, node_of[q]), find(&mut root, node_of[r as usize]));
        root[a] = b;
    }
    let mut count = 0;
    let mut bare = 0;
    for x in 0..nodes {
        if find(&mut root, x) == x {
            count += 1;
            if (p.free..nodes).all(|a| find(&mut root, a) != x) {
                bare += 1;
            }
        }
    }
    (count, bare)
}

/// Σ (auxiliary ports − 1) over agents: for a closed connected net, twice its independent loops less
/// two. A connected right side cannot lower it (see `law`).
pub fn phi(sig: &Sig, kinds: &[u8]) -> i32 {
    kinds.iter().map(|&k| sig.arity[k as usize] as i32 - 1).sum()
}

fn matchings(free: &mut Vec<bool>, partner: &mut Vec<u8>, each: &mut dyn FnMut(&[u8])) {
    let Some(a) = free.iter().position(|&f| f) else {
        each(partner);
        return;
    };
    free[a] = false;
    for b in a + 1..free.len() {
        if free[b] {
            free[b] = false;
            partner[a] = b as u8;
            partner[b] = a as u8;
            matchings(free, partner, each);
            free[b] = true;
        }
    }
    free[a] = true;
}

/// Every piece with `free` free ports and up to `max` agents of the given kinds, one per net up to
/// renaming its agents, all agents reachable from the free ports.
pub fn pieces(sig: &Sig, pool: &[u8], free: usize, max: usize) -> Vec<Piece> {
    let mut seen: HashMap<Vec<u8>, Piece> = HashMap::new();
    let mut multisets: Vec<Vec<u8>> = vec![vec![]];
    for k in 0..=max {
        for kinds in &multisets {
            let n = free + kinds.iter().map(|&k| sig.ports(k)).sum::<usize>();
            if n % 2 == 1 {
                continue;
            }
            let (mut fr, mut partner) = (vec![true; n], vec![0u8; n]);
            matchings(&mut fr, &mut partner, &mut |m| {
                let p = Piece { kinds: kinds.clone(), partner: m.to_vec(), free };
                if let Some(c) = canon(sig, &p) {
                    seen.entry(c).or_insert(p);
                }
            });
        }
        if k < max {
            multisets = multisets
                .iter()
                .flat_map(|m| pool.iter().filter(move |&&x| m.last().map_or(true, |&l| x >= l)).map(move |&x| {
                    let mut m = m.clone();
                    m.push(x);
                    m
                }))
                .collect();
        }
    }
    let mut v: Vec<Piece> = seen.into_values().collect();
    v.sort_by_key(|p| canon(sig, p));
    v
}

/// Right sides that put a new agent's principal port on every free port, their auxiliary ports wired
/// among themselves (Lafont's commutation is one).
pub fn outward(sig: &Sig, pool: &[u8], free: usize) -> Vec<Piece> {
    let mut out = vec![];
    let mut kinds = vec![0u8; free];
    let n = pool.len().pow(free as u32);
    for code in 0..n {
        let mut c = code;
        for k in kinds.iter_mut() {
            *k = pool[c % pool.len()];
            c /= pool.len();
        }
        let total = free + kinds.iter().map(|&k| sig.ports(k)).sum::<usize>();
        let mut fr = vec![true; total];
        let mut partner = vec![0u8; total];
        let mut at = free;
        for f in 0..free {
            partner[f] = at as u8;
            partner[at] = f as u8;
            fr[f] = false;
            fr[at] = false;
            at += sig.ports(kinds[f]);
        }
        matchings(&mut fr, &mut partner, &mut |m| out.push(Piece { kinds: kinds.clone(), partner: m.to_vec(), free }));
    }
    out
}

/// The eraser's rule with kind k: an eraser on each of k's auxiliary ports (none, when k is the eraser).
pub fn eraser_rule(sig: &Sig, k: u8) -> Piece {
    let e = sig.eraser.unwrap();
    let f = if k == e { 0 } else { sig.arity[k as usize] as usize };
    let mut partner: Vec<u8> = (0..f).map(|i| (f + i) as u8).collect();
    partner.extend((0..f).map(|i| i as u8));
    Piece { kinds: vec![e; f], partner, free: f }
}

/// Swaps the two agents of a same-kind pair: their auxiliary ports trade places.
pub fn swap_perm(a: usize) -> Vec<usize> {
    (0..2 * a).map(|i| (i + a) % (2 * a)).collect()
}

/// The right sides tried for the pair a ⋈ b: every quiet one of up to `small` agents (wires alone among
/// them), and every outward one; for a same-kind pair, only those that read the same with the two
/// agents traded.
pub fn rule_space(sig: &Sig, a: u8, b: u8, small: usize) -> Vec<Piece> {
    let f = (sig.arity[a as usize] + sig.arity[b as usize]) as usize;
    let pool: Vec<u8> = (0..sig.kinds()).collect();
    let mut seen: HashMap<Vec<u8>, Piece> = HashMap::new();
    for p in pieces(sig, &pool, f, small).into_iter().chain(outward(sig, &pool, f)) {
        if !quiet(sig, &p) {
            continue;
        }
        let c = canon(sig, &p).unwrap();
        if a == b && canon(sig, &rename_free(&p, &swap_perm(sig.arity[a as usize] as usize))).unwrap() != c {
            continue;
        }
        seen.entry(c).or_insert(p);
    }
    let mut v: Vec<(Vec<u8>, Piece)> = seen.into_iter().collect();
    v.sort_by(|x, y| x.0.cmp(&y.0));
    v.into_iter().map(|(_, p)| p).collect()
}

/// A system: for each of `sig.pairs()`, the chosen right side; the eraser's rules added.
pub fn system(sig: &Sig, chosen: &[&Piece]) -> System {
    let n = sig.kinds() as usize;
    let mut sys = System { arity: sig.arity.clone(), eraser: sig.eraser, rules: vec![None; n * n], marks: vec![0; n * n] };
    let mut put = |a: u8, b: u8, p: Piece| {
        let i = sys.pair(a, b);
        sys.marks[i] = parts(sig, &p).0.max(1);
        sys.rules[i] = Some(p);
    };
    for (&(a, b), p) in sig.pairs().iter().zip(chosen) {
        put(a, b, (*p).clone());
    }
    if let Some(e) = sig.eraser {
        for k in 0..sig.kinds() {
            put(k, e, eraser_rule(sig, k));
        }
    }
    sys
}

/// The footprint law, checked on a right side: a step whose right side has c parts lowers Σ (aux − 1)
/// by at most 2(c − 1). With E ≥ nodes − c wires, and 2E = free ports + every agent's ports, the right
/// side's Σ (aux − 1) ≥ free − 2c, and the pair's is free − 2.
pub fn law(sig: &Sig, a: u8, b: u8, p: &Piece) -> (bool, bool) {
    let (c, _) = parts(sig, p);
    let lhs = phi(sig, &[a, b]);
    let rhs = phi(sig, &p.kinds);
    let bound = lhs - rhs;
    (2 * (c as i32 - 1) >= bound, 2 * (c as i32 - 1) == bound)
}

/// The renamings of a signature: kinds traded among those with as many ports (the eraser kept), and
/// each binary kind's two auxiliary ports swapped or not.
pub fn renamings(sig: &Sig) -> Vec<(Vec<u8>, Vec<bool>)> {
    let n = sig.kinds() as usize;
    let mut perms: Vec<Vec<u8>> = vec![vec![]];
    for _ in 0..n {
        perms = perms
            .iter()
            .flat_map(|p| (0..n as u8).filter(move |x| !p.contains(x)).map(move |x| [p.clone(), vec![x]].concat()))
            .collect();
    }
    perms.retain(|p| (0..n).all(|k| sig.arity[k] == sig.arity[p[k] as usize] && (Some(k as u8) == sig.eraser) == (Some(p[k]) == sig.eraser)));
    let binary: Vec<usize> = (0..n).filter(|&k| sig.arity[k] == 2).collect();
    let mut out = vec![];
    for p in &perms {
        for bits in 0..1usize << binary.len() {
            let mut swap = vec![false; n];
            for (i, &k) in binary.iter().enumerate() {
                swap[k] = bits >> i & 1 == 1;
            }
            out.push((p.clone(), swap));
        }
    }
    out
}

/// The rule for a ⋈ b renamed: a right side for the pair the renaming makes of it.
pub fn rename_rule(sig: &Sig, a: u8, b: u8, p: &Piece, perm: &[u8], swap: &[bool]) -> Piece {
    let (ar, br) = (sig.arity[a as usize] as usize, sig.arity[b as usize] as usize);
    let flip = |k: u8, i: usize| if swap[k as usize] { 1 - i } else { i };
    let a_first = perm[a as usize] <= perm[b as usize];
    let mut map: Vec<usize> = (0..p.partner.len()).collect();
    for r in 0..p.free {
        let (k, i, first) = if r < ar { (a, r, a_first) } else { (b, r - ar, !a_first) };
        let width = if k == a { ar } else { br };
        map[r] = if first { flip(k, i) } else { p.free - width + flip(k, i) };
    }
    for (o, &k) in offsets(sig, p).iter().zip(&p.kinds) {
        if swap[k as usize] {
            map[o + 1] = o + 2;
            map[o + 2] = o + 1;
        }
    }
    let mut partner = vec![0u8; p.partner.len()];
    for (q, &r) in p.partner.iter().enumerate() {
        partner[map[q]] = map[r as usize] as u8;
    }
    Piece { kinds: p.kinds.iter().map(|&k| perm[k as usize]).collect(), partner, free: p.free }
}

/// The system up to renaming: the least, over every renaming, of its rules' canonical forms in
/// `sig.pairs()` order.
pub fn system_key(sig: &Sig, chosen: &[&Piece]) -> Vec<Vec<u8>> {
    let pairs = sig.pairs();
    renamings(sig)
        .iter()
        .map(|(perm, swap)| {
            let mut key = vec![vec![]; pairs.len()];
            for (&(a, b), p) in pairs.iter().zip(chosen) {
                let (x, y) = (perm[a as usize], perm[b as usize]);
                let at = pairs.iter().position(|&q| q == (x.min(y), x.max(y))).unwrap();
                key[at] = canon(sig, &rename_rule(sig, a, b, p, perm, swap)).unwrap();
            }
            key
        })
        .min()
        .unwrap()
}
