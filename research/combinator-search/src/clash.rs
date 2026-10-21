//! Stage 1's clash check, on whole nets rather than right sides alone. Salikhmetov's two active pairs
//! clash when they reduce to the same net; terms in their ports count. The witnesses tried here glue a
//! copy of the rule's own right side into its left side's four ports, through a renaming σ of the ports:
//! the pair `glue_pair(σ)` reduces to the right side glued to that copy through σ, which is the same net
//! as the copy glued to the right side through σ⁻¹, the reduct of `glue_pair(σ⁻¹)`. Whenever those two
//! pairs differ, the rule clashes with itself.

use crate::{ports, Net, FREE};

/// A net with no free ports: agents, each port's partner, and wire loops with no agent on them.
pub struct Closed {
    pub kinds: Vec<u8>,
    pub partner: Vec<u32>,
    pub loops: usize,
}

fn closed_offsets(kinds: &[u8]) -> Vec<usize> {
    let mut o = Vec::with_capacity(kinds.len());
    let mut at = 0;
    for &k in kinds {
        o.push(at);
        at += ports(k);
    }
    o
}

/// The net written out from one agent, numbering agents as they are met; None past that agent's part.
fn walk(c: &Closed, off: &[usize], owner: &[(u32, u8)], start: usize, comp: &mut Vec<usize>) -> Vec<u8> {
    let mut num = vec![u32::MAX; c.kinds.len()];
    let mut order = vec![start];
    num[start] = 0;
    let mut out = vec![c.kinds[start]];
    let mut qi = 0;
    while qi < order.len() {
        let ag = order[qi];
        qi += 1;
        for p in 0..ports(c.kinds[ag]) {
            let (b, lp) = owner[c.partner[off[ag] + p] as usize];
            let b = b as usize;
            if num[b] == u32::MAX {
                num[b] = order.len() as u32;
                order.push(b);
            }
            out.extend_from_slice(&[c.kinds[b], lp, num[b] as u8]);
        }
    }
    *comp = order;
    out
}

/// The net up to renaming its agents: each connected part written from whichever of its agents gives the
/// least string, the parts sorted, then the loops.
pub fn canon_closed(c: &Closed) -> Vec<u8> {
    let off = closed_offsets(&c.kinds);
    let mut owner = vec![(0u32, 0u8); c.partner.len()];
    for (i, &k) in c.kinds.iter().enumerate() {
        for p in 0..ports(k) {
            owner[off[i] + p] = (i as u32, p as u8);
        }
    }
    let mut seen = vec![false; c.kinds.len()];
    let mut parts: Vec<Vec<u8>> = vec![];
    for s in 0..c.kinds.len() {
        if seen[s] {
            continue;
        }
        let mut comp = vec![];
        let mut best = walk(c, &off, &owner, s, &mut comp);
        for &a in &comp {
            seen[a] = true;
        }
        for &a in &comp[1..] {
            let mut tmp = vec![];
            let w = walk(c, &off, &owner, a, &mut tmp);
            if w < best {
                best = w;
            }
        }
        parts.push(best);
    }
    parts.sort();
    let mut out = vec![];
    for p in parts {
        out.extend(p);
        out.push(255);
    }
    out.push(c.loops as u8);
    out
}

/// The left side x ⋈ y with a copy of the rule's right side in its ports: the port in position j (x's
/// auxiliary ports, then y's) meets the copy's free port σ(j).
pub fn glue_pair(rule: &Net, x: u8, y: u8, sigma: [usize; 4]) -> Closed {
    let mut kinds = vec![x, y];
    kinds.extend_from_slice(&rule.kinds);
    let inner = rule.partner.len() - FREE;
    let mut partner = vec![u32::MAX; 6 + inner];
    let wire = |a: usize, b: usize, partner: &mut Vec<u32>| {
        partner[a] = b as u32;
        partner[b] = a as u32;
    };
    let lhs = [1usize, 2, 4, 5];
    wire(0, 3, &mut partner);
    let map = |p: usize| 6 + p - FREE;
    for p in FREE..rule.partner.len() {
        let q = rule.partner[p] as usize;
        if q >= FREE {
            wire(map(p), map(q), &mut partner);
        }
    }
    let mut inv = [0usize; 4];
    for j in 0..4 {
        inv[sigma[j]] = j;
    }
    for j in 0..4 {
        let q = rule.partner[sigma[j]] as usize;
        if q >= FREE {
            wire(lhs[j], map(q), &mut partner);
        } else if inv[q] > j {
            wire(lhs[j], lhs[inv[q]], &mut partner);
        }
    }
    Closed { kinds, partner, loops: 0 }
}

/// Two copies of the right side, R and C, R's free port j meeting C's free port σ(j). Wires running from
/// free port to free port are followed through to the agents they join; those that close on themselves
/// are loops.
pub fn glue_rhs(rule: &Net, sigma: [usize; 4]) -> Closed {
    let inner = rule.partner.len() - FREE;
    let mut kinds = rule.kinds.clone();
    kinds.extend_from_slice(&rule.kinds);
    let mut partner = vec![u32::MAX; 2 * inner];
    // copy 0 is R, copy 1 is C; a port of the rule, in a copy
    let at = |copy: usize, p: usize| copy * inner + p - FREE;
    let mut inv = [0usize; 4];
    for j in 0..4 {
        inv[sigma[j]] = j;
    }
    let cross = |copy: usize, f: usize| if copy == 0 { (1, sigma[f]) } else { (0, inv[f]) };
    let mut free_seen = [[false; 4]; 2];
    for copy in 0..2 {
        for p in FREE..rule.partner.len() {
            let q = rule.partner[p] as usize;
            if q >= FREE {
                partner[at(copy, p)] = at(copy, q) as u32;
            } else if partner[at(copy, p)] == u32::MAX {
                // follow the free port across the glue until an agent's port
                let (mut c, mut f) = (copy, q);
                free_seen[c][f] = true;
                loop {
                    let (c2, f2) = cross(c, f);
                    free_seen[c2][f2] = true;
                    let q2 = rule.partner[f2] as usize;
                    if q2 >= FREE {
                        partner[at(copy, p)] = at(c2, q2) as u32;
                        partner[at(c2, q2)] = at(copy, p) as u32;
                        break;
                    }
                    free_seen[c2][q2] = true;
                    (c, f) = (c2, q2);
                }
            }
        }
    }
    // free ports no agent reached: wires that close into loops
    let mut loops = 0;
    for copy in 0..2 {
        for f in 0..4 {
            if free_seen[copy][f] {
                continue;
            }
            loops += 1;
            let (mut c, mut g) = (copy, f);
            while !free_seen[c][g] {
                free_seen[c][g] = true;
                let h = rule.partner[g] as usize;
                free_seen[c][h] = true;
                (c, g) = cross(c, h);
            }
        }
    }
    Closed { kinds, partner, loops }
}

/// A renaming σ of the four ports under which the rule clashes with itself, if there is one: the pairs
/// glued through σ and σ⁻¹ differ, and reduce to the same net.
pub fn self_clash(rule: &Net, x: u8, y: u8, perms: &[[usize; 4]]) -> Option<[usize; 4]> {
    for &s in perms {
        let mut inv = [0usize; 4];
        for j in 0..4 {
            inv[s[j]] = j;
        }
        let a = canon_closed(&glue_pair(rule, x, y, s));
        let b = canon_closed(&glue_pair(rule, x, y, inv));
        if a != b {
            return Some(s);
        }
    }
    None
}

/// The two reducts behind a witness, which must be the same net.
pub fn reducts_agree(rule: &Net, s: [usize; 4]) -> bool {
    let mut inv = [0usize; 4];
    for j in 0..4 {
        inv[s[j]] = j;
    }
    canon_closed(&glue_rhs(rule, s)) == canon_closed(&glue_rhs(rule, inv))
}
