//! Counts the rules and systems in the smallest search space of research/COMBINATOR_SEARCH_PLAN.md: two
//! binary agent kinds A and B (a principal port and two auxiliary ones) and an eraser E. A rule for a pair
//! of binary kinds replaces the pair by a right side: a net of agents wired to the pair's four auxiliary
//! ports, x1 x2 of the first agent and y1 y2 of the second. A system is a rule for A A, one for B B and
//! one for A B; the eraser's rules are fixed.
//!
//! `cargo run --release -- K` counts right sides of up to K agents (the eraser only in right sides of up
//! to 2), the systems they make, and the systems that can run backward except through the eraser:
//! every other rule connected, and no two active pairs reducing to the same net (Salikhmetov's
//! conditions, checked here on the right sides alone, a necessary condition). Systems are also counted up
//! to renaming: swapping A and B, and swapping a kind's two auxiliary ports.

use std::collections::{HashMap, HashSet};

const A: u8 = 0;
const B: u8 = 1;
const E: u8 = 2;
const FREE: usize = 4;

fn ports(kind: u8) -> usize {
    if kind == E { 1 } else { 3 }
}

/// A right side: its agents' kinds, and each port's partner. Ports 0..4 are x1 x2 y1 y2; each agent's
/// ports follow in turn, principal first.
#[derive(Clone)]
struct Net {
    kinds: Vec<u8>,
    partner: Vec<u8>,
}

fn offsets(kinds: &[u8]) -> Vec<usize> {
    let mut o = Vec::with_capacity(kinds.len());
    let mut at = FREE;
    for &k in kinds {
        o.push(at);
        at += ports(k);
    }
    o
}

/// The net written out by walking from x1 x2 y1 y2 in turn, numbering agents as they are met: the same
/// for any two nets that differ only in how their agents are numbered. None if some agent cannot be
/// reached from the free ports (a closed loop of garbage).
fn canon(net: &Net) -> Option<Vec<u8>> {
    let n = net.kinds.len();
    let off = offsets(&net.kinds);
    let mut owner = vec![(0usize, 0usize); net.partner.len()];
    for (i, &k) in net.kinds.iter().enumerate() {
        for p in 0..ports(k) {
            owner[off[i] + p] = (i, p);
        }
    }
    let mut num = vec![u8::MAX; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);
    let mut out = Vec::with_capacity(2 * net.partner.len());
    let name = |p: usize, out: &mut Vec<u8>, num: &mut Vec<u8>, order: &mut Vec<usize>| {
        if p < FREE {
            out.push(p as u8);
        } else {
            let (ag, local) = owner[p];
            if num[ag] == u8::MAX {
                num[ag] = order.len() as u8;
                order.push(ag);
            }
            out.push(16 + net.kinds[ag] * 4 + local as u8);
            out.push(num[ag]);
        }
    };
    for f in 0..FREE {
        name(net.partner[f] as usize, &mut out, &mut num, &mut order);
    }
    let mut qi = 0;
    while qi < order.len() {
        let ag = order[qi];
        qi += 1;
        for p in 0..ports(net.kinds[ag]) {
            name(net.partner[off[ag] + p] as usize, &mut out, &mut num, &mut order);
        }
    }
    (order.len() == n).then_some(out)
}

/// The net with its ports renamed by `map` (old port to new port).
fn rename(net: &Net, kinds: Vec<u8>, map: &[usize]) -> Net {
    let mut partner = vec![0u8; net.partner.len()];
    for (p, &q) in net.partner.iter().enumerate() {
        partner[map[p]] = map[q as usize] as u8;
    }
    Net { kinds, partner }
}

fn rename_free(net: &Net, perm: [usize; 4]) -> Net {
    let mut map: Vec<usize> = (0..net.partner.len()).collect();
    map[..FREE].copy_from_slice(&perm);
    rename(net, net.kinds.clone(), &map)
}

/// No active pair inside: no two of the right side's agents meet at their principal ports, so a rule
/// ends in a net that is already done (true of all six of Lafont's rules).
fn quiet(net: &Net) -> bool {
    let off = offsets(&net.kinds);
    let principal: HashSet<usize> = off.iter().copied().collect();
    off.iter().all(|&p| !principal.contains(&(net.partner[p] as usize)))
}

/// Every free port meets a principal port: the new agents face outward, as in Lafont's commutation.
fn outward(net: &Net) -> bool {
    let principal: HashSet<usize> = offsets(&net.kinds).into_iter().collect();
    (0..FREE).all(|f| principal.contains(&(net.partner[f] as usize)))
}

/// Salikhmetov's connected: the right side does not split into parts that share no wire.
fn connected(net: &Net) -> bool {
    let off = offsets(&net.kinds);
    let nodes = FREE + net.kinds.len();
    let mut node_of = vec![0usize; net.partner.len()];
    for f in 0..FREE {
        node_of[f] = f;
    }
    for (i, &k) in net.kinds.iter().enumerate() {
        for p in 0..ports(k) {
            node_of[off[i] + p] = FREE + i;
        }
    }
    let mut root: Vec<usize> = (0..nodes).collect();
    fn find(r: &mut Vec<usize>, x: usize) -> usize {
        let mut x = x;
        while r[x] != x {
            r[x] = r[r[x]];
            x = r[x];
        }
        x
    }
    for (p, &q) in net.partner.iter().enumerate() {
        let (a, b) = (find(&mut root, node_of[p]), find(&mut root, node_of[q as usize]));
        root[a] = b;
    }
    let r0 = find(&mut root, 0);
    (0..nodes).all(|x| find(&mut root, x) == r0)
}

/// Every wiring of the ports in pairs.
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

/// Right sides of up to `max` agents, one per net up to renaming its agents.
fn right_sides(max: usize) -> Vec<Net> {
    let mut seen: HashMap<Vec<u8>, Net> = HashMap::new();
    for k in 0..=max {
        let pool: &[u8] = if k <= 2 { &[A, B, E] } else { &[A, B] };
        // the kinds of k agents, in order, each kind at least the one before (a multiset)
        let mut multisets: Vec<Vec<u8>> = vec![vec![]];
        for _ in 0..k {
            let mut next = vec![];
            for m in &multisets {
                for &x in pool {
                    if m.last().map_or(true, |&l| x >= l) {
                        let mut m = m.clone();
                        m.push(x);
                        next.push(m);
                    }
                }
            }
            multisets = next;
        }
        for kinds in multisets {
            let n = FREE + kinds.iter().map(|&k| ports(k)).sum::<usize>();
            if n % 2 == 1 {
                continue;
            }
            let mut free = vec![true; n];
            let mut partner = vec![0u8; n];
            matchings(&mut free, &mut partner, &mut |p| {
                let net = Net { kinds: kinds.clone(), partner: p.to_vec() };
                if let Some(c) = canon(&net) {
                    seen.entry(c).or_insert(net);
                }
            });
        }
    }
    seen.into_values().collect()
}

const SWAP_XY: [usize; 4] = [2, 3, 0, 1];

fn perms4() -> Vec<[usize; 4]> {
    let mut out = vec![];
    for a in 0..4 {
        for b in 0..4 {
            for c in 0..4 {
                for d in 0..4 {
                    let p = [a, b, c, d];
                    if (0..4).all(|i| p.contains(&i)) {
                        out.push(p);
                    }
                }
            }
        }
    }
    out
}

/// A renaming of the space: `swap` exchanges A and B; `aux_a` and `aux_b` exchange the two auxiliary
/// ports of every A, or every B (decided by the kind before renaming).
#[derive(Clone, Copy)]
struct Renaming {
    swap: bool,
    aux_a: bool,
    aux_b: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Slot {
    AA,
    BB,
    AB,
}

/// A rule for `slot`, renamed: the slot it becomes and its right side. The pair's own ports rename too:
/// x1 x2 are its first agent's auxiliary ports, so they swap with that kind's, and if A and B swap, the
/// pair A B is read the other way round so that A stays first.
fn renamed(net: &Net, slot: Slot, g: Renaming) -> (Slot, Net) {
    let aux = |k: u8| (k == A && g.aux_a) || (k == B && g.aux_b);
    let kind = |k: u8| if g.swap && k != E { 1 - k } else { k };
    let (x, y) = match slot {
        Slot::AA => (A, A),
        Slot::BB => (B, B),
        Slot::AB => (A, B),
    };
    let mut map: Vec<usize> = (0..net.partner.len()).collect();
    let mut free = [0usize, 1, 2, 3];
    if aux(x) {
        free.swap(0, 1);
    }
    if aux(y) {
        free.swap(2, 3);
    }
    let new_slot = match (kind(x), kind(y)) {
        (A, A) => Slot::AA,
        (B, B) => Slot::BB,
        (A, B) => Slot::AB,
        _ => {
            free = free.map(|f| SWAP_XY[f]);
            Slot::AB
        }
    };
    map[..FREE].copy_from_slice(&free);
    let off = offsets(&net.kinds);
    for (i, &k) in net.kinds.iter().enumerate() {
        if aux(k) {
            map[off[i] + 1] = off[i] + 2;
            map[off[i] + 2] = off[i] + 1;
        }
    }
    let kinds = net.kinds.iter().map(|&k| kind(k)).collect();
    (new_slot, rename(net, kinds, &map))
}

struct Info {
    canon: Vec<u8>,
    /// The net up to renaming its free ports too: two active pairs whose right sides share this clash.
    orbit: Vec<u8>,
    agents: usize,
    symmetric: bool,
    connected: bool,
    /// Renamings of the free ports that leave the net as it is.
    autos: usize,
    quiet: bool,
    outward: bool,
}

fn main() {
    let max: usize = std::env::args().nth(1).and_then(|a| a.parse().ok()).unwrap_or(2);
    let t0 = std::time::Instant::now();
    let nets = right_sides(max);
    let perms = perms4();
    let infos: Vec<Info> = nets
        .iter()
        .map(|n| {
            let c = canon(n).unwrap();
            let forms: Vec<Vec<u8>> = perms.iter().map(|&p| canon(&rename_free(n, p)).unwrap()).collect();
            Info {
                symmetric: canon(&rename_free(n, SWAP_XY)).unwrap() == c,
                connected: connected(n),
                autos: forms.iter().filter(|f| **f == c).count(),
                orbit: forms.iter().min().unwrap().clone(),
                agents: n.kinds.len(),
                quiet: quiet(n),
                outward: outward(n),
                canon: c,
            }
        })
        .collect();
    let mut by_agents: Vec<usize> = vec![0; max + 1];
    for i in &infos {
        by_agents[i.agents] += 1;
    }
    let index: HashMap<&[u8], usize> = infos.iter().enumerate().map(|(i, x)| (x.canon.as_slice(), i)).collect();
    println!("right sides of up to {max} agents: {} ({} by agent count), connected: {}, quiet: {}", infos.len(),
        by_agents.iter().enumerate().map(|(k, n)| format!("{k}: {n}")).collect::<Vec<_>>().join(", "),
        infos.iter().filter(|i| i.connected).count(), infos.iter().filter(|i| i.quiet).count());
    let report = |label: &str, keep: &dyn Fn(&Info) -> bool| {
        // a rule for A A or B B must read the same with its two agents traded; to run backward, nothing else
        // may leave it as it is. A rule for A B must be left as it is by no renaming of its free ports.
        let same: Vec<usize> = (0..infos.len()).filter(|&i| keep(&infos[i]) && infos[i].symmetric).collect();
        let cross: Vec<usize> = (0..infos.len()).filter(|&i| keep(&infos[i])).collect();
        let rev_same: Vec<usize> = same.iter().copied().filter(|&i| infos[i].connected && infos[i].autos == 2).collect();
        let rev_cross: Vec<usize> = cross.iter().copied().filter(|&i| infos[i].connected && infos[i].autos == 1).collect();
        let rev_same_set: HashSet<usize> = rev_same.iter().copied().collect();
        let pairs_apart = |a: &[usize], b: &[usize]| -> u128 {
            let mut oa: HashMap<&[u8], u128> = HashMap::new();
            for &i in a {
                *oa.entry(&infos[i].orbit).or_default() += 1;
            }
            let same_orbit: u128 = b.iter().map(|&i| oa.get(infos[i].orbit.as_slice()).copied().unwrap_or(0)).sum();
            a.len() as u128 * b.len() as u128 - same_orbit
        };

        println!("{label}");
        println!("rules for A A or B B: {}, reversible-shaped: {}", same.len(), rev_same.len());
        println!("rules for A B: {}, reversible-shaped: {}", cross.len(), rev_cross.len());
        let all = (same.len() as u128).pow(2) * cross.len() as u128;
        let rev = pairs_apart(&rev_same, &rev_same) * rev_cross.len() as u128;
        println!("systems: {all}; reversible except the eraser: {rev}");

        // up to renaming (Burnside: the average number of systems each renaming leaves as they are)
        let index_of = |n: &Net| index[canon(n).unwrap().as_slice()];
        let fixed = |set: &[usize], slot: Slot, g: Renaming| -> Vec<usize> {
            set.iter().copied().filter(|&i| index_of(&renamed(&nets[i], slot, g).1) == i).collect()
        };
        let (mut sum_all, mut sum_rev) = (0u128, 0u128);
        for bits in 0..8 {
            let g = Renaming { swap: bits & 1 == 1, aux_a: bits & 2 == 2, aux_b: bits & 4 == 4 };
            let ab_all = fixed(&cross, Slot::AB, g).len() as u128;
            let ab_rev = fixed(&rev_cross, Slot::AB, g).len() as u128;
            if !g.swap {
                let (aa, bb) = (fixed(&same, Slot::AA, g), fixed(&same, Slot::BB, g));
                let (raa, rbb) = (fixed(&rev_same, Slot::AA, g), fixed(&rev_same, Slot::BB, g));
                sum_all += aa.len() as u128 * bb.len() as u128 * ab_all;
                sum_rev += pairs_apart(&raa, &rbb) * ab_rev;
            } else {
                // A A and B B trade rules: the B B rule is the A A rule renamed, and renaming twice must give
                // the A A rule back
                let mut n_all = 0u128;
                let mut n_rev = 0u128;
                for &i in &same {
                    let (_, once) = renamed(&nets[i], Slot::AA, g);
                    let (_, twice) = renamed(&once, Slot::BB, g);
                    if index_of(&twice) != i {
                        continue;
                    }
                    n_all += 1;
                    let j = index_of(&once);
                    if rev_same_set.contains(&i) && rev_same_set.contains(&j) && infos[i].orbit != infos[j].orbit {
                        n_rev += 1;
                    }
                }
                sum_all += n_all * ab_all;
                sum_rev += n_rev * ab_rev;
            }
        }
        assert!(sum_all % 8 == 0 && sum_rev % 8 == 0, "Burnside sums must divide by the group's 8 renamings");
        println!("up to renaming: systems {}; reversible except the eraser {}", sum_all / 8, sum_rev / 8);
    };
    report("\nall right sides:", &|_| true);
    report("\nquiet right sides only (no active pair left inside):", &|i| i.quiet);
    report("\noutward right sides only (every free port on a new agent's principal port):", &|i| i.outward && i.quiet);
    println!("({:.1} s)", t0.elapsed().as_secs_f64());
}
