//! Adequacy by gadgets (Stage 3 of research/COMBINATOR_SEARCH_PLAN.md): a system runs Lafont's
//! interaction combinators when one small net of its agents can stand for γ, another for δ, its eraser
//! stands for ε, and each combinator rule holds with its two sides built from these: the left side
//! reduces to the right side. Lafont translates every interaction system into the combinators, so a
//! system that passes runs the tree calculus's own net (evaluators/tree-net) too.

use crate::live::{canon, port, reduce, Live, Piece, System, FREE_KIND};
use crate::space::{self, Sig};

/// Pieces with three free ports that may stand for γ or δ: port 0 on an agent's principal port, ports 1
/// and 2 on auxiliary ports (so they meet nothing the combinator would not), and no active pair inside.
pub fn gadgets(sig: &Sig, max: usize) -> Vec<Piece> {
    let pool: Vec<u8> = (0..sig.kinds()).collect();
    space::pieces(sig, &pool, 3, max)
        .into_iter()
        .filter(|p| {
            let pr = space::principal(sig, p);
            let at = |f: usize| p.partner[f] as usize;
            (0..3).all(|f| at(f) >= 3) && pr[at(0)] && !pr[at(1)] && !pr[at(2)] && space::quiet(sig, p)
        })
        .collect()
}

#[derive(Clone, Copy, Default, Debug)]
pub struct Cost {
    pub interactions: u64,
    pub marks: u64,
    pub erasures: u64,
}

impl std::ops::Add for Cost {
    type Output = Cost;
    fn add(self, o: Cost) -> Cost {
        Cost { interactions: self.interactions + o.interactions, marks: self.marks + o.marks, erasures: self.erasures + o.erasures }
    }
}

/// Reduces a small net (at most 200 interactions and 64 agents) and returns its normal form.
fn run(sys: &System, net: &mut Live, iface: &[u32]) -> Option<(Vec<u32>, Cost)> {
    let st = reduce(sys, net, 200, 64);
    st.halted.then(|| (canon(net, iface), Cost { interactions: st.interactions, marks: st.marks, erasures: st.erasures }))
}

/// Four interface ports wired in pairs.
fn wires(pairs: [(usize, usize); 2]) -> Vec<u32> {
    let mut net = Live::default();
    let iface: Vec<u32> = (0..4).map(|_| net.add(FREE_KIND)).collect();
    for (a, b) in pairs {
        net.link(port(iface[a], 0), port(iface[b], 0));
    }
    canon(&net, &iface)
}

/// γ ⋈ γ: two copies of g, principal to principal, must leave only the wires joining their auxiliary
/// ports, straight (first to first) or crossed. True if crossed.
fn annihilation(sys: &System, sig: &Sig, g: &Piece) -> Option<(bool, Cost)> {
    let mut net = Live::default();
    let a = net.place(&sig.arity, g);
    let b = net.place(&sig.arity, g);
    net.link(a[0], b[0]);
    let iface = [net.tap(a[1]), net.tap(a[2]), net.tap(b[1]), net.tap(b[2])];
    let (c, cost) = run(sys, &mut net, &iface)?;
    if c == wires([(0, 2), (1, 3)]) {
        Some((false, cost))
    } else if c == wires([(0, 3), (1, 2)]) {
        Some((true, cost))
    } else {
        None
    }
}

/// ε ⋈ γ: an eraser on g's principal port must leave an eraser on each auxiliary port.
fn erasure(sys: &System, sig: &Sig, g: &Piece) -> Option<Cost> {
    let e = sig.eraser?;
    let mut net = Live::default();
    let a = net.place(&sig.arity, g);
    let x = net.add(e);
    net.link(port(x, 0), a[0]);
    let iface = [net.tap(a[1]), net.tap(a[2])];
    let (c, cost) = run(sys, &mut net, &iface)?;
    let mut want = Live::default();
    let wi: Vec<u32> = (0..2).map(|_| want.add(FREE_KIND)).collect();
    for &i in &wi {
        let y = want.add(e);
        want.link(port(i, 0), port(y, 0));
    }
    (c == canon(&want, &wi)).then_some(cost)
}

/// γ ⋈ δ: Lafont's commutation. γ's auxiliary ports get a copy of δ each and δ's a copy of γ each, the
/// i-th δ's j-th auxiliary port wired to the j-th γ's i-th.
fn commutation(sys: &System, sig: &Sig, g: &Piece, d: &Piece) -> Option<Cost> {
    let mut net = Live::default();
    let a = net.place(&sig.arity, g);
    let b = net.place(&sig.arity, d);
    net.link(a[0], b[0]);
    let iface = [net.tap(a[1]), net.tap(a[2]), net.tap(b[1]), net.tap(b[2])];
    let (c, cost) = run(sys, &mut net, &iface)?;
    let mut want = Live::default();
    let (d0, d1) = (want.place(&sig.arity, d), want.place(&sig.arity, d));
    let (g0, g1) = (want.place(&sig.arity, g), want.place(&sig.arity, g));
    want.link(d0[1], g0[1]);
    want.link(d0[2], g1[1]);
    want.link(d1[1], g0[2]);
    want.link(d1[2], g1[2]);
    let wi = [want.tap(d0[0]), want.tap(d1[0]), want.tap(g0[0]), want.tap(g1[0])];
    (c == canon(&want, &wi)).then_some(cost)
}

/// One way a system runs the combinators.
#[derive(Clone, Debug)]
pub struct Sim {
    pub gamma: usize,
    pub delta: usize,
    /// Whether γ ⋈ γ and δ ⋈ δ cross their wires.
    pub crossed: (bool, bool),
    /// γγ, δδ, γδ, γε, δε.
    pub cost: [Cost; 5],
}

/// Every pair of gadgets (γ before δ in the list) through which the system runs the combinators.
pub fn simulate(sys: &System, sig: &Sig, gadgets: &[Piece]) -> Vec<Sim> {
    let mut ok = vec![];
    for (i, g) in gadgets.iter().enumerate() {
        if let Some((crossed, c)) = annihilation(sys, sig, g) {
            if let Some(e) = erasure(sys, sig, g) {
                ok.push((i, crossed, c, e));
            }
        }
    }
    let mut out = vec![];
    for x in 0..ok.len() {
        for y in x + 1..ok.len() {
            let (i, ci, ann_i, er_i) = ok[x];
            let (j, cj, ann_j, er_j) = ok[y];
            if let Some(c) = commutation(sys, sig, &gadgets[i], &gadgets[j]) {
                out.push(Sim { gamma: i, delta: j, crossed: (ci, cj), cost: [ann_i, ann_j, c, er_i, er_j] });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn combinators(sig: &Sig) -> System {
        let straight = Piece { kinds: vec![], partner: vec![2, 3, 0, 1], free: 4 };
        let mut partner = vec![0u8; 16];
        for (a, b) in [(0usize, 4usize), (1, 7), (2, 10), (3, 13), (5, 11), (6, 14), (8, 12), (9, 15)] {
            partner[a] = b as u8;
            partner[b] = a as u8;
        }
        let commute = Piece { kinds: vec![1, 1, 0, 0], partner, free: 4 };
        space::system(sig, &[&straight, &commute, &straight])
    }

    #[test]
    fn combinators_run_themselves() {
        let sig = Sig::new("AB", &[2, 2], true);
        let sys = combinators(&sig);
        let gs = gadgets(&sig, 1);
        let sims = simulate(&sys, &sig, &gs);
        assert!(sims.iter().any(|s| gs[s.gamma].kinds == [0] && gs[s.delta].kinds == [1] && s.crossed == (false, false)));
        assert!(simulate(&sys, &sig, &gadgets(&sig, 3)).len() >= sims.len());
    }

    #[test]
    fn renamed_combinators_still_run() {
        let sig = Sig::new("AB", &[2, 2], true);
        let sys = combinators(&sig);
        let gs = gadgets(&sig, 1);
        let mut keys = std::collections::HashSet::new();
        for (perm, swap) in space::renamings(&sig) {
            let rules: Vec<Piece> = sig
                .pairs()
                .into_iter()
                .map(|(a, b)| {
                    // the pair that the renaming makes into a ⋈ b
                    let image = |u: u8, v: u8| (perm[u as usize].min(perm[v as usize]), perm[u as usize].max(perm[v as usize]));
                    let (u, v) = sig.pairs().into_iter().find(|&(u, v)| image(u, v) == (a, b)).unwrap();
                    space::rename_rule(&sig, u, v, sys.rules[sys.pair(u, v)].as_ref().unwrap(), &perm, &swap)
                })
                .collect();
            let renamed = space::system(&sig, &rules.iter().collect::<Vec<_>>());
            assert!(!simulate(&renamed, &sig, &gs).is_empty(), "renaming {perm:?} {swap:?}");
            keys.insert(space::system_key(&sig, &rules.iter().collect::<Vec<_>>()));
        }
        assert_eq!(keys.len(), 1);
    }

    #[test]
    fn wires_through_the_pair() {
        let sig = Sig::new("AB", &[2, 2], true);
        let sys = combinators(&sig);
        // x's auxiliary ports joined to each other: straight annihilation joins what lies at y's
        let mut net = Live::default();
        let (x, y) = (net.add(0), net.add(0));
        net.link(port(x, 0), port(y, 0));
        net.link(port(x, 1), port(x, 2));
        let iface = [net.tap(port(y, 1)), net.tap(port(y, 2))];
        reduce(&sys, &mut net, 10, 10);
        let mut want = Live::default();
        let wi = [want.add(FREE_KIND), want.add(FREE_KIND)];
        want.link(port(wi[0], 0), port(wi[1], 0));
        assert_eq!(canon(&net, &iface), canon(&want, &wi));
        // every auxiliary port on the other agent's: two loops of wire, nothing left
        let mut net = Live::default();
        let (x, y) = (net.add(1), net.add(1));
        net.link(port(x, 0), port(y, 0));
        net.link(port(x, 1), port(y, 1));
        net.link(port(x, 2), port(y, 2));
        let st = reduce(&sys, &mut net, 10, 10);
        assert!(st.halted && net.agents == 0 && canon(&net, &[]).is_empty());
    }
}
