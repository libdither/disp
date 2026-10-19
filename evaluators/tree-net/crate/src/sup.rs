//! Superposed terms: inputs with superpositions in chosen places, and answers holding some.
//! `&ℓ{a,b}` is a in one universe and b in the other, and a label picks the same side
//! everywhere, so a term holding labels ℓ₁…ℓₙ stands for 2ⁿ plain terms, one per universe
//! (rules.rs `SUP_RULES`). The oracle stays plain: a superposed answer is checked against it
//! universe by universe.

use crate::net::{Net, Ref};
use crate::oracle::{self, Fuel, Lcg, Term};
use crate::rules::Tag;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum STerm {
    L,
    S(Rc<STerm>),
    F(Rc<STerm>, Rc<STerm>),
    Ap(Rc<STerm>, Rc<STerm>),
    Sup(u8, Rc<STerm>, Rc<STerm>),
}

pub fn ap(f: STerm, x: STerm) -> STerm { STerm::Ap(Rc::new(f), Rc::new(x)) }
pub fn sup(label: u8, a: STerm, b: STerm) -> STerm { STerm::Sup(label, Rc::new(a), Rc::new(b)) }

impl From<&Term> for STerm {
    fn from(t: &Term) -> STerm {
        match t {
            Term::L => STerm::L,
            Term::S(a) => STerm::S(Rc::new((&**a).into())),
            Term::F(a, b) => STerm::F(Rc::new((&**a).into()), Rc::new((&**b).into())),
            Term::Ap(a, b) => STerm::Ap(Rc::new((&**a).into()), Rc::new((&**b).into())),
        }
    }
}

impl STerm {
    /// The term in one universe: `second(ℓ)` says whether label ℓ takes its second side.
    pub fn pick(&self, second: &dyn Fn(u8) -> bool) -> Term {
        match self {
            STerm::L => Term::L,
            STerm::S(a) => oracle::s(a.pick(second)),
            STerm::F(a, b) => oracle::f2(a.pick(second), b.pick(second)),
            STerm::Ap(a, b) => oracle::ap(a.pick(second), b.pick(second)),
            STerm::Sup(l, a, b) => if second(*l) { b.pick(second) } else { a.pick(second) },
        }
    }

    /// The labels it holds, each once, smallest first.
    pub fn labels(&self) -> Vec<u8> {
        fn go(t: &STerm, out: &mut Vec<u8>) {
            match t {
                STerm::L => {}
                STerm::S(a) => go(a, out),
                STerm::F(a, b) | STerm::Ap(a, b) => { go(a, out); go(b, out); }
                STerm::Sup(l, a, b) => { out.push(*l); go(a, out); go(b, out); }
            }
        }
        let mut v = vec![];
        go(self, &mut v);
        v.sort_unstable();
        v.dedup();
        v
    }

    /// The plain term, when it holds no superposition.
    pub fn plain(&self) -> Option<Term> { self.labels().is_empty().then(|| self.pick(&|_| false)) }

    /// Every universe of these labels: which take their second side (bit i for `labels[i]`), and
    /// the term there.
    pub fn universes(&self, labels: &[u8]) -> Vec<(u32, Term)> {
        (0..1u32 << labels.len()).map(|bits| {
            let second = |l: u8| labels.iter().position(|&x| x == l).is_some_and(|i| bits >> i & 1 == 1);
            (bits, self.pick(&second))
        }).collect()
    }

    /// What it collapses to: the term of each of its universes, each once.
    pub fn answers(&self) -> Vec<Term> {
        let mut v: Vec<Term> = vec![];
        for (_, t) in self.universes(&self.labels()) { if !v.contains(&t) { v.push(t); } }
        v
    }
}

/// Universes as one superposed term (`STerm::universes` over these labels, in order): a
/// superposition of the first label's two sides, each the rest over the other labels, and just
/// one side where the two are equal. Two answers agree in every universe exactly when their
/// collapsed terms are equal.
pub fn collapse(universes: &[(u32, Term)], labels: &[u8]) -> STerm {
    fn go(u: &[(u32, Term)], labels: &[u8], i: usize, bits: u32) -> STerm {
        if i == labels.len() { return u.iter().find(|(b, _)| *b == bits).map_or(STerm::L, |(_, t)| t.into()); }
        let (a, b) = (go(u, labels, i + 1, bits), go(u, labels, i + 1, bits | 1 << i));
        if a == b { a } else { sup(labels[i], a, b) }
    }
    go(universes, labels, 0, 0)
}

/// `L`, `S(x)`, `F(x,y)` and `@(f,x)` as the oracle prints them, and `&ℓ{a,b}`.
pub fn show(t: &STerm) -> String {
    match t {
        STerm::L => "L".into(),
        STerm::S(a) => format!("S({})", show(a)),
        STerm::F(a, b) => format!("F({},{})", show(a), show(b)),
        STerm::Ap(a, b) => format!("@({},{})", show(a), show(b)),
        STerm::Sup(l, a, b) => format!("&{l}{{{},{}}}", show(a), show(b)),
    }
}

/// Build a superposed term as `Net::build` builds a plain one, superpositions as `Sup` agents
/// carrying their labels. Returns the root, whose port 0 is the output.
pub fn build(net: &mut Net, t: &STerm) -> u32 {
    let (tag, label, kids): (Tag, u8, Vec<&STerm>) = match t {
        STerm::L => (Tag::L, 0, vec![]),
        STerm::S(a) => (Tag::S, 0, vec![a]),
        STerm::F(a, b) => (Tag::F, 0, vec![a, b]),
        STerm::Ap(a, b) => (Tag::P, 0, vec![a, b]),
        STerm::Sup(l, a, b) => (Tag::Sup, *l, vec![a, b]),
    };
    let id = net.mk_labelled(tag, label);
    for (q, k) in kids.into_iter().enumerate() {
        let c = build(net, k);
        net.link(id, q as u8 + 1, c, 0);
    }
    id
}

/// The value hanging off a ref, superpositions included, as `Net::readback` reads a plain one.
pub fn read(net: &Net, r: Option<Ref>) -> Option<STerm> {
    let (id, _) = r?;
    let a = net.agents.get(id as usize)?.as_ref()?;
    let kid = |q: usize| read(net, a.ports[q]).map(Rc::new);
    Some(match a.tag {
        Tag::L => STerm::L,
        Tag::S => STerm::S(kid(1)?),
        Tag::F => STerm::F(kid(1)?, kid(2)?),
        Tag::Sup => STerm::Sup(a.label, kid(1)?, kid(2)?),
        _ => return None,
    })
}

/// Build, drive, reduce and read back, as `net::normalize` does.
pub fn normalize(t: &STerm, budget: u64) -> (Option<STerm>, bool, u64) {
    let mut net = Net::new();
    let root = build(&mut net, t);
    let (_nrm, out) = net.drive(root);
    let done = net.reduce(budget);
    (read(&net, net.get(out).ports[0]), done, net.ints)
}

/// The oracle's answer in every universe of the input (`STerm::universes` over its labels), or
/// None when it runs out of fuel in one.
pub fn oracle_answers(input: &STerm, fuel: u64) -> Option<Vec<(u32, Term)>> {
    input.universes(&input.labels()).into_iter().map(|(u, t)| Some((u, oracle::nf(t, &mut Fuel(fuel)).ok()?))).collect()
}

/// Whether an answer is the oracle's (`oracle_answers`) in every universe of the input: Ok, or
/// the first universe where it is not.
pub fn check(input: &STerm, want: &[(u32, Term)], answer: &STerm) -> Result<(), String> {
    let labels = input.labels();
    for ((u, t), (_, w)) in answer.universes(&labels).iter().zip(want) {
        if t != w {
            let side = labels.iter().enumerate().map(|(i, l)| format!("&{l}={}", u >> i & 1)).collect::<Vec<_>>().join(" ");
            return Err(format!("in the universe {side}: {} where the oracle has {}", oracle::show(t), oracle::show(w)));
        }
    }
    Ok(())
}

impl Lcg {
    /// A random term as `rand_term` makes them, each node a superposition `&ℓ{x,y}` of two random
    /// terms with chance p, ℓ one of 1..=labels (so labels repeat, and a repeated label must pick
    /// the same side everywhere).
    pub fn rand_sup_term(&mut self, depth: u32, labels: u8, p: f64) -> STerm {
        if depth > 0 && self.next() < p {
            let l = 1 + (self.next() * labels as f64) as u8 % labels;
            return sup(l, self.rand_sup_term(depth - 1, labels, p), self.rand_sup_term(depth - 1, labels, p));
        }
        if depth == 0 || self.next() < 0.35 { return STerm::L; }
        ap(self.rand_sup_term(depth - 1, labels, p), self.rand_sup_term(depth - 1, labels, p))
    }
}
