//! Keeping history on the strand lattice (`Params::resid`, rules.rs `RESID_RULES`): every application
//! of the loaded term is an apply that leaves what it would have dropped, and behind gates waits the
//! run back, built as the term's mirror (research/reversible-core/residual.py is the model this runs).
//!
//! The net for a term t with the history kept:
//! - forward, t as it is, every application an apply (`A+`) whose residual goes into a gate, and a
//!   normalizer reading the answer into a gate;
//! - behind the gates, t's mirror: an un-apply (`A-`) for each application, reading its residual and
//!   what came of it and handing what it gives back to the mirrors of its function and argument; a
//!   fork or stem of t holding applications taken apart (`Fm`, `Sm`); and every value of t that holds
//!   none read out at a root (`Nrm`, `Out`), in the order t has them.
//!
//! Opening the gates (`Lattice::open_gates`) sets the run back going. With the history erased instead,
//! each residual goes to an eraser as it is made and the answer to `Out`.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::rules::Tag;

/// What a gate holds shut, or a root reads: the answer, application i's residual (in the order the
/// term has its applications), value i of the term (in its order).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Role { Answer, History(u32), Part(u32) }

/// The gates and roots of a loaded net, by agent id, and the term's values, to compare what comes back.
#[derive(Clone, Debug, Default)]
pub struct Parts { pub gates: Vec<(u32, Role)>, pub roots: Vec<(u32, Role)>, pub values: Vec<Term> }

/// The term as built: a value holding no application (its number), an application (its apply and
/// number), or a fork or stem holding applications.
enum Built { Value(u32), Apply(u32, u32, Box<Built>, Box<Built>), Stem(Box<Built>), Fork(Box<Built>, Box<Built>) }

fn plain(t: &Term) -> bool {
    match t { Term::L => true, Term::S(a) => plain(a), Term::F(a, b) => plain(a) && plain(b), Term::Ap(..) => false }
}

/// The net for t, with the history kept behind gates or erased as it is made. Returns the net, the root
/// to lay it out from, and its gates and roots.
pub fn net(t: &Term, keep: bool) -> (Net, u32, Parts) {
    let mut net = Net::new();
    let mut parts = Parts::default();
    let (mut applies, mut at) = (0u32, (0u32, 0u8));
    let built = build(&mut net, t, keep, &mut applies, &mut parts.values, &mut at);
    let nrm = net.mk(Tag::Nrm);
    net.link(nrm, 0, at.0, at.1);
    if !keep {
        let out = net.mk(Tag::Out);
        net.link(nrm, 1, out, 0);
        parts.roots.push((out, Role::Answer));
        return (net, out, parts);
    }
    let answer = gate(&mut net, (nrm, 1), Role::Answer, &mut parts);
    unbuild(&mut net, (answer, 1), &built, &mut parts);
    let out = parts.roots[0].0;
    (net, out, parts)
}

/// t built forward; `at` is set to the port giving its value.
fn build(net: &mut Net, t: &Term, keep: bool, applies: &mut u32, values: &mut Vec<Term>, at: &mut (u32, u8)) -> Built {
    if plain(t) {
        *at = (net.build(t), 0);
        values.push(t.clone());
        return Built::Value(values.len() as u32 - 1);
    }
    let mut kid = |net: &mut Net, t: &Term, into: (u32, u8)| { let mut a = (0, 0); let b = build(net, t, keep, applies, values, &mut a); net.link(into.0, into.1, a.0, a.1); b };
    match t {
        Term::Ap(f, x) => {
            let ra = net.mk(Tag::RA);
            let (bf, bx) = (kid(net, f, (ra, 0)), kid(net, x, (ra, 1)));
            if !keep { let e = net.mk(Tag::Eps); net.link(ra, 3, e, 0); }
            *at = (ra, 2);
            *applies += 1;
            Built::Apply(ra, *applies - 1, Box::new(bf), Box::new(bx))
        }
        Term::S(c) => { let s = net.mk(Tag::S); let b = kid(net, c, (s, 1)); *at = (s, 0); Built::Stem(Box::new(b)) }
        Term::F(l, r) => { let f = net.mk(Tag::F); let (bl, br) = (kid(net, l, (f, 1)), kid(net, r, (f, 2))); *at = (f, 0); Built::Fork(Box::new(bl), Box::new(br)) }
        Term::L => unreachable!("a leaf is plain"),
    }
}

/// The mirror of what was built, reading what comes back at `from`.
fn unbuild(net: &mut Net, from: (u32, u8), b: &Built, parts: &mut Parts) {
    match b {
        Built::Value(i) => {
            let (n, o) = (net.mk(Tag::Nrm), net.mk(Tag::Out));
            net.link(n, 0, from.0, from.1);
            net.link(n, 1, o, 0);
            parts.roots.push((o, Role::Part(*i)));
        }
        Built::Apply(ra, i, f, x) => {
            let h = gate(net, (*ra, 3), Role::History(*i), parts);
            let ua = net.mk(Tag::UA);
            net.link(ua, 0, h, 1);
            net.link(ua, 1, from.0, from.1);
            unbuild(net, (ua, 3), f, parts);
            unbuild(net, (ua, 2), x, parts);
        }
        Built::Stem(c) => { let m = net.mk(Tag::Sm); net.link(m, 0, from.0, from.1); unbuild(net, (m, 1), c, parts); }
        Built::Fork(l, r) => { let m = net.mk(Tag::Fm); net.link(m, 0, from.0, from.1); unbuild(net, (m, 1), l, parts); unbuild(net, (m, 2), r, parts); }
    }
}

fn gate(net: &mut Net, from: (u32, u8), role: Role, parts: &mut Parts) -> u32 {
    let g = net.mk(Tag::Gate);
    net.link(g, 0, from.0, from.1);
    parts.gates.push((g, role));
    g
}
