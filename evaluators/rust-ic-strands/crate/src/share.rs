//! Building the loaded term with its equal parts shared: a part that occurs more than once, of at
//! least `min` agents, is built once and read through duplicators, as if the program had bound it
//! to a name. Only the term decides what is shared, so what a run costs is still a function of the
//! program (research/OPTIMIZER.typ: memo is the optimizer's job, not the substrate's).

use rust_ca_lattice::net::{Net, Ref};
use rust_ca_lattice::oracle::Term;
use rust_ca_lattice::sup::STerm;
use rust_ca_lattice::rules::Tag;
use std::collections::HashMap;

/// One distinct part: its kind (0 leaf, 1 stem, 2 fork, 3 application), its parts, and how many
/// agents it is built apart.
struct Part { kind: u8, kids: [usize; 2], size: u64 }

/// The term built and wrapped for normalization (`Net::drive`): the net and its root `Out`.
pub fn net(t: &Term, min: usize) -> (Net, u32) {
    let mut net = Net::new();
    let root = if min == 0 { net.build(t) } else { build(&mut net, t, min) };
    let (_, out) = net.drive(root);
    (net, out)
}

/// `net` for a term that may hold superpositions; one that does is built whole, with no sharing.
pub fn net_sup(t: &STerm, min: usize) -> (Net, u32) {
    if let Some(t) = t.plain() { return net(&t, min); }
    let mut net = Net::new();
    let root = rust_ca_lattice::sup::build(&mut net, t);
    let (_, out) = net.drive(root);
    (net, out)
}

/// Build `t` with every part of at least `min` agents that occurs more than once built once.
/// Returns the root agent, whose port 0 is the output.
pub fn build(net: &mut Net, t: &Term, min: usize) -> u32 {
    let (mut parts, mut by_key, mut by_ptr) = (vec![], HashMap::new(), HashMap::new());
    let root = intern(t, &mut parts, &mut by_key, &mut by_ptr);
    // How many places read each part once shared parts are built once: parts are numbered after
    // their own parts, so going down the numbers visits every reader first.
    let mut refs = vec![0u64; parts.len()];
    let mut shared = vec![false; parts.len()];
    refs[root] = 1;
    for i in (0..parts.len()).rev() {
        if refs[i] == 0 { continue; }
        shared[i] = refs[i] > 1 && parts[i].size >= min as u64;
        let copies = if shared[i] { 1 } else { refs[i] };
        let p = &parts[i];
        for &k in &p.kids[..arity(p.kind)] { refs[k] += copies; }
    }
    let mut outs: HashMap<usize, Vec<Ref>> = HashMap::new();
    let r = emit(net, root, &parts, &refs, &shared, &mut outs);
    debug_assert_eq!(r.1, 0);
    r.0
}

fn arity(kind: u8) -> usize { [0, 1, 2, 2][kind as usize] }

fn intern(t: &Term, parts: &mut Vec<Part>, by_key: &mut HashMap<(u8, usize, usize), usize>, by_ptr: &mut HashMap<*const Term, usize>) -> usize {
    if let Some(&i) = by_ptr.get(&(t as *const Term)) { return i; }
    let (kind, kids) = match t {
        Term::L => (0, [0, 0]),
        Term::S(a) => (1, [intern(a, parts, by_key, by_ptr), 0]),
        Term::F(a, b) => (2, [intern(a, parts, by_key, by_ptr), intern(b, parts, by_key, by_ptr)]),
        Term::Ap(a, b) => (3, [intern(a, parts, by_key, by_ptr), intern(b, parts, by_key, by_ptr)]),
    };
    let i = *by_key.entry((kind, kids[0], kids[1])).or_insert_with(|| {
        let size = 1 + kids[..arity(kind)].iter().map(|&k| parts[k].size).fold(0u64, u64::saturating_add);
        parts.push(Part { kind, kids, size });
        parts.len() - 1
    });
    by_ptr.insert(t as *const Term, i);
    i
}

/// An output carrying part i: the part built afresh, or for a shared part the next free copy of it.
fn emit(net: &mut Net, i: usize, parts: &[Part], refs: &[u64], shared: &[bool], outs: &mut HashMap<usize, Vec<Ref>>) -> Ref {
    if shared[i] {
        if !outs.contains_key(&i) {
            let v = make(net, i, parts, refs, shared, outs);
            let copies = fan(net, (v, 0), refs[i]);
            outs.insert(i, copies);
        }
        return outs.get_mut(&i).unwrap().pop().expect("a copy for every reader");
    }
    (make(net, i, parts, refs, shared, outs), 0)
}

fn make(net: &mut Net, i: usize, parts: &[Part], refs: &[u64], shared: &[bool], outs: &mut HashMap<usize, Vec<Ref>>) -> u32 {
    let p = &parts[i];
    let a = net.mk([Tag::L, Tag::S, Tag::F, Tag::P][p.kind as usize]);
    for (q, &k) in p.kids[..arity(p.kind)].iter().enumerate() {
        let (b, r) = emit(net, k, parts, refs, shared, outs);
        net.link(a, q as u8 + 1, b, r);
    }
    a
}

/// `n` copies of what `src` carries, through a balanced tree of duplicators.
fn fan(net: &mut Net, src: Ref, n: u64) -> Vec<Ref> {
    if n <= 1 { return vec![src]; }
    let d = net.mk(Tag::Dn);
    net.link(d, 0, src.0, src.1);
    let mut v = fan(net, (d, 1), n / 2);
    v.extend(fan(net, (d, 2), n - n / 2));
    v
}
