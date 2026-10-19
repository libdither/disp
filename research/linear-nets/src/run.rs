//! EXPERIMENTAL: two schedules for `net.rs`, with the work ledger split by rule and by where the
//! value a rewrite reads came from.
//! - `ideal`: the strand lattice's idealised lazy machine (rust-ic-strands ideal.rs `ideal_depth`,
//!   README "Fork"): demand at once, every wanted rewrite at once a round, erasers collecting as
//!   on the lattice, the S rule's two halves both wanted. Its rewrites are the lattice's rewrites.
//! - `all`: the abstract net (rust-ca-lattice net.rs `reduce`): every active pair fires, no
//!   collection, so a copy that is thrown away is still made in full.

use crate::net::{End, Fired, Net, Origin, Rule, Tag};
use std::collections::BTreeMap;

#[derive(Default, Clone)]
pub struct Ledger {
    /// Rewrites by rule name and the read value's origin (code, data, built by the run).
    pub by_rule: BTreeMap<String, [u64; 3]>,
    /// Collections: an eraser on a duplicator's output (it becomes a wire), on a computation's
    /// result (erasers on its inputs), on both outputs of an unpair.
    pub collect: [u64; 3],
    pub rewrites: u64,
    pub rounds: u64,
    /// S-rule firings (T1·S) by what the stem holds and the pair's first arm: s = K f (B shape),
    /// b = K g (C shape), s and b the same tree (W shape), other.
    pub s_shapes: [u64; 4],
    /// Pairs that fired, and those whose two agents one rewrite made together.
    pub pairs: u64,
    pub together: u64,
    /// Each use of a piece of loaded code (an apply, triage or normalizer reading a copy of it): the
    /// piece and the round; and each copy made of one (a duplicator reading it).
    pub uses: Vec<(u32, u64)>,
    pub copies: u64,
    /// Routing rewrites fired at once and for free (CHAIN).
    pub free: u64,
}

impl Ledger {
    pub fn count(&mut self, net: &Net, f: &Fired) {
        let name = match f.rule { usize::MAX => "Dn·Ref".to_string(), x if x == usize::MAX - 1 => "Eps·Ref".to_string(), i => net.rules[i].name() };
        self.by_rule.entry(name).or_default()[f.origin as usize] += 1;
        self.rewrites += 1;
    }
    pub fn consumer(&self, c: &str) -> [u64; 3] {
        let mut s = [0; 3];
        for (k, v) in &self.by_rule { if k.split('·').next() == Some(c) { for i in 0..3 { s[i] += v[i]; } } }
        s
    }
    pub fn total(&self) -> u64 { self.rewrites + self.collect.iter().sum::<u64>() }
}

/// Which fresh agents of a rule start wanted (rust-ic-strands lattice.rs `fresh_wanted_in`, with
/// `inputs`: the S rule's `(b c)` and the duplicator sharing `c` start wanted too).
pub fn fresh_wanted(rule: &Rule, inputs: bool) -> Vec<bool> {
    let mut wanted: Vec<bool> = rule.fresh.iter().map(|t| matches!(t, Tag::Nrm | Tag::Eps)).collect();
    loop {
        let mut changed = false;
        for &(a, b) in &rule.wires {
            for (x, y) in [(a, b), (b, a)] {
                let End::X(f, p) = x else { continue };
                let tf = rule.fresh[f as usize];
                if wanted[f as usize] || !tf.is_consumer() || p == 0 || !tf.is_source(p as usize) { continue; }
                let feeds = match y {
                    End::C(i) => rule.consumer.is_source(i as usize),
                    End::X(g, 0) => rule.fresh[g as usize].is_consumer() && wanted[g as usize],
                    End::X(g, 1) if inputs => matches!(rule.fresh[g as usize], Tag::A | Tag::T1 | Tag::Sel) && wanted[g as usize],
                    _ => false,
                };
                if feeds { wanted[f as usize] = true; changed = true; }
            }
        }
        if !changed { return wanted; }
    }
}

/// The value agent a port leads to, seeing through duplicators (a copy is the value it copies).
fn value_at(net: &Net, mut p: Option<(u32, u8)>) -> Option<u32> {
    for _ in 0..10_000 {
        let (id, q) = p?;
        let a = net.agents[id as usize].as_ref()?;
        if a.tag == Tag::Dn && q > 0 { p = a.ports[0]; continue; }
        return (q == 0 && a.tag.is_producer()).then_some(id);
    }
    None
}

/// The tree a port leads to, through duplicators, when it is all values (at most `budget` nodes).
fn tree_at(net: &Net, p: Option<(u32, u8)>, budget: &mut u32) -> Option<rust_ca_lattice::oracle::Term> {
    use rust_ca_lattice::oracle::Term;
    if *budget == 0 { return None; }
    *budget -= 1;
    let id = value_at(net, p)?;
    let a = net.get(id);
    let mut kid = |q: usize| tree_at(net, a.ports[q], budget).map(std::rc::Rc::new);
    match a.tag {
        Tag::L => Some(Term::L),
        Tag::S => Some(Term::S(kid(1)?)),
        Tag::F => { let l = kid(1)?; Some(Term::F(l, kid(2)?)) }
        _ => None,
    }
}

/// An S-rule firing (T1·S) by the shape of what it shares: s = K f (a B shape), b = K g (a C
/// shape), s and b the same tree (a W shape), or a real S.
fn peek_s_shape(net: &Net, consumer: u32, producer: u32) -> usize {
    let is_k = |p: Option<(u32, u8)>| value_at(net, p).is_some_and(|f| net.get(f).tag == Tag::F && value_at(net, net.get(f).ports[1]).is_some_and(|l| net.get(l).tag == Tag::L));
    let s = net.get(producer).ports[1];
    let b = value_at(net, net.get(consumer).ports[1]).filter(|&x| net.get(x).tag == Tag::Pair).and_then(|x| net.get(x).ports[1]);
    if is_k(s) { 0 } else if is_k(b) { 1 } else {
        let (ts, tb) = (tree_at(net, s, &mut 4096), tree_at(net, b, &mut 4096));
        if ts.is_some() && ts == tb { 2 } else { 3 }
    }
}

/// Eraser `e` reads output `q` of `b`: collect it as the lattice does. Whether it did.
fn collect(net: &mut Net, e: u32, b: u32, q: u8, want: &mut Vec<bool>, live: &mut Vec<u32>, ledger: &mut Ledger) -> bool {
    let a = net.get(b).clone();
    let mut eps = |net: &mut Net, r: (u32, u8), want: &mut Vec<bool>| {
        let x = net.mk(Tag::Eps, Origin::Built);
        net.link(x, 0, r.0, r.1);
        want.resize(net.agents.len(), false);
        want[x as usize] = true;
        live.push(x);
    };
    match a.tag {
        Tag::Dn if q > 0 => {
            let (src, dst) = (a.ports[0].unwrap(), a.ports[3 - q as usize].unwrap());
            if src == (b, 3 - q) { return false; }
            net.kill(e);
            net.kill(b);
            net.link(src.0, src.1, dst.0, dst.1);
            ledger.collect[0] += 1;
        }
        Tag::SelRef | Tag::Unw if q == 1 => {
            net.kill(e);
            net.kill(b);
            eps(net, a.ports[0].unwrap(), want);
            ledger.collect[1] += 1;
        }
        Tag::A | Tag::T1 | Tag::Sel if q == 2 => {
            net.kill(e);
            net.kill(b);
            eps(net, a.ports[0].unwrap(), want);
            eps(net, a.ports[1].unwrap(), want);
            ledger.collect[1] += 1;
        }
        Tag::Unp if q > 0 => {
            let (o, _) = a.ports[3 - q as usize].unwrap();
            if o == e || net.get(o).tag != Tag::Eps { return false; }
            for x in [e, o, b] { net.kill(x); }
            eps(net, a.ports[0].unwrap(), want);
            ledger.collect[2] += 1;
        }
        _ => return false,
    }
    true
}

/// The new rules, before the 26 (and the table's plain unfold) get a turn: a table triage reading
/// what it dispatches on, an apply reading table code, and the peeking S rule of modes 1 and 2.
/// Returns the fresh agents and those an unfold made, if one of them fired.
pub fn native_step(net: &mut Net, c: u32, b: u32, ledger: &mut Ledger) -> Option<(Vec<u32>, Vec<u32>)> {
    let (ct, bt) = (net.get(c).tag, net.get(b).tag);
    if ct == Tag::Unw {
        let fresh = net.unwrap_step(c, b);
        ledger.by_rule.entry("Unw".into()).or_default()[0] += 1;
        ledger.rewrites += 1;
        return Some((fresh, vec![]));
    }
    if ct == Tag::A && bt == Tag::F && net.hints {
        if let Some(fresh) = net.hinted_apply(c, b) {
            ledger.by_rule.entry("A·F hinted".into()).or_default()[0] += 1;
            ledger.rewrites += 1;
            return Some((fresh, vec![]));
        }
    }
    if ct == Tag::SelRef {
        let fresh = net.sel_ref(c, b);
        ledger.by_rule.entry("SelRef".into()).or_default()[0] += 1;
        ledger.rewrites += 1;
        return Some((fresh, vec![]));
    }
    if ct == Tag::T1 && bt == Tag::Ref && net.native >= 5 {
        let fresh = net.t1_ref(c, b);
        ledger.by_rule.entry("T1·Ref native".into()).or_default()[0] += 1;
        ledger.rewrites += 1;
        return Some((fresh, vec![]));
    }
    if ct == Tag::A && bt == Tag::Ref && net.native >= 3 {
        let fresh = net.native_apply(c, b)?;
        ledger.by_rule.entry("A·Ref native".into()).or_default()[0] += 1;
        ledger.rewrites += 1;
        return Some((fresh, vec![]));
    }
    if ct == Tag::T1 && (net.native == 1 || net.native == 2) {
        let (mut b, mut made) = (b, vec![]);
        if bt == Tag::Ref && net.rom.nodes[net.get(b).rom as usize].0 == Tag::S { let (n, m) = net.unfold(b); b = n; made = m; }
        if net.get(b).tag != Tag::S { return None; }
        if let Some((fresh, extra)) = net.native_s(c, b) {
            ledger.by_rule.entry("T1·S native".into()).or_default()[2] += 1;
            ledger.rewrites += extra;
            return Some((fresh, made));
        }
    }
    None
}

/// Every port wired both ways, and every wire joins a port that supplies a value to one that needs it.
pub fn check(net: &Net) -> Result<(), String> {
    for (i, a) in net.agents.iter().enumerate() {
        let Some(a) = a else { continue };
        for p in 0..a.tag.arity() {
            let Some((b, q)) = a.ports[p] else { return Err(format!("{}#{i} port {p} open", a.tag.name())) };
            let Some(bb) = net.agents[b as usize].as_ref() else { return Err(format!("{}#{i} port {p} to a dead agent", a.tag.name())) };
            if bb.ports[q as usize] != Some((i as u32, p as u8)) { return Err(format!("{}#{i} port {p} one-way", a.tag.name())); }
            if a.tag.is_source(p) == bb.tag.is_source(q as usize) { return Err(format!("{}#{i}.{p} – {}#{b}.{q} joins two of a kind", a.tag.name(), bb.tag.name())); }
        }
    }
    Ok(())
}

/// The idealised lazy machine. Returns whether the root read back as a tree within `max_rounds`.
pub fn ideal(net: &mut Net, out: u32, fork: bool, max_rounds: u64, ledger: &mut Ledger) -> bool {
    let wanted_by_rule: Vec<Vec<bool>> = net.rules.iter().map(|r| fresh_wanted(r, fork)).collect();
    let mut want: Vec<bool> = net.agents.iter().map(|a| matches!(a, Some(a) if matches!(a.tag, Tag::Nrm | Tag::Out | Tag::Eps))).collect();
    let mut live: Vec<u32> = (0..net.agents.len() as u32).filter(|&i| net.agents[i as usize].is_some()).collect();
    for round in 0..max_rounds {
        if net.readback(net.get(out).ports[0]).is_some() { ledger.rounds = round; return true; }
        want.resize(net.agents.len(), false);
        let mut stack: Vec<u32> = live.iter().copied().filter(|&i| want[i as usize] && net.tag(i).is_some_and(|t| t.is_consumer() && t != Tag::Eps)).collect();
        while let Some(c) = stack.pop() {
            let Some((b, q)) = net.agents[c as usize].as_ref().and_then(|a| a.ports[0]) else { continue };
            let bt = net.tag(b).unwrap();
            if q > 0 && bt.is_consumer() && bt.is_source(q as usize) && !want[b as usize] { want[b as usize] = true; stack.push(b); }
        }
        let mut acted = false;
        for i in 0..live.len() {
            let c = live[i];
            let Some(a) = net.agents[c as usize].clone() else { continue };
            net.clock += 1;
            if !a.tag.is_consumer() || !want[c as usize] { continue; }
            let Some((b, q)) = a.ports[0] else { continue };
            let bt = net.tag(b).unwrap();
            if q == 0 && bt.is_producer() {
                let code = net.get(b).code;
                if code != u32::MAX {
                    if matches!(a.tag, Tag::A | Tag::T1 | Tag::Sel | Tag::Nrm) { ledger.uses.push((code, round)); }
                    if a.tag == Tag::Dn { ledger.copies += 1; }
                }
                let (bc, bp) = (net.birth(c), net.birth(b));
                if bc == bp && bc > 0 { ledger.together += 1; }
                ledger.pairs += 1;
                if a.tag == Tag::T1 && bt == Tag::S { ledger.s_shapes[peek_s_shape(net, c, b)] += 1; }
                if let Some((fresh, unfolded)) = native_step(net, c, b, ledger) {
                    want.resize(net.agents.len(), false);
                    for &x in &fresh { if net.tag(x).is_some_and(|t| t.is_consumer()) { want[x as usize] = true; } }
                    live.extend(fresh.iter().chain(&unfolded).copied());
                    acted = true;
                    continue;
                }
                let f = net.fire(c, b);
                ledger.count(net, &f);
                want.resize(net.agents.len(), false);
                if f.rule < net.rules.len() {
                    for (k, &x) in f.fresh.iter().enumerate() { want[x as usize] = wanted_by_rule[f.rule][k]; }
                }
                live.extend(f.fresh.iter().chain(&f.unfolded).copied());
                // Chains compiled to direct connections (CHAIN=1, an upper bound): a wanted apply born
                // facing a B, C or I (CHAIN=2: K too) fires at once, in this round and for free.
                if let Ok(level) = std::env::var("CHAIN") {
                    let mut todo: Vec<u32> = f.fresh.clone();
                    while let Some(x) = todo.pop() {
                        let Some(ax) = net.agents[x as usize].clone() else { continue };
                        if ax.tag != Tag::A || !want[x as usize] { continue; }
                        let Some((y, 0)) = ax.ports[0] else { continue };
                        let yt = net.tag(y).unwrap();
                        let routing = matches!(yt, Tag::B | Tag::C | Tag::I) || (level == "2" && yt == Tag::K);
                        if !routing { continue; }
                        let g = net.fire(x, y);
                        ledger.free += 1;
                        want.resize(net.agents.len(), false);
                        for (k, &z) in g.fresh.iter().enumerate() { want[z as usize] = wanted_by_rule[g.rule][k]; }
                        live.extend(g.fresh.iter().copied());
                        todo.extend(g.fresh.iter().copied());
                    }
                }
            } else if a.tag == Tag::Eps && collect(net, c, b, q, &mut want, &mut live, ledger) {
            } else { continue; }
            acted = true;
        }
        live.retain(|&i| net.agents[i as usize].is_some());
        if net.check { check(net).unwrap_or_else(|e| panic!("round {round}: {e}")); }
        if !acted { ledger.rounds = round; return false; }
    }
    false
}

/// The abstract net: fire every active pair until none is left or `budget` rewrites.
pub fn all(net: &mut Net, budget: u64, ledger: &mut Ledger) -> bool {
    let mut todo: Vec<u32> = (0..net.agents.len() as u32).rev().collect();
    while let Some(id) = todo.pop() {
        let Some(a) = &net.agents[id as usize] else { continue };
        let Some((b, 0)) = a.ports[0] else { continue };
        let bt = net.tag(b).unwrap();
        let (c, p) = if a.tag.is_consumer() && bt.is_producer() { (id, b) } else if a.tag.is_producer() && bt.is_consumer() { (b, id) } else { continue };
        if ledger.rewrites >= budget { return false; }
        if net.get(c).tag == Tag::T1 && net.get(p).tag == Tag::S { ledger.s_shapes[peek_s_shape(net, c, p)] += 1; }
        let near: Vec<u32> = [c, p].iter().flat_map(|&x| net.get(x).ports).flatten().map(|r| r.0).filter(|&x| x != c && x != p).collect();
        net.clock += 1;
        if let Some((fresh, unfolded)) = native_step(net, c, p, ledger) {
            if net.check { check(net).unwrap_or_else(|e| panic!("after a native rule: {e}")); }
            todo.extend(near);
            todo.extend(fresh.iter().chain(&unfolded));
            continue;
        }
        let f = net.fire(c, p);
        ledger.count(net, &f);
        todo.extend(near);
        todo.extend(f.fresh.iter().chain(&f.unfolded));
    }
    true
}
