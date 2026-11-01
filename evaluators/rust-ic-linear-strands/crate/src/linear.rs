//! The website's two-way programs as a linear net (tree-net lin.rs, from ../lin/rules.ts): loaded from
//! the net the TypeScript compiler builds (../lin/compile.ts, which unrolls each call the run makes
//! into a static circuit), run with two-way programs' 4 ports a slot, read back at the root sink, and
//! turned around by `flip`, agent by agent.
//!
//! A net comes as text: `n|tag tag …|a p b q,a p b q,…`, n agents numbered from 0 by their place in the
//! tag list, each link joining port p of agent a to port q of agent b.

use crate::lattice::{code, tag_of, Lattice};
use tree_net::lin;
use tree_net::net::Net;
use tree_net::rules::{Tag, ALL_TAGS};

fn tag_named(name: &str) -> Option<Tag> {
    lin::LIN_TAGS.iter().chain(&ALL_TAGS).copied().find(|t| t.name() == name)
}

/// The net, and the root sink's id.
pub fn parse(src: &str) -> Result<(Net, u32), String> {
    let mut parts = src.trim().split('|');
    let n: usize = parts.next().and_then(|s| s.trim().parse().ok()).ok_or("lin: no agent count")?;
    let tags: Vec<Tag> = parts.next().ok_or("lin: no tags")?.split_whitespace()
        .map(|t| tag_named(t).ok_or_else(|| format!("lin: no agent {t}")))
        .collect::<Result<_, _>>()?;
    if tags.len() != n { return Err(format!("lin: {} tags for {n} agents", tags.len())); }
    let mut net = Net::new();
    for &t in &tags { net.mk(t); }
    for l in parts.next().ok_or("lin: no links")?.split(',').filter(|l| !l.trim().is_empty()) {
        let v: Vec<u32> = l.split_whitespace().map(|x| x.parse().map_err(|_| format!("lin: bad link {l}"))).collect::<Result<_, _>>()?;
        let [a, p, b, q] = v[..] else { return Err(format!("lin: bad link {l}")) };
        if a as usize >= n || b as usize >= n { return Err(format!("lin: link {l} to no agent")); }
        net.link(a, p as u8, b, q as u8);
    }
    let out = tags.iter().position(|&t| t == Tag::Out).ok_or("lin: no root sink")? as u32;
    Ok((net, out))
}

/// The root sink: agents are numbered afresh when a saved state is put back, so it is found, not kept.
pub fn sink(net: &Net) -> Option<u32> { net.agents.iter().position(|a| a.as_ref().is_some_and(|a| a.tag == Tag::Out)).map(|i| i as u32) }

/// The value a finished run holds as △ text, and whether it went forward; None while anything still
/// carries a run but the root at the sink.
pub fn answer(net: &Net, out: u32) -> Option<(String, bool)> {
    let (r, 0) = net.get(out).ports[0]? else { return None };
    let root = net.get(r);
    if !lin::producer(root.tag) { return None; }
    let busy = net.agents.iter().enumerate().any(|(i, a)| i as u32 != r && a.as_ref().is_some_and(|a| lin::runs(a.tag)));
    if busy { return None; }
    fn show(net: &Net, id: u32, from: usize, shape: char) -> String {
        let kid = |i: usize| {
            let (c, _) = net.get(id).ports[from + i].expect("a kid");
            let t = net.get(c).tag;
            show(net, c, 1, t.name().chars().next().unwrap())
        };
        match shape { 'L' => "△".into(), 'S' => format!("△{}", kid(0)), _ => format!("({}, {})", kid(0), kid(1)) }
    }
    let name = root.tag.name();
    Some((show(net, r, 2, name.chars().nth(1).unwrap()), name.ends_with('f')))
}

/// Why a run stopped short: the pairs that face each other with no rule.
pub fn stuck(net: &Net) -> Vec<(Tag, Tag)> {
    net.all_active_pairs().into_iter().map(|(c, p)| (net.get(c).tag, net.get(p).tag)).filter(|&(c, _)| c != Tag::Out).collect()
}

impl Lattice {
    /// Turn every run around (lin.rs `flip`): each agent that carries one becomes its partner, its
    /// ports permuted, on the lattice and in the abstract net alike. A purely local relabelling.
    pub fn flip_lin(&mut self) {
        // a finished run has cooled; turned around it runs again, at the temperature it ran at
        if let Some((_, t)) = self.cooling.take() { self.set_temp(t); }
        let k_real = self.p.k;
        for s in 0..self.sites() {
            for k in 0..k_real {
                let c = self.tags[s as usize * self.ks + k];
                if c == 0 { continue; }
                let t = tag_of(c);
                let Some((to, perm)) = lin::flip(t) else { continue };
                let n = t.arity();
                let base = s as usize * self.ends;
                let end = |p: usize| (k * self.ar + p) as u8;
                let old: Vec<u8> = (0..n).map(|p| self.mate[base + end(p) as usize]).collect();
                let mut inv = [0usize; 4];
                for (i, &p) in perm[..n].iter().enumerate() { inv[p as usize] = i; }
                let own = |m: u8| (m as usize) >= k * self.ar && (m as usize) < k * self.ar + n;
                for i in 0..n {
                    let m = old[perm[i] as usize];
                    let m = if own(m) { end(inv[m as usize - k * self.ar]) } else { m };
                    self.mate[base + end(i) as usize] = m;
                    if !own(old[perm[i] as usize]) && m != crate::lattice::NONE { self.mate[base + m as usize] = end(i); }
                }
                self.tags[s as usize * self.ks + k] = code(to);
                let sid = self.sids[s as usize * self.ks + k];
                flip_agent(&mut self.shadow, sid, to, &perm[..n]);
            }
        }
        self.lin_rewant();
    }

    /// Who faces whom has changed (a flip, or the sites put back as a saved state had them): the consumer
    /// of each meeting is wanted, the rest of the circuit idle.
    pub fn lin_rewant(&mut self) {
        for s in 0..self.sites() { for k in 0..self.p.k { if self.tags[s as usize * self.ks + k] != 0 { self.lin_want(s, k); } } }
    }
}

/// The same relabelling in the abstract net.
pub fn flip_agent(net: &mut Net, id: u32, to: Tag, perm: &[u8]) {
    let a = net.agents[id as usize].as_mut().expect("live agent");
    let old = a.ports;
    a.tag = to;
    let mut inv = [0usize; 4];
    for (i, &p) in perm.iter().enumerate() { inv[p as usize] = i; }
    for (i, &p) in perm.iter().enumerate() {
        let m = old[p as usize];
        let m = match m { Some((b, q)) if b == id => Some((b, inv[q as usize] as u8)), m => m };
        net.agents[id as usize].as_mut().unwrap().ports[i] = m;
        if let Some((b, q)) = m { if b != id { net.agents[b as usize].as_mut().unwrap().ports[q as usize] = Some((id, i as u8)); } }
    }
}

/// Flip a whole abstract net (for runs off the lattice).
pub fn flip_net(net: &mut Net) {
    for id in 0..net.agents.len() as u32 {
        let Some(a) = &net.agents[id as usize] else { continue };
        if let Some((to, perm)) = lin::flip(a.tag) { let n = a.tag.arity(); flip_agent(net, id, to, &perm[..n]); }
    }
}
