//! The idealised lazy machine (README "Fork"): the abstract net run as the lattice runs it, with
//! demand arriving at once and every wanted rewrite firing at once. What the lattice could reach at
//! best, by rounds (the computation's depth) and rewrites.

use crate::polarity_is_source as is_source;
use tree_net::net::{Net, Ref};
use tree_net::rules::Tag;

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
