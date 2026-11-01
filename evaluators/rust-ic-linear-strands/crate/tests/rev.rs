//! Reversible rewriting (src/rev.rs): run forward to the answer, then undo every event, in random order
//! among those that can be undone; the net must come back exactly as it was loaded. On the way, part of
//! the run is undone and run again.
use tree_net::net::Net;
use tree_net::oracle::{self, Fuel, Lcg, Term};
use tree_net::rules::Tag;
use rust_ic_strands::rev::Rev;

fn terms() -> Vec<(String, Term)> {
    let mut rng = Lcg(91);
    let mut v: Vec<(String, Term)> = (0..80).map(|i| (format!("corpus {i}"), rng.rand_term(4 + (i % 3) as u32))).collect();
    for (name, n) in [("share-tower", 3), ("s-rule", 0), ("disp-t", 0), ("convoy", 3), ("fib", 0), ("sort", 0)] {
        if let Some(t) = tree_net::term::workload(name, n) { v.push((name.into(), t)); }
    }
    v
}

fn same(a: &Net, b: &Net) -> Result<(), String> {
    for i in 0..a.agents.len().max(b.agents.len()) {
        let (x, y) = (a.agents.get(i).and_then(|x| x.as_ref()), b.agents.get(i).and_then(|y| y.as_ref()));
        match (x, y) {
            (None, None) => {}
            (Some(x), Some(y)) if x.tag == y.tag && x.label == y.label && x.ports == y.ports => {}
            _ => return Err(format!("agent {i}: {:?} vs {:?}", x.map(|x| (x.tag, x.ports)), y.map(|y| (y.tag, y.ports)))),
        }
    }
    Ok(())
}

#[test]
fn forward_then_backward_restores_the_net() {
    let mut rng = Lcg(5);
    let (mut runs, mut events, mut residue) = (0, 0, 0);
    for (name, t) in terms() {
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(2_000_000)) else { continue };
        let (net, _) = rust_ic_strands::load::net(&t);
        let start = Net { agents: net.agents.clone(), ..Net::new() };
        let mut r = Rev::new(net, true);
        r.erase = runs % 2 == 0;
        // Part of the way, back some of it in any order, then on to the end.
        let part = r.run(1 + (rng.next() * 200.0) as usize);
        for _ in 0..(rng.next() * part as f64) as usize { r.undo_any(|n| ((rng.next() * n as f64) as usize).min(n - 1)); }
        r.run(400_000);
        assert_eq!(r.answer(), Some(want), "{name}: wrong answer after {} events", r.live_events());
        events += r.live_events();
        residue += r.net.agents.iter().flatten().filter(|a| a.tag == Tag::Eps).count();
        // Scrubbing: back some of the way, latest first or in any order, then done again exactly.
        let end = Net { agents: r.net.agents.clone(), ..Net::new() };
        let k = (rng.next() * r.live_events() as f64) as usize;
        for _ in 0..k { if runs % 3 == 0 { r.undo_any(|n| ((rng.next() * n as f64) as usize).min(n - 1)); } else { r.undo_latest(); } }
        let t = r.events.len();
        r.seek(t / 3);
        r.seek(t);
        same(&r.net, &end).unwrap_or_else(|e| panic!("{name}: not the same after undoing {k} and redoing: {e}"));
        // Undo, picking at random among the events that can be undone now.
        while r.undo_any(|n| ((rng.next() * n as f64) as usize).min(n - 1)).is_some() {}
        assert_eq!(r.live_events(), 0, "{name}: {} events could not be undone", r.live_events());
        same(&r.net, &start).unwrap_or_else(|e| panic!("{name}: not the loaded net: {e}"));
        runs += 1;
    }
    eprintln!("{runs} terms, {events} events forward and back, {residue} erasers left (half the runs keep erased values)");
    assert!(runs >= 60);
}

/// Run by rounds (every rewrite there is at once, what a round makes waiting for the next): the same
/// answer as one at a time, no event's inputs made in its own round, and back round by round to the
/// net as loaded.
#[test]
fn rounds_fire_every_pair_at_once() {
    let (mut runs, mut events, mut rounds) = (0, 0, 0);
    for (name, t) in terms() {
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(2_000_000)) else { continue };
        let (net, _) = rust_ic_strands::load::net(&t);
        let start = Net { agents: net.agents.clone(), ..Net::new() };
        let mut r = Rev::new(net, true);
        while r.round() > 0 { assert!(r.rounds.len() < 100_000, "{name}: no end"); }
        assert_eq!(r.answer(), Some(want.clone()), "{name}: wrong answer by rounds");
        let bounds: Vec<usize> = r.rounds.iter().copied().chain([r.events.len()]).collect();
        for w in bounds.windows(2) {
            for e in w[0]..w[1] {
                let ev = r.events[e].as_ref().unwrap();
                for (was, _) in &ev.gone {
                    assert!(r.maker[was.id as usize].is_none_or(|m| (m as usize) < w[0]), "{name}: event {e} used what its own round made");
                }
            }
        }
        events += r.live_events();
        rounds += r.rounds.len();
        for &b in r.rounds.clone().iter().rev() { r.seek(b); }
        same(&r.net, &start).unwrap_or_else(|e| panic!("{name}: not the loaded net after going back by rounds: {e}"));
        r.seek(bounds[bounds.len() / 2]);
        r.seek(*bounds.last().unwrap());
        assert_eq!(r.answer(), Some(want), "{name}: not the answer after scrubbing back and forth");
        runs += 1;
    }
    eprintln!("{runs} terms by rounds: {events} events in {rounds} rounds");
}
