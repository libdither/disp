//! A state taken off one lattice and put into another, as the browser player does when it hands a
//! run between its CPU and its GPU, runs on exactly as if it had never left, in the current design too.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ic_strands::lattice::{chip, latest, Lattice, Params};

fn load(t: &Term, p: Params) -> Option<Lattice> {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    Lattice::load(p, net, out).ok()
}

#[test]
fn resume_after_a_handover() { handover(chip()) }

#[test]
fn resume_after_a_handover_in_the_current_design() { handover(latest()) }

fn handover(p: Params) {
    let p = Params { w: 56, h: 56, depth: 6, ..p };
    let mut rng = Lcg(20260730);
    let mut tried = 0;
    for i in 0..80 {
        let t = rng.rand_term(6 + i % 3);
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(5_000)) else { continue };
        let Some(mut a) = load(&t, p) else { continue };
        for _ in 0..40 { a.chip_clock(); }
        if a.readback().is_some() { continue; }
        tried += 1;
        // Handed over sparsely (the sites holding anything or a field) and whole (no field).
        let mut b = load(&t, p).unwrap();
        b.put_sites(&a.held_sites());
        let mut c = load(&t, p).unwrap();
        c.set_state_words(&a.state_words());
        c.adopt();
        let whole = !a.fields.on;
        for l in [&mut b, &mut c] {
            l.stats.clocks = a.stats.clocks;
            assert_eq!(l.stats.strands, a.stats.strands, "term {i}: strands counted afresh");
            l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        }
        let mut clocks = 40;
        while a.readback().is_none() && clocks < 20_000 {
            for l in [&mut a, &mut b, &mut c] { l.chip_clock(); }
            clocks += 1;
            if clocks % 50 == 0 || a.readback().is_some() {
                let s = a.state_words();
                assert!(b.state_words() == s && (!whole || c.state_words() == s), "term {i}: clock {clocks} differs after the handover");
                assert!(b.fields.values() == a.fields.values(), "term {i}: clock {clocks}: the field differs after the handover");
            }
        }
        assert_eq!(b.readback().map(|t| oracle::show(&t)), Some(oracle::show(&want)), "term {i}");
        assert_eq!(b.stats.strands, a.stats.strands, "term {i}");
        if whole { assert_eq!(c.stats.strands, a.stats.strands, "term {i}"); }
        b.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        if tried == 4 { return; }
    }
    panic!("only {tried} terms ran long enough to hand over");
}

/// A run saved at some clock and put back later (the player's rewinding) runs on exactly as it
/// did, cooling included, given that cooling starts again at the clock it started at (the player
/// records it).
#[test]
fn rewind_repeats_the_run() {
    let p = Params { w: 56, h: 56, depth: 6, ..latest() };
    let mut rng = Lcg(20261006);
    let mut tried = 0;
    for i in 0..80 {
        let t = rng.rand_term(6 + i % 3);
        let Some(mut l) = load(&t, p) else { continue };
        for _ in 0..30 { l.chip_clock(); }
        if l.readback().is_some() { continue; }
        tried += 1;
        // Cooling from clock 50 on, so a saved state from before and one from during it both count.
        let mut saved = vec![];
        let mut seen = vec![];
        for c in 30..160 {
            if c == 50 { l.cool(); }
            if c % 40 == 30 { saved.push((l.held_sites(), l.stats.clone(), l.p.temp, l.cooling)); }
            l.chip_clock();
            seen.push((l.state_words(), l.fields.values().to_vec(), l.stats.fires, l.p.temp));
        }
        for (j, (held, stats, temp, cooling)) in saved.into_iter().enumerate() {
            l.put_sites(&held);
            l.stats = stats;
            l.cooling = cooling;
            l.set_temp(temp);
            for c in 30 + 40 * j..160 {
                if c == 50 { l.cool(); }
                l.chip_clock();
                let (w, f, fires, temp) = &seen[c - 30];
                assert!(l.state_words() == *w && l.fields.values() == &f[..], "term {i}: rewound to {} and ran to {}: the sites differ", 30 + 40 * j, c + 1);
                assert_eq!((l.stats.fires, l.p.temp), (*fires, *temp), "term {i}: clock {}", c + 1);
            }
        }
        if tried == 3 { return; }
    }
    panic!("only {tried} terms ran long enough to rewind");
}
