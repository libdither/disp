//! A state taken off one lattice and put into another, as the browser player does when it hands a
//! run between its CPU and its GPU, runs on exactly as if it had never left, demand field and all.

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
fn resume_after_a_handover_with_the_field() { handover(latest()) }

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
