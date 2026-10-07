//! Superpositions on the strand lattice (rules.rs `SUP_RULES`): random superposed terms run in the
//! current design, some with every invariant re-checked after every move, each answer collapsed
//! and checked universe by universe against the oracle; the root, read back as a superposed term,
//! means the answer all along; a run saved and put back with its labels repeats exactly; and the
//! GPU refuses them.

use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ca_lattice::rules::{N_RULES, RULES, SUP_RULES};
use rust_ca_lattice::sup::{self, STerm};
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::{readback, share, tables};

fn load(t: &STerm, p: Params) -> Option<Lattice> {
    let (net, out) = share::net_sup(t, 0);
    Lattice::load(p, net, out).ok()
}

/// Random terms with superpositions in random places, labels repeating, and the oracle's answer
/// in each of their universes.
fn corpus(n: usize) -> Vec<(STerm, Vec<(u32, Term)>)> {
    let mut rng = Lcg(20261007);
    (0..4 * n as u32).filter_map(|i| {
        let t = rng.rand_sup_term(3 + i % 4, 1 + (i % 3) as u8, 0.2);
        if t.labels().is_empty() { return None; }
        sup::oracle_answers(&t, 5_000).map(|w| (t, w))
    }).take(n).collect()
}

#[test]
fn superposed_terms_run_on_the_lattice() {
    let (mut checked, mut universes) = (0, 0);
    let mut fired = vec![0u64; N_RULES];
    for (i, (t, want)) in corpus(120).iter().enumerate() {
        // Every eighth on a small lattice, re-checking every invariant after every move.
        let small = i % 8 == 0;
        let p = if small { Params { w: 16, h: 16, depth: 4, ..latest() } } else { Params { w: 48, h: 48, depth: 6, ..latest() } };
        let Some(mut l) = load(t, Params { seed: i as u64 + 1, ..p }) else { assert!(small, "term {i} does not fit"); continue };
        if small { l.check_every = 1; checked += 1; }
        l.trace_start();
        assert!(l.run(50_000_000), "term {i} did not finish: {}", sup::show(t));
        for f in &l.trace.fires { fired[f.1] += 1; }
        let got = l.read_answer().expect("an answer");
        sup::check(t, want, &got).unwrap_or_else(|e| panic!("term {i} {}: {e}", sup::show(t)));
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        universes += want.len();
    }
    println!("{universes} universes, {checked} runs checked after every move");
    assert!(checked >= 10, "only {checked} terms fit the small lattice");
    for (i, r) in SUP_RULES.iter().enumerate() {
        println!("  {}·{} {:?}: {}", r.consumer.name(), r.producer.name(), r.when, fired[RULES.len() + i]);
        assert!(fired[RULES.len() + i] > 0, "{}·{} {:?} never fired on the lattice", r.consumer.name(), r.producer.name(), r.when);
    }
}

#[test]
fn the_root_reads_back_as_the_answer_all_along() {
    let (mut looks, mut read) = (0, 0);
    for (i, (t, want)) in corpus(30).iter().enumerate() {
        let Some(mut l) = load(t, Params { w: 40, h: 40, depth: 8, seed: i as u64 + 1, ..latest() }) else { continue };
        let labels = t.labels();
        let mut c = 0;
        while !l.answered() && c < 50_000 {
            if c % 12 == 0 {
                let (s, k) = l.find_out().unwrap();
                let r = readback::Reader::new(&l.shadow).meaning(l.sids[s as usize * l.ks + k]);
                looks += 1;
                if let Some(now) = readback::sup_term(&r) {
                    read += 1;
                    for ((u, now), (_, w)) in now.universes(&labels).into_iter().zip(want) {
                        let got = oracle::nf(now, &mut Fuel(1_000_000)).ok();
                        assert_eq!(got.as_ref(), Some(w), "term {i}, clock {c}, universe {u}: {}", readback::text(&r, 400).0);
                    }
                }
            }
            l.chip_clock();
            c += 1;
        }
    }
    assert!(read * 10 >= looks * 9, "only {read} of {looks} looks read the whole term");
}

#[test]
fn a_saved_state_keeps_its_labels() {
    let mut tried = 0;
    for (i, (t, want)) in corpus(60).iter().enumerate() {
        let Some(mut l) = load(t, Params { w: 40, h: 40, depth: 6, ..latest() }) else { continue };
        for _ in 0..20 { l.chip_clock(); }
        if l.answered() || l.labelled() == 0 { continue; }
        let (held, labels, stats) = (l.held_sites(), l.held_labels(), l.stats.clone());
        let seen: Vec<_> = (0..150).map(|_| { l.chip_clock(); (l.state_words(), l.held_labels()) }).collect();
        l.put_sites_labelled(&held, &labels);
        l.stats = stats;
        l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
        for (c, (w, lab)) in seen.iter().enumerate() {
            l.chip_clock();
            assert!(l.state_words() == *w && l.held_labels() == *lab, "term {i}: clock {} differs after the state was put back", 21 + c);
        }
        assert!(l.run(50_000_000), "term {i} did not finish");
        sup::check(t, want, &l.read_answer().unwrap()).unwrap_or_else(|e| panic!("term {i}: {e}"));
        tried += 1;
        if tried == 4 { return; }
    }
    panic!("only {tried} terms ran long enough to save");
}

#[test]
fn the_gpu_refuses_superpositions() {
    let p = Params { w: 32, h: 32, depth: 4, ..latest() };
    let plain = load(&term::parse_sup("@(@(S(L),S(L)),L)").unwrap(), p).unwrap();
    assert_eq!(tables::gpu_refuses(&plain), None);
    let superposed = load(&term::parse_sup("@(&1{L,S(L)},L)").unwrap(), p).unwrap();
    assert_eq!(tables::gpu_refuses(&superposed), Some("it runs no superpositions"));
}
