//! Abstract differential: the table-driven rule ROM against the independent oracle.
//! The gate requires zero mismatches, zero missing normal forms, and coverage of every ROM
//! interaction across the corpus.

use rust_ca_lattice::net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ca_lattice::rules::{find, Tag};
use std::collections::BTreeMap;

#[test]
fn rom_validates() {
    rust_ca_lattice::rules::validate().unwrap();
}

#[test]
fn pins() {
    // K false true → false
    check(ap(ap(oracle::k(), s(Term::L)), Term::L));
    // (F w x) applied: dispatches through A→T1→Sel
    check(oracle::disp_t());
    // chain of K redexes
    check(oracle::chain_k(6));
    // sharing: triage on a stem duplicates the argument through Dn
    check(ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)));
    // Ordered S-rule: distinguish (s c)(b c) from the reversed application.
    check(ap(f2(s(Term::L), s(Term::L)), Term::L));
}

fn check(t: Term) {
    let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(200_000)).expect("pin diverged"));
    let (got, done, _) = net::normalize(&t, 500_000);
    assert!(done, "net did not quiesce on {}", oracle::show(&t));
    assert_eq!(oracle::show(&got.expect("readback")), want, "on {}", oracle::show(&t));
}

#[test]
fn differential_4000() {
    let mut rng = Lcg(12345);
    let (mut pass, mut skip) = (0u32, 0u32);
    let mut coverage: BTreeMap<(Tag, Tag), u64> = BTreeMap::new();

    for i in 0..4000 {
        let term = rng.rand_term(3 + (i % 4));
        let want = match oracle::nf(term.clone(), &mut Fuel(5_000)) {
            Ok(w) => oracle::show(&w),
            Err(_) => { skip += 1; continue; } // diverges / oracle fuel: not a differential point
        };
        // Run with a coverage-instrumented loop (mirrors net::normalize).
        let mut n = net::Net::new();
        let root = n.build(&term);
        let (_nrm, out) = n.drive(root);
        let mut done = false;
        for _ in 0..500_000 {
            match n.active_pair() {
                Some((c, p)) => {
                    let key = (n.get(c).tag, n.get(p).tag);
                    *coverage.entry(key).or_insert(0) += 1;
                    n.fire(c, p);
                }
                None => { done = true; break; }
            }
        }
        assert!(done, "no quiescence on {}", oracle::show(&term));
        let got = n.readback(n.get(out).ports[0]).map(|t| oracle::show(&t));
        assert_eq!(got.as_deref(), Some(want.as_str()), "MISMATCH on {}", oracle::show(&term));
        pass += 1;
    }

    println!("abstract differential: {pass} pass, {skip} skipped (diverge/fuel)");
    let mut hist: Vec<_> = coverage.iter().collect();
    hist.sort_by(|a, b| b.1.cmp(a.1));
    for ((c, p), n) in &hist {
        println!("  {}·{}: {}", c.name(), p.name(), n);
    }
    assert!(pass >= 3900, "corpus mostly skipped? pass={pass}");
    // Full rule coverage: every ROM entry fired.
    for r in rust_ca_lattice::rules::RULES {
        let n = coverage.get(&(r.consumer, r.producer)).copied().unwrap_or(0);
        assert!(n > 0, "rule {}·{} never fired in 4000 terms", r.consumer.name(), r.producer.name());
    }
    // And nothing outside the ROM fired (find() would have panicked, but be explicit).
    for key in coverage.keys() {
        assert!(find(key.0, key.1).is_some());
    }
}
