//! Superpositions on the abstract net against the oracle, universe by universe: random terms
//! with superpositions in random places (labels repeat, and a repeated label must pick the same
//! side everywhere), normalized by the ROM and collapsed, against the oracle run on every choice.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Lcg, Term};
use rust_ca_lattice::rules::{all_rules, find_index_labelled, SUP_RULES, RULES};
use rust_ca_lattice::sup::{self, STerm};
use std::rc::Rc;

/// The pins' notation: `L`, `S(x)`, `F(x,y)`, `@(f,x)` and `&ℓ{a,b}` with a one-digit label.
fn parse(src: &str) -> STerm {
    fn go(b: &[u8], i: &mut usize) -> STerm {
        let c = b[*i];
        let label = if c == b'&' { *i += 1; b[*i] - b'0' } else { 0 };
        *i += 2; // the head and its bracket
        let mut kids = vec![];
        if c != b'L' {
            loop {
                kids.push(Rc::new(go(b, i)));
                *i += 1; // ',' or the closing bracket
                if matches!(b[*i - 1], b')' | b'}') { break; }
            }
        } else { *i -= 1; }
        match (c, &kids[..]) {
            (b'L', []) => STerm::L,
            (b'S', [x]) => STerm::S(x.clone()),
            (b'F', [x, y]) => STerm::F(x.clone(), y.clone()),
            (b'@', [x, y]) => STerm::Ap(x.clone(), y.clone()),
            (b'&', [x, y]) => STerm::Sup(label, x.clone(), y.clone()),
            _ => panic!("bad pin {}", String::from_utf8_lossy(b)),
        }
    }
    let b: Vec<u8> = src.bytes().filter(|c| !c.is_ascii_whitespace()).collect();
    let mut i = 0;
    let t = go(&b, &mut i);
    assert_eq!(i, b.len(), "trailing pin text");
    t
}

/// Normalize on the abstract net and compare every universe with the oracle.
fn check(t: &STerm) -> STerm {
    let want = sup::oracle_answers(t, 200_000).expect("pin diverged");
    let (got, done, _) = sup::normalize(t, 500_000);
    assert!(done, "net did not quiesce on {}", sup::show(t));
    let got = got.expect("readback");
    sup::check(t, &want, &got).unwrap_or_else(|e| panic!("{}: {e}", sup::show(t)));
    got
}

#[test]
fn pins() {
    // Applying a superposition: &1{L,S(L)} L = &1{S(L), F(L,L)}.
    assert_eq!(sup::show(&check(&parse("@(&1{L,S(L)},L)"))), "&1{S(L),F(L,L)}");
    // The same label twice picks the same side: two answers, not four.
    let same = check(&parse("@(&1{L,S(L)},&1{L,S(L)})"));
    assert_eq!(same.answers().len(), 2, "{}", sup::show(&same));
    // Different labels are independent: four.
    assert_eq!(check(&parse("@(&1{L,S(L)},&2{L,S(L)})")).answers().len(), 4);
    // K throws a superposition away.
    assert_eq!(sup::show(&check(&parse("@(@(S(L),L),&1{L,S(L)})"))), "L");
    // Triage and dispatch on a superposition copy their arms.
    check(&parse("@(F(&1{L,S(L)},S(L)),F(L,L))"));
    check(&parse("@(F(F(L,S(L)),&2{L,S(L)}),&1{L,S(L)})"));
    // A superposed function shares an argument that a duplicator of another label copies.
    check(&parse("@(F(S(L),&1{L,S(L)}),&2{L,F(L,L)})"));
}

#[test]
fn differential_with_superpositions() {
    let mut rng = Lcg(20261007);
    let (mut pass, mut skip, mut unfinished, mut universes) = (0u32, 0u32, 0u32, 0usize);
    let mut fired = vec![0u64; RULES.len() + SUP_RULES.len()];
    for i in 0..4000u32 {
        let t = rng.rand_sup_term(3 + i % 4, 1 + (i % 3) as u8, 0.2);
        if t.labels().is_empty() { skip += 1; continue; }
        let Some(want) = sup::oracle_answers(&t, 5_000) else { skip += 1; continue };
        let mut n = Net::new();
        let root = sup::build(&mut n, &t);
        let (_nrm, out) = n.drive(root);
        let mut done = false;
        for _ in 0..500_000 {
            let Some((c, p)) = n.active_pair() else { done = true; break };
            let (ca, pa) = (n.get(c), n.get(p));
            fired[find_index_labelled(ca.tag, ca.label, pa.tag, pa.label).expect("a rule")] += 1;
            n.fire(c, p);
        }
        // A triage on a superposition copies its arms, and copying forces what it copies: an arm
        // the oracle never runs may diverge here.
        if !done { unfinished += 1; continue; }
        let got = sup::read(&n, n.get(out).ports[0]).unwrap_or_else(|| panic!("no answer on {}", sup::show(&t)));
        sup::check(&t, &want, &got).unwrap_or_else(|e| panic!("MISMATCH on {}: {e}", sup::show(&t)));
        pass += 1;
        universes += want.len();
    }
    println!("superposed differential: {pass} pass ({universes} universes), {skip} skipped, {unfinished} unfinished");
    for (i, r) in all_rules().enumerate() { println!("  {}·{} {:?}: {}", r.consumer.name(), r.producer.name(), r.when, fired[i]); }
    assert!(pass >= 2000, "corpus mostly skipped? pass={pass}");
    assert!(unfinished * 100 <= pass, "{unfinished} unfinished of {pass}");
    for (i, r) in SUP_RULES.iter().enumerate() {
        assert!(fired[RULES.len() + i] > 0, "rule {}·{} {:?} never fired", r.consumer.name(), r.producer.name(), r.when);
    }
}

#[test]
fn plain_terms_unchanged() {
    // A term without superpositions builds and reads back as before.
    let mut rng = Lcg(12345);
    for i in 0..300 {
        let t: Term = rng.rand_term(3 + (i % 4));
        let Ok(w) = oracle::nf(t.clone(), &mut oracle::Fuel(5_000)) else { continue };
        let (got, done, ints) = sup::normalize(&STerm::from(&t), 500_000);
        let (plain, _, plain_ints) = rust_ca_lattice::net::normalize(&t, 500_000);
        assert!(done);
        assert_eq!(got.and_then(|g| g.plain()), Some(w.clone()));
        assert_eq!((plain, ints), (Some(w), plain_ints));
    }
}
