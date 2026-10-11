//! Fixed units on the abstract net: a function loaded as a `Ref`, served by one `Unit`, gives the
//! oracle's answers, however often it is called and copied.
use rust_ca_lattice::net::{Net, Tree};
use rust_ca_lattice::oracle::{ap, nf, show, Fuel, Term, Term::*};
use rust_ca_lattice::rules::Tag;
use std::rc::Rc;

/// A tree written L, S(x) or F(x,y).
fn t(s: &str) -> Term {
    fn go(b: &[u8], i: &mut usize) -> Term {
        let c = b[*i];
        *i += 1;
        if c == b'L' { return L; }
        *i += 1;
        let a = go(b, i);
        let r = if c == b'S' { S(Rc::new(a)) } else { *i += 1; F(Rc::new(a), Rc::new(go(b, i))) };
        *i += 1;
        r
    }
    go(s.as_bytes(), &mut 0)
}

/// Run a net with `units` (code, how many unit agents) to the end, docking requests as they come.
fn run(term: &Term, units: &[Term]) -> (Term, u64, usize) {
    let mut net = Net::new();
    net.units = units.iter().map(Tree::of).collect();
    for u in 0..units.len() { net.mk_labelled(Tag::Unit, u as u8); }
    let root = net.build_units(term);
    let refs = net.agents.iter().flatten().filter(|a| a.tag == Tag::Ref).count();
    let (_, out) = net.drive(root);
    loop {
        assert!(net.reduce(1 << 24), "out of budget");
        if net.dock() == 0 { break; }
    }
    (net.readback(net.get(out).ports[0]).expect("a normal form"), net.ints, refs)
}

#[test]
fn units_answer_as_the_oracle_does() {
    // second x y = y; pick x y = triage-heavy, both forks.
    let second = t("F(L,F(F(L,L),L))");
    let swap_k = t("F(S(F(L,F(L,L))),F(L,L))");
    for f in [&second, &swap_k] {
        assert!(matches!(f, F(..)), "a unit's code is a fork");
        let args = [t("L"), t("S(L)"), t("F(L,S(L))"), t("F(F(L,L),S(S(L)))")];
        for a in &args {
            for b in &args {
                let call = ap(ap(f.clone(), a.clone()), b.clone());
                // Twice, and inside a fork, so the reference is loaded more than once.
                let term = F(Rc::new(call.clone()), Rc::new(ap(ap(f.clone(), b.clone()), a.clone())));
                let want = nf(term.clone(), &mut Fuel(1 << 24)).unwrap();
                let (got, _, refs) = run(&term, &[f.clone()]);
                assert_eq!(show(&got), show(&want), "{} {} {}", show(f), show(a), show(b));
                assert_eq!(refs, 2);
            }
        }
    }
}

#[test]
fn copied_references_reach_one_unit() {
    // s = △(△a)b c = a c (b c): with a = b = K f, both halves apply f, from copies of one Ref.
    let f = t("F(L,F(F(L,L),L))");
    let kf = F(Rc::new(L), Rc::new(f.clone()));
    let s = F(Rc::new(S(Rc::new(kf.clone()))), Rc::new(kf));
    let term = ap(ap(ap(s, t("S(L)")), t("L")), t("F(L,L)"));
    let want = nf(term.clone(), &mut Fuel(1 << 24)).unwrap();
    let (got, _, refs) = run(&term, &[f]);
    assert_eq!(show(&got), show(&want));
    assert_eq!(refs, 2);
}

#[test]
fn unit_rules_validate() {
    rust_ca_lattice::rules::validate_units().unwrap();
    rust_ca_lattice::rules::validate().unwrap();
}
