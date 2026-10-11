//! Fixed units on the lattice (rules.rs `UNIT_RULES`): calls walk to one unit and come back as
//! words, the abstract net in step after every clock, every answer the oracle's.
use rust_ca_lattice::oracle::{self, ap, Fuel, Term, Term::*};
use rust_ca_lattice::sup::STerm;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use std::rc::Rc;

fn t(s: &str) -> Term { rust_ic_mesh::term::parse(s).unwrap() }
fn fork(a: Term, b: Term) -> Term { F(Rc::new(a), Rc::new(b)) }

/// Run `term` with `unit` served by a fixed unit d sites away, checking every clock; the clocks taken
/// and the calls the unit answered.
fn run(term: &Term, unit: &Term, d: u32) -> (f64, u64) {
    let p = Params { w: 48, h: 48, depth: 4, unit_dist: d, unit_lat: 5, ..latest() };
    let (net, out) = rust_ic_strands::share::net_with_units(&STerm::from(term), &p, std::slice::from_ref(unit)).unwrap();
    let mut l = Lattice::load(p, net, out).unwrap();
    while !l.answered() {
        assert!(l.stats.clocks < 40_000.0, "no answer");
        l.chip_clock();
        l.check_invariants().unwrap();
        l.check_projection().unwrap();
    }
    let want = oracle::nf(term.clone(), &mut Fuel(1 << 24)).unwrap();
    assert_eq!(l.readback(), Some(want), "{}", oracle::show(term));
    (l.stats.clocks, l.units[0].served)
}

#[test]
fn calls_reach_one_unit_and_come_back() {
    // second x y = y; swap_k triages its first argument.
    for f in [t("F(L,F(F(L,L),L))"), t("F(S(F(L,F(L,L))),F(L,L))")] {
        let calls = [("L", "S(L)"), ("F(L,S(L))", "L"), ("S(S(L))", "F(F(L,L),S(L))")];
        let mut term = L;
        for (a, b) in calls.iter().rev() { term = fork(ap(ap(f.clone(), t(a)), t(b)), term); }
        let (_, served) = run(&term, &f, 4);
        assert_eq!(served, 3);
    }
}

#[test]
fn a_copied_reference_calls_the_same_unit() {
    // △(△(K f))(K f) c: both halves of an S rule apply f, from copies of one reference.
    let f = t("F(L,F(F(L,L),L))");
    let kf = fork(L, f.clone());
    let s = fork(S(Rc::new(kf.clone())), kf);
    let term = ap(ap(ap(s, t("S(L)")), t("L")), t("F(L,L)"));
    let (_, served) = run(&term, &f, 6);
    assert_eq!(served, 1);
}

#[test]
fn farther_units_take_longer() {
    let f = t("F(L,F(F(L,L),L))");
    let term = ap(ap(f.clone(), t("S(L)")), t("F(L,L)"));
    let (near, _) = run(&term, &f, 2);
    let (far, _) = run(&term, &f, 16);
    assert!(far > near, "{near} clocks 2 sites away, {far} at 16");
}
