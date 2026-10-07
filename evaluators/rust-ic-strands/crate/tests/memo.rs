//! Merging equal computations (memo.rs, `Params::memo`) keeps the answer, with every invariant and
//! the projection checked after every clock and every merge; and reading terms back hashes equal
//! terms alike, however duplicators share them.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ic_strands::lattice::memo::{self, Terms};
use rust_ic_strands::lattice::{latest, Lattice, Params};

fn lattice(t: &Term, p: Params) -> Option<Lattice> {
    let mut net = Net::new();
    let root = net.build(t);
    let (_, out) = net.drive(root);
    Lattice::load(p, net, out).ok()
}

#[test]
fn merging_keeps_the_answer() {
    let k = oracle::k();
    let x = ap(ap(k.clone(), s(Term::L)), Term::L);
    let y = ap(ap(f2(s(Term::L), s(Term::L)), Term::L), f2(Term::L, Term::L));
    // Twins built apart, and twins the S rule makes: (s c)(b c) with s = b.
    let mut terms = vec![f2(x.clone(), x.clone()), f2(y.clone(), f2(y.clone(), y.clone())), ap(f2(x.clone(), x), y.clone()),
                         ap(f2(s(s(Term::L)), s(Term::L)), y.clone()), ap(ap(f2(s(k.clone()), k), Term::L), y)];
    let mut rng = Lcg(20261007);
    terms.extend((0..40).map(|i| rng.rand_term(4 + i % 3)));
    let (mut merged, mut checked) = (0, 0);
    for (i, t) in terms.iter().enumerate() {
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(100_000)) else { continue };
        for (r, memo_local) in [(2, false), (16, false), (1, true)] {
            let p = Params { w: 16, h: 16, depth: 4, memo: r, memo_every: 2, memo_local, seed: i as u64 + 1, ..latest() };
            let Some(mut l) = lattice(t, p) else { continue };
            l.check_every = 1;
            assert!(l.run(50_000_000), "term {i} did not finish at radius {r}");
            assert_eq!(l.readback().map(|t| oracle::show(&t)), Some(oracle::show(&want)), "term {i} at radius {r}");
            l.check_projection().unwrap_or_else(|e| panic!("term {i}: {e}"));
            merged += l.stats.merged;
            checked += 1;
        }
    }
    assert!(checked >= 60 && merged >= 5, "{checked} runs, {merged} merges");
}

/// A term built apart and the same term with its equal parts shared through duplicators read back
/// as one term, and its parts as the parts of it.
#[test]
fn equal_terms_hash_alike() {
    let x = ap(ap(oracle::k(), s(Term::L)), f2(Term::L, Term::L));
    let t = f2(ap(x.clone(), x.clone()), f2(x.clone(), ap(x.clone(), x)));
    let mut terms = Terms::default();
    let mut ids = vec![];
    for min in [0, 1] {
        let (net, out) = rust_ic_strands::share::net(&t, min);
        let at = terms.of_net(&net);
        let (root, port) = net.get(out).ports[0].unwrap();
        ids.push(at[root as usize][port as usize]);
    }
    assert_eq!(ids[0], ids[1]);
    let tm = terms.term(ids[0], &mut Default::default()).unwrap();
    assert_eq!(*tm, t);
    assert!(memo::computes(rust_ca_lattice::rules::Tag::P));
}
