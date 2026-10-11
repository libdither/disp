//! Movement agents (rules.rs `VIEW_RULES`, net.rs `build_views`) on the abstract net, against the
//! oracle: random terms, code applied to data (head and arguments worked out first, as the compiler
//! hands programs over), and code rich in the K, S, B, C, I and W shapes applied to values and to
//! applications, each with only the code in views and with every value in views (so triage and
//! dispatch read views as data, and spell them out). Every answer is the oracle's, every term the plain
//! net finishes finishes, and every rule fires.

use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Lcg, Term};
use rust_ca_lattice::rules::{validate_views, VIEW_RULES};

/// Random terms, as the differential makes them, and the same split into a head and arguments
/// each worked out first.
fn corpus(n: u32) -> Vec<Term> {
    let mut rng = Lcg(12345);
    let mut out = vec![];
    for i in 0..n {
        let t = rng.rand_term(3 + (i % 4));
        if oracle::nf(t.clone(), &mut Fuel(5_000)).is_err() { continue; }
        out.push(t.clone());
        let (mut h, mut args) = (t, vec![]);
        while let Term::Ap(f, x) = h.clone() { args.insert(0, (*x).clone()); h = (*f).clone(); }
        let Ok(h) = oracle::nf(h, &mut Fuel(5_000)) else { continue };
        let Ok(args) = args.into_iter().map(|a| oracle::nf(a, &mut Fuel(5_000))).collect::<Result<Vec<_>, _>>() else { continue };
        if !args.is_empty() { out.push(args.into_iter().fold(h, ap)); }
    }
    out
}

/// Random code rich in the views' shapes, applied to random values and applications.
fn shapes(n: u32) -> Vec<Term> {
    fn code(rng: &mut Lcg, d: u32) -> Term {
        if d == 0 { return if rng.next() < 0.5 { Term::L } else { s(Term::L) }; }
        let r = rng.next();
        if r < 0.15 { f2(Term::L, code(rng, d - 1)) }                                     // K
        else if r < 0.35 { f2(s(f2(Term::L, code(rng, d - 1))), code(rng, d - 1)) }      // B
        else if r < 0.5 { f2(s(code(rng, d - 1)), f2(Term::L, code(rng, d - 1))) }       // C
        else if r < 0.6 { f2(s(code(rng, d - 1)), code(rng, d - 1)) }                    // S
        else if r < 0.65 { let x = code(rng, d - 1); f2(s(x.clone()), x) }                // W
        else if r < 0.7 { f2(f2(Term::L, Term::L), Term::L) }                             // I
        else if r < 0.8 { f2(f2(code(rng, d - 1), code(rng, d - 1)), code(rng, d - 1)) } // triage
        else if r < 0.9 { s(code(rng, d - 1)) }
        else { Term::L }
    }
    let mut rng = Lcg(777);
    let mut out = vec![];
    let mut i = 0;
    while (out.len() as u32) < n && i < 1_000_000 {
        i += 1;
        let h = code(&mut rng, 2 + i % 3);
        // Some arguments still to be worked out, so readers meet suspensions too.
        let t = (0..1 + i % 3).fold(h, |f, _| {
            let x = if rng.next() < 0.25 { ap(code(&mut rng, 1), code(&mut rng, 2)) } else { code(&mut rng, 1 + i % 3) };
            ap(f, x)
        });
        if oracle::nf(t.clone(), &mut Fuel(20_000)).is_ok() { out.push(t); }
    }
    out
}

/// How a term is built: plainly, with its code in views (`build_views`), or with every value in views.
#[derive(Clone, Copy, PartialEq)]
enum Build { Plain, Code, Every }

/// Built so, reduced to quiescence: the answer, if it finished.
fn normalize(t: &Term, how: Build, fired: &mut [u64]) -> Option<String> {
    let mut net = Net::new();
    let root = match how { Build::Plain => net.build(t), Build::Code => net.build_views(t), Build::Every => net.build_every_view(t) };
    let (_, out) = net.drive(root);
    let done = net.reduce_listed(2_000_000, |r| {
        if let Some(i) = VIEW_RULES.iter().position(|x| std::ptr::eq(x, r)) { fired[i] += 1; }
    });
    if !done { return None; }
    net.readback(net.get(out).ports[0]).map(|t| oracle::show(&t))
}

#[test]
fn views_agree_with_the_oracle() {
    validate_views().unwrap();
    let mut fired = vec![0u64; VIEW_RULES.len()];
    let (mut n, mut unfinished) = (0, 0);
    for (i, t) in corpus(3000).iter().chain(&shapes(3000)).enumerate() {
        let want = oracle::show(&oracle::nf(t.clone(), &mut Fuel(50_000)).unwrap());
        let plain = normalize(t, Build::Plain, &mut vec![0; VIEW_RULES.len()]);
        for how in [Build::Code, Build::Every] {
            match normalize(t, how, &mut fired) {
                Some(got) => assert_eq!(got, want, "term {i}: {}", oracle::show(t)),
                None => { assert!(plain.is_none(), "term {i} finishes plainly but not with views: {}", oracle::show(t)); unfinished += 1; }
            }
            n += 1;
        }
    }
    println!("{n} terms, {unfinished} unfinished either way");
    assert!(unfinished * 20 < n, "{unfinished} of {n} terms unfinished");
    for (r, &k) in VIEW_RULES.iter().zip(&fired) {
        println!("  {}·{}: {k}", r.consumer.name(), r.producer.name());
        assert!(k > 0, "{}·{} never fired", r.consumer.name(), r.producer.name());
    }
}

/// Each shape loads as its view, in code only: the arguments stay trees.
#[test]
fn code_loads_as_views_and_data_as_trees() {
    let k = |x: Term| f2(Term::L, x);
    let i = f2(f2(Term::L, Term::L), Term::L);
    let tags = |t: &Term| {
        let mut net = Net::new();
        net.build_views(t);
        let mut v: Vec<&str> = net.agents.iter().flatten().map(|a| a.tag.name()).collect();
        v.sort();
        v.join(" ")
    };
    // B (K L) I applied to K L: the argument is data, and a K shape stays a plain fork.
    let b = f2(s(k(k(Term::L))), i.clone());
    assert_eq!(tags(&ap(b, k(Term::L))), "B F F I L L L L P");
    // C L (K L), W (S L), Sx L (S L), each applied to a leaf.
    assert_eq!(tags(&ap(f2(s(Term::L), k(k(Term::L))), Term::L)), "C F L L L L P");
    // A number inside code (K shapes all the way down) stays a plain tree; the code around it is a view.
    let three = k(k(k(Term::L)));
    assert_eq!(tags(&ap(f2(s(Term::L), k(three)), Term::L)), "C F F F L L L L L L P");
    assert_eq!(tags(&ap(f2(s(s(Term::L)), s(Term::L)), Term::L)), "L L P S W");
    assert_eq!(tags(&ap(f2(s(Term::L), s(Term::L)), Term::L)), "L L L P S Sx");
    // A value applied to nothing is data.
    assert_eq!(tags(&i), "F F L L L");
}
