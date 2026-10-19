//! Two-way programs on the lattice (twoway.rs, rules.rs `TWO_RULES`): programs are trees; the lattice
//! gives what the reference interpreter gives and hands the program back, and the inverse program takes
//! the result back to the input.

use tree_net::oracle::Lcg;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::twoway::{self, add, call, cond, lp, mirror, nat, par, seq, twice, Prog, Val};

fn params(seed: u64) -> Params { Params { two: true, lazy: false, seed, ..latest() } }

/// Runs p on x on the lattice: the program handed back and the result (None if stuck anywhere), and
/// the clocks it took.
fn on_lattice(p: &Prog, x: &Val, seed: u64) -> (Option<(Val, Val)>, f64) {
    let (net, out) = twoway::net(p, x);
    let side = ((net.live_count() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut l = Lattice::load(Params { w: side, h: side, ..params(seed) }, net, out).expect("load");
    l.run(200_000_000);
    l.check_projection().unwrap();
    let done = twoway::leftovers(&l.shadow).is_empty();
    (twoway::read(&l.shadow, out).filter(|_| done), l.stats.clocks)
}

#[test]
fn add_on_the_reference() {
    for a in 0..6 {
        for b in 0..4 {
            let x = Val::f(nat(a), nat(b));
            let y = add().run(&x, &mut 1000).expect("add runs");
            assert_eq!(y, Val::f(nat(a), nat(a + b)), "add {a} {b}");
            assert_eq!(add().dagger().run(&y, &mut 1000), Some(x), "sub {a} {b}");
        }
    }
}

#[test]
fn add_on_the_lattice() {
    for (a, b) in [(0, 0), (1, 2), (3, 1)] {
        let x = Val::f(nat(a), nat(b));
        let (got, clocks) = on_lattice(&add(), &x, 1);
        let (p, y) = got.unwrap_or_else(|| panic!("add {a} {b}: no result"));
        assert_eq!(y, Val::f(nat(a), nat(a + b)), "add {a} {b}");
        assert_eq!(p, add().code(), "add {a} {b}: the program handed back");
        let (back, _) = on_lattice(&add().dagger(), &y, 2);
        assert_eq!(back.map(|r| r.1), Some(x), "sub {a} {b}");
        eprintln!("add {a} {b}: {clocks:.0} clocks");
    }
}

fn rand_val(rng: &mut Lcg, d: u32) -> Val {
    let x = rng.next();
    if d == 0 || x < 0.3 { Val::L } else if x < 0.5 { Val::s(rand_val(rng, d - 1)) } else { Val::f(rand_val(rng, d - 1), rand_val(rng, d - 1)) }
}

const STEPS: [Prog; 13] = [Prog::Id, Prog::Swap, Prog::Flip, Prog::AssocL, Prog::AssocR, Prog::Uniti, Prog::Unite, Prog::Copy,
                           Prog::Merge, Prog::Triage, Prog::Untriage, Prog::StemFork, Prog::ForkStem];

fn rand_prog(rng: &mut Lcg, d: u32, loops: bool) -> Prog {
    let x = rng.next();
    if d == 0 || x < 0.45 { return STEPS[(rng.next() * STEPS.len() as f64) as usize % STEPS.len()].clone(); }
    let (a, b) = (rand_prog(rng, d - 1, loops), rand_prog(rng, d - 1, loops));
    match (x * 40.0) as u32 % if loops { 4 } else { 3 } { 0 => seq(a, b), 1 => par(a, b), 2 => cond(a, b), _ => lp(a) }
}

/// Random programs of every instruction but loops and eval, on random trees: where the reference gives
/// a result, so does the lattice, with the program handed back, and the inverse takes it back; where the
/// reference is stuck, so is the lattice.
#[test]
fn random_programs() {
    let mut rng = Lcg(20261015);
    let (mut ran, mut stuck) = (0, 0);
    while ran < 120 {
        let (p, x) = (rand_prog(&mut rng, 4, false), rand_val(&mut rng, 4));
        let want = p.run(&x, &mut 1000);
        if want.is_none() && stuck >= 30 { continue; }
        let (got, _) = on_lattice(&p, &x, ran as u64 + stuck as u64);
        match want {
            None => { assert_eq!(got, None, "{} on {}: the lattice gave a result where the reference is stuck", p.show(), x.show()); stuck += 1; }
            Some(y) => {
                let (q, v) = got.unwrap_or_else(|| panic!("{} on {}: no result", p.show(), x.show()));
                assert_eq!(v, y, "{} on {}", p.show(), x.show());
                assert_eq!(q, p.code(), "{} on {}: the program handed back", p.show(), x.show());
                let (back, _) = on_lattice(&p.dagger(), &y, 7);
                assert_eq!(back.map(|r| r.1), Some(x.clone()), "{} back from {}", p.show(), y.show());
                ran += 1;
            }
        }
    }
}

/// A program's tree and its text agree: the tree reads back as the program, and the inverse made from the
/// tree, a node at a time as `Dg` makes it, is the inverse read off the text.
#[test]
fn programs_are_trees() {
    let mut rng = Lcg(7);
    for p in (0..300).map(|_| rand_prog(&mut rng, 5, true)).chain([add(), twice(), call(), mirror()]) {
        assert_eq!(Prog::of(&p.code()), Some(p.clone()), "{}", p.show());
        assert_eq!(twoway::dagger(&p.code()), Some(p.dagger().code()), "{}", p.show());
    }
    assert_eq!(Prog::Id.code(), Val::L);
    assert_eq!(seq(Prog::Swap, Prog::Id).code(), Val::f(Val::s(Val::L), Val::L));
}

/// A program made by a program, run: `twice` copies p into the program p; p and runs it, and its inverse
/// runs the copy's inverse (`uneval`). On the reference and on the lattice.
#[test]
fn programs_making_programs() {
    let succ = twoway::succ();
    let x = Val::f(succ.code(), nat(3));
    let y = Val::f(succ.code(), nat(5));
    assert_eq!(twice().run(&x, &mut 100), Some(y.clone()));
    assert_eq!(twice().dagger().run(&y, &mut 100), Some(x.clone()));
    let (got, clocks) = on_lattice(&twice(), &x, 3);
    assert_eq!(got, Some((twice().code(), y.clone())), "twice on the lattice");
    let (back, back_clocks) = on_lattice(&twice().dagger(), &y, 4);
    assert_eq!(back.map(|r| r.1), Some(x), "twice back on the lattice");
    eprintln!("twice succ 3: {clocks:.0} clocks, back {back_clocks:.0}");
}

fn mirrored(v: &Val) -> Val {
    match v { Val::L => Val::L, Val::S(a) => Val::s(mirrored(a)), Val::F(a, b) => Val::f(mirrored(b), mirrored(a)) }
}

/// Recursion that branches: `mirror` calls itself on a fork's two parts at once. Its inverse (mirror
/// again, made by `Dg` as it runs) takes the result back. Run on a program it gives a program, which runs.
#[test]
fn mirror_recurses() {
    let m = mirror().code();
    let mut rng = Lcg(11);
    for _ in 0..40 {
        let x = rand_val(&mut rng, 4);
        let y = call().run(&Val::f(m.clone(), x.clone()), &mut 10_000).expect("mirror runs");
        assert_eq!(y, Val::f(m.clone(), mirrored(&x)), "mirror {}", x.show());
        assert_eq!(call().dagger().run(&y, &mut 10_000), Some(Val::f(m.clone(), x.clone())), "mirror back {}", x.show());
    }
    // A program mirrored is a program: (a; b) becomes (b'; a').
    let p = seq(Prog::Uniti, par(Prog::Swap, Prog::Id));
    let (_, q) = call().run(&Val::f(m.clone(), p.code()), &mut 10_000).unwrap().fork_parts();
    assert_eq!(Prog::of(&q), Some(seq(par(Prog::Id, Prog::Swap), Prog::Uniti)));
    // On the lattice, on a program (forward: `~twice` runs uneval there, and `mirror (△, △△)` is in the player's checks).
    let x = seq(Prog::Swap, Prog::Id).code();
    let (got, clocks) = on_lattice(&call(), &Val::f(m.clone(), x.clone()), 5);
    assert_eq!(got, Some((call().code(), Val::f(m.clone(), seq(Prog::Id, Prog::Swap).code()))), "mirror [swap; id] on the lattice");
    eprintln!("mirror [swap; id]: {clocks:.0} clocks");
}

trait Parts { fn fork_parts(self) -> (Val, Val); }
impl Parts for Val { fn fork_parts(self) -> (Val, Val) { match self { Val::F(a, b) => ((*a).clone(), (*b).clone()), _ => panic!("not a fork") } } }

#[test]
fn programs_as_text() {
    use rust_ic_strands::twoway::parse_run;
    let (p, x) = parse_run("add 2 3").unwrap();
    assert_eq!((p, x.clone()), (add(), Val::f(nat(2), nat(3))));
    assert_eq!(add().run(&x, &mut 100).unwrap().decoded(), "(2, 5)");
    let (p, x) = parse_run("~(swap; (s2f | id)) @ ((△, △), △△)").unwrap();
    assert_eq!(p, seq(par(Prog::ForkStem, Prog::Id), Prog::Swap));
    assert_eq!(x, Val::f(Val::f(Val::L, Val::L), Val::s(Val::L)));
    // `show` drops how steps nest, which changes nothing they do.
    let (q, _) = parse_run(&format!("{} @ 3", add().show())).unwrap();
    assert_eq!(q.run(&Val::f(nat(2), nat(3)), &mut 100), add().run(&Val::f(nat(2), nat(3)), &mut 100));
    let (p, x) = parse_run("twice @ ([succ], 3)").unwrap();
    assert_eq!(p.run(&x, &mut 100).unwrap().decoded(), format!("({}, 5)", twoway::succ().code().show()));
    assert_eq!(parse_run("mirror (△, △△)").unwrap(), (call(), Val::f(mirror().code(), Val::f(Val::L, Val::s(Val::L)))));
    assert!(parse_run("frob @ △").is_err());
}
