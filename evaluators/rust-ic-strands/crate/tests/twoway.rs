//! Two-way programs on the lattice (twoway.rs, rules.rs `TWO_RULES`): the lattice gives what the
//! reference interpreter gives and hands the program back, and the inverse program takes the result
//! back to the input.

use rust_ca_lattice::oracle::Lcg;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::twoway::{self, add, cond, nat, par, seq, Prog, Val};

fn params(seed: u64) -> Params { Params { two: true, lazy: false, seed, ..latest() } }

/// Runs p on x on the lattice: the program handed back and the result (None if stuck anywhere), and
/// the clocks it took.
fn on_lattice(p: &Prog, x: &Val, seed: u64) -> (Option<(Prog, Val)>, f64) {
    let (net, out) = twoway::net(p, x);
    let side = ((net.live_count() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut l = Lattice::load(Params { w: side, h: side, ..params(seed) }, net, out).expect("load");
    l.run(50_000_000);
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
            assert_eq!(add().dagger().unwrap().run(&y, &mut 1000), Some(x), "sub {a} {b}");
        }
    }
}

#[test]
fn add_on_the_lattice() {
    for (a, b) in [(0, 0), (1, 2), (3, 1), (4, 4)] {
        let x = Val::f(nat(a), nat(b));
        let (got, clocks) = on_lattice(&add(), &x, 1);
        let (p, y) = got.unwrap_or_else(|| panic!("add {a} {b}: no result"));
        assert_eq!(y, Val::f(nat(a), nat(a + b)), "add {a} {b}");
        assert_eq!(p, add(), "add {a} {b}: the program handed back");
        let (back, _) = on_lattice(&add().dagger().unwrap(), &y, 2);
        assert_eq!(back.map(|r| r.1), Some(x), "sub {a} {b}");
        eprintln!("add {a} {b}: {clocks:.0} clocks");
    }
}

fn rand_val(rng: &mut Lcg, d: u32) -> Val {
    let x = rng.next();
    if d == 0 || x < 0.3 { Val::L } else if x < 0.5 { Val::s(rand_val(rng, d - 1)) } else { Val::f(rand_val(rng, d - 1), rand_val(rng, d - 1)) }
}

fn rand_prog(rng: &mut Lcg, d: u32) -> Prog {
    use Prog::*;
    let leaves = [Id, Swap, AssocL, AssocR, StemFork, ForkStem, Uniti, Unite, Copy, Merge, Flip];
    let x = rng.next();
    if d == 0 || x < 0.45 { return leaves[(rng.next() * leaves.len() as f64) as usize % leaves.len()].clone(); }
    let (a, b) = (rand_prog(rng, d - 1), rand_prog(rng, d - 1));
    match (x * 30.0) as u32 % 3 { 0 => seq(a, b), 1 => par(a, b), _ => cond(a, b) }
}

/// Random programs of every opcode but loops and eval, on random trees: where the reference gives a
/// result, so does the lattice, with the program handed back, and the inverse takes it back; where
/// the reference is stuck, so is the lattice.
#[test]
fn random_programs() {
    let mut rng = Lcg(20261015);
    let (mut ran, mut stuck) = (0, 0);
    while ran < 120 {
        let (p, x) = (rand_prog(&mut rng, 4), rand_val(&mut rng, 4));
        let want = p.run(&x, &mut 1000);
        if want.is_none() && stuck >= 30 { continue; }
        let (got, _) = on_lattice(&p, &x, ran as u64 + stuck as u64);
        match want {
            None => { assert_eq!(got, None, "{} on {}: the lattice gave a result where the reference is stuck", p.show(), x.show()); stuck += 1; }
            Some(y) => {
                let (q, v) = got.unwrap_or_else(|| panic!("{} on {}: no result", p.show(), x.show()));
                assert_eq!(v, y, "{} on {}", p.show(), x.show());
                assert_eq!(q, p, "{} on {}: the program handed back", p.show(), x.show());
                let (back, _) = on_lattice(&p.dagger().unwrap(), &y, 7);
                assert_eq!(back.map(|r| r.1), Some(x.clone()), "{} back from {}", p.show(), y.show());
                ran += 1;
            }
        }
    }
}

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
    assert!(parse_run("frob @ △").is_err());
}
