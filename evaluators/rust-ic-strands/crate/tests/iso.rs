//! Two-way programs (src/iso.rs): every program's inverse undoes it; the machine, run to the end and
//! then with its token turned around back to the beginning, ends where it started; the tree calculus,
//! derived as the two-way program `tc`, gives eager evaluation's answers, and its trace takes it back.
use rust_ca_lattice::oracle::{Lcg, Term};
use rust_ic_strands::iso::{bennett, decode, encode, library, nat, parse_call, run, show, tc_apply, Iso, Machine};
use std::rc::Rc;

fn fork(a: Term, b: Term) -> Term { Term::F(Rc::new(a), Rc::new(b)) }
fn list(xs: &[usize]) -> Term { xs.iter().rev().fold(Term::L, |t, &x| fork(nat(x), t)) }

/// The tree calculus, eagerly, on trees (as research/REVERSIBLE_PIPELINE.md's test of disp's programs did).
fn eager(p: &Term, x: &Term, fuel: &mut u64) -> Option<Term> {
    if *fuel == 0 { return None; }
    *fuel -= 1;
    Some(match p {
        Term::L => Term::S(Rc::new(x.clone())),
        Term::S(a) => fork((**a).clone(), x.clone()),
        Term::F(a, b) => match &**a {
            Term::L => (**b).clone(),
            Term::S(c) => { let r = eager(c, x, fuel)?; let q = eager(b, x, fuel)?; eager(&r, &q, fuel)? }
            Term::F(w, y) => match x {
                Term::L => (**w).clone(),
                Term::S(u) => eager(y, u, fuel)?,
                Term::F(u, v) => { let r = eager(b, u, fuel)?; eager(&r, v, fuel)? }
                Term::Ap(..) => unreachable!(),
            },
            Term::Ap(..) => unreachable!(),
        },
        Term::Ap(..) => unreachable!(),
    })
}
fn rand_tree(rng: &mut Lcg, depth: u32) -> Term {
    if depth == 0 || rng.next() < 0.3 { return Term::L; }
    if rng.next() < 0.3 { Term::S(Rc::new(rand_tree(rng, depth - 1))) } else { fork(rand_tree(rng, depth - 1), rand_tree(rng, depth - 1)) }
}

#[test]
fn inverses_read_off_the_text() {
    let defs = library();
    for b in &defs.bodies { assert_eq!(&b.dagger().dagger(), b); }
    let call = |name: &str, inv: bool| Iso::Call(defs.find(name).unwrap(), inv);
    let mut fuel = u64::MAX;
    for a in 0..6 {
        for b in 0..6 {
            let out = run(&defs, &call("add", false), fork(nat(a), nat(b)), &mut fuel).unwrap();
            assert_eq!(out, fork(nat(a), nat(a + b)));
            assert_eq!(run(&defs, &call("add", true), out, &mut fuel).unwrap(), fork(nat(a), nat(b)));
        }
        assert_eq!(run(&defs, &call("double", false), nat(a), &mut fuel).unwrap(), fork(nat(a), nat(2 * a)));
    }
    assert!(run(&defs, &call("add", true), fork(nat(3), nat(2)), &mut fuel).is_err(), "taking away more than is there");
    let mut rng = Lcg(3);
    for n in 0..8 {
        let xs: Vec<usize> = (0..n).map(|_| (rng.next() * 4.0) as usize).collect();
        let back: Vec<usize> = xs.iter().rev().copied().collect();
        for name in ["rev", "revfast"] {
            let out = run(&defs, &call(name, false), list(&xs), &mut fuel).unwrap();
            assert_eq!(out, list(&back), "{name} {xs:?}");
            assert_eq!(run(&defs, &call(name, true), out, &mut fuel).unwrap(), list(&xs));
        }
    }
}

#[test]
fn programs_are_trees() {
    let defs = library();
    let dagger = Iso::Call(defs.find("dagger").unwrap(), false);
    let mut fuel = u64::MAX;
    for (name, body) in defs.names.iter().zip(&defs.bodies) {
        assert_eq!(decode(&encode(body)).map(|p| encode(&p)), Some(encode(body)), "{name}: decoding undoes encoding, up to the pieces' letters");
        // The inverse, worked out by a two-way program on the program as a tree, is the one read off the text.
        let inv = run(&defs, &dagger, encode(body), &mut fuel).unwrap_or_else(|e| panic!("dagger {name}: {e}"));
        assert_eq!(inv, encode(&body.dagger()), "dagger {name}");
        assert_eq!(run(&defs, &Iso::Call(defs.find("dagger").unwrap(), true), inv, &mut fuel).unwrap(), encode(body), "dagger⁻¹ {name}");
    }
    // A program handed over as data runs as itself, and stays.
    let add = encode(&defs.bodies[defs.find("add").unwrap()]);
    let out = run(&defs, &Iso::Eval(false), fork(add.clone(), fork(nat(2), nat(3))), &mut fuel).unwrap();
    assert_eq!(out, fork(add, fork(nat(2), nat(5))));
}

#[test]
fn the_tree_calculus_derived() {
    let defs = library();
    let mut rng = Lcg(17);
    let (mut tried, mut steps) = (0, 0u64);
    for _ in 0..400 {
        let (p, x) = (rand_tree(&mut rng, 5), rand_tree(&mut rng, 4));
        let Some(want) = eager(&p, &x, &mut 2000) else { continue };
        let mut fuel = 5_000_000;
        let out = run(&defs, &tc_apply(&defs, 1), fork(fork(p.clone(), x.clone()), Term::L), &mut fuel).unwrap();
        let Term::F(r, trace) = &out else { panic!() };
        assert_eq!(**r, want, "tc {} {}", show(&p), show(&x));
        // Back from the answer and the trace to the program and its argument.
        assert_eq!(run(&defs, &tc_apply(&defs, 1).dagger(), out.clone(), &mut { u64::MAX }).unwrap(), fork(fork(p.clone(), x.clone()), Term::L));
        // Cleaned up: the answer next to the program and its argument, no trace.
        assert_eq!(run(&defs, &bennett(tc_apply(&defs, 1)), fork(p.clone(), x.clone()), &mut { u64::MAX }).unwrap(), fork(fork(p, x), want));
        let _ = trace;
        tried += 1;
        steps += 5_000_000 - fuel;
    }
    eprintln!("{tried} random applications agree with eager evaluation ({steps} steps)");
    assert!(tried > 200);
}

#[test]
fn the_machine_runs_back_by_turning_its_token_around() {
    let defs = library();
    let mut rng = Lcg(11);
    let mut report = vec![];
    for src in ["add 2 3", "add' 2 5", "double 3", "snoc [1, 2] 3", "rev [1, 2, 3]", "revfast [1, 2, 3]", "rev' [2, 1]",
                "tc F(L,L) L", "tc F(S(L),L) S(L)", "tc F(F(L,S(L)),L) S(L)", "bennett F(F(L,S(L)),L) F(L,L)", "run add 2 3", "dagger add", "dagger snoc"] {
        let (main, input) = parse_call(&defs, src).unwrap();
        let mut fuel = u64::MAX;
        let want = run(&defs, &main, input.clone(), &mut fuel).unwrap();
        let mut m = Machine::new(defs.clone(), &main, &input);
        let (prog, nodes) = (m.live(), m.nodes());
        let n = m.run(10_000_000).unwrap();
        assert!(m.done(), "{src}: not at the end");
        assert_eq!(m.payload(), want, "{src}");
        assert_eq!((m.depth, m.live()), (0, prog), "{src}: definitions left laid out");
        for _ in 0..20 {
            let k = (rng.next() * n as f64) as u64;
            m.flip(); m.run(k).unwrap(); m.flip(); m.run(k / 2).unwrap();
        }
        m.flip();
        m.run(u64::MAX).unwrap();
        assert!(m.done() && m.clock == 0, "{src}: not back at the beginning (clock {})", m.clock);
        assert_eq!(m.payload(), input, "{src}: not the input");
        assert_eq!((m.depth, m.live()), (0, prog), "{src}: not as it started");
        assert!(m.nodes() <= nodes, "{src}: more tree nodes than at the start");
        report.push(format!("{src}: {n} steps"));
    }
    eprintln!("{}", report.join("\n"));
}

/// A disp program as the player has it (player/programs.js, compiled by the real elaborator).
fn disp(name: &str) -> Term {
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).unwrap();
    let at = js.find(&format!("\"name\": \"{name}\"")).unwrap_or_else(|| panic!("no disp program {name}"));
    let tree = &js[at..];
    let start = tree.find("\"tree\": \"").unwrap() + 9;
    let end = start + tree[start..].find('"').unwrap();
    rust_ic_mesh::term::ternary(&tree[start..end]).unwrap()
}

#[test]
fn disp_on_the_derived_tree_calculus() {
    let defs = library();
    let app = |f: Term, xs: &[Term]| xs.iter().fold(f, |a, x| fork(a, x.clone()));
    for (name, args, want) in [("add", vec![nat(1), nat(1)], nat(2)), ("add", vec![nat(2), nat(3)], nat(5)), ("mul", vec![nat(2), nat(2)], nat(4)),
                               ("fib", vec![nat(3)], nat(2)), ("rev", vec![list(&[1, 2, 3])], list(&[3, 2, 1]))] {
        let input = fork(app(disp(name), &args), Term::L);
        let out = run(&defs, &tc_apply(&defs, args.len()), input.clone(), &mut { u64::MAX }).unwrap();
        let Term::F(r, _) = &out else { panic!() };
        assert_eq!(**r, want, "{name}");
        assert_eq!(run(&defs, &tc_apply(&defs, args.len()).dagger(), out, &mut { u64::MAX }).unwrap(), input, "{name} back");
    }
    // rev is its own inverse: as a bijection it leaves nothing over but itself, twice.
    let rev = disp("rev");
    let out = run(&defs, &rust_ic_strands::iso::bijection(&defs), fork(fork(rev.clone(), rev.clone()), list(&[1, 2, 3])), &mut { u64::MAX }).unwrap();
    assert_eq!(out, fork(fork(rev.clone(), rev.clone()), list(&[3, 2, 1])));
    // add is not one: its "inverse" here does not give the argument back, and the run says so.
    let add1 = fork(disp("add"), nat(1));
    assert!(run(&defs, &rust_ic_strands::iso::bijection(&defs), fork(fork(add1.clone(), add1), nat(2)), &mut { u64::MAX }).is_err());
}

/// The answer at the end of a `tck` run on k arguments: the right of what the token carries' left.
fn tck_answer(out: &Term) -> Term { let Term::F(a, _) = out else { panic!() }; let Term::F(_, r) = &**a else { panic!() }; (**r).clone() }

#[test]
fn the_tree_calculus_keeping_its_programs() {
    let defs = library();
    let mut rng = Lcg(23);
    let mut tried = 0;
    for _ in 0..400 {
        let (p, x) = (rand_tree(&mut rng, 5), rand_tree(&mut rng, 4));
        let Some(want) = eager(&p, &x, &mut 2000) else { continue };
        let input = fork(fork(p.clone(), x.clone()), Term::L);
        let out = run(&defs, &rust_ic_strands::iso::tck_apply(&defs, 1), input.clone(), &mut { 5_000_000 }).unwrap();
        assert_eq!(out, fork(fork(p.clone(), want), match &out { Term::F(_, t) => (**t).clone(), _ => unreachable!() }), "tck keeps the program");
        assert_eq!(run(&defs, &rust_ic_strands::iso::tck_apply(&defs, 1).dagger(), out, &mut { u64::MAX }).unwrap(), input);
        tried += 1;
    }
    assert!(tried > 200);
    for (name, args, want) in [("add", vec![nat(1), nat(1)], nat(2)), ("fib", vec![nat(3)], nat(2)), ("mul", vec![nat(2), nat(2)], nat(4))] {
        let input = fork(args.iter().fold(disp(name), |a, x| fork(a, x.clone())), Term::L);
        let out = run(&defs, &rust_ic_strands::iso::tck_apply(&defs, args.len()), input.clone(), &mut { u64::MAX }).unwrap();
        assert_eq!(tck_answer(&out), want, "{name}");
        assert_eq!(run(&defs, &rust_ic_strands::iso::tck_apply(&defs, args.len()).dagger(), out, &mut { u64::MAX }).unwrap(), input, "{name} back");
    }
}
