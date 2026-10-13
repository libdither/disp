//! What the derived tree calculus's trace holds (not a check: prints a breakdown).
use rust_ca_lattice::oracle::Term;
use rust_ic_strands::iso::{library, nat, run, tc_apply, tck_apply};
use std::collections::HashMap;
use std::rc::Rc;
fn fork(a: Term, b: Term) -> Term { Term::F(Rc::new(a), Rc::new(b)) }
fn disp(name: &str) -> Term {
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).unwrap();
    let at = js.find(&format!("\"name\": \"{name}\"")).unwrap();
    let tree = &js[at..];
    let start = tree.find("\"tree\": \"").unwrap() + 9;
    let end = start + tree[start..].find('"').unwrap();
    rust_ic_mesh::term::ternary(&tree[start..end]).unwrap()
}
fn size(t: &Term) -> usize { match t { Term::L => 1, Term::S(a) => 1 + size(a), Term::F(a, b) => 1 + size(a) + size(b), _ => 0 } }
/// Hash-consing: each distinct subtree gets one id.
struct Cons(HashMap<(u8, u32, u32), u32>);
impl Cons {
    fn id(&mut self, t: &Term) -> u32 {
        let key = match t { Term::L => (0, 0, 0), Term::S(a) => (1, self.id(a), 0), Term::F(a, b) => { let x = self.id(a); (2, x, self.id(b)) } _ => unreachable!() };
        let n = self.0.len() as u32;
        *self.0.entry(key).or_insert(n)
    }
}
#[test]
fn trace_breakdown() {
    let defs = library();
    for (name, args) in [("add", vec![nat(1), nat(1)]), ("fib", vec![nat(3)]), ("mul", vec![nat(2), nat(2)])] {
        let p = disp(name);
        let app = args.iter().fold(p.clone(), |a, x| fork(a, x.clone()));
        let out = run(&defs, &tc_apply(&defs, args.len()), fork(app.clone(), Term::L), &mut { u64::MAX }).unwrap();
        let Term::F(_, trace) = &out else { panic!() };
        // the entries, newest first
        let mut entries = vec![];
        let mut t = &**trace;
        while let Term::F(e, rest) = t { entries.push(&**e); t = rest; }
        let (mut tags, mut k, mut tri, mut n_k, mut n_tri) = (0, 0, 0, 0, 0);
        let mut cons = Cons(HashMap::new());
        let input_ids: std::collections::HashSet<u32> = { let mut s = std::collections::HashSet::new(); fn all(t: &Term, c: &mut Cons, s: &mut std::collections::HashSet<u32>) { s.insert(c.id(t)); match t { Term::S(a) => all(a, c, s), Term::F(a, b) => { all(a, c, s); all(b, c, s) } _ => {} } } all(&app, &mut cons, &mut s); s };
        let before = cons.0.len();
        let (mut dropped_from_input, mut dropped) = (0, 0);
        for e in &entries {
            match e {
                Term::L | Term::S(_) => tags += size(e),
                Term::F(a, d) => match &**a {
                    Term::L => { k += size(d); n_k += 1; dropped += 1; if input_ids.contains(&cons.id(d)) { dropped_from_input += 1; } tags += 2; }
                    Term::S(_) => tags += size(e),
                    _ => { tri += size(d); n_tri += 1; let Term::F(x, y) = &**d else { panic!() }; for z in [x, y] { dropped += 1; if input_ids.contains(&cons.id(z)) { dropped_from_input += 1; } } tags += size(a) + 1; }
                },
                _ => {}
            }
        }
        let distinct = { let mut c2 = Cons(HashMap::new()); c2.id(trace); c2.0.len() };
        let new_vs_input = { cons.id(trace); cons.0.len() - before };
        eprintln!("{name}: trace {} nodes, {} entries: tags {tags}, K drops {n_k} ({k} nodes), triage drops {n_tri} ({tri} nodes); distinct subtrees {distinct}, not already in the input {new_vs_input}; dropped pieces that are subtrees of the input: {dropped_from_input} of {dropped}",
            size(trace), entries.len());
    }
}

#[test]
fn tck_trace() {
    let defs = library();
    for (name, args) in [("add", vec![nat(1), nat(1)]), ("fib", vec![nat(3)]), ("mul", vec![nat(2), nat(2)])] {
        let app = args.iter().fold(disp(name), |a, x| fork(a, x.clone()));
        for (label, prog) in [("tc", tc_apply(&defs, args.len())), ("tck", tck_apply(&defs, args.len()))] {
            let out = run(&defs, &prog, fork(app.clone(), Term::L), &mut { u64::MAX }).unwrap();
            let Term::F(_, trace) = &out else { panic!() };
            let mut entries = 0; let mut t = &**trace; while let Term::F(_, rest) = t { entries += 1; t = rest; }
            let distinct = { let mut c = Cons(HashMap::new()); c.id(trace); c.0.len() };
            let new = { let mut c = Cons(HashMap::new()); c.id(&app); let before = c.0.len(); c.id(trace); c.0.len() - before };
            eprintln!("{name} {label}: trace {entries} entries, {} nodes as a tree, {distinct} distinct, {new} not in the input", size(trace));
        }
    }
}
