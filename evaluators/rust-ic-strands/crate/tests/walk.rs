//! The persistent two-way machine (src/walk.rs): it gives what the direct run gives, runs back to its
//! exact start, and makes agents only on the occasions it says.
use rust_ic_strands::iso::{library, parse_call, run};
use rust_ic_strands::walk::Walker;

#[test]
fn the_persistent_machine() {
    let defs = library();
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).unwrap();
    let disp = |name: &str| { let at = js.find(&format!("\"name\": \"{name}\"")).unwrap(); let t = &js[at..]; let s = t.find("\"tree\": \"").unwrap() + 9; format!("#{}", &t[s..s + t[s..].find('"').unwrap()]) };
    let calls: Vec<String> = ["swap 1 2", "succ 3", "add 2 3", "add' 2 5", "double 3", "snoc [1, 2] 3", "rev [1, 2, 3]", "revfast [1, 2, 3]", "rev' [2, 1]",
        "tc F(L,L) L", "tck F(S(F(L,L)),F(L,L)) S(L)", "bennett F(F(L,S(L)),L) F(L,L)", "run add 2 3", "dagger add"].iter().map(|s| s.to_string())
        .chain([format!("tc {} 1 1", disp("add")), format!("tck {} 1 1", disp("add")), format!("bennett {} 1 1", disp("add"))]).collect();
    let mut report = vec![];
    for src in &calls {
        let (main, input) = parse_call(&defs, src).unwrap();
        let want = run(&defs, &main, input.clone(), &mut { u64::MAX }).unwrap();
        let mut w = Walker::new(defs.clone(), &main, &input);
        let (code0, data0, _) = w.agents();
        let n = w.run(u64::MAX).unwrap();
        assert!(w.done(), "{src}: not at the end");
        assert_eq!(w.payload(), want, "{src}");
        let (code1, data1, stack1) = w.agents();
        assert_eq!(stack1, 0, "{src}: frames left on the stack");
        assert_eq!(code1 as i64 - code0 as i64 + data1 as i64 - data0 as i64, w.made as i64 - w.gone as i64, "{src}: agents made and gone do not add up");
        let (made, gone) = (w.made, w.gone);
        w.flip();
        w.run(u64::MAX).unwrap();
        assert!(w.done() && w.clock == 0, "{src}: not back at the start");
        assert_eq!(w.payload(), input, "{src}: not the input");
        let short = if src.len() > 40 { format!("{}…", &src[..40]) } else { src.clone() };
        report.push(format!("{short}: {n} steps; agents at the start {}, made {made}, taken away {gone}", code0 + data0));
    }
    eprintln!("{}", report.join("\n"));
}

/// Many tokens: a `both` splits the token, the halves come back together. By rounds (everything that
/// can move, at once) the answer is the one-at-a-time answer and the run goes back to its input; moved
/// in random order, time turning around at random, it still ends exactly where it began.
#[test]
fn tokens_split_and_come_back_together() {
    use rust_ic_strands::walk::Actor;
    let defs = library();
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).unwrap();
    let disp = |name: &str| { let at = js.find(&format!("\"name\": \"{name}\"")).unwrap(); let t = &js[at..]; let s = t.find("\"tree\": \"").unwrap() + 9; format!("#{}", &t[s..s + t[s..].find('"').unwrap()]) };
    let calls: Vec<String> = ["tck F(S(F(L,L)),F(L,L)) S(L)", "dagger add", "dagger tck", "add 2 3"].iter().map(|s| s.to_string())
        .chain([format!("tck {} 1 1", disp("add")), format!("tck {} 2", disp("fib")), format!("bennett {} 1 1", disp("add"))]).collect();
    let mut rng = rust_ca_lattice::oracle::Lcg(7);
    let mut report = vec![];
    for src in &calls {
        let (main, input) = parse_call(&defs, src).unwrap();
        let want = run(&defs, &main, input.clone(), &mut { u64::MAX }).unwrap();
        let mut w = Walker::new(defs.clone(), &main, &input);
        let (mut rounds, mut moves, mut most) = (0, 0, 1);
        loop { let k = w.round().unwrap(); if k == 0 { break; } rounds += 1; moves += k; most = most.max(w.tokens()); }
        assert_eq!(w.tokens(), 1, "{src}: the halves did not all come back together");
        assert_eq!(w.payload(), want, "{src}: by rounds");
        w.flip();
        while w.round().unwrap() > 0 {}
        assert!(w.clock == 0 && w.payload() == input && w.forks.iter().flatten().count() == 0, "{src}: not back at the input by rounds");
        // Any order: a random actor each move, time turning around now and then; then all the way back.
        w.flip();
        for _ in 0..3 * moves {
            if rng.next() < 0.02 { w.flip(); }
            let a = w.actors();
            if a.is_empty() { w.flip(); continue; }
            let pick: Actor = a[((rng.next() * a.len() as f64) as usize).min(a.len() - 1)];
            let _ = pick;
            w.step_by(pick).unwrap();
        }
        if w.forward { w.flip(); }
        while w.round().unwrap() > 0 {}
        assert!(w.clock == 0 && w.payload() == input && w.tokens() == 1, "{src}: not back at the input after moving in random order");
        let short = if src.len() > 40 { format!("{}…", &src[..40]) } else { src.clone() };
        report.push(format!("{short}: {moves} moves in {rounds} rounds, at most {most} tokens"));
    }
    eprintln!("{}", report.join("\n"));
}

/// A jet: disp's `add` and `rev`, recognised by their trees, run as the library's two-way `add` and
/// `rev`, and give the answers the tree calculus gives; forward to the end and back to the input.
#[test]
fn jets_give_what_the_tree_calculus_gives() {
    let js = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../player/programs.js")).unwrap();
    let tree = |name: &str| { let at = js.find(&format!("\"name\": \"{name}\"")).unwrap(); let t = &js[at..]; let s = t.find("\"tree\": \"").unwrap() + 9; t[s..s + t[s..].find('"').unwrap()].to_string() };
    let lit = |s: &str| { let (_, t) = parse_call(&library(), &format!("swap {s} L")).unwrap(); let rust_ca_lattice::oracle::Term::F(a, _) = t else { unreachable!() }; (*a).clone() };
    let defs = library().with_jets(&["add", "rev"].map(|n| (n.to_string(), lit(&format!("#{}", tree(n))))));
    // the answer: the last thing applied, under the trace
    let answer = |t: &rust_ca_lattice::oracle::Term| { use rust_ca_lattice::oracle::Term::F; let F(l, _) = t else { panic!() }; let F(_, r) = &**l else { panic!() }; (**r).clone() };
    let mut calls: Vec<String> = vec![];
    for a in 0..5 { for b in 0..5 { calls.push(format!("#{} {a} {b}", tree("add"))); } }
    for xs in ["[]", "[1]", "[1, 2, 3]", "[3, 0, 2, 2]"] { calls.push(format!("#{} {xs}", tree("rev"))); }
    let (mut slow, mut fast) = (0, 0);
    for c in &calls {
        let (main, input) = parse_call(&defs, &format!("tck {c}")).unwrap();
        let want = answer(&run(&defs, &main, input, &mut { u64::MAX }).unwrap());
        let (main, input) = parse_call(&defs, &format!("jet {c}")).unwrap();
        assert_eq!(answer(&run(&defs, &main, input.clone(), &mut { u64::MAX }).unwrap()), want, "jet {}…", &c[..20]);
        let mut w = Walker::new(defs.clone(), &main, &input);
        fast += w.run(u64::MAX).unwrap();
        assert_eq!(answer(&w.payload()), want);
        w.flip();
        w.run(u64::MAX).unwrap();
        assert!(w.clock == 0 && w.payload() == input, "jet: not back at the input");
        let (main, input) = parse_call(&defs, &format!("tck {c}")).unwrap();
        slow += Walker::new(defs.clone(), &main, &input).run(u64::MAX).unwrap();
    }
    eprintln!("{} calls: {slow} moves interpreted, {fast} by jets", calls.len());
}
