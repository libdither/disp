//! The search with strict reversibility dropped (option 1 after Stage 1): footprint marks are a cost to
//! measure, not a filter; the signature varies; and a system is adequate when it runs the interaction
//! combinators through gadgets (ic.rs).

use crate::behave::{behave, Behaviour, Rng};
use crate::ic::{self, Sim};
use crate::live::Piece;
use crate::space::{self, Sig};
use std::fmt::Write;

/// `which`: the signatures to search, by name (A, AU, AB, AUV, ABU), or all four of A, AU, AB and AUV.
/// `same_wires`: a pair of the same kind gets only right sides of wires alone, as in the combinators.
pub fn run(which: &str, g: usize, k: usize, budget: u64, same_wires: bool) {
    let sigs = [
        Sig::new("A", &[2], true),
        Sig::new("AU", &[2, 1], true),
        Sig::new("AB", &[2, 2], true),
        Sig::new("AUV", &[2, 1, 1], true),
        Sig::new("ABU", &[2, 2, 1], true),
    ];
    std::fs::create_dir_all("out").unwrap();
    for sig in &sigs {
        if (which == "all" && sig.name != "ABU") || which == sig.name {
            one(sig, g, k, budget, same_wires);
        }
    }
}

/// The choice of right side for each pair, from a system's number.
fn decode(mut i: u128, sizes: &[usize]) -> Vec<usize> {
    sizes
        .iter()
        .map(|&s| {
            let c = (i % s as u128) as usize;
            i /= s as u128;
            c
        })
        .collect()
}

struct Found {
    choice: Vec<usize>,
    sims: Vec<Sim>,
}

fn one(sig: &Sig, g: usize, k: usize, budget: u64, same_wires: bool) {
    let t0 = std::time::Instant::now();
    let pairs = sig.pairs();
    let spaces: Vec<Vec<Piece>> = pairs
        .iter()
        .map(|&(a, b)| {
            let sp = space::rule_space(sig, a, b, k);
            if same_wires && a == b { sp.into_iter().filter(|p| p.kinds.is_empty()).collect() } else { sp }
        })
        .collect();
    let sizes: Vec<usize> = spaces.iter().map(Vec::len).collect();
    let rules = pairs.len() + sig.kinds() as usize;
    let bits = (rules as f64).log2();
    let name: String = (0..sig.kinds()).map(|x| sig.letter(x)).collect();
    println!("\n== {name} (auxiliary ports {:?}): {rules} rules, a mark names one in {bits:.2} bits", sig.arity);
    for (&(a, b), s) in pairs.iter().zip(&sizes) {
        print!("{}{}: {s} right sides; ", sig.letter(a), sig.letter(b));
    }
    println!();

    // the footprint law on every right side tried, and the eraser's
    let (mut held, mut tight, mut all) = (0, 0, 0);
    let mut check = |a: u8, b: u8, p: &Piece| {
        let (ok, eq) = space::law(sig, a, b, p);
        all += 1;
        held += ok as usize;
        tight += eq as usize;
    };
    for (&(a, b), sp) in pairs.iter().zip(&spaces) {
        sp.iter().for_each(|p| check(a, b, p));
    }
    if let Some(e) = sig.eraser {
        (0..sig.kinds()).for_each(|x| check(x, e, &space::eraser_rule(sig, x)));
    }
    println!("footprint law (parts − 1 ≥ drop in Σ(aux − 1) / 2): holds for {held} of {all} right sides, with equality for {tight}");

    let gadgets = ic::gadgets(sig, g);
    let total: u128 = sizes.iter().map(|&s| s as u128).product();
    let exhaustive = total <= budget as u128;
    let n = if exhaustive { total as u64 } else { budget };
    println!("gadgets of up to {g} agents: {}; systems: {total}, {} {n}", gadgets.len(),
        if exhaustive { "all" } else { "sampled" });
    // a combinator rule leaves only wires (needs a right side with a bare wire) or doubles the agents
    // (needs one that grows)
    let flags: Vec<Vec<(bool, bool)>> = spaces
        .iter()
        .map(|sp| sp.iter().map(|p| (space::parts(sig, p).1 > 0, p.kinds.len() > 2)).collect())
        .collect();
    let threads = 16u64;
    let (found, maybe): (Vec<Found>, u64) = std::thread::scope(|sc| {
        let hs: Vec<_> = (0..threads)
            .map(|t| {
                let (spaces, sizes, flags, gadgets) = (&spaces, &sizes, &flags, &gadgets);
                sc.spawn(move || {
                    let mut rng = Rng(0x9E37_79B9_7F4A_7C15 ^ (t + 1).wrapping_mul(0x2545_F491_4F6C_DD1D));
                    let (mut found, mut maybe) = (vec![], 0u64);
                    let mut i = t;
                    while i < n {
                        let choice = if exhaustive { decode(i as u128, sizes) } else { sizes.iter().map(|&s| rng.below(s)).collect() };
                        i += threads;
                        let f = choice.iter().enumerate().map(|(p, &c)| flags[p][c]);
                        if !f.clone().any(|x| x.0) || !f.clone().any(|x| x.1) {
                            continue;
                        }
                        maybe += 1;
                        let chosen: Vec<&Piece> = choice.iter().zip(spaces).map(|(&c, sp)| &sp[c]).collect();
                        let sims = ic::simulate(&space::system(sig, &chosen), sig, gadgets);
                        if !sims.is_empty() {
                            found.push(Found { choice, sims });
                        }
                    }
                    (found, maybe)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().unwrap()).fold((vec![], 0), |(mut f, m), (f2, m2)| {
            f.extend(f2);
            (f, m + m2)
        })
    });
    let sims: usize = found.iter().map(|f| f.sims.len()).sum();
    let orbits: std::collections::HashSet<Vec<Vec<u8>>> = found
        .iter()
        .map(|f| space::system_key(sig, &f.choice.iter().zip(&spaces).map(|(&c, sp)| &sp[c]).collect::<Vec<_>>()))
        .collect();
    println!("with a bare wire and a growing rule: {maybe}; adequate: {} systems ({} up to renaming), {sims} ways ({:.1} s)",
        found.len(), orbits.len(), t0.elapsed().as_secs_f64());

    // each way's cost per combinator rule, and those no other way beats on every count
    let agents = |s: &Sim| gadgets[s.gamma].kinds.len() + gadgets[s.delta].kinds.len();
    let score = |s: &Sim| -> (usize, u64, u64) {
        (agents(s), s.cost.iter().map(|c| c.interactions).sum(), s.cost.iter().map(|c| c.marks).sum())
    };
    let mut all_ways: Vec<(&Found, &Sim)> = found.iter().flat_map(|f| f.sims.iter().map(move |s| (f, s))).collect();
    all_ways.sort_by_key(|(_, s)| score(s));
    let front: Vec<&(&Found, &Sim)> = all_ways
        .iter()
        .filter(|(_, s)| {
            let a = score(s);
            !all_ways.iter().any(|(_, t)| {
                let b = score(t);
                b.0 <= a.0 && b.1 <= a.1 && b.2 <= a.2 && b != a
            })
        })
        .collect();
    let mut report = String::new();
    let describe = |f: &Found, s: &Sim, out: &mut String| {
        let rules: Vec<String> = pairs
            .iter()
            .zip(&f.choice)
            .zip(&spaces)
            .map(|((&(a, b), &c), sp)| format!("{}{} → {}", sig.letter(a), sig.letter(b), sig.text(&sp[c])))
            .collect();
        let per: Vec<String> = s.cost.iter().map(|c| format!("{}/{}", c.interactions, c.marks)).collect();
        writeln!(out, "  {}\n    γ = {}, δ = {}; annihilations {}; interactions/marks for γγ δδ γδ γε δε: {}",
            rules.join("; "), sig.text(&gadgets[s.gamma]), sig.text(&gadgets[s.delta]),
            match s.crossed { (false, false) => "straight", (true, true) => "crossed", _ => "mixed" }, per.join(" ")).unwrap();
    };
    let mut seen_scores = std::collections::HashSet::new();
    println!("not beaten on gadget agents, interactions and marks (summed over the five rules), one of each:");
    for (f, s) in &front {
        if seen_scores.insert(score(s)) {
            let mut line = String::new();
            describe(f, s, &mut line);
            let (a, i, m) = score(s);
            print!("  [{a} agents, {i} interactions, {m} marks = {:.1} bits]\n{line}", m as f64 * bits);
        }
    }
    for (f, s) in &all_ways {
        describe(f, s, &mut report);
    }
    let mode = if same_wires { "-wires" } else { "" };
    std::fs::write(format!("out/search-{name}-g{g}-k{k}{mode}.txt"), report).unwrap();

    // Stage 2: random nets, for the adequate systems and for systems drawn at random
    let sample = |choices: Vec<Vec<usize>>| -> Behaviour {
        let mut sum = Behaviour::default();
        let len = choices.len().max(1) as f64;
        for (i, c) in choices.iter().enumerate() {
            let chosen: Vec<&Piece> = c.iter().zip(&spaces).map(|(&c, sp)| &sp[c]).collect();
            let b = behave(&space::system(sig, &chosen), sig, 30, 60, 0xC0FFEE + i as u64);
            sum.nets += b.nets;
            sum.halted += b.halted;
            sum.blew_up += b.blew_up;
            for (x, y) in [(&mut sum.interactions, b.interactions), (&mut sum.rounds, b.rounds), (&mut sum.peak, b.peak),
                (&mut sum.left, b.left), (&mut sum.erasures, b.erasures), (&mut sum.marks, b.marks)] {
                *x += y / len;
            }
            sum.forms += b.forms;
        }
        sum
    };
    let show = |label: &str, n: usize, b: &Behaviour| {
        println!("  {label} ({n} systems, {} nets of 30 agents): halt {:.0}%, blow up {:.0}%; when halting: {:.1} interactions and {:.2} rounds an agent, peak {:.2}×, left {:.2}×, eraser {:.0}% of interactions, {:.2} marks an interaction, {:.0} normal forms a system",
            b.nets, 100.0 * b.halted as f64 / b.nets.max(1) as f64, 100.0 * b.blew_up as f64 / b.nets.max(1) as f64,
            b.interactions, b.rounds, b.peak, b.left, 100.0 * b.erasures, b.marks, b.forms as f64 / n.max(1) as f64);
    };
    println!("Stage 2, random closed nets:");
    let adequate: Vec<Vec<usize>> = found.iter().take(200).map(|f| f.choice.clone()).collect();
    if !adequate.is_empty() {
        show("adequate", adequate.len(), &sample(adequate.clone()));
    }
    let mut rng = Rng(0xABCDEF);
    let random: Vec<Vec<usize>> = (0..200).map(|_| sizes.iter().map(|&s| rng.below(s)).collect()).collect();
    show("any", random.len(), &sample(random));
}
