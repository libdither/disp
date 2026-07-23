//! Generate `lattice_cascade.js`, the bundle `lattice_player.html` replays. The roster
//! carries the historical suite scenarios (translation, the A·F trio, the full rule
//! atlas, the reduction chains) plus the cascade-native demos. Usage:
//! `cargo run --release --bin dump-cascade -- <out.js>`

use rust_ca_lattice::cascade::Grid2;
use rust_ca_lattice::cascade_run::{
    dock_fixture, lay_wire, load_net, load_net_tree, place_agent, place_obstruction, Discipline,
    Runner,
};
use rust_ca_lattice::cascade_trace::trace_run;
use rust_ca_lattice::lattice::Dir;
use rust_ca_lattice::lattice::{Pos, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{ap, f2, s, Term};
use rust_ca_lattice::rules::{find_index, Tag, RULES};

fn term_runner(term: &Term) -> Runner {
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, _out) = shadow.drive(root);
    let grid = load_net(&shadow, Topo::Full3D).expect("loads");
    Runner::new(grid, shadow, Discipline::Fifo)
}

/// Same, on the tidy-tree embedding: the term reads as an actual syntax tree.
fn tree_runner(term: &Term) -> Runner {
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, _out) = shadow.drive(root);
    let grid = load_net_tree(&shadow, Topo::Full3D).expect("tree loads");
    Runner::new(grid, shadow, Discipline::Fifo)
}

/// An eraser consuming a producer tree: the old cascade-eps-fork, which erases the whole
/// net to empty.
fn eps_fork_runner() -> Runner {
    let mut shadow = Net::new();
    let root = shadow.build(&f2(Term::L, Term::L));
    let eps = shadow.mk(Tag::Eps);
    shadow.link(eps, 0, root, 0);
    let grid = load_net(&shadow, Topo::Full3D).expect("loads");
    Runner::new(grid, shadow, Discipline::Fifo)
}

fn crossing_runner() -> Runner {
    let mut grid = Grid2::new(Topo::Full3D);
    let mut shadow = Net::new();
    let n = 4i32;
    for k in 0..n {
        let y = k * 3;
        let l = place_agent(&mut grid, &mut shadow, (0, y, 0), Tag::L, Dir::E, None);
        let e = place_agent(&mut grid, &mut shadow, (14, y, 0), Tag::Eps, Dir::W, None);
        shadow.link(l.id, 0, e.id, 0);
        lay_wire(&mut grid, &(0..=14).map(|x| (x, y, 0)).collect::<Vec<Pos>>());
    }
    for k in 0..n {
        let x = 3 + k * 3;
        let l = place_agent(&mut grid, &mut shadow, (x, -3, 0), Tag::L, Dir::S, None);
        let e = place_agent(&mut grid, &mut shadow, (x, n * 3, 0), Tag::Eps, Dir::N, None);
        shadow.link(l.id, 0, e.id, 0);
        lay_wire(&mut grid, &(-3..=n * 3).map(|y| (x, y, 0)).collect::<Vec<Pos>>());
    }
    Runner::new(grid, shadow, Discipline::Fifo)
}

fn af_rule() -> &'static rust_ca_lattice::rules::Rule {
    &RULES[find_index(Tag::A, Tag::F).expect("A·F in the ROM")]
}

fn main() {
    let path = std::env::args().nth(1).unwrap_or_else(|| "lattice_cascade.js".into());
    let mut scenarios: Vec<(String, String, Runner, u64)> = vec![];

    // The old must tier, migrated: translation and the A·F workshop trio.
    scenarios.push((
        "translate-straight".into(),
        "F walks three straight cells (demand heats the wire first), docks A, and the A\u{b7}F blocklet grows and fires".into(),
        {
            let (g, s) = dock_fixture(af_rule(), 3, false);
            Runner::new(g, s, Discipline::Fifo)
        },
        2_000_000,
    ));
    scenarios.push((
        "translate-bend".into(),
        "the same walk around a dogleg: four bent cells, then the dock and the fire".into(),
        {
            let (g, s) = dock_fixture(af_rule(), 0, true);
            Runner::new(g, s, Discipline::Fifo)
        },
        2_000_000,
    ));
    scenarios.push((
        "af-fallback".into(),
        "one growth orientation is walled off by an inert obstruction; the dock's roll ladder picks another and still fires".into(),
        {
            let (mut g, mut s) = dock_fixture(af_rule(), 0, false);
            place_obstruction(&mut g, &mut s, (0, -1, 0), Dir::N);
            Runner::new(g, s, Discipline::Fifo)
        },
        2_000_000,
    ));
    scenarios.push((
        "af-declined".into(),
        "every growth orientation is walled off; the dock declines and matter stays unchanged (the old af-blocked)".into(),
        {
            let (mut g, mut s) = dock_fixture(af_rule(), 0, false);
            for (at, toward) in [
                ((0, -1, 0), Dir::N),
                ((0, 1, 0), Dir::S),
                ((0, 0, 1), Dir::U),
                ((0, 0, -1), Dir::D),
            ] {
                place_obstruction(&mut g, &mut s, at, toward);
            }
            Runner::new(g, s, Discipline::Fifo)
        },
        500_000,
    ));

    // The full rule atlas: every ROM interaction fires from a docked pair. Seated rules
    // resolve in the dock transaction itself; scripted rules grow their blocklet first.
    for rule in RULES.iter() {
        let name = format!("rule-{}-{}", rule.consumer.name(), rule.producer.name());
        let rule_idx = rust_ca_lattice::rules::find_index(rule.consumer, rule.producer).unwrap() as u8;
        let seated = rust_ca_lattice::blocklet::layout(rule_idx).seated.is_some();
        let note = if seated {
            format!(
                "{}\u{b7}{} resolves seated: no blocklet, the fresh agents appear in the dying pair's own cells in one transaction",
                rule.consumer.name(), rule.producer.name(),
            )
        } else if rule.fresh.is_empty() {
            format!(
                "{}\u{b7}{} is pure fusion: the cables splice through the dying pair's cells, one transaction",
                rule.consumer.name(), rule.producer.name(),
            )
        } else {
            format!(
                "{}\u{b7}{} fires from a docked pair: seed, blocklet growth, resolve, and the fresh agents parked",
                rule.consumer.name(), rule.producer.name(),
            )
        };
        let (g, s) = dock_fixture(rule, 0, false);
        scenarios.push((name, note, Runner::new(g, s, Discipline::Fifo), 2_000_000));
    }

    // The old example tier, migrated, plus the cascade-native demos.
    scenarios.push((
        "eps-fork".into(),
        "an eraser consumes F(L,L): Eps\u{b7}F places two fresh erasers, the leaves walk in, and the whole net erases to empty".into(),
        eps_fork_runner(),
        2_000_000,
    ));
    scenarios.push((
        "fork-stem".into(),
        "@(F(S(L),L),L) drives A\u{b7}F then T1\u{b7}S, the largest blocklet, and fully normalizes (the old line stalled at arity-three crossings)".into(),
        term_runner(&ap(f2(s(Term::L), Term::L), Term::L)),
        8_000_000,
    ));
    scenarios.push((
        "identity".into(),
        "@(L,L) to S(L): demand heats the wire, P walks and docks Nrm, four fires chain to the normal form delivered at Out".into(),
        term_runner(&ap(Term::L, Term::L)),
        2_000_000,
    ));
    scenarios.push((
        "crossings".into(),
        "eight erasure cascades crossing perpendicularly: walkers guest across each other's wires, every pair erases".into(),
        crossing_runner(),
        2_000_000,
    ));
    scenarios.push((
        "fork-dispatch".into(),
        "@(F(L,L),L) to L, the old embed-term: six chained fires with growth, eviction, guest crossings, and hairpin collapse, fully normalized".into(),
        term_runner(&ap(f2(Term::L, Term::L), Term::L)),
        8_000_000,
    ));

    // The tree tier: bigger terms on the tidy-tree embedding, reading as syntax trees.
    scenarios.push((
        "tree-identity".into(),
        "@(L,L) to S(L) on the tree layout: the Nrm driver on top, four fires walk down the spine".into(),
        tree_runner(&ap(Term::L, Term::L)),
        2_000_000,
    ));
    scenarios.push((
        "tree-k".into(),
        "the K combinator @(K,S(L),L) to S(L): triage picks the first arm and erases the second, in tree form".into(),
        tree_runner(&ap(ap(rust_ca_lattice::oracle::k(), s(Term::L)), Term::L)),
        8_000_000,
    ));
    scenarios.push((
        "tree-fork".into(),
        "@(F(L,L),L) to L on the tree layout: fork dispatch with the two leaves hung as children".into(),
        tree_runner(&ap(f2(Term::L, Term::L), Term::L)),
        8_000_000,
    ));
    scenarios.push((
        "tree-s-rule".into(),
        "@(F(S(L),S(L)),L): the S rule duplicates its argument through Dn, the deepest chained reduction in the suite, in tree form".into(),
        tree_runner(&ap(f2(s(Term::L), s(Term::L)), Term::L)),
        8_000_000,
    ));
    scenarios.push((
        "tree-tower".into(),
        "@(F(S(L),S(L)),F(L,L)): the S rule applied to a fork — sharing meets dispatch on one tree".into(),
        tree_runner(&ap(f2(s(Term::L), s(Term::L)), f2(Term::L, Term::L))),
        8_000_000,
    ));

    let mut out = String::from(
        "// generated by dump-cascade (rust-ca-lattice); cascade traces in schema v4\n\
         window.TRACES = Object.assign(window.TRACES || {}, {\n",
    );
    for (i, (name, note, runner, budget)) in scenarios.into_iter().enumerate() {
        if i != 0 {
            out.push_str(",\n");
        }
        let mut trace = String::new();
        trace_run(&name, &note, runner, budget, &mut trace);
        out.push_str(&format!("\"{}\":", name));
        out.push_str(&trace);
        eprintln!("traced {name}");
    }
    out.push_str("\n});\n");
    std::fs::write(&path, out).expect("write bundle");
    eprintln!("wrote {path}");
}
