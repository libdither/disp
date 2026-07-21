//! Generate the Cell64 suite bundle replayed by `research/interaction-combinator/
//! lattice_player.html`, or just print the pass/fail summary (a fast suite runner).
//!
//! Usage: `dump-suite [seed] [suite.js]` — with no `suite.js` the bundle is not written.

use rust_ca_lattice::lattice::step;
use rust_ca_lattice::rewrite64::rewrite_orientation;
use rust_ca_lattice::suite::{self, Outcome, Scenario};
use rust_ca_lattice::tracejs::{frame, pos};
use std::fmt::Write as _;

const SWEEP_CAP: u64 = 400;

fn add(origin: (i32, i32, i32), relative: (i32, i32, i32)) -> (i32, i32, i32) {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

fn emit_scenario(sc: &Scenario, seed: u64, bundle: &mut String) -> Option<Outcome> {
    let mut out = String::new();
    write!(
        out,
        "{{\"schema_version\":4,\"engine_revision\":\"cell64-suite-1\",\"kind\":\"cell64-suite\",\"name\":\"{}\",\"topo\":\"{}\",\"suite_tier\":\"{}\",\"note\":\"{}\",\"term\":\"{}\",\"oracle\":\"{}\",\"bit_layout\":{{\"name\":\"cell64\",\"payload\":{{\"fixed\":20}},\"fields\":{{\"chi\":8,\"sigma\":8}},\"motion\":{{\"fixed\":28}},\"observer\":{{\"sid\":32}},\"max_local\":64}},\"expect\":{{\"transport_min\":{},\"rewrites\":{},\"ints\":{},\"controls_clear\":{},\"matter_unchanged\":{},\"chi_seen\":{},\"frontier_min\":{}}},\"frames\":[",
        sc.key,
        sc.topo.name(),
        sc.tier.key(),
        sc.note,
        sc.term,
        sc.oracle,
        sc.expect.transport_min,
        sc.expect.rewrites,
        sc.expect.ints,
        sc.expect.controls_clear,
        sc.expect.matter_unchanged,
        sc.expect.chi_seen,
        sc.expect.frontier_min,
    ).unwrap();

    let mut placement_tick = None;
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let (mut grid, mut shadow) = suite::build(sc);
        frame(&mut out, &grid, &shadow, 0, "", None);
        let outcome = suite::run(sc, &mut grid, &mut shadow, seed, SWEEP_CAP, &mut |grid, shadow, a| {
            let event = if let Some(fire) = a.fire {
                placement_tick = Some(a.tick);
                let (side, lift) = rewrite_orientation(fire.axis, fire.side, fire.lift).unwrap();
                let workshop = rust_ca_lattice::compile64::workshop_for(
                    fire.rule.get(), fire.axis, side, lift,
                ).unwrap();
                let rule = &rust_ca_lattice::rules::RULES[fire.rule.get() as usize];
                let rule_name = format!("{}·{}", rule.consumer.name(), rule.producer.name());
                let mut fresh = workshop.slots.iter().filter_map(|slot| {
                    slot.fresh.map(|index| (index, add(a.at, slot.at)))
                }).collect::<Vec<_>>();
                fresh.sort_by_key(|(index, _)| *index);
                let fresh = fresh.into_iter().map(|(_, at)| pos(at)).collect::<Vec<_>>().join(",");
                format!(
                    "{{\"t\":\"fire\",\"rule\":\"{}\",\"c\":{},\"p\":{},\"fresh\":[{}]}},{{\"t\":\"activate\",\"at\":{},\"changed\":true,\"round\":{},\"index\":{},\"sweep\":{}}}",
                    rule_name, pos(a.at), pos(step(a.at, fire.axis)), fresh, pos(a.at), a.round, a.index, a.sweep,
                )
            } else {
                format!(
                    "{{\"t\":\"activate\",\"at\":{},\"changed\":{},\"before\":\"{:016x}\",\"after\":\"{:016x}\",\"round\":{},\"index\":{},\"sweep\":{}}}",
                    pos(a.at), a.before != a.after, a.before.0, a.after.0, a.round, a.index, a.sweep,
                )
            };
            out.push(',');
            frame(&mut out, grid, shadow, a.tick, &event, Some(&a.changed));
        });
        outcome
    }));

    let outcome = match result {
        Ok(outcome) => outcome,
        Err(panic) => {
            let reason = panic.downcast::<String>().map(|s| *s)
                .or_else(|panic| panic.downcast::<&str>().map(|s| s.to_string()))
                .unwrap_or_else(|_| "unknown panic".to_string());
            eprintln!("suite scenario {} panicked: {}", sc.key, reason);
            return None;
        }
    };

    write!(
        out,
        "],\"status\":\"{}\",\"readback\":\"{}\",\"ticks\":{},\"nf_tick\":{},\"settled\":{},\"capture_stride\":1,\"activation_seed\":{},\"result\":{{\"pass\":{},\"fail\":{},\"ticks\":{},\"sweeps\":{},\"transport\":{},\"rewrites\":{},\"ints\":{},\"frontier\":{}}}}}",
        if outcome.pass { "done" } else { "cap" },
        sc.oracle,
        outcome.ticks,
        placement_tick.map_or("null".into(), |value| value.to_string()),
        outcome.pass,
        seed,
        outcome.pass,
        outcome.fail.as_deref().map_or("null".into(), |f| format!("\"{f}\"")),
        outcome.ticks,
        outcome.sweeps,
        outcome.transport,
        outcome.rewrites,
        outcome.ints,
        outcome.frontier,
    ).unwrap();
    write!(bundle, "window.TRACES[\"{}\"] = {};\n", sc.key, out).unwrap();
    Some(outcome)
}

fn main() {
    let seed = std::env::args().nth(1).and_then(|arg| arg.parse().ok()).unwrap_or(0);
    let path = std::env::args().nth(2);
    let mut bundle = String::from(
        "// generated by dump-suite (rust-ca-lattice); schema v4 Cell64 suite\nwindow.TRACES = {};\n",
    );
    let mut all_pass = true;
    for sc in suite::scenarios() {
        match emit_scenario(&sc, seed, &mut bundle) {
            Some(outcome) => {
                println!(
                    "{} {:<18} {:<7} ticks {:<6} sweeps {:<4} transport {:<3} rewrites {} ints {} frontier {} — {}",
                    if outcome.pass { "✓" } else { "✗" },
                    sc.key,
                    sc.tier.key(),
                    outcome.ticks,
                    outcome.sweeps,
                    outcome.transport,
                    outcome.rewrites,
                    outcome.ints,
                    outcome.frontier,
                    outcome.fail.as_deref().unwrap_or("ok"),
                );
                all_pass &= outcome.pass;
            }
            None => {
                println!("✗ {:<18} panicked (see stderr)", sc.key);
                all_pass = false;
            }
        }
    }
    if let Some(path) = path {
        std::fs::write(&path, bundle).expect("write suite bundle");
        println!("wrote {path}");
    }
    if !all_pass { std::process::exit(1); }
}
