//! Emit an activation-by-activation trace of the packed local substrate.
//!
//! Usage: `dump-packed [bilayer|full3d] [seed] [activation-cap] [trace.js] [preferred|fallback|blocked]`

use rust_ca_lattice::cell64::Dir;
use rust_ca_lattice::lattice::{step, Topo};
use rust_ca_lattice::packed_local::{activation_order, activate_with_shadow};
use rust_ca_lattice::rewrite64::{
    apply_fork_fixture64, apply_fork_workshop, rewrite_orientation,
};
use rust_ca_lattice::suite::add_lift_blocker;
use rust_ca_lattice::tracejs::{frame, pos};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Scenario { Preferred, Fallback, Blocked }

impl Scenario {
    fn parse(value: Option<String>) -> Self {
        match value.as_deref() {
            None | Some("preferred") => Self::Preferred,
            Some("fallback") => Self::Fallback,
            Some("blocked") => Self::Blocked,
            Some(other) => panic!("unknown packed trace scenario {other:?}"),
        }
    }

    fn key(self) -> &'static str {
        match self {
            Self::Preferred => "packed-apply-fork",
            Self::Fallback => "packed-apply-fork-fallback",
            Self::Blocked => "packed-apply-fork-blocked",
        }
    }

    fn note(self) -> &'static str {
        match self {
            Self::Preferred => {
                "A·F requests the preferred U lift, then places its 16-cell result by one-cell live-read activations. Both auxiliary ports share one cable and split only at zipper cells."
            }
            Self::Fallback => {
                "An inert reciprocal blocker declines the preferred U lift. The request clears without changing matter, retries the D lift, and places the same 16-cell result."
            }
            Self::Blocked => {
                "Inert reciprocal blockers decline both U and D lifts. Both request trees clear and the A·F pair, its cables, and every blocker remain unchanged."
            }
        }
    }

    fn outcome(self) -> &'static str {
        if self == Self::Blocked { "blocked" } else { "placed" }
    }
}

fn main() {
    let topo = match std::env::args().nth(1).as_deref() {
        Some("full3d") => Topo::Full3D,
        _ => Topo::Bilayer,
    };
    let seed = std::env::args().nth(2).and_then(|arg| arg.parse().ok()).unwrap_or(0);
    let cap = std::env::args().nth(3).and_then(|arg| arg.parse().ok()).unwrap_or(10_000u64);
    let scenario = Scenario::parse(std::env::args().nth(5));
    assert!(
        scenario == Scenario::Preferred || topo == Topo::Full3D,
        "fallback and blocked traces require full3d so both lift directions exist",
    );
    let (mut grid, mut shadow, driver) = apply_fork_fixture64(topo);
    if matches!(scenario, Scenario::Fallback | Scenario::Blocked) {
        add_lift_blocker(&mut grid, &mut shadow, driver, Dir::U, Dir::D);
    }
    if scenario == Scenario::Blocked {
        add_lift_blocker(&mut grid, &mut shadow, driver, Dir::D, Dir::U);
    }
    grid.check_projection(&shadow);
    let mut out = String::new();
    write!(
        out,
        "{{\"schema_version\":4,\"engine_revision\":\"cell64-zipper-2\",\"kind\":\"packed-workshop\",\"name\":\"{}\",\"topo\":\"{}\",\"rewrite_outcome\":\"{}\",\"note\":\"{}\",\"term\":\"A.p ↔ F.p\",\"oracle\":\"{}\",\"bit_layout\":{{\"name\":\"cell64\",\"payload\":{{\"fixed\":20}},\"fields\":{{\"chi\":8,\"sigma\":8}},\"motion\":{{\"fixed\":28}},\"observer\":{{\"sid\":32}},\"max_local\":64}},\"frames\":[",
        scenario.key(),
        topo.name(),
        scenario.outcome(),
        scenario.note(),
        if scenario == Scenario::Blocked { "unchanged active pair" } else { "T1 + Pair" },
    ).unwrap();
    frame(&mut out, &grid, &shadow, 0, "", None);

    let mut tick = 0u64;
    let mut placement_tick = None;
    let mut done = false;
    let mut saw_protocol = false;
    'rounds: for round in 0..1_000u64 {
        for at in activation_order(&grid, seed, round) {
            if tick >= cap { break 'rounds; }
            tick += 1;
            let before = grid.word(at);
            let before_observers: BTreeMap<_, _> = grid.observer_sid.clone();
            let result = activate_with_shadow(&mut grid, &mut shadow, at);
            saw_protocol |= grid.has_protocol();
            let changed = result.is_some();
            let fire = result.and_then(|(_, _, effect)| effect.rewrite_fire);
            if fire.is_some() { placement_tick = Some(tick); }
            let event = if let Some(fire) = fire {
                let (side, lift) = rewrite_orientation(fire.axis, fire.side, fire.lift).unwrap();
                let workshop = apply_fork_workshop(fire.axis, side, lift, false, false).unwrap();
                let mut fresh = workshop.slots.iter().filter_map(|slot| {
                    slot.fresh.map(|index| (index, add(at, slot.at)))
                }).collect::<Vec<_>>();
                fresh.sort_by_key(|(index, _)| *index);
                let fresh = fresh.into_iter().map(|(_, at)| pos(at)).collect::<Vec<_>>().join(",");
                format!(
                    "{{\"t\":\"fire\",\"rule\":\"A·F\",\"c\":{},\"p\":{},\"fresh\":[{}]}},{{\"t\":\"activate\",\"at\":{},\"changed\":true}}",
                    pos(at),
                    pos(step(at, fire.axis)),
                    fresh,
                    pos(at),
                )
            } else {
                format!("{{\"t\":\"activate\",\"at\":{},\"changed\":{},\"before\":\"{:016x}\",\"after\":\"{:016x}\"}}", pos(at), changed, before.0, grid.word(at).0)
            };
            let mut changed_cells = BTreeSet::new();
            if before != grid.word(at) { changed_cells.insert(at); }
            for p in before_observers.keys().chain(grid.observer_sid.keys()) {
                if before_observers.get(p) != grid.observer_sid.get(p) { changed_cells.insert(*p); }
            }
            let changed_cells = changed_cells.into_iter().collect::<Vec<_>>();
            out.push(',');
            frame(&mut out, &grid, &shadow, tick, &event, Some(&changed_cells));
            let complete = if scenario == Scenario::Blocked {
                saw_protocol && grid.rewrites == 0 && !grid.has_protocol()
            } else {
                grid.rewrites == 1 && !grid.has_protocol()
            };
            if complete {
                done = true;
                break 'rounds;
            }
        }
    }
    if done { grid.check_projection(&shadow); }
    write!(
        out,
        "],\"status\":\"{}\",\"readback\":\"{}\",\"ticks\":{},\"nf_tick\":{},\"settled\":{},\"settle_ticks\":0,\"capture_stride\":1,\"activation_seed\":{}}}",
        if done { "done" } else { "cap" },
        if done {
            if scenario == Scenario::Blocked { "unchanged active pair" } else { "T1 + Pair" }
        } else {
            "incomplete"
        },
        tick,
        placement_tick.map_or("null".into(), |value| value.to_string()),
        done,
        seed,
    ).unwrap();
    if let Some(path) = std::env::args().nth(4) {
        let script = format!("window.TRACES[\"{}\"] = {out};\n", scenario.key());
        std::fs::write(path, script).expect("write packed trace bundle");
    } else {
        println!("{out}");
    }
}

fn add(origin: (i32, i32, i32), relative: (i32, i32, i32)) -> (i32, i32, i32) {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}
