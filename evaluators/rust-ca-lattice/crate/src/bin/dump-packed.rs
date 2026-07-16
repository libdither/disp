//! Emit an activation-by-activation trace of the packed local substrate.
//!
//! Usage: `dump-packed [bilayer|full3d] [seed] [activation-cap] [trace.js] [preferred|fallback|blocked]`

use rust_ca_lattice::cell64::{
    Control, DecodedCell, Dir, LaneCount, Matter, Phase, RewritePhase, DIRS,
};
use rust_ca_lattice::lattice::{step, Pos, Topo};
use rust_ca_lattice::packed_local::{activation_order, activate_with_shadow};
use rust_ca_lattice::rewrite64::{
    apply_fork_fixture64, apply_fork_workshop, rewrite_orientation,
};
use rust_ca_lattice::rules::Tag;
use rust_ca_lattice::substrate::Grid64;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

fn pos(p: Pos) -> String { format!("[{},{},{}]", p.0, p.1, p.2) }

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

fn add(origin: Pos, relative: Pos) -> Pos {
    (origin.0 + relative.0, origin.1 + relative.1, origin.2 + relative.2)
}

fn add_lift_blocker(
    grid: &mut Grid64,
    shadow: &mut rust_ca_lattice::net::Net,
    driver: Pos,
    lift: Dir,
    opposite_lift: Dir,
) {
    let target = apply_fork_workshop(Dir::E, Dir::N, lift, false, false).unwrap();
    let other = apply_fork_workshop(Dir::E, Dir::N, opposite_lift, false, false).unwrap();
    let target_positions = target.slots.iter().map(|slot| add(driver, slot.at))
        .collect::<BTreeSet<_>>();
    let other_positions = other.slots.iter().map(|slot| add(driver, slot.at))
        .collect::<BTreeSet<_>>();
    let blocked = *target_positions.difference(&other_positions)
        .find(|at| grid.is_empty(**at))
        .expect("lift needs a private request cell");
    let forbidden = target_positions.union(&other_positions).copied().collect::<BTreeSet<_>>();
    let (face, buddy) = DIRS.into_iter().find_map(|face| {
        let buddy = step(blocked, face);
        (grid.is_empty(buddy) && !forbidden.contains(&buddy)).then_some((face, buddy))
    }).expect("lift blocker needs one adjacent private cell");

    let blocker_sid = shadow.mk(Tag::Out);
    let buddy_sid = shadow.mk(Tag::Out);
    shadow.link(blocker_sid, 0, buddy_sid, 0);
    grid.set(blocked, DecodedCell {
        matter: Matter::Agent {
            tag: Tag::Out,
            principal: face,
            tail: None,
            aux_flip: false,
        },
        ..DecodedCell::default()
    });
    grid.set(buddy, DecodedCell {
        matter: Matter::Agent {
            tag: Tag::Out,
            principal: face.opp(),
            tail: None,
            aux_flip: false,
        },
        ..DecodedCell::default()
    });
    grid.observer_sid.insert(blocked, blocker_sid);
    grid.observer_sid.insert(buddy, buddy_sid);
}

fn phase_name(phase: Phase) -> &'static str {
    match phase {
        Phase::Offer => "offer",
        Phase::Ack => "ack",
        Phase::Commit => "commit",
        Phase::Done => "done",
        Phase::Abort => "abort",
    }
}

fn rewrite_phase_name(phase: RewritePhase) -> &'static str {
    match phase {
        RewritePhase::Request => "request",
        RewritePhase::Ready => "ready",
        RewritePhase::Blocked => "blocked",
        RewritePhase::Place => "place",
        RewritePhase::Placed => "placed",
    }
}

fn control_name(control: Control) -> Option<(String, String)> {
    match control {
        Control::Idle => None,
        Control::Translate { role, phase, .. } => {
            Some((format!("translate-{role:?}").to_lowercase(), phase_name(phase).into()))
        }
        Control::CableShift { role, phase, .. } => {
            Some((format!("cable-{role:?}").to_lowercase(), phase_name(phase).into()))
        }
        Control::PressureRelief { role, phase, .. } => {
            Some((format!("pressure-{role:?}").to_lowercase(), phase_name(phase).into()))
        }
        Control::Rewrite {
            role, phase, axis, side, lift, slot, fallback, ..
        } => {
            let direction = rewrite_orientation(axis, side, lift)
                .map(|(_, lift)| lift.ch())
                .unwrap_or('?');
            Some((
                format!("rewrite-{role:?}").to_lowercase(),
                format!(
                    "{} · {} {} · slot {}",
                    rewrite_phase_name(phase),
                    if fallback { "fallback" } else { "preferred" },
                    direction,
                    slot.get(),
                ),
            ))
        }
        Control::Contest { stable, .. } => Some(("contest".into(), format!("hold {}", stable.get()))),
    }
}

fn snapshot(grid: &Grid64, out: &mut String) {
    out.push_str("{\"agents\":[");
    let mut first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Matter::Agent { tag, principal, tail, .. } = cell.matter else { continue };
        if !first { out.push(','); }
        first = false;
        let sid = grid.observer_sid.get(&p).copied().unwrap_or(u32::MAX);
        let mut faces = vec![principal];
        if let Some(tail) = tail {
            faces.push(tail);
            if tag.arity() == 3 { faces.push(tail); }
        }
        let faces = faces.iter().map(|face| format!("\"{}\"", face.ch()))
            .collect::<Vec<_>>().join(",");
        write!(out, "[{},{},{},\"{}\",{},[{}]]", p.0, p.1, p.2, tag.name(), sid, faces).unwrap();
    }

    out.push_str("],\"wires\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Matter::Link { ends, lanes: LaneCount::One, hot, .. } = cell.matter else { continue };
        if !first { out.push(','); }
        first = false;
        write!(
            out,
            "[{},{},{},[\"{}{}{}\"]]",
            p.0, p.1, p.2, ends.a.ch(), ends.b.ch(), if hot.get() != 0 { "*" } else { "" },
        ).unwrap();
    }

    out.push_str("],\"cables\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Matter::Link { ends, lanes: LaneCount::Two, twist, hot, .. } = cell.matter else { continue };
        if !first { out.push(','); }
        first = false;
        write!(
            out,
            "[{},{},{},\"{}{}\",{},{}]",
            p.0, p.1, p.2, ends.a.ch(), ends.b.ch(), twist, hot.get(),
        ).unwrap();
    }

    out.push_str("],\"zips\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Matter::Zip { trunk, branches, twist, hot, .. } = cell.matter else { continue };
        if !first { out.push(','); }
        first = false;
        write!(
            out,
            "[{},{},{},\"{}\",\"{}\",\"{}\",{},{}]",
            p.0, p.1, p.2, trunk.ch(), branches[0].ch(), branches[1].ch(), twist, hot.get(),
        ).unwrap();
    }

    out.push_str("],\"crosses\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Matter::Cross { routes, hot, .. } = cell.matter else { continue };
        if !first { out.push(','); }
        first = false;
        write!(
            out,
            "[{},{},{},[\"{}{}\",\"{}{}\"],{}]",
            p.0, p.1, p.2,
            routes[0].a.ch(), routes[0].b.ch(), routes[1].a.ch(), routes[1].b.ch(), hot.get(),
        ).unwrap();
    }

    out.push_str("],\"chi\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        if cell.chi == 0 { continue; }
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, cell.chi).unwrap();
    }

    out.push_str("],\"sigma\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        if cell.sigma == 0 { continue; }
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, cell.sigma).unwrap();
    }

    out.push_str("],\"motion\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let Some((role, phase)) = control_name(cell.control) else { continue };
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{}\",\"{}\"]", p.0, p.1, p.2, role, phase).unwrap();
    }

    out.push_str("],\"words\":[");
    first = true;
    for (&p, word) in &grid.cells {
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{:016x}\"]", p.0, p.1, p.2, word.0).unwrap();
    }
    out.push_str("]}");
}

fn delta_cell(grid: &Grid64, p: Pos, out: &mut String) {
    let word = grid.word(p);
    let cell = word.unpack().unwrap();
    let sid = grid.observer_sid.get(&p).copied();
    if word.0 == 0 && sid.is_none() {
        write!(out, "[{},{},{},null]", p.0, p.1, p.2).unwrap();
        return;
    }
    write!(out, "[{},{},{},{{\"word\":\"{:016x}\"", p.0, p.1, p.2, word.0).unwrap();
    if let Some(sid) = sid { write!(out, ",\"sid\":{}", sid).unwrap(); }
    match cell.matter {
        Matter::Empty => {}
        Matter::Agent { tag, principal, tail, .. } => {
            let mut faces = vec![principal];
            if let Some(tail) = tail {
                faces.push(tail);
                if tag.arity() == 3 { faces.push(tail); }
            }
            let faces = faces.iter().map(|face| format!("\"{}\"", face.ch()))
                .collect::<Vec<_>>().join(",");
            write!(out, ",\"agent\":[\"{}\",[{}]]", tag.name(), faces).unwrap();
        }
        Matter::Link { ends, lanes: LaneCount::One, hot, .. } => {
            write!(
                out,
                ",\"wire\":[\"{}{}{}\"]",
                ends.a.ch(), ends.b.ch(), if hot.get() != 0 { "*" } else { "" },
            ).unwrap();
        }
        Matter::Link { ends, lanes: LaneCount::Two, twist, hot, .. } => {
            write!(out, ",\"cable\":[\"{}{}\",{},{}]", ends.a.ch(), ends.b.ch(), twist, hot.get()).unwrap();
        }
        Matter::Zip { trunk, branches, twist, hot, .. } => {
            write!(
                out,
                ",\"zip\":[\"{}\",\"{}\",\"{}\",{},{}]",
                trunk.ch(), branches[0].ch(), branches[1].ch(), twist, hot.get(),
            ).unwrap();
        }
        Matter::Cross { routes, hot, .. } => {
            write!(
                out,
                ",\"cross\":[[\"{}{}\",\"{}{}\"],{}]",
                routes[0].a.ch(), routes[0].b.ch(), routes[1].a.ch(), routes[1].b.ch(), hot.get(),
            ).unwrap();
        }
    }
    if cell.chi != 0 { write!(out, ",\"chi\":{}", cell.chi).unwrap(); }
    if cell.sigma != 0 { write!(out, ",\"sigma\":{}", cell.sigma).unwrap(); }
    if let Some((role, phase)) = control_name(cell.control) {
        write!(out, ",\"motion\":[\"{}\",\"{}\"]", role, phase).unwrap();
    }
    out.push_str("}]");
}

fn frame(
    out: &mut String,
    grid: &Grid64,
    shadow: &rust_ca_lattice::net::Net,
    tick: u64,
    event: &str,
    delta: Option<&[Pos]>,
) {
    let length: usize = grid.cells.values().map(|word| match word.unpack().unwrap().matter {
        Matter::Link { lanes: LaneCount::One, .. } => 1,
        Matter::Link { lanes: LaneCount::Two, .. } | Matter::Zip { .. } | Matter::Cross { .. } => 2,
        _ => 0,
    }).sum();
    write!(
        out,
        "{{\"tick\":{},\"events\":[{}],\"ints\":{},\"transport\":{},\"pairs\":{},\"wire\":{{\"length\":{},\"chord\":null,\"slack\":null}}",
        tick,
        event,
        shadow.ints,
        grid.transport,
        shadow.all_active_pairs().len(),
        length,
    ).unwrap();
    if let Some(delta) = delta {
        out.push_str(",\"delta\":[");
        for (index, p) in delta.iter().enumerate() {
            if index != 0 { out.push(','); }
            delta_cell(grid, *p, out);
        }
        out.push(']');
    } else {
        out.push_str(",\"grid\":");
        snapshot(grid, out);
    }
    out.push('}');
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
            let before_observers: BTreeMap<Pos, u32> = grid.observer_sid.clone();
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
