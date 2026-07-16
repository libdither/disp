//! Emit an activation-by-activation trace of the packed local substrate.
//!
//! Usage: `dump-packed [bilayer|full3d] [seed] [activation-cap] [trace.js]`

use rust_ca_lattice::cell64::{Control, LaneCount, Matter, Phase};
use rust_ca_lattice::lattice::{Pos, Topo};
use rust_ca_lattice::packed_local::{activation_order, activate_with_shadow};
use rust_ca_lattice::rewrite64::apply_fork_fixture64;
use rust_ca_lattice::substrate::Grid64;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

fn pos(p: Pos) -> String { format!("[{},{},{}]", p.0, p.1, p.2) }

fn phase_name(phase: Phase) -> &'static str {
    match phase {
        Phase::Offer => "offer",
        Phase::Ack => "ack",
        Phase::Commit => "commit",
        Phase::Done => "done",
        Phase::Abort => "abort",
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
        Control::Rewrite { role, phase, slot, .. } => Some((
            format!("rewrite-{role:?}").to_lowercase(),
            format!("{} · slot {}", phase_name(phase), slot.get()),
        )),
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
    let (mut grid, mut shadow, _) = apply_fork_fixture64(topo);
    let mut out = String::new();
    write!(
        out,
        "{{\"schema_version\":4,\"engine_revision\":\"cell64-zipper-1\",\"kind\":\"packed-workshop\",\"name\":\"packed-apply-fork\",\"topo\":\"{}\",\"note\":\"A·F claimed and committed by one-cell live-read activations; both auxiliary ports share one cable and split only at zipper cells.\",\"term\":\"A.p ↔ F.p\",\"oracle\":\"T1 + Pair\",\"bit_layout\":{{\"name\":\"cell64\",\"payload\":{{\"fixed\":20}},\"fields\":{{\"chi\":8,\"sigma\":8}},\"motion\":{{\"fixed\":28}},\"observer\":{{\"sid\":32}},\"max_local\":64}},\"frames\":[",
        topo.name(),
    ).unwrap();
    frame(&mut out, &grid, &shadow, 0, "", None);

    let mut tick = 0u64;
    let mut commit_tick = None;
    let mut done = false;
    'rounds: for round in 0..1_000u64 {
        for at in activation_order(&grid, seed, round) {
            if tick >= cap { break 'rounds; }
            tick += 1;
            let before = grid.word(at);
            let before_observers: BTreeMap<Pos, u32> = grid.observer_sid.clone();
            let result = activate_with_shadow(&mut grid, &mut shadow, at);
            let changed = result.is_some();
            let fired = result.is_some_and(|(_, _, effect)| effect.rewrite_fire.is_some());
            if fired { commit_tick = Some(tick); }
            let event = if fired {
                format!("{{\"t\":\"fire\",\"rule\":\"A·F\",\"c\":{},\"p\":[1,0,0],\"fresh\":[]}},{{\"t\":\"activate\",\"at\":{},\"changed\":true}}", pos((0, 0, 0)), pos(at))
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
            if grid.rewrites == 1 && !grid.has_protocol() {
                done = true;
                break 'rounds;
            }
        }
    }
    write!(
        out,
        "],\"status\":\"{}\",\"readback\":\"{}\",\"ticks\":{},\"nf_tick\":{},\"settled\":{},\"settle_ticks\":0,\"capture_stride\":1,\"activation_seed\":{}}}",
        if done { "done" } else { "cap" },
        if done { "T1 + Pair" } else { "incomplete" },
        tick,
        commit_tick.map_or("null".into(), |value| value.to_string()),
        done,
        seed,
    ).unwrap();
    if let Some(path) = std::env::args().nth(4) {
        let script = format!("window.TRACES[\"packed-apply-fork\"] = {out};\n");
        std::fs::write(path, script).expect("write packed trace bundle");
    } else {
        println!("{out}");
    }
}
