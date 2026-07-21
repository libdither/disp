//! JSON serialization for packed Cell64 activation traces (schema 4), shared by the trace
//! generators (`dump-packed`, `dump-suite`). The format is replayed by
//! `research/interaction-combinator/lattice_player.html`; the viewer's inflation code
//! (`inflatePackedTrace`) is the authoritative reader.

use crate::cell64::{Control, LaneCount, LaneMask, Matter, Phase, RewritePhase};
use crate::lattice::Pos;
use crate::net::Net;
use crate::rewrite64::rewrite_orientation;
use crate::substrate::Grid64;
use std::fmt::Write as _;

pub fn pos(p: Pos) -> String { format!("[{},{},{}]", p.0, p.1, p.2) }

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

pub fn control_name(control: Control) -> Option<(String, String)> {
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

pub fn snapshot(grid: &Grid64, out: &mut String) {
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
        let (ends, twist, hot) = match cell.matter {
            Matter::Link { ends, lanes: LaneCount::Two, twist, hot, .. } => (ends, twist, hot),
            Matter::GuestLink { ends, twist, .. } => (ends, twist, LaneMask::new(0).unwrap()),
            _ => continue,
        };
        if !first { out.push(','); }
        first = false;
        write!(
            out,
            "[{},{},{},\"{}{}\",{},{}]",
            p.0, p.1, p.2, ends.a.ch(), ends.b.ch(), twist, hot.get(),
        ).unwrap();
    }

    // Additive guest markers: [x, y, z, tag, plane, principal face] for every rider cell.
    out.push_str("],\"guests\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let (tag, plane, principal) = match cell.matter {
            Matter::GuestZip { tag, plane, trunk, .. } => (tag, plane, trunk),
            Matter::GuestLink { tag, plane, principal, .. } => (tag, plane, principal),
            _ => continue,
        };
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{}\",{},\"{}\"]", p.0, p.1, p.2, tag.name(), plane, principal.ch()).unwrap();
    }

    out.push_str("],\"zips\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        let (trunk, branches, twist, hot) = match cell.matter {
            Matter::Zip { trunk, branches, twist, hot, .. } => (trunk, branches, twist, hot),
            // A guest serializes as its underlying matter entry; the marker is separate.
            Matter::GuestZip { trunk, branches, twist, .. } => {
                (trunk, branches, twist, LaneMask::new(0).unwrap())
            }
            _ => continue,
        };
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
        if cell.chi == 0 { continue };
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, cell.chi).unwrap();
    }

    out.push_str("],\"sigma\":[");
    first = true;
    for (&p, word) in &grid.cells {
        let cell = word.unpack().unwrap();
        if cell.sigma == 0 { continue };
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

pub fn delta_cell(grid: &Grid64, p: Pos, out: &mut String) {
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
        // A guest serializes as the underlying matter entry plus an additive guest marker
        // (tag name, ridden lane, principal face); the player consumes the marker separately.
        Matter::GuestZip { tag, plane, trunk, branches, twist, .. } => {
            write!(
                out,
                ",\"zip\":[\"{}\",\"{}\",\"{}\",{},0],\"guest\":[\"{}\",{},\"{}\"]",
                trunk.ch(), branches[0].ch(), branches[1].ch(), twist, tag.name(), plane, trunk.ch(),
            ).unwrap();
        }
        Matter::GuestLink { tag, principal, plane, ends, twist, .. } => {
            write!(
                out,
                ",\"cable\":[\"{}{}\",{},0],\"guest\":[\"{}\",{},\"{}\"]",
                ends.a.ch(), ends.b.ch(), twist, tag.name(), plane, principal.ch(),
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

/// One trace frame: a full keyframe grid (`delta: None`) or a one-activation delta frame.
pub fn frame(
    out: &mut String,
    grid: &Grid64,
    shadow: &Net,
    tick: u64,
    event: &str,
    delta: Option<&[Pos]>,
) {
    let length: usize = grid.cells.values().map(|word| match word.unpack().unwrap().matter {
        Matter::Link { lanes: LaneCount::One, .. } => 1,
        Matter::Link { lanes: LaneCount::Two, .. }
        | Matter::Zip { .. }
        | Matter::Cross { .. }
        | Matter::GuestZip { .. }
        | Matter::GuestLink { .. } => 2,
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
