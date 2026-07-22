//! Schema-4 trace serialization for cascade grids, shaped so the existing
//! `lattice_player.html` renders them unchanged: routes appear as wire strands (hot ones
//! starred gold), agents with their port faces, and seeds plus builder cursors as control
//! outlines. Loaded additively from `lattice_cascade.js` next to the Cell64 bundle.

use crate::cascade::{Cell, Grid2, Word2};
use crate::cascade_run::{Event, Runner};
use crate::lattice::Pos;
use std::fmt::Write as _;

fn strand(route: &crate::cascade::Route, hot: bool) -> String {
    format!("{}{}{}", route.a.face.ch(), route.b.face.ch(), if hot { "*" } else { "" })
}

fn agent_faces(tag: crate::rules::Tag, principal: &crate::cascade::EndPt, aux: &[crate::cascade::EndPt; 2]) -> String {
    let mut faces = vec![principal.face];
    for k in 0..tag.arity().saturating_sub(1) {
        faces.push(aux[k].face);
    }
    faces.iter().map(|f| format!("\"{}\"", f.ch())).collect::<Vec<_>>().join(",")
}

fn motion_of(site: &crate::cascade::Site) -> Option<(String, String)> {
    let seed = match &site.cell {
        Cell::Seed { rule, half, .. } => Some(format!(
            "r{} {}",
            rule,
            match half { crate::cascade::Half::Consumer => "consumer", crate::cascade::Half::Producer => "producer" },
        )),
        _ => None,
    };
    match (&site.cursor, seed) {
        (Some(c), Some(s)) => Some(("seed+cursor".into(), format!("{s} pc{}", c.pc))),
        (Some(c), None) => Some(("cursor".into(), format!("pc{}{}", c.pc, if c.reverse { " rev" } else { "" }))),
        (None, Some(s)) => Some(("seed".into(), s)),
        (None, None) => None,
    }
}

pub fn snapshot(grid: &Grid2, out: &mut String) {
    let decoded: Vec<(Pos, crate::cascade::Site)> = grid
        .cells
        .iter()
        .map(|(p, w)| (*p, w.unpack().expect("canonical")))
        .collect();

    out.push_str("{\"agents\":[");
    let mut first = true;
    for (p, site) in &decoded {
        let Cell::Agent { tag, principal, aux, .. } = &site.cell else { continue };
        if !first { out.push(','); }
        first = false;
        let sid = grid.sid.get(p).copied().unwrap_or(u32::MAX);
        write!(out, "[{},{},{},\"{}\",{},[{}]]", p.0, p.1, p.2, tag.name(), sid,
            agent_faces(*tag, principal, aux)).unwrap();
    }

    out.push_str("],\"wires\":[");
    first = true;
    for (p, site) in &decoded {
        let Cell::Wire { routes, hot, .. } = &site.cell else { continue };
        if !first { out.push(','); }
        first = false;
        let strands = routes.iter().enumerate()
            .map(|(i, r)| format!("\"{}\"", strand(r, (hot >> i) & 1 == 1)))
            .collect::<Vec<_>>().join(",");
        write!(out, "[{},{},{},[{}]]", p.0, p.1, p.2, strands).unwrap();
    }

    // Passthrough routes on agents and pending seed passthroughs render as extra strands.
    for (p, site) in &decoded {
        let pass: Vec<crate::cascade::Route> = match &site.cell {
            Cell::Agent { pass, .. } => pass.clone(),
            Cell::Seed { pass, .. } => pass.iter().copied().collect(),
            _ => continue,
        };
        if pass.is_empty() { continue; }
        if !first { out.push(','); }
        first = false;
        let strands = pass.iter().map(|r| format!("\"{}\"", strand(r, false)))
            .collect::<Vec<_>>().join(",");
        write!(out, "[{},{},{},[{}]]", p.0, p.1, p.2, strands).unwrap();
    }

    out.push_str("],\"cables\":[],\"guests\":[");
    // Nursery agents get the dashed guest ring so growth is visible.
    first = true;
    for (p, site) in &decoded {
        let Cell::Agent { tag, principal, nursery: true, .. } = &site.cell else { continue };
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{}\",0,\"{}\"]", p.0, p.1, p.2, tag.name(), principal.face.ch()).unwrap();
    }

    out.push_str("],\"zips\":[],\"crosses\":[],\"chi\":[");
    first = true;
    for (p, site) in &decoded {
        if site.chi == 0 { continue; }
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, site.chi as u32 * 16).unwrap();
    }

    out.push_str("],\"sigma\":[],\"motion\":[");
    first = true;
    for (p, site) in &decoded {
        let Some((role, phase)) = motion_of(site) else { continue };
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{}\",\"{}\"]", p.0, p.1, p.2, role, phase).unwrap();
    }

    out.push_str("],\"words\":[");
    first = true;
    for (p, _) in &decoded {
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},\"{:016x}\"]", p.0, p.1, p.2, grid.cells[p].0).unwrap();
    }
    out.push_str("]}");
}

pub fn delta_cell(grid: &Grid2, p: Pos, out: &mut String) {
    let word = grid.cells.get(&p).copied().unwrap_or(Word2::EMPTY);
    let sid = grid.sid.get(&p).copied();
    if word == Word2::EMPTY && sid.is_none() {
        write!(out, "[{},{},{},null]", p.0, p.1, p.2).unwrap();
        return;
    }
    let site = word.unpack().expect("canonical");
    write!(out, "[{},{},{},{{\"word\":\"{:016x}\"", p.0, p.1, p.2, word.0).unwrap();
    if let Some(sid) = sid { write!(out, ",\"sid\":{}", sid).unwrap(); }
    match &site.cell {
        Cell::Empty { .. } => {}
        Cell::Agent { tag, principal, aux, pass, nursery, .. } => {
            write!(out, ",\"agent\":[\"{}\",[{}]]", tag.name(), agent_faces(*tag, principal, aux)).unwrap();
            if !pass.is_empty() {
                let strands = pass.iter().map(|r| format!("\"{}\"", strand(r, false)))
                    .collect::<Vec<_>>().join(",");
                write!(out, ",\"wire\":[{}]", strands).unwrap();
            }
            if *nursery {
                write!(out, ",\"guest\":[\"{}\",0,\"{}\"]", tag.name(), principal.face.ch()).unwrap();
            }
        }
        Cell::Wire { routes, hot, .. } => {
            let strands = routes.iter().enumerate()
                .map(|(i, r)| format!("\"{}\"", strand(r, (hot >> i) & 1 == 1)))
                .collect::<Vec<_>>().join(",");
            write!(out, ",\"wire\":[{}]", strands).unwrap();
        }
        Cell::Seed { pass, .. } => {
            if let Some(r) = pass {
                write!(out, ",\"wire\":[\"{}\"]", strand(r, false)).unwrap();
            }
        }
    }
    if site.chi != 0 { write!(out, ",\"chi\":{}", site.chi as u32 * 16).unwrap(); }
    if let Some((role, phase)) = motion_of(&site) {
        write!(out, ",\"motion\":[\"{}\",\"{}\"]", role, phase).unwrap();
    }
    out.push_str("}]");
}

fn frame(out: &mut String, r: &Runner, tick: u64, events: &str, delta: Option<&[Pos]>) {
    let length: usize = r.grid.cells.values()
        .filter(|w| matches!(w.unpack().unwrap().cell, Cell::Wire { .. }))
        .count();
    write!(
        out,
        "{{\"tick\":{},\"events\":[{}],\"ints\":{},\"transport\":{},\"pairs\":{},\"wire\":{{\"length\":{},\"chord\":null,\"slack\":null}}",
        tick, events, r.shadow.ints, r.grid.transport, r.shadow.all_active_pairs().len(), length,
    ).unwrap();
    if let Some(delta) = delta {
        out.push_str(",\"delta\":[");
        for (i, p) in delta.iter().enumerate() {
            if i != 0 { out.push(','); }
            delta_cell(&r.grid, *p, out);
        }
        out.push(']');
    } else {
        out.push_str(",\"grid\":");
        snapshot(&r.grid, out);
    }
    out.push('}');
}

/// Run a serial cascade to quiescence, recording one keyframe and per-activation deltas.
/// Run a serial cascade to quiescence, recording one keyframe and then one frame per
/// generation: every transition that fired in the same wavefront step is merged into one
/// displayed tick, which is the substrate's maximal simultaneous parallelism. Each
/// activation event carries its index and the front's width, and the trace note reports
/// the peak and mean widths.
pub fn trace_run(
    name: &str,
    note: &str,
    mut runner: Runner,
    budget: u64,
    out: &mut String,
) {
    let mut key = String::new();
    frame(&mut key, &runner, 0, "", None);

    let mut frames: Vec<String> = vec![key];
    let mut seen_events = 0usize;
    let mut spent = 0u64;
    let mut widths: Vec<usize> = vec![];
    'run: loop {
        // One whole generation: collect every commit of the wavefront into one frame.
        let start_gen = runner.generation;
        let before = runner.grid.cells.clone();
        let mut acts: Vec<(Pos, bool)> = vec![];
        loop {
            if spent >= budget {
                break 'run;
            }
            let snap = runner.grid.cells.clone();
            let Some(at) = runner.tick_traced() else {
                if acts.is_empty() {
                    break 'run;
                }
                break;
            };
            spent += 1;
            let changed_now = snap != runner.grid.cells;
            acts.push((at, changed_now));
            if runner.generation != start_gen {
                break;
            }
        }
        let mut changed: Vec<Pos> = vec![];
        for (p, w) in &runner.grid.cells {
            if before.get(p) != Some(w) {
                changed.push(*p);
            }
        }
        for p in before.keys() {
            if !runner.grid.cells.contains_key(p) {
                changed.push(*p);
            }
        }
        let width = acts.iter().filter(|(_, c)| *c).count();
        if changed.is_empty() && runner.events.len() == seen_events {
            if runner.quiescent() {
                break;
            }
            continue;
        }
        widths.push(width);
        let mut events = String::new();
        let mut idx = 0usize;
        for (at, changed_now) in &acts {
            if !*changed_now {
                continue;
            }
            if !events.is_empty() {
                events.push(',');
            }
            write!(
                events,
                "{{\"t\":\"activate\",\"at\":[{},{},{}],\"changed\":true,\"index\":{},\"sweep\":{},\"round\":{}}}",
                at.0, at.1, at.2, idx, width, start_gen,
            ).unwrap();
            idx += 1;
        }
        for e in &runner.events[seen_events..] {
            if let Event::Fire(p, rule) = e {
                let r = &crate::rules::RULES[*rule as usize];
                if !events.is_empty() {
                    events.push(',');
                }
                write!(
                    events,
                    "{{\"t\":\"fire\",\"rule\":\"{}\u{b7}{}\",\"c\":[{},{},{}],\"p\":[{},{},{}],\"fresh\":[]}}",
                    r.consumer.name(), r.producer.name(), p.0, p.1, p.2, p.0, p.1, p.2,
                ).unwrap();
            }
        }
        seen_events = runner.events.len();
        let mut f = String::new();
        frame(&mut f, &runner, frames.len() as u64, &events, Some(&changed));
        frames.push(f);
        if runner.quiescent() {
            break;
        }
    }

    let quiet = runner.quiescent();
    let max_w = widths.iter().copied().max().unwrap_or(0);
    let mean_w = if widths.is_empty() {
        0.0
    } else {
        widths.iter().sum::<usize>() as f64 / widths.len() as f64
    };
    write!(
        out,
        "{{\"schema_version\":4,\"engine_revision\":\"cascade\",\"topo\":\"{}\",\"readback\":\"\",\"kind\":\"cascade-suite\",\"name\":\"{}\",\"suite_tier\":\"example\",\"note\":\"{} [one frame per generation; parallel width peak {} mean {:.1}]\",\"bit_layout\":{{\"name\":\"cascade\",\"payload\":{{\"fixed\":36}},\"fields\":{{\"chi\":4}},\"motion\":{{\"fixed\":21}},\"observer\":{{\"sid\":32}},\"max_local\":64}},\"frames\":[",
        runner.grid.topo.name(), name, note, max_w, mean_w,
    ).unwrap();
    for (i, f) in frames.iter().enumerate() {
        if i != 0 {
            out.push(',');
        }
        out.push_str(f);
    }
    write!(
        out,
        "],\"ticks\":{},\"result\":{{\"pass\":{},\"sweeps\":{},\"transport\":{},\"rewrites\":{},\"ints\":{}}}}}",
        frames.len() - 1, quiet, runner.generation, runner.grid.transport, runner.grid.rewrites,
        runner.shadow.ints,
    ).unwrap();
}
