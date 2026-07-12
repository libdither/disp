//! Trace dumper: run a term on the lattice and emit one JSON document with a full grid
//! snapshot + the tick's events per frame. This is the instrumentation face the player
//! consumes (research/interaction-combinator/lattice_player.html): the engine is the
//! model, the visualization is a replay client — never a second simulator.
//!
//! Usage: dump-run <fork|stem|chain1|chain2|chain3|ksimple|kargs|share2|share|disp> [max_ticks]

use rust_ca_lattice::lattice::{Cell, Grid};
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Event, Sim};
use std::fmt::Write as _;

fn dir_ch(d: rust_ca_lattice::lattice::Dir) -> char {
    use rust_ca_lattice::lattice::Dir::*;
    match d { N => 'N', E => 'E', S => 'S', W => 'W' }
}

fn snapshot(grid: &Grid, out: &mut String) {
    out.push_str("{\"agents\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Agent(a) = c {
            if !first { out.push(','); }
            first = false;
            let faces: Vec<String> = (0..a.tag.arity()).map(|i| {
                let (d, l) = a.face_of(i);
                format!("\"{}{}\"", dir_ch(d), l)
            }).collect();
            let tucks: Vec<String> = a.tucks.iter().flatten().map(|t| {
                format!("\"{}{}{}{}\"", dir_ch(t.a.0), t.a.1, dir_ch(t.b.0), t.b.1)
            }).collect();
            write!(out, "[{},{},\"{}\",{},[{}],[{}]]", p.0, p.1, a.tag.name(), a.sid,
                faces.join(","), tucks.join(",")).unwrap();
        }
    }
    out.push_str("],\"wires\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Wire(w) = c {
            if !first { out.push(','); }
            first = false;
            let ss: Vec<String> = w.iter().map(|t| {
                format!("\"{}{}{}{}\"", dir_ch(t.a.0), t.a.1, dir_ch(t.b.0), t.b.1)
            }).collect();
            write!(out, "[{},{},[{}]]", p.0, p.1, ss.join(",")).unwrap();
        }
    }
    out.push_str("]}");
}

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "fork".into());
    let max_ticks: u64 = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(400);
    let term = match which.as_str() {
        "fork" => ap(f2(Term::L, Term::L), Term::L),
        "stem" => ap(s(Term::L), Term::L),
        "chain1" => oracle::chain_k(1),
        "chain2" => oracle::chain_k(2),
        "chain3" => oracle::chain_k(3),
        "ksimple" => ap(ap(oracle::k(), Term::L), s(Term::L)),
        "kargs" => ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
        "share2" => ap(f2(s(Term::L), Term::L), Term::L),
        "share" => ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
        "disp" => oracle::disp_t(),
        other => panic!("unknown term {other}"),
    };
    let oracle_nf = oracle::nf(term.clone(), &mut Fuel(200_000))
        .map(|w| oracle::show(&w)).unwrap_or_else(|_| "(diverges)".into());

    let mut sim = Sim::load(&term);
    let mut out = String::new();
    write!(out, "{{\"name\":\"{which}\",\"term\":\"{}\",\"oracle\":\"{oracle_nf}\",\"frames\":[",
        oracle::show(&term)).unwrap();

    let mut status = "cap";
    let mut frames = 0u64;
    // frame 0: the embedding, no events
    out.push_str("{\"tick\":0,\"events\":[],\"ints\":0,\"transport\":0,\"pairs\":");
    write!(out, "{},", sim.shadow.all_active_pairs().len()).unwrap();
    out.push_str("\"grid\":");
    snapshot(&sim.grid, &mut out);
    out.push('}');
    for tick in 1..=max_ticks {
        if sim.shadow.all_active_pairs().is_empty() { status = "done"; break; }
        let n = sim.tick(CheckLevel::Tick);
        let mut ev = String::new();
        for e in &sim.events {
            if !ev.is_empty() { ev.push(','); }
            match e {
                Event::Fire { cpos, ppos, rule, fresh } => {
                    let fr: Vec<String> = fresh.iter().map(|p| format!("[{},{}]", p.0, p.1)).collect();
                    write!(ev, "{{\"t\":\"fire\",\"c\":[{},{}],\"p\":[{},{}],\"rule\":\"{}·{}\",\"fresh\":[{}]}}",
                        cpos.0, cpos.1, ppos.0, ppos.1, rule.0, rule.1, fr.join(",")).unwrap();
                }
                Event::Reel { from, to, sid } => {
                    write!(ev, "{{\"t\":\"reel\",\"from\":[{},{}],\"to\":[{},{}],\"sid\":{}}}",
                        from.0, from.1, to.0, to.1, sid).unwrap();
                }
            }
        }
        write!(out, ",{{\"tick\":{tick},\"events\":[{ev}],\"ints\":{},\"transport\":{},\"pairs\":{},\"grid\":",
            sim.shadow.ints, sim.grid.transport, sim.shadow.all_active_pairs().len()).unwrap();
        snapshot(&sim.grid, &mut out);
        out.push('}');
        frames = tick;
        if n == 0 { status = "stuck"; break; }
    }
    if sim.shadow.all_active_pairs().is_empty() { status = "done"; }
    let readback = sim.grid.readback().map(|t| oracle::show(&t)).unwrap_or_else(|| "-".into());
    write!(out, "],\"status\":\"{status}\",\"readback\":\"{readback}\",\"ticks\":{frames}}}").unwrap();
    println!("{out}");
}
