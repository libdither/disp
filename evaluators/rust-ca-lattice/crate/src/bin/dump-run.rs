//! Trace dumper: run a term on the lattice and emit one JSON document with a full grid
//! snapshot + the tick's events per frame. This is the instrumentation face the player
//! consumes (research/interaction-combinator/lattice_player.html): the engine is the
//! model, the visualization is a replay client — never a second simulator.
//!
//! Usage: dump-run <fork|stem|chain1|chain2|chain3|ksimple|kargs|share2|share|disp> [max_ticks] [bilayer|full3d]
//!
//! Trace schema (v2, the topo lift): positions are [x,y,z]; faces are single direction
//! chars N/E/S/W/U/D; strands are two-char face pairs; agents carry no tucks.

use rust_ca_lattice::lattice::{Cell, Grid, Topo};
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Event, Sim};
use std::fmt::Write as _;

fn snapshot(grid: &Grid, out: &mut String) {
    out.push_str("{\"agents\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Agent(a) = c {
            if !first { out.push(','); }
            first = false;
            let faces: Vec<String> = (0..a.tag.arity())
                .map(|i| format!("\"{}\"", a.face_of(i).ch())).collect();
            write!(out, "[{},{},{},\"{}\",{},[{}]]", p.0, p.1, p.2, a.tag.name(), a.sid,
                faces.join(",")).unwrap();
        }
    }
    out.push_str("],\"wires\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Wire(w) = c {
            if !first { out.push(','); }
            first = false;
            let ss: Vec<String> = w.iter().map(|t| {
                format!("\"{}{}\"", t.a.ch(), t.b.ch())
            }).collect();
            write!(out, "[{},{},{},[{}]]", p.0, p.1, p.2, ss.join(",")).unwrap();
        }
    }
    out.push_str("]}");
}

fn pos_json(p: rust_ca_lattice::lattice::Pos) -> String { format!("[{},{},{}]", p.0, p.1, p.2) }

/// The showcase terms: each isolates one reduction dynamic (mirroring the website tree
/// view's examples), with a one-line note the player shows as `watch:`.
fn named(which: &str) -> (Term, &'static str) {
    match which {
        "app" => (ap(Term::L, Term::L),
            "the smallest fire: A·△ — apply(△,x) = S x. One interaction; the fresh S is a RESIDENT (takes over the dying apply cell, zero rewiring)."),
        "stem" => (ap(s(Term::L), Term::L),
            "A·S then normalization: the fresh F inherits one dying cell, the other becomes a splice hub."),
        "fork" => (ap(f2(Term::L, Term::L), Term::L),
            "fork dispatch: A·F packs the arms into a Pair, T1·△ takes the K branch — Unp splits ⟨b,c⟩, ε eats c. On full3d it completes; on bilayer this exact term WEDGES at the fire seam (the measured residue)."),
        "chain1" => (oracle::chain_k(1),
            "a K-redex spine: suspensions force (A·@), then values REEL cell by cell along their principal wires to the waiting consumers. Transport dominates work."),
        "chain2" => (oracle::chain_k(2), "a deeper K spine (see chain1)."),
        "chain3" => (oracle::chain_k(3), "a deeper K spine (see chain1)."),
        "ksimple" => (ap(ap(oracle::k(), Term::L), s(Term::L)),
            "K discards an argument: T1·△ answers with b and mints an ε; watch the value walk INTO the eraser (Eps·S, Eps·△) — GC one cell at a time."),
        "kargs" => (ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
            "K with bigger arguments (currently pockets on both topologies — residue)."),
        "erase" => (ap(ap(oracle::k(), Term::L), f2(Term::L, Term::L)),
            "ε eats a fork: Eps·F FORKS the death pulse into two erasers — the cascade is the GC. Erasers are the cheapest agents: one port, one cell."),
        "forkarg" => (ap(f2(Term::L, Term::L), f2(Term::L, Term::L)),
            "K discards a FORK: T1·△ answers with b, then the death pulse cascades — Eps·Pair splits into two erasers, Eps·F forks again. GC is cheap: every ε is one port, one cell."),
        "share2" => (ap(f2(s(Term::L), Term::L), Term::L),
            "THE S-rule (T1·S), the biggest routine complex: net +3 agents in one fire — Unp unpacks the args pair, δ shares c, three A's build (b c)(s c). The space question lives here."),
        "share" => (ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
            "the S-rule where the shared value is a FORK: δ deep-copies structure (Dn·F: two fresh forks, two child duplicators). This trace ends STUCK after the copy begins — live residue."),
        "dup" => (ap(f2(s(Term::L), Term::L), s(Term::L)),
            "need-duplication of a stem: after the S-rule shares c, δ meets S(△) — Dn·S copies the spine, sharing the child through a fresh δ. Ends STUCK after the copies land — live residue."),
        "selL" => (ap(f2(f2(Term::L, Term::L), Term::L), Term::L),
            "two-level dispatch, leaf arm: T1·F builds Sel with arms ⟨w,⟨x,b⟩⟩; z = △ picks w and ε erases the unused pair (Eps·Pair)."),
        "selS" => (ap(f2(f2(Term::L, Term::L), Term::L), s(Term::L)),
            "two-level dispatch, stem arm: Sel·S answers (x u) and erases w and b — two Unps, two ε, one A in a single template."),
        "selF" => (ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)),
            "two-level dispatch: T1·F mints the Sel complex (Sel + two Pairs + Unp), and on this trace that complex WEDGES before Sel ever meets z — the sharpest live exhibit of the fire-space problem the stamp proposal targets. Sel·F itself, when it fires, is the largest rule in the ROM: 6 fresh."),
        "disp" => (oracle::disp_t(), "drives A → T1 → Sel end to end (currently pockets — residue)."),
        other => panic!("unknown term {other}"),
    }
}

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "fork".into());
    let max_ticks: u64 = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(400);
    let topo = match std::env::args().nth(3).as_deref() {
        Some("full3d") => Topo::Full3D,
        _ => Topo::Bilayer,
    };
    let (term, note) = named(&which);
    let oracle_nf = oracle::nf(term.clone(), &mut Fuel(200_000))
        .map(|w| oracle::show(&w)).unwrap_or_else(|_| "(diverges)".into());

    let mut sim = Sim::load(&term, topo);
    let mut out = String::new();
    let name = if topo == Topo::Full3D { format!("{which}-3d") } else { which.clone() };
    write!(out, "{{\"name\":\"{name}\",\"topo\":\"{}\",\"note\":\"{note}\",\"term\":\"{}\",\"oracle\":\"{oracle_nf}\",\"frames\":[",
        topo.name(), oracle::show(&term)).unwrap();

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
                    let fr: Vec<String> = fresh.iter().map(|p| pos_json(*p)).collect();
                    write!(ev, "{{\"t\":\"fire\",\"c\":{},\"p\":{},\"rule\":\"{}·{}\",\"fresh\":[{}]}}",
                        pos_json(*cpos), pos_json(*ppos), rule.0, rule.1, fr.join(",")).unwrap();
                }
                Event::Reel { from, to, sid } => {
                    write!(ev, "{{\"t\":\"reel\",\"from\":{},\"to\":{},\"sid\":{}}}",
                        pos_json(*from), pos_json(*to), sid).unwrap();
                }
                Event::Retract { a, b } => {
                    write!(ev, "{{\"t\":\"retract\",\"a\":{},\"b\":{}}}", pos_json(*a), pos_json(*b)).unwrap();
                }
                Event::Slide { at } => {
                    write!(ev, "{{\"t\":\"slide\",\"at\":{}}}", pos_json(*at)).unwrap();
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
