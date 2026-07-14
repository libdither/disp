//! Trace dumper: run a term on the lattice and emit one JSON document with a full grid
//! snapshot + the tick's events per frame. This is the instrumentation face the player
//! consumes (research/interaction-combinator/lattice_player.html): the engine is the
//! model, the visualization is a replay client — never a second simulator.
//!
//! Usage: dump-run <fork|stem|chain1..chain4|ksimple|kargs|share2|share|disp|selF|...> [max_ticks] [bilayer|full3d] [stride]
//!
//! Trace schema (v2, the topo lift): positions are [x,y,z]; faces are single direction
//! chars N/E/S/W/U/D; strands are two-char face pairs (a trailing `*` marks ψ-hot); agents
//! carry no tucks. `stride` > 1 thins frames for long runs: a frame is emitted on every
//! fire/dock, else every stride-th tick, with the skipped ticks' events merged into the
//! next emitted frame (the player interpolates agents across the gap).

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
        } else if let Cell::Seed(s) = c {
            if !first { out.push(','); }
            first = false;
            let faces: Vec<String> = s.faces.iter().flatten()
                .map(|f| format!("\"{}\"", f.ch())).collect();
            write!(out, "[{},{},{},\"Seed\",{},[{}]]", p.0, p.1, p.2,
                900_000 + (p.0.rem_euclid(97) * 97 + p.1.rem_euclid(97)) as u32,
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
                format!("\"{}{}{}\"", t.a.ch(), t.b.ch(), if t.hot { "*" } else { "" })
            }).collect();
            write!(out, "[{},{},{},[{}]]", p.0, p.1, p.2, ss.join(",")).unwrap();
        }
    }
    // χ pressure, sparse: only cells the field currently touches
    out.push_str("],\"chi\":[");
    let mut first = true;
    for (p, v) in &grid.chi {
        if *v == 0 { continue; }
        if !first { out.push(','); }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, v).unwrap();
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
            "fork dispatch: A·F packs the arms into a Pair, T1·△ takes the K branch — Unp splits ⟨b,c⟩, ε eats c. Completes on both topologies (the old bilayer fire-seam wedge dissolved once walks ate their own drag)."),
        "chain1" => (oracle::chain_k(1),
            "a K-redex spine: suspensions force (A·@), then values REEL cell by cell along their principal wires to the waiting consumers. Transport dominates work."),
        "chain2" => (oracle::chain_k(2), "a deeper K spine (see chain1)."),
        "chain3" => (oracle::chain_k(3),
            "a deeper K spine: several concurrent walkers. Completes on full3d only once walks eat their own drag (truncation re-anchors); before that fix, the walkers paved the basement with trail mats and froze. The trail-economy exhibit."),
        "chain4" => (oracle::chain_k(4),
            "the deep-spine class, unlocked by TENSION: before the survey it froze holding 774 strands of slack; now bends migrate (flips), U-turns cancel (retract), and the run finishes holding 5. Watch wires visibly tighten behind the walkers."),
        "ksimple" => (ap(ap(oracle::k(), Term::L), s(Term::L)),
            "K discards an argument: T1·△ answers with b and mints an ε; watch the value walk INTO the eraser (Eps·S, Eps·△) — GC one cell at a time."),
        "kargs" => (ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
            "K with bigger arguments — pressure's first conquest: watch χ (red glow) evaporate the mats and hop bends downhill around the jam. Always completes on full3d; on bilayer it sits exactly at the liveness margin, flipping between done and stuck across tunings."),
        "erase" => (ap(ap(oracle::k(), Term::L), f2(Term::L, Term::L)),
            "ε eats a fork: Eps·F FORKS the death pulse into two erasers — the cascade is the GC. Erasers are the cheapest agents: one port, one cell."),
        "forkarg" => (ap(f2(Term::L, Term::L), f2(Term::L, Term::L)),
            "K discards a FORK: T1·△ answers with b, then the death pulse cascades — Eps·Pair splits into two erasers, Eps·F forks again. GC is cheap: every ε is one port, one cell."),
        "share2" => (ap(f2(s(Term::L), Term::L), Term::L),
            "THE S-rule (T1·S), the biggest routine complex: net +3 agents in one fire — Unp unpacks the args pair, δ shares c, three A's build (b c)(s c). The space question lives here."),
        "share" => (ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
            "the S-rule where the shared value is a FORK: δ deep-copies structure (Dn·F: two fresh forks, two child duplicators). Completes on full3d under the fields; bilayer residue."),
        "dup" => (ap(f2(s(Term::L), Term::L), s(Term::L)),
            "need-duplication of a stem: after the S-rule shares c, δ meets S(△) — Dn·S copies the spine, sharing the child through a fresh δ."),
        "selL" => (ap(f2(f2(Term::L, Term::L), Term::L), Term::L),
            "two-level dispatch, leaf arm: T1·F builds Sel with arms ⟨w,⟨x,b⟩⟩; z = △ picks w and ε erases the unused pair (Eps·Pair)."),
        "selS" => (ap(f2(f2(Term::L, Term::L), Term::L), s(Term::L)),
            "two-level dispatch, stem arm: Sel·S answers (x u) and erases w and b — two Unps, two ε, one A in a single template."),
        "selF" => (ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)),
            "two-level dispatch: T1·F mints the Sel complex (Sel + two Pairs + Unp) — the fire-space stress test that motivated stamps, grow fire, and the fields. On full3d it now runs end to end: watch hot (gold) wires pull walkers and aux trails get eaten behind them. Sel·F, when it fires, is the largest rule in the ROM: 6 fresh."),
        "disp" => (oracle::disp_t(), "drives A → T1 → Sel end to end (completes on full3d; bilayer pockets — residue)."),
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

    let stride: u64 = std::env::args().nth(4).and_then(|s| s.parse().ok()).unwrap_or(1);
    let mut status = "cap";
    let mut frames = 0u64;
    // frame 0: the embedding, no events
    out.push_str("{\"tick\":0,\"events\":[],\"ints\":0,\"transport\":0,\"pairs\":");
    write!(out, "{},", sim.shadow.all_active_pairs().len()).unwrap();
    out.push_str("\"grid\":");
    snapshot(&sim.grid, &mut out);
    out.push('}');
    // Stall detection mirrors the scheduler's: a zero-applied streak plus a shadow-ints
    // drought (the field moves keep single ticks busy; one quiet tick is not a stall).
    let mut zero_streak = 0u32;
    let (mut last_ints, mut last_progress) = (0u64, 0u64);
    let mut ev = String::new(); // merged events since the last emitted frame
    let mut last_emit = 0u64;
    for tick in 1..=max_ticks {
        if sim.shadow.all_active_pairs().is_empty() { status = "done"; break; }
        let n = sim.tick(CheckLevel::Tick);
        let mut structural = false;
        for e in &sim.events {
            if matches!(e, Event::Fire { .. } | Event::Dock { .. }) { structural = true; }
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
                Event::Slide { at, why } => {
                    write!(ev, "{{\"t\":\"slide\",\"at\":{},\"why\":\"{why}\"}}", pos_json(*at)).unwrap();
                }
                Event::Dock { cpos, ppos, rule } => {
                    write!(ev, "{{\"t\":\"dock\",\"c\":{},\"p\":{},\"rule\":\"{}·{}\"}}",
                        pos_json(*cpos), pos_json(*ppos), rule.0, rule.1).unwrap();
                }
                Event::Grow { at } => {
                    write!(ev, "{{\"t\":\"grow\",\"at\":{}}}", pos_json(*at)).unwrap();
                }
                Event::Abort { at } => {
                    write!(ev, "{{\"t\":\"abort\",\"at\":{}}}", pos_json(*at)).unwrap();
                }
                Event::Shove { from, to } => {
                    write!(ev, "{{\"t\":\"shove\",\"from\":{},\"to\":{}}}", pos_json(*from), pos_json(*to)).unwrap();
                }
                Event::Flip { from, to, hot } => {
                    write!(ev, "{{\"t\":\"flip\",\"from\":{},\"to\":{},\"hot\":{hot}}}", pos_json(*from), pos_json(*to)).unwrap();
                }
                Event::Approach { from, to } => {
                    write!(ev, "{{\"t\":\"approach\",\"from\":{},\"to\":{}}}", pos_json(*from), pos_json(*to)).unwrap();
                }
            }
        }
        if sim.shadow.ints > last_ints { last_ints = sim.shadow.ints; last_progress = tick; }
        let stuck_now = if n == 0 {
            zero_streak += 1;
            zero_streak > 48
        } else {
            zero_streak = 0;
            tick - last_progress > 2_000
        };
        let last_tick = tick == max_ticks || stuck_now || sim.shadow.all_active_pairs().is_empty();
        if structural || tick - last_emit >= stride || last_tick {
            write!(out, ",{{\"tick\":{tick},\"events\":[{ev}],\"ints\":{},\"transport\":{},\"pairs\":{},\"grid\":",
                sim.shadow.ints, sim.grid.transport, sim.shadow.all_active_pairs().len()).unwrap();
            snapshot(&sim.grid, &mut out);
            out.push('}');
            ev.clear();
            last_emit = tick;
            frames = tick;
        }
        if stuck_now { status = "stuck"; break; }
    }
    if sim.shadow.all_active_pairs().is_empty() { status = "done"; }
    let readback = sim.grid.readback().map(|t| oracle::show(&t)).unwrap_or_else(|| "-".into());
    write!(out, "],\"status\":\"{status}\",\"readback\":\"{readback}\",\"ticks\":{frames}}}").unwrap();
    println!("{out}");
}
