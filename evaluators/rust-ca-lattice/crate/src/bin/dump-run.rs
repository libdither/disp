//! Trace dumper: run a term on the lattice and emit one JSON document with a full grid
//! snapshot + the tick's events per frame. This is the instrumentation face the player
//! consumes (research/interaction-combinator/lattice_player.html): the engine is the
//! model, the visualization is a replay client — never a second simulator.
//!
//! Usage: dump-run <fork|stem|chain1..chain4|ksimple|kargs|share2|share|disp|selF|rules|...> [max_ticks] [bilayer|full3d] [stride]
//!
//! Trace schema (v3, local motion + settling): positions are [x,y,z]; faces are single
//! direction chars N/E/S/W/U/D; strands are two-char face pairs (a trailing `*` marks
//! ψ-hot); agents carry no tucks. Each frame includes wire length/chord/slack and compact
//! local-motion roles. Top-level `capture_stride` records the requested capture density.
//! After semantic NF the trace continues until an exact dynamic fixed
//! point, capped at 1,000 settling ticks. `stride` defaults to 1, capturing every evaluator
//! tick. Opt-in `stride` > 1 thins long exports: a frame is emitted on every fire/dock,
//! else every stride-th tick, with skipped events merged into the next emitted frame. Every
//! recorded frame is one exact discrete state; the player does not infer geometry inside a
//! thinned gap.

use rust_ca_lattice::lattice::{manhattan, Cell, Grid, Topo};
use rust_ca_lattice::fixtures::{rule_atlas, RuleAtlasEntry};
use rust_ca_lattice::local::{
    BackPhase, EndpointPhase, MotionMark, SourcePhase, SquareMode, SquarePhase, TargetPhase,
};
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Event, Sim};
use std::collections::BTreeMap;
use std::fmt::Write as _;

fn snapshot(grid: &Grid, out: &mut String) {
    out.push_str("{\"agents\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Agent(a) = c {
            if !first {
                out.push(',');
            }
            first = false;
            let faces: Vec<String> = (0..a.tag.arity())
                .map(|i| format!("\"{}\"", a.face_of(i).ch()))
                .collect();
            write!(
                out,
                "[{},{},{},\"{}\",{},[{}]]",
                p.0,
                p.1,
                p.2,
                a.tag.name(),
                a.sid,
                faces.join(",")
            )
            .unwrap();
        } else if let Cell::Seed(s) = c {
            if !first {
                out.push(',');
            }
            first = false;
            let faces: Vec<String> = s
                .faces
                .iter()
                .flatten()
                .map(|f| format!("\"{}\"", f.ch()))
                .collect();
            write!(
                out,
                "[{},{},{},\"Seed\",{},[{}]]",
                p.0,
                p.1,
                p.2,
                900_000 + (p.0.rem_euclid(97) * 97 + p.1.rem_euclid(97)) as u32,
                faces.join(",")
            )
            .unwrap();
        }
    }
    out.push_str("],\"wires\":[");
    let mut first = true;
    for (p, c) in &grid.cells {
        if let Cell::Wire(w) = c {
            if !first {
                out.push(',');
            }
            first = false;
            let ss: Vec<String> = w
                .iter()
                .map(|t| {
                    format!(
                        "\"{}{}{}\"",
                        t.a.ch(),
                        t.b.ch(),
                        if t.hot { "*" } else { "" }
                    )
                })
                .collect();
            write!(out, "[{},{},{},[{}]]", p.0, p.1, p.2, ss.join(",")).unwrap();
        }
    }
    // χ pressure, sparse: only cells the field currently touches
    out.push_str("],\"chi\":[");
    let mut first = true;
    for (p, v) in &grid.chi {
        if *v == 0 {
            continue;
        }
        if !first {
            out.push(',');
        }
        first = false;
        write!(out, "[{},{},{},{}]", p.0, p.1, p.2, v).unwrap();
    }
    // The staged radius-one geometry protocol is part of the simulated cell state.
    // Keep its replay representation deliberately small: position, role, phase.  The
    // payload before/after commit remains visible in `agents` and `wires`.
    out.push_str("],\"motion\":[");
    let mut first = true;
    for (p, mark) in &grid.motion {
        if !first {
            out.push(',');
        }
        first = false;
        let (role, phase) = motion_role(*mark);
        write!(out, "[{},{},{},\"{}\",\"{}\"]", p.0, p.1, p.2, role, phase).unwrap();
    }
    out.push_str("]}");
}

fn source_phase(phase: SourcePhase) -> String {
    match phase {
        SourcePhase::Claim => "claim".into(),
        SourcePhase::Armed => "armed".into(),
        SourcePhase::Countdown(n) => format!("c{n}"),
    }
}

fn target_phase(phase: TargetPhase) -> String {
    match phase {
        TargetPhase::Claim => "claim".into(),
        TargetPhase::Ready => "ready".into(),
        TargetPhase::Countdown(n) => format!("c{n}"),
    }
}

fn square_phase(phase: SquarePhase) -> String {
    match phase {
        SquarePhase::Claim => "claim".into(),
        SquarePhase::Ready => "ready".into(),
        SquarePhase::Countdown(n) => format!("c{n}"),
    }
}

fn endpoint_phase(phase: EndpointPhase) -> String {
    match phase {
        EndpointPhase::Ready => "ready".into(),
        EndpointPhase::Countdown(n) => format!("c{n}"),
    }
}

fn back_phase(phase: BackPhase) -> String {
    match phase {
        BackPhase::Ready => "ready".into(),
        BackPhase::Countdown(n) => format!("c{n}"),
    }
}

fn motion_role(mark: MotionMark) -> (&'static str, String) {
    match mark {
        MotionMark::Source { phase, .. } => ("source", source_phase(phase)),
        MotionMark::Target { phase, .. } => ("target", target_phase(phase)),
        MotionMark::Square { mode, phase, .. } => (
            match mode {
                SquareMode::Extend => "square+",
                SquareMode::Truncate => "square-",
            },
            square_phase(phase),
        ),
        MotionMark::Endpoint { mode, phase, .. } => (
            match mode {
                SquareMode::Extend => "endpoint+",
                SquareMode::Truncate => "endpoint-",
            },
            endpoint_phase(phase),
        ),
        MotionMark::Back { phase, .. } => ("back", back_phase(phase)),
        MotionMark::BulgeSource { phase, .. } => ("bulge-source", source_phase(phase)),
        MotionMark::BulgeSpine { phase, .. } => ("bulge-spine", target_phase(phase)),
        MotionMark::BulgeCorner { plus, phase, .. } => (
            if plus {
                "bulge-corner+"
            } else {
                "bulge-corner-"
            },
            square_phase(phase),
        ),
        MotionMark::BulgeEndpoint { plus, phase, .. } => (
            if plus {
                "bulge-endpoint+"
            } else {
                "bulge-endpoint-"
            },
            endpoint_phase(phase),
        ),
        MotionMark::Debt { .. } => ("debt", "hold".into()),
        MotionMark::SharedContest { age, .. } => ("contested", format!("w{age}")),
    }
}

/// Observer-only wire ledger.  Each abstract edge is visited from both endpoint ports,
/// then retained only from the lexicographically smaller `(sid, port)` endpoint.  This
/// keeps parallel edges and self-edges honest without exposing ids to the dynamics.
/// During seed growth some endpoints are intentionally opaque or dangling, so the whole
/// ledger is null rather than reporting a misleading partial chord.
fn wire_diagnostics(grid: &Grid) -> Option<(i64, i64, i64)> {
    if grid.has_seeds() {
        return None;
    }

    let mut chord = 0i64;
    for (p, agent) in grid.agents() {
        for port in 0..agent.tag.arity() {
            let (far_pos, far_port) = grid.try_trace(p, agent.face_of(port))?;
            let far = grid.agent(far_pos)?;
            let here_key = (agent.sid, port as u8);
            let far_key = (far.sid, far_port as u8);
            if here_key < far_key {
                chord += i64::from(manhattan(p, far_pos).saturating_sub(1));
            }
        }
    }

    let length = grid.total_strands() as i64;
    Some((length, chord, length - chord))
}

fn write_wire_diagnostics(grid: &Grid, out: &mut String) {
    match wire_diagnostics(grid) {
        Some((length, chord, slack)) => {
            write!(
                out,
                "{{\"length\":{length},\"chord\":{chord},\"slack\":{slack}}}"
            )
            .unwrap();
        }
        None => out.push_str("null"),
    }
}

/// Collision-free observer key for exact fixed-point detection.  `tick()` deliberately
/// does not count cooldown bookkeeping, so `n == 0` alone is insufficient.  Comparing
/// the complete dynamic planes before and after the tick catches those silent changes
/// without feeding any global observer back into the automaton.
fn dynamic_key(grid: &Grid) -> String {
    format!(
        "{:?}|{:?}|{}|{:?}|{:?}|{:?}",
        grid.cells, grid.reserved, grid.seed_count, grid.chi, grid.sigma, grid.motion
    )
}

fn pos_json(p: rust_ca_lattice::lattice::Pos) -> String {
    format!("[{},{},{}]", p.0, p.1, p.2)
}

/// The showcase terms: each isolates one reduction dynamic (mirroring the website tree
/// view's examples), with a one-line note the player shows as `watch:`.
fn named(which: &str) -> (Term, &'static str) {
    match which {
        "app" => (ap(Term::L, Term::L),
            "the smallest fire: A·△ — apply(△,x) = S x. One interaction; the fresh S is a RESIDENT (takes over the dying apply cell, zero rewiring)."),
        "stem" => (ap(s(Term::L), Term::L),
            "A·S then normalization: the fresh F inherits one dying cell, the other becomes a splice hub."),
        "fork" => (ap(f2(Term::L, Term::L), Term::L),
            "fork dispatch: A·F packs the arms into a Pair, T1·△ takes the K branch — Unp splits ⟨b,c⟩ and ε eats c. Truncating walks consume their own drag."),
        "chain1" => (oracle::chain_k(1),
            "a K-redex spine: suspensions force (A·@), then values REEL cell by cell along their principal wires to the waiting consumers. Transport dominates work."),
        "chain2" => (oracle::chain_k(2),
            "a deeper K-redex spine: nested suspensions force local star transport through several principal forwarders before the waiting consumers can fire."),
        "chain3" => (oracle::chain_k(3),
            "a deeper K spine with several concurrent walkers. Truncation re-anchors each auxiliary while consuming drag, and pressure lets crowded cells clear themselves."),
        "chain4" => (oracle::chain_k(4),
            "a deep K spine that emphasizes tension: bends move by flips, U-turns cancel by retract, and wires visibly tighten behind the walkers."),
        "ksimple" => (ap(ap(oracle::k(), Term::L), s(Term::L)),
            "K discards an argument: T1·△ answers with b and mints an ε; watch the value walk INTO the eraser (Eps·S, Eps·△) — GC one cell at a time."),
        "kargs" => (ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
            "K with larger arguments: watch χ (red glow) authorize each crowded cell to move its own strand downhill while tension tightens the surrounding wires."),
        "erase" => (ap(ap(oracle::k(), Term::L), f2(Term::L, Term::L)),
            "ε eats a fork: Eps·F FORKS the death pulse into two erasers — the cascade is the GC. Erasers are the cheapest agents: one port, one cell."),
        "forkarg" => (ap(f2(Term::L, Term::L), f2(Term::L, Term::L)),
            "K discards a FORK: T1·△ answers with b, then the death pulse cascades — Eps·Pair splits into two erasers, Eps·F forks again. GC is cheap: every ε is one port, one cell."),
        "share2" => (ap(f2(s(Term::L), Term::L), Term::L),
            "THE S-rule (T1·S), the biggest routine complex: net +3 agents in one fire — Unp unpacks the args pair, δ shares c, three A's build (s c)(b c). The space question lives here."),
        "share" => (ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
            "the S-rule where the shared value is a FORK: δ deep-copies structure (Dn·F creates two forks and two child duplicators) while the fields manage the growing geometry."),
        "dup" => (ap(f2(s(Term::L), Term::L), s(Term::L)),
            "need-duplication of a stem: after the S-rule shares c, δ meets S(△) — Dn·S copies the spine, sharing the child through a fresh δ."),
        "selL" => (ap(f2(f2(Term::L, Term::L), Term::L), Term::L),
            "two-level dispatch, leaf arm: T1·F builds Sel with arms ⟨w,⟨x,b⟩⟩; z = △ picks w and ε erases the unused pair (Eps·Pair)."),
        "selS" => (ap(f2(f2(Term::L, Term::L), Term::L), s(Term::L)),
            "two-level dispatch, stem arm: Sel·S answers (x u) and erases w and b — two Unps, two ε, one A in a single template."),
        "selF" => (ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)),
            "two-level dispatch: T1·F creates the Sel complex (Sel + two Pairs + Unp). Watch hot (gold) wires pull walkers while truncation consumes auxiliary trails. Sel·F is the largest rule in the ROM: 6 fresh agents."),
        "disp" => (oracle::disp_t(), "drives A → T1 → Sel end to end and exercises dispatch, transport, pressure clearance, and geometric settling."),
        other => panic!("unknown term {other}"),
    }
}

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "fork".into());
    let max_ticks: u64 = std::env::args()
        .nth(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(400);
    let topo = match std::env::args().nth(3).as_deref() {
        Some("full3d") => Topo::Full3D,
        _ => Topo::Bilayer,
    };
    let is_rule_atlas = which == "rules";
    let (mut sim, atlas_entries, note, term_text, oracle_nf) = if is_rule_atlas {
        let (sim, entries) = rule_atlas(topo);
        (
            sim,
            entries,
            "all 26 interaction rules start as isolated active pairs; each principal edge has exactly one wire cell, and every auxiliary ends at a private inert Out pad".to_string(),
            "complete 26-rule interaction ROM".to_string(),
            "one spatial fire of every ROM rule".to_string(),
        )
    } else {
        let (term, note) = named(&which);
        let oracle_nf = oracle::nf(term.clone(), &mut Fuel(200_000))
            .map(|w| oracle::show(&w))
            .unwrap_or_else(|_| "(diverges)".into());
        (
            Sim::load(&term, topo),
            Vec::<RuleAtlasEntry>::new(),
            note.to_string(),
            oracle::show(&term),
            oracle_nf,
        )
    };
    let mut out = String::new();
    let name = if topo == Topo::Full3D {
        format!("{which}-3d")
    } else {
        which.clone()
    };
    let kind = if is_rule_atlas { "rule-atlas" } else { "evaluation" };
    write!(out, "{{\"schema_version\":3,\"engine_revision\":\"local-motion-2026-07-15\",\"kind\":\"{kind}\",\"name\":\"{name}\",\"topo\":\"{}\",\"note\":\"{note}\",\"term\":\"{term_text}\",\"oracle\":\"{oracle_nf}\",\"frames\":[",
        topo.name()).unwrap();

    let requested_stride: u64 = std::env::args()
        .nth(4)
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);
    // Atlas navigation names exact frame numbers, and the fixture is intentionally
    // short, so retain every evaluator tick even if a thinning stride was supplied.
    let stride = if is_rule_atlas { 1 } else { requested_stride };
    let settle_cap = if is_rule_atlas { 1 } else { 1_000 };
    let mut atlas_fire_ticks: BTreeMap<(&'static str, &'static str), u64> = BTreeMap::new();
    let mut nf_tick = sim.shadow.all_active_pairs().is_empty().then_some(0u64);
    let mut status = if nf_tick.is_some() { "done" } else { "cap" };
    let mut settled = false;
    let mut settle_ticks = 0u64;
    let mut tick = 0u64;
    // frame 0: the embedding, no events
    out.push_str("{\"tick\":0,\"events\":[],\"ints\":0,\"transport\":0,\"pairs\":");
    write!(out, "{},\"wire\":", sim.shadow.all_active_pairs().len()).unwrap();
    write_wire_diagnostics(&sim.grid, &mut out);
    out.push_str(",\"grid\":");
    snapshot(&sim.grid, &mut out);
    out.push('}');
    // Stall detection mirrors the scheduler's: a zero-applied streak plus a shadow-ints
    // drought (the field moves keep single ticks busy; one quiet tick is not a stall).
    let mut zero_streak = 0u32;
    let (mut last_ints, mut last_progress) = (0u64, 0u64);
    let mut ev = String::new(); // merged events since the last emitted frame
    let mut last_emit = 0u64;
    loop {
        let may_reduce = nf_tick.is_none() && tick < max_ticks;
        let may_settle = nf_tick.is_some() && !settled && settle_ticks < settle_cap;
        if !may_reduce && !may_settle {
            break;
        }

        tick += 1;
        let before = dynamic_key(&sim.grid);
        let n = sim.tick(CheckLevel::Tick);
        let exact_fixed_point = n == 0 && before == dynamic_key(&sim.grid);
        let mut structural = false;
        for e in &sim.events {
            if matches!(e, Event::Fire { .. } | Event::Dock { .. }) {
                structural = true;
            }
            if !ev.is_empty() {
                ev.push(',');
            }
            match e {
                Event::Fire {
                    cpos,
                    ppos,
                    rule,
                    fresh,
                } => {
                    atlas_fire_ticks.entry(*rule).or_insert(tick);
                    let fr: Vec<String> = fresh.iter().map(|p| pos_json(*p)).collect();
                    write!(
                        ev,
                        "{{\"t\":\"fire\",\"c\":{},\"p\":{},\"rule\":\"{}·{}\",\"fresh\":[{}]}}",
                        pos_json(*cpos),
                        pos_json(*ppos),
                        rule.0,
                        rule.1,
                        fr.join(",")
                    )
                    .unwrap();
                }
                Event::Reel { from, to, sid } => {
                    write!(
                        ev,
                        "{{\"t\":\"reel\",\"from\":{},\"to\":{},\"sid\":{}}}",
                        pos_json(*from),
                        pos_json(*to),
                        sid
                    )
                    .unwrap();
                }
                Event::Retract { a, b } => {
                    write!(
                        ev,
                        "{{\"t\":\"retract\",\"a\":{},\"b\":{}}}",
                        pos_json(*a),
                        pos_json(*b)
                    )
                    .unwrap();
                }
                Event::Slide { at, why } => {
                    write!(
                        ev,
                        "{{\"t\":\"slide\",\"at\":{},\"why\":\"{why}\"}}",
                        pos_json(*at)
                    )
                    .unwrap();
                }
                Event::Dock { cpos, ppos, rule } => {
                    write!(
                        ev,
                        "{{\"t\":\"dock\",\"c\":{},\"p\":{},\"rule\":\"{}·{}\"}}",
                        pos_json(*cpos),
                        pos_json(*ppos),
                        rule.0,
                        rule.1
                    )
                    .unwrap();
                }
                Event::Grow { at } => {
                    write!(ev, "{{\"t\":\"grow\",\"at\":{}}}", pos_json(*at)).unwrap();
                }
                Event::Abort { at } => {
                    write!(ev, "{{\"t\":\"abort\",\"at\":{}}}", pos_json(*at)).unwrap();
                }
                Event::AgentStep { from, to } => {
                    write!(
                        ev,
                        "{{\"t\":\"agent-step\",\"from\":{},\"to\":{}}}",
                        pos_json(*from),
                        pos_json(*to)
                    )
                    .unwrap();
                }
                Event::Flip { from, to, hot } => {
                    write!(
                        ev,
                        "{{\"t\":\"flip\",\"from\":{},\"to\":{},\"hot\":{hot}}}",
                        pos_json(*from),
                        pos_json(*to)
                    )
                    .unwrap();
                }
                Event::Approach { from, to } => {
                    write!(
                        ev,
                        "{{\"t\":\"approach\",\"from\":{},\"to\":{}}}",
                        pos_json(*from),
                        pos_json(*to)
                    )
                    .unwrap();
                }
                Event::LocalPressure { at, level } => {
                    write!(
                        ev,
                        "{{\"t\":\"local-pressure\",\"at\":{},\"level\":{level}}}",
                        pos_json(*at)
                    )
                    .unwrap();
                }
            }
        }
        if sim.shadow.ints > last_ints {
            last_ints = sim.shadow.ints;
            last_progress = tick;
        }

        if nf_tick.is_none() && sim.shadow.all_active_pairs().is_empty() {
            nf_tick = Some(tick);
            status = "done";
        }
        if nf_tick.is_some_and(|at| tick > at) {
            settle_ticks += 1;
            settled = exact_fixed_point;
        }

        // Once the semantic net is normal, a long quiet streak is geometry settling
        // rather than a semantic stall. During reduction, use the semantic liveness
        // detector.
        let stuck_now = if nf_tick.is_some() {
            false
        } else if n == 0 {
            zero_streak += 1;
            zero_streak > 48
        } else {
            zero_streak = 0;
            tick - last_progress > 2_000
        };

        let reduction_cap = nf_tick.is_none() && tick == max_ticks;
        let settlement_stop = nf_tick.is_some() && (settled || settle_ticks == settle_cap);
        let last_tick = reduction_cap || settlement_stop || stuck_now;
        if structural || tick - last_emit >= stride || last_tick {
            write!(out, ",{{\"tick\":{tick},\"events\":[{ev}],\"ints\":{},\"transport\":{},\"pairs\":{},\"wire\":",
                sim.shadow.ints, sim.grid.transport, sim.shadow.all_active_pairs().len()).unwrap();
            write_wire_diagnostics(&sim.grid, &mut out);
            out.push_str(",\"grid\":");
            snapshot(&sim.grid, &mut out);
            out.push('}');
            ev.clear();
            last_emit = tick;
        }
        if stuck_now {
            status = "stuck";
            break;
        }
    }
    if nf_tick.is_some() {
        status = "done";
    }
    let readback = if is_rule_atlas {
        "-".into()
    } else {
        sim.grid
            .readback()
            .map(|t| oracle::show(&t))
            .unwrap_or_else(|| "-".into())
    };
    let nf_json = nf_tick.map_or_else(|| "null".into(), |at| at.to_string());
    out.push(']');
    if is_rule_atlas {
        out.push_str(",\"rule_atlas\":{\"principal_wire_cells\":1,\"entries\":[");
        for (index, entry) in atlas_entries.iter().enumerate() {
            if index > 0 { out.push(','); }
            let fire = atlas_fire_ticks
                .get(&(entry.consumer.name(), entry.producer.name()))
                .copied();
            let fire_json = fire.map_or_else(|| "null".into(), |at| at.to_string());
            let end_json = fire
                .map(|at| (at + 1).min(tick))
                .map_or_else(|| "null".into(), |at| at.to_string());
            let y = entry.consumer_pos.1;
            write!(out,
                "{{\"rule\":\"{}·{}\",\"label\":\"{}.p ↔ {}.p\",\"start\":0,\"fire\":{fire_json},\"end\":{end_json},\"start_frame\":0,\"fire_frame\":{fire_json},\"end_frame\":{end_json},\"start_tick\":0,\"fire_tick\":{fire_json},\"end_tick\":{end_json},\"focus\":[[-2,{},0],[4,{},0]],\"principal\":[{},{},{}],\"consumer_sid\":{},\"producer_sid\":{}}}",
                entry.consumer.name(), entry.producer.name(),
                entry.consumer.name(), entry.producer.name(),
                y - 2, y + 2,
                pos_json(entry.consumer_pos), pos_json(entry.principal_wire_pos),
                pos_json(entry.producer_pos), entry.consumer_sid, entry.producer_sid,
            ).unwrap();
        }
        out.push_str("]}");
    }
    write!(out, ",\"status\":\"{status}\",\"readback\":\"{readback}\",\"ticks\":{tick},\"nf_tick\":{nf_json},\"settled\":{settled},\"settle_ticks\":{settle_ticks},\"capture_stride\":{stride}}}").unwrap();
    println!("{out}");
}
