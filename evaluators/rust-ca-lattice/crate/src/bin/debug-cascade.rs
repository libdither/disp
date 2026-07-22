//! Stuck-state census for the cascade engine: run a term to quiescence and print agents,
//! seeds, cursors (blocked op decoded), and heat fronts, so frontier debugging needs no
//! throwaway test files.
//!
//! Usage: cargo run --release --bin debug-cascade -- <preset|term>
//!            [--discipline fifo|lifo|random|addr] [--budget N]
//! Presets: identity, fork, k, s-rule, k-chain, disp-t; or a term in the oracle's show()
//! syntax, e.g. "@(F(L,L),L)".

use rust_ca_lattice::blocklet::{layout, Op};
use rust_ca_lattice::cascade::{rot_cell, rot_dir, Cell, EndPt, Grid2, Route, Site};
use rust_ca_lattice::cascade_run::{load_net, Discipline, Runner};
use rust_ca_lattice::lattice::DIRS;
use rust_ca_lattice::lattice::{step, Pos, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, nf, s, show, Fuel, Term};
use rust_ca_lattice::rules::RULES;

fn preset(name: &str) -> Option<Term> {
    Some(match name {
        "identity" => ap(Term::L, Term::L),
        "fork" => ap(f2(Term::L, Term::L), Term::L),
        "k" => ap(ap(oracle::k(), s(Term::L)), Term::L),
        "s-rule" => ap(f2(s(Term::L), s(Term::L)), Term::L),
        "k-chain" => oracle::chain_k(2),
        "disp-t" => oracle::disp_t(),
        _ => return None,
    })
}

/// Parse the oracle's show() syntax: L, S(t), F(t,t), @(t,t).
fn parse_term(src: &str) -> Option<Term> {
    fn eat(b: &[u8], i: &mut usize, c: u8) -> Option<()> {
        (b.get(*i) == Some(&c)).then(|| *i += 1)
    }
    fn go(b: &[u8], i: &mut usize) -> Option<Term> {
        let head = *b.get(*i)?;
        *i += 1;
        Some(match head {
            b'L' => Term::L,
            b'S' => {
                eat(b, i, b'(')?;
                let a = go(b, i)?;
                eat(b, i, b')')?;
                s(a)
            }
            b'F' | b'@' => {
                eat(b, i, b'(')?;
                let a = go(b, i)?;
                eat(b, i, b',')?;
                let x = go(b, i)?;
                eat(b, i, b')')?;
                if head == b'F' { f2(a, x) } else { ap(a, x) }
            }
            _ => return None,
        })
    }
    let b = src.as_bytes();
    let mut i = 0;
    let t = go(b, &mut i)?;
    (i == b.len()).then_some(t)
}

fn rule_name(rule: u8) -> String {
    let r = &RULES[rule as usize];
    format!("{}·{}", r.consumer.name(), r.producer.name())
}

fn ep(e: EndPt) -> String { format!("{}{}", e.face.ch(), e.lane) }
fn rt(r: Route) -> String { format!("{}-{}", ep(r.a), ep(r.b)) }

/// Kind + route count + hot bits, for one-glance summaries.
fn brief(cell: &Cell) -> String {
    match cell {
        Cell::Empty { reserved: None } => "Empty".into(),
        Cell::Empty { reserved: Some(d) } => format!("Empty rsv<-{}", d.ch()),
        Cell::Wire { routes, hot, .. } => format!("Wire {}r hot={:03b}", routes.len(), hot),
        Cell::Agent { tag, pass, .. } => format!("Agent {} ({} pass)", tag.name(), pass.len()),
        Cell::Seed { rule, half, .. } => format!("Seed {} {:?}", rule_name(*rule), half),
    }
}

/// Every field of a cell, compactly; a `*` marks a hot route.
fn full(cell: &Cell) -> String {
    match cell {
        Cell::Empty { .. } => brief(cell),
        Cell::Wire { routes, hot, cooldown, reserved } => format!(
            "Wire[{}] cd={}{}",
            routes
                .iter()
                .enumerate()
                .map(|(i, r)| format!("{}{}", rt(*r), if (hot >> i) & 1 == 1 { "*" } else { "" }))
                .collect::<Vec<_>>()
                .join(" "),
            cooldown,
            reserved.map(|d| format!(" rsv<-{}", d.ch())).unwrap_or_default()
        ),
        Cell::Agent { tag, principal, aux, pass, nursery, cooldown } => format!(
            "Agent {} pr={} aux=[{} {}] pass=[{}]{}{}",
            tag.name(),
            ep(*principal),
            ep(aux[0]),
            ep(aux[1]),
            pass.iter().map(|r| rt(*r)).collect::<Vec<_>>().join(" "),
            if *nursery { " nursery" } else { "" },
            if *cooldown > 0 { format!(" cd={cooldown}") } else { String::new() }
        ),
        Cell::Seed { rule, half, partner, roll, stub, plane, pass } => format!(
            "Seed {} {:?} partner={} roll={roll} plane={plane} stub=[{} {}] pass={}",
            rule_name(*rule),
            half,
            partner.ch(),
            ep(stub[0]),
            ep(stub[1]),
            pass.as_ref().map_or("-".into(), |r| rt(*r))
        ),
    }
}

fn full_site(site: &Site) -> String {
    let mut out = full(&site.cell);
    if let Some(c) = &site.cursor {
        out.push_str(&format!(" +cursor(pc={})", c.pc));
    }
    if site.chi > 0 {
        out.push_str(&format!(" chi={}", site.chi));
    }
    if site.claim {
        out.push_str(" claim");
    }
    out
}

/// Walk a consumer's principal wire reading hot bits until the heat stops or matter ends.
fn heat_front(grid: &Grid2, p: Pos, start: EndPt) -> String {
    let mut cur = step(p, start.face);
    let mut enter = EndPt { face: start.face.opp(), lane: start.lane };
    let mut hot_cells = 0u32;
    for _ in 0..200 {
        let site = grid.site(cur);
        match &site.cell {
            Cell::Wire { routes, hot, .. } => {
                let Some(i) = routes.iter().position(|r| r.through(enter).is_some()) else {
                    return format!("dangling at {cur:?} (no route through {})", ep(enter));
                };
                if (*hot >> i) & 1 == 0 {
                    return format!("{hot_cells} hot cells, first cold at {cur:?} ({})", rt(routes[i]));
                }
                hot_cells += 1;
                let exit = routes[i].through(enter).unwrap();
                cur = step(cur, exit.face);
                enter = EndPt { face: exit.face.opp(), lane: exit.lane };
            }
            Cell::Agent { tag, pass, .. } => {
                // A guest crossing continues; anything else is the far terminus.
                if let Some(exit) = pass.iter().find_map(|r| r.through(enter)) {
                    cur = step(cur, exit.face);
                    enter = EndPt { face: exit.face.opp(), lane: exit.lane };
                } else {
                    return format!("hot all the way ({hot_cells} cells) to Agent {} {cur:?}", tag.name());
                }
            }
            Cell::Seed { rule, pass, .. } => {
                if let Some(exit) = pass.as_ref().and_then(|r| r.through(enter)) {
                    cur = step(cur, exit.face);
                    enter = EndPt { face: exit.face.opp(), lane: exit.lane };
                } else {
                    return format!("hot all the way ({hot_cells} cells) to Seed {} {cur:?}", rule_name(*rule));
                }
            }
            Cell::Empty { .. } => return format!("runs into empty at {cur:?}"),
        }
    }
    format!("{hot_cells}+ hot cells, no terminus within 200")
}

fn main() {
    let mut which = None;
    let mut discipline = Discipline::Fifo;
    let mut budget = 20_000_000u64;
    let mut why = false;
    let mut kick = false;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--discipline" => {
                discipline = match args.next().as_deref() {
                    Some("fifo") => Discipline::Fifo,
                    Some("lifo") => Discipline::Lifo,
                    Some("random") => Discipline::Random(1),
                    Some("addr") => Discipline::AddressOrdered,
                    other => panic!("--discipline fifo|lifo|random|addr, got {other:?}"),
                };
            }
            "--budget" => budget = args.next().and_then(|v| v.parse().ok()).expect("--budget N"),
            "--why" => why = true,
            "--kick" => kick = true,
            _ => which = Some(a),
        }
    }
    let which = which.unwrap_or_else(|| {
        eprintln!("usage: debug-cascade <identity|fork|k|s-rule|k-chain|disp-t|term> [--discipline fifo|lifo|random|addr] [--budget N] [--why]");
        std::process::exit(2);
    });
    let term = preset(&which).or_else(|| parse_term(&which)).unwrap_or_else(|| {
        eprintln!("unknown preset / unparsable term {which:?}");
        std::process::exit(2);
    });

    let oracle_nf = nf(term.clone(), &mut Fuel(100_000_000)).ok();
    println!("term:       {}", show(&term));
    println!("oracle nf:  {}", oracle_nf.as_ref().map(show).unwrap_or_else(|| "out of fuel".into()));
    println!("discipline: {discipline:?}   budget: {budget}");

    let mut shadow = Net::new();
    let root = shadow.build(&term);
    let (_nrm, out) = shadow.drive(root);
    let grid = load_net(&shadow, Topo::Full3D).expect("net embeds");
    let mut r = Runner::new(grid, shadow, discipline);
    let quiet = r.run(budget);
    let read = r.shadow.readback(r.shadow.get(out).ports[0]);
    let complete = read.is_some() && read == oracle_nf;
    println!();
    println!("quiescent:  {}", if quiet { "yes" } else { "no (budget exhausted)" });
    println!(
        "rewrites:   {}   transport: {}   generations: {}   shadow ints: {}",
        r.grid.rewrites, r.grid.transport, r.generation, r.shadow.ints
    );
    println!(
        "out port:   {}  [{}]",
        read.as_ref().map(show).unwrap_or_else(|| "unreadable".into()),
        if complete { "COMPLETE" } else { "PARKED" }
    );

    // --why probe requests collected during the census: (label, relief target, planned).
    let mut probes: Vec<(String, Pos, Option<Cell>, Option<Pos>)> = vec![];

    let agents: Vec<(Pos, Site)> = r.grid.agents().collect();
    println!();
    println!("agents ({}):", agents.len());
    for (p, site) in &agents {
        let Cell::Agent { tag, principal, aux, pass, nursery, .. } = &site.cell else { unreachable!() };
        let sid = r.grid.sid.get(p).map_or("-".into(), |s| s.to_string());
        let auxes: Vec<String> = (0..tag.arity().saturating_sub(1)).map(|k| ep(aux[k])).collect();
        let facing = r.grid.site(step(*p, principal.face));
        println!(
            "  {:<4} {:?} sid={sid} pr={} aux=[{}] nursery={nursery} pass={} | faces {}",
            tag.name(), p, ep(*principal), auxes.join(" "), pass.len(), brief(&facing.cell)
        );
        // A demanded producer that is not moving is a walk wedge: dump its own cell and
        // the principal target's neighborhood so the refusal is decodable.
        let target_hot = match &facing.cell {
            Cell::Wire { routes, hot, .. } => {
                let enter = EndPt { face: principal.face.opp(), lane: principal.lane };
                routes.iter().position(|r| r.through(enter).is_some())
                    .is_some_and(|i| (hot >> i) & 1 == 1)
            }
            _ => false,
        };
        if tag.is_producer() && !nursery && target_hot {
            let t = step(*p, principal.face);
            println!("       walk-blocked: self {}", full_site(site));
            println!("       target {:?}: {}", t, full_site(&facing));
            for nd in DIRS {
                println!("         nb {} {:?}: {}", nd.ch(), step(t, nd), full_site(&r.grid.site(step(t, nd))));
            }
            probes.push((format!("walker {} self-relief at {p:?}", tag.name()), *p, None, None));
            probes.push((format!("walker {} target relief at {t:?}", tag.name()), t, None, None));
        }
    }

    let sites: Vec<(Pos, Site)> = r.grid.cells.keys().map(|p| (*p, r.grid.site(*p))).collect();
    let seeds: Vec<&(Pos, Site)> =
        sites.iter().filter(|(_, s)| matches!(s.cell, Cell::Seed { .. })).collect();
    println!();
    println!("seeds ({}):", seeds.len());
    for (p, site) in &seeds {
        println!("  {:?} {}", p, full(&site.cell));
    }

    let mut blocked_place = false;
    let cursors: Vec<&(Pos, Site)> = sites.iter().filter(|(_, s)| s.cursor.is_some()).collect();
    println!("cursors ({}):", cursors.len());
    for (p, site) in &cursors {
        let c = site.cursor.unwrap();
        let lay = layout(c.rule);
        println!(
            "  {:?} rule={} axis={} roll={} pc={}/{} (resolve@{}) reverse={} on={}",
            p, rule_name(c.rule), c.axis.ch(), c.roll, c.pc, lay.script.len(), lay.resolve_pc,
            c.reverse, brief(&site.cell)
        );
        let idx = if c.reverse {
            (c.pc > 0).then(|| c.pc as usize - 1)
        } else {
            ((c.pc as usize) < lay.script.len()).then(|| c.pc as usize)
        };
        let Some(idx) = idx else {
            println!("    past the script end (cursor evaporates next activation)");
            continue;
        };
        match &lay.script[idx] {
            Op::Place { dir, cell } => {
                blocked_place = true;
                let d = rot_dir(*dir, c.axis, c.roll);
                let t = step(*p, d);
                println!("    op Place toward {} at {:?}", d.ch(), t);
                println!("      planned: {}", full(&rot_cell(cell, c.axis, c.roll)));
                println!("      target:  {}", full_site(&r.grid.site(t)));
                probes.push((
                    format!("place target at {t:?}"),
                    t,
                    Some(rot_cell(cell, c.axis, c.roll)),
                    Some(*p),
                ));
                for nd in DIRS {
                    println!("      nb {} {:?}: {}", nd.ch(), step(t, nd), full_site(&r.grid.site(step(t, nd))));
                }
                // The whole pocket: every non-empty cell within Chebyshev distance 2 of
                // the blocked target, so eviction geometry is decodable from one dump.
                println!("      pocket (d<=2 around {:?}):", t);
                for dz in -2i32..=2 {
                    for dy in -2i32..=2 {
                        for dx in -2i32..=2 {
                            let q = (t.0 + dx, t.1 + dy, t.2 + dz);
                            let s = r.grid.site(q);
                            if matches!(s.cell, Cell::Empty { reserved: None }) && s.cursor.is_none() {
                                continue;
                            }
                            println!("        {:?}: {}", q, full_site(&s));
                        }
                    }
                }
            }
            Op::Hop { dir, finalize } => {
                let d = rot_dir(*dir, c.axis, c.roll);
                let t = if c.reverse { step(*p, d.opp()) } else { step(*p, d) };
                println!(
                    "    op Hop toward {} finalize={finalize} target {:?}: {}",
                    d.ch(), t, full_site(&r.grid.site(t))
                );
            }
        }
    }

    println!();
    println!("heat fronts:");
    let mut any_front = false;
    for (p, site) in &agents {
        let Cell::Agent { tag, principal, nursery, .. } = &site.cell else { unreachable!() };
        if *nursery || !tag.is_consumer() {
            continue;
        }
        any_front = true;
        println!("  {:<4} {:?}: {}", tag.name(), p, heat_front(&r.grid, *p, *principal));
    }
    if !any_front {
        println!("  (no live consumers)");
    }

    let live: Vec<&(Pos, Site)> = agents
        .iter()
        .filter(|(_, s)| !matches!(s.cell, Cell::Agent { nursery: true, .. }))
        .collect();
    let parked_cold = |p: Pos, pr: EndPt| {
        let enter = EndPt { face: pr.face.opp(), lane: pr.lane };
        match &r.grid.site(step(p, pr.face)).cell {
            Cell::Wire { routes, hot, .. } => routes
                .iter()
                .position(|x| x.through(enter).is_some())
                .is_some_and(|i| (*hot >> i) & 1 == 0),
            _ => false,
        }
    };
    let demand_gap = !live.is_empty() && live.iter().all(|(p, s)| {
        let Cell::Agent { principal, .. } = &s.cell else { return false };
        parked_cold(*p, *principal)
    });
    let knot = live.iter().any(|(p, s)| {
        let Cell::Agent { tag, principal, .. } = &s.cell else { return false };
        tag.is_producer() && matches!(r.grid.site(step(*p, principal.face)).cell, Cell::Agent { .. })
    });
    let hint = if complete {
        "complete or unknown"
    } else if blocked_place {
        "growth wedge"
    } else if demand_gap {
        "demand gap"
    } else if knot {
        "movement knot"
    } else {
        "complete or unknown"
    };
    println!();
    println!("hint: {hint}");

    // --why: walk the relief decision tree read-only-ish for every blocked op, on a
    // scratch copy of the parked grid, and print each refusing check. A probe that
    // SUCCEEDS on the parked state means relief is possible right now, which points at a
    // lost wake rather than a genuine wedge.
    if why {
        for (label, target, planned, owner) in probes {
            println!();
            println!("why-probe: {label}");
            let mut scratch = Runner::new(r.grid.clone(), Net::new(), Discipline::Fifo);
            scratch.explain = Some(vec![]);
            scratch.relief_owner = owner;
            scratch.relief_root = owner.map(|_| target);
            let ok = scratch.try_evict(target, planned.as_ref(), 2);
            for line in scratch.explain.take().unwrap_or_default() {
                println!("  {line}");
            }
            println!(
                "  verdict: {}",
                if ok {
                    "SUCCEEDED — relief is possible at this state; a park here means a lost wake"
                } else {
                    "refused (genuinely stuck at the current state)"
                }
            );
        }
    } else if !probes.is_empty() {
        println!("({} blocked op(s); rerun with --why for the relief decision trace)", probes.len());
    }

    // --kick: wake every live cell of the parked run and keep going. Progress after a
    // kick means the park was a lost wake, not a genuine wedge.
    if kick {
        let live: Vec<Pos> = r.grid.cells.keys().copied().collect();
        for p in live {
            r.wake(p);
        }
        let quiet = r.run(budget);
        let read = r.shadow.readback(r.shadow.get(out).ports[0]);
        let complete = read.is_some() && read == oracle_nf;
        println!();
        println!(
            "after kick: quiescent {} rewrites {} generations {} out {} [{}]",
            if quiet { "yes" } else { "no" },
            r.grid.rewrites,
            r.generation,
            read.as_ref().map(show).unwrap_or_else(|| "unreadable".into()),
            if complete { "COMPLETE" } else { "PARKED" }
        );
    }
}
