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
            _ => which = Some(a),
        }
    }
    let which = which.unwrap_or_else(|| {
        eprintln!("usage: debug-cascade <identity|fork|k|s-rule|k-chain|disp-t|term> [--discipline fifo|lifo|random|addr] [--budget N]");
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
                for nd in DIRS {
                    println!("      nb {} {:?}: {}", nd.ch(), step(t, nd), full_site(&r.grid.site(step(t, nd))));
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
}
