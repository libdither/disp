//! Step 0 of the corridor-as-claim rung (AC_IDEA): is a reaction's own cable big enough
//! to pay for its footprint? Measures, over the soak corpus, how many cells a reacting
//! pair VACATED walking to its meeting point — the corridor it could have retained as
//! claimed area — against the number of cells that reaction's blocklet actually needs.
//!
//! No engine changes: the walk counts are replayed from the event log, so this refutes
//! or supports the whole rung before a line of it is built. If corridors are
//! systematically shorter than footprints, the idea dies here and cheaply.
use rust_ca_lattice::blocklet::layout;
use rust_ca_lattice::cascade::Cell;
use rust_ca_lattice::cascade_run::{load_net, load_net_tree, Discipline, Event, Runner};
use rust_ca_lattice::lattice::{step, Pos, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use rust_ca_lattice::rules::{find_index, RULES};
use std::collections::BTreeMap;

/// Cells this rule's blocklet must place beyond the two seed cells themselves.
fn footprint(rule: u8) -> usize {
    let l = layout(rule);
    if l.seated.is_some() {
        0
    } else {
        l.extras.len()
    }
}

fn main() {
    let mut rng = Lcg(20260730);
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    // (corridor cells the pair vacated, cells its blocklet needs), one row per reaction.
    let mut fired: Vec<(usize, usize, u8)> = vec![];
    let mut declined: Vec<(usize, usize, u8)> = vec![];
    let mut near_empty: Vec<(usize, usize)> = vec![];
    for i in 0..160u32 {
        let term = rng.rand_term(3 + (i % 4));
        if oracle::nf(term.clone(), &mut Fuel(5_000)).is_err() {
            continue;
        }
        let discipline = disciplines[((i / 8) % 4) as usize];
        let tree = (i / 4) % 2 == 1;
        let mut shadow = Net::new();
        let root = shadow.build(&term);
        let (_nrm, _out) = shadow.drive(root);
        let grid = if tree {
            load_net_tree(&shadow, Topo::Full3D).expect("loads")
        } else {
            load_net(&shadow, Topo::Full3D).expect("loads")
        };
        let mut r = Runner::new(grid, shadow, discipline);
        if !r.run(4_000_000) {
            continue;
        }
        // Replay the log, carrying each agent's vacated-cell count along its moves.
        let mut walked: BTreeMap<Pos, usize> = BTreeMap::new();
        for e in &r.events {
            match e {
                Event::Move(from, to) => {
                    let n = walked.remove(from).unwrap_or(0) + 1;
                    // A swap moves two agents at once; the other's count arrives on its
                    // own Move, so accumulate rather than overwrite.
                    *walked.entry(*to).or_insert(0) += n;
                }
                Event::Dock(t, p, rule) => {
                    let corridor =
                        walked.get(t).copied().unwrap_or(0) + walked.get(p).copied().unwrap_or(0);
                    fired.push((corridor, footprint(*rule), *rule));
                }
                _ => {}
            }
        }
        // Pairs still facing each other at quiescence: the declined-dock class, the one
        // corridor-as-claim is aimed at. Their corridor is what they walked to get here.
        let sites: Vec<(Pos, _)> = r.grid.cells.keys().map(|q| (*q, r.grid.site(*q))).collect();
        for (p, s) in &sites {
            let Cell::Agent { tag, principal, nursery: false, .. } = &s.cell else { continue };
            if !tag.is_producer() {
                continue;
            }
            let t = step(*p, principal.face);
            if let Cell::Agent { tag: ct, principal: cp, nursery: false, .. } =
                &r.grid.site(t).cell
            {
                if ct.is_consumer()
                    && cp.face == principal.face.opp()
                    && cp.lane == principal.lane
                {
                    if let Some(rule) = find_index(*ct, *tag) {
                        let corridor = walked.get(p).copied().unwrap_or(0)
                            + walked.get(&t).copied().unwrap_or(0);
                        declined.push((corridor, footprint(rule as u8), rule as u8));
                        // How much genuinely free space sits around this stuck dock
                        // right now? "Cells vacated" is an upper bound on what could
                        // have been retained; this is the lower bound on what is
                        // available without retaining anything. The two together say
                        // whether the problem is scarcity or routing.
                        let mut empty = 0;
                        for dx in -3i32..=3 {
                            for dy in -3i32..=3 {
                                for dz in -3i32..=3 {
                                    let q = (p.0 + dx, p.1 + dy, p.2 + dz);
                                    if !r.grid.cells.contains_key(&q) {
                                        empty += 1;
                                    }
                                }
                            }
                        }
                        near_empty.push((empty, footprint(rule as u8)));
                    }
                }
            }
        }
    }
    for (label, rows) in [("FIRED", &fired), ("DECLINED (the target class)", &declined)] {
        if rows.is_empty() {
            println!("\n{label}: none");
            continue;
        }
        let growing: Vec<&(usize, usize, u8)> = rows.iter().filter(|(_, f, _)| *f > 0).collect();
        let covered = growing.iter().filter(|(c, f, _)| c >= f).count();
        let mut corr: Vec<usize> = growing.iter().map(|(c, _, _)| *c).collect();
        corr.sort_unstable();
        let median = corr.get(corr.len() / 2).copied().unwrap_or(0);
        println!(
            "\n{label}: {} reactions, {} need growth",
            rows.len(),
            growing.len()
        );
        println!(
            "  corridor covers the footprint: {covered}/{} ({:.0}%)",
            growing.len(),
            100.0 * covered as f64 / growing.len().max(1) as f64
        );
        println!("  median corridor: {median} cells");
        let mut by_rule: BTreeMap<u8, (usize, usize, usize)> = BTreeMap::new();
        for (c, f, rule) in growing {
            let e = by_rule.entry(*rule).or_insert((0, 0, *f));
            e.0 += 1;
            if c >= f {
                e.1 += 1;
            }
        }
        for (rule, (n, ok, f)) in by_rule {
            let r = &RULES[rule as usize];
            println!(
                "    {:<10} needs {f:>2} cells: {ok}/{n} funded",
                format!("{}·{}", r.consumer.name(), r.producer.name())
            );
        }
    }
    if !near_empty.is_empty() {
        let n = near_empty.len();
        let enough = near_empty.iter().filter(|(e, f)| e >= f).count();
        let mut v: Vec<usize> = near_empty.iter().map(|(e, _)| *e).collect();
        v.sort_unstable();
        println!(
            "\nfree space ALREADY around each stuck dock (radius 3): median {} empty cells; {enough}/{n} already have room for the footprint without claiming anything",
            v[v.len() / 2]
        );
    }

}
