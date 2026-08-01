//! Why do parked soak runs park? Classifies every non-completing run in the soak corpus
//! by the shape of its stuck state, so capability work is aimed by measurement rather
//! than by whichever term was looked at last.
use rust_ca_lattice::cascade::{Cell, EndPt, Site};
use rust_ca_lattice::cascade_run::{load_net, load_net_tree, Discipline, Runner};
use rust_ca_lattice::lattice::Pos;
use rust_ca_lattice::lattice::{step, Topo};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg};
use std::collections::BTreeMap;

fn main() {
    let mut rng = Lcg(20260730);
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    let mut classes: BTreeMap<&str, Vec<u32>> = BTreeMap::new();
    for i in 0..160u32 {
        let term = rng.rand_term(3 + (i % 4));
        if oracle::nf(term.clone(), &mut Fuel(5_000)).is_err() {
            continue;
        }
        let discipline = disciplines[((i / 8) % 4) as usize];
        let tree = (i / 4) % 2 == 1;
        let mut shadow = Net::new();
        let root = shadow.build(&term);
        let (_nrm, out) = shadow.drive(root);
        let grid = if tree {
            load_net_tree(&shadow, Topo::Full3D).expect("loads")
        } else {
            load_net(&shadow, Topo::Full3D).expect("loads")
        };
        let mut r = Runner::new(grid, shadow, discipline);
        if !r.run(4_000_000) {
            classes.entry("NO QUIESCENCE").or_default().push(i);
            continue;
        }
        let answered = r.shadow.readback(r.shadow.get(out).ports[0]).is_some();
        if answered && r.grid.seed_sids.is_empty() {
            continue; // complete
        }
        let sites: Vec<_> = r.grid.cells.keys().map(|p| (*p, r.grid.site(*p))).collect();
        let cursors = sites.iter().filter(|(_, s)| s.cursor.is_some()).count();
        let seeds = sites.iter().filter(|(_, s)| matches!(s.cell, Cell::Seed { .. })).count();
        // A facing consumer/producer pair that never docked.
        let facing = sites.iter().any(|(p, s)| {
            let Cell::Agent { tag, principal, nursery: false, .. } = &s.cell else { return false };
            if !tag.is_producer() {
                return false;
            }
            matches!(&r.grid.site(step(*p, principal.face)).cell,
                Cell::Agent { tag: ct, principal: cp, nursery: false, .. }
                if ct.is_consumer() && cp.face == principal.face.opp()
                    && cp.lane == principal.lane)
        });
        // A genuinely wedged producer: its principal target is not wire it can walk into,
        // AND it is not simply DELIVERED — a producer whose principal meets a consumer's
        // aux (or any non-principal port) is an argument at rest, exactly where it
        // belongs, not a wedge. Counting those was hiding the real distribution.
        let wedged = |p: &Pos, s: &Site| {
            let Cell::Agent { tag, principal, nursery: false, .. } = &s.cell else { return false };
            if !tag.is_producer() {
                return false;
            }
            let t = step(*p, principal.face);
            let target = r.grid.site(t);
            match &target.cell {
                Cell::Wire { .. } => false,
                Cell::Agent { principal: tp, aux, pass, .. } => {
                    let back = EndPt {
                        face: principal.face.opp(),
                        lane: principal.lane,
                    };
                    // Delivered: the cable ENDS on one of the blocking agent's own
                    // ports, so this producer is an argument at rest. A passthrough is
                    // not delivery — the cable continues through, so the producer still
                    // has to get past this cell.
                    let _ = pass;
                    let delivered = *tp == back || aux.iter().any(|a| *a == back);
                    !delivered
                }
                _ => true,
            }
        };
        let walk_blocked = sites.iter().any(|(p, s)| wedged(p, s));
        // Of the walk wedges: is the blocked producer arity-1 (a leaf or eraser, the
        // teleport candidate), and is the thing in its way an agent?
        if walk_blocked {
            for (p, s) in &sites {
                let Cell::Agent { tag, principal, nursery: false, .. } = &s.cell else { continue };
                if !wedged(p, s) {
                    continue;
                }
                let target = r.grid.site(step(*p, principal.face));
                let key = match (tag.arity() == 1, &target.cell) {
                    (true, Cell::Agent { .. }) => "  wedge detail: arity-1 blocked BY AN AGENT",
                    (false, Cell::Agent { .. }) => "  wedge detail: arity-2+ blocked by an agent",
                    (true, _) => "  wedge detail: arity-1 blocked by other",
                    (false, _) => "  wedge detail: arity-2+ blocked by other",
                };
                classes.entry(key).or_default().push(i);
            }
        }
        let class = if cursors > 0 {
            "growth wedge (cursor stalled mid-script)"
        } else if seeds > 0 {
            "seed stalled (docked, no cursor)"
        } else if facing {
            "dock declined (facing pair, never docked)"
        } else if walk_blocked {
            "walk wedge (producer blocked, no dock)"
        } else {
            "quiet park (no live pair, answer incomplete)"
        };
        classes.entry(class).or_default().push(i);
    }
    // Detail rows count blocked AGENTS, not runs, so they are summarised separately.
    let total: usize =
        classes.iter().filter(|(k, _)| !k.starts_with("  ")).map(|(_, v)| v.len()).sum();
    println!("parked runs: {total}");
    for (k, v) in &classes {
        println!("  {:<44} {:>3}   e.g. {:?}", k, v.len(), &v[..v.len().min(6)]);
    }
}
