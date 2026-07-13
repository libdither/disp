//! Stall analyzer: run a term to the first stuck tick, then explain every remaining pair.
use rust_ca_lattice::lattice::{manhattan, Topo};
use rust_ca_lattice::oracle::{self, ap, f2, s, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Sim};
use rust_ca_lattice::transitions::{plan_fire, plan_reel};

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "k".into());
    let topo = match std::env::args().nth(2).as_deref() {
        Some("full3d") => Topo::Full3D,
        _ => Topo::Bilayer,
    };
    let term = match which.as_str() {
        "k" => ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
        "fork" => ap(f2(Term::L, Term::L), Term::L),
        "chain" => oracle::chain_k(4),
        "disp" => oracle::disp_t(),
        "share" => ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
        "share2" => ap(f2(s(Term::L), Term::L), Term::L),
        "ksimple" => ap(ap(oracle::k(), Term::L), s(Term::L)),
        _ => panic!("k|chain|disp|share|share2|ksimple"),
    };
    let mut sim = Sim::load(&term, topo);
    let mut ticks = 0u64;
    loop {
        if sim.shadow.all_active_pairs().is_empty() { println!("DONE in {ticks} ticks"); return; }
        if sim.tick(CheckLevel::Tick) == 0 { break; }
        ticks += 1;
        if ticks > 50_000 { println!("cap"); return; }
    }
    println!("STUCK at tick {ticks} on {}. remaining pairs:", topo.name());
    for (c, p) in sim.shadow.all_active_pairs() {
        let (ct, pt) = (sim.shadow.get(c).tag, sim.shadow.get(p).tag);
        let cp = sim.grid.agents().find(|(_, a)| a.sid == c).map(|(q, _)| q);
        let pp = sim.grid.agents().find(|(_, a)| a.sid == p).map(|(q, _)| q);
        let dist = match (cp, pp) { (Some(a), Some(b)) => manhattan(a, b), _ => -1 };
        println!("  {}#{c}@{cp:?} · {}#{p}@{pp:?}  dist={dist}", ct.name(), pt.name());
        if let (Some(cq), Some(pq)) = (cp, pp) {
            if dist == 1 {
                println!("    plan_fire -> {:?}", plan_fire(&sim.grid, cq).map(|pl| pl.fresh_cells.clone()));
                for dz in 0..=1i32 {
                    println!("    plane z={}", cq.2 + dz);
                    for dy in -2..=3i32 { for dx in -2..=2i32 {
                        let p = (cq.0 + dx, cq.1 + dy, cq.2 + dz);
                        match sim.grid.cells.get(&p) {
                            None => println!("      {p:?} ."),
                            Some(c) => println!("      {p:?} {c:?}"),
                        }
                    } }
                }
            } else {
                println!("    plan_reel(producer) -> {:?}", plan_reel(&sim.grid, pq).map(|r| r.npos));
                let ag = sim.grid.agent(pq).unwrap();
                println!("    producer faces {:?}", &ag.faces[..ag.tag.arity()]);
                for d in rust_ca_lattice::lattice::DIRS {
                    let n = rust_ca_lattice::lattice::step(pq, d);
                    println!("      {d:?}: {:?}", sim.grid.cells.get(&n));
                }
                use rust_ca_lattice::transitions::{plan_slide, reel_blocked};
                if let Some((npos, blockers)) = reel_blocked(&sim.grid, pq) {
                    for s in blockers {
                        println!("    CLEAR target {s:?} at {npos:?}:");
                        println!("      slide -> {:?}", plan_slide(&sim.grid, npos, s, &[], false).map(|p| p.strands.iter().map(|(c, _)| *c).collect::<Vec<_>>()));
                        for (lbl, c) in [("na", rust_ca_lattice::lattice::step(npos, s.a)), ("nb", rust_ca_lattice::lattice::step(npos, s.b))] {
                            println!("      {lbl} {c:?}: {:?}", sim.grid.cells.get(&c));
                        }
                    }
                } else {
                    println!("    reel_blocked -> None (not a shared-cell block)");
                }
            }
        }
    }
}
