//! Stall analyzer: run a term to the first stuck tick, then explain every remaining pair.
use rust_ca_lattice::oracle::{self, ap, f2, s, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Sim};
use rust_ca_lattice::transitions::{plan_fire, plan_reel};

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "k".into());
    let term = match which.as_str() {
        "k" => ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
        "chain" => oracle::chain_k(4),
        "disp" => oracle::disp_t(),
        "share" => ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
        "share2" => ap(f2(s(Term::L), Term::L), Term::L),
        "ksimple" => ap(ap(oracle::k(), Term::L), s(Term::L)),
        _ => panic!("k|chain|disp|share"),
    };
    let mut sim = Sim::load(&term);
    let mut ticks = 0u64;
    loop {
        if sim.shadow.all_active_pairs().is_empty() { println!("DONE in {ticks} ticks"); return; }
        if sim.tick(CheckLevel::Tick) == 0 { break; }
        ticks += 1;
        if ticks > 50_000 { println!("cap"); return; }
    }
    println!("STUCK at tick {ticks}. remaining pairs:");
    std::env::set_var("TR_DEBUG", "1");
    for (c, p) in sim.shadow.all_active_pairs() {
        let (ct, pt) = (sim.shadow.get(c).tag, sim.shadow.get(p).tag);
        let cp = sim.grid.agents().find(|(_, a)| a.sid == c).map(|(q, _)| q);
        let pp = sim.grid.agents().find(|(_, a)| a.sid == p).map(|(q, _)| q);
        let dist = match (cp, pp) { (Some(a), Some(b)) => (a.0 - b.0).abs() + (a.1 - b.1).abs(), _ => -1 };
        println!("  {}#{c}@{cp:?} · {}#{p}@{pp:?}  dist={dist}", ct.name(), pt.name());
        if let (Some(cq), Some(pq)) = (cp, pp) {
            if dist == 1 {
                println!("    plan_fire -> {:?}", plan_fire(&sim.grid, cq).map(|pl| pl.fresh_cells.clone()));
                for dy in -2..=3i32 { for dx in -2..=2i32 {
                    let p = (cq.0 + dx, cq.1 + dy);
                    match sim.grid.cells.get(&p) {
                        None => println!("      {p:?} ."),
                        Some(c) => println!("      {p:?} {c:?}"),
                    }
                } }
            } else {
                println!("    plan_reel(producer) -> {:?}", plan_reel(&sim.grid, pq).map(|r| r.npos));
                // context: the producer's neighborhood
                let ag = sim.grid.agent(pq).unwrap();
                println!("    producer faces {:?} tuck {:?}", &ag.faces[..ag.tag.arity()], ag.tucks);
                for d in rust_ca_lattice::lattice::DIRS {
                    let n = rust_ca_lattice::lattice::step(pq, d);
                    println!("      {d:?}: {:?}", sim.grid.cells.get(&n));
                }
            }
        }
    }
}
