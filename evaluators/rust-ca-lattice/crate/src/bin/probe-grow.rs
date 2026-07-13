//! Event-census probe: how often each mechanism engages on a stuck pin.
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::oracle::{self, ap, f2, s, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Event, FireMode, Sim};
fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "chain2".into());
    let term = match which.as_str() {
        "chain2" => oracle::chain_k(2),
        "chain3" => oracle::chain_k(3),
        "kargs" => ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
        "selF" => ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)),
        _ => panic!(),
    };
    let mut sim = Sim::load(&term, Topo::Full3D);
    sim.fire_mode = match std::env::args().nth(2).as_deref() {
        Some("search") => FireMode::Search,
        Some("grow") => FireMode::Grow,
        _ => FireMode::GrowThenSearch,
    };
    let (mut fires, mut docks, mut grows, mut shoves, mut slides) = (0u32, 0u32, 0u32, 0u32, 0u32);
    let mut zero = 0;
    for t in 0..3000 {
        if sim.shadow.all_active_pairs().is_empty() && !sim.grid.has_seeds() { println!("DONE tick {t}"); break; }
        let n = sim.tick(CheckLevel::Tick);
        for e in &sim.events {
            match e {
                Event::Fire { .. } => fires += 1,
                Event::Dock { .. } => docks += 1,
                Event::Grow { .. } => grows += 1,
                Event::Shove { .. } => shoves += 1,
                Event::Slide { .. } => slides += 1,
                _ => {}
            }
        }
        if n == 0 { zero += 1; if zero > 60 { println!("STUCK tick {t}"); break; } } else { zero = 0; }
    }
    println!("fires={fires} docks={docks} grow_steps={grows} shoves={shoves} slides={slides}");
    println!("chi cells={} max={:?} reserved={} seeds={}",
        sim.grid.chi.len(), sim.grid.chi.values().max(), sim.grid.reserved.len(), sim.grid.seed_count);
    // autopsy: every hot-blocked producer and its neighborhood
    use rust_ca_lattice::lattice::{step, DIRS};
    use rust_ca_lattice::transitions::plan_reel;
    for (p, a) in sim.grid.agents().map(|(p, a)| (p, a.clone())).collect::<Vec<_>>() {
        if !a.tag.is_producer() || a.nascent { continue; }
        let Some(d) = a.faces[0] else { continue };
        let q = step(p, d);
        let hot = sim.grid.wire(q).and_then(|w| w.with_he(d.opp())).map(|m| m.hot).unwrap_or(false);
        if !hot || plan_reel(&sim.grid, p).is_some() { continue; }
        println!("BLOCKED {} at {:?} faces {:?} chi(self)={}", a.tag.name(), p, &a.faces[..a.tag.arity()], sim.grid.chi_at(p));
        println!("  npos {:?}: {:?}", q, sim.grid.cells.get(&q));
        if let Some(w) = sim.grid.wire(q) {
            use rust_ca_lattice::transitions::plan_slide;
            for s in w.iter() {
                let (na, nb) = (step(q, s.a), step(q, s.b));
                println!("  strand {:?}: slide={:?}", s, plan_slide(&sim.grid, q, s, &[], false).map(|pl| pl.strands.len()));
                println!("    na {:?}: {:?}", na, sim.grid.cells.get(&na));
                println!("    nb {:?}: {:?}", nb, sim.grid.cells.get(&nb));
            }
        }
        for dd in DIRS {
            let n = step(p, dd);
            println!("    {:?} {:?} chi={} : {:?}", dd, n, sim.grid.chi_at(n), sim.grid.cells.get(&n));
        }
        // full radius-2 map around npos: . empty  # agent  n strand-count  R reserved
        for dz in -1i32..=1 {
            println!("  z={}:", q.2 + dz);
            for dy in -2i32..=2 {
                let mut row = String::from("    ");
                for dx in -2i32..=2 {
                    let c = (q.0 + dx, q.1 + dy, q.2 + dz);
                    let ch = if sim.grid.reserved.contains_key(&c) { 'R' }
                        else { match sim.grid.cells.get(&c) {
                            None => '.',
                            Some(rust_ca_lattice::lattice::Cell::Agent(_)) => '#',
                            Some(rust_ca_lattice::lattice::Cell::Wire(w)) => char::from_digit(w.count() as u32, 10).unwrap(),
                            Some(rust_ca_lattice::lattice::Cell::Seed(_)) => 'S',
                        } };
                    row.push(ch); row.push(' ');
                }
                println!("{row}");
            }
        }
        break; // first one is enough
    }
}
