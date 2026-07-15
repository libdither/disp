//! End-state analyzer: run a term through the liveness grace/cap and explain remaining pairs.
use rust_ca_lattice::lattice::{he_opp, manhattan, step, Topo};
use rust_ca_lattice::oracle::{self, ap, f2, s, Lcg, Term};
use rust_ca_lattice::scheduler::{CheckLevel, Sim};
use rust_ca_lattice::transitions::{plan_agent_step, plan_fire, plan_grow_dock, plan_reel};

fn main() {
    let which = std::env::args().nth(1).unwrap_or_else(|| "k".into());
    let topo = match std::env::args().nth(2).as_deref() {
        Some("full3d") => Topo::Full3D,
        _ => Topo::Bilayer,
    };
    let term = if let Some(raw) = which.strip_prefix("corpus") {
        let index: u32 = raw.parse().expect("corpus term must be written as corpus<index>");
        let mut rng = Lcg(999);
        (0..=index).map(|i| rng.rand_term(3 + (i % 3))).last().unwrap()
    } else { match which.as_str() {
        "k" => ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))),
        "fork" => ap(f2(Term::L, Term::L), Term::L),
        "stem" => ap(s(Term::L), Term::L),
        "chain" => oracle::chain_k(4),
        "chain1" => oracle::chain_k(1),
        "chain2" => oracle::chain_k(2),
        "chain3" => oracle::chain_k(3),
        "disp" => oracle::disp_t(),
        "share" => ap(f2(s(Term::L), Term::L), f2(Term::L, Term::L)),
        "share2" => ap(f2(s(Term::L), Term::L), Term::L),
        "ksimple" => ap(ap(oracle::k(), Term::L), s(Term::L)),
        "selF" => ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)),
        _ => panic!("k|fork|stem|chain|chain1|chain2|chain3|disp|share|share2|ksimple|selF|corpus<index>"),
    }};
    let mut sim = Sim::load(&term, topo);
    let cap = std::env::var("DEBUG_CAP").ok().and_then(|s| s.parse().ok()).unwrap_or(50_000);
    let mut ticks = 0u64;
    let mut zero = 0u32;
    loop {
        if sim.shadow.all_active_pairs().is_empty() { println!("DONE in {ticks} ticks"); return; }
        // one quiet tick is not a stall under the fields; autopsy the true fixpoint
        let applied = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            sim.tick(CheckLevel::Every)
        })).unwrap_or_else(|failure| {
            eprintln!("tick panicked at {ticks}; events: {:?}", sim.events);
            for &p in sim.grid.cells.keys() {
                for h in sim.grid.used_hes(p) {
                    let q = step(p, h);
                    if !sim.grid.used_hes(q).contains(&he_opp(h)) {
                        eprintln!("broken {p:?}.{h:?} -> {q:?}");
                        for (label, r) in [("source", p), ("target", q)] {
                            eprintln!("  {label} {r:?}: cell={:?} motion={:?} chi={} sigma={}",
                                sim.grid.cells.get(&r), sim.grid.motion.get(&r),
                                sim.grid.chi_at(r), sim.grid.sigma_at(r));
                            for d in rust_ca_lattice::lattice::DIRS {
                                let n = step(r, d);
                                eprintln!("    {d:?} {n:?}: cell={:?} motion={:?} chi={} sigma={}",
                                    sim.grid.cells.get(&n), sim.grid.motion.get(&n),
                                    sim.grid.chi_at(n), sim.grid.sigma_at(n));
                            }
                        }
                    }
                }
            }
            std::panic::resume_unwind(failure)
        });
        if applied == 0 { zero += 1; if zero > 60 { break; } } else { zero = 0; }
        ticks += 1;
        if ticks > cap { println!("cap"); break; }
    }
    if std::env::var("MOTION_TRACE").is_ok() {
        for i in 0..30 {
            let n = sim.tick(CheckLevel::Every);
            println!("motion+{i}: applied={n} marks={:?} events={:?}", sim.grid.motion, sim.events);
        }
    }
    println!("STUCK at tick {ticks} on {}. remaining pairs:", topo.name());
    for (p, mark) in &sim.grid.motion {
        if let rust_ca_lattice::local::MotionMark::BulgeSource { spec, .. } = mark {
            let spine = rust_ca_lattice::lattice::step(*p, spec.side);
            println!("  bulge source {p:?} {spec:?}:");
            for (label, q) in [
                ("spine", spine),
                ("plus corner", rust_ca_lattice::lattice::step(spine, spec.axis)),
                ("minus corner", rust_ca_lattice::lattice::step(spine, spec.axis.opp())),
                ("plus endpoint", rust_ca_lattice::lattice::step(*p, spec.axis)),
                ("minus endpoint", rust_ca_lattice::lattice::step(*p, spec.axis.opp())),
            ] {
                println!("    {label} {q:?}: chi={} motion={:?} {:?}",
                    sim.grid.chi_at(q), sim.grid.motion.get(&q), sim.grid.cells.get(&q));
            }
        }
    }
    for (c, p) in sim.shadow.all_active_pairs() {
        let (ct, pt) = (sim.shadow.get(c).tag, sim.shadow.get(p).tag);
        let cp = sim.grid.agents().find(|(_, a)| a.sid == c).map(|(q, _)| q);
        let pp = sim.grid.agents().find(|(_, a)| a.sid == p).map(|(q, _)| q);
        let dist = match (cp, pp) { (Some(a), Some(b)) => manhattan(a, b), _ => -1 };
        println!("  {}#{c}@{cp:?} · {}#{p}@{pp:?}  dist={dist}", ct.name(), pt.name());
        if let (Some(cq), Some(pq)) = (cp, pp) {
            if dist == 1 {
                println!("    plan_fire -> {:?}", plan_fire(&sim.grid, cq).map(|pl| pl.fresh_cells.clone()));
                println!("    plan_grow_dock -> {:?}", plan_grow_dock(&sim.grid, cq).map(|pl| pl.cells));
                println!("    motion marks -> {:?}", sim.grid.motion);
                for dz in 0..=1i32 {
                    println!("    plane z={}", cq.2 + dz);
                    for dy in -2..=3i32 { for dx in -2..=2i32 {
                        let p = (cq.0 + dx, cq.1 + dy, cq.2 + dz);
                        match sim.grid.cells.get(&p) {
                            None => println!("      {p:?} ."),
                            Some(c) => println!("      {p:?} chi={} agent_step={:?} {c:?}",
                                sim.grid.chi_at(p), plan_agent_step(&sim.grid, p).map(|s| s.npos)),
                        }
                    } }
                }
            } else {
                println!("    plan_reel(producer) -> {:?}", plan_reel(&sim.grid, pq).map(|r| r.npos));
                let ag = sim.grid.agent(pq).unwrap();
                println!("    producer faces {:?}", &ag.faces[..ag.tag.arity()]);
                println!("    producer chi={} motion={:?}", sim.grid.chi_at(pq), sim.grid.motion.get(&pq));
                for d in rust_ca_lattice::lattice::DIRS {
                    let n = rust_ca_lattice::lattice::step(pq, d);
                    println!("      {d:?}: chi={} motion={:?} {:?}",
                        sim.grid.chi_at(n), sim.grid.motion.get(&n), sim.grid.cells.get(&n));
                }
                use rust_ca_lattice::transitions::{plan_slide, reel_blocked};
                if let Some(npos) = reel_blocked(&sim.grid, pq) {
                    println!("    blocked target {npos:?} chi={} motion={:?}",
                        sim.grid.chi_at(npos), sim.grid.motion.get(&npos));
                    for d in rust_ca_lattice::lattice::DIRS {
                        let n = rust_ca_lattice::lattice::step(npos, d);
                        println!("      target.{d:?} {n:?}: in_bounds={} chi={} motion={:?} {:?}",
                            sim.grid.topo.in_bounds(n), sim.grid.chi_at(n),
                            sim.grid.motion.get(&n), sim.grid.cells.get(&n));
                    }
                    let mine = ag.faces[0]
                        .and_then(|d| sim.grid.wire(npos).and_then(|w| w.with_he(d.opp())));
                    let blockers = sim.grid.wire(npos).into_iter()
                        .flat_map(|w| w.iter()).filter(|s| Some(*s) != mine);
                    for s in blockers {
                        println!("    PRESSURED target blocker {s:?} at {npos:?}:");
                        println!("      slide -> {:?}", plan_slide(&sim.grid, npos, s, &[]).map(|p| p.strands.iter().map(|(c, _)| *c).collect::<Vec<_>>()));
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
