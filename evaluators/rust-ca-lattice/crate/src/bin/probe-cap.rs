//! Probe the TICK-CAPPED class: terms where the tick never goes quiet (χ/SHOVE churn
//! defeats the zero-applied stall detector) so the 20k budget binds. For each, re-run at
//! 100k to separate SLOW COMPLETERS (shadow ints keep growing; need budget) from CHURN
//! LOOPS (shadow frozen; need a shadow-progress stall window).
use rust_ca_lattice::lattice::TOPOS;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ca_lattice::scheduler::{run_term, CheckLevel};

fn main() {
    for topo in TOPOS {
        let mut rng = Lcg(999);
        let mut capped: Vec<(u32, Term, u64, u64)> = vec![];
        let (mut pass, mut stuck) = (0u32, 0u32);
        let n_terms = 400;
        for i in 0..n_terms {
            let term = rng.rand_term(3 + (i % 3));
            if oracle::nf(term.clone(), &mut Fuel(20_000)).is_err() { continue; }
            let out = run_term(&term, topo, 20_000, CheckLevel::End);
            if out.done { pass += 1; }
            else if out.stuck { stuck += 1; }
            else { capped.push((i, term, out.ints, out.transport)); }
        }
        println!("[{}] pass={pass} stuck={stuck} capped={}", topo.name(), capped.len());
        let (mut slow_done, mut frozen, mut crawling) = (0u32, 0u32, 0u32);
        for (i, term, ints20, tr20) in capped {
            let b = run_term(&term, topo, 100_000, CheckLevel::End);
            let verdict = if b.done { slow_done += 1; "SLOW-DONE" }
                else if b.ints == ints20 { frozen += 1; "FROZEN" }
                else { crawling += 1; "CRAWLING" };
            println!("  #{i} {verdict}: 20k ints={ints20} tr={tr20} | 100k done={} stuck={} ticks={} ints={} tr={} | {}",
                b.done, b.stuck, b.ticks, b.ints, b.transport, oracle::show(&term));
        }
        println!("[{}] capped breakdown: slow-done={slow_done} frozen={frozen} crawling={crawling}", topo.name());
    }
}
