use rust_ca_lattice::lattice::TOPOS;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Term};
use rust_ca_lattice::scheduler::{run_term_with, CheckLevel, FireMode};
fn main() {
    for topo in TOPOS {
        for (mode, ml) in [(FireMode::Search, "search"), (FireMode::Grow, "grow"), (FireMode::GrowThenSearch, "grow+search")] {
            println!("== {} · {} ==", topo.name(), ml);
            let t = |name: &str, term: Term| {
                let want = oracle::nf(term.clone(), &mut Fuel(100_000)).map(|w| oracle::show(&w)).unwrap_or("DIV".into());
                let out = run_term_with(&term, topo, mode, 30_000, CheckLevel::Tick);
                println!("{name}: done={} ticks={} ints={} transport={} {}", out.done, out.ticks, out.ints, out.transport,
                    if out.done { format!("nf={} ok={}", out.result.as_ref().map(oracle::show).unwrap_or_default(),
                        out.result.as_ref().map(oracle::show).as_deref() == Some(want.as_str())) } else { "STUCK".into() });
            };
            t("fork-dispatch", ap(f2(Term::L, Term::L), Term::L));
            t("K-simple", ap(ap(oracle::k(), Term::L), s(Term::L)));
            t("K-args", ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))));
            t("chain1", oracle::chain_k(1));
            t("chain2", oracle::chain_k(2));
            t("chain3", oracle::chain_k(3));
            t("stem-app", ap(s(Term::L), Term::L));
            t("share-small", ap(f2(s(Term::L), Term::L), Term::L));
            t("erase-fork", ap(f2(Term::L, Term::L), f2(Term::L, Term::L)));
            t("selF", ap(f2(f2(Term::L, Term::L), Term::L), f2(Term::L, Term::L)));
        }
    }
}
