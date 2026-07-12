use rust_ca_lattice::oracle::{self, ap, f2, s, Term, Fuel};
use rust_ca_lattice::scheduler::{run_term, CheckLevel};
fn t(name: &str, term: Term) {
    let want = oracle::nf(term.clone(), &mut Fuel(100_000)).map(|w| oracle::show(&w)).unwrap_or("DIV".into());
    let out = run_term(&term, 30_000, CheckLevel::Tick);
    println!("{name}: done={} ticks={} ints={} transport={} {}", out.done, out.ticks, out.ints, out.transport,
        if out.done { format!("nf={} ok={}", out.result.as_ref().map(oracle::show).unwrap_or_default(),
            out.result.as_ref().map(oracle::show).as_deref() == Some(want.as_str())) } else { "STUCK".into() });
}
fn main() {
    t("fork-dispatch", ap(f2(Term::L, Term::L), Term::L));
    t("K-simple", ap(ap(oracle::k(), Term::L), s(Term::L)));
    t("K-args", ap(ap(oracle::k(), f2(Term::L, s(Term::L))), s(s(Term::L))));
    t("chain1", oracle::chain_k(1));
    t("chain2", oracle::chain_k(2));
    t("chain3", oracle::chain_k(3));
    t("stem-app", ap(s(Term::L), Term::L));
    t("share-small", ap(f2(s(Term::L), Term::L), Term::L));
}
