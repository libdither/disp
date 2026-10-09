//! Several reductions side by side under one root (the player's `a; b`, wasm.rs `parse_all`): the
//! tuple `F(a, F(b, c))` runs on the lattice, each part is read back as soon as it is in
//! (`Lattice::read_part`), and each is the oracle's answer for that part alone.

use rust_ca_lattice::oracle::{self, f2, Fuel, Term};
use rust_ic_mesh::term;
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::share;

#[test]
fn parts_finish_on_their_own_and_each_is_its_answer() {
    let parts: Vec<Term> = vec![oracle::disp_t(), term::workload("sort", 1).unwrap(), oracle::chain_k(3)];
    let want: Vec<String> = parts.iter().map(|t| oracle::show(&oracle::nf(t.clone(), &mut Fuel(50_000_000)).unwrap())).collect();
    let tuple = parts.iter().rev().skip(1).fold(parts.last().unwrap().clone(), |acc, p| f2(p.clone(), acc));
    // A grid as the player sizes it, grown until the drawing fits.
    let t = (&tuple).into();
    let mut side = ((share::net_sup(&t, 0).0.agents.len() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let mut l = loop {
        let (net, out) = share::net_sup(&t, 0);
        match Lattice::load(Params { w: side, h: side, ..latest() }, net, out) { Ok(l) => break l, Err(_) => side = side * 7 / 5 }
    };
    let n = parts.len();
    let mut first: Vec<Option<f64>> = vec![None; n];
    while !l.answered() {
        let at = l.stats.proposals + 20_000;
        l.run(at);
        for i in 0..n {
            if first[i].is_none() && l.read_part(i, n).is_some() { first[i] = Some(l.stats.clocks); }
        }
        assert!(l.stats.clocks < 200_000.0, "unfinished");
    }
    for i in 0..n {
        let got = l.read_part(i, n).and_then(|t| t.plain()).map(|t| oracle::show(&t));
        assert_eq!(got.as_deref(), Some(want[i].as_str()), "part {i}");
    }
    // The parts run at once, so the small ones are in well before sort(1).
    let whole = l.stats.clocks;
    println!("parts in at {first:?}, the whole at {whole}");
    assert!(first[0].unwrap() < 0.5 * whole && first[2].unwrap() < 0.5 * whole, "the small parts waited for sort(1)");
}
