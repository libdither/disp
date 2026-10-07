//! Building the loaded term with its equal parts shared (share.rs) keeps its meaning: on the lattice
//! the answer is the oracle's, from fewer agents at load.

use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::share;

fn run(t: &Term, min: usize, seed: u64) -> (Option<String>, usize) {
    let (net, out) = share::net(t, min);
    let agents = net.live_count();
    let mut l = Lattice::load(Params { w: 72, h: 72, depth: 8, seed, ..latest() }, net, out).expect("load");
    l.run(400_000_000);
    (l.readback().map(|t| oracle::show(&t)), agents)
}

#[test]
fn shared_parts_keep_the_answer() {
    let mut rng = Lcg(20261007);
    let mut terms: Vec<(String, Term)> = (0..40).map(|i| (format!("corpus {i}"), rng.rand_term(4 + i % 3))).collect();
    for (name, n) in [("share-tower", 3), ("s-rule", 0), ("disp-t", 0), ("convoy", 2)] {
        terms.push((name.into(), rust_ic_mesh::term::workload(name, n).unwrap()));
    }
    let (mut checked, mut fewer) = (0, 0);
    for (name, t) in terms {
        let Ok(want) = oracle::nf(t.clone(), &mut Fuel(1_000_000)) else { continue };
        let want = oracle::show(&want);
        let (_, apart) = run(&t, 0, 1);
        for min in [1, 3] {
            let (got, agents) = run(&t, min, 2);
            assert_eq!(got.as_deref(), Some(want.as_str()), "{name}, shared from {min} agents up");
            assert!(agents <= apart, "{name}: {agents} agents shared, {apart} apart");
            if agents < apart { fewer += 1; }
            checked += 1;
        }
    }
    assert!(checked >= 60 && fewer >= 10, "{checked} runs, {fewer} with fewer agents");
}
