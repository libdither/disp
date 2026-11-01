//! The linear two-way net (tree-net lin.rs) on the lattice: each case of lin-fixtures.txt (written by
//! ../lin/fixtures.mts from the TypeScript compiler, with core.ts's answers) runs to the interpreter's
//! answer, the lattice matching the abstract net wire for wire; and flipped halfway, it runs back to the
//! input.
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::linear;

fn cases() -> Vec<(String, String, String, String)> {
    include_str!("lin-fixtures.txt").lines().map(|l| {
        let v: Vec<&str> = l.split('\t').collect();
        (v[0].into(), v[1].into(), v[2].into(), v[3].into())
    }).collect()
}

fn lattice(src: &str, seed: u64) -> (Lattice, u32) {
    let (net, out) = linear::parse(src).expect("parses");
    let side = ((net.live_count() as f64).sqrt() * 4.0).ceil() as u32 + 12;
    let p = Params { two: true, lazy: false, seed, w: side, h: side, ..latest() };
    (Lattice::load(p, net, out).expect("loads"), out)
}

#[test]
fn runs_to_the_interpreters_answer() {
    for (name, src, _, want) in cases() {
        let (mut l, out) = lattice(&src, 1);
        assert!(l.run(400_000_000), "{name}: did not finish");
        l.check_projection().unwrap();
        let got = linear::answer(&l.shadow, out);
        assert_eq!(got, Some((want.clone(), true)), "{name}: stuck on {:?}", linear::stuck(&l.shadow));
        eprintln!("{name:>14}: {:>7.0} clocks, {:>5} rewrites, {:>5} agents", l.stats.clocks, l.stats.fires, l.shadow.live_count());
    }
}

#[test]
fn flipped_halfway_runs_back() {
    for (name, src, input, _) in cases() {
        let (mut l, _) = lattice(&src, 2);
        l.run(400_000_000);
        let half = l.stats.proposals / 2;
        let (mut l, out) = lattice(&src, 2);
        l.run(half);
        l.flip_lin();
        l.check_projection().unwrap();
        assert!(l.run(800_000_000), "{name}: did not finish back");
        l.check_projection().unwrap();
        assert_eq!(linear::answer(&l.shadow, out), Some((input.clone(), false)), "{name}: stuck on {:?}", linear::stuck(&l.shadow));
    }
}
