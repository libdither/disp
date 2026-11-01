// Flip a linear-net case on the lattice after k proposals, for several k, and run it back.
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::linear;
fn main() {
    let name = std::env::args().nth(1).unwrap_or("notw".into());
    let line = include_str!("../tests/lin-fixtures.txt").lines().find(|l| l.starts_with(&format!("{name}\t"))).expect("case");
    let v: Vec<&str> = line.split('\t').collect();
    let load = || {
        let (net, out) = linear::parse(v[1]).unwrap();
        let side = ((net.live_count() as f64).sqrt() * 4.0).ceil() as u32 + 12;
        (Lattice::load(Params { two: true, lazy: false, seed: 2, w: side, h: side, ..latest() }, net, out).unwrap(), out)
    };
    let (mut l, _) = load();
    l.run(u64::MAX / 4);
    let total = l.stats.proposals;
    for i in 1..10u64 {
        let k = total * i / 10;
        let (mut l, out) = load();
        l.run(k);
        let fires = l.stats.fires;
        l.flip_lin();
        l.check_projection().unwrap();
        l.run(k * 20 + 10_000_000);
        let got = linear::answer(&l.shadow, out);
        let runs: Vec<_> = l.shadow.agents.iter().flatten().filter(|a| tree_net::lin::runs(a.tag)).map(|a| a.tag.name()).collect();
        println!("flip after {k} proposals ({fires} rewrites): {:?}; stuck {:?}; carrying a run: {:?}", got.map(|(v2, f)| (v2 == v[2], f)), linear::stuck(&l.shadow), runs);
    }
}
