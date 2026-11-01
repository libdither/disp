// A linear-net case on the lattice with a small budget: where it gets to.
use rust_ic_strands::lattice::{latest, Lattice, Params};
use rust_ic_strands::linear;
fn main() {
    let name = std::env::args().nth(1).unwrap_or("cross".into());
    let budget: u64 = std::env::args().nth(2).and_then(|s| s.parse().ok()).unwrap_or(2_000_000);
    let line = include_str!("../tests/lin-fixtures.txt").lines().find(|l| l.starts_with(&format!("{name}\t"))).expect("case");
    let v: Vec<&str> = line.split('\t').collect();
    let (net, out) = linear::parse(v[1]).unwrap();
    println!("loaded {} agents; active: {:?}", net.live_count(), net.all_active_pairs().iter().map(|&(c, p)| (net.get(c).tag.name(), net.get(p).tag.name())).collect::<Vec<_>>());
    let side = ((net.live_count() as f64).sqrt() * 4.0).ceil() as u32 + 12;
    let seed: u64 = std::env::args().nth(3).and_then(|s| s.parse().ok()).unwrap_or(1);
    let mut p = Params { two: true, lazy: false, seed, w: side, h: side, ..latest() };
    for kv in std::env::args().nth(4).unwrap_or_default().split(',').filter(|s| !s.is_empty()) {
        let (k, v) = kv.split_once('=').unwrap();
        p.set(k, v).unwrap();
    }
    println!("depth {}, k {}, margolus {}", p.depth, p.k, p.margolus);
    let mut l = Lattice::load(p, net, out).unwrap();
    let done = l.run(budget);
    println!("done {done}: {} clocks, {} rewrites, {} proposals", l.stats.clocks, l.stats.fires, l.stats.proposals);
    println!("answer {:?}; stuck {:?}", linear::answer(&l.shadow, out), linear::stuck(&l.shadow));
    let pairs: Vec<_> = l.shadow.all_active_pairs().iter().map(|&(c, p)| (l.shadow.get(c).tag.name(), l.shadow.get(p).tag.name())).collect();
    println!("active {:?}", pairs);
    let at = |id: u32| (0..l.sites()).flat_map(|s| (0..l.ks).map(move |k| (s, k))).find(|&(s, k)| l.tags[s as usize * l.ks + k] != 0 && l.sids[s as usize * l.ks + k] == id).map(|(s, _)| (s % side, s / side));
    for (c, p) in l.shadow.all_active_pairs() { println!("  {} at {:?}, {} at {:?}", l.shadow.get(c).tag.name(), at(c), l.shadow.get(p).tag.name(), at(p)); }
    let seat = |id: u32| (0..l.sites()).flat_map(|s| (0..l.ks).map(move |k| (s, k))).find(|&(s, k)| l.tags[s as usize * l.ks + k] != 0 && l.sids[s as usize * l.ks + k] == id);
    for (c, _) in l.shadow.all_active_pairs() { if let Some((s, k)) = seat(c) {
        println!("  consumer step: {}; wanted {}", l.probe_step(s, k), l.want[s as usize * l.ks + k]);
        let m = l.mate_of(s, (k * l.ar) as u8);
        let f = (m as usize - l.ar * l.ks) / l.p.lanes;
        let t = l.nb(s, f);
        for j in 0..l.ks { let c = l.tags[t as usize * l.ks + j]; if c != 0 { println!("    next site holds {} wanted {}", rust_ic_strands::lattice::tag_of(c).name(), l.want[t as usize * l.ks + j]); } }
    } }
    println!("blocked fires {}, walk ok {}, walk fail {:?}, side {side}, live {}", l.stats.blocked_fires, l.stats.walk_ok, l.stats.walk_fail, l.shadow.live_count());
}
