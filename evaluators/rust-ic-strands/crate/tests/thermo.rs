//! The lattice run thermodynamically (thermo.rs): energy is kept exactly, the banks settle at the bath's
//! temperature, two-way programs give the reference's results, and with the drive turned round the run
//! goes back.

use tree_net::net::Agent;
use rust_ic_strands::lattice::{tag_of, thermal, Lattice, Params};
use rust_ic_strands::twoway::{self, add, nat, Prog, Val};

fn load(p: &Prog, x: &Val, seed: u64) -> (Lattice, Vec<Option<Agent>>, u32) {
    let (net, out) = twoway::net(p, x);
    let start = net.agents.clone();
    let side = ((net.live_count() as f64).sqrt() * 7.0).ceil() as u32 + 16;
    let l = Lattice::load(Params { w: side, h: side, seed, two: true, lazy: false, ..thermal() }, net, out).expect("load");
    (l, start, out)
}

/// The lattice's energy, the banks', the bath's and the fuel's add up to the same (looked at every 64
/// events: a slip stays in the sum), until every kind of event has happened.
#[test]
fn energy_is_kept() {
    let (mut l, _, _) = load(&add(), &Val::f(nat(1), nat(1)), 1);
    let kept = l.energy_kept();
    for i in 0..3_000_000 {
        l.event();
        if i % 64 > 0 { continue; }
        assert_eq!(l.energy_kept(), kept, "by event {i}: energy not kept (ledger {:?})", l.th.ledger);
        if l.th.ledger.made.iter().all(|&n| n > 0) { break; }
    }
    assert_eq!(l.energy_kept(), kept);
    assert!(l.th.ledger.made.iter().all(|&n| n > 0), "every kind of event happened: tried {:?}, made {:?}", l.th.ledger.tried, l.th.ledger.made);
}

/// Two-way programs give the reference's result and hand the program back.
#[test]
fn programs_give_their_results() {
    let cases = [(add(), Val::f(nat(0), nat(1))), (add(), Val::f(nat(1), nat(0)))];
    for (i, (p, x)) in cases.iter().enumerate() {
        let want = p.run(x, &mut 1000).unwrap();
        let (mut l, _, out) = load(p, x, i as u64 + 1);
        assert!(l.run_thermo(200_000_000), "{}: no result", p.show());
        assert_eq!(twoway::read(&l.shadow, out), Some((p.code(), want)), "{}", p.show());
        eprintln!("{}: {:.0} clocks, ledger {:?}", p.show(), l.stats.clocks, l.th.ledger);
    }
}

/// Once the answer is in, the banks hold on average what the bath's temperature says.
#[test]
fn banks_settle_at_the_bath() {
    let (mut l, _, _) = load(&add(), &Val::f(nat(0), nat(1)), 3);
    assert!(l.run_thermo(200_000_000));
    l.run_events(400_000);
    let (mut sum, mut n) = (0.0, 0.0);
    for _ in 0..200 {
        l.run_events(2_000);
        for &s in l.live() { sum += l.th.bank[s as usize] as f64; n += 1.0; }
    }
    let (mean, want) = (sum / n, l.bank_mean());
    assert!((mean / want - 1.0).abs() < 0.1, "mean bank {mean:.2} quanta, the bath's {want:.2}");
}

/// With no drive, and the supply filling half the slots, nothing is pushed: every kind of move takes
/// heat from its bank as often as it gives heat back (each is the other's reverse), and wires fold as
/// often as they unfold.
#[test]
fn moves_balance_with_nothing_driving() {
    let (mut l, _, _) = load(&add(), &Val::f(nat(1), nat(1)), 7);
    l.p.drive = 0.0;
    l.run_events(2_000_000);
    let g0 = l.th.ledger.clone();
    l.run_events(6_000_000);
    let g = &l.th.ledger;
    let n = |a: &[u64; 10], b: &[u64; 10], k: usize| (a[k] - b[k]) as f64;
    let pairs = [("hop", n(&g.up, &g0.up, 0), n(&g.down, &g0.down, 0)), ("exchange", n(&g.up, &g0.up, 1), n(&g.down, &g0.down, 1)),
                 ("flip", n(&g.up, &g0.up, 2), n(&g.down, &g0.down, 2)), ("unfold, fold", n(&g.made, &g0.made, 4), n(&g.made, &g0.made, 3))];
    for (what, a, b) in pairs {
        eprintln!("{what}: {a} against {b}");
        assert!(a + b > 50.0 && (a - b).abs() <= 0.05 * (a + b) + 4.0 * (a + b).sqrt(), "{what}: {a} against {b}");
    }
}

/// Slots run emptier at sites with a wanted reader than elsewhere: the work draws fuel to it.
#[test]
fn fuel_runs_low_where_the_work_is() {
    let (mut l, _, _) = load(&add(), &Val::f(nat(1), nat(1)), 9);
    let (k, ks) = (l.p.k, l.ks);
    let (mut near, mut far) = ((0.0, 0.0), (0.0, 0.0));
    for _ in 0..100 {
        l.run_events(50_000);
        for &s in l.live() {
            let full = l.th.fuel[s as usize].count_ones() as f64;
            let reader = (0..k).any(|i| { let t = l.tag(s, i); t != 0 && tag_of(t).is_consumer() && l.want[s as usize * ks + i] });
            let at = if reader { &mut near } else { &mut far };
            *at = (at.0 + full, at.1 + 1.0);
        }
    }
    let (near, far) = (near.0 / near.1, far.0 / far.1);
    eprintln!("full slots where readers are {near:.2}, elsewhere {far:.2}");
    assert!(near < 0.9 * far, "full slots where readers are {near:.2}, elsewhere {far:.2}");
}

/// Forward to the answer, then the drive turned round: the answer is taken back and a tenth of the
/// rewrites are undone, energy kept all the while (turning the drive round is work, counted).
#[test]
fn turned_round_it_goes_back() {
    let (mut l, _, _) = load(&add(), &Val::f(nat(0), nat(1)), 5);
    assert!(l.run_thermo(200_000_000));
    let (kept, records, forward) = (l.energy_kept(), l.records_kept(), l.stats.clocks);
    l.set_drive(-8.0);
    let stop = l.stats.proposals + 200_000_000;
    while (l.answered() || l.records_kept() * 10 > records * 9) && l.stats.proposals < stop { l.run_events(100_000); }
    assert!(!l.answered() && l.records_kept() * 10 <= records * 9, "{} of {records} records left, answered {}", l.records_kept(), l.answered());
    assert_eq!(l.energy_kept(), kept);
    eprintln!("forward {forward:.0} clocks, {records} records; a tenth undone by {:.0}; borrowed {} of {} fuel", l.stats.clocks, l.th.ledger.borrowed, l.th.ledger.fuel);
}
