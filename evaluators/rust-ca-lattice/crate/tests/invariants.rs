//! Mechanism invariants (AC_IDEA harness hardening), each pinning a claim the design
//! documents but the other suites only exercise incidentally:
//!
//! - kick: quiescence means nothing was forgotten — re-waking the whole grid moves no
//!   progress counter (the lost-wake bug class, made a standing gate).
//! - audit: per-op-class commit write-sets and read radii, measured and pinned as
//!   ceilings that may only move DOWN toward AC_IDEA's chip contract (≤2-cell commits,
//!   no combinational long reads).
//! - cooldown ablation: dropping a heuristic bit class parks more, never answers wrong.
//! - nursery: dropping a correctness-of-mechanism bit class actually breaks — the
//!   negative control that keeps the bit-class table honest.

use rust_ca_lattice::cascade_run::{check_projection, load_net, load_net_tree, Discipline, Runner};
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Lcg, Term};
use rust_ca_lattice::rules::RULES;
use std::collections::BTreeMap;

const BUDGET: u64 = 4_000_000;
/// Ablation runs only need enough budget to distinguish "quiesces" from "does not":
/// the whole corpus quiesces well under this; a livelocked run just burns whatever we
/// give it, so give it little.
const ABLATION_BUDGET: u64 = 600_000;

fn frontier_terms() -> Vec<(&'static str, Term)> {
    use rust_ca_lattice::oracle::{ap, f2, s};
    vec![
        ("identity", ap(Term::L, Term::L)),
        ("k-combinator", ap(ap(oracle::k(), s(Term::L)), Term::L)),
        ("fork-dispatch", ap(f2(Term::L, Term::L), Term::L)),
        ("s-rule-sharing", ap(f2(s(Term::L), s(Term::L)), Term::L)),
        ("k-chain", oracle::chain_k(2)),
        ("disp-t", oracle::disp_t()),
    ]
}

/// The soak's stratified rotation, truncated: enough random terms to include parked
/// runs of every congestion class without soak-scale runtime.
fn soak_slice(count: u32) -> Vec<(String, Term, Discipline, bool)> {
    let mut rng = Lcg(20260730);
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    let mut out = vec![];
    for i in 0..count {
        let term = rng.rand_term(3 + (i % 4));
        if oracle::nf(term.clone(), &mut Fuel(5_000)).is_err() {
            continue;
        }
        let discipline = disciplines[((i / 8) % 4) as usize];
        let tree_embed = (i / 4) % 2 == 1;
        out.push((format!("term {i}"), term, discipline, tree_embed));
    }
    out
}

fn run_term(term: &Term, discipline: Discipline, tree_embed: bool) -> (Runner, u32) {
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, out) = shadow.drive(root);
    let grid = if tree_embed {
        load_net_tree(&shadow, Topo::Full3D).expect("loads")
    } else {
        load_net(&shadow, Topo::Full3D).expect("loads")
    };
    (Runner::new(grid, shadow, discipline), out)
}

/// After quiescence — parked or complete — waking every live cell must advance no
/// progress counter. Any post-kick rewrite, walk, dock, or retract is a lost wake: a
/// cell that was willing to act but had been dropped by the event plumbing.
#[test]
fn kick_after_quiescence_is_a_no_op() {
    use rust_ca_lattice::cascade_run::Event;
    let mut cases: Vec<(String, Term, Discipline, bool)> = frontier_terms()
        .into_iter()
        .map(|(n, t)| (n.to_string(), t, Discipline::Fifo, false))
        .collect();
    cases.extend(soak_slice(48));
    let mut parked = 0u32;
    for (name, term, discipline, tree) in cases {
        let (mut r, _out) = run_term(&term, discipline, tree);
        assert!(r.run(BUDGET), "{name}: did not quiesce");
        let counts = |r: &Runner| {
            let (mut docks, mut retracts) = (0u64, 0u64);
            for e in &r.events {
                match e {
                    Event::Dock(..) => docks += 1,
                    Event::Retract(..) => retracts += 1,
                    _ => {}
                }
            }
            (r.grid.rewrites, r.grid.transport, docks, retracts)
        };
        let before = counts(&r);
        if !r.grid.seed_sids.is_empty() {
            parked += 1;
        }
        r.kick();
        assert!(r.run(BUDGET), "{name}: kicked run did not re-quiesce");
        let after = counts(&r);
        assert_eq!(
            before, after,
            "{name} [{discipline:?}/{}]: progress after kick — lost wake \
             (rewrites/transport/docks/retracts before vs after)",
            if tree { "tree" } else { "row" }
        );
    }
    println!("kick invariant: held on every run ({parked} parked runs among them)");
}

/// Measure every activation's write set and read radius, grouped by op class, over the
/// full rule atlas and the frontier terms. The ceilings pin today's worst cases; the
/// chip contract is write ≤ 2 and reads at adjacency, so these numbers may only move
/// DOWN as AC_IDEA rungs land (token-chain relief, signal-plane demand, host-assisted
/// roll scan). A new op class fails the test by design: classify it here.
#[test]
fn commit_footprints_and_read_radii_pinned() {
    use rust_ca_lattice::cascade_run::{dock_fixture, OpAudit};
    let mut total: BTreeMap<&'static str, OpAudit> = BTreeMap::new();
    let mut fold = |audit: BTreeMap<&'static str, OpAudit>| {
        for (k, v) in audit {
            let t = total.entry(k).or_default();
            t.activations += v.activations;
            t.max_writes = t.max_writes.max(v.max_writes);
            t.max_read_r = t.max_read_r.max(v.max_read_r);
        }
    };
    for rule in RULES {
        let (grid, shadow) = dock_fixture(rule, 3, false);
        let mut r = Runner::new(grid, shadow, Discipline::Fifo);
        r.audit = Some(BTreeMap::new());
        assert!(r.run(2_000_000), "atlas {}·{}: quiesce", rule.consumer.name(), rule.producer.name());
        fold(r.audit.take().unwrap());
    }
    for (name, term) in frontier_terms() {
        let (mut r, _) = run_term(&term, Discipline::Fifo, false);
        r.audit = Some(BTreeMap::new());
        assert!(r.run(8_000_000), "{name}: quiesce");
        fold(r.audit.take().unwrap());
    }
    println!("op class          activations  max_writes  max_read_r");
    for (k, v) in &total {
        println!("{k:<16}  {:>11}  {:>10}  {:>10}", v.activations, v.max_writes, v.max_read_r);
    }
    // The pinned ceilings (max_writes, max_read_r), measured 2026-07-30 over this
    // corpus. Chip contract: writes ≤ 2, reads at adjacency (r 1) — the gap is the
    // burn-down AC_IDEA's rungs must close (token-chain relief, signal-plane demand,
    // host-assisted roll scan). Raising any number is a regression toward less local.
    let ceiling: BTreeMap<&str, (u32, u32)> = [
        ("refusal", (0, 11)), // pure reads; r=11 = the dock-decline blocker scan (the
                              // roll/footprint scan family: host-assist is the lever)
        ("fabric", (6, 11)),  // heat, contraction, relief; r=11 = the same blocker scan
                              // on a declined dock whose relief then commits
        ("move", (4, 2)),     // walk + detour reservation pair
        ("dock", (2, 11)),    // the dock write is small; r=11 IS the roll/footprint scan
        ("growth", (6, 2)),   // reserve + merge + cursor advance; 6 = the merge-fail
                              // relief primitive (the same bracket footprint fabric
                              // hosts — the 4 was under-measurement before guest
                              // swings let those evictions succeed from growth)
        ("resolve", (2, 1)),  // the seated splice: already at the chip contract
        ("retract", (7, 3)),  // unwind + pair restore (not exercised by this corpus yet)
    ]
    .into_iter()
    .collect();
    for (k, v) in &total {
        let Some((w, r)) = ceiling.get(k) else {
            panic!("unclassified op class {k}: add it to the ceiling table");
        };
        assert!(
            v.max_writes <= *w,
            "{k}: write set grew {} > {w} — a commit got LESS local",
            v.max_writes
        );
        assert!(
            v.max_read_r <= *r,
            "{k}: read radius grew {} > {r} — a read got LESS local",
            v.max_read_r
        );
    }
}

/// The cooldown ablation lane. What it PINS is the claim that holds no matter what the
/// relief machinery does: undamped, the substrate parks more and never answers wrong.
///
/// The livelock count is a printed ledger, deliberately not asserted. It moved three
/// times in one day as termination mechanisms landed — 21/48 (undamped ping-pong), 6,
/// 0 (the displacement order took over), 3 (the order's pays-for-itself exemption,
/// whose payment can be stolen between the relief and the placement it unblocks, so
/// the stamps carry the residue). A control that has to be re-pinned by hand on every
/// mechanism change is measuring the mechanism, not the bit class; the classification
/// argument lives in AC_IDEA's bit-class table, where it can carry its reasoning.
/// Read the printed count when judging that table, and hunt any large jump with
/// `debug-cascade soak:N --churn`.
#[test]
fn cooldown_ablation_never_wrongs_but_can_livelock() {
    let (mut complete, mut livelocked, mut ran) = (0u32, 0u32, 0u32);
    for (name, term, discipline, tree) in soak_slice(48) {
        let want = oracle::show(&oracle::nf(term.clone(), &mut Fuel(5_000)).expect("filtered"));
        let (mut r, out) = run_term(&term, discipline, tree);
        r.cooldown_stamps = false;
        let quiesced = r.run(ABLATION_BUDGET);
        ran += 1;
        if !quiesced {
            livelocked += 1;
        }
        if let Some(got) = r.shadow.readback(r.shadow.get(out).ports[0]) {
            assert_eq!(oracle::show(&got), want, "{name}: WRONG ANSWER without cooldown");
            if quiesced && r.grid.seed_sids.is_empty() {
                complete += 1;
            }
        }
    }
    println!(
        "cooldown ablation: {complete}/{ran} complete, {livelocked} livelocked, zero wrong answers"
    );
    // The pinned claim: parking more is allowed, answering wrong is not (asserted per
    // run above). A run that never quiesces still cannot have answered wrong.
    assert!(complete > 0, "cooldown ablation completed nothing: the lane is broken");
}

/// The nursery bit is classified correctness-of-mechanism: letting grown agents skip it
/// should break something (wrong answer, invariant panic, no quiescence, or a quiescent
/// grid that no longer projects). This negative control hunts for a witness across the
/// frontier terms, a soak slice, every discipline, and both embeddings.
#[test]
fn nursery_is_load_bearing() {
    let disciplines = [
        Discipline::Fifo,
        Discipline::Lifo,
        Discipline::Random(0x9e37_79b9_7f4a_7c15),
        Discipline::AddressOrdered,
    ];
    let mut cases: Vec<(String, Term, Discipline, bool)> = vec![];
    for (name, term) in frontier_terms() {
        for d in disciplines {
            for tree in [false, true] {
                cases.push((name.to_string(), term.clone(), d, tree));
            }
        }
    }
    cases.extend(soak_slice(32));
    let total = cases.len();
    let mut violations: Vec<String> = vec![];
    for (name, term, discipline, tree) in cases {
        let want = oracle::show(&oracle::nf(term.clone(), &mut Fuel(100_000)).expect("nf"));
        let ctx = format!("{name} [{discipline:?}/{}]", if tree { "tree" } else { "row" });
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let (mut r, out) = run_term(&term, discipline, tree);
            r.nursery_discipline = false;
            if !r.run(ABLATION_BUDGET) {
                return Some("no quiescence".to_string());
            }
            if let Some(got) = r.shadow.readback(r.shadow.get(out).ports[0]) {
                if oracle::show(&got) != want {
                    return Some(format!("wrong answer {}", oracle::show(&got)));
                }
            }
            if r.grid.seed_sids.is_empty() {
                if let Err(e) = check_projection(&r.grid, &r.shadow) {
                    return Some(format!("projection: {e}"));
                }
            }
            None
        }));
        match outcome {
            Err(_) => violations.push(format!("{ctx}: panicked (invariant tripped)")),
            Ok(Some(v)) => violations.push(format!("{ctx}: {v}")),
            Ok(None) => {}
        }
    }
    for v in violations.iter().take(8) {
        println!("nursery ablation violation — {v}");
    }
    println!("nursery ablation: {} violation(s) across {total} runs", violations.len());
    // STATUS LEDGER, pinned so any drift trips. 2026-07-30: 4-6/80 violations
    // (Lifo/tree no-quiescence witnesses) — load-bearing. 2026-07-31 (reel): the
    // witnesses vanished (0/80), bit provisionally UNPROVEN. 2026-07-31 (endpoint
    // swings): witnesses RETURNED — disp-t [Fifo/tree] and [Lifo/tree] quiesce
    // seed-free but no longer project (grid 9 live vs shadow 13). Mechanism: the
    // swingable check trusts the nursery bit, so with the bit ablated a half-grown
    // agent looks like fair relief matter and gets its ports re-anchored mid-growth.
    // LOAD-BEARING; the corpus found its own witness, no adversarial construction
    // needed. Pinned as a floor (the exact count is schedule-chaotic).
    assert!(
        !violations.is_empty(),
        "nursery ablation found no witness — re-litigate the bit-class in AC_IDEA"
    );
}
