//! Model-cost census for the cascade substrate: the whole corpus (named pins plus the
//! size-knobbed families) runs to quiescence and reports substrate costs — rewrites,
//! transport, generations (the parallel critical path), peak live cells (area), dock and
//! retract tallies — never wall-clock: until the message driver exists, host time
//! measures the simulator, not the design. Fifo only: the canonical deterministic
//! schedule (discipline variance is the suite's and the soak's concern), so the numbers
//! diff cleanly.
//!
//! Usage: cargo run --release --bin census-cascade [-- --write-baseline]
//!
//! Against `census-baseline.tsv` (beside Cargo.toml): a completion regression fails, and
//! so does a stale baseline (an unpinned improvement or a new corpus row) — floors only
//! move up, deliberately. Cost drift prints but never gates (chaotic-margin discipline:
//! per-term costs flip, completion aggregates hold). A wrong answer panics — that is the
//! soak contract and no baseline may bless it. Budget exhaustion also panics: parked
//! means quiesced-incomplete; a run that never quiesces is a livelock and must not be
//! recordable.

use rust_ca_lattice::cascade_run::{load_net, Discipline, Event, Runner};
use rust_ca_lattice::lattice::Topo;
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, ap, f2, s, Fuel, Term};
use std::collections::BTreeMap;

// Sized for the corpus's biggest legitimate run (discard-tree(6): a 127-node tree's
// load + erasure), not for speed; a run that exhausts this is a livelock.
const BUDGET: u64 = 96_000_000;
const HEADER: &str =
    "family\tn\tcomplete\trewrites\ttransport\tgenerations\tpeak_cells\tdocks\tretracts";

fn corpus() -> Vec<(&'static str, u32, Term)> {
    use Term::L;
    let mut v: Vec<(&'static str, u32, Term)> = vec![
        ("identity", 0, ap(L, L)),
        ("fork-dispatch", 0, ap(f2(L, L), L)),
        ("s-rule-sharing", 0, ap(f2(s(L), s(L)), L)),
        ("disp-t", 0, oracle::disp_t()),
    ];
    for n in 1..=4 {
        v.push(("k-chain", n, oracle::chain_k(n)));
    }
    for n in 1..=6 {
        v.push(("discard-tree", n, oracle::discard_tree(n)));
    }
    for n in 1..=6 {
        v.push(("convoy", n, oracle::convoy(n)));
    }
    for n in 1..=5 {
        v.push(("share-tower", n, oracle::share_tower(n)));
    }
    v
}

#[derive(Clone, Debug, PartialEq)]
struct Row {
    family: String,
    n: u32,
    complete: bool,
    rewrites: u64,
    transport: u64,
    generations: u64,
    peak_cells: u64,
    docks: u64,
    retracts: u64,
}

impl Row {
    fn tsv(&self) -> String {
        format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.family,
            self.n,
            u8::from(self.complete),
            self.rewrites,
            self.transport,
            self.generations,
            self.peak_cells,
            self.docks,
            self.retracts
        )
    }
    fn parse(line: &str) -> Option<Row> {
        let f: Vec<&str> = line.split('\t').collect();
        if f.len() != 9 {
            return None;
        }
        Some(Row {
            family: f[0].into(),
            n: f[1].parse().ok()?,
            complete: f[2] == "1",
            rewrites: f[3].parse().ok()?,
            transport: f[4].parse().ok()?,
            generations: f[5].parse().ok()?,
            peak_cells: f[6].parse().ok()?,
            docks: f[7].parse().ok()?,
            retracts: f[8].parse().ok()?,
        })
    }
}

type RuleFires = BTreeMap<(&'static str, &'static str), u64>;
type RuleGrown = BTreeMap<(&'static str, &'static str), u64>;

fn run_one(
    family: &str,
    n: u32,
    term: &Term,
    rule_fires: &mut RuleFires,
    rule_grown: &mut RuleGrown,
) -> Row {
    let want = oracle::show(
        &oracle::nf(term.clone(), &mut Fuel(1_000_000)).expect("corpus term must normalize"),
    );
    let mut shadow = Net::new();
    let root = shadow.build(term);
    let (_nrm, out) = shadow.drive(root);
    let grid = load_net(&shadow, Topo::Full3D).expect("corpus term must load");
    let mut r = Runner::new(grid, shadow, Discipline::Fifo);
    let mut peak = r.grid.cells.len() as u64;
    let mut spent = 0u64;
    // Per-tick peak sampling: a retract can drop a whole blocklet footprint within one
    // generation, so coarser sampling would miss grow-then-retract spikes.
    while !r.quiescent() {
        assert!(spent < BUDGET, "{family}({n}): budget exhausted (livelock?)");
        r.tick_one();
        spent += 1;
        peak = peak.max(r.grid.cells.len() as u64);
    }
    let seed_free = r.grid.seed_sids.is_empty();
    let got = r.shadow.readback(r.shadow.get(out).ports[0]).map(|t| oracle::show(&t));
    if let Some(got) = &got {
        assert_eq!(got, &want, "{family}({n}): WRONG ANSWER");
    }
    for (rule, cells) in &r.grown_by_rule {
        let ru = &rust_ca_lattice::rules::RULES[*rule as usize];
        *rule_grown.entry((ru.consumer.name(), ru.producer.name())).or_insert(0) += cells;
    }
    let (mut docks, mut retracts) = (0u64, 0u64);
    for e in &r.events {
        match e {
            Event::Dock(..) => docks += 1,
            Event::Retract(..) => retracts += 1,
            Event::Fire(_, rule) => {
                let ru = &rust_ca_lattice::rules::RULES[*rule as usize];
                *rule_fires.entry((ru.consumer.name(), ru.producer.name())).or_insert(0) += 1;
            }
            Event::Move(..) => {}
        }
    }
    Row {
        family: family.into(),
        n,
        complete: seed_free && got.as_deref() == Some(want.as_str()),
        rewrites: r.grid.rewrites,
        transport: r.grid.transport,
        generations: r.generation,
        peak_cells: peak,
        docks,
        retracts,
    }
}

fn main() {
    let write_baseline = std::env::args().any(|a| a == "--write-baseline");
    let baseline_path = format!("{}/census-baseline.tsv", env!("CARGO_MANIFEST_DIR"));

    // Rows stream as they compute, so a mid-corpus panic still shows everything before it.
    let mut rule_fires = RuleFires::new();
    let mut rule_grown = RuleGrown::new();
    println!("{HEADER}");
    let rows: Vec<Row> = corpus()
        .iter()
        .map(|(f, n, t)| {
            let row = run_one(f, *n, t, &mut rule_fires, &mut rule_grown);
            println!("{}", row.tsv());
            row
        })
        .collect();

    let mut families: BTreeMap<&str, Vec<&Row>> = BTreeMap::new();
    for r in &rows {
        families.entry(&r.family).or_default().push(r);
    }
    println!("\ncompletion curves:");
    for (fam, rs) in &families {
        if rs.len() == 1 {
            println!("  {fam}: {}", if rs[0].complete { "complete" } else { "PARKED" });
        } else {
            let up_to = rs.iter().take_while(|r| r.complete).count();
            println!("  {fam}: completes up to n={up_to} of {}", rs.len());
        }
    }
    println!("\nrule fires across corpus (grown cells — the clump-rule cost evidence):");
    for ((c_name, p_name), c) in &rule_fires {
        let grown = rule_grown.get(&(*c_name, *p_name)).copied().unwrap_or(0);
        println!("  {c_name}·{p_name}: {c} ({grown} cells grown)");
    }

    if write_baseline {
        let mut out = String::from(HEADER);
        out.push('\n');
        for r in &rows {
            out.push_str(&r.tsv());
            out.push('\n');
        }
        std::fs::write(&baseline_path, &out).expect("write baseline");
        println!("\nbaseline written: {baseline_path}");
        return;
    }

    let Ok(base_text) = std::fs::read_to_string(&baseline_path) else {
        println!("\nno baseline at {baseline_path}; run with --write-baseline to create it");
        std::process::exit(1);
    };
    // A baseline that no longer parses must fail loudly, or the gate silently compares
    // against nothing.
    let mut base_lines = base_text.lines();
    assert_eq!(
        base_lines.next(),
        Some(HEADER),
        "baseline column drift: re-pin with --write-baseline"
    );
    let base: BTreeMap<(String, u32), Row> = base_lines
        .map(|l| Row::parse(l).unwrap_or_else(|| panic!("bad baseline row: {l:?}")))
        .map(|r| ((r.family.clone(), r.n), r))
        .collect();

    let mut regressions = vec![];
    let mut stale = vec![];
    println!("\nvs baseline:");
    for r in &rows {
        let Some(b) = base.get(&(r.family.clone(), r.n)) else {
            stale.push(format!("{}({}) is new (no baseline row)", r.family, r.n));
            continue;
        };
        match (b.complete, r.complete) {
            (true, false) => regressions.push(format!("{}({}) completed, now parks", r.family, r.n)),
            (false, true) => stale.push(format!("{}({}) parked, now completes", r.family, r.n)),
            _ => {}
        }
        if b.complete && r.complete {
            let mut ds = vec![];
            for (name, was, now) in [
                ("rewrites", b.rewrites, r.rewrites),
                ("transport", b.transport, r.transport),
                ("generations", b.generations, r.generations),
                ("peak", b.peak_cells, r.peak_cells),
            ] {
                if was == now {
                    continue;
                }
                if was == 0 {
                    ds.push(format!("{name} 0 -> {now}"));
                } else {
                    let d = (now as f64 / was as f64 - 1.0) * 100.0;
                    if d.abs() >= 2.0 {
                        ds.push(format!("{name} {d:+.1}% ({was} -> {now})"));
                    }
                }
            }
            if !ds.is_empty() {
                println!("  {}({}): {}", r.family, r.n, ds.join(", "));
            }
        }
    }
    for (key, b) in &base {
        if !rows.iter().any(|r| (r.family.as_str(), r.n) == (key.0.as_str(), key.1)) {
            regressions.push(format!("{}({}) vanished from the corpus", b.family, b.n));
        }
    }
    if regressions.is_empty() && stale.is_empty() {
        println!("  completion: no regressions, baseline current");
        return;
    }
    for r in &regressions {
        println!("  REGRESSION: {r}");
    }
    // Floors only move up: an unpinned improvement lets the next regression back to the
    // old level pass silently, so staleness fails too, with its own message.
    for s in &stale {
        println!("  BASELINE STALE: {s} — re-pin with --write-baseline");
    }
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::Row;

    /// Header, tsv() and parse() must agree; drift is self-checking.
    #[test]
    fn row_roundtrip() {
        let r = Row {
            family: "k-chain".into(),
            n: 3,
            complete: true,
            rewrites: 17,
            transport: 1234,
            generations: 99,
            peak_cells: 421,
            docks: 5,
            retracts: 1,
        };
        assert_eq!(Row::parse(&r.tsv()), Some(r));
        assert_eq!(super::HEADER.split('\t').count(), 9);
    }
}
