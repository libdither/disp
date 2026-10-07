//! The chip configuration's constants as WGSL (the GPU kernel's tables), from the simulator's own
//! rules, energies and probabilities, so the two cannot drift apart. hw/rtl gets the same as
//! Verilog from `strands-hw`.
use crate::lattice::{chip, fresh_wanted_in, Channel, Energy, Params, Source, NONE};
use rust_ca_lattice::rules::{End, Tag, ALL_TAGS, RULES};
use std::fmt::Write;

fn code(t: Tag) -> u32 { ALL_TAGS.iter().position(|x| *x == t).unwrap() as u32 + 1 }

/// An end of a rule wire in 8 bits: kind (0 fresh, 1 consumer aux, 2 producer aux) << 6, then the
/// fresh agent << 3 and its port, or the aux port.
pub fn end(e: End) -> u32 {
    match e {
        End::Fresh(f, q) => (f as u32) << 3 | q as u32,
        End::CAux(i) => 1 << 6 | i as u32,
        End::PAux(i) => 2 << 6 | i as u32,
    }
}

fn array(w: &mut String, name: &str, vals: &[u32]) {
    let body = vals.iter().map(|v| format!("{v}u")).collect::<Vec<_>>().join(", ");
    writeln!(w, "const {name}: array<u32, {}> = array<u32, {}>({body});", vals.len(), vals.len()).unwrap();
}

/// Why the GPU kernels cannot run a configuration: they are the chip's block schedule on its
/// 40-byte site, with pulses; energies, probabilities and switches come from the tables.
pub fn gpu_unfit(p: &Params) -> Option<&'static str> {
    if !(p.margolus && p.block_moves && p.block && p.block_side == 2) { return Some("it runs only 2×2×2 blocks with a turn for every site"); }
    if p.k != 2 || p.lanes != 4 { return Some("its site holds 2 agents and 4 strands per link"); }
    if !p.pulse { return Some("demand always travels as pulses there"); }
    if p.pressure != 0.0 || p.repel != 0.0 { return Some("it has no pressure or repulsion"); }
    if p.strangers != 0.0 || p.trees != 0.0 || p.garbage != 0.0 { return Some("it keeps no tree labels and does not look for strangers"); }
    if p.memo != 0 { return Some("merging equal computations is surgery by a central detector, on the CPU"); }
    let mut fields = p.fields.iter().filter(|c| c.source != Source::NONE);
    if let Some(c) = fields.next() {
        if fields.next().is_some() { return Some("it keeps one field"); }
        if c.source.0 & !(Source::READER | Source::PULSE) != 0 { return Some("its field's sources are wanted readers and demand pulses"); }
        if c.wire_step != NONE && c.wire_step < c.step { return Some("its field spreads alike along wires and through space"); }
        if c.bits > 4 { return Some("its field has at most 4 bits"); }
    }
    None
}

/// Why the GPU cannot run this lattice: its configuration (`gpu_unfit`), or superpositions, whose
/// labels its 40-byte site has no room for (and whose rules its tables leave out).
pub fn gpu_refuses(l: &crate::lattice::Lattice) -> Option<&'static str> {
    gpu_unfit(&l.p).or_else(|| (l.labelled() > 0).then_some("it runs no superpositions"))
}

pub fn wgsl() -> String { wgsl_for(&chip()) }

/// The tables for configuration p (which `gpu_unfit` accepts).
pub fn wgsl_for(p: &Params) -> String {
    assert!(gpu_unfit(p).is_none(), "{}", gpu_unfit(p).unwrap());
    let e = Energy::new(p);
    let chance = |x: f64| (x * 256.0).round() as u32;
    let mut v = String::new();
    let w = &mut v;
    writeln!(w, "// Generated from the simulator's rules and chip configuration (src/tables.rs).").unwrap();
    writeln!(w, "const E_PRINCIPAL: i32 = {}; const E_PRINCIPAL_IDLE: i32 = {}; const E_AUX: i32 = {}; const E_AUX_IDLE: i32 = {};",
        e.principal[0], e.principal[1], e.aux[0], e.aux[1]).unwrap();
    writeln!(w, "const E_CROWD: i32 = {}; const E_IDLE: i32 = {}; const E_LINK: i32 = {}; const E_BOARD: i32 = {};", e.crowd, e.idle, e.link, e.board).unwrap();
    writeln!(w, "const CH_ACTIVE: u32 = {}u; const CH_HOP: u32 = {}u; const CH_ALONG: u32 = {}u; const CH_SWAP: u32 = {}u;",
        chance(p.active), chance(p.p_hop), chance(0.7), chance(p.swap)).unwrap();
    // No cap: more pairings than a site's 33 ends can hold.
    writeln!(w, "const PAIRS: u32 = {}u; const LAZY: bool = {}; const GC: bool = {};", if p.pairs == 0 { 31 } else { p.pairs }, p.lazy, p.gc).unwrap();
    // The one field (field.rs), if any; with none, every field term is dead code.
    let f = p.fields.iter().find(|c| c.source != Source::NONE).copied();
    let c = f.unwrap_or(Channel::OFF);
    let fw = c.weights();
    writeln!(w, "const CALLS: bool = {}; const FIELD: bool = {}; const F_READER: bool = {}; const F_PULSE: bool = {};",
        p.calls, f.is_some(), c.source.0 & Source::READER != 0, c.source.0 & Source::PULSE != 0).unwrap();
    writeln!(w, "const F_LEVEL: u32 = {}u; const F_CAP: u32 = {}u; const F_STEP: u32 = {}u; const F_DECAY: u32 = {}u;",
        c.level.min(if f.is_some() { c.cap() } else { 0 }), if f.is_some() { c.cap() } else { 0 }, c.step, c.decay).unwrap();
    writeln!(w, "const F_W: array<i32, 3> = array<i32, 3>({}, {}, {}); const F_WIRE: i32 = {};", fw[0], fw[1], fw[2], fw[3]).unwrap();
    for t in ALL_TAGS {
        let name = match t { Tag::Pair => "PAIR".into(), Tag::Sel => "SEL".into(), Tag::Unp => "UNP".into(), Tag::Dn => "DN".into(),
                             Tag::Eps => "EPS".into(), Tag::Nrm => "NRM".into(), Tag::Out => "OUT".into(), _ => t.name().to_uppercase() };
        writeln!(w, "const T_{name}: u32 = {}u;", code(t)).unwrap();
    }
    writeln!(w, "const ACCEPT_LEN: i32 = {};", e.accept.len()).unwrap();
    array(w, "ACCEPT", &e.accept);
    let nt = ALL_TAGS.len() as u32 + 1;
    let mut rule_of = vec![31u32; (nt * nt) as usize];
    for (i, r) in RULES.iter().enumerate() { rule_of[(code(r.consumer) * nt + code(r.producer)) as usize] = i as u32; }
    writeln!(w, "const NRULES: u32 = {}u; const NTAGS: u32 = {nt}u;", RULES.len()).unwrap();
    array(w, "RULE_OF", &rule_of);
    array(w, "RULE_N", &RULES.iter().map(|r| r.fresh.len() as u32).collect::<Vec<_>>());
    array(w, "RULE_NW", &RULES.iter().map(|r| r.wires.len() as u32).collect::<Vec<_>>());
    array(w, "RULE_WANTED", &RULES.iter().map(|r| fresh_wanted_in(r, p.fork).iter().enumerate().fold(0, |m, (f, &b)| m | (b as u32) << f)).collect::<Vec<_>>());
    let mut fresh = vec![0u32; RULES.len() * 6];
    let (mut wa, mut wb) = (vec![0u32; RULES.len() * 9], vec![0u32; RULES.len() * 9]);
    for (i, r) in RULES.iter().enumerate() {
        assert!(r.fresh.len() <= 6 && r.wires.len() <= 9, "a rule outgrew the tables");
        for (f, t) in r.fresh.iter().enumerate() { fresh[i * 6 + f] = code(*t); }
        for (k, &(a, b)) in r.wires.iter().enumerate() { wa[i * 9 + k] = end(a); wb[i * 9 + k] = end(b); }
    }
    array(w, "RULE_FRESH", &fresh);
    array(w, "RULE_WA", &wa);
    array(w, "RULE_WB", &wb);
    w.push_str(r#"
fn accept_thr(d: u32) -> u32 { if (d >= u32(ACCEPT_LEN)) { return 0u; } return ACCEPT[d]; }
/// The rule for a consumer tag meeting a producer tag (31: none).
fn rule_of(ct: u32, pt: u32) -> u32 { if (ct >= NTAGS || pt >= NTAGS) { return 31u; } return RULE_OF[ct * NTAGS + pt]; }
fn rule_n(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_N[ri]; }
fn rule_nw(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_NW[ri]; }
/// Bit f: fresh agent f starts out wanted.
fn rule_wanted(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_WANTED[ri]; }
fn rule_fresh(ri: u32, f: u32) -> u32 { if (ri >= NRULES || f >= 6u) { return 0u; } return RULE_FRESH[ri * 6u + f]; }
/// The two ends of wire k of a rule (see src/tables.rs `end`).
fn rule_wa(ri: u32, k: u32) -> u32 { if (ri >= NRULES || k >= 9u) { return 0u; } return RULE_WA[ri * 9u + k]; }
fn rule_wb(ri: u32, k: u32) -> u32 { if (ri >= NRULES || k >= 9u) { return 0u; } return RULE_WB[ri * 9u + k]; }
"#);
    v
}

/// The whole GPU shader for configuration p: the tables, then the stages in order.
pub fn shader(p: &Params) -> String {
    [wgsl_for(p).as_str(), include_str!("gpu/prelude.wgsl"), include_str!("gpu/collect.wgsl"), include_str!("gpu/fire.wgsl"),
     include_str!("gpu/moves.wgsl"), include_str!("gpu/block.wgsl"), include_str!("gpu/busy.wgsl")].join("\n")
}
