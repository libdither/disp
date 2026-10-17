//! The chip configuration's constants as WGSL (the GPU kernel's tables), from the simulator's own
//! rules, energies and probabilities, so the two cannot drift apart. hw/rtl gets the same as
//! Verilog from `strands-hw`.
use crate::lattice::{chip, fresh_wanted_in, ports, Channel, Energy, Params, SiteBytes, Source, NONE};
use rust_ca_lattice::rules::{End, Rule, Tag, ALL_TAGS, RULES, TWO_RULES, TWO_TAGS, VIEW_RULES, VIEW_TAGS};
use std::fmt::Write;

fn code(t: Tag) -> u32 { crate::lattice::code(t) as u32 }

/// The GPU's rules, numbered as its tables have them: the chip's, the movement agents', then two-way programs'.
fn rules(p: &Params) -> Vec<&'static Rule> { RULES.iter().chain(VIEW_RULES).chain(if p.two { TWO_RULES } else { &[] }).collect() }

/// Tags numbered below this are the chip's and the movement agents' (`ALL_TAGS`, `VIEW_TAGS`, from 1).
const BASE_TAGS: u32 = (ALL_TAGS.len() + VIEW_TAGS.len()) as u32 + 1;

/// The rule number for no rule.
const NO_RULE: u32 = 255;

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
/// 40-byte site (44 with two-way programs' 4 ports a slot), with pulses; energies, probabilities and
/// switches come from the tables.
pub fn gpu_unfit(p: &Params) -> Option<&'static str> {
    if p.resid > 0 { return Some("keeping history runs on the CPU"); }
    if !(p.margolus && p.block_moves && p.block && p.block_side == 2) { return Some("it runs only 2×2×2 blocks with a turn for every site"); }
    if p.k != 2 || p.lanes != 4 { return Some("its site holds 2 agents and 4 strands per link"); }
    if !p.pulse { return Some("demand always travels as pulses there"); }
    if p.pressure != 0.0 || p.repel != 0.0 { return Some("it has no pressure or repulsion"); }
    if p.strangers != 0.0 || p.trees != 0.0 || p.garbage != 0.0 { return Some("it keeps no tree labels and does not look for strangers"); }
    if p.memo != 0 { return Some("merging equal computations is surgery by a central detector, on the CPU"); }
    if p.phys { return Some("the substrate's records and undoing run on the CPU"); }
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
    writeln!(w, "const PAIRS: u32 = {}u; const LAZY: bool = {}; const GC: bool = {}; const DN_ROOM: u32 = {}u;", if p.pairs == 0 { 31 } else { p.pairs }, p.lazy, p.gc, p.dn_room).unwrap();
    // The one field (field.rs), if any; with none, every field term is dead code.
    let f = p.fields.iter().find(|c| c.source != Source::NONE).copied();
    let c = f.unwrap_or(Channel::OFF);
    let fw = c.weights();
    writeln!(w, "const CALLS: bool = {}; const FIELD: bool = {}; const F_READER: bool = {}; const F_PULSE: bool = {};",
        p.calls, f.is_some(), c.source.0 & Source::READER != 0, c.source.0 & Source::PULSE != 0).unwrap();
    writeln!(w, "const F_LEVEL: u32 = {}u; const F_CAP: u32 = {}u; const F_STEP: u32 = {}u; const F_DECAY: u32 = {}u;",
        c.level.min(if f.is_some() { c.cap() } else { 0 }), if f.is_some() { c.cap() } else { 0 }, c.step, c.decay).unwrap();
    writeln!(w, "const F_W: array<i32, 3> = array<i32, 3>({}, {}, {}); const F_WIRE: i32 = {};", fw[0], fw[1], fw[2], fw[3]).unwrap();
    for t in ALL_TAGS.into_iter().chain(VIEW_TAGS) {
        let name = match t { Tag::Pair => "PAIR".into(), Tag::Sel => "SEL".into(), Tag::Unp => "UNP".into(), Tag::Dn => "DN".into(),
                             Tag::Eps => "EPS".into(), Tag::Nrm => "NRM".into(), Tag::Out => "OUT".into(), _ => t.name().to_uppercase() };
        writeln!(w, "const T_{name}: u32 = {}u;", code(t)).unwrap();
    }
    writeln!(w, "const ACCEPT_LEN: i32 = {};", e.accept.len()).unwrap();
    array(w, "ACCEPT", &e.accept);
    // A site's layout (lattice.rs `SiteBytes`): agent ports first, AR to a slot, then the strand ends.
    let (ar, sb) = (ports(p) as u32, SiteBytes::of(ports(p)));
    writeln!(w, "const AR: u32 = {ar}u; const A_ENDS: u32 = {}u; const NE: u32 = {}u; const SW: u32 = {}u;", 3 * ar, sb.ends, sb.words).unwrap();
    writeln!(w, "const B_TAG: u32 = {}u; const B_WANT: u32 = {}u; const B_PUL: u32 = {}u;", sb.tags, sb.want, SiteBytes::PULSE).unwrap();
    writeln!(w, "const EMPTY: Site = Site({});", sb.empty().iter().map(|v| format!("0x{v:08X}u")).collect::<Vec<_>>().join(", ")).unwrap();
    // Two-way programs' tags (numbered from TWO_P): the opcodes, producers, then the walkers, consumers.
    let (tp, tc) = (code(TWO_TAGS[0]), code(TWO_TAGS[0]) + TWO_TAGS.iter().take_while(|t| t.is_producer()).count() as u32);
    assert!(TWO_TAGS.iter().enumerate().all(|(i, t)| code(*t) == tp + i as u32 && t.is_producer() == (code(*t) < tc) && t.is_consumer() != t.is_producer()),
            "two-way programs' producers, then their consumers");
    let mut two_ar = [0u32; 2];
    for (i, t) in TWO_TAGS.iter().enumerate() { two_ar[i / 16] |= (t.arity() as u32 - 1) << (2 * (i % 16)); }
    writeln!(w, "const TWO: bool = {}; const TWO_P: u32 = {tp}u; const TWO_C: u32 = {tc}u; const TWO_END: u32 = {}u; const TWO_AR0: u32 = {}u; const TWO_AR1: u32 = {}u;",
        p.two, tp + TWO_TAGS.len() as u32, two_ar[0], two_ar[1]).unwrap();
    // The rule table is indexed by tag, two-way programs' following the movement agents'.
    let rules = rules(p);
    let tix = |t: u32| if t >= tp { t - tp + BASE_TAGS } else { t };
    let nt = BASE_TAGS + if p.two { TWO_TAGS.len() as u32 } else { 0 };
    let mut rule_of = vec![NO_RULE; (nt * nt) as usize];
    for (i, r) in rules.iter().enumerate() { rule_of[(tix(code(r.consumer)) * nt + tix(code(r.producer))) as usize] = i as u32; }
    let maxw = rules.iter().map(|r| r.wires.len()).max().unwrap().max(9);
    writeln!(w, "const NRULES: u32 = {}u; const NTAGS: u32 = {nt}u; const BASE_TAGS: u32 = {BASE_TAGS}u; const NO_RULE: u32 = {NO_RULE}u; const MAXW: u32 = {maxw}u;",
        rules.len()).unwrap();
    array(w, "RULE_OF", &rule_of);
    array(w, "RULE_N", &rules.iter().map(|r| r.fresh.len() as u32).collect::<Vec<_>>());
    array(w, "RULE_NW", &rules.iter().map(|r| r.wires.len() as u32).collect::<Vec<_>>());
    array(w, "RULE_WANTED", &rules.iter().map(|r| fresh_wanted_in(r, p.fork).iter().enumerate().fold(0, |m, (f, &b)| m | (b as u32) << f)).collect::<Vec<_>>());
    let mut fresh = vec![0u32; rules.len() * 6];
    let (mut wa, mut wb) = (vec![0u32; rules.len() * maxw], vec![0u32; rules.len() * maxw]);
    for (i, r) in rules.iter().enumerate() {
        assert!(r.fresh.len() <= 6 && r.wires.len() <= 12, "a rule outgrew the tables");
        for (f, t) in r.fresh.iter().enumerate() { fresh[i * 6 + f] = code(*t); }
        for (k, &(a, b)) in r.wires.iter().enumerate() { wa[i * maxw + k] = end(a); wb[i * maxw + k] = end(b); }
    }
    array(w, "RULE_FRESH", &fresh);
    array(w, "RULE_WA", &wa);
    array(w, "RULE_WB", &wb);
    w.push_str(r#"
fn accept_thr(d: u32) -> u32 { if (d >= u32(ACCEPT_LEN)) { return 0u; } return ACCEPT[d]; }
/// A tag's place in the rule table: two-way programs' follow the movement agents' (NTAGS: none).
fn tix(t: u32) -> u32 { if (TWO && t >= TWO_P) { return t - TWO_P + BASE_TAGS; } if (t >= BASE_TAGS) { return NTAGS; } return t; }
/// The rule for a consumer tag meeting a producer tag (NO_RULE: none).
fn rule_of(ct: u32, pt: u32) -> u32 { let c = tix(ct); let q = tix(pt); if (c >= NTAGS || q >= NTAGS) { return NO_RULE; } return RULE_OF[c * NTAGS + q]; }
/// Whether a consumer and a producer whose principals touch interact (rules.rs `meets`): a two-way
/// program is stuck where it has no rule.
fn meets(ct: u32, pt: u32) -> bool { return !TWO || (ct < TWO_P && pt < TWO_P) || rule_of(ct, pt) != NO_RULE; }
fn rule_n(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_N[ri]; }
fn rule_nw(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_NW[ri]; }
/// Bit f: fresh agent f starts out wanted.
fn rule_wanted(ri: u32) -> u32 { if (ri >= NRULES) { return 0u; } return RULE_WANTED[ri]; }
fn rule_fresh(ri: u32, f: u32) -> u32 { if (ri >= NRULES || f >= 6u) { return 0u; } return RULE_FRESH[ri * 6u + f]; }
/// The two ends of wire k of a rule (see src/tables.rs `end`).
fn rule_wa(ri: u32, k: u32) -> u32 { if (ri >= NRULES || k >= MAXW) { return 0u; } return RULE_WA[ri * MAXW + k]; }
fn rule_wb(ri: u32, k: u32) -> u32 { if (ri >= NRULES || k >= MAXW) { return 0u; } return RULE_WB[ri * MAXW + k]; }
"#);
    v
}

/// The whole GPU shader for configuration p: the tables, then the stages in order.
pub fn shader(p: &Params) -> String {
    [wgsl_for(p).as_str(), include_str!("gpu/prelude.wgsl"), include_str!("gpu/collect.wgsl"), include_str!("gpu/fire.wgsl"),
     include_str!("gpu/moves.wgsl"), include_str!("gpu/block.wgsl"), include_str!("gpu/busy.wgsl")].join("\n")
}
