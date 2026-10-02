//! The browser build: the strand lattice driven from JavaScript through a flat C ABI. The
//! player draws straight from the lattice's own arrays.

use crate::lattice::{Lattice, Params};
use rust_ca_lattice::net::Net;
use rust_ca_lattice::oracle::{self, Fuel, Term};
use rust_ic_mesh::term;

struct State { l: Lattice, term: Term, done: bool }
static mut STATE: Option<State> = None;
static mut TEXT: Vec<u8> = Vec::new();
static mut STATS: [f64; 24] = [0.0; 24];

#[allow(static_mut_refs)]
fn st() -> &'static mut State { unsafe { STATE.as_mut().expect("no lattice loaded") } }
#[allow(static_mut_refs)]
fn set_text(s: &str) -> u32 { unsafe { TEXT.clear(); TEXT.extend_from_slice(s.as_bytes()); TEXT.len() as u32 } }

#[no_mangle]
pub extern "C" fn alloc(n: usize) -> *mut u8 { let mut v = Vec::<u8>::with_capacity(n); let p = v.as_mut_ptr(); std::mem::forget(v); p }
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn text_ptr() -> *const u8 { unsafe { TEXT.as_ptr() } }
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn text_len() -> u32 { unsafe { TEXT.len() as u32 } }

fn parse(src: &str) -> Result<Term, String> {
    let src = src.trim();
    if let Some((name, n)) = src.split_once(':') {
        if let (Some(t), Ok(n)) = (term::workload(name, 0), n.trim().parse::<u64>()) { let _ = t; return Ok(term::workload(name, n).unwrap()); }
    }
    if let Some(t) = term::workload(src, 0) { return Ok(t); }
    if let Some(rest) = src.strip_prefix("random:") {
        let (seed, depth) = rest.split_once(':').unwrap_or((rest, "5"));
        let mut rng = oracle::Lcg(seed.trim().parse().map_err(|_| "random:<seed>:<depth>")?);
        return Ok(rng.rand_term(depth.trim().parse().map_err(|_| "random:<seed>:<depth>")?));
    }
    term::parse(src)
}

/// Load a term. Flags: bit 0 block rewrites, bit 1 lazy, bit 2 demand pulses, bit 3 Margolus
/// blocks, bit 4 erasers collect duplicators. Returns 0, or an error in the text.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn strands_new(w: u32, h: u32, depth: u32, k: u32, lanes: u32, flags: u32, temp: f64, swap: f64, agent_turns: f64, w_principal: f64, w_aux: f64,
                              seed: u32, src: *const u8, len: usize) -> i32 {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let t = match parse(src) { Ok(t) => t, Err(e) => { set_text(&e); return 1; } };
    let p = Params { w, h, depth, k: k as usize, lanes: lanes as usize, block: flags & 1 != 0, lazy: flags & 2 != 0,
                     pulse: flags & 4 != 0, margolus: flags & 8 != 0, gc: flags & 16 != 0, temp, swap, agent_turns, w_principal, w_aux, seed: seed as u64, ..Params::default() };
    if p.margolus && !p.block { set_text("2×2×2 blocks need rewrites inside one 2×2 block"); return 3; }
    let mut net = Net::new();
    let root = net.build(&t);
    let (_, out) = net.drive(root);
    match Lattice::load(p, net, out) {
        Ok(l) => { unsafe { STATE = Some(State { l, term: t, done: false }); } 0 }
        Err(e) => { set_text(&e); 2 }
    }
}

/// Make `n` more proposals (stopping when the answer is in). Returns 1 once finished.
#[no_mangle]
pub extern "C" fn strands_run(n: u32) -> u32 {
    let s = st();
    if !s.done {
        let target = s.l.stats.proposals + n as u64;
        s.done = s.l.run(target);
    }
    s.done as u32
}

#[no_mangle] pub extern "C" fn tags_ptr() -> *const u8 { st().l.tags.as_ptr() }
#[no_mangle] pub extern "C" fn want_ptr() -> *const bool { st().l.want.as_ptr() }
#[no_mangle] pub extern "C" fn mate_ptr() -> *const u8 { st().l.mate.as_ptr() }
#[no_mangle] pub extern "C" fn pulse_ptr() -> *const u8 { st().l.pulse_at.as_ptr() }
#[no_mangle] pub extern "C" fn live_ptr() -> *const u32 { st().l.live().as_ptr() }
#[no_mangle] pub extern "C" fn live_len() -> u32 { st().l.live().len() as u32 }
#[no_mangle] pub extern "C" fn slots_per_site() -> u32 { st().l.ks as u32 }
#[no_mangle] pub extern "C" fn ends_per_site() -> u32 { st().l.ends as u32 }
#[no_mangle] pub extern "C" fn fire_log_ptr() -> *const u32 { st().l.fire_log.as_ptr() }
#[no_mangle] pub extern "C" fn fire_log_len() -> u32 { st().l.fire_log.len() as u32 }
#[no_mangle] pub extern "C" fn fire_log_clear() { st().l.fire_log.clear(); }

/// proposals, clocks, fires, hops, swaps, folds, flips, strands, peak strands, blocked, done,
/// agents, wanted walker steps, walker steps blocked by a full site, demand pulses delivered,
/// peak live sites, duplicators collected
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn stats_ptr() -> *const f64 {
    let s = st();
    let x = &s.l.stats;
    let v = [x.proposals as f64, x.clocks, x.fires as f64, x.hops as f64, x.swaps as f64, x.folds as f64, x.flips as f64,
             x.strands as f64, x.peak_strands as f64, x.blocked_fires as f64, s.done as u8 as f64,
             s.l.shadow.live_count() as f64, x.walk_ok as f64, x.walk_fail[0] as f64, x.pulses as f64, x.peak_live as f64, x.collected as f64];
    unsafe { for (i, y) in v.iter().enumerate() { STATS[i] = *y; } STATS.as_ptr() }
}

#[no_mangle]
pub extern "C" fn strands_answer() -> u32 { match st().l.readback() { Some(t) => set_text(&oracle::show(&t)), None => set_text("") } }

#[no_mangle]
pub extern "C" fn strands_answer_decoded() -> u32 {
    let Some(t) = st().l.readback() else { return set_text("") };
    if let Some(n) = term::read_nat(&t) { return set_text(&format!("{n}")); }
    if let Some(xs) = term::read_nat_list(&t) { return set_text(&format!("{xs:?}")); }
    set_text("")
}

#[no_mangle]
pub extern "C" fn strands_oracle(fuel: u32) -> u32 {
    match oracle::nf(st().term.clone(), &mut Fuel(fuel as u64 * 1000)) { Ok(t) => set_text(&oracle::show(&t)), Err(_) => set_text("") }
}

/// The size of the term's initial drawing (agents), or -1 with the parse error in the text.
#[no_mangle]
pub extern "C" fn strands_probe(src: *const u8, len: usize) -> i32 {
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    match parse(src) {
        Ok(t) => { let mut net = Net::new(); let r = net.build(&t); net.drive(r); net.agents.len() as i32 }
        Err(e) => { set_text(&e); -1 }
    }
}

#[no_mangle]
pub extern "C" fn catalog() -> u32 {
    let w: Vec<String> = term::WORKLOADS.iter().map(|(n, d, k)| format!("{{\"name\":\"{n}\",\"desc\":\"{}\",\"n\":{k}}}", d.replace('"', "'"))).collect();
    set_text(&format!("{{\"workloads\":[{}]}}", w.join(",")))
}
