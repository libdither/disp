//! The browser build: the same `Mesh`, driven from JavaScript through a flat C ABI. The
//! player reads slots, router buffers and per-tick activity straight out of linear memory,
//! so what it draws is the engine's own state, never a replay of it.

use crate::mesh::{Config, Mesh, Recorder, Slot, FIELDS};
use crate::{run, term};
use rust_ca_lattice::oracle::{self, Fuel, Term};
use rust_ca_lattice::rules::RULES;

struct State {
    mesh: Mesh,
    term: Term,
    /// Fires and per-tile departures accumulated since the player last looked.
    frame_fires: Vec<[u32; 2]>,
    traffic: Vec<u32>,
}

static mut STATE: Option<State> = None;
static mut TEXT: Vec<u8> = Vec::new();
static mut STATS: [f64; 40] = [0.0; 40];

#[allow(static_mut_refs)]
fn state() -> &'static mut State { unsafe { STATE.as_mut().expect("no mesh loaded") } }

#[allow(static_mut_refs)]
fn set_text(s: &str) -> u32 {
    unsafe {
        TEXT.clear();
        TEXT.extend_from_slice(s.as_bytes());
        TEXT.len() as u32
    }
}

fn install_panic_hook() {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
}

#[no_mangle]
pub extern "C" fn alloc(n: usize) -> *mut u8 {
    let mut v = Vec::<u8>::with_capacity(n);
    let p = v.as_mut_ptr();
    std::mem::forget(v);
    p
}

#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn text_ptr() -> *const u8 { unsafe { TEXT.as_ptr() } }

#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn text_len() -> u32 { unsafe { TEXT.len() as u32 } }

/// Load a term (`@(…)` notation, ternary, or `workload:n`) onto a fresh mesh. Returns 0 on
/// success; otherwise the error is in the text buffer.
#[no_mangle]
#[allow(clippy::too_many_arguments)]
pub extern "C" fn mesh_new(w: u32, h: u32, k: u32, fifo: u32, ev: u32, spec: u32, fill: u32, outbox_cap: u32, events_cap: u32,
                           src: *const u8, len: usize) -> i32 {
    install_panic_hook();
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let t = match parse_source(src) {
        Ok(t) => t,
        Err(e) => { set_text(&e); return 1; }
    };
    let cfg = Config { w, h, k, fifo, events_per_tick: ev, speculate: spec, init_fill: fill, outbox_cap, events_cap };
    match run::load(&t, cfg, false) {
        Ok(mut mesh) => {
            mesh.rec = Some(Recorder::default());
            let cells = mesh.cells() as usize;
            unsafe { STATE = Some(State { mesh, term: t, frame_fires: vec![], traffic: vec![0; cells] }); }
            0
        }
        Err(e) => { set_text(&e); 2 }
    }
}

fn parse_source(src: &str) -> Result<Term, String> {
    let src = src.trim();
    if let Some((name, n)) = src.split_once(':') {
        if let Ok(n) = n.trim().parse::<u64>() {
            if let Some(t) = term::workload(name.trim(), n) { return Ok(t); }
        }
    }
    if let Some(t) = term::workload(src, 0) { return Ok(t); }
    if let Some(rest) = src.strip_prefix("random:") {
        let (seed, depth) = rest.split_once(':').unwrap_or((rest, "5"));
        let mut rng = oracle::Lcg(seed.trim().parse().map_err(|_| "random:<seed>:<depth>")?);
        return Ok(rng.rand_term(depth.trim().parse().map_err(|_| "random:<seed>:<depth>")?));
    }
    term::parse(src)
}

/// Run up to `n` ticks (stopping at quiescence). Returns the ticks actually run.
#[no_mangle]
pub extern "C" fn mesh_step(n: u32) -> u32 {
    let st = state();
    let mut ran = 0;
    while ran < n && !st.mesh.quiescent() && st.mesh.stalled < 256 {
        st.mesh.step();
        ran += 1;
        let rec = st.mesh.rec.as_ref().unwrap();
        if st.frame_fires.len() < 1 << 14 { st.frame_fires.extend_from_slice(&rec.fires); }
        for m in &rec.moves { st.traffic[m[0] as usize] += 1; }
    }
    ran
}

#[no_mangle]
pub extern "C" fn mesh_frame_reset() {
    let st = state();
    st.frame_fires.clear();
    st.traffic.iter_mut().for_each(|x| *x = 0);
}

#[no_mangle] pub extern "C" fn frame_fires_ptr() -> *const u32 { state().frame_fires.as_ptr() as *const u32 }
#[no_mangle] pub extern "C" fn frame_fires_len() -> u32 { state().frame_fires.len() as u32 }
#[no_mangle] pub extern "C" fn traffic_ptr() -> *const u32 { state().traffic.as_ptr() }
#[no_mangle] pub extern "C" fn moves_ptr() -> *const u32 { state().mesh.rec.as_ref().unwrap().moves.as_ptr() as *const u32 }
#[no_mangle] pub extern "C" fn moves_len() -> u32 { state().mesh.rec.as_ref().unwrap().moves.len() as u32 }
#[no_mangle] pub extern "C" fn slots_ptr() -> *const Slot { state().mesh.slots.as_ptr() }
#[no_mangle] pub extern "C" fn slot_size() -> u32 { std::mem::size_of::<Slot>() as u32 }
#[no_mangle] pub extern "C" fn fifo_ptr() -> *const u8 { state().mesh.fifo_len.as_ptr() }
#[no_mangle] pub extern "C" fn free_ptr() -> *const u8 { state().mesh.free.as_ptr() }
#[no_mangle] pub extern "C" fn phi_ptr() -> *const u8 { state().mesh.phi.as_ptr() as *const u8 }
#[no_mangle] pub extern "C" fn phi_fields() -> u32 { FIELDS as u32 }
#[no_mangle] pub extern "C" fn out_addr() -> u32 { state().mesh.out_addr }

/// Counters, in the order the player's STAT_NAMES lists them.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn stats_ptr() -> *const f64 {
    let m = &state().mesh;
    let s = &m.stats;
    let v: [u64; 26] = [
        m.tick, s.fires, s.hops, s.live, s.peak_live, s.inds, s.reserved, s.in_flight, s.peak_in_flight,
        s.cancels, s.direct_ships, s.local_fires, s.events, s.parked_reserves, m.quiescent() as u64,
        m.outcome() as u64, s.sent[0], s.sent[1], s.sent[2], s.sent[3], s.sent[4], s.sent[5], s.sent[6], s.sent[7],
        s.max_outbox, s.max_events,
    ];
    unsafe {
        for (i, x) in v.iter().enumerate() { STATS[i] = *x as f64; }
        STATS.as_ptr()
    }
}

/// The answer the root holds, as `@(…)` text, if it holds one.
#[no_mangle]
pub extern "C" fn mesh_answer() -> u32 {
    match state().mesh.readback() {
        Some(t) => set_text(&oracle::show(&t)),
        None => set_text(""),
    }
}

/// The answer read as the benchmark programs encode numbers and lists, when it is one.
#[no_mangle]
pub extern "C" fn mesh_answer_decoded() -> u32 {
    let Some(t) = state().mesh.readback() else { return set_text("") };
    if let Some(n) = term::read_nat(&t) { return set_text(&format!("{n}")); }
    if let Some(xs) = term::read_nat_list(&t) { return set_text(&format!("{xs:?}")); }
    set_text("")
}

/// The independent oracle's normal form for the loaded term ("" when it runs out of fuel).
#[no_mangle]
pub extern "C" fn mesh_oracle(fuel: u32) -> u32 {
    match oracle::nf(state().term.clone(), &mut Fuel(fuel as u64 * 1000)) {
        Ok(t) => set_text(&oracle::show(&t)),
        Err(_) => set_text(""),
    }
}

/// Workloads and rule names, as JSON.
#[no_mangle]
pub extern "C" fn catalog() -> u32 {
    let w: Vec<String> = term::WORKLOADS.iter()
        .map(|(n, d, k)| format!("{{\"name\":\"{n}\",\"desc\":\"{}\",\"n\":{k}}}", d.replace('"', "'")))
        .collect();
    let r: Vec<String> = RULES.iter()
        .map(|r| format!("\"{}·{}\"", r.consumer.name(), r.producer.name()))
        .collect();
    set_text(&format!("{{\"workloads\":[{}],\"rules\":[{}]}}", w.join(","), r.join(",")))
}

/// How many agents the term's initial net has (so the player can size the grid), or -1
/// with the parse error in the text buffer.
#[no_mangle]
pub extern "C" fn mesh_probe(src: *const u8, len: usize) -> i32 {
    install_panic_hook();
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    match parse_source(src) {
        Ok(t) => {
            let mut net = rust_ca_lattice::net::Net::new();
            let root = net.build(&t);
            net.drive(root);
            net.agents.len() as i32
        }
        Err(e) => { set_text(&e); -1 }
    }
}

/// Fires per rule, in rule-table order (26 counts).
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn rule_fires_ptr() -> *const f64 {
    static mut RULE: [f64; 26] = [0.0; 26];
    let s = &state().mesh.stats;
    unsafe {
        for (i, x) in s.rule_fires.iter().enumerate() { RULE[i] = *x as f64; }
        RULE.as_ptr()
    }
}
