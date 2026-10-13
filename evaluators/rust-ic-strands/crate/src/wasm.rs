//! The browser build: the strand lattice driven from JavaScript through a flat C ABI. The
//! player draws straight from the lattice's own arrays.

use crate::lattice::{latest, Lattice, Params, Stats};
use rust_ca_lattice::oracle;
use rust_ca_lattice::sup::{self, STerm};
use rust_ic_mesh::term;

/// `parts`: the reductions the input asked for, separated by `;` (`parse_all`), one term each.
struct State { l: Lattice, term: STerm, parts: Vec<STerm>, done: bool }
static mut STATE: Option<State> = None;
static mut TEXT: Vec<u8> = Vec::new();
static mut STATS: [f64; 24] = [0.0; 24];
/// Words passed between JavaScript and the engine (a GPU's sites and tally).
static mut WORDS: Vec<u32> = Vec::new();

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

/// A workload (`fib:2`, or `fib:&1{2,3}` superposed), a random term, or a term (`term::parse_sup`).
fn parse(src: &str) -> Result<STerm, String> {
    let src = src.trim();
    if let Some((name, n)) = src.split_once(':') {
        if let (Some(_), Ok(n)) = (term::workload(name, 0), n.trim().parse::<u64>()) { return Ok((&term::workload(name, n).unwrap()).into()); }
        if let Some(t) = term::workload_sup(name, n.trim()) { return t; }
    }
    if let Some(t) = term::workload(src, 0) { return Ok((&t).into()); }
    if let Some(rest) = src.strip_prefix("random:") {
        let (seed, depth) = rest.split_once(':').unwrap_or((rest, "5"));
        let mut rng = oracle::Lcg(seed.trim().parse().map_err(|_| "random:<seed>:<depth>")?);
        return Ok((&rng.rand_term(depth.trim().parse().map_err(|_| "random:<seed>:<depth>")?)).into());
    }
    term::parse_sup(src)
}

/// Several reductions side by side, `a; b; c`, as one tuple `F(a, F(b, c))` under one root: the root's
/// normalizer splits it into one normalizer a part at once, so the parts run in parallel, and their
/// order stays the tuple's wherever the run goes (`Lattice::read_part` reads each). Returns the tuple
/// and the parts.
fn parse_all(src: &str) -> Result<(STerm, Vec<STerm>), String> {
    let (mut parts, mut depth, mut from) = (vec![], 0i32, 0);
    for (i, c) in src.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            ';' if depth == 0 => { parts.push(&src[from..i]); from = i + 1; }
            _ => {}
        }
    }
    parts.push(&src[from..]);
    let parts: Vec<STerm> = parts.into_iter().filter(|p| !p.trim().is_empty()).map(parse).collect::<Result<_, _>>()?;
    let mut all = parts.last().ok_or("nothing to run")?.clone();
    for p in parts.iter().rev().skip(1) { all = STerm::F(std::rc::Rc::new(p.clone()), std::rc::Rc::new(all)); }
    Ok((all, parts))
}

/// An answer as the player shows it: as it is, or for a superposed input its universes over the
/// input's labels (`sup::collapse`), as the oracle's are shown.
fn collapsed(input: &STerm, answer: &STerm) -> STerm {
    let labels = input.labels();
    if labels.is_empty() { answer.clone() } else { sup::collapse(&answer.universes(&labels), &labels) }
}

/// The next run's settings, which start from the current design (lattice.rs `latest`).
static mut NEXT: Option<Params> = None;
#[allow(static_mut_refs)]
fn next() -> &'static mut Params { unsafe { NEXT.get_or_insert_with(latest) } }

/// Start the next run's settings over from the current design.
#[no_mangle]
pub extern "C" fn strands_settings() { *next() = latest(); }

/// Change one of the next run's settings, as `key=value` (lattice.rs `Params::set`). Returns 0, or
/// 1 with why in the text.
#[no_mangle]
pub extern "C" fn strands_set(src: *const u8, len: usize) -> i32 {
    let kv = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let r = kv.split_once('=').ok_or(format!("{kv}: not key=value")).and_then(|(k, v)| next().set(k, v));
    match r { Ok(()) => 0, Err(e) => { set_text(&e); 1 } }
}

/// The next run's layers.
#[no_mangle]
pub extern "C" fn strands_depth() -> u32 { next().depth }

/// Load a term on a w×h lattice with the next run's settings. Returns 0, or an error in the text
/// (2: the term's drawing does not fit).
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn strands_new(w: u32, h: u32, src: *const u8, len: usize) -> i32 {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let (t, parts) = match parse_all(src) { Ok(t) => t, Err(e) => { set_text(&e); return 1; } };
    let p = Params { w, h, ..*next() };
    if p.margolus && !p.block { set_text("2×2×2 blocks need rewrites inside one 2×2 block"); return 3; }
    let (net, out) = match crate::share::net_for(&t, &p) { Ok(n) => n, Err(e) => { set_text(&e); return 1; } };
    match Lattice::load(p, net, out) {
        Ok(l) => { unsafe { STATE = Some(State { l, term: t, parts, done: false }); SAVED.clear(); } 0 }
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

/// One move (with blocks, one clock), recorded. Returns how many events it made; 2 << 16 is
/// added once the answer is in.
#[no_mangle]
pub extern "C" fn strands_step(max_proposals: u32) -> u32 {
    let s = st();
    s.l.step(max_proposals as u64);
    if !s.done { s.done = s.l.answered(); }
    s.l.events.len() as u32 | if s.done { 2 << 16 } else { 0 }
}
#[no_mangle] pub extern "C" fn events_ptr() -> *const u32 { st().l.events.as_ptr() as *const u32 }
#[no_mangle] pub extern "C" fn events_len() -> u32 { st().l.events.len() as u32 }

/// After the answer: keep running so erasers collect the garbage.
#[no_mangle]
pub extern "C" fn strands_run_on(n: u32) {
    let l = &mut st().l;
    let target = l.stats.proposals + n as u64;
    l.run_on(target);
}

static mut MARKS: Vec<u8> = Vec::new();
/// Mark every agent the answer no longer depends on; returns how many there are.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn strands_garbage() -> u32 { unsafe { st().l.garbage(&mut MARKS) as u32 } }
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn garbage_ptr() -> *const u8 { unsafe { MARKS.as_ptr() } }

/// Agents reached from the root this time (`SEEN.1[id] == SEEN.0`), kept between calls.
static mut SEEN: (u32, Vec<u32>) = (0, Vec::new());
/// How many agents `strands_garbage` would mark, without marking them: a walk over the root's piece
/// of the net and the sites in use, cheap enough to ask after every clock.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn strands_garbage_left() -> u32 {
    let l = &st().l;
    let (stamp, seen) = unsafe { &mut SEEN };
    *stamp = stamp.wrapping_add(1);
    if *stamp == 0 { seen.fill(0); *stamp = 1; }
    if seen.len() < l.shadow.agents.len() { seen.resize(l.shadow.agents.len(), 0); }
    let slots = || l.live().iter().flat_map(|&s| (0..l.ks).map(move |k| s as usize * l.ks + k)).filter(|&i| l.tags[i] != 0);
    let mut stack: Vec<u32> = slots().filter(|&i| l.tags[i] as u32 == code(Tag::Out)).map(|i| l.sids[i]).collect();
    for &id in &stack { seen[id as usize] = *stamp; }
    while let Some(id) = stack.pop() {
        for p in l.shadow.get(id).ports.iter().flatten() {
            if seen[p.0 as usize] != *stamp { seen[p.0 as usize] = *stamp; stack.push(p.0); }
        }
    }
    slots().filter(|&i| seen[l.sids[i] as usize] != *stamp).count() as u32
}

/// The clock, the proposals made and the strands of wire, without working out the other counts
/// (`stats_ptr` walks every agent ever made).
#[no_mangle]
pub extern "C" fn strands_now(i: u32) -> f64 { let x = &st().l.stats; [x.clocks, x.proposals as f64, x.strands as f64][i as usize % 3] }

/// "A·F → T1 Pair" for rule i.
#[no_mangle]
pub extern "C" fn rule_text(i: u32) -> u32 {
    let r = rust_ca_lattice::rules::rule(i as usize);
    let fresh: Vec<&str> = r.fresh.iter().map(|t| t.name()).collect();
    set_text(&format!("{}·{} → {}", r.consumer.name(), r.producer.name(), if fresh.is_empty() { "nothing".to_string() } else { fresh.join(" ") }))
}

#[no_mangle] pub extern "C" fn tags_ptr() -> *const u8 { st().l.tags.as_ptr() }
#[no_mangle] pub extern "C" fn want_ptr() -> *const bool { st().l.want.as_ptr() }
#[no_mangle] pub extern "C" fn mate_ptr() -> *const u8 { st().l.mate.as_ptr() }
#[no_mangle] pub extern "C" fn pulse_ptr() -> *const u8 { st().l.pulse_at.as_ptr() }
#[no_mangle] pub extern "C" fn live_ptr() -> *const u32 { st().l.live().as_ptr() }
#[no_mangle] pub extern "C" fn live_len() -> u32 { st().l.live().len() as u32 }
#[no_mangle] pub extern "C" fn slots_per_site() -> u32 { st().l.ks as u32 }
/// The loaded lattice's layers, agents per site and strands per link (0, 1, 2).
#[no_mangle] pub extern "C" fn lattice_shape(i: u32) -> u32 { let p = &st().l.p; [p.depth, p.k as u32, p.lanes as u32][i as usize % 3] }
/// The demand field at every site, and its bits (0: no field).
#[no_mangle] pub extern "C" fn field_ptr() -> *const u8 { st().l.fields.values().as_ptr() }
#[no_mangle] pub extern "C" fn field_bits() -> u32 { st().l.fields.bits() }
/// The sites where the demand field is not zero.
#[no_mangle] pub extern "C" fn field_sites_ptr() -> *const u32 { st().l.fields.near().as_ptr() }
#[no_mangle] pub extern "C" fn field_sites_len() -> u32 { st().l.fields.near().len() as u32 }
#[no_mangle] pub extern "C" fn ends_per_site() -> u32 { st().l.ends as u32 }
#[no_mangle] pub extern "C" fn fire_log_ptr() -> *const u32 { st().l.fire_log.as_ptr() }
#[no_mangle] pub extern "C" fn fire_log_len() -> u32 { st().l.fire_log.len() as u32 }
#[no_mangle] pub extern "C" fn fire_log_clear() { st().l.fire_log.clear(); }

/// proposals, clocks, fires, hops, swaps, folds, flips, strands, peak strands, blocked, done,
/// agents, wanted walker steps, walker steps blocked by a full site, demand pulses delivered,
/// peak live sites, garbage collected
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
pub extern "C" fn strands_answer() -> u32 {
    let s = st();
    match s.l.read_answer() { Some(a) => set_text(&sup::show(&collapsed(&s.term, &a))), None => set_text("") }
}

/// An answer as a benchmark number or list of numbers, each universe's for a superposed input.
fn decode(t: &STerm) -> Option<String> {
    if let STerm::Sup(l, a, b) = t { return Some(format!("&{l}{{{}, {}}}", decode(a)?, decode(b)?)); }
    let t = t.plain()?;
    term::read_nat(&t).map(|n| format!("{n}")).or_else(|| term::read_nat_list(&t).map(|xs| format!("{xs:?}")))
}

/// The answer as a benchmark number or list of numbers, each universe's for a superposed input.
#[no_mangle]
pub extern "C" fn strands_answer_decoded() -> u32 {
    let s = st();
    let Some(a) = s.l.read_answer() else { return set_text("") };
    set_text(&decode(&collapsed(&s.term, &a)).unwrap_or_default())
}

/// How many reductions the input asked for (`parse_all`).
#[no_mangle]
pub extern "C" fn strands_parts() -> u32 { st().parts.len() as u32 }

/// Part i's answer once it is in, whatever the other parts are doing (else empty): as the engine
/// prints trees, or with `decoded` as `strands_answer_decoded` gives it.
#[no_mangle]
pub extern "C" fn strands_part(i: u32, decoded: u32) -> u32 {
    let s = st();
    let Some(part) = s.parts.get(i as usize) else { return set_text("") };
    let Some(a) = s.l.read_part(i as usize, s.parts.len()) else { return set_text("") };
    let a = collapsed(part, &a);
    set_text(&if decoded != 0 { decode(&a).unwrap_or_default() } else { sup::show(&a) })
}

/// The oracle's answer, universe by universe for a superposed input, collapsed as `strands_answer` is.
#[no_mangle]
pub extern "C" fn strands_oracle(fuel: u32) -> u32 {
    let t = &st().term;
    match sup::oracle_answers(t, fuel as u64 * 1000) { Some(u) => set_text(&sup::show(&sup::collapse(&u, &t.labels()))), None => set_text("") }
}

/// The size of the term's initial drawing (agents), or -1 with the parse error in the text.
#[no_mangle]
pub extern "C" fn strands_probe(src: *const u8, len: usize) -> i32 {
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    match parse_all(src) {
        Ok((t, _)) => match crate::share::net_for(&t, next()) { Ok((n, _)) => n.agents.len() as i32, Err(e) => { set_text(&e); -1 } },
        Err(e) => { set_text(&e); -1 }
    }
}

#[no_mangle]
pub extern "C" fn catalog() -> u32 {
    let w: Vec<String> = term::WORKLOADS.iter().map(|(n, d, k)| format!("{{\"name\":\"{n}\",\"desc\":\"{}\",\"n\":{k}}}", d.replace('"', "'"))).collect();
    set_text(&format!("{{\"workloads\":[{}]}}", w.join(",")))
}

// ---- handing a run to and from a GPU (player/gpu.js) ----------------------------------------

/// The GPU shader for the loaded configuration, in the text; or 0 and why the GPU cannot run it.
#[no_mangle]
pub extern "C" fn strands_gpu_shader() -> u32 {
    match crate::tables::gpu_refuses(&st().l) {
        Some(why) => { set_text(why); 0 }
        None => { set_text(&crate::tables::shader(&st().l.p)); 1 }
    }
}

/// Exactly n clocks of the chip's block schedule, as a GPU runs them. Returns 1 once the answer is in.
#[no_mangle]
pub extern "C" fn strands_clocks(n: u32) -> u32 {
    let s = st();
    for _ in 0..n { s.l.chip_clock(); }
    if !s.done { s.done = s.l.answered(); }
    s.done as u32
}

/// Room for n words to pass in; returns where they go.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn words_ptr(n: u32) -> *mut u32 { unsafe { WORDS.resize(n as usize, 0); WORDS.as_mut_ptr() } }
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn words_len() -> u32 { unsafe { WORDS.len() as u32 } }

/// The sites holding anything or a field (each its index, its 10 words, its field), into the words.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn strands_held() -> *const u32 { unsafe { WORDS = st().l.held_sites(); WORDS.as_ptr() } }

/// After a GPU ran `clocks` clocks from this lattice: the words are the sites holding anything
/// then (as `strands_held`), followed by its tally (tiles.wgsl `T_*`) and the sites of `fires`
/// rewrites. Returns 1 once the answer is in.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn strands_gpu_ran(clocks: u32, held: u32, fires: u32) -> u32 {
    let s = st();
    unsafe { HANDED_BACK += 1; }
    let w = unsafe { &WORDS };
    let (held, rest) = w.split_at(held as usize);
    s.l.put_sites(held);
    let x = &mut s.l.stats;
    x.clocks += clocks as f64;
    for (n, c) in [&mut x.proposals, &mut x.fires, &mut x.blocked_fires, &mut x.hops, &mut x.swaps, &mut x.folds, &mut x.flips,
                   &mut x.collected, &mut x.walk_ok, &mut x.walk_fail[0], &mut x.pulses].into_iter().zip(&rest[..11]) { *n += *c as u64; }
    let room = 4096usize.saturating_sub(s.l.fire_log.len());
    s.l.fire_log.extend(rest[11..11 + fires as usize].iter().take(room));
    if !s.done { s.done = s.l.answered(); }
    s.done as u32
}

/// A run as it was at some clock, for going back to it: the sites holding anything and the
/// superposition labels there, the counts, whether the answer was in, and the temperature and
/// cooling (which change once the answer is left alone). Restoring one and running on repeats the
/// run exactly, as a handover does.
struct Saved { held: Vec<u32>, labels: Vec<u32>, stats: Stats, done: bool, temp: f64, cooling: Option<(f64, f64)> }
static mut SAVED: Vec<Option<Saved>> = Vec::new();

/// Save the run as it is now; returns the saved state's number.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn save_state() -> u32 {
    let s = st();
    let v = Saved { held: s.l.held_sites(), labels: s.l.held_labels(), stats: s.l.stats.clone(), done: s.done, temp: s.l.p.temp, cooling: s.l.cooling };
    let saved = unsafe { &mut SAVED };
    match saved.iter().position(|x| x.is_none()) {
        Some(i) => { saved[i] = Some(v); i as u32 }
        None => { saved.push(Some(v)); saved.len() as u32 - 1 }
    }
}

/// Put the run back as saved state i had it.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn restore_state(i: u32) {
    let s = st();
    let v = unsafe { SAVED[i as usize].as_ref().expect("a saved state") };
    // Putting sites back numbers the agents afresh, as a GPU's hand-back does: picks are found again
    // in the net as it was at the last look (the first time since it).
    unsafe { if LOOKED_AT == HANDED_BACK { picked().remember(&s.l); } }
    s.l.put_sites_labelled(&v.held, &v.labels);
    unsafe { HANDED_BACK += 1; }
    s.l.stats = v.stats.clone();
    s.l.cooling = v.cooling;
    s.l.set_temp(v.temp);
    s.l.events.clear();
    s.l.fire_log.clear();
    s.done = v.done;
}

/// Forget saved state i.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn drop_state(i: u32) { unsafe { SAVED[i as usize] = None; } }

/// The bytes saved state i takes.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn state_bytes(i: u32) -> u32 {
    unsafe { SAVED[i as usize].as_ref().map_or(0, |v| ((v.held.len() + v.labels.len()) * 4 + std::mem::size_of::<Saved>()) as u32) }
}

/// Start cooling the run (lattice.rs `cool`); the temperature it runs at now.
#[no_mangle] pub extern "C" fn strands_cool() { st().l.cool(); }
#[no_mangle] pub extern "C" fn strands_temp() -> f64 { st().l.p.temp }

// ---- reading the net back, and computations picked in the player (readback.rs) ----------------

use crate::readback::{self, Fate, Selection, NOWHERE};
use rust_ca_lattice::rules::Tag;

/// Times a GPU handed a run back, each renumbering the abstract net.
static mut HANDED_BACK: u64 = 0;
static mut LOOKED_AT: u64 = 0;
static mut PICKED: Option<Selection> = None;
/// Seats (site × slots + slot) passed out to JavaScript.
static mut SEATS: Vec<u32> = Vec::new();

#[allow(static_mut_refs)]
fn picked() -> &'static mut Selection { unsafe { PICKED.get_or_insert_with(Selection::default) } }
#[allow(static_mut_refs)]
fn put_seats(v: Vec<u32>) -> u32 { unsafe { SEATS = v; SEATS.len() as u32 } }
fn code(t: Tag) -> u32 { crate::lattice::code(t) as u32 }
/// The id of the agent in a seat.
fn at_seat(site: u32, slot: u32) -> Option<u32> {
    let l = &st().l;
    let i = site as usize * l.ks + slot as usize;
    (slot < l.ks as u32 && i < l.tags.len() && l.tags[i] != 0).then(|| l.sids[i])
}
/// Where every agent sits (readback.rs `seats`), worked out once for each state of the lattice.
static mut WHERE: (u64, u64, u64, usize, Vec<u32>) = (0, 0, 0, 0, Vec::new());
#[allow(static_mut_refs)]
fn to_seats(ids: impl IntoIterator<Item = u32>) -> Vec<u32> {
    let l = &st().l;
    let now = (l.stats.proposals, l.stats.clocks.to_bits(), unsafe { HANDED_BACK }, l.shadow.agents.len());
    let at = unsafe {
        if (WHERE.0, WHERE.1, WHERE.2, WHERE.3) != now || WHERE.4.is_empty() { WHERE = (now.0, now.1, now.2, now.3, readback::seats(l)); }
        &WHERE.4
    };
    ids.into_iter().map(|id| at.get(id as usize).copied().unwrap_or(NOWHERE)).collect()
}
fn put_term(r: &readback::Rd, budget: u32) -> u32 {
    let (s, at) = readback::text(r, budget as usize);
    put_seats(to_seats(at));
    set_text(&s)
}

#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn seats_ptr() -> *const u32 { unsafe { SEATS.as_ptr() } }
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn seats_len() -> u32 { unsafe { SEATS.len() as u32 } }

/// Every agent and its wiring (readback.rs `wires`), into the seats, 6 numbers each; returns how many agents.
#[no_mangle]
pub extern "C" fn strands_net() -> u32 { put_seats(readback::wires(&st().l)) / 6 }
/// Times the abstract net was renumbered (a GPU handing a run back, or a saved state put back).
#[no_mangle]
pub extern "C" fn strands_renumbered() -> u32 { unsafe { HANDED_BACK as u32 } }

/// The agents the agent in a seat depends on (readback.rs `subtree`), into the seats; returns how many.
#[no_mangle]
pub extern "C" fn strands_subtree(site: u32, slot: u32) -> u32 {
    let Some(id) = at_seat(site, slot) else { return put_seats(vec![]) };
    put_seats(to_seats(readback::subtree(&st().l.shadow, id)))
}

/// What the agent in a seat means now (readback.rs `text`), in the text; each node's seat in the seats.
#[no_mangle]
pub extern "C" fn strands_term(site: u32, slot: u32, budget: u32) -> u32 {
    let Some(id) = at_seat(site, slot) else { put_seats(vec![]); return set_text("") };
    put_term(&readback::Reader::new(&st().l.shadow).meaning(id), budget)
}

/// Pick the agent in a seat, or let it go if it is picked: 1 if it is picked now, 0 if let go, -1 if
/// the seat is empty.
#[no_mangle]
pub extern "C" fn pick_toggle(site: u32, slot: u32) -> i32 {
    let Some(id) = at_seat(site, slot) else { return -1 };
    picked().toggle(&st().l.shadow, id) as i32
}

#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn pick_clear() { *picked() = Selection::default(); unsafe { WHERE.4.clear(); } }

/// Find the picked agents again (readback.rs `Selection::follow`). `gpu`: a GPU may hand the run
/// back before the next look. The seats: [seat, tag, output (3: all)] for each root, then [tag, what
/// happened (1 rewritten, 2 forced, 3 consumed, 4 lost), other tag] for each root not where it was.
/// Returns the roots, plus those events << 16.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn pick_follow(gpu: u32) -> u32 {
    let renumbered = unsafe { std::mem::replace(&mut LOOKED_AT, HANDED_BACK) != HANDED_BACK };
    let sel = picked();
    if sel.roots.is_empty() { return put_seats(vec![]); }
    let reports = sel.follow(&st().l, renumbered, gpu != 0);
    let seat = to_seats(sel.roots.iter().map(|r| r.id));
    let mut v = vec![];
    for (r, s) in sel.roots.iter().zip(seat) { v.extend([s, code(r.tag), r.out.map_or(3, |p| p as u32)]); }
    for e in &reports {
        let fate = match e.fate { Fate::Rewritten => 1, Fate::Forced => 2, Fate::Consumed => 3, Fate::Lost => 4 };
        v.extend([code(e.was), fate, e.other.map_or(0, code)]);
    }
    put_seats(v);
    sel.roots.len() as u32 | (reports.len() as u32) << 16
}

/// What root i stands for now, as `strands_term` gives it.
#[no_mangle]
pub extern "C" fn pick_term(i: u32, budget: u32) -> u32 {
    let Some(r) = picked().roots.get(i as usize) else { put_seats(vec![]); return set_text("") };
    put_term(&r.meaning(&st().l.shadow), budget)
}

/// The agents root i depends on, as `strands_subtree` gives them.
#[no_mangle]
pub extern "C" fn pick_subtree(i: u32) -> u32 {
    let Some(r) = picked().roots.get(i as usize) else { return put_seats(vec![]) };
    put_seats(to_seats(readback::subtree(&st().l.shadow, r.id)))
}

// ---- Reversible rewriting (rev.rs), for player/rev.html ----------------------------------------

static mut REV: Option<(crate::rev::Rev, oracle::Lcg)> = None;
#[allow(static_mut_refs)]
fn rv() -> &'static mut (crate::rev::Rev, oracle::Lcg) { unsafe { REV.as_mut().expect("no reversible net loaded") } }
/// An event's kind as a number: its rule's (`rules::rule`), or 1000 un-copy, 1001 dead work, 1002 unpaired.
fn kind_code(k: crate::rev::Kind) -> u32 {
    use crate::rev::Kind;
    match k { Kind::Rule(i) => i as u32, Kind::Uncopy => 1000, Kind::Dead => 1001, Kind::Unpaired => 1002 }
}
/// A tag as the player numbers it (`lattice::code`), a seam 21.
fn rev_code(t: Tag) -> u32 { if t == Tag::Seam { 21 } else { code(t) } }

/// Load a term to run reversibly; `erase`: erasers take values apart (else they keep them whole);
/// `collect`: un-copy and the rest. Returns how many agents, or -1 (the text says why).
#[no_mangle]
pub extern "C" fn rev_new(src: *const u8, len: usize, erase: u32, collect: u32, seed: u32) -> i32 {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let t = match parse(src) { Ok(t) => t, Err(e) => { set_text(&e); return -1; } };
    let (net, _) = crate::share::net_sup(&t, 0);
    let mut r = crate::rev::Rev::new(net, true);
    r.erase = erase != 0;
    r.collect = collect != 0;
    let n = r.net.live_count() as i32;
    unsafe { REV = Some((r, oracle::Lcg(seed as i64))); }
    n
}
/// Up to n events forward. Returns how many were made.
#[no_mangle]
pub extern "C" fn rev_forward(n: u32) -> u32 { rv().0.forward(n as usize) as u32 }
/// One round forward, every rewrite there is at once (`Rev::round`). Returns how many events it made.
#[no_mangle]
pub extern "C" fn rev_round() -> u32 { rv().0.round() as u32 }
/// Where each round began, as event numbers, into the seats. Returns how many rounds.
#[no_mangle]
pub extern "C" fn rev_rounds() -> u32 { put_seats(rv().0.rounds.iter().map(|&b| b as u32).collect()) }
/// Go to time t: the events before it done, the rest undone (`Rev::seek`).
#[no_mangle]
pub extern "C" fn rev_seek(t: u32) { rv().0.seek(t as usize) }
/// Undo up to n events: the latest each time, or with `any` one picked at random among those undoable now.
/// Returns how many were undone.
#[no_mangle]
pub extern "C" fn rev_back(n: u32, any: u32) -> u32 {
    let (r, rng) = rv();
    let mut k = 0;
    while k < n {
        let e = if any != 0 { r.undo_any(|m| ((rng.next() * m as f64) as usize).min(m - 1)) } else { r.undo_latest() };
        if e.is_none() { break; }
        k += 1;
    }
    k
}
/// Undo event e if it can be undone now. Returns 1 if it was.
#[no_mangle]
pub extern "C" fn rev_undo(e: u32) -> u32 {
    let r = &mut rv().0;
    if !r.undoable(e as usize) { return 0; }
    r.undo(e as usize);
    1
}
/// Fire the rewrite agent `id` takes part in, if any (`Rev::fire_at`). Returns the event + 1, or 0.
#[no_mangle]
pub extern "C" fn rev_fire(id: u32) -> u32 { rv().0.fire_at(id).map_or(0, |e| e as u32 + 1) }
/// The live net, into the seats, 7 numbers an agent: id, tag (`rev_code`), wanted, the event that made
/// it + 1 (0 if loaded), then each port's far end as id * 4 + port (all ones if none). Returns how many agents.
#[no_mangle]
pub extern "C" fn rev_net() -> u32 {
    let r = &rv().0;
    let mut v = vec![];
    for (i, a) in r.net.agents.iter().enumerate() {
        let Some(a) = a else { continue };
        v.extend([i as u32, rev_code(a.tag), r.want[i] as u32, r.maker[i].map_or(0, |m| m + 1)]);
        for q in 0..3 { v.push(a.ports[q].map_or(u32::MAX, |(b, p)| b * 4 + p as u32)); }
    }
    put_seats(v) / 7
}
/// Every event, into the seats: kind (`kind_code`), state (0 undone, 1 standing, 2 undoable now, 3 undone
/// and can be done again), how many
/// agents it made and consumed, the ids it made, then each consumed agent's id and tag. Returns how many events.
#[no_mangle]
pub extern "C" fn rev_events() -> u32 {
    let r = &rv().0;
    let mut v = vec![];
    for (e, ev) in r.events.iter().enumerate() {
        let Some(ev) = ev else {
            let again = r.undone.get(e).and_then(|x| x.as_ref());
            match again {
                Some(ev) => { v.extend([kind_code(ev.kind), 3, 0, ev.gone.len() as u32]); for (was, _) in &ev.gone { v.extend([was.id, rev_code(was.tag)]); } }
                None => v.extend([0, 0, 0, 0]),
            }
            continue;
        };
        v.extend([kind_code(ev.kind), 1 + r.undoable(e) as u32, ev.made.len() as u32, ev.gone.len() as u32]);
        v.extend(ev.made.iter().copied());
        for (was, _) in &ev.gone { v.extend([was.id, rev_code(was.tag)]); }
    }
    let n = r.events.len() as u32;
    put_seats(v);
    n
}
/// An event kind's name (`kind_code`), in the text.
#[no_mangle]
pub extern "C" fn rev_kind_name(k: u32) -> u32 {
    use crate::rev::Kind;
    set_text(&match k { 1000 => Kind::Uncopy, 1001 => Kind::Dead, 1002 => Kind::Unpaired, i => Kind::Rule(i as usize) }.name())
}
/// The value at the root if it is one, as the engine prints trees, in the text. Returns its length (0 if no value yet).
#[no_mangle]
pub extern "C" fn rev_answer() -> u32 { set_text(&rv().0.answer().map_or(String::new(), |t| sup::show(&STerm::from(&t)))) }
/// Whether nothing is left to do forward (1) and whether every event is undone and the net is as loaded (2).
#[no_mangle]
pub extern "C" fn rev_state() -> u32 {
    let r = &rv().0;
    r.idle() as u32 | if r.live_events() == 0 { 2 } else { 0 }
}

// ---- Two-way programs (iso.rs), for player/rev.html ----------------------------------------------

static mut ISO: Option<(crate::iso::Machine, u64)> = None;
#[allow(static_mut_refs)]
fn iso_m() -> &'static mut (crate::iso::Machine, u64) { unsafe { ISO.as_mut().expect("no two-way program loaded") } }

/// Load a two-way program call (`iso::parse_call`), run it to the end and back to see how long it is,
/// and leave the token at the beginning. Returns how many steps it takes, or -1 (the text says why).
/// The seats get, for each step, how many definitions are laid out and how many tree nodes are held.
#[no_mangle]
pub extern "C" fn iso_new(src: *const u8, len: usize) -> i64 {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let defs = iso_defs();
    let (main, input) = match crate::iso::parse_call(&defs, src) { Ok(x) => x, Err(e) => { set_text(&e); return -1; } };
    let mut m = crate::iso::Machine::new(defs, &main, &input);
    let mut prof = vec![m.depth as u32, m.nodes() as u32];
    loop {
        match m.step() {
            Ok(true) => prof.extend([m.depth as u32, m.nodes() as u32]),
            Ok(false) => break,
            Err(e) => { set_text(&format!("step {}: {e}", prof.len() / 2)); return -1; }
        }
        if prof.len() > 4_000_000 { set_text("over two million steps"); return -1; }
    }
    let n = prof.len() as u64 / 2 - 1;
    m.flip();
    if let Err(e) = m.run(u64::MAX) {
        // The run cannot go back (it forgets): start it again from its input.
        if !e.contains("forget") { set_text(&e); return -1; }
        m = crate::iso::Machine::new(iso_defs(), &main, &input);
    } else { m.flip(); }
    put_seats(prof);
    unsafe { ISO = Some((m, n)); }
    n as i64
}
/// Go to step t (0 the beginning), turning the token around as needed. Returns where it got to (it
/// stops where a step cannot be taken, as going back past a forget).
#[no_mangle]
pub extern "C" fn iso_seek(t: u32) -> i64 {
    let (m, _) = iso_m();
    let t = t as i64;
    if t != m.clock && (t > m.clock) != m.forward { m.flip(); }
    while m.clock != t { match m.step() { Ok(true) => {} Ok(false) => break, Err(e) => { set_text(&e); break; } } }
    m.clock
}
/// Where it is: `step total forward depth nodes forgotten` in the text.
#[no_mangle]
pub extern "C" fn iso_state() -> u32 { let (m, n) = iso_m(); set_text(&format!("{} {} {} {} {} {}", m.clock, n, m.forward as u8, m.depth, m.nodes(), m.forgotten)) }
/// What the token carries, as the engine prints trees.
#[no_mangle]
pub extern "C" fn iso_payload() -> u32 { set_text(&crate::iso::show(&iso_m().0.payload())) }
/// The program tree, into the seats, 6 numbers an instruction: id, parent's id + 1 (0 for the root), its
/// place among the parent's parts, kind (0 then, 1 rearrange, 2 copy, 3 on, 4 choose, 5 loop, 6 call,
/// 7 run, 8 forget, 9 a note; + 16 if it is the inverse: un-copy, a call or run backward), the root + 1 of the tree
/// parked on it (0 if none), and where the token is: 1 about to go in, 2 just come out, 0 elsewhere (+ 4
/// if going back in time). The text gets each one's label, then for a choice each arm's when and gives,
/// tab-separated, a line each.
#[no_mangle]
pub extern "C" fn iso_prog() -> u32 {
    use crate::iso::{Op, Way};
    let m = &iso_m().0;
    let (mut v, mut text) = (vec![], String::new());
    // Preorder from the root, so parents come before their parts.
    let mut stack = vec![m.root];
    while let Some(n) = stack.pop() {
        let p = m.pnode(n);
        let (kind, inv) = match &p.op {
            Op::Seq => (0, false), Op::Note => (9, false), Op::Re(..) => (1, false), Op::Dup(b) => (2, *b), Op::On(_) => (3, false), Op::Case(_) => (4, false),
            Op::Loop(..) => (5, false), Op::Call(_, b) => (6, *b), Op::Eval(b) => (7, *b), Op::Forget => (8, false),
        };
        let at = if m.at.0 == n { (if m.at.1 == Way::Down { 1 } else { 2 }) + (if m.forward { 0 } else { 4 }) } else { 0 };
        let (up, k) = p.up.map_or((0, 0), |(u, k)| (u + 1, k));
        v.extend([n, up, k, kind + if inv { 16 } else { 0 }, p.park.map_or(0, |r| r + 1), at]);
        text.push_str(&p.label);
        if let Op::Case(cs) = &p.op { for (w, g) in cs { text.push('\t'); text.push_str(&w.show()); text.push('\t'); text.push_str(&g.show()); } }
        text.push('\n');
        for &c in p.kids.iter().rev() { stack.push(c); }
    }
    set_text(&text);
    put_seats(v) / 6
}
/// The tree below data node `root` (as `iso_prog` gives roots, less one; u32::MAX for what the token
/// carries), into the seats, preorder, 2 numbers a node: kind (1 leaf, 2 stem, 3 fork, 4 hole, 5 a cut:
/// what is below is left out) and, for a cut, how many nodes it leaves out. At most `budget` nodes.
#[no_mangle]
pub extern "C" fn iso_tree(root: u32, budget: u32) -> u32 {
    use crate::iso::DK;
    let m = &iso_m().0;
    let root = if root == u32::MAX { m.payload } else { root };
    /// How many nodes are below n, counting shared ones each time (at most a million).
    fn size(m: &crate::iso::Machine, n: u32) -> u32 {
        let (mut count, mut stack) = (0u32, vec![n]);
        while let Some(k) = stack.pop() { count += 1; if count >= 1_000_000 { break; } for c in m.dnode(k).kids { if c != u32::MAX { stack.push(c); } } }
        count
    }
    let mut v = vec![];
    let mut left = budget as i64;
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        let d = m.dnode(n);
        if left <= 0 { v.extend([5, size(m, n)]); continue; }
        left -= 1;
        v.extend([match d.kind { DK::L => 1, DK::S => 2, DK::F => 3, DK::Hole => 4 }, 0]);
        for &k in d.kids.iter().rev() { if k != u32::MAX { stack.push(k); } }
    }
    put_seats(v) / 2
}

/// disp's programs that have jets (`iso::JETTED`), by name and tree, as the page gives them.
static mut JETS: Vec<(String, rust_ca_lattice::oracle::Term)> = vec![];
/// The library, with jets for the programs the page has given.
#[allow(static_mut_refs)]
fn iso_defs() -> crate::iso::Defs { crate::iso::library().with_jets(unsafe { &JETS }) }
/// Give disp's programs that jets stand for: a line each, its name and its tree as `#` and ternary
/// digits. Returns how many were taken.
#[no_mangle]
pub extern "C" fn iso_jets(src: *const u8, len: usize) -> u32 {
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let mut jets = vec![];
    for line in src.lines() {
        let Some((name, tree)) = line.split_once(' ') else { continue };
        let Ok((_, rust_ca_lattice::oracle::Term::F(t, _))) = crate::iso::parse_call(&crate::iso::library(), &format!("swap {tree} L")) else { continue };
        jets.push((name.to_string(), (*t).clone()));
    }
    let n = jets.len() as u32;
    unsafe { JETS = jets; }
    n
}

/// A call as the interaction-net page (player/inet.js) builds it: the main program, each definition it
/// reaches (forward or backward) and the input, as trees (`iso::encode`), in the text: `main\tTREE`,
/// `def D INV NAME\tTREE` a line each, then `input\tTREE`. Returns 1, or 0 (the text says why).
#[no_mangle]
pub extern "C" fn inet_program(src: *const u8, len: usize) -> u32 {
    use crate::iso::{encode, show, Iso};
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    let defs = iso_defs();
    let (main, input) = match crate::iso::parse_call(&defs, src) { Ok(x) => x, Err(e) => { set_text(&e); return 0; } };
    fn reach(defs: &crate::iso::Defs, i: &Iso, out: &mut Vec<(usize, bool)>) {
        match i {
            Iso::Call(d, v) => if !out.contains(&(*d, *v)) { out.push((*d, *v)); reach(defs, defs.body(*d, *v), out); },
            Iso::Seq(xs) => xs.iter().for_each(|x| reach(defs, x, out)),
            Iso::On(_, f) | Iso::Note(_, f) => reach(defs, f, out),
            Iso::Case(cs) => cs.iter().for_each(|c| reach(defs, &c.body, out)),
            Iso::Loop { body, again, .. } => { reach(defs, body, out); reach(defs, again, out) }
            Iso::Both(f, g) => { reach(defs, f, out); reach(defs, g, out) }
            _ => {}
        }
    }
    let mut out = vec![];
    reach(&defs, &main, &mut out);
    let mut text = format!("main\t{}\n", show(&encode(&main)));
    for (d, v) in out { text.push_str(&format!("def {d} {} {}\t{}\n", v as u8, defs.names[d], show(&encode(defs.body(d, v))))); }
    text.push_str(&format!("input\t{}", show(&input)));
    set_text(&text);
    1
}

// ---- The persistent two-way machine (walk.rs), for player/rev.html -------------------------------

/// The machine, how many moves the run is, and its call.
static mut WALK: Option<(crate::walk::Walker, u64, String)> = None;
#[allow(static_mut_refs)]
fn walk_m() -> &'static mut (crate::walk::Walker, u64, String) { unsafe { WALK.as_mut().expect("no persistent program loaded") } }
fn walk_fresh(src: &str) -> crate::walk::Walker {
    let defs = iso_defs();
    let (main, input) = crate::iso::parse_call(&defs, src).unwrap();
    crate::walk::Walker::new(defs, &main, &input)
}

/// Load a call as `iso::parse_call` reads it, on the persistent machine; run it to the end by rounds
/// and back, to see how long it is, and leave it at the start. Returns how many rounds, or -1 (the text
/// says why). The seats get, for the start and after each round: the tokens, the agents they carry,
/// their stacks' (and the forks'), the moves made so far, and the agents made and taken away so far.
#[no_mangle]
pub extern "C" fn walk_new(src: *const u8, len: usize) -> i64 {
    std::panic::set_hook(Box::new(|info| { set_text(&format!("engine panic: {info}")); }));
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") }.to_string();
    let defs = iso_defs();
    if let Err(e) = crate::iso::parse_call(&defs, &src) { set_text(&e); return -1; }
    let mut w = walk_fresh(&src);
    let sample = |w: &crate::walk::Walker| { let (_, d, s) = w.agents(); [w.tokens() as u32, d as u32, s as u32, w.clock as u32, w.made as u32, w.gone as u32] };
    let mut prof = sample(&w).to_vec();
    loop {
        match w.round() {
            Ok(0) => break,
            Ok(_) => prof.extend(sample(&w)),
            Err(e) => { set_text(&format!("round {}: {e}", prof.len() / 6)); return -1; }
        }
        if prof.len() > 6_000_000 { set_text("over a million rounds"); return -1; }
    }
    let n = w.clock as u64;
    w.flip();
    let back = (|| { while w.round()? > 0 {} Ok::<_, String>(()) })();
    match back {
        Err(e) if !e.contains("forget") => { set_text(&e); return -1; }
        Err(_) => w = walk_fresh(&src),
        Ok(()) => w.flip(),
    }
    let rounds = prof.len() as i64 / 6 - 1;
    put_seats(prof);
    unsafe { WALK = Some((w, n, src)); }
    rounds
}
/// Turn time to go forward (1) or back (0).
fn walk_way(fwd: u32) -> &'static mut crate::walk::Walker {
    let w = &mut walk_m().0;
    if w.forward != (fwd == 1) { w.flip(); }
    w
}
/// Past a forget going back: start again from the input (what was forgotten cannot run back).
fn walk_replay() { let m = walk_m(); m.0 = walk_fresh(&m.2); }
/// One round forward (1) or back (0). Returns how many moves (0 if none could be made).
#[no_mangle]
pub extern "C" fn walk_round(fwd: u32) -> u32 {
    match walk_way(fwd).round() {
        Ok(n) => n as u32,
        Err(e) if e.contains("forget") => { walk_replay(); set_text("replayed from the input: what was forgotten cannot run back"); 0 }
        Err(e) => { set_text(&e); 0 }
    }
}
/// One move forward (1) or back (0), by the first that can move. Returns 1 if one was made.
#[no_mangle]
pub extern "C" fn walk_step(fwd: u32) -> u32 {
    match walk_way(fwd).step() {
        Ok(b) => b as u32,
        Err(e) if e.contains("forget") => { walk_replay(); set_text("replayed from the input: what was forgotten cannot run back"); 0 }
        Err(e) => { set_text(&e); 0 }
    }
}
/// Go to where `t` moves are made: by rounds while a whole one fits, then a move at a time. Returns
/// where it got to.
#[no_mangle]
pub extern "C" fn walk_seek(t: u32) -> i64 {
    let t = t as i64;
    loop {
        let w = &walk_m().0;
        if w.clock == t { break; }
        let fwd = (t > w.clock) as u32;
        let gap = (t - w.clock).unsigned_abs() as usize;
        let room = w.actors().len() + w.forks.iter().flatten().count();
        let moved = if gap > room { walk_round(fwd) } else { walk_step(fwd) };
        if moved == 0 { break; }
    }
    walk_m().0.clock
}
/// `moves total forward rounds made gone code carried stack tokens forks` in the text.
#[no_mangle]
pub extern "C" fn walk_state() -> u32 {
    let (w, n, _) = walk_m();
    let (c, d, s) = w.agents();
    set_text(&format!("{} {} {} {} {} {} {} {} {} {} {}", w.clock, n, w.forward as u8, w.rounds, w.made, w.gone, c, d, s, w.tokens(), w.forks.iter().flatten().count()))
}
/// The program's tree, into the seats, 5 numbers a node: id, kind (1 leaf, 2 stem, 3 fork), its parent's
/// id + 1 (0 for a root), which child of it (0, 1), and, if an instruction's node, what it does + 1 (0
/// then, 1 rearrange, 2 copy, 3 on, 4 choose, 5 loop, 6 call, 7 run, 8 forget, 9 note, 10 both; + 16
/// backward). The text gets each instruction node's id and label, tab-separated, a line each.
#[no_mangle]
pub extern "C" fn walk_code() -> u32 {
    use crate::walk::Op;
    let w = &walk_m().0;
    let (mut v, mut text) = (vec![], String::new());
    for (i, c) in w.code.iter().enumerate() {
        let Some(c) = c else { continue };
        let i = i as u32;
        let up = if c.up == u32::MAX { 0 } else { c.up + 1 };
        let k = if c.up != u32::MAX && w.cnode(c.up).kids[1] == i { 1 } else { 0 };
        let what = w.instr.get(&i).map_or(0, |ins| 1 + match ins.op {
            Op::Seq => 0, Op::Re(..) => 1, Op::Dup(b) => 2 + 16 * b as u32, Op::On(_) => 3, Op::Case(_) => 4, Op::Loop(..) => 5,
            Op::Call(_, b) => 6 + 16 * b as u32, Op::Eval(b) => 7 + 16 * b as u32, Op::Forget => 8, Op::Note => 9, Op::Both => 10 });
        let kind = match c.kind { crate::iso::DK::L => 1, crate::iso::DK::S => 2, _ => 3 };
        v.extend([i, kind, up, k, what]);
        if let Some(ins) = w.instr.get(&i) { text.push_str(&format!("{i}\t{}\n", ins.label)); }
    }
    set_text(&text);
    put_seats(v) / 5
}
/// The tokens, into the seats, 8 numbers each: its number, its node, 1 if it is about to come out of it
/// (else about to go in), whether it goes through the program backward (inside an odd number of calls
/// run backward), the tree it carries, how deep its stack is, the fork it is a half of + 1 (0 if none),
/// and which half (0 left, 1 right).
#[no_mangle]
pub extern "C" fn walk_tokens() -> u32 {
    use crate::walk::Frame;
    let w = &walk_m().0;
    let mut v = vec![];
    for (i, t) in w.toks.iter().enumerate() {
        let Some(t) = t else { continue };
        let (half, side) = match t.stack.first() { Some(&Frame::Half { fork, side }) => (fork + 1, side as u32), _ => (0, 0) };
        v.extend([i as u32, t.at.0, (t.at.1 == crate::iso::Way::Up) as u32, t.parity as u32, t.payload, t.stack.len() as u32, half, side]);
    }
    put_seats(v) / 8
}
/// The forks, into the seats, 5 numbers each: its number, its `both` node, its pair, and where each half
/// is (0 away, 1 back at its part's start, 2 at its end).
#[no_mangle]
pub extern "C" fn walk_forks() -> u32 {
    use crate::walk::Slot;
    let w = &walk_m().0;
    let slot = |s: Slot| match s { Slot::Away => 0, Slot::Start => 1, Slot::End => 2 };
    let mut v = vec![];
    for (j, f) in w.forks.iter().enumerate() { if let Some(f) = f { v.extend([j as u32, f.node, f.pair, slot(f.halves[0]), slot(f.halves[1])]); } }
    put_seats(v) / 5
}
/// The instructions whose rules the last move or round fired, into the seats. Returns how many.
#[no_mangle]
pub extern "C" fn walk_fired() -> u32 { put_seats(walk_m().0.fired.clone()) }
/// A token's stack, into the seats, bottom first, 4 numbers a frame: 0 a parked tree, 1 a return point,
/// 2 a run (its program parked), 3 the fork it is a half of; the tree's root + 1 (0 if none); the call's
/// node + 1 (the fork's number + 1 for a half); 1 if backward (the side, for a half).
#[no_mangle]
pub extern "C" fn walk_stack(tok: u32) -> u32 {
    use crate::walk::Frame;
    let w = &walk_m().0;
    let mut v = vec![];
    if let Some(Some(t)) = w.toks.get(tok as usize) {
        for f in &t.stack {
            v.extend(match *f { Frame::Park(r) => [0, r + 1, 0, 0], Frame::Ret { call, inv } => [1, 0, call + 1, inv as u32],
                Frame::Eval { call, inv, prog, .. } => [2, prog + 1, call + 1, inv as u32], Frame::Half { fork, side } => [3, 0, fork + 1, side as u32] });
        }
    }
    put_seats(v) / 4
}
/// A tree the persistent machine holds, as `iso_tree` gives one.
#[no_mangle]
pub extern "C" fn walk_tree(root: u32, budget: u32) -> u32 {
    use crate::iso::DK;
    let w = &walk_m().0;
    let size = |n: u32| { let (mut count, mut stack) = (0u32, vec![n]); while let Some(k) = stack.pop() { count += 1; if count >= 1_000_000 { break; } for c in w.dnode(k).kids { if c != u32::MAX { stack.push(c); } } } count };
    let mut v = vec![];
    let mut left = budget as i64;
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        let d = w.dnode(n);
        if left <= 0 { v.extend([5, size(n)]); continue; }
        left -= 1;
        v.extend([match d.kind { DK::L => 1, DK::S => 2, DK::F => 3, DK::Hole => 4 }, 0]);
        for &k in d.kids.iter().rev() { if k != u32::MAX { stack.push(k); } }
    }
    put_seats(v) / 2
}
/// A tree the persistent machine holds, as the engine prints trees (holes as `_`).
#[no_mangle]
pub extern "C" fn walk_show(root: u32) -> u32 {
    use crate::iso::DK;
    let w = &walk_m().0;
    fn go(w: &crate::walk::Walker, n: u32, out: &mut String) {
        let d = w.dnode(n);
        match d.kind { DK::L => out.push('L'), DK::Hole => out.push('_'), DK::S => { out.push_str("S("); go(w, d.kids[0], out); out.push(')') }
            DK::F => { out.push_str("F("); go(w, d.kids[0], out); out.push(','); go(w, d.kids[1], out); out.push(')') } }
    }
    let mut s = String::new();
    go(w, root, &mut s);
    set_text(&s)
}
