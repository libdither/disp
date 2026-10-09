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
    let (net, out) = crate::share::net_sup(&t, p.share);
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
        Ok((t, _)) => crate::share::net_sup(&t, next().share).0.agents.len() as i32,
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
use rust_ca_lattice::rules::{Tag, ALL_TAGS};

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
fn code(t: Tag) -> u32 { ALL_TAGS.iter().position(|x| *x == t).unwrap() as u32 + 1 }
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
