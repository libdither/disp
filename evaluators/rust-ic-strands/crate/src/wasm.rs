//! The browser build: the strand lattice driven from JavaScript through a flat C ABI. The
//! player draws straight from the lattice's own arrays.

use crate::lattice::{latest, Lattice, Params, Stats};
use crate::twoway::{self, Prog, Val};
use rust_ca_lattice::oracle;
use rust_ca_lattice::sup::{self, STerm};
use rust_ic_mesh::term;

/// `parts`: the reductions the input asked for, separated by `;` (`parse_all`), one term each.
/// `start`: under the substrate, the abstract net as loaded, to tell when a run has come all the way back.
/// `two`: a two-way program and the value it runs on (`two:` input), instead of a term.
struct State { l: Lattice, term: STerm, parts: Vec<STerm>, done: bool, start: Vec<Option<rust_ca_lattice::net::Agent>>, two: Option<(Prog, Val)> }
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
    if let Some(run) = src.trim().strip_prefix("two:") { return two_new(w, h, run); }
    let (t, parts) = match parse_all(src) { Ok(t) => t, Err(e) => { set_text(&e); return 1; } };
    let p = Params { w, h, ..*next() };
    if p.margolus && !p.block { set_text("2×2×2 blocks need rewrites inside one 2×2 block"); return 3; }
    let (net, out) = match net_for(&t, &p) { Ok(n) => n, Err(e) => { set_text(&e); return 1; } };
    let start = if p.phys { net.agents.clone() } else { vec![] };
    match Lattice::load(p, net, out) {
        Ok(mut l) => {
            if l.p.phys { l.schedule = vec![(0.0, [l.p.drive, l.p.erase, l.p.forget, l.p.bond])]; }
            unsafe { STATE = Some(State { l, term: t, parts, done: false, start, two: None }); SAVED.clear(); }
            0
        }
        Err(e) => { set_text(&e); 2 }
    }
}

/// A two-way program on a value (twoway.rs `parse_run`), run eagerly with 4 ports a slot.
#[allow(static_mut_refs)]
fn two_new(w: u32, h: u32, run: &str) -> i32 {
    let (prog, x) = match twoway::parse_run(run) { Ok(r) => r, Err(e) => { set_text(&e); return 1; } };
    let mut p = Params { w, h, ..*next() };
    if let Err(e) = p.set("two", "1") { set_text(&e); return 1; }
    let (net, out) = twoway::net(&prog, &x);
    match Lattice::load(p, net, out) {
        Ok(l) => { unsafe { STATE = Some(State { l, term: STerm::L, parts: vec![], done: false, start: vec![], two: Some((prog, x)) }); SAVED.clear(); } 0 }
        Err(e) => { set_text(&e); 2 }
    }
}

/// A two-way run's answer once it has finished: the result, if the program came back as it went in;
/// "stuck" where no rule fits (a swap of a leaf, a merge of two different halves).
fn two_answer(s: &State, decoded: bool) -> String {
    let (prog, _) = s.two.as_ref().unwrap();
    match twoway::answer(&s.l.shadow) {
        Some((q, y)) if q == prog.code() => if decoded { y.decoded() } else { y.show() },
        Some((q, _)) => format!("the program came back changed: {}", twoway::Prog::of(&q).map_or(q.show(), |p| p.show())),
        None => "stuck".into(),
    }
}

/// The net for a term with the next run's settings, as `load::net_for` builds it. Keeping history runs
/// only on the CPU (strands-resid).
fn net_for(t: &STerm, p: &Params) -> Result<(rust_ca_lattice::net::Net, u32), String> {
    if p.resid > 0 { return Err("keeping history runs on the CPU only (strands-resid)".into()); }
    crate::load::net_for(t, p)
}

/// Ports a slot's switchboard has: 3, or 4 keeping history or running two-way programs.
#[no_mangle]
pub extern "C" fn ports_per_slot() -> u32 { st().l.ar as u32 }

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
    if s.two.is_some() { return set_text(&if s.done { two_answer(s, false) } else { String::new() }); }
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
    if s.two.is_some() { return set_text(&if s.done { two_answer(s, true) } else { String::new() }); }
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
    if let Some((prog, x)) = &st().two { return set_text(&prog.run(x, &mut (fuel as u64)).map_or("stuck".into(), |y| y.show())); }
    let t = &st().term;
    match sup::oracle_answers(t, fuel as u64 * 1000) { Some(u) => set_text(&sup::show(&sup::collapse(&u, &t.labels()))), None => set_text("") }
}

/// The size of the term's initial drawing (agents), or -1 with the parse error in the text.
#[no_mangle]
pub extern "C" fn strands_probe(src: *const u8, len: usize) -> i32 {
    let src = unsafe { std::str::from_utf8(std::slice::from_raw_parts(src, len)).unwrap_or("") };
    if let Some(run) = src.trim().strip_prefix("two:") {
        return match twoway::parse_run(run) { Ok((prog, x)) => twoway::net(&prog, &x).0.agents.len() as i32, Err(e) => { set_text(&e); -1 } };
    }
    match parse_all(src) {
        Ok((t, _)) => match net_for(&t, next()) { Ok((n, _)) => n.agents.len() as i32, Err(e) => { set_text(&e); -1 } },
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

/// Words a site takes in `strands_held` (lattice.rs `SiteBytes`): 10, or 11 with 4 ports a slot.
#[no_mangle]
pub extern "C" fn site_words() -> u32 { st().l.site_bytes().words as u32 }

/// The sites holding anything or a field (each its index, its `site_words` words, its field), into the words.
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
/// Under the substrate the whole run is kept instead (`snap`), ids and records included.
struct Saved { held: Vec<u32>, labels: Vec<u32>, stats: Stats, done: bool, temp: f64, cooling: Option<(f64, f64)>, snap: Option<crate::lattice::substrate::Snap> }
static mut SAVED: Vec<Option<Saved>> = Vec::new();

/// Save the run as it is now; returns the saved state's number.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn save_state() -> u32 {
    let s = st();
    let v = if s.l.p.phys { Saved { held: vec![], labels: vec![], stats: s.l.stats.clone(), done: s.done, temp: s.l.p.temp, cooling: s.l.cooling, snap: Some(s.l.snap()) } }
        else { Saved { held: s.l.held_sites(), labels: s.l.held_labels(), stats: s.l.stats.clone(), done: s.done, temp: s.l.p.temp, cooling: s.l.cooling, snap: None } };
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
    if let Some(snap) = &v.snap {
        s.l.put_snap(snap);
        s.done = v.done;
        return;
    }
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
    unsafe { SAVED[i as usize].as_ref().map_or(0, |v| ((v.held.len() + v.labels.len()) * 4 + std::mem::size_of::<Saved>() + v.snap.as_ref().map_or(0, |x| x.bytes())) as u32) }
}

// ---- the substrate (substrate.rs) -----------------------------------------------------------------

/// The substrate's settings from this clock on (lattice.rs `Lattice::reset_schedule`): the drive ε,
/// the chance an eraser takes a leaf, the chance a turn an agent forgets what made it, and the bond.
#[no_mangle]
pub extern "C" fn strands_substrate(drive: f64, erase: f64, forget: f64, bond: f64) { st().l.reset_schedule([drive, erase, forget, bond]); }

/// 1 when a substrate run has come all the way back: no record left and the net exactly as loaded.
#[no_mangle]
pub extern "C" fn strands_at_start() -> u32 { let s = st(); (s.l.records_kept() == 0 && s.l.same_net(&s.start)) as u32 }

/// Whether the answer is in now (not once: under the substrate it can be undone again).
#[no_mangle]
pub extern "C" fn strands_answered() -> u32 { st().l.answered() as u32 }

/// The seats of agents kept records tie together, two by two, into the seats (`Lattice::bonds`).
/// Returns how many pairs.
#[no_mangle]
pub extern "C" fn substrate_bonds() -> u32 { put_seats(st().l.bonds()) / 2 }

static mut SUB: [f64; 20] = [0.0; 20];
/// The substrate's account: heat in all, from the drive, leaves erased, records forgotten; rewrites
/// forward and undone (records' ledger), un-copies kept, records kept now, erasers now, agents now
/// and loaded, rewrites undone (stats), undos tried apart and without room, then the settings
/// (drive, erase, forget, bond) and the temperature.
#[no_mangle]
#[allow(static_mut_refs)]
pub extern "C" fn substrate_ptr() -> *const f64 {
    let l = &st().l;
    let g = &l.sub.ledger;
    let v = [g.heat(), g.drive, g.erased as f64, g.forgot as f64, g.forward as f64, g.back as f64, g.uncopied as f64, l.records_kept() as f64,
             l.erasers() as f64, l.shadow.live_count() as f64, g.loaded as f64, l.stats.unfires as f64, l.stats.undo_far as f64, l.stats.undo_full as f64,
             l.p.drive, l.p.erase, l.p.forget, l.p.bond, l.p.temp, l.p.phys as u8 as f64];
    unsafe { SUB = v; SUB.as_ptr() }
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
pub extern "C" fn strands_net() -> u32 { put_seats(readback::wires(&st().l)) / 7 }
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
    let (net, _) = crate::load::net_sup(&t);
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
