// Who takes a turn when, a turn's stages in order, and the kernels (hw/rtl/strands_block.v and
// strands_lattice.v; lattice.rs `margolus_clock`, `turn`, `step_pulses`).

/// One clock's parameters: the lattice's size, the clock mixed with the seed (lattice.rs `tick`),
/// blocks per axis, how many invocations a full-lattice dispatch covers; the next clock's tick, the
/// stamp that marks a block as on its busy list (busy.wgsl), the list that clock runs and the list
/// to empty; then the clock's parity (which pulse buffer it writes), whether its turns start with
/// the previous clock's pulse phase, and which busy list it runs. `zero` is always 0 (see `rolled`).
struct Clock { w: u32, h: u32, d: u32, tick: u32, nbx: u32, nby: u32, nbz: u32, n: u32,
               tick_next: u32, stamp: u32, next: u32, clear: u32, zero: u32, pad1: u32, par: u32, fused: u32,
               list: u32, pad2: u32, pad3: u32, pad4: u32 }
@group(0) @binding(0) var<uniform> clk: Clock;
/// Every site, 10 words each, at x + w * (y + h * z).
@group(0) @binding(1) var<storage, read_write> sites: array<u32>;
/// Every site's pulse once a clock's turns are done, in two buffers by the clock's parity: a pulse
/// phase reads its neighbours' from the clock before while the turns write this clock's.
@group(0) @binding(2) var<storage, read_write> pul: array<u32>;
/// Recorded turns and blocks to replay (the `vectors` kernel), REC words each; or the sites holding
/// anything, gathered to look for the answer (busy.wgsl `busy_gather`).
@group(0) @binding(3) var<storage, read_write> recs: array<u32>;

/// The clock mixed with the seed.
var<private> tick: u32;
/// The block's corner (its position 0), which may lie off the lattice.
var<private> corner: vec3<i32>;

/// What this invocation's turns and pulses did, by `T_*` (busy.wgsl), added to `counts` at the end.
var<private> tally: array<u32, 11>;

/// The block's place in the lattice (the RTL's `edges`), in the RTL's wrapping arithmetic.
fn edges(c: vec3<i32>, w: u32, h: u32, d: u32) {
  corner = c; onl = 0u; lat = 0u;
  let size = vec3<u32>(w, h, d);
  for (var a = 0u; a < 3u; a++) {
    for (var b = 0u; b < 2u; b++) {
      let x = u32(c[a] + i32(b));
      lat |= (u32(x + 1u < size[a]) | (u32(x != 0u) << 1u)) << (4u * a + 2u * b);
    }
  }
  for (var q = 0u; q < 8u; q++) {
    let x = u32(c.x + i32(q & 1u)); let y = u32(c.y + i32((q >> 1u) & 1u)); let z = u32(c.z + i32((q >> 2u) & 1u));
    if (x < w && y < h && z < d) { onl |= 1u << q; }
  }
}
/// A position's key for its dice: its coordinates, 10 bits each.
fn key(q: u32) -> u32 {
  let x = u32(corner.x + i32(q & 1u)); let y = u32(corner.y + i32((q >> 1u) & 1u)); let z = u32(corner.z + i32((q >> 2u) & 1u));
  return (x & 1023u) | ((y & 1023u) << 10u) | ((z & 1023u) << 20u);
}

/// A consumer at position q whose partner is here or one strand away (lattice.rs `active_pair`),
/// as the block stood before its first turn; `seen`: the positions across a strand it looked at.
struct Pair { found: bool, stale: bool, via: bool, inf: bool, kc: u32, kp: u32, face: u32, inf_pos: u32, inf_k: u32, seen: u32 }
fn active_pair(q: u32) -> Pair {
  var r = Pair(false, false, false, false, 0u, 0u, 0u, 0u, 0u, 0u);
  for (var k = 0u; k < 3u; k++) {
    let t = gt(q, k); let m = gm(q, ae(k, 0u));
    var skip = t == 0u || !is_consumer(t) || (LAZY && !gw(q, k)) || m == NONE;
    var sp = q; var mm = m; var f = 0u;
    if (!skip && is_strand(m)) {
      f = face(m); let nn = inb(q, f);
      if ((nn & 8u) == 0u) { skip = true; }
      else { r.seen |= 1u << (nn & 7u); sp = nn & 7u; mm = gm(sp, se(f ^ 1u, lane(m))); }
    }
    if (!skip && !is_strand(mm) && mm != NONE) {
      let k2 = port_k(mm); let qq = port_q(mm); let t2 = gt(sp, k2);
      if (qq == 0u && t2 != 0u && is_producer(t2)) {
        r.found = true; r.kc = k; r.kp = k2; r.via = is_strand(m); r.face = f; return r;
      } else if (LAZY && qq != 0u && t2 != 0u && is_consumer(t2) && t != T_EPS) {
        r.inf = true; r.inf_pos = sp; r.inf_k = k2;
      }
    }
  }
  return r;
}
/// Position q's pair, noted in slot PAIR for its turn.
fn note_pair(q: u32) -> Pair {
  let r = active_pair(q);
  sb[at(PAIR, q)] = u32(r.found) | (u32(r.via) << 1u) | (u32(r.inf) << 2u) | (r.kc << 3u) | (r.kp << 5u) | (r.face << 7u)
                  | (r.inf_pos << 10u) | (r.inf_k << 13u) | (r.seen << 16u);
  return r;
}
/// Position q's pair as noted. Turns since change only the positions they took, and q is not one
/// of them, so the pair is the same unless it looked across a strand at a taken position: then
/// the turn read stale state (lattice.rs `active_pair` stops at the first such look).
fn noted_pair(q: u32) -> Pair {
  let v = sb[at(PAIR, q)];
  return Pair((v & 1u) != 0u, (taken & (v >> 16u)) != 0u, (v & 2u) != 0u, (v & 4u) != 0u, (v >> 3u) & 3u, (v >> 5u) & 3u,
              (v >> 7u) & 7u, (v >> 10u) & 7u, (v >> 13u) & 3u, v >> 16u);
}

fn vwanted_reader(s: Site, k: u32) -> bool { let t = vgt(s, k); return t != 0u && is_consumer(t) && t != T_EPS && vgw(s, k); }
/// Bit k: slot k of position s holds a wanted reader (its tags are word 8's top three bytes, its
/// wanted bits word 9's low three).
fn readers(s: u32) -> u32 {
  let tags = sb[at(s, 8u)] >> 8u; let wants = sb[at(s, 9u)];
  var m = 0u;
  for (var k = 0u; k < 3u; k++) {
    let t = (tags >> (8u * k)) & 0xFFu;
    if (t != 0u && is_consumer(t) && t != T_EPS && ((wants >> (8u * k)) & 0xFFu) != 0u) { m |= 1u << k; }
  }
  return m;
}
/// Bit k: slot k of position s holds an agent that walks along its principal wire on an active
/// turn: a wanted reader, or (with `CALLS`) a called value.
fn walkers(s: u32) -> u32 {
  var m = readers(s);
  if (CALLS) {
    let tags = sb[at(s, 8u)] >> 8u; let wants = sb[at(s, 9u)];
    for (var k = 0u; k < 3u; k++) {
      let t = (tags >> (8u * k)) & 0xFFu;
      if (t != 0u && is_producer(t) && ((wants >> (8u * k)) & 0xFFu) != 0u) { m |= 1u << k; }
    }
  }
  return m;
}
/// The wanted reader an active turn moves (the RTL's `active_reader`), among `rd` (bit k: slot k).
fn reader_k(rd: u32) -> u32 {
  var ksl = 0u; var n = 0u;
  for (var k = 0u; k < 3u; k++) { if (((rd >> k) & 1u) != 0u) { ksl |= k << (2u * n); n++; } }
  return (ksl >> (2u * pick5(d_agent(), n))) & 3u;
}

const ST_PAIR: u32 = 0u; const ST_MOVE: u32 = 1u; const ST_COLLECT: u32 = 2u; const ST_FIRE: u32 = 3u;
/// Position p's turn (lattice.rs `turn`): collect, else react, else take an infection and step,
/// exchange, fold or flip; then drop the pulses whose strand was rewired. The stages run in a
/// loop with the rare ones (collecting, a rewrite) last in it, so that the code a turn usually
/// runs lies together: the GPU fetches less of it.
fn turn() {
  dice = hash(key(p), tick); touched = 0u; stale = false;
  let eraser = (gt(p, 0u) == T_EPS && gm(p, ae(0u, 0u)) != NONE) || (gt(p, 1u) == T_EPS && gm(p, ae(1u, 0u)) != NONE);
  var st = select(ST_PAIR, ST_COLLECT, GC && eraser);
  var pr: Pair;
  loop {
    if (st == ST_PAIR) {
      pr = noted_pair(p);
      if (pr.stale) { stale = true; break; }
      st = select(ST_MOVE, ST_FIRE, pr.found);
    }
    if (st == ST_MOVE) {
      let rd = walkers(p);
      let active_mode = d_active() < CH_ACTIVE && rd != 0u;
      let act_k = reader_k(rd);
      // A reader that touched a pending computation wants it now (lattice.rs `take_infect`).
      if (pr.inf && !gw(pr.inf_pos, pr.inf_k)) { sw(pr.inf_pos, pr.inf_k, true); touched |= 1u << pr.inf_pos; }
      move_stage(active_mode, act_k);
      break;
    }
    if (st == ST_COLLECT) {
      if (collect_stage()) { break; }
      st = ST_PAIR;
      continue;
    }
    // A rewrite that finds no room writes nothing, and the turn goes on to a move.
    let r = fire(pr.kc, pr.kp, pr.via, pr.face);
    if (r == 0u) { tally[T_BLOCKED]++; st = ST_MOVE; continue; }
    if (r == 1u) { tally[T_FIRES]++; log_fire(site_at(corner, p)); }
    break;
  }
  // A pulse whose strand's mate changed during the turn is lost (only a touched position changes).
  var tb = touched;
  while (tb != 0u) {
    let q = firstTrailingBit(tb);
    tb &= tb - 1u;
    let e = gpul(q);
    if (e != NONE && gm(q, e) != pmate(q)) { spul(q, NONE); }
    set_pmate(q);
  }
  taken |= touched;
}

/// Byte q: the mate of the strand end position q's pulse sits on (none without a pulse), as it
/// was before the turn under way; kept up to date as turns touch positions.
var<private> pmates: vec2<u32>;
fn pmate(q: u32) -> u32 { return (pmates[q >> 2u] >> (8u * (q & 3u))) & 0xFFu; }
fn set_pmate(q: u32) {
  let i = q >> 2u; let sh = 8u * (q & 3u);
  pmates[i] = (pmates[i] & ~(0xFFu << sh)) | (gm(q, gpul(q)) << sh);
}
fn set_pmates() { for (var q = 0u; q < 8u; q++) { set_pmate(q); } }

/// The block's turns (lattice.rs `margolus_clock` with `block_moves`): sites with a reaction
/// ready, then with agents, then bare wire, each in block order rotated by the block's random
/// word; a site an earlier turn changed sits out.
fn run_block() {
  taken = 0u;
  set_pmates();
  var cls: array<u32, 8>;
  for (var q = 0u; q < 8u; q++) {
    var c = 3u;
    if ((onl & (1u << q)) != 0u) {
      if (occ(q) != 0u) { c = select(1u, 0u, note_pair(q).found); }
      else { sb[at(PAIR, q)] = 0u; if (mv_strands(q) != 0u) { c = 2u; } }
    }
    cls[q] = c;
  }
  let bkey = (u32(corner.x + 1) & 1023u) | ((u32(corner.y + 1) & 1023u) << 10u) | ((u32(corner.z + 1) & 1023u) << 20u) | (1u << 31u);
  let rot = hash(bkey, tick).x & 7u;
  // The positions in turn order, 3 bits each, so that a wave runs as many turns as its busiest
  // block has sites rather than one per position and group.
  var order = 0u; var n = 0u;
  for (var bi = 0u; bi < 24u; bi++) {
    let q = ((bi & 7u) + rot) & 7u;
    if (cls[q] == (bi >> 3u)) { order |= q << (3u * n); n++; }
  }
  for (var j = 0u; j < n; j++) {
    let q = (order >> (3u * j)) & 7u;
    if ((taken & (1u << q)) == 0u) { p = q; tally[T_TURNS]++; turn(); }
  }
}

/// The site at position q of the block with corner c, or none off the lattice.
fn site_at(c: vec3<i32>, q: u32) -> u32 {
  let x = c.x + i32(q & 1u); let y = c.y + i32((q >> 1u) & 1u); let z = c.z + i32((q >> 2u) & 1u);
  if (x < 0 || y < 0 || z < 0 || u32(x) >= clk.w || u32(y) >= clk.h || u32(z) >= clk.d) { return 0xFFFFFFFFu; }
  return u32(x) + clk.w * (u32(y) + clk.h * u32(z));
}
fn load(s: u32) -> Site { var t: Site; for (var i = 0u; i < 10u; i++) { t[i] = sites[s * 10u + i]; } return t; }
fn store(s: u32, t: Site) { for (var i = 0u; i < 10u; i++) { sites[s * 10u + i] = t[i]; } }
/// Invocation g's index in a dispatch of n workgroups of `size`.
fn invocation(g: vec3<u32>, n: vec3<u32>, size: u32) -> u32 { return g.x + g.y * n.x * size; }

fn nsites() -> u32 { return clk.w * clk.h * clk.d; }
/// A site's pulse word after the previous clock's turns: the strand end its pulse sits on (low
/// byte, NONE without one) and its demand field (the next byte; lattice.rs `update_fields`).
fn pul_read(s: u32) -> u32 { return pul[(clk.par ^ 1u) * nsites() + s]; }
/// A site's pulse word in the buffer this clock's turns write.
fn pul_here(s: u32) -> u32 { return pul[clk.par * nsites() + s]; }
fn set_pul(s: u32, v: u32) { pul[clk.par * nsites() + s] = v; }
/// A pulse word with no pulse and no field.
const QUIET: u32 = 0xFFu;
fn pw_end(w: u32) -> u32 { return w & 0xFFu; }
fn pw_field(w: u32) -> u32 { return (w >> 8u) & 0xFFu; }

/// Bits 4q .. 4q + 3: position q's demand field this clock.
var<private> fld: u32;
fn fld_at(q: u32) -> u32 { return (fld >> (4u * (q & 7u))) & 15u; }
/// Site s's demand field this clock (lattice.rs `update_fields`): `src` if it is a source (a
/// wanted reader or a demand pulse there), its own value of the clock before less the fade, and
/// each neighbour's less the falloff, whichever is most.
fn field_of(s: u32, src: bool) -> u32 {
  let x = s % clk.w; let y = (s / clk.w) % clk.h; let z = s / (clk.w * clk.h);
  let on = array<bool, 6>(x + 1u < clk.w, x > 0u, y + 1u < clk.h, y > 0u, z + 1u < clk.d, z > 0u);
  let nb = array<u32, 6>(s + 1u, s - 1u, s + clk.w, s - clk.w, s + clk.w * clk.h, s - clk.w * clk.h);
  var fs: array<u32, 6>;
  for (var f = 0u; f < 6u; f++) { fs[f] = select(0u, pw_field(pul_read(select(s, nb[f], on[f]))), on[f]); }
  var v = select(0u, F_LEVEL, src);
  let own = pw_field(pul_read(s));
  if (F_DECAY != NONE && own > F_DECAY) { v = max(v, own - F_DECAY); }
  for (var f = 0u; f < 6u; f++) { if (F_STEP != NONE && fs[f] > F_STEP) { v = max(v, fs[f] - F_STEP); } }
  return min(v, F_CAP);
}
/// The corner of block b at this clock's offset (a hash of the clock).
fn block_corner(b: vec3<i32>) -> vec3<i32> {
  let o = hash(0xFFFFFFFFu, clk.tick).x;
  return 2 * b - vec3<i32>(i32(o & 1u), i32((o >> 8u) & 1u), select(0, i32((o >> 16u) & 1u), clk.d > 1u));
}

/// Position q's site, already loaded as t, into its slot (EMPTY off the lattice); whether it holds
/// anything.
fn stage(q: u32, t: Site) -> u32 {
  let on = (onl & (1u << q)) != 0u;
  var any = 0u;
  for (var i = 0u; i < 10u; i++) { let v = select(EMPTY[i], t[i], on); if (v != EMPTY[i]) { any = 1u << q; } sb[at(q, i)] = v; }
  return any;
}

/// Block b's turns. With `fused`, its sites first take the previous clock's pulse phase. Every
/// site's pulse goes to this clock's buffer afterwards, so a site that emptied leaves no stale
/// pulse behind. A site that holds something, or has a pulse left in the buffer the next clock
/// writes (to be cleared then), puts its block on the next clock's busy list. Loads are issued
/// together rather than site by site, since each waits on memory.
fn block_turns(b: vec3<i32>) {
  tick = clk.tick;
  next_clock();
  let c = block_corner(b);
  edges(c, clk.w, clk.h, clk.d);
  if (onl == 0u) { return; }
  // A position off the lattice reads site 0 and is ignored.
  var sx: array<u32, 8>;
  for (var q = 0u; q < 8u; q++) { sx[q] = select(0u, site_at(c, q), (onl & (1u << q)) != 0u); }
  let t0 = load(sx[0]); let t1 = load(sx[1]); let t2 = load(sx[2]); let t3 = load(sx[3]);
  let t4 = load(sx[4]); let t5 = load(sx[5]); let t6 = load(sx[6]); let t7 = load(sx[7]);
  // Bit q: a pulse or a field in the buffer this clock writes, and in the previous clock's.
  var here = 0u; var before = 0u;
  for (var q = 0u; q < 8u; q++) {
    here |= u32(pul_here(sx[q]) != QUIET) << q;
    before |= u32(pul_read(sx[q]) != QUIET) << q;
  }
  here &= onl; before &= onl;
  let any = stage(0u, t0) | stage(1u, t1) | stage(2u, t2) | stage(3u, t3) | stage(4u, t4) | stage(5u, t5) | stage(6u, t6) | stage(7u, t7);
  var dirty = 0u;
  if (clk.fused != 0u) {
    for (var q = 0u; q < 8u; q++) { if ((any & (1u << q)) != 0u) { dirty |= pulse_at(q, sx[q]); } }
  }
  // The demand field, from the sites as the pulse phase left them; empty sites have one too.
  fld = 0u;
  if (FIELD) {
    for (var q = 0u; q < 8u; q++) {
      if ((onl & (1u << q)) == 0u) { continue; }
      let src = (F_READER && readers(q) != 0u) || (F_PULSE && gpul(q) != NONE);
      fld |= field_of(sx[q], src) << (4u * q);
    }
  }
  if (any == 0u && here == 0u && (!FIELD || (before == 0u && fld == 0u))) { return; }
  taken = 0u;
  if (any != 0u) { run_block(); }
  var held = before;
  for (var q = 0u; q < 8u; q++) {
    if ((onl & (1u << q)) != 0u) {
      let s = sx[q];
      if (((taken | dirty) & (1u << q)) != 0u) { store(s, get_site(q)); }
      set_pul(s, gpul(q) | (fld_at(q) << 8u));
      for (var i = 0u; i < 10u; i++) { if (sb[at(q, i)] != EMPTY[i]) { held |= 1u << q; } }
      if (fld_at(q) != 0u) { held |= 1u << q; }
    }
  }
  if (clk.next != NO_LIST) {
    mark_next(c, held);
    // A field strong enough to reach a neighbour next clock puts the neighbour's block on the list.
    if (FIELD && F_STEP != NONE) {
      for (var q = 0u; q < 8u; q++) {
        if ((onl & (1u << q)) == 0u || fld_at(q) <= F_STEP) { continue; }
        for (var f = 0u; f < 6u; f++) { if (onlat(q, f)) { mark_next_site(site_at(c, q) + face_step(f)); } }
      }
    }
  }
  add_tally();
}
/// Every block's turns for this clock.
@compute @workgroup_size(WG)
fn turns(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  lid = l;
  let id = invocation(g, nw, WG);
  if (id >= clk.n) { return; }
  block_turns(vec3<i32>(i32(id % clk.nbx), i32((id / clk.nbx) % clk.nby), i32(id / (clk.nbx * clk.nby))));
}

/// The index step to the neighbour across face f.
fn face_step(f: u32) -> u32 {
  let d = array<u32, 6>(1u, 0xFFFFFFFFu, clk.w, 0u - clk.w, clk.w * clk.h, 0u - clk.w * clk.h);
  return d[min(f, 5u)];
}
/// Site s's neighbours' pulses after the previous clock's turns, by face (none off the lattice).
fn neighbour_pulses(s: u32) -> array<u32, 6> {
  let x = s % clk.w; let y = (s / clk.w) % clk.h; let z = s / (clk.w * clk.h);
  let on = array<bool, 6>(x + 1u < clk.w, x > 0u, y + 1u < clk.h, y > 0u, z + 1u < clk.d, z > 0u);
  let nb = array<u32, 6>(s + 1u, s - 1u, s + clk.w, s - clk.w, s + clk.w * clk.h, s - clk.w * clk.h);
  var pes: array<u32, 6>;
  for (var f = 0u; f < 6u; f++) { pes[f] = select(NONE, pw_end(pul_read(select(s, nb[f], on[f]))), on[f]); }
  return pes;
}
/// A site's pulse phase (lattice.rs `step_pulses`; hw/rtl/strands_lattice.v `strands_site_net`),
/// from its state `old` and its neighbours' pulses `pes`: pulses pointing at the site arrive, the
/// one through its lowest face winning; a pulse reaching a computation's output wants it; a
/// wanted reader with no pulse in its site sends one.
fn pulse_step(old: Site, pes: array<u32, 6>) -> Site {
  var t = old; var arr = NONE; var got = false;
  for (var f = 0u; f < 6u; f++) {
    let pe = pes[f];
    if (is_strand(pe) && face(pe) == (f ^ 1u)) {
      let m = vgm(t, se(f, lane(pe)));
      if (is_strand(m)) { if (!got) { got = true; arr = m; } }
      else if (m != NONE) {
        let k = port_k(m); let qq = port_q(m);
        // A pulse reaching a value's principal port calls it (lattice.rs `calls`).
        if (CALLS && qq == 0u && vgt(t, k) != 0u && is_producer(vgt(t, k))) { t = vsw(t, k, true); }
        if (qq != 0u && vgt(t, k) != 0u && is_consumer(vgt(t, k)) && !vgw(t, k)) { t = vsw(t, k, true); tally[T_PULSES]++; }
      }
    }
  }
  // A wanted reader calls a value it shares its site with (lattice.rs `call_here`).
  if (CALLS) {
    for (var k = 0u; k < 3u; k++) {
      let m = vgm(t, ae(k, 0u));
      if (vwanted_reader(t, k) && m != NONE && !is_strand(m) && vgt(t, port_k(m)) != 0u && is_producer(vgt(t, port_k(m)))) { t = vsw(t, port_k(m), true); }
    }
  }
  var sent = false;
  for (var k = 0u; k < 3u; k++) {
    if (!got && !sent && vocc(t) != 0u && vwanted_reader(t, k) && is_strand(vgm(t, ae(k, 0u)))) { arr = vgm(t, ae(k, 0u)); sent = true; }
  }
  return vspul(t, arr);
}
/// The pulse phase of position q, site s; bit q if it changed.
fn pulse_at(q: u32, s: u32) -> u32 {
  let t = get_site(q); let u = pulse_step(t, neighbour_pulses(s));
  var changed = false;
  for (var i = 0u; i < 10u; i++) { if (u[i] != t[i]) { changed = true; } }
  if (changed) { put_site(q, u); }
  return u32(changed) << q;
}
fn site_pulse(s: u32) {
  let old = load(s);
  let t = pulse_step(old, neighbour_pulses(s));
  var changed = false;
  for (var i = 0u; i < 10u; i++) { if (t[i] != old[i]) { changed = true; } }
  if (changed) { store(s, t); }
}
/// Every site's pulse phase.
@compute @workgroup_size(64)
fn pulses(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let s = invocation(g, nw, 64u);
  if (s < clk.n) { site_pulse(s); add_tally(); }
}

/// A recorded turn or block (lattice.rs test vectors), REC words: kind (0 turn, 1 block), the
/// clock's tick, the corner (3, signed), the lattice's size (3), the valid positions, the turn's
/// position and the positions taken before it, the 8 sites before (80 words, replaced by after),
/// then the turn's touched positions and whether it was dropped.
const REC: u32 = 93u;
@compute @workgroup_size(WG)
fn vectors(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  lid = l;
  let r = invocation(g, nw, WG);
  if (r >= clk.n) { return; }
  let b = r * REC;
  tick = recs[b + 1u];
  edges(vec3<i32>(i32(recs[b + 2u]), i32(recs[b + 3u]), i32(recs[b + 4u])), recs[b + 5u], recs[b + 6u], recs[b + 7u]);
  for (var q = 0u; q < 8u; q++) { for (var i = 0u; i < 10u; i++) { sb[at(q, i)] = recs[b + 11u + q * 10u + i]; } }
  if (recs[b] == 0u) { taken = recs[b + 10u]; p = recs[b + 9u]; note_pair(p); set_pmates(); turn(); }
  else { run_block(); }
  for (var q = 0u; q < 8u; q++) { for (var i = 0u; i < 10u; i++) { recs[b + 11u + q * 10u + i] = sb[at(q, i)]; } }
  recs[b + 91u] = touched;
  recs[b + 92u] = u32(stale);
}
