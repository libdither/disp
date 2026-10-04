// Who takes a turn when, a turn's stages in order, and the kernels (hw/rtl/strands_block.v and
// strands_lattice.v; lattice.rs `margolus_clock`, `turn`, `step_pulses`).

/// One clock's parameters: the lattice's size, the clock mixed with the seed (lattice.rs `tick`),
/// blocks per axis, how many invocations a full-lattice dispatch covers, and the tiles (tiles.wgsl):
/// blocks per tile and tiles per axis.
struct Clock { w: u32, h: u32, d: u32, tick: u32, nbx: u32, nby: u32, nbz: u32, n: u32,
               tbx: u32, tby: u32, tbz: u32, ntx: u32, nty: u32, ntz: u32, pad0: u32, pad1: u32 }
@group(0) @binding(0) var<uniform> clk: Clock;
/// Every site, 10 words each, at x + w * (y + h * z).
@group(0) @binding(1) var<storage, read_write> sites: array<u32>;
/// Every site's pulse once the turns are done: the pulse phase reads its neighbours' here.
@group(0) @binding(2) var<storage, read_write> pul: array<u32>;
/// Recorded turns and blocks to replay (the `vectors` kernel), REC words each.
@group(0) @binding(3) var<storage, read_write> recs: array<u32>;

/// The clock mixed with the seed.
var<private> tick: u32;
/// The block's corner (its position 0), which may lie off the lattice.
var<private> corner: vec3<i32>;

/// The block's place in the lattice (the RTL's `edges`), in the RTL's wrapping arithmetic.
fn edges(c: vec3<i32>, w: u32, h: u32, d: u32) {
  corner = c; onl = 0u;
  for (var q = 0u; q < 8u; q++) {
    let x = u32(c.x + i32(q & 1u)); let y = u32(c.y + i32((q >> 1u) & 1u)); let z = u32(c.z + i32((q >> 2u) & 1u));
    latq[q] = u32(x + 1u < w) | (u32(x != 0u) << 1u) | (u32(y + 1u < h) << 2u) | (u32(y != 0u) << 3u)
            | (u32(z + 1u < d) << 4u) | (u32(z != 0u) << 5u);
    if (x < w && y < h && z < d) { onl |= 1u << q; }
  }
}
/// A position's key for its dice: its coordinates, 10 bits each.
fn key(q: u32) -> u32 {
  let x = u32(corner.x + i32(q & 1u)); let y = u32(corner.y + i32((q >> 1u) & 1u)); let z = u32(corner.z + i32((q >> 2u) & 1u));
  return (x & 1023u) | ((y & 1023u) << 10u) | ((z & 1023u) << 20u);
}

/// A consumer at position q whose partner is here or one strand away (lattice.rs `active_pair`).
struct Pair { found: bool, stale: bool, via: bool, inf: bool, kc: u32, kp: u32, face: u32, inf_pos: u32, inf_k: u32 }
fn active_pair(q: u32) -> Pair {
  var r = Pair(false, false, false, false, 0u, 0u, 0u, 0u, 0u);
  let s = blk[q];
  for (var k = 0u; k < 3u; k++) {
    let t = gt(s, k); let m = gm(s, ae(k, 0u));
    var skip = t == 0u || !is_consumer(t) || (LAZY && !gw(s, k)) || m == NONE;
    var sp = q; var mm = m; var f = 0u;
    if (!skip && is_strand(m)) {
      f = face(m); let nn = inb(q, f);
      if ((nn & 8u) == 0u) { skip = true; }
      else if ((taken & (1u << (nn & 7u))) != 0u) { r.stale = true; return r; }
      else { sp = nn & 7u; mm = gm(blk[sp], se(f ^ 1u, lane(m))); }
    }
    if (!skip && !is_strand(mm) && mm != NONE) {
      let k2 = port_k(mm); let qq = port_q(mm); let t2 = gt(blk[sp], k2);
      if (qq == 0u && t2 != 0u && is_producer(t2)) {
        r.found = true; r.kc = k; r.kp = k2; r.via = is_strand(m); r.face = f; return r;
      } else if (LAZY && qq != 0u && t2 != 0u && is_consumer(t2) && t != T_EPS) {
        r.inf = true; r.inf_pos = sp; r.inf_k = k2;
      }
    }
  }
  return r;
}

fn wanted_reader(s: Site, k: u32) -> bool { let t = gt(s, k); return t != 0u && is_consumer(t) && t != T_EPS && gw(s, k); }
/// The wanted reader an active turn moves (the RTL's `active_reader`).
fn reader_k() -> u32 {
  let s = blk[p]; var ksl = 0u; var n = 0u;
  for (var k = 0u; k < 3u; k++) { if (wanted_reader(s, k)) { ksl |= k << (2u * n); n++; } }
  return (ksl >> (2u * pick5(d_agent(), n))) & 3u;
}

/// Position p's turn (lattice.rs `turn`): collect, else react, else take an infection and step,
/// exchange, fold or flip; then drop the pulses whose strand was rewired.
fn turn() {
  dice = hash(key(p), tick); touched = 0u; stale = false;
  var pm0: array<u32, 8>;
  for (var q = 0u; q < 8u; q++) { pm0[q] = gm(blk[q], gpul(blk[q])); }
  var done = false;
  if (GC) { done = collect_stage(); }
  if (!done) {
    let active_mode = d_active() < CH_ACTIVE && (wanted_reader(blk[p], 0u) || wanted_reader(blk[p], 1u) || wanted_reader(blk[p], 2u));
    let pr = active_pair(p);
    if (pr.stale) { stale = true; done = true; }
    else if (pr.found) { done = fire(pr.kc, pr.kp, pr.via, pr.face) != 0u; }
    if (!done) {
      let act_k = reader_k();
      // A reader that touched a pending computation wants it now (lattice.rs `take_infect`).
      if (pr.inf && !gw(blk[pr.inf_pos], pr.inf_k)) { blk[pr.inf_pos] = sw(blk[pr.inf_pos], pr.inf_k, true); touched |= 1u << pr.inf_pos; }
      move_stage(active_mode, act_k);
    }
  }
  // A pulse whose strand's mate changed during the turn is lost.
  for (var q = 0u; q < 8u; q++) {
    let e = gpul(blk[q]);
    if (e != NONE && gm(blk[q], e) != pm0[q]) { blk[q] = spul(blk[q], NONE); }
  }
  taken |= touched;
}

/// The block's turns (lattice.rs `margolus_clock` with `block_moves`): sites with a reaction
/// ready, then with agents, then bare wire, each in block order rotated by the block's random
/// word; a site an earlier turn changed sits out.
fn run_block() {
  taken = 0u;
  var cls: array<u32, 8>;
  for (var q = 0u; q < 8u; q++) {
    let s = blk[q]; var c = 3u;
    if ((onl & (1u << q)) != 0u) {
      if (occ(s) != 0u) { c = select(1u, 0u, active_pair(q).found); }
      else { for (var e = 9u; e < NE; e++) { if (getb(s, e) != NONE) { c = 2u; break; } } }
    }
    cls[q] = c;
  }
  let bkey = (u32(corner.x + 1) & 1023u) | ((u32(corner.y + 1) & 1023u) << 10u) | ((u32(corner.z + 1) & 1023u) << 20u) | (1u << 31u);
  let rot = hash(bkey, tick).x & 7u;
  for (var bi = 0u; bi < 24u; bi++) {
    let q = ((bi & 7u) + rot) & 7u;
    if (cls[q] == (bi >> 3u) && (taken & (1u << q)) == 0u) { p = q; turn(); }
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
fn invocation(g: vec3<u32>, n: vec3<u32>) -> u32 { return g.x + g.y * n.x * 64u; }

/// The turns of block b (corner 2b - the clock's offset, a hash of the clock), if it holds anything.
fn block_turns(b: vec3<i32>) {
  tick = clk.tick;
  let o = hash(0xFFFFFFFFu, tick).x;
  let off = vec3<i32>(i32(o & 1u), i32((o >> 8u) & 1u), select(0, i32((o >> 16u) & 1u), clk.d > 1u));
  let c = 2 * b - off;
  edges(c, clk.w, clk.h, clk.d);
  if (onl == 0u) { return; }
  var any = false;
  for (var q = 0u; q < 8u; q++) {
    blk[q] = EMPTY;
    if ((onl & (1u << q)) != 0u) {
      blk[q] = load(site_at(c, q));
      for (var i = 0u; i < 10u; i++) { if (blk[q][i] != EMPTY[i]) { any = true; } }
    }
  }
  if (!any) { return; }
  run_block();
  for (var q = 0u; q < 8u; q++) {
    if ((onl & (1u << q)) != 0u) {
      let s = site_at(c, q);
      if ((taken & (1u << q)) != 0u) { store(s, blk[q]); }
      pul[s] = gpul(blk[q]);
    }
  }
}
/// Every block's turns for this clock.
@compute @workgroup_size(64)
fn turns(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let id = invocation(g, nw);
  if (id >= clk.n) { return; }
  block_turns(vec3<i32>(i32(id % clk.nbx), i32((id / clk.nbx) % clk.nby), i32(id / (clk.nbx * clk.nby))));
}

/// One site's pulse phase (lattice.rs `step_pulses`; hw/rtl/strands_lattice.v `strands_site_net`):
/// pulses pointing at the site arrive, the one through its lowest face winning; a pulse reaching
/// a computation's output wants it; a wanted reader with no pulse in its site sends one.
fn site_pulse(s: u32) {
  let x = s % clk.w; let y = (s / clk.w) % clk.h; let z = s / (clk.w * clk.h);
  let on = array<bool, 6>(x + 1u < clk.w, x > 0u, y + 1u < clk.h, y > 0u, z + 1u < clk.d, z > 0u);
  let nb = array<u32, 6>(s + 1u, s - 1u, s + clk.w, s - clk.w, s + clk.w * clk.h, s - clk.w * clk.h);
  let old = load(s);
  var t = old; var arr = NONE; var got = false;
  for (var f = 0u; f < 6u; f++) {
    if (!on[f]) { continue; }
    let pe = pul[nb[f]];
    if (is_strand(pe) && face(pe) == (f ^ 1u)) {
      let m = gm(t, se(f, lane(pe)));
      if (is_strand(m)) { if (!got) { got = true; arr = m; } }
      else if (m != NONE) {
        let k = port_k(m); let qq = port_q(m);
        if (qq != 0u && gt(t, k) != 0u && is_consumer(gt(t, k)) && !gw(t, k)) { t = sw(t, k, true); }
      }
    }
  }
  var sent = false;
  for (var k = 0u; k < 3u; k++) {
    if (!got && !sent && occ(t) != 0u && wanted_reader(t, k) && is_strand(gm(t, ae(k, 0u)))) { arr = gm(t, ae(k, 0u)); sent = true; }
  }
  t = spul(t, arr);
  var changed = false;
  for (var i = 0u; i < 10u; i++) { if (t[i] != old[i]) { changed = true; } }
  if (changed) { store(s, t); }
}
/// Every site's pulse phase.
@compute @workgroup_size(64)
fn pulses(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let s = invocation(g, nw);
  if (s < clk.n) { site_pulse(s); }
}

/// A recorded turn or block (lattice.rs test vectors), REC words: kind (0 turn, 1 block), the
/// clock's tick, the corner (3, signed), the lattice's size (3), the valid positions, the turn's
/// position and the positions taken before it, the 8 sites before (80 words, replaced by after),
/// then the turn's touched positions and whether it was dropped.
const REC: u32 = 93u;
@compute @workgroup_size(64)
fn vectors(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let r = invocation(g, nw);
  if (r >= clk.n) { return; }
  let b = r * REC;
  tick = recs[b + 1u];
  edges(vec3<i32>(i32(recs[b + 2u]), i32(recs[b + 3u]), i32(recs[b + 4u])), recs[b + 5u], recs[b + 6u], recs[b + 7u]);
  for (var q = 0u; q < 8u; q++) { for (var i = 0u; i < 10u; i++) { blk[q][i] = recs[b + 11u + q * 10u + i]; } }
  if (recs[b] == 0u) { taken = recs[b + 10u]; p = recs[b + 9u]; turn(); }
  else { run_block(); }
  for (var q = 0u; q < 8u; q++) { for (var i = 0u; i < 10u; i++) { recs[b + 11u + q * 10u + i] = blk[q][i]; } }
  recs[b + 91u] = touched;
  recs[b + 92u] = u32(stale);
}
