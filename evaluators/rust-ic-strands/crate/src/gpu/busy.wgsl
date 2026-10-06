// Locality: most of a lattice is empty, and a clock moves anything at most one site, inside its
// block, so a clock need run only the blocks holding something: its busy list. Each block makes
// the next clock's as it finishes its turns, marking the next clock's blocks that hold its sites;
// the lists take turns, three of them, so that clock t runs list t % 3, fills list (t + 1) % 3
// and empties list (t + 2) % 3 for the clock after. A clock is one dispatch of a fixed number of
// workgroups that share out however many blocks are busy, so nothing waits on the list's length.

/// Per block: one more than the last clock whose busy list holds it.
@group(0) @binding(4) var<storage, read_write> marks: array<atomic<u32>>;
/// The three busy lists, by block index, each as long as there are blocks.
@group(0) @binding(5) var<storage, read_write> busy: array<u32>;
/// The lengths of the three busy lists and of the gathered sites; from TALLY on, what the turns
/// did; from FIRES on, how many rewrites fired, then their sites.
@group(0) @binding(6) var<storage, read_write> counts: array<atomic<u32>>;
/// What the turns did, in the order of the simulator's `Stats`: turns, rewrites, rewrites without
/// room, steps (an exchange is two), exchanges, folds, flips, collections, wanted readers stepping
/// and finding no seat, demand pulses delivered.
const TALLY: u32 = 4u;
const T_TURNS: u32 = 0u; const T_FIRES: u32 = 1u; const T_BLOCKED: u32 = 2u; const T_HOPS: u32 = 3u; const T_SWAPS: u32 = 4u;
const T_FOLDS: u32 = 5u; const T_FLIPS: u32 = 6u; const T_COLLECTED: u32 = 7u; const T_WALKS: u32 = 8u; const T_NO_SEAT: u32 = 9u;
const T_PULSES: u32 = 10u;
const FIRES: u32 = 15u;
const FIRES_MAX: u32 = 4096u;
fn add_tally() { for (var i = 0u; i < 11u; i++) { if (tally[i] != 0u) { atomicAdd(&counts[TALLY + i], tally[i]); tally[i] = 0u; } } }
fn log_fire(s: u32) { let j = atomicAdd(&counts[FIRES], 1u); if (j < FIRES_MAX) { atomicStore(&counts[FIRES + 1u + j], s); } }
/// No busy list: the dense kernels make none.
const NO_LIST: u32 = 3u;

fn nblocks() -> u32 { return clk.nbx * clk.nby * clk.nbz; }
/// The next clock's offset, which places its blocks.
var<private> next_offset: vec3<u32>;
fn next_clock() {
  let o = hash(0xFFFFFFFFu, clk.tick_next).x;
  next_offset = vec3<u32>(o & 1u, (o >> 8u) & 1u, select(0u, (o >> 16u) & 1u, clk.d > 1u));
}
/// Site s holds something the next clock must run: its block joins the next busy list, once.
fn mark_next_site(s: u32) {
  let x = vec3<u32>(s % clk.w, (s / clk.w) % clk.h, s / (clk.w * clk.h));
  let b3 = (x + next_offset) / 2u;
  let b = b3.x + clk.nbx * (b3.y + clk.nby * b3.z);
  if (atomicMax(&marks[b], clk.stamp) < clk.stamp) { busy[clk.next * nblocks() + atomicAdd(&counts[clk.next], 1u)] = b; }
}
/// The positions of the block with corner c whose sites hold something (bit q): their blocks
/// join the next busy list, each once. Along an axis where the corner plus the next offset is
/// even, both positions fall in one next block, named by the lower; the marks go out together.
fn mark_next(c: vec3<i32>, held: u32) {
  let ev = (vec3<u32>(c) + next_offset) & vec3<u32>(1u);
  let merged = select(0u, 1u, ev.x == 0u) | select(0u, 2u, ev.y == 0u) | select(0u, 4u, ev.z == 0u);
  var need = 0u;
  for (var q = 0u; q < 8u; q++) { if ((held & (1u << q)) != 0u) { need |= 1u << (q & ~merged); } }
  var bs: array<u32, 8>; var won = 0u;
  for (var r = 0u; r < 8u; r++) {
    let x = vec3<u32>(vec3<i32>(c.x + i32(r & 1u), c.y + i32((r >> 1u) & 1u), c.z + i32((r >> 2u) & 1u)));
    let b3 = (x + next_offset) / 2u;
    bs[r] = b3.x + clk.nbx * (b3.y + clk.nby * b3.z);
  }
  var old: array<u32, 8>;
  for (var r = 0u; r < 8u; r++) { if ((need & (1u << r)) != 0u) { old[r] = atomicMax(&marks[bs[r]], clk.stamp); } }
  for (var r = 0u; r < 8u; r++) { if ((need & (1u << r)) != 0u && old[r] < clk.stamp) { won |= 1u << r; } }
  if (won == 0u) { return; }
  var j = clk.next * nblocks() + atomicAdd(&counts[clk.next], countOneBits(won));
  for (var r = 0u; r < 8u; r++) { if ((won & (1u << r)) != 0u) { busy[j] = bs[r]; j++; } }
}
fn busy_block(i: u32) -> vec3<i32> {
  let id = busy[clk.list * nblocks() + i];
  return vec3<i32>(i32(id % clk.nbx), i32((id / clk.nbx) % clk.nby), i32(id / (clk.nbx * clk.nby)));
}
/// The workgroups' share of a busy list: invocation i takes entries i, i + all, i + 2 all, ...
fn stride(nw: vec3<u32>) -> u32 { return nw.x * 64u; }

/// After loading a state: the first clock's busy list, from every site holding something.
@compute @workgroup_size(64)
fn seed_busy(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let s = invocation(g, nw, 64u);
  if (s >= nsites()) { return; }
  next_clock();
  var held = false;
  for (var k = 0u; k < 10u; k++) { if (sites[s * 10u + k] != EMPTY[k]) { held = true; } }
  // An upload writes both pulse buffers alike, so either holds the field.
  let fv = pw_field(pul_read(s));
  if (held || fv != 0u) { mark_next_site(s); }
  if (FIELD && F_STEP != NONE && fv > F_STEP) {
    let x = s % clk.w; let y = (s / clk.w) % clk.h; let z = s / (clk.w * clk.h);
    let on = array<bool, 6>(x + 1u < clk.w, x > 0u, y + 1u < clk.h, y > 0u, z + 1u < clk.d, z > 0u);
    for (var f = 0u; f < 6u; f++) { if (on[f]) { mark_next_site(s + face_step(f)); } }
  }
}

/// One clock: the busy blocks' turns, each starting with the previous clock's pulse phase when
/// `fused`; they make the next clock's busy list, and the list after that starts empty.
@compute @workgroup_size(WG)
fn clock_turns(@builtin(workgroup_id) wg: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  lid = l;
  if (wg.x == 0u && l == 0u) { atomicStore(&counts[clk.clear], 0u); }
  let n = atomicLoad(&counts[clk.list]);
  for (var i = wg.x * WG + l; i < n; i += nw.x * WG) { block_turns(busy_block(i)); }
}

/// The pulse phase of the clock whose busy list is `list` (every site holding anything is in one of
/// its blocks): closes a batch of clocks.
@compute @workgroup_size(64)
fn busy_pulses(@builtin(workgroup_id) wg: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let n = atomicLoad(&counts[clk.list]);
  for (var i = wg.x * 64u + l; i < n; i += stride(nw)) {
    let c = block_corner(busy_block(i));
    for (var q = 0u; q < 8u; q++) {
      let s = site_at(c, q);
      if (s != 0xFFFFFFFFu) { site_pulse(s); }
    }
  }
  add_tally();
}

/// The sites holding anything or a field (all in the busy blocks of the clock whose list is
/// `list`), each as its index, its 10 words and its field, appended to `recs` (their count in
/// counts[3]).
@compute @workgroup_size(64)
fn busy_gather(@builtin(workgroup_id) wg: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let n = atomicLoad(&counts[clk.list]);
  for (var i = wg.x * 64u + l; i < n; i += stride(nw)) {
    let c = block_corner(busy_block(i));
    for (var q = 0u; q < 8u; q++) {
      let s = site_at(c, q);
      if (s == 0xFFFFFFFFu) { continue; }
      var held = false;
      for (var k = 0u; k < 10u; k++) { if (sites[s * 10u + k] != EMPTY[k]) { held = true; } }
      let fv = pw_field(pul_read(s));
      if (held || fv != 0u) {
        let j = atomicAdd(&counts[3], 1u) * 12u;
        recs[j] = s;
        for (var k = 0u; k < 10u; k++) { recs[j + 1u + k] = sites[s * 10u + k]; }
        recs[j + 11u] = fv;
      }
    }
  }
}
