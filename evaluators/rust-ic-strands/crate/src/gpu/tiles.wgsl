// Locality: most of a lattice is empty, and a clock moves anything at most one site, so only the
// tiles near something need running, and of their blocks only the ones holding something (the
// busy list, made afresh every clock so that each invocation of the turn kernel has work). A tile is tbx×tby×tbz blocks, 64 of them (4×4×4, or 8×8×1 on
// a flat lattice), one workgroup's worth, one block per invocation; it owns those blocks whatever
// the clock's offset, so it reaches at most one site into its lower neighbours. Every few clocks
// (no more than a tile is wide) the active list is rebuilt: the tiles holding something, and
// every tile next to one, which covers wherever their contents can get to before the next rebuild.

/// Per tile: active from the next rebuild on.
@group(0) @binding(4) var<storage, read_write> marks: array<atomic<u32>>;
/// The active tiles.
@group(0) @binding(5) var<storage, read_write> list: array<u32>;
/// The length of the active list being built, of the busy list of each parity, of the gathered
/// sites; from TALLY on, what the turns did; from FIRES on, how many rewrites fired, then their sites.
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
fn add_tally() { for (var i = 0u; i < 11u; i++) { if (tally[i] != 0u) { atomicAdd(&counts[TALLY + i], tally[i]); } } }
fn log_fire(s: u32) { let j = atomicAdd(&counts[FIRES], 1u); if (j < FIRES_MAX) { atomicStore(&counts[FIRES + 1u + j], s); } }
/// The busy blocks, by index.
@group(0) @binding(7) var<storage, read_write> busy: array<u32>;
/// The indirect dispatches over the active tiles and over the busy blocks (x, y, z each).
@group(1) @binding(0) var<storage, read_write> args: array<atomic<u32>>;

fn ntiles() -> u32 { return clk.ntx * clk.nty * clk.ntz; }
fn tile_xyz(t: u32) -> vec3<u32> { return vec3<u32>(t % clk.ntx, (t / clk.ntx) % clk.nty, t / (clk.ntx * clk.nty)); }
/// A tile's own sites: the 2×2×2 of each of its blocks at offset 0.
fn tile_sites() -> vec3<u32> { return 2u * vec3<u32>(clk.tbx, clk.tby, clk.tbz); }
/// The i-th of tile t's own sites, or none off the lattice.
fn tile_site(t: vec3<u32>, i: u32) -> u32 {
  let e = tile_sites();
  let x = t.x * e.x + i % e.x; let y = t.y * e.y + (i / e.x) % e.y; let z = t.z * e.z + i / (e.x * e.y);
  if (x >= clk.w || y >= clk.h || z >= clk.d) { return 0xFFFFFFFFu; }
  return x + clk.w * (y + clk.h * z);
}

var<workgroup> holds: atomic<u32>;
/// An active tile holding anything marks itself and its 26 neighbours.
@compute @workgroup_size(64)
fn tile_mark(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  if (l == 0u) { atomicStore(&holds, 0u); }
  workgroupBarrier();
  let t = tile_xyz(list[wg.x]);
  let e = tile_sites();
  for (var i = l; i < e.x * e.y * e.z; i += 64u) {
    let s = tile_site(t, i);
    if (s != 0xFFFFFFFFu && live[s] != 0u) { atomicStore(&holds, 1u); }
  }
  workgroupBarrier();
  if (atomicLoad(&holds) != 0u && l < 27u) {
    let n = vec3<i32>(t) + vec3<i32>(i32(l % 3u) - 1, i32((l / 3u) % 3u) - 1, i32(l / 9u) - 1);
    if (all(n >= vec3<i32>(0)) && all(vec3<u32>(n) < vec3<u32>(clk.ntx, clk.nty, clk.ntz))) {
      atomicStore(&marks[u32(n.x) + clk.ntx * (u32(n.y) + clk.nty * u32(n.z))], 1u);
    }
  }
}

/// Before marking: no tile marked, an empty list.
@compute @workgroup_size(64)
fn tile_clear(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let t = invocation(g, nw);
  if (t == 0u) { atomicStore(&counts[0], 0u); }
  if (t < ntiles()) { atomicStore(&marks[t], 0u); }
}

/// The marked tiles become the active list (in no particular order).
@compute @workgroup_size(64)
fn tile_compact(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let t = invocation(g, nw);
  if (t < ntiles() && atomicLoad(&marks[t]) != 0u) { list[atomicAdd(&counts[0], 1u)] = t; }
}

/// The dispatch over the new list.
@compute @workgroup_size(1)
fn tile_args() {
  atomicStore(&args[0], atomicLoad(&counts[0])); atomicStore(&args[1], 1u); atomicStore(&args[2], 1u);
}

/// A block of an active tile is busy when one of its sites is live (holds something, or still
/// has a pulse in the buffer this clock writes, from before it emptied, to be cleared).
@compute @workgroup_size(64)
fn tile_busy(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let t = tile_xyz(list[wg.x]);
  let b = t * vec3<u32>(clk.tbx, clk.tby, clk.tbz) + vec3<u32>(l % clk.tbx, (l / clk.tbx) % clk.tby, l / (clk.tbx * clk.tby));
  if (b.x >= clk.nbx || b.y >= clk.nby || b.z >= clk.nbz) { return; }
  let c = block_corner(vec3<i32>(b));
  var held = false;
  for (var q = 0u; q < 8u; q++) { let s = site_at(c, q); if (s != 0xFFFFFFFFu && live[s] != 0u) { held = true; } }
  if (held) { busy[atomicAdd(&counts[1u + clk.par], 1u)] = b.x + clk.nbx * (b.y + clk.nby * b.z); }
}

/// The dispatch over this clock's busy list, and an empty list for the next clock.
@compute @workgroup_size(1)
fn busy_args() {
  atomicStore(&args[3], (atomicLoad(&counts[1u + clk.par]) + 63u) / 64u); atomicStore(&args[4], 1u); atomicStore(&args[5], 1u);
  atomicStore(&counts[1u + (clk.par ^ 1u)], 0u);
}

fn busy_block(i: u32) -> vec3<i32> {
  let id = busy[i];
  return vec3<i32>(i32(id % clk.nbx), i32((id / clk.nbx) % clk.nby), i32(id / (clk.nbx * clk.nby)));
}
/// The busy blocks' turns, 64 to a workgroup.
@compute @workgroup_size(64)
fn busy_turns(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  lid = l;
  let i = wg.x * 64u + l;
  if (i >= atomicLoad(&counts[1u + clk.list])) { return; }
  block_turns(busy_block(i));
}
/// The pulse phase of the clock whose busy list is `list` (every site holding anything is in one of
/// its blocks): closes a batch of clocks.
@compute @workgroup_size(64)
fn busy_pulses(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let i = wg.x * 64u + l;
  if (i >= atomicLoad(&counts[1u + clk.list])) { return; }
  let c = block_corner(busy_block(i));
  for (var q = 0u; q < 8u; q++) {
    let s = site_at(c, q);
    if (s != 0xFFFFFFFFu) { site_pulse(s); }
  }
  add_tally();
}

/// The sites holding anything (all in the busy blocks of the clock whose list is `list`), each as
/// its index and its 10 words, appended to `recs` (their count in counts[3]).
@compute @workgroup_size(64)
fn busy_gather(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let i = wg.x * 64u + l;
  if (i >= atomicLoad(&counts[1u + clk.list])) { return; }
  let c = block_corner(busy_block(i));
  for (var q = 0u; q < 8u; q++) {
    let s = site_at(c, q);
    if (s == 0xFFFFFFFFu) { continue; }
    var held = false;
    for (var k = 0u; k < 10u; k++) { if (sites[s * 10u + k] != EMPTY[k]) { held = true; } }
    if (held) {
      let j = atomicAdd(&counts[3], 1u) * 11u;
      recs[j] = s;
      for (var k = 0u; k < 10u; k++) { recs[j + 1u + k] = sites[s * 10u + k]; }
    }
  }
}
