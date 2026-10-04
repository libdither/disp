// Locality: most of a lattice is empty, and a clock moves anything at most one site, so only the
// tiles near something need running. A tile is tbx×tby×tbz blocks, 64 of them (4×4×4, or 8×8×1 on
// a flat lattice), one workgroup's worth, one block per invocation; it owns those blocks whatever
// the clock's offset, so it reaches at most one site into its lower neighbours. Every few clocks
// (no more than a tile is wide) the active list is rebuilt: the tiles holding something, and
// every tile next to one, which covers wherever their contents can get to before the next rebuild.

/// Per tile: active from the next rebuild on.
@group(0) @binding(4) var<storage, read_write> marks: array<atomic<u32>>;
/// The active tiles.
@group(0) @binding(5) var<storage, read_write> list: array<u32>;
/// The indirect dispatch over the active tiles (x, y, z), then the length of the list being built.
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

/// The active tiles' blocks' turns: one tile per workgroup.
@compute @workgroup_size(64)
fn tile_turns(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  lid = l;
  let t = tile_xyz(list[wg.x]);
  let b = t * vec3<u32>(clk.tbx, clk.tby, clk.tbz) + vec3<u32>(l % clk.tbx, (l / clk.tbx) % clk.tby, l / (clk.tbx * clk.tby));
  if (b.x >= clk.nbx || b.y >= clk.nby || b.z >= clk.nbz) { return; }
  block_turns(vec3<i32>(b));
}

/// The active tiles' sites' pulse phase.
@compute @workgroup_size(64)
fn tile_pulses(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let t = tile_xyz(list[wg.x]);
  let e = tile_sites();
  for (var i = l; i < e.x * e.y * e.z; i += 64u) {
    let s = tile_site(t, i);
    if (s != 0xFFFFFFFFu) { site_pulse(s); }
  }
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
    if (s != 0xFFFFFFFFu) {
      for (var k = 0u; k < 10u; k++) { if (sites[s * 10u + k] != EMPTY[k]) { atomicStore(&holds, 1u); break; } }
    }
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
  if (t == 0u) { atomicStore(&args[3], 0u); }
  if (t < ntiles()) { atomicStore(&marks[t], 0u); }
}

/// The marked tiles become the active list (in no particular order).
@compute @workgroup_size(64)
fn tile_compact(@builtin(global_invocation_id) g: vec3<u32>, @builtin(num_workgroups) nw: vec3<u32>) {
  let t = invocation(g, nw);
  if (t < ntiles() && atomicLoad(&marks[t]) != 0u) { list[atomicAdd(&args[3], 1u)] = t; }
}

/// The dispatch over the new list.
@compute @workgroup_size(1)
fn tile_args() {
  atomicStore(&args[0], atomicLoad(&args[3])); atomicStore(&args[1], 1u); atomicStore(&args[2], 1u);
}

/// The active tiles' sites, tile after tile in list order, into `recs` (to look for the answer).
@compute @workgroup_size(64)
fn tile_gather(@builtin(workgroup_id) wg: vec3<u32>, @builtin(local_invocation_index) l: u32) {
  let t = tile_xyz(list[wg.x]);
  let e = tile_sites();
  let n = e.x * e.y * e.z;
  for (var i = l; i < n; i += 64u) {
    let s = tile_site(t, i);
    for (var k = 0u; k < 10u; k++) {
      var v = EMPTY[k];
      if (s != 0xFFFFFFFFu) { v = sites[s * 10u + k]; }
      recs[(wg.x * n + i) * 10u + k] = v;
    }
  }
}
