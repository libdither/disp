# rust-ca-lattice

The local CA substrate for disp's tree-calculus interaction nets: rungs 1 and 2 of
`research/interaction-combinator/LOCAL_CA_DESIGN.md`, built as a Rust crate under the
project's differential-oracle discipline, now on the LIFTED lattice (the "topo lift":
cells at (x, y, z) with six faces). Not yet a `Session` backend (that is rung 4); today it
is a library plus tests.

## What is here

- `crate/src/rules.rs` (rung 1) — the interaction rules frozen as a validated template
  ROM over the LOWERED alphabet: every agent has at most 3 ports. The legacy 4-port `T1`
  carries its arms as a `Pair`; the 5-port `T2` becomes `Sel` over nested pairs; `Unp` is
  the destructor (`Unp·Pair` is a pure double fusion). 13 tags, 26 interactions, each a
  perfect matching over dying-aux + fresh ports, checked mechanically at load. Export with
  `cargo run --bin export-rules` (a generated copy lives at
  `research/interaction-combinator/rules.json` for the JS side).
- `crate/src/oracle.rs` — the independent recursive normalizer (shares no code with the
  engines; the differential's power is that independence).
- `crate/src/net.rs` — the table-driven abstract engine: one generic `fire` interprets the
  ROM; also the SHADOW net the lattice checks its projection against.
- `crate/src/lattice.rs` (rung 2 state) — per-cell state on the lifted lattice, no ids in
  the dynamics: an agent cell (≤3 ports, one attachment per face) or a wire cell (≤3
  strands). Connectivity is positional. The 2D model's wire layers, via cells, and tucked
  strands are DELETED — z is the capacity they were simulating; a crossing is two wires at
  different z. Two topologies, dynamics identical: `Bilayer` (z ∈ {0,1}, the 2.5D chip,
  the honest worst case) and `Full3D`. The loader (host code, global routing allowed) and
  the executable projection invariant (`check_projection`) live here.
- `crate/src/transitions.rs` (rung 2 dynamics) — footprint-atomic transitions: `plan_*`
  reads a bounded neighborhood and returns a Plan carrying its exact footprint; `apply_*`
  machine-checks every write against it. FIRE (dying cells become splice hubs; bounded
  in-board placement backtracking + wire router), REEL (walk one cell along the principal,
  gated on "a fire awaits"; aux wires re-anchored by the same bounded router — the
  straight-behind bend and the corner detour are its 1-cell and 3-cell routes; a walker
  at a shared cell WAITS, never tucks), and the two POLYMER MOVES, both projecting to
  identity: RETRACT (a width-1 U-turn annihilates, wire −2, the discrete curve-shortening
  step) and SLIDE (a strand relocates out of a shared cell through the shortest empty-cell
  route — the excluded-volume move; the kink-flip is its 1-cell case).
- `crate/src/scheduler.rs` — the sequential deterministic schedule over the four
  transitions: fire · clear (walker-demanded slides) · reel · retract-to-fixpoint. Rung 3
  adds parallel and async-fuzz schedules over the SAME transitions.
- `crate/tests/stage1.rs` — abstract net vs oracle: 4000 random terms, zero mismatches,
  full 26-rule coverage enforced.
- `crate/tests/stage2.rs` — lattice vs oracle on BOTH topologies: correctness gated
  absolutely (zero wrong NFs, per-transition projection asserts, bit determinism);
  liveness measured, with per-topology floors pinning the baselines.
- `crate/src/bin/debug-stuck.rs`, `scan-pins.rs` — stall analyzers (topology-aware).
- `crate/src/bin/dump-run.rs` — the instrumentation face: runs a term and emits one JSON
  document (full grid snapshot + tick events per frame; v2 schema: [x,y,z] positions,
  single-char faces, no tucks). The replay client is
  `research/interaction-combinator/lattice_player.html` (upper planes render offset and
  dimmer; embedded traces in `lattice_traces.js` cover both topologies; drop any dump-run
  output onto it to view others). The engine is the model; the visualization never
  simulates.

## Status (measured)

`cargo test --release`: all green. Stage 1: 3998+/0. Stage 2 corpus (400 random terms,
depths 3–5): full3d 205 reach normal form, bilayer 179; ZERO wrong results, zero invariant
violations, zero tick-cap hits, on both topologies and under every fire mode (the search
planner, the precomputed stamp, and the hybrid). Must-complete pins: stem application
on both topologies; fork dispatch, K erasure, chain1, and the sharing S-rule on full3d.
Every stall is liveness, never correctness: the bare sequential schedule still ships no
fields, and the stalls concentrate where fire seams knot — splices and reel trails exhaust
local capacity around the hub, and on bilayer the single overflow plane makes this bite
almost immediately (the same terms complete on full3d). That residue is
LOCAL_CA_DESIGN.md §13's liveness question, now measurable per topology against a sound
substrate; policies compete on top without being able to break correctness.

Liveness findings folded into the design (each found by measurement here):

1. Walk only when a fire awaits (far end of the principal is a consumer's principal):
   values that walked to their parent's aux port squatted on crossings and deadlocked the
   crossed wire's owners.
2. The topo lift deletes coexistence: with z a real coordinate, exclusive faces suffice
   and tucks/vias/layers disappear. The old 2D "exclusive faces deadlock walkers" finding
   was a symptom of the missing dimension.
3. A walker blocked at a shared cell needs DISPLACEMENT, not coexistence: the SLIDE move
   (walker-demanded, excluded-volume) replaces the old tuck-through. Fixed-shape moves
   (kink-flip, one-direction bulge) are not enough — congested neighborhoods need the
   general shortest-reroute, plus one level of knot-loosening at the blocker's endpoints.
4. Slides must be strictly decongesting (empty-cell trails only, loosening only out of
   shared cells): a permissive variant that threads through occupied cells lets the
   scheduler shuffle strands forever, and stall detection dies with it.
5. Reel's aux re-anchoring wants the router, not bespoke bend shapes: with exclusive faces
   the second aux always needs a detour, and the fixed L-shape detour dead-ends where the
   shared router finds five-cell routes.
6. STAMP FIRE (`plan_fire_stamp` + `FireMode`): the fire layout solved once per (rule,
   dock axis, anchor faces, topology) on an empty synthetic workshop, applied to the live
   lattice as a fixed pattern — fire enabledness becomes purely local (freeness reads, no
   search). Sound (zero mismatches under CheckLevel::Every) but measured NEUTRAL:
   stamp+search is byte-identical to search alone, and stamp-only wedges after 1-2 fires.
   The cause is diagnostic gold: fixed layouts are unfittable amid ambient REEL TRAILS,
   which occupy the prime cells around every post-reel pair — exactly where the search
   planner survives by routing around and stacking. The congestion is inherent to firing
   amid trails, not to the planner's choices on clean boards; the fix must move the
   trails or grow incrementally, not choose layouts better.
7. Trail placement is a real lever: routing reel trails DOWN into full3d's uncontested
   basement (z<0; nothing else routes there) lifted the corpus 201→205 with pins intact
   and no cost elsewhere. A blanket up-first order was measured to HURT (trails then
   contend with the overflow plane that detours and fires need). Bilayer has no basement
   and keeps the standard order — its relief has to come from pressure, not geometry.
