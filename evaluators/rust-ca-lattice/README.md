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
  in-board placement backtracking plus the wire router), REEL (walk one cell along the
  principal, gated on the ψ hot bit, a single neighbor-local read; aux wires re-anchor
  truncation-first, eating the walker's own drag, with the bounded router as the
  extension fallback; a walker at a shared cell waits, never tucks), SHOVE (a χ-descent
  step for parked agents; hot walkers, docked pairs, and Out are immune), and the two
  polymer moves, both projecting to identity: RETRACT (a width-1 U-turn annihilates,
  wire −2, the discrete curve-shortening step) and SLIDE (a strand relocates out of a
  shared cell through the shortest empty-cell route, the excluded-volume move; the
  kink-flip is its 1-cell case; a second relaxed pass may thread count-1 trail cells,
  for walker-demanded clearings only).
- `crate/src/lattice.rs` also carries the two fields, both strictly neighbor-local.
  ψ (demand) is a per-strand `hot` bit pumped by adjacent live consumer principals and
  propagated one cell per tick along wire continuations, monotone within a tick sweep
  and cold-on-write; it replaced the only unbounded read in the dynamics (`try_trace`
  is now observer-only). χ (pressure) is a sparse per-cell u8 under a Jacobi relax
  `(2·self + Σnbrs)/8` with a unit leak, pumped at 250 by frustration sources (blocked
  clears, hot-but-unplannable walkers, waiting seeds, squatted reservations).
- `crate/src/scheduler.rs` — the sequential deterministic schedule over the transitions:
  heat sweep, fire (or dock/grow), clear (walker-demanded slides, three fallbacks deep),
  reel, shove, pressure decongestion and evaporation, retract to fixpoint, then one χ
  step. Stall detection is two-tier: a zero-applied streak, plus a shadow-progress
  drought (`PROGRESS_DROUGHT` ticks without a fire) because the field moves keep ticks
  busy while a hopeless knot churns. Rung 3 adds parallel and async-fuzz schedules over
  the same transitions.
- `crate/tests/stage1.rs` — abstract net vs oracle: 4000 random terms, zero mismatches,
  full 26-rule coverage enforced.
- `crate/tests/stage2.rs` — lattice vs oracle on BOTH topologies: correctness gated
  absolutely (zero wrong NFs, per-transition projection asserts, bit determinism);
  liveness measured, with per-topology floors pinning the baselines.
- `crate/src/bin/debug-stuck.rs`, `scan-pins.rs` — stall analyzers (topology-aware);
  `probe-grow.rs` — event census plus radius-2 occupancy maps around blocked walkers;
  `probe-cap.rs` — the budget-vs-churn classifier (re-runs tick-capped terms at five
  times the budget and reports whether shadow ints moved).
- `crate/src/bin/dump-run.rs` — the instrumentation face: runs a term and emits one JSON
  document (full grid snapshot + tick events per frame; v2 schema: [x,y,z] positions,
  single-char faces, a trailing `*` marks a ψ-hot strand, a sparse per-cell `chi` list
  carries the pressure field, no tucks). The replay client is
  `research/interaction-combinator/lattice_player.html` (upper planes render offset and
  dimmer; embedded traces in `lattice_traces.js` cover both topologies; drop any dump-run
  output onto it to view others). The engine is the model; the visualization never
  simulates.

## Status (measured)

`cargo test --release`: all green. Stage 1: 3998+/0. Stage 2 corpus (400 random terms,
depths 3 to 5), now with the ψ/χ fields in the sequential schedule: full3d 326 reach
normal form (205 before the fields), bilayer 223 (was 179); zero wrong results, zero
invariant violations, zero tick-cap hits, on both topologies and under every fire mode.
Per mode: stamp+search reproduces search exactly (326/223; under pressure the stamp is
no longer wedged by trails, it just ties the search), stamp-only reaches 216/198, and
grow-only beats search on bilayer (238: on the congested topology the patient
dock-and-extrude wins once the fields clear its squatters). Must-complete pins: stem
application and fork dispatch on both topologies; on full3d additionally K erasure, the
sharing S-rule, kargs, chain1 through chain3, the deep-copy share, disp, and selF end to
end, which is every named pin. Every stall is liveness, never correctness. What remains
stuck: on bilayer the no-basement seam class (the single overflow plane is that
topology's geometric ceiling); on full3d 74 corpus terms of chain4-class deep spines and
multi-walker seam knots. That residue is LOCAL_CA_DESIGN.md §13's liveness question, now
measurable per topology against a sound substrate; policies compete on top without being
able to break correctness.

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
8. GROW FIRE (`FireMode::{Grow, GrowThenSearch}`, the dock-and-extrude prototype): where
   the stamp needs its whole layout open NOW, a dock RESERVES the template's cells
   (squatting wire cells allowed if slideable right then), unfolds ONE cell per tick as
   cells open, slides squatters out itself, and finalizes by replacing its own two cells
   last. Reservations shield the half-wired emissions from retract/slide/clear and keep
   claimed cells from refilling. The shadow fires at COMPLETION — the linearization point
   — which is what makes seeds ABORTABLE: after 8 stalled ticks a seed deletes its
   emissions (all shielded, so nothing references them), restores the pair verbatim, and
   yields to the other planners. The first cut linearized at DOCK and taught the lesson
   the hard way: an unabortable seed is a one-way door, and it deadlocks against the very
   walkers its own reservation parks (measured: grow+search 200 < search 205). With
   completion-linearization and the abort valve, grow+search returns to 205 — parity,
   deadlock-free, and every step of the unfold has a 1-cell footprint.
9. What is left standing in every mode's stuck set is the AGENT blocker: a parked walker
   or value sitting on the cells a fire needs. Docks refuse agent squatters (agents move
   only by reeling, on their own demand), the search cannot route through them, and no
   fire mechanism — atomic, stamped, or grown — can move them. Fire is now as local as
   geometry allows; the remaining mover is the pressure field (χ) that pushes agents and
   unslideable knots, i.e. the field rung, on top of a fire that can finally wait
   gracefully for it.
10. ψ kills the trace at zero liveness cost: the reel gate "a fire awaits at the far end"
   was the dynamics' one unbounded read (walking the whole wire). The per-strand hot bit
   (pumped at consumer principals, spread one cell per tick, cold on write, sources dying
   with their wires so staleness is structural) reproduces the gate exactly; the corpus
   did not move by a single term when the trace died. Demand was always local information
   arriving late, and the walker can afford to wait for it.
11. χ needs precision pumping: pumping every frustrated pair and blocked walker measured
   as a regression (churn drowned the signal). Pump only real frustration (clears whose
   every fallback failed, hot walkers that cannot plan, waiting seeds, squatted
   reservations) and shove only parked agents; hot walkers are immune, or the field
   undoes the very deliveries it exists to enable. Decongestion under χ must stay
   strictly decongesting; relaxed slides that thread count-1 trails are sound only when
   walker-demanded (ambient relaxation shuffles strands forever, 15k to 272k slides
   measured). With that discipline, pressure moves what no fire mechanism could: kargs
   completed for the first time.
12. Walks must eat slack, or they pave their own prison: extension-only aux re-anchoring
   nets +1 strand per step per aux wire, so every long walk deposits a drag staircase,
   and the staircases are the count-1 mats that block clears, evaporation, and fire
   boards (invisible to crowding-based decongestion, since every cell holds just one
   strand). The truncation re-anchor walks the aux chain up to three cells and, at the
   first cell whose continuation already sits adjacent to the mover's new seat, deletes
   the walked strands and repoints the surviving half: net −1 or better, zero new wire
   needed exactly where the router has no room. This unlocked the chain class and selF,
   and with it every full3d named pin completes. Its complement is χ evaporation: under
   strong pressure (χ ≥ 3) count-1 cells slide their cold strand out through empty
   trails strictly downhill in χ, so existing mats thin from the boundary inward.
13. A field schedule needs a shadow-progress stall detector: shoves, decongestion, and
   detour reels keep ticks busy while a hopeless knot churns, so "a full tick applied
   nothing" no longer catches every stall; terms burned the whole tick budget, and the
   probe measured the capped class 40/40 frozen (ints and transport bit-identical at 20k
   and 100k ticks). Progress is shadow ints alone: docks, grow placements, and aborts
   all cycle (dock, place, abort, redock), so none of them may reset the window.
