# rust-ca-lattice

The six-neighbor cellular substrate for disp's tree-calculus interaction net.

## Cascade substrate (current line)

`cascade.rs` and its siblings implement the event-driven successor specified in
`research/interaction-combinator/CASCADE_CELL_DESIGN.md`. One `u64` per site; the atomic
primitive is an edge transaction over one face-adjacent pair; there are no protocol phases,
no request trees, and no activation sweeps. Matter is four kinds: empty (with an optional
growth reservation), wire (up to three single-lane routes; reservations too), agent (tag,
principal endpoint, two independent aux endpoints, up to two passthrough routes), and seed.
Demand (per-route hot bits) spreads one cell per generation from consumer principals; only
hot wires are walked, so undemanded values never move. Walks eat their own slack
(truncation), lay split trails, and detour one aux through side cells when a foreign lane
occupies the crossed edge. Rewrites dock into a two-cell seed whose builder cursor places a
small per-rule blocklet (compiled once, deterministically, in `blocklet.rs`; worst rule 62
cells against the old workshop cap of 63), resolves the fire while every fresh agent is
still seated, then finalizes the nursery.

Three drivers run the same transition rules:

- `cascade_run.rs`: the serial worklist runner (FIFO generations are the physical tick)
  with four adversarial queue disciplines, the route tracer, projection and reciprocity
  checks, and the corridor loader.
- `cascade_par.rs`: N threads over one shared `AtomicU64` array. Mutual exclusion is the
  word's claim bit, compare-and-swapped in address order over a transition's write set, so
  contention exists only where two wake fronts touch: measured at 0 conflicts for disjoint
  cascades and about 0.5 percent of commits under deliberately crossing traffic at 8
  threads, with bit-identical results. Movement, heat, swaps, and fusion docks run
  parallel; blocklet growth stays serial for now.
- `cascade_gather.rs`: the GPU/shader lowering. A repeating six-phase domino schedule
  (axis times parity) partitions the lattice into disjoint pairs; each phase is one pure
  gather into a double buffer, deterministic and bit-identical run to run. It shares the
  pair-decision function with the threaded driver.

Gates: `cargo test --release --lib` (codec exhaustives, blocklet compiler, movement,
parallel, gather) and `cargo test --release --test cascade_suite` (the 26-rule atlas under
all four disciplines, translation straight and bent, the A·F roll-fallback and declined
docks, `@(L,L)` normalized end to end under all four, and the pinned deep-reduction
frontier: `@(F(L,L),L)` reaches its normal form in six chained fires). `dump-cascade`
regenerates `research/interaction-combinator/lattice_cascade.js` with the old suite's
scenario roster migrated onto this substrate (translation, the A·F workshop trio, all 26
atlas rules, the eraser cascade to empty, the T1·S stem chain) plus the cascade-native
demos; `lattice_player.html` replays them next to the Cell64 bundle, one frame per
generation, so a displayed tick is the maximal simultaneous wavefront, and each trace's
note reports its measured parallel width. `bench-cascade` prints the timing snapshot and
`debug-cascade` dumps a parked run's census.

The relief rung landed: growth-blocked cells evict cold routes (corner-cut, straight
shift, out-of-plane bracket), demand looks and wakes through guests, hairpins collapse by
truncation, and consumers can be swap partners. The remaining frontier, pinned by
`frontier_deep_reductions` at 1 of 5 deep terms complete: parked runs wedge where an
eviction is itself boxed into a crowded corner; the next levers are recursive room-making
and proactive slack retraction. The substrate never computes a wrong answer, only
sometimes an incomplete one.

## Cell64 substrate (previous line)

The dynamic boundary is one mutable 64-bit center word plus six immutable neighbor words:

```rust
pub fn update_cell(center: &mut CellWord, adjacent: &[CellWord; 6]) -> UpdateEffect
```

An activation cannot read coordinates, stable ids, endpoint ids, routes, a clock, or any cell
beyond those six neighbors. It can replace only `center`. The runner repeatedly activates a
fair deterministic random permutation of the sparse lattice and its one-cell halo. Later
activations observe earlier writes, so protocols must be delay-insensitive rather than rely on
a global barrier.

## Cell word

Every site is exactly one `u64`:

| Field | Bits | Purpose |
|---|---:|---|
| matter | 20 | empty, agent, link/cable, zipper, or fixed crossing |
| control | 28 | translation, cable shift, pressure response, rewrite, or contest |
| `χ` | 8 | obstruction pressure |
| `σ` | 8 | standing consumer shell |
| total | **64** | constant for every site and every phase |

Stable display ids and semantic events are observer sidecars. They are updated from
`UpdateEffect` after a cell activation and never enter the local rule.

`cell64.rs` defines the field ADTs, validates their geometry, and packs/unpacks the word. Its
tests exhaust every agent orientation, all 15 link face-pairs, all 120 zipper orientations,
all 45 fixed crossing geometries, and every rewrite address.

## Matter geometry

An arity-three agent has one principal face and one tail face. Both auxiliary ports occupy
ordered lanes of the tail cable:

```text
                 aux 1 ─ lane 0 ─┐
agent tail ═══════════════════════╪═ two-lane cable
                 aux 2 ─ lane 1 ─┘
```

The matter variants are:

- `Agent`: tag, principal face, optional tail face, and auxiliary lane order;
- `Link`: one exclusive face-pair with either one or two lanes;
- `Zip`: one two-lane trunk mapped to two ordered single-lane branches;
- `Cross`: exactly two independent single-lane routes using four distinct faces; and
- `Empty`.

A zipper is a real local cell, not metadata. A cable stays doubled while an agent moves along
it and opens only where its two semantic wires need separate destinations. A crossing never
joins, repoints, or displaces either route. No requester moves another occupant; unavailable
space is represented by a blocked response and pressure.

## Translation

A producer advances through one adjacent single-lane principal link. Translation uses three
persistent roles—source, target, and tail—with
`offer → acknowledge → commit → done` coordination. For an arity-three agent:

```text
before:  zipper ══ source ─ target-link ─ …
after:   zipper ══ double-link ══ source ─ …
```

The target becomes the agent. The vacated source becomes one two-lane link, so both auxiliary
wires remain overlapped behind it. There is no diagonal swept cell and the agent moves by
exactly one face-neighbor. The observer id moves only when the source commits.

## Rewrite workshop

`rewrite64.rs` holds the hand-crafted `A·F` workshop (16 cells, request tree, bilayer lift).
`compile64.rs` compiles the workshops for the complete 26-rule ROM: a deterministic search
(seat library, boundary branch candidates, per-net Dijkstra routing over z ∈ {0, ±1} with
rip-up retries) whose candidates are accepted only when they pack, connect, fit the 63-slot
budget, and — checked against the real fixture — project exactly onto the fired shadow net.
One canonical patch per rule and handedness is cached and rotated into every `(axis, side,
lift)` orientation; `A·F` keeps its hand workshop as ROM index 2.

The rewrite is a directional request/response protocol (unchanged for the full ROM):

1. The driver requests the preferred lift direction from its adjacent request-tree children.
2. Empty children accept locally and relay the same request by one face.
3. `Ready` responses return from leaves after the complete descendant region is available.
4. Any occupied, conflicting, or out-of-bounds child returns `Blocked`; matter is unchanged.
5. After a blocked request has cleared, the driver retries the opposite lift direction once.
6. Once the selected direction is ready, the driver begins one outward `Place` wave and emits
   the semantic interaction event.
7. Every participant writes only its own final matter; `Placed` responses confirm completion
   through the same tree before controls clear.

The local transition from `Request` to `Blocked` emits an obstruction-pressure pulse. The
requester still cannot modify the refusing cell; pressure is the only clearance signal.

`compile64.rs` source layout: the ROM netlist is lowered to physical port endpoints (boundary
zip/link branches for the dying pair, seat + zipper faces for fresh agents), a deterministic
search places and routes the patch, `check_projection` against the fired shadow net is the
acceptance oracle, and canonical patches rotate into all orientations. Compiled slot counts
range from 2 (`Eps·L`) to 63 (`T1·S`, at the `U6` cap).

## Source layout

- `crate/src/cell64.rs` — exact word ADTs and codec.
- `crate/src/substrate.rs` — sparse `Grid64`, zipper-aware tracing, projection, and loader.
- `crate/src/packed_local.rs` — center-only update rule and fair live-read runner.
- `crate/src/rewrite64.rs` — hand-crafted A·F workshop and local rewrite geometry.
- `crate/src/compile64.rs` — searched, projection-verified workshops for the complete ROM.
- `crate/src/suite.rs` — the Cell64 suite: scenario registry, fixtures, runner, expectations.
- `crate/src/tracejs.rs` — schema-4 trace JSON serialization shared by the generators.
- `crate/src/rules.rs` — validated 26-rule semantic ROM.
- `crate/src/net.rs` — abstract shadow net.
- `crate/src/oracle.rs` — independent recursive normalizer.
- `crate/src/bin/dump-suite.rs` — suite runner and `lattice_suite.js` generator.
- `crate/src/bin/dump-packed.rs` — single-scenario activation trace generator.
- `crate/tests/cell64_suite.rs` — the suite gate: every scenario, four adversarial seeds.
- `crate/tests/compile64.rs` — workshop structure, projection, live placement, rotation.

The broader semantic and geometry regression modules remain available while Cell64 workshop
coverage is extended across the ROM.

## The Cell64 suite and the viewer

The suite is the smallest set of things the packed substrate must always do, plus example
reductions at the current frontier:

- `translate-straight`, `translate-bend` (must) — an arity-three producer walks three
  one-lane links, straight or around a bend, leaving one continuous two-lane cable.
- `af-preferred`, `af-fallback`, `af-blocked` (must) — the docked A·F workshop: preferred
  lift, forced opposite-lift retry, and both lifts declined with matter unchanged.
- `rule-<c>-<p>` × 25 (must) — the rule atlas: every ROM interaction fires its compiled
  workshop from a docked pair, places the fresh agents, and projects exactly onto the fired
  shadow net.
- `reduce-apply-fork` (example) — reduction end to end: F translates three faces to dock
  with A, the workshop fires, T1 + Pair are placed.
- `cascade-apply-fork` (example) — a multi-fire chain: A·F, then a leaf producer crosses
  zipper and trunk cells as a guest rider to dock with T1, and T1·L fires; the chain then
  stalls at Unp·Pair (arity-three riders are the open cable-shift frontier).
- `cascade-eps-fork` (example) — the first complete multi-fire reduction to a normal form:
  Eps·F places the two erasers, both leaves guest-ride across, Eps·L fires twice, and every
  agent is erased (frontier 0).
- `cascade-fork-stem` (example) — the S-rule end to end: A·F fires, the stem guest-rides
  (arity-two crossing) to T1, and T1·S places its 63-cell workshop before the chain stalls
  at the arity-three crossings.
- `embed-term` (example) — the embedder lays out `@(F(L,L),L)` under Nrm; producers
  translate, Nrm forces the suspension (Nrm·P fires once), and the run stalls on a
  producer–producer traffic jam (no reroute mechanism yet).
Schema 4 records one complete keyframe and then one-cell deltas. Each activation event
carries its sweep round, its index within the round, and the round's activation-set size, so
the fair random order is visible during replay.

Run the suite (a fast pass/fail summary) and regenerate the viewer bundle from `crate/`:

```sh
cargo test --release --test cell64_suite
cargo run --release --bin dump-suite -- 0 ../../../research/interaction-combinator/lattice_suite.js
```

`research/interaction-combinator/lattice_player.html` replays the bundle. It draws two cable
lanes separately, marks zippers as square split/join cells, shows fixed crossings as two
independent routes, outlines active control roles, and marks the currently and next
activated cells so the no-clock order is visible. Selecting a cell shows its matter,
control, fields, exact word, face reciprocity, and constant bit budget. The view is a
rotatable orthographic 3D camera; keybinds pan, zoom, rotate, step, and jump between a
selected cell's activations and word changes.

## Verification

From `crate/`:

```sh
cargo test --release --lib
cargo test --release --test cell64_suite
cargo test --release rewrite64 --lib
cargo test --release packed_local --lib
```

Current execution coverage includes packed loading/projection, live-read cell-by-cell
translation (straight and bent, three-face walks), bonded heat/field relaxation, the
complete 26-rule workshop ROM (searched and projection-verified in `compile64.rs`), one
end-to-end reduction (translation, docking, fire, placement), and a multi-fire cascade
across guest-rider cable crossings — all gated by the Cell64 suite under four adversarial
activation seeds. Cable shift for arity-three riders (the tail-unzip wave) and
pressure-relief controls remain the open frontier before Cell64 is a complete evaluator
backend; `embed-term` marks the embedder's bent-arrival pose issue.
