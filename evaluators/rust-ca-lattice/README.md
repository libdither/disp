# rust-ca-lattice

The six-neighbor cellular substrate for disp's tree-calculus interaction net.

## Cascade substrate

`cascade.rs` and its siblings implement the event-driven substrate specified in
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
  contention exists only where two wake fronts touch: 0 conflicts for disjoint cascades,
  low single-digit percent under deliberately crossing traffic at 8 threads, with
  bit-identical results. Movement, heat, swaps, docks, and blocklet growth (place, hop,
  resolve, finalize) all run parallel; the full atlas grows through the claim machinery
  bit-identically across 1/2/4/8 threads. Retraction, seed arbitration, and congestion
  relief stay serial: a blocked op waits, so congested runs park earlier here than on the
  serial runner.
- `cascade_gather.rs`: the GPU/shader lowering. A repeating six-phase domino schedule
  (axis times parity) partitions the lattice into disjoint pairs; each phase is one pure
  gather into a double buffer, deterministic and bit-identical run to run. It shares the
  pair-decision function with the threaded driver.

Gates: `cargo test --release --lib` (codec exhaustives, blocklet compiler, movement,
parallel, gather) and `cargo test --release --test cascade_suite` (the 26-rule atlas under
all four disciplines, translation straight and bent, the A·F roll-fallback and declined
docks, `@(L,L)` normalized end to end under all four, and the pinned deep-reduction
frontier: `@(F(L,L),L)` reaches its normal form in six chained fires). `dump-cascade`
regenerates `research/interaction-combinator/lattice_cascade.js` with the historical
suite roster (translation, the A·F trio, all 26 atlas rules, the eraser cascade to empty,
the T1·S stem chain) plus the cascade-native demos; `lattice_player.html` replays the
bundle, one frame per generation, so a displayed tick is the maximal simultaneous
wavefront, and each trace's note reports its measured parallel width. `bench-cascade`
prints the timing snapshot and `debug-cascade` dumps a parked run's census.

Two relief rungs have landed. The first: growth-blocked cells evict cold routes
(corner-cut, straight shift, out-of-plane bracket), demand looks and wakes through
guests, hairpins collapse by truncation, and consumers can be swap partners. The second:
eviction recurses into its own blockers (full or lane-starved side cells, continuation
cells, and agents shedding their own passthroughs; blocked walkers, docks, and detours
relieve their own cells with the same primitive), U-turn folds splice out, cold shift
detours retract straight, a last-resort pass may move hot routes with their heat, and
colliding blocklets arbitrate by seed address (the loser retracts and re-docks).
Cooldown stamps damp displacement ping-pong. The frontier, pinned by
`frontier_deep_reductions` at 2 of 5 deep terms complete: the remaining parked runs are
corridor knots where a walker sits over a doubled hot foreign cable, and a wedged
growth chain behind it. The substrate never computes a wrong answer, only sometimes an
incomplete one.

## Source layout

- `crate/src/cascade.rs` — the word codec: four matter kinds, routes, split aux endpoints,
  seeds, the builder-cursor overlay, χ, the claim bit.
- `crate/src/blocklet.rs` — per-rule rewrite patches compiled once, deterministically;
  growth scripts and merge rules.
- `crate/src/cascade_run.rs` — the serial worklist runner: transition decisions, queue
  disciplines, demand pumping, eviction, the tracer, projection checks, and the corridor
  loader.
- `crate/src/cascade_par.rs` — the threaded driver over one shared `AtomicU64` array.
- `crate/src/cascade_gather.rs` — the deterministic six-phase gather driver.
- `crate/src/cascade_trace.rs` — schema-4 replay serialization for the player.
- `crate/src/rules.rs` — the validated 26-rule semantic ROM.
- `crate/src/net.rs` — the abstract shadow net (projection target).
- `crate/src/oracle.rs` — the independent recursive normalizer.
- `crate/src/lattice.rs` — positions, the six faces, topologies.
- `crate/src/bin/dump-cascade.rs` — regenerates the player bundle.
- `crate/src/bin/bench-cascade.rs`, `debug-cascade.rs` — timing snapshot, parked-run
  census.
- `crate/tests/cascade_suite.rs` — the suite gate described above.
- `crate/tests/stage1.rs` — the abstract net against the oracle: pins plus a 4000-term
  random differential with full ROM coverage.

## Verification

From `crate/`:

```sh
cargo test --release --lib
cargo test --release --test stage1
cargo test --release --test cascade_suite
cargo run --release --bin dump-cascade -- ../../../research/interaction-combinator/lattice_cascade.js
```

The previous Cell64 line (packed center-only update rule, translation handshakes, searched
rewrite workshops, request trees) and its suite were removed 2026-07-22 once every scenario
migrated to the cascade; git history holds them.
