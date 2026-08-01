# rust-ca-lattice

The six-neighbor cellular substrate for disp's tree-calculus interaction net.

## Cascade substrate

`cascade.rs` and its siblings implement the event-driven substrate specified in
`research/interaction-combinator/CASCADE_CELL_DESIGN.md`. One `u64` per site; the atomic
primitive is an edge transaction over one face-adjacent pair; there are no protocol phases,
no request trees, and no activation sweeps. Matter is four kinds: empty (with an optional
growth reservation), wire (up to three single-lane routes; reservations too), agent (tag,
principal endpoint, two independent aux endpoints, up to two passthrough routes), and seed.
Demand is the signal plane (`signal.rs`, runtime-selectable backends like queue
disciplines; raises commute and are property-pinned): consumers raise the wire route at
their principal, and nothing scans — the old five-hop `hot_beyond` lookahead and the
periodic contraction sweep are deleted. Only hot wires are walked, so undemanded values
never move. Three backends reach the same fixpoint: `worklist` (heat in the wire words,
extending one cell per activation, one guest-relay hop per generation; heat persists
until its route is rewired, an over-approximation of demand), and two derivational
backends that recompute the fixpoint from matter whenever the routing epoch moves —
`components` (union-find over route reciprocity: a cable heats in one instant and guest
chains are exact, the model of the unclocked fabric) and `dense` (the same fixpoint by
iterative recompute, GPU-shaped, deliberately sharing no machinery). The cross-backend
gate (`tests/signal_backends.rs`) holds each to the full substrate contract, including
the kick invariant; measured 2026-07-31, completion is per-case identical across all
three (28/54 on the gate corpus, zero verdict flips) even though exact-instant heat
floods walkers the creeping wave staggered and cools cables the moment their consumer
docks — the heat over-approximation is not load-bearing, and any future flips are
reported, never pinned. Walks eat their own slack
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
parallel, gather), `cargo test --release --test cascade_suite` (the 26-rule atlas under
all four disciplines, translation straight and bent, the A·F roll-fallback and declined
docks, `@(L,L)` normalized end to end under all four, and the deep-reduction frontier,
all five terms pinned to their normal forms — the deepest, disp-t, in 18 chained
fires), and
`cargo test --release --test invariants` (mechanism invariants: the kick/lost-wake gate,
per-op-class commit write-sets and read radii pinned as only-move-down ceilings, the
cooldown ablation lane — undamped, parks more and never answers wrong — and the
nursery negative control). `dump-cascade`
regenerates `research/interaction-combinator/lattice_cascade.js` with the historical
suite roster (translation, the A·F trio, all 26 atlas rules, the eraser cascade to empty,
the T1·S stem chain) plus the cascade-native demos, AND `lattice_frontier.js` (gitignored,
regenerated every run) carrying the live frontier corpus — the deep-reduction terms the
suite floors pin, parked ones included, plus recently-moved census family members — so
the player's picker never trails the suite; `lattice_player.html` replays the
bundle, one frame per generation, so a displayed tick is the maximal simultaneous
wavefront. Every trace's note opens with a verdict MEASURED from the run just recorded —
WORKING with its fire count, or STUCK with where it stopped — never a hand-written
claim, because hand-written outcomes go stale the moment the engine moves (two frontier
captions still said "parks" for terms that had started completing). The frontier tier
carries one exhibit per stuck class the park census still finds, named for the shape of
the stuck state rather than a term index, so a class that gets fixed flips its own
exhibit to WORKING and the next census says what to put there instead. `bench-cascade`
prints the timing snapshot and `debug-cascade` dumps a parked run's census.

Five relief rungs have landed. The first: growth-blocked cells evict cold routes
(corner-cut, straight shift, out-of-plane bracket), demand looks and wakes through
guests, hairpins collapse by truncation, and consumers can be swap partners. The second:
eviction recurses into its own blockers (full or lane-starved side cells, continuation
cells, and agents shedding their own passthroughs; blocked walkers, docks, and detours
relieve their own cells with the same primitive), U-turn folds splice out, cold shift
detours retract straight, a last-resort pass may move hot routes with their heat, and
colliding blocklets arbitrate by seed address (the loser retracts and re-docks). The
third: undemanded guests squatting on a demanded walker's wire or relief geometry are
shoved (a one-shot walk license; a cornered endpoint sidesteps, lengthening its own wire
by one retractable segment), relief is self-sustaining (every progress report wakes its
requester), and a cursor's own cell and reserved target are exempt from the prohibitions
protecting them from foreign relief. The fourth (live-cable relief, the composition that
completed k-chain): routes threading agents' passthrough lists swing like wire, a
requesting dock's ring is forbidden as a displacement receiver (relief drains rings),
only the address-lowest ready pair runs ring relief, and every displacement's primary
direction must ascend a fixed linear form (`Runner.relief_g`), so displacement cycles
are impossible at move granularity. The fifth (endpoint swings + the termination
redesign): a displaced route ending on an agent's own port re-anchors with the same
one-word swing, consumer squatters sidestep off hot wires, an over-full squatter sheds
its own passthrough, stamped evictions refuse plainly (stamps self-decay and their
expiry wakes the neighborhood), contraction and sidesteps obey the same order, a
hopeless growth merge never reserves, quiescence is re-checked by a full one-shot
sweep at the worklist's edge (the kick invariant true by construction), and shoves
fire only at guests with nothing of their own to act on (a demanded guest walks
itself; shoving it buys a sidestep it walks straight back from). The sixth loosens
the order where it can be paid for: a displacement may descend `relief_g` when the
route it moves is exactly the one whose removal lets a blocked placement's matter
merge, so every exception is the last move before progress that the reduction itself
bounds. The payment is verified per route (assuming it per request re-arms cycles
outright) and can still be stolen before the placement commits, which is why the
cooldown stamps stay load-bearing. A declined dock's last blocker pays the same way
(clearing it makes the ring whole), and with that in hand the ring-clearing bound had
nothing left to protect and was removed. Shoves no longer sidestep a guest off a
walker's wire: the guest leaves its trail in the contested cell and its principal
re-anchors into it, so the demand it was shoved out of marches it straight back. A
walker blocked by a stationary guest that carries its cable exchanges places with it
instead (`try_pass_guest`, the asymmetric counterpart of the head-on swap): cables
crossing the shared face are sorted into paired deliveries, the walker's trails, and the
guest's connectors, and the face's two lanes bound the sum — which is why an arity-3
agent at rest cannot be moved at all, only walked forward. As of 2026-08-01 the deep-reduction
frontier is complete: `frontier_deep_reductions` pins all five terms normalizing end
to end, the last being disp-t at its T1·F comb dock. The random soak, a harder and
much wider corpus, sits at 129 of 160. A run may still park, and a park is a valid
outcome the geometry checks hold to account; what the substrate never does is compute
a wrong answer.
`debug-cascade <term> --why` walks the relief decision tree for every blocked op and
prints each refusing check, and lists a declined dock's per-roll first-ring blockers
(the counts say whether it is one nudge from firing or genuinely crowded); `--kick` re-wakes a parked run to distinguish lost wakes
from genuine wedges; `--churn` is the pump playbook in one flag (warm up, count which
cells host mutating activations on the plateau, capture the decision notes filtered to
the top cells); `soak:N` reproduces a soak failure line verbatim (its term plus the
rotation's discipline and embedding, so "term 95 [AddressOrdered/tree]" is exactly
`debug-cascade soak:95 --churn`); `--backend worklist|components|dense` runs the same
diagnosis under a chosen signal backend (exact-instant heat surfaces pumps the
staggered wave masks); `--trace <out.js>` records the run as a schema-4
trace the player's load button replays; `regen.sh` in
`research/interaction-combinator/` regenerates the player bundle and revalidates it
(`validate_cascade_bundle.mjs` checks it against the player's data contract).

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
- `crate/src/bin/park-census.rs` — classifies every parked soak run by the shape of its
  stuck state, so capability work is aimed by distribution rather than by anecdote.
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
