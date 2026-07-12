# rust-ca-lattice

The local CA substrate for disp's tree-calculus interaction nets: rungs 1 and 2 of
`research/interaction-combinator/LOCAL_CA_DESIGN.md`, built as a Rust crate under the
project's differential-oracle discipline. Not yet a `Session` backend (that is rung 4);
today it is a library plus tests.

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
- `crate/src/lattice.rs` (rung 2 state) — per-cell state, no ids in the dynamics: an agent
  cell (≤3 ports on half-edges, ≤2 tucked strands behind it) or a wire cell (≤3 strands).
  Connectivity is positional; every face carries up to two WIRE LAYERS, so a half-edge is
  `(direction, layer)`. The loader (host code, global routing allowed) and the executable
  projection invariant (`check_projection`) live here.
- `crate/src/transitions.rs` (rung 2 dynamics) — footprint-atomic transitions: `plan_*`
  reads a bounded neighborhood and returns a Plan carrying its exact footprint; `apply_*`
  machine-checks every write against it. FIRE (dying cells become splice hubs; bounded
  in-board placement backtracking + wire router), REEL (walk one cell along the principal,
  gated on "a fire awaits"; aux wires bend through the vacated cell, corner-detour
  fallback; crossings tuck behind the mover).
- `crate/src/scheduler.rs` — the sequential deterministic schedule. Rung 3 adds Margolus
  blocks and async-fuzz interleavings over the SAME transitions.
- `crate/tests/stage1.rs` — abstract net vs oracle: 4000 random terms, zero mismatches,
  full 26-rule coverage enforced.
- `crate/tests/stage2.rs` — lattice vs oracle: correctness gated absolutely (zero wrong
  NFs, per-transition projection asserts, bit determinism); liveness measured.
- `crate/src/bin/debug-stuck.rs`, `scan-pins.rs` — stall analyzers.

## Status (measured)

`cargo test --release`: all green. Stage 1: 3998+/0. Stage 2 corpus (400 random terms,
depths 3–5): 211 reach normal form, 189 stall, ZERO wrong results, zero invariant
violations, zero tick-cap hits. Every stall is liveness, never correctness: the bare
sequential schedule ships with no relaxation policies (no straighten, no drift, no
pressure), and dense strand pockets around the reduction focus jam walks and the larger
fire templates. That residue is LOCAL_CA_DESIGN.md §13's liveness question, now measurable
against a sound substrate; policies compete on top of it without being able to break
correctness (they only choose among enabled transitions, and every transition preserves
the projection invariant).

Liveness findings already folded into the design (each found by measurement here):

1. Walk only when a fire awaits (far end of the principal is a consumer's principal):
   values that walked to their parent's aux port squatted on crossings and deadlocked the
   crossed wire's owners.
2. Exclusive faces are over-strict: with one attachment per face, a 3-port walker cannot
   pass any via (3 + 2 tuck faces > 4). Per-edge wire layers (2), which §2/§3 already
   describe, dissolve the class; §4.4's face-uniqueness survives as a placement preference.
3. Place-and-route inside a fire's footprint needs backtracking and most-constrained-first
   wire ordering; greedy first-fit seals its own hubs.
