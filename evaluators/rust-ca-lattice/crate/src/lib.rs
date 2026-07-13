//! rust-ca-lattice — the local CA substrate for disp's tree-calculus interaction nets.
//!
//! Implements LOCAL_CA_DESIGN.md rungs 1–2 (see research/interaction-combinator/):
//!
//! - `rules`   — rung 1: the interaction rules frozen as a validated template ROM, over the
//!               LOWERED alphabet (every agent ≤3 ports; T1/T2 carried by Pair/Sel/Unp).
//! - `oracle`  — the independent recursive normalizer (shares no code with the engines).
//! - `net`     — the table-driven abstract engine; stage-1 differential subject AND the
//!               shadow the lattice checks its projection against.
//! - `lattice` — rung 2 state after the TOPO LIFT: (x, y, z) cells with six faces, over a
//!               selectable topology (bilayer = the 2.5D chip, or full 3D). Wire layers,
//!               vias, and tucks are GONE — z is the capacity they were simulating.
//!               The loader and the projection live here.
//! - `transitions` — the footprint-atomic transitions (FIRE, REEL, and the polymer moves
//!               RETRACT + KINK-FLIP); every transition declares the cell set it may
//!               touch, and apply() machine-checks it.
//! - `scheduler` — the sequential deterministic schedule (rung 3 adds parallel + async
//!               fuzz over the SAME transitions).
//!
//! The correctness spec is the projection invariant (EMBEDDING_THEOREM.md §4): at every
//! step the lattice projects to a well-formed abstract net, and every transition projects
//! to identity (REEL) or exactly one interaction (FIRE). The differential-oracle discipline
//! is the project's usual one: an independent oracle, bit-compared normal forms, zero
//! tolerated mismatches; liveness (reaching NF) is MEASURED, never assumed.

pub mod rules;
pub mod oracle;
pub mod net;
pub mod lattice;
pub mod transitions;
pub mod scheduler;
