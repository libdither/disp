//! rust-ca-lattice — the local CA substrate for disp's tree-calculus interaction nets.
//!
//! The implementation is organized around these layers:
//!
//! - `rules`   — the interaction rules frozen as a validated template ROM, over the
//!               LOWERED alphabet (every agent ≤3 ports; T1/T2 carried by Pair/Sel/Unp).
//! - `oracle`  — the independent recursive normalizer (shares no code with the engines).
//! - `net`     — the table-driven abstract engine and the shadow against which lattice
//!               projection is checked.
//! - `lattice` — six-face `(x, y, z)` cell state, topology, loader, and projection.
//! - `local`   — frozen-snapshot, radius-one, center-write geometry protocols.
//! - `transitions` — bounded host-planned transitions. Every transition declares the
//!               complete cell set it may touch, and `apply_*` checks that footprint.
//! - `scheduler` — the deterministic schedule combining local protocols and declared
//!               host-planned transitions.
//!
//! The correctness spec is the projection invariant (EMBEDDING_THEOREM.md §4). At every
//! seed-free check point the lattice projects to a well-formed abstract net; geometry
//! transitions project to identity and a completed fire projects to exactly one interaction.
//! Tests compare normal forms with an independent oracle, while liveness is reported rather
//! than assumed. Seed growth is an internal protocol interval and is checked when it commits
//! or aborts back to a projectable state.

pub mod rules;
pub mod oracle;
pub mod net;
pub mod lattice;
pub mod local;
pub mod transitions;
pub mod scheduler;
pub mod fixtures;
