//! The cascade cellular substrate for disp's tree-calculus interaction net
//! (research/interaction-combinator/CASCADE_CELL_DESIGN.md).
//!
//! One `u64` per site; the atomic dynamic primitive is an edge transaction over one
//! face-adjacent pair. [`cascade`] is the word codec, [`blocklet`] compiles the per-rule
//! rewrite patches, and three drivers run the same transition rules: [`cascade_run`]
//! (serial worklist, generations as physical ticks), [`cascade_par`] (N threads over one
//! shared `AtomicU64` array, claims only where cascades meet), and [`cascade_gather`]
//! (the deterministic six-phase GPU/shader lowering). [`rules`], [`net`], and [`oracle`]
//! define and independently check the semantic layer; [`cascade_trace`] serializes
//! player replays.

pub mod rules;
pub mod oracle;
pub mod net;
pub mod lattice;
pub mod cascade;
pub mod cascade_run;
pub mod blocklet;
pub mod cascade_par;
pub mod cascade_gather;
pub mod cascade_trace;
