//! Six-neighbor cellular substrate for disp's tree-calculus interaction net.
//!
//! The Cell64 path is [`cell64`] → [`substrate`] → [`packed_local`], with face-relative
//! rewrite workshops in [`rewrite64`]. Its dynamic rule mutates one 64-bit center from six
//! immutable neighbor words under a fair live-read activation order. Stable ids, coordinates,
//! events, and the projection shadow do not cross that rule boundary.
//!
//! [`rules`], [`net`], and [`oracle`] define and independently check the semantic layer. The
//! remaining modules provide the broader geometry regression harness used while packed
//! workshop coverage is extended across the complete rule ROM.

pub mod rules;
pub mod oracle;
pub mod net;
pub mod cell64;
pub mod substrate;
pub mod packed_local;
pub mod rewrite64;
pub mod lattice;
pub mod local;
pub mod transitions;
pub mod scheduler;
pub mod fixtures;
