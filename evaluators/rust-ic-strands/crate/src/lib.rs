//! Interaction nets as strands on the links of a lattice (evaluators/rust-ic-strands).

pub mod lattice;
pub mod readback;
pub mod tables;
#[cfg(feature = "gpu")]
pub mod gpu;
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub(crate) fn polarity_is_source(t: rust_ca_lattice::rules::Tag, p: usize) -> bool { rust_ic_mesh::polarity::is_source(t, p) }
