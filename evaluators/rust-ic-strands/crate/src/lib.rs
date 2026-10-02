//! Interaction nets as strands on the links of a lattice (evaluators/rust-ic-strands).

pub mod lattice;

pub(crate) fn polarity_is_source(t: rust_ca_lattice::rules::Tag, p: usize) -> bool { rust_ic_mesh::polarity::is_source(t, p) }
