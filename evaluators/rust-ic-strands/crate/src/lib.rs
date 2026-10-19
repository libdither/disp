//! Interaction nets as strands on the links of a lattice (evaluators/rust-ic-strands).

pub mod iso;
pub mod lattice;
pub mod readback;
pub mod resid;
pub mod rev;
pub mod load;
pub mod tables;
pub mod twoway;
#[cfg(feature = "gpu")]
pub mod gpu;
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub(crate) fn polarity_is_source(t: tree_net::rules::Tag, p: usize) -> bool { tree_net::polarity::is_source(t, p) }
