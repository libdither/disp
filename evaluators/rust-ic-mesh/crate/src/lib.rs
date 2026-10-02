//! disp's tree-calculus interaction net on a flat mesh of message-passing tiles
//! (evaluators/rust-ic-mesh/README.md). [`mesh`] is the machine, [`polarity`] the one fact
//! about the rule ROM it relies on, [`term`] the front end, and [`run`] the one-call driver.

pub mod mesh;
mod tile;
pub mod polarity;
pub mod term;
pub mod run;
#[cfg(all(target_arch = "wasm32", feature = "player"))]
pub mod wasm;
