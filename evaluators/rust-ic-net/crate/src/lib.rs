//! rust-ic-net — a MATERIALIZED interaction-net evaluator for disp tree calculus.
//!
//! Agents and ports are REAL arena nodes; active pairs sit in a schedulable redex bag.
//! Unlike the sibling `rust-eager` (a hash-consed tree reducer, the default checker
//! backend), nothing is shared by hash-consing — the price of admission for the optimizer's
//! per-candidate provenance (DISP_BACKPROP) and for distributing reduction across threads.
//! Differentially gated against `rust-eager` / disp-eager (strong confluence ⇒ identical
//! NFs). Design: research/interaction-combinator/RUST_IC_NET_DESIGN.md; calculus: tc-net.typ.
//!
//! Milestones (landed through M2d; see the design doc §10):
//! - **M0** — the full rule kernel with δˢ (structural copy), sequential drain.
//! - **M1** — the S-rule spawns δⁿ (demand-before-copy): a duplicated SUSPENSION's work
//!   runs at most once (Theorem 6). **M1b** — free-on-consume GC (linear net ⇒ each agent
//!   consumed once; peak arena tracks the live working set).
//! - **M2a/b/c** — atomic exchange linker (no CAS) + a shared `&Net` atomic arena + a
//!   native parallel work-stealing drain (`parallel`, non-wasm only).
//! - **M2d** — native CLI / benchmark entry points (`bench`, non-wasm only).
//!
//! Module map: [`port`] (the tagged-word encoding + agent tags) · [`net`] (the shared
//! atomic arena, the exchange linker, the term algebra) · [`reduce`] (the `interact` rule
//! kernel + `drain` + `full_nf`/`equal`) · [`codec`] (ternary interchange) · [`parallel`]
//! (the M2c parallel drain) · [`tiled`] (the E3 tiled drain: per-thread arena stripes,
//! placement at birth, inbox routing) · [`bench`] (native CLI helpers) · [`ffi`] (the wasm
//! `tc_*` Session C-ABI). The wasm build is the SEQUENTIAL oracle; the native build
//! additionally compiles `parallel` + `bench`.
#![allow(clippy::missing_safety_doc)]

mod codec;
mod ffi;
mod net;
mod port;
// Wire-RC rides `Worker` (net.rs) like the tracer/tiles, so the module compiles on wasm
// too; only the native `-rc` entry point constructs it, leaving it dead on wasm.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
mod rc;
mod reduce;
// The tile types ride `Worker` (net.rs), so the module compiles on wasm too; the driver
// (threads) is cfg-excluded there, leaving the driver-only constructors dead on wasm.
#[cfg_attr(target_arch = "wasm32", allow(dead_code))]
mod tiled;
mod trace;

// crossbeam/threads (parallel) and the native CLI helpers don't build on wasm32 — the wasm
// build is the single-threaded oracle, so they are `#[cfg]`-excluded there.
#[cfg(not(target_arch = "wasm32"))]
mod bench;
#[cfg(not(target_arch = "wasm32"))]
mod parallel;

#[cfg(not(target_arch = "wasm32"))]
pub use bench::{
    reduce_fold_rc, reduce_fold_tiled, reduce_fold_tiled_aware, reduce_fold_timed,
    reduce_fold_traced, reduce_wide_tiled, reduce_wide_tiled_aware, reduce_wide_timed,
    RcReport, TraceReport,
};
#[cfg(not(target_arch = "wasm32"))]
pub use tiled::TiledStats;

#[cfg(test)]
mod tests;
