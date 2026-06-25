//! rust-eager — a sequential, hash-consed reducer for disp tree calculus.
//!
//! **M0 + M1 (the hash-consed reducer strategy — TC_NET_PLAN.md §Implementation
//! strategy).** With hash-consing the formal interaction net collapses to a tree
//! reducer; the genuinely-materialized parallel net is the sibling `rust-ic-net` crate.
//! Materialized agents + parked duplicators + reachability GC are deferred to that M2
//! substrate, where they are actually required.
//!
//! - **M0 (eager):** [`reduce`](arena::Arena::reduce) fully normalizes `apply(f,a)` —
//!   the elaboration-conformance mode (disp's elaborator is eager-normative). This is
//!   what `tc_apply` ships.
//! - **M1 (lazy / call-by-need):** [`force`](arena::Arena::force) drives WHNF on demand,
//!   memoizing each visited `Susp` (`δⁿ` at-most-once). Off the production path; kept
//!   and validated so M2 inherits a live M1 core.
//!
//! Module map: [`arena`] (Node + hash-cons table + term algebra) · [`reduce`] (the
//! eager + lazy reducers + `equal`) · [`codec`] (ternary) · [`ffi`] (the `tc_*` Session
//! C-ABI). Hashing/maps are `hashbrown` + `rustc-hash`. A WASM instance owns exactly one
//! [`arena::Arena`]; `dispose()` drops it wholesale (grow-until-dispose absorbs the
//! per-session laziness leak — tc-net.typ §Costs of δⁿ).
#![allow(clippy::missing_safety_doc, dead_code)]

mod arena;
mod codec;
mod ffi;
mod reduce;
#[cfg(test)]
mod tests;

use arena::Arena;
use std::cell::RefCell;

thread_local! {
    /// The single per-instance session arena (one WASM instance = one session).
    static ARENA: RefCell<Arena> = RefCell::new(Arena::new());
}

/// Borrow the session arena mutably for one operation — the entry point every `tc_*`
/// export funnels through.
#[inline]
pub(crate) fn with<T>(f: impl FnOnce(&mut Arena) -> T) -> T {
    ARENA.with(|a| f(&mut a.borrow_mut()))
}
