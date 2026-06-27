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
mod memo;
mod reduce;
#[cfg(test)]
mod tests;

use arena::{Arena, LEAF_ID};
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

/// Native CLI entry (`src/bin/eager-cli.rs`) — the fair native-subprocess baseline for the
/// rust-ic-net benchmark (M2d). Mirrors src/eval/batch.ts#fold: left-fold eager-apply over
/// the ternary `terms`, serialize the NF. Returns `(nf, reduce_ms, interactions)`; the timer
/// excludes arena allocation (matching disp-cli's "session created before t0"). None on
/// budget exhaustion.
#[cfg(not(target_arch = "wasm32"))]
pub fn reduce_fold_timed(terms: &[Vec<u8>], budget: i64) -> Option<(String, f64, u64)> {
    let mut a = Arena::new();
    let t0 = std::time::Instant::now();
    let mut b = budget;
    let mut acc: Option<u32> = None;
    for t in terms {
        let mut i = 0usize;
        let term = a.parse(t, &mut i);
        acc = Some(match acc {
            None => term,
            Some(f) => a.reduce(f, term, &mut b).ok()?,
        });
    }
    let h = acc?;
    let mut out = Vec::new();
    a.dump_emit(h, &mut out, &mut b).ok()?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((String::from_utf8(out).ok()?, ms, a.interactions))
}

// The same wide independent-chain workload rust-ic-net's CLI builds (2^depth chains of
// not^chain false) — the sequential baseline against ic-net's parallel sweep.
#[cfg(not(target_arch = "wasm32"))]
fn build_not_eager(a: &mut Arena) -> u32 {
    let sl = a.stem(LEAF_ID);
    let fll = a.fork(LEAF_ID, LEAF_ID);
    let inner = a.fork(sl, fll);
    a.fork(inner, LEAF_ID)
}
#[cfg(not(target_arch = "wasm32"))]
fn build_wide_eager(a: &mut Arena, d: usize, chain: usize) -> u32 {
    if d == 0 {
        let mut e = LEAF_ID;
        for _ in 0..chain {
            let not = build_not_eager(a);
            e = a.susp(not, e);
        }
        e
    } else {
        let l = build_wide_eager(a, d - 1, chain);
        let r = build_wide_eager(a, d - 1, chain);
        a.fork(l, r)
    }
}
/// Build + sequentially reduce the wide workload (the rust-eager baseline for ic-net's
/// parallel sweep). Returns `(nf, reduce_ms, interactions)`; None on exhaustion.
#[cfg(not(target_arch = "wasm32"))]
pub fn reduce_wide_timed(depth: usize, chain: usize, budget: i64) -> Option<(String, f64, u64)> {
    let mut a = Arena::new();
    let t0 = std::time::Instant::now();
    let h = build_wide_eager(&mut a, depth, chain);
    let mut b = budget;
    let mut out = Vec::new();
    a.dump_emit(h, &mut out, &mut b).ok()?;
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    Some((String::from_utf8(out).ok()?, ms, a.interactions))
}
