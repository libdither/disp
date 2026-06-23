//! Rooted TC-Net: a sequential interaction-net evaluator for disp tree calculus.
//!
//! **M0 SCAFFOLD.** This file pins the FFI surface (the C-ABI exports the disp
//! `Session` wrapper calls) and the arena/agent shape. The reduction engine is
//! the work of `TC_NET_PLAN.md` (M0 = δˢ-only sequential; M1 = δⁿ call-by-need +
//! demand-driven scheduler + reachability GC). Every body is `todo!()` until then.
//!
//! Design: `research/interaction-combinator/tc-net.typ` (agents, dispatch +
//! sharing rules, the δⁿ demand-before-copy variant, hash-consing/equality).
//! FFI sizing: `EVALUATOR_PLAN.md` §4.2 — only the ternary string crosses as
//! bytes; `leaf/stem/fork/apply` are `u32 → u32`, numbers cross free. A WASM
//! instance owns exactly one arena; the host's `dispose()` drops the instance,
//! so the arena vanishes wholesale (no per-handle free).
#![allow(dead_code, unused_variables, clippy::missing_safety_doc)]

use std::cell::RefCell;

/// A node in the term graph — the *producers* of tc-net.typ §Agents. `L`/`S`/`F`
/// are constructors; `P` is a suspended application (the call-by-need parking
/// point). The *consumers* (`A` apply, `T1`/`T2` triage, `δˢ`/`δⁿ` duplicators,
/// `ε` erase) are reduction steps over these, not stored node kinds (M1).
#[derive(Clone, Copy)]
enum Node {
    Leaf,
    Stem(u32),
    Fork(u32, u32),
    /// `P(f, a)`: suspended `apply f a`, reduced on demand (the rooted-construction root).
    Susp(u32, u32),
}

/// One session's state (a WASM instance owns exactly one). Handles are indices
/// into `nodes`. M1 adds: the active-pair work queue, the `δⁿ` parked-duplicator
/// table, and reachability-GC marks for parked duplicators whose results are
/// erased (tc-net.typ §Costs of δⁿ).
struct Arena {
    nodes: Vec<Node>,
    // hash-cons: (tag, child ids) → id, so structural copy can return a shared
    // ref (tc-net.typ §Implementation Note). NB: with live suspensions, id≠id no
    // longer implies ≠, so `tc_equal` is demand-then-compare, NOT an id check
    // (EVALUATOR_PLAN decision 1).
}

thread_local! {
    static ARENA: RefCell<Arena> = RefCell::new(Arena { nodes: vec![Node::Leaf] });
}

// ── term algebra (Session.leaf / stem / fork) ───────────────────────────────
#[no_mangle] pub extern "C" fn tc_leaf() -> u32 { todo!("M0: intern Leaf (hash-consed)") }
#[no_mangle] pub extern "C" fn tc_stem(child: u32) -> u32 { todo!("M0: intern Stem") }
#[no_mangle] pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 { todo!("M0: intern Fork") }

// ── computation (Session.apply) ─────────────────────────────────────────────
/// Build `P(f, x)`; reduce on demand. `budget` bounds interactions; exhaustion
/// returns `u32::MAX` (the host maps that to BudgetExhausted — no panic across
/// the boundary). M0: δˢ-only, eager-ish. M1: the two-level dispatch rules +
/// δⁿ-spawning S-rule + demand-driven scheduling (tc-net.typ §Dispatch, §Sharing).
#[no_mangle] pub extern "C" fn tc_apply(f: u32, x: u32, budget: u32) -> u32 {
    todo!("M0/M1: A-dispatch + sharing rules")
}

// ── bulk / interchange (Session.loadTernary / dumpTernary) ──────────────────
/// Allocate `len` bytes; host writes the ternary string there, then calls
/// `tc_load_ternary`. The only per-term byte copy (EVALUATOR_PLAN §4.2).
#[no_mangle] pub extern "C" fn tc_alloc(len: u32) -> u32 { todo!("M0: bump/Vec alloc in linear memory") }
#[no_mangle] pub extern "C" fn tc_free(ptr: u32, len: u32) { todo!("M0") }
/// Parse a preorder `0`/`1`/`2` ternary string (byte-identical to disp's codec) into nodes.
#[no_mangle] pub extern "C" fn tc_load_ternary(ptr: u32, len: u32) -> u32 { todo!("M0: parse → intern") }
/// Force full normal form, serialize to ternary, return `(ptr << 32) | len`
/// (host reads `len` bytes at `ptr` from exported `memory`, then `tc_free`s).
/// `budget` bounds the forced reduction. Needs the full-normalizer consumer (M1).
#[no_mangle] pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 { todo!("M1: full normalize + serialize") }

// ── observations (optional native overrides of engine-side derivations) ─────
/// Weak-head classify: returns `0`=leaf, `1`=stem, `2`=fork (children via
/// `tc_child0/1`); `u32::MAX` on budget exhaustion. Forces WHNF (tc-net.typ
/// weak-head equality).
#[no_mangle] pub extern "C" fn tc_classify(h: u32, budget: u32) -> u32 { todo!("M1: demand to WHNF") }
#[no_mangle] pub extern "C" fn tc_child0(h: u32) -> u32 { todo!("M1") }
#[no_mangle] pub extern "C" fn tc_child1(h: u32) -> u32 { todo!("M1") }
/// Normal-form equality: demand both sides, compare canonical NFs — O(min size),
/// the demand-then-compare check (NOT the eager backend's O(1) id check; decision 1).
#[no_mangle] pub extern "C" fn tc_equal(a: u32, b: u32, budget: u32) -> u32 { todo!("M1") }
