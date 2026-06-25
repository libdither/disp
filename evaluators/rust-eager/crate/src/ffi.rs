//! The Session C-ABI (`tc_*` exports). Numbers-only except the one ternary byte-copy
//! per term (EVALUATOR_PLAN §4.2). Handles are `u32` arena indices, opaque to the host;
//! budget exhaustion returns a sentinel, never traps (`panic = "abort"`).

use crate::arena::{Arena, Node, LEAF_ID};
use crate::with;

#[inline]
fn pack(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

/// Run a closure with a fresh `i64` budget threaded from the FFI `u32` (the shared
/// shape of the forcing exports: apply / dump / classify / equal).
#[inline]
fn with_budget<T>(budget: u32, f: impl FnOnce(&mut Arena, &mut i64) -> T) -> T {
    with(|a| {
        let mut b = budget as i64;
        f(a, &mut b)
    })
}

/// WHNF a handle with an effectively-unbounded budget and return its node (the host
/// calls `tc_classify` first, so the force is usually a no-op cache hit).
#[inline]
fn whnf_node(a: &mut Arena, h: u32) -> Node {
    let mut b = i64::MAX;
    let w = a.whnf(h, &mut b).unwrap_or(LEAF_ID);
    a.node(w)
}

// ── term algebra (Session.leaf / stem / fork) ───────────────────────────────
#[no_mangle]
pub extern "C" fn tc_leaf() -> u32 {
    LEAF_ID
}
#[no_mangle]
pub extern "C" fn tc_stem(child: u32) -> u32 {
    with(|a| a.stem(child))
}
#[no_mangle]
pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 {
    with(|a| a.fork(left, right))
}

// ── computation (Session.apply) ─────────────────────────────────────────────
/// **EAGER (M0)** — fully normalize `apply(f, x)`. The elaboration-conformance mode:
/// disp's elaborator is eager-normative (EVALUATOR_PLAN decision 7), so the full-
/// `lib/tests` gate needs eager reduction (a lazy `tc_apply` defers redexes the
/// elaborator's host-side observations assume are reduced). `u32::MAX` on exhaustion.
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, budget: u32) -> u32 {
    with_budget(budget, |a, b| a.reduce(f, x, b).unwrap_or(u32::MAX))
}

/// M1 lazy apply: `apply f x = Susp(f, x)`, reduced on demand by the forcing
/// observations (`dump`/`classify`/`equal`). The work-sharing path (δⁿ at-most-once);
/// exposed for the reduction differential + laziness benchmark, not the Session `apply`.
#[no_mangle]
pub extern "C" fn tc_apply_lazy(f: u32, x: u32, _budget: u32) -> u32 {
    with(|a| a.susp(f, x))
}

/// Register the `tree_eq` definition's handle (Session.recognizeNative) so
/// `apply(tree_eq, a) b` fast-paths to the O(1) hash-cons compare. Idempotent.
#[no_mangle]
pub extern "C" fn tc_recognize_tree_eq(handle: u32) {
    with(|a| a.tree_eq_id = handle);
}

// ── memory knobs (run at smaller footprints) ────────────────────────────────
/// Cap the eager apply memo at `n` entries (0 = unbounded); over the cap the memo is
/// cleared. Pure cache → correctness-preserving (just re-reduces).
#[no_mangle]
pub extern "C" fn tc_set_memo_limit(n: u32) {
    with(|a| a.memo_limit = if n == 0 { usize::MAX } else { n as usize });
}
/// Drop all caches (apply memo + susp WHNF memo) and release their backing memory. The
/// node arena (live trees reachable via handles) is untouched — only re-derivable cache.
#[no_mangle]
pub extern "C" fn tc_clear_caches() {
    with(|a| {
        a.memo.clear();
        a.memo.shrink_to_fit();
        a.forced.clear();
        a.forced.shrink_to_fit();
    });
}

// ── bulk / interchange (Session.loadTernary / dumpTernary) ──────────────────
#[no_mangle]
pub extern "C" fn tc_alloc(len: u32) -> u32 {
    let mut buf = vec![0u8; len as usize];
    let ptr = buf.as_mut_ptr() as u32;
    std::mem::forget(buf);
    ptr
}
#[no_mangle]
pub unsafe extern "C" fn tc_free(ptr: u32, len: u32) {
    if ptr != 0 {
        drop(Vec::from_raw_parts(ptr as *mut u8, len as usize, len as usize));
    }
}
#[no_mangle]
pub unsafe extern "C" fn tc_load_ternary(ptr: u32, len: u32) -> u32 {
    let bytes = std::slice::from_raw_parts(ptr as *const u8, len as usize);
    with(|a| {
        let mut i = 0usize;
        a.parse(bytes, &mut i)
    })
}
/// Force full NF, serialize to ternary, return `(ptr << 32) | len`. On budget exhaustion
/// returns `(u32::MAX << 32)` (ptr = `u32::MAX`, len = 0) — the host checks the ptr.
#[no_mangle]
pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 {
    with_budget(budget, |a, b| {
        let mut out: Vec<u8> = Vec::new();
        if a.dump_emit(h, &mut out, b).is_err() {
            return pack(u32::MAX, 0);
        }
        let len = out.len() as u32;
        let ptr = out.as_ptr() as u32;
        std::mem::forget(out);
        pack(ptr, len)
    })
}

// ── observations ────────────────────────────────────────────────────────────
/// WHNF classify: `0`=leaf `1`=stem `2`=fork; `u32::MAX` on exhaustion. Forces.
#[no_mangle]
pub extern "C" fn tc_classify(h: u32, budget: u32) -> u32 {
    with_budget(budget, |a, b| match a.whnf(h, b) {
        Ok(w) => match a.node(w) {
            Node::Leaf => 0,
            Node::Stem(_) => 1,
            Node::Fork(..) => 2,
            Node::Susp(..) => u32::MAX,
        },
        Err(_) => u32::MAX,
    })
}
/// Left child of a WHNF node (stem child / fork left). Assumes a preceding `tc_classify`
/// already forced `h`; if not, forces with a large budget.
#[no_mangle]
pub extern "C" fn tc_child0(h: u32) -> u32 {
    with(|a| match whnf_node(a, h) {
        Node::Stem(c) => c,
        Node::Fork(l, _) => l,
        _ => LEAF_ID,
    })
}
/// Right child of a WHNF fork (else leaf).
#[no_mangle]
pub extern "C" fn tc_child1(h: u32) -> u32 {
    with(|a| match whnf_node(a, h) {
        Node::Fork(_, r) => r,
        _ => LEAF_ID,
    })
}
/// NF equality: `0`=false `1`=true `2`=budget exhausted (host checks `> 1`).
#[no_mangle]
pub extern "C" fn tc_equal(a_h: u32, b_h: u32, budget: u32) -> u32 {
    with_budget(budget, |a, b| match a.equal(a_h, b_h, b) {
        Ok(true) => 1,
        Ok(false) => 0,
        Err(_) => 2,
    })
}
/// Total interactions consumed this session (the backend-declared budget unit).
#[no_mangle]
pub extern "C" fn tc_interactions() -> u64 {
    with(|a| a.interactions)
}
