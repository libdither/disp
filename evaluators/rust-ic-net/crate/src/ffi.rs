//! The wasm Session C-ABI (`tc_*` exports). One `Net` + one `Worker` per WASM instance
//! (the host calls on a single thread = the sequential oracle). Numbers cross free; the
//! only buffer copy is one ternary string per `loadTernary`/`dumpTernary`. `apply` is LAZY
//! (builds `P(f,x)` in O(1)); the forcing observations (`dumpTernary`/`equal`) drain the
//! net. Budget exhaustion returns a sentinel, never traps (`panic = "abort"`).

use crate::net::{Ctx, Net, Worker};
use crate::port::*;
use std::cell::RefCell;
use std::sync::atomic::Ordering;

// One Net + one Worker per WASM instance (the host calls on a single thread). The Net is
// interior-mutable (atomics), so a plain `&` suffices; the Worker is RefCell (briefly
// borrowed per op — never nested, so no double-borrow).
thread_local! {
    static NET: Net = Net::new(NODE_CAP, VAR_CAP);
    static WK: RefCell<Worker> = RefCell::new(Worker::new());
}
#[inline]
fn with<T>(f: impl FnOnce(&mut Ctx) -> T) -> T {
    NET.with(|net| {
        WK.with(|wk| {
            let mut w = wk.borrow_mut();
            let mut ctx = net.ctx(&mut w);
            f(&mut ctx)
        })
    })
}
#[inline]
fn pack_ptr(ptr: u32, len: u32) -> u64 {
    ((ptr as u64) << 32) | (len as u64)
}

// ── term algebra (Session.leaf / stem / fork) ───────────────────────────────
#[no_mangle]
pub extern "C" fn tc_leaf() -> u32 {
    pack(L, 0)
}
#[no_mangle]
pub extern "C" fn tc_stem(child: u32) -> u32 {
    with(|c| c.stem(child))
}
#[no_mangle]
pub extern "C" fn tc_fork(left: u32, right: u32) -> u32 {
    with(|c| c.fork(left, right))
}
/// **LAZY** apply: build `P(f,x)` in O(1); reduction is deferred to dump/equal.
#[no_mangle]
pub extern "C" fn tc_apply(f: u32, x: u32, _budget: u32) -> u32 {
    with(|c| c.susp(f, x))
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
    with(|c| {
        let mut i = 0usize;
        c.parse(bytes, &mut i)
    })
}
/// Force full NF, serialize to ternary, return `(ptr<<32)|len`; `(u32::MAX<<32)` on exhaust.
#[no_mangle]
pub extern "C" fn tc_dump_ternary(h: u32, budget: u32) -> u64 {
    with(|c| {
        let mut b = budget as i64;
        match c.full_nf(h, &mut b) {
            None => pack_ptr(u32::MAX, 0),
            Some(nf) => {
                let mut out: Vec<u8> = Vec::new();
                c.emit(nf, &mut out);
                let len = out.len() as u32;
                let ptr = out.as_ptr() as u32;
                std::mem::forget(out);
                pack_ptr(ptr, len)
            }
        }
    })
}

// ── observations ────────────────────────────────────────────────────────────
/// NF equality: `0`=false `1`=true `2`=budget exhausted.
#[no_mangle]
pub extern "C" fn tc_equal(a: u32, b: u32, budget: u32) -> u32 {
    with(|c| {
        let mut bud = budget as i64;
        match c.equal(a, b, &mut bud) {
            Some(true) => 1,
            Some(false) => 0,
            None => 2,
        }
    })
}
/// Total interactions consumed this session.
#[no_mangle]
pub extern "C" fn tc_interactions() -> u64 {
    NET.with(|n| n.interactions.load(Ordering::Relaxed))
}
/// Peak node cells used (the bump high-water — materialized-net proof + footprint).
#[no_mangle]
pub extern "C" fn tc_nodes() -> u64 {
    NET.with(|n| n.node_top.load(Ordering::Relaxed) as u64)
}
/// δⁿ ⊗ P firings = how many duplicated suspensions were shared.
#[no_mangle]
pub extern "C" fn tc_dnp() -> u64 {
    NET.with(|n| n.dnp.load(Ordering::Relaxed))
}
