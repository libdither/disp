//! The native N-API Session backend: a `#[napi]` class `EagerSession` mirroring the wasm
//! `tc_*` C-ABI (see `ffi.rs`), but as an in-process Node addon. The point is the ceiling:
//! the wasm Session is capped at the wasm32 4 GiB linear-memory address space (the wall the
//! auto-verify check hits today); native runs in host RAM, with -O3 and no wasm sandbox tax
//! on the reduce hot loop.
//!
//! Why a class, not the wasm `thread_local` arena: a `.node` addon is loaded once per
//! process, so a global arena would alias every Session. The class owns one `Arena` per JS
//! instance; napi's finalizer drops it when the JS object is collected — `dispose()` just
//! releases the reference. That matches the wasm one-instance-per-session model (drop the
//! whole arena; no per-handle free).
//!
//! napi-derive lowercases method names to camelCase (`load_ternary` → `loadTernary`), which
//! is what `src/eval/rust-eager-native.ts` calls. Handles stay `u32` (plain JS numbers);
//! the eager-apply exhaustion sentinel is `u32::MAX` = JS `4294967295` (NOT the wasm path's
//! signed `-1`, since napi returns `u32` unsigned).

use crate::arena::{Arena, Node, LEAF_ID};
use napi_derive::napi;

/// One session = one owned hash-consed arena (the per-instance analogue of the wasm
/// `thread_local! ARENA`).
#[napi]
pub struct EagerSession {
    arena: Arena,
}

#[napi]
impl EagerSession {
    #[napi(constructor)]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        EagerSession { arena: Arena::new() }
    }

    // ── term algebra (Session.leaf / stem / fork) ──
    #[napi]
    pub fn leaf(&self) -> u32 {
        LEAF_ID
    }
    #[napi]
    pub fn stem(&mut self, child: u32) -> u32 {
        self.arena.stem(child)
    }
    #[napi]
    pub fn fork(&mut self, left: u32, right: u32) -> u32 {
        self.arena.fork(left, right)
    }

    // ── computation (Session.apply) ──
    /// EAGER apply — fully normalize `apply(f, x)`. Returns `u32::MAX` (JS 4294967295) on
    /// budget exhaustion (the host treats it as the sentinel, like the wasm `-1`).
    #[napi]
    pub fn apply(&mut self, f: u32, x: u32, budget: u32) -> u32 {
        let mut b = budget as i64;
        self.arena.reduce(f, x, &mut b).unwrap_or(u32::MAX)
    }
    /// M1 lazy apply: build `Susp(f, x)` in O(1), forced on demand by the next observation.
    #[napi]
    pub fn apply_lazy(&mut self, f: u32, x: u32) -> u32 {
        self.arena.susp(f, x)
    }
    /// Register the `tree_eq` definition's handle (Session.recognizeNative) for the O(1)
    /// hash-cons fast-path.
    #[napi]
    pub fn recognize_tree_eq(&mut self, handle: u32) {
        self.arena.tree_eq_id = handle;
    }

    // ── memory knobs ──
    /// Cap the eager apply memo at `n` entries (0 = unbounded); over the cap it sheds.
    #[napi]
    pub fn set_memo_limit(&mut self, n: u32) {
        self.arena.memo.set_limit(n as usize);
    }
    /// Drop the re-derivable caches (apply memo + susp WHNF memo); live trees untouched.
    #[napi]
    pub fn clear_caches(&mut self) {
        let node_hw = self.arena.node_count();
        self.arena.memo.clear(node_hw);
        self.arena.forced.clear();
        self.arena.forced.shrink_to_fit();
    }

    // ── interchange (Session.loadTernary / dumpTernary) — native strings, no ptr/len
    //    marshalling: napi copies the JS string in and the result String out directly. ──
    #[napi]
    pub fn load_ternary(&mut self, s: String) -> u32 {
        let mut i = 0usize;
        self.arena.parse(s.as_bytes(), &mut i)
    }
    /// Force full NF, serialize to ternary. `null` (None) on budget exhaustion.
    #[napi]
    pub fn dump_ternary(&mut self, h: u32, budget: u32) -> Option<String> {
        let mut b = budget as i64;
        let mut out: Vec<u8> = Vec::new();
        if self.arena.dump_emit(h, &mut out, &mut b).is_err() {
            return None;
        }
        // ternary output is ASCII '0'/'1'/'2' — always valid UTF-8.
        Some(String::from_utf8(out).unwrap_or_default())
    }

    // ── observations ──
    /// WHNF classify: `0`=leaf `1`=stem `2`=fork; `-1` on exhaustion (or a stuck susp).
    #[napi]
    pub fn classify(&mut self, h: u32, budget: u32) -> i32 {
        let mut b = budget as i64;
        match self.arena.whnf(h, &mut b) {
            Ok(w) => match self.arena.node(w) {
                Node::Leaf => 0,
                Node::Stem(_) => 1,
                Node::Fork(..) => 2,
                Node::Susp(..) => -1,
            },
            Err(_) => -1,
        }
    }
    /// Left child of a WHNF node (stem child / fork left); assumes a preceding `classify`.
    #[napi]
    pub fn child0(&mut self, h: u32) -> u32 {
        match self.whnf_node(h) {
            Node::Stem(c) => c,
            Node::Fork(l, _) => l,
            _ => LEAF_ID,
        }
    }
    /// Right child of a WHNF fork (else leaf).
    #[napi]
    pub fn child1(&mut self, h: u32) -> u32 {
        match self.whnf_node(h) {
            Node::Fork(_, r) => r,
            _ => LEAF_ID,
        }
    }
    /// NF equality: `0`=false `1`=true `2`=budget exhausted (host checks `> 1`).
    #[napi]
    pub fn equal(&mut self, a: u32, b: u32, budget: u32) -> u32 {
        let mut bud = budget as i64;
        match self.arena.equal(a, b, &mut bud) {
            Ok(true) => 1,
            Ok(false) => 0,
            Err(_) => 2,
        }
    }
    /// Total interactions this session (the backend budget unit). `f64` ⇒ a plain JS number.
    #[napi]
    pub fn interactions(&self) -> f64 {
        self.arena.interactions as f64
    }
}

impl EagerSession {
    /// WHNF `h`, then read its node (the host calls `classify` first, so usually a cache hit).
    fn whnf_node(&mut self, h: u32) -> Node {
        let mut b = i64::MAX;
        let w = self.arena.whnf(h, &mut b).unwrap_or(LEAF_ID);
        self.arena.node(w)
    }
}
