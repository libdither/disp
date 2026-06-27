//! The session arena: hash-consed `Node`s + the term algebra (`leaf`/`stem`/`fork`/
//! `susp`). A WASM instance owns exactly one `Arena`; `dispose()` drops it wholesale.
//!
//! With hash-consing the formal net collapses to a tree reducer: producers
//! (`Leaf`/`Stem`/`Fork`/`Susp`=`P`) are interned nodes; the consumers (`A`/`T₁`/`T₂`/
//! `δ`/`ε`) are the control flow of the reducers (see `reduce`), not stored agents.
//! `δˢ` (structural copy) = sharing a hash-cons reference (free); `δⁿ` at-most-once =
//! the `forced` memo cell on a shared `Susp`. (tc-net.typ §Agents / §Implementation Note.)

use hashbrown::HashTable;
use rustc_hash::{FxHashMap, FxHasher};
use crate::memo::Memo;
use std::hash::{Hash, Hasher};

/// A producer node (tc-net.typ §Agents). `Leaf`/`Stem`/`Fork` are constructors;
/// `Susp(f,a)` is the suspended application `P(f,a)` — the call-by-need parking point.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Node {
    Leaf,
    Stem(u32),
    Fork(u32, u32),
    Susp(u32, u32),
}

/// LEAF is handle 1, not 0: handle 0 is a reserved null sentinel so that NO valid
/// handle is JS-falsy. The host elaborator (`src/compile.ts`) tests handles with
/// truthiness (`entry?.tree ? …`); a `0` LEAF handle would read as "absent" → spurious
/// "unresolved free variable". Reserving 0 makes every real handle ≥ 1 (truthy).
pub(crate) const LEAF_ID: u32 = 1;

/// Budget exhaustion, threaded as `Err` through the reducers so the FFI boundary can
/// return the `u32::MAX` sentinel instead of trapping.
pub(crate) struct Exhausted;
pub(crate) type R = Result<u32, Exhausted>;

/// FxHash of a node, for the intern table.
#[inline]
fn node_hash(n: Node) -> u64 {
    let mut h = FxHasher::default();
    n.hash(&mut h);
    h.finish()
}

/// One session's arena. Handles are indices into `nodes`; the intern table makes
/// structurally-identical nodes share one id (so `δˢ` copy is reference sharing and
/// identical `Susp`s share one `forced` memo cell).
pub(crate) struct Arena {
    nodes: Vec<Node>,
    /// Hash-cons table: a SwissTable storing node IDS (not Nodes), hashed and compared
    /// THROUGH `nodes`, so each interned node is stored once (in `nodes`), not twice — a
    /// `HashTable<Node>` would duplicate every Node as its key (~half the arena's
    /// overhead on a heavy file). hashbrown owns the probing/growth/control bytes.
    intern: HashTable<u32>,
    /// `Susp` WHNF memo (`δⁿ` at-most-once): susp id → its forced WHNF. SPARSE — only
    /// forced susps appear, so eager mode (which never forces) keeps it empty and `mk`
    /// does zero per-node bookkeeping. No size cap (unlike `memo`) — the production
    /// Session path is eager so it stays empty; a long-lived *lazy* session bounds it
    /// via `tc_clear_caches`/`dispose`.
    pub(crate) forced: FxHashMap<u32, u32>,
    /// Eager apply memo (M0): `apply(f,a)` is a pure function of `(f,a)`, and
    /// hash-consing makes `(f,a)` a complete key. Backend-abstracted (see `memo.rs`):
    /// default FxHashMap+clear, or `memo-quickcache`/`memo-clock` bounded self-evicting.
    pub(crate) memo: Memo,
    /// Reduction counter = fork-dispatches (the budget unit; eager-`steps`-equivalent,
    /// reported via stats). Leaf/stem builds and the tree_eq fast-path are O(1) and
    /// uncounted, matching src/core/tree.ts.
    pub(crate) interactions: u64,
    /// The `tree_eq` definition's handle, registered via `recognizeNative` (0 = unset).
    /// When set, `apply(tree_eq, a) b` is intercepted as the O(1) hash-cons structural
    /// compare (the two-stage susp scheme), so conversion checking is cheap — without
    /// it, in-language `tree_eq` makes kernel verification blow the host's apply budget.
    pub(crate) tree_eq_id: u32,
    /// Scott `TT`/`FF` (the tree_eq fast-path results), built once at session init.
    pub(crate) tt: u32,
    pub(crate) ff: u32,
}

impl Arena {
    pub(crate) fn new() -> Self {
        let mut a = Arena {
            nodes: Vec::with_capacity(1 << 16),
            intern: HashTable::with_capacity(1 << 16),
            forced: FxHashMap::default(),
            memo: Memo::new(),
            interactions: 0,
            tree_eq_id: 0,
            tt: 0,
            ff: 0,
        };
        // index 0: reserved null sentinel (never interned, never returned) so no valid
        // handle is JS-falsy — see LEAF_ID.
        a.nodes.push(Node::Leaf);
        let id = a.mk(Node::Leaf); // real LEAF interned at index 1
        debug_assert_eq!(id, LEAF_ID);
        // Scott TT/FF — the exact trees the prelude's TT/FF compile to. Mirror
        // src/core/tree.ts:595-596: TT = K K = fork(L, K); FF = K (K I) = fork(L, K I).
        let l = LEAF_ID;
        let k = a.stem(l); // K = △△
        a.tt = a.fork(l, k); // TT = fork(L, K) = K K
        let ll = a.fork(l, l);
        let i = a.fork(ll, l); // I = fork(fork(L,L), L)
        let ki = a.fork(l, i); // K I = fork(L, I)
        a.ff = a.fork(l, ki); // FF = fork(L, K I) = K (K I)
        a
    }

    /// Intern a node (hash-cons): structurally-identical nodes share one id. The hash is
    /// computed once and reused for find + insert. The table stores only IDS; the eq and
    /// rehash closures look the node up THROUGH `nodes` (the id-only memory win). The
    /// field destructure gives `intern` and `nodes` disjoint borrows so the closures can
    /// read `nodes` while `intern` is borrowed.
    #[inline]
    fn mk(&mut self, n: Node) -> u32 {
        let h = node_hash(n);
        let Arena { nodes, intern, .. } = self;
        if let Some(&id) = intern.find(h, |&id| nodes[id as usize] == n) {
            return id;
        }
        let id = nodes.len() as u32;
        nodes.push(n);
        intern.insert_unique(h, id, |&id| node_hash(nodes[id as usize]));
        id
    }

    // ── term algebra (Session.leaf / stem / fork + the lazy susp) ──
    #[inline]
    pub(crate) fn node(&self, id: u32) -> Node {
        self.nodes[id as usize]
    }
    /// Current node high-water (interned count) — the watermark backend's baseline source.
    #[inline]
    pub(crate) fn node_count(&self) -> u32 {
        self.nodes.len() as u32
    }
    #[inline]
    pub(crate) fn stem(&mut self, c: u32) -> u32 {
        self.mk(Node::Stem(c))
    }
    #[inline]
    pub(crate) fn fork(&mut self, l: u32, r: u32) -> u32 {
        self.mk(Node::Fork(l, r))
    }
    #[inline]
    pub(crate) fn susp(&mut self, f: u32, a: u32) -> u32 {
        self.mk(Node::Susp(f, a))
    }
}
