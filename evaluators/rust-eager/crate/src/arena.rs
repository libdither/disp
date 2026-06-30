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

// Packed node layout: each node is ONE u64, not a 12-byte enum — `[tag:2][child0:31]
// [child1:31]`. Tree calculus has only 0/1/2-child nodes, so 31-bit child ids (2.1B nodes,
// far above any real workload) leave room for a 2-bit tag in one word: ~33% less memory and
// denser cache lines (8/line vs 5.3). `node()` unpacks to the `Node` enum so every reader
// (reduce/codec/end_scope) is unchanged; only the storage + `mk` touch the raw word. (Same
// kind-in-pointer trick rust-ic-net uses — port.rs.)
const TAG_STEM: u64 = 1;
const TAG_FORK: u64 = 2;
const TAG_SUSP: u64 = 3;
const CHILD_MASK: u64 = (1 << 31) - 1;

#[inline]
fn pack(n: Node) -> u64 {
    match n {
        Node::Leaf => 0, // tag 0, both children 0
        Node::Stem(c) => TAG_STEM | ((c as u64) << 2),
        Node::Fork(l, r) => TAG_FORK | ((l as u64) << 2) | ((r as u64) << 33),
        Node::Susp(f, a) => TAG_SUSP | ((f as u64) << 2) | ((a as u64) << 33),
    }
}
#[inline]
fn unpack(w: u64) -> Node {
    match w & 0b11 {
        0 => Node::Leaf,
        1 => Node::Stem(((w >> 2) & CHILD_MASK) as u32),
        2 => Node::Fork(((w >> 2) & CHILD_MASK) as u32, (w >> 33) as u32),
        _ => Node::Susp(((w >> 2) & CHILD_MASK) as u32, (w >> 33) as u32),
    }
}
/// FxHash of a packed node, for the intern table.
#[inline]
fn word_hash(w: u64) -> u64 {
    let mut h = FxHasher::default();
    w.hash(&mut h);
    h.finish()
}

/// One session's arena. Handles are indices into `nodes`; the intern table makes
/// structurally-identical nodes share one id (so `δˢ` copy is reference sharing and
/// identical `Susp`s share one `forced` memo cell).
pub(crate) struct Arena {
    /// Packed nodes — one u64 each (`[tag:2][child0:31][child1:31]`, see `pack`/`unpack`).
    nodes: Vec<u64>,
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
    /// Scoped-reclamation watermark stack (`begin_scope`/`end_scope`). Each entry is the node
    /// high-water at a scope's start; `end_scope(keep)` reclaims everything allocated in the
    /// scope that is NOT reachable from `keep`. Empty = no active scope.
    scopes: Vec<u32>,
    /// Slots freed by `end_scope` (interior holes below the new high-water), reused by `mk`
    /// before bumping. Hash-consing stays consistent: a freed slot's intern entry is removed,
    /// and a freed id is never live (it survived no `keep`), so reuse can't alias a live node.
    free_list: Vec<u32>,
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
            scopes: Vec::new(),
            free_list: Vec::new(),
        };
        // index 0: reserved null sentinel (never interned, never returned) so no valid
        // handle is JS-falsy — see LEAF_ID.
        a.nodes.push(pack(Node::Leaf));
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
        let w = pack(n);
        let h = word_hash(w);
        let Arena { nodes, intern, free_list, .. } = self;
        if let Some(&id) = intern.find(h, |&id| nodes[id as usize] == w) {
            return id;
        }
        // Reuse a slot freed by `end_scope` before extending the arena (so a scoped
        // workload's peak — not its cumulative allocation — bounds `nodes.len()`).
        let id = if let Some(reused) = free_list.pop() {
            nodes[reused as usize] = w;
            reused
        } else {
            let id = nodes.len() as u32;
            debug_assert!(id < (1u32 << 31), "node id exceeds the 31-bit packing limit");
            nodes.push(w);
            id
        };
        intern.insert_unique(h, id, |&id| word_hash(nodes[id as usize]));
        id
    }

    // ── term algebra (Session.leaf / stem / fork + the lazy susp) ──
    #[inline]
    pub(crate) fn node(&self, id: u32) -> Node {
        unpack(self.nodes[id as usize])
    }
    /// Current node high-water (interned count) — the watermark backend's baseline source.
    #[inline]
    pub(crate) fn node_count(&self) -> u32 {
        self.nodes.len() as u32
    }
    /// Reusable holes freed by `end_scope` (live nodes = `node_count - free_count`). A
    /// non-moving collector can't lower `node_count` below a surviving top node, but these
    /// holes absorb future allocations, so cumulative growth is bounded by the live peak.
    #[inline]
    pub(crate) fn free_count(&self) -> u32 {
        self.free_list.len() as u32
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

    // ── scoped reclamation (Session.beginScope / endScope) ──
    /// Open a reclamation scope: remember the current node high-water. Everything allocated
    /// after this is reclaimable by the matching `end_scope` unless reachable from its `keep`.
    pub(crate) fn begin_scope(&mut self) {
        self.scopes.push(self.nodes.len() as u32);
    }

    /// Close the innermost scope: reclaim every node allocated in it that is NOT reachable
    /// from `keep` (a mark-sweep over `[base, top)`). Survivors keep their ids (non-moving);
    /// interior dead slots go to the free list, the trailing dead run is truncated, and the
    /// intern/memo/forced caches drop entries touching freed nodes (kept entries stay valid).
    /// Nodes below `base` (older scopes / the permanent base) are never touched — and by
    /// construction-order immutability they can't reference anything in `[base, top)`, so they
    /// need no marking. A no-op if `keep` is complete-but-empty: the scope fully rolls back.
    pub(crate) fn end_scope(&mut self, keep: &[u32]) {
        let base = match self.scopes.pop() {
            Some(b) => b,
            None => return,
        };
        let top = self.nodes.len() as u32;
        if top <= base {
            return; // nothing allocated in the scope
        }
        let span = (top - base) as usize;
        let mut marked = vec![false; span];
        let mut work: Vec<u32> = Vec::new();
        // Mark a node IN-SCOPE (>= base); below-base nodes are permanent and stop the trace.
        macro_rules! visit {
            ($id:expr) => {{
                let id = $id;
                if id >= base {
                    let i = (id - base) as usize;
                    if !marked[i] {
                        marked[i] = true;
                        work.push(id);
                    }
                }
            }};
        }
        for &k in keep {
            visit!(k);
        }
        // The arena's own permanent roots (in case any landed in-scope).
        visit!(self.tt);
        visit!(self.ff);
        if self.tree_eq_id != 0 {
            visit!(self.tree_eq_id);
        }
        while let Some(id) = work.pop() {
            match unpack(self.nodes[id as usize]) {
                Node::Leaf => {}
                Node::Stem(c) => visit!(c),
                Node::Fork(l, r) => {
                    visit!(l);
                    visit!(r);
                }
                Node::Susp(f, a) => {
                    visit!(f);
                    visit!(a);
                }
            }
        }
        // new_top = one past the highest surviving id (the trailing dead run is truncated).
        let mut new_top = base;
        for i in (0..span).rev() {
            if marked[i] {
                new_top = base + i as u32 + 1;
                break;
            }
        }
        // Interior dead slots (< new_top, unmarked) become reusable holes.
        for id in base..new_top {
            if !marked[(id - base) as usize] {
                self.free_list.push(id);
            }
        }
        self.nodes.truncate(new_top as usize);
        // A node id survives iff it is below base (permanent) or a marked survivor.
        let live = |id: u32| id < base || (id < new_top && marked[(id - base) as usize]);
        self.intern.retain(|id| live(*id));
        self.memo.retain(|id| live(id)); // drop only entries touching freed nodes
        self.forced.retain(|&k, v| live(k) && live(*v));
    }
}
