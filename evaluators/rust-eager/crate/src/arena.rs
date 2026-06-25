//! The session arena: hash-consed `Node`s + the term algebra (`leaf`/`stem`/`fork`/
//! `susp`). A WASM instance owns exactly one `Arena`; `dispose()` drops it wholesale.
//!
//! With hash-consing the formal net collapses to a tree reducer: producers
//! (`Leaf`/`Stem`/`Fork`/`Susp`=`P`) are interned nodes; the consumers (`A`/`T₁`/`T₂`/
//! `δ`/`ε`) are the control flow of the reducers (see `reduce`), not stored agents.
//! `δˢ` (structural copy) = sharing a hash-cons reference (free); `δⁿ` at-most-once =
//! the `forced` memo cell on a shared `Susp`. (tc-net.typ §Agents / §Implementation Note.)

use crate::hash::{Map, FX_K};

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

/// Hash a node directly (for the id-only hash-cons table). `tag + 1` so Leaf isn't 0.
#[inline]
fn node_hash(n: Node) -> u64 {
    let (tag, a, b) = match n {
        Node::Leaf => (0u64, 0u64, 0u64),
        Node::Stem(c) => (1, c as u64, 0),
        Node::Fork(l, r) => (2, l as u64, r as u64),
        Node::Susp(f, x) => (3, f as u64, x as u64),
    };
    let mut h = (tag + 1).wrapping_mul(FX_K);
    h = (h.rotate_left(5) ^ a).wrapping_mul(FX_K);
    h = (h.rotate_left(5) ^ b).wrapping_mul(FX_K);
    h
}

/// 1-byte hash tag for a slot, never 0 (0 is reserved for "empty slot").
#[inline]
fn node_tag(h: u64) -> u8 {
    let t = (h >> 56) as u8;
    if t == 0 {
        1
    } else {
        t
    }
}

/// One session's arena. Handles are indices into `nodes`; the intern table makes
/// structurally-identical nodes share one id (so `δˢ` copy is reference sharing and
/// identical `Susp`s share one `forced` memo cell).
pub(crate) struct Arena {
    nodes: Vec<Node>,
    /// Hash-cons table: an open-addressing set of node IDS (not Nodes), hashed and
    /// compared THROUGH `nodes`, so each interned node is stored once (in `nodes`), not
    /// twice — a `Map<Node,u32>` duplicated every Node as its key (~half the arena's
    /// overhead on a heavy file). Power-of-two size; stored value 0 = empty slot (id 0
    /// is the reserved null, so never a real entry); grows at 7/8 load.
    intern_table: Vec<u32>,
    /// Parallel 1-byte hash tags (0 = empty slot, else the node's hash tag). A probe
    /// checks this contiguous array first and only chases `nodes[id]` on a tag match
    /// (~1/256 false positives), so most probes never touch the node arena.
    intern_tags: Vec<u8>,
    intern_count: usize,
    /// `Susp` WHNF memo (`δⁿ` at-most-once): susp id → its forced WHNF. SPARSE — only
    /// forced susps appear, so eager mode (which never forces) keeps it empty and `mk`
    /// does zero per-node bookkeeping. No size cap (unlike `memo`) — the production
    /// Session path is eager so it stays empty; a long-lived *lazy* session bounds it
    /// via `tc_clear_caches`/`dispose`.
    pub(crate) forced: Map<u32, u32>,
    /// Eager apply memo (M0): `apply(f,a)` is a pure function of `(f,a)`, and
    /// hash-consing makes `(f,a)` a complete key (the eager backend's `applyMemo`).
    pub(crate) memo: Map<(u32, u32), u32>,
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
    /// Cap on `memo` entries; over it the eager apply memo is cleared (pure cache —
    /// correctness-preserving, just re-reduces). Default unbounded; lower via
    /// `tc_set_memo_limit` to trade speed for a smaller footprint on long sessions.
    pub(crate) memo_limit: usize,
}

impl Arena {
    pub(crate) fn new() -> Self {
        let mut a = Arena {
            nodes: Vec::with_capacity(1 << 16),
            intern_table: vec![0u32; 1 << 16],
            intern_tags: vec![0u8; 1 << 16],
            intern_count: 0,
            forced: Map::default(),
            memo: Map::default(),
            interactions: 0,
            tree_eq_id: 0,
            tt: 0,
            ff: 0,
            memo_limit: usize::MAX,
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

    /// Intern a node (hash-cons): structurally-identical nodes share one id. The hash
    /// is computed ONCE here and threaded into find + insert, so a new node hashes once,
    /// not twice (the hottest path in the evaluator).
    #[inline]
    fn mk(&mut self, n: Node) -> u32 {
        let h = node_hash(n);
        if let Some(id) = self.intern_find(n, h) {
            return id;
        }
        let id = self.nodes.len() as u32;
        self.nodes.push(n);
        self.intern_insert(id, h);
        id
    }

    /// Find `n`'s id given its precomputed hash, or `None`. Linear-probe; the tag check
    /// skips the `nodes[id]` compare on mismatch (exact compare on tag match).
    #[inline]
    fn intern_find(&self, n: Node, h: u64) -> Option<u32> {
        let mask = self.intern_table.len() - 1;
        let tag = node_tag(h);
        let mut i = (h as usize) & mask;
        loop {
            let t = self.intern_tags[i];
            if t == 0 {
                return None; // empty slot ⇒ not present
            }
            if t == tag {
                let id = self.intern_table[i];
                if self.nodes[id as usize] == n {
                    return Some(id);
                }
            }
            i = (i + 1) & mask;
        }
    }

    /// Insert an id (its `nodes[id]` not yet in the table) with precomputed hash `h`,
    /// growing at 7/8 load. The rehash recomputes hashes (amortized rare); the common
    /// path reuses `h` from `mk`.
    #[inline]
    fn intern_insert(&mut self, id: u32, h: u64) {
        if (self.intern_count + 1) * 8 >= self.intern_table.len() * 7 {
            let new_size = self.intern_table.len() * 2;
            let old = std::mem::replace(&mut self.intern_table, vec![0u32; new_size]);
            self.intern_tags = vec![0u8; new_size];
            for old_id in old {
                if old_id != 0 {
                    let oh = node_hash(self.nodes[old_id as usize]);
                    self.intern_put(old_id, oh);
                }
            }
        }
        self.intern_put(id, h);
        self.intern_count += 1;
    }

    /// Place an id at its probe slot (assumes spare capacity) given its hash, setting
    /// its tag.
    #[inline]
    fn intern_put(&mut self, id: u32, h: u64) {
        let mask = self.intern_table.len() - 1;
        let mut i = (h as usize) & mask;
        while self.intern_tags[i] != 0 {
            i = (i + 1) & mask;
        }
        self.intern_tags[i] = node_tag(h);
        self.intern_table[i] = id;
    }

    // ── term algebra (Session.leaf / stem / fork + the lazy susp) ──
    #[inline]
    pub(crate) fn node(&self, id: u32) -> Node {
        self.nodes[id as usize]
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
