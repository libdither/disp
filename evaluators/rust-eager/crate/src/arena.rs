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
use crate::snapshot::{self, FrozenBase};
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
    /// Bool `true`/`false` (the tree_eq fast-path results), built once at session
    /// init: raw shapes `△` / `△ △` per TYPE_THEORY §2.7 (Scott until 2026-07-07).
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
    /// Frozen base tier from a loaded snapshot (persistent reduction cache): ids
    /// `0..base_len` read from `base`; the live `nodes`/`intern`/`memo` are the overlay
    /// above it. The base behaves like an outermost never-closed scope — `end_scope`
    /// never touches it (scope watermarks are always ≥ `base_len`), so its intern/memo
    /// entries are permanently valid. `base_len` is 0 with no snapshot (identity offsets).
    base: Option<FrozenBase>,
    base_len: u32,
    /// Frozen-memo hits this session (telemetry: the snapshot's realized value).
    pub(crate) frozen_hits: u64,
}

/// `save_snapshot` outcome: written, or skipped because the run added nothing durable.
pub(crate) enum SaveOutcome {
    Saved(u32, u32),
    Unchanged,
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
            base: None,
            base_len: 0,
            frozen_hits: 0,
        };
        // index 0: reserved null sentinel (never interned, never returned) so no valid
        // handle is JS-falsy — see LEAF_ID.
        a.nodes.push(pack(Node::Leaf));
        let id = a.mk(Node::Leaf); // real LEAF interned at index 1
        debug_assert_eq!(id, LEAF_ID);
        // Bool true/false — the exact trees the prelude's `true := t` and
        // `false := t t` compile to (raw shapes per TYPE_THEORY §2.7). Mirror
        // src/core/tree.ts TREE_TRUE/TREE_FALSE: true = LEAF, false = stem(LEAF).
        // (Scott K K / K (K I) until 2026-07-07 — the §2.7 polarity migration.)
        let l = LEAF_ID;
        a.tt = l; // true = △
        a.ff = a.stem(l); // false = △ △ (K's tree)
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
        // The frozen base tier resolves first: a structurally-identical node keeps its
        // snapshot id, which is what makes persisted memo keys re-arise as hits.
        if let Some(b) = &self.base {
            if let Some(id) = b.intern_find(w, h) {
                return id;
            }
        }
        let Arena { nodes, intern, free_list, base, base_len, .. } = self;
        let bl = *base_len;
        let word = |id: u32| -> u64 {
            if id < bl { base.as_ref().unwrap().nodes[id as usize] } else { nodes[(id - bl) as usize] }
        };
        if let Some(&id) = intern.find(h, |&id| word(id) == w) {
            return id;
        }
        // Reuse a slot freed by `end_scope` before extending the arena (so a scoped
        // workload's peak — not its cumulative allocation — bounds `nodes.len()`).
        let id = if let Some(reused) = free_list.pop() {
            nodes[(reused - bl) as usize] = w;
            reused
        } else {
            let id = bl + nodes.len() as u32;
            debug_assert!(id < (1u32 << 31), "node id exceeds the 31-bit packing limit");
            nodes.push(w);
            id
        };
        let Arena { nodes, intern, base, base_len, .. } = self;
        let bl = *base_len;
        intern.insert_unique(h, id, |&id| {
            word_hash(if id < bl { base.as_ref().unwrap().nodes[id as usize] } else { nodes[(id - bl) as usize] })
        });
        id
    }

    // ── term algebra (Session.leaf / stem / fork + the lazy susp) ──
    #[inline]
    pub(crate) fn node(&self, id: u32) -> Node {
        unpack(if id < self.base_len {
            self.base.as_ref().unwrap().nodes[id as usize]
        } else {
            self.nodes[(id - self.base_len) as usize]
        })
    }
    /// Current node high-water (interned count) — the watermark backend's baseline source.
    #[inline]
    pub(crate) fn node_count(&self) -> u32 {
        self.base_len + self.nodes.len() as u32
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
        self.scopes.push(self.node_count());
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
        let top = self.node_count();
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
            // In-scope ids only (>= base >= base_len), so plain overlay reads.
            match unpack(self.nodes[(id - self.base_len) as usize]) {
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
        self.nodes.truncate((new_top - self.base_len) as usize);
        // A node id survives iff it is below base (permanent) or a marked survivor.
        let live = |id: u32| id < base || (id < new_top && marked[(id - base) as usize]);
        self.intern.retain(|id| live(*id));
        self.memo.retain(|id| live(id)); // drop only entries touching freed nodes
        self.forced.retain(|&k, v| live(k) && live(*v));
    }

    // ── persistent reduction cache (snapshot load/save) ──
    /// Tiered memo lookup: the frozen tier's facts, then the live map.
    #[inline]
    pub(crate) fn memo_lookup(&mut self, f: u32, x: u32) -> Option<u32> {
        if let Some(b) = &self.base {
            if let Some(r) = b.memo_get(f, x) {
                self.frozen_hits += 1;
                return Some(r);
            }
        }
        self.memo.get(f, x)
    }

    /// Adopt a snapshot as the frozen base tier. Only legal on a pristine session
    /// (bootstrap nodes only, no scopes/holes); the snapshot's bootstrap prefix is
    /// verified so LEAF/tt/ff ids line up by construction. Returns (nodes, memo entries).
    pub(crate) fn load_snapshot(&mut self, path: &str, stamp: &[u8]) -> Result<(u32, u32), String> {
        if self.base.is_some() || !self.scopes.is_empty() || !self.free_list.is_empty() {
            return Err("session is not pristine".into());
        }
        if self.nodes.len() != 3 || self.tree_eq_id != 0 {
            return Err("session already has interned nodes".into());
        }
        let b = snapshot::load(path, stamp)?;
        if b.nodes.len() < 3
            || b.nodes[LEAF_ID as usize] != pack(Node::Leaf)
            || b.nodes[self.ff as usize] != pack(Node::Stem(LEAF_ID))
        {
            return Err("snapshot bootstrap prefix mismatch".into());
        }
        let memo_n = b.memo_len();
        self.base_len = b.node_len();
        self.base = Some(b);
        self.nodes.clear();
        self.intern.clear();
        Ok((self.base_len, memo_n))
    }

    /// Persist the arena (frozen base + overlay), COMPACTED: live ids (the interned
    /// set — hash-consing guarantees an interned node's children are interned) remap
    /// densely, dropping every hole, and only memo entries whose recomputation cost
    /// is ≥ `min_cost` fork-dispatches persist (cheap facts are cheaper to recompute
    /// than to store/load; the run's live memo keeps everything regardless). Callers
    /// snapshot at a quiet point (post-endScope). Skips writing when the run added
    /// nothing durable — the converged fixed point.
    pub(crate) fn save_snapshot(&mut self, path: &str, stamp: &[u8], min_cost: u32) -> Result<SaveOutcome, String> {
        let overlay_live = self.nodes.len() - self.free_list.len();
        let new_facts = self.memo.iter().filter(|&(.., c)| c >= min_cost).count();
        if self.base.is_some() && overlay_live == 0 && new_facts == 0 {
            return Ok(SaveOutcome::Unchanged);
        }
        // Live ids, ascending (keeps the bootstrap prefix 0/1/2 fixed): the reserved
        // sentinel + frozen ∪ live intern. Frozen and overlay id ranges are disjoint.
        let mut interned: Vec<u32> = self.base.as_ref().map(|b| b.intern_iter().collect()).unwrap_or_default();
        interned.extend(self.intern.iter().copied());
        interned.sort_unstable();
        let mut live: Vec<u32> = Vec::with_capacity(interned.len() + 1);
        live.push(0);
        live.extend_from_slice(&interned);
        let mut remap: Vec<u32> = vec![u32::MAX; self.node_count() as usize];
        for (new_id, &old) in live.iter().enumerate() {
            remap[old as usize] = new_id as u32;
        }
        let words: Vec<u64> = live
            .iter()
            .map(|&old| {
                pack(match self.node(old) {
                    Node::Leaf => Node::Leaf,
                    Node::Stem(c) => Node::Stem(remap[c as usize]),
                    Node::Fork(l, r) => Node::Fork(remap[l as usize], remap[r as usize]),
                    Node::Susp(f, a) => Node::Susp(remap[f as usize], remap[a as usize]),
                })
            })
            .collect();
        let frozen_memo: Vec<(u32, u32, u32, u32)> =
            self.base.as_ref().map(|b| b.memo_iter().collect()).unwrap_or_default();
        let live_memo: Vec<(u32, u32, u32, u32)> = self.memo.iter().collect();
        let m = |id: u32| remap[id as usize];
        snapshot::save(
            path,
            stamp,
            words,
            interned.iter().map(|&id| m(id)),
            frozen_memo
                .into_iter()
                .chain(live_memo)
                .filter(|&(f, x, r, c)| {
                    c >= min_cost && m(f) != u32::MAX && m(x) != u32::MAX && m(r) != u32::MAX
                })
                .map(|(f, x, r, c)| (m(f), m(x), m(r), c)),
        )
        .map(|(n, e)| SaveOutcome::Saved(n, e))
    }
}
