//! Port encoding + the agent-kind tag table (tc-net.typ §Agents). A port is one tagged
//! word: tag in the low 4 bits, value (node base index / var index) in the high 28 bits.
//! "Kind-in-pointer" (HVM2-style) — a redex `(Port, Port)` names both interacting agent
//! kinds with zero memory loads, so rule dispatch reads two 4-bit tags out of one `u64`.

pub(crate) const TAG_BITS: u32 = 4;
pub(crate) const TAG_MASK: u32 = 0xF;

pub(crate) const NUL: u32 = 0; // empty slot / null (pack(NUL,0) == 0); no real port equals 0
pub(crate) const L: u32 = 1; // leaf  (nullary producer)
pub(crate) const S: u32 = 2; // stem  (1 aux: x)
pub(crate) const F: u32 = 3; // fork  (2 aux: l, r)
pub(crate) const P: u32 = 4; // suspended application (2 aux: f, a)
pub(crate) const A: u32 = 5; // apply (2 aux: arg, res)
pub(crate) const T1: u32 = 6; // first-level dispatch (3 aux: b, c, res)
pub(crate) const T2: u32 = 7; // second-level dispatch (4 aux: w, x, b, res)
pub(crate) const DS: u32 = 8; // structural duplicator δˢ (2 aux: l, r) — copies syntax (Prop 5)
pub(crate) const EPS: u32 = 9; // eraser ε (nullary consumer)
pub(crate) const N: u32 = 10; // recursive normalizer (1 aux: res)
pub(crate) const VAR: u32 = 11; // a substitution-cell wire end (value = vars index)
pub(crate) const DN: u32 = 12; // need duplicator δⁿ (2 aux: l, r) — demand-before-copy (Thm 6)

/// Default fixed-arena sizes (cells). The bump allocator + free-on-consume keep the live
/// set tiny (benchmarks peak ~64K cells), so this is generous headroom; overflow traps.
pub(crate) const NODE_CAP: usize = 1 << 21; // 2M cells (8 MiB)
pub(crate) const VAR_CAP: usize = 1 << 21;

/// Per-worker bump REGION size (cells) — Stage 3 of the M2c scheduler rework. A worker
/// leases a contiguous region from the shared cursor with ONE atomic, then bumps locally —
/// turning the per-alloc `node_top`/`var_top.fetch_add` into one per ~1024 allocs AND
/// giving each worker its own cache lines (different workers no longer interleave cells on
/// a shared line). A region's unused tail (< a few cells at a forced re-lease, or a
/// low-traffic worker's whole region) is abandoned, so `node_top` reports cells *leased*,
/// slightly above cells *used*.
pub(crate) const NODE_REGION: u32 = 1 << 10;
pub(crate) const VAR_REGION: u32 = 1 << 10;

#[inline]
pub(crate) fn pack(tag: u32, val: u32) -> u32 {
    (val << TAG_BITS) | tag
}
#[inline]
pub(crate) fn tag(p: u32) -> u32 {
    p & TAG_MASK
}
#[inline]
pub(crate) fn val(p: u32) -> usize {
    (p >> TAG_BITS) as usize
}
#[inline]
pub(crate) fn is_var(p: u32) -> bool {
    tag(p) == VAR
}
#[inline]
pub(crate) fn is_producer(t: u32) -> bool {
    (L..=P).contains(&t)
}
/// Number of auxiliary cells an agent of this kind occupies (0 = nullary / no node).
#[inline]
pub(crate) fn arity(t: u32) -> usize {
    match t {
        S | N => 1,
        F | P | A | DS | DN => 2,
        T1 => 3,
        T2 => 4,
        _ => 0, // L, EPS, VAR, NUL — nullary, no allocated node
    }
}
