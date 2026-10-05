// The chip's block schedule on a GPU: what every stage shares. Names follow hw/rtl
// (strands_fn.vh, strands_geom.vh), which follow crate/src/lattice.rs.
//
// A site is 40 bytes in 10 words, as the simulator's state dumps and test vectors have it:
//   bytes  0..32  the mate of each of 33 ends (255: none). Ends 0..8 are agent ports
//                 (slot * 3 + port; slot 2 is the transient slot of an exchange), ends 9..32
//                 strand ends (9 + face * 4 + lane)
//   bytes 33..35  the three slots' tags (0: empty)
//   bytes 36..38  the three slots' wanted bits
//   byte  39      the strand end a demand pulse sits on (255: none)
// The stages name sites by slot (below); the `v` functions, for the pulse phase, take and return
// a site as a value.

alias Site = array<u32, 10>;
const NONE: u32 = 255u;
const NE: u32 = 33u;
/// A site holding nothing.
const EMPTY: Site = Site(0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
                         0xFFFFFFFFu, 0xFFFFFFFFu, 0x000000FFu, 0xFF000000u);

// ---- the block under way ------------------------------------------------------------------
/// Bit 4a + 2b + d: along axis a, the neighbour of the positions on side b in direction d (0 up,
/// 1 down) is on the lattice.
var<private> lat: u32;
/// Bit q: position q is on the lattice.
var<private> onl: u32;
/// The position taking its turn.
var<private> p: u32;
/// Positions some earlier turn changed this clock: a turn that looks at one is dropped.
var<private> taken: u32;
/// Positions this turn wrote.
var<private> touched: u32;
/// This turn looked at a taken position.
var<private> stale: bool;
/// The turn's 64 random bits: .x bits 0..31, .y bits 32..63.
var<private> dice: vec2<u32>;

// ---- a site as a value ---------------------------------------------------------------------
fn vgetb(s: Site, i: u32) -> u32 { var t = s; return (t[i >> 2u] >> ((i & 3u) * 8u)) & 0xFFu; }
fn vsetb(s: Site, i: u32, v: u32) -> Site {
  var t = s; let w = i >> 2u; let sh = (i & 3u) * 8u;
  t[w] = (t[w] & ~(0xFFu << sh)) | ((v & 0xFFu) << sh);
  return t;
}
/// The mate of end e (none for an end beyond the 33).
fn vgm(s: Site, e: u32) -> u32 { if (e >= NE) { return NONE; } return vgetb(s, e); }
/// n, hidden from the compiler: a loop it bounds is not unrolled, so its code is fetched once (a
/// turn's code is several times the GPU's instruction cache).
fn rolled(n: u32) -> u32 { return n + clk.zero; }
/// End e if c holds, else no end.
fn only(c: bool, e: u32) -> u32 { return select(NONE, e, c); }
fn vgt(s: Site, k: u32) -> u32 { return vgetb(s, 33u + min(k, 2u)); }
fn vgw(s: Site, k: u32) -> bool { return vgetb(s, 36u + min(k, 2u)) != 0u; }
fn vsw(s: Site, k: u32, v: bool) -> Site { return vsetb(s, 36u + min(k, 2u), select(0u, 1u, v)); }
fn vspul(s: Site, v: u32) -> Site { return vsetb(s, 39u, v); }

// ---- sites in slots -------------------------------------------------------------------------
// Each invocation keeps its sites in slots of workgroup memory: slots 0..7 are the block's
// positions, then working copies (TMP on; the rewrite's working copy is the most, four), then
// slot PAIR, a word per position noting its pair (block.wgsl). Functions name a site by its slot and change it in
// place, so a site's byte is an address, not a choice among ten words held in registers. Word w of
// slot s of invocation lid is at (s * 10 + w) * WG + lid: neighbouring invocations, neighbouring banks.
// A workgroup is WG invocations, fewer than a wave: a wave runs every path any of its blocks
// takes, so fewer blocks to a wave means fewer paths, and the waves are spread over more of the GPU.
const SLOTS: u32 = 13u;
const TMP: u32 = 8u;
const PAIR: u32 = 12u;
const WG: u32 = 16u;
var<workgroup> sb: array<u32, 2080>;
/// This invocation's place in its workgroup.
var<private> lid: u32;
fn at(s: u32, w: u32) -> u32 { return (s * 10u + w) * WG + lid; }
fn getb(s: u32, i: u32) -> u32 { return (sb[at(s, i >> 2u)] >> ((i & 3u) * 8u)) & 0xFFu; }
fn setb(s: u32, i: u32, v: u32) { let a = at(s, i >> 2u); let sh = (i & 3u) * 8u; sb[a] = (sb[a] & ~(0xFFu << sh)) | ((v & 0xFFu) << sh); }
fn get_site(s: u32) -> Site { var t: Site; for (var w = 0u; w < 10u; w++) { t[w] = sb[at(s, w)]; } return t; }
fn put_site(s: u32, t: Site) { for (var w = 0u; w < 10u; w++) { sb[at(s, w)] = t[w]; } }
/// Slot d becomes a copy of slot s.
fn copy(d: u32, s: u32) { for (var w = 0u; w < 10u; w++) { sb[at(d, w)] = sb[at(s, w)]; } }
/// The mate of end e of slot s (none for an end beyond the 33).
fn gm(s: u32, e: u32) -> u32 { if (e >= NE) { return NONE; } return getb(s, e); }
/// Set the mate of end e; a write to no end (NONE, or beyond the 33) changes nothing.
fn sm(s: u32, e: u32, v: u32) { if (e < NE) { setb(s, e, v); } }
fn lk(s: u32, a: u32, b: u32) { sm(s, a, b); sm(s, b, a); }
fn gt(s: u32, k: u32) -> u32 { return getb(s, 33u + min(k, 2u)); }
fn stg(s: u32, k: u32, v: u32) { setb(s, 33u + min(k, 2u), v); }
fn gw(s: u32, k: u32) -> bool { return getb(s, 36u + min(k, 2u)) != 0u; }
fn sw(s: u32, k: u32, v: bool) { setb(s, 36u + min(k, 2u), select(0u, 1u, v)); }
fn gpul(s: u32) -> u32 { return getb(s, 39u); }
fn spul(s: u32, v: u32) { setb(s, 39u, v); }
fn occ(s: u32) -> u32 { return u32(gt(s, 0u) != 0u) + u32(gt(s, 1u) != 0u) + u32(gt(s, 2u) != 0u); }
/// Bit j for each byte j of word x that is not NONE.
fn held(x: u32) -> u32 {
  let y = ~x; let z = y | (y >> 4u); let v = z | (z >> 2u); let b = (v | (v >> 1u)) & 0x01010101u;
  return (b | (b >> 7u) | (b >> 14u) | (b >> 21u)) & 15u;
}
/// Pairings on the switchboard, a word at a time (ends 0..31 in words 0..7, end 32 in word 8).
fn pairs(s: u32) -> u32 {
  var n = held(sb[at(s, 8u)]) & 1u;
  for (var w = 0u; w < 8u; w++) { n += countOneBits(held(sb[at(s, w)])); }
  return n >> 1u;
}
fn idle(s: u32, k: u32) -> bool { return gt(s, k) != 0u && !gw(s, k); }
fn idle_at(s: u32) -> u32 { return u32(idle(s, 0u)) + u32(idle(s, 1u)) + u32(idle(s, 2u)); }
/// Which of face f's four lanes hold a strand end: bytes 9 + 4f .. 12 + 4f, across two words.
fn lanes_used(s: u32, f: u32) -> u32 {
  if (f >= 6u) { return 0u; }
  return held((sb[at(s, 2u + f)] >> 8u) | (sb[at(s, 3u + f)] << 24u));
}
fn used_lanes(s: u32, f: u32) -> u32 { return countOneBits(lanes_used(s, f)); }
fn free_lane(s: u32, f: u32) -> u32 { let u = lanes_used(s, f); return select(4u | firstTrailingBit(~u), 0u, u == 15u); }
fn free_slot(s: u32) -> u32 {
  if (gt(s, 0u) == 0u) { return 4u; }
  if (gt(s, 1u) == 0u) { return 5u; }
  return 0u;
}

// ---- ends ------------------------------------------------------------------------------------
fn is_strand(e: u32) -> bool { return e >= 9u && e < NE; }
fn face(e: u32) -> u32 { return ((e - 9u) >> 2u) & 7u; }
fn lane(e: u32) -> u32 { return (e - 9u) & 3u; }
fn se(f: u32, i: u32) -> u32 { return 9u + 4u * f + i; }
fn ae(k: u32, q: u32) -> u32 { return 3u * k + q; }
fn port_k(e: u32) -> u32 { return select(select(0u, 1u, e >= 3u), 2u, e >= 6u); }
fn port_q(e: u32) -> u32 { return (e - 3u * port_k(e)) & 3u; }

// ---- tags --------------------------------------------------------------------------------------
fn is_consumer(t: u32) -> bool { return t >= T_A && t <= T_NRM; }
fn is_producer(t: u32) -> bool { return t >= T_L && t <= T_PAIR; }
fn arity(t: u32) -> u32 {
  if (t == T_L || t == T_EPS || t == T_OUT) { return 1u; }
  if (t == T_S || t == T_NRM) { return 2u; }
  return 3u;
}
/// Wanted from the start: the normalizer, the root and erasers.
fn born_wanted(t: u32) -> bool { return t == T_NRM || t == T_OUT || t == T_EPS; }

// ---- counts ------------------------------------------------------------------------------------
/// Agents in the three slots.
fn vocc(s: Site) -> u32 { return u32(vgt(s, 0u) != 0u) + u32(vgt(s, 1u) != 0u) + u32(vgt(s, 2u) != 0u); }
/// pick(n bits of field, among): (bits * among) >> n.
fn pick5(bits: u32, among: u32) -> u32 { return (bits * among) >> 5u; }
fn pick3(bits: u32, among: u32) -> u32 { return (bits * among) >> 3u; }

// ---- the block's geometry -------------------------------------------------------------------
/// The position across face f from position q: 8 | position if inside the block, else 0.
fn nbq(q: u32, f: u32) -> u32 {
  switch f {
    case 0u: { if ((q & 1u) != 0u) { return 0u; } return 8u | (q | 1u); }
    case 1u: { if ((q & 1u) != 0u) { return 8u | (q & 6u); } return 0u; }
    case 2u: { if ((q & 2u) != 0u) { return 0u; } return 8u | (q | 2u); }
    case 3u: { if ((q & 2u) != 0u) { return 8u | (q & 5u); } return 0u; }
    case 4u: { if ((q & 4u) != 0u) { return 0u; } return 8u | (q | 4u); }
    case 5u: { if ((q & 4u) != 0u) { return 8u | (q & 3u); } return 0u; }
    default: { return 0u; }
  }
}
/// Whether position q's neighbour across face f is on the lattice.
fn onlat(q: u32, f: u32) -> bool { let a = f >> 1u; return f < 6u && ((lat >> (4u * a + 2u * ((q >> a) & 1u) + (f & 1u))) & 1u) != 0u; }
/// Position q's neighbour across f if it is in the block (and so on the lattice): 8 | position, else 0.
fn inb(q: u32, f: u32) -> u32 { let n = nbq(q, f); if ((n & 8u) != 0u && onlat(q, f)) { return n; } return 0u; }

// ---- randomness ----------------------------------------------------------------------------------
/// The random word for a key at a clock: eight add-rotate-xor rounds (lattice.rs `hash`);
/// .x bits 0..31, .y bits 32..63.
fn hash(key: u32, clock: u32) -> vec2<u32> {
  var x = clock; var y = key;
  for (var i = 0u; i < 8u; i++) {
    x = (((x >> 8u) | (x << 24u)) + y) ^ key ^ (i * 0x9E3779B9u);
    y = ((y << 3u) | (y >> 29u)) ^ x;
  }
  return vec2<u32>(y, x);
}
/// n bits of the turn's random word from bit `at` (fields never straddle bit 32).
fn dbits(at: u32, n: u32) -> u32 {
  let m = (1u << n) - 1u;
  if (at >= 32u) { return (dice.y >> (at - 32u)) & m; }
  return (dice.x >> at) & m;
}
fn d_metro() -> u32 { return dbits(0u, 16u); }
fn d_active() -> u32 { return dbits(16u, 8u); }
fn d_hop() -> u32 { return dbits(24u, 8u); }
fn d_along() -> u32 { return dbits(32u, 8u); }
fn d_swap() -> u32 { return dbits(40u, 8u); }
fn d_agent() -> u32 { return dbits(48u, 5u); }
fn d_where() -> u32 { return dbits(53u, 5u); }
fn d_resident() -> u32 { return dbits(58u, 3u); }
fn d_region() -> u32 { return dbits(61u, 3u); }

/// Metropolis in integers: a rise of de quarter units passes when 16 random bits are below
/// accept_thr(de).
fn accept(de: i32, metro: u32) -> bool { return de <= 0 || (de < ACCEPT_LEN && metro < accept_thr(u32(de))); }
