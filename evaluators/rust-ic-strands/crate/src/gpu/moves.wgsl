// The rest of a turn (hw/rtl/strands_moves.vh and `strands_move` in strands_stages.v; lattice.rs
// `turn`, `active_turn`): a step or an exchange, or a wire folded or its corner flipped. Each move
// mirrors the function of the same name in lattice.rs, writing switchboard entries in the same order.
// Moves name sites by slot and change them in place; one that fails writes nothing.

/// x * x of x's low 5 bits, as the RTL's 5-bit input truncates it.
fn sq(x: u32) -> u32 { let y = x & 31u; return y * y; }

/// Bit i - 9 for each strand end i of slot s that has a mate.
fn mv_strands(s: u32) -> u32 {
  var u = 0u;
  for (var w = 2u; w < 9u; w++) { u |= held(sb[at(s, w)]) << (4u * w - 8u); }
  return (u >> 1u) & 0xFFFFFFu;
}

/// hop_to's {ok, de}.
struct Hop { ok: bool, de: i32 }

/// Agent k of slot s0 steps across face f into slot k2 of the neighbour t0, the result built in
/// slots so and to (which may be s0 and t0) if the step can be made. With `metropolis`, the step
/// must leave the neighbour room and pass the energy test.
fn hop_to(s0: u32, t0: u32, so: u32, to: u32, k: u32, f: u32, k2: u32, metropolis: bool, metro: u32) -> Hop {
  let f1 = f ^ 1u;
  let tg = gt(s0, k); let a = arity(tg); let wnt = gw(s0, k); let idl = LAZY && !wnt;
  // Per port: plan (0 drag, 1 loop, 2 through), its lane or loop end, and a dragged wire's mate.
  var need = 0u; var de: i32 = 0; var pk = array<u32, 3>(0u, 0u, 0u); var pv = array<u32, 3>(0u, 0u, 0u);
  var dm = array<u32, 3>(NONE, NONE, NONE); var fail = false;
  for (var q = 0u; q < 3u; q++) {
    if (q < a) {
      let m = gm(s0, ae(k, q));
      let w = select(select(E_AUX, E_AUX_IDLE, idl), select(E_PRINCIPAL, E_PRINCIPAL_IDLE, idl), q == 0u);
      if (m < 9u && port_k(m) == k) { pk[q] = 1u; pv[q] = port_q(m); }
      else if (is_strand(m) && face(m) == f) { pk[q] = 2u; pv[q] = lane(m); de -= w; }
      else { need++; de += w; dm[q] = m; }
    }
  }
  // Through lanes in port order, then free lanes, 2 bits each.
  var nl = 0u; var lanes = 0u;
  for (var q = 0u; q < 3u; q++) { if (q < a && pk[q] == 2u) { lanes |= pv[q] << (2u * nl); nl++; } }
  let fu = lanes_used(s0, f);
  for (var i = 0u; i < 4u; i++) { if (((fu >> i) & 1u) == 0u) { lanes |= i << (2u * nl); nl++; } }
  if (nl < need) { fail = true; }
  var loops = 0u; var through = 0u;
  for (var q = 0u; q < 3u; q++) {
    if (q < a) {
      if (pk[q] == 1u) { loops++; }
      if (pk[q] == 2u) { through++; }
    }
  }
  loops = loops / 2u;
  let pt = pairs(t0);
  if (metropolis && pt + need + loops > PAIRS) { fail = true; }
  de += E_CROWD * (i32(occ(t0)) - (i32(occ(s0)) - 1));
  if (E_IDLE != 0 && idle(s0, k)) { de += E_IDLE * (i32(idle_at(t0)) - (i32(idle_at(s0)) - 1)); }
  if (E_LINK != 0) { let n = i32(countOneBits(fu)); let n2 = n - i32(through) + i32(need); de += E_LINK * (n2 * n2 - n * n); }
  if (E_BOARD != 0) {
    let ps = pairs(s0);
    de += E_BOARD * (i32(sq(ps - through - loops)) - i32(sq(ps)) + i32(sq(pt + need + loops)) - i32(sq(pt)));
  }
  if (metropolis && !accept(de, metro)) { fail = true; }
  if (fail) { return Hop(false, de); }
  // Where the through-wires land inside the neighbour: a wire coming back to another port of the
  // agent becomes a loop.
  var tgt = array<u32, 3>(NONE, NONE, NONE);
  for (var q = 0u; q < 3u; q++) {
    if (q < a && pk[q] == 2u) {
      let mt = gm(t0, se(f1, pv[q]));
      tgt[q] = mt;
      if (is_strand(mt) && face(mt) == f1) {
        for (var r = 3u; r > 0u; r--) { if (r - 1u < a && pk[r - 1u] == 2u && pv[r - 1u] == lane(mt)) { tgt[q] = ae(k2, r - 1u); } }
      }
    }
  }
  if (so != s0) { copy(so, s0); }
  if (to != t0) { copy(to, t0); }
  var li = 0u; var dj = array<u32, 3>(0u, 0u, 0u);
  for (var q = 0u; q < 3u; q++) {
    sm(so, only(q < a && pk[q] == 2u, se(f, pv[q])), NONE);
    sm(to, only(q < a && pk[q] == 2u, se(f1, pv[q])), NONE);
    if (q < a && pk[q] == 0u) { dj[q] = (lanes >> (2u * li)) & 3u; li++; }
  }
  for (var q = 0u; q < 3u; q++) { sm(so, only(q < a, ae(k, q)), NONE); }
  stg(so, k, 0u); sw(so, k, false);
  stg(to, k2, tg); sw(to, k2, wnt);
  for (var q = 0u; q < 3u; q++) {
    let drag = q < a && pk[q] == 0u;
    lk(so, only(drag, dm[q]), only(drag, se(f, dj[q])));
    lk(to, only(drag, se(f1, dj[q])), only(drag, ae(k2, q)));
  }
  for (var q = 0u; q < 3u; q++) {
    lk(to, only(q < a && pk[q] != 0u, ae(k2, q)),
       select(select(NONE, tgt[q], q < a && pk[q] == 2u), ae(k2, pv[q]), q < a && pk[q] == 1u));
  }
  return Hop(true, de);
}

/// Move the agent in slot `from_` of site s to the empty slot `to`, keeping its wires.
fn relocate(s: u32, from_: u32, to: u32) {
  let tg = gt(s, from_); let wnt = gw(s, from_); let a = arity(tg);
  var mates = array<u32, 3>(NONE, NONE, NONE);
  for (var q = 0u; q < 3u; q++) { mates[q] = gm(s, ae(from_, q)); }
  for (var q = 0u; q < 3u; q++) { sm(s, only(q < a, ae(from_, q)), NONE); }
  stg(s, to, tg); sw(s, to, wnt);
  stg(s, from_, 0u); sw(s, from_, false);
  for (var q = 0u; q < 3u; q++) {
    var m = mates[q];
    if (m < 9u && port_k(m) == from_) { m = ae(to, port_q(m)); }
    lk(s, only(q < a, ae(to, q)), only(q < a, m));
  }
}

/// A wire leaving site s on face f lane i and coming straight back from t on lane j snaps shut;
/// whether it did.
fn fold(s: u32, t: u32, e: u32) -> bool {
  let f = face(e); let f1 = f ^ 1u; let i = lane(e);
  let mt = gm(t, se(f1, i)); let j = lane(mt);
  let a = gm(s, se(f, i)); let b = gm(s, se(f, j));
  if (!(is_strand(mt) && face(mt) == f1 && a != se(f, j))) { return false; }
  sm(s, se(f, i), NONE); sm(s, se(f, j), NONE);
  sm(t, se(f1, i), NONE); sm(t, se(f1, j), NONE);
  lk(s, a, b);
  return true;
}

/// A wire turning a corner at site s (in on face f1 from x, out on face f2 to z) moves to the
/// opposite corner w of the square; whether it did.
fn flip(s: u32, x: u32, z: u32, w: u32, e: u32, metro: u32) -> bool {
  let m = gm(s, e); let f1 = face(e); let i = lane(e); let f2 = face(m); let j = lane(m);
  let c = free_lane(x, f2); let d = free_lane(w, f1 ^ 1u); let pw = pairs(w);
  let de = 2 * E_LINK * (i32(used_lanes(x, f2)) + i32(used_lanes(w, f1 ^ 1u)) - i32(used_lanes(s, f1)) - i32(used_lanes(s, f2)) + 2)
         + 2 * E_BOARD * (i32(pw) - i32(pairs(s)) + 1);
  if (!((c & 4u) != 0u && (d & 4u) != 0u && pw + 1u <= PAIRS && accept(de, metro))) { return false; }
  let a = gm(x, se(f1 ^ 1u, i)); let g = gm(z, se(f2 ^ 1u, j));
  sm(s, e, NONE); sm(s, m, NONE);
  sm(x, se(f1 ^ 1u, i), NONE); sm(z, se(f2 ^ 1u, j), NONE);
  lk(x, a, se(f2, c & 3u));
  lk(w, se(f2 ^ 1u, c & 3u), se(f1 ^ 1u, d & 3u));
  lk(z, se(f1, d & 3u), g);
  return true;
}

/// Agent k at p is a wanted reader stepping along its principal wire across f (lattice.rs `hop`).
fn walker(k: u32, f: u32) -> bool {
  let m = gm(p, ae(k, 0u));
  return LAZY && gw(p, k) && is_consumer(gt(p, k)) && is_strand(m) && face(m) == f;
}

/// Whether an earlier turn this clock changed position n (its low 3 bits).
fn mv_taken(n: u32) -> bool { return ((taken >> (n & 7u)) & 1u) != 0u; }

/// The move stage (the RTL's `strands_move`): go (step or exchange agent k across face f), or
/// reshape the wire at end e (fold it with the neighbour T across f, or flip its corner through
/// T, Z across f2, and W). An exchange's two halves run one after the other in working slots.
fn move_stage(active_mode: bool, act_k: u32) {
  // move_decide: what the turn does, decided from its own site.
  var go = false; var may_swap = false; var reshape = false; var k = 0u; var f = 0u; var e = 0u; var tn = 0u;
  if (active_mode) {
    k = act_k; let m = gm(p, ae(k, 0u));
    if (is_strand(m)) { go = true; f = face(m); may_swap = d_swap() < CH_SWAP; }
  } else if (d_hop() < CH_HOP && occ(p) != 0u) {
    var n = 0u; var ksl = 0u;
    for (var i = 0u; i < 3u; i++) { if (gt(p, i) != 0u) { ksl |= i << (2u * n); n++; } }
    var pk_ = pick5(d_agent(), n); k = (ksl >> (2u * pk_)) & 3u; let m = gm(p, ae(k, 0u)); tn = inb(p, face(m));
    if (is_strand(m) && (tn & 8u) != 0u && d_along() < CH_ALONG) { go = true; f = face(m); }
    else {
      n = 0u; var faces = 0u;
      for (var i = 0u; i < 6u; i++) { tn = inb(p, i); if ((tn & 8u) != 0u) { faces |= i << (3u * n); n++; } }
      if (n != 0u) { go = true; pk_ = pick5(d_where(), n); f = (faces >> (3u * pk_)) & 7u; }
    }
    may_swap = d_swap() < CH_SWAP;
  } else {
    // Reshape a wire: the chosen one among the strand ends in use (with a neighbour in the
    // block), in order.
    var used = mv_strands(p);
    for (var g = 0u; g < 6u; g++) { if ((inb(p, g) & 8u) == 0u) { used &= ~(15u << (4u * g)); } }
    let n = countOneBits(used);
    if (n != 0u) {
      reshape = true; let pk_ = pick5(d_where(), n);
      for (var c = 0u; c < pk_; c++) { used &= used - 1u; }
      e = 9u + firstTrailingBit(used); f = face(e);
    }
  }
  tn = inb(p, f); let m = gm(p, e); let f2 = face(m);
  let zn = inb(p, f2); let wn = select(0u, inb(tn & 7u, f2), (tn & 8u) != 0u);
  let t = tn & 7u;

  // move_act: fold or flip (written directly), or set up the hop: whether T is full, which of
  // its residents an exchange moves.
  var hop = false; var full = false; var kb = 0u; var fsl = 0u; var wk = false;
  var mv_touched = 0u; var mv_stale = false;
  if (reshape) {
    var folded = false;
    if (mv_taken(tn)) { mv_stale = true; }
    else if (fold(p, t, e)) { mv_touched |= (1u << p) | (1u << t); folded = true; tally[T_FOLDS]++; }
    if (!folded && !mv_stale && is_strand(m) && (f >> 1u) != (f2 >> 1u) && onlat(p, f) && onlat(p, f2)) {
      if ((tn & 8u) == 0u) {}
      else if ((zn & 8u) == 0u) {}
      else if (mv_taken(zn)) { mv_stale = true; }
      else if ((wn & 8u) == 0u) {}
      else if (mv_taken(wn)) { mv_stale = true; }
      else if (flip(p, t, zn & 7u, wn & 7u, e, d_metro())) {
        mv_touched |= (1u << p) | (1u << t) | (1u << (zn & 7u)) | (1u << (wn & 7u));
        tally[T_FLIPS]++;
      }
    }
  }
  if (go) {
    if ((tn & 8u) == 0u) {}
    else if (mv_taken(tn)) { mv_stale = true; }
    else {
      fsl = free_slot(t); full = (fsl & 4u) == 0u;
      wk = walker(k, f);
      if (full && wk) { tally[T_NO_SEAT]++; }
      // An exchange's resident: T is full, so both its slots hold one.
      kb = select(1u, 0u, pick3(d_resident(), 2u) == 0u);
      hop = !(full && !may_swap);
    }
  }

  // The hop and move_write: a step into the free slot (judged on its own), or an exchange: agent
  // k into T's transient slot, then the resident kb back into k's old slot, then the transient
  // agent into kb's slot, judged on the two halves' total. A step is written in place, an
  // exchange built in slots xs, xt until it is judged.
  if (hop) {
    let xs = select(p, TMP, full); let xt = select(t, TMP + 1u, full);
    let h1 = hop_to(p, t, xs, xt, k, f, select(fsl & 3u, 2u, full), !full, d_metro());
    if (h1.ok && !full) { mv_touched |= (1u << p) | (1u << t); tally[T_HOPS]++; if (wk) { tally[T_WALKS]++; } }
    if (h1.ok && full) {
      let h = hop_to(xt, xs, xt, xs, kb, f ^ 1u, k, false, d_metro());
      if (h.ok) {
        relocate(xt, 2u, kb);
        if (pairs(xs) <= PAIRS && pairs(xt) <= PAIRS && accept(h1.de + h.de, d_metro())) {
          copy(p, xs); copy(t, xt);
          mv_touched |= (1u << p) | (1u << t);
          tally[T_HOPS] += 2u; tally[T_SWAPS]++;
        }
      }
    }
  }
  touched |= mv_touched;
  if (mv_stale) { stale = true; }
}
