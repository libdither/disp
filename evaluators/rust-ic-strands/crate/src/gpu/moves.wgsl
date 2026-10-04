// The rest of a turn (hw/rtl/strands_moves.vh and `strands_move` in strands_stages.v; lattice.rs
// `turn`, `active_turn`): a step or an exchange, or a wire folded or its corner flipped. Each move
// mirrors the function of the same name in lattice.rs, writing switchboard entries in the same order.

/// x * x of x's low 5 bits, as the RTL's 5-bit input truncates it.
fn sq(x: u32) -> u32 { let y = x & 31u; return y * y; }

/// hop_to's {ok, de, S', T'}.
struct Hop { ok: bool, de: i32, s: Site, t: Site }

/// Agent k of site S steps across face f into slot k2 of the neighbour T. With `metropolis`,
/// the step must leave T room and pass the energy test.
fn hop_to(S0: Site, T0: Site, k: u32, f: u32, k2: u32, metropolis: bool, metro: u32) -> Hop {
  var S = S0; var T = T0; let f1 = f ^ 1u;
  let tg = gt(S, k); let a = arity(tg); let wnt = gw(S, k); let idl = LAZY && !wnt;
  // Per port: plan (0 drag, 1 loop, 2 through) and its lane or loop end.
  var need = 0u; var de: i32 = 0; var pk = array<u32, 3>(0u, 0u, 0u); var pv = array<u32, 3>(0u, 0u, 0u); var fail = false;
  for (var q = 0u; q < 3u; q++) {
    if (q < a) {
      let m = gm(S, ae(k, q));
      let w = select(select(E_AUX, E_AUX_IDLE, idl), select(E_PRINCIPAL, E_PRINCIPAL_IDLE, idl), q == 0u);
      if (m < 9u && port_k(m) == k) { pk[q] = 1u; pv[q] = port_q(m); }
      else if (is_strand(m) && face(m) == f) { pk[q] = 2u; pv[q] = lane(m); de -= w; }
      else { need++; de += w; }
    }
  }
  // Through lanes in port order, then free lanes.
  var nl = 0u; var lanes = array<u32, 8>(0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u);
  for (var q = 0u; q < 3u; q++) { if (q < a && pk[q] == 2u) { lanes[nl] = pv[q]; nl++; } }
  let fu = lanes_used(S, f);
  for (var i = 0u; i < 4u; i++) { if (((fu >> i) & 1u) == 0u) { lanes[nl] = i; nl++; } }
  if (nl < need) { fail = true; }
  var loops = 0u; var through = 0u;
  for (var q = 0u; q < 3u; q++) {
    if (q < a) {
      if (pk[q] == 1u) { loops++; }
      if (pk[q] == 2u) { through++; }
    }
  }
  loops = loops / 2u;
  if (metropolis && pairs(T) + need + loops > PAIRS) { fail = true; }
  de += E_CROWD * (i32(occ(T)) - (i32(occ(S)) - 1));
  if (E_IDLE != 0 && idle(S, k)) { de += E_IDLE * (i32(idle_at(T)) - (i32(idle_at(S)) - 1)); }
  if (E_LINK != 0) { let n = i32(used_lanes(S, f)); let n2 = n - i32(through) + i32(need); de += E_LINK * (n2 * n2 - n * n); }
  if (E_BOARD != 0) {
    let ps = pairs(S); let pt = pairs(T);
    de += E_BOARD * (i32(sq(ps - through - loops)) - i32(sq(ps)) + i32(sq(pt + need + loops)) - i32(sq(pt)));
  }
  if (metropolis && !accept(de, metro)) { fail = true; }
  // Where the through-wires land inside T: a wire coming back to another port of the agent
  // becomes a loop.
  var tgt = array<u32, 3>(NONE, NONE, NONE);
  for (var q = 0u; q < 3u; q++) {
    if (q < a && pk[q] == 2u) {
      let mt = gm(T, se(f1, pv[q]));
      tgt[q] = mt;
      if (is_strand(mt) && face(mt) == f1) {
        for (var r = 3u; r > 0u; r--) { if (r - 1u < a && pk[r - 1u] == 2u && pv[r - 1u] == lane(mt)) { tgt[q] = ae(k2, r - 1u); } }
      }
    }
  }
  // Writes a port does not make go to no end, rather than choosing between whole sites.
  var li = 0u; var dm = array<u32, 3>(NONE, NONE, NONE); var dj = array<u32, 3>(0u, 0u, 0u);
  for (var q = 0u; q < 3u; q++) {
    S = sm(S, only(q < a && pk[q] == 2u, se(f, pv[q])), NONE);
    T = sm(T, only(q < a && pk[q] == 2u, se(f1, pv[q])), NONE);
    if (q < a && pk[q] == 0u) { dm[q] = gm(S, ae(k, q)); dj[q] = lanes[li] & 3u; li++; }
  }
  for (var q = 0u; q < 3u; q++) { S = sm(S, only(q < a, ae(k, q)), NONE); }
  S = stg(S, k, 0u); S = sw(S, k, false);
  T = stg(T, k2, tg); T = sw(T, k2, wnt);
  for (var q = 0u; q < 3u; q++) {
    let drag = q < a && pk[q] == 0u;
    S = lk(S, only(drag, dm[q]), only(drag, se(f, dj[q])));
    T = lk(T, only(drag, se(f1, dj[q])), only(drag, ae(k2, q)));
  }
  for (var q = 0u; q < 3u; q++) {
    T = lk(T, only(q < a && pk[q] != 0u, ae(k2, q)),
           select(select(NONE, tgt[q], q < a && pk[q] == 2u), ae(k2, pv[q]), q < a && pk[q] == 1u));
  }
  return Hop(!fail, de, S, T);
}

/// Move the agent in slot `from_` of S to the empty slot `to`, keeping its wires.
fn relocate(S0: Site, from_: u32, to: u32) -> Site {
  var S = S0; let a = arity(gt(S, from_)); var mates = array<u32, 3>(NONE, NONE, NONE);
  for (var q = 0u; q < 3u; q++) { mates[q] = gm(S, ae(from_, q)); }
  for (var q = 0u; q < 3u; q++) { S = sm(S, only(q < a, ae(from_, q)), NONE); }
  S = stg(S, to, gt(S0, from_)); S = sw(S, to, gw(S0, from_));
  S = stg(S, from_, 0u); S = sw(S, from_, false);
  for (var q = 0u; q < 3u; q++) {
    var m = mates[q];
    if (m < 9u && port_k(m) == from_) { m = ae(to, port_q(m)); }
    S = lk(S, only(q < a, ae(to, q)), only(q < a, m));
  }
  return S;
}

/// fold's {ok, S', T'}.
struct mv_Fold { ok: bool, s: Site, t: Site }

/// A wire leaving S on face f lane i and coming straight back from T on lane j snaps shut.
fn fold(S0: Site, T0: Site, e: u32) -> mv_Fold {
  let f = face(e); let f1 = f ^ 1u; let i = lane(e);
  let mt = gm(T0, se(f1, i)); let j = lane(mt);
  let a = gm(S0, se(f, i)); let b = gm(S0, se(f, j));
  var S = sm(sm(S0, se(f, i), NONE), se(f, j), NONE);
  let T = sm(sm(T0, se(f1, i), NONE), se(f1, j), NONE);
  S = lk(S, a, b);
  return mv_Fold(is_strand(mt) && face(mt) == f1 && a != se(f, j), S, T);
}

/// flip's {ok, S', X', Z', W'}.
struct mv_Flip { ok: bool, s: Site, x: Site, z: Site, w: Site }

/// A wire turning a corner at S (in on face f1 from X, out on face f2 to Z) moves to the opposite
/// corner W of the square.
fn flip(S0: Site, X0: Site, Z0: Site, W0: Site, e: u32, metro: u32) -> mv_Flip {
  let m = gm(S0, e); let f1 = face(e); let i = lane(e); let f2 = face(m); let j = lane(m);
  let c = free_lane(X0, f2); let d = free_lane(W0, f1 ^ 1u);
  let de = 2 * E_LINK * (i32(used_lanes(X0, f2)) + i32(used_lanes(W0, f1 ^ 1u)) - i32(used_lanes(S0, f1)) - i32(used_lanes(S0, f2)) + 2)
         + 2 * E_BOARD * (i32(pairs(W0)) - i32(pairs(S0)) + 1);
  let a = gm(X0, se(f1 ^ 1u, i)); let g = gm(Z0, se(f2 ^ 1u, j));
  let S = sm(sm(S0, e, NONE), m, NONE);
  var X = sm(X0, se(f1 ^ 1u, i), NONE); var Z = sm(Z0, se(f2 ^ 1u, j), NONE); var W = W0;
  X = lk(X, a, se(f2, c & 3u));
  W = lk(W, se(f2 ^ 1u, c & 3u), se(f1 ^ 1u, d & 3u));
  Z = lk(Z, se(f1, d & 3u), g);
  return mv_Flip((c & 4u) != 0u && (d & 4u) != 0u && pairs(W0) + 1u <= PAIRS && accept(de, metro), S, X, Z, W);
}

/// Whether an earlier turn this clock changed position n (its low 3 bits).
fn mv_taken(n: u32) -> bool { return ((taken >> (n & 7u)) & 1u) != 0u; }

/// The move stage (the RTL's `strands_move`): go (step or exchange agent k across face f), or
/// reshape the wire at end e (fold it with the neighbour T across f, or flip its corner through
/// T, Z across f2, and W). An exchange's two halves run one after the other.
fn move_stage(active_mode: bool, act_k: u32) {
  let S = blk[p];
  // move_decide: what the turn does, decided from its own site.
  var go = false; var may_swap = false; var reshape = false; var k = 0u; var f = 0u; var e = 0u; var tn = 0u;
  if (active_mode) {
    k = act_k; let m = gm(S, ae(k, 0u));
    if (is_strand(m)) { go = true; f = face(m); may_swap = d_swap() < CH_SWAP; }
  } else if (d_hop() < CH_HOP && occ(S) != 0u) {
    var n = 0u; var ksl = 0u;
    for (var i = 0u; i < 3u; i++) { if (gt(S, i) != 0u) { ksl |= i << (2u * n); n++; } }
    var pk_ = pick5(d_agent(), n); k = (ksl >> (2u * pk_)) & 3u; let m = gm(S, ae(k, 0u)); tn = inb(p, face(m));
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
    var n = 0u; var used = 0u;
    for (var i = 9u; i < 33u; i++) {
      tn = inb(p, face(i));
      if (gm(S, i) != NONE && (tn & 8u) != 0u) { used |= 1u << (i - 9u); n++; }
    }
    if (n != 0u) {
      reshape = true; let pk_ = pick5(d_where(), n); e = 0u; var c = 0u;
      for (var i = 0u; i < 24u; i++) { let u = (used >> i) & 1u; if (u != 0u && c == pk_) { e = 9u + i; } c += u; }
      f = face(e);
    }
  }
  tn = inb(p, f); let f2 = face(gm(S, e));
  let zn = inb(p, f2); let wn = select(0u, inb(tn & 7u, f2), (tn & 8u) != 0u);
  let T = blk[tn & 7u]; let Z = blk[zn & 7u]; let W = blk[wn & 7u];

  // move_act: fold or flip (written directly), or set up the hop: whether T is full, which of
  // its residents an exchange moves.
  var hop = false; var full = false; var kb = 0u; var fsl = 0u;
  var mv_touched = 0u; var mv_stale = false;
  if (reshape) {
    var folded = false;
    if (mv_taken(tn)) { mv_stale = true; }
    else {
      let fo = fold(S, T, e);
      if (fo.ok) {
        blk[p] = fo.s; blk[tn & 7u] = fo.t;
        mv_touched |= (1u << p) | (1u << (tn & 7u)); folded = true;
      }
    }
    let m = gm(S, e);
    if (!folded && !mv_stale && is_strand(m) && (f >> 1u) != (f2 >> 1u) && onlat(p, f) && onlat(p, f2)) {
      if ((tn & 8u) == 0u) {}
      else if ((zn & 8u) == 0u) {}
      else if (mv_taken(zn)) { mv_stale = true; }
      else if ((wn & 8u) == 0u) {}
      else if (mv_taken(wn)) { mv_stale = true; }
      else {
        let fl = flip(S, T, Z, W, e, d_metro());
        if (fl.ok) {
          blk[p] = fl.s; blk[tn & 7u] = fl.x; blk[zn & 7u] = fl.z; blk[wn & 7u] = fl.w;
          mv_touched |= (1u << p) | (1u << (tn & 7u)) | (1u << (zn & 7u)) | (1u << (wn & 7u));
        }
      }
    }
  }
  if (go) {
    if ((tn & 8u) == 0u) {}
    else if (mv_taken(tn)) { mv_stale = true; }
    else {
      fsl = free_slot(T); full = (fsl & 4u) == 0u;
      // An exchange's resident: T is full, so both its slots hold one.
      kb = select(1u, 0u, pick3(d_resident(), 2u) == 0u);
      hop = !(full && !may_swap);
    }
  }

  // The hop and move_write: a step into the free slot (judged on its own), or an exchange: agent
  // k into T's transient slot, then the resident kb back into k's old slot, then the transient
  // agent into kb's slot, judged on the two halves' total.
  if (hop && !full) {
    let h = hop_to(S, T, k, f, fsl & 3u, true, d_metro());
    if (h.ok) {
      blk[p] = h.s; blk[tn & 7u] = h.t;
      mv_touched |= (1u << p) | (1u << (tn & 7u));
    }
  }
  if (hop && full) {
    let h1 = hop_to(S, T, k, f, 2u, false, d_metro());
    let h = hop_to(h1.t, h1.s, kb, f ^ 1u, k, false, d_metro());
    let d1 = h1.de; let d2 = h.de;
    let S2 = h.t; let T2 = relocate(h.s, 2u, kb);
    if (h1.ok && h.ok && pairs(S2) <= PAIRS && pairs(T2) <= PAIRS && accept(d1 + d2, d_metro())) {
      blk[p] = S2; blk[tn & 7u] = T2;
      mv_touched |= (1u << p) | (1u << (tn & 7u));
    }
  }
  touched |= mv_touched;
  if (mv_stale) { stale = true; }
}
