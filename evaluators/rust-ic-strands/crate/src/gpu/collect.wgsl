// An eraser at the turn's site touching garbage collects it (hw/rtl/strands_stages.v
// `strands_collect`; lattice.rs `collect`, `erase_inputs`, `erase_unpair`). The RTL looks at one
// eraser per cycle: the first, then (c_again) the second; here both in one call.

/// What one eraser's look decided (the RTL's outputs of the same names).
struct co_Out { c_apply: bool, c_stale: bool, c_two: bool, c_touched: u32, c_dpos: u32, c_s: Site, c_d: Site }

/// Eraser ke at p, and what it reads here or one strand away (the RTL's `collect_stage`). S is the
/// site at p, D the one holding the garbage; when they are the same site (`same`) the two copies
/// are kept in step as in the RTL.
fn co_collect(ke: u32) -> co_Out {
  var o = co_Out(false, false, false, 0u, 0u, EMPTY, EMPTY);
  var S = blk[p]; var D = S;
  var done = false;
  let t = gt(S, ke); let m = gm(S, ae(ke, 0u));
  var ok = t == T_EPS && m != NONE; var via = false; var dpos = p; var md = m; var f = 0u; var i = 0u;
  if (ok && is_strand(m)) {
    f = face(m); i = lane(m); let nn = inb(p, f);
    if ((nn & 8u) == 0u) { ok = false; }
    else if ((taken & (1u << (nn & 7u))) != 0u) { ok = false; o.c_stale = true; done = true; }
    else { via = true; dpos = nn & 7u; D = blk[dpos]; md = gm(D, se(f ^ 1u, i)); }
  }
  let same = !via;
  if (ok && md != NONE && !is_strand(md)) {
    let kd = port_k(md); let q = port_q(md); let td = gt(D, kd);
    if (td != 0u && (td == T_A || td == T_T1 || td == T_SEL) && q == 2u) {
      // erase_inputs: the dead computation's two inputs each get an eraser.
      let m0 = gm(D, ae(kd, 0u)); let m1 = gm(D, ae(kd, 1u));
      S = sm(S, ae(ke, 0u), NONE); if (same) { D = S; }
      for (var pp = 0u; pp < 3u; pp++) { D = sm(D, ae(kd, pp), NONE); }
      if (same) { S = D; }
      S = stg(S, ke, 0u); S = sw(S, ke, false); if (same) { D = S; }
      D = stg(D, kd, 0u); D = sw(D, kd, false);
      D = stg(D, kd, T_EPS); D = sw(D, kd, true);
      D = lk(D, ae(kd, 0u), m1);
      if (same) { S = D; }
      S = stg(S, ke, T_EPS); S = sw(S, ke, true);
      if (same) { S = lk(S, ae(ke, 0u), m0); D = S; }
      else { S = lk(S, ae(ke, 0u), se(f, i)); D = lk(D, se(f ^ 1u, i), m0); }
      o.c_apply = true; done = true;
    }
    if (!done && td == T_UNP && q != 0u) {
      // erase_unpair: both outputs being erased, the unpair and both erasers become one eraser.
      let other = gm(D, ae(kd, (3u - q) & 3u)); var ok2 = true; var via2 = false; var s2pos = dpos; var k2 = 0u; var f2 = 0u; var i2 = 0u;
      if (other != NONE && !is_strand(other)) { s2pos = dpos; k2 = port_k(other); }
      else if (is_strand(other) && nbq(dpos, face(other)) == (8u | p)) {
        f2 = face(other); i2 = lane(other); let mm = gm(S, se(f2 ^ 1u, i2));
        if (mm == NONE || is_strand(mm)) { ok2 = false; } else { s2pos = p; k2 = port_k(mm); via2 = true; }
      } else { ok2 = false; }
      let same2 = s2pos == p;
      if (ok2 && (select(gt(D, k2), gt(S, k2), same2) != T_EPS || (same2 && k2 == ke))) { ok2 = false; }
      if (ok2) {
        let m0 = gm(D, ae(kd, 0u));
        S = sm(S, only(via, se(f, i)), NONE); D = sm(D, only(via, se(f ^ 1u, i)), NONE);
        D = sm(D, only(via2, se(f2, i2)), NONE); S = sm(S, only(via2, se(f2 ^ 1u, i2)), NONE);
        S = sm(S, ae(ke, 0u), NONE); S = stg(S, ke, 0u); S = sw(S, ke, false); if (same) { D = S; }
        if (same2) { S = sm(S, ae(k2, 0u), NONE); S = stg(S, k2, 0u); S = sw(S, k2, false); if (same) { D = S; } }
        else { D = sm(D, ae(k2, 0u), NONE); D = stg(D, k2, 0u); D = sw(D, k2, false); }
        for (var pp = 0u; pp < 3u; pp++) { D = sm(D, ae(kd, pp), NONE); }
        D = stg(D, kd, 0u); D = sw(D, kd, false);
        D = stg(D, kd, T_EPS); D = sw(D, kd, true);
        D = lk(D, ae(kd, 0u), m0);
        if (same) { S = D; }
        o.c_apply = true; done = true;
      }
    }
    if (!done && td == T_DN && q != 0u) {
      // A duplicator read by an eraser becomes a plain wire from its input to its other output.
      let input_ = gm(D, ae(kd, 0u)); let other = gm(D, ae(kd, (3u - q) & 3u));
      if (input_ != ae(kd, (3u - q) & 3u)) {
        S = sm(S, only(via, se(f, i)), NONE); D = sm(D, only(via, se(f ^ 1u, i)), NONE);
        S = sm(S, ae(ke, 0u), NONE); if (same) { D = S; }
        for (var pp = 0u; pp < 3u; pp++) { D = sm(D, ae(kd, pp), NONE); }
        D = lk(D, input_, other);
        if (same) { S = D; }
        S = stg(S, ke, 0u); S = sw(S, ke, false); if (same) { D = S; }
        D = stg(D, kd, 0u); D = sw(D, kd, false);
        if (same) { S = D; }
        o.c_apply = true; done = true;
      }
    }
  }
  if (o.c_apply) {
    o.c_s = S; o.c_touched = 1u << p;
    if (!same) { o.c_d = D; o.c_dpos = dpos; o.c_two = true; o.c_touched |= 1u << dpos; }
  }
  return o;
}

/// The turn's first stage (the RTL's `collect_where` and the top's S_T_COLLECT): the first eraser
/// looks, then the second when the first did nothing. True when the turn ends here.
fn collect_stage() -> bool {
  let here = blk[p];
  let e0 = gt(here, 0u) == T_EPS && gm(here, ae(0u, 0u)) != NONE;
  let e1 = gt(here, 1u) == T_EPS && gm(here, ae(1u, 0u)) != NONE;
  let ke = select(0u, 1u, !e0);
  var c = co_collect(ke);
  let c_again = ke == 0u && e1 && !c.c_apply && !c.c_stale;
  if (c_again) { c = co_collect(1u); }
  if (c.c_stale) { stale = true; return true; }
  if (c.c_apply) {
    blk[p] = c.c_s;
    if (c.c_two) { blk[c.c_dpos] = c.c_d; }
    touched = c.c_touched;
    return true;
  }
  return false;
}
