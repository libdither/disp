// An eraser at the turn's site touching garbage collects it (hw/rtl/strands_stages.v
// `strands_collect`; lattice.rs `collect`, `erase_inputs`, `erase_unpair`). The RTL looks at one
// eraser per cycle: the first, then (c_again) the second; here both in one call.

/// What one eraser's look decided (the RTL's outputs of the same names; the sites themselves are
/// changed in place).
struct co_Out { c_apply: bool, c_stale: bool, c_touched: u32 }

/// Eraser ke at p, and what it reads here or one strand away (the RTL's `collect_stage`). S is the
/// slot of the site at p, D of the one holding the garbage, the same slot when they are the same
/// site (`same`: the RTL's two copies kept in step). Every write is on a path that applies, so a
/// look that collects nothing leaves the block as it was.
fn co_collect(ke: u32) -> co_Out {
  var o = co_Out(false, false, 0u);
  let S = p; var D = S;
  var done = false;
  let t = gt(S, ke); let m = gm(S, ae(ke, 0u));
  var ok = t == T_EPS && m != NONE; var via = false; var dpos = p; var md = m; var f = 0u; var i = 0u;
  if (ok && is_strand(m)) {
    f = face(m); i = lane(m); let nn = inb(p, f);
    if ((nn & 8u) == 0u) { ok = false; }
    else if ((taken & (1u << (nn & 7u))) != 0u) { ok = false; o.c_stale = true; done = true; }
    else { via = true; dpos = nn & 7u; D = dpos; md = gm(D, se(f ^ 1u, i)); }
  }
  let same = !via;
  if (ok && md != NONE && !is_strand(md)) {
    let kd = port_k(md); let q = port_q(md); let td = gt(D, kd);
    if (td != 0u && (td == T_A || td == T_T1 || td == T_SEL) && q == 2u) {
      // erase_inputs: the dead computation's two inputs each get an eraser.
      let m0 = gm(D, ae(kd, 0u)); let m1 = gm(D, ae(kd, 1u));
      sm(S, ae(ke, 0u), NONE);
      for (var pp = 0u; pp < 3u; pp++) { sm(D, ae(kd, pp), NONE); }
      stg(S, ke, 0u); sw(S, ke, false);
      stg(D, kd, 0u); sw(D, kd, false);
      stg(D, kd, T_EPS); sw(D, kd, true);
      lk(D, ae(kd, 0u), m1);
      stg(S, ke, T_EPS); sw(S, ke, true);
      if (same) { lk(S, ae(ke, 0u), m0); }
      else { lk(S, ae(ke, 0u), se(f, i)); lk(D, se(f ^ 1u, i), m0); }
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
      let S2 = select(D, S, same2);
      if (ok2 && (gt(S2, k2) != T_EPS || (same2 && k2 == ke))) { ok2 = false; }
      if (ok2) {
        let m0 = gm(D, ae(kd, 0u));
        sm(S, only(via, se(f, i)), NONE); sm(D, only(via, se(f ^ 1u, i)), NONE);
        sm(D, only(via2, se(f2, i2)), NONE); sm(S, only(via2, se(f2 ^ 1u, i2)), NONE);
        sm(S, ae(ke, 0u), NONE); stg(S, ke, 0u); sw(S, ke, false);
        sm(S2, ae(k2, 0u), NONE); stg(S2, k2, 0u); sw(S2, k2, false);
        for (var pp = 0u; pp < 3u; pp++) { sm(D, ae(kd, pp), NONE); }
        stg(D, kd, 0u); sw(D, kd, false);
        stg(D, kd, T_EPS); sw(D, kd, true);
        lk(D, ae(kd, 0u), m0);
        o.c_apply = true; done = true;
      }
    }
    if (!done && td == T_DN && q != 0u) {
      // A duplicator read by an eraser becomes a plain wire from its input to its other output.
      let input_ = gm(D, ae(kd, 0u)); let other = gm(D, ae(kd, (3u - q) & 3u));
      if (input_ != ae(kd, (3u - q) & 3u)) {
        sm(S, only(via, se(f, i)), NONE); sm(D, only(via, se(f ^ 1u, i)), NONE);
        sm(S, ae(ke, 0u), NONE);
        for (var pp = 0u; pp < 3u; pp++) { sm(D, ae(kd, pp), NONE); }
        lk(D, input_, other);
        stg(S, ke, 0u); sw(S, ke, false);
        stg(D, kd, 0u); sw(D, kd, false);
        o.c_apply = true; done = true;
      }
    }
  }
  if (o.c_apply) { o.c_touched = (1u << p) | (1u << dpos); }
  return o;
}

/// The turn's first stage (the RTL's `collect_where` and the top's S_T_COLLECT): the first eraser
/// looks, then the second when the first did nothing. True when the turn ends here.
fn collect_stage() -> bool {
  let e0 = gt(p, 0u) == T_EPS && gm(p, ae(0u, 0u)) != NONE;
  let e1 = gt(p, 1u) == T_EPS && gm(p, ae(1u, 0u)) != NONE;
  let ke = select(0u, 1u, !e0);
  var c = co_collect(ke);
  let c_again = ke == 0u && e1 && !c.c_apply && !c.c_stale;
  if (c_again) { c = co_collect(1u); }
  if (c.c_stale) { stale = true; return true; }
  if (c.c_apply) { touched = c.c_touched; return true; }
  return false;
}
