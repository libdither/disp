// The rewrite (hw/rtl/strands_stages.v `strands_fire_setup`, `_prep`, `_search`, `_init`, `_link`;
// strands_route.vh; strands_block.v states S_F_SETUP .. S_F_WB; lattice.rs `fire`, `regions`,
// `free_lanes`). The RTL's ports and registers keep their names behind an fr_ prefix; its cycles
// (one seating, one link per cycle) become loops.
//
// A link's terminal is 9 bits, as in the RTL: {1, at the producer's site across the strand, 0,
// end (63: none)} for an end that stays, {000, fresh agent, port} for a fresh port.

// ---- the pair being rewritten (fc_*) and the setup's results (fs_*) --------------------------
var<private> fr_fc_kc: u32;
var<private> fr_fc_kp: u32;
var<private> fr_fc_via: bool;
var<private> fr_fc_face: u32;
var<private> fr_fs_ri: u32;
var<private> fr_fs_n: u32;
var<private> fr_fs_nl: u32;
var<private> fr_fs_la: array<u32, 9>;
var<private> fr_fs_lb: array<u32, 9>;
var<private> fr_fs_sp: u32;
var<private> fr_fs_lane: u32;
var<private> fr_fs_nreg: u32;
var<private> fr_fs_regs: array<u32, 3>;
var<private> fr_fs_lost_s: u32;
var<private> fr_fs_lost_sp: u32;
/// The rule's wires, read from the table once, and the other end of each dying aux port's wire.
var<private> fr_fs_wa: array<u32, 9>;
var<private> fr_fs_wb: array<u32, 9>;
var<private> fr_fs_nw: u32;
var<private> fr_fs_part: array<u32, 4>;

// ---- the candidate square under way (fp_*) ---------------------------------------------------
var<private> fr_fp_fx: u32;
var<private> fr_fp_fy: u32;
var<private> fr_fp_pos: array<u32, 4>;
var<private> fr_fp_ploc: u32;
/// Per square position: the slots fresh agents may take, in order (2 bits each), and how many.
var<private> fr_fp_slots: array<u32, 4>;
var<private> fr_fp_nslots: array<u32, 4>;
/// Per square edge: its free lanes (the strand between the pair counted free).
var<private> fr_fp_avail: array<u32, 4>;
/// Per square position: the pairings left once the dying pair goes.
var<private> fr_fp_left: array<u32, 4>;
var<private> fr_fp_q: array<Site, 4>;

// ---- the rewrite's working copy (aq, aloc, aseat, alanes, aptr, atch) -----------------------------
var<private> fr_aq: array<Site, 4>;
var<private> fr_aloc: u32;
var<private> fr_aseat: u32;
/// Per square edge: its lanes for new strands (2 bits each), and how many are still unused.
var<private> fr_alanes: array<u32, 4>;
var<private> fr_aptr: array<u32, 4>;
var<private> fr_atch: u32;

// ---- routes inside the square (strands_route.vh) -----------------------------------------------
fn fr_rt(n: u32, s1: u32, e1: u32, d1: u32, s0: u32, e0: u32, d0: u32) -> u32 {
  return (n << 12u) | (s1 << 10u) | (e1 << 8u) | (d1 << 6u) | (s0 << 4u) | (e0 << 2u) | d0;
}
/// The route from square position a to b: {steps, step 1, step 0}, a step being
/// {site, edge, direction (0 fx, 1 back along fx, 2 fy, 3 back along fy)}.
fn fr_route(a: u32, b: u32) -> u32 {
  switch ((a << 2u) | b) {
    case 1u:  { return fr_rt(1u, 0u, 0u, 0u, 0u, 0u, 0u); }
    case 4u:  { return fr_rt(1u, 0u, 0u, 0u, 1u, 0u, 1u); }
    case 2u:  { return fr_rt(1u, 0u, 0u, 0u, 0u, 1u, 2u); }
    case 8u:  { return fr_rt(1u, 0u, 0u, 0u, 2u, 1u, 3u); }
    case 7u:  { return fr_rt(1u, 0u, 0u, 0u, 1u, 2u, 2u); }
    case 13u: { return fr_rt(1u, 0u, 0u, 0u, 3u, 2u, 3u); }
    case 11u: { return fr_rt(1u, 0u, 0u, 0u, 2u, 3u, 0u); }
    case 14u: { return fr_rt(1u, 0u, 0u, 0u, 3u, 3u, 1u); }
    case 3u:  { return fr_rt(2u, 1u, 2u, 2u, 0u, 0u, 0u); }
    case 12u: { return fr_rt(2u, 1u, 0u, 1u, 3u, 2u, 3u); }
    case 6u:  { return fr_rt(2u, 0u, 1u, 2u, 1u, 0u, 1u); }
    case 9u:  { return fr_rt(2u, 0u, 0u, 0u, 2u, 1u, 3u); }
    default:  { return 0u; }
  }
}
fn fr_dir_face(dir: u32) -> u32 {
  if (dir == 0u) { return fr_fp_fx; }
  if (dir == 1u) { return fr_fp_fx ^ 1u; }
  if (dir == 2u) { return fr_fp_fy; }
  return fr_fp_fy ^ 1u;
}
/// Where a link end sits in the square, for a seating.
fn fr_at(t: u32, loc: u32) -> u32 {
  if ((t & 0x100u) != 0u) { return select(0u, fr_fp_ploc, (t & 0x80u) != 0u); }
  return (loc >> (((t >> 3u) & 7u) * 2u)) & 3u;
}
/// The end a terminal stands for, given the fresh agents' seats (an end that stays as 63: none).
fn fr_end(t: u32, seat: u32) -> u32 {
  if ((t & 0x100u) != 0u) { let m = t & 63u; return select(m, NONE, m == 63u); }
  return ae((seat >> (((t >> 3u) & 7u) * 2u)) & 3u, t & 3u);
}
/// For a seating: the new strands per square edge (4 bits each: up to one per link), and the
/// pairings each site gains (6 bits each): (need, add).
fn fr_need_add(loc: u32) -> vec2<u32> {
  var nd = array<u32, 4>(0u, 0u, 0u, 0u);
  var add = array<u32, 4>(0u, 0u, 0u, 0u);
  for (var j = 0u; j < 9u; j++) {
    if (j >= fr_fs_nl) { break; }
    let la = fr_at(fr_fs_la[j], loc); let lb = fr_at(fr_fs_lb[j], loc);
    let r = fr_route(la, lb);
    for (var si = 0u; si < 2u; si++) {
      if (si < (r >> 12u)) {
        let st = (r >> (si * 6u)) & 63u;
        nd[(st >> 2u) & 3u] += 1u;
        add[(st >> 4u) & 3u] += 1u;
      }
    }
    add[lb] += 1u;
  }
  var need = 0u; var gain = 0u;
  for (var i = 0u; i < 4u; i++) { need |= (nd[i] & 15u) << (4u * i); gain |= (add[i] & 63u) << (6u * i); }
  return vec2<u32>(need, gain);
}

// ---- the setup (strands_fire_setup) -----------------------------------------------------------------
/// The other end of the rule wire that has end e.
fn fr_partner(e: u32) -> u32 {
  for (var w = 0u; w < 9u; w++) {
    if (w < fr_fs_nw) {
      if (fr_fs_wa[w] == e) { return fr_fs_wb[w]; }
      if (fr_fs_wb[w] == e) { return fr_fs_wa[w]; }
    }
  }
  return 0xFFu;
}
/// A dying aux port, by index: the consumer's 1 and 2, then the producer's 1 and 2.
fn fr_aux_end(a: u32) -> u32 { return (select(1u, 2u, (a & 2u) != 0u) << 6u) | select(1u, 2u, (a & 1u) != 0u); }
fn fr_aux_index(e: u32) -> u32 { return ((e >> 6u) & 2u) | ((e >> 1u) & 1u); }
/// Orientation oi of a square: its faces fx and fy, in lattice.rs `regions` order.
fn fr_fx(oi: u32) -> u32 { if (oi < 4u) { return oi / 2u; } if (oi < 8u) { return (oi - 4u) / 2u; } return 2u + (oi - 8u) / 2u; }
fn fr_fy(oi: u32) -> u32 { if (oi < 4u) { return 2u + oi % 2u; } return 4u + oi % 2u; }

/// A rewrite's rule, its links between fresh ports and ends that stay, the candidate squares, and
/// the pairings the dying pair takes with it; true when it looked at a taken position (fs_stale).
fn fr_setup() -> bool {
  let kc = fr_fc_kc; let kp = fr_fc_kp; let via = fr_fc_via;
  let S = blk[p];
  let sp4 = inb(p, fr_fc_face); fr_fs_sp = select(p, sp4 & 7u, via);
  var SP = S; if (via) { SP = blk[nbq(p, fr_fc_face) & 7u]; }
  let ct = vgt(S, kc); let pt = vgt(SP, kp); fr_fs_ri = rule_of(ct, pt); fr_fs_n = rule_n(fr_fs_ri);
  fr_fs_lane = lane(vgm(S, ae(kc, 0u)));
  // rule_wires
  fr_fs_nw = rule_nw(fr_fs_ri);
  for (var w = 0u; w < 9u; w++) { fr_fs_wa[w] = rule_wa(fr_fs_ri, w); fr_fs_wb[w] = rule_wb(fr_fs_ri, w); }
  for (var w = 0u; w < 4u; w++) { fr_fs_part[w] = fr_partner(fr_aux_end(w)); }
  var dmc = array<u32, 4>(NONE, vgm(S, ae(kc, 1u)), vgm(S, ae(kc, 2u)), NONE);
  var dmp = array<u32, 4>(NONE, vgm(SP, ae(kp, 1u)), vgm(SP, ae(kp, 2u)), NONE);
  if (arity(ct) < 3u) { dmc[2] = NONE; } if (arity(ct) < 2u) { dmc[1] = NONE; }
  if (arity(pt) < 3u) { dmp[2] = NONE; } if (arity(pt) < 2u) { dmp[1] = NONE; }
  // Where the path from each dying aux port ends: through its mate while that is another dying
  // aux port, and on along that port's rule wire, to a fresh port or an end that stays.
  var term = array<u32, 4>(0u, 0u, 0u, 0u);
  for (var j = 0u; j < 4u; j++) {
    var e = fr_aux_end(j); var x = 0u;
    for (var it = 0u; it < 6u; it++) {
      if ((e >> 6u) == 0u) { x = e & 63u; break; }
      var m = dmp[e & 3u]; if ((e >> 6u) == 1u) { m = dmc[e & 3u]; }
      var d = 0xFFu;
      if (m < 9u) {
        let k = port_k(m); let q = port_q(m);
        if (!((e >> 6u) == 2u && via) && k == kc && q != 0u) { d = (1u << 6u) | q; }
        else if (((e >> 6u) == 2u || !via) && k == kp && q != 0u) { d = (2u << 6u) | q; }
      }
      if (d == 0xFFu) { x = 0x100u | select(0u, 0x80u, (e >> 6u) == 2u && via) | (m & 63u); break; }
      e = fr_fs_part[fr_aux_index(d)];
    }
    term[j] = x;
  }
  // Each rule wire becomes a link between its ends' terminals, once, in the rule's order.
  var wa: array<u32, 9>; var wb: array<u32, 9>;
  for (var w = 0u; w < 9u; w++) {
    let a = fr_fs_wa[w]; wa[w] = select(term[fr_aux_index(a)], a & 63u, (a >> 6u) == 0u);
    let b = fr_fs_wb[w]; wb[w] = select(term[fr_aux_index(b)], b & 63u, (b >> 6u) == 0u);
  }
  fr_fs_nl = 0u; fr_fs_la = array<u32, 9>(); fr_fs_lb = array<u32, 9>();
  for (var w = 0u; w < 9u; w++) {
    var dup = !(w < fr_fs_nw);
    for (var l = 0u; l < w; l++) {
      if ((wa[l] == wa[w] && wb[l] == wb[w]) || (wa[l] == wb[w] && wb[l] == wa[w])) { dup = true; }
    }
    if (!dup && fr_fs_nl < 9u) { fr_fs_la[fr_fs_nl] = wa[w]; fr_fs_lb[fr_fs_nl] = wb[w]; }
    fr_fs_nl = (fr_fs_nl + u32(!dup)) & 15u;
  }
  // The squares to try: the twelve orientations from a random one, in the simulator's order of
  // checks (off the lattice, the producer not in the square, then looking at each site).
  let start = pick3(d_region(), 12u);
  fr_fs_nreg = 0u; fr_fs_regs = array<u32, 3>(0u, 0u, 0u);
  var fs_stale = false;
  for (var j = 0u; j < 12u; j++) {
    let oi = (start + j) % 12u;
    let fx = fr_fx(oi); let fy = fr_fy(oi);
    let hx = inb(p, fx); let hy = inb(p, fy);
    var dd = 0u; if ((hx & 8u) != 0u) { dd = inb(hx & 7u, fy); }
    if (onlat(p, fx) && onlat(p, fy)
        && (fr_fs_sp == p || ((hx & 8u) != 0u && fr_fs_sp == (hx & 7u)) || ((hy & 8u) != 0u && fr_fs_sp == (hy & 7u)))) {
      if ((hx & 8u) == 0u) {}
      else if ((taken & (1u << (hx & 7u))) != 0u) { fs_stale = true; break; }
      else if ((hy & 8u) == 0u) {}
      else if ((taken & (1u << (hy & 7u))) != 0u) { fs_stale = true; break; }
      else if ((dd & 8u) == 0u) {}
      else if ((taken & (1u << (dd & 7u))) != 0u) { fs_stale = true; break; }
      else {
        if (fr_fs_nreg < 3u) { fr_fs_regs[fr_fs_nreg] = oi; }
        fr_fs_nreg = (fr_fs_nreg + 1u) & 3u;
      }
    }
  }
  // Pairings the dying pair takes with it, per site (a pairing between two dying ports once).
  fr_fs_lost_s = 0u; fr_fs_lost_sp = 0u;
  for (var j = 0u; j < 6u; j++) {
    let k = select(kp, kc, j < 3u); let q = j % 3u; let at_s = j < 3u || !via;
    var m = vgm(SP, ae(k, q)); if (at_s) { m = vgm(S, ae(k, q)); }
    var dying = m < 9u && port_k(m) == kp;
    if (at_s) { dying = m < 9u && (port_k(m) == kc || (!via && port_k(m) == kp)); }
    if (m != NONE && (!dying || m > ae(k, q))) {
      if (at_s) { fr_fs_lost_s = (fr_fs_lost_s + 1u) & 7u; } else { fr_fs_lost_sp = (fr_fs_lost_sp + 1u) & 7u; }
    }
  }
  return fs_stale;
}

// ---- one candidate square (strands_fire_prep) ---------------------------------------------------------
/// Square fr of the setup's: its sites, each site's seats and pairings left, each edge's free lanes.
fn fr_prep(fr: u32) {
  let oi = fr_fs_regs[fr];
  fr_fp_fx = fr_fx(oi); fr_fp_fy = fr_fy(oi);
  fr_fp_pos[0] = p; fr_fp_pos[1] = nbq(p, fr_fp_fx) & 7u; fr_fp_pos[2] = nbq(p, fr_fp_fy) & 7u;
  fr_fp_pos[3] = nbq(fr_fp_pos[1], fr_fp_fy) & 7u;
  fr_fp_ploc = select(select(2u, 1u, fr_fs_sp == fr_fp_pos[1]), 0u, fr_fs_sp == p);
  for (var l = 0u; l < 4u; l++) {
    let Q = blk[fr_fp_pos[l]]; fr_fp_q[l] = Q;
    // Slots for fresh agents: the dying producer's, the dying consumer's, then free ones.
    var n = 0u; var lst = 0u;
    if (l == fr_fp_ploc) { lst |= fr_fc_kp << (n * 2u); n++; }
    if (l == 0u) { lst |= fr_fc_kc << (n * 2u); n++; }
    for (var kk = 0u; kk < 2u; kk++) { if (vgt(Q, kk) == 0u) { lst |= kk << (n * 2u); n++; } }
    fr_fp_slots[l] = lst & 0xFFu; fr_fp_nslots[l] = n & 7u;
    fr_fp_left[l] = (vpairs(Q) - select(0u, fr_fs_lost_s, l == 0u) - select(0u, fr_fs_lost_sp, l != 0u && l == fr_fp_ploc)) & 31u;
  }
  fr_fp_avail[0] = (4u - vused_lanes(fr_fp_q[0], fr_fp_fx)) & 7u;
  fr_fp_avail[1] = (4u - vused_lanes(fr_fp_q[0], fr_fp_fy)) & 7u;
  fr_fp_avail[2] = (4u - vused_lanes(fr_fp_q[1], fr_fp_fy)) & 7u;
  fr_fp_avail[3] = (4u - vused_lanes(fr_fp_q[2], fr_fp_fx)) & 7u;
  // The strand between the pair comes free.
  if (fr_fc_via && fr_fp_ploc == 1u) { fr_fp_avail[0] = (fr_fp_avail[0] + 1u) & 7u; }
  if (fr_fc_via && fr_fp_ploc == 2u) { fr_fp_avail[1] = (fr_fp_avail[1] + 1u) & 7u; }
}

// ---- one seating (strands_fire_search) -----------------------------------------------------------------
/// Seating `code` (fresh agent i's square position in digit fs_n - 1 - i): its positions, 2 bits each.
fn fr_loc(code: u32) -> u32 {
  var loc = 0u;
  for (var i = 0u; i < 6u; i++) { if (i < fr_fs_n) { loc |= ((code >> ((fr_fs_n - 1u - i) * 2u)) & 3u) << (i * 2u); } }
  return loc;
}
/// Whether seating `code` fits the square: 32 | its cost in new strands, else 0.
fn fr_search(code: u32) -> u32 {
  let loc = fr_loc(code);
  var cnt = array<u32, 4>(0u, 0u, 0u, 0u);
  for (var i = 0u; i < 6u; i++) { if (i < fr_fs_n) { let l = (loc >> (i * 2u)) & 3u; cnt[l] = (cnt[l] + 1u) & 7u; } }
  for (var l = 0u; l < 4u; l++) { if (cnt[l] > fr_fp_nslots[l]) { return 0u; } }
  let na = fr_need_add(loc);
  var cost = 0u; var ok = true;
  for (var l = 0u; l < 4u; l++) {
    let nd = (na.x >> (4u * l)) & 15u; let add = (na.y >> (6u * l)) & 63u;
    cost += nd;
    ok = ok && nd <= fr_fp_avail[l] && fr_fp_left[l] + add <= PAIRS;
  }
  return select(0u, 32u | (cost & 31u), ok);
}

// ---- the working copy (strands_fire_init) and its links (strands_fire_link) ------------------------------
/// The lowest free lanes of a face, as many as needed and found: {count, lanes (2 bits each)}.
fn fr_lanes_for(used: u32, need: u32) -> u32 {
  var n = 0u; var ls = 0u;
  for (var i = 0u; i < 4u; i++) { if (((used >> i) & 1u) == 0u && n < need) { ls |= i << (n * 2u); n++; } }
  return (n << 8u) | ls;
}
/// A rewrite's working copy of its square: the strand between the pair freed, the pair removed,
/// the fresh agents seated, each edge's lanes for new strands chosen.
fn fr_init(best_code: u32) {
  let n = fr_fs_n; let via = fr_fc_via; let ploc = fr_fp_ploc;
  let loc = fr_loc(best_code);
  // Seats: each fresh agent takes the next slot of its site.
  var seat = 0u; var tk = 0u;
  for (var i = 0u; i < 6u; i++) {
    if (i < n) {
      let la = (loc >> (i * 2u)) & 3u; let sl = (tk >> (la * 2u)) & 3u;
      seat |= ((fr_fp_slots[la] >> (sl * 2u)) & 3u) << (i * 2u);
      tk = (tk & ~(3u << (la * 2u))) | (((sl + 1u) & 3u) << (la * 2u));
    }
  }
  var tch = 1u | (1u << ploc);
  for (var i = 0u; i < 6u; i++) { if (i < n) { tch |= 1u << ((loc >> (i * 2u)) & 3u); } }
  let bneed = fr_need_add(loc).x;
  let wanted = rule_wanted(fr_fs_ri);
  for (var l = 0u; l < 4u; l++) {
    var Y = fr_fp_q[l];
    // The strand between the pair comes free.
    Y = vsm(Y, only(via && l == 0u, se(fr_fc_face, fr_fs_lane)), NONE);
    Y = vsm(Y, only(via && l == ploc, se(fr_fc_face ^ 1u, fr_fs_lane)), NONE);
    // Lanes for new strands, per edge (fx and fy from here, fy from the fx neighbour, fx from
    // the fy neighbour): the lowest free ones, used from the highest down.
    if (l == 0u) {
      var fl = fr_lanes_for(vlanes_used(Y, fr_fp_fx), bneed & 15u); fr_alanes[0] = fl & 0xFFu; fr_aptr[0] = fl >> 8u;
      fl = fr_lanes_for(vlanes_used(Y, fr_fp_fy), (bneed >> 4u) & 15u); fr_alanes[1] = fl & 0xFFu; fr_aptr[1] = fl >> 8u;
    }
    if (l == 1u) { let fl = fr_lanes_for(vlanes_used(Y, fr_fp_fy), (bneed >> 8u) & 15u); fr_alanes[2] = fl & 0xFFu; fr_aptr[2] = fl >> 8u; }
    if (l == 2u) { let fl = fr_lanes_for(vlanes_used(Y, fr_fp_fx), (bneed >> 12u) & 15u); fr_alanes[3] = fl & 0xFFu; fr_aptr[3] = fl >> 8u; }
    // The pair goes: its ports and slots empty.
    for (var e = 0u; e < 9u; e++) {
      if ((l == 0u && e / 3u == fr_fc_kc) || (l == ploc && e / 3u == fr_fc_kp)) { Y = vsm(Y, e, NONE); }
    }
    for (var e = 0u; e < 3u; e++) {
      if ((l == 0u && e == fr_fc_kc) || (l == ploc && e == fr_fc_kp)) { Y = vstg(Y, e, 0u); Y = vsw(Y, e, false); }
    }
    // The fresh agents take their seats.
    for (var i = 0u; i < 6u; i++) {
      if (i < n && ((loc >> (i * 2u)) & 3u) == l) {
        let kk = (seat >> (i * 2u)) & 3u; let t = rule_fresh(fr_fs_ri, i);
        Y = vstg(Y, kk, t); Y = vsw(Y, kk, born_wanted(t) || (LAZY && ((wanted >> i) & 1u) != 0u));
      }
    }
    fr_aq[l] = Y;
  }
  fr_aloc = loc; fr_aseat = seat; fr_atch = tch;
}
/// Link aj of a rewrite, wired along its route through the working copy.
fn fr_link(aj: u32) {
  let ta = fr_fs_la[aj]; let tb = fr_fs_lb[aj];
  let la = fr_at(ta, fr_aloc); let lb = fr_at(tb, fr_aloc);
  let ea = fr_end(ta, fr_aseat); let eb = fr_end(tb, fr_aseat);
  // The route's steps (none if both ends share a site), each taking its edge's next lane.
  let r = fr_route(la, lb); let nst = select(r >> 12u, 0u, la == lb); let x1 = (r >> 10u) & 3u;
  let f0 = fr_dir_face(r & 3u); let f1 = fr_dir_face((r >> 6u) & 3u);
  var l0 = 0u; var l1 = 0u;
  for (var si = 0u; si < 2u; si++) {
    if (si < nst) {
      let ed = (r >> (si * 6u + 2u)) & 3u;
      fr_aptr[ed] = (fr_aptr[ed] - 1u) & 7u;
      let ln = (fr_alanes[ed] >> (fr_aptr[ed] * 2u)) & 3u;
      if (si == 0u) { l0 = ln; } else { l1 = ln; }
    }
  }
  // Each site on the route links one pair of ends: the start, the site passed through, the end.
  for (var l = 0u; l < 4u; l++) {
    var a = NONE; var b = NONE;
    if (l == la) { a = ea; b = select(se(f0, l0), eb, nst == 0u); fr_atch |= 1u << l; }
    else if (nst == 2u && l == x1) { a = se(f0 ^ 1u, l0); b = se(f1, l1); fr_atch |= 1u << l; }
    else if (l == lb) { a = select(se(f1 ^ 1u, l1), se(f0 ^ 1u, l0), nst == 1u); b = eb; fr_atch |= 1u << l; }
    fr_aq[l] = vlk(fr_aq[l], a, b);
  }
}

// ---- the sequence (strands_block.v S_F_SETUP .. S_F_WB) ----------------------------------------------------
/// The rewrite of the pair at p (consumer in slot kc, producer in slot kp, here or across face
/// fc_face): 0 when no square or seating fits, 1 when it fired, 2 when it looked at a taken position.
fn fire(kc: u32, kp: u32, via: bool, fc_face: u32) -> u32 {
  fr_fc_kc = kc; fr_fc_kp = kp; fr_fc_via = via; fr_fc_face = fc_face;
  if (fr_setup()) { stale = true; return 2u; }
  if (fr_fs_nreg == 0u) { return 0u; }
  // Per square, the seatings in order: the first free of new strands, else the cheapest, ties to
  // the earliest.
  let code_last = ((1u << (2u * fr_fs_n)) - 1u) & 0xFFFu;
  var fr = 0u; var best_code = 0u;
  loop {
    fr_prep(fr);
    var best_valid = false; var best_cost = 0u;
    for (var code = 0u; code <= code_last; code++) {
      let sr = fr_search(code); let sr_ok = sr != 0u; let sr_cost = sr & 31u;
      if (sr_ok && (!best_valid || sr_cost < best_cost)) { best_valid = true; best_cost = sr_cost; best_code = code; }
      if (sr_ok && sr_cost == 0u) { break; }
    }
    if (best_valid) { break; }
    if (fr + 1u < fr_fs_nreg) { fr++; } else { return 0u; }
  }
  fr_init(best_code);
  for (var aj = 0u; aj < fr_fs_nl; aj++) { fr_link(aj); }
  touched = 0u;
  for (var l = 0u; l < 4u; l++) {
    if (((fr_atch >> l) & 1u) != 0u) { blk[fr_fp_pos[l]] = fr_aq[l]; touched |= 1u << fr_fp_pos[l]; }
  }
  return 1u;
}
