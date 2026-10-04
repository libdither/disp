// The stages of a turn, as combinational modules of the block unit (strands_block.v). Each mirrors
// the function of the same name in crate/src/lattice.rs. A stage sees the turn's site (here) and
// up to three more (there), at the positions it names (x_rd) from what it sees here.

// An eraser at the turn's site touching garbage collects it (lattice.rs `collect`,
// `erase_inputs`, `erase_unpair`). One eraser per cycle: the first, then (c_again) the second.
module strands_collect (
  input wire [218:0] here,
  input wire [3*219-1:0] there,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [47:0] lat,
  input wire c_second,
  output reg c_apply, c_stale, c_two, c_again,
  output reg [7:0] c_touched,
  output reg [2:0] c_dpos,
  output reg [218:0] c_s, c_d,
  output reg [8:0] c_rd
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  // Which eraser looks this cycle, and the site its strand leads to (read through there).
  reg [1:0] ke; reg e0, e1;
  always @* begin : collect_where
    reg [3:0] n;
    e0 = gt(here, 0) == T_EPS && gm(here, ae(0, 0)) != NONE; e1 = gt(here, 1) == T_EPS && gm(here, ae(1, 0)) != NONE;
    ke = c_second || !e0 ? 2'd1 : 2'd0;
    n = nbq(p, face(gm(here, ae(ke, 0)))); c_rd = {6'd0, n[2:0]};
  end
  always @* begin : collect_stage
    reg [SW-1:0] S, D; reg [5:0] m, md, other, input_, mm, m0, m1; reg [3:0] t, td, nn;
    reg [2:0] dpos, s2pos, f, f2; reg [1:0] i, i2, kd, q, k2; reg via, via2, same, same2, done, ok, ok2;
    integer pp;
    S = 0; D = 0; m = 0; md = 0; other = 0; input_ = 0; mm = 0; m0 = 0; m1 = 0; t = 0; td = 0; nn = 0; dpos = 0; s2pos = 0; f = 0; f2 = 0; i = 0; i2 = 0; kd = 0; q = 0; k2 = 0; via = 0; via2 = 0; same = 0; same2 = 0; done = 0; ok = 0; ok2 = 0;
    c_apply = 0; c_stale = 0; c_touched = 0; done = 0; c_two = 0; c_dpos = 0; c_s = 0; c_d = 0;
    S = here; D = S;
    begin
      t = gt(S, ke); m = gm(S, ae(ke, 0));
      ok = t == T_EPS && m != NONE; via = 0; dpos = p; md = m; f = 0; i = 0;
      if (ok && is_strand(m)) begin
        f = face(m); i = lane(m); nn = inb(p, f);
        if (!nn[3]) ok = 0;
        else if (taken[nn[2:0]]) begin ok = 0; c_stale = 1; done = 1; end
        else begin via = 1; dpos = nn[2:0]; D = there[0 +: SW]; md = gm(D, se(f ^ 3'd1, i)); end
      end
      same = !via;
      if (ok && md != NONE && !is_strand(md)) begin
        kd = port_k(md); q = port_q(md); td = gt(D, kd);
        if (td != 0 && (td == T_A || td == T_T1 || td == T_SEL) && q == 2'd2) begin
          // erase_inputs: the dead computation's two inputs each get an eraser.
          m0 = gm(D, ae(kd, 0)); m1 = gm(D, ae(kd, 1));
          S = sm(S, ae(ke, 0), NONE); if (same) D = S;
          for (pp = 0; pp < 3; pp = pp + 1) D = sm(D, ae(kd, pp), NONE);
          if (same) S = D;
          S = stg(S, ke, 4'd0); S = sw(S, ke, 1'b0); if (same) D = S;
          D = stg(D, kd, 4'd0); D = sw(D, kd, 1'b0);
          D = stg(D, kd, T_EPS); D = sw(D, kd, 1'b1);
          D = lk(D, ae(kd, 0), m1);
          if (same) S = D;
          S = stg(S, ke, T_EPS); S = sw(S, ke, 1'b1);
          if (same) begin S = lk(S, ae(ke, 0), m0); D = S; end
          else begin S = lk(S, ae(ke, 0), se(f, i)); D = lk(D, se(f ^ 3'd1, i), m0); end
          c_apply = 1; done = 1;
        end
        if (!done && td == T_UNP && q != 2'd0) begin
          // erase_unpair: both outputs being erased, the unpair and both erasers become one eraser.
          other = gm(D, ae(kd, 2'd3 - q)); ok2 = 1; via2 = 0; s2pos = dpos; k2 = 0; f2 = 0; i2 = 0;
          if (other != NONE && !is_strand(other)) begin s2pos = dpos; k2 = port_k(other); end
          else if (is_strand(other) && nbq(dpos, face(other)) == {1'b1, p}) begin
            f2 = face(other); i2 = lane(other); mm = gm(S, se(f2 ^ 3'd1, i2));
            if (mm == NONE || is_strand(mm)) ok2 = 0; else begin s2pos = p; k2 = port_k(mm); via2 = 1; end
          end else ok2 = 0;
          same2 = s2pos == p;
          if (ok2 && (gt(same2 ? S : D, k2) != T_EPS || (same2 && k2 == ke))) ok2 = 0;
          if (ok2) begin
            m0 = gm(D, ae(kd, 0));
            S = sm(S, only(via, se(f, i)), NONE); D = sm(D, only(via, se(f ^ 3'd1, i)), NONE);
            D = sm(D, only(via2, se(f2, i2)), NONE); S = sm(S, only(via2, se(f2 ^ 3'd1, i2)), NONE);
            S = sm(S, ae(ke, 0), NONE); S = stg(S, ke, 4'd0); S = sw(S, ke, 1'b0); if (same) D = S;
            if (same2) begin S = sm(S, ae(k2, 0), NONE); S = stg(S, k2, 4'd0); S = sw(S, k2, 1'b0); if (same) D = S; end
            else begin D = sm(D, ae(k2, 0), NONE); D = stg(D, k2, 4'd0); D = sw(D, k2, 1'b0); end
            for (pp = 0; pp < 3; pp = pp + 1) D = sm(D, ae(kd, pp), NONE);
            D = stg(D, kd, 4'd0); D = sw(D, kd, 1'b0);
            D = stg(D, kd, T_EPS); D = sw(D, kd, 1'b1);
            D = lk(D, ae(kd, 0), m0);
            if (same) S = D;
            c_apply = 1; done = 1;
          end
        end
        if (!done && td == T_DN && q != 2'd0) begin
          // A duplicator read by an eraser becomes a plain wire from its input to its other output.
          input_ = gm(D, ae(kd, 0)); other = gm(D, ae(kd, 2'd3 - q));
          if (input_ != ae(kd, 2'd3 - q)) begin
            S = sm(S, only(via, se(f, i)), NONE); D = sm(D, only(via, se(f ^ 3'd1, i)), NONE);
            S = sm(S, ae(ke, 0), NONE); if (same) D = S;
            for (pp = 0; pp < 3; pp = pp + 1) D = sm(D, ae(kd, pp), NONE);
            D = lk(D, input_, other);
            if (same) S = D;
            S = stg(S, ke, 4'd0); S = sw(S, ke, 1'b0); if (same) D = S;
            D = stg(D, kd, 4'd0); D = sw(D, kd, 1'b0);
            if (same) S = D;
            c_apply = 1; done = 1;
          end
        end
      end
      if (c_apply) begin
        c_s = S; c_touched[p] = 1'b1;
        if (!same) begin c_d = D; c_dpos = dpos; c_two = 1; c_touched[dpos] = 1'b1; end
      end
    end
    c_again = ke == 0 && e1 && !c_apply && !c_stale;
  end


endmodule

// A consumer at site ap_pos whose partner is here or one strand away (lattice.rs `active_pair`).
module strands_pair (
  input wire [218:0] here,
  input wire [3*219-1:0] there,
  input wire [2:0] ap_pos,
  input wire [7:0] taken,
  input wire [47:0] lat,
  output reg ap_found, ap_stale, ap_via, ap_inf,
  output reg [1:0] ap_kc, ap_kp, ap_inf_k,
  output reg [2:0] ap_face, ap_inf_pos,
  output reg [8:0] ap_rd
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  // The site each slot's principal strand leads to (read through there, one per slot).
  always @* begin : pair_where
    reg [3:0] n; integer k;
    for (k = 0; k < 3; k = k + 1) begin n = nbq(ap_pos, face(gm(here, ae(k, 0)))); ap_rd[k*3 +: 3] = n[2:0]; end
  end
  always @* begin : pair_stage
    reg [SW-1:0] S, N; reg [5:0] m, mm; reg [3:0] t, t2, nn; reg [2:0] sp, f; reg [1:0] k2, q; reg stop, skip;
    integer k;
    S = 0; N = 0; m = 0; mm = 0; t = 0; t2 = 0; nn = 0; sp = 0; f = 0; k2 = 0; q = 0; stop = 0; skip = 0;
    ap_found = 0; ap_stale = 0; ap_via = 0; ap_inf = 0; ap_kc = 0; ap_kp = 0; ap_face = 0; ap_inf_pos = 0; ap_inf_k = 0; stop = 0;
    S = here;
    for (k = 0; k < 3; k = k + 1) if (!stop) begin
      t = gt(S, k); m = gm(S, ae(k, 0)); skip = 0; sp = ap_pos; N = S; mm = m; f = 0;
      if (t == 0 || !is_consumer(t) || (LAZY && !gw(S, k)) || m == NONE) skip = 1;
      if (!skip && is_strand(m)) begin
        f = face(m); nn = inb(ap_pos, f);
        if (!nn[3]) skip = 1;
        else if (taken[nn[2:0]]) begin ap_stale = 1; stop = 1; skip = 1; end
        else begin sp = nn[2:0]; N = there[k*SW +: SW]; mm = gm(N, se(f ^ 3'd1, lane(m))); end
      end
      if (!skip && !is_strand(mm) && mm != NONE) begin
        k2 = port_k(mm); q = port_q(mm); t2 = gt(N, k2);
        if (q == 0 && t2 != 0 && is_producer(t2)) begin
          ap_found = 1; stop = 1; ap_kc = k; ap_kp = k2; ap_via = is_strand(m); ap_face = f;
        end else if (LAZY && q != 0 && t2 != 0 && is_consumer(t2) && t != T_EPS) begin
          ap_inf = 1; ap_inf_pos = sp; ap_inf_k = k2;
        end
      end
    end
  end


endmodule

// A rewrite's rule, its links between fresh ports and ends that stay, the candidate squares, and
// the pairings the dying pair takes with it (lattice.rs `fire`, `regions`).
module strands_fire_setup (
  input wire [218:0] here,
  input wire [3*219-1:0] there,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [47:0] lat,
  input wire [63:0] dice,
  input wire [1:0] fc_kc, fc_kp,
  input wire fc_via,
  input wire [2:0] fc_face,
  output reg [4:0] fs_ri,
  output reg [2:0] fs_n,
  output reg [3:0] fs_nl,
  output reg [9*9-1:0] fs_la, fs_lb,
  output reg [2:0] fs_sp,
  output reg [1:0] fs_lane,
  output reg [1:0] fs_nreg,
  output reg [11:0] fs_regs,
  output reg fs_stale,
  output reg [2:0] fs_lost_s, fs_lost_sp,
  output wire [8:0] fs_rd
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  wire [15:0] d_metro = dice[15:0]; wire [7:0] d_hop = dice[31:24], d_along = dice[39:32], d_swap = dice[47:40];
  wire [4:0] d_agent = dice[52:48], d_where = dice[57:53]; wire [2:0] d_resident = dice[60:58], d_region = dice[63:61];
  // The producer's site, when it is across the principal strand (read through there).
  wire [3:0] fs_nb = nbq(p, fc_face);
  assign fs_rd = {6'd0, fs_nb[2:0]};
  // The rule's wires, read from the table once, and the other end of each dying aux port's wire.
  reg [71:0] fs_wa, fs_wb; reg [3:0] fs_nw; reg [31:0] fs_part;
  /// The other end of the rule wire that has end e.
  function [7:0] partner(input [7:0] e);
    integer w; reg found;
    begin
      partner = 8'hFF; found = 0;
      for (w = 0; w < 9; w = w + 1) if (!found && w < fs_nw) begin
        if (fs_wa[w*8 +: 8] == e) begin partner = fs_wb[w*8 +: 8]; found = 1; end
        else if (fs_wb[w*8 +: 8] == e) begin partner = fs_wa[w*8 +: 8]; found = 1; end
      end
    end
  endfunction
  /// A dying aux port, by index: the consumer's 1 and 2, then the producer's 1 and 2.
  function [7:0] aux_end(input [1:0] a); aux_end = {a[1] ? 2'd2 : 2'd1, 4'd0, a[0] ? 2'd2 : 2'd1}; endfunction
  function [1:0] aux_index(input [7:0] e); aux_index = {e[7], e[1]}; endfunction
  always @* begin : rule_wires
    integer w;
    fs_nw = rule_nw(fs_ri);
    for (w = 0; w < 9; w = w + 1) begin fs_wa[w*8 +: 8] = rule_wa(fs_ri, w); fs_wb[w*8 +: 8] = rule_wb(fs_ri, w); end
    for (w = 0; w < 4; w = w + 1) fs_part[w*8 +: 8] = partner(aux_end(w));
  end
  always @* begin : fire_setup
    reg [SW-1:0] S, SP; reg [3:0] ct, pt, sp4, hx, hy, dd; reg [7:0] e, d; reg [5:0] m; reg [8:0] ta, tb, x;
    reg [17:0] dmc, dmp; reg fin, dup, stop, at_s, dying; reg [1:0] k, q; reg [3:0] start; reg [2:0] fx, fy; reg [35:0] term; reg [80:0] wa, wb;
    integer w, it, l, j, oi;
    term = 0; wa = 0; wb = 0; S = 0; SP = 0; ct = 0; pt = 0; sp4 = 0; hx = 0; hy = 0; dd = 0; e = 0; d = 0; m = 0; ta = 0; tb = 0; x = 0; dmc = 0; dmp = 0; fin = 0; dup = 0; stop = 0; at_s = 0; dying = 0; k = 0; q = 0; start = 0; fx = 0; fy = 0;
    S = here;
    sp4 = inb(p, fc_face); fs_sp = fc_via ? sp4[2:0] : p;
    SP = fc_via ? there[0 +: SW] : S;
    ct = gt(S, fc_kc); pt = gt(SP, fc_kp); fs_ri = rule_of(ct, pt); fs_n = rule_n(fs_ri);
    fs_lane = lane(gm(S, ae(fc_kc, 0)));
    dmc = {gm(S, ae(fc_kc, 2)), gm(S, ae(fc_kc, 1)), NONE};
    dmp = {gm(SP, ae(fc_kp, 2)), gm(SP, ae(fc_kp, 1)), NONE};
    if (arity(ct) < 3) dmc[17:12] = NONE; if (arity(ct) < 2) dmc[11:6] = NONE;
    if (arity(pt) < 3) dmp[17:12] = NONE; if (arity(pt) < 2) dmp[11:6] = NONE;
    // Where the path from each dying aux port ends: through its mate while that is another dying
    // aux port, and on along that port's rule wire, to a fresh port or an end that stays.
    for (j = 0; j < 4; j = j + 1) begin
      e = aux_end(j); fin = 0; x = 0;
      for (it = 0; it < 6; it = it + 1) if (!fin) begin
        if (e[7:6] == 2'd0) begin x = {3'b000, e[5:0]}; fin = 1; end
        else begin
          m = e[7:6] == 2'd1 ? dmc[e[1:0]*6 +: 6] : dmp[e[1:0]*6 +: 6];
          d = 8'hFF;
          if (m < 6'd9) begin
            k = port_k(m); q = port_q(m);
            if (!(e[7:6] == 2'd2 && fc_via) && k == fc_kc && q != 0) d = {2'd1, 3'd0, 1'b0, q};
            else if ((e[7:6] == 2'd2 || !fc_via) && k == fc_kp && q != 0) d = {2'd2, 3'd0, 1'b0, q};
          end
          if (d == 8'hFF) begin x = {1'b1, e[7:6] == 2'd2 && fc_via, 1'b0, m}; fin = 1; end
          else e = fs_part[aux_index(d)*8 +: 8];
        end
      end
      term[j*9 +: 9] = x;
    end
    // Each rule wire becomes a link between its ends' terminals, once, in the rule's order.
    for (w = 0; w < 9; w = w + 1) begin
      e = fs_wa[w*8 +: 8]; wa[w*9 +: 9] = e[7:6] == 2'd0 ? {3'b000, e[5:0]} : term[aux_index(e)*9 +: 9];
      e = fs_wb[w*8 +: 8]; wb[w*9 +: 9] = e[7:6] == 2'd0 ? {3'b000, e[5:0]} : term[aux_index(e)*9 +: 9];
    end
    fs_nl = 0; fs_la = 0; fs_lb = 0;
    for (w = 0; w < 9; w = w + 1) begin
      dup = !(w < fs_nw);
      for (l = 0; l < w; l = l + 1)
        if ((wa[l*9 +: 9] == wa[w*9 +: 9] && wb[l*9 +: 9] == wb[w*9 +: 9]) || (wa[l*9 +: 9] == wb[w*9 +: 9] && wb[l*9 +: 9] == wa[w*9 +: 9])) dup = 1;
      for (l = 0; l < 9; l = l + 1) if (!dup && fs_nl == l) begin fs_la[l*9 +: 9] = wa[w*9 +: 9]; fs_lb[l*9 +: 9] = wb[w*9 +: 9]; end
      fs_nl = fs_nl + !dup;
    end
    // The squares to try: the twelve orientations from a random one, in the simulator's order of
    // checks (off the lattice, the producer not in the square, then looking at each site).
    start = pick3(d_region, 4'd12);
    fs_nreg = 0; fs_regs = 0; fs_stale = 0; stop = 0;
    for (j = 0; j < 12; j = j + 1) if (!stop) begin
      oi = (start + j) % 12;
      fx = oi < 4 ? oi / 2 : oi < 8 ? (oi - 4) / 2 : 2 + (oi - 8) / 2;
      fy = oi < 4 ? 2 + oi % 2 : 4 + oi % 2;
      hx = inb(p, fx); hy = inb(p, fy); dd = hx[3] ? inb(hx[2:0], fy) : 4'd0;
      if (onlat(p, fx) && onlat(p, fy) && (fs_sp == p || (hx[3] && fs_sp == hx[2:0]) || (hy[3] && fs_sp == hy[2:0]))) begin
        if (!hx[3]) ;
        else if (taken[hx[2:0]]) begin fs_stale = 1; stop = 1; end
        else if (!hy[3]) ;
        else if (taken[hy[2:0]]) begin fs_stale = 1; stop = 1; end
        else if (!dd[3]) ;
        else if (taken[dd[2:0]]) begin fs_stale = 1; stop = 1; end
        else begin for (l = 0; l < 3; l = l + 1) if (l == fs_nreg) fs_regs[l*4 +: 4] = oi; fs_nreg = fs_nreg + 1; end
      end
    end
    // Pairings the dying pair takes with it, per site (a pairing between two dying ports once).
    fs_lost_s = 0; fs_lost_sp = 0;
    for (j = 0; j < 6; j = j + 1) begin
      k = j < 3 ? fc_kc : fc_kp; q = j % 3; at_s = j < 3 || !fc_via;
      m = gm(at_s ? S : SP, ae(k, q));
      dying = m < 6'd9 && (at_s ? (port_k(m) == fc_kc || (!fc_via && port_k(m) == fc_kp)) : port_k(m) == fc_kp);
      if (m != NONE && (!dying || m > ae(k, q))) begin
        if (at_s) fs_lost_s = fs_lost_s + 1; else fs_lost_sp = fs_lost_sp + 1;
      end
    end
  end


endmodule

// One candidate square of a rewrite: its sites, each site's seats and pairings left, each edge's
// free lanes.
module strands_fire_prep (
  input wire [218:0] here,
  input wire [3*219-1:0] there,
  input wire [2:0] p,
  input wire [47:0] lat,
  input wire [1:0] fr,
  input wire [11:0] fs_regs,
  input wire [2:0] fs_sp,
  input wire [1:0] fc_kc, fc_kp,
  input wire fc_via,
  input wire [2:0] fs_lost_s, fs_lost_sp,
  output reg [2:0] fp_fx, fp_fy,
  output reg [11:0] fp_pos,
  output reg [1:0] fp_ploc,
  output reg [31:0] fp_slots,
  output reg [11:0] fp_nslots, fp_avail,
  output reg [19:0] fp_left,
  output reg [4*219-1:0] fp_q,
  output wire [8:0] fp_rd
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  // The square: its orientation and sites (the three besides this one read through there).
  always @* begin : prep_where
    reg [3:0] h, oi;
    oi = fs_regs[fr*4 +: 4];
    fp_fx = oi < 4 ? oi / 2 : oi < 8 ? (oi - 4) / 2 : 2 + (oi - 8) / 2;
    fp_fy = oi < 4 ? 2 + oi % 2 : 4 + oi % 2;
    fp_pos[2:0] = p; h = nbq(p, fp_fx); fp_pos[5:3] = h[2:0]; h = nbq(p, fp_fy); fp_pos[8:6] = h[2:0];
    h = nbq(fp_pos[5:3], fp_fy); fp_pos[11:9] = h[2:0];
    fp_ploc = fs_sp == p ? 2'd0 : fs_sp == fp_pos[5:3] ? 2'd1 : 2'd2;
  end
  assign fp_rd = fp_pos[11:3];
  always @* begin : fire_prep
    reg [SW-1:0] Q; integer l, kk; reg [2:0] n; reg [7:0] lst;
    Q = 0; n = 0; lst = 0;
    fp_slots = 0; fp_nslots = 0; fp_left = 0; fp_q = 0;
    for (l = 0; l < 4; l = l + 1) begin
      Q = l == 0 ? here : there[(l-1)*SW +: SW]; fp_q[l*SW +: SW] = Q;
      // Slots for fresh agents: the dying producer's, the dying consumer's, then free ones.
      n = 0; lst = 0;
      if (l == fp_ploc) begin lst[n*2 +: 2] = fc_kp; n = n + 1; end
      if (l == 0) begin lst[n*2 +: 2] = fc_kc; n = n + 1; end
      for (kk = 0; kk < 2; kk = kk + 1) if (gt(Q, kk) == 0) begin lst[n*2 +: 2] = kk; n = n + 1; end
      fp_slots[l*8 +: 8] = lst; fp_nslots[l*3 +: 3] = n;
      fp_left[l*5 +: 5] = pairs(Q) - (l == 0 ? fs_lost_s : 0) - (l != 0 && l == fp_ploc ? fs_lost_sp : 0);
    end
    fp_avail[2:0]  = 3'd4 - used_lanes(fp_q[0 +: SW], fp_fx);
    fp_avail[5:3]  = 3'd4 - used_lanes(fp_q[0 +: SW], fp_fy);
    fp_avail[8:6]  = 3'd4 - used_lanes(fp_q[SW +: SW], fp_fy);
    fp_avail[11:9] = 3'd4 - used_lanes(fp_q[2*SW +: SW], fp_fx);
    // The strand between the pair comes free.
    if (fc_via && fp_ploc == 2'd1) fp_avail[2:0] = fp_avail[2:0] + 3'd1;
    if (fc_via && fp_ploc == 2'd2) fp_avail[5:3] = fp_avail[5:3] + 3'd1;
  end


endmodule

// Whether one seating of a rewrite's fresh agents fits the square, and its cost in new strands.
module strands_fire_search (
  input wire [11:0] code,
  input wire [2:0] fs_n,
  input wire [3:0] fs_nl,
  input wire [9*9-1:0] fs_la, fs_lb,
  input wire [1:0] fp_ploc,
  input wire [2:0] fp_fx, fp_fy,
  input wire [11:0] fp_nslots, fp_avail,
  input wire [19:0] fp_left,
  output reg sr_ok,
  output reg [4:0] sr_cost,
  output reg [11:0] sr_loc,
  output reg [15:0] sr_need
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_route.vh"
  /* verilator no_inline_module */
  always @* begin : fire_search
    reg [2:0] cnt0, cnt1, cnt2, cnt3; reg [39:0] na; integer i;
    cnt0 = 0; cnt1 = 0; cnt2 = 0; cnt3 = 0; na = 0;
    sr_loc = 0;
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) sr_loc[i*2 +: 2] = code[(fs_n - 1 - i)*2 +: 2];
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) case (sr_loc[i*2 +: 2])
      2'd0: cnt0 = cnt0 + 1; 2'd1: cnt1 = cnt1 + 1; 2'd2: cnt2 = cnt2 + 1; default: cnt3 = cnt3 + 1; endcase
    na = need_add(sr_loc);
    sr_need = na[15:0];
    sr_cost = na[3:0] + na[7:4] + na[11:8] + na[15:12];
    sr_ok = cnt0 <= fp_nslots[2:0] && cnt1 <= fp_nslots[5:3] && cnt2 <= fp_nslots[8:6] && cnt3 <= fp_nslots[11:9]
         && na[3:0] <= fp_avail[2:0] && na[7:4] <= fp_avail[5:3] && na[11:8] <= fp_avail[8:6] && na[15:12] <= fp_avail[11:9]
         && fp_left[4:0] + na[21:16] <= PAIRS && fp_left[9:5] + na[27:22] <= PAIRS && fp_left[14:10] + na[33:28] <= PAIRS && fp_left[19:15] + na[39:34] <= PAIRS;
  end

endmodule

// A rewrite's working copy of its square: the strand between the pair freed, the pair removed,
// the fresh agents seated, each edge's lanes for new strands chosen. Site by site: the pair's
// ports and slots, the freed strand's ends and the fresh agents' slots are the only changes.
module strands_fire_init (
  input wire [4*219-1:0] fp_q,
  input wire [1:0] fp_ploc,
  input wire [2:0] fp_fx, fp_fy,
  input wire [31:0] fp_slots,
  input wire [2:0] fs_n,
  input wire [3:0] fs_nl,
  input wire [9*9-1:0] fs_la, fs_lb,
  input wire [4:0] fs_ri,
  input wire [1:0] fs_lane,
  input wire [1:0] fc_kc, fc_kp,
  input wire fc_via,
  input wire [2:0] fc_face,
  input wire [11:0] best_code,
  output reg [4*219-1:0] i_q,
  output reg [11:0] i_loc, i_seat,
  output reg [31:0] i_lanes,
  output reg [11:0] i_ptr,
  output reg [3:0] i_tch
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_route.vh"
  /* verilator no_inline_module */
  /// The lowest free lanes of a face, as many as needed and found: {count, lanes (2 bits each)}.
  function [10:0] lanes_for(input [3:0] used, input [3:0] need);
    integer i; reg [2:0] n; reg [7:0] ls;
    begin n = 0; ls = 0; for (i = 0; i < 4; i = i + 1) if (!used[i] && n < need) begin ls[n*2 +: 2] = i; n = n + 1; end lanes_for = {n, ls}; end
  endfunction
  always @* begin : fire_init
    reg [SW-1:0] Y; reg [7:0] tk; reg [3:0] t; reg [2:0] sl; reg [1:0] la, kk; reg [5:0] wanted; reg [39:0] bneed; reg [10:0] fl;
    integer i, l, e;
    Y = 0; tk = 0; t = 0; sl = 0; la = 0; kk = 0; wanted = 0; bneed = 0; fl = 0;
    i_loc = 0;
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) i_loc[i*2 +: 2] = best_code[(fs_n - 1 - i)*2 +: 2];
    // Seats: each fresh agent takes the next slot of its site.
    i_seat = 0;
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) begin
      la = i_loc[i*2 +: 2]; sl = tk[la*2 +: 2];
      i_seat[i*2 +: 2] = fp_slots[la*8 + sl*2 +: 2];
      tk[la*2 +: 2] = sl + 1;
    end
    i_tch = 4'b0001; i_tch[fp_ploc] = 1'b1;
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) i_tch[i_loc[i*2 +: 2]] = 1'b1;
    bneed = need_add(i_loc); i_lanes = 0; i_ptr = 0; wanted = rule_wanted(fs_ri);
    for (l = 0; l < 4; l = l + 1) begin
      Y = fp_q[l*SW +: SW];
      // The strand between the pair comes free.
      Y = sm(Y, only(fc_via && l == 0, se(fc_face, fs_lane)), NONE);
      Y = sm(Y, only(fc_via && l == fp_ploc, se(fc_face ^ 3'd1, fs_lane)), NONE);
      // Lanes for new strands, per edge (fx and fy from here, fy from the fx neighbour, fx from
      // the fy neighbour): the lowest free ones, used from the highest down.
      if (l == 0) begin
        fl = lanes_for(lanes_used(Y, fp_fx), bneed[3:0]); i_lanes[7:0] = fl[7:0]; i_ptr[2:0] = fl[10:8];
        fl = lanes_for(lanes_used(Y, fp_fy), bneed[7:4]); i_lanes[15:8] = fl[7:0]; i_ptr[5:3] = fl[10:8];
      end
      if (l == 1) begin fl = lanes_for(lanes_used(Y, fp_fy), bneed[11:8]); i_lanes[23:16] = fl[7:0]; i_ptr[8:6] = fl[10:8]; end
      if (l == 2) begin fl = lanes_for(lanes_used(Y, fp_fx), bneed[15:12]); i_lanes[31:24] = fl[7:0]; i_ptr[11:9] = fl[10:8]; end
      // The pair goes: its ports and slots empty.
      for (e = 0; e < 9; e = e + 1) if ((l == 0 && e / 3 == fc_kc) || (l == fp_ploc && e / 3 == fc_kp)) Y[e*6 +: 6] = NONE;
      for (e = 0; e < 3; e = e + 1) if ((l == 0 && e == fc_kc) || (l == fp_ploc && e == fc_kp)) begin Y = stg(Y, e, 4'd0); Y = sw(Y, e, 1'b0); end
      // The fresh agents take their seats.
      for (i = 0; i < 6; i = i + 1) if (i < fs_n && i_loc[i*2 +: 2] == l) begin
        kk = i_seat[i*2 +: 2]; t = rule_fresh(fs_ri, i);
        Y = stg(Y, kk, t); Y = sw(Y, kk, born_wanted(t) || (LAZY && wanted[i]));
      end
      i_q[l*SW +: SW] = Y;
    end
  end

endmodule

// One link of a rewrite, wired along its route through the working copy.
module strands_fire_link (
  input wire [4*219-1:0] aq,
  input wire [11:0] aloc, aseat,
  input wire [31:0] alanes,
  input wire [11:0] aptr,
  input wire [3:0] atch, aj,
  input wire [3:0] fs_nl,
  input wire [9*9-1:0] fs_la, fs_lb,
  input wire [1:0] fp_ploc,
  input wire [2:0] fp_fx, fp_fy,
  output reg [4*219-1:0] l_q,
  output reg [11:0] l_ptr,
  output reg [3:0] l_tch
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_route.vh"
  /* verilator no_inline_module */
  always @* begin : fire_link
    reg [8:0] ta, tb; reg [1:0] la, lb, x1, e_, l0, l1; reg [5:0] ea, eb, a, b; reg [13:0] r; reg [2:0] f0, f1;
    reg [1:0] nst; integer s_, l;
    ta = 0; tb = 0; la = 0; lb = 0; x1 = 0; e_ = 0; l0 = 0; l1 = 0; ea = 0; eb = 0; a = 0; b = 0; r = 0; f0 = 0; f1 = 0; nst = 0;
    l_ptr = aptr; l_tch = atch;
    ta = fs_la[aj*9 +: 9]; tb = fs_lb[aj*9 +: 9];
    la = at_(ta, aloc); lb = at_(tb, aloc);
    ea = ta[8] ? ta[5:0] : ae(aseat[ta[5:3]*2 +: 2], ta[1:0]);
    eb = tb[8] ? tb[5:0] : ae(aseat[tb[5:3]*2 +: 2], tb[1:0]);
    // The route's steps (none if both ends share a site), each taking its edge's next lane.
    r = route(la, lb); nst = la == lb ? 2'd0 : r[13:12]; x1 = r[11:10];
    f0 = dir_face(r[1:0]); f1 = dir_face(r[7:6]);
    for (s_ = 0; s_ < 2; s_ = s_ + 1) if (s_ < nst) begin
      e_ = r[s_*6+2 +: 2]; l_ptr[e_*3 +: 3] = l_ptr[e_*3 +: 3] - 3'd1;
      if (s_ == 0) l0 = alanes[e_*8 + l_ptr[e_*3 +: 3]*2 +: 2]; else l1 = alanes[e_*8 + l_ptr[e_*3 +: 3]*2 +: 2];
    end
    // Each site on the route links one pair of ends: the start, the site passed through, the end.
    for (l = 0; l < 4; l = l + 1) begin
      a = NONE; b = NONE;
      if (l == la) begin a = ea; b = nst == 0 ? eb : se(f0, l0); l_tch[l] = 1'b1; end
      else if (nst == 2 && l == x1) begin a = se(f0 ^ 3'd1, l0); b = se(f1, l1); l_tch[l] = 1'b1; end
      else if (l == lb) begin a = nst == 1 ? se(f0 ^ 3'd1, l0) : se(f1 ^ 3'd1, l1); b = eb; l_tch[l] = 1'b1; end
      l_q[l*SW +: SW] = lk(aq[l*SW +: SW], a, b);
    end
  end

endmodule

// The rest of a turn (lattice.rs `turn`, `active_turn`): a step or an exchange, or a wire folded or
// its corner flipped. Reports the sites it writes. A step and each half of an exchange share one
// hop: an exchange asks for a second cycle (mv_more) with its first half's result handed back in h1.
module strands_move (
  input wire [218:0] here,
  input wire [3*219-1:0] there,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [47:0] lat,
  input wire [63:0] dice,
  input wire active_mode,
  input wire [1:0] act_k,
  input wire swap2,
  input wire [2*219+16:0] h1,
  output reg [7:0] mv_touched,
  output reg mv_stale,
  output reg mv_more,
  output reg [2:0] mv_n,
  output reg [11:0] mv_pos,
  output reg [4*219-1:0] mv_dat,
  output wire [2*219+16:0] mv_h,
  output wire [8:0] mv_rd
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
`include "strands_moves.vh"
  /* verilator no_inline_module */
  wire [15:0] d_metro = dice[15:0]; wire [7:0] d_hop = dice[31:24], d_along = dice[39:32], d_swap = dice[47:40];
  wire [4:0] d_agent = dice[52:48], d_where = dice[57:53]; wire [2:0] d_resident = dice[60:58], d_region = dice[63:61];
  // What the turn does, decided from its own site: go (step or exchange agent k across face f), or
  // reshape the wire at end e (fold it with the neighbour across f, or flip its corner through the
  // neighbours across f and f2). Those neighbours, T, Z and W, are read through there.
  reg go, may_swap, reshape; reg [1:0] k; reg [2:0] f, f2; reg [5:0] e; reg [3:0] tn, zn, wn; wire [SW-1:0] S = here;
  always @* begin : move_decide
    reg [5:0] m; reg [4:0] n; reg [17:0] faces; reg [23:0] used; reg [4:0] c; integer i; reg [5:0] ksl; reg [4:0] pk_;
    m = 0; n = 0; faces = 0; used = 0; c = 0; ksl = 0; pk_ = 0;
    go = 0; may_swap = 0; reshape = 0; k = 0; f = 0; e = 0; tn = 0;
    if (active_mode) begin
      k = act_k; m = gm(S, ae(k, 0));
      if (is_strand(m)) begin go = 1; f = face(m); may_swap = d_swap < CH_SWAP; end
    end else if (d_hop < CH_HOP && occ(S) != 0) begin
      n = 0; ksl = 0;
      for (i = 0; i < 3; i = i + 1) if (gt(S, i) != 0) begin ksl[n*2 +: 2] = i; n = n + 1; end
      pk_ = pick5(d_agent, n); k = ksl[pk_*2 +: 2]; m = gm(S, ae(k, 0)); tn = inb(p, face(m));
      if (is_strand(m) && tn[3] && d_along < CH_ALONG) begin go = 1; f = face(m); end
      else begin
        n = 0; faces = 0;
        for (i = 0; i < 6; i = i + 1) begin tn = inb(p, i); if (tn[3]) begin faces[n*3 +: 3] = i; n = n + 1; end end
        if (n != 0) begin go = 1; pk_ = pick5(d_where, n); f = faces[pk_*3 +: 3]; end
      end
      may_swap = d_swap < CH_SWAP;
    end else begin
      // Reshape a wire: the chosen one among the strand ends in use (with a neighbour in the
      // block), in order.
      n = 0; used = 0;
      for (i = 9; i < 33; i = i + 1) begin
        tn = inb(p, face(i));
        used[i - 9] = gm(S, i) != NONE && tn[3]; n = n + used[i - 9];
      end
      if (n != 0) begin
        reshape = 1; pk_ = pick5(d_where, n); e = 0; c = 0;
        for (i = 0; i < 24; i = i + 1) begin if (used[i] && c == pk_) e = 9 + i; c = c + used[i]; end
        f = face(e);
      end
    end
    tn = inb(p, f); f2 = face(gm(S, e));
    zn = inb(p, f2); wn = tn[3] ? inb(tn[2:0], f2) : 4'd0;
  end
  assign mv_rd = {wn[2:0], zn[2:0], tn[2:0]};
  // Fold or flip (written directly), or set up the hop: whether T is full, which of its residents
  // an exchange moves.
  reg hop, full; reg [1:0] kb; reg [2:0] fsl; reg [SW-1:0] T;
  reg [7:0] r_touched; reg [2:0] r_n; reg [11:0] r_pos; reg [4*SW-1:0] r_dat;
  always @* begin : move_act
    reg [SW-1:0] Z, W; reg [5:0] m; reg folded; reg [2*SW:0] sw_; reg [4*SW:0] fl;
    m = 0; folded = 0; sw_ = 0; fl = 0; hop = 0; full = 0; kb = 0; fsl = 0;
    r_touched = 0; mv_stale = 0; r_n = 0; r_pos = 0; r_dat = 0;
    T = there[0 +: SW]; Z = there[SW +: SW]; W = there[2*SW +: SW];
    if (reshape) begin
      if (taken[tn[2:0]]) mv_stale = 1;
      else begin
        sw_ = fold(S, T, e);
        if (sw_[2*SW]) begin
          r_n = 2; r_pos = {6'd0, tn[2:0], p}; r_dat = {{2*SW{1'b0}}, sw_[SW-1:0], sw_[2*SW-1:SW]};
          r_touched[p] = 1'b1; r_touched[tn[2:0]] = 1'b1; folded = 1;
        end
      end
      m = gm(S, e);
      if (!folded && !mv_stale && is_strand(m) && f[2:1] != f2[2:1] && onlat(p, f) && onlat(p, f2)) begin
        if (!tn[3]) ;
        else if (!zn[3]) ;
        else if (taken[zn[2:0]]) mv_stale = 1;
        else if (!wn[3]) ;
        else if (taken[wn[2:0]]) mv_stale = 1;
        else begin
          fl = flip(S, T, Z, W, e, d_metro);
          if (fl[4*SW]) begin
            r_n = 4; r_pos = {wn[2:0], zn[2:0], tn[2:0], p}; r_dat = {fl[SW-1:0], fl[2*SW-1:SW], fl[3*SW-1:2*SW], fl[4*SW-1:3*SW]};
            r_touched[p] = 1'b1; r_touched[tn[2:0]] = 1'b1; r_touched[zn[2:0]] = 1'b1; r_touched[wn[2:0]] = 1'b1;
          end
        end
      end
    end
    if (go) begin
      if (!tn[3]) ;
      else if (taken[tn[2:0]]) mv_stale = 1;
      else begin
        fsl = free_slot(T); full = !fsl[2];
        // An exchange's resident: T is full, so both its slots hold one.
        kb = pick3(d_resident, 4'd2) == 0 ? 2'd0 : 2'd1;
        hop = !(full && !may_swap);
      end
    end
  end
  // The hop: a step into the free slot (judged on its own), or an exchange's first half (agent k
  // into the transient slot) or second half (the resident kb back into k's old slot).
  wire [SW-1:0] hs = swap2 ? h1[SW-1:0] : S, ht = swap2 ? h1[2*SW-1:SW] : T;
  wire [1:0] hk = swap2 ? kb : k, hk2 = swap2 ? k : full ? 2'd2 : fsl[1:0];
  wire [2*SW+16:0] h = hop_to(hs, ht, hk, swap2 ? f ^ 3'd1 : f, hk2, !swap2 && !full, d_metro);
  assign mv_h = h;
  always @* begin : move_write
    reg [SW-1:0] S2, T2; reg signed [15:0] d1, d2;
    S2 = 0; T2 = 0; d1 = 0; d2 = 0; mv_more = 0;
    mv_touched = r_touched; mv_n = r_n; mv_pos = r_pos; mv_dat = r_dat;
    if (hop && !full && h[2*SW+16]) begin
      mv_n = 2; mv_pos = {6'd0, tn[2:0], p}; mv_dat = {{2*SW{1'b0}}, h[SW-1:0], h[2*SW-1:SW]}; mv_touched[p] = 1'b1; mv_touched[tn[2:0]] = 1'b1;
    end
    if (hop && full && !swap2) mv_more = 1;
    if (hop && full && swap2) begin
      d1 = h1[2*SW +: 16]; d2 = h[2*SW +: 16];
      S2 = h[SW-1:0]; T2 = relocate(h[2*SW-1:SW], 2'd2, kb);
      if (h1[2*SW+16] && h[2*SW+16] && pairs(S2) <= PAIRS && pairs(T2) <= PAIRS && accept(d1 + d2, d_metro)) begin
        mv_n = 2; mv_pos = {6'd0, tn[2:0], p}; mv_dat = {{2*SW{1'b0}}, T2, S2}; mv_touched[p] = 1'b1; mv_touched[tn[2:0]] = 1'b1;
      end
    end
  end
endmodule
