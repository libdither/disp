// The stages of a turn, as combinational modules of the block unit (strands_block.v). Each mirrors
// the function of the same name in crate/src/lattice.rs.

// An eraser at the turn's site touching garbage collects it (lattice.rs `collect`,
// `erase_inputs`, `erase_unpair`).
module strands_collect (
  input wire [8*219-1:0] st,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
  output reg c_apply, c_stale, c_two,
  output reg [7:0] c_touched,
  output reg [2:0] c_dpos,
  output reg [218:0] c_s, c_d
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  always @* begin : collect_stage
    reg [SW-1:0] S, D; reg [5:0] m, md, other, input_, mm, m0, m1; reg [3:0] t, td, nn;
    reg [2:0] dpos, s2pos, f, f2; reg [1:0] i, i2, kd, q, k2; reg via, via2, same, same2, done, ok, ok2;
    integer ke, pp;
    S = 0; D = 0; m = 0; md = 0; other = 0; input_ = 0; mm = 0; m0 = 0; m1 = 0; t = 0; td = 0; nn = 0; dpos = 0; s2pos = 0; f = 0; f2 = 0; i = 0; i2 = 0; kd = 0; q = 0; k2 = 0; via = 0; via2 = 0; same = 0; same2 = 0; done = 0; ok = 0; ok2 = 0;
    c_apply = 0; c_stale = 0; c_touched = 0; done = 0; c_two = 0; c_dpos = 0; c_s = 0; c_d = 0;
    for (ke = 0; ke < 2; ke = ke + 1) if (!done) begin
      S = site(st, p); D = S;
      t = gt(S, ke); m = gm(S, ae(ke, 0));
      ok = t == T_EPS && m != NONE; via = 0; dpos = p; md = m; f = 0; i = 0;
      if (ok && is_strand(m)) begin
        f = face(m); i = lane(m); nn = inb(p, f);
        if (!nn[3]) ok = 0;
        else if (taken[nn[2:0]]) begin ok = 0; c_stale = 1; done = 1; end
        else begin via = 1; dpos = nn[2:0]; D = site(st, dpos); md = gm(D, se(f ^ 3'd1, i)); end
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
            if (via) begin S = sm(S, se(f, i), NONE); D = sm(D, se(f ^ 3'd1, i), NONE); end
            if (via2) begin D = sm(D, se(f2, i2), NONE); S = sm(S, se(f2 ^ 3'd1, i2), NONE); end
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
            if (via) begin S = sm(S, se(f, i), NONE); D = sm(D, se(f ^ 3'd1, i), NONE); end
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
      if (c_apply && !c_two && !c_touched[p]) begin
        c_s = S; c_touched[p] = 1'b1;
        if (!same) begin c_d = D; c_dpos = dpos; c_two = 1; c_touched[dpos] = 1'b1; end
      end
    end
  end


endmodule

// A consumer at site ap_pos whose partner is here or one strand away (lattice.rs `active_pair`).
module strands_pair (
  input wire [8*219-1:0] st,
  input wire [2:0] ap_pos,
  input wire [7:0] taken,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
  output reg ap_found, ap_stale, ap_via, ap_inf,
  output reg [1:0] ap_kc, ap_kp, ap_inf_k,
  output reg [2:0] ap_face, ap_inf_pos
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  always @* begin : pair_stage
    reg [SW-1:0] S; reg [5:0] m, mm; reg [3:0] t, t2, nn; reg [2:0] sp, f; reg [1:0] k2, q; reg stop, skip;
    integer k;
    S = 0; m = 0; mm = 0; t = 0; t2 = 0; nn = 0; sp = 0; f = 0; k2 = 0; q = 0; stop = 0; skip = 0;
    ap_found = 0; ap_stale = 0; ap_via = 0; ap_inf = 0; ap_kc = 0; ap_kp = 0; ap_face = 0; ap_inf_pos = 0; ap_inf_k = 0; stop = 0;
    S = site(st, ap_pos);
    for (k = 0; k < 3; k = k + 1) if (!stop) begin
      t = gt(S, k); m = gm(S, ae(k, 0)); skip = 0; sp = ap_pos; mm = m; f = 0;
      if (t == 0 || !is_consumer(t) || (LAZY && !gw(S, k)) || m == NONE) skip = 1;
      if (!skip && is_strand(m)) begin
        f = face(m); nn = inb(ap_pos, f);
        if (!nn[3]) skip = 1;
        else if (taken[nn[2:0]]) begin ap_stale = 1; stop = 1; skip = 1; end
        else begin sp = nn[2:0]; mm = gm(site(st, sp), se(f ^ 3'd1, lane(m))); end
      end
      if (!skip && !is_strand(mm) && mm != NONE) begin
        k2 = port_k(mm); q = port_q(mm); t2 = gt(site(st, sp), k2);
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
  input wire [8*219-1:0] st,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
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
  output reg [2:0] fs_lost_s, fs_lost_sp
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  wire [15:0] d_metro = dice[15:0]; wire [7:0] d_hop = dice[31:24], d_along = dice[39:32], d_swap = dice[47:40];
  wire [4:0] d_agent = dice[52:48], d_where = dice[57:53]; wire [2:0] d_resident = dice[60:58], d_region = dice[63:61];
  // The rule's wires, read from the table once.
  reg [71:0] fs_wa, fs_wb; reg [3:0] fs_nw;
  always @* begin : rule_wires
    integer w;
    fs_nw = rule_nw(fs_ri);
    for (w = 0; w < 9; w = w + 1) begin fs_wa[w*8 +: 8] = rule_wa(fs_ri, w); fs_wb[w*8 +: 8] = rule_wb(fs_ri, w); end
  end
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
  always @* begin : fire_setup
    reg [SW-1:0] S, SP; reg [3:0] ct, pt, sp4, hx, hy, dd; reg [7:0] e, d; reg [5:0] m; reg [8:0] ta, tb, x;
    reg [17:0] dmc, dmp; reg fin, dup, stop, at_s, dying; reg [1:0] k, q; reg [3:0] start; reg [2:0] fx, fy;
    integer w, it, l, j, oi;
    S = 0; SP = 0; ct = 0; pt = 0; sp4 = 0; hx = 0; hy = 0; dd = 0; e = 0; d = 0; m = 0; ta = 0; tb = 0; x = 0; dmc = 0; dmp = 0; fin = 0; dup = 0; stop = 0; at_s = 0; dying = 0; k = 0; q = 0; start = 0; fx = 0; fy = 0;
    S = site(st, p);
    sp4 = inb(p, fc_face); fs_sp = fc_via ? sp4[2:0] : p;
    SP = site(st, fs_sp);
    ct = gt(S, fc_kc); pt = gt(SP, fc_kp); fs_ri = rule_of(ct, pt); fs_n = rule_n(fs_ri);
    fs_lane = lane(gm(S, ae(fc_kc, 0)));
    dmc = {gm(S, ae(fc_kc, 2)), gm(S, ae(fc_kc, 1)), NONE};
    dmp = {gm(SP, ae(fc_kp, 2)), gm(SP, ae(fc_kp, 1)), NONE};
    if (arity(ct) < 3) dmc[17:12] = NONE; if (arity(ct) < 2) dmc[11:6] = NONE;
    if (arity(pt) < 3) dmp[17:12] = NONE; if (arity(pt) < 2) dmp[11:6] = NONE;
    // Walk each rule wire's ends through dying aux ports to fresh ports or ends that stay.
    fs_nl = 0; fs_la = 0; fs_lb = 0; ta = 0; tb = 0;
    for (w = 0; w < 9; w = w + 1) if (w < fs_nw) begin
      for (j = 0; j < 2; j = j + 1) begin
        e = j == 0 ? fs_wa[w*8 +: 8] : fs_wb[w*8 +: 8]; fin = 0; x = 0;
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
            else e = partner(d);
          end
        end
        if (j == 0) ta = x; else tb = x;
      end
      dup = 0;
      for (l = 0; l < 9; l = l + 1) if (l < fs_nl)
        if ((fs_la[l*9 +: 9] == ta && fs_lb[l*9 +: 9] == tb) || (fs_la[l*9 +: 9] == tb && fs_lb[l*9 +: 9] == ta)) dup = 1;
      if (!dup) begin fs_la[fs_nl*9 +: 9] = ta; fs_lb[fs_nl*9 +: 9] = tb; fs_nl = fs_nl + 1; end
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
        else begin fs_regs[fs_nreg*4 +: 4] = oi; fs_nreg = fs_nreg + 1; end
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
  input wire [8*219-1:0] st,
  input wire [2:0] p,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
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
  output reg [19:0] fp_left
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
  /* verilator no_inline_module */
  always @* begin : fire_prep
    reg [3:0] h; reg [SW-1:0] Q; reg [3:0] oi; integer l, kk; reg [2:0] n; reg [7:0] lst;
    h = 0; Q = 0; oi = 0; n = 0; lst = 0;
    oi = fs_regs[fr*4 +: 4];
    fp_fx = oi < 4 ? oi / 2 : oi < 8 ? (oi - 4) / 2 : 2 + (oi - 8) / 2;
    fp_fy = oi < 4 ? 2 + oi % 2 : 4 + oi % 2;
    fp_pos[2:0] = p; h = nbq(p, fp_fx); fp_pos[5:3] = h[2:0]; h = nbq(p, fp_fy); fp_pos[8:6] = h[2:0];
    h = nbq(fp_pos[5:3], fp_fy); fp_pos[11:9] = h[2:0];
    fp_ploc = fs_sp == p ? 2'd0 : fs_sp == fp_pos[5:3] ? 2'd1 : 2'd2;
    fp_slots = 0; fp_nslots = 0; fp_left = 0;
    for (l = 0; l < 4; l = l + 1) begin
      Q = site(st, fp_pos[l*3 +: 3]);
      // Slots for fresh agents: the dying producer's, the dying consumer's, then free ones.
      n = 0; lst = 0;
      if (l == fp_ploc) begin lst[n*2 +: 2] = fc_kp; n = n + 1; end
      if (l == 0) begin lst[n*2 +: 2] = fc_kc; n = n + 1; end
      for (kk = 0; kk < 2; kk = kk + 1) if (gt(Q, kk) == 0) begin lst[n*2 +: 2] = kk; n = n + 1; end
      fp_slots[l*8 +: 8] = lst; fp_nslots[l*3 +: 3] = n;
      fp_left[l*5 +: 5] = pairs(Q) - (l == 0 ? fs_lost_s : 0) - (l != 0 && l == fp_ploc ? fs_lost_sp : 0);
    end
    fp_avail[2:0]  = 3'd4 - used_lanes(site(st, p), fp_fx);
    fp_avail[5:3]  = 3'd4 - used_lanes(site(st, p), fp_fy);
    fp_avail[8:6]  = 3'd4 - used_lanes(site(st, fp_pos[5:3]), fp_fy);
    fp_avail[11:9] = 3'd4 - used_lanes(site(st, fp_pos[8:6]), fp_fx);
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
// the fresh agents seated, each edge's lanes for new strands chosen.
module strands_fire_init (
  input wire [8*219-1:0] st,
  input wire [2:0] p,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
  input wire [11:0] fp_pos,
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
`include "strands_geom.vh"
`include "strands_route.vh"
  /* verilator no_inline_module */
  always @* begin : fire_init
    reg [4*SW-1:0] Q; reg [7:0] tk; reg [3:0] t; reg [2:0] sl, ef; reg [1:0] la, kk; integer i, j, n_, ee; reg [SW-1:0] Y; reg [5:0] wanted; reg [39:0] bneed;
    Q = 0; tk = 0; t = 0; sl = 0; ef = 0; la = 0; kk = 0; Y = 0; wanted = 0; bneed = 0;
    Q = {site(st, fp_pos[11:9]), site(st, fp_pos[8:6]), site(st, fp_pos[5:3]), site(st, p)};
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
    if (fc_via) begin
      Q = p4(Q, 0, sm(q4(Q, 0), se(fc_face, fs_lane), NONE));
      Q = p4(Q, fp_ploc, sm(q4(Q, fp_ploc), se(fc_face ^ 3'd1, fs_lane), NONE));
    end
    // Lanes for new strands, per edge: the lowest free ones, used from the highest down.
    bneed = need_add(i_loc); i_lanes = 0; i_ptr = 0;
    for (ee = 0; ee < 4; ee = ee + 1) begin
      Y = ee == 0 ? q4(Q, 0) : ee == 1 ? q4(Q, 0) : ee == 2 ? q4(Q, 1) : q4(Q, 2);
      ef = ee == 0 ? fp_fx : ee == 1 ? fp_fy : ee == 2 ? fp_fy : fp_fx;
      n_ = 0;
      for (i = 0; i < 4; i = i + 1) if (gm(Y, se(ef, i)) == NONE && n_ < bneed[ee*4 +: 4]) begin i_lanes[ee*8 + n_*2 +: 2] = i; n_ = n_ + 1; end
      i_ptr[ee*3 +: 3] = n_;
    end
    for (i = 0; i < 2; i = i + 1) begin
      la = i == 0 ? 2'd0 : fp_ploc; kk = i == 0 ? fc_kc : fc_kp; Y = q4(Q, la);
      for (j = 0; j < 3; j = j + 1) Y = sm(Y, ae(kk, j), NONE);
      Y = stg(Y, kk, 4'd0); Y = sw(Y, kk, 1'b0);
      Q = p4(Q, la, Y);
    end
    wanted = rule_wanted(fs_ri);
    for (i = 0; i < 6; i = i + 1) if (i < fs_n) begin
      la = i_loc[i*2 +: 2]; kk = i_seat[i*2 +: 2]; t = rule_fresh(fs_ri, i);
      Y = q4(Q, la); Y = stg(Y, kk, t); Y = sw(Y, kk, born_wanted(t) || (LAZY && wanted[i]));
      Q = p4(Q, la, Y); i_tch[la] = 1'b1;
    end
    i_q = Q;
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
    reg [8:0] ta, tb; reg [1:0] la, lb, x, lane_, e_; reg [5:0] ea, eb, cur; reg [13:0] r; reg [2:0] fc; integer s_;
    ta = 0; tb = 0; la = 0; lb = 0; x = 0; lane_ = 0; e_ = 0; ea = 0; eb = 0; cur = 0; r = 0; fc = 0;
    l_q = aq; l_ptr = aptr; l_tch = atch;
    ta = fs_la[aj*9 +: 9]; tb = fs_lb[aj*9 +: 9];
    la = at_(ta, aloc); lb = at_(tb, aloc);
    ea = ta[8] ? ta[5:0] : ae(aseat[ta[5:3]*2 +: 2], ta[1:0]);
    eb = tb[8] ? tb[5:0] : ae(aseat[tb[5:3]*2 +: 2], tb[1:0]);
    l_tch[la] = 1'b1; l_tch[lb] = 1'b1;
    if (la == lb) l_q = p4(l_q, la, lk(q4(l_q, la), ea, eb));
    else begin
      r = route(la, lb); cur = ea;
      for (s_ = 0; s_ < 2; s_ = s_ + 1) if (s_ < r[13:12]) begin
        x = r[s_*6+4 +: 2]; e_ = r[s_*6+2 +: 2]; fc = dir_face(r[s_*6 +: 2]);
        l_ptr[e_*3 +: 3] = l_ptr[e_*3 +: 3] - 3'd1; lane_ = alanes[e_*8 + l_ptr[e_*3 +: 3]*2 +: 2];
        l_q = p4(l_q, x, lk(q4(l_q, x), cur, se(fc, lane_))); l_tch[x] = 1'b1;
        cur = se(fc ^ 3'd1, lane_);
      end
      l_q = p4(l_q, lb, lk(q4(l_q, lb), cur, eb));
    end
  end


endmodule

// The rest of a turn (lattice.rs `turn`, `active_turn`): a step or an exchange, or a wire folded or
// its corner flipped. Reports the sites it writes.
module strands_move (
  input wire [8*219-1:0] st,
  input wire [2:0] p,
  input wire [7:0] taken,
  input wire [10:0] cx1, cy1, cz1, lw, lh, ld,
  input wire [63:0] dice,
  input wire active_mode,
  input wire [1:0] act_k,
  output reg [7:0] mv_touched,
  output reg mv_stale,
  output reg [2:0] mv_n,
  output reg [11:0] mv_pos,
  output reg [4*219-1:0] mv_dat
);
`include "strands_tables.vh"
`include "strands_fn.vh"
`include "strands_geom.vh"
`include "strands_moves.vh"
  /* verilator no_inline_module */
  wire [15:0] d_metro = dice[15:0]; wire [7:0] d_hop = dice[31:24], d_along = dice[39:32], d_swap = dice[47:40];
  wire [4:0] d_agent = dice[52:48], d_where = dice[57:53]; wire [2:0] d_resident = dice[60:58], d_region = dice[63:61];
  always @* begin : move_stage
    reg [8*SW-1:0] B; reg [SW-1:0] S, T, X, Z, W; reg [5:0] m, e, list_e; reg [3:0] tn, xn, zn, wn;
    reg [2:0] f, f1, f2; reg [1:0] k, slot; reg [2:0] fsl; reg [4:0] n; reg go, may_swap, folded;
    reg [2*SW+16:0] h; reg [2*SW:0] sw_; reg [4*SW:0] fl; reg [17:0] faces; reg [71:0] used; integer i, j;
    reg [5:0] ksl; reg [4:0] pk_;
    B = 0; S = 0; T = 0; X = 0; Z = 0; W = 0; m = 0; e = 0; list_e = 0; tn = 0; xn = 0; zn = 0; wn = 0; f = 0; f1 = 0; f2 = 0; k = 0; slot = 0; fsl = 0; n = 0; go = 0; may_swap = 0; folded = 0; h = 0; sw_ = 0; fl = 0; faces = 0; used = 0; ksl = 0; pk_ = 0;
    B = st; mv_touched = 0; mv_stale = 0; mv_n = 0; mv_pos = 0; mv_dat = 0;
    S = site(st, p);
    go = 0; k = 0; f = 0; may_swap = 0;
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
      // Reshape a wire: fold it, or flip its corner.
      n = 0; used = 0;
      for (i = 9; i < 33; i = i + 1) begin
        tn = inb(p, face(i));
        if (gm(S, i) != NONE && tn[3]) begin used[n*6 +: 6] = i; n = n + 1; end
      end
      if (n != 0) begin
        pk_ = pick5(d_where, n); e = used[pk_*6 +: 6]; f1 = face(e); tn = inb(p, f1); folded = 0;
        if (taken[tn[2:0]]) mv_stale = 1;
        else begin
          T = site(B, tn[2:0]); sw_ = fold(S, T, e);
          if (sw_[2*SW]) begin
            mv_n = 2; mv_pos = {6'd0, tn[2:0], p}; mv_dat = {{2*SW{1'b0}}, sw_[SW-1:0], sw_[2*SW-1:SW]};
            mv_touched[p] = 1'b1; mv_touched[tn[2:0]] = 1'b1; folded = 1;
          end
        end
        m = gm(S, e); f2 = face(m);
        if (!folded && !mv_stale && is_strand(m) && f1[2:1] != f2[2:1] && onlat(p, f1) && onlat(p, f2)) begin
          xn = inb(p, f1); zn = inb(p, f2); wn = xn[3] ? inb(xn[2:0], f2) : 4'd0;
          if (!xn[3]) ;
          else if (taken[xn[2:0]]) mv_stale = 1;
          else if (!zn[3]) ;
          else if (taken[zn[2:0]]) mv_stale = 1;
          else if (!wn[3]) ;
          else if (taken[wn[2:0]]) mv_stale = 1;
          else begin
            X = site(B, xn[2:0]); Z = site(B, zn[2:0]); W = site(B, wn[2:0]);
            fl = flip(S, X, Z, W, e, d_metro);
            if (fl[4*SW]) begin
              mv_n = 4; mv_pos = {wn[2:0], zn[2:0], xn[2:0], p}; mv_dat = {fl[SW-1:0], fl[2*SW-1:SW], fl[3*SW-1:2*SW], fl[4*SW-1:3*SW]};
              mv_touched[p] = 1'b1; mv_touched[xn[2:0]] = 1'b1; mv_touched[zn[2:0]] = 1'b1; mv_touched[wn[2:0]] = 1'b1;
            end
          end
        end
      end
    end
    // A step, or an exchange with a resident of a full site.
    if (go) begin
      tn = inb(p, f);
      if (!tn[3]) ;
      else if (taken[tn[2:0]]) mv_stale = 1;
      else begin
        T = site(B, tn[2:0]); fsl = free_slot(T);
        if (!fsl[2]) begin
          if (may_swap) begin
            sw_ = swap(S, T, k, f, d_resident, d_metro);
            if (sw_[2*SW]) begin mv_n = 2; mv_pos = {6'd0, tn[2:0], p}; mv_dat = {{2*SW{1'b0}}, sw_[SW-1:0], sw_[2*SW-1:SW]}; mv_touched[p] = 1'b1; mv_touched[tn[2:0]] = 1'b1; end
          end
        end else begin
          h = hop_to(S, T, k, f, fsl[1:0], 1'b1, d_metro);
          if (h[2*SW+16]) begin mv_n = 2; mv_pos = {6'd0, tn[2:0], p}; mv_dat = {{2*SW{1'b0}}, h[SW-1:0], h[2*SW-1:SW]}; mv_touched[p] = 1'b1; mv_touched[tn[2:0]] = 1'b1; end
        end
      end
    end
  end


endmodule
