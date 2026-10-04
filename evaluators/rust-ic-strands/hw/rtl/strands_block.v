// One 2×2×2 block of the strand lattice: the storage of its 8 sites and the logic that gives
// each site its turn, mirroring the block schedule of crate/src/lattice.rs (`margolus_clock`
// with `block_moves`) bit for bit. Positions are x + 2y + 4z within the block.
//
// A turn runs as a short sequence of stages, one cycle each unless noted:
//   COLLECT  an eraser touching garbage collects it, one eraser per cycle   (`collect`)
//   PAIR     look for a reaction, here or one strand away                (`active_pair`)
//   FIRE     rewrite: set up; then per candidate square, prepare and     (`fire`, `regions`)
//            try the seatings one per cycle in the simulator's order; apply
//   MOVE     take an infection, then step, or exchange (two cycles), or   (`turn`, `active_turn`)
//            fold or flip
//   SETTLE   drop pulses whose strand the turn rewired                    (`settle_pulses`)
// A block first asks every site whether a reaction is ready (one cycle each), then runs the
// turns: ready sites, then sites with agents, then bare wire, each in block order rotated by the
// block's random word, skipping sites an earlier turn changed.
module strands_block (
  input  wire          clk,
  input  wire          rst,
  // Storage: write a site (loading).
  input  wire          wr_en,
  input  wire [2:0]    wr_pos,
  input  wire [218:0]  wr_data,
  // All eight sites at once (shifting the lattice, the pulse phase, reading back).
  input  wire          ld_all,
  input  wire [8*219-1:0] ld_data,
  output wire [8*219-1:0] st_out,
  // Run one turn (at turn_pos, with turn_taken changed earlier this clock), or a whole block.
  input  wire          go_turn,
  input  wire          go_block,
  input  wire [2:0]    turn_pos,
  input  wire [7:0]    turn_taken,
  input  wire [31:0]   clock,                    // these three are taken when a turn or block starts
  input  wire [10:0]   cx1, cy1, cz1,            // the block's corner, plus one
  input  wire [10:0]   lw, lh, ld,               // the lattice's size
  output reg           busy,
  output reg  [7:0]    touched,                  // sites the last turn wrote
  output reg           stale,                    // the last turn looked at a site changed earlier
  output reg  [7:0]    taken,                    // sites changed so far this clock
  output reg  [31:0]   cycles                    // cycles the last turn or block took
);
`include "strands_tables.vh"
`include "strands_fn.vh"
  /* verilator hier_block */

  reg [8*SW-1:0] st;
  // The block's place in the lattice and the clock, as they were when the turn or block started:
  // whether each position's neighbour across each face is on the lattice (bit q*6 + f), whether
  // each position is, and each position's key for its dice.
  reg [10:0] bx1, by1, bz1, bw, bh, bd; reg [31:0] bclock;
  reg [47:0] lat; reg [7:0] onl;
  always @* begin : edges
    reg [10:0] x, y, z; integer q;
    for (q = 0; q < 8; q = q + 1) begin
      x = bx1 - 11'd1 + q[0]; y = by1 - 11'd1 + q[1]; z = bz1 - 11'd1 + q[2];
      lat[q*6 +: 6] = {z != 0, z + 11'd1 < bd, y != 0, y + 11'd1 < bh, x != 0, x + 11'd1 < bw};
      onl[q] = x < bw && y < bh && z < bd;
    end
  end
  function [31:0] key(input [2:0] q);
    key = {2'b00, bz1[9:0] - 10'd1 + q[2], by1[9:0] - 10'd1 + q[1], bx1[9:0] - 10'd1 + q[0]};
  endfunction
`include "strands_geom.vh"
  assign st_out = st;

  // ---- the turn under way -------------------------------------------------------------------
  reg [2:0] p;           // the site taking its turn
  reg [63:0] dice;       // its random word
  wire [7:0] d_active = dice[23:16];
  wire [4:0] d_agent  = dice[52:48];
  reg active_mode;       // a wanted reader took the turn (`active_turn`)
  reg inf; reg [2:0] inf_pos; reg [1:0] inf_k;   // a pending computation the reader touched
  reg [2:0] ap_pos;      // the site asked whether a reaction is ready
  reg [1:0] fc_kc, fc_kp; reg fc_via; reg [2:0] fc_face;     // the pair being rewritten
  reg [1:0] fr;                                              // which candidate square
  reg [11:0] code, best_code; reg best_valid; reg [4:0] best_cost;   // the seating search
  reg [4*SW-1:0] aq; reg [11:0] aloc, aseat; reg [31:0] alanes; reg [11:0] aptr; reg [3:0] atch; reg [3:0] aj;  // the rewrite's working copy
  reg swap2;             // an exchange's second half, its first half's result kept in aq
  reg [1:0] act_k;       // the wanted reader an active turn moves, chosen before any infection

  // ---- reading sites -------------------------------------------------------------------------
  // The site asked whether a reaction is ready (a block's first cycles) or taking its turn, and
  // three more at positions the stage under way names: one multiplexer each, for all stages.
  wire [SW-1:0] here = site(st, ap_pos);
  reg [8:0] rd_at;
  wire [3*SW-1:0] there = {site(st, rd_at[8:6]), site(st, rd_at[5:3]), site(st, rd_at[2:0])};

  // ---- the stages ---------------------------------------------------------------------------
  wire c_apply, c_stale, c_two, c_again; wire [7:0] c_touched; wire [2:0] c_dpos; wire [SW-1:0] c_s, c_d; reg c_second; wire [8:0] c_rd;
  strands_collect collect (.here(here), .there(there), .p(p), .taken(taken), .lat(lat), .c_second(c_second),
    .c_apply(c_apply), .c_stale(c_stale), .c_two(c_two), .c_again(c_again), .c_touched(c_touched), .c_dpos(c_dpos), .c_s(c_s), .c_d(c_d), .c_rd(c_rd));
  wire ap_found, ap_stale, ap_via, ap_inf; wire [1:0] ap_kc, ap_kp, ap_inf_k; wire [2:0] ap_face, ap_inf_pos; wire [8:0] ap_rd;
  strands_pair pair (.here(here), .there(there), .ap_pos(ap_pos), .taken(taken), .lat(lat),
    .ap_found(ap_found), .ap_stale(ap_stale), .ap_via(ap_via), .ap_inf(ap_inf), .ap_kc(ap_kc), .ap_kp(ap_kp), .ap_inf_k(ap_inf_k),
    .ap_face(ap_face), .ap_inf_pos(ap_inf_pos), .ap_rd(ap_rd));
  // The setup's results, kept from its cycle on: later stages read other sites through there.
  wire [4:0] su_ri; wire [2:0] su_n; wire [3:0] su_nl; wire [9*9-1:0] su_la, su_lb; wire [2:0] su_sp; wire [1:0] su_lane, su_nreg;
  wire [11:0] su_regs; wire su_stale; wire [2:0] su_lost_s, su_lost_sp; wire [8:0] fs_rd;
  reg [4:0] fs_ri; reg [2:0] fs_n; reg [3:0] fs_nl; reg [9*9-1:0] fs_la, fs_lb; reg [2:0] fs_sp; reg [1:0] fs_lane, fs_nreg;
  reg [11:0] fs_regs; reg [2:0] fs_lost_s, fs_lost_sp;
  strands_fire_setup setup (.here(here), .there(there), .p(p), .taken(taken), .lat(lat), .dice(dice),
    .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fc_face(fc_face), .fs_ri(su_ri), .fs_n(su_n), .fs_nl(su_nl), .fs_la(su_la),
    .fs_lb(su_lb), .fs_sp(su_sp), .fs_lane(su_lane), .fs_nreg(su_nreg), .fs_regs(su_regs), .fs_stale(su_stale),
    .fs_lost_s(su_lost_s), .fs_lost_sp(su_lost_sp), .fs_rd(fs_rd));
  wire [2:0] fp_fx, fp_fy; wire [11:0] fp_pos; wire [1:0] fp_ploc; wire [31:0] fp_slots; wire [11:0] fp_nslots, fp_avail; wire [19:0] fp_left;
  wire [4*SW-1:0] fp_q; wire [8:0] fp_rd;
  strands_fire_prep prep (.here(here), .there(there), .p(p), .lat(lat), .fr(fr), .fs_regs(fs_regs),
    .fs_sp(fs_sp), .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fs_lost_s(fs_lost_s), .fs_lost_sp(fs_lost_sp), .fp_fx(fp_fx),
    .fp_fy(fp_fy), .fp_pos(fp_pos), .fp_ploc(fp_ploc), .fp_slots(fp_slots), .fp_nslots(fp_nslots), .fp_avail(fp_avail), .fp_left(fp_left), .fp_q(fp_q), .fp_rd(fp_rd));
  wire sr_ok; wire [4:0] sr_cost; wire [11:0] sr_loc; wire [15:0] sr_need;
  strands_fire_search search (.code(code), .fs_n(fs_n), .fs_nl(fs_nl), .fs_la(fs_la), .fs_lb(fs_lb), .fp_ploc(fp_ploc), .fp_fx(fp_fx),
    .fp_fy(fp_fy), .fp_nslots(fp_nslots), .fp_avail(fp_avail), .fp_left(fp_left), .sr_ok(sr_ok), .sr_cost(sr_cost), .sr_loc(sr_loc),
    .sr_need(sr_need));
  wire [11:0] code_last = (12'd1 << (2 * fs_n)) - 12'd1;
  wire [4*SW-1:0] i_q; wire [11:0] i_loc, i_seat; wire [31:0] i_lanes; wire [11:0] i_ptr; wire [3:0] i_tch;
  strands_fire_init init (.fp_q(fp_q), .fp_ploc(fp_ploc),
    .fp_fx(fp_fx), .fp_fy(fp_fy), .fp_slots(fp_slots), .fs_n(fs_n), .fs_nl(fs_nl), .fs_la(fs_la), .fs_lb(fs_lb), .fs_ri(fs_ri),
    .fs_lane(fs_lane), .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fc_face(fc_face), .best_code(best_code), .i_q(i_q), .i_loc(i_loc),
    .i_seat(i_seat), .i_lanes(i_lanes), .i_ptr(i_ptr), .i_tch(i_tch));
  wire [4*SW-1:0] l_q; wire [11:0] l_ptr; wire [3:0] l_tch;
  strands_fire_link link (.aq(aq), .aloc(aloc), .aseat(aseat), .alanes(alanes), .aptr(aptr), .atch(atch), .aj(aj), .fs_nl(fs_nl),
    .fs_la(fs_la), .fs_lb(fs_lb), .fp_ploc(fp_ploc), .fp_fx(fp_fx), .fp_fy(fp_fy), .l_q(l_q), .l_ptr(l_ptr), .l_tch(l_tch));
  wire [7:0] mv_touched; wire mv_stale, mv_more; wire [2:0] mv_n; wire [11:0] mv_pos; wire [4*SW-1:0] mv_dat; wire [2*SW+16:0] mv_h; wire [8:0] mv_rd;
  strands_move move (.here(here), .there(there), .p(p), .taken(taken), .lat(lat), .dice(dice),
    .active_mode(active_mode), .act_k(act_k), .swap2(swap2), .h1(aq[2*SW+16:0]), .mv_touched(mv_touched), .mv_stale(mv_stale),
    .mv_more(mv_more), .mv_n(mv_n), .mv_pos(mv_pos), .mv_dat(mv_dat), .mv_h(mv_h), .mv_rd(mv_rd));

  reg [1:0] act_k_now;
  always @* begin : active_reader
    reg [SW-1:0] S; reg [5:0] ksl; reg [4:0] n, pk_; integer i;
    S = here; ksl = 0; n = 0; pk_ = 0;
    for (i = 0; i < 3; i = i + 1)
      if (gt(S, i) != 0 && is_consumer(gt(S, i)) && gt(S, i) != T_EPS && gw(S, i)) begin ksl[n*2 +: 2] = i; n = n + 1; end
    pk_ = pick5(d_agent, n); act_k_now = ksl[pk_*2 +: 2];
  end

  // ---- the turn and the block ---------------------------------------------------------------
  localparam S_IDLE = 0, S_B_READY = 1, S_B_NEXT = 2, S_T_BEGIN = 3, S_T_COLLECT = 4, S_T_PAIR = 5, S_F_SETUP = 6,
             S_F_PREP = 7, S_F_SEARCH = 8, S_F_A0 = 9, S_F_LINK = 10, S_F_WB = 11, S_T_INFECT = 12, S_T_MOVE = 13,
             S_T_SETTLE = 14, S_T_DONE = 15;
  reg [3:0] state; reg block_mode; reg [4:0] bi; reg [2:0] rot; reg [15:0] cls;   // 2 bits per site: 0 ready, 1 agents, 2 wire, 3 none
  always @* case (state)
    S_T_COLLECT:                  rd_at = c_rd;
    S_B_READY, S_T_PAIR:          rd_at = ap_rd;
    S_F_SETUP:                    rd_at = fs_rd;
    S_F_PREP, S_F_SEARCH, S_F_A0: rd_at = fp_rd;
    S_T_MOVE:                     rd_at = mv_rd;
    default:                      rd_at = 9'd0;
  endcase
  reg [47:0] pm0;                                                            // per site: its pulse strand's mate when the turn began
  // One hash serves both: the block's word while it asks its sites, a site's dice when its turn
  // begins.
  wire [63:0] hword = hash(state == S_B_READY ? {1'b1, 1'b0, bz1[9:0], by1[9:0], bx1[9:0]} : key(p), bclock);
  integer qi;
  // A site's class at the start of a block: 0 a reaction is ready, 1 it holds agents, 2 only
  // wire, 3 nothing (it takes no turn).
  reg [1:0] rcls;
  always @* begin : ready_class
    reg [SW-1:0] Q;
    Q = here;
    rcls = !onl[ap_pos] ? 2'd3 : occ(Q) != 0 ? (ap_found ? 2'd0 : 2'd1) : Q[197:54] != {24{NONE}} ? 2'd2 : 2'd3;
  end
  // Every stage writes whole sites through four ports; a site takes the last port naming it.
  reg [3:0] wb_en; reg [11:0] wb_pos; reg [4*SW-1:0] wb_dat;
  always @* begin : write_ports
    wb_en = 0; wb_pos = 0; wb_dat = 0;
    case (state)
      S_IDLE:      begin wb_en[0] = wr_en; wb_pos[2:0] = wr_pos; wb_dat[SW-1:0] = wr_data; end
      S_T_COLLECT: begin wb_en[1:0] = {c_two, 1'b1} & {2{!c_stale && c_apply}}; wb_pos[5:0] = {c_dpos, p}; wb_dat[2*SW-1:0] = {c_d, c_s}; end
      S_F_WB:      begin wb_en = atch; wb_pos = fp_pos; wb_dat = aq; end
      S_T_MOVE:    begin wb_en = {mv_n > 3, mv_n > 2, mv_n > 1, mv_n > 0}; wb_pos = mv_pos; wb_dat = mv_dat; end
      default: ;
    endcase
  end
`ifdef COVER
  // Which of a turn's rarer paths ran: validate.sh builds the testbench with -DCOVER and checks
  // that the recorded turns reach every one of these.
  always @(posedge clk) if (!rst) begin
    if (state == S_T_COLLECT && c_apply && c_two) $display("cover collect-via");
    if (state == S_T_COLLECT && c_apply && !c_two) $display("cover collect-here");
    if (state == S_T_COLLECT && c_apply && c_second) $display("cover collect-second-eraser");
    if (state == S_F_A0 && !fc_via) $display("cover fire-here");
    if (state == S_F_A0 && fc_via && fp_ploc == 1) $display("cover fire-via-x");
    if (state == S_F_A0 && fc_via && fp_ploc == 2) $display("cover fire-via-y");
    if (state == S_F_A0 && fs_nl == 0) $display("cover fire-no-links");
    if (state == S_F_A0 && fs_n == 6) $display("cover fire-six-fresh");
    if (state == S_T_MOVE && !swap2 && mv_n == 2) $display("cover move-2-sites");
    if (state == S_T_MOVE && !swap2 && mv_n == 4) $display("cover move-4-sites");
    if (state == S_T_MOVE && swap2 && mv_n != 0) $display("cover exchange-done");
    if (state == S_T_MOVE && swap2 && mv_n == 0) $display("cover exchange-refused");
  end
`endif
  integer wi;
  always @(posedge clk) begin
    if (!rst) for (qi = 0; qi < 8; qi = qi + 1) for (wi = 0; wi < 4; wi = wi + 1)
      if (wb_en[wi] && wb_pos[wi*3 +: 3] == qi) st[qi*SW +: SW] <= wb_dat[wi*SW +: SW];
    if (rst) begin
      state <= S_IDLE; busy <= 0; st <= 0; taken <= 0; touched <= 0; stale <= 0; cycles <= 0;
    end else begin
      if (busy) cycles <= cycles + 1;
      case (state)
        S_IDLE: begin
          if (ld_all) st <= ld_data;
          if (go_turn || go_block) begin bx1 <= cx1; by1 <= cy1; bz1 <= cz1; bw <= lw; bh <= lh; bd <= ld; bclock <= clock; end
          if (go_turn) begin
            p <= turn_pos; taken <= turn_taken; block_mode <= 0; busy <= 1; cycles <= 1; state <= S_T_BEGIN;
          end else if (go_block) begin
            taken <= 0; block_mode <= 1; busy <= 1; cycles <= 1; bi <= 0; ap_pos <= 0; state <= S_B_READY;
          end
        end
        S_B_READY: begin
          // Each site's class for the order of turns.
          cls[ap_pos*2 +: 2] <= rcls; rot <= hword[2:0];
          ap_pos <= ap_pos + 1;
          if (ap_pos == 3'd7) state <= S_B_NEXT;
        end
        S_B_NEXT: begin
          if (bi == 5'd24) begin busy <= 0; state <= S_IDLE; end
          else begin
            bi <= bi + 1;
            if (cls[((bi[2:0] + rot) & 3'd7)*2 +: 2] == bi[4:3] && !taken[(bi[2:0] + rot) & 3'd7]) begin
              p <= (bi[2:0] + rot) & 3'd7; state <= S_T_BEGIN;
            end
`ifdef DEBUG
            if (bi == 0) $display("block clock %0d rot %0d classes %b", bclock, rot, cls);
            if (cls[((bi[2:0] + rot) & 3'd7)*2 +: 2] == bi[4:3] && !taken[(bi[2:0] + rot) & 3'd7]) $display("  turn at %0d", (bi[2:0] + rot) & 3'd7);
`endif
          end
        end
        S_T_BEGIN: begin
          dice <= hword; touched <= 0; stale <= 0; inf <= 0; active_mode <= 0; ap_pos <= p; swap2 <= 0; c_second <= 0;
          for (qi = 0; qi < 8; qi = qi + 1) pm0[qi*6 +: 6] <= gm(site(st, qi), gpul(site(st, qi)));
          state <= S_T_COLLECT;
        end
        S_T_COLLECT: begin
          if (c_stale) begin stale <= 1; state <= S_T_SETTLE; end
          else if (c_apply) begin touched <= c_touched; state <= S_T_SETTLE; end
          else if (c_again) c_second <= 1;
          else begin
            // A wanted reader takes the turn (`active_turn`) if the dice say so and there is one.
            active_mode <= d_active < CH_ACTIVE && (
              (gt(here, 0) != 0 && is_consumer(gt(here, 0)) && gt(here, 0) != T_EPS && gw(here, 0)) ||
              (gt(here, 1) != 0 && is_consumer(gt(here, 1)) && gt(here, 1) != T_EPS && gw(here, 1)) ||
              (gt(here, 2) != 0 && is_consumer(gt(here, 2)) && gt(here, 2) != T_EPS && gw(here, 2)));
            state <= S_T_PAIR;
          end
        end
        S_T_PAIR: begin
          inf <= ap_inf; inf_pos <= ap_inf_pos; inf_k <= ap_inf_k;
          if (ap_stale) begin stale <= 1; state <= S_T_SETTLE; end
          else if (ap_found) begin fc_kc <= ap_kc; fc_kp <= ap_kp; fc_via <= ap_via; fc_face <= ap_face; state <= S_F_SETUP; end
          else state <= S_T_INFECT;
        end
        S_F_SETUP: begin
          fs_ri <= su_ri; fs_n <= su_n; fs_nl <= su_nl; fs_la <= su_la; fs_lb <= su_lb; fs_sp <= su_sp; fs_lane <= su_lane;
          fs_nreg <= su_nreg; fs_regs <= su_regs; fs_lost_s <= su_lost_s; fs_lost_sp <= su_lost_sp;
          if (su_stale) begin stale <= 1; state <= S_T_SETTLE; end
          else if (su_nreg == 0) state <= S_T_INFECT;
          else begin fr <= 0; state <= S_F_PREP; end
        end
        S_F_PREP: begin code <= 0; best_valid <= 0; state <= S_F_SEARCH; end
        S_F_SEARCH: begin
          if (sr_ok && (!best_valid || sr_cost < best_cost)) begin best_valid <= 1; best_cost <= sr_cost; best_code <= code; end
          if ((sr_ok && sr_cost == 0) || code == code_last) state <= (sr_ok || best_valid) ? S_F_A0 : (fr + 1 < fs_nreg ? S_F_PREP : S_T_INFECT);
          if ((sr_ok && sr_cost == 0) || code == code_last) begin if (!(sr_ok || best_valid)) fr <= fr + 1; end
          code <= code + 1;
        end
        S_F_A0: begin
`ifdef DEBUG
          $display("fire clock %0d pos %0d region %0d fx %0d fy %0d ploc %0d pos %o best %o cost %0d n %0d nl %0d", bclock, p, fr, fp_fx, fp_fy, fp_ploc, fp_pos, best_code, best_cost, fs_n, fs_nl);
`endif
          aq <= i_q; aloc <= i_loc; aseat <= i_seat; alanes <= i_lanes; aptr <= i_ptr; atch <= i_tch; aj <= 0;
          state <= fs_nl == 0 ? S_F_WB : S_F_LINK;
        end
        S_F_LINK: begin
          aq <= l_q; aptr <= l_ptr; atch <= l_tch; aj <= aj + 1;
          if (aj + 1 == fs_nl) state <= S_F_WB;
        end
        S_F_WB: begin
          touched <= (atch[0] ? 8'd1 << fp_pos[2:0] : 8'd0) | (atch[1] ? 8'd1 << fp_pos[5:3] : 8'd0)
                   | (atch[2] ? 8'd1 << fp_pos[8:6] : 8'd0) | (atch[3] ? 8'd1 << fp_pos[11:9] : 8'd0);
          state <= S_T_SETTLE;
        end
        S_T_INFECT: begin
          // The reader an active turn moves is chosen before the infection (`active_turn`).
          act_k <= act_k_now;
          // A reader that touched a pending computation wants it now (`take_infect`).
          if (inf && !gw(site(st, inf_pos), inf_k)) begin
            for (qi = 0; qi < 8; qi = qi + 1) for (wi = 0; wi < 3; wi = wi + 1)
              if (inf_pos == qi && inf_k == wi) st[qi*SW + 210 + wi] <= 1'b1;
            touched[inf_pos] <= 1'b1;
          end
          state <= S_T_MOVE;
        end
        S_T_MOVE: begin
          if (mv_more) begin aq[2*SW+16:0] <= mv_h; swap2 <= 1; end
          else begin touched <= touched | mv_touched; stale <= mv_stale; state <= S_T_SETTLE; end
        end
        S_T_SETTLE: begin
          // A pulse whose strand's mate changed during the turn is lost.
          for (qi = 0; qi < 8; qi = qi + 1)
            if (gpul(site(st, qi)) != NONE && gm(site(st, qi), gpul(site(st, qi))) != pm0[qi*6 +: 6])
              st[qi*SW + 213 +: 6] <= NONE;
          state <= S_T_DONE;
        end
        S_T_DONE: begin
          taken <= taken | touched;
          if (block_mode) state <= S_B_NEXT;
          else begin busy <= 0; state <= S_IDLE; end
        end
        default: state <= S_IDLE;
      endcase
    end
  end
endmodule
