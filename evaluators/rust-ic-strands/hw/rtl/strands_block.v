// One 2×2×2 block of the strand lattice: the storage of its 8 sites and the logic that gives
// each site its turn, mirroring the block schedule of crate/src/lattice.rs (`margolus_clock`
// with `block_moves`) bit for bit. Positions are x + 2y + 4z within the block.
//
// A turn runs as a short sequence of stages, one cycle each unless noted:
//   COLLECT  an eraser touching garbage collects it                      (`collect`)
//   PAIR     look for a reaction, here or one strand away                (`active_pair`)
//   FIRE     rewrite: set up; then per candidate square, prepare and     (`fire`, `regions`)
//            try the seatings one per cycle in the simulator's order; apply
//   MOVE     take an infection, then step or exchange, or fold or flip    (`turn`, `active_turn`)
//   SETTLE   drop pulses whose strand the turn rewired                    (`settle_pulses`)
// A block first asks every site whether a reaction is ready (one cycle each), then runs the
// turns: ready sites, then sites with agents, then bare wire, each in block order rotated by the
// block's random word, skipping sites an earlier turn changed.
module strands_block (
  input  wire          clk,
  input  wire          rst,
  // Storage: write a site, read a site (loading, shifting, reading back).
  input  wire          wr_en,
  input  wire [2:0]    wr_pos,
  input  wire [218:0]  wr_data,
  input  wire [2:0]    rd_pos,
  output wire [218:0]  rd_data,
  // All eight sites at once (shifting the lattice, the pulse phase).
  input  wire          ld_all,
  input  wire [8*219-1:0] ld_data,
  output wire [8*219-1:0] st_out,
  // Run one turn (at turn_pos, with turn_taken changed earlier this clock), or a whole block.
  input  wire          go_turn,
  input  wire          go_block,
  input  wire [2:0]    turn_pos,
  input  wire [7:0]    turn_taken,
  input  wire [31:0]   clock,
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
`include "strands_geom.vh"
  function [8*SW-1:0] put(input [8*SW-1:0] B, input [2:0] q, input [SW-1:0] v);
    integer j; begin put = B; for (j = 0; j < 8; j = j + 1) if (q == j) put[j*SW +: SW] = v; end
  endfunction
  assign rd_data = site(st, rd_pos);
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
  reg [1:0] act_k;       // the wanted reader an active turn moves, chosen before any infection

  // ---- the stages ---------------------------------------------------------------------------
  wire c_apply, c_stale, c_two; wire [7:0] c_touched; wire [2:0] c_dpos; wire [SW-1:0] c_s, c_d;
  strands_collect collect (.st(st), .p(p), .taken(taken), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld),
    .c_apply(c_apply), .c_stale(c_stale), .c_two(c_two), .c_touched(c_touched), .c_dpos(c_dpos), .c_s(c_s), .c_d(c_d));
  wire ap_found, ap_stale, ap_via, ap_inf; wire [1:0] ap_kc, ap_kp, ap_inf_k; wire [2:0] ap_face, ap_inf_pos;
  strands_pair pair (.st(st), .ap_pos(ap_pos), .taken(taken), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld),
    .ap_found(ap_found), .ap_stale(ap_stale), .ap_via(ap_via), .ap_inf(ap_inf), .ap_kc(ap_kc), .ap_kp(ap_kp), .ap_inf_k(ap_inf_k),
    .ap_face(ap_face), .ap_inf_pos(ap_inf_pos));
  wire [4:0] fs_ri; wire [2:0] fs_n; wire [3:0] fs_nl; wire [9*9-1:0] fs_la, fs_lb; wire [2:0] fs_sp; wire [1:0] fs_lane, fs_nreg;
  wire [11:0] fs_regs; wire fs_stale; wire [2:0] fs_lost_s, fs_lost_sp;
  strands_fire_setup setup (.st(st), .p(p), .taken(taken), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld), .dice(dice),
    .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fc_face(fc_face), .fs_ri(fs_ri), .fs_n(fs_n), .fs_nl(fs_nl), .fs_la(fs_la),
    .fs_lb(fs_lb), .fs_sp(fs_sp), .fs_lane(fs_lane), .fs_nreg(fs_nreg), .fs_regs(fs_regs), .fs_stale(fs_stale),
    .fs_lost_s(fs_lost_s), .fs_lost_sp(fs_lost_sp));
  wire [2:0] fp_fx, fp_fy; wire [11:0] fp_pos; wire [1:0] fp_ploc; wire [31:0] fp_slots; wire [11:0] fp_nslots, fp_avail; wire [19:0] fp_left;
  strands_fire_prep prep (.st(st), .p(p), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld), .fr(fr), .fs_regs(fs_regs),
    .fs_sp(fs_sp), .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fs_lost_s(fs_lost_s), .fs_lost_sp(fs_lost_sp), .fp_fx(fp_fx),
    .fp_fy(fp_fy), .fp_pos(fp_pos), .fp_ploc(fp_ploc), .fp_slots(fp_slots), .fp_nslots(fp_nslots), .fp_avail(fp_avail), .fp_left(fp_left));
  wire sr_ok; wire [4:0] sr_cost; wire [11:0] sr_loc; wire [15:0] sr_need;
  strands_fire_search search (.code(code), .fs_n(fs_n), .fs_nl(fs_nl), .fs_la(fs_la), .fs_lb(fs_lb), .fp_ploc(fp_ploc), .fp_fx(fp_fx),
    .fp_fy(fp_fy), .fp_nslots(fp_nslots), .fp_avail(fp_avail), .fp_left(fp_left), .sr_ok(sr_ok), .sr_cost(sr_cost), .sr_loc(sr_loc),
    .sr_need(sr_need));
  wire [11:0] code_last = (12'd1 << (2 * fs_n)) - 12'd1;
  wire [4*SW-1:0] i_q; wire [11:0] i_loc, i_seat; wire [31:0] i_lanes; wire [11:0] i_ptr; wire [3:0] i_tch;
  strands_fire_init init (.st(st), .p(p), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld), .fp_pos(fp_pos), .fp_ploc(fp_ploc),
    .fp_fx(fp_fx), .fp_fy(fp_fy), .fp_slots(fp_slots), .fs_n(fs_n), .fs_nl(fs_nl), .fs_la(fs_la), .fs_lb(fs_lb), .fs_ri(fs_ri),
    .fs_lane(fs_lane), .fc_kc(fc_kc), .fc_kp(fc_kp), .fc_via(fc_via), .fc_face(fc_face), .best_code(best_code), .i_q(i_q), .i_loc(i_loc),
    .i_seat(i_seat), .i_lanes(i_lanes), .i_ptr(i_ptr), .i_tch(i_tch));
  wire [4*SW-1:0] l_q; wire [11:0] l_ptr; wire [3:0] l_tch;
  strands_fire_link link (.aq(aq), .aloc(aloc), .aseat(aseat), .alanes(alanes), .aptr(aptr), .atch(atch), .aj(aj), .fs_nl(fs_nl),
    .fs_la(fs_la), .fs_lb(fs_lb), .fp_ploc(fp_ploc), .fp_fx(fp_fx), .fp_fy(fp_fy), .l_q(l_q), .l_ptr(l_ptr), .l_tch(l_tch));
  wire [7:0] mv_touched; wire mv_stale; wire [2:0] mv_n; wire [11:0] mv_pos; wire [4*SW-1:0] mv_dat;
  strands_move move (.st(st), .p(p), .taken(taken), .cx1(cx1), .cy1(cy1), .cz1(cz1), .lw(lw), .lh(lh), .ld(ld), .dice(dice),
    .active_mode(active_mode), .act_k(act_k), .mv_touched(mv_touched), .mv_stale(mv_stale), .mv_n(mv_n), .mv_pos(mv_pos), .mv_dat(mv_dat));

  reg [1:0] act_k_now;
  always @* begin : active_reader
    reg [SW-1:0] S; reg [5:0] ksl; reg [4:0] n, pk_; integer i;
    S = site(st, p); ksl = 0; n = 0; pk_ = 0;
    for (i = 0; i < 3; i = i + 1)
      if (gt(S, i) != 0 && is_consumer(gt(S, i)) && gt(S, i) != T_EPS && gw(S, i)) begin ksl[n*2 +: 2] = i; n = n + 1; end
    pk_ = pick5(d_agent, n); act_k_now = ksl[pk_*2 +: 2];
  end

  // ---- the turn and the block ---------------------------------------------------------------
  localparam S_IDLE = 0, S_B_READY = 1, S_B_NEXT = 2, S_T_BEGIN = 3, S_T_COLLECT = 4, S_T_PAIR = 5, S_F_SETUP = 6,
             S_F_PREP = 7, S_F_SEARCH = 8, S_F_A0 = 9, S_F_LINK = 10, S_F_WB = 11, S_T_INFECT = 12, S_T_MOVE = 13,
             S_T_SETTLE = 14, S_T_DONE = 15;
  reg [3:0] state; reg block_mode; reg [4:0] bi; reg [2:0] rot; reg [15:0] cls;   // 2 bits per site: 0 ready, 1 agents, 2 wire, 3 none
  reg [47:0] pm0;                                                            // per site: its pulse strand's mate when the turn began
  wire [63:0] bword = hash({1'b1, 1'b0, cz1[9:0], cy1[9:0], cx1[9:0]}, clock);
  integer qi;
  // A site's class at the start of a block: 0 a reaction is ready, 1 it holds agents, 2 only
  // wire, 3 nothing (it takes no turn).
  reg [1:0] rcls;
  always @* begin : ready_class
    reg [SW-1:0] Q;
    Q = site(st, ap_pos);
    rcls = !onl(ap_pos) ? 2'd3 : occ(Q) != 0 ? (ap_found ? 2'd0 : 2'd1) : Q[197:54] != {24{NONE}} ? 2'd2 : 2'd3;
  end
  always @(posedge clk) begin
    if (rst) begin
      state <= S_IDLE; busy <= 0; st <= 0; taken <= 0; touched <= 0; stale <= 0; cycles <= 0;
    end else begin
      if (busy) cycles <= cycles + 1;
      case (state)
        S_IDLE: begin
          if (ld_all) st <= ld_data;
          else if (wr_en) st <= put(st, wr_pos, wr_data);
          if (go_turn) begin
            p <= turn_pos; taken <= turn_taken; block_mode <= 0; busy <= 1; cycles <= 1; state <= S_T_BEGIN;
          end else if (go_block) begin
            taken <= 0; block_mode <= 1; busy <= 1; cycles <= 1; bi <= 0; ap_pos <= 0; rot <= bword[2:0]; state <= S_B_READY;
          end
        end
        S_B_READY: begin
          // Each site's class for the order of turns.
          cls[ap_pos*2 +: 2] <= rcls;
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
            if (bi == 0) $display("block clock %0d rot %0d classes %b", clock, rot, cls);
            if (cls[((bi[2:0] + rot) & 3'd7)*2 +: 2] == bi[4:3] && !taken[(bi[2:0] + rot) & 3'd7]) $display("  turn at %0d", (bi[2:0] + rot) & 3'd7);
`endif
          end
        end
        S_T_BEGIN: begin
          dice <= hash(key(p), clock); touched <= 0; stale <= 0; inf <= 0; active_mode <= 0; ap_pos <= p;
          for (qi = 0; qi < 8; qi = qi + 1) pm0[qi*6 +: 6] <= gm(site(st, qi), gpul(site(st, qi)));
          state <= S_T_COLLECT;
        end
        S_T_COLLECT: begin
          if (c_stale) begin stale <= 1; state <= S_T_SETTLE; end
          else if (c_apply) begin
            st[p*SW +: SW] <= c_s; if (c_two) st[c_dpos*SW +: SW] <= c_d;
            touched <= c_touched; state <= S_T_SETTLE;
          end
          else begin
            // A wanted reader takes the turn (`active_turn`) if the dice say so and there is one.
            active_mode <= d_active < CH_ACTIVE && (
              (gt(site(st, p), 0) != 0 && is_consumer(gt(site(st, p), 0)) && gt(site(st, p), 0) != T_EPS && gw(site(st, p), 0)) ||
              (gt(site(st, p), 1) != 0 && is_consumer(gt(site(st, p), 1)) && gt(site(st, p), 1) != T_EPS && gw(site(st, p), 1)) ||
              (gt(site(st, p), 2) != 0 && is_consumer(gt(site(st, p), 2)) && gt(site(st, p), 2) != T_EPS && gw(site(st, p), 2)));
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
          if (fs_stale) begin stale <= 1; state <= S_T_SETTLE; end
          else if (fs_nreg == 0) state <= S_T_INFECT;
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
          $display("fire clock %0d pos %0d region %0d fx %0d fy %0d ploc %0d pos %o best %o cost %0d n %0d nl %0d", clock, p, fr, fp_fx, fp_fy, fp_ploc, fp_pos, best_code, best_cost, fs_n, fs_nl);
`endif
          aq <= i_q; aloc <= i_loc; aseat <= i_seat; alanes <= i_lanes; aptr <= i_ptr; atch <= i_tch; aj <= 0;
          state <= fs_nl == 0 ? S_F_WB : S_F_LINK;
        end
        S_F_LINK: begin
          aq <= l_q; aptr <= l_ptr; atch <= l_tch; aj <= aj + 1;
          if (aj + 1 == fs_nl) state <= S_F_WB;
        end
        S_F_WB: begin
          for (qi = 0; qi < 4; qi = qi + 1) if (atch[qi]) st[fp_pos[qi*3 +: 3]*SW +: SW] <= aq[qi*SW +: SW];
          touched <= (atch[0] ? 8'd1 << fp_pos[2:0] : 8'd0) | (atch[1] ? 8'd1 << fp_pos[5:3] : 8'd0)
                   | (atch[2] ? 8'd1 << fp_pos[8:6] : 8'd0) | (atch[3] ? 8'd1 << fp_pos[11:9] : 8'd0);
          state <= S_T_SETTLE;
        end
        S_T_INFECT: begin
          // The reader an active turn moves is chosen before the infection (`active_turn`).
          act_k <= act_k_now;
          // A reader that touched a pending computation wants it now (`take_infect`).
          if (inf && !gw(site(st, inf_pos), inf_k)) begin
            st[inf_pos*SW + 210 + inf_k] <= 1'b1; touched[inf_pos] <= 1'b1;
          end
          state <= S_T_MOVE;
        end
        S_T_MOVE: begin
          for (qi = 0; qi < 4; qi = qi + 1) if (qi < mv_n) st[mv_pos[qi*3 +: 3]*SW +: SW] <= mv_dat[qi*SW +: SW];
          touched <= touched | mv_touched; stale <= mv_stale; state <= S_T_SETTLE;
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
