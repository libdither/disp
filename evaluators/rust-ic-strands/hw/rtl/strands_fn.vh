// Helpers for the block unit, included inside the module. Names follow crate/src/lattice.rs.
//
// A site, as the chip holds it (219 bits):
//   [  0 +: 198]  switchboard: the mate of each of 33 ends, 6 bits each (63: none). Ends 0..8 are
//                 agent ports (slot * 3 + port; slot 2 is the transient slot of an exchange),
//                 ends 9..32 strand ends (9 + face * 4 + lane).
//   [198 +:  12]  the three slots' tags, 4 bits each (0: empty)
//   [210 +:   3]  the three slots' wanted bits
//   [213 +:   6]  the strand end a demand pulse sits on (63: none)
localparam SW = 219, NE = 33, NONE = 6'd63;
localparam [3:0] T_L = 1, T_S = 2, T_F = 3, T_P = 4, T_PAIR = 5, T_A = 6, T_T1 = 7, T_SEL = 8,
                 T_UNP = 9, T_DN = 10, T_EPS = 11, T_NRM = 12, T_OUT = 13;

function [5:0] gm(input [SW-1:0] S, input [5:0] e);
  integer j;
  begin gm = NONE; for (j = 0; j < NE; j = j + 1) if (e == j) gm = S[j*6 +: 6]; end
endfunction
function [SW-1:0] sm(input [SW-1:0] S, input [5:0] e, input [5:0] v);
  integer j;
  begin sm = S; for (j = 0; j < NE; j = j + 1) if (e == j) sm[j*6 +: 6] = v; end
endfunction
function [SW-1:0] lk(input [SW-1:0] S, input [5:0] a, input [5:0] b);
  lk = sm(sm(S, a, b), b, a);
endfunction
function [3:0] gt(input [SW-1:0] S, input [1:0] k);
  gt = k == 2'd0 ? S[198 +: 4] : k == 2'd1 ? S[202 +: 4] : S[206 +: 4];
endfunction
function [SW-1:0] stg(input [SW-1:0] S, input [1:0] k, input [3:0] v);
  begin stg = S; if (k == 2'd0) stg[198 +: 4] = v; else if (k == 2'd1) stg[202 +: 4] = v; else stg[206 +: 4] = v; end
endfunction
function gw(input [SW-1:0] S, input [1:0] k);
  gw = k == 2'd0 ? S[210] : k == 2'd1 ? S[211] : S[212];
endfunction
function [SW-1:0] sw(input [SW-1:0] S, input [1:0] k, input v);
  begin sw = S; if (k == 2'd0) sw[210] = v; else if (k == 2'd1) sw[211] = v; else sw[212] = v; end
endfunction
function [5:0] gpul(input [SW-1:0] S); gpul = S[213 +: 6]; endfunction

function is_strand(input [5:0] e); is_strand = e != NONE && e >= 6'd9; endfunction
function [2:0] face(input [5:0] e);
  reg [5:0] d; begin d = e - 6'd9; face = d[4:2]; end
endfunction
function [1:0] lane(input [5:0] e);
  reg [5:0] d; begin d = e - 6'd9; lane = d[1:0]; end
endfunction
function [5:0] se(input [2:0] f, input [1:0] i); se = 6'd9 + {1'b0, f, 2'b00} + {4'd0, i}; endfunction
function [5:0] ae(input [1:0] k, input [1:0] q); ae = {4'd0, k} * 6'd3 + {4'd0, q}; endfunction
function [1:0] port_k(input [5:0] e); port_k = e >= 6'd6 ? 2'd2 : e >= 6'd3 ? 2'd1 : 2'd0; endfunction
function [1:0] port_q(input [5:0] e); port_q = e - {port_k(e), 1'b0} - {4'd0, port_k(e)}; endfunction

function is_consumer(input [3:0] t); is_consumer = t >= T_A && t <= T_NRM; endfunction
function is_producer(input [3:0] t); is_producer = t >= T_L && t <= T_PAIR; endfunction
function [1:0] arity(input [3:0] t);
  arity = (t == T_L || t == T_EPS || t == T_OUT) ? 2'd1 : (t == T_S || t == T_NRM) ? 2'd2 : 2'd3;
endfunction
/// Wanted from the start: the normalizer, the root and erasers.
function born_wanted(input [3:0] t); born_wanted = t == T_NRM || t == T_OUT || t == T_EPS; endfunction

function [1:0] occ(input [SW-1:0] S);
  occ = (gt(S, 0) != 0) + (gt(S, 1) != 0) + (gt(S, 2) != 0);
endfunction
function [4:0] pairs(input [SW-1:0] S);
  integer j; reg [5:0] n;
  begin n = 0; for (j = 0; j < NE; j = j + 1) if (S[j*6 +: 6] != NONE) n = n + 1; pairs = n[5:1]; end
endfunction
function idle(input [SW-1:0] S, input [1:0] k); idle = gt(S, k) != 0 && !gw(S, k); endfunction
function [1:0] idle_at(input [SW-1:0] S); idle_at = idle(S, 0) + idle(S, 1) + idle(S, 2); endfunction
function [2:0] used_lanes(input [SW-1:0] S, input [2:0] f);
  integer i; reg [2:0] n;
  begin n = 0; for (i = 0; i < 4; i = i + 1) if (gm(S, se(f, i)) != NONE) n = n + 1; used_lanes = n; end
endfunction
/// {found, lane}: the lowest free lane on face f.
function [2:0] free_lane(input [SW-1:0] S, input [2:0] f);
  integer i;
  begin free_lane = 0; for (i = 3; i >= 0; i = i - 1) if (gm(S, se(f, i)) == NONE) free_lane = {1'b1, i[1:0]}; end
endfunction
/// {found, slot}: the lowest free slot among the two real ones.
function [2:0] free_slot(input [SW-1:0] S);
  free_slot = gt(S, 0) == 0 ? 3'b100 : gt(S, 1) == 0 ? 3'b101 : 3'b000;
endfunction
/// pick(n bits of field, among): ((bits * among) >> n).
function [4:0] pick5(input [4:0] bits, input [4:0] among);
  reg [9:0] m; begin m = bits * among; pick5 = m[9:5]; end
endfunction
function [3:0] pick3(input [2:0] bits, input [3:0] among);
  reg [6:0] m; begin m = bits * among; pick3 = m[6:3]; end
endfunction

/// The block position across face f from position q (x + 2y + 4z): {inside the block, position}.
function [3:0] nbq(input [2:0] q, input [2:0] f);
  case (f)
    3'd0: nbq = q[0] ? 4'd0 : {1'b1, q | 3'b001};
    3'd1: nbq = q[0] ? {1'b1, q & 3'b110} : 4'd0;
    3'd2: nbq = q[1] ? 4'd0 : {1'b1, q | 3'b010};
    3'd3: nbq = q[1] ? {1'b1, q & 3'b101} : 4'd0;
    3'd4: nbq = q[2] ? 4'd0 : {1'b1, q | 3'b100};
    3'd5: nbq = q[2] ? {1'b1, q & 3'b011} : 4'd0;
    default: nbq = 4'd0;
  endcase
endfunction

/// The random word for a key at a clock: eight add-rotate-xor rounds (lattice.rs `hash`).
function [63:0] hash(input [31:0] hk, input [31:0] hc);
  reg [31:0] x, y; integer i;
  begin
    x = hc; y = hk;
    for (i = 0; i < 8; i = i + 1) begin
      x = ({x[7:0], x[31:8]} + y) ^ hk ^ (i * 32'h9E3779B9);
      y = {y[28:0], y[31:29]} ^ x;
    end
    hash = {x, y};
  end
endfunction
