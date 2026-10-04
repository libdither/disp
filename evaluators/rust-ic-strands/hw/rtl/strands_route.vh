// Routes inside a rewrite's square, included in modules with ports fp_ploc, fp_fx, fp_fy, fs_nl,
// fs_la, fs_lb.
  /// The route from square position a to b: {steps, step 1, step 0}, a step being
  /// {site, edge, direction (0 fx, 1 back along fx, 2 fy, 3 back along fy)}.
  function [13:0] route(input [1:0] a, input [1:0] b);
    case ({a, b})
      4'b0001: route = {2'd1, 6'd0, 2'd0, 2'd0, 2'd0};
      4'b0100: route = {2'd1, 6'd0, 2'd1, 2'd0, 2'd1};
      4'b0010: route = {2'd1, 6'd0, 2'd0, 2'd1, 2'd2};
      4'b1000: route = {2'd1, 6'd0, 2'd2, 2'd1, 2'd3};
      4'b0111: route = {2'd1, 6'd0, 2'd1, 2'd2, 2'd2};
      4'b1101: route = {2'd1, 6'd0, 2'd3, 2'd2, 2'd3};
      4'b1011: route = {2'd1, 6'd0, 2'd2, 2'd3, 2'd0};
      4'b1110: route = {2'd1, 6'd0, 2'd3, 2'd3, 2'd1};
      4'b0011: route = {2'd2, 2'd1, 2'd2, 2'd2, 2'd0, 2'd0, 2'd0};
      4'b1100: route = {2'd2, 2'd1, 2'd0, 2'd1, 2'd3, 2'd2, 2'd3};
      4'b0110: route = {2'd2, 2'd0, 2'd1, 2'd2, 2'd1, 2'd0, 2'd1};
      4'b1001: route = {2'd2, 2'd0, 2'd0, 2'd0, 2'd2, 2'd1, 2'd3};
      default: route = 14'd0;
    endcase
  endfunction
  function [2:0] dir_face(input [1:0] dir);
    dir_face = dir == 2'd0 ? fp_fx : dir == 2'd1 ? fp_fx ^ 3'd1 : dir == 2'd2 ? fp_fy : fp_fy ^ 3'd1;
  endfunction
  /// Where a link end sits in the square, for a seating.
  function [1:0] at_(input [8:0] t, input [11:0] loc);
    at_ = t[8] ? (t[7] ? fp_ploc : 2'd0) : loc[t[5:3]*2 +: 2];
  endfunction

  /// For a seating: the new strands per square edge (4 bits each: up to one per link), and the
  /// pairings each site gains (6 bits each): {add[23:0], need[15:0]}.
  function [39:0] need_add(input [11:0] loc);
    reg [13:0] r; reg [1:0] la, lb, x; reg [3:0] nd0, nd1, nd2, nd3; reg [5:0] add0, add1, add2, add3; integer j, s_;
    begin
      nd0 = 0; nd1 = 0; nd2 = 0; nd3 = 0; add0 = 0; add1 = 0; add2 = 0; add3 = 0; r = 0; la = 0; lb = 0; x = 0;
      for (j = 0; j < 9; j = j + 1) if (j < fs_nl) begin
        la = at_(fs_la[j*9 +: 9], loc); lb = at_(fs_lb[j*9 +: 9], loc);
        r = route(la, lb);
        for (s_ = 0; s_ < 2; s_ = s_ + 1) if (s_ < r[13:12]) begin
          case (r[s_*6+2 +: 2]) 2'd0: nd0 = nd0 + 1; 2'd1: nd1 = nd1 + 1; 2'd2: nd2 = nd2 + 1; default: nd3 = nd3 + 1; endcase
          x = r[s_*6+4 +: 2];
          case (x) 2'd0: add0 = add0 + 1; 2'd1: add1 = add1 + 1; 2'd2: add2 = add2 + 1; default: add3 = add3 + 1; endcase
        end
        case (lb) 2'd0: add0 = add0 + 1; 2'd1: add1 = add1 + 1; 2'd2: add2 = add2 + 1; default: add3 = add3 + 1; endcase
      end
      need_add = {add3, add2, add1, add0, nd3, nd2, nd1, nd0};
    end
  endfunction

  function [SW-1:0] q4(input [4*SW-1:0] Q, input [1:0] l);
    q4 = l == 2'd0 ? Q[0 +: SW] : l == 2'd1 ? Q[SW +: SW] : l == 2'd2 ? Q[2*SW +: SW] : Q[3*SW +: SW];
  endfunction
  function [4*SW-1:0] p4(input [4*SW-1:0] Q, input [1:0] l, input [SW-1:0] v);
    begin p4 = Q; if (l == 2'd0) p4[0 +: SW] = v; else if (l == 2'd1) p4[SW +: SW] = v; else if (l == 2'd2) p4[2*SW +: SW] = v; else p4[3*SW +: SW] = v; end
  endfunction
