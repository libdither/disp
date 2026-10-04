// The block's sites and its place in the lattice, included in modules with ports cx1, cy1, cz1,
// lw, lh, ld (see strands_block.v).
  function [SW-1:0] site(input [8*SW-1:0] B, input [2:0] q);
    integer j; begin site = B[SW-1:0]; for (j = 1; j < 8; j = j + 1) if (q == j) site = B[j*SW +: SW]; end
  endfunction

  // ---- geometry -----------------------------------------------------------------------------
  /// Whether position q's neighbour across face f is on the lattice.
  function onlat(input [2:0] q, input [2:0] f);
    reg [10:0] x, y, z;
    begin
      x = cx1 - 11'd1 + q[0]; y = cy1 - 11'd1 + q[1]; z = cz1 - 11'd1 + q[2];
      case (f)
        3'd0: onlat = x + 11'd1 < lw;  3'd1: onlat = x != 0;
        3'd2: onlat = y + 11'd1 < lh;  3'd3: onlat = y != 0;
        3'd4: onlat = z + 11'd1 < ld;  3'd5: onlat = z != 0;
        default: onlat = 0;
      endcase
    end
  endfunction
  /// Position q's neighbour across f if it is in the block (and so on the lattice): {yes, position}.
  function [3:0] inb(input [2:0] q, input [2:0] f);
    reg [3:0] n; begin n = nbq(q, f); inb = n[3] && onlat(q, f) ? n : 4'd0; end
  endfunction
  function onl(input [2:0] q); onl = cx1 - 11'd1 + q[0] < lw && cy1 - 11'd1 + q[1] < lh && cz1 - 11'd1 + q[2] < ld; endfunction
  function [31:0] key(input [2:0] q);
    key = {2'b00, cz1[9:0] - 10'd1 + q[2], cy1[9:0] - 10'd1 + q[1], cx1[9:0] - 10'd1 + q[0]};
  endfunction

