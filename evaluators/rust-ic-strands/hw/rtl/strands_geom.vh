// The block's sites and its edges, included in modules with an input lat: bit q*6 + f says whether
// position q's neighbour across face f is on the lattice (see strands_block.v).
  function [SW-1:0] site(input [8*SW-1:0] B, input [2:0] q);
    integer j; begin site = B[SW-1:0]; for (j = 1; j < 8; j = j + 1) if (q == j) site = B[j*SW +: SW]; end
  endfunction

  /// Whether position q's neighbour across face f is on the lattice.
  function onlat(input [2:0] q, input [2:0] f); onlat = f < 3'd6 && lat[q*6 + f]; endfunction
  /// Position q's neighbour across f if it is in the block (and so on the lattice): {yes, position}.
  function [3:0] inb(input [2:0] q, input [2:0] f);
    reg [3:0] n; begin n = nbq(q, f); inb = n[3] && onlat(q, f) ? n : 4'd0; end
  endfunction
