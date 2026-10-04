// The whole lattice: W×H×D sites held in 2×2×2 block units, run one clock at a time exactly as
// crate/src/lattice.rs `margolus_clock` with `block_moves` does.
//
// The block units sit still. A clock's random offset o picks which 2×2×2 blocks move together;
// instead of moving the partition, the lattice's contents shift up by o (a cycle per axis), every
// unit runs its block, the contents shift back, and then every pulse crosses its strand. Every
// wire here runs between neighbouring sites. The physical array is one site larger than the
// lattice on each axis, rounded up to whole blocks; the extra sites are always empty.
module strands_lattice #(parameter W = 16, H = 16, D = 4) (
  input  wire          clk,
  input  wire          rst,
  // Backdoor to sites by lattice coordinates, while idle (loading and reading back).
  input  wire          wr_en,
  input  wire [10:0]   wr_x, wr_y, wr_z,
  input  wire [218:0]  wr_data,
  input  wire [10:0]   rd_x, rd_y, rd_z,
  output wire [218:0]  rd_data,
  input  wire [31:0]   seed,                     // mixed into the clock wherever it makes random bits
  input  wire          step,                     // run one clock
  output reg           busy,
  output reg  [31:0]   clock,
  output reg  [31:0]   cycles                    // cycles the last clock took
);
`include "strands_tables.vh"
`include "strands_fn.vh"
  localparam BX = (W + 2) / 2, BY = (H + 2) / 2, BZ = (D + 2) / 2, NB = BX * BY * BZ;
  localparam PX = 2 * BX, PY = 2 * BY, PZ = 2 * BZ, NP = PX * PY * PZ;
  localparam [SW-1:0] EMPTY = {6'd63, 3'd0, 12'd0, {33{6'd63}}};

  // Each physical site's state, from its unit.
  wire [SW-1:0] ps [0:NP-1];
  wire [NB-1:0] ubusy;
  wire [SW-1:0] nx [0:NP-1];          // what each site loads this cycle
  wire ld, go;
  reg  [2:0] o;                        // this clock's offset
  genvar ux, uy, uz, q;
  generate
    for (uz = 0; uz < BZ; uz = uz + 1) for (uy = 0; uy < BY; uy = uy + 1) for (ux = 0; ux < BX; ux = ux + 1) begin : u
      localparam integer UI = (uz * BY + uy) * BX + ux;
      wire [8*SW-1:0] so, li;
      for (q = 0; q < 8; q = q + 1) begin : s
        localparam integer PI = ((2 * uz + q / 4) * PY + (2 * uy + q / 2 % 2)) * PX + 2 * ux + q % 2;
        assign ps[PI] = so[q*SW +: SW];
        assign li[q*SW +: SW] = nx[PI];
      end
      wire hit = wr_en && wr_x[10:1] == ux && wr_y[10:1] == uy && wr_z[10:1] == uz;
      wire ubusy_;
      strands_block blk (
        .clk(clk), .rst(rst),
        .wr_en(hit), .wr_pos({wr_z[0], wr_y[0], wr_x[0]}), .wr_data(wr_data),
        .ld_all(ld), .ld_data(li), .st_out(so),
        .go_turn(1'b0), .go_block(go), .turn_pos(3'd0), .turn_taken(8'd0),
        .clock(clock ^ seed),
        .cx1(11'd2 * ux - o[0] + 11'd1), .cy1(11'd2 * uy - o[1] + 11'd1), .cz1(11'd2 * uz - o[2] + 11'd1),
        .lw(W[10:0]), .lh(H[10:0]), .ld(D[10:0]),
        .busy(ubusy_), .touched(), .stale(), .taken(), .cycles());
      assign ubusy[UI] = ubusy_;
    end
  endgenerate
  assign rd_data = ps[(rd_z * PY + rd_y) * PX + rd_x];

  // ---- the phases of a clock ----------------------------------------------------------------
  localparam IDLE = 0, SIN = 1, RUN = 2, WAIT = 3, SOUT = 4, PULSE = 5;
  reg [2:0] phase; reg [1:0] axis;
  // Each site's next state: shifted from a neighbour, or after the pulse phase.
  genvar gx, gy, gz;
  generate
    for (gz = 0; gz < PZ; gz = gz + 1) for (gy = 0; gy < PY; gy = gy + 1) for (gx = 0; gx < PX; gx = gx + 1) begin : n
      localparam integer I = (gz * PY + gy) * PX + gx;
      wire [6*SW-1:0] nb;
      wire [5:0] on;   // which neighbours are on the lattice
      assign nb[0*SW +: SW] = gx + 1 < PX ? ps[I + 1] : EMPTY;
      assign nb[1*SW +: SW] = gx > 0 ? ps[I - 1] : EMPTY;
      assign nb[2*SW +: SW] = gy + 1 < PY ? ps[I + PX] : EMPTY;
      assign nb[3*SW +: SW] = gy > 0 ? ps[I - PX] : EMPTY;
      assign nb[4*SW +: SW] = gz + 1 < PZ ? ps[I + PX * PY] : EMPTY;
      assign nb[5*SW +: SW] = gz > 0 ? ps[I - PX * PY] : EMPTY;
      assign on = {gz > 0 && gz < D, gz + 1 < D, gy > 0 && gy < H, gy + 1 < H, gx > 0 && gx < W, gx + 1 < W};
      wire [SW-1:0] next;
      strands_site_net sn (.s(ps[I]), .nb(nb), .on(on), .here(gx < W && gy < H && gz < D),
                           .pulse(phase == PULSE), .up(phase == SIN), .axis(axis), .next(next));
      assign nx[I] = next;
    end
  endgenerate

  // Sites load in the cycle that computes what they load; units start in RUN.
  assign ld = ((phase == SIN || phase == SOUT) && o[axis]) || phase == PULSE;
  assign go = phase == RUN;
  wire [63:0] g = hash(32'hFFFFFFFF, clock ^ seed);
  wire [2:0] o_now = {D > 1 ? g[16] : 1'b0, g[8], g[0]};

  always @(posedge clk) begin
    if (rst) begin phase <= IDLE; busy <= 0; clock <= 0; cycles <= 0; o <= 0; end
    else begin
      if (busy) cycles <= cycles + 1;
      case (phase)
        IDLE: if (step) begin busy <= 1; cycles <= 1; o <= o_now; axis <= 0; phase <= SIN; end
        SIN: if (axis == 2'd2) phase <= RUN; else axis <= axis + 1;
        RUN: phase <= WAIT;
        WAIT: if (ubusy == 0) begin axis <= 0; phase <= SOUT; end
        SOUT: if (axis == 2'd2) phase <= PULSE; else axis <= axis + 1;
        PULSE: begin phase <= IDLE; busy <= 0; clock <= clock + 1; end
        default: phase <= IDLE;
      endcase
    end
  end
endmodule

// A site's next state in a shift (from the neighbour below along `axis` when moving up, above
// when moving down) or in the pulse phase (lattice.rs `step_pulses`): pulses pointing at the site
// arrive, the one through its lowest face winning; a pulse reaching a computation's output wants
// it; a wanted reader with no pulse in its site sends one along its principal wire.
module strands_site_net (
  input  wire [218:0]   s,
  input  wire [6*219-1:0] nb,
  input  wire [5:0]     on,
  input  wire           here,
  input  wire           pulse,
  input  wire           up,
  input  wire [1:0]     axis,
  output reg  [218:0]   next
);
`include "strands_tables.vh"
`include "strands_fn.vh"
  /* verilator no_inline_module */
  always @* begin : site_net
    reg [SW-1:0] S, N; reg [5:0] pe, m, arr; reg [1:0] k, qq; reg got, sent; integer g, kk;
    S = 0; N = 0; pe = 0; m = 0; arr = 0; k = 0; qq = 0; got = 0; sent = 0;
    if (!pulse) next = up ? nb[(2 * axis + 1)*SW +: SW] : nb[(2 * axis)*SW +: SW];
    else if (!here) next = s;
    else begin
      S = s; arr = NONE;
      for (g = 0; g < 6; g = g + 1) if (on[g]) begin
        N = nb[g*SW +: SW]; pe = gpul(N);
        if (is_strand(pe) && face(pe) == (g ^ 1)) begin
          m = gm(S, se(g, lane(pe)));
          if (is_strand(m)) begin if (!got) begin got = 1; arr = m; end end
          else if (m != NONE) begin
            k = port_k(m); qq = port_q(m);
            if (qq != 0 && gt(S, k) != 0 && is_consumer(gt(S, k)) && !gw(S, k)) S = sw(S, k, 1'b1);
          end
        end
      end
      for (kk = 0; kk < 3; kk = kk + 1)
        if (!got && !sent && occ(S) != 0 && gt(S, kk) != 0 && is_consumer(gt(S, kk)) && gt(S, kk) != T_EPS && gw(S, kk)
            && is_strand(gm(S, ae(kk, 0)))) begin arr = gm(S, ae(kk, 0)); sent = 1; end
      S[213 +: 6] = arr;
      next = S;
    end
  end
endmodule
