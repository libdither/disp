// Moves on one or a few sites, included inside the block unit. Each mirrors the function of the
// same name in crate/src/lattice.rs, writing switchboard entries in the same order.

/// Metropolis in integers: a rise of de quarter units passes when 16 random bits are below
/// accept_thr(de).
function accept(input integer de, input [15:0] metro);
  accept = de <= 0 || (de < ACCEPT_LEN && metro < accept_thr(de[7:0]));
endfunction

/// Agent k of site S steps across face f into slot k2 of the neighbour T. With `metropolis`,
/// the step must leave T room and pass the energy test. Returns {ok, de[15:0], S', T'}.
function [2*SW+16:0] hop_to(input [SW-1:0] S0, input [SW-1:0] T0, input [1:0] k, input [2:0] f,
                            input [1:0] k2, input metropolis, input [15:0] metro);
  reg [SW-1:0] S, T;
  reg [3:0] tg; reg [1:0] a; reg wnt, idl, fail, back;
  reg [5:0] pk, pv;             // per port: plan (0 drag, 1 loop, 2 through) and its lane or loop end
  reg [5:0] m, mt;
  reg [13:0] lanes;             // through lanes in port order, then free lanes: 2 bits each
  reg [17:0] tgt, dm;           // per port: through target, dragged mate
  reg [5:0] dj;                 // per port: lane a dragged wire takes
  reg [2:0] f1;
  integer q, r, i, need, de, nl, li, loops, through, w, n, n2, ps, pt, ds, dt;
  begin
    S = S0; T = T0; f1 = f ^ 3'd1;
    tg = gt(S, k); a = arity(tg); wnt = gw(S, k); idl = LAZY && !wnt;
    need = 0; de = 0; pk = 0; pv = 0; fail = 0;
    for (q = 0; q < 3; q = q + 1) if (q < a) begin
      m = gm(S, ae(k, q));
      w = q == 0 ? (idl ? E_PRINCIPAL_IDLE : E_PRINCIPAL) : (idl ? E_AUX_IDLE : E_AUX);
      if (m < 6'd9 && port_k(m) == k) begin pk[q*2 +: 2] = 2'd1; pv[q*2 +: 2] = port_q(m); end
      else if (is_strand(m) && face(m) == f) begin pk[q*2 +: 2] = 2'd2; pv[q*2 +: 2] = lane(m); de = de - w; end
      else begin need = need + 1; de = de + w; end
    end
    nl = 0; lanes = 0;
    for (q = 0; q < 3; q = q + 1) if (q < a && pk[q*2 +: 2] == 2'd2) begin lanes[nl*2 +: 2] = pv[q*2 +: 2]; nl = nl + 1; end
    for (i = 0; i < 4; i = i + 1) if (gm(S, se(f, i)) == NONE) begin lanes[nl*2 +: 2] = i; nl = nl + 1; end
    if (nl < need) fail = 1;
    loops = 0; through = 0;
    for (q = 0; q < 3; q = q + 1) if (q < a) begin
      if (pk[q*2 +: 2] == 2'd1) loops = loops + 1;
      if (pk[q*2 +: 2] == 2'd2) through = through + 1;
    end
    loops = loops / 2;
    if (metropolis && pairs(T) + need + loops > PAIRS) fail = 1;
    de = de + E_CROWD * (occ(T) - (occ(S) - 1));
    if (E_IDLE != 0 && idle(S, k)) de = de + E_IDLE * (idle_at(T) - (idle_at(S) - 1));
    if (E_LINK != 0) begin n = used_lanes(S, f); n2 = n - through + need; de = de + E_LINK * (n2 * n2 - n * n); end
    if (E_BOARD != 0) begin
      ps = pairs(S); pt = pairs(T); ds = -(through + loops); dt = need + loops;
      de = de + E_BOARD * ((ps + ds) * (ps + ds) - ps * ps + (pt + dt) * (pt + dt) - pt * pt);
    end
    if (metropolis && !accept(de, metro)) fail = 1;
    // Where the through-wires land inside T: a wire coming back to another port of the agent
    // becomes a loop.
    tgt = {3{NONE}};
    for (q = 0; q < 3; q = q + 1) if (q < a && pk[q*2 +: 2] == 2'd2) begin
      mt = gm(T, se(f1, pv[q*2 +: 2]));
      back = 0; tgt[q*6 +: 6] = mt;
      if (is_strand(mt) && face(mt) == f1)
        for (r = 2; r >= 0; r = r - 1) if (r < a && pk[r*2 +: 2] == 2'd2 && pv[r*2 +: 2] == lane(mt)) tgt[q*6 +: 6] = ae(k2, r);
    end
    li = 0; dm = {3{NONE}}; dj = 0;
    for (q = 0; q < 3; q = q + 1) if (q < a) begin
      if (pk[q*2 +: 2] == 2'd2) begin
        S = sm(S, se(f, pv[q*2 +: 2]), NONE);
        T = sm(T, se(f1, pv[q*2 +: 2]), NONE);
      end else if (pk[q*2 +: 2] == 2'd0) begin
        dm[q*6 +: 6] = gm(S, ae(k, q)); dj[q*2 +: 2] = lanes[li*2 +: 2]; li = li + 1;
      end
    end
    for (q = 0; q < 3; q = q + 1) if (q < a) S = sm(S, ae(k, q), NONE);
    S = stg(S, k, 4'd0); S = sw(S, k, 1'b0);
    T = stg(T, k2, tg); T = sw(T, k2, wnt);
    for (q = 0; q < 3; q = q + 1) if (q < a && pk[q*2 +: 2] == 2'd0) begin
      S = lk(S, dm[q*6 +: 6], se(f, dj[q*2 +: 2]));
      T = lk(T, se(f1, dj[q*2 +: 2]), ae(k2, q));
    end
    for (q = 0; q < 3; q = q + 1) if (q < a) begin
      if (pk[q*2 +: 2] == 2'd1) T = lk(T, ae(k2, q), ae(k2, pv[q*2 +: 2]));
      if (pk[q*2 +: 2] == 2'd2) T = lk(T, ae(k2, q), tgt[q*6 +: 6]);
    end
    hop_to = {!fail, de[15:0], S, T};
  end
endfunction

/// Move the agent in slot `from` of S to the empty slot `to`, keeping its wires.
function [SW-1:0] relocate(input [SW-1:0] S0, input [1:0] from, input [1:0] to);
  reg [SW-1:0] S; reg [1:0] a; reg [17:0] mates; reg [5:0] m; integer q;
  begin
    S = S0; a = arity(gt(S, from));
    for (q = 0; q < 3; q = q + 1) mates[q*6 +: 6] = gm(S, ae(from, q));
    for (q = 0; q < 3; q = q + 1) if (q < a) S = sm(S, ae(from, q), NONE);
    S = stg(S, to, gt(S0, from)); S = sw(S, to, gw(S0, from));
    S = stg(S, from, 4'd0); S = sw(S, from, 1'b0);
    for (q = 0; q < 3; q = q + 1) if (q < a) begin
      m = mates[q*6 +: 6];
      if (m < 6'd9 && port_k(m) == from) m = ae(to, port_q(m));
      S = lk(S, ae(to, q), m);
    end
    relocate = S;
  end
endfunction

/// Agent ka of S and a resident of the full neighbour T trade places, as two steps through the
/// transient slot judged by their total energy. Returns {ok, S', T'}.
function [2*SW:0] swap(input [SW-1:0] S0, input [SW-1:0] T0, input [1:0] ka, input [2:0] f,
                       input [2:0] resident, input [15:0] metro);
  reg [SW-1:0] S, T; reg [2*SW+16:0] h1, h2; reg [1:0] kb; reg [3:0] nres; reg ok; integer d1, d2;
  begin
    nres = (gt(T0, 0) != 0) + (gt(T0, 1) != 0);
    kb = pick3(resident, nres) == 0 ? (gt(T0, 0) != 0 ? 2'd0 : 2'd1) : 2'd1;
    h1 = hop_to(S0, T0, ka, f, 2'd2, 1'b0, 16'd0);
    h2 = hop_to(h1[SW-1:0], h1[2*SW-1:SW], kb, f ^ 3'd1, ka, 1'b0, 16'd0);
    d1 = $signed(h1[2*SW +: 16]); d2 = $signed(h2[2*SW +: 16]);
    S = h2[SW-1:0]; T = relocate(h2[2*SW-1:SW], 2'd2, kb);
    ok = nres != 0 && h1[2*SW+16] && h2[2*SW+16] && pairs(S) <= PAIRS && pairs(T) <= PAIRS && accept(d1 + d2, metro);
    swap = {ok, S, T};
  end
endfunction

/// A wire leaving S on face f lane i and coming straight back from T on lane j snaps shut.
/// Returns {ok, S', T'}.
function [2*SW:0] fold(input [SW-1:0] S0, input [SW-1:0] T0, input [5:0] e);
  reg [SW-1:0] S, T; reg [2:0] f, f1; reg [1:0] i, j; reg [5:0] mt, a, b;
  begin
    f = face(e); f1 = f ^ 3'd1; i = lane(e);
    mt = gm(T0, se(f1, i)); j = lane(mt);
    a = gm(S0, se(f, i)); b = gm(S0, se(f, j));
    S = sm(sm(S0, se(f, i), NONE), se(f, j), NONE);
    T = sm(sm(T0, se(f1, i), NONE), se(f1, j), NONE);
    S = lk(S, a, b);
    fold = {is_strand(mt) && face(mt) == f1 && a != se(f, j), S, T};
  end
endfunction

/// A wire turning a corner at S (in on face f1 from X, out on face f2 to Z) moves to the opposite
/// corner W of the square. Returns {ok, S', X', Z', W'}.
function [4*SW:0] flip(input [SW-1:0] S0, input [SW-1:0] X0, input [SW-1:0] Z0, input [SW-1:0] W0,
                       input [5:0] e, input [15:0] metro);
  reg [SW-1:0] S, X, Z, W; reg [2:0] f1, f2, c, d; reg [1:0] i, j; reg [5:0] m, a, g; integer de;
  begin
    m = gm(S0, e); f1 = face(e); i = lane(e); f2 = face(m); j = lane(m);
    c = free_lane(X0, f2); d = free_lane(W0, f1 ^ 3'd1);
    de = 2 * E_LINK * (used_lanes(X0, f2) + used_lanes(W0, f1 ^ 3'd1) - used_lanes(S0, f1) - used_lanes(S0, f2) + 2)
       + 2 * E_BOARD * (pairs(W0) - pairs(S0) + 1);
    a = gm(X0, se(f1 ^ 3'd1, i)); g = gm(Z0, se(f2 ^ 3'd1, j));
    S = sm(sm(S0, e, NONE), m, NONE);
    X = sm(X0, se(f1 ^ 3'd1, i), NONE); Z = sm(Z0, se(f2 ^ 3'd1, j), NONE); W = W0;
    X = lk(X, a, se(f2, c[1:0]));
    W = lk(W, se(f2 ^ 3'd1, c[1:0]), se(f1 ^ 3'd1, d[1:0]));
    Z = lk(Z, se(f1, d[1:0]), g);
    flip = {c[2] && d[2] && pairs(W0) + 1 <= PAIRS && accept(de, metro), S, X, Z, W};
  end
endfunction
