// The graph view (index.html): the net as it is now, drawn by who is wired to whom rather than
// where agents sit. Each agent is a node and each wire a line, laid out by forces: springs along
// wires, short-range repulsion, a pull that hangs what feeds an input below its reader (so the
// term reads as a tree from the root down), and a weak pull to the middle. Nodes are kept by agent
// id, so the layout carries on as the run plays: a new agent starts where its neighbours are, and
// when the net is renumbered (a GPU hand-back, going back) agents are found again by seat and
// then along their wires (graphRematch).
const GRAPH = { L: null, id: null, index: null, seat: null, tag: null, far: null, renumbered: -1, n: 0, x: null, y: null, vx: null, vy: null, deg: null,
  wa: null, wb: null, src: null, flag: null, wires: 0, alpha: 0, stepMs: 0, fitUntil: 0, fitView: null, grid: null };
/// World units: a wire's rest length (and the room between neighbours in a fresh layout), a
/// node's radius, how near two nodes push each other apart.
const GSP = 30, GRAD = 7, GREP = 30;
/// A number in [-0.5, 0.5) from an agent id: jitter that is the same every time.
const graphJitter = (id, salt) => (Math.imul(id * 2 + salt, 2654435761) >>> 0) / 4294967296 - 0.5;

/// Bring the layout up to date with NET: positions kept by id (across a renumbering, by graphRematch),
/// new agents placed by their neighbours, and agents with no placed neighbour hung as trees beside the rest.
function graphSync() {
  if (!L) return;
  netSync();
  if (GRAPH.L !== L) Object.assign(GRAPH, { L, id: null, index: null, seat: null, n: 0, alpha: 0 });
  if (GRAPH.id === NET.id) return;
  const n = NET.n, x = new Float64Array(n), y = new Float64Array(n), vx = new Float64Array(n), vy = new Float64Array(n), at = new Uint8Array(n);
  const old = new Int32Array(n).fill(-1);
  let kept = 0;
  if (GRAPH.id && GRAPH.renumbered === NET.renumbered) for (let i = 0; i < n; i++) old[i] = GRAPH.index.get(NET.id[i]) ?? -1;
  // Renumbered with most agents not found again (a long jump in time): laid out afresh.
  else if (GRAPH.id && graphRematch(old) < n / 4) old.fill(-1);
  for (let i = 0; i < n; i++) {
    const j = old[i]; if (j < 0) continue;
    x[i] = GRAPH.x[j]; y[i] = GRAPH.y[j]; vx[i] = GRAPH.vx[j]; vy[i] = GRAPH.vy[j]; at[i] = 1; kept++;
  }
  // New agents start where their placed neighbours are: below a reader they feed, above what they read.
  const queue = [];
  for (let i = 0; i < n; i++) if (!at[i]) for (let q = 0; q < 3; q++) { const f = NET.far[3 * i + q]; if (f >= 0 && at[f >> 2]) { queue.push(i); break; } }
  for (let h = 0; h < queue.length; h++) {
    const i = queue[h]; if (at[i]) continue;
    let sx = 0, sy = 0, c = 0;
    for (let q = 0; q < 3; q++) {
      const f = NET.far[3 * i + q], j = f >> 2; if (f < 0 || j === i || !at[j]) continue;
      sx += x[j]; sy += y[j] + (isSource(NET.tag[i], q) ? 0.6 : -0.6) * GSP; c++;
    }
    if (!c) continue;
    x[i] = sx / c + graphJitter(NET.id[i], 0) * GSP * 0.5; y[i] = sy / c + graphJitter(NET.id[i], 1) * GSP * 0.2; at[i] = 1;
    for (let q = 0; q < 3; q++) { const f = NET.far[3 * i + q]; if (f >= 0 && !at[f >> 2]) queue.push(f >> 2); }
  }
  graphHang(at, x, y, kept > 0);
  const changed = (n - kept) + (GRAPH.n - kept), fresh = kept === 0;
  Object.assign(GRAPH, { id: NET.id, index: NET.index, seat: NET.seat, tag: NET.tag, far: NET.far, renumbered: NET.renumbered, n, x, y, vx, vy });
  graphWires();
  GRAPH.alpha = fresh ? 1 : Math.max(GRAPH.alpha, Math.min(0.5, 0.12 + 2 * changed / Math.max(1, n)));
  if (fresh) graphRelax(40, 400);
}

/// Across a renumbering, each agent's old self (its index in the last layout, into `old`): an agent
/// of the same kind in the same seat, then outward along wires, one of the same kind reached by
/// the same port from an agent already found. Returns how many were found.
function graphRematch(old) {
  const taken = new Uint8Array(GRAPH.n), by = new Map(), queue = [];
  GRAPH.seat.forEach((s, j) => by.set(s, j));
  for (let i = 0; i < NET.n; i++) {
    const j = by.get(NET.seat[i]);
    if (j !== undefined && GRAPH.tag[j] === NET.tag[i]) { old[i] = j; taken[j] = 1; queue.push(i); }
  }
  for (let h = 0; h < queue.length; h++) {
    const i = queue[h], j = old[i];
    for (let q = 0; q < 3; q++) {
      const f = NET.far[3 * i + q], g = GRAPH.far[3 * j + q], a = f >> 2, b = g >> 2;
      if (f < 0 || g < 0 || (f & 3) !== (g & 3) || old[a] >= 0 || taken[b] || NET.tag[a] !== GRAPH.tag[b]) continue;
      old[a] = b; taken[b] = 1; queue.push(a);
    }
  }
  return queue.length;
}

/// Lay out the agents not yet placed as trees: each hung from a top (the root first, then anything
/// whose output no unplaced agent reads), what feeds an input below it, side by side, to the right
/// of what is placed if anything is.
function graphHang(at, x, y, beside) {
  const n = NET.n, starts = [];
  for (let i = 0; i < n; i++) {
    if (at[i]) continue;
    let read = false;
    for (let q = 0; q < 3; q++) { const f = NET.far[3 * i + q]; if (f >= 0 && isSource(NET.tag[i], q) && !at[f >> 2]) read = true; }
    if (!read) starts.push(i);
  }
  starts.sort((a, b) => (NET.tag[b] === 13) - (NET.tag[a] === 13));
  for (let i = 0; i < n; i++) if (!at[i]) starts.push(i);
  // A spanning tree over inputs, first visit wins, in preorder, each node's children in term order.
  const order = [], tops = [], depth = new Int32Array(n), first = new Int32Array(n).fill(-1), last = new Int32Array(n).fill(-1), next = new Int32Array(n).fill(-1), seen = new Uint8Array(n);
  for (const s of starts) {
    if (seen[s]) continue;
    tops.push(s);
    const stack = [s, -1];
    while (stack.length) {
      const p = stack.pop(), i = stack.pop();
      if (seen[i]) continue;
      seen[i] = 1; order.push(i); depth[i] = p < 0 ? 0 : depth[p] + 1;
      if (p >= 0) { if (last[p] < 0) first[p] = i; else next[last[p]] = i; last[p] = i; }
      const ins = netInputs(i);
      for (let k = ins.length - 1; k >= 0; k--) if (!seen[ins[k]] && !at[ins[k]]) stack.push(ins[k], i);
    }
  }
  if (!order.length) return;
  // A tidy tree (after Reingold and Tilford): from the leaves up, each node's subtrees side by side,
  // as close as their outlines allow, and it over the middle of them. An outline is the leftmost
  // and rightmost x at each depth, stored deepest first so a parent adds its own row with a push.
  const ROW = GSP * 0.85, outline = new Array(n), rel = new Float64Array(n);
  const kidsOf = i => { const out = []; for (let c = first[i]; c >= 0; c = next[c]) out.push(c); return out; };
  const join = (kids, d) => {
    let acc = null, end = 0;
    for (const c of kids) {
      const o = outline[c]; outline[c] = null;
      if (!acc) { acc = o; rel[c] = 0; continue; }
      let need = -Infinity;
      for (let e = d, lo = Math.min(acc.bottom, o.bottom); e <= lo; e++) need = Math.max(need, acc.r[acc.bottom - e] + acc.off - o.l[o.bottom - e] - o.off);
      o.off += need + GSP; rel[c] = end = need + GSP;
      if (o.bottom > acc.bottom) { for (let e = d; e <= acc.bottom; e++) o.l[o.bottom - e] = acc.l[acc.bottom - e] + acc.off - o.off; acc = o; }
      else for (let e = d; e <= o.bottom; e++) acc.r[acc.bottom - e] = o.r[o.bottom - e] + o.off - acc.off;
    }
    acc.off -= end / 2;
    for (const c of kids) rel[c] -= end / 2;
    return acc;
  };
  for (let k = order.length - 1; k >= 0; k--) {
    const i = order[k];
    if (first[i] < 0) { outline[i] = { l: [0], r: [0], off: 0, bottom: depth[i] }; continue; }
    const o = join(kidsOf(i), depth[i] + 1);
    o.l.push(-o.off); o.r.push(-o.off); outline[i] = o;
  }
  join(tops, 0);
  let x0 = Infinity, x1 = -Infinity, deepest = 0;
  for (const i of order) {
    for (let c = first[i]; c >= 0; c = next[c]) rel[c] += rel[i];
    x0 = Math.min(x0, rel[i]); x1 = Math.max(x1, rel[i]); deepest = Math.max(deepest, depth[i]);
  }
  let dx = -(x0 + x1) / 2, dy = -deepest * ROW / 2;
  if (beside) {
    let right = -Infinity, top = Infinity;
    for (let i = 0; i < n; i++) if (at[i]) { right = Math.max(right, x[i]); top = Math.min(top, y[i]); }
    dx = right + GSP * 2 - x0; dy = top;
  }
  for (const i of order) { x[i] = rel[i] + dx; y[i] = depth[i] * ROW + dy; at[i] = 1; }
}

/// Every wire once, between agents a and b: which carries whose output (src, -1 if neither end is an
/// output), and flags: 1 an active pair (a reader's principal port wired to a value's, so a rule
/// fires once they meet), 2 a's principal port, 4 b's.
function graphWires() {
  const n = NET.n, wa = [], wb = [], src = [], flag = [], deg = new Uint8Array(n);
  for (let i = 0; i < n; i++) for (let q = 0; q < 3; q++) {
    const f = NET.far[3 * i + q], j = f >> 2, p = f & 3;
    if (f < 0 || j === i) continue;
    deg[i]++;
    if (j < i) continue;
    const ti = NET.tag[i], tj = NET.tag[j], active = q === 0 && p === 0 && (isConsumer(ti) && isProducer(tj) || isConsumer(tj) && isProducer(ti));
    wa.push(i); wb.push(j);
    src.push(isSource(ti, q) ? i : isSource(tj, p) ? j : -1);
    flag.push((active ? 1 : 0) | (q === 0 ? 2 : 0) | (p === 0 ? 4 : 0));
  }
  Object.assign(GRAPH, { wa: Int32Array.from(wa), wb: Int32Array.from(wb), src: Int32Array.from(src), flag: Uint8Array.from(flag), wires: wa.length, deg });
}

/// Steps of the forces, until the layout settles: at most `most`, and no more than fit in `ms`
/// milliseconds at the pace of the last step (always one).
function graphRelax(ms, most) {
  const t0 = performance.now();
  for (let k = 0, t = t0; k < most && GRAPH.alpha > 0.004 && (k === 0 || t - t0 + GRAPH.stepMs < ms); k++) {
    graphStep(GRAPH.alpha); GRAPH.alpha *= 0.977;
    const now = performance.now(); GRAPH.stepMs = now - t; t = now;
  }
}
function graphStep(a) {
  const { n, x, y, vx, vy, deg, wa, wb, src, wires } = GRAPH;
  if (!n) return;
  // springs along wires, shared out by degree as in d3-force, and the hang: an output at least
  // three quarters of a wire below the input reading it
  for (let w = 0; w < wires; w++) {
    const i = wa[w], j = wb[w];
    let dx = x[j] + vx[j] - x[i] - vx[i], dy = y[j] + vy[j] - y[i] - vy[i];
    const d = Math.sqrt(dx * dx + dy * dy) || 0.01, k = (d - GSP) / d * a / Math.min(deg[i], deg[j]), bias = deg[i] / (deg[i] + deg[j]);
    dx *= k; dy *= k;
    vx[j] -= dx * bias; vy[j] -= dy * bias; vx[i] += dx * (1 - bias); vy[i] += dy * (1 - bias);
    const s = src[w];
    if (s >= 0) {
      const r = s === i ? j : i, short = GSP * 0.75 - (y[s] - y[r]);
      if (short > 0) { const f = short * a * 0.12; vy[s] += f; vy[r] -= f; }
    }
  }
  // repulsion between nodes nearer than GREP, found through a hashed grid of GREP-sized cells
  let g = GRAPH.grid;
  if (!g || g.items.length < n) {
    const size = 1 << Math.ceil(Math.log2(2 * n + 2));
    g = GRAPH.grid = { mask: size - 1, head: new Int32Array(size + 1), cur: new Int32Array(size), items: new Int32Array(n), gx: new Int32Array(n), gy: new Int32Array(n) };
  }
  const { mask, head, cur, items, gx, gy } = g, R = GREP, R2 = R * R, kr = a * 0.3, near = new Int32Array(9);
  const cell = (cx, cy) => (Math.imul(cx, 73856093) ^ Math.imul(cy, 19349663)) & mask;
  head.fill(0);
  for (let i = 0; i < n; i++) { gx[i] = Math.floor(x[i] / R); gy[i] = Math.floor(y[i] / R); head[cell(gx[i], gy[i]) + 1]++; }
  for (let h = 0; h <= mask; h++) head[h + 1] += head[h];
  cur.set(head.subarray(0, mask + 1));
  for (let i = 0; i < n; i++) items[cur[cell(gx[i], gy[i])]++] = i;
  for (let i = 0; i < n; i++) {
    let m = 0;
    for (let ox = -1; ox <= 1; ox++) for (let oy = -1; oy <= 1; oy++) {
      const c = cell(gx[i] + ox, gy[i] + oy);
      let dup = false; for (let u = 0; u < m; u++) if (near[u] === c) dup = true;
      if (dup) continue;
      near[m++] = c;
      for (let t = head[c], e = head[c + 1]; t < e; t++) {
        const j = items[t]; if (j <= i) continue;
        let dx = x[j] - x[i], dy = y[j] - y[i], d2 = dx * dx + dy * dy;
        if (d2 >= R2) continue;
        if (d2 < 1e-6) { dx = graphJitter(i, 0); dy = graphJitter(j, 1); d2 = dx * dx + dy * dy; }
        const d = Math.sqrt(d2), f = (R - d) / d * kr;
        vx[j] += dx * f; vy[j] += dy * f; vx[i] -= dx * f; vy[i] -= dy * f;
      }
    }
  }
  // the weak pull to the middle, on all of it together so nothing is bent by it. No node goes
  // faster than a third of a wire a step, so one rewired far away glides there.
  let mx = 0, my = 0;
  for (let i = 0; i < n; i++) { mx += x[i]; my += y[i]; }
  mx = mx / n * a * 0.02; my = my / n * a * 0.02;
  const cap = GSP / 3;
  for (let i = 0; i < n; i++) {
    let u = (vx[i] - mx) * 0.6, v = (vy[i] - my) * 0.6;
    const sp = Math.sqrt(u * u + v * v);
    if (sp > cap) { u *= cap / sp; v *= cap / sp; }
    vx[i] = u; vy[i] = v; x[i] += u; y[i] += v;
  }
}

/// Where the nodes are, as [x0, y0, x1, y1].
function graphBox() {
  const { n, x, y } = GRAPH;
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (let i = 0; i < n; i++) { x0 = Math.min(x0, x[i]); y0 = Math.min(y0, y[i]); x1 = Math.max(x1, x[i]); y1 = Math.max(y1, y[i]); }
  return n ? [x0 - GRAD, y0 - GRAD, x1 + GRAD, y1 + GRAD] : [-GSP, -GSP, GSP, GSP];
}
/// Fit the nodes to the page, and keep fitting them for a moment while the layout settles, unless
/// the view is moved meanwhile.
function fitGraph(follow = true) {
  graphSync();
  let [x0, y0, x1, y1] = graphBox();
  const pad = Math.max(GSP, (x1 - x0) * 0.05); x0 -= pad; y0 -= pad; x1 += pad; y1 += pad;
  const r = cv.getBoundingClientRect(), top = 50, bottom = 130;
  view.s = Math.min(2.5, (r.width - 30) / (x1 - x0), (r.height - top - bottom) / (y1 - y0));
  view.ox = (r.width - (x1 - x0) * view.s) / 2 - x0 * view.s; view.oy = top + (r.height - top - bottom - (y1 - y0) * view.s) / 2 - y0 * view.s;
  GRAPH.fitView = { ...view };
  if (follow) GRAPH.fitUntil = performance.now() + 2500;
}

/// The node nearest the pointer, within a little more than its radius, as a seat.
function graphAgentAt(cx, cy) {
  if (!L) return null;
  const r = cv.getBoundingClientRect(), wx = (cx - r.left - view.ox) / view.s, wy = (cy - r.top - view.oy) / view.s;
  for (let tries = 0; tries < 2; tries++) {
    graphSync();
    const { n, x, y } = GRAPH;
    let best = -1, bd = Math.max(GRAD * 1.4, 6 / view.s) ** 2;
    for (let i = 0; i < n; i++) { const d = (x[i] - wx) ** 2 + (y[i] - wy) ** 2; if (d < bd) { bd = d; best = i; } }
    if (best < 0) return null;
    // While playing NET can be a moment old: read it again if the agent is no longer in its seat.
    if (views().tags[NET.seat[best]] === NET.tag[best]) return NET.seat[best];
    netSync(true);
  }
  return null;
}

/// A colour (r, g, b from 0 to 1) at opacity a over the page's background, as an opaque css colour.
const graphMix = (c, a) => `rgb(${Math.round(255 * c[0] * a + 11 * (1 - a))},${Math.round(255 * c[1] * a + 14 * (1 - a))},${Math.round(255 * c[2] * a + 20 * (1 - a))})`;
const graphRgb = hex => [1, 3, 5].map(k => parseInt(hex.slice(k, k + 2), 16) / 255);

function drawGraph() {
  const d = devicePixelRatio || 1;
  ctx.setTransform(1, 0, 0, 1, 0, 0); ctx.fillStyle = "#0b0e14"; ctx.fillRect(0, 0, cv.width, cv.height);
  if (!L) return;
  pickSync();
  graphSync();
  graphRelax(6, 4);
  if (performance.now() < GRAPH.fitUntil) {
    const f = GRAPH.fitView;
    if (f && f.s === view.s && f.ox === view.ox && f.oy === view.oy) fitGraph(false); else GRAPH.fitUntil = 0;
  }
  const V = views(), { n, x, y, wa, wb, src, flag, wires } = GRAPH, segs = showSegments ? NET.segs : null, pk = PICK.on ? PICK.marks : null;
  ctx.setTransform(d * view.s, 0, 0, d * view.s, d * view.ox, d * view.oy);
  const r = cv.getBoundingClientRect(), rad = Math.max(GRAD, 1.2 / view.s), m = rad * 2;
  const vx0 = -view.ox / view.s - m, vy0 = -view.oy / view.s - m, vx1 = (r.width - view.ox) / view.s + m, vy1 = (r.height - view.oy) / view.s + m;
  const shown = i => x[i] >= vx0 && x[i] <= vx1 && y[i] >= vy0 && y[i] <= vy1;
  const seatOf = i => NET.seat[i], gone = i => dead(siteOf(NET.seat[i]), slotOf(NET.seat[i]));
  const wanted = i => isConsumer(NET.tag[i]) && V.tags[seatOf(i)] === NET.tag[i] && V.want[seatOf(i)] && !gone(i);
  // Everything is drawn opaque, its colour mixed with the background beforehand: a path of
  // thousands of lines drawn see-through is many times slower. How faint, by how much it matters:
  // 0 as it is, 1 garbage, 2 garbage in a pick, 3 outside the picks.
  const KIND = GRAPH.kind ??= COLOR.map(graphRgb), GREY = graphRgb("#4a5465"), WIRE = graphRgb("#7896be");
  const PINKS = [graphRgb(PINK), graphRgb("#ff9be6")], BLUE = graphRgb("#56c8ff"), ORANGE = graphRgb("#ffa94d");
  const WIRE_A = [1, 0.3, 0.6, 0.12], NODE_A = [1, 0.28, 0.6, 0.14];
  const tones = new Map();
  /// Colour number `key` at opacity a, for use `slot`, mixed once a frame.
  const tone = (key, c, slot, a) => { let t = tones.get(key); if (!t) tones.set(key, t = []); return t[slot] ??= graphMix(c, a); };
  const keyOf = i => segs ? NET.seg[i] : -2 - NET.tag[i], colourOf = i => segs ? (NET.seg[i] >= 0 ? segs[NET.seg[i]].rgb : GREY) : KIND[NET.tag[i]];
  // Paths batched by colour, width (0 a fill) and dash, drawn lowest z first so what is lit lands on top.
  const batches = new Map();
  const into = (style, z, width, dash = false) => {
    const k = (width === 1.2 || width === 0) && !dash ? style : `${style}|${width}|${dash}`;
    let b = batches.get(k);
    if (!b) batches.set(k, b = { style, z, width, dash, path: new Path2D() });
    return b.path;
  };
  const outline = view.s * rad > 3;
  const flush = () => {
    for (const b of [...batches.values()].sort((p, q) => p.z - q.z)) {
      if (b.width) { ctx.strokeStyle = b.style; ctx.lineWidth = lw(b.width); ctx.setLineDash(b.dash ? [lw(3), lw(3)] : []); ctx.stroke(b.path); continue; }
      ctx.fillStyle = b.style; ctx.fill(b.path);
      if (outline) { ctx.strokeStyle = "#0b0e14"; ctx.lineWidth = lw(1); ctx.stroke(b.path); }
    }
    ctx.setLineDash([]); batches.clear();
  };
  const line = (p, i, j) => { p.moveTo(x[i], y[i]); p.lineTo(x[j], y[j]); };
  ctx.lineCap = "butt";
  // the pinned site's agents, and the agents at the far ends of their wires
  const atPinned = new Set(), farEnds = new Set();
  if (pinned != null) for (let k = 0; k < L.ks; k++) { const i = NET.bySeat[pinned * L.ks + k]; if (i >= 0) atPinned.add(i); }
  // wires: in the colour of the segment whose output each carries, or muted; with picks, their
  // wires pink (or in segment colour) and the rest faint; a wanted reader's principal wire blue.
  // Zoomed far out, a wire under a pixel long is hidden by its nodes: not drawn.
  const tiny = 1 / view.s ** 2;
  for (let w = 0; w < wires; w++) {
    const i = wa[w], j = wb[w];
    if (Math.max(x[i], x[j]) < vx0 || Math.min(x[i], x[j]) > vx1 || Math.max(y[i], y[j]) < vy0 || Math.min(y[i], y[j]) > vy1) continue;
    const s = src[w], from = s >= 0 ? s : i, reader = s === i ? j : i, b = pk ? pk[seatOf(reader)] : 0;
    const lv = pk && !b ? 3 : gone(from) ? (b ? 2 : 1) : 0, a = WIRE_A[lv];
    if ((x[i] - x[j]) ** 2 + (y[i] - y[j]) ** 2 >= tiny) {
      if (b & 16) line(into("#ffffff", 3, 2.2), i, j);
      else if (b && !segs) line(into(tone(-30 - (b & 1), PINKS[b & 1 ? 0 : 1], lv, a), 1 + a, 1.8), i, j);
      else line(into(segs ? tone(keyOf(from), colourOf(from), lv, a * 0.85) : tone(-20, WIRE, lv, a * 0.6), a, 1.2), i, j);
    }
    // where a pick's root sends its value, dashed
    if (pk && s >= 0 && pk[seatOf(s)] & 10) line(into(PINK, 4, 1.6, true), i, j);
    if (((flag[w] & 2) && wanted(i)) || ((flag[w] & 4) && wanted(j))) line(into(tone(-40, BLUE, lv, lv === 3 ? 0.3 : 0.9), 2 + a, 2), i, j);
    if (atPinned.has(i) || atPinned.has(j)) { line(into("#ffaa46", 5, 2.4), i, j); farEnds.add(atPinned.has(i) ? j : i); }
  }
  // active pairs, about to rewrite: the wire between their principal ports thick and gold, a diamond in its middle
  for (let w = 0; w < wires; w++) {
    if (!(flag[w] & 1)) continue;
    const i = wa[w], j = wb[w];
    if (!shown(i) && !shown(j)) continue;
    const a = pk && !(pk[seatOf(i)] || pk[seatOf(j)]) ? 0.2 : gone(i) && gone(j) ? 0.4 : 1, gold = graphMix([1, 0.85, 0.4], a);
    line(into(gold, 6 + a, 3), i, j);
    const mx = (x[i] + x[j]) / 2, my = (y[i] + y[j]) / 2, h = rad * 0.75, p = into(gold, 7 + a, 0);
    p.moveTo(mx, my - h); p.lineTo(mx + h, my); p.lineTo(mx, my + h); p.lineTo(mx - h, my); p.closePath();
  }
  flush();
  // nodes, in segment colour (or by kind), garbage faint, with picks the rest fainter; a pixel or
  // two across, squares, which look the same and draw much faster
  const level = new Uint8Array(n), visible = [], round = view.s * rad > 2.5, squares = new Map();
  for (let i = 0; i < n; i++) {
    if (!shown(i)) continue;
    visible.push(i);
    const b = pk ? pk[seatOf(i)] : 0, lv = pk && !b ? 3 : gone(i) ? (b ? 2 : 1) : 0, style = tone(keyOf(i), colourOf(i), 4 + lv, NODE_A[lv]);
    level[i] = lv;
    if (round) { const p = into(style, NODE_A[lv], 0); p.moveTo(x[i] + rad, y[i]); p.arc(x[i], y[i], rad, 0, 7); }
    else { let q = squares.get(style); if (!q) squares.set(style, q = { z: NODE_A[lv], at: [] }); q.at.push(i); }
  }
  flush();
  for (const [style, q] of [...squares].sort((p, q) => p[1].z - q[1].z)) {
    ctx.fillStyle = style;
    for (const i of q.at) ctx.fillRect(x[i] - rad, y[i] - rad, 2 * rad, 2 * rad);
  }
  // rings: wanted readers blue, called values orange, picks pink (roots twice), the pinned site's
  // agents in a blue square as in 2D, the far ends of its wires orange, the hovered site faintly
  const ring = (p, i, k) => { p.moveTo(x[i] + rad * k, y[i]); p.arc(x[i], y[i], rad * k, 0, 7); };
  const square = (p, i, k) => p.rect(x[i] - rad * k, y[i] - rad * k, 2 * rad * k, 2 * rad * k);
  for (const i of visible) {
    const seat = seatOf(i), t = NET.tag[i], b = pk ? pk[seat] : 0, lv = level[i], a = NODE_A[lv];
    if (V.tags[seat] === t && V.want[seat]) ring(into(isConsumer(t) ? tone(-41, BLUE, lv, a) : tone(-42, ORANGE, lv, a), a, 1.4), i, 1.45);
    if (b && !(segs && !(b & 26))) ring(into(b & 16 ? "#ffffff" : b & 3 ? PINK : "#ff9be6", 2, b & 16 ? 1.6 : 1.1), i, 1.2);
    if (b & 10) ring(into(PINK, 2, 2), i, 1.9);
    if (atPinned.has(i)) square(into("#56c8ff", 3, 1.5), i, 1.9);
    else if (hover != null && siteOf(seat) === hover) square(into("#6a7280", 3, 1), i, 1.9);
    if (farEnds.has(i)) ring(into("#ffaa46", 3, 1.4), i, 1.7);
  }
  flush();
  // close up, each node's principal port on its rim, toward the wire it is on, and its letter
  if (view.s * rad > 3.5) {
    const labels = view.s * rad > 6;
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    for (const i of visible) {
      ctx.globalAlpha = NODE_A[level[i]];
      const f = NET.far[3 * i], j = f >> 2, ang = f >= 0 && j !== i ? Math.atan2(y[j] - y[i], x[j] - x[i]) : -Math.PI / 2;
      ctx.fillStyle = "#0b0e14"; ctx.beginPath(); ctx.arc(x[i] + rad * Math.cos(ang), y[i] + rad * Math.sin(ang), rad * 0.32, 0, 7); ctx.fill();
      ctx.strokeStyle = "#ffffff"; ctx.lineWidth = lw(0.7); ctx.stroke();
      if (labels) {
        const lab = LABEL[TAGS[NET.tag[i]]];
        ctx.fillStyle = "#0b0e14"; ctx.font = `bold ${rad * (lab.length > 1 ? 0.8 : 1.05)}px ui-monospace,monospace`;
        ctx.fillText(lab, x[i], y[i] + rad * 0.12);
      }
    }
    ctx.globalAlpha = 1;
  }
  // rewrites are not flashed here; let their rings run out
  const now = performance.now(); let keep = 0;
  for (const b of bursts) if (now - b.t < b.life) bursts[keep++] = b;
  bursts.length = keep;
  pickPanel(false);
}
