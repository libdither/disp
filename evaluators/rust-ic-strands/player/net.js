// The net itself, read off the engine (readback.rs `wires`, for index.html): every agent and what
// each of its ports is wired to, whatever strands the wire takes on the lattice. Every view colours
// agents by segment from it, and the graph view (graph.js) lays it out.
//
// Segments: the term the root computes (or each pick's, if anything is picked) is a tree of
// applications. At every application (an apply or a suspension, f x; a triage or a dispatch, t a b c;
// the arms of a choice) the function and each argument become segments of their own, and so on down
// through nested applications; values, sharing and the rest stay in the segment they are in. A
// segment's hue is the middle of its share of the hue circle, shared out among the parts of each
// application by how many applications each holds, so nested segments take hues near their parent's;
// its lightness alternates with how deep it is nested, so a part stands out from the whole around it.
const NET = { key: "", segKey: "", at: 0, rev: 0, n: 0, id: null, seat: null, tag: null, far: null, index: new Map(), bySeat: null,
  seg: null, segs: [], renumbered: 0 };
/// Applications and the arms of a choice: P, Pair, A, T1, Sel.
const SPLITS = [false, false, false, false, true, true, true, true, true, false, false, false, false, false];

/// Bring NET up to date with the lattice and the picks: while playing, a big net at most every 120 ms
/// (a small one every frame), unless forced. True if anything changed.
function netSync(force) {
  if (!L) return false;
  const st = stats(), key = `${st[0]}|${st[1]}|${E.strands_renumbered()}|${L.w}x${L.h}x${L.depth}`;
  const roots = PICK.roots.map(r => r.seat).join(",");
  if (key === NET.key && roots === NET.segKey) return false;
  if (!force && playing && performance.now() - NET.at < Math.min(120, NET.n / 100)) return false;
  NET.at = performance.now();
  if (key !== NET.key) { NET.key = key; netRead(); }
  NET.segKey = roots;
  segment();
  NET.rev++;
  return true;
}

function netRead() {
  const n = E.strands_net(), w = new Uint32Array(E.memory.buffer, E.seats_ptr(), 6 * n).slice(), seats = L.w * L.h * L.depth * L.ks;
  if (!NET.bySeat || NET.bySeat.length !== seats) NET.bySeat = new Int32Array(seats).fill(-1);
  else if (NET.seat) for (const s of NET.seat) NET.bySeat[s] = -1;
  NET.renumbered = E.strands_renumbered();
  NET.n = n; NET.id = new Uint32Array(n); NET.seat = new Uint32Array(n); NET.tag = new Uint8Array(n); NET.far = new Int32Array(3 * n);
  NET.index = new Map();
  for (let i = 0; i < n; i++) {
    NET.id[i] = w[6 * i]; NET.seat[i] = w[6 * i + 1]; NET.tag[i] = w[6 * i + 2];
    NET.index.set(NET.id[i], i); NET.bySeat[NET.seat[i]] = i;
  }
  for (let i = 0; i < n; i++) for (let q = 0; q < 3; q++) {
    const far = w[6 * i + 3 + q], j = far === NOWHERE ? undefined : NET.index.get(far >>> 2);
    NET.far[3 * i + q] = j === undefined ? -1 : j * 4 + (far & 3);
  }
}

/// What feeds agent i's inputs, in the order a term reads them (f before x).
function netInputs(i) {
  const t = NET.tag[i], out = [];
  for (let q = 0; q < ARITY[t]; q++) { const f = NET.far[3 * i + q]; if (f >= 0 && !isSource(t, q)) out.push(f >> 2); }
  return out;
}

function segment() {
  const n = NET.n, seg = new Int32Array(n).fill(-1), parent = new Int32Array(n).fill(-1), order = [];
  const roots = PICK.roots.length ? PICK.roots.map(r => NET.bySeat[r.seat]).filter(i => i >= 0)
    : [...NET.tag.keys()].filter(i => NET.tag[i] === 13);
  // A spanning tree over inputs from the roots, first visit wins, in preorder.
  const seen = new Uint8Array(n), stack = roots.slice().reverse().map(i => [i, -1]);
  while (stack.length) {
    const [i, p] = stack.pop();
    if (seen[i]) continue;
    seen[i] = 1; parent[i] = p; order.push(i);
    const kids = netInputs(i);
    for (let k = kids.length - 1; k >= 0; k--) if (!seen[kids[k]]) stack.push([kids[k], i]);
  }
  // How many applications each part holds, to share out the hue by.
  const weight = new Float64Array(n);
  for (let k = order.length - 1; k >= 0; k--) { const i = order[k]; weight[i] += SPLITS[NET.tag[i]] ? 1 : 0; if (parent[i] >= 0) weight[parent[i]] += weight[i]; }
  const kidsOf = new Map();
  for (const i of order) if (parent[i] >= 0) { const p = parent[i]; if (!kidsOf.has(p)) kidsOf.set(p, []); kidsOf.get(p).push(i); }
  const segs = [], lo = new Float64Array(n), hi = new Float64Array(n);
  const share = (items, a, b, depth, from) => {
    const total = items.reduce((s, i) => s + Math.max(1, weight[i]), 0);
    let x = a;
    for (const i of items) {
      const y = x + (b - a) * Math.max(1, weight[i]) / total;
      lo[i] = x; hi[i] = y; seg[i] = segs.length; segs.push({ lo: x, hi: y, depth, parent: from }); x = y;
    }
  };
  share(roots.filter(i => parent[i] === -1), 0, 1, 0, -1);
  for (const i of order) {
    const kids = kidsOf.get(i) ?? [];
    if (SPLITS[NET.tag[i]]) share(kids, lo[i], hi[i], segs[seg[i]].depth + 1, seg[i]);
    else for (const c of kids) { lo[c] = lo[i]; hi[c] = hi[i]; seg[c] = seg[i]; }
  }
  for (const s of segs) {
    const h = (200 + 300 * (s.lo + s.hi) / 2) % 360, l = s.depth % 2 ? 0.5 : 0.66, sat = 0.75;
    s.css = `hsl(${h.toFixed(1)},${sat * 100}%,${l * 100}%)`;
    s.tint = `hsla(${h.toFixed(1)},${sat * 100}%,${l * 100}%,0.16)`;
    s.rgb = hslRgb(h / 360, sat, l);
  }
  NET.seg = seg; NET.segs = segs;
}
function hslRgb(h, s, l) {
  const f = n => { const k = (n + h * 12) % 12, a = s * Math.min(l, 1 - l); return l - a * Math.max(-1, Math.min(k - 3, 9 - k, 1)); };
  return [f(0), f(8), f(4)];
}
/// The segment of the agent in a seat, or null if it is in none (garbage, or outside every pick).
function segAt(seat) {
  const i = NET.bySeat ? NET.bySeat[seat] : -1;
  return i >= 0 && NET.seg[i] >= 0 ? NET.segs[NET.seg[i]] : null;
}
