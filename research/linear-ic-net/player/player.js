// The player's logic (index.html): reads a run off engine.js, lays the net out, draws it, and plays it.
// The agent view of evaluators/rust-ic-strands/player (rev.html, graph.js), rebuilt for the reflective nets.
"use strict";
const $ = id => document.getElementById(id);
const { K, KINDS, POL, ARITY, FAMILIES, CODE, ruleFor, Run, show, marksOf, PRELUDE, ADDS } = RLN;
const { shapeInto, tipAt, bestFit, SvgPath, svgAt, iconSVG } = ICN;
const cv = $("cv"), ctx = cv.getContext("2d"), sv = $("strip"), sctx = sv.getContext("2d");

// ---- what each kind of agent is, and how it is drawn -----------------------------------------------
const NAMES = { lam: "function", Lam: "opened function", app: "application (code)", var: "use of a variable", rho: "ρ, the reader at rest",
  O: "running application", T: "thaw", R: "reader", sig: "split", dupI: "copier, going in", dupO: "copier, going out",
  eraI: "eraser, going in", eraO: "eraser, going out", root: "root" };
const SHORT = { lam: "a function", Lam: "an opened function", app: "an application", var: "a use of a variable", rho: "ρ",
  O: "a running application", T: "a thaw", R: "a reader", sig: "a split", dupI: "a copier", dupO: "a copier", eraI: "an eraser", eraO: "an eraser" };
const PORTS = { lam: ["value", "x (to the uses)", "body"], Lam: ["value", "x (to the uses)", "body"], app: ["value", "function", "argument"], var: ["value", "holds"],
  rho: ["value"], O: ["function", "argument", "result"], T: ["code", "result"], R: ["value read", "result"],
  sig: ["from the binder", "use", "use"], dupI: ["copied", "copy", "copy"], dupO: ["to the uses", "copy", "copy"], eraI: ["erased"], eraO: ["to the uses"], root: ["answer"] };
const MEANING = {
  run: "a call. The argument goes to the variable's uses, and a thaw starts on the body",
  open: "the argument goes to the variable's uses, and the body is handed back as code, not run",
  reflect: "ρ takes its argument and becomes a reader",
  thawApp: "the application starts running, with a thaw on each part",
  thawVar: "whatever the variable holds is handed on as it is",
  thawValue: "it is a value already, so it is handed on. A function's body stays frozen",
  viewFun: "the first arm gets the function, opened",
  viewOpened: "the second arm gets the function, closed again",
  viewApp: "the third arm gets its two parts, as code",
  viewVar: "the fourth arm gets the captured value",
  viewPrim: "the fifth arm runs",
  copy: "two of that agent now, and a copier on each of its other ports",
  cancel: "the two ends of one copy have met, and are done",
  erase: "that agent is gone, and an eraser goes to each of its other ports",
};
const FAMILY = [["call", "#e86bd8", "a running application meets a function"], ["open", "#9d7cff", "a running application meets an opened function"],
  ["read", "#56c8ff", "ρ, and a reader meeting the value it reads"], ["thaw", "#6ee787", "a thaw meets code"],
  ["copy", "#ffa94d", "a split or a copier meets an agent"], ["cancel", "#ffd866", "the two ends of a copy meet"], ["erase", "#ff6b6b", "an eraser meets an agent"]];
const copyColor = op => `hsl(${(op * 67 + 28) % 360} 90% 62%)`;

/// Every kind as it is drawn (icons.js: the choice made in icons.html), by name and by number.
const SHAPE = ICN.current(), SHAPES = KINDS.map(k => SHAPE[k]);
const does = kind => (ICN.ICONS[kind] ?? {}).does ?? "";
const colourOf = (t, op) => op >= 0 ? copyColor(op) : SHAPES[t].color;
/// Which side of its icon each port is on, as the legend draws it. In the net an icon turns and flips as
/// a whole to face what it is wired to, so the sides hold relative to each other.
const SIDE = { root: ["↓"], lam: ["↑", "←", "↓"], Lam: ["↑", "←", "↓"], app: ["↑", "↙", "↘"], var: ["↑", "↙"], rho: ["↑"], O: ["↙", "↘", "↑"],
  T: ["↓", "↑"], R: ["↓", "↑"], sig: ["↑", "↙", "↘"], dupI: ["↓", "↖", "↗"], dupO: ["↓", "↖", "↗"], eraI: ["↓"], eraO: ["↓"] };
const PORT_SHORT = { lam: ["", "x", "body"], Lam: ["", "x", "body"], app: ["", "function", "argument"], var: ["", "holds"], O: ["", "argument", "result"], T: ["", "result"], R: ["", "result"],
  sig: ["", "use", "use"], dupI: ["", "copy", "copy"], dupO: ["", "copy", "copy"] };
/// A hue for a name, the same every time: a named part and its copies share it.
const hueOf = name => { let h = 7; for (const ch of name) h = (h * 31 + ch.charCodeAt(0)) % 360; return h; };

// ---- the net on show -------------------------------------------------------------------------------
let RUN = null;
/// Parts opened by hand: they stay open until their tag is clicked.
const OPEN = new Set();
/// The smallest part worth folding, in agents.
const MINFOLD = 3;
/// What is drawn: the agents not folded away, then a pod for each part folded shut. Densely numbered:
/// ids (a pod's is -1 - part), kinds, and for each port the node and port it is wired to (4 * node +
/// port, or -1). `index` finds a node by id, `op` a copier's copy, `pods` what each pod stands for, and
/// `halos` the whole parts that are drawn open. `tops` names the agent at the top of each part that is
/// whole, for reading terms. `cl` is the structure each node is in (or -1) and `clusters` the structures:
/// { nodes, top, label, hue }, a hue of -1 where the label is a term and not a name. `marks` has the
/// text of each number, bare tree of bits and bit, by the node at its top.
let NET = { n: 0, id: new Int32Array(0), tag: new Uint8Array(0), far: new Int32Array(0), index: new Map(), op: new Int32Array(0), pods: new Map(), halos: [],
  tops: new Map(), cl: new Int32Array(0), clusters: [], shape: null, marks: new Map() };
function readNet() {
  const N = RUN.net, live = N.live, folding = $("fold").checked;
  const members = new Map();
  for (let i = 0; i < live; i++) { const a = N.ids[i], g = N.grp.get(a); if (g === undefined) continue; let l = members.get(g); if (!l) members.set(g, l = []); l.push(a); }
  // A part is whole while every agent it was written with is still there. Whole parts fold, the smallest
  // first, so a part folded inside a larger one is taken into it. A part folds when one wire leaves it,
  // from its top, to a side port: it is a value nothing is acting on. Otherwise it is drawn open, named.
  const hidden = new Map(), pods = new Map(), open = [], tops = new Map();
  const whole = [...members].filter(([g, ids]) => ids.length === N.wholeSize(g)).sort((p, q) => p[1].length - q[1].length || p[0] - q[0]);
  for (const [g, ids] of whole) {
    const inside = new Set(ids), outs = [], within = [];
    for (const a of ids) for (let q = 0; q <= ARITY[N.kind[a]]; q++) {
      const f = N.far[3 * a + q]; if (f < 0) continue;
      const b = f >> 2; if (inside.has(b)) continue;
      const h = hidden.get(b);
      if (h !== undefined && pods.has(h) && pods.get(h).out === a * 4 + q) { within.push(h); continue; }
      outs.push(a, q, f);
    }
    const out = outs.length === 3 && outs[1] === 0 ? outs[2] : -1, quiet = out >= 0 && ((out & 3) !== 0 || N.kind[out >> 2] === K.root);
    if (out >= 0 && !N.parts[g].name.startsWith("view of")) tops.set(outs[0], N.parts[g].name);
    let size = ids.length; for (const h of within) size += pods.get(h).cover.length;
    if (folding && quiet && size >= MINFOLD && !OPEN.has(g)) {
      const cover = ids.slice();
      for (const h of within) { for (const a of pods.get(h).cover) cover.push(a); pods.delete(h); }
      for (const a of cover) hidden.set(a, g);
      pods.set(g, { top: outs[0], out, cover, name: N.parts[g].name });
    } else open.push({ g, name: N.parts[g].name, ids, within, byHand: OPEN.has(g) && quiet });
  }
  const vis = []; for (let i = 0; i < live; i++) if (!hidden.has(N.ids[i])) vis.push(N.ids[i]);
  const podList = [...pods.keys()], n = vis.length + podList.length;
  const id = new Int32Array(n), tag = new Uint8Array(n), far = new Int32Array(3 * n).fill(-1), op = new Int32Array(n).fill(-1), index = new Map();
  vis.forEach((a, i) => { id[i] = a; tag[i] = N.kind[a]; index.set(a, i); if (N.op.has(a)) op[i] = N.op.get(a); });
  podList.forEach((g, k) => { const i = vis.length + k; id[i] = -1 - g; tag[i] = K.pod; index.set(-1 - g, i); });
  const node = pc => { const h = hidden.get(pc >> 2); return h !== undefined ? index.get(-1 - h) * 4 : index.get(pc >> 2) * 4 + (pc & 3); };
  vis.forEach((a, i) => { for (let q = 0; q < 3; q++) { const f = N.far[3 * a + q]; if (f >= 0) far[3 * i + q] = node(f); } });
  podList.forEach((g, k) => { far[3 * (vis.length + k)] = node(pods.get(g).out); });
  const halos = [];
  for (const o of open) {
    const nodes = [];
    for (const a of o.ids) { const i = index.get(a); if (i !== undefined) nodes.push(i); }
    for (const h of o.within) { const i = index.get(-1 - h); if (i !== undefined) nodes.push(i); }
    if (nodes.length >= 2) halos.push({ g: o.g, name: o.name, nodes, byHand: o.byHand });
  }
  NET = { n, id, tag, far, index, op, pods, halos, tops, cl: new Int32Array(n).fill(-1), clusters: [], shape: null, marks: new Map() };
  marks();
  structures();
  shapes();
}
const isBitMark = text => text === "b0" || text === "b1";
/// The largest net whose numbers are looked for, in nodes: looking reads the whole net.
const MARKED = 6000;
/// Find the numbers the net holds: each number, each tree of bits with no depth beside it, and each bit,
/// by the node at its top. A part that is whole has its name already (so a bit that was written, or
/// copied whole, is b0 or b1), and what is folded shows nothing.
function marks() {
  if (NET.n > MARKED) return;
  const N = RUN.net, where = new Map(), folded = new Map();
  for (const pod of NET.pods.values()) folded.set(pod.top, pod.name);
  const term = N.read(N.mate(N.root * 4), { budget: 20 * MARKED, short: folded, where });
  for (const [a, text] of marksOf(term, where)) { const i = NET.index.get(a); if (i !== undefined && !NET.tops.has(a)) NET.marks.set(i, text); }
}
/// Whether a node is code or sits in it: a split and an eraser on a binder's own wire do.
const inCode = i => { const t = NET.tag[i], f = NET.far[3 * i]; return CODE.has(t) || t === K.sig || t === K.pod || (t === K.eraI && f >= 0 && bindPort(NET.tag[f >> 2], f & 3)); };
/// Find the structures: code that hangs together. Two nodes of code joined by a wire are in one, so a
/// structure is one value, or one stretch of code, that something running works on. What runs, copies
/// or erases stands alone between them. Each gets a label: the name of the part most of it comes from,
/// or else the start of its term. One that holds a number is data, whatever built it, and gets its term.
function structures() {
  const { n, far, tag, id, cl } = NET, N = RUN.net, up = new Int32Array(n);
  const find = i => { while (up[i] !== i) { up[i] = up[up[i]]; i = up[i]; } return i; };
  for (let i = 0; i < n; i++) up[i] = i;
  const code = new Uint8Array(n); for (let i = 0; i < n; i++) code[i] = inCode(i) ? 1 : 0;
  for (let i = 0; i < n; i++) if (code[i]) for (let q = 0; q < 3; q++) { const f = far[3 * i + q], j = f >> 2; if (f >= 0 && j > i && code[j]) up[find(i)] = find(j); }
  const at = new Map();
  for (let i = 0; i < n; i++) {
    if (!code[i]) continue;
    const r = find(i); let c = at.get(r);
    if (c === undefined) { c = NET.clusters.length; at.set(r, c); NET.clusters.push({ nodes: [], top: i, label: "", hue: -1 }); }
    cl[i] = c; NET.clusters[c].nodes.push(i);
  }
  for (const c of NET.clusters) {
    for (const i of c.nodes) { const f = far[3 * i]; if (POL[tag[i]][0] === "o" && tag[i] !== K.sig && (f < 0 || cl[f >> 2] !== cl[i])) { c.top = i; break; } }
    if (c.nodes.length < 3) continue;
    const tally = new Map(); let agents = 0;
    for (const i of c.nodes) { if (tag[i] === K.pod) continue; agents++; const g = N.grp.get(id[i]); if (g !== undefined) tally.set(N.parts[g].name, (tally.get(N.parts[g].name) ?? 0) + 1); }
    const [name, count] = [...tally].sort((p, q) => q[1] - p[1])[0] ?? ["", 0];
    const data = c.nodes.some(i => { const m = NET.marks.get(i); return m !== undefined && !isBitMark(m); });
    if (count >= 3 && count >= 0.6 * agents && !data) { c.label = name; c.hue = hueOf(name); continue; }
    if (tag[c.top] === K.pod) continue;
    const text = show(N.read(id[c.top] * 4, { budget: data ? 6000 : 40, short: NET.tops }));   // a number must be read whole to be told
    c.label = text.length > 28 ? text.slice(0, 27) + "…" : text;
  }
}
/// A wire on a variable's way from its binder to its uses: drawn thin and violet, and it does not say
/// which end hangs below the other.
const bindPort = (t, q) => ((t === K.lam || t === K.Lam) && q === 1) || t === K.sig || t === K.dupO || t === K.eraO;
/// Whether port q of an agent of kind t hands a value up to a reader: that agent hangs below.
const isSource = (t, q) => POL[t][q] === "o" && !bindPort(t, q);
const isBind = (ti, q, tj, p) => bindPort(ti, q) || bindPort(tj, p);
/// 0: not a pair. 1: about to rewrite. 2: joined by their main ports with no rule (stuck).
function pairState(ti, q, tj, p) {
  if (q || p || ti === K.root || tj === K.root || ti === K.pod || tj === K.pod) return 0;
  return ruleFor(ti, tj) ? 1 : 2;
}
/// Half the width and half the height of a pod, by its name.
const podBox = name => [(name.length * 0.36 + 0.75) * GRAD, 1.05 * GRAD];

// ---- layout (after rev.html and graph.js of the strand player) -------------------------------------
const GSP = 30, GRAD = 7, GREP = 30;
/// Where each node is: id -> [x, y, vx, vy, turn, flip as drawn (-1 to 1), flip it is going to].
const POS = new Map();
let alpha = 1, view = { s: 2, ox: 0, oy: 0 };
const jitter = (id, salt) => (Math.imul(id * 2 + salt, 2654435761) >>> 0) / 4294967296 - 0.5;
function place() {
  const { n, id, far, tag } = NET;
  const at = i => POS.has(id[i]);
  let fresh = 0; const placed = [];
  for (let i = 0; i < n; i++) if (!at(i)) fresh++;
  const beside = POS.size > 0;
  const nearPlaced = () => {
    const queue = [];
    for (let i = 0; i < n; i++) if (!at(i)) for (let q = 0; q < 3; q++) { const f = far[3 * i + q]; if (f >= 0 && at(f >> 2)) { queue.push(i); break; } }
    for (let h = 0; h < queue.length; h++) {
      const i = queue[h]; if (at(i)) continue;
      let sx = 0, sy = 0, c = 0;
      for (let q = 0; q < 3; q++) {
        const f = far[3 * i + q], j = f >> 2; if (f < 0 || j === i || !at(j)) continue;
        const p = POS.get(id[j]), bind = isBind(tag[i], q, tag[j], f & 3);
        if (!bind && p[4] !== undefined) { const t = tipAt(SHAPES[tag[j]], f & 3, p[0], p[1], p[4], GRAD, p[5] ?? 1); sx += p[0] + t[2] * GSP; sy += p[1] + t[3] * GSP; }   // out along the pin it is wired to
        else { sx += p[0]; sy += p[1] + (bind ? 0 : isSource(tag[i], q) ? 0.6 : -0.6) * GSP; }
        c++;
      }
      if (!c) continue;
      POS.set(id[i], [sx / c + jitter(id[i], 0) * GSP * 0.3, sy / c + jitter(id[i], 1) * GSP * 0.2, 0, 0]);
      placed.push(id[i]);
      for (let q = 0; q < 3; q++) { const f = far[3 * i + q]; if (f >= 0 && !at(f >> 2)) queue.push(f >> 2); }
    }
  };
  if (beside) nearPlaced();
  // A fresh load: the term hung as a tidy tree from the root down; splits and erasers then go between
  // their binder and its uses.
  hang(at, beside, true);
  nearPlaced();
  hang(at, POS.size > 0, false);
  // what a rewrite just made starts near where the pair met, and grows out from there
  // (run the physical way it starts a wire's length off, so that it has some way to go to its next pair)
  const out = physical() ? 1 : 0.35;
  for (const a of placed) { const b = ANIM.born.get(a); if (b) { const p = POS.get(a); p[0] = b.x + (p[0] - b.x) * out; p[1] = b.y + (p[1] - b.y) * out; } }
  // forget what is gone; an agent folded away keeps its place for when its part opens
  for (const k of [...POS.keys()]) if (!NET.index.has(k) && (k < 0 || !RUN.net.kind[k])) POS.delete(k);
  alpha = Math.max(alpha, Math.min(0.6, 0.15 + 2 * fresh / Math.max(1, n)));
}
/// What feeds agent i's inputs, in the order a term reads them.
function inputs(i) {
  const t = NET.tag[i], out = [];
  for (let q = 0; q <= ARITY[t]; q++) {
    const f = NET.far[3 * i + q]; if (f < 0 || POL[t][q] !== "i") continue;
    if (!isBind(t, q, NET.tag[f >> 2], f & 3)) out.push(f >> 2);
  }
  return out;
}
/// Tidy trees (after Reingold and Tilford), one hung from each of `starts`, `kids(i)` being what hangs
/// below node i: the nodes in the order met, and for each how far across it sits and how deep.
function tidy(starts, kids) {
  const n = NET.n, order = [], tops = [], depth = new Int32Array(n), first = new Int32Array(n).fill(-1), last = new Int32Array(n).fill(-1), next = new Int32Array(n).fill(-1), seen = new Uint8Array(n);
  for (const s0 of starts) {
    if (seen[s0]) continue;
    tops.push(s0);
    const stack = [s0, -1];
    while (stack.length) {
      const p = stack.pop(), i = stack.pop();
      if (seen[i]) continue;
      seen[i] = 1; order.push(i); depth[i] = p < 0 ? 0 : depth[p] + 1;
      if (p >= 0) { if (last[p] < 0) first[p] = i; else next[last[p]] = i; last[p] = i; }
      const ins = kids(i);
      for (let k = ins.length - 1; k >= 0; k--) if (!seen[ins[k]]) stack.push(ins[k], i);
    }
  }
  const outline = new Array(n), rel = new Float64Array(n);
  if (!order.length) return { order, rel, depth };
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
  for (const i of order) for (let c = first[i]; c >= 0; c = next[c]) rel[c] += rel[i];
  return { order, rel, depth };
}
/// The room between one row of a tidy tree and the next.
const ROW = GSP * 0.85;
/// Lay out the agents not yet placed as trees, each hung from a top (the root first), what feeds an
/// input below it. `treeOnly` leaves agents that only sit on variable wires for later. To the right of
/// what is placed if `beside`.
function hang(at, beside, treeOnly) {
  const { n, far, tag, id } = NET, starts = [];
  const onWire = i => tag[i] === K.sig || tag[i] === K.dupO || tag[i] === K.eraO || (tag[i] === K.eraI && far[3 * i] >= 0 && bindPort(tag[far[3 * i] >> 2], far[3 * i] & 3));
  const skip = i => at(i) || (treeOnly && onWire(i));
  for (let i = 0; i < n; i++) {
    if (skip(i)) continue;
    let read = false;
    for (let q = 0; q <= ARITY[tag[i]]; q++) { const f = far[3 * i + q]; if (f >= 0 && isSource(tag[i], q) && !isBind(tag[i], q, tag[f >> 2], f & 3) && !skip(f >> 2)) read = true; }
    if (!read) starts.push(i);
  }
  starts.sort((a, b) => (tag[b] === K.root) - (tag[a] === K.root));
  for (let i = 0; i < n; i++) if (!skip(i)) starts.push(i);
  const { order, rel, depth } = tidy(starts, i => inputs(i).filter(k => !skip(k)));
  if (!order.length) return;
  let x0 = Infinity, x1 = -Infinity, deepest = 0;
  for (const i of order) { x0 = Math.min(x0, rel[i]); x1 = Math.max(x1, rel[i]); deepest = Math.max(deepest, depth[i]); }
  let dx = -(x0 + x1) / 2, dy = -deepest * ROW / 2;
  if (beside) {
    let right = -Infinity, top = Infinity;
    for (const [, p] of POS) { right = Math.max(right, p[0]); top = Math.min(top, p[1]); }
    dx = right + GSP * 2 - x0; dy = top;
  }
  for (const i of order) POS.set(id[i], [rel[i] + dx, depth[i] * ROW + dy, 0, 0]);
}
/// The shape each structure settles into: a tidy tree hung from its top. For each node of one, where it
/// belongs, as a way across and a way down from the top (`NET.shape`).
function shapes() {
  const { n, cl, clusters } = NET, top = new Int32Array(n).fill(-1), dx = new Float32Array(n), dy = new Float32Array(n);
  const tops = clusters.filter(c => c.nodes.length >= 3).map(c => c.top);
  const { order, rel, depth } = tidy(tops, i => inputs(i).filter(k => cl[k] === cl[i]));
  for (const i of order) { const t = clusters[cl[i]].top; top[i] = t; dx[i] = rel[i] - rel[t]; dy[i] = depth[i] * ROW; }
  NET.shape = { top, dx, dy };
}
/// The room between the tips of two pins that a wire joins: inside one structure, and between two.
const REST = 8, REST_APART = 30;
/// How far apart two nodes of different structures keep, where no wire joins them.
const GAPART = 66;
/// The agent being dragged, held where the pointer is: { id, x, y }.
let HELD = null;
/// Let the layout settle for some steps. `pulling`: the two agents of every pair that can fire are drawn
/// together until their main pins touch.
function relax(steps, pulling = false) {
  const { n, id, far, tag, cl, shape } = NET;
  if (!n) return;
  const x = new Float64Array(n), y = new Float64Array(n), vx = new Float64Array(n), vy = new Float64Array(n), th = new Float64Array(n), fl = new Float64Array(n), ft = new Float64Array(n), deg = new Uint8Array(n);
  for (let i = 0; i < n; i++) { const p = POS.get(id[i]); x[i] = p[0]; y[i] = p[1]; vx[i] = p[2]; vy[i] = p[3]; th[i] = p[4] ?? NaN; fl[i] = p[5] ?? 1; ft[i] = p[6] ?? 1; }
  const wa = [], wb = [], wq = [], wp = [], src = [], bind = [], rest = [], partner = new Int32Array(n).fill(-1);
  for (let i = 0; i < n; i++) for (let q = 0; q < 3; q++) {
    const f = far[3 * i + q], j = f >> 2; if (f < 0 || j === i) continue;
    const b = isBind(tag[i], q, tag[j], f & 3);
    if (!b) deg[i]++;
    if (j < i) continue;
    const pair = pulling && pairState(tag[i], q, tag[j], f & 3) === 1;
    if (pair) { partner[i] = j; partner[j] = i; }
    wa.push(i); wb.push(j); wq.push(q); wp.push(f & 3); bind.push(b && !pair); src.push(b ? -1 : isSource(tag[i], q) ? i : isSource(tag[j], f & 3) ? j : -1);
    rest.push(pair ? 0 : cl[i] < 0 || cl[i] !== cl[j] ? REST_APART : REST);
  }
  // Which nodes keep close: those of one structure. A pair being drawn together brings its two
  // structures into one, so that neither holds the other's agent off.
  const group = new Int32Array(n), lead = new Int32Array(NET.clusters.length + n);
  for (let g = 0; g < lead.length; g++) lead[g] = g;
  const find = g => { while (lead[g] !== g) { lead[g] = lead[lead[g]]; g = lead[g]; } return g; };
  for (let i = 0; i < n; i++) group[i] = cl[i] >= 0 ? cl[i] : NET.clusters.length + i;
  for (let i = 0; i < n; i++) if (partner[i] > i) lead[find(group[i])] = find(group[partner[i]]);
  for (let i = 0; i < n; i++) group[i] = find(group[i]);
  const held = HELD ? NET.index.get(HELD.id) : undefined;
  // Each icon turns as a whole, and flips left for right, to whatever best faces its pins toward what
  // they are wired to. A turn glides; a flip goes through edge-on. The root and pods stay upright.
  const dirs = [0, 0, 0], w = [0, 0, 0];
  const turn = () => {
    for (let i = 0; i < n; i++) {
      const t = tag[i];
      if (t === K.root || t === K.pod) { th[i] = 0; fl[i] = ft[i] = 1; continue; }
      const sh = SHAPES[t];
      for (let q = 0; q < 3; q++) {
        const f = q < sh.ports.length ? far[3 * i + q] : -1, j = f >> 2;
        if (f < 0 || j === i) { dirs[q] = undefined; continue; }
        dirs[q] = Math.atan2(y[j] - y[i], x[j] - x[i]); w[q] = deg[i] && isBind(t, q, tag[j], f & 3) ? 0.35 : 1;
      }
      const fit = bestFit(sh, dirs, w, ft[i]);
      if (!fit.any) { if (isNaN(th[i])) th[i] = 0; continue; }
      ft[i] = fit.flip;
      if (isNaN(th[i])) { th[i] = fit.th; fl[i] = fit.flip; continue; }
      const d = fit.th - th[i]; th[i] += Math.atan2(Math.sin(d), Math.cos(d)) * 0.3;
      fl[i] += (ft[i] - fl[i]) * 0.3; if (Math.abs(ft[i] - fl[i]) < 0.03) fl[i] = ft[i];
    }
  };
  /// A pin pulls what it is wired to onto the line it points along: from tip P (with its way out) to tip Q.
  const align = (P, Q, e, o, f) => {
    const ex = Q[0] - P[0], ey = Q[1] - P[1], along = ex * P[2] + ey * P[3], px = ex - along * P[2], py = ey - along * P[3];
    vx[o] -= px * f; vy[o] -= py * f; vx[e] += px * f; vy[e] += py * f;
  };
  turn();
  for (let s = 0; s < steps && alpha > 0.004; s++, alpha *= 0.985) {
    const a = alpha;
    if (held !== undefined) { x[held] = HELD.x; y[held] = HELD.y; vx[held] = vy[held] = 0; }
    for (let k = 0; k < wa.length; k++) {
      const i = wa[k], j = wb[k];
      if (bind[k]) {
        // a variable's wire pulls only when it is stretched far, and weakly: the tree keeps its shape
        let dx = x[j] + vx[j] - x[i] - vx[i], dy = y[j] + vy[j] - y[i] - vy[i];
        const d = Math.sqrt(dx * dx + dy * dy) || 0.01;
        if (d < GSP * 2.2) continue;
        const f = (d - GSP * 2.2) / d * a * 0.08;
        dx *= f; dy *= f; vx[j] -= dx * 0.5; vy[j] -= dy * 0.5; vx[i] += dx * 0.5; vy[i] += dy * 0.5;
        continue;
      }
      if (!rest[k]) {
        // a pair being drawn together: each agent goes straight for the other, the one with fewer wires faster
        let dx = x[j] + vx[j] - x[i] - vx[i], dy = y[j] + vy[j] - y[i] - vy[i];
        const d = Math.sqrt(dx * dx + dy * dy) || 0.01, f = Math.max(0, d - TOUCH * 0.6) / d * Math.min(1, a * 4), bias = deg[i] || deg[j] ? deg[i] / (deg[i] + deg[j]) : 0.5;
        dx *= f; dy *= f; vx[j] -= dx * bias; vy[j] -= dy * bias; vx[i] += dx * (1 - bias); vy[i] += dy * (1 - bias);
        continue;
      }
      // a spring between the two pin tips, and each pin pulling the other end into line with it
      const A = tipAt(SHAPES[tag[i]], wq[k], x[i] + vx[i], y[i] + vy[i], th[i], GRAD, fl[i]), B = tipAt(SHAPES[tag[j]], wp[k], x[j] + vx[j], y[j] + vy[j], th[j], GRAD, fl[j]);
      let dx = B[0] - A[0], dy = B[1] - A[1];
      const d = Math.sqrt(dx * dx + dy * dy) || 0.01, f = (d - rest[k]) / d * a / Math.max(1, Math.min(deg[i], deg[j])), bias = deg[i] / Math.max(1, deg[i] + deg[j]);
      // an agent being drawn to its partner heeds no other wire: nothing can hold it back
      const gi = partner[i] >= 0 ? 0 : 1, gj = partner[j] >= 0 ? 0 : 1;
      dx *= f; dy *= f; vx[j] -= dx * bias * gj; vy[j] -= dy * bias * gj; vx[i] += dx * (1 - bias) * gi; vy[i] += dy * (1 - bias) * gi;
      if (!gi || !gj) continue;
      align(A, B, i, j, a * 0.1); align(B, A, j, i, a * 0.1);
      const o = src[k];
      if (o >= 0) { const r = o === i ? j : i, short = GSP * 0.75 - (y[o] - y[r]); if (short > 0) { const g = short * a * 0.12; vy[o] += g; vy[r] -= g; } }
    }
    // each structure is drawn into its shape, a tidy tree hung from its top
    for (let i = 0; i < n; i++) {
      const t = shape.top[i]; if (t < 0 || t === i || partner[i] >= 0) continue;
      vx[i] += (x[t] + shape.dx[i] - x[i]) * a * 0.2; vy[i] += (y[t] + shape.dy[i] - y[i]) * a * 0.2;
    }
    // an agent that only sits on variable wires rests between what it joins
    for (let i = 0; i < n; i++) {
      if (deg[i] || partner[i] >= 0) continue;
      let sx = 0, sy = 0, c = 0;
      for (let q = 0; q < 3; q++) { const f = far[3 * i + q]; if (f >= 0 && (f >> 2) !== i) { sx += x[f >> 2]; sy += y[f >> 2]; c++; } }
      if (c) { vx[i] += (sx / c - x[i]) * a * 0.12; vy[i] += (sy / c - y[i]) * a * 0.12; }
    }
    // Nodes push each other away: those of one structure, or joined by a wire, from close by; those of
    // two structures from further off, so that structures stand apart.
    const cells = new Map(), C = GAPART, kr = a * 0.3;
    for (let i = 0; i < n; i++) { const c = Math.floor(x[i] / C) * 65536 + Math.floor(y[i] / C); let l = cells.get(c); if (!l) cells.set(c, l = []); l.push(i); }
    for (let i = 0; i < n; i++) {
      const cx = Math.floor(x[i] / C), cy = Math.floor(y[i] / C);
      for (let ox = -1; ox <= 1; ox++) for (let oy = -1; oy <= 1; oy++) for (const j of cells.get((cx + ox) * 65536 + cy + oy) ?? []) {
        if (j <= i || partner[i] >= 0 || partner[j] >= 0) continue;   // an agent on its way to its partner passes through what is between
        let dx = x[j] - x[i], dy = y[j] - y[i], d2 = dx * dx + dy * dy;
        const near = group[i] === group[j] || (far[3 * i] >> 2) === j || (far[3 * i + 1] >> 2) === j || (far[3 * i + 2] >> 2) === j, R = near ? GREP : GAPART;
        if (d2 >= R * R) continue;
        if (d2 < 1e-6) { dx = jitter(i, 0); dy = jitter(j, 1); d2 = dx * dx + dy * dy; }
        const d = Math.sqrt(d2), f = (R - d) / d * kr * (near ? 1 : 1.5);
        vx[j] += dx * f; vy[j] += dy * f; vx[i] -= dx * f; vy[i] -= dy * f;
      }
    }
    let mx = 0, my = 0; for (let i = 0; i < n; i++) { mx += x[i]; my += y[i]; }
    mx = held === undefined ? mx / n * a * 0.02 : 0; my = held === undefined ? my / n * a * 0.02 : 0;
    for (let i = 0; i < n; i++) {
      if (i === held) { vx[i] = vy[i] = 0; continue; }
      let u = (vx[i] - mx) * 0.6, v = (vy[i] - my) * 0.6; const sp = Math.hypot(u, v);
      if (sp > GSP / 3) { u *= GSP / 3 / sp; v *= GSP / 3 / sp; }
      vx[i] = u; vy[i] = v; x[i] += u; y[i] += v;
    }
    turn();
  }
  for (let i = 0; i < n; i++) POS.set(id[i], [x[i], y[i], vx[i], vy[i], th[i], fl[i], ft[i]]);
}
const maxScale = 2.5;
let fitMargins = [70, 200, 0, 0];
function fit(glide = false) {
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (const [, p] of POS) { x0 = Math.min(x0, p[0]); y0 = Math.min(y0, p[1]); x1 = Math.max(x1, p[0]); y1 = Math.max(y1, p[1]); }
  if (!isFinite(x0)) { x0 = y0 = -GSP; x1 = y1 = GSP; }
  const pad = GSP, box = cv.getBoundingClientRect(), [top, bottom, left, right] = fitMargins;
  const r = { width: box.width - left - right, height: box.height };
  x0 -= pad; y0 -= pad; x1 += pad; y1 += pad;
  const s = Math.min(maxScale, (r.width - 30) / (x1 - x0), (r.height - top - bottom) / (y1 - y0));
  const target = { s, ox: left + (r.width - (x1 - x0) * s) / 2 - x0 * s, oy: top + (r.height - top - bottom - (y1 - y0) * s) / 2 - y0 * s };
  if (!glide) { view = target; return; }
  const W2 = box.width / 2, H2 = box.height / 2;
  const k = 0.12, ns = view.s * (target.s / view.s) ** k, mx = (W2 - view.ox) / view.s, my = (H2 - view.oy) / view.s;
  const tx = (W2 - target.ox) / target.s, ty = (H2 - target.oy) / target.s, cx = mx + (tx - mx) * k, cy = my + (ty - my) * k;
  view = { s: ns, ox: W2 - cx * ns, oy: H2 - cy * ns };
}

// ---- running ---------------------------------------------------------------------------------------
let mode = 0, credit = 0, dirty = true;
const rate = () => 10 ** (+$("speed").value / 25 - 1.2);
/// The physical way of running: pairs are drawn together, and one fires when its main pins touch.
const physical = () => $("physical").checked;
const byRound = () => $("byRound").checked && RUN.order === "rounds" && RUN.rounds.length > 2 && !physical() && RUN.status !== "open";
const tlLength = () => byRound() ? RUN.rounds.length - 1 : RUN.n;
const tlPos = () => byRound() ? RUN.roundsDone(RUN.at) : RUN.at;
/// A rewrite, shown as it happens: the two agents it used up shrink into the place they met (`ghosts`),
/// a ring spreads from there (`sparks`), and what it made grows out of it (`born`: id -> when and where).
const ANIM = { ghosts: [], sparks: [], born: new Map() };
const animTime = () => mode === 0 ? 360 : Math.max(140, Math.min(360, 360 / Math.max(rate(), 0.5)));
/// Rewrites `from` up to `to` have just been made, and the net on show is still the one before them.
function animate(from, to) {
  if (!$("motion").checked || to - from > 40) return;
  const t0 = performance.now(), dur = animTime();
  for (let e = from; e < to; e++) {
    const a = RUN.pa[e], b = RUN.pb[e], ia = NET.index.get(a), ib = NET.index.get(b), pa = POS.get(a), pb = POS.get(b);
    if (ia === undefined || ib === undefined || !pa || !pb) continue;
    const mx = (pa[0] + pb[0]) / 2, my = (pa[1] + pb[1]) / 2;
    for (const [i, p] of [[ia, pa], [ib, pb]]) ANIM.ghosts.push({ t: NET.tag[i], c: colourOf(NET.tag[i], NET.op[i]), x: p[0], y: p[1], th: p[4] ?? 0, fl: p[5] ?? 1, mx, my, t0, dur });
    ANIM.sparks.push({ x: mx, y: my, c: FAMILY[RUN.fam[e]][1], t0, dur });
    for (let k = 0; k < RUN.madeN[e]; k++) ANIM.born.set(RUN.madeAt[e] + k, { x: mx, y: my, t0, dur });
  }
}
/// A run whose pairs were fired where they met has no end planned: plan the rest from the moment on show.
function settle() { if (RUN.status === "open") { RUN.replan(); dirty = true; } }
/// How far apart the middles of two agents are when they touch.
const TOUCH = GRAD * 2;
/// The pairs that can fire, as the layout has them: [node, node, the room left between the two agents].
function activePairs() {
  const out = [];
  for (let i = 0; i < NET.n; i++) {
    const pr = pairOf(i); if (!pr || pr[1] < i) continue;
    const a = POS.get(NET.id[i]), b = POS.get(NET.id[pr[1]]);
    out.push([i, pr[1], Math.max(0, Math.hypot(b[0] - a[0], b[1] - a[1]) - TOUCH)]);
  }
  return out;
}
/// Frames in a row in which pairs were on their way and none met.
let waiting = 0;
/// Fire the pairs that have met (or, for one step, the closest pair there is). With no pair left the run
/// is over, and it is planned so that its end is known. Should none meet for three seconds, the closest
/// goes anyway. How many fired.
function touch(closest = false) {
  if (waiting > 180) closest = true;
  const pairs = activePairs().sort((p, q) => p[2] - q[2]), from = RUN.at;
  if (!pairs.length || from >= RUN.limit) { RUN.replan(); dirty = true; return 0; }
  // an agent still growing out of the rewrite that made it is not ready to meet anything
  const now = performance.now(), growing = i => { const b = ANIM.born.get(NET.id[i]); return !!b && now - b.t0 < b.dur; };
  let fired = 0;
  for (const [i, j, gap] of closest ? pairs.slice(0, 1) : pairs) {
    if (!closest && (gap > 0 || growing(i) || growing(j))) continue;
    if (RUN.fireLive(NET.id[i], NET.id[j])) fired++;
  }
  waiting = fired ? 0 : waiting + 1;
  if (fired) { animate(from, RUN.at); dirty = true; }
  return fired;
}
function forward(n) {
  const from = RUN.at;
  if (byRound()) RUN.seek(RUN.rounds[Math.min(RUN.rounds.length - 1, RUN.roundsDone(RUN.at) + n)]); else RUN.seek(RUN.at + n);
  if (RUN.at !== from) { animate(from, RUN.at); dirty = true; }
  return RUN.at - from;
}
function back(n) {
  const from = RUN.at;
  if (byRound()) { const r = RUN.roundsDone(from); RUN.seek(RUN.rounds[Math.max(0, (RUN.rounds[r] < from ? r + 1 : r) - n)]); } else RUN.seek(from - n);
  if (RUN.at !== from) dirty = true;
  return from - RUN.at;
}
function seek(t) { RUN.seek(byRound() ? RUN.rounds[t] : t); dirty = true; }
function setMode(m) {
  mode = m; credit = 0;
  $("playFwd").classList.toggle("on", m === 1); $("playBack").classList.toggle("on", m === -1);
}
/// What each pod on show hides, and where it was when it folded: part -> { cover, x, y }.
let FOLD = new Map();
function sync() {
  if (!dirty) return;
  dirty = false;
  const before = FOLD;
  readNet();
  // a part that has just opened: its agents come out around where its pod was
  for (const [g, was] of before) {
    if (NET.pods.has(g)) continue;
    const p = POS.get(-1 - g);
    if (p && was.x !== undefined) { const dx = p[0] - was.x, dy = p[1] - was.y; for (const a of was.cover) { const q = POS.get(a); if (q) { q[0] += dx; q[1] += dy; } } }
    POS.delete(-1 - g);
  }
  // a part that has just folded: its pod goes where its agents were
  FOLD = new Map();
  for (const [g, pod] of NET.pods) {
    let p = POS.get(-1 - g);
    if (!p) { let sx = 0, sy = 0, c = 0; for (const a of pod.cover) { const q = POS.get(a); if (q) { sx += q[0]; sy += q[1]; c++; } } if (c) POS.set(-1 - g, p = [sx / c, sy / c, 0, 0]); }
    const was = before.get(g);
    FOLD.set(g, { cover: pod.cover, x: was ? was.x : p ? p[0] : undefined, y: was ? was.y : p ? p[1] : undefined });
  }
  place();
  for (const [g, rec] of FOLD) if (rec.x === undefined) { const p = POS.get(-1 - g); rec.x = p[0]; rec.y = p[1]; }
  hudText();
}
const clip = (text, most) => text.length > most ? text.slice(0, most - 1) + "…" : text;
const ruleOf = e => { const k = RUN.kinds[e]; return ruleFor(k >> 4, k & 15)[2]; };
const eventText = e => { const k = RUN.kinds[e]; return `${SHORT[KINDS[k >> 4]]} meets ${SHORT[KINDS[k & 15]]}: ${MEANING[ruleOf(e)]}`; };
function hudText() {
  const R = RUN, N = R.net, t = R.at, end = t === R.n;
  $("answer").textContent = end && R.answer ? clip(show(N.read(N.mate(N.root * 4), { budget: 30000, short: NET.tops })), 220) : "";
  let s, cls = "";
  if (R.status === "open" && end) { const k = N.pairs().filter(p => p[2]).length; s = `rewrite ${t}, each fired where its pair met: ${k} pair${k === 1 ? "" : "s"} on the way to meeting`; }
  else if (end && R.status === "done") { s = `the end: ${R.n} rewrites${R.order === "rounds" && !R.byHand ? ` in ${R.rounds.length - 1} rounds` : ""}, and only the answer is left`; cls = "ok"; }
  else if (end && R.status === "stuck") { s = `stuck after ${R.n} rewrites: ${R.stuckPairs} pair${R.stuckPairs > 1 ? "s" : ""} with no rule (red)`; cls = "bad"; }
  else if (end) { s = `stopped after ${R.n} rewrites: this run goes on (the page keeps ${R.limit})`; cls = "bad"; }
  else if (!t) s = `the program, loaded: ${N.live} agents, ${R.n}${R.status === "limit" ? "+" : ""} rewrites to come`;
  else if (byRound()) { const r = R.roundsDone(t); s = `round ${r} of ${R.rounds.length - 1}${R.rounds[r] === t && r ? `: ${R.rounds[r] - R.rounds[r - 1]} rewrite${R.rounds[r] - R.rounds[r - 1] > 1 ? "s" : ""} at once` : ""}`; }
  else s = `rewrite ${t} of ${R.n}${R.status === "limit" ? "+" : ""}`;
  $("status").textContent = s; $("status").className = cls;
  $("rule").textContent = !byRound() && t ? eventText(t - 1) : "";
  $("whole").textContent = clip(show(N.read(N.mate(N.root * 4), { budget: 4000, short: NET.tops })), 520);
  const copies = new Set(); let running = 0, code = 0, copiers = 0, erasers = 0;
  for (let i = 0; i < N.live; i++) {
    const a = N.ids[i], k = N.kind[a];
    if (k === K.O || k === K.T || k === K.R) running++;
    else if (k === K.dupI || k === K.dupO) { copiers++; copies.add(N.op.get(a)); }
    else if (k === K.eraI || k === K.eraO) erasers++;
    else if (k !== K.root && k !== K.sig) code++;
  }
  const c = N.counts, folded = N.live - (NET.n - NET.pods.size);
  $("counts").innerHTML = [
    ["agents in the net", N.live], ["…folded into named parts", `${folded} in ${NET.pods.size}`], ["…code and values", code], ["…running (apply, thaw, read)", running], ["…copiers / erasers", `${copiers} / ${erasers}`],
    ["copies under way", copies.size], ["rewrites done", `${t} of ${R.n}${R.status === "limit" ? "+" : ""}`], ["…pairs you fired", R.byHand],
    ["…calls / opens / reads", `${c[0]} / ${c[1]} / ${c[2]}`], ["…thaws", c[3]], ["…copies / cancels / erases", `${c[4]} / ${c[5]} / ${c[6]}`],
  ].map(([k, v]) => `<div><span>${k}</span><span>${v}</span></div>`).join("");
}

// ---- loading ---------------------------------------------------------------------------------------
const EXAMPLES = [
  ["(\\x. x) (\\y. y)", "a call"],
  ["(\\x y. x) ρ ρ", "an argument that is not used"],
  ["(\\f. f (f ρ)) (\\x. x)", "a function used twice"],
  ["(\\x. \\y. (\\z. z) x) ρ", "the body of a function stays frozen"],
  ["(\\f. f (f (f ρ))) (\\x. x)", "a function used three times: two copies at once"],
  ["(\\t. t t) (\\f x. f (f x)) (\\y. y) ρ", "two times two: copies of a function that copies"],
  ["(^d. (\\x. x x) (\\y. y)) ρ", "a quote: an opened function hands back its body"],
  ["ρ (\\x. x x) (\\o a v p F. F ρ) no no no no", "read a function, and open it"],
  ["kindOf ρ", "is it a function?"],
  ["close (open suc) zero", "read a function, close it, run it"],
  ["add (suc zero) (suc zero)", "1 + 1, left as a function"],
  ["size (\\x y. y x)", "count the binders in a function (recursion, reading)"],
  ["interp ((^d. (\\x. x) (\\y. y)) ρ)", "the interpreter on a quoted program"],
  ["linear_unary_add 2u 3u", "linear unary add, 2 + 3: nothing is erased, one copy per successor"],
  ["linear_list_add 3 1", "linear add on lists of bits, 3 + 1: five copies per bit, and no eraser"],
  ["linear_tree_add 3t 1t", "linear add on trees of bits, 3 + 1: the sum is a tree again"],
  ["reversible_tree_add 3t 1t reversible_tree_sub", "an add that can be undone: 3 + 1, then the subtract, and 3 and 1 are back"],
  ["linear_unary_add 100u 100u", "a longer loop: 100 + 100 in unary (4,325 rewrites)"],
  ["linear_list_add 1000 2000", "1000 + 2000 on lists of twelve bits (3,505 rewrites)"],
  ["size size", "size reads its own code (165,713 rewrites)"],
  ["(^d. d) ρ (\\x. x)", "stuck: code that is not a function cannot be applied"],
  ["(\\x. x x) (\\x. x x)", "never ends"],
];
const TOUR = [
  ["One call, a rewrite at a time: the thaw walks down, the application starts running, and meets its function.", "src=(\\x. x) (\\y. y)&rounds=0"],
  ["A variable used twice: the split copies the function, and each copier cancels with its other end.", "src=(\\f. f (f ρ)) (\\x. x)&play=1&speed=14"],
  ["A frozen body: (λz. z) x sits inside λy and is never touched, though x has its value.", "src=(\\x. \\y. (\\z. z) x) ρ&play=1&speed=14"],
  ["Two copies under way at once, each in its own colour. A cancel always joins one colour.", "src=(\\t. t t) (\\f x. f (f x)) (\\y. y) ρ&play=1&speed=16"],
  ["Reading: ρ builds five arms, the first one gets the function opened, and opening it gives its body as code.", "src=ρ (\\x. x x) (\\o a v p F. F ρ) no no no no&play=1&speed=14"],
  ["A recursive program that reads code. Nearly all of the work is copying and erasing.", "src=size (\\x y. y x)&play=1&speed=62"],
  ["The same program, one pair at a time, newest first: the same answer after the same number of rewrites.", "src=size (\\x y. y x)&order=newest&play=1&speed=62"],
  ["A linear program: 2 + 3 with nothing erased. Each successor copies the step once, and zero hands the last copy back.", "src=linear_unary_add 2u 3u&play=1&speed=20"],
  ["A linear add on lists of bits, 3 + 1: each bit copies the step, and the step sorts three bits. No eraser ever appears.", "src=linear_list_add 3 1&play=1&speed=30"],
  ["Numbers as trees of bits, 3 + 1: the adder for four leaves is two adders for two. The sum is a tree again, tagged with its number.", "src=linear_tree_add 3t 1t&play=1&speed=30"],
  ["1014 + 2413 on trees of sixteen bits, with nothing erased. Its end shows the tree of 3427.", "src=linear_tree_add 1014t 2413t&play=1&speed=70"],
  ["The same sum the usual way: each bit is tested and the arm not taken is erased. Three times the rewrites.", "src=ordinary_tree_add 1014t 2413t&play=1&speed=90"],
  ["The same sum in parallel, a round at a time: every part of the trees works out its carry at once, and the carries then go down. A quarter of the rounds, and still nothing erased.", "src=linear_tree_add_parallel 1014t 2413t&play=1&speed=40"],
  ["An add that can be undone. It hands back 1014 beside the sum, and the subtract takes those two and hands back 1014 and 2413.", "src=reversible_tree_add 1014t 2413t reversible_tree_sub&play=1&speed=80"],
];
function load() {
  const src = $("src").value;
  try {
    RUN = new Run(src.replace(/;/g, "\n"), { order: $("order").value, seed: 7, limit: 400000 });
  } catch (e) {
    $("src").classList.add("bad"); $("status").className = "bad"; $("status").textContent = `cannot read the program: ${e.message}`; return false;
  }
  $("src").classList.remove("bad");
  RUN.seek(0);
  POS.clear(); OPEN.clear(); FOLD = new Map(); ANIM.ghosts.length = ANIM.sparks.length = 0; ANIM.born.clear();
  alpha = 1; dirty = true; hover = null; hoverPair = null; runGen++; setMode(0);
  $("byRound").disabled = physical() || RUN.order !== "rounds";
  sync(); relax(140); fit();
  return true;
}

// ---- drawing ---------------------------------------------------------------------------------------
/// What the pointer is on: a node (by id), a rewrite on the bar, or nothing; and the pair a click would fire.
let hover = null, hoverPair = null;
const hoverIndex = () => hover?.id !== undefined ? NET.index.get(hover.id) : undefined;
const lw = w => w / view.s;
/// The name tags drawn this frame, for the pointer: { g, name, hue, x, y, w, h, byHand } in the net's own lengths.
let TAGS = [];
const easeOutBack = p => { const c = 1.9; return 1 + (c + 1) * (p - 1) ** 3 + c * (p - 1) ** 2; };
function draw() {
  const d = devicePixelRatio || 1, r = cv.getBoundingClientRect(), now = performance.now();
  if (cv.width !== Math.round(r.width * d) || cv.height !== Math.round(r.height * d)) { cv.width = Math.round(r.width * d); cv.height = Math.round(r.height * d); }
  ctx.setTransform(1, 0, 0, 1, 0, 0); ctx.fillStyle = "#0b0e14"; ctx.fillRect(0, 0, cv.width, cv.height);
  ctx.setTransform(d * view.s, 0, 0, d * view.s, d * view.ox, d * view.oy);
  const { n, id, tag, far, op, cl } = NET, P = i => POS.get(id[i]), hi = hoverIndex(), big = view.s * GRAD > 5;
  ctx.lineCap = "round"; ctx.lineJoin = "round";
  const radOf = i => tag[i] === K.pod ? podBox(NET.pods.get(-1 - id[i]).name)[0] + GRAD : GRAD * 2.3;
  /// A soft band under some nodes: a disc at each, and a thick line along each wire that `joined` takes.
  const band = (nodes, joined, fill) => {
    const blob = new Path2D(), links = new Path2D();
    for (const i of nodes) {
      const p = P(i), rad = radOf(i);
      blob.moveTo(p[0] + rad, p[1]); blob.arc(p[0], p[1], rad, 0, 7);
      for (let q = 0; q < 3; q++) { const f = far[3 * i + q], j = f >> 2; if (f >= 0 && j > i && joined(i, j)) { const o = P(j); links.moveTo(p[0], p[1]); links.lineTo(o[0], o[1]); } }
    }
    ctx.fillStyle = ctx.strokeStyle = fill; ctx.lineWidth = GRAD * 3.4;
    ctx.fill(blob); ctx.stroke(links);
  };
  // every structure of code on a faint band of its own: where the band stops, the structure does
  const banded = []; for (let i = 0; i < n; i++) if (cl[i] >= 0 && NET.clusters[cl[i]].nodes.length > 1) banded.push(i);
  band(banded, (i, j) => cl[i] === cl[j], "#131d2e");
  // named parts drawn open: a band in the part's hue, the bigger ones first, and a tag with the name
  TAGS = [];
  for (const h of [...NET.halos].sort((p, q) => q.nodes.length - p.nodes.length)) {
    const inside = new Set(h.nodes), hue = hueOf(h.name);
    band(h.nodes, (i, j) => inside.has(j), `hsl(${hue} 38% 13%)`);
    let x0 = Infinity, y0 = Infinity;
    for (const i of h.nodes) { const p = P(i), rad = radOf(i); x0 = Math.min(x0, p[0] - rad); y0 = Math.min(y0, p[1] - rad); }
    if (big) TAGS.push({ g: h.g, name: h.name, hue, x: x0 + GRAD * 0.6, y: y0 - GRAD * 0.5, w: 0, h: 0, byHand: h.byHand });
  }
  // the subnet under the pointer, lit, and all else dimmed
  const lit = subnets() && hi !== undefined ? subnetOf(hi) : null;
  if (lit) band(lit, (i, j) => lit.has(j), "hsl(205 60% 21%)");
  const showBinds = $("binds").checked, holes = view.s * GRAD > 3.5;
  const scaleOf = i => { const b = ANIM.born.get(id[i]); if (!b) return 1; const p = (now - b.t0) / b.dur; if (p >= 1) { ANIM.born.delete(id[i]); return 1; } return Math.max(0.12, easeOutBack(Math.max(0, p))); };
  const sc = new Float32Array(n); for (let i = 0; i < n; i++) sc[i] = scaleOf(i);
  /// Draw the nodes `node` takes and the wires `wire` takes, as strongly as `fade` says.
  const paint = (node, wire, fade) => {
    // Wires, each from the tip of one pin to the tip of another, leaving each the way the pin points:
    // the tree's plain, a variable's thin and violet, a stuck pair's red, and a pair that can fire
    // bright, in the colour of what its rewrite will do.
    const plain = new Path2D(), binds = new Path2D(), red = new Path2D(), mine = new Path2D(), live = new Map();
    for (let i = 0; i < n; i++) for (let q = 0; q < 3; q++) {
      const f = far[3 * i + q], j = f >> 2; if (f < 0 || j < i || !wire(i, j)) continue;
      const a = P(i), b = P(j), p = f & 3, st = pairState(tag[i], q, tag[j], p), bind = !st && isBind(tag[i], q, tag[j], p);
      if (bind && !showBinds) continue;
      const A = tipAt(SHAPES[tag[i]], q, a[0], a[1], a[4] ?? 0, GRAD * sc[i], a[5] ?? 1), B = tipAt(SHAPES[tag[j]], p, b[0], b[1], b[4] ?? 0, GRAD * sc[j], b[5] ?? 1), c = Math.min(0.4 * Math.hypot(B[0] - A[0], B[1] - A[1]), 14);
      let path = st === 2 ? red : bind ? binds : plain;
      if (st === 1) { const colour = FAMILY[ruleFor(tag[i], tag[j])[1]][1]; path = live.get(colour); if (!path) live.set(colour, path = new Path2D()); }
      for (const pth of i === hi || j === hi ? [path, mine] : [path]) { pth.moveTo(A[0], A[1]); pth.bezierCurveTo(A[0] + A[2] * c, A[1] + A[3] * c, B[0] + B[2] * c, B[1] + B[3] * c, B[0], B[1]); }
    }
    ctx.globalAlpha = fade;
    ctx.strokeStyle = "#6d5aa8"; ctx.lineWidth = lw(0.9); ctx.stroke(binds);
    ctx.strokeStyle = "#4a5d7c"; ctx.lineWidth = lw(1.3); ctx.stroke(plain);
    ctx.lineWidth = lw(3.2); for (const [colour, path] of live) { ctx.strokeStyle = colour; ctx.stroke(path); }
    ctx.setLineDash([lw(4), lw(4)]); ctx.strokeStyle = "#ff6b6b"; ctx.lineWidth = lw(2.4); ctx.stroke(red); ctx.setLineDash([]);
    ctx.globalAlpha = 0.5 * fade; ctx.strokeStyle = "#ffffff"; ctx.lineWidth = lw(1.4); ctx.stroke(mine); ctx.globalAlpha = fade;
    // agents, each in its kind's icon, turned and flipped as the layout has it; a copier in the colour of
    // its copy; then the letters, upright; then a dot on the tip of each main port
    const byColour = new Map(), letters = new Map(), dots = new Path2D(), liveDots = new Path2D();
    for (let i = 0; i < n; i++) {
      const t = tag[i]; if (t === K.pod || !node(i)) continue;
      const p = P(i), th = p[4] ?? 0, fl = p[5] ?? 1, sh = SHAPES[t], rad = GRAD * sc[i];
      const c = colourOf(t, op[i]); let path = byColour.get(c); if (!path) byColour.set(c, path = new Path2D());
      shapeInto(path, sh, p[0], p[1], rad, th, fl, holes);
      if (!big) continue;
      if (sh.glyph) { const key = `${sh.glyph.color ?? c}|${(sh.glyph.size * rad).toFixed(1)}`; let l = letters.get(key); if (!l) letters.set(key, l = []); l.push(sh.glyph.text, p[0], p[1] + (sh.glyph.dy ?? 0) * rad); }
      const m = tipAt(sh, 0, p[0], p[1], th, rad, fl), f = far[3 * i], ds = f >= 0 && pairState(t, 0, tag[f >> 2], f & 3) === 1 ? liveDots : dots;
      ds.moveTo(m[0] + rad * 0.24, m[1]); ds.arc(m[0], m[1], rad * 0.24, 0, 7);
    }
    for (const [c, path] of byColour) { ctx.fillStyle = c; ctx.fill(path); }
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    for (const [key, l] of letters) {
      const [c, size] = key.split("|");
      ctx.fillStyle = c; ctx.font = `700 ${size}px ${ICN.FONT}`;
      for (let k = 0; k < l.length; k += 3) ctx.fillText(l[k], l[k + 1], l[k + 2]);
    }
    // pods: a named part folded shut, as its name on a pill
    for (const [g, pod] of NET.pods) {
      const i = NET.index.get(-1 - g); if (!node(i)) continue;
      const p = P(i), [hw, hh] = podBox(pod.name), hue = hueOf(pod.name), pinPath = new Path2D();
      shapeInto(pinPath, SHAPES[K.pod], p[0], p[1], GRAD, 0, 1, false);
      ctx.fillStyle = `hsl(${hue} 45% 55%)`; ctx.fill(pinPath);
      ctx.beginPath(); ctx.roundRect(p[0] - hw, p[1] - hh, 2 * hw, 2 * hh, hh);
      ctx.fillStyle = `hsl(${hue} 40% 24%)`; ctx.fill(); ctx.strokeStyle = `hsl(${hue} 45% 55%)`; ctx.lineWidth = lw(1.2); ctx.stroke();
      if (big) { ctx.fillStyle = "#e6edf3"; ctx.font = `600 ${GRAD * 1.15}px ui-monospace, monospace`; ctx.fillText(pod.name, p[0], p[1] + GRAD * 0.06); }
      const m = tipAt(SHAPES[K.pod], 0, p[0], p[1], 0, GRAD, 1); dots.moveTo(m[0] + GRAD * 0.24, m[1]); dots.arc(m[0], m[1], GRAD * 0.24, 0, 7);
    }
    ctx.fillStyle = "#ffffff"; ctx.fill(dots); ctx.fillStyle = "#ffd866"; ctx.fill(liveDots);
    ctx.globalAlpha = 1;
  };
  if (lit) { paint(i => !lit.has(i), (i, j) => !(lit.has(i) && lit.has(j)), 0.3); paint(i => lit.has(i), (i, j) => lit.has(i) && lit.has(j), 1); }
  else paint(() => true, () => true, 1);
  // a rewrite under way: the pair it used up shrinking into the place they met, and a ring spreading from it
  ANIM.ghosts = ANIM.ghosts.filter(g => now - g.t0 < g.dur); ANIM.sparks = ANIM.sparks.filter(g => now - g.t0 < g.dur);
  for (const g of ANIM.ghosts) {
    const p = (now - g.t0) / g.dur, e = p * p, path = new Path2D();
    shapeInto(path, SHAPES[g.t], g.x + (g.mx - g.x) * e, g.y + (g.my - g.y) * e, GRAD * (1 - 0.65 * p), g.th, g.fl, false);
    ctx.globalAlpha = 1 - p; ctx.fillStyle = g.c; ctx.fill(path);
  }
  for (const g of ANIM.sparks) {
    const p = (now - g.t0) / g.dur;
    ctx.globalAlpha = (1 - p) * 0.9; ctx.strokeStyle = g.c; ctx.lineWidth = lw(2.2 * (1 - p) + 0.6);
    ctx.beginPath(); ctx.arc(g.x, g.y, GRAD * (0.4 + 2.4 * p), 0, 7); ctx.stroke();
  }
  ctx.globalAlpha = 1;
  // the name of each part drawn open, on a small tag over its band's top left; × folds one opened by hand
  for (const t of TAGS) {
    const text = t.name + (t.byHand ? "  ×" : ""), fs = GRAD * 1.15, pad = GRAD * 0.45;
    ctx.font = `600 ${fs}px ui-monospace, monospace`; ctx.textBaseline = "alphabetic"; ctx.textAlign = "left";
    const wd = ctx.measureText(text).width;
    ctx.beginPath(); ctx.roundRect(t.x - pad, t.y - fs * 0.95, wd + 2 * pad, fs * 1.35, fs * 0.4);
    ctx.fillStyle = `hsl(${t.hue} 42% 14%)`; ctx.fill(); ctx.strokeStyle = `hsl(${t.hue} 45% 45%)`; ctx.lineWidth = lw(1); ctx.stroke();
    ctx.fillStyle = `hsl(${t.hue} 60% 78%)`; ctx.fillText(text, t.x, t.y);
    t.x -= pad; t.y -= fs * 0.95; t.w = wd + 2 * pad; t.h = fs * 1.35;
  }
  // What each structure is, over its top left: the name of the part it comes from, or the start of its
  // term. The larger structures first; a label that would sit on another is left out.
  if (view.s * GRAD > 2.2) {
    const fs = Math.max(GRAD * 1.02, 10.5 / view.s), taken = TAGS.map(t => [t.x, t.y, t.x + t.w, t.y + t.h]);
    ctx.font = `500 ${fs}px ui-monospace, monospace`; ctx.textBaseline = "alphabetic"; ctx.textAlign = "left";
    for (const c of [...NET.clusters].sort((p, q) => q.nodes.length - p.nodes.length)) {
      if (!c.label || NET.marks.get(c.top) === c.label) continue;
      let x0 = Infinity, y0 = Infinity;
      for (const i of c.nodes) { const p = P(i), rad = radOf(i); x0 = Math.min(x0, p[0] - rad); y0 = Math.min(y0, p[1] - rad); }
      const x = x0 + GRAD * 0.6, y = y0 - GRAD * 0.3, box = [x - 2, y - fs, x + ctx.measureText(c.label).width + 2, y + fs * 0.35];
      if (taken.some(o => box[0] < o[2] && box[2] > o[0] && box[1] < o[3] && box[3] > o[1])) continue;
      taken.push(box);
      ctx.fillStyle = "rgba(11,14,20,.8)"; ctx.fillRect(box[0], box[1], box[2] - box[0], box[3] - box[1]);
      ctx.fillStyle = c.hue >= 0 ? `hsl(${c.hue} 55% 74%)` : "#8fa3bf"; ctx.fillText(c.label, x, y);
    }
  }
  // Which number each number is, on a pill beside its top; and each bit that is drawn open, as a digit.
  if (NET.marks.size && view.s * GRAD > 2.2) {
    const fs = Math.max(GRAD * 1.3, 12 / view.s), small = GRAD * 1.25, pad = fs * 0.4;
    ctx.textBaseline = "middle"; ctx.textAlign = "left";
    for (const [i, text] of NET.marks) {
      const p = P(i);
      if (isBitMark(text)) {
        if (view.s * GRAD < 4.5) continue;
        ctx.font = `700 ${small}px ui-monospace, monospace`;
        ctx.fillStyle = "rgba(11,14,20,.85)"; ctx.beginPath(); ctx.arc(p[0] + GRAD * 1.7, p[1] - GRAD * 1.1, small * 0.62, 0, 7); ctx.fill();
        ctx.fillStyle = text === "b1" ? "#9be9a8" : "#7d93b5"; ctx.textAlign = "center"; ctx.fillText(text[1], p[0] + GRAD * 1.7, p[1] - GRAD * 1.05); ctx.textAlign = "left";
        continue;
      }
      ctx.font = `700 ${fs}px ui-monospace, monospace`;
      const wd = ctx.measureText(text).width, x = p[0] - GRAD * 1.9 - wd, y = p[1] - GRAD * 0.2;
      ctx.beginPath(); ctx.roundRect(x - pad, y - fs * 0.72, wd + 2 * pad, fs * 1.44, fs * 0.72);
      ctx.fillStyle = "hsl(152 45% 13%)"; ctx.fill(); ctx.strokeStyle = "hsl(152 55% 48%)"; ctx.lineWidth = lw(1.3); ctx.stroke();
      ctx.fillStyle = "#b9f3c9"; ctx.fillText(text, x, y + fs * 0.04);
    }
  }
  ctx.textBaseline = "middle";
  // the pair a click would fire, ringed gold; the node pointed at, white
  if (hoverPair && !HELD) for (const a of hoverPair) { const i = NET.index.get(a); if (i === undefined) continue; const p = P(i); ctx.strokeStyle = "#ffd866"; ctx.lineWidth = lw(2); ctx.beginPath(); ctx.arc(p[0], p[1], GRAD * 2, 0, 7); ctx.stroke(); }
  else if (hi !== undefined && tag[hi] !== K.pod) { const p = P(hi); ctx.strokeStyle = "#ffffff"; ctx.lineWidth = lw(1.5); ctx.beginPath(); ctx.arc(p[0], p[1], GRAD * 2, 0, 7); ctx.stroke(); }
  drawStrip();
}
/// The run: one column a round (or a rewrite, or a few when there are more than pixels), its height how
/// many rewrites fired at once, its colour what most of them did; faint where the run has not got to
/// yet; a white cap where a pair was fired by hand.
function drawStrip() {
  const d = devicePixelRatio || 1, r = sv.getBoundingClientRect(), W = Math.round(r.width * d), H = Math.round(r.height * d);
  if (sv.width !== W || sv.height !== H) { sv.width = W; sv.height = H; }
  sctx.fillStyle = "#0b0e14"; sctx.fillRect(0, 0, W, H);
  const R = RUN, len = tlLength(), rounds = byRound();
  if (!len) return;
  const per = Math.max(1, len / W), cols = Math.ceil(len / per), cw = W / cols, bw = Math.max(cw - (cw > 3 ? 1 : 0), 1);
  const from = c => rounds ? R.rounds[Math.floor(c * per)] : Math.floor(c * per), upto = c => { const u = Math.min(len, Math.floor((c + 1) * per) || 1); return rounds ? R.rounds[u] : u; };
  let top = 1;
  if (rounds) for (let k = 0; k + 1 < R.rounds.length; k++) top = Math.max(top, R.rounds[k + 1] - R.rounds[k]);
  for (let c = 0; c < cols; c++) {
    const a = from(c), b = Math.max(a + 1, upto(c)), tally = new Int32Array(FAMILY.length);
    let hand = false;
    for (let e = a; e < b && e < R.n; e++) { tally[R.fam[e]]++; if (R.hand[e]) hand = true; }
    let widest = 1;
    if (rounds) { widest = 0; for (let k = Math.floor(c * per); k < Math.min(len, Math.floor((c + 1) * per) || 1); k++) widest = Math.max(widest, R.rounds[k + 1] - R.rounds[k]); }
    const h = (rounds ? (H - 8 * d) * widest / top : H - 8 * d);
    sctx.fillStyle = FAMILY[tally.indexOf(Math.max(...tally))][1]; sctx.globalAlpha = a < R.at ? 0.85 : 0.22;
    sctx.fillRect(c * cw, H - h, bw, h); sctx.globalAlpha = 1;
    if (hand) { sctx.fillStyle = "#ffffff"; sctx.fillRect(c * cw, H - h - 4 * d, bw, 3 * d); }
  }
  const x = tlPos() / len * W;
  sctx.fillStyle = "#ffffff"; sctx.fillRect(Math.min(W - 2 * d, Math.max(0, x - d)), 0, 2 * d, H);
  if (hover?.event !== undefined && !rounds) { const c = Math.floor(hover.event / per); sctx.strokeStyle = "#ffffff"; sctx.lineWidth = d; sctx.strokeRect(c * cw + 0.5, 0.5, Math.max(cw, 2) - 1, H - 1); }
}

/// Whether node i sits on a variable's own wire, between a binder and its uses.
const onBinder = i => { const t = NET.tag[i], f = NET.far[3 * i]; return t === K.sig || t === K.dupO || t === K.eraO || (t === K.eraI && f >= 0 && bindPort(NET.tag[f >> 2], f & 3)); };
/// The function whose variable's wire node i sits on: the node, or -1.
function binderOf(i) {
  for (let hops = 0; hops < 256; hops++) {
    const t = NET.tag[i];
    if (t === K.lam || t === K.Lam) return i;
    if (!onBinder(i) || t === K.eraO) return -1;
    const f = NET.far[3 * i + (t === K.dupO ? 1 : 0)]; if (f < 0) return -1;
    i = f >> 2;
  }
  return -1;
}
/// The subnet under a node: the node and all its value is made of. An input is followed down, and a
/// function takes in what sits on its own variable's wire; a use of a variable is not followed up to
/// its binder. A node on a variable's wire stands for the function that binds it.
function subnetOf(i0) {
  const { tag, far } = NET, seen = new Set(), todo = [];
  const add = i => { if (!seen.has(i)) { seen.add(i); todo.push(i); } };
  add(onBinder(i0) && binderOf(i0) >= 0 ? binderOf(i0) : i0);
  while (todo.length) {
    const a = todo.pop(), t = tag[a];
    if (t === K.dupO || t === K.eraO) continue;
    for (let q = 0; q <= ARITY[t]; q++) {
      const f = far[3 * a + q]; if (f < 0) continue;
      const b = f >> 2, tb = tag[b];
      if ((t === K.lam || t === K.Lam) && q === 1) { if (onBinder(b)) add(b); }
      else if (t === K.sig) { if (q && tb === K.sig) add(b); }
      else if (POL[t][q] === "i" && !bindPort(tb, f & 3)) add(b);
    }
  }
  return seen;
}
/// Where to read node i's term, as a port of the engine's net, and what that term is to the node.
function termOf(i) {
  const N = RUN.net, t = NET.tag[i];
  if (t === K.pod) return [NET.pods.get(-1 - NET.id[i]).top * 4, "What is folded in it"];
  const a = NET.id[i];
  if (CODE.has(t)) return [a * 4, "As a term"];
  if (t === K.O) return [a * 4 + 2, "The application it is running"];
  if (t === K.T) return [a * 4 + 1, "What it is thawing"];
  if (t === K.R) return [a * 4 + 1, "What it is reading"];
  if (t === K.root) return [N.mate(a * 4), "The whole net"];
  const f = N.mate(a * 4);
  if ((t === K.dupI || t === K.eraI) && f >= 0 && !bindPort(N.kind[f >> 2], f & 3)) return [f, t === K.dupI ? "What it has still to copy" : "What it is erasing"];
  const b = binderOf(i);
  return b >= 0 ? [NET.id[b] * 4, "The function whose variable this wire is"] : [-1, ""];
}
const esc = text => text.replace(/&/g, "&amp;").replace(/</g, "&lt;");
function termHTML(i) {
  const [port, what] = termOf(i);
  if (port < 0) return "";
  const text = clip(show(RUN.net.read(port, { budget: 4000, short: NET.tops })), 900);
  return `<div class="dim" style="margin-top:5px">${what} <span class="dim">(${subnetOf(i).size} shown)</span>:</div><div class="term">${esc(text)}</div>`;
}
const cap = t => /^[a-z]/.test(t) ? t[0].toUpperCase() + t.slice(1) : t;
const ruleLink = (kx, ky, text, act) => `<a data-rule="${kx},${ky}"${act ? ' class="act"' : ""}>${text}</a>`;
function agentHTML(i) {
  const t = NET.tag[i], kind = KINDS[t], c = colourOf(t, NET.op[i]);
  if (t === K.pod) {
    const pod = NET.pods.get(-1 - NET.id[i]);
    return `<div><b>${pod.name}</b> <span class="dim">(a named part, folded: ${pod.cover.length} agents)</span></div><div class="dim">All of it is code nothing is acting on. Click it to open it; it opens by itself when something reaches it.</div>${termHTML(i)}`;
  }
  let meets = 0;
  const ports = PORTS[kind].map((name, q) => {
    const f = NET.far[3 * i + q]; if (f < 0) return "";
    const j = f >> 2, tj = NET.tag[j], kj = KINDS[tj], st = pairState(t, q, tj, f & 3);
    if (st === 1) meets = tj;
    const what = st === 1 ? `<div style="margin-left:2.1em">${ruleLink(t, tj, "about to fire (click the wire): " + MEANING[ruleFor(t, tj)[2]], true)}</div>`
      : st === 2 ? `<div style="margin-left:2.1em;color:var(--warn)">stuck: there is no rule for this pair</div>` : "";
    const far = tj === K.pod ? `the part ${NET.pods.get(-1 - NET.id[j]).name}` : SHORT[kj] ?? "the root";
    return `<div><span class="pm">${SIDE[kind][q]}${q ? "" : "●"}</span><span class="dim">${name} →</span> ${far}</div>${what}`;
  }).join("");
  const partners = [];
  for (let p = 1; p <= K.eraO; p++) if (ruleFor(t, p)) partners.push(ruleLink(t, p, NAMES[KINDS[p]], p === meets));
  const rules = partners.length ? `<div style="margin-top:4px"><span class="dim">Its main port rewrites with:</span> ${partners.join(", ")}</div>` : "";
  const g = RUN.net.grp.get(NET.id[i]), part = g !== undefined ? `<div class="dim">part of ${RUN.net.parts[g].name}</div>` : "";
  const extra = NET.op[i] >= 0 ? `, copy ${NET.op[i]}` : "";
  return `<div>${iconSVG(SHAPE[kind], 22, c)}<b>${NAMES[kind]}${extra}</b> <span class="dim">(agent ${NET.id[i]})</span></div><div class="dim">${does(kind)}</div>${part}${termHTML(i)}${ports}${rules}`;
}
/// The box is only rewritten when what it shows changes, so its links stay put to be clicked.
let detailKey = null, runGen = 0;
function showDetail() {
  const i = hoverIndex();
  if (hover?.id !== undefined && i === undefined) { hover = null; hoverPair = null; }
  const key = hover?.event !== undefined ? `e${hover.event}:${runGen}:${RUN.n}` : i !== undefined ? `a${hover.id}:${RUN.at}:${runGen}:${RUN.n}` : "";
  if (key === detailKey) return;
  detailKey = key;
  const e = hover?.event;
  $("detail").innerHTML = e !== undefined && e < RUN.n
    ? `<div><span class="sw" style="background:${FAMILY[RUN.fam[e]][1]}"></span><b>rewrite ${e + 1}</b>${RUN.hand[e] ? " (fired by hand)" : ""}</div><div>${eventText(e)}</div>`
    : i !== undefined ? agentHTML(i) : "point at an agent, or at the bar";
}
/// A rule as a picture: the pair before, and what replaces it, wired to the same side ports. It is made
/// by firing the rule in a net of just the two agents, so it shows what the engine does.
function ruleDiagram(lead, other) {
  const N = new RLN.Net(), W = 150, H = 150, R = 7, mid = W / 2, GAP = 30, clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
  const x = N.make(lead), y = N.make(other);
  for (const a of [x, y]) if (N.kind[a] === K.dupI || N.kind[a] === K.dupO) N.op.set(a, 0);
  N.link(x * 4, y * 4);
  const pos = new Map(), rot = new Map(), label = new Map(), stubs = new Set(), shapeOf = a => SHAPES[N.kind[a]];
  // The pair, turned so that their main pins face each other, one above the other; each side port
  // leads to a stub where its pin points.
  rot.set(x, [Math.PI / 2 - shapeOf(x).ports[0][4], 1]); rot.set(y, [-Math.PI / 2 - shapeOf(y).ports[0][4], 1]);
  pos.set(x, [0, 0]);
  { const a = tipAt(shapeOf(x), 0, 0, 0, rot.get(x)[0], R), b = tipAt(shapeOf(y), 0, 0, 0, rot.get(y)[0], R); pos.set(y, [a[0] - b[0], a[1] + 12 - b[1]]); }
  for (const a of [x, y]) {
    const k = N.kind[a], c = pos.get(a);
    for (let i = 1; i <= ARITY[k]; i++) {
      const [tx, ty, dx, dy] = tipAt(shapeOf(a), i, c[0], c[1], rot.get(a)[0], R), st = N.make(K.root);
      N.link(st * 4, a * 4 + i); stubs.add(st);
      pos.set(st, [tx + dx * 20, ty + dy * 20]); label.set(st, [PORT_SHORT[KINDS[k]][i], dx, dy]);
    }
  }
  { // centre the whole of it in the panel
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (const [, p] of pos) { x0 = Math.min(x0, p[0]); y0 = Math.min(y0, p[1]); x1 = Math.max(x1, p[0]); y1 = Math.max(y1, p[1]); }
    const sx = mid - (x0 + x1) / 2, sy = H / 2 - (y0 + y1) / 2;
    for (const [k, p] of pos) pos.set(k, [clamp(p[0] + sx, 12, W - 12), clamp(p[1] + sy, 14, H - 14)]);
  }
  const tip = (key, a, q) => key === "view" || stubs.has(a) ? [...pos.get(key), 0, 0] : tipAt(shapeOf(a), q, ...pos.get(a), rot.get(a)[0], R, rot.get(a)[1]);
  /// One side of the picture: the agents in `nodes`, and those in `group` drawn as one box.
  const panel = (nodes, group, dx) => {
    const keyOf = a => group?.has(a) ? "view" : a;
    const wires = new SvgPath(), gold = new SvgPath(), seen = new Set();
    for (const a of [...nodes, ...(group ?? []), ...stubs]) for (let q = 0; q <= ARITY[N.kind[a]]; q++) {
      const f = N.far[3 * a + q]; if (f < 0) continue;
      const b = f >> 2, ka = keyOf(a), kb = keyOf(b);
      if (ka === kb) continue;
      const wire = Math.min(a * 4 + q, f); if (seen.has(wire)) continue; seen.add(wire);
      const A = tip(ka, a, q), B = tip(kb, b, f & 3), c = Math.min(0.4 * Math.hypot(B[0] - A[0], B[1] - A[1]), 14);
      const active = !q && !(f & 3) && !stubs.has(a) && !stubs.has(b) && ka !== "view" && kb !== "view" && ruleFor(N.kind[a], N.kind[b]);
      const w = active ? gold : wires; w.moveTo(A[0], A[1]); w.bezierCurveTo(A[0] + A[2] * c, A[1] + A[3] * c, B[0] + B[2] * c, B[1] + B[3] * c, B[0], B[1]);
    }
    let shapes = "";
    for (const a of nodes) { const p = pos.get(a), [th, fl] = rot.get(a); shapes += svgAt(shapeOf(a), p[0], p[1], R, th, fl, N.op.has(a) ? copyColor(N.op.get(a)) : shapeOf(a).color); }
    if (group) { const p = pos.get("view"); shapes += `<rect x="${(p[0] - 19).toFixed(1)}" y="${(p[1] - 9).toFixed(1)}" width="38" height="18" rx="5" fill="#1b2436" stroke="#56c8ff"/><text x="${p[0].toFixed(1)}" y="${(p[1] + 3).toFixed(1)}" text-anchor="middle" fill="#c8d3e0" font-size="9">view</text>`; }
    for (const st of stubs) {
      const p = pos.get(st), [text, ux, uy] = label.get(st);
      shapes += `<circle cx="${p[0].toFixed(1)}" cy="${p[1].toFixed(1)}" r="2.2" fill="#0b0e14" stroke="#8595ad"/><text x="${clamp(p[0] + ux * 9, 18, W - 18).toFixed(1)}" y="${(p[1] + uy * 9 + 3).toFixed(1)}" text-anchor="middle" fill="#8595ad" font-size="8">${text}</text>`;
    }
    return `<g transform="translate(${dx} 0)"><path d="${wires.d}" stroke="#4d6080" stroke-width="1.2" fill="none"/><path d="${gold.d}" stroke="#ffd866" stroke-width="2.6" fill="none" stroke-linecap="round"/>${shapes}</g>`;
  };
  const before = panel([x, y], null, 0);
  N.fire(x, y);
  const made = []; for (let n = 0; n < N.live; n++) if (!stubs.has(N.ids[n])) made.push(N.ids[n]);
  made.sort((a, b) => a - b);
  let nodes = made, group = null;
  if (N.last.rule.startsWith("view")) {
    nodes = N.last.rule === "viewFun" || N.last.rule === "viewOpened" ? [made[0]] : [];
    group = new Set(made.filter(a => !nodes.includes(a)));
  }
  // what was made sits between what it is wired to, each kept apart from the others and clear of the
  // stubs, and turns and flips to face it
  const keys = [...nodes, ...(group ? ["view"] : [])], keyOf = a => group?.has(a) ? "view" : a, nbrs = new Map(keys.map(k => [k, new Set()]));
  for (const a of made) for (let q = 0; q <= ARITY[N.kind[a]]; q++) { const f = N.far[3 * a + q]; if (f >= 0 && keyOf(f >> 2) !== keyOf(a)) nbrs.get(keyOf(a)).add(keyOf(f >> 2)); }
  keys.forEach((k, i) => pos.set(k, [mid + (i - (keys.length - 1) / 2) * 6, H / 2 + (i % 2) * 4]));
  for (let it = 0; it < 150; it++) {
    for (const k of keys) { let sx = 0, sy = 0, c = 0; for (const m of nbrs.get(k)) { const p = pos.get(m); sx += p[0]; sy += p[1]; c++; } if (c) { const p = pos.get(k); pos.set(k, [p[0] + (sx / c - p[0]) * 0.3, p[1] + (sy / c - p[1]) * 0.3]); } }
    for (const k of keys) for (const m of keys) { if (k === m) continue; const p = pos.get(k), q = pos.get(m), ex = p[0] - q[0], ey = p[1] - q[1], d = Math.hypot(ex, ey) || 0.01; if (d < GAP) { const f = (GAP - d) / d * 0.25; pos.set(k, [p[0] + ex * f, p[1] + ey * f]); } }
    for (const k of keys) for (const st of stubs) { const p = pos.get(k), q = pos.get(st), ex = p[0] - q[0], ey = p[1] - q[1], d = Math.hypot(ex, ey) || 0.01; if (d < 20) { const f = (20 - d) / d * 0.5, cx = d < 0.5 ? mid - q[0] : ex, cy = d < 0.5 ? H / 2 - q[1] : ey; pos.set(k, [p[0] + cx * f, p[1] + cy * f]); } }
    for (const k of keys) { const p = pos.get(k); pos.set(k, [clamp(p[0], 18, W - 18), clamp(p[1], 26, H - 26)]); }
  }
  for (const a of nodes) {
    const p = pos.get(a), sh = shapeOf(a), dirs = sh.ports.map((_, q) => { const f = N.far[3 * a + q]; if (f < 0) return undefined; const o = pos.get(keyOf(f >> 2)); return Math.atan2(o[1] - p[1], o[0] - p[0]); });
    const fit = bestFit(sh, dirs, [1, 1, 1], 1);
    rot.set(a, [fit.any ? fit.th : 0, fit.flip]);
  }
  const after = panel(nodes, group, W + 22);
  return `<svg viewBox="0 0 ${2 * W + 22} ${H}" xmlns="http://www.w3.org/2000/svg" font-family="ui-monospace, monospace"><text x="${W + 11}" y="${H / 2 + 4}" text-anchor="middle" fill="#8595ad" font-size="14">→</text>${before}${after}</svg>`;
}
/// Put a rule in the rule box: what meets what, what happens, and its picture.
let ruleKey = "";
function showRule(kx, ky) {
  const r = ruleFor(kx, ky); if (!r) return;
  const [lead, other] = r[0] ? [ky, kx] : [kx, ky], key = `${lead},${other}`;
  if (key === ruleKey) return;
  ruleKey = key;
  const more = r[2].startsWith("view") ? "The view is a function of five arms, <code>λk1 k2 k3 k4 k5. …</code>: its body applies the arm for this kind of value to the other four arms, then to the parts."
    : r[2] === "copy" ? "The same for every kind of agent a copy walks through: a copier goes to each side port, facing the way that port faces."
    : r[2] === "erase" && ARITY[other] ? "The same for every kind of agent: an eraser goes to each side port." : "";
  const what = r[2] === "erase" && !ARITY[other] ? (other === K.eraI ? "the two erasers have met, and both are gone" : "it has no side ports, so both are gone") : MEANING[r[2]];
  $("ruleBox").innerHTML = `<div><span class="sw" style="background:${FAMILY[r[1]][1]}"></span><b>${cap(SHORT[KINDS[lead]])} meets ${SHORT[KINDS[other]]}</b></div><div>${cap(what)}.</div>`
    + ruleDiagram(lead, other) + (more ? `<div class="dim">${more}</div>` : "");
}

// ---- input -----------------------------------------------------------------------------------------
function worldAt(cx, cy) { const r = cv.getBoundingClientRect(); return [(cx - r.left - view.ox) / view.s, (cy - r.top - view.oy) / view.s]; }
function pointedAt(cx, cy) {
  const [wx, wy] = worldAt(cx, cy), reach = Math.max(GRAD * 1.4, 7 / view.s) ** 2;
  let best = null, bd = reach, pod = null;
  for (let i = 0; i < NET.n; i++) {
    const p = POS.get(NET.id[i]);
    if (NET.tag[i] === K.pod) { const [hw, hh] = podBox(NET.pods.get(-1 - NET.id[i]).name); if (Math.abs(wx - p[0]) <= hw && Math.abs(wy - p[1]) <= hh) pod = { agent: i }; continue; }
    const dd = (p[0] - wx) ** 2 + (p[1] - wy) ** 2; if (dd < bd) { bd = dd; best = { agent: i }; }
  }
  return best ?? pod;   // an agent over a pod is the one pointed at
}
/// The name tag under the pointer, if any.
function tagAt(cx, cy) { const [wx, wy] = worldAt(cx, cy); return TAGS.find(t => wx >= t.x && wx <= t.x + t.w && wy >= t.y && wy <= t.y + t.h) ?? null; }
/// The pair a node's main port is in, if it is one that can fire.
function pairOf(i) {
  const f = NET.far[3 * i]; if (f < 0 || (f & 3)) return null;
  return pairState(NET.tag[i], 0, NET.tag[f >> 2], 0) === 1 ? [i, f >> 2] : null;
}
const tipOf = (i, q) => { const p = POS.get(NET.id[i]); return tipAt(SHAPES[NET.tag[i]], q, p[0], p[1], p[4] ?? 0, GRAD, p[5] ?? 1); };
/// The pair a click here would fire: that of the agent pointed at, else the nearest wire of a pair that can fire.
function pairAt(cx, cy) {
  const at = pointedAt(cx, cy), own = at && pairOf(at.agent);
  if (own) return own;
  const [wx, wy] = worldAt(cx, cy);
  let best = null, bd = Math.max(4, 7 / view.s);
  for (let i = 0; i < NET.n; i++) {
    const pr = pairOf(i); if (!pr || pr[1] < i) continue;
    const a = tipOf(i, 0), b = tipOf(pr[1], 0), dx = b[0] - a[0], dy = b[1] - a[1];
    const t = Math.max(0, Math.min(1, ((wx - a[0]) * dx + (wy - a[1]) * dy) / (dx * dx + dy * dy || 1))), dd = Math.hypot(wx - a[0] - t * dx, wy - a[1] - t * dy);
    if (dd < bd) { bd = dd; best = pr; }
  }
  return best;
}
/// Fire the pair now, whatever was planned next: the rest of the run is planned again from there.
function fireByHand(i, j) {
  const a = NET.id[i], b = NET.id[j], kx = NET.tag[i], ky = NET.tag[j], t = RUN.at;
  if (physical()) { if (!RUN.fireLive(a, b, true)) return; }
  else { setMode(0); if (!RUN.fireNow(a, b)) return; }
  animate(t, t + 1); dirty = true; hoverPair = null;
  showRule(kx, ky);
}
const setFollow = on => { $("follow").checked = on; };
/// A press on an agent drags that agent, and the layout gives way around it; a press on empty space
/// drags the view. A press let go where it began is a click: on a name tag it folds the part, on a pod
/// it opens it, and otherwise it fires the pair there, if there is one.
let drag = null;
cv.addEventListener("pointerdown", ev => {
  const at = pointedAt(ev.clientX, ev.clientY);
  drag = { x: ev.clientX, y: ev.clientY, ox: view.ox, oy: view.oy, moved: false, node: at ? NET.id[at.agent] : undefined };
  try { cv.setPointerCapture(ev.pointerId); } catch (e) {}
});
cv.addEventListener("pointermove", ev => {
  if (drag) {
    const dx = ev.clientX - drag.x, dy = ev.clientY - drag.y;
    if (Math.abs(dx) + Math.abs(dy) > 3) drag.moved = true;
    if (!drag.moved) return;
    cv.style.cursor = "grabbing";
    if (drag.node !== undefined && NET.index.has(drag.node)) { const [wx, wy] = worldAt(ev.clientX, ev.clientY); HELD = { id: drag.node, x: wx, y: wy }; alpha = Math.max(alpha, 0.3); }
    else { view.ox = drag.ox + dx; view.oy = drag.oy + dy; setFollow(false); }
    return;
  }
  const tag = tagAt(ev.clientX, ev.clientY), at = pointedAt(ev.clientX, ev.clientY), pr = tag ? null : pairAt(ev.clientX, ev.clientY);
  hover = at ? { id: NET.id[at.agent] } : pr ? { id: NET.id[pr[0]] } : null;
  hoverPair = pr ? [NET.id[pr[0]], NET.id[pr[1]]] : null;
  cv.style.cursor = pr || tag?.byHand || (at && NET.tag[at.agent] === K.pod) ? "pointer" : "grab";
  if (pr) showRule(NET.tag[pr[0]], NET.tag[pr[1]]);
  showDetail();
});
cv.addEventListener("pointerup", ev => {
  const was = drag; drag = null; HELD = null; cv.style.cursor = "grab";
  if (!was || was.moved) return;
  const tag = tagAt(ev.clientX, ev.clientY), at = pointedAt(ev.clientX, ev.clientY);
  if (tag?.byHand) { OPEN.delete(tag.g); dirty = true; return; }
  if (at && NET.tag[at.agent] === K.pod) { OPEN.add(-1 - NET.id[at.agent]); dirty = true; alpha = Math.max(alpha, 0.5); return; }
  const pr = pairAt(ev.clientX, ev.clientY);
  if (pr) fireByHand(pr[0], pr[1]);
});
cv.addEventListener("pointercancel", () => { drag = null; HELD = null; });
cv.addEventListener("wheel", ev => {
  ev.preventDefault(); const r = cv.getBoundingClientRect(), k = Math.exp(-ev.deltaY * 0.0015), mx = ev.clientX - r.left, my = ev.clientY - r.top;
  view.ox = mx - (mx - view.ox) * k; view.oy = my - (my - view.oy) * k; view.s *= k; setFollow(false);
}, { passive: false });
for (const box of [$("detail"), $("ruleBox")]) box.addEventListener("click", ev => {
  const a = ev.target.closest("a[data-rule]"); if (!a) return;
  const [kx, ky] = a.dataset.rule.split(",").map(Number); showRule(kx, ky);
});
/// The rule of the rewrite just made, in the rule box.
function lastRule() { if (RUN.at) { const k = RUN.kinds[RUN.at - 1]; showRule(k >> 4, k & 15); } }
$("rule").onclick = () => { lastRule(); $("side").scrollTop = 0; };
/// Dragging along the bar goes to that point of the run.
let scrubbing = false;
const scrubTo = ev => { const r = sv.getBoundingClientRect(); seek(Math.round(Math.min(1, Math.max(0, (ev.clientX - r.left) / r.width)) * tlLength())); };
sv.addEventListener("pointerdown", ev => { scrubbing = true; try { sv.setPointerCapture(ev.pointerId); } catch (e) {} setMode(0); scrubTo(ev); });
sv.addEventListener("pointerup", () => { scrubbing = false; });
sv.addEventListener("pointermove", ev => {
  if (scrubbing) { scrubTo(ev); return; }
  if (byRound()) return;
  const r = sv.getBoundingClientRect(), e = Math.min(RUN.n - 1, Math.floor((ev.clientX - r.left) / r.width * RUN.n));
  hover = e >= 0 ? { event: e } : null; hoverPair = null;
  if (e >= 0) showRule(RUN.kinds[e] >> 4, RUN.kinds[e] & 15);
  showDetail();
});
sv.addEventListener("pointerleave", () => { hover = null; showDetail(); });
$("stepFwd").onclick = () => { setMode(0); if (physical()) { sync(); touch(true); } else forward(1); if (!byRound()) lastRule(); };
$("stepBack").onclick = () => { setMode(0); back(1); if (!byRound()) lastRule(); };
$("playFwd").onclick = () => setMode(mode === 1 ? 0 : 1);
$("playBack").onclick = () => setMode(mode === -1 ? 0 : -1);
$("toEnd").onclick = () => { settle(); setMode(2); };
$("toStart").onclick = () => setMode(-2);
$("src").addEventListener("keydown", ev => { if (ev.key === "Enter") { load(); $("src").blur(); } });
$("examples").onchange = () => { if ($("examples").value) { $("src").value = $("examples").value; $("examples").value = ""; setFollow(true); load(); } };
$("order").onchange = () => load();
$("byRound").onchange = () => { dirty = true; };
$("physical").onchange = () => { if (!physical()) settle(); $("byRound").disabled = physical() || RUN.order !== "rounds"; alpha = Math.max(alpha, 0.4); dirty = true; };
$("subnets").onchange = () => { detailKey = null; };
$("fold").onchange = () => { dirty = true; alpha = Math.max(alpha, 0.5); };
$("speed").oninput = () => { $("speedLabel").textContent = (r => r >= 1 ? `${Math.round(r)}/frame` : `1/${Math.round(1 / r)} frames`)(rate()); };
document.addEventListener("keydown", ev => {
  if (ev.target === $("src") || ev.target.tagName === "SELECT") return;
  const k = ev.key;
  if (k === "ArrowRight") $("stepFwd").onclick(); else if (k === "ArrowLeft") $("stepBack").onclick();
  else if (k === "r" || k === "R") { if (!$("byRound").disabled) { $("byRound").checked = !$("byRound").checked; dirty = true; } }
  else if (k === "v" || k === "V") $("binds").checked = !$("binds").checked;
  else if (k === "l" || k === "L") { $("fold").checked = !$("fold").checked; $("fold").onchange(); }
  else if (k === "p" || k === "P") { $("physical").checked = !physical(); $("physical").onchange(); }
  else if (k === "s" || k === "S") { $("subnets").checked = !$("subnets").checked; }
  else if (k === " ") { ev.preventDefault(); setMode(ev.shiftKey ? (mode === -1 ? 0 : -1) : mode === 1 ? 0 : 1); }
  else if (k === "End") setMode(2); else if (k === "Home") setMode(-2); else if (k === "f" || k === "F") setFollow(!$("follow").checked);
  else return;
});
/// Shift held down lights up the subnet under the pointer, as the "subnets" box does.
let shift = false;
const subnets = () => shift || $("subnets").checked;
for (const type of ["keydown", "keyup"]) document.addEventListener(type, ev => { if (ev.key === "Shift") shift = type === "keydown"; });
addEventListener("blur", () => { shift = false; });
addEventListener("resize", () => { if ($("follow").checked) fit(); });

function frame() {
  if (RUN) {
    const pulling = physical() && mode === 1;
    if (mode === 2 && !forward(byRound() ? 40 : 4000)) setMode(0);
    else if (mode === -2 && !back(byRound() ? 40 : 4000)) setMode(0);
    else if ((mode === 1 && !pulling) || mode === -1) {
      credit += rate();
      const k = Math.floor(credit); credit -= k;
      if (k && !(mode === 1 ? forward(k) : back(k))) setMode(0);
    }
    sync();
    if (HELD) alpha = Math.max(alpha, 0.25);
    // the speed sets how hard pairs are drawn together, and past one step a frame, how many steps
    if (pulling) alpha = Math.max(alpha, 0.05 + 0.17 * Math.min(1, rate() * 2.5));
    relax(pulling ? Math.max(1, Math.min(12, Math.round(rate() * 2.5))) : NET.n > 3000 ? 1 : 3, pulling);
    if (pulling && !touch() && RUN.status !== "open" && RUN.at === RUN.n) setMode(0);
    if ($("follow").checked && !HELD) fit(true);
    draw();
    showDetail();
  }
  requestAnimationFrame(frame);
}

/// Load what the link says and play it if it asks.
function start() {
  const q = new URLSearchParams(location.hash.slice(1));
  $("src").value = q.get("src") ?? EXAMPLES[2][0];
  $("order").value = ["rounds", "newest", "random"].includes(q.get("order")) ? q.get("order") : "rounds";
  $("byRound").checked = q.get("rounds") !== "0";
  $("fold").checked = q.get("fold") !== "0";
  $("physical").checked = q.get("physical") === "1";
  $("subnets").checked = q.get("subnets") === "1";
  $("speed").value = q.get("speed") ?? 22;
  $("speed").oninput();
  setFollow(true);
  if (!load()) return;
  // open=1: the numbers the program writes start drawn open, not folded
  if (q.get("open") === "1") { RUN.net.parts.forEach((p, g) => { if (/^\d+[utscap]?$/.test(p.name)) OPEN.add(g); }); dirty = true; sync(); alpha = 1; relax(400); fit(); }
  if (q.has("t")) { RUN.seek(+q.get("t")); dirty = true; sync(); alpha = 1; relax(500); fit(); }
  if (q.get("play") === "1") setMode(1);
}
$("families").innerHTML = FAMILY.map(([n, c, d]) => `<span title="${d}"><span class="sw" style="background:${c}"></span>${n}</span>`).join("");
$("legend").innerHTML = ICN.ORDER.map(k => `<span title="${does(k).replace(/"/g, "&quot;")}">${iconSVG(SHAPE[k], 22)}${k === "dupI" ? "copier" : k === "eraI" ? "eraser" : NAMES[k]}</span>`).join("")
  + `<span><svg class="kind" width="34" height="18" viewBox="0 0 34 18"><rect x="1" y="2" width="32" height="14" rx="7" fill="hsl(150 40% 24%)" stroke="hsl(150 45% 55%)"/><text x="17" y="12.5" text-anchor="middle" font-size="9" fill="#e6edf3" font-family="ui-monospace, monospace">suc</text></svg>a named part, folded</span>`;
const attr = s => s.replace(/&/g, "&amp;").replace(/"/g, "&quot;");
$("examples").innerHTML = `<option value="">examples…</option>` + EXAMPLES.map(([s, d]) => `<option value="${attr(s)}">${d}</option>`).join("")
  + ADDS.map(([group, rows]) => `<optgroup label="${attr(group)}">${rows.map(([s, kind]) => `<option value="${attr(s)}">${kind}: ${s}</option>`).join("")}</optgroup>`).join("");
// the adds, each a link that plays it: a long run plays faster
const paceOf = n => n < 2500 ? 40 : n < 6000 ? 62 : n < 13000 ? 76 : 86;
$("adds").innerHTML = ADDS.map(([group, rows]) => `<h3>${group}</h3>` + rows.map(([s, kind, what, rewrites, rounds]) =>
  `<p><a href="#src=${attr(s)}&play=1&speed=${paceOf(rewrites)}" data-src="${attr(s)}" data-speed="${paceOf(rewrites)}"><code>${s}</code></a><br>`
  + `<span class="kind ${kind}">${kind}</span> ${what} <span class="dim">${rewrites.toLocaleString("en")} rewrites in ${rounds.toLocaleString("en")} rounds.</span></p>`).join("")).join("");
for (const a of $("adds").querySelectorAll("a")) a.onclick = ev => { ev.preventDefault(); location.hash = `src=${a.dataset.src}&play=1&speed=${a.dataset.speed}`; start(); };
$("tour").innerHTML = TOUR.map(([what, link], i) => `<p><a href="#${link}" data-t="${i}">${i + 1}.</a> ${what}</p>`).join("");
for (const a of $("tour").querySelectorAll("a")) a.onclick = ev => { ev.preventDefault(); location.hash = TOUR[+a.dataset.t][1]; start(); };
$("prelude").textContent = PRELUDE.trim();
start();
requestAnimationFrame(frame);
