// Two-way programs in rev.html (crate/src/iso.rs): the program is a tree of instructions, and the token
// walks it, down into an instruction and back up, carrying the tree it works on. Working on a part parks
// the rest of the tree at the instruction (drawn beside it); a call lays a copy of its definition out
// under it while the token is inside. The tree the token carries hangs beside the token. Going back in
// time is the token walking the same way back, turned around (pink), undoing each step as it goes.
"use strict";

/// How the carried tree is written out: "nat" (forks over a leaf as numbers), "list" (and lists of
/// them), or "raw"; and, when set ({x, y, w, h} as fractions of the page), a panel of its own for it.
const TWO_VIEW = { read: "nat", panel: null };
/// A tree as the program's patterns write it: △ a leaf, △x a stem, (x, y) a fork; numbers and lists as
/// TWO_VIEW.read says.
function twoText(t) {
  const nat = t => { let n = 0; while (t.k === 2 && t.l.k === 0) { n++; t = t.r; } return t.k === 0 && n > 0 ? n : null; };
  const go = t => {
    if (TWO_VIEW.read !== "raw") { const n = nat(t); if (n !== null) return String(n); }
    if (TWO_VIEW.read === "list" && t.k === 2) {
      const xs = []; let u = t;
      while (u.k === 2) { const n = nat(u.l) ?? (u.l.k === 0 ? 0 : null); if (n === null) break; xs.push(n); u = u.r; }
      if (u.k === 0 && xs.length) return `[${xs.join(", ")}]`;
    }
    if (t.k === 0) return "△";
    if (t.k === 1) return "△" + (t.c.k === 2 ? go(t.c) : go(t.c));
    return `(${go(t.l)}, ${go(t.r)})`;
  };
  return go(t);
}
const TWO = {
  n: 0, prof: [], clock: 0, forward: true, depth: 0, held: 0, forgotten: 0, input: "", what: "", err: "",
  nodes: [], byId: new Map(), token: null, payload: null, parks: new Map(), shown: new Map(), target: new Map(),
};
/// Each kind of instruction: its name, colour and what it does.
const TWO_KIND = [
  ["then", "#8595ad", "runs its parts one after the other (going back, last first)"],
  ["rearrange", "#56c8ff", "takes the tree apart by the left pattern and puts the same pieces together by the right one; going back, the other way round"],
  ["copy", "#c792ea", "a → (a, a); going back it takes the copy back, which it can only do from two equal halves"],
  ["on", "#4fd6be", "works on one part of the tree: the token takes the part down, and the rest waits parked here until it comes back up"],
  ["choose", "#c792ea", "the first arm whose when-pattern the tree fits; coming back up, the arm's gives-pattern must fit (and no earlier arm's), and going back in time it is the gives-patterns that choose"],
  ["loop", "#f7768e", "runs its first part; until the until-pattern fits, its second part and the first again; the from-pattern fits on the way in and on no later round"],
  ["call", "#ffa94d", "a definition from the library: a copy of it is laid out underneath while the token is inside, and taken away when it leaves"],
  ["run", "#ffd866", "the left half of the tree is a program: it is laid out underneath and run on the right half, and waits parked here meanwhile"],
  ["forget", "#ff5f5f", "throws the right half away: the one step that cannot be taken back"],
  ["note", "#dfe4ea", "says in words what its part does; the part runs as it is"],
  ["both", "#7ee787", "the two halves of the pair at once: the token becomes a fork, the pair waiting with a hole for each half and a token of its own taking each; with both halves back in their holes it is one token again"],
];
const TWO_EXAMPLES = [
  ["add 2 3", "(a, b) → (a, a + b): hands a back with the sum"],
  ["add' 2 5", "add's inverse, read off its text: subtraction"],
  ["rev [1, 2, 3]", "reverse a list by recursion"],
  ["revfast [1, 2, 3, 4]", "reverse with a loop, in n steps"],
  ["dagger add", "a two-way program inverting add, given as a tree"],
  ["run add 2 3", "add handed over as a tree, and run"],
  ["tc F(L,F(L,L)) 3", "the tree calculus's K rule, derived: the argument goes onto the trace"],
  ["tc F(S(F(L,L)),F(L,L)) 2", "the S rule, derived: the argument copied, three applications"],
  ["tc F(F(L,S(L)),L) S(L)", "triage, derived: the arms not taken go onto the trace"],
  ["tc add 1 1", "disp's add, run by the derived tree calculus, with its trace"],
  ["tck add 1 1", "the same, keeping its programs: the trace a third shorter"],
  ["bennett add 1 1", "the same, the trace cleaned up by running back (twice the steps)"],
  ["forget add 1 1", "the same, the trace thrown away: the end cannot go back"],
  ["bijection rev rev [1, 2, 3]", "disp's rev is its own inverse: run as a bijection, nothing is left over"],
];
/// What the program's input and answer are, to read them as disp values.
const TWO_READS = { swap: ["(n, n)", "(n, n)"], succ: ["n", "n"], add: ["(n, n)", "(n, n)"], double: ["n", "(n, n)"], snoc: ["(list, n)", "list"], rev: ["list", "list"], revfast: ["list", "list"] };

/// A disp program named in a tree-calculus call (`tc add 1 1`) as its tree, for the engine; after a
/// program on binary numbers, numbers as number trees (`tck relay_add 3 1`).
function twoSource(input) {
  const m = input.match(/^(tc|tck|jet|bennett|forget|bijection)\s+(.*)$/s);
  if (!m) return input;
  let k;
  // each word naming a disp program becomes its tree
  return `${m[1]} ` + m[2].replace(/\b[a-z_][a-z_0-9]*\b|\b\d+\b/g, w => {
    if (/^\d+$/.test(w)) return k === undefined ? w : `#${bitTree(BigInt(w), k)}`;
    const d = (window.DISP_PROGRAMS || []).find(p => p.name === w);
    if (d?.kind === "bits") k = d.width ? Math.log2(d.width) : bitsWidth(m[2]);
    return d ? `#${d.tree}` : w;
  });
}
/// The call's mode word (tc, bennett, …; "" for a library program), the program's name, and whether it is
/// run backward.
function twoCall(input) {
  const word = input.split(/\s/)[0], name = word.replace(/('|⁻¹)$/, "");
  return { what: ["tc", "tck", "jet", "bennett", "forget", "run", "dagger", "bijection", "both"].includes(word) ? word : "", name, inv: name !== word,
    halves: word === "both" ? input.split(/\s+/).slice(1, 3).map(w => [w.replace(/('|⁻¹)$/, ""), /('|⁻¹)$/.test(w)]) : null,
    bits: input.split(/\s+/).some(w => (window.DISP_PROGRAMS || []).find(p => p.name === w)?.kind === "bits") };
}
/// What tree t, held at the start (at 0) or the end (at 1) of a call `st` loaded, reads as; else null.
function twoReading(st, t, at) {
  if (at >= 0 && TWO_READS[st.name] && !st.what) return readAs(TWO_READS[st.name][st.inv ? 1 - at : at], t);
  if (at >= 0 && st.what === "both" && t.k === 2) {
    const half = ([name, inv], u) => TWO_READS[name] ? readAs(TWO_READS[name][inv ? 1 - at : at], u) : null;
    const a = half(st.halves[0], t.l), b = half(st.halves[1], t.r);
    return a !== null && b !== null ? `(${a}, ${b})` : null;
  }
  if (at === 1 && st.what === "tc" && t.k === 2) return `answer ${readAs("n", t.l) ?? ""} and a trace`.replace("answer  and", "an answer and");
  if (at === 1 && (st.what === "tck" || st.what === "jet") && t.k === 2 && t.l.k === 2) return `answer ${(st.bits ? bitsValue(t.l.r) : readAs("n", t.l.r) ?? readAs("list", t.l.r)) ?? ""}, the program kept, and a trace`.replace("answer ,", "an answer,");
  if (at === 1 && st.what === "forget") return readAs("n", t) !== null ? `${readAs("n", t)}` : null;
  if (at === 1 && st.what === "bennett" && t.k === 2) return `${readAs("n", t.r) ?? readAs("list", t.r) ?? "the answer"} with the program and its arguments`;
  if (st.what === "bijection" && t.k === 2 && at >= 0) return `${readAs("list", t.r) ?? readAs("n", t.r) ?? "it"}, with the program and its inverse`;
  return null;
}
function twoLoad(input) {
  const n = withString(twoSource(input), (p, len) => Number(E.iso_new(p, len)));
  TWO.ok = n >= 0;
  if (n < 0) return text(E.text_len());
  const pr = seats(2 * (n + 1));
  Object.assign(TWO, { n, prof: pr, input, ...twoCall(input), shown: new Map(), target: new Map(), err: "" });
  twoSync();
  for (const [k, p] of TWO.target) TWO.shown.set(k, p.slice());
  return null;
}
function twoState() {
  const [clock, n, fwd, depth, held, forgotten] = text(E.iso_state()).split(" ").map(Number);
  Object.assign(TWO, { clock, n, forward: !!fwd, depth, held, forgotten });
}
/// Go to step t; returns how many steps the token moved (fewer if a step cannot be taken).
function twoGo(t) {
  const before = TWO.clock;
  if (t === before) return 0;
  let got = Number(E.iso_seek(t)), why = got !== t ? text(E.text_len()) : "";
  if (got !== t && t < got && /forget/.test(why)) {
    // What was forgotten cannot run back: start again from the input and run forward to t.
    withString(twoSource(TWO.input), (p, len) => E.iso_new(p, len));
    got = Number(E.iso_seek(t));
    why = got === t ? "replayed from the input: what was forgotten cannot run back" : text(E.text_len());
  }
  twoState();
  TWO.err = why;
  dirty = true;
  return Math.abs(got - before);
}
/// A tree as the engine gives it (`iso_tree`, preorder kinds), nested.
function twoTree(root, budget) {
  const n = E.iso_tree(root, budget), w = seats(2 * n);
  let i = 0;
  const go = () => {
    const k = w[2 * i], cut = w[2 * i + 1]; i++;
    if (k === 2) return { k, kids: [go()] };
    if (k === 3) { const a = go(); return { k, kids: [a, go()] }; }
    return { k, cut, kids: [] };
  };
  return n ? go() : null;
}
function twoSync() {
  if (!TWO.ok) return;
  twoState();
  const n = E.iso_prog(), w = seats(6 * n), lines = text(E.text_len()).split("\n");
  const nodes = [], byId = new Map();
  for (let i = 0; i < n; i++) {
    const [id, up, k, kind, park, at] = w.subarray(6 * i, 6 * i + 6), parts = lines[i].split("\t");
    const node = { id, up: up - 1, k, kind: kind & 15, inv: kind >= 16, park: park - 1, at, label: parts[0], arms: [], kids: [] };
    for (let a = 1; a + 1 < parts.length; a += 2) node.arms.push([parts[a], parts[a + 1]]);
    nodes.push(node); byId.set(id, node);
    if (node.up >= 0) byId.get(node.up).kids[k] = node;
  }
  TWO.nodes = nodes; TWO.byId = byId;
  TWO.token = nodes.find(x => x.at);
  TWO.payload = twoTree(0xffffffff, 500);
  TWO.payloadText = TWO.held < 3000 ? twoText(parseTreeText(text(E.iso_payload()))) : `${TWO.held} nodes`;
  TWO.parks = new Map(nodes.filter(x => x.park >= 0).map(x => [x.id, twoTree(x.park, 160)]));
  twoLay();
}

// ---- layout: the program top down, each instruction a box, its parts underneath in order ----------
const TB = { h: 15, row: 34, gap: 9, ch: 5.4 };
const twoLines = x => x.kind === 1 ? x.label.split(" ↔ ").map((s, i) => (i ? "↔ " : "") + s) : [x.label];
const twoWidth = x => Math.max(26, Math.max(...twoLines(x).map(s => s.length)) * TB.ch + 10);
const twoHeight = x => TB.h * twoLines(x).length - (twoLines(x).length - 1) * 4;
/// Whether instruction x is a call or run laid out now: its part is a copy of a definition (or the program
/// the data held), a frame of its own.
const twoOpen = x => (x.kind === 6 || x.kind === 7) && x.kids.length > 0;
/// Lay the program out frame by frame: the main program at the top, and under it, one below the other,
/// each laid-out call's copy (the calls the token is inside), each drawn whole, top down, its parts under
/// each instruction in order. A frame hangs from its call by a link.
function twoLay() {
  const root = TWO.nodes[0]; if (!root) return;
  const frames = [root];
  const path = []; for (let x = TWO.token; x; x = TWO.byId.get(x.up)) path.push(x);
  for (const x of path.reverse()) if (twoOpen(x)) frames.push(x.kids[0]);
  TWO.frames = frames;
  const span = new Map(), kidsOf = x => twoOpen(x) ? [] : x.kids;
  const measure = x => {
    const own = twoWidth(x) + (TWO.parks.has(x.id) ? 70 : 0), ks = kidsOf(x);
    const kids = ks.reduce((s, c) => s + measure(c) + TB.gap, -TB.gap);
    const w = Math.max(own, ks.length ? kids : 0); span.set(x.id, w); return w;
  };
  TWO.target.clear();
  let top = 0;
  for (const f of frames) {
    measure(f);
    let bottom = top;
    const place = (x, left, y) => {
      const w = span.get(x.id), ks = kidsOf(x);
      TWO.target.set(x.id, [left + w / 2, y]);
      bottom = Math.max(bottom, y + twoHeight(x));
      let at = left + (w - ks.reduce((s, c) => s + span.get(c.id) + TB.gap, -TB.gap)) / 2;
      for (const c of ks) { place(c, at, y + TB.row + twoHeight(x) - TB.h); at += span.get(c.id) + TB.gap; }
    };
    place(f, -span.get(f.id) / 2, top);
    top = bottom + TB.row * 1.6;
  }
  for (const [k, p] of TWO.target) if (!TWO.shown.has(k)) {
    // A newly drawn instruction starts where its parent is.
    const x = TWO.byId.get(k), up = x && TWO.shown.get(x.up);
    TWO.shown.set(k, up ? up.slice() : p.slice());
  }
  for (const k of [...TWO.shown.keys()]) if (!TWO.target.has(k)) TWO.shown.delete(k);
}
function twoGlide(k) { for (const [key, t] of TWO.target) { const p = TWO.shown.get(key); p[0] += (t[0] - p[0]) * k; p[1] += (t[1] - p[1]) * k; } }
/// Where the token is drawn: on the edge into its instruction, just above it.
function twoTokenAt() {
  const t = TWO.token; if (!t) return null;
  const p = TWO.shown.get(t.id); return p && [p[0], p[1] - twoHeight(t) / 2 - 9];
}
/// Everything drawn, as [x0, y0, x1, y1], and the token's place, for the view to follow.
function twoBox() {
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (const [k, p] of TWO.target) { const x = TWO.byId.get(k), w = twoWidth(x) / 2; x0 = Math.min(x0, p[0] - w); x1 = Math.max(x1, p[0] + w); y0 = Math.min(y0, p[1] - 20); y1 = Math.max(y1, p[1] + 20); }
  return [x0, y0, x1, y1];
}

// ---- drawing --------------------------------------------------------------------------------------
/// Lay out a tree (as `twoTree` gives it) tidily from (x, y) down: leaves `dx` apart, levels `dy`.
function treeLay(t, dx, dy) {
  const out = [];
  let col = 0;
  const go = (n, depth) => {
    if (!n.kids.length) { const p = { n, x: col++ * dx, y: depth * dy }; out.push(p); return p; }
    const kids = n.kids.map(c => go(c, depth + 1)), p = { n, x: (kids[0].x + kids[kids.length - 1].x) / 2, y: depth * dy, kids };
    out.push(p); return p;
  };
  const top = go(t, 0);
  return { top, all: out, w: Math.max(1, col) * dx, h: (Math.max(...out.map(p => p.y)) || 0) + dy };
}
/// A tree drawn in a dark card with its top-left at (x, y), at most w × h, a node at most r across;
/// `u` turns a width in pixels into the current transform's units. Returns the card's size.
function drawCard(t, x, y, maxW, maxH, r, tint, u) {
  if (!t) return [0, 0];
  const L = treeLay(t, 2.4, 2.9), pad = 1.8;
  r = Math.min(r, maxW / (L.w + pad * 2), maxH / (L.h + pad * 2));
  const w = (L.w - 2.4 + 2 + pad * 2) * r, h = (L.h - 2.9 + 2 + pad * 2) * r, ox = x + pad * r, oy = y + pad * r;
  ctx.fillStyle = "rgba(11,14,20,0.92)"; ctx.strokeStyle = tint; ctx.lineWidth = u;
  ctx.beginPath(); ctx.roundRect(x, y, w, h, 3 * u); ctx.fill(); ctx.stroke();
  ctx.strokeStyle = "#5b6a80"; ctx.lineWidth = 0.8 * u; ctx.beginPath();
  for (const p of L.all) for (const c of p.kids ?? []) { ctx.moveTo(ox + p.x * r, oy + p.y * r); ctx.lineTo(ox + c.x * r, oy + c.y * r); }
  ctx.stroke();
  const marks = new Path2D(), byColour = new Map();
  for (const p of L.all) {
    const k = p.n.k;
    if (k >= 1 && k <= 3) { const c = kindColor(k); let path = byColour.get(c); if (!path) byColour.set(c, path = new Path2D()); shapeInto(path, k, ox + p.x * r, oy + p.y * r, r, 0, -1, r / u > 3.5, marks); }
  }
  for (const [c, path] of byColour) { ctx.fillStyle = c; ctx.fill(path); }
  ctx.fillStyle = "#0b0e14"; ctx.fill(marks);
  ctx.font = `${Math.max(r * 1.5, 7 * u)}px ui-monospace, monospace`; ctx.textAlign = "center"; ctx.textBaseline = "middle";
  for (const p of L.all) {
    if (p.n.k === 4) { ctx.strokeStyle = "#ffa94d"; ctx.setLineDash([2 * u, 2 * u]); ctx.lineWidth = u; ctx.beginPath(); ctx.arc(ox + p.x * r, oy + p.y * r, r * 0.8, 0, 7); ctx.stroke(); ctx.setLineDash([]); }
    if (p.n.k === 5) { ctx.fillStyle = "#8595ad"; ctx.fillText(`…${p.n.cut}`, ox + p.x * r, oy + p.y * r); }
  }
  return [w, h];
}
function twoDraw() {
  if (!TWO.ok) return;
  twoGlide(0.3);
  const nodes = TWO.nodes.filter(x => TWO.target.has(x.id)), P = id => TWO.shown.get(id);
  // edges, a choice's arms labelled with their when-patterns
  ctx.strokeStyle = "#2f4a6e"; ctx.lineWidth = lw(1.6); ctx.beginPath();
  for (const x of nodes) if (!twoOpen(x)) for (const c of x.kids) { const a = P(x.id), b = P(c.id); if (b) { ctx.moveTo(a[0], a[1] + twoHeight(x) / 2); ctx.lineTo(b[0], b[1] - twoHeight(c) / 2); } }
  ctx.stroke();
  // each laid-out call to its copy, below
  ctx.strokeStyle = "#ffa94d99"; ctx.setLineDash([lw(4), lw(3)]); ctx.lineWidth = lw(1.4); ctx.beginPath();
  for (const x of nodes) if (twoOpen(x)) { const a = P(x.id), b = P(x.kids[0].id); if (b) { const y0 = a[1] + twoHeight(x) / 2, y1 = b[1] - twoHeight(x.kids[0]) / 2; ctx.moveTo(a[0], y0); ctx.bezierCurveTo(a[0], y0 + 40, b[0], y1 - 40, b[0], y1); } }
  ctx.stroke(); ctx.setLineDash([]);
  const big = view.s > 0.9;
  ctx.textAlign = "center"; ctx.textBaseline = "middle";
  if (big) {
    ctx.font = `8px ui-monospace, monospace`; ctx.fillStyle = "#8595ad";
    for (const x of nodes) if (x.kind === 4) x.kids.forEach((c, i) => {
      const b = P(c.id), w = x.arms[i]?.[0] ?? "", room = Math.max(4, Math.floor(twoWidth(c) / 4.6));
      if (b) ctx.fillText(w.length > room ? w.slice(0, room - 1) + "…" : w, b[0], b[1] - twoHeight(c) / 2 - 7);
    });
  }
  // instructions
  ctx.font = `9px ui-monospace, monospace`;
  for (const x of nodes) {
    const p = P(x.id), w = twoWidth(x), h = twoHeight(x), [name, colour] = TWO_KIND[x.kind] ?? ["?", "#8a96a8"];
    const open = twoOpen(x);
    ctx.fillStyle = open ? "#1d2633" : "#121a26"; ctx.strokeStyle = colour; ctx.lineWidth = lw(x === TWO.token ? 2.2 : 1.2);
    ctx.beginPath(); ctx.roundRect(p[0] - w / 2, p[1] - h / 2, w, h, 4); ctx.fill(); ctx.stroke();
    if (big || view.s * w > 30) { ctx.fillStyle = colour; twoLines(x).forEach((s, i) => ctx.fillText(s + (open && i === 0 ? " ▾" : ""), p[0], p[1] - h / 2 + 7.5 + i * 11)); }
  }
  // parked trees, beside their instructions
  for (const [id, t] of TWO.parks) { const p = P(id), x = TWO.byId.get(id); drawCard(t, p[0] + twoWidth(x) / 2 + 6, p[1] - 8, 60, 60, 2.4, "#ffa94d88", lw(1)); }
  // the token, an arrow the way it is going, and the tree it carries
  const tp = twoTokenAt();
  if (tp) {
    const down = (TWO.token.at & 3) === 1, colour = TWO.forward ? "#56c8ff" : "#ff7edb";
    ctx.fillStyle = colour; ctx.shadowColor = colour; ctx.shadowBlur = 10;
    ctx.beginPath(); ctx.arc(tp[0], tp[1], 5.5, 0, 7); ctx.fill(); ctx.shadowBlur = 0;
    const s = down ? 1 : -1;
    ctx.beginPath(); ctx.moveTo(tp[0] - 3.5, tp[1] + s * 4); ctx.lineTo(tp[0] + 3.5, tp[1] + s * 4); ctx.lineTo(tp[0], tp[1] + s * 9); ctx.fill();
  }
  if (hover?.two !== undefined) { const x = TWO.byId.get(hover.two), p = x && P(x.id); if (p) { ctx.strokeStyle = "#fff"; ctx.lineWidth = lw(1.5); const w = twoWidth(x) + 6, h = twoHeight(x) + 6; ctx.strokeRect(p[0] - w / 2, p[1] - h / 2, w, h); } }
  // the tree the token carries, in a card at the top right of the page, a line from the token to it
  if (tp) {
    const d = devicePixelRatio || 1, r = cv.getBoundingClientRect(), colour = TWO.forward ? "#56c8ff" : "#ff7edb";
    const sx = view.ox + tp[0] * view.s, sy = view.oy + tp[1] * view.s;
    ctx.setTransform(d, 0, 0, d, 0, 0);
    const pn = TWO_VIEW.panel;
    if (pn) {
      // A panel of its own: the tree written out large above it, the tree drawn big, centred.
      const px = pn.x * r.width, py = pn.y * r.height, pw = pn.w * r.width, ph = pn.h * r.height;
      ctx.fillStyle = "rgba(11,14,20,0.94)"; ctx.fillRect(px - 0.02 * r.width, 0, r.width - px + 0.02 * r.width, r.height);
      ctx.fillStyle = "#e6edf3"; ctx.font = "15px system-ui, sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "alphabetic";
      ctx.fillText("the token carries", px + pw / 2, py + 16);
      const line = TWO.payloadText ?? "";
      let size = 30; ctx.font = `${size}px ui-monospace, monospace`;
      while (size > 13 && ctx.measureText(line).width > pw) { size -= 1; ctx.font = `${size}px ui-monospace, monospace`; }
      ctx.fillStyle = colour; ctx.fillText(line.length > 90 ? line.slice(0, 89) + "…" : line, px + pw / 2, py + 26 + size);
      const top = py + 40 + size;
      const [w, h] = drawCard(TWO.payload, -1e5, -1e5, pw, ph - (top - py), 12, colour, 1);
      const x = px + (pw - w) / 2;
      ctx.strokeStyle = colour + "66"; ctx.lineWidth = 1.5; ctx.setLineDash([5, 5]); ctx.beginPath(); ctx.moveTo(sx + 6, sy); ctx.lineTo(x, top + h / 2); ctx.stroke(); ctx.setLineDash([]);
      drawCard(TWO.payload, x, top, pw, ph - (top - py), 12, colour, 1);
    } else {
      const maxW = r.width * 0.34, maxH = r.height * 0.4, top = 64;
      // measure first (drawn off the page), then place at the right edge
      const [w, h] = drawCard(TWO.payload, -1e5, -1e5, maxW, maxH, 6, colour, 1);
      const x = r.width - w - 14;
      ctx.strokeStyle = colour + "66"; ctx.lineWidth = 1; ctx.setLineDash([4, 4]); ctx.beginPath(); ctx.moveTo(sx + 6, sy); ctx.lineTo(x, top + h / 2); ctx.stroke(); ctx.setLineDash([]);
      drawCard(TWO.payload, x, top, maxW, maxH, 6, colour, 1);
      ctx.fillStyle = colour; ctx.font = "11px ui-monospace, monospace"; ctx.textAlign = "right"; ctx.textBaseline = "alphabetic";
      ctx.fillText(`the token carries (${TWO.held} nodes held)`, x + w, top - 6);
    }
    ctx.setTransform(d * view.s, 0, 0, d * view.s, d * view.ox, d * view.oy);
  }
}
/// The bar: how many definitions are laid out after each step (amber, bright where it has run), and how
/// many tree nodes are held (the line).
function twoStrip(W, H, d) {
  if (!TWO.ok) return;
  const n = TWO.n, pr = TWO.prof; if (!n) return;
  let topD = 1, topN = 1; for (let i = 0; i <= n; i++) { topD = Math.max(topD, pr[2 * i]); topN = Math.max(topN, pr[2 * i + 1]); }
  const per = Math.max(1, (n + 1) / W), cols = Math.ceil((n + 1) / per), cw = W / cols;
  sctx.beginPath();
  for (let c = 0; c < cols; c++) {
    const a = Math.floor(c * per), b = Math.min(n + 1, Math.floor((c + 1) * per) || a + 1);
    let m = 0, h = 0; for (let k = a; k < b; k++) { m = Math.max(m, pr[2 * k]); h = Math.max(h, pr[2 * k + 1]); }
    const bh = 2 * d + (H - 8 * d) * m / topD;
    sctx.fillStyle = a <= TWO.clock ? "#ffa94d" : "#4a3a24"; sctx.fillRect(c * cw, H - bh, Math.max(cw - (cw > 3 ? 1 : 0), 1), bh);
    const y = H - 2 * d - (H - 8 * d) * h / topN;
    if (c === 0) sctx.moveTo(c * cw, y); else sctx.lineTo(c * cw + cw / 2, y);
  }
  sctx.strokeStyle = "#56c8ff"; sctx.lineWidth = d; sctx.stroke();
}
function twoHud() {
  if (!TWO.ok) return;
  const tree = text(E.iso_payload()), at = TWO.clock === 0 ? 0 : TWO.clock === TWO.n ? 1 : -1;
  const read = twoReading(TWO, parseTreeText(tree), at);
  $("answer").textContent = (read !== null ? `${read} = ` : "") + tree.slice(0, 90) + (tree.length > 90 ? "…" : "");
  const s = TWO.err ? TWO.err : at === 0 ? "the beginning: the input, nothing else" : at === 1 ? (TWO.forgotten ? "the end: the answer; the trace is forgotten, so this cannot go back" : "the end: the answer, nothing else kept")
    : `step ${TWO.clock} of ${TWO.n}, the token going ${TWO.forward ? "forward" : "back"}`;
  $("status").textContent = s; $("status").className = TWO.err && !/^replayed/.test(TWO.err) ? "bad" : at >= 0 ? "ok" : "";
  $("counts").innerHTML = [
    ["step", `${TWO.clock} of ${TWO.n}`], ["the token is going", TWO.forward ? "forward" : "back"], ["instructions laid out", TWO.nodes.length],
    ["…definitions laid out", TWO.depth], ["tree nodes held", TWO.held], ["…parked", TWO.parks.size + " trees"],
    ["kept to go back", TWO.forgotten ? "nothing, and the trace was thrown away" : "nothing"],
  ].map(([k, v]) => `<div><span>${k}</span><span>${v}</span></div>`).join("");
}
function twoPointed(wx, wy) {
  for (const x of TWO.nodes) { const p = TWO.shown.get(x.id), w = twoWidth(x) / 2, h = twoHeight(x) / 2; if (p && Math.abs(wx - p[0]) <= w && Math.abs(wy - p[1]) <= h) return { two: x.id }; }
  const tp = twoTokenAt(); if (tp && (wx - tp[0]) ** 2 + (wy - tp[1]) ** 2 < 64) return { token: true };
  return null;
}
function twoDetail(hit) {
  if (hit?.token) return `<b>the token</b><div>carries the tree being worked on; click it to turn it around and run the other way</div>`;
  const x = hit && TWO.byId.get(hit.two);
  if (!x) return "point at an instruction or the token";
  const [name, colour, desc] = TWO_KIND[x.kind];
  const arms = x.arms.map(([w, g], i) => `<div style="font-family:var(--mono);font-size:11px">${i + 1}. when ${w}, gives ${g}</div>`).join("");
  return `<div><span class="sw" style="background:${colour}"></span><b>${name}${x.inv ? " (backward)" : ""}</b>: ${x.label}</div><div>${desc}</div>${arms}`
    + (TWO.parks.has(x.id) ? `<div style="color:var(--fg-muted)">a tree is parked here (beside it), waiting for the token to come back up</div>` : "");
}
const TWOV = { st: TWO, load: twoLoad, sync: twoSync, hud: twoHud, draw: twoDraw, strip: twoStrip, pointed: twoPointed, detail: twoDetail,
  go: twoGo, box: twoBox, tokenAt: twoTokenAt, examples: TWO_EXAMPLES, follow: 1.6, panel: 0 };
