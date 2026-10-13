// The persistent two-way machine in rev.html (crate/src/walk.rs). The program is one raw tree of agents
// (leaf, stem, fork) that never changes; tokens walk it, and what an instruction does shows only as the
// rule that fires when a token reaches its node: the rule's words flash there, and the node keeps a ring
// in the colour of what it did while the run is past it. Each token carries a tree and a stack. At a
// `both` a token becomes a fork: its pair waits beside the node with a hole for each half, a token of its
// own taking each half (a dashed wire from the hole to it); a half done comes back into its hole, and with
// both back the fork is one token again. Moves need no order: "a round at a time" moves every token and
// fork that can move at once, else one moves at a time. The bar: how many tokens there are, round by round.
"use strict";

const PER = {
  ok: false, n: 0, rounds: 0, prof: null, clock: 0, forward: true, codeN: 0, dataN: 0, stackN: 0, input: "", err: "", codeN0: -1,
  code: null, pos: new Map(), toks: [], forks: [], shown: new Map(), fireAt: new Map(), seen: new Map(), most: 1,
};
const PER_EXAMPLES = [
  ["both rev rev [1, 2] [3, 4, 5]", "a token for each half: each reverses its list, and the first done waits in its hole for the other"],
  ["both succ rev 3 [1, 2, 3]", "halves of different lengths: one is back long before the other"],
  ["tck F(S(F(L,L)),F(L,L)) S(L)", "the tree calculus's S rule: c x and b x by two tokens at once"],
  ["tck add 1 1", "disp's add on the tree calculus: up to four tokens"],
  ["jet add 3 4", "the same add as a jet: the tree calculus knows add's tree and runs the library's two-way add, 53 moves against 11,845"],
  ["add 2 3", "one token: the recursion walks back to add's node; agents are made only as b grows"],
  ["revfast [1, 2, 3]", "one token round a loop"],
  ["add' 2 5", "add backward: the call walks add's tree backward"],
  ["dagger tck", "inverting the tree calculus, given as a tree: both's two sides inverted at once"],
  ["tck relay_add 3 1", "disp's relay adder on 2-bit numbers: no more than five tokens, however many bits"],
];
/// What each kind of instruction is called, by the number walk_code gives it.
const PER_ROLE = ["then", "rearrange", "copy", "on a part", "choose", "loop", "call", "run", "forget", "note", "both"];
/// A token's colour, by its number (the first blue).
const perHue = id => `hsl(${(200 + id * 137.5) % 360}, 85%, 66%)`;

function perLoad(input) {
  const n = withString(twoSource(input), (p, len) => Number(E.walk_new(p, len)));
  PER.ok = n >= 0;
  if (n < 0) return text(E.text_len());
  const prof = seats(6 * (n + 1));
  let most = 1; for (let i = 0; i <= n; i++) most = Math.max(most, prof[6 * i]);
  Object.assign(PER, { rounds: n, prof, most, input, ...twoCall(input), codeN0: -1, fireAt: new Map(), seen: new Map(), shown: new Map(), err: "" });
  perSync();
  return null;
}
function perState() {
  const [clock, n, fwd, , , , codeN, dataN, stackN] = text(E.walk_state()).split(" ").map(Number);
  Object.assign(PER, { clock, n, forward: !!fwd, codeN, dataN, stackN });
}
/// The rules the last move or round fired: they flash, and count while the run is past them.
function perFired(sign) {
  const n = E.walk_fired(), w = seats(n), now = performance.now();
  for (const f of w) {
    const c = (PER.seen.get(f) ?? 0) + sign;
    if (c > 0) PER.seen.set(f, c); else PER.seen.delete(f);
    PER.fireAt.set(f, now);
  }
}
/// k rounds (or k moves, one at a time) forward or back. Returns how many were made.
function perMove(k, fwd) {
  let done = 0;
  for (; done < k; done++) {
    const moved = $("byRound").checked ? E.walk_round(fwd ? 1 : 0) : E.walk_step(fwd ? 1 : 0);
    if (!moved) break;
    perFired(fwd ? 1 : -1);
  }
  if (done < k) { const why = text(E.text_len()); PER.err = /replayed|forget/.test(why) ? why : ""; if (PER.err) PER.seen.clear(); }
  else PER.err = "";
  dirty = true;
  return done;
}
/// Go to where t moves are made (the bar): by rounds while a whole one fits, then one move at a time.
function perGo(t) {
  const before = PER.clock;
  let at = before, guard = 0;
  while (at !== t && guard++ < 1e6) {
    const fwd = t > at, gap = Math.abs(t - at), room = PER.toks.length + 2 * PER.forks.length + 1;
    const moved = gap > room ? E.walk_round(fwd ? 1 : 0) : E.walk_step(fwd ? 1 : 0);
    if (!moved) break;
    perFired(fwd ? 1 : -1);
    at = Number(text(E.walk_state()).split(" ")[0]);
  }
  dirty = true;
  return Math.abs(at - before);
}
function perSync() {
  if (!PER.ok) return;
  perState();
  // The program's tree changes only when a program held as data is laid out to run: read it again then.
  if (PER.codeN !== PER.codeN0) { PER.codeN0 = PER.codeN; perReadCode(); }
  const nt = E.walk_tokens(), w = seats(8 * nt);
  PER.toks = [];
  for (let i = 0; i < nt; i++) {
    const [id, at, up, par, payload, depth, half, side] = w.subarray(8 * i, 8 * i + 8);
    PER.toks.push({ id, at, up: !!up, par: !!par, payload, depth, half: half - 1, side });
  }
  const nf = E.walk_forks(), wf = seats(5 * nf);
  PER.forks = [];
  for (let j = 0; j < nf; j++) { const [id, node, pair, s0, s1] = wf.subarray(5 * j, 5 * j + 5); PER.forks.push({ id, node, pair, slots: [s0, s1] }); }
  const budget = Math.max(60, Math.floor(400 / Math.max(1, PER.toks.length)));
  for (const t of PER.toks) {
    t.tree = perTree(t.payload, budget);
    t.text = PER.dataN < 2500 ? twoText(parseTreeText(text(E.walk_show(t.payload)))) : "…";
    if (t.depth) { const k = E.walk_stack(t.id), s = seats(4 * k); for (let f = k - 1; f >= 0; f--) if (s[4 * f] === 1 || s[4 * f] === 2) { t.ret = s[4 * f + 2] - 1; break; } }
  }
  for (const f of PER.forks) f.tree = perTree(f.pair, 120);
  // one token: its stack, frame by frame, for the panel
  PER.stack = [];
  if (PER.toks.length === 1) {
    const k = E.walk_stack(PER.toks[0].id), s = seats(4 * k);
    for (let i = 0; i < k; i++) PER.stack.push({ kind: s[4 * i], root: s[4 * i + 1] - 1, call: s[4 * i + 2] - 1, inv: !!s[4 * i + 3] });
    for (const f of PER.stack.slice(-6)) if (f.root >= 0) f.tree = perTree(f.root, 90);
  }
  for (const k of [...PER.shown.keys()]) if (!PER.toks.some(t => t.id === k)) PER.shown.delete(k);
}
/// A tree the machine holds (`walk_tree`), nested as `twoTree` gives one.
function perTree(root, budget) {
  const n = E.walk_tree(root, budget), w = seats(2 * n);
  let i = 0;
  const go = () => { const k = w[2 * i], cut = w[2 * i + 1]; i++; if (k === 2) return { k, kids: [go()] }; if (k === 3) { const a = go(); return { k, kids: [a, go()] }; } return { k, cut, kids: [] }; };
  return n ? go() : null;
}

/// The program's tree, laid out top down, each node over its children, subtrees packed as close as their
/// outlines let them (after Reingold and Tilford).
const PG = { dx: 9, dy: 13, r: 3.2 };
function perReadCode() {
  const n = E.walk_code(), w = seats(5 * n), labels = new Map();
  for (const line of text(E.text_len()).split("\n")) { const tab = line.indexOf("\t"); if (tab > 0) labels.set(+line.slice(0, tab), line.slice(tab + 1)); }
  const nodes = new Map();
  for (let i = 0; i < n; i++) { const [id, kind, up, k, what] = w.subarray(5 * i, 5 * i + 5); nodes.set(id, { id, kind, up: up - 1, k, what: what - 1, kids: [], label: labels.get(id) ?? "" }); }
  for (const x of nodes.values()) if (x.up >= 0) nodes.get(x.up).kids[x.k] = x;
  // each subtree's outline: its leftmost and rightmost x at each depth below it, from its own x
  const rel = new Map();
  const outline = x => {
    const ks = x.kids.filter(Boolean);
    if (!ks.length) return { l: [0], r: [0] };
    let acc = null; const at = [];
    for (const o of ks.map(outline)) {
      if (!acc) { acc = o; at.push(0); continue; }
      let need = -Infinity;
      for (let e = 0; e < Math.min(acc.r.length, o.l.length); e++) need = Math.max(need, acc.r[e] - o.l[e] + PG.dx);
      at.push(need);
      for (let e = 0; e < o.l.length; e++) { if (e < acc.l.length) acc.r[e] = o.r[e] + need; else { acc.l.push(o.l[e] + need); acc.r.push(o.r[e] + need); } }
    }
    const mid = (at[0] + at[at.length - 1]) / 2;
    ks.forEach((c, i) => rel.set(c.id, at[i] - mid));
    return { l: [0, ...acc.l.map(v => v - mid)], r: [0, ...acc.r.map(v => v - mid)] };
  };
  const pos = new Map();
  const place = (x, px, depth) => { pos.set(x.id, [px, depth * PG.dy]); for (const c of x.kids) if (c) place(c, px + rel.get(c.id), depth + 1); };
  let left = 0;
  for (const x of nodes.values()) if (x.up < 0) { const o = outline(x); place(x, left - Math.min(...o.l), 0); left += Math.max(...o.r) - Math.min(...o.l) + 3 * PG.dx; }
  PER.code = nodes; PER.pos = pos;
}
function perBox() {
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (const [, p] of PER.pos) { x0 = Math.min(x0, p[0]); x1 = Math.max(x1, p[0]); y0 = Math.min(y0, p[1]); y1 = Math.max(y1, p[1]); }
  return [x0, y0, x1, y1];
}
/// Where each token belongs: beside its node, above it about to go in, below it come out; tokens at one
/// node side by side. And where each fork's pair waits: below and left of its `both`, one under another.
function perPlaces() {
  const out = new Map(), count = new Map();
  for (const t of PER.toks) {
    const p = PER.pos.get(t.at); if (!p) continue;
    const k = count.get(t.at) ?? 0; count.set(t.at, k + 1);
    out.set(t.id, [p[0] + 6 + 7 * k, p[1] + (t.up ? 7 : -7)]);
  }
  const forks = new Map(), fcount = new Map();
  for (const f of PER.forks) {
    const p = PER.pos.get(f.node); if (!p) continue;
    const k = fcount.get(f.node) ?? 0; fcount.set(f.node, k + 1);
    forks.set(f.id, [p[0] - 16 - 20 * k, p[1] + 10]);
  }
  return { toks: out, forks };
}
/// Where the view follows: the middle of the tokens and the pairs waiting for their halves.
function perTokenAt() {
  const pl = perPlaces(), ps = [...pl.toks.values(), ...pl.forks.values()]; if (!ps.length) return null;
  let x = 0, y = 0; for (const p of ps) { x += p[0]; y += p[1]; }
  return [x / ps.length, y / ps.length];
}
/// The words of an instruction's rule, as it flashes.
const perSays = x => (x.what & 15) === 6 ? `call ${x.label}` : x.label;

function perDraw() {
  if (!PER.ok || !PER.code) return;
  const now = performance.now(), P = id => PER.pos.get(id), colourOf = x => TWO_KIND[x.what & 15]?.[1] ?? "#fff";
  const places = perPlaces();
  for (const [id, want] of places.toks) { const s = PER.shown.get(id); if (!s) PER.shown.set(id, want.slice()); else { s[0] += (want[0] - s[0]) * 0.35; s[1] += (want[1] - s[1]) * 0.35; } }
  ctx.strokeStyle = "#2a3a52"; ctx.lineWidth = lw(1); ctx.beginPath();
  for (const x of PER.code.values()) for (const c of x.kids) if (c) { const a = P(x.id), b = P(c.id); ctx.moveTo(a[0], a[1]); ctx.lineTo(b[0], b[1]); }
  ctx.stroke();
  // the program's agents, the same kinds as any tree's: what they mean is not drawn, only found out
  const marks = new Path2D(), byColour = new Map();
  for (const x of PER.code.values()) {
    const p = P(x.id), c = kindColor(x.kind); let path = byColour.get(c); if (!path) byColour.set(c, path = new Path2D());
    shapeInto(path, x.kind, p[0], p[1], PG.r, 0, -1, view.s * PG.r > 3.5, marks);
  }
  ctx.globalAlpha = 0.8; for (const [c, path] of byColour) { ctx.fillStyle = c; ctx.fill(path); } ctx.globalAlpha = 1;
  ctx.fillStyle = "#0b0e14"; ctx.fill(marks);
  // nodes whose rule has fired by now: a ring in the colour of what it turned out to do
  ctx.lineWidth = lw(1.6);
  for (const [id] of PER.seen) { const x = PER.code.get(id), p = P(id); if (!x || !p) continue; ctx.strokeStyle = colourOf(x); ctx.beginPath(); ctx.arc(p[0], p[1], PG.r * 1.9, 0, 7); ctx.stroke(); }
  // forks: the pair waiting beside its both, a hole for each half away, wired to the token that has it
  for (const f of PER.forks) {
    const fp = places.forks.get(f.id), np = P(f.node); if (!fp || !np) continue;
    ctx.strokeStyle = "#7ee78799"; ctx.lineWidth = lw(1); ctx.beginPath(); ctx.moveTo(np[0], np[1]); ctx.lineTo(fp[0], fp[1]); ctx.stroke();
    const path = new Path2D(), mk = new Path2D();
    shapeInto(path, 3, fp[0], fp[1], PG.r * 2.2, 0, -1, true, mk);
    ctx.fillStyle = "#7ee787"; ctx.fill(path); ctx.fillStyle = "#0b0e14"; ctx.fill(mk);
    f.slots.forEach((s, side) => {
      const hx = fp[0] + (side ? 5.5 : -5.5), hy = fp[1] + 8.5;
      ctx.lineWidth = lw(1.2);
      if (s === 0) {
        ctx.strokeStyle = "#7ee787"; ctx.setLineDash([lw(2), lw(2)]); ctx.beginPath(); ctx.arc(hx, hy, 3, 0, 7); ctx.stroke(); ctx.setLineDash([]);
        const t = PER.toks.find(t => t.half === f.id && t.side === side), tp = t && PER.shown.get(t.id);
        if (tp) { ctx.strokeStyle = perHue(t.id); ctx.globalAlpha = 0.55; ctx.setLineDash([lw(3), lw(3)]); ctx.beginPath(); ctx.moveTo(hx, hy); ctx.lineTo(tp[0], tp[1]); ctx.stroke(); ctx.setLineDash([]); ctx.globalAlpha = 1; }
      } else { ctx.fillStyle = s === 2 ? "#7ee787" : "#8595ad"; ctx.beginPath(); ctx.arc(hx, hy, 3, 0, 7); ctx.fill(); }
    });
  }
  // a rule that just fired: its words over its node, fading
  const fs = Math.max(7, 11 / Math.sqrt(view.s));
  ctx.textAlign = "center"; ctx.textBaseline = "alphabetic"; ctx.font = `${fs}px ui-monospace, monospace`;
  for (const [id, t] of PER.fireAt) {
    const a = 1 - (now - t) / 1600; if (a <= 0) { PER.fireAt.delete(id); continue; }
    const x = PER.code.get(id), p = P(id); if (!x || !p) continue;
    const says = perSays(x), w = ctx.measureText(says).width + 8, y = p[1] - PG.r * 2.5;
    ctx.globalAlpha = Math.min(1, a * 1.5);
    ctx.fillStyle = "rgba(11,14,20,0.85)"; ctx.fillRect(p[0] - w / 2, y - fs - 3, w, fs + 3);
    ctx.fillStyle = colourOf(x); ctx.fillText(says, p[0], y - 3);
    ctx.globalAlpha = 1;
  }
  // the tokens, each its own colour, ringed blue going forward in time and pink going back
  const ring = PER.forward ? "#56c8ff" : "#ff7edb";
  for (const t of PER.toks) {
    const tp = PER.shown.get(t.id); if (!tp) continue;
    ctx.fillStyle = perHue(t.id); ctx.shadowColor = ring; ctx.shadowBlur = 10;
    ctx.beginPath(); ctx.arc(tp[0], tp[1], 4.2, 0, 7); ctx.fill(); ctx.shadowBlur = 0;
    ctx.strokeStyle = ring; ctx.lineWidth = lw(1.3); ctx.stroke();
    const s = t.up ? -1 : 1;
    ctx.beginPath(); ctx.moveTo(tp[0] - 2.8, tp[1] + s * 3); ctx.lineTo(tp[0] + 2.8, tp[1] + s * 3); ctx.lineTo(tp[0], tp[1] + s * 7); ctx.fill();
  }
  if (hover?.per !== undefined) { const p = P(hover.per); if (p) { ctx.strokeStyle = "#fff"; ctx.lineWidth = lw(1.4); ctx.beginPath(); ctx.arc(p[0], p[1], PG.r * 2.4, 0, 7); ctx.stroke(); } }
  perPanel(ring);
}
/// The panel at the right: what each token carries (one token: its stack too, return points linked to
/// their calls' nodes), and the pairs waiting for their halves.
function perPanel(ring) {
  const d = devicePixelRatio || 1, r = cv.getBoundingClientRect(), P = id => PER.pos.get(id);
  ctx.setTransform(d, 0, 0, d, 0, 0);
  const pn = TWO_VIEW.panel ?? { x: 0.65, y: 0.09, w: 0.33, h: 0.36 };
  const px = pn.x * r.width, pw = pn.w * r.width, big = TWO_VIEW.panel ? 1.25 : 1, bottom = r.height - fitMargins[1] - 20;
  ctx.fillStyle = "#0b0e14"; ctx.fillRect(px - 0.015 * r.width, 0, r.width, r.height);
  ctx.fillStyle = "#1b2436"; ctx.fillRect(px - 0.015 * r.width, 0, 1, r.height);
  let y = pn.y * r.height;
  const head = s => { ctx.textAlign = "center"; ctx.fillStyle = "#e6edf3"; ctx.font = `${13 * big}px system-ui, sans-serif`; ctx.fillText(s, px + pw / 2, y + 12 * big); y += 18 * big; };
  const fitText = (s, size, width) => {
    ctx.font = `${size}px ui-monospace, monospace`;
    while (size > 10 && ctx.measureText(s).width > width) { size--; ctx.font = `${size}px ui-monospace, monospace`; }
    if (ctx.measureText(s).width > width) { while (s.length > 1 && ctx.measureText(s + "…").width > width) s = s.slice(0, Math.floor(s.length * 0.9)); s += "…"; }
    return [s, size];
  };
  const links = [];
  if (PER.toks.length === 1 && !PER.forks.length) {
    const t = PER.toks[0];
    head("the token carries");
    const [line, size] = fitText(t.text, 24 * big, pw);
    ctx.textAlign = "center"; ctx.fillStyle = perHue(t.id); ctx.fillText(line, px + pw / 2, y + size); y += size + 8;
    const [cw, ch] = drawCard(t.tree, -1e5, -1e5, pw, pn.h * r.height, 10, perHue(t.id), 1);
    drawCard(t.tree, px + (pw - cw) / 2, y, pw, pn.h * r.height, 10, perHue(t.id), 1);
    y += ch + 16;
    head(PER.stack.length ? `and a stack, ${PER.stack.length} deep (top first)` : "and a stack, empty");
    const small = `${11 * big}px ui-monospace, monospace`;
    let shown = 0;
    for (let i = PER.stack.length - 1; i >= Math.max(0, PER.stack.length - 6) && y < bottom - 90; i--) {
      const f = PER.stack[i]; shown++;
      ctx.font = small; ctx.fillStyle = "#ffa94d"; ctx.textAlign = "left";
      if (f.kind === 0) {
        ctx.fillText("the rest, waiting for its part:", px, y + 11 * big);
        const [, h] = drawCard(f.tree, px, y + 15 * big, pw, 60 * big, 5, "#ffa94d88", 1);
        y += 21 * big + h;
      } else {
        const call = PER.code?.get(f.call);
        ctx.fillText(`↩ back to ${call ? perSays(call) : "the call"}${f.kind === 2 ? ", the program run waiting:" : ""}`, px, y + 11 * big);
        links.push([px - 4, y + 7 * big, f.call]);
        y += 17 * big;
        if (f.kind === 2 && f.tree) { const [, h] = drawCard(f.tree, px, y, pw, 50 * big, 4, "#ffd86688", 1); y += h + 6; }
      }
    }
    if (shown < PER.stack.length) { ctx.font = small; ctx.textAlign = "left"; ctx.fillStyle = "#8595ad"; ctx.fillText(`… and ${PER.stack.length - shown} more below`, px, y + 12); }
  } else {
    head(PER.toks.length > 1 ? `${PER.toks.length} tokens, each carrying its own tree` : "a token, carrying");
    const each = Math.min(110 * big, (bottom - y - (PER.forks.length ? 60 : 0)) / Math.max(1, PER.toks.length));
    for (const t of PER.toks) {
      if (y > bottom - 30) break;
      ctx.fillStyle = perHue(t.id); ctx.beginPath(); ctx.arc(px + 6, y + 9 * big, 5 * big, 0, 7); ctx.fill();
      const [line, size] = fitText(t.text, 15 * big, pw - 20);
      ctx.textAlign = "left"; ctx.fillStyle = perHue(t.id); ctx.fillText(line, px + 18, y + 9 * big + size / 3);
      y += 16 * big;
      const [cw, ch] = drawCard(t.tree, px + 18, y, pw - 20, Math.max(20, each - 26 * big), 6, perHue(t.id) + "", 1);
      if (t.depth) { ctx.font = `${10 * big}px ui-monospace, monospace`; ctx.fillStyle = "#8595ad"; ctx.fillText(`stack ${t.depth} deep`, px + 24 + cw, y + 10 * big); }
      y += ch + 10 * big;
    }
    if (PER.forks.length) {
      y += 4;
      head(`${PER.forks.length} pair${PER.forks.length > 1 ? "s" : ""} waiting for ${PER.forks.length > 1 ? "their" : "its"} halves`);
      for (const f of PER.forks) {
        if (y > bottom - 20) break;
        const [cw, ch] = drawCard(f.tree, px, y, pw / 2, 46 * big, 5, "#7ee78799", 1);
        ctx.font = `${10 * big}px ui-monospace, monospace`; ctx.fillStyle = "#7ee787"; ctx.textAlign = "left";
        ctx.fillText(f.slots.map(s => ["away", "back (start)", "back"][s]).join(" · "), px + cw + 8, y + 12 * big);
        y += ch + 6;
      }
    }
  }
  ctx.setLineDash([4, 4]); ctx.strokeStyle = "#ffa94d77"; ctx.lineWidth = 1;
  for (const [sx, sy, call] of links) {
    const p = P(call); if (!p) continue;
    const cx = view.ox + p[0] * view.s, cy = view.oy + p[1] * view.s;
    if (cx < 0 || cx > px - 0.015 * r.width || cy < 0 || cy > r.height) continue;
    ctx.beginPath(); ctx.moveTo(sx, sy); ctx.bezierCurveTo(sx - 80, sy, cx + 60, cy, cx, cy); ctx.stroke();
  }
  ctx.setLineDash([]);
  ctx.setTransform(d * view.s, 0, 0, d * view.s, d * view.ox, d * view.oy);
}
/// The bar, along the moves of the run made forward by rounds: each round's width its moves, its height
/// how many tokens there were after it; bright up to where the run is now.
function perStrip(W, H, d) {
  if (!PER.ok) return;
  const n = PER.rounds, pr = PER.prof, total = Math.max(1, pr[6 * n + 3]);
  for (let i = 1; i <= n; i++) {
    const a = pr[6 * (i - 1) + 3], b = pr[6 * i + 3], x0 = a / total * W, x1 = Math.max(x0 + 1, b / total * W);
    const h = 3 * d + (H - 9 * d) * pr[6 * i] / PER.most;
    sctx.fillStyle = b <= PER.clock ? "#7ee787" : "#24402c"; sctx.fillRect(x0, H - h, x1 - x0, h);
  }
}
function perHud() {
  if (!PER.ok) return;
  const at = PER.clock === 0 ? 0 : PER.clock === PER.n ? 1 : -1, t = PER.toks[0];
  const tree = PER.toks.length === 1 && PER.dataN < 2500 ? parseTreeText(text(E.walk_show(t.payload))) : null;
  const read = tree && twoReading(PER, tree, at), line = PER.toks.length === 1 ? (t.text.length > 70 ? t.text.slice(0, 69) + "…" : t.text) : `${PER.toks.length} tokens`;
  $("answer").textContent = read !== null && read !== undefined ? `${read} = ${line}` : line;
  $("status").textContent = PER.err || (at === 0 ? "the beginning: the input" : at === 1 ? "the end: the answer" : `${PER.clock} of ${PER.n} moves made, ${PER.forward ? "going forward" : "going back"}`);
  $("status").className = PER.err && !/^replayed/.test(PER.err) ? "bad" : at >= 0 ? "ok" : "";
  $("counts").innerHTML = [
    ["moves made", `${PER.clock} of ${PER.n}`], ["…in rounds, going forward", `${PER.rounds}`], ["tokens now", `${PER.toks.length} (at most ${PER.most})`],
    ["pairs waiting for halves", PER.forks.length], ["the program's agents", `${PER.codeN}, never changed`], ["agents the tokens carry", PER.dataN],
    ["…on stacks, and forks", PER.stackN], ["nodes whose rule has fired", PER.seen.size],
  ].map(([k, v]) => `<div><span>${k}</span><span>${v}</span></div>`).join("");
}
function perPointed(wx, wy) {
  for (const t of PER.toks) { const tp = PER.shown.get(t.id); if (tp && (wx - tp[0]) ** 2 + (wy - tp[1]) ** 2 < 30) return { token: t.id }; }
  const pl = perPlaces().forks;
  for (const f of PER.forks) { const p = pl.get(f.id); if (p && (wx - p[0]) ** 2 + (wy - p[1]) ** 2 < 50) return { fork: f.id }; }
  let best = null, bd = (PG.r * 2) ** 2;
  for (const [id, p] of PER.pos) { const dd = (p[0] - wx) ** 2 + (p[1] - wy) ** 2; if (dd < bd) { bd = dd; best = { per: id }; } }
  return best;
}
function perDetail(hit) {
  if (hit?.token !== undefined) {
    const t = PER.toks.find(t => t.id === hit.token);
    return `<b>a token</b><div>carries ${t ? twoText(parseTreeText(text(E.walk_show(t.payload)))).slice(0, 80) : "a tree"}; its stack ${t?.depth ?? 0} deep${t?.half >= 0 ? ", the bottom of it the pair it is a half of" : ""}. Click it to turn time around.</div>`;
  }
  if (hit?.fork !== undefined) {
    const f = PER.forks.find(f => f.id === hit.fork);
    return `<b>a fork</b>: a pair whose halves went their own ways at a both<div>${f ? f.slots.map((s, i) => `${i ? "right" : "left"} half ${["away (a token has it)", "back, at its start", "back, done"][s]}`).join("; ") : ""}. With both halves back it is one token again.</div>`;
  }
  const x = hit && PER.code?.get(hit.per);
  if (!x) return "point at an agent of the program, a token or a fork";
  const name = ["", "a leaf", "a stem", "a fork"][x.kind];
  if (x.what < 0) return `<b>${name}</b> of the program<div>part of how an instruction is written down: no rule fires here</div>`;
  const role = x.what & 15, seen = PER.seen.get(x.id) ?? 0;
  return `<div><span class="sw" style="background:${TWO_KIND[role][1]}"></span><b>${name}</b>: by its shape, a <b>${PER_ROLE[role]}</b>${x.what >= 16 ? " (backward)" : ""}</div>`
    + `<div style="font-family:var(--mono)">${perSays(x)}</div><div>${TWO_KIND[role][2]}</div>`
    + `<div style="color:var(--fg-muted)">${seen ? `its rule has fired ${seen} time${seen > 1 ? "s" : ""} by now` : "its rule has not fired by now"}</div>`;
}
const PERV = { st: PER, load: perLoad, sync: perSync, hud: perHud, draw: perDraw, strip: perStrip, pointed: perPointed, detail: perDetail,
  go: perGo, fwd: k => perMove(k, true), bwd: k => perMove(k, false), box: perBox, tokenAt: perTokenAt, examples: PER_EXAMPLES, follow: 0.8, panel: 0.36 };
