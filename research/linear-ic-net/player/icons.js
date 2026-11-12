// How each kind of agent is drawn: what it really does, four ways to draw it, and which one is in use.
// Shared by index.html (the player) and icons.html (where the choice is made; it is kept in localStorage).
"use strict";
(function (root) {

const DEG = Math.PI / 180, polar = (deg, r) => [Math.cos(deg * DEG) * r, Math.sin(deg * DEG) * r];
/// A regular polygon of n corners at radius r, the first at angle `from` (degrees, clockwise from east).
const ngon = (n, r, from) => Array.from({ length: n }, (_, i) => polar(from + 360 * i / n, r)).flat();
/// Arms of half-width w from the middle out to radius len, at the given angles, as one polygon.
function arms(angles, len, w) {
  const out = [];
  angles.forEach((a, i) => {
    const b = i + 1 < angles.length ? angles[i + 1] : angles[0] + 360, c = Math.cos(a * DEG), s = Math.sin(a * DEG), j = w / Math.sin((b - a) * DEG / 2);
    out.push(len * c + w * s, len * s - w * c, len * c - w * s, len * s + w * c, j * Math.cos((a + b) * DEG / 2), j * Math.sin((a + b) * DEG / 2));
  });
  return out;
}
const arc = (cx, cy, r, a0, a1, n = 12) => Array.from({ length: n + 1 }, (_, i) => { const a = (a0 + (a1 - a0) * i / n) * DEG; return [cx + r * Math.cos(a), cy + r * Math.sin(a)]; }).flat();
/// A rectangle of half-width hw and half-height hh with corners rounded to r, about (cx, cy).
const rrect = (hw, hh, r, cx = 0, cy = 0) => [...arc(cx + hw - r, cy - hh + r, r, -90, 0, 4), ...arc(cx + hw - r, cy + hh - r, r, 0, 90, 4), ...arc(cx - hw + r, cy + hh - r, r, 90, 180, 4), ...arc(cx - hw + r, cy - hh + r, r, 180, 270, 4)];
const scale = (p, k, ky = k) => p.map((v, i) => i % 2 ? v * ky : v * k);
const TRI = [0, -1.15, 0.98, 0.72, -0.98, 0.72], DOWN = scale(TRI, 1, -1), DIAMOND = [0, -1.18, 0.95, 0, 0, 1.18, -0.95, 0];
const TILE = rrect(0.95, 0.95, 0.3), DOME = [...arc(0, 0.45, 1.08, 180, 360, 16)];
const ALMOND = [...arc(0, 0.85, 1.45, 216, 324, 10), ...arc(0, -0.85, 1.45, 36, 144, 10)];
const ARROW = [-0.34, -1.0, 0.34, -1.0, 0.34, 0.05, 0.95, 0.05, 0, 1.1, -0.95, 0.05, -0.34, 0.05];
const DROP = [0, -1.2, ...arc(0, 0.25, 0.82, -35, 215, 14)];
const SPARK = ngon(8, 1.1, -90).map((v, i) => i % 4 < 2 ? v : v * 0.45);
const CHEVRON = (y) => [-0.85, y - 0.45, 0, y + 0.3, 0.85, y - 0.45, 0.85, y - 0.05, 0, y + 0.7, -0.85, y - 0.05];

/// A port's pin: the side of the icon the port is on, from near the middle out to the tip a wire joins.
const pin = (from, tip, w = 0.1) => ({ from, tip, w });
const ray = (deg, r0, r1, w) => pin(polar(deg, r0), polar(deg, r1), w);
const FUN = [pin([0, -0.6], [0, -1.7]), pin([-0.6, 0.5], [-1.65, 0.5]), pin([0, 0.5], [0, 1.6])];
const Y_UP = (w, r) => [ray(-90, 0, r, w), ray(125, 0, r, w), ray(55, 0, r, w)], Y_DOWN = (w, r) => [ray(90, 0, r, w), ray(-125, 0, r, w), ray(-55, 0, r, w)];
/// Each kind's pins, the main port first. A pin's side is a property of the kind, whatever the icon:
/// a function's reader, its variable, its body; an application's reader, function, argument; and so on.
const PINS = {
  root: [ray(90, 0.6, 1.6)],
  lam: FUN, Lam: FUN,
  app: [ray(-90, 0.3, 1.45), ray(125, 0.3, 1.45), ray(55, 0.3, 1.45)],
  var: [ray(-90, 0.2, 1.05), ray(135, 0.2, 1.05)],
  rho: [ray(-90, 0.5, 1.55)],
  O: [ray(125, 0.5, 1.6), ray(55, 0.5, 1.6), ray(-90, 0.5, 1.6)],
  T: [ray(90, 0.5, 1.65), ray(-90, 0.5, 1.65)], R: [ray(90, 0.6, 1.75), ray(-90, 0.6, 1.75)],
  sig: Y_UP(0.16, 1.35), dupI: Y_DOWN(0.25, 1.45), dupO: Y_DOWN(0.25, 1.45),
  eraI: [ray(90, 0.2, 1.5)], eraO: [ray(90, 0.2, 1.5)],
  pod: [ray(-90, 0.3, 1.9)],
};

const INK = "#0b0e14";
const glyph = (text, size = 1.45, color = INK, dy = 0) => ({ text, size, color, dy });
/// What each kind of agent does, and four ways to draw it: a colour, shapes to fill (`parts`: polygons
/// [x0, y0, …] and disks [cx, cy, r]), shapes cut out of them (`cut`), and maybe a letter (`glyph`; with no
/// colour of its own it takes the icon's). `bare`: no body, so the pins stop short of the letter.
const ICONS = {
  lam: { title: "function", does: "A function, shut. It does nothing until a running application meets it: then its variable is wired to the argument, and a thaw starts on its body.", pick: 0, options: [
    { name: "λ on a tile", color: "#dfe4ea", parts: [TILE], glyph: glyph("λ") },
    { name: "triangle", color: "#dfe4ea", parts: [TRI] },
    { name: "dome over its body", color: "#f2cc8f", parts: [DOME], glyph: glyph("λ", 1.0, INK, -0.12) },
    { name: "λ alone", color: "#ffffff", parts: [], bare: 0.85, glyph: glyph("λ", 1.9, null) },
  ] },
  Lam: { title: "opened function", does: "A function with its lid off. Applied, it hands its body out as code instead of running it. Reading a function opens it, and reading an opened one shuts it again.", pick: 0, options: [
    { name: "Λ on an open tile", color: "#dfe4ea", parts: [TILE], cut: [rrect(0.72, 0.72, 0.18)], glyph: glyph("Λ", 1.15, null) },
    { name: "hollow triangle", color: "#dfe4ea", parts: [TRI], cut: [TRI.map((v, i) => i % 2 ? (v - 0.1) * 0.55 + 0.1 : v * 0.55)] },
    { name: "open dome", color: "#f2cc8f", parts: [DOME], cut: [[...arc(0, 0.45, 0.8, 180, 360, 14)].map((v, i) => i % 2 ? v - 0.02 : v)], glyph: glyph("Λ", 0.8, null, -0.05) },
    { name: "Λ alone", color: "#ffffff", parts: [], bare: 0.85, glyph: glyph("Λ", 1.9, null) },
  ] },
  app: { title: "application (code)", does: "An application written down: a function part and an argument part. It is frozen, and does nothing until a thaw reaches it.", pick: 0, options: [
    { name: "small grey disk", color: "#7c8694", parts: [[0, 0, 0.62]] },
    { name: "@", color: "#8a96a8", parts: [], bare: 0.7, glyph: glyph("@", 1.5, null) },
    { name: "grey pill", color: "#7c8694", parts: [rrect(0.85, 0.45, 0.42)] },
    { name: "small square", color: "#6b7689", parts: [rrect(0.55, 0.55, 0.12)] },
  ] },
  O: { title: "running application", does: "An application running. It waits for its function part to become a value, then calls it, opens it or reads with it.", pick: 1, options: [
    { name: "pink disk", color: "#e86bd8", parts: [[0, 0, 0.9]] },
    { name: "@ lit", color: "#e86bd8", parts: [[0, 0, 0.95]], glyph: glyph("@", 1.25) },
    { name: "pink pill", color: "#ff7ab8", parts: [rrect(1.0, 0.6, 0.55)] },
    { name: "ring, waiting", color: "#e86bd8", parts: [[0, 0, 0.95]], cut: [[0, 0, 0.55]] },
  ] },
  var: { title: "use of a variable", does: "One use of a variable: a hole in the code. Its second wire leads to the binder, or, once the function is called, to the value.", pick: 0, options: [
    { name: "dot", color: "#aab3bf", parts: [[0, 0, 0.36]] },
    { name: "small ring: a hole", color: "#aab3bf", parts: [[0, 0, 0.48]], cut: [[0, 0, 0.26]] },
    { name: "x", color: "#aab3bf", parts: [], bare: 0.55, glyph: glyph("x", 1.3, null, -0.05) },
    { name: "small diamond", color: "#aab3bf", parts: [scale(DIAMOND, 0.45)] },
  ] },
  rho: { title: "ρ, the reader at rest", does: "ρ as a value. Applied to something, it becomes a reader of it.", pick: 1, options: [
    { name: "violet diamond", color: "#c792ea", parts: [scale(DIAMOND, 0.82)] },
    { name: "ρ on a disk", color: "#c792ea", parts: [[0, 0, 0.9]], glyph: glyph("ρ", 1.3, INK, -0.08) },
    { name: "eye shut", color: "#c792ea", parts: [scale(ALMOND, 0.9, 0.45)] },
    { name: "ρ alone", color: "#c792ea", parts: [], bare: 0.8, glyph: glyph("ρ", 1.8, null, -0.1) },
  ] },
  R: { title: "reader", does: "A reader at work. It waits for the value it reads, then builds a function of five arms and hands the value's parts to the arm for its kind.", pick: 1, options: [
    { name: "cyan diamond", color: "#56c8ff", parts: [DIAMOND] },
    { name: "ρ lit", color: "#56c8ff", parts: [[0, 0, 0.98]], glyph: glyph("ρ", 1.3, INK, -0.08) },
    { name: "eye open", color: "#56c8ff", parts: [scale(ALMOND, 0.95)], cut: [[0, 0, 0.36]] },
    { name: "magnifier", color: "#56c8ff", parts: [[-0.12, -0.12, 0.82], [0.35, 0.55, 0.55, 0.35, 1.1, 0.9, 0.9, 1.1]], cut: [[-0.12, -0.12, 0.52]] },
  ] },
  T: { title: "thaw", does: "A thaw. It walks down written code and sets it running, one agent at a time, and stops at a function: the body stays frozen.", pick: 1, options: [
    { name: "green hexagon", color: "#6ee787", parts: [ngon(6, 0.95, -90)] },
    { name: "arrow at the code", color: "#6ee787", parts: [ARROW] },
    { name: "drop: melting", color: "#4fd6be", parts: [DROP] },
    { name: "spark", color: "#b8f06a", parts: [SPARK] },
  ] },
  sig: { title: "split", does: "The one place a variable used twice divides. When a value arrives it starts a copy of it.", pick: 0, options: [
    { name: "thin Y", color: "#8a7fbf", parts: [[0, 0, 0.3]] },
    { name: "node with prongs", color: "#8a7fbf", parts: [[0, 0, 0.5]] },
    { name: "2 on a disk", color: "#8a7fbf", parts: [[0, 0, 0.62]], glyph: glyph("2", 0.9) },
    { name: "small ring", color: "#8a7fbf", parts: [[0, 0, 0.55]], cut: [[0, 0, 0.3]] },
  ] },
  dupI: { title: "copier", does: "A copy under way. It walks through a value one agent at a time and leaves two of each; where the two ends of one copy meet, they cancel.", pick: 0, options: [
    { name: "thick Y", color: "#ffa94d", parts: [[0, 0, 0.44]] },
    { name: "δ in a triangle", color: "#ffa94d", parts: [scale(DOWN, 0.9)], glyph: glyph("δ", 1.0, INK, -0.25) },
    { name: "two squares", color: "#ffa94d", parts: [rrect(0.5, 0.5, 0.1, -0.28, -0.28), rrect(0.5, 0.5, 0.1, 0.28, 0.28)] },
    { name: "double chevron", color: "#ffa94d", parts: [CHEVRON(-0.45), CHEVRON(0.35)] },
  ] },
  eraI: { title: "eraser", does: "An eraser. It walks through a value nobody uses and removes it, one agent at a time.", pick: 0, options: [
    { name: "red ×", color: "#ff6b6b", parts: [arms([45, 135, 225, 315], 0.85, 0.2)] },
    { name: "ε on a disk", color: "#ff6b6b", parts: [[0, 0, 0.85]], glyph: glyph("ε", 1.3, INK, -0.1) },
    { name: "∅ alone", color: "#ff6b6b", parts: [], bare: 0.75, glyph: glyph("∅", 1.7, null) },
    { name: "red ring: a hole", color: "#ff6b6b", parts: [[0, 0, 0.8]], cut: [[0, 0, 0.5]] },
  ] },
  root: { title: "root", does: "Where the program's answer arrives.", pick: 0, options: [
    { name: "ring round a dot", color: "#ffffff", parts: [[0, 0, 1.05], [0, 0, 0.42]], cut: [[0, 0, 0.75]] },
    { name: "white disk", color: "#ffffff", parts: [[0, 0, 0.85]] },
    { name: "frame round a dot", color: "#ffffff", parts: [rrect(1.0, 1.0, 0.2), [0, 0, 0.36]], cut: [rrect(0.72, 0.72, 0.1)] },
    { name: "◎ alone", color: "#ffffff", parts: [], bare: 0.9, glyph: glyph("◎", 2.0, null) },
  ] },
};
ICONS.dupO = { ...ICONS.dupI, same: "dupI" }; ICONS.eraO = { ...ICONS.eraI, same: "eraI" };
/// The kinds in the order the lab and the legend show them.
const ORDER = ["root", "lam", "Lam", "app", "var", "rho", "T", "O", "R", "sig", "dupI", "eraI"];

const signedArea = p => { let a = 0; for (let i = 0; i < p.length; i += 2) { const j = (i + 2) % p.length; a += p[i] * p[j + 1] - p[j] * p[i + 1]; } return a; };
/// Polygons wound one way for a fill and the other for a hole, so holes cut under the nonzero rule.
function wind(p, hole) {
  if (p.length === 3 || (signedArea(p) > 0) !== hole) return p;
  const out = []; for (let i = p.length - 2; i >= 0; i -= 2) out.push(p[i], p[i + 1]); return out;
}
/// A kind drawn one of its ways, ready to draw: fills (its pins among them), cuts, and for each port where
/// its wire joins, the way it leaves, and that way as an angle.
function compile(kind, i) {
  const icon = ICONS[kind], opt = kind === "pod" ? { name: "pod", color: "#2f6f4f", parts: [] } : icon.options[i], bare = opt.bare ?? 0;
  const pins = PINS[kind].map(({ from, tip, w }) => {
    let fx = from[0], fy = from[1]; const dx = tip[0] - fx, dy = tip[1] - fy, l = Math.hypot(dx, dy);
    // with no body, a pin starts clear of the letter
    if (bare && Math.hypot(fx, fy) < bare) { const t = Math.min(0.8, (bare - Math.hypot(fx, fy)) / l); fx += dx * t; fy += dy * t; }
    const nx = -dy / l * w, ny = dx / l * w;
    return [fx + nx, fy + ny, tip[0] + nx, tip[1] + ny, tip[0] - nx, tip[1] - ny, fx - nx, fy - ny];
  });
  return { kind, option: i, name: opt.name, color: opt.color, glyph: opt.glyph ?? null,
    fill: [...pins, ...opt.parts].map(p => wind(p, false)), cut: (opt.cut ?? []).map(p => wind(p, true)),
    ports: PINS[kind].map(({ from, tip }) => { const dx = tip[0] - from[0], dy = tip[1] - from[1], l = Math.hypot(dx, dy); return [tip[0], tip[1], dx / l, dy / l, Math.atan2(dy, dx)]; }) };
}

// ---- which way each kind is drawn ----
const KEY = "rln.icons";
function choices() {
  const out = {};
  for (const k of Object.keys(ICONS)) out[k] = ICONS[k].pick;
  try { const saved = JSON.parse(root.localStorage?.getItem(KEY) ?? "{}"); for (const k of Object.keys(saved)) if (ICONS[k] && ICONS[k].options[saved[k]]) out[k] = saved[k]; } catch (e) {}
  out.dupO = out.dupI; out.eraO = out.eraI;
  return out;
}
function choose(kind, i) {
  const now = choices(); now[kind] = i;
  try { root.localStorage?.setItem(KEY, JSON.stringify(now)); } catch (e) {}
}
function reset() { try { root.localStorage?.removeItem(KEY); } catch (e) {} }
/// Every kind as it is drawn now, by name.
function current() {
  const pick = choices(), out = { pod: compile("pod", 0) };
  for (const k of Object.keys(ICONS)) out[k] = compile(k, pick[k]);
  return out;
}

// ---- drawing ----
/// One part of a shape into a path (anything with moveTo, lineTo, arc, closePath): about (x, y), r across,
/// turned by th, and mirrored left to right when fl is negative (fl between -1 and 1 squashes it on the way).
function partInto(path, p, x, y, r, c, s, fl, hole) {
  if (p.length === 3) {
    const cx = x + r * (c * p[0] * fl - s * p[1]), cy = y + r * (s * p[0] * fl + c * p[1]);
    path.moveTo(cx + r * p[2], cy); path.arc(cx, cy, r * p[2], 0, 2 * Math.PI, hole);
    return;
  }
  // a mirrored polygon is walked backward, so it stays wound the way it was
  const n = p.length, at = k => fl < 0 ? n - 2 - k : k;
  for (let k = 0; k < n; k += 2) { const i = at(k), px = p[i] * fl, py = p[i + 1], wx = x + r * (c * px - s * py), wy = y + r * (s * px + c * py); if (k) path.lineTo(wx, wy); else path.moveTo(wx, wy); }
  path.closePath();
}
function shapeInto(path, sh, x, y, r, th = 0, fl = 1, holes = true) {
  const c = Math.cos(th), s = Math.sin(th);
  for (const p of sh.fill) partInto(path, p, x, y, r, c, s, fl, false);
  if (holes) for (const p of sh.cut) partInto(path, p, x, y, r, c, s, fl, true);
}
/// The tip of port q, and the way its wire leaves: [x, y, dx, dy].
function tipAt(sh, q, x, y, th, r, fl = 1) {
  const p = sh.ports[q], c = Math.cos(th), s = Math.sin(th), lx = p[0] * fl, ly = p[1];
  let dx = p[2] * fl, dy = p[3]; const l = Math.hypot(dx, dy) || 1; dx /= l; dy /= l;
  return [x + r * (c * lx - s * ly), y + r * (s * lx + c * ly), c * dx - s * dy, s * dx + c * dy];
}
/// The turn and the flip that best face an icon's pins toward what they are wired to. dirs[q] is the angle
/// from the agent to what port q is wired to (undefined if nothing), w[q] how much it counts; `was` is
/// the flip it has, kept unless the other is clearly better.
function bestFit(sh, dirs, w, was = 1) {
  let best = null;
  for (const flip of [1, -1]) {
    let sx = 0, sy = 0, tot = 0;
    for (let q = 0; q < sh.ports.length; q++) {
      if (dirs[q] === undefined) continue;
      const a = dirs[q] - (flip > 0 ? sh.ports[q][4] : Math.PI - sh.ports[q][4]);
      sx += w[q] * Math.cos(a); sy += w[q] * Math.sin(a); tot += w[q];
    }
    const score = Math.hypot(sx, sy) + (flip === was ? 0.15 * tot : 0);
    if (!best || score > best.score) best = { score, th: Math.atan2(sy, sx), flip, any: Math.hypot(sx, sy) > 1e-6 };
  }
  return best;
}
/// A path as SVG text, taking the calls a canvas path takes.
class SvgPath {
  constructor() { this.d = ""; }
  moveTo(x, y) { this.d += `M${+x.toFixed(2)} ${+y.toFixed(2)}`; }
  lineTo(x, y) { this.d += `L${+x.toFixed(2)} ${+y.toFixed(2)}`; }
  closePath() { this.d += "Z"; }
  bezierCurveTo(a, b, c, d, x, y) { this.d += `C${+a.toFixed(2)} ${+b.toFixed(2)} ${+c.toFixed(2)} ${+d.toFixed(2)} ${+x.toFixed(2)} ${+y.toFixed(2)}`; }
  arc(cx, cy, r, a0, a1, hole) { const f = v => +v.toFixed(2), sw = hole ? 0 : 1; this.d += `M${f(cx + r)} ${f(cy)}A${f(r)} ${f(r)} 0 1 ${sw} ${f(cx - r)} ${f(cy)}A${f(r)} ${f(r)} 0 1 ${sw} ${f(cx + r)} ${f(cy)}Z`; }
}
const FONT = `"DejaVu Sans", "Noto Sans", "Segoe UI Symbol", system-ui, sans-serif`;
/// A shape as SVG: its body turned and flipped, its letter upright, a dot on its main pin.
function svgAt(sh, x, y, r, th = 0, fl = 1, colour = sh.color, dot = "#ffffff") {
  const path = new SvgPath(), m = tipAt(sh, 0, x, y, th, r, fl);
  shapeInto(path, sh, x, y, r, th, fl, true);
  const g = sh.glyph, text = g ? `<text x="${x.toFixed(2)}" y="${(y + (g.dy ?? 0) * r).toFixed(2)}" text-anchor="middle" dominant-baseline="central" font-family='${FONT}' font-weight="700" font-size="${(g.size * r).toFixed(2)}" fill="${g.color ?? colour}">${g.text}</text>` : "";
  return `<path d="${path.d}" fill="${colour}"/>${text}${dot ? `<circle cx="${m[0].toFixed(2)}" cy="${m[1].toFixed(2)}" r="${(0.24 * r).toFixed(2)}" fill="${dot}"/>` : ""}`;
}
/// A kind's icon as an inline SVG, px pixels square.
function iconSVG(sh, px = 20, colour = sh.color) {
  return `<svg class="kind" width="${px}" height="${px}" viewBox="-2 -2 4 4" aria-hidden="true">${svgAt(sh, 0, 0, 1, 0, 1, colour)}</svg>`;
}

const api = { ICONS, ORDER, PINS, compile, choices, choose, reset, current, shapeInto, tipAt, bestFit, SvgPath, svgAt, iconSVG, FONT };
if (typeof module !== "undefined" && module.exports) module.exports = api; else root.ICN = api;
})(typeof globalThis !== "undefined" ? globalThis : this);
