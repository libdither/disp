// What kind an agent is, as its shape (index.html, graph.js), so colour can say where it is in the
// term. One family per role, the same in the flat views, the graph view and the legend, and as
// solids in 3D: values (leaf, stem, fork) are triangles △ with a hole for each child, pointing at
// their principal port; applications are disks, a suspended one hollow; choices (triage and its
// second dispatch) are diamonds; the pair a choice carries is a pill, and the unpair a pill split in
// two; a duplicator is a Y, an eraser a ×, a normalizer a hexagon; the root is a ring around a dot;
// a superposition (one value standing for two) is two triangles overlapping.

/// A regular polygon [x0, y0, x1, y1, …] of n corners at radius r, the first at angle `from` (degrees, clockwise from east).
const ngon = (n, r, from) => Array.from({ length: n }, (_, i) => [Math.cos((from + 360 * i / n) * Math.PI / 180) * r, Math.sin((from + 360 * i / n) * Math.PI / 180) * r]).flat();
/// Arms of half-width w from the middle out to radius len, at the given angles (degrees), as one polygon.
function arms(angles, len, w) {
  const out = [], rad = a => a * Math.PI / 180;
  angles.forEach((a, i) => {
    const b = i + 1 < angles.length ? angles[i + 1] : angles[0] + 360, c = Math.cos(rad(a)), s = Math.sin(rad(a)), j = w / Math.sin(rad(b - a) / 2);
    out.push(len * c + w * s, len * s - w * c, len * c - w * s, len * s + w * c, j * Math.cos(rad((a + b) / 2)), j * Math.sin(rad((a + b) / 2)));
  });
  return out;
}
/// Points along an arc (degrees), for curves inside a polygon.
const arcPts = (cx, cy, r, a0, a1, n) => Array.from({ length: n + 1 }, (_, i) => { const a = (a0 + (a1 - a0) * i / n) * Math.PI / 180; return [cx + r * Math.cos(a), cy + r * Math.sin(a)]; }).flat();
const TRI = [0, -1.15, 0.98, 0.72, -0.98, 0.72], HALF = 0.13;
/// Each kind's shape, by tag, in a circle of radius about 1 with its principal port on top (y grows
/// downward, as on a canvas): `fill`, `cut` (holes, seen through) and `mark` (holes drawn dark, so a
/// wire under them does not show) list polygons [x0, y0, x1, y1, …] and circles [cx, cy, r].
/// `turn`: in the graph view, turned so the principal port faces its wire.
const SHAPE = [];
SHAPE[1] = { fill: [TRI], turn: true };
SHAPE[2] = { fill: [TRI], mark: [[0, 0.24, 0.3]], turn: true };
SHAPE[3] = { fill: [TRI], mark: [[-0.37, 0.33, 0.23], [0.37, 0.33, 0.23]], turn: true };
SHAPE[4] = { fill: [[0, 0, 1]], cut: [[0, 0, 0.56]] };
SHAPE[5] = { fill: [[...arcPts(0.42, 0, 0.62, -90, 90, 8), ...arcPts(-0.42, 0, 0.62, 90, 270, 8)]], turn: true };
SHAPE[6] = { fill: [[0, 0, 0.95]] };
SHAPE[7] = { fill: [[0, -1.18, 0.95, 0, 0, 1.18, -0.95, 0]] };
SHAPE[8] = { fill: [[0, -1.18, 0.95 * (1 - HALF / 1.18), -HALF, -0.95 * (1 - HALF / 1.18), -HALF], [0, 1.18, -0.95 * (1 - HALF / 1.18), HALF, 0.95 * (1 - HALF / 1.18), HALF]] };
SHAPE[9] = { fill: [[...arcPts(0.4, 0, 0.6, -90, 90, 7), 0.13, 0.6, 0.13, -0.6], [...arcPts(-0.4, 0, 0.6, 90, 270, 7), -0.13, -0.6, -0.13, 0.6]], turn: true };
SHAPE[10] = { fill: [arms([-90, 30, 150], 1.12, 0.27)], turn: true };
SHAPE[11] = { fill: [arms([45, 135, 225, 315], 1.02, 0.24)] };
SHAPE[12] = { fill: [ngon(6, 1, -90)] };
SHAPE[13] = { fill: [[0, 0, 1.12], [0, 0, 0.44]], cut: [[0, 0, 0.8]] };
// A superposition: one value standing for two, as two triangles overlapping, the overlap cut out.
SHAPE[14] = { fill: [[-1.05, 0.72, -0.4, -1.15, 0, -0.4, 0.4, -1.15, 1.05, 0.72]], cut: [[0, 0.02, 0.36, 0.56, -0.36, 0.56]], turn: true };
/// A kind with no shape of its own yet.
const SHAPE_ANY = { fill: [[0, 0, 0.9]] };
/// The spark on a wire joining two agents about to rewrite (graph.js).
const SPARK = ngon(8, 1, -90).map((v, i) => i % 4 < 2 ? v : v * 0.38);

const signedArea = p => { let a = 0; for (let i = 0; i < p.length; i += 2) { const j = (i + 2) % p.length; a += p[i] * p[j + 1] - p[j] * p[i + 1]; } return a; };
/// Polygons wound one way for a fill, the other for a hole, so holes cut under the nonzero rule.
function wind(p, hole) {
  if (p.length === 3 || (signedArea(p) > 0) !== hole) return p;
  const out = [];
  for (let i = p.length - 2; i >= 0; i -= 2) out.push(p[i], p[i + 1]);
  return out;
}
const insidePart = (p, x, y) => {
  if (p.length === 3) return (x - p[0]) ** 2 + (y - p[1]) ** 2 <= p[2] ** 2;
  let c = false;
  for (let i = 0, j = p.length - 2; i < p.length; j = i, i += 2)
    if ((p[i + 1] > y) !== (p[j + 1] > y) && x < (p[j] - p[i]) * (y - p[i + 1]) / (p[j + 1] - p[i + 1]) + p[i]) c = !c;
  return c;
};
/// How far each shape reaches toward each of RIMS directions (clockwise from east): where a port sits on its rim.
const RIMS = 64;
for (const sh of [...SHAPE, SHAPE_ANY]) {
  if (!sh) continue;
  sh.fill = sh.fill.map(p => wind(p, false)); sh.cut = (sh.cut ?? []).map(p => wind(p, true));
  sh.markCut = (sh.mark ?? []).map(p => wind(p, true)); sh.mark = (sh.mark ?? []).map(p => wind(p, false));
  sh.rim = Float32Array.from({ length: RIMS }, (_, k) => {
    const a = 2 * Math.PI * k / RIMS;
    let t = 1.4; while (t > 0 && !sh.fill.some(p => insidePart(p, t * Math.cos(a), t * Math.sin(a)))) t -= 0.02;
    return t;
  });
}
const shapeOf = t => SHAPE[t] ?? SHAPE_ANY;
/// How far a kind's shape reaches at angle a (radians, clockwise from east, in its own upright frame).
const rimAt = (t, a) => shapeOf(t).rim[((Math.round(a / (2 * Math.PI) * RIMS) % RIMS) + RIMS) % RIMS];

/// One part of a shape (polygon or circle) into a path: centred at (x, y), r across, its top turned to
/// the unit vector (ux, uy).
function partInto(path, p, x, y, r, ux, uy, hole) {
  const c = -uy, s = ux;
  if (p.length === 3) {
    const cx = x + r * (c * p[0] - s * p[1]), cy = y + r * (s * p[0] + c * p[1]);
    path.moveTo(cx + r * p[2], cy); path.arc(cx, cy, r * p[2], 0, 2 * Math.PI, hole);
    return;
  }
  path.moveTo(x + r * (c * p[0] - s * p[1]), y + r * (s * p[0] + c * p[1]));
  for (let i = 2; i < p.length; i += 2) path.lineTo(x + r * (c * p[i] - s * p[i + 1]), y + r * (s * p[i] + c * p[i + 1]));
  path.closePath();
}
/// A kind's shape into a path (anything with moveTo, lineTo, arc and closePath), at (x, y), r across,
/// upright or with its top turned to (ux, uy); without holes when they would be too small to see.
/// Its marks go into `marks`, to be filled dark over it, or without that are cut out too.
function shapeInto(path, t, x, y, r, ux = 0, uy = -1, holes = true, marks = null) {
  const sh = shapeOf(t);
  for (const p of sh.fill) partInto(path, p, x, y, r, ux, uy, false);
  if (!holes) return;
  for (const p of sh.cut) partInto(path, p, x, y, r, ux, uy, true);
  if (marks) for (const p of sh.mark) partInto(marks, p, x, y, r, ux, uy, false);
  else for (const p of sh.markCut) partInto(path, p, x, y, r, ux, uy, true);
}
/// A kind's shape as an inline SVG icon, px pixels square, in its colour.
function shapeSVG(t, colour, px = 15) {
  const sh = shapeOf(t), f = v => +v.toFixed(3);
  const part = (p, hole) => p.length === 3
    ? `M${f(p[0] + p[2])} ${f(p[1])}A${f(p[2])} ${f(p[2])} 0 1 ${hole ? 0 : 1} ${f(p[0] - p[2])} ${f(p[1])}A${f(p[2])} ${f(p[2])} 0 1 ${hole ? 0 : 1} ${f(p[0] + p[2])} ${f(p[1])}Z`
    : `M${p.map(f).join(" ").replace(/^(\S+ \S+) /, "$1L")}Z`;
  const d = sh.fill.map(p => part(p, false)).join("") + [...sh.cut, ...sh.markCut].map(p => part(p, true)).join("");
  return `<svg class="kind" width="${px}" height="${px}" viewBox="-1.32 -1.32 2.64 2.64" aria-hidden="true"><path d="${d}" fill="${colour}"/></svg>`;
}

/// Each kind's solid for the 3D view (three.js as T), about 1 across with its principal port on top
/// (y up): pyramids for values, a knob under one for each child; a sphere for an apply; a ring for a
/// suspension; an octahedron for triage, split for its dispatch; a capsule for a pair, two halves for
/// an unpair; a Y for a duplicator, a × for an eraser, a hexagonal nut for a normalizer; the root a
/// ring around a ball. `face`: turned to face the camera, as the flat shapes are.
function shapeSolid(T, t) {
  const flat = g => { g = g.index ? g.toNonIndexed() : g; g.computeVertexNormals(); return g; };
  const at = (g, x, y, z = 0) => g.translate(x, y, z);
  const merge = (...gs) => {
    gs = gs.map(g => g.index ? g.toNonIndexed() : g);
    const out = new T.BufferGeometry();
    for (const name of ["position", "normal"]) {
      const all = new Float32Array(gs.reduce((n, g) => n + g.attributes[name].array.length, 0));
      let o = 0; for (const g of gs) { all.set(g.attributes[name].array, o); o += g.attributes[name].array.length; }
      out.setAttribute(name, new T.BufferAttribute(all, 3));
    }
    return out;
  };
  const pyramid = (r = 0.95, h = 2.1) => flat(new T.ConeGeometry(r, h, 3));
  const knob = (x, y) => at(new T.SphereGeometry(0.27, 8, 6), x, y);
  const bar = (len, w, a) => at(new T.BoxGeometry(len, w, w).rotateZ(a), Math.cos(a) * len / 2, Math.sin(a) * len / 2);
  let g, face = false;
  switch (t) {
    case 1: g = pyramid(); break;
    case 2: g = merge(pyramid(), knob(0, -1.3)); break;
    case 3: g = merge(pyramid(), knob(-0.42, -1.28), knob(0.42, -1.28)); break;
    case 4: g = new T.TorusGeometry(0.72, 0.27, 8, 24); face = true; break;
    case 5: g = new T.CapsuleGeometry(0.58, 0.9, 4, 10).rotateZ(Math.PI / 2); break;
    case 6: g = new T.SphereGeometry(0.95, 14, 10); break;
    case 7: g = new T.OctahedronGeometry(1.15); break;
    case 8: g = merge(at(flat(new T.ConeGeometry(0.95, 1, 4)), 0, 0.62), at(flat(new T.ConeGeometry(0.95, 1, 4)).rotateZ(Math.PI), 0, -0.62)); break;
    case 9: g = merge(at(new T.SphereGeometry(0.62, 12, 8).scale(0.62, 1, 1), -0.5, 0), at(new T.SphereGeometry(0.62, 12, 8).scale(0.62, 1, 1), 0.5, 0)); break;
    case 10: g = merge(...[Math.PI / 2, -Math.PI / 6, Math.PI * 7 / 6].map(a => flat(bar(1.1, 0.42, a))), new T.SphereGeometry(0.3, 8, 6)); face = true; break;
    case 11: g = merge(flat(new T.BoxGeometry(2.1, 0.44, 0.44).rotateZ(Math.PI / 4)), flat(new T.BoxGeometry(2.1, 0.44, 0.44).rotateZ(-Math.PI / 4))); face = true; break;
    case 12: g = flat(new T.CylinderGeometry(0.95, 0.95, 1.1, 6)); break;
    case 13: g = merge(new T.TorusGeometry(1.0, 0.2, 8, 28), new T.SphereGeometry(0.46, 12, 8)); face = true; break;
    case 14: g = merge(at(pyramid(0.78), -0.42, 0), at(pyramid(0.78), 0.42, 0)); break;
    default: g = new T.SphereGeometry(0.9, 12, 8);
  }
  g.computeBoundingSphere();
  return { geometry: g, face };
}
