'use strict';
// The underdrawing: a clean vector scene the painter (painter.js) turns into brush strokes.
// Everything here is in design units (1000×1200). Nothing drawn here reaches the final image directly.

const DW = 1000, DH = 1200, TAU = Math.PI * 2;

// ─── seeded randomness ───
function mulberry32(a) {
  return () => {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    let t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}
let R = mulberry32(7);
const rand = (a = 0, b = 1) => a + R() * (b - a);
const pick = arr => arr[Math.floor(R() * arr.length)];
function gauss() { let u = 0, v = 0; while (!u) u = R(); while (!v) v = R(); return Math.sqrt(-2 * Math.log(u)) * Math.cos(TAU * v); }
const ease = x => x <= 0 ? 0 : x >= 1 ? 1 : Math.sin(x * Math.PI / 2);
const lerp = (a, b, t) => a + (b - a) * t;
const clamp = (x, a, b) => Math.max(a, Math.min(b, x));

// ─── palette: Anna's Archive site colours (from its templates), Monero, ISBN-visualizer plasma ───
const AA = { blue: '#0096ff', button: '#0195ff', deep: '#0160a7', light: '#a0d7ff', red: '#ff005b', purple: '#7f01ff', yellow: '#fffe92', green: '#2cde1c', grey: '#f2f2f2', grey2: '#dddddd', ink: '#333333' };
const XMR = { orange: '#ff6600', grey: '#4c4c4c' };
const NAVY = '#1d2636';
const C = {
  skin: '#ffe8df', skinLit: '#fff5f0', skinShade: '#f0b9b8', skinLine: '#b4616d',
  hairRoot: '#0a7fe8', hairTip: '#8fd0ff', hairShade: '#0160a7', hairShadeTip: '#2f86dc', hairHi: '#e8f6ff', hairLine: '#002c57',
  backRoot: '#0160a7', backTip: '#3aa2ff',
  eyeInk: '#10213f', sweater: '#f7f8fb', sweaterShade: '#c9d3e3', sweaterLine: '#5b6477',
  tights: '#7f01ff', tightsShade: '#43008a', tightsSheen: '#c28bff', skirt: '#2e2f36',
};
const PLASMA = ['#0d0887', '#41049d', '#6a00a8', '#8f0da4', '#b12a90', '#cc4778', '#e16462', '#f2844b', '#fca636', '#fcce25', '#f0f921'];
function hex2rgb(h) { const n = parseInt(h.slice(1), 16); return [n >> 16, n >> 8 & 255, n & 255]; }
const PLASMA_RGB = PLASMA.map(hex2rgb);
function plasma(t, k = 1) {
  t = clamp(t, 0, 1) * (PLASMA.length - 1);
  const i = Math.min(PLASMA.length - 2, Math.floor(t)), f = t - i, a = PLASMA_RGB[i], b = PLASMA_RGB[i + 1];
  return `rgb(${Math.round(lerp(a[0], b[0], f) * k)},${Math.round(lerp(a[1], b[1], f) * k)},${Math.round(lerp(a[2], b[2], f) * k)})`;
}

// ─── geometry helpers ───
/// Catmull-Rom spline through control points, sampled into a polyline.
function spline(pts, closed = false, seg = 10) {
  const n = pts.length, out = [];
  const at = i => closed ? pts[(i + n) % n] : pts[Math.max(0, Math.min(n - 1, i))];
  const last = closed ? n : n - 1;
  for (let i = 0; i < last; i++) {
    const p0 = at(i - 1), p1 = at(i), p2 = at(i + 1), p3 = at(i + 2);
    for (let s = 0; s < seg; s++) {
      const t = s / seg, t2 = t * t, t3 = t2 * t;
      out.push([0, 1].map(k => 0.5 * (2 * p1[k] + (-p0[k] + p2[k]) * t + (2 * p0[k] - 5 * p1[k] + 4 * p2[k] - p3[k]) * t2 + (-p0[k] + 3 * p1[k] - 3 * p2[k] + p3[k]) * t3)));
    }
  }
  if (!closed) out.push(pts[n - 1]);
  return out;
}
function trace(ctx, pts, close = true) {
  ctx.beginPath(); ctx.moveTo(pts[0][0], pts[0][1]);
  for (let i = 1; i < pts.length; i++) ctx.lineTo(pts[i][0], pts[i][1]);
  if (close) ctx.closePath();
}
function fillPoly(ctx, pts, style) { if (style) ctx.fillStyle = style; trace(ctx, pts); ctx.fill(); }
const shape = (ctx, pts, style, seg = 8) => fillPoly(ctx, spline(pts, true, seg), style);
const mirror = pts => pts.map(([x, y]) => [-x, y]);
function arcLen(pts) { const c = [0]; for (let i = 1; i < pts.length; i++) c.push(c[i - 1] + Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1])); return c; }
function normals(pts) {
  return pts.map((_, i) => {
    const p = pts[Math.max(0, i - 1)], q = pts[Math.min(pts.length - 1, i + 1)];
    const d = Math.hypot(q[0] - p[0], q[1] - p[1]) || 1;
    return [-(q[1] - p[1]) / d, (q[0] - p[0]) / d];
  });
}
/// Offsets a polyline both ways by a half-width profile hw(t): a closed outline plus its two edges.
function ribbon(pts, hw) {
  const cum = arcLen(pts), L = cum[cum.length - 1] || 1, nrm = normals(pts), left = [], right = [];
  pts.forEach((p, i) => {
    const w = hw(cum[i] / L);
    left.push([p[0] + nrm[i][0] * w, p[1] + nrm[i][1] * w]);
    right.push([p[0] - nrm[i][0] * w, p[1] - nrm[i][1] * w]);
  });
  return { left, right, poly: left.concat(right.slice().reverse()), center: pts };
}
/// Tapered ink line, thin at both ends.
function ink(ctx, ctrl, w, color, o = {}) {
  const pts = o.sampled ? ctrl : spline(ctrl, false, o.seg || 8);
  const a = o.a ?? 0.25, b = o.b ?? 0.25, min = o.min ?? 0.1;
  const r = ribbon(pts, t => w / 2 * Math.max(min, Math.min(ease(t / a), ease((1 - t) / b))));
  ctx.globalAlpha = o.alpha ?? 1; fillPoly(ctx, r.poly, color); ctx.globalAlpha = 1;
  return r;
}
function ellipse(ctx, x, y, rx, ry, rot = 0, style) {
  ctx.beginPath(); ctx.ellipse(x, y, rx, ry, rot, 0, TAU);
  if (style) { ctx.fillStyle = style; ctx.fill(); }
}
function outline(ctx, w, color) { ctx.lineWidth = w; ctx.strokeStyle = color; ctx.lineJoin = 'round'; ctx.lineCap = 'round'; ctx.stroke(); }
function lin(ctx, x0, y0, x1, y1, stops) { const g = ctx.createLinearGradient(x0, y0, x1, y1); stops.forEach(([t, c]) => g.addColorStop(t, c)); return g; }
function rad(ctx, x, y, r0, r1, stops) { const g = ctx.createRadialGradient(x, y, r0, x, y, r1); stops.forEach(([t, c]) => g.addColorStop(t, c)); return g; }
function text(ctx, s, x, y, font, fill, o = {}) {
  ctx.save(); ctx.translate(x, y); if (o.rot) ctx.rotate(o.rot);
  ctx.font = font; ctx.textAlign = o.align || 'left'; ctx.textBaseline = o.base || 'alphabetic'; ctx.lineJoin = 'round';
  if (o.line) { ctx.lineWidth = o.lw || 4; ctx.strokeStyle = o.line; ctx.strokeText(s, 0, 0); }
  ctx.fillStyle = fill; ctx.fillText(s, 0, 0);
  ctx.restore();
}
function deform(pts, depth, spread) {
  let cur = pts.map(p => [p[0], p[1], p[2] ?? spread]);
  for (let d = 0; d < depth; d++) {
    const out = [];
    for (let i = 0; i < cur.length; i++) {
      const a = cur[i], b = cur[(i + 1) % cur.length], len = Math.hypot(b[0] - a[0], b[1] - a[1]), v = (a[2] + b[2]) / 2;
      out.push(a, [(a[0] + b[0]) / 2 + gauss() * len * v, (a[1] + b[1]) / 2 + gauss() * len * v, v * rand(0.55, 1.05)]);
    }
    cur = out;
  }
  return cur;
}

// The painter needs to know which way hair and limbs run; every ribbon drawn here is recorded
// with its transform so painter.js can rasterise a direction field from it.
const FLOWS = [];
function recordFlow(ctx, r, weight = 1) { FLOWS.push({ m: ctx.getTransform(), r, weight }); }

// Where fine brushes are allowed: faces, hands, lettering. Recorded as shapes with transforms.
const DETAIL = [];
function markDetail(ctx, pts, level = 1) { DETAIL.push({ m: ctx.getTransform(), pts, level }); }
function markRect(ctx, x, y, w, h, level = 1) { markDetail(ctx, [[x, y], [x + w, y], [x + w, y + h], [x, y + h]], level); }
function markCircle(ctx, x, y, r, level = 1) { markDetail(ctx, Array.from({ length: 24 }, (_, i) => [x + Math.cos(i / 24 * TAU) * r, y + Math.sin(i / 24 * TAU) * r]), level); }

// ─── the ISBN map (after phiresky's visualization of all ISBNs, the bounty winner Anna's Archive hosts) ───
const MAP = { x: 26, y: 34, w: 948, cols: 10, row1: 520, gap: 10, row2: 440 };
const G978 = [
  ['English', '(978-0-)', 0.92, [0.25, 0.95]], ['English', '(978-1-)', 0.9, [0.45, 1]], ['French', '(978-2-)', 0.8, [0.2, 0.9]],
  ['German language', '(978-3-)', 0.9, [0.1, 0.95]], ['Japan', '(978-4-)', 0.85, [0, 0.75]], ['former U.S.S.R', '(978-5-)', 0.75, [0.35, 1]],
  ['978-6', '(120054 publishers)', 0.38, [0.6, 1]], ["China, People's Republic", '(978-7-)', 0.72, [0.3, 0.9]],
  ['978-8', '(306840 publishers)', 0.62, [0.2, 1]], ['978-9', '(394053 publishers)', 0.66, [0.3, 1]],
];
const G979 = [
  ['Sheet Music (ISMNs)', '(979-0-)', 0.04, [0.5, 1]], ['979-1', '(41575 publishers)', 0.4, [0.6, 1]], ['Unassigned', '(979-2-)', 0, [0, 0]],
  ['Unassigned', '(979-3-)', 0, [0, 0]], ['Unassigned', '(979-4-)', 0, [0, 0]], ['Unassigned', '(979-5-)', 0, [0, 0]],
  ['Unassigned', '(979-6-)', 0, [0, 0]], ['Unassigned', '(979-7-)', 0, [0, 0]], ['United States', '(979-8-)', 0.3, [0.7, 1]], ['Unassigned', '(979-9-)', 0, [0, 0]],
];
/// Where an ISBN-13 lands on the map: each digit after the prefix picks one of 10 slices, alternating rows and columns.
function isbnToMap(isbn) {
  const d = isbn.replace(/\D/g, ''), row = d.startsWith('979') ? 1 : 0, col = +d[3];
  const bw = MAP.w / MAP.cols;
  let x = MAP.x + col * bw, y = MAP.y + row * (MAP.row1 + MAP.gap), w = bw, h = row ? MAP.row2 : MAP.row1, alongY = true;
  for (const ch of d.slice(4, 8)) {
    if (alongY) { h /= 10; y += +ch * h; } else { w /= 10; x += +ch * w; }
    alongY = !alongY;
  }
  return [x + w / 2, y + h / 2];
}
function drawIsbnBlock(ctx, x, y, w, h, g) {
  const [label, sub, dens, [y0, y1]] = g;
  ctx.fillStyle = '#050608'; ctx.fillRect(x, y, w, h);
  const bandH = h / 10;
  for (let b = 0; b < 10; b++) {
    const by = y + b * bandH;
    let bandDens = dens * rand(0.45, 1.3) * (label === '979-1' ? (b < 4 ? 1.6 : 0.15) : 1) * (label === '978-6' && b > 3 ? 0.3 : 1);
    if (label === 'United States') { ctx.fillStyle = 'rgba(80,60,140,.55)'; ctx.fillRect(x, by, w, bandH); bandDens *= b < 3 ? 1.4 : 0.6; }
    for (let c = 0; c < 10; c++) {
      const cx = x + c * w / 10, colDens = clamp(bandDens * rand(0.2, 1.5), 0, 1);
      // a whole publisher range painted in one colour, like the big blocks in the real map
      if (R() < colDens * 0.28) {
        const t = rand(y0, y1);
        ctx.fillStyle = plasma(t, rand(0.75, 1)); ctx.fillRect(cx, by, w / 10, bandH * rand(0.3, 1));
        continue;
      }
      for (let k = 0; k < 10; k++) {
        if (R() > colDens) continue;
        ctx.fillStyle = plasma(rand(y0, y1), rand(0.55, 1.05));
        ctx.fillRect(cx + rand(0, 1), by + k * bandH / 10, w / 10 * rand(0.5, 1), bandH / 10);
      }
    }
    ctx.fillStyle = 'rgba(70,70,80,.9)'; ctx.fillRect(x, by, w, 0.8);
  }
  ctx.strokeStyle = '#3a3f4a'; ctx.lineWidth = 1.6; ctx.strokeRect(x, y, w, h);
  if (label === 'Unassigned' || !dens) return;
  // vertical labels like the real map
  markRect(ctx, x + w / 2 - 34, y + h / 2 - 150, 52, 300, 0.55);
  ctx.save(); ctx.translate(x + w / 2 - 6, y + h / 2); ctx.rotate(-Math.PI / 2);
  ctx.font = '800 25px Nunito, sans-serif'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle'; ctx.lineJoin = 'round';
  ctx.lineWidth = 6; ctx.strokeStyle = 'rgba(0,0,0,.85)'; ctx.strokeText(label, 0, 0);
  ctx.fillStyle = 'rgba(255,255,255,.93)'; ctx.fillText(label, 0, 0);
  ctx.font = '700 16px Nunito, sans-serif'; ctx.strokeText(sub, 0, 26); ctx.fillStyle = 'rgba(255,255,255,.8)'; ctx.fillText(sub, 0, 26);
  ctx.restore();
}
function drawIsbnMap(ctx) {
  // ragged-edged navy canvas
  const edge = deform([[MAP.x - 14, MAP.y - 16], [DW / 2, MAP.y - 22], [MAP.x + MAP.w + 14, MAP.y - 14], [MAP.x + MAP.w + 18, 520], [MAP.x + MAP.w + 12, 1010], [DW / 2, 1016], [MAP.x - 12, 1012], [MAP.x - 18, 520]], 4, 0.012);
  fillPoly(ctx, edge, NAVY);
  ctx.save(); trace(ctx, edge); ctx.clip();
  const bw = MAP.w / MAP.cols;
  G978.forEach((g, i) => drawIsbnBlock(ctx, MAP.x + i * bw + 2, MAP.y, bw - 4, MAP.row1, g));
  G979.forEach((g, i) => drawIsbnBlock(ctx, MAP.x + i * bw + 2, MAP.y + MAP.row1 + MAP.gap, bw - 4, MAP.row2, g));
  // backlight so she pops: the map glows hottest right behind her
  ctx.globalCompositeOperation = 'lighter';
  ctx.fillStyle = rad(ctx, 520, 380, 30, 470, [[0, 'rgba(255,0,91,.42)'], [0.45, 'rgba(127,1,255,.22)'], [1, 'rgba(0,150,255,0)']]); ctx.fillRect(0, 0, DW, DH);
  ctx.fillStyle = rad(ctx, 520, 300, 10, 230, [[0, 'rgba(160,215,255,.5)'], [1, 'rgba(160,215,255,0)']]); ctx.fillRect(0, 0, DW, DH);
  ctx.globalCompositeOperation = 'source-over';
  ctx.fillStyle = lin(ctx, 0, 0, 0, DH, [[0, 'rgba(10,14,30,.35)'], [0.3, 'rgba(10,14,30,0)'], [0.75, 'rgba(10,14,30,0)'], [1, 'rgba(10,14,30,.6)']]); ctx.fillRect(0, 0, DW, DH);
  ctx.fillStyle = rad(ctx, 520, 360, 240, 700, [[0, 'rgba(6,8,20,0)'], [1, 'rgba(6,8,20,.62)']]); ctx.fillRect(0, 0, DW, DH);
  // light rays fanning out from behind her head
  ctx.globalCompositeOperation = 'lighter';
  for (let i = 0; i < 12; i++) {
    const a = -Math.PI / 2 + (i - 5.5) * 0.26 + rand(-0.04, 0.04), w = rand(0.02, 0.045), len = rand(460, 640);
    ctx.fillStyle = rad(ctx, 520, 250, 60, len, [[0, i % 3 ? 'rgba(160,215,255,.2)' : 'rgba(255,111,176,.2)'], [1, 'rgba(160,215,255,0)']]);
    ctx.beginPath(); ctx.moveTo(520, 250); ctx.arc(520, 250, len, a - w, a + w); ctx.closePath(); ctx.fill();
  }
  ctx.globalCompositeOperation = 'source-over';
  ctx.restore();
}
/// The book she is reading, pinned on the map with the visualizer's own popup.
const READING = { title: 'Fahrenheit 451', author: 'Ray Bradbury', isbn: '978-1-4516-7331-9' };
function drawIsbnPin(ctx) {
  const [px, py] = isbnToMap(READING.isbn);
  ctx.save();
  ctx.globalCompositeOperation = 'lighter';
  ctx.fillStyle = rad(ctx, px, py, 0, 46, [[0, 'rgba(255,254,146,.95)'], [0.3, 'rgba(255,200,80,.5)'], [1, 'rgba(255,120,40,0)']]); ctx.fillRect(px - 50, py - 50, 100, 100);
  ctx.globalCompositeOperation = 'source-over';
  ellipse(ctx, px, py, 14, 14); outline(ctx, 3, AA.yellow);
  ellipse(ctx, px, py, 5, 5, 0, '#fff');
  // popup card, as the visualizer shows when you tap a book
  const x = px + 20, y = py - 214, w = 196, h = 104;
  ctx.setLineDash([4, 5]); ctx.beginPath(); ctx.moveTo(px + 10, py - 10); ctx.lineTo(x + 18, y + h); outline(ctx, 2, AA.yellow); ctx.setLineDash([]);
  ctx.beginPath(); ctx.roundRect(x, y, w, h, 10); ctx.fillStyle = '#ffffff'; ctx.fill(); outline(ctx, 2, '#1d2636');
  ctx.fillStyle = lin(ctx, 0, y + 12, 0, y + 90, [[0, '#ff4d00'], [1, '#7a0010']]); ctx.fillRect(x + 12, y + 12, 44, 66);
  flame(ctx, x + 34, y + 70, 16, 30);
  text(ctx, 'Book:', x + 64, y + 28, '700 14px Nunito, sans-serif', '#222');
  text(ctx, READING.title, x + 64, y + 46, '900 16px Nunito, sans-serif', '#111');
  text(ctx, 'by ' + READING.author, x + 64, y + 63, '700 13px Nunito, sans-serif', '#333');
  text(ctx, 'ISBN: ' + READING.isbn, x + 12, y + 96, '700 13px Nunito, sans-serif', '#222');
  markRect(ctx, x - 6, y - 6, w + 12, h + 12, 0.95);
  ctx.restore();
}
function flame(ctx, x, y, w, h) {
  const outer = [[x, y], [x - w * 0.55, y - h * 0.25], [x - w * 0.35, y - h * 0.62], [x - w * 0.1, y - h * 0.5], [x, y - h], [x + w * 0.2, y - h * 0.55], [x + w * 0.45, y - h * 0.7], [x + w * 0.55, y - h * 0.25]];
  shape(ctx, outer, lin(ctx, 0, y - h, 0, y, [[0, '#fffe92'], [0.5, '#ffb000'], [1, '#ff4d00']]));
  shape(ctx, [[x, y - 2], [x - w * 0.25, y - h * 0.22], [x - w * 0.05, y - h * 0.5], [x + w * 0.22, y - h * 0.24]], '#fff7c2');
}

// ─── AI scraper bots ───
function scraperBot(ctx, x, y, s, rot, name, target) {
  // hose sucking ISBN cells off the map
  if (target) {
    const [tx, ty] = target, mx = (x + tx) / 2 + 30, my = (y + ty) / 2 - 40;
    const hose = spline([[x, y + 10 * s], [mx, my], [tx, ty]], false, 20);
    ctx.lineCap = 'round';
    trace(ctx, hose, false); outline(ctx, 11 * s, '#2a3140'); trace(ctx, hose, false); outline(ctx, 7 * s, '#9aa7ba');
    ctx.save(); ctx.globalCompositeOperation = 'lighter';
    ctx.fillStyle = rad(ctx, tx, ty, 0, 40 * s, [[0, 'rgba(0,150,255,.7)'], [1, 'rgba(0,150,255,0)']]); ctx.fillRect(tx - 50, ty - 50, 100, 100);
    ctx.restore();
    for (let i = 0; i < 14; i++) {
      const t = i / 14, p = hose[Math.floor(t * (hose.length - 1))];
      ctx.fillStyle = plasma(rand(0.2, 1)); ctx.fillRect(p[0] + rand(-10, 10) * s - 3, p[1] + rand(-10, 10) * s - 3, 6 * s, 5 * s);
    }
  }
  ctx.save(); ctx.translate(x, y); ctx.rotate(rot); ctx.scale(s, s);
  // thruster glow
  ctx.save(); ctx.globalCompositeOperation = 'lighter';
  ctx.fillStyle = rad(ctx, 0, 40, 0, 30, [[0, 'rgba(160,215,255,.9)'], [1, 'rgba(0,150,255,0)']]); ctx.fillRect(-30, 10, 60, 60);
  ctx.restore();
  shape(ctx, [[-12, 26], [0, 46], [12, 26]], '#a0d7ff');
  // body
  const body = [[-34, -6], [-30, -30], [0, -40], [30, -30], [34, -6], [28, 24], [0, 32], [-28, 24]];
  shape(ctx, body, lin(ctx, -30, -40, 30, 30, [[0, '#ffffff'], [0.6, '#d7dee9'], [1, '#8d9ab0']]));
  trace(ctx, spline(body, true, 8)); outline(ctx, 2.4, '#1c2230');
  // visor with glowing eyes
  ctx.beginPath(); ctx.roundRect(-24, -26, 48, 22, 10); ctx.fillStyle = '#10151f'; ctx.fill();
  ctx.save(); ctx.globalCompositeOperation = 'lighter';
  for (const ex of [-10, 10]) { ctx.fillStyle = rad(ctx, ex, -15, 0, 9, [[0, 'rgba(120,255,240,1)'], [1, 'rgba(0,200,255,0)']]); ctx.fillRect(ex - 10, -26, 20, 22); }
  ctx.restore();
  ellipse(ctx, -10, -15, 3.5, 4.5, 0, '#e8fffb'); ellipse(ctx, 10, -15, 3.5, 4.5, 0, '#e8fffb');
  // antenna
  ctx.beginPath(); ctx.moveTo(0, -40); ctx.lineTo(4, -56); outline(ctx, 2.4, '#1c2230');
  ellipse(ctx, 4, -58, 4.5, 4.5, 0, AA.red);
  // little arms, one grabbing a page
  ctx.beginPath(); ctx.moveTo(-32, 4); ctx.quadraticCurveTo(-48, 10, -52, 22); outline(ctx, 4, '#1c2230');
  ctx.beginPath(); ctx.moveTo(32, 4); ctx.quadraticCurveTo(48, 8, 54, 18); outline(ctx, 4, '#1c2230');
  ctx.save(); ctx.translate(58, 24); ctx.rotate(0.3);
  ctx.fillStyle = '#fffaf0'; ctx.fillRect(-8, -10, 16, 20); ctx.strokeStyle = '#1c2230'; ctx.lineWidth = 1.5; ctx.strokeRect(-8, -10, 16, 20);
  ctx.restore();
  text(ctx, name, 0, 16, '900 11px Nunito, sans-serif', '#0b3d6e', { align: 'center' });
  markRect(ctx, -30, 4, 60, 18, 0.95); markRect(ctx, -26, -28, 52, 26, 0.8);
  ctx.restore();
}

// ─── the Anna's Archive page with its "Recommended" shelf ───
const RECS = [
  { t: READING.title, a: READING.author, c: ['#ff4d00', '#7a0010'], flame: true, reading: true },
  { t: 'Labyrinths', a: 'Jorge Luis Borges', c: ['#e8c07a', '#6b4a1f'] },
  { t: 'The Name of the Rose', a: 'Umberto Eco', c: ['#3a1e2e', '#120812'], rose: true },
];
function drawCard(ctx, x, y, rot) {
  const w = 262, h = 300;
  ctx.save(); ctx.translate(x, y); ctx.rotate(rot);
  ctx.save(); ctx.shadowColor = 'rgba(0,0,0,.55)'; ctx.shadowBlur = 30; ctx.shadowOffsetY = 14;
  ctx.beginPath(); ctx.roundRect(0, 0, w, h, 14); ctx.fillStyle = '#ffffff'; ctx.fill(); ctx.restore();
  ctx.save(); ctx.beginPath(); ctx.roundRect(0, 0, w, h, 14); ctx.clip();
  ctx.fillStyle = AA.grey; ctx.fillRect(0, 0, w, 34);
  [AA.red, '#ffc400', AA.green].forEach((c, i) => ellipse(ctx, 16 + i * 14, 17, 5, 5, 0, c));
  ctx.beginPath(); ctx.roundRect(62, 8, 188, 19, 9); ctx.fillStyle = '#ffffff'; ctx.fill();
  text(ctx, 'annas-archive.gl', 76, 23, '800 14px Nunito, sans-serif', '#333');
  // header
  bookStackIcon(ctx, 22, 60, 1);
  text(ctx, 'Anna’s Archive', 42, 67, '900 19px Nunito, sans-serif', '#000');
  ctx.beginPath(); ctx.roundRect(14, 80, 170, 24, 5); ctx.fillStyle = 'rgba(0,0,0,.067)'; ctx.fill();
  text(ctx, 'Search…', 22, 97, '700 13px Nunito, sans-serif', '#777');
  ctx.beginPath(); ctx.roundRect(190, 80, 58, 24, 5); ctx.fillStyle = AA.button; ctx.fill();
  text(ctx, 'Search', 219, 97, '800 13px Nunito, sans-serif', '#fff', { align: 'center' });
  // recommended shelf
  star(ctx, 22, 126, 8, AA.button);
  text(ctx, 'Recommended', 34, 132, '900 17px Nunito, sans-serif', '#000');
  RECS.forEach((b, i) => {
    const ry = 144 + i * 50;
    if (b.reading) { ctx.fillStyle = AA.yellow; ctx.fillRect(8, ry - 3, w - 16, 48); }
    ctx.fillStyle = lin(ctx, 0, ry, 0, ry + 42, [[0, b.c[0]], [1, b.c[1]]]); ctx.fillRect(16, ry, 30, 42);
    if (b.flame) flame(ctx, 31, ry + 38, 12, 22);
    if (b.rose) { ellipse(ctx, 31, ry + 18, 7, 7, 0, '#c2185b'); ctx.fillStyle = '#2e7d32'; ctx.fillRect(30, ry + 24, 2, 12); }
    if (!b.flame && !b.rose) { ctx.strokeStyle = 'rgba(80,50,10,.7)'; ctx.lineWidth = 1.5; for (let k = 0; k < 3; k++) { ctx.strokeRect(20 + k * 4, ry + 6 + k * 5, 22 - k * 8, 30 - k * 10); } }
    text(ctx, b.t, 56, ry + 17, '900 15px Nunito, sans-serif', '#000');
    text(ctx, b.a, 56, ry + 34, '700 13px Nunito, sans-serif', '#555');
    if (b.reading) { ctx.beginPath(); ctx.roundRect(186, ry + 24, 62, 17, 8); ctx.fillStyle = AA.blue; ctx.fill(); text(ctx, 'reading', 217, ry + 37, '900 11px Nunito, sans-serif', '#fff', { align: 'center' }); }
  });
  ctx.fillStyle = AA.blue; ctx.fillRect(0, h - 6, w * 0.62, 6);
  ctx.restore();
  ctx.beginPath(); ctx.roundRect(0, 0, w, h, 14); outline(ctx, 2, '#0d1220');
  markRect(ctx, -8, -8, w + 16, h + 16, 0.95);
  ctx.restore();
}
function bookStackIcon(ctx, x, y, s) {
  const books = [[0, 6, 22, '#ff005b'], [2, 0, 18, '#0096ff'], [-1, -6, 20, '#2cde1c']];
  for (const [dx, dy, bw, col] of books) { ctx.beginPath(); ctx.roundRect(x + dx * s - bw / 2 * s, y + dy * s - 3 * s, bw * s, 6 * s, 1.5); ctx.fillStyle = col; ctx.fill(); outline(ctx, 1.2, '#111'); }
}
function star(ctx, x, y, r, fill, line) {
  ctx.beginPath();
  for (let i = 0; i < 10; i++) { const a = i * Math.PI / 5 - Math.PI / 2, rr = i % 2 ? r * 0.48 : r; ctx.lineTo(x + Math.cos(a) * rr, y + Math.sin(a) * rr); }
  ctx.closePath(); ctx.fillStyle = fill; ctx.fill(); if (line) outline(ctx, 1.6, line);
}

// ─── the mountain of books she sits on (spines styled like the visualizer's bookshelf view) ───
const SPINES = ['#d81b8c', '#f06292', '#f4a582', '#ee8a57', '#b12a90', '#6a00a8', '#e84393', '#ff9e7a', '#8f0da4', '#cc4778', '#0160a7', '#0096ff', '#fca636', '#3b1d4a', '#2a1a4a', '#fcce25'];
function spineBook(ctx, x, y, w, h, rot, col) {
  ctx.save(); ctx.translate(x, y); ctx.rotate(rot);
  ctx.fillStyle = lin(ctx, 0, -h / 2, 0, h / 2, [[0, '#ffffff'], [0.12, col], [0.8, col], [1, 'rgba(0,0,0,.55)']]);
  ctx.fillRect(-w / 2, -h / 2, w, h);
  ctx.fillStyle = col; ctx.globalAlpha = 0.55; ctx.fillRect(-w / 2, -h / 2 + h * 0.12, w, h * 0.7); ctx.globalAlpha = 1;
  ctx.strokeStyle = '#1b0a24'; ctx.lineWidth = 2; ctx.strokeRect(-w / 2, -h / 2, w, h);
  // title bars, a barcode like the bookshelf view
  ctx.fillStyle = 'rgba(30,10,40,.55)';
  ctx.fillRect(-w / 2 + 14, -2, w * 0.45, 3.2); ctx.fillRect(-w / 2 + 14, 4, w * 0.3, 2.4);
  const bx = w / 2 - 34;
  ctx.fillStyle = '#fff'; ctx.fillRect(bx - 2, -h / 2 + 4, 30, h - 8);
  ctx.fillStyle = '#111'; for (let i = 0; i < 12; i++) ctx.fillRect(bx + i * 2.3, -h / 2 + 6, rand(0.6, 1.6), h - 12);
  ctx.restore();
}
function pageBlock(ctx, x, y, w, h, rot, col) {
  ctx.save(); ctx.translate(x, y); ctx.rotate(rot);
  ctx.fillStyle = col; ctx.fillRect(-w / 2, -h / 2, w, h);
  ctx.fillStyle = '#fbf3e4'; ctx.fillRect(-w / 2 + 3, -h / 2 + 3, w - 6, h - 6);
  ctx.strokeStyle = 'rgba(150,120,90,.5)'; ctx.lineWidth = 0.8;
  for (let yy = -h / 2 + 6; yy < h / 2 - 3; yy += 2.4) { ctx.beginPath(); ctx.moveTo(-w / 2 + 4, yy); ctx.lineTo(w / 2 - 4, yy); ctx.stroke(); }
  ctx.strokeStyle = '#1b0a24'; ctx.lineWidth = 2; ctx.strokeRect(-w / 2, -h / 2, w, h);
  ctx.restore();
}
function drawBookPile(ctx) {
  const tops = x => x < 160 ? 870 + (160 - x) * 0.3 : x < 430 ? 836 + Math.sin(x / 40) * 10 : x < 770 ? 814 : x < 950 ? 880 : 905;
  // books standing on end at the edges of the pile
  for (const [x, h, r] of [[40, 150, -0.12], [70, 120, 0.05], [964, 140, 0.1], [936, 110, -0.06]]) spineBook(ctx, x, 1080 - h / 2 + 10, h, 30, Math.PI / 2 + r, pick(SPINES));
  let x = 20;
  while (x < 980) {
    const w = rand(100, 190), top = tops(x + w / 2);
    let y = 1090, row = 0;
    while (y > top) {
      const h = rand(16, 34), bx = x + w / 2 + rand(-14, 14), rot = rand(-0.06, 0.06);
      (R() < 0.18 ? pageBlock : spineBook)(ctx, bx, y - h / 2, w * rand(0.85, 1.1), h, rot, pick(SPINES));
      y -= h - 1; row++;
    }
    x += w * rand(0.7, 0.92);
  }
  ctx.fillStyle = lin(ctx, 0, 840, 0, 1090, [[0, 'rgba(10,5,30,0)'], [1, 'rgba(10,5,30,.55)']]); ctx.fillRect(0, 840, DW, 260);
}
/// Leaning towers of books at the sides, like a canyon under the ISBN sky.
function bookTower(ctx, x, base, top, lean, wmin, wmax) {
  let y = base, i = 0;
  while (y > top) {
    const h = rand(18, 34), w = rand(wmin, wmax), t = (base - y) / (base - top);
    (R() < 0.2 ? pageBlock : spineBook)(ctx, x + lean * t * t + rand(-9, 9), y - h / 2, w, h, rand(-0.07, 0.07) + lean * 0.0012, pick(SPINES));
    y -= h - 1; i++;
  }
  ctx.fillStyle = lin(ctx, 0, top, 0, base, [[0, 'rgba(10,5,30,0)'], [1, 'rgba(10,5,30,.5)']]); ctx.fillRect(x - wmax, top - 20, wmax * 2 + Math.abs(lean), base - top + 20);
  // a few books tumbling off the pile
  spineBook(ctx, 120, 800, 150, 26, -0.5, '#e84393');
  spineBook(ctx, 900, 845, 130, 24, 0.35, '#6a00a8');
}

// ─── Anna ───
// Head space: origin at the centre of the face, chin at y≈100. Tilted toward her raised arm.
const HEAD = { x: 526, y: 284, tilt: -0.14, scale: 1.26 };
const withHead = (ctx, fn) => { ctx.save(); ctx.translate(HEAD.x, HEAD.y); ctx.rotate(HEAD.tilt); ctx.scale(HEAD.scale, HEAD.scale); fn(); ctx.restore(); };
const FACE = [[-70, -70], [-75, -25], [-73, 10], [-67, 40], [-55, 64], [-36, 84], [-14, 97], [0, 101], [14, 97], [36, 84], [55, 64], [67, 40], [73, 10], [75, -25], [70, -70], [40, -100], [0, -108], [-40, -100]];
const JAW = [[-74, 0], [-67, 40], [-55, 64], [-36, 84], [-14, 97], [0, 101], [14, 97], [36, 84], [55, 64], [67, 40], [74, 0]];
const EYE = [[-19, 3], [-13, -3], [-2, -6], [10, -5], [19, -1], [23, 3], [17, 8], [5, 11], [-7, 10], [-15, 7]];

function lockShape(spine, w, o = {}) {
  const pts = spline(spine, false, 14);
  return ribbon(pts, t => w / 2 * (0.4 + 0.6 * ease(t / 0.14)) * Math.pow(1 - t, o.tip ?? 0.85) * (1 + (o.bulge ?? 0.3) * Math.sin(Math.PI * t)));
}
/// One clump of hair: gradient fill, cel shadow down one side, tapered outline, strand lines.
function lock(ctx, spine, w, o = {}) {
  const r = lockShape(spine, w, o), n = r.center.length, s0 = spine[0], s1 = spine[spine.length - 1];
  fillPoly(ctx, r.poly, lin(ctx, s0[0], s0[1], s1[0], s1[1], [[0, o.root || C.hairRoot], [1, o.tipC || C.hairTip]]));
  const edge = (o.shadeSide ?? 1) > 0 ? r.right : r.left;
  const inner = r.center.map((c, i) => [lerp(c[0], edge[i][0], 0.2), lerp(c[1], edge[i][1], 0.2)]);
  const k0 = Math.floor(n * 0.12);
  ctx.globalAlpha = o.shadeAlpha ?? 0.6;
  fillPoly(ctx, inner.slice(k0).concat(edge.slice(k0).reverse()), lin(ctx, s0[0], s0[1], s1[0], s1[1], [[0, 'rgba(0,0,0,0)'], [0.2, o.shade || C.hairShade], [1, o.shadeTip || C.hairShadeTip]]));
  ctx.globalAlpha = 1;
  for (const f of [-0.5, 0.15]) {
    const pts = r.center.map((c, i) => [lerp(c[0], r.left[i][0], f), lerp(c[1], r.left[i][1], f)]).slice(Math.floor(n * 0.25), Math.floor(n * 0.9));
    ink(ctx, pts, 1.3, o.lineC || C.hairLine, { sampled: true, alpha: 0.35, a: 0.3, b: 0.4 });
  }
  const k = Math.floor(n * (o.lineFrom ?? 0.15)), lw = o.lw ?? 2;
  ink(ctx, r.left.slice(k), lw, o.lineC || C.hairLine, { sampled: true, a: 0.3, b: 0.04, min: 0.3 });
  ink(ctx, r.right.slice(k), lw, o.lineC || C.hairLine, { sampled: true, a: 0.3, b: 0.04, min: 0.3 });
  recordFlow(ctx, r);
  return r;
}
function resample(pts, n) {
  const cum = arcLen(pts), L = cum[cum.length - 1], out = [];
  for (let i = 0, j = 0; i < n; i++) {
    const d = L * i / (n - 1);
    while (j < pts.length - 2 && cum[j + 1] < d) j++;
    const f = (d - cum[j]) / ((cum[j + 1] - cum[j]) || 1);
    out.push([lerp(pts[j][0], pts[j + 1][0], f), lerp(pts[j][1], pts[j + 1][1], f)]);
  }
  return out;
}
/// A sheet of hair: n locks interpolated between guide curves A and B, over a solid base so no gaps show.
function sweep(ctx, A, B, n, w, o = {}) {
  const m = o.samples ?? 8, a = resample(spline(A, false, 10), m), b = resample(spline(B, false, 10), m);
  if (o.base !== false) {
    const k = Math.round(m * 0.7), sa = a.slice(0, k), sb = b.slice(0, k);
    fillPoly(ctx, spline(sa, false, 8).concat(spline(sb, false, 8).reverse()), o.baseC || C.hairShade);
  }
  const locks = [], wave = o.wave ?? 0, nrm = normals(a);
  for (let i = 0; i < n; i++) {
    const t = n === 1 ? 0.5 : i / (n - 1), len = rand(o.minLen ?? 0.78, 1), ph = rand(0, TAU), amp = wave * rand(0.6, 1.3);
    const pts = a.map((p, j) => {
      const u = j / (m - 1), sw = Math.sin(u * 5 + ph + t * 2) * amp * u;
      return [lerp(p[0], b[j][0], t) + nrm[j][0] * sw + (j ? gauss() * (o.jit ?? 3) : 0), lerp(p[1], b[j][1], t) + nrm[j][1] * sw + (j ? gauss() * (o.jit ?? 3) : 0)];
    });
    locks.push({ pts: pts.slice(0, Math.max(3, Math.round(pts.length * len))), w: w * rand(0.75, 1.2), t });
  }
  (o.order === 'in' ? locks.reverse() : locks).forEach(l => {
    const r = lock(ctx, l.pts, l.w, o);
    if (o.shine) hairShine(ctx, r, o.shine[0], o.shine[1], 0.3, o.shineA ?? 0.45);
    if (o.rim && l.t < 0.25) rimLight(ctx, r, o.rimSide ?? -1, o.rim, 0.25, 0.97);
  });
}
function hairShine(ctx, r, t0, t1, wf, alpha = 0.8, color = C.hairHi) {
  const n = r.center.length, i0 = Math.floor(n * t0), i1 = Math.floor(n * t1), L = [], Rr = [];
  for (let i = i0; i <= i1; i++) {
    const t = (i - i0) / (i1 - i0), bump = Math.sin(Math.PI * t) * wf, c = r.center[i];
    L.push([lerp(c[0], r.left[i][0], bump), lerp(c[1], r.left[i][1], bump)]); Rr.push([lerp(c[0], r.right[i][0], bump * 0.8), lerp(c[1], r.right[i][1], bump * 0.8)]);
  }
  ctx.globalAlpha = alpha; fillPoly(ctx, L.concat(Rr.reverse()), color); ctx.globalAlpha = 1;
}
/// Rim light from the glowing map behind her, along one edge of a lock.
function rimLight(ctx, r, side, color, t0 = 0.2, t1 = 0.95) {
  const n = r.center.length, e = side < 0 ? r.left : r.right;
  ink(ctx, e.slice(Math.floor(n * t0), Math.floor(n * t1)), 3.2, color, { sampled: true, a: 0.3, b: 0.3, alpha: 0.85 });
}

// Back hair: a huge mane streaming left in the data wind.
const BACK_LOCKS = [
  { s: [[-20, -120], [-120, -150], [-230, -150], [-330, -110], [-410, -40]], w: 64 },
  { s: [[-40, -110], [-150, -100], [-250, -40], [-320, 50], [-360, 160], [-420, 240]], w: 104 },
  { s: [[-60, -80], [-160, -20], [-240, 70], [-280, 190], [-300, 310], [-360, 420]], w: 110 },
  { s: [[-80, -40], [-150, 60], [-200, 180], [-215, 320], [-250, 450], [-310, 560]], w: 104 },
  { s: [[-80, 0], [-120, 110], [-140, 250], [-130, 390], [-160, 520], [-200, 610]], w: 90 },
  { s: [[60, -90], [120, 0], [140, 120], [150, 250], [175, 370], [160, 470]], w: 84 },
  { s: [[40, -110], [120, -80], [180, 0], [205, 110], [240, 200], [290, 250]], w: 60 },
];
const BACKCAP = [[0, -126], [70, -112], [100, -60], [104, 20], [80, 100], [0, 120], [-80, 100], [-104, 20], [-100, -60], [-70, -112]];
const CAP = [[-86, -10], [-94, -60], [-74, -106], [-30, -128], [20, -130], [64, -114], [90, -76], [94, -26], [82, 10], [40, -40], [0, -60], [-44, -46]];
const BANGS = [
  { s: [[38, -120], [0, -104], [-38, -76], [-62, -36], [-72, 8]], w: 54 },
  { s: [[34, -118], [12, -86], [-12, -52], [-24, -16]], w: 34 },
  { s: [[42, -116], [38, -86], [28, -54], [20, -30]], w: 26 },
  { s: [[56, -112], [72, -74], [80, -32], [78, 12]], w: 36 },
  { s: [[-40, -118], [-76, -84], [-90, -34], [-88, 22]], w: 34 },
  { s: [[20, -124], [-10, -110], [-50, -96], [-80, -60]], w: 26 },
];
const SIDE_LOCKS = [
  { s: [[-70, -50], [-84, 30], [-86, 120], [-76, 210], [-94, 300], [-80, 380]], w: 34, side: -1 },
  { s: [[72, -50], [86, 40], [94, 140], [110, 230], [100, 320]], w: 30, side: 1 },
];

// Back hair lives in design units: a mane streaming left in the data wind, and a shorter fall on the right.
function drawBackHair(ctx) {
  const back = { root: C.backRoot, tipC: C.backTip, shade: '#003b73', shadeTip: '#1467bd', shadeAlpha: 0.5, lw: 2, lineFrom: 0.2, baseC: '#02467f' };
  sweep(ctx, [[560, 178], [640, 230], [672, 320], [684, 420], [704, 520]], [[556, 240], [596, 290], [616, 370], [626, 450], [642, 530]], 6, 46, { ...back, shadeSide: 1, shine: [0.3, 0.45], rim: '#9fd8ff', rimSide: 1 });
  sweep(ctx, [[505, 168], [420, 178], [330, 212], [252, 272], [206, 352], [176, 440], [128, 530], [92, 610], [40, 680]],
    [[500, 250], [456, 312], [424, 396], [390, 486], [364, 576], [330, 664], [296, 744], [244, 820]], 18, 58, { ...back, shadeSide: 1, shine: [0.3, 0.44], rim: '#ff6fb0', order: 'in', wave: 18, samples: 12, minLen: 0.7 });
  withHead(ctx, () => fillPoly(ctx, spline(BACKCAP, true, 10), lin(ctx, 0, -130, 0, 120, [[0, C.backRoot], [1, '#0a57a0']])));
}

function drawEye(ctx, side) {
  ctx.save(); ctx.translate(side * 34, 10); ctx.scale(side * 1.12, 1.12);
  ctx.save(); ctx.globalAlpha = 0.28; shape(ctx, [[-20, -3], [-6, -15], [14, -14], [28, -3], [14, -7], [-4, -8]], AA.purple); ctx.restore();
  const gaze = 3.5 * side, white = spline(EYE, true, 8), gy = 2;
  fillPoly(ctx, white, '#fbfbff');
  ctx.save(); trace(ctx, white); ctx.clip();
  ctx.fillStyle = lin(ctx, 0, -12, 0, 4, [[0, 'rgba(90,110,190,.8)'], [1, 'rgba(90,110,190,0)']]); ctx.fillRect(-26, -14, 52, 20);
  ctx.save(); ellipse(ctx, 3 + gaze, 5 + gy, 9.5, 11); ctx.clip();
  ctx.fillStyle = lin(ctx, 0, -6, 0, 16, [[0, '#062a5c'], [0.45, AA.deep], [0.8, AA.blue], [1, AA.light]]); ctx.fillRect(-20, -14, 40, 32);
  ellipse(ctx, 4 + gaze, 7 + gy, 4.2, 6, 0, '#051428');
  ctx.fillStyle = lin(ctx, 0, -10, 0, 0, [[0, 'rgba(0,10,40,.8)'], [1, 'rgba(0,10,40,0)']]); ctx.fillRect(-20, -14, 40, 16);
  ctx.restore();
  ellipse(ctx, 3 + gaze, 5 + gy, 9.5, 11); outline(ctx, 1.4, 'rgba(4,20,60,.9)');
  ellipse(ctx, 0 + gaze, 3 + gy, 2.6, 3.2, -0.3, '#ffffff'); ellipse(ctx, 8 + gaze, 10 + gy, 1.3, 1.3, 0, '#ffffff');
  ctx.restore();
  ink(ctx, [[-21, 4], [-15, -4], [-2, -8], [11, -7], [21, -2], [27, 1], [33, -3]], 5.6, C.eyeInk, { a: 0.3, b: 0.1, min: 0.15 });
  ink(ctx, [[23, -2], [31, -7], [37, -8]], 1.9, C.eyeInk, { a: 0.1, b: 0.8 });
  ink(ctx, [[26, 1], [35, 0]], 1.5, C.eyeInk, { a: 0.1, b: 0.8 });
  ink(ctx, [[2, 11], [12, 10], [19, 6]], 1.4, C.eyeInk, { alpha: 0.8 });
  ink(ctx, [[-12, -14], [3, -17], [16, -14], [24, -8]], 1.3, C.skinLine, { alpha: 0.55 });
  ctx.restore();
}

function drawHeadFront(ctx) {
  const face = spline(FACE, true, 10);
  fillPoly(ctx, face, lin(ctx, -70, -60, 70, 100, [[0, C.skinLit], [1, C.skin]]));
  ctx.save(); trace(ctx, face); ctx.clip();
  ctx.globalAlpha = 0.7;
  for (const b of BANGS) { ctx.save(); ctx.translate(3, 9); fillPoly(ctx, lockShape(b.s, b.w).poly, C.skinShade); ctx.restore(); }
  ctx.globalAlpha = 1;
  ctx.fillStyle = lin(ctx, -80, 0, -30, 0, [[0, 'rgba(236,160,175,.6)'], [1, 'rgba(236,160,175,0)']]); ctx.fillRect(-90, -80, 60, 200);
  // magenta rim from the map on her cheek
  ctx.fillStyle = lin(ctx, -78, 0, -58, 0, [[0, 'rgba(255,80,160,.55)'], [1, 'rgba(255,80,160,0)']]); ctx.fillRect(-90, -30, 40, 120);
  for (const s of [-1, 1]) {
    ctx.save(); ctx.translate(s * 42, 44); ctx.scale(1, 0.45);
    ctx.fillStyle = rad(ctx, 0, 0, 1, 22, [[0, 'rgba(255,110,150,.45)'], [1, 'rgba(255,110,150,0)']]); ctx.fillRect(-24, -24, 48, 48); ctx.restore();
  }
  ctx.restore();
  ink(ctx, JAW, 1.9, C.skinLine, { a: 0.18, b: 0.18 });
  // nose, lips
  ink(ctx, [[4, 40], [2, 49]], 1.8, '#d08c93', { a: 0.4, b: 0.4 });
  ellipse(ctx, 1, 74, 11, 3.8, 0.05, 'rgba(235,90,122,.55)');
  ink(ctx, [[-15, 69], [-5, 72.5], [6, 72], [17, 66]], 2.2, '#8a2c48', { a: 0.2, b: 0.25 });
  ink(ctx, [[-5, 78], [1, 79.5], [7, 78]], 1.3, '#c2566f', { alpha: 0.6 });
  ellipse(ctx, 3, 76.5, 3, 1.1, 0, 'rgba(255,255,255,.7)');
  drawEye(ctx, -1); drawEye(ctx, 1);
  ellipse(ctx, 46, 34, 1.6, 1.6, 0, '#5a2a3a');
  for (const s of [-1, 1]) ink(ctx, [[s * 16, -31], [s * 34, -37], [s * 55, -31]], 2.2, '#07366a', { a: 0.3, b: 0.5 });
  markDetail(ctx, spline(FACE, true, 4).map(([x, y]) => [x * 1.05, y * 1.02]), 1);
}

function drawFrontHair(ctx) {
  for (const l of SIDE_LOCKS) {
    const r = lock(ctx, l.s, l.w, { shadeSide: l.side, lw: 2 });
    hairShine(ctx, r, 0.1, 0.24, 0.4, 0.7);
    rimLight(ctx, r, l.side, l.side < 0 ? '#ff6fb0' : '#9fd8ff', 0.25, 0.95);
  }
  const cap = spline(CAP, true, 10);
  fillPoly(ctx, cap, rad(ctx, -20, -110, 8, 120, [[0, '#48adff'], [1, C.hairRoot]]));
  ink(ctx, spline(CAP.slice(0, 9), false, 10), 2.2, C.hairLine, { sampled: true, a: 0.12, b: 0.12 });
  recordFlow(ctx, ribbon(spline([[-80, -60], [-40, -118], [30, -126], [86, -70]], false, 10), () => 20));
  sweep(ctx, [[-44, -122], [-78, -90], [-94, -40], [-92, 20]], [[-20, -126], [-50, -96], [-66, -56], [-72, -10]], 3, 32, { base: false, shadeSide: -1, lw: 1.8 });
  sweep(ctx, [[46, -118], [66, -84], [80, -40], [80, 14]], [[44, -120], [48, -88], [44, -54], [36, -26]], 3, 28, { base: false, shadeSide: 1, lw: 1.8, shine: [0.18, 0.34], shineA: 0.6 });
  sweep(ctx, [[40, -124], [-10, -120], [-58, -94], [-90, -46], [-98, 14]], [[40, -118], [22, -92], [2, -58], [-14, -22], [-20, -6]], 6, 36, { base: false, shadeSide: 1, lw: 1.9, shine: [0.16, 0.3], shineA: 0.6, minLen: 0.9 });
  // a yellow clip shaped like a tiny book, the highlight colour of the site
  ctx.save(); ctx.translate(62, -92); ctx.rotate(0.5);
  ctx.beginPath(); ctx.roundRect(-13, -8, 26, 16, 3); ctx.fillStyle = AA.yellow; ctx.fill(); outline(ctx, 1.8, '#5a4a00');
  ctx.beginPath(); ctx.moveTo(0, -8); ctx.lineTo(0, 8); outline(ctx, 1.4, '#5a4a00');
  ctx.restore();
}

// Body parts are in design units, no head tilt.
const ARM_UP = [[402, 508], [382, 424], [366, 334], [360, 262], [364, 216], [378, 190], [402, 186], [418, 210], [426, 280], [438, 364], [454, 444], [468, 506]];
const ARM_BOOK = [[640, 490], [676, 484], [702, 508], [720, 570], [736, 640], [730, 682], [704, 690], [690, 646], [670, 584], [650, 530]];
const FOREARM = [[704, 690], [736, 670], [724, 628], [704, 598], [684, 584], [668, 598], [680, 632], [692, 664]];
const TORSO = [[400, 495], [440, 500], [490, 512], [540, 516], [590, 512], [640, 500], [686, 494], [702, 540], [698, 586], [676, 626], [662, 664], [666, 704], [590, 714], [520, 714], [452, 704], [458, 664], [440, 626], [424, 586], [418, 540]];
const SKIRT = [[446, 686], [520, 700], [600, 704], [664, 688], [690, 740], [706, 790], [650, 824], [560, 834], [480, 828], [430, 812], [426, 750]];
const LEG_BACK = [[600, 858], [670, 872], [700, 900], [706, 940], [692, 1000], [678, 1080], [636, 1080], [646, 1000], [650, 950], [630, 910], [600, 890]];
const LEG_TOP = [[556, 796], [660, 800], [740, 818], [784, 838], [802, 868], [790, 912], [766, 970], [738, 1040], [722, 1080], [682, 1080], [700, 1030], [726, 960], [748, 906], [744, 886], [690, 872], [620, 864], [560, 852]];
const BOOK = { x: 590, y: 712, w: 240, h: 164, rot: -0.08 };

function limb(ctx, pts, fill, line, spineFlow, lw = 2.4) {
  const p = spline(pts, true, 8);
  fillPoly(ctx, p, fill); trace(ctx, p); outline(ctx, lw, line);
  if (spineFlow) recordFlow(ctx, ribbon(spline(spineFlow[0], false, 10), () => spineFlow[1]));
}
function drawBody(ctx) {
  // legs in purple tights
  const tightsFill = (x0, x1) => lin(ctx, x0, 0, x1, 0, [[0, C.tightsShade], [0.35, C.tights], [0.6, '#9a3dff'], [1, C.tightsShade]]);
  limb(ctx, LEG_BACK, tightsFill(630, 710), '#240046', [[[610, 874], [680, 904], [672, 1000], [658, 1080]], 26]);
  ctx.save(); trace(ctx, spline(LEG_BACK, true, 8)); ctx.clip(); ctx.fillStyle = 'rgba(20,0,50,.35)'; ctx.fillRect(560, 840, 200, 260); ctx.restore();
  ink(ctx, [[684, 916], [682, 980], [668, 1060]], 3.5, C.tightsSheen, { alpha: 0.45 });
  limb(ctx, LEG_TOP, tightsFill(560, 800), '#240046', [[[570, 826], [700, 836], [770, 862], [750, 960], [704, 1080]], 32]);
  ink(ctx, [[590, 810], [670, 812], [740, 828], [780, 848]], 5, C.tightsSheen, { alpha: 0.75 });
  ink(ctx, [[782, 900], [760, 956], [734, 1030]], 4, C.tightsSheen, { alpha: 0.6 });
  ellipse(ctx, 786, 860, 11, 7, 0.6, 'rgba(230,200,255,.6)');
  // skirt
  limb(ctx, SKIRT, lin(ctx, 0, 690, 0, 820, [[0, '#45464f'], [1, '#1f2026']]), '#101014');
  for (const [x0, x1] of [[452, 440], [478, 470], [506, 504], [534, 538], [562, 570], [640, 660], [668, 690]]) {
    ink(ctx, [[x0, 700], [lerp(x0, x1, 0.5), 760], [x1, 820]], 3, 'rgba(120,130,160,.55)', {});
    ink(ctx, [[x0 + 5, 704], [x1 + 6, 822]], 1.6, 'rgba(0,0,0,.5)', {});
  }
  ink(ctx, [[434, 808], [500, 824], [580, 830], [652, 822], [704, 792]], 3, 'rgba(160,170,200,.5)', {});
  // neck and bare shoulders
  shape(ctx, [[420, 478], [470, 462], [515, 452], [565, 452], [610, 462], [655, 478], [690, 494], [685, 516], [540, 526], [400, 516]], lin(ctx, 0, 450, 0, 520, [[0, C.skinLit], [1, C.skin]]));
  shape(ctx, [[517, 350], [515, 410], [508, 458], [574, 460], [566, 410], [561, 352]], C.skin);
  ctx.fillStyle = lin(ctx, 0, 380, 0, 430, [[0, 'rgba(230,150,160,.8)'], [1, 'rgba(230,150,160,0)']]); ctx.fillRect(505, 370, 75, 60);
  ink(ctx, [[478, 478], [508, 486], [530, 482]], 1.6, C.skinLine, { alpha: 0.7 });
  ink(ctx, [[552, 484], [578, 488], [612, 480]], 1.6, C.skinLine, { alpha: 0.7 });
  ink(ctx, [[517, 380], [514, 420], [509, 456]], 1.8, C.skinLine, { alpha: 0.8 });
  ink(ctx, [[562, 382], [566, 420], [572, 458]], 1.8, C.skinLine, { alpha: 0.8 });
  // choker in the site's banner red
  ink(ctx, [[512, 436], [540, 443], [568, 436]], 7, AA.red, { a: 0.1, b: 0.1, min: 0.8 });
  ctx.beginPath(); ctx.roundRect(534, 444, 12, 9, 2); ctx.fillStyle = AA.yellow; ctx.fill(); outline(ctx, 1.2, '#5a4a00');
  // raised arm in the sleeve, then the off-shoulder sweater
  const sweater = (x0, x1) => lin(ctx, x0, 0, x1, 0, [[0, '#9fb0cf'], [0.3, '#e4eaf4'], [0.62, '#ffffff'], [0.85, '#d7dfee'], [1, '#8d9dc0']]);
  limb(ctx, ARM_UP, lin(ctx, 360, 0, 468, 0, [[0, '#7f90b8'], [0.3, '#dfe6f2'], [0.6, '#ffffff'], [1, '#aebbd6']]), '#3f4a63', [[[434, 500], [410, 400], [392, 290], [392, 200]], 32], 2.8);
  ellipse(ctx, 392, 204, 18, 14, -0.3, 'rgba(255,255,255,.7)');
  for (const [y, d] of [[250, 1], [300, -1], [352, 1], [410, -1]]) ink(ctx, [[380 + (y - 250) * 0.1, y], [398 + (y - 250) * 0.1, y + 8 * d], [420 + (y - 250) * 0.12, y + 2]], 1.4, C.sweaterLine, { alpha: 0.55 });
  ink(ctx, [[392, 470], [382, 390], [376, 310]], 5, 'rgba(160,175,205,.6)', {});
  const torso = spline(TORSO, true, 10);
  fillPoly(ctx, torso, sweater(420, 700)); trace(ctx, torso); outline(ctx, 2.4, C.sweaterLine);
  ctx.save(); trace(ctx, torso); ctx.clip();
  ctx.strokeStyle = 'rgba(120,135,165,.35)'; ctx.lineWidth = 1.6;
  for (let x = 420; x < 700; x += 9) {
    const bend = (x < 555 ? x - 500 : x - 612) * 0.35;
    ctx.beginPath(); ctx.moveTo(x, 520); ctx.bezierCurveTo(x + bend, 560, x + bend * 0.8, 600, x + (x < 555 ? 8 : -8), 640); ctx.lineTo(x + (x < 555 ? 6 : -6), 714); ctx.stroke();
  }
  ctx.fillStyle = lin(ctx, 0, 505, 0, 560, [[0, 'rgba(90,110,150,.35)'], [1, 'rgba(90,110,150,0)']]); ctx.fillRect(400, 500, 300, 60);
  // soft form of the chest under the knit, and a nipped waist
  for (const [cx, cy] of [[500, 566], [612, 562]]) {
    ctx.fillStyle = rad(ctx, cx, cy + 16, 20, 64, [[0, 'rgba(0,0,0,0)'], [0.6, 'rgba(100,118,160,.28)'], [1, 'rgba(100,118,160,0)']]); ctx.fillRect(cx - 80, cy - 70, 160, 150);
    ctx.fillStyle = rad(ctx, cx - 12, cy - 16, 2, 44, [[0, 'rgba(255,255,255,1)'], [1, 'rgba(255,255,255,0)']]); ctx.fillRect(cx - 70, cy - 70, 140, 140);
  }
  ink(ctx, [[456, 580], [496, 610], [548, 600]], 3.2, 'rgba(110,125,165,.6)', {});
  ink(ctx, [[566, 598], [614, 606], [664, 576]], 3.2, 'rgba(110,125,165,.6)', {});
  ink(ctx, [[470, 640], [520, 628], [560, 634]], 2, 'rgba(110,125,165,.45)', {});
  ink(ctx, [[660, 520], [640, 560], [632, 600]], 2, 'rgba(110,125,165,.45)', {});
  ctx.fillStyle = lin(ctx, 420, 0, 470, 0, [[0, 'rgba(110,125,160,.5)'], [1, 'rgba(110,125,160,0)']]); ctx.fillRect(420, 560, 60, 160);
  ctx.restore();
  recordFlow(ctx, ribbon(spline([[450, 600], [560, 610], [680, 600]], false, 10), () => 90), 0.5);
  ink(ctx, [[400, 497], [470, 510], [540, 517], [610, 510], [686, 492]], 12, '#eef1f7', { a: 0.05, b: 0.05, min: 0.9 });
  ink(ctx, [[400, 491], [470, 504], [540, 511], [610, 504], [686, 486]], 2, C.sweaterLine, { a: 0.05, b: 0.05, min: 0.9 });
  // the arm that holds the book
  limb(ctx, ARM_BOOK, sweater(640, 740), C.sweaterLine, [[[660, 500], [700, 580], [718, 670]], 32]);
  limb(ctx, FOREARM, sweater(670, 740), C.sweaterLine, [[[714, 676], [700, 630], [684, 596]], 26]);
  ink(ctx, [[672, 596], [700, 588], [722, 612]], 6, '#e6ebf3', { min: 0.8 });
  // rim light from the glowing map: hot pink on her left edges, cool blue on her right
  ink(ctx, [[398, 500], [380, 420], [366, 330], [362, 262], [368, 214]], 2.6, '#ff6fb0', { alpha: 0.8 });
  ink(ctx, [[420, 540], [424, 586], [440, 626], [456, 664]], 3.5, '#ff6fb0', { alpha: 0.8 });
  ink(ctx, [[688, 496], [702, 540], [714, 590], [730, 640]], 3.5, '#8fd0ff', { alpha: 0.85 });
  ink(ctx, [[566, 356], [570, 410], [576, 452]], 2.5, '#8fd0ff', { alpha: 0.7 });
}

function drawReadingBook(ctx) {
  const { x, y, w, h, rot } = BOOK, hw = w / 2, hh = h / 2;
  ctx.save(); ctx.translate(x, y); ctx.rotate(rot);
  // back cover (left half): blurb, barcode with the real ISBN
  ctx.beginPath(); ctx.roundRect(-hw, -hh, hw - 3, h, [6, 0, 0, 6]); ctx.fillStyle = lin(ctx, -hw, 0, 0, 0, [[0, '#4a0010'], [1, '#8a0a18']]); ctx.fill(); outline(ctx, 2.2, '#1a0006');
  ctx.fillStyle = 'rgba(255,220,200,.7)'; for (let i = 0; i < 5; i++) ctx.fillRect(-hw + 14, -hh + 18 + i * 10, hw - 40 - (i % 2) * 14, 3);
  ctx.fillStyle = '#fff'; ctx.fillRect(-hw + 16, hh - 58, 78, 44);
  ctx.fillStyle = '#111'; for (let i = 0; i < 30; i++) ctx.fillRect(-hw + 20 + i * 2.4, hh - 54, (i * 7 % 3) ? 1 : 1.8, 28);
  text(ctx, '9 781451 673319', -hw + 55, hh - 18, '800 8.5px Nunito, sans-serif', '#111', { align: 'center' });
  // spine
  ctx.fillStyle = '#26000a'; ctx.fillRect(-4, -hh, 8, h);
  // front cover (right half): flames and the title
  ctx.beginPath(); ctx.roundRect(3, -hh, hw - 3, h, [0, 6, 6, 0]); ctx.fillStyle = lin(ctx, 0, -hh, 0, hh, [[0, '#ff4d00'], [0.6, '#c01020'], [1, '#5c0010']]); ctx.fill(); outline(ctx, 2.2, '#1a0006');
  flame(ctx, 62, hh - 8, 70, 86);
  text(ctx, 'FAHRENHEIT', 62, -hh + 30, '900 18px Nunito, sans-serif', '#fffe92', { align: 'center', line: '#5c0010', lw: 3 });
  text(ctx, '451', 62, -hh + 70, '900 40px Nunito, sans-serif', '#ffffff', { align: 'center', line: '#5c0010', lw: 4 });
  text(ctx, 'RAY BRADBURY', 62, -hh + 92, '800 12px Nunito, sans-serif', '#ffe1c8', { align: 'center' });
  markRect(ctx, -hw - 6, -hh - 6, w + 12, h + 12, 0.95);
  // her fingers over the top edge, near the corner
  [[52, 26], [66, 30], [80, 30], [94, 26]].forEach(([fx, len]) => {
    ctx.beginPath(); ctx.roundRect(fx - 6, -hh - 12, 12, len, [5, 5, 6, 6]);
    ctx.fillStyle = lin(ctx, fx - 6, 0, fx + 6, 0, [[0, C.skinLit], [1, '#f6c9c4']]); ctx.fill(); outline(ctx, 1.6, C.skinLine);
    ellipse(ctx, fx, -hh + len - 18, 3.4, 4, 0, 'rgba(255,0,91,.75)');
  });
  shape(ctx, [[48, -hh - 10], [70, -hh - 26], [100, -hh - 30], [114, -hh - 20], [104, -hh - 8], [70, -hh - 6]], C.skin);
  markRect(ctx, 40, -hh - 36, 80, 70, 0.95);
  ctx.restore();
}

// ─── Monero-chan (chibi), sitting on the pile with an XMR coin ───
function moneroLogo(ctx, x, y, r) {
  ctx.save(); ctx.translate(x, y);
  ellipse(ctx, 0, 0, r, r, 0, XMR.orange);
  ctx.save(); ellipse(ctx, 0, 0, r, r); ctx.clip(); ctx.fillStyle = XMR.grey; ctx.fillRect(-r, r * 0.42, 2 * r, r); ctx.restore();
  ctx.beginPath();
  ctx.moveTo(-r * 0.62, r * 0.42); ctx.lineTo(-r * 0.62, -r * 0.5); ctx.lineTo(0, r * 0.1); ctx.lineTo(r * 0.62, -r * 0.5); ctx.lineTo(r * 0.62, r * 0.42);
  ctx.lineTo(r * 0.36, r * 0.42); ctx.lineTo(r * 0.36, -r * 0.02); ctx.lineTo(0, r * 0.4); ctx.lineTo(-r * 0.36, -r * 0.02); ctx.lineTo(-r * 0.36, r * 0.42); ctx.closePath();
  ctx.fillStyle = '#ffffff'; ctx.fill();
  ellipse(ctx, 0, 0, r, r); outline(ctx, Math.max(1.2, r * 0.08), '#2b1200');
  ctx.restore();
}
function drawMoneroChan(ctx, x, y, s) {
  ctx.save(); ctx.translate(x, y); ctx.scale(s, s);
  const line = '#2b1a14';
  // hair behind: dark with orange underlayer
  shape(ctx, [[-48, -40], [-58, 10], [-54, 60], [-40, 88], [-20, 70], [20, 70], [40, 88], [54, 60], [58, 10], [48, -40], [0, -64]], '#262428');
  shape(ctx, [[-44, 10], [-50, 60], [-38, 84], [-26, 60], [-30, 20]], XMR.orange);
  shape(ctx, [[44, 10], [50, 60], [38, 84], [26, 60], [30, 20]], XMR.orange);
  // body: dark top, orange capelet with the logo, orange skirt, dark thigh-highs
  shape(ctx, [[-10, 76], [-16, 110], [-8, 124], [8, 124], [16, 110], [10, 76]], '#2a2a2e');
  shape(ctx, [[-18, 112], [-26, 132], [26, 132], [18, 112]], XMR.orange);
  for (const sx of [-1, 1]) { shape(ctx, [[sx * 8, 128], [sx * 16, 128], [sx * 18, 160], [sx * 10, 160]], '#2a2a2e'); ellipse(ctx, sx * 14, 162, 6, 4, 0, XMR.orange); }
  shape(ctx, [[-30, 70], [-34, 96], [-20, 104], [0, 98], [20, 104], [34, 96], [30, 70], [0, 62]], XMR.orange);
  trace(ctx, spline([[-30, 70], [-34, 96], [-20, 104], [0, 98], [20, 104], [34, 96], [30, 70], [0, 62]], true, 6)); outline(ctx, 1.8, line);
  // face
  shape(ctx, [[-36, -18], [-38, 16], [-28, 42], [-12, 56], [0, 59], [12, 56], [28, 42], [38, 16], [36, -18], [0, -40]], C.skin);
  ink(ctx, [[-37, 10], [-28, 42], [-12, 56], [0, 59], [12, 56], [28, 42], [37, 10]], 1.6, C.skinLine, {});
  // eyes: orange, one winking
  ctx.save(); ellipse(ctx, -16, 20, 9, 12); ctx.clip();
  ctx.fillStyle = lin(ctx, 0, 8, 0, 32, [[0, '#6a2200'], [0.55, XMR.orange], [1, '#ffd08a']]); ctx.fillRect(-26, 6, 20, 28);
  ellipse(ctx, -16, 21, 4, 6, 0, '#2a0e00'); ellipse(ctx, -19, 15, 3, 3.6, 0, '#fff');
  ctx.restore();
  ink(ctx, [[-27, 12], [-20, 6], [-10, 6], [-5, 11]], 2.6, line, {});
  ink(ctx, [[6, 20], [16, 15], [26, 20]], 2.6, line, {});
  ink(ctx, [[-4, 38], [0, 41], [5, 38]], 1.6, '#8a2c48', {});
  for (const bx of [-24, 22]) { ctx.save(); ctx.translate(bx, 34); ctx.scale(1, 0.5); ellipse(ctx, 0, 0, 8, 8, 0, 'rgba(255,120,120,.45)'); ctx.restore(); }
  // bangs, dark with orange streaks
  const bangs = [[-38, -8], [-40, -40], [-10, -58], [30, -52], [42, -20], [38, 4], [26, -14], [14, 6], [4, -12], [-8, 8], [-18, -10], [-30, 6]];
  shape(ctx, bangs, '#2e2b30');
  ink(ctx, [[-20, -40], [-14, -20], [-18, -6]], 5, XMR.orange, { alpha: 0.9 });
  ink(ctx, [[20, -44], [22, -22], [18, -8]], 4, XMR.orange, { alpha: 0.9 });
  trace(ctx, spline(bangs, true, 6)); outline(ctx, 1.6, line);
  // heart earrings with the M
  for (const ex of [-37, 37]) { heartShape(ctx, ex, 44, 7, '#f2f2f2'); }
  // the coin, held up
  for (const hx of [-12, 12]) ellipse(ctx, hx, 94, 6, 5, 0, C.skin);
  moneroLogo(ctx, 0, 84, 17);
  markRect(ctx, -60, -70, 120, 240, 0.95);
  ctx.restore();
}
function heartShape(ctx, x, y, s, fill) {
  ctx.save(); ctx.translate(x, y); ctx.scale(s / 10, s / 10);
  ctx.beginPath(); ctx.moveTo(0, 6); ctx.bezierCurveTo(-6, 1, -10, -2, -10, -6); ctx.bezierCurveTo(-10, -11, -3, -12, 0, -7); ctx.bezierCurveTo(3, -12, 10, -11, 10, -6); ctx.bezierCurveTo(10, -2, 6, 1, 0, 6);
  ctx.fillStyle = fill; ctx.fill(); outline(ctx, 1.2, '#333');
  ctx.restore();
}

// ─── the data wind and the embers ───
function dataStream(ctx, front) {
  const n = front ? 26 : 70;
  for (let i = 0; i < n; i++) {
    const t = R(), band = rand(-1, 1);
    const x = lerp(700, 40, t), y = 330 + band * 140 + Math.sin(t * 5 + band * 2) * 60 + t * 240;
    if (front && x > 360) continue;
    const col = plasma(rand(0.3, 1)), s = rand(3, front ? 8 : 6);
    ctx.save(); ctx.globalCompositeOperation = 'lighter';
    ctx.fillStyle = rad(ctx, x, y, 0, s * 3.5, [[0, 'rgba(255,220,160,.5)'], [1, 'rgba(255,120,200,0)']]); ctx.fillRect(x - s * 4, y - s * 4, s * 8, s * 8);
    ctx.restore();
    ctx.fillStyle = col; ctx.fillRect(x - s / 2, y - s / 2, s, s * 0.8);
    ctx.strokeStyle = col; ctx.globalAlpha = 0.5; ctx.lineWidth = s * 0.5; ctx.beginPath(); ctx.moveTo(x + s, y); ctx.lineTo(x + s * rand(4, 9), y - s * 0.6); ctx.stroke(); ctx.globalAlpha = 1;
  }
  if (!front) for (let i = 0; i < 7; i++) {
    const x = rand(80, 420), y = rand(380, 760);
    ctx.save(); ctx.translate(x, y); ctx.rotate(rand(-0.8, 0.8));
    ctx.fillStyle = '#fbf3e4'; ctx.fillRect(-9, -12, 18, 24); ctx.strokeStyle = '#3b2a4a'; ctx.lineWidth = 1.4; ctx.strokeRect(-9, -12, 18, 24);
    markRect(ctx, -14, -17, 28, 34, 0.5);
    ctx.fillStyle = 'rgba(80,60,90,.6)'; for (let k = 0; k < 4; k++) ctx.fillRect(-6, -8 + k * 5, 12, 1.4);
    ctx.restore();
  }
}
function embers(ctx) {
  const [bx, by] = [BOOK.x + 62, BOOK.y - 20];
  ctx.save(); ctx.globalCompositeOperation = 'lighter';
  ctx.fillStyle = rad(ctx, bx, by, 10, 150, [[0, 'rgba(255,140,40,.35)'], [1, 'rgba(255,60,0,0)']]); ctx.fillRect(bx - 160, by - 160, 320, 320);
  for (let i = 0; i < 16; i++) {
    const x = bx + rand(-60, 70) + i * 2, y = by - 90 - rand(0, 200), r = rand(1.5, 3.5);
    ctx.fillStyle = rad(ctx, x, y, 0, r * 4, [[0, 'rgba(255,230,140,1)'], [0.35, 'rgba(255,120,30,.7)'], [1, 'rgba(255,60,0,0)']]); ctx.fillRect(x - r * 4, y - r * 4, r * 8, r * 8);
  }
  ctx.restore();
}

// ─── lettering ───
const DOMAINS = ['annas-archive.gl', 'annas-archive.pk', 'annas-archive.gd'];
function drawLettering(ctx) {
  // ribbon with the live domains
  const ry = 1150;
  shape(ctx, [[120, ry - 26], [500, ry - 34], [880, ry - 26], [900, ry + 22], [500, ry + 16], [100, ry + 22]], AA.blue, 4);
  for (const s of [-1, 1]) shape(ctx, [[500 + s * 390, ry - 24], [500 + s * 450, ry - 20], [500 + s * 428, ry], [500 + s * 452, ry + 30], [500 + s * 396, ry + 20]], AA.deep, 3);
  trace(ctx, spline([[120, ry - 26], [500, ry - 34], [880, ry - 26], [900, ry + 22], [500, ry + 16], [100, ry + 22]], true, 4)); outline(ctx, 2.4, '#00284d');
  text(ctx, DOMAINS.join('  ·  '), 500, ry + 7, '900 25px Nunito, sans-serif', '#ffffff', { align: 'center' });
  markRect(ctx, 150, ry - 18, 700, 32, 0.95);
  // title
  const g = lin(ctx, 0, 990, 0, 1080, [[0, '#ffffff'], [0.45, AA.light], [1, AA.blue]]);
  ctx.save(); ctx.translate(500, 1082); ctx.rotate(-0.04);
  ctx.font = '400 124px "Kaushan Script", cursive'; ctx.textAlign = 'center'; ctx.lineJoin = 'round';
  ctx.lineWidth = 22; ctx.strokeStyle = '#00193a'; ctx.strokeText("Anna's Archive", 4, 6);
  ctx.lineWidth = 12; ctx.strokeStyle = AA.deep; ctx.strokeText("Anna's Archive", 0, 0);
  ctx.fillStyle = g; ctx.fillText("Anna's Archive", 0, 0);
  ctx.restore();
  markRect(ctx, 60, 960, 880, 150, 0.9);
}

// ─── the whole underdrawing ───
function drawScene(ctx) {
  FLOWS.length = 0; DETAIL.length = 0;
  drawIsbnMap(ctx);
  drawIsbnPin(ctx);
  scraperBot(ctx, 88, 300, 0.95, -0.12, 'GPTBot', [70, 440]);
  scraperBot(ctx, 610, 74, 0.72, 0.1, 'CCBot', [560, 40]);
  scraperBot(ctx, 830, 470, 0.9, 0.12, 'ClaudeBot', [808, 376]);
  drawCard(ctx, 700, 70, 0.06);
  drawBookPile(ctx);
  bookTower(ctx, 96, 900, 610, 26, 110, 150);
  bookTower(ctx, 900, 900, 660, -20, 110, 150);
  dataStream(ctx, false);
  drawBackHair(ctx);
  drawBody(ctx);
  withHead(ctx, () => { drawHeadFront(ctx); drawFrontHair(ctx); });
  drawReadingBook(ctx);
  drawMoneroChan(ctx, 896, 540, 0.95);
  embers(ctx);
  dataStream(ctx, true);
  drawLettering(ctx);
}
