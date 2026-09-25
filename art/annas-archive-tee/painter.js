'use strict';
// Turns the underdrawing (scene.js) into brush strokes, the way a painter blocks in and refines:
// big brushes first, then smaller brushes only where the canvas still differs from the underdrawing.
// After Hertzmann, "Painterly Rendering with Curved Brush Strokes of Multiple Sizes" (SIGGRAPH 1998).
// The strokes are resolution-independent (design units), so the print file replays the same painting.

const PAINTER = {
  RS: 1.4, // underdrawing pixels per design unit
  layers: [
    { r: 14, T: 0, len: [3, 12], fc: 0.85, imp: 0, jit: 16 },
    { r: 8, T: 30, len: [3, 12], fc: 0.8, imp: 0, jit: 12 },
    { r: 4.4, T: 30, len: [2, 10], fc: 0.72, imp: 0, jit: 9 },
    { r: 2.4, T: 36, len: [2, 10], fc: 0.65, imp: 0.45, jit: 6 },
    { r: 1.35, T: 34, len: [2, 10], fc: 0.6, imp: 0.9, jit: 4 },
    { r: 0.8, T: 32, len: [2, 8], fc: 0.5, imp: 0.9, jit: 2 },
    { r: 0.5, T: 24, len: [2, 8], fc: 0.5, imp: 0.99, jit: 1 },
  ],
};

/// Draws the underdrawing plus its two helper maps: brush direction (hair, limbs) and where detail matters.
function buildUnderdrawing(sceneSeed) {
  const RS = PAINTER.RS, W = Math.round(DW * RS), H = Math.round(DH * RS);
  const mk = () => { const c = document.createElement('canvas'); c.width = W; c.height = H; return c; };
  const ref = mk(), rctx = ref.getContext('2d', { willReadFrequently: true });
  rctx.setTransform(RS, 0, 0, RS, 0, 0);
  R = mulberry32(sceneSeed);
  drawScene(rctx);
  const dev = (m, [x, y]) => [m.a * x + m.c * y + m.e, m.b * x + m.d * y + m.f];

  const flow = mk(), fctx = flow.getContext('2d', { willReadFrequently: true });
  for (const { m, r } of FLOWS) {
    const L = r.left.map(p => dev(m, p)), Rr = r.right.map(p => dev(m, p)), Cn = r.center.map(p => dev(m, p));
    for (let i = 0; i < Cn.length - 1; i++) {
      const phi = Math.atan2(Cn[i + 1][1] - Cn[i][1], Cn[i + 1][0] - Cn[i][0]);
      fctx.fillStyle = `rgb(${Math.round((Math.cos(2 * phi) + 1) * 127.5)},${Math.round((Math.sin(2 * phi) + 1) * 127.5)},255)`;
      fillPoly(fctx, [L[i], L[i + 1], Rr[i + 1], Rr[i]]);
    }
  }
  const imp = mk(), ictx = imp.getContext('2d', { willReadFrequently: true });
  ictx.fillStyle = 'rgb(51,51,51)'; ictx.fillRect(0, 0, W, H);
  ictx.fillStyle = 'rgb(128,128,128)';
  for (const { m, r } of FLOWS) fillPoly(ictx, r.poly.map(p => dev(m, p)));
  for (const { m, pts, level } of DETAIL) { const v = Math.round(level * 255); fillPoly(ictx, pts.map(p => dev(m, p)), `rgb(${v},${v},${v})`); }
  return { W, H, ref, flow, imp, refData: rctx.getImageData(0, 0, W, H).data, flowData: fctx.getImageData(0, 0, W, H).data, impData: ictx.getImageData(0, 0, W, H).data };
}

/// Three box blurs ≈ a gaussian; runs in place on a Float32Array channel.
function blurChannel(src, W, H, sigma, tmp) {
  const r = Math.max(0, Math.round((Math.sqrt(4 * sigma * sigma + 1) - 1) / 2));
  if (!r) return;
  for (let pass = 0; pass < 3; pass++) {
    for (let y = 0; y < H; y++) {
      const o = y * W; let acc = 0;
      for (let x = -r; x <= r; x++) acc += src[o + clamp(x, 0, W - 1)];
      for (let x = 0; x < W; x++) {
        tmp[o + x] = acc / (2 * r + 1);
        acc += src[o + Math.min(W - 1, x + r + 1)] - src[o + Math.max(0, x - r)];
      }
    }
    for (let x = 0; x < W; x++) {
      let acc = 0;
      for (let y = -r; y <= r; y++) acc += tmp[clamp(y, 0, H - 1) * W + x];
      for (let y = 0; y < H; y++) {
        src[y * W + x] = acc / (2 * r + 1);
        acc += tmp[Math.min(H - 1, y + r + 1) * W + x] - tmp[Math.max(0, y - r) * W + x];
      }
    }
  }
}

/// Paints layer after layer; calls onLayer(newStrokes, layerIndex) so the page can show progress.
async function paintStrokes(ud, seed, onLayer) {
  const { W, H, refData, flowData, impData } = ud, N = W * H, RS = PAINTER.RS;
  const rnd = mulberry32(seed);
  const work = document.createElement('canvas'); work.width = W; work.height = H;
  const wctx = work.getContext('2d', { willReadFrequently: true });
  wctx.lineCap = 'round'; wctx.lineJoin = 'round';
  const pr = new Float32Array(N), pg = new Float32Array(N), pb = new Float32Array(N), pa = new Float32Array(N), tmp = new Float32Array(N);
  const br = new Float32Array(N), bg = new Float32Array(N), bb = new Float32Array(N), lum = new Float32Array(N);
  const all = [];
  for (let li = 0; li < PAINTER.layers.length; li++) {
    const L = PAINTER.layers[li], rp = L.r * RS;
    // blurred, premultiplied copy of the underdrawing at this brush size
    for (let i = 0, j = 0; i < N; i++, j += 4) { const a = refData[j + 3] / 255; pr[i] = refData[j] * a; pg[i] = refData[j + 1] * a; pb[i] = refData[j + 2] * a; pa[i] = a; }
    const sigma = rp * 0.5;
    for (const ch of [pr, pg, pb, pa]) blurChannel(ch, W, H, sigma, tmp);
    for (let i = 0; i < N; i++) {
      const a = pa[i] || 1e-6; br[i] = pr[i] / a; bg[i] = pg[i] / a; bb[i] = pb[i] / a;
      lum[i] = 0.3 * br[i] + 0.59 * bg[i] + 0.11 * bb[i];
    }
    const cur = wctx.getImageData(0, 0, W, H).data;
    const diff = i => { const j = i * 4; if (cur[j + 3] < 128) return 999; const dr = cur[j] - br[i], dg = cur[j + 1] - bg[i], db = cur[j + 2] - bb[i]; return Math.sqrt(dr * dr + dg * dg + db * db); };
    const grid = Math.max(2, Math.round(rp * 1.2)), strokes = [];
    for (let y0 = 0; y0 < H; y0 += grid) {
      for (let x0 = 0; x0 < W; x0 += grid) {
        const ci = Math.min(H - 1, y0 + (grid >> 1)) * W + Math.min(W - 1, x0 + (grid >> 1));
        const im = impData[ci * 4] / 255;
        if (im < L.imp) continue;
        let sum = 0, cnt = 0, best = -1, bi = -1;
        for (let y = y0; y < Math.min(H, y0 + grid); y++) for (let x = x0; x < Math.min(W, x0 + grid); x++) {
          const i = y * W + x;
          if (refData[i * 4 + 3] < 128) continue;
          const d = diff(i); sum += d; cnt++;
          if (d > best) { best = d; bi = i; }
        }
        // background tolerates more difference than faces and lettering
        if (cnt < grid * grid * 0.3 || sum / cnt <= L.T * (1.6 - Math.min(im, 0.6))) continue;
        strokes.push(traceStroke(bi % W, (bi / W) | 0, im));
      }
    }
    function traceStroke(x0, y0, im) {
      const i0 = y0 * W + x0, j = L.jit * (im > 0.99 ? 0.2 : im > 0.8 ? 0.4 : 1), k = (rnd() - 0.5) * j;
      const col = [clamp(br[i0] + k + (rnd() - 0.5) * j * 0.6, 0, 255), clamp(bg[i0] + k + (rnd() - 0.5) * j * 0.6, 0, 255), clamp(bb[i0] + k + (rnd() - 0.5) * j * 0.6, 0, 255)];
      const pts = [x0, y0];
      let x = x0, y = y0, ldx = 0, ldy = 0;
      for (let n = 1; n <= L.len[1]; n++) {
        const xi = Math.round(x), yi = Math.round(y), i = yi * W + xi;
        if (n > L.len[0]) {
          const dc = diff(i), ds = Math.hypot(br[i] - col[0], bg[i] - col[1], bb[i] - col[2]);
          if (dc < ds) break;
        }
        let dx, dy;
        const fj = i * 4;
        if (flowData[fj + 2] > 200 && flowData[fj + 3] > 200) {
          const phi = Math.atan2(flowData[fj + 1] / 127.5 - 1, flowData[fj] / 127.5 - 1) / 2;
          dx = Math.cos(phi); dy = Math.sin(phi);
        } else {
          const gx = lum[yi * W + Math.min(W - 1, xi + 1)] - lum[yi * W + Math.max(0, xi - 1)];
          const gy = lum[Math.min(H - 1, yi + 1) * W + xi] - lum[Math.max(0, yi - 1) * W + xi];
          const mag = Math.hypot(gx, gy);
          if (mag < 0.5) {
            if (n === 1) { const a = 0.9 + (rnd() - 0.5) * 0.3; dx = Math.cos(a); dy = Math.sin(a); } else { dx = ldx; dy = ldy; }
          } else { dx = -gy / mag; dy = gx / mag; }
        }
        if (dx * ldx + dy * ldy < 0) { dx = -dx; dy = -dy; }
        if (n > 1) { dx = L.fc * dx + (1 - L.fc) * ldx; dy = L.fc * dy + (1 - L.fc) * ldy; const m = Math.hypot(dx, dy) || 1; dx /= m; dy /= m; }
        const nx = x + rp * dx, ny = y + rp * dy;
        if (nx < 0 || ny < 0 || nx >= W || ny >= H || refData[(Math.round(ny) * W + Math.round(nx)) * 4 + 3] < 100) break;
        x = nx; y = ny; ldx = dx; ldy = dy; pts.push(x, y);
      }
      if (pts.length === 2) { const a = 0.75 + (rnd() - 0.5); pts.push(x0 + Math.cos(a) * rp * 0.6, y0 + Math.sin(a) * rp * 0.6); }
      const p = new Float32Array(pts.length);
      for (let q = 0; q < pts.length; q++) p[q] = pts[q] / RS;
      return { p, r: L.r * (0.85 + rnd() * 0.3), c: col.map(Math.round), a: refData[i0 * 4 + 3] / 255, seed: (rnd() * 1e9) | 0 };
    }
    for (let i = strokes.length - 1; i > 0; i--) { const j = Math.floor(rnd() * (i + 1)); [strokes[i], strokes[j]] = [strokes[j], strokes[i]]; }
    // simple round-brush version on the working canvas, only used to measure what is left to paint
    wctx.setTransform(RS, 0, 0, RS, 0, 0);
    for (const s of strokes) {
      wctx.globalAlpha = s.a; wctx.lineWidth = 2 * s.r; wctx.strokeStyle = `rgb(${s.c[0]},${s.c[1]},${s.c[2]})`;
      wctx.beginPath(); wctx.moveTo(s.p[0], s.p[1]); for (let q = 2; q < s.p.length; q += 2) wctx.lineTo(s.p[q], s.p[q + 1]); wctx.stroke();
    }
    wctx.setTransform(1, 0, 0, 1, 0, 0); wctx.globalAlpha = 1;
    all.push(...strokes);
    if (onLayer) await onLayer(strokes, li);
  }
  return all;
}

// ─── rendering the strokes with a bristly, impasto brush ───
const rgb = (c, d = 0) => `rgb(${clamp(c[0] + d, 0, 255) | 0},${clamp(c[1] + d, 0, 255) | 0},${clamp(c[2] + d, 0, 255) | 0})`;
function offsetPath(ctx, p, off) {
  const n = p.length / 2;
  ctx.beginPath();
  for (let i = 0; i < n; i++) {
    const a = Math.max(0, i - 1) * 2, b = Math.min(n - 1, i + 1) * 2;
    let tx = p[b] - p[a], ty = p[b + 1] - p[a + 1]; const m = Math.hypot(tx, ty) || 1; tx /= m; ty /= m;
    const x = p[i * 2] - ty * off, y = p[i * 2 + 1] + tx * off;
    i ? ctx.lineTo(x, y) : ctx.moveTo(x, y);
  }
}
function renderStroke(ctx, s) {
  const { p, r, c, a } = s, rr = mulberry32(s.seed);
  ctx.globalAlpha = a * 0.94; ctx.lineWidth = 2 * r; ctx.strokeStyle = rgb(c);
  offsetPath(ctx, p, 0); ctx.stroke();
  if (r < 3.6) return;
  // bristle streaks and a lit ridge of paint on the upper-left side
  const n = r > 6 ? 5 : 3;
  ctx.lineWidth = Math.max(0.35, r * 0.26);
  for (let k = 0; k < n; k++) {
    ctx.globalAlpha = a * (0.22 + rr() * 0.18);
    ctx.strokeStyle = rgb(c, (rr() - 0.5) * 46);
    offsetPath(ctx, p, ((k + 0.5) / n - 0.5) * 1.6 * r); ctx.stroke();
  }
  const tx = p[p.length - 2] - p[0], ty = p[p.length - 1] - p[1], side = (-ty - tx) < 0 ? 1 : -1;
  ctx.globalAlpha = a * 0.28; ctx.lineWidth = Math.max(0.3, r * 0.22); ctx.strokeStyle = rgb(c, 48);
  offsetPath(ctx, p, side * r * 0.62); ctx.stroke();
  ctx.globalAlpha = a * 0.2; ctx.strokeStyle = rgb(c, -50);
  offsetPath(ctx, p, -side * r * 0.7); ctx.stroke();
}
function renderStrokes(ctx, strokes, from = 0, to = strokes.length) {
  ctx.lineCap = 'round'; ctx.lineJoin = 'round';
  for (let i = from; i < to; i++) renderStroke(ctx, strokes[i]);
  ctx.globalAlpha = 1;
}
/// Canvas weave over the paint only.
function weaveTile() {
  const c = document.createElement('canvas'); c.width = c.height = 8;
  const x = c.getContext('2d');
  x.fillStyle = 'rgba(255,255,255,.55)'; x.fillRect(0, 0, 8, 3); x.fillRect(0, 4, 3, 4);
  x.fillStyle = 'rgba(0,0,0,.45)'; x.fillRect(3, 3, 5, 1); x.fillRect(3, 4, 1, 4); x.fillRect(7, 0, 1, 3);
  return c;
}
function renderWeave(ctx) {
  ctx.save(); ctx.globalCompositeOperation = 'source-atop'; ctx.globalAlpha = 0.16;
  const pat = ctx.createPattern(weaveTile(), 'repeat'); pat.setTransform(new DOMMatrix().scale(0.32));
  ctx.fillStyle = pat; ctx.fillRect(0, 0, DW, DH); ctx.restore();
}
/// The finished painting at `scale` pixels per design unit.
function renderPainting(strokes, scale) {
  const c = document.createElement('canvas'); c.width = Math.round(DW * scale); c.height = Math.round(DH * scale);
  const ctx = c.getContext('2d'); ctx.setTransform(scale, 0, 0, scale, 0, 0);
  renderStrokes(ctx, strokes); renderWeave(ctx);
  return c;
}
