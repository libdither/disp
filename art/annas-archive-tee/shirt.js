'use strict';
// T-shirt mockup: fabric, folds and stitching drawn around the painting.
const SHIRTS = [
  ['Midnight', '#26213a'], ['Cloud white', '#f6f4f1'], ['Sakura', '#ffd6e7'], ['Lavender', '#dcd0f7'],
  ['Sky', '#cde5fb'], ['Mint', '#cbf0e1'], ['Butter', '#fff0c0'], ['Heather', '#b9b5c1'], ['Black', '#141216'],
];
const PRINT = { x: 300, y: 222, w: 400, h: 480 };
function shirtPath(ctx, fresh = true) {
  if (fresh) ctx.beginPath();
  ctx.moveTo(392, 118);
  ctx.quadraticCurveTo(500, 146, 608, 118);
  ctx.quadraticCurveTo(690, 150, 768, 166);
  ctx.quadraticCurveTo(872, 236, 940, 356);
  ctx.lineTo(828, 458);
  ctx.quadraticCurveTo(782, 418, 738, 372);
  ctx.quadraticCurveTo(724, 700, 730, 1030);
  ctx.quadraticCurveTo(500, 1052, 270, 1030);
  ctx.quadraticCurveTo(276, 700, 262, 372);
  ctx.quadraticCurveTo(218, 418, 172, 458);
  ctx.lineTo(60, 356);
  ctx.quadraticCurveTo(128, 236, 232, 166);
  ctx.quadraticCurveTo(310, 150, 392, 118);
  ctx.closePath();
}
function luminance(hex) { const n = parseInt(hex.slice(1), 16); return (0.299 * (n >> 16) + 0.587 * (n >> 8 & 255) + 0.114 * (n & 255)) / 255; }
/// Soft shape via the offscreen-shadow trick, so blur works in every browser.
function softStroke(ctx, K, pts, width, color, blur) {
  ctx.save(); ctx.shadowColor = color; ctx.shadowBlur = blur * K; ctx.shadowOffsetX = 4000 * K;
  ctx.lineWidth = width; ctx.lineCap = 'round'; ctx.strokeStyle = '#000';
  trace(ctx, spline(pts, false, 10).map(([x, y]) => [x - 4000, y]), false); ctx.stroke(); ctx.restore();
}
function drawShirt(ctx, W, H, color, art) {
  const K = Math.min(W / 1000, H / 1100), lum = luminance(color);
  ctx.save(); ctx.setTransform(1, 0, 0, 1, 0, 0); ctx.clearRect(0, 0, W, H);
  ctx.translate((W - 1000 * K) / 2, (H - 1100 * K) / 2); ctx.scale(K, K);
  // drop shadow onto the page
  ctx.save(); ctx.shadowColor = 'rgba(70,40,90,.28)'; ctx.shadowBlur = 44 * K; ctx.shadowOffsetY = 20 * K;
  shirtPath(ctx); ctx.fillStyle = color; ctx.fill(); ctx.restore();
  ctx.save(); shirtPath(ctx); ctx.clip();
  // inside of the back neck
  ctx.beginPath(); ctx.moveTo(392, 118); ctx.quadraticCurveTo(500, 146, 608, 118); ctx.quadraticCurveTo(560, 196, 500, 196); ctx.quadraticCurveTo(440, 196, 392, 118);
  ctx.fillStyle = color; ctx.fill(); ctx.fillStyle = lum > 0.5 ? 'rgba(80,60,100,.22)' : 'rgba(0,0,0,.35)'; ctx.fill();
  ctx.beginPath(); ctx.moveTo(398, 121); ctx.quadraticCurveTo(500, 150, 602, 121); ctx.lineWidth = 11; ctx.strokeStyle = lum > 0.5 ? 'rgba(80,60,100,.12)' : 'rgba(255,255,255,.06)'; ctx.stroke();
  ctx.beginPath(); ctx.roundRect(484, 138, 32, 16, 3); ctx.fillStyle = '#ffb3d6'; ctx.fill();
  ctx.fillStyle = '#4a2350'; ctx.font = '400 10px "Mochiy Pop One", sans-serif'; ctx.textAlign = 'center'; ctx.textBaseline = 'middle'; ctx.fillText('A', 500, 147);
  // collar rib
  ctx.beginPath(); ctx.moveTo(392, 118); ctx.quadraticCurveTo(440, 196, 500, 196); ctx.quadraticCurveTo(560, 196, 608, 118);
  ctx.lineWidth = 30; ctx.strokeStyle = color; ctx.stroke();
  ctx.lineWidth = 30; ctx.strokeStyle = lum > 0.5 ? 'rgba(90,70,110,.08)' : 'rgba(255,255,255,.05)'; ctx.stroke();
  ctx.beginPath(); ctx.moveTo(380, 122); ctx.quadraticCurveTo(438, 214, 500, 214); ctx.quadraticCurveTo(562, 214, 620, 122);
  ctx.setLineDash([5, 4]); ctx.lineWidth = 1.4; ctx.strokeStyle = lum > 0.5 ? 'rgba(80,60,100,.35)' : 'rgba(255,255,255,.22)'; ctx.stroke(); ctx.setLineDash([]);
  // the print
  ctx.drawImage(art, PRINT.x, PRINT.y, PRINT.w, PRINT.h);
  // fabric shading over everything, print included
  ctx.globalCompositeOperation = 'multiply';
  const shade = a => lum > 0.5 ? `rgba(110,90,140,${a * 0.5})` : `rgba(0,0,0,${a})`;
  ctx.save(); ctx.shadowColor = shade(0.5); ctx.shadowBlur = 80 * K;
  ctx.beginPath(); ctx.rect(-4000, -4000, 9000, 9000); shirtPath(ctx, false); ctx.fillStyle = '#000'; ctx.fill('evenodd'); ctx.restore();
  const folds = [
    [[[268, 392], [320, 438], [380, 470]], 16, 30, 0.45], [[[732, 392], [680, 438], [620, 470]], 16, 30, 0.45],
    [[[200, 240], [160, 320], [130, 380]], 14, 34, 0.35], [[[800, 240], [840, 320], [870, 380]], 14, 34, 0.35],
    [[[350, 1030], [356, 960]], 10, 30, 0.25], [[[640, 1030], [632, 970]], 10, 30, 0.25],
  ];
  for (const [pts, w, b, a] of folds) softStroke(ctx, K, pts, w, shade(a), b);
  ctx.globalCompositeOperation = 'screen';
  const hi = lum > 0.5 ? 'rgba(255,255,255,.55)' : 'rgba(170,150,210,.22)';
  const lights = [[[[292, 410], [340, 452], [392, 486]], 10, 20], [[[708, 410], [660, 452], [608, 486]], 10, 20], [[[368, 1020], [376, 940]], 10, 22], [[[612, 1020], [604, 950]], 10, 22], [[[210, 250], [172, 330]], 10, 22], [[[500, 260], [500, 900]], 120, 120]];
  for (const [pts, w, b] of lights) softStroke(ctx, K, pts, w, hi, b);
  ctx.globalCompositeOperation = 'source-over';
  // stitching
  ctx.setLineDash([6, 5]); ctx.lineWidth = 1.5; ctx.strokeStyle = lum > 0.5 ? 'rgba(80,60,100,.35)' : 'rgba(255,255,255,.2)';
  for (const seg of [[[69, 346], [181, 448]], [[931, 346], [819, 448]]]) { ctx.beginPath(); ctx.moveTo(...seg[0]); ctx.lineTo(...seg[1]); ctx.stroke(); }
  ctx.beginPath(); ctx.moveTo(272, 1014); ctx.quadraticCurveTo(500, 1036, 728, 1014); ctx.stroke();
  ctx.beginPath(); ctx.moveTo(232, 166); ctx.quadraticCurveTo(262, 270, 262, 372); ctx.stroke();
  ctx.beginPath(); ctx.moveTo(768, 166); ctx.quadraticCurveTo(738, 270, 738, 372); ctx.stroke();
  ctx.setLineDash([]);
  ctx.restore();
  shirtPath(ctx); ctx.lineWidth = 1.5; ctx.strokeStyle = lum > 0.5 ? 'rgba(80,60,100,.25)' : 'rgba(255,255,255,.12)'; ctx.stroke();
  ctx.restore();
}

