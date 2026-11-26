// CORE START
const Q = 1.602176634e-19, KT = 1.380649e-23 * 300, PHI = KT / Q;
const DEV = { vt: 0.35, n: 1.5, is: 1e-6 };
const F = x => { const h = x / 2, s = h > 30 ? h : Math.log1p(Math.exp(h)); return s * s; };

/// net pull-up current, net pull-down current, and the sum of all four one-way currents (for shot noise)
function inv(vin, vout, vdd, o) {
  const a = (vin - DEV.vt) / DEV.n / PHI, b = (vdd - vin - DEV.vt) / DEV.n / PHI;
  const nf = DEV.is * F(a), nr = DEV.is * F(a - vout / PHI);
  const pf = DEV.is * F(b), pr = DEV.is * F(b - (vdd - vout) / PHI);
  o.up = pf - pr; o.down = nf - nr; o.shot = pf + pr + nf + nr;
}

function vtcAt(vin, vdd) {
  const o = {}; let lo = 0, hi = vdd;
  for (let i = 0; i < 60; i++) { const m = (lo + hi) / 2; inv(vin, m, vdd, o); if (o.up - o.down > 0) lo = m; else hi = m; }
  return (lo + hi) / 2;
}

function rng(seed) {
  let s = (seed >>> 0) || 1, spare = null;
  const u = () => { s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return (s + 0.5) / 4294967296; };
  u.gauss = () => {
    if (spare !== null) { const g = spare; spare = null; return g; }
    const r = Math.sqrt(-2 * Math.log(u())), t = 2 * Math.PI * u(); spare = r * Math.sin(t); return r * Math.cos(t);
  };
  return u;
}

/// static curve, gain, noise margins, and the delay table, all from the fine model
function characterize(vdd, C) {
  const G = 241, vin = [], vout = [];
  for (let i = 0; i < G; i++) { vin.push(vdd * i / (G - 1)); vout.push(vtcAt(vin[i], vdd)); }
  let gain = 0, vil = null, vih = null;
  for (let i = 1; i < G; i++) {
    const s = (vout[i] - vout[i - 1]) / (vin[i] - vin[i - 1]);
    gain = Math.max(gain, -s);
    if (s <= -1) { if (vil === null) vil = vin[i - 1]; vih = vin[i]; }
  }
  let margin = 0, nml = 0, nmh = 0;
  if (vil !== null) { nml = vil - vtcAt(vih, vdd); nmh = vtcAt(vil, vdd) - vih; margin = Math.max(0, Math.min(nml, nmh)); }

  const o = {}; inv(0, vdd / 2, vdd, o);
  const d0 = C * vdd / 2 / o.up, dt = d0 / 50, M = 10;
  const v = new Float64Array(M + 1), nv = new Float64Array(M + 1), tc = new Array(M + 1).fill(null), side = [];
  for (let i = 1; i <= M; i++) v[i] = vtcAt(v[i - 1], vdd);
  for (let i = 1; i <= M; i++) side[i] = v[i] > vdd / 2;
  v[0] = vdd;
  for (let s = 1; s < 50 * 400 && tc[M - 1] === null; s++) {
    for (let i = 1; i <= M; i++) { inv(v[i - 1], v[i], vdd, o); nv[i] = v[i] + (o.up - o.down) / C * dt; }
    for (let i = 1; i <= M; i++) { v[i] = nv[i]; if (tc[i] === null && (v[i] > vdd / 2) !== side[i]) tc[i] = s * dt; }
  }
  const dies = tc[M - 1] === null;
  const d = dies ? d0 : (tc[M - 1] - tc[2]) / (M - 3), d1 = dies ? d0 : tc[1];
  return { vin, vout, gain, vil, vih, margin, nml, nmh, d, d1, dies };
}

/// run the fine model, read it through the lens, and score the coarse model against it
function simulate(p) {
  const { vdd, C, N, period, clocked, k, feedTau, decap, nBits, seed, windowBits = 24, rowsTarget = 480, record = true } = p;
  const ch = characterize(vdd, C);
  const d = ch.d, SPD = 50, dt = d / SPD, spb = Math.max(1, Math.round(period * SPD)), Tb = spb * dt;
  const r = rng(seed), bits = new Uint8Array(nBits);
  for (let i = 0; i < nBits; i++) bits[i] = r() < 0.5 ? 1 : 0;
  const bitAt = m => (m < 0 ? 0 : bits[Math.min(m, nBits - 1)]);
  const half = vdd / 2, par = (k - 1) & 1;
  const isReg = new Uint8Array(N + 1);
  if (clocked) for (let i = k; i <= N; i += k) isReg[i] = 1;
  const regs = []; for (let i = 1; i <= N; i++) if (isReg[i]) regs.push(i);

  const v = new Float64Array(N + 1), up = new Float64Array(N + 1), dn = new Float64Array(N + 1), sh = new Float64Array(N + 1);
  const latch = new Uint8Array(N + 1), sup = new Float64Array(N + 1).fill(vdd), nsup = new Float64Array(N + 1);
  for (let i = 1; i <= N; i++) {
    if (isReg[i]) { latch[i] = v[i - 1] > half ? 1 : 0; v[i] = vtcAt(latch[i] ? 0 : vdd, vdd); }
    else v[i] = vtcAt(v[i - 1], vdd);
  }
  const Cd = Math.max(decap, 0.01) * C, R = feedTau > 0 ? feedTau * d / Cd : 0;

  const D = i => ch.d1 + (i - 1) * d;
  const TOL = 5, L = SPD + TOL + 1, ring = new Uint8Array((N + 1) * L);
  const spr = record ? Math.max(1, Math.round(windowBits * spb / rowsTarget)) : Math.max(1, Math.round(spb / 8));
  const rows = record ? Math.floor(windowBits * spb / spr) : 0;
  const fine = record ? new Float32Array(rows * N) : null, lens = record ? new Uint8Array(rows * N) : null;
  const coarse = record ? new Int8Array(rows * N).fill(-1) : null, bad = record ? new Uint8Array(rows * N) : null;
  const supv = record ? new Float32Array(rows * N) : null;
  const HB = 96, hist = new Float64Array(HB);
  const stageBad = new Float64Array(N + 1), stageN = new Float64Array(N + 1);
  let e2eBad = 0, e2eN = 0, oneBad = 0, oneN = 0, energy = 0;
  let prevRead = regs.map((i, j) => (par * ((j + 1) & 1)));
  let prevIn = 0;
  const o = {}, total = nBits * spb;

  for (let s = 0; s < total; s++) {
    const m = Math.floor(s / spb), edge = s % spb === 0;
    if (edge) {
      if (clocked && m >= 1) {
        const now = regs.map(i => (v[i] > half ? 1 : 0));
        for (let j = 0; j < regs.length; j++) {
          const i = regs[j], jj = j + 1;
          const e2e = bitAt(m - 1 - jj) ^ (par & jj);
          const one = (j === 0 ? prevIn : prevRead[j - 1]) ^ par;
          { e2eN++; stageN[i]++; if (now[j] !== e2e) { e2eBad++; stageBad[i]++; if (record) markEdge(i, s); } }
          if (m >= 2) { oneN++; if (now[j] !== one) oneBad++; }
        }
        prevRead = now; prevIn = bits[m - 1];
        for (const i of regs) latch[i] = v[i - 1] > half ? 1 : 0;
      }
      v[0] = bits[m] * vdd;
    }
    for (let i = 1; i <= N; i++) {
      const vl = sup[i], vin = isReg[i] ? (latch[i] ? 0 : vl) : v[i - 1];
      inv(vin, v[i], vl, o); up[i] = o.up; dn[i] = o.down; sh[i] = o.shot;
    }
    for (let i = 1; i <= N; i++) {
      v[i] += (up[i] - dn[i]) / C * dt + Math.sqrt(Q * sh[i] * dt) / C * r.gauss();
      if (R > 0) {
        const feed = (vdd - sup[i]) / R, l = i > 1 ? sup[i - 1] : sup[i], rr = i < N ? sup[i + 1] : sup[i];
        nsup[i] = sup[i] + (feed + (l + rr - 2 * sup[i]) / R - up[i]) / Cd * dt;
        energy += vdd * feed * dt;
      } else energy += vdd * up[i] * dt;
    }
    if (R > 0) for (let i = 1; i <= N; i++) sup[i] = nsup[i];
    if (!clocked) for (let i = 1; i <= N; i++) ring[i * L + (s % L)] = v[i] > half ? 1 : 0;

    if (s % spr === 0) {
      const row = record ? s / spr : -1, t = s * dt;
      for (let i = 1; i <= N; i++) {
        const x = v[i] / vdd, b = Math.floor((x + 0.1) / 1.2 * HB);
        if (b >= 0 && b < HB) hist[b]++;
        const lb = v[i] > half ? 1 : 0;
        let pred = -1, ok = true;
        if (!clocked) {
          const predAt = tt => bitAt(Math.floor((tt - D(i)) / Tb)) ^ (i & 1);
          pred = predAt(t);
          ok = lb === pred || lb === predAt(t - TOL * dt) || lb === predAt(t + TOL * dt);
          e2eN++; stageN[i]++; if (!ok) { e2eBad++; stageBad[i]++; }
          if (i >= 2 && s >= SPD + TOL) {
            let one = false;
            for (let lag = SPD - TOL; lag <= SPD + TOL && !one; lag++) one = lb === 1 - ring[(i - 1) * L + ((s - lag) % L)];
            oneN++; if (!one) oneBad++;
          }
        } else if (isReg[i]) {
          const jj = regs.indexOf(i) + 1; pred = bitAt(m - jj) ^ (par & jj);
        }
        if (record && row < rows) {
          const at = row * N + (i - 1);
          fine[at] = x; lens[at] = lb; coarse[at] = pred; supv[at] = sup[i] / vdd;
          if (!clocked && !ok) bad[at] = 1;
        }
      }
    }
  }
  function markEdge(i, s) {
    const row = Math.round(s / spr); if (row >= rows) return;
    for (let k2 = Math.max(0, row - 3); k2 < Math.min(rows, row + 1); k2++) bad[k2 * N + (i - 1)] = 1;
  }
  const stage = []; for (let i = 1; i <= N; i++) if (stageN[i]) stage.push([i, stageBad[i] / stageN[i]]);
  return { ch, d, dt, spr, rows, N, fine, lens, coarse, bad, supv, hist, stage,
    e2e: e2eN ? e2eBad / e2eN : 0, one: oneN ? oneBad / oneN : 0,
    energyPerStageBit: energy / (N * nBits), sigma: Math.sqrt(KT / C), electrons: C * vdd / Q, Tb };
}

DEV.is *= characterize(1, 1e-15).d / 15e-12;
// CORE END

const $ = id => document.getElementById(id);
const tip = $("tip");
const vddOf = x => 0.03 * Math.pow(1.2 / 0.03, x / 1000);
const capOf = x => 0.005e-15 * Math.pow(1 / 0.005, x / 1000);
const fmtT = s => s < 1e-9 ? (s * 1e12).toPrecision(3) + " ps" : s < 1e-6 ? (s * 1e9).toPrecision(3) + " ns" : (s * 1e6).toPrecision(3) + " µs";
const fmtE = j => j < 1e-15 ? (j * 1e18).toPrecision(3) + " aJ" : (j * 1e15).toPrecision(3) + " fJ";
const pct = x => x === 0 ? "0%" : x < 1e-3 ? (x * 100).toPrecision(2) + "%" : (x * 100).toPrecision(3) + "%";
let seed = 7, sweepCache = null, sweepKey = "";

const presets = {
  healthy: { vdd: 951, cap: 1000, per: 60, mode: 0, k: 4, feed: 0, dec: 30 },
  tiny: { vdd: 733, cap: 170, per: 60, mode: 0, k: 4, feed: 0, dec: 30 },
  clocked: { vdd: 733, cap: 170, per: 60, mode: 1, k: 4, feed: 0, dec: 30 },
  fast: { vdd: 951, cap: 1000, per: 14, mode: 0, k: 4, feed: 0, dec: 30 },
  low: { vdd: 110, cap: 1000, per: 60, mode: 0, k: 4, feed: 0, dec: 30 },
  droop: { vdd: 951, cap: 1000, per: 60, mode: 0, k: 4, feed: 6, dec: 30 },
  droopc: { vdd: 951, cap: 1000, per: 60, mode: 1, k: 4, feed: 6, dec: 30 },
};

function params() {
  const clocked = $("mode").value === "1", k = +$("k").value;
  return { vdd: vddOf(+$("vdd").value), C: capOf(+$("cap").value), period: +$("per").value / 10,
    clocked, k, feedTau: +$("feed").value / 2, decap: +$("dec").value / 10 + 0.5, N: 32, nBits: 120, seed };
}

function labels(p) {
  $("vddL").textContent = (p.vdd * 1000).toFixed(0) + " mV";
  $("capL").textContent = (p.C * 1e15).toPrecision(2) + " fF";
  $("perL").textContent = (p.period).toFixed(1) + (p.clocked ? " (clock period)" : "");
  $("modeL").textContent = p.clocked ? "clock cycles" : "bits + delays";
  $("kL").textContent = p.k + (p.clocked ? "" : " (unused)");
  $("feedL").textContent = p.feedTau ? p.feedTau.toFixed(1) : "ideal";
  $("decL").textContent = p.decap.toFixed(1) + (p.feedTau ? "" : " (unused)");
}

const hex = h => [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16));
const LO = hex("#3987e5"), MID = hex("#383835"), HI = hex("#e66767"), BAD = hex("#fab219"), NONE = hex("#242423");
function vcol(x) {
  x = Math.max(0, Math.min(1, x));
  const [a, b, t] = x < 0.5 ? [LO, MID, x * 2] : [MID, HI, (x - 0.5) * 2];
  return [0, 1, 2].map(c => a[c] + (b[c] - a[c]) * t);
}

function spaceTime(res, p) {
  const st = $("st"); st.innerHTML = "";
  const N = res.N, H = res.rows, flip = [];
  for (let i = 1, f = 0; i <= N; i++) { if (!(p.clocked && i % p.k === 0)) f ^= 1; flip.push(f); }
  const lv = at => flip[at % N] ? 1 - res.fine[at] : res.fine[at], lb = (b, at) => b ^ flip[at % N];
  const panels = [
    ["Fine: voltage on each wire", "what the physics does", (at) => vcol(lv(at))],
    ["Lens: read as a bit", "above half the supply → 1", (at) => lb(res.lens[at], at) ? HI : LO],
    ["Coarse model, run on its own", p.clocked ? "registers only; one value per cycle" : "input shifted by the delay table", (at) => res.coarse[at] < 0 ? NONE : lb(res.coarse[at], at) ? HI : LO],
    ["Where they disagree", p.clocked ? "checked at each clock edge" : "checked at every sample", (at) => res.bad[at] ? BAD : lb(res.lens[at], at) ? [74, 50, 50] : [40, 52, 72]],
  ];
  if (p.feedTau > 0) panels.push(["Supply at each cell", "blue = sagging below nominal", (at) => vcol(0.5 + (res.supv[at] - 1) * 5)]);
  const cw = Math.max(3, Math.min(8, Math.floor((st.clientWidth - 10 * (panels.length - 1)) / (panels.length * N))));
  for (const [title, sub, col] of panels) {
    const fig = document.createElement("figure"), cap = document.createElement("figcaption");
    cap.innerHTML = title + "<span>" + sub + "</span>";
    const cv = document.createElement("canvas"); cv.width = N; cv.height = H;
    cv.style.width = N * cw + "px"; cv.style.height = H + "px";
    const ctx = cv.getContext("2d"), img = ctx.createImageData(N, H);
    for (let at = 0; at < N * H; at++) { const c = col(at); img.data[at * 4] = c[0]; img.data[at * 4 + 1] = c[1]; img.data[at * 4 + 2] = c[2]; img.data[at * 4 + 3] = 255; }
    ctx.putImageData(img, 0, 0);
    cv.onmousemove = e => {
      const rc = cv.getBoundingClientRect(), i = Math.floor((e.clientX - rc.left) / cw), row = Math.floor(e.clientY - rc.top);
      if (i < 0 || i >= N || row < 0 || row >= H) return;
      const at = row * N + i, c = res.coarse[at];
      tip.textContent = `stage ${i + 1}   t = ${fmtT(row * res.spr * res.dt)}\nvoltage ${(res.fine[at] * p.vdd * 1000).toFixed(1)} mV\nlens ${res.lens[at]}   coarse ${c < 0 ? "—" : c}` +
        (p.feedTau ? `\nsupply ${(res.supv[at] * p.vdd * 1000).toFixed(1)} mV` : "") + (res.bad[at] ? "\n⚠ disagree" : "");
      tip.style.display = "block"; tip.style.left = e.clientX + 14 + "px"; tip.style.top = e.clientY + 10 + "px";
    };
    cv.onmouseleave = () => (tip.style.display = "none");
    fig.append(cap, cv); st.append(fig);
  }
}

function tiles(res, p) {
  const ch = res.ch, kln2 = KT * Math.LN2, cv2 = 0.5 * p.C * p.vdd * p.vdd;
  const items = [
    ["Stage delay d", ch.dies ? "—" : fmtT(res.d), ch.dies ? "the signal dies: no bit level" : "measured from the fine model", ch.dies],
    ["Electrons on a wire", Math.round(res.electrons).toLocaleString(), "C·V / q"],
    ["Thermal noise", (res.sigma * 1000).toPrecision(3) + " mV", "√(kT/C) on one wire"],
    ["Peak gain", ch.gain.toPrecision(3), ch.gain > 1 ? "above 1: levels restore" : "below 1: levels fade", ch.gain <= 1],
    ["Noise margin", (ch.margin * 1000).toPrecision(3) + " mV", (ch.margin / res.sigma).toPrecision(3) + " noise widths", ch.margin < 6 * res.sigma],
    ["Energy per stage per bit", fmtE(res.energyPerStageBit), Math.round(res.energyPerStageBit / kln2).toLocaleString() + " × kT·ln2;  ½CV² = " + fmtE(cv2)],
    ["Coarse model run alone", pct(res.e2e), "samples it gets wrong", res.e2e > 0.01],
    ["One coarse step", pct(res.one), "the commuting square", res.one > 0.01],
  ];
  $("tiles").innerHTML = items.map(([k, v, n, w]) => `<div class="tile${w ? " warn" : ""}"><div class="k">${k}</div><div class="v">${v}</div><div class="n">${n}</div></div>`).join("");
}

/// a small canvas chart: one y-axis, optional log scales, hover tooltip on the nearest point
function plot(cv, o) {
  const dpr = window.devicePixelRatio || 1, W = cv.clientWidth, H = cv.clientHeight;
  cv.width = W * dpr; cv.height = H * dpr;
  const g = cv.getContext("2d"); g.setTransform(dpr, 0, 0, dpr, 0, 0); g.clearRect(0, 0, W, H);
  const m = { l: 54, r: 10, t: 8, b: 30 };
  const tx = o.xlog ? Math.log10 : x => x, ty = o.ylog ? Math.log10 : y => y;
  const [x0, x1] = o.xlim.map(tx), [y0, y1] = o.ylim.map(ty);
  const X = x => m.l + (tx(x) - x0) / (x1 - x0) * (W - m.l - m.r), Y = y => H - m.b - (ty(y) - y0) / (y1 - y0) * (H - m.t - m.b);
  g.font = "10.5px system-ui"; g.fillStyle = "#898781"; g.strokeStyle = "#2c2c2a"; g.lineWidth = 1;
  const ticks = (a, b, log) => { if (log) { const t = []; for (let e = Math.ceil(a); e <= Math.floor(b); e++) t.push(Math.pow(10, e)); return t; }
    const step = Math.pow(10, Math.floor(Math.log10((b - a) / 4))), s2 = (b - a) / step > 8 ? step * 2 : step, t = []; for (let v = Math.ceil(a / s2) * s2; v <= b + 1e-12; v += s2) t.push(+v.toPrecision(6)); return t; };
  for (const t of ticks(y0, y1, o.ylog)) { g.beginPath(); g.moveTo(m.l, Y(t)); g.lineTo(W - m.r, Y(t)); g.stroke(); g.textAlign = "right"; g.fillText(o.yfmt ? o.yfmt(t) : t, m.l - 4, Y(t) + 3); }
  for (const t of ticks(x0, x1, o.xlog)) { g.textAlign = "center"; g.fillText(o.xfmt ? o.xfmt(t) : t, X(t), H - m.b + 13); }
  g.textAlign = "center"; g.fillText(o.xlabel, (m.l + W - m.r) / 2, H - 3);
  g.save(); g.translate(11, (m.t + H - m.b) / 2); g.rotate(-Math.PI / 2); g.fillText(o.ylabel, 0, 0); g.restore();
  g.strokeStyle = "#383835"; g.beginPath(); g.moveTo(m.l, H - m.b); g.lineTo(W - m.r, H - m.b); g.stroke();
  for (const b of o.bands || []) { g.fillStyle = "rgba(250,178,25,.10)"; g.fillRect(X(b[0]), m.t, X(b[1]) - X(b[0]), H - m.t - m.b); }
  for (const vl of o.vlines || []) { g.strokeStyle = "#898781"; g.setLineDash([3, 3]); g.beginPath(); g.moveTo(X(vl), m.t); g.lineTo(X(vl), H - m.b); g.stroke(); g.setLineDash([]); }
  const pts = [];
  for (const s of o.series) {
    const ok = s.pts.filter(([x, y]) => isFinite(ty(y)) && isFinite(tx(x)));
    g.strokeStyle = g.fillStyle = s.color; g.lineWidth = 2;
    if (s.kind === "bar") {
      const bw = Math.max(1, (W - m.l - m.r) / s.pts.length - 1);
      for (const [x, y] of ok) { const yy = Math.max(Y(y), m.t); g.fillRect(X(x) - bw / 2, yy, bw, H - m.b - yy); }
    } else {
      g.beginPath(); ok.forEach(([x, y], i) => { const yy = Math.min(Math.max(Y(y), m.t), H - m.b); i ? g.lineTo(X(x), yy) : g.moveTo(X(x), yy); }); g.stroke();
      if (s.dots) for (const [x, y] of ok) { g.beginPath(); g.arc(X(x), Math.min(Math.max(Y(y), m.t), H - m.b), 3, 0, 7); g.fill(); }
    }
    for (const [x, y] of ok) pts.push([X(x), Math.min(Math.max(Y(y), m.t), H - m.b), s.label, x, y]);
  }
  for (const hl of o.highlight || []) { g.fillStyle = "#f0efec"; g.beginPath(); g.arc(X(hl[0]), Y(hl[1]), 5, 0, 7); g.fill(); g.strokeStyle = "#1a1a19"; g.lineWidth = 2; g.stroke(); }
  cv.onmousemove = e => {
    const rc = cv.getBoundingClientRect(), mx = e.clientX - rc.left, my = e.clientY - rc.top;
    let best = null, bd = 1e9; for (const p of pts) { const dd = Math.abs(p[0] - mx) + Math.abs(p[1] - my) * 0.3; if (dd < bd) { bd = dd; best = p; } }
    if (!best || bd > 40) { tip.style.display = "none"; return; }
    tip.textContent = (best[2] ? best[2] + "\n" : "") + o.tipfmt(best[3], best[4]);
    tip.style.display = "block"; tip.style.left = e.clientX + 14 + "px"; tip.style.top = e.clientY + 10 + "px";
  };
  cv.onmouseleave = () => (tip.style.display = "none");
}

function charts(res, p) {
  const floor = 1e-4;
  plot($("cStage"), { xlim: [0, res.N + 1], ylim: [floor, 1], ylog: true, xlabel: "stage", ylabel: "wrong (fraction)",
    yfmt: t => t >= 0.01 ? t * 100 + "%" : (t * 100).toPrecision(1) + "%",
    series: [{ pts: res.stage.map(([i, f]) => [i, Math.max(f, floor)]), color: "#3987e5", dots: true, label: "coarse model run alone" }],
    tipfmt: (x, y) => `stage ${x}: ${y <= floor ? "≤ 0.01%" : pct(y)} wrong` });
  $("lStage").innerHTML = res.stage.length ? `<span>${p.clocked ? "registers only (the coarse model has nothing else)" : "every stage"}; 0 is drawn at 0.01%</span>` : "";
  const HB = res.hist.length;
  const ch = res.ch, band = ch.vil !== null ? [[ch.vil / p.vdd, ch.vih / p.vdd]] : [[0, 1]];
  plot($("cHist"), { xlim: [-0.1, 1.1], ylim: [1, Math.max(10, ...res.hist) * 2], ylog: true, xlabel: "voltage ÷ supply", ylabel: "samples", bands: band, vlines: [0.5],
    yfmt: t => t >= 1e3 ? t / 1e3 + "k" : t,
    series: [{ kind: "bar", pts: Array.from(res.hist, (c, b) => [-0.1 + (b + 0.5) * 1.2 / HB, c]), color: "#3987e5", label: "" }],
    tipfmt: (x, y) => `${(x * p.vdd * 1000).toFixed(0)} mV: ${y.toLocaleString()} samples` });
  plot($("cVtc"), { xlim: [0, p.vdd], ylim: [0, p.vdd], xlabel: "input (V)", ylabel: "output (V)", bands: band.map(b => b.map(x => x * p.vdd)),
    series: [{ pts: ch.vin.map((x, i) => [x, ch.vout[i]]), color: "#3987e5", label: "out = f(in)" },
      { pts: ch.vin.map((x, i) => [ch.vout[i], x]).sort((a, b) => a[0] - b[0]), color: "#d95926", label: "mirror" }],
    tipfmt: (x, y) => `in ${(x * 1000).toFixed(0)} mV → out ${(y * 1000).toFixed(0)} mV` });
}

function sweep(p) {
  const key = JSON.stringify({ ...p, vdd: 0 });
  if (key === sweepKey) return Promise.resolve(sweepCache);
  $("busy").textContent = "sweeping supply…";
  const grid = []; for (let i = 0; i < 20; i++) grid.push(0.03 * Math.pow(1.2 / 0.03, i / 19));
  const out = [];
  return new Promise(done => {
    const step = j => {
      if (j === grid.length) { sweepKey = key; sweepCache = out; $("busy").textContent = ""; return done(out); }
      const r = simulate({ ...p, vdd: grid[j], N: 16, nBits: 40, record: false });
      out.push({ vdd: grid[j], e: r.energyPerStageBit, e2e: r.e2e, one: r.one, dies: r.ch.dies });
      setTimeout(() => step(j + 1), 0);
    };
    step(0);
  });
}

function sweepCharts(sw, res, p) {
  const xs = { xlim: [0.025, 1.3], xlog: true, xlabel: "supply (V)", xfmt: t => t + " V" };
  const es = sw.map(s => [s.vdd, s.e]), emin = Math.min(...es.map(e => e[1])), emax = Math.max(...es.map(e => e[1]));
  plot($("cEnergy"), { ...xs, ylim: [Math.pow(10, Math.floor(Math.log10(emin))), Math.pow(10, Math.ceil(Math.log10(emax)))], ylog: true, ylabel: "energy (J)",
    yfmt: t => fmtE(t), series: [{ pts: es, color: "#3987e5", dots: true, label: "energy per stage per bit" }],
    vlines: [p.vdd],
    tipfmt: (x, y) => `${(x * 1000).toFixed(0)} mV: ${fmtE(y)}  (${Math.round(y / (KT * Math.LN2)).toLocaleString()} kT·ln2)` });
  const floor = 1e-4;
  plot($("cErr"), { ...xs, ylim: [floor, 1], ylog: true, ylabel: "wrong (fraction)", yfmt: t => t >= 0.01 ? t * 100 + "%" : (t * 100).toPrecision(1) + "%",
    series: [{ pts: sw.map(s => [s.vdd, Math.max(s.e2e, floor)]), color: "#3987e5", dots: true, label: "coarse model run alone" },
      { pts: sw.map(s => [s.vdd, Math.max(s.one, floor)]), color: "#d95926", dots: true, label: "one coarse step" }],
    vlines: [p.vdd],
    tipfmt: (x, y) => `${(x * 1000).toFixed(0)} mV: ${y <= floor ? "≤ 0.01%" : pct(y)} wrong` });
}

let pending = null;
function run() {
  if (pending) clearTimeout(pending);
  pending = setTimeout(async () => {
    const p = params(); labels(p);
    $("busy").textContent = "simulating…";
    await new Promise(r => setTimeout(r, 0));
    const res = simulate(p);
    $("busy").textContent = "";
    tiles(res, p); spaceTime(res, p); charts(res, p);
    const sw = await sweep(p); sweepCharts(sw, res, p);
  }, 60);
}
for (const id of ["vdd", "cap", "per", "mode", "k", "feed", "dec"]) $(id).addEventListener("input", run);
$("reseed").onclick = () => { seed = (seed * 48271) % 2147483647; sweepKey = ""; run(); };
document.querySelectorAll("[data-p]").forEach(b => b.onclick = () => {
  const s = presets[b.dataset.p]; for (const k in s) $(k).value = s[k]; run();
});
window.addEventListener("resize", run);
if (presets[location.hash.slice(1)]) { const s = presets[location.hash.slice(1)]; for (const k in s) $(k).value = s[k]; }
run();
