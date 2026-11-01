// Monte Carlo checks under model.mjs's formulas, and the one thing a formula cannot give: what a
// self-configuring array pays to route around cells that came out faulty. `node sim.mjs`.
import * as m from './model.mjs'

let seed = 12345
const rand = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296)
const gauss = () => Math.sqrt(-2 * Math.log(rand() + 1e-300)) * Math.cos(2 * Math.PI * rand())

/** Low-swing transport: k hops of thermal noise, then a sense amplifier with its own static offset.
 * Thermal noise is drawn every time; the offset once per amplifier. Returns the average error rate, and
 * the share of amplifiers whose own error rate exceeds `bad` (the ones a self-test would trim or avoid). */
export function lowSwingMC(p, amps = 2000, trials = 2000, bad = 1e-6) {
  const sTh = Math.sqrt(p.k) * m.thermal(p.hopC, p)
  const off = p.calibrated ? p.senseResidual : p.senseOffset
  let errs = 0
  let worse = 0
  for (let a = 0; a < amps; a++) {
    const o = off * gauss()
    // this amplifier's own error rate, exactly, and its sampled errors
    const own = 0.5 * (m.Q((p.swing / 2 - o) / sTh) + m.Q((p.swing / 2 + o) / sTh))
    if (own > bad) worse++
    for (let t = 0; t < trials / amps + 1; t++) {
      const bit = rand() < 0.5 ? 1 : -1
      const got = bit * (p.swing / 2) + sTh * gauss() + o
      if (Math.sign(got) !== bit) errs++
    }
  }
  return { mc: errs / (amps * (Math.floor(trials / amps) + 1)), formula: m.Q(p.swing / 2 / Math.sqrt(sTh ** 2 + off ** 2)), badShare: worse / amps }
}

/** Self-repair: a w×w array whose cells are faulty with chance pf; g gates are placed on good cells in
 * snake order and each joined to the next by the shortest path through good cells. Returns the mean hops
 * per net (1 with no faults), the cells used per gate, and whether every net could be routed. */
export function repair(pf, w = 48, g = 400) {
  const good = Array.from({ length: w * w }, () => rand() >= pf)
  const order = []
  for (let y = 0; y < w; y++) for (let i = 0; i < w; i++) order.push(y * w + (y % 2 ? w - 1 - i : i))
  const placed = order.filter((c) => good[c]).slice(0, g)
  if (placed.length < g) return { ok: false }
  const bfs = (a, b) => {
    const dist = new Int32Array(w * w).fill(-1)
    dist[a] = 0
    const q = [a]
    for (let h = 0; h < q.length; h++) {
      const c = q[h]
      if (c === b) return dist[c]
      const x = c % w
      for (const d of [x > 0 ? c - 1 : -1, x < w - 1 ? c + 1 : -1, c - w, c + w]) {
        if (d < 0 || d >= w * w || dist[d] >= 0 || !good[d]) continue
        dist[d] = dist[c] + 1
        q.push(d)
      }
    }
    return -1
  }
  let hops = 0
  for (let i = 1; i < placed.length; i++) {
    const h = bfs(placed[i - 1], placed[i])
    if (h < 0) return { ok: false }
    hops += h
  }
  const span = placed[placed.length - 1] - placed[0] + 1
  return { ok: true, hopsPerNet: hops / (g - 1), cellsPerGate: span / g }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('low-swing transport, Monte Carlo against the formula (k hops of thermal noise, an offset per amplifier)')
  for (const [swing, k, calibrated] of [
    [0.03, 8, false], [0.05, 8, false], [0.05, 8, true], [0.1, 8, false], [0.1, 32, true], [0.02, 16, true]
  ]) {
    const p = { ...m.P, swing, k, calibrated }
    const r = lowSwingMC(p, 4000, 400000)
    console.log(`  swing ${swing * 1e3} mV, k ${k}, ${calibrated ? 'trimmed' : 'untrimmed'}: MC ${r.mc.toExponential(2)}, formula ${r.formula.toExponential(2)}, amplifiers worse than 1e-6 alone ${(r.badShare * 100).toFixed(1)}%`)
  }
  console.log('self-repair: route a 400-gate pipeline through an array with faulty cells (20 arrays each)')
  for (const pf of [0, 0.001, 0.01, 0.03, 0.1, 0.2, 0.3, 0.35]) {
    let ok = 0
    let hops = 0
    let cpg = 0
    for (let t = 0; t < 20; t++) {
      const r = repair(pf)
      if (r.ok) {
        ok++
        hops += r.hopsPerNet
        cpg += r.cellsPerGate
      }
    }
    const asicYield = (1 - pf) ** 400
    console.log(`  faulty ${pf}: routed ${ok}/20, hops per net ${ok ? (hops / ok).toFixed(2) : '-'}, cells per gate ${ok ? (cpg / ok).toFixed(2) : '-'}; a fixed 400-gate circuit works with chance ${asicYield.toExponential(2)}`)
  }
}
