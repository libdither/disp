// Sweeps every design's knobs over each workload (model.mjs), keeps the settings that meet the error
// target and can be built (a fixed chip must yield; a self-configuring array routes around faulty cells
// at the cost sim.mjs measured), adds triple voting or a run-back check where that is cheaper, and
// keeps each design's energy–time frontier. Writes results.json for explorer.html and prints a summary.
// `node sweep.mjs`
import { writeFileSync } from 'node:fs'
import * as m from './model.mjs'
import { repair } from './sim.mjs'

let target = 1e-12 // chance a run's output is wrong
const chipSites = 1e7 // a fixed chip works only if none of its gate sites is faulty: it must yield 90%
const range = (a, b, n, log = false) =>
  Array.from({ length: n }, (_, i) => (log ? a * (b / a) ** (i / (n - 1)) : a + ((b - a) * i) / (n - 1)))

// self-repair overhead, measured: faulty share of cells → [hops per net, cells per gate]
const REPAIR = [0, 0.001, 0.01, 0.03, 0.06, 0.1, 0.15].map((pf) => {
  let h = 0, c = 0, ok = 0
  for (let t = 0; t < 12; t++) {
    const r = repair(pf)
    if (r.ok) { ok++; h += r.hopsPerNet; c += r.cellsPerGate }
  }
  return { pf, ok: ok / 12, hops: h / Math.max(1, ok), cells: c / Math.max(1, ok) }
})
function repairCost(pf) {
  if (pf > 0.15) return null
  const i = REPAIR.findIndex((r) => r.pf >= pf)
  const [a, b] = i <= 0 ? [REPAIR[0], REPAIR[0]] : [REPAIR[i - 1], REPAIR[i]]
  const t = b.pf === a.pf ? 0 : (pf - a.pf) / (b.pf - a.pf)
  return { hops: a.hops + t * (b.hops - a.hops), cells: a.cells + t * (b.cells - a.cells) }
}

/** The best of `evaluateAt` over how often a run may miss its clock. */
function evaluate(design, w, p, extra = []) {
  let best = null
  for (const timingErr of [1e-15, 1e-12, 1e-9, 1e-6, 1e-3]) {
    const q = evaluateAt(design, w, { ...p, timingErr }, extra)
    if (q && (!best || q.energy * q.time < best.energy * best.time)) best = { ...q, timingErr }
  }
  return best
}
/** One design at one setting, made buildable and correct enough, or null. */
function evaluateAt(design, w, p, extra = []) {
  let r = m.DESIGNS[design](w, p, ...extra)
  const fabric = ['cells', 'lowSwing', 'adiabatic', 'strands'].includes(design)
  if (fabric) {
    // a cell is several gates: it is faulty if any is; the array tests every cell and routes around the bad
    const pfCell = 1 - (1 - r.faultPerSite) ** p.cellGates
    const rc = repairCost(pfCell)
    if (!rc) return null
    const testE = r.area * 64 * p.cellGates * p.activity * p.c * p.vdd ** 2
    r = { ...r, energy: r.energy * (1 + 0.5 * (rc.hops - 1)), time: r.time * (1 + 0.5 * (rc.hops - 1)), area: r.area * rc.cells,
      setup: { energy: r.setup.energy + testE, time: r.setup.time } }
  } else if ((1 - r.faultPerSite) ** chipSites < 0.9) return null
  const options = [{ ...r, fix: 'none' }, { ...m.tmr(r, w.outBits), fix: 'vote' }, { ...m.replay(r, w.depth), fix: 'replay late' }]
  if (w.reversible) options.push({ ...m.checked(r), fix: 'run back' })
  const ok = options.filter((o) => o.err <= target)
  if (!ok.length) return null
  return ok.reduce((a, b) => (b.energy < a.energy ? b : a))
}

/** Points no other point beats in both energy and time. */
function frontier(points) {
  const s = points.filter(Boolean).sort((a, b) => a.time - b.time || a.energy - b.energy)
  const out = []
  let best = Infinity
  for (const q of s) {
    if (q.energy >= best * 0.999) continue
    // of points at about the same time, keep the cheapest
    if (out.length && q.time < out[out.length - 1].time * 1.01) out.pop()
    out.push(q)
    best = q.energy
  }
  return out
}

const V = range(0.16, 1.0, 43)
const C = [0.2e-15, 0.5e-15, 1e-15, 2e-15, 4e-15]
const results = { target, repair: REPAIR, workloads: {}, macs: {}, amortize: {} }
for (const [key, w] of Object.entries(m.WORKLOADS)) {
  const designs = {}
  for (const design of Object.keys(m.DESIGNS)) {
    const pts = []
    for (const vdd of V)
      for (const c of C) {
        const base = { ...m.P, vdd, c }
        if (design === 'lowSwing') {
          for (const swing of [0.05, 0.08, 0.12, 0.18, 0.25])
            for (const k of [2, 4, 8, 16, 32])
              for (const calibrated of [false, true]) {
                if (swing > vdd) continue
                const q = evaluate(design, w, { ...base, swing, k, calibrated })
                if (q) pts.push({ ...q, vdd, c, swing, k, calibrated })
              }
        } else if (design === 'adiabatic') {
          for (const ramp of range(20e-12, 200e-9, 25, true)) {
            const q = evaluate(design, w, { ...base, ramp })
            if (q) pts.push({ ...q, vdd, c, ramp })
          }
        } else if (design === 'strands') {
          for (const cpr of [3, 10, 28]) {
            const q = evaluate(design, w, base, [cpr])
            if (q) pts.push({ ...q, vdd, c, clocksPerRewrite: cpr })
          }
        } else {
          const q = evaluate(design, w, base)
          if (q) pts.push({ ...q, vdd, c })
        }
      }
    designs[design] = frontier(pts).map(({ energy, time, err, area, setup, fix, vdd, c, swing, k, calibrated, ramp, clocksPerRewrite }) =>
      ({ energy, time, err, area, setup, fix, vdd, c, swing, k, calibrated, ramp, clocksPerRewrite }))
  }
  results.workloads[key] = { name: w.name, designs }
}

// numbers carried as values: energy per multiply-accumulate against precision
for (let bits = 2; bits <= 14; bits++) {
  const w = { ...m.WORKLOADS.dot, macs: 64 }
  const row = {}
  const best = (f) => {
    let b = null
    for (const vdd of V) for (const c of C) {
      const r = f({ ...m.P, vdd, c, bits })
      if (r && (!b || r.energy < b.energy)) b = { ...r, vdd, c }
    }
    return b
  }
  // the digital MAC: an 8-bit multiplier grows as bits², an accumulator as bits
  const scaled = { ...w, gates: 64 * (bits * bits * 6 + bits * 2 * 5), depth: 4 * bits + 8, hops: 64 * bits * bits * 10 }
  row.digital = best((p) => evaluate('cells', scaled, p))
  row.analogTrimmed = best((p) => m.analogMac(w, { ...p, calibrated: true }))
  row.analogUntrimmed = best((p) => m.analogMac(w, { ...p, calibrated: false }))
  row.stochastic = bits <= 12 ? best((p) => m.stochastic(w, p)) : null
  results.macs[bits] = Object.fromEntries(Object.entries(row).map(([k, r]) => [k, r && { energy: r.energy / 64, time: r.time, setup: r.setup.energy / 64 }]))
}

// set-up shared by runs: the array writing its own configuration, an FPGA loading a bitstream
for (const key of ['addw', 'mul']) {
  const w = m.WORKLOADS[key]
  const pick = (d, p) => evaluate(d, w, p)
  const c = pick('cells', { ...m.P, vdd: 0.4 })
  const f = pick('fpga', { ...m.P, vdd: 0.8 })
  const a = pick('asic', { ...m.P, vdd: 0.6 })
  results.amortize[key] = [1, 3, 10, 30, 100, 1e3, 1e4, 1e5, 1e6].map((uses) => ({
    uses, cells: m.amortized(c, uses), fpga: m.amortized(f, uses), asic: a.energy
  }))
}

// what tolerating errors buys: least energy against the error allowed (a 32-bit add, digital; a
// multiply-accumulate carried as an analog value, whose error is its precision)
results.tolerance = []
for (const allowed of [1e-1, 1e-2, 1e-3, 1e-4, 1e-6, 1e-9, 1e-12, 1e-15]) {
  target = allowed
  const row = { allowed }
  for (const d of ['asic', 'cells', 'adiabatic']) {
    let b = null
    for (const vdd of V) for (const c of [0.05e-15, 0.1e-15, ...C]) {
      const q = d === 'adiabatic'
        ? range(20e-12, 200e-9, 13, true).map((ramp) => evaluate(d, m.WORKLOADS.addw, { ...m.P, vdd, c, ramp })).filter(Boolean).sort((x, y) => x.energy * x.time - y.energy * y.time)[0]
        : evaluate(d, m.WORKLOADS.addw, { ...m.P, vdd, c })
      if (q && (!b || q.energy * q.time < b.energy * b.time)) b = { energy: q.energy, time: q.time, vdd, c, fix: q.fix }
    }
    row[d] = b
  }
  const bits = Math.max(1, Math.ceil(-Math.log2(allowed)))
  row.analogBits = bits
  row.analogMac = bits <= 20 ? m.analogMac(m.WORKLOADS.dot, { ...m.P, bits, calibrated: true, vdd: 0.6 }).energy / 64 : null
  results.tolerance.push(row)
}
target = 1e-12

writeFileSync(new URL('./results.json', import.meta.url), JSON.stringify(results))

// summary: each design's lowest energy, and its fastest point, per workload
const fmtE = (e) => (e < 1e-12 ? `${(e * 1e15).toFixed(1)} fJ` : e < 1e-9 ? `${(e * 1e12).toFixed(2)} pJ` : `${(e * 1e9).toFixed(2)} nJ`)
const fmtT = (t) => (t < 1e-9 ? `${(t * 1e12).toFixed(0)} ps` : t < 1e-6 ? `${(t * 1e9).toFixed(2)} ns` : `${(t * 1e6).toFixed(2)} µs`)
for (const [key, r] of Object.entries(results.workloads)) {
  console.log(`\n${r.name} (output wrong at most ${target}):`)
  for (const [d, f] of Object.entries(r.designs)) {
    if (!f.length) { console.log(`  ${d.padEnd(10)} cannot meet it`); continue }
    const low = f.reduce((a, b) => (b.energy < a.energy ? b : a))
    const fast = f[0]
    console.log(`  ${d.padEnd(10)} least energy ${fmtE(low.energy)} in ${fmtT(low.time)} at ${low.vdd.toFixed(2)} V, ${(low.c * 1e15).toFixed(1)} fF, ${low.fix}` +
      `${low.swing ? `, swing ${low.swing * 1e3} mV every ${low.k} hops${low.calibrated ? ' trimmed' : ''}` : ''}${low.ramp ? `, ramp ${fmtT(low.ramp)}` : ''}` +
      ` | fastest ${fmtT(fast.time)} for ${fmtE(fast.energy)}`)
  }
}
console.log('\nmultiply-accumulate, energy each by precision (best supply and size):')
for (const [b, row] of Object.entries(results.macs))
  console.log(`  ${String(b).padStart(2)} bits: ` + Object.entries(row).map(([k, r]) => `${k} ${r ? fmtE(r.energy) : '-'}`).join(', '))
console.log('\nwhat tolerating errors buys (32-bit add, least energy × time; analog MAC energy at the precision that error allows):')
const pt = (q) => (q ? `${fmtE(q.energy)} in ${fmtT(q.time)} (${q.vdd.toFixed(2)} V, ${q.fix})` : '-')
for (const r of results.tolerance)
  console.log(`  wrong at most ${r.allowed.toExponential(0)}: fixed ${pt(r.asic)}; array ${pt(r.cells)}; adiabatic ${pt(r.adiabatic)}; analog MAC (${r.analogBits} bits) ${r.analogMac ? fmtE(r.analogMac) : '-'}`)
console.log('\nset-up shared by runs (32-bit add), energy per run:')
for (const r of results.amortize.addw) console.log(`  ${String(r.uses).padStart(7)} runs: array ${fmtE(r.cells)}, FPGA ${fmtE(r.fpga)}, fixed chip ${fmtE(r.asic)}`)
