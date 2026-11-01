// A small page for the thread: the 32-bit add's frontier and energy per multiply-accumulate against
// precision, with explorer.src.html's chart code. `node build-summary.mjs` after sweep.mjs.
import { readFileSync, writeFileSync } from 'node:fs'
import * as m from './model.mjs'
const here = (f) => new URL(f, import.meta.url)
const src = readFileSync(here('./explorer.src.html'), 'utf-8')
const style = src.slice(src.indexOf('<style>'), src.indexOf('</style>') + 8)
const code = src.slice(src.indexOf('const NAMES'), src.indexOf('// ---- the frontier ----'))
const r = JSON.parse(readFileSync(here('./results.json'), 'utf-8'))
const r3 = (x) => +x.toPrecision(3)
// each frontier thinned to at most 9 points spread evenly in log time, its ends kept
const thin = (f) => {
  if (f.length <= 9) return f
  const lt = f.map((p) => Math.log(p.time)), lo = lt[0], hi = lt[lt.length - 1]
  const keep = new Set([0, f.length - 1])
  for (let i = 1; i < 8; i++) { const t = lo + ((hi - lo) * i) / 8; keep.add(lt.reduce((b, v, j) => (Math.abs(v - t) < Math.abs(lt[b] - t) ? j : b), 0)) }
  return [...keep].sort((a, b) => a - b).map((i) => f[i])
}
const front = Object.fromEntries(Object.entries(r.workloads.addw.designs).map(([d, f]) => [d, thin(f).map((p) => ({ time: r3(p.time), energy: r3(p.energy), vdd: r3(p.vdd) }))]))
// the analog multiply-accumulate near its noise floor with values kept analog between stages (no ADC)
const w = { ...m.WORKLOADS.dot, macs: 64 }
const near = {}
for (let b = 2; b <= 14; b++) near[b] = r3(Math.min(...[0.3, 0.4, 0.5, 0.6, 0.8, 1].map((vdd) => m.analogMac(w, { ...m.P, vdd, bits: b, calibrated: true, adcFom: 0, analogOverhead: 3 }).energy / 64)))
const macs = Object.fromEntries(Object.entries(r.macs).map(([b, row]) => [b, { digital: row.digital && r3(row.digital.energy), analog: row.analogTrimmed && r3(row.analogTrimmed.energy), floor: near[b], stochastic: row.stochastic && r3(row.stochastic.energy) }]))
const html = `<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>Signal fabrics</title>${style}</head><body><div class="viz-root">
<div class="card"><h3>A 32-bit add: energy against time, each design at its best settings</h3><p class="sub">Every point is wrong at most once in 10<sup>12</sup> runs and buildable (a fixed chip must yield; the arrays route around faulty cells). No point of a line is beaten in both. Log scales; lower left is better.</p><div id="front"></div><div class="legend" id="frontLegend"></div><details><summary>table view</summary><div id="frontTable"></div></details></div>
<div class="card" style="margin-top:16px"><h3>A multiply-accumulate: energy against precision</h3><p class="sub">Analog as modelled converts each result back to digital (an ADC) with 30× its noise floor in overhead; near the floor and kept analog between stages, it wins to about 8 bits. Log scale.</p><div id="mac"></div><div class="legend" id="macLegend"></div><details><summary>table view</summary><div id="macTable"></div></details></div>
</div><div class="tip" id="tip"></div><script>
const FRONT = ${JSON.stringify(front)}, MACS = ${JSON.stringify(macs)};
${code}
Object.assign(NAMES, { asic: 'fixed chip', cells: 'cell array', lowSwing: 'low-swing array', adiabatic: 'adiabatic array', strands: 'strand lattice' });
const series = ORDER.map((d, i) => ({ label: NAMES[d], color: col(i), points: FRONT[d].map(p => ({ x: p.time, y: p.energy, tip: () => [[fmtE(p.energy), 'in ' + fmtT(p.time)], [NAMES[d]], [p.vdd.toFixed(2) + ' V']] })) }));
chart(document.getElementById('front'), series, { xfmt: fmtT, yfmt: fmtE, xlabel: 'time per run', ylabel: 'energy per run' });
legend(document.getElementById('frontLegend'), ORDER.map((d, i) => [NAMES[d], col(i)]));
table(document.getElementById('frontTable'), ['design', 'least energy', 'its time', 'fastest', 'its energy'], ORDER.map(d => { const f = FRONT[d]; const lo = f.reduce((a, b) => b.energy < a.energy ? b : a); return [NAMES[d], fmtE(lo.energy), fmtT(lo.time), fmtT(f[0].time), fmtE(f[0].energy)]; }));
const MK = [['digital', 'digital'], ['analog', 'analog + ADC'], ['floor', 'analog, kept analog'], ['stochastic', 'stochastic']];
chart(document.getElementById('mac'), MK.map(([k, n], i) => ({ label: n, color: col(i), points: Object.entries(MACS).filter(([, v]) => v[k]).map(([b, v]) => ({ x: +b, y: v[k], tip: () => [[fmtE(v[k]), 'at ' + b + ' bits'], [n]] })) })), { xlog: false, xfmt: v => String(v), yfmt: fmtE, xlabel: 'bits of precision', ylabel: 'energy per multiply-accumulate', h: 320 });
legend(document.getElementById('macLegend'), MK.map(([k, n], i) => [n, col(i)]));
table(document.getElementById('macTable'), ['bits', ...MK.map(x => x[1])], Object.entries(MACS).map(([b, v]) => [b, ...MK.map(([k]) => fmtE(v[k]))]));
</script></body></html>`
writeFileSync(here('./summary.html'), html)
console.log(`summary.html: ${html.length} bytes`)
