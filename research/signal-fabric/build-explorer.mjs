// Inlines model.mjs and a thinned results.json into explorer.src.html, giving explorer.html: one file
// that opens from disk (and is small enough to show inline). `node sweep.mjs && node build-explorer.mjs`
import { readFileSync, writeFileSync } from 'node:fs'
const here = (f) => new URL(f, import.meta.url)
// the model without its comments, which stay in model.mjs
const model = readFileSync(here('./model.mjs'), 'utf-8').replace(/^export /gm, '').replace(/^\s*\/\/.*$/gm, '').replace(/\/\*\*[\s\S]*?\*\//g, '').replace(/\n{2,}/g, '\n')
const r = JSON.parse(readFileSync(here('./results.json'), 'utf-8'))
const round = (x) => (typeof x === 'number' ? +x.toPrecision(3) : x)
const slim = (o) => Object.fromEntries(Object.entries(o).filter(([, v]) => v !== undefined && v !== null && v !== false).map(([k, v]) => [k, typeof v === 'object' ? slim(v) : round(v)]))
// each frontier thinned to at most 10 points, its ends kept, evenly spread in log time
const thin = (f) => {
  if (f.length <= 10) return f
  const lt = f.map((p) => Math.log(p.time)), lo = lt[0], hi = lt[lt.length - 1]
  const keep = new Set([0, f.length - 1])
  for (let i = 1; i < 9; i++) { const t = lo + ((hi - lo) * i) / 9; keep.add(lt.reduce((b, v, j) => (Math.abs(v - t) < Math.abs(lt[b] - t) ? j : b), 0)) }
  return [...keep].sort((a, b) => a - b).map((i) => f[i])
}
for (const w of Object.values(r.workloads)) for (const [d, f] of Object.entries(w.designs)) w.designs[d] = thin(f).map(({ err, area, setup, ...p }) => slim(p))
for (const row of Object.values(r.macs)) for (const k of Object.keys(row)) if (row[k]) row[k] = slim({ energy: row[k].energy })
r.amortize = { addw: r.amortize.addw.map(slim) }
r.repair = r.repair.map(slim)
delete r.tolerance
const html = readFileSync(here('./explorer.src.html'), 'utf-8').replace('/*MODEL*/', () => model).replace('/*RESULTS*/null', () => JSON.stringify(r))
writeFileSync(here('./explorer.html'), html)
console.log(`explorer.html: ${html.length} bytes`)
