// Checks the dial's promises on every example, over every combination of settings:
//  - turning any one dial down a notch never adds beats, and only merges them: every boundary between beats after
//    the turn was a boundary before it;
//  - beats cover the run exactly, in order, with no gaps or overlaps;
//  - every beat is tagged with the setting its call has in the side list.
// usage: node archive/playground-dial/invariants.mjs
import { readFileSync } from 'node:fs'
import vm from 'node:vm'
const ctx = vm.createContext({ console })
vm.runInContext(readFileSync(new URL('engine.js', import.meta.url), 'utf8') + ';globalThis.E = { EXAMPLES, run, beatsOf, LEVELS, show }', ctx)
const { E } = ctx
let checked = 0
let failed = 0
for (const [name, ex] of Object.entries(E.EXAMPLES)) {
  const r = E.run(ex.build())
  const defs = ex.defs.map((d) => d.name)
  const choices = ex.defs.map((d) => (d.kind === 'constant' ? [0, 3] : d.name === 'main' ? [2, 3] : [0, 1, 2, 3]))
  const cut = (levels) => {
    const yours = (n) => ex.defs.find((d) => d.name === n)?.kind === 'yours'
    const beats = E.beatsOf(r.steps, (o) => levels[defs.indexOf(o)] ?? 2, r.frames, r.calls, yours)
    let at = 0
    for (const b of beats) {
      if (b.from !== at || b.to <= b.from) throw new Error(`${name}: beats don't tile the run at ${at}: ${JSON.stringify(beats.map((x) => [x.from, x.to]))}`)
      at = b.to
    }
    if (at !== r.steps.length) throw new Error(`${name}: beats end at ${at}, the run at ${r.steps.length}`)
    for (const b of beats) {
      const side = b.owner === 'main' ? Math.max(2, levels[defs.indexOf('main')] ?? 2) : levels[defs.indexOf(b.owner)]
      if (b.level !== side) throw new Error(`${name}: a beat of ${b.owner} is tagged ${E.LEVELS[b.level]}, the side list says ${E.LEVELS[side]}`)
    }
    return beats.map((b) => b.from)
  }
  const count = (levels) => cut(levels).length
  const all = choices.reduce((acc, cs) => acc.flatMap((a) => cs.map((c) => [...a, c])), [[]])
  for (const levels of all) {
    const fine = cut(levels)
    levels.forEach((l, i) => {
      const lower = choices[i].filter((c) => c < l).pop()
      if (lower === undefined) return
      const coarse = cut(levels.map((x, j) => (j === i ? lower : x)))
      checked++
      const extra = coarse.filter((b) => !fine.includes(b))
      if (extra.length) {
        failed++
        if (failed <= 5) console.log(`FAIL ${name}: ${defs[i]} ${E.LEVELS[l]}→${E.LEVELS[lower]} adds boundaries ${extra} (${fine.length}→${coarse.length} beats) with ${defs.map((d, j) => `${d}=${E.LEVELS[levels[j]]}`).join(' ')}`)
      }
    })
  }
  console.log(`${name}: ${all.length} settings, beats from ${Math.min(...all.map(count))} to ${Math.max(...all.map(count))} (the run is ${r.steps.length} rule steps)`)
}
console.log(failed ? `${failed} of ${checked} one-notch-down turns added a boundary` : `ok: all ${checked} one-notch-down turns only merged beats`)
process.exit(failed ? 1 : 0)
