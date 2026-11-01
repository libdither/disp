// Checks the fabric against the two-way interpreter: for each example, on a chip with no faulty cells and
// on chips with some, routed around them by the self-test, every input gives the interpreter's answer;
// without the self-test some chips go wrong; a noise flip is caught by flipping back, and a clean run
// flips back with nothing caught. `npx --prefix website tsx research/signal-fabric/fabric/check.mts`
import { eq } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { circuitOf, answer, outputValue, reference } from './circuit.ts'
import { layout, cellAcross } from './layout.ts'
import { Chip, simulate, retract, setup, type Knobs } from './sim.ts'
import { EXAMPLES } from './examples.ts'

let seed = 7
const rand = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296)
const never = () => 1
let failures = 0
const fail = (msg: string) => { failures++; console.log('  FAIL ' + msg) }

for (const ex of EXAMPLES) {
  const t0 = performance.now()
  const c = circuitOf(ex.prog, ex.input)
  const n = c.inputs.length
  const inputs = Array.from({ length: Math.min(1 << n, 512) }, (_, k) => Array.from({ length: n }, (_, i) => (n > 9 ? (rand() < 0.5 ? 1 : 0) : (k >> i) & 1)))
  const base: Knobs = { vdd: 0.8, c: 1e-15, adiabatic: false, ramp: 2e-10, test: true, seed: 1 }
  const notes: string[] = []
  // a chip and supply with no faulty cells, then lower supplies where some are, with and without the self-test
  for (const [vdd, test] of [[0.8, true], [0.33, true], [0.3, true], [0.3, false]] as [number, boolean][]) {
    let wrongChips = 0, chips = 0, avoided = 0, cells = 0
    for (let s = 1; s <= (vdd === 0.8 ? 1 : 6); s++) {
      const chip = new Chip({ ...base, vdd, test, seed: s })
      const lay = layout(c, test ? (x, y) => chip.faulty(x, y) : () => false)
      cells += lay.w * lay.h
      for (let y = 0; y < lay.h; y++) for (let x = 1; x < lay.w - 1; x++) if (chip.faulty(x, y)) avoided++
      // the routes are trees from their drivers reaching every reader, no link shared
      const seen = new Set<number>()
      for (const r of lay.routes) {
        const reached = new Set([r.src])
        for (const l of r.links) {
          if (!reached.has(l >> 2)) fail(`${ex.name}: a link leaves a cell its net has not reached`)
          if (seen.has(l)) fail(`${ex.name}: a link carries two nets`)
          seen.add(l)
          reached.add(cellAcross(l, lay.w))
        }
        for (const [sink, l] of r.into) if (cellAcross(l, lay.w) !== sink) fail(`${ex.name}: a net does not reach its reader`)
      }
      let wrong = 0
      for (const bits of inputs) {
        const run = simulate(c, lay, chip, bits, null, null, never)
        const want = answer(c, bits)
        const ok = !!want && eq(want, outputValue(c, run.outBits))
        if (!eq(outputValue(c, run.outRef), outputValue(c, reference(c, bits)))) fail(`${ex.name}: the fault-free values are not the diagrams'`)
        if (!ok) wrong++
        if (test && !ok) { fail(`${ex.name} at ${vdd} V, chip ${s}: wrong answer with the self-test on`); break }
        if (vdd === 0.8 && run.flipped.length) fail(`${ex.name}: noise where there should be none`)
        if (retract(c, lay, chip, run, bits).caught.length) fail(`${ex.name}: a clean run flips back with a mismatch`)
      }
      if (wrong) wrongChips++
      chips++
      if (s === 1 && vdd === 0.8) {
        // one noise flip in a gate or wire cell is caught on the way back
        let caught = 0, tries = 0
        const used = [...new Set(lay.routes.flatMap((r) => r.links.map((l) => l >> 2)))].filter((cell) => lay.sites.get(cell)?.k !== 'in')
        for (let t = 0; t < 40; t++) {
          const bits = inputs[t % inputs.length]
          const zap = used[Math.floor(rand() * used.length)]
          const run = simulate(c, lay, chip, bits, null, zap, never)
          tries++
          if (retract(c, lay, chip, run, bits).caught.length) caught++
        }
        if (used.length && caught !== tries) fail(`${ex.name}: ${tries - caught} of ${tries} noise flips not caught`)
        const su = setup(lay, chip)
        notes.push(`${lay.w}×${lay.h} cells, ${c.gates.length} gates, ${lay.wireCells} wire cells; set-up ${(su.time * 1e9).toFixed(1)} ns, ${(su.energy * 1e12).toFixed(2)} pJ`)
      }
    }
    if (vdd !== 0.8) notes.push(`${vdd} V ${test ? 'self-tested' : 'untested'}: ${((avoided / cells) * 100).toFixed(1)}% cells faulty, wrong on ${wrongChips}/${chips} chips`)
  }
  console.log(`${ex.name} (${n} inputs, ${inputs.length} tried, ${(performance.now() - t0).toFixed(0)} ms): ${notes.join('; ')}`)
}
console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
