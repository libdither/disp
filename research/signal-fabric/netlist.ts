// A two-way program run on a word whose bits are unknowns, giving the fixed circuit it computes: core.ts's
// interpreter, where a value may be a bit (△ if false, △△ if true) or a choice between two values on a
// condition. Wherever the program looks at a bit's shape the run splits, and the branches merge back
// into gates; a word's shape never depends on its bits for the library's word programs, so the result
// is one circuit. `npx tsx research/signal-fabric/netlist.ts`
import { regs, L as VL, S as VS, F as VF, type V } from '../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg } from '../../website/src/lib/evaluators/twoway/text.ts'
import '../../website/src/lib/evaluators/twoway/library.ts'
import { nodes, compile, type SV } from './symbolic.ts'

/** Gates and depth of the circuit computing a value's bits; whether its shape is fixed. */
function circuit(out: SV) {
  const roots: number[] = []
  let fixed = true
  const walk = (v: SV) => {
    if (v.k === 'B') roots.push(v.b)
    else if (v.k === 'S') walk(v.c)
    else if (v.k === 'F') { walk(v.l); walk(v.r) }
    else if (v.k === 'M') { fixed = false; roots.push(v.c); walk(v.t); walk(v.f) }
  }
  walk(out)
  const depth = new Map<number, number>()
  const count: Record<string, number> = {}
  const d = (i: number): number => {
    if (i < 2) return 0
    const known = depth.get(i)
    if (known !== undefined) return known
    const n = nodes[i]
    // a node that is just its variable is a wire; its negation an inverter; anything else a multiplexer
    const wire = n.lo === 0 && n.hi === 1
    const kind = wire ? 'wire' : n.lo === 1 && n.hi === 0 ? 'not' : 'mux'
    const r = wire ? 0 : Math.max(d(n.lo), d(n.hi)) + 1
    depth.set(i, r)
    if (!wire) count[kind] = (count[kind] ?? 0) + 1
    return r
  }
  const deepest = Math.max(0, ...roots.map(d))
  return { gates: Object.values(count).reduce((a, b) => a + b, 0), count, depth: deepest, fixed }
}

const word = (n: number, k: number) => Array.from({ length: n }, (_, i) => ((k >> i) & 1 ? '1' : '0')).join('')
const only = process.argv[2]
// name, program, input, registers the input holds (0: a tree of bits)
const cases: [string, string, V, number][] = [
  ['notw', 'notw', regs([word(8, 0)]), 1],
  ['notw', 'notw', regs([word(64, 0)]), 1],
  ['cnotw', 'cnotw', regs([word(8, 0), word(8, 0)]), 2],
  ['addw', 'addw', regs([word(4, 0), word(4, 0)]), 2],
  ['addw', 'addw', regs([word(8, 0), word(8, 0)]), 2],
  ['mirror', 'mirror', VF(VF(VL(), VS(VL())), VF(VS(VL()), VL())), 0]
]
for (const [name, src, x, rs] of cases.filter((_, i) => only === undefined || String(i) === only)) {
  const t0 = performance.now()
  try {
    const { out, inputs, stuck, steps } = compile(parseProg(src), x, rs)
    const c = circuit(out)
    console.log(`${name} on ${x.tag === 'fork' ? 'a word' : 'a value'} of ${inputs} bits: ${c.gates} gates (${Object.entries(c.count).map(([k, v]) => `${v} ${k}`).join(', ')}), depth ${c.depth}, shape ${c.fixed ? 'fixed' : 'depends on the bits'}, stuck ${stuck === 0 ? 'never' : 'on some inputs'}; ${steps} steps, ${(performance.now() - t0).toFixed(0)} ms`)
  } catch (e) {
    console.log(`${name}: ${(e as Error).message}`)
  }
}
