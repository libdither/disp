// Checks the signal-tree machine against the two-way interpreter. For each physics, each example and each
// layout, with one rule book: the first run (learning rules), a second run on the same input, three runs on
// other bits, and the first run flipped back; each must give core.ts's answer (or the input again). Prints
// the ticks of each, the rewrites by the machine's own rules and by learned ones, and the rules learned;
// with wires of cells, the most wires through one cell, and with sizes, the largest signal.
// `npx --prefix website tsx research/signal-fabric/tree/check.mts [cheap|honest|cell|all]`
import { run, eq, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal, valText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { Book, CHEAP, HONEST, Net, type Layout, type Physics } from './engine.ts'
import { EXAMPLES, otherBits } from './examples.ts'

const PHYSICS: Record<string, Physics> = { cheap: CHEAP, honest: HONEST, cell: { ...HONEST, rules: 'cell' } }
const which = process.argv[2] ?? 'all'
let failures = 0
for (const [name, phys] of Object.entries(PHYSICS)) {
  if (which !== 'all' && which !== name) continue
  console.log(`-- ${name}`)
  let seed = 7
  const rand = () => ((seed = (seed * 1103515245 + 12345) % 2 ** 31) / 2 ** 31)
  for (const layout of ['rows', 'packed'] as Layout[]) for (const ex of EXAMPLES) {
    const p = parseProg(ex.prog)
    const x0 = parseVal(ex.input)
    const book = new Book()
    const t0 = performance.now()
    const parts: string[] = []
    let lanes = 0, size = 0
    const once = (x: V, what: string) => {
      const net = Net.of(x, layout, phys)
      net.book = book
      const want = run(p, x)
      const ticks = net.run(p)
      const got = net.value()
      lanes = Math.max(lanes, net.peakLanes)
      size = Math.max(size, net.peakSize)
      if (!want || !eq(got, want)) { failures++; parts.push(`${what} WRONG: got ${valText(got, 'regs')} want ${want ? valText(want, 'regs') : 'stuck'}`) }
      else parts.push(`${what} ${ticks} (${net.rewrites}+${net.hits})`)
      return net
    }
    try {
      const first = once(x0, 'first')
      once(x0, 'again')
      for (let i = 0; i < 3; i++) once(otherBits(x0, rand), 'other')
      const r0 = first.now
      first.run(p, false)
      if (!eq(first.value(), x0)) { failures++; parts.push('flip WRONG') }
      else parts.push(`flip ${first.now - r0}`)
      const ok = !parts.some((s) => s.includes('WRONG'))
      const extra = `${phys.wires === 'cells' ? `, lanes ${lanes}` : ''}${phys.sizes ? `, signal ${size}` : ''}`
      console.log(`${ok ? 'ok  ' : 'FAIL'} ${layout.padEnd(6)} ${ex.name.padEnd(9)} ${parts.join(', ')}; ${book.all.length} rules${extra} (${(performance.now() - t0).toFixed(0)} ms)`)
    } catch (e) {
      failures++
      console.log(`FAIL ${layout} ${ex.name} ${parts.join(', ')}: ${(e as Error).stack}`)
    }
  }
}
console.log('ticks (rewrites by own rules + by learned ones)')
console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
