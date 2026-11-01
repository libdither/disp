// What each shortcut is worth: addw (or another program) run under the honest physics with one shortcut
// put back at a time, and under the cheap physics with one charge added at a time. Prints the ticks with
// no rules, the first run, and the mean of three runs on other bits, with the largest signal (symbols) and
// the most wires through one cell.
// `npx --prefix website tsx research/signal-fabric/tree/ablate.mts [program] [input] [rows|packed]`
import { run, eq, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal } from '../../../website/src/lib/evaluators/twoway/text.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { Book, CHEAP, HONEST, Net, type Physics, type Layout } from './engine.ts'
import { otherBits } from './examples.ts'
const [progName, inp, layout] = [process.argv[2] ?? 'addw', process.argv[3] ?? '<0b1011, 0b0110>', (process.argv[4] ?? 'packed') as Layout]
const p = parseProg(progName), x = parseVal(inp)
const VARIANTS: Record<string, Physics> = {
  cheap: CHEAP,
  'cheap+sizes': { ...CHEAP, sizes: true }, 'cheap+writes': { ...CHEAP, writes: true }, 'cheap+cells': { ...CHEAP, wires: 'cells' }, 'cheap+carried': { ...CHEAP, rules: 'carried' }, 'cheap+cell': { ...CHEAP, rules: 'cell' },
  'cheap+sizes+carried': { ...CHEAP, sizes: true, rules: 'carried' },
  'honest-sizes': { ...HONEST, sizes: false }, 'honest-writes': { ...HONEST, writes: false }, 'honest-cells': { ...HONEST, wires: 'drawn' }, 'honest/shared': { ...HONEST, rules: 'shared' }, 'honest/cell': { ...HONEST, rules: 'cell' },
  honest: HONEST
}
for (const [name, phys] of Object.entries(VARIANTS)) {
  let seed = 3
  const rand = () => ((seed = (seed * 1103515245 + 12345) % 2 ** 31) / 2 ** 31)
  const book = new Book()
  const go = (v: V, b: Book | null) => { const net = Net.of(v, layout, phys); net.book = b; const t = net.run(p); if (!eq(net.value(), run(p, v)!)) throw new Error(`wrong under ${name}`); return [t, net.peakSize, net.peakLanes] }
  const t0 = performance.now()
  const none = go(x, null), first = go(x, book)
  const others = [0, 1, 2].map(() => go(otherBits(x, rand), book))
  console.log(`${name.padEnd(20)} none ${String(none[0]).padStart(6)}  first ${String(first[0]).padStart(6)}  other ${String(Math.round(others.reduce((s, o) => s + o[0], 0) / 3)).padStart(6)}  signal ${Math.max(...others.map(o => o[1]))} lanes ${Math.max(...others.map(o => o[2]))} (${(performance.now() - t0).toFixed(0)} ms)`)
}
