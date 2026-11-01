// How addw's time grows with the word: for 4 to 32 bits, in each layout and physics, the ticks with no
// rules, the first run (learning), and the mean of three runs on other bits with what it learned. Under
// each column, its growth from 8 to 32 bits as an exponent (ticks ∝ n^e) and how far that is above the
// n^0.5 any 2D layout needs to reach n bits from one root. Physics: cheap, honest, cell (honest, rules kept
// in the cell they were learned in), shared (honest but for one book every node reads: not local).
// `npx --prefix website tsx research/signal-fabric/tree/scale.mts [physics,…] [rows,packed] [bits,…]`
import { run, eq, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal } from '../../../website/src/lib/evaluators/twoway/text.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { Book, CHEAP, HONEST, Net, type Layout, type Physics } from './engine.ts'
import { otherBits } from './examples.ts'

const PHYSICS: Record<string, Physics> = { cheap: CHEAP, honest: HONEST, cell: { ...HONEST, rules: 'cell' }, shared: { ...HONEST, rules: 'shared' } }
const physics = (process.argv[2] ?? 'cheap,honest').split(',')
const layouts = (process.argv[3] ?? 'rows,packed').split(',') as Layout[]
const sizes = (process.argv[4] ?? '4,8,16,32').split(',').map(Number)
const p = parseProg('addw')
const word = (n: number, s: number) => Array.from({ length: n }, (_, i) => ((i * 7 + s) % 3 ? '1' : '0')).join('')
const exp = (a: number, b: number, n0: number, n1: number) => Math.log(b / a) / Math.log(n1 / n0)

for (const name of physics) for (const layout of layouts) {
  const phys = PHYSICS[name]
  console.log(`-- ${name}, ${layout}`)
  console.log('bits   no rules     first  other bits   largest signal  most lanes   seconds')
  const rows: number[][] = []
  for (const n of sizes) {
    const t0 = performance.now()
    const x = parseVal(`<0b${word(n, 1)}, 0b${word(n, 2)}>`)
    let seed = 3
    const rand = () => ((seed = (seed * 1103515245 + 12345) % 2 ** 31) / 2 ** 31)
    const book = new Book()
    let size = 0, lanes = 0
    const go = (v: V, b: Book | null) => {
      const net = Net.of(v, layout, phys)
      net.book = b
      const t = net.run(p)
      if (!eq(net.value(), run(p, v)!)) throw new Error(`wrong at ${n} bits`)
      size = Math.max(size, net.peakSize)
      lanes = Math.max(lanes, net.peakLanes)
      return t
    }
    const none = go(x, null), first = go(x, book)
    const other = Math.round([0, 1, 2].map(() => go(otherBits(x, rand), book)).reduce((s, t) => s + t, 0) / 3)
    rows.push([n, none, first, other])
    const f = (v: number, w: number) => String(v).padStart(w)
    console.log(`${f(n, 4)} ${f(none, 10)} ${f(first, 9)} ${f(other, 11)} ${f(size, 16)} ${f(lanes, 11)} ${((performance.now() - t0) / 1000).toFixed(1).padStart(9)}`)
  }
  const a = rows.find((r) => r[0] === 8), b = rows.find((r) => r[0] === 32)
  if (a && b) {
    const e = [1, 2, 3].map((i) => exp(a[i], b[i], 8, 32))
    console.log(`growth 8→32: n^${e.map((x) => x.toFixed(2)).join(', n^')}   (above n^0.5 by ${e.map((x) => (x - 0.5).toFixed(2)).join(', ')})`)
  }
}
