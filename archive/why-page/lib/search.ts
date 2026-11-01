// A toy of the search the page describes: half-written tree-calculus programs, tried against
// a spec. Running a half-written program either gets an answer or stops at a hole it needs
// filled, so a wrong answer rules out every way of filling the holes it never looked at.
// Disp's own search is not built; this is the idea, in the page's JavaScript.

export type P = { k: 'hole'; id: number } | { k: 'leaf' } | { k: 'stem'; a: P } | { k: 'fork'; a: P; b: P }

const LEAF: P = { k: 'leaf' }
const stem = (a: P): P => ({ k: 'stem', a })
const fork = (a: P, b: P): P => ({ k: 'fork', a, b })

class Blocked {
  constructor(readonly hole: number) {}
}
class OutOfFuel {}

/** Applies f to x, the five tree-calculus rules; throws Blocked at a hole whose shape the rule needs. */
function apply(f: P, x: P, fuel: { n: number }): P {
  if (--fuel.n < 0) throw new OutOfFuel()
  switch (f.k) {
    case 'hole':
      throw new Blocked(f.id)
    case 'leaf':
      return stem(x)
    case 'stem':
      return fork(f.a, x)
    case 'fork': {
      const a = f.a
      if (a.k === 'hole') throw new Blocked(a.id)
      if (a.k === 'leaf') return f.b
      if (a.k === 'stem') return apply(apply(a.a, x, fuel), apply(f.b, x, fuel), fuel)
      if (x.k === 'hole') throw new Blocked(x.id)
      if (x.k === 'leaf') return a.a
      if (x.k === 'stem') return apply(a.b, x.a, fuel)
      return apply(apply(f.b, x.a, fuel), x.b, fuel)
    }
  }
}

/** Compares a result against a wanted tree: false once a shape differs, Blocked where a hole must be read. */
function same(got: P, want: P): boolean {
  if (got.k === 'hole') throw new Blocked(got.id)
  if (got.k !== want.k) return false
  if (got.k === 'stem') return same(got.a, (want as { a: P }).a)
  if (got.k === 'fork') return same(got.a, (want as { a: P }).a) && same(got.b, (want as { b: P }).b)
  return true
}

export type Spec = { name: string; cases: [P, P][] }

export type Outcome = { v: 'Proved' } | { v: 'Refuted'; input: P } | { v: 'Open'; hole: number }

/** A spec here is a finite list of cases, so every verdict is final. */
export function judge(prog: P, spec: Spec): Outcome {
  for (const [input, want] of spec.cases) {
    try {
      if (!same(apply(prog, input, { n: 2000 }), want)) return { v: 'Refuted', input }
    } catch (e) {
      if (e instanceof Blocked) return { v: 'Open', hole: e.hole }
      if (e instanceof OutOfFuel) return { v: 'Refuted', input }
      throw e
    }
  }
  return { v: 'Proved' }
}

function fill(p: P, id: number, by: P): P {
  if (p.k === 'hole') return p.id === id ? by : p
  if (p.k === 'stem') return stem(fill(p.a, id, by))
  if (p.k === 'fork') return fork(fill(p.a, id, by), fill(p.b, id, by))
  return p
}

export type Node = { id: number; prog: P; depth: number; bits: number; parent: number | null; outcome: Outcome; children: number[] }

type Shape = 'leaf' | 'stem' | 'fork'
/** A proposer: how likely each shape is at a hole. Nothing it says is trusted, only the order changes. */
export type Proposer = (prog: P, hole: number) => Record<Shape, number>
export const NO_IDEA: Proposer = () => ({ leaf: 1 / 3, stem: 1 / 3, fork: 1 / 3 })

export type Result = { nodes: Node[]; found: Node | null; budgetSpent: boolean }

/** Best first by bits spent, -log2 of the proposer's odds along the way, so a shape it called unlikely costs more to reach. */
export function search(spec: Spec, proposer: Proposer, limit = 20000): Result {
  let nextHole = 1
  const nodes: Node[] = []
  const queue: { prog: P; depth: number; bits: number; parent: number | null }[] = [
    { prog: { k: 'hole', id: 0 }, depth: 0, bits: 0, parent: null }
  ]
  while (queue.length > 0 && nodes.length < limit) {
    let best = 0
    for (let i = 1; i < queue.length; i++) if (queue[i].bits < queue[best].bits) best = i
    const { prog, depth, bits, parent } = queue.splice(best, 1)[0]
    const outcome = judge(prog, spec)
    const node: Node = { id: nodes.length, prog, depth, bits, parent, outcome, children: [] }
    nodes.push(node)
    if (parent !== null) nodes[parent].children.push(node.id)
    if (outcome.v === 'Proved') return { nodes, found: node, budgetSpent: false }
    if (outcome.v !== 'Open') continue
    const odds = proposer(prog, outcome.hole)
    for (const shape of ['leaf', 'stem', 'fork'] as const) {
      const by: P =
        shape === 'leaf'
          ? LEAF
          : shape === 'stem'
            ? stem({ k: 'hole', id: nextHole++ })
            : fork({ k: 'hole', id: nextHole++ }, { k: 'hole', id: nextHole++ })
      queue.push({ prog: fill(prog, outcome.hole, by), depth: depth + 1, bits: bits - Math.log2(odds[shape]), parent: node.id })
    }
  }
  return { nodes, found: null, budgetSpent: queue.length > 0 }
}

/** A proposer that knows a program and puts `trust` of its odds on that program's shape at each hole; a low
 * `trust` makes it a confident liar. */
export function proposerFrom(target: P, trust: number): Proposer {
  return (prog, hole) => {
    const at = (p: P, t: P): Shape | null => {
      if (p.k === 'hole') return p.id === hole ? (t.k === 'hole' ? 'leaf' : t.k) : null
      if (p.k === 'stem' && t.k === 'stem') return at(p.a, t.a)
      if (p.k === 'fork' && t.k === 'fork') return at(p.a, t.a) ?? at(p.b, t.b)
      return null
    }
    const best = at(prog, target)
    const rest = (1 - trust) / 2
    if (!best) return NO_IDEA(prog, hole)
    return { leaf: rest, stem: rest, fork: rest, [best]: trust }
  }
}

/** Trees of each size, by node count: how fast blind enumeration grows. */
export function treesOfSize(max: number): number[] {
  const c = [0]
  for (let n = 1; n <= max; n++) {
    let total = n === 1 ? 1 : c[n - 1]
    for (let i = 1; i <= n - 2; i++) total += c[i] * c[n - 1 - i]
    c.push(total)
  }
  return c.slice(1)
}

export const size = (p: P): number =>
  p.k === 'leaf' ? 1 : p.k === 'hole' ? 0 : p.k === 'stem' ? 1 + size(p.a) : 1 + size(p.a) + size(p.b)

export const show = (p: P): string => {
  if (p.k === 'hole') return '?'
  if (p.k === 'leaf') return 't'
  const arg = (q: P) => (q.k === 'leaf' || q.k === 'hole' ? show(q) : `(${show(q)})`)
  return p.k === 'stem' ? `t ${arg(p.a)}` : `t ${arg(p.a)} ${arg(p.b)}`
}

const T = LEAF
const nat = (n: number): P => (n === 0 ? T : fork(T, nat(n - 1)))
const TT = stem(T)

export const SPECS: Spec[] = [
  { name: 'not', cases: [[T, TT], [TT, T]] },
  { name: 'add one', cases: [[nat(0), nat(1)], [nat(1), nat(2)], [nat(2), nat(3)]] },
  { name: 'take a pair apart', cases: [[fork(T, TT), T], [fork(TT, T), TT], [fork(nat(1), T), nat(1)]] },
  { name: 'is it zero?', cases: [[nat(0), T], [nat(1), TT], [nat(2), TT], [nat(3), TT]] }
]

/** The share of all trees up to size `max` that the refuted half-written programs ruled out, each refuted
 * program standing for every way of filling its holes. They cannot overlap: one tree fits one branch. */
export function ruledOut(nodes: Node[], max: number): number {
  const counts = [0, ...treesOfSize(max)]
  const total = counts.reduce((a, b) => a + b, 0)
  const holes = (p: P): number => (p.k === 'hole' ? 1 : p.k === 'stem' ? holes(p.a) : p.k === 'fork' ? holes(p.a) + holes(p.b) : 0)
  // fills[h][m]: ways to fill h holes with trees of m nodes in all
  const fills: number[][] = [[1, ...Array(max).fill(0)]]
  for (let h = 1; h <= max; h++) {
    const row = Array(max + 1).fill(0)
    for (let m = 0; m <= max; m++) for (let k = 1; k <= m; k++) row[m] += counts[k] * fills[h - 1][m - k]
    fills.push(row)
  }
  let out = 0
  for (const n of nodes) {
    if (n.outcome.v !== 'Refuted') continue
    const h = holes(n.prog)
    if (h > max) continue
    for (let m = 0; m + size(n.prog) <= max; m++) out += fills[h][m]
  }
  return out / total
}
