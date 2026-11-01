// A two-way program run on a value whose bits are unknowns, giving the fixed circuit it computes: core.ts's
// interpreter, where a value may be a bit (△ if false, △△ if true) or a choice between two values on a
// condition. Wherever the program looks at a bit's shape the run splits, and the branches merge back
// into gates; a word's shape never depends on its bits for the library's word programs, so the result
// is one circuit. netlist.ts counts its gates; fabric/ lays it out on a cell array.
import { form, bodyOf, asBit, type V } from '../../website/src/lib/evaluators/twoway/core.ts'

// ---- booleans as reduced ordered decision diagrams (0 false, 1 true), so equal conditions are equal
// numbers and an impossible branch is exactly 0. A diagram read as a circuit is one 2:1 multiplexer a node.
export type Node = { v: number; lo: number; hi: number }
export const nodes: Node[] = [{ v: Infinity, lo: 0, hi: 0 }, { v: Infinity, lo: 1, hi: 1 }]
const unique = new Map<string, number>()
function node(v: number, lo: number, hi: number): number {
  if (lo === hi) return lo
  const k = `${v},${lo},${hi}`
  let i = unique.get(k)
  if (i === undefined) { i = nodes.push({ v, lo, hi }) - 1; unique.set(k, i) }
  return i
}
export const input = (i: number) => node(i, 0, 1)
const iteMemo = new Map<string, number>()
/** if f then g else h */
export function ite(f: number, g: number, h: number): number {
  if (f === 1) return g
  if (f === 0) return h
  if (g === h) return g
  if (g === 1 && h === 0) return f
  const k = `${f},${g},${h}`
  const known = iteMemo.get(k)
  if (known !== undefined) return known
  const v = Math.min(nodes[f].v, nodes[g].v, nodes[h].v)
  const cof = (x: number, b: 0 | 1) => (nodes[x].v === v ? (b ? nodes[x].hi : nodes[x].lo) : x)
  const r = node(v, ite(cof(f, 0), cof(g, 0), cof(h, 0)), ite(cof(f, 1), cof(g, 1), cof(h, 1)))
  iteMemo.set(k, r)
  return r
}
export const not = (a: number) => ite(a, 0, 1)
const and = (a: number, b: number) => ite(a, b, 0)
const or = (a: number, b: number) => ite(a, 1, b)
const mux = (c: number, a: number, b: number) => ite(c, a, b)
/** A diagram's value on concrete inputs. */
export function evalNode(i: number, bits: ArrayLike<number>): number {
  while (i > 1) i = bits[nodes[i].v] ? nodes[i].hi : nodes[i].lo
  return i
}

// ---- symbolic values ----
export type SV = { k: 'L' } | { k: 'S'; c: SV } | { k: 'F'; l: SV; r: SV } | { k: 'B'; b: number } | { k: 'M'; c: number; t: SV; f: SV }
const L: SV = { k: 'L' }
const S = (c: SV): SV => ({ k: 'S', c })
const F = (l: SV, r: SV): SV => ({ k: 'F', l, r })
const bitOf = (v: SV): number | null => (v.k === 'B' ? v.b : v.k === 'L' ? 0 : v.k === 'S' && v.c.k === 'L' ? 1 : null)
const B = (b: number): SV => (b === 0 ? L : b === 1 ? S(L) : { k: 'B', b })
function same(a: SV, b: SV): boolean {
  if (a === b) return true
  if (a.k !== b.k) return false
  switch (a.k) {
    case 'L': return true
    case 'S': return same(a.c, (b as typeof a).c)
    case 'F': return same(a.l, (b as typeof a).l) && same(a.r, (b as typeof a).r)
    case 'B': return a.b === (b as typeof a).b
    case 'M': { const o = b as typeof a; return a.c === o.c && same(a.t, o.t) && same(a.f, o.f) }
  }
}
/** c ? t : f, pushed down through shapes the two share, bits becoming gates. */
function choose(c: number, t: SV, f: SV): SV {
  if (c === 1) return t
  if (c === 0) return f
  if (same(t, f)) return t
  const bt = bitOf(t), bf = bitOf(f)
  if (bt !== null && bf !== null) return B(mux(c, bt, bf))
  if (t.k === 'S' && f.k === 'S') return S(choose(c, t.c, f.c))
  if (t.k === 'F' && f.k === 'F') return F(choose(c, t.l, f.l), choose(c, t.r, f.r))
  return { k: 'M', c, t, f }
}
/** The value's top shape in each case: [condition, value whose top is L, S or F]. */
function view(v: SV, c = 1): [number, SV][] {
  if (c === 0) return []
  if (v.k === 'B') return [[and(c, v.b), S(L)], [and(c, not(v.b)), L]].filter(([q]) => q !== 0) as [number, SV][]
  if (v.k === 'M') return [...view(v.t, and(c, v.c)), ...view(v.f, and(c, not(v.c)))]
  return [[c, v]]
}
const merge = (alts: [number, SV][]): SV => alts.reduceRight<SV | null>((acc, [c, v]) => (acc ? choose(c, v, acc) : v), null) ?? L

let stuck = 0 // condition under which the run is stuck
let steps = 0
function run(p: V, x: SV, fwd: boolean, me: V | null, pc: number): SV {
  if (++steps > 2_000_000) throw new Error('too long: not a fixed circuit at this size')
  const f = form(p)
  const each = (fn: (top: SV, q: number) => SV) => merge(view(x, pc).map(([q, top]) => [q, fn(top, q)]))
  const fail = (q: number): SV => { stuck = or(stuck, q); return L }
  switch (f.k) {
    case 'id': return x
    case 'none': return fail(pc)
    case 'dag': return run(f.b, x, !fwd, me, pc)
    case 'seq': return fwd ? run(f.b, run(f.a, x, fwd, me, pc), fwd, me, pc) : run(f.a, run(f.b, x, fwd, me, pc), fwd, me, pc)
    case 'fix': return run(f.b, x, fwd, p, pc)
    case 'atom': {
      if (f.n === 'self') return me ? run(bodyOf(me), x, fwd, me, pc) : fail(pc)
      if (f.n === 'eval') throw new Error('eval is not compiled')
      const n = f.n === 'stem' || f.n === 'unit' ? (fwd ? f.n : f.n + '†') : f.n
      if (n === 'stem') return S(x)
      if (n === 'unit') return F(L, x)
      return each((t, q) => {
        if (n === 'swap') return t.k === 'F' ? F(t.r, t.l) : t
        if (n === 'stem†') return t.k === 'S' ? t.c : fail(q)
        if (n === 'unit†') return t.k === 'F' ? merge(view(t.l, q).map(([q2, l]) => [q2, l.k === 'L' ? t.r : fail(q2)])) : fail(q)
        if (n === 'cross') return t.k === 'F' ? merge(view(t.l, q).map(([q2, l]) => [q2, l.k === 'F' ? F(F(l.l, t.r), l.r) : t])) : t
        // sink
        if (t.k === 'S') return merge(view(t.c, q).map(([q2, c]) => [q2, c.k === 'F' ? F(S(c.l), c.r) : t]))
        if (t.k === 'F') return merge(view(t.l, q).map(([q2, l]) => [q2, l.k === 'S' ? S(F(l.c, t.r)) : t]))
        return t
      })
    }
    case 'map':
      return each((t, q) => (t.k === 'S' ? S(run(f.a, t.c, fwd, me, q)) : t.k === 'F' ? F(run(f.b, t.l, fwd, me, q), run(f.c, t.r, fwd, me, q)) : t))
    case 'tri':
      return each((t, q) => {
        if (t.k !== 'F') return t
        return merge(view(t.l, q).map(([q2, s]) => {
          if (s.k === 'L') return [q2, F(L, run(f.a, t.r, fwd, me, q2))]
          if (s.k === 'S') {
            const o = run(f.b, F(s.c, t.r), fwd, me, q2)
            return [q2, merge(view(o, q2).map(([q3, oo]) => [q3, oo.k === 'F' ? F(S(oo.l), oo.r) : fail(q3)]))]
          }
          const fs = s as { l: SV; r: SV }
          const o = run(f.c, F(fs.l, F(fs.r, t.r)), fwd, me, q2)
          return [q2, merge(view(o, q2).map(([q3, oo]) => [q3, oo.k === 'F' ? merge(view(oo.r, q3).map(([q4, rr]) => [q4, rr.k === 'F' ? F(F(oo.l, rr.l), rr.r) : fail(q4)])) : fail(q3)]))]
        }))
      })
  }
}

/** The value with each of its bits made an unknown, numbered left to right. A bit is a △ or △△ where one
 * can be; in registers stored bit by bit (`regs`, core.ts), the bits are the tuple at each leaf of the
 * word, under the stem that marks the leaf, since a △△ there would read as a bit too. */
export function symbolic(v: V, regs: number, next = { i: 0 }): SV {
  const bitIn = () => B(input(next.i++))
  const tuple = (k: number): SV => (k === 1 ? bitIn() : F(bitIn(), tuple(k - 1)))
  const word = (w: V): SV => (w.tag === 'stem' ? S(tuple(regs)) : w.tag === 'fork' ? F(word(w.l), word(w.r)) : L)
  const tree = (t: V): SV => (asBit(t) !== null ? bitIn() : t.tag === 'stem' ? S(tree(t.c)) : t.tag === 'fork' ? F(tree(t.l), tree(t.r)) : L)
  return regs ? word(v) : tree(v)
}

/** p run on x with x's bits unknown (x holds `regs` registers, or 0 for a tree of bits): the output, whose
 * bits are diagrams over the inputs, the number of inputs, the condition under which the run is stuck,
 * and the steps the symbolic run took. */
export function compile(p: V, x: V, regs: number, fwd = true) {
  stuck = 0
  steps = 0
  const next = { i: 0 }
  const out = run(p, symbolic(x, regs, next), fwd, null, 1)
  return { out, inputs: next.i, stuck, steps }
}
