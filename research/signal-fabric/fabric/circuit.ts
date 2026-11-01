// A two-way program as a circuit to lay out: the symbolic run (../symbolic.ts) gives each output bit as a
// decision diagram over the input bits; each diagram node that is not just an input becomes a gate
// reading its own input bit and the one or two nodes under it. A net is a diagram node: an input's net
// is its input pad's, a gate's is its output, and 0 and 1 are constants.
import { parseProg, parseVal } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { run as interpret, registersOf, asBit, bit, L, S, F, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { compile, symbolic, nodes, not, evalNode, type SV } from '../symbolic.ts'

/** ¬x, x ⊕ a, x ∧ a, ¬x ∧ a, x ∨ a, ¬x ∨ a, or x ? b : a, for the gate's input bit x. */
export type Kind = 'not' | 'xor' | 'and' | 'andn' | 'or' | 'orn' | 'mux'
export interface Gate {
  net: number // the diagram node it drives
  kind: Kind
  ins: number[] // nets read: the input bit x's, then a (and b)
  level: number
}
export interface Pad { net: number; label: string }
export interface Circuit {
  src: string
  input: V
  regs: number // registers the input holds, 0 for a tree of bits
  inputs: Pad[]
  outputs: Pad[]
  gates: Gate[]
  depth: number
  out: SV
  stuck: number
}

const isVar = (i: number) => i > 1 && nodes[i].lo === 0 && nodes[i].hi === 1

function kindOf(i: number): Kind {
  const { lo, hi } = nodes[i]
  if (lo === 1 && hi === 0) return 'not'
  if (lo === 0) return 'and'
  if (hi === 0) return 'andn'
  if (hi === 1) return 'or'
  if (lo === 1) return 'orn'
  if (hi === not(lo)) return 'xor'
  return 'mux'
}
/** A gate's output from the values of the nets it reads. */
export function gateOut(kind: Kind, v: number[]): number {
  const [x, a, b] = v
  switch (kind) {
    case 'not': return x ^ 1
    case 'xor': return x ^ a
    case 'and': return x & a
    case 'andn': return (x ^ 1) & a
    case 'or': return x | a
    case 'orn': return (x ^ 1) | a
    case 'mux': return x ? b : a
  }
}

const subscript = (n: number) => String(n).replace(/\d/g, (d) => '₀₁₂₃₄₅₆₇₈₉'[+d])
/** The bit slots of a symbolic value in order, each as its diagram node (0 or 1 if constant) and a label:
 * register and place where it holds `regs` registers, else every unknown bit of a tree of bits. */
function slots(v: SV, regs: number): Pad[] {
  const out: Pad[] = []
  const bitOf = (s: SV) => (s.k === 'B' ? s.b : s.k === 'L' ? 0 : s.k === 'S' && s.c.k === 'L' ? 1 : 0)
  let place = 0
  const word = (w: SV) => {
    if (w.k === 'F') { word(w.l); word(w.r); return }
    if (w.k !== 'S') return
    let t = w.c
    for (let r = 0; r < regs; r++) {
      const last = r === regs - 1
      const name = regs === 1 ? 'x' : 'abcdefgh'[r] ?? `r${r}`
      out.push({ net: bitOf(last || t.k !== 'F' ? t : t.l), label: name + subscript(place) })
      if (!last && t.k === 'F') t = t.r
    }
    place++
  }
  const tree = (t: SV) => {
    if (t.k === 'B') out.push({ net: t.b, label: 'x' + subscript(out.length) })
    else if (t.k === 'S') tree(t.c)
    else if (t.k === 'F') { tree(t.l); tree(t.r) }
  }
  if (regs) word(v); else tree(v)
  return out
}

/** Compiles a program for inputs shaped like `inputText`, a value: registers `<0b…, 0b…>` or a tree of T/F. */
export function circuitOf(src: string, inputText: string): Circuit {
  const input = parseVal(inputText)
  const regs = /<|0b/.test(inputText) ? (registersOf(input)?.length ?? 0) : 0
  const { out, inputs: n, stuck } = compile(parseProg(src), input, regs)
  const fixed = (v: SV): boolean => (v.k === 'M' ? false : v.k === 'S' ? fixed(v.c) : v.k === 'F' ? fixed(v.l) && fixed(v.r) : true)
  if (!fixed(out)) throw new Error("the output's shape depends on its bits: no one circuit computes it")
  // the same walk compile numbered the inputs by, so pad i drives the node input(i)
  const inputs = slots(symbolic(input, regs), regs)
  if (inputs.length !== n) throw new Error('input slots and symbolic inputs disagree')
  const sample = interpret(parseProg(src), withBits(input, regs, inputs.map(() => 0)))
  const outputs = slots(out, regs && sample ? (registersOf(sample)?.length ?? 0) : 0)

  const gates = new Map<number, Gate>()
  const level = (i: number): number => {
    if (i <= 1 || isVar(i)) return 0
    let g = gates.get(i)
    if (!g) {
      const kind = kindOf(i)
      const { v, lo, hi } = nodes[i]
      const under = { not: [], xor: [lo], and: [hi], andn: [lo], or: [lo], orn: [hi], mux: [lo, hi] }[kind]
      g = { net: i, kind, ins: [inputs[v].net, ...under], level: 0 }
      gates.set(i, g)
      g.level = 1 + Math.max(0, ...under.map(level))
    }
    return g.level
  }
  for (const p of outputs) level(p.net)
  const list = [...gates.values()].sort((a, b) => a.level - b.level)
  return { src, input, regs, inputs, outputs, gates: list, depth: Math.max(0, ...list.map((g) => g.level)), out, stuck }
}

/** The input value with its bits set to `bits`, in slot order. */
export function withBits(input: V, regs: number, bits: number[]): V {
  let i = 0
  const next = () => bit(!!bits[i++])
  const tuple = (k: number): V => (k === 1 ? next() : F(next(), tuple(k - 1)))
  const word = (w: V): V => (w.tag === 'stem' ? S(tuple(regs)) : w.tag === 'fork' ? F(word(w.l), word(w.r)) : L())
  const tree = (t: V): V => (asBit(t) !== null ? next() : t.tag === 'stem' ? S(tree(t.c)) : t.tag === 'fork' ? F(tree(t.l), tree(t.r)) : L())
  return regs ? word(input) : tree(input)
}
/** The input's own bits, in slot order. */
export function bitsOf(input: V, regs: number): number[] {
  const out: number[] = []
  const tuple = (t: V, k: number) => {
    if (k === 1) out.push(asBit(t) ?? 0)
    else if (t.tag === 'fork') { out.push(asBit(t.l) ?? 0); tuple(t.r, k - 1) }
  }
  const word = (w: V) => { if (w.tag === 'stem') tuple(w.c, regs); else if (w.tag === 'fork') { word(w.l); word(w.r) } }
  const tree = (t: V) => {
    const b = asBit(t)
    if (b !== null) out.push(b)
    else if (t.tag === 'stem') tree(t.c)
    else if (t.tag === 'fork') { tree(t.l); tree(t.r) }
  }
  if (regs) word(input); else tree(input)
  return out
}
/** The output value for the output pads' bits: the circuit's shape with its unknown bits filled in. */
export function outputValue(c: Circuit, bits: number[]): V {
  const varying = c.outputs.flatMap((p, i) => (p.net > 1 ? [i] : []))
  let k = 0
  const walk = (v: SV): V => (v.k === 'B' ? bit(!!bits[varying[k++]]) : v.k === 'S' ? S(walk(v.c)) : v.k === 'F' ? F(walk(v.l), walk(v.r)) : L())
  return walk(c.out)
}
/** What the two-way interpreter answers for these input bits, or null where it gets stuck. */
export const answer = (c: Circuit, bits: number[]): V | null => interpret(parseProg(c.src), withBits(c.input, c.regs, bits))
/** The output pads' bits straight from the diagrams: what a fault-free fabric must give. */
export const reference = (c: Circuit, bits: number[]): number[] => c.outputs.map((p) => evalNode(p.net, bits))
