// A two-way program (website/src/lib/evaluators/twoway) compiled into the linear net (rules.ts) for one input.
//
// Nothing in the net copies, so recursion cannot unroll as it runs: the program is laid out as the
// circuit this run goes through, every fix and self the run reaches replaced by its body, and a self it
// never reaches left a hole. Which ones it reaches, a run of the interpreter says (`calls`); the same
// circuit serves the run back, which goes through the same calls. dag is done here too: b† is b's parts
// in the other order, each move its inverse.

import { form, bodyOf, move, F as VF, S as VS, L as VL, Stuck, TooLong, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { GATE_TAG, INVERSE, R, type Gate } from './rules.ts'
import { Net, REF, idOf, type Agent } from './net.ts'

/** The calls a run made inside one call: each fix or self it ran, by the program node, and what that call made. */
export class Calls {
  kids = new Map<V, Calls>()
  at(v: V): Calls {
    let c = this.kids.get(v)
    if (!c) this.kids.set(v, (c = new Calls()))
    return c
  }
  count(): number {
    let n = 1
    for (const k of this.kids.values()) n += k.count()
    return n
  }
}

/** core.ts's `Runner`, noting each call it makes (a self reached twice in one call would be two
 * calls, which a two-way program never does: each part runs at most once in a call). */
export function calls(p: V, x: V, limit = 2_000_000): { out: V | null; calls: Calls } {
  const top = new Calls()
  let steps = 0
  const go = (p: V, x: V, fwd: boolean, me: V | null, c: Calls): V => {
    if (++steps > limit) throw new TooLong()
    const f = form(p)
    switch (f.k) {
      case 'id':
        return x
      case 'none':
        throw new Stuck()
      case 'atom': {
        if (f.n === 'self') {
          if (!me) throw new Stuck()
          if (c.kids.has(p)) throw new Error('a self ran twice in one call')
          return go(bodyOf(me), x, fwd, me, c.at(p))
        }
        if (f.n === 'eval') {
          if (x.tag === 'fork') throw new Stuck()
          return x
        }
        const y = move(f.n, x, fwd)
        if (!y) throw new Stuck()
        return y
      }
      case 'dag':
        return go(f.b, x, !fwd, me, c)
      case 'seq':
        return fwd ? go(f.b, go(f.a, x, fwd, me, c), fwd, me, c) : go(f.a, go(f.b, x, fwd, me, c), fwd, me, c)
      case 'fix':
        if (c.kids.has(p)) throw new Error('a fix ran twice in one call')
        return go(f.b, x, fwd, p, c.at(p))
      case 'map':
        if (x.tag === 'stem') return VS(go(f.a, x.c, fwd, me, c))
        if (x.tag === 'fork') return VF(go(f.b, x.l, fwd, me, c), go(f.c, x.r, fwd, me, c))
        return x
      case 'tri': {
        if (x.tag !== 'fork') return x
        const s = x.l
        if (s.tag === 'leaf') return VF(s, go(f.a, x.r, fwd, me, c))
        if (s.tag === 'stem') {
          const o = go(f.b, VF(s.c, x.r), fwd, me, c)
          if (o.tag !== 'fork') throw new Stuck()
          return VF(VS(o.l), o.r)
        }
        const o = go(f.c, VF(s.l, VF(s.r, x.r)), fwd, me, c)
        if (o.tag !== 'fork' || o.r.tag !== 'fork') throw new Stuck()
        return VF(VF(o.l, o.r.l), o.r.r)
      }
    }
  }
  try {
    return { out: go(p, x, true, null, top), calls: top }
  } catch (e) {
    if (e instanceof Stuck) return { out: null, calls: top }
    throw e
  }
}

/** A compiled part: the circuit's tree before it is made of agents. */
export type T =
  | { k: 'gate'; g: Gate }
  | { k: 'id' }
  | { k: 'hole' }
  | { k: 'seq'; a: T; b: T }
  | { k: 'tri'; on: 'L' | 'S' | 'F'; a: T }
  | { k: 'onstem' | 'first' | 'second'; a: T }
  | { k: 'par'; a: T; b: T }

const ID: T = { k: 'id' }
const seq = (ts: T[]): T => {
  const parts = ts.filter((t) => t.k !== 'id')
  const half = (v: T[]): T => (v.length === 0 ? ID : v.length === 1 ? v[0] : { k: 'seq', a: half(v.slice(0, v.length >> 1)), b: half(v.slice(v.length >> 1)) })
  return half(parts)
}
const flat = (t: T): T[] => (t.k === 'seq' ? [...flat(t.a), ...flat(t.b)] : t.k === 'id' ? [] : [t])

/** p as a circuit, run forward (or backward, `inv`) in call `c`; `fix` is the body self stands for. */
export function compile(p: V, inv: boolean, c: Calls | undefined, fix: V | null): T {
  const f = form(p)
  const sub = (q: V) => compile(q, inv, c, fix)
  switch (f.k) {
    case 'id':
      return ID
    case 'none':
      return { k: 'hole' }
    case 'dag':
      return compile(f.b, !inv, c, fix)
    case 'seq': {
      // each part is already turned round inside; run backward, the second goes first
      const [a, b] = [flat(sub(f.a)), flat(sub(f.b))]
      return seq(inv ? [...b, ...a] : [...a, ...b])
    }
    case 'tri': {
      // tri(a, b, c) is a tri for each shape at the value's left, one after another (they commute)
      const parts: T[] = []
      for (const [on, q] of [['L', f.a], ['S', f.b], ['F', f.c]] as const) {
        const t = sub(q)
        if (t.k !== 'id') parts.push({ k: 'tri', on, a: t })
      }
      return seq(parts)
    }
    case 'map': {
      // map(a, b, c) is map(a, id, id); map(id, b, c)
      const [a, b, cc] = [sub(f.a), sub(f.b), sub(f.c)]
      const onFork: T = b.k === 'id' ? (cc.k === 'id' ? ID : { k: 'second', a: cc }) : cc.k === 'id' ? { k: 'first', a: b } : { k: 'par', a: b, b: cc }
      return seq([a.k === 'id' ? ID : { k: 'onstem', a }, onFork])
    }
    case 'fix': {
      const k = c?.kids.get(p)
      return k ? compile(f.b, inv, k, f.b) : { k: 'hole' }
    }
    case 'atom': {
      if (f.n === 'self') {
        const k = c?.kids.get(p)
        return k && fix ? compile(fix, inv, k, fix) : { k: 'hole' }
      }
      if (f.n === 'eval') throw new Error('eval is not compiled')
      const g = f.n as Gate
      return { k: 'gate', g: inv ? INVERSE[g] : g }
    }
  }
}

/** The agents of a compiled part, at rest (every node facing up); returns the top. */
export function build(net: Net, t: T): Agent {
  const node = (tag: string, kids: T[]) => {
    const n = net.mk(tag)
    kids.forEach((k, i) => net.link(REF(n.id, i + 1), REF(build(net, k).id, 0)))
    return n
  }
  switch (t.k) {
    case 'gate':
      return net.mk(GATE_TAG[t.g])
    case 'id':
      return net.mk('Id')
    case 'hole':
      return net.mk('Gap')
    case 'seq':
      return node('Seq0', [t.a, t.b])
    case 'tri':
      return node(`T${t.on}0`, [t.a])
    case 'onstem':
      return node('M0', [t.a])
    case 'first':
      return node('Fst0', [t.a])
    case 'second':
      return node('Snd0', [t.a])
    case 'par':
      return node('P0', [t.a, t.b])
  }
}

export function value(net: Net, v: V): Agent {
  const n = net.mk(v.tag === 'leaf' ? 'L' : v.tag === 'stem' ? 'S' : 'F')
  ;(v.tag === 'stem' ? [v.c] : v.tag === 'fork' ? [v.l, v.r] : []).forEach((k, i) => net.link(REF(n.id, i + 1), REF(value(net, k).id, 0)))
  return n
}

/** The net that runs p on x: the value's top as the run, going down into the circuit from the root sink. */
export function load(p: V, x: V): { net: Net; out: V | null; calls: number } {
  const run = calls(p, x)
  const net = new Net()
  const out = net.mk('Out')
  const top = build(net, compile(p, false, run.calls, null))
  const v = value(net, x)
  const shape = v.tag as 'L' | 'S' | 'F'
  const r = net.mk(R(shape, 'f'))
  net.link(REF(r.id, 0), REF(top.id, 0))
  net.link(REF(r.id, 1), REF(out.id, 0))
  v.ports.slice(1).forEach((k, i) => net.link(REF(r.id, 2 + i), k))
  net.ag.delete(v.id)
  return { net, out: run.out, calls: run.calls.count() - 1 }
}

/** Agents carrying a run: roots, heads, readers, tris reading, a map turned round holding a half. */
export const CURSOR = /^(R[LSF][fb]|H[fb]|K(u|c|ss|sf)[fb]|T[LSF]r[fb]p?|TFc[fb]p?|PHR[fb])$/

/** The value a finished run holds: a root at the sink, nothing else running. */
export function result(net: Net): { val: V; fwd: boolean } | null {
  const out = [...net.ag.values()].find((a) => a.tag === 'Out')
  if (!out) return null
  const r = net.ag.get(idOf(out.ports[0]))
  if (!r || !/^R[LSF][fb]$/.test(r.tag) || r.ports[0] !== REF(out.id, 0)) return null
  if ([...net.ag.values()].some((a) => a !== r && CURSOR.test(a.tag))) return null
  const read = (a: Agent, from: number): V => {
    const shape = a.tag.startsWith('R') ? a.tag[1] : a.tag
    const kid = (i: number) => read(net.ag.get(idOf(a.ports[from + i]))!, 1)
    return shape === 'L' ? VL() : shape === 'S' ? VS(kid(0)) : VF(kid(0), kid(1))
  }
  return { val: read(r, 2), fwd: r.tag.endsWith('f') }
}

/** The net as text for the lattice (crate/src/linear.rs `parse`): `n|tags|a p b q,…`, agents numbered by place. */
export function serialize(net: Net): string {
  const ids = [...net.ag.keys()]
  const at = new Map(ids.map((id, i) => [id, i]))
  const links: string[] = []
  for (const id of ids) {
    const a = net.ag.get(id)!
    a.ports.forEach((r, p) => {
      if (r < 0) return
      const [b, q] = [idOf(r), r & 7]
      if (b < id || (b === id && q < p)) return
      links.push(`${at.get(id)} ${p} ${at.get(b)} ${q}`)
    })
  }
  return `${ids.length}|${ids.map((id) => net.ag.get(id)!.tag).join(' ')}|${links.join(',')}`
}
