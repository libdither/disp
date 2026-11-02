// Replays bracket abstraction one rewrite at a time, as frames of the whole
// term. The rewrites themselves come from the compiler's own abstractName: this
// only watches its recursion, and checks it lands on the compiler's output.

import { abstractName, eliminateLams, containsFree, cap, S, type Cir } from '../../../../src/elab/cir.ts'
import type { BracketRule } from './protocol.ts'

export interface BracketFrame {
  term: Cir
  rule: BracketRule
  name: string
  at: number[]
  before?: Cir
  after?: Cir
}

const MAX_REWRITES = 1200

const lam = (x: string, body: Cir): Cir => ({ tag: 'lam', x, body })
const sApp = (f: Cir, g: Cir): Cir => cap(cap(S, f), g)

/// Where the rest of the term sits around the spot being rewritten.
interface Hole {
  wrap: (c: Cir) => Cir
  at: number[]
}

function same(a: Cir, b: Cir): boolean {
  if (a === b) return true
  if (a.tag !== b.tag) return false
  switch (a.tag) {
    case 'lit':
      return a.t === (b as typeof a).t
    case 'var':
      return a.name === (b as typeof a).name
    case 'app':
      return same(a.f, (b as typeof a).f) && same(a.x, (b as typeof a).x)
    case 'lam':
      return a.x === (b as typeof a).x && same(a.body, (b as typeof a).body)
    default:
      return true
  }
}

export function bracketReplay(cir: Cir): { frames: BracketFrame[]; skipped: number; mismatch: boolean } {
  const frames: BracketFrame[] = [{ term: cir, rule: 'start', name: '', at: [] }]
  let skipped = 0

  const note = (rule: BracketRule, name: string, hole: Hole, before: Cir, after: Cir, term: Cir) => {
    if (frames.length > MAX_REWRITES) skipped++
    else frames.push({ term: hole.wrap(term), rule, name, at: hole.at, before: lam(name, before), after })
  }

  function abstract(name: string, body: Cir, hole: Hole): Cir {
    const splits = body.tag === 'app' && containsFree(body, name) && !(body.x.tag === 'var' && body.x.name === name && !containsFree(body.f, name))
    if (!splits) {
      const out = abstractName(name, body)
      const rule = !containsFree(body, name) ? 'const' : body.tag === 'var' ? 'id' : body.tag === 'lam' ? 'lam' : 'eta'
      note(rule, name, hole, body, out, out)
      return out
    }
    const f = body.f
    const x = body.x
    const parts = sApp(lam(name, f), lam(name, x))
    note('split', name, hole, body, parts, parts)
    let af: Cir | null = null
    const out = abstractName(name, body, (n, b) => {
      if (af === null) return (af = abstract(n, b, { wrap: (c) => hole.wrap(sApp(c, lam(n, x))), at: [...hole.at, 0, 0] }))
      const left = af
      return abstract(n, b, { wrap: (c) => hole.wrap(sApp(left, c)), at: [...hole.at, 1] })
    })
    // anything but S(f, g) means a shortcut fired on the way back up
    if (!(out.tag === 'app' && out.f.tag === 'app' && out.f.f.tag === 'S')) {
      const rule = out.tag === 'app' && out.f.tag === 'K' ? 'k-comp' : 'eta-after'
      note(rule, name, hole, body, out, out)
    }
    return out
  }

  function walk(e: Cir, hole: Hole): Cir {
    if (e.tag === 'lam') {
      const body = walk(e.body, { wrap: (c) => hole.wrap(lam(e.x, c)), at: [...hole.at, 0] })
      return abstract(e.x, body, hole)
    }
    if (e.tag === 'app') {
      const f = walk(e.f, { wrap: (c) => hole.wrap(cap(c, e.x)), at: [...hole.at, 0] })
      const x = walk(e.x, { wrap: (c) => hole.wrap(cap(f, c)), at: [...hole.at, 1] })
      return f === e.f && x === e.x ? e : cap(f, x)
    }
    return e
  }

  const final = walk(cir, { wrap: (c) => c, at: [] })
  if (skipped > 0) frames.push({ term: final, rule: 'skip', name: '', at: [] })
  return { frames, skipped, mismatch: !same(final, eliminateLams(cir)) }
}

