// A run, a local move at a time. The state is a tree of dots. A dot is a program counter: its program,
// never written to; the parts of the program's sequence, in the order they run (every sequence inside
// opened up, a † reversing the order of what it covers); which way it is going; how many parts have run
// that way; and, while a part that is a call or a combinator is in flight, the dots running that part's
// own parts. Fire makes one move at one dot and looks no further than the dots just under it: a move in
// the sequence fires in place, a call or combinator opens dots for its parts, and once they have all run
// it closes and the count goes on. A map or tri whose parts are single moves fires in one step. A dot has
// run when its count reaches the end. Flip turns a dot around and computes nothing: the other way,
// counting from the other end, and the dots under it flipped too. Undoing a move is flip, fire, flip.
// Dots that do not wait on each other fire in the same round.

import { F, S, bodyOf, form, freshId, move, size, type Form, type V } from './core'

export type Wait =
  | 'self' // self: the fix's body runs again
  | 'fix' // fix(b): b runs, self standing for this
  | 'eval' // (q, z): q runs on z, the fork and q kept
  | 'mapS' // map on a stem: its inside runs, the stem kept
  | 'mapF' // map on a fork: both halves run, the fork kept
  | 'triL' // tri on (△, y): y runs, the fork and leaf kept
  | 'triS' // tri on (△u, y): (u, y) runs, the stem kept
  | 'triF' // tri on ((p, q), y): (p, (q, y)) runs

/** A program on a value, `pc` parts of its sequence run in direction `fwd`; `me` is the fix that `self`
 * inside it stands for. While a part is in flight, `x` is null and `kids` run its parts; `at` is the
 * node its form took apart, by id (the stem or fork map or tri opened, the fork eval split), and `kept`
 * what it set aside whole (tri's leaf, the program eval runs). */
export type Dot = {
  k: 'dot'
  id: number
  p: V
  fwd: boolean
  pc: number
  x: V | null
  me: V | null
  w: Wait | null
  kids: Dot[]
  at: number
  kept: V | null
}
export type Node = Dot

/** A part of a sequence, and whether it runs turned around. */
export type Part = [V, boolean]
const SPINES = new WeakMap<V, Part[]>()
/** The parts of a program's sequence in the order they run forward: every sequence inside opened up, a
 * † reversing the order of what it covers and turning each part; id parts are dropped. */
export function spine(p: V): Part[] {
  let s = SPINES.get(p)
  if (s) return s
  const out: Part[] = []
  const walk = (q: V, rev: boolean) => {
    const f = form(q)
    if (f.k === 'id') return
    if (f.k === 'seq') {
      walk(rev ? f.b : f.a, rev)
      walk(rev ? f.a : f.b, rev)
    } else if (f.k === 'dag') walk(f.b, !rev)
    else out.push([q, rev])
  }
  walk(p, false)
  SPINES.set(p, out)
  return out
}

/** How a part reads: its form, under the †s on top, and which way those †s make it run. */
export function read(p: V): { f: Form; base: V; fwd: boolean } {
  let fwd = true
  let t = p
  for (let f = form(t); f.k === 'dag'; f = form(t)) {
    fwd = !fwd
    t = f.b
  }
  return { f: form(t), base: t, fwd }
}

const dot = (p: V, fwd: boolean, x: V, me: V | null): Dot => ({ k: 'dot', id: freshId(), p, fwd, pc: 0, x, me, w: null, kids: [], at: 0, kept: null })
export const start = (p: V, x: V): Dot => dot(p, true, x, null)
/** A dot that has run: its count is at the end. */
export const done = (n: Node): boolean => n.kids.length === 0 && n.pc >= spine(n.p).length

/** Where a dot is in its program, in the order the parts run forward: the parts before it have been
 * applied to the value, and while one is in flight, it is the part at this index. */
export function cursor(n: Dot): number {
  const len = spine(n.p).length
  return n.fwd ? n.pc : len - n.pc - (n.kids.length ? 1 : 0)
}

/** The part a dot is at, and which way it runs; none at the end. */
export function partOf(n: Dot): { q: V; f: Form; base: V; dir: boolean } | null {
  const s = spine(n.p)
  if (n.pc >= s.length) return null
  const [q, rev] = s[n.fwd ? n.pc : s.length - 1 - n.pc]
  const r = read(q)
  return { q, f: r.f, base: r.base, dir: n.fwd !== rev === r.fwd }
}

/** A part that is a single move or id, under its †s (self and eval are calls, not moves). */
function plain(q: V): boolean {
  const { f } = read(q)
  return f.k === 'id' || (f.k === 'atom' && f.n !== 'self' && f.n !== 'eval')
}
/** A single move or id run on v in direction d: null where it is stuck. */
function moveOf(q: V, v: V, d: boolean): V | null {
  const r = read(q)
  return r.f.k === 'atom' ? move(r.f.n, v, d === r.fwd) : v
}

/** The move at this dot, if it has one now: the dot it becomes. */
export function fire(n: Node): Node | null {
  if (n.kids.length) {
    if (!n.kids.every(done)) return null
    const y = close(n)
    return y && { ...n, pc: n.pc + 1, x: y, w: null, kids: [], at: 0, kept: null }
  }
  const at = partOf(n)
  if (!at) return null
  const { f, base, dir } = at
  const x = n.x!
  const me = n.me
  const next = (y: V): Dot => ({ ...n, pc: n.pc + 1, x: y })
  const open = (w: Wait, kids: Dot[], a = 0, kept: V | null = null): Dot => ({ ...n, x: null, w, kids, at: a, kept })
  const inline = (q: V, v: V, wrap: (y: V) => V | null): Dot | null => {
    const y = moveOf(q, v, dir)
    const z = y && wrap(y)
    return z ? next(z) : null
  }
  switch (f.k) {
    case 'id':
      return next(x)
    case 'none':
    case 'dag':
    case 'seq':
      return null
    case 'atom': {
      if (f.n === 'self') return me ? open('self', [dot(bodyOf(me), dir, x, me)]) : null
      if (f.n === 'eval') return x.tag === 'fork' ? open('eval', [dot(x.l, dir, x.r, null)], x.id, x.l) : next(x)
      const y = move(f.n, x, dir)
      return y && next(y)
    }
    case 'fix':
      return open('fix', [dot(f.b, dir, x, base)])
    case 'map':
      if (x.tag === 'stem') {
        if (plain(f.a)) return inline(f.a, x.c, (y) => S(y, x.id))
        return open('mapS', [dot(f.a, dir, x.c, me)], x.id)
      }
      if (x.tag === 'fork') {
        if (plain(f.b) && plain(f.c)) {
          const l = moveOf(f.b, x.l, dir)
          const r = l && moveOf(f.c, x.r, dir)
          return r ? next(F(l, r, x.id)) : null
        }
        return open('mapF', [dot(f.b, dir, x.l, me), dot(f.c, dir, x.r, me)], x.id)
      }
      return next(x)
    case 'tri': {
      if (x.tag !== 'fork') return next(x)
      const s = x.l
      if (s.tag === 'leaf') {
        if (plain(f.a)) return inline(f.a, x.r, (y) => F(s, y, x.id))
        return open('triL', [dot(f.a, dir, x.r, me)], x.id, s)
      }
      if (s.tag === 'stem') {
        const y = F(s.c, x.r, x.id)
        if (plain(f.b)) return inline(f.b, y, (o) => (o.tag === 'fork' ? F(S(o.l, s.id), o.r, o.id) : null))
        return open('triS', [dot(f.b, dir, y, me)], s.id)
      }
      const y = F(s.l, F(s.r, x.r, s.id), x.id)
      if (plain(f.c)) return inline(f.c, y, (o) => (o.tag === 'fork' && o.r.tag === 'fork' ? F(F(o.l, o.r.l, o.r.id), o.r.r, o.id) : null))
      return open('triF', [dot(f.c, dir, y, me)])
    }
  }
}

/** What a part in flight gives, once the dots for its parts have all run. */
function close(n: Dot): V | null {
  const ys = n.kids.map((k) => k.x!)
  const y = ys[0]
  const { at, kept } = n
  switch (n.w!) {
    case 'self':
    case 'fix':
      return y
    case 'eval':
    case 'triL':
      return F(kept!, y, at)
    case 'mapS':
      return S(y, at)
    case 'mapF':
      return F(y, ys[1], at)
    case 'triS':
      return y.tag === 'fork' ? F(S(y.l, at), y.r, y.id) : null
    case 'triF':
      return y.tag === 'fork' && y.r.tag === 'fork' ? F(F(y.l, y.r.l, y.r.id), y.r.r, y.id) : null
  }
}

/** The same dot turned around: the other way, counting from the other end. Nothing is computed. */
export function flip(n: Node): Node {
  const len = spine(n.p).length
  return { ...n, fwd: !n.fwd, pc: n.kids.length ? len - 1 - n.pc : len - n.pc, kids: n.kids.map(flip) }
}

/** The move that made this dot, undone: flip, fire, flip. */
export function unfire(n: Node): Node | null {
  const m = fire(flip(n))
  return m && flip(m)
}

/** Which way the dot's next move goes: its part's way, or the dot's own at the end. */
export const backward = (n: Dot): boolean => !(partOf(n)?.dir ?? n.fwd)

export const children = (n: Node): Node[] => n.kids

/** Every dot that can fire now, outermost first: a dot that fires is not looked into, since its move
 * rewrites what is under it. */
export function movable(n: Node): Node[] {
  const out: Node[] = []
  const walk = (t: Node) => {
    if (fire(t)) out.push(t)
    else t.kids.forEach(walk)
  }
  walk(n)
  return out
}

/** The tree with the dots whose ids are in `at` fired once, or flipped. */
export function update(n: Node, at: Set<number>, how: (n: Node) => Node | null): Node {
  if (at.has(n.id)) {
    const m = how(n)
    if (m) return m
  }
  if (!n.kids.length) return n
  let changed = false
  const kids = n.kids.map((k) => {
    const r = update(k, at, how)
    if (r !== k) changed = true
    return r
  })
  return changed ? { ...n, kids } : n
}
export const fireAll = (n: Node, at: Set<number>) => update(n, at, fire)

export function find(n: Node, id: number): Node | null {
  if (n.id === id) return n
  for (const k of n.kids) {
    const r = find(k, id)
    if (r) return r
  }
  return null
}

/** The same state, ids aside. */
export function same(a: Node, b: Node): boolean {
  return (
    a.fwd === b.fwd &&
    a.pc === b.pc &&
    a.w === b.w &&
    a.me === b.me &&
    eqV(a.p, b.p) &&
    eqV(a.x, b.x) &&
    eqV(a.kept, b.kept) &&
    a.kids.length === b.kids.length &&
    a.kids.every((k, i) => same(k, b.kids[i]))
  )
}
function eqV(a: V | null, b: V | null): boolean {
  if (a === b) return true
  if (!a || !b || a.tag !== b.tag) return false
  if (a.tag === 'stem') return eqV(a.c, (b as typeof a).c)
  if (a.tag === 'fork') return eqV(a.l, (b as typeof a).l) && eqV(a.r, (b as typeof a).r)
  return true
}

/** Rounds and moves from the start until the run has run, every dot that can fire firing in each round;
 * `long` where it runs past `maxMoves` or `budgetMs` (it may never end). */
export function measure(
  p: V,
  x: V,
  maxMoves = 400_000,
  budgetMs = Infinity
): { rounds: number; moves: number; y: V | null; stuck: boolean; long: boolean } {
  let n: Node = start(p, x)
  let rounds = 0
  let moves = 0
  const until = performance.now() + budgetMs
  while (!done(n)) {
    if ((rounds & 63) === 63 && performance.now() > until) return { rounds, moves, y: null, stuck: false, long: true }
    const ms = movable(n)
    if (!ms.length) return { rounds, moves, y: null, stuck: true, long: false }
    n = fireAll(n, new Set(ms.map((m) => m.id)))
    moves += ms.length
    rounds++
    if (moves > maxMoves) return { rounds, moves, y: null, stuck: false, long: true }
  }
  return { rounds, moves, y: n.x, stuck: false, long: false }
}

/** Nodes in the state, values and all. */
export const weight = (n: Node): number =>
  1 + (n.x ? size(n.x) : 0) + (n.kept ? size(n.kept) : 0) + n.kids.reduce((s, k) => s + weight(k), 0)

// ---- what a move is, in a glyph and in words ----

/** The mark a dot wears for the move it can make now. */
export function glyph(n: Node): string | null {
  if (!fire(n)) return null
  if (n.kids.length) return '✓'
  const { f, dir } = partOf(n)!
  if (f.k !== 'atom') return { id: '=', tri: '?', map: '∥', fix: 'μ', dag: '', seq: '', none: '' }[f.k]
  return { self: '↩', swap: '⇄', cross: '×', sink: '⇅', stem: dir ? '+' : '−', unit: dir ? '⊕' : '⊖', eval: '▷' }[f.n]
}

const MOVE_SAYS: Record<string, [string, string]> = {
  swap: ['swap: (u, v) becomes (v, u)', 'swap: (u, v) becomes (v, u)'],
  cross: ['cross: ((u, v), w) becomes ((u, w), v)', 'cross: ((u, v), w) becomes ((u, w), v)'],
  sink: ['sink: △(u, v) becomes (△u, v), and (△u, v) becomes △(u, v)', 'sink: △(u, v) becomes (△u, v), and (△u, v) becomes △(u, v)'],
  stem: ['stem: x becomes △x', 'stem†: △x becomes x'],
  unit: ['unit: x becomes (△, x)', 'unit†: (△, x) becomes x']
}

/** What firing n does, said for the readout. */
export function say(n: Node): string {
  if (n.kids.length) {
    switch (n.w!) {
      case 'mapS':
        return 'map: the stem closes over what its inside gave'
      case 'mapF':
        return 'map: the fork closes over what both halves gave'
      case 'triL':
        return 'tri: the △ goes back on the left'
      case 'triS':
        return 'tri: the stem goes back on the left'
      case 'triF':
        return 'tri: the left fork closes again'
      case 'eval':
        return 'eval: the program goes back beside its result'
      default:
        return `${n.w}: its part has run, and the count moves on`
    }
  }
  const at = partOf(n)
  if (!at) return 'it has run; flip it to run it back'
  const { f, dir } = at
  const back = dir ? '' : ', run backward'
  const x = n.x!
  switch (f.k) {
    case 'id':
      return 'id: nothing to do'
    case 'none':
    case 'dag':
    case 'seq':
      return 'not a program: no move fits'
    case 'atom':
      if (f.n === 'self') return `self: the fix around it runs its body again${back}`
      if (f.n === 'eval') return x.tag === 'fork' ? `eval: the left part runs as a program on the right${back}` : 'eval: no fork, nothing to run'
      return MOVE_SAYS[f.n][dir ? 0 : 1]
    case 'fix':
      return `fix: inside, self stands for this program${back}`
    case 'map':
      if (x.tag === 'stem') return `map: a stem, so the first part runs inside it${back}`
      if (x.tag === 'fork') return `map: a fork, so its halves run the other two parts at once${back}`
      return 'map: a leaf, nothing inside'
    case 'tri': {
      if (x.tag !== 'fork') return 'tri: not a fork, nothing to look at'
      const s = x.l.tag
      if (s === 'leaf') return `tri: the left is △, so the first part runs on the right${back}`
      if (s === 'stem') return `tri: the left is a stem; it comes off, and the second part runs${back}`
      return `tri: the left is a fork; it opens, and the third part runs on (p, (q, y))${back}`
    }
  }
}

/** A short tag for counting moves of a parallel round. */
export function tag(n: Node): string {
  if (n.kids.length) return { self: 'self', fix: 'fix', eval: 'eval', mapS: 'map', mapF: 'map', triL: 'tri', triS: 'tri', triF: 'tri' }[n.w!]
  const at = partOf(n)
  if (!at) return 'id'
  const { f, dir } = at
  if (f.k === 'atom') return dir || (f.n !== 'stem' && f.n !== 'unit') ? f.n : f.n + '†'
  return { id: 'id', dag: '†', seq: ';', tri: 'tri', map: 'map', fix: 'fix', none: '?' }[f.k]
}
