// A run, a local move at a time. The state is a tree of two kinds of node: a dot, a program and the value
// it acts on, and a frame, a combinator waiting on the dots of its parts. A dot whose program is held has
// run; any other dot is live. Fire makes one move at one node and looks no further than its children: a
// live dot starts (a move rearranges its value and holds its program; a combinator becomes a frame over
// dots for its parts), a frame whose dots have all run closes, or a sequence goes on to its second part.
// Flip turns a node around and computes nothing: a dot's program is held or let go and turned the other
// way, and a frame flips every dot under it. Undoing a move is flip, fire, flip. Dots that do not wait on
// each other fire in the same round.

import { F, S, bodyOf, flipProg, form, freshId, hold, isHeld, move, size, turn, type Form, type V } from './core'

export type Wait =
  | 'self' // self: the fix's body runs again
  | 'seq1' // a ; b, the part that runs first running
  | 'seq2' // a ; b, the other part running
  | 'fix' // fix(b): b runs, self standing for this
  | 'eval' // (q, z): q runs on z, the fork and q kept
  | 'mapS' // map on a stem: its inside runs, the stem kept
  | 'mapF' // map on a fork: both halves run, the fork kept
  | 'triL' // tri on (△, y): y runs, the fork and leaf kept
  | 'triS' // tri on (△u, y): (u, y) runs, the stem kept
  | 'triF' // tri on ((p, q), y): (p, (q, y)) runs

/** A program on a value; `me` is the fix that `self` inside it stands for. */
export type Dot = { k: 'dot'; id: number; p: V; x: V; me: V | null }
/** A frame keeps the node its form took apart, by id (`at`: the stem or fork map or tri opened, the fork
 * eval split), and what it set aside whole (`kept`: tri's leaf, the program eval runs). */
export type Frame = { k: 'frame'; id: number; p: V; me: V | null; w: Wait; kids: Node[]; at: number; kept: V | null }
export type Node = Dot | Frame

const dot = (p: V, x: V, me: V | null, id = freshId()): Dot => ({ k: 'dot', id, p, x, me })
const frame = (n: Node, w: Wait, kids: Node[], at = 0, kept: V | null = null): Frame => ({
  k: 'frame',
  id: n.id,
  p: n.p,
  me: n.me,
  w,
  kids,
  at,
  kept
})

export const start = (p: V, x: V): Dot => dot(p, x, null)
/** A dot that has run: its program is held. */
export const done = (n: Node): n is Dot => n.k === 'dot' && isHeld(n.p)

/** How a program reads: its form, under the †s on top, and which way those †s make it run. */
export function read(p: V): { f: Form; base: V; fwd: boolean } {
  let fwd = true
  let t = p
  for (let f = form(t); f.k === 'dag'; f = form(t)) {
    fwd = !fwd
    t = f.b
  }
  return { f: form(t), base: t, fwd }
}
/** A dot for a part of a program, run the way the whole is running; none for a part that is held. */
function kid(q: V, fwd: boolean, x: V, me: V | null): Dot | null {
  const r = fwd ? q : turn(q)
  return isHeld(r) ? null : dot(r, x, me)
}
/** A frame over the dots for its parts, or none if a part is held. */
const over = (n: Node, w: Wait, kids: (Dot | null)[], at = 0, kept: V | null = null): Frame | null =>
  kids.every((k) => k) ? frame(n, w, kids as Dot[], at, kept) : null

/** The move at this node, if it has one now: the node it becomes. A combinator never starts a part that is
 * held: that would be a run that had already happened, so a program with one where it would run is
 * stuck there. */
export function fire(n: Node): Node | null {
  const { p, me, id } = n
  const { f, base, fwd } = read(p)
  if (n.k === 'dot') {
    if (isHeld(p)) return null
    const x = n.x
    const ran = (y: V) => dot(hold(p), y, me, id)
    switch (f.k) {
      case 'id':
      case 'held':
        return ran(x)
      case 'none':
      case 'dag':
        return null
      case 'atom': {
        if (f.n === 'self') return me ? over(n, 'self', [kid(bodyOf(me), fwd, x, me)]) : null
        if (f.n === 'eval') return x.tag === 'fork' ? over(n, 'eval', [kid(x.l, fwd, x.r, null)], x.id, x.l) : ran(x)
        const y = move(f.n, x, fwd)
        return y && ran(y)
      }
      case 'seq':
        return over(n, 'seq1', [kid(fwd ? f.a : f.b, fwd, x, me)])
      case 'fix':
        return over(n, 'fix', [kid(f.b, fwd, x, base)])
      case 'map':
        if (x.tag === 'stem') return over(n, 'mapS', [kid(f.a, fwd, x.c, me)], x.id)
        if (x.tag === 'fork') return over(n, 'mapF', [kid(f.b, fwd, x.l, me), kid(f.c, fwd, x.r, me)], x.id)
        return ran(x)
      case 'tri': {
        if (x.tag !== 'fork') return ran(x)
        const s = x.l
        if (s.tag === 'leaf') return over(n, 'triL', [kid(f.a, fwd, x.r, me)], x.id, s)
        if (s.tag === 'stem') return over(n, 'triS', [kid(f.b, fwd, F(s.c, x.r, x.id), me)], s.id)
        return over(n, 'triF', [kid(f.c, fwd, F(s.l, F(s.r, x.r, s.id), x.id), me)])
      }
    }
  }
  if (!n.kids.every(done)) return null
  const ys = n.kids.map((k) => (k as Dot).x)
  const y = ys[0]
  const ran = (v: V) => dot(hold(p), v, me, id)
  const { at, kept } = n
  switch (n.w) {
    case 'self':
    case 'fix':
    case 'seq2':
      return ran(y)
    case 'seq1': {
      const s = f as { a: V; b: V }
      return over(n, 'seq2', [kid(fwd ? s.b : s.a, fwd, y, me)])
    }
    case 'eval':
    case 'triL':
      return ran(F(kept!, y, at))
    case 'mapS':
      return ran(S(y, at))
    case 'mapF':
      return ran(F(y, ys[1], at))
    case 'triS':
      return y.tag === 'fork' ? ran(F(S(y.l, at), y.r, y.id)) : null
    case 'triF':
      return y.tag === 'fork' && y.r.tag === 'fork' ? ran(F(F(y.l, y.r.l, y.r.id), y.r.r, y.id)) : null
  }
}

/** The same node turned around: nothing is computed, only which way it goes and whether it has run. */
export function flip(n: Node): Node {
  if (n.k === 'dot') return { ...n, p: flipProg(n.p) }
  return { ...n, p: turn(n.p), w: n.w === 'seq1' ? 'seq2' : n.w === 'seq2' ? 'seq1' : n.w, kids: n.kids.map(flip) }
}

/** The move that made this node, undone: flip, fire, flip. */
export function unfire(n: Node): Node | null {
  const m = fire(flip(n))
  return m && flip(m)
}

export const children = (n: Node): Node[] => (n.k === 'frame' ? n.kids : [])

/** Every node that can fire now, outermost first: a node that fires is not looked into, since its move
 * rewrites what is under it. */
export function movable(n: Node): Node[] {
  const out: Node[] = []
  const walk = (t: Node) => {
    if (fire(t)) out.push(t)
    else t.k === 'frame' && t.kids.forEach(walk)
  }
  walk(n)
  return out
}

/** The tree with the nodes whose ids are in `at` fired once, or flipped. */
export function update(n: Node, at: Set<number>, how: (n: Node) => Node | null): Node {
  if (at.has(n.id)) {
    const m = how(n)
    if (m) return m
  }
  if (n.k !== 'frame') return n
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
  for (const k of children(n)) {
    const r = find(k, id)
    if (r) return r
  }
  return null
}

/** The same state, ids aside. */
export function same(a: Node, b: Node): boolean {
  if (a.k !== b.k || a.me !== b.me || !eqV(a.p, b.p)) return false
  if (a.k === 'dot') return eqV(a.x, (b as Dot).x)
  const c = b as Frame
  return (
    a.w === c.w &&
    (a.kept === null ? c.kept === null : c.kept !== null && eqV(a.kept, c.kept)) &&
    a.kids.length === c.kids.length &&
    a.kids.every((k, i) => same(k, c.kids[i]))
  )
}
function eqV(a: V, b: V): boolean {
  if (a === b) return true
  if (a.tag !== b.tag) return false
  if (a.tag === 'stem') return eqV(a.c, (b as typeof a).c)
  if (a.tag === 'fork') return eqV(a.l, (b as typeof a).l) && eqV(a.r, (b as typeof a).r)
  return true
}

/** Rounds and moves from the start until the run has run, every node that can fire firing in each
 * round; `long` where it runs past `maxMoves` or `budgetMs` (it may never end). */
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
  return { rounds, moves, y: (n as Dot).x, stuck: false, long: false }
}

/** Nodes in the state, values and all. */
export const weight = (n: Node): number =>
  n.k === 'dot' ? 1 + size(n.x) : 1 + (n.kept ? size(n.kept) : 0) + n.kids.reduce((s, k) => s + weight(k), 0)

// ---- what a move is, in a glyph and in words ----

/** The mark a node wears for the move it can make now. */
export function glyph(n: Node): string | null {
  if (!fire(n)) return null
  if (n.k === 'frame') return n.w === 'seq1' ? '→' : '✓'
  const { f, fwd } = read(n.p)
  if (f.k !== 'atom') return { id: '=', held: '=', dag: '', seq: ';', tri: '?', map: '∥', fix: 'μ', none: '' }[f.k]
  return { self: '↩', swap: '⇄', cross: '×', sink: '⇅', stem: fwd ? '+' : '−', unit: fwd ? '⊕' : '⊖', eval: '▷' }[f.n]
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
  const { f, fwd } = read(n.p)
  const back = fwd ? '' : ', run backward'
  if (n.k === 'dot') {
    if (isHeld(n.p)) return 'held: it has run; flip it to run it back'
    const x = n.x
    switch (f.k) {
      case 'id':
      case 'held':
        return 'id: nothing to do'
      case 'none':
      case 'dag':
        return 'not a program: no move fits'
      case 'atom':
        if (f.n === 'self') return `self: the fix around it runs its body again${back}`
        if (f.n === 'eval') return x.tag === 'fork' ? `eval: the left part runs as a program on the right${back}` : 'eval: no fork, nothing to run'
        return MOVE_SAYS[f.n][fwd ? 0 : 1]
      case 'seq':
        return fwd ? ';: the first part runs' : ';: run backward, so the second part runs first, turned around'
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
  switch (n.w) {
    case 'seq1':
      return ';: the first part has run, and the second runs on what it gave'
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
      return `${{ self: 'self', seq2: ';', fix: 'fix' }[n.w]}: has run, and is held`
  }
}

/** A short tag for counting moves of a parallel round. */
export function tag(n: Node): string {
  if (n.k === 'frame') return { seq1: ';', seq2: ';', self: 'self', fix: 'fix', eval: 'eval', mapS: 'map', mapF: 'map', triL: 'tri', triS: 'tri', triF: 'tri' }[n.w]
  const { f, fwd } = read(n.p)
  if (f.k === 'atom') return fwd || (f.n !== 'stem' && f.n !== 'unit') ? f.n : f.n + '†'
  return { id: 'id', held: 'id', dag: '†', seq: ';', tri: 'tri', map: 'map', fix: 'fix', none: '?' }[f.k]
}
