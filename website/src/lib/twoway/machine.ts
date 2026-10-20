// A run, a local move at a time. The state is a tree of three kinds of node: a run of a program on a
// value, a frame waiting on the runs it started, and a done run with its result. Every move rewrites
// one node and looks no further than its children: a run starts (an atom moves its value; a combinator
// becomes a frame over the runs of its parts), a frame whose parts are done finishes, or a sequence goes
// on to its second part. Every move has an inverse that looks just as far, so a run goes backward the
// same way it goes forward. Calls that do not wait on each other move in the same round.

import { F, S, bodyOf, form, freshId, move, size, type V } from './core'

export type Wait =
  | 'self' // self: the fix's body runs again
  | 'dag' // b†: b runs the other way
  | 'seq1' // a ; b, a running
  | 'seq2' // a ; b, b running
  | 'fix' // fix(b): b runs, self standing for this
  | 'eval' // (q, z): q runs on z, the fork and q kept
  | 'mapS' // map on a stem: its inside runs, the stem kept
  | 'mapF' // map on a fork: both halves run, the fork kept
  | 'triL' // tri on (△, y): y runs, the fork and leaf kept
  | 'triS' // tri on (△u, y): (u, y) runs, the stem kept
  | 'triF' // tri on ((p, q), y): (p, (q, y)) runs

interface Base {
  id: number
  p: V
  me: V | null
  fwd: boolean
}
export type Run = Base & { k: 'run'; x: V }
export type Done = Base & { k: 'done'; y: V }
/** A frame keeps the node its form took apart, by id (`at`: the stem or fork map or tri opened, the fork
 * eval split), and what it set aside whole (`kept`: tri's leaf, the program eval runs). */
export type Frame = Base & { k: 'frame'; w: Wait; kids: Node[]; at: number; kept: V | null }
export type Node = Run | Done | Frame

const run = (p: V, x: V, me: V | null, fwd: boolean, id = freshId()): Run => ({ k: 'run', id, p, x, me, fwd })
const done = (p: V, y: V, me: V | null, fwd: boolean, id = freshId()): Done => ({ k: 'done', id, p, y, me, fwd })
const frame = (n: Base, w: Wait, kids: Node[], at = 0, kept: V | null = null): Frame => ({
  k: 'frame',
  id: n.id,
  p: n.p,
  me: n.me,
  fwd: n.fwd,
  w,
  kids,
  at,
  kept
})

export const start = (p: V, x: V): Run => run(p, x, null, true)

/** The move at this node, if it has one now: the node it becomes. */
export function fire(n: Node): Node | null {
  if (n.k === 'done') return null
  const { p, me, fwd, id } = n
  const f = form(p)
  if (n.k === 'run') {
    const x = n.x
    const out = (y: V) => done(p, y, me, fwd, id)
    switch (f.k) {
      case 'id':
        return out(x)
      case 'none':
        return null
      case 'atom': {
        if (f.n === 'self') return me ? frame(n, 'self', [run(bodyOf(me), x, me, fwd)]) : null
        if (f.n === 'eval') return x.tag === 'fork' ? frame(n, 'eval', [run(x.l, x.r, null, fwd)], x.id, x.l) : out(x)
        const y = move(f.n, x, fwd)
        return y && out(y)
      }
      case 'dag':
        return frame(n, 'dag', [run(f.b, x, me, !fwd)])
      case 'seq':
        return frame(n, 'seq1', [run(fwd ? f.a : f.b, x, me, fwd)])
      case 'fix':
        return frame(n, 'fix', [run(f.b, x, p, fwd)])
      case 'map':
        if (x.tag === 'stem') return frame(n, 'mapS', [run(f.a, x.c, me, fwd)], x.id)
        if (x.tag === 'fork') return frame(n, 'mapF', [run(f.b, x.l, me, fwd), run(f.c, x.r, me, fwd)], x.id)
        return out(x)
      case 'tri': {
        if (x.tag !== 'fork') return out(x)
        const s = x.l
        if (s.tag === 'leaf') return frame(n, 'triL', [run(f.a, x.r, me, fwd)], x.id, s)
        if (s.tag === 'stem') return frame(n, 'triS', [run(f.b, F(s.c, x.r, x.id), me, fwd)], s.id)
        return frame(n, 'triF', [run(f.c, F(s.l, F(s.r, x.r, s.id), x.id), me, fwd)])
      }
    }
  }
  if (!n.kids.every((k) => k.k === 'done')) return null
  const ys = n.kids.map((k) => (k as Done).y)
  const y = ys[0]
  const out = (v: V) => done(p, v, me, fwd, id)
  const { at, kept } = n
  switch (n.w) {
    case 'self':
    case 'dag':
    case 'fix':
    case 'seq2':
      return out(y)
    case 'seq1': {
      const s = f as { a: V; b: V }
      return frame(n, 'seq2', [run(fwd ? s.b : s.a, y, me, fwd)])
    }
    case 'eval':
    case 'triL':
      return out(F(kept!, y, at))
    case 'mapS':
      return out(S(y, at))
    case 'mapF':
      return out(F(y, ys[1], at))
    case 'triS':
      return y.tag === 'fork' ? out(F(S(y.l, at), y.r, y.id)) : null
    case 'triF':
      return y.tag === 'fork' && y.r.tag === 'fork' ? out(F(F(y.l, y.r.l, y.r.id), y.r.r, y.id)) : null
  }
}

/** The move that made this node, run backward: the node it was, or null if no move made it. */
export function unfire(n: Node): Node | null {
  if (n.k === 'run') {
    return null
  }
  const { p, me, fwd, id } = n
  const f = form(p)
  if (n.k === 'done') {
    const y = n.y
    const back = (x: V) => run(p, x, me, fwd, id)
    switch (f.k) {
      case 'id':
        return back(y)
      case 'none':
        return null
      case 'atom': {
        if (f.n === 'self') return me ? frame(n, 'self', [done(bodyOf(me), y, me, fwd)]) : null
        if (f.n === 'eval') return y.tag === 'fork' ? frame(n, 'eval', [done(y.l, y.r, null, fwd)], y.id, y.l) : back(y)
        const x = move(f.n, y, !fwd)
        return x && back(x)
      }
      case 'dag':
        return frame(n, 'dag', [done(f.b, y, me, !fwd)])
      case 'seq':
        return frame(n, 'seq2', [done(fwd ? f.b : f.a, y, me, fwd)])
      case 'fix':
        return frame(n, 'fix', [done(f.b, y, p, fwd)])
      case 'map':
        if (y.tag === 'stem') return frame(n, 'mapS', [done(f.a, y.c, me, fwd)], y.id)
        if (y.tag === 'fork') return frame(n, 'mapF', [done(f.b, y.l, me, fwd), done(f.c, y.r, me, fwd)], y.id)
        return back(y)
      case 'tri': {
        if (y.tag !== 'fork') return back(y)
        const s = y.l
        if (s.tag === 'leaf') return frame(n, 'triL', [done(f.a, y.r, me, fwd)], y.id, s)
        if (s.tag === 'stem') return frame(n, 'triS', [done(f.b, F(s.c, y.r, y.id), me, fwd)], s.id)
        return frame(n, 'triF', [done(f.c, F(s.l, F(s.r, y.r, s.id), y.id), me, fwd)])
      }
    }
  }
  if (!n.kids.every((k) => k.k === 'run')) return null
  const xs = n.kids.map((k) => (k as Run).x)
  const x = xs[0]
  const back = (v: V) => run(p, v, me, fwd, id)
  const { at, kept } = n
  switch (n.w) {
    case 'self':
    case 'dag':
    case 'fix':
    case 'seq1':
      return back(x)
    case 'seq2': {
      const s = f as { a: V; b: V }
      return frame(n, 'seq1', [done(fwd ? s.a : s.b, x, me, fwd)])
    }
    case 'eval':
    case 'triL':
      return back(F(kept!, x, at))
    case 'mapS':
      return back(S(x, at))
    case 'mapF':
      return back(F(x, xs[1], at))
    case 'triS':
      return x.tag === 'fork' ? back(F(S(x.l, at), x.r, x.id)) : null
    case 'triF':
      return x.tag === 'fork' && x.r.tag === 'fork' ? back(F(F(x.l, x.r.l, x.r.id), x.r.r, x.id)) : null
  }
}

export const children = (n: Node): Node[] => (n.k === 'frame' ? n.kids : [])

/** Every node that has a move in this direction now, outermost first: a node that moves is not looked
 * into, since its move rewrites what is under it. */
export function movable(n: Node, fwd: boolean): Node[] {
  const out: Node[] = []
  const walk = (t: Node) => {
    if ((fwd ? fire : unfire)(t)) out.push(t)
    else t.k === 'frame' && t.kids.forEach(walk)
  }
  walk(n)
  return out
}

/** The tree with the nodes whose ids are in `at` each moved once (forward or back). */
export function moveAll(n: Node, at: Set<number>, fwd: boolean): Node {
  if (at.has(n.id)) {
    const m = (fwd ? fire : unfire)(n)
    if (m) return m
  }
  if (n.k !== 'frame') return n
  let changed = false
  const kids = n.kids.map((k) => {
    const r = moveAll(k, at, fwd)
    if (r !== k) changed = true
    return r
  })
  return changed ? { ...n, kids } : n
}

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
  if (a.k !== b.k || a.p !== b.p || a.fwd !== b.fwd || a.me !== b.me) return false
  if (a.k === 'run') return eqV(a.x, (b as Run).x)
  if (a.k === 'done') return eqV(a.y, (b as Done).y)
  const c = b as Frame
  return (
    a.w === c.w &&
    (a.kept === null ? c.kept === null : c.kept !== null && eqV(a.kept, c.kept)) &&
    a.kids.length === c.kids.length &&
    a.kids.every((k, i) => same(k, c.kids[i]))
  )
}
function eqV(a: V, b: V): boolean {
  if (a.tag !== b.tag) return false
  if (a.tag === 'stem') return eqV(a.c, (b as typeof a).c)
  if (a.tag === 'fork') return eqV(a.l, (b as typeof a).l) && eqV(a.r, (b as typeof a).r)
  return true
}

/** Rounds and moves from the start to the end, every movable node moving in each round. */
/** Rounds and moves from the start to the end, every movable node moving in each round; `long` where it
 * runs past `maxMoves` or `budgetMs` (it may never end). */
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
  while (n.k !== 'done') {
    if ((rounds & 63) === 63 && performance.now() > until) return { rounds, moves, y: null, stuck: false, long: true }
    const ms = movable(n, true)
    if (!ms.length) return { rounds, moves, y: null, stuck: true, long: false }
    n = moveAll(n, new Set(ms.map((m) => m.id)), true)
    moves += ms.length
    rounds++
    if (moves > maxMoves) return { rounds, moves, y: null, stuck: false, long: true }
  }
  return { rounds, moves, y: n.y, stuck: false, long: false }
}

/** Nodes in the state, values and all. */
export const weight = (n: Node): number =>
  n.k === 'run' ? 1 + size(n.x) : n.k === 'done' ? 1 + size(n.y) : 1 + (n.kept ? size(n.kept) : 0) + n.kids.reduce((s, k) => s + weight(k), 0)

// ---- what a move is, in a glyph and in words ----

/** The mark a node wears for the move it can make now (forward, or back with `fwd` false). */
export function glyph(n: Node, fwd: boolean): string | null {
  if (!fwd) return unfire(n) ? '↶' : null
  if (!fire(n)) return null
  if (n.k === 'frame') return n.w === 'seq1' ? '→' : '✓'
  const f = form(n.p)
  if (f.k !== 'atom') return { id: '=', dag: '†', seq: ';', tri: '?', map: '∥', fix: 'μ', none: '' }[f.k]
  const back = !n.fwd
  return {
    self: '↩',
    not: '¬',
    swap: '⇄',
    rot: back ? '↺' : '↻',
    stem: back ? '−' : '+',
    unit: back ? '⊖' : '⊕',
    eval: '▷'
  }[f.n]
}

const ATOM_SAYS: Record<string, [string, string]> = {
  not: ['not: △ and △△ trade places', 'not: △ and △△ trade places'],
  swap: ['swap: (u, v) becomes (v, u)', 'swap: (u, v) becomes (v, u)'],
  rot: ['rot: ((u, v), w) becomes (u, (v, w))', 'rot†: (u, (v, w)) becomes ((u, v), w)'],
  stem: ['stem: x becomes △x', 'stem†: △x becomes x'],
  unit: ['unit: x becomes (△, x)', 'unit†: (△, x) becomes x']
}

/** What the move at n does, said for the readout: the move a run starts with, or a frame's last one. */
export function say(n: Node): string {
  const f = form(n.p)
  if (n.k === 'run') {
    const x = n.x
    switch (f.k) {
      case 'id':
        return 'id: nothing to do'
      case 'none':
        return 'not a program: no move fits'
      case 'atom':
        if (f.n === 'self') return 'self: the fix around it runs its body again'
        if (f.n === 'eval') return x.tag === 'fork' ? 'eval: the left part runs as a program on the right' : 'eval: no fork, nothing to run'
        return ATOM_SAYS[f.n][n.fwd ? 0 : 1]
      case 'dag':
        return '†: the part inside runs the other way'
      case 'seq':
        return n.fwd ? ';: the first part runs' : ';: run backward, so the second part runs first'
      case 'fix':
        return 'fix: inside, self stands for this program'
      case 'map':
        if (x.tag === 'stem') return 'map: a stem, so the first part runs inside it'
        if (x.tag === 'fork') return 'map: a fork, so its halves run the other two parts at once'
        return 'map: a leaf, nothing inside'
      case 'tri': {
        if (x.tag !== 'fork') return 'tri: not a fork, nothing to look at'
        const s = x.l.tag
        if (s === 'leaf') return 'tri: the left is △, so the first part runs on the right'
        if (s === 'stem') return 'tri: the left is a stem; it comes off, and the second part runs'
        return 'tri: the left is a fork; it opens, and the third part runs on (p, (q, y))'
      }
    }
  }
  if (n.k === 'done') return 'done'
  switch (n.w) {
    case 'seq1':
      return ';: the first part is done, and the second runs on what it gave'
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
      return `${{ self: 'self', dag: '†', seq2: ';', fix: 'fix' }[n.w]}: finished`
  }
}

/** A short tag for counting moves of a parallel round. */
export function tag(n: Node): string {
  const f = form(n.p)
  if (n.k === 'frame') return { seq1: ';', seq2: ';', self: 'self', dag: '†', fix: 'fix', eval: 'eval', mapS: 'map', mapF: 'map', triL: 'tri', triS: 'tri', triF: 'tri' }[n.w]
  if (f.k === 'atom') return n.fwd || f.n === 'not' || f.n === 'swap' || f.n === 'self' || f.n === 'eval' ? f.n : f.n + '†'
  return { id: 'id', dag: '†', seq: ';', tri: 'tri', map: 'map', fix: 'fix', none: '?' }[f.k]
}
