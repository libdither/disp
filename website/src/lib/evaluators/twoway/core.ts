// Two-way programs: a reversible calculus on trees. A program is a tree, read by its top shape the way
// tree calculus reads one, and built from five moves that rearrange a tree (swap, cross, sink, stem, unit;
// the first three are their own inverse) and a few ways to put programs together: a then b, b run backward, a branch on the shape of a value's left
// part that puts that shape back (tri), the same on the value's own shape with its parts at once (map),
// and recursion (fix, self). Programs being trees, a program can make, take apart and run programs
// (eval). machine.ts runs one a local move at a time; `run` here is the same thing all at once.

export type V =
  | { tag: 'leaf'; id: number }
  | { tag: 'stem'; c: V; id: number }
  | { tag: 'fork'; l: V; r: V; id: number }

let nextId = 1
export const freshId = () => nextId++
/** Each node has an id, so a drawing can follow it while moves rearrange the tree around it. */
export const L = (id = freshId()): V => ({ tag: 'leaf', id })
export const S = (c: V, id = freshId()): V => ({ tag: 'stem', c, id })
export const F = (l: V, r: V, id = freshId()): V => ({ tag: 'fork', l, r, id })

export const kids = (v: V): V[] => (v.tag === 'stem' ? [v.c] : v.tag === 'fork' ? [v.l, v.r] : [])
const SIZES = new WeakMap<V, number>()
export function size(v: V): number {
  let n = SIZES.get(v)
  if (n === undefined) SIZES.set(v, (n = 1 + kids(v).reduce((s, c) => s + size(c), 0)))
  return n
}
export function eq(a: V, b: V): boolean {
  if (a === b) return true
  if (a.tag !== b.tag) return false
  if (a.tag === 'stem') return eq(a.c, (b as typeof a).c)
  if (a.tag === 'fork') return eq(a.l, (b as typeof a).l) && eq(a.r, (b as typeof a).r)
  return true
}
const KEYS = new WeakMap<V, string>()
/** The tree as a string, for looking it up. */
export function key(v: V): string {
  let k = KEYS.get(v)
  if (k === undefined) {
    k = v.tag === 'leaf' ? '.' : v.tag === 'stem' ? 's' + key(v.c) : `(${key(v.l)}${key(v.r)})`
    KEYS.set(v, k)
  }
  return k
}
/** The same tree with new ids. */
export const copy = (v: V): V => (v.tag === 'leaf' ? L() : v.tag === 'stem' ? S(copy(v.c)) : F(copy(v.l), copy(v.r)))
export const show = (v: V): string =>
  v.tag === 'leaf' ? '△' : v.tag === 'stem' ? '△' + show(v.c) : `(${show(v.l)}, ${show(v.r)})`

// ---- data -----------------------------------------------------------------------------------------

/** F is △ and T is △△, the two smallest trees, so a boolean is a bit; a number n is n stems on a leaf. */
export const bit = (b: boolean): V => (b ? S(L()) : L())
export const asBit = (v: V): 0 | 1 | null => (v.tag === 'leaf' ? 0 : v.tag === 'stem' && v.c.tag === 'leaf' ? 1 : null)
export function nat(n: number): V {
  let v = L()
  for (let i = 0; i < n; i++) v = S(v)
  return v
}
export function asNat(v: V): number | null {
  let n = 0
  for (let cur = v; ; n++) {
    if (cur.tag === 'leaf') return n
    if (cur.tag !== 'stem') return null
    cur = cur.c
  }
}

/** A word of 2^k bits is a balanced tree of forks, high half on the left, each bit in a stem at a leaf:
 * so a program can tell a bit from a half by its shape. Registers stored bit by bit share one tree,
 * a leaf holding a tuple of bits, (a, b) or (a, (b, c)). */
export function regs(words: string[]): V {
  const n = words[0].length
  if (!n || n & (n - 1) || words.some((w) => w.length !== n)) throw new Error('words have 1, 2, 4, 8, … bits, all the same')
  if (n === 1) {
    const bits = words.map((w) => bit(w === '1'))
    return S(bits.slice(0, -1).reduceRight((t, b) => F(b, t), bits[bits.length - 1]))
  }
  const h = n / 2
  return F(regs(words.map((w) => w.slice(0, h))), regs(words.map((w) => w.slice(h))))
}
function tuple(v: V): string | null {
  const b = asBit(v)
  if (b !== null) return String(b)
  if (v.tag !== 'fork') return null
  const h = asBit(v.l)
  const rest = h === null ? null : tuple(v.r)
  return rest && String(h) + rest
}
/** The registers a tree holds, if it holds some, one string of bits each. */
export function registersOf(v: V): string[] | null {
  if (v.tag === 'stem') {
    const t = tuple(v.c)
    return t ? [...t] : null
  }
  if (v.tag !== 'fork') return null
  const l = registersOf(v.l)
  const r = l && registersOf(v.r)
  if (!l || !r || l.length !== r.length || l[0].length !== r[0].length) return null
  return l.map((s, i) => s + r[i])
}

// ---- programs -------------------------------------------------------------------------------------

export const ATOMS = ['self', 'swap', 'cross', 'sink', 'stem', 'unit', 'eval'] as const
export type Atom = (typeof ATOMS)[number]
/** What a tree does as a program. A tree of no form here is no program, and running it is stuck. */
export type Form =
  | { k: 'id' }
  | { k: 'dag'; b: V }
  | { k: 'seq'; a: V; b: V }
  | { k: 'tri'; a: V; b: V; c: V }
  | { k: 'map'; a: V; b: V; c: V }
  | { k: 'fix'; b: V }
  | { k: 'atom'; n: Atom }
  | { k: 'none' }

const FORMS = new WeakMap<V, Form>()
/** The form of a program: a leaf is id; a fork (l, r) reads l the way tree calculus does, (△, b) b†,
 * (△a, b) a ; b, ((a, b), c)
 * tri(a, b, c); a stem on a fork is △(△, b) fix(b) or △((a, b), c) map(a, b, c); and k stems on a
 * leaf are the moves, △△ self, △△△ swap, … up to eval. */
export function form(p: V): Form {
  let f = FORMS.get(p)
  if (f) return f
  if (p.tag === 'leaf') f = { k: 'id' }
  else if (p.tag === 'fork') {
    const h = p.l
    f =
      h.tag === 'leaf'
        ? { k: 'dag', b: p.r }
        : h.tag === 'stem'
          ? { k: 'seq', a: h.c, b: p.r }
          : { k: 'tri', a: h.l, b: h.r, c: p.r }
  } else if (p.c.tag === 'fork') {
    const h = p.c.l
    f =
      h.tag === 'leaf'
        ? { k: 'fix', b: p.c.r }
        : h.tag === 'fork'
          ? { k: 'map', a: h.l, b: h.r, c: p.c.r }
          : { k: 'none' }
  } else {
    const n = asNat(p)
    f = n !== null && n <= ATOMS.length ? { k: 'atom', n: ATOMS[n - 1] } : { k: 'none' }
  }
  FORMS.set(p, f)
  return f
}

export const prog = {
  id: (): V => L(),
  dag: (b: V): V => F(L(), b),
  seq: (a: V, b: V): V => F(S(a), b),
  tri: (a: V, b: V, c: V): V => F(F(a, b), c),
  map: (a: V, b: V, c: V): V => S(F(F(a, b), c)),
  fix: (b: V): V => S(F(L(), b)),
  atom: (n: Atom): V => nat(ATOMS.indexOf(n) + 1)
}

/** One of the five moves on x, forward or backward, keeping every node it does not make or take away:
 * null where it has nothing to undo (stem† needs a stem, unit† a (△, x)). swap, cross and sink leave a
 * tree they do not fit as it is, so each is its own inverse. */
export function move(n: Atom, x: V, fwd: boolean): V | null {
  switch (n) {
    case 'swap':
      return x.tag === 'fork' ? F(x.r, x.l, x.id) : x
    case 'cross':
      return x.tag === 'fork' && x.l.tag === 'fork' ? F(F(x.l.l, x.r, x.l.id), x.l.r, x.id) : x
    case 'sink':
      if (x.tag === 'stem' && x.c.tag === 'fork') return F(S(x.c.l, x.id), x.c.r, x.c.id)
      return x.tag === 'fork' && x.l.tag === 'stem' ? S(F(x.l.c, x.r, x.id), x.l.id) : x
    case 'stem':
      if (fwd) return S(x)
      return x.tag === 'stem' ? x.c : null
    case 'unit':
      if (fwd) return F(L(), x)
      return x.tag === 'fork' && x.l.tag === 'leaf' ? x.r : null
    default:
      return null
  }
}

/** The body a `self` runs: self stands for the innermost fix around it, fix(b), and runs b again. */
export const bodyOf = (me: V): V => (me as { c: { r: V } }).c.r

export class Stuck extends Error {}
export class TooLong extends Error {}

/** The whole run at once, as machine.ts runs it a move at a time. */
export class Runner {
  moves = 0
  limit: number
  constructor(limit = 2_000_000) {
    this.limit = limit
  }
  depth = 0
  run(p: V, x: V, fwd: boolean, me: V | null): V {
    if (++this.moves > this.limit || this.depth > 1500) throw new TooLong()
    this.depth++
    try {
      return this.step(p, x, fwd, me)
    } finally {
      this.depth--
    }
  }
  private step(p: V, x: V, fwd: boolean, me: V | null): V {
    const f = form(p)
    switch (f.k) {
      case 'id':
        return x
      case 'none':
        throw new Stuck()
      case 'atom': {
        if (f.n === 'self') {
          if (!me) throw new Stuck()
          return this.run(bodyOf(me), x, fwd, me)
        }
        if (f.n === 'eval') return x.tag === 'fork' ? F(x.l, this.run(x.l, x.r, fwd, null), x.id) : x
        const y = move(f.n, x, fwd)
        if (!y) throw new Stuck()
        return y
      }
      case 'dag':
        return this.run(f.b, x, !fwd, me)
      case 'seq':
        return fwd ? this.run(f.b, this.run(f.a, x, fwd, me), fwd, me) : this.run(f.a, this.run(f.b, x, fwd, me), fwd, me)
      case 'fix':
        return this.run(f.b, x, fwd, p)
      case 'map':
        if (x.tag === 'stem') return S(this.run(f.a, x.c, fwd, me), x.id)
        if (x.tag === 'fork') return F(this.run(f.b, x.l, fwd, me), this.run(f.c, x.r, fwd, me), x.id)
        return x
      case 'tri': {
        if (x.tag !== 'fork') return x
        const s = x.l
        if (s.tag === 'leaf') return F(s, this.run(f.a, x.r, fwd, me), x.id)
        if (s.tag === 'stem') {
          const o = this.run(f.b, F(s.c, x.r, x.id), fwd, me)
          if (o.tag !== 'fork') throw new Stuck()
          return F(S(o.l, s.id), o.r, o.id)
        }
        const o = this.run(f.c, F(s.l, F(s.r, x.r, s.id), x.id), fwd, me)
        if (o.tag !== 'fork' || o.r.tag !== 'fork') throw new Stuck()
        return F(F(o.l, o.r.l, o.r.id), o.r.r, o.id)
      }
    }
  }
}

/** p run on x, or null where it gets stuck; throws TooLong past the limit. */
export function run(p: V, x: V, fwd = true, limit?: number): V | null {
  try {
    return new Runner(limit).run(p, x, fwd, null)
  } catch (e) {
    if (e instanceof Stuck) return null
    throw e
  }
}
