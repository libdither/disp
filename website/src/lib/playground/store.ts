// A hash-consed term store for the inspector: every distinct tree or held
// application is one integer id, so equality is ===, and size, depth and "is
// there still work in here?" are O(1) facts computed when a node is made. That
// is what lets a reduction trace keep every frame without copying trees.

import { G_LEAF, G_STEM, G_FORK, G_APP, G_VAR, G_LAM, G_HELD_K, type Graph, type RawTree } from '../disp/protocol.ts'

export const LEAF = 0
export const STEM = 1
export const FORK = 2
export const APP = 3
export const VAR = 4
export const CUT = 5
/// a binder bracket abstraction hasn't removed yet: a = the name's index in strs, b = the body
export const LAM = 6
/// K(a) that stays visibly K in a bracket replay; `encode` turns it into its real fork
export const HELD_K = 7

export type Id = number

/// The five rules. 0 = this application can't fire (yet).
export const R_NONE = 0
export const R_LEAF = 1
export const R_STEM = 2
export const R_K = 3
export const R_S = 4
export const R_F = 5

export const RULE_GLYPH = ['', '△', 'f', 'K', 'S', 'F']
export const RULE_TEXT = [
  '',
  'build a stem: t(x)',
  'build a fork: t(a, x)',
  'K: t(t, b, x) = b',
  'S: t(t(c), b, x) = c(x, b(x))',
  'F: t(t(c, d), b) looks at the shape of x'
]

const F_APP = 1 // an application somewhere inside
const F_REDEX = 2 // an application that can fire somewhere inside
const F_STR = 4 // a non-empty cons list of printable codepoints

export type LabelKind = 'nat' | 'str' | 'name'

export class Store {
  tag = new Uint8Array(1 << 12)
  a = new Int32Array(1 << 12)
  b = new Int32Array(1 << 12)
  flags = new Uint8Array(1 << 12)
  rule = new Uint8Array(1 << 12)
  size = new Float64Array(1 << 12)
  depth = new Int32Array(1 << 12)
  natOf = new Int32Array(1 << 12)
  n = 0
  #table = new Int32Array(1 << 13).fill(-1)
  #mask = (1 << 13) - 1
  strs: string[] = []
  names = new Map<Id, string>()
  /// a cut node, once its structure has been fetched
  resolved = new Map<Id, Id>()
  readonly leaf: Id
  readonly i: Id

  constructor() {
    this.leaf = this.#make(LEAF, 0, 0)
    this.i = this.fork(this.fork(this.leaf, this.leaf), this.leaf)
  }

  #grow() {
    const cap = this.tag.length * 2
    const up = <T extends { set(x: T): void }>(old: T, mk: (n: number) => T): T => {
      const next = mk(cap)
      next.set(old)
      return next
    }
    this.tag = up(this.tag, (n) => new Uint8Array(n))
    this.a = up(this.a, (n) => new Int32Array(n))
    this.b = up(this.b, (n) => new Int32Array(n))
    this.flags = up(this.flags, (n) => new Uint8Array(n))
    this.rule = up(this.rule, (n) => new Uint8Array(n))
    this.size = up(this.size, (n) => new Float64Array(n))
    this.depth = up(this.depth, (n) => new Int32Array(n))
    this.natOf = up(this.natOf, (n) => new Int32Array(n))
    const table = new Int32Array(cap * 2).fill(-1)
    const mask = cap * 2 - 1
    for (let i = 0; i < this.n; i++) {
      let h = hash(this.tag[i], this.a[i], this.b[i]) & mask
      while (table[h] !== -1) h = (h + 1) & mask
      table[h] = i
    }
    this.#table = table
    this.#mask = mask
  }

  #make(tag: number, a: number, b: number): Id {
    let h = hash(tag, a, b) & this.#mask
    for (;;) {
      const i = this.#table[h]
      if (i === -1) break
      if (this.tag[i] === tag && this.a[i] === a && this.b[i] === b) return i
      h = (h + 1) & this.#mask
    }
    if (this.n >= this.tag.length) {
      this.#grow()
      h = hash(tag, a, b) & this.#mask
      while (this.#table[h] !== -1) h = (h + 1) & this.#mask
    }
    const id = this.n++
    this.#table[h] = id
    this.tag[id] = tag
    this.a[id] = a
    this.b[id] = b
    this.natOf[id] = -1
    let flags = 0
    let rule = R_NONE
    if (tag === STEM || tag === HELD_K) {
      flags = this.flags[a] & (F_APP | F_REDEX)
      this.size[id] = 1 + this.size[a]
      this.depth[id] = 1 + this.depth[a]
    } else if (tag === LAM) {
      flags = this.flags[b] & (F_APP | F_REDEX)
      this.size[id] = 1 + this.size[b]
      this.depth[id] = 1 + this.depth[b]
    } else if (tag === FORK || tag === APP) {
      flags = (this.flags[a] | this.flags[b]) & (F_APP | F_REDEX)
      this.size[id] = 1 + this.size[a] + this.size[b]
      this.depth[id] = 1 + Math.max(this.depth[a], this.depth[b])
      if (tag === APP) {
        flags |= F_APP
        rule = this.#ruleOf(a, b)
        if (rule !== R_NONE) flags |= F_REDEX
      } else if (this.tag[a] === LEAF && this.natOf[b] >= 0) {
        this.natOf[id] = this.natOf[b] + 1
      }
      if (tag === FORK && this.natOf[a] >= 32 && (this.tag[b] === LEAF || this.flags[b] & F_STR)) flags |= F_STR
    } else {
      this.size[id] = 1
      if (tag === LEAF) this.natOf[id] = 0
    }
    this.flags[id] = flags
    this.rule[id] = rule
    return id
  }

  #ruleOf(f: Id, x: Id): number {
    const ft = this.tag[f]
    if (ft === LEAF) return R_LEAF
    if (ft === STEM) return R_STEM
    if (ft !== FORK) return R_NONE
    const l = this.a[f]
    const lt = this.tag[l]
    if (lt === LEAF) return R_K
    if (lt === STEM) return R_S
    if (lt !== FORK) return R_NONE
    const xt = this.tag[x]
    // I gives back all three shapes, so it can pass a symbol or a stuck application on unseen
    if (f === this.i && (xt === VAR || (xt === APP && !this.hasRedex(x)))) return R_F
    return xt === LEAF || xt === STEM || xt === FORK ? R_F : R_NONE
  }

  stem = (c: Id): Id => this.#make(STEM, c, 0)
  fork = (l: Id, r: Id): Id => this.#make(FORK, l, r)
  app = (f: Id, x: Id): Id => this.#make(APP, f, x)
  variable(name: string): Id {
    let i = this.strs.indexOf(name)
    if (i < 0) i = this.strs.push(name) - 1
    return this.#make(VAR, i, 0)
  }
  cut = (handle: number): Id => this.#make(CUT, handle, 0)
  lam(name: string, body: Id): Id {
    let i = this.strs.indexOf(name)
    if (i < 0) i = this.strs.push(name) - 1
    return this.#make(LAM, i, body)
  }
  heldK = (x: Id): Id => this.#make(HELD_K, x, 0)

  /// The same term with every held K written as the fork it really is.
  encode(root: Id, memo = new Map<Id, Id>()): Id {
    const hit = memo.get(root)
    if (hit !== undefined) return hit
    const t = this.tag[root]
    const a = this.a[root]
    const b = this.b[root]
    let out = root
    if (t === HELD_K) out = this.fork(this.leaf, this.encode(a, memo))
    else if (t === LAM) out = this.#make(LAM, a, this.encode(b, memo))
    else if (t === STEM) out = this.stem(this.encode(a, memo))
    else if (t === FORK) out = this.fork(this.encode(a, memo), this.encode(b, memo))
    else if (t === APP) out = this.app(this.encode(a, memo), this.encode(b, memo))
    memo.set(root, out)
    return out
  }

  /// Read a node as the combinator its shape encodes, with each operand's id and path below the node.
  readComb(id: Id): { op: 'S' | 'K' | 'I' | 'F'; kids: [Id, string][] } | null {
    const t = this.tag[id]
    const a = this.a[id]
    const b = this.b[id]
    if (t === HELD_K) return { op: 'K', kids: [[a, '1']] }
    if (t === STEM) {
      if (a === this.leaf) return { op: 'K', kids: [] }
      return this.tag[a] === STEM ? { op: 'S', kids: [[this.a[a], '00']] } : null
    }
    if (t !== FORK) return null
    if (id === this.i) return { op: 'I', kids: [] }
    if (id === this.fork(this.stem(this.fork(this.leaf, this.leaf)), this.leaf)) return { op: 'S', kids: [] }
    const lt = this.tag[a]
    if (lt === LEAF) return { op: 'K', kids: [[b, '1']] }
    if (lt === STEM) return { op: 'S', kids: [[this.a[a], '00'], [b, '1']] }
    if (lt === FORK) return { op: 'F', kids: [[this.a[a], '00'], [this.b[a], '01'], [b, '1']] }
    return null
  }

  hasApp = (id: Id): boolean => (this.flags[id] & F_APP) !== 0
  hasRedex = (id: Id): boolean => (this.flags[id] & F_REDEX) !== 0

  /// Load a worker dump; returns the store id of each root (-1 stays -1).
  load(g: Graph): Id[] {
    const n = g.nodes.length / 3
    const map = new Int32Array(n)
    for (let i = 0; i < n; i++) {
      const t = g.nodes[i * 3]
      const a = g.nodes[i * 3 + 1]
      const b = g.nodes[i * 3 + 2]
      if (t === G_LEAF) map[i] = this.leaf
      else if (t === G_STEM) map[i] = this.stem(map[a])
      else if (t === G_FORK) map[i] = this.fork(map[a], map[b])
      else if (t === G_APP) map[i] = this.app(map[a], map[b])
      else if (t === G_VAR) map[i] = this.variable(g.strs[a])
      else if (t === G_LAM) map[i] = this.lam(g.strs[a], map[b])
      else if (t === G_HELD_K) map[i] = this.heldK(map[a])
      else map[i] = this.cut(a)
    }
    for (const [i, name] of g.labels) this.names.set(map[i], name)
    return g.roots.map((r) => (r < 0 ? -1 : map[r]))
  }

  /// The numeral / string / name this subtree displays as, if any.
  label(id: Id): { kind: LabelKind; text: string } | null {
    const nat = this.natOf[id]
    if (nat > 0) return { kind: 'nat', text: String(nat) }
    if (this.flags[id] & F_STR) return { kind: 'str', text: JSON.stringify(this.stringOf(id)) }
    const name = this.names.get(id)
    return name ? { kind: 'name', text: name } : null
  }

  stringOf(id: Id): string {
    let out = ''
    let cur = id
    while (this.tag[cur] === FORK && out.length < 200) {
      out += String.fromCodePoint(this.natOf[this.a[cur]])
      cur = this.b[cur]
    }
    return this.tag[cur] === FORK ? out + '…' : out
  }

  /// A short term rendering: `t(a, b)` for trees, `f(x)` for held applications, cut off past `budget` nodes.
  pretty(root: Id, budget = 80, sk = false): string {
    let left = budget
    const go = (id: Id, top: boolean, head = false): string => {
      const t = this.tag[id]
      if (t === LEAF) return 't'
      if (t === VAR) return this.strs[this.a[id]]
      if (t === CUT) return this.names.get(id) ?? '…'
      const l = this.label(id)
      // something being called is code, even when its shape spells a number
      if (l && !(sk && head && l.kind === 'nat')) return l.text
      if (t === LAM) return `{${this.strs[this.a[id]]}} => ${go(this.b[id], false)}`
      if (sk || t === HELD_K) {
        const c = this.readComb(id)
        if (c) return c.kids.length === 0 ? c.op : --left < 0 ? '…' : `${c.op}(${c.kids.map(([k]) => go(k, false)).join(', ')})`
      }
      // finished data with no name would only be noise: give its size instead
      if (!top && t !== APP && !this.hasApp(id) && this.size[id] > 9) return `…${this.size[id]}`
      if (--left < 0) return '…'
      if (t === STEM) return `t(${go(this.a[id], false)})`
      if (t === FORK) return `t(${go(this.a[id], false)}, ${go(this.b[id], false)})`
      const args = [this.b[id]]
      let f = this.a[id]
      while (this.tag[f] === APP) {
        args.unshift(this.b[f])
        f = this.a[f]
      }
      return `${go(f, false, true)}(${args.map((x) => go(x, false)).join(', ')})`
    }
    return go(root, true)
  }

  /// How many distinct subtrees sit under root (the hash-consed size), capped.
  distinct(root: Id, cap = 200000): number {
    const seen = new Set<Id>()
    const stack = [root]
    while (stack.length > 0 && seen.size < cap) {
      const id = stack.pop()!
      if (seen.has(id)) continue
      seen.add(id)
      const t = this.tag[id]
      if (t === STEM || t === HELD_K) stack.push(this.a[id])
      else if (t === LAM) stack.push(this.b[id])
      else if (t === FORK || t === APP) stack.push(this.a[id], this.b[id])
    }
    return seen.size
  }

  fromRaw(r: RawTree): Id {
    if (r === 0) return this.leaf
    return r.length === 1 ? this.stem(this.fromRaw(r[0])) : this.fork(this.fromRaw(r[0]), this.fromRaw(r[1]))
  }
}

function hash(t: number, a: number, b: number): number {
  let h = Math.imul(t + 1, 0x9e3779b1) ^ Math.imul(a + 1, 0x85ebca6b)
  h = Math.imul(h ^ (h >>> 15), 0xc2b2ae35) ^ Math.imul(b + 1, 0x27d4eb2f)
  return (h ^ (h >>> 13)) >>> 0
}

/// Child ids of a node in draw order.
export function kids(s: Store, id: Id): Id[] {
  const t = s.tag[id]
  if (t === STEM || t === HELD_K) return [s.a[id]]
  if (t === LAM) return [s.b[id]]
  if (t === FORK || t === APP) return [s.a[id], s.b[id]]
  return []
}
