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
/// S(a, b) made by removing a binder, while a or b is still being removed, or about to shorten
export const SPLIT = 8
/// the S, K or I bracket abstraction made, kept apart from a literal tree of the same shape: a = that tree, b = 0, 1 or 2
export const ATOM = 9

export type Id = number

/// The five rules. 0 = this application can't fire (yet).
export const R_NONE = 0
export const R_LEAF = 1
export const R_STEM = 2
export const R_K = 3
export const R_S = 4
export const R_F = 5
/// a binder ready to come off (its body has none left), or an S about to shorten
export const R_BIND = 6

export const RULE_GLYPH = ['', '△', 'f', 'K', 'S', 'F', 'λ']
export const RULE_TEXT = [
  '',
  'build a stem: t(x)',
  'build a fork: t(a, x)',
  'K: t(t, b, x) = b',
  'S: t(t(c), b, x) = c(x, b(x))',
  'F: t(t(c, d), b) looks at the shape of x',
  'a binder comes off'
]

/// One binder rewrite, named as the compiler's replay names them.
export type BindKind = 'const' | 'id' | 'eta' | 'split' | 'eta-after' | 'k-comp'

const F_APP = 1 // an application somewhere inside
const F_REDEX = 2 // an application that can fire somewhere inside
const F_STR = 4 // a non-empty cons list of printable codepoints
const F_BIND = 8 // a binder (or an S still being built from one) somewhere inside
const F_BREADY = 16 // a binder rewrite that can happen somewhere inside
const ATOMS = 'SKI'

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
  /// the definition on show, named by its own name even when an earlier definition shares its tree
  own: { id: Id; name: string } | null = null
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
    const KEEP = F_APP | F_REDEX | F_BIND | F_BREADY
    if (tag === STEM || tag === HELD_K) {
      flags = this.flags[a] & KEEP
      this.size[id] = 1 + this.size[a]
      this.depth[id] = 1 + this.depth[a]
    } else if (tag === LAM || tag === SPLIT) {
      // nothing inside an unfinished binder runs: only its own rewrites count
      const inner = tag === LAM ? this.flags[b] : this.flags[a] | this.flags[b]
      const ready = tag === LAM ? !(inner & F_BIND) : !(inner & F_BIND) && this.#shortens(a, b)
      if (ready) rule = R_BIND
      flags = F_BIND | (inner & F_APP) | (ready || inner & F_BREADY ? F_BREADY | F_REDEX : 0)
      this.size[id] = tag === LAM ? 1 + this.size[b] : 2 + this.size[a] + this.size[b]
      this.depth[id] = tag === LAM ? 1 + this.depth[b] : 1 + Math.max(1 + this.depth[a], this.depth[b])
    } else if (tag === ATOM) {
      this.size[id] = this.size[a]
      this.depth[id] = this.depth[a]
    } else if (tag === FORK || tag === APP) {
      flags = (this.flags[a] | this.flags[b]) & KEEP
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

  // the tree rules see a held K or an atom as the tree it stands for
  /// tag as a tree
  tt = (id: Id): number => (this.tag[id] === HELD_K ? FORK : this.tag[id] === ATOM ? this.tag[this.a[id]] : this.tag[id])
  /// first child as a tree
  ta = (id: Id): Id => (this.tag[id] === HELD_K ? this.leaf : this.tag[id] === ATOM ? this.a[this.a[id]] : this.a[id])
  /// second child as a tree
  tb = (id: Id): Id => (this.tag[id] === HELD_K ? this.a[id] : this.tag[id] === ATOM ? this.b[this.a[id]] : this.b[id])

  #ruleOf(f: Id, x: Id): number {
    const ft = this.tt(f)
    if (ft === LEAF) return R_LEAF
    if (ft === STEM) return R_STEM
    if (ft !== FORK) return R_NONE
    const lt = this.tt(this.ta(f))
    if (lt === LEAF) return R_K
    if (lt === STEM) return R_S
    if (lt !== FORK) return R_NONE
    const xt = this.tt(x)
    // I gives back all three shapes, so it can pass a symbol or a stuck application on unseen
    if ((f === this.i || (this.tag[f] === ATOM && this.a[f] === this.i)) && (xt === VAR || (xt === APP && !this.hasRedex(x)))) return R_F
    return xt === LEAF || xt === STEM || xt === FORK ? R_F : R_NONE
  }

  /// S(K(p), I) and S(K(p), K(q)) shorten once both sides are done, as the compiler does
  #shortens(l: Id, r: Id): boolean {
    return this.tag[l] === HELD_K && ((this.tag[r] === ATOM && this.b[r] === 2) || this.tag[r] === HELD_K)
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
  atom(op: 'S' | 'K' | 'I'): Id {
    const tree = op === 'I' ? this.i : op === 'K' ? this.stem(this.leaf) : this.fork(this.stem(this.fork(this.leaf, this.leaf)), this.leaf)
    return this.#make(ATOM, tree, ATOMS.indexOf(op))
  }
  /// S(l, r) from a binder: plain S once both sides are done, unless it shortens, which is a step of its own
  split(l: Id, r: Id): Id {
    if (!((this.flags[l] | this.flags[r]) & F_BIND) && !this.#shortens(l, r)) return this.fork(this.stem(l), r)
    return this.#make(SPLIT, l, r)
  }
  isBinder = (id: Id): boolean => (this.flags[id] & F_BIND) !== 0
  bindReady = (id: Id): boolean => (this.flags[id] & F_BREADY) !== 0

  /// The i-th child, with the paths the drawing uses: a held K and an S from a binder draw as the forks they are.
  kid(id: Id, i: number): Id {
    const t = this.tag[id]
    if (t === STEM) return this.a[id]
    if (t === LAM) return this.b[id]
    if (t === HELD_K) return i === 1 ? this.a[id] : this.leaf
    if (t === SPLIT) return i === 0 ? this.stem(this.a[id]) : this.b[id]
    return i === 0 ? this.a[id] : this.b[id]
  }
  withKid(id: Id, i: number, c: Id): Id {
    const t = this.tag[id]
    if (t === STEM) return this.stem(c)
    if (t === LAM) return this.#make(LAM, this.a[id], c)
    if (t === HELD_K) return i === 1 ? this.heldK(c) : id
    if (t === SPLIT) return i === 0 ? this.split(this.a[c], this.b[id]) : this.split(this.a[id], c)
    if (t === FORK) return i === 0 ? this.fork(c, this.b[id]) : this.fork(this.a[id], c)
    return i === 0 ? this.app(c, this.b[id]) : this.app(this.a[id], c)
  }

  // ---- bracket abstraction, one rewrite at a time --------------------------
  // The compiler's abstractName, as local rewrites on a binder whose body has
  // no binders left. The body is read back as the compiler's own terms:
  // K(b) is a held K, S(f, g) a fork of a stem, S(f) a stem of a stem.

  #freeMemo = new Map<number, Map<Id, boolean>>()
  /// whether the variable with string index x occurs in a binder-free term
  #free(id: Id, x: number): boolean {
    let memo = this.#freeMemo.get(x)
    if (!memo) this.#freeMemo.set(x, (memo = new Map()))
    const go = (n: Id): boolean => {
      const t = this.tag[n]
      if (t === VAR) return this.a[n] === x
      if (t !== STEM && t !== FORK && t !== APP && t !== HELD_K) return false
      const hit = memo.get(n)
      if (hit !== undefined) return hit
      const out = go(this.a[n]) || ((t === FORK || t === APP) && go(this.b[n]))
      memo.set(n, out)
      return out
    }
    return go(id)
  }

  /// A body that uses the variable, read as the compiler's application f(a).
  #asApp(id: Id): [Id, Id] | null {
    const t = this.tag[id]
    if (t === APP) return [this.a[id], this.b[id]]
    if (t === HELD_K) return [this.atom('K'), this.a[id]]
    if (t === FORK && this.tag[this.a[id]] === STEM) return [this.stem(this.a[id]), this.b[id]]
    if (t === STEM && this.tag[this.a[id]] === STEM) return [this.atom('S'), this.a[this.a[id]]]
    return null
  }

  #hasVar(id: Id): boolean {
    const t = this.tag[id]
    if (t === VAR) return true
    if (t === STEM || t === HELD_K) return this.#hasVar(this.a[id])
    if (t === FORK || t === APP) return this.#hasVar(this.a[id]) || this.#hasVar(this.b[id])
    return false
  }

  /// The compiler's p(q), drawn the way the replay draws it.
  #capply(p: Id, q: Id): Id {
    const t = this.tag[p]
    if (t === ATOM) return this.b[p] === 2 ? q : this.b[p] === 1 ? this.heldK(q) : this.stem(this.stem(q))
    if (t === HELD_K) return this.a[p]
    if (t === STEM && this.tag[this.a[p]] === STEM && this.#hasVar(p)) return this.fork(this.a[p], q)
    return this.app(p, q)
  }

  /// Which rewrite a ready binder (or shortening S) makes; null for anything else.
  bindKind(id: Id): BindKind | null {
    if (this.rule[id] !== R_BIND) return null
    if (this.tag[id] === SPLIT) return this.tag[this.b[id]] === ATOM ? 'eta-after' : 'k-comp'
    const x = this.a[id]
    const body = this.b[id]
    if (!this.#free(body, x)) return 'const'
    if (this.tag[body] === VAR) return 'id'
    const fa = this.#asApp(body)
    if (fa && this.tag[fa[1]] === VAR && this.a[fa[1]] === x && !this.#free(fa[0], x)) return 'eta'
    return 'split'
  }

  /// One binder rewrite at a ready binder, or the shortening of a finished S.
  bindStep(id: Id): Id {
    const kind = this.bindKind(id)
    const a = this.a[id]
    const b = this.b[id]
    switch (kind) {
      case 'eta-after':
        return this.a[a]
      case 'k-comp':
        return this.heldK(this.#capply(this.a[a], this.a[b]))
      case 'const':
        return this.heldK(b)
      case 'id':
        return this.atom('I')
      case 'eta':
        return this.#asApp(b)![0]
      case 'split': {
        const fa = this.#asApp(b)
        if (!fa) throw new Error('a binder body this view cannot read')
        return this.split(this.#make(LAM, a, fa[0]), this.#make(LAM, a, fa[1]))
      }
    }
    return id
  }

  /// What the replay's drawing applies on the spot, applied: I(x) is x, K(a, b) is a, S(f, g) is the fork it builds.
  folded(root: Id, memo = new Map<Id, Id>()): Id {
    const hit = memo.get(root)
    if (hit !== undefined) return hit
    const t = this.tag[root]
    let out = root
    if (t === APP) {
      // by the compiler's own shape of the call, as the replay reads it
      const f = this.a[root]
      const x = this.folded(this.b[root], memo)
      const ft = this.tag[f]
      const g = ft === APP && this.tag[this.a[f]] === ATOM ? this.b[this.a[f]] : -1
      if (ft === ATOM) out = this.b[f] === 2 ? x : this.b[f] === 1 ? this.heldK(x) : this.stem(this.stem(x))
      else if (ft === HELD_K || g === 1) out = this.folded(ft === HELD_K ? this.a[f] : this.b[f], memo)
      else if (g === 0) out = this.fork(this.stem(this.folded(this.b[f], memo)), x)
      else if (ft === STEM && this.tag[this.a[f]] === STEM) out = this.fork(this.stem(this.folded(this.a[this.a[f]], memo)), x)
      else out = this.app(this.folded(f, memo), x)
    } else if (t === STEM) out = this.stem(this.folded(this.a[root], memo))
    else if (t === HELD_K) out = this.heldK(this.folded(this.a[root], memo))
    else if (t === FORK) out = this.fork(this.folded(this.a[root], memo), this.folded(this.b[root], memo))
    else if (t === LAM) out = this.#make(LAM, this.a[root], this.folded(this.b[root], memo))
    else if (t === ATOM) out = this.a[root]
    memo.set(root, out)
    return out
  }

  /// Every binder removed, nothing run: the compiler's bracket abstraction of the whole term.
  removeBinders(root: Id, memo = new Map<Id, Id>()): Id {
    const hit = memo.get(root)
    if (hit !== undefined) return hit
    const t = this.tag[root]
    let out = root
    if (t === LAM) out = this.removeBinders(this.bindStep(this.#make(LAM, this.a[root], this.removeBinders(this.b[root], memo))), memo)
    else if (t === SPLIT) {
      const s = this.split(this.removeBinders(this.a[root], memo), this.removeBinders(this.b[root], memo))
      out = this.tag[s] === SPLIT ? this.bindStep(s) : s
    } else if (t === STEM) out = this.stem(this.removeBinders(this.a[root], memo))
    else if (t === HELD_K) out = this.heldK(this.removeBinders(this.a[root], memo))
    else if (t === FORK) out = this.fork(this.removeBinders(this.a[root], memo), this.removeBinders(this.b[root], memo))
    else if (t === APP) out = this.app(this.removeBinders(this.a[root], memo), this.removeBinders(this.b[root], memo))
    memo.set(root, out)
    return out
  }

  /// The same term with every held K written as the fork it really is.
  encode(root: Id, memo = new Map<Id, Id>()): Id {
    const hit = memo.get(root)
    if (hit !== undefined) return hit
    const t = this.tag[root]
    const a = this.a[root]
    const b = this.b[root]
    let out = root
    if (t === HELD_K) out = this.fork(this.leaf, this.encode(a, memo))
    else if (t === ATOM) out = a
    else if (t === SPLIT) out = this.fork(this.stem(this.encode(a, memo)), this.encode(b, memo))
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
    if (t === ATOM) return { op: ATOMS[b] as 'S' | 'K' | 'I', kids: [] }
    if (t === SPLIT) return { op: 'S', kids: [[a, '00'], [b, '1']] }
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
    // aliases share a tree; the first definition names it (a later `let x := pair_snd` shouldn't rename nat_pred)
    for (const [i, name] of g.labels) if (!this.names.has(map[i])) this.names.set(map[i], name)
    return g.roots.map((r) => (r < 0 ? -1 : map[r]))
  }

  /// The numeral / string / name this subtree displays as, if any.
  label(id: Id): { kind: LabelKind; text: string } | null {
    const nat = this.natOf[id]
    if (nat > 0) return { kind: 'nat', text: String(nat) }
    if (this.flags[id] & F_STR) return { kind: 'str', text: JSON.stringify(this.stringOf(id)) }
    const name = this.own?.id === id ? this.own.name : this.names.get(id)
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
  /// `depth` cuts anything nested deeper to `…`, so a caption stays a glance
  pretty(root: Id, budget = 80, sk = false, depth = Infinity): string {
    let left = budget
    const go = (id: Id, top: boolean, head = false, d = 0): string => {
      const t = this.tag[id]
      if (t === LEAF) return 't'
      if (t === VAR) return this.strs[this.a[id]]
      if (t === CUT) return this.names.get(id) ?? '…'
      const l = this.label(id)
      // something being called is code, even when its shape spells a number
      if (l && !(sk && head && l.kind === 'nat')) return l.text
      if (t === LAM) return `{${this.strs[this.a[id]]}} => ${go(this.b[id], false, false, d + 1)}`
      if (sk || t === HELD_K || t === ATOM || t === SPLIT) {
        const c = this.readComb(id)
        if (c) return c.kids.length === 0 ? c.op : d > depth || --left < 0 ? '…' : `${c.op}(${c.kids.map(([k]) => go(k, false, false, d + 1)).join(', ')})`
      }
      // finished data with no name would only be noise: give its size instead
      if (!top && t !== APP && !this.hasApp(id) && this.size[id] > 9) return `…${this.size[id]}`
      if (d > depth || --left < 0) return '…'
      if (t === STEM) return `t(${go(this.a[id], false, false, d + 1)})`
      if (t === FORK) return `t(${go(this.a[id], false, false, d + 1)}, ${go(this.b[id], false, false, d + 1)})`
      const args = [this.b[id]]
      let f = this.a[id]
      while (this.tag[f] === APP) {
        args.unshift(this.b[f])
        f = this.a[f]
      }
      return `${go(f, false, true, d)}(${args.map((x) => go(x, false, false, d + 1)).join(', ')})`
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
      if (t === STEM || t === HELD_K || t === ATOM) stack.push(this.a[id])
      else if (t === LAM) stack.push(this.b[id])
      else if (t === FORK || t === APP || t === SPLIT) stack.push(this.a[id], this.b[id])
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
  if (t === SPLIT) return [s.stem(s.a[id]), s.b[id]]
  if (t === FORK || t === APP) return [s.a[id], s.b[id]]
  return []
}
