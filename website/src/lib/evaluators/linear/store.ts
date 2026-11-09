// The term store of the nine-rule linear tree calculus, cut down from the playground's: every distinct tree,
// held application or binder is one integer id, so equality is === and size, depth and "is there still work
// in here?" are O(1) facts computed when a node is made. That is what lets a trace keep every frame.
//
// A function is a fork △ a b; applied to x, its left child a and one more node pick the rule:
//   a = △    : the right child b picks I, K or C
//   a = △p   : p picks W, B or B′
//   a = △cd  : x picks one of triage's three cases, and the chosen arm gets the two arms it beat
// Every rule eats the three nodes it reads; only K drops something and only W copies something.

export const LEAF = 0
export const STEM = 1
export const FORK = 2
export const APP = 3
export const VAR = 4
/// a binder bracket abstraction hasn't removed yet: a = the name's index in strs, b = the body
export const LAM = 5
/// B(p, q) made by removing a binder while q still has binders, or about to merge into B′: draws as △(△(△p)) q
export const BUILD = 6

export type Id = number

export const R_NONE = 0
export const R_LEAF = 1
export const R_STEM = 2
export const R_I = 3
export const R_K = 4
export const R_C = 5
export const R_W = 6
export const R_B = 7
export const R_B2 = 8
export const R_T0 = 9
export const R_T1 = 10
export const R_T2 = 11
/// a binder ready to come off (its body has none left), or a B about to merge into B′
export const R_BIND = 12

export const RULE_GLYPH = ['', '△', 'f', 'I', 'K', 'C', 'W', 'B', 'B′', 'T', 'T', 'T', 'λ']
export const RULE_NAME = ['', 'build', 'build', 'I', 'K', 'C', 'W', 'B', 'B′', 'triage', 'triage', 'triage', 'binder']
export const RULE_TEXT = [
  '',
  'build a stem: △ x',
  'build a fork: △ a x',
  'I: △ △ △ x → x',
  'K: △ △ (△ q) x → q, and x is dropped',
  'C: △ △ (△ q r) x → q x r: x goes left',
  'W: △ (△ △) b x → b x x: x is copied',
  'B: △ (△ (△ f)) b x → f (b x): x goes right',
  'B′: △ (△ (△ f g)) b x → f (g (b x)): x goes right, twice',
  'triage on a leaf: △ (△ c d) b △ → c d b',
  'triage on a stem: △ (△ c d) b (△ u) → d c b u',
  'triage on a fork: △ (△ c d) b (△ u v) → b c d u v'
]
/// what each rule does with its argument, for the colours
export type RoleKind = 'pass' | 'drop' | 'left' | 'copy' | 'right' | 'look' | 'build' | 'bind'
export const RULE_ROLE: RoleKind[] = ['build', 'build', 'build', 'pass', 'drop', 'left', 'copy', 'right', 'right', 'look', 'look', 'look', 'bind']

/// One binder rewrite, named by the case of bracket abstraction it is.
export type BindKind = 'id' | 'const' | 'eta' | 'right' | 'left' | 'both' | 'chain'
export const BIND_TEXT: Record<BindKind, string> = {
  id: 'the binder of λx. x becomes I',
  const: 'x is never used: λx. M becomes K M',
  eta: 'λx. M x becomes M',
  right: 'x only on the right: λx. M N becomes B M (λx. N)',
  left: 'x only on the left: λx. M N becomes C (λx. M) N',
  both: 'x on both sides: λx. M N becomes W (λx. λx′. M N′)',
  chain: 'two right moves in a row merge: B M (B g r) becomes B′ M g r'
}

const F_APP = 1 // an application somewhere inside
const F_REDEX = 2 // an application or binder that can fire somewhere inside
const F_BIND = 4 // a binder (or a B still being built from one) somewhere inside
const F_BREADY = 8 // a binder rewrite that can happen somewhere inside

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
  /// kept for the drawing's shared code: a folded part fetched later (never used here)
  resolved = new Map<Id, Id>()
  readonly leaf: Id
  readonly i: Id

  constructor() {
    this.leaf = this.#make(LEAF, 0, 0)
    this.i = this.fork(this.leaf, this.leaf)
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
    if (tag === STEM) {
      flags = this.flags[a] & KEEP
      this.size[id] = 1 + this.size[a]
      this.depth[id] = 1 + this.depth[a]
      if (this.natOf[a] >= 0) this.natOf[id] = this.natOf[a] + 1
    } else if (tag === LAM || tag === BUILD) {
      // nothing inside an unfinished binder runs: only its own rewrites count
      const inner = tag === LAM ? this.flags[b] : this.flags[b]
      const ready = !(inner & F_BIND)
      if (ready) rule = R_BIND
      flags = F_BIND | (inner & F_APP) | (ready || inner & F_BREADY ? F_BREADY | F_REDEX : 0)
      // drawn as the binder pill over its body, or as the B tree △(△(△p)) q
      this.size[id] = tag === LAM ? 1 + this.size[b] : 3 + this.size[a] + this.size[b]
      this.depth[id] = tag === LAM ? 1 + this.depth[b] : Math.max(3 + this.depth[a], 1 + this.depth[b])
    } else if (tag === FORK || tag === APP) {
      flags = (this.flags[a] | this.flags[b]) & KEEP
      this.size[id] = 1 + this.size[a] + this.size[b]
      this.depth[id] = 1 + Math.max(this.depth[a], this.depth[b])
      if (tag === APP) {
        flags |= F_APP
        rule = this.#ruleOf(a, b)
        if (rule !== R_NONE) flags |= F_REDEX
      }
    } else {
      this.size[id] = 1
      if (tag === LEAF) this.natOf[id] = 0
    }
    this.flags[id] = flags
    this.rule[id] = rule
    return id
  }

  /// Which rule an application f(x) fires, reading only the nodes the rule reads; 0 while one of them isn't a tree yet.
  #ruleOf(f: Id, x: Id): number {
    const ft = this.tag[f]
    if (ft === LEAF) return R_LEAF
    if (ft === STEM) return R_STEM
    if (ft !== FORK) return R_NONE
    const a = this.a[f]
    const at = this.tag[a]
    if (at === LEAF) {
      const bt = this.tag[this.b[f]]
      return bt === LEAF ? R_I : bt === STEM ? R_K : bt === FORK ? R_C : R_NONE
    }
    if (at === STEM) {
      const pt = this.tag[this.a[a]]
      return pt === LEAF ? R_W : pt === STEM ? R_B : pt === FORK ? R_B2 : R_NONE
    }
    if (at !== FORK) return R_NONE
    const xt = this.tag[x]
    return xt === LEAF ? R_T0 : xt === STEM ? R_T1 : xt === FORK ? R_T2 : R_NONE
  }

  stem = (c: Id): Id => this.#make(STEM, c, 0)
  fork = (l: Id, r: Id): Id => this.#make(FORK, l, r)
  app = (f: Id, x: Id): Id => this.#make(APP, f, x)
  variable(name: string): Id {
    return this.#make(VAR, this.#name(name), 0)
  }
  lam(name: string, body: Id): Id {
    return this.#make(LAM, this.#name(name), body)
  }
  #name(name: string): number {
    let i = this.strs.indexOf(name)
    if (i < 0) i = this.strs.push(name) - 1
    return i
  }
  nat(n: number): Id {
    let t = this.leaf
    for (let k = 0; k < n; k++) t = this.stem(t)
    return t
  }

  // the combinators as the trees they are
  get I(): Id {
    return this.i
  }
  K = (q: Id): Id => this.fork(this.leaf, this.stem(q))
  C = (q: Id, r: Id): Id => this.fork(this.leaf, this.fork(q, r))
  W = (b: Id): Id => this.fork(this.stem(this.leaf), b)
  B = (f: Id, b: Id): Id => this.fork(this.stem(this.stem(f)), b)
  B2 = (f: Id, g: Id, b: Id): Id => this.fork(this.stem(this.fork(f, g)), b)
  /// the B tree △(△(△g)) r, read back: [g, r]
  #asB(q: Id): [Id, Id] | null {
    if (this.tag[q] !== FORK) return null
    const s = this.a[q]
    if (this.tag[s] !== STEM || this.tag[this.a[s]] !== STEM) return null
    return [this.a[this.a[s]], this.b[q]]
  }
  /// B(p, q) from a binder: the plain B tree once q is done, unless q is a B too, which merges as a step of its own
  build(p: Id, q: Id): Id {
    if (!(this.flags[q] & F_BIND) && !this.#asB(q)) return this.B(p, q)
    return this.#make(BUILD, p, q)
  }

  isBinder = (id: Id): boolean => (this.flags[id] & F_BIND) !== 0
  bindReady = (id: Id): boolean => (this.flags[id] & F_BREADY) !== 0
  hasApp = (id: Id): boolean => (this.flags[id] & F_APP) !== 0
  hasRedex = (id: Id): boolean => (this.flags[id] & F_REDEX) !== 0

  /// The i-th child, with the paths the drawing uses: a B being built draws as the B tree it is.
  kid(id: Id, i: number): Id {
    const t = this.tag[id]
    if (t === STEM) return this.a[id]
    if (t === LAM) return this.b[id]
    if (t === BUILD) return i === 0 ? this.stem(this.stem(this.a[id])) : this.b[id]
    return i === 0 ? this.a[id] : this.b[id]
  }
  withKid(id: Id, i: number, c: Id): Id {
    const t = this.tag[id]
    if (t === STEM) return this.stem(c)
    if (t === LAM) return this.#make(LAM, this.a[id], c)
    if (t === BUILD) return i === 0 ? this.build(this.a[this.a[c]], this.b[id]) : this.build(this.a[id], c)
    if (t === FORK) return i === 0 ? this.fork(c, this.b[id]) : this.fork(this.a[id], c)
    return i === 0 ? this.app(c, this.b[id]) : this.app(this.a[id], c)
  }

  // ---- bracket abstraction, one rewrite at a time --------------------------
  // The compiler of the nine-rule calculus as local rewrites on a binder whose body has no binders left:
  // x itself is I, an unused x is K, x on the right only is B (merging into B′ when that side is a B),
  // x on the left only is C, x on both sides is one W. A tree △ a b in a body reads as the application (△ a) b.

  #free(id: Id, x: number): boolean {
    const t = this.tag[id]
    if (t === VAR) return this.a[id] === x
    if (t === STEM) return this.#free(this.a[id], x)
    if (t === FORK || t === APP) return this.#free(this.a[id], x) || this.#free(this.b[id], x)
    return false
  }

  /// A body read as an application f(a): a tree △ a b is (△ a) b, a stem △ a is △ applied to a.
  #asApp(id: Id): [Id, Id] | null {
    const t = this.tag[id]
    if (t === APP) return [this.a[id], this.b[id]]
    if (t === FORK) return [this.stem(this.a[id]), this.b[id]]
    if (t === STEM) return [this.leaf, this.a[id]]
    return null
  }

  #rename(id: Id, x: number, y: Id): Id {
    const t = this.tag[id]
    if (t === VAR) return this.a[id] === x ? y : id
    if (t === STEM) return this.stem(this.#rename(this.a[id], x, y))
    if (t === FORK) return this.fork(this.#rename(this.a[id], x, y), this.#rename(this.b[id], x, y))
    if (t === APP) return this.app(this.#rename(this.a[id], x, y), this.#rename(this.b[id], x, y))
    return id
  }

  /// Which rewrite a ready binder (or a B about to merge) makes; null for anything else.
  bindKind(id: Id): BindKind | null {
    if (this.rule[id] !== R_BIND) return null
    if (this.tag[id] === BUILD) return 'chain'
    const x = this.a[id]
    const body = this.b[id]
    if (!this.#free(body, x)) return 'const'
    if (this.tag[body] === VAR) return 'id'
    const [p, q] = this.#asApp(body)!
    const inP = this.#free(p, x)
    const inQ = this.#free(q, x)
    if (!inP && this.tag[q] === VAR) return 'eta'
    if (!inP) return 'right'
    if (!inQ) return 'left'
    return 'both'
  }

  /// One binder rewrite at a ready binder, or the merge of a finished B into B′.
  bindStep(id: Id): Id {
    const kind = this.bindKind(id)
    if (kind === 'chain') {
      const [g, r] = this.#asB(this.b[id])!
      return this.B2(this.a[id], g, r)
    }
    const x = this.a[id]
    const body = this.b[id]
    const name = this.strs[x]
    switch (kind) {
      case 'const':
        return this.K(body)
      case 'id':
        return this.I
      case 'eta':
        return this.#asApp(body)![0]
      case 'right': {
        const [p, q] = this.#asApp(body)!
        return this.build(p, this.#make(LAM, x, q))
      }
      case 'left': {
        const [p, q] = this.#asApp(body)!
        return this.C(this.#make(LAM, x, p), q)
      }
      case 'both': {
        const [p, q] = this.#asApp(body)!
        let fresh = name + '′'
        while (this.#free(body, this.#name(fresh))) fresh += '′'
        const y = this.variable(fresh)
        return this.W(this.#make(LAM, x, this.lam(fresh, this.app(p, this.#rename(q, x, y)))))
      }
    }
    return id
  }

  /// Every binder removed, nothing run: the bracket abstraction of the whole term.
  removeBinders(root: Id, memo = new Map<Id, Id>()): Id {
    const hit = memo.get(root)
    if (hit !== undefined) return hit
    const t = this.tag[root]
    let out = root
    if (t === LAM || t === BUILD) {
      const inner = t === LAM ? this.#make(LAM, this.a[root], this.removeBinders(this.b[root], memo)) : this.build(this.a[root], this.removeBinders(this.b[root], memo))
      out = this.rule[inner] === R_BIND ? this.removeBinders(this.bindStep(inner), memo) : inner
    } else if (t === STEM) out = this.stem(this.removeBinders(this.a[root], memo))
    else if (t === FORK) out = this.fork(this.removeBinders(this.a[root], memo), this.removeBinders(this.b[root], memo))
    else if (t === APP) out = this.app(this.removeBinders(this.a[root], memo), this.removeBinders(this.b[root], memo))
    memo.set(root, out)
    return out
  }

  /// Read a node as the combinator its shape is, with each operand's id and path below the node.
  readComb(id: Id): { op: string; kids: [Id, string][] } | null {
    const t = this.tag[id]
    if (t === BUILD) return { op: 'B', kids: [[this.a[id], '000'], [this.b[id], '1']] }
    if (t !== FORK) return null
    const a = this.a[id]
    const b = this.b[id]
    const at = this.tag[a]
    if (at === LEAF) {
      const bt = this.tag[b]
      if (bt === LEAF) return { op: 'I', kids: [] }
      if (bt === STEM) return { op: 'K', kids: [[this.a[b], '10']] }
      if (bt === FORK) return { op: 'C', kids: [[this.a[b], '10'], [this.b[b], '11']] }
      return null
    }
    if (at === STEM) {
      const p = this.a[a]
      const pt = this.tag[p]
      if (pt === LEAF) return { op: 'W', kids: [[b, '1']] }
      if (pt === STEM) return { op: 'B', kids: [[this.a[p], '000'], [b, '1']] }
      if (pt === FORK) return { op: 'B′', kids: [[this.a[p], '000'], [this.b[p], '001'], [b, '1']] }
      return null
    }
    if (at === FORK) return { op: 'T', kids: [[this.a[a], '00'], [this.b[a], '01'], [b, '1']] }
    return null
  }

  #parts = new Map<Id, string>()
  #partsFor = -1
  /// The definition a subtree is a piece of (the smallest one holding it), to name a fold that has no name of its own.
  partOf(id: Id): string | undefined {
    if (this.#partsFor !== this.names.size) {
      this.#partsFor = this.names.size
      this.#parts = new Map()
      const defs = [...this.names].sort((a, b) => this.size[a[0]] - this.size[b[0]])
      let budget = 200_000
      for (const [root, name] of defs) {
        const stack = [root]
        while (stack.length && budget-- > 0) {
          const id = stack.pop()!
          const t = this.tag[id]
          const kids = t === FORK || t === APP || t === BUILD ? [this.a[id], this.b[id]] : t === STEM ? [this.a[id]] : t === LAM ? [this.b[id]] : []
          for (const c of kids)
            if (!this.#parts.has(c) && !this.names.has(c) && this.size[c] > 1) {
              this.#parts.set(c, name)
              stack.push(c)
            }
        }
      }
    }
    return this.#parts.get(id)
  }

  #shown = new Map<Id, number>()
  #shownFor = -1
  /// How many nodes a subtree draws as below its top, with each named part and numeral one node.
  shownSize(id: Id): number {
    if (this.#shownFor !== this.names.size) {
      this.#shownFor = this.names.size
      this.#shown = new Map()
    }
    const memo = this.#shown
    const kidsOf = (n: Id): Id[] => {
      const t = this.tag[n]
      return t === FORK || t === APP || t === BUILD ? [this.a[n], this.b[n]] : t === STEM ? [this.a[n]] : t === LAM ? [this.b[n]] : []
    }
    const stack = [id]
    while (stack.length) {
      const n = stack[stack.length - 1]
      if (memo.has(n)) {
        stack.pop()
        continue
      }
      const todo = kidsOf(n).filter((k) => !memo.has(k) && !this.label(k))
      if (todo.length) {
        stack.push(...todo)
        continue
      }
      stack.pop()
      memo.set(n, 1 + kidsOf(n).reduce((sum, k) => sum + (this.label(k) ? 1 : memo.get(k)!), 0))
    }
    return memo.get(id)!
  }

  /// The numeral or name this subtree displays as, if any (a numeral is stems on a leaf).
  label(id: Id): { kind: LabelKind; text: string } | null {
    const name = this.names.get(id)
    if (name) return { kind: 'name', text: name }
    const nat = this.natOf[id]
    if (nat > 1) return { kind: 'nat', text: String(nat) }
    return null
  }

  /// A short term rendering: △ for the leaf, `△(a, b)` for trees, `f(x)` for applications, `{x} => b` for binders.
  pretty(root: Id, budget = 80, sk = true, depth = Infinity): string {
    let left = budget
    const go = (id: Id, top: boolean, d = 0): string => {
      const t = this.tag[id]
      if (t === LEAF) return '△'
      if (t === VAR) return this.strs[this.a[id]]
      const l = this.label(id)
      if (l && !top) return l.text
      if (t === LAM) return `{${this.strs[this.a[id]]}} => ${go(this.b[id], false, d + 1)}`
      if (sk) {
        const c = this.readComb(id)
        if (c) return c.kids.length === 0 ? c.op : d > depth || --left < 0 ? '…' : `${c.op}(${c.kids.map(([k]) => go(k, false, d + 1)).join(', ')})`
      }
      if (!top && t !== APP && !this.hasApp(id) && this.shownSize(id) > 9) return `…${this.size[id]}`
      if (d > depth || --left < 0) return '…'
      if (t === STEM) return `△(${go(this.a[id], false, d + 1)})`
      if (t === FORK) return `△(${go(this.a[id], false, d + 1)}, ${go(this.b[id], false, d + 1)})`
      if (t === BUILD) return `B(${go(this.a[id], false, d + 1)}, ${go(this.b[id], false, d + 1)})`
      const args = [this.b[id]]
      let f = this.a[id]
      while (this.tag[f] === APP) {
        args.unshift(this.b[f])
        f = this.a[f]
      }
      return `${go(f, false, d)}(${args.map((x) => go(x, false, d + 1)).join(', ')})`
    }
    return go(root, true)
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
  if (t === STEM) return [s.a[id]]
  if (t === LAM) return [s.b[id]]
  if (t === BUILD) return [s.stem(s.stem(s.a[id])), s.b[id]]
  if (t === FORK || t === APP) return [s.a[id], s.b[id]]
  return []
}
