// Two-way patterns: a reversible calculus on trees, read the way tree calculus reads them. A program is
// a tree, and its five shapes are tree calculus's five rules, each made reversible: the two that only
// build (△ and △a) already are; K, S and triage, which lose their input, become dagger, composition
// and a clause. A clause's two sides are patterns, which read the same five shapes again, and matching
// a pattern is building it backward, so a clause runs both ways.
//
// apply(f, x) = (r, y): the value y, and the residual r, the program that undoes this run: f† = (△, f),
// one fork on top. So apply(r, y) = (f, x). steps.ts runs a program one call at a time.

export type V =
  | { tag: 'leaf'; id: number }
  | { tag: 'stem'; c: V; id: number }
  | { tag: 'fork'; l: V; r: V; id: number }

let nextId = 1
const fresh = () => nextId++
export const L = (): V => ({ tag: 'leaf', id: fresh() })
export const S = (c: V): V => ({ tag: 'stem', c, id: fresh() })
export const F = (l: V, r: V): V => ({ tag: 'fork', l, r, id: fresh() })

export const kids = (v: V): V[] => (v.tag === 'stem' ? [v.c] : v.tag === 'fork' ? [v.l, v.r] : [])
const SIZES = new WeakMap<V, number>()
/** Nodes in v (trees never change, so it is remembered). */
export function size(v: V): number {
  let n = SIZES.get(v)
  if (n === undefined) SIZES.set(v, (n = 1 + kids(v).reduce((s, c) => s + size(c), 0)))
  return n
}
export const depth = (v: V): number => 1 + Math.max(0, ...kids(v).map(depth))
export function eq(a: V, b: V): boolean {
  if (a.tag !== b.tag) return false
  if (a.tag === 'stem') return eq(a.c, (b as typeof a).c)
  if (a.tag === 'fork') return eq(a.l, (b as typeof a).l) && eq(a.r, (b as typeof a).r)
  return true
}
/** The tree as a string, for looking it up. */
export const key = (v: V): string =>
  v.tag === 'leaf' ? '.' : v.tag === 'stem' ? 's' + key(v.c) : `(${key(v.l)}${key(v.r)})`
/** The same tree with new ids. */
export const copy = (v: V): V =>
  v.tag === 'leaf' ? L() : v.tag === 'stem' ? S(copy(v.c)) : F(copy(v.l), copy(v.r))
/** △ a leaf, △v a stem, (u, v) a fork. */
export const show = (v: V): string =>
  v.tag === 'leaf' ? '△' : v.tag === 'stem' ? '△' + show(v.c) : `(${show(v.l)}, ${show(v.r)})`
export function at(v: V, path: string): V | null {
  let cur: V = v
  for (const d of path) {
    if (d === 'c' && cur.tag === 'stem') cur = cur.c
    else if (d === 'l' && cur.tag === 'fork') cur = cur.l
    else if (d === 'r' && cur.tag === 'fork') cur = cur.r
    else return null
  }
  return cur
}

// ---- data -----------------------------------------------------------------------------------------

/** F is (△, △), T is (△△, △); 0 is (△, △), n + 1 is (△△, n). */
/** F is △ and T is △△, the two smallest trees (so a boolean is a bit); a number n is n stems on a leaf. */
export const T_ = (): V => S(L())
export const F_ = (): V => L()
export function nat(n: number): V {
  let v = L()
  for (let i = 0; i < n; i++) v = S(v)
  return v
}
export const asBool = (v: V): 'T' | 'F' | null =>
  v.tag === 'leaf' ? 'F' : v.tag === 'stem' && v.c.tag === 'leaf' ? 'T' : null
export function asNat(v: V): number | null {
  let n = 0
  for (let cur = v; ; n++) {
    if (cur.tag === 'leaf') return n
    if (cur.tag !== 'stem') return null
    cur = cur.c
  }
}

/** A bit is △ (0) or △△ (1); a word of 2^k bits is a balanced tree of them, high half on the left. */
export function word(bits: string): V {
  if (bits.length === 1) return bits === '1' ? S(L()) : L()
  if (bits.length & (bits.length - 1)) throw new Error('a word has 1, 2, 4, 8, … bits')
  const h = bits.length / 2
  return F(word(bits.slice(0, h)), word(bits.slice(h)))
}
/** Two registers of the same width, stored bit by bit: each leaf holds (a_i, b_i). */
export function zip(a: V, b: V): V {
  if (a.tag === 'fork' && b.tag === 'fork') return F(zip(a.l, b.l), zip(a.r, b.r))
  if (a.tag === 'fork' || b.tag === 'fork') throw new Error('<a, b> needs two words of the same width')
  return F(a, b)
}
const bitOf = (v: V): string | null => (v.tag === 'leaf' ? '0' : v.tag === 'stem' && v.c.tag === 'leaf' ? '1' : null)
/** The bits of a word, or null. */
export function wordBits(v: V): string | null {
  const b = bitOf(v)
  if (b) return b
  if (v.tag !== 'fork') return null
  const l = wordBits(v.l)
  const r = l && wordBits(v.r)
  return l && r && l.length === r.length ? l + r : null
}
/** The registers stored in a tree whose leaves each hold the same number of bits, one per register. */
export function registers(v: V): string[] | null {
  const leaf = (t: V): string[] | null => {
    const b = bitOf(t)
    if (b) return [b]
    if (t.tag !== 'fork') return null
    const h = bitOf(t.l)
    const rest = h && leaf(t.r)
    return h && rest ? [h, ...rest] : null
  }
  const own = leaf(v)
  if (own && v.tag === 'fork') return own
  if (v.tag !== 'fork') return null
  const l = registers(v.l)
  const r = l && registers(v.r)
  if (!l || !r || l.length !== r.length || l[0].length !== r[0].length) return null
  return l.map((s, i) => s + r[i])
}

// ---- reading a tree -------------------------------------------------------------------------------

/** What a tree does as a program: tree calculus's five shapes, each a reversible rule. */
export type ProgShape =
  | { k: 'id' }
  | { k: 'pair'; a: V }
  | { k: 'dagger'; b: V }
  | { k: 'compose'; a: V; b: V }
  | { k: 'clause'; l: V; r: V; rest: V }
export function progShape(t: V): ProgShape {
  if (t.tag === 'leaf') return { k: 'id' }
  if (t.tag === 'stem') return { k: 'pair', a: t.c }
  const h = t.l
  if (h.tag === 'leaf') return { k: 'dagger', b: t.r }
  if (h.tag === 'stem') return { k: 'compose', a: h.c, b: t.r }
  return { k: 'clause', l: h.l, r: h.r, rest: t.r }
}

/** What a tree means as a pattern: the same five shapes, read for matching and building. */
export type PatShape =
  | { k: 'leaf' }
  | { k: 'stem'; p: V }
  | { k: 'var'; n: V }
  | { k: 'fork'; a: V; b: V }
  | { k: 'call'; f: V; back: boolean; p: V }
export function patShape(p: V): PatShape {
  if (p.tag === 'leaf') return { k: 'leaf' }
  if (p.tag === 'stem') return { k: 'stem', p: p.c }
  const h = p.l
  if (h.tag === 'leaf') return { k: 'var', n: p.r }
  if (h.tag === 'stem') return { k: 'fork', a: h.c, b: p.r }
  return { k: 'call', f: h.l, back: h.r.tag !== 'leaf', p: p.r }
}

/** A constant inside a pattern: every fork gets a stem on its left part, so it reads as a fork. */
export const quote = (v: V): V =>
  v.tag === 'leaf' ? L() : v.tag === 'stem' ? S(quote(v.c)) : F(S(quote(v.l)), quote(v.r))
/** The constant a pattern with no variables and no calls stands for, or null. */
export function unquote(p: V): V | null {
  const s = patShape(p)
  if (s.k === 'leaf') return L()
  if (s.k === 'stem') {
    const c = unquote(s.p)
    return c && S(c)
  }
  if (s.k === 'fork') {
    const a = unquote(s.a)
    const b = a && unquote(s.b)
    return a && b && F(a, b)
  }
  return null
}

/** Variable number k is named by k stems on a leaf. */
const nameTree = (k: number): V => {
  let v = L()
  for (let i = 0; i < k; i++) v = S(v)
  return v
}
const nameIndex = (n: V): number | null => {
  let k = 0
  for (let cur = n; ; k++) {
    if (cur.tag === 'leaf') return k
    if (cur.tag !== 'stem') return null
    cur = cur.c
  }
}

// ---- the surface syntax ---------------------------------------------------------------------------
//
//   △  a leaf      △p  a stem      (p, q)  a fork      T F 0 1 2 …  constants
//   x  a variable  f(p)  p run through program f (self: the program itself; a variable: a program
//   that came in as data)      f†(p)  the same, f run backward      l ⇄ r  a clause, ; between clauses
// ASCII: ^ for △, <=> for ⇄, ' for †.

type Pat =
  | { k: 'leaf' }
  | { k: 'stem'; p: Pat }
  | { k: 'fork'; a: Pat; b: Pat }
  | { k: 'var'; name: string }
  | { k: 'lit'; v: V }
  | { k: 'call'; head: string; back: boolean; p: Pat }

type Tok = { t: string; at: number }
function lex(src: string): Tok[] {
  const out: Tok[] = []
  let i = 0
  while (i < src.length) {
    const ch = src[i]
    if (ch === '\n') out.push({ t: ';', at: i++ })
    else if (/\s/.test(ch)) i++
    else if (src.startsWith('<=>', i) || src.startsWith('<->', i)) out.push({ t: '⇄', at: i }), (i += 3)
    else if (ch === '^') out.push({ t: '△', at: i++ })
    else if (ch === "'") out.push({ t: '†', at: i++ })
    else if ('△()[],;⇄†<>'.includes(ch)) out.push({ t: ch, at: i++ })
    else if (/^0b[01]+/.test(src.slice(i))) {
      const m = /^0b[01]+/.exec(src.slice(i))![0]
      out.push({ t: m, at: i })
      i += m.length
    }
    else {
      const m = /^([A-Za-z_][A-Za-z0-9_]*|[0-9]+)/.exec(src.slice(i))
      if (!m) throw new Error(`unexpected ${ch} at ${i}`)
      out.push({ t: m[0], at: i })
      i += m[0].length
    }
  }
  return out
}

class Reader {
  i = 0
  toks: Tok[]
  values: boolean
  constructor(toks: Tok[], values: boolean) {
    this.toks = toks
    this.values = values
  }
  peek = () => this.toks[this.i]?.t
  next = () => this.toks[this.i++]?.t
  expect(t: string) {
    const got = this.toks[this.i]
    if (got?.t !== t) throw new Error(`expected ${t}${got ? ` at ${got.at}, found ${got.t}` : ' at the end'}`)
    this.i++
  }
  startsAtom = () => {
    const t = this.peek()
    return t !== undefined && (t === '△' || t === '(' || t === '[' || t === '<' || /^[A-Za-z_0-9]/.test(t))
  }
  pat(): Pat {
    const t = this.next()
    if (t === undefined) throw new Error('unexpected end')
    if (t === '△') return this.startsAtom() ? { k: 'stem', p: this.pat() } : { k: 'leaf' }
    if (t === '(') {
      const a = this.pat()
      this.expect(',')
      const b = this.pat()
      this.expect(')')
      return { k: 'fork', a, b }
    }
    if (t === '[') {
      if (!this.values) throw new Error('a [program] can only be data')
      const start = this.i
      let d = 1
      while (this.i < this.toks.length && d > 0) {
        const u = this.next()
        if (u === '[') d++
        if (u === ']') d--
      }
      if (d) throw new Error('expected ]')
      const inner = this.toks.slice(start, this.i - 1)
      return { k: 'lit', v: progFromTokens(inner) }
    }
    if (/^0b[01]+$/.test(t)) return { k: 'lit', v: word(t.slice(2)) }
    if (t === '<') {
      if (!this.values) throw new Error('<a, b> pairs two registers; only data has them')
      const a = valueOf(this.pat())
      this.expect(',')
      const b = valueOf(this.pat())
      this.expect('>')
      return { k: 'lit', v: zip(a, b) }
    }
    if (/^[0-9]+$/.test(t)) return { k: 'lit', v: nat(parseInt(t, 10)) }
    const back = this.peek() === '†' && this.toks[this.i + 1]?.t === '('
    if (back) this.i++
    if (this.peek() === '(' && !(t === 'T' || t === 'F')) {
      if (this.values) throw new Error(`${t}(…) is a call; data has none`)
      this.i++
      let p = this.pat()
      if (this.peek() === ',') {
        this.i++
        p = { k: 'fork', a: p, b: this.pat() }
      }
      this.expect(')')
      return { k: 'call', head: t, back, p }
    }
    if (t === 'T') return { k: 'lit', v: T_() }
    if (t === 'F') return { k: 'lit', v: F_() }
    if (this.values) {
      const lib = LIB.get(t)
      if (!lib) throw new Error(`no program called ${t}`)
      return { k: 'lit', v: copy(lib) }
    }
    return { k: 'var', name: t }
  }
}

function patVars(p: Pat, out: string[]) {
  if (p.k === 'var' && !out.includes(p.name)) out.push(p.name)
  if (p.k === 'stem' || p.k === 'call') patVars(p.p, out)
  if (p.k === 'fork') patVars(p.a, out), patVars(p.b, out)
}
function encode(p: Pat, names: string[]): V {
  switch (p.k) {
    case 'leaf': return L()
    case 'stem': return S(encode(p.p, names))
    case 'fork': return F(S(encode(p.a, names)), encode(p.b, names))
    case 'var': return F(L(), nameTree(names.indexOf(p.name)))
    case 'lit': return quote(p.v)
    case 'call': {
      let head: V
      if (p.head === 'self') head = L()
      else if (names.includes(p.head)) head = F(L(), nameTree(names.indexOf(p.head)))
      else {
        const lib = LIB.get(p.head)
        if (!lib) throw new Error(`no program or variable called ${p.head}`)
        head = copy(lib)
      }
      return F(F(head, p.back ? S(L()) : L()), encode(p.p, names))
    }
  }
}

/** Variable names by clause, so a tree read back shows the names it was written with. */
const NAMES = new Map<string, string[]>()

function progFromTokens(toks: Tok[]): V {
  const groups: Tok[][] = [[]]
  let d = 0
  for (const t of toks) {
    if (t.t === '(' || t.t === '[') d++
    if (t.t === ')' || t.t === ']') d--
    if (t.t === ';' && d === 0) groups.push([])
    else groups[groups.length - 1].push(t)
  }
  const clauses = groups.filter((g) => g.length)
  const topArrow = (g: Tok[]) => {
    let k = 0
    return g.some((t) => ((k += t.t === '[' ? 1 : t.t === ']' ? -1 : 0), t.t === '⇄' && k === 0))
  }
  if (!clauses.some(topArrow)) {
    // no clause: the program is written as a tree
    if (clauses.length !== 1) throw new Error('a program is clauses (l ⇄ r) or one tree')
    const r = new Reader(clauses[0], true)
    const v = r.pat()
    if (r.i !== clauses[0].length) throw new Error(`unexpected ${r.peek()}`)
    return valueOf(v)
  }
  const encoded = clauses.map((g) => {
    const r = new Reader(g, false)
    const l = r.pat()
    r.expect('⇄')
    const rr = r.pat()
    if (r.i !== g.length) throw new Error(`unexpected ${r.peek()} after a clause`)
    const names: string[] = []
    patVars(l, names)
    patVars(rr, names)
    const lt = encode(l, names)
    const rt = encode(rr, names)
    NAMES.set(key(lt) + '|' + key(rt), names)
    return F(lt, rt)
  })
  let t = L()
  for (let i = encoded.length - 1; i >= 0; i--) t = F(encoded[i], t)
  return t
}
function valueOf(p: Pat): V {
  switch (p.k) {
    case 'leaf': return L()
    case 'stem': return S(valueOf(p.p))
    case 'fork': return F(valueOf(p.a), valueOf(p.b))
    case 'lit': return p.v
    default: throw new Error('data has no variables or calls')
  }
}
/** A program: clauses `l ⇄ r` split by ; or newlines, or a single tree (every tree is a program). */
export const parseProg = (src: string): V => progFromTokens(lex(src))
/** A value: △ / △v / (u, v), T, F, numbers, a library program by name, or [clauses]. */
export function parseVal(src: string): V {
  const toks = lex(src).filter((t) => t.t !== ';')
  const r = new Reader(toks, true)
  const v = valueOf(r.pat())
  if (r.i !== toks.length) throw new Error(`unexpected ${r.peek()}`)
  return v
}

// ---- the library ----------------------------------------------------------------------------------

export const LIBRARY: { name: string; src: string }[] = [
  { name: 'not', src: 'F ⇄ T; T ⇄ F' },
  { name: 'swap', src: '(x, y) ⇄ (y, x)' },
  { name: 'cnot', src: '(T, t) ⇄ (T, not(t))' },
  { name: 'toffoli', src: '((T, T), c) ⇄ ((T, T), not(c))' },
  { name: 'and', src: '(T, T) ⇄ ((T, T), T); (a, b) ⇄ ((a, b), F)' },
  { name: 'succ', src: 'n ⇄ △n' },
  { name: 'ss', src: '(x, y) ⇄ (△x, △y)' },
  { name: 'add', src: '(△a, b) ⇄ ss(self(a, b))' },
  { name: 'mirror', src: '△x ⇄ △self(x); (x, y) ⇄ (self(y), self(x))' },
  { name: 'copy', src: 'x ⇄ (x, x)' },
  { name: 'twice', src: '(p, x) ⇄ (p, p(p(x)))' },
  { name: 'eval', src: '(p, x) ⇄ (p, p(x))' },
  { name: 'dagger', src: 'f ⇄ (△, f)' },
  { name: 'notw', src: 'F ⇄ T; T ⇄ F; (x, y) ⇄ (self(x), self(y))' },
  { name: 'cnotw', src: '(T, t) ⇄ (T, not(t)); ((p, q), r) ⇄ (self((p, q)), self(r))' },
  { name: 'prep', src: '((p, q), r) ⇄ (self((p, q)), self(r)); (a, b) ⇄ (△, (a, b))' },
  { name: 'comb', src: '((△△, h), (s, l)) ⇄ (s, ((△△, h), (s, l))); ((s, h), l) ⇄ (s, ((s, h), l))' },
  {
    name: 'up',
    src: '((p, q), r) ⇄ comb(self((p, q)), self(r)); (c, (△, △)) ⇄ (△, (c, (△, △))); (c, (△△, △△)) ⇄ (△△△, (c, (△△, △△))); (c, (a, b)) ⇄ (△△, (c, (a, b)))'
  },
  {
    name: 'down',
    src: '(△, (△, (a, b))) ⇄ (△, (a, b)); (△△, (△, (a, b))) ⇄ (△△, (a, b)); (c, ((s, h), (△, l))) ⇄ ((s, self(△, h)), (△, self(c, l))); (c, ((s, h), (△△△, l))) ⇄ ((s, self(△△, h)), (△△△, self(c, l))); (c, ((s, h), (△△, l))) ⇄ ((s, self(c, h)), (△△, self(c, l)))'
  },
  { name: 'carryin', src: '(s, t) ⇄ (s, down(△, t))' },
  { name: 'carries', src: 'z ⇄ up†(carryin(up(z)))' },
  { name: 'sumz', src: '((p, q), r) ⇄ (self((p, q)), self(r)); (△, (△, b)) ⇄ (△, (△, b)); (△△, (△△, b)) ⇄ (△△, (△△, b)); (c, (a, b)) ⇄ (c, (a, not(b)))' },
  { name: 'negb', src: '((p, q), r) ⇄ (self((p, q)), self(r)); (c, (a, b)) ⇄ (c, (a, not(b)))' },
  { name: 'addw', src: 'z ⇄ prep†(negb(carries†(negb(sumz(carries(prep(z)))))))' }
]
const LIB = new Map<string, V>()
const LIB_BY_KEY = new Map<string, string>()
let LIB_MAX = 0
for (const { name, src } of LIBRARY) {
  const t = parseProg(src)
  LIB.set(name, t)
  LIB_BY_KEY.set(key(t), name)
  LIB_MAX = Math.max(LIB_MAX, size(t))
}
export const libTree = (name: string): V => copy(LIB.get(name)!)
/** The library name of a program tree, or of the program it daggers. */
export function progName(t: V): string | null {
  if (size(t) > LIB_MAX + 2) return null
  const n = LIB_BY_KEY.get(key(t))
  if (n) return n
  const s = progShape(t)
  if (s.k === 'dagger') {
    const m = LIB_BY_KEY.get(key(s.b))
    if (m) return m + '†'
  }
  return null
}

// ---- reading trees back as text -------------------------------------------------------------------

export type Reads = 'bool' | 'nat' | 'bits' | 'regs' | 'tree'
const DEFAULT = ['x', 'y', 'z', 'u', 'v', 'w']
export const nameFor = (n: V, names: string[] | undefined): string => {
  const k = nameIndex(n)
  return k === null ? show(n) : (names?.[k] ?? DEFAULT[k] ?? `v${k}`)
}
export const clauseNames = (l: V, r: V) => NAMES.get(key(l) + '|' + key(r))

export function valText(v: V, reads: Reads): string {
  const p = v.tag === 'fork' && size(v) > 8 ? progName(v) : null
  if (p) return p
  if (reads === 'bool') {
    const b = asBool(v)
    if (b) return b
  }
  if (reads === 'nat') {
    const n = asNat(v)
    if (n !== null) return String(n)
  }
  if (reads === 'regs') {
    const regs = v.tag === 'fork' ? registers(v) : null
    if (regs && regs[0].length > 1) return `<${regs.map((r) => '0b' + r).join(', ')}>`
  }
  if (reads === 'bits' || reads === 'regs') {
    const w = wordBits(v)
    if (w && w.length > 1) return '0b' + w
    if (w) return w
  }
  if (v.tag === 'leaf') return '△'
  if (v.tag === 'stem') return '△' + valText(v.c, reads)
  return `(${valText(v.l, reads)}, ${valText(v.r, reads)})`
}

export function patText(p: V, names: string[] | undefined, reads: Reads): string {
  const lit = unquote(p)
  if (lit) return valText(lit, reads)
  const s = patShape(p)
  switch (s.k) {
    case 'leaf': return '△'
    case 'stem': return '△' + patText(s.p, names, reads)
    case 'var': return nameFor(s.n, names)
    case 'fork': return `(${patText(s.a, names, reads)}, ${patText(s.b, names, reads)})`
    case 'call': {
      const arg = patShape(s.p)
      const inner =
        arg.k === 'fork'
          ? `${patText(arg.a, names, reads)}, ${patText(arg.b, names, reads)}`
          : patText(s.p, names, reads)
      return `${headText(s.f, names)}${s.back ? '†' : ''}(${inner})`
    }
  }
}
function headText(f: V, names: string[] | undefined): string {
  if (f.tag === 'leaf') return 'self'
  if (f.tag === 'fork' && f.l.tag === 'leaf') return nameFor(f.r, names)
  return progName(f) ?? '[…]'
}

/** What a program does, in one line: its clauses, or what its shape means. */
export function programText(t: V, reads: Reads): string {
  const sub = (u: V) => progName(u) ?? `[${programText(u, reads)}]`
  const s = progShape(t)
  switch (s.k) {
    case 'id': return '△: leaves x as it is'
    case 'pair': return `△a: pairs x with ${valText(s.a, reads)}`
    case 'dagger': return `${sub(s.b)}, run backward`
    case 'compose': return `${sub(s.a)}, then ${sub(s.b)}`
    case 'clause': {
      const out: string[] = []
      let cur = t
      for (let c = progShape(cur); c.k === 'clause'; c = progShape(cur)) {
        const names = clauseNames(c.l, c.r)
        out.push(`${patText(c.l, names, reads)} ⇄ ${patText(c.r, names, reads)}`)
        cur = c.rest
      }
      if (cur.tag !== 'leaf') out.push(`otherwise ${sub(cur)}`)
      return out.join('; ')
    }
  }
}

/** Clause i (from 0) of a program, as its two sides in text, or null. */
export function clauseSides(t: V, i: number, reads: Reads): [string, string] | null {
  let cur = t
  for (let j = 0, c = progShape(cur); c.k === 'clause'; j++, c = progShape(cur)) {
    if (j === i) {
      const names = clauseNames(c.l, c.r)
      return [patText(c.l, names, reads), patText(c.r, names, reads)]
    }
    cur = c.rest
  }
  return null
}

const ROLES = new WeakMap<V, Map<Reads, Map<number, string>>>()
/** What each node of a program tree is, for drawing it opened: ⇄1 a clause, † a dagger, ∘ a
 * composition, a variable's name, a call by what it runs, a constant by its value. */
export function programRoles(t: V, reads: Reads): Map<number, string> {
  let byReads = ROLES.get(t)
  if (!byReads) ROLES.set(t, (byReads = new Map()))
  const had = byReads.get(reads)
  if (had) return had
  const out = new Map<number, string>()
  const prog = (u: V) => {
    const s = progShape(u)
    if (s.k === 'dagger') out.set(u.id, '†'), prog(s.b)
    else if (s.k === 'compose') out.set(u.id, '∘'), prog(s.a), prog(s.b)
    else if (s.k === 'pair') out.set(u.id, `holds ${valText(s.a, reads)}`)
    else if (s.k === 'clause') {
      let cur = u
      for (let i = 0, c = progShape(cur); c.k === 'clause'; i++, c = progShape(cur)) {
        const head = (cur as { l: V }).l
        out.set(head.id, `⇄${i + 1}`)
        const names = clauseNames(c.l, c.r)
        pat(c.l, names)
        pat(c.r, names)
        out.set(c.l.id, patText(c.l, names, reads))
        out.set(c.r.id, patText(c.r, names, reads))
        cur = c.rest
      }
    }
  }
  const pat = (p: V, names: string[] | undefined) => {
    const lit = unquote(p)
    if (lit && p.tag !== 'leaf') return void out.set(p.id, valText(lit, reads))
    const s = patShape(p)
    if (s.k === 'stem') pat(s.p, names)
    if (s.k === 'fork') pat(s.a, names), pat(s.b, names)
    if (s.k === 'var') out.set(p.id, nameFor(s.n, names))
    if (s.k === 'call') {
      out.set(p.id, `${headText(s.f, names)}${s.back ? '†' : ''}(…)`)
      pat(s.p, names)
      if (s.f.tag !== 'leaf' && !(s.f.tag === 'fork' && s.f.l.tag === 'leaf')) prog(s.f)
    }
  }
  prog(t)
  byReads.set(reads, out)
  return out
}

// ---- running ---------------------------------------------------------------------------------------

class Stuck extends Error {}
class Unbound extends Error {}
const MAX_STEPS = 200_000
const MAX_MADE = 1_000_000
type Env = Map<string, V>

/** One run. Every clause keeps two promises, or the run is stuck: every variable it matched is built
 * again (nothing is thrown away), and no earlier clause's output side fits what it gives (so the way
 * back picks the same clause: the symmetric first-match policy). */
class Runner {
  steps = 0
  made = 0

  /** A fresh copy of v, counted against how much one run may make. */
  copied(v: V): V {
    this.made += size(v)
    if (this.made > MAX_MADE) throw new Error('grew too big (it may not stop)')
    return copy(v)
  }

  run(t: V, x: V, fwd: boolean): V {
    if (++this.steps > MAX_STEPS) throw new Error('ran too long (it may not stop)')
    const s = progShape(t)
    switch (s.k) {
      case 'id':
        return x
      case 'pair':
        if (fwd) return F(this.copied(s.a), x)
        if (x.tag === 'fork' && eq(x.l, s.a)) return x.r
        throw new Stuck()
      case 'dagger':
        return this.run(s.b, x, !fwd)
      case 'compose':
        return fwd ? this.run(s.b, this.run(s.a, x, true), true) : this.run(s.a, this.run(s.b, x, false), false)
      case 'clause':
        return this.clauses(t, x, fwd)
    }
  }

  clauses(t: V, x: V, fwd: boolean): V {
    const cs: { l: V; r: V }[] = []
    let tail = t
    for (let s = progShape(tail); s.k === 'clause'; s = progShape(tail)) {
      cs.push({ l: s.l, r: s.r })
      tail = s.rest
    }
    const out = (c: { l: V; r: V }) => (fwd ? c.r : c.l)
    for (let i = 0; i < cs.length; i++) {
      const env: Env = new Map()
      let fits = false
      try {
        fits = this.match(fwd ? cs[i].l : cs[i].r, x, env, t)
      } catch (e) {
        if (!(e instanceof Stuck)) throw e
      }
      if (!fits) continue
      const used = new Set<string>()
      const y = this.build(out(cs[i]), env, t, used)
      for (const k of env.keys()) if (!used.has(k)) throw new Stuck()
      for (let j = 0; j < i; j++) if (this.fits(out(cs[j]), y, t)) throw new Stuck()
      return y
    }
    const y = tail.tag === 'leaf' ? x : this.run(tail, x, fwd)
    for (const c of cs) if (this.fits(out(c), y, t)) throw new Stuck()
    return y
  }

  fits(p: V, x: V, me: V): boolean {
    try {
      return this.match(p, x, new Map(), me)
    } catch (e) {
      if (e instanceof Stuck || e instanceof Unbound) return false
      throw e
    }
  }

  /** The program a call runs: the clause list itself, a program held in a variable, or a literal. */
  resolve(f: V, env: Env, me: V): V {
    if (f.tag === 'leaf') return me
    if (f.tag === 'fork' && f.l.tag === 'leaf') {
      const v = env.get(key(f.r))
      if (!v) throw new Unbound()
      return v
    }
    return f
  }

  match(p: V, x: V, env: Env, me: V): boolean {
    const s = patShape(p)
    switch (s.k) {
      case 'leaf':
        return x.tag === 'leaf'
      case 'stem':
        return x.tag === 'stem' && this.match(s.p, x.c, env, me)
      case 'var': {
        const k = key(s.n)
        const had = env.get(k)
        if (had) return eq(had, x)
        env.set(k, x)
        return true
      }
      case 'fork': {
        if (x.tag !== 'fork') return false
        const saved = new Map(env)
        try {
          return this.match(s.a, x.l, env, me) && this.match(s.b, x.r, env, me)
        } catch (e) {
          if (!(e instanceof Unbound)) throw e
          // a call on the left runs a program bound on the right: match the right first
          env.clear()
          for (const [k, v] of saved) env.set(k, v)
          return this.match(s.b, x.r, env, me) && this.match(s.a, x.l, env, me)
        }
      }
      case 'call': {
        // f(q) stands for f's output: run f backward on what is here, and q must fit what comes out
        let z: V
        try {
          z = this.run(this.resolve(s.f, env, me), x, s.back)
        } catch (e) {
          if (e instanceof Stuck) return false
          throw e
        }
        return this.match(s.p, z, env, me)
      }
    }
  }

  build(p: V, env: Env, me: V, used: Set<string>): V {
    const s = patShape(p)
    switch (s.k) {
      case 'leaf':
        return L()
      case 'stem':
        return S(this.build(s.p, env, me, used))
      case 'var': {
        const k = key(s.n)
        const v = env.get(k)
        if (!v) throw new Stuck()
        if (used.has(k)) return this.copied(v)
        used.add(k)
        return v
      }
      case 'fork':
        return F(this.build(s.a, env, me, used), this.build(s.b, env, me, used))
      case 'call': {
        const arg = this.build(s.p, env, me, used)
        try {
          return this.run(this.resolve(s.f, env, me), arg, !s.back)
        } catch (e) {
          if (e instanceof Unbound) throw new Stuck()
          throw e
        }
      }
    }
  }
}

/** f† puts one fork on top, (△, f), or takes one off if f has an odd number of them stacked there:
 * two daggers cancel, so flipping twice gives back the same tree. */
export function flipProg(f: V): V {
  let k = 0
  for (let t = f; t.tag === 'fork' && t.l.tag === 'leaf'; t = t.r) k++
  return k % 2 ? (f as { r: V }).r : F(L(), f)
}

/** A program holding a value: (△△s, c), or its dagger (△, (△△s, c)). */
export function holding(t: V): { s: V; code: V; back: boolean } | null {
  if (t.tag !== 'fork') return null
  if (t.l.tag === 'stem' && t.l.c.tag === 'stem') return { s: t.l.c.c, code: t.r, back: false }
  if (t.l.tag === 'leaf' && t.r.tag === 'fork' && t.r.l.tag === 'stem' && t.r.l.c.tag === 'stem')
    return { s: t.r.l.c.c, code: t.r.r, back: true }
  return null
}

/** Run a program on a value, or null if it gets stuck. */
export function run(f: V, x: V): V | null {
  try {
    return new Runner().run(f, x, true)
  } catch (e) {
    if (e instanceof Stuck) return null
    throw e
  }
}

/** apply(f, x) = (residual, value): the residual is f†, and a program holding a value gets back the
 * left half of what its code gives. apply(apply(f, x)) = (f, x). */
export function apply(f: V, x: V): [V, V] | null {
  const r = new Runner()
  try {
    const h = holding(f)
    if (!h) return [flipProg(f), r.run(f, x, true)]
    const out = r.run(h.code, F(h.s, x), !h.back)
    if (out.tag !== 'fork') return null
    const inner = F(S(S(out.l)), h.code)
    return [h.back ? inner : F(L(), inner), out.r]
  } catch (e) {
    if (e instanceof Stuck) return null
    throw e
  }
}

// ---- presets --------------------------------------------------------------------------------------

export interface Preset {
  name: string
  prog: string
  input: string
  reads: Reads
  tip: string
}
export const PRESETS: Preset[] = [
  { name: 'not', prog: 'F ⇄ T; T ⇄ F', input: 'T', reads: 'bool', tip: 'F is △ and T is △△, the two smallest trees. not is two clauses; the bud wears the one that will fire. Afterwards not† stays on the left: the program that undoes it.' },
  { name: 'cnot', prog: '(T, t) ⇄ (T, not(t))', input: '(T, F)', reads: 'bool', tip: 'One clause with a call on its right side: firing cnot grows a new bud, not(t), which fires next. (F, t) fits no clause and passes through.' },
  { name: 'mirror', prog: '△x ⇄ △self(x); (x, y) ⇄ (self(y), self(x))', input: '((△, △△), (△△△, △))', reads: 'tree', tip: 'Every fork splits into two calls that do not wait on each other, so each step fires all of them at once.' },
  { name: 'NOT, 8 bits', prog: 'F ⇄ T; T ⇄ F; (x, y) ⇄ (self(x), self(y))', input: '0b10110010', reads: 'bits', tip: 'A word is a balanced tree of bits (a bit is a boolean: △ or △△). NOT splits the word and runs on both halves at once: 15 calls in 4 parallel steps; 64 bits would take 7.' },
  { name: 'CNOT, 8 bits', prog: '(T, t) ⇄ (T, not(t)); ((p, q), r) ⇄ (self((p, q)), self(r))', input: '<0b11110000, 0b10101010>', reads: 'regs', tip: 'Two registers stored bit by bit: each leaf holds (control, target). Where the control is 1 the target flips, everywhere at once.' },
  { name: 'add, 4 bits', prog: 'z ⇄ prep†(negb(carries†(negb(sumz(carries(prep(z)))))))', input: '<0b0101, 0b0011>', reads: 'regs', tip: '(a, b) → (a, a + b) in place, in passes over the tree that each run in parallel: give every bit a carry slot, work out every carry (statuses up the tree, carries down it), add, then take the carries back out.' },
  { name: 'add, 8 bits', prog: 'z ⇄ prep†(negb(carries†(negb(sumz(carries(prep(z)))))))', input: '<0b10110101, 0b01101110>', reads: 'regs', tip: 'The same adder on 8 bits: 86 parallel steps, and 149 for 64 bits. Each width doubling adds a fixed number of steps.' },
  { name: 'add, unary', prog: '(△a, b) ⇄ ss(self(a, b))', input: '(3, 2)', reads: 'nat', tip: 'A number is that many stems on a leaf. (a, b) → (a, a + b): each call waits for the one inside it, so nothing runs in parallel.' },
  { name: 'sub = add†', prog: '(△, add)', input: '(2, 5)', reads: 'nat', tip: 'add run backward. Its clause is matched from the right side, ss(self(a, b)), whose calls become calls the other way (ss†, add†); a blue match node waits for them before it builds.' },
  { name: 'swap', prog: '(x, y) ⇄ (y, x)', input: '(T, F)', reads: 'bool', tip: 'Variables carry pieces across: the halves glide to each other’s places.' },
  { name: 'copy', prog: 'x ⇄ (x, x)', input: '(T, F)', reads: 'bool', tip: 'A variable used twice copies. Backward, the two halves must be equal.' },
  { name: 'twice', prog: '(p, x) ⇄ (p, p(p(x)))', input: '(succ, 1)', reads: 'nat', tip: 'A program that came in as data runs as a call: p(…) runs whatever p is.' }
]
