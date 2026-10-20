// Programs and values as text. A program is the moves (swap, cross, sink, stem, unit, eval) and id, put
// together with `a ; b`, `a†`, tri(a, b, c), map(a, b, c) and fix(b), with self inside a fix, and
// held(p) for a program that has run; first(f), second(f), par(f, g) and when(f) are shorthands for maps
// and a tri, and {tree} is any tree read as a program. A value is △, △v, (u, v), T, F, a number, a word 0b1011, registers <0b10, 0b01>, or a
// program as the tree it is, [p].

import { F, L, S, ATOMS, asBit, asNat, copy, form, key, nat, prog, regs, registersOf, size, type Atom, type V } from './core'

type Tok = { t: string; at: number }
function lex(src: string): Tok[] {
  const out: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    if (/\s/.test(c)) i++
    else if (src.startsWith('0b', i)) {
      const m = /^0b[01]+/.exec(src.slice(i))
      if (!m) throw new Error(`a word needs bits after 0b, at ${i}`)
      out.push({ t: m[0], at: i })
      i += m[0].length
    } else if (/[A-Za-z_0-9]/.test(c)) {
      const m = /^[A-Za-z_0-9]+/.exec(src.slice(i))!
      out.push({ t: m[0], at: i })
      i += m[0].length
    } else if ('△^()[]{}<>,;†\'~'.includes(c)) {
      out.push({ t: c === '^' ? '△' : c === "'" ? '†' : c, at: i })
      i++
    } else throw new Error(`unexpected ${c} at ${i}`)
  }
  return out
}

/** The programs a name stands for: the library, filled in by library.ts. */
export const NAMED = new Map<string, V>()
const NAME_OF = new Map<string, string>()
export function define(name: string, p: V) {
  NAMED.set(name, p)
  if (!NAME_OF.has(key(p))) NAME_OF.set(key(p), name)
}
/** The library's name for a program, or for the program it runs backward. */
export function nameOf(p: V): string | null {
  const n = NAME_OF.get(key(p))
  if (n) return n
  const f = form(p)
  if (f.k === 'dag') {
    const m = NAME_OF.get(key(f.b))
    if (m) return m + '†'
  }
  return null
}

const ARITY: Record<string, number> = { tri: 3, map: 3, fix: 1, held: 1, first: 1, second: 1, par: 2, when: 1 }

class Reader {
  i = 0
  toks: Tok[]
  constructor(src: string) {
    this.toks = lex(src)
  }
  peek = () => this.toks[this.i]?.t
  where = () => (this.toks[this.i] ? `at ${this.toks[this.i].at}` : 'at the end')
  eat(t: string): boolean {
    if (this.peek() !== t) return false
    this.i++
    return true
  }
  want(t: string) {
    if (!this.eat(t)) throw new Error(`expected ${t} ${this.where()}`)
  }
  end() {
    if (this.i < this.toks.length) throw new Error(`unexpected ${this.peek()} ${this.where()}`)
  }

  prog(): V {
    const a = this.post()
    return this.eat(';') ? prog.seq(a, this.prog()) : a
  }
  post(): V {
    let p = this.pre()
    while (this.eat('†')) p = prog.dag(p)
    return p
  }
  pre(): V {
    if (this.eat('~')) return prog.dag(this.pre())
    if (this.eat('(')) {
      const p = this.prog()
      this.want(')')
      return p
    }
    if (this.eat('{')) {
      const v = this.val()
      this.want('}')
      return v
    }
    const at = this.where()
    const n = this.peek()
    if (!n || !/^[A-Za-z_]/.test(n)) throw new Error(`expected a program ${at}`)
    this.i++
    if (n in ARITY) {
      this.want('(')
      const args = [this.prog()]
      while (this.eat(',')) args.push(this.prog())
      this.want(')')
      if (args.length !== ARITY[n]) throw new Error(`${n} takes ${ARITY[n]} part${ARITY[n] > 1 ? 's' : ''} ${at}`)
      const [a, b, c] = args
      switch (n) {
        case 'tri':
          return prog.tri(a, b, c)
        case 'map':
          return prog.map(a, b, c)
        case 'fix':
          return prog.fix(a)
        case 'held':
          return prog.held(a)
        case 'first':
          return prog.map(L(), a, L())
        case 'second':
          return prog.map(L(), L(), a)
        case 'par':
          return prog.map(L(), a, b)
        default:
          return prog.tri(L(), prog.tri(a, L(), L()), L())
      }
    }
    if (n === 'id') return L()
    if ((ATOMS as readonly string[]).includes(n)) return prog.atom(n as Atom)
    const p = NAMED.get(n)
    if (!p) throw new Error(`${n}: no such program ${at}`)
    return copy(p)
  }

  val(): V {
    const at = this.where()
    const t = this.peek()
    if (t === '△' || t === 't') {
      this.i++
      const n = this.peek()
      const more = n === '△' || n === 't' || n === '(' || n === '[' || n === 'T' || n === 'F' || (!!n && /^[0-9]/.test(n))
      return more ? S(this.val()) : L()
    }
    if (this.eat('(')) {
      const a = this.val()
      this.want(',')
      const b = this.val()
      this.want(')')
      return F(a, b)
    }
    if (this.eat('[')) {
      const p = this.prog()
      this.want(']')
      return copy(p)
    }
    if (this.eat('<')) {
      const ws = [this.word()]
      while (this.eat(',')) ws.push(this.word())
      this.want('>')
      return regs(ws)
    }
    if (t?.startsWith('0b')) return regs([this.word()])
    this.i++
    if (t === 'T') return S(L())
    if (t === 'F') return L()
    if (t && /^[0-9]+$/.test(t)) {
      if (+t > 1000) throw new Error(`${t} stems is a lot of stems ${at}`)
      return nat(+t)
    }
    throw new Error(`expected a value ${at}`)
  }
  word(): string {
    const t = this.peek()
    if (!t?.startsWith('0b')) throw new Error(`expected a word like 0b1011 ${this.where()}`)
    this.i++
    return t.slice(2)
  }
}

export function parseProg(src: string): V {
  const r = new Reader(src)
  const p = r.prog()
  r.end()
  return p
}
export function parseVal(src: string): V {
  const r = new Reader(src)
  const v = r.val()
  r.end()
  return v
}

const isId = (p: V) => p.tag === 'leaf'

/** A program as text, by its library names where it has them (`names`; not at the top when it is
 * spelled out, `expand`), so that reading the text back gives the same tree. */
export function progText(p: V, names = true, expand = false): string {
  const go = (t: V, top: boolean): string => {
    if (names && !top) {
      const n = nameOf(t)
      if (n) return n
    }
    const f = form(t)
    switch (f.k) {
      case 'id':
        return 'id'
      case 'atom':
        return f.n
      case 'none':
        return `{${valText(t, 'tree')}}`
      case 'dag': {
        const b = go(f.b, false)
        return form(f.b).k === 'seq' ? `(${b})†` : `${b}†`
      }
      case 'seq': {
        const a = go(f.a, false)
        return `${form(f.a).k === 'seq' ? `(${a})` : a} ; ${go(f.b, false)}`
      }
      case 'tri': {
        const inner = form(f.b)
        if (isId(f.a) && isId(f.c) && inner.k === 'tri' && isId(inner.b) && isId(inner.c)) return `when(${go(inner.a, false)})`
        return `tri(${go(f.a, false)}, ${go(f.b, false)}, ${go(f.c, false)})`
      }
      case 'map':
        if (!isId(f.a)) return `map(${go(f.a, false)}, ${go(f.b, false)}, ${go(f.c, false)})`
        if (isId(f.b) && !isId(f.c)) return `second(${go(f.c, false)})`
        if (isId(f.c) && !isId(f.b)) return `first(${go(f.b, false)})`
        return `par(${go(f.b, false)}, ${go(f.c, false)})`
      case 'fix':
        return `fix(${go(f.b, false)})`
      case 'held':
        return `held(${go(f.b, false)})`
    }
  }
  return go(p, expand)
}
const LABELS = new WeakMap<V, string>()
const DEFS = new WeakMap<V, string>()
/** A program as its name and what the name stands for, or as its text. */
export function progDef(p: V): string {
  let s = DEFS.get(p)
  if (s === undefined) {
    const n = nameOf(p)
    DEFS.set(p, (s = n && !n.endsWith('†') ? `${n} = ${progText(p, true, true)}` : progLabel(p)))
  }
  return s
}
/** A program's library name, or its text (remembered: programs never change). */
export function progLabel(p: V): string {
  let s = LABELS.get(p)
  if (s === undefined) LABELS.set(p, (s = nameOf(p) ?? progText(p)))
  return s
}

/** How to read values: as booleans, numbers, words, registers, or plain trees. */
export type Reads = 'bool' | 'nat' | 'bits' | 'regs' | 'tree'

export function valText(v: V, reads: Reads): string {
  if (reads !== 'tree' && size(v) >= 6) {
    const n = nameOf(v)
    if (n && !n.endsWith('†')) return `[${n}]`
  }
  if (reads === 'bool') {
    const b = asBit(v)
    if (b !== null) return b ? 'T' : 'F'
  }
  if (reads === 'nat') {
    const n = asNat(v)
    if (n !== null) return String(n)
  }
  if (reads === 'bits' || reads === 'regs') {
    const rs = registersOf(v)
    if (rs) return rs.length > 1 ? `<${rs.map((r) => '0b' + r).join(', ')}>` : '0b' + rs[0]
  }
  if (v.tag === 'leaf') return '△'
  if (v.tag === 'stem') return '△' + valText(v.c, reads)
  return `(${valText(v.l, reads)}, ${valText(v.r, reads)})`
}
