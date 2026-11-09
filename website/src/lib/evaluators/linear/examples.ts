// Programs for the nine-rule visualizer, in a small λ syntax, and the parser that loads them into a store.
//   {x, y} => body      a binder (bracket abstraction removes it, one step at a time)
//   f x y, f(x, y)      application
//   t                   the leaf △; t a is a stem, t a b a fork, applied further it runs
//   3                   a numeral: three stems on a leaf
//   name := term        a definition, used by name below (it shows folded under its name)
//   any other name      a symbol, left as it is
// `//` starts a comment.

import { Store, type Id } from './store.ts'

export interface Example {
  id: string
  title: string
  blurb: string
  source: string
}

export const EXAMPLES: Example[] = [
  {
    id: 'right',
    title: 'Composition: x goes right',
    blurb: 'λf g x. f (g x) becomes B through two binder steps; then B moves its argument into the right operand.',
    source: `({f, g, x} => f (g x)) a b c`
  },
  {
    id: 'left',
    title: 'Swap: x goes left',
    blurb: 'λx y. y x needs C: the argument goes into the left part.',
    source: `({x, y} => y x) a b`
  },
  {
    id: 's',
    title: 'S: x used twice',
    blurb: 'x on both sides becomes one W, the only rule that copies. Two right moves in a row merge into B′.',
    source: `({f, g, x} => f x (g x)) a b c`
  },
  {
    id: 'drop',
    title: 'Drop: x unused',
    blurb: 'An unused variable becomes K, the only rule that drops: the dropped argument falls off the tree.',
    source: `({x, y} => x) a b`
  },
  {
    id: 'triage',
    title: 'Triage hands back what it beat',
    blurb: 'On a fork, triage picks its third arm and gives it the two arms it did not pick, then the fork’s children.',
    source: `t (t c d) b (t u v)`
  },
  {
    id: 'add',
    title: 'Add, dropping only at the end',
    blurb:
      'Each round triage reads one stem of m; the arms it did not pick go into the next round, and only the arm that ran is copied (W), from the spare dh. Only the last round drops (K).',
    source: `// the arm for a stem of m: the next round's arms are the two handed back plus a copy of this one
d := {o1, o2, u, n, dh} => t (t o1 dh) o2 u (t n) dh
// the arm for the end: keep n, drop the rest
c := {o1, o2, n, dh} => n
add := t (t c d) t
add 3 2 d`
  },
  {
    id: 'radd',
    title: 'Reversible add: the program comes back',
    blurb: 'Nothing is dropped: the last round rebuilds the program from the arms it was handed and its spares, and the answer keeps m.',
    source: `// a round: rebuild the arms from the two handed back and a copy of this one, add a stem to a and to n
d := {o1, o2, u, a, n, dh} => t (t o1 dh) o2 u (t a) (t n) dh
// the end: the arms come back as they were, with a (= m) and n (= m + n)
c := {o1, o2, a, n, dh} => t (t (t (t o2 o1) o2) dh) (t a n)
radd := t (t c d) c
radd 2 t 1 d`
  },
  {
    id: 'mirror',
    title: 'Reversible mirror of a small tree',
    blurb: 'Tree recursion with nothing dropped: the second branch runs on the program the first branch handed back.',
    source: `c := {o1, o2, cs, ds, bs, k} => k (t (t cs o1) o2) cs ds bs t
d := {o1, o2, u, cs, ds, bs, k} => t (t o1 ds) o2 u cs ds bs ({P, c1, d1, b1, r} => k P c1 d1 b1 (t r))
b := {o1, o2, u, v, cs, ds, bs, k} => t (t o1 o2) bs u cs ds bs ({P, c1, d1, b1, ru} => P v c1 d1 b1 ({P2, c2, d2, b2, rv} => k P2 c2 d2 b2 (t rv ru)))
k0 := {P, c1, d1, b1, r} => t (t P (t c1 (t d1 b1))) r
mirror := t (t c d) b
mirror (t (t t) t) c d b k0`
  }
]

type Tok = { k: 'id' | 'num' | 'sym'; v: string; at: number }

function lex(src: string): Tok[] {
  const out: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    if (c === '/' && src[i + 1] === '/') {
      while (i < src.length && src[i] !== '\n') i++
      continue
    }
    if (/\s/.test(c)) {
      i++
      continue
    }
    if (src.startsWith(':=', i) || src.startsWith('=>', i)) {
      out.push({ k: 'sym', v: src.slice(i, i + 2), at: i })
      i += 2
      continue
    }
    if ('{},()'.includes(c)) {
      out.push({ k: 'sym', v: c, at: i })
      i++
      continue
    }
    if (c === '△') {
      out.push({ k: 'id', v: 't', at: i })
      i++
      continue
    }
    const m = /^[0-9]+/.exec(src.slice(i)) ?? /^[A-Za-z_][A-Za-z0-9_′']*/.exec(src.slice(i))
    if (!m) throw new SyntaxError(`unexpected '${c}' at ${i}`)
    out.push({ k: /[0-9]/.test(m[0][0]) ? 'num' : 'id', v: m[0], at: i })
    i += m[0].length
  }
  return out
}

/// Surface terms before they go into the store: binders, applications, names, the leaf, numerals.
type Term = { k: 'lam'; xs: string[]; body: Term } | { k: 'app'; f: Term; x: Term } | { k: 'name'; v: string } | { k: 'leaf' } | { k: 'num'; n: number }

function parseTerm(toks: Tok[], pos: { i: number }, stop: Set<string>): Term {
  const peek = () => toks[pos.i]
  const eat = (v: string) => {
    const t = toks[pos.i]
    if (!t || t.v !== v) throw new SyntaxError(`expected '${v}'${t ? ` at ${t.at}` : ' at the end'}`)
    pos.i++
  }
  const atom = (): Term | null => {
    const t = peek()
    if (!t || (t.k === 'sym' && t.v !== '(' && t.v !== '{')) return null
    if (t.k === 'sym' && t.v === '{') {
      pos.i++
      const xs: string[] = []
      for (;;) {
        const n = peek()
        if (!n || n.k !== 'id') throw new SyntaxError(`expected a name in a binder${n ? ` at ${n.at}` : ''}`)
        xs.push(n.v)
        pos.i++
        if (peek()?.v === ',') pos.i++
        else break
      }
      eat('}')
      eat('=>')
      return { k: 'lam', xs, body: parseTerm(toks, pos, stop) }
    }
    if (t.v === '(') {
      pos.i++
      const inner = parseTerm(toks, pos, new Set([')']))
      eat(')')
      return inner
    }
    pos.i++
    if (t.k === 'num') return { k: 'num', n: Number(t.v) }
    if (t.v === 't') return { k: 'leaf' }
    return { k: 'name', v: t.v }
  }
  let head = atom()
  if (!head) throw new SyntaxError(`expected a term${peek() ? ` at ${peek().at}` : ' at the end'}`)
  for (;;) {
    const t = peek()
    if (!t || (t.k === 'sym' && stop.has(t.v))) break
    // f(a, b) right after a name or a closing parenthesis is a call with several arguments
    if (t.v === '(' && toks[pos.i - 1] && toks[pos.i - 1].at + toks[pos.i - 1].v.length === t.at && toks[pos.i - 1].v !== '(') {
      pos.i++
      for (;;) {
        head = { k: 'app', f: head, x: parseTerm(toks, pos, new Set([',', ')'])) }
        if (peek()?.v === ',') pos.i++
        else break
      }
      eat(')')
      continue
    }
    const next = atom()
    if (!next) break
    head = { k: 'app', f: head, x: next }
  }
  return head
}

export interface Loaded {
  root: Id
  /// definitions in order, with the store id of their source and of their compiled tree
  defs: { name: string; source: Id; tree: Id }[]
}

/// Parse a program into the store: definitions are named and used in place; the last line is the term to run.
export function loadProgram(store: Store, src: string): Loaded {
  // split on lines that start a definition, so a definition may run over several lines
  const lines = src.split('\n')
  const chunks: string[] = []
  for (const line of lines) {
    const bare = line.replace(/\/\/.*$/, '')
    if (/^\s*[A-Za-z_][A-Za-z0-9_′']*\s*:=/.test(bare) || chunks.length === 0) chunks.push(bare)
    else if (bare.trim() && /^\s*[A-Za-z_][A-Za-z0-9_′']*\s*:=/.test(chunks[chunks.length - 1]) && !/^\s/.test(bare)) chunks.push(bare)
    else chunks[chunks.length - 1] += '\n' + bare
  }
  const env = new Map<string, Id>()
  const defs: Loaded['defs'] = []
  let root: Id | null = null
  for (const chunk of chunks) {
    if (!chunk.trim()) continue
    const def = /^\s*([A-Za-z_][A-Za-z0-9_′']*)\s*:=([\s\S]*)$/.exec(chunk)
    const text = def ? def[2] : chunk
    const toks = lex(text)
    if (!toks.length) continue
    const pos = { i: 0 }
    const term = parseTerm(toks, pos, new Set())
    if (pos.i < toks.length) throw new SyntaxError(`unexpected '${toks[pos.i].v}' at ${toks[pos.i].at}`)
    const id = build(store, term, env, new Set())
    if (def) {
      const tree = store.removeBinders(id)
      env.set(def[1], id)
      defs.push({ name: def[1], source: id, tree })
      if (!store.names.has(id)) store.names.set(id, def[1])
      if (!store.names.has(tree)) store.names.set(tree, def[1])
    } else root = id
  }
  if (root === null) throw new SyntaxError('nothing to run: the last line should be a term')
  return { root, defs }
}

/// Into the store: the leaf applied to one or two things is the stem or fork it builds.
function build(store: Store, t: Term, env: Map<string, Id>, bound: Set<string>): Id {
  switch (t.k) {
    case 'leaf':
      return store.leaf
    case 'num':
      return store.nat(t.n)
    case 'name':
      if (bound.has(t.v)) return store.variable(t.v)
      return env.get(t.v) ?? store.variable(t.v)
    case 'lam': {
      const inner = new Set(bound)
      for (const x of t.xs) inner.add(x)
      let body = build(store, t.body, env, inner)
      for (let i = t.xs.length - 1; i >= 0; i--) body = store.lam(t.xs[i], body)
      return body
    }
    case 'app': {
      const args: Term[] = []
      let f: Term = t
      while (f.k === 'app') {
        args.unshift(f.x)
        f = f.f
      }
      const xs = args.map((a) => build(store, a, env, bound))
      if (f.k === 'leaf') {
        let head = xs.length >= 2 ? store.fork(xs[0], xs[1]) : store.stem(xs[0])
        for (const x of xs.slice(2)) head = store.app(head, x)
        return head
      }
      let head = build(store, f, env, bound)
      for (const x of xs) head = store.app(head, x)
      return head
    }
  }
}
