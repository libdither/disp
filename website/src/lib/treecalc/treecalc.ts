// A small tree-calculus playground engine for the site's visual widgets —
// ported from INTERACTIVE_WALKTHROUGH.html's in-page reducer. This is the
// PEDAGOGICAL evaluator (plain objects, one visible step at a time, apply
// nodes materialized); the real engine behind the playground is the compiler
// worker. Kept semantically faithful to the five rules.

export type T =
  | { tag: 'leaf' }
  | { tag: 'stem'; c: T }
  | { tag: 'fork'; l: T; r: T }
  | { tag: 'apply'; f: T; x: T }
  // a named leaf: a free variable. It has no shape of its own, so nothing can
  // fire on it — applications with a var head (and triage on a var) stay stuck,
  // which is exactly how reductions read on paper: K x y ▷ x, symbolically.
  | { tag: 'var'; name: string }

export const leaf: T = { tag: 'leaf' }
export const stem = (c: T): T => ({ tag: 'stem', c })
export const fork = (l: T, r: T): T => ({ tag: 'fork', l, r })
export const varLeaf = (name: string): T => ({ tag: 'var', name })

// Construction rules 1–2 fire immediately; a fork application materializes an
// apply node for the stepper to fire visibly.
export function app(f: T, x: T): T {
  if (f.tag === 'leaf') return stem(x)
  if (f.tag === 'stem') return fork(f.c, x)
  return { tag: 'apply', f, x }
}

export function nat(n: number): T {
  let cur: T = leaf
  for (let i = 0; i < n; i++) cur = fork(leaf, cur)
  return cur
}

export function natValue(t: T): number | null {
  let n = 0
  let cur = t
  while (cur.tag === 'fork' && cur.l.tag === 'leaf') {
    n++
    cur = cur.r
  }
  return cur.tag === 'leaf' ? n : null
}

export const childrenOf = (t: T): T[] =>
  t.tag === 'stem' ? [t.c] : t.tag === 'fork' ? [t.l, t.r] : t.tag === 'apply' ? [t.f, t.x] : []

export function treeEq(a: T, b: T): boolean {
  if (a === b) return true
  if (a.tag !== b.tag) return false
  if (a.tag === 'var') return a.name === (b as T & { tag: 'var' }).name
  const ca = childrenOf(a)
  const cb = childrenOf(b)
  return ca.length === cb.length && ca.every((c, i) => treeEq(c, cb[i]))
}

// ---- the five rules, one step at a time ----------------------------------

export interface StepResult {
  result: T
  rule: string // human-readable description of the fired rule
  tag: 'L' | 's' | 'K' | 'S' | 'F' | null
}

export function reduceStep(f: T, x: T): StepResult {
  if (f.tag === 'leaf') return { result: stem(x), rule: 'leaf rule (△): t ▷ x = t x', tag: 'L' }
  if (f.tag === 'stem')
    return { result: fork(f.c, x), rule: 'fork-construction rule (f): t u ▷ x = t u x', tag: 's' }
  if (f.tag === 'fork') {
    const a = f.l
    const b = f.r
    if (a.tag === 'leaf') return { result: b, rule: 'K rule: t t b ▷ x = b', tag: 'K' }
    if (a.tag === 'stem')
      return {
        result: { tag: 'apply', f: { tag: 'apply', f: a.c, x }, x: { tag: 'apply', f: b, x } },
        rule: 'S rule: t (t c) b ▷ x = (c x) (b x)',
        tag: 'S'
      }
    if (a.tag === 'fork') {
      // triage on x's shape (x must itself be in normal form)
      if (x.tag === 'leaf') return { result: a.l, rule: 'F rule (x = leaf): t (t c d) b ▷ t = c', tag: 'F' }
      if (x.tag === 'stem')
        return { result: { tag: 'apply', f: a.r, x: x.c }, rule: 'F rule (x = stem): t (t c d) b ▷ t u = d u', tag: 'F' }
      if (x.tag === 'fork')
        return {
          result: { tag: 'apply', f: { tag: 'apply', f: b, x: x.l }, x: x.r },
          rule: 'F rule (x = fork): t (t c d) b ▷ t u v = b u v',
          tag: 'F'
        }
    }
  }
  return { result: { tag: 'apply', f, x }, rule: 'stuck', tag: null }
}

// Which apply node fires next under the leftmost-innermost order the widget
// animates (descend into f, then f.l, then — for triage — into x). Free
// variables leave stuck applies behind; a stuck node is skipped, but its
// argument may still hold live redexes.
export function nextApplyToFire(n: T): (T & { tag: 'apply' }) | null {
  if (n.tag === 'apply') {
    const f = n.f
    if (f.tag === 'apply') {
      const inner = nextApplyToFire(f)
      if (inner) return inner
    }
    if (f.tag === 'fork') {
      const a = f.l
      if (a.tag === 'apply') {
        const inner = nextApplyToFire(a)
        if (inner) return inner
      }
      if (a.tag === 'fork' && n.x.tag === 'apply') {
        const inner = nextApplyToFire(n.x)
        if (inner) return inner
      }
    }
    if (readyRule(n)) return n as T & { tag: 'apply' }
    // stuck here (var head, or triage waiting on a shapeless argument):
    // reductions may still hide in either side
    return nextApplyToFire(f) ?? nextApplyToFire(n.x)
  }
  if (n.tag === 'fork') return nextApplyToFire(n.l) ?? nextApplyToFire(n.r)
  if (n.tag === 'stem') return nextApplyToFire(n.c)
  return null
}

// Replace `target` (by reference) with `repl` inside `root`.
export function replaceNode(root: T, target: T, repl: T): T {
  if (root === target) return repl
  if (root.tag === 'leaf' || root.tag === 'var') return root
  if (root.tag === 'stem') {
    const c = replaceNode(root.c, target, repl)
    return c === root.c ? root : stem(c)
  }
  if (root.tag === 'fork') {
    const l = replaceNode(root.l, target, repl)
    const r = replaceNode(root.r, target, repl)
    return l === root.l && r === root.r ? root : fork(l, r)
  }
  const f = replaceNode(root.f, target, repl)
  const x = replaceNode(root.x, target, repl)
  return f === root.f && x === root.x ? root : { tag: 'apply', f, x }
}

// Which rule (if any) is READY to fire at this node right now, without other
// applies resolving first. Confluence makes every ready redex independent.
export function readyRule(node: T): StepResult['tag'] {
  if (node.tag !== 'apply') return null
  const f = node.f
  if (f.tag === 'leaf') return 'L'
  if (f.tag === 'stem') return 's'
  if (f.tag === 'fork') {
    const a = f.l
    if (a.tag === 'leaf') return 'K'
    if (a.tag === 'stem') return 'S'
    // triage reads x's shape: pending applies and shapeless free variables
    // both leave it waiting
    if (a.tag === 'fork') return node.x.tag === 'apply' || node.x.tag === 'var' ? null : 'F'
    return null // f.l is itself pending or a free variable
  }
  return null // f is pending or a free variable
}

// Every redex that is ready in the CURRENT tree (by reference).
export function readySites(root: T, out: (T & { tag: 'apply' })[] = []): (T & { tag: 'apply' })[] {
  if (readyRule(root)) out.push(root as T & { tag: 'apply' })
  for (const c of childrenOf(root)) readySites(c, out)
  return out
}

// One visible step at the next redex; null at normal form.
export function stepOnce(root: T): { next: T; rule: string; tag: StepResult['tag'] } | null {
  const site = nextApplyToFire(root)
  if (!site) return null
  const { result, rule, tag } = reduceStep(site.f, site.x)
  return { next: replaceNode(root, site, result), rule, tag }
}

// One PARALLEL step: fire every redex that is ready in the current tree, in a
// single bottom-up pass (Tait–Martin-Löf style). Returns null at normal form.
export function stepParallel(root: T): { next: T; fired: NonNullable<StepResult['tag']>[] } | null {
  const fired: NonNullable<StepResult['tag']>[] = []
  function go(t: T): T {
    if (t.tag === 'leaf' || t.tag === 'var') return t
    if (t.tag === 'stem') {
      const c = go(t.c)
      return c === t.c ? t : stem(c)
    }
    if (t.tag === 'fork') {
      const l = go(t.l)
      const r = go(t.r)
      return l === t.l && r === t.r ? t : fork(l, r)
    }
    // an apply: rewrite children first, then fire here if ready
    const tag = readyRule(t)
    const f = go(t.f)
    const x = go(t.x)
    const rebuilt: T = f === t.f && x === t.x ? t : { tag: 'apply', f, x }
    if (tag) {
      // readiness was judged on the ORIGINAL node; children of a ready redex
      // are untouched by construction (they contain no ready applies for
      // L/s/K/S, and F additionally required x in normal form)
      const s = reduceStep(rebuilt.tag === 'apply' ? rebuilt.f : f, rebuilt.tag === 'apply' ? rebuilt.x : x)
      fired.push(tag)
      return s.result
    }
    return rebuilt
  }
  const next = go(root)
  if (fired.length === 0) return null
  return { next, fired }
}

export function normalize(root: T, limit = 2000): T {
  let cur = root
  for (let i = 0; i < limit; i++) {
    const s = stepOnce(cur)
    if (!s) return cur
    cur = s.next
  }
  return cur
}

// ---- bracket abstraction (mirrors src/elab/cir.ts's optimizations) --------

type Cir =
  | { tag: 'var'; name: string }
  | { tag: 'app'; f: Cir; x: Cir }
  | { tag: 'lam'; param: string; body: Cir }
  | { tag: 'tree'; tree: T }
  | { tag: 'K' }
  | { tag: 'I' }
  | { tag: 'S' }

const cK: Cir = { tag: 'K' }
const cI: Cir = { tag: 'I' }
const cS: Cir = { tag: 'S' }

function isFree(name: string, e: Cir): boolean {
  if (e.tag === 'var') return e.name === name
  if (e.tag === 'app') return isFree(name, e.f) || isFree(name, e.x)
  if (e.tag === 'lam') return e.param !== name && isFree(name, e.body)
  return false
}

function abs(name: string, body: Cir): Cir {
  if (!isFree(name, body)) return { tag: 'app', f: cK, x: body }
  if (body.tag === 'var') return cI
  if (body.tag === 'app') {
    if (body.x.tag === 'var' && body.x.name === name && !isFree(name, body.f)) return body.f
    const af = abs(name, body.f)
    const ax = abs(name, body.x)
    if (af.tag === 'app' && af.f.tag === 'K' && ax.tag === 'I') return af.x
    if (af.tag === 'app' && af.f.tag === 'K' && ax.tag === 'app' && ax.f.tag === 'K')
      return { tag: 'app', f: cK, x: { tag: 'app', f: af.x, x: ax.x } }
    return { tag: 'app', f: { tag: 'app', f: cS, x: af }, x: ax }
  }
  if (body.tag === 'lam') return abs(name, abs(body.param, body.body))
  throw new Error('cannot abstract over ' + body.tag)
}

const K_TREE: T = stem(leaf) // K x y = x  (fires the K rule when saturated)
const I_TREE: T = fork(stem(K_TREE), K_TREE) // = S K K: I x → (K x)(K x) → x
// S = t (t (t t t)) t: S a → t (t a), S a b → t (t a) b, S a b c → (a c)(b c).
// (bracket-abstracting {a,b} -> t (t a) b by hand: η-reduce b, then [a](t (t a))
// = S (K t) t, whose fork form is exactly this literal.)
const S_TREE: T = fork(stem(fork(leaf, leaf)), leaf)

function cirToTree(e: Cir): T {
  if (e.tag === 'I') return I_TREE
  if (e.tag === 'K') return K_TREE
  if (e.tag === 'S') return S_TREE
  if (e.tag === 'tree') return e.tree
  if (e.tag === 'lam') return cirToTree(abs(e.param, e.body))
  if (e.tag === 'var') throw new Error(`unbound variable in lam body: ${e.name}`)
  // S(x)(y) / K(x)(y) short-circuit to their fork forms
  if (e.f.tag === 'app' && e.f.f.tag === 'S') return fork(stem(cirToTree(e.f.x)), cirToTree(e.x))
  if (e.f.tag === 'app' && e.f.f.tag === 'K') return cirToTree(e.f.x)
  if (e.f.tag === 'S') return normalize(app(S_TREE, cirToTree(e.x)))
  if (e.f.tag === 'K') return fork(leaf, cirToTree(e.x))
  return normalize(app(cirToTree(e.f), cirToTree(e.x)))
}

/** Compile `{params} -> body` where body is built from vars/apps/trees. */
export function lam(params: string[], body: Cir): T {
  let cur = body
  for (let i = params.length - 1; i >= 0; i--) cur = { tag: 'lam', param: params[i], body: cur }
  return cirToTree(cur)
}
export const v = (name: string): Cir => ({ tag: 'var', name })
export const a = (...es: Cir[]): Cir => es.reduce((f, x) => ({ tag: 'app', f, x }))
export const tr = (tree: T): Cir => ({ tag: 'tree', tree })
/** A cir-level lambda, for lambdas nested inside a larger lam() body. */
export const cl = (params: string[], body: Cir): Cir =>
  params.reduceRight((b, p) => ({ tag: 'lam', param: p, body: b }), body)

// ---- named trees (kept aligned with the CURRENT lib encodings) ------------

// disp's booleans today: true = t, false = t t (lib/prelude.disp)
const TRUE = leaf
const FALSE = stem(leaf)

// triage-built not: true(leaf) -> false; false(stem u) -> true
const NOT = fork(fork(FALSE, fork(leaf, TRUE)), fork(leaf, fork(leaf, TRUE)))

// K x as a tree (fork(leaf, x), by the fork-construction rule)
const kOf = (x: T): T => fork(leaf, x)

// and: triage on a — true(leaf) -> b (the identity), false(stem) -> false
const AND = fork(fork(I_TREE, kOf(kOf(FALSE))), kOf(kOf(FALSE)))
// or: true(leaf) -> true regardless, false(stem) -> b
const OR = fork(fork(kOf(TRUE), kOf(I_TREE)), kOf(kOf(I_TREE)))

// recursion the tree-calculus way (the walkthrough's construction):
//   wait a b c = a b c, but wait a b does NOT evaluate a b
//   fix f = wait m (λx. f (wait m x))   with m = λx. x x
const WAIT = lam(
  ['a', 'b', 'c'],
  a(tr(leaf), a(tr(leaf), v('a')), a(tr(leaf), tr(leaf), v('c')), v('b'))
)
const MFIX = lam(['x'], a(v('x'), v('x')))
const FIX = lam(
  ['f'],
  a(tr(WAIT), tr(MFIX), cl(['x'], a(v('f'), a(tr(WAIT), tr(MFIX), v('x')))))
)
// add n m: triage on n — zero(leaf) -> m; succ(fork t pred) -> succ (add pred m)
// (succ IS K at the tree level: K x = fork(leaf, x)). Same equations as the
// lib's add, compiled small so the storm stays watchable: the recursive body
// takes only the deferred self and RETURNS the triage fork directly (no λn —
// the dispatcher's own F rule consumes n), a K wrap eats triage's junk leaf
// where λl would, and `self p m` η-collapses under bracket abstraction.
// 89 nodes vs 160 for the λself λn λm spelling; peak mid-reduction ~880 vs
// ~3500 on add 2 3.
const ADD = normalize(
  app(
    FIX,
    lam(
      ['s'],
      a(
        tr(leaf),
        a(tr(leaf), tr(I_TREE), tr(leaf)),
        a(tr(K_TREE), cl(['p', 'm'], a(tr(K_TREE), a(v('s'), v('p'), v('m')))))
      )
    )
  )
)

export const DEFS: Record<string, T> = {
  K: K_TREE,
  I: I_TREE,
  S: S_TREE,
  true: TRUE,
  false: FALSE,
  not: NOT,
  and: AND,
  or: OR,
  add: ADD
}

// ---- parser: t, △, (), numbers, names ------------------------------------

// `lazyTop` keeps the OUTERMOST application from eager-constructing, so a bare
// leaf- or stem-headed program shows its construction rule fire as a real step:
// `t x` materializes as (△ · x) and fires the leaf rule (△) into a stem, and
// `t x y` builds the stem `t x` eagerly then fires the fork rule on the last
// application — the stem/fork rules on their own, no K needed to expose them.
export function parseTree(
  input: string,
  defs: Record<string, T> = DEFS,
  opts: { lazyTop?: boolean } = {}
): T {
  const tokens: string[] = []
  for (let i = 0; i < input.length; i++) {
    const ch = input[i]
    if (/\s/.test(ch)) continue
    if (ch === '(' || ch === ')') {
      tokens.push(ch)
      continue
    }
    if (ch === '△') {
      tokens.push('t')
      continue
    }
    if (/[A-Za-z_]/.test(ch)) {
      let j = i
      while (j < input.length && /[A-Za-z_0-9]/.test(input[j])) j++
      tokens.push(input.slice(i, j))
      i = j - 1
      continue
    }
    if (/[0-9]/.test(ch)) {
      let j = i
      while (j < input.length && /[0-9]/.test(input[j])) j++
      tokens.push(input.slice(i, j))
      i = j - 1
      continue
    }
    throw new Error('unexpected character: ' + ch)
  }
  let p = 0
  function atom(): T {
    const tok = tokens[p]
    if (tok === '(') {
      p++
      const e = expr(false) // parenthesized groups always construct eagerly
      if (tokens[p] !== ')') throw new Error('expected )')
      p++
      return e
    }
    if (tok === undefined || tok === ')') throw new Error('expected an atom, got ' + (tok ?? 'end of input'))
    if (tok === 't') {
      p++
      return leaf
    }
    if (/^[0-9]+$/.test(tok)) {
      p++
      return nat(parseInt(tok, 10))
    }
    if (Object.prototype.hasOwnProperty.call(defs, tok)) {
      p++
      return structuredClone(defs[tok])
    }
    // any other name is a named leaf — a free variable to compute with
    // symbolically (K x y ▷ x, S f g x ▷ (f x)(g x))
    p++
    return varLeaf(tok)
  }
  function expr(isTop: boolean): T {
    // Partial applications of S denote VALUES: `S a` is entered directly as
    // its normal form t (t a) — the same pre-assembly bracket abstraction
    // performs — so the widget shows ONE S firing at saturation instead of
    // the partial application constructing itself first. (K needs no such
    // help: `K a` is a silent stem→fork construction already.) After the
    // first argument, the ordinary construction rules keep the chain silent:
    // t (t a) b forks to t (t a) b, and the third argument materializes the
    // one visible redex.
    const sHead =
      tokens[p] === 'S' &&
      Object.prototype.hasOwnProperty.call(defs, 'S') &&
      treeEq(defs.S, S_TREE)
    let e = atom()
    let sPending = sHead
    while (p < tokens.length && tokens[p] !== ')') {
      const arg = atom()
      const isLast = p >= tokens.length || tokens[p] === ')'
      if (sPending) {
        e = stem(stem(arg))
        sPending = false
      } else if (opts.lazyTop && isTop && isLast) {
        // hold the last top-level application unreduced so its construction
        // rule (leaf → stem, or stem → fork) fires as a visible step
        e = { tag: 'apply', f: e, x: arg }
      } else {
        e = app(e, arg)
      }
    }
    return e
  }
  const r = expr(true)
  if (p !== tokens.length) throw new Error('extra tokens after expression')
  return r
}

// ---- pretty + layout -------------------------------------------------------

export function nodeCount(t: T): number {
  return 1 + childrenOf(t).reduce((s, c) => s + nodeCount(c), 0)
}

export function pretty(t: T, opts: { names?: boolean } = { names: true }): string {
  if (opts.names) {
    const n = natValue(t)
    if (n !== null && n > 0) return String(n)
    // only name substantial trees — tiny ones are ambiguous on purpose
    // (today's encodings make false, K, and t t the SAME tree)
    if (nodeCount(t) >= 3) {
      for (const [name, d] of Object.entries(DEFS)) if (treeEq(t, d)) return name
    }
  }
  const go = (t: T, atomPos: boolean): string => {
    if (t.tag === 'leaf') return 't'
    if (t.tag === 'var') return t.name
    const inner =
      t.tag === 'stem'
        ? `t ${go(t.c, true)}`
        : t.tag === 'fork'
          ? `t ${go(t.l, true)} ${go(t.r, true)}`
          : `${go(t.f, false)} · ${go(t.x, true)}`
    return atomPos ? `(${inner})` : inner
  }
  return go(t, false)
}

export interface LaidNode {
  tree: T
  x: number
  depth: number
}

// Tidy layout: leaves get unit widths, parents center over their children.
export function layoutTidy(tree: T, depth = 0): { nodes: LaidNode[]; rootX: number; width: number } {
  const children = childrenOf(tree)
  if (children.length === 0) return { nodes: [{ tree, x: 0, depth }], rootX: 0, width: 1 }
  let cumX = 0
  const all: LaidNode[] = []
  let firstRoot: number | null = null
  let lastRoot = 0
  for (const c of children) {
    const sub = layoutTidy(c, depth + 1)
    for (const n of sub.nodes) all.push({ tree: n.tree, x: n.x + cumX, depth: n.depth })
    const r = sub.rootX + cumX
    if (firstRoot === null) firstRoot = r
    lastRoot = r
    cumX += sub.width
  }
  const myX = ((firstRoot ?? 0) + lastRoot) / 2
  all.push({ tree, x: myX, depth })
  return { nodes: all, rootX: myX, width: Math.max(cumX, 1) }
}
