// Surface → tree calculus. Grammar is documented in SYNTAX.typ; this file
// implements it with a tiny parser-combinator library, then walks the parsed
// AST (the driver) to resolve scopes, inline `use`d files, and compile terms
// via bracket abstraction.
//
// Sections:
//   1. Tokens + tokenizer
//   2. AST types  (SIR untyped, Surface typed, Item declarations)
//   3. Parser combinators
//   4. Grammar productions
//   5. Bracket abstraction (SIR → Tree)
//   6. Driver (scope stack, `use`, typed defs, produces Decl[])

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import {
  Tree, LEAF, stem, fork, applyTree, treeEqual, isFork, prettyTree, FAST_EQ,
} from "./tree.js"
import {
  elab as elabSurface, type Surface, substituteMetas, decodeMetaSolutions,
} from "./elaborate.js"

export type { Surface }

// ───────────────────────────── 1. Tokens ─────────────────────────────────

export type Tok =
  | { t: "id"; v: string }
  | { t: "punct"; v: string }
  | { t: "kw"; v: string }
  | { t: "leaf" }
  | { t: "str"; v: string }
  | { t: "eof" }

const KEYWORDS = new Set(["def", "test", "elab", "raw", "use"])
// Order matters: longer punctuation first so "->" isn't chopped into "-" ">".
const PUNCT = ["->", "→", "\\", ".", "(", ")", "=", ":", "{", "}"] as const
const IDENT_HEAD = /[A-Za-z_]/
const IDENT_TAIL = /[A-Za-z0-9_']/

export function tokenize(src: string): Tok[] {
  const toks: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    if (/\s/.test(c)) { i++; continue }
    if (c === ";") { while (i < src.length && src[i] !== "\n") i++; continue }
    if (c === '"') {
      const j = src.indexOf('"', i + 1)
      if (j < 0) throw new Error(`tokenize: unterminated string at offset ${i}`)
      toks.push({ t: "str", v: src.slice(i + 1, j) })
      i = j + 1; continue
    }
    if (c === "△") { toks.push({ t: "leaf" }); i++; continue }
    // Bare `t` (not followed by an ident char) is the leaf; otherwise an ident.
    if (c === "t" && !(i + 1 < src.length && IDENT_TAIL.test(src[i + 1]))) {
      toks.push({ t: "leaf" }); i++; continue
    }
    const p = PUNCT.find(p => src.startsWith(p, i))
    if (p) { toks.push({ t: "punct", v: p }); i += p.length; continue }
    if (IDENT_HEAD.test(c)) {
      let j = i + 1
      while (j < src.length && IDENT_TAIL.test(src[j])) j++
      const word = src.slice(i, j)
      toks.push(KEYWORDS.has(word) ? { t: "kw", v: word } : { t: "id", v: word })
      i = j; continue
    }
    throw new Error(`tokenize: unexpected ${JSON.stringify(c)} at offset ${i}`)
  }
  toks.push({ t: "eof" })
  return toks
}

// ──────────────────────────── 2. AST types ───────────────────────────────

export type Sir =
  | { tag: "leaf" }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Sir; x: Sir }
  | { tag: "lam"; x: string; body: Sir }

export type Item =
  | { tag: "def"; name: string; expr: Sir }
  | { tag: "deftyped"; name: string; type: Surface; expr: Surface }
  | { tag: "elab"; name: string; expr: Surface }
  | { tag: "test"; lhs: Sir; rhs: Sir }
  | { tag: "use"; path: string }
  | { tag: "block"; items: Item[] }

// ───────────────────────── 3. Parser combinators ─────────────────────────

type Pos = number
type Ok<T>  = { ok: true;  v: T; pos: Pos }
type Err    = { ok: false; msg: string; pos: Pos }
type Res<T> = Ok<T> | Err
type P<T>   = (toks: Tok[], pos: Pos) => Res<T>

const ok  = <T>(v: T, pos: Pos): Res<T> => ({ ok: true, v, pos })
const err = (msg: string, pos: Pos): Res<never> => ({ ok: false, msg, pos })

const map = <A, B>(p: P<A>, f: (a: A) => B): P<B> =>
  (ts, i) => { const r = p(ts, i); return r.ok ? ok(f(r.v), r.pos) : r }

// seq is variadic with positional destructuring; downstream code uses
// tuple patterns like ([, x, , T, , , body]) => ...
const seq = <Ps extends P<unknown>[]>(...ps: Ps): P<{ [K in keyof Ps]: Ps[K] extends P<infer U> ? U : never }> =>
  (ts, i) => {
    const out: unknown[] = []
    let pos = i
    for (const p of ps) {
      const r = p(ts, pos)
      if (!r.ok) return r
      out.push(r.v); pos = r.pos
    }
    return ok(out as never, pos)
  }

// alt returns the first alternative that succeeds; on all-failure, returns
// the failure that got deepest (most informative error).
const alt = <T>(...ps: P<T>[]): P<T> =>
  (ts, i) => {
    let deepest: Err = { ok: false, msg: "no alternative matched", pos: i }
    for (const p of ps) {
      const r = p(ts, i)
      if (r.ok) return r
      if (r.pos >= deepest.pos) deepest = r
    }
    return deepest
  }

const many = <T>(p: P<T>): P<T[]> =>
  (ts, i) => {
    const out: T[] = []
    let pos = i
    for (;;) {
      const r = p(ts, pos)
      if (!r.ok) return ok(out, pos)
      out.push(r.v); pos = r.pos
    }
  }

const many1 = <T>(p: P<T>): P<T[]> =>
  (ts, i) => {
    const r = p(ts, i)
    if (!r.ok) return r
    const rest = many(p)(ts, r.pos)
    return rest.ok ? ok([r.v, ...rest.v], rest.pos) : rest
  }

const optional = <T>(p: P<T>): P<T | null> =>
  (ts, i) => { const r = p(ts, i); return r.ok ? r : ok(null, i) }

const lazy = <T>(f: () => P<T>): P<T> => (ts, i) => f()(ts, i)

const tokP = (pred: (t: Tok) => boolean, label: string): P<Tok> =>
  (ts, i) => pred(ts[i]) ? ok(ts[i], i + 1) : err(`expected ${label}, got ${describe(ts[i])}`, i)

const describe = (t: Tok): string => {
  switch (t.t) {
    case "id": return `identifier '${t.v}'`
    case "kw": return `keyword '${t.v}'`
    case "punct": return `'${t.v}'`
    case "str": return `string "${t.v}"`
    case "leaf": return "leaf"
    case "eof": return "end of input"
  }
}

const punctP = (v: string): P<Tok> => tokP(t => t.t === "punct" && t.v === v, `'${v}'`)
const kwP    = (v: string): P<Tok> => tokP(t => t.t === "kw" && t.v === v, `'${v}'`)
const idP:     P<string>  = map(tokP(t => t.t === "id", "identifier"), t => (t as Tok & {v: string}).v)
const leafP:   P<Tok>     = tokP(t => t.t === "leaf", "leaf")
const strP:    P<string>  = map(tokP(t => t.t === "str", "string literal"), t => (t as Tok & {v: string}).v)
const arrowP:  P<Tok>     = alt(punctP("->"), punctP("→"))

// Shorthand constructors used in grammar productions.
const inParens = <T>(p: P<T>): P<T> => map(seq(punctP("("), p, punctP(")")), ([, v]) => v)
const inBraces = <T>(p: P<T>): P<T> => map(seq(punctP("{"), p, punctP("}")), ([, v]) => v)

// ───────────────────────── 4. Grammar productions ────────────────────────

// Untyped term grammar ---------------------------------------------------

const atom: P<Sir> = lazy(() => alt<Sir>(
  inParens(term),
  map(leafP, () => ({ tag: "leaf" })),
  map(idP,   name => ({ tag: "var", name })),
))

const app: P<Sir> = map(
  seq(atom, many(atom)),
  ([h, xs]) => xs.reduce<Sir>((f, x) => ({ tag: "app", f, x }), h),
)

const lambda: P<Sir> = map(
  seq(punctP("\\"), many1(idP), punctP("."), lazy(() => term)),
  ([, ns, , body]) => ns.reduceRight<Sir>((b, n) => ({ tag: "lam", x: n, body: b }), body),
)

const term: P<Sir> = alt(lambda, app)

// Typed term grammar -----------------------------------------------------

const rawEscape: P<Surface> = map(
  seq(kwP("raw"), inParens(term)),
  ([, sir]) => ({ tag: "raw", tree: compileSir(sir) }),
)

const typedAtom: P<Surface> = lazy(() => alt<Surface>(
  rawEscape,
  inParens(typed),
  map(leafP, () => ({ tag: "leaf" })),
  map(idP,   name => name === "_" ? { tag: "hole" } : { tag: "var", name }),
))

const typedApp: P<Surface> = map(
  seq(typedAtom, many(typedAtom)),
  ([h, xs]) => xs.reduce<Surface>((f, x) => ({ tag: "app", f, x }), h),
)

// `arrow` covers both the non-dependent case `A -> B` and the fall-through
// where we just have a `tapp`. Dependent Pi is handled separately by `pi`.
const arrow: P<Surface> = map(
  seq(typedApp, optional(seq(arrowP, lazy(() => typed)))),
  ([left, tail]) => tail ? { tag: "arrow", dom: left, cod: tail[1] } : left,
)

const pi: P<Surface> = map(
  seq(
    punctP("("), idP, punctP(":"), lazy(() => typed), punctP(")"),
    arrowP, lazy(() => typed),
  ),
  ([, x, , dom, , , cod]) => ({ tag: "pi", x, dom, cod }),
)

const typedLam: P<Surface> = map(
  seq(
    punctP("\\"), punctP("("), idP, punctP(":"), lazy(() => typed), punctP(")"),
    punctP("."), lazy(() => typed),
  ),
  ([, , x, , ty, , , body]) => ({ tag: "alam", x, type: ty, body }),
)

const typed: P<Surface> = alt(typedLam, pi, arrow)

// Items (top-level declarations) -----------------------------------------

const useItem: P<Item> = map(
  seq(kwP("use"), strP),
  ([, path]) => ({ tag: "use", path }),
)

// def NAME ( : T = T | = T )
const defItem: P<Item> = map(
  seq(
    kwP("def"), idP,
    alt<Item>(
      // def NAME : T = E
      map(
        seq(punctP(":"), typed, punctP("="), typed),
        ([, T, , E]) => ({ tag: "deftyped", name: "", type: T, expr: E }),
      ),
      // def NAME = E
      map(
        seq(punctP("="), term),
        ([, E]) => ({ tag: "def", name: "", expr: E }),
      ),
    ),
  ),
  ([, name, item]) => ({ ...item, name }),
)

const elabItem: P<Item> = map(
  seq(kwP("elab"), idP, punctP("="), typed),
  ([, name, , expr]) => ({ tag: "elab", name, expr }),
)

const testItem: P<Item> = map(
  seq(kwP("test"), term, punctP("="), term),
  ([, lhs, , rhs]) => ({ tag: "test", lhs, rhs }),
)

const blockItem: P<Item> = map(
  inBraces(many(lazy(() => item))),
  items => ({ tag: "block", items }),
)

const item: P<Item> = alt(blockItem, useItem, defItem, testItem, elabItem)

const program: P<Item[]> = many(item)

// Parse a source string into raw items. Pure — no IO, no scope resolution.
// `use` paths are preserved as strings; the driver resolves them.
export function parseItems(src: string): Item[] {
  const toks = tokenize(src)
  const r = program(toks, 0)
  if (!r.ok) throw new Error(`parse: ${r.msg}`)
  if (toks[r.pos].t !== "eof") {
    throw new Error(`parse: unexpected trailing ${describe(toks[r.pos])}`)
  }
  return r.v
}

// ──────────────────────── 5. Bracket abstraction ─────────────────────────

// CIR: SIR with explicit S/K/I sentinels. Bracket abstraction works here;
// cirToTree pattern-matches the sentinels into fork/stem shapes at the end.
type Cir =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Cir; x: Cir }
  | { tag: "lam"; x: string; body: Cir }
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

const S: Cir = { tag: "S" }
const K: Cir = { tag: "K" }
const I: Cir = { tag: "I" }
const cap = (f: Cir, x: Cir): Cir => ({ tag: "app", f, x })

const I_TREE = fork(fork(LEAF, LEAF), LEAF)
const K_TREE = stem(LEAF)
// S-tree derivation:
//   apply(S_TREE, c)           = stem(stem(c))         (S-rule via (S K_LEAF LEAF))
//   apply(stem(stem(c)), b)    = fork(stem(c), b)      (stem rule)
//   apply(fork(stem(c), b), x) = apply(apply(c,x), apply(b,x))   (S-rule)
const S_TREE = fork(stem(fork(LEAF, LEAF)), LEAF)

function containsFree(e: Cir, name: string): boolean {
  switch (e.tag) {
    case "lit": case "S": case "K": case "I": return false
    case "var": return e.name === name
    case "app": return containsFree(e.f, name) || containsFree(e.x, name)
    case "lam": return e.x === name ? false : containsFree(e.body, name)
  }
}

function abstractName(name: string, body: Cir): Cir {
  if (!containsFree(body, name)) return cap(K, body)
  switch (body.tag) {
    case "var": return I
    case "app": return cap(cap(S, abstractName(name, body.f)), abstractName(name, body.x))
    case "lam": return abstractName(name, abstractName(body.x, body.body))
    case "lit": case "S": case "K": case "I":
      throw new Error("abstract: unreachable (containsFree returned false)")
  }
}

function eliminateLams(e: Cir): Cir {
  switch (e.tag) {
    case "lit": case "var": case "S": case "K": case "I": return e
    case "app": return cap(eliminateLams(e.f), eliminateLams(e.x))
    case "lam": return abstractName(e.x, eliminateLams(e.body))
  }
}

function cirToTree(e: Cir): Tree {
  switch (e.tag) {
    case "lit": return e.t
    case "var": throw new Error(`cirToTree: unresolved free variable ${e.name}`)
    case "I":   return I_TREE
    case "K":   return K_TREE
    case "S":   return S_TREE
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return fork(stem(cirToTree(e.f.x)), cirToTree(e.x))
      if (e.f.tag === "K") return fork(LEAF, cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      return applyTree(cirToTree(e.f), cirToTree(e.x), 10_000_000)
    }
  }
}

// SIR → CIR with globals from `scope` inlined as literals. Unknown names are
// left as `var` nodes; abstractName will eliminate them if they're bound by
// an enclosing lam, or cirToTree will throw if they're genuinely free.
function sirToCir(e: Sir, lookup: (name: string) => Tree | undefined): Cir {
  switch (e.tag) {
    case "leaf": return { tag: "lit", t: LEAF }
    case "var": {
      const t = lookup(e.name)
      return t ? { tag: "lit", t } : { tag: "var", name: e.name }
    }
    case "app": return { tag: "app", f: sirToCir(e.f, lookup), x: sirToCir(e.x, lookup) }
    case "lam": return { tag: "lam", x: e.x, body: sirToCir(e.body, lookup) }
  }
}

// Compilation context pointer — the parser-combinator-land `raw (EXPR)`
// production needs to compile the SIR inside. We set this to the current
// driver state before parsing each item so `rawEscape` can see live scope.
let currentLookup: (name: string) => Tree | undefined = () => undefined
const compileSir = (e: Sir): Tree => cirToTree(eliminateLams(sirToCir(e, currentLookup)))

// ──────────────────────────── 6. Driver ─────────────────────────────────

export type Decl =
  | { kind: "Def"; name: string; tree: Tree }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export function parseProgram(src: string, sourcePath?: string): Decl[] {
  // Scope stack: frame 0 holds built-ins. Each `{` pushes a new frame.
  // Name lookup walks inner-to-outer.
  const stack: Map<string, Tree>[] = [new Map([["fast_eq", FAST_EQ]])]
  const decls: Decl[] = []
  const dirStack = [sourcePath ? dirname(pathResolve(sourcePath)) : process.cwd()]

  const lookup = (name: string): Tree | undefined => {
    for (let i = stack.length - 1; i >= 0; i--) {
      const t = stack[i].get(name)
      if (t !== undefined) return t
    }
    return undefined
  }
  const define = (name: string, tree: Tree) => stack[stack.length - 1].set(name, tree)
  const flat = (): Map<string, Tree> => {
    const m = new Map<string, Tree>()
    for (const frame of stack) for (const [k, v] of frame) m.set(k, v)
    return m
  }

  function runItem(it: Item): void {
    // Make current scope visible to `raw (EXPR)` inside any typed item we
    // parse while processing sub-items (e.g. a `use`d file).
    currentLookup = lookup
    switch (it.tag) {
      case "def": {
        const tree = compileSir(it.expr)
        define(it.name, tree)
        decls.push({ kind: "Def", name: it.name, tree })
        return
      }
      case "deftyped": {
        const env = flat()
        const typeTree = elabSurface(it.type, env)
        const exprTree = elabSurface(it.expr, env)
        const tree = runTypedCheck(it.name, typeTree, exprTree, lookup)
        define(it.name, tree)
        decls.push({ kind: "Def", name: it.name, tree })
        return
      }
      case "elab": {
        const tree = elabSurface(it.expr, flat())
        define(it.name, tree)
        decls.push({ kind: "Def", name: it.name, tree })
        return
      }
      case "test": {
        decls.push({
          kind: "Test",
          lhs: compileSir(it.lhs),
          rhs: compileSir(it.rhs),
        })
        return
      }
      case "use": {
        const abs = pathResolve(dirStack[dirStack.length - 1], it.path)
        const src = readFileSync(abs, "utf-8")
        dirStack.push(dirname(abs))
        try {
          for (const sub of parseItems(src)) runItem(sub)
        } finally {
          dirStack.pop()
        }
        return
      }
      case "block": {
        stack.push(new Map())
        try {
          for (const sub of it.items) runItem(sub)
        } finally {
          stack.pop()
        }
        return
      }
    }
  }

  for (const it of parseItems(src)) runItem(it)
  return decls
}

function runTypedCheck(
  name: string,
  typeTree: Tree,
  exprTree: Tree,
  lookup: (name: string) => Tree | undefined,
): Tree {
  const predOfLvl = lookup("pred_of_lvl")
  const stateInit = lookup("state_init")
  const ctxNone   = lookup("ctx_none")
  if (!predOfLvl || !stateInit || !ctxNone) {
    throw new Error(
      `def ${name}: typed def needs 'pred_of_lvl', 'ctx_none', and 'state_init' in scope`,
    )
  }
  // pred_of_lvl : ty → tyCtx → cand → candCtx → state → (bool, state')
  const res = applyTree(
    applyTree(applyTree(applyTree(applyTree(predOfLvl, typeTree), ctxNone), exprTree), ctxNone),
    stateInit,
    10_000_000,
  )
  if (!isFork(res)) throw new Error(`def ${name}: pred_of_lvl returned non-tuple ${prettyTree(res)}`)
  const okBit = res.left
  const finalState = res.right
  if (!treeEqual(okBit, LEAF)) {
    throw new Error(`def ${name}: type check failed (got ${prettyTree(okBit)})`)
  }
  const metasList = isFork(finalState) ? finalState.right : LEAF
  const solutions = decodeMetaSolutions(metasList)
  return solutions.size > 0 ? substituteMetas(exprTree, solutions) : exprTree
}
