// Surface → tree calculus. Grammar is documented in SYNTAX.typ; this file
// implements it with a tiny parser-combinator library, then walks the parsed
// AST (the driver) to resolve scopes, inline `use`d files, and compile terms
// via bracket abstraction.
//
// Sections:
//   1. Tokens + tokenizer
//   2. AST types
//   3. Parser combinators
//   4. Grammar productions
//   5. Bracket abstraction (Expr → Cir → Tree)
//   6. Driver (scope stack, `use`, produces Decl[])

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import {
  Tree, LEAF, stem, fork, applyTree, FAST_EQ, getApplyStats, type ApplyStats,
} from "./tree.js"

// ───────────────────────────── 1. Tokens ─────────────────────────────────

export type Tok =
  | { t: "id"; v: string }
  | { t: "punct"; v: string }
  | { t: "kw"; v: string }
  | { t: "leaf" }
  | { t: "str"; v: string }
  | { t: "nl" }
  | { t: "eof" }

const KEYWORDS = new Set(["let", "test", "use", "open"])
// Order matters: longer punctuation first so ":=" isn't chopped into ":" "=".
const PUNCT = [":=", "->", "→", ".", ",", ";", "(", ")", "=", ":", "{", "}"] as const
const IDENT_HEAD = /[A-Za-z_]/
const IDENT_TAIL = /[A-Za-z0-9_']/

export function tokenize(src: string): Tok[] {
  const toks: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    // Newlines are significant (SEMI/COMMA separator).
    if (c === "\n") { toks.push({ t: "nl" }); i++; continue }
    if (/[ \t\r]/.test(c)) { i++; continue }
    // Line comment: //
    if (c === "/" && i + 1 < src.length && src[i + 1] === "/") {
      while (i < src.length && src[i] !== "\n") i++
      continue
    }
    // Block comment: /* */
    if (c === "/" && i + 1 < src.length && src[i + 1] === "*") {
      i += 2
      while (i + 1 < src.length && !(src[i] === "*" && src[i + 1] === "/")) i++
      if (i + 1 >= src.length) throw new Error(`tokenize: unterminated block comment`)
      i += 2; continue
    }
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

export type Expr =
  | { tag: "leaf" }
  | { tag: "var"; name: string }
  | { tag: "hole" }
  | { tag: "app"; f: Expr; x: Expr }
  | { tag: "binder"; params: Param[]; body: Expr }
  | { tag: "ann"; expr: Expr; type: Expr }
  | { tag: "proj"; target: Expr; field: string }
  | { tag: "recType"; fields: TypedField[] }
  | { tag: "recValue"; fields: NamedField[] }
  | { tag: "use"; path: string }

export type Param = { name: string | null; type: Expr | null }
export type TypedField = { name: string; type: Expr }
export type NamedField = { name: string; type: Expr | null; value: Expr }

export type Item =
  | { tag: "let"; name: string; type: Expr | null; body: Expr }
  | { tag: "test"; lhs: Expr; rhs: Expr }
  | { tag: "open"; expr: Expr }

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
    case "nl": return "newline"
    case "eof": return "end of input"
  }
}

const punctP = (v: string): P<Tok> => tokP(t => t.t === "punct" && t.v === v, `'${v}'`)
const kwP    = (v: string): P<Tok> => tokP(t => t.t === "kw" && t.v === v, `'${v}'`)
const idP:     P<string>  = map(tokP(t => t.t === "id", "identifier"), t => (t as Tok & {v: string}).v)
const leafP:   P<Tok>     = tokP(t => t.t === "leaf", "leaf")
const strP:    P<string>  = map(tokP(t => t.t === "str", "string literal"), t => (t as Tok & {v: string}).v)
const arrowP:  P<Tok>     = alt(punctP("->"), punctP("→"))

// Skip newlines (used where newlines are insignificant, e.g. inside parens/braces).
const skipNl: P<null> = (ts, i) => {
  while (ts[i].t === "nl") i++
  return ok(null, i)
}

// Wrap a parser to skip leading newlines.
const nl = <T>(p: P<T>): P<T> => (ts, i) => {
  while (ts[i].t === "nl") i++
  return p(ts, i)
}

// ───────────────────────── 4. Grammar productions ────────────────────────

// --- Separator parsers ---

// sepBy1(p, sep): one or more `p` separated by `sep`, optional trailing sep.
function sepBy1<T>(p: P<T>, sep: P<unknown>): P<T[]> {
  return (ts, i) => {
    const first = p(ts, i)
    if (!first.ok) return first
    const out: T[] = [first.v]
    let pos = first.pos
    for (;;) {
      const s = sep(ts, pos)
      if (!s.ok) return ok(out, pos)
      const next = p(ts, s.pos)
      if (!next.ok) return ok(out, pos) // trailing separator
      out.push(next.v); pos = next.pos
    }
  }
}

// SEMI = ";" | NEWLINE (consumes one or more)
const semiP: P<null> = (ts, i) => {
  if (ts[i].t === "nl" || (ts[i].t === "punct" && (ts[i] as any).v === ";")) {
    let pos = i + 1
    while (ts[pos].t === "nl" || (ts[pos].t === "punct" && (ts[pos] as any).v === ";")) pos++
    return ok(null, pos)
  }
  return err(`expected ';' or newline, got ${describe(ts[i])}`, i)
}

// COMMA = "," (optionally surrounded by newlines) | bare NEWLINE
const commaP: P<null> = (ts, i) => {
  let pos = i
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t === "punct" && (ts[pos] as any).v === ",") {
    pos++
    while (ts[pos].t === "nl") pos++
    return ok(null, pos)
  }
  if (pos > i) return ok(null, pos) // bare newline(s)
  return err(`expected ',' or newline, got ${describe(ts[i])}`, i)
}

// --- Expressions ---

// `_` as a bare token (single underscore, not `_foo`)
const holeP: P<Expr> = (ts, i) => {
  if (ts[i].t === "id" && (ts[i] as any).v === "_") return ok({ tag: "hole" }, i + 1)
  return err(`expected '_', got ${describe(ts[i])}`, i)
}

const simple: P<Expr> = lazy(() => alt<Expr>(
  // Parenthesized: ( expr ) or ( expr : expr )
  map(
    seq(punctP("("), skipNl, lazy(() => expr), nl(optional(seq(punctP(":"), skipNl, lazy(() => expr)))), nl(punctP(")"))),
    ([, , e, ann]) => ann ? { tag: "ann", expr: e, type: ann[2] } : e,
  ),
  // Braced: binder, recType, recValue
  lazy(() => braced),
  // use STRING
  map(seq(kwP("use"), strP), ([, path]): Expr => ({ tag: "use", path })),
  // leaf
  map(leafP, (): Expr => ({ tag: "leaf" })),
  // hole _
  holeP,
  // identifier
  map(idP, (name): Expr => ({ tag: "var", name })),
))

// atom = simple ("." IDENT)*
// Newlines before atoms are insignificant — expressions can span lines.
// Item separation works because keywords (let, test) can't start an atom.
const atom: P<Expr> = (ts, i) => {
  while (ts[i].t === "nl") i++
  const base = simple(ts, i)
  if (!base.ok) return base
  let result: Expr = base.v
  let pos = base.pos
  // Postfix projections (no newline skip before "." — binds tightly)
  while (ts[pos].t === "punct" && (ts[pos] as any).v === ".") {
    pos++
    const field = idP(ts, pos)
    if (!field.ok) return field
    result = { tag: "proj", target: result, field: field.v }
    pos = field.pos
  }
  return ok(result, pos)
}

// app = atom atom*
const app: P<Expr> = map(
  seq(atom, many(atom)),
  ([h, xs]) => xs.reduce<Expr>((f, x) => ({ tag: "app", f, x }), h),
)

// binderParam = (IDENT | "_") (":" expr)?
const binderParam: P<Param> = (ts, i) => {
  let pos = i
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t !== "id") return err(`expected identifier or '_', got ${describe(ts[pos])}`, pos)
  const v = (ts[pos] as any).v as string
  const name = v === "_" ? null : v
  pos++
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t === "punct" && (ts[pos] as any).v === ":") {
    pos++
    while (ts[pos].t === "nl") pos++
    const tyR = expr(ts, pos)
    if (!tyR.ok) return tyR
    return ok({ name, type: tyR.v }, tyR.pos)
  }
  return ok({ name, type: null }, pos)
}

// Parse binder contents after "{". Expects params, "}", ARROW, body.
const binderInner: P<Expr> = (ts, startPos) => {
  let pos = startPos
  while (ts[pos].t === "nl") pos++
  const params = sepBy1(binderParam, commaP)(ts, pos)
  if (!params.ok) return params
  pos = params.pos
  while (ts[pos].t === "nl") pos++
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === "}"))
    return err(`expected '}', got ${describe(ts[pos])}`, pos)
  pos++
  while (ts[pos].t === "nl") pos++
  const arr = arrowP(ts, pos)
  if (!arr.ok) return err(`expected '->' after '}', got ${describe(ts[pos])}`, pos)
  pos = arr.pos
  while (ts[pos].t === "nl") pos++
  const bodyR = expr(ts, pos)
  if (!bodyR.ok) return bodyR
  if (params.v.length === 0) return err("binder must have at least one param", startPos)
  return ok({ tag: "binder" as const, params: params.v, body: bodyR.v }, bodyR.pos)
}

// typedField = IDENT ":" expr
const typedFieldP: P<TypedField> = (ts, i) => {
  let pos = i
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t !== "id") return err(`expected identifier, got ${describe(ts[pos])}`, pos)
  const name = (ts[pos] as any).v as string
  if (name === "_") return err(`recType field cannot be '_'`, pos)
  pos++
  while (ts[pos].t === "nl") pos++
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === ":"))
    return err(`expected ':', got ${describe(ts[pos])}`, pos)
  pos++
  while (ts[pos].t === "nl") pos++
  const tyR = expr(ts, pos)
  if (!tyR.ok) return tyR
  return ok({ name, type: tyR.v }, tyR.pos)
}

// Parse recType contents after "{". Expects typed fields, "}".
const recTypeInner: P<Expr> = (ts, startPos) => {
  let pos = startPos
  while (ts[pos].t === "nl") pos++
  const fields = sepBy1(typedFieldP, commaP)(ts, pos)
  if (!fields.ok) return fields
  pos = fields.pos
  while (ts[pos].t === "nl") pos++
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === "}"))
    return err(`expected '}', got ${describe(ts[pos])}`, pos)
  pos++
  return ok({ tag: "recType" as const, fields: fields.v }, pos)
}

// namedField = IDENT (":" expr)? ":=" expr
const namedFieldP: P<NamedField> = (ts, i) => {
  let pos = i
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t !== "id") return err(`expected identifier, got ${describe(ts[pos])}`, pos)
  const name = (ts[pos] as any).v as string
  pos++
  while (ts[pos].t === "nl") pos++
  let type: Expr | null = null
  // ":" expr before ":="
  if (ts[pos].t === "punct" && (ts[pos] as any).v === ":") {
    pos++
    while (ts[pos].t === "nl") pos++
    const tyR = expr(ts, pos)
    if (!tyR.ok) return tyR
    type = tyR.v
    pos = tyR.pos
    while (ts[pos].t === "nl") pos++
  }
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === ":="))
    return err(`expected ':=', got ${describe(ts[pos])}`, pos)
  pos++
  while (ts[pos].t === "nl") pos++
  const valR = expr(ts, pos)
  if (!valR.ok) return valR
  return ok({ name, type, value: valR.v }, valR.pos)
}

// Parse recValue contents after "{". Expects named fields, "}".
const recValueInner: P<Expr> = (ts, startPos) => {
  let pos = startPos
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t === "punct" && (ts[pos] as any).v === "}") {
    return ok({ tag: "recValue" as const, fields: [] }, pos + 1)
  }
  const fields = sepBy1(namedFieldP, semiP)(ts, pos)
  if (!fields.ok) return fields
  pos = fields.pos
  // optional trailing semi
  const s = semiP(ts, pos)
  if (s.ok) pos = s.pos
  while (ts[pos].t === "nl") pos++
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === "}"))
    return err(`expected '}', got ${describe(ts[pos])}`, pos)
  pos++
  return ok({ tag: "recValue" as const, fields: fields.v }, pos)
}

// Disambiguate braced content after seeing "{".
// Peek at the first entry to determine: recValue, binder, or recTypeOrBinder.
const braced: P<Expr> = (ts, i) => {
  if (!(ts[i].t === "punct" && (ts[i] as any).v === "{"))
    return err(`expected '{', got ${describe(ts[i])}`, i)
  let pos = i + 1
  while (ts[pos].t === "nl") pos++

  // Empty braces → empty recValue
  if (ts[pos].t === "punct" && (ts[pos] as any).v === "}") {
    return ok({ tag: "recValue" as const, fields: [] }, pos + 1)
  }

  // Block: starts with "let" or "test"
  if (ts[pos].t === "kw" && ((ts[pos] as any).v === "let" || (ts[pos] as any).v === "test")) {
    return err(`block expressions are not yet supported`, pos)
  }

  const shape = classifyBracedContent(ts, pos)
  if (shape === "recValue") return recValueInner(ts, pos)
  if (shape === "binder") return binderInner(ts, pos)
  // recTypeOrBinder: try binder first (needs "}" then ARROW), fall back to recType.
  const binderAttempt = binderInner(ts, pos)
  if (binderAttempt.ok) return binderAttempt
  return recTypeInner(ts, pos)
}

// Peek at tokens after "{" to classify the braced content.
function classifyBracedContent(ts: Tok[], pos: number): "recValue" | "binder" | "recTypeOrBinder" {
  let p = pos
  while (ts[p].t === "nl") p++
  if (ts[p].t === "id") {
    const name = (ts[p] as any).v as string
    p++
    while (ts[p].t === "nl") p++
    if (name === "_") {
      if (ts[p].t === "punct" && (ts[p] as any).v === ":") return "recTypeOrBinder"
      return "binder"
    }
    if (ts[p].t === "punct" && ((ts[p] as any).v === "}" || (ts[p] as any).v === ",")) return "binder"
    if (ts[p].t === "punct" && (ts[p] as any).v === ":=") return "recValue"
    if (ts[p].t === "punct" && (ts[p] as any).v === ":") {
      // Scan for ":=" at this nesting depth (before "}", ",", ";")
      let depth = 0
      let q = p + 1
      while (q < ts.length && ts[q].t !== "eof") {
        if (ts[q].t === "punct") {
          const v = (ts[q] as any).v
          if (v === "(" || v === "{") depth++
          else if (v === ")" || v === "}") { if (depth === 0) break; depth-- }
          else if (depth === 0 && v === ":=") return "recValue"
          else if (depth === 0 && (v === "," || v === ";")) break
        }
        q++
      }
      return "recTypeOrBinder"
    }
  }
  return "binder"
}

// expr = binder | app (ARROW expr)?
// A standalone binder `{...} -> body` is parsed by `braced` inside `simple`.
// The ARROW suffix handles `A -> B` sugar.
const expr: P<Expr> = (ts, i) => {
  const lhs = app(ts, i)
  if (!lhs.ok) return lhs
  const arr = arrowP(ts, lhs.pos)
  if (!arr.ok) return lhs
  let pos = arr.pos
  while (ts[pos].t === "nl") pos++
  const rhs = expr(ts, pos)
  if (!rhs.ok) return rhs
  // A -> B desugars to binder with anonymous param typed A
  return ok(
    { tag: "binder" as const, params: [{ name: null, type: lhs.v }], body: rhs.v },
    rhs.pos,
  )
}

// --- Items ---

const letItem: P<Item> = map(
  seq(
    kwP("let"), idP,
    optional(seq(punctP(":"), skipNl, lazy(() => expr))),
    nl(punctP("=")), skipNl, lazy(() => expr),
  ),
  ([, name, ann, , , body]) => ({
    tag: "let" as const,
    name,
    type: ann ? ann[2] : null,
    body,
  }),
)

const testItem: P<Item> = map(
  seq(kwP("test"), lazy(() => expr), nl(punctP("=")), skipNl, lazy(() => expr)),
  ([, lhs, , , rhs]) => ({ tag: "test" as const, lhs, rhs }),
)

const openItem: P<Item> = map(
  seq(kwP("open"), lazy(() => expr)),
  ([, expr]) => ({ tag: "open" as const, expr }),
)

const itemP: P<Item> = nl(alt(openItem, letItem, testItem))

// Parse source into items.
export function parseItems(src: string): Item[] {
  const toks = tokenize(src)
  const items: Item[] = []
  let pos = 0
  while (toks[pos].t === "nl") pos++
  while (toks[pos].t !== "eof") {
    const r = itemP(toks, pos)
    if (!r.ok) throw new Error(`parse: ${r.msg}`)
    items.push(r.v)
    pos = r.pos
    while (toks[pos].t === "nl" || (toks[pos].t === "punct" && (toks[pos] as any).v === ";")) pos++
  }
  return items
}

// Parse a single expression.
export function parseExpr(src: string): Expr {
  const toks = tokenize(src)
  let pos = 0
  while (toks[pos].t === "nl") pos++
  const r = expr(toks, pos)
  if (!r.ok) throw new Error(`parse: ${r.msg}`)
  pos = r.pos
  while (toks[pos].t === "nl") pos++
  if (toks[pos].t !== "eof")
    throw new Error(`parse: unexpected trailing ${describe(toks[pos])}`)
  return r.v
}

// ──────────────────────── 5. Bracket abstraction ─────────────────────────

// CIR: intermediate representation with explicit S/K/I sentinels.
type Cir =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Cir; x: Cir }
  | { tag: "lam"; x: string; body: Cir }
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

const S: Cir = { tag: "S" }
const K: Cir = { tag: "K" }
const I_CIR: Cir = { tag: "I" }
const cap = (f: Cir, x: Cir): Cir => ({ tag: "app", f, x })

const I_TREE = fork(fork(LEAF, LEAF), LEAF)
const K_TREE = stem(LEAF)
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
    case "var": return I_CIR
    case "app": {
      // η-optimization: [x](f x) where x ∉ f → f
      if (body.x.tag === "var" && body.x.name === name && !containsFree(body.f, name))
        return body.f

      const af = abstractName(name, body.f)
      const ax = abstractName(name, body.x)

      // S (K p) I → p  (η-reduction)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "I") return af.x

      // S (K p) (K q) → K (p q)  (K-composition: compile-time eval)
      if (af.tag === "app" && af.f.tag === "K" && ax.tag === "app" && ax.f.tag === "K")
        return cap(K, cap(af.x, ax.x))

      return cap(cap(S, af), ax)
    }
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

// Build selector: \x0 x1 ... xn-1. xi (picks the i-th of n arguments)
function buildSelector(n: number, i: number): Cir {
  const names = Array.from({ length: n }, (_, j) => `__sel${j}`)
  let body: Cir = { tag: "var", name: names[i] }
  for (let j = n - 1; j >= 0; j--) {
    body = { tag: "lam", x: names[j], body }
  }
  return body
}

function selectorTree(n: number, i: number): Tree {
  return cirToTree(eliminateLams(buildSelector(n, i)))
}

// Scope entry: a compiled tree plus optional record field names for projection.
interface ScopeEntry { tree: Tree; fields?: string[] }

// Expr → Cir, with scope lookup and use-resolution.
function exprToCir(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
): Cir {
  const lookup = (name: string) => lookupEntry(name)?.tree
  switch (e.tag) {
    case "leaf": return { tag: "lit", t: LEAF }
    case "var": {
      const entry = lookupEntry(e.name)
      return entry ? { tag: "lit", t: entry.tree } : { tag: "var", name: e.name }
    }
    case "hole": throw new Error("hole '_' cannot appear in untyped compilation")
    case "app":
      return { tag: "app",
        f: exprToCir(e.f, lookupEntry, resolveUse),
        x: exprToCir(e.x, lookupEntry, resolveUse),
      }
    case "ann": return exprToCir(e.expr, lookupEntry, resolveUse) // erase type
    case "binder": {
      // Shadow binder params so they don't resolve to scope entries.
      const paramNames = new Set(e.params.map((p, i) => p.name ?? `_anon${i}`))
      const shadowedLookup = (name: string): ScopeEntry | undefined =>
        paramNames.has(name) ? undefined : lookupEntry(name)
      let body = exprToCir(e.body, shadowedLookup, resolveUse)
      for (let i = e.params.length - 1; i >= 0; i--) {
        const name = e.params[i].name ?? `_anon${i}`
        body = { tag: "lam", x: name, body }
      }
      return body
    }
    case "recType": throw new Error("recType cannot appear in untyped compilation")
    case "recValue": {
      // Church encoding: {x := a; y := b} → \sel. sel a b
      const fieldCirs = e.fields.map(f => exprToCir(f.value, lookupEntry, resolveUse))
      const selName = "__sel"
      let body: Cir = { tag: "var", name: selName }
      for (const fc of fieldCirs) body = cap(body, fc)
      return { tag: "lam", x: selName, body }
    }
    case "use": {
      const entry = resolveUse(e.path)
      return { tag: "lit", t: entry.tree }
    }
    case "proj": {
      // Compile target, then look up field index from target's field metadata.
      // Target must be a known record (var with fields, or use expression).
      const fieldNames = resolveExprFields(e.target, lookupEntry, resolveUse)
      if (!fieldNames)
        throw new Error(`projection '.${e.field}': target has no known record fields`)
      const idx = fieldNames.indexOf(e.field)
      if (idx < 0)
        throw new Error(`projection '.${e.field}': field not found (available: ${fieldNames.join(", ")})`)
      const target = exprToCir(e.target, lookupEntry, resolveUse)
      const sel = buildSelector(fieldNames.length, idx)
      return cap(target, sel)
    }
  }
}

// Resolve the field names of an expression (for projection).
function resolveExprFields(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
): string[] | undefined {
  if (e.tag === "var") return lookupEntry(e.name)?.fields
  if (e.tag === "use") return resolveUse(e.path).fields
  if (e.tag === "recValue") return e.fields.map(f => f.name)
  if (e.tag === "proj") {
    // Nested projection: target.field — would need the inner record's field type
    // which itself is a record. Not supported yet.
    return undefined
  }
  return undefined
}

function compileExpr(
  e: Expr,
  lookupEntry: (name: string) => ScopeEntry | undefined,
  resolveUse: (path: string) => ScopeEntry,
): Tree {
  return cirToTree(eliminateLams(exprToCir(e, lookupEntry, resolveUse)))
}

// ──────────────────────────── 6. Driver ─────────────────────────────────

export type Decl =
  | { kind: "Def"; name: string; tree: Tree }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export type ParseItemStats = {
  kind: "let" | "test" | "open"
  name?: string
  testIndex?: number
  sourcePath?: string
  depth: number
  stats: ApplyStats
}

export type ParseProgramOptions = {
  onItem?: (item: ParseItemStats) => void
}

export function parseProgram(src: string, sourcePath?: string, options: ParseProgramOptions = {}): Decl[] {
  const stack: Map<string, ScopeEntry>[] = [new Map([["fast_eq", { tree: FAST_EQ }]])]
  const decls: Decl[] = []
  const dirStack = [sourcePath ? dirname(pathResolve(sourcePath)) : process.cwd()]
  const sourceStack = [sourcePath ? pathResolve(sourcePath) : undefined]
  const loadedFiles = new Set<string>() // cycle detection
  let compiledTestIndex = 0

  const lookupEntry = (name: string): ScopeEntry | undefined => {
    for (let i = stack.length - 1; i >= 0; i--) {
      const e = stack[i].get(name)
      if (e !== undefined) return e
    }
    return undefined
  }
  const define = (name: string, entry: ScopeEntry) => stack[stack.length - 1].set(name, entry)

  function resolveUse(path: string): ScopeEntry {
    const abs = pathResolve(dirStack[dirStack.length - 1], path)
    if (loadedFiles.has(abs)) throw new Error(`use: circular dependency on ${abs}`)
    loadedFiles.add(abs)
    const fileSrc = readFileSync(abs, "utf-8")
    dirStack.push(dirname(abs))
    sourceStack.push(abs)
    // Push a new scope frame for the used file.
    stack.push(new Map())
    const fileDecls: Decl[] = []
    try {
      for (const it of parseItems(fileSrc)) {
        runItem(it, fileDecls)
      }
    } finally {
      const fileScope = stack.pop()!
      dirStack.pop()
      sourceStack.pop()
      loadedFiles.delete(abs)
    }
    // Collect the file's top-level defs as a record.
    const fieldNames: string[] = []
    const fieldTrees: Tree[] = []
    for (const d of fileDecls) {
      if (d.kind === "Def") {
        fieldNames.push(d.name)
        fieldTrees.push(d.tree)
      }
    }
    // Church-encode: \sel. sel v1 v2 ... vn
    const n = fieldTrees.length
    if (n === 0) return { tree: LEAF, fields: [] }
    // Build as Cir, then compile
    const selName = "__use_sel"
    let body: Cir = { tag: "var", name: selName }
    for (const ft of fieldTrees) body = cap(body, { tag: "lit", t: ft })
    const cir: Cir = { tag: "lam", x: selName, body }
    const tree = cirToTree(eliminateLams(cir))
    return { tree, fields: fieldNames }
  }

  function recordItem(kind: "let" | "test" | "open", name?: string, testIndex?: number): void {
    options.onItem?.({
      kind,
      name,
      testIndex,
      sourcePath: sourceStack[sourceStack.length - 1],
      depth: sourceStack.length - 1,
      stats: getApplyStats(),
    })
  }

  function runItem(it: Item, target: Decl[]): void {
    switch (it.tag) {
      case "let": {
        const tree = compileExpr(it.body, lookupEntry, resolveUse)
        // If the body is a use or recValue, track fields.
        const fields = resolveExprFields(it.body, lookupEntry, resolveUse)
        define(it.name, { tree, fields })
        target.push({ kind: "Def", name: it.name, tree })
        recordItem("let", it.name)
        return
      }
      case "test": {
        compiledTestIndex++
        target.push({
          kind: "Test",
          lhs: compileExpr(it.lhs, lookupEntry, resolveUse),
          rhs: compileExpr(it.rhs, lookupEntry, resolveUse),
        })
        recordItem("test", undefined, compiledTestIndex)
        return
      }
      case "open": {
        const tree = compileExpr(it.expr, lookupEntry, resolveUse)
        const fields = resolveExprFields(it.expr, lookupEntry, resolveUse)
        if (!fields || fields.length === 0)
          throw new Error("open: expression has no known record fields")
        const n = fields.length
        for (let i = 0; i < n; i++) {
          const fieldTree = applyTree(tree, selectorTree(n, i), 10_000_000)
          const name = fields[i]
          const existing = lookupEntry(name)
          if (existing) {
            // Idempotent: skip if same tree (e.g. diamond dependency via open)
            if (existing.tree.id === fieldTree.id) continue
            throw new Error(`open: name '${name}' already in scope with different value`)
          }
          define(name, { tree: fieldTree })
        }
        recordItem("open")
        return
      }
    }
  }

  for (const it of parseItems(src)) runItem(it, decls)
  return decls
}
