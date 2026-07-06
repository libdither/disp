// Surface grammar for Disp. Documented in SYNTAX.typ; this file
// implements it with a tiny parser-combinator library.
//
// Sections:
//   1. Tokens + tokenizer
//   2. AST types
//   3. Parser combinators
//   4. Grammar productions

// ───────────────────────────── 1. Tokens ─────────────────────────────────

export type Tok =
  | { t: "id"; v: string }
  | { t: "num"; v: number }
  | { t: "punct"; v: string }
  | { t: "kw"; v: string }
  | { t: "leaf" }
  | { t: "str"; v: string }
  | { t: "nl" }
  | { t: "eof" }

const KEYWORDS = new Set(["let", "test", "use", "open", "match", "if", "then", "else"])
// Order matters: longer punctuation first so ":=" isn't chopped into ":" "=".
// `<`/`>` are the sum-type-literal delimiters (`< Tag : T, … >`). They are single
// chars with no multi-char punctuation built on them, and `->`/`=>`/`→` are
// matched first, so a bare `>` never steals an arrow's tail.
const PUNCT = [":=", "=>", "->", "→", ".", ",", ";", "(", ")", "=", ":", "{", "}", "[", "]", "<", ">"] as const
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
    if (/[0-9]/.test(c)) {
      let j = i + 1
      while (j < src.length && /[0-9]/.test(src[j])) j++
      toks.push({ t: "num", v: Number(src.slice(i, j)) })
      i = j; continue
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
  | { tag: "num"; value: number }
  | { tag: "str"; value: string }
  | { tag: "var"; name: string }
  | { tag: "hole" }
  | { tag: "app"; f: Expr; x: Expr }
  | { tag: "binder"; params: Param[]; body: Expr }
  | { tag: "ann"; expr: Expr; type: Expr }
  | { tag: "proj"; target: Expr; field: string }
  | { tag: "recType"; fields: TypedField[] }
  // `< Tag1 : T1, Tag2, … >` — the coproduct (sum) type literal; the DUAL of
  // recType. Desugars (compile.ts) to `Coproduct [pair "Tag1" [T1], pair "Tag2" []]`.
  | { tag: "sumType"; variants: SumVariant[] }
  | { tag: "recValue"; fields: NamedField[]; members?: RecMember[]; trailing?: Expr }
  | { tag: "use"; path: string; raw?: boolean }
  // `if c then a else b` — the boolean conditional. Desugars to `cond` (a
  // select-then-apply over the Scott Bool, closure-wrapped per arm). The old
  // `match c { TT => a; FF => b }` boolean-match surface was removed in favour
  // of this; coproduct `match` (the §2.6 cut) desugars in compile.ts, where
  // arm bodies are closed over their free vars the way `if` branches are.
  | { tag: "if"; cond: Expr; thenBody: Expr; elseBody: Expr }
  | { tag: "match"; cond: Expr; arms: Arm[] }

// A binder parameter. `default` (a `:= expr` suffix) is the named-argument
// fallback: when a function declared `{x : A, y : B := d} -> …` is *named-called*
// `f { x := a }`, the omitted `y` falls back to `d`. The default is metadata for
// callers only — a plain (positional) compilation of the binder ignores it
// (the lambda is `{x} -> {y} -> …`); see compile.ts § named-argument resolution.
export type Param = { name: string | null; type: Expr | null; default?: Expr | null }
export type TypedField = { name: string; type: Expr | null; value?: Expr | null }
// A coproduct variant: `Tag : T` (single-arg, `type` set) or `Tag` (nullary, null).
export type SumVariant = { name: string; type: Expr | null }
export type NamedField = { name: string; type: Expr | null; value: Expr }

// Unified record body member — shared by file bodies and inline { ... } recValues.
// "field" (name := expr) is exported; "let" is private; "test"/"open" are side-effects.
export type RecMember =
  | { tag: "field"; name: string; type: Expr | null; value: Expr | null; pun?: boolean; head?: Expr }
  | { tag: "let"; name: string; type: Expr | null; body: Expr }
  | { tag: "test"; lhs: Expr; rhs: Expr }
  | { tag: "open"; expr: Expr }
// A "field" is a DECLARATION: `head? NAME (: T)? (:= v)?` (COMPILATION.typ
// § Declarations as requests). `head` is the optional request-decorator expression
// (e.g. `guard g`); `value` is null only for interface entries (`guard g X : T`),
// which the parser only produces when a head is present. Braced record members
// always carry a value and no head.

// (Item alias removed; callers now use RecMember directly.)

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
    case "num": return `number '${t.v}'`
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
const numP:    P<number>  = map(tokP(t => t.t === "num", "number"), t => (t as Tok & {v: number}).v)
const leafP:   P<Tok>     = tokP(t => t.t === "leaf", "leaf")
const strP:    P<string>  = map(tokP(t => t.t === "str", "string literal"), t => (t as Tok & {v: string}).v)
const arrowP:  P<Tok>     = alt(punctP("->"), punctP("→"))
// `raw` modifier on `use` — not a reserved keyword, so it tokenizes as an id.
const rawKwP:  P<Tok>     = tokP(t => t.t === "id" && t.v === "raw", "'raw'")

function duplicateName(fields: { name: string }[]): string | null {
  const seen = new Set<string>()
  for (const f of fields) {
    if (seen.has(f.name)) return f.name
    seen.add(f.name)
  }
  return null
}

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

// Postfix projections: base ("." IDENT)*  — no newline skip, binds tightly.
const withProj = (base: P<Expr>): P<Expr> => (ts, i) => {
  const r = base(ts, i)
  if (!r.ok) return r
  let result = r.v, pos = r.pos
  while (ts[pos].t === "punct" && (ts[pos] as any).v === ".") {
    pos++
    const field = idP(ts, pos)
    if (!field.ok) return field
    result = { tag: "proj", target: result, field: field.v }
    pos = field.pos
  }
  return ok(result, pos)
}

// simple/lineSimple differ only in which braced parser and which `if` atom
// they use (multi-line vs newline-terminated bodies — see ifP / lineIfP).
const makeSimple = (bracedP: () => P<Expr>, ifAtom: () => P<Expr>): P<Expr> => lazy(() => alt<Expr>(
  // Parenthesized: ( expr ) or ( expr : expr )
  map(
    seq(punctP("("), skipNl, lazy(() => expr), nl(optional(seq(punctP(":"), skipNl, lazy(() => expr)))), nl(punctP(")"))),
    ([, , e, ann]) => ann ? { tag: "ann", expr: e, type: ann[2] } : e,
  ),
  lazy(() => matchP),
  lazy(ifAtom),
  lazy(() => arrayP),
  lazy(() => sumTypeP),
  lazy(bracedP),
  map(seq(kwP("use"), optional(rawKwP), strP), ([, raw, path]): Expr => raw !== null ? { tag: "use", path, raw: true } : { tag: "use", path }),
  map(strP, (value): Expr => ({ tag: "str", value })),
  map(leafP, (): Expr => ({ tag: "leaf" })),
  map(numP, (value): Expr => ({ tag: "num", value })),
  holeP,
  map(idP, (name): Expr => ({ tag: "var", name })),
))
const simple: P<Expr> = makeSimple(() => braced, () => ifP)

// Check if position starts a field definition (IDENT ":=" or IDENT ":" ... ":=").
// Used to prevent atoms from consuming field starts after a newline.
function isFieldStart(ts: Tok[], i: number): boolean {
  if (ts[i].t !== "id") return false
  const next = ts[i + 1]
  if (next?.t === "punct" && (next as any).v === ":=") return true
  if (!(next?.t === "punct" && (next as any).v === ":")) return false
  let depth = 0, q = i + 2
  while (q < ts.length && ts[q].t !== "eof") {
    if (ts[q].t === "punct") {
      const v = (ts[q] as any).v
      if (v === "(" || v === "{") depth++
      else if (v === ")" || v === "}") { if (depth === 0) break; depth-- }
      else if (depth === 0 && v === ":=") return true
      else if (depth === 0 && (v === ";" || v === "=")) break
    }
    if (ts[q].t === "nl" && depth === 0) break
    q++
  }
  return false
}

// Generalization of isFieldStart to DECORATED declarations (`guard g X : T := v`),
// which have no leading `IDENT :=` signature: after a newline, a line whose top-level
// (bracket-depth-0) tokens reach `:` or `:=` before the line ends is a declaration,
// not an application continuation. Bare top-level `:`/`:=` never occur mid-expression
// (ascriptions are parenthesized, binder/record colons live inside braces), so this
// cannot cut a legitimate expression.
function isDeclStart(ts: Tok[], i: number): boolean {
  if (ts[i].t !== "id" && !(ts[i].t === "punct" && (ts[i] as any).v === "(")) return false
  let depth = 0, q = i
  while (q < ts.length) {
    const t = ts[q]
    if (t.t === "eof" || (t.t === "nl" && depth === 0)) return false
    if (t.t === "kw" && depth === 0) return false
    if (t.t === "punct") {
      const v = (t as any).v
      if (v === "(" || v === "{" || v === "[") depth++
      else if (v === ")" || v === "}" || v === "]") { if (depth === 0) return false; depth-- }
      else if (depth === 0 && (v === ":=" || v === ":")) return true
      else if (depth === 0 && (v === "=" || v === ";" || v === "=>" || v === "->")) return false
    }
    q++
  }
  return false
}

// Check if position starts a match arm pattern: a constructor followed by zero
// or more binder idents and then `=>` (e.g. `Ctor =>`, `Ctor x =>`,
// `Ctor a b c =>`). Used to stop multi-line arm bodies before the next arm.
// `=>` is match-only, so this only ever fires inside a match.
function isArmStart(ts: Tok[], i: number): boolean {
  if (ts[i].t !== "id") return false
  let j = i + 1
  while (ts[j]?.t === "id") j++           // skip binder idents
  return ts[j]?.t === "punct" && (ts[j] as any).v === "=>"
}

// atom = simple ("." IDENT)*
// Newlines before atoms are insignificant — expressions can span lines.
// Item separation works because keywords (let, test, open) can't start an atom,
// and field definitions (IDENT ":=") are detected via lookahead so expressions
// don't accidentally consume the start of a field.
const atom: P<Expr> = withProj((ts, i) => {
  const hadNewline = ts[i].t === "nl"
  while (ts[i].t === "nl") i++
  if (hadNewline && (isFieldStart(ts, i) || isDeclStart(ts, i)))
    return err(`field definition, not an atom`, i)
  return simple(ts, i)
})

// app = atom atom*
const app: P<Expr> = map(
  seq(atom, many(atom)),
  ([h, xs]) => xs.reduce<Expr>((f, x) => ({ tag: "app", f, x }), h),
)

// --- Line-mode parsers (for block-let bodies) ---
// In line mode, newlines terminate expressions. Braces still group freely.
// This prevents `let x = a\nb` from parsing `a b` as one expression.

const lineSimple: P<Expr> = makeSimple(() => lineBraced, () => lineIfP)

const lineAtom: P<Expr> = withProj((ts, i) => {
  if (ts[i].t === "nl") return err(`unexpected newline`, i)
  return lineSimple(ts, i)
})

// lineApp: first atom may skip newlines; subsequent atoms must stay on line.
const lineApp: P<Expr> = (ts, i) => {
  while (ts[i].t === "nl") i++
  const head = withProj(lineSimple)(ts, i)
  if (!head.ok) return head
  let result: Expr = head.v
  let pos = head.pos
  for (;;) {
    const arg = lineAtom(ts, pos)
    if (!arg.ok) break
    result = { tag: "app", f: result, x: arg.v }
    pos = arg.pos
  }
  return ok(result, pos)
}

// makeExpr: arrow-suffix handler shared by expr and lineExpr.
const makeExpr = (appP: P<Expr>): P<Expr> => {
  const p: P<Expr> = (ts, i) => {
    const lhs = appP(ts, i)
    if (!lhs.ok) return lhs
    const arr = arrowP(ts, lhs.pos)
    if (!arr.ok) return lhs
    let pos = arr.pos
    while (ts[pos].t === "nl") pos++
    const rhs = p(ts, pos)
    if (!rhs.ok) return rhs
    return ok(
      { tag: "binder" as const, params: [{ name: null, type: lhs.v }], body: rhs.v },
      rhs.pos,
    )
  }
  return p
}

const lineExpr: P<Expr> = makeExpr(lineApp)

// matchAtom: like atom but also stops before "TT =>" or "FF =>" after a newline.
// Used in match arm bodies so multi-line arms don't consume the next arm's pattern.
const matchAtom: P<Expr> = withProj((ts, i) => {
  const hadNewline = ts[i].t === "nl"
  while (ts[i].t === "nl") i++
  if (hadNewline && (isFieldStart(ts, i) || isArmStart(ts, i)))
    return err(`arm boundary, not an atom`, i)
  return simple(ts, i)
})

// matchApp / matchExpr: full multi-line expression parser for match arm bodies.
// Spans newlines freely but stops before the next arm pattern (TT/FF =>).
const matchApp: P<Expr> = map(
  seq(matchAtom, many(matchAtom)),
  ([h, xs]) => xs.reduce<Expr>((f, x) => ({ tag: "app", f, x }), h),
)
const matchExpr: P<Expr> = makeExpr(matchApp)

// binderParam = (IDENT | "_") (":" expr)? (":=" expr)?
// The trailing `:= expr` is a named-argument default (see Param). `expr` stops
// before `:=` (it isn't an operator in expr), so the type parses cleanly first.
const binderParam: P<Param> = nl(map(
  seq(idP,
    optional(seq(nl(punctP(":")), skipNl, lazy(() => expr))),
    optional(seq(nl(punctP(":=")), skipNl, lazy(() => expr)))),
  // Omit `default` when absent so the AST shape is unchanged for plain params
  // (mirrors TypedField omitting `value`); extractSignature reads `?? null`.
  ([v, ann, def]) => def
    ? { name: v === "_" ? null : v, type: ann ? ann[2] : null, default: def[2] }
    : { name: v === "_" ? null : v, type: ann ? ann[2] : null },
))

// Parse binder: params "}" ARROW body. Parameterized by bodyParser.
const makeBinderInner = (bodyParser: P<Expr>): P<Expr> => map(
  seq(sepBy1(binderParam, commaP), nl(punctP("}")), nl(arrowP), skipNl, lazy(() => bodyParser)),
  ([params, , , , body]) => ({ tag: "binder" as const, params, body }),
)
const binderInner: P<Expr> = makeBinderInner(lazy(() => expr))
const lineBinderInner: P<Expr> = makeBinderInner(lazy(() => lineExpr))

// typedField = IDENT (":" expr)? (":=" expr)? — at least one part present.
// `name : T` is an opaque telescope entry; `name := e` / `name : T := e` is a
// DERIVED entry (the field is pinned to recipe e over the prior fields).
const typedFieldP: P<TypedField> = nl((ts, i) => {
  // Reject "_" as a recType field name
  if (ts[i].t === "id" && (ts[i] as any).v === "_") return err(`recType field cannot be '_'`, i)
  const r = idP(ts, i)
  if (!r.ok) return r
  const name = r.v
  let pos = r.pos
  let type: Expr | null = null
  let value: Expr | null = null
  const withType = seq(nl(punctP(":")), skipNl, lazy(() => expr))(ts, pos)
  if (withType.ok) { type = withType.v[2] as Expr; pos = withType.pos }
  const withVal = seq(nl(punctP(":=")), skipNl, lazy(() => expr))(ts, pos)
  if (withVal.ok) { value = withVal.v[2] as Expr; pos = withVal.pos }
  if (type === null && value === null) return err(`expected ':' or ':=' after field name '${name}'`, pos)
  // Omit `value` for plain fields so the AST shape is unchanged for them.
  return ok(value === null ? { name, type } : { name, type, value }, pos)
})

// Parse recType contents after "{". Expects typed fields, "}".
const recTypeInner: P<Expr> = (ts, i) => {
  const r = seq(sepBy1(typedFieldP, commaP), nl(punctP("}")))(ts, i)
  if (!r.ok) return r
  const [fields] = r.v
  const dup = duplicateName(fields)
  if (dup) return err(`duplicate record field '${dup}'`, i)
  return ok({ tag: "recType" as const, fields }, r.pos)
}

// match arm: IDENT "=>" expr (where IDENT is "TT" or "FF").
// Body uses matchExpr so it can span multiple lines; it stops before the next
// "TT =>" or "FF =>" pattern (via matchAtom's isArmStart lookahead).
// match arm: `Ctor b0 b1 ... => body` — a constructor and zero or more binders
// (each may be `_`). `Ctor` is the tag (a string, by spelling); `_` as the
// constructor is the wildcard/default arm.
export type Arm = { pat: string; binders: string[]; body: Expr }
const matchArmP: P<Arm> = nl((ts, i) => {
  const ctor = idP(ts, i)
  if (!ctor.ok) return ctor
  let pos = ctor.pos
  const binders: string[] = []
  while (ts[pos].t === "id") { binders.push((ts[pos] as any).v as string); pos++ }
  const r = seq(nl(punctP("=>")), skipNl, lazy(() => matchExpr))(ts, pos)
  if (!r.ok) return r
  return ok({ pat: ctor.v, binders, body: r.v[2] }, r.pos)
})

// Coproduct match: `match c { V1 x => b1; V2 y => b2 }` parses to a `match`
// node; the desugar to the §2.6 cut lives in compile.ts, where arm bodies are
// closed over their free vars (so it can consult the compiled bodies). Tags are
// the constructor *names as strings*; `_` is the wildcard/default arm.
const matchP: P<Expr> = (ts, i) => {
  const r = seq(
    kwP("match"), skipNl, lazy(() => app),
    nl(punctP("{")),
    sepBy1(matchArmP, semiP),
    nl(punctP("}")),
  )(ts, i)
  if (!r.ok) return r
  const [, , cond, , arms] = r.v
  const isBool = arms.length === 2 && !arms.some(a => a.binders.length > 0)
    && arms.some(a => a.pat === "TT") && arms.some(a => a.pat === "FF")
  if (isBool) {
    // Boolean match was removed in favour of `if c then a else b` (which desugars
    // to the prelude `cond`). `match` is now exclusively the §2.6 coproduct cut.
    // Report at r.pos (past the arms) so this beats the generic idP error in `alt`.
    return err(`boolean 'match c { TT => …; FF => … }' was removed — use 'if c then … else …'`, r.pos)
  }
  return ok({ tag: "match", cond, arms }, r.pos)
}

// `if c then a else b` — the boolean conditional (desugars to `cond` in
// compile.ts). The cond is an app chain bounded by the `then` keyword; `then`
// and `else` (keywords, never atoms) bound the first two parts. The else body's
// trailing boundary is mode-sensitive, so `if` comes in two flavours like
// simple/lineSimple:
//   ifP     (multi-line): bodies use matchExpr — span newlines but stop at the
//           enclosing boundary (closing punct, next field, or next match arm).
//           Reached via `simple`, including inside match arms, so a nested
//           `Ok v => if … else …` stops at the next arm. `else if` chains and
//           nested `if`s fall out for free (the else body re-enters ifP).
//   lineIfP (line mode): bodies use lineApp/lineExpr — newline-terminated, so
//           `let r = if x then a else b` ends at the line, leaving the block's
//           trailing expression untouched. Reached via `lineSimple`.
const makeIf = (condP: () => P<Expr>, bodyP: () => P<Expr>): P<Expr> => (ts, i) => {
  const r = seq(
    kwP("if"), skipNl, lazy(condP),
    nl(kwP("then")), skipNl, lazy(bodyP),
    nl(kwP("else")), skipNl, lazy(bodyP),
  )(ts, i)
  if (!r.ok) return r
  const [, , cond, , , thenBody, , , elseBody] = r.v
  return ok({ tag: "if" as const, cond, thenBody, elseBody }, r.pos)
}
const ifP: P<Expr> = makeIf(() => app, () => matchExpr)
const lineIfP: P<Expr> = makeIf(() => lineApp, () => lineExpr)

// Build the leaf-based cons-chain `t e1 (t e2 (... t))` — cons = `t a b` (fork),
// nil = leaf — matching the library's cons/nil (§2.6 arrays).
function mkConsChain(elems: Expr[]): Expr {
  let acc: Expr = { tag: "leaf" }
  for (let k = elems.length - 1; k >= 0; k--)
    acc = { tag: "app", f: { tag: "app", f: { tag: "leaf" }, x: elems[k] }, x: acc }
  return acc
}

// Array literal: [ e1, e2, ... ] (comma- or newline-separated). Empty `[]` is nil.
const arrayP: P<Expr> = (ts, i) => {
  const open = punctP("[")(ts, i)
  if (!open.ok) return open
  let pos = open.pos
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t === "punct" && (ts[pos] as any).v === "]")
    return ok({ tag: "leaf" }, pos + 1)
  const elemsR = sepBy1(nl(lazy(() => expr)), commaP)(ts, pos)
  if (!elemsR.ok) return elemsR
  pos = elemsR.pos
  while (ts[pos].t === "nl") pos++
  const close = punctP("]")(ts, pos)
  if (!close.ok) return close
  return ok(mkConsChain(elemsR.v), close.pos)
}

// sumVariant = IDENT (":" expr)? — `Tag : T` is a single-arg variant, `Tag` is
// nullary. The payload type is a full `expr` (newlines are insignificant inside
// `<…>`, like array elements), compiled in TYPE position by the desugar.
const sumVariantP: P<SumVariant> = nl((ts, i) => {
  if (ts[i].t === "id" && (ts[i] as any).v === "_") return err(`sum variant cannot be '_'`, i)
  const r = idP(ts, i)
  if (!r.ok) return r
  const name = r.v
  let pos = r.pos
  let type: Expr | null = null
  const withType = seq(nl(punctP(":")), skipNl, lazy(() => expr))(ts, pos)
  if (withType.ok) { type = withType.v[2] as Expr; pos = withType.pos }
  return ok({ name, type }, pos)
})

// Sum-type literal: `< variant (COMMA variant)* COMMA? >` (COMMA = "," or NEWLINE,
// like record fields). Empty `<>` is the empty sum (⊥). Desugars in compile.ts to
// `Coproduct [pair "Tag" [T]…]` — the DUAL of recType → Telescope. `>` is a token
// distinct from `->`/`=>`, so a variant's `expr` type (incl. `A -> B`) stops cleanly
// at the closing `>`.
const sumTypeP: P<Expr> = (ts, i) => {
  const open = punctP("<")(ts, i)
  if (!open.ok) return open
  let pos = open.pos
  while (ts[pos].t === "nl") pos++
  if (ts[pos].t === "punct" && (ts[pos] as any).v === ">") // empty <>
    return ok({ tag: "sumType", variants: [] }, pos + 1)
  const varsR = sepBy1(sumVariantP, commaP)(ts, pos)
  if (!varsR.ok) return varsR
  pos = varsR.pos
  const trail = commaP(ts, pos) // optional trailing COMMA (sepBy1 leaves it unconsumed)
  if (trail.ok) pos = trail.pos
  while (ts[pos].t === "nl") pos++
  const close = punctP(">")(ts, pos)
  if (!close.ok) return close
  const dup = duplicateName(varsR.v)
  if (dup) return err(`duplicate sum variant '${dup}'`, i)
  return ok({ tag: "sumType" as const, variants: varsR.v }, close.pos)
}

// field = IDENT (":" expr)? ":=" valParser
const makeFieldP = (valParser: P<Expr>): P<{ tag: "field"; name: string; type: Expr | null; value: Expr }> =>
  nl(map(
    seq(idP,
      optional(seq(nl(punctP(":")), skipNl, lazy(() => expr))),
      nl(punctP(":=")), skipNl, lazy(() => valParser)),
    ([name, ann, , , value]) => ({
      tag: "field" as const, name, type: ann ? ann[2] : null, value,
    }),
  ))
const bracedFieldP = makeFieldP(lineExpr)
const topFieldP = makeFieldP(lazy(() => expr))

// --- Braced member combinators (let/test/open/field inside { ... }) ---
// Each parses one member + trailing SEMI, returning a RecMember.

const bracedLetP: P<RecMember> = map(
  seq(kwP("let"), nl(idP),
    optional(seq(nl(punctP(":")), skipNl, lazy(() => expr))),
    nl(alt(punctP(":="), punctP("="))), skipNl, lazy(() => lineExpr), semiP),
  ([, name, ann, , , body]) => ({
    tag: "let" as const, name, type: ann ? ann[2] : null, body,
  }),
)

const bracedTestP: P<RecMember> = map(
  seq(kwP("test"), skipNl, lazy(() => lineExpr),
    nl(punctP("=")), skipNl, lazy(() => lineExpr), semiP),
  ([, , lhs, , , rhs]) => ({ tag: "test" as const, lhs, rhs }),
)

const bracedOpenP: P<RecMember> = map(
  seq(kwP("open"), skipNl, lazy(() => lineExpr), semiP),
  ([, , e]) => ({ tag: "open" as const, expr: e }),
)

// recValue field separator: ";" | "," | NEWLINE. recValues historically used
// SEMI/newline; "," is also accepted so named-argument calls read naturally
// (`f { host := "h", port := 8000 }`, the design-doc spelling).
const fieldSepP: P<null> = (ts, i) => {
  const c = commaP(ts, i)
  return c.ok ? c : semiP(ts, i)
}

// bracedFieldP already defined above; wrap it as a RecMember with optional separator.
const bracedFieldMemberP: P<RecMember> = (ts, i) => {
  const r = bracedFieldP(ts, i)
  if (!r.ok) return r
  let pos = r.pos
  const s = fieldSepP(ts, pos)
  if (s.ok) pos = s.pos
  return ok(r.v as RecMember, pos)
}

// Field pun (Rust-style shorthand): a bare IDENT member is `name := name`,
// the name resolving in the OUTER scope (a field's value compiles before its
// name binds). Only meaningful alongside at least one real `:=` field —
// unifiedBracedInner reinterprets or rejects puns in field-less bodies.
const bracedPunP: P<RecMember> = (ts, i) => {
  const r = idP(ts, i)
  if (!r.ok) return r
  const name = r.v
  if (name === "_") return err("'_' cannot be a field pun", i)
  const punMember = { tag: "field" as const, name, type: null, value: { tag: "var" as const, name }, pun: true }
  const s = fieldSepP(ts, r.pos)
  if (s.ok) return ok(punMember as RecMember, s.pos)
  if (ts[r.pos].t === "punct" && (ts[r.pos] as any).v === "}")
    return ok(punMember as RecMember, r.pos)
  return err("bare identifier is not a member (pun needs ';', ',', newline, or '}')", i)
}

const bracedMemberP: P<RecMember> = nl(alt<RecMember>(bracedLetP, bracedTestP, bracedOpenP, bracedFieldMemberP, bracedPunP))

// Unified braced body parser: handles blocks, recValues, and mixed members.
// Parses members (let/test/open/field). Then:
//   - If any `:=` field found → recValue (fields are exports, lets are private stmts)
//   - If no `:=` field and trailing expr → block (desugar as App(Binder, ...))
//   - If no `:=` field and no trailing expr → empty recValue
const unifiedBracedInner: P<Expr> = (ts, startPos) => {
  const membersR = many(bracedMemberP)(ts, startPos)
  if (!membersR.ok) return membersR
  const members = membersR.v.slice()
  let pos = membersR.pos

  // Field puns only make sense alongside a real `:=` field. In a field-less
  // body, a single FINAL pun is really the block's trailing expression
  // (e.g. `{ let a = t; a }`); anything else is an error.
  let preTrailing: Expr | null = null
  const isPun = (m: RecMember) => m.tag === "field" && (m as any).pun === true
  const realFieldCount = members.filter(m => m.tag === "field" && !isPun(m)).length
  const punCount = members.filter(isPun).length
  if (realFieldCount === 0 && punCount > 0) {
    const last = members[members.length - 1]
    if (punCount === 1 && isPun(last)) {
      members.pop()
      preTrailing = { tag: "var", name: (last as any).name }
    } else {
      return err("bare identifier member (field pun) needs a sibling ':=' field", startPos)
    }
  }

  const exportedFields: NamedField[] = []
  const bindings: { name: string; type: Expr | null; body: Expr }[] = []
  for (const m of members) {
    if (m.tag === "field") {
      if (exportedFields.some(f => f.name === m.name))
        return err(`duplicate exported field '${m.name}'`, startPos)
      exportedFields.push({ name: m.name, type: m.type, value: m.value! }) // braced fields always carry a value
    }
    if (m.tag === "let") bindings.push({ name: m.name, type: m.type, body: m.body })
  }

  while (ts[pos].t === "nl") pos++

  const hasNonFieldMembers = members.some(m => m.tag !== "field")
  const membersOrUndef = hasNonFieldMembers ? members : undefined

  // If we have exported fields → recValue
  if (exportedFields.length > 0) {
    if (!(ts[pos].t === "punct" && (ts[pos] as any).v === "}"))
      return err(`expected '}', got ${describe(ts[pos])}`, pos)
    pos++
    const rv: Expr = { tag: "recValue", fields: exportedFields }
    if (membersOrUndef) (rv as any).members = membersOrUndef
    return ok(rv, pos)
  }

  // No exported fields: "}" → empty recValue, or trailing expr → block
  if (preTrailing === null && ts[pos].t === "punct" && (ts[pos] as any).v === "}") {
    pos++
    const rv: Expr = { tag: "recValue", fields: [] }
    if (membersOrUndef) (rv as any).members = membersOrUndef
    return ok(rv, pos)
  }

  if (preTrailing === null && bindings.length === 0 && members.length === 0)
    return err(`expected '}' or expression, got ${describe(ts[pos])}`, pos)

  // Block with trailing expression (a converted final pun arrives pre-parsed).
  let trailing: Expr
  if (preTrailing !== null) {
    trailing = preTrailing
  } else {
    const trailR = expr(ts, pos)
    if (!trailR.ok) return trailR
    trailing = trailR.v
    pos = trailR.pos
    const s = semiP(ts, pos)
    if (s.ok) pos = s.pos
  }
  while (ts[pos].t === "nl") pos++
  if (!(ts[pos].t === "punct" && (ts[pos] as any).v === "}"))
    return err(`expected '}', got ${describe(ts[pos])}`, pos)
  pos++

  // If the block contains test/open members, the simple App(Binder, val)
  // desugaring would drop them — they have no Expr-level placeholder.
  // Preserve them by emitting a recValue with a `trailing` body; compile.ts
  // processes members (let/test/open) in order then evaluates `trailing`
  // in the resulting scope, returning its value. This makes Q2 (inline
  // tests in blocks) work without changing the desugaring for plain
  // let-blocks (which still produce App(Binder, val) for back-compat).
  const hasTestOrOpen = members.some(m => m.tag === "test" || m.tag === "open")
  if (hasTestOrOpen) {
    const rv: Expr = { tag: "recValue", fields: [], members, trailing }
    return ok(rv, pos)
  }

  // Desugar block: right-to-left wrap trailing expr in nested App(Binder, body)
  let result: Expr = trailing
  for (let i = bindings.length - 1; i >= 0; i--) {
    const b = bindings[i]
    const val: Expr = b.type
      ? { tag: "ann", expr: b.body, type: b.type }
      : b.body
    result = {
      tag: "app",
      f: { tag: "binder", params: [{ name: b.name, type: null }], body: result },
      x: val,
    }
  }
  return ok(result, pos)
}


// Parse bare recType: {a, b, c} or {a, b, c : T} (spread type).
const bareRecTypeInner: P<Expr> = (ts, i) => {
  const r = seq(
    sepBy1(nl(idP), nl(punctP(","))),
    optional(seq(nl(punctP(":")), skipNl, lazy(() => expr))),
    nl(punctP("}")),
  )(ts, i)
  if (!r.ok) return r
  const [names, ann] = r.v
  const fields = names.map(name => ({ name, type: ann ? ann[2] : null }))
  const dup = duplicateName(fields)
  if (dup) return err(`duplicate record field '${dup}'`, i)
  return ok({ tag: "recType" as const, fields }, r.pos)
}

// Disambiguate braced content after seeing "{".
// Peek at the first member to determine: recValue/block, binder, or recType.
// Parameterized by binderParser so braced (normal) and lineBraced share one function.
function parseBraced(binderParser: P<Expr>): P<Expr> {
  return (ts, i) => {
    if (!(ts[i].t === "punct" && (ts[i] as any).v === "{"))
      return err(`expected '{', got ${describe(ts[i])}`, i)
    let pos = i + 1
    while (ts[pos].t === "nl") pos++

    // Empty braces → empty recValue
    if (ts[pos].t === "punct" && (ts[pos] as any).v === "}") {
      return ok({ tag: "recValue" as const, fields: [] }, pos + 1)
    }

    // Keyword → unified body (let/test/open, possibly mixed with fields)
    if (ts[pos].t === "kw" && ((ts[pos] as any).v === "let" || (ts[pos] as any).v === "test" || (ts[pos] as any).v === "open")) {
      return unifiedBracedInner(ts, pos)
    }

    const shape = classifyBracedContent(ts, pos)
    if (shape === "recValue") {
      // A `:=`-bearing brace followed by `->` is a binder whose params have
      // defaults (`{ y : B := d } -> body`), not a record value. Try the binder
      // parser first in that case; it only succeeds with a trailing ARROW, so a
      // plain recValue (no arrow) still falls through to unifiedBracedInner.
      if (bracedFollowedByArrow(ts, pos)) {
        const b = binderParser(ts, pos)
        if (b.ok) return b
      }
      return unifiedBracedInner(ts, pos)
    }
    if (shape === "binder") {
      const binderAttempt = binderParser(ts, pos)
      if (binderAttempt.ok) return binderAttempt
      return bareRecTypeInner(ts, pos)
    }
    // recTypeOrBinder: try binder first (needs "}" then ARROW), fall back to recType.
    const binderAttempt = binderParser(ts, pos)
    if (binderAttempt.ok) return binderAttempt
    return recTypeInner(ts, pos)
  }
}
const braced: P<Expr> = parseBraced(binderInner)
const lineBraced: P<Expr> = parseBraced(lineBinderInner)

// Scan forward for a ":=" at depth 0, stopping at the enclosing "}" — used to
// spot a recValue whose first member doesn't show one directly (a typed field
// `a : T := e`, or a field pun `respond; a := 1`).
function scanForFieldAssign(ts: Tok[], q: number, stopAtCommaSemi: boolean): boolean {
  let depth = 0
  while (q < ts.length && ts[q].t !== "eof") {
    if (ts[q].t === "punct") {
      const v = (ts[q] as any).v
      if (v === "(" || v === "{") depth++
      else if (v === ")" || v === "}") { if (depth === 0) return false; depth-- }
      else if (depth === 0 && v === ":=") return true
      else if (depth === 0 && stopAtCommaSemi && (v === "," || v === ";")) return false
    }
    q++
  }
  return false
}

// From the position just after a "{", find the matching "}" and report whether
// the next significant token is an ARROW. Such a brace group is a *binder* (its
// params may carry `:= default` suffixes, which otherwise look like recValue
// fields) — `{ x : A, y : B := d } -> body`. Depth-tracks nested braces/parens.
function bracedFollowedByArrow(ts: Tok[], pos: number): boolean {
  let depth = 0, q = pos
  while (q < ts.length && ts[q].t !== "eof") {
    if (ts[q].t === "punct") {
      const v = (ts[q] as any).v
      if (v === "{" || v === "(" || v === "[") depth++
      else if (v === "}" || v === ")" || v === "]") {
        if (depth === 0) { // this is our matching "}"
          let r = q + 1
          while (ts[r]?.t === "nl") r++
          return ts[r]?.t === "punct" && ((ts[r] as any).v === "->" || (ts[r] as any).v === "→")
        }
        depth--
      }
    }
    q++
  }
  return false
}

// Peek at tokens after "{" to classify the braced content.
function classifyBracedContent(ts: Tok[], pos: number): "recValue" | "binder" | "recTypeOrBinder" {
  let p = pos
  while (ts[p].t === "nl") p++
  if (ts[p].t === "id") {
    const name = (ts[p] as any).v as string
    p++
    let crossedNl = false
    while (ts[p].t === "nl") { crossedNl = true; p++ }
    if (name === "_") {
      if (ts[p].t === "punct" && (ts[p] as any).v === ":") return "recTypeOrBinder"
      return "binder"
    }
    if (ts[p].t === "punct" && ((ts[p] as any).v === "}" || (ts[p] as any).v === ",")) return "binder"
    if (ts[p].t === "punct" && (ts[p] as any).v === ":=") return "recValue"
    if (ts[p].t === "punct" && (ts[p] as any).v === ":") {
      // `name : T …` — a later ":=" in this member means a typed field.
      return scanForFieldAssign(ts, p + 1, true) ? "recValue" : "recTypeOrBinder"
    }
    // `name ;` or `name NEWLINE id …` — possibly a field pun leading a
    // recValue; a ":=" anywhere before the closing "}" decides it.
    if ((ts[p].t === "punct" && (ts[p] as any).v === ";") || (crossedNl && ts[p].t === "id")) {
      if (scanForFieldAssign(ts, p, false)) return "recValue"
    }
  }
  return "binder"
}

// expr = binder | app (ARROW expr)?
// A standalone binder `{...} -> body` is parsed by `braced` inside `simple`.
// The ARROW suffix handles `A -> B` sugar.
const expr: P<Expr> = makeExpr(app)

// --- Items ---

const letItem: P<RecMember> = map(
  seq(
    kwP("let"), idP,
    optional(seq(punctP(":"), skipNl, lazy(() => expr))),
    nl(alt(punctP(":="), punctP("="))), skipNl, lazy(() => expr),
  ),
  ([, name, ann, , , body]) => ({
    tag: "let" as const,
    name,
    type: ann ? ann[2] : null,
    body,
  }),
)

const testItem: P<RecMember> = map(
  seq(kwP("test"), lazy(() => expr), nl(punctP("=")), skipNl, lazy(() => expr)),
  ([, lhs, , , rhs]) => ({ tag: "test" as const, lhs, rhs }),
)

const openItem: P<RecMember> = map(
  seq(kwP("open"), lazy(() => expr)),
  ([, expr]) => ({ tag: "open" as const, expr }),
)

// A decorated declaration: `head… NAME (: T)? (:= v)?` — one or more head atoms
// before the name (COMPILATION.typ § Declarations as requests). The name is the last
// atom of the pre-`:`/`:=` spine; the head is everything before it (a request
// decorator, applied to the built request by the driver). Head atoms are line-local
// so the spine cannot swallow the next item. A head with only an annotation is an
// interface entry (value = null); a bare spine with neither `:` nor `:=` is not an
// item. Tried after topFieldP, so plain `NAME := v` fields are unaffected.
const headFieldP: P<RecMember> = (ts, i) => {
  const atoms: Expr[] = []
  let pos = i
  for (;;) {
    const r = lineAtom(ts, pos)
    if (!r.ok) break
    atoms.push(r.v)
    pos = r.pos
  }
  if (atoms.length < 2) return err(`decorated declaration`, i)
  const last = atoms[atoms.length - 1]
  if (last.tag !== "var") return err(`decorated declaration: the declared name must be a plain identifier`, pos)
  const head = atoms.slice(0, -1).reduce((f, x) => ({ tag: "app" as const, f, x }))
  let type: Expr | null = null
  const ann = optional(seq(nl(punctP(":")), skipNl, lazy(() => expr)))(ts, pos)
  if (ann.ok && ann.v) { type = ann.v[2]; pos = ann.pos }
  let value: Expr | null = null
  const asn = optional(seq(nl(punctP(":=")), skipNl, lazy(() => expr)))(ts, pos)
  if (asn.ok && asn.v) { value = asn.v[2]; pos = asn.pos }
  if (type === null && value === null) return err(`decorated declaration: expected ':' or ':='`, pos)
  return ok({ tag: "field" as const, name: last.name, type, value, head }, pos)
}

const itemP: P<RecMember> = nl(alt(openItem, letItem, testItem, topFieldP, headFieldP))

// Parse source into items.
export function parseItems(src: string): RecMember[] {
  const toks = tokenize(src)
  const items: RecMember[] = []
  let pos = 0
  while (toks[pos].t === "nl") pos++
  while (toks[pos].t !== "eof") {
    const r = itemP(toks, pos)
    if (!r.ok) throw new Error(`parse: ${r.msg}`)
    // Top-level field redefinition is now guard-mediated (a rebind request), so it is
    // legal syntax; the driver rejects UNGUARDED duplicates (the old accident check
    // moved there, where guard knowledge lives). Braced records still reject
    // duplicates at parse time.
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
