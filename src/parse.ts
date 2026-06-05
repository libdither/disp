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

const KEYWORDS = new Set(["let", "test", "use", "open", "match"])
// Order matters: longer punctuation first so ":=" isn't chopped into ":" "=".
const PUNCT = [":=", "=>", "->", "→", ".", ",", ";", "(", ")", "=", ":", "{", "}", "[", "]"] as const
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
  | { tag: "recValue"; fields: NamedField[]; members?: RecMember[]; trailing?: Expr }
  | { tag: "use"; path: string; raw?: boolean }
  | { tag: "match"; cond: Expr; thenBody: Expr; elseBody: Expr }

export type Param = { name: string | null; type: Expr | null }
export type TypedField = { name: string; type: Expr | null }
export type NamedField = { name: string; type: Expr | null; value: Expr }

// Unified record body member — shared by file bodies and inline { ... } recValues.
// "field" (name := expr) is exported; "let" is private; "test"/"open" are side-effects.
export type RecMember =
  | { tag: "field"; name: string; type: Expr | null; value: Expr }
  | { tag: "let"; name: string; type: Expr | null; body: Expr }
  | { tag: "test"; lhs: Expr; rhs: Expr }
  | { tag: "open"; expr: Expr }

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

// simple/lineSimple differ only in which braced parser they use.
const makeSimple = (bracedP: () => P<Expr>): P<Expr> => lazy(() => alt<Expr>(
  // Parenthesized: ( expr ) or ( expr : expr )
  map(
    seq(punctP("("), skipNl, lazy(() => expr), nl(optional(seq(punctP(":"), skipNl, lazy(() => expr)))), nl(punctP(")"))),
    ([, , e, ann]) => ann ? { tag: "ann", expr: e, type: ann[2] } : e,
  ),
  lazy(() => matchP),
  lazy(() => arrayP),
  lazy(bracedP),
  map(seq(kwP("use"), optional(rawKwP), strP), ([, raw, path]): Expr => raw !== null ? { tag: "use", path, raw: true } : { tag: "use", path }),
  map(strP, (value): Expr => ({ tag: "str", value })),
  map(leafP, (): Expr => ({ tag: "leaf" })),
  map(numP, (value): Expr => ({ tag: "num", value })),
  holeP,
  map(idP, (name): Expr => ({ tag: "var", name })),
))
const simple: P<Expr> = makeSimple(() => braced)

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
  if (hadNewline && isFieldStart(ts, i))
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

const lineSimple: P<Expr> = makeSimple(() => lineBraced)

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

// binderParam = (IDENT | "_") (":" expr)?
const binderParam: P<Param> = nl(map(
  seq(idP, optional(seq(nl(punctP(":")), skipNl, lazy(() => expr)))),
  ([v, ann]) => ({ name: v === "_" ? null : v, type: ann ? ann[2] : null }),
))

// Parse binder: params "}" ARROW body. Parameterized by bodyParser.
const makeBinderInner = (bodyParser: P<Expr>): P<Expr> => map(
  seq(sepBy1(binderParam, commaP), nl(punctP("}")), nl(arrowP), skipNl, lazy(() => bodyParser)),
  ([params, , , , body]) => ({ tag: "binder" as const, params, body }),
)
const binderInner: P<Expr> = makeBinderInner(lazy(() => expr))
const lineBinderInner: P<Expr> = makeBinderInner(lazy(() => lineExpr))

// typedField = IDENT ":" expr
const typedFieldP: P<TypedField> = nl((ts, i) => {
  // Reject "_" as a recType field name
  if (ts[i].t === "id" && (ts[i] as any).v === "_") return err(`recType field cannot be '_'`, i)
  const r = seq(idP, nl(punctP(":")), skipNl, lazy(() => expr))(ts, i)
  if (!r.ok) return r
  const [name, , , type] = r.v
  return ok({ name, type }, r.pos)
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
type Arm = { pat: string; binders: string[]; body: Expr }
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

const binderName = (b: string): string | null => (b === "_" ? null : b)
const apE = (f: Expr, x: Expr): Expr => ({ tag: "app", f, x })
const varE = (name: string): Expr => ({ tag: "var", name })

// The handler an arm contributes to the cut's case-product. The cut feeds it
// the scrutinee's payload `pair_snd c`; binders destructure it:
//   0 binders → ignore the payload;  1 → the payload IS the binder;
//   n≥2 → the payload is a right-nested pair (pair b0 (pair b1 …)), projected.
function armHandler(a: Arm): Expr {
  const n = a.binders.length
  if (n <= 1)
    return { tag: "binder", params: [{ name: n === 0 ? null : binderName(a.binders[0]), type: null }], body: a.body }
  const inner: Expr = { tag: "binder", params: a.binders.map(b => ({ name: binderName(b), type: null })), body: a.body }
  let appd: Expr = inner
  for (let k = 0; k < n; k++) {
    let acc: Expr = varE("__p")
    for (let s = 0; s < k; s++) acc = apE(varE("pair_snd"), acc)   // pair_snd^k __p
    appd = apE(appd, k < n - 1 ? apE(varE("pair_fst"), acc) : acc) // last field is the bare snd-chain
  }
  return { tag: "binder", params: [{ name: "__p", type: null }], body: appd }
}

// Two flavours, discriminated by the arms:
//   Bool:      `match c { TT => a; FF => b }`        → select desugar (compile.ts)
//   Coproduct: `match c { V1 x => b1; V2 y => b2 }`  → the cut (§2.6):
//              `(prod (pair ["V1","V2"] [{x}->b1, {y}->b2])) c`   (needs `prod` in scope)
// Tags are the constructor *names as strings*; `_` is the wildcard arm, whose
// handler is appended past the names so an unmatched tag's `index_of` (= the
// name count) lands on it.
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
    const tt = arms.find(a => a.pat === "TT")!, ff = arms.find(a => a.pat === "FF")!
    return ok({ tag: "match" as const, cond, thenBody: tt.body, elseBody: ff.body }, r.pos)
  }
  const named = arms.filter(a => a.pat !== "_")
  const wildcard = arms.find(a => a.pat === "_")
  const names = mkConsChain(named.map(a => ({ tag: "str", value: a.pat } as Expr)))
  const handlerExprs = named.map(armHandler)
  if (wildcard) handlerExprs.push(armHandler(wildcard))   // default: past the names
  const handlers = mkConsChain(handlerExprs)
  const table: Expr = { tag: "app", f: { tag: "app", f: { tag: "leaf" }, x: names }, x: handlers }
  const prodTable: Expr = { tag: "app", f: { tag: "var", name: "prod" }, x: table }
  return ok({ tag: "app", f: prodTable, x: cond }, r.pos)
}

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
    nl(punctP("=")), skipNl, lazy(() => lineExpr), semiP),
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

// bracedFieldP already defined above; wrap it as a RecMember with optional SEMI.
const bracedFieldMemberP: P<RecMember> = (ts, i) => {
  const r = bracedFieldP(ts, i)
  if (!r.ok) return r
  let pos = r.pos
  const s = semiP(ts, pos)
  if (s.ok) pos = s.pos
  return ok(r.v as RecMember, pos)
}

const bracedMemberP: P<RecMember> = nl(alt<RecMember>(bracedLetP, bracedTestP, bracedOpenP, bracedFieldMemberP))

// Unified braced body parser: handles blocks, recValues, and mixed members.
// Parses members (let/test/open/field). Then:
//   - If any `:=` field found → recValue (fields are exports, lets are private stmts)
//   - If no `:=` field and trailing expr → block (desugar as App(Binder, ...))
//   - If no `:=` field and no trailing expr → empty recValue
const unifiedBracedInner: P<Expr> = (ts, startPos) => {
  const membersR = many(bracedMemberP)(ts, startPos)
  if (!membersR.ok) return membersR
  const members = membersR.v
  let pos = membersR.pos

  const exportedFields: NamedField[] = []
  const bindings: { name: string; type: Expr | null; body: Expr }[] = []
  for (const m of members) {
    if (m.tag === "field") {
      if (exportedFields.some(f => f.name === m.name))
        return err(`duplicate exported field '${m.name}'`, startPos)
      exportedFields.push({ name: m.name, type: m.type, value: m.value })
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
  if (ts[pos].t === "punct" && (ts[pos] as any).v === "}") {
    pos++
    const rv: Expr = { tag: "recValue", fields: [] }
    if (membersOrUndef) (rv as any).members = membersOrUndef
    return ok(rv, pos)
  }

  if (bindings.length === 0 && members.length === 0)
    return err(`expected '}' or expression, got ${describe(ts[pos])}`, pos)

  // Block with trailing expression
  const trailR = expr(ts, pos)
  if (!trailR.ok) return trailR
  pos = trailR.pos
  const s = semiP(ts, pos)
  if (s.ok) pos = s.pos
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
    const rv: Expr = { tag: "recValue", fields: [], members, trailing: trailR.v }
    return ok(rv, pos)
  }

  // Desugar block: right-to-left wrap trailing expr in nested App(Binder, body)
  let result: Expr = trailR.v
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
    if (shape === "recValue") return unifiedBracedInner(ts, pos)
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
const expr: P<Expr> = makeExpr(app)

// --- Items ---

const letItem: P<RecMember> = map(
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

const testItem: P<RecMember> = map(
  seq(kwP("test"), lazy(() => expr), nl(punctP("=")), skipNl, lazy(() => expr)),
  ([, lhs, , , rhs]) => ({ tag: "test" as const, lhs, rhs }),
)

const openItem: P<RecMember> = map(
  seq(kwP("open"), lazy(() => expr)),
  ([, expr]) => ({ tag: "open" as const, expr }),
)

const itemP: P<RecMember> = nl(alt(openItem, letItem, testItem, topFieldP))

// Parse source into items.
export function parseItems(src: string): RecMember[] {
  const toks = tokenize(src)
  const items: RecMember[] = []
  const exportedNames = new Set<string>()
  let pos = 0
  while (toks[pos].t === "nl") pos++
  while (toks[pos].t !== "eof") {
    const r = itemP(toks, pos)
    if (!r.ok) throw new Error(`parse: ${r.msg}`)
    if (r.v.tag === "field") {
      if (exportedNames.has(r.v.name))
        throw new Error(`parse: duplicate exported field '${r.v.name}'`)
      exportedNames.add(r.v.name)
    }
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
