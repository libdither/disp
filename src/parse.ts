// Surface syntax parser for Disp.
// Minimal parser combinator framework + grammar on a token stream.
//
// Syntax:
//   let [rec] id [: type] := value    declaration
//   {x y} -> body                     lambda
//   (A : Type) -> B                   dependent Pi
//   A -> B                            non-dependent function type
//   f x                               application (juxtaposition)
//   Type                              the sort
//   {x : A, y : B}                    record type (sugar)
//   {x := a, y := b}                  record value (sugar)
//   <L : A | R : B>                   coproduct type (sugar)

// --- Source positions ---

export type Span = { start: number, end: number }

// --- Surface AST ---

export type SExpr =
  | { tag: "svar", name: string, pos?: Span }
  | { tag: "sapp", func: SExpr, arg: SExpr, pos?: Span }
  | { tag: "slam", params: string[], body: SExpr, pos?: Span }
  | { tag: "spi", name: string, domain: SExpr, codomain: SExpr, pos?: Span }
  | { tag: "stype", pos?: Span }
  | { tag: "stree", pos?: Span }

export type SDecl = {
  name: string
  type: SExpr | null  // null if no annotation
  value: SExpr
  isRec: boolean
}

export function svar(name: string, pos?: Span): SExpr {
  return pos ? { tag: "svar", name, pos } : { tag: "svar", name }
}
export function sapp(func: SExpr, arg: SExpr, pos?: Span): SExpr {
  return pos ? { tag: "sapp", func, arg, pos } : { tag: "sapp", func, arg }
}
export function slam(params: string[], body: SExpr, pos?: Span): SExpr {
  return pos ? { tag: "slam", params, body, pos } : { tag: "slam", params, body }
}
export function spi(name: string, domain: SExpr, codomain: SExpr, pos?: Span): SExpr {
  return pos ? { tag: "spi", name, domain, codomain, pos } : { tag: "spi", name, domain, codomain }
}
export const stype: SExpr = { tag: "stype" }

// --- Internal synthetic variable names ---
// '#' prefix can't collide with user identifiers (tokenizer rejects '#').
export const REC_TYPE_VAR = "#R"
export const REC_FN_VAR   = "#f"
export const COP_TYPE_VAR = "#R"

// --- Tokenizer ---

export type Token =
  | { tag: "ident", value: string, pos: Span }
  | { tag: "num", value: number, pos: Span }
  | { tag: "lbrace", pos: Span }   // {
  | { tag: "rbrace", pos: Span }   // }
  | { tag: "lparen", pos: Span }   // (
  | { tag: "rparen", pos: Span }   // )
  | { tag: "arrow", pos: Span }    // ->
  | { tag: "colon", pos: Span }    // :
  | { tag: "coloneq", pos: Span }  // :=
  | { tag: "kw_let", pos: Span }
  | { tag: "kw_type", pos: Span }
  | { tag: "kw_tree", pos: Span }
  | { tag: "kw_true", pos: Span }
  | { tag: "kw_false", pos: Span }
  | { tag: "comma", pos: Span }    // ,
  | { tag: "langle", pos: Span }   // <
  | { tag: "rangle", pos: Span }   // >
  | { tag: "pipe", pos: Span }     // |
  | { tag: "eof", pos: Span }

export class ParseError extends Error {
  constructor(msg: string, public span: Span) { super(msg) }
}

const SINGLE: Record<string, Token["tag"]> = {
  '{': "lbrace", '}': "rbrace", '(': "lparen", ')': "rparen",
  ':': "colon", ',': "comma", '<': "langle", '>': "rangle", '|': "pipe",
}
const KEYWORDS: Record<string, Token["tag"]> = {
  let: "kw_let", Type: "kw_type", true: "kw_true", false: "kw_false",
}

export function tokenize(input: string): Token[] {
  const tokens: Token[] = []
  let i = 0

  while (i < input.length) {
    if (/\s/.test(input[i])) { i++; continue }
    if (input[i] === '-' && input[i + 1] === '-') {
      while (i < input.length && input[i] !== '\n') i++
      continue
    }
    if (input[i] === '-' && input[i + 1] === '>') {
      tokens.push({ tag: "arrow", pos: { start: i, end: i + 2 } }); i += 2; continue
    }
    if (input[i] === ':' && input[i + 1] === '=') {
      tokens.push({ tag: "coloneq", pos: { start: i, end: i + 2 } }); i += 2; continue
    }
    const s = i
    const ch = input[i]
    const single = SINGLE[ch]
    if (single) { tokens.push({ tag: single, pos: { start: s, end: s + 1 } } as Token); i++; continue }
    if (/[0-9]/.test(ch)) {
      const start = i
      while (i < input.length && /[0-9]/.test(input[i])) i++
      tokens.push({ tag: "num", value: parseInt(input.substring(start, i), 10), pos: { start, end: i } })
      continue
    }
    if (/[a-zA-Z_]/.test(ch)) {
      const start = i
      while (i < input.length && /[a-zA-Z0-9_']/.test(input[i])) i++
      const word = input.substring(start, i)
      const pos: Span = { start, end: i }
      const kw = KEYWORDS[word]
      if (kw) tokens.push({ tag: kw, pos } as Token)
      else tokens.push({ tag: "ident", value: word, pos })
      continue
    }
    throw new ParseError(`Unexpected character: '${ch}'`, { start: i, end: i + 1 })
  }

  tokens.push({ tag: "eof", pos: { start: i, end: i } })
  return tokens
}

// === Parser Combinator Framework ===
//
// P<T> parses tokens starting at `pos`, returns [value, newPos] or null.
// Tokens stored in module-level `_t` (set once per parse by `run`).
// Furthest failure tracked in `_far`/`_expected` for error messages.

let _t: Token[] = []
let _far = 0
let _expected: string[] = []

type P<T> = (pos: number) => [T, number] | null

const FAIL = Symbol()

/** Sequential parser: `parser($ => { const x = $(p1); $(p2); return f(x) })` */
function parser<R>(f: ($: <T>(p: P<T>) => T) => R): P<R> {
  return pos => {
    let cur = pos
    try {
      return [f(<T>(p: P<T>): T => {
        const r = p(cur)
        if (!r) throw FAIL
        cur = r[1]
        return r[0]
      }), cur]
    } catch (e) { if (e === FAIL) return null; throw e }
  }
}

/** Match a single token by tag */
function tok(tag: Token["tag"]): P<Token> {
  return pos => {
    if (pos > _far) { _far = pos; _expected = [tag] }
    else if (pos === _far && !_expected.includes(tag)) _expected.push(tag)
    return _t[pos]?.tag === tag ? [_t[pos], pos + 1] : null
  }
}

/** Transform parser result */
function map<A, B>(p: P<A>, f: (a: A) => B): P<B> {
  return pos => { const r = p(pos); return r && [f(r[0]), r[1]] }
}

/** Try alternatives in order (automatic backtracking) */
function or<T>(...ps: P<T>[]): P<T> {
  return pos => { for (const p of ps) { const r = p(pos); if (r) return r } return null }
}

/** Zero or more repetitions */
function many<T>(p: P<T>): P<T[]> {
  return pos => {
    const xs: T[] = []; let cur = pos
    for (let r = p(cur); r; r = p(cur)) { xs.push(r[0]); cur = r[1] }
    return [xs, cur]
  }
}

/** One or more repetitions */
function many1<T>(p: P<T>): P<T[]> {
  return pos => { const r = many(p)(pos)!; return r[0].length ? r : null }
}

/** Optional: returns null on failure (never fails itself) */
function opt<T>(p: P<T>): P<T | null> {
  return pos => p(pos) ?? [null, pos]
}

/** Deferred evaluation for recursive grammars */
function lazy<T>(f: () => P<T>): P<T> {
  let c: P<T> | null = null
  return pos => (c ??= f())(pos)
}

/** Parse a then b, keep only b */
function right<B>(a: P<unknown>, b: P<B>): P<B> {
  return pos => { const ra = a(pos); return ra && b(ra[1]) }
}

/** One or more items separated by delimiter */
function sepBy1<T>(p: P<T>, sep: P<unknown>): P<T[]> {
  return parser($ => [$(p), ...$(many(right(sep, p)))])
}

/** Run parser on tokens, throw ParseError on failure */
function run<T>(p: P<T>, tokens: Token[]): T {
  _t = tokens; _far = 0; _expected = []
  const r = p(0)
  if (!r) {
    const t = _t[Math.min(_far, _t.length - 1)]
    const exp = _expected.length ? _expected.join(" or ") : "unknown"
    throw new ParseError(`Expected ${exp}, got ${t.tag}`, t.pos)
  }
  return r[0]
}

// === Grammar ===

const span = (a: Span, b: Span): Span => ({ start: a.start, end: b.end })
const ident = map(tok("ident"), t => ({ name: (t as any).value as string, pos: t.pos }))
const num = map(tok("num"), t => ({ value: (t as any).value as number, pos: t.pos }))

// "rec" contextual keyword: always succeeds, returns true if consumed "rec"
const recKw: P<boolean> = pos =>
  _t[pos]?.tag === "ident" && (_t[pos] as any).value === "rec"
    ? [true, pos + 1] : [false, pos]

// Number literal: n → succ^n(zero)
function treeNumeral(n: number, pos?: Span): SExpr {
  let e: SExpr = svar("zero", pos)
  for (let i = 0; i < n; i++) e = sapp(svar("succ", pos), e, pos)
  return e
}

// --- Compound parsers ---

// Reusable: ident <sep> expr — used by records and coproducts
const field = (sep: P<unknown>) => parser($ => {
  const n = $(ident); $(sep); return { name: n.name, expr: $(lazy(() => expr)) }
})

// ( expr )
const parenExpr: P<SExpr> = parser($ => {
  const lp = $(tok("lparen"))
  const e = $(lazy(() => expr))
  const rp = $(tok("rparen"))
  return { ...e, pos: span(lp.pos, rp.pos) }
})

// {x : A, y : B} → (R : Type) -> ((x : A) -> (y : B) -> R) -> R
const recordType: P<SExpr> = parser($ => {
  const lb = $(tok("lbrace"))
  const fields = $(sepBy1(field(tok("colon")), tok("comma")))
  const rb = $(tok("rbrace"))
  let inner: SExpr = svar(REC_TYPE_VAR)
  for (let i = fields.length - 1; i >= 0; i--)
    inner = spi(fields[i].name, fields[i].expr, inner)
  return spi(REC_TYPE_VAR, stype, spi("_", inner, svar(REC_TYPE_VAR)), span(lb.pos, rb.pos))
})

// {x := a, y := b} → {R f} -> f a b
const recordValue: P<SExpr> = parser($ => {
  const lb = $(tok("lbrace"))
  const fields = $(sepBy1(field(tok("coloneq")), tok("comma")))
  const rb = $(tok("rbrace"))
  let body: SExpr = svar(REC_FN_VAR)
  for (const f of fields) body = sapp(body, f.expr)
  return slam([REC_TYPE_VAR, REC_FN_VAR], body, span(lb.pos, rb.pos))
})

// <L : A | R : B> → (R : Type) -> (A -> R) -> (B -> R) -> R
const coproductType: P<SExpr> = parser($ => {
  const la = $(tok("langle"))
  const variants = $(sepBy1(field(tok("colon")), tok("pipe")))
  const ra = $(tok("rangle"))
  let result: SExpr = svar(COP_TYPE_VAR)
  for (let i = variants.length - 1; i >= 0; i--)
    result = spi("_", spi("_", variants[i].expr, svar(COP_TYPE_VAR)), result)
  return spi(COP_TYPE_VAR, stype, result, span(la.pos, ra.pos))
})

// --- Core grammar rules ---

// Atom: identifiers, keywords, literals, parens, records, coproducts
// Note: lambdas are NOT atoms (only records are brace-atoms)
const atom: P<SExpr> = or(
  map(ident, i => svar(i.name, i.pos)),
  map(tok("kw_type"), t => ({ tag: "stype", pos: t.pos } as SExpr)),
  map(tok("kw_true"), t => svar("true", t.pos)),
  map(tok("kw_false"), t => svar("false", t.pos)),
  map(num, n => treeNumeral(n.value, n.pos)),
  parenExpr,
  recordType,
  recordValue,
  coproductType,
)

// Application: left-associative atom chain
const app: P<SExpr> = map(many1(atom), atoms =>
  atoms.reduce((f, a) => sapp(f, a, { start: f.pos?.start ?? 0, end: a.pos?.end ?? 0 }))
)

// Lambda: {params} -> body
const lambda: P<SExpr> = parser($ => {
  const lb = $(tok("lbrace"))
  const params = $(many1(ident))
  $(tok("rbrace"))
  $(tok("arrow"))
  const body = $(lazy(() => arrow))
  return slam(params.map(p => p.name), body, span(lb.pos, body.pos!))
})

// Dependent Pi: (name : domain) -> codomain
const piDep: P<SExpr> = parser($ => {
  const lp = $(tok("lparen"))
  const name = $(ident)
  $(tok("colon"))
  const domain = $(lazy(() => expr))
  $(tok("rparen"))
  $(tok("arrow"))
  const codomain = $(lazy(() => arrow))
  return spi(name.name, domain, codomain, span(lp.pos, codomain.pos!))
})

// Arrow: lambda | dependent Pi | app [-> arrow]
const arrow: P<SExpr> = or(
  lambda,
  piDep,
  parser($ => {
    const lhs = $(app)
    const rhs = $(opt(right(tok("arrow"), lazy(() => arrow))))
    return rhs ? spi("_", lhs, rhs, span(lhs.pos!, rhs.pos!)) : lhs
  }),
)

const expr: P<SExpr> = arrow

// Declaration: let [rec] name [: type] := value
const decl: P<SDecl> = parser($ => {
  $(tok("kw_let"))
  const isRec = $(recKw)
  const name = $(ident)
  const type = $(opt(right(tok("colon"), expr)))
  $(tok("coloneq"))
  const value = $(expr)
  return { name: name.name, type, value, isRec }
})

// Top-level line: declaration or expression
const line: P<SDecl | SExpr> = or<SDecl | SExpr>(decl, expr)

// === Public API ===

export function parseExpr(input: string): SExpr {
  const result = run(line, tokenize(input))
  return "tag" in result ? result : result.value
}

export function parseLine(input: string): SDecl | SExpr {
  return run(line, tokenize(input))
}

// === Utilities ===

// Recognize tree-encoded literals in SExpr for pretty printing
export function recognizeLiteral(expr: SExpr): string | null {
  if (expr.tag === "svar") {
    if (expr.name === "true") return "true"
    if (expr.name === "false") return "false"
    if (expr.name === "zero") return "0"
    if (expr.name === "leaf") return "leaf"
  }
  let count = 0
  let e: SExpr = expr
  while (e.tag === "sapp" && e.func.tag === "svar" && e.func.name === "succ") {
    count++
    e = e.arg
  }
  if (e.tag === "svar" && e.name === "zero" && count > 0) return String(count)
  return null
}

// Strip position info for test comparisons
export function stripPos(expr: SExpr): SExpr {
  switch (expr.tag) {
    case "svar": return { tag: "svar", name: expr.name }
    case "stype": return { tag: "stype" }
    case "stree": return { tag: "stree" }
    case "sapp": return { tag: "sapp", func: stripPos(expr.func), arg: stripPos(expr.arg) }
    case "slam": return { tag: "slam", params: expr.params, body: stripPos(expr.body) }
    case "spi": return { tag: "spi", name: expr.name, domain: stripPos(expr.domain), codomain: stripPos(expr.codomain) }
  }
}

// --- Multi-line definition merging ---

export function mergeDefinitions(content: string): { text: string, startLine: number }[] {
  const lines = content.split("\n")
  const blocks: { text: string, startLine: number }[] = []
  let current: { text: string, startLine: number } | null = null

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim()

    if (!trimmed) {
      if (current) { blocks.push(current); current = null }
      continue
    }
    if (trimmed.startsWith("--")) {
      if (current) { blocks.push(current); current = null }
      blocks.push({ text: lines[i], startLine: i + 1 })
      continue
    }
    if (trimmed.startsWith("let ") || trimmed === "let") {
      if (current) blocks.push(current)
      current = { text: lines[i], startLine: i + 1 }
      continue
    }
    if (current) {
      current.text += "\n" + lines[i]
    } else {
      current = { text: lines[i], startLine: i + 1 }
    }
  }

  if (current) blocks.push(current)
  return blocks
}

// --- Pretty printer ---

export function printExpr(expr: SExpr): string {
  const literal = recognizeLiteral(expr)
  if (literal !== null) return literal
  switch (expr.tag) {
    case "svar": return expr.name
    case "stype": return "Type"
    case "stree": return "Tree"
    case "sapp": {
      const func = needsParensAsFunc(expr.func) ? `(${printExpr(expr.func)})` : printExpr(expr.func)
      const arg = printAtom(expr.arg)
      return `${func} ${arg}`
    }
    case "slam":
      return `{${expr.params.join(" ")}} -> ${printExpr(expr.body)}`
    case "spi":
      if (expr.name === "_") {
        const dom = printAtom(expr.domain)
        return `${dom} -> ${printExpr(expr.codomain)}`
      }
      return `(${expr.name} : ${printExpr(expr.domain)}) -> ${printExpr(expr.codomain)}`
  }
}

function needsParensAsFunc(expr: SExpr): boolean {
  return expr.tag === "spi" || expr.tag === "slam"
}

function printAtom(expr: SExpr): string {
  if (expr.tag === "svar" || expr.tag === "stype" || expr.tag === "stree") return printExpr(expr)
  const literal = recognizeLiteral(expr)
  if (literal !== null) return literal
  return `(${printExpr(expr)})`
}
