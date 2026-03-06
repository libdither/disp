// Surface syntax parser for Disp.
// Produces a named-variable AST (not bind-trees — that's ast.ts).
//
// Syntax:
//   let id : (A : Type) -> A -> A := {A x} -> x
//   {x y} -> body         multi-param lambda (sugar for nested)
//   (A : Type) -> B        dependent function type (Pi)
//   A -> B                 non-dependent function type
//   f x                    application (juxtaposition)
//   Type                   the sort

// --- Source positions ---

export type Span = { start: number, end: number }

// --- Surface AST ---

export type SExpr =
  | { tag: "svar", name: string, pos?: Span }
  | { tag: "sapp", func: SExpr, arg: SExpr, pos?: Span }
  | { tag: "slam", params: string[], body: SExpr, pos?: Span }
  | { tag: "spi", name: string, domain: SExpr, codomain: SExpr, pos?: Span }
  | { tag: "stype", pos?: Span }

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
  | { tag: "kw_true", pos: Span }
  | { tag: "kw_false", pos: Span }
  | { tag: "comma", pos: Span }    // ,
  | { tag: "langle", pos: Span }   // <
  | { tag: "rangle", pos: Span }   // >
  | { tag: "pipe", pos: Span }     // |
  | { tag: "eof", pos: Span }

export class ParseError extends Error {
  constructor(msg: string, public span: Span) {
    super(msg)
  }
}

export function tokenize(input: string): Token[] {
  const tokens: Token[] = []
  let i = 0

  while (i < input.length) {
    // Skip whitespace
    if (/\s/.test(input[i])) { i++; continue }

    // Skip line comments
    if (input[i] === '-' && input[i + 1] === '-') {
      while (i < input.length && input[i] !== '\n') i++
      continue
    }

    // Two-character tokens
    if (input[i] === '-' && input[i + 1] === '>') {
      tokens.push({ tag: "arrow", pos: { start: i, end: i + 2 } }); i += 2; continue
    }
    if (input[i] === ':' && input[i + 1] === '=') {
      tokens.push({ tag: "coloneq", pos: { start: i, end: i + 2 } }); i += 2; continue
    }

    // Single-character tokens
    const s = i
    if (input[i] === '{') { tokens.push({ tag: "lbrace", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === '}') { tokens.push({ tag: "rbrace", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === '(') { tokens.push({ tag: "lparen", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === ')') { tokens.push({ tag: "rparen", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === ':') { tokens.push({ tag: "colon", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === ',') { tokens.push({ tag: "comma", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === '<') { tokens.push({ tag: "langle", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === '>') { tokens.push({ tag: "rangle", pos: { start: s, end: s + 1 } }); i++; continue }
    if (input[i] === '|') { tokens.push({ tag: "pipe", pos: { start: s, end: s + 1 } }); i++; continue }

    // Number literals
    if (/[0-9]/.test(input[i])) {
      let start = i
      while (i < input.length && /[0-9]/.test(input[i])) i++
      tokens.push({ tag: "num", value: parseInt(input.substring(start, i), 10), pos: { start, end: i } })
      continue
    }

    // Identifiers and keywords
    if (/[a-zA-Z_]/.test(input[i])) {
      let start = i
      while (i < input.length && /[a-zA-Z0-9_']/.test(input[i])) i++
      const word = input.substring(start, i)
      const pos: Span = { start, end: i }
      if (word === "let") tokens.push({ tag: "kw_let", pos })
      else if (word === "Type") tokens.push({ tag: "kw_type", pos })
      else if (word === "true") tokens.push({ tag: "kw_true", pos })
      else if (word === "false") tokens.push({ tag: "kw_false", pos })
      else tokens.push({ tag: "ident", value: word, pos })
      continue
    }

    throw new ParseError(`Unexpected character: '${input[i]}'`, { start: i, end: i + 1 })
  }

  tokens.push({ tag: "eof", pos: { start: i, end: i } })
  return tokens
}

// --- Internal synthetic variable names ---
// These names use a '#' prefix that the tokenizer does not accept as an identifier
// start character, so they can never collide with user-defined names.
export const REC_TYPE_VAR = "#R"     // record type: (R : Type) -> ... -> R
export const REC_FN_VAR   = "#f"     // record value: {R f} -> f v1 v2 ...
export const COP_TYPE_VAR = "#R"     // coproduct type: (R : Type) -> ... -> R

// --- Parser ---

class Parser {
  private pos = 0

  constructor(private tokens: Token[]) {}

  private peek(): Token {
    return this.tokens[this.pos]
  }

  private advance(): Token {
    const tok = this.tokens[this.pos]
    this.pos++
    return tok
  }

  private expect(tag: Token["tag"]): Token {
    const tok = this.peek()
    if (tok.tag !== tag) {
      throw new ParseError(`Expected ${tag}, got ${tok.tag}`, tok.pos)
    }
    return this.advance()
  }

  private match(tag: Token["tag"]): boolean {
    if (this.peek().tag === tag) {
      this.advance()
      return true
    }
    return false
  }

  // Span from startPos to the end of the last consumed token
  private spanFrom(start: Span): Span {
    return { start: start.start, end: this.tokens[this.pos - 1].pos.end }
  }

  // Parse a top-level declaration:
  // let name : type := value
  // let name := value
  parseDecl(): SDecl {
    this.expect("kw_let")

    // Check for 'rec' contextual keyword (just an ident with value "rec")
    let isRec = false
    if (this.peek().tag === "ident" && (this.peek() as { tag: "ident", value: string }).value === "rec") {
      isRec = true
      this.advance()
    }

    const nameTok = this.expect("ident")
    const name = (nameTok as { tag: "ident", value: string }).value

    let type: SExpr | null = null
    if (this.match("colon")) {
      type = this.parseExpr()
    }

    this.expect("coloneq")
    const value = this.parseExpr()

    return { name, type, value, isRec }
  }

  // Parse an expression. Handles arrows (Pi types and non-dependent function types).
  parseExpr(): SExpr {
    return this.parseArrow()
  }

  // Arrow: either a Pi type or application-level expr
  // (x : A) -> B     — dependent Pi
  // A -> B           — non-dependent function type
  // {params} -> body — lambda
  // {x : A, y : B}   — record type
  // {x := a, y := b} — record value
  private parseArrow(): SExpr {
    // Check for brace expression (lambda, record type, or record value)
    if (this.peek().tag === "lbrace") {
      return this.parseBraceExpr()
    }

    // Check for dependent Pi: (name : domain) -> codomain
    if (this.peek().tag === "lparen") {
      const saved = this.pos
      if (this.tryParsePi()) {
        this.pos = saved
        return this.parsePi()
      }
      this.pos = saved
    }

    // Application-level, then possibly -> for non-dependent function type
    const left = this.parseApp()

    if (this.match("arrow")) {
      const right = this.parseArrow()
      const span: Span = { start: left.pos?.start ?? 0, end: right.pos?.end ?? 0 }
      return spi("_", left, right, span)
    }

    return left
  }

  // Check if we're at (name : type) -> ...
  private tryParsePi(): boolean {
    try {
      this.expect("lparen")
      const tok = this.peek()
      if (tok.tag !== "ident") return false
      this.advance()
      if (this.peek().tag !== "colon") return false
      // It looks like a Pi binding
      return true
    } catch {
      return false
    }
  }

  // Parse (name : domain) -> codomain
  private parsePi(): SExpr {
    const start = this.peek().pos
    this.expect("lparen")
    const nameTok = this.expect("ident")
    const name = (nameTok as { tag: "ident", value: string }).value
    this.expect("colon")
    const domain = this.parseExpr()
    this.expect("rparen")
    this.expect("arrow")
    const codomain = this.parseArrow()
    return spi(name, domain, codomain, this.spanFrom(start))
  }

  // Disambiguate { ... } — three forms:
  //   { name : type, ... }   → record type
  //   { name := expr, ... }  → record value
  //   { params } -> body     → lambda
  private parseBraceExpr(): SExpr {
    const saved = this.pos
    this.expect("lbrace")
    if (this.peek().tag === "ident") {
      const identPos = this.pos
      this.advance() // consume ident
      const next = this.peek().tag
      this.pos = saved // restore
      if (next === "colon") {
        return this.parseRecordType()
      }
      if (next === "coloneq") {
        return this.parseRecordValue()
      }
    } else {
      this.pos = saved
    }
    return this.parseLambda()
  }

  // Parse {x : A, y : B} → (R$rec : Type) -> ((x : A) -> (y : B) -> R$rec) -> R$rec
  private parseRecordType(): SExpr {
    const start = this.peek().pos
    this.expect("lbrace")
    const fields: { name: string, type: SExpr }[] = []
    while (this.peek().tag !== "rbrace") {
      if (fields.length > 0) this.expect("comma")
      const nameTok = this.expect("ident")
      const name = (nameTok as { tag: "ident", value: string }).value
      this.expect("colon")
      const type = this.parseExpr()
      fields.push({ name, type })
    }
    this.expect("rbrace")

    if (fields.length === 0) {
      throw new ParseError("Record type must have at least one field", this.peek().pos)
    }

    const span = this.spanFrom(start)

    // Build inner Pi: (x : A) -> (y : B) -> ... -> R$rec
    let innerPi: SExpr = svar(REC_TYPE_VAR)
    for (let i = fields.length - 1; i >= 0; i--) {
      innerPi = spi(fields[i].name, fields[i].type, innerPi)
    }

    // (R$rec : Type) -> ((x : A) -> (y : B) -> R$rec) -> R$rec
    return spi(REC_TYPE_VAR, stype, spi("_", innerPi, svar(REC_TYPE_VAR)), span)
  }

  // Parse {x := a, y := b} → {R$rec f$rec} -> f$rec a b
  private parseRecordValue(): SExpr {
    const start = this.peek().pos
    this.expect("lbrace")
    const fields: { name: string, value: SExpr }[] = []
    while (this.peek().tag !== "rbrace") {
      if (fields.length > 0) this.expect("comma")
      const nameTok = this.expect("ident")
      const _name = (nameTok as { tag: "ident", value: string }).value
      this.expect("coloneq")
      const value = this.parseExpr()
      fields.push({ name: _name, value })
    }
    this.expect("rbrace")

    if (fields.length === 0) {
      throw new ParseError("Record value must have at least one field", this.peek().pos)
    }

    const span = this.spanFrom(start)

    // Build: {R$rec f$rec} -> f$rec v1 v2 ...
    let body: SExpr = svar(REC_FN_VAR)
    for (const field of fields) {
      body = sapp(body, field.value)
    }
    return slam([REC_TYPE_VAR, REC_FN_VAR], body, span)
  }

  // Check if position is at the start of a record (not a lambda)
  private isRecordStart(): boolean {
    if (this.peek().tag !== "lbrace") return false
    const saved = this.pos
    this.advance() // consume {
    if (this.peek().tag === "ident") {
      this.advance() // consume ident
      const next = this.peek().tag
      this.pos = saved
      return next === "colon" || next === "coloneq"
    }
    this.pos = saved
    return false
  }

  // Parse <Left : A | Right : B> → (R$cop : Type) -> (A -> R$cop) -> (B -> R$cop) -> R$cop
  private parseCoproductType(): SExpr {
    const start = this.peek().pos
    this.expect("langle")
    const variants: { name: string, type: SExpr }[] = []
    while (this.peek().tag !== "rangle") {
      if (variants.length > 0) this.expect("pipe")
      const nameTok = this.expect("ident")
      const _name = (nameTok as { tag: "ident", value: string }).value
      this.expect("colon")
      const type = this.parseExpr()
      variants.push({ name: _name, type })
    }
    this.expect("rangle")

    if (variants.length === 0) {
      throw new ParseError("Coproduct type must have at least one variant", this.peek().pos)
    }

    const span = this.spanFrom(start)

    // Build: (R$cop : Type) -> (A -> R$cop) -> (B -> R$cop) -> ... -> R$cop
    let result: SExpr = svar(COP_TYPE_VAR)
    for (let i = variants.length - 1; i >= 0; i--) {
      result = spi("_", spi("_", variants[i].type, svar(COP_TYPE_VAR)), result)
    }
    return spi(COP_TYPE_VAR, stype, result, span)
  }

  // Parse {x y z} -> body  (sugar for nested lambdas)
  private parseLambda(): SExpr {
    const start = this.peek().pos
    this.expect("lbrace")
    const params: string[] = []
    while (this.peek().tag === "ident") {
      const tok = this.advance() as { tag: "ident", value: string }
      params.push(tok.value)
    }
    this.expect("rbrace")
    this.expect("arrow")
    const body = this.parseArrow()

    if (params.length === 0) {
      throw new ParseError("Lambda must have at least one parameter", start)
    }

    return slam(params, body, this.spanFrom(start))
  }

  // Application: atom atom atom ... (left-associative)
  private parseApp(): SExpr {
    let func = this.parseAtom()
    while (this.isAtomStart()) {
      const arg = this.parseAtom()
      const span: Span = { start: func.pos?.start ?? 0, end: arg.pos?.end ?? 0 }
      func = sapp(func, arg, span)
    }
    return func
  }

  private isAtomStart(): boolean {
    const tag = this.peek().tag
    if (tag === "ident" || tag === "kw_type" || tag === "lparen"
      || tag === "num" || tag === "kw_true" || tag === "kw_false"
      || tag === "langle") return true
    if (tag === "lbrace" && this.isRecordStart()) return true
    return false
  }

  // Atom: identifier, Type, number, boolean, or parenthesized expression
  private parseAtom(): SExpr {
    const tok = this.peek()

    if (tok.tag === "ident") {
      this.advance()
      return svar((tok as { tag: "ident", value: string }).value, tok.pos)
    }

    if (tok.tag === "kw_type") {
      this.advance()
      return { tag: "stype", pos: tok.pos }
    }

    if (tok.tag === "kw_true") {
      this.advance()
      // true = {R t f} -> t
      return slam(["R", "t", "f"], svar("t"), tok.pos)
    }

    if (tok.tag === "kw_false") {
      this.advance()
      // false = {R t f} -> f
      return slam(["R", "t", "f"], svar("f"), tok.pos)
    }

    if (tok.tag === "num") {
      this.advance()
      return churchNumeralExpr((tok as { tag: "num", value: number }).value, tok.pos)
    }

    if (tok.tag === "lparen") {
      const start = tok.pos
      this.advance()
      const expr = this.parseExpr()
      this.expect("rparen")
      // Parenthesized expression gets the span of the whole parens
      const span = this.spanFrom(start)
      // Propagate the span to the inner expression
      return { ...expr, pos: span }
    }

    if (tok.tag === "lbrace" && this.isRecordStart()) {
      return this.parseBraceExpr()
    }

    if (tok.tag === "langle") {
      return this.parseCoproductType()
    }

    throw new ParseError(`Unexpected token: ${tok.tag}`, tok.pos)
  }

  // Parse a single line: either a declaration (starts with 'let') or an expression
  parseLine(): SDecl | SExpr {
    if (this.peek().tag === "kw_let") {
      return this.parseDecl()
    }
    return this.parseExpr()
  }
}

// --- Church encoding helpers ---

// Build Church numeral n = {R s z} -> s^n(z)
function churchNumeralExpr(n: number, pos?: Span): SExpr {
  let body: SExpr = svar("z")
  for (let i = 0; i < n; i++) {
    body = sapp(svar("s"), body)
  }
  return slam(["R", "s", "z"], body, pos)
}

// Recognize Church-encoded literals in SExpr for pretty printing
export function recognizeChurchLiteral(expr: SExpr): string | null {
  if (expr.tag !== "slam") return null
  if (expr.params.length !== 3) return null

  const [_r, s, z] = expr.params

  // Check for true: body is just the 2nd param (distinct from all numerals)
  if (expr.body.tag === "svar" && expr.body.name === s) return "true"

  // Check for Church numeral pattern: s^n(z)
  let body = expr.body
  let count = 0
  while (body.tag === "sapp" && body.func.tag === "svar" && body.func.name === s) {
    count++
    body = body.arg
  }
  if (body.tag === "svar" && body.name === z) {
    // 0 is structurally identical to false; prefer "false" for display
    if (count === 0) return "false"
    return String(count)
  }

  return null
}

// --- Utility: strip positions for test comparisons ---

export function stripPos(expr: SExpr): SExpr {
  switch (expr.tag) {
    case "svar": return { tag: "svar", name: expr.name }
    case "stype": return { tag: "stype" }
    case "sapp": return { tag: "sapp", func: stripPos(expr.func), arg: stripPos(expr.arg) }
    case "slam": return { tag: "slam", params: expr.params, body: stripPos(expr.body) }
    case "spi": return { tag: "spi", name: expr.name, domain: stripPos(expr.domain), codomain: stripPos(expr.codomain) }
  }
}

// --- Public API ---

export function parseExpr(input: string): SExpr {
  const tokens = tokenize(input)
  const parser = new Parser(tokens)
  const result = parser.parseLine()
  if ("tag" in result) return result
  return result.value
}

export function parseLine(input: string): SDecl | SExpr {
  const tokens = tokenize(input)
  const parser = new Parser(tokens)
  return parser.parseLine()
}

// --- Pretty printer (for round-trip testing) ---

export function printExpr(expr: SExpr): string {
  switch (expr.tag) {
    case "svar": return expr.name
    case "stype": return "Type"
    case "sapp": {
      const func = printExpr(expr.func)
      const arg = printAtom(expr.arg)
      return `${func} ${arg}`
    }
    case "slam": {
      const literal = recognizeChurchLiteral(expr)
      if (literal !== null) return literal
      return `{${expr.params.join(" ")}} -> ${printExpr(expr.body)}`
    }
    case "spi": {
      if (expr.name === "_") {
        // Non-dependent function type
        const dom = printAtom(expr.domain)
        return `${dom} -> ${printExpr(expr.codomain)}`
      }
      return `(${expr.name} : ${printExpr(expr.domain)}) -> ${printExpr(expr.codomain)}`
    }
  }
}

function printAtom(expr: SExpr): string {
  if (expr.tag === "svar" || expr.tag === "stype") return printExpr(expr)
  if (expr.tag === "slam" && recognizeChurchLiteral(expr) !== null) return printExpr(expr)
  return `(${printExpr(expr)})`
}
