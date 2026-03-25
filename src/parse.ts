// Surface syntax parser for Disp.
// Produces a named-variable AST (SExpr) from source text.
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
      else if (word === "Tree") tokens.push({ tag: "kw_tree", pos })
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

  private identValue(tok: Token): string {
    return (tok as { tag: "ident", value: string }).value
  }

  private numValue(tok: Token): number {
    return (tok as { tag: "num", value: number }).value
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
    if (this.peek().tag === "ident" && this.identValue(this.peek()) === "rec") {
      isRec = true
      this.advance()
    }

    const nameTok = this.expect("ident")
    const name = this.identValue(nameTok)

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
    if (this.peek().tag !== "lparen") return false
    this.advance() // consume (
    if (this.peek().tag !== "ident") return false
    this.advance() // consume ident
    return this.peek().tag === "colon"
  }

  // Parse (name : domain) -> codomain
  private parsePi(): SExpr {
    const start = this.peek().pos
    this.expect("lparen")
    const name = this.identValue(this.expect("ident"))
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
    const kind = this.peekBraceKind()
    if (kind === "record_type") return this.parseRecordType()
    if (kind === "record_value") return this.parseRecordValue()
    return this.parseLambda()
  }

  // Lookahead to determine what a { ... } expression is:
  //   { name : ... } → record type
  //   { name := ... } → record value
  //   { params } -> body → lambda
  private peekBraceKind(): "record_type" | "record_value" | "lambda" {
    const saved = this.pos
    this.advance() // consume {
    if (this.peek().tag === "ident") {
      this.advance() // consume ident
      const next = this.peek().tag
      this.pos = saved
      if (next === "colon") return "record_type"
      if (next === "coloneq") return "record_value"
      return "lambda"
    }
    this.pos = saved
    return "lambda"
  }

  // Parse {x : A, y : B} → (R$rec : Type) -> ((x : A) -> (y : B) -> R$rec) -> R$rec
  private parseRecordType(): SExpr {
    const start = this.peek().pos
    this.expect("lbrace")
    const fields: { name: string, type: SExpr }[] = []
    while (this.peek().tag !== "rbrace") {
      if (fields.length > 0) this.expect("comma")
      const name = this.identValue(this.expect("ident"))
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
      const name = this.identValue(this.expect("ident"))
      this.expect("coloneq")
      const value = this.parseExpr()
      fields.push({ name, value })
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

  private isRecordStart(): boolean {
    if (this.peek().tag !== "lbrace") return false
    const kind = this.peekBraceKind()
    return kind === "record_type" || kind === "record_value"
  }

  // Parse <Left : A | Right : B> → (R$cop : Type) -> (A -> R$cop) -> (B -> R$cop) -> R$cop
  private parseCoproductType(): SExpr {
    const start = this.peek().pos
    this.expect("langle")
    const variants: { name: string, type: SExpr }[] = []
    while (this.peek().tag !== "rangle") {
      if (variants.length > 0) this.expect("pipe")
      const name = this.identValue(this.expect("ident"))
      this.expect("colon")
      const type = this.parseExpr()
      variants.push({ name, type })
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
      params.push(this.identValue(this.advance()))
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
    if (tag === "ident" || tag === "kw_type" || tag === "kw_tree" || tag === "lparen"
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
      return svar(this.identValue(tok), tok.pos)
    }

    if (tok.tag === "kw_type") {
      this.advance()
      return { tag: "stype", pos: tok.pos }
    }

    if (tok.tag === "kw_tree") {
      this.advance()
      return { tag: "stree", pos: tok.pos }
    }

    if (tok.tag === "kw_true") {
      this.advance()
      return svar("true", tok.pos)
    }

    if (tok.tag === "kw_false") {
      this.advance()
      return svar("false", tok.pos)
    }

    if (tok.tag === "num") {
      this.advance()
      return treeNumeralExpr(this.numValue(tok), tok.pos)
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

// --- Tree-encoded literal helpers ---

// Build tree-encoded numeral n = succ^n(zero)
function treeNumeralExpr(n: number, pos?: Span): SExpr {
  let body: SExpr = svar("zero")
  for (let i = 0; i < n; i++) {
    body = sapp(svar("succ"), body, pos)
  }
  return body
}

// Recognize tree-encoded literals in SExpr for pretty printing
export function recognizeLiteral(expr: SExpr): string | null {
  if (expr.tag === "svar") {
    if (expr.name === "true") return "true"
    if (expr.name === "false") return "false"
    if (expr.name === "zero") return "0"
    if (expr.name === "leaf") return "leaf"
  }
  // Count succ/stem applications for numerals
  let count = 0
  let e: SExpr = expr
  while (e.tag === "sapp" && e.func.tag === "svar" && e.func.name === "succ") {
    count++
    e = e.arg
  }
  if (e.tag === "svar" && e.name === "zero" && count > 0) return String(count)
  return null
}

// --- Utility: strip positions for test comparisons ---

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

// Groups file content into logical blocks for parsing. Each block is either:
// - A `let` definition (possibly spanning multiple lines)
// - A standalone comment line
// - A standalone expression
// Continuation lines (not starting with `let`, not comments, not blank) are
// appended to the current block. The parser already handles multi-line input;
// this function just bridges the gap between line-oriented file reading and
// the token-oriented parser.
export function mergeDefinitions(content: string): { text: string, startLine: number }[] {
  const lines = content.split("\n")
  const blocks: { text: string, startLine: number }[] = []
  let current: { text: string, startLine: number } | null = null

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim()

    // Blank lines end the current block
    if (!trimmed) {
      if (current) { blocks.push(current); current = null }
      continue
    }

    // Comment lines are standalone blocks
    if (trimmed.startsWith("--")) {
      if (current) { blocks.push(current); current = null }
      blocks.push({ text: lines[i], startLine: i + 1 })
      continue
    }

    // `let` starts a new block
    if (trimmed.startsWith("let ") || trimmed === "let") {
      if (current) blocks.push(current)
      current = { text: lines[i], startLine: i + 1 }
      continue
    }

    // Continuation line: append to current block, or standalone expression
    if (current) {
      current.text += "\n" + lines[i]
    } else {
      // Standalone expression (not starting with `let`)
      current = { text: lines[i], startLine: i + 1 }
    }
  }

  if (current) blocks.push(current)
  return blocks
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
    case "slam": {
      return `{${expr.params.join(" ")}} -> ${printExpr(expr.body)}`
    }
    case "spi": {
      if (expr.name === "_") {
        const dom = printAtom(expr.domain)
        return `${dom} -> ${printExpr(expr.codomain)}`
      }
      return `(${expr.name} : ${printExpr(expr.domain)}) -> ${printExpr(expr.codomain)}`
    }
  }
}

function needsParensAsFunc(expr: SExpr): boolean {
  if (expr.tag === "spi") return true
  if (expr.tag === "slam") return true
  return false
}

function printAtom(expr: SExpr): string {
  if (expr.tag === "svar" || expr.tag === "stype" || expr.tag === "stree") return printExpr(expr)
  const literal = recognizeLiteral(expr)
  if (literal !== null) return literal
  return `(${printExpr(expr)})`
}
