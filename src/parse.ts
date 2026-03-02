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

// --- Surface AST ---

export type SExpr =
  | { tag: "svar", name: string }
  | { tag: "sapp", func: SExpr, arg: SExpr }
  | { tag: "slam", params: string[], body: SExpr }
  | { tag: "spi", name: string, domain: SExpr, codomain: SExpr }
  | { tag: "stype" }

export type SDecl = {
  name: string
  type: SExpr | null  // null if no annotation
  value: SExpr
}

export function svar(name: string): SExpr { return { tag: "svar", name } }
export function sapp(func: SExpr, arg: SExpr): SExpr { return { tag: "sapp", func, arg } }
export function slam(params: string[], body: SExpr): SExpr { return { tag: "slam", params, body } }
export function spi(name: string, domain: SExpr, codomain: SExpr): SExpr { return { tag: "spi", name, domain, codomain } }
export const stype: SExpr = { tag: "stype" }

// --- Tokenizer ---

export type Token =
  | { tag: "ident", value: string }
  | { tag: "lbrace" }   // {
  | { tag: "rbrace" }   // }
  | { tag: "lparen" }   // (
  | { tag: "rparen" }   // )
  | { tag: "arrow" }    // ->
  | { tag: "colon" }    // :
  | { tag: "coloneq" }  // :=
  | { tag: "kw_let" }
  | { tag: "kw_type" }
  | { tag: "eof" }

export class ParseError extends Error {
  constructor(msg: string, public pos: number) {
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
      tokens.push({ tag: "arrow" }); i += 2; continue
    }
    if (input[i] === ':' && input[i + 1] === '=') {
      tokens.push({ tag: "coloneq" }); i += 2; continue
    }

    // Single-character tokens
    if (input[i] === '{') { tokens.push({ tag: "lbrace" }); i++; continue }
    if (input[i] === '}') { tokens.push({ tag: "rbrace" }); i++; continue }
    if (input[i] === '(') { tokens.push({ tag: "lparen" }); i++; continue }
    if (input[i] === ')') { tokens.push({ tag: "rparen" }); i++; continue }
    if (input[i] === ':') { tokens.push({ tag: "colon" }); i++; continue }

    // Identifiers and keywords
    if (/[a-zA-Z_]/.test(input[i])) {
      let start = i
      while (i < input.length && /[a-zA-Z0-9_']/.test(input[i])) i++
      const word = input.substring(start, i)
      if (word === "let") tokens.push({ tag: "kw_let" })
      else if (word === "Type") tokens.push({ tag: "kw_type" })
      else tokens.push({ tag: "ident", value: word })
      continue
    }

    throw new ParseError(`Unexpected character: '${input[i]}'`, i)
  }

  tokens.push({ tag: "eof" })
  return tokens
}

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
      throw new ParseError(`Expected ${tag}, got ${tok.tag}`, this.pos)
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

  // Parse a top-level declaration:
  // let name : type := value
  // let name := value
  parseDecl(): SDecl {
    this.expect("kw_let")
    const nameTok = this.expect("ident")
    const name = (nameTok as { tag: "ident", value: string }).value

    let type: SExpr | null = null
    if (this.match("colon")) {
      type = this.parseExpr()
    }

    this.expect("coloneq")
    const value = this.parseExpr()

    return { name, type, value }
  }

  // Parse an expression. Handles arrows (Pi types and non-dependent function types).
  parseExpr(): SExpr {
    return this.parseArrow()
  }

  // Arrow: either a Pi type or application-level expr
  // (x : A) -> B     — dependent Pi
  // A -> B           — non-dependent function type
  // {params} -> body — lambda
  private parseArrow(): SExpr {
    // Check for lambda: {params} -> body
    if (this.peek().tag === "lbrace") {
      return this.parseLambda()
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
      return spi("_", left, right)
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
    this.expect("lparen")
    const nameTok = this.expect("ident")
    const name = (nameTok as { tag: "ident", value: string }).value
    this.expect("colon")
    const domain = this.parseExpr()
    this.expect("rparen")
    this.expect("arrow")
    const codomain = this.parseArrow()
    return spi(name, domain, codomain)
  }

  // Parse {x y z} -> body  (sugar for nested lambdas)
  private parseLambda(): SExpr {
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
      throw new ParseError("Lambda must have at least one parameter", this.pos)
    }

    return slam(params, body)
  }

  // Application: atom atom atom ... (left-associative)
  private parseApp(): SExpr {
    let func = this.parseAtom()
    while (this.isAtomStart()) {
      const arg = this.parseAtom()
      func = sapp(func, arg)
    }
    return func
  }

  private isAtomStart(): boolean {
    const tag = this.peek().tag
    return tag === "ident" || tag === "kw_type" || tag === "lparen"
  }

  // Atom: identifier, Type, or parenthesized expression
  private parseAtom(): SExpr {
    const tok = this.peek()

    if (tok.tag === "ident") {
      this.advance()
      return svar((tok as { tag: "ident", value: string }).value)
    }

    if (tok.tag === "kw_type") {
      this.advance()
      return stype
    }

    if (tok.tag === "lparen") {
      this.advance()
      const expr = this.parseExpr()
      this.expect("rparen")
      return expr
    }

    throw new ParseError(`Unexpected token: ${tok.tag}`, this.pos)
  }

  // Parse a single line: either a declaration (starts with 'let') or an expression
  parseLine(): SDecl | SExpr {
    if (this.peek().tag === "kw_let") {
      return this.parseDecl()
    }
    return this.parseExpr()
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
  return `(${printExpr(expr)})`
}
