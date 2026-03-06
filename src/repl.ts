// Interactive REPL for Disp.
// Pipeline: parse → type-check → compile → evaluate → print

import * as readline from "node:readline"
import * as fs from "node:fs"
import * as path from "node:path"
import { parseLine, printExpr, recognizeChurchLiteral, spi, stype, svar, type SExpr, type SDecl, type Span, ParseError } from "./parse.js"
import { checkDecl, infer, check, convertibleSExpr, type Context, whnfSExpr, TypeError } from "./typecheck.js"
import { compile, compileAndEval, compileRecAndEval } from "./compile.js"
import { apply, prettyTree, type Tree, LEAF, stem, I, BudgetExhausted } from "./tree.js"
import { cocCheckDecl, buildWrapped, unwrapData, unwrapType, printEncoded, convertible, normalize, CocError, loadCocPrelude, buildNameMap, type Env } from "./coc.js"

export type ReplState = {
  ctx: Context
  defs: Map<string, Tree>     // compiled definitions
  defExprs: Map<string, SExpr> // source-level definitions (for display)
  defIsRec: Set<string>       // names defined with 'let rec'
  cocMode: boolean            // whether to use CoC-on-trees pipeline
  cocEnv: Env                 // CoC environment (name → wrapped tree)
}

export function initialState(): ReplState {
  return { ctx: [], defs: new Map(), defExprs: new Map(), defIsRec: new Set(), cocMode: false, cocEnv: new Map() }
}

// --- Error formatting ---

function offsetToLineCol(source: string, offset: number): { line: number, col: number } {
  let line = 1, col = 1
  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === '\n') { line++; col = 1 }
    else col++
  }
  return { line, col }
}

function formatError(source: string, prefix: string, msg: string, span?: Span): string {
  if (!span) return `${prefix}: ${msg}`
  const { line, col } = offsetToLineCol(source, span.start)
  // Find the source line containing the span start
  const lines = source.split('\n')
  const srcLine = lines[line - 1] ?? ''
  const underLen = Math.max(1, Math.min(span.end - span.start, srcLine.length - col + 1))
  const underline = ' '.repeat(col - 1) + '^'.repeat(underLen)
  return `${prefix} at ${line}:${col}: ${msg}\n  ${srcLine}\n  ${underline}`
}

// Process a single line of input. Returns the output string.
export function processLine(state: ReplState, input: string): string {
  const trimmed = input.trim()
  if (!trimmed || trimmed.startsWith("--")) return ""

  // Commands
  if (trimmed.startsWith(":")) {
    return handleCommand(state, trimmed)
  }

  try {
    const parsed = parseLine(trimmed)

    if (state.cocMode) {
      if (isDecl(parsed)) {
        return handleCocDecl(state, parsed)
      } else {
        return handleCocExpr(state, parsed)
      }
    }

    if (isDecl(parsed)) {
      return handleDecl(state, parsed)
    } else {
      return handleExpr(state, parsed)
    }
  } catch (e) {
    if (e instanceof TypeError) return formatError(trimmed, "Type error", e.message, e.span)
    if (e instanceof ParseError) return formatError(trimmed, "Parse error", e.message, e.span)
    if (e instanceof CocError) return `CoC error: ${e.message}`
    if (e instanceof BudgetExhausted) return `Error: evaluation did not terminate`
    if (e instanceof Error) return `Error: ${e.message}`
    return `Error: ${e}`
  }
}

function isDecl(x: SDecl | SExpr): x is SDecl {
  return "name" in x && "value" in x
}

function handleDecl(state: ReplState, decl: SDecl): string {
  const { name, type, value, isRec } = decl

  // Type check
  const result = checkDecl(state.ctx, name, type, value, isRec)
  state.ctx = result.ctx

  // Compile and evaluate
  let compiled: Tree
  if (isRec) {
    compiled = compileRecAndEval(name, value, state.defs)
    state.defIsRec.add(name)
  } else {
    compiled = compileAndEval(value, state.defs)
  }
  state.defs.set(name, compiled)
  state.defExprs.set(name, value)

  const typeStr = printExpr(result.type)
  return `${name} : ${typeStr}`
}

function handleExpr(state: ReplState, expr: SExpr): string {
  // Type check — try inference first, fall back for Church literals
  let type: SExpr
  try {
    type = infer(state.ctx, expr)
  } catch (e) {
    if (e instanceof TypeError && expr.tag === "slam") {
      const litType = churchLiteralType(expr)
      if (litType) {
        check(state.ctx, expr, litType)
        type = litType
      } else {
        throw e
      }
    } else {
      throw e
    }
  }

  // Compile and evaluate eagerly
  const compiled = compileAndEval(expr, state.defs)

  // Try to recognize the result, using type to disambiguate
  const display = recognizeTree(compiled, type, state)
  const typeStr = printExpr(type)

  if (display) {
    return `${display} : ${typeStr}`
  }
  return `${prettyTree(compiled)} : ${typeStr}`
}

// Return the type for a recognized Church literal, or null
function churchLiteralType(expr: SExpr): SExpr | null {
  const lit = recognizeChurchLiteral(expr)
  if (lit === null) return null
  if (lit === "true" || lit === "false") {
    // (R : Type) -> R -> R -> R
    return spi("R", stype, spi("_", svar("R"), spi("_", svar("R"), svar("R"))))
  }
  // Church numeral: (R : Type) -> (R -> R) -> R -> R
  return spi("R", stype, spi("_", spi("_", svar("R"), svar("R")), spi("_", svar("R"), svar("R"))))
}

function handleCommand(state: ReplState, input: string): string {
  const parts = input.split(/\s+/)
  const cmd = parts[0]
  const rest = parts.slice(1).join(" ")

  switch (cmd) {
    case ":type":
    case ":t": {
      if (!rest) return "Usage: :type <expr>"
      try {
        const expr = parseLine(rest) as SExpr
        const type = infer(state.ctx, expr)
        return printExpr(type)
      } catch (e) {
        return e instanceof Error ? `Error: ${e.message}` : `Error: ${e}`
      }
    }

    case ":tree": {
      if (!rest) return "Usage: :tree <expr>"
      try {
        const expr = parseLine(rest) as SExpr
        const compiled = compile(expr, state.defs)
        return prettyTree(compiled)
      } catch (e) {
        return e instanceof Error ? `Error: ${e.message}` : `Error: ${e}`
      }
    }

    case ":ctx":
    case ":context": {
      if (state.cocMode) {
        if (state.cocEnv.size === 0) return "(empty context)"
        const nameMap = buildNameMap(state.cocEnv)
        const lines: string[] = []
        for (const [name, wrapped] of state.cocEnv) {
          const type = unwrapType(wrapped)
          lines.push(`${name} : ${printEncoded(type, nameMap)}`)
        }
        return lines.join("\n")
      }
      if (state.ctx.length === 0) return "(empty context)"
      return state.ctx.map(e => `${e.name} : ${printExpr(e.type)}`).join("\n")
    }

    case ":load":
    case ":l": {
      if (!rest) return "Usage: :load <file>"
      return loadFile(state, rest)
    }

    case ":save":
    case ":s": {
      if (!rest) return "Usage: :save <file>"
      return saveFile(state, rest)
    }

    case ":coc": {
      state.cocMode = !state.cocMode
      if (state.cocMode && state.cocEnv.size === 0) {
        // Load CoC builtins (Tree, leaf, stem, fork, triage, enc*, wrap/unwrap)
        state.cocEnv = loadCocPrelude(state.cocEnv)
        // Load user prelude in coc mode if available
        const preludePath = path.resolve("prelude.disp")
        if (fs.existsSync(preludePath)) {
          loadFile(state, preludePath, true)
        }
      }
      return `CoC-on-trees mode ${state.cocMode ? "enabled" : "disabled"}`
    }

    case ":quit":
    case ":q":
      return ":quit"

    case ":help":
    case ":h":
      return [
        "Commands:",
        "  :type <expr>    Show the type of an expression",
        "  :tree <expr>    Show the compiled tree calculus form",
        "  :ctx            Show the current context",
        "  :load <file>    Load declarations from a file",
        "  :save <file>    Save declarations to a file",
        "  :coc            Toggle CoC-on-trees mode",
        "  :quit           Exit the REPL",
        "  :help           Show this help message",
        "",
        "Syntax:",
        "  let name : type := value    Define a named value",
        "  {x y} -> body               Lambda expression",
        "  (x : A) -> B                Dependent function type (Pi)",
        "  A -> B                      Function type",
        "  f x                         Application",
        "  Type                        The universe",
        "  true / false                Church booleans",
        "  0, 1, 2, ...                Church numerals",
      ].join("\n")

    default:
      return `Unknown command: ${cmd}. Try :help`
  }
}

// Canonical type shapes for Church encodings
const BOOL_TYPE = spi("R", stype, spi("_", svar("R"), spi("_", svar("R"), svar("R"))))
const NAT_TYPE = spi("R", stype, spi("_", spi("_", svar("R"), svar("R")), spi("_", svar("R"), svar("R"))))

// Core recognition: type-guided Church literal check, then name lookup, then last-resort behavioral
function recognizeChurchValue(
  tree: Tree, isBool: boolean, isNat: boolean,
  lookupName: (tree: Tree) => string | null,
): string | null {
  if (isBool) {
    const result = tryChurchBool(tree)
    if (result !== null) return result
  }
  if (isNat) {
    const result = tryChurchNat(tree)
    if (result !== null) return result
  }
  const name = lookupName(tree)
  if (name) return name
  if (!isBool && !isNat) {
    const boolResult = tryChurchBool(tree)
    if (boolResult !== null) return boolResult
    const natResult = tryChurchNat(tree)
    if (natResult !== null) return natResult
  }
  return null
}

// Try to recognize common Church-encoded values, using type to disambiguate
function recognizeTree(tree: Tree, type: SExpr, state: ReplState): string | null {
  const isBool = convertibleSExpr(type, BOOL_TYPE, state.ctx)
  const isNat = !isBool && convertibleSExpr(type, NAT_TYPE, state.ctx)
  return recognizeChurchValue(tree, isBool, isNat, (t) => {
    for (const [name, def] of state.defs) {
      if (t.id === def.id) return name
    }
    return null
  })
}

function tryChurchBool(tree: Tree): string | null {
  try {
    const budget = { remaining: 1000 }
    // Apply tree to R=LEAF, t=stem(LEAF), f=stem(stem(LEAF))
    const r = LEAF
    const t = stem(LEAF)
    const f = stem(stem(LEAF))
    const result = apply(apply(apply(tree, r, budget), t, budget), f, budget)
    if (result.id === t.id) return "true"
    if (result.id === f.id) return "false"
    return null
  } catch {
    return null
  }
}

function tryChurchNat(tree: Tree): string | null {
  try {
    const budget = { remaining: 1000 }
    // Apply tree to R=LEAF, s=I (identity), z=LEAF
    // Then count how many times stem is applied: I^n(LEAF) = LEAF always,
    // so use a different approach: s = stem, z = LEAF
    // stem^n(LEAF) gives a tree we can count
    const r = LEAF
    const s = stem(LEAF) // K: K x y = x. Applied once: K(LEAF) = stem(LEAF)...
    // Actually, let's use I and count applications
    // Better: apply tree R s z where s builds a recognizable chain
    // Use: z = LEAF, s = △ (makes stem)
    // s(z) = △(LEAF) = stem(LEAF), s(s(z)) = △(stem(LEAF)) = stem(stem(LEAF))
    const result = apply(apply(apply(tree, r, budget), LEAF, budget), LEAF, budget)
    // △(LEAF) x = stem(x). So s=LEAF means s(z) = stem(z) = stem(LEAF),
    // s(s(z)) = stem(stem(LEAF)), etc.
    // Count stem depth
    let node = result
    let count = 0
    while (node.tag === "stem") {
      count++
      node = node.child
    }
    if (node.tag === "leaf" && count >= 0) {
      return String(count)
    }
    return null
  } catch {
    return null
  }
}

// --- File operations ---

export function loadFile(state: ReplState, filePath: string, silent = false): string {
  try {
    const content = fs.readFileSync(filePath, "utf-8")
    const lines = content.split("\n")
    const results: string[] = []
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum]
      const result = processLine(state, line)
      if (result) {
        if (result.includes("error") || result.includes("Error")) {
          return `${filePath}:${lineNum + 1}: ${result}`
        }
        results.push(result)
      }
    }
    if (silent) return ""
    return results.length > 0 ? `Loaded ${filePath} (${results.length} definitions)` : `Loaded ${filePath}`
  } catch (e) {
    if (silent) return ""
    return `Error: Could not load ${filePath}: ${e instanceof Error ? e.message : e}`
  }
}

function saveFile(state: ReplState, filePath: string): string {
  try {
    const lines: string[] = []
    for (const entry of state.ctx) {
      const typeStr = printExpr(entry.type)
      const valExpr = state.defExprs.get(entry.name)
      if (valExpr) {
        const valStr = printExpr(valExpr)
        const recStr = state.defIsRec.has(entry.name) ? "rec " : ""
        lines.push(`let ${recStr}${entry.name} : ${typeStr} := ${valStr}`)
      }
    }
    fs.writeFileSync(filePath, lines.join("\n") + "\n", "utf-8")
    return `Saved ${lines.length} definitions to ${filePath}`
  } catch (e) {
    return `Error: Could not save to ${filePath}: ${e instanceof Error ? e.message : e}`
  }
}

// --- CoC-on-trees handlers ---

function handleCocDecl(state: ReplState, decl: SDecl): string {
  const { name, type, value, isRec } = decl
  const result = cocCheckDecl(state.cocEnv, name, type, value, isRec)
  state.cocEnv = result.env
  state.defExprs.set(name, value)
  if (isRec) state.defIsRec.add(name)

  const nameMap = buildNameMap(state.cocEnv)
  const typeStr = printEncoded(result.type, nameMap)
  return `${name} : ${typeStr}`
}

function handleCocExpr(state: ReplState, expr: SExpr): string {
  const wrapped = buildWrapped(expr, state.cocEnv)
  const data = unwrapData(wrapped)
  const type = unwrapType(wrapped)

  const nameMap = buildNameMap(state.cocEnv)
  const typeStr = printEncoded(type, nameMap)

  // Also compile the expression through the old pipeline for behavioral testing.
  // The CoC data is an encoded term (encLam/encApp/etc.), not a raw compiled tree.
  // To recognize Church literals, we need the compiled (raw) tree form.
  let compiled: Tree | null = null
  try {
    // Build a defs map from cocEnv for the old compiler
    const defs = new Map<string, Tree>()
    for (const [name, w] of state.cocEnv) {
      // Only include user-defined values that were compiled, not builtins
      if (state.defExprs.has(name)) {
        try { defs.set(name, compileAndEval(state.defExprs.get(name)!, defs)) } catch {}
      }
    }
    compiled = compileAndEval(expr, defs)
  } catch {}

  if (compiled) {
    const display = recognizeCocValue(compiled, type, state)
    if (display) return `${display} : ${typeStr}`
  }

  // Fall back to name lookup on encoded data
  const name = nameMap.get(data.id)
  if (name) return `${name} : ${typeStr}`

  const dataStr = printEncoded(data, nameMap)
  return `${dataStr} : ${typeStr}`
}

function recognizeCocValue(compiled: Tree, type: Tree, state: ReplState): string | null {
  const boolEntry = state.cocEnv.get("Bool")
  const natEntry = state.cocEnv.get("Nat")
  const isBool = !!(boolEntry && convertible(type, unwrapData(boolEntry)))
  const isNat = !isBool && !!(natEntry && convertible(type, unwrapData(natEntry)))
  return recognizeChurchValue(compiled, isBool, isNat, (t) => {
    for (const [name, w] of state.cocEnv) {
      if (unwrapData(w).id === t.id) return name
    }
    return null
  })
}

// --- Main REPL loop ---

export async function runRepl(): Promise<void> {
  const state = initialState()

  // Auto-load prelude if it exists
  const preludePath = path.resolve("prelude.disp")
  if (fs.existsSync(preludePath)) {
    loadFile(state, preludePath, true)
  }

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: "> ",
  })

  console.log("Disp - Tree Calculus Runtime")
  console.log("Type :help for available commands, :quit to exit\n")

  rl.prompt()

  for await (const line of rl) {
    const result = processLine(state, line)

    if (result === ":quit") {
      rl.close()
      break
    }

    if (result) console.log(result)
    console.log()
    rl.prompt()
  }
}
