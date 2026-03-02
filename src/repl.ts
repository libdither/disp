// Interactive REPL for Disp.
// Pipeline: parse → type-check → compile → evaluate → print

import * as readline from "node:readline"
import * as fs from "node:fs"
import * as path from "node:path"
import { parseLine, printExpr, recognizeChurchLiteral, spi, stype, svar, type SExpr, type SDecl } from "./parse.js"
import { checkDecl, infer, check, convertibleSExpr, type Context, whnfSExpr, TypeError } from "./typecheck.js"
import { compile, compileAndEval, compileRecAndEval } from "./compile.js"
import { apply, prettyTree, type Tree, LEAF, stem, I, BudgetExhausted } from "./tree.js"

export type ReplState = {
  ctx: Context
  defs: Map<string, Tree>     // compiled definitions
  defExprs: Map<string, SExpr> // source-level definitions (for display)
  defIsRec: Set<string>       // names defined with 'let rec'
}

export function initialState(): ReplState {
  return { ctx: [], defs: new Map(), defExprs: new Map(), defIsRec: new Set() }
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

    if (isDecl(parsed)) {
      return handleDecl(state, parsed)
    } else {
      return handleExpr(state, parsed)
    }
  } catch (e) {
    if (e instanceof TypeError) return `Type error: ${e.message}`
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

// Try to recognize common Church-encoded values, using type to disambiguate
function recognizeTree(tree: Tree, type: SExpr, state: ReplState): string | null {
  // Use type to guide recognition — this avoids collisions like id/1 or zero/false
  const isBool = convertibleSExpr(type, BOOL_TYPE, state.ctx)
  const isNat = !isBool && convertibleSExpr(type, NAT_TYPE, state.ctx)

  if (isBool) {
    const result = tryChurchBool(tree)
    if (result !== null) return result
  }

  if (isNat) {
    const result = tryChurchNat(tree)
    if (result !== null) return result
  }

  // Fall back to definition name matching (skip Bool/Nat-typed defs to avoid collisions)
  for (const [name, def] of state.defs) {
    if (tree.id === def.id) return name
  }

  // Last resort: behavioral tests if type didn't match known patterns
  if (!isBool && !isNat) {
    const boolResult = tryChurchBool(tree)
    if (boolResult !== null) return boolResult
    const natResult = tryChurchNat(tree)
    if (natResult !== null) return natResult
  }

  return null
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
    for (const line of lines) {
      const result = processLine(state, line)
      if (result) {
        if (result.startsWith("Error:") || result.startsWith("Type error:")) {
          return result
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
