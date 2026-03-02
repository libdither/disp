// Interactive REPL for Disp.
// Pipeline: parse → type-check → compile → evaluate → print

import * as readline from "node:readline"
import { parseLine, printExpr, type SExpr, type SDecl } from "./parse.js"
import { checkDecl, infer, check, type Context, whnfSExpr, TypeError } from "./typecheck.js"
import { compile } from "./compile.js"
import { apply, prettyTree, type Tree, BudgetExhausted } from "./tree.js"

export type ReplState = {
  ctx: Context
  defs: Map<string, Tree>     // compiled definitions
  defExprs: Map<string, SExpr> // source-level definitions (for display)
}

export function initialState(): ReplState {
  return { ctx: [], defs: new Map(), defExprs: new Map() }
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
  const { name, type, value } = decl

  // Type check
  const result = checkDecl(state.ctx, name, type, value)
  state.ctx = result.ctx

  // Compile
  const compiled = compile(value, state.defs)
  state.defs.set(name, compiled)
  state.defExprs.set(name, value)

  const typeStr = printExpr(result.type)
  return `${name} : ${typeStr}`
}

function handleExpr(state: ReplState, expr: SExpr): string {
  // Type check
  const type = infer(state.ctx, expr)

  // Compile and evaluate
  const compiled = compile(expr, state.defs)

  // Try to recognize the result
  const display = recognizeTree(compiled, state)
  const typeStr = printExpr(type)

  if (display) {
    return `${display} : ${typeStr}`
  }
  return `${prettyTree(compiled)} : ${typeStr}`
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
      ].join("\n")

    default:
      return `Unknown command: ${cmd}. Try :help`
  }
}

// Try to recognize common Church-encoded values
function recognizeTree(tree: Tree, state: ReplState): string | null {
  // Check against known definitions
  for (const [name, def] of state.defs) {
    if (tree.id === def.id) return name
  }

  // Try to recognize Church booleans
  const boolResult = tryChurchBool(tree, state)
  if (boolResult !== null) return boolResult

  // Try to recognize Church numerals
  const natResult = tryChurchNat(tree, state)
  if (natResult !== null) return natResult

  return null
}

function tryChurchBool(tree: Tree, state: ReplState): string | null {
  try {
    const budget = { remaining: 1000 }
    // Church true R t f = t, false R t f = f
    // Use distinguishable test values
    const r = { tag: "leaf" as const, id: -1 } // dummy
    const t = { tag: "stem" as const, child: { tag: "leaf" as const, id: -2 }, id: -3 }
    const f = { tag: "stem" as const, child: { tag: "stem" as const, child: { tag: "leaf" as const, id: -4 }, id: -5 }, id: -6 }

    // Actually use real trees
    const tVal = state.defs.get("true")
    const fVal = state.defs.get("false")
    if (!tVal || !fVal) return null

    // Can't easily test without running — just compare against known values
    if (tree.id === tVal.id) return "true"
    if (tree.id === fVal.id) return "false"
    return null
  } catch {
    return null
  }
}

function tryChurchNat(tree: Tree, state: ReplState): string | null {
  const zero = state.defs.get("zero")
  if (!zero) return null

  if (tree.id === zero.id) return "0"

  // Try applying succ repeatedly
  const succ = state.defs.get("succ")
  if (!succ) return null

  try {
    let current = zero
    for (let n = 1; n <= 20; n++) {
      const budget = { remaining: 10000 }
      current = apply(succ, current, budget)
      if (tree.id === current.id) return String(n)
    }
  } catch {
    // Budget exceeded or other error
  }

  return null
}

// --- Main REPL loop ---

export async function runRepl(): Promise<void> {
  const state = initialState()

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
