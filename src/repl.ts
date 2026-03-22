// Interactive REPL for Disp.
// Pipeline: parse → type-check (CoC-on-trees) → compile → evaluate → print

import * as readline from "node:readline"
import * as fs from "node:fs"
import * as path from "node:path"
import { parseLine, printExpr, type SExpr, type SDecl, ParseError } from "./parse.js"
import { compileAndEval, compileRecAndEval } from "./compile.js"
import { prettyTree, type Tree, LEAF, stem, treeEqual, BudgetExhausted } from "./tree.js"
import { cocCheckDecl, buildWrapped, unwrapData, unwrapType, printEncoded, CocError, type Env, TREE_TYPE, BOOL_TYPE, NAT_TYPE } from "./coc.js"
import { loadCocPrelude, buildNameMap, COC_PRELUDE, TREE_NATIVE_BUILTINS, PRIMITIVE_BUILTINS } from "./tree-native.js"

export type ReplState = {
  cocEnv: Env                   // CoC environment (name → wrapped tree)
  defs: Map<string, Tree>       // compiled definitions (for evaluation)
  defExprs: Map<string, SExpr>  // source-level definitions (for save/display)
  defIsRec: Set<string>         // names defined with 'let rec'
}

function loadCocPreludeIntoDefs(state: ReplState): void {
  // Add primitive type values to defs
  state.defs.set("Tree", TREE_TYPE)
  state.defs.set("Bool", BOOL_TYPE)
  state.defs.set("Nat",  NAT_TYPE)

  // Add primitive builtins to defs
  for (const builtin of PRIMITIVE_BUILTINS) {
    state.defs.set(builtin.name, builtin.data)
  }

  // Compile COC_PRELUDE string definitions
  for (const decl of COC_PRELUDE) {
    const parsed = parseLine(decl)
    if ("name" in parsed) {
      const sdecl = parsed as SDecl
      try {
        const compiled = sdecl.isRec
          ? compileRecAndEval(sdecl.name, sdecl.value, state.defs)
          : compileAndEval(sdecl.value, state.defs)
        state.defs.set(sdecl.name, compiled)
      } catch (e) {
        console.error(`Warning: failed to compile prelude definition '${sdecl.name}':`, e instanceof Error ? e.message : e)
      }
    }
  }

  // Add tree-native builtins to defs
  for (const builtin of TREE_NATIVE_BUILTINS) {
    state.defs.set(builtin.name, builtin.data)
  }
}

export function initialState(): ReplState {
  const state: ReplState = { cocEnv: new Map(), defs: new Map(), defExprs: new Map(), defIsRec: new Set() }
  // Auto-load CoC prelude (Tree, leaf, stem, fork, triage, enc*, wrap/unwrap, builtins)
  state.cocEnv = loadCocPrelude(state.cocEnv)
  loadCocPreludeIntoDefs(state)
  return state
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

type Span = { start: number, end: number }

function formatError(source: string, prefix: string, msg: string, span?: Span): string {
  if (!span) return `${prefix}: ${msg}`
  const { line, col } = offsetToLineCol(source, span.start)
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

    if (isDecl(parsed)) {
      return handleDecl(state, parsed)
    } else {
      return handleExpr(state, parsed)
    }
  } catch (e) {
    if (e instanceof ParseError) return formatError(trimmed, "Parse error", e.message, e.span)
    if (e instanceof CocError) return `Type error: ${e.message}`
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

  // Type check via CoC-on-trees
  const result = cocCheckDecl(state.cocEnv, name, type, value, isRec)
  state.cocEnv = result.env
  state.defExprs.set(name, value)
  if (isRec) state.defIsRec.add(name)

  // Compile and evaluate
  let compiled: Tree
  if (isRec) {
    compiled = compileRecAndEval(name, value, state.defs)
  } else {
    compiled = compileAndEval(value, state.defs)
  }
  state.defs.set(name, compiled)

  const nameMap = buildNameMap(state.cocEnv)
  const typeStr = printEncoded(result.type, nameMap)
  return `${name} : ${typeStr}`
}

function handleExpr(state: ReplState, expr: SExpr): string {
  const wrapped = buildWrapped(expr, state.cocEnv)

  const data = unwrapData(wrapped)
  const type = unwrapType(wrapped)

  const nameMap = buildNameMap(state.cocEnv)
  const typeStr = printEncoded(type, nameMap)

  // Compile and evaluate for display
  let compiled: Tree | null = null
  try {
    compiled = compileAndEval(expr, state.defs)
  } catch {
    // Compilation can fail for type-level expressions that don't reduce to trees.
    // Fall back to displaying the encoded form below.
  }

  if (compiled) {
    const display = recognizeValue(compiled, type, state)
    if (display) return `${display} : ${typeStr}`
    return `${prettyTree(compiled)} : ${typeStr}`
  }

  // Fall back to name lookup on encoded data
  const name = nameMap.get(data.id)
  if (name) return `${name} : ${typeStr}`

  const dataStr = printEncoded(data, nameMap)
  return `${dataStr} : ${typeStr}`
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
        const wrapped = buildWrapped(expr, state.cocEnv)
        const type = unwrapType(wrapped)
        const nameMap = buildNameMap(state.cocEnv)
        return printEncoded(type, nameMap)
      } catch (e) {
        return e instanceof Error ? `Error: ${e.message}` : `Error: ${e}`
      }
    }

    case ":tree": {
      if (!rest) return "Usage: :tree <expr>"
      try {
        const expr = parseLine(rest) as SExpr
        const compiled = compileAndEval(expr, state.defs)
        return prettyTree(compiled)
      } catch (e) {
        return e instanceof Error ? `Error: ${e.message}` : `Error: ${e}`
      }
    }

    case ":ctx":
    case ":context": {
      if (state.cocEnv.size === 0) return "(empty context)"
      const nameMap = buildNameMap(state.cocEnv)
      const lines: string[] = []
      for (const [name, w] of state.cocEnv) {
        const type = unwrapType(w)
        lines.push(`${name} : ${printEncoded(type, nameMap)}`)
      }
      return lines.join("\n")
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
        "  true / false                Booleans (tree-encoded)",
        "  0, 1, 2, ...                Natural numbers (tree-encoded)",
        "  Tree / Bool / Nat           Primitive types",
        "  leaf / stem / fork          Tree constructors",
      ].join("\n")

    default:
      return `Unknown command: ${cmd}. Try :help`
  }
}

// Structural recognition of primitive values
function recognizeValue(
  tree: Tree, type: Tree, state: ReplState,
): string | null {
  // Primitive Bool (true = LEAF, false = stem(LEAF))
  if (treeEqual(type, BOOL_TYPE)) {
    if (treeEqual(tree, LEAF)) return "true"
    if (treeEqual(tree, stem(LEAF))) return "false"
    return null
  }
  // Primitive Nat (zero = LEAF, succ(n) = stem(n))
  if (treeEqual(type, NAT_TYPE)) {
    let n = tree, count = 0
    while (n.tag === "stem") { count++; n = n.child }
    if (n.tag === "leaf") return String(count)
    return null
  }
  // Primitive Tree
  if (treeEqual(type, TREE_TYPE)) {
    return printTreeValue(tree)
  }
  // Name lookup
  for (const [name, def] of state.defs) {
    if (tree.id === def.id) return name
  }
  return null
}

function printTreeValue(t: Tree): string {
  if (t.tag === "leaf") return "leaf"
  if (t.tag === "stem") return `stem ${printTreeValueAtom(t.child)}`
  return `fork ${printTreeValueAtom(t.left)} ${printTreeValueAtom(t.right)}`
}

function printTreeValueAtom(t: Tree): string {
  return t.tag === "leaf" ? "leaf" : `(${printTreeValue(t)})`
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
    const nameMap = buildNameMap(state.cocEnv)
    for (const [name, wrapped] of state.cocEnv) {
      const valExpr = state.defExprs.get(name)
      if (valExpr) {
        const type = unwrapType(wrapped)
        const typeStr = printEncoded(type, nameMap)
        const valStr = printExpr(valExpr)
        const recStr = state.defIsRec.has(name) ? "rec " : ""
        lines.push(`let ${recStr}${name} : ${typeStr} := ${valStr}`)
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
