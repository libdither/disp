// Interactive REPL for Disp.
// Pipeline: parse → type-check (native tree types) → compile → evaluate → print

import * as readline from "node:readline"
import * as fs from "node:fs"
import * as path from "node:path"
import { parseLine, printExpr, mergeDefinitions, type SExpr, type SDecl, ParseError } from "./parse.js"
import { compileAndEval } from "./compile.js"
import { prettyTree, type Tree, LEAF, stem, treeEqual, BudgetExhausted } from "./tree.js"
import { loadPrelude, processDecl } from "./prelude.js"
import { type KnownDefs, TN_TYPE, TN_TREE, TN_BOOL, TN_NAT } from "./tree-native-checker.js"
import { type NativeEnv, printNativeType, buildDirect, NativeElabError } from "./tree-native-elaborate.js"

export type ReplState = {
  defs: Map<string, Tree>       // compiled definitions (for evaluation)
  defExprs: Map<string, SExpr>  // source-level definitions (for save/display)
  defIsRec: Set<string>         // names defined with 'let rec'
  nativeDefs: KnownDefs         // tree-native type checker known definitions
  nativeEnv: NativeEnv          // native elaborator environment
}

export function initialState(): ReplState {
  const { defs, nativeDefs, nativeEnv } = loadPrelude()
  return { defs, defExprs: new Map(), defIsRec: new Set(), nativeDefs, nativeEnv }
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
    if (e instanceof NativeElabError) return `Type error: ${e.message}`
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

  const result = processDecl(name, type, value, isRec, state.nativeEnv, state.nativeDefs, state.defs)
  state.defExprs.set(name, value)
  if (isRec) state.defIsRec.add(name)

  const nameMap = buildNativeNameMap(state.nativeEnv)
  const typeStr = printNativeType(result.nativeType, nameMap)
  const warning = result.checkOk === false ? `\n[native] check failed for ${name}` : ""
  return `${name} : ${typeStr}${warning}`
}

// Build a name map from nativeEnv (tree ID → name) for type printing
const NATIVE_NAME_MAP_SKIP = new Set(["leaf", "stem", "fork", "true", "zero", "succ", "tDelta", "encType", "encVar", "wrap"])

function buildNativeNameMap(nativeEnv: NativeEnv): Map<number, string> {
  const map = new Map<number, string>()
  for (const [name, { tree }] of nativeEnv) {
    if (NATIVE_NAME_MAP_SKIP.has(name)) continue
    if (!map.has(tree.id)) map.set(tree.id, name)
  }
  return map
}

// Structural recognition of primitive values using native type constants
function recognizeValueNative(
  tree: Tree, type: Tree, state: ReplState,
): string | null {
  if (treeEqual(type, TN_BOOL)) {
    if (treeEqual(tree, LEAF)) return "true"
    if (treeEqual(tree, stem(LEAF))) return "false"
    return null
  }
  if (treeEqual(type, TN_NAT)) {
    let n = tree, count = 0
    while (n.tag === "stem") { count++; n = n.child }
    if (n.tag === "leaf") return String(count)
    return null
  }
  if (treeEqual(type, TN_TREE)) {
    return printTreeValue(tree)
  }
  for (const [name, def] of state.defs) {
    if (tree.id === def.id) return name
  }
  return null
}

// Check if an expression references any definitions whose types are opaque
// (contain unresolved <type#...> components from Church-encoded types or dependent types).
function exprUsesOpaqueTypes(expr: SExpr, env: NativeEnv): boolean {
  function hasOpaque(type: Tree): boolean {
    // Opaque types are stems that aren't recognized primitives
    if (type.tag === "stem" && !treeEqual(type, TN_TREE) && !treeEqual(type, TN_BOOL) && !treeEqual(type, TN_NAT))
      return true
    if (type.tag === "fork") return hasOpaque(type.left) || hasOpaque(type.right)
    return false
  }
  function check(e: SExpr): boolean {
    if (e.tag === "svar") {
      const entry = env.get(e.name)
      if (entry && hasOpaque(entry.type)) return true
    }
    if (e.tag === "sapp") return check(e.func) || check(e.arg)
    if (e.tag === "slam") return check(e.body)
    if (e.tag === "spi") return check(e.domain) || check(e.codomain)
    return false
  }
  return check(expr)
}

function handleExpr(state: ReplState, expr: SExpr): string {
  let elabResult: { tree: Tree, type: Tree } | null = null
  let elabError: NativeElabError | null = null
  try {
    elabResult = buildDirect(expr, state.nativeEnv)
  } catch (e) {
    if (e instanceof NativeElabError) {
      // Fall back to compile-only when any expression in the chain involves
      // definitions with opaque types (Church-encoded type constructors,
      // dependent types with unevaluated applications).
      // Detect this by checking if any definition used in the expression has an
      // opaque type (non-recognized stem type in nativeEnv).
      const hasOpaqueDefinitions = exprUsesOpaqueTypes(expr, state.nativeEnv)
      if (hasOpaqueDefinitions) {
        elabError = e
      } else {
        throw e
      }
    } else {
      throw e
    }
  }

  if (elabResult) {
    const { tree: elabTree, type: nativeType } = elabResult
    const nameMap = buildNativeNameMap(state.nativeEnv)
    const typeStr = printNativeType(nativeType, nameMap)

    // For type-valued expressions, print the native type tree directly
    if (treeEqual(nativeType, TN_TYPE)) {
      const valueStr = printNativeType(elabTree, nameMap)
      return `${valueStr} : Type`
    }

    let compiled: Tree | null = null
    try {
      compiled = compileAndEval(expr, state.defs)
    } catch {}

    if (compiled) {
      const display = recognizeValueNative(compiled, nativeType, state)
      if (display) return `${display} : ${typeStr}`
      return `${prettyTree(compiled)} : ${typeStr}`
    }

    return `_ : ${typeStr}`
  }

  // Type elaboration failed — try to compile and evaluate without type info
  let compiled: Tree | null = null
  try {
    compiled = compileAndEval(expr, state.defs)
  } catch {}

  if (compiled) {
    // Try to recognize the value as a known definition or primitive
    const nameMap = buildNativeNameMap(state.nativeEnv)
    // Try each primitive type for display
    for (const tryType of [TN_BOOL, TN_NAT, TN_TREE]) {
      const display = recognizeValueNative(compiled, tryType, state)
      if (display) return `${display} : ${printNativeType(tryType, nameMap)}`
    }
    // Check if it matches a named definition
    for (const [name, def] of state.defs) {
      if (compiled.id === def.id) return `${name} : Tree`
    }
    return `${prettyTree(compiled)} : Tree`
  }

  // Compilation also failed — report the original type error
  if (elabError) throw elabError
  throw new NativeElabError("Cannot type-check or evaluate expression")
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
        const { type: nativeType } = buildDirect(expr, state.nativeEnv)
        const nameMap = buildNativeNameMap(state.nativeEnv)
        return printNativeType(nativeType, nameMap)
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
      if (state.nativeEnv.size === 0) return "(empty context)"
      const nameMap = buildNativeNameMap(state.nativeEnv)
      const lines: string[] = []
      for (const [name, { type }] of state.nativeEnv) {
        lines.push(`${name} : ${printNativeType(type, nameMap)}`)
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
    const blocks = mergeDefinitions(content)
    let count = 0
    for (const block of blocks) {
      const trimmed = block.text.trim()
      if (!trimmed || trimmed.startsWith("--")) continue
      try {
        const parsed = parseLine(trimmed)
        if (isDecl(parsed)) {
          handleDecl(state, parsed)
          count++
        }
        // Non-declarations in files are silently ignored
      } catch (e) {
        const msg = e instanceof ParseError
          ? formatError(trimmed, "Parse error", e.message, e.span)
          : e instanceof NativeElabError ? `Type error: ${e.message}`
          : e instanceof BudgetExhausted ? `Error: evaluation did not terminate`
          : e instanceof Error ? `Error: ${e.message}` : `Error: ${e}`
        return `${filePath}:${block.startLine}: ${msg}`
      }
    }
    if (silent) return ""
    return count > 0 ? `Loaded ${filePath} (${count} definitions)` : `Loaded ${filePath}`
  } catch (e) {
    if (silent) return ""
    return `Error: Could not load ${filePath}: ${e instanceof Error ? e.message : e}`
  }
}

function saveFile(state: ReplState, filePath: string): string {
  try {
    const lines: string[] = []
    const nameMap = buildNativeNameMap(state.nativeEnv)
    for (const [name, { type }] of state.nativeEnv) {
      const valExpr = state.defExprs.get(name)
      if (valExpr) {
        const typeStr = printNativeType(type, nameMap)
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

  // Auto-load prelude and stdlib if they exist
  const preludePath = path.resolve("prelude.disp")
  if (fs.existsSync(preludePath)) {
    loadFile(state, preludePath, true)
  }
  const stdlibPath = path.resolve("stdlib.disp")
  if (fs.existsSync(stdlibPath)) {
    loadFile(state, stdlibPath, true)
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
