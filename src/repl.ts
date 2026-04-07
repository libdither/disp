// REPL for Disp: read-eval-print loop over tree calculus.
// Loads types.disp on startup, then accepts expressions and declarations.

import * as readline from "node:readline"
import * as fs from "node:fs"
import * as path from "node:path"
import { Tree, LEAF, stem, isLeaf, isStem, isFork, prettyTree, BudgetExhausted, FAST_EQ } from "./tree.js"
import { parseLine, parseExpr, ParseError } from "./parse.js"
import { compile, declare, loadFile, loadTypedFile, Env, entry, typecheckDeclSource, typecheckExprSource } from "./elaborate.js"

// === Tree display ===

/** Structural display: leaf, stem(...), fork(..., ...) */
function showTree(t: Tree, depth = 0): string {
  if (depth > 30) return "..."
  if (isLeaf(t)) return "leaf"
  if (isStem(t)) return `stem(${showTree(t.child, depth + 1)})`
  return `fork(${showTree(t.left, depth + 1)}, ${showTree(t.right, depth + 1)})`
}

/** Try to read a tree as a nat (stem-chain ending in leaf) */
function asNat(t: Tree): number | null {
  let n = 0, cur = t
  while (isStem(cur)) { n++; cur = cur.child }
  return isLeaf(cur) ? n : null
}

/** Display a tree with type hints */
function displayTree(t: Tree): string {
  const n = asNat(t)
  if (n === 0) return "leaf  -- 0, true"
  if (n === 1) return "stem(leaf)  -- 1, false"
  if (n !== null) return `${showTree(t)}  -- ${n}`
  return showTree(t)
}

// === REPL state ===

type ReplState = { env: Env }

function seedEnv(): Env {
  return new Map([
    ["leaf", entry(LEAF)],
    ["fastEq", entry(FAST_EQ)],
    // Literal support: parser desugars true/false/numbers to these names
    ["true", entry(LEAF)],           // true = leaf
    ["false", entry(stem(LEAF))],    // false = stem(leaf)
    ["zero", entry(LEAF)],           // 0 = leaf
    ["succ", entry(LEAF)],           // succ(n) = apply(leaf, n) = stem(n)
  ])
}

function initState(): ReplState {
  const typesPath = path.join(import.meta.dirname, "..", "types.disp")
  const preludePath = path.join(import.meta.dirname, "..", "prelude.disp")
  let env = loadFile(fs.readFileSync(typesPath, "utf-8"), seedEnv())
  try {
    env = loadTypedFile(fs.readFileSync(preludePath, "utf-8"), env)
  } catch {
    // prelude.disp is optional
  }
  return { env }
}

// === Line evaluation ===

function evalLine(state: ReplState, input: string): { state: ReplState, output: string } {
  const trimmed = input.trim()
  if (!trimmed || trimmed.startsWith("--")) return { state, output: "" }
  if (trimmed.startsWith(":")) return handleCommand(state, trimmed)

  try {
    const result = parseLine(trimmed)
    if ("tag" in result) {
      // Expression
      const tree = compile(result, state.env)
      return { state, output: displayTree(tree) }
    }
    // Declaration
    if (result.type) {
      // Typed declaration: typecheck via typed pipeline
      const checkResult = typecheckDeclSource(trimmed, state.env)
      if (!checkResult.ok) {
        return { state, output: `Type error [${checkResult.stage}]: ${checkResult.message}` }
      }
      const e = checkResult.env.get(result.name)!
      return {
        state: { env: checkResult.env },
        output: `${result.name} = ${displayTree(e.tree)}  (checked)`,
      }
    }
    // Untyped declaration: bare compilation
    const newEnv = declare(result, state.env)
    const e = newEnv.get(result.name)!
    return {
      state: { env: newEnv },
      output: `${result.name} = ${displayTree(e.tree)}`,
    }
  } catch (e) {
    if (e instanceof ParseError) return { state, output: `Parse error: ${e.message}` }
    if (e instanceof BudgetExhausted) return { state, output: "Budget exhausted (computation too large)" }
    if (e instanceof Error) return { state, output: `Error: ${e.message}` }
    return { state, output: "Unknown error" }
  }
}

// === Commands ===

function handleCommand(state: ReplState, input: string): { state: ReplState, output: string } {
  const parts = input.split(/\s+/)
  const cmd = parts[0]
  const arg = parts.slice(1).join(" ")

  switch (cmd) {
    case ":q": case ":quit":
      process.exit(0)

    case ":env": case ":e": {
      const names = [...state.env.keys()].sort()
      return { state, output: names.join("  ") }
    }

    case ":load": case ":l": {
      if (!arg) return { state, output: "Usage: :load <file>" }
      try {
        const source = fs.readFileSync(arg, "utf-8")
        const before = state.env.size
        const newEnv = loadFile(source, state.env)
        const added = newEnv.size - before
        return { state: { env: newEnv }, output: `Loaded ${added} new definition(s)` }
      } catch (e) {
        return { state, output: `Load error: ${(e as Error).message}` }
      }
    }

    case ":tree": case ":t": {
      if (!arg) return { state, output: "Usage: :tree <expr>" }
      try {
        const result = parseLine(arg)
        const sexpr = "tag" in result ? result : result.value
        const tree = compile(sexpr, state.env)
        return { state, output: prettyTree(tree) }
      } catch (e) {
        return { state, output: `Error: ${(e as Error).message}` }
      }
    }

    case ":type": {
      if (!arg) return { state, output: "Usage: :type <name>" }
      const e = state.env.get(arg)
      if (!e) return { state, output: `Unknown: ${arg}` }
      if (!isFork(e.type)) return { state, output: `${arg} : (untyped)` }
      return { state, output: `${arg} : ${showTree(e.type)}` }
    }

    case ":check": case ":c": {
      if (!arg) return { state, output: "Usage: :check <expr> : <type>" }
      const colonIdx = arg.lastIndexOf(":")
      if (colonIdx <= 0) return { state, output: "Usage: :check <expr> : <type>" }
      const exprSrc = arg.substring(0, colonIdx).trim()
      const typeSrc = arg.substring(colonIdx + 1).trim()
      if (!exprSrc || !typeSrc) return { state, output: "Usage: :check <expr> : <type>" }
      const result = typecheckExprSource(exprSrc, typeSrc, state.env)
      if (result.ok) {
        return { state, output: `OK : ${typeSrc}` }
      }
      return { state, output: `Type error [${result.stage}]: ${result.message}` }
    }

    case ":reset": case ":r":
      return { state: initState(), output: "Reset to initial state" }

    case ":help": case ":h": case ":?":
      return { state, output: HELP_TEXT }

    default:
      return { state, output: `Unknown command: ${cmd}. Try :help` }
  }
}

const HELP_TEXT = `Commands:
  :env              List defined names
  :load <file>      Load a .disp file
  :tree <expr>      Show raw tree (△ notation)
  :type <name>      Show type annotation
  :check <e> : <t>  Type-check expression against type
  :reset            Reset environment
  :quit             Exit

Enter an expression to evaluate, or a declaration to define:
  not true                        -- evaluate expression
  let double := {x} -> add x x   -- untyped definition
  let inc : Nat -> Nat := {n} -> succ n  -- typed definition`

// === Main loop ===

export function startRepl(): void {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout })

  let state = initState()
  console.log(`disp v0.1 — ${state.env.size} definitions loaded from types.disp`)
  console.log("Type :help for commands\n")

  rl.setPrompt("disp> ")
  rl.prompt()

  rl.on("line", input => {
    const { state: newState, output } = evalLine(state, input)
    state = newState
    if (output) console.log(output)
    rl.prompt()
  })

  rl.on("close", () => { console.log(); process.exit(0) })
}
