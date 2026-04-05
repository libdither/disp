// Elaborator for Disp.
//
// Two modes:
//   1. Bare mode: untyped bracket abstraction. Compiles lambda terms to tree
//      combinators. Used to bootstrap type predicates from types.disp.
//   2. Typed mode: bidirectional type inference + bracket abstraction.
//      Produces annotated trees with type predicates at each node.
//      (TODO: after types.disp is loaded)
//
// Bracket abstraction rules:
//   [x] x       = I
//   [x] c       = K(c)          when x not free in c
//   [x] (f g)   = S([x]f, [x]g) when x free in both
//   [x] (f g)   = S(K(f), [x]g) when x free only in g
//   [x] (f g)   = S([x]f, K(g)) when x free only in f
//   Eta:  [x] (f x) = f          when x not free in f
//   Opt:  S(K(p), K(q)) = K(apply(p, q))
//   Opt:  S(K(p), I) = p

import { Tree, LEAF, stem, fork, apply, isLeaf, isFork, treeEqual, I, K } from "./tree.js"
import { SExpr, SDecl, parseLine, mergeDefinitions } from "./parse.js"

// === Expression IR (pre-bracket-abstraction) ===

export type Expr =
  | { tag: "eTree", tree: Tree }
  | { tag: "eFvar", name: string }
  | { tag: "eApp", func: Expr, arg: Expr }
  | { tag: "eS", left: Expr, right: Expr }  // S(f, g) — collapses to fork(stem(f), g)

export function eTree(tree: Tree): Expr { return { tag: "eTree", tree } }
export function eFvar(name: string): Expr { return { tag: "eFvar", name } }
export function eApp(func: Expr, arg: Expr): Expr { return { tag: "eApp", func, arg } }
function eS(left: Expr, right: Expr): Expr { return { tag: "eS", left, right } }

// === Free variable check ===

function freeIn(name: string, expr: Expr): boolean {
  switch (expr.tag) {
    case "eTree": return false
    case "eFvar": return expr.name === name
    case "eApp": return freeIn(name, expr.func) || freeIn(name, expr.arg)
    case "eS": return freeIn(name, expr.left) || freeIn(name, expr.right)
  }
}

// === Bracket abstraction: [name] expr → Expr ===

function bracketAbstract(name: string, expr: Expr): Expr {
  switch (expr.tag) {
    case "eTree":
      return eApp(eTree(K), expr)

    case "eFvar":
      return expr.name === name ? eTree(I) : eApp(eTree(K), expr)

    case "eS": {
      const fInL = freeIn(name, expr.left)
      const fInR = freeIn(name, expr.right)
      if (!fInL && !fInR) return eApp(eTree(K), expr)
      // eS(f, g) = fork(stem(f), g) = apply(apply(leaf, apply(leaf, f)), g)
      // Convert to equivalent tree-constructor application and re-abstract
      const asApp = eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), expr.left)), expr.right)
      return bracketAbstract(name, asApp)
    }

    case "eApp": {
      const { func, arg } = expr
      const fInF = freeIn(name, func)
      const fInA = freeIn(name, arg)

      if (!fInF && !fInA) return eApp(eTree(K), expr)

      // Eta: [x](f x) = f when x not free in f
      if (!fInF && arg.tag === "eFvar" && arg.name === name) return func

      const absF = fInF ? bracketAbstract(name, func) : eApp(eTree(K), func)
      const absA = fInA ? bracketAbstract(name, arg) : eApp(eTree(K), arg)

      return eS(absF, absA)
    }
  }
}

// === Collapse: Expr → Tree ===
// Converts bracket-abstracted Expr to an actual tree.
// eS nodes become fork(stem(f), g) with optimizations.
// eApp nodes eagerly evaluate via apply().

// Budget for eager evaluation during compilation. Large programs (like PiCheck)
// need significant headroom for partial application chains.
const COMPILE_BUDGET = { remaining: 0 }
function resetCompileBudget() { COMPILE_BUDGET.remaining = 10_000_000 }
resetCompileBudget()

function collapse(expr: Expr, env: Map<string, Tree>): Tree {
  switch (expr.tag) {
    case "eTree":
      return expr.tree
    case "eFvar": {
      const t = env.get(expr.name)
      if (!t) throw new Error(`Unbound variable: ${expr.name}`)
      return t
    }
    case "eS": {
      const f = collapse(expr.left, env)
      const g = collapse(expr.right, env)

      // S(K(p), K(q)) = K(apply(p, q))
      if (isFork(f) && isLeaf(f.left) && isFork(g) && isLeaf(g.left)) {
        return fork(LEAF, apply(f.right, g.right, COMPILE_BUDGET))
      }
      // S(K(p), I) = p
      if (isFork(f) && isLeaf(f.left) && treeEqual(g, I)) {
        return f.right
      }

      return fork(stem(f), g)
    }
    case "eApp": {
      // Normal application: eagerly evaluate
      return apply(collapse(expr.func, env), collapse(expr.arg, env), COMPILE_BUDGET)
    }
  }
}

// === SExpr → Expr conversion ===

function sexprToExpr(sexpr: SExpr, env: Map<string, Tree>): Expr {
  switch (sexpr.tag) {
    case "svar": {
      const t = env.get(sexpr.name)
      if (t) return eTree(t)
      return eFvar(sexpr.name)
    }
    case "sapp":
      return eApp(sexprToExpr(sexpr.func, env), sexprToExpr(sexpr.arg, env))
    case "slam": {
      // {x y} -> body desugars to [x][y]body (bracket-abstract inside-out)
      let body = sexprToExpr(sexpr.body, env)
      for (let i = sexpr.params.length - 1; i >= 0; i--) {
        body = bracketAbstract(sexpr.params[i], body)
      }
      return body
    }
    case "stype":
      return eTree(LEAF)
    case "stree":
      return eTree(stem(LEAF))
    case "spi":
      throw new Error("Pi types not supported in bare mode")
  }
}

// === Bare mode: compile untyped declarations ===

export type BareEnv = Map<string, Tree>

/** Compile an SExpr to a tree in bare (untyped) mode */
export function bareCompile(sexpr: SExpr, env: BareEnv): Tree {
  return collapse(sexprToExpr(sexpr, env), env)
}

/** Process a declaration in bare mode, returns updated env */
export function bareDeclare(decl: SDecl, env: BareEnv): BareEnv {
  const newEnv = new Map(env)
  resetCompileBudget()
  const startBudget = COMPILE_BUDGET.remaining

  if (decl.isRec) {
    const fixTree = env.get("fix")
    if (!fixTree) throw new Error(`Recursive def '${decl.name}' requires 'fix' in env`)

    // Compile body with self as free variable, then bracket-abstract self out
    const bodyExpr = sexprToExpr(decl.value, env)
    const stepExpr = bracketAbstract(decl.name, bodyExpr)
    const stepFn = collapse(stepExpr, env)
    newEnv.set(decl.name, apply(fixTree, stepFn, COMPILE_BUDGET))
  } else {
    newEnv.set(decl.name, bareCompile(decl.value, env))
  }

  return newEnv
}

/** Load a .disp file in bare mode. Returns env with all definitions. */
export function bareLoadFile(source: string, env: BareEnv = new Map()): BareEnv {
  const blocks = mergeDefinitions(source)
  for (const block of blocks) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const result = parseLine(trimmed)
    if ("tag" in result) continue // standalone expression, skip
    env = bareDeclare(result, env)
  }
  return env
}
