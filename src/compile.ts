// Compiler: Surface AST (SExpr) → Tree Calculus
//
// Two-phase process:
// 1. AST → Expr: resolve lambdas to named free variables
// 2. Bracket abstraction: eliminate named variables using S/K/I
// 3. Collapse: convert Expr to Tree using treeApply
//
// In tree calculus:
//   K(c) = fork(LEAF, c)  — applied to x gives c (Rule 1)
//   S(f)(g) = fork(stem(f), g) — applied to x gives f x (g x) (Rule 2)
//   I = identity combinator

import { type SExpr } from "./parse.js"
import { type Tree, LEAF, stem, fork, treeApply, apply, I } from "./tree.js"

// --- Expr: intermediate representation ---

export type Expr =
  | { tag: "tree", value: Tree }
  | { tag: "fvar", name: string }
  | { tag: "app", func: Expr, arg: Expr }

export function eTree(value: Tree): Expr { return { tag: "tree", value } }
export function eFvar(name: string): Expr { return { tag: "fvar", name } }
export function eApp(func: Expr, arg: Expr): Expr { return { tag: "app", func, arg } }

// --- Phase 1: AST → Expr ---

export function astToExpr(ast: SExpr, defs: Map<string, Tree> = new Map()): Expr {
  switch (ast.tag) {
    case "stype":
      // Type erases to LEAF
      return eTree(LEAF)

    case "svar": {
      // Look up in definitions first
      const def = defs.get(ast.name)
      if (def) return eTree(def)
      return eFvar(ast.name)
    }

    case "sapp":
      return eApp(astToExpr(ast.func, defs), astToExpr(ast.arg, defs))

    case "slam": {
      // Convert multi-param lambda to nested single-param
      let body = astToExpr(ast.body, defs)
      // Bracket-abstract from innermost to outermost
      for (let i = ast.params.length - 1; i >= 0; i--) {
        body = bracketAbstract(ast.params[i], body)
      }
      return body
    }

    case "spi":
      // Pi types erase to lambdas (domain is dropped)
      // (x : A) -> B  compiles same as {x} -> B
      // Non-dependent A -> B compiles same as {_} -> B
      if (ast.name === "_") {
        // Non-dependent: the codomain doesn't use the variable
        // Still need to abstract over a dummy variable
        return bracketAbstract("_$pi", astToExpr(ast.codomain, defs))
      }
      return bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
  }
}

// --- Phase 2: Bracket abstraction ---
// Eliminate a named variable from an Expr.
//
// [x] x         = I  (identity)
// [x] c         = K c  (x not free in c)
// [x] (f g)     = S ([x]f) ([x]g)
//
// In tree calculus:
//   I = identity tree
//   K c = fork(LEAF, c)  — when applied to x, Rule 1 returns c
//   S f g = fork(stem(f), g) — when applied to x, Rule 2 gives f x (g x)

export function bracketAbstract(name: string, expr: Expr): Expr {
  switch (expr.tag) {
    case "fvar":
      if (expr.name === name) {
        return eTree(I)  // [x] x = I
      }
      // [x] y = K y (where y ≠ x)
      return kOf(expr)

    case "tree":
      // [x] c = K c (constant)
      return kOf(expr)

    case "app": {
      const freeInFunc = hasFreeVar(name, expr.func)
      const freeInArg = hasFreeVar(name, expr.arg)

      if (!freeInFunc && !freeInArg) {
        // Neither has the variable: K (f g)
        return kOf(expr)
      }

      // Eta reduction: [x](f x) = f when x not free in f
      if (!freeInFunc && expr.arg.tag === "fvar" && expr.arg.name === name) {
        return expr.func
      }

      // S ([x]f) ([x]g)
      const f = bracketAbstract(name, expr.func)
      const g = bracketAbstract(name, expr.arg)
      return optimizedS(f, g)
    }
  }
}

// Check if a free variable appears in an Expr
function hasFreeVar(name: string, expr: Expr): boolean {
  switch (expr.tag) {
    case "fvar": return expr.name === name
    case "tree": return false
    case "app": return hasFreeVar(name, expr.func) || hasFreeVar(name, expr.arg)
  }
}

// K(e): an Expr that, when collapsed and applied to x, returns collapse(e)
// In tree calculus: K c = fork(LEAF, c)
// As Expr: app(tree(LEAF), e) → treeApply(LEAF, collapse(e)) = stem(collapse(e))
// Wait, that gives stem, not K. K c = fork(LEAF, c).
// Actually treeApply(LEAF, c) = stem(c). But K c applied to x: stem(c) applied to x = fork(c, x).
// That's NOT K behavior. K c x should return c.
//
// K c = fork(LEAF, c). Applied to x: fork(fork(LEAF, c), x) → Rule 1 → c. ✓
//
// But how to build fork(LEAF, c) from Expr?
// We can't use treeApply because treeApply(LEAF, c) = stem(c), not fork(LEAF, c).
// We need a direct tree construction.
//
// Solution: during collapse, handle K and S specially.
// Or: represent K and S as Expr constructors.

// Actually, let's just represent them as tree constructions directly:
function kOf(e: Expr): Expr {
  // Result should collapse to fork(LEAF, collapse(e))
  // We represent this as: apply LEAF to (LEAF applied to e)
  // No... let's just use a special Expr node.
  // Actually, the cleanest approach: extend Expr with 'stem' constructor.
  // But the plan says Expr is just tree/fvar/app.
  //
  // Alternative: treeApply(stem(LEAF), collapse(e)) = fork(LEAF, collapse(e)). ✓
  // So K(e) = app(tree(stem(LEAF)), e). Since stem(LEAF) = K_tree.
  // collapse: treeApply(K_tree, collapse(e)) = treeApply(stem(LEAF), collapse(e)) = fork(LEAF, collapse(e)). ✓
  return eApp(eTree(stem(LEAF)), e)
}

// S(f, g): result should collapse to fork(stem(collapse(f)), collapse(g))
// treeApply(X, collapse(g)) = fork(stem(collapse(f)), collapse(g)) requires X = stem(stem(collapse(f)))
// treeApply(stem(stem(A)), B) = fork(stem(A), B). ✓
//
// So we need to build stem(stem(collapse(f))) as an Expr:
// stem(X) via treeApply: treeApply(LEAF, X) = stem(X).
// So stem(collapse(f)) = collapse(app(tree(LEAF), f))
// And stem(stem(collapse(f))) = collapse(app(tree(LEAF), app(tree(LEAF), f)))
//
// S(f, g) = app(app(tree(LEAF), app(tree(LEAF), f)), g)
function sOf(f: Expr, g: Expr): Expr {
  const stemF = eApp(eTree(LEAF), f)     // stem(collapse(f))
  const stemStemF = eApp(eTree(LEAF), stemF) // stem(stem(collapse(f)))
  return eApp(stemStemF, g)              // fork(stem(collapse(f)), collapse(g))
}

// --- Optimization helpers ---

// Recognize K(e) = eApp(eTree(stem(LEAF)), e), return the inner e or null
function isKOf(e: Expr): Expr | null {
  if (e.tag === "app" && e.func.tag === "tree" && e.func.value.tag === "stem" && e.func.value.child.tag === "leaf") {
    return e.arg
  }
  return null
}

// Recognize the identity tree I
function isI(e: Expr): boolean {
  return e.tag === "tree" && e.value.id === I.id
}

// Build S(f, g) with optimizations:
//   S(K p)(K q) = K(p q)  — both constant, apply at compile time
//   S(K p) I = p           — eta for K-wrapped value
function optimizedS(f: Expr, g: Expr): Expr {
  const kf = isKOf(f)
  const kg = isKOf(g)

  if (kf !== null && kg !== null) {
    // S(K p)(K q) = K(apply(p, q))
    // But p and q are Exprs, not Trees. We can only optimize when both are trees.
    if (kf.tag === "tree" && kg.tag === "tree") {
      return kOf(eTree(apply(kf.value, kg.value)))
    }
    // Otherwise: K(p q) — still saves one abstraction level
    return kOf(eApp(kf, kg))
  }

  if (kf !== null && isI(g)) {
    // S(K p) I = p
    return kf
  }

  return sOf(f, g)
}

// --- Phase 3: Collapse Expr → Tree ---

export function collapse(expr: Expr): Tree {
  switch (expr.tag) {
    case "tree":
      return expr.value
    case "fvar":
      throw new Error(`Free variable in collapse: ${expr.name}`)
    case "app":
      return treeApply(collapse(expr.func), collapse(expr.arg))
  }
}

// --- Phase 3b: Collapse with eager evaluation ---

export function collapseAndEval(expr: Expr, budget = { remaining: 100000 }): Tree {
  switch (expr.tag) {
    case "tree":
      return expr.value
    case "fvar":
      throw new Error(`Free variable in collapse: ${expr.name}`)
    case "app":
      return apply(collapseAndEval(expr.func, budget), collapseAndEval(expr.arg, budget), budget)
  }
}

// --- Top-level compile ---

export function compile(ast: SExpr, defs: Map<string, Tree> = new Map()): Tree {
  const expr = astToExpr(ast, defs)
  return collapse(expr)
}

export function compileAndEval(ast: SExpr, defs: Map<string, Tree> = new Map(), budget = { remaining: 100000 }): Tree {
  const expr = astToExpr(ast, defs)
  return collapseAndEval(expr, budget)
}

// --- Fixed-point combinator for recursive definitions ---
//
// Strategy: Given `let rec f := body` where body references f,
// we build a self-applying structure using the omega combinator:
//
//   omega = {x} -> body[f := {v} -> x x v]
//   result = omega omega  (structural, not eager)
//
// When result is applied to arg:
//   omega omega arg
//   = body[f := {v} -> omega omega v] arg
//   = body with f behaving as the recursive function
//
// The key: omega omega is built with treeApply (structural),
// NOT apply (eager), so no divergence at definition time.

export function compileRecAndEval(
  name: string,
  body: SExpr,
  defs: Map<string, Tree>,
  budget = { remaining: 100000 }
): Tree {
  // 1. Compile body with `name` free
  const defsWithoutSelf = new Map(defs)
  defsWithoutSelf.delete(name)
  const bodyExpr = astToExpr(body, defsWithoutSelf)

  // If the body doesn't actually reference `name`, no fixpoint needed
  if (!hasFreeVar(name, bodyExpr)) {
    return collapseAndEval(bodyExpr, budget)
  }

  // 2. Build the self-ref thunk: {v} -> x x v
  //    where x is also a free variable
  const xVar = eFvar("x$rec")
  const vVar = eFvar("v$rec")
  const selfApp = eApp(eApp(xVar, xVar), vVar)
  const thunk = bracketAbstract("v$rec", selfApp) // {v} -> x x v

  // 3. Substitute name with thunk in bodyExpr
  const bodyWithThunk = substExpr(name, thunk, bodyExpr)

  // 4. Abstract over x$rec
  const omega = bracketAbstract("x$rec", bodyWithThunk)

  // 5. Collapse omega structurally (no eager eval)
  const omegaTree = collapse(omega)

  // 6. Build omega(omega) structurally using treeApply
  return treeApply(omegaTree, omegaTree)
}

// Substitute a free variable name with an Expr in an Expr
export function substExpr(name: string, replacement: Expr, expr: Expr): Expr {
  switch (expr.tag) {
    case "fvar":
      return expr.name === name ? replacement : expr
    case "tree":
      return expr
    case "app":
      return eApp(substExpr(name, replacement, expr.func),
                  substExpr(name, replacement, expr.arg))
  }
}
