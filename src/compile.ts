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

    case "stree":
      // Tree as a type value = stem(LEAF) = TN_TREE in the native type encoding
      return eTree(stem(LEAF))

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

    case "spi": {
      // Pi types as values: fork(domain, [x]codomain) = native Pi encoding
      const domain = astToExpr(ast.domain, defs)
      const codomain = ast.name === "_"
        ? bracketAbstract("_$pi", astToExpr(ast.codomain, defs))
        : bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
      // apply(apply(LEAF, domain), codomain) = apply(stem(domain), codomain) = fork(domain, codomain)
      return eApp(eApp(eTree(LEAF), domain), codomain)
    }
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

// K(e) collapses to fork(LEAF, collapse(e)).
// Built as app(stem(LEAF), e): treeApply(stem(LEAF), collapse(e)) = fork(LEAF, collapse(e)).
function kOf(e: Expr): Expr {
  return eApp(eTree(stem(LEAF)), e)
}

// S(f, g) collapses to fork(stem(collapse(f)), collapse(g)).
// stem via treeApply: treeApply(LEAF, X) = stem(X), so two LEAF applications build stem(stem(f)).
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
// we use the lazy FIX combinator (from lambada/tree calculus):
//
//   fix = \f wait m (\x f (wait m x))
//   where wait a b c = a b c (but delays a b until c arrives)
//         m = \x x x (self-application)
//
// When (fix f) is applied to arg:
//   wait m G arg = m G arg = G G arg = f (fix f) arg
//
// The wait combinator prevents eager divergence of self-application.

// wait a b c = △ (△ a) (△ △ c) b = a b c
// Extensionally a 3-arg identity, but wait a b does NOT evaluate a b.
// The application is deferred until the third argument c arrives.
export const WAIT: Tree = (() => {
  const body = eApp(
    eApp(
      eApp(eTree(LEAF), eApp(eTree(LEAF), eFvar("a"))),
      eApp(eApp(eTree(LEAF), eTree(LEAF)), eFvar("c"))
    ),
    eFvar("b")
  )
  let e = bracketAbstract("c", body)
  e = bracketAbstract("b", e)
  e = bracketAbstract("a", e)
  return collapseAndEval(e)
})()

// m = \x x x (self-application: m z = z z)
const SELF_APP: Tree = collapseAndEval(bracketAbstract("x", eApp(eFvar("x"), eFvar("x"))))

// fix f = wait m (\x f (wait m x))
export const FIX: Tree = collapseAndEval(bracketAbstract("f",
  eApp(
    eApp(eTree(WAIT), eTree(SELF_APP)),
    bracketAbstract("x", eApp(
      eFvar("f"),
      eApp(eApp(eTree(WAIT), eTree(SELF_APP)), eFvar("x"))
    ))
  )))

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

  // 2. Bracket-abstract name to get \self -> body
  const selfFn = bracketAbstract(name, bodyExpr)

  // 3. Apply FIX to the compiled function
  return apply(FIX, collapseAndEval(selfFn, budget), budget)
}
