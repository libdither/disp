// Bind-tree AST for the Calculus of Constructions.
// Converts named-variable surface AST to bind-tree representation.
// Implements substitution and WHNF.

import { type SExpr } from "./parse.js"

// --- Bind-trees ---
// Mirror the Var/App skeleton of the body.
// None       = variable NOT bound here
// End        = variable IS bound here (at a Var)
// Branch(l,r)= recurse into both subtrees of an App

export type Bind =
  | { tag: "none" }
  | { tag: "end" }
  | { tag: "branch", left: Bind, right: Bind }

export const NONE: Bind = { tag: "none" }
export const END: Bind = { tag: "end" }
export function branch(left: Bind, right: Bind): Bind {
  // Optimize: Branch(None, None) = None
  if (left.tag === "none" && right.tag === "none") return NONE
  return { tag: "branch", left, right }
}

// --- AST ---

export type AST =
  | { tag: "var" }
  | { tag: "app", func: AST, arg: AST }
  | { tag: "lam", bind: Bind, body: AST }
  | { tag: "pi", domain: AST, bind: Bind, codomain: AST }
  | { tag: "sort" }

export const VAR: AST = { tag: "var" }
export const SORT: AST = { tag: "sort" }
export function app(func: AST, arg: AST): AST { return { tag: "app", func, arg } }
export function lam(bind: Bind, body: AST): AST { return { tag: "lam", bind, body } }
export function pi(domain: AST, bind: Bind, codomain: AST): AST { return { tag: "pi", domain, bind, codomain } }

// --- Convert surface AST to bind-tree AST ---

// Compute the bind-tree for a given variable name within an AST body.
// The bind-tree mirrors Var and App nodes:
//   Var → End if this IS the variable, None otherwise
//   App(f, x) → Branch(bindOf(f), bindOf(x))
//   Lam/Pi/Sort → transparent (recurse into body)
function computeBind(name: string, body: SExpr): Bind {
  switch (body.tag) {
    case "svar":
      return body.name === name ? END : NONE
    case "sapp":
      return branch(computeBind(name, body.func), computeBind(name, body.arg))
    case "slam":
      // Lambda is transparent to the outer bind-tree
      // But if the lambda shadows the name, the variable isn't free in the body
      if (body.params.includes(name)) return NONE
      return computeBind(name, body.body)
    case "spi":
      if (body.name === name) {
        // Only free in domain, not codomain (shadowed)
        return computeBind(name, body.domain)
      }
      // Free in both domain and codomain.
      // Domain is at "type annotation" level, codomain under a binder.
      // For bind-trees, both are part of the outer scope.
      // But bind-trees only track Var/App skeleton, and Pi's domain/codomain
      // are transparent. So we just recurse.
      const domBind = computeBind(name, body.domain)
      const codBind = computeBind(name, body.codomain)
      // Merge: if either has the var, it's present
      return mergeBind(domBind, codBind)
    case "stype":
      return NONE
  }
}

// Merge two bind-trees: used when a variable appears in multiple
// transparent subterms (e.g., Pi domain and codomain)
function mergeBind(a: Bind, b: Bind): Bind {
  if (a.tag === "none") return b
  if (b.tag === "none") return a
  // Both non-none: shouldn't happen in well-formed terms
  // (a variable can only appear once at each Var position)
  // But we handle it gracefully
  return a
}

// Convert surface AST to bind-tree AST
export function convertToBind(expr: SExpr): AST {
  return convert(expr)
}

function convert(expr: SExpr): AST {
  switch (expr.tag) {
    case "svar":
      return VAR
    case "stype":
      return SORT
    case "sapp": {
      const func = convert(expr.func)
      const arg = convert(expr.arg)
      return app(func, arg)
    }
    case "slam": {
      // Desugar multi-param: {x y} -> body = {x} -> {y} -> body
      let body = expr.body
      let params = [...expr.params]

      // Convert body first
      let result = convert(body)

      // Wrap from innermost to outermost
      for (let i = params.length - 1; i >= 0; i--) {
        const name = params[i]
        const bind = computeBindAST(name, expr, i)
        result = lam(bind, result)
      }
      return result
    }
    case "spi": {
      const domain = convert(expr.domain)
      const bind = computeBindAST(expr.name, expr, -1)
      const codomain = convert(expr.codomain)
      return pi(domain, bind, codomain)
    }
  }
}

// Compute bind-tree for a variable name within the surface expr,
// considering only the relevant scope.
function computeBindAST(name: string, expr: SExpr, paramIdx: number): Bind {
  if (name === "_") return NONE // non-dependent

  if (expr.tag === "slam") {
    // For multi-param lambda {x y z} -> body at index i,
    // compute bind for params[i] in the sub-expression:
    // {params[i+1..]} -> body
    const innerBody = expr.body
    const remainingParams = expr.params.slice(paramIdx + 1)

    // Build the effective body for this param
    let effectiveBody: SExpr = innerBody
    // The remaining params create nested lambdas that may shadow
    for (const p of remainingParams) {
      if (p === name) {
        // Shadowed by inner lambda
        return NONE
      }
    }

    return computeBind(name, effectiveBody)
  }

  if (expr.tag === "spi") {
    return computeBind(name, expr.codomain)
  }

  return NONE
}

// --- Substitution ---
// subst(bind, body, val): substitute val into body guided by bind-tree.

export function subst(bind: Bind, body: AST, val: AST): AST {
  if (bind.tag === "none") return body  // variable not present

  if (bind.tag === "end") {
    // End means the variable is at this position in the Var/App skeleton.
    // But the body might have transparent Lam/Pi nodes we pass through.
    switch (body.tag) {
      case "var": return val  // actual substitution
      case "lam": return lam(body.bind, subst(bind, body.body, val))
      case "pi": return pi(subst(bind, body.domain, val), body.bind, subst(bind, body.codomain, val))
      case "sort": return body
      case "app":
        // End at an App node shouldn't happen with well-formed bind-trees
        throw new Error("Bind-tree mismatch: End at App node")
    }
  }

  // Branch: recurse into App
  switch (body.tag) {
    case "app":
      return app(
        subst(bind.left, body.func, val),
        subst(bind.right, body.arg, val)
      )
    case "lam":
      // Pass through inner lambda
      return lam(body.bind, subst(bind, body.body, val))
    case "pi":
      return pi(
        subst(bind, body.domain, val),
        body.bind,
        subst(bind, body.codomain, val)
      )
    case "var":
      throw new Error("Bind-tree mismatch: Branch at Var node")
    case "sort":
      return body
  }
}

// --- WHNF (Weak Head Normal Form) ---

export class WhnfBudgetExhausted extends Error {
  constructor() { super("WHNF budget exhausted") }
}

export function whnf(term: AST, budget = { remaining: 10000 }): AST {
  if (budget.remaining <= 0) throw new WhnfBudgetExhausted()

  switch (term.tag) {
    case "app": {
      budget.remaining--
      const func = whnf(term.func, budget)
      if (func.tag === "lam") {
        // Beta reduction: (λ body) arg → body[arg]
        return whnf(subst(func.bind, func.body, term.arg), budget)
      }
      // Stuck: neutral term
      return app(func, term.arg)
    }
    default:
      // Var, Lam, Pi, Sort are already in WHNF
      return term
  }
}

// --- Convertibility ---
// Two terms are convertible if they reduce to the same WHNF.

export function convertible(a: AST, b: AST, budget = { remaining: 10000 }): boolean {
  const aNf = whnf(a, budget)
  const bNf = whnf(b, budget)
  return astEqual(aNf, bNf)
}

// Structural equality of ASTs (after WHNF, used for conversion checking)
function astEqual(a: AST, b: AST): boolean {
  if (a.tag !== b.tag) return false

  switch (a.tag) {
    case "var": return true
    case "sort": return true
    case "app":
      return astEqual(a.func, (b as typeof a).func) &&
             astEqual(a.arg, (b as typeof a).arg)
    case "lam":
      return bindEqual(a.bind, (b as typeof a).bind) &&
             astEqual(a.body, (b as typeof a).body)
    case "pi":
      return astEqual(a.domain, (b as typeof a).domain) &&
             bindEqual(a.bind, (b as typeof a).bind) &&
             astEqual(a.codomain, (b as typeof a).codomain)
  }
}

function bindEqual(a: Bind, b: Bind): boolean {
  if (a.tag !== b.tag) return false
  if (a.tag === "branch" && b.tag === "branch") {
    return bindEqual(a.left, b.left) && bindEqual(a.right, b.right)
  }
  return true // both none or both end
}

// --- Pretty printer (for debugging) ---

export function printAST(ast: AST, names: string[] = []): string {
  switch (ast.tag) {
    case "var":
      return names.length > 0 ? names[names.length - 1] : "?"
    case "sort":
      return "Type"
    case "app":
      return `(${printAST(ast.func, names)} ${printAST(ast.arg, names)})`
    case "lam": {
      const n = freshName(names)
      return `(λ${n}. ${printASTUnderBinder(ast.bind, ast.body, n, names)})`
    }
    case "pi": {
      const n = freshName(names)
      const dom = printAST(ast.domain, names)
      const cod = printASTUnderBinder(ast.bind, ast.codomain, n, names)
      if (ast.bind.tag === "none") return `(${dom} -> ${cod})`
      return `(Π(${n}:${dom}). ${cod})`
    }
  }
}

function printASTUnderBinder(bind: Bind, body: AST, name: string, outerNames: string[]): string {
  // For now, simple approach: add the name to the stack
  return printAST(body, [...outerNames, name])
}

let nameCounter = 0
function freshName(existing: string[]): string {
  const base = "xyzabcdefghijklmnopqrstuvw"
  for (const c of base) {
    if (!existing.includes(c)) return c
  }
  return `v${nameCounter++}`
}
