// Elaborator for Disp.
//
// Unified bracket abstraction pipeline with optional type tracking.
//
// Bare mode:  types are opaque (stem(LEAF)), D = null → raw S-nodes
// Typed mode: types tracked from annotations, D computed → annotated S-nodes
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

import { Tree, LEAF, stem, fork, apply, isLeaf, isFork, isStem, treeEqual, I, K } from "./tree.js"
import { SExpr, SDecl, parseLine, mergeDefinitions } from "./parse.js"

// === Opaque type sentinel ===
// Used for bare mode where types are unknown.
const OPAQUE: Tree = stem(LEAF)

// === Environment ===
// Single unified env: every definition carries its tree and type.
// Untyped definitions get type = OPAQUE.

export type Env = Map<string, { tree: Tree, type: Tree }>

/** Create an untyped env entry */
export function entry(tree: Tree): { tree: Tree, type: Tree } {
  return { tree, type: OPAQUE }
}

/** Extract bare trees from env (for collapse) */
function envTrees(env: Env): Map<string, Tree> {
  const m = new Map<string, Tree>()
  for (const [k, v] of env) m.set(k, v.tree)
  return m
}

// === Expression IR ===
// Single unified IR for both bare and typed modes.
// In bare mode: ty = OPAQUE, D = null
// In typed mode: ty = actual type predicate, D = K(intermediate type)

export type Expr =
  | { tag: "lit", tree: Tree, ty: Tree }
  | { tag: "var", name: string, ty: Tree }
  | { tag: "app", func: Expr, arg: Expr, ty: Tree }
  | { tag: "k", body: Expr, ty: Tree }
  | { tag: "s", left: Expr, right: Expr, D: Tree | null, ty: Tree }

function lit(tree: Tree, ty: Tree = OPAQUE): Expr { return { tag: "lit", tree, ty } }
function evar(name: string, ty: Tree = OPAQUE): Expr { return { tag: "var", name, ty } }
function app(func: Expr, arg: Expr, ty: Tree = OPAQUE): Expr { return { tag: "app", func, arg, ty } }
function kNode(body: Expr, ty: Tree = OPAQUE): Expr { return { tag: "k", body, ty } }
function sNode(left: Expr, right: Expr, D: Tree | null, ty: Tree = OPAQUE): Expr {
  return { tag: "s", left, right, D, ty }
}

// === Type helpers ===

/** K-wrap a type: K(T) = fork(LEAF, T) */
function kType(t: Tree): Tree { return fork(LEAF, t) }

/** Unwrap K(T) → T. Returns null if not K-shaped. */
function unwrapK(family: Tree): Tree | null {
  if (isFork(family) && isLeaf(family.left)) return family.right
  return null
}

/** Wrap a bare tree in ascription format: fork(stem(T), stem(body))
 *  PiCheck verifies T = expectedPi via O(1) hash-consing equality. */
function wrapAscription(body: Tree, ty: Tree): Tree {
  return fork(stem(ty), stem(body))
}

// === Free variable check ===

function freeIn(name: string, expr: Expr): boolean {
  switch (expr.tag) {
    case "lit": return false
    case "var": return expr.name === name
    case "app": return freeIn(name, expr.func) || freeIn(name, expr.arg)
    case "k": return freeIn(name, expr.body)
    case "s": return freeIn(name, expr.left) || freeIn(name, expr.right)
  }
}

// === Bracket abstraction: [name : nameType] expr → Expr ===
// When nameType = OPAQUE, behaves as bare mode (D = null).
// When nameType is a real type, computes D for S-nodes.

function abstract(name: string, nameType: Tree, expr: Expr): Expr {
  const typed = !treeEqual(nameType, OPAQUE)
  const piTy = (resultType: Tree) => typed ? fork(nameType, kType(resultType)) : OPAQUE

  switch (expr.tag) {
    case "lit":
      return kNode(expr, piTy(expr.ty))

    case "var":
      if (expr.name === name) {
        return lit(I, piTy(nameType))
      }
      return kNode(expr, piTy(expr.ty))

    case "k": {
      if (!freeIn(name, expr)) return kNode(expr, piTy(expr.ty))
      // Convert K(body) to app(K, body) and re-abstract
      const asApp: Expr = app(lit(K, OPAQUE), expr.body, expr.ty)
      return abstract(name, nameType, asApp)
    }

    case "s": {
      const fInL = freeIn(name, expr.left)
      const fInR = freeIn(name, expr.right)
      if (!fInL && !fInR) return kNode(expr, piTy(expr.ty))

      if (expr.D !== null) {
        // Typed S-node: recursively abstract to preserve inner D annotations
        const absL = fInL ? abstract(name, nameType, expr.left)
          : kNode(expr.left, piTy(expr.left.ty))
        const absR = fInR ? abstract(name, nameType, expr.right)
          : kNode(expr.right, piTy(expr.right.ty))
        const newD = kType(absResultType(absR))
        return sNode(absL, absR, newD, piTy(expr.ty))
      }

      // Bare S-node: convert to application form and re-abstract
      // S(f,g) = fork(stem(f), g) = apply(apply(leaf, apply(leaf, f)), g)
      const asApp = app(app(lit(LEAF), app(lit(LEAF), expr.left)), expr.right)
      return abstract(name, nameType, asApp)
    }

    case "app": {
      const { func, arg } = expr
      const fInF = freeIn(name, func)
      const fInA = freeIn(name, arg)

      if (!fInF && !fInA) return kNode(expr, piTy(expr.ty))

      // Eta: [x](f x) = f when x not free in f
      if (!fInF && arg.tag === "var" && arg.name === name) return func

      const absF = fInF ? abstract(name, nameType, func)
        : kNode(func, piTy(func.ty))
      const absA = fInA ? abstract(name, nameType, arg)
        : kNode(arg, piTy(arg.ty))

      const D = typed ? kType(absResultType(absA)) : null
      return sNode(absF, absA, D, piTy(expr.ty))
    }
  }
}

/** Extract the result type T from Pi(A, K(T)) */
function absResultType(expr: Expr): Tree {
  if (isFork(expr.ty)) {
    const inner = unwrapK(expr.ty.right)
    if (inner !== null) return inner
  }
  return expr.ty
}

// === Collapse: Expr → Tree ===

const COMPILE_BUDGET = { remaining: 0 }
function resetCompileBudget() { COMPILE_BUDGET.remaining = 10_000_000 }
resetCompileBudget()

function collapse(expr: Expr, trees: Map<string, Tree>): Tree {
  switch (expr.tag) {
    case "lit":
      return expr.tree
    case "var": {
      const t = trees.get(expr.name)
      if (!t) throw new Error(`Unbound variable: ${expr.name}`)
      return t
    }
    case "k": {
      const body = collapse(expr.body, trees)
      return fork(LEAF, body)
    }
    case "s": {
      const f = collapse(expr.left, trees)
      const g = collapse(expr.right, trees)

      // S(K(p), K(q)) = K(apply(p, q))
      if (isFork(f) && isLeaf(f.left) && isFork(g) && isLeaf(g.left)) {
        return fork(LEAF, apply(f.right, g.right, COMPILE_BUDGET))
      }
      // S(K(p), I) = p
      if (isFork(f) && isLeaf(f.left) && treeEqual(g, I)) {
        return f.right
      }

      if (expr.D !== null) {
        // Annotated S-node: fork(stem(D), fork(c, b))
        return fork(stem(expr.D), fork(f, g))
      }
      // Bare S-node: fork(stem(c), b)
      return fork(stem(f), g)
    }
    case "app": {
      return apply(collapse(expr.func, trees), collapse(expr.arg, trees), COMPILE_BUDGET)
    }
  }
}

// === SExpr → Expr conversion ===
// When typed=true, Pi-typed definitions get ascription wrappers at use site.
// When typed=false (bare mode), definitions are used as-is.

function sexprToExpr(
  sexpr: SExpr,
  env: Env,
  freeTypes: Map<string, Tree>,
  typed: boolean,
): Expr {
  switch (sexpr.tag) {
    case "svar": {
      // Lambda params first
      const ft = freeTypes.get(sexpr.name)
      if (ft !== undefined) return evar(sexpr.name, ft)

      // Env lookup (with ascriptions for Pi-typed defs in typed mode)
      const e = env.get(sexpr.name)
      if (e) {
        if (typed && isFork(e.type)) {
          return lit(wrapAscription(e.tree, e.type), e.type)
        }
        return lit(e.tree, e.type)
      }

      return evar(sexpr.name)
    }
    case "sapp": {
      const func = sexprToExpr(sexpr.func, env, freeTypes, typed)
      const arg = sexprToExpr(sexpr.arg, env, freeTypes, typed)
      if (!isFork(func.ty)) {
        // Not a Pi type — opaque function, propagate type
        return app(func, arg, func.ty)
      }
      const B = func.ty.right
      const T = unwrapK(B)
      if (T === null) {
        // Dependent codomain — treat as opaque
        return app(func, arg, func.ty)
      }
      return app(func, arg, T)
    }
    case "slam": {
      // Inner lambda: process body, then bracket-abstract params (bare)
      const bodyExpr = sexprToExpr(sexpr.body, env, freeTypes, typed)
      let result = bodyExpr
      for (let i = sexpr.params.length - 1; i >= 0; i--) {
        result = abstract(sexpr.params[i], OPAQUE, result)
      }
      return { ...result, ty: OPAQUE }
    }
    case "stype":
      return lit(LEAF, LEAF)
    case "stree":
      return lit(stem(LEAF), LEAF)
    case "spi":
      throw new Error("Pi type expressions not supported in elaboration")
  }
}

// === Compile: SExpr → Tree (bare mode) ===

/** Compile an SExpr to a tree in bare (untyped) mode */
export function compile(sexpr: SExpr, env: Env): Tree {
  resetCompileBudget()
  const expr = sexprToExpr(sexpr, env, new Map(), false)
  return collapse(expr, envTrees(env))
}

// === Declare: process a declaration, returns updated env ===

/** Process a declaration: compile tree + type annotation in one pass */
export function declare(decl: SDecl, env: Env): Env {
  const newEnv = new Map(env)
  resetCompileBudget()
  const trees = envTrees(env)

  let tree: Tree
  if (decl.isRec) {
    const fixTree = env.get("fix")?.tree
    if (!fixTree) throw new Error(`Recursive def '${decl.name}' requires 'fix' in env`)
    const bodyExpr = sexprToExpr(decl.value, env, new Map(), false)
    const stepExpr = abstract(decl.name, OPAQUE, bodyExpr)
    const stepFn = collapse(stepExpr, trees)
    tree = apply(fixTree, stepFn, COMPILE_BUDGET)
  } else {
    const expr = sexprToExpr(decl.value, env, new Map(), false)
    tree = collapse(expr, trees)
  }

  const type = decl.type ? compileType(decl.type, env) : OPAQUE
  newEnv.set(decl.name, { tree, type })
  return newEnv
}

// === Load file: process all declarations from source ===

/** Load a .disp file. Returns env with all definitions and their type annotations.
 *  Two-pass internally: pass 1 compiles trees, pass 2 compiles type annotations.
 *  This handles forward references (e.g., `let f : Bool -> Bool` before `Bool` is defined).
 */
export function loadFile(source: string, env: Env = new Map()): Env {
  const blocks = mergeDefinitions(source)
  const decls: SDecl[] = []

  // Pass 1: compile all trees (type annotations deferred)
  for (const block of blocks) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const result = parseLine(trimmed)
    if ("tag" in result) continue
    decls.push(result)
    env = declare({ ...result, type: null }, env)
  }

  // Pass 2: compile type annotations (all trees now available)
  for (const decl of decls) {
    if (!decl.type) continue
    const existing = env.get(decl.name)
    if (!existing) continue
    const type = compileType(decl.type, env)
    env.set(decl.name, { tree: existing.tree, type })
  }

  return env
}

// === Typed mode: bracket abstraction with S-node annotations ===

/** Compile a lambda with typed bracket abstraction.
 *  expectedType = Pi(A, K(T)) — determines parameter types.
 *  Returns annotated tree with D at S-nodes + ascriptions for typed refs.
 */
export function typedCompileLam(
  params: string[],
  body: SExpr,
  expectedType: Tree,
  env: Env,
): Tree {
  resetCompileBudget()

  // Peel off Pi layers to get parameter types
  const paramTypes: Tree[] = []
  let curType = expectedType
  for (const param of params) {
    if (!isFork(curType)) throw new Error(`Expected Pi type for param ${param}`)
    paramTypes.push(curType.left)
    const inner = unwrapK(curType.right)
    if (inner === null) throw new Error(`Dependent Pi for param ${param} not supported yet`)
    curType = inner
  }

  // Build free-variable type map
  const freeTypes = new Map<string, Tree>()
  for (let i = 0; i < params.length; i++) {
    freeTypes.set(params[i], paramTypes[i])
  }

  // Convert body to Expr with types (typed=true for ascription wrapping)
  const bodyExpr = sexprToExpr(body, env, freeTypes, true)

  // Chain typed bracket abstractions for all params (inside-out)
  let curExpr = bodyExpr
  for (let i = params.length - 1; i >= 0; i--) {
    curExpr = abstract(params[i], paramTypes[i], curExpr)
  }

  return collapse(curExpr, envTrees(env))
}

// === Compile type annotations ===

/** Compile a type SExpr to a tree type.
 *  A -> B  →  fork(compile(A), K(compile(B)))  (non-dependent Pi)
 *  svar(name)  →  env lookup
 *  Type  →  LEAF
 */
export function compileType(sexpr: SExpr, env: Env): Tree {
  switch (sexpr.tag) {
    case "svar": {
      const t = env.get(sexpr.name)?.tree
      if (!t) throw new Error(`Unknown type: ${sexpr.name}`)
      return t
    }
    case "spi": {
      const domain = compileType(sexpr.domain, env)
      const codomain = compileType(sexpr.codomain, env)
      if (sexpr.name === "_") {
        return fork(domain, kType(codomain))
      }
      throw new Error("Dependent Pi types in annotations not yet supported")
    }
    case "stype":
      return LEAF
    case "stree":
      return stem(LEAF)
    case "sapp":
      return apply(compileType(sexpr.func, env), compileType(sexpr.arg, env), COMPILE_BUDGET)
    case "slam":
      throw new Error("Lambda in type position not supported")
  }
}
