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

function collapse(expr: Expr, env: Map<string, Tree>): Tree {
  switch (expr.tag) {
    case "lit":
      return expr.tree
    case "var": {
      const t = env.get(expr.name)
      if (!t) throw new Error(`Unbound variable: ${expr.name}`)
      return t
    }
    case "k": {
      // K(body) — try to collapse to fork(LEAF, body)
      const body = collapse(expr.body, env)
      return fork(LEAF, body)
    }
    case "s": {
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

      if (expr.D !== null) {
        // Annotated S-node: fork(stem(D), fork(c, b))
        return fork(stem(expr.D), fork(f, g))
      }
      // Bare S-node: fork(stem(c), b)
      return fork(stem(f), g)
    }
    case "app": {
      return apply(collapse(expr.func, env), collapse(expr.arg, env), COMPILE_BUDGET)
    }
  }
}

// === SExpr → Expr conversion ===
// Unified: uses TypedEnv when available, BareEnv otherwise.
// freeTypes maps lambda params to their types (empty in bare mode).

export type BareEnv = Map<string, Tree>
export type TypedEnv = Map<string, { tree: Tree, type: Tree }>

function sexprToExpr(
  sexpr: SExpr,
  tenv: TypedEnv | null,
  bareEnv: BareEnv,
  freeTypes: Map<string, Tree>,
): Expr {
  switch (sexpr.tag) {
    case "svar": {
      // Lambda params first
      const ft = freeTypes.get(sexpr.name)
      if (ft !== undefined) return evar(sexpr.name, ft)

      // Typed env (with ascriptions for Pi-typed defs)
      if (tenv) {
        const entry = tenv.get(sexpr.name)
        if (entry) {
          if (isFork(entry.type)) {
            return lit(wrapAscription(entry.tree, entry.type), entry.type)
          }
          return lit(entry.tree, entry.type)
        }
      }

      // Bare env fallback
      const t = bareEnv.get(sexpr.name)
      if (t) return lit(t)
      return evar(sexpr.name)
    }
    case "sapp": {
      const func = sexprToExpr(sexpr.func, tenv, bareEnv, freeTypes)
      const arg = sexprToExpr(sexpr.arg, tenv, bareEnv, freeTypes)
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
      const bodyExpr = sexprToExpr(sexpr.body, tenv, bareEnv, freeTypes)
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

// === Bare mode: compile untyped declarations ===

/** Compile an SExpr to a tree in bare (untyped) mode */
export function bareCompile(sexpr: SExpr, env: BareEnv): Tree {
  const expr = sexprToExpr(sexpr, null, env, new Map())
  return collapse(expr, env)
}

/** Process a declaration in bare mode, returns updated env */
export function bareDeclare(decl: SDecl, env: BareEnv): BareEnv {
  const newEnv = new Map(env)
  resetCompileBudget()

  if (decl.isRec) {
    const fixTree = env.get("fix")
    if (!fixTree) throw new Error(`Recursive def '${decl.name}' requires 'fix' in env`)
    const bodyExpr = sexprToExpr(decl.value, null, env, new Map())
    const stepExpr = abstract(decl.name, OPAQUE, bodyExpr)
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
    if ("tag" in result) continue
    env = bareDeclare(result, env)
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
  tenv: TypedEnv,
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

  // Build bare env for collapse
  const bareEnv: BareEnv = new Map()
  for (const [name, entry] of tenv) bareEnv.set(name, entry.tree)

  // Convert body to Expr with types
  const bodyExpr = sexprToExpr(body, tenv, bareEnv, freeTypes)

  // Chain typed bracket abstractions for all params (inside-out)
  let curExpr = bodyExpr
  for (let i = params.length - 1; i >= 0; i--) {
    curExpr = abstract(params[i], paramTypes[i], curExpr)
  }

  return collapse(curExpr, bareEnv)
}

// === Compile type annotations ===

/** Compile a type SExpr to a tree type.
 *  A -> B  →  fork(compile(A), K(compile(B)))  (non-dependent Pi)
 *  svar(name)  →  env lookup
 *  Type  →  LEAF
 */
export function compileType(sexpr: SExpr, env: BareEnv): Tree {
  switch (sexpr.tag) {
    case "svar": {
      const t = env.get(sexpr.name)
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

// === Two-pass typed loading ===

/** Pass 2: record type annotations from source. Bare trees kept as-is.
 *  Definitions with Pi types get ascription wrappers at use site (in sexprToExpr).
 */
export function typedLoadFile(source: string, bareEnv: BareEnv): TypedEnv {
  const blocks = mergeDefinitions(source)
  const tenv: TypedEnv = new Map()

  for (const [name, tree] of bareEnv) {
    tenv.set(name, { tree, type: OPAQUE })
  }

  for (const block of blocks) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const result = parseLine(trimmed)
    if ("tag" in result) continue

    const decl = result
    if (!decl.type) continue

    const bareTree = bareEnv.get(decl.name)
    if (!bareTree) continue

    const declType = compileType(decl.type, bareEnv)
    tenv.set(decl.name, { tree: bareTree, type: declType })
  }

  return tenv
}
