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
  | { tag: "eS", left: Expr, right: Expr }      // S(f, g) — bare, no annotation
  | { tag: "eTyS", left: Expr, right: Expr, D: Tree }  // S(f, g) with intermediate type D

export function eTree(tree: Tree): Expr { return { tag: "eTree", tree } }
export function eFvar(name: string): Expr { return { tag: "eFvar", name } }
export function eApp(func: Expr, arg: Expr): Expr { return { tag: "eApp", func, arg } }
function eS(left: Expr, right: Expr): Expr { return { tag: "eS", left, right } }
function eTyS(left: Expr, right: Expr, D: Tree): Expr { return { tag: "eTyS", left, right, D } }

// === Free variable check ===

function freeIn(name: string, expr: Expr): boolean {
  switch (expr.tag) {
    case "eTree": return false
    case "eFvar": return expr.name === name
    case "eApp": return freeIn(name, expr.func) || freeIn(name, expr.arg)
    case "eS": return freeIn(name, expr.left) || freeIn(name, expr.right)
    case "eTyS": return freeIn(name, expr.left) || freeIn(name, expr.right)
  }
}

// === Bracket abstraction: [name] expr → Expr ===

function bracketAbstract(name: string, expr: Expr): Expr {
  switch (expr.tag) {
    case "eTree":
      return eApp(eTree(K), expr)

    case "eFvar":
      return expr.name === name ? eTree(I) : eApp(eTree(K), expr)

    case "eS":
    case "eTyS": {
      const fInL = freeIn(name, expr.left)
      const fInR = freeIn(name, expr.right)
      if (!fInL && !fInR) return eApp(eTree(K), expr)
      // eS/eTyS(f, g) = fork(stem(f), g) = apply(apply(leaf, apply(leaf, f)), g)
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
    case "eTyS": {
      const f = collapse(expr.left, env)
      const g = collapse(expr.right, env)

      // Optimizations that eliminate S-nodes (no annotation needed)
      if (isFork(f) && isLeaf(f.left) && isFork(g) && isLeaf(g.left)) {
        return fork(LEAF, apply(f.right, g.right, COMPILE_BUDGET))
      }
      if (isFork(f) && isLeaf(f.left) && treeEqual(g, I)) {
        return f.right
      }

      // Annotated S-node: fork(stem(D), fork(c, b))
      return fork(stem(expr.D), fork(f, g))
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

// ============================================================
// === Typed mode: bracket abstraction with S-node annotations
// ============================================================
//
// Runs after types.disp is loaded. Produces annotated trees where
// S-nodes carry their intermediate type D.
//
// Pipeline: SExpr → TExpr (with types) → Expr (with eTyS) → Tree
//
// The typed elaborator requires:
//   - A TypedEnv mapping names to { tree, type }
//   - A type annotation on lambdas (from declaration type or expected type)

// === Typed expression IR ===

export type TExpr =
  | { tag: "tLit", tree: Tree, ty: Tree }    // resolved constant with known type
  | { tag: "tVar", name: string, ty: Tree }   // free variable (lambda param) with type
  | { tag: "tApp", func: TExpr, arg: TExpr, ty: Tree }
  | { tag: "tBare", expr: Expr, ty: Tree }    // bare sub-expression (inner lambda etc.)
  | { tag: "tK", body: TExpr, ty: Tree }      // K(body) from bracket abstraction
  | { tag: "tS", left: TExpr, right: TExpr, D: Tree, ty: Tree }  // S(c,b) with D

export type TypedEnv = Map<string, { tree: Tree, type: Tree }>

// === Type helpers ===

/** K-wrap a type: K(T) = fork(LEAF, T) */
function kType(t: Tree): Tree { return fork(LEAF, t) }

/** Unwrap K(T) → T. Returns null if not K-shaped. */
function unwrapK(family: Tree): Tree | null {
  if (isFork(family) && isLeaf(family.left)) return family.right
  return null
}

// === SExpr → TExpr conversion ===

/** Convert SExpr to typed expression, inferring types from env. */
function sexprToTExpr(
  sexpr: SExpr,
  tenv: TypedEnv,
  freeTypes: Map<string, Tree>,  // lambda params: name → type
): TExpr {
  switch (sexpr.tag) {
    case "svar": {
      // Check lambda params first
      const ft = freeTypes.get(sexpr.name)
      if (ft !== undefined) return { tag: "tVar", name: sexpr.name, ty: ft }
      // Check typed env
      const entry = tenv.get(sexpr.name)
      if (entry) return { tag: "tLit", tree: entry.tree, ty: entry.type }
      throw new Error(`Unbound typed variable: ${sexpr.name}`)
    }
    case "sapp": {
      const func = sexprToTExpr(sexpr.func, tenv, freeTypes)
      const arg = sexprToTExpr(sexpr.arg, tenv, freeTypes)
      if (!isFork(func.ty)) {
        // Not a Pi type — untyped/opaque function (e.g. triage, fix).
        // Propagate the type through: result is also opaque.
        return { tag: "tApp", func, arg, ty: func.ty }
      }
      const B = func.ty.right  // codomain family
      const T = unwrapK(B)
      if (T === null) {
        // Dependent codomain — treat as opaque for now
        return { tag: "tApp", func, arg, ty: func.ty }
      }
      return { tag: "tApp", func, arg, ty: T }
    }
    case "slam": {
      // Inner lambda: compile body, then bracket-abstract inner params (bare).
      // Outer free variables stay as eFvar/tVar through tBare.
      const bodyTExpr = sexprToTExpr(sexpr.body, tenv, freeTypes)
      let bodyExpr = tExprToExpr(bodyTExpr)
      for (let i = sexpr.params.length - 1; i >= 0; i--) {
        bodyExpr = bracketAbstract(sexpr.params[i], bodyExpr)
      }
      return { tag: "tBare", expr: bodyExpr, ty: stem(LEAF) }  // opaque type
    }
    case "stype":
      return { tag: "tLit", tree: LEAF, ty: LEAF }
    case "stree":
      return { tag: "tLit", tree: stem(LEAF), ty: LEAF }
    case "spi":
      throw new Error("Pi type expressions not supported in typed elaboration yet")
  }
}

// === Free variable check for TExpr ===

function tFreeIn(name: string, texpr: TExpr): boolean {
  switch (texpr.tag) {
    case "tLit": return false
    case "tVar": return texpr.name === name
    case "tApp": return tFreeIn(name, texpr.func) || tFreeIn(name, texpr.arg)
    case "tBare": return freeIn(name, texpr.expr)
    case "tK": return tFreeIn(name, texpr.body)
    case "tS": return tFreeIn(name, texpr.left) || tFreeIn(name, texpr.right)
  }
}

// === TExpr → Expr (drop types, used for K-wrapping non-free sub-exprs) ===

function tExprToExpr(texpr: TExpr): Expr {
  switch (texpr.tag) {
    case "tLit": return eTree(texpr.tree)
    case "tVar": return eFvar(texpr.name)
    case "tApp": return eApp(tExprToExpr(texpr.func), tExprToExpr(texpr.arg))
    case "tBare": return texpr.expr
    case "tK": return eApp(eTree(K), tExprToExpr(texpr.body))
    case "tS": return eTyS(tExprToExpr(texpr.left), tExprToExpr(texpr.right), texpr.D)
  }
}

// === Typed bracket abstraction ===
//
// [x : A] texpr → TExpr with type Pi(A, K(resultType))
// S-nodes get tS with D = K(intermediate type).
// Returns TExpr so abstractions can be chained for multi-param lambdas.

/** Extract the "result type" (the T in Pi(A, K(T))) from a TExpr returned by typedAbstract */
function absResultType(texpr: TExpr): Tree {
  // The type is Pi(A, K(T)) = fork(A, fork(LEAF, T))
  if (isFork(texpr.ty)) {
    const inner = unwrapK(texpr.ty.right)
    if (inner !== null) return inner
  }
  return texpr.ty  // fallback for opaque types
}

function typedAbstract(name: string, nameType: Tree, texpr: TExpr): TExpr {
  const piType = (resultType: Tree) => fork(nameType, kType(resultType))

  switch (texpr.tag) {
    case "tLit":
      // x not free → K(c)
      return { tag: "tK", body: texpr, ty: piType(texpr.ty) }

    case "tBare": {
      // Bare sub-expression — check if name is free
      if (!freeIn(name, texpr.expr)) {
        return { tag: "tK", body: texpr, ty: piType(texpr.ty) }
      }
      // Name is free: fall back to bare bracket abstraction, wrap result
      const abstracted = bracketAbstract(name, texpr.expr)
      return { tag: "tBare", expr: abstracted, ty: piType(texpr.ty) }
    }

    case "tK": {
      // K(body) — name could be free inside body
      if (!tFreeIn(name, texpr)) {
        return { tag: "tK", body: texpr, ty: piType(texpr.ty) }
      }
      // Convert to app form and re-abstract
      const asApp: TExpr = { tag: "tApp",
        func: { tag: "tLit", tree: K, ty: stem(LEAF) },
        arg: texpr.body, ty: texpr.ty }
      return typedAbstract(name, nameType, asApp)
    }

    case "tS": {
      const fInL = tFreeIn(name, texpr.left)
      const fInR = tFreeIn(name, texpr.right)
      if (!fInL && !fInR) {
        return { tag: "tK", body: texpr, ty: piType(texpr.ty) }
      }
      // Recursively abstract left and right, preserving inner D annotations
      const absL = fInL ? typedAbstract(name, nameType, texpr.left)
        : { tag: "tK" as const, body: texpr.left, ty: piType(texpr.left.ty) }
      const absR = fInR ? typedAbstract(name, nameType, texpr.right)
        : { tag: "tK" as const, body: texpr.right, ty: piType(texpr.right.ty) }
      const newD = kType(absResultType(absR))
      return { tag: "tS", left: absL, right: absR, D: newD, ty: piType(texpr.ty) }
    }

    case "tVar":
      if (texpr.name === name) {
        // [x]x = I
        return { tag: "tLit", tree: I, ty: piType(nameType) }
      }
      // x not free → K(y)
      return { tag: "tK", body: texpr, ty: piType(texpr.ty) }

    case "tApp": {
      const { func, arg } = texpr
      const fInF = tFreeIn(name, func)
      const fInA = tFreeIn(name, arg)

      if (!fInF && !fInA) {
        return { tag: "tK", body: texpr, ty: piType(texpr.ty) }
      }

      // Eta: [x](f x) = f when x not free in f
      if (!fInF && arg.tag === "tVar" && arg.name === name) {
        return func  // func already has the right type (it's a function A → T)
      }

      const absF = fInF ? typedAbstract(name, nameType, func)
        : { tag: "tK" as const, body: func, ty: piType(func.ty) }
      const absA = fInA ? typedAbstract(name, nameType, arg)
        : { tag: "tK" as const, body: arg, ty: piType(arg.ty) }

      // D = K(intermediate type) = K(result type of absA)
      const D = kType(absResultType(absA))

      return { tag: "tS", left: absF, right: absA, D, ty: piType(texpr.ty) }
    }
  }
}

// === Typed compilation entry points ===

/** Compile a lambda with typed bracket abstraction.
 *  expectedType = Pi(A, K(T)) — determines parameter types.
 *  Returns annotated tree with D at S-nodes.
 *
 *  For multi-param lambdas: inner params abstracted with bare mode,
 *  outermost param abstracted with typed mode (gets D annotations).
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

  // Build free-variable type map for ALL params
  const freeTypes = new Map<string, Tree>()
  for (let i = 0; i < params.length; i++) {
    freeTypes.set(params[i], paramTypes[i])
  }

  // Convert body to TExpr (all params are tVar with known types)
  const texpr = sexprToTExpr(body, tenv, freeTypes)

  // Chain typed bracket abstractions for ALL params (inside-out)
  let curTExpr: TExpr = texpr
  for (let i = params.length - 1; i >= 0; i--) {
    curTExpr = typedAbstract(params[i], paramTypes[i], curTExpr)
  }

  // Collapse to tree
  const bareEnv: BareEnv = new Map()
  for (const [name, entry] of tenv) bareEnv.set(name, entry.tree)
  return collapse(tExprToExpr(curTExpr), bareEnv)
}

// === Compile type annotations ===

/** Compile a type SExpr to a tree type.
 *  A -> B  →  fork(compile(A), K(compile(B)))  (non-dependent Pi)
 *  svar(name)  →  env lookup
 *  Type  →  LEAF
 *  Tree  →  stem(LEAF)
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
        // Non-dependent: A -> B = fork(A, K(B))
        return fork(domain, kType(codomain))
      }
      throw new Error("Dependent Pi types in annotations not yet supported")
    }
    case "stype":
      return LEAF  // Type = LEAF
    case "stree":
      return stem(LEAF)  // Tree = stem(LEAF)
    case "sapp": {
      // Application in type position: e.g. Vec n
      return apply(compileType(sexpr.func, env), compileType(sexpr.arg, env), COMPILE_BUDGET)
    }
    case "slam":
      throw new Error("Lambda in type position not supported")
  }
}

// === Two-pass typed loading ===

/** Load types.disp with two passes:
 *  Pass 1 (already done): bareLoadFile → BareEnv
 *  Pass 2: re-compile definitions with type annotations using typed elaboration.
 *  Returns TypedEnv with annotated trees for typed defs, bare trees for untyped.
 */
export function typedLoadFile(source: string, bareEnv: BareEnv): TypedEnv {
  const blocks = mergeDefinitions(source)
  const tenv: TypedEnv = new Map()

  // Seed the typed env with all bare definitions (no types)
  // These will be overwritten as typed versions are compiled
  for (const [name, tree] of bareEnv) {
    tenv.set(name, { tree, type: stem(LEAF) })  // default type = Tree
  }

  for (const block of blocks) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const result = parseLine(trimmed)
    if ("tag" in result) continue  // standalone expression

    const decl = result
    if (!decl.type) {
      // No type annotation — keep bare tree with Tree type
      continue
    }

    // Compile the type annotation using the bare env
    const declType = compileType(decl.type, bareEnv)

    // Get the bare tree (already compiled in pass 1)
    const bareTree = bareEnv.get(decl.name)
    if (!bareTree) continue

    // Re-compile with typed elaboration if it's a lambda
    if (decl.value.tag === "slam") {
      resetCompileBudget()
      try {
        const annotatedTree = typedCompileLam(
          decl.value.params, decl.value.body, declType, tenv
        )
        tenv.set(decl.name, { tree: annotatedTree, type: declType })
      } catch {
        // Typed compilation failed (e.g. dependent type, missing binding)
        // Fall back to bare tree
        tenv.set(decl.name, { tree: bareTree, type: declType })
      }
    } else {
      // Non-lambda: use bare tree, store the type
      tenv.set(decl.name, { tree: bareTree, type: declType })
    }
  }

  return tenv
}
