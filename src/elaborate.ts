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
import { SExpr, SDecl, ParseError, parseExpr, parseLine, mergeDefinitions } from "./parse.js"

// === Opaque type sentinel ===
// Used for bare mode where types are unknown.
const OPAQUE: Tree = stem(LEAF)

// === Environment ===
// Every definition carries its tree, type predicate, and optional Pi structure.
// - type: a predicate tree (apply(type, value) → tt/ff). OPAQUE if untyped.
//         For Pi types before Phase 5: raw Pi pair fork(A, K(B)).
//         For Pi types after Phase 5: specialized triage predicate.
// - piPair: raw Pi structure for elaboration (peel binder layers, ascription wrapping).
//         Only present for Pi-typed entries. Survives after Phase 5.
//         wrapAscription uses fork(piPair.domain, piPair.codomain), never type.

export type EnvEntry = {
  tree: Tree,
  type: Tree,
  piPair?: { domain: Tree, codomain: Tree }
}

export type Env = Map<string, EnvEntry>

/** Create an untyped env entry */
export function entry(tree: Tree): EnvEntry {
  return { tree, type: OPAQUE }
}

/** Specialize a raw Pi pair into an evaluable predicate via piPred.
 *  Returns the raw pair unchanged if piPred is not available in env. */
function specializePiType(rawPiPair: Tree, env: Env): Tree {
  const pp = env.get("piPred")?.tree
  if (!pp || !isFork(rawPiPair)) return rawPiPair
  const budget = { remaining: 500_000 }
  return apply(apply(pp, rawPiPair.left, budget), rawPiPair.right, budget)
}

// === Allowlist ===
// Reserved env key for the allowlist tree (list of (body, rawPiPair) pairs).
const ALLOWLIST_KEY = "__allowlist__"

/** Build allowlist from all Pi-typed env entries.
 *  Format: fork(fork(body1, ty1), fork(fork(body2, ty2), ... LEAF)) */
function buildAllowlist(env: Env): Tree {
  let al: Tree = LEAF
  for (const [name, e] of env) {
    if (name.startsWith("__") || !e.piPair) continue
    const rawPi = fork(e.piPair.domain, e.piPair.codomain)
    al = fork(fork(e.tree, rawPi), al)
  }
  return al
}

/** Append a (body, rawPiPair) entry to an existing allowlist. */
function appendAllowlist(al: Tree, body: Tree, rawPi: Tree): Tree {
  return fork(fork(body, rawPi), al)
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

/** Detect Pi pair from type SExpr + compiled tree + env.
 *  A type is Pi-shaped if:
 *  1. The SExpr is spi (direct Pi annotation), or
 *  2. The SExpr is svar resolving to an env entry with piPair (type alias)
 *  Returns {domain, codomain} or null. */
function detectPiPair(
  typeExpr: SExpr | null,
  compiledType: Tree,
  env: Env,
): { domain: Tree, codomain: Tree } | undefined {
  if (!isFork(compiledType)) return undefined
  // Direct Pi annotation
  if (typeExpr?.tag === "spi") {
    return { domain: compiledType.left, codomain: compiledType.right }
  }
  // Type alias: svar that resolves to a Pi-typed definition
  if (typeExpr?.tag === "svar") {
    const entry = env.get(typeExpr.name)
    if (entry?.piPair) {
      return { domain: compiledType.left, codomain: compiledType.right }
    }
  }
  return undefined
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

/** Check if a variable name appears free in surface syntax */
function freeInSExpr(name: string, sexpr: SExpr): boolean {
  switch (sexpr.tag) {
    case "svar": return sexpr.name === name
    case "sapp": return freeInSExpr(name, sexpr.func) || freeInSExpr(name, sexpr.arg)
    case "slam": return !sexpr.params.includes(name) && freeInSExpr(name, sexpr.body)
    case "spi": return freeInSExpr(name, sexpr.domain) ||
      (sexpr.name !== name && freeInSExpr(name, sexpr.codomain))
    case "stype": case "stree": return false
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
function newCheckBudget(): { remaining: number } {
  return { remaining: 500_000 }
}

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

export function extract(tree: Tree): Tree {
  if (!tree) throw new Error("extract: undefined tree")
  if (isLeaf(tree)) return tree
  if (isStem(tree)) return stem(extract(tree.child))
  const left = tree.left
  const right = tree.right
  if (isLeaf(left)) return fork(LEAF, extract(right))
  if (isStem(left)) {
    if (isStem(right)) return right.child  // ascription: unwrap body
    if (isFork(right)) return fork(stem(extract(right.left)), extract(right.right))
    // stem(D) with leaf right — shouldn't happen in valid annotated trees
    return fork(left, right)
  }
  return fork(fork(extract(left.left), extract(left.right)), extract(right))
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
        // Expr ty field uses raw Pi pair (for elaboration: binder peeling, result type extraction).
        // Env type field stores the specialized predicate (for checking).
        const exprTy = e.piPair ? fork(e.piPair.domain, e.piPair.codomain) : e.type
        if (typed && e.piPair) {
          // Ascriptions carry the raw Pi pair (type identity), not the type predicate.
          // PiCheck's sOrAsc compares against fork(A, B), so we must use the raw pair.
          return lit(wrapAscription(e.tree, exprTy), exprTy)
        }
        return lit(e.tree, exprTy)
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

  const rawType = decl.type ? compileType(decl.type, env) : OPAQUE
  let piPair = detectPiPair(decl.type, rawType, env)
  // If the value is a Pi-shaped tree (fork(A, K(B))) and the annotation is Type,
  // detect it as a type alias for a Pi type.
  if (!piPair && treeEqual(rawType, LEAF) && isFork(tree) && unwrapK(tree.right) !== null) {
    piPair = { domain: tree.left, codomain: tree.right }
  }
  const type = piPair ? specializePiType(rawType, env) : rawType
  newEnv.set(decl.name, { tree, type, piPair })
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
    const rawType = compileType(decl.type, env)
    const piPair = detectPiPair(decl.type, rawType, env)
    const type = piPair ? specializePiType(rawType, env) : rawType
    env.set(decl.name, { tree: existing.tree, type, piPair })
  }

  // Build allowlist from all Pi-typed definitions (if allowlistCheck is available)
  if (env.has("allowlistCheck")) {
    env.set(ALLOWLIST_KEY, { tree: buildAllowlist(env), type: OPAQUE })
  }

  return env
}

/** Load a typed .disp file. Every declaration must have a type annotation.
 *  Each definition is compiled and type-checked via the typed pipeline.
 *  Returns updated env with all definitions verified. */
export function loadTypedFile(source: string, env: Env): Env {
  const blocks = mergeDefinitions(source)

  for (const block of blocks) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const result = parseLine(trimmed)
    if ("tag" in result) continue // skip bare expressions
    if (!result.type) {
      // Untyped decls in typed files: compile bare (useful for type aliases)
      env = declare(result, env)
      continue
    }
    const checkResult = typecheckDeclSource(block.text.trim(), env)
    if (!checkResult.ok) {
      throw new Error(`Type error in ${result.name} [${checkResult.stage}]: ${checkResult.message}`)
    }
    env = checkResult.env
  }

  return env
}

// === Typed mode: bracket abstraction with S-node annotations ===

/** Compile a lambda with typed bracket abstraction.
 *  expectedType = Pi(A, K(T)) — determines parameter types.
 *  Returns annotated tree with D at S-nodes + ascriptions for typed refs.
 *  When a dependent codomain is encountered, remaining params are compiled bare
 *  and the result is wrapped in an ascription with the expected type.
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
  let hasOpaqueParams = false
  for (const param of params) {
    if (!isFork(curType)) {
      // After hitting a dependent codomain (or non-Pi type), remaining params are bare
      paramTypes.push(OPAQUE)
      hasOpaqueParams = true
      continue
    }
    paramTypes.push(curType.left)
    const inner = unwrapK(curType.right)
    if (inner !== null) {
      curType = inner  // Non-dependent: unwrap K
    } else {
      // Dependent codomain: can't peel further statically.
      curType = OPAQUE
    }
  }

  // Build free-variable type map
  const freeTypes = new Map<string, Tree>()
  for (let i = 0; i < params.length; i++) {
    freeTypes.set(params[i], paramTypes[i])
  }

  // Convert body to Expr with types (typed=true for ascription wrapping)
  const bodyExpr = sexprToExpr(body, env, freeTypes, true)

  // Chain bracket abstractions for all params (inside-out)
  // Typed for params with known types, bare for OPAQUE params
  let curExpr = bodyExpr
  for (let i = params.length - 1; i >= 0; i--) {
    curExpr = abstract(params[i], paramTypes[i], curExpr)
  }

  const tree = collapse(curExpr, envTrees(env))

  // If inner params were compiled bare (OPAQUE) due to dependent codomains,
  // PiCheck can't structurally verify them. Wrap in ascription so the
  // ascription rule (type identity check) handles verification.
  if (hasOpaqueParams) {
    return wrapAscription(tree, expectedType)
  }

  return tree
}

// === Type-level expression compilation ===
// Converts type-level SExprs to Expr IR so bracket abstraction can produce
// codomain families for dependent Pi types.

function sexprToTypeExpr(
  sexpr: SExpr,
  env: Env,
  typeVars: Map<string, Tree>,
): Expr {
  switch (sexpr.tag) {
    case "svar": {
      if (typeVars.has(sexpr.name)) return evar(sexpr.name, OPAQUE)
      const e = env.get(sexpr.name)
      if (e) return lit(e.tree, OPAQUE)
      throw new Error(`Unknown type: ${sexpr.name}`)
    }
    case "sapp": {
      const func = sexprToTypeExpr(sexpr.func, env, typeVars)
      const arg = sexprToTypeExpr(sexpr.arg, env, typeVars)
      return app(func, arg, OPAQUE)
    }
    case "spi": {
      const domainExpr = sexprToTypeExpr(sexpr.domain, env, typeVars)
      if (sexpr.name === "_" || !freeInSExpr(sexpr.name, sexpr.codomain)) {
        const codomainExpr = sexprToTypeExpr(sexpr.codomain, env, typeVars)
        // Build fork(domain, K(codomain)) as Expr:
        // fork(d, K(c)) = apply(apply(LEAF, d), apply(K, c))
        return app(
          app(lit(LEAF, OPAQUE), domainExpr, OPAQUE),
          app(lit(K, OPAQUE), codomainExpr, OPAQUE),
          OPAQUE,
        )
      }
      // Dependent: bracket-abstract the codomain over the binder
      const trees = envTrees(env)
      for (const [k, v] of typeVars) trees.set(k, v)
      const domainTree = collapse(domainExpr, trees)
      const newTypeVars = new Map(typeVars)
      newTypeVars.set(sexpr.name, domainTree)
      const codomainExpr = sexprToTypeExpr(sexpr.codomain, env, newTypeVars)
      const newTrees = envTrees(env)
      for (const [k, v] of newTypeVars) newTrees.set(k, v)
      const codomainFamily = collapse(abstract(sexpr.name, OPAQUE, codomainExpr), newTrees)
      return lit(fork(domainTree, codomainFamily), OPAQUE)
    }
    case "stype": return lit(LEAF, OPAQUE)
    case "stree": return lit(stem(LEAF), OPAQUE)
    case "slam": throw new Error("Lambda in type position not supported")
  }
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
      if (sexpr.name === "_" || !freeInSExpr(sexpr.name, sexpr.codomain)) {
        // Non-dependent (or named but codomain doesn't use binder)
        const codomain = compileType(sexpr.codomain, env)
        return fork(domain, kType(codomain))
      }
      // Dependent: bracket-abstract the codomain over the binder to produce a family
      resetCompileBudget()
      const typeVars = new Map<string, Tree>([[sexpr.name, domain]])
      const codomainExpr = sexprToTypeExpr(sexpr.codomain, env, typeVars)
      const trees = envTrees(env)
      trees.set(sexpr.name, domain)
      const codomainFamily = collapse(abstract(sexpr.name, OPAQUE, codomainExpr), trees)
      return fork(domain, codomainFamily)
    }
    case "stype":
      // Type = Tree predicate if available, else LEAF as identity
      return env.get("Tree")?.tree ?? LEAF
    case "stree":
      return stem(LEAF)
    case "sapp":
      return apply(compileType(sexpr.func, env), compileType(sexpr.arg, env), COMPILE_BUDGET)
    case "slam":
      throw new Error("Lambda in type position not supported")
  }
}

export type TypecheckStage = "parse" | "elaborate" | "check"

export type ExprCheckResult =
  | { ok: true, tree: Tree, type: Tree }
  | { ok: false, stage: TypecheckStage, message: string }

export type DeclCheckResult =
  | { ok: true, name: string, tree: Tree, type: Tree, env: Env }
  | { ok: false, stage: TypecheckStage, message: string }

/** Check tree against a type predicate + allowlist.
 *  Phase 1: piPred/base predicate check (structural type correctness).
 *  Phase 2: allowlistCheck (ascription body verification). */
function checkAgainstType(tree: Tree, typePred: Tree, env: Env): boolean {
  // Phase 1: type predicate check
  if (!treeEqual(apply(typePred, tree, newCheckBudget()), LEAF)) return false

  // Phase 2: allowlist check (if available)
  const alEntry = env.get(ALLOWLIST_KEY)
  const alChecker = env.get("allowlistCheck")?.tree
  if (alEntry && alChecker) {
    const walker = apply(alChecker, alEntry.tree, newCheckBudget())
    if (!treeEqual(apply(walker, tree, newCheckBudget()), LEAF)) return false
  }

  return true
}

function compileCheckedExpr(sexpr: SExpr, expectedType: Tree, env: Env): Tree {
  if (sexpr.tag === "slam") {
    return typedCompileLam(sexpr.params, sexpr.body, expectedType, env)
  }
  // Pi/Type expressions in value position (e.g., `let MyFn : Type := Bool -> Bool`)
  if (sexpr.tag === "spi" || sexpr.tag === "stype" || sexpr.tag === "stree") {
    return compileType(sexpr, env)
  }
  return compile(sexpr, env)
}

export function typecheckExprSource(
  source: string,
  expectedTypeSource: string,
  env: Env,
): ExprCheckResult {
  let sexpr: SExpr
  let expectedTypeExpr: SExpr
  try {
    sexpr = parseExpr(source)
    expectedTypeExpr = parseExpr(expectedTypeSource)
  } catch (e) {
    if (e instanceof ParseError) return { ok: false, stage: "parse", message: e.message }
    return { ok: false, stage: "parse", message: (e as Error).message }
  }

  let rawType: Tree
  let typePred: Tree
  let tree: Tree
  try {
    rawType = compileType(expectedTypeExpr, env)
    const piPair = detectPiPair(expectedTypeExpr, rawType, env)
    typePred = piPair ? specializePiType(rawType, env) : rawType
    tree = compileCheckedExpr(sexpr, rawType, env)
  } catch (e) {
    return { ok: false, stage: "elaborate", message: (e as Error).message }
  }

  // Pre-add compiled tree to allowlist for ascription-wrapped trees
  let checkEnv = env
  if (isFork(rawType)) {
    checkEnv = new Map(env)
    const al = checkEnv.get(ALLOWLIST_KEY)?.tree ?? LEAF
    checkEnv.set(ALLOWLIST_KEY, { tree: appendAllowlist(al, extract(tree), rawType), type: OPAQUE })
  }

  try {
    if (!checkAgainstType(tree, typePred, checkEnv)) {
      return { ok: false, stage: "check", message: "Expression does not satisfy expected type" }
    }
  } catch (e) {
    return { ok: false, stage: "check", message: (e as Error).message }
  }

  return { ok: true, tree, type: typePred }
}

export function typecheckDeclSource(source: string, env: Env): DeclCheckResult {
  let parsed: SDecl | SExpr
  try {
    parsed = parseLine(source)
  } catch (e) {
    if (e instanceof ParseError) return { ok: false, stage: "parse", message: e.message }
    return { ok: false, stage: "parse", message: (e as Error).message }
  }

  if ("tag" in parsed) {
    return { ok: false, stage: "parse", message: "Expected declaration, got expression" }
  }
  if (!parsed.type) {
    return { ok: false, stage: "elaborate", message: "Typed declaration required" }
  }

  let rawType: Tree
  let piPair: { domain: Tree, codomain: Tree } | undefined
  let typePred: Tree
  try {
    rawType = compileType(parsed.type, env)
    piPair = detectPiPair(parsed.type, rawType, env)
    typePred = piPair ? specializePiType(rawType, env) : rawType
  } catch (e) {
    return { ok: false, stage: "elaborate", message: (e as Error).message }
  }

  if (parsed.isRec) {
    return typecheckRecDecl(parsed, rawType, typePred, piPair, env)
  }

  let tree: Tree
  try {
    // compileCheckedExpr needs the raw Pi pair for typed lambda compilation
    tree = compileCheckedExpr(parsed.value, rawType, env)
  } catch (e) {
    return { ok: false, stage: "elaborate", message: (e as Error).message }
  }

  // Pre-add the compiled tree to allowlist before checking, so that
  // ascription-wrapped trees (from typedCompileLam) can pass allowlistCheck.
  // The elaborator is the trusted component: if it compiled the tree, it's allowed.
  let checkEnv = env
  if (piPair) {
    checkEnv = new Map(env)
    const al = checkEnv.get(ALLOWLIST_KEY)?.tree ?? LEAF
    const rawPi = fork(piPair.domain, piPair.codomain)
    checkEnv.set(ALLOWLIST_KEY, { tree: appendAllowlist(al, extract(tree), rawPi), type: OPAQUE })
  }

  try {
    if (!checkAgainstType(tree, typePred, checkEnv)) {
      return { ok: false, stage: "check", message: "Declaration value does not satisfy annotation" }
    }
  } catch (e) {
    return { ok: false, stage: "check", message: (e as Error).message }
  }

  const newEnv = new Map(env)
  const extractedTree = extract(tree)
  newEnv.set(parsed.name, { tree: extractedTree, type: typePred, piPair })
  // Append to allowlist if Pi-typed
  if (piPair) {
    const al = newEnv.get(ALLOWLIST_KEY)?.tree ?? LEAF
    const rawPi = fork(piPair.domain, piPair.codomain)
    newEnv.set(ALLOWLIST_KEY, { tree: appendAllowlist(al, extractedTree, rawPi), type: OPAQUE })
  }
  return { ok: true, name: parsed.name, tree, type: typePred, env: newEnv }
}

function typecheckRecDecl(
  parsed: SDecl,
  rawType: Tree,
  typePred: Tree,
  piPair: { domain: Tree, codomain: Tree } | undefined,
  env: Env,
): DeclCheckResult {
  const fixTree = env.get("fix")?.tree
  if (!fixTree) return { ok: false, stage: "elaborate", message: "Recursive def requires 'fix' in env" }

  resetCompileBudget()

  let stepTree: Tree
  try {
    // Build step function by abstracting the recursive name from the body
    // Put the recursive name in env with expected type so typed elaboration can ascription-wrap it
    const tempEnv = new Map(env)
    tempEnv.set(parsed.name, { tree: I, type: typePred, piPair })

    const bodyExpr = sexprToExpr(parsed.value, tempEnv, new Map(), piPair !== undefined)
    const stepExpr = abstract(parsed.name, OPAQUE, bodyExpr)
    const tempTrees = envTrees(tempEnv)
    stepTree = collapse(stepExpr, tempTrees)
  } catch (e) {
    return { ok: false, stage: "elaborate", message: (e as Error).message }
  }

  // Check step : T -> T (the step function must preserve the type)
  if (piPair) {
    try {
      const stepRawType = fork(rawType, kType(rawType))
      const stepPred = specializePiType(stepRawType, env)
      // Pre-add the recursive placeholder (I at declared type) + step tree to allowlist
      const checkEnv = new Map(env)
      const al = checkEnv.get(ALLOWLIST_KEY)?.tree ?? LEAF
      const rawPi = fork(piPair.domain, piPair.codomain)
      const al2 = appendAllowlist(appendAllowlist(al, I, rawPi), extract(stepTree), stepRawType)
      checkEnv.set(ALLOWLIST_KEY, { tree: al2, type: OPAQUE })
      if (!checkAgainstType(stepTree, stepPred, checkEnv)) {
        return { ok: false, stage: "check", message: "Recursive step does not preserve type" }
      }
    } catch (e) {
      return { ok: false, stage: "check", message: (e as Error).message }
    }
  }

  // Build fixed point
  const tree = apply(fixTree, stepTree, COMPILE_BUDGET)

  const newEnv = new Map(env)
  newEnv.set(parsed.name, { tree, type: typePred, piPair })
  // Append fixpoint to allowlist
  if (piPair) {
    const al = newEnv.get(ALLOWLIST_KEY)?.tree ?? LEAF
    const rawPi = fork(piPair.domain, piPair.codomain)
    newEnv.set(ALLOWLIST_KEY, { tree: appendAllowlist(al, tree, rawPi), type: OPAQUE })
  }
  return { ok: true, name: parsed.name, tree, type: typePred, env: newEnv }
}
