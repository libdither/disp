// Tree-native elaboration: two-phase pipeline from surface syntax to annotated trees.
//
// Phase 1: elaborate(sexpr, env, expectedType?) → TypedExpr
//   Walks SExpr, resolves names, computes types (evaluating type-level trees
//   for dependent codomains). Lambdas stay as `lam` nodes. No compilation.
//
// Phase 2: compile(typedExpr) → annotated Tree
//   Bracket-abstracts all lambdas (inside-out), producing combI/combK/combS nodes.
//   Collapses to annotated tree with D types embedded at S nodes.

import { type Tree, LEAF, stem, fork, apply, treeEqual, I, isLeaf, isStem, isFork } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, kOf, collapseAndEval, compileRecAndEval } from "./compile.js"
import { freshMarker, registerNativeBuiltinId } from "./native-utils.js"
import { type SExpr } from "./parse.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow,
  annK, annS, annAscribe,
  extract,
  isNonDep,
  type KnownDefs, checkAnnotated,
} from "./tree-native-checker.js"

// ============================================================
// TypedExpr: typed expression tree
// ============================================================

// Source-level nodes (produced by elaborate):
//   tree — resolved constant (builtin, definition, type value)
//   var  — bound variable (from lambda parameter)
//   app  — function application (unevaluated)
//   lam  — lambda abstraction (with explicit parameter type)
//
// Combinator nodes (produced by bracket abstraction):
//   combI — identity combinator [x]x
//   combK — constant combinator [x]c where x not free in c
//   combS — sharing combinator [x](f g) where x free in both f and g
//           carries D (intermediate type) for annotation

export type TypedExpr =
  | { tag: "tree",  value: Tree, type: Tree }
  | { tag: "var",   name: string, type: Tree }
  | { tag: "app",   func: TypedExpr, arg: TypedExpr, type: Tree }
  | { tag: "lam",   param: string, domain: Tree, body: TypedExpr, type: Tree }
  | { tag: "combI", type: Tree }
  | { tag: "combK", inner: TypedExpr, type: Tree }
  | { tag: "combS", c: TypedExpr, b: TypedExpr, D: Tree, type: Tree }

function teTree(value: Tree, type: Tree): TypedExpr { return { tag: "tree", value, type } }
function teVar(name: string, type: Tree): TypedExpr { return { tag: "var", name, type } }
function teApp(func: TypedExpr, arg: TypedExpr, type: Tree): TypedExpr { return { tag: "app", func, arg, type } }
function teLam(param: string, domain: Tree, body: TypedExpr, type: Tree): TypedExpr { return { tag: "lam", param, domain, body, type } }
function teCombI(type: Tree): TypedExpr { return { tag: "combI", type } }
function teCombK(inner: TypedExpr, type: Tree): TypedExpr { return { tag: "combK", inner, type } }
function teCombS(c: TypedExpr, b: TypedExpr, D: Tree, type: Tree): TypedExpr { return { tag: "combS", c, b, D, type } }

// ============================================================
// NativeEnv and errors
// ============================================================

export type NativeEnv = Map<string, { tree: Tree, annTree: Tree, type: Tree }>

export class NativeElabError extends Error {
  constructor(msg: string) { super(msg); this.name = "NativeElabError" }
}

// ============================================================
// Pretty-print a tree-native type
// ============================================================

function depPiPlaceholder(depth: number): Tree {
  let idx: Tree = LEAF
  for (let i = 0; i < depth; i++) idx = stem(idx)
  return fork(stem(stem(stem(stem(LEAF)))), idx)
}

export function printNativeType(type: Tree, nameMap?: Map<number, string>, depth = 0): string {
  if (depth > 20) return "..."
  if (treeEqual(type, TN_TYPE)) return "Type"
  if (treeEqual(type, TN_TREE)) return "Tree"
  if (treeEqual(type, TN_BOOL)) return "Bool"
  if (treeEqual(type, TN_NAT)) return "Nat"

  if (nameMap) {
    const name = nameMap.get(type.id)
    if (name) return name
  }

  if (isFork(type)) {
    const A = type.left
    const B = type.right
    const nd = isNonDep(B)
    if (nd !== null) {
      const domStr = isFork(A) ? `(${printNativeType(A, nameMap, depth)})` : printNativeType(A, nameMap, depth)
      return `${domStr} -> ${printNativeType(nd, nameMap, depth)}`
    }
    const bname = String.fromCharCode(97 + (depth % 26))
    const placeholder = depPiPlaceholder(depth)
    let bodyType: Tree
    try {
      bodyType = apply(B, placeholder)
    } catch {
      return `<type#${type.id}>`
    }
    const extMap = new Map(nameMap ?? [])
    extMap.set(placeholder.id, bname)
    const domStr = isFork(A) ? `(${printNativeType(A, extMap, depth)})` : printNativeType(A, extMap, depth)
    const codStr = printNativeType(bodyType, extMap, depth + 1)
    return `(${bname} : ${domStr}) -> ${codStr}`
  }

  return `<type#${type.id}>`
}

// ============================================================
// Safe apply for type-level computation
// ============================================================

function safeApply(f: Tree, x: Tree): Tree {
  try {
    return apply(f, x, { remaining: 100000 })
  } catch {
    return fork(f, x)
  }
}

// ============================================================
// Phase 1: elaborate — SExpr → TypedExpr
// ============================================================

// Helper: abstract a marker out of a type-level tree (for Pi codomain functions).
// Uses Expr bracket abstraction since this is type-level computation, not program compilation.
function abstractTypeMarker(tree: Tree, marker: Tree): Tree {
  const name = "__type_marker__"
  const expr = treeToExprReplacing(tree, marker, name, 0)
  if (expr === null) return fork(LEAF, tree) // K(tree) fallback
  return collapseAndEval(bracketAbstract(name, expr))
}

function treeContains(tree: Tree, target: Tree): boolean {
  const visited = new Set<number>()
  const stack: Tree[] = [tree]
  while (stack.length > 0) {
    const t = stack.pop()!
    if (treeEqual(t, target)) return true
    if (visited.has(t.id)) continue
    visited.add(t.id)
    if (isStem(t)) stack.push(t.child)
    else if (isFork(t)) { stack.push(t.left); stack.push(t.right) }
  }
  return false
}

function treeToExprReplacing(t: Tree, target: Tree, varName: string, depth: number): Expr | null {
  if (depth > 500) return null
  if (treeEqual(t, target)) return eFvar(varName)
  if (!treeContains(t, target)) return eTree(t)
  if (isLeaf(t)) return eTree(t)
  if (isStem(t)) {
    const child = treeToExprReplacing(t.child, target, varName, depth + 1)
    if (child === null) return null
    if (child.tag === "tree" && treeEqual(child.value, t.child)) return eTree(t)
    return eApp(eTree(LEAF), child)
  }
  const left = treeToExprReplacing(t.left, target, varName, depth + 1)
  if (left === null) return null
  const right = treeToExprReplacing(t.right, target, varName, depth + 1)
  if (right === null) return null
  if (left.tag === "tree" && treeEqual(left.value, t.left) &&
      right.tag === "tree" && treeEqual(right.value, t.right))
    return eTree(t)
  return eApp(eApp(eTree(LEAF), left), right)
}

export function elaborate(
  sexpr: SExpr, env: NativeEnv, expectedType?: Tree,
  boundNames?: Set<string>,
): TypedExpr {
  switch (sexpr.tag) {
    case "stype":
      return teTree(LEAF, TN_TYPE)

    case "stree":
      return teTree(TN_TREE, TN_TYPE)

    case "svar": {
      if (sexpr.name === "Tree") return teTree(TN_TREE, TN_TYPE)
      if (sexpr.name === "Bool") return teTree(TN_BOOL, TN_TYPE)
      if (sexpr.name === "Nat") return teTree(TN_NAT, TN_TYPE)

      const entry = env.get(sexpr.name)
      if (!entry) throw new NativeElabError(`Unbound variable: ${sexpr.name}`)

      if (boundNames?.has(sexpr.name)) {
        return teVar(sexpr.name, entry.type)
      }
      return teTree(entry.tree, entry.type)
    }

    case "spi": {
      const dom = elaborate(sexpr.domain, env, TN_TYPE, boundNames)
      if (!treeEqual(dom.type, TN_TYPE)) throw new NativeElabError("Pi domain must be a type")
      const domTree = typeTreeOf(dom)

      const m = freshMarker()
      const extEnv = new Map(env)
      if (sexpr.name !== "_") extEnv.set(sexpr.name, { tree: m, annTree: m, type: domTree })

      const cod = elaborate(sexpr.codomain, extEnv, TN_TYPE, boundNames)
      if (!treeEqual(cod.type, TN_TYPE)) throw new NativeElabError("Pi codomain must be a type")
      const codTree = typeTreeOf(cod)

      // Check dependency
      if (treeContains(codTree, m)) {
        const codFn = abstractTypeMarker(codTree, m)
        const piTree = fork(domTree, codFn)
        return teTree(piTree, TN_TYPE)
      }

      // Check Expr-level dependency (marker consumed by evaluation)
      if (sexpr.name !== "_" && teHasFreeVar(sexpr.name, cod)) {
        // Build codomain function via Expr bracket abstraction
        const codExpr = typedExprToExpr(cod)
        const codFn = collapseAndEval(bracketAbstract(sexpr.name, codExpr))
        const piTree = fork(domTree, codFn)
        return teTree(piTree, TN_TYPE)
      }

      const arrTree = tnArrow(domTree, codTree)
      return teTree(arrTree, TN_TYPE)
    }

    case "slam": {
      if (!expectedType) throw new NativeElabError("Cannot infer type of lambda expression")

      // Desugar multi-param
      if (sexpr.params.length > 1) {
        const [first, ...rest] = sexpr.params
        const innerLam: SExpr = { tag: "slam", params: rest, body: sexpr.body }
        const singleLam: SExpr = { tag: "slam", params: [first], body: innerLam }
        return elaborate(singleLam, env, expectedType, boundNames)
      }

      const param = sexpr.params[0]
      if (!isFork(expectedType)) throw new NativeElabError("Lambda needs function type")
      const A = expectedType.left
      const B = expectedType.right

      const m = freshMarker()
      const extEnv = new Map(env)
      extEnv.set(param, { tree: m, annTree: m, type: A })
      const extBound = new Set(boundNames ?? [])
      extBound.add(param)

      const bodyExpected = isNonDep(B) !== null ? isNonDep(B)! : safeApply(B, m)
      const body = elaborate(sexpr.body, extEnv, bodyExpected, extBound)

      return teLam(param, A, body, expectedType)
    }

    case "sapp": {
      const func = elaborate(sexpr.func, env, undefined, boundNames)

      if (!isFork(func.type)) throw new NativeElabError("Expected function type in application")
      const A = func.type.left
      const B = func.type.right

      const arg = elaborate(sexpr.arg, env, A, boundNames)

      if (!treeEqual(arg.type, A)) {
        // Constant-motive coercion
        if (treeEqual(arg.type, TN_TYPE) && isFork(A)) {
          const domCod = isNonDep(A.right)
          if (domCod !== null && treeEqual(domCod, TN_TYPE)) {
            // Wrap in K: the arg becomes K(arg) to match X -> Type domain
            const kArg = teCombK(arg, A)
            const resultType = isNonDep(B) !== null
              ? isNonDep(B)!
              : safeApply(B, fork(LEAF, typeTreeOf(arg))) // K(argTree)
            return teApp(func, kArg, resultType)
          }
        }

        const isOpaqueExpected = isStem(A) && !treeEqual(A, TN_TREE) && !treeEqual(A, TN_BOOL) && !treeEqual(A, TN_NAT)
        if (!isOpaqueExpected) {
          throw new NativeElabError(`Type mismatch: expected ${printNativeType(A)}, got ${printNativeType(arg.type)}`)
        }
      }

      // For type computation, we need the tree value of the argument
      // to evaluate dependent codomains: apply(B, argTree)
      const resultType = isNonDep(B) !== null
        ? isNonDep(B)!
        : safeApply(B, typeTreeOf(arg))

      return teApp(func, arg, resultType)
    }
  }
}

// Extract a Tree value from a TypedExpr for type-level computation.
// For tree nodes, return the value directly. For combinators, build the tree.
// For vars, they should have been substituted with markers during elaboration — use Expr fallback.
// For apps, eagerly evaluate. For lams, bracket-abstract via Expr and evaluate.
function typeTreeOf(te: TypedExpr): Tree {
  switch (te.tag) {
    case "tree": return te.value
    case "var":
      // During elaboration, vars are mapped to markers in the env.
      // typeTreeOf is called on elaboration results where vars reference markers.
      // If we reach here, the var wasn't resolved — shouldn't happen in well-formed programs.
      return LEAF
    case "app":
      return safeApply(typeTreeOf(te.func), typeTreeOf(te.arg))
    case "lam": {
      // Convert to Expr, bracket-abstract, and evaluate to get the tree function
      const bodyExpr = typedExprToExpr(te.body)
      const abstracted = bracketAbstract(te.param, bodyExpr)
      return collapseAndEval(abstracted)
    }
    case "combI": return I
    case "combK": return fork(LEAF, typeTreeOf(te.inner))
    case "combS": return fork(stem(typeTreeOf(te.c)), typeTreeOf(te.b))
  }
}

// Check if a TypedExpr contains a free variable reference
function teHasFreeVar(name: string, te: TypedExpr): boolean {
  switch (te.tag) {
    case "tree": return false
    case "var": return te.name === name
    case "app": return teHasFreeVar(name, te.func) || teHasFreeVar(name, te.arg)
    case "lam": return te.param !== name && teHasFreeVar(name, te.body)
    case "combI": return false
    case "combK": return teHasFreeVar(name, te.inner)
    case "combS": return teHasFreeVar(name, te.c) || teHasFreeVar(name, te.b)
  }
}

// Convert TypedExpr to Expr (for type-level bracket abstraction in Pi)
function typedExprToExpr(te: TypedExpr): Expr {
  switch (te.tag) {
    case "tree": return eTree(te.value)
    case "var": return eFvar(te.name)
    case "app": return eApp(typedExprToExpr(te.func), typedExprToExpr(te.arg))
    case "lam": return bracketAbstract(te.param, typedExprToExpr(te.body))
    case "combI": return eTree(I)
    case "combK": return kOf(typedExprToExpr(te.inner))
    case "combS": {
      // S(c, b) as Expr: app(app(LEAF, app(LEAF, c)), b)
      const cE = typedExprToExpr(te.c)
      const bE = typedExprToExpr(te.b)
      const stemC = eApp(eTree(LEAF), cE)
      const stemStemC = eApp(eTree(LEAF), stemC)
      return eApp(stemStemC, bE)
    }
  }
}

// ============================================================
// Phase 2: compile — TypedExpr → annotated Tree
// ============================================================

// Step 2a: bracket-abstract all lambdas, producing combinator nodes.
// Processes inside-out: innermost lambdas first.

function compileTypedExpr(te: TypedExpr): TypedExpr {
  switch (te.tag) {
    case "tree":
    case "var":
    case "combI":
      return te

    case "combK":
      return teCombK(compileTypedExpr(te.inner), te.type)

    case "combS":
      return teCombS(compileTypedExpr(te.c), compileTypedExpr(te.b), te.D, te.type)

    case "app":
      return teApp(compileTypedExpr(te.func), compileTypedExpr(te.arg), te.type)

    case "lam": {
      // First compile the body (handles inner lambdas)
      const compiledBody = compileTypedExpr(te.body)
      // Then bracket-abstract this lambda's parameter
      return typedBracketAbstract(te.param, te.domain, compiledBody, te.type)
    }
  }
}

// Typed bracket abstraction: eliminate a named variable, producing combI/combK/combS.
// Returns a TypedExpr with the given variable removed.
function typedBracketAbstract(
  name: string, domain: Tree, body: TypedExpr, piType: Tree
): TypedExpr {
  const B = piType.tag === "fork" ? piType.right : fork(LEAF, body.type) // codomain

  switch (body.tag) {
    case "var":
      if (body.name === name) {
        // [x]x = I
        return teCombI(piType)
      }
      // [x]y = K(y)
      return teCombK(body, piType)

    case "tree":
      // [x]c = K(c)
      return teCombK(body, piType)

    case "combI":
      // I has no free vars → K(I)
      return teCombK(body, piType)

    case "combK": {
      // K(e) = apply(stem(LEAF), e) — convert to app and delegate.
      // This lets the standard app-case handle eta, S(Kp)(Kq), etc.
      if (!teHasFreeVar(name, body.inner)) {
        return teCombK(body, piType)
      }
      const kTree = teTree(stem(LEAF), tnArrow(body.inner.type, body.type))
      const asApp = teApp(kTree, body.inner, body.type)
      return typedBracketAbstract(name, domain, asApp, piType)
    }

    case "combS": {
      // S(c, b) = fork(stem(c), b) = apply(apply(LEAF, apply(LEAF, c)), b)
      // Convert to app chain and delegate to the standard app-case.
      // This gets eta, S(Kp)(Kq), S(Kp)I optimizations for free.
      if (!teHasFreeVar(name, body.c) && !teHasFreeVar(name, body.b)) {
        return teCombK(body, piType)
      }
      // Build: app(app(LEAF, app(LEAF, c)), b)
      // Types are approximate (Tree-level construction) but sufficient for abstraction.
      const stemC = teApp(teTree(LEAF, TN_TREE), body.c, TN_TREE) // stem(c)
      const stemStemC = teApp(teTree(LEAF, TN_TREE), stemC, TN_TREE) // stem(stem(c))
      const asApp = teApp(stemStemC, body.b, body.type)
      return typedBracketAbstract(name, domain, asApp, piType)
    }

    case "app": {
      const freeInFunc = teHasFreeVar(name, body.func)
      const freeInArg = teHasFreeVar(name, body.arg)

      if (!freeInFunc && !freeInArg) {
        // [x](f g) where x not in f or g → K(f g)
        return teCombK(body, piType)
      }

      // Eta reduction: [x](f x) when x not free in f
      if (!freeInFunc && body.arg.tag === "var" && body.arg.name === name) {
        return body.func
      }

      // S case: [x](f g) = S([x]f, [x]g)
      // [x]g : Pi(domain, D) where D is the codomain of g's abstracted type
      // [x]f : Pi(domain, sCodomain(D, B))

      // Compute types for the abstracted sub-expressions
      const argType = body.arg.type
      const funcType = body.func.type

      // For [x]g: if g doesn't use x, type is Pi(domain, K(argType))
      //           if g uses x, we recurse and get the type from the result
      const absArg = typedBracketAbstract(name, domain, body.arg,
        fork(domain, freeInArg ? fork(LEAF, argType) : fork(LEAF, argType)))
      // D is the codomain of absArg's type
      const D = isFork(absArg.type) ? absArg.type.right : fork(LEAF, argType)

      // For [x]f: type is Pi(domain, sCodomain(D, B))
      const absFunc = typedBracketAbstract(name, domain, body.func,
        fork(domain, fork(LEAF, funcType))) // approximate

      // S(Kp)(Kq) = K(pq) optimization
      if (absFunc.tag === "combK" && absArg.tag === "combK") {
        // Both are constant — neither uses x. Wrap application in K.
        return teCombK(teApp(absFunc.inner, absArg.inner, body.type), piType)
      }

      // S(Kp) I = p optimization (eta)
      if (absFunc.tag === "combK" && absArg.tag === "combI") {
        // Verify type compatibility
        return absFunc.inner
      }

      return teCombS(absFunc, absArg, D, piType)
    }

    case "lam":
      // Should not happen — lams should be compiled before outer abstraction
      throw new NativeElabError("BUG: lam node during bracket abstraction")
  }
}

// Step 2b: collapse combinator TypedExpr to annotated tree.
// No lam or var nodes should remain.

function collapseAnnotated(te: TypedExpr, nativeDefs?: KnownDefs): Tree {
  switch (te.tag) {
    case "tree":
      return te.value
    case "combI":
      return I
    case "combK":
      return annK(collapseAnnotated(te.inner, nativeDefs))
    case "combS":
      return annS(te.D, collapseAnnotated(te.c, nativeDefs), collapseAnnotated(te.b, nativeDefs))
    case "app": {
      // Unevaluated application — result of eta reduction or S(Kp)(Kq)=K(pq).
      // Evaluate to get the tree value, wrap in ascription.
      const raw = collapseRaw(te)
      if (nativeDefs) nativeDefs.set(raw.id, te.type)
      return annAscribe(te.type, raw)
    }
    case "var":
      throw new NativeElabError("BUG: free variable in collapseAnnotated: " + te.name)
    case "lam":
      throw new NativeElabError("BUG: lam node in collapseAnnotated")
  }
}

// Collapse to raw tree (for execution)
function collapseRaw(te: TypedExpr): Tree {
  switch (te.tag) {
    case "tree": return te.value
    case "combI": return I
    case "combK": return fork(LEAF, collapseRaw(te.inner))
    case "combS": return fork(stem(collapseRaw(te.c)), collapseRaw(te.b))
    case "app": return safeApply(collapseRaw(te.func), collapseRaw(te.arg))
    case "var": throw new NativeElabError("BUG: free variable in collapseRaw: " + te.name)
    case "lam": throw new NativeElabError("BUG: lam node in collapseRaw")
  }
}

// Full compile: TypedExpr → { annotated, compiled }
// annotated = structural annotation for checking (D at S nodes, K/I pass through)
// compiled = fully evaluated tree for execution (collapseRaw eagerly evaluates app nodes)
export function compileTE(te: TypedExpr, nativeDefs?: KnownDefs): { annotated: Tree, compiled: Tree } {
  const combinators = compileTypedExpr(te)
  const annotated = collapseAnnotated(combinators, nativeDefs)
  const compiled = collapseRaw(combinators)
  return { annotated, compiled }
}

// ============================================================
// Backward-compatible buildDirect wrapper
// ============================================================

// Returns { tree, type } for callers that need type-level tree values.
// Used by prelude (builtin type elaboration) and REPL (type display).
export function buildDirect(
  sexpr: SExpr, env: NativeEnv, expectedType?: Tree,
  boundNames?: Set<string>,
): { tree: Tree, type: Tree } {
  const te = elaborate(sexpr, env, expectedType, boundNames)
  return { tree: typeTreeOf(te), type: te.type }
}

// ============================================================
// Native declaration pipeline
// ============================================================

export function nativeElabDecl(
  env: NativeEnv,
  compiledDefs: Map<string, Tree>,
  nativeDefs: KnownDefs,
  name: string,
  type: SExpr | null,
  value: SExpr,
  isRec: boolean,
): { env: NativeEnv, nativeType: Tree, compiled: Tree, annotated: Tree, checkOk: boolean } {
  // 1. Elaborate type annotation
  let nativeType: Tree
  if (type) {
    const te = elaborate(type, env, TN_TYPE)
    if (!treeEqual(te.type, TN_TYPE)) throw new NativeElabError("Type annotation must be a type")
    nativeType = typeTreeOf(te)
  } else {
    if (isRec) throw new NativeElabError(`Recursive definition '${name}' requires a type annotation`)
    throw new NativeElabError("Type inference not yet supported in native elaborator")
  }

  if (isRec) {
    // Recursive definitions still use the old compile path for FIX.
    // The TypedExpr pipeline handles the step function; FIX wraps it.
    const selfMarker = freshMarker()
    const extEnv = new Map(env)
    extEnv.set(name, { tree: selfMarker, annTree: selfMarker, type: nativeType })

    const bodyTE = elaborate(value, extEnv, nativeType)

    // Abstract self out: compile the body, then bracket-abstract name
    const compiledBody = compileTypedExpr(bodyTE)
    const selfFn = typedBracketAbstract(name, nativeType, compiledBody,
      tnArrow(nativeType, nativeType))
    const selfFnTree = collapseRaw(selfFn)

    // Use compileRecAndEval for the actual FIX-wrapped compiled tree
    const compiled = compileRecAndEval(name, value, compiledDefs)
    compiledDefs.set(name, compiled)

    nativeDefs.set(compiled.id, nativeType)
    registerNativeBuiltinId(compiled.id, compiled)

    // Verify step function at T → T
    const selfFnType = tnArrow(nativeType, nativeType)
    nativeDefs.set(selfFnTree.id, selfFnType)
    const annSelfFn = annAscribe(selfFnType, selfFnTree)
    const checkOk = checkAnnotated(nativeDefs, annSelfFn, selfFnType)

    const annotated = annAscribe(nativeType, compiled)
    const newEnv = new Map(env)
    newEnv.set(name, { tree: compiled, annTree: annotated, type: nativeType })

    return { env: newEnv, nativeType, compiled, annotated, checkOk }
  }

  // Non-recursive: use the new TypedExpr pipeline
  const bodyTE = elaborate(value, env, nativeType)
  const { annotated, compiled } = compileTE(bodyTE, nativeDefs)

  // Register the compiled tree (from extract(annotated)) as the canonical form
  compiledDefs.set(name, compiled)
  nativeDefs.set(compiled.id, nativeType)

  // Check the structurally annotated tree
  const checkOk = checkAnnotated(nativeDefs, annotated, nativeType)

  const treeForEnv = treeEqual(nativeType, TN_TYPE) ? typeTreeOf(bodyTE) : compiled

  const newEnv = new Map(env)
  newEnv.set(name, { tree: treeForEnv, annTree: annotated, type: nativeType })

  return { env: newEnv, nativeType, compiled, annotated, checkOk }
}
