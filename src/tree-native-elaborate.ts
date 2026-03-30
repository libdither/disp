// Tree-native elaboration adapter.
//
// Bridges the existing CoC elaborator with the tree-native checker:
// 1. convertCocType: CoC-encoded type → tree-native type
// 2. annotateTree: raw compiled tree + native type → annotated tree (D at S nodes)
// 3. nativeCheckDecl: full declaration pipeline

import { type Tree, LEAF, stem, fork, apply, treeEqual, I, K, isLeaf, isStem, isFork } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapse, collapseAndEval, compileAndEval, compileRecAndEval } from "./compile.js"
import {
  termTag, unPi, unVar, unLam, unApp,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
  whnfTree, evalToNative, encVar, freshMarker,
  cocCheckDecl, registerNativeBuiltinId, type Env,
} from "./coc.js"
import { type SExpr } from "./parse.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow, tnPi,
  annK, annS, annTriage, annAscribe, extract,
  isNonDep, stemCodomain, forkCodomain, sCodomain,
  type KnownDefs, checkAnnotated,
} from "./tree-native-checker.js"

// ============================================================
// CoC type → tree-native type conversion
// ============================================================

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

function treeToExprReplacingBounded(t: Tree, target: Tree, varName: string, depth: number): Expr | null {
  if (depth > 500) return null
  if (treeEqual(t, target)) return eFvar(varName)
  // Optimization: if subtree doesn't contain target, return as constant (no recursion needed)
  if (!treeContains(t, target)) return eTree(t)
  if (isLeaf(t)) return eTree(t)
  if (isStem(t)) {
    const child = treeToExprReplacingBounded(t.child, target, varName, depth + 1)
    if (child === null) return null
    if (child.tag === "tree" && treeEqual(child.value, t.child)) return eTree(t)
    return eApp(eTree(LEAF), child)
  }
  const left = treeToExprReplacingBounded(t.left, target, varName, depth + 1)
  if (left === null) return null
  const right = treeToExprReplacingBounded(t.right, target, varName, depth + 1)
  if (right === null) return null
  if (left.tag === "tree" && treeEqual(left.value, t.left) &&
      right.tag === "tree" && treeEqual(right.value, t.right))
    return eTree(t)
  return eApp(eApp(eTree(LEAF), left), right)
}

function abstractTreeMarker(tree: Tree, marker: Tree): Tree {
  const name = "__tn_marker__"
  const expr = treeToExprReplacingBounded(tree, marker, name, 0)
  if (expr === null) {
    // Tree too deep — fall back to using treeToExprReplacing from coc.ts
    // but protect against stack overflow
    return fork(LEAF, tree) // K(tree) as conservative fallback
  }
  return collapse(bracketAbstract(name, expr))
}

function convertPiBody(body: Tree): Tree {
  const m = freshMarker()
  const neutral = encVar(m)
  const applied = apply(body, neutral)
  const converted = convertCocType(applied)
  if (treeContains(converted, m)) {
    return abstractTreeMarker(converted, m)
  }
  return fork(LEAF, converted) // K(converted) — non-dependent
}

export function convertCocType(t: Tree): Tree {
  // Known type constants
  if (treeEqual(t, TREE_TYPE)) return TN_TREE
  if (treeEqual(t, BOOL_TYPE)) return TN_BOOL
  if (treeEqual(t, NAT_TYPE)) return TN_NAT

  let w: Tree
  try { w = whnfTree(t) } catch { w = t }

  if (treeEqual(w, TREE_TYPE)) return TN_TREE
  if (treeEqual(w, BOOL_TYPE)) return TN_BOOL
  if (treeEqual(w, NAT_TYPE)) return TN_NAT

  const tag = termTag(w)
  switch (tag) {
    case "type": return TN_TYPE
    case "var": return unVar(w)!
    case "pi": {
      const p = unPi(w)!
      const domain = convertCocType(p.domain)
      const body = convertPiBody(p.body)
      return fork(domain, body)
    }
    case "app": {
      const native = evalToNative(w)
      if (native !== null && !treeEqual(native, w)) {
        return convertCocType(native)
      }
      return TN_TREE // stuck application → fallback
    }
    case "lam": {
      const l = unLam(w)!
      const m = freshMarker()
      const neutral = encVar(m)
      const applied = apply(l.body, neutral)
      const converted = convertCocType(applied)
      return abstractTreeMarker(converted, m)
    }
    default:
      return TN_TREE
  }
}

// ============================================================
// Annotation inference: raw tree + type → annotated tree
// ============================================================

function inferGroundType(v: Tree, defs: KnownDefs): Tree {
  const known = defs.get(v.id)
  if (known) return known
  if (isLeaf(v)) return TN_NAT
  if (isStem(v)) {
    let t: Tree = v
    while (isStem(t)) t = t.child
    if (isLeaf(t)) return TN_NAT
    return TN_TREE
  }
  return TN_TREE
}

function inferIntermediateType(b: Tree, A: Tree, defs: KnownDefs): Tree {
  // I = identity → D = K(A)
  if (treeEqual(b, I)) return fork(LEAF, A)

  // K(v) = fork(leaf, v) → D = K(typeOf(v))
  if (isFork(b) && isLeaf(b.left)) {
    const vType = inferGroundType(b.right, defs)
    return fork(LEAF, vType)
  }

  // leaf (stem constructor)
  if (isLeaf(b)) {
    if (treeEqual(A, TN_NAT)) return fork(LEAF, TN_NAT)
    if (treeEqual(A, TN_BOOL)) return fork(LEAF, TN_NAT)
    return fork(LEAF, TN_TREE)
  }

  // stem(a) (fork constructor)
  if (isStem(b)) return fork(LEAF, TN_TREE)

  // Known definition with Pi type
  const known = defs.get(b.id)
  if (known && isFork(known)) return known.right

  // Default: D = K(Tree)
  return fork(LEAF, TN_TREE)
}

export function annotateTree(raw: Tree, type: Tree, defs: KnownDefs): Tree {
  // Known definition at matching type — no annotation needed.
  // If the type doesn't match, fall through to structural annotation
  // (the def may be polymorphic, e.g., I : Tree -> Tree used at Nat -> Nat).
  const knownType = defs.get(raw.id)
  if (knownType !== undefined && treeEqual(knownType, type)) return raw

  // Base type value
  if (isStem(type) || treeEqual(type, TN_TYPE)) return raw

  // Must be Pi type (fork)
  if (!isFork(type)) return raw

  const A = type.left
  const B = type.right

  // Leaf as function
  if (isLeaf(raw)) return LEAF

  // Stem as function
  if (isStem(raw)) return raw

  // K(v): fork(leaf, v)
  if (isLeaf(raw.left)) {
    const v = raw.right
    const B0 = isNonDep(B)
    const vType = B0 !== null ? B0 : apply(B, LEAF)
    return annK(annotateTree(v, vType, defs))
  }

  // S(c, b): fork(stem(c), b)
  if (isStem(raw.left)) {
    const c = raw.left.child
    const b = raw.right

    let D = inferIntermediateType(b, A, defs)
    const B0 = isNonDep(B)

    // If D inference from b gave K(TN_TREE) fallback, try inferring from c's known type.
    // S(c,b)(x) = c(x)(b(x)), so c : A -> D -> B. If c is known, extract D from c's type.
    const D0_from_b = isNonDep(D)
    if (D0_from_b !== null && treeEqual(D0_from_b, TN_TREE) && B0 !== null) {
      const cKnown = defs.get(c.id)
      if (cKnown && isFork(cKnown)) {
        // c : fork(A', cCod). cCod should encode A -> D -> B0.
        // After one application, c(x) : D -> B0, so cCod applied should give fork(D0, K(B0))
        const cCod = cKnown.right
        const cCod0 = isNonDep(cCod)
        if (cCod0 !== null && isFork(cCod0)) {
          // c(x) : cCod0 = fork(D0_real, _)
          D = fork(LEAF, cCod0.left) // K(D0_real)
        }
      }
    }

    const D0 = isNonDep(D)

    const bType = fork(A, D)
    const cType = D0 !== null && B0 !== null
      ? tnArrow(A, tnArrow(D0, B0))
      : fork(A, sCodomain(D, B))

    return annS(D, annotateTree(c, cType, defs), annotateTree(b, bType, defs))
  }

  // Triage(onL, onS, onF): fork(fork(onL, onS), onF)
  if (!isFork(raw.left)) return raw // shouldn't happen
  const onL = raw.left.left
  const onS = raw.left.right
  const onF = raw.right
  const B0 = isNonDep(B)

  if (treeEqual(A, TN_TREE)) {
    const leafType = B0 !== null ? B0 : apply(B, LEAF)
    const stemB = B0 !== null ? fork(LEAF, B0) : stemCodomain(B)
    const forkB = B0 !== null ? fork(LEAF, tnArrow(TN_TREE, B0)) : forkCodomain(B)
    return annTriage(
      annotateTree(onL, leafType, defs),
      annotateTree(onS, fork(TN_TREE, stemB), defs),
      annotateTree(onF, fork(TN_TREE, forkB), defs)
    )
  }

  if (treeEqual(A, TN_NAT)) {
    const leafType = B0 !== null ? B0 : apply(B, LEAF)
    const stemB = B0 !== null ? fork(LEAF, B0) : stemCodomain(B)
    return annTriage(
      annotateTree(onL, leafType, defs),
      annotateTree(onS, fork(TN_NAT, stemB), defs),
      annotateTree(onF, raw.right, defs) // fork case vacuous for Nat
    )
  }

  if (treeEqual(A, TN_BOOL)) {
    const leafType = B0 !== null ? B0 : apply(B, LEAF)
    const stemB = B0 !== null ? fork(LEAF, B0) : stemCodomain(B)
    return annTriage(
      annotateTree(onL, leafType, defs),
      annotateTree(onS, fork(TN_TREE, stemB), defs),
      annotateTree(onF, raw.right, defs)
    )
  }

  // Default: annotate branches conservatively
  const leafType = B0 !== null ? B0 : TN_TREE
  const stemType = B0 !== null ? tnArrow(TN_TREE, B0) : tnArrow(TN_TREE, TN_TREE)
  const forkType = B0 !== null ? tnArrow(TN_TREE, tnArrow(TN_TREE, B0)) : tnArrow(TN_TREE, tnArrow(TN_TREE, TN_TREE))
  return annTriage(
    annotateTree(onL, leafType, defs),
    annotateTree(onS, stemType, defs),
    annotateTree(onF, forkType, defs)
  )
}

// ============================================================
// Native declaration pipeline
// ============================================================

export function nativeCheckDecl(
  cocEnv: Env,
  compiledDefs: Map<string, Tree>,
  nativeDefs: KnownDefs,
  name: string,
  type: SExpr | null,
  value: SExpr,
  isRec: boolean,
): { cocResult: { env: Env, type: Tree }, nativeType: Tree, annotated: Tree, nativeDefs: KnownDefs, checkOk: boolean } {
  // 1. Type-check with CoC elaborator
  const cocResult = cocCheckDecl(cocEnv, name, type, value, isRec)

  // 2. Compile to raw tree
  const raw = isRec
    ? compileRecAndEval(name, value, compiledDefs)
    : compileAndEval(value, compiledDefs)

  // 3. Convert type
  const nativeType = convertCocType(cocResult.type)

  // 4. Annotate
  const annotated = annotateTree(raw, nativeType, nativeDefs)

  // 5. Verify
  const checkOk = checkAnnotated(nativeDefs, annotated, nativeType)

  // 6. Register
  const newDefs = new Map(nativeDefs)
  newDefs.set(raw.id, nativeType)

  return { cocResult, nativeType, annotated, nativeDefs: newDefs, checkOk }
}

// ============================================================
// Pretty-print a tree-native type
// ============================================================

// Unique placeholder tree for binder names in dependent Pi printing
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

  // Name lookup (binder names in dependent types, user-defined type names)
  if (nameMap) {
    const name = nameMap.get(type.id)
    if (name) return name
  }

  if (isFork(type)) {
    const A = type.left
    const B = type.right
    // Non-dependent arrow: fork(A, fork(LEAF, B)) = A -> B
    const nd = isNonDep(B)
    if (nd !== null) {
      const domStr = isFork(A) ? `(${printNativeType(A, nameMap, depth)})` : printNativeType(A, nameMap, depth)
      return `${domStr} -> ${printNativeType(nd, nameMap, depth)}`
    }
    // Dependent Pi: fork(A, B) where B is not K(_)
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
// Phase 3: Native Elaborator
// ============================================================

// --- TypedExpr: expressions with types attached ---

export type TypedExpr =
  | { tag: "tlit", tree: Tree, annTree: Tree, type: Tree }
  | { tag: "tfvar", name: string, type: Tree }
  | { tag: "tapp", func: TypedExpr, arg: TypedExpr, type: Tree }
  | { tag: "tI", type: Tree }
  | { tag: "tK", value: TypedExpr, type: Tree }
  | { tag: "tS", d: Tree, c: TypedExpr, b: TypedExpr, type: Tree }

export type NativeEnv = Map<string, { tree: Tree, annTree: Tree, type: Tree }>

// --- Collapse TypedExpr to executable tree ---

export function collapseTypedExpr(texpr: TypedExpr): Tree {
  switch (texpr.tag) {
    case "tlit": return texpr.tree
    case "tfvar": throw new Error(`Free variable in collapseTypedExpr: ${texpr.name}`)
    case "tapp": return apply(collapseTypedExpr(texpr.func), collapseTypedExpr(texpr.arg))
    case "tI": return I
    case "tK": return fork(LEAF, collapseTypedExpr(texpr.value))
    case "tS": {
      const c = collapseTypedExpr(texpr.c)
      const b = collapseTypedExpr(texpr.b)
      return fork(stem(c), b)
    }
  }
}

// --- Collapse TypedExpr to annotated tree (with D annotations + ascriptions) ---

export function collapseToAnnotated(texpr: TypedExpr, defs?: KnownDefs): Tree {
  switch (texpr.tag) {
    case "tlit": return texpr.annTree
    case "tfvar": throw new Error(`Free variable leak: ${texpr.name}`)
    case "tapp": {
      const f = extract(collapseToAnnotated(texpr.func, defs))
      const g = extract(collapseToAnnotated(texpr.arg, defs))
      const result = apply(f, g)
      if (defs) defs.set(result.id, texpr.type)
      return annAscribe(texpr.type, result)
    }
    case "tI": return I
    case "tK": return annK(collapseToAnnotated(texpr.value, defs))
    case "tS": return annS(texpr.d, collapseToAnnotated(texpr.c, defs), collapseToAnnotated(texpr.b, defs))
  }
}

// --- Collapse TypedExpr for type computation (resolves tfvar via env markers) ---

function collapseForType(texpr: TypedExpr, env: NativeEnv): Tree {
  if (texpr.tag === "tfvar") {
    const entry = env.get(texpr.name)
    if (entry) return entry.tree
    throw new Error(`Free variable in collapseForType: ${texpr.name}`)
  }
  if (texpr.tag === "tapp") {
    return apply(collapseForType(texpr.func, env), collapseForType(texpr.arg, env))
  }
  return collapseTypedExpr(texpr)
}

// --- Check if a free variable appears in a TypedExpr ---

// Check if a TypedExpr contains a tlit whose tree matches the given marker.
// Used to detect dependency in Pi types when eager evaluation erases the marker
// from the collapsed tree (e.g., Pair A B where A is a marker).
function typedContainsMarker(texpr: TypedExpr, marker: Tree): boolean {
  switch (texpr.tag) {
    case "tlit": return treeEqual(texpr.tree, marker)
    case "tfvar": return false
    case "tapp": return typedContainsMarker(texpr.func, marker) || typedContainsMarker(texpr.arg, marker)
    case "tI": return false
    case "tK": return typedContainsMarker(texpr.value, marker)
    case "tS": return typedContainsMarker(texpr.c, marker) || typedContainsMarker(texpr.b, marker)
  }
}

function typedHasFreeVar(name: string, texpr: TypedExpr): boolean {
  switch (texpr.tag) {
    case "tlit": return false
    case "tfvar": return texpr.name === name
    case "tapp": return typedHasFreeVar(name, texpr.func) || typedHasFreeVar(name, texpr.arg)
    case "tI": return false
    case "tK": return typedHasFreeVar(name, texpr.value)
    case "tS": return typedHasFreeVar(name, texpr.c) || typedHasFreeVar(name, texpr.b)
  }
}

// --- Convert TypedExpr to Expr (for bracket abstraction) ---

function typedToExpr(texpr: TypedExpr): Expr {
  switch (texpr.tag) {
    case "tlit": return eTree(texpr.tree)
    case "tfvar": return eFvar(texpr.name)
    case "tapp": return eApp(typedToExpr(texpr.func), typedToExpr(texpr.arg))
    case "tI": return eTree(I)
    case "tK": return eApp(eTree(LEAF), typedToExpr(texpr.value))
    case "tS": return eApp(eApp(eTree(LEAF), typedToExpr(texpr.c)), typedToExpr(texpr.b))
  }
}

// Convert TypedExpr to Expr, replacing tlit nodes matching a marker with a free variable.
// Used to build codomain functions for dependent Pi types where eager evaluation erases the marker.
function typedToExprReplacingMarker(texpr: TypedExpr, marker: Tree, varName: string): Expr {
  switch (texpr.tag) {
    case "tlit":
      if (treeEqual(texpr.tree, marker)) return eFvar(varName)
      return eTree(texpr.tree)
    case "tfvar": return eFvar(texpr.name)
    case "tapp":
      return eApp(
        typedToExprReplacingMarker(texpr.func, marker, varName),
        typedToExprReplacingMarker(texpr.arg, marker, varName),
      )
    case "tI": return eTree(I)
    case "tK": return eApp(eTree(LEAF), typedToExprReplacingMarker(texpr.value, marker, varName))
    case "tS":
      return eApp(
        eApp(eTree(LEAF), typedToExprReplacingMarker(texpr.c, marker, varName)),
        typedToExprReplacingMarker(texpr.b, marker, varName),
      )
  }
}

// --- Typed bracket abstraction ---

export function typedBracketAbstract(
  varName: string, texpr: TypedExpr, varType: Tree,
): { result: TypedExpr, codomain: Tree } {
  const A = varType

  // [x] x = I, codomain = K(A)
  if (texpr.tag === "tfvar" && texpr.name === varName) {
    const cod = fork(LEAF, A) // K(A)
    return { result: { tag: "tI", type: fork(A, cod) }, codomain: cod }
  }

  // [x] c = K(c), codomain = K(type_of_c)
  if (!typedHasFreeVar(varName, texpr)) {
    const cod = fork(LEAF, texpr.type) // K(T)
    return { result: { tag: "tK", value: texpr, type: fork(A, cod) }, codomain: cod }
  }

  // tI/tK/tS with free vars: decompose to tapp form.
  // During Phase 3-4 transition, tK/tS from inner abstractions may appear as inputs
  // to outer abstractions (via multi-param lambdas).
  if (texpr.tag === "tK") {
    // tK(v) ≡ tapp(K_combinator, v). K = stem(LEAF).
    // We know v has the free var. Treat as tapp with a constant func.
    const kType = fork(texpr.value.type, fork(LEAF, fork(A, fork(LEAF, texpr.value.type))))
    const kLit: TypedExpr = { tag: "tlit", tree: K, annTree: K, type: kType }
    const asApp: TypedExpr = { tag: "tapp", func: kLit, arg: texpr.value, type: texpr.type }
    return typedBracketAbstract(varName, asApp, A)
  }
  if (texpr.tag === "tS") {
    // tS(d, c, b) ≡ tapp(tapp(stem_fn, c), b). stem = LEAF as constructor.
    // Decompose: fork(stem(c), b) = apply(stem(c), b) = apply(apply(LEAF, c), b)
    const innerType = fork(texpr.b.type, fork(LEAF, texpr.type))
    const stemApp: TypedExpr = { tag: "tapp", func: { tag: "tlit", tree: LEAF, annTree: LEAF, type: fork(texpr.c.type, fork(LEAF, innerType)) }, arg: texpr.c, type: innerType }
    const asApp: TypedExpr = { tag: "tapp", func: stemApp, arg: texpr.b, type: texpr.type }
    return typedBracketAbstract(varName, asApp, A)
  }

  // [x] (f g): must be tapp (tlit has no free vars, tfvar was handled above, tI has none)
  if (texpr.tag !== "tapp") {
    const cod = fork(LEAF, texpr.type)
    return { result: { tag: "tK", value: texpr, type: fork(A, cod) }, codomain: cod }
  }

  // Eta reduction: [x](f x) = f when x not free in f
  if (!typedHasFreeVar(varName, texpr.func) && texpr.arg.tag === "tfvar" && texpr.arg.name === varName) {
    if (isFork(texpr.func.type)) {
      return { result: texpr.func, codomain: texpr.func.type.right }
    }
    return { result: texpr.func, codomain: fork(LEAF, texpr.type) }
  }

  // General S case: [x](f g) = S([x]f, [x]g)
  const { result: cResult, codomain: cCod } = typedBracketAbstract(varName, texpr.func, A)
  const { result: bResult, codomain: D } = typedBracketAbstract(varName, texpr.arg, A)

  // S(K(p), I) = p (eta reduction)
  if (cResult.tag === "tK" && bResult.tag === "tI") {
    // p = cResult.value. S(K(p), I)(x) = p(x), so codomain = p.type.right
    const vType = cResult.value.type
    const cod = isFork(vType) ? vType.right : fork(LEAF, texpr.type)
    return { result: cResult.value, codomain: cod }
  }

  // Full S: S(c, b) with D as intermediate type
  const B = fork(LEAF, texpr.type) // K(resultType) — non-dependent approximation
  return {
    result: { tag: "tS", d: D, c: cResult, b: bResult, type: fork(A, sCodomain(D, B)) },
    codomain: sCodomain(D, B),
  }
}

// --- Build native wrapped (elaborate SExpr to TypedExpr + type) ---

export class NativeElabError extends Error {
  constructor(msg: string) { super(msg); this.name = "NativeElabError" }
}

export function buildNativeWrapped(
  sexpr: SExpr, env: NativeEnv, expectedType?: Tree,
  boundNames?: Set<string>,
): { texpr: TypedExpr, type: Tree } {
  switch (sexpr.tag) {
    case "stype":
      return { texpr: { tag: "tlit", tree: LEAF, annTree: LEAF, type: TN_TYPE }, type: TN_TYPE }

    case "stree":
      return { texpr: { tag: "tlit", tree: TN_TREE, annTree: TN_TREE, type: TN_TYPE }, type: TN_TYPE }

    case "svar": {
      // Special-case primitive type names
      if (sexpr.name === "Tree") return { texpr: { tag: "tlit", tree: TN_TREE, annTree: TN_TREE, type: TN_TYPE }, type: TN_TYPE }
      if (sexpr.name === "Bool") return { texpr: { tag: "tlit", tree: TN_BOOL, annTree: TN_BOOL, type: TN_TYPE }, type: TN_TYPE }
      if (sexpr.name === "Nat") return { texpr: { tag: "tlit", tree: TN_NAT, annTree: TN_NAT, type: TN_TYPE }, type: TN_TYPE }

      const entry = env.get(sexpr.name)
      if (!entry) throw new NativeElabError(`Unbound variable: ${sexpr.name}`)

      // Bound variables produce tfvar (preserves structure for bracket abstraction)
      if (boundNames?.has(sexpr.name)) {
        return { texpr: { tag: "tfvar", name: sexpr.name, type: entry.type }, type: entry.type }
      }
      return { texpr: { tag: "tlit", tree: entry.tree, annTree: entry.annTree, type: entry.type }, type: entry.type }
    }

    case "spi": {
      // Elaborate domain (expect Type)
      const { texpr: domTexpr, type: domType } = buildNativeWrapped(sexpr.domain, env, TN_TYPE, boundNames)
      if (!treeEqual(domType, TN_TYPE)) throw new NativeElabError("Pi domain must be a type")
      const domTree = collapseForType(domTexpr, env)

      // Create marker for binder (type-level computation, goes in env only)
      const m = freshMarker()
      const extEnv = new Map(env)
      if (sexpr.name !== "_") extEnv.set(sexpr.name, { tree: m, annTree: m, type: domTree })

      // Elaborate codomain
      const { texpr: codTexpr, type: codType } = buildNativeWrapped(sexpr.codomain, extEnv, TN_TYPE, boundNames)
      if (!treeEqual(codType, TN_TYPE)) throw new NativeElabError("Pi codomain must be a type")

      // Check if codomain depends on the binder.
      // First try collapsing (eager) and checking for marker in result tree.
      const codTree = collapseForType(codTexpr, extEnv)
      if (treeContains(codTree, m)) {
        // Dependent: abstract marker out to get codomain function
        const codFn = abstractTreeMarker(codTree, m)
        const piTree = fork(domTree, codFn)
        return { texpr: { tag: "tlit", tree: piTree, annTree: piTree, type: TN_TYPE }, type: TN_TYPE }
      }

      // Eager evaluation may have consumed the marker (e.g., Church-encoded type
      // constructors like Pair applied to type variables, or dependent types like
      // P applied to values). Check the TypedExpr structure for the marker.
      if (sexpr.name !== "_" && typedContainsMarker(codTexpr, m)) {
        // Dependent, but marker was erased by evaluation.
        // Build codomain function via bracket abstraction on the TypedExpr.
        const varName = "__pi_binder__"
        const codExpr = typedToExprReplacingMarker(codTexpr, m, varName)
        const codFn = collapseAndEval(bracketAbstract(varName, codExpr))
        const piTree = fork(domTree, codFn)
        return { texpr: { tag: "tlit", tree: piTree, annTree: piTree, type: TN_TYPE }, type: TN_TYPE }
      }

      // Non-dependent: A -> B = fork(A, K(B))
      const arrTree = tnArrow(domTree, codTree)
      return { texpr: { tag: "tlit", tree: arrTree, annTree: arrTree, type: TN_TYPE }, type: TN_TYPE }
    }

    case "slam": {
      if (!expectedType) throw new NativeElabError("Cannot infer type of lambda expression")

      // Desugar multi-param lambda
      if (sexpr.params.length > 1) {
        const [first, ...rest] = sexpr.params
        const innerLam: SExpr = { tag: "slam", params: rest, body: sexpr.body }
        const singleLam: SExpr = { tag: "slam", params: [first], body: innerLam }
        return buildNativeWrapped(singleLam, env, expectedType, boundNames)
      }

      const param = sexpr.params[0]
      // Decompose expected type as Pi(A, B)
      if (!isFork(expectedType)) throw new NativeElabError("Lambda needs function type")
      const A = expectedType.left
      const B = expectedType.right

      // Create a fresh marker for dependent type computation (goes in env)
      // and add param to boundNames (so svar produces tfvar)
      const m = freshMarker()
      const extEnv = new Map(env)
      extEnv.set(param, { tree: m, annTree: m, type: A })
      const extBound = new Set(boundNames ?? [])
      extBound.add(param)

      // Compute expected body type
      const bodyExpected = isNonDep(B) !== null ? isNonDep(B)! : apply(B, m)

      // Elaborate body — bound vars become tfvar, defs become tlit
      const { texpr: bodyTexpr } = buildNativeWrapped(sexpr.body, extEnv, bodyExpected, extBound)

      // Bracket-abstract the parameter out (no substituteMarker needed)
      const { result } = typedBracketAbstract(param, bodyTexpr, A)

      // Adjust the result type to match the expected Pi type
      const adjusted = { ...result, type: expectedType }
      return { texpr: adjusted, type: expectedType }
    }

    case "sapp": {
      // Elaborate function
      const { texpr: funcTexpr, type: funcType } = buildNativeWrapped(sexpr.func, env, undefined, boundNames)

      // Function type must be Pi (fork)
      if (!isFork(funcType)) throw new NativeElabError("Expected function type in application")
      const A = funcType.left
      const B = funcType.right

      // Elaborate argument against domain
      const { texpr: argTexpr, type: argType } = buildNativeWrapped(sexpr.arg, env, A, boundNames)

      // Type check: arg type must match domain.
      if (!treeEqual(argType, A)) {
        // Constant-motive coercion: when domain is X -> Type and arg is Type,
        // wrap the argument in K to produce a constant motive function.
        // This handles patterns like natElim R z s n where R : Type but
        // natElim expects (Nat -> Type) as the motive.
        if (treeEqual(argType, TN_TYPE) && isFork(A)) {
          const domCod = isNonDep(A.right)
          if (domCod !== null && treeEqual(domCod, TN_TYPE)) {
            // Domain is X -> Type, arg is Type → wrap in K
            const kWrapped: TypedExpr = { tag: "tK", value: argTexpr, type: A }
            const resultType = isNonDep(B) !== null
              ? isNonDep(B)!
              : apply(B, collapseForType(kWrapped, env))
            return {
              texpr: { tag: "tapp", func: funcTexpr, arg: kWrapped, type: resultType },
              type: resultType,
            }
          }
        }

        // Allow mismatches when the expected domain is opaque (produced by unevaluated
        // dependent type applications like P true, P b, etc.). These are stems that
        // aren't recognized type constants.
        const isOpaqueExpected = isStem(A) && !treeEqual(A, TN_TREE) && !treeEqual(A, TN_BOOL) && !treeEqual(A, TN_NAT)
        if (!isOpaqueExpected) {
          throw new NativeElabError(`Type mismatch: expected ${printNativeType(A)}, got ${printNativeType(argType)}`)
        }
        // Opaque expected type — allow the argument and use it as-is
      }

      // Result type: apply B to the argument value (use collapseForType for bound vars)
      const resultType = isNonDep(B) !== null
        ? isNonDep(B)!
        : apply(B, collapseForType(argTexpr, env))

      return {
        texpr: { tag: "tapp", func: funcTexpr, arg: argTexpr, type: resultType },
        type: resultType,
      }
    }
  }
}

// --- Native declaration pipeline (standalone, replaces CoC) ---

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
    const { texpr: typeTexpr, type: typeOfType } = buildNativeWrapped(type, env, TN_TYPE)
    if (!treeEqual(typeOfType, TN_TYPE)) throw new NativeElabError("Type annotation must be a type")
    nativeType = collapseTypedExpr(typeTexpr)
  } else {
    if (isRec) throw new NativeElabError(`Recursive definition '${name}' requires a type annotation`)
    throw new NativeElabError("Type inference not yet supported in native elaborator")
  }

  if (isRec) {
    // Recursive: add self as bound variable, elaborate body, abstract self out
    const selfMarker = freshMarker()
    const extEnv = new Map(env)
    extEnv.set(name, { tree: selfMarker, annTree: selfMarker, type: nativeType })
    const boundNames = new Set([name])

    const { texpr: bodyTexpr } = buildNativeWrapped(value, extEnv, nativeType, boundNames)

    // Abstract self out to get a T → T function
    const { result: selfAbstracted } = typedBracketAbstract(name, bodyTexpr, nativeType)
    const annSelfFn = collapseToAnnotated(selfAbstracted, nativeDefs)

    // Compile using existing compileRecAndEval (trusted backend)
    const compiled = compileRecAndEval(name, value, compiledDefs)
    compiledDefs.set(name, compiled)

    // Register before checking — ascription verification needs the compiled tree in defs
    nativeDefs.set(compiled.id, nativeType)
    registerNativeBuiltinId(compiled.id, compiled)

    // Verify the self-abstracted function at T → T
    const selfFnType = tnArrow(nativeType, nativeType)
    const checkOk = checkAnnotated(nativeDefs, annSelfFn, selfFnType)

    // FIX results are opaque — store as ascription
    const annotated = annAscribe(nativeType, compiled)
    const newEnv = new Map(env)
    newEnv.set(name, { tree: compiled, annTree: annotated, type: nativeType })

    return { env: newEnv, nativeType, compiled, annotated, checkOk }
  }

  // Non-recursive: elaborate value with expected type
  const { texpr: valueTexpr } = buildNativeWrapped(value, env, nativeType)

  // Produce annotated tree from elaboration (exact D annotations)
  const annotated = collapseToAnnotated(valueTexpr, nativeDefs)

  // Compile using existing compileAndEval (trusted backend)
  const compiled = compileAndEval(value, compiledDefs)
  compiledDefs.set(name, compiled)

  // Register before checking — ascription verification needs the compiled tree in defs
  nativeDefs.set(compiled.id, nativeType)

  // Safety check: annotated tree should verify
  const checkOk = checkAnnotated(nativeDefs, annotated, nativeType)

  // For type definitions, store the native type tree in nativeEnv.
  const treeForEnv = treeEqual(nativeType, TN_TYPE)
    ? collapseTypedExpr(valueTexpr)
    : compiled

  const newEnv = new Map(env)
  newEnv.set(name, { tree: treeForEnv, annTree: annotated, type: nativeType })

  return { env: newEnv, nativeType, compiled, annotated, checkOk }
}
