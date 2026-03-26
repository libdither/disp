// Tree-native elaboration adapter.
//
// Bridges the existing CoC elaborator with the tree-native checker:
// 1. convertCocType: CoC-encoded type → tree-native type
// 2. annotateTree: raw compiled tree + native type → annotated tree (D at S nodes)
// 3. nativeCheckDecl: full declaration pipeline

import { type Tree, LEAF, stem, fork, apply, treeEqual, I, K, isLeaf, isStem, isFork } from "./tree.js"
import { bracketAbstract, collapse, compileAndEval, compileRecAndEval } from "./compile.js"
import {
  termTag, unPi, unVar, unLam, unApp,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
  whnfTree, evalToNative, encVar, freshMarker,
  treeToExprReplacing,
  cocCheckDecl, type Env,
} from "./coc.js"
import { type SExpr } from "./parse.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow, tnPi,
  annK, annS, annTriage,
  isNonDep, stemCodomain, forkCodomain, sCodomain,
  type KnownDefs, checkAnnotated,
} from "./tree-native-checker.js"

// ============================================================
// CoC type → tree-native type conversion
// ============================================================

function treeContains(tree: Tree, target: Tree): boolean {
  if (treeEqual(tree, target)) return true
  if (isLeaf(tree)) return false
  if (isStem(tree)) return treeContains(tree.child, target)
  return treeContains(tree.left, target) || treeContains(tree.right, target)
}

function abstractTreeMarker(tree: Tree, marker: Tree): Tree {
  const name = "__tn_marker__"
  const expr = treeToExprReplacing(tree, marker, name)
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
  // Known definition — no annotation needed
  if (defs.has(raw.id)) return raw

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

    const D = inferIntermediateType(b, A, defs)
    const D0 = isNonDep(D)
    const B0 = isNonDep(B)

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
