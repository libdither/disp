// Tree-native type checker: types and programs are both trees.
//
// Type encoding:
//   Type          = leaf                    (the type of types)
//   Tree          = stem(leaf)              (the type of all trees)
//   Bool          = stem(stem(leaf))        (booleans)
//   Nat           = stem(stem(stem(leaf)))  (naturals)
//   Pi(A, B)      = fork(A, B)             (dependent function type)
//   A -> B        = fork(A, K(B))          (non-dependent function type)
//
// Annotated tree format (D embedded at S nodes):
//   fork(leaf, ann_v)                      K — unchanged
//   fork(stem(D), fork(ann_c, ann_b))      S — D in stem slot
//   fork(fork(ann_c, ann_d), ann_b)        Triage — unchanged
//   leaf                                   constructor — unchanged
//   stem(ann_a)                            constructor — unchanged

import { type Tree, LEAF, stem, fork, apply, treeEqual, I, K, isLeaf, isStem, isFork } from "./tree.js"

// ============================================================
// Type constants
// ============================================================

export const TN_TYPE = LEAF
export const TN_TREE = stem(LEAF)
export const TN_BOOL = stem(stem(LEAF))
export const TN_NAT  = stem(stem(stem(LEAF)))

export function tnArrow(A: Tree, B: Tree): Tree { return fork(A, fork(LEAF, B)) }
export function tnPi(A: Tree, B: Tree): Tree { return fork(A, B) }

// ============================================================
// Annotation constructors
// ============================================================

export function annK(annV: Tree): Tree { return fork(LEAF, annV) }
export function annS(D: Tree, annC: Tree, annB: Tree): Tree { return fork(stem(D), fork(annC, annB)) }
export function annTriage(annC: Tree, annD: Tree, annB: Tree): Tree { return fork(fork(annC, annD), annB) }

// ============================================================
// Program extraction (strip D annotations from S nodes)
// ============================================================

export function extract(ann: Tree): Tree {
  if (isLeaf(ann)) return LEAF
  if (isStem(ann)) return stem(extract(ann.child))
  // fork — dispatch on left child
  if (isLeaf(ann.left)) {
    // K(v): fork(leaf, ann_v) → fork(leaf, extract(v))
    return fork(LEAF, extract(ann.right))
  }
  if (isStem(ann.left)) {
    // Annotated S: fork(stem(D), fork(ann_c, ann_b))
    // → fork(stem(extract(c)), extract(b))
    if (isFork(ann.right)) {
      return fork(stem(extract(ann.right.left)), extract(ann.right.right))
    }
    return fork(stem(extract(ann.left.child)), extract(ann.right))
  }
  // Triage: fork(fork(c, d), b)
  return fork(fork(extract(ann.left.left), extract(ann.left.right)), extract(ann.right))
}

// ============================================================
// Base type membership
// ============================================================

export function isOfBaseType(v: Tree, T: Tree): boolean {
  if (treeEqual(T, TN_TREE) || treeEqual(T, TN_TYPE)) return true
  if (treeEqual(T, TN_BOOL)) {
    return treeEqual(v, LEAF) || (isStem(v) && treeEqual(v.child, LEAF))
  }
  if (treeEqual(T, TN_NAT)) {
    let t: Tree = v
    while (isStem(t)) t = t.child
    return isLeaf(t)
  }
  return false
}

// ============================================================
// Helpers
// ============================================================

export function isNonDep(B: Tree): Tree | null {
  return isFork(B) && isLeaf(B.left) ? B.right : null
}

function ultimateCod(t: Tree): Tree {
  if (!isFork(t)) return t
  const nd = isNonDep(t.right)
  return nd ? ultimateCod(nd) : t
}

// ============================================================
// Abstract type markers (for polymorphism)
// ============================================================

const abstractIds = new Set<number>()
let abstractCtr = 0

export function freshAbstract(): Tree {
  let t: Tree = fork(LEAF, fork(LEAF, LEAF))
  for (let i = 0; i < abstractCtr; i++) t = fork(t, t)
  const m = fork(fork(stem(stem(stem(stem(LEAF)))), t), LEAF)
  abstractCtr++
  abstractIds.add(m.id)
  return m
}

export function isAbstract(t: Tree): boolean { return abstractIds.has(t.id) }

export function resetAbstractCounter(): void {
  abstractCtr = 0
  abstractIds.clear()
}

// ============================================================
// Dependent codomain helpers
// ============================================================

// stemCodomain(B) = S(K(B), leaf): [u] apply(B, stem(u))
export function stemCodomain(B: Tree): Tree {
  return fork(stem(fork(LEAF, B)), LEAF)
}

// forkCodomain(B) = S(K(stem(stem(K(B)))), leaf): [u][v] apply(B, fork(u,v))
export function forkCodomain(B: Tree): Tree {
  const KB = fork(LEAF, B)
  const sKB = stem(KB)
  const ssKB = stem(sKB)
  return fork(stem(fork(LEAF, ssKB)), LEAF)
}

// sCodomain(D, B) = S(S(K(leaf), D), S(K(K), B)): [x] Pi(D(x), K(B(x)))
export function sCodomain(D: Tree, B: Tree): Tree {
  const left = fork(stem(fork(LEAF, LEAF)), D)         // S(K(leaf), D)
  const right = fork(stem(fork(LEAF, stem(LEAF))), B)   // S(K(K), B)
  return fork(stem(left), right)                         // S(left, right)
}

// ============================================================
// Leaf/stem as function checkers
// ============================================================

function checkLeafFn(A: Tree, B: Tree): boolean {
  const B0 = isNonDep(B)
  if (B0 !== null) {
    if (treeEqual(B0, TN_TREE)) return true
    if (treeEqual(B0, TN_NAT)) return treeEqual(A, TN_NAT) || treeEqual(A, TN_BOOL)
    if (treeEqual(B0, TN_BOOL)) return false
    if (isFork(B0) && treeEqual(ultimateCod(B0), TN_TREE)) return true
    return false
  }
  return false
}

function checkStemFn(a: Tree, A: Tree, B: Tree): boolean {
  const B0 = isNonDep(B)
  if (!B0) return false
  if (treeEqual(B0, TN_TREE)) return true
  if (treeEqual(B0, TN_NAT) || treeEqual(B0, TN_BOOL)) return false
  if (isFork(B0)) {
    if (isLeaf(a)) {
      const inner = { d: B0.left, c: B0.right }
      const D0 = isNonDep(inner.c)
      if (D0 && (treeEqual(A, D0) || treeEqual(D0, TN_TREE) || treeEqual(D0, TN_TYPE))) return true
      if (D0 && treeEqual(D0, TN_NAT) && treeEqual(A, TN_BOOL)) return true
    }
    if (treeEqual(ultimateCod(B0), TN_TREE)) return true
  }
  return false
}

// ============================================================
// Known definitions context
// ============================================================

export type KnownDefs = Map<number, Tree>

// ============================================================
// The checker: check(defs, annotatedTree, type) → boolean
// ============================================================

export function checkAnnotated(
  defs: KnownDefs,
  ann: Tree,
  type: Tree,
  inputAssumption?: { value: Tree; type: Tree },
  depth = 0
): boolean {
  if (depth > 80) return false

  const prog = extract(ann)

  // Base type membership check FIRST — values can be members of multiple types
  // (e.g., LEAF is zero:Nat, true:Bool, leaf:Tree, Type:Type)
  if (isStem(type)) return isOfBaseType(prog, type)

  // Type : Type
  if (treeEqual(type, TN_TYPE)) return true

  // Known definition lookup — only for fork-shaped programs (actual combinators).
  // Leaf and stem trees can inhabit multiple types (e.g., LEAF = zero = true = leaf,
  // stem(LEAF) = false = K = succ(zero)), so we never short-circuit on them.
  if (isFork(prog) && isFork(type)) {
    const known = defs.get(prog.id)
    if (known !== undefined) return treeEqual(known, type)
  }

  // Abstract type — identity rule
  if (isAbstract(type)) {
    return !!(inputAssumption && treeEqual(prog, inputAssumption.value) && treeEqual(type, inputAssumption.type))
  }

  // Must be a Pi type (fork)
  if (!isFork(type)) return false

  const A = type.left
  const B = type.right

  // === LEAF (stem constructor) ===
  if (isLeaf(ann)) return checkLeafFn(A, B)

  // === STEM (fork constructor) ===
  if (isStem(ann)) return checkStemFn(ann.child, A, B)

  // === K: fork(leaf, ann_v) ===
  if (isLeaf(ann.left)) {
    const annV = ann.right
    const B0 = isNonDep(B)
    if (B0 !== null) return checkAnnotated(defs, annV, B0, undefined, depth + 1)
    // Dependent codomain
    if (treeEqual(A, TN_TYPE) || isAbstract(A)) {
      return checkAnnotated(defs, annV, apply(B, freshAbstract()), undefined, depth + 1)
    }
    if (treeEqual(A, TN_BOOL)) {
      return checkAnnotated(defs, annV, apply(B, LEAF), undefined, depth + 1) &&
             checkAnnotated(defs, annV, apply(B, stem(LEAF)), undefined, depth + 1)
    }
    if (treeEqual(A, TN_NAT)) {
      return checkAnnotated(defs, annV, apply(B, LEAF), undefined, depth + 1) &&
             checkAnnotated(defs, annV, apply(B, stem(LEAF)), undefined, depth + 1)
    }
    return checkAnnotated(defs, annV, apply(B, LEAF), undefined, depth + 1)
  }

  // === S: fork(stem(D), fork(ann_c, ann_b)) ===
  if (isStem(ann.left)) {
    const D = ann.left.child
    if (!isFork(ann.right)) return false
    const annC = ann.right.left
    const annB = ann.right.right

    // b : Pi(A, D)
    if (!checkAnnotated(defs, annB, fork(A, D), undefined, depth + 1)) return false

    // c : Pi(A, cCodomain) where cCodomain(x) = Pi(D(x), K(B(x)))
    const B0 = isNonDep(B)
    const D0 = isNonDep(D)
    if (B0 !== null && D0 !== null) {
      // Non-dependent: c : A → D₀ → B₀
      return checkAnnotated(defs, annC, tnArrow(A, tnArrow(D0, B0)), undefined, depth + 1)
    }
    // Dependent: use sCodomain
    return checkAnnotated(defs, annC, fork(A, sCodomain(D, B)), undefined, depth + 1)
  }

  // === TRIAGE: fork(fork(ann_c, ann_d), ann_b) ===
  const annC = ann.left.left
  const annD = ann.left.right
  const annB = ann.right
  const B0 = isNonDep(B)

  // Abstract domain
  if (isAbstract(A)) {
    const exp = B0 !== null ? B0 : apply(B, LEAF)
    const pc = extract(annC), pd = extract(annD), pb = extract(annB)
    const leafOk = treeEqual(pc, LEAF) && isAbstract(exp)
      ? true : checkAnnotated(defs, annC, exp, { value: LEAF, type: A }, depth + 1)
    const stemOk = treeEqual(pd, LEAF) && B0 !== null && isAbstract(B0)
    const forkOk = treeEqual(pb, LEAF) && B0 !== null && isAbstract(B0)
    return leafOk && stemOk && forkOk
  }

  // Pi domain (function-type inputs)
  if (isFork(A) && !treeEqual(A, TN_BOOL) && !treeEqual(A, TN_NAT) && !treeEqual(A, TN_TREE)) {
    const pc = extract(annC), pd = extract(annD), pb = extract(annB)
    const leafOk = treeEqual(pc, LEAF) && B0 !== null && treeEqual(B0, A)
      ? true : checkAnnotated(defs, annC, B0 ?? apply(B, LEAF), { value: LEAF, type: A }, depth + 1)
    const stemOk = treeEqual(pd, LEAF) && B0 !== null && treeEqual(B0, A)
    const forkOk = treeEqual(pb, LEAF) && B0 !== null && treeEqual(B0, A)
    return leafOk && stemOk && forkOk
  }

  // Tree domain
  if (treeEqual(A, TN_TREE)) {
    const Bleaf = B0 !== null ? B0 : apply(B, LEAF)
    if (!checkAnnotated(defs, annC, Bleaf, undefined, depth + 1)) return false
    const stemB = B0 !== null ? fork(LEAF, B0) : stemCodomain(B)
    if (!checkAnnotated(defs, annD, fork(TN_TREE, stemB), undefined, depth + 1)) return false
    const forkB = B0 !== null ? fork(LEAF, tnArrow(TN_TREE, B0)) : forkCodomain(B)
    return checkAnnotated(defs, annB, fork(TN_TREE, forkB), undefined, depth + 1)
  }

  // Nat domain
  if (treeEqual(A, TN_NAT)) {
    const Bleaf = B0 !== null ? B0 : apply(B, LEAF)
    if (!checkAnnotated(defs, annC, Bleaf, undefined, depth + 1)) return false
    const stemB = B0 !== null ? fork(LEAF, B0) : stemCodomain(B)
    return checkAnnotated(defs, annD, fork(TN_NAT, stemB), undefined, depth + 1)
  }

  // Bool domain
  if (treeEqual(A, TN_BOOL)) {
    const Bleaf = B0 !== null ? B0 : apply(B, LEAF)
    if (!checkAnnotated(defs, annC, Bleaf, undefined, depth + 1)) return false
    const Bstem = B0 !== null ? B0 : apply(B, stem(LEAF))
    return checkAnnotated(defs, apply(extract(annD), LEAF), Bstem, undefined, depth + 1)
  }

  return false
}
