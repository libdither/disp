// Annotated trees: program + derivation superimposed into a single tree.
//
// Observation: the derivation mirrors the program's structure exactly.
// The ONLY new information is the intermediate type D at S nodes.
// So we embed D directly into the tree format:
//
//   Annotated tree format (3-way dispatch on left child, same as before):
//
//     fork(leaf, ann_v)                      K form — unchanged
//     fork(stem(D), fork(ann_c, ann_b))      S form — D replaces c in stem position
//     fork(fork(ann_c, ann_d), ann_b)         Triage — unchanged
//     leaf                                    constructor — unchanged
//     stem(ann_a)                             constructor — unchanged
//
// The checker processes this in ONE triage dispatch per node.
// No separate derivation input. No lock-step traversal of two trees.
//
// To execute, extract the pure program in O(n):
//     extract(fork(stem(D), fork(c, b))) = fork(stem(extract(c)), extract(b))

import { describe, it, expect } from "vitest"
import { Tree, LEAF, stem, fork, apply, treeEqual, I, K, prettyTree, isLeaf, isStem, isFork } from "../src/tree.js"

// === TYPE ENCODING ===
const TYPE = LEAF
const TREE = stem(LEAF)
const BOOL = stem(stem(LEAF))
const NAT  = stem(stem(stem(LEAF)))
function arrow(A: Tree, B: Tree): Tree { return fork(A, fork(LEAF, B)) }

// === BASE TYPE MEMBERSHIP ===
function isOfBaseType(v: Tree, T: Tree): boolean {
  if (treeEqual(T, TREE)) return true
  if (treeEqual(T, TYPE)) return true
  if (treeEqual(T, BOOL)) {
    return treeEqual(v, LEAF) || (isStem(v) && treeEqual(v.child, LEAF))
  }
  if (treeEqual(T, NAT)) {
    let t: Tree = v
    while (isStem(t)) t = t.child
    return isLeaf(t)
  }
  return false
}

// === ANNOTATED TREE CONSTRUCTION ===

// Build an annotated K node: fork(leaf, annotated_value)
function annK(annV: Tree): Tree {
  return fork(LEAF, annV)
}

// Build an annotated S node: fork(stem(D), fork(annotated_c, annotated_b))
// D is the intermediate type; c and b are already annotated
function annS(D: Tree, annC: Tree, annB: Tree): Tree {
  return fork(stem(D), fork(annC, annB))
}

// Build an annotated Triage node: fork(fork(ann_c, ann_d), ann_b)
function annTriage(annC: Tree, annD: Tree, annB: Tree): Tree {
  return fork(fork(annC, annD), annB)
}

// Annotated leaf/stem are unchanged
// leaf = LEAF
// stem(a) = stem(annotated_a)  [for constructor-as-function]

// === PROGRAM EXTRACTION ===
// Strip type annotations from S nodes, recover the pure executable program.

function extract(ann: Tree): Tree {
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
      const c = extract(ann.right.left)
      const b = extract(ann.right.right)
      return fork(stem(c), b)
    }
    // Malformed — treat as raw
    return fork(stem(extract(ann.left.child)), extract(ann.right))
  }
  // Triage: fork(fork(c, d), b) → fork(fork(extract(c), extract(d)), extract(b))
  return fork(fork(extract(ann.left.left), extract(ann.left.right)), extract(ann.right))
}

// === ANNOTATED TREE CHECKER ===
// Single-pass: one triage dispatch per node, reads D inline.

// Known definitions context
type KnownDefs = Map<number, Tree>

// Abstract type markers for polymorphism
const abstractIds = new Set<number>()
let abstractCtr = 0
function freshAbstract(): Tree {
  let t: Tree = fork(LEAF, fork(LEAF, LEAF))
  for (let i = 0; i < abstractCtr; i++) t = fork(t, t)
  const m = fork(fork(stem(stem(stem(stem(LEAF)))), t), LEAF)
  abstractCtr++
  abstractIds.add(m.id)
  return m
}
function isAbstract(t: Tree): boolean { return abstractIds.has(t.id) }

function isNonDep(B: Tree): Tree | null {
  return isFork(B) && isLeaf(B.left) ? B.right : null
}

function ultimateCodomain(t: Tree): Tree {
  if (!isFork(t)) return t
  const nd = isNonDep(t.right)
  if (!nd) return t
  return ultimateCodomain(nd)
}

function checkLeafFn(A: Tree, B: Tree): boolean {
  const B0 = isNonDep(B)
  if (!B0) return false
  if (treeEqual(B0, TREE)) return true
  if (treeEqual(B0, NAT)) return treeEqual(A, NAT) || treeEqual(A, BOOL)
  if (treeEqual(B0, BOOL)) return false
  if (isFork(B0) && treeEqual(ultimateCodomain(B0), TREE)) return true
  return false
}

function checkStemFn(a: Tree, A: Tree, B: Tree): boolean {
  const B0 = isNonDep(B)
  if (!B0) return false
  if (treeEqual(B0, TREE)) return true
  if (treeEqual(B0, NAT) || treeEqual(B0, BOOL)) return false
  if (isFork(B0)) {
    // fork(a, x) : Pi(C, D). If a = leaf → K(x) rule.
    const inner = isFork(B0) ? { d: B0.left, c: B0.right } : null
    if (inner && isLeaf(a)) {
      const D0 = isNonDep(inner.c)
      if (D0 && (treeEqual(A, D0) || treeEqual(D0, TREE) || treeEqual(D0, TYPE))) return true
      if (D0 && treeEqual(D0, NAT) && treeEqual(A, BOOL)) return true
    }
    if (treeEqual(ultimateCodomain(B0), TREE)) return true
    return false
  }
  return false
}

// The single-pass checker. Takes annotated tree + type → bool.
function checkAnn(
  defs: KnownDefs,
  ann: Tree,
  type: Tree,
  inputAssumption?: { value: Tree; type: Tree },
  depth = 0
): boolean {
  if (depth > 60) return false

  // Known definition lookup (by the EXTRACTED program's id)
  const prog = extract(ann)
  const known = defs.get(prog.id)
  if (known !== undefined) return treeEqual(known, type)

  // Base type
  if (isStem(type)) return isOfBaseType(prog, type)

  // Type : Type
  if (treeEqual(type, TYPE)) return true

  // Abstract type
  if (isAbstract(type)) {
    if (inputAssumption && treeEqual(prog, inputAssumption.value) && treeEqual(type, inputAssumption.type))
      return true
    return false
  }

  // Pi type: fork(A, B)
  if (!isFork(type)) return false
  const A = type.left, B = type.right

  // Dispatch on annotated tree structure (ONE triage)
  if (isLeaf(ann)) {
    // leaf as function
    return checkLeafFn(A, B)
  }
  if (isStem(ann)) {
    // stem(a) as function
    return checkStemFn(ann.child, A, B)
  }

  // fork — 3-way dispatch on left child
  if (isLeaf(ann.left)) {
    // K(v): fork(leaf, ann_v)
    const annV = ann.right
    const B0 = isNonDep(B)
    if (B0 !== null) return checkAnn(defs, annV, B0, undefined, depth + 1)
    if (treeEqual(A, TYPE) || isAbstract(A)) {
      const alpha = freshAbstract()
      return checkAnn(defs, annV, apply(B, alpha), undefined, depth + 1)
    }
    if (treeEqual(A, BOOL)) {
      return checkAnn(defs, annV, apply(B, LEAF), undefined, depth + 1) &&
             checkAnn(defs, annV, apply(B, stem(LEAF)), undefined, depth + 1)
    }
    return false
  }

  if (isStem(ann.left)) {
    // ANNOTATED S: fork(stem(D), fork(ann_c, ann_b))
    // D is the intermediate type, read directly from the tree!
    const D = ann.left.child
    if (!isFork(ann.right)) return false
    const annC = ann.right.left
    const annB = ann.right.right

    const B0 = isNonDep(B)
    const D0 = isNonDep(D)

    // Check b : Pi(A, D)
    const bOk = checkAnn(defs, annB, fork(A, D), undefined, depth + 1)
    if (!bOk) return false

    // Check c : Pi(A, x → Pi(D(x), _ → B(x)))
    if (B0 !== null && D0 !== null) {
      return checkAnn(defs, annC, arrow(A, arrow(D0, B0)), undefined, depth + 1)
    }
    // Dependent case: build c's codomain function
    const cCod = fork(stem(fork(stem(fork(LEAF, LEAF)), D)), fork(stem(fork(LEAF, K)), B))
    return checkAnn(defs, annC, fork(A, cCod), undefined, depth + 1)
  }

  // TRIAGE: fork(fork(ann_c, ann_d), ann_b)
  const annC = ann.left.left
  const annD = ann.left.right
  const annB = ann.right
  const B0 = isNonDep(B)

  // Abstract domain
  if (isAbstract(A)) {
    const exp = B0 !== null ? B0 : apply(B, LEAF)
    const progC = extract(annC)
    const leafOk = treeEqual(progC, LEAF) && isAbstract(exp)
      ? true
      : checkAnn(defs, annC, exp, { value: LEAF, type: A }, depth + 1)
    const progD = extract(annD)
    const stemOk = treeEqual(progD, LEAF) && B0 !== null && isAbstract(B0)
    const progB = extract(annB)
    const forkOk = treeEqual(progB, LEAF) && B0 !== null && isAbstract(B0)
    return leafOk && stemOk && forkOk
  }

  // Pi domain (function types)
  if (isFork(A)) {
    const progC = extract(annC), progD = extract(annD), progB = extract(annB)
    const leafOk = treeEqual(progC, LEAF) && B0 !== null && treeEqual(B0, A)
      ? true : checkAnn(defs, annC, B0 ?? apply(B, LEAF), { value: LEAF, type: A }, depth + 1)
    const stemOk = treeEqual(progD, LEAF) && B0 !== null && treeEqual(B0, A)
      ? true : (() => { const o = apply(progD, LEAF); return checkAnn(defs, annD, B0 ?? apply(B, stem(LEAF)), undefined, depth + 1) })()
    const forkOk = treeEqual(progB, LEAF) && B0 !== null && treeEqual(B0, A)
    return leafOk && stemOk && forkOk
  }

  // Tree domain
  if (treeEqual(A, TREE)) {
    const Rleaf = B0 !== null ? B0 : apply(B, LEAF)
    const leafOk = checkAnn(defs, annC, Rleaf, undefined, depth + 1)
    const stemOk = B0 !== null
      ? checkAnn(defs, annD, arrow(TREE, B0), undefined, depth + 1)
      : false
    const forkOk = B0 !== null
      ? checkAnn(defs, annB, arrow(TREE, arrow(TREE, B0)), undefined, depth + 1)
      : false
    return leafOk && stemOk && forkOk
  }

  // Nat domain
  if (treeEqual(A, NAT)) {
    const Rleaf = B0 !== null ? B0 : apply(B, LEAF)
    const leafOk = checkAnn(defs, annC, Rleaf, undefined, depth + 1)
    const stemOk = B0 !== null
      ? checkAnn(defs, annD, arrow(NAT, B0), undefined, depth + 1)
      : false
    return leafOk && stemOk
  }

  // Bool domain
  if (treeEqual(A, BOOL)) {
    const Rleaf = B0 !== null ? B0 : apply(B, LEAF)
    const leafOk = checkAnn(defs, annC, Rleaf, undefined, depth + 1)
    const progD = extract(annD)
    const Rstem = B0 !== null ? B0 : apply(B, stem(LEAF))
    const stemOk = checkAnn(defs, apply(progD, LEAF), Rstem, undefined, depth + 1)
    return leafOk && stemOk
  }

  return false
}


// ============================================================
// TESTS
// ============================================================

describe("Annotated tree format", () => {
  it("K(v) — annotation is structural identity", () => {
    const kTrue = annK(LEAF) // K(true) annotated = fork(leaf, leaf) — same as raw!
    const raw = fork(LEAF, LEAF)
    expect(treeEqual(kTrue, raw)).toBe(true)
    console.log(`K(true) annotated: ${prettyTree(kTrue)} = ${prettyTree(raw)} (identical!)`)
  })

  it("Triage — annotation is structural identity", () => {
    const annI = annTriage(LEAF, LEAF, LEAF) // Triage(leaf, leaf, leaf)
    expect(treeEqual(annI, I)).toBe(true)
    console.log(`I annotated: ${prettyTree(annI)} = ${prettyTree(I)} (identical!)`)
  })

  it("S node — D embedded in stem position", () => {
    // S(K(succ), succ) with D=Nat
    // Raw program: fork(stem(K(succ)), succ) = fork(stem(fork(leaf,leaf)), leaf)
    // Annotated:   fork(stem(Nat), fork(K(succ)_ann, succ_ann))
    //            = fork(stem(K(Nat)), fork(fork(leaf,leaf), leaf))
    //
    // K(Nat) = fork(leaf, Nat) is the non-dependent D wrapper

    const ann = annS(
      fork(LEAF, NAT),         // D = K(Nat) (non-dependent intermediate type)
      annK(LEAF),              // annotated c = K(succ) = fork(leaf, leaf)
      LEAF                     // annotated b = succ = leaf
    )
    console.log(`\nS(K(succ), succ) with D=Nat:`)
    console.log(`  annotated: ${prettyTree(ann)}`)
    console.log(`  form: fork(stem(K(Nat)), fork(K(succ)_ann, succ_ann))`)

    // Extract pure program
    const prog = extract(ann)
    const rawProg = fork(stem(fork(LEAF, LEAF)), LEAF) // S(K(succ), succ)
    console.log(`  extracted: ${prettyTree(prog)}`)
    console.log(`  raw prog:  ${prettyTree(rawProg)}`)
    expect(treeEqual(prog, rawProg)).toBe(true)

    // Verify execution
    expect(treeEqual(apply(prog, LEAF), stem(stem(LEAF)))).toBe(true) // succ(succ(0)) = 2
  })
})

describe("Annotated tree extraction", () => {
  it("leaf → leaf", () => {
    expect(treeEqual(extract(LEAF), LEAF)).toBe(true)
  })

  it("K node passes through", () => {
    const ann = annK(stem(LEAF)) // K(1)
    expect(treeEqual(extract(ann), fork(LEAF, stem(LEAF)))).toBe(true)
  })

  it("Triage passes through", () => {
    const ann = annTriage(LEAF, LEAF, LEAF) // I
    expect(treeEqual(extract(ann), I)).toBe(true)
  })

  it("S node strips D and repacks c, b", () => {
    // Annotated: fork(stem(D), fork(ann_c, ann_b))
    // Extracted: fork(stem(extract(ann_c)), extract(ann_b))
    const ann = annS(fork(LEAF, NAT), annK(LEAF), LEAF)
    const prog = extract(ann)
    expect(prog.tag).toBe("fork")
    if (isFork(prog) && isStem(prog.left)) {
      // Left is stem(c_extracted): c = extract(K(leaf)) = K(leaf) = fork(leaf, leaf)
      expect(treeEqual(prog.left.child, fork(LEAF, LEAF))).toBe(true)
      // Right is extract(leaf) = leaf = b
      expect(treeEqual(prog.right, LEAF)).toBe(true)
    }
  })

  it("nested S nodes both strip D", () => {
    // double = S(S(K(add), I), I) with D=Nat at both levels
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))

    const innerAnn = annS(fork(LEAF, NAT), annK(mockAdd), annTriage(LEAF, LEAF, LEAF))
    const outerAnn = annS(fork(LEAF, NAT), innerAnn, annTriage(LEAF, LEAF, LEAF))

    const prog = extract(outerAnn)
    // Should be S(S(K(add), I), I)
    // = fork(stem(fork(stem(fork(leaf, add)), I)), I)
    console.log(`\ndouble extracted: ${prettyTree(prog)}`)

    // Verify inner S extracted correctly
    if (isFork(prog) && isStem(prog.left)) {
      const innerProg = prog.left.child // S(K(add), I)
      console.log(`  inner S: ${prettyTree(innerProg)}`)
      if (isFork(innerProg) && isStem(innerProg.left)) {
        // c = K(add), b = I
        expect(treeEqual(innerProg.left.child, fork(LEAF, mockAdd))).toBe(true) // K(add)
        expect(treeEqual(innerProg.right, I)).toBe(true) // I
      }
    }
  })
})

describe("Single-pass annotated checker", () => {
  it("K(true) : Bool → Bool", () => {
    const ann = annK(LEAF)
    expect(checkAnn(new Map(), ann, arrow(BOOL, BOOL))).toBe(true)
  })

  it("K(true) : Nat → Bool", () => {
    const ann = annK(LEAF)
    expect(checkAnn(new Map(), ann, arrow(NAT, BOOL))).toBe(true)
  })

  it("K(fork) : Bool → Nat — REJECTS", () => {
    const ann = annK(fork(LEAF, LEAF))
    expect(checkAnn(new Map(), ann, arrow(BOOL, NAT))).toBe(false)
  })

  it("I : Nat → Nat", () => {
    const ann = annTriage(LEAF, LEAF, LEAF)
    expect(checkAnn(new Map(), ann, arrow(NAT, NAT))).toBe(true)
  })

  it("I : Bool → Bool", () => {
    const ann = annTriage(LEAF, LEAF, LEAF)
    expect(checkAnn(new Map(), ann, arrow(BOOL, BOOL))).toBe(true)
  })

  it("I : Tree → Tree", () => {
    const ann = annTriage(LEAF, LEAF, LEAF)
    expect(checkAnn(new Map(), ann, arrow(TREE, TREE))).toBe(true)
  })

  it("not : Bool → Bool", () => {
    const ann = annTriage(
      stem(LEAF),                              // false
      annK(LEAF),                              // K(true)
      annK(annK(stem(LEAF)))                   // K(K(false))
    )
    expect(checkAnn(new Map(), ann, arrow(BOOL, BOOL))).toBe(true)
  })

  it("leaf : Nat → Nat (succ)", () => {
    expect(checkAnn(new Map(), LEAF, arrow(NAT, NAT))).toBe(true)
  })

  it("leaf : Bool → Bool — REJECTS", () => {
    expect(checkAnn(new Map(), LEAF, arrow(BOOL, BOOL))).toBe(false)
  })

  it("K = stem(leaf) : Nat → (Bool → Nat)", () => {
    expect(checkAnn(new Map(), K, arrow(NAT, arrow(BOOL, NAT)))).toBe(true)
  })

  it("S(K(succ), succ) : Nat → Nat with D=Nat", () => {
    const ann = annS(
      fork(LEAF, NAT),  // D = K(Nat)
      annK(LEAF),        // K(succ) annotated
      LEAF               // succ = leaf
    )
    expect(checkAnn(new Map(), ann, arrow(NAT, NAT))).toBe(true)
  })

  it("S with wrong D — REJECTS", () => {
    const ann = annS(
      fork(LEAF, BOOL),  // D = K(Bool) — WRONG
      annK(LEAF),
      LEAF
    )
    expect(checkAnn(new Map(), ann, arrow(NAT, NAT))).toBe(false)
  })

  it("double = S(S(K(add), I), I) with add in context", () => {
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))
    const defs: KnownDefs = new Map()
    defs.set(mockAdd.id, arrow(NAT, arrow(NAT, NAT)))

    const ann = annS(
      fork(LEAF, NAT),  // outer D = K(Nat)
      annS(
        fork(LEAF, NAT),  // inner D = K(Nat)
        annK(mockAdd),     // K(add)
        annTriage(LEAF, LEAF, LEAF) // I
      ),
      annTriage(LEAF, LEAF, LEAF)  // I
    )

    expect(checkAnn(defs, ann, arrow(NAT, NAT))).toBe(true)
  })

  it("polymorphic identity: K(I) : Pi(Type, α → α)", () => {
    const selfArrow = fork(stem(LEAF), K) // S(leaf, K) maps α to α→α
    const ann = annK(annTriage(LEAF, LEAF, LEAF)) // K(I)
    expect(checkAnn(new Map(), ann, fork(TYPE, selfArrow))).toBe(true)
  })

  it("polymorphic identity REJECT: K(K(leaf)) : Pi(Type, α → α)", () => {
    const selfArrow = fork(stem(LEAF), K)
    const ann = annK(annK(LEAF)) // K(K(leaf)) — not identity
    expect(checkAnn(new Map(), ann, fork(TYPE, selfArrow))).toBe(false)
  })

  it("I : (Nat → Nat) → (Nat → Nat) — identity on functions", () => {
    const fnType = arrow(NAT, NAT)
    const ann = annTriage(LEAF, LEAF, LEAF)
    expect(checkAnn(new Map(), ann, arrow(fnType, fnType))).toBe(true)
  })
})

describe("Size comparison: annotated vs separate derivation", () => {
  function treeSize(t: Tree): number {
    if (isLeaf(t)) return 1
    if (isStem(t)) return 1 + treeSize(t.child)
    return 1 + treeSize(t.left) + treeSize(t.right)
  }

  it("K(v) — identical size (no annotation needed)", () => {
    const raw = fork(LEAF, stem(LEAF)) // K(1)
    const ann = annK(stem(LEAF))
    expect(treeSize(raw)).toBe(treeSize(ann))
    console.log(`K(1): raw=${treeSize(raw)}, annotated=${treeSize(ann)} (same)`)
  })

  it("I (Triage) — identical size", () => {
    const raw = I
    const ann = annTriage(LEAF, LEAF, LEAF)
    expect(treeSize(raw)).toBe(treeSize(ann))
    console.log(`I: raw=${treeSize(raw)}, annotated=${treeSize(ann)} (same)`)
  })

  it("S(K(succ), succ) — annotated has D overhead", () => {
    const raw = fork(stem(fork(LEAF, LEAF)), LEAF) // S(K(succ), succ)
    const ann = annS(fork(LEAF, NAT), annK(LEAF), LEAF)

    const rawSize = treeSize(raw)
    const annSize = treeSize(ann)
    const overhead = annSize - rawSize

    console.log(`S(K(succ),succ): raw=${rawSize}, annotated=${annSize}, overhead=+${overhead} (D=K(Nat))`)
    // D = K(Nat) = fork(leaf, stem(stem(stem(leaf)))) adds a few nodes
    // But we eliminated the entire separate derivation tree!
  })

  it("double S(S(K(add),I),I) — overhead is 2 D annotations", () => {
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))

    const rawInner = fork(stem(fork(LEAF, mockAdd)), I)
    const raw = fork(stem(rawInner), I)

    const ann = annS(
      fork(LEAF, NAT),
      annS(fork(LEAF, NAT), annK(mockAdd), annTriage(LEAF, LEAF, LEAF)),
      annTriage(LEAF, LEAF, LEAF)
    )

    const rawSize = treeSize(raw)
    const annSize = treeSize(ann)
    console.log(`double: raw=${rawSize}, annotated=${annSize}, overhead=+${annSize - rawSize}`)
    console.log(`  (2 S nodes × ~5 nodes per D = ~10 extra nodes)`)
  })
})

// ============================================================
// CONTEXT EQUIVALENCE + HASH-CONS MEMOIZATION
// ============================================================

describe("Annotated tree = distributed typing context", () => {
  it("D annotations correspond to variable types in context", () => {
    // In traditional type theory:
    //   Γ, x : Nat ⊢ add x x : Nat
    //   The context records "x has type Nat"
    //
    // In bracket abstraction:
    //   [x] add(x)(x) = S(S(K(add), I), I)
    //   Each S node is a point where x is USED (shared between sub-expressions)
    //   D=Nat at each S node says "the value flowing through this wire has type Nat"
    //
    // The D annotations ARE the typing context, distributed across data flow points:
    //   - Traditional: one entry "x : Nat" covers all uses of x
    //   - Annotated:   each S node where x flows records D=Nat independently
    //
    // In a linear program (x used once), there's no S → no D → no context needed.
    // In a program using x twice, there are S nodes → D records the shared type.

    console.log(`\n=== CONTEXT EQUIVALENCE ===`)
    console.log(``)
    console.log(`Traditional: Γ, x : Nat ⊢ add x x : Nat`)
    console.log(`  Context entry: x : Nat (one entry, covers all uses)`)
    console.log(``)
    console.log(`Annotated tree: S(S(K(add), I), I) with D=Nat at each S`)
    console.log(`  Inner S: "I(x) produces a Nat" → D=Nat`)
    console.log(`  Outer S: "I(x) produces a Nat" → D=Nat`)
    console.log(`  Each D records the type at a USE SITE of the argument.`)
    console.log(``)
    console.log(`K nodes = WEAKENING (argument unused → no context entry needed)`)
    console.log(`S nodes = CONTRACTION (argument shared → context entry required = D)`)
    console.log(`Triage  = ELIMINATION (argument destructured → type determines branches)`)
    console.log(``)
    console.log(`In structural type theory:`)
    console.log(`  weakening (discard) = K combinator  → no annotation`)
    console.log(`  contraction (share) = S combinator  → D annotation (type of shared value)`)
    console.log(`  exchange (reorder)  = built into S's argument passing`)
    console.log(``)
    console.log(`The annotated tree is a PROOF TERM in structural type theory,`)
    console.log(`with the typing context inlined at each structural rule application.`)
  })

  it("demonstrate: same program, different D = different context = different annotated tree", () => {
    // S(I, I) can be typed as:
    //   Tree → Tree with D=Tree (self-application on trees)
    //   Nat → Nat  would need D=Nat (but I(x) might not be Nat→Nat applicable... actually it is)
    //
    // Same program fork(stem(I), I) gets DIFFERENT annotated trees for different D:
    const annTreeD = annS(fork(LEAF, TREE), annTriage(LEAF, LEAF, LEAF), annTriage(LEAF, LEAF, LEAF))
    const annNatD  = annS(fork(LEAF, NAT), annTriage(LEAF, LEAF, LEAF), annTriage(LEAF, LEAF, LEAF))

    // Different trees (different D)
    expect(treeEqual(annTreeD, annNatD)).toBe(false)

    // But both extract to the same program
    expect(treeEqual(extract(annTreeD), extract(annNatD))).toBe(true)

    console.log(`\nSame program S(I, I), different contexts:`)
    console.log(`  D=Tree: id=${annTreeD.id}, tree=${prettyTree(annTreeD).slice(0, 60)}...`)
    console.log(`  D=Nat:  id=${annNatD.id}, tree=${prettyTree(annNatD).slice(0, 60)}...`)
    console.log(`  Extracted (same): ${prettyTree(extract(annTreeD))}`)
    console.log(`  This is correct: same program, different typing derivations → different annotated trees`)
  })
})

describe("Hash-consing and memoization", () => {
  it("shared sub-expressions get shared annotated trees", () => {
    // In double = S(S(K(add), I), I), the two I's are the same annotated tree
    const annI = annTriage(LEAF, LEAF, LEAF)

    // Both occurrences of I in the annotated double refer to the SAME tree object
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))
    const innerS = annS(fork(LEAF, NAT), annK(mockAdd), annI)
    const outerS = annS(fork(LEAF, NAT), innerS, annI)

    // The annI in innerS and outerS are hash-consed identical
    // (fork constructor returns cached tree for same left+right ids)
    if (isFork(outerS) && isFork(outerS.right)) {
      const outerB = outerS.right.right  // the outer I
      if (isFork(innerS) && isFork(innerS.right)) {
        const innerB = innerS.right.right  // the inner I
        expect(outerB.id).toBe(innerB.id) // same hash-cons id!
        expect(outerB.id).toBe(annI.id)   // and same as the standalone annI
      }
    }

    console.log(`\nHash-cons sharing in double = S(S(K(add), I), I):`)
    console.log(`  annI = id ${annI.id}`)
    console.log(`  Both I's in the annotated tree share this id.`)
    console.log(`  A memoized checker computing checkAnn(annI, Nat→Nat) once`)
    console.log(`  gets the result free the second time via (annI.id, type.id) cache.`)
  })

  it("D types are hash-consed across S nodes", () => {
    // Both S nodes in double use D = K(Nat) = fork(leaf, Nat)
    const D = fork(LEAF, NAT) // K(Nat)
    const stemD = stem(D)     // stem(K(Nat)) — the left child of annotated S

    // Build the two annotated S nodes
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))
    const annI = annTriage(LEAF, LEAF, LEAF)
    const innerS = annS(D, annK(mockAdd), annI)  // uses D
    const outerS = annS(D, innerS, annI)          // uses same D

    // Both S nodes share the SAME stem(D) as their left child
    if (isFork(innerS) && isStem(innerS.left) && isFork(outerS) && isStem(outerS.left)) {
      expect(innerS.left.id).toBe(outerS.left.id) // stem(K(Nat)) shared
      expect(innerS.left.child.id).toBe(D.id)      // K(Nat) shared
    }

    console.log(`\nD sharing across S nodes:`)
    console.log(`  D = K(Nat) = id ${D.id}`)
    console.log(`  stem(D) = id ${stemD.id}`)
    console.log(`  Both S nodes' left child = stem(D) with same id`)
    console.log(`  Hash-consing means D costs memory ONCE, not per-S-node.`)
  })

  it("memoized checker: count actual check calls vs cache hits", () => {
    // Build a checker with call counting
    let checkCalls = 0
    let cacheHits = 0
    const checkCache = new Map<string, boolean>()

    function checkMemo(ann: Tree, type: Tree, depth = 0): boolean {
      const key = `${ann.id}:${type.id}`
      const cached = checkCache.get(key)
      if (cached !== undefined) { cacheHits++; return cached }
      checkCalls++

      // Simplified checker for counting purposes
      if (isStem(type)) {
        const r = isOfBaseType(extract(ann), type)
        checkCache.set(key, r)
        return r
      }
      if (treeEqual(type, TYPE)) { checkCache.set(key, true); return true }
      if (!isFork(type)) { checkCache.set(key, false); return false }

      const A = type.left, B = type.right
      const B0 = isFork(B) && isLeaf(B.left) ? B.right : null

      if (isLeaf(ann)) {
        // leaf as function
        const r = B0 !== null && (treeEqual(B0, TREE) || (treeEqual(B0, NAT) && (treeEqual(A, NAT) || treeEqual(A, BOOL))))
        checkCache.set(key, r)
        return r
      }
      if (isStem(ann)) {
        const r = B0 !== null && treeEqual(B0, TREE)
        checkCache.set(key, r)
        return r
      }

      if (isLeaf(ann.left)) {
        // K(v)
        if (B0) { const r = checkMemo(ann.right, B0, depth + 1); checkCache.set(key, r); return r }
        checkCache.set(key, false); return false
      }

      if (isStem(ann.left)) {
        // Annotated S
        if (!isFork(ann.right)) { checkCache.set(key, false); return false }
        const D = ann.left.child
        const D0 = isFork(D) && isLeaf(D.left) ? D.right : null
        if (!B0 || !D0) { checkCache.set(key, false); return false }
        const bOk = checkMemo(ann.right.right, fork(A, D), depth + 1)
        const cOk = checkMemo(ann.right.left, arrow(A, arrow(D0, B0)), depth + 1)
        const r = bOk && cOk
        checkCache.set(key, r)
        return r
      }

      // Triage
      if (treeEqual(A, NAT) && B0) {
        const leafOk = checkMemo(ann.left.left, B0, depth + 1)
        const stemOk = checkMemo(ann.left.right, arrow(NAT, B0), depth + 1)
        const r = leafOk && stemOk
        checkCache.set(key, r)
        return r
      }
      checkCache.set(key, false)
      return false
    }

    // Test: double = S(S(K(add), I), I) : Nat → Nat
    const mockAdd = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF))
    const addType = arrow(NAT, arrow(NAT, NAT))

    // Register add in our simplified checker
    const addKey = `${mockAdd.id}:${addType.id}`
    checkCache.set(addKey, true)

    const annI = annTriage(LEAF, LEAF, LEAF)
    const D = fork(LEAF, NAT) // K(Nat)
    const ann = annS(D, annS(D, annK(mockAdd), annI), annI)

    const result = checkMemo(ann, arrow(NAT, NAT))

    console.log(`\nMemoized checking of double = S(S(K(add), I), I) : Nat → Nat`)
    console.log(`  Result: ${result}`)
    console.log(`  Check calls (actual work): ${checkCalls}`)
    console.log(`  Cache hits (free):         ${cacheHits}`)
    console.log(`  Total lookups:             ${checkCalls + cacheHits}`)
    console.log(``)
    console.log(`  The two I's (annI) have the same id=${annI.id}.`)
    console.log(`  First I checked against Nat→Nat: real work.`)
    console.log(`  Second I checked against Nat→Nat: cache hit on (${annI.id}, ${arrow(NAT, NAT).id}).`)
    console.log(``)
    console.log(`  Cache key is (annotated_tree.id, type.id) — just 2 integers.`)
    console.log(`  Hash-consing guarantees: same structure → same id → cache hit.`)

    expect(result).toBe(true)
    expect(cacheHits).toBeGreaterThan(0)
  })

  it("illustrate: annotated tree ids for the same program with different types", () => {
    const annI = annTriage(LEAF, LEAF, LEAF) // always id = I.id

    // Check I against multiple types — all use the SAME annotated tree
    // but pair with different type ids → different cache keys
    console.log(`\nSame annotated tree, different types:`)
    console.log(`  annI.id = ${annI.id}`)
    console.log(`  (Nat→Nat).id = ${arrow(NAT, NAT).id}`)
    console.log(`  (Bool→Bool).id = ${arrow(BOOL, BOOL).id}`)
    console.log(`  (Tree→Tree).id = ${arrow(TREE, TREE).id}`)
    console.log(`  Cache keys: (${annI.id},${arrow(NAT, NAT).id}), (${annI.id},${arrow(BOOL, BOOL).id}), (${annI.id},${arrow(TREE, TREE).id})`)
    console.log(`  Each type check is independent. First use computes, subsequent uses of same pair hit cache.`)
    console.log(``)
    console.log(`  Contrast with S nodes:`)
    const D1 = fork(LEAF, NAT)
    const D2 = fork(LEAF, BOOL)
    const ann1 = annS(D1, annK(LEAF), LEAF) // S with D=Nat
    const ann2 = annS(D2, annK(LEAF), LEAF) // S with D=Bool
    console.log(`  S(K(succ), succ) with D=Nat:  id=${ann1.id}`)
    console.log(`  S(K(succ), succ) with D=Bool: id=${ann2.id}`)
    console.log(`  Different ids! Hash-consing distinguishes typing contexts.`)
    console.log(`  The checker will never confuse one for the other.`)
    expect(ann1.id).not.toBe(ann2.id) // different D → different annotated tree → different id
  })
})

describe("Annotated format structure exhibit", () => {
  it("show the format for each combinator form", () => {
    console.log(`
┌─────────────────────────────────────────────────────────────────┐
│              ANNOTATED TREE FORMAT                              │
├───────────────┬────────────────────┬────────────────────────────┤
│ Combinator    │ Raw Program        │ Annotated                  │
├───────────────┼────────────────────┼────────────────────────────┤
│ K(v)          │ fork(leaf, v)      │ fork(leaf, ann_v)          │
│               │                    │ SAME STRUCTURE             │
├───────────────┼────────────────────┼────────────────────────────┤
│ S(c, b) + D   │ fork(stem(c), b)   │ fork(stem(D), fork(c, b)) │
│               │                    │ D replaces c in stem pos   │
│               │                    │ c, b packed in right fork  │
├───────────────┼────────────────────┼────────────────────────────┤
│ Triage(c,d,b) │ fork(fork(c,d), b) │ fork(fork(c, d), b)       │
│               │                    │ SAME STRUCTURE             │
├───────────────┼────────────────────┼────────────────────────────┤
│ leaf          │ leaf               │ leaf  (SAME)               │
│ stem(a)       │ stem(a)            │ stem(a)  (SAME)            │
├───────────────┴────────────────────┴────────────────────────────┤
│                                                                 │
│ Key insight: ONLY S nodes gain overhead. K, Triage, leaf, stem  │
│ are identical in both formats. The annotation cost is exactly   │
│ |D| per S node.                                                 │
│                                                                 │
│ Checker interface:                                              │
│   Raw:      check(program, type, derivation) → bool  (3 inputs)│
│   Annotated: check(annotated_tree, type) → bool      (2 inputs)│
│                                                                 │
│ The checker does ONE triage dispatch per node.                  │
│ No separate derivation tree. No lock-step traversal.            │
│ D is read directly from stem(D) in the S case.                 │
└─────────────────────────────────────────────────────────────────┘
`)
  })
})
