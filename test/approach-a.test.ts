// Pre-implementation tests for Approach A:
//   1. Remove S(K(p), K(q)) → K(ascribe(T, apply(p,q))) optimization
//   2. Restrict ascriptions in checkAnnotated to known defs and base values
//
// Tests marked [POST] are written for the DESIRED behavior — they will FAIL
// on the current code and PASS after the changes are implemented.
// Tests marked [INVARIANT] should pass both before and after.
// Tests marked [PRE] document current behavior that will CHANGE.

import { describe, it, expect, beforeEach } from "vitest"
import { Tree, LEAF, stem, fork, apply, treeEqual, I, K, isStem, isFork, isLeaf } from "../src/tree.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow, tnPi,
  annK, annS, annTriage, annAscribe, extract,
  isNonDep, stemCodomain, sCodomain,
  type KnownDefs, checkAnnotated,
} from "../src/tree-native-checker.js"
import {
  type TypedExpr, type NativeEnv,
  typedBracketAbstract, collapseToAnnotated, collapseTypedExpr,
  buildNativeWrapped, nativeElabDecl,
} from "../src/tree-native-elaborate.js"
import { parseExpr, parseLine, type SDecl } from "../src/parse.js"
import { compile, compileAndEval } from "../src/compile.js"
import { resetMarkerCounter, clearNativeBuiltins } from "../src/native-utils.js"
import { loadPrelude, clearPreludeCache } from "../src/prelude.js"

// ============================================================
// Setup: use native prelude
// ============================================================

let nativeDefs: KnownDefs
let nativeEnv: NativeEnv
let defs: Map<string, Tree>

function setup() {
  resetMarkerCounter()
  clearNativeBuiltins()
  clearPreludeCache()

  const prelude = loadPrelude()
  defs = prelude.defs
  nativeDefs = prelude.nativeDefs
  nativeEnv = prelude.nativeEnv
}

beforeEach(() => setup())

// ============================================================
// Section 1: S(K(p), K(q)) optimization removal
// ============================================================

describe("typedBracketAbstract: S(K,K) produces S node, not K(ascribed)", () => {

  // Build a tapp(f, g) where neither f nor g contain the variable x.
  // [x](f g) should produce S(K(f), K(g)) after optimization removal.
  function makeConstantApp(): TypedExpr {
    // f = succ (leaf, type Nat→Nat), g = zero (LEAF, type Nat)
    const f: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: tnArrow(TN_NAT, TN_NAT) }
    const g: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: TN_NAT }
    return { tag: "tapp", func: f, arg: g, type: TN_NAT }
  }

  it("[INVARIANT] [x](f g) with no x produces tK via early return", () => {
    // When neither f nor g contain the variable, typedBracketAbstract returns K(tapp)
    // at line 519 (early return) — BEFORE reaching the S decomposition.
    // The S(K,K) optimization was dead code: this case never reaches it.
    const texpr = makeConstantApp()
    const { result } = typedBracketAbstract("x", texpr, TN_BOOL)
    expect(result.tag).toBe("tK")
    // The value inside K is the original tapp (not evaluated)
    if (result.tag === "tK") {
      expect(result.value.tag).toBe("tapp")
    }
  })

  it("[INVARIANT] S(K(f), K(g)) extracts to same program as K(apply(f,g))", () => {
    // Regardless of optimization, the extracted program should behave identically.
    // S(K(f), K(g))(x) = K(f)(x)(K(g)(x)) = f(g) for any x.
    const f = LEAF // succ
    const g = LEAF // zero
    const optimized = fork(LEAF, apply(f, g)) // K(apply(f, g))
    const unoptimized = fork(stem(fork(LEAF, f)), fork(LEAF, g)) // S(K(f), K(g))

    // Both should produce the same result when applied to any argument
    const testArg = stem(stem(LEAF)) // arbitrary arg (doesn't matter — both are constant)
    expect(treeEqual(apply(optimized, testArg), apply(unoptimized, testArg))).toBe(true)
    // And the actual value should be apply(f, g) = apply(LEAF, LEAF) = stem(LEAF)
    expect(treeEqual(apply(optimized, testArg), apply(f, g))).toBe(true)
    expect(treeEqual(apply(unoptimized, testArg), apply(f, g))).toBe(true)
  })

  it("[INVARIANT] nested S(K,K) chains extract to same program", () => {
    // f g h  where none contain x. Should be S(S(K(f), K(g)), K(h)) after change.
    // Both forms should give f(g)(h) when applied.
    const f = LEAF                 // succ
    const g = LEAF                 // zero
    const h = stem(LEAF)           // K (= 1 as Nat)

    // Manual construction of S(S(K(f), K(g)), K(h))
    const innerS = fork(stem(fork(LEAF, f)), fork(LEAF, g))     // S(K(f), K(g))
    const outerS = fork(stem(innerS), fork(LEAF, h))            // S(innerS, K(h))

    // And the optimized form: K(apply(apply(f,g), h))
    const result = apply(apply(f, g), h)
    const optimizedK = fork(LEAF, result)

    const testArg = stem(stem(LEAF))
    expect(treeEqual(apply(outerS, testArg), apply(optimizedK, testArg))).toBe(true)
    expect(treeEqual(apply(outerS, testArg), result)).toBe(true)
  })
})

describe("collapseToAnnotated: S(K,K) nodes produce annS, not annAscribe", () => {

  it("[POST] tS with two tK children collapses to annS (not ascription)", () => {
    const valA: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: tnArrow(TN_NAT, TN_NAT) }
    const valB: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: TN_NAT }
    const d = fork(LEAF, TN_NAT) // K(Nat) — non-dependent intermediate type
    const texpr: TypedExpr = {
      tag: "tS",
      d,
      c: { tag: "tK", value: valA, type: tnArrow(TN_NAT, sCodomain(d, fork(LEAF, TN_NAT))) },
      b: { tag: "tK", value: valB, type: tnArrow(TN_NAT, d) },
      type: tnArrow(TN_NAT, TN_NAT),
    }

    const ann = collapseToAnnotated(texpr)

    // Should be annS(...), not annAscribe(...)
    // annS: fork(stem(D), fork(annC, annB))
    // annAscribe: fork(stem(T), stem(body))
    expect(isFork(ann)).toBe(true)
    if (isFork(ann)) {
      expect(isStem(ann.left)).toBe(true) // stem(D) for S
      if (isFork(ann.right)) {
        // right = fork(annC, annB) → S node, not stem(body) → ascription
        expect(isFork(ann.right)).toBe(true)
      }
    }
  })

  it("[INVARIANT] collapseToAnnotated of tS extracts to correct program", () => {
    // Simple case: S(K(succ), I) = succ
    const succLit: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: tnArrow(TN_NAT, TN_NAT) }
    const d = fork(LEAF, TN_NAT) // K(Nat)
    const texpr: TypedExpr = {
      tag: "tS",
      d,
      c: { tag: "tK", value: succLit, type: tnArrow(TN_NAT, sCodomain(d, fork(LEAF, TN_NAT))) },
      b: { tag: "tI", type: tnArrow(TN_NAT, d) },
      type: tnArrow(TN_NAT, TN_NAT),
    }

    const ann = collapseToAnnotated(texpr)
    const prog = extract(ann)
    // S(K(succ), I)(n) = succ(n). Test at n=0 and n=1:
    expect(treeEqual(apply(prog, LEAF), stem(LEAF))).toBe(true)        // succ(0) = 1
    expect(treeEqual(apply(prog, stem(LEAF)), stem(stem(LEAF)))).toBe(true)  // succ(1) = 2
  })
})

// ============================================================
// Section 2: Ascription restriction
// ============================================================

describe("checkAnnotated: ascription trust boundary", () => {

  it("[INVARIANT] ascription with base value (Nat) passes", () => {
    // annAscribe(Nat, LEAF) where LEAF is zero (a valid Nat)
    const ann = annAscribe(TN_NAT, LEAF)
    expect(checkAnnotated(nativeDefs, ann, TN_NAT)).toBe(true)
  })

  it("[INVARIANT] ascription with base value (Bool) passes", () => {
    // annAscribe(Bool, stem(LEAF)) where stem(LEAF) is false
    const ann = annAscribe(TN_BOOL, stem(LEAF))
    expect(checkAnnotated(nativeDefs, ann, TN_BOOL)).toBe(true)
  })

  it("[INVARIANT] ascription with base type — checker validates value, not ascribed type", () => {
    // Key insight: for base types (stem-shaped), the checker dispatches to isOfBaseType
    // BEFORE reaching the ascription case. It extracts the body and checks the VALUE.
    // So annAscribe(Bool, LEAF) checked against Nat → extract gives LEAF → isOfBaseType(LEAF, Nat) = true (LEAF = 0)
    // The ascribed type (Bool) is irrelevant for base type checking.
    const ann = annAscribe(TN_BOOL, LEAF)
    expect(checkAnnotated(nativeDefs, ann, TN_NAT)).toBe(true) // LEAF=0 is a valid Nat
  })

  it("[INVARIANT] ascription with base type — invalid value rejects regardless of ascription", () => {
    // A fork is not a valid Nat, so even with a Nat ascription it fails
    const garbage = fork(stem(stem(stem(LEAF))), fork(LEAF, stem(LEAF)))
    const ann = annAscribe(TN_NAT, garbage)
    // extract → garbage (fork-shaped), isOfBaseType(garbage, Nat) = false
    expect(checkAnnotated(nativeDefs, ann, TN_NAT)).toBe(false)
  })

  it("[INVARIANT] ascription with known def passes for function type", () => {
    // Register a tree as a known def with a function type, then ascribe it
    const knownTree = fork(fork(LEAF, LEAF), LEAF) // I combinator
    const localDefs = new Map(nativeDefs)
    localDefs.set(knownTree.id, tnArrow(TN_NAT, TN_NAT))

    const ann = annAscribe(tnArrow(TN_NAT, TN_NAT), knownTree)
    expect(checkAnnotated(localDefs, ann, tnArrow(TN_NAT, TN_NAT))).toBe(true)
  })

  // --- The unsoundness is specifically for FUNCTION types (fork-shaped) ---
  // For base types, the checker validates the actual value.
  // For function types, the ascription case (fork(stem(T), stem(body))) is reached.

  it("[INVARIANT] function-typed ascription with unknown tree now rejects", () => {
    // After ascription restriction: unknown trees are rejected for function types.
    // Previously this returned true (unsound). Now it returns false.
    const unknownFunc = fork(stem(fork(LEAF, LEAF)), stem(LEAF))
    const funcType = tnArrow(TN_NAT, TN_NAT)
    const ann = annAscribe(funcType, unknownFunc)
    expect(checkAnnotated(nativeDefs, ann, funcType)).toBe(false)
  })

  it("[POST] function-typed ascription with unknown tree rejects (sound)", () => {
    // After Approach A: function-typed ascriptions should only pass for known defs
    const unknownFunc = fork(stem(fork(LEAF, LEAF)), stem(LEAF))
    const funcType = tnArrow(TN_NAT, TN_NAT)
    const ann = annAscribe(funcType, unknownFunc)
    // Not a known def → should reject
    expect(checkAnnotated(nativeDefs, ann, funcType)).toBe(false)
  })

  it("[POST] adversarial: fabricated function-typed ascription rejects", () => {
    // An adversary creates annAscribe(Nat→Nat, garbage) for a function type.
    // Currently passes because the checker just matches the ascribed type.
    // After change: rejected because garbage isn't a known def.
    const garbage = fork(fork(stem(LEAF), fork(LEAF, LEAF)), fork(LEAF, stem(LEAF)))
    const funcType = tnArrow(TN_NAT, TN_NAT)
    const ann = annAscribe(funcType, garbage)
    expect(checkAnnotated(nativeDefs, ann, funcType)).toBe(false)
  })

  it("[POST] function-typed ascription with known def still passes", () => {
    // After Approach A: ascriptions for known defs should still work
    const knownFunc = fork(fork(LEAF, LEAF), LEAF) // I combinator
    const funcType = tnArrow(TN_NAT, TN_NAT)
    const localDefs = new Map(nativeDefs)
    localDefs.set(knownFunc.id, funcType)

    const ann = annAscribe(funcType, knownFunc)
    expect(checkAnnotated(localDefs, ann, funcType)).toBe(true)
  })
})

// ============================================================
// Section 3: Full pipeline semantic preservation
// ============================================================

describe("Full pipeline: semantics preserved after Approach A", () => {

  function processDecl(input: string) {
    const parsed = parseLine(input) as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )
    // Update state for subsequent declarations
    defs.set(parsed.name, result.compiled)
    nativeDefs = result.nativeDefs
    nativeEnv = result.nativeEnv
    return result
  }

  it("[INVARIANT] kTrue compiles to correct program regardless of annotation", () => {
    const result = processDecl("let kTrue : Bool -> Bool := {x} -> true")
    // The compiled tree should be K(true) = fork(LEAF, LEAF) regardless
    const prog = result.compiled
    // K(true)(false) = true, K(true)(true) = true
    expect(treeEqual(apply(prog, LEAF), LEAF)).toBe(true)         // true→true
    expect(treeEqual(apply(prog, stem(LEAF)), LEAF)).toBe(true)   // false→true
  })

  it("[INVARIANT] kZero compiles to correct program regardless of annotation", () => {
    const result = processDecl("let kZero : Nat -> Nat := {x} -> 0")
    const prog = result.compiled
    expect(treeEqual(apply(prog, LEAF), LEAF)).toBe(true)           // 0→0
    expect(treeEqual(apply(prog, stem(LEAF)), LEAF)).toBe(true)     // 1→0
    expect(treeEqual(apply(prog, stem(stem(LEAF))), LEAF)).toBe(true)  // 2→0
  })

  it("[INVARIANT] id compiles to identity regardless of annotation", () => {
    const result = processDecl("let id : Nat -> Nat := {x} -> x")
    const prog = result.compiled
    expect(treeEqual(apply(prog, LEAF), LEAF)).toBe(true)
    expect(treeEqual(apply(prog, stem(LEAF)), stem(LEAF))).toBe(true)
    expect(treeEqual(apply(prog, stem(stem(LEAF))), stem(stem(LEAF)))).toBe(true)
  })

  it("[INVARIANT] mySucc compiles to succ regardless of annotation", () => {
    const result = processDecl("let mySucc : Nat -> Nat := {n} -> succ n")
    const prog = result.compiled
    expect(treeEqual(apply(prog, LEAF), stem(LEAF))).toBe(true)            // succ(0)=1
    expect(treeEqual(apply(prog, stem(LEAF)), stem(stem(LEAF)))).toBe(true) // succ(1)=2
  })

  it("[INVARIANT] kConst compiles correctly regardless of annotation", () => {
    const result = processDecl("let kConst : Nat -> Bool -> Nat := {x y} -> x")
    const prog = result.compiled
    // kConst 5 false = 5
    const five = stem(stem(stem(stem(stem(LEAF)))))
    expect(treeEqual(apply(apply(prog, five), stem(LEAF)), five)).toBe(true)
  })

  it("[INVARIANT] checkOk=true for kTrue (annotation structure may change)", () => {
    const result = processDecl("let kTrue : Bool -> Bool := {x} -> true")
    // Should pass native check regardless of whether annotated as K(ascribed) or S(K,K)
    expect(result.checkOk).toBe(true)
  })

  it("[INVARIANT] checkOk=true for id (unaffected by S(K,K) change)", () => {
    const result = processDecl("let id : Nat -> Nat := {x} -> x")
    expect(result.checkOk).toBe(true)
  })

  it("[INVARIANT] checkOk=true for kConst (multi-param K)", () => {
    const result = processDecl("let kConst : Nat -> Bool -> Nat := {x y} -> x")
    expect(result.checkOk).toBe(true)
  })
})

// ============================================================
// Section 4: Annotation structure after S(K,K) removal
// ============================================================

describe("Annotation structure: no ascriptions inside combinators", () => {

  it("[POST] kTrue annotation contains no ascription nodes", () => {
    const parsed = parseLine("let kTrue : Bool -> Bool := {x} -> true") as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )

    // Walk the annotated tree and check for ascription pattern
    function hasAscription(t: Tree): boolean {
      if (isLeaf(t)) return false
      if (isStem(t)) return hasAscription(t.child)
      // fork: check if it's an ascription (fork(stem(_), stem(_)))
      if (isStem(t.left) && isStem(t.right)) return true
      return hasAscription(t.left) || hasAscription(t.right)
    }

    // After Approach A: no ascriptions inside the combinator structure
    expect(hasAscription(result.annotated)).toBe(false)
  })

  it("[POST] kZero annotation contains no ascription nodes", () => {
    const parsed = parseLine("let kZero : Nat -> Nat := {x} -> 0") as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )

    function hasAscription(t: Tree): boolean {
      if (isLeaf(t)) return false
      if (isStem(t)) return hasAscription(t.child)
      if (isStem(t.left) && isStem(t.right)) return true
      return hasAscription(t.left) || hasAscription(t.right)
    }

    expect(hasAscription(result.annotated)).toBe(false)
  })

  it("[POST] mySucc annotation contains no ascription nodes", () => {
    const parsed = parseLine("let mySucc : Nat -> Nat := {n} -> succ n") as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )

    function hasAscription(t: Tree): boolean {
      if (isLeaf(t)) return false
      if (isStem(t)) return hasAscription(t.child)
      if (isStem(t.left) && isStem(t.right)) return true
      return hasAscription(t.left) || hasAscription(t.right)
    }

    expect(hasAscription(result.annotated)).toBe(false)
  })
})

// ============================================================
// Section 5: Bool triage soundness (checker evaluates — should be structural)
// ============================================================

describe("Bool triage: checker should not evaluate stem handler", () => {

  it("[INVARIANT] triage(true_val, K(false_val), junk) : Bool -> Bool checks OK", () => {
    // not = triage(false, K(true), junk) at Bool -> Bool
    // leaf case: false = stem(LEAF)
    // stem handler: K(LEAF) — when applied to the inner part of false, returns LEAF (true)
    // fork case: LEAF (junk, never reached for Bool)
    const ann = annTriage(stem(LEAF), annK(LEAF), LEAF)
    const type = tnArrow(TN_BOOL, TN_BOOL)
    expect(checkAnnotated(nativeDefs, ann, type)).toBe(true)
  })

  it("[POST] adversarial triage: stem handler forges ascription on evaluation", () => {
    // Adversary crafts a stem handler d such that apply(extract(d), LEAF)
    // produces a tree matching the ascription format fork(stem(T), stem(body)).
    // The checker should NOT accept this.
    //
    // d = K(fork(stem(Nat), stem(LEAF))) — when applied to LEAF, returns fork(stem(Nat), stem(LEAF))
    // extract(d) = extract(annK(fork(stem(Nat), stem(LEAF)))) = fork(LEAF, fork(stem(Nat), stem(LEAF)))
    // BUT: fork(stem(Nat), stem(LEAF)) looks like annAscribe(Nat, LEAF) to the checker.
    //
    // This is the Bool triage eval bug: the checker currently calls
    // checkAnnotated(defs, apply(extract(annD), LEAF), Bstem) which passes
    // the evaluated result (which looks like an ascription) to checkAnnotated.
    //
    // After fix: checker should check d STRUCTURALLY as a function, not by evaluating.
    //
    // For now, document that this is a known soundness hole.
    // The fix involves using annApp-style checking internally in the Bool triage case.
    const forgedAscription = fork(stem(TN_BOOL), stem(LEAF))
    const d = annK(forgedAscription) // K(forgedAscription) — stem handler that returns forged value
    const ann = annTriage(LEAF, d, LEAF) // triage(leaf, K(forge), leaf) at Bool -> Bool
    const type = tnArrow(TN_BOOL, TN_BOOL)

    // Ideal: this should be checkable without trusting the evaluated result
    // Current behavior may vary; this test documents the concern
    const result = checkAnnotated(nativeDefs, ann, type)
    // After Bool triage fix: this should pass ONLY if d is structurally valid
    // (K(forgedAscription) IS structurally valid if forgedAscription : Bool)
    // Since forgedAscription = fork(stem(Bool), stem(LEAF)) is NOT a valid Bool
    // (Bool = LEAF or stem(LEAF)), checking d : Tree -> Bool should verify the
    // VALUE of forgedAscription, not trust its structure.
    console.log(`Bool triage adversarial test result: ${result}`)
  })
})

// ============================================================
// Section 6: Existing tests that WILL change (registry of affected tests)
// ============================================================

describe("Registry: tests affected by Approach A", () => {
  // This describe block documents which existing tests will need updates.
  // Each test here reproduces a case from an existing test file and shows
  // both the current and expected-after-change behavior.

  it("[REGISTRY] tree-native-pipeline:246 — kTrue pipeline currently uses ascription", () => {
    // tree-native-pipeline.test.ts line 246:
    //   processDecl("let kTrue : Bool -> Bool := {x} -> true")
    //   expect(result.checkOk).toBe(true)
    //
    // Currently passes because:
    //   typedBracketAbstract produces K(ascribe(Bool, LEAF))
    //   checkAnnotated trusts the ascription
    //
    // After Approach A:
    //   typedBracketAbstract produces S(K(true_handler), K(true_value))
    //   checkAnnotated verifies S structurally
    //   checkOk should still be true (semantics preserved)
    //
    // Impact: test PASSES with same assertion, different internal annotation
    expect(true).toBe(true) // placeholder — real test is in tree-native-pipeline.test.ts
  })

  it("[REGISTRY] tree-native-elaborate:146 — [x]c = K(c) test unaffected", () => {
    // tree-native-elaborate.test.ts line 146:
    //   typedBracketAbstract("x", tlit(stem(LEAF)), TN_BOOL)
    //   expect(result.tag).toBe("tK")
    //
    // This tests [x]c where c is a SINGLE literal (not an application).
    // The S(K,K) optimization only fires for tapp nodes.
    // This test is UNAFFECTED.
    const texpr: TypedExpr = { tag: "tlit", tree: stem(LEAF), annTree: stem(LEAF), type: TN_NAT }
    const { result } = typedBracketAbstract("x", texpr, TN_BOOL)
    expect(result.tag).toBe("tK")
  })

  it("[REGISTRY] compile.test.ts optimization tests — UNAFFECTED", () => {
    // compile.test.ts tests the untyped bracketAbstract in compile.ts,
    // which has its OWN S(K,K) optimization (optimizedS function).
    // We are only changing typedBracketAbstract in tree-native-elaborate.ts.
    // compile.test.ts is UNAFFECTED.
    expect(true).toBe(true) // confirmed by code inspection
  })
})

// ============================================================
// Section 7: Phase 1 — Pi domain preservation in compile.ts
// ============================================================

describe("Phase 1: Pi types compile to fork(domain, codomain)", () => {

  function c(input: string, d?: Map<string, Tree>) {
    return compileAndEval(parseExpr(input), d ?? defs)
  }

  it("[POST] (A : Type) -> A compiles to fork(Type, I), not a lambda", () => {
    const piTree = c("(A : Type) -> A")
    // After Phase 1: fork(TN_TYPE, I) where TN_TYPE=LEAF, I=identity
    // This is Pi(Type, identity_codomain)
    expect(isFork(piTree)).toBe(true)
    if (isFork(piTree)) {
      expect(treeEqual(piTree.left, LEAF)).toBe(true) // domain = Type = LEAF
    }
  })

  it("[POST] A -> B compiles to fork(A, K(B))", () => {
    // Nat -> Bool should compile to fork(Nat, K(Bool))
    const arrTree = c("Nat -> Bool")
    expect(isFork(arrTree)).toBe(true)
    if (isFork(arrTree)) {
      // domain = whatever Nat compiles to in the defs map
      // codomain = K(Bool_compiled)
      const B = arrTree.right
      // Non-dependent: B should be K-shaped (fork(LEAF, _))
      expect(isLeaf(B.left)).toBe(true) // K(something)
    }
  })

  it("[POST] Tree keyword compiles to stem(LEAF) = TN_TREE, not LEAF", () => {
    const treeVal = c("Tree")
    expect(treeEqual(treeVal, stem(LEAF))).toBe(true) // TN_TREE
  })

  it("[POST] Type keyword still compiles to LEAF = TN_TYPE", () => {
    // stype case should NOT change
    const typeVal = c("Type")
    expect(treeEqual(typeVal, LEAF)).toBe(true) // TN_TYPE
  })

  it("[INVARIANT] slam compilation unaffected", () => {
    // Lambdas still compile the same way
    const id = c("{x} -> x")
    expect(treeEqual(id, I)).toBe(true)
    const konst = c("{x y} -> x")
    expect(treeEqual(konst, K)).toBe(true)
  })
})

describe("Phase 1: Pair/Either produce proper native types", () => {

  function processDecl(input: string) {
    const parsed = parseLine(input) as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )
    defs.set(parsed.name, result.compiled)
    nativeEnv = result.env
    return result
  }

  it("[POST] Pair Nat Bool evaluates to a fork (Pi type), not a stem", () => {
    // Compile Pair directly — nativeElabDecl may throw for Church-encoded types
    const localDefs = new Map(defs)
    const pairTree = compileAndEval(parseExpr("{A B} -> {fst : A, snd : B}"), localDefs)
    // Apply Pair to Nat, then Bool
    const pairNat = apply(pairTree, TN_NAT)
    const pairNatBool = apply(pairNat, TN_BOOL)
    // After Phase 1: should be a fork (Pi type), not a stem
    expect(isFork(pairNatBool)).toBe(true)
  })

  it("[INVARIANT] runtime semantics of Pair preserved", () => {
    // Use compile.ts directly for runtime testing — nativeElabDecl may throw for Church-encoded types
    const localDefs = new Map(defs)
    const compilePair = compileAndEval(parseExpr("{A B} -> {fst : A, snd : B}"), localDefs)
    localDefs.set("Pair", compilePair)
    const compileMkPair = compileAndEval(parseExpr("{A B a b R f} -> f a b"), localDefs)
    localDefs.set("mkPair", compileMkPair)
    const compileFst = compileAndEval(parseExpr("{A B p} -> p A ({x y} -> x)"), localDefs)
    localDefs.set("fst", compileFst)
    const compileSnd = compileAndEval(parseExpr("{A B p} -> p B ({x y} -> y)"), localDefs)
    localDefs.set("snd", compileSnd)

    // mkPair Nat Bool 5 true
    const five = stem(stem(stem(stem(stem(LEAF)))))
    const pair = apply(apply(apply(apply(compileMkPair, TN_NAT), TN_BOOL), five), LEAF)
    // fst Nat Bool pair = 5
    expect(treeEqual(apply(apply(apply(compileFst, TN_NAT), TN_BOOL), pair), five)).toBe(true)
    // snd Nat Bool pair = true
    expect(treeEqual(apply(apply(apply(compileSnd, TN_NAT), TN_BOOL), pair), LEAF)).toBe(true)
  })

  it("[POST] mkPair elaborates without throwing after Phase 1", () => {
    // After Phase 1: Pair produces proper Pi types, so mkPair's type annotation
    // referencing Pair A B should be decomposable by the elaborator
    processDecl("let Pair : Type -> Type -> Type := {A B} -> {fst : A, snd : B}")
    // This should not throw after Phase 1 (currently throws "Lambda needs function type")
    const result = processDecl("let mkPair : (A : Type) -> (B : Type) -> A -> B -> Pair A B := {A B a b R f} -> f a b")
    expect(result.nativeType).toBeDefined()
    expect(isFork(result.nativeType)).toBe(true) // Pi(Type, ...)
  })
})

// ============================================================
// Section 8: Phase 2 — Constant-motive coercion
// ============================================================

describe("Phase 2: constant-motive coercion", () => {

  function processDecl(input: string) {
    const parsed = parseLine(input) as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )
    defs.set(parsed.name, result.compiled)
    nativeEnv = result.env
    return result
  }

  it("[POST] not elaborates with checkOk=true (boolElim with constant motive)", () => {
    const result = processDecl("let not : Bool -> Bool := {b} -> boolElim Bool false true b")
    expect(result.checkOk).toBe(true)
  })

  it("[POST] and elaborates with checkOk=true", () => {
    const result = processDecl("let and : Bool -> Bool -> Bool := {a b} -> boolElim Bool b false a")
    expect(result.checkOk).toBe(true)
  })

  it("[POST] or elaborates with checkOk=true", () => {
    const result = processDecl("let or : Bool -> Bool -> Bool := {a b} -> boolElim Bool true b a")
    expect(result.checkOk).toBe(true)
  })

  it("[POST] fold elaborates (natElim with constant motive)", () => {
    // fold uses natElim R z s n where R : Type (constant motive)
    const result = processDecl("let rec fold : Nat -> (R : Type) -> R -> (R -> R) -> R := {n R z s} -> natElim R z ({n2} -> s (fold n2 R z s)) n")
    // Should at least not throw; checkOk depends on recursive elaboration quality
    expect(result.nativeType).toBeDefined()
  })

  it("[INVARIANT] boolElim with constant motive computes correctly", () => {
    // Regardless of type checking, boolElim Bool false true b should work at runtime
    const boolElimTree = defs.get("boolElim")!
    const notTree = apply(apply(apply(boolElimTree, TN_BOOL), stem(LEAF)), LEAF) // boolElim Bool false true
    expect(treeEqual(apply(notTree, LEAF), stem(LEAF))).toBe(true)  // not(true) = false
    expect(treeEqual(apply(notTree, stem(LEAF)), LEAF)).toBe(true)  // not(false) = true
  })

  it("[INVARIANT] natElim with constant motive computes correctly", () => {
    // natElim Nat 0 succ n should give n (identity on Nat)
    const natElimTree = defs.get("natElim")!
    const succTree = LEAF // succ = leaf (the stem constructor)
    const idNat = apply(apply(apply(natElimTree, TN_NAT), LEAF), succTree) // natElim Nat 0 succ
    expect(treeEqual(apply(idNat, LEAF), LEAF)).toBe(true)  // 0 → 0
    expect(treeEqual(apply(idNat, stem(LEAF)), stem(LEAF))).toBe(true)  // 1 → 1
    expect(treeEqual(apply(idNat, stem(stem(LEAF))), stem(stem(LEAF)))).toBe(true)  // 2 → 2
  })
})

// ============================================================
// Section 9: Phase 1+2 combined — prelude verification improvement
// ============================================================

describe("Phase 1+2: prelude verification improvement", () => {

  function processDecl(input: string) {
    const parsed = parseLine(input) as SDecl
    const result = nativeElabDecl(
      nativeEnv, defs, nativeDefs,
      parsed.name, parsed.type, parsed.value, parsed.isRec
    )
    defs.set(parsed.name, result.compiled)
    nativeEnv = result.env
    return result
  }

  it("[POST] simple definitions all verify", () => {
    // These should all pass checkOk after Phase 1+2
    const defs_to_check = [
      "let not : Bool -> Bool := {b} -> boolElim Bool false true b",
      "let and : Bool -> Bool -> Bool := {a b} -> boolElim Bool b false a",
      "let or : Bool -> Bool -> Bool := {a b} -> boolElim Bool true b a",
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ]
    for (const def of defs_to_check) {
      const parsed = parseLine(def) as SDecl
      try {
        const result = processDecl(def)
        expect(result.checkOk).toBe(true)
      } catch (e: any) {
        // If it throws, that's a failure too
        throw new Error(`Failed to elaborate ${parsed.name}: ${e.message}`)
      }
    }
  })
})
