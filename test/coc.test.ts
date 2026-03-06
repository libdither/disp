import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, treeEqual, apply, I, prettyTree } from "../src/tree.js"
import {
  encType, encVar, encApp, encLam, encPi,
  termTag, unVar, unApp, unLam, unPi,
  wrap, unwrapData, unwrapType, K_SEL, K_STAR_SEL,
  freshMarker, resetMarkerCounter,
  treeToExprReplacing, abstractMarkerOut,
  whnfTree, normalize, convertible, convertibleUnderBinder,
  buildWrapped, cocCheckDecl, cocCheckRecDecl,
  printEncoded, loadCocPrelude, buildNameMap,
  FST, SND, CHILD, ENC_APP_T, ENC_LAM_T, ENC_PI_T,
  TERM_CASE,
  CocError, type Env,
} from "../src/coc.js"
import { eTree, eFvar, eApp, bracketAbstract, collapse } from "../src/compile.js"
import { parseExpr, parseLine, type SExpr, type SDecl } from "../src/parse.js"

function declEnv(decls: string[], env: Env = new Map()): Env {
  for (const d of decls) {
    const parsed = parseLine(d)
    if (!("name" in parsed)) throw new Error("Expected declaration")
    const decl = parsed as SDecl
    const result = cocCheckDecl(env, decl.name, decl.type, decl.value, decl.isRec)
    env = result.env
  }
  return env
}

beforeEach(() => {
  resetMarkerCounter()
})

// ============================================================
// Phase 1: Encoding Primitives
// ============================================================

describe("Phase 1: Encoding Primitives", () => {
  it("encType returns LEAF", () => {
    expect(treeEqual(encType(), LEAF)).toBe(true)
  })

  it("encVar creates stem(marker)", () => {
    const m = stem(stem(LEAF))
    const v = encVar(m)
    expect(v.tag).toBe("stem")
    expect(treeEqual(v, stem(m))).toBe(true)
  })

  it("encApp creates correct structure", () => {
    const m = LEAF
    const n = stem(LEAF)
    const app = encApp(m, n)
    expect(app.tag).toBe("fork")
    if (app.tag === "fork") {
      expect(treeEqual(app.left, LEAF)).toBe(true)
      expect(app.tag).toBe("fork")
    }
  })

  it("encLam creates correct structure", () => {
    const dom = LEAF
    const body = stem(LEAF)
    const lam = encLam(dom, body)
    expect(lam.tag).toBe("fork")
    if (lam.tag === "fork") {
      expect(lam.left.tag).toBe("stem")
    }
  })

  it("encPi creates correct structure", () => {
    const dom = LEAF
    const body = stem(LEAF)
    const pi = encPi(dom, body)
    expect(pi.tag).toBe("fork")
    if (pi.tag === "fork") {
      expect(pi.left.tag).toBe("fork")
      if (pi.left.tag === "fork") {
        expect(treeEqual(pi.left.right, LEAF)).toBe(true)
      }
    }
  })

  it("termTag correctly identifies all 5 tags", () => {
    expect(termTag(encType())).toBe("type")
    expect(termTag(encVar(LEAF))).toBe("var")
    expect(termTag(encApp(LEAF, LEAF))).toBe("app")
    expect(termTag(encLam(LEAF, LEAF))).toBe("lam")
    expect(termTag(encPi(LEAF, LEAF))).toBe("pi")
  })

  it("destructuring round-trips with encoding", () => {
    const m = stem(LEAF)
    expect(treeEqual(unVar(encVar(m))!, m)).toBe(true)

    const app = unApp(encApp(LEAF, m))!
    expect(treeEqual(app.func, LEAF)).toBe(true)
    expect(treeEqual(app.arg, m)).toBe(true)

    const lam = unLam(encLam(LEAF, m))!
    expect(treeEqual(lam.domain, LEAF)).toBe(true)
    expect(treeEqual(lam.body, m)).toBe(true)

    const pi = unPi(encPi(LEAF, m))!
    expect(treeEqual(pi.domain, LEAF)).toBe(true)
    expect(treeEqual(pi.body, m)).toBe(true)
  })

  it("wrap/unwrapData/unwrapType round-trips", () => {
    const data = encApp(LEAF, stem(LEAF))
    const type = encPi(LEAF, LEAF)

    const wrapped = wrap(data, type)
    const gotData = unwrapData(wrapped)
    const gotType = unwrapType(wrapped)

    expect(treeEqual(gotData, data)).toBe(true)
    expect(treeEqual(gotType, type)).toBe(true)
  })

  it("freshMarker generates distinct markers", () => {
    const m1 = freshMarker()
    const m2 = freshMarker()
    const m3 = freshMarker()
    expect(treeEqual(m1, m2)).toBe(false)
    expect(treeEqual(m2, m3)).toBe(false)
    expect(treeEqual(m1, m3)).toBe(false)

    // Markers should be fork-based (disjoint from encodings)
    expect(m1.tag).toBe("fork")
    expect(m2.tag).toBe("fork")
  })
})

// ============================================================
// Phase 2: Bracket Abstraction Integration
// ============================================================

describe("Phase 2: Bracket Abstraction Integration", () => {
  it("variable in encApp: [x]encApp(x, y) applied to arg gives encApp(arg, y)", () => {
    const m = freshMarker()
    const y = encVar(freshMarker())
    const arg = encType()

    // Build encApp(marker, y), then abstract out marker
    const appTree = encApp(encVar(m), y)
    const abstracted = abstractMarkerOut(appTree, encVar(m))

    // Apply to arg
    const result = apply(abstracted, arg)
    const expected = encApp(arg, y)
    expect(treeEqual(result, expected)).toBe(true)
  })

  it("variable in encLam domain: [x]encLam(x, body) applied to arg gives encLam(arg, body)", () => {
    const m = freshMarker()
    const body = stem(LEAF)
    const arg = encPi(LEAF, LEAF)

    const lamTree = encLam(encVar(m), body)
    const abstracted = abstractMarkerOut(lamTree, encVar(m))
    const result = apply(abstracted, arg)
    const expected = encLam(arg, body)
    expect(treeEqual(result, expected)).toBe(true)
  })

  it("identity body: [x]x = I combinator", () => {
    const m = freshMarker()
    const neutral = encVar(m)
    const abstracted = abstractMarkerOut(neutral, neutral)
    const arg = encApp(LEAF, LEAF)
    const result = apply(abstracted, arg)
    expect(treeEqual(result, arg)).toBe(true)
  })

  it("constant body: [x]c = K(c)", () => {
    const m = freshMarker()
    const c = encPi(LEAF, stem(LEAF))
    const abstracted = abstractMarkerOut(c, encVar(m))
    const arg = encApp(LEAF, LEAF)
    const result = apply(abstracted, arg)
    expect(treeEqual(result, c)).toBe(true)
  })

  it("variable used twice: [x]encApp(x, x) applied to arg gives encApp(arg, arg)", () => {
    const m = freshMarker()
    const neutral = encVar(m)
    const appTree = encApp(neutral, neutral)
    const abstracted = abstractMarkerOut(appTree, neutral)
    const arg = encType()
    const result = apply(abstracted, arg)
    const expected = encApp(arg, arg)
    expect(treeEqual(result, expected)).toBe(true)
  })

  it("nested encoding: variable inside inner lambda domain", () => {
    const m = freshMarker()
    const neutral = encVar(m)
    // encLam(x, LEAF) where domain contains the marker
    const tree = encLam(neutral, LEAF)
    const abstracted = abstractMarkerOut(tree, neutral)
    const arg = encPi(LEAF, LEAF)
    const result = apply(abstracted, arg)
    expect(treeEqual(result, encLam(arg, LEAF))).toBe(true)
  })
})

// ============================================================
// Phase 3: WHNF, Convertibility, Core Building
// ============================================================

describe("Phase 3: WHNF", () => {
  it("Type is in WHNF", () => {
    const t = encType()
    expect(treeEqual(whnfTree(t), t)).toBe(true)
  })

  it("Var is in WHNF", () => {
    const t = encVar(LEAF)
    expect(treeEqual(whnfTree(t), t)).toBe(true)
  })

  it("Lam is in WHNF", () => {
    const t = encLam(LEAF, I)
    expect(treeEqual(whnfTree(t), t)).toBe(true)
  })

  it("Pi is in WHNF", () => {
    const t = encPi(LEAF, I)
    expect(treeEqual(whnfTree(t), t)).toBe(true)
  })

  it("App(Lam(A, I), x) reduces to x (beta reduction)", () => {
    const x = encVar(freshMarker())
    const t = encApp(encLam(LEAF, I), x)
    const result = whnfTree(t)
    expect(treeEqual(result, x)).toBe(true)
  })

  it("stuck application stays stuck", () => {
    const x = encVar(freshMarker())
    const y = encVar(freshMarker())
    const t = encApp(x, y)
    const result = whnfTree(t)
    expect(treeEqual(result, t)).toBe(true)
  })

  it("chained beta: App(App(Lam(_, Lam(_, I)), a), b) reduces to b", () => {
    const a = encVar(freshMarker())
    const b = encVar(freshMarker())
    // Inner lambda: {_} -> body = I (returns its argument)
    // Outer lambda: {_} -> innerLam
    // We need to build [x][y]y
    // [y]y = I
    // [x]I = K(I) -- since I is constant wrt x
    const innerBody = I  // [y]y
    const outerBody = abstractMarkerOut(encLam(LEAF, innerBody), encVar(freshMarker()))
    // Actually let's build it properly:
    // encLam(LEAF, K(encLam(LEAF, I))) where K = bracket abstraction returning constant
    // Simpler: just apply twice
    const twiceAbstracted = encLam(LEAF, collapse(bracketAbstract("__unused__", eTree(encLam(LEAF, I)))))
    const t = encApp(encApp(twiceAbstracted, a), b)
    const result = whnfTree(t)
    expect(treeEqual(result, b)).toBe(true)
  })
})

describe("Phase 3: Convertibility", () => {
  it("identical terms (O(1) via hash-consing)", () => {
    const t = encApp(encVar(LEAF), encType())
    expect(convertible(t, t)).toBe(true)
  })

  it("beta-convertible: App(Lam(_, I), y) ≡ y", () => {
    const y = encVar(freshMarker())
    const app = encApp(encLam(LEAF, I), y)
    expect(convertible(app, y)).toBe(true)
  })

  it("different variables not convertible", () => {
    const x = encVar(freshMarker())
    const y = encVar(freshMarker())
    expect(convertible(x, y)).toBe(false)
  })

  it("Pi types with convertible components", () => {
    const dom = encType()
    const body = I
    const pi1 = encPi(dom, body)
    const pi2 = encPi(dom, body)
    expect(convertible(pi1, pi2)).toBe(true)
  })

  it("bodies compared under fresh binder", () => {
    // Two lambdas that are the same function: [x]x
    const lam1 = encLam(LEAF, I)
    const lam2 = encLam(LEAF, I)
    expect(convertible(lam1, lam2)).toBe(true)
  })
})

describe("Phase 3: buildWrapped basics", () => {
  it("Type -> wrap(LEAF, LEAF)", () => {
    const env: Env = new Map()
    const result = buildWrapped(parseExpr("Type"), env)
    expect(treeEqual(unwrapData(result), encType())).toBe(true)
    expect(treeEqual(unwrapType(result), encType())).toBe(true)
  })

  it("variable lookup returns wrapped value from env", () => {
    const env: Env = new Map()
    const valData = encApp(LEAF, LEAF)
    const valType = encType()
    env.set("x", wrap(valData, valType))

    const result = buildWrapped(parseExpr("x"), env)
    expect(treeEqual(unwrapData(result), valData)).toBe(true)
    expect(treeEqual(unwrapType(result), valType)).toBe(true)
  })

  it("unbound variable throws CocError", () => {
    expect(() => buildWrapped(parseExpr("x"), new Map())).toThrow(CocError)
  })

  it("(A : Type) -> Type produces Pi with type Type", () => {
    const env: Env = new Map()
    const result = buildWrapped(parseExpr("(A : Type) -> Type"), env)
    const data = unwrapData(result)
    const type = unwrapType(result)
    expect(termTag(data)).toBe("pi")
    expect(treeEqual(type, encType())).toBe(true)
  })

  it("{x} -> x checked against (A : Type) -> A produces identity", () => {
    const env: Env = new Map()
    // Build the expected type: (A : Type) -> A
    const piType = unwrapData(buildWrapped(parseExpr("(A : Type) -> A"), env))

    const result = buildWrapped(parseExpr("{x} -> x"), env, piType)
    const data = unwrapData(result)
    const type = unwrapType(result)

    expect(termTag(data)).toBe("lam")
    expect(termTag(type)).toBe("pi")

    // The lambda body should be identity: applying it to any arg gives that arg back
    const lam = unLam(data)!
    const testArg = encVar(freshMarker())
    const bodyResult = apply(lam.body, testArg)
    expect(treeEqual(bodyResult, testArg)).toBe(true)
  })

  it("application: function applied to argument with matching types", () => {
    const env = declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])

    // id Type
    const result = buildWrapped(parseExpr("id Type"), env)
    const type = unwrapType(result)
    // id Type : Type -> Type
    expect(termTag(type)).toBe("pi")
  })
})

// ============================================================
// Phase 4: Declaration Pipeline
// ============================================================

describe("Phase 4: Declarations", () => {
  it("defines Bool as a type", () => {
    const env: Env = new Map()
    const result = cocCheckDecl(env, "Bool", parseExpr("Type"),
      parseExpr("(R : Type) -> R -> R -> R"))
    expect(treeEqual(result.type, encType())).toBe(true)
  })

  it("defines polymorphic identity", () => {
    const env = declEnv([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const wrapped = env.get("id")!
    const type = unwrapType(wrapped)
    expect(termTag(type)).toBe("pi")
  })

  it("declarations build on each other", () => {
    const env = declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
    ])
    expect(env.has("Bool")).toBe(true)
    expect(env.has("not")).toBe(true)
  })

  it("id Type has type Type -> Type (dependent application)", () => {
    const env = declEnv([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const result = buildWrapped(parseExpr("id Type"), env)
    const type = unwrapType(result)
    // Should be Type -> Type (non-dependent Pi with domain=Type, codomain=Type)
    const pi = unPi(whnfTree(type))
    expect(pi).not.toBeNull()
    expect(treeEqual(pi!.domain, encType())).toBe(true)
  })

  it("type mismatch in application throws", () => {
    const env = declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
    ])
    // not expects Bool, but Type is not Bool
    expect(() => buildWrapped(parseExpr("not Type"), env)).toThrow(CocError)
  })

  it("multi-param lambda desugars correctly", () => {
    const env = declEnv([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const wrapped = env.get("id")!
    const data = unwrapData(wrapped)
    expect(termTag(data)).toBe("lam")
  })

  it("non-dependent arrow works as sugar for Pi", () => {
    const env = declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
    ])
    const wrapped = env.get("Bool")!
    const data = unwrapData(wrapped)
    // (R : Type) -> R -> R -> R should be a Pi
    expect(termTag(data)).toBe("pi")
  })

  it("declaration without type annotation infers from value", () => {
    const env: Env = new Map()
    const result = cocCheckDecl(env, "T", null, parseExpr("Type"))
    expect(treeEqual(result.type, encType())).toBe(true)
  })
})

// ============================================================
// Phase 5: Pretty Printing and Normalization
// ============================================================

describe("Phase 5: Pretty Printing", () => {
  it("prints Type", () => {
    expect(printEncoded(encType())).toBe("Type")
  })

  it("prints App", () => {
    const t = encApp(encVar(freshMarker()), encType())
    const s = printEncoded(t)
    expect(s).toContain("Type")
  })
})

describe("Phase 5: Normalization", () => {
  it("normalizes Type", () => {
    const result = normalize(encType())
    expect(treeEqual(result, encType())).toBe(true)
  })

  it("normalizes beta redex", () => {
    const x = encVar(freshMarker())
    const redex = encApp(encLam(LEAF, I), x)
    const result = normalize(redex)
    expect(treeEqual(result, x)).toBe(true)
  })
})

// ============================================================
// Phase 6: Recursive Definitions and Validation
// ============================================================

describe("Phase 6: Full Pipeline", () => {
  it("full prelude loads without errors", () => {
    expect(() => declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
      "let and : Bool -> Bool -> Bool := {a b R t f} -> a R (b R t f) f",
      "let or : Bool -> Bool -> Bool := {a b R t f} -> a R t (b R t f)",
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let zero : Nat := {R s z} -> z",
      "let succ : Nat -> Nat := {n R s z} -> s (n R s z)",
      "let add : Nat -> Nat -> Nat := {m n R s z} -> m R s (n R s z)",
      "let mul : Nat -> Nat -> Nat := {m n R s z} -> m R (n R s) z",
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])).not.toThrow()
  })

  it("Church not true = false (behavioral equivalence)", () => {
    const env = declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
    ])

    // Build `not true` through the full CoC pipeline
    const result = buildWrapped(parseExpr("not true"), env)
    const resultData = unwrapData(result)

    // Build false
    const boolType = unwrapData(env.get("Bool")!)
    const falseWrapped = buildWrapped(parseExpr("{R t f} -> f"), env, boolType)
    const falseData = unwrapData(falseWrapped)

    // not(true) should be convertible with false
    expect(convertible(resultData, falseData)).toBe(true)
  })

  it("Church add 2 3 = 5 (behavioral equivalence)", () => {
    const env = declEnv([
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let add : Nat -> Nat -> Nat := {m n R s z} -> m R s (n R s z)",
    ])

    // Build add 2 3 through the full CoC pipeline
    const result = buildWrapped(parseExpr("add 2 3"), env)
    const resultData = unwrapData(result)

    // Build 5
    const natType = unwrapData(env.get("Nat")!)
    const fiveWrapped = buildWrapped(parseExpr("5"), env, natType)
    const fiveData = unwrapData(fiveWrapped)

    // add 2 3 should be convertible with 5
    expect(convertible(resultData, fiveData)).toBe(true)
  })

  it("equivalence with old pipeline: id Type has correct type", () => {
    const env = declEnv([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const result = buildWrapped(parseExpr("id Type"), env)
    const type = unwrapType(result)
    // Should be a Pi: Type -> Type
    const pi = unPi(whnfTree(type))
    expect(pi).not.toBeNull()
  })
})

// ============================================================
// Phase 5: REPL Integration
// ============================================================

import { processLine, initialState } from "../src/repl.js"

describe("Phase 5: REPL Integration", () => {
  it(":coc command toggles mode", () => {
    const state = initialState()
    expect(state.cocMode).toBe(false)
    const r1 = processLine(state, ":coc")
    expect(r1).toContain("enabled")
    expect(state.cocMode).toBe(true)
    const r2 = processLine(state, ":coc")
    expect(r2).toContain("disabled")
    expect(state.cocMode).toBe(false)
  })

  it("processLine in coc mode handles declarations", () => {
    const state = initialState()
    state.cocMode = true
    const r = processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    expect(r).toContain("Bool")
    expect(r).toContain("Type")
    expect(state.cocEnv.has("Bool")).toBe(true)
  })

  it("processLine in coc mode handles expressions", () => {
    const state = initialState()
    state.cocMode = true
    const r = processLine(state, "Type")
    expect(r).toContain("Type")
  })

  it("coc mode handles multi-def pipeline", () => {
    const state = initialState()
    state.cocMode = true
    processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    processLine(state, "let id : (A : Type) -> A -> A := {A x} -> x")
    const r = processLine(state, "id Bool")
    expect(r).not.toContain("error")
    expect(r).not.toContain("Error")
  })
})

// ============================================================
// CoC Prelude: Built-in definitions
// ============================================================

describe("CoC Prelude", () => {
  it("loads without errors", () => {
    expect(() => loadCocPrelude(new Map())).not.toThrow()
  })

  it("defines all expected builtins", () => {
    const env = loadCocPrelude(new Map())
    const expected = [
      "Tree", "leaf", "stem", "fork", "triage",
      "encType", "encVar", "encApp", "encLam", "encPi",
      "Wrapped", "wrap", "unwrapData", "unwrapType",
    ]
    for (const name of expected) {
      expect(env.has(name), `missing: ${name}`).toBe(true)
    }
  })

  it("Tree is a Type", () => {
    const env = loadCocPrelude(new Map())
    const treeType = unwrapType(env.get("Tree")!)
    expect(convertible(treeType, encType())).toBe(true)
  })

  it("leaf has type Tree", () => {
    const env = loadCocPrelude(new Map())
    const leafType = unwrapType(env.get("leaf")!)
    const treeData = unwrapData(env.get("Tree")!)
    expect(convertible(leafType, treeData)).toBe(true)
  })

  it("stem has type Tree -> Tree", () => {
    const env = loadCocPrelude(new Map())
    const stemType = unwrapType(env.get("stem")!)
    const treeData = unwrapData(env.get("Tree")!)
    // Should be Pi(Tree, Tree)
    const pi = unPi(whnfTree(stemType))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("fork has type Tree -> Tree -> Tree", () => {
    const env = loadCocPrelude(new Map())
    const forkType = unwrapType(env.get("fork")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(forkType))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("encApp has type Tree -> Tree -> Tree", () => {
    const env = loadCocPrelude(new Map())
    const t = unwrapType(env.get("encApp")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(t))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("wrap has type Tree -> Tree -> Wrapped", () => {
    const env = loadCocPrelude(new Map())
    const t = unwrapType(env.get("wrap")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(t))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("builtins are usable: encApp encType encType type-checks", () => {
    const env = loadCocPrelude(new Map())
    expect(() => buildWrapped(parseExpr("encApp encType encType"), env)).not.toThrow()
  })

  it("builtins are usable: wrap encType encType type-checks", () => {
    const env = loadCocPrelude(new Map())
    expect(() => buildWrapped(parseExpr("wrap encType encType"), env)).not.toThrow()
  })

  it("unwrapData (wrap x y) = x (Church pair round-trip)", () => {
    const env = loadCocPrelude(new Map())
    const result = buildWrapped(parseExpr("unwrapData (wrap leaf leaf)"), env)
    const resultData = unwrapData(result)
    const leafData = unwrapData(env.get("leaf")!)
    expect(convertible(resultData, leafData)).toBe(true)
  })

  it("unwrapType (wrap x y) = y (Church pair round-trip)", () => {
    const env = loadCocPrelude(new Map())
    const result = buildWrapped(parseExpr("unwrapType (wrap leaf leaf)"), env)
    const resultData = unwrapData(result)
    const leafData = unwrapData(env.get("leaf")!)
    expect(convertible(resultData, leafData)).toBe(true)
  })

  it(":ctx in coc mode shows builtins", () => {
    const state = initialState()
    processLine(state, ":coc")
    const ctx = processLine(state, ":ctx")
    expect(ctx).toContain("Tree")
    expect(ctx).toContain("leaf")
    expect(ctx).toContain("stem")
    expect(ctx).toContain("fork")
    expect(ctx).toContain("triage")
    expect(ctx).toContain("encType")
    expect(ctx).toContain("encVar")
    expect(ctx).toContain("encApp")
    expect(ctx).toContain("encLam")
    expect(ctx).toContain("encPi")
    expect(ctx).toContain("wrap")
    expect(ctx).toContain("unwrapData")
    expect(ctx).toContain("unwrapType")
  })

  it("user defs build on top of builtins", () => {
    const state = initialState()
    processLine(state, ":coc")
    const r = processLine(state, "let myTree : Tree := fork leaf leaf")
    expect(r).toContain("myTree")
    expect(r).not.toContain("Error")
    expect(r).not.toContain("error")
  })
})

// ============================================================
// Extensive cross-pipeline equivalence tests
// ============================================================

describe("Cross-pipeline equivalence", () => {
  function fullEnv(): Env {
    return declEnv([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
      "let and : Bool -> Bool -> Bool := {a b R t f} -> a R (b R t f) f",
      "let or : Bool -> Bool -> Bool := {a b R t f} -> a R t (b R t f)",
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let zero : Nat := {R s z} -> z",
      "let succ : Nat -> Nat := {n R s z} -> s (n R s z)",
      "let add : Nat -> Nat -> Nat := {m n R s z} -> m R s (n R s z)",
      "let mul : Nat -> Nat -> Nat := {m n R s z} -> m R (n R s) z",
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
  }

  function checkConv(env: Env, expr: string, expected: string, expectedType?: string) {
    const result = buildWrapped(parseExpr(expr), env)
    const resultData = unwrapData(result)

    // Build expected value — need a type annotation for Church literals
    let expType: Tree | undefined
    if (expectedType) {
      expType = unwrapData(env.get(expectedType)!)
    }
    const expectedWrapped = expType
      ? buildWrapped(parseExpr(expected), env, expType)
      : buildWrapped(parseExpr(expected), env)
    const expectedData = unwrapData(expectedWrapped)

    expect(convertible(resultData, expectedData, { remaining: 50000 })).toBe(true)
  }

  // --- Boolean operations ---

  it("not true = false", () => {
    const env = fullEnv()
    checkConv(env, "not true", "{R t f} -> f", "Bool")
  })

  it("not false = true", () => {
    const env = fullEnv()
    checkConv(env, "not false", "{R t f} -> t", "Bool")
  })

  it("and true true = true", () => {
    const env = fullEnv()
    checkConv(env, "and true true", "{R t f} -> t", "Bool")
  })

  it("and true false = false", () => {
    const env = fullEnv()
    checkConv(env, "and true false", "{R t f} -> f", "Bool")
  })

  it("and false true = false", () => {
    const env = fullEnv()
    checkConv(env, "and false true", "{R t f} -> f", "Bool")
  })

  it("and false false = false", () => {
    const env = fullEnv()
    checkConv(env, "and false false", "{R t f} -> f", "Bool")
  })

  it("or true false = true", () => {
    const env = fullEnv()
    checkConv(env, "or true false", "{R t f} -> t", "Bool")
  })

  it("or false false = false", () => {
    const env = fullEnv()
    checkConv(env, "or false false", "{R t f} -> f", "Bool")
  })

  it("or false true = true", () => {
    const env = fullEnv()
    checkConv(env, "or false true", "{R t f} -> t", "Bool")
  })

  it("not (not true) = true", () => {
    const env = fullEnv()
    checkConv(env, "not (not true)", "{R t f} -> t", "Bool")
  })

  // --- Nat operations ---

  it("add 0 0 = 0", () => {
    const env = fullEnv()
    checkConv(env, "add 0 0", "0", "Nat")
  })

  it("add 1 0 = 1", () => {
    const env = fullEnv()
    checkConv(env, "add 1 0", "1", "Nat")
  })

  it("add 0 1 = 1", () => {
    const env = fullEnv()
    checkConv(env, "add 0 1", "1", "Nat")
  })

  it("add 2 3 = 5", () => {
    const env = fullEnv()
    checkConv(env, "add 2 3", "5", "Nat")
  })

  it("add 3 4 = 7", () => {
    const env = fullEnv()
    checkConv(env, "add 3 4", "7", "Nat")
  })

  it("succ 0 = 1", () => {
    const env = fullEnv()
    checkConv(env, "succ 0", "1", "Nat")
  })

  it("succ (succ 0) = 2", () => {
    const env = fullEnv()
    checkConv(env, "succ (succ 0)", "2", "Nat")
  })

  it("mul 2 3 = 6", () => {
    const env = fullEnv()
    checkConv(env, "mul 2 3", "6", "Nat")
  })

  it("mul 0 5 = 0", () => {
    const env = fullEnv()
    checkConv(env, "mul 0 5", "0", "Nat")
  })

  it("mul 1 7 = 7", () => {
    const env = fullEnv()
    checkConv(env, "mul 1 7", "7", "Nat")
  })

  it("mul 3 3 = 9", () => {
    const env = fullEnv()
    checkConv(env, "mul 3 3", "9", "Nat")
  })

  it("add (mul 2 3) (mul 1 2) = 8", () => {
    const env = fullEnv()
    checkConv(env, "add (mul 2 3) (mul 1 2)", "8", "Nat")
  })

  // --- Identity function ---

  it("id Bool true = true", () => {
    const env = fullEnv()
    checkConv(env, "id Bool true", "{R t f} -> t", "Bool")
  })

  it("id Nat 3 = 3", () => {
    const env = fullEnv()
    checkConv(env, "id Nat 3", "3", "Nat")
  })

  it("id (Bool -> Bool) not applied to true = false", () => {
    const env = fullEnv()
    checkConv(env, "id (Bool -> Bool) not true", "{R t f} -> f", "Bool")
  })

  // --- Type-level operations ---

  it("id applied to Bool -> Bool has correct type", () => {
    const env = fullEnv()
    const result = buildWrapped(parseExpr("id (Bool -> Bool)"), env)
    const type = unwrapType(result)
    // type should be Type -> Type applied, so the result type is Type
    // Actually id : (A : Type) -> A -> A, so id (Bool -> Bool) : (Bool -> Bool) -> (Bool -> Bool)
    // The type of the result is (Bool -> Bool) -> (Bool -> Bool), which is a Pi type
    expect(termTag(whnfTree(type))).toBe("pi")
  })

  it("not applied to (and true false) = true", () => {
    const env = fullEnv()
    checkConv(env, "not (and true false)", "{R t f} -> t", "Bool")
  })

  it("add (succ 0) (succ (succ 0)) = 3", () => {
    const env = fullEnv()
    checkConv(env, "add (succ 0) (succ (succ 0))", "3", "Nat")
  })
})

// ============================================================
// Tree-native core operations
// ============================================================

describe("Tree-native operations", () => {
  describe("destructors (FST, SND, CHILD)", () => {
    it("FST extracts left child of fork", () => {
      const l = stem(LEAF)
      const r = stem(stem(LEAF))
      expect(treeEqual(apply(FST, fork(l, r)), l)).toBe(true)
    })

    it("SND extracts right child of fork", () => {
      const l = stem(LEAF)
      const r = stem(stem(LEAF))
      expect(treeEqual(apply(SND, fork(l, r)), r)).toBe(true)
    })

    it("CHILD extracts child of stem", () => {
      const c = fork(LEAF, LEAF)
      expect(treeEqual(apply(CHILD, stem(c)), c)).toBe(true)
    })
  })

  describe("encoding constructors", () => {
    it("ENC_APP_T builds same structure as encApp", () => {
      const m = stem(LEAF)
      const n = stem(stem(LEAF))
      const native = apply(apply(ENC_APP_T, m), n)
      expect(treeEqual(native, encApp(m, n))).toBe(true)
    })

    it("ENC_LAM_T builds same structure as encLam", () => {
      const d = LEAF
      const b = stem(LEAF)
      const native = apply(apply(ENC_LAM_T, d), b)
      expect(treeEqual(native, encLam(d, b))).toBe(true)
    })

    it("ENC_PI_T builds same structure as encPi", () => {
      const d = LEAF
      const b = stem(LEAF)
      const native = apply(apply(ENC_PI_T, d), b)
      expect(treeEqual(native, encPi(d, b))).toBe(true)
    })
  })

  describe("termCase (5-way dispatch)", () => {
    // Handlers that tag each case with a distinct tree
    const tag = (n: number) => { let t: Tree = LEAF; for (let i = 0; i < n; i++) t = stem(t); return t }
    const onType = tag(0)  // LEAF
    const onVar = tag(1)   // stem(LEAF) — will receive marker
    const onApp = tag(2)   // stem(stem(LEAF)) — will receive func, arg
    const onLam = tag(3)
    const onPi = tag(4)

    it("dispatches Type → onType", () => {
      const budget = { remaining: 1000 }
      const result = apply(apply(apply(apply(apply(apply(
        TERM_CASE, onType), onVar), onApp), onLam), onPi), encType(), budget)
      expect(treeEqual(result, onType)).toBe(true)
    })

    it("dispatches Var → onVar(marker)", () => {
      const marker = stem(stem(LEAF))
      const budget = { remaining: 1000 }
      const result = apply(apply(apply(apply(apply(apply(
        TERM_CASE, onType), onVar), onApp), onLam), onPi), encVar(marker), budget)
      expect(treeEqual(result, apply(onVar, marker))).toBe(true)
    })

    it("dispatches App → onApp(func)(arg)", () => {
      const f = stem(LEAF), x = stem(stem(LEAF))
      const budget = { remaining: 1000 }
      const result = apply(apply(apply(apply(apply(apply(
        TERM_CASE, onType), onVar), onApp), onLam), onPi), encApp(f, x), budget)
      expect(treeEqual(result, apply(apply(onApp, f, budget), x, budget))).toBe(true)
    })

    it("dispatches Lam → onLam(domain)(body)", () => {
      const d = LEAF, b = stem(LEAF)
      const budget = { remaining: 1000 }
      const result = apply(apply(apply(apply(apply(apply(
        TERM_CASE, onType), onVar), onApp), onLam), onPi), encLam(d, b), budget)
      expect(treeEqual(result, apply(apply(onLam, d, budget), b, budget))).toBe(true)
    })

    it("dispatches Pi → onPi(domain)(body)", () => {
      const d = stem(LEAF), b = stem(stem(LEAF))
      const budget = { remaining: 1000 }
      const result = apply(apply(apply(apply(apply(apply(
        TERM_CASE, onType), onVar), onApp), onLam), onPi), encPi(d, b), budget)
      expect(treeEqual(result, apply(apply(onPi, d, budget), b, budget))).toBe(true)
    })
  })

  describe("builtins visible in :ctx", () => {
    it(":ctx shows tree-native builtins", () => {
      const state = initialState()
      processLine(state, ":coc")
      const ctx = processLine(state, ":ctx")
      expect(ctx).toContain("tfst")
      expect(ctx).toContain("tsnd")
      expect(ctx).toContain("tchild")
      expect(ctx).toContain("tEncApp")
      expect(ctx).toContain("tEncLam")
      expect(ctx).toContain("tEncPi")
      expect(ctx).toContain("termCase")
    })
  })
})
