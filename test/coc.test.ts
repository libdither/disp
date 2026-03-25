import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, treeEqual, apply, I, prettyTree, clearApplyCache } from "../src/tree.js"
import {
  encType, encVar, encApp, encLam, encPi,
  termTag, unVar, unApp, unLam, unPi,
  wrap, unwrapData, unwrapType,
  freshMarker, resetMarkerCounter, clearNativeBuiltins,
  treeToExprReplacing, abstractMarkerOut,
  whnfTree, normalize, convertible, convertibleUnderBinder,
  buildWrapped, cocCheckDecl, cocCheckRecDecl,
  printEncoded,
  CocError, type Env,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
} from "../src/coc.js"
import {
  FST, SND, CHILD, ENC_APP_T, ENC_LAM_T, ENC_PI_T,
  TERM_CASE, TREE_EQ_STEP, WHNF_STEP, ABSTRACT_OUT_STEP, CONVERTIBLE_STEP, TYPECHECK,
} from "../src/tree-native.js"
import { loadPrelude, buildNameMap } from "../src/prelude.js"
import { eTree, eFvar, eApp, bracketAbstract, collapse, collapseAndEval, compileRecAndEval, compileAndEval } from "../src/compile.js"
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
  clearNativeBuiltins()
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

  it("not true = false (via REPL evaluation)", () => {
    // Primitive builtins (boolElim, natElim) reduce at the tree level, not the CoC level.
    // Test behavioral equivalence through REPL compilation + evaluation.
    const state = initialState()
    processLine(state, "let not : Bool -> Bool := {b} -> boolElim Bool false true b")
    const result = processLine(state, "not true")
    expect(result).toMatch(/^false : Bool/)
  })

  it("add 2 3 = 5 (via REPL evaluation)", () => {
    const state = initialState()
    loadFile(state, "prelude.disp", true)
    const result = processLine(state, "add 2 3")
    expect(result).toMatch(/^5 : Nat/)
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

import { processLine, initialState, loadFile } from "../src/repl.js"

describe("Phase 5: REPL Integration", () => {
  it("processLine handles declarations", () => {
    const state = initialState()
    const r = processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    expect(r).toContain("Bool")
    expect(r).toContain("Type")
    expect(state.cocEnv.has("Bool")).toBe(true)
  })

  it("processLine handles expressions", () => {
    const state = initialState()
    const r = processLine(state, "Type")
    expect(r).toContain("Type")
  })

  it("handles multi-def pipeline", () => {
    const state = initialState()
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
    expect(() => loadPrelude()).not.toThrow()
  })

  it("defines all expected builtins", () => {
    const { cocEnv: env } = loadPrelude()
    const expected = [
      "Tree", "Bool", "Nat",
      "leaf", "stem", "fork", "triage",
      "true", "false", "boolElim",
      "zero", "succ", "natElim",
      "encType", "encVar", "encApp", "encLam", "encPi",
      "wrap", "unwrapData", "unwrapType",
    ]
    for (const name of expected) {
      expect(env.has(name), `missing: ${name}`).toBe(true)
    }
  })

  it("Tree is a Type", () => {
    const { cocEnv: env } = loadPrelude()
    const treeType = unwrapType(env.get("Tree")!)
    expect(convertible(treeType, encType())).toBe(true)
  })

  it("leaf has type Tree", () => {
    const { cocEnv: env } = loadPrelude()
    const leafType = unwrapType(env.get("leaf")!)
    const treeData = unwrapData(env.get("Tree")!)
    expect(convertible(leafType, treeData)).toBe(true)
  })

  it("stem has type Tree -> Tree", () => {
    const { cocEnv: env } = loadPrelude()
    const stemType = unwrapType(env.get("stem")!)
    const treeData = unwrapData(env.get("Tree")!)
    // Should be Pi(Tree, Tree)
    const pi = unPi(whnfTree(stemType))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("fork has type Tree -> Tree -> Tree", () => {
    const { cocEnv: env } = loadPrelude()
    const forkType = unwrapType(env.get("fork")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(forkType))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("encApp has type Tree -> Tree -> Tree", () => {
    const { cocEnv: env } = loadPrelude()
    const t = unwrapType(env.get("encApp")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(t))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("wrap has type Tree -> Tree -> Tree", () => {
    const { cocEnv: env } = loadPrelude()
    const t = unwrapType(env.get("wrap")!)
    const treeData = unwrapData(env.get("Tree")!)
    const pi = unPi(whnfTree(t))
    expect(pi).not.toBeNull()
    expect(convertible(pi!.domain, treeData)).toBe(true)
  })

  it("builtins are usable: encApp encType encType type-checks", () => {
    const { cocEnv: env } = loadPrelude()
    expect(() => buildWrapped(parseExpr("encApp encType encType"), env)).not.toThrow()
  })

  it("builtins are usable: wrap encType encType type-checks", () => {
    const { cocEnv: env } = loadPrelude()
    expect(() => buildWrapped(parseExpr("wrap encType encType"), env)).not.toThrow()
  })

  it("unwrapData (wrap x y) = x (fork pair round-trip at tree level)", () => {
    // wrap/unwrapData are tree-level primitives now (fork pairs, not Church pairs)
    // Test at the tree level directly
    const data = encApp(LEAF, stem(LEAF))
    const type = encPi(LEAF, LEAF)
    const wrapped = wrap(data, type)
    expect(treeEqual(unwrapData(wrapped), data)).toBe(true)
  })

  it("unwrapType (wrap x y) = y (fork pair round-trip at tree level)", () => {
    const data = encApp(LEAF, stem(LEAF))
    const type = encPi(LEAF, LEAF)
    const wrapped = wrap(data, type)
    expect(treeEqual(unwrapType(wrapped), type)).toBe(true)
  })

  it(":ctx shows builtins", () => {
    const state = initialState()
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
  // With tree-encoded primitives (boolElim, natElim), behavioral equivalence
  // is tested through REPL compilation + evaluation, not CoC-level convertibility.
  function fullState(): ReturnType<typeof initialState> {
    const state = initialState()
    loadFile(state, "prelude.disp", true)
    return state
  }

  function checkEval(state: ReturnType<typeof initialState>, expr: string, expected: RegExp) {
    const result = processLine(state, expr)
    expect(result).toMatch(expected)
  }

  // --- Boolean operations ---

  it("not true = false", () => {
    const state = fullState()
    checkEval(state, "not true", /^false : Bool/)
  })

  it("not false = true", () => {
    const state = fullState()
    checkEval(state, "not false", /^true : Bool/)
  })

  it("and true true = true", () => {
    const state = fullState()
    checkEval(state, "and true true", /^true : Bool/)
  })

  it("and true false = false", () => {
    const state = fullState()
    checkEval(state, "and true false", /^false : Bool/)
  })

  it("and false true = false", () => {
    const state = fullState()
    checkEval(state, "and false true", /^false : Bool/)
  })

  it("and false false = false", () => {
    const state = fullState()
    checkEval(state, "and false false", /^false : Bool/)
  })

  it("or true false = true", () => {
    const state = fullState()
    checkEval(state, "or true false", /^true : Bool/)
  })

  it("or false false = false", () => {
    const state = fullState()
    checkEval(state, "or false false", /^false : Bool/)
  })

  it("or false true = true", () => {
    const state = fullState()
    checkEval(state, "or false true", /^true : Bool/)
  })

  it("not (not true) = true", () => {
    const state = fullState()
    checkEval(state, "not (not true)", /^true : Bool/)
  })

  // --- Nat operations ---

  it("add 0 0 = 0", () => {
    const state = fullState()
    checkEval(state, "add 0 0", /^0 : Nat/)
  })

  it("add 1 0 = 1", () => {
    const state = fullState()
    checkEval(state, "add 1 0", /^1 : Nat/)
  })

  it("add 0 1 = 1", () => {
    const state = fullState()
    checkEval(state, "add 0 1", /^1 : Nat/)
  })

  it("add 2 3 = 5", () => {
    const state = fullState()
    checkEval(state, "add 2 3", /^5 : Nat/)
  })

  it("add 3 4 = 7", () => {
    const state = fullState()
    checkEval(state, "add 3 4", /^7 : Nat/)
  })

  it("succ 0 = 1", () => {
    const state = fullState()
    checkEval(state, "succ 0", /^1 : Nat/)
  })

  it("succ (succ 0) = 2", () => {
    const state = fullState()
    checkEval(state, "succ (succ 0)", /^2 : Nat/)
  })

  it("mul 2 3 = 6", () => {
    const state = fullState()
    checkEval(state, "mul 2 3", /^6 : Nat/)
  })

  it("mul 0 5 = 0", () => {
    const state = fullState()
    checkEval(state, "mul 0 5", /^0 : Nat/)
  })

  it("mul 1 7 = 7", () => {
    const state = fullState()
    checkEval(state, "mul 1 7", /^7 : Nat/)
  })

  it("mul 3 3 = 9", () => {
    const state = fullState()
    checkEval(state, "mul 3 3", /^9 : Nat/)
  })

  it("add (mul 2 3) (mul 1 2) = 8", () => {
    const state = fullState()
    checkEval(state, "add (mul 2 3) (mul 1 2)", /^8 : Nat/)
  })

  // --- Identity function ---

  it("id Bool true = true", () => {
    const state = fullState()
    checkEval(state, "id Bool true", /^true : Bool/)
  })

  it("id Nat 3 = 3", () => {
    const state = fullState()
    checkEval(state, "id Nat 3", /^3 : Nat/)
  })

  it("id (Bool -> Bool) not applied to true = false", () => {
    const state = fullState()
    checkEval(state, "id (Bool -> Bool) not true", /^false : Bool/)
  })

  // --- Type-level operations ---

  it("id applied to Bool -> Bool has correct type", () => {
    const state = fullState()
    const result = processLine(state, "id (Bool -> Bool)")
    // The result should have a Pi type (Bool -> Bool) -> (Bool -> Bool)
    expect(result).not.toContain("Error")
  })

  it("not applied to (and true false) = true", () => {
    const state = fullState()
    checkEval(state, "not (and true false)", /^true : Bool/)
  })

  it("add (succ 0) (succ (succ 0)) = 3", () => {
    const state = fullState()
    checkEval(state, "add (succ 0) (succ (succ 0))", /^3 : Nat/)
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

// ============================================================
// Tree-native step functions
// ============================================================

describe("Tree-native step functions", () => {
  // --- Helpers ---
  const budget = { remaining: 100000 }
  function freshBudget() { return { remaining: 100000 } }

  // Compile a Church numeral as a tree: n f x = f^n(x)
  function fuel(n: number): Tree {
    let body: Expr = eFvar("x")
    for (let i = 0; i < n; i++) body = eApp(eFvar("f"), body)
    return collapse(bracketAbstract("f", bracketAbstract("x", body)))
  }

  // TT/FF as tree-encoded booleans: TT = LEAF, FF = stem(LEAF)
  const TT = LEAF
  const FF = stem(LEAF)

  // base case for treeEq fold: {x y} -> ff
  const BASE_EQ: Tree = collapse(bracketAbstract("x", bracketAbstract("y", eTree(FF))))

  function mkTreeEq(n: number): (a: Tree, b: Tree) => Tree {
    const eq = apply(apply(fuel(n), TREE_EQ_STEP), BASE_EQ, freshBudget())
    return (a, b) => apply(apply(eq, a, freshBudget()), b, freshBudget())
  }

  // Build whnf with given fuel
  function mkWhnf(n: number): (t: Tree) => Tree {
    const w = apply(apply(fuel(n), WHNF_STEP), I, freshBudget()) // base = identity
    return (t) => apply(w, t, freshBudget())
  }

  function isTT(t: Tree): boolean { return treeEqual(t, TT) }
  function isFF(t: Tree): boolean { return treeEqual(t, FF) }

  // --- TREE_EQ_STEP ---

  describe("TREE_EQ_STEP", () => {
    it("leaf == leaf → TT", () => {
      const eq = mkTreeEq(5)
      expect(isTT(eq(LEAF, LEAF))).toBe(true)
    })

    it("stem(leaf) == stem(leaf) → TT", () => {
      const eq = mkTreeEq(5)
      expect(isTT(eq(stem(LEAF), stem(LEAF)))).toBe(true)
    })

    it("fork(leaf,leaf) == fork(leaf,leaf) → TT", () => {
      const eq = mkTreeEq(5)
      expect(isTT(eq(fork(LEAF, LEAF), fork(LEAF, LEAF)))).toBe(true)
    })

    it("leaf != stem(leaf) → FF", () => {
      const eq = mkTreeEq(5)
      expect(isFF(eq(LEAF, stem(LEAF)))).toBe(true)
    })

    it("stem(leaf) != leaf → FF", () => {
      const eq = mkTreeEq(5)
      expect(isFF(eq(stem(LEAF), LEAF))).toBe(true)
    })

    it("fork(leaf,leaf) != fork(stem(leaf),leaf) → FF", () => {
      const eq = mkTreeEq(5)
      expect(isFF(eq(fork(LEAF, LEAF), fork(stem(LEAF), LEAF)))).toBe(true)
    })

    it("deeper structures: stem(stem(leaf)) == stem(stem(leaf))", () => {
      const eq = mkTreeEq(10)
      expect(isTT(eq(stem(stem(LEAF)), stem(stem(LEAF))))).toBe(true)
    })

    it("fuel=0 always returns FF (base case)", () => {
      const eq = mkTreeEq(0)
      expect(isFF(eq(LEAF, LEAF))).toBe(true)
    })
  })

  // --- WHNF_STEP ---

  describe("WHNF_STEP", () => {
    it("Type stays unchanged", () => {
      const w = mkWhnf(5)
      expect(treeEqual(w(encType()), encType())).toBe(true)
    })

    it("Var stays unchanged", () => {
      const w = mkWhnf(5)
      const v = encVar(stem(LEAF))
      expect(treeEqual(w(v), v)).toBe(true)
    })

    it("Lam stays unchanged", () => {
      const w = mkWhnf(5)
      const lam = encLam(LEAF, I)
      expect(treeEqual(w(lam), lam)).toBe(true)
    })

    it("Pi stays unchanged", () => {
      const w = mkWhnf(5)
      const pi = encPi(LEAF, I)
      expect(treeEqual(w(pi), pi)).toBe(true)
    })

    it("App(Lam(_, I), x) beta-reduces to x", () => {
      const w = mkWhnf(5)
      const x = encVar(stem(stem(LEAF)))
      const app = encApp(encLam(LEAF, I), x)
      expect(treeEqual(w(app), x)).toBe(true)
    })
  })

  // --- ABSTRACT_OUT_STEP ---

  describe("ABSTRACT_OUT_STEP", () => {
    // abstractOutStep : eq -> target -> self -> tree -> Tree
    // fold: fuel (Tree→Tree) (abstractOutStep eq target) base tree
    // base = tK = stem(LEAF)
    function mkAbstractOut(n: number, eqN: number, target: Tree): (tree: Tree) => Tree {
      const b1 = freshBudget()
      const eqFn = apply(apply(fuel(eqN), TREE_EQ_STEP), BASE_EQ, b1)
      const step = apply(apply(ABSTRACT_OUT_STEP, eqFn, b1), target, b1)
      const abs = apply(apply(fuel(n), step, b1), stem(LEAF), b1)
      return (tree: Tree) => apply(abs, tree, freshBudget())
    }

    it("target → I (identity)", () => {
      const target = stem(LEAF)
      const abs = mkAbstractOut(5, 5, target)
      const result = abs(target)
      // Result should be I — applying to any arg gives that arg
      const testArg = fork(LEAF, LEAF)
      expect(treeEqual(apply(result, testArg, freshBudget()), testArg)).toBe(true)
    })

    it("constant → K(constant)", () => {
      const target = stem(stem(LEAF))
      const abs = mkAbstractOut(5, 5, target)
      const result = abs(LEAF) // abstract target out of LEAF (which doesn't contain target)
      // K(LEAF) applied to anything gives LEAF
      const testArg = fork(LEAF, LEAF)
      expect(treeEqual(apply(result, testArg, freshBudget()), LEAF)).toBe(true)
    })
  })

  // --- CONVERTIBLE_STEP ---

  describe("CONVERTIBLE_STEP", () => {
    const BIG = 2000000

    // Build convertible with given fuels
    // Each partial application gets a fresh budget since these are eager and expensive
    function mkConvertible(n: number, whnfN: number, eqN: number): (a: Tree, b: Tree) => Tree {
      const whnfFn = apply(apply(fuel(whnfN), WHNF_STEP), I, { remaining: BIG })
      const treeEqFn = apply(apply(fuel(eqN), TREE_EQ_STEP), BASE_EQ, { remaining: BIG })
      const base = collapse(bracketAbstract("m", bracketAbstract("x", bracketAbstract("y",
        eTree(stem(LEAF))))))  // FF = stem(LEAF)
      const step = apply(apply(CONVERTIBLE_STEP, whnfFn, { remaining: BIG }), treeEqFn, { remaining: BIG })
      const folded = apply(apply(fuel(n), step, { remaining: BIG }), base, { remaining: BIG })
      const withMarker = apply(folded, LEAF, { remaining: BIG })
      return (a, b) => {
        const withA = apply(withMarker, a, { remaining: BIG })
        return apply(withA, b, { remaining: BIG })
      }
    }

    it("Type == Type → TT", () => {
      const conv = mkConvertible(5, 5, 5)
      expect(isTT(conv(encType(), encType()))).toBe(true)
    })

    it("Type != Var(m) → FF", () => {
      const conv = mkConvertible(5, 5, 5)
      expect(isFF(conv(encType(), encVar(LEAF)))).toBe(true)
    })

    it("Var(m) != Type → FF", () => {
      const conv = mkConvertible(5, 5, 5)
      expect(isFF(conv(encVar(LEAF), encType()))).toBe(true)
    })

    it("Var(m) == Var(m) → TT", () => {
      const conv = mkConvertible(5, 5, 10)
      const m = stem(LEAF)
      expect(isTT(conv(encVar(m), encVar(m)))).toBe(true)
    })

    it("Var(m) != Var(n) → FF", () => {
      const conv = mkConvertible(5, 5, 10)
      expect(isFF(conv(encVar(stem(LEAF)), encVar(stem(stem(LEAF)))))).toBe(true)
    })

    it("App(f,x) == App(f,x) → TT", () => {
      const conv = mkConvertible(5, 5, 10)
      const f = encVar(stem(LEAF))
      const x = encVar(stem(stem(LEAF)))
      expect(isTT(conv(encApp(f, x), encApp(f, x)))).toBe(true)
    })

    it("App(f,x) != App(f,y) → FF", () => {
      const conv = mkConvertible(5, 5, 10)
      const f = encVar(stem(LEAF))
      const x = encVar(stem(stem(LEAF)))
      const y = encVar(stem(stem(stem(LEAF))))
      expect(isFF(conv(encApp(f, x), encApp(f, y)))).toBe(true)
    })

    it("Lam(d,b) == Lam(d,b) → TT (early equality)", () => {
      const conv = mkConvertible(5, 5, 10)
      const lam = encLam(LEAF, I)
      expect(isTT(conv(lam, lam))).toBe(true)
    })

    it("Pi(d,b) == Pi(d,b) → TT (early equality)", () => {
      const conv = mkConvertible(5, 5, 10)
      const pi = encPi(LEAF, I)
      expect(isTT(conv(pi, pi))).toBe(true)
    })

    it("App(Lam(d, I), x) == x via beta (WHNF)", () => {
      const conv = mkConvertible(5, 5, 10)
      const x = encVar(stem(LEAF))
      const redex = encApp(encLam(LEAF, I), x)
      expect(isTT(conv(redex, x))).toBe(true)
    })
  })

  // --- TYPECHECK / inferStep ---

  describe("TYPECHECK (inferStep)", () => {
    const BIG = 50_000_000

    // Empty Church list: {r n c} -> n
    const EMPTY_ENV = collapse(bracketAbstract("r", bracketAbstract("n", bracketAbstract("c", eFvar("n")))))

    // Build infer function: (env, term) -> wrapped
    // Uses collapseAndEval for absOutFn to avoid collapse/apply mismatch
    function mkInfer(n: number, whnfN: number, eqN: number, absOutN: number): (env: Tree, term: Tree) => Tree {
      const b = { remaining: BIG }
      const whnfFn = apply(apply(fuel(whnfN), WHNF_STEP), I, b)
      const treeEqFn = apply(apply(fuel(eqN), TREE_EQ_STEP), BASE_EQ, b)
      const absOutExpr = eApp(
        eApp(eApp(eTree(fuel(absOutN)),
          eApp(eApp(eTree(ABSTRACT_OUT_STEP), eTree(treeEqFn)), eFvar("target"))),
          eTree(stem(LEAF))),
        eFvar("tree"))
      const absOutFn = collapseAndEval(bracketAbstract("target", bracketAbstract("tree", absOutExpr)))
      const step = apply(apply(apply(TYPECHECK, whnfFn, b), absOutFn, b), treeEqFn, b)
      const base = collapse(bracketAbstract("e", bracketAbstract("t", eTree(LEAF))))
      const folded = apply(apply(fuel(n), step, b), base, b)
      return (env, term) => {
        const b2 = { remaining: BIG }
        return apply(apply(folded, env, b2), term, b2)
      }
    }

    // Helper: build a Church-list env with one entry
    function mkSingleEnv(marker: Tree, wrapped: Tree): Tree {
      const entry = fork(marker, wrapped)
      const envBody = eApp(eApp(eFvar("c"), eTree(entry)),
        eApp(eApp(eApp(eTree(EMPTY_ENV), eFvar("r")), eFvar("n")), eFvar("c")))
      return collapse(bracketAbstract("r", bracketAbstract("n", bracketAbstract("c", envBody))))
    }

    it("Type → wrap(Type, Type)", () => {
      const infer = mkInfer(3, 3, 10, 10)
      const result = infer(EMPTY_ENV, encType())
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(treeEqual(d, encType())).toBe(true)
      expect(treeEqual(t, encType())).toBe(true)
    })

    it("Var lookup from env", () => {
      const infer = mkInfer(3, 3, 10, 10)
      const marker1 = stem(LEAF)
      const env = mkSingleEnv(marker1, wrap(encType(), encType()))
      const result = infer(env, encVar(marker1))
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(treeEqual(d, encType())).toBe(true)
      expect(treeEqual(t, encType())).toBe(true)
    })

    it("Pi(Type, K(Type)) → wrap(Pi(...), Type)", () => {
      const infer = mkInfer(5, 5, 15, 15)
      const piTerm = encPi(encType(), fork(LEAF, LEAF))
      const result = infer(EMPTY_ENV, piTerm)
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(treeEqual(t, encType())).toBe(true)
      expect(termTag(d)).toBe("pi")
    })

    it("Pi(Type, I) — dependent Pi (x:Type) -> x", () => {
      const infer = mkInfer(5, 5, 15, 15)
      const piTerm = encPi(encType(), I)
      const result = infer(EMPTY_ENV, piTerm)
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(treeEqual(t, encType())).toBe(true)
      expect(termTag(d)).toBe("pi")
      expect(treeEqual(unPi(d)!.domain, encType())).toBe(true)
    })

    it("App(Var(f), Type) where f : Type -> Type", () => {
      const infer = mkInfer(5, 5, 15, 15)
      const fMarker = stem(LEAF)
      const fData = encLam(encType(), fork(LEAF, LEAF))
      const fType = encPi(encType(), fork(LEAF, LEAF))
      const env = mkSingleEnv(fMarker, wrap(fData, fType))
      const term = encApp(encVar(fMarker), encType())
      const result = infer(env, term)
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(termTag(d)).toBe("app")
      expect(treeEqual(t, encType())).toBe(true)
    })

    it("Lam(Type, I) = {x:Type} -> x infers Pi(Type, K(Type))", () => {
      const infer = mkInfer(5, 5, 15, 15)
      const term = encLam(encType(), I)
      const result = infer(EMPTY_ENV, term)
      const d = unwrapData(result)
      const t = unwrapType(result)
      // data is a Lam with domain=Type, body=I
      expect(termTag(d)).toBe("lam")
      const lam = unLam(d)!
      expect(treeEqual(lam.domain, encType())).toBe(true)
      expect(treeEqual(lam.body, I)).toBe(true)
      // type is Pi(Type, K(Type)) = Type -> Type
      expect(termTag(t)).toBe("pi")
      const pi = unPi(t)!
      expect(treeEqual(pi.domain, encType())).toBe(true)
      expect(treeEqual(pi.body, fork(LEAF, LEAF))).toBe(true)
    })

    it("App(Lam(Type, I), Type) — beta redex", () => {
      // Needs more fuel: 2 recursive inferences (Lam + Type) + whnf + absOut
      const infer = mkInfer(8, 5, 15, 15)
      const term = encApp(encLam(encType(), I), encType())
      const result = infer(EMPTY_ENV, term)
      const d = unwrapData(result)
      const t = unwrapType(result)
      expect(termTag(d)).toBe("app")
      expect(treeEqual(t, encType())).toBe(true)
    })
  })

  // --- Prelude integration ---

  describe("Prelude integration", () => {
    it("treeEq via prelude: leaf == leaf", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)
      const r = processLine(state, "treeEq 5 leaf leaf")
      expect(r).toContain("true")
    })

    it("treeEq via prelude: leaf != stem leaf", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)
      const r = processLine(state, "treeEq 5 leaf (stem leaf)")
      expect(r).toContain("false")
    })

    it("leaf/stem/fork primitives visible", () => {
      const state = initialState()
      const ctx = processLine(state, ":ctx")
      expect(ctx).toContain("leaf")
      expect(ctx).toContain("stem")
      expect(ctx).toContain("fork")
    })
  })

  // --- FIX combinator and Church fold tests ---

  describe("FIX combinator and Church folds", () => {
    it("tFix visible in context", () => {
      const state = initialState()
      const ctx = processLine(state, ":ctx")
      expect(ctx).toContain("tFix")
    })

    it("triage stem depth counting", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)
      // triage Nat: leaf→0, stem(child)→succ(0) (just counts 1 level), fork→0
      const def = processLine(state,
        "let isLeaf : Tree -> Nat := {t} -> triage Nat (succ zero) ({_} -> zero) ({_ _} -> zero) t")
      expect(def).not.toContain("Error")

      expect(processLine(state, "isLeaf leaf")).toContain("1")
      expect(processLine(state, "isLeaf (stem leaf)")).toContain("0")
    })

    it("let rec compiles with FIX (compile level)", () => {
      const defs = new Map<string, Tree>()
      const body = parseExpr("{n} -> n")
      const result = compileRecAndEval("myId", body as SExpr, defs)
      const budget = { remaining: 10000 }
      expect(treeEqual(apply(result, stem(LEAF), budget), stem(LEAF))).toBe(true)
    })

    it("let rec with self-reference via REPL", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)
      // Recursive function that references itself but terminates
      const def = processLine(state, "let rec myId : Nat -> Nat := {n} -> n")
      expect(def).not.toContain("Error")
      expect(processLine(state, "myId 5")).toMatch(/^5/)
    })
  })

  // --- Step counting / performance tests ---

  describe("Step counting", () => {
    it("arithmetic evaluates correctly", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)

      expect(processLine(state, "add 2 3")).toMatch(/^5/)
      expect(processLine(state, "mul 3 4")).toMatch(/^12/)
      expect(processLine(state, "add (mul 2 3) (mul 2 2)")).toMatch(/^10/)
    })

    it("fuel-based treeEq step counts", () => {
      const state = initialState()
      loadFile(state, "prelude.disp", true)

      const leafTree = state.defs.get("leaf")!
      const stemFn = state.defs.get("stem")!
      const forkFn = state.defs.get("fork")!
      const mkStem = (t: Tree) => apply(stemFn, t, { remaining: 1000 })
      const mkFork = (l: Tree, r: Tree) => apply(apply(forkFn, l, { remaining: 1000 }), r, { remaining: 1000 })

      const fuelTreeEq = state.defs.get("treeEq")!
      const churchTen = compileAndEval(parseExpr("10"), state.defs)

      const measure = (a: Tree, b: Tree) => {
        clearApplyCache()
        const budget = { remaining: 1000000 }
        apply(apply(apply(fuelTreeEq, churchTen, budget), a, budget), b, budget)
        return 1000000 - budget.remaining
      }

      const l = leafTree
      const s3 = mkStem(mkStem(mkStem(l)))
      const f = mkFork(mkStem(l), mkFork(l, l))

      const steps1 = measure(l, l)
      const steps2 = measure(s3, s3)
      const steps3 = measure(f, f)
      console.log(`  fuel(10) treeEq: leaf==leaf ${steps1}, stem^3==stem^3 ${steps2}, fork==fork ${steps3} steps`)

      // Verify all step counts are reasonable
      expect(steps1).toBeGreaterThan(0)
      expect(steps2).toBeGreaterThan(steps1)
      expect(steps3).toBeGreaterThan(steps1)
    })
  })
})
