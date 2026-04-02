import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, apply, treeEqual, I } from "../src/tree.js"
import { resetMarkerCounter, clearNativeBuiltins } from "../src/native-utils.js"
import { loadPrelude, clearPreludeCache } from "../src/prelude.js"
import { parseLine, parseExpr, type SExpr, type SDecl } from "../src/parse.js"
import { bracketAbstract, eTree, eFvar, eApp, collapseAndEval } from "../src/compile.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow, type KnownDefs,
} from "../src/tree-native-checker.js"
import {
  buildDirect, nativeElabDecl, type NativeEnv,
  printNativeType,
} from "../src/tree-native-elaborate.js"

// Build a NativeEnv from the prelude
function makeNativeEnv(): { env: NativeEnv, defs: Map<string, import("../src/tree.js").Tree>, nativeDefs: KnownDefs } {
  const { defs, nativeDefs, nativeEnv } = loadPrelude()
  return { env: nativeEnv, defs, nativeDefs }
}

beforeEach(() => {
  resetMarkerCounter()
  clearNativeBuiltins()
  clearPreludeCache()
})

describe("buildDirect", () => {
  it("elaborates Type", () => {
    const env: NativeEnv = new Map()
    const result = buildDirect({ tag: "stype" }, env)
    expect(treeEqual(result.type, TN_TYPE)).toBe(true)
    expect(treeEqual(result.tree, LEAF)).toBe(true)
  })

  it("elaborates Tree", () => {
    const env: NativeEnv = new Map()
    const result = buildDirect({ tag: "stree" }, env)
    expect(treeEqual(result.type, TN_TYPE)).toBe(true)
  })

  it("elaborates variable lookup", () => {
    const env: NativeEnv = new Map()
    env.set("x", { tree: stem(LEAF), annTree: stem(LEAF), type: TN_NAT })
    const result = buildDirect({ tag: "svar", name: "x" }, env)
    expect(treeEqual(result.type, TN_NAT)).toBe(true)
    expect(treeEqual(result.tree, stem(LEAF))).toBe(true)
  })

  it("elaborates non-dependent Pi type", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("Nat -> Nat")
    const result = buildDirect(sexpr, env, TN_TYPE)
    expect(treeEqual(result.type, TN_TYPE)).toBe(true)
    expect(treeEqual(result.tree, tnArrow(TN_NAT, TN_NAT))).toBe(true)
  })

  it("elaborates dependent Pi type", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("(A : Type) -> A -> A")
    const result = buildDirect(sexpr, env, TN_TYPE)
    expect(treeEqual(result.type, TN_TYPE)).toBe(true)
  })

  it("elaborates simple lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x} -> x")
    const expectedType = tnArrow(TN_NAT, TN_NAT)
    const result = buildDirect(sexpr, env, expectedType)
    expect(treeEqual(result.type, expectedType)).toBe(true)
  })

  it("elaborates constant lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x} -> true")
    const expectedType = tnArrow(TN_NAT, TN_BOOL)
    const result = buildDirect(sexpr, env, expectedType)
    expect(treeEqual(result.type, expectedType)).toBe(true)
  })

  it("elaborates application succ 0", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("succ 0")
    const result = buildDirect(sexpr, env)
    expect(treeEqual(result.type, TN_NAT)).toBe(true)
  })

  it("elaborates number literal 3", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("3")
    const result = buildDirect(sexpr, env)
    expect(treeEqual(result.type, TN_NAT)).toBe(true)
  })

  it("elaborates true/false", () => {
    const { env } = makeNativeEnv()
    const t = buildDirect(parseExpr("true"), env)
    expect(treeEqual(t.type, TN_BOOL)).toBe(true)
    const f = buildDirect(parseExpr("false"), env)
    expect(treeEqual(f.type, TN_BOOL)).toBe(true)
  })

  it("elaborates not true via application", () => {
    const { env } = makeNativeEnv()
    // First define not in the env
    const { env: env2 } = (() => {
      const { env: e, defs, nativeDefs } = makeNativeEnv()
      const parsed = parseLine("let not2 : Bool -> Bool := {b} -> boolElim Bool false true b") as any
      return nativeElabDecl(e, defs, nativeDefs, parsed.name, parsed.type, parsed.value, parsed.isRec)
    })()
    const result = buildDirect(parseExpr("not2 true"), env2)
    expect(treeEqual(result.type, TN_BOOL)).toBe(true)
  })

  it("elaborates multi-param lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x y} -> x")
    const expectedType = tnArrow(TN_NAT, tnArrow(TN_BOOL, TN_NAT))
    const result = buildDirect(sexpr, env, expectedType)
    expect(treeEqual(result.type, expectedType)).toBe(true)
  })

  it("rejects unbound variable", () => {
    const env: NativeEnv = new Map()
    expect(() => buildDirect({ tag: "svar", name: "missing" }, env)).toThrow("Unbound variable")
  })

  it("rejects lambda without expected type", () => {
    const env: NativeEnv = new Map()
    const sexpr = parseExpr("{x} -> x")
    expect(() => buildDirect(sexpr, env)).toThrow("Cannot infer type")
  })
})

describe("bracketAbstract equivalence", () => {
  it("[x]x = I", () => {
    const expr = eFvar("x")
    const result = collapseAndEval(bracketAbstract("x", expr))
    expect(treeEqual(result, I)).toBe(true)
  })

  it("[x]c = K(c)", () => {
    const expr = eTree(stem(LEAF))
    const result = collapseAndEval(bracketAbstract("x", expr))
    expect(treeEqual(result, fork(LEAF, stem(LEAF)))).toBe(true)
  })
})

describe("nativeElabDecl", () => {
  it("elaborates simple non-recursive declaration", () => {
    const { env, defs, nativeDefs } = makeNativeEnv()
    const parsed = parseLine("let kTrue : Bool -> Bool := {x} -> true") as SDecl

    const result = nativeElabDecl(env, defs, nativeDefs, parsed.name, parsed.type, parsed.value, parsed.isRec)
    expect(treeEqual(result.nativeType, tnArrow(TN_BOOL, TN_BOOL))).toBe(true)
    expect(result.compiled).toBeDefined()
  })

  it("elaborates not : Bool -> Bool", () => {
    const { env, defs, nativeDefs } = makeNativeEnv()
    const parsed = parseLine("let not2 : Bool -> Bool := {b} -> boolElim Bool false true b") as SDecl

    const result = nativeElabDecl(env, defs, nativeDefs, parsed.name, parsed.type, parsed.value, parsed.isRec)
    expect(treeEqual(result.nativeType, tnArrow(TN_BOOL, TN_BOOL))).toBe(true)
  })

  it("elaborates recursive declaration", () => {
    const { env, defs, nativeDefs } = makeNativeEnv()
    const parsed = parseLine("let rec add2 : Nat -> Nat -> Nat := {m n} -> natElim Nat n ({m'} -> succ (add2 m' n)) m") as SDecl

    const result = nativeElabDecl(env, defs, nativeDefs, parsed.name, parsed.type, parsed.value, parsed.isRec)
    expect(treeEqual(result.nativeType, tnArrow(TN_NAT, tnArrow(TN_NAT, TN_NAT)))).toBe(true)
  })

  it("rejects recursive without type annotation", () => {
    const { env, defs, nativeDefs } = makeNativeEnv()
    expect(() => {
      nativeElabDecl(env, defs, nativeDefs, "f", null, parseExpr("{x} -> x"), true)
    }).toThrow("requires a type annotation")
  })
})

describe("dependent type elaboration", () => {
  it("elaborates polymorphic identity type (A : Type) -> A -> A", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("(A : Type) -> A -> A")
    const result = buildDirect(sexpr, env, TN_TYPE)
    expect(treeEqual(result.type, TN_TYPE)).toBe(true)
    // The result should be a Pi type: fork(TN_TYPE, B) where B is dependent
    expect(result.tree.tag).toBe("fork")
  })

  it("elaborates polymorphic identity value {A x} -> x", () => {
    const { env } = makeNativeEnv()
    const polyIdType = parseExpr("(A : Type) -> A -> A")
    const typeResult = buildDirect(polyIdType, env, TN_TYPE)
    const expectedType = typeResult.tree

    const sexpr = parseExpr("{A x} -> x")
    const result = buildDirect(sexpr, env, expectedType)
    expect(treeEqual(result.type, expectedType)).toBe(true)
  })

  it("elaborates boolElim with constant motive", () => {
    const { env } = makeNativeEnv()
    // boolElim Bool false true b — this is `not`
    const sexpr = parseExpr("{b} -> boolElim Bool false true b")
    const expectedType = tnArrow(TN_BOOL, TN_BOOL)
    const result = buildDirect(sexpr, env, expectedType)
    expect(treeEqual(result.type, expectedType)).toBe(true)
  })

  it("elaborates natElim with constant motive", () => {
    const { env } = makeNativeEnv()
    // natElim Nat 0 ({n} -> succ n) 3 — this is identity on Nat via natElim
    const sexpr = parseExpr("natElim Nat 0 ({n} -> succ n) 3")
    const result = buildDirect(sexpr, env)
    expect(treeEqual(result.type, TN_NAT)).toBe(true)
  })
})

describe("printNativeType", () => {
  it("prints base types", () => {
    expect(printNativeType(TN_TYPE)).toBe("Type")
    expect(printNativeType(TN_TREE)).toBe("Tree")
    expect(printNativeType(TN_BOOL)).toBe("Bool")
    expect(printNativeType(TN_NAT)).toBe("Nat")
  })

  it("prints non-dependent arrow", () => {
    expect(printNativeType(tnArrow(TN_NAT, TN_BOOL))).toBe("Nat -> Bool")
  })

  it("prints nested arrows", () => {
    expect(printNativeType(tnArrow(TN_NAT, tnArrow(TN_NAT, TN_NAT)))).toBe("Nat -> Nat -> Nat")
  })

  it("prints arrow with complex domain", () => {
    expect(printNativeType(tnArrow(tnArrow(TN_NAT, TN_BOOL), TN_NAT))).toBe("(Nat -> Bool) -> Nat")
  })
})
