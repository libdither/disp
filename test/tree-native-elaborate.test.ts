import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, apply, treeEqual, I } from "../src/tree.js"
import { resetMarkerCounter, clearNativeBuiltins } from "../src/native-utils.js"
import { loadPrelude, clearPreludeCache } from "../src/prelude.js"
import { parseLine, parseExpr, type SExpr, type SDecl } from "../src/parse.js"
import {
  TN_TYPE, TN_TREE, TN_BOOL, TN_NAT,
  tnArrow, type KnownDefs,
} from "../src/tree-native-checker.js"
import {
  buildNativeWrapped, collapseTypedExpr, typedBracketAbstract,
  nativeElabDecl, type NativeEnv, type TypedExpr,
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

describe("buildNativeWrapped", () => {
  it("elaborates Type", () => {
    const env: NativeEnv = new Map()
    const { texpr, type } = buildNativeWrapped({ tag: "stype" }, env)
    expect(treeEqual(type, TN_TYPE)).toBe(true)
    expect(treeEqual(collapseTypedExpr(texpr), LEAF)).toBe(true)
  })

  it("elaborates Tree", () => {
    const env: NativeEnv = new Map()
    const { type } = buildNativeWrapped({ tag: "stree" }, env)
    expect(treeEqual(type, TN_TYPE)).toBe(true)
  })

  it("elaborates variable lookup", () => {
    const env: NativeEnv = new Map()
    env.set("x", { tree: stem(LEAF), annTree: stem(LEAF), type: TN_NAT })
    const { texpr, type } = buildNativeWrapped({ tag: "svar", name: "x" }, env)
    expect(treeEqual(type, TN_NAT)).toBe(true)
    expect(treeEqual(collapseTypedExpr(texpr), stem(LEAF))).toBe(true)
  })

  it("elaborates non-dependent Pi type", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("Nat -> Nat")
    const { texpr, type } = buildNativeWrapped(sexpr, env, TN_TYPE)
    expect(treeEqual(type, TN_TYPE)).toBe(true)
    const piTree = collapseTypedExpr(texpr)
    expect(treeEqual(piTree, tnArrow(TN_NAT, TN_NAT))).toBe(true)
  })

  it("elaborates dependent Pi type", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("(A : Type) -> A -> A")
    const { texpr, type } = buildNativeWrapped(sexpr, env, TN_TYPE)
    expect(treeEqual(type, TN_TYPE)).toBe(true)
  })

  it("elaborates simple lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x} -> x")
    const expectedType = tnArrow(TN_NAT, TN_NAT)
    const { texpr, type } = buildNativeWrapped(sexpr, env, expectedType)
    expect(treeEqual(type, expectedType)).toBe(true)
  })

  it("elaborates constant lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x} -> true")
    const expectedType = tnArrow(TN_NAT, TN_BOOL)
    const { texpr, type } = buildNativeWrapped(sexpr, env, expectedType)
    expect(treeEqual(type, expectedType)).toBe(true)
  })

  it("elaborates application succ 0", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("succ 0")
    const { type } = buildNativeWrapped(sexpr, env)
    expect(treeEqual(type, TN_NAT)).toBe(true)
  })

  it("elaborates number literal 3", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("3")
    const { type } = buildNativeWrapped(sexpr, env)
    expect(treeEqual(type, TN_NAT)).toBe(true)
  })

  it("elaborates true/false", () => {
    const { env } = makeNativeEnv()
    const t = buildNativeWrapped(parseExpr("true"), env)
    expect(treeEqual(t.type, TN_BOOL)).toBe(true)
    const f = buildNativeWrapped(parseExpr("false"), env)
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
    const { type } = buildNativeWrapped(parseExpr("not2 true"), env2)
    expect(treeEqual(type, TN_BOOL)).toBe(true)
  })

  it("elaborates multi-param lambda", () => {
    const { env } = makeNativeEnv()
    const sexpr = parseExpr("{x y} -> x")
    const expectedType = tnArrow(TN_NAT, tnArrow(TN_BOOL, TN_NAT))
    const { type } = buildNativeWrapped(sexpr, env, expectedType)
    expect(treeEqual(type, expectedType)).toBe(true)
  })

  it("rejects unbound variable", () => {
    const env: NativeEnv = new Map()
    expect(() => buildNativeWrapped({ tag: "svar", name: "missing" }, env)).toThrow("Unbound variable")
  })

  it("rejects lambda without expected type", () => {
    const env: NativeEnv = new Map()
    const sexpr = parseExpr("{x} -> x")
    expect(() => buildNativeWrapped(sexpr, env)).toThrow("Cannot infer type")
  })
})

describe("typedBracketAbstract", () => {
  it("[x]x = I with codomain K(A)", () => {
    const texpr: TypedExpr = { tag: "tfvar", name: "x", type: TN_NAT }
    const { result, codomain } = typedBracketAbstract("x", texpr, TN_NAT)
    expect(result.tag).toBe("tI")
    expect(treeEqual(collapseTypedExpr(result), I)).toBe(true)
    expect(treeEqual(codomain, fork(LEAF, TN_NAT))).toBe(true)
  })

  it("[x]c = K(c) with codomain K(type)", () => {
    const texpr: TypedExpr = { tag: "tlit", tree: stem(LEAF), annTree: stem(LEAF), type: TN_NAT }
    const { result, codomain } = typedBracketAbstract("x", texpr, TN_BOOL)
    expect(result.tag).toBe("tK")
    // collapsed should be K(stem(LEAF)) = fork(LEAF, stem(LEAF))
    expect(treeEqual(collapseTypedExpr(result), fork(LEAF, stem(LEAF)))).toBe(true)
    // codomain should be K(Nat)
    expect(treeEqual(codomain, fork(LEAF, TN_NAT))).toBe(true)
  })
})

describe("collapseTypedExpr", () => {
  it("collapses tlit", () => {
    const texpr: TypedExpr = { tag: "tlit", tree: stem(LEAF), annTree: stem(LEAF), type: TN_NAT }
    expect(treeEqual(collapseTypedExpr(texpr), stem(LEAF))).toBe(true)
  })

  it("collapses tapp", () => {
    const f: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: tnArrow(TN_NAT, TN_NAT) }
    const x: TypedExpr = { tag: "tlit", tree: LEAF, annTree: LEAF, type: TN_NAT }
    const texpr: TypedExpr = { tag: "tapp", func: f, arg: x, type: TN_NAT }
    // apply(LEAF, LEAF) = stem(LEAF)
    expect(treeEqual(collapseTypedExpr(texpr), stem(LEAF))).toBe(true)
  })

  it("throws on free variable", () => {
    const texpr: TypedExpr = { tag: "tfvar", name: "x", type: TN_NAT }
    expect(() => collapseTypedExpr(texpr)).toThrow("Free variable")
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
    const { texpr, type } = buildNativeWrapped(sexpr, env, TN_TYPE)
    expect(treeEqual(type, TN_TYPE)).toBe(true)
    // The result should be a Pi type: fork(TN_TYPE, B) where B is dependent
    const piTree = collapseTypedExpr(texpr)
    expect(piTree.tag).toBe("fork")
  })

  it("elaborates polymorphic identity value {A x} -> x", () => {
    const { env } = makeNativeEnv()
    const polyIdType = parseExpr("(A : Type) -> A -> A")
    const { texpr: typeTexpr } = buildNativeWrapped(polyIdType, env, TN_TYPE)
    const expectedType = collapseTypedExpr(typeTexpr)

    const sexpr = parseExpr("{A x} -> x")
    const { type } = buildNativeWrapped(sexpr, env, expectedType)
    expect(treeEqual(type, expectedType)).toBe(true)
  })

  it("elaborates boolElim with constant motive", () => {
    const { env } = makeNativeEnv()
    // boolElim Bool false true b — this is `not`
    const sexpr = parseExpr("{b} -> boolElim Bool false true b")
    const expectedType = tnArrow(TN_BOOL, TN_BOOL)
    const { type } = buildNativeWrapped(sexpr, env, expectedType)
    expect(treeEqual(type, expectedType)).toBe(true)
  })

  it("elaborates natElim with constant motive", () => {
    const { env } = makeNativeEnv()
    // natElim Nat 0 ({n} -> succ n) 3 — this is identity on Nat via natElim
    const sexpr = parseExpr("natElim Nat 0 ({n} -> succ n) 3")
    const { type } = buildNativeWrapped(sexpr, env)
    expect(treeEqual(type, TN_NAT)).toBe(true)
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
