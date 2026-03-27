import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, treeEqual } from "../src/tree.js"
import { resetMarkerCounter, clearNativeBuiltins } from "../src/native-utils.js"
import { TN_TREE, TN_BOOL, TN_NAT, TN_TYPE } from "../src/tree-native-checker.js"
import { loadPrelude, clearPreludeCache } from "../src/prelude.js"

beforeEach(() => {
  resetMarkerCounter()
  clearNativeBuiltins()
  clearPreludeCache()
})

describe("loadPrelude", () => {
  it("loads without error", () => {
    expect(() => loadPrelude()).not.toThrow()
  })

  it("returns nativeEnv with primitive types", () => {
    const { nativeEnv } = loadPrelude()
    expect(nativeEnv.has("Tree")).toBe(true)
    expect(nativeEnv.has("Bool")).toBe(true)
    expect(nativeEnv.has("Nat")).toBe(true)
  })

  it("Tree native type is TN_TREE", () => {
    const { nativeEnv } = loadPrelude()
    const entry = nativeEnv.get("Tree")!
    expect(treeEqual(entry.tree, TN_TREE)).toBe(true)
    expect(treeEqual(entry.type, TN_TYPE)).toBe(true)
  })

  it("Bool native type is TN_BOOL", () => {
    const { nativeEnv } = loadPrelude()
    const entry = nativeEnv.get("Bool")!
    expect(treeEqual(entry.tree, TN_BOOL)).toBe(true)
    expect(treeEqual(entry.type, TN_TYPE)).toBe(true)
  })

  it("returns nativeEnv with primitive builtins", () => {
    const { nativeEnv } = loadPrelude()
    for (const name of ["leaf", "stem", "fork", "triage", "true", "false", "boolElim", "zero", "succ", "natElim"]) {
      expect(nativeEnv.has(name), `missing primitive: ${name}`).toBe(true)
    }
  })

  it("returns nativeEnv with tree-native builtins", () => {
    const { nativeEnv } = loadPrelude()
    for (const name of ["tEncApp", "tEncLam", "tEncPi", "termCase", "treeEqStep", "whnfStep", "convertibleStep", "inferStep"]) {
      expect(nativeEnv.has(name), `missing tree-native: ${name}`).toBe(true)
    }
  })

  it("returns defs map with all builtins", () => {
    const { defs } = loadPrelude()
    expect(defs.has("leaf")).toBe(true)
    expect(defs.has("true")).toBe(true)
    expect(defs.has("treeEqStep")).toBe(true)
  })

  it("primitive data matches expected trees", () => {
    const { defs } = loadPrelude()
    expect(treeEqual(defs.get("leaf")!, LEAF)).toBe(true)
    expect(treeEqual(defs.get("true")!, LEAF)).toBe(true)
    expect(treeEqual(defs.get("false")!, stem(LEAF))).toBe(true)
    expect(treeEqual(defs.get("zero")!, LEAF)).toBe(true)
  })
})

describe("nativeEnv contents", () => {
  it("has entries for types and builtins", () => {
    const { nativeEnv } = loadPrelude()
    expect(nativeEnv.size).toBeGreaterThan(0)
  })

  it("each entry has tree, annTree, and type", () => {
    const { nativeEnv } = loadPrelude()
    for (const [name, entry] of nativeEnv) {
      expect(entry.tree, `${name} missing tree`).toBeDefined()
      expect(entry.annTree, `${name} missing annTree`).toBeDefined()
      expect(entry.type, `${name} missing type`).toBeDefined()
    }
  })

  it("type entries have type TN_TYPE", () => {
    const { nativeEnv } = loadPrelude()
    for (const name of ["Tree", "Bool", "Nat"]) {
      expect(treeEqual(nativeEnv.get(name)!.type, TN_TYPE)).toBe(true)
    }
  })
})
