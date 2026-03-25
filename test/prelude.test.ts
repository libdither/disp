import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, treeEqual } from "../src/tree.js"
import { resetMarkerCounter, clearNativeBuiltins, unwrapData, unwrapType, TREE_TYPE, BOOL_TYPE, NAT_TYPE } from "../src/coc.js"
import { loadPrelude, buildNameMap } from "../src/prelude.js"

beforeEach(() => {
  resetMarkerCounter()
  clearNativeBuiltins()
})

describe("loadPrelude", () => {
  it("loads without error", () => {
    expect(() => loadPrelude()).not.toThrow()
  })

  it("returns env with primitive types", () => {
    const { cocEnv } = loadPrelude()
    expect(cocEnv.has("Tree")).toBe(true)
    expect(cocEnv.has("Bool")).toBe(true)
    expect(cocEnv.has("Nat")).toBe(true)
  })

  it("Tree type data is TREE_TYPE", () => {
    const { cocEnv } = loadPrelude()
    const treeWrapped = cocEnv.get("Tree")!
    expect(treeEqual(unwrapData(treeWrapped), TREE_TYPE)).toBe(true)
  })

  it("Bool type data is BOOL_TYPE", () => {
    const { cocEnv } = loadPrelude()
    const boolWrapped = cocEnv.get("Bool")!
    expect(treeEqual(unwrapData(boolWrapped), BOOL_TYPE)).toBe(true)
  })

  it("returns env with primitive builtins", () => {
    const { cocEnv } = loadPrelude()
    for (const name of ["leaf", "stem", "fork", "triage", "true", "false", "boolElim", "zero", "succ", "natElim"]) {
      expect(cocEnv.has(name), `missing primitive: ${name}`).toBe(true)
    }
  })

  it("returns env with tree-native builtins", () => {
    const { cocEnv } = loadPrelude()
    for (const name of ["tEncApp", "tEncLam", "tEncPi", "termCase", "treeEqStep", "whnfStep", "convertibleStep", "inferStep"]) {
      expect(cocEnv.has(name), `missing tree-native: ${name}`).toBe(true)
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

describe("buildNameMap", () => {
  it("maps tree IDs to names", () => {
    const { cocEnv } = loadPrelude()
    const nameMap = buildNameMap(cocEnv)
    // Should have entries for non-skipped builtins
    expect(nameMap.size).toBeGreaterThan(0)
  })

  it("skips colliding names", () => {
    const { cocEnv } = loadPrelude()
    const nameMap = buildNameMap(cocEnv)
    // Names like "leaf", "true", "zero" share LEAF data — should be skipped
    const names = Array.from(nameMap.values())
    expect(names).not.toContain("leaf")
    expect(names).not.toContain("true")
    expect(names).not.toContain("zero")
  })

  it("includes non-colliding builtins", () => {
    const { cocEnv } = loadPrelude()
    const nameMap = buildNameMap(cocEnv)
    const names = Array.from(nameMap.values())
    expect(names).toContain("boolElim")
    expect(names).toContain("triage")
  })
})
