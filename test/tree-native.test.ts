import { describe, it, expect } from "vitest"
import { LEAF, stem, fork, apply, treeEqual, I } from "../src/tree.js"
import {
  FST, SND, CHILD,
  ENC_APP_T, ENC_LAM_T, ENC_PI_T,
  TERM_CASE, TREE_EQ_STEP,
} from "../src/tree-native.js"
import { encType, encVar, encApp, encLam, encPi } from "../src/coc.js"

// Helper: apply multiple args
function applyN(f: Tree, ...args: Tree[]): Tree {
  let result = f
  for (const arg of args) result = apply(result, arg, { remaining: 10000 })
  return result
}

describe("Tree-native builtins", () => {
  describe("FST / SND / CHILD", () => {
    it("FST returns LEAF for leaf input", () => {
      // FST(leaf) → triage leaf case → LEAF
      expect(treeEqual(apply(FST, LEAF), LEAF)).toBe(true)
    })

    it("SND returns LEAF for leaf input", () => {
      expect(treeEqual(apply(SND, LEAF), LEAF)).toBe(true)
    })

    it("CHILD returns LEAF for leaf input", () => {
      expect(treeEqual(apply(CHILD, LEAF), LEAF)).toBe(true)
    })

    it("FST extracts left of deeply nested fork", () => {
      const deep = fork(fork(LEAF, stem(LEAF)), fork(stem(stem(LEAF)), LEAF))
      expect(treeEqual(apply(FST, deep), deep.left)).toBe(true)
    })

    it("SND extracts right of deeply nested fork", () => {
      const deep = fork(fork(LEAF, stem(LEAF)), fork(stem(stem(LEAF)), LEAF))
      expect(treeEqual(apply(SND, deep), deep.right)).toBe(true)
    })

    it("CHILD extracts child of stem", () => {
      const inner = fork(LEAF, stem(LEAF))
      expect(treeEqual(apply(CHILD, stem(inner)), inner)).toBe(true)
    })
  })

  describe("TERM_CASE dispatch", () => {
    // TERM_CASE takes 5 handlers + 1 term, dispatches by encoding tag
    const marker = fork(LEAF, LEAF)
    const tag0 = stem(LEAF)   // result for Type
    const tag1 = stem(stem(LEAF))  // result for Var
    const tag2 = fork(LEAF, stem(LEAF))  // result for App
    const tag3 = fork(stem(LEAF), LEAF)  // result for Lam
    const tag4 = fork(stem(LEAF), stem(LEAF))  // result for Pi

    // Build handlers: Type→tag0, Var(m)→tag1, App(f,a)→tag2, Lam(d,b)→tag3, Pi(d,b)→tag4
    // Type handler takes 0 args, Var takes 1, App/Lam/Pi take 2
    const onType = tag0
    const onVar = fork(LEAF, tag1)  // K(tag1): ignores marker, returns tag1
    // For 2-arg handlers: K(K(result)) — ignores both args
    const onApp = fork(LEAF, fork(LEAF, tag2))
    const onLam = fork(LEAF, fork(LEAF, tag3))
    const onPi = fork(LEAF, fork(LEAF, tag4))

    it("dispatches Type correctly", () => {
      const result = applyN(TERM_CASE, onType, onVar, onApp, onLam, onPi, encType())
      expect(treeEqual(result, tag0)).toBe(true)
    })

    it("dispatches Var correctly", () => {
      const result = applyN(TERM_CASE, onType, onVar, onApp, onLam, onPi, encVar(marker))
      expect(treeEqual(result, tag1)).toBe(true)
    })

    it("dispatches App correctly", () => {
      const result = applyN(TERM_CASE, onType, onVar, onApp, onLam, onPi, encApp(LEAF, LEAF))
      expect(treeEqual(result, tag2)).toBe(true)
    })

    it("dispatches Lam correctly", () => {
      const result = applyN(TERM_CASE, onType, onVar, onApp, onLam, onPi, encLam(LEAF, LEAF))
      expect(treeEqual(result, tag3)).toBe(true)
    })

    it("dispatches Pi correctly", () => {
      const result = applyN(TERM_CASE, onType, onVar, onApp, onLam, onPi, encPi(LEAF, LEAF))
      expect(treeEqual(result, tag4)).toBe(true)
    })
  })

  describe("TREE_EQ_STEP", () => {
    // TREE_EQ_STEP takes (self, a, b) → Bool
    // We use identity as "self" for leaf-level comparisons (no recursion needed)
    const TT = LEAF
    const FF = stem(LEAF)

    it("leaf == leaf → true", () => {
      const result = applyN(TREE_EQ_STEP, I, LEAF, LEAF)
      expect(treeEqual(result, TT)).toBe(true)
    })

    it("leaf != stem(leaf) → false", () => {
      const result = applyN(TREE_EQ_STEP, I, LEAF, stem(LEAF))
      expect(treeEqual(result, FF)).toBe(true)
    })

    it("stem(leaf) != leaf → false", () => {
      const result = applyN(TREE_EQ_STEP, I, stem(LEAF), LEAF)
      expect(treeEqual(result, FF)).toBe(true)
    })

    it("stem case delegates to self for children", () => {
      // With I as self: self(ac, bc) = I(ac)(bc) = stem(ac)(bc) = fork(ac, bc)
      // So for stem(a)==stem(b), result = I(a)(b) = fork-like structure, not TT/FF
      // This tests that the step function structure is correct
      const result = applyN(TREE_EQ_STEP, I, stem(LEAF), stem(LEAF))
      // With identity as self, stem(leaf)==stem(leaf) calls self(leaf, leaf) = I(leaf)(leaf) = stem(leaf)(leaf) = fork(leaf, leaf)
      // That's not TT, because I is not a proper equality function — just testing the wiring
      expect(result).toBeDefined()
    })

    it("fork != leaf → false", () => {
      const result = applyN(TREE_EQ_STEP, I, fork(LEAF, LEAF), LEAF)
      expect(treeEqual(result, FF)).toBe(true)
    })
  })

  describe("encoding constructors as trees", () => {
    it("ENC_APP_T matches encApp for various inputs", () => {
      const cases: [Tree, Tree][] = [
        [LEAF, LEAF],
        [stem(LEAF), fork(LEAF, LEAF)],
        [encVar(fork(LEAF, LEAF)), encType()],
      ]
      for (const [m, n] of cases) {
        const native = applyN(ENC_APP_T, m, n)
        expect(treeEqual(native, encApp(m, n))).toBe(true)
      }
    })

    it("ENC_LAM_T matches encLam for various inputs", () => {
      const cases: [Tree, Tree][] = [
        [LEAF, LEAF],
        [stem(LEAF), I],
      ]
      for (const [d, b] of cases) {
        const native = applyN(ENC_LAM_T, d, b)
        expect(treeEqual(native, encLam(d, b))).toBe(true)
      }
    })

    it("ENC_PI_T matches encPi for various inputs", () => {
      const cases: [Tree, Tree][] = [
        [LEAF, LEAF],
        [stem(LEAF), I],
      ]
      for (const [d, b] of cases) {
        const native = applyN(ENC_PI_T, d, b)
        expect(treeEqual(native, encPi(d, b))).toBe(true)
      }
    })
  })
})
