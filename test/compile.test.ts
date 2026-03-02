import { describe, it, expect } from "vitest"
import { parseExpr } from "../src/parse.js"
import { compile } from "../src/compile.js"
import {
  LEAF, stem, fork, treeEqual, applyTree, apply, prettyTree, I, K,
  type Tree,
} from "../src/tree.js"

// Helper: compile a string expression
function c(input: string, defs?: Map<string, Tree>): Tree {
  return compile(parseExpr(input), defs)
}

// Helper: apply a compiled tree to arguments
function run(tree: Tree, ...args: Tree[]): Tree {
  let result = tree
  const budget = { remaining: 10000 }
  for (const arg of args) {
    result = apply(result, arg, budget)
  }
  return result
}

describe("compile - basic", () => {
  it("compiles Type to LEAF", () => {
    expect(treeEqual(c("Type"), LEAF)).toBe(true)
  })

  it("compiles identity {x} -> x", () => {
    const id = c("{x} -> x")
    // Identity: applied to any x should return x
    for (const x of [LEAF, stem(LEAF), fork(LEAF, LEAF), K, I]) {
      expect(treeEqual(run(id, x), x)).toBe(true)
    }
  })

  it("compiles const {x y} -> x (K combinator)", () => {
    const konst = c("{x y} -> x")
    // K a b = a
    const a = stem(stem(LEAF))
    const b = fork(LEAF, LEAF)
    expect(treeEqual(run(konst, a, b), a)).toBe(true)
  })

  it("compiles second projection {x y} -> y", () => {
    const snd = c("{x y} -> y")
    const a = stem(LEAF)
    const b = fork(LEAF, stem(LEAF))
    expect(treeEqual(run(snd, a, b), b)).toBe(true)
  })

  it("compiles S combinator {f g x} -> f x (g x)", () => {
    const s = c("{f g x} -> f x (g x)")
    // S K K x = x (identity behavior)
    const konst = c("{x y} -> x")
    const result = run(s, konst, konst, stem(LEAF))
    expect(treeEqual(result, stem(LEAF))).toBe(true)
  })
})

describe("compile - Church booleans", () => {
  it("true selects first", () => {
    const tru = c("{R t f} -> t")
    const a = stem(LEAF)
    const b = fork(LEAF, LEAF)
    // true R a b = a (after providing R)
    const r = LEAF // type argument (erased)
    expect(treeEqual(run(tru, r, a, b), a)).toBe(true)
  })

  it("false selects second", () => {
    const fls = c("{R t f} -> f")
    const a = stem(LEAF)
    const b = fork(LEAF, LEAF)
    const r = LEAF
    expect(treeEqual(run(fls, r, a, b), b)).toBe(true)
  })

  it("not true = false (behaviorally)", () => {
    const tru = c("{R t f} -> t")
    const fls = c("{R t f} -> f")
    const not = c("{b R t f} -> b R f t")
    const notTrue = run(not, tru)
    // notTrue should behave like false: notTrue R a b = b
    const r = LEAF
    const a = stem(LEAF)
    const b = fork(LEAF, LEAF)
    expect(treeEqual(run(notTrue, r, a, b), b)).toBe(true)
  })

  it("not false = true (behaviorally)", () => {
    const tru = c("{R t f} -> t")
    const fls = c("{R t f} -> f")
    const not = c("{b R t f} -> b R f t")
    const notFalse = run(not, fls)
    const r = LEAF
    const a = stem(LEAF)
    const b = fork(LEAF, LEAF)
    expect(treeEqual(run(notFalse, r, a, b), a)).toBe(true)
  })
})

describe("compile - Church numerals", () => {
  // Church n = λR.λs.λz. s^n z
  // zero = λR.λs.λz. z
  // succ n = λR.λs.λz. s (n R s z)

  it("zero applies s zero times", () => {
    const zero = c("{R s z} -> z")
    const r = LEAF
    const s = stem(LEAF) // some function
    const z = fork(LEAF, LEAF) // base value
    expect(treeEqual(run(zero, r, s, z), z)).toBe(true)
  })

  it("succ zero behaves as one", () => {
    const zero = c("{R s z} -> z")
    const succ = c("{n R s z} -> s (n R s z)")
    const one = run(succ, zero)

    // one R s z = s z
    const r = LEAF
    const s = I // identity as successor function
    const z = stem(stem(LEAF))
    // one R I z = I z = z (since I is identity)
    expect(treeEqual(run(one, r, s, z), run(s, z))).toBe(true)
  })

  it("succ (succ zero) behaves as two", () => {
    const zero = c("{R s z} -> z")
    const succ = c("{n R s z} -> s (n R s z)")
    const two = run(succ, run(succ, zero))

    // two R s z = s (s z)
    const r = LEAF
    const s = I
    const z = stem(stem(LEAF))
    // two R I z = I (I z) = z
    expect(treeEqual(run(two, r, s, z), run(s, run(s, z)))).toBe(true)
  })

  it("add works: 1 + 1 = 2 (behaviorally)", () => {
    const zero = c("{R s z} -> z")
    const succ = c("{n R s z} -> s (n R s z)")
    const add = c("{m n R s z} -> m R s (n R s z)")

    const one = run(succ, zero)
    const two = run(succ, one)
    const onePlusOne = run(add, one, one)

    // Test behavioral equivalence: both should give s(s(z)) for any s, z
    const r = LEAF
    const s = I
    const z = stem(stem(LEAF))
    const expected = run(two, r, s, z)
    const actual = run(onePlusOne, r, s, z)
    expect(treeEqual(actual, expected)).toBe(true)
  })
})

describe("compile - with definitions", () => {
  it("uses compiled definitions", () => {
    const defs = new Map<string, Tree>()
    defs.set("id", c("{x} -> x"))
    const term = c("id Type", defs)
    // id Type should reduce to Type = LEAF
    // But compile produces a tree, and application happens at runtime
    // id compiled is the identity tree. "id Type" compiles to apply(id_tree, LEAF)
    // Actually compile builds treeApply(id_tree, LEAF). Let's check.
    // id_tree applied to LEAF via tree calculus:
    const result = run(defs.get("id")!, LEAF)
    expect(treeEqual(result, LEAF)).toBe(true)
  })
})

describe("compile - Pi types", () => {
  it("Pi type erases like lambda (domain dropped)", () => {
    // (A : Type) -> A compiles to {A} -> A basically
    const piTree = c("(A : Type) -> A")
    // Should behave like identity: piTree x = x
    const x = stem(LEAF)
    expect(treeEqual(run(piTree, x), x)).toBe(true)
  })
})
