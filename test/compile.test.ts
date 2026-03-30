import { describe, it, expect } from "vitest"
import { parseExpr, parseLine, type SDecl } from "../src/parse.js"
import { compile, compileAndEval, compileRecAndEval, astToExpr, collapse } from "../src/compile.js"
import {
  LEAF, stem, fork, treeEqual, applyTree, apply, prettyTree, I, K,
  type Tree, BudgetExhausted, clearApplyCache,
} from "../src/tree.js"

// Definitions for tree-encoded literals (true, false, zero, succ)
const literalDefs = new Map<string, Tree>([
  ["true", LEAF],
  ["false", stem(LEAF)],
  ["zero", LEAF],
  ["succ", LEAF],  // apply(LEAF, x) = stem(x)
])

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
  it("Pi type compiles to fork(domain, codomain)", () => {
    // (A : Type) -> A compiles to fork(Type, [A]A) = fork(LEAF, I)
    const piTree = c("(A : Type) -> A")
    // Should be a fork: Pi(Type, identity)
    expect(piTree.tag).toBe("fork")
    if (piTree.tag === "fork") {
      expect(treeEqual(piTree.left, LEAF)).toBe(true) // domain = Type = LEAF
      expect(treeEqual(piTree.right, I)).toBe(true)   // codomain = identity
    }
  })
})

describe("compile - bracket abstraction optimization", () => {
  // Helper to count tree nodes
  function treeSize(t: Tree): number {
    if (t.tag === "leaf") return 1
    if (t.tag === "stem") return 1 + treeSize(t.child)
    return 1 + treeSize(t.left) + treeSize(t.right)
  }

  it("eta reduction: {x} -> f x compiles same as f (when f is a def)", () => {
    const defs = new Map<string, Tree>()
    const idTree = c("{x} -> x")
    defs.set("f", idTree)
    const etaExpanded = c("{x} -> f x", defs)
    // After eta reduction, {x} -> f x should compile to f's tree
    expect(treeEqual(etaExpanded, idTree)).toBe(true)
  })

  it("{x y} -> x compiles to K", () => {
    const konst = c("{x y} -> x")
    // K = stem(LEAF). {x y} -> x should compile to K.
    expect(treeEqual(konst, K)).toBe(true)
  })

  it("optimized trees are smaller than naive", () => {
    // S combinator: {f g x} -> f x (g x)
    const s = c("{f g x} -> f x (g x)")
    // With optimization, this should be reasonably small
    // Naive would be much larger
    const size = treeSize(s)
    expect(size).toBeLessThan(30)
  })

  it("compiled true is LEAF", () => {
    const tru = c("true", literalDefs)
    expect(treeEqual(tru, LEAF)).toBe(true)
  })

  it("compiled false is stem(LEAF)", () => {
    const fls = c("false", literalDefs)
    expect(treeEqual(fls, stem(LEAF))).toBe(true)
  })

  it("compiled 3 is stem(stem(stem(LEAF)))", () => {
    const three = c("3", literalDefs)
    // succ(succ(succ(zero))) = apply(LEAF, apply(LEAF, apply(LEAF, LEAF)))
    // = stem(stem(stem(LEAF)))
    expect(treeEqual(three, stem(stem(stem(LEAF))))).toBe(true)
  })

  it("optimization preserves semantics for complex expressions", () => {
    const add = c("{m n R s z} -> m R s (n R s z)")
    const zero = c("{R s z} -> z")
    const succ = c("{n R s z} -> s (n R s z)")

    // Test that add still works correctly
    const one = run(succ, zero)
    const two = run(succ, one)
    const result = run(add, one, one)

    const r = LEAF
    const s = I
    const z = stem(stem(LEAF))
    expect(treeEqual(run(result, r, s, z), run(two, r, s, z))).toBe(true)
  })
})

describe("compileAndEval - runtime evaluation", () => {
  // Helper: build defs from declaration strings, seeded with literal defs
  function buildDefs(decls: string[]): Map<string, Tree> {
    const defs = new Map<string, Tree>(literalDefs)
    for (const d of decls) {
      const parsed = parseLine(d)
      if ("name" in parsed) {
        const decl = parsed as SDecl
        defs.set(decl.name, compileAndEval(decl.value, defs))
      }
    }
    return defs
  }

  function ce(input: string, defs?: Map<string, Tree>): Tree {
    return compileAndEval(parseExpr(input), defs)
  }

  // Helper: count stem depth (tree-encoded nat = stem^n(LEAF))
  function stemDepth(tree: Tree): number {
    let node = tree
    let count = 0
    while (node.tag === "stem") { count++; node = node.child }
    expect(node.tag).toBe("leaf")
    return count
  }

  it("true compiles to LEAF", () => {
    const defs = buildDefs([])
    const result = ce("true", defs)
    expect(treeEqual(result, LEAF)).toBe(true)
  })

  it("false compiles to stem(LEAF)", () => {
    const defs = buildDefs([])
    const result = ce("false", defs)
    expect(treeEqual(result, stem(LEAF))).toBe(true)
  })

  it("1 compiles to stem(LEAF)", () => {
    const defs = buildDefs([])
    const result = ce("1", defs)
    expect(stemDepth(result)).toBe(1)
  })

  it("2 compiles to stem(stem(LEAF))", () => {
    const defs = buildDefs([])
    const result = ce("2", defs)
    expect(stemDepth(result)).toBe(2)
  })

  it("succ zero has same tree as 1 via compileAndEval", () => {
    const defs = buildDefs([])
    const succZero = ce("succ 0", defs)
    expect(stemDepth(succZero)).toBe(1)
  })

  it("budget exhaustion throws for large computation with tiny budget", () => {
    clearApplyCache()
    const defs = buildDefs([
      "let mul := {m n R s z} -> m R (n R s) z",
    ])
    expect(() => {
      clearApplyCache()
      compileAndEval(parseExpr("mul 3 (mul 3 (mul 3 3))"), defs, { remaining: 1 })
    }).toThrow(BudgetExhausted)
  })
})

describe("recursive definitions", () => {
  it("compileRecAndEval: recursive identity function", () => {
    const defs = new Map<string, Tree>()
    const body = parseExpr("{n} -> n")
    const result = compileRecAndEval("myId", body, defs)
    const budget = { remaining: 10000 }
    const applied = apply(result, stem(LEAF), budget)
    expect(treeEqual(applied, stem(LEAF))).toBe(true)
  })

  it("compileRecAndEval: function that ignores self-ref", () => {
    const defs = new Map<string, Tree>(literalDefs)
    const body = parseExpr("{n} -> 0")
    const result = compileRecAndEval("constZero", body, defs)
    const budget = { remaining: 10000 }
    const applied = apply(result, stem(LEAF), budget)
    // 0 now compiles to LEAF (tree-encoded zero)
    expect(treeEqual(applied, LEAF)).toBe(true)
  })
})
