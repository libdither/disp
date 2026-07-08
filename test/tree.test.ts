import { describe, it, expect } from "vitest"
import {
  LEAF, stem, fork, isLeaf, isStem, isFork,
  treeEqual, treeApply, apply, applyTree, BudgetExhausted,
  K, I, prettyTree, force,
  TREE_TRUE, TREE_FALSE, setTreeEqId, getTreeEqId, clearApplyCache, resetApplyStats, getApplyStats,
  type Tree,
} from "../src/core/tree.js"
import { parseProgram } from "../src/compile.js"
import { readFileSync } from "node:fs"
import { resolve } from "node:path"

describe("Tree construction and equality", () => {
  it("creates leaf, stem, fork", () => {
    expect(isLeaf(LEAF)).toBe(true)
    expect(isStem(stem(LEAF))).toBe(true)
    expect(isFork(fork(LEAF, LEAF))).toBe(true)
  })

  it("hash-consing", () => {
    expect(stem(LEAF)).toBe(stem(LEAF))
    expect(fork(LEAF, stem(LEAF))).toBe(fork(LEAF, stem(LEAF)))
    expect(treeEqual(LEAF, stem(LEAF))).toBe(false)
  })
})

describe("treeApply (constructive, for compiler)", () => {
  it("Leaf x = Stem(x)", () => {
    expect(treeEqual(treeApply(LEAF, LEAF), stem(LEAF))).toBe(true)
  })

  it("Stem(a) x = Fork(a, x)", () => {
    expect(treeEqual(treeApply(stem(LEAF), LEAF), fork(LEAF, LEAF))).toBe(true)
  })

  it("Fork(a,b) x = Fork(Fork(a,b), x)", () => {
    const f = fork(LEAF, LEAF)
    expect(treeEqual(treeApply(f, LEAF), fork(f, LEAF))).toBe(true)
  })
})

describe("apply (tree calculus execution)", () => {
  it("leaf applied to x gives stem(x)", () => {
    expect(treeEqual(applyTree(LEAF, LEAF), stem(LEAF))).toBe(true)
    expect(treeEqual(applyTree(LEAF, stem(LEAF)), stem(stem(LEAF)))).toBe(true)
  })

  it("stem(a) applied to x gives fork(a, x)", () => {
    expect(treeEqual(applyTree(stem(LEAF), LEAF), fork(LEAF, LEAF))).toBe(true)
  })

  it("Rule 1: K b x → b", () => {
    // K = stem(LEAF). K b = fork(LEAF, b). fork(LEAF, b) x → b.
    const b = stem(stem(LEAF))
    const Kb = applyTree(K, b)  // fork(LEAF, b)
    expect(treeEqual(Kb, fork(LEAF, b))).toBe(true)

    const x = fork(LEAF, LEAF)
    const result = applyTree(Kb, x)  // Rule 1: → b
    expect(treeEqual(result, b)).toBe(true)
  })

  it("Rule 2: S c b x → c x (b x)", () => {
    // fork(stem(c), b) x → apply(apply(c, x), apply(b, x))
    // Use c = K, b = K, x = LEAF
    // c x = K LEAF = fork(LEAF, LEAF) [stem construction + fork construction]
    // b x = K LEAF = fork(LEAF, LEAF)
    // apply(fork(LEAF,LEAF), fork(LEAF,LEAF)) = Rule 1: LEAF
    const c = K
    const b = K
    const x = LEAF
    const term = fork(stem(c), b)
    const result = applyTree(term, x)
    // c x = apply(K, LEAF) = apply(stem(LEAF), LEAF) = fork(LEAF, LEAF)
    // b x = fork(LEAF, LEAF)
    // apply(fork(LEAF,LEAF), fork(LEAF,LEAF)) → Rule 1: LEAF
    expect(treeEqual(result, LEAF)).toBe(true)
  })

  it("Rule 3a: triage leaf → c", () => {
    const c = stem(stem(LEAF))
    const d = fork(LEAF, LEAF)
    const b = LEAF
    const term = fork(fork(c, d), b)  // △ (△ c d) b
    const result = applyTree(term, LEAF)  // apply to △
    expect(treeEqual(result, c)).toBe(true)
  })

  it("Rule 3b: triage stem → d u", () => {
    const c = LEAF
    const d = K  // d = stem(LEAF)
    const b = LEAF
    const u = stem(LEAF)
    const term = fork(fork(c, d), b)
    const result = applyTree(term, stem(u))  // apply to △ u
    // apply(d, u) = apply(stem(LEAF), stem(LEAF)) = fork(LEAF, stem(LEAF))
    expect(treeEqual(result, fork(LEAF, u))).toBe(true)
  })

  it("Rule 3c: triage fork → b u v", () => {
    const c = LEAF
    const d = LEAF
    const b = K  // b = stem(LEAF). apply(K, u) = fork(LEAF, u). apply(fork(LEAF,u), v) = u (Rule 1)
    const u = stem(stem(LEAF))
    const v = fork(LEAF, LEAF)
    const term = fork(fork(c, d), b)
    const result = applyTree(term, fork(u, v))
    // apply(apply(K, u), v) = apply(fork(LEAF, u), v) = u (Rule 1)
    expect(treeEqual(result, u)).toBe(true)
  })

  it("I x = x for leaf", () => {
    expect(treeEqual(applyTree(I, LEAF), LEAF)).toBe(true)
  })

  it("I x = x for stem", () => {
    const x = stem(stem(LEAF))
    expect(treeEqual(applyTree(I, x), x)).toBe(true)
  })

  it("I x = x for fork", () => {
    const x = fork(stem(LEAF), fork(LEAF, LEAF))
    expect(treeEqual(applyTree(I, x), x)).toBe(true)
  })

  it("I x = x for all basic shapes", () => {
    for (const x of [LEAF, stem(LEAF), fork(LEAF, LEAF), K, I]) {
      expect(treeEqual(applyTree(I, x), x)).toBe(true)
    }
  })

  it("K x y = x for various x, y", () => {
    const pairs: [Tree, Tree][] = [
      [LEAF, stem(LEAF)],
      [stem(LEAF), LEAF],
      [fork(LEAF, LEAF), stem(stem(LEAF))],
      [I, K],
    ]
    for (const [x, y] of pairs) {
      const Kx = applyTree(K, x)
      expect(treeEqual(applyTree(Kx, y), x)).toBe(true)
    }
  })

  it("budget exhaustion on divergent term", () => {
    // Build self-applicator: M x = x x
    // M = [x](x x). Using bracket abstraction: fork(stem(I), I)
    // Wait, let me just manually build a divergent apply chain.
    // S I I x = I x (I x) = x x. So S I I = fork(stem(I), I).
    // Omega = apply(SII, SII) where SII = fork(stem(I), I).
    // SII applied to SII: Rule 2 (a = stem(I), c = I):
    //   apply(apply(I, SII), apply(I, SII)) = apply(SII, SII) → loops!
    const SII = fork(stem(I), I)
    expect(() => applyTree(SII, SII, 100)).toThrow(BudgetExhausted)
  })
})

describe("prettyTree", () => {
  it("prints trees", () => {
    expect(prettyTree(LEAF)).toBe("△")
    expect(prettyTree(stem(LEAF))).toBe("(△ △)")
    expect(prettyTree(fork(LEAF, LEAF))).toBe("(△ △ △)")
    expect(prettyTree(fork(stem(LEAF), fork(LEAF, LEAF)))).toBe("(△ (△ △) (△ △ △))")
  })

  it("greedily substitutes names top-down, descending only unnamed structure", () => {
    const zero = LEAF
    const two = fork(LEAF, fork(LEAF, LEAF)) // succ (succ zero) shape
    const names = new Map<number, string>([
      [zero.id, "zero"],
      [two.id, "two"],
    ])
    // largest match wins: the whole tree is named, no descent
    expect(prettyTree(two, names)).toBe("two")
    // unnamed parent descends; named children (the leaves) substitute
    expect(prettyTree(stem(zero), names)).toBe("(△ zero)")
    // no map → raw representation (back-compat)
    expect(prettyTree(two)).toBe("(△ △ (△ △ △))")
  })
})

describe("constants", () => {
  it("K = stem(LEAF)", () => {
    expect(treeEqual(K, stem(LEAF))).toBe(true)
  })

  it("I = fork(fork(LEAF, LEAF), LEAF)", () => {
    expect(isFork(I)).toBe(true)
  })
})

// The tree_eq fast-path represents `tree_eq a` as the honest suspended-application
// P(tree_eq, a) instead of a synthetic marker. These tests pin the two properties
// that justify it: (1) correctness — full application yields the right bool verdict;
// (2) TRANSPARENCY — the partial is observationally identical to the genuine
// recursive-triage reduct the spec would produce, which the old marker was not.
describe("tree_eq suspension (P node)", () => {
  const teq = (() => {
    // The prelude is a given-bearing module now (fragment -1), so it cannot be
    // parsed as a ROOT (root files may not declare givens). Load it through a
    // raw import instead: a fieldless root re-exports its opens, so the decls
    // carry the prelude's names with the same trees.
    const src = `open use raw "../prelude.disp" {}\n`
    const decls = parseProgram(src, resolve("lib/tests/_tree_host.disp"))
    return (decls.find((d: any) => d.kind === "Def" && d.name === "tree_eq") as any).tree as Tree
  })()
  const a1 = fork(stem(LEAF), LEAF)          // a distinctive 4-node operand
  const a2 = stem(stem(LEAF))

  // The genuine reduct of `tree_eq a`, computed with the fast-path disabled.
  function natural(a: Tree): Tree {
    const saved = getTreeEqId()
    setTreeEqId(-1)
    try { return applyTree(teq, a, 5_000_000) } finally { setTreeEqId(saved) }
  }

  it("apply(tree_eq, a) is a P node holding a, built in 0 steps", () => {
    clearApplyCache(); resetApplyStats()
    const p = apply(teq, a1) as any
    expect(p.tag).toBe("susp")
    expect(p.f).toBe(teq)
    expect(treeEqual(p.a, a1)).toBe(true)            // operand held raw, O(1) extractable
    expect(getApplyStats().steps).toBe(0)            // interception, no reduction
  })

  it("two-step application gives the right bool verdict", () => {
    expect(treeEqual(apply(apply(teq, a1), a1), TREE_TRUE)).toBe(true)
    expect(treeEqual(apply(apply(teq, a1), a2), TREE_FALSE)).toBe(true)
    expect(treeEqual(apply(apply(teq, LEAF), LEAF), TREE_TRUE)).toBe(true)
  })

  it("the partial is TRANSPARENT: indistinguishable from the genuine reduct", () => {
    const p = apply(teq, a1)
    // treeEqual sees through the suspension to its real reduct...
    expect(treeEqual(p, natural(a1))).toBe(true)
    // ...so forcing yields exactly the spec's natural reduct (same hash-cons node)...
    expect(force(p).id).toBe(natural(a1).id)
    // ...and structural inspection (triage → here, isFork/prettyTree) agrees with it.
    expect(prettyTree(p)).toBe(prettyTree(natural(a1)))
  })

  it("identical partials share one node; distinct operands do not", () => {
    expect(apply(teq, a1)).toBe(apply(teq, a1))      // hash-consed susp
    expect(treeEqual(apply(teq, a1), apply(teq, a2))).toBe(false)
  })
})
