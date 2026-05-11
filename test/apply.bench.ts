// Evaluator microbenches — measure raw applyTree cost across representative
// workloads. Run with `vitest bench` or `npm run bench`.
// Skipped during normal `vitest run` / `npm test`.
//
// Two modes:
//   - "cold cache": clearApplyCache() before each iteration. Measures the
//     real cost of evaluating from scratch.
//   - "warm cache": caches retained across iterations after a one-time
//     warm-up. Most subresults memoize on the first call so subsequent
//     iterations measure amortized cache-hit overhead.

import { describe, bench, beforeAll } from "vitest"
import { resolve } from "node:path"
import { parseProgram } from "../src/compile.js"
import {
  type Tree, LEAF, I, applyTree,
  clearApplyCache, resetApplyStats, resetCacheStats,
} from "../src/tree.js"

// Synthetic source: opens kernel/prelude + std/nat/arith so we have
// tree_eq, nat_rec, add, succ in scope; binds them to bench_* names so
// they survive as Defs. Numeric literals pre-compile to closed Nat trees.
const WORKLOAD = `
open use "kernel/prelude.disp"
open use "std/nat/arith.disp"

bench_tree_eq := tree_eq
bench_add := add
bench_succ := succ
bench_zero := 0
bench_three := 3
bench_five := 5
bench_small_tree := t (t t) (t t (t t))
`

const trees: Record<string, Tree> = {}
const BIG_BUDGET = 10_000_000

beforeAll(() => {
  // Synthetic source path inside lib/ so relative imports resolve against
  // the real library tree. The file need not exist on disk.
  const synthPath = resolve("lib/__bench_apply__.disp")
  const decls = parseProgram(WORKLOAD, synthPath)
  for (const d of decls) {
    if (d.kind === "Def") trees[d.name] = d.tree
  }
  resetApplyStats()
  resetCacheStats()
})

// --- Cold cache: full evaluation each iteration ------------------------

describe("apply (cold cache)", () => {
  bench("I LEAF (identity triage)", () => {
    clearApplyCache()
    applyTree(I, LEAF, BIG_BUDGET)
  })

  bench("tree_eq T T (fast-path TT)", () => {
    clearApplyCache()
    const t1 = applyTree(trees.bench_tree_eq, trees.bench_small_tree, BIG_BUDGET)
    applyTree(t1, trees.bench_small_tree, BIG_BUDGET)
  })

  bench("tree_eq 3 5 (fast-path FF)", () => {
    clearApplyCache()
    const t1 = applyTree(trees.bench_tree_eq, trees.bench_three, BIG_BUDGET)
    applyTree(t1, trees.bench_five, BIG_BUDGET)
  })

  bench("add 3 5 (nat_rec eliminator)", () => {
    clearApplyCache()
    const t1 = applyTree(trees.bench_add, trees.bench_three, BIG_BUDGET)
    applyTree(t1, trees.bench_five, BIG_BUDGET)
  })

  bench("succ 0 (single fork build)", () => {
    clearApplyCache()
    applyTree(trees.bench_succ, trees.bench_zero, BIG_BUDGET)
  })
})

// --- Warm cache: amortized cost after first call ----------------------

describe("apply (warm cache)", () => {
  beforeAll(() => {
    clearApplyCache()
    // One-time warm-up of every workload.
    applyTree(I, LEAF, BIG_BUDGET)
    const e1 = applyTree(trees.bench_tree_eq, trees.bench_small_tree, BIG_BUDGET)
    applyTree(e1, trees.bench_small_tree, BIG_BUDGET)
    const e2 = applyTree(trees.bench_tree_eq, trees.bench_three, BIG_BUDGET)
    applyTree(e2, trees.bench_five, BIG_BUDGET)
    const a1 = applyTree(trees.bench_add, trees.bench_three, BIG_BUDGET)
    applyTree(a1, trees.bench_five, BIG_BUDGET)
    applyTree(trees.bench_succ, trees.bench_zero, BIG_BUDGET)
  })

  bench("I LEAF", () => { applyTree(I, LEAF, BIG_BUDGET) })

  bench("tree_eq T T", () => {
    const t1 = applyTree(trees.bench_tree_eq, trees.bench_small_tree, BIG_BUDGET)
    applyTree(t1, trees.bench_small_tree, BIG_BUDGET)
  })

  bench("tree_eq 3 5", () => {
    const t1 = applyTree(trees.bench_tree_eq, trees.bench_three, BIG_BUDGET)
    applyTree(t1, trees.bench_five, BIG_BUDGET)
  })

  bench("add 3 5", () => {
    const t1 = applyTree(trees.bench_add, trees.bench_three, BIG_BUDGET)
    applyTree(t1, trees.bench_five, BIG_BUDGET)
  })

  bench("succ 0", () => {
    applyTree(trees.bench_succ, trees.bench_zero, BIG_BUDGET)
  })
})
