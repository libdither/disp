import { describe, it, expect } from "vitest"
import { type Tree, LEAF, stem, fork, apply, isLeaf, isStem, isFork, clearApplyCache, treeEqual, FAST_EQ } from "../src/tree.js"
import {
  TREE_EQ_STEP, WHNF_STEP, ABSTRACT_OUT_STEP, CONVERTIBLE_STEP, TYPECHECK,
} from "../src/tree-native.js"
import { loadPrelude } from "../src/prelude.js"
import { initialState, loadFile, processLine } from "../src/repl.js"
import { encType, encPi } from "../src/coc.js"
import { compileAndEval, bracketAbstract, eTree, eFvar, eApp, collapseAndEval } from "../src/compile.js"
import { parseExpr } from "../src/parse.js"

// --- Utilities ---

/** Count unique nodes in the tree DAG (shared subtrees counted once) */
function treeSize(t: Tree): number {
  const visited = new Set<number>()
  const stack: Tree[] = [t]
  while (stack.length > 0) {
    const node = stack.pop()!
    if (visited.has(node.id)) continue
    visited.add(node.id)
    if (isStem(node)) stack.push(node.child)
    else if (isFork(node)) { stack.push(node.left); stack.push(node.right) }
  }
  return visited.size
}

/** Count total nodes if the tree were fully expanded (no sharing) */
function treeSizeUnshared(t: Tree): number {
  if (isLeaf(t)) return 1
  if (isStem(t)) return 1 + treeSizeUnshared(t.child)
  return 1 + treeSizeUnshared(t.left) + treeSizeUnshared(t.right)
}

interface ApplyStats {
  totalCalls: number
  uniquePairs: number
  leafCalls: number
  stemCalls: number
  kCalls: number
  sCalls: number
  triageCalls: number
}

/**
 * Instrumented apply that collects statistics.
 * Mirrors the logic of apply() exactly but tracks metrics.
 */
// FAST_EQ_MARKER is FAST_EQ's child (FAST_EQ = stem(MARKER))
const FAST_EQ_MARKER_ID = isStem(FAST_EQ) ? FAST_EQ.child.id : -1

function applyInstrumented(
  f: Tree, x: Tree,
  budget: { remaining: number },
  stats: ApplyStats,
  seen: Set<string>,
): Tree {
  if (budget.remaining <= 0) throw new Error("Budget exhausted")
  stats.totalCalls++
  const key = `${f.id},${x.id}`
  if (!seen.has(key)) {
    seen.add(key)
    stats.uniquePairs++
  }

  budget.remaining--

  if (isLeaf(f)) { stats.leafCalls++; return stem(x) }
  if (isStem(f)) { stats.stemCalls++; return fork(f.child, x) }

  // FAST_EQ shortcut: fork(FAST_EQ_MARKER, a) applied to b → O(1) identity check
  if (f.left.id === FAST_EQ_MARKER_ID) {
    return treeEqual(f.right, x) ? LEAF : stem(LEAF)
  }

  const a = f.left
  const b = f.right

  if (isLeaf(a)) {
    stats.kCalls++
    return b
  }
  if (isStem(a)) {
    stats.sCalls++
    const c = a.child
    if (isFork(c) && isLeaf(c.left)) {
      // B combinator fast-path: S(K(f)) g x = f (g x)
      if (isFork(b) && isLeaf(b.left)) {
        // S(K f)(K g) x = f g
        return applyInstrumented(c.right, b.right, budget, stats, seen)
      }
      const gx = applyInstrumented(b, x, budget, stats, seen)
      return applyInstrumented(c.right, gx, budget, stats, seen)
    }
    const cx = applyInstrumented(c, x, budget, stats, seen)
    if (isFork(cx) && isLeaf(cx.left)) {
      // Speculative K check: cx is K(v), skip bx
      return cx.right
    }
    if (isFork(b) && isLeaf(b.left)) {
      // C combinator: S f (K g) x = (f x) g
      return applyInstrumented(cx, b.right, budget, stats, seen)
    }
    const bx = applyInstrumented(b, x, budget, stats, seen)
    return applyInstrumented(cx, bx, budget, stats, seen)
  }

  stats.triageCalls++
  const c = a.left
  const d = a.right

  if (isLeaf(x)) return c
  if (isStem(x)) return applyInstrumented(d, x.child, budget, stats, seen)
  return applyInstrumented(
    applyInstrumented(b, x.left, budget, stats, seen),
    x.right, budget, stats, seen,
  )
}

function freshStats(): { stats: ApplyStats; seen: Set<string> } {
  return {
    stats: { totalCalls: 0, uniquePairs: 0, leafCalls: 0, stemCalls: 0, kCalls: 0, sCalls: 0, triageCalls: 0 },
    seen: new Set(),
  }
}

/** Apply with stats collection, returning both result and stats */
function applyMeasured(f: Tree, x: Tree, maxSteps = 1_000_000): { result: Tree; stats: ApplyStats; stepsUsed: number } {
  const budget = { remaining: maxSteps }
  const { stats, seen } = freshStats()
  const result = applyInstrumented(f, x, budget, stats, seen)
  return { result, stats, stepsUsed: maxSteps - budget.remaining }
}

/** Multi-arg apply with stats */
function applyManyMeasured(fn: Tree, args: Tree[], maxSteps = 1_000_000): { result: Tree; stats: ApplyStats; stepsUsed: number } {
  const budget = { remaining: maxSteps }
  const { stats, seen } = freshStats()
  let cur = fn
  for (const arg of args) {
    cur = applyInstrumented(cur, arg, budget, stats, seen)
  }
  return { result: cur, stats, stepsUsed: maxSteps - budget.remaining }
}

/** Median of an array of numbers */
function median(arr: number[]): number {
  const sorted = [...arr].sort((a, b) => a - b)
  const mid = Math.floor(sorted.length / 2)
  return sorted.length % 2 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2
}

/** Run a function N times and return median wall-clock time in ms (cold = clear cache each run) */
function benchTime(fn: () => void, runs = 5, cold = true): number {
  const times: number[] = []
  for (let i = 0; i < runs; i++) {
    if (cold) clearApplyCache()
    const start = performance.now()
    fn()
    times.push(performance.now() - start)
  }
  return median(times)
}

/** Measure both cold (fresh cache) and warm (populated cache) performance */
function benchColdWarm(fn: () => void, runs = 5): { cold: number; warm: number } {
  // Cold: clear cache before each run
  const cold = benchTime(fn, runs, true)
  // Warm: run once to populate cache, then measure without clearing
  fn()
  const warm = benchTime(fn, runs, false)
  clearApplyCache()
  return { cold, warm }
}

// --- Table formatting ---

type Row = { name: string; steps: number; unique: number; ratio: string; cold: string; warm: string; treeSize?: number }

function printTable(title: string, rows: Row[]) {
  const nameW = Math.max(25, ...rows.map(r => r.name.length))
  const sizeHdr = rows[0]?.treeSize !== undefined ? ` ${"Size".padStart(6)} │` : ""
  const header = `│ ${"Benchmark".padEnd(nameW)} │ ${"Steps".padStart(9)} │ ${"Unique".padStart(8)} │ ${"Ratio".padStart(7)} │ ${"Cold".padStart(8)} │ ${"Warm".padStart(8)} │${sizeHdr}`
  const sep = header.replace(/[^│\n]/g, "─").replace(/│/g, "┼")
  console.log(`\n  ${title}`)
  console.log(`  ${sep.replace(/┼/g, "┬").replace(/^─/, "┌").replace(/─$/, "┐")}`)
  console.log(`  ${header}`)
  console.log(`  ${sep}`)
  for (const r of rows) {
    const sizeCol = r.treeSize !== undefined ? ` ${String(r.treeSize).padStart(6)} │` : ""
    console.log(`  │ ${r.name.padEnd(nameW)} │ ${String(r.steps).padStart(9)} │ ${String(r.unique).padStart(8)} │ ${r.ratio.padStart(7)} │ ${r.cold.padStart(8)} │ ${r.warm.padStart(8)} │${sizeCol}`)
  }
  console.log(`  ${sep.replace(/┼/g, "┴").replace(/^─/, "└").replace(/─$/, "┘")}`)
}

// --- Benchmarks ---

describe("Benchmarks", () => {
  it("tree sizes of step function constants", () => {
    const constants: [string, Tree][] = [
      ["TREE_EQ_STEP", TREE_EQ_STEP],
      ["ABSTRACT_OUT_STEP", ABSTRACT_OUT_STEP],
      ["WHNF_STEP", WHNF_STEP],
      ["CONVERTIBLE_STEP", CONVERTIBLE_STEP],
      ["TYPECHECK", TYPECHECK],
    ]

    console.log("\n  Tree Sizes (DAG / Unshared)")
    for (const [name, tree] of constants) {
      const dag = treeSize(tree)
      const full = treeSizeUnshared(tree)
      console.log(`    ${name.padEnd(22)} DAG: ${String(dag).padStart(6)}  Unshared: ${String(full).padStart(8)}  Sharing: ${(full / dag).toFixed(1)}x`)
      expect(dag).toBeGreaterThan(0)
    }
  })

  it("single-step costs (trivial inputs)", () => {
    const rows: Row[] = []

    // TREE_EQ_STEP applied to (leaf, leaf)
    {
      const t = benchColdWarm(() => apply(apply(TREE_EQ_STEP, LEAF, { remaining: 100000 }), LEAF, { remaining: 100000 }))
      const m = applyManyMeasured(TREE_EQ_STEP, [LEAF, LEAF])
      rows.push({
        name: "treeEqStep(leaf,leaf)",
        steps: m.stepsUsed, unique: m.stats.uniquePairs,
        ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
        cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        treeSize: treeSize(TREE_EQ_STEP),
      })
    }

    // WHNF_STEP applied to encType() (Type is already in WHNF)
    {
      const typeTree = encType()
      const t = benchColdWarm(() => apply(WHNF_STEP, typeTree, { remaining: 100000 }))
      const m = applyMeasured(WHNF_STEP, typeTree)
      rows.push({
        name: "whnfStep(Type)",
        steps: m.stepsUsed, unique: m.stats.uniquePairs,
        ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
        cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        treeSize: treeSize(WHNF_STEP),
      })
    }

    // ABSTRACT_OUT_STEP applied to (leaf, leaf)
    {
      const t = benchColdWarm(() => apply(apply(ABSTRACT_OUT_STEP, LEAF, { remaining: 100000 }), LEAF, { remaining: 100000 }))
      const m = applyManyMeasured(ABSTRACT_OUT_STEP, [LEAF, LEAF])
      rows.push({
        name: "abstractOutStep(leaf,leaf)",
        steps: m.stepsUsed, unique: m.stats.uniquePairs,
        ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
        cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        treeSize: treeSize(ABSTRACT_OUT_STEP),
      })
    }

    // CONVERTIBLE_STEP applied to (Type, Type)
    {
      const typeTree = encType()
      const t = benchColdWarm(() => apply(apply(CONVERTIBLE_STEP, typeTree, { remaining: 100000 }), typeTree, { remaining: 100000 }))
      const m = applyManyMeasured(CONVERTIBLE_STEP, [typeTree, typeTree])
      rows.push({
        name: "convertStep(Type,Type)",
        steps: m.stepsUsed, unique: m.stats.uniquePairs,
        ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
        cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        treeSize: treeSize(CONVERTIBLE_STEP),
      })
    }

    printTable("Single-step costs", rows)
  })

  it("fuel-scaled prelude wrappers", () => {
    const { defs } = loadPrelude()

    // Load prelude.disp definitions
    const state = initialState()
    loadFile(state, "prelude.disp", true)

    const treeEqFn = state.defs.get("treeEq")!
    const leafTree = state.defs.get("leaf")!
    const stemFn = state.defs.get("stem")!
    const mkStem = (t: Tree) => apply(stemFn, t, { remaining: 1000 })

    const rows: Row[] = []

    for (const fuel of [5, 10, 20]) {
      const fuelTree = compileAndEval(parseExpr(String(fuel)), state.defs)

      // treeEq(fuel, leaf, leaf)
      {
        const m = applyManyMeasured(treeEqFn, [fuelTree, leafTree, leafTree])
        // Correctness: leaf == leaf should be true (LEAF)
        expect(isLeaf(m.result), `treeEq(${fuel},leaf,leaf) should be true`).toBe(true)
        const runEq = () => {
          const b = { remaining: 1_000_000 }
          apply(apply(apply(treeEqFn, fuelTree, b), leafTree, b), leafTree, b)
        }
        const t = benchColdWarm(runEq)
        rows.push({
          name: `treeEq(${fuel},leaf,leaf)`,
          steps: m.stepsUsed, unique: m.stats.uniquePairs,
          ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
          cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        })
      }

      // treeEq(fuel, stem^3, stem^3)
      {
        const s3 = mkStem(mkStem(mkStem(leafTree)))
        const m = applyManyMeasured(treeEqFn, [fuelTree, s3, s3])
        // Correctness: stem^3 == stem^3 should be true (LEAF)
        expect(isLeaf(m.result), `treeEq(${fuel},stem³,stem³) should be true`).toBe(true)
        const runEq = () => {
          const b = { remaining: 1_000_000 }
          apply(apply(apply(treeEqFn, fuelTree, b), s3, b), s3, b)
        }
        const t = benchColdWarm(runEq)
        rows.push({
          name: `treeEq(${fuel},stem³,stem³)`,
          steps: m.stepsUsed, unique: m.stats.uniquePairs,
          ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
          cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
        })
      }
    }

    printTable("Fuel-scaled treeEq", rows)
  })

  it("end-to-end infer via tree-native TYPECHECK", () => {
    const { defs } = loadPrelude()
    const state = initialState()
    loadFile(state, "prelude.disp", true)
    loadFile(state, "stdlib.disp", true)

    const inferFn = state.defs.get("infer")
    if (!inferFn) {
      console.log("  infer not found in prelude/stdlib — skipping end-to-end benchmark")
      return
    }

    // Build empty env and fuel
    const fuel3 = compileAndEval(parseExpr("3"), state.defs)
    const fuel5 = compileAndEval(parseExpr("5"), state.defs)
    const fuel10 = compileAndEval(parseExpr("10"), state.defs)

    // Empty env = leaf (Church nil)
    const emptyEnv = LEAF
    const typeTree = encType()

    const rows: Row[] = []

    // infer(3, 3, 10, emptyEnv, Type)
    {
      const args = [fuel3, fuel3, fuel10, emptyEnv, typeTree]
      const m = applyManyMeasured(inferFn, args, 5_000_000)
      const runInfer = () => {
        const b = { remaining: 5_000_000 }
        let cur = inferFn
        for (const arg of args) cur = apply(cur, arg, b)
      }
      const t = benchColdWarm(runInfer)
      rows.push({
        name: "infer(3,3,10,∅,Type)",
        steps: m.stepsUsed, unique: m.stats.uniquePairs,
        ratio: `${(m.stepsUsed / m.stats.uniquePairs).toFixed(1)}x`,
        cold: `${t.cold.toFixed(2)}ms`, warm: `${t.warm.toFixed(2)}ms`,
      })
    }

    printTable("End-to-end infer", rows)
  })

  it("practical comparison: TS bootstrap vs tree-native type checking", () => {
    const state = initialState()
    loadFile(state, "prelude.disp", true)

    // --- TypeScript bootstrap type checker (what the REPL uses) ---
    console.log(`\n  === TS Bootstrap Type Checker (processLine) ===`)
    const tsPrograms: [string, string][] = [
      ["Type", "Type"],
      ["(A : Type) -> A -> A", "Pi type"],
      ["let id2 : (A : Type) -> A -> A := {A x} -> x", "polymorphic id"],
      ["let not2 : Bool -> Bool := {b} -> boolElim Bool false true b", "not (Bool)"],
    ]

    for (const [prog, label] of tsPrograms) {
      const time = benchTime(() => { processLine(state, prog) }, 10, false)
      console.log(`    ${label.padEnd(25)} ${time.toFixed(3)}ms`)
    }

    // --- Tree-native type checker ---
    console.log(`\n  === Tree-Native Type Checker (prelude infer) ===`)
    const inferFn = state.defs.get("infer")!
    const emptyEnv = LEAF
    const typeTree = encType()

    // K body = fork(LEAF, x) → when applied to any arg, returns x (Rule 1)
    const kType = fork(LEAF, encType()) // K(Type): always returns Type regardless of binder var
    const piTypeType = encPi(encType(), kType)  // (A : Type) -> Type

    const cases: { name: string; term: Tree; fuels: [number, number, number] }[] = [
      { name: "infer Type", term: typeTree, fuels: [3, 3, 10] },
      { name: "infer Type (fuel=10)", term: typeTree, fuels: [10, 10, 15] },
      { name: "infer Pi(T,K(T))", term: piTypeType, fuels: [5, 5, 15] },
      { name: "infer Pi(T,K(T)) f=10", term: piTypeType, fuels: [10, 10, 20] },
    ]

    for (const c of cases) {
      const [f1, f2, f3] = c.fuels.map(n => compileAndEval(parseExpr(String(n)), state.defs))
      const args = [f1, f2, f3, emptyEnv, c.term]

      try {
        const runInfer = () => {
          const b = { remaining: 50_000_000 }
          let cur = inferFn
          for (const a of args) cur = apply(cur, a, b)
        }
        const t = benchColdWarm(runInfer)
        const m = applyManyMeasured(inferFn, args, 50_000_000)
        console.log(`    ${c.name.padEnd(25)} cold: ${t.cold.toFixed(3).padStart(8)}ms  warm: ${t.warm.toFixed(3).padStart(8)}ms  steps: ${m.stepsUsed}`)
      } catch (e: any) {
        console.log(`    ${c.name.padEnd(25)} FAILED: ${e.message?.slice(0, 60)}`)
      }
    }

    // --- Convertible comparison ---
    console.log(`\n  === Tree-Native convertible ===`)
    const convFn = state.defs.get("convertible")!

    const convCases: { name: string; a: Tree; b: Tree; fuels: [number, number, number] }[] = [
      { name: "Type == Type", a: typeTree, b: typeTree, fuels: [5, 5, 10] },
      { name: "Type == Type (f=20)", a: typeTree, b: typeTree, fuels: [20, 20, 20] },
      { name: "Pi == Pi", a: piTypeType, b: piTypeType, fuels: [10, 10, 15] },
    ]

    for (const c of convCases) {
      const [f1, f2, f3] = c.fuels.map(n => compileAndEval(parseExpr(String(n)), state.defs))
      const args = [f1, f2, f3, c.a, c.b]

      try {
        const runConv = () => {
          const b = { remaining: 50_000_000 }
          let cur = convFn
          for (const a of args) cur = apply(cur, a, b)
        }
        const t = benchColdWarm(runConv)
        const m = applyManyMeasured(convFn, args, 50_000_000)
        console.log(`    ${c.name.padEnd(25)} cold: ${t.cold.toFixed(3).padStart(8)}ms  warm: ${t.warm.toFixed(3).padStart(8)}ms  steps: ${m.stepsUsed}`)
      } catch (e: any) {
        console.log(`    ${c.name.padEnd(25)} FAILED: ${e.message?.slice(0, 60)}`)
      }
    }
  })

  it("tree-native infer on realistic expressions", () => {
    const state = initialState()
    loadFile(state, "prelude.disp", true)

    const inferFn = state.defs.get("infer")!
    const convFn = state.defs.get("convertible")!
    const emptyEnv = LEAF

    // Helper: build a Church-list env with N dummy bindings as a Tree
    function buildChurchEnv(n: number): Tree {
      const nilExpr = bracketAbstract("_r", bracketAbstract("_n", bracketAbstract("_c", eFvar("_n"))))
      let env = collapseAndEval(nilExpr)

      for (let i = 0; i < n; i++) {
        const marker = fork(LEAF, stem(fork(LEAF, stem(compileAndEval(parseExpr(String(i + 100)), state.defs)))))
        const entry = fork(marker, fork(LEAF, LEAF))
        const entryTree = entry
        const restTree = env
        const consExpr = bracketAbstract("_r", bracketAbstract("_n", bracketAbstract("_c",
          eApp(eApp(eFvar("_c"), eTree(entryTree)),
            eApp(eApp(eApp(eTree(restTree), eFvar("_r")), eFvar("_n")), eFvar("_c"))))))
        env = collapseAndEval(consExpr)
      }
      return env
    }

    // Helper: run tree-native infer and measure
    function runInfer(
      name: string, term: Tree, env: Tree,
      fuels: [number, number, number], maxSteps = 50_000_000
    ): { cold: number; warm: number; steps: number; ok: boolean } {
      const [f1, f2, f3] = fuels.map(n => compileAndEval(parseExpr(String(n)), state.defs))
      const args = [f1, f2, f3, env, term]
      try {
        const runFn = () => {
          const b = { remaining: maxSteps }
          let cur = inferFn
          for (const a of args) cur = apply(cur, a, b)
        }
        const t = benchColdWarm(runFn)
        const m = applyManyMeasured(inferFn, args, maxSteps)
        return { cold: t.cold, warm: t.warm, steps: m.stepsUsed, ok: true }
      } catch {
        return { cold: -1, warm: -1, steps: -1, ok: false }
      }
    }

    // === Infer on progressively complex terms (empty env) ===
    console.log(`\n  === Tree-Native Infer: Term Complexity Scaling ===`)
    const typeTree = encType()
    const kType = fork(LEAF, encType())
    const piTypeType = encPi(encType(), kType)

    const termCases: { name: string; term: Tree; fuels: [number, number, number] }[] = [
      { name: "Type", term: typeTree, fuels: [3, 3, 10] },
      { name: "Pi(Type, K(Type))", term: piTypeType, fuels: [5, 5, 15] },
    ]

    for (const c of termCases) {
      const r = runInfer(c.name, c.term, emptyEnv, c.fuels)
      if (r.ok) {
        console.log(`    ${c.name.padEnd(25)} cold: ${r.cold.toFixed(3).padStart(8)}ms  warm: ${r.warm.toFixed(3).padStart(8)}ms  steps: ${r.steps}`)
      } else {
        console.log(`    ${c.name.padEnd(25)} FAILED (budget or encoding)`)
      }
    }

    // === Infer with growing env size ===
    console.log(`\n  === Tree-Native Infer: Env Size Scaling ===`)
    for (const envSize of [0, 5, 10, 20]) {
      const env = envSize === 0 ? emptyEnv : buildChurchEnv(envSize)
      const r = runInfer(`Type (env=${envSize})`, typeTree, env, [5, 5, 15])
      if (r.ok) {
        console.log(`    ${ `infer Type (env=${envSize})`.padEnd(30)} cold: ${r.cold.toFixed(3).padStart(8)}ms  warm: ${r.warm.toFixed(3).padStart(8)}ms  steps: ${r.steps}`)
      } else {
        console.log(`    ${ `infer Type (env=${envSize})`.padEnd(30)} FAILED`)
      }
    }

    // === Convertible on non-trivial types ===
    console.log(`\n  === Tree-Native Convertible: Complexity Scaling ===`)
    const convCases: { name: string; a: Tree; b: Tree; fuels: [number, number, number] }[] = [
      { name: "Type == Type", a: typeTree, b: typeTree, fuels: [5, 5, 10] },
      { name: "Pi(T,K(T)) == same", a: piTypeType, b: piTypeType, fuels: [10, 10, 15] },
      { name: "Type != Pi", a: typeTree, b: piTypeType, fuels: [10, 10, 15] },
    ]

    for (const c of convCases) {
      const [f1, f2, f3] = c.fuels.map(n => compileAndEval(parseExpr(String(n)), state.defs))
      const args = [f1, f2, f3, c.a, c.b]
      try {
        const runConv = () => {
          const b = { remaining: 50_000_000 }
          let cur = convFn
          for (const a of args) cur = apply(cur, a, b)
        }
        const t = benchColdWarm(runConv)
        const m = applyManyMeasured(convFn, args, 50_000_000)
        console.log(`    ${c.name.padEnd(25)} cold: ${t.cold.toFixed(3).padStart(8)}ms  warm: ${t.warm.toFixed(3).padStart(8)}ms  steps: ${m.stepsUsed}  result: ${isLeaf(m.result) ? "true" : "false"}`)
      } catch (e: any) {
        console.log(`    ${c.name.padEnd(25)} FAILED: ${e.message?.slice(0, 60)}`)
      }
    }

    expect(true).toBe(true)
  })

  it("rule breakdown for representative operation", () => {
    const { defs } = loadPrelude()
    const state = initialState()
    loadFile(state, "prelude.disp", true)

    const treeEqFn = state.defs.get("treeEq")!
    const leafTree = state.defs.get("leaf")!
    const fuel10 = compileAndEval(parseExpr("10"), state.defs)

    const m = applyManyMeasured(treeEqFn, [fuel10, leafTree, leafTree])

    console.log(`\n  Rule breakdown for treeEq(10, leaf, leaf):`)
    console.log(`    Total calls:  ${m.stats.totalCalls}`)
    console.log(`    Unique pairs: ${m.stats.uniquePairs}`)
    console.log(`    Leaf (→stem): ${m.stats.leafCalls}`)
    console.log(`    Stem (→fork): ${m.stats.stemCalls}`)
    console.log(`    K rule:       ${m.stats.kCalls}`)
    console.log(`    S rule:       ${m.stats.sCalls}`)
    console.log(`    Triage:       ${m.stats.triageCalls}`)
    console.log(`    Redundancy:   ${(m.stats.totalCalls / m.stats.uniquePairs).toFixed(1)}x`)

    expect(m.stats.totalCalls).toBeGreaterThan(0)
  })
})

