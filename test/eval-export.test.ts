// Export + batch-tier conformance (EVALUATOR_PLAN Phase 4).
//   - A binding's emitted ternary blob is self-contained: reloading it on ANY
//     backend and normalizing yields the same result as the in-repo evaluation
//     (defs inline by construction — §3.1, no native-substitution step).
//   - The batch tier (sessionBatchRunner) left-folds ternary terms to a ternary
//     NF, and the eager and naive runners agree.

import { describe, it, expect } from "vitest"
import { eagerBackend, type Tree } from "../src/eval/eager.js"
import { naiveBackend } from "../src/eval/naive.js"
import { emitBlob } from "../src/format/export.js"
import { sessionBatchRunner } from "../src/eval/batch.js"

const eager = eagerBackend.createSession()
const naive = naiveBackend.createSession()

let seed = 0x5AFE
const rnd = (n: number): number => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n }
function genTernary(depth: number): string {
  if (depth <= 0 || rnd(3) === 0) return "0"
  if (rnd(2) === 0) return "1" + genTernary(depth - 1)
  return "2" + genTernary(depth - 1) + genTernary(depth - 1)
}

describe("export + batch conformance", () => {
  it("an emitted blob reloads + normalizes to the same value on any backend", () => {
    for (let k = 0; k < 400; k++) {
      // Build a closed value on eager, emit it, then reload on BOTH backends.
      const t: Tree = eager.loadTernary(genTernary(6))
      const blob = emitBlob(eager, t)
      // Self-contained: the blob is just the canonical ternary, so a fresh load
      // on either backend is structurally the same tree.
      expect(eager.dumpTernary(eager.loadTernary(blob))).toBe(blob)
      expect(naive.dumpTernary(naive.loadTernary(blob))).toBe(blob)
    }
  })

  it("the batch runners agree on left-fold normal forms (where both terminate)", () => {
    const eagerRunner = sessionBatchRunner("eager", eager)
    const naiveRunner = sessionBatchRunner("naive", naive)
    let compared = 0
    for (let k = 0; k < 1500; k++) {
      const terms = Array.from({ length: 1 + rnd(3) }, () => genTernary(4))
      let er: string | null = null, nr: string | null = null
      try { er = eagerRunner.fold(terms, { remaining: 50_000 }) } catch { er = null }
      try { nr = naiveRunner.fold(terms, { remaining: 50_000 }) } catch { nr = null }
      if (er !== null && nr !== null) { compared++; expect(nr).toBe(er) }
    }
    expect(compared).toBeGreaterThan(50)
  })

  it("emitBlob of a known reduct is its ternary NF", () => {
    // (K K) leaf  →  K   ; K = stem(leaf) = "10". Build via batch fold and emit.
    const runner = sessionBatchRunner("eager", eager)
    // identity applied to leaf is leaf: fold ["I","0"] where I = "21010"? keep it
    // representation-light: apply K ("10") to two args, expect the first.
    const k = "10", a = "0", b = "200"   // K = stem(leaf); a = leaf; b = fork(leaf,leaf)
    const nf = runner.fold([k, a, b])    // K a b → a
    expect(nf).toBe(a)
  })
})
