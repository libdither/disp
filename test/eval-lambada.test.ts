// Differential conformance: disp's eager reducer vs ALL 11 lambada (lambada-llc/
// tree-calculus) evaluators over the shared ternary interchange
// (EVALUATOR_LAMBADA_PLAN.md). Each lambada evaluator is an INDEPENDENT
// implementation of the same 5-rule calculus (eager/lazy × value-rep × app-rep ×
// memory strategy), so agreement across all of them + disp on real programs is a
// strong external proof the Session ABI + ternary codec are correct (prior
// conformance was all eager-vs-naive — same author, same codec lineage).
//
// Gated on the built artifact (evaluators/lambada/build.sh); skipped otherwise,
// so the default `npm test` never pulls the foreign build.

import { describe, it, expect } from "vitest"
import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { eagerBackend } from "../src/eval/eager.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import { lambadaRunner, lambadaAvailable, LAMBADA_EVALUATORS } from "../src/eval/lambada.js"

// Eager's default budget is only 10k steps; the benchmark programs need more.
const dispFold = (terms: string[], steps = 500_000_000): string =>
  sessionBatchRunner("eager", eagerBackend.createSession()).fold(terms, { remaining: steps })

// nat → ternary, mirroring lambada benchmark/run.sh `encode_nat`: a binary,
// LSB-first bit list — cons = fork(bit, rest), nil = leaf("0"), 0-bit = leaf("0"),
// 1-bit = stem(leaf)("10"). So 0→"0", 1→"2100", 2→"202100", …
function encodeNat(n: number): string {
  const bits: number[] = []
  for (let m = n; m > 0; m >>= 1) bits.push(m & 1)
  let r = "0"
  for (let i = bits.length - 1; i >= 0; i--) r = (bits[i] === 1 ? "210" : "20") + r
  return r
}
// fork-encoded nat list, mirroring run.sh `make_nat_list` (cons h t = "2" h t, nil "0").
function makeNatList(start: number, stop: number, step: number): string {
  let r = "0"
  for (let k = stop; k !== start - step; k -= step) r = "2" + encodeNat(k) + r
  return r
}

// The five benchmark PROGRAMS, extracted verbatim from lambada benchmark/run.sh at
// the pinned commit into a committed fixture (see the extractor in
// EVALUATOR_LAMBADA_PLAN.md). We use SMALL args so every evaluator (including the
// non-memoizing eager ones) finishes fast — the cross-implementation agreement is
// the point, not the throughput; that is what bench/bench-evaluators.ts measures.
const FIXTURE = join(dirname(fileURLToPath(import.meta.url)), "..", "bench", "programs", "lambada-benchmarks.json")
const programs: Record<string, string> = JSON.parse(readFileSync(FIXTURE, "utf8")).programs

// Each case folds [program, arg]; `expected` (where derivable) guards against a
// shared bug passing by mutual agreement on a wrong answer.
const cases: Array<{ name: string; terms: string[]; expected?: string }> = [
  { name: "identity ∘ leaf → leaf", terms: ["21100", "0"], expected: "0" },
  { name: "value passthrough", terms: ["110"], expected: "110" },
  { name: "size(SIZE)", terms: [programs["size"], programs["size"]] },
  { name: "recursive-fib(8) = 34", terms: [programs["recursive-fib"], encodeNat(8)], expected: encodeNat(34) },
  { name: "silly-exp(4) = 16", terms: [programs["silly-exp"], encodeNat(4)], expected: encodeNat(16) },
  { name: "exercise-rules(30) = true", terms: [programs["exercise-rules"], encodeNat(30)], expected: "10" },
  { name: "merge-sort([6..1]) = [1..6]", terms: [programs["merge-sort"], makeNatList(6, 1, -1)], expected: makeNatList(1, 6, 1) },
]

describe.skipIf(!lambadaAvailable())("disp ↔ lambada differential conformance", () => {
  // Every lambada evaluator must agree with disp-eager on every case (and disp
  // must hit the independently-derived value).
  for (const ev of LAMBADA_EVALUATORS) {
    describe(`lambada-${ev}`, () => {
      const lam = lambadaRunner(ev)
      for (const c of cases) {
        it(c.name, () => {
          const d = dispFold(c.terms)
          expect(lam.fold(c.terms), `lambada-${ev} disagrees with disp on ${c.name}`).toBe(d)
          if (c.expected !== undefined) expect(d).toBe(c.expected)
        })
      }
    })
  }

  // Randomized small total terms against a representative lazy + eager evaluator
  // (EVALUATOR_LAMBADA_PLAN.md §4.4). A term that diverges/blows up on EITHER
  // backend (disp budget, lambada timeout) is out of the total-corpus contract
  // and skipped — only terms total on both are compared.
  for (const ev of ["lazy-stacks", "eager-value-adt"] as const) {
    it(`randomized small folds agree: disp ↔ lambada-${ev}`, () => {
      const lam = lambadaRunner(ev, 5_000)
      let seed = 0xC0FFEE   // deterministic LCG ⇒ reproducible failures
      const rnd = () => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed / 0x7fffffff }
      const genValue = (depth: number): string =>
        depth <= 0 || rnd() < 0.34 ? "0"
          : rnd() < 0.5 ? "1" + genValue(depth - 1)
          : "2" + genValue(depth - 1) + genValue(depth - 1)
      let compared = 0
      for (let i = 0; i < 50; i++) {
        const terms = [genValue(3), genValue(3)]
        let d: string, l: string
        try { d = dispFold(terms, 20_000_000) } catch { continue }
        try { l = lam.fold(terms) } catch { continue }
        expect(l, `disagree on fold ${JSON.stringify(terms)}`).toBe(d)
        compared++
      }
      expect(compared, "randomized differential compared no cases — guard too strict").toBeGreaterThan(15)
    })
  }
})
