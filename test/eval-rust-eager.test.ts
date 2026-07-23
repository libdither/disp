// Differential conformance: disp's eager reducer (the oracle) vs the Rooted
// rust-eager Rust→WASM backend (EVALUATOR.md) over the shared ternary
// interchange. rust-eager is an INDEPENDENT implementation of the same 5-rule
// calculus on a different substrate (hash-consed lazy interaction-net reducer),
// so agreement on the real benchmark programs + randomized total terms is the M0
// conformance gate: "everything eager accepts, rust-eager accepts" (decision 7, one
// direction). Mirrors test/eval-lambada.test.ts.
//
// Gated on the built artifact (evaluators/rust-eager/build.sh); skipped otherwise, so
// the default `npm test` needs no Rust toolchain.

import { describe, it, expect } from "vitest"
import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { eagerBackend } from "../src/eval/eager.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import { rustEagerBackend, rustEagerAvailable } from "../src/eval/rust-eager.js"

// Eager's default budget is only 10k steps; the benchmark programs need more.
const dispFold = (terms: string[], steps = 500_000_000): string =>
  sessionBatchRunner("eager", eagerBackend.createSession()).fold(terms, { remaining: steps })

// Fresh rust-eager session per fold; dispose drops the WASM instance + its arena.
const rustEagerFold = (terms: string[], budget = 4_000_000_000): string => {
  const s = rustEagerBackend.createSession()
  try {
    return sessionBatchRunner("rust-eager", s).fold(terms, { remaining: budget })
  } finally {
    s.dispose()
  }
}

// nat → ternary, mirroring lambada benchmark/run.sh `encode_nat` (LSB-first bit
// list: 0→"0", 1→"2100", 2→"202100", …). Copied from eval-lambada.test.ts.
function encodeNat(n: number): string {
  const bits: number[] = []
  for (let m = n; m > 0; m >>= 1) bits.push(m & 1)
  let r = "0"
  for (let i = bits.length - 1; i >= 0; i--) r = (bits[i] === 1 ? "210" : "20") + r
  return r
}
function makeNatList(start: number, stop: number, step: number): string {
  let r = "0"
  for (let k = stop; k !== start - step; k -= step) r = "2" + encodeNat(k) + r
  return r
}

const FIXTURE = join(dirname(fileURLToPath(import.meta.url)), "..", "bench", "programs", "lambada-benchmarks.json")
const programs: Record<string, string> = JSON.parse(readFileSync(FIXTURE, "utf8")).programs

const cases: Array<{ name: string; terms: string[]; expected?: string }> = [
  { name: "identity ∘ leaf → leaf", terms: ["21100", "0"], expected: "0" },
  { name: "value passthrough", terms: ["110"], expected: "110" },
  { name: "size(SIZE)", terms: [programs["size"], programs["size"]] },
  { name: "recursive-fib(8) = 34", terms: [programs["recursive-fib"], encodeNat(8)], expected: encodeNat(34) },
  { name: "silly-exp(4) = 16", terms: [programs["silly-exp"], encodeNat(4)], expected: encodeNat(16) },
  { name: "exercise-rules(30) = true", terms: [programs["exercise-rules"], encodeNat(30)], expected: "10" },
  { name: "merge-sort([6..1]) = [1..6]", terms: [programs["merge-sort"], makeNatList(6, 1, -1)], expected: makeNatList(1, 6, 1) },
]

describe.skipIf(!rustEagerAvailable())("disp-eager ↔ rust-eager differential conformance", () => {
  // The named benchmark programs are total — rust-eager MUST produce the answer (no
  // skip), and it must equal eager's NF (and the independently-derived value).
  for (const c of cases) {
    it(c.name, () => {
      const oracle = dispFold(c.terms)
      expect(rustEagerFold(c.terms), `rust-eager disagrees with eager on ${c.name}`).toBe(oracle)
      if (c.expected !== undefined) expect(oracle).toBe(c.expected)
    })
  }

  // Randomized small total terms. A term that diverges/blows up on EITHER backend
  // (eager budget, rust-eager budget) is out of the total-corpus contract and skipped —
  // only terms total on both are compared (same shape as the lambada differential).
  it("randomized small folds agree: eager ↔ rust-eager", () => {
    let seed = 0xC0FFEE
    const rnd = () => {
      seed = (seed * 1103515245 + 12345) & 0x7fffffff
      return seed / 0x7fffffff
    }
    const genValue = (depth: number): string =>
      depth <= 0 || rnd() < 0.34 ? "0" : rnd() < 0.5 ? "1" + genValue(depth - 1) : "2" + genValue(depth - 1) + genValue(depth - 1)
    let compared = 0
    for (let i = 0; i < 50; i++) {
      const terms = [genValue(3), genValue(3)]
      let d: string, t: string
      try {
        d = dispFold(terms, 20_000_000)
      } catch {
        continue
      }
      try {
        t = rustEagerFold(terms, 20_000_000)
      } catch {
        continue
      }
      expect(t, `disagree on fold ${JSON.stringify(terms)}`).toBe(d)
      compared++
    }
    expect(compared, "randomized differential compared no cases — guard too strict").toBeGreaterThan(15)
  })

  // canonicalHandles is false (live suspensions ⇒ id≠id no longer implies ≠).
  it("reports non-canonical handles", () => {
    const s = rustEagerBackend.createSession()
    try {
      expect(s.canonicalHandles).toBe(false)
    } finally {
      s.dispose()
    }
  })
})
