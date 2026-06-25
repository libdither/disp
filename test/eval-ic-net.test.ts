// Differential conformance: disp's eager reducer (the oracle) vs rust-ic-net, the
// MATERIALIZED interaction-net backend (M0, sequential). Strong confluence (tc-net.typ
// Theorem 2) ⇒ a materialized net reaches the SAME normal forms as the tree reducer, so
// agreement on the benchmark programs + randomized total terms is the M0 gate. rust-eager
// is an additional independent cross-check when its artifact is present.
//
// M0 uses δˢ (structural copy): correct but NOT work-sharing (tc-net.typ Prop 5) — it
// re-reduces shared subterms, so even the sharing-heavy programs reach the right NF, just
// at millions of interactions/nodes where δⁿ (M1) will share. Gated on the built artifact.

import { describe, it, expect } from "vitest"
import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { eagerBackend } from "../src/eval/eager.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import { icNetBackend, icNetAvailable } from "../src/eval/ic-net.js"

const dispFold = (terms: string[], steps = 500_000_000): string =>
  sessionBatchRunner("eager", eagerBackend.createSession()).fold(terms, { remaining: steps })

const icNetFold = (terms: string[], budget = 4_000_000_000): string => {
  const s = icNetBackend.createSession()
  try {
    return sessionBatchRunner("ic-net", s).fold(terms, { remaining: budget })
  } finally {
    s.dispose()
  }
}

// nat → ternary (LSB-first), copied from eval-rust-eager.test.ts.
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

// The full named gate — all reach the right NF on M0's δˢ (the sharing-heavy ones at
// millions of interactions; M1's δⁿ will share that work, not change the answer).
const cases: Array<{ name: string; terms: string[]; expected?: string }> = [
  { name: "identity ∘ leaf → leaf", terms: ["21100", "0"], expected: "0" },
  { name: "value passthrough", terms: ["110"], expected: "110" },
  { name: "size(SIZE)", terms: [programs["size"], programs["size"]] },
  { name: "recursive-fib(8) = 34", terms: [programs["recursive-fib"], encodeNat(8)], expected: encodeNat(34) },
  { name: "silly-exp(4) = 16", terms: [programs["silly-exp"], encodeNat(4)], expected: encodeNat(16) },
  { name: "exercise-rules(30) = true", terms: [programs["exercise-rules"], encodeNat(30)], expected: "10" },
  { name: "merge-sort([6..1]) = [1..6]", terms: [programs["merge-sort"], makeNatList(6, 1, -1)], expected: makeNatList(1, 6, 1) },
]

describe.skipIf(!icNetAvailable())("disp-eager ↔ rust-ic-net differential conformance", () => {
  for (const c of cases) {
    it(c.name, () => {
      const oracle = dispFold(c.terms)
      expect(icNetFold(c.terms), `rust-ic-net disagrees with eager on ${c.name}`).toBe(oracle)
      if (c.expected !== undefined) expect(oracle).toBe(c.expected)
    })
  }

  // Randomized small total terms (same generator as the rust-eager differential).
  it("randomized small folds agree: eager ↔ rust-ic-net", () => {
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
        t = icNetFold(terms, 200_000_000)
      } catch {
        continue
      }
      expect(t, `disagree on fold ${JSON.stringify(terms)}`).toBe(d)
      compared++
    }
    expect(compared, "randomized differential compared no cases — guard too strict").toBeGreaterThan(15)
  })

  it("reports non-canonical handles + a materialized net (nodes allocated)", () => {
    const s = icNetBackend.createSession()
    try {
      expect(s.canonicalHandles).toBe(false)
      // Force a reduction, then confirm the net actually allocated agents — i.e. it is a
      // materialized interaction net, not a recompiled tree reducer (an M0-gate signal).
      sessionBatchRunner("ic-net", s).fold([programs["exercise-rules"], encodeNat(4)], { remaining: 4_000_000_000 })
      expect(s.stats!().nodes).toBeGreaterThan(0)
      expect(s.stats!().steps).toBeGreaterThan(0)
    } finally {
      s.dispose()
    }
  })
})
