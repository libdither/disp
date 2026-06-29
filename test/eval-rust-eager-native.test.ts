// Differential conformance: disp's eager reducer (the oracle) vs the rust-eager-native
// in-process N-API backend. This is the SAME Rust hash-consed eager reducer as the wasm
// rust-eager backend, just compiled to a native Node addon instead of wasm32 — so this gate
// guards the napi binding (the class-owned arena, the string interchange, the unsigned
// exhaustion sentinel), not the reducer (already covered by eval-rust-eager.test.ts).
// Agreement on the real benchmark programs + randomized total terms is the conformance gate:
// "everything eager accepts, rust-eager-native accepts" (decision 7, one direction).
//
// Gated on the built artifact (evaluators/rust-eager/build-native.sh); skipped otherwise, so
// the default `npm test` needs no native toolchain.

import { describe, it, expect } from "vitest"
import { readFileSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { eagerBackend } from "../src/eval/eager.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import { rustEagerNativeBackend, rustEagerNativeAvailable } from "../src/eval/rust-eager-native.js"

// Eager's default budget is only 10k steps; the benchmark programs need more.
const dispFold = (terms: string[], steps = 500_000_000): string =>
  sessionBatchRunner("eager", eagerBackend.createSession()).fold(terms, { remaining: steps })

// Fresh native session per fold; dispose drops the napi object (its finalizer frees the arena).
const nativeFold = (terms: string[], budget = 4_000_000_000): string => {
  const s = rustEagerNativeBackend.createSession()
  try {
    return sessionBatchRunner("rust-eager-native", s).fold(terms, { remaining: budget })
  } finally {
    s.dispose()
  }
}

// nat → ternary, mirroring lambada benchmark/run.sh `encode_nat` (LSB-first bit
// list: 0→"0", 1→"2100", 2→"202100", …). Copied from eval-rust-eager.test.ts.
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

describe.skipIf(!rustEagerNativeAvailable())("disp-eager ↔ rust-eager-native differential conformance", () => {
  for (const c of cases) {
    it(c.name, () => {
      const oracle = dispFold(c.terms)
      expect(nativeFold(c.terms), `rust-eager-native disagrees with eager on ${c.name}`).toBe(oracle)
      if (c.expected !== undefined) expect(oracle).toBe(c.expected)
    })
  }

  // Randomized small total terms. A term that diverges/blows up on EITHER backend is out of
  // the total-corpus contract and skipped — only terms total on both are compared.
  it("randomized small folds agree: eager ↔ rust-eager-native", () => {
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
        t = nativeFold(terms, 20_000_000)
      } catch {
        continue
      }
      expect(t, `disagree on fold ${JSON.stringify(terms)}`).toBe(d)
      compared++
    }
    expect(compared, "randomized differential compared no cases — guard too strict").toBeGreaterThan(15)
  })

  it("reports non-canonical handles", () => {
    const s = rustEagerNativeBackend.createSession()
    try {
      expect(s.canonicalHandles).toBe(false)
    } finally {
      s.dispose()
    }
  })
})
