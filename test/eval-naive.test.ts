// Differential conformance for the naive "honesty" backend (EVALUATOR.md):
// non-canonical handles, and the naive reducer agrees with the eager
// reference on every reduction that terminates on both. This is the cheap proof
// the Session ABI is real and that nothing depends on canonical handles / O(1)
// id-equality.

import { describe, it, expect } from "vitest"
import { eagerBackend, type Tree } from "../src/eval/eager.js"
import { naiveBackend } from "../src/eval/naive.js"

const eager = eagerBackend.createSession()
const naive = naiveBackend.createSession()

let seed = 0xABCDEF
const rnd = (n: number): number => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n }

// Always-complete random ternary term.
function genTernary(depth: number): string {
  if (depth <= 0 || rnd(3) === 0) return "0"
  if (rnd(2) === 0) return "1" + genTernary(depth - 1)
  return "2" + genTernary(depth - 1) + genTernary(depth - 1)
}

describe("naive backend conformance", () => {
  it("handles are non-canonical: distinct objects, but equal() is structural", () => {
    const a = naive.loadTernary("2100")        // fork(stem(leaf), leaf), built once
    const b = naive.loadTernary("2100")        // same structure, built again
    expect(a).not.toBe(b)                       // distinct objects (no hash-consing)
    expect((a as Tree).id).not.toBe((b as Tree).id)
    expect(naive.equal!(a, b)).toBe(true)        // ...yet structurally equal
    expect(naive.canonicalHandles).toBe(false)
    // Contrast: the eager backend canonicalizes — same structure ⇒ same handle.
    expect(eager.loadTernary("2100")).toBe(eager.loadTernary("2100"))
    expect(eager.canonicalHandles).toBe(true)
  })

  it("ternary round-trips on the naive backend", () => {
    for (let k = 0; k < 300; k++) {
      const s = genTernary(6)
      expect(naive.dumpTernary(naive.loadTernary(s))).toBe(s)
    }
  })

  it("classify matches construction", () => {
    expect(naive.classify!(naive.leaf())).toEqual({ tag: "leaf" })
    const c = naive.leaf()
    expect(naive.classify!(naive.stem(c))).toMatchObject({ tag: "stem" })
    expect(naive.classify!(naive.fork(c, c))).toMatchObject({ tag: "fork" })
  })

  it("agrees with the eager reducer on every term that terminates on both", () => {
    let compared = 0, agreed = 0
    for (let k = 0; k < 4000; k++) {
      const fs = genTernary(5), xs = genTernary(4)
      const run = (s: ReturnType<typeof eagerBackend.createSession>): string | null => {
        try {
          return s.dumpTernary(s.apply(s.loadTernary(fs), s.loadTernary(xs), { remaining: 50_000 }))
        } catch { return null }   // BudgetExhausted (or grew too big) — skip
      }
      const er = run(eager), nr = run(naive)
      if (er !== null && nr !== null) { compared++; if (er === nr) agreed++ }
    }
    expect(compared).toBeGreaterThan(100)   // meaningful overlap actually exercised
    expect(agreed).toBe(compared)           // and they agree on all of it
  })
})
