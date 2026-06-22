// Conformance for the Session ABI surface on the eager reference backend
// (EVALUATOR_PLAN §3): ternary round-trips, dump is canonical, classify matches
// construction, equal agrees with the canonical dump. When the Phase-3 naive
// backend lands, the same checks run against it (differential conformance).

import { describe, it, expect } from "vitest"
import { eagerBackend, type Tree } from "../src/tree.js"
import type { Session } from "../src/eval/types.js"

let seed = 0x1234567
const rnd = (n: number): number => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed % n }

function genTree(s: Session<Tree>, depth: number): Tree {
  if (depth <= 0 || rnd(3) === 0) return s.leaf()
  if (rnd(2) === 0) return s.stem(genTree(s, depth - 1))
  return s.fork(genTree(s, depth - 1), genTree(s, depth - 1))
}

describe("eval ABI (eager backend)", () => {
  const s = eagerBackend.createSession()

  it("reports canonical handles", () => {
    expect(s.canonicalHandles).toBe(true)
  })

  it("ternary round-trips (loadTernary ∘ dumpTernary = id)", () => {
    for (let k = 0; k < 500; k++) {
      const t = genTree(s, 7)
      const round = s.loadTernary(s.dumpTernary(t))
      // Hash-consing makes the round-trip identity, so equal? is even O(1) here.
      expect(s.equal!(t, round)).toBe(true)
    }
  })

  it("dumpTernary is canonical (equal trees ⇒ equal strings)", () => {
    for (let k = 0; k < 300; k++) {
      const a = genTree(s, 5)
      const b = genTree(s, 5)
      expect(s.equal!(a, b)).toBe(s.dumpTernary(a) === s.dumpTernary(b))
    }
  })

  it("classify matches construction", () => {
    expect(s.classify!(s.leaf())).toEqual({ tag: "leaf" })

    const child = s.leaf()
    const st = s.stem(child)
    const cst = s.classify!(st)
    expect(cst.tag).toBe("stem")
    if (cst.tag === "stem") expect(s.equal!(cst.child, child)).toBe(true)

    const fk = s.fork(child, st)
    const cfk = s.classify!(fk)
    expect(cfk.tag).toBe("fork")
    if (cfk.tag === "fork") {
      expect(s.equal!(cfk.left, child)).toBe(true)
      expect(s.equal!(cfk.right, st)).toBe(true)
    }
  })

  it("loadTernary rejects malformed input", () => {
    expect(() => s.loadTernary("0extra")).toThrow()
    expect(() => s.loadTernary("1")).toThrow()      // stem with no child
    expect(() => s.loadTernary("2 0")).toThrow()    // bad character
  })
})
