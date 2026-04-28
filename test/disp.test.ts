// Disp language tests: runs test/disp.disp through the parser/driver.
// NbE pipeline, type constructors, eliminators, Eq, arithmetic — all .disp source.

import { describe, it, expect } from "vitest"
import { runFile } from "../src/run.js"

describe("disp", () => {
  it("disp.disp passes all assertions", () => {
    const r = runFile("test/disp.disp")
    if (r.failed.length > 0) {
      const msgs = r.failed.map(f => `[test ${f.i}] ${f.msg}`).join("\n")
      throw new Error(`disp.disp:\n${msgs}`)
    }
    expect(r.passed).toBe(r.tests)
    expect(r.passed).toBeGreaterThan(0)
  })
})
