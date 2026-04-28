// NbE tree programs: runs test/nbe_tree.disp through the parser/driver.
// All NbE components are defined and tested in the .disp file.

import { describe, it, expect } from "vitest"
import { runFile } from "../src/run.js"

describe("NbE Tree Programs (.disp)", () => {
  it("nbe_tree.disp passes all assertions", () => {
    const r = runFile("test/nbe_tree.disp")
    if (r.failed.length > 0) {
      const msgs = r.failed.map(f => `[test ${f.i}] ${f.msg}`).join("\n")
      throw new Error(`nbe_tree.disp:\n${msgs}`)
    }
    expect(r.passed).toBe(r.tests)
    expect(r.passed).toBeGreaterThan(0)
  })
})
