// Tree-native tests: each example .disp file's `test` declarations execute
// inside the tree calculus runtime; this harness only asserts that all of
// them pass.

import { describe, it, expect } from "vitest"
import { runFile } from "../src/run.js"

const EXAMPLES = ["combinators", "data", "triage", "tagged", "recursion", "splice", "normalize", "picheck", "splice_full", "typing", "checking", "predicates", "erase", "elab"]

describe("disp examples", () => {
  for (const name of EXAMPLES) {
    it(name, () => {
      const r = runFile(`examples/${name}.disp`)
      if (r.failed.length > 0) {
        const msgs = r.failed.map(f => `[test ${f.i}] ${f.msg}`).join("\n")
        throw new Error(`${name}.disp:\n${msgs}`)
      }
      expect(r.passed).toBe(r.tests)
      expect(r.passed).toBeGreaterThan(0)
    })
  }
})
