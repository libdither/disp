// Disp language tests: runs all *.test.disp files through the parser/driver.

import { describe, it, expect } from "vitest"
import { readdirSync } from "node:fs"
import { join } from "node:path"
import { runFile } from "../src/run.js"

const libDir = join(import.meta.dirname, "..", "lib")
const testFiles = readdirSync(libDir)
  .filter(f => f.endsWith(".test.disp"))
  .sort()

describe("disp", () => {
  for (const file of testFiles) {
    it(file, () => {
      const r = runFile(join(libDir, file))
      if (r.failed.length > 0) {
        const msgs = r.failed.map(f => `[test ${f.i}] ${f.msg}`).join("\n")
        throw new Error(`${file}:\n${msgs}`)
      }
      expect(r.passed).toBe(r.tests)
      expect(r.passed).toBeGreaterThan(0)
    })
  }
})
