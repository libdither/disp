// Disp language tests: runs all *.test.disp files through the parser/driver.

import { describe, it, expect } from "vitest"
import { readdirSync } from "node:fs"
import { join } from "node:path"
import { runFile } from "../src/run.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const testsDir = join(import.meta.dirname, "..", "lib", "tests")

// One session shared across ALL test files: the kernel (`use`d by every file)
// is interned + apply-memoized ONCE for the whole suite instead of re-elaborated
// cold per file — the hash-cons arena and apply memo are session state, so a
// shared session turns 36× redundant kernel reduction into 1× + memo hits. Scope
// is per-runFile-call (a fresh Map stack), so there is no cross-file name bleed;
// only the immutable hash-cons arena / memo / global verifiedModules are shared,
// all of which are sound to share (identical trees → identical ids by design).
// Opt out with DISP_INDEPENDENT_SESSIONS=1 for an isolated arena per file (bounds
// peak memory at one file's worth, at the cost of re-elaborating the kernel each).
const SHARE_SESSIONS = process.env.DISP_INDEPENDENT_SESSIONS !== "1"
const sharedSession = SHARE_SESSIONS
  ? (getBackend(defaultBackendName).createSession() as unknown as Session<Tree>)
  : undefined

// Recursively find all .test.disp files under lib/tests/.
function findTestFiles(dir: string, rel = ""): string[] {
  const entries = readdirSync(dir, { withFileTypes: true })
  const out: string[] = []
  for (const e of entries) {
    const p = rel ? `${rel}/${e.name}` : e.name
    if (e.isDirectory()) out.push(...findTestFiles(join(dir, e.name), p))
    else if (e.name.endsWith(".test.disp")) out.push(p)
  }
  return out
}
const testFiles = findTestFiles(testsDir).sort()

describe("disp", () => {
  // Files share one session by default (see sharedSession above) so the kernel is
  // elaborated once for the whole suite. Set DISP_INDEPENDENT_SESSIONS=1 to give
  // each file a fresh session (the prior behavior — clean per-file memo/stats).
  for (const file of testFiles) {
    it(file, () => {
      // Session 3: defense-in-depth temporarily disabled — the elaborator's
      // assertTypeCheck path applies the public guarded type to its tree,
      // which now hits q_guard_fn's entry scan; intermediate elaborator
      // trees may contain certified-neutral hypotheses that the scan
      // rejects. Re-enable once session 5 routes the elaborator through
      // kernel-internal checking that bypasses the public boundary.
      const r = runFile(join(testsDir, file), sharedSession ? { session: sharedSession } : {})
      if (r.failed.length > 0) {
        // TODO: figure out how to return specific line in test file that failed (e.g. via spans)
        const msgs = r.failed.map(f => `[test ${f.i}] ${f.msg}`).join("\n")
        throw new Error(`${file}:\n${msgs}`)
      }
      expect(r.passed).toBe(r.tests)
      expect(r.passed).toBeGreaterThan(0)
    })
  }
})
