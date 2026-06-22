// Real elaboration + verification on the NON-CANONICAL naive backend
// (EVALUATOR_PLAN Phase 3) — the strongest proof the Session ABI is real:
// compile.ts drives a full kernel module to its verdict on a backend with NO
// hash-consing, fresh non-canonical handles, and structural equality, so nothing
// in the elaborator secretly depends on canonical handles or O(1) id-equality.
//
// Slow by construction (no hash-consing + a structural memo re-reduces more than
// the eager backend), so this runs ONE representative module rather than the full
// corpus; the fast differential test (eval-naive.test.ts) covers the reducer, and
// the full-corpus-on-naive fixture is future work (a lighter memoized variant).
// Its own test file ⇒ its own vitest worker ⇒ isolated heap.

import { describe, it, expect } from "vitest"
import { readFileSync } from "node:fs"
import { join } from "node:path"
import { parseProgram, type Decl } from "../src/compile.js"
import { naiveBackend } from "../src/eval/naive.js"

describe("naive backend elaboration", () => {
  it("elaborates + verifies a real kernel module (nat.test.disp)", { timeout: 300_000 }, () => {
    const abs = join(import.meta.dirname, "..", "lib", "tests", "nat.test.disp")
    const session = naiveBackend.createSession()
    const decls: Decl[] = parseProgram(readFileSync(abs, "utf-8"), abs, { session })
    const tests = decls.filter((d): d is Extract<Decl, { kind: "Test" }> => d.kind === "Test")
    expect(tests.length).toBeGreaterThan(0)
    for (const d of tests) expect(session.equal!(d.lhs, d.rhs)).toBe(true)
    // Sanity: the backend really is the non-canonical one.
    expect(session.canonicalHandles).toBe(false)
  })
})
