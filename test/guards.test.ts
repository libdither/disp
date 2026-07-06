// Host-side pins for the guard layer's REJECTIONS (a guard refusal is a load
// failure, so it can only be asserted from outside the module). The positive
// lifecycle lives in lib/tests/guards.test.disp.

import { describe, it, expect } from "vitest"
import { join } from "node:path"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const HERE = join(process.cwd(), "lib/tests/_guards_host.disp")
const K = `open use "../prelude.disp"
open use "../kernel/prelude.disp"
open use "../std/oeq.disp"
`
// One shared native-backend session (the disp.test.ts pattern): the kernel is
// elaborated once for all cases; the eager TS session would blow the worker heap.
const session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
  .createSession() as unknown as Session<Tree>
const run = (src: string) => parseProgram(src, HERE, { session })

describe("guard layer rejections", () => {
  it("freeze refuses rebinds", () => {
    expect(() => run(K + `guard freeze V : Nat := 3\nV : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 300000)

  it("a licensed name refuses a plain (credential-less) rebind", () => {
    expect(() => run(K + `guard (license_guard (oeq Nat)) n : Nat := 3\nn : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("a lying proof is rejected", () => {
    expect(() => run(K + `guard (license_guard (oeq Nat)) m : Nat := 3\nm : Nat := { new := 4; proof := refl }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("an honest payload rebind passes", () => {
    expect(() => run(K + `guard (license_guard (oeq Nat)) k : Nat := 3\nk : Nat := { new := 3; proof := refl }\ntest k = 3\n`))
      .not.toThrow()
  }, 120000)

  it("ownership is not surrendered (guard proposals refused)", () => {
    expect(() => run(K + `guard (license_guard (oeq Nat)) p : Nat := 3\nguard freeze p : Nat := { new := 3; proof := refl }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("shadowing default_guard changes the ambient policy for the scope", () => {
    expect(() => run(K + `let default_guard := freeze\nx : Nat := 3\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("let_dec as a head makes the binding private (bound, not exported)", () => {
    const decls = run(K + `let_dec p : Nat := 3\ntest p = 3\n`)
    expect(decls.some(d => d.kind === "Def" && d.name === "p")).toBe(false)
  }, 120000)
})
