// Host-side pins for the given layer's REJECTIONS (a bad instantiation is a
// load failure, so it can only be asserted from outside the module). The
// positive lifecycle lives in lib/tests/given.test.disp; the design is
// MODULES.md (slice 1).

import { describe, it, expect } from "vitest"
import { join } from "node:path"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const HERE = join(process.cwd(), "lib/tests/_modules_host.disp")
const K = `open use "../prelude.disp"
open use "../kernel/prelude.disp"
`
// One shared native-backend session (the guards.test.ts pattern): the kernel is
// elaborated once for all cases.
const session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
  .createSession() as unknown as Session<Tree>
const run = (src: string) => parseProgram(src, HERE, { session })

// A tmp fixture next to nothing: it must open the kernel itself (hermetic-ready),
// via absolute-ish paths back into the repo's lib/.
const LIB = join(process.cwd(), "lib")
function tmpModule(name: string, body: string): string {
  const dir = mkdtempSync(join(tmpdir(), "disp-mod-"))
  const path = join(dir, name)
  writeFileSync(path, `open use "${LIB}/prelude.disp"\nopen use "${LIB}/kernel/prelude.disp"\n` + body)
  return path
}

describe("module dependencies (given) rejections", () => {
  it("a bare use of a given-bearing module errors, listing the givens", () => {
    expect(() => run(K + `m := use "given_mod.disp"\n`))
      .toThrow(/unfilled given\(s\) add/)
  }, 300000)

  it("an empty fill record still misses the required given", () => {
    expect(() => run(K + `m := use "given_mod.disp" {}\n`))
      .toThrow(/unfilled given\(s\) add/)
  }, 120000)

  it("an unknown fill name is rejected", () => {
    expect(() => run(K + `m := use "given_mod.disp" { add := ({x} -> {y} -> x), bogus := t }\n`))
      .toThrow(/unknown fill 'bogus'/)
  }, 120000)

  it("an ill-typed fill fails the deferred linking check", () => {
    expect(() => run(K + `m := use "given_mod.disp" { add := t }\n`))
      .toThrow(/given 'add'/)
  }, 120000)

  it("the root module cannot declare dependencies", () => {
    expect(() => run(`given x : Nat\nfoo := t\n`))
      .toThrow(/root module cannot declare dependencies/)
  }, 120000)

  it("duplicate givens are rejected", () => {
    const p = tmpModule("dup.disp", `given a : Nat\ngiven a : Nat\nx := a\n`)
    expect(() => run(K + `m := use "${p}" { a := zero }\n`))
      .toThrow(/duplicate dependency/)
  }, 120000)

  it("a given needs a type annotation", () => {
    const p = tmpModule("unty.disp", `given b := t\nx := b\n`)
    expect(() => run(K + `m := use "${p}" {}\n`))
      .toThrow(/needs a type annotation/)
  }, 120000)

  it("a shadowed `given` producing a param request is rejected (dynamic givens)", () => {
    expect(() => run(K
      + `given := {req} -> { value := req.value; ty := req.ty; guard := req.guard; private := FF; param := TT }\n`
      + `given x : Nat := zero\n`))
      .toThrow(/dynamic givens are unsupported/)
  }, 120000)
})
