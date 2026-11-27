// Stage 1 end-to-end tests: prove that an alternate frontend (`sexpFrontend`)
// plugs cleanly into the single core driver (`parseProgram`), produces
// hash-cons-identical trees to `dispFrontend`, imports standard `.disp` modules,
// runs `lib/kernel/` `check_module` verification, and supports declaration guards.

import { describe, it, expect } from "vitest"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"
import { parseProgram, dispFrontend, sexpFrontend } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const LIB = join(process.cwd(), "lib")
const session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
  .createSession() as unknown as Session<Tree>

function defTree(decls: ReturnType<typeof parseProgram>, name: string): Tree {
  const d = decls.find(x => x.kind === "Def" && x.name === name)
  if (!d || d.kind !== "Def") throw new Error(`missing def '${name}'`)
  return d.tree
}

describe("Stage 1: syntax-agnostic core driver + pluggable frontends", () => {
  it("produces hash-cons-identical trees across Disp and S-expression frontends", () => {
    const dispDecls = parseProgram(`
      open use raw "${LIB}/prelude.disp" {}
      id := {x} => x
      const := {x, y} => x
      add := fix({self, a, b} => if (is_zero(a)) { b } else { succ(self(nat_pred(a), b)) })
      five := add(2, 3)
    `, undefined, { session, frontend: dispFrontend })

    const sexpDecls = parseProgram(`
      (open (use-raw "${LIB}/prelude.disp" ()))
      (def id (lam (x) x))
      (def const (lam (x y) x))
      (def add (fix (lam (self a b) (if (is_zero a) b (succ (self (nat_pred a) b))))))
      (def five (add 2 3))
    `, undefined, { session, frontend: sexpFrontend })

    for (const name of ["id", "const", "add", "five"]) {
      expect(session.equal!(defTree(dispDecls, name), defTree(sexpDecls, name))).toBe(true)
    }
  })

  it("supports cross-syntax imports (Disp importing S-expr module and vice versa)", () => {
    const dir = mkdtempSync(join(tmpdir(), "disp-sexp-"))
    const modPath = join(dir, "math.sexp")
    writeFileSync(modPath, `
      (open (use-raw "${LIB}/prelude.disp" ()))
      (let twice_fix (fix (lam (self n) (if (is_zero n) 0 (succ (succ (self (nat_pred n))))))))
      (def dbl (lam (n) (twice_fix n)))
    `)

    // Disp file importing an S-expression module via custom frontend resolver
    const decls = parseProgram(`
      open use raw "${LIB}/prelude.disp" {}
      open use "${modPath}"
      res := dbl(4)
    `, join(dir, "main.disp"), {
      session,
      resolveFrontend: (abs, cur) => abs.endsWith(".sexp") ? sexpFrontend : abs.endsWith(".disp") ? dispFrontend : cur,
    })
    const expected = parseProgram(`open use raw "${LIB}/prelude.disp" {}\neight := 8\n`, undefined, { session })
    expect(session.equal!(defTree(decls, "res"), defTree(expected, "eight"))).toBe(true)
  })

  it("runs promoted kernel `check_module` verification on S-expression modules", () => {
    const dir = mkdtempSync(join(tmpdir(), "disp-sexp-chk-"))
    const goodMod = join(dir, "good.sexp")
    writeFileSync(goodMod, `
      (open (use "${LIB}/kernel/prelude.disp"))
      (def inc : (arrow Nat Nat) (lam (n) (succ n)))
      (def poly_id : (pi (A Type) (arrow A A)) (lam (A x) x))
    `)
    const badMod = join(dir, "bad.sexp")
    writeFileSync(badMod, `
      (open (use "${LIB}/kernel/prelude.disp"))
      (def lie : (arrow Nat Nat) (lam (n) (pair n n)))
    `)

    const resolveFrontend = (abs: string, cur: typeof dispFrontend) =>
      abs.endsWith(".sexp") ? sexpFrontend : abs.endsWith(".disp") ? dispFrontend : cur

    expect(() => parseProgram(`(open (use "${goodMod}"))`, join(dir, "root.sexp"), {
      session, frontend: sexpFrontend, resolveFrontend,
    })).not.toThrow()

    expect(() => parseProgram(`(open (use "${badMod}"))`, join(dir, "root.sexp"), {
      session, frontend: sexpFrontend, resolveFrontend,
    })).toThrow(/type check failed.*failing entry: lie/)
  }, 180000)

  it("runs declaration guards (`equiv_guard`) identically from S-expression syntax", () => {
    const okSrc = `
      (open (use "${LIB}/kernel/prelude.disp"))
      (guard equiv_guard answer : Nat 3)
      (def answer (rec (new (succ (succ (succ zero)))) (proof t)))
      (test answer 3)
    `
    const decls = parseProgram(okSrc, undefined, { session, frontend: sexpFrontend })
    const t = decls.find(d => d.kind === "Test")
    if (!t || t.kind !== "Test") throw new Error("missing test")
    expect(session.equal!(t.lhs, t.rhs)).toBe(true)

    const badSrc = `
      (open (use "${LIB}/kernel/prelude.disp"))
      (guard equiv_guard answer : Nat 3)
      (def answer (rec (new 4) (proof t)))
    `
    expect(() => parseProgram(badSrc, undefined, { session, frontend: sexpFrontend }))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("attributes unresolved variables from alternate frontends to exact line:col", () => {
    expect(() => parseProgram("(def a t)\n(def b (lam (x) (nope x)))\n", "alt.sexp", {
      session, frontend: sexpFrontend,
    })).toThrow(/unresolved free variable nope at .*alt\.sexp:2:18/)
  })
})
