// Host-side pins for lib/theories/checked.disp: a definition `judge` does not prove is a
// load error, so it can only be asserted from outside the module. The error names the
// definition and prints its verdict. The accepted side is lib/theories/checked.test.disp.

import { describe, it, expect, vi } from "vitest"
import { join } from "node:path"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const LIB = join(process.cwd(), "lib")
const session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
  .createSession() as unknown as Session<Tree>

// a checked module with this body, loaded (and so checked) from a root next to it
function load(body: string): () => unknown {
  const dir = mkdtempSync(join(tmpdir(), "disp-checked-"))
  const mod = join(dir, "mod.disp")
  writeFileSync(mod, `open use raw "${LIB}/prelude.disp" {}\nopen use raw "${LIB}/list.disp" {}\nopen use raw "${LIB}/theories/checked.disp" {}\n${body}\n`)
  return () => parseProgram(`open use "${mod}" {}\n`, join(dir, "root.disp"), { session })
}

describe("checked.disp rejections", () => {
  it("a Proved definition loads", () => {
    expect(load(`next : Nat -> Nat := {n} => succ(n)`)).not.toThrow()
  }, 120000)

  it("a refuted definition is a load error that prints its counterexample", () => {
    expect(load(`dup : Nat -> Nat := {n} => pair(n, n)`))
      .toThrow(/failing entry: dup \(t\("Refuted"/)
  }, 120000)

  it("a program that inspects its function argument is refuted with a stored function", () => {
    expect(load(`peek : (Nat -> Nat) -> Nat := {g} => g`))
      .toThrow(/failing entry: peek \(t\("Refuted"/)
  }, 120000)

  it("an open definition is rejected too, with its reason", () => {
    expect(load(`spin : Nat -> Nat := fix({self, n} => self(n))`))
      .toThrow(/failing entry: spin \(t\("Open"/)
  }, 120000)

  it("only the failing definitions are named", () => {
    expect(load(`next : Nat -> Nat := {n} => succ(n)\ndup : Nat -> Nat := {n} => pair(n, n)`))
      .toThrow(/failing entry: dup \(/)
  }, 120000)

  const cat = `rec cat : {xs : List(Nat), ys : List(Nat)} -> List(Nat) :=> if (is_leaf(xs)) { ys } else { cons(xs.fst, cat(xs.snd, ys)) }`
  const rev = `rec rev : {xs : List(Nat)} -> List(Nat) :=> if (is_leaf(xs)) { [] } else { cat(rev(xs.snd), [xs.fst]) }`
  it("an unannotated callee is read by a type among the goal's own", () => {
    const plus = `rec plus := {a, b} => if (is_zero(a)) { b } else { succ(plus(nat_pred(a), b)) }`
    expect(load(`${plus}\nrec times : {a : Nat, b : Nat} -> Nat :=> if (is_zero(a)) { 0 } else { plus(b, times(nat_pred(a), b)) }`)).not.toThrow()
    // squaring is Nat -> Nat, and plus's type is not built from that
    expect(load(`${plus}\nrec times := {a, b} => if (is_zero(a)) { 0 } else { plus(b, times(nat_pred(a), b)) }\nsquare : Nat -> Nat := {k} => times(k, k)`))
      .toThrow(/failing entry: square \(t\("Open"/)
  }, 120000)

  it("a Proved definition is a lemma for the ones after it; an unannotated one whose type the goal lacks is none", () => {
    expect(load(`${cat}\n${rev}`)).not.toThrow()
    const unchecked = `rec cat := {xs, ys} => if (is_leaf(xs)) { ys } else { cons(xs.fst, cat(xs.snd, ys)) }`
    expect(load(`${unchecked}\n${rev}`)).toThrow(/failing entry: rev \(t\("Open"/)
  }, 120000)

  it("a type comes with its eliminator, and an equation proved with it loads", () => {
    const sum = `down := eliminator(Nat)\nsum : Nat -> Nat -> Nat := {a, b} => down(a, b, {_k, r} => succ(r))`
    expect(load(`${sum}\nsquare : Nat -> Nat := {k} => down(k, 0, {_j, r} => sum(k, r))`)).not.toThrow()
    // a step that does not answer in the result type is not what the type's rule asks
    expect(load(`${sum}\nbad : Nat -> Nat -> Nat := {a, b} => down(a, b, {_k, r} => pair(r, r))`))
      .toThrow(/failing entry: bad \(/)
    // n + 0 = n by eliminating n: equal successors are equal numbers, so the step's hypothesis is its proof
    const proof = `{n} => down(n, t, {_k, ih} => ih)`
    expect(load(`${sum}\nsum_zero : {n : Nat} -> Eq(sum(n, 0), n) := ${proof}`)).not.toThrow()
    // the same proof of a false equation is refuted with a number
    expect(load(`${sum}\nsum_one : {n : Nat} -> Eq(sum(n, 1), n) := ${proof}`))
      .toThrow(/failing entry: sum_one \(t\("Refuted"/)
    // an equation already Proved is used through equality's own eliminator: here it is turned around
    expect(load(`${sum}\nsum_zero : {n : Nat} -> Eq(sum(n, 0), n) := ${proof}\nzero_sum : {n : Nat} -> Eq(n, sum(n, 0)) := {n} => cast(sum_zero(n), t)`)).not.toThrow()
    // and a proof that eliminates nothing does no induction
    expect(load(`${sum}\nidle : {n : Nat} -> Eq(sum(n, 0), n) := {_n} => t`))
      .toThrow(/failing entry: idle \(t\("Open"/)
  }, 120000)
})

describe("checked.disp settings", () => {
  it("with on_open := \"assume\", an open definition loads with a note, and one proved with it rests on it", () => {
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {})
    try {
      expect(load(`check_module := check_with{ on_open := "assume" }
rev_l : List(Nat) -> List(Nat) := reverse
back_again : List(Nat) -> List(Nat) := {xs} => reverse(reverse(xs))`)).not.toThrow()
      const notes = warn.mock.calls.map(c => String(c[0])).join("\n")
      expect(notes).toMatch(/loads on assumptions: rev_l \(t\("Open".*back_again \(t\("resting on", t\("rev_l"/)
    } finally { warn.mockRestore() }
  }, 120000)

  it("a refuted definition still fails under on_open := \"assume\"", () => {
    expect(load(`check_module := check_with{ on_open := "assume" }\ndup : Nat -> Nat := {n} => pair(n, n)`))
      .toThrow(/failing entry: dup \(t\("Refuted"/)
  }, 120000)

  it("the search budgets are settings: with no splits, a proof by cases stays open", () => {
    expect(load(`check_module := check_with{ depth := 0 }
rec twice : {n : Nat} -> Nat :=> if (is_zero(n)) { 0 } else { succ(succ(twice(nat_pred(n)))) }`))
      .toThrow(/failing entry: twice \(t\("Open"/)
  }, 120000)
})
