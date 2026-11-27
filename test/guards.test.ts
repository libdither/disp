// Host-side pins for the guard layer's REJECTIONS (a guard refusal is a load
// failure, so it can only be asserted from outside the module). The positive
// lifecycle lives in archive/live-kernel/tests/guards.test.disp. These cases
// pin DRIVER machinery against the archived live kernel's relation/license
// library; the promoted kernel's guard protocol is pinned in lib/kernel/.

import { describe, it, expect } from "vitest"
import { join } from "node:path"
import { parseProgram } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const HERE = join(process.cwd(), "lib/tests/_guards_host.disp")
const K = `open use "../../archive/live-kernel/kernel/prelude.disp"
open use "../../archive/live-kernel/std/relation.disp"
`
// One shared native-backend session (the disp.test.ts pattern): the kernel is
// elaborated once for all cases; the eager TS session would blow the worker heap.
const session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
  .createSession() as unknown as Session<Tree>
const run = (src: string) => parseProgram(src, HERE, { session })

describe("guard layer rejections", () => {
  it("freeze refuses rebinds", () => {
    expect(() => run(K + `guard(freeze) V : Nat := 3\nV : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 300000)

  it("a licensed name refuses a plain (credential-less) rebind", () => {
    expect(() => run(K + `guard(license_guard((eq_relation(Nat)).rel)) n : Nat := 3\nn : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("a lying proof is rejected", () => {
    expect(() => run(K + `guard(license_guard((eq_relation(Nat)).rel)) m : Nat := 3\nm : Nat := { new := 4; proof := refl }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("an honest payload rebind passes", () => {
    expect(() => run(K + `guard(license_guard((eq_relation(Nat)).rel)) k : Nat := 3\nk : Nat := { new := 3; proof := refl }\ntest k = 3\n`))
      .not.toThrow()
  }, 120000)

  it("license_guard_deriv: a derivation with mismatched endpoints is rejected", () => {
    // The rule relates id to succ; the rebind proposes succ∘succ, so check_deriv
    // computes false (tree_eq at the rule instance's rhs) and the load fails.
    expect(() => run(K + `open use "../../archive/live-kernel/std/deriv.disp"
guard(license_guard_deriv(cons({_} => pair({n} => n, {n} => succ(n)), nil), nil)) g : Arrow(Nat, Nat) := ({n} => n)
g := { new := ({n} => succ(succ(n))) ; deriv := (d_rule(0, t)) }
`)).toThrow(/rejected by its guard/)
  }, 120000)

  it("two_face_guard: a worker that fails a constructor obligation is rejected", () => {
    // The zero obligation demands Eq Nat 0 (succ 0); refl cannot close it, so the
    // rebind fails the load (the shift-shaped attack that the pointwise license
    // accepts — probe_license_sr — dies here at the concrete face).
    expect(() => run(K + `let NatRB := {u, v} => Eq(Nat, u, v)
guard(two_face_guard(PositiveFnCoverage(Nat, NatRB))) f : Arrow(Nat, Nat) := ({n} => n)
f := { fast := ({n} => succ(n)) ; proof := { zero := refl; succ := ({k} => {_ih} => refl) } }
`)).toThrow(/rejected by its guard/)
  }, 120000)

  it("ownership is not surrendered (guard proposals refused)", () => {
    expect(() => run(K + `guard(license_guard((eq_relation(Nat)).rel)) p : Nat := 3\nguard(freeze) p : Nat := { new := 3; proof := refl }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("shadowing default_guard changes the ambient policy for the scope", () => {
    expect(() => run(K + `let default_guard := freeze\nx : Nat := 3\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("`let` as a head makes the binding private (bound, not exported)", () => {
    const decls = run(K + `let p : Nat := 3\ntest p = 3\n`)
    expect(decls.some(d => d.kind === "Def" && d.name === "p")).toBe(false)
  }, 120000)

  it("shadowing `let` changes what the scope's lets mean (a let that exports)", () => {
    // `let` is an ordinary library value: rebind it to a decorator that leaves the
    // request public, and subsequent `let`s in the scope EXPORT. (The pristine
    // fast path only applies while `let` is tree-identical to the cut.disp value.)
    const decls = run(K
      + `let := {req} => { value := req.value; ty := req.ty; guard := req.guard; private := false }\n`
      + `let vis : Nat := 7\ntest vis = 7\n`)
    expect(decls.some(d => d.kind === "Def" && d.name === "vis")).toBe(true)
  }, 120000)

  it("the `test` marker is a value: shadowing it preprocesses equation lhs", () => {
    const testOf = (src: string) => {
      const t = run(K + src).find(d => d.kind === "Test")
      if (!t || t.kind !== "Test") throw new Error("missing test")
      return t
    }
    // Pristine marker: `test zero = succ(zero)` peels to zero vs succ zero — unequal.
    const pristine = testOf(`test zero = succ(zero)\n`)
    expect(session.equal!(pristine.lhs, pristine.rhs)).toBe(false)
    // Shadowed marker RUNS (no peel): lhs = succ zero — equal.
    const shadowed = testOf(`test := succ\ntest zero = succ(zero)\n`)
    expect(session.equal!(shadowed.lhs, shadowed.rhs)).toBe(true)
  }, 120000)
})

const P = `open use "../kernel/prelude.disp"\n`

describe("promoted kernel guard layer", () => {
  it("freeze refuses rebinds", () => {
    expect(() => run(P + `guard(freeze) V : Nat := 3\nV : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("equiv_guard refuses a plain (credential-less) rebind", () => {
    expect(() => run(P + `guard(equiv_guard) n : Nat := 3\nn : Nat := 4\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("equiv_guard refuses a lying value proof", () => {
    expect(() => run(P + `guard(equiv_guard) m : Nat := 3\nm : Nat := { new := 4; proof := t }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("equiv_guard accepts an honest value rebind", () => {
    expect(() => run(P + `guard(equiv_guard) k : Nat := 3\nk : Nat := { new := succ(succ(succ(zero))); proof := t }\ntest k = 3\n`))
      .not.toThrow()
  }, 120000)

  it("fn_equiv_guard rejects a bogus proof and the shift spoofing attack", () => {
    const setup = P + `
rec add_s := {a, b} => if (is_zero(a)) { b } else { succ(add_s(nat_pred(a), b)) }
rec dbl_f := {n} => if (is_zero(n)) { zero } else { succ(succ(dbl_f(nat_pred(n)))) }
guard(fn_equiv_guard(Nat)) dbl_g : Nat -> Nat := {n} => add_s(n, n)
`
    expect(() => run(setup + `dbl_g := { new := dbl_f; proof := {_n} => t }\n`))
      .toThrow(/rejected by its guard/)
    // Shift attack that previously fooled the archived live kernel's pointwise license_guard:
    // the evaluator-map kernel splits symbolic Nat into concrete 0 / succ(k), catching 0 != 1.
    expect(() => run(setup + `
dbl_shift := {n} => if (is_zero(n)) { 1 } else { dbl_f(n) }
dbl_g := { new := dbl_shift; proof := {n} => if (is_zero(n)) { t } else { t } }
`)).toThrow(/rejected by its guard/)
  }, 120000)

  it("fn_equiv_guard rejects forged (uncertified) certs and buggy list rewrites", () => {
    const setup = P + `guard(fn_equiv_guard(List(Tree))) my_rev : List(Tree) -> List(Tree) := reverse\n`
    expect(() => run(setup + `my_rev := { new := {xs} => xs; proof := {_xs} => t }\n`))
      .toThrow(/rejected by its guard/)
    // Attempting to inject an uncertified lemma pair in `certs` is rejected by `unwrap_certs`.
    expect(() => run(setup + `
fake_cert := [pair("lie", pair({xs} => t, {xs : List(Tree)} -> Eq(xs, reverse(xs))))]
my_rev := { new := {xs} => xs; proof := {_xs} => t; certs := fake_cert }
`)).toThrow(/rejected by its guard/)
  }, 120000)

  it("ownership is not surrendered (guard proposals refused)", () => {
    expect(() => run(P + `guard(equiv_guard) p : Nat := 3\nguard(freeze) p : Nat := { new := 3; proof := t }\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("shadowing default_guard changes the ambient policy for the scope", () => {
    expect(() => run(P + `let default_guard := freeze\nx : Nat := 3\n`))
      .toThrow(/rejected by its guard/)
  }, 120000)

  it("shadowing `let` makes subsequent lets export publicly", () => {
    const decls = run(P
      + `let := {req} => mk_req(field(req, "value"), field(req, "ty"), field(req, "guard"), false, false)\n`
      + `let vis : Nat := 7\ntest vis = 7\n`)
    expect(decls.some(d => d.kind === "Def" && d.name === "vis")).toBe(true)
  }, 120000)

  it("open-splice reconciles verified_opt.disp and verified_opt.opt.disp in BOTH orders", () => {
    const getBinding = (src: string, name: string) => {
      const d = run(src).find(x => x.kind === "Def" && x.name === name)
      if (!d || d.kind !== "Def") throw new Error(`missing ${name}`)
      return d.tree
    }
    const specRev = getBinding(`open use "../kernel/verified_opt.disp"\n`, "rev")
    const optRev = getBinding(`open use "../kernel/verified_opt.opt.disp"\n`, "rev")
    expect(session.equal!(specRev, optRev)).toBe(false)

    // Order 1: spec then opt -> opt wins
    const order1 = getBinding(`open use "../kernel/verified_opt.disp"\nopen use "../kernel/verified_opt.opt.disp"\n`, "rev")
    expect(session.equal!(order1, optRev)).toBe(true)

    // Order 2: opt then spec -> opt STILL wins (late-arriving spec does not overwrite licensed upgrade)
    const order2 = getBinding(`open use "../kernel/verified_opt.opt.disp"\nopen use "../kernel/verified_opt.disp"\n`, "rev")
    expect(session.equal!(order2, optRev)).toBe(true)
  }, 180000)
})
