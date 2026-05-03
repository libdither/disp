// Intended trust-system behavior. These tests are skipped until trust
// provenance is implemented in the elaborator.
//
// Each scenario is written as a small Disp program and states whether parsing
// / elaboration should accept it or reject it once untrusted type positions are
// enforced.

import { describe, it, expect } from "vitest"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"

import { parseProgram } from "../src/compile.js"

const libDir = join(import.meta.dirname, "..", "lib")
const sourcePath = join(libDir, "__trust_scenario.disp")

const kernelHeader = `
open use "prelude.disp"
trust open use "kernel.disp"
`

function shouldPass(src: string): void {
  expect(() => parseProgram(src, sourcePath, { debugTypeCheck: true })).not.toThrow()
}

function shouldThrow(src: string): void {
  expect(() => parseProgram(src, sourcePath, { debugTypeCheck: true })).toThrow()
}

function tempDispFile(name: string, src: string): string {
  const dir = mkdtempSync(join(tmpdir(), "disp-trust-"))
  const path = join(dir, name)
  writeFileSync(path, src)
  return path
}

describe.skip("trust provenance", () => {
  describe("trusted kernel types", () => {
    it("accepts trusted kernel types in annotations", () => {
      shouldPass(`${kernelHeader}
let id : Nat -> Nat = {x} -> x
let not : Bool -> Bool = {b} -> bool_rec ({_} -> Bool) FF TT b
test id 3 = 3
test not TT = FF
`)
    })

    it("accepts trusted dependent annotations", () => {
      shouldPass(`${kernelHeader}
let refl_nat : {n : Nat} -> Eq Nat n n = {n} -> refl
test refl_nat 2 = t
`)
    })

    it("accepts trusted aliases declared with trust", () => {
      shouldPass(`${kernelHeader}
trust MyNat : Type 0 = Nat
let id : MyNat -> MyNat = {x} -> x
test id 4 = 4
`)
    })
  })

  describe("untrusted values in term positions", () => {
    it("allows untrusted predicates to run as ordinary values", () => {
      shouldPass(`${kernelHeader}
let Any = {_} -> TT
let accepts_leaf = Any t
test accepts_leaf = TT
`)
    })

    it("allows an untrusted helper to be rechecked behind a trusted annotation", () => {
      shouldPass(`${kernelHeader}
let id_impl = {x} -> x
trust id_nat : Nat -> Nat = id_impl
test id_nat 5 = 5
`)
    })

    it("rejects an untrusted implementation that fails the trusted boundary", () => {
      shouldThrow(`${kernelHeader}
let bad_impl = {_} -> FF
trust bad_nat : Nat -> Nat = bad_impl
`)
    })
  })

  describe("untrusted values in type positions", () => {
    it("rejects an untrusted predicate as a direct annotation", () => {
      shouldThrow(`${kernelHeader}
let Any = {_} -> TT
let bogus : Any = FF
`)
    })

    it("rejects an untrusted alias of a trusted core type", () => {
      shouldThrow(`${kernelHeader}
let MyNat = Nat
let x : MyNat = 0
`)
    })

    it("rejects an untrusted predicate in an arrow domain", () => {
      shouldThrow(`${kernelHeader}
let Any = {_} -> TT
let f : Any -> Nat = {_} -> 0
`)
    })

    it("rejects an untrusted predicate in a dependent binder domain", () => {
      shouldThrow(`${kernelHeader}
let Any = {_} -> TT
let f : {x : Any} -> Nat = {_} -> 0
`)
    })

    it("rejects an untrusted predicate in a type definition", () => {
      shouldThrow(`${kernelHeader}
let Any = {_} -> TT
let Alias : Type 0 = Any
`)
    })

    it("rejects trust declarations whose annotation is untrusted", () => {
      shouldThrow(`${kernelHeader}
let Any = {_} -> TT
trust bogus : Any = t
`)
    })
  })

  describe("trust propagation", () => {
    it("marks definitions checked against trusted annotations as trusted-by-construction", () => {
      shouldPass(`${kernelHeader}
let id : Nat -> Nat = {x} -> x
let use_id : Nat -> Nat = {n} -> id n
test use_id 6 = 6
`)
    })

    it("does not let an untrusted alias become trusted just because it reduces to a trusted type", () => {
      shouldThrow(`${kernelHeader}
let Alias = Nat
let id : Alias -> Alias = {x} -> x
`)
    })

    it("allows trust to establish a checked wrapper around an untrusted value", () => {
      shouldPass(`${kernelHeader}
let raw_const = {_} -> 0
trust const_zero : Nat -> Nat = raw_const
let use_const : Nat -> Nat = {n} -> const_zero n
test use_const 9 = 0
`)
    })
  })

  describe("imports", () => {
    it("rejects untrusted imported predicates in local annotations", () => {
      const untrustedLib = tempDispFile("untrusted.disp", `${kernelHeader}
FakeNat := {_} -> TT
`)

      shouldThrow(`${kernelHeader}
open use "${untrustedLib}"
let x : FakeNat = FF
`)
    })

    it("preserves trusted imported aliases through ordinary open", () => {
      const trustedLib = tempDispFile("trusted.disp", `${kernelHeader}
trust MyNat : Type 0 = Nat
`)

      shouldPass(`${kernelHeader}
open use "${trustedLib}"
let x : MyNat = 7
test x = 7
`)
    })

    it("does not let trust open upgrade untrusted exports", () => {
      const untrustedLib = tempDispFile("untrusted-open.disp", `${kernelHeader}
FakeNat := {_} -> TT
`)

      shouldThrow(`${kernelHeader}
trust open use "${untrustedLib}"
let x : FakeNat = t
`)
    })
  })

  describe("public neutral constructors", () => {
    it("rejects user code that manufactures StuckElim directly", () => {
      shouldThrow(`${kernelHeader}
let fake_proof : Eq Nat 0 1 = StuckElim (Eq Nat 0 1) t
`)
    })

    it("still allows trusted eliminators to produce stuck eliminations internally", () => {
      shouldPass(`${kernelHeader}
let bool_to_nat : Bool -> Nat = {b} -> bool_rec ({_} -> Nat) 1 0 b
test bool_to_nat TT = 1
test bool_to_nat FF = 0
`)
    })
  })
})
