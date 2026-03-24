import { describe, it, expect, beforeEach, afterEach } from "vitest"
import * as fs from "node:fs"
import * as path from "node:path"
import * as os from "node:os"
import { initialState, processLine, loadFile } from "../src/repl.js"
import type { ReplState } from "../src/repl.js"

describe("REPL - processLine", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
  })

  it("handles empty lines", () => {
    expect(processLine(state, "")).toBe("")
    expect(processLine(state, "   ")).toBe("")
  })

  it("handles comments", () => {
    expect(processLine(state, "-- this is a comment")).toBe("")
  })

  it("handles declaration", () => {
    const result = processLine(state, "let MyType : Type := (R : Type) -> R -> R -> R")
    expect(result).toBe("MyType : Type")
  })

  it("handles expression with context", () => {
    const result = processLine(state, "Bool")
    expect(result).toContain("Type")
  })

  it("handles number literals", () => {
    // Nat is already a primitive type; define something that uses it
    const result = processLine(state, "let myZero : Nat := zero")
    expect(result).toBe("myZero : Nat")
  })

  it("handles true/false keywords in expressions", () => {
    processLine(state, "let not : Bool -> Bool := {b} -> boolElim Bool false true b")
    const result = processLine(state, "not true")
    // Should be recognized as something (not an error)
    expect(result).not.toContain("Error")
  })

  it("bare true/false literals infer their type", () => {
    const result = processLine(state, "true")
    expect(result).not.toContain("Error")
    expect(result).toContain("Bool")
  })

  it("bare number literals infer their type", () => {
    const result = processLine(state, "3")
    expect(result).not.toContain("Error")
  })
})

describe("REPL - prelude loading", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
  })

  it("loads prelude.disp without errors", () => {
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    const result = loadFile(state, preludePath)
    expect(result).not.toContain("Error")
    expect(result).toContain("Loaded")
  })

  it("prelude defines expected names", () => {
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    loadFile(state, preludePath)
    // Bool, Nat, zero, succ are now primitives (already in defs before prelude)
    // Prelude defines not, and, or, add, mul, id, fold, etc.
    const expectedNames = ["not", "and", "or", "add", "mul", "id", "fold"]
    for (const name of expectedNames) {
      expect(state.defs.has(name)).toBe(true)
    }
  })

  it("expressions work after prelude load", () => {
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    loadFile(state, preludePath)
    const result = processLine(state, "add 2 3")
    expect(result).not.toContain("Error")
  })
})

describe("REPL - :load and :save", () => {
  let state: ReplState
  let tmpDir: string

  beforeEach(() => {
    state = initialState()
    tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "disp-test-"))
  })

  afterEach(() => {
    fs.rmSync(tmpDir, { recursive: true, force: true })
  })

  it(":load loads a file", () => {
    const filePath = path.join(tmpDir, "test.disp")
    fs.writeFileSync(filePath, "let MyNat : Type := (R : Type) -> (R -> R) -> R -> R\n")
    const result = processLine(state, `:load ${filePath}`)
    expect(result).toContain("Loaded")
    expect(state.defs.has("MyNat")).toBe(true)
  })

  it(":save writes declarations", () => {
    processLine(state, "let not : Bool -> Bool := {b} -> boolElim Bool false true b")
    processLine(state, "let myVal : Bool := true")
    const filePath = path.join(tmpDir, "out.disp")
    const result = processLine(state, `:save ${filePath}`)
    expect(result).toContain("Saved 2 definitions")
    const content = fs.readFileSync(filePath, "utf-8")
    expect(content).toContain("let not")
    expect(content).toContain("let myVal")
  })

  it("round-trip: save then load produces equivalent context", () => {
    processLine(state, "let MyNat : Type := (R : Type) -> (R -> R) -> R -> R")
    processLine(state, "let myZero : MyNat := {R s z} -> z")
    const filePath = path.join(tmpDir, "roundtrip.disp")
    processLine(state, `:save ${filePath}`)

    // Load into fresh state
    const state2 = initialState()
    const result = processLine(state2, `:load ${filePath}`)
    expect(result).toContain("Loaded")
    expect(state2.defs.has("MyNat")).toBe(true)
    expect(state2.defs.has("myZero")).toBe(true)
  })

  it(":load with missing file gives error", () => {
    const result = processLine(state, ":load nonexistent.disp")
    expect(result).toContain("Error")
  })

  it(":load handles multi-line definitions", () => {
    const filePath = path.join(tmpDir, "multiline.disp")
    fs.writeFileSync(filePath, [
      "let myId :",
      "  (A : Type) ->",
      "  A -> A :=",
      "  {A x} -> x",
      "",
      "let test : Nat := myId Nat 42",
    ].join("\n"))
    const result = processLine(state, `:load ${filePath}`)
    expect(result).toContain("Loaded")
    expect(result).toContain("2 definitions")
    const val = processLine(state, "test")
    expect(val).toBe("42 : Nat")
  })
})

describe("REPL - type-guided recognition", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    loadFile(state, preludePath)
  })

  it("1 shows as 1, not id", () => {
    const result = processLine(state, "1")
    expect(result).toMatch(/^1 :/)
  })

  it("true shows as true, not id", () => {
    const result = processLine(state, "true")
    expect(result).toMatch(/^true :/)
  })

  it("false shows as false, not zero", () => {
    const result = processLine(state, "false")
    expect(result).toMatch(/^false :/)
  })

  it("id shows as id (polymorphic type)", () => {
    const result = processLine(state, "id")
    expect(result).toMatch(/^id :/)
  })

  it("not true shows as false", () => {
    const result = processLine(state, "not true")
    expect(result).toMatch(/^false : Bool/)
  })

  it("add 2 3 shows as 5", () => {
    const result = processLine(state, "add 2 3")
    expect(result).toMatch(/^5 : Nat/)
  })
})

describe("REPL - recursive definitions", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    // Nat is now a primitive type, no need to define it
  })

  it("let rec myId : Nat -> Nat := {n} -> n then myId 5 = 5", () => {
    const result1 = processLine(state, "let rec myId : Nat -> Nat := {n} -> n")
    expect(result1).not.toContain("Error")
    expect(result1).toContain("myId")
    const result2 = processLine(state, "myId 5")
    expect(result2).toMatch(/^5 : Nat/)
  })

  it("let rec without type annotation errors", () => {
    const result = processLine(state, "let rec f := {x} -> x")
    expect(result).toContain("error")
  })

  it("save/load round-trip preserves let rec", () => {
    const fs = require("fs")
    const os = require("os")
    const path = require("path")
    processLine(state, "let rec myId : Nat -> Nat := {n} -> n")
    const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "disp-rec-"))
    const filePath = path.join(tmpDir, "rec.disp")
    processLine(state, `:save ${filePath}`)
    const content = fs.readFileSync(filePath, "utf-8")
    expect(content).toContain("let rec myId")
    fs.rmSync(tmpDir, { recursive: true, force: true })
  })
})

describe("REPL - records", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    loadFile(state, preludePath)
  })

  it("record projection works via application", () => {
    processLine(state, "let myPair : {fst : Nat, snd : Nat} := {fst := 3, snd := 5}")
    const result = processLine(state, "myPair Nat ({x y} -> x)")
    expect(result).not.toContain("Error")
    expect(result).toMatch(/^3/)
  })
})

describe("REPL - integration: prelude records and coproducts", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    const preludePath = path.resolve(__dirname, "../prelude.disp")
    loadFile(state, preludePath)
  })

  it("prelude defines Pair, mkPair, fst, snd, Either, inl, inr", () => {
    for (const name of ["Pair", "mkPair", "fst", "snd", "Either", "inl", "inr"]) {
      expect(state.defs.has(name)).toBe(true)
    }
  })

  it("fst Nat Nat (mkPair Nat Nat 3 5) = 3", () => {
    const result = processLine(state, "fst Nat Nat (mkPair Nat Nat 3 5)")
    expect(result).toMatch(/^3 : Nat/)
  })

  it("snd Nat Nat (mkPair Nat Nat 3 5) = 5", () => {
    const result = processLine(state, "snd Nat Nat (mkPair Nat Nat 3 5)")
    expect(result).toMatch(/^5 : Nat/)
  })

  it("coproduct elimination via application", () => {
    processLine(state, "let e : Either Nat Nat := inl Nat Nat 3")
    const result = processLine(state, "e Nat ({x} -> x) ({y} -> y)")
    expect(result).toMatch(/^3/)
  })
})

describe("REPL - :ctx command", () => {
  it("shows CoC prelude builtins by default", () => {
    const state = initialState()
    const result = processLine(state, ":ctx")
    expect(result).toContain("Tree")
    expect(result).toContain("leaf")
  })

  it("shows context after definitions", () => {
    const state = initialState()
    processLine(state, "let MyBool : Type := (R : Type) -> R -> R -> R")
    const result = processLine(state, ":ctx")
    expect(result).toContain("MyBool : Type")
  })
})

describe("REPL - type errors", () => {
  it("rejects type mismatch in application", () => {
    const state = initialState()
    // Bool, Nat, zero are now primitives
    processLine(state, "let id : (A : Type) -> A -> A := {A x} -> x")
    const result = processLine(state, "id Bool zero")
    expect(result).toContain("error")
  })

  it("rejects applying non-function", () => {
    const state = initialState()
    const result = processLine(state, "Type Type")
    expect(result).toContain("error")
  })

  it("rejects lambda without function type annotation", () => {
    const state = initialState()
    const result = processLine(state, "{x} -> x")
    expect(result).toContain("error")
  })
})

describe("REPL - computational equality (delta reduction)", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    loadFile(state, path.resolve("prelude.disp"), true)
    processLine(state, "let Eq : (A : Type) -> A -> A -> Type := {A x y} -> (P : A -> Type) -> P x -> P y")
    processLine(state, "let refl : (A : Type) -> (x : A) -> Eq A x x := {A x P px} -> px")
  })

  it("not true = false", () => {
    const result = processLine(state, "let _ : Eq Bool (not true) false := refl Bool false")
    expect(result).not.toContain("error")
  })

  it("not false = true", () => {
    const result = processLine(state, "let _ : Eq Bool (not false) true := refl Bool true")
    expect(result).not.toContain("error")
  })

  it("not (not true) = true", () => {
    const result = processLine(state, "let _ : Eq Bool (not (not true)) true := refl Bool true")
    expect(result).not.toContain("error")
  })

  it("add 1 1 = 2", () => {
    const result = processLine(state, "let _ : Eq Nat (add 1 1) 2 := refl Nat 2")
    expect(result).not.toContain("error")
  })

  it("add 2 3 = 5", () => {
    const result = processLine(state, "let _ : Eq Nat (add 2 3) 5 := refl Nat 5")
    expect(result).not.toContain("error")
  })

  it("mul 2 3 = 6", () => {
    const result = processLine(state, "let _ : Eq Nat (mul 2 3) 6 := refl Nat 6")
    expect(result).not.toContain("error")
  })

  it("mul 3 3 = 9", () => {
    const result = processLine(state, "let _ : Eq Nat (mul 3 3) 9 := refl Nat 9")
    expect(result).not.toContain("error")
  })

  it("add 0 n = n", () => {
    const result = processLine(state, "let _ : Eq Nat (add 0 5) 5 := refl Nat 5")
    expect(result).not.toContain("error")
  })

  it("mul 0 n = 0", () => {
    const result = processLine(state, "let _ : Eq Nat (mul 0 100) 0 := refl Nat 0")
    expect(result).not.toContain("error")
  })
})

describe("REPL - stdlib", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    loadFile(state, path.resolve("prelude.disp"), true)
    loadFile(state, path.resolve("stdlib.disp"), true)
  })

  it("loads stdlib without errors", () => {
    expect(state.cocEnv.has("compose")).toBe(true)
    expect(state.cocEnv.has("Eq")).toBe(true)
    expect(state.cocEnv.has("refl")).toBe(true)
    expect(state.cocEnv.has("sym")).toBe(true)
    expect(state.cocEnv.has("trans")).toBe(true)
    expect(state.cocEnv.has("cong")).toBe(true)
    expect(state.cocEnv.has("subst")).toBe(true)
    expect(state.cocEnv.has("List")).toBe(true)
    expect(state.cocEnv.has("nil")).toBe(true)
    expect(state.cocEnv.has("cons")).toBe(true)
    expect(state.cocEnv.has("filter")).toBe(true)
  })

  it("xor works", () => {
    expect(processLine(state, "xor true false")).toBe("true : Bool")
    expect(processLine(state, "xor true true")).toBe("false : Bool")
  })

  it("implies works", () => {
    expect(processLine(state, "implies true false")).toBe("false : Bool")
    expect(processLine(state, "implies false true")).toBe("true : Bool")
  })

  it("list length works", () => {
    expect(processLine(state, "length Nat (cons Nat 1 (cons Nat 2 (cons Nat 3 (nil Nat))))")).toBe("3 : Nat")
  })

  it("list sum works", () => {
    expect(processLine(state, "sum (cons Nat 10 (cons Nat 20 (cons Nat 30 (nil Nat))))")).toBe("60 : Nat")
  })

  it("computational proofs type-check", () => {
    expect(state.cocEnv.has("not_true_eq")).toBe(true)
    expect(state.cocEnv.has("add_2_3")).toBe(true)
    expect(state.cocEnv.has("mul_3_3")).toBe(true)
  })

  it("sym and trans compose with proofs", () => {
    const result = processLine(state, "sym Bool (not true) false not_true_eq")
    expect(result).not.toContain("error")
  })

  it("not_involution exists", () => {
    expect(state.cocEnv.has("not_involution")).toBe(true)
  })

  it("natRec exists", () => {
    expect(state.cocEnv.has("natRec")).toBe(true)
  })
})

describe("REPL - dependent elimination", () => {
  let state: ReplState

  beforeEach(() => {
    state = initialState()
    loadFile(state, path.resolve("prelude.disp"), true)
    loadFile(state, path.resolve("stdlib.disp"), true)
  })

  it("boolElimDep type-checks with dependent motive", () => {
    // Type-checks with a dependent motive (P : Bool -> Type)
    const r1 = processLine(state, "boolElimDep ({_} -> Nat) 1 0 true")
    expect(r1).not.toContain("error")
    expect(r1).toContain("Nat")
    const r2 = processLine(state, "boolElimDep ({_} -> Nat) 1 0 false")
    expect(r2).not.toContain("error")
    expect(r2).toContain("Nat")
  })

  it("natElimDep type-checks with dependent motive", () => {
    const r1 = processLine(state, "natElimDep ({_} -> Nat) 0 ({n} -> succ n) 3")
    expect(r1).not.toContain("error")
    expect(r1).toContain("Nat")
    const r2 = processLine(state, "natElimDep ({_} -> Nat) 0 ({n} -> succ n) 0")
    expect(r2).not.toContain("error")
    expect(r2).toContain("Nat")
  })

  it("not_involution applies to true", () => {
    const result = processLine(state, "not_involution true")
    expect(result).not.toContain("error")
  })

  it("not_involution applies to false", () => {
    const result = processLine(state, "not_involution false")
    expect(result).not.toContain("error")
  })

  it("natRec type-checks", () => {
    // natRec exists and has the right type
    const result = processLine(state, ":type natRec")
    expect(result).not.toContain("error")
    expect(result).toContain("Nat")
  })
})
