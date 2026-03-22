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
