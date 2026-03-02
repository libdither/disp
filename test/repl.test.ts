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
    const result = processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    expect(result).toBe("Bool : Type")
  })

  it("handles expression with context", () => {
    processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    const result = processLine(state, "Bool")
    expect(result).toContain("Type")
  })

  it("handles number literals", () => {
    const result = processLine(state, "let Nat : Type := (R : Type) -> (R -> R) -> R -> R")
    expect(result).toBe("Nat : Type")
  })

  it("handles true/false keywords in expressions", () => {
    processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    processLine(state, "let not : Bool -> Bool := {b R t f} -> b R f t")
    const result = processLine(state, "not true")
    // Should be recognized as something (not an error)
    expect(result).not.toContain("Error")
  })

  it("bare true/false literals infer their type", () => {
    const result = processLine(state, "true")
    expect(result).not.toContain("Error")
    expect(result).toContain("Type")
  })

  it("bare number literals infer their type", () => {
    const result = processLine(state, "3")
    expect(result).not.toContain("Error")
    expect(result).toContain("Type")
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
    const expectedNames = ["Bool", "not", "and", "or", "Nat", "zero", "succ", "add", "mul", "id"]
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
    fs.writeFileSync(filePath, "let Nat : Type := (R : Type) -> (R -> R) -> R -> R\n")
    const result = processLine(state, `:load ${filePath}`)
    expect(result).toContain("Loaded")
    expect(state.defs.has("Nat")).toBe(true)
  })

  it(":save writes declarations", () => {
    processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    processLine(state, "let not : Bool -> Bool := {b R t f} -> b R f t")
    const filePath = path.join(tmpDir, "out.disp")
    const result = processLine(state, `:save ${filePath}`)
    expect(result).toContain("Saved 2 definitions")
    const content = fs.readFileSync(filePath, "utf-8")
    expect(content).toContain("let Bool")
    expect(content).toContain("let not")
  })

  it("round-trip: save then load produces equivalent context", () => {
    processLine(state, "let Nat : Type := (R : Type) -> (R -> R) -> R -> R")
    processLine(state, "let zero : Nat := {R s z} -> z")
    const filePath = path.join(tmpDir, "roundtrip.disp")
    processLine(state, `:save ${filePath}`)

    // Load into fresh state
    const state2 = initialState()
    const result = processLine(state2, `:load ${filePath}`)
    expect(result).toContain("Loaded")
    expect(state2.defs.has("Nat")).toBe(true)
    expect(state2.defs.has("zero")).toBe(true)
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

describe("REPL - :ctx command", () => {
  it("shows empty context", () => {
    const state = initialState()
    expect(processLine(state, ":ctx")).toBe("(empty context)")
  })

  it("shows context after definitions", () => {
    const state = initialState()
    processLine(state, "let Bool : Type := (R : Type) -> R -> R -> R")
    const result = processLine(state, ":ctx")
    expect(result).toContain("Bool : Type")
  })
})
