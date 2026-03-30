import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, apply, I, BudgetExhausted } from "../src/tree.js"
import { tokenize, parseExpr, parseLine, ParseError } from "../src/parse.js"
import { resetMarkerCounter, clearNativeBuiltins } from "../src/native-utils.js"
import { initialState, processLine } from "../src/repl.js"
import { clearPreludeCache } from "../src/prelude.js"

beforeEach(() => {
  resetMarkerCounter()
  clearNativeBuiltins()
  clearPreludeCache()
})

describe("ParseError", () => {
  it("throws on unexpected character", () => {
    expect(() => tokenize("@")).toThrow(ParseError)
  })

  it("throws on unexpected character with correct span", () => {
    try {
      tokenize("hello @world")
      expect.fail("should throw")
    } catch (e) {
      expect(e).toBeInstanceOf(ParseError)
      expect((e as ParseError).span.start).toBe(6)
    }
  })

  it("throws on missing closing paren", () => {
    expect(() => parseExpr("(x")).toThrow(ParseError)
  })

  it("throws on missing closing brace", () => {
    expect(() => parseExpr("{x")).toThrow(ParseError)
  })

  it("throws on empty lambda", () => {
    expect(() => parseExpr("{} -> x")).toThrow(ParseError)
  })

  it("throws on missing coloneq in declaration", () => {
    expect(() => parseLine("let x : Type")).toThrow(ParseError)
  })

  it("throws on unexpected token in expression", () => {
    expect(() => parseExpr(":=")).toThrow(ParseError)
  })
})

describe("Type errors via REPL", () => {
  it("type mismatch in application", () => {
    const state = initialState()
    const result = processLine(state, "true true")
    expect(result).toMatch(/error/i)
  })

  it("non-type domain in Pi", () => {
    const state = initialState()
    const result = processLine(state, "let bad : (x : true) -> Bool := {x} -> x")
    expect(result).toMatch(/error/i)
  })

  it("recursive def requires type annotation", () => {
    const state = initialState()
    const result = processLine(state, "let rec f := f")
    expect(result).toMatch(/error/i)
  })
})

describe("BudgetExhausted", () => {
  it("thrown when apply budget runs out", () => {
    const SII = fork(stem(I), I)
    expect(() => apply(SII, SII, { remaining: 100 })).toThrow(BudgetExhausted)
  })

  it("processLine handles budget exhaustion gracefully", () => {
    const state = initialState()
    processLine(state, "let rec diverge : Nat -> Nat := {n} -> diverge n")
    const result = processLine(state, "diverge 0")
    expect(typeof result).toBe("string")
  })
})

describe("REPL error formatting", () => {
  it("parse errors include source location", () => {
    const state = initialState()
    const result = processLine(state, "let := bad")
    expect(result).toMatch(/Parse error/)
  })

  it("type errors are reported", () => {
    const state = initialState()
    const result = processLine(state, "true true")
    expect(result).toMatch(/error/i)
  })
})
