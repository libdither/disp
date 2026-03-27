import { describe, it, expect, beforeEach } from "vitest"
import { LEAF, stem, fork, apply, I, BudgetExhausted } from "../src/tree.js"
import { tokenize, parseExpr, parseLine, ParseError } from "../src/parse.js"
import {
  buildWrapped, whnfTree, convertible, cocCheckDecl,
  CocError, resetMarkerCounter, clearNativeBuiltins,
  encType, encApp, encVar, type Env,
} from "../src/coc.js"
import { initialState, processLine } from "../src/repl.js"
import { loadCocPrelude, clearPreludeCache } from "../src/prelude.js"

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

describe("CocError", () => {
  it("throws on unbound variable", () => {
    expect(() => buildWrapped({ tag: "svar", name: "nonexistent" }, new Map())).toThrow(CocError)
  })

  it("throws on lambda without expected type", () => {
    const { cocEnv } = loadCocPrelude()
    expect(() => buildWrapped(
      { tag: "slam", params: ["x"], body: { tag: "svar", name: "x" } },
      cocEnv,
    )).toThrow(CocError)
  })

  it("throws on type mismatch in application", () => {
    const state = initialState()
    // true true — true is Bool, not a function
    const result = processLine(state, "true true")
    expect(result).toMatch(/error/i)
  })

  it("throws on non-type domain in Pi", () => {
    const state = initialState()
    // (x : true) -> x — true is not a type
    const result = processLine(state, "let bad : (x : true) -> Bool := {x} -> x")
    expect(result).toMatch(/error/i)
  })

  it("recursive def requires type annotation", () => {
    const env: Env = new Map()
    expect(() => cocCheckDecl(env, "f", null, { tag: "svar", name: "f" }, true))
      .toThrow()
  })
})

describe("BudgetExhausted", () => {
  it("thrown when apply budget runs out", () => {
    // SII x = x x (diverges when applied to itself)
    const SII = fork(stem(I), I)
    expect(() => apply(SII, SII, { remaining: 100 })).toThrow(BudgetExhausted)
  })

  it("WHNF budget exhaustion throws CocError", () => {
    // Build a deeply nested application that would exhaust WHNF budget
    let term = encType()
    for (let i = 0; i < 20; i++) {
      term = encApp(term, encType())
    }
    expect(() => whnfTree(term, { remaining: 3 })).toThrow(CocError)
  })

  it("processLine handles budget exhaustion gracefully", () => {
    const state = initialState()
    processLine(state, "let rec diverge : Nat -> Nat := {n} -> diverge n")
    // This should not crash — just report an error
    const result = processLine(state, "diverge 0")
    // Either evaluates to an error or shows a result
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
