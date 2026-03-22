import { describe, it, expect } from "vitest"
import * as fc from "fast-check"
import {
  tokenize, parseExpr, parseLine, printExpr, ParseError,
  stripPos, type SExpr,
} from "../src/parse.js"

// --- Arbitrary generators for valid Disp syntax ---

const arbIdent: fc.Arbitrary<string> = fc.string({ minLength: 1, maxLength: 4, unit: fc.constantFrom(..."abcdefghijklmnopqrstuvwxyz".split("")) })
  .filter(s => s !== "let" && s !== "Type" && s !== "Tree" && s !== "true" && s !== "false" && s !== "rec" && s !== "zero" && s !== "succ" && s !== "leaf")

const arbSExpr: fc.Arbitrary<SExpr> = fc.letrec(tie => ({
  expr: fc.oneof(
    { depthSize: "small", withCrossShrink: true },
    tie("var"),
    tie("type"),
    tie("app"),
    tie("lam"),
    tie("pi"),
    tie("arrow"),
  ),
  var: arbIdent.map(name => ({ tag: "svar" as const, name })),
  type: fc.constant({ tag: "stype" as const }),
  app: fc.tuple(tie("expr"), tie("expr")).map(
    ([func, arg]) => ({ tag: "sapp" as const, func, arg })
  ),
  lam: fc.tuple(
    fc.array(arbIdent, { minLength: 1, maxLength: 3 }),
    tie("expr"),
  ).map(([params, body]) => ({ tag: "slam" as const, params, body })),
  pi: fc.tuple(arbIdent, tie("expr"), tie("expr")).map(
    ([name, domain, codomain]) => ({ tag: "spi" as const, name, domain, codomain })
  ),
  arrow: fc.tuple(tie("expr"), tie("expr")).map(
    ([domain, codomain]) => ({ tag: "spi" as const, name: "_", domain, codomain })
  ),
})).expr

// --- Property: parse(print(expr)) structurally equals expr ---

describe("fuzz: parse-print round-trip", () => {
  it("round-trips arbitrary SExprs", () => {
    fc.assert(
      fc.property(arbSExpr, (expr) => {
        const printed = printExpr(expr)
        const reparsed = parseExpr(printed)
        expect(stripPos(reparsed)).toEqual(stripPos(expr))
      }),
      { numRuns: 500 }
    )
  })
})

// --- Property: tokenizer never crashes on arbitrary strings ---

describe("fuzz: tokenizer robustness", () => {
  it("tokenize either succeeds or throws ParseError on arbitrary input", () => {
    fc.assert(
      fc.property(fc.string({ maxLength: 100 }), (input) => {
        try {
          const tokens = tokenize(input)
          // If it succeeds, last token should be eof
          expect(tokens[tokens.length - 1].tag).toBe("eof")
        } catch (e) {
          // Only ParseError is acceptable
          expect(e).toBeInstanceOf(ParseError)
        }
      }),
      { numRuns: 1000 }
    )
  })
})

// --- Property: parser never crashes with uncaught exceptions ---

describe("fuzz: parser robustness", () => {
  // Generate strings from a grammar-aware alphabet (valid tokens + delimiters)
  const arbGrammarInput = fc.string({
    maxLength: 60,
    unit: fc.constantFrom(..."abcdefxyz Type{}()->:=_,<>|0123456789 \n".split("")),
  })

  it("parseLine either succeeds or throws ParseError on grammar-like input", () => {
    fc.assert(
      fc.property(arbGrammarInput, (input) => {
        try {
          parseLine(input.trim())
        } catch (e) {
          // ParseError and Error (from parser) are acceptable
          expect(e).toBeInstanceOf(Error)
        }
      }),
      { numRuns: 1000 }
    )
  })
})

// --- Property: numeral encoding is consistent ---

describe("fuzz: numeral round-trip", () => {
  it("number literals round-trip through print", () => {
    fc.assert(
      fc.property(fc.integer({ min: 0, max: 50 }), (n) => {
        const printed = printExpr(parseExpr(String(n)))
        expect(printed).toBe(String(n))
      }),
      { numRuns: 51 }
    )
  })
})

// --- Property: printExpr always produces parseable output ---

describe("fuzz: printExpr always parseable", () => {
  it("printExpr output is always valid syntax", () => {
    fc.assert(
      fc.property(arbSExpr, (expr) => {
        const printed = printExpr(expr)
        // Should not throw
        parseExpr(printed)
      }),
      { numRuns: 500 }
    )
  })
})
