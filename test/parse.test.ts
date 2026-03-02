import { describe, it, expect } from "vitest"
import {
  tokenize, parseExpr, parseLine, printExpr, ParseError,
  svar, sapp, slam, spi, stype,
  type SDecl, type SExpr,
} from "../src/parse.js"

function isDecl(x: SDecl | SExpr): x is SDecl {
  return "name" in x && "value" in x
}

describe("tokenizer", () => {
  it("tokenizes basic expression", () => {
    const tokens = tokenize("let id : Type := x")
    expect(tokens.map(t => t.tag)).toEqual([
      "kw_let", "ident", "colon", "kw_type", "coloneq", "ident", "eof"
    ])
  })

  it("tokenizes arrows and braces", () => {
    const tokens = tokenize("{x} -> x")
    expect(tokens.map(t => t.tag)).toEqual(["lbrace", "ident", "rbrace", "arrow", "ident", "eof"])
  })

  it("skips line comments", () => {
    const tokens = tokenize("x -- this is a comment\ny")
    expect(tokens.filter(t => t.tag === "ident").length).toBe(2)
  })

  it("throws on unexpected character", () => {
    expect(() => tokenize("@")).toThrow(ParseError)
  })
})

describe("parser - atoms and application", () => {
  it("parses identifier", () => {
    expect(parseExpr("x")).toEqual(svar("x"))
  })

  it("parses Type", () => {
    expect(parseExpr("Type")).toEqual(stype)
  })

  it("parses application (left-associative)", () => {
    expect(parseExpr("f x y")).toEqual(sapp(sapp(svar("f"), svar("x")), svar("y")))
  })

  it("parses parenthesized expression", () => {
    expect(parseExpr("f (g x)")).toEqual(sapp(svar("f"), sapp(svar("g"), svar("x"))))
  })
})

describe("parser - lambda", () => {
  it("parses single-param lambda", () => {
    expect(parseExpr("{x} -> x")).toEqual(slam(["x"], svar("x")))
  })

  it("parses multi-param lambda", () => {
    expect(parseExpr("{x y} -> x")).toEqual(slam(["x", "y"], svar("x")))
  })

  it("parses nested lambda body", () => {
    expect(parseExpr("{x} -> {y} -> x")).toEqual(
      slam(["x"], slam(["y"], svar("x")))
    )
  })
})

describe("parser - Pi types", () => {
  it("parses dependent Pi", () => {
    expect(parseExpr("(A : Type) -> A")).toEqual(spi("A", stype, svar("A")))
  })

  it("parses non-dependent function type", () => {
    expect(parseExpr("A -> B")).toEqual(spi("_", svar("A"), svar("B")))
  })

  it("parses right-associative arrows", () => {
    expect(parseExpr("A -> B -> C")).toEqual(
      spi("_", svar("A"), spi("_", svar("B"), svar("C")))
    )
  })

  it("parses mixed Pi and arrow", () => {
    expect(parseExpr("(A : Type) -> A -> A")).toEqual(
      spi("A", stype, spi("_", svar("A"), svar("A")))
    )
  })
})

describe("parser - declarations", () => {
  it("parses typed declaration", () => {
    const result = parseLine("let id : (A : Type) -> A -> A := {A x} -> x")
    expect(isDecl(result)).toBe(true)
    if (isDecl(result)) {
      expect(result.name).toBe("id")
      expect(result.type).toEqual(spi("A", stype, spi("_", svar("A"), svar("A"))))
      expect(result.value).toEqual(slam(["A", "x"], svar("x")))
    }
  })

  it("parses untyped declaration", () => {
    const result = parseLine("let x := Type")
    expect(isDecl(result)).toBe(true)
    if (isDecl(result)) {
      expect(result.name).toBe("x")
      expect(result.type).toBeNull()
      expect(result.value).toEqual(stype)
    }
  })
})

describe("pretty printer", () => {
  it("prints identifier", () => {
    expect(printExpr(svar("x"))).toBe("x")
  })

  it("prints Type", () => {
    expect(printExpr(stype)).toBe("Type")
  })

  it("prints application", () => {
    expect(printExpr(sapp(svar("f"), svar("x")))).toBe("f x")
  })

  it("prints lambda", () => {
    expect(printExpr(slam(["x", "y"], svar("x")))).toBe("{x y} -> x")
  })

  it("prints dependent Pi", () => {
    expect(printExpr(spi("A", stype, svar("A")))).toBe("(A : Type) -> A")
  })

  it("prints non-dependent function type", () => {
    expect(printExpr(spi("_", svar("A"), svar("B")))).toBe("A -> B")
  })
})

describe("round-trip", () => {
  const cases = [
    "x",
    "Type",
    "f x",
    "f x y",
    "{x} -> x",
    "{x y} -> x",
    "(A : Type) -> A",
    "A -> B -> C",
    "(A : Type) -> A -> A",
  ]

  for (const input of cases) {
    it(`round-trips: ${input}`, () => {
      const ast = parseExpr(input)
      const output = printExpr(ast)
      const ast2 = parseExpr(output)
      expect(ast2).toEqual(ast)
    })
  }
})
