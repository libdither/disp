import { describe, it, expect } from "vitest"
import {
  tokenize, parseExpr, parseLine, printExpr, ParseError,
  recognizeChurchLiteral,
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

describe("tokenizer - numbers and booleans", () => {
  it("tokenizes number literal", () => {
    const tokens = tokenize("42")
    expect(tokens[0]).toEqual({ tag: "num", value: 42 })
  })

  it("tokenizes true/false keywords", () => {
    const tokens = tokenize("true false")
    expect(tokens[0].tag).toBe("kw_true")
    expect(tokens[1].tag).toBe("kw_false")
  })

  it("tokenizes mixed", () => {
    const tokens = tokenize("not true 3")
    expect(tokens.map(t => t.tag)).toEqual(["ident", "kw_true", "num", "eof"])
  })
})

describe("parser - literals", () => {
  it("parses true as Church boolean", () => {
    expect(parseExpr("true")).toEqual(slam(["R", "t", "f"], svar("t")))
  })

  it("parses false as Church boolean", () => {
    expect(parseExpr("false")).toEqual(slam(["R", "t", "f"], svar("f")))
  })

  it("parses 0 as Church zero", () => {
    expect(parseExpr("0")).toEqual(slam(["R", "s", "z"], svar("z")))
  })

  it("parses 3 as Church numeral", () => {
    const expected = slam(["R", "s", "z"],
      sapp(svar("s"), sapp(svar("s"), sapp(svar("s"), svar("z"))))
    )
    expect(parseExpr("3")).toEqual(expected)
  })

  it("parses literals in application", () => {
    expect(parseExpr("not true")).toEqual(
      sapp(svar("not"), slam(["R", "t", "f"], svar("t")))
    )
  })
})

describe("recognizeChurchLiteral", () => {
  it("recognizes true", () => {
    expect(recognizeChurchLiteral(slam(["R", "t", "f"], svar("t")))).toBe("true")
  })

  it("recognizes false", () => {
    expect(recognizeChurchLiteral(slam(["R", "t", "f"], svar("f")))).toBe("false")
  })

  it("recognizes 0 as false (structurally identical)", () => {
    expect(recognizeChurchLiteral(slam(["R", "s", "z"], svar("z")))).toBe("false")
  })

  it("recognizes 3", () => {
    const three = slam(["R", "s", "z"],
      sapp(svar("s"), sapp(svar("s"), sapp(svar("s"), svar("z"))))
    )
    expect(recognizeChurchLiteral(three)).toBe("3")
  })

  it("returns null for non-literal", () => {
    expect(recognizeChurchLiteral(svar("x"))).toBeNull()
  })
})

describe("parser - recursive declarations", () => {
  it("parses let rec with isRec true", () => {
    const result = parseLine("let rec f : (A : Type) -> A -> A := {A x} -> x")
    expect(isDecl(result)).toBe(true)
    if (isDecl(result)) {
      expect(result.name).toBe("f")
      expect(result.isRec).toBe(true)
      expect(result.type).not.toBeNull()
    }
  })

  it("parses let (non-rec) with isRec false", () => {
    const result = parseLine("let f := Type")
    expect(isDecl(result)).toBe(true)
    if (isDecl(result)) {
      expect(result.isRec).toBe(false)
    }
  })

  it("rec is usable as a variable name", () => {
    const result = parseLine("let x := rec")
    expect(isDecl(result)).toBe(true)
    if (isDecl(result)) {
      expect(result.name).toBe("x")
      expect(result.isRec).toBe(false)
      expect(result.value).toEqual(svar("rec"))
    }
  })
})

describe("parser - record types", () => {
  it("parses {x : A, y : B} as Pi type", () => {
    const result = parseExpr("{x : A, y : B}")
    // (R$rec : Type) -> ((x : A) -> (y : B) -> R$rec) -> R$rec
    const expected = spi("R$rec", stype,
      spi("_",
        spi("x", svar("A"), spi("y", svar("B"), svar("R$rec"))),
        svar("R$rec")))
    expect(result).toEqual(expected)
  })

  it("parses {x := a, y := b} as lambda", () => {
    const result = parseExpr("{x := a, y := b}")
    // {R$rec f$rec} -> f$rec a b
    const expected = slam(["R$rec", "f$rec"],
      sapp(sapp(svar("f$rec"), svar("a")), svar("b")))
    expect(result).toEqual(expected)
  })

  it("{x} -> x still works as lambda", () => {
    expect(parseExpr("{x} -> x")).toEqual(slam(["x"], svar("x")))
  })

  it("f {x := a} works as application", () => {
    const result = parseExpr("f {x := a}")
    const recordVal = slam(["R$rec", "f$rec"], sapp(svar("f$rec"), svar("a")))
    expect(result).toEqual(sapp(svar("f"), recordVal))
  })

  it("single-field record type", () => {
    const result = parseExpr("{x : A}")
    const expected = spi("R$rec", stype,
      spi("_", spi("x", svar("A"), svar("R$rec")), svar("R$rec")))
    expect(result).toEqual(expected)
  })
})

describe("parser - coproduct types", () => {
  it("parses <Left : A | Right : B> as Pi type", () => {
    const result = parseExpr("<Left : A | Right : B>")
    // (R$cop : Type) -> (A -> R$cop) -> (B -> R$cop) -> R$cop
    const expected = spi("R$cop", stype,
      spi("_", spi("_", svar("A"), svar("R$cop")),
        spi("_", spi("_", svar("B"), svar("R$cop")),
          svar("R$cop"))))
    expect(result).toEqual(expected)
  })

  it("parses three-variant coproduct", () => {
    const result = parseExpr("<A : X | B : Y | C : Z>")
    const expected = spi("R$cop", stype,
      spi("_", spi("_", svar("X"), svar("R$cop")),
        spi("_", spi("_", svar("Y"), svar("R$cop")),
          spi("_", spi("_", svar("Z"), svar("R$cop")),
            svar("R$cop")))))
    expect(result).toEqual(expected)
  })

  it("coproduct type as atom in application", () => {
    const result = parseExpr("f <Left : A | Right : B>")
    expect(result.tag).toBe("sapp")
  })
})

describe("pretty printer - literals", () => {
  it("prints true keyword", () => {
    expect(printExpr(parseExpr("true"))).toBe("true")
  })

  it("prints false keyword", () => {
    expect(printExpr(parseExpr("false"))).toBe("false")
  })

  it("prints number literal", () => {
    expect(printExpr(parseExpr("3"))).toBe("3")
  })

  it("round-trips true", () => {
    const ast = parseExpr("true")
    const ast2 = parseExpr(printExpr(ast))
    expect(ast2).toEqual(ast)
  })

  it("round-trips 5", () => {
    const ast = parseExpr("5")
    const ast2 = parseExpr(printExpr(ast))
    expect(ast2).toEqual(ast)
  })
})
