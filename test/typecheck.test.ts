import { describe, it, expect } from "vitest"
import { parseExpr, parseLine, type SDecl, type SExpr, svar, sapp, slam, spi, stype } from "../src/parse.js"
import {
  infer, check, checkDecl, TypeError,
  type Context, whnfSExpr, convertibleSExpr, substNamed,
} from "../src/typecheck.js"

// Helpers
function inferStr(input: string, ctx: Context = []): SExpr {
  return infer(ctx, parseExpr(input))
}

function checkStr(input: string, typeStr: string, ctx: Context = []): void {
  check(ctx, parseExpr(input), parseExpr(typeStr))
}

function declCtx(decls: string[]): Context {
  let ctx: Context = []
  for (const d of decls) {
    const parsed = parseLine(d)
    if (!("name" in parsed)) throw new Error("Expected declaration")
    const decl = parsed as SDecl
    const result = checkDecl(ctx, decl.name, decl.type, decl.value)
    ctx = result.ctx
  }
  return ctx
}

describe("substNamed", () => {
  it("substitutes variable", () => {
    const result = substNamed("x", svar("x"), stype)
    expect(result).toEqual(stype)
  })

  it("doesn't substitute different variable", () => {
    const result = substNamed("x", svar("y"), stype)
    expect(result).toEqual(svar("y"))
  })

  it("respects shadowing in lambda", () => {
    const result = substNamed("x", slam(["x"], svar("x")), stype)
    expect(result.tag).toBe("slam")
  })
})

describe("whnfSExpr", () => {
  it("beta reduces application", () => {
    const term = sapp(slam(["x"], svar("x")), stype)
    const result = whnfSExpr(term)
    expect(result).toEqual(stype)
  })

  it("multi-param lambda applied once", () => {
    // ({x y} -> x) Type = {y} -> Type
    const term = sapp(slam(["x", "y"], svar("x")), stype)
    const result = whnfSExpr(term)
    expect(result.tag).toBe("slam")
  })

  it("stuck application stays", () => {
    const term = sapp(svar("f"), stype)
    const result = whnfSExpr(term)
    expect(result.tag).toBe("sapp")
  })
})

describe("type checker - basic", () => {
  it("Type : Type", () => {
    const ty = inferStr("Type")
    expect(ty.tag).toBe("stype")
  })

  it("variable lookup", () => {
    const ctx: Context = [{ name: "x", type: stype }]
    const ty = infer(ctx, svar("x"))
    expect(ty.tag).toBe("stype")
  })

  it("unbound variable throws", () => {
    expect(() => inferStr("x")).toThrow(TypeError)
  })

  it("Pi type formation", () => {
    const ty = inferStr("(A : Type) -> Type")
    expect(ty.tag).toBe("stype")
  })

  it("non-dependent function type", () => {
    const ctx: Context = [{ name: "A", type: stype }]
    const ty = infer(ctx, parseExpr("A -> A"))
    expect(ty.tag).toBe("stype")
  })
})

describe("type checker - lambda and application", () => {
  it("checks identity function", () => {
    const idType = parseExpr("(A : Type) -> A -> A")
    const idVal = parseExpr("{A x} -> x")
    expect(() => check([], idVal, idType)).not.toThrow()
  })

  it("checks const function", () => {
    const constType = parseExpr("(A : Type) -> (B : Type) -> A -> B -> A")
    const constVal = parseExpr("{A B x y} -> x")
    expect(() => check([], constVal, constType)).not.toThrow()
  })

  it("infers application type", () => {
    const ctx = declCtx([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    // id Type : Type -> Type
    const ty = infer(ctx, parseExpr("id Type"))
    // Should be convertible to Type -> Type
    expect(convertibleSExpr(ty, parseExpr("Type -> Type"))).toBe(true)
  })

  it("infers nested application", () => {
    const ctx = declCtx([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const ty = infer(ctx, parseExpr("id Type Type"))
    expect(convertibleSExpr(ty, stype, ctx)).toBe(true)
  })
})

describe("type checker - Church booleans", () => {
  it("checks Church boolean type for true/false keywords", () => {
    const ctx = declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
    ])
    // true and false keywords should type-check as Bool
    expect(() => check(ctx, parseExpr("true"), parseExpr("Bool"))).not.toThrow()
    expect(() => check(ctx, parseExpr("false"), parseExpr("Bool"))).not.toThrow()
  })

  it("checks not function", () => {
    expect(() => declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
    ])).not.toThrow()
  })

  it("checks and function", () => {
    expect(() => declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let and : Bool -> Bool -> Bool := {a b R t f} -> a R (b R t f) f",
    ])).not.toThrow()
  })
})

describe("type checker - Church naturals", () => {
  it("checks Church nat definitions", () => {
    expect(() => declCtx([
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let zero : Nat := {R s z} -> z",
      "let succ : Nat -> Nat := {n R s z} -> s (n R s z)",
    ])).not.toThrow()
  })

  it("checks add function", () => {
    expect(() => declCtx([
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let zero : Nat := {R s z} -> z",
      "let succ : Nat -> Nat := {n R s z} -> s (n R s z)",
      "let add : Nat -> Nat -> Nat := {m n R s z} -> m R s (n R s z)",
    ])).not.toThrow()
  })
})

describe("type checker - errors", () => {
  it("rejects type mismatch in application", () => {
    const ctx = declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
      "let id : (A : Type) -> A -> A := {A x} -> x",
      "let zero : Nat := {R s z} -> z",
    ])
    // id Bool zero should fail (zero : Nat, not Bool)
    expect(() => infer(ctx, parseExpr("id Bool zero"))).toThrow(TypeError)
  })

  it("rejects applying non-function", () => {
    expect(() => infer(
      [{ name: "x", type: stype }],
      parseExpr("Type Type")
    )).toThrow(TypeError)
  })

  it("rejects lambda without function type annotation", () => {
    expect(() => infer([], parseExpr("{x} -> x"))).toThrow(TypeError)
  })
})

describe("type checker - conversion", () => {
  it("accepts beta-convertible types", () => {
    const ctx = declCtx([
      "let id : (A : Type) -> A -> A := {A x} -> x",
    ])
    const termType = infer(ctx, parseExpr("id Type Type"))
    expect(convertibleSExpr(termType, stype, ctx)).toBe(true)
  })
})

describe("type checker - literal keywords", () => {
  it("true has Church boolean type", () => {
    const boolType = parseExpr("(R : Type) -> R -> R -> R")
    expect(() => check([], parseExpr("true"), boolType)).not.toThrow()
  })

  it("false has Church boolean type", () => {
    const boolType = parseExpr("(R : Type) -> R -> R -> R")
    expect(() => check([], parseExpr("false"), boolType)).not.toThrow()
  })

  it("0 has Church nat type", () => {
    const natType = parseExpr("(R : Type) -> (R -> R) -> R -> R")
    expect(() => check([], parseExpr("0"), natType)).not.toThrow()
  })

  it("3 has Church nat type", () => {
    const natType = parseExpr("(R : Type) -> (R -> R) -> R -> R")
    expect(() => check([], parseExpr("3"), natType)).not.toThrow()
  })
})

describe("type checker - records", () => {
  it("record value checks against record type", () => {
    const ctx = declCtx([
      "let A : Type := (R : Type) -> R -> R -> R",
      "let B : Type := (R : Type) -> R -> R -> R",
    ])
    const recordType = parseExpr("{x : A, y : B}")
    const recordVal = parseExpr("{x := true, y := false}")
    expect(() => check(ctx, recordVal, recordType)).not.toThrow()
  })

  it("dependent record {fst : Type, snd : fst}", () => {
    const recordType = parseExpr("{fst : Type, snd : fst}")
    // This should be a valid type
    const typeOfType = infer([], recordType)
    expect(typeOfType.tag).toBe("stype")
  })
})

describe("type checker - coproducts", () => {
  it("constructor checks against coproduct type", () => {
    const ctx = declCtx([
      "let A : Type := (R : Type) -> R -> R -> R",
      "let B : Type := (R : Type) -> R -> R -> R",
    ])
    const copType = parseExpr("<Left : A | Right : B>")
    // inl : A -> CopType = {a R l r} -> l a
    const inl = parseExpr("{a R l r} -> l a")
    const inlType = parseExpr("A -> <Left : A | Right : B>")
    expect(() => check(ctx, inl, inlType)).not.toThrow()
  })
})

describe("type checker - recursive definitions", () => {
  it("recursive def without type annotation throws", () => {
    expect(() => checkDecl([], "f", null, parseExpr("{x} -> x"), true))
      .toThrow(TypeError)
  })

  it("recursive body can reference own name", () => {
    // let rec myId : Nat -> Nat := {n} -> n
    const ctx = declCtx([
      "let Nat : Type := (R : Type) -> (R -> R) -> R -> R",
    ])
    const type = parseExpr("Nat -> Nat")
    const body = parseExpr("{n} -> n")
    expect(() => checkDecl(ctx, "myId", type, body, true)).not.toThrow()
  })

  it("recursive def type-checks with self-reference in body", () => {
    // let rec f : (A : Type) -> A -> A := {A x} -> f A x
    // The body references f, which should be available in context during checking
    const type = parseExpr("(A : Type) -> A -> A")
    const body = parseExpr("{A x} -> f A x")
    expect(() => checkDecl([], "f", type, body, true)).not.toThrow()
  })
})

// Helper to look up type in context
function lookupType(ctx: Context, name: string): SExpr | null {
  for (let i = ctx.length - 1; i >= 0; i--) {
    if (ctx[i].name === name) return ctx[i].type
  }
  return null
}
