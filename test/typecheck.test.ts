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
  it("checks Church boolean definitions", () => {
    const ctx = declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let true : Bool := {R t f} -> t",
      "let false : Bool := {R t f} -> f",
    ])
    // true and false should have type Bool
    expect(lookupType(ctx, "true")).not.toBeNull()
    expect(lookupType(ctx, "false")).not.toBeNull()
  })

  it("checks not function", () => {
    expect(() => declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let true : Bool := {R t f} -> t",
      "let false : Bool := {R t f} -> f",
      "let not : Bool -> Bool := {b R t f} -> b R f t",
    ])).not.toThrow()
  })

  it("checks and function", () => {
    expect(() => declCtx([
      "let Bool : Type := (R : Type) -> R -> R -> R",
      "let true : Bool := {R t f} -> t",
      "let false : Bool := {R t f} -> f",
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

// Helper to look up type in context
function lookupType(ctx: Context, name: string): SExpr | null {
  for (let i = ctx.length - 1; i >= 0; i--) {
    if (ctx[i].name === name) return ctx[i].type
  }
  return null
}
