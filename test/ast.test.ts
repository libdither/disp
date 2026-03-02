import { describe, it, expect } from "vitest"
import { parseExpr } from "../src/parse.js"
import {
  convertToBind, subst, whnf, convertible,
  VAR, SORT, app, lam, pi,
  NONE, END, branch,
  type AST, type Bind,
} from "../src/ast.js"

// Helper: parse and convert to bind-tree AST
function pc(input: string): AST {
  return convertToBind(parseExpr(input))
}

describe("convertToBind", () => {
  it("converts identity: {x} -> x", () => {
    const result = pc("{x} -> x")
    // λx. x = Lam(End, Var)
    expect(result).toEqual(lam(END, VAR))
  })

  it("converts const: {x y} -> x", () => {
    const result = pc("{x y} -> x")
    // λx. λy. x = Lam(End, Lam(None, Var))
    expect(result).toEqual(lam(END, lam(NONE, VAR)))
  })

  it("converts second projection: {x y} -> y", () => {
    const result = pc("{x y} -> y")
    // λx. λy. y = Lam(None, Lam(End, Var))
    expect(result).toEqual(lam(NONE, lam(END, VAR)))
  })

  it("converts application in body: {x y} -> x y", () => {
    const result = pc("{x y} -> x y")
    // λx. λy. x y = Lam(Branch(End,None), Lam(Branch(None,End), App(Var,Var)))
    expect(result).toEqual(
      lam(branch(END, NONE), lam(branch(NONE, END), app(VAR, VAR)))
    )
  })

  it("converts Type", () => {
    expect(pc("Type")).toEqual(SORT)
  })

  it("converts simple application", () => {
    expect(pc("f x")).toEqual(app(VAR, VAR))
  })

  it("converts Pi type: (A : Type) -> A", () => {
    const result = pc("(A : Type) -> A")
    // Pi(Type, End, Var)
    expect(result).toEqual(pi(SORT, END, VAR))
  })

  it("converts non-dependent function type: A -> B", () => {
    const result = pc("A -> B")
    // Pi(Var, None, Var) — the _ binder is None
    expect(result).toEqual(pi(VAR, NONE, VAR))
  })

  it("converts identity type: (A : Type) -> A -> A", () => {
    const result = pc("(A : Type) -> A -> A")
    // Pi(Type, End, Pi(Var, None, Var))
    // A appears in the codomain: the first A in "A -> A" is a Var (bound by outer Pi)
    // The inner Pi "A -> A" is non-dependent: Pi(Var, None, Var)
    expect(result).toEqual(pi(SORT, END, pi(VAR, NONE, VAR)))
  })
})

describe("substitution", () => {
  it("None: body unchanged", () => {
    const body = app(VAR, SORT)
    const result = subst(NONE, body, SORT)
    expect(result).toEqual(body)
  })

  it("End at Var: substitute", () => {
    const val = SORT
    const result = subst(END, VAR, val)
    expect(result).toEqual(SORT)
  })

  it("Branch at App: recurse both sides", () => {
    // subst(Branch(End, None), App(Var, Var), val) → App(val, Var)
    const bind = branch(END, NONE)
    const body = app(VAR, VAR)
    const val = SORT
    const result = subst(bind, body, val)
    expect(result).toEqual(app(SORT, VAR))
  })

  it("passes through lambda", () => {
    // subst(End, Lam(innerBind, Var), val) → Lam(innerBind, val)
    const result = subst(END, lam(NONE, VAR), SORT)
    expect(result).toEqual(lam(NONE, SORT))
  })

  it("passes through pi", () => {
    // subst(End, Pi(Var, innerBind, body), val) → Pi(val, innerBind, body)
    const result = subst(END, pi(VAR, NONE, SORT), SORT)
    expect(result).toEqual(pi(SORT, NONE, SORT))
  })
})

describe("WHNF", () => {
  it("identity application: ({x} -> x) y → y", () => {
    // (λ.Var) applied to some argument
    const id = lam(END, VAR)
    const y = SORT
    const term = app(id, y)
    const result = whnf(term)
    expect(result).toEqual(SORT)
  })

  it("const application: ({x y} -> x) a b → a", () => {
    const konst = lam(END, lam(NONE, VAR))
    const a = SORT
    const b = lam(NONE, VAR) // some other term
    const term = app(app(konst, a), b)
    const result = whnf(term)
    expect(result).toEqual(SORT)
  })

  it("second projection: ({x y} -> y) a b → b", () => {
    const snd = lam(NONE, lam(END, VAR))
    const a = SORT
    const b = lam(NONE, VAR)
    const term = app(app(snd, a), b)
    const result = whnf(term)
    expect(result).toEqual(b)
  })

  it("stuck application stays as app", () => {
    // free variable applied to something
    const term = app(VAR, SORT)
    const result = whnf(term)
    expect(result).toEqual(app(VAR, SORT))
  })

  it("lambda is already WHNF", () => {
    const term = lam(END, VAR)
    expect(whnf(term)).toEqual(term)
  })

  it("sort is already WHNF", () => {
    expect(whnf(SORT)).toEqual(SORT)
  })

  it("nested beta reduction", () => {
    // ({f x} -> f x) ({y} -> y) z → ({y} -> y) z → z
    // But z is free (VAR), so result is VAR
    const apply_fn = lam(branch(END, NONE), lam(branch(NONE, END), app(VAR, VAR)))
    const id = lam(END, VAR)
    const z = SORT
    const term = app(app(apply_fn, id), z)
    const result = whnf(term)
    expect(result).toEqual(SORT)
  })
})

describe("convertibility", () => {
  it("same terms are convertible", () => {
    expect(convertible(SORT, SORT)).toBe(true)
    expect(convertible(VAR, VAR)).toBe(true)
  })

  it("beta-equal terms are convertible", () => {
    // (λx.x) Type  ≡  Type
    const term1 = app(lam(END, VAR), SORT)
    const term2 = SORT
    expect(convertible(term1, term2)).toBe(true)
  })

  it("different terms are not convertible", () => {
    expect(convertible(SORT, VAR)).toBe(false)
    expect(convertible(lam(END, VAR), lam(NONE, SORT))).toBe(false)
  })
})
