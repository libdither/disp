import { describe, it, expect } from "vitest"
import * as fs from "node:fs"
import * as path from "node:path"
import { LEAF, stem, FAST_EQ, treeEqual, fork, apply, isFork, type Tree } from "../src/tree.js"
import {
  Env,
  compileType,
  declare,
  entry,
  loadFile,
  typecheckDeclSource,
  typecheckExprSource,
  type ExprCheckResult,
  type DeclCheckResult,
} from "../src/elaborate.js"
import { parseExpr, parseLine } from "../src/parse.js"

function seedEnv(): Env {
  return new Map([
    ["leaf", entry(LEAF)],
    ["fastEq", entry(FAST_EQ)],
    ["true", entry(LEAF)],
    ["false", entry(stem(LEAF))],
    ["zero", entry(LEAF)],
    ["succ", entry(LEAF)],
  ])
}

function baseEnv(): Env {
  const typesPath = path.join(import.meta.dirname, "..", "types.disp")
  const source = fs.readFileSync(typesPath, "utf-8")
  return loadFile(source, seedEnv())
}

function boolFamilyEnv(): Env {
  const env = baseEnv()
  env.set("TreeToBool", entry(compileType(parseExpr("Tree -> Bool"), env)))
  return declare(parseLine("let F := {b} -> triage Bool ({_} -> TreeToBool) ({_ _} -> Tree) b") as any, env)
}

function vecEnv(): Env {
  const typesPath = path.join(import.meta.dirname, "..", "types.disp")
  const source = fs.readFileSync(typesPath, "utf-8")
  const extra = `
let rec Vec := {n v} -> triage (isLeaf v) ({n'} -> triage ff ({_} -> ff) ({h t} -> Vec n' t) v) ({_ _} -> ff) n
`
  return loadFile(source + extra, seedEnv())
}

function specializePi(env: Env, A: Tree, B: Tree) {
  const pc = env.get("piCheck")!.tree
  const budget = { remaining: 500_000 }
  return apply(apply(pc, A, budget), B, budget)
}

function expectExprOk(result: ExprCheckResult): asserts result is Extract<ExprCheckResult, { ok: true }> {
  expect(result.ok).toBe(true)
}

function expectDeclOk(result: DeclCheckResult): asserts result is Extract<DeclCheckResult, { ok: true }> {
  expect(result.ok).toBe(true)
}

function expectExprStage(
  result: ExprCheckResult,
  stage: "parse" | "elaborate" | "check",
  message?: string,
): void {
  expect(result.ok).toBe(false)
  if (result.ok) throw new Error("expected failure")
  expect(result.stage).toBe(stage)
  if (message !== undefined) expect(result.message).toContain(message)
}

function expectDeclStage(
  result: DeclCheckResult,
  stage: "parse" | "elaborate" | "check",
  message?: string,
): void {
  expect(result.ok).toBe(false)
  if (result.ok) throw new Error("expected failure")
  expect(result.stage).toBe(stage)
  if (message !== undefined) expect(result.message).toContain(message)
}

describe("typecheck end-to-end", () => {
  describe("simple typed programs", () => {
    it("accepts a valid typed lambda expression", () => {
      const result = typecheckExprSource("{x} -> not (not x)", "Bool -> Bool", baseEnv())
      expectExprOk(result)
    })

    it("accepts a composed expression using a checked declaration from the env", () => {
      const decl = typecheckDeclSource("let myNot : Bool -> Bool := {x} -> not x", baseEnv())
      expectDeclOk(decl)

      const expr = typecheckExprSource("{x} -> myNot (myNot x)", "Bool -> Bool", decl.env)
      expectExprOk(expr)
    })

    it("stores typed function annotations as specialized predicates that check directly", () => {
      const decl = typecheckDeclSource("let myNot : Bool -> Bool := {x} -> not x", baseEnv())
      expectDeclOk(decl)

      const myNotEntry = decl.env.get("myNot")!
      // Stored type is a specialized predicate (triage-shaped, from piPred)
      const isTriage = isFork(myNotEntry.type) && isFork(myNotEntry.type.left)
      expect(isTriage).toBe(true)
      // The predicate checks annotated trees directly via apply
      const myNotTree = myNotEntry.tree
      expect(treeEqual(apply(myNotEntry.type, myNotTree, { remaining: 500_000 }), LEAF)).toBe(true)
    })

    it("rejects an invalid base-typed expression at check time", () => {
      const result = typecheckExprSource("leaf leaf leaf", "Bool", baseEnv())
      expectExprStage(result, "check", "does not satisfy expected type")
    })

    it("rejects an invalid typed declaration at check time", () => {
      const result = typecheckDeclSource("let bad : Bool := leaf leaf leaf", baseEnv())
      expectDeclStage(result, "check", "does not satisfy annotation")
    })

    it("returns parse errors cleanly", () => {
      const result = typecheckExprSource("{x -> x", "Bool -> Bool", baseEnv())
      expectExprStage(result, "parse")
    })
  })

  describe("target dependent core", () => {
    it("accepts lambdas against dependent expected types", () => {
      const result = typecheckExprSource("{x} -> x", "(n : Nat) -> Nat", baseEnv())
      expectExprOk(result)
    })

    it("accepts named Pi annotations whose codomain does not depend on the binder", () => {
      const result = typecheckDeclSource("let choose : (b : Bool) -> Bool := {b} -> b", baseEnv())
      expectDeclOk(result)
    })

    it("accepts polymorphic-style declarations once annotation compilation has local type binders", () => {
      const result = typecheckDeclSource("let depId : (A : Type) -> A -> A := {A x} -> x", baseEnv())
      expectDeclOk(result)
    })
  })

  describe("target soundness and env invariants", () => {
    it("re-uses typed non-function constants inside typed lambdas without misclassifying them as Pi-typed", () => {
      const decl = typecheckDeclSource("let t : Bool := true", baseEnv())
      expectDeclOk(decl)

      const result = typecheckExprSource("{x} -> t", "Bool -> Bool", decl.env)
      expectExprOk(result)
    })

    it("accepts self-referential typed recursive declarations after checking step : T -> T", () => {
      const result = typecheckDeclSource("let rec loop : Bool -> Bool := {x} -> loop x", baseEnv())
      expectDeclOk(result)
    })

    it("accepts non-self-referential let rec declarations", () => {
      const result = typecheckDeclSource("let rec alwaysTrue : Bool := true", baseEnv())
      expectDeclOk(result)
    })
  })

  describe("target dependent milestones", () => {
    it("accepts a Bool-indexed dependent family when both branches are satisfied", () => {
      const result = typecheckExprSource(
        "{b} -> triage ff ({_} -> {_} -> ff) ({_ _} -> leaf) b",
        "(b : Bool) -> F b",
        boolFamilyEnv(),
      )
      expectExprOk(result)
    })

    it("rejects a Bool-indexed dependent family when one branch violates the codomain", () => {
      const result = typecheckExprSource(
        "{b} -> ff",
        "(b : Bool) -> F b",
        boolFamilyEnv(),
      )
      expectExprStage(result, "check")
    })

    it("accepts Vec zero constructors end-to-end once Nat-indexed families are supported", () => {
      const result = typecheckDeclSource("let vnil : Vec zero := leaf", vecEnv())
      expectDeclOk(result)
    })

    it("rejects Vec constructors at the wrong index", () => {
      const result = typecheckDeclSource("let badVnil : Vec (succ zero) := leaf", vecEnv())
      expectDeclStage(result, "check")
    })

    it.todo("supports inline dependent family literals in expected types; this is currently not specifiable because type expressions reject lambdas in type position and term elaboration rejects Pi expressions in term position")
  })

  describe("declaration checking", () => {
    it("rejects invalid non-rec typed declaration at check stage", () => {
      const result = typecheckDeclSource("let bad : Nat -> Nat := {x} -> leaf leaf x", baseEnv())
      expectDeclStage(result, "check")
    })

    it("valid rec typed decl checks step against T -> T", () => {
      const result = typecheckDeclSource("let rec myId : Bool -> Bool := {x} -> myId x", baseEnv())
      expectDeclOk(result)
    })

    it("invalid rec typed decl step rejected", () => {
      const result = typecheckDeclSource("let rec bad : Nat -> Nat := {x} -> leaf leaf x", baseEnv())
      expectDeclStage(result, "check")
    })
  })

  describe("type alias bypass", () => {
    it.skip("Phase 5: type alias used as annotation works even with svar tag", () => {
      // MyFn has SExpr tag "svar", not "spi" — checking must not dispatch on surface syntax
      const env = baseEnv()
      const declMyFn = typecheckDeclSource("let MyFn : Type := Bool -> Bool", env)
      // This requires Type annotations to work; for now just test the concept
      // Once types are predicates, apply(MyFn_predicate, annotated_tree) should work uniformly
    })
  })

  describe("ascription soundness", () => {
    it("correct ascription with raw Pi pair passes PiCheck", () => {
      const env = baseEnv()
      const pc = env.get("piCheck")!.tree
      const tBool = env.get("Bool")!.tree
      const notTree = env.get("not")!.tree
      const rawPiPair = fork(tBool, fork(LEAF, tBool))  // fork(Bool, K(Bool))
      const ascribed = fork(stem(rawPiPair), stem(notTree))
      const budget = { remaining: 500_000 }
      const result = apply(apply(apply(pc, tBool, budget), fork(LEAF, tBool), budget), ascribed, budget)
      expect(treeEqual(result, LEAF)).toBe(true)
    })

    it("wrong ascription with mismatched raw Pi pair fails PiCheck", () => {
      const env = baseEnv()
      const pc = env.get("piCheck")!.tree
      const tBool = env.get("Bool")!.tree
      const tTree = env.get("Tree")!.tree
      const notTree = env.get("not")!.tree
      const wrongPiPair = fork(tTree, fork(LEAF, tTree))  // fork(Tree, K(Tree)) — wrong type
      const ascribed = fork(stem(wrongPiPair), stem(notTree))
      const budget = { remaining: 500_000 }
      const result = apply(apply(apply(pc, tBool, budget), fork(LEAF, tBool), budget), ascribed, budget)
      expect(treeEqual(result, LEAF)).toBe(false)
    })

    it("wrong ascription caught at next use site in typed composition", () => {
      // Even if a function has a wrong ascription, the next predicate check catches it
      const env = baseEnv()
      const pc = env.get("piCheck")!.tree
      const tBool = env.get("Bool")!.tree
      const tNat = env.get("Nat")!.tree
      // succ (leaf) wrongly ascribed as Bool -> Bool
      const wrongPiPair = fork(tBool, fork(LEAF, tBool))
      const succTree = LEAF  // leaf as succ: stem constructor
      const ascribed = fork(stem(wrongPiPair), stem(succTree))
      // Check ascribed as Bool -> Bool — the ascription passes (D = fork(Bool, K(Bool)) matches)
      const budget1 = { remaining: 500_000 }
      const ascPasses = apply(apply(apply(pc, tBool, budget1), fork(LEAF, tBool), budget1), ascribed, budget1)
      expect(treeEqual(ascPasses, LEAF)).toBe(true)

      // But if we use the result: succ(true) = stem(leaf) = 1 — is that a Bool? No!
      // apply(succTree, LEAF) = stem(LEAF)
      // apply(succTree, stem(LEAF)) = stem(stem(LEAF)) — not a Bool
      // The next predicate check should catch this
      const result = apply(succTree, stem(LEAF))  // succ(false) = stem(stem(leaf))
      expect(treeEqual(apply(tBool, result, { remaining: 500_000 }), LEAF)).toBe(false)
    })
  })
})
