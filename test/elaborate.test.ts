import { describe, it, expect } from "vitest"
import * as fs from "node:fs"
import * as path from "node:path"
import { LEAF, stem, fork, apply, I, K, treeEqual, prettyTree, FAST_EQ } from "../src/tree.js"
import { bareCompile, bareDeclare, bareLoadFile, BareEnv, typedCompileLam, typedLoadFile, TypedEnv } from "../src/elaborate.js"
import { parseLine, parseExpr } from "../src/parse.js"

// Helper: compile a bare expression with optional env
function compile(src: string, env: BareEnv = new Map()): ReturnType<typeof apply> {
  const result = parseLine(src)
  if (!("tag" in result)) throw new Error("Expected expression, got declaration")
  return bareCompile(result, env)
}

// Helper: declare and return the env
function declare(src: string, env: BareEnv = new Map()): BareEnv {
  const result = parseLine(src)
  if ("tag" in result) throw new Error("Expected declaration, got expression")
  return bareDeclare(result, env)
}

// Seed env with leaf + fastEq
function seedEnv(): BareEnv {
  return new Map([["leaf", LEAF], ["fastEq", FAST_EQ]])
}

const TT = LEAF
const FF = stem(LEAF)

describe("bare bracket abstraction", () => {
  it("compiles identity: {x} -> x = I", () => {
    const tree = compile("{x} -> x")
    expect(treeEqual(tree, I)).toBe(true)
  })

  it("compiles K: {x y} -> x = K", () => {
    const tree = compile("{x y} -> x")
    expect(treeEqual(tree, K)).toBe(true)
  })

  it("compiles S: {x} -> (f x) (g x) uses S combinator", () => {
    const env: BareEnv = new Map([["f", stem(stem(LEAF))], ["g", stem(stem(stem(LEAF)))]])
    const tree = compile("{x} -> f x (g x)", env)
    const f = stem(stem(LEAF))
    const g = stem(stem(stem(LEAF)))
    expect(treeEqual(tree, fork(stem(f), g))).toBe(true)
  })

  it("applies S(K(p), K(q)) optimization", () => {
    const env: BareEnv = new Map([["p", stem(LEAF)], ["q", stem(stem(LEAF))]])
    const tree = compile("{x} -> p q", env)
    const pq = apply(stem(LEAF), stem(stem(LEAF)))
    expect(treeEqual(tree, fork(LEAF, pq))).toBe(true)
  })
})

describe("bare declarations", () => {
  it("compiles simple let", () => {
    const env = declare("let id := {x} -> x")
    expect(treeEqual(env.get("id")!, I)).toBe(true)
  })

  it("definitions are available to later defs", () => {
    let env = declare("let id := {x} -> x")
    env = declare("let applyId := {x} -> id x", env)
    expect(treeEqual(env.get("applyId")!, I)).toBe(true)
  })
})

describe("types.disp bootstrap", () => {
  const typesPath = path.join(import.meta.dirname, "..", "types.disp")
  const source = fs.readFileSync(typesPath, "utf-8")

  const env = bareLoadFile(source, seedEnv())

  it("loads without errors", () => {
    expect(env.size).toBeGreaterThan(5)
  })

  it("defines tt = leaf", () => {
    expect(treeEqual(env.get("tt")!, LEAF)).toBe(true)
  })

  it("defines ff = stem(leaf)", () => {
    expect(treeEqual(env.get("ff")!, stem(LEAF))).toBe(true)
  })

  it("triage works: triage(a, b, c)(leaf) = a", () => {
    const triageTree = env.get("triage")!
    const a = stem(stem(LEAF))
    const b = stem(stem(stem(LEAF)))
    const c = fork(LEAF, stem(LEAF))
    const triageABC = apply(apply(apply(triageTree, a), b), c)
    expect(treeEqual(apply(triageABC, LEAF), a)).toBe(true)
    const x = stem(LEAF)
    expect(treeEqual(apply(triageABC, stem(x)), apply(b, x))).toBe(true)
    const y = LEAF
    expect(treeEqual(apply(triageABC, fork(x, y)), apply(apply(c, x), y))).toBe(true)
  })

  it("fix produces a working fixed point", () => {
    const fixTree = env.get("fix")!
    const kI = fork(LEAF, I)
    const result = apply(fixTree, kI)
    expect(treeEqual(apply(result, LEAF), LEAF)).toBe(true)
    expect(treeEqual(apply(result, stem(LEAF)), stem(LEAF))).toBe(true)
  })

  describe("Tree predicate", () => {
    it("accepts everything", () => {
      const tp = env.get("Tree")!
      expect(treeEqual(apply(tp, LEAF), TT)).toBe(true)
      expect(treeEqual(apply(tp, stem(LEAF)), TT)).toBe(true)
      expect(treeEqual(apply(tp, fork(LEAF, LEAF)), TT)).toBe(true)
    })
  })

  describe("Bool predicate", () => {
    it("accepts true (leaf)", () => {
      expect(treeEqual(apply(env.get("Bool")!, LEAF), TT)).toBe(true)
    })
    it("accepts false (stem(leaf))", () => {
      expect(treeEqual(apply(env.get("Bool")!, stem(LEAF)), TT)).toBe(true)
    })
    it("rejects stem(stem(leaf))", () => {
      expect(treeEqual(apply(env.get("Bool")!, stem(stem(LEAF))), FF)).toBe(true)
    })
    it("rejects fork(leaf, leaf)", () => {
      expect(treeEqual(apply(env.get("Bool")!, fork(LEAF, LEAF)), FF)).toBe(true)
    })
  })

  describe("Nat predicate", () => {
    it("accepts zero (leaf)", () => {
      expect(treeEqual(apply(env.get("Nat")!, LEAF), TT)).toBe(true)
    })
    it("accepts 1", () => {
      expect(treeEqual(apply(env.get("Nat")!, stem(LEAF)), TT)).toBe(true)
    })
    it("accepts 3", () => {
      expect(treeEqual(apply(env.get("Nat")!, stem(stem(stem(LEAF)))), TT)).toBe(true)
    })
    it("rejects fork(leaf, leaf)", () => {
      expect(treeEqual(apply(env.get("Nat")!, fork(LEAF, LEAF)), FF)).toBe(true)
    })
  })

  describe("PiCheck", () => {
    const pc = env.get("piCheck")!
    const tNat = env.get("Nat")!
    const tBool = env.get("Bool")!
    const tTree = env.get("Tree")!
    const iComb = env.get("iComb")!

    // Helper: Pi(A, K(B)) non-dependent
    const kWrap = (B: ReturnType<typeof stem>) => fork(LEAF, B)

    // Helper: check ann against Pi(A, B) — needs large budget for complex PiCheck tree
    const check = (A: typeof LEAF, B: typeof LEAF, ann: typeof LEAF) => {
      try {
        return treeEqual(apply(apply(apply(pc, A), B), ann, { remaining: 500000 }), TT)
      } catch { return false }
    }

    describe("Rule 5: I combinator", () => {
      it("I : Nat -> Nat", () => {
        expect(check(tNat, kWrap(tNat), iComb)).toBe(true)
      })
      it("I : Bool -> Bool", () => {
        expect(check(tBool, kWrap(tBool), iComb)).toBe(true)
      })
      it("I : Tree -> Tree", () => {
        expect(check(tTree, kWrap(tTree), iComb)).toBe(true)
      })
    })

    describe("Rule 1: K check", () => {
      it("K(zero) : Nat -> Nat", () => {
        expect(check(tNat, kWrap(tNat), fork(LEAF, LEAF))).toBe(true)
      })
      it("K(false) : Nat -> Bool", () => {
        expect(check(tNat, kWrap(tBool), fork(LEAF, stem(LEAF)))).toBe(true)
      })
      it("K(fork(leaf,leaf)) : Nat -> Nat REJECTED", () => {
        expect(check(tNat, kWrap(tNat), fork(LEAF, fork(LEAF, LEAF)))).toBe(false)
      })
    })

    describe("Rule 4: leaf as function", () => {
      it("leaf : Tree -> Tree (K-shaped codomain)", () => {
        expect(check(tTree, kWrap(tTree), LEAF)).toBe(true)
      })
      it("leaf : Bool -> Bool REJECTED", () => {
        expect(check(tBool, kWrap(tBool), LEAF)).toBe(false)
      })
      it("leaf : Bool -> Nat (stem maps Bool to Nat)", () => {
        expect(check(tBool, kWrap(tNat), LEAF)).toBe(true)
      })
    })

    describe("Rule 3: Triage (not : Bool -> Bool)", () => {
      it("not : Bool -> Bool", () => {
        // not = triage(false, K(true), K(K(false)))
        const notTree = fork(
          fork(stem(LEAF), fork(LEAF, LEAF)),     // fork(false, K(true))
          fork(LEAF, fork(LEAF, stem(LEAF)))       // K(K(false))
        )
        expect(check(tBool, kWrap(tBool), notTree)).toBe(true)
      })
    })

    describe("Compiled functions from env", () => {
      const notFn = env.get("not")!
      const andFn = env.get("and")!
      const isLeafFn = env.get("isLeaf")!

      it("not : Bool -> Bool (compiled)", () => {
        expect(check(tBool, kWrap(tBool), notFn)).toBe(true)
      })

      it("not : Nat -> Nat (maps {0,1} to {0,1}, both are Nats)", () => {
        // not(0)=ff=1, not(succ(x))=tt=0 — both valid Nats
        expect(check(tNat, kWrap(tNat), notFn)).toBe(true)
      })

      it("isLeaf : Bool -> Bool", () => {
        expect(check(tBool, kWrap(tBool), isLeafFn)).toBe(true)
      })

      it("isLeaf : Nat -> Bool", () => {
        expect(check(tNat, kWrap(tBool), isLeafFn)).toBe(true)
      })

      it("isLeaf : Tree -> Bool", () => {
        expect(check(tTree, kWrap(tBool), isLeafFn)).toBe(true)
      })
    })

    describe("Higher-order: functions returning functions", () => {
      // Known limitation: stemFnCheck only accepts K(tTree) codomain
      it("K : Tree -> Tree (stemFnCheck accepts K(tTree))", () => {
        expect(check(tTree, kWrap(tTree), K)).toBe(true)
      })

      it("K : Bool -> Tree -> Bool (stemFnCheck enumerates Bool domain)", () => {
        const innerPi = fork(tTree, kWrap(tBool))
        expect(check(tBool, kWrap(innerPi), K)).toBe(true)
      })

      it("K(K(zero)) : Tree -> Tree -> Nat", () => {
        const innerPi = fork(tTree, kWrap(tNat))
        const kk0 = fork(LEAF, fork(LEAF, LEAF))
        expect(check(tTree, kWrap(innerPi), kk0)).toBe(true)
      })
    })

    describe("S-node functions (KNOWN LIMITATION: need type annotations)", () => {
      // S-nodes in bare-compiled trees lack intermediate type D.
      // PiCheck extracts D from tree structure, but for S(c,b) = fork(stem(c), b),
      // c is the function itself, not a type annotation. These all fail.
      it("{x} -> and x x : Bool -> Bool REJECTED (unannotated S-node)", () => {
        const fn = bareCompile(parseLine("{x} -> and x x") as any, env)
        expect(check(tBool, kWrap(tBool), fn)).toBe(false)
      })

      it("{x} -> not (not x) : Bool -> Bool REJECTED (unannotated S-node)", () => {
        const fn = bareCompile(parseLine("{x} -> not (not x)") as any, env)
        expect(check(tBool, kWrap(tBool), fn)).toBe(false)
      })
    })

    describe("Negative cases", () => {
      it("and : Bool -> Bool REJECTED (partial application, not a Bool)", () => {
        const andFn = env.get("and")!
        expect(check(tBool, kWrap(tBool), andFn)).toBe(false)
      })

      it("random fork rejected as Nat -> Nat", () => {
        const junk = fork(stem(stem(LEAF)), fork(LEAF, stem(LEAF)))
        expect(check(tNat, kWrap(tNat), junk)).toBe(false)
      })

      it("K(zero) : Nat -> Nat (LEAF is both true and zero)", () => {
        expect(check(tNat, kWrap(tNat), fork(LEAF, LEAF))).toBe(true)
      })

      it("K(stem(stem(leaf))) : Bool -> Bool REJECTED (not a bool)", () => {
        expect(check(tBool, kWrap(tBool), fork(LEAF, stem(stem(LEAF))))).toBe(false)
      })
    })
  })

  describe("Typed elaborator (two-pass bootstrap)", () => {
    // Two-pass loading: bare first, then typed with annotations
    const tenv = typedLoadFile(source, env)
    const tBool = env.get("Bool")!
    const tTree = env.get("Tree")!
    const pc = env.get("piCheck")!
    const kw = (B: Tree) => fork(LEAF, B)

    const check = (A: Tree, B: Tree, ann: Tree) => {
      try {
        return treeEqual(apply(apply(apply(pc, A), B), ann, { remaining: 500000 }), TT)
      } catch { return false }
    }

    it("typedLoadFile produces TypedEnv", () => {
      expect(tenv.size).toBeGreaterThan(5)
      // not should have a type annotation
      const notEntry = tenv.get("not")!
      expect(treeEqual(notEntry.type, fork(tBool, kw(tBool)))).toBe(true)
    })

    it("typed defs keep bare trees (ascription wrapping happens at use site)", () => {
      // typedLoadFile records types but doesn't re-compile
      const andEntry = tenv.get("and")!
      expect(treeEqual(andEntry.tree, env.get("and")!)).toBe(true)
      // Type is recorded
      expect(treeEqual(andEntry.type, fork(tBool, kw(fork(tBool, kw(tBool)))))).toBe(true)
    })

    // Helper: compile lambda with typed env from two-pass loading
    function typedLam(src: string, expectedType: Tree): Tree {
      const parsed = parseLine(src)
      if (!("tag" in parsed) || parsed.tag !== "slam") throw new Error("Expected lambda")
      return typedCompileLam(parsed.params, parsed.body, expectedType, tenv)
    }

    it("{x} -> not (not x) : Bool -> Bool", () => {
      const annotated = typedLam("{x} -> not (not x)", fork(tBool, kw(tBool)))
      expect(check(tBool, kw(tBool), annotated)).toBe(true)
    })

    it("{x} -> and x x : Bool -> Bool (inner `and` now annotated)", () => {
      const annotated = typedLam("{x} -> and x x", fork(tBool, kw(tBool)))
      expect(check(tBool, kw(tBool), annotated)).toBe(true)
    })

    it("{x} -> or x (not x) : Bool -> Bool (inner `or` now annotated)", () => {
      const annotated = typedLam("{x} -> or x (not x)", fork(tBool, kw(tBool)))
      expect(check(tBool, kw(tBool), annotated)).toBe(true)
    })

    it("annotated tree differs from bare tree", () => {
      const annotated = typedLam("{x} -> not (not x)", fork(tBool, kw(tBool)))
      const bare = bareCompile(parseLine("{x} -> not (not x)") as any, env)
      expect(treeEqual(annotated, bare)).toBe(false)
    })
  })
})
