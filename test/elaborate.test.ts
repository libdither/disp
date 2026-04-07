import { describe, it, expect } from "vitest"
import * as fs from "node:fs"
import * as path from "node:path"
import { LEAF, stem, fork, apply, I, K, treeEqual, FAST_EQ, isFork, isStem, type Tree } from "../src/tree.js"
import { compile, declare, loadFile, Env, entry, typedCompileLam, compileType, extract } from "../src/elaborate.js"
import { parseLine, parseExpr } from "../src/parse.js"

// Helper: compile a bare expression with optional env
function compileExpr(src: string, env: Env = new Map()): ReturnType<typeof apply> {
  const result = parseLine(src)
  if (!("tag" in result)) throw new Error("Expected expression, got declaration")
  return compile(result, env)
}

// Helper: declare and return the env
function declStr(src: string, env: Env = new Map()): Env {
  const result = parseLine(src)
  if ("tag" in result) throw new Error("Expected declaration, got expression")
  return declare(result, env)
}

// Seed env with leaf + fastEq
function seedEnv(): Env {
  return new Map([["leaf", entry(LEAF)], ["fastEq", entry(FAST_EQ)]])
}

const TT = LEAF
const FF = stem(LEAF)

describe("bare bracket abstraction", () => {
  it("compiles identity: {x} -> x = I", () => {
    const tree = compileExpr("{x} -> x")
    expect(treeEqual(tree, I)).toBe(true)
  })

  it("compiles K: {x y} -> x = K", () => {
    const tree = compileExpr("{x y} -> x")
    expect(treeEqual(tree, K)).toBe(true)
  })

  it("compiles S: {x} -> (f x) (g x) uses S combinator", () => {
    const env: Env = new Map([["f", entry(stem(stem(LEAF)))], ["g", entry(stem(stem(stem(LEAF))))]])
    const tree = compileExpr("{x} -> f x (g x)", env)
    const f = stem(stem(LEAF))
    const g = stem(stem(stem(LEAF)))
    expect(treeEqual(tree, fork(stem(f), g))).toBe(true)
  })

  it("applies S(K(p), K(q)) optimization", () => {
    const env: Env = new Map([["p", entry(stem(LEAF))], ["q", entry(stem(stem(LEAF)))]])
    const tree = compileExpr("{x} -> p q", env)
    const pq = apply(stem(LEAF), stem(stem(LEAF)))
    expect(treeEqual(tree, fork(LEAF, pq))).toBe(true)
  })
})

describe("bare declarations", () => {
  it("compiles simple let", () => {
    const env = declStr("let id := {x} -> x")
    expect(treeEqual(env.get("id")!.tree, I)).toBe(true)
  })

  it("definitions are available to later defs", () => {
    let env = declStr("let id := {x} -> x")
    env = declStr("let applyId := {x} -> id x", env)
    expect(treeEqual(env.get("applyId")!.tree, I)).toBe(true)
  })
})

describe("types.disp bootstrap", () => {
  const typesPath = path.join(import.meta.dirname, "..", "types.disp")
  const source = fs.readFileSync(typesPath, "utf-8")

  const env = loadFile(source, seedEnv())

  it("loads without errors", () => {
    expect(env.size).toBeGreaterThan(5)
  })

  it("defines tt = leaf", () => {
    expect(treeEqual(env.get("tt")!.tree, LEAF)).toBe(true)
  })

  it("defines ff = stem(leaf)", () => {
    expect(treeEqual(env.get("ff")!.tree, stem(LEAF))).toBe(true)
  })

  it("triage works: triage(a, b, c)(leaf) = a", () => {
    const triageTree = env.get("triage")!.tree
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
    const fixTree = env.get("fix")!.tree
    const kI = fork(LEAF, I)
    const result = apply(fixTree, kI)
    expect(treeEqual(apply(result, LEAF), LEAF)).toBe(true)
    expect(treeEqual(apply(result, stem(LEAF)), stem(LEAF))).toBe(true)
  })

  describe("Tree predicate", () => {
    it("accepts everything", () => {
      const tp = env.get("Tree")!.tree
      expect(treeEqual(apply(tp, LEAF), TT)).toBe(true)
      expect(treeEqual(apply(tp, stem(LEAF)), TT)).toBe(true)
      expect(treeEqual(apply(tp, fork(LEAF, LEAF)), TT)).toBe(true)
    })
  })

  describe("Bool predicate", () => {
    it("accepts true (leaf)", () => {
      expect(treeEqual(apply(env.get("Bool")!.tree, LEAF), TT)).toBe(true)
    })
    it("accepts false (stem(leaf))", () => {
      expect(treeEqual(apply(env.get("Bool")!.tree, stem(LEAF)), TT)).toBe(true)
    })
    it("rejects stem(stem(leaf))", () => {
      expect(treeEqual(apply(env.get("Bool")!.tree, stem(stem(LEAF))), FF)).toBe(true)
    })
    it("rejects fork(leaf, leaf)", () => {
      expect(treeEqual(apply(env.get("Bool")!.tree, fork(LEAF, LEAF)), FF)).toBe(true)
    })
  })

  describe("Nat predicate", () => {
    it("accepts zero (leaf)", () => {
      expect(treeEqual(apply(env.get("Nat")!.tree, LEAF), TT)).toBe(true)
    })
    it("accepts 1", () => {
      expect(treeEqual(apply(env.get("Nat")!.tree, stem(LEAF)), TT)).toBe(true)
    })
    it("accepts 3", () => {
      expect(treeEqual(apply(env.get("Nat")!.tree, stem(stem(stem(LEAF)))), TT)).toBe(true)
    })
    it("rejects fork(leaf, leaf)", () => {
      expect(treeEqual(apply(env.get("Nat")!.tree, fork(LEAF, LEAF)), FF)).toBe(true)
    })
  })

  describe("PiCheck", () => {
    const pc = env.get("piCheck")!.tree
    const tNat = env.get("Nat")!.tree
    const tBool = env.get("Bool")!.tree
    const tTree = env.get("Tree")!.tree
    const iComb = env.get("iComb")!.tree

    // Helper: Pi(A, K(B)) non-dependent
    const kWrap = (B: ReturnType<typeof stem>) => fork(LEAF, B)
    const specialize = (A: typeof LEAF, B: typeof LEAF) => {
      const budget = { remaining: 500000 }
      return apply(apply(pc, A, budget), B, budget)
    }

    const checkOrThrow = (A: typeof LEAF, B: typeof LEAF, ann: typeof LEAF) =>
      treeEqual(apply(apply(apply(pc, A), B), ann, { remaining: 500000 }), TT)

    const check = checkOrThrow

    describe("Specialization behavior", () => {
      it("partially applying piCheck twice yields a reusable shared predicate", () => {
        const pred1 = specialize(tBool, kWrap(tBool))
        const pred2 = specialize(tBool, kWrap(tBool))
        const notFn = env.get("not")!.tree

        expect(treeEqual(pred1, pred2)).toBe(true)
        expect(treeEqual(apply(pred1, notFn, { remaining: 500000 }), TT)).toBe(true)
        expect(treeEqual(apply(pred2, notFn, { remaining: 500000 }), TT)).toBe(true)
      })

      it("specialized pi predicates work as checkers via apply", () => {
        // 3-arg piCheck produces partials (not triage-shaped trees), but
        // apply(partial, ann) triggers the checker and returns tt/ff.
        // This is the correct design — the Futamura projection happens
        // via apply memoization at the runtime level, not as a tree-level triage.
        const pred = specialize(tBool, kWrap(tBool))
        const notFn = env.get("not")!.tree
        expect(treeEqual(apply(pred, notFn, { remaining: 500000 }), TT)).toBe(true)
        expect(treeEqual(apply(pred, fork(LEAF, LEAF), { remaining: 500000 }), TT)).toBe(true) // K(zero) : Bool -> Bool? zero=true=leaf, yes
        expect(treeEqual(apply(pred, fork(LEAF, stem(stem(LEAF))), { remaining: 500000 }), FF)).toBe(true) // K(2) : Bool -> Bool? 2 not Bool
      })
    })

    describe("piPred: evaluable Pi type predicates", () => {
      const piPredTree = env.get("piPred")!.tree
      const budget = () => ({ remaining: 500_000 })

      const buildPred = (A: Tree, B: Tree) => {
        const b = budget()
        return apply(apply(piPredTree, A, b), B, b)
      }

      it("apply(apply(piPred, A), B) produces a triage-shaped tree", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        expect(isFork(pred) && isFork(pred.left)).toBe(true)
      })

      it("specialization stays within budget", () => {
        const b = { remaining: 100_000 }
        const pred = apply(apply(piPredTree, tBool, b), kWrap(tBool), b)
        const cost = 100_000 - b.remaining
        expect(cost).toBeLessThan(50_000)
      })

      it("same (A,B) produces same predicate tree (hash-consing)", () => {
        const pred1 = buildPred(tBool, kWrap(tBool))
        const pred2 = buildPred(tBool, kWrap(tBool))
        expect(treeEqual(pred1, pred2)).toBe(true)
      })

      it("predicate accepts not : Bool -> Bool", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        const notFn = env.get("not")!.tree
        expect(treeEqual(apply(pred, notFn, budget()), TT)).toBe(true)
      })

      it("predicate accepts I : Nat -> Nat", () => {
        const pred = buildPred(tNat, kWrap(tNat))
        expect(treeEqual(apply(pred, iComb, budget()), TT)).toBe(true)
      })

      it("predicate accepts K(zero) : Nat -> Nat", () => {
        const pred = buildPred(tNat, kWrap(tNat))
        expect(treeEqual(apply(pred, fork(LEAF, LEAF), budget()), TT)).toBe(true)
      })

      it("predicate rejects K(fork(leaf,leaf)) : Nat -> Nat", () => {
        const pred = buildPred(tNat, kWrap(tNat))
        expect(treeEqual(apply(pred, fork(LEAF, fork(LEAF, LEAF)), budget()), FF)).toBe(true)
      })

      it("predicate accepts leaf : Tree -> Tree", () => {
        const pred = buildPred(tTree, kWrap(tTree))
        expect(treeEqual(apply(pred, LEAF, budget()), TT)).toBe(true)
      })

      it("predicate rejects leaf : Bool -> Bool", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        expect(treeEqual(apply(pred, LEAF, budget()), FF)).toBe(true)
      })

      it("predicate agrees with piCheck on typed elaborator output", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        const annotated = typedCompileLam(
          ["x"], (parseLine("{x} -> not (not x)") as any).body,
          fork(tBool, fork(LEAF, tBool)), env)

        const predResult = treeEqual(apply(pred, annotated, budget()), TT)
        const pcResult = check(tBool, kWrap(tBool), annotated)
        expect(predResult).toBe(true)
        expect(pcResult).toBe(true)
        expect(predResult).toBe(pcResult)
      })

      it("predicate accepts valid ascription", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        const notEntry = env.get("not")!
        const ascribed = fork(stem(fork(tBool, kWrap(tBool))), stem(notEntry.tree))
        expect(treeEqual(apply(pred, ascribed, budget()), TT)).toBe(true)
      })

      it("predicate rejects mismatched ascription", () => {
        const pred = buildPred(tBool, kWrap(tBool))
        const notEntry = env.get("not")!
        const wrongType = fork(tTree, kWrap(tTree))
        const ascribed = fork(stem(wrongType), stem(notEntry.tree))
        expect(treeEqual(apply(pred, ascribed, budget()), FF)).toBe(true)
      })
    })

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

      it("Nat-domain triage checks zero and succ cases directly", () => {
        const isZero = compile(parseLine("{x} -> triage tt ({_} -> ff) ({_ _} -> ff) x") as any, env)
        expect(checkOrThrow(tNat, kWrap(tBool), isZero)).toBe(true)
      })

      it("Tree-domain triage checks the fork-handler path", () => {
        const isForkFn = compile(parseLine("{x} -> triage ff ({_} -> ff) ({_ _} -> tt) x") as any, env)
        expect(checkOrThrow(tTree, kWrap(tBool), isForkFn)).toBe(true)
      })
    })

    describe("Compiled functions from env", () => {
      const notFn = env.get("not")!.tree
      const andFn = env.get("and")!.tree
      const isLeafFn = env.get("isLeaf")!.tree

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
        const fn = compile(parseLine("{x} -> and x x") as any, env)
        expect(check(tBool, kWrap(tBool), fn)).toBe(false)
      })

      it("{x} -> not (not x) : Bool -> Bool REJECTED (unannotated S-node)", () => {
        const fn = compile(parseLine("{x} -> not (not x)") as any, env)
        expect(check(tBool, kWrap(tBool), fn)).toBe(false)
      })
    })

    describe("Negative cases", () => {
      it("and : Bool -> Bool REJECTED (partial application, not a Bool)", () => {
        const andFn = env.get("and")!.tree
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

  describe("Typed elaborator (single-pass)", () => {
    // loadFile already records type annotations — no second pass needed
    const tBool = env.get("Bool")!.tree
    const tTree = env.get("Tree")!.tree
    const pc = env.get("piCheck")!.tree
    const kw = (B: Tree) => fork(LEAF, B)

    const checkOrThrow = (A: Tree, B: Tree, ann: Tree) =>
      treeEqual(apply(apply(apply(pc, A), B), ann, { remaining: 500000 }), TT)
    const check = checkOrThrow

    const specializeStoredPi = (A: Tree, B: Tree) => {
      const budget = { remaining: 500000 }
      return apply(apply(pc, A, budget), B, budget)
    }

    it("loadFile stores specialized Pi predicates for function annotations", () => {
      expect(env.size).toBeGreaterThan(5)
      const notEntry = env.get("not")!
      // Stored type is a specialized predicate (triage-shaped) from piPred
      expect(isFork(notEntry.type) && isFork(notEntry.type.left)).toBe(true)
      // piPair preserves the raw structure for elaboration
      expect(notEntry.piPair).toBeDefined()
      expect(treeEqual(fork(notEntry.piPair!.domain, notEntry.piPair!.codomain), fork(tBool, kw(tBool)))).toBe(true)
    })

    it("typed defs store specialized predicates alongside bare trees", () => {
      const andEntry = env.get("and")!
      // Specialized predicate is triage-shaped
      expect(isFork(andEntry.type) && isFork(andEntry.type.left)).toBe(true)
      // piPair preserves raw structure
      const rawAndType = fork(tBool, kw(fork(tBool, kw(tBool))))
      expect(treeEqual(fork(andEntry.piPair!.domain, andEntry.piPair!.codomain), rawAndType)).toBe(true)
    })

    // Helper: compile lambda with typed env
    function typedLam(src: string, expectedType: Tree): Tree {
      const parsed = parseLine(src)
      if (!("tag" in parsed) || parsed.tag !== "slam") throw new Error("Expected lambda")
      return typedCompileLam(parsed.params, parsed.body, expectedType, env)
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
      const bare = compile(parseLine("{x} -> not (not x)") as any, env)
      expect(treeEqual(annotated, bare)).toBe(false)
    })

    describe("explicit S-node and ascription coverage", () => {
      const annotatedS = typedLam("{x} -> not (not x)", fork(tBool, kw(tBool)))
      const junk = fork(stem(stem(LEAF)), fork(LEAF, stem(LEAF)))

      it("accepts an explicit annotated S-node emitted by typed elaboration", () => {
        expect(isFork(annotatedS) && isStem(annotatedS.left) && isFork(annotatedS.right)).toBe(true)
        expect(check(tBool, kw(tBool), annotatedS)).toBe(true)
      })

      it("rejects an explicit annotated S-node with wrong D", () => {
        if (!(isFork(annotatedS) && isStem(annotatedS.left) && isFork(annotatedS.right))) {
          throw new Error("expected outer S-node shape")
        }
        const wrongD = fork(stem(kw(tTree)), annotatedS.right)
        expect(checkOrThrow(tBool, kw(tBool), wrongD)).toBe(false)
      })

      it("rejects an explicit annotated S-node with wrong annC", () => {
        if (!(isFork(annotatedS) && isStem(annotatedS.left) && isFork(annotatedS.right))) {
          throw new Error("expected outer S-node shape")
        }
        const wrongAnnC = fork(annotatedS.left, fork(junk, annotatedS.right.right))
        expect(checkOrThrow(tBool, kw(tBool), wrongAnnC)).toBe(false)
      })

      it("rejects an explicit annotated S-node with wrong annB", () => {
        if (!(isFork(annotatedS) && isStem(annotatedS.left) && isFork(annotatedS.right))) {
          throw new Error("expected outer S-node shape")
        }
        const wrongAnnB = fork(annotatedS.left, fork(annotatedS.right.left, junk))
        expect(checkOrThrow(tBool, kw(tBool), wrongAnnB)).toBe(false)
      })

      it("accepts a direct ascription when the claimed type matches", () => {
        const notEntry = env.get("not")!
        // Ascriptions carry the raw Pi pair (type identity), not the specialized predicate
        const rawPiPair = fork(notEntry.piPair!.domain, notEntry.piPair!.codomain)
        const ascribed = fork(stem(rawPiPair), stem(notEntry.tree))
        expect(checkOrThrow(tBool, kw(tBool), ascribed)).toBe(true)
      })

      it("rejects a direct ascription when the claimed type mismatches", () => {
        const notEntry = env.get("not")!
        const wrongType = fork(tTree, kw(tTree))
        const ascribed = fork(stem(wrongType), stem(notEntry.tree))
        expect(checkOrThrow(tBool, kw(tBool), ascribed)).toBe(false)
      })
    })
  })

  describe("extract function", () => {
    it("extracts leaf unchanged", () => {
      expect(treeEqual(extract(LEAF), LEAF)).toBe(true)
    })

    it("extracts stem(a) to stem(extract(a))", () => {
      const tree = stem(stem(LEAF))
      expect(treeEqual(extract(tree), tree)).toBe(true)
    })

    it("extracts K-node fork(leaf, v) to fork(leaf, extract(v))", () => {
      const v = stem(stem(LEAF))
      const kNode = fork(LEAF, v)
      expect(treeEqual(extract(kNode), fork(LEAF, v))).toBe(true)
    })

    it("strips D from annotated S-node fork(stem(D), fork(c, b))", () => {
      const D = stem(stem(LEAF))
      const c = fork(LEAF, LEAF)
      const b = stem(LEAF)
      const annotated = fork(stem(D), fork(c, b))
      // extract should strip D: fork(stem(extract(c)), extract(b))
      expect(treeEqual(extract(annotated), fork(stem(extract(c)), extract(b)))).toBe(true)
    })

    it("extracts triage node fork(fork(c, d), b) recursively", () => {
      const c = LEAF
      const d = stem(LEAF)
      const b = fork(LEAF, LEAF)
      const triage = fork(fork(c, d), b)
      expect(treeEqual(extract(triage), fork(fork(extract(c), extract(d)), extract(b)))).toBe(true)
    })

    it("unwraps ascription fork(stem(T), stem(body)) to body without recursing into body", () => {
      // body contains fork(stem(c), fork(x, y)) — a valid bare S-node
      // extract should return body as-is, NOT recurse and misinterpret it
      const c = stem(LEAF)
      const x = LEAF
      const y = stem(stem(LEAF))
      const body = fork(stem(c), fork(x, y))  // looks like annotated S-node if recursed into
      const T = fork(LEAF, fork(LEAF, stem(LEAF)))  // some type
      const ascription = fork(stem(T), stem(body))

      // Per theory: extract(fork(stem(T), stem(body))) = body
      // Current bug: code does extract(right.child) which recurses into body
      expect(treeEqual(extract(ascription), body)).toBe(true)
    })

    it("round-trip: extract(annotated) executes identically to bare tree", () => {
      const annotated = typedCompileLam(
        ["x"], (parseLine("{x} -> not (not x)") as any).body,
        fork(env.get("Bool")!.tree, fork(LEAF, env.get("Bool")!.tree)), env)
      const bare = compile(parseLine("{x} -> not (not x)") as any, env)
      const extracted = extract(annotated)

      // Both should produce the same results when applied
      expect(treeEqual(apply(extracted, LEAF, { remaining: 10000 }),
                        apply(bare, LEAF, { remaining: 10000 }))).toBe(true)
      expect(treeEqual(apply(extracted, stem(LEAF), { remaining: 10000 }),
                        apply(bare, stem(LEAF), { remaining: 10000 }))).toBe(true)
    })
  })

  describe("compileType", () => {
    const tBool = env.get("Bool")!.tree
    const tNat = env.get("Nat")!.tree

    it("Bool -> Bool compiles to raw Pi pair fork(Bool, K(Bool))", () => {
      const result = compileType(parseExpr("Bool -> Bool"), env)
      expect(treeEqual(result, fork(tBool, fork(LEAF, tBool)))).toBe(true)
    })

    it("Nat -> Bool -> Nat compiles to nested raw Pi pair", () => {
      const result = compileType(parseExpr("Nat -> Bool -> Nat"), env)
      expect(treeEqual(result, fork(tNat, fork(LEAF, fork(tBool, fork(LEAF, tNat)))))).toBe(true)
    })

    it("applied type Vec zero compiles via apply", () => {
      const typesPath = path.join(import.meta.dirname, "..", "types.disp")
      const source = fs.readFileSync(typesPath, "utf-8")
      const extra = `\nlet rec Vec := {n v} -> triage (isLeaf v) ({n'} -> triage ff ({_} -> ff) ({h t} -> Vec n' t) v) ({_ _} -> ff) n\n`
      const vecSeed: Env = new Map([...seedEnv(), ["zero", entry(LEAF)], ["succ", entry(LEAF)]])
      const vecEnv = loadFile(source + extra, vecSeed)
      const vecTree = vecEnv.get("Vec")!.tree
      const result = compileType(parseExpr("Vec zero"), vecEnv)
      expect(treeEqual(result, apply(vecTree, LEAF))).toBe(true)
    })

    it("rejects lambdas in type position", () => {
      expect(() => compileType(parseExpr("{x} -> x"), env)).toThrow("Lambda in type position")
    })

    it("(b : Bool) -> Bool compiles same as Bool -> Bool", () => {
      const dep = compileType(parseExpr("(b : Bool) -> Bool"), env)
      const nonDep = compileType(parseExpr("Bool -> Bool"), env)
      expect(treeEqual(dep, nonDep)).toBe(true)
    })

    it("(A : Type) -> A -> A compiles dependent Pi with bracket-abstracted codomain", () => {
      const result = compileType(parseExpr("(A : Type) -> A -> A"), env)
      // Result should be fork(Type, codomainFamily) where codomainFamily is NOT K-wrapped
      expect(isFork(result)).toBe(true)
      if (!isFork(result)) throw new Error("expected fork")
      // Domain is Type = LEAF
      expect(treeEqual(result.left, LEAF)).toBe(true)
      // Codomain should NOT be K-wrapped (it's dependent)
      // apply(codomainFamily, Bool) should give Pi(Bool, K(Bool)) = fork(Bool, K(Bool))
      const tBool = env.get("Bool")!.tree
      const instantiated = apply(result.right, tBool)
      expect(treeEqual(instantiated, fork(tBool, fork(LEAF, tBool)))).toBe(true)
    })
  })

  describe("allowlist checker", () => {
    const tBool = env.get("Bool")!.tree
    const tNat = env.get("Nat")!.tree
    const tTree = env.get("Tree")!.tree
    const notTree = env.get("not")!.tree
    const notType = fork(tBool, fork(LEAF, tBool))  // fork(Bool, K(Bool))
    const andTree = env.get("and")!.tree
    const andType = fork(tBool, fork(LEAF, fork(tBool, fork(LEAF, tBool))))
    const alContains = env.get("allowlistContains")!.tree
    const alCheck = env.get("allowlistCheck")!.tree
    const pc = env.get("piCheck")!.tree
    const kw = (B: Tree) => fork(LEAF, B)
    const budget = () => ({ remaining: 500_000 })

    // Build a simple allowlist: [(notTree, notType), (andTree, andType)]
    const allowlist = fork(fork(notTree, notType), fork(fork(andTree, andType), LEAF))

    describe("allowlistContains", () => {
      it("finds (not, Bool->Bool) in the allowlist", () => {
        const b = budget()
        const result = apply(apply(apply(alContains, allowlist, b), notTree, b), notType, b)
        expect(treeEqual(result, TT)).toBe(true)
      })

      it("finds (and, Bool->Bool->Bool) in the allowlist", () => {
        const b = budget()
        const result = apply(apply(apply(alContains, allowlist, b), andTree, b), andType, b)
        expect(treeEqual(result, TT)).toBe(true)
      })

      it("rejects a body not in the allowlist", () => {
        const junk = fork(stem(stem(LEAF)), LEAF)
        const b = budget()
        const result = apply(apply(apply(alContains, allowlist, b), junk, b), notType, b)
        expect(treeEqual(result, FF)).toBe(true)
      })

      it("rejects correct body with wrong type", () => {
        const wrongType = fork(tNat, kw(tNat))
        const b = budget()
        const result = apply(apply(apply(alContains, allowlist, b), notTree, b), wrongType, b)
        expect(treeEqual(result, FF)).toBe(true)
      })

      it("rejects on empty allowlist", () => {
        const b = budget()
        const result = apply(apply(apply(alContains, LEAF, b), notTree, b), notType, b)
        expect(treeEqual(result, FF)).toBe(true)
      })
    })

    describe("allowlistCheck", () => {
      // Build the walker with our allowlist
      const walker = apply(alCheck, allowlist, budget())

      it("accepts leaf", () => {
        expect(treeEqual(apply(walker, LEAF, budget()), TT)).toBe(true)
      })

      it("accepts stem(leaf)", () => {
        expect(treeEqual(apply(walker, stem(LEAF), budget()), TT)).toBe(true)
      })

      it("accepts K-node fork(leaf, v) with no ascriptions", () => {
        expect(treeEqual(apply(walker, fork(LEAF, LEAF), budget()), TT)).toBe(true)
      })

      it("accepts triage node with no ascriptions", () => {
        // not = fork(fork(stem(leaf), fork(leaf, leaf)), fork(leaf, fork(leaf, stem(leaf))))
        // This is a raw triage tree — no ascriptions inside
        expect(treeEqual(apply(walker, notTree, budget()), TT)).toBe(true)
      })

      it("accepts a valid ascription when body is in allowlist", () => {
        const ascribed = fork(stem(notType), stem(notTree))
        expect(treeEqual(apply(walker, ascribed, budget()), TT)).toBe(true)
      })

      it("rejects an ascription with body NOT in allowlist", () => {
        const malicious = fork(stem(stem(LEAF)), LEAF)  // junk body
        const ascribed = fork(stem(notType), stem(malicious))
        expect(treeEqual(apply(walker, ascribed, budget()), FF)).toBe(true)
      })

      it("rejects an ascription with correct body but wrong type", () => {
        const wrongType = fork(tNat, kw(tNat))
        const ascribed = fork(stem(wrongType), stem(notTree))
        expect(treeEqual(apply(walker, ascribed, budget()), FF)).toBe(true)
      })

      it("accepts annotated S-node with valid ascriptions in sub-terms", () => {
        // S-node: fork(stem(D), fork(annC, annB))
        // where annC and annB are valid ascriptions of not
        const D = kw(tBool)  // K(Bool) — intermediate type
        const annC = fork(stem(notType), stem(notTree))
        const annB = fork(stem(notType), stem(notTree))
        const sNode = fork(stem(D), fork(annC, annB))
        expect(treeEqual(apply(walker, sNode, budget()), TT)).toBe(true)
      })

      it("rejects annotated S-node with forged ascription in sub-term", () => {
        const D = kw(tBool)
        const forged = fork(stem(notType), stem(fork(stem(stem(LEAF)), LEAF)))  // junk body
        const annB = fork(stem(notType), stem(notTree))
        const sNode = fork(stem(D), fork(forged, annB))
        expect(treeEqual(apply(walker, sNode, budget()), FF)).toBe(true)
      })

      it("does NOT recurse into ascription bodies (body is opaque)", () => {
        // Body contains what looks like a forged ascription inside, but
        // the walker shouldn't recurse into it — body is opaque
        const innerForge = fork(stem(fork(tTree, kw(tTree))), stem(LEAF))
        // Put innerForge as the body of a valid ascription
        // We need innerForge to be in the allowlist for the outer ascription to pass
        const alWithInner = fork(fork(innerForge, notType), fork(fork(notTree, notType), LEAF))
        const walkerWithInner = apply(alCheck, alWithInner, budget())
        const ascribed = fork(stem(notType), stem(innerForge))
        // Outer ascription: (innerForge, notType) is in allowlist → passes
        // Inner structure of innerForge is NOT walked → irrelevant
        expect(treeEqual(apply(walkerWithInner, ascribed, budget()), TT)).toBe(true)
      })
    })

    describe("verifiedCheck (piCheck AND allowlistCheck)", () => {
      const walker = apply(alCheck, allowlist, budget())

      const verifiedCheck = (A: Tree, B: Tree, ann: Tree): boolean => {
        const b1 = budget()
        const alOk = treeEqual(apply(walker, ann, b1), TT)
        const b2 = budget()
        const pcOk = treeEqual(apply(apply(apply(pc, A, b2), B, b2), ann, b2), TT)
        return alOk && pcOk
      }

      it("accepts valid ascription of not : Bool -> Bool", () => {
        const ascribed = fork(stem(notType), stem(notTree))
        expect(verifiedCheck(tBool, kw(tBool), ascribed)).toBe(true)
      })

      it("rejects forged ascription — passes piCheck but fails allowlistCheck", () => {
        // leaf (succ constructor) wrongly ascribed as Bool -> Bool
        const forged = fork(stem(notType), stem(LEAF))
        // piCheck: fastEq(notType, fork(Bool, K(Bool))) → true (type matches)
        const b = budget()
        const pcPasses = treeEqual(apply(apply(apply(pc, tBool, b), kw(tBool), b), forged, b), TT)
        expect(pcPasses).toBe(true)  // piCheck alone would accept!

        // But verifiedCheck catches it
        expect(verifiedCheck(tBool, kw(tBool), forged)).toBe(false)
      })

      it("accepts typed elaborator output with annotated S-nodes", () => {
        // {x} -> not (not x) : Bool -> Bool
        const annotated = typedCompileLam(
          ["x"], (parseLine("{x} -> not (not x)") as any).body,
          fork(tBool, kw(tBool)), env)

        // piCheck passes (tested elsewhere)
        // allowlistCheck: walks the annotated tree, finds ascriptions of not, checks allowlist
        expect(verifiedCheck(tBool, kw(tBool), annotated)).toBe(true)
      })

      it("accepts raw triage functions (no ascriptions to check)", () => {
        // not as a raw triage tree — no ascriptions, allowlistCheck trivially passes
        expect(verifiedCheck(tBool, kw(tBool), notTree)).toBe(true)
      })

      it("rejects wrong D — piCheck catches, allowlistCheck irrelevant", () => {
        // Annotated S-node with wrong D
        const annotated = typedCompileLam(
          ["x"], (parseLine("{x} -> not (not x)") as any).body,
          fork(tBool, kw(tBool)), env)
        if (!(isFork(annotated) && isStem(annotated.left))) throw new Error("expected S-node")
        const wrongD = fork(stem(kw(tTree)), annotated.right)
        expect(verifiedCheck(tBool, kw(tBool), wrongD)).toBe(false)
      })
    })
  })
})
