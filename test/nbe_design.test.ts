// NbE Design Validation Prototype
// This validates the architecture using TypeScript types.
// The real implementation would be tree programs in .disp.
//
// Key design points being validated:
// - napply: KH→stuck, KStuck→stuck, VLam→body, data→tree-calc rules
// - H-rule in napply: universal neutral-argument check before dispatch
// - Types as VLam with metadata: Pi has PI_TAG, Universe has UNIV_TAG
// - type_of_neutral: spine inference via piCodFn
// - conv: structural comparison for Pi/Universe, reference equality for generic lambdas

import { describe, it, expect } from "vitest"

// ============================================================
// Val representation
// ============================================================

type Val = VLeaf | VStem | VFork | VLam | VHyp | VStuck

interface VLeaf { tag: "leaf" }
interface VStem { tag: "stem"; child: Val }
interface VFork { tag: "fork"; left: Val; right: Val }
interface VLam { tag: "lam"; metadata: Val | null; body: (x: Val) => Val }
interface VHyp { tag: "hyp"; storedType: Val; id: number }
interface VStuck { tag: "stuck"; head: Val; arg: Val }

const TT: Val = { tag: "leaf" }
const FF: Val = { tag: "stem", child: { tag: "leaf" } }

// ============================================================
// Fresh hypothesis counter
// ============================================================

let _nextHypId = 0
function resetHypCounter() { _nextHypId = 0 }
function freshHyp(type: Val): VHyp {
  return { tag: "hyp", storedType: type, id: _nextHypId++ }
}

// ============================================================
// Structural Val equality (like hash-cons fast_eq)
// ============================================================

function valEq(a: Val, b: Val): boolean {
  if (a === b) return true
  if (a.tag !== b.tag) return false
  switch (a.tag) {
    case "leaf": return true
    case "stem": return valEq(a.child, (b as VStem).child)
    case "fork": return valEq(a.left, (b as VFork).left) && valEq(a.right, (b as VFork).right)
    case "lam": return false // reference equality only; real impl uses hash-cons
    case "hyp": return a.id === (b as VHyp).id
    case "stuck":
      return valEq(a.head, (b as VStuck).head) && valEq(a.arg, (b as VStuck).arg)
  }
}

// ============================================================
// Metadata tags and reflection
// ============================================================

const PI_TAG: Val = { tag: "stem", child: { tag: "stem", child: { tag: "leaf" } } }
const UNIV_TAG: Val = { tag: "fork", left: { tag: "leaf" }, right: { tag: "stem", child: { tag: "leaf" } } }

function isPi(v: Val): boolean {
  return v.tag === "lam" && v.metadata !== null
    && v.metadata.tag === "fork" && valEq(v.metadata.left, PI_TAG)
}

function piDom(v: Val): Val {
  return ((v as VLam).metadata as VFork).right.tag === "fork"
    ? ((v as VLam).metadata as VFork).right.left
    : (() => { throw new Error("piDom: malformed metadata") })()
}

function piCodVal(v: Val): Val {
  const r = ((v as VLam).metadata as VFork).right
  if (r.tag !== "fork") throw new Error("piCodVal: malformed metadata")
  return r.right
}

function piCodFn(v: Val): (arg: Val) => Val {
  const cod = piCodVal(v)
  if (cod.tag !== "lam") throw new Error("piCodFn: codomain is not a lambda")
  return cod.body
}

function isUniverse(v: Val): boolean {
  return v.tag === "lam" && v.metadata !== null
    && v.metadata.tag === "fork" && valEq(v.metadata.left, UNIV_TAG)
}

function universeRank(v: Val): number {
  return valToNat(((v as VLam).metadata as VFork).right)
}

function isNeutral(v: Val): boolean {
  return v.tag === "hyp" || v.tag === "stuck"
}

// ============================================================
// Nat encoding: Zero = leaf, Succ n = fork(leaf, n)
// ============================================================

const Zero: Val = { tag: "leaf" }
function Succ(n: Val): Val { return { tag: "fork", left: { tag: "leaf" }, right: n } }
function natToVal(n: number): Val { return n === 0 ? Zero : Succ(natToVal(n - 1)) }
function valToNat(v: Val): number {
  if (v.tag === "leaf") return 0
  if (v.tag === "fork" && v.left.tag === "leaf") return 1 + valToNat(v.right)
  throw new Error("valToNat: not a nat")
}

// ============================================================
// type_of_neutral — spine inference
// ============================================================

function typeOfNeutral(v: Val): Val {
  if (v.tag === "hyp") return v.storedType
  if (v.tag === "stuck") {
    const headType = typeOfNeutral(v.head)
    if (!isPi(headType)) throw new Error("typeOfNeutral: head is not Pi-typed")
    return piCodFn(headType)(v.arg)
  }
  throw new Error("typeOfNeutral: not a neutral")
}

// ============================================================
// conv — semantic equality
// ============================================================

function conv(a: Val, b: Val): boolean {
  if (a === b) return true
  if (valEq(a, b)) return true

  if (isPi(a) && isPi(b)) {
    if (!conv(piDom(a), piDom(b))) return false
    const h = freshHyp(piDom(a))
    return conv(piCodFn(a)(h), piCodFn(b)(h))
  }

  if (isUniverse(a) && isUniverse(b)) {
    return universeRank(a) === universeRank(b)
  }

  // Generic lambdas: reference equality only (real impl: hash-cons)
  return false
}

// ============================================================
// napply — the evaluator
// ============================================================

function napply(f: Val, x: Val): Val {
  // H-rule: if argument is neutral, check if its type matches the predicate
  if (isNeutral(x)) {
    try {
      const xType = typeOfNeutral(x)
      if (conv(f, xType)) return TT
    } catch (_) {
      // typeOfNeutral failed (malformed spine) — fall through
    }
  }

  if (f.tag === "hyp") return { tag: "stuck", head: f, arg: x }
  if (f.tag === "stuck") return { tag: "stuck", head: f, arg: x }
  if (f.tag === "lam") return f.body(x)
  if (f.tag === "leaf") return { tag: "stem", child: x }
  if (f.tag === "stem") return { tag: "fork", left: f.child, right: x }
  if (f.tag === "fork") return napply3(f.left, f.right, x)
  throw new Error(`napply: unhandled ${(f as any).tag}`)
}

function napply3(a: Val, b: Val, x: Val): Val {
  if (a.tag === "leaf") return b
  if (a.tag === "stem") return napply(napply(a.child, x), napply(b, x))
  if (a.tag === "fork") {
    if (x.tag === "leaf") return a.left
    if (x.tag === "stem") return napply(a.right, x.child)
    if (x.tag === "fork") return napply(napply(b, x.left), x.right)
    // x is neutral — stuck triage (shouldn't happen for tagged-term backends)
    return { tag: "stuck", head: { tag: "fork", left: a, right: b }, arg: x }
  }
  if (isNeutral(a)) {
    return { tag: "stuck", head: { tag: "stuck", head: a, arg: b }, arg: x }
  }
  throw new Error(`napply3: unhandled ${a.tag}`)
}

// ============================================================
// Type constructors
// ============================================================

// Pi constructor: produces a VLam with PI_TAG metadata
function mkPi(domain: Val, codomainFn: Val): Val {
  const metadata: Val = {
    tag: "fork",
    left: PI_TAG,
    right: { tag: "fork", left: domain, right: codomainFn }
  }
  const piVal: Val = {
    tag: "lam",
    metadata,
    body: (f: Val) => {
      // napply's H-rule handles neutral f with exact type match.
      // For non-neutral f: run checking logic.
      const a = freshHyp(domain)
      const result = napply(f, a)
      const codType = napply(codomainFn, a)
      return napply(codType, result)
    }
  }
  return piVal
}

// Non-dependent arrow: A -> B
function mkArrow(a: Val, b: Val): Val {
  return mkPi(a, { tag: "lam", metadata: null, body: (_: Val) => b })
}

// Registry for base types
const registry: Val[] = []
function registerBaseType(t: Val) { registry.push(t) }
function isRegistered(t: Val): boolean { return registry.some(r => r === t) }

// Universe constructor
function mkUniverse(rank: number): Val {
  const metadata: Val = {
    tag: "fork",
    left: UNIV_TAG,
    right: natToVal(rank)
  }
  const univVal: Val = {
    tag: "lam",
    metadata,
    body: (t: Val) => {
      // Neutral with universe-typed stored type: cumulativity
      if (isNeutral(t)) {
        try {
          const tType = typeOfNeutral(t)
          if (isUniverse(tType) && universeRank(tType) <= rank) return TT
        } catch (_) { /* fall through */ }
        return FF
      }
      // Case 1: universe below rank
      if (isUniverse(t) && universeRank(t) < rank) return TT
      // Case 2: Pi with components at rank ≤ n
      if (isPi(t)) {
        const domCheck = napply(univVal, piDom(t))
        if (!valEq(domCheck, TT)) return FF
        const a = freshHyp(piDom(t))
        return napply(univVal, piCodFn(t)(a))
      }
      // Case 3: registered base type
      if (isRegistered(t)) return TT
      return FF
    }
  }
  return univVal
}

// ============================================================
// Data type predicates
// ============================================================

const Nat: Val = {
  tag: "lam",
  metadata: null,
  body: function natBody(n: Val): Val {
    // napply's H-rule handles neutrals with type === Nat before we get here
    if (valEq(n, Zero)) return TT
    if (n.tag === "fork" && valEq(n.left, Zero)) {
      return napply(Nat, n.right) // recurse on predecessor
    }
    return FF
  }
}
registerBaseType(Nat)

const Bool: Val = {
  tag: "lam",
  metadata: null,
  body: (b: Val) => {
    if (valEq(b, TT) || valEq(b, FF)) return TT
    return FF
  }
}
registerBaseType(Bool)

// ============================================================
// check helper
// ============================================================

function check(type: Val, value: Val): boolean {
  return valEq(napply(type, value), TT)
}

// ============================================================
// Tests
// ============================================================

describe("NbE Design Validation", () => {
  const Type0 = mkUniverse(0)
  const Type1 = mkUniverse(1)

  describe("Base type checking", () => {
    it("zero : Nat", () => {
      expect(check(Nat, Zero)).toBe(true)
    })

    it("3 : Nat", () => {
      expect(check(Nat, natToVal(3))).toBe(true)
    })

    it("FF !: Nat", () => {
      expect(check(Nat, FF)).toBe(false)
    })

    it("TT : Bool", () => {
      expect(check(Bool, TT)).toBe(true)
    })

    it("FF : Bool", () => {
      expect(check(Bool, FF)).toBe(true)
    })

    it("Succ(Zero) !: Bool", () => {
      expect(check(Bool, Succ(Zero))).toBe(false)
    })
  })

  describe("Arrow types (non-dependent Pi)", () => {
    const NatToNat = mkArrow(Nat, Nat)

    it("id : Nat -> Nat", () => {
      const id: Val = { tag: "lam", metadata: null, body: x => x }
      expect(check(NatToNat, id)).toBe(true)
    })

    it("succ : Nat -> Nat", () => {
      const succ: Val = { tag: "lam", metadata: null, body: x => Succ(x) }
      expect(check(NatToNat, succ)).toBe(true)
    })

    it("const zero : Nat -> Nat", () => {
      const k0: Val = { tag: "lam", metadata: null, body: _ => Zero }
      expect(check(NatToNat, k0)).toBe(true)
    })

    it("const FF !: Nat -> Nat", () => {
      const bad: Val = { tag: "lam", metadata: null, body: _ => FF }
      expect(check(NatToNat, bad)).toBe(false)
    })
  })

  describe("Higher-order functions (stuck applications)", () => {
    it("{f,x} -> f x : (Nat->Nat) -> Nat -> Nat", () => {
      const type = mkArrow(mkArrow(Nat, Nat), mkArrow(Nat, Nat))
      const term: Val = {
        tag: "lam", metadata: null,
        body: f => ({ tag: "lam", metadata: null, body: x => napply(f, x) })
      }
      expect(check(type, term)).toBe(true)
    })

    it("{f,x} -> f (f x) : (Nat->Nat) -> Nat -> Nat", () => {
      const type = mkArrow(mkArrow(Nat, Nat), mkArrow(Nat, Nat))
      const term: Val = {
        tag: "lam", metadata: null,
        body: f => ({
          tag: "lam", metadata: null,
          body: x => napply(f, napply(f, x))
        })
      }
      expect(check(type, term)).toBe(true)
    })

    it("{f,g,x} -> f (g x) : compose Nat", () => {
      const type = mkArrow(mkArrow(Nat, Nat), mkArrow(mkArrow(Nat, Nat), mkArrow(Nat, Nat)))
      const term: Val = {
        tag: "lam", metadata: null,
        body: f => ({
          tag: "lam", metadata: null,
          body: g => ({
            tag: "lam", metadata: null,
            body: x => napply(f, napply(g, x))
          })
        })
      }
      expect(check(type, term)).toBe(true)
    })
  })

  describe("Polymorphic identity (dependent Pi)", () => {
    it("{A : Type 0} -> {x : A} -> x : poly id type", () => {
      const polyIdType = mkPi(Type0, {
        tag: "lam", metadata: null,
        body: A => mkArrow(A, A)
      })
      const polyId: Val = {
        tag: "lam", metadata: null,
        body: _A => ({ tag: "lam", metadata: null, body: x => x })
      }
      expect(check(polyIdType, polyId)).toBe(true)
    })

    it("{A : Type 0} -> {x : A} -> {y : A} -> x : first projection", () => {
      const type = mkPi(Type0, {
        tag: "lam", metadata: null,
        body: A => mkArrow(A, mkArrow(A, A))
      })
      const term: Val = {
        tag: "lam", metadata: null,
        body: _A => ({
          tag: "lam", metadata: null,
          body: x => ({ tag: "lam", metadata: null, body: _y => x })
        })
      }
      expect(check(type, term)).toBe(true)
    })
  })

  describe("Dependent function application", () => {
    it("{A,B} -> {f : A->B} -> {x : A} -> f x : dependent apply", () => {
      // {A : Type 0} -> {B : Type 0} -> (A -> B) -> A -> B
      const type = mkPi(Type0, {
        tag: "lam", metadata: null,
        body: A => mkPi(Type0, {
          tag: "lam", metadata: null,
          body: B => mkArrow(mkArrow(A, B), mkArrow(A, B))
        })
      })
      const term: Val = {
        tag: "lam", metadata: null,
        body: _A => ({
          tag: "lam", metadata: null,
          body: _B => ({
            tag: "lam", metadata: null,
            body: f => ({
              tag: "lam", metadata: null,
              body: x => napply(f, x)
            })
          })
        })
      }
      expect(check(type, term)).toBe(true)
    })
  })

  describe("Universe checking", () => {
    it("Nat : Type 0", () => {
      expect(check(Type0, Nat)).toBe(true)
    })

    it("Bool : Type 0", () => {
      expect(check(Type0, Bool)).toBe(true)
    })

    it("Nat -> Nat : Type 0", () => {
      expect(check(Type0, mkArrow(Nat, Nat))).toBe(true)
    })

    it("Type 0 : Type 1", () => {
      expect(check(Type1, Type0)).toBe(true)
    })

    it("Type 0 !: Type 0 (no Type-in-Type)", () => {
      expect(check(Type0, Type0)).toBe(false)
    })

    it("{A:Type 0} -> A -> A : Type 1", () => {
      const polyIdType = mkPi(Type0, {
        tag: "lam", metadata: null,
        body: A => mkArrow(A, A)
      })
      expect(check(Type1, polyIdType)).toBe(true)
    })
  })

  describe("Cumulativity", () => {
    it("Nat : Type 1 (cumulative)", () => {
      expect(check(mkUniverse(1), Nat)).toBe(true)
    })

    it("Nat -> Nat : Type 1 (cumulative)", () => {
      expect(check(mkUniverse(1), mkArrow(Nat, Nat))).toBe(true)
    })

    it("Type 0 : Type 2 (cumulative)", () => {
      expect(check(mkUniverse(2), Type0)).toBe(true)
    })
  })

  describe("Rejection of ill-typed terms", () => {
    it("rejects self-application: {A} -> {x:A} -> x x", () => {
      const polyIdType = mkPi(Type0, {
        tag: "lam", metadata: null,
        body: A => mkArrow(A, A)
      })
      const selfApp: Val = {
        tag: "lam", metadata: null,
        body: _A => ({
          tag: "lam", metadata: null,
          body: x => napply(x, x)
        })
      }
      expect(check(polyIdType, selfApp)).toBe(false)
    })

    it("rejects wrong return type: {x:Nat} -> FF as Nat->Nat", () => {
      const bad: Val = { tag: "lam", metadata: null, body: _ => FF }
      expect(check(mkArrow(Nat, Nat), bad)).toBe(false)
    })
  })

  describe("conv on types", () => {
    it("conv(Nat->Nat, Nat->Nat) across different mkArrow calls", () => {
      const t1 = mkArrow(Nat, Nat)
      const t2 = mkArrow(Nat, Nat)
      expect(conv(t1, t2)).toBe(true)
    })

    it("conv(Nat->Nat, Nat->Bool) = false", () => {
      expect(conv(mkArrow(Nat, Nat), mkArrow(Nat, Bool))).toBe(false)
    })

    it("conv(Nat->Bool, Bool->Nat) = false", () => {
      expect(conv(mkArrow(Nat, Bool), mkArrow(Bool, Nat))).toBe(false)
    })

    it("conv(Type 0, Type 0) across different mkUniverse calls", () => {
      expect(conv(mkUniverse(0), mkUniverse(0))).toBe(true)
    })

    it("conv(Type 0, Type 1) = false", () => {
      expect(conv(mkUniverse(0), mkUniverse(1))).toBe(false)
    })
  })

  describe("Dependent type with indexed families", () => {
    it("{F : Nat -> Type 0} -> {n : Nat} -> F n -> F n", () => {
      const FType = mkArrow(Nat, Type0)
      const fullType = mkPi(FType, {
        tag: "lam", metadata: null,
        body: F => mkPi(Nat, {
          tag: "lam", metadata: null,
          body: n => mkArrow(napply(F, n), napply(F, n))
        })
      })
      const term: Val = {
        tag: "lam", metadata: null,
        body: _F => ({
          tag: "lam", metadata: null,
          body: _n => ({
            tag: "lam", metadata: null,
            body: x => x
          })
        })
      }
      expect(check(fullType, term)).toBe(true)
    })
  })

  // ==================================================================
  // Raw-predicate bootstrapping validation
  //
  // These tests verify the key property: type predicates are ordinary
  // functions. The caller does apply(type, value) — no eval, no special
  // infrastructure. The predicate bootstraps Val-land internally when
  // it needs hypothetical reasoning.
  // ==================================================================

  describe("Raw-predicate bootstrapping", () => {
    it("concrete data: napply handles untagged leaf/fork directly", () => {
      // Nat predicate applied to raw data — no hypotheses, no H-rule.
      // napply dispatches via VLam → body, body does raw pattern matching.
      const result = napply(Nat, Succ(Succ(Zero)))
      expect(valEq(result, TT)).toBe(true)
    })

    it("napply mixes raw data and tagged hypotheses seamlessly", () => {
      // Apply a raw function to a tagged hypothesis.
      // napply(VLam, VHyp) → body(VHyp). The body receives the hypothesis.
      const h = freshHyp(Nat)
      const raw_succ: Val = { tag: "lam", metadata: null, body: (x: Val) => Succ(x) }
      const result = napply(raw_succ, h)
      // Result: fork(leaf, h) — raw fork wrapping a tagged hypothesis
      expect(result.tag).toBe("fork")
      expect(valEq((result as VFork).left, Zero)).toBe(true)
      expect((result as VFork).right).toBe(h) // same object
    })

    it("predicate checks hybrid raw+tagged value", () => {
      // Nat predicate on fork(leaf, hypothesis) — raw structure with tagged interior
      const h = freshHyp(Nat)
      const hybrid = Succ(h) // fork(leaf, VHyp)
      // Nat pattern-matches the fork: first=leaf ✓, recurse on h.
      // H-rule fires on h: conv(Nat, type_of_hyp(h)) = conv(Nat, Nat) = TT.
      expect(check(Nat, hybrid)).toBe(true)
    })

    it("Pi predicate bootstraps Val-land on a raw function", () => {
      // Pi receives a raw function (VLam). Internally:
      // 1. Creates fresh_hyp (tagged)
      // 2. napply(raw_fn, hyp) — raw fn applied to tagged hyp
      // 3. Checks result against codomain
      // 4. Returns TT/FF (raw)
      // The caller did nothing special — napply handled the boundary.
      const NatToNat = mkArrow(Nat, Nat)
      const raw_fn: Val = { tag: "lam", metadata: null, body: x => x }
      expect(check(NatToNat, raw_fn)).toBe(true)
    })

    it("hypothesis-as-type: H-rule bridges raw and tagged", () => {
      // h_A is a hypothesis used AS A TYPE (the predicate).
      // h_x is a hypothesis of type h_A.
      // napply(h_A, h_x): H-rule checks conv(h_A, type_of_neutral(h_x)) = conv(h_A, h_A) = TT.
      // No Val-land bootstrap needed — the H-rule handles it directly.
      const h_A = freshHyp(mkUniverse(0))
      const h_x = freshHyp(h_A)
      expect(valEq(napply(h_A, h_x), TT)).toBe(true)
    })

    it("stuck application type inferred across raw/tagged boundary", () => {
      // h_f : Nat -> Nat (tagged hypothesis)
      // napply(h_f, Zero) where Zero is raw data
      // Result: VStuck(h_f, Zero) — tagged head, raw arg
      // type_of_neutral infers: pi_cod(Nat->Nat) applied to Zero = Nat
      // Then: napply(Nat, stuck) → H-rule: conv(Nat, Nat) = TT
      const NatToNat = mkArrow(Nat, Nat)
      const h_f = freshHyp(NatToNat)
      const stuck = napply(h_f, Zero)
      expect(stuck.tag).toBe("stuck")

      // The stuck value passes the Nat check via H-rule + type_of_neutral
      expect(check(Nat, stuck)).toBe(true)
    })

    it("end-to-end: complex type on raw term, no explicit Val conversion", () => {
      // Type: (Nat -> Nat) -> Nat -> Nat
      // Term: {f, x} -> f (f x) — raw function, no annotations
      // The Pi predicate bootstraps Val-land at each binder,
      // raw functions seamlessly interact with hypotheses.
      const type = mkArrow(mkArrow(Nat, Nat), mkArrow(Nat, Nat))
      const term: Val = {
        tag: "lam", metadata: null,
        body: f => ({
          tag: "lam", metadata: null,
          body: x => napply(f, napply(f, x))
        })
      }
      expect(check(type, term)).toBe(true)
    })
  })
})
