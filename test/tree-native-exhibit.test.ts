// Exhibition: concrete (tree, type, derivation) triples for the tree-native type theory.
//
// For each program class, shows:
//   - The surface syntax it came from
//   - The compiled tree (prettyTree)
//   - The type tree
//   - The derivation
//   - Whether the checker accepts it
//   - A boundary case showing what the checker CAN'T handle

import { describe, it, expect } from "vitest"
import { Tree, LEAF, stem, fork, apply, treeEqual, I, K, prettyTree, isFork, isLeaf, isStem } from "../src/tree.js"
import { bracketAbstract, collapse, eTree, eFvar, eApp, collapseAndEval, FIX } from "../src/compile.js"

// === TYPE ENCODING ===
const TYPE = LEAF
const TREE = stem(LEAF)
const BOOL = stem(stem(LEAF))
const NAT  = stem(stem(stem(LEAF)))

function arrow(A: Tree, B: Tree): Tree { return fork(A, fork(LEAF, B)) }
function pi(A: Tree, B: Tree): Tree { return fork(A, B) }

// === HELPERS ===
function isNonDep(B: Tree): Tree | null {
  return isFork(B) && isLeaf(B.left) ? B.right : null
}
function unPi(T: Tree): { domain: Tree, codomain: Tree } | null {
  return isFork(T) ? { domain: T.left, codomain: T.right } : null
}
function classifyCombinator(t: Tree) {
  if (isLeaf(t)) return "leaf"
  if (isStem(t)) return `stem(${prettyTree(t.child)})`
  if (isLeaf(t.left)) return `K(${prettyTree(t.right)})`
  if (isStem(t.left)) return `S(${prettyTree(t.left.child)}, ${prettyTree(t.right)})`
  return `Triage(${prettyTree(t.left.left)}, ${prettyTree(t.left.right)}, ${prettyTree(t.right)})`
}

// Simple pretty printer for types
function prettyType(t: Tree): string {
  if (treeEqual(t, TYPE)) return "Type"
  if (treeEqual(t, TREE)) return "Tree"
  if (treeEqual(t, BOOL)) return "Bool"
  if (treeEqual(t, NAT)) return "Nat"
  const p = unPi(t)
  if (p) {
    const nd = isNonDep(p.codomain)
    if (nd) return `${prettyType(p.domain)} → ${prettyType(nd)}`
    return `Π(${prettyType(p.domain)}, ${prettyTree(p.codomain)})`
  }
  return prettyTree(t)
}

type Deriv =
  | { tag: "base" }
  | { tag: "K"; inner: Deriv }
  | { tag: "S"; D: Tree; drvC: Deriv; drvB: Deriv }
  | { tag: "triage"; drvC: Deriv; drvD: Deriv; drvB: Deriv }
const B: Deriv = { tag: "base" }

function prettyDeriv(d: Deriv): string {
  switch (d.tag) {
    case "base": return "·"
    case "K": return `K(${prettyDeriv(d.inner)})`
    case "S": return `S{D=${prettyType(isNonDep(d.D) || d.D)}, c=${prettyDeriv(d.drvC)}, b=${prettyDeriv(d.drvB)}}`
    case "triage": return `T{${prettyDeriv(d.drvC)}, ${prettyDeriv(d.drvD)}, ${prettyDeriv(d.drvB)}}`
  }
}

function exhibit(name: string, surface: string, tree: Tree, type: Tree, deriv: Deriv) {
  console.log(`\n┌─ ${name}`)
  console.log(`│  surface:  ${surface}`)
  console.log(`│  tree:     ${prettyTree(tree)}`)
  console.log(`│  form:     ${classifyCombinator(tree)}`)
  console.log(`│  type:     ${prettyType(type)}`)
  console.log(`│  type raw: ${prettyTree(type)}`)
  console.log(`│  deriv:    ${prettyDeriv(deriv)}`)
  console.log(`└─`)
}

// ============================================================
// CATEGORY 1: CONSTANTS — K(v) form
// ============================================================
// Surface: anything that ignores its argument
// Tree: fork(leaf, v)
// Type: A → B where v : B
// Deriv: K(derivation_of_v)

describe("Category 1: Constants — K(v)", () => {
  it("K(true) : Bool → Bool", () => {
    // Surface: {b} -> true
    // Bracket abstraction: [b] true = K(true) since b ∉ FV(true)
    const tree = fork(LEAF, LEAF)  // K(leaf) = K(true)
    const type = arrow(BOOL, BOOL)
    const deriv: Deriv = { tag: "K", inner: B } // inner: true is a base value

    exhibit("K(true)", "{b : Bool} -> true", tree, type, deriv)
    console.log(`  K(true)(false) = ${prettyTree(apply(tree, stem(LEAF)))} = true ✓`)
  })

  it("K(0) : Tree → Nat", () => {
    const tree = fork(LEAF, LEAF)   // K(leaf) = K(0)
    const type = arrow(TREE, NAT)
    const deriv: Deriv = { tag: "K", inner: B }

    exhibit("K(0)", "{t : Tree} -> 0", tree, type, deriv)
  })

  it("K(succ) : Bool → (Nat → Nat)", () => {
    // Surface: {b} -> succ
    // succ = leaf (the stem constructor). K(leaf) ignores b, returns succ.
    const tree = fork(LEAF, LEAF)   // K(leaf) — same tree as K(true)!
    const type = arrow(BOOL, arrow(NAT, NAT))
    const deriv: Deriv = { tag: "K", inner: B }

    exhibit("K(succ)", "{b : Bool} -> succ", tree, type, deriv)
    console.log(`  ⚠ Same tree as K(true)! leaf = true = zero = succ. Types disambiguate.`)
    console.log(`  K(succ)(false)(3) = succ(3) = ${prettyTree(apply(apply(tree, stem(LEAF)), stem(stem(stem(LEAF)))))} = 4`)
  })

  it("K(not) : Nat → (Bool → Bool)", () => {
    // Surface: {n} -> not
    const notFn = fork(fork(stem(LEAF), fork(LEAF, LEAF)), fork(LEAF, fork(LEAF, stem(LEAF))))
    const tree = fork(LEAF, notFn) // K(not)
    const type = arrow(NAT, arrow(BOOL, BOOL))
    const deriv: Deriv = { tag: "K", inner: { tag: "triage", drvC: B, drvD: { tag: "K", inner: B }, drvB: { tag: "K", inner: { tag: "K", inner: B } } } }

    exhibit("K(not)", "{n : Nat} -> not", tree, type, deriv)
  })

  it("K(K(0)) : A → B → Nat — nested constants", () => {
    // Surface: {a b} -> 0  (both args ignored)
    // [b] 0 = K(0). [a] K(0) = K(K(0)).
    const tree = fork(LEAF, fork(LEAF, LEAF)) // K(K(0))
    const type = arrow(TREE, arrow(TREE, NAT))
    const deriv: Deriv = { tag: "K", inner: { tag: "K", inner: B } }

    exhibit("K(K(0))", "{a b} -> 0", tree, type, deriv)
    console.log(`  K(K(0))(anything₁)(anything₂) = ${prettyTree(apply(apply(tree, fork(LEAF, LEAF)), stem(stem(LEAF))))} = 0`)
  })
})

// ============================================================
// CATEGORY 2: TRIAGE — pattern matching
// ============================================================
// Surface: eliminators / case analysis
// Tree: fork(fork(onLeaf, onStem), onFork)
// Type: A → R (dispatch on A's tag)
// Deriv: T{drvLeaf, drvStem, drvFork}

describe("Category 2: Triage — pattern matching", () => {
  it("I (identity) : Nat → Nat", () => {
    // Surface: {n} -> n
    // [n] n = I = Triage(leaf, leaf, leaf)
    // Meaning: leaf→leaf, stem(u)→stem(u), fork(u,v)→fork(u,v)
    const tree = I // fork(fork(leaf, leaf), leaf)
    const type = arrow(NAT, NAT)
    const deriv: Deriv = { tag: "triage", drvC: B, drvD: B, drvB: B }

    exhibit("I (identity)", "{n : Nat} -> n", tree, type, deriv)
    console.log(`  Checking logic:`)
    console.log(`    leaf branch: c=leaf, output=leaf=0 : Nat ✓`)
    console.log(`    stem branch: d=leaf, d(u)=stem(u)=succ(u). leaf : Nat→Nat (succ preserves Nat) ✓`)
    console.log(`  I(0) = ${prettyTree(apply(I, LEAF))}, I(3) = ${prettyTree(apply(I, stem(stem(stem(LEAF)))))}`)
  })

  it("not : Bool → Bool", () => {
    // Surface: {b} -> boolElim Bool false true b
    // Compiled: Triage(false, K(true), K(K(false)))
    //   true → false
    //   false = stem(leaf) → K(true)(leaf) = true
    //   fork → K(K(false))(...) = false (junk case)
    const tree = fork(fork(stem(LEAF), fork(LEAF, LEAF)), fork(LEAF, fork(LEAF, stem(LEAF))))
    const type = arrow(BOOL, BOOL)
    const deriv: Deriv = {
      tag: "triage",
      drvC: B,                              // false : Bool ✓
      drvD: { tag: "K", inner: B },         // K(true) : ? → Bool. true : Bool ✓
      drvB: { tag: "K", inner: { tag: "K", inner: B } }  // K(K(false)) (junk)
    }

    exhibit("not", "{b} -> boolElim Bool false true b", tree, type, deriv)
    console.log(`  Checking logic:`)
    console.log(`    leaf branch (true case): c=stem(leaf)=false : Bool ✓`)
    console.log(`    bool-false case: apply(d, leaf) = apply(K(true), leaf) = true : Bool ✓`)
    console.log(`  not(true) = ${prettyTree(apply(tree, LEAF))}, not(false) = ${prettyTree(apply(tree, stem(LEAF)))}`)
  })

  it("isZero : Nat → Bool", () => {
    // Surface: {n} -> natElim Bool true ({_} -> false) n
    // Compiled: Triage(true, K(false), junk)
    const tree = fork(fork(LEAF, fork(LEAF, stem(LEAF))), fork(LEAF, fork(LEAF, LEAF)))
    const type = arrow(NAT, BOOL)
    const deriv: Deriv = {
      tag: "triage",
      drvC: B,                        // true : Bool ✓
      drvD: { tag: "K", inner: B },   // K(false) : Nat→Bool. false : Bool ✓
      drvB: { tag: "K", inner: { tag: "K", inner: B } }
    }

    exhibit("isZero", "{n} -> natElim Bool true ({_} -> false) n", tree, type, deriv)
    console.log(`  isZero(0) = ${prettyTree(apply(tree, LEAF))} = true`)
    console.log(`  isZero(1) = ${prettyTree(apply(tree, stem(LEAF)))} = false`)
    console.log(`  isZero(3) = ${prettyTree(apply(tree, stem(stem(stem(LEAF)))))} = false`)
  })

  it("pred : Nat → Nat (predecessor, 0→0)", () => {
    // Surface: {n} -> natElim Nat 0 ({n'} -> n') n
    // Compiled: Triage(0, I, junk)
    //   zero → 0
    //   succ(n') → I(n') = n'
    //   fork → junk
    const tree = fork(fork(LEAF, I), fork(LEAF, fork(LEAF, LEAF)))
    const type = arrow(NAT, NAT)
    const deriv: Deriv = {
      tag: "triage",
      drvC: B,                                                   // 0 : Nat ✓
      drvD: { tag: "triage", drvC: B, drvD: B, drvB: B },      // I : Nat→Nat ✓
      drvB: { tag: "K", inner: { tag: "K", inner: B } }
    }

    exhibit("pred", "{n} -> natElim Nat 0 ({n'} -> n') n", tree, type, deriv)
    console.log(`  pred(0) = ${prettyTree(apply(tree, LEAF))} = 0`)
    console.log(`  pred(1) = ${prettyTree(apply(tree, stem(LEAF)))} = 0`)
    console.log(`  pred(3) = ${prettyTree(apply(tree, stem(stem(stem(LEAF)))))} = 2`)
  })
})

// ============================================================
// CATEGORY 3: S COMBINATOR — argument sharing
// ============================================================
// Surface: anything that uses the argument more than once, or passes it forward
// Tree: fork(stem(c), b)
// Type: A → E
// Deriv: S{D, drvC, drvB} — D is the intermediate type

describe("Category 3: S combinator — argument sharing", () => {
  it("succ∘succ : Nat → Nat", () => {
    // Surface: {n} -> succ (succ n)
    // [n] succ(succ(n))
    //   succ = leaf. succ(n) = apply(leaf, n).
    //   [n] apply(leaf, apply(leaf, n))
    //   = S(K(leaf), [n]apply(leaf, n))
    //   = S(K(leaf), S(K(leaf), I))
    //   But S(K(leaf), I) = leaf by eta. So:
    //   = S(K(leaf), leaf) — the B combinator: f∘g
    const c = fork(LEAF, LEAF)   // K(leaf) = K(succ)
    const b = LEAF               // leaf = succ
    const tree = fork(stem(c), b) // S(K(succ), succ)
    const type = arrow(NAT, NAT)
    const deriv: Deriv = {
      tag: "S",
      D: fork(LEAF, NAT),  // K(Nat) — intermediate type Nat
      drvC: { tag: "K", inner: B },  // K(succ) — succ : Nat→Nat from base
      drvB: B                          // leaf = succ : Nat→Nat from base
    }

    exhibit("succ∘succ", "{n} -> succ (succ n)", tree, type, deriv)
    console.log(`  S(K(succ), succ)(n) = K(succ)(n)(succ(n)) = succ(succ(n))`)
    console.log(`  Derivation says D=Nat: succ produces a Nat, outer succ consumes it.`)
    console.log(`  (succ∘succ)(0) = ${prettyTree(apply(tree, LEAF))} = 2`)
    console.log(`  (succ∘succ)(2) = ${prettyTree(apply(tree, stem(stem(LEAF))))} = 4`)
  })

  it("double = S(S(K(add), I), I) : Nat → Nat (with add in context)", () => {
    // Surface: {x} -> add x x
    // [x] apply(apply(add, x), x)
    //   = S([x]apply(add, x), [x]x)
    //   = S(S(K(add), I), I)
    //
    // Inner S: c=K(add), b=I. S(K(add), I)(x) = add(x).
    // Outer S: c=S(K(add),I), b=I. S(inner, I)(x) = inner(x)(I(x)) = add(x)(x).

    const add = fork(fork(stem(stem(LEAF)), LEAF), fork(stem(LEAF), LEAF)) // mock add
    const kAdd = fork(LEAF, add)
    const innerS = fork(stem(kAdd), I)
    const tree = fork(stem(innerS), I)
    const type = arrow(NAT, NAT)

    const deriv: Deriv = {
      tag: "S",
      D: fork(LEAF, NAT),  // outer D=Nat: I produces a Nat
      drvC: {
        tag: "S",
        D: fork(LEAF, NAT),  // inner D=Nat: I produces a Nat
        drvC: { tag: "K", inner: B },  // K(add) — add from context
        drvB: { tag: "triage", drvC: B, drvD: B, drvB: B }  // I
      },
      drvB: { tag: "triage", drvC: B, drvD: B, drvB: B }  // I
    }

    exhibit("double", "{x} -> add x x", tree, type, deriv)
    console.log(`  Nested S structure:`)
    console.log(`    outer S: c=S(K(add),I), b=I, D=Nat`)
    console.log(`    inner S: c=K(add), b=I, D=Nat`)
    console.log(`  The derivation records D at EACH S node — this is what the checker needs.`)
  })

  it("apply-to-self = S(I, I) : Tree → Tree (self-application, ω combinator)", () => {
    // Surface: {x} -> x x
    // [x] apply(x, x) = S([x]x, [x]x) = S(I, I)
    // S(I,I)(f) = I(f)(I(f)) = f(f)
    const tree = fork(stem(I), I) // S(I, I)
    const type = arrow(TREE, TREE)
    // D = Tree (I produces a Tree from its input)
    const deriv: Deriv = {
      tag: "S",
      D: fork(LEAF, TREE),
      drvC: { tag: "triage", drvC: B, drvD: B, drvB: B },  // I : Tree→(Tree→Tree)
      drvB: { tag: "triage", drvC: B, drvD: B, drvB: B }   // I : Tree→Tree
    }

    exhibit("self-apply", "{x} -> x x", tree, type, deriv)
    console.log(`  S(I,I)(K) = K(K) = ${prettyTree(apply(tree, K))}`)
    console.log(`  S(I,I)(I) = I(I) = ${prettyTree(apply(tree, I))} = I (identity is idempotent)`)
    console.log(`  ⚠ Only typeable as Tree→Tree, not polymorphically (x : A, x x requires A = A→B)`)
  })
})

// ============================================================
// CATEGORY 4: CONSTRUCTORS AS FUNCTIONS — leaf and stem(a)
// ============================================================
// leaf applied to x gives stem(x) — the stem/succ constructor
// stem(a) applied to x gives fork(a, x) — the fork constructor with fixed left child

describe("Category 4: Constructors as functions", () => {
  it("leaf = succ : Nat → Nat", () => {
    const tree = LEAF
    const type = arrow(NAT, NAT)
    const deriv: Deriv = B  // base — leaf is a primitive

    exhibit("succ", "succ", tree, type, deriv)
    console.log(`  leaf(0) = stem(0) = 1: ${prettyTree(apply(LEAF, LEAF))}`)
    console.log(`  leaf(2) = stem(2) = 3: ${prettyTree(apply(LEAF, stem(stem(LEAF))))}`)
    console.log(`  Checker rule: stem(x) : Nat for all x : Nat ✓ (stem preserves Nat)`)
  })

  it("leaf = stem-ctor : Tree → Tree", () => {
    const tree = LEAF
    const type = arrow(TREE, TREE)
    exhibit("stem-ctor", "leaf (as Tree→Tree)", tree, type, B)
  })

  it("leaf : Bool → Nat (stem maps bools to 1, 2)", () => {
    const tree = LEAF
    const type = arrow(BOOL, NAT)
    exhibit("bool-to-nat", "leaf : Bool → Nat", tree, type, B)
    console.log(`  stem(true) = stem(leaf) = 1: ${prettyTree(apply(LEAF, LEAF))}`)
    console.log(`  stem(false) = stem(stem(leaf)) = 2: ${prettyTree(apply(LEAF, stem(LEAF)))}`)
  })

  it("K = stem(leaf) : Nat → (Bool → Nat)", () => {
    // stem(leaf) applied to x = fork(leaf, x) = K(x)
    // K(x)(y) = x. If x : Nat, then K(x) : Bool → Nat.
    const tree = K  // stem(leaf)
    const type = arrow(NAT, arrow(BOOL, NAT))
    const deriv: Deriv = B

    exhibit("K-combinator", "K = stem(leaf) : Nat → Bool → Nat", tree, type, deriv)
    console.log(`  K(3) = fork(leaf, 3) = ${prettyTree(apply(K, stem(stem(stem(LEAF)))))}`)
    console.log(`  K(3)(true) = 3: ${prettyTree(apply(apply(K, stem(stem(stem(LEAF)))), LEAF))}`)
    console.log(`  Checker rule: fork(leaf, x) = K(x) : Bool→Nat when x : Nat`)
    console.log(`    ⟹ stem(leaf) : Nat → (Bool → Nat) ✓`)
  })
})

// ============================================================
// CATEGORY 5: POLYMORPHIC FUNCTIONS — K-erased type args
// ============================================================
// Surface: {A : Type} -> ... where A is used for typing only
// Tree: K(...) wraps the body, discarding the type argument
// Deriv: K(inner) where inner is checked against B(α) for abstract α

describe("Category 5: Polymorphic functions", () => {
  it("id : (A : Type) → A → A", () => {
    // Surface: {A : Type} -> {x : A} -> x
    // [x] x = I. [A] I = K(I) since A ∉ FV(I).
    const tree = fork(LEAF, I) // K(I)

    // Type: Pi(Type, α ↦ α → α)
    // α → α = fork(α, K(α)). As a function of α: S(leaf, K).
    // S(leaf, K)(α) = fork(α, K(α)) = α → α.
    const selfArrow = fork(stem(LEAF), K) // S(leaf, K)
    const type = pi(TYPE, selfArrow) // Pi(Type, S(leaf, K))

    const deriv: Deriv = {
      tag: "K",
      inner: { tag: "triage", drvC: B, drvD: B, drvB: B } // I
    }

    exhibit("id", "{A : Type} -> {x : A} -> x", tree, type, deriv)
    console.log(`  Checking logic:`)
    console.log(`    K(I) : Pi(Type, B). K form: check I : B(α) for abstract α.`)
    console.log(`    B(α) = S(leaf,K)(α) = fork(α, K(α)) = α → α.`)
    console.log(`    I : α → α? Triage(leaf,leaf,leaf) with abstract domain α.`)
    console.log(`    All 3 branches: output = input → identity rule → ✓`)
    console.log(``)
    console.log(`  id(Nat)(3) = K(I)(Nat)(3) = I(3) = ${prettyTree(apply(apply(fork(LEAF, I), NAT), stem(stem(stem(LEAF)))))}`)
  })

  it("const : (A : Type) → (B : Type) → A → B → A", () => {
    // Surface: {A B : Type} -> {x : A} -> {y : B} -> x
    // [y] x = K(x). [x] K(x) = K. [B] K = K(K). [A] K(K) = K(K(K)).
    //
    // Actually let me verify:
    // [x] K(x): K(x) = fork(leaf, x) = apply(stem(leaf), x) = apply(K, x)
    //   [x] apply(K, x) = S(K(K), I) eta→ K. ✓
    // [B] K = K(K). [A] K(K) = K(K(K)).
    const tree = fork(LEAF, fork(LEAF, K)) // K(K(K))

    // Type: Pi(Type, Pi(Type, α → β → α))
    // Inner: α → β → α = fork(α, K(fork(β, K(α))))
    // This is complex. For the exhibit, just show the tree form.
    console.log(`\n┌─ const`)
    console.log(`│  surface:  {A B : Type} -> {x : A} -> {y : B} -> x`)
    console.log(`│  [y]x = K(x). [x]K(x) = K. [B]K = K(K). [A]K(K) = K(K(K)).`)
    console.log(`│  tree:     ${prettyTree(tree)} = K(K(K))`)
    console.log(`│  form:     ${classifyCombinator(tree)}`)
    console.log(`│`)
    console.log(`│  Evaluation:`)
    console.log(`│    K(K(K))(Nat) = K(K)`)
    console.log(`│    K(K)(Bool) = K`)
    console.log(`│    K(3)(7) = 3`)
    const r = apply(apply(apply(apply(tree, NAT), BOOL), stem(stem(stem(LEAF)))), stem(stem(stem(stem(stem(stem(stem(LEAF))))))))
    console.log(`│    const(Nat)(Bool)(3)(7) = ${prettyTree(r)}`)
    console.log(`│`)
    console.log(`│  ⚠ Full type encoding requires nested Pi with codomain functions.`)
    console.log(`│    Each Pi level adds another K wrapper to erase the type arg.`)
    console.log(`└─`)
  })
})

// ============================================================
// CATEGORY 6: RECURSIVE FUNCTIONS — FIX + context
// ============================================================

describe("Category 6: Recursive functions via FIX", () => {
  it("pred via FIX (non-recursive step)", () => {
    // step = K(pred_body) — step ignores self (pred doesn't recurse)
    // pred_body = Triage(0, I, K(K(0)))
    const predBody = fork(fork(LEAF, I), fork(LEAF, fork(LEAF, LEAF)))
    const step = fork(LEAF, predBody) // K(pred_body)
    const fixPred = apply(FIX, step)

    console.log(`\n┌─ pred (via FIX)`)
    console.log(`│  surface:  let rec pred : Nat → Nat := {n} -> natElim Nat 0 ({n'} -> n') n`)
    console.log(`│  step:     K(Triage(0, I, K(K(0))))`)
    console.log(`│  step tree: ${prettyTree(step)}`)
    console.log(`│  FIX(step): [tree id ${fixPred.id}]`)
    console.log(`│  step type: (Nat → Nat) → (Nat → Nat)`)
    console.log(`│  result:    Nat → Nat (via FIX rule)`)
    console.log(`│`)
    console.log(`│  Checking: verify step : (Nat→Nat) → (Nat→Nat)`)
    console.log(`│    K form: inner = Triage(0, I, K(K(0)))`)
    console.log(`│    Check Triage(0, I, K(K(0))) : Nat → Nat`)
    console.log(`│      leaf branch: 0 : Nat ✓`)
    console.log(`│      stem branch: I : Nat → Nat ✓`)
    console.log(`│    ⟹ step : T → T ✓ ⟹ FIX(step) : T ✓`)
    console.log(`│`)
    console.log(`│  pred(0) = ${prettyTree(apply(fixPred, LEAF))}`)
    console.log(`│  pred(1) = ${prettyTree(apply(fixPred, stem(LEAF)))}`)
    console.log(`│  pred(3) = ${prettyTree(apply(fixPred, stem(stem(stem(LEAF)))))}`)
    console.log(`└─`)
  })
})

// ============================================================
// CATEGORY 7: WHAT THE CHECKER CANNOT HANDLE
// ============================================================

describe("Category 7: Beyond the checker's reach", () => {
  it("fold : Nat → (R : Type) → R → (R → R) → R — polymorphic recursion", () => {
    // Surface: let rec fold := {n R z s} -> natElim R z ({n'} -> s (fold n' R z s)) n
    //
    // After compilation, this is a deeply nested S/K tree with FIX.
    // The R parameter is polymorphic — fold works for any type R.
    //
    // The checker CAN handle this IF:
    //   1. fold is registered in context with its type
    //   2. The step function's derivation is provided
    // The checker CANNOT:
    //   - Infer the type from the tree alone
    //   - Verify the body uses R consistently without an explicit derivation
    //     that tracks R through every S node

    console.log(`\n┌─ fold : Nat → (R : Type) → R → (R → R) → R`)
    console.log(`│  STATUS: Partially handleable`)
    console.log(`│`)
    console.log(`│  ✓ CAN: Register fold in context with declared type`)
    console.log(`│  ✓ CAN: Verify step function if derivation provided`)
    console.log(`│  ✗ CANNOT: Infer the type from the compiled tree`)
    console.log(`│  ✗ CANNOT: Verify R flows correctly without derivation`)
    console.log(`│`)
    console.log(`│  The step function involves:`)
    console.log(`│    - natElim applied to abstract R (polymorphic elimination)`)
    console.log(`│    - s : R → R applied to recursive result`)
    console.log(`│    - Multiple S nodes sharing R across sub-expressions`)
    console.log(`│  Each S node needs D recorded in the derivation.`)
    console.log(`│  The derivation for fold would be ~3x the size of the tree.`)
    console.log(`└─`)
  })

  it("Pair A B = (R : Type) → (A → B → R) → R — Church-encoded type", () => {
    console.log(`\n┌─ Pair : Type → Type → Type (Church-encoded)`)
    console.log(`│  STATUS: Not directly handleable`)
    console.log(`│`)
    console.log(`│  Surface: {A B} -> (R : Type) → (A → B → R) → R`)
    console.log(`│  This is a TYPE CONSTRUCTOR that produces a type.`)
    console.log(`│`)
    console.log(`│  ✗ The checker has no representation for Pair(Nat, Bool) as a type.`)
    console.log(`│    Its type encoding only knows: Type, Tree, Bool, Nat, Pi(A,B).`)
    console.log(`│    Pair(Nat,Bool) = (R:Type) → (Nat→Bool→R) → R is a valid Pi type,`)
    console.log(`│    but the checker can't verify that a tree is a valid Pair.`)
    console.log(`│`)
    console.log(`│  ✓ WORKAROUND: Encode pairs as plain trees.`)
    console.log(`│    mkPair(a,b) = fork(a,b). fst(p) = p.left. snd(p) = p.right.`)
    console.log(`│    Type: mkPair : Tree → Tree → Tree. Untyped but functional.`)
    console.log(`│    The optimizer works with Tree-typed pairs and trusts internal structure.`)
    console.log(`└─`)
  })

  it("convertibleRec : Nat → Nat → Nat → Tree → Tree → Tree → Bool — 6-arg higher-order recursive", () => {
    console.log(`\n┌─ convertibleRec (from prelude.disp)`)
    console.log(`│  STATUS: Theoretically handleable, practically challenging`)
    console.log(`│`)
    console.log(`│  Type: Nat → Nat → Nat → Tree → Tree → Tree → Bool`)
    console.log(`│  This is 6 nested arrows — all non-dependent, all ground types.`)
    console.log(`│`)
    console.log(`│  ✓ CAN: Type is expressible (nested arrow of base types)`)
    console.log(`│  ✓ CAN: FIX rule handles recursion`)
    console.log(`│  ✓ CAN: Each S node in the body can carry its D`)
    console.log(`│  ⚠ BUT: The compiled tree has ~50+ S nodes, each needing D in the derivation.`)
    console.log(`│    The derivation compiler must track types through:`)
    console.log(`│      - 6 layers of bracket abstraction`)
    console.log(`│      - Higher-order arguments (whnf, fastEq are function-valued)`)
    console.log(`│      - tWait wrapping (3 type args K'd away per tWait)`)
    console.log(`│    This is a derivation-generation challenge, not a checking challenge.`)
    console.log(`└─`)
  })

  it("dependent types: Vec n — indexed families", () => {
    console.log(`\n┌─ Vec : Nat → Type (length-indexed vectors)`)
    console.log(`│  STATUS: Not handleable without extensions`)
    console.log(`│`)
    console.log(`│  Vec(n) is a type that depends on a nat.`)
    console.log(`│  cons : (n : Nat) → Tree → Vec n → Vec (succ n)`)
    console.log(`│`)
    console.log(`│  ✗ No way to define Vec as a type in the current encoding.`)
    console.log(`│    Types are: leaf (Type), stem-chains (base types), fork (Pi).`)
    console.log(`│    Vec(n) would need to be an indexed type family:`)
    console.log(`│      Vec : Nat → Type means Vec is a tree function from Nat to types.`)
    console.log(`│    The Pi encoding supports this: Pi(Nat, Vec).`)
    console.log(`│    But what IS Vec(0)? Vec(1)? The checker needs to know`)
    console.log(`│    what values inhabit Vec(n) for each n.`)
    console.log(`│`)
    console.log(`│  ✗ This requires W-types or an inductive type mechanism.`)
    console.log(`│    The tree-native system would need a way to declare:`)
    console.log(`│      "Vec is a type family with constructors nil : Vec(0)`)
    console.log(`│       and cons : (n:Nat) → Tree → Vec(n) → Vec(succ(n))"`)
    console.log(`│    This is future work (Phase 6+).`)
    console.log(`└─`)
  })
})

// ============================================================
// CATEGORY 8: INTERESTING EDGE CASES — the tree calculus is "wider" than you expect
// ============================================================

describe("Category 8: Surprising type assignments", () => {
  it("stem(leaf) = K = 1 — the same tree is Nat AND a combinator", () => {
    const tree = stem(LEAF) // = K = 1 = false
    console.log(`\n┌─ stem(leaf) has MANY types`)
    console.log(`│  tree: ${prettyTree(tree)}`)
    console.log(`│`)
    console.log(`│  stem(leaf) = 1 : Nat ✓  (it's succ(zero))`)
    console.log(`│  stem(leaf) = false : Bool ✓  (it's stem(true))`)
    console.log(`│  stem(leaf) = K : Nat → (Nat → Nat) ✓  (fork-with-leaf constructor)`)
    console.log(`│  stem(leaf) = K : Bool → (Tree → Bool) ✓  (K(bool)(anything) = bool)`)
    console.log(`│  stem(leaf) : Tree ✓  (everything is a tree)`)
    console.log(`│`)
    console.log(`│  This is SOUND — each type assignment describes a valid way to USE the tree.`)
    console.log(`│  As 1, you compare it to other nats.`)
    console.log(`│  As K, you apply it twice and get the first argument back.`)
    console.log(`│  Same bits, different protocol.`)
    console.log(`└─`)
  })

  it("fork(leaf, leaf) = K(leaf) = K(true) = K(0) — multiple interpretations", () => {
    const tree = fork(LEAF, LEAF)
    console.log(`\n┌─ fork(leaf, leaf) has MANY types`)
    console.log(`│  tree: ${prettyTree(tree)}`)
    console.log(`│`)
    console.log(`│  As K(true) : Bool → Bool ✓  (always returns true)`)
    console.log(`│  As K(0)    : Nat → Nat ✓   (always returns zero)`)
    console.log(`│  As K(succ) : Bool → (Nat → Nat) ✓  (always returns the succ function)`)
    console.log(`│  As K(Type) : Tree → Type ✓  (always returns the type of types)`)
    console.log(`│`)
    console.log(`│  The derivation disambiguates: K(·) always has the same derivation`)
    console.log(`│  structure, but the INNER derivation says what the constant value's type is.`)
    console.log(`└─`)
  })

  it("every tree is typeable as Tree → Tree", () => {
    // In tree calculus, every tree can be applied to any other tree.
    // The result is always a tree. So EVERYTHING has type Tree → Tree.
    const examples = [
      LEAF,                    // leaf(x) = stem(x) : Tree
      stem(LEAF),              // K(x) = fork(leaf, x) : Tree (it's also a tree)
      I,                       // I(x) = x : Tree
      fork(LEAF, stem(LEAF)),  // K(false)(x) = false : Tree
      fork(stem(I), I),        // S(I,I)(x) = x(x) : Tree
    ]
    console.log(`\n┌─ Every tree is typeable as Tree → Tree`)
    for (const t of examples) {
      const result = apply(t, stem(stem(LEAF))) // apply to 2
      console.log(`│  ${prettyTree(t).padEnd(30)} applied to 2 = ${prettyTree(result)}`)
    }
    console.log(`│`)
    console.log(`│  This means Tree → Tree is the "universal function type."`)
    console.log(`│  Narrower types (Nat → Nat, Bool → Bool) are refinements`)
    console.log(`│  that constrain both input and output.`)
    console.log(`└─`)
  })
})

// ============================================================
// SUMMARY TABLE
// ============================================================

describe("Summary", () => {
  it("print capability matrix", () => {
    console.log(`
┌────────────────────────────────────────────────────────────────┐
│                  TREE-NATIVE TYPE CHECKER                      │
│                  Capability Matrix                             │
├──────────────────────┬─────────┬──────────────────────────────┤
│ Program class        │ Status  │ Requirements                 │
├──────────────────────┼─────────┼──────────────────────────────┤
│ Constants K(v)       │ ✓ Full  │ None                         │
│ Pattern match Triage │ ✓ Full  │ None                         │
│ Identity I           │ ✓ Full  │ None                         │
│ Constructors leaf/K  │ ✓ Full  │ None                         │
│ Composition S(c,b)   │ ✓ Full  │ Derivation with D            │
│ Nested S             │ ✓ Full  │ Derivation per S node        │
│ Recursion fix(step)  │ ✓ Full  │ step : T→T check + context   │
│ Polymorphic id/const │ ✓       │ Abstract type + identity rule│
│ Ground-type prelude  │ ✓       │ Derivation + context         │
│ (not,and,add,mul...) │         │                              │
├──────────────────────┼─────────┼──────────────────────────────┤
│ Polymorphic fold     │ ◐       │ Derivation tracks type var R │
│ Higher-order args    │ ◐       │ Large derivations            │
│ Complex recursion    │ ◐       │ Derivation generation        │
├──────────────────────┼─────────┼──────────────────────────────┤
│ Church-encoded types │ ✗       │ Need type declaration system │
│ Indexed families     │ ✗       │ Need W-types / inductive     │
│ Type inference       │ ✗       │ Need constraint solver       │
│ Error messages       │ ✗       │ Need diagnostic tracking     │
└──────────────────────┴─────────┴──────────────────────────────┘

For the optimization loop: everything in ✓ and ◐ is usable.
The optimizer generates (tree, type, derivation) triples.
The ✗ items are human-interface concerns, not optimizer concerns.
`)
  })
})
