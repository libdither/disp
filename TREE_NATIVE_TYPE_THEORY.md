# Tree-Native Type Theory

A dependent type system for tree calculus where types are executable predicate trees, type checking is predicate evaluation, and the checker itself is a tree program.

## Trees as Typed Combinators

In tree calculus, every tree is one of three things:

```
leaf          Base element. As a function: apply(leaf, x) = stem(x)
stem(a)       Partial pair. As a function: apply(stem(a), x) = fork(a, x)
fork(a, b)    A combinator whose behavior depends on a
```

Every `fork(a, b)` falls into exactly one of three categories:

```
fork(leaf,      v)  =  K(v)        Constant: ignores argument, returns v
fork(stem(c),   b)  =  S(c, b)     Share: applies c and b to argument, combines
fork(fork(c,d), b)  =  Triage      Match: dispatches on argument's tag
```

Every compiled program is a tree of S/K/Triage nodes. S says "the argument is shared here," K says "the argument is discarded here," Triage says "pattern-match on the argument." These structural features are exactly what a type checker needs to verify.

## Types as Predicates

**The central idea**: a type is a tree that, when applied to a value, returns true (= leaf) or false (= stem(leaf)). Types are not opaque tags — they are executable predicate functions. The universal type-checking operation is simply `apply(type, value)`.

```
Tree  = {_} -> true                     Accepts everything
Bool  = triage true isLeaf (K(K false)) Accepts leaf (true) and stem(leaf) (false)
Nat   = fix {self} ->                   Accepts leaf (zero) and stem-chains (succ^n(zero))
          triage true self (K(K false))
```

These are defined as ordinary tree calculus programs in `types.disp`, compiled by the same bracket abstraction used for all code. They are first-class values — you can pass them around, apply them, store them in data structures.

```
apply(Nat, leaf)              = true    (zero is a Nat)
apply(Nat, stem(stem(leaf)))  = true    (2 is a Nat)
apply(Nat, fork(leaf, leaf))  = false   (not a Nat)
apply(Bool, stem(leaf))       = true    (false is a Bool)
apply(Bool, stem(stem(leaf))) = false   (not a Bool)
```

**Type:Type**: in the current system (without universe hierarchy), the predicate for types is `Tree = K(true)` — every tree is a valid type. This is inconsistent as a logic but fine as a programming language. Universe hierarchies can be added later.

**Terminology**: throughout this document, "type" and "predicate" are synonyms. The document uses `true`/`false` for clarity; `types.disp` uses `tt`/`ff` to avoid parser keyword conflicts.

**Termination**: base predicates (Nat, Bool) terminate by structural recursion on the input. User-defined predicates could diverge. The system uses evaluation budgets as a practical bound. Decidability is not guaranteed for arbitrary predicates — this is a known tradeoff of types-as-programs.

## Pi Types as Evaluable Predicates

A function type `Pi(A, B)` is constructed by partially applying PiCheck:

```
Pi(A, B) = apply(apply(PiCheck, A), B)
```

where `A` is the domain predicate and `B` is the codomain family (`apply(B, x)` gives the output type for input `x`). PiCheck is a tree program defined in `types.disp`.

**The result is a triage combinator** — an evaluable predicate just like Nat or Bool:

```
apply(apply(PiCheck, A), B) = fork(fork(leafResult, stemHandler), forkBody)
```

PiCheck eagerly evaluates when partially applied, producing a triage tree where:
- `leafResult` = precomputed boolean: does the stem constructor preserve `A → B`?
- `stemHandler` = a tree that checks `stem(a)` as a function from A to B
- `forkBody` = a tree that dispatches on K/S/Triage/Ascription annotation structure

When the Pi type predicate is applied to an annotated tree:
```
apply(Pi(A, B), leaf)      → leafResult           (precomputed, O(1))
apply(Pi(A, B), stem(u))   → apply(stemHandler, u)
apply(Pi(A, B), fork(u,v)) → apply(apply(forkBody, u), v)
```

**This means ALL types are evaluable predicates.** Base types dispatch on value structure. Pi types dispatch on annotation structure. The protocol is uniform: `apply(type, value_or_annotated_tree) → true/false`.

### Non-dependent arrows

`A -> C` = `apply(apply(PiCheck, A), K(C))`. The codomain family is `K(C)` — constant for all inputs. `apply(K(C), x) = C` for all x.

### Dependent functions

`(n : Nat) -> Vec n` = `apply(apply(PiCheck, Nat), Vec)`. The codomain family IS Vec — `apply(Vec, 3)` gives the predicate for 3-element vectors.

### Precomputation: the Futamura projection

When you construct a Pi type, PiCheck **eagerly runs its checking logic**, partially evaluated with respect to A and B. This is the Futamura projection applied to type checking: specializing the checker with the type produces a residual program — the predicate tree — that is the specialized checker for that type.

```
-- Constructing Pi(Nat, K(Nat)):
-- PiCheck eagerly computes leafCheck(Nat, K(Nat)):
--   Inspects Nat's triage structure, finds stem handler = Nat
--   FAST_EQ(Nat, Nat) = true → leafResult = true
-- Result: fork(fork(true, stemHandler), forkBody) with true baked in

-- Later, checking leaf : Pi(Nat, K(Nat)):
-- apply(piType, leaf) → triage leaf case → true  (O(1), precomputed!)
```

With hash-consing, `Pi(Nat, K(Nat))` is constructed ONCE. Every occurrence of `Nat → Nat` in the program shares the same specialized checker tree.

### Type compilation = value compilation

Codomain families are bracket-abstracted from type expressions using the same pipeline as values:

```
-- For the type (n : Nat) -> Vec n -> Vec (succ n):
-- Codomain family B = [n] Pi(apply(Vec, n), K(apply(Vec, stem(n))))
-- After bracket abstraction, B is a tree-level function:
-- apply(B, 3) = Pi(Vec(3), K(Vec(4))) = Vec 3 -> Vec 4
```

### Type equality

Two Pi types are equal iff they are the same tree (O(1) via hash-consing). Since `apply(apply(PiCheck, A), B)` is deterministic and hash-consed, the same A and B always produce the same Pi type ID.

## The Annotated Tree Format

Programs are annotated with type information for the checker to verify. **Why does only S need annotation?**

- **K** discards the argument — there is nothing to type at the discard point
- **Triage** dispatches on the domain — the domain is already known from the enclosing Pi type
- **S** shares the argument between two sub-computations — D records what the shared value's type looks like at the join point. Without D, the checker cannot determine how the two halves compose.

```
Raw program              Annotated (superimposed)
────────────             ────────────────────────
fork(leaf, v)            fork(leaf, ann_v)                    K — IDENTICAL
fork(stem(c), b)         fork(stem(D), fork(ann_c, ann_b))   S — D in stem slot
—                        fork(stem(T), stem(body))            Ascription — type T wraps opaque body
fork(fork(c,d), b)       fork(fork(ann_c, ann_d), ann_b)     Triage — IDENTICAL
leaf                     leaf                                 IDENTICAL
stem(a)                  stem(ann_a)                          IDENTICAL
```

**Annotations are predicates.** D at S nodes is a predicate (or predicate family). The annotated tree is "a program with predicates cached at each composition point."

The checker dispatches with **one triage on the left child**:
- `leaf` → K rule
- `stem(D)` → S rule (D is right there); or if right is also `stem`, Ascription rule
- `fork(c,d)` → Triage rule

The pure executable program is extractable in O(n):
```
extract(fork(stem(D), fork(c, b))) = fork(stem(extract(c)), extract(b))   -- strip D
extract(fork(stem(T), stem(body))) = body                                  -- unwrap ascription
extract(fork(leaf, v))             = fork(leaf, extract(v))                -- K: pass through
extract(fork(fork(c, d), b))       = fork(fork(extract(c), extract(d)), extract(b))
extract(leaf)                      = leaf
extract(stem(a))                   = stem(extract(a))
```

## Typing Rules

All rules are implemented as cases within PiCheck's triage dispatch. When `apply(Pi(A, B), ann)` fires, the triage structure routes `ann` to the appropriate rule.

### Rule 0: Base Types — Predicate Evaluation

For base types (Nat, Bool, Tree), checking IS running the predicate:

```
check(value, Nat)  = apply(Nat, value)
check(value, Bool) = apply(Bool, value)
check(value, Tree) = apply(Tree, value)    Always true
```

No hardcoded rules — the predicate defines what values the type accepts.

### Rule 1: K — Weakening

`ann = fork(leaf, ann_v)` — K ignores its argument; the value must satisfy the codomain.

Non-dependent case (`B = K(B₀)`): check `v : B₀` — recursively apply B₀ to ann_v.

Dependent codomain over finite domain (Bool): exhaustive check — verify `v : apply(B, true)` and `v : apply(B, false)`.

Dependent codomain over infinite domain (Nat, Tree, Type): **constructor-class representative check**. Every tree is built from three constructors: `leaf`, `stem`, and `fork`. Evaluate the codomain family B at one representative from each constructor class — `leaf`, `stem(leaf)`, `fork(leaf, leaf)` — and check v against each resulting concrete type. This exploits the fact that tree calculus has exactly three constructors: if a proof obligation holds at a representative of each form, it covers the inductive structure of all trees.

For parametric codomain families (standard polymorphism, where B builds types from its argument without dispatching on it), the checking proof at each representative has the same structure — type equality comparisons reduce to reflexivity (`A = A`). This is the combinator analog of checking under a binder: where lambda calculus introduces a free variable and checks once, tree calculus evaluates the codomain at each constructor class and checks three times, with the same reflexive proof at each.

### Rule 2: S — Contraction

`ann = fork(stem(D), fork(ann_c, ann_b))` — **D is read directly from the tree.**

```
S(c, b) : Pi(A, B)    if    b : Pi(A, D)
                             c : Pi(A, sCodomain(D, B))
```

D is the interface contract between the two halves: `b` maps A-values to D-values, `c` maps (A-value, D-value) pairs to B-values. If both sides satisfy their contracts, `S(c, b)` maps A to B — **regardless of what D is**. D is a sufficient specification, not a unique one.

PiCheck recurses: it constructs `Pi(A, D)` and `Pi(A, sCodomain(D, B))` by calling itself with new arguments, then applies these derived Pi predicates to the sub-terms.

The codomain helper `sCodomain` computes c's expected type:
```
sCodomain(D, B) = S(S(K(leaf), D), S(K(K), B))

-- Derivation: apply(sCodomain(D, B), x)
--   S(K(leaf), D)(x) = apply(leaf, apply(D, x)) = stem(D(x))
--   S(K(K), B)(x)    = apply(K, apply(B, x)) = K(B(x))
--   So sCodomain(D,B)(x) = apply(stem(D(x)), K(B(x))) = fork(D(x), K(B(x))) = Pi(D(x), K(B(x)))
-- Result: [x] Pi(D(x), K(B(x)))  ✓
```

**Worked example**: `double : Nat -> Nat` defined as `{x} -> add x x` compiles to `S(add, I)`:

```
S(add, I) : Pi(Nat, K(Nat))  with D = K(Nat)

Check b = I : Pi(Nat, D) = Pi(Nat, K(Nat)) = Nat -> Nat
  I is the identity combinator → accepted at any Pi(A, K(A)) via Rule 5  ✓

Check c = add : Pi(Nat, sCodomain(K(Nat), K(Nat)))
  sCodomain(K(Nat), K(Nat))(x) = Pi(Nat, K(Nat)) = Nat -> Nat  (constant)
  So c = add : Pi(Nat, K(Nat -> Nat)) = Nat -> Nat -> Nat  ✓

Both pass → S(add, I) : Nat -> Nat  ✓
```

### Rule 3: Triage — Elimination

`ann = fork(fork(ann_c, ann_d), ann_b)` — identical structure to raw.

PiCheck dispatches on the domain predicate A (via O(1) hash-consing equality) to determine which branches to check:

**Tree domain** (all three branches):
```
c : apply(B, leaf)
d : Pi(Tree, stemCodomain(B))     where stemCodomain(B) = S(K(B), leaf)
b : Pi(Tree, forkCodomain(B))     where forkCodomain(B) = S(K(stem(stem(K(B)))), leaf)
```

**Nat domain** (leaf + stem): zero case + succ case.

**Bool domain** (leaf + specific stem): true case + false case.

### Rule 4: leaf/stem as Functions — Predicate Inspection

When `ann = leaf` or `ann = stem(a)` at a Pi type, PiCheck verifies the tree constructor preserves types by **inspecting the codomain predicate's triage structure**. No free variables — pure predicate structure inspection.

For `leaf : Pi(A, K(B₀))` — does the stem constructor map A-values to B₀-values?

```
leafCheck(A, B₀):
  -- By the triage stem rule: apply(B₀, stem(x)) = apply(d, x) where d = B₀'s stem handler
  -- So "B₀ accepts all stems of A-values" = "d accepts all A-values"
  d := B₀.left.right          -- extract stem handler from B₀'s triage structure
  if FAST_EQ(d, A): true      -- stem handler IS the domain → trivially preserves
  if A is finite: enumerate    -- check apply(B₀, stem(v)) for each v in A
  else: false                  -- can't verify → ascription needed
```

This is **eagerly evaluated at Pi type construction time**. The boolean result is baked into the triage tree's leaf position. Checking `leaf` against the Pi type is then O(1).

**Example**: `leaf : Pi(Nat, K(Nat))` — succ preserves Nat?
```
d = Nat.left.right = Nat        -- Nat's stem handler IS Nat
FAST_EQ(Nat, Nat) = true       -- Nat's own definition proves succ preserves Nat-ness
```

**Example**: `leaf : Pi(Bool, K(Bool))` — stem preserves Bool?
```
d = Bool.left.right = isLeaf    -- Bool's stem handler is isLeaf, not Bool
FAST_EQ(isLeaf, Bool) = false
Bool is finite → enumerate:
  apply(Bool, stem(leaf))       = true    (stem(true) = false ✓)
  apply(Bool, stem(stem(leaf))) = false   (stem(false) not a Bool ✗)
→ false                          -- correctly rejected
```

**K-shaped codomain** (B₀ = K(true) = Tree): short-circuit, always true.

### Rule 5: I — Identity (Variable Rule)

`I = fork(fork(leaf, leaf), leaf)` — the analog of the variable rule `Γ, x:A ⊢ x : A`. Recognized and accepted at any `Pi(A, K(A))`. This is the base case of structural recursion.

### Rule 6: Ascription

`ann = fork(stem(T), stem(body))` — opaque value with declared type.

```
check(ascribe(T, body), expectedType) = treeEqual(T, expectedType)
```

O(1) hash-consing comparison. See "Ascription Soundness" below.

### Rule 7: FIX — Recursion

```
fix(step) : T    if    step : T → T
```

The step function is checked against `T → T`. The fixed point is stored as an ascription.

## Dependent Type Checking

### The Elaborator

The elaborator takes surface syntax and produces annotated trees in two phases:

**Phase 1 (Symbolic)**: Bidirectional type inference with named variables. Types are tracked symbolically. The elaborator resolves names, checks applications, and tracks dependencies.

**Phase 2 (Compilation)**: Bracket-abstract all lambdas into S/K/I combinators. At each S node, record D. Compile symbolic types to tree predicates by calling `apply(apply(PiCheck, A_tree), B_tree)` for Pi types. Collapse to annotated tree + raw tree.

The elaborator never decomposes a tree-level Pi type — it works symbolically in Phase 1 and only constructs tree-level types in Phase 2.

### Dependent Instantiation = Tree Reduction

When a dependent function is applied to a concrete argument, the codomain family is EVALUATED:

```
id : Pi(Tree, B)    where B computes apply(B, A) = Pi(A, K(A)) = A → A

id Nat:
  1. Domain check: apply(Tree, Nat) = true ✓
  2. Result type: apply(B, Nat) = Pi(Nat, K(Nat)) = Nat → Nat  ← CONCRETE

id Nat 3:
  1. Domain check: apply(Nat, 3) = true ✓
  2. Result type: Nat
  3. Runtime: apply(apply(K(I), Nat), 3) = apply(I, 3) = 3  ✓
```

**Dependent type checking never needs abstract variables.** The elaborator computes codomain families while variables are in scope. After compilation, all type-level computation happens on concrete values.

### End-to-End Example: `not : Bool -> Bool`

Source: `let not : Bool -> Bool := {b} -> boolElim false true b`

**Phase 1**: expected type `Pi(Bool, K(Bool))`. Lambda body checks: boolElim dispatches on b, returning false for true and true for false. Both branches satisfy Bool.

**Phase 2**: `[b](boolElim false true b)` eta-reduces to the triage tree `fork(fork(false, K(true)), handler)`. No S nodes, no D annotations. Triage nodes are self-annotating.

**Checking**: `apply(Pi(Bool, K(Bool)), annotated_not)`:
- Triage node, domain = Bool
- Leaf case (true → false): `apply(Bool, stem(leaf)) = true` ✓
- Stem handler (false → true): handler applied to inner gives leaf. `apply(Bool, leaf) = true` ✓
- Result: `not : Bool → Bool` ✓

### Example: Length-Indexed Vectors

```
-- Vec : Nat -> Type (a predicate family)
let rec Vec := {n v} ->
  triage
    (isLeaf v)                                    -- n=0: only leaf is Vec 0
    ({n'} -> triage false ({_} -> false)           -- n=succ(n'):
             ({h t} -> Vec n' t) v)                --   fork(h,t) with t : Vec n'
    ({_ _} -> false)                               -- n=fork: invalid Nat
    n

-- Constructors (ascribed)
vnil  : Vec 0
vcons : (n : Nat) -> Tree -> Vec n -> Vec (succ n)
```

**Application-time checking** (all types resolve concretely):
```
vcons 0 c vnil:
  1. vcons 0: apply(B_vcons, 0) = Pi(Tree, K(Pi(Vec(0), K(Vec(1)))))    ← concrete!
  2. vcons 0 c: apply(Tree, c) = true ✓. Result: Pi(Vec(0), K(Vec(1)))
  3. vcons 0 c vnil: apply(apply(Vec, 0), leaf) = isLeaf(leaf) = true ✓
  Result type: Vec(1)
```

## Ascription Soundness

Ascriptions are the **only** soundness hole in the type system. Every other rule is self-policing:

- **S-rule D annotations** cannot introduce unsoundness. D is an existential witness — PiCheck checks both `b : Pi(A, D)` and `c : Pi(A, sCodomain(D, B))`. A too-weak D fails the c-check; a too-strong D fails the b-check; a divergent D causes budget exhaustion (check failure). Only a D that accurately describes a valid intermediate type lets both checks pass.
- **K, Triage, I, leaf/stem rules** all structurally recurse — no trust assumptions.

Ascriptions bypass structural checking entirely. `fork(stem(T), stem(body))` passes if `T = expectedType` via O(1) hash-consing, regardless of what `body` actually does.

### Why ascriptions exist

Some trees are **structurally opaque** to PiCheck:

1. **Bootstrap definitions**: definitions compiled before PiCheck exists lack annotations. PiCheck can't structurally verify them.
2. **Fixpoints**: `apply(fix, step)` produces combinator structure from the recursion machinery that doesn't match any typing rule.

In both cases, the type is established by external reasoning, but PiCheck can't derive it from the tree's structure alone.

Note: dependent codomains over infinite domains (polymorphism) do **not** require ascription — the constructor-class representative check (Rule 1) handles them structurally.

### The allowlist model

Ascriptions are verified against an **allowlist** — a set of `(body, type_identity)` pairs enumerating the approved opaque terms.

```
check(ascribe(T, body), expectedType) =
  treeEqual(T, expectedType)                     -- claimed type matches expected
  AND allowlistContains(allowlist, body, T)       -- body is approved at type T
```

The allowlist contains only genuinely opaque terms:
- Bootstrap definitions compiled before PiCheck exists
- Fixpoints whose step functions were verified via `step : T → T`

All other definitions — including polymorphic ones — are verified structurally by PiCheck via their annotated trees and do **not** appear on the allowlist. When a verified definition is referenced inside another typed expression, PiCheck re-verifies its annotated structure directly. The allowlist is a small, auditable trust boundary.

The elaborator produces two outputs: the **annotated tree** and the **allowlist** (its certificate). A third party can verify correctness with just PiCheck + the allowlist, without trusting the elaborator. The elaborator becomes a proof assistant, not a trusted component.

### Allowlist as a tree program

The allowlist is a tree-encoded data structure — a parameter to the checker. The extended PiCheck takes three arguments at the top level: `VerifiedCheck(allowlist, A, B)` returns a predicate that checks annotated trees, verifying ascriptions against the allowlist. With FAST_EQ, membership lookup is O(n) with a list encoding, O(log n) with a trie.

### Bounded blast radius (without allowlist)

Even without an allowlist, wrong ascriptions have bounded damage. The next predicate evaluation catches the error:

```
-- leaf wrongly ascribed as Nat → Bool:
wrong_succ 3:
  Domain check: apply(Nat, 3) = true ✓     -- passes (Nat is correct)
  Result type: Bool                          -- from the wrong ascription
  Runtime result: 4                          -- actually a Nat

-- At the NEXT use site:
boolElim (wrong_succ 3) t f:
  Domain check: apply(Bool, 4) = false ✗    -- CAUGHT
```

The one exception: a `Tree`-typed context (Tree accepts everything) — but no type-dependent decision is made, so the unsoundness is harmless. This bounded-blast-radius property holds regardless of whether an allowlist is used.

## Bootstrap Architecture

```
1. types.disp    — Defines base predicates (Nat, Bool, Tree), PiCheck, fix, wait, triage
                    Compiled by bare (untyped) bracket abstraction
                    All definitions get default type Tree
                    PiCheck is the most complex definition — it implements all typing rules

2. Typed env     — Built from types.disp output:
                    Type names: Nat, Bool, Tree → predicate trees typed as Type
                    Constructors: zero, succ, true, false → typed by predicate inspection
                    Eliminators: boolElim, natElim → ascribed with dependent types

3. prelude.disp  — User-level definitions (not, and, add, mul, id, Pair, etc.)
                    Compiled by typed elaborator
                    Each definition checked via apply(type_predicate, annotated_tree)
```

**Predicates bootstrap themselves.** `types.disp` is compiled in bare mode (no types needed). The predicate trees it produces — including PiCheck — become the type system for everything after.

## Structural Type Theory Correspondence

The annotated tree is a proof term in structural type theory:

| Structural rule | Combinator | Annotation | Why |
|---|---|---|---|
| **Weakening** | K(v) | None | Argument discarded — nothing to record |
| **Contraction** | S(c, b) | D = intermediate predicate | Argument shared — D records the join-point type |
| **Elimination** | Triage(c, d, b) | None | Domain known from enclosing Pi type |
| **Variable** | I | None | Identity preserves types — the base case |

K=weakening and S=contraction are known correspondences (Curry 1963). Triage=elimination and D-as-contraction-witness are specific to tree calculus.

## Swappable Type Systems

Different type systems are different PiCheck implementations sharing the same base predicates. Since Pi types ARE PiCheck partially applied, swapping PiCheck changes what compositions are accepted without changing what values each base type contains.

| Type system | PiCheck behavior | Accepts |
|---|---|---|
| **Simple** | K: non-dependent only. S: non-dependent D. | Simply-typed programs |
| **Dependent** | K: exhaustive for Bool, ascription for infinite. S: dependent D, B. | Dependently-typed programs |
| **System F** | K: polymorphic over Type via ascription. S: type-level D. | Polymorphic programs |

Base predicates (Nat, Bool, Tree) are **identical across all type systems**. An annotated tree produced by one system can be re-checked by another. Hash-consing prevents accidental mixing — Pi types from different PiCheck instances are different trees.

## Hash-Consing and O(1) Type Equality

Trees are hash-consed: structurally equal trees share the same ID.

- **O(1) type equality**: `a.id === b.id` — no normalization needed
- **O(1) ascription verification**: claimed type equals expected type in one comparison
- **Memoized checking**: cache key `(annotated_tree.id, type.id) → bool` prevents redundant work
- **D sharing**: identical intermediate predicates at different S nodes stored once
- **Pi type sharing**: `Pi(Nat, K(Nat))` constructed once, reused everywhere

## Prior Art and Novelty

Type checking on S/K/I combinators (simple types) is classical — Curry & Feys 1958, Hindley 1969.

Barry Jay ("Typed Program Analysis without Encodings," PEPM 2025) types tree calculus using structural program types with subtyping. **The key difference**: Jay's types are inert data in a separate grammar. Our types are executable predicates in the same language as values. This is what enables dependent types: codomain families must compute, and executable types make computation free.

Altenkirch et al. (FSCD 2023) work algebraically with categories of families. Zaldivar Aiello (2024) adds an M combinator. Neither produces a practical checking algorithm.

**Novel contributions**:
1. **Types as executable predicates** — types are tree programs, not inert data
2. **Pi types as partially-applied checkers** — `Pi(A, B) = apply(apply(PiCheck, A), B)` is an evaluable predicate, uniform with base types. The Futamura projection applied to type checking.
3. **Predicate inspection for constructor typing** — derives `succ : Nat → Nat` from Nat's triage structure without external axioms
4. **Precomputed constructor checks** — leafCheck evaluated eagerly at Pi type construction time, baked into the triage tree. O(1) at check time.
5. **Self-bootstrapping** — predicates and PiCheck compiled by the same pipeline as programs
6. **Swappable type systems** — different PiCheck trees, shared base predicates
7. **No abstract variables in checking** — codomain families eliminate variables; predicate inspection reasons about predicates, not hypothetical values
8. **Allowlist-verified ascriptions** — opaque terms (bare-compiled defs, fixpoints) verified against an allowlist certificate; D annotations need no external validation (the S-rule is self-policing)
