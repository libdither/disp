# Tree-Native Type Theory

A dependent type system defined directly over tree calculus terms, eliminating the need for intermediate encodings, markers, or runtime bracket abstraction.

## Motivation

### The Current Architecture

Disp currently uses a two-layer approach to type checking:

```
Surface syntax:    {x : Nat} -> add x x
                        |
                   CoC Encoding (5-way tagged trees)
                        |
                   encLam(NAT, body)     <-- type checker works HERE
                        |
                   Bracket Abstraction
                        |
Compiled tree:     S(S(K(add))(I))(I)    <-- evaluator works HERE
```

The CoC encoding is a **middleman**. It exists solely because traditional type theory works with named variables and binders, which don't exist in tree calculus. To bridge this gap, the current system:

1. Invents unique "marker" trees as variable names
2. Substitutes markers into binder bodies to "open" them
3. Type-checks the opened body
4. Runs `abstractOut` to rebuild an S/K/I combinator from the result ("closing" the binder)

Step 4 is the critical bottleneck. `abstractOut` walks the **entire** result tree, checking every node against the marker (O(n) equality checks), then rebuilds the result as an S/K/I combinator. With k nested binders, this produces O(n * k) work that grows superlinearly.

### The Key Observation

The compiled tree `S(S(K(add))(I))(I)` **already encodes** all the type-relevant structure:

```
S(S(K(add))(I))(I)
│ │ │       │   └─ I: argument passed through unchanged
│ │ │       └───── I: argument passed to add
│ │ └───────────── K(add): add is a constant (doesn't depend on x)
│ └─────────────── S: add receives x (via I) — uses the argument
└───────────────── S: result of (add x) receives x (via I) — uses the argument again
```

Every S tells us "the argument is used here." Every K tells us "the argument is ignored here." Every triage tells us "pattern-match on the argument." These are exactly the structural features a type checker needs to verify.

**We don't need the encoding layer.** We can type-check the compiled tree directly.

### What We Gain

| | CoC Encoding | Direct Tree Typing |
|---|---|---|
| Term representation | 5-way tagged encoding | Raw tree (leaf/stem/fork) |
| Variable mechanism | Unique marker trees | None needed |
| Opening a binder | Apply body to marker | Nothing (already open) |
| Closing a binder | `abstractOut` — O(n) per binder | Nothing (already closed) |
| Dispatch | 5-way TERM_CASE | 3-way triage (built into tree calculus) |
| Environment | Church list (O(n) lookup) | None needed |
| Type checker structure | Complex step function | Nested triage |

---

## Theory

### Trees as Typed Combinators

In tree calculus, every tree is one of three things:

```
leaf          The stem constructor: apply(leaf, x) = stem(x)
stem(a)       The fork-with-a constructor: apply(stem(a), x) = fork(a, x)
fork(a, b)    A combinator whose behavior depends on a
```

Every `fork(a, b)` falls into exactly one of three categories, determined by `a`:

```
fork(leaf,      v)  =  K(v)        Constant: ignores its argument, returns v
fork(stem(c),   b)  =  S(c, b)     Share: applies c and b to the argument, combines results
fork(fork(c,d), b)  =  Triage      Match: dispatches on the argument's tag (leaf/stem/fork)
```

Each category has a natural typing rule.

### Types as Trees

Types are encoded as trees using a simple scheme:

```
Type        = leaf                      The type of types
Tree        = stem(leaf)                The type of all trees
Bool        = stem(stem(leaf))          Booleans (leaf=true, stem(leaf)=false)
Nat         = stem(stem(stem(leaf)))    Naturals (leaf=zero, stem(n)=succ(n))
Pi(A, B)    = fork(A, B)               Dependent function: B is a tree computing the codomain
A -> B      = fork(A, fork(leaf, B))    Non-dependent function (special case: B = K(B₀))
```

Non-dependent function types `A -> B` are a special case of `Pi(A, B)` where `B = K(B₀) = fork(leaf, B₀)`. Applying `B` to any argument returns `B₀` — the codomain doesn't depend on the input.

For dependent functions like `(n : Nat) -> Vec n`, `B` is a tree function that computes the codomain from the argument: `apply(B, 3) = Vec 3`.

### The Typing Rules

#### Rule 0: Base Types

Every tree is a valid `Tree`. Subtypes are checked structurally:

```
check(t, Tree) = true                           Everything is a tree
check(leaf, Nat) = true                          leaf = zero
check(stem(n), Nat) = check(n, Nat)              stem(n) = succ(n) if n : Nat
check(leaf, Bool) = true                         leaf = true
check(stem(leaf), Bool) = true                   stem(leaf) = false
```

**Example**: Is `stem(stem(leaf))` a valid `Nat`?
```
check(stem(stem(leaf)), Nat)
  = check(stem(leaf), Nat)          succ(?) — check inner
  = check(leaf, Nat)                succ(?) — check inner
  = true                            zero!
→ stem(stem(leaf)) = succ(succ(zero)) = 2 : Nat  ✓
```

#### Rule 1: K — Constant Functions

`fork(leaf, v)` is the K combinator: applied to any argument, it returns `v`.

```
K(v) : A -> B    if    v : B
```

K ignores its argument entirely, so the codomain must not depend on the argument, and `v` must have the codomain type.

**Example**: `K(true) : Nat -> Bool`

```
K(true) = fork(leaf, leaf)

check(fork(leaf, leaf), Nat -> Bool):
  K form: check the value (leaf) against the codomain (Bool)
  check(leaf, Bool) = true   ✓

→ K(true) : Nat -> Bool  ✓     (for any n:Nat, K(true)(n) = true : Bool)
```

**Example**: `flip = K(I) : A -> B -> B`

```
K(I) = fork(leaf, fork(fork(leaf, leaf), leaf))

check(K(I), A -> B -> B):
  K form: check I against B -> B
  check(I, B -> B):
    ...triage rule (see below)...  ✓

→ K(I) : A -> B -> B  ✓        (flip x y = I y = y)
```

#### Rule 2: S — Shared Application

`fork(stem(c), b)` is the S combinator: `S(c, b)(x) = c(x)(b(x))`. Both `c` and `b` receive the argument `x`; `c(x)` is then applied to `b(x)`.

```
S(c, b) : A -> E    if    c : A -> D -> E    and    b : A -> D
```

Here D is an **intermediate type**: `b` computes a value of type D from the argument, and `c` takes both the argument and this intermediate value to produce the result.

**The challenge**: D doesn't appear in the expected type `A -> E`. It must come from somewhere. In the proof-carrying approach, the program is paired with a **derivation** that records D for each S node. More on this below.

**Example**: `double = S(S(K(add))(I))(I) : Nat -> Nat`

The derivation tells us D = Nat for the outer S. Reading it:

```
S(S(K(add))(I), I) : Nat -> Nat

Outer S: c = S(K(add))(I), b = I, D = Nat
  b = I : Nat -> Nat (identity — returns argument unchanged)
  So D = Nat (b's return type)

  c = S(K(add))(I) must have type Nat -> Nat -> Nat:
    Inner S: c' = K(add), b' = I, D' = Nat
      b' = I : Nat -> Nat  ✓
      c' = K(add): check add against Nat -> Nat -> Nat
        add : Nat -> Nat -> Nat (from context)  ✓
      K(add) : Nat -> (Nat -> Nat -> Nat)  ✓
    S(K(add), I) : Nat -> (Nat -> Nat)  ✓
    Which is Nat -> Nat -> Nat  ✓

→ S(S(K(add))(I), I) : Nat -> Nat  ✓
```

Notice what happened: the entire type derivation was structural recursion on the tree. No markers. No environment. No `abstractOut`. Just reading the S/K structure.

#### Rule 3: Triage — Dependent Elimination

`fork(fork(c, d), b)` is the triage combinator: it dispatches on its argument's tag.

```
triage(c, d, b)(leaf)       = c
triage(c, d, b)(stem(u))    = d(u)
triage(c, d, b)(fork(u, v)) = b(u)(v)
```

This IS the dependent eliminator for Tree. The typing rule depends on what type the argument has:

**Eliminating Tree** (the full induction principle):
```
Triage(c, d, b) : Tree -> R    if    c : R
                                      d : Tree -> R
                                      b : Tree -> Tree -> R
```

**Eliminating Nat** (nat values are leaf or stem(n), never fork):
```
Triage(c, d, b) : Nat -> R     if    c : R              (zero case)
                                      d : Nat -> R        (succ case)
```

**Eliminating Bool** (bool values are leaf=true or stem(leaf)=false):
```
Triage(c, d, b) : Bool -> R    if    c : R              (true case)
                                      apply(d, leaf) : R  (false case)
```

For Bool, there's only one possible stem value: `stem(leaf) = false`. So the stem handler `d` is evaluated on `leaf` (the inner value of `stem(leaf)`), and we check that the result has type R.

**Example**: `not : Bool -> Bool`

```
not = fork(fork(stem(leaf), fork(leaf, leaf)), fork(leaf, fork(leaf, stem(leaf))))
    = Triage(c = false,  d = K(true),  b = K(K(false)))

check(not, Bool -> Bool):
  Triage form, input type = Bool, return type R = Bool:

  true case:  check(c, Bool) = check(stem(leaf), Bool) = true  ✓
              (true -> false, and false : Bool)

  false case: check(apply(d, leaf), Bool)
              d = K(true) = fork(leaf, leaf)
              apply(fork(leaf, leaf), leaf) = leaf = true
              check(leaf, Bool) = true  ✓
              (false -> true, and true : Bool)

→ not : Bool -> Bool  ✓
```

**Example**: `I : Nat -> Nat` (identity preserves Nat)

```
I = fork(fork(leaf, leaf), leaf) = Triage(leaf, leaf, leaf)

check(I, Nat -> Nat):
  Triage form, input type = Nat, return type R = Nat:

  zero case: check(leaf, Nat) = true  ✓
             (zero -> zero = leaf -> leaf)

  succ case: check(leaf, Nat -> Nat)
             leaf as a function: apply(leaf, n) = stem(n) = succ(n)
             If n : Nat, then succ(n) : Nat  ✓
             (succ(n) -> succ(n), preserving Nat)

→ I : Nat -> Nat  ✓
```

#### Leaf and Stem as Functions

`leaf` and `stem(a)` can appear in function position:

```
apply(leaf, x) = stem(x)         leaf is the stem constructor
apply(stem(a), x) = fork(a, x)   stem(a) is the fork-with-a constructor
```

Typing:
```
leaf : Tree -> Tree                always valid (stem is a tree constructor)
leaf : Nat -> Nat                  valid because stem(n) = succ(n) preserves Nat
stem(a) : Tree -> Tree             always valid (fork is a tree constructor)
```

### Dependent Types

For dependent function types `Pi(A, B)` where `B : A -> Type`, the codomain depends on the argument value.

**Dependent Triage**: When checking `Triage(c, d, b) : (t : Tree) -> P(t)`:

```
c : P(leaf)                                   leaf case, specific return type
d : (u : Tree) -> P(stem(u))                  stem case, return type depends on u
b : (u : Tree) -> (v : Tree) -> P(fork(u,v))  fork case, return type depends on u and v
```

The motive `P` is a tree function. Evaluating `apply(P, leaf)` gives the expected type for the leaf case. Evaluating `apply(P, stem(u))` gives the expected type for the stem case.

**Dependent S**: For `S(c, b) : Pi(A, B)`:

```
c : Pi(A, x -> Pi(D(x), B(x)))
b : Pi(A, D)
```

Both D and the relationship between D and B can depend on the argument. The derivation carries D as a tree function.

### The Hierarchy of Power

Each tree calculus evaluation rule corresponds to a type-theoretic capability. Adding rules to the checker progressively increases the power of the type system:

**Base types** (triage on type tag):
```
Check membership: "3 : Nat", "true : Bool"
No functions.
```

**K rule** (constant functions):
```
Check functions that ignore their argument: "K(true) : Nat -> Bool"
Corresponds to weakening in structural type theory.
```

**Triage rule** (pattern matching):
```
Check functions that dispatch on their argument's tag: "not : Bool -> Bool"
Corresponds to the induction principle for Tree (and derived types Nat, Bool).
This is where computation enters the type system.
```

**S rule** (shared arguments):
```
Check functions that use their argument multiple times: "{x} -> add x x : Nat -> Nat"
Corresponds to contraction in structural type theory.
Without S, each argument can be used at most once.
```

**Dependent codomains**:
```
Check functions whose return type depends on the input value.
Pi(Nat, n -> Vec(n)) — "a vector whose length matches the input"
This is what makes the type system a logic (Curry-Howard).
```

**Universe hierarchy** (level tracking):
```
Type₀ : Type₁ : Type₂ : ...
Adds a natural number parameter to the checker that tracks universe levels.
Pi(A : Type_n, B : Type_m) : Type_max(n,m)
Prevents Girard's paradox (Type : Type is inconsistent).
```

**Identity types + J eliminator**:
```
Id(A, a, b) — the type of proofs that a equals b.
In tree calculus, identity is hash-consing equality: a.id == b.id.
J (path induction) is trivially sound since the only proof is refl.
Nearly free — just a fastEq check.
```

Each level adds one operation to the type checker and one class of programs becomes verifiable. Everything through identity types maps cleanly to tree calculus operations.

### Where the Pattern Breaks

**Univalence** (equivalence ≃ equality) requires changing what type equality *means*. In tree calculus, type equality is structural identity (same tree). Univalence demands that equivalent types (with bijective functions between them) are considered equal, even when they're different trees. This is a foundational change, not an added operation.

**Higher Inductive Types** (constructors for paths) require identity proofs beyond `refl`. In tree calculus, every tree has a unique normal form, so there's no room for distinct paths between the same endpoints. HITs would need a quotient mechanism layered on top of tree calculus.

---

## Implementation

### Phase 1: K + Triage Checker (No Derivations)

The simplest useful checker handles K forms and triage without needing the S rule or derivations. This already covers:
- All constants (from a context mapping tree IDs to types)
- `not : Bool -> Bool` (triage)
- `I : Nat -> Nat` (triage)
- `K(v) : A -> B` (any constant function)

The checker is a single tree-calculus function built with `compileTree(...)`:

```
check(ctx, tree, type) : Bool

Dispatch on type:
  leaf         → isValidType(tree)
  stem(tag)    → checkBaseType(tree, tag)      (Tree/Bool/Nat membership)
  fork(A, B)   → checkPi(ctx, tree, A, B)      (function types)

checkPi dispatches on tree structure:
  leaf             → checkLeafAsFn(A, B)        (stem constructor)
  stem(a)          → checkStemAsFn(a, A, B)     (fork constructor)
  fork(leaf, v)    → check(ctx, v, apply(B, leaf))     (K rule)
  fork(fork(c,d),b)→ checkTriage(ctx, c, d, b, A, B)  (elimination)
  fork(stem(c), b) → false                      (S rule — Phase 2)
```

The triage checker dispatches on the input type A:
```
checkTriage(ctx, c, d, b, A, B):
  R = apply(B, leaf)    (evaluate codomain at a dummy — works for non-dependent B)

  if A == Tree:
    check(ctx, c, R) AND check(ctx, d, Tree -> R) AND check(ctx, b, Tree -> Tree -> R)
  if A == Nat:
    check(ctx, c, R) AND check(ctx, d, Nat -> R)
  if A == Bool:
    check(ctx, c, R) AND check(ctx, apply(d, leaf), R)
```

The entire checker compiles to a tree constant via nested triage — no TERM_CASE, no markers, no environment.

**Testing strategy**: For each test in `coc.test.ts`, compile the surface expression to a tree, then verify `check(compiled_tree, expected_type) = true`. Compare step counts against the existing TYPECHECK.

### Phase 2: S Rule with Derivations

The S rule requires an intermediate type D that isn't present in the tree or the expected type. The solution: **proof-carrying code**. The compiler emits a derivation tree alongside the program.

A derivation mirrors the program structure:

```
For K(v):            deriv(v_derivation)
For S(c, b):         fork(D, fork(c_derivation, b_derivation))
For Triage(c,d,b):   fork(c_derivation, fork(d_derivation, b_derivation))
For leaf/constant:   leaf
```

The extended checker:

```
check(ctx, tree, type, derivation) : Bool
```

For the S case:
```
fork(stem(c), b) with derivation fork(D, fork(drvC, drvB)):
  E = apply(B, leaf)
  check(ctx, b, Pi(A, D), drvB) AND check(ctx, c, Pi(A, x -> Pi(D, E)), drvC)
```

The compiler (already in TypeScript) knows all intermediate types during bracket abstraction. Emitting derivations is a natural extension of the compilation process — at each step of bracket abstraction, record the types alongside the S/K/I construction:

```
[x : A] body:
  If body = x:           emit I,        derivation = leaf
  If x not in body:      emit K(body),  derivation = derive(body)
  If body = (f g):       emit S(cf, cg), derivation = fork(D, fork(derive(cf), derive(cg)))
    where D = return type of cg, cf = [x]f, cg = [x]g
```

### Phase 3: Dependent Types

The codomain `B` in `Pi(A, B)` becomes a genuine function. The checker evaluates `apply(B, value)` to compute the actual codomain for a specific argument.

For dependent triage with motive `P : A -> Type`:
```
Triage(c, d, b) : Pi(A, P)

check c   against apply(P, leaf)
check d   against Pi(A_inner, u -> apply(P, stem(u)))
check b   against Pi(A_inner, u -> Pi(A_inner, v -> apply(P, fork(u, v))))
```

The codomain computation uses the existing tree calculus `apply()` — the evaluator and the type checker share the same reduction machinery.

### Phase 4: Universe Hierarchy

Add a universe level parameter:

```
check(ctx, tree, type, derivation, level) : Bool
```

Types are annotated with levels:
```
Type_n = fork(leaf, n)     where n is a nat (level)
```

The Pi rule computes the maximum level:
```
Pi(A : Type_n, B : Type_m) : Type_max(n, m)
```

Universe checking adds one `max` computation and one `≤` comparison per typing rule. Both are trivial tree-calculus functions on nats.

### Phase 5: Identity Types

Identity types leverage hash-consing:

```
Id(A, a, b) : Type    — the type of proofs that a = b

refl(a) : Id(A, a, a) — the constructor (only exists when a = a)

J : (A : Type) -> (P : (a : A) -> (b : A) -> Id(A,a,b) -> Type) ->
    ((a : A) -> P(a, a, refl(a))) ->
    (a : A) -> (b : A) -> (p : Id(A,a,b)) -> P(a, b, p)
```

In tree calculus, checking `refl(a) : Id(A, a, b)` reduces to `fastEq(a, b)`. The J eliminator pattern-matches on identity proofs — since the only proof is `refl`, J simply returns the base case.

The checker operation: one `fastEq` call per identity type check.

### Architecture

```
                    ┌──────────────────────────┐
Surface syntax      │  {x : Nat} -> add x x    │
                    └───────────┬──────────────┘
                                │ parse
                    ┌───────────▼──────────────┐
AST                 │  Lam("x", Nat, App(...)) │
                    └───────────┬──────────────┘
                                │ bracket abstraction + type-directed derivation
                    ┌───────────▼──────────────┐
Compiled output     │  fork(program, fork(type, derivation))  │
                    │                          │
                    │  program = S(S(K(add))(I))(I)           │
                    │  type    = fork(Nat, fork(leaf, Nat))   │
                    │  derivation = fork(Nat, fork(...))      │
                    └───────────┬──────────────┘
                                │ check(program, type, derivation)
                    ┌───────────▼──────────────┐
Tree-native checker │  triage on type           │
                    │  triage on tree structure  │
                    │  triage on S/K/Triage form │
                    │  → true / false           │
                    └──────────────────────────┘
```

The checker is a single tree constant, built from nested triages. No TERM_CASE. No markers. No environment. No `abstractOut`. The program, its type, and its derivation are all trees. Checking is evaluation.

This is the GOALS.md vision realized: **"turn type checker into a function that takes a program and returns 1 or 0."** The function is a tree. The program is a tree. The result is leaf (well-typed) or stem(leaf) (ill-typed).
