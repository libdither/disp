# Tree-Native Type Theory

A dependent type system defined directly over tree calculus terms, where type annotations are embedded into the program tree itself, eliminating the need for intermediate encodings, markers, or runtime bracket abstraction.

**Status**: Prototype verified in `test/tree-native-{typecheck,exhibit,annotated,dependent}.test.ts` (555 tests passing). Checker handles K, S (with type-annotated D), Triage, constructors, polymorphism, dependent types, and recursion. Elaborator adaptation is the remaining engineering task.

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

**We don't need the encoding layer.** We can type-check the compiled tree directly — if we annotate S nodes with the one piece of information that's missing: the intermediate type D.

### What We Gain

| | CoC Encoding | Tree-Native (Annotated) |
|---|---|---|
| Term representation | 5-way tagged encoding | Raw tree (leaf/stem/fork) |
| Variable mechanism | Unique marker trees | None needed |
| Opening a binder | Apply body to marker | Nothing (already open) |
| Closing a binder | `abstractOut` — O(n) per binder | Nothing (already closed) |
| Type equality | `convertible` — WHNF + structural compare | `treeEqual` — O(1) hash-consing |
| Dispatch | 5-way TERM_CASE | 3-way triage (built into tree calculus) |
| Environment | Church list (O(n) lookup) | Known-defs map (O(1) lookup) |
| Type annotations | Separate tree | Embedded at S nodes |
| Checker interface | `check(program, type, derivation)` | `check(annotated_tree, type)` |

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

```
Type        = leaf                      The type of types
Tree        = stem(leaf)                The type of all trees
Bool        = stem(stem(leaf))          Booleans (leaf=true, stem(leaf)=false)
Nat         = stem(stem(stem(leaf)))    Naturals (leaf=zero, stem(n)=succ(n))
Pi(A, B)    = fork(A, B)               Dependent function: B computes codomain
A -> B      = fork(A, fork(leaf, B))   Non-dependent: B = K(B₀) returns constant
```

Non-dependent `A -> B` is a special case of `Pi(A, B)` where `B = K(B₀)`. `apply(K(B₀), x) = B₀` for all x.

For dependent functions like `(n : Nat) -> Vec n`, `B` is a tree function: `apply(B, 3) = Vec 3`.

### The Annotated Tree Format

The type annotations are **embedded directly into the program tree**. Only S nodes carry extra information (the intermediate type D). K, Triage, leaf, and stem are identical in raw and annotated form:

```
Raw program              Annotated (superimposed)
────────────             ────────────────────────
fork(leaf, v)            fork(leaf, ann_v)                    K — IDENTICAL
fork(stem(c), b)         fork(stem(D), fork(ann_c, ann_b))   S — D in stem slot
fork(fork(c,d), b)       fork(fork(ann_c, ann_d), ann_b)     Triage — IDENTICAL
leaf                     leaf                                 IDENTICAL
stem(a)                  stem(ann_a)                          IDENTICAL
```

The checker dispatches with **one triage on the left child** — same as for raw programs:
- `leaf` → K rule
- `stem(D)` → S rule (D is right there in the stem)
- `fork(c,d)` → Triage rule

The pure executable program is extractable in O(n):
```
extract(fork(stem(D), fork(c, b))) = fork(stem(extract(c)), extract(b))   -- strip D, repack
extract(fork(leaf, v))             = fork(leaf, extract(v))                -- K: pass through
extract(fork(fork(c, d), b))       = fork(fork(extract(c), extract(d)), extract(b))  -- Triage: pass through
```

### Structural Type Theory Correspondence

The annotated tree is a **proof term in structural type theory** with the typing context distributed at contraction points:

| Structural rule | Combinator | Annotation | Context role |
|---|---|---|---|
| **Weakening** (discard) | K(v) | None | Argument unused → no context entry |
| **Contraction** (share) | S(c, b) | D = type of shared value | Argument used multiply → D records its type |
| **Elimination** (destruct) | Triage(c, d, b) | None | Domain type determines branch types |

In a traditional context `Γ, x : Nat ⊢ add x x : Nat`, the single entry `x : Nat` covers all uses. In the annotated tree, each S node independently records `D = Nat` — the type of the value flowing through that contraction point.

K=weakening and S-involves-contraction are known correspondences (Curry 1963, BCK/BCI logic). The triage=elimination identification and D-as-contraction-witness interpretation are new to tree calculus.

### The Typing Rules

#### Rule 0: Base Types

```
check(t, Tree) = true                           Everything is a tree
check(leaf, Nat) = true                          leaf = zero
check(stem(n), Nat) = check(n, Nat)              stem(n) = succ(n) if n : Nat
check(leaf, Bool) = true                         leaf = true
check(stem(leaf), Bool) = true                   stem(leaf) = false
```

#### Rule 1: K — Weakening (Constant Functions)

`fork(leaf, ann_v)` in annotated form. K ignores its argument; the value `v` must have the codomain type.

```
K(v) : Pi(A, B)    if    v : apply(B, _) for all _ : A
```

Non-dependent case (`B = K(B₀)`): just check `v : B₀`.

Polymorphic case (`A = Type`): introduce abstract type marker α, check `v : apply(B, α)`.

Bool case: exhaustive — check `v : apply(B, true)` and `v : apply(B, false)`.

#### Rule 2: S — Contraction (Shared Application)

`fork(stem(D), fork(ann_c, ann_b))` in annotated form. **D is read directly from the tree.**

```
S(c, b) : Pi(A, B)    if    b : Pi(A, D)
                             c : Pi(A, x → Pi(D(x), B(x)))
```

D is the intermediate type: `b` computes a D-value from the argument, `c` takes both and produces the result.

For non-dependent D and B: `c : A → D₀ → B₀` and `b : A → D₀`.

For dependent S, the checker constructs c's codomain function as a tree:
```
sCodomain(D, B) = S(S(K(leaf), D), S(K(K), B))
```
where `apply(sCodomain(D, B), x) = Pi(apply(D, x), K(apply(B, x)))`.

#### Rule 3: Triage — Elimination (Pattern Matching)

`fork(fork(ann_c, ann_d), ann_b)` in annotated form — identical to raw.

```
triage(c, d, b)(leaf)       = c
triage(c, d, b)(stem(u))    = d(u)
triage(c, d, b)(fork(u, v)) = b(u)(v)
```

Dispatch on the domain type A:

**Tree domain** (all three branches):
```
c : apply(B, leaf)
d : Pi(Tree, stemCodomain(B))      where stemCodomain(B) = S(K(B), leaf)
b : Pi(Tree, forkCodomain(B))      where forkCodomain(B) = S(K(stem(stem(K(B)))), leaf)
```

**Nat domain** (leaf + stem only, no fork nats):
```
c : apply(B, leaf)                  zero case
d : Pi(Nat, stemCodomain(B))        succ case
```

**Bool domain** (leaf + specific stem):
```
c : apply(B, leaf)                  true case
apply(d, leaf) : apply(B, stem(leaf))   false case (stem(leaf) = false)
```

**Abstract domain α** (polymorphic — all branches, identity rule):
```
If output = input in a branch, the type is preserved (identity rule).
I = Triage(leaf, leaf, leaf) : α → α works because each branch returns its input.
```

The dependent codomain helpers are S/K combinators:
```
stemCodomain(B)  = S(K(B), leaf)                    computes [u] apply(B, stem(u))
forkCodomain(B)  = S(K(stem(stem(K(B)))), leaf)     computes [u][v] apply(B, fork(u,v))
sCodomain(D, B)  = S(S(K(leaf), D), S(K(K), B))     computes [x] Pi(D(x), K(B(x)))
```

#### Leaf and Stem as Functions

```
leaf : Nat → Nat          stem(n) = succ(n) preserves Nat
leaf : Tree → Tree         stem(anything) is a tree
leaf : Bool → Nat          stem maps bools to {1, 2}
leaf : Bool → Bool         REJECTED — stem(false) = stem(stem(leaf)) ∉ Bool

stem(leaf) = K : A → (B → A)   fork(leaf, x) = K(x), returns x discarding second arg
```

#### Polymorphism — The Identity Rule

For `K(I) : Pi(Type, α → α)`: introduce abstract type marker α, check `I : α → α`.

In each triage branch over an abstract domain, the checker tracks the **input assumption**: the input value has the domain type. If the output equals the input, the type is preserved.

```
I = Triage(leaf, leaf, leaf) : α → α

leaf branch:  input = leaf : α, output = leaf = input → output : α  ✓
stem branch:  d = leaf, d(u) = stem(u) = input → output : α         ✓
fork branch:  b = leaf, b(u)(v) = fork(u,v) = input → output : α    ✓
```

This handles the polymorphic identity, polymorphic const (`K(K(K))`), and any function that preserves its input.

#### Recursion — FIX Rule

```
fix(step) : T    if    step : T → T
```

The step function is checked against `T → T`. The fixed point is registered in a known-definitions context. At check time, the checker looks up `fix(step)` by tree ID and confirms the type matches.

### The Hierarchy of Power

Each combinator rule corresponds to a type-theoretic capability:

| Rule | Capability | Structural correspondence |
|---|---|---|
| Base types | Membership checking | Type dispatch on stem-chain structure |
| K | Constant functions | Weakening — discard unused argument |
| Triage | Pattern matching / elimination | Induction principle for Tree/Nat/Bool |
| S | Shared arguments / composition | Contraction — use argument multiply |
| Dependent B | Return type depends on input | Curry-Howard (types as propositions) |
| Universes | `Type₀ : Type₁ : Type₂ : ...` | Prevents Girard's paradox |
| Identity types | `Id(A, a, b)` proofs | Hash-consing equality = `fastEq` |

Everything through identity types maps cleanly to tree calculus operations. Univalence and HITs would require mechanisms beyond structural tree identity.

---

## Implementation

### Hash-Consing and Memoization

Trees are hash-consed: structurally equal trees share the same ID. This gives:

- **O(1) type equality**: `treeEqual(a, b)` replaces `convertible(a, b)` — no WHNF or structural comparison needed, since trees are always in normal form.
- **Memoized checking**: cache key `(annotated_tree.id, type.id) → bool`. Sub-expression sharing is automatic — checking the same annotated sub-tree against the same type computes once.
- **D sharing**: if multiple S nodes use the same intermediate type (e.g., D = K(Nat)), hash-consing ensures D is stored once.

Measured on `double = S(S(K(add), I), I) : Nat → Nat`:
```
Check calls (actual work): 6
Cache hits (free):         2
```

The two `I` nodes (hash-consed, same ID) share the `I : Nat → Nat` check result.

### Overhead of the Annotated Format

| Program | Raw nodes | Annotated nodes | Overhead |
|---|---|---|---|
| K(1) | 4 | 4 | +0 (identical) |
| I (identity) | 5 | 5 | +0 (identical) |
| S(K(succ), succ) | 6 | 13 | +7 (one D) |
| double S(S(K(add),I),I) | 26 | 40 | +14 (two D's) |

Only S nodes gain overhead. Cost per S node ≈ `|D|` tree nodes (≈5 for K(Nat)).

### Architecture

```
                    ┌──────────────────────────┐
Surface syntax      │  {x : Nat} -> add x x    │
                    └───────────┬──────────────┘
                                │ parse + elaborate (existing buildWrapped,
                                │   adapted to emit annotated trees)
                    ┌───────────▼──────────────┐
Annotated tree      │  fork(stem(K(Nat)),       │
                    │    fork(                   │
                    │      fork(stem(K(Nat)),    │  ← D=Nat at each S node
                    │        fork(K(add), I)),   │
                    │      I))                   │
                    │                            │
                    │  type = Nat → Nat          │
                    └───────────┬──────────────┘
                                │ check(annotated_tree, type)
                    ┌───────────▼──────────────┐
Tree-native checker │  ONE triage per node:     │
                    │    leaf? → K rule          │
                    │    stem(D)? → S rule       │
                    │    fork(c,d)? → Triage     │
                    │  → true / false            │
                    └───────────┬──────────────┘
                                │ extract(annotated_tree)
                    ┌───────────▼──────────────┐
Executable program  │  S(S(K(add))(I))(I)       │
                    └──────────────────────────┘
```

The checker is a single tree constant built from nested triages. Two inputs: annotated tree + type. One output: bool.

### What the Checker Handles

| Program class | Status | Requirements |
|---|---|---|
| Constants K(v) | Full | None |
| Pattern match Triage | Full | None |
| Identity I | Full | None |
| Constructors leaf/stem | Full | None |
| Composition S(c,b) | Full | D annotation at S node |
| Nested S | Full | D per S node |
| Recursion fix(step) | Full | step : T→T check + context |
| Polymorphic id/const | Full | Abstract type + identity rule |
| Ground-type prelude (not, add, mul...) | Full | D annotations + context |
| Dependent triage (motives) | Full | Codomain helpers |
| Dependent S | Full | sCodomain(D, B) |

**Partially handled**: polymorphic fold (annotation must track type variable R through S nodes), higher-order args (many S nodes = many D annotations), complex recursion (annotation generation challenge).

**Not handled**: Church-encoded types (Pair, Either — need type declaration system), indexed families (Vec n — need W-types), type inference (need constraint solver).

### Remaining Work

**1. Elaborator Adaptation (~300 LOC)** — the main engineering task.

`buildWrapped` in `coc.ts` currently outputs CoC-encoded terms. It needs to output annotated trees:

- Track types during bracket abstraction to emit D at S nodes
- Replace `convertible()` with `treeEqual()` (hash-consing equality)
- Replace `abstractMarkerOut` with direct bracket abstraction into annotated form
- Markers still used internally during elaboration; output is annotation-only

**2. Tree-Constant Checker** — compile the TypeScript checker prototype to a tree calculus function (like `inferStep` in `tree-native.ts`). This makes the checker self-referential: a tree that verifies other trees.

**3. Prelude Validation** — compile each `prelude.disp` definition to an annotated tree and verify the checker accepts it. This is the integration milestone.

### Prior Art and Novelty

Type checking on S/K/I combinators (simple types) is classical — Curry & Feys 1958, Hindley 1969. The K=weakening and S=contraction correspondences date to Curry 1963 and BCK/BCI logic.

Dependent types on combinators is an active research frontier: Altenkirch et al. (FSCD 2023, "Combinatory Logic and Lambda Calculus Are Equal, Algebraically") work algebraically with categories of families; Zaldivar Aiello (2024) adds an M combinator for SKPi. Neither produces a practical checking algorithm.

**Novel contributions of this design**:
1. **Dependent type checking as structural dispatch on tree calculus** — the concrete algorithm
2. **The annotated tree format** — type annotations embedded at S nodes, K/Triage unchanged
3. **Triage as dependent elimination** with stemCodomain/forkCodomain helpers — tree-calculus-specific
4. **The structural type theory correspondence** — D as contraction witness, context distributed at S nodes
5. **Dependent types for tree calculus specifically** — Barry Jay's work only covers simple types / System F

### Validated Test Files

- `test/tree-native-typecheck.test.ts` — core checker prototype (93 tests): K, Triage, S with D annotation, polymorphism, FIX, identity rule
- `test/tree-native-exhibit.test.ts` — concrete (tree, type, annotation) triples for each program class (27 tests)
- `test/tree-native-annotated.test.ts` — annotated format, extraction, memoization, context equivalence (35 tests)
- `test/tree-native-dependent.test.ts` — dependent codomain helpers, dependent triage/S, gap analysis (17 tests)
