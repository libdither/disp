# Tree-Native Type Theory

A dependent type system defined directly over tree calculus terms, where type annotations are embedded into the program tree itself.

## Trees as Typed Combinators

In tree calculus, every tree is one of three things:

```
leaf          The stem constructor: apply(leaf, x) = stem(x)
stem(a)       The fork-with-a constructor: apply(stem(a), x) = fork(a, x)
fork(a, b)    A combinator whose behavior depends on a
```

Every `fork(a, b)` falls into exactly one of three categories:

```
fork(leaf,      v)  =  K(v)        Constant: ignores argument, returns v
fork(stem(c),   b)  =  S(c, b)     Share: applies c and b to argument, combines
fork(fork(c,d), b)  =  Triage      Match: dispatches on argument's tag
```

The compiled tree `S(S(K(add))(I))(I)` already encodes all type-relevant structure: every S says "the argument is used here", every K says "the argument is ignored here", every Triage says "pattern-match on the argument". These are exactly the structural features a type checker needs to verify — if we annotate S nodes with the intermediate type D.

## Types as Trees

```
Type        = leaf                      The type of types
Tree        = stem(leaf)                The type of all trees
Bool        = stem(stem(leaf))          Booleans (leaf=true, stem(leaf)=false)
Nat         = stem(stem(stem(leaf)))    Naturals (leaf=zero, stem(n)=succ(n))
Pi(A, B)    = fork(A, B)               Dependent function: B computes codomain
A -> B      = fork(A, fork(leaf, B))   Non-dependent: B = K(B₀)
```

Non-dependent `A -> B` is `Pi(A, K(B))` where `apply(K(B), x) = B` for all x.

For dependent functions like `(n : Nat) -> Vec n`, `B` is a tree function: `apply(B, 3) = Vec 3`.

## The Annotated Tree Format

Type annotations are embedded directly into the program tree. Only S nodes carry extra information (the intermediate type D). K, Triage, leaf, and stem are identical in raw and annotated form:

```
Raw program              Annotated (superimposed)
────────────             ────────────────────────
fork(leaf, v)            fork(leaf, ann_v)                    K — IDENTICAL
fork(stem(c), b)         fork(stem(D), fork(ann_c, ann_b))   S — D in stem slot
—                        fork(stem(T), stem(body))            Ascription — opaque trusted value
fork(fork(c,d), b)       fork(fork(ann_c, ann_d), ann_b)     Triage — IDENTICAL
leaf                     leaf                                 IDENTICAL
stem(a)                  stem(ann_a)                          IDENTICAL
```

The checker dispatches with **one triage on the left child** — same as for raw programs:
- `leaf` → K rule
- `stem(D)` → S rule (D is right there in the stem); or if right is also `stem`, Ascription rule
- `fork(c,d)` → Triage rule

The pure executable program is extractable in O(n):
```
extract(fork(stem(D), fork(c, b))) = fork(stem(extract(c)), extract(b))   -- strip D
extract(fork(stem(T), stem(body))) = body                                  -- unwrap ascription
extract(fork(leaf, v))             = fork(leaf, extract(v))                -- K: pass through
extract(fork(fork(c, d), b))       = fork(fork(extract(c), extract(d)), extract(b))
```

Ascription nodes wrap opaque values (apply() results, FIX results) whose types are known but whose structure can't be checked. The checker verifies `treeEqual(ascribedType, expectedType)`.

## Typing Rules

### Rule 0: Base Types

```
check(t, Tree) = true                           Everything is a tree
check(leaf, Nat) = true                          leaf = zero
check(stem(n), Nat) = check(n, Nat)              stem(n) = succ(n) if n : Nat
check(leaf, Bool) = true                         leaf = true
check(stem(leaf), Bool) = true                   stem(leaf) = false
```

### Rule 1: K — Weakening (Constant Functions)

`fork(leaf, ann_v)` in annotated form. K ignores its argument; the value `v` must have the codomain type.

```
K(v) : Pi(A, B)    if    v : apply(B, _) for all _ : A
```

Non-dependent case (`B = K(B₀)`): just check `v : B₀`.

Dependent codomain: only Bool is sound (2 inhabitants — exhaustive check):
- Bool case: check `v : apply(B, true)` and `v : apply(B, false)`.
- All other domains (Type, Nat, Tree): **rejected**. Cannot enumerate inhabitants to verify `v` works for all inputs. No abstract markers needed — this trades completeness for soundness.

### Rule 2: S — Contraction (Shared Application)

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

### Rule 3: Triage — Elimination (Pattern Matching)

`fork(fork(ann_c, ann_d), ann_b)` in annotated form — identical to raw.

Dispatch on the domain type A:

**Tree domain** (all three branches):
```
c : apply(B, leaf)
d : Pi(Tree, stemCodomain(B))
b : Pi(Tree, forkCodomain(B))
```

**Nat domain** (leaf + stem only): zero case + succ case.

**Bool domain** (leaf + specific stem): true case + false case.

**Abstract domain α** (polymorphic): identity rule — if output equals input, type is preserved.

### Rule 4: Ascription

`fork(stem(T), stem(body))` — verified wrapper for known definitions.

```
check(ascribe(T, body), expectedType) =
    treeEqual(T, expectedType)  ∧  knownDefs(body.id) = T
```

Ascriptions are only valid when the body is registered in the known-definitions map with a matching type. This prevents adversarial annotated trees from fabricating type claims. Ascriptions arise from two sources: FIX results (one per recursive definition) and `apply()` results during annotation collapse (registered before checking). For base types (stem-shaped: Tree, Bool, Nat), the checker validates the actual value via `isOfBaseType` before the ascription case is reached, so ascriptions only matter for function types.

### Rule 5: FIX — Recursion

```
fix(step) : T    if    step : T → T
```

The step function is checked against `T → T`. The fixed point is stored as an ascription.

### Codomain Helpers

```
stemCodomain(B)  = S(K(B), leaf)                    computes [u] apply(B, stem(u))
forkCodomain(B)  = S(K(stem(stem(K(B)))), leaf)     computes [u][v] apply(B, fork(u,v))
sCodomain(D, B)  = S(S(K(leaf), D), S(K(K), B))     computes [x] Pi(D(x), K(B(x)))
```

## Structural Type Theory Correspondence

The annotated tree is a proof term in structural type theory with the typing context distributed at contraction points:

| Structural rule | Combinator | Annotation | Context role |
|---|---|---|---|
| **Weakening** (discard) | K(v) | None | Argument unused → no context entry |
| **Contraction** (share) | S(c, b) | D = type of shared value | Argument used multiply → D records its type |
| **Elimination** (destruct) | Triage(c, d, b) | None | Domain type determines branch types |

K=weakening and S=contraction are known correspondences (Curry 1963, BCK/BCI logic). The triage=elimination identification and D-as-contraction-witness interpretation are new to tree calculus.

## Hash-Consing and O(1) Type Equality

Trees are hash-consed: structurally equal trees share the same ID. This gives:

- **O(1) type equality**: `treeEqual(a, b)` replaces `convertible(a, b)` — no WHNF needed since trees are always in normal form
- **Memoized checking**: cache key `(annotated_tree.id, type.id) → bool`
- **D sharing**: identical intermediate types at different S nodes are stored once
- **`fastEq` runtime primitive**: O(1) equality via `a.id === b.id`, used by tree-encoded step functions

## Prior Art and Novelty

Type checking on S/K/I combinators (simple types) is classical — Curry & Feys 1958, Hindley 1969. The K=weakening and S=contraction correspondences date to Curry 1963 and BCK/BCI logic.

Dependent types on combinators is an active research frontier: Altenkirch et al. (FSCD 2023, "Combinatory Logic and Lambda Calculus Are Equal, Algebraically") work algebraically with categories of families; Zaldivar Aiello (2024) adds an M combinator for SKPi. Neither produces a practical checking algorithm.

**Novel contributions of this design**:
1. **Dependent type checking as structural dispatch on tree calculus** — the concrete algorithm
2. **The annotated tree format** — type annotations embedded at S nodes, K/Triage unchanged
3. **Ascription nodes** — opaque wrapping for apply()/FIX results, enabling self-contained proof certificates
4. **Triage as dependent elimination** with stemCodomain/forkCodomain helpers
5. **The structural type theory correspondence** — D as contraction witness, context distributed at S nodes
6. **Dependent types for tree calculus specifically** — Barry Jay's work only covers simple types / System F
