# Tree-Native Type Theory

A dependent type system for tree calculus where types are executable predicate trees, type checking is predicate evaluation, and the checker itself is a tree program.

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

Every compiled program is a tree of S/K/Triage nodes. S says "the argument is shared here," K says "the argument is discarded here," Triage says "pattern-match on the argument." These structural features are exactly what a type checker needs to verify — annotated with intermediate types at S nodes.

## Types as Predicates

**The central idea**: a type is a tree that, when applied to a value, returns true (= leaf) or false (= stem(leaf)). Types are not opaque tags — they are executable predicate functions.

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

## Pi Types as Predicate Constructors

A function type `Pi(A, B)` is encoded as `fork(A, B)` where:
- `A` is the domain predicate (a type)
- `B` is the codomain family: a tree function where `apply(B, x)` gives the type (predicate) of the output when the input is `x`

**Non-dependent arrows**: `A -> C` = `fork(A, K(C))` = `fork(A, fork(leaf, C))`. The codomain is constant — `apply(K(C), x) = C` for all x.

**Dependent functions**: `(n : Nat) -> Vec n` = `fork(Nat, Vec)`. The codomain varies with the input — `apply(Vec, 3)` gives the predicate for 3-element vectors.

The codomain family is itself a tree, computed by bracket-abstracting the bound variable from the codomain type expression. This uses the same compilation pipeline as values:
```
-- For (n : Nat) -> Vec n -> Vec (succ n):
-- Codomain family B = [n] fork(apply(Vec, n), K(apply(Vec, stem(n))))
-- B = S(S(K(leaf), Vec), S(K(K), S(K(Vec), leaf)))
-- apply(B, 3) = fork(Vec(3), K(Vec(4)))  = Vec 3 -> Vec 4
```

**Type compilation = value compilation.** Types are bracket-abstracted from symbolic expressions using the same Expr → bracketAbstract → collapse pipeline. No separate type-level machinery.

## The Pi Predicate (Type Checker as a Tree)

Pi is a predicate CONSTRUCTOR: `apply(apply(Pi, A), B)` produces a predicate that checks annotated trees against the function type `Pi(A, B)`. The Pi predicate dispatches on the annotated tree's combinator structure:

```
Pi(A, B)(ann) = dispatch on ann:
  K node  → check inner value against codomain
  S node  → check sub-predicates compose correctly via D annotation
  Triage  → check branches against domain-specific codomains
  Ascription → verify claimed type matches expected type
  I       → accept (identity preserves types)
```

Every type IS a predicate. Base types (Nat, Bool) dispatch on value structure. Pi types dispatch on annotation structure. The universal "type check" operation is simply `apply(type, value_or_annotated_tree)`.

With hash-consing, Pi is stored once. Each concrete Pi type `Pi(Nat, K(Bool))` adds one node (the partial application). No redundancy.

## The Annotated Tree Format

Type annotations are embedded directly into the program tree. Only S nodes carry extra information (the intermediate type D). K, Triage, leaf, and stem are identical in raw and annotated form:

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

The checker dispatches with **one triage on the left child** — same as for raw programs:
- `leaf` → K rule
- `stem(D)` → S rule (D is right there); or if right is also `stem`, Ascription rule
- `fork(c,d)` → Triage rule

**Annotations are predicates.** D at S nodes is a predicate (or predicate family). The annotated tree is literally "a program with predicates cached at each composition point."

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

### Rule 0: Base Types — Predicate Evaluation

For base types (Nat, Bool, Tree), checking IS running the predicate:

```
check(value, Nat)  = apply(Nat, value)     Runs NatPred on the value
check(value, Bool) = apply(Bool, value)    Runs BoolPred on the value
check(value, Tree) = apply(Tree, value)    Always true (Tree = K(true))
```

No hardcoded rules — the predicate defines what values the type accepts. Adding a new base type means defining a new predicate in `.disp`.

### Rule 1: K — Weakening (Constant Functions)

`fork(leaf, ann_v)` in annotated form. K ignores its argument; the value must satisfy the codomain.

```
K(v) : Pi(A, B)    if    v : apply(B, _) for all _ : A
```

Non-dependent case (`B = K(B₀)`): check `v : B₀`. The codomain is constant, so just verify the inner value.

Dependent codomain over finite domain (Bool, 2 inhabitants): exhaustive check:
- Check `v : apply(B, true)` and `v : apply(B, false)`.

Dependent codomain over infinite domain (Nat, Tree, Type): **ascription required**. Cannot enumerate inhabitants. (See "When Ascriptions Are Needed" below.)

### Rule 2: S — Contraction (Shared Application)

`fork(stem(D), fork(ann_c, ann_b))` in annotated form. **D is read directly from the tree.**

```
S(c, b) : Pi(A, B)    if    b : Pi(A, D)
                             c : Pi(A, sCodomain(D, B))
```

D is the intermediate predicate family: `b` computes a D-value from the argument, `c` takes the argument and the D-value and produces the result.

The checker recursively verifies both sub-terms against their derived types. D was placed by the elaborator during bracket abstraction — it captures how the intermediate type depends on the input. **D is sound for any choice** — if both sub-checks pass, the composition is valid.

For non-dependent D and B: `c : A → D₀ → B₀` and `b : A → D₀`.

For dependent S:
```
sCodomain(D, B) = S(S(K(leaf), D), S(K(K), B))
```
where `apply(sCodomain(D, B), x) = Pi(apply(D, x), K(apply(B, x)))`.

### Rule 3: Triage — Elimination (Pattern Matching)

`fork(fork(ann_c, ann_d), ann_b)` in annotated form — identical to raw.

Dispatch on the domain predicate A. The Pi predicate inspects A's triage structure to determine which branches to check:

**Tree domain** (all three branches):
```
c : apply(B, leaf)
d : Pi(Tree, stemCodomain(B))
b : Pi(Tree, forkCodomain(B))
```

**Nat domain** (leaf + stem): zero case + succ case. Fork handler unchecked (no valid Nat is a fork).

**Bool domain** (leaf + specific stem): true case + false case.

The Pi predicate determines the domain by comparing A against known predicates (via hash-consing O(1) equality). This is extensible — new domains can be added as new predicates.

### Rule 4: leaf/stem as Functions — Predicate Inspection

When `leaf` or `stem(a)` appears as an annotated tree at a Pi type, the checker verifies the tree constructor preserves types by **inspecting the codomain predicate's triage structure**. No free variables are involved — the reasoning is purely about the predicate tree's own structure.

**Leaf case**: `leaf : Pi(A, K(B₀))` means "does the stem constructor map A-values to B₀-values?" Since `apply(leaf, x) = stem(x)`, the question is: does B₀ accept `stem(x)` for all `x : A`?

The checker answers this WITHOUT reasoning about `x`:

```
leafCheck(A, B₀):
  -- B₀(stem(x)) = apply(B₀, stem(x))
  -- If B₀ is triage-shaped: B₀ = fork(fork(c, d), handler)
  --   then apply(B₀, stem(x)) = apply(d, x)  (triage stem rule)
  -- So "B₀ accepts all stems of A-values" = "d accepts all A-values" = "A implies d"

  d := B₀.left.right          -- extract stem handler from B₀'s triage structure
  if FAST_EQ(d, A): true      -- stem handler IS the domain predicate → trivially preserves
  if A is finite: enumerate    -- check apply(B₀, stem(v)) for each v in A
  else: ascription needed      -- can't verify for infinite domains
```

Two tree field accesses + one O(1) identity comparison. No `x`, no `stem(x)`, no free variables. The predicate's structure answers the question.

**Example**: `leaf : Pi(Nat, K(Nat))`
```
B₀ = Nat = fork(fork(leaf, Nat), forkHandler)
d  = Nat.left.right = Nat           -- Nat's stem handler is Nat itself!
FAST_EQ(Nat, Nat) = true            -- same hash-cons ID
→ true ✓                            -- stem preserves Nat-ness (succ maps Nat to Nat)
```

**Example**: `leaf : Pi(Bool, K(Bool))`
```
B₀ = Bool = fork(fork(leaf, isLeaf), forkHandler)
d  = Bool.left.right = isLeaf       -- Bool's stem handler is isLeaf
FAST_EQ(isLeaf, Bool) = false       -- different trees!
Bool is finite → enumerate:
  apply(Bool, stem(leaf)) = apply(isLeaf, leaf) = true      ✓  (stem(true) = false, a Bool)
  apply(Bool, stem(stem(leaf))) = apply(isLeaf, stem(leaf)) = false  ✗  (stem(false) not a Bool)
→ false ✗                           -- correctly rejected!
```

**Stem case**: `stem(a) : Pi(A, K(B₀))` means "does `fork(a, -)` preserve A → B₀?" The checker inspects B₀'s fork handler similarly. This covers `stem(leaf)` used as the 2-arg fork constructor.

**K-shaped codomain** (B₀ = K(true) = Tree): If the codomain accepts everything, leaf/stem trivially preserve it. The checker detects K-shaped predicates and short-circuits.

This predicate inspection mechanism means **basic constructors do not need ascription**. The trust root for `succ : Nat → Nat` is Nat's own triage structure — Nat's stem handler being Nat is what MAKES succ type-correct.

### Rule 5: I — Identity (Variable Rule)

The identity combinator `I = fork(fork(leaf, leaf), leaf)` is the analog of the variable rule in standard type theory (`Γ, x:A ⊢ x : A`). It is recognized by the checker and accepted at any `Pi(A, K(A))`:

```
I : Pi(A, K(A))    for any A
```

This is the base case of the structural recursion — every bracket-abstracted variable becomes I, and I preserves types.

### Rule 6: Ascription — Opaque Declarations

`fork(stem(T), stem(body))` — wraps a value with its declared type.

```
check(ascribe(T, body), expectedType) = treeEqual(T, expectedType)
```

Ascriptions declare that `body` has type `T`. The checker verifies the claimed type matches the expected type via O(1) hash-consing equality.

### Rule 7: FIX — Recursion

```
fix(step) : T    if    step : T → T
```

The step function is checked against `T → T`. The fixed point is an opaque tree stored as an ascription.

## When Ascriptions Are Needed

Ascription is the mechanism for cases where the checker cannot structurally derive the type from the annotated tree and predicate structure alone. Intuitively, **ascription is needed when the checking problem requires information that isn't present in the tree**.

### What the checker CAN derive (no ascription needed)

The checker can verify types when the annotated tree provides enough structural information:

- **Base values**: `leaf : Nat`, `stem(leaf) : Bool` — run the predicate directly. The predicate IS the specification.
- **Constructors via predicate inspection**: `leaf : Pi(Nat, K(Nat))` — Nat's triage structure proves that its stem handler preserves Nat-ness. The trust root is the predicate definition itself.
- **I combinator**: `I : Pi(A, K(A))` — recognized and accepted at any type. The variable rule.
- **S compositions**: `S(c, b) : Pi(A, B)` — the D annotation at the S node tells the checker everything. Recursively verify sub-terms.
- **K with non-dependent codomain**: `K(v) : Pi(A, K(B₀))` — check `v : B��`. The codomain is constant, so K-wrapping is trivially sound.
- **K with dependent codomain over finite domain**: `K(v) : Pi(Bool, B)` — exhaustively check both Bool inhabitants.
- **Triage nodes**: branches checked against domain-specific codomains derived from the predicate.

### What requires ascription

Ascription is needed when the checker faces a **fundamentally undecidable or structurally opaque** problem:

**1. Recursive definitions** (`fix`-wrapped results):

```
let rec add : Nat → Nat → Nat := {m n} -> natElim Nat n ({m'} -> succ (add m' n)) m
```

`fix(step)` produces an opaque tree whose internal structure doesn't match the annotation format. The checker can't unfold the fixed point. The elaborator verifies the step function has type `T → T` and ascribes the result.

*Intuition*: Recursion introduces infinite behavior. The checker works on finite tree structure. Ascription bridges the gap — the elaborator verified the ONE step, and fix preserves types by induction.

**2. Polymorphic K over infinite domains**:

```
let id : (A : Type) → A → A := {A x} -> x
```

Compiles to `K(I)`. The checker sees K with dependent codomain `B(A) = A → A` over Type domain (infinite). To verify K(I), it would need `I : A → A` for ALL types A — infinitely many checks.

*Intuition*: Parametric polymorphism is a universal statement ("for all types"). Checking universal statements over infinite domains requires proof, not enumeration. Ascription is the programmer's assertion of this universal truth. At each USE site (`id Nat 3`), the concrete instantiation IS checked.

**3. S(K(p), K(q)) optimization results**:

When bracket abstraction optimizes `S(K(p), K(q))` to `K(apply(p, q))`, the inner `apply(p, q)` is an eagerly evaluated tree. Its combinator structure has been destroyed by evaluation — there are no S/K/Triage annotations to check.

*Intuition*: Evaluation collapses structure. The annotated tree preserves structure for checking, but evaluated sub-expressions lose it. Ascription re-attaches the type to the opaque result.

**4. Constructor types that predicate inspection can't handle**:

When the codomain predicate is S-shaped (not triage-shaped or K-shaped), the checker can't extract a stem/fork handler to inspect. Complex user-defined type families may produce predicates with non-standard structure.

*Intuition*: Predicate inspection works because triage-shaped predicates have a known, inspectable structure (leaf/stem/fork handlers). When a predicate is built from S combinators or other complex patterns, the checker can't reason about its behavior from structure alone.

### Ascription soundness analysis

**Can ascription introduce unsoundness?** Yes — a wrong ascription like `ascribe(Pi(Nat, K(Bool)), LEAF)` (claiming leaf maps Nat to Bool) would be accepted by the checker. But the damage is bounded:

```
wrong_succ 3:
  Domain check: apply(Nat, 3) = true ✓     -- argument check passes (Nat is correct)
  Result type: Bool                          -- from the wrong ascription
  Runtime result: stem(stem(stem(stem(leaf)))) = 4   -- actually a Nat, not a Bool

-- At the NEXT use site:
boolElim (wrong_succ 3) t f:
  Domain check: apply(Bool, 4) = false ✗    -- CAUGHT! 4 is not a Bool
```

A wrong ascription propagates exactly ONE step. The next predicate evaluation on the wrongly-typed value catches the error. The blast radius is bounded to one application — the value cannot silently corrupt further computation without hitting a predicate check.

**The one exception**: if the wrongly-typed value flows into a `Tree`-typed context (Tree accepts everything), the error is never caught. But in that context, the value is treated as an opaque tree with no type-specific operations, so the unsoundness is harmless — no type-dependent decision is made on the wrong value.

### Why each ascription case is justified

**Recursive definitions**: The elaborator structurally verifies `stepFn : T → T` before ascribing `fix(stepFn) : T`. The ascription is justified by induction — if the step preserves the type, the fixed point has the type. The only trust is that the fix combinator correctly computes fixed points, which is a one-time property of the combinator itself.

**Polymorphic K over infinite domains**: `K(I) : (A:Type) → A → A`. The universal claim ("for all types A") can't be checked by enumeration. But at every concrete use site (`id Nat 3`), the specific instantiation IS checked — the domain predicate runs on the argument, and the codomain family evaluates to a concrete type. Wrong polymorphic ascriptions would be caught the moment a specific instantiation fails.

**S(K(p),K(q)) optimization**: The optimization `S(K(p), K(q))(x) = apply(p, q)` is mathematically equivalent to the un-optimized form. The ascription preserves the type the un-optimized tree would have had. This could be eliminated entirely by running the codomain predicate on `apply(p, q)` — a single predicate evaluation per optimization. Worth considering as a defense-in-depth measure.

### The trust model

Ascription is NOT blind trust. Each ascription is:
1. **Created only by the elaborator** during compilation — not user-facing
2. **Justified by structural reasoning** — the elaborator verifies a related structural property (step function type, concrete instantiations) before creating the ascription
3. **Verified at every use site** — domain predicates RUN on concrete arguments, codomain families EVALUATE to concrete types
4. **Bounded blast radius** — wrong ascriptions caught at the next predicate evaluation
5. **O(1) to verify** — just a hash-cons ID comparison between claimed and expected type

The trust surface is small and enumerable: recursive definitions, polymorphic abstractions, and compilation artifacts. Everything else is structurally verified or predicate-checked.

**Optional defense-in-depth**: For ascribed functions, the elaborator could insert runtime result checks — after `apply(f, x)`, also verify `apply(codomain_pred, result) = true`. This catches wrong ascriptions immediately at the cost of one extra predicate evaluation per ascribed application. Not required for soundness of well-formed programs, but useful for debugging and catching elaborator bugs.

## Dependent Type Checking: How It Works

### The Elaborator's Role

The elaborator takes surface syntax and produces annotated trees. It works in two phases:

**Phase 1 (Symbolic)**: Bidirectional type inference with named variables. Types are tracked symbolically (as SExpr). The elaborator resolves names, checks applications, and tracks how types depend on bound variables.

**Phase 2 (Compilation)**: Bracket-abstract all lambdas into S/K/I combinators. At each S node, record D — the intermediate predicate family. Compile symbolic types to tree predicates by bracket-abstracting bound variables from type expressions. Collapse to annotated tree + raw tree.

**Key insight**: Phase 2 for types uses the same bracket abstraction as for values. A dependent codomain like `{n} -> Vec n -> Vec (succ n)` is compiled by:
1. Building the type as an Expr with `n` as a free variable
2. Bracket-abstracting `n` out
3. Collapsing to a tree-level function (the codomain family)

After Phase 2, there are no variables — only tree-level codomain families at S nodes and in Pi types.

### Dependent Instantiation = Tree Reduction

When a dependent function is applied to a concrete argument, the codomain family is EVALUATED:

```
id : fork(Tree, B)    where B = fork(stem(leaf), stem(leaf))
                      and apply(B, A) = fork(A, K(A)) = A → A

id Nat:
  1. Domain check: apply(Tree, Nat) = true ✓
  2. Result type: apply(B, Nat) = fork(Nat, K(Nat)) = Nat → Nat  ← CONCRETE
```

The codomain family B is a tree. Applying it to a concrete argument gives a concrete type. No variables, no markers, no symbolic evaluation — just tree calculus `apply()`.

**Dependent type checking never needs abstract variables.** The elaborator computes codomain families while variables are in scope (Phase 1-2). After that, all type-level computation happens on concrete values at application sites.

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

-- Constructors (ascribed — like axioms in standard type theory)
vnil  : Vec 0                                      -- vnil = leaf
vcons : (n : Nat) -> Tree -> Vec n -> Vec (succ n) -- vcons n h t = fork(h, t)
```

Vec IS a tree — a recursive predicate. `apply(apply(Vec, 2), fork(a, fork(b, leaf)))` reduces to true/false. The predicate defines what valid vectors look like; the constructors are ascribed.

**Application-time checking** (where types resolve concretely):
```
vcons 0 c vnil:
  1. vcons 0: apply(B_vcons, 0) = Tree -> Vec(0) -> Vec(1)    ← concrete type!
  2. vcons 0 c: apply(Tree, c) = true. Result: Vec(0) -> Vec(1)
  3. vcons 0 c vnil: apply(apply(Vec, 0), leaf) = isLeaf(leaf) = true ✓
  Result type: Vec(1)
```

Every step runs concrete predicate evaluation. The codomain family for vcons was computed once at definition time; at each application, it's instantiated by tree reduction.

## Codomain Helpers

Tree-level functions that compute derived codomain families:

```
stemCodomain(B)  = S(K(B), leaf)                    computes [u] apply(B, stem(u))
forkCodomain(B)  = S(K(stem(stem(K(B)))), leaf)     computes [u][v] apply(B, fork(u,v))
sCodomain(D, B)  = S(S(K(leaf), D), S(K(K), B))     computes [x] Pi(D(x), K(B(x)))
```

These are ordinary tree combinators. The checker uses them to derive the types of sub-terms in S and Triage rules.

## Bootstrap Architecture

The type system bootstraps from tree calculus itself:

```
1. types.disp    — Defines predicates (Nat, Bool, Tree, Vec), fix, wait, triage
                    Compiled by bare (untyped) bracket abstraction
                    All definitions get default type Tree

2. Typed env     — Built from types.disp output:
                    Type names: Nat, Bool, Tree → predicate trees typed as Type
                    Constructors: zero, succ, true, false → ascribed with their types
                    Eliminators: boolElim, natElim → ascribed with dependent types

3. prelude.disp  — User-level definitions (not, and, add, mul, id, Pair, etc.)
                    Compiled by typed elaborator
                    Each definition checked against its type annotation
```

**Predicates bootstrap themselves.** `types.disp` is compiled in bare mode (no types needed). The predicate trees it produces become the type system for everything after. The compiler for types is the same compiler for values.

## Structural Type Theory Correspondence

The annotated tree is a proof term in structural type theory with the typing context distributed at contraction points:

| Structural rule | Combinator | Annotation | Context role |
|---|---|---|---|
| **Weakening** (discard) | K(v) | None | Argument unused → no context entry |
| **Contraction** (share) | S(c, b) | D = intermediate predicate | Argument used multiply → D records its type |
| **Elimination** (destruct) | Triage(c, d, b) | None | Domain predicate determines branch types |
| **Variable** | I | None | Identity preserves types (base case) |

K=weakening and S=contraction are known correspondences (Curry 1963, BCK/BCI logic). The triage=elimination identification and D-as-contraction-witness interpretation are specific to tree calculus.

## Swappable Type Systems

Different type systems are different Pi predicates sharing the same base predicates:

| Type system | Pi behavior | Accepts |
|---|---|---|
| **Simple** | K: non-dependent only. S: non-dependent D. | Simply-typed programs |
| **Dependent** | K: exhaustive for Bool, ascription for infinite. S: dependent D,B. | Dependently-typed programs |
| **System F** | K: polymorphic over Type via ascription. S: type-level D. | Polymorphic programs |

Base predicates (Nat, Bool, Tree) are **identical across all type systems** — the same tree, the same behavior. Only the Pi predicate differs. An annotated tree produced by one type system can be re-checked by another; a simply-typed program passes both simple and dependent checkers.

Hash-consing prevents accidental mixing: predicates from different Pi instances are different trees, so D annotations from one system are incompatible with another system's expectations.

## Hash-Consing and O(1) Type Equality

Trees are hash-consed: structurally equal trees share the same ID.

- **O(1) type equality**: `treeEqual(a, b)` via `a.id === b.id` — no normalization needed since trees are always in normal form
- **O(1) ascription verification**: claimed type equals expected type in one comparison
- **Memoized checking**: cache key `(annotated_tree.id, type.id) → bool` prevents redundant work
- **D sharing**: identical intermediate predicates at different S nodes stored once
- **Codomain family sharing**: `Pi(Nat, K(Bool))` at different program points is one tree

## Prior Art and Novelty

Type checking on S/K/I combinators (simple types) is classical — Curry & Feys 1958, Hindley 1969. The K=weakening and S=contraction correspondences date to Curry 1963 and BCK/BCI logic.

Barry Jay's work on tree calculus typing ("Typed Program Analysis without Encodings," PEPM 2025) uses structural program types with subtyping — types mirror tree structure (`Leaf`, `Stem(T)`, `Fork(T,U)`), and polymorphism arises from subtyping on program types. His types are inert data in a separate grammar; dependent types are not supported because type-level computation requires types to be executable.

Altenkirch et al. (FSCD 2023, "Combinatory Logic and Lambda Calculus Are Equal, Algebraically") work algebraically with categories of families. Zaldivar Aiello (2024) adds an M combinator for SKPi. Neither produces a practical checking algorithm.

**Novel contributions of this design**:
1. **Types as executable predicates** — types are tree programs that check membership, not inert data
2. **Dependent type checking as tree reduction** — codomain families are trees; instantiation is `apply()`
3. **The annotated tree format** — type predicates embedded at S nodes, annotations are predicates
4. **Self-bootstrapping** — predicates compiled by the same bracket abstraction used for programs
5. **Pi as a tree-level predicate constructor** — the type checker is a tree, parameterizable for different type systems
6. **Constructor typing via predicate inspection** — the checker derives `succ : Nat → Nat` by inspecting Nat's triage structure, finding its stem handler is Nat itself. No external axioms needed for basic constructors. The predicate IS the trust root.
7. **No abstract variables in checking** — codomain families eliminate variables; all checking is on concrete trees. Predicate inspection reasons about predicates, not hypothetical values.
8. **Ascription for the genuinely opaque** — only recursion, polymorphism over infinite domains, and evaluation artifacts need trusted declarations. Everything else is derived structurally.
9. **Structural type theory with predicate semantics** — D as contraction witness, triage as elimination, predicates as types
