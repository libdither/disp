# Tree-Native Type Theory

A dependent type system defined directly over tree calculus terms, where type annotations are embedded into the program tree itself.

**Status**: Native pipeline is primary. All 619 tests passing. Checker handles K, S (with D annotation), Triage, constructors, polymorphism, dependent types, recursion, and ascription. CoC pipeline retained only for legacy test compatibility.

## Architecture (Current)

```
Surface syntax     {x : Nat} -> add x x
                        |
                   Parse (src/parse.ts)
                        |
                   Elaborate (src/tree-native-elaborate.ts)
                   - buildNativeWrapped: SExpr → TypedExpr + type
                   - typedBracketAbstract: TypedExpr → TypedExpr (with tI/tK/tS)
                   - collapseToAnnotated: TypedExpr → annotated tree
                   - collapseTypedExpr: TypedExpr → raw program tree
                        |
                   Compile (src/compile.ts) — trusted backend
                   - astToExpr + bracketAbstract + collapseAndEval
                        |
                   Check (src/tree-native-checker.ts)
                   - checkAnnotated(defs, annotatedTree, type) → bool
                        |
                   Runtime — apply() with hash-consing + memoization
```

### Key Files

| File | Role |
|------|------|
| `src/tree-native-checker.ts` | Checker: `checkAnnotated(defs, ann, type) → bool`, type constants, annotation constructors, codomain helpers |
| `src/tree-native-elaborate.ts` | Elaborator: `buildNativeWrapped`, `typedBracketAbstract`, `collapseToAnnotated`, `nativeElabDecl`, `printNativeType` |
| `src/native-utils.ts` | Shared: `freshMarker`, `registerNativeBuiltinId`, `isNativeBuiltin`, cache callbacks |
| `src/prelude.ts` | Prelude loading: `loadPrelude` (native-only), `processDecl` (native with graceful fallback) |
| `src/repl.ts` | REPL: fully native, no CoC fallback |
| `src/compile.ts` | Compiler: `astToExpr` + bracket abstraction + `collapseAndEval`. Trusted compilation backend |
| `src/coc.ts` | Legacy CoC encoding. Retained for tree-encoded step functions and tests |

---

## Type System

### Types as Trees

```
Type        = leaf                      The type of types
Tree        = stem(leaf)                The type of all trees
Bool        = stem(stem(leaf))          Booleans (leaf=true, stem(leaf)=false)
Nat         = stem(stem(stem(leaf)))    Naturals (leaf=zero, stem(n)=succ(n))
Pi(A, B)    = fork(A, B)               Dependent function: B computes codomain
A -> B      = fork(A, fork(leaf, B))   Non-dependent: B = K(B₀)
```

Non-dependent `A -> B` is `Pi(A, K(B))` where `apply(K(B), x) = B` for all x.

### Annotated Tree Format

Type annotations embedded directly into program trees. Only S nodes carry extra information (intermediate type D):

```
fork(leaf, ann_v)                      K — unchanged
fork(stem(D), fork(ann_c, ann_b))      S — D in stem slot
fork(stem(T), stem(body))              Ascription — "body has type T, trust it"
fork(fork(ann_c, ann_d), ann_b)        Triage — unchanged
leaf                                   constructor — unchanged
stem(ann_a)                            constructor — unchanged
```

Ascription nodes wrap opaque values (apply() results, FIX results) whose types are known but whose structure can't be checked. The checker verifies `treeEqual(ascribedType, expectedType)`.

### Typing Rules

**K (Weakening):** `K(v) : Pi(A, B)` if `v : apply(B, _)` for all `_ : A`.

**S (Contraction):** `S(c, b) : Pi(A, B)` if `b : Pi(A, D)` and `c : Pi(A, x → Pi(D(x), B(x)))`. D is read directly from the annotated S node.

**Triage (Elimination):** `triage(c, d, b) : Pi(A, B)` — dispatches on domain type A (Tree, Nat, Bool, abstract).

**Ascription:** `ascribe(T, body) : T` — trusted, types must match exactly.

**FIX:** `fix(step) : T` if `step : T → T`. Result stored as ascription.

### Codomain Helpers (Tree Combinators)

```
stemCodomain(B)  = S(K(B), leaf)                    [u] apply(B, stem(u))
forkCodomain(B)  = S(K(stem(stem(K(B)))), leaf)     [u][v] apply(B, fork(u,v))
sCodomain(D, B)  = S(S(K(leaf), D), S(K(K), B))     [x] Pi(D(x), K(B(x)))
```

---

## Elaboration Pipeline

### TypedExpr

The elaborator works on `TypedExpr`, an intermediate representation that preserves combinator structure:

```typescript
type TypedExpr =
  | { tag: "tlit", tree: Tree, annTree: Tree, type: Tree }  // known tree literal
  | { tag: "tfvar", name: string, type: Tree }               // bound variable (for bracket abstraction)
  | { tag: "tapp", func: TypedExpr, arg: TypedExpr, type: Tree }
  | { tag: "tI", type: Tree }                                // I combinator
  | { tag: "tK", value: TypedExpr, type: Tree }              // K(v) combinator
  | { tag: "tS", d: Tree, c: TypedExpr, b: TypedExpr, type: Tree } // S(c,b) with D annotation
```

Key functions:
- `collapseTypedExpr(texpr) → Tree` — extract raw program
- `collapseToAnnotated(texpr) → Tree` — produce annotated tree with D annotations + ascriptions

### buildNativeWrapped

Elaborates `SExpr → { texpr: TypedExpr, type: Tree }` with:
- `boundNames: Set<string>` — lambda-bound variables produce `tfvar` (not `tlit`)
- `collapseForType(texpr, env)` — resolves `tfvar` to markers for type-level computation
- Pi binder markers in env for dependent type computation

### typedBracketAbstract

`(varName, texpr, varType) → { result: TypedExpr, codomain: Tree }`

Returns a TypedExpr (not collapsed tree), preserving structure for outer abstractions. This fixes multi-param lambda type tracking:

```
{x y} -> add x y
  inner [y]: returns tapp(tlit(ADD), tfvar("x")) via eta — NOT collapsed
  outer [x]: sees tfvar("x") inside, S case fires correctly
```

Optimizations: eta reduction, S(K(p))(K(q)) = K(apply(p,q)), S(K(p))(I) = p.

### processDecl (Graceful Degradation)

```
1. Try nativeElabDecl (full type check + annotated tree)
2. If that fails:
   a. Compile via compile.ts (always succeeds)
   b. Try elaborating just the type annotation
   c. Register with ascription-wrapped compiled tree
   d. checkOk = false
```

This ensures all definitions are available at runtime even when the native elaborator can't handle their types.

---

## Current Limitations and Next Steps

### The Pi Domain Erasure Problem

**This is the single biggest limitation and the most impactful next change.**

`compile.ts` line 59-68 erases Pi type domains:

```typescript
case "spi":
  // Pi types erase to lambdas (domain is dropped)
  // (x : A) -> B  compiles same as {x} -> B
  return bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
```

**Impact**: When a type expression like `Pair A B` is evaluated at runtime, `Pair` was compiled with all Pi domains erased. So `Pair Nat Bool` evaluates to `stem(LEAF)` (≈ `{R _} -> R` with domains dropped) instead of a proper native Pi type `fork(Type, [R] fork(Nat->Bool->R->R, K(R)))`.

**Consequence**: The elaborator can't decompose user-defined type constructors:
- `mkPair : (A:Type) -> (B:Type) -> A -> B -> Pair A B` — the lambda body needs `Pair A B` to be a valid `fork(domain, codomain)` to type-check the remaining params (R, f)
- `natElim R z s n` — the motive R is applied as a constant function, but natElim's first param expects `Nat -> Type`, and `R : Type` doesn't match
- All Church-encoded types (Pair, Either, records, coproducts) fall back to `TN_TREE`

**Currently affected definitions**: mkPair, fst, snd, inl, inr (types degrade to Tree). These compile and run correctly, but type display is wrong.

### Fix: Preserve Pi Domains in Compilation

Change `compile.ts` spi case to preserve domain information:

```typescript
case "spi":
  // Pi types as values: fork(domain, [x]codomain)
  // This preserves type structure so Pair A B evaluates to a proper Pi type
  const domain = astToExpr(ast.domain, defs)
  const codomain = ast.name === "_"
    ? bracketAbstract("_$pi", astToExpr(ast.codomain, defs))
    : bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
  return eApp(eApp(eTree(LEAF), domain), codomain)
  // This compiles to fork(domain_tree, codomain_tree) = native Pi encoding
```

Wait — `eApp(eApp(eTree(LEAF), domain), codomain)` collapses to `apply(apply(LEAF, domain_tree), codomain_tree)` = `apply(stem(domain_tree), codomain_tree)` = `fork(domain_tree, codomain_tree)`. This is exactly the native Pi type `fork(A, B)`.

**Considerations**:
1. `stype` and `stree` also need updating: `Type` should compile to `LEAF` (already does), `Tree` should compile to `stem(LEAF)` (currently compiles to `LEAF` — wrong, should be `TN_TREE`).
2. After this change, `Pair Nat Bool` evaluates to a valid native Pi type. The elaborator can decompose it as `fork(domain, codomain)` and continue type-checking.
3. This changes the compiled representation of ALL type expressions. Existing compiled trees for type-valued definitions change. Tests that compare exact tree shapes need updating.
4. Runtime behavior for VALUE-level code is unaffected — Pi types only appear in type annotations and type-valued definitions.

**Testing strategy**: After making the change, run all 619 tests. Failures will indicate where compiled type representations changed. The native verification pass rate (currently 7/22 prelude defs) should improve significantly since the elaborator can now handle Church-encoded types.

### Dependent Elimination Motives

Even after Pi domain preservation, there's a type-level mismatch:

```
natElim : (M : Nat -> Type) -> M 0 -> ((n:Nat) -> M n -> M (succ n)) -> Nat -> M n
```

When called as `natElim R z s n` where `R : Type`, the first argument `R` has type `Type` but the domain expects `Nat -> Type`. In the CoC pipeline, this works because application is untyped. In the native elaborator, `treeEqual(TN_TYPE, fork(TN_NAT, K(TN_TYPE)))` fails.

**Options**:
1. **Implicit coercion**: When the expected domain is `A -> Type` and the argument has type `Type`, wrap the argument in `K(arg)` automatically. This matches the common pattern of constant motives.
2. **Subtyping**: Treat `Type` as a subtype of `A -> Type` (since any type can be used as a constant function). This is sound because `apply(type_tree, anything) = stem(type_tree)` which IS a valid tree (just not the right one without Pi preservation).
3. **Let the user write K explicitly**: `natElim ({_} -> R) z s n` instead of `natElim R z s n`. Most explicit, least convenient.

Option 1 is the most pragmatic and matches user expectations.

### After Pi Domain Preservation — Remaining Work

Once Pi domains are preserved and constant-motive coercion is added:

1. **Full prelude verification**: All 22 prelude defs should produce correct annotated trees that pass `checkAnnotated`. This validates the entire native pipeline.

2. **Remove compile.ts as trusted backend**: Currently `processDecl` compiles via compile.ts independently. Once `extract(collapseToAnnotated(texpr))` produces the same trees as `compileAndEval(value, defs)`, the elaborator can be the sole compilation path.

3. **Encode checker as a tree constant**: `checkAnnotated` is a pure function `(annotatedTree, type) → bool`. Compile it to a tree calculus combinator (like `inferStep` in `tree-native.ts`). This makes the checker self-referential: a tree that verifies other trees.

4. **Remove legacy CoC code**: Once all tests use the native pipeline, remove `loadCocPrelude`, `processDeclCoc`, `buildNameMap`, `annotateTree`, `convertCocType`, `nativeCheckDecl`, and `coc-prelude.disp`.

5. **Scoring functions (GOALS.md)**: With a tree-encoded checker, define scoring functions that combine type-checking (0/1) with program size/performance metrics. This is the input to the optimizer.

---

## Theory

### Trees as Typed Combinators

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

### Structural Type Theory Correspondence

| Structural rule | Combinator | Annotation | Context role |
|---|---|---|---|
| **Weakening** (discard) | K(v) | None | Argument unused |
| **Contraction** (share) | S(c, b) | D = type of shared value | Argument used multiply |
| **Elimination** (destruct) | Triage(c, d, b) | None | Domain type determines branches |

K=weakening and S=contraction are known (Curry 1963, BCK/BCI logic). The triage=elimination identification and D-as-contraction-witness interpretation are specific to tree calculus.

### Hash-Consing and O(1) Type Equality

Trees are hash-consed: structurally equal trees share the same ID.
- `treeEqual(a, b)` is O(1) — replaces `convertible(a, b)` (WHNF + structural compare)
- `fastEq` runtime primitive: O(1) equality via `a.id === b.id`
- Memoized checking: cache key `(annotated_tree.id, type.id) → bool`

### Prior Art

Type checking on S/K/I combinators (simple types) is classical — Curry & Feys 1958, Hindley 1969. Dependent types on combinators: Altenkirch et al. (FSCD 2023), Zaldivar Aiello (2024). Neither produces a practical checking algorithm.

**Novel**: Dependent type checking as structural dispatch on tree calculus, the annotated tree format, triage as dependent elimination, D as contraction witness.

---

## Test Coverage

- `test/tree-native-typecheck.test.ts` — core checker (93 tests)
- `test/tree-native-exhibit.test.ts` — (tree, type, annotation) triples (27 tests)
- `test/tree-native-annotated.test.ts` — annotated format, extraction, memoization (35 tests)
- `test/tree-native-dependent.test.ts` — dependent codomain helpers, gap analysis (17 tests)
- `test/tree-native-pipeline.test.ts` — end-to-end CoC→native pipeline (101 tests)
- `test/tree-native-elaborate.test.ts` — native elaborator, TypedExpr, bracket abstraction (32 tests)
- `test/repl.test.ts` — REPL integration with prelude.disp loading (58 tests)
- `test/prelude.test.ts` — prelude loading, native env contents (11 tests)
