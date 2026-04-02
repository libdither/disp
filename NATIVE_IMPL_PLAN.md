# Native Type System — Implementation Plan

Engineering roadmap for the tree-native type system. Theory is in `TREE_NATIVE_TYPE_THEORY.md`.

## Current State (2026-04-01)

**Native pipeline is primary.** 483 tests, 481 passing. 2 `[POST]` aspirational failures (Church-encoded types: mkPair, fold). Previous 3 `[POST]` ascription-free annotation tests now pass via TypedExpr pipeline. Checker soundness fixes applied. `coc.ts` retained only as encoding library.

### Architecture

Two-phase pipeline: **elaborate** (SExpr → TypedExpr) then **compile** (TypedExpr → annotated Tree).

```
Parse (src/parse.ts) → Elaborate (TypedExpr) → Compile (bracket abstract + collapse) → Check (src/tree-native-checker.ts)
```

| File | Role |
|------|------|
| `src/tree-native-checker.ts` | `checkAnnotated(defs, ann, type) → bool`, type constants, codomain helpers |
| `src/tree-native-elaborate.ts` | `TypedExpr` type, `elaborate` (Phase 1), `compileTE` (Phase 2), `nativeElabDecl` |
| `src/native-utils.ts` | `freshMarker`, `registerNativeBuiltinId`, `isNativeBuiltin`, cache callbacks |
| `src/prelude.ts` | `loadPrelude` (native-only), `processDecl` (native with graceful fallback) |
| `src/repl.ts` | Fully native REPL, no CoC fallback or imports |
| `src/compile.ts` | Legacy compiler: `astToExpr` + bracket abstraction. Still used for: recursive defs (FIX), REPL expression eval, type-level computation. |
| `src/coc.ts` | Encoding library only. Used by tree-encoded step functions and bench tests. |

### What Works

- All primitive types and builtins type-check natively
- **22/22 prelude definitions verify** via `checkAnnotated`: not, and, or, fold, add, mul, treeEq, whnf, abstractOut, abstractOutFast, convertibleRec, convertible, inferRec, infer, id, Pair, mkPair, fst, snd, Either, inl, inr
- Church-encoded types display correctly: `Pair Nat Bool` → `(a : Type) -> (Nat -> Bool -> a) -> a`
- Pi types compile to `fork(domain, codomain)` — proper native type values
- `Tree` keyword compiles to `stem(LEAF)` = TN_TREE
- Ascriptions restricted to known definitions with matching types (adversarially sound for function types)
- Recursive definitions elaborate, abstract self out, verify body at T→T, store FIX as ascription
- Constant-motive coercion: `Type` auto-wrapped in `K` when `X -> Type` domain expected
- Hand-written dependent types for natElim/boolElim/triage (fixes data-tree sharing overwrite)
- All runtime computation works correctly regardless of verification status

### What Doesn't Work

**All 22 prelude definitions now verify.** 2 `[POST]` failures for Church-encoded types (mkPair, fold) — elaboration fails on deeply nested dependent type computation. Known limitations are theoretical (see Known Issues below), not practical blockers for current prelude.

### TypedExpr Pipeline

The elaborator uses a two-phase architecture:

```typescript
type TypedExpr =
  | { tag: "tree",  value: Tree, type: Tree }     // resolved constant
  | { tag: "var",   name: string, type: Tree }     // bound variable
  | { tag: "app",   func: TypedExpr, arg: TypedExpr, type: Tree }  // application
  | { tag: "lam",   param: string, domain: Tree, body: TypedExpr, type: Tree }  // lambda
  | { tag: "combI", type: Tree }                   // identity combinator
  | { tag: "combK", inner: TypedExpr, type: Tree } // constant combinator
  | { tag: "combS", c: TypedExpr, b: TypedExpr, D: Tree, type: Tree }  // sharing combinator + D annotation
```

**Phase 1: `elaborate(sexpr, env, expectedType?) → TypedExpr`**
- Resolves names, computes types (evaluating type-level trees for dependent codomains)
- Lambdas stay as `lam` nodes — no compilation yet
- Every node carries its type

**Phase 2: `compileTE(typedExpr, nativeDefs?) → { annotated, compiled }`**
- Bracket-abstracts all lambdas inside-out, producing combI/combK/combS nodes
- combK/combS cases delegate to app-chain form, getting S(Kp)(Kq) and eta optimizations for free
- `collapseAnnotated` → annotated tree with D at S nodes from types
- `collapseRaw` → raw compiled tree (eagerly evaluated for execution)
- No intermediate Expr or hash-cons pollution

**`buildDirect`** preserved as backward-compatible wrapper returning `{ tree, type }` for REPL type queries and builtin type elaboration.

---

## Completed Work

### Phase 1: Pi Domain Preservation ✓

**Completed 2026-03-30.** `spi` case: Pi types compile to `fork(domain, codomain)` — proper native type encoding. `stree` case: Tree-as-value = TN_TREE.

### Phase 2: Constant-Motive Coercion ✓

**Completed 2026-03-30.** When domain is `X -> Type` and argument has type `Type`, auto-wrap in `K(arg)`.

### Approach A: Ascription Restriction ✓

**Completed 2026-03-30.** Ascriptions require `defs.get(body.id)` to match expected type. `collapseToAnnotated` registers apply results. Registration ordering fixed.

### Legacy CoC Cleanup ✓

**Completed 2026-03-30.** Removed from production path:

- `src/prelude.ts`: Removed `loadCocPrelude`, `processDeclCoc`, `buildNameMap`, `registerBuiltinsCoc`, `loadDeclFileCoc`, all CoC imports
- `src/tree-native-elaborate.ts`: Removed `convertCocType`, `annotateTree`, `inferIntermediateType`, `inferGroundType`, `nativeCheckDecl`, `convertPiBody`, all CoC imports
- Deleted `coc-prelude.disp`
- Deleted `test/coc.test.ts` (147 tests), `test/tree-native-pipeline.test.ts` (31 tests)
- Cleaned CoC dependencies from `test/errors.test.ts`, `test/bench.test.ts`, `test/tree-native-elaborate.test.ts`, `test/approach-a.test.ts`

Remaining CoC usage: `src/coc.ts` as encoding library for tree-native step functions and bench tests. `test/tree-native.test.ts` imports encoding constructors for reference comparisons.

---

## Next Steps

### Proper Builtin Native Types ✓

**Completed 2026-03-30.** Hand-written dependent types for natElim, boolElim, triage using bracket abstraction + collapseAndEval. All 6 eliminator builtins (dep + non-dep) share the same type, fixing the nativeDefs overwrite bug.

### Checker Soundness Fixes ✓

**Completed 2026-03-31.** Two confirmed soundness holes closed, abstract marker system removed:

- **Bool triage**: Now checks stem handler structurally (was: evaluated and misinterpreted result)
- **K dependent codomain**: Rejects dependent K over Nat/Type/Tree (was: unsound sample-point checks). Only Bool exhaustive check retained (2 inhabitants).
- **Abstract marker system removed**: `freshAbstract`, `isAbstract`, `resetAbstractCounter`, `inputAssumption` all deleted. ~50 lines removed. Checker has no external mutable state besides `defs`.

### TypedExpr Pipeline ✓

**Completed 2026-04-01.** Two-phase elaboration: `elaborate` (SExpr → TypedExpr) + `compileTE` (TypedExpr → annotated Tree). Replaced the old triple-compilation approach (buildDirect expr + buildDirect tree + compileAndEval) with a single TypedExpr that carries types through bracket abstraction. D annotations at S nodes come from types naturally. Ascription-free annotations for non-recursive definitions. 3 `[POST]` tests for ascription-free annotations now pass.

Key design decisions:
- combK/combS cases delegate to app-chain form for bracket abstraction, getting S(Kp)(Kq) and eta optimizations for free (avoids incorrect manual formula derivation)
- Eta-reduced subexpressions that are unevaluated apps get ascriptions with runtime registration in nativeDefs
- `collapseRaw` eagerly evaluates for execution; `collapseAnnotated` preserves structure for checking

### Full Prelude Verification ✓

**Completed 2026-03-30.** All 22 prelude.disp definitions produce correct annotated trees passing `checkAnnotated`.

### Unify Remaining Compilation Paths

**Goal:** Remove `compile.ts` dependency from the elaboration path entirely.

**Remaining uses of old compile.ts in elaborator:**
1. `compileRecAndEval` — recursive definitions still use FIX from compile.ts
2. `collapseAndEval` + `bracketAbstract` — type-level computation (Pi codomain abstraction)
3. `typedExprToExpr` — fallback conversion for type-level bracket abstraction

**Remaining uses in REPL/prelude:**
1. `compileAndEval` — REPL expression evaluation
2. `compileRecAndEval` — prelude fallback for failed elaboration

### Encode Checker as Tree Constant

**Goal:** `checkAnnotated` as a tree calculus combinator — a tree that verifies other trees. Checker now has no external mutable state besides `defs` — ready for tree-encoding.

### Remove Remaining CoC Code

**Prerequisite:** All tests and tree-encoded step functions work without `coc.ts`.

- Replace `encType`/`encPi` usage in bench tests with inline tree constructors
- Replace encoding imports in `tree-native.test.ts` with direct tree construction
- Consider whether tree-encoded step functions (WHNF_STEP etc.) still need `coc.ts` at runtime or can be decoupled

### Scoring Functions (GOALS.md)

With a tree-encoded checker:
```
score(program) = treeCheck(annotate(program), type) * performance_weight(program) * size_weight(program)
```

---

## Known Issues

### Soundness

- ~~**Bool triage evaluation**: Fixed 2026-03-31. Checks stem handler structurally.~~
- ~~**K-Nat two-point check**: Fixed 2026-03-31. Rejects dependent K over Nat/Type/Tree.~~
- **Opaque type escape hatch** (`elaborate.ts`): Silently accepts type mismatches when domain is an unrecognized stem.

### Incompleteness

- **No abstract/Pi domain triage**: Removed (was unsound). Triage only handles Tree, Nat, Bool domains.
- **Dependent K over non-Bool**: Rejected (was unsound). Only Bool exhaustive check retained.
- **Depth limit 80**: Hard recursion limit in checker.
- **No type inference**: All lambdas require expected types.
- **safeApply fallback**: Budget exhaustion during elaboration-time type computation falls back to structural `fork(f, x)`, yielding approximate types for deeply recursive definitions.

### 2 Aspirational Test Failures

`test/approach-a.test.ts`: `[POST]` mkPair and fold elaboration — fails because Church-encoded types produce opaque type trees during deeply nested dependent type computation. Previous 3 `[POST]` ascription-free annotation tests now pass via TypedExpr pipeline.

---

## Test Coverage

| Test file | Tests | Coverage |
|-----------|-------|---------|
| `test/tree-native-typecheck.test.ts` | 93 | Core checker |
| `test/tree-native-exhibit.test.ts` | 27 | (tree, type, annotation) triples |
| `test/tree-native-annotated.test.ts` | 35 | Annotated format, extraction, memoization |
| `test/tree-native-dependent.test.ts` | 17 | Dependent codomain helpers |
| `test/tree-native-elaborate.test.ts` | ~28 | Native elaborator, buildDirect |
| `test/approach-a.test.ts` | ~47 | Ascription restriction, Phase 1+2, pipeline semantics, soundness regression |
| `test/tree-native.test.ts` | 19 | Tree-native builtins (FST, SND, TERM_CASE, etc.) |
| `test/repl.test.ts` | 58 | REPL integration with prelude.disp |
| `test/prelude.test.ts` | 11 | Prelude loading |
| `test/compile.test.ts` | 30 | Compilation, bracket abstraction |
| `test/parse.test.ts` | 68 | Parser |
| `test/parse.fuzz.test.ts` | 5 | Parser fuzz |
| `test/tree.test.ts` | 21 | Tree data structure |
| `test/errors.test.ts` | 14 | Error handling |
| `test/bench.test.ts` | 7 | Benchmarks |
| **Total** | **483** | 481 passing, 2 `[POST]` failures |
