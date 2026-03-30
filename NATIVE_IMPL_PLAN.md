# Native Type System — Implementation Plan

Engineering roadmap for the tree-native type system. Theory is in `TREE_NATIVE_TYPE_THEORY.md`.

## Current State (2026-03-30)

**Native pipeline is primary. Legacy CoC code removed from production path.** 481 tests, 478 passing. 3 `[POST]` aspirational failures (ascription-free annotations). REPL and prelude fully native. `coc.ts` retained only as a library for encoding primitives used by tree-encoded step functions and benchmarks.

### Architecture

```
Parse (src/parse.ts) → Elaborate (src/tree-native-elaborate.ts) → Compile (src/compile.ts) → Check (src/tree-native-checker.ts)
```

| File | Role |
|------|------|
| `src/tree-native-checker.ts` | `checkAnnotated(defs, ann, type) → bool`, type constants, codomain helpers, ascription verification |
| `src/tree-native-elaborate.ts` | `buildNativeWrapped`, `typedBracketAbstract`, `collapseToAnnotated`, `nativeElabDecl` |
| `src/native-utils.ts` | `freshMarker`, `registerNativeBuiltinId`, `isNativeBuiltin`, cache callbacks |
| `src/prelude.ts` | `loadPrelude` (native-only), `processDecl` (native with graceful fallback) |
| `src/repl.ts` | Fully native REPL, no CoC fallback or imports |
| `src/compile.ts` | Compiler: `astToExpr` + bracket abstraction. Pi types compile to `fork(domain, codomain)`. |
| `src/coc.ts` | Encoding library only. Used by tree-encoded step functions and bench tests. No production imports. |

### What Works

- All primitive types and builtins type-check natively
- 7/22 prelude definitions verify via `checkAnnotated`: not, and, or, id, Pair, Either, infer
- Church-encoded types display correctly: `Pair Nat Bool` → `(a : Type) -> (Nat -> Bool -> a) -> a`
- Pi types compile to `fork(domain, codomain)` — proper native type values
- `Tree` keyword compiles to `stem(LEAF)` = TN_TREE
- Ascriptions restricted to known definitions with matching types (adversarially sound for function types)
- Recursive definitions elaborate, abstract self out, verify body at T→T, store FIX as ascription
- Constant-motive coercion: `Type` auto-wrapped in `K` when `X -> Type` domain expected
- All runtime computation works correctly regardless of verification status

### What Doesn't Work

**15/22 prelude definitions fall back to unchecked compilation.** They compile and run correctly but have `checkOk = false`. Root causes:

1. **Simplified builtin types** (fold, add, mul, treeEq, whnf, abstractOut, etc.): Builtins whose dependent types can't be elaborated natively fall back to `TN_TREE` type. natElim/boolElim/triage need hand-written native types with proper dependent structure.

2. **Church-encoded value elaboration** (mkPair, fst, snd, inl, inr): The elaborator can decompose `Pair A B` as a proper Pi type, but elaborating through Church-encoded value bodies requires the expected type to be fully decomposable at each lambda. Some of these may work once builtin types are fixed.

3. **Complex recursive step functions** (treeEq, whnf, abstractOut, etc.): Deep nesting of tWait/natElim patterns exceeds current elaboration capabilities.

### TypedExpr

The elaborator's intermediate representation:

```typescript
type TypedExpr =
  | { tag: "tlit", tree: Tree, annTree: Tree, type: Tree }
  | { tag: "tfvar", name: string, type: Tree }
  | { tag: "tapp", func: TypedExpr, arg: TypedExpr, type: Tree }
  | { tag: "tI", type: Tree }
  | { tag: "tK", value: TypedExpr, type: Tree }
  | { tag: "tS", d: Tree, c: TypedExpr, b: TypedExpr, type: Tree }
```

- `collapseTypedExpr(texpr) → Tree` — raw program
- `collapseToAnnotated(texpr, defs?) → Tree` — annotated tree with D + ascriptions; registers apply results in defs
- `typedBracketAbstract(name, texpr, type) → { result: TypedExpr, codomain: Tree }` — preserves structure for outer abstractions

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

### Proper Builtin Native Types

**Priority: HIGH. This is the current blocker for more prelude verification.**

Builtins whose dependent types can't be elaborated natively fall back to `TN_TREE`. The key builtins needing hand-written native types:

- **natElim**: `Pi(Pi(Nat, K(Type)), ...)` — (M : Nat -> Type) -> M 0 -> ((n:Nat) -> M n -> M (succ n)) -> (n:Nat) -> M n
- **boolElim**: `Pi(Pi(Bool, K(Type)), ...)` — (M : Bool -> Type) -> M true -> M false -> (b:Bool) -> M b
- **triage**: `Pi(Pi(Tree, K(Type)), ...)` — similar dependent elimination

Hand-write these in `registerBuiltins()`. Most other builtins have simple arrow types that already elaborate correctly.

**Unblocks:** Phase 2 coercion for fold/add/mul, plus any definition that applies natElim/boolElim.

### Checker Soundness Fixes

**Priority: MEDIUM. Correctness improvements, not blocking progress.**

- **Bool triage evaluates stem handler** (`tree-native-checker.ts:334`): Mixes evaluation and verification. Fix: check `annD` structurally.
- **K-Nat two-point check** (`tree-native-checker.ts:250-253`): Unsound for dependent B over Nat that varies at n≥2. Fix: reject dependent K over Nat.

### Full Prelude Verification

**Goal:** All 22 prelude.disp definitions produce correct annotated trees passing `checkAnnotated`.

**Prerequisites:** Proper builtin native types. The 15 failing defs break down as:
- **3 need constant-motive** (fold, add, mul) — blocked on builtin types
- **5 need Church-type elaboration** (mkPair, fst, snd, inl, inr) — may work once builtin types fixed
- **7 need complex recursive elaboration** (treeEq, whnf, abstractOut, etc.) — deep tWait/natElim nesting

### Unify Compilation Paths

**Goal:** Remove `compile.ts` as a separate trusted backend. The elaborator becomes the sole compilation path.

**Approach:**
1. After builtin native types are fixed, verify `extract(collapseToAnnotated(texpr)) === compileAndEval(value, defs)` for all prelude defs
2. Replace `compileAndEval` calls in `nativeElabDecl` with `extract(collapseToAnnotated(...))`
3. Keep `compile.ts` for REPL `:tree` command

### Encode Checker as Tree Constant

**Goal:** `checkAnnotated` as a tree calculus combinator — a tree that verifies other trees.

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

- **Bool triage evaluation** (`checker.ts:334`): Evaluates stem handler instead of checking structurally.
- **K-Nat two-point check** (`checker.ts:250-253`): Unsound for dependent B over Nat that varies at n≥2.
- **Opaque type escape hatch** (`elaborate.ts`): Silently accepts type mismatches when domain is an unrecognized stem.

### Incompleteness

- **Abstract domain triage**: Only handles identity triage on abstract types.
- **Depth limit 80**: Hard recursion limit in checker.
- **No type inference**: All lambdas require expected types.
- **Marker erasure**: Eager evaluation consumes markers during dependent type detection.

### 3 Aspirational Test Failures

`test/approach-a.test.ts` Section 4 "Annotation structure": `[POST]` tests expecting no ascription nodes inside combinator annotations. `collapseToAnnotated` currently wraps `tapp` results in ascriptions. Will pass when elaboration produces fully structural annotations.

---

## Test Coverage

| Test file | Tests | Coverage |
|-----------|-------|---------|
| `test/tree-native-typecheck.test.ts` | 93 | Core checker |
| `test/tree-native-exhibit.test.ts` | 27 | (tree, type, annotation) triples |
| `test/tree-native-annotated.test.ts` | 35 | Annotated format, extraction, memoization |
| `test/tree-native-dependent.test.ts` | 17 | Dependent codomain helpers |
| `test/tree-native-elaborate.test.ts` | 31 | Native elaborator, TypedExpr |
| `test/approach-a.test.ts` | 45 | Ascription restriction, Phase 1+2, pipeline semantics |
| `test/tree-native.test.ts` | 19 | Tree-native builtins (FST, SND, TERM_CASE, etc.) |
| `test/repl.test.ts` | 58 | REPL integration with prelude.disp |
| `test/prelude.test.ts` | 11 | Prelude loading |
| `test/compile.test.ts` | 30 | Compilation, bracket abstraction |
| `test/parse.test.ts` | 68 | Parser |
| `test/parse.fuzz.test.ts` | 5 | Parser fuzz |
| `test/tree.test.ts` | 21 | Tree data structure |
| `test/errors.test.ts` | 14 | Error handling |
| `test/bench.test.ts` | 7 | Benchmarks |
| **Total** | **481** | 478 passing, 3 `[POST]` failures |
