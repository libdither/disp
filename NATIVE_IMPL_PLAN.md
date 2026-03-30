# Native Type System — Implementation Plan

Engineering roadmap for the tree-native type system. Theory is in `TREE_NATIVE_TYPE_THEORY.md`.

## Current State (2026-03-30)

**Native pipeline is primary.** 664 tests passing. REPL is fully CoC-free. CoC retained only for legacy test backward compatibility and tree-encoded step functions.

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
| `src/coc.ts` | Legacy. Used by tree-encoded step functions and test backward compat only |

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

1. **Simplified builtin types** (fold, add, mul, treeEq, whnf, abstractOut, etc.): Primitive builtins get types from `convertCocType(cocType)` which loses dependent structure. natElim's first domain shows as `Type` instead of `Nat -> Type`, preventing the constant-motive coercion from triggering.

2. **Church-encoded value elaboration** (mkPair, fst, snd, inl, inr): The elaborator can now decompose `Pair A B` as a proper Pi type (Phase 1 fix), but elaborating through the Church-encoded value bodies requires the expected type to be fully decomposable at each lambda. Some of these may work once builtin types are fixed.

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

**Completed 2026-03-30.** Two changes in `src/compile.ts`:

- `spi` case: `bracketAbstract(name, codomain)` → `eApp(eApp(eTree(LEAF), domain), codomain)`. Pi types compile to `fork(domain, codomain)` — proper native type encoding.
- `stree` case: `eTree(LEAF)` → `eTree(stem(LEAF))`. Tree-as-value = TN_TREE.

**Impact:** `Pair Nat Bool` evaluates to a structured fork (native Pi type) instead of an opaque stem. The REPL displays full types for Church-encoded definitions. One compile test updated (Pi test checks fork structure instead of lambda behavior).

**Surprise:** No prelude verification count change (7/22) because the legacy `processDeclCoc` path uses `annotateTree` which doesn't benefit from compilation changes. The native path `nativeElabDecl` sees the improvement but the same 7 defs that passed before still pass.

### Phase 2: Constant-Motive Coercion ✓

**Completed 2026-03-30.** 12 lines added in `buildNativeWrapped` sapp case: when domain is `fork(X, K(Type))` = `X -> Type` and argument has type `Type`, automatically wrap in `K(arg)`.

**Impact:** Correctly handles `natElim R z s n` where `R : Type`. However, the coercion doesn't trigger for current prelude definitions because `convertCocType` simplifies natElim's domain to `Type` (already matching). Will activate when builtins get proper native types.

### Approach A: Ascription Restriction ✓

**Completed 2026-03-30.** Three changes:

1. **Checker** (`tree-native-checker.ts`): Ascription case now requires `defs.get(body.id)` to exist and match the expected type. Previously trusted any ascription blindly.

2. **Elaborator** (`tree-native-elaborate.ts`): `collapseToAnnotated` accepts optional `defs` parameter. Registers each `apply()` result in defs before returning ascription, making it verifiable by the checker.

3. **Registration ordering**: `nativeElabDecl` registers compiled tree in `nativeDefs` before calling `checkAnnotated` (both recursive and non-recursive cases).

**Discovery:** The S(K(p), K(q)) → K(apply(p,q)) optimization in `typedBracketAbstract` was dead code. The early K return at line 519 (`!typedHasFreeVar → K(texpr)`) catches all cases where neither sub-expression contains the variable, so the S(K,K) case is never reached. Removed as cleanup.

**Discovery:** The elaboration and compilation paths produce different trees — elaboration uses native type constants (TN_BOOL etc.) while compilation uses CoC markers (BOOL_TYPE etc.). Ascription bodies from `collapseToAnnotated` never match compiled trees by ID. The defs registration in `collapseToAnnotated` is what makes ascription verification work.

---

## Next Steps

### Proper Builtin Native Types

**Priority: HIGH. This is the current blocker for more prelude verification.**

Primitive builtins (natElim, boolElim, etc.) get native types from `convertCocType(cocType)` which loses dependent structure. natElim's type should be:

```
Pi(Pi(Nat, K(Type)), ...)    -- (M : Nat -> Type) -> M 0 -> ... -> Nat -> M n
```

But `convertCocType` produces:

```
Pi(Type, ...)                -- simplified, first domain is just Type
```

This prevents the constant-motive coercion (Phase 2) from triggering and prevents proper elaboration of fold/add/mul.

**Fix options:**
1. Hand-write native types for each builtin in `registerBuiltins`
2. Improve `convertCocType` to preserve dependent structure
3. Elaborate builtin type strings directly via `buildNativeWrapped` (already attempted in `registerBuiltins` but some fail)

Option 1 is the most reliable. There are 13 primitive builtins and ~18 tree-native builtins. Most have simple types; only natElim, boolElim, and triage have dependent types that need care.

**Unblocks:** Phase 2 coercion for fold/add/mul, plus any definition that applies natElim/boolElim.

### Checker Soundness Fixes

**Priority: MEDIUM. Correctness improvements, not blocking progress.**

**Bool triage evaluates stem handler** (`tree-native-checker.ts:334`): The Bool case calls `apply(extract(annD), LEAF)` and passes the raw result to `checkAnnotated`. This mixes evaluation and verification. Fix: check `annD` structurally as `Pi(Tree, stemCodomain(B))`, like the Tree triage case already does.

**K-Nat two-point check** (`tree-native-checker.ts:250-253`): For `K(v) : Pi(Nat, B)` where B is dependent, checks only at 0 and 1. Unsound for B that varies at n≥2. Fix: reject dependent K over Nat (in practice, if B varies, K(v) can't inhabit it).

### Phase 3: Full Prelude Verification

**Goal:** All 22 prelude.disp definitions produce correct annotated trees passing `checkAnnotated`.

**Prerequisites:** Proper builtin native types. The 15 failing defs break down as:
- **3 need constant-motive** (fold, add, mul) — blocked on builtin types
- **5 need Church-type elaboration** (mkPair, fst, snd, inl, inr) — may work once builtin types fixed and expected types decompose correctly
- **7 need complex recursive elaboration** (treeEq, whnf, abstractOut, etc.) — deep tWait/natElim nesting, may need elaborator improvements

**Target:** 22/22 verified.

### Phase 4: Unify Compilation Paths

**Goal:** Remove `compile.ts` as a separate trusted backend. The elaborator becomes the sole compilation path.

**Current obstacle:** The elaboration and compilation paths use different tree spaces (native types vs CoC markers). Unification requires all builtins to use native types (not CoC markers from `defs` map).

**Approach:**
1. After builtin native types are fixed, verify `extract(collapseToAnnotated(texpr)) === compileAndEval(value, defs)` for all prelude defs
2. Replace `compileAndEval` calls in `nativeElabDecl` with `extract(collapseToAnnotated(...))`
3. Keep `compile.ts` for REPL `:tree` command

### Phase 5: Encode Checker as Tree Constant

**Goal:** `checkAnnotated` as a tree calculus combinator — a tree that verifies other trees.

The checker is now nearly adversarially sound (restricted ascriptions, structural S/K/triage rules). After Bool triage and K-Nat fixes, it becomes fully structural with no evaluation, making it suitable for tree-calculus encoding.

Key operations to encode:
- 3-way triage dispatch on annotated tree structure
- `treeEqual` via `fastEq` (O(1))
- `apply(B, x)` for dependent codomain evaluation
- `isOfBaseType` — structural check on stem chains
- Known-def lookup via tree-encoded map
- Recursion via `tFix` + `tWait`

### Phase 6: Remove Legacy CoC Code

**Prerequisite:** All tests use the native pipeline.

Remove from `src/prelude.ts`: `loadCocPrelude`, `processDeclCoc`, `buildNameMap`, `registerBuiltinsCoc`, `loadDeclFileCoc`, `cocPreludePath`, and all imports from `coc.js`.

Remove from `src/tree-native-elaborate.ts`: `annotateTree`, `inferIntermediateType`, `inferGroundType`, `convertCocType`, `convertPiBody`, `nativeCheckDecl`, and all imports from `coc.js`.

Delete `coc-prelude.disp`.

Keep `src/coc.ts` and `test/coc.test.ts` for CoC encoding tests and tree-encoded step functions.

### Phase 7: Scoring Functions (GOALS.md)

With a tree-encoded checker:

```
score(program) = treeCheck(annotate(program), type) * performance_weight(program) * size_weight(program)
```

Where `treeCheck` returns 0 or 1, and performance/size weights are tree-encoded metric functions.

---

## Known Issues

### Soundness

- **Bool triage evaluation** (`checker.ts:334`): Evaluates stem handler instead of checking structurally. Can misinterpret evaluated results as annotation format.
- **K-Nat two-point check** (`checker.ts:250-253`): Unsound for dependent B over Nat that varies at n≥2.
- **Opaque type escape hatch** (`elaborate.ts:712`): Silently accepts type mismatches when domain is an unrecognized stem. Needed for dependent type applications with bound variables.

### Incompleteness

- **Abstract domain triage** (`checker.ts:291-298`): Only handles identity triage on abstract types. Non-identity polymorphic triages fail.
- **Depth limit 80** (`checker.ts:198`): Hard recursion limit. Deep bracket-abstracted programs may exceed it.
- **No type inference** (`elaborate.ts:758`): All lambdas require expected types. Prevents `let f := {x} -> x`.
- **Marker erasure**: Eager evaluation consumes markers during dependent type detection. Three fallback strategies in spi case (check collapsed tree, check TypedExpr, bracket-abstract). Fundamental tension with eager evaluation model.

### Technical Debt

- Legacy CoC imports in `tree-native-elaborate.ts` (lines 10-15): `termTag`, `unPi`, `whnfTree`, `evalToNative`, `cocCheckDecl` — only used by legacy functions (`convertCocType`, `annotateTree`, `nativeCheckDecl`). Dead code from REPL's perspective.
- `coc-prelude.disp`: Only loaded by `loadCocPrelude` (test-only).
- Dual builtin registration: PRIMITIVE_BUILTINS registered in both CoC env and native env.

---

## Legacy Code Reference

Code retained for test backward compatibility (remove in Phase 6):

| Function | File | Used By |
|----------|------|---------|
| `loadCocPrelude` | `src/prelude.ts` | `test/coc.test.ts`, `test/bench.test.ts`, `test/errors.test.ts` |
| `processDeclCoc` | `src/prelude.ts` | `test/bench.test.ts`, `test/tree-native-elaborate.test.ts` |
| `buildNameMap` | `src/prelude.ts` | `test/coc.test.ts` |
| `annotateTree` | `src/tree-native-elaborate.ts` | `test/tree-native-pipeline.test.ts`, `test/bench.test.ts` |
| `convertCocType` | `src/tree-native-elaborate.ts` | `test/tree-native-pipeline.test.ts`, `test/tree-native-elaborate.test.ts`, `test/bench.test.ts` |
| `nativeCheckDecl` | `src/tree-native-elaborate.ts` | `test/tree-native-pipeline.test.ts` |

## Test Coverage

| Test file | Tests | Coverage |
|-----------|-------|---------|
| `test/tree-native-typecheck.test.ts` | 93 | Core checker |
| `test/tree-native-exhibit.test.ts` | 27 | (tree, type, annotation) triples |
| `test/tree-native-annotated.test.ts` | 35 | Annotated format, extraction, memoization |
| `test/tree-native-dependent.test.ts` | 17 | Dependent codomain helpers |
| `test/tree-native-pipeline.test.ts` | 101 | CoC→native pipeline (legacy) |
| `test/tree-native-elaborate.test.ts` | 32 | Native elaborator, TypedExpr |
| `test/approach-a.test.ts` | 45 | Ascription restriction, Phase 1+2, pipeline semantics |
| `test/repl.test.ts` | 58 | REPL integration with prelude.disp |
| `test/prelude.test.ts` | 11 | Prelude loading |
| `test/coc.test.ts` | 147 | CoC encoding (legacy, keep) |
| `test/compile.test.ts` | 72 | Compilation, bracket abstraction |
| `test/bench.test.ts` | 8 | Benchmarks |
| `test/errors.test.ts` | 17 | Error handling |
| Total | 664 | All passing |
