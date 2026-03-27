# Native Type System — Implementation Plan

Engineering roadmap for the tree-native type system. Theory is in `TREE_NATIVE_TYPE_THEORY.md`.

## Current State (2026-03-27)

**Native pipeline is primary.** 619 tests passing. CoC retained only for legacy test backward compatibility.

### Architecture

```
Parse (src/parse.ts) → Elaborate (src/tree-native-elaborate.ts) → Compile (src/compile.ts) → Check (src/tree-native-checker.ts)
```

| File | Role |
|------|------|
| `src/tree-native-checker.ts` | `checkAnnotated(defs, ann, type) → bool`, type constants, codomain helpers |
| `src/tree-native-elaborate.ts` | `buildNativeWrapped`, `typedBracketAbstract`, `collapseToAnnotated`, `nativeElabDecl` |
| `src/native-utils.ts` | `freshMarker`, `registerNativeBuiltinId`, `isNativeBuiltin`, cache callbacks |
| `src/prelude.ts` | `loadPrelude` (native-only), `processDecl` (native with graceful fallback) |
| `src/repl.ts` | Fully native REPL, no CoC fallback |
| `src/compile.ts` | Compiler: `astToExpr` + bracket abstraction. Still used as trusted compilation backend |
| `src/coc.ts` | Legacy. Used by tree-encoded step functions and test backward compat |

### What Works

- All primitive types and builtins type-check natively
- Simple definitions (not, and, or, add, mul, fold, id) elaborate fully with correct annotated trees
- Recursive definitions elaborate, abstract self out, verify body at T→T, store FIX as ascription
- Multi-param lambdas track types correctly through TypedExpr (tI/tK/tS)
- Polymorphic identity, dependent Pi types, higher-order functions
- Prelude loads all 22 definitions (graceful fallback for 9 that can't be fully elaborated)

### What Doesn't Work

Definitions involving **Church-encoded types** (Pair, Either, records, coproducts) compile and run correctly at runtime but have degraded types in the REPL. The elaborator falls back to `Tree` for their types.

Root cause: the compiler erases Pi type domains (see Phase 1 below).

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
- `collapseToAnnotated(texpr) → Tree` — annotated tree with D + ascriptions
- `typedBracketAbstract(name, texpr, type) → { result: TypedExpr, codomain: Tree }` — preserves structure for outer abstractions

### processDecl (Graceful Degradation)

```
1. Try nativeElabDecl (full type check + annotated tree)
2. If that fails:
   a. Compile via compile.ts (always succeeds)
   b. Try elaborating just the type annotation
   c. Register with ascription-wrapped compiled tree
   d. checkOk = false
```

---

## Phase 1: Preserve Pi Domains in Compilation

**Priority: HIGH. This is the single most impactful change.**

### Problem

`src/compile.ts` lines 59-68 erase Pi type domains:

```typescript
case "spi":
  // Pi types erase to lambdas (domain is dropped)
  // (x : A) -> B  compiles same as {x} -> B
  return bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
```

**Effect**: `Pair := {A B} -> (R : Type) -> (A -> B -> R) -> R` compiles to the equivalent of `{A B R _} -> R` since all Pi domains are dropped. So `Pair Nat Bool` evaluates to `stem(LEAF)` instead of a proper native Pi type `fork(Type, [R] fork(...))`.

The elaborator needs `Pair A B` to decompose as `fork(domain, codomain)` to type-check lambdas whose expected type involves Pair. Without domain info, it falls back to `Tree`.

### Fix

Change the `spi` case in `src/compile.ts` `astToExpr`:

```typescript
case "spi": {
  // Pi types as values: fork(domain, [x]codomain) = native Pi encoding
  const domain = astToExpr(ast.domain, defs)
  const codomain = ast.name === "_"
    ? bracketAbstract("_$pi", astToExpr(ast.codomain, defs))
    : bracketAbstract(ast.name, astToExpr(ast.codomain, defs))
  // apply(apply(LEAF, domain), codomain) = apply(stem(domain), codomain) = fork(domain, codomain)
  return eApp(eApp(eTree(LEAF), domain), codomain)
}
```

This makes `(A : Type) -> B` compile to `fork(A_tree, [x]B_tree)` which is exactly the native Pi type `fork(A, B)`.

### Also needed

- `stree` case: `Tree` currently compiles to `LEAF`. It should compile to `stem(LEAF)` = `TN_TREE`. Without this, definitions that reference `Tree` as a value embed the wrong constant.

  ```typescript
  case "stree":
    return eTree(stem(LEAF))  // TN_TREE, not LEAF
  ```

### Impact

- `Pair Nat Bool` evaluates to `fork(LEAF, [R] fork(fork(Nat, K(fork(Bool, K(R)))), K(R)))` — a valid native Pi type
- The elaborator can decompose this and type-check `mkPair`, `fst`, `snd` properly
- All Church-encoded types (records, coproducts) should start working
- **Compiled trees change** for every type-valued definition. Tests comparing exact tree shapes will need updating.
- Runtime behavior for value-level code is unaffected (Pi types only appear in type annotations and type-valued defs)

### Testing

Run `npx vitest run`. Expect test failures where compiled type representations changed. The native verification pass rate (currently 7/22 prelude defs passing `checkAnnotated`) should improve. Fix tests to match new representations.

---

## Phase 2: Constant-Motive Coercion

### Problem

Even after Pi domain preservation, dependent elimination has a type mismatch:

```
natElim : (M : Nat -> Type) -> M 0 -> ((n:Nat) -> M n -> M (succ n)) -> Nat -> M n
```

Called as `natElim R z s n` where `R : Type`. The first argument `R` has type `Type` but the domain expects `Nat -> Type`. The native elaborator rejects: `treeEqual(TN_TYPE, fork(TN_NAT, K(TN_TYPE))) → false`.

Similarly, `boolElim Bool false true b` passes `Bool : Type` but expects `Bool -> Type`.

### Fix

In `buildNativeWrapped`'s `sapp` case, when type-checking the argument against the domain: if the domain is `Pi(A, K(Type))` (= `A -> Type`) and the argument has type `Type`, automatically wrap the argument in `K(arg)`.

```typescript
// In sapp case, after computing argType:
if (!treeEqual(argType, A) && treeEqual(argType, TN_TYPE) && isFork(A)) {
  // Implicit constant motive: wrap Type-valued arg in K for A -> Type domain
  const argTree = collapseForType(argTexpr, env)
  const kWrapped: TypedExpr = { tag: "tK", value: argTexpr, type: A }
  // Continue with kWrapped as the argument
  ...
}
```

This matches the common pattern where users pass a type directly as a constant motive.

### Testing

`natElim R z s n`, `boolElim Bool false true b`, and `fold 5 Nat 0 succ` should all type-check without errors. Prelude definitions using these patterns should elaborate fully.

---

## Phase 3: Full Prelude Verification

### Goal

All 22 prelude.disp definitions produce correct annotated trees that pass `checkAnnotated`. This validates the entire native pipeline end-to-end.

### Approach

1. Run `nativeElabDecl` for each prelude def
2. Verify `checkAnnotated(nativeDefs, annotated, nativeType) === true` for all 22
3. Oracle check: `treeEqual(extract(annotated), compiled)` for non-recursive defs
4. For recursive defs: `checkAnnotated(nativeDefs, annSelfFn, tnArrow(T, T))` passes

### Current Status

7/22 pass native verification. After Phases 1-2, expect significant improvement. Target: 22/22.

---

## Phase 4: Unify Compilation Paths

### Goal

Remove `compile.ts` as a separate trusted backend. The elaborator becomes the sole compilation path.

### Prerequisite

Phase 3 must verify `extract(collapseToAnnotated(texpr)) === compileAndEval(value, defs)` for all prelude defs. This confirms the two bracket abstraction implementations (TypedExpr-level and Expr-level) produce identical programs.

### Approach

1. In `processDecl`, replace `compileAndEval(value, defs)` with `extract(collapseToAnnotated(texpr))`
2. In `nativeElabDecl`, replace `compileAndEval`/`compileRecAndEval` calls with `extract(collapseToAnnotated(...))`
3. Keep `compile.ts` for the REPL's `:tree` command and standalone compilation, but remove it from the type-checking pipeline

---

## Phase 5: Encode Checker as Tree Constant

### Goal

`checkAnnotated` is a pure function `(annotatedTree, type) → bool`. Compile it to a tree calculus combinator, like `inferStep` in `src/tree-native.ts`. This makes the checker self-referential: a tree that verifies other trees.

### Approach

The existing tree-encoded `inferStep` handles CoC-encoded terms. The new checker needs to handle native-encoded types and the annotated tree format. Key operations to encode:

- 3-way triage dispatch on annotated tree structure (K/S/Triage)
- `treeEqual` — already O(1) via `fastEq`
- `apply(B, x)` for dependent codomain evaluation — use existing tree-calculus `apply`
- `isOfBaseType` — structural check on stem chains
- Recursion through annotated tree structure — use `tFix` + `tWait`

### Testing

The tree-encoded checker should produce identical results to the TypeScript `checkAnnotated` for all test cases in `test/tree-native-typecheck.test.ts`.

---

## Phase 6: Remove Legacy CoC Code

### Prerequisite

All tests use the native pipeline. No test imports `loadCocPrelude`, `processDeclCoc`, `buildNameMap`, `annotateTree`, `convertCocType`, or `nativeCheckDecl`.

### Remove

From `src/prelude.ts`: `loadCocPrelude`, `processDeclCoc`, `buildNameMap`, `registerBuiltinsCoc`, `loadDeclFileCoc`, `cocPreludePath`, and all imports from `coc.js`.

From `src/tree-native-elaborate.ts`: `annotateTree`, `inferIntermediateType`, `inferGroundType`, `convertCocType`, `convertPiBody`, `nativeCheckDecl`, and all imports from `coc.js`.

File `coc-prelude.disp`: Delete.

Keep `src/coc.ts` and `test/coc.test.ts` for the CoC encoding tests and tree-encoded step functions (these test the tree-as-type-checker infrastructure).

---

## Phase 7: Scoring Functions (GOALS.md)

With a tree-encoded checker, define scoring functions per GOALS.md:

```
score(program) = typecheck(program) * performance_weight(program) * size_weight(program)
```

Where `typecheck` returns 0 or 1, and performance/size weights are tree-encoded metric functions. This is the input to the external optimizer.

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
| `test/repl.test.ts` | 58 | REPL integration with prelude.disp |
| `test/prelude.test.ts` | 11 | Prelude loading |
| `test/coc.test.ts` | 147 | CoC encoding (legacy, keep) |
| `test/bench.test.ts` | 8 | Benchmarks |
| `test/errors.test.ts` | 17 | Error handling |
| Total | 619 | All passing |
