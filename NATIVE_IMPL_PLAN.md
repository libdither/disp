# Unified Native Type System Plan

## Context

The Disp REPL currently uses a hybrid CoC + tree-native pipeline. The native path works but has three structural problems:
1. **Heuristic annotation** — `annotateTree` guesses D at S nodes, failing 9/22 prelude defs
2. **CoC entanglement** — `processDecl` always calls `cocCheckDecl`, `handleExpr` falls back to `buildWrapped`
3. **Environment opacity** — `NativeEnv` stores raw trees; `checkAnnotated` requires `knownDefs` for external lookup

This plan merges the goals of the old NATIVE_IMPL_PLAN.md and COC_MIGRATION.md into a single phased migration that:
- Produces exact D annotations during elaboration (no heuristics)
- Removes all CoC imports from the hot path (repl.ts, prelude.ts)
- Makes annotated trees self-contained via ascription nodes + stored annotated forms
- Keeps `compile.ts` as the trusted compilation backend (risk mitigation)
- Keeps `coc.ts` for its own tests and tree-encoded step functions

The end goal is a checker that works as a pure function `(annotatedTree, type) → bool` with no external state — suitable for eventual encoding as a tree program per GOALS.md.

---

## Phase 0: Extract Shared Utilities

Create `src/native-utils.ts` with functions that both systems need but that don't belong in `coc.ts`.

**Move from `src/coc.ts`:**
- `freshMarker()`, `resetMarkerCounter()`, `markerCounter` state
- `registerNativeBuiltinId()`, `clearNativeBuiltins()`, `isNativeBuiltin()`, `getNativeForm()`
- `nativeBuiltinIds` Set, `nativeCompiledForms` Map

**`src/coc.ts`**: Re-export everything from `native-utils.ts` so existing imports work unchanged. Internal callers (`whnfTree`, `evalToNative`, `cocCheckRecDecl`) import from `native-utils.ts`.

**Tests**: Zero breakage — all existing import paths work through re-exports.

---

## Phase 1: Add Ascription Node

Add a sixth annotated tree form for wrapping opaque values whose types are known but whose structure can't be checked.

**Format**: `fork(stem(T), stem(body))` — "body has type T, trust it"

This slot is unused: the S form `fork(stem(D), fork(c, b))` requires a fork on the right. A stem on the right is a new case.

**`src/tree-native-checker.ts` changes:**

```typescript
// New constructor
export function annAscribe(type: Tree, body: Tree): Tree {
  return fork(stem(type), stem(body))
}

// extract(): in the isStem(ann.left) branch, before S:
if (isStem(ann.right)) {
  return ann.right.child  // ascription: return raw body
}

// checkAnnotated(): in the isStem(ann.left) branch, before S:
if (isStem(ann.right)) {
  const ascribedType = ann.left.child
  return treeEqual(ascribedType, type)  // trust if types match
}
```

**Tests**: Add unit tests for annAscribe, extract(ascription), checkAnnotated(ascription). Existing S-node tests unchanged (they use fork on the right).

---

## Phase 2: Extend TypedExpr and NativeEnv

**`src/tree-native-elaborate.ts` — new TypedExpr:**
```typescript
export type TypedExpr =
  | { tag: "tlit", tree: Tree, annTree: Tree, type: Tree }  // annTree added
  | { tag: "tfvar", name: string, type: Tree }
  | { tag: "tapp", func: TypedExpr, arg: TypedExpr, type: Tree }
  | { tag: "tI", type: Tree }                                // new
  | { tag: "tK", value: TypedExpr, type: Tree }              // new
  | { tag: "tS", d: Tree, c: TypedExpr, b: TypedExpr, type: Tree } // new
```

**NativeEnv:**
```typescript
export type NativeEnv = Map<string, { tree: Tree, annTree: Tree, type: Tree }>
```

For builtins: `annTree = annAscribe(nativeType, rawTree)` (builtins are trusted constants).
For user defs: `annTree` is the annotated tree from elaboration.
For bound vars (lambda params): use a separate mechanism (see Phase 4).

**New function — `collapseToAnnotated`:**
```typescript
function collapseToAnnotated(texpr: TypedExpr): Tree {
  switch (texpr.tag) {
    case "tlit": return texpr.annTree
    case "tfvar": throw new Error(`Free variable leak: ${texpr.name}`)
    case "tapp": {
      const f = extract(collapseToAnnotated(texpr.func))
      const g = extract(collapseToAnnotated(texpr.arg))
      return annAscribe(texpr.type, apply(f, g))
    }
    case "tI": return I
    case "tK": return annK(collapseToAnnotated(texpr.value))
    case "tS": return annS(texpr.d, collapseToAnnotated(texpr.c), collapseToAnnotated(texpr.b))
  }
}
```

**Update helpers** — `collapseTypedExpr`, `typedHasFreeVar`, `typedToExpr` gain cases for tI/tK/tS.

**Migration**: All existing `tlit` constructions get `annTree: tree` initially (identity — no behavior change). Tests updated mechanically.

---

## Phase 3: Rewrite typedBracketAbstract

The core fix for multi-param lambdas and exact D annotations.

**New signature:**
```typescript
function typedBracketAbstract(
  varName: string, texpr: TypedExpr, varType: Tree,
): { result: TypedExpr, codomain: Tree }
```

Returns a **TypedExpr** (not a collapsed Tree), preserving structure for outer abstractions.

**Cases:**

| Input | Result | Codomain |
|-------|--------|----------|
| `tfvar(varName)` | `tI(Pi(A, K(A)))` | `K(A)` |
| no free var | `tK(texpr, Pi(A, K(T)))` | `K(T)` |
| `tapp(f, tfvar(varName))`, name not in f | `f` (eta) | `f.type.right` |
| `tapp(f, g)` general | `tS(D, [x]f, [x]g, Pi(A, B_result))` | `B_result` |
| `tK(value)` with free var | decompose to `S(K(K), [x]value)` | computed |
| `tS(d, c, b)` with free var | decompose to `tapp(tapp(tlit(LEAF), c), b)`, recurse | computed |
| `tI` | `tK(tI, ...)` (always constant) | `K(T)` |

**Optimizations preserved:**
- `S(K(p), K(q))` = `K(apply(p, q))` — but wrap result in ascription since apply() is opaque
- `S(K(p), I)` = `p` — eta reduction

**Multi-param fix**: `{x y} -> add x y`
- Inner `[y]`: returns TypedExpr `tapp(tlit(ADD), tfvar("x"))` via eta
- Outer `[x]`: sees `tfvar("x")` inside, general S case fires correctly
- No more marker-baked-into-tlit bug

**Tests**: Rewrite existing typedBracketAbstract tests. Add multi-param tests: `{x y} -> add x y`, `{x y} -> add x x`, `{x y} -> add y x`, `{f x} -> f x`.

---

## Phase 4: Rewrite buildNativeWrapped

**Bound vs definition variables**: Use a `boundVars` map:

```typescript
function buildNativeWrapped(
  sexpr: SExpr, env: NativeEnv, expectedType?: Tree,
  boundVars?: Map<string, { marker: Tree, type: Tree }>,
): { texpr: TypedExpr, type: Tree }
```

**svar case**: If name is in `boundVars`, return `tfvar(name, type)`. Otherwise return `tlit(entry.tree, entry.annTree, entry.type)`.

**slam case**:
1. Create marker for dependent type computation
2. Add param to `boundVars` (not to env)
3. Elaborate body — bound vars become `tfvar`, defs become `tlit` with `annTree`
4. `substituteMarker` is no longer needed — bound vars are already `tfvar`
5. `typedBracketAbstract(param, bodyTexpr, A)` → `{ result, codomain }`
6. Return `result` directly (TypedExpr, not wrapped in tlit)

**sapp case**: Unchanged structurally. The `tapp` carries annotated children.

**spi case**: Unchanged — type-level computation doesn't need annotations.

---

## Phase 5: New Declaration Pipeline

**`nativeElabDecl` — non-recursive:**
```
1. Elaborate type → nativeType
2. Elaborate value with expected type → texpr
3. annTree = collapseToAnnotated(texpr)
4. compiled = compileAndEval(value, defs)     ← keep compile.ts as trusted backend
5. checkAnnotated(knownDefs, annTree, nativeType)  ← safety check (should always pass)
6. Verify: treeEqual(extract(annTree), compiled)   ← oracle check during development
7. Register in env: { tree: compiled, annTree, type: nativeType }
8. Register in defs, nativeDefs
```

**Why keep `compileAndEval`**: Two independent bracket abstraction implementations (Expr-level in compile.ts, TypedExpr-level in typedBracketAbstract) should produce the same trees, but verifying this is safer than trusting it. The oracle check (step 6) catches mismatches. Once verified across all prelude defs, step 4 can optionally be replaced with `extract(annTree)`.

**Recursive:**
```
1. Elaborate type → nativeType
2. Add self as bound variable
3. Elaborate body → bodyTexpr
4. typedBracketAbstract(name, bodyTexpr, nativeType) → selfAbstracted
5. annSelfFn = collapseToAnnotated(selfAbstracted)
6. compiled = compileRecAndEval(name, value, defs)  ← compile.ts backend
7. checkAnnotated(knownDefs, annSelfFn, tnArrow(nativeType, nativeType))
8. Register: { tree: compiled, annTree: annAscribe(nativeType, compiled), type: nativeType }
```

FIX results are opaque → stored as ascriptions. The body was verified at T→T (step 7).

**Tests**: Oracle comparison `extract(annTree) === compiled` for all 22 prelude defs. The 9 previously-failing defs should now pass annotation + checking.

---

## Phase 6: Native-Only registerBuiltins

**`src/prelude.ts` — new registerBuiltins:**
```typescript
function registerBuiltins(
  builtins: TreeBuiltin[],
  nativeEnv: NativeEnv, nativeDefs: KnownDefs, defs: Map<string, Tree>,
): void {
  for (const builtin of builtins) {
    registerNativeBuiltinId(builtin.data.id)
    const typeSexpr = parseLine(builtin.type) as SExpr
    const { texpr } = buildNativeWrapped(typeSexpr, nativeEnv, TN_TYPE)
    const nativeType = collapseTypedExpr(texpr)
    nativeDefs.set(builtin.data.id, nativeType)
    nativeEnv.set(builtin.name, {
      tree: builtin.data,
      annTree: annAscribe(nativeType, builtin.data),
      type: nativeType,
    })
    defs.set(builtin.name, builtin.data)
  }
}
```

No `buildWrapped`, no `cocEnv`, no `convertCocType`. Builtin type strings are elaborated directly through the native path.

**Prerequisite**: `buildNativeWrapped` must handle all builtin type strings. These are arrow types and dependent Pi types referencing `Type`, `Tree`, `Bool`, `Nat`, `succ`, etc. — all already supported. Builtins must be registered in dependency order (leaf/stem/fork before triage, true/false before boolElim, etc.).

---

## Phase 7: Native-Only loadPrelude and processDecl

**New loadPrelude:**
```typescript
export function loadPrelude(): {
  defs: Map<string, Tree>, nativeDefs: KnownDefs, nativeEnv: NativeEnv
} {
  const defs = new Map<string, Tree>()
  const nativeDefs: KnownDefs = new Map()
  const nativeEnv: NativeEnv = new Map()

  // 1. Primitive types
  for (const [name, marker] of [["Tree", TN_TREE], ["Bool", TN_BOOL], ["Nat", TN_NAT]]) {
    nativeDefs.set(marker.id, TN_TYPE)
    nativeEnv.set(name, { tree: marker, annTree: marker, type: TN_TYPE })
    defs.set(name, marker)
  }

  // 2. Primitive builtins
  registerBuiltins(PRIMITIVE_BUILTINS, nativeEnv, nativeDefs, defs)

  // 3. Tree-native builtins
  registerBuiltins(TREE_NATIVE_BUILTINS, nativeEnv, nativeDefs, defs)

  // No coc-prelude.disp! No cocEnv!
  return { defs, nativeDefs, nativeEnv }
}
```

**New processDecl** (simplified):
```typescript
export function processDecl(
  name: string, type: SExpr | null, value: SExpr, isRec: boolean,
  nativeEnv: NativeEnv, nativeDefs: KnownDefs, defs: Map<string, Tree>,
): { nativeType: Tree, compiled: Tree, checkOk: boolean }
```

No `cocEnv` parameter. No CoC fallback.

**Type data change**: `defs.get("Tree")` returns `TN_TREE = stem(LEAF)` instead of `TREE_TYPE = fork(fork(LEAF,LEAF), fork(LEAF,LEAF))`. This is correct for the native system — the type Tree is represented as stem(LEAF) in the native encoding. Code that manipulates type values at runtime sees the native representation.

**Remove**: `loadDeclFile` for coc-prelude.disp, `cocPreludePath()`, `buildNameMap` (uses CoC unwrapData).

---

## Phase 8: Remove CoC from repl.ts

**Remove from ReplState**: `cocEnv`

**Remove imports**: `buildWrapped`, `unwrapData`, `unwrapType`, `printEncoded`, `whnfTree`, `CocError`, `Env`, `TREE_TYPE`, `BOOL_TYPE`, `NAT_TYPE` from `coc.js`. Import `registerNativeBuiltinId` from `native-utils.js` instead.

**handleExpr**: Remove try/catch CoC fallback. Just call `handleExprNative` directly.

**handleCommand `:type`**: Remove CoC fallback (`buildWrapped` → `printEncoded`). Native-only.

**Remove**: `recognizeValue` (CoC version), `buildNameMap` import.

**Update handleDecl**: `processDecl` returns `{ nativeType, compiled, checkOk }` — no `env` to assign to `state.cocEnv`.

---

## Phase 9: Cleanup

**Remove from `src/tree-native-elaborate.ts`** (~350 lines):
- `annotateTree`, `inferIntermediateType`, `inferGroundType` — replaced by elaboration
- `convertCocType`, `convertPiBody` — no more CoC type conversion needed
- `nativeCheckDecl` — old CoC-dependent pipeline
- `substituteMarker` — bound vars use tfvar directly
- All imports from `coc.ts`: `termTag`, `unPi`, `unVar`, `unLam`, `unApp`, `TREE_TYPE`, `BOOL_TYPE`, `NAT_TYPE`, `whnfTree`, `evalToNative`, `encVar`, `cocCheckDecl`, `type Env`

**Remove from `src/prelude.ts`** (~50 lines):
- All `coc.ts` imports: `encType`, `wrap`, `unwrapData`, `buildWrapped`, `cocCheckDecl`, `Env`
- `buildNameMap` (uses `unwrapData`)
- `cocPreludePath()`

**Move tests**: `test/tree-native-pipeline.test.ts` tests that use `convertCocType`, `annotateTree`, `nativeCheckDecl` → move to `test/coc-pipeline-legacy.test.ts` or rewrite for new pipeline.

**Keep**: `src/coc.ts`, `src/coc-printer.ts`, `test/coc.test.ts` — for the CoC encoding tests and tree-encoded step functions.

---

## Critical Files

| File | Changes |
|------|---------|
| `src/native-utils.ts` | **New** — freshMarker, registerNativeBuiltinId extracted |
| `src/tree-native-checker.ts` | Add annAscribe, extend extract + checkAnnotated |
| `src/tree-native-elaborate.ts` | TypedExpr extension, typedBracketAbstract rewrite, buildNativeWrapped rewrite, new collapseToAnnotated, new nativeElabDecl, remove annotateTree/convertCocType |
| `src/prelude.ts` | registerBuiltins rewrite, loadPrelude rewrite, processDecl simplification, remove CoC imports |
| `src/repl.ts` | Remove cocEnv, remove CoC fallback, remove CoC imports |
| `src/coc.ts` | Re-export from native-utils.ts, otherwise untouched |

## Verification

After each phase: `npx vitest run` — all tests pass (619+).

**Key oracle tests** (Phase 5):
- For all 22 prelude defs: `extract(annTree) === compileAndEval(value, defs)` — compilation parity
- For all 22 prelude defs: `checkAnnotated(defs, annTree, type) === true` — annotation correctness (expect 22/22, up from 13/22)

**REPL parity** (Phase 8):
- Every test in `test/repl.test.ts` produces identical output
- `printNativeType` already handles dependent Pi with binder names (a, b, c, ...)
- `recognizeValueNative` matches Bool/Nat/Tree the same way

**End state**: Zero imports from `coc.ts` in repl.ts and prelude.ts. Annotated trees are self-contained proof certificates. The checker can verify any annotated tree without external state (knownDefs is optional memoization).
