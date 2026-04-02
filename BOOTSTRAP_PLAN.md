# Bootstrap Plan: Single Pipeline to Self-Hosted Type Checker

Goal: SExpr → TypedExpr → annotated Tree, checked by a tree-calculus verifier.

## Current State

The TypedExpr pipeline works for non-recursive definitions. 481/483 tests pass.
Remaining dependencies on compile.ts in the elaborator:
- `compileRecAndEval` — recursive definitions
- `collapseAndEval` + `bracketAbstract` — type-level computation (Pi codomains)
- `typedExprToExpr` — bridge function for above

## Issue 1: Recursive defs use compileRecAndEval

**Problem**: `nativeElabDecl` already computes the step function via TypedExpr
(`selfFnTree = collapseRaw(selfFn)`), then redundantly recompiles through the
old Expr path via `compileRecAndEval(name, value, compiledDefs)`.

**Fix**: Replace `compileRecAndEval` call with `apply(FIX, selfFnTree)`.
FIX is a tree constant. The TypedExpr pipeline already produces the step function.

Details:
- Check `teHasFreeVar(name, bodyTE)` — if body doesn't reference itself, skip FIX
- Annotate the step function structurally via `collapseAnnotated(selfFn, nativeDefs)`
- Only the FIX result remains as an ascription (legitimately opaque — infinite unfolding)
- Import `FIX` from compile.ts as a tree constant (no Expr functions needed)

Difficulty: Easy. ~15 lines changed.

## Issue 2: Type-level computation uses Expr

**Problem**: `abstractTypeMarker` converts Tree → Expr → bracketAbstract → collapseAndEval → Tree.
`typeTreeOf` for lam nodes does the same. `typedExprToExpr` exists solely for this.

**Fix**: Implement `treeAbstract(tree, marker) → Tree` directly on Trees:

```
treeAbstract(t, m):
  if t == m: return I
  if !contains(t, m): return fork(LEAF, t)     // K(t)
  if stem(a):
    abs = treeAbstract(a, m)
    if isK(abs): return fork(LEAF, t)           // S(K(LEAF), K(a)) = K(stem(a))
    else: return fork(stem(fork(LEAF, LEAF)), abs)  // S(K(LEAF), abs)
  if fork(a, b):
    absA = treeAbstract(a, m)
    absB = treeAbstract(b, m)
    // S(Kp)(Kq) = K(apply(p, q))
    if isK(absA) && isK(absB): return fork(LEAF, apply(kInner(absA), kInner(absB)))
    // S(Kp) I = p  (eta)
    if isK(absA) && absB == I: return kInner(absA)
    // General S
    return fork(stem(absA), absB)
```

This eliminates: `treeToExprReplacing`, `treeContains` (type-level), `abstractTypeMarker`,
`typedExprToExpr`, and all Expr/bracketAbstract/collapseAndEval imports from the elaborator.

Difficulty: Medium. ~40 lines new, ~80 lines deleted.

Validation: compare `treeAbstract(tree, marker)` against `abstractTypeMarker(tree, marker)`
for all marker positions in all prelude type computations.

## Issue 3: Church-encoded types fail to elaborate (mkPair, fold)

**Problem**: `Pair A B` produces an opaque type tree when A and B are markers.
The elaborator can't decompose the result as a Pi type. "Lambda needs function type."

**Root cause**: `typeTreeOf(app(Pair, A_marker))` eagerly evaluates, producing a
complex tree that IS structurally a Pi type but the elaborator doesn't recognize it.

**Options**:
- **(a) Symbolic expansion**: Teach the elaborator to expand `Pair A B` to
  `(R : Type) -> (A -> B -> R) -> R` by registering type constructor expansion rules.
  Hard, error-prone.
- **(b) Opaque tolerance**: Accept opaque types and produce ascription annotations.
  This is the current fallback in `processDecl`. Already works.
- **(c) Deferred**: Focus on issues 1, 2, 4 first. Revisit when the rest of the
  pipeline is clean.

**Recommendation**: Option (c). The 2 [POST] tests document the aspiration.
The fallback path handles these definitions correctly at runtime.

## Issue 4: Encode checkAnnotated as a tree constant

**Problem**: `checkAnnotated` is TypeScript. The pipeline becomes self-hosted when
this is a tree combinator: `(defs, annotatedTree, type) → Bool`.

**Approach**: Follow the TYPECHECK pattern in `src/tree-native.ts`:

1. Build the checker logic as Expr trees (eTree, eFvar, eApp, bracketAbstract)
2. Compile to a tree constant via collapseAndEval
3. Wrap with fuel-based recursion (fold/tFix)

**Sub-functions to encode**:

| Function | Tree encoding |
|----------|--------------|
| `extract(ann)` | Recursive triage dispatch on annotated tree |
| `isOfBaseType(v, T)` | Triage on T (stem chain), validate v |
| `isNonDep(B)` | Check if B = fork(LEAF, _) |
| K rule | Check inner at codomain type |
| S rule | Check b at Pi(A,D), c at Pi(A, sCodomain(D,B)) |
| Triage rule | Dispatch on domain: Tree/Nat/Bool, check branches |
| Ascription | Lookup body in defs list, compare types |
| `stemCodomain`, `forkCodomain`, `sCodomain` | Already tree functions (combinators) |

**Data structures**:
- `defs` map → Church list of `fork(treeId, type)` pairs with FAST_EQ for lookup
- Recursion → fuel-based fold (like existing step functions)
- Boolean results → TT/FF (LEAF/stem(LEAF))

**Size estimate**: ~200-400 lines of Expr construction (TYPECHECK is ~150 lines).
The checker is simpler than CoC inference (no environments, no WHNF, no substitution)
but has more dispatch cases (K/S/Triage/Ascription × Tree/Nat/Bool domains).

**Validation**: For each prelude definition, verify that the tree-encoded checker
produces the same result as the TypeScript `checkAnnotated`.

Difficulty: Hard but well-patterned. The existing TYPECHECK is a working template.

## Execution Order

```
Issue 1 (recursive defs)     →  eliminates compileRecAndEval from elaborator
Issue 2 (tree abstraction)   →  eliminates ALL Expr from elaborator
                                 elaborator imports only FIX from compile.ts (a Tree constant)
Issue 4 (checker as tree)    →  self-hosted type checking
Issue 3 (Church types)       →  deferred, revisit after pipeline is clean
```

After 1+2: the elaborator is Expr-free. Pipeline is `elaborate → compileTE → checkAnnotated`.
After 4: the checker is a tree. Scoring functions become possible:
```
score(program) = treeCheck(annotate(program), type) * size(program) * speed(program)
```

All trees. All in the same universe. Ready for neural synthesis.
