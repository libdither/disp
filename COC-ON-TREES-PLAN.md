# CoC on Tree Calculus: Implementation Plan

## Motivation

The current pipeline has a clean separation: type checking happens on SExpr (named
variables), and trees are a separate compilation target. This works, but it means the
type checker lives *outside* the tree calculus world — it's TypeScript code operating
on a TypeScript data structure.

This plan encodes the entire Calculus of Constructions — terms, types, and type checking
— directly as tree calculus operations. The key benefits:

1. **Self-hosting / bootstrapping**: If the type checker operates on trees using tree
   calculus primitives (apply, treeApply), the type checker itself can eventually be
   compiled to trees. A system that type-checks itself within tree calculus.

2. **Unification of substitution**: Instead of maintaining two separate substitution
   mechanisms (named substitution in `typecheck.ts`, bracket abstraction in `compile.ts`),
   both type checking and compilation use the same mechanism: bracket abstraction + `apply()`.

3. **Toward a reflective tower**: With terms, types, and type checking all living in tree
   calculus, programs can inspect and manipulate their own types at runtime — the foundation
   for a reflective dependently typed language.

4. **Simplification**: The bind-tree AST (`ast.ts`) and its WHNF/substitution become
   unnecessary. The tree calculus `apply()` *is* the evaluator; bracket abstraction *is*
   the substitution mechanism. No separate concepts needed.

## Architecture Overview

A new module `src/coc.ts` (+ `test/coc.test.ts`) implements the Calculus of Constructions
entirely within tree calculus. Instead of the current pipeline where type checking happens
on SExpr (named variables) and trees are a separate compilation target, this system encodes
CoC terms—including their types—directly as trees.

### Key Differences from Current Pipeline

| Aspect | Current (`typecheck.ts`) | New (`coc.ts`) |
|--------|-------------------------|----------------|
| Type checking substrate | SExpr (named variables) | Tree (tree-native encoding) |
| Context | `Context = { name, type, value? }[]` | `Env = Map<string, Tree>` (name → wrapped tree) |
| Substitution | `substNamed()` on SExpr | `apply()` on bracket-abstracted trees |
| Variable binding | Named variables + alpha-renaming | Bracket abstraction (S/K/I elimination) |
| Type info | Separate context | Embedded in each wrapped term |

### Encoding

```
Type         = LEAF
Var(i)       = stem(marker)                 -- fresh neutral markers
App(M, N)    = fork(LEAF, fork(M, N))       -- leaf-tagged compound
Lam(A, body) = fork(stem(A), body)          -- stem-tagged, body bracket-abstracted
Pi(A, body)  = fork(fork(A, LEAF), body)    -- fork-tagged, body bracket-abstracted
```

Tag discrimination via two-level triage:
- Level 1: `leaf` → Type, `stem(_)` → Var, `fork(L, R)` → compound
- Level 2 (on L): `leaf` → App, `stem(_)` → Lam, `fork(_,_)` → Pi

```
WRAP(data, type) = Church pair: collapse(bracketAbstract("f", app(app(fvar("f"), tree(data)), tree(type))))
```

This compiles to `S(S I (K data))(K type)` via bracket abstraction. Extraction uses
tree calculus selectors:
- `apply(wrapped, K)` → data, where `K = stem(LEAF)` (returns first arg, discards second)
- `apply(wrapped, K*)` → type, where `K* = fork(LEAF, I)` (discards first arg, returns second)

Why this works:
- `K(data)(type)`: `stem(LEAF)` applied to `data` = `fork(LEAF, data)`, then applied to `type` = `data` (Rule 1). ✓
- `K*(data)(type)`: `fork(LEAF, I)` applied to `data` = `I` (Rule 1), then `I` applied to `type` = `type`. ✓

### How "No Context" Works

| Scenario | How it works |
|----------|-------------|
| Variable lookup | `env.get(name)` returns a wrapped tree; type extracted via `apply(wrapped, K*)` |
| Lambda bound var | `env.set(name, wrap(encVar(marker), domain))` — domain type carried IN the wrap |
| Let binding | `env.set(name, wrap(encoded_value, encoded_type))` — fully self-contained |
| Recursive binding | Pre-extend with `wrap(encVar(selfMarker), annotated_type)` |
| Definition unfolding | Definitions eagerly substituted during build — `whnf` needs no context to unfold |

### The Key Algorithm: `treeToExprReplacing`

The bridge between tree-encoded terms and bracket abstraction. Given a completed Tree
containing a marker `target`, converts it back to an `Expr` by replacing `target` with
`eFvar(name)`, preserving all other structure:

```typescript
function treeToExprReplacing(t: Tree, target: Tree, varName: string): Expr {
  if (treeEqual(t, target)) return eFvar(varName)  // O(1) via hash-consing
  if (t.tag === "leaf") return eTree(t)
  if (t.tag === "stem") {
    const child = treeToExprReplacing(t.child, target, varName)
    if (child.tag === "tree" && treeEqual(child.value, t.child)) return eTree(t)
    return eApp(eTree(LEAF), child)  // stem(x) = treeApply(LEAF, x)
  }
  // fork
  const left = treeToExprReplacing(t.left, target, varName)
  const right = treeToExprReplacing(t.right, target, varName)
  if (left.tag === "tree" && treeEqual(left.value, t.left) &&
      right.tag === "tree" && treeEqual(right.value, t.right))
    return eTree(t)  // no marker found — return original tree
  return eApp(eApp(eTree(LEAF), left), right)  // fork(a,b) = treeApply(stem(a), b)
}
```

Then `abstractMarkerOut(tree, target)` = `collapse(bracketAbstract(name, treeToExprReplacing(tree, target, name)))`.

This connects encoded CoC terms to the existing bracket abstraction machinery.

### Build Process for Lambda (the core operation)

For `{x} -> body` checked against `Pi(A, codBody)`:

1. Create fresh marker: `m = freshMarker()`
2. Create wrapped neutral: `wrap(encVar(m), A)` — carries its type
3. Extend env: `env.set("x", wrappedNeutral)`
4. Recursively `buildWrapped(body, extendedEnv, expectedCod)` → wrapped result
5. Extract result data and type from the wrapped result
6. `abstractMarkerOut(resultData, encVar(m))` → bracket-abstracted lambda body
7. `abstractMarkerOut(resultType, encVar(m))` → bracket-abstracted codomain (for the type)
8. Return `wrap(encLam(A, abstractedBody), encPi(A, abstractedCodomain))`

---

## Files

| File | Action | Purpose |
|------|--------|---------|
| `src/coc.ts` | **CREATE** | All encoding, wrapping, type checking, building |
| `test/coc.test.ts` | **CREATE** | Test suite (~50 tests across 6 phases) |
| `src/compile.ts` | **MODIFY** | Export `eTree`, `eFvar`, `eApp` (add `export` to 3 existing functions) |
| `src/repl.ts` | **MODIFY** | Add `cocMode` flag and dual-pipeline support |
| All other files | **KEEP** | Unchanged; reuse `tree.ts`, `parse.ts` |

---

## Phase 1: Encoding Primitives

**Goal**: Tree-native encoding for CoC terms + WRAP structure. No type checking yet.

### Functions (~15)

```typescript
// Encoding constructors
encType(): Tree                    // → LEAF
encVar(marker: Tree): Tree         // → stem(marker)
encApp(m: Tree, n: Tree): Tree     // → fork(LEAF, fork(m, n))
encLam(domain: Tree, body: Tree): Tree  // → fork(stem(domain), body)
encPi(domain: Tree, body: Tree): Tree   // → fork(fork(domain, LEAF), body)

// Tag recognition
termTag(t: Tree): "type"|"var"|"app"|"lam"|"pi"|null

// Destructuring
unVar(t: Tree): Tree | null
unApp(t: Tree): { func: Tree, arg: Tree } | null
unLam(t: Tree): { domain: Tree, body: Tree } | null
unPi(t: Tree): { domain: Tree, body: Tree } | null

// WRAP (Church pair via bracket abstraction)
wrap(data: Tree, type: Tree): Tree    // collapse(bracketAbstract("f", app(app(fvar("f"), tree(data)), tree(type))))
unwrapData(wrapped: Tree): Tree       // apply(wrapped, K) where K = stem(LEAF)
unwrapType(wrapped: Tree): Tree       // apply(wrapped, K*) where K* = fork(LEAF, I)

// Selectors (constants)
K_SEL: Tree                           // stem(LEAF) — first projection
K_STAR_SEL: Tree                      // fork(LEAF, I) — second projection

// Fresh markers (fork-based to avoid collision with encodings)
freshMarker(): Tree               // unique fork(LEAF, stem^n(LEAF))
```

### Tests (~8)

- `encType` returns LEAF
- `encVar` creates stem(marker)
- `encApp`/`encLam`/`encPi` create correct structures
- `termTag` correctly identifies all 5 tags
- Destructuring round-trips with encoding
- `wrap`/`unwrapData`/`unwrapType` round-trips (via K and K* selectors)
- `freshMarker` generates distinct markers (fork-based, disjoint from encodings)

### Dependencies
- `src/tree.ts`, `src/compile.ts` (for `bracketAbstract`, `collapse`, `eTree`, `eFvar`, `eApp`)

---

## Phase 2: Bracket Abstraction Integration

**Goal**: Build bracket-abstracted lambda/Pi bodies from tree-encoded terms. When
`apply()`'d to an argument, the result is the correctly-encoded substituted body.

### Functions (~5)

```typescript
// Expr-level builders for constructing encoded terms with free variables
exprStem(e: Expr): Expr            // stem via treeApply
exprFork(a: Expr, b: Expr): Expr   // fork via treeApply
exprEncApp(m: Expr, n: Expr): Expr // encoded App as Expr
exprEncLam(domain: Expr, body: Expr): Expr
exprEncPi(domain: Expr, body: Expr): Expr

// The key algorithm
treeToExprReplacing(t: Tree, target: Tree, varName: string): Expr
abstractMarkerOut(tree: Tree, target: Tree): Tree
```

### Tests (~6)

- Variable in `encApp`: `[x]encApp(x, y)` applied to arg gives `encApp(arg, y)`
- Variable in `encLam` domain: `[x]encLam(x, body)` applied to arg gives `encLam(arg, body)`
- Identity body: `[x]x` = I combinator, `apply(I, arg)` = arg
- Constant body: `[x]c` = K(c), `apply(K(c), arg)` = c
- Variable used twice: `[x]encApp(x, x)` applied to arg gives `encApp(arg, arg)`
- Nested encoding: variable inside inner lambda domain

### Changes to `src/compile.ts`
- Export `eTree`, `eFvar`, `eApp` (add `export` keyword to 3 existing functions)

---

## Phase 3: WHNF, Convertibility, and Core Building

**Goal**: Working type checker on tree-encoded terms. The core `buildWrapped` function.

### Functions (~8)

```typescript
class CocError extends Error { ... }

// WHNF: reduce head beta-redexes
whnf(t: Tree, budget?): Tree
// App(Lam(A, body), arg) → apply(body, arg) — native tree calc beta reduction!

// Convertibility
convertible(a: Tree, b: Tree, budget?): boolean
convertibleUnderBinder(body1: Tree, body2: Tree, budget?): boolean
// Apply both to fresh neutral, compare results

// The main builder
type Env = Map<string, Tree>  // name → wrapped tree
buildWrapped(sexpr: SExpr, env: Env, expectedType?: Tree, budget?): Tree

// Helpers
ensureType(typeTree: Tree): void  // throw if not convertible with encType()
```

### `buildWrapped` cases

| SExpr tag | Action |
|-----------|--------|
| `stype` | Return `wrap(encType(), encType())` — Type : Type |
| `svar` | Lookup in env, return wrapped value (carries its type) |
| `spi` | Build domain (check it's Type), create neutral for binder, build codomain (check it's Type), abstract marker out of both, return `wrap(encPi(...), encType())` |
| `slam` | Requires expectedType. WHNF it to Pi. Create neutral with domain type. Build body with expected codomain. Abstract marker out. Return `wrap(encLam(...), expectedType)` |
| `sapp` | Build func, extract its type (should be Pi). Build arg (check against domain). Result data = `encApp(f, x)`. Result type = `apply(codomain_body, argData)`. Return wrapped. |

### Tests (~15)

**WHNF:**
- Type, Var, Lam, Pi are already in WHNF
- `App(Lam(A, I), x)` reduces to `x` (beta reduction)
- Stuck application stays stuck
- Chained beta: `App(App(Lam(_, Lam(_, I)), a), b)` reduces to `b`

**Convertibility:**
- Identical terms (O(1) via hash-consing)
- Beta-convertible: `App(Lam(_, I), y)` ≡ `y`
- Different variables not convertible
- Pi types with convertible components
- Bodies compared under fresh binder

**buildWrapped basics:**
- `Type` → `wrap(LEAF, LEAF)`
- Variable lookup returns wrapped value from env
- Unbound variable throws `CocError`
- `(A : Type) -> Type` produces Pi with type Type
- `{x} -> x` checked against `(A:Type) -> A` produces identity lambda
- Application: function applied to argument with matching types

---

## Phase 4: Full Declaration Pipeline

**Goal**: Handle declarations (`let name : type := value`), dependent types,
multi-argument lambdas, and non-dependent arrows.

### Functions (~3)

```typescript
cocCheckDecl(env: Env, name: string, type: SExpr|null, value: SExpr, isRec?: boolean):
  { env: Env, type: Tree }

// For recursive defs:
cocCheckRecDecl(env: Env, name: string, type: SExpr, value: SExpr):
  { env: Env, type: Tree }
```

### Tests (~8)

- `let Bool : Type := (R : Type) -> R -> R -> R` — defines Bool
- `let id : (A : Type) -> A -> A := {A x} -> x` — polymorphic identity
- Declarations build on each other (Bool then not)
- `id Type` has type `Type -> Type` (dependent application)
- Type mismatch in application throws
- Multi-param lambda `{A x} -> x` desugars correctly
- Non-dependent arrow `A -> B` works as sugar for `(_ : A) -> B`
- Declaration without type annotation (infer from value)

---

## Phase 5: REPL Integration

**Goal**: Wire the new pipeline into the REPL as an alternative mode.

### Functions (~4)

```typescript
// Full normalization (not just WHNF)
normalize(encoded: Tree, budget?): Tree

// Pretty-print encoded terms
printEncoded(encoded: Tree): string

// REPL modifications
// Add cocMode: boolean and cocEnv: Env to ReplState
// Dual pipeline: :coc command toggles mode
```

### Changes to `src/repl.ts`
- Add `cocMode` flag and `cocEnv: Env` to `ReplState`
- When `cocMode`: `handleDecl` uses `cocCheckDecl`, `handleExpr` uses `buildWrapped`
- Add `:coc` command to toggle mode
- Keep old pipeline as default — fully backward compatible

### Tests (~5)

- `processLine` in coc mode handles declarations
- `processLine` in coc mode handles expressions
- Church literal recognition works from wrapped terms
- `:coc` command toggles mode
- Coc mode loads prelude

---

## Phase 6: Recursive Definitions and Validation

**Goal**: Handle `let rec`, Church encoding evaluation, full prelude, and validate
against the existing pipeline.

### Recursive Definition Strategy

1. Require type annotation (same as current system)
2. Pre-extend env with `wrap(encVar(selfMarker), annotatedType)`
3. Build body in extended env — self-references become `encVar(selfMarker)`
4. Abstract self-marker out of body data
5. Build omega combinator: `omega = [x] body[self := [v] x x v]`; result = `treeApply(omega, omega)`
6. Wrap with annotated type

### Tests (~8)

- Simple recursive definition type-checks
- Church `not true = false` (behavioral equivalence)
- Church `add 2 3 = 5` (behavioral equivalence)
- **Full prelude loads without errors** (the ultimate test)
- Equivalence with old pipeline: `id Type`, `not true`, `add 2 3` produce same behavior

---

## Summary

| Phase | Goal | Functions | Tests | Depends On |
|-------|------|-----------|-------|------------|
| 1 | Encoding primitives | ~15 | ~8 | tree.ts, compile.ts |
| 2 | Bracket abstraction integration | ~5 | ~6 | Phase 1, compile.ts |
| 3 | WHNF + convertibility + buildWrapped | ~8 | ~15 | Phase 2, parse.ts |
| 4 | Declaration pipeline | ~3 | ~8 | Phase 3 |
| 5 | REPL integration | ~4 | ~5 | Phase 4, repl.ts |
| 6 | Recursive defs + validation | ~3 | ~8 | Phase 5 |
| **Total** | | **~38** | **~50** | |

### New files: 2 (`src/coc.ts`, `test/coc.test.ts`)
### Modified files: 2 (`src/compile.ts` — 3 exports, `src/repl.ts` — dual mode)
### Unchanged files: everything else
