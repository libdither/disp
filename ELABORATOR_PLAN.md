# Elaborator Plan

Bidirectional type-directed compilation for Disp. This document covers
the file split, the elaborator's check/infer architecture, the trusted
table, scope changes, and per-node compilation rules.

## Motivation

Disp has shared syntax: `{x : A} -> B` is a lambda or a Pi depending
on context. The current compiler threads no type information — it
compiles everything as lambdas and erases annotations. The elaborator
resolves syntactic ambiguity by inspecting the **expected type** using
the kernel's O(1) signature checks.

The current COMPILATION.typ proposes a `"term" | "type"` mode flag,
but mode is a lossy proxy for the expected type. It can't distinguish
a binder that should be a lambda returning a type from one that should
be a Pi. Bidirectional check/infer replaces the mode flag with the
actual expected type, which the compiler reads using kernel metadata
extractors. The kernel is the only source of truth about what types
mean.

## Step 1: File split

Split `src/parse.ts` (currently ~1275 lines, 6 sections) into two
files along the existing section boundary.

### `src/parse.ts` — tokenizer + grammar

Keeps sections 1–4:

- Tokens + tokenizer (`tokenize`)
- AST types (`Expr`, `Param`, `TypedField`, `NamedField`, `RecMember`, `Item`)
- Parser combinators (`ok`, `err`, `map`, `seq`, `alt`, `many`, etc.)
- Grammar productions (`expr`, `binder`, `app`, `atom`, `braced`, etc.)
- `parseItems(src: string): Item[]`

Exports: `tokenize`, `parseItems`, all AST types, `Tok`.

Changes to parse.ts: add `trust` to the keyword set. Add grammar
production for `trust` items (identical shape to `let`). Parse
`trust` into a new `RecMember` variant:

```typescript
// Added to RecMember union:
| { tag: "trust"; name: string; type: Expr | null; body: Expr }
```

### `src/compile.ts` — bracket abstraction + driver + elaborator

Takes sections 5–6 from parse.ts, plus the new elaborator:

- `Cir` type and combinators (`S`, `K`, `I_CIR`, `cap`)
- Bracket abstraction (`containsFree`, `abstractName`, `eliminateLams`)
- Tree-level abstraction (`abstractTree`, `containsTree`) — new
- `cirToTree`, `buildSelector`, `selectorTree`
- `ScopeEntry` (extended with `type` field)
- `exprToCir` (becomes the untyped fallback path)
- `resolveExprRecord`, `compileExpr`
- `check`, `infer` (new — the elaborator)
- Trusted table
- Driver (`parseProgram`, `resolveUse`, `runItem`)
- Exports: `parseProgram`, `Decl`, `ParseItemStats`, `ParseProgramOptions`

Imports from `parse.ts`: AST types, `parseItems`.
Imports from `tree.ts`: `Tree`, `LEAF`, `stem`, `fork`, `applyTree`, `FAST_EQ`, etc.

### `src/run.ts` — unchanged

Currently imports `parseProgram` and `ParseItemStats` from `parse.ts`.
After split, imports from `compile.ts` instead. No other changes.

### Dependency graph

```
parse.ts  →  (no imports from compile.ts or tree.ts)
              ↑
compile.ts → parse.ts, tree.ts
              ↑
run.ts    →  compile.ts, tree.ts
```

No circular dependencies. Parse is pure syntax; compile depends on
parse + tree runtime.

### Test impact

- `test/parser.test.ts` — imports from `parse.ts`. May need to also
  import from `compile.ts` if any tests use `parseProgram`. Check
  which imports are used and update.
- `test/tree.test.ts` — no changes.
- `test/disp.test.ts` — imports `runFile` from `run.ts`. No changes.

## Step 2: Scope changes

### Current ScopeEntry

```typescript
interface ScopeEntry { tree?: Tree; fields?: string[]; fieldTrees?: Tree[] }
```

### New ScopeEntry

```typescript
interface ScopeEntry {
  tree?: Tree
  type?: Tree | null    // null = untyped, undefined = not yet set
  fields?: string[]
  fieldTrees?: Tree[]
  fieldTypes?: (Tree | null)[]  // per-field types for open
}
```

`type: null` means "untyped" — the expression compiled but we have no
type information. This is the escape hatch for fixpoints, bare leaves,
unannotated bindings, and all existing code. When checking against
`null`, the compiler skips type-directed dispatch and compiles like
today.

`type: null` is NOT a kernel type. It does not participate in the
universe hierarchy. It's the absence of type information in the
compiler. No kernel changes needed.

### Scope carries types through `use` / `open`

`resolveUse` currently returns `ScopeEntry { tree, fields, fieldTrees }`.
Extended to also return `fieldTypes`. This requires extending `Decl`
to carry types:

```typescript
type Decl =
  | { kind: "Def"; name: string; tree: Tree; type?: Tree | null }
  | { kind: "Test"; lhs: Tree; rhs: Tree }
```

Then `resolveUse` extracts `fieldTypes` from the Decl array:

```typescript
function resolveUse(path: string): ScopeEntry {
  // ... parse and run items ...
  const fieldTypes = decls
    .filter(d => d.kind === "Def")
    .map(d => d.type ?? null)
  return { tree, fields, fieldTrees, fieldTypes }
}
```

`open` splices field types into scope alongside trees:

```typescript
case "open":
  for (let i = 0; i < n; i++) {
    define(name, {
      tree: fieldTree,
      type: entry.fieldTypes?.[i] ?? null,
      ...
    })
  }
```

## Step 3: Trusted table

The elaborator needs handles to kernel-provided trees for constructing
types and checking signatures. These are introduced with `trust`:

```disp
trust Pi = {domain, codFn} -> wait kernel_ref.pi (make_pi_meta domain codFn)
trust Arrow = {a, b} -> Pi a ({_} -> b)
trust Type = {rank} -> wait kernel_ref.type rank
trust zero = t
trust succ = {n} -> t t n
```

`trust` compiles identically to `let` but also registers the compiled
tree in a side table:

```typescript
const trusted: Map<string, Tree> = new Map()

case "trust":
  const { tree } = compileBinding(it.name, it.type, it.body)
  trusted.set(it.name, tree)
```

The elaborator reads from `trusted` to:

- Build Pi types: `trusted.get("Pi")` — used when compiling binders
  in type mode (expected type is a universe).
- Build Arrow types: `trusted.get("Arrow")` — optional shortcut.
- Check universes: `trusted.get("Type")` — used when verifying that
  a type annotation is valid.
- Expand numeric literals: `trusted.get("zero")`, `trusted.get("succ")`
  — replaces current scope-based lookup.

If no `trust` items are in scope (file doesn't import the kernel), the
elaborator falls back to untyped compilation everywhere. All existing
code works unchanged.

### Propagation through `trust open`

Trusted definitions propagate through `use` / `open` only when
explicitly requested via `trust open`. The `open` statement expands
into synthetic `let` bindings that access each field of the opened
record; `trust open` prefixes `trust` to all of those expanded
bindings.

```disp
use "kernel.disp" as K
trust open K             // expands to: trust Pi = K.Pi; trust Type = K.Type; ...
open K                   // expands to: let Pi = K.Pi; let Type = K.Type; ...
                         // (values in scope, but NOT in trusted table)
```

This is explicit opt-in: a file must say `trust open` to activate
the elaborator's kernel-aware dispatch. Plain `open` imports values
without trusting them for type-directed compilation.

Grammar change: `trust` becomes a modifier on `open` (in addition
to being a standalone statement). The AST for open gets a boolean:

```typescript
// Updated RecMember variant for open:
| { tag: "open"; trust: boolean; expr: Expr }
```

Implementation in the driver:

```typescript
case "open": {
  const isTrust = it.trust === true
  const entry = lookupEntry(it.name) ?? resolveUse(...)
  for (let i = 0; i < entry.fields.length; i++) {
    const name = entry.fields[i]
    const scopeEntry = {
      tree: entry.fieldTrees[i],
      type: entry.fieldTypes?.[i] ?? null
    }
    define(name, scopeEntry)
    if (isTrust) {
      trusted.set(name, entry.fieldTrees[i])
    }
  }
}
```

Future extension: selective trust (`trust open K { Pi, Type }`) to
trust only specific fields.

## Step 4: The elaborator — check and infer

Two mutually recursive functions that extend the existing `compileExpr`
pipeline.

```typescript
function check(e: Expr, expected: Tree | null, scope, resolveUse): Tree
function infer(e: Expr, scope, resolveUse): { tree: Tree, type: Tree | null }
```

`compileExpr(e, lookup, resolveUse)` becomes
`check(e, null, lookup, resolveUse)` — identical behavior when
expected is null.

### Kernel queries used by the elaborator

All dispatch decisions use the kernel's existing tree-level operations.
No new kernel code needed:

```typescript
// Signature checks — O(1) via hash-consing
function isUniverse(t: Tree): boolean  // has_sig kernel_ref.type
function isPi(t: Tree): boolean        // has_sig kernel_ref.pi

// Metadata extraction — O(1) tree projections
function piDomain(t: Tree): Tree       // pi_meta_domain(type_meta(t))
function piCodFn(t: Tree): Tree        // pi_meta_cod_fn(type_meta(t))
function univRank(t: Tree): Tree       // type_meta(t)

// Neutral check
function isNeutral(t: Tree): boolean   // has_sig kernel.hyp_reduce

// Hypothesis construction
function makeHyp(type: Tree, id: Tree): Tree  // wait kernel.hyp_reduce (make_neutral_meta type id)
```

These are thin wrappers around `applyTree` and `FAST_EQ` on known
kernel trees. The kernel trees are obtained from the trusted table or
by compiling the kernel prelude.

### Hypothesis identity

Pi types derive hypothesis identity from their own metadata. When the
elaborator creates a hypothesis for checking `{x : A} -> body` against
`Pi(D, codFn)`, the identity is the Pi type's metadata:

```typescript
const piMeta = typeMeta(expected)  // fork(domain, codFn)
const hyp = makeHyp(D, piMeta)
```

Nested Pi checks produce distinct hypotheses because the inner Pi
(from evaluating the outer's codomain) has structurally different
metadata. No external depth counter needed.

When compiling a binder against a universe (type mode), the
elaborator constructs the Pi metadata from the compiled domain and
codomain function, which serves as identity.

### Infer-mode hypothesis identity

When `infer` compiles a binder with an annotated domain, it needs a
hypothesis but has no Pi metadata to use as identity. The identity
is derived from the elaborator's context — no global mutable state:

```typescript
// For infer-mode multi-param binders {x : A, y : A} -> ...,
// disambiguate with scope depth and parameter index:
const id = fork(A_tree, fork(buildNat(scopeDepth), buildNat(paramIndex)))
const hyp = makeHyp(A_tree, id)
```

Where `scopeDepth` is the current scope stack length (already
available) and `paramIndex` is the parameter's position within
the multi-param binder (a loop index, not global state).

Hypotheses are structurally unique by construction:
- Different domains → different `A_tree` in the id.
- Same domain, different nesting → different `scopeDepth`.
- Same domain, same depth, multi-param → different `paramIndex`.

This is self-describing and free of global mutable state —
important for eventual self-hosting where the elaborator is
rewritten in Disp.

### Tree-level abstraction

The elaborator's type-directed paths (Case 2 and infer-mode
binders) need to construct codomain functions by abstracting over
a hypothesis *tree*, not a named *Cir* variable. The existing
`abstractName`/`eliminateLams` pipeline operates on `Cir` with
string names, but in type mode the hypothesis is already compiled
into a `Tree` value — the name is gone.

New primitive: `abstractTree(h, body)` — given a tree `body`
containing subtree `h`, produce a tree `F` such that
`apply(F, v) = body[h := v]`.

```typescript
function abstractTree(h: Tree, body: Tree): Tree {
  if (FAST_EQ(body, h)) return I_TREE
  if (!containsTree(h, body)) return kTree(body)
  if (body.tag === "fork") {
    const l = abstractTree(h, body.left)
    const r = abstractTree(h, body.right)
    // η-reduction: S K_f I → f
    if (l.tag === "fork" && FAST_EQ(l.left, K_TREE) && FAST_EQ(r, I_TREE))
      return l.right
    // K-composition: S (K p) (K q) → K (p q)
    if (l.tag === "fork" && FAST_EQ(l.left, K_TREE) &&
        r.tag === "fork" && FAST_EQ(r.left, K_TREE))
      return kTree(fork(l.right, r.right))
    return sTree(l, r)
  }
  if (body.tag === "stem") {
    const inner = abstractTree(h, body.child)
    // stem(x) = fork(leaf, x), so abstract as S (K leaf) inner
    return sTree(kTree(LEAF), inner)
  }
  return kTree(body)
}

function containsTree(h: Tree, t: Tree): boolean {
  if (FAST_EQ(h, t)) return true
  if (t.tag === "leaf") return false
  if (t.tag === "stem") return containsTree(h, t.child)
  return containsTree(h, t.left) || containsTree(h, t.right)
}
```

This is structurally the same algorithm as `eliminateLams` but
operates on `Tree` nodes with `FAST_EQ` for variable identity
instead of string comparison. The same η-reduction and
K-composition optimizations apply. Hash-consing makes
`containsTree` memoizable.

`abstractTree` is also the primitive the self-hosted elaborator
would use, since the Disp version operates on trees natively.

### Performance note: compile-time reduction

The type-directed paths evaluate `applyTree(codFn, hyp)` at compile
time to compute result types. This triggers real tree-calculus
reduction (the kernel's `wait`/`fix` machinery). For most types the
cost is negligible (constant codomains return immediately), and
deeply nested dependent types are rare in practice. Not worth
optimizing now, but a potential concern for complex type-level
computations. Memoization on `applyTree` inputs (keyed by
hash-consed tree identity) could mitigate this if needed.

### Invariant: `conv = FAST_EQ`

The elaborator uses `FAST_EQ` for type comparison (e.g., in
`check(Ann)` and `check(Var)`). This is identity, not
convertibility — but hash-consing guarantees that deterministic
elaboration produces identical trees for convertible types. Two
structurally different trees cannot be convertible in this system.
If this invariant is violated, it's a bug in elaboration (non-
deterministic tree construction), not in the comparison.

## Step 5: Per-node compilation rules

### `check(expr, expected, scope) → Tree`

When `expected` is `null`, `check` delegates directly to
`compileExpr` (the existing `exprToCir → eliminateLams → cirToTree`
pipeline). This avoids reimplementing the untyped path and
guarantees zero divergence:

```typescript
function check(e: Expr, expected: Tree | null, ...): Tree {
  if (expected === null) return compileExpr(e, lookupEntry, resolveUse)
  // ... type-directed dispatch below ...
}
```

#### Binder `{x : A} -> body`

The central dispatch. Three cases based on the expected type:

**Case 1: `expected = null` (no type info)**

Compile as lambda. Identical to current `exprToCir` binder case.

```
shadow params in scope
body_cir = exprToCir(body, scope)
wrap in lam nodes, run eliminateLams, cirToTree
```

**Case 2: `expected` is a universe (`is_universe(expected) = true`)**

Binder means Pi. The domain is a type; the codomain is a type.

```
A_tree = check(A, expected, scope)       // domain is a type at this universe
id = fork(A_tree, fork(buildNat(scopeDepth), buildNat(paramIndex)))
hyp = makeHyp(A_tree, id)
scope.push(x → { tree: hyp, type: A_tree })
B_tree = check(body, expected, scope)    // codomain is a type too
scope.pop(x)
codFn = abstractTree(hyp, B_tree)        // tree-level abstraction (see §4)
return applyTree(applyTree(trusted.get("Pi")!, A_tree), codFn)
```

If param has no type annotation (`A = null`), error: "Pi domain
requires a type annotation" (no metavariables yet).

**Case 3: `expected` is a Pi (`is_pi(expected) = true`)**

Binder means lambda. The domain is known from the Pi; the codomain
guides the body.

```
D = piDomain(expected)
codFn = piCodFn(expected)
if A is annotated:
  A_tree = infer(A, scope).tree
  assert FAST_EQ(A_tree, D), "domain annotation doesn't match expected"
hyp = makeHyp(D, typeMeta(expected))    // Pi metadata as identity
scope.push(x → { tree: hyp, type: D })
result_type = applyTree(codFn, hyp)      // tree reduction at compile time (see perf note §4)
body_tree = check(body, result_type, scope)
scope.pop(x)
lambda = abstractTree(hyp, body_tree)    // tree-level abstraction (see §4)
// Defense-in-depth (debugTypeCheck only): verify the kernel agrees
assert applyTree(expected, lambda) == TT, "type check failed"
return lambda
```

**Case 4: `expected` is something else (Nat, Bool, neutral, ...)**

Fall back to infer, then verify:

```
{ tree, type } = infer(binder_expr, scope)
if type != null:
  assert FAST_EQ(type, expected) or applyTree(expected, tree) == TT
else:
  assert applyTree(expected, tree) == TT
return tree
```

#### Multi-param binders

`{x : A, y : B} -> body` desugars right-associatively during
elaboration, same as today's bracket abstraction but with type
threading:

```
check({x : A, y : B} -> body, expected):
  // Treat as check({x : A} -> {y : B} -> body, expected)
  // The outer check extracts the domain for x,
  // computes the codomain type for {y : B} -> body,
  // and the inner check extracts the domain for y.
```

#### Leaf `t`

```
check(t, expected):
  if expected == null: return LEAF
  assert applyTree(expected, LEAF) == TT
  return LEAF
```

The predicate decides whether LEAF is valid (zero for Nat, TT for
Bool, refl for Eq, etc.).

#### Num `42`

```
check(42, expected):
  tree = build_succ_chain(42, trusted.zero, trusted.succ)
  if expected != null:
    assert applyTree(expected, tree) == TT
  return tree
```

#### Var `x`

```
check(x, expected):
  entry = scope.lookup(x)
  if expected != null and entry.type != null:
    assert FAST_EQ(entry.type, expected) or applyTree(expected, entry.tree) == TT
  else if expected != null:
    assert applyTree(expected, entry.tree) == TT
  return entry.tree
```

#### App `f x`

Falls back to infer in all cases:

```
check(App(f, x), expected):
  { tree, type } = infer(App(f, x), scope)
  if expected != null:
    if type != null: assert FAST_EQ(type, expected)
    else: assert applyTree(expected, tree) == TT
  return tree
```

#### Ann `(e : T)`

Always synthesizes, then checks compatibility:

```
check(Ann(e, T), expected):
  { tree, type } = infer(Ann(e, T), scope)
  if expected != null and type != null:
    assert FAST_EQ(type, expected)
  return tree
```

#### RecValue `{ a := e1; b := e2 }`

Inline `let` members use **infer** (or check-against-annotation if
annotated), following Rust's model. The expected type flows only to
the exported fields, not to `let` bindings:

```
check(RecValue(members, fields), expected):
  // Process members (let/test/open) to build scope
  for m in members:
    if m.tag == "let":
      if m.type != null:
        type_tree = infer(m.type, scope).tree   // verify is a type
        tree = check(m.body, type_tree, scope)
        scope.push(m.name, { tree, type: type_tree })
      else:
        { tree, type } = infer(m.body, scope)
        scope.push(m.name, { tree, type })
    // test/open: handle as today

  // Compile exported fields — church-encode as today
  tree = church_encode(fields, scope)
  if expected != null:
    assert applyTree(expected, tree) == TT
  return tree
```

Future: if `expected` is a known record type, check each field
against its expected field type individually.

#### RecType `{ a : A, b : B }`

```
check(RecType(fields), expected):
  if expected != null: assert isUniverse(expected)
  // Compile to kernel record type (future work, currently throws)
  error("recType compilation not yet implemented")
```

#### Proj `e.field`

```
check(Proj(e, field), expected):
  { tree, type } = infer(Proj(e, field), scope)
  if expected != null and type != null:
    assert FAST_EQ(type, expected)
  return tree
```

#### Hole `_`

```
check(_, expected):
  error("holes not yet supported")
```

Future: create a metavariable constrained by `expected`.

### `infer(expr, scope) → { tree: Tree, type: Tree | null }`

#### Var `x`

```
infer(x):
  entry = scope.lookup(x)
  return { tree: entry.tree, type: entry.type ?? null }
```

#### App `f x` — solves issue #4

```
infer(App(f, x)):
  { tree: f_tree, type: f_type } = infer(f)

  if f_type != null and isPi(f_type):
    D = piDomain(f_type)
    codFn = piCodFn(f_type)
    x_tree = check(x, D)                  // ← argument checked against domain
    result_tree = applyTree(f_tree, x_tree)
    result_type = applyTree(codFn, x_tree) // ← tree reduction
    return { tree: result_tree, type: result_type }

  // f_type unknown or not Pi — untyped fallback
  x_tree = check(x, null)
  result_tree = applyTree(f_tree, x_tree)
  return { tree: result_tree, type: null }
```

This is the key case. The argument `x` is checked against the
domain `D`, so `Pi Nat ({x : Nat} -> Bool)` compiles the second
argument `{x : Nat} -> Bool` against `Nat -> Type` (a Pi type).
The inner binder sees a Pi expected type and compiles as lambda.
No more incorrect mode inheritance.

#### Ann `(e : T)`

```
infer(Ann(e, T)):
  // Infer T, verify it's a type (infer-then-verify strategy)
  { tree: T_tree, type: T_kind } = infer(T)
  if T_kind != null and not isUniverse(T_kind):
    error("annotation is not a type")
  // Check e against T
  e_tree = check(e, T_tree)
  return { tree: e_tree, type: T_tree }
```

#### Binder `{x : A} -> body` (no expected type)

Default to lambda. If domain is annotated, infer its type and
construct a Pi type for the result:

```
infer({x : A} -> body):
  if A != null:
    { tree: A_tree, type: A_kind } = infer(A)
    id = fork(A_tree, fork(buildNat(scopeDepth), buildNat(paramIndex)))
    hyp = makeHyp(A_tree, id)
    scope.push(x → { tree: hyp, type: A_tree })
    { tree: body_tree, type: body_type } = infer(body)
    scope.pop(x)
    lam = abstractTree(hyp, body_tree)       // tree-level abstraction (see §4)
    if body_type != null and trusted.Pi != null:
      codFn = abstractTree(hyp, body_type)   // exact by construction: body_type
                                              // contains hyp iff the type depends
                                              // on x. If not, abstractTree produces
                                              // K(body_type) — correct constant
                                              // codomain. Hash-consing guarantees
                                              // hyp is unique.
      pi = applyTree(applyTree(trusted.get("Pi")!, A_tree), codFn)
      return { tree: lam, type: pi }
    return { tree: lam, type: null }

  // No annotation — pure lambda, no type info
  compile as lambda (current behavior via compileExpr)
  return { tree: lam, type: null }
```

#### Leaf

```
infer(t):
  return { tree: LEAF, type: null }   // ambiguous — no type info
```

#### Num

```
infer(42):
  tree = build_succ_chain(42)
  return { tree, type: trusted.Nat ?? null }
```

Where `trusted.Nat` would be available if the kernel is in scope.
Not a `trust` item itself — looked up from scope after `open use`
of the kernel surface.

#### Proj `e.field`

```
infer(Proj(e, field)):
  { tree: e_tree, type: e_type } = infer(e)
  // Use compile-time field metadata for index (as today)
  proj_tree = apply_selector(e_tree, field_index)
  // Type: if e_type is a known record type, extract field type
  // For now, return null (field types from record types are future work)
  return { tree: proj_tree, type: null }
```

#### RecValue, RecType, Hole, Use

```
infer(RecValue):
  tree = church_encode(fields)
  return { tree, type: null }

infer(RecType):
  error("recType inference not yet supported")

infer(Hole):
  error("holes not yet supported")

infer(Use):
  // Already resolved by driver before reaching elaborator
  unreachable
```

## Step 6: Driver changes

### `runItem` with elaborator

```typescript
function runItem(it: Item, target: Decl[], isExport: boolean): void {
  switch (it.tag) {
    case "field":
    case "let": {
      let tree: Tree, type: Tree | null
      if (it.type != null) {
        // Typed binding: infer type annotation, verify it's a type
        const { tree: type_tree, type: type_kind } = infer(it.type, ...)
        if (type_kind != null && !isUniverse(type_kind))
          error("annotation is not a type")
        tree = check(it.body, type_tree, ...)
        type = type_tree
      } else {
        // Untyped binding: infer
        const result = infer(it.body, ...)
        tree = result.tree
        type = result.type
      }
      define(it.name, { tree, type, ... })
      if (isExport || it.tag === "field")
        target.push({ kind: "Def", name: it.name, tree, type })
      return
    }

    case "trust": {
      // Same as let, but register in trusted table
      let tree: Tree, type: Tree | null
      if (it.type != null) {
        const { tree: type_tree, type: type_kind } = infer(it.type, ...)
        if (type_kind != null && !isUniverse(type_kind))
          error("annotation is not a type")
        tree = check(it.body, type_tree, ...)
        type = type_tree
      } else {
        const result = infer(it.body, ...)
        tree = result.tree
        type = result.type
      }
      define(it.name, { tree, type, ... })
      trusted.set(it.name, tree)
      if (isExport)
        target.push({ kind: "Def", name: it.name, tree, type })
      return
    }

    case "test":
      // Unchanged: compile both sides via compileExpr (untyped), assert FAST_EQ.
      // Tests are runtime assertions on tree equality, not type-checked.
      // Typed tests (e.g. `test (e : T)`) are a possible future extension.
    case "open": {
      // Extended: carry types from opened entries
      // If it.trust is true, also register each field in trusted table
      const isTrust = it.trust === true
      for (let i = 0; i < entry.fields.length; i++) {
        define(entry.fields[i], {
          tree: entry.fieldTrees[i],
          type: entry.fieldTypes?.[i] ?? null
        })
        if (isTrust) trusted.set(entry.fields[i], entry.fieldTrees[i])
      }
    }
  }
}
```

### Backward compatibility

All existing `.disp` files work unchanged:

- No `trust` items → trusted table is empty → all `check` calls
  receive `null` expected type → elaborator falls back to untyped
  compilation (current `exprToCir` path).
- No type annotations on `let` → `infer` is called → for most
  expressions, `type` comes back as `null` → scope entries are
  untyped → everything compiles as today.
- `Ann` nodes still erase in untyped mode (when `trusted.Type` is
  not available, `check(T, null)` compiles T without checking, and
  `check(e, T_tree)` runs the predicate but this is the defense-in-
  depth assert, not a mode change).

### Activation path

Type-directed compilation activates when:

1. A file `trust`s kernel constructors (Pi, Type, etc.).
2. Bindings have type annotations.
3. `Ann` nodes appear in expressions.

These feed expected types into `check`, which inspects kernel
signatures and dispatches accordingly. The elaborator progressively
"lights up" as type information becomes available.

## Step 7: Error reporting

### New error kinds

| Error | Source |
|-------|--------|
| domain mismatch | `check(binder, Pi)`: explicit annotation disagrees with expected domain |
| type check failed | `check`: defense-in-depth assert fails |
| Pi domain requires annotation | `check(binder, universe)`: param has no type |
| cannot check binder against T | `check(binder, Nat/Bool/etc)`: binder at wrong type |
| function type expected | `infer(App)`: f_type is known but not Pi |

### Error format

Errors carry source spans (from AST nodes) and include the expected
type pretty-printed as a tree. Example:

```
error: type check failed
  at lib/example.disp:12:5
  expected: Pi(Nat, {_} -> Nat)
  got:      (tree that failed the predicate)
```

### Failing gracefully

If an assertion fails in `check`, the error is reported for that
binding but compilation continues for subsequent bindings (same
multi-error strategy as today's test failures).

## Open questions

### Record types

`RecType` compilation is not yet designed. The Church-encoded
record type `{A : Type} -> ({x : X_1, ..., x_n : X_n} -> A) -> A`
needs kernel support. Deferred.

### Metavariables

Currently, unannotated positions get `type: null`. Metavariables
would replace `null` with solvable unknowns `?A`, enabling
inference of unannotated binder domains from downstream usage.
Significant machinery (unification, constraint solving). Deferred.

### Universe level strategy

`check(T, trusted.Type(0))` hardcodes universe level 0. Three
strategies were considered:

**A) Infer-then-verify (current choice).** Infer the annotation's
type, verify it's *some* `Type n`. Simplest; works with the existing
kernel; matches the "progressive lighting up" philosophy. Downside:
no top-down universe pressure, so level mismatches are caught only
by the defense-in-depth kernel predicate, not with targeted errors.

**B) Bidirectional level propagation.** Thread `Type n` downward
through type-formation rules. Better error messages, natural
foundation for universe polymorphism. But requires a fallback to (A)
for unannotated positions, and may be over-restrictive without
cumulativity.

**C) Cumulative universes.** `Type n` is a subtype of `Type (n+1)`.
Most ergonomic but requires a subtyping relation, complicates
`conv = FAST_EQ`, and may need kernel changes.

Start with (A). The driver infers annotations rather than checking
against a specific level:

```typescript
case "field":
case "let": {
  if (it.type != null) {
    const { tree: type_tree, type: type_kind } = infer(it.type, ...)
    if (type_kind != null && !isUniverse(type_kind))
      error("annotation is not a type")
    tree = check(it.body, type_tree, ...)
    type = type_tree
  }
}
```

Move to (B) when concrete test cases need better universe error
messages. Defer (C) until there is user demand from large proof
developments.

### Neutral expected types

When the expected type is a neutral (opaque hypothesis), the
compiler can't inspect its signature. Falls back to infer, then
compares by identity. This is sound but incomplete — some valid
programs are rejected. Annotations fix the rejected cases.
Metavariables would improve this.

## Implementation order

1. **File split.** Move sections 5–6 from `parse.ts` to `compile.ts`.
   Update imports in `run.ts` and test files. Verify all tests pass.

2. **Add `trust` keyword.** Add to tokenizer keyword set, add grammar
   production (same shape as `let`), add `RecMember` variant. Also
   add `trust` as a modifier on `open` (boolean flag on `open` AST
   node). Trusted table in driver. Verify all tests pass.

3. **Extend ScopeEntry with `type`.** Add field, propagate through
   `use`/`open`. All types are `null` at this point. Verify all tests
   pass.

4. **Kernel query helpers + `abstractTree`.** Implement `isUniverse`,
   `isPi`, `isNeutral`, `piDomain`, `piCodFn`, `makeHyp` as thin
   wrappers. Implement `abstractTree(h, body)` and `containsTree(h, t)`
   as the tree-level abstraction primitive (see §4). These need kernel
   trees from trusted table — they're no-ops when trusted table is
   empty. `abstractTree` is independently testable.

5. **`check` and `infer`.** Wire `check(e, null, ...)` to delegate to
   `compileExpr` — pure mechanical replacement, zero divergence risk.
   Then implement type-directed binder dispatch (Cases 2–4) using
   `abstractTree`. Implement `infer` for App (the key case that solves
   mode propagation). Verify all existing tests still pass (everything
   goes through the `null` → `compileExpr` path).

6. **First typed test.** Create a small `.disp` file that `trust`s
   Pi/Type/Arrow, annotates a binding, and verifies that the
   elaborator compiles it correctly. This exercises the check path
   for the first time.

7. **Defense-in-depth asserts.** Add `applyTree(expected, tree) == TT`
   checks at all `check` exit points. Gate behind a `debugTypeCheck`
   option in `ParseProgramOptions`:

   ```typescript
   interface ParseProgramOptions {
     // ...existing fields...
     debugTypeCheck?: boolean
   }

   function checkResult(tree: Tree, expected: Tree | null, opts: ParseProgramOptions): Tree {
     if (opts.debugTypeCheck && expected != null) {
       const ok = applyTree(expected, tree)
       if (!FAST_EQ(ok, TT))
         throw new Error(`defense-in-depth: type check failed`)
     }
     return tree
   }
   ```

   CI/test harness runs with `debugTypeCheck: true`. Normal
   compilation skips the redundant kernel invocations. Zero overhead
   in production, full validation in tests.
