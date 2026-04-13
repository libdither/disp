# Elaboration Design

A tree-native architecture for bidirectional elaboration, normalization, conversion, and compilation-to-runtime. Supersedes the current `sexprToExpr`/`typedCompileLam`/`piCheck`-on-SKI pipeline.

**Status**: design, not yet implemented. The TypeScript elaborator's current behavior is the baseline; this document specifies the architecture we intend to move to.

**Scope**: everything from surface syntax through to the closed SKI tree handed off to the tree-calculus runtime. Focuses on the shape of the elaboration universe and its operations; does not re-derive the existing `TREE_NATIVE_TYPE_THEORY.md` predicates, which remain the basis for base-type checking.

**Companion docs**:
- `TREE_NATIVE_TYPE_THEORY.md` — the type theory this elaborator targets.
- `DEVELOPMENT_PHILOSOPHY.md` — load-bearing. Every operation specified here has a tree-calculus encoding by construction; host implementations are optimizations of these specs.
- `NATIVE_TYPE_THEORY_ISSUES.md` — the open problems this design is intended to resolve.

## Two universes

The system operates in two disjoint tree universes, connected by one directed pass.

- **Elaboration universe.** Tagged trees carrying binders, annotations, metavariables. Manipulated by `normalize`, `unify`, `check`, `infer`. Never executed by tree-calculus `apply`.
- **Runtime universe.** Closed, bracket-abstracted SKI trees. Manipulated by `apply`. The target of compilation.

The bridge is `erase : ElabTree → RuntimeTree`, run once after checking succeeds. Runtime trees never carry annotations, binders, or metavariables — they are pure tree-calculus programs.

This separation resolves the core problem in the current system: the checker operating on bracket-abstracted form has to reverse-engineer binder structure from combinators. In the new design the checker sees binders directly, and bracket abstraction happens *after* checking, as a closed-form compilation.

## Types as predicates, dually represented

`TREE_NATIVE_TYPE_THEORY.md`'s core claim — that types are tree programs that recognize values — is preserved. The refinement introduced by two universes is that types wear different clothes in each:

- **Base types** (`Bool`, `Nat`, `Tree`, and any user-defined recognizer) remain tree programs throughout. In the elaboration universe they appear wrapped as `base(P)` where `P` is the predicate tree. In the runtime universe they are just `P`.
- **Compound types** (`Pi`, eventually `Sigma`, `Eq`, etc.) have two representations: a tagged structural form during elaboration (`pi(A, body)`), and a predicate form at runtime produced by `erase` (via the existing `piPred` construction from `TREE_NATIVE_TYPE_THEORY.md`). Both describe the same acceptance set; the structural form is ergonomic for the checker, the predicate form is canonical for runtime and synthesis.

The semantic claim "a well-typed value satisfies its type's predicate form" holds in both directions: post-erase types are predicates and satisfied by the erased values; pre-erase tagged types are structurally checked via `normalize` + `fastEq`, which is provably equivalent to predicate satisfaction on the erased forms.

**Predicate evaluation during elaboration is rare and localized.** The checker's hot path — type equality, Pi-codomain instantiation, structural checking under binders, meta solving — operates on tagged forms via `normalize` and `fastEq`. Predicates are invoked only at specific boundaries:

1. **`base(P)` boundary checks** — verifying a non-literal value against a base type or refinement predicate falls back to `apply(P, erase(v))`. The same mechanism as the current `checkAgainstType`.
2. **Trust assertions / foreign references** — when an opaque tree claims a type without structural proof, the predicate is run to verify.
3. **Post-erase runtime** — any `apply(type, value)` at runtime is predicate evaluation by definition.

This is a deliberate optimization of the existing design, not a retreat from it. Predicates are arbitrary tree programs with potentially unbounded `fix`-recursion; pushing their evaluation to well-defined boundaries keeps the checker's hot path in O(1) `fastEq` and linear-in-size `normalize` territory. The predicate view reasserts itself where it is load-bearing: at runtime, and as the specification target for neural synthesis.

**For synthesis**: the predicate form is the target. A neural model searches for trees `t` satisfying `apply(erased_type, t) = true`. The tagged form of the type exists during elaboration only; synthesis operates on erased predicates. This preserves `GOALS.md`'s framing of types as the specification handed to the optimizer.

## Tag encoding

Every tree in the elaboration universe is a tagged form. A tag has the shape:

```
tagged(kind, payload)  :=  fork(fork(TAG_ROOT, kind), payload)
```

where `TAG_ROOT` is a reserved tree shape used to identify "this is an elaboration-universe tree, not a raw value," and `kind` is a small distinct tree identifying which construct this node represents.

**TAG_ROOT** is any fixed tree the elaborator commits to. A concrete choice (to be adopted):

```
TAG_ROOT := stem(stem(stem(stem(stem(leaf)))))
```

Any tree starting with `fork(fork(TAG_ROOT, _), _)` is a tagged form. Any other tree appearing in the elaboration universe is a protocol error (the elaborator never emits bare trees).

**Kinds** (distinct shapes, no encoding overlap):

| Kind | Shape | Construct |
|---|---|---|
| `KIND_VAR` | `leaf` | variable reference |
| `KIND_LAM` | `stem(leaf)` | lambda abstraction |
| `KIND_APP` | `stem(stem(leaf))` | application |
| `KIND_PI` | `stem(stem(stem(leaf)))` | Pi type |
| `KIND_ANNOT` | `fork(leaf, leaf)` | type ascription |
| `KIND_META` | `fork(leaf, stem(leaf))` | metavariable reference |
| `KIND_BASE` | `fork(leaf, stem(stem(leaf)))` | embedded runtime value |

Further constructs (`Sigma`, `Eq`, pattern-match, inductive constructors) add new kinds without disturbing existing ones.

**Forms**:

| Form | Encoding |
|---|---|
| `var(ℓ)` | `tagged(KIND_VAR, nat(ℓ))` where `nat` is the unary-stem encoding |
| `lam(A, body)` | `tagged(KIND_LAM, fork(A, body))` |
| `app(f, x)` | `tagged(KIND_APP, fork(f, x))` |
| `pi(A, body)` | `tagged(KIND_PI, fork(A, body))` |
| `annot(T, v)` | `tagged(KIND_ANNOT, fork(T, v))` |
| `meta(α)` | `tagged(KIND_META, nat(α))` |
| `base(v)` | `tagged(KIND_BASE, v)` |

**Conventions**:
- Variable ids (`ℓ`) are de Bruijn **levels**: `ℓ = 0` is the outermost binder. Levels are stable under context extension; hash-consing gives alpha-equivalence on closed forms automatically.
- `lam(A, body)` does not carry a name; the bound variable is referred to by `var(length(ctx))` inside `body`.
- `pi(A, body)` likewise: no name, body references the binder at its depth.
- `base(v)` is the only place a raw runtime tree appears inside an elaboration tree.

## Context

A context `ctx` is a snoc-list of entries:

```
ctx  :=  leaf
      |  fork(ctx_prev, entry)
```

Entries are tagged. Two kinds:

```
entry_binder(A)      :=  fork(ENTRY_BINDER, A)       -- binder: pending, type A
entry_meta(α, T, s?) :=  fork(ENTRY_META,
                           fork(nat(α), fork(T, solution)))
```

where `solution` is either `UNSOLVED` (a sentinel like `leaf`) or `fork(SOLVED, term)`.

`ENTRY_BINDER` and `ENTRY_META` are distinct tag shapes (e.g., `leaf` and `stem(leaf)`).

**Operations**:

- `empty := leaf`
- `extend(ctx, entry) := fork(ctx, entry)`
- `length(ctx)`: recursive walk, counts entries. Result is a Nat. The current level is `length(ctx)` — i.e., the level the next extension will occupy.
- `lookup(ctx, ℓ)`: walk left `length(ctx) - 1 - ℓ` times, take right. Returns the entry at level ℓ. O(depth − ℓ). Adequate for realistic contexts.

**Binder entries** do not carry values — the value of a bound variable during normalization is always the neutral `var(ℓ)` (a tagged stuck form). The entry's purpose is to carry the *type*.

**Meta entries** carry type and possibly-solution. Updating a meta is a context operation: walk to the entry, replace its solution field, rebuild the tail.

**Important**: the context is one structure, unifying what conventional elaborators split into "local context" and "metatable." Both kinds of entries live in the same list, distinguished by tag.

## normalize

The core operation. Takes an elaboration-universe term and a context; produces a normal-form elaboration-universe term.

```
normalize : ElabTree × Ctx → ElabTree
```

**Definition** (tree-program pseudocode):

```
normalize(term, ctx) :=
  dispatch on (kind of term):

    KIND_VAR(ℓ):
      -- look up in ctx
      entry = lookup(ctx, ℓ)
      case entry of:
        entry_binder(_):
          term                                 -- stays as a neutral
        entry_meta(_, _, UNSOLVED):
          term                                 -- shouldn't happen at KIND_VAR, only at KIND_META
        entry_meta(_, _, SOLVED(t)):
          normalize(t, ctx)                    -- substitute solution

    KIND_META(α):
      entry = lookup(ctx, α)
      case entry.solution of:
        UNSOLVED: term                          -- stays as a neutral
        SOLVED(t): normalize(t, ctx)

    KIND_LAM(A, body):
      A' = normalize(A, ctx)
      ctx' = extend(ctx, entry_binder(A'))
      body' = normalize(body, ctx')
      lam(A', body')

    KIND_PI(A, body):
      A' = normalize(A, ctx)
      ctx' = extend(ctx, entry_binder(A'))
      body' = normalize(body, ctx')
      pi(A', body')

    KIND_APP(f, x):
      f' = normalize(f, ctx)
      x' = normalize(x, ctx)
      normalize_app(f', x', ctx)

    KIND_ANNOT(T, v):
      T' = normalize(T, ctx)
      v' = normalize(v, ctx)
      annot(T', v')

    KIND_BASE(v):
      term                                      -- already terminal
```

```
normalize_app(f, x, ctx) :=
  if f is lam(A, body):                        -- β-redex
    body' = subst(body, length-at-f's-binding-depth, x)
    normalize(body', ctx)
  else:
    app(f, x)                                   -- stuck application

subst(term, ℓ, v) :=                            -- replace var(ℓ) with v in term
  dispatch on (kind of term):
    KIND_VAR(ℓ'):     if ℓ == ℓ' then v else term
    KIND_LAM(A, b):   lam(subst(A, ℓ, v), subst(b, ℓ, v))
    KIND_PI(A, b):    pi(subst(A, ℓ, v), subst(b, ℓ, v))
    KIND_APP(f, x):   app(subst(f, ℓ, v), subst(x, ℓ, v))
    KIND_ANNOT(T, w): annot(subst(T, ℓ, v), subst(w, ℓ, v))
    KIND_META(α):     term                      -- metas don't mention vars directly
    KIND_BASE(v):     term
```

**Properties**:

1. **Terminates** on well-typed terms in a terminating type theory. With `fix`, terminates under an evaluation budget (as already used in tree-calc `apply`).
2. **Idempotent**: `normalize(normalize(t, ctx), ctx) = normalize(t, ctx)` (up to hash-cons identity).
3. **Canonical**: if `t ≡ u` definitionally under `ctx`, then `fastEq(normalize(t, ctx), normalize(u, ctx))`.
4. **Terminal forms**: after normalization, applications occur only as stuck spines with a variable or unsolved meta at the head; β-redexes never appear.

Property 3 is the load-bearing one. It's what makes `fastEq` a complete conversion check on normal forms, eliminating the need for a separate `conv` function.

**De Bruijn caveat.** Using *levels* (not indices) means `var(ℓ)` has a stable global meaning. Substituting `v` for `var(ℓ)` does not require renaming other variables. If `v` itself contains free variables, their levels refer to whatever binders currently enclose — the caller is responsible for ensuring scopes align. In practice, `subst` is only called during β-reduction where `v` was elaborated under the same context spine that contains the binder being substituted out, so this works out.

## unify

Unification is conversion plus meta-solving. Both sides normalize first; if the resulting trees are hash-cons identical, success. Otherwise, try pattern-fragment meta solving. Otherwise, fail.

```
unify : ElabTree × ElabTree × Ctx → Ok(Ctx) | Fail

unify(t1, t2, ctx) :=
  n1 = normalize(t1, ctx)
  n2 = normalize(t2, ctx)
  if fastEq(n1, n2):
    return Ok(ctx)
  case (head-kind n1, head-kind n2):
    (META-spine α spine, _):
      try_solve(α, spine, n2, ctx)
    (_, META-spine α spine):
      try_solve(α, spine, n1, ctx)
    (PI A1 body1, PI A2 body2):
      ctx1 = unify(A1, A2, ctx)
      -- compare bodies under an extended ctx
      ctx2 = extend(ctx1, entry_binder(A1))
      unify(body1, body2, ctx2)
    (LAM A1 body1, LAM A2 body2):
      -- η-free comparison: α-normal forms must match structurally
      ctx1 = unify(A1, A2, ctx)
      ctx2 = extend(ctx1, entry_binder(A1))
      unify(body1, body2, ctx2)
    (APP-spine h1 spine1, APP-spine h2 spine2):
      if not fastEq(h1, h2): return Fail
      if length(spine1) != length(spine2): return Fail
      fold-unify over zip(spine1, spine2)
    _:
      Fail
```

**try_solve** (Miller pattern fragment):

```
try_solve(α, spine, rhs, ctx) :=
  -- spine is the args the meta was applied to
  -- pattern: all spine elements must be distinct variables
  if not all_are_distinct_vars(spine): return Fail
  -- occurs check
  if meta α appears in rhs: return Fail
  -- scope check: free vars of rhs must be a subset of spine
  if not free_vars(rhs) ⊆ vars_of(spine): return Fail
  -- solve: abstract rhs over spine
  solution = bracket_abstract_elab(spine, rhs)
  ctx' = update_meta(α, solution, ctx)
  Ok(ctx')
```

`bracket_abstract_elab` is bracket abstraction *in the elaboration universe* — it produces a tagged `lam`-form, not a SKI tree. It's a different function from the runtime-universe bracket abstraction used by `erase`. Straightforward recursive definition:

```
bracket_abstract_elab(var_level, term) :=
  -- produce lam(A, term') where term' has var(var_level) replaced by var(new_level)
  -- A is the type of the abstracted variable, retrieved from ctx
```

**`unify` never diverges** as long as `normalize` terminates. Each recursive call reduces the structure of one or both sides.

## Bidirectional check and infer

Standard bidirectional elaboration, adapted to tagged forms.

```
check : Ctx × SExpr × ElabTree → ElabTree
infer : Ctx × SExpr → (ElabTree, ElabTree)     -- (term, type)
```

**check cases**:

```
check(ctx, slam(body), pi(A, codom)):
  -- expected Pi-type: enter the lambda, check body against codomain
  ctx' = extend(ctx, entry_binder(A))
  body_type = normalize(codom, ctx')
  body' = check(ctx', body, body_type)
  lam(A, body')

check(ctx, shole, T):
  -- user wrote _: insert a fresh meta applied to the local scope
  α = fresh_meta_id()
  update_ctx(ctx, add entry_meta(α, abstracted(T, ctx), UNSOLVED))
  meta_applied_to_spine(α, ctx)

check(ctx, e, T):
  -- fallback: infer, then unify
  (e', T') = infer(ctx, e)
  ctx' = unify(T, T', ctx)    -- may solve metas in ctx
  e'
```

**infer cases**:

```
infer(ctx, svar(name)):
  -- resolve to a level, produce var(ℓ) and read its type from ctx
  ℓ = resolve_name(ctx, name)
  entry = lookup(ctx, ℓ)
  type = entry.type
  (var(ℓ), type)

infer(ctx, sapp(f, x)):
  (f', fT) = infer(ctx, f)
  fT_norm = normalize(fT, ctx)
  case fT_norm of:
    pi(A, codom):
      x' = check(ctx, x, A)
      result_type = normalize(subst(codom, current_level, x'), ctx)
      (app(f', x'), result_type)
    meta_stuck α spine:
      -- force the meta to be a Pi: allocate two fresh metas for domain and codomain
      A_meta = fresh_meta_type(ctx)
      codom_meta = fresh_meta_type(ctx ++ [A_meta])
      unify(fT, pi(A_meta, codom_meta), ctx)
      x' = check(ctx, x, A_meta)
      result_type = normalize(subst(codom_meta, current_level, x'), ctx)
      (app(f', x'), result_type)
    _:
      Error "applied a non-function"

infer(ctx, spi(x_name, A, B)):
  A' = check(ctx, A, Type)
  ctx' = extend(ctx, entry_binder(A'))
  B' = check(ctx', B, Type)
  (pi(A', B'), Type)

infer(ctx, stype):
  (base(LEAF), base(LEAF))         -- Type : Type in our setting

infer(ctx, sannot(e, T)):
  T' = check(ctx, T, Type)
  e' = check(ctx, e, T')
  (annot(T', e'), T')

infer(ctx, slam(body)):
  -- no expected type: create metas for domain and codomain, check body
  A_meta = fresh_meta_type(ctx)
  ctx' = extend(ctx, entry_binder(A_meta))
  (body', B) = infer(ctx', body)
  (lam(A_meta, body'), pi(A_meta, B))
```

**Note on `slam` with no expected type**: the domain type becomes a meta and gets solved when the lambda is applied. Kovacs-style.

## erase

Compile an elaboration-universe tree to a closed runtime-universe SKI tree.

```
erase : ElabTree → RuntimeTree
```

```
erase(term) :=
  dispatch on kind:
    KIND_VAR(ℓ):
      error "erase encountered free variable"     -- expected closed input
    KIND_META(α):
      case meta α's solution of:
        SOLVED(t): erase(t)
        UNSOLVED: error "erase encountered unsolved meta"
    KIND_LAM(A, body):
      bracket_abstract_runtime(level_of_this_binder, erase(body))
    KIND_APP(f, x):
      tree_apply_constructive(erase(f), erase(x))
    KIND_PI(A, body):
      -- type becomes a predicate tree via piPred mechanism from TREE_NATIVE_TYPE_THEORY.md
      apply(apply(piPred_tree, erase(A)), bracket_abstract_runtime(level, erase(body)))
    KIND_ANNOT(T, v):
      erase(v)                                    -- annotations drop at erase time
    KIND_BASE(v):
      v                                           -- unwrap: v is already runtime-universe
```

`bracket_abstract_runtime` is the existing current-system bracket abstraction, unchanged. It takes a level to abstract and a tree with that level as a free variable, produces an SKI tree with that variable eliminated.

`tree_apply_constructive` is the non-reducing application (current `treeApply` in `tree.ts`) — it builds the tree structure without evaluation.

**Key property**: `erase` is called after `check` succeeds. Inputs are fully-typed, all metas solved, all Pi types instantiable. Erase never encounters an unsolved meta on a well-typed program.

## Two traced examples

### Example 1: `id`

**Surface**: `let id : (A : Type) → A → A := λA. λx. x`

**Elaborated type**:
```
pi(base(LEAF),                    -- A : Type
   pi(var(0),                     -- (x : A)
      var(0)))                    -- result A
```

Levels: A is level 0; the inner pi introduces x at level 1; the result type references level 0 (A).

**Elaborated value** (after check):
```
lam(base(LEAF),                   -- λA.
    lam(var(0),                   -- λx:A.
        var(1)))                  -- x
```

A is level 0; x is level 1; the body references level 1 (x).

**check trace**:
1. `check(∅, λA.λx.x, pi(Type, pi(var(0), var(0))))`
2. Peel outer: ctx = [binder Type]; check `λx.x` against `pi(var(0), var(0))` (codomain, normalized under ctx)
3. Peel inner: ctx = [binder Type, binder var(0)]; check `x` against `var(0)` (codomain)
4. `infer(ctx, x)` → resolve name `x` → level 1 → type is entry[1].type = var(0). Returns `(var(1), var(0))`.
5. `unify(var(0), var(0), ctx)` → normalize both sides: both are var(0) (binder entries are pending, stay as neutrals). `fastEq` succeeds. ✓
6. Build up: returns `lam(Type, lam(var(0), var(1)))`.

**erase trace**:
```
erase(lam(Type, lam(var(0), var(1))))
  = bracket_abstract_runtime(0, erase(lam(var(0), var(1))))
  = bracket_abstract_runtime(0, bracket_abstract_runtime(1, erase(var(1))))
  = ... → I  (after bracket-abstraction optimizations on an identity body)
```

The runtime tree is `I` (the structural identity combinator).

### Example 2: `compose`

**Surface**: `let compose : (A B C : Type) → (B → C) → (A → B) → A → C := λA B C g f x. g (f x)`

This is the case currently blocked by `NATIVE_TYPE_THEORY_ISSUES.md`'s "nested application of parameters." In the new design, it works because the checker sees explicit binders and types, not combinators.

**Elaborated type** (after peeling 3 Type binders):
```
pi(Type, pi(Type, pi(Type,
  pi(pi(var(1), var(2)),              -- B → C
    pi(pi(var(0), var(1)),            -- A → B
      pi(var(0), var(2)))))))         -- A → C
```

Levels: A=0, B=1, C=2, g=3, f=4, x=5.

**check trace on the body**:

After peeling all 6 binders, ctx = [Type, Type, Type, pi(var(1),var(2)), pi(var(0),var(1)), var(0)], and we must check `g (f x)` against `var(2)` (= C).

1. `infer(ctx, g (f x))`:
   - `infer(ctx, g)`: resolve g → level 3 → type `pi(var(1), var(2))`. Normalized: `pi(var(1), var(2))`.
   - Applied, so expect a Pi. Domain is `var(1)`.
   - `check(ctx, f x, var(1))`:
     - `infer(ctx, f x)`:
       - `infer(ctx, f)`: resolve f → level 4 → type `pi(var(0), var(1))`.
       - Applied. Domain `var(0)`.
       - `check(ctx, x, var(0))`:
         - `infer(ctx, x)` → var(5), type var(0).
         - `unify(var(0), var(0), ctx)` → fastEq. ✓
         - Return var(5).
       - Build `app(var(4), var(5))`. Codomain `var(1)`, no dependency on the bound var. Normalize → var(1).
       - Return `(app(var(4), var(5)), var(1))`.
     - `unify(var(1), var(1), ctx)` → fastEq. ✓
     - Return `app(var(4), var(5))`.
   - Build `app(var(3), app(var(4), var(5)))`. Codomain `var(2)`. Return type var(2).
   - Return `(app(var(3), app(var(4), var(5))), var(2))`.
2. Outer `check` expects `var(2)`. `unify(var(2), var(2), ctx)` → fastEq. ✓

Checker succeeds. The expression is well-typed.

**Why the current system fails here**: after bracket-abstracting all six binders, the combinator tree has S/K/I structure whose type annotations don't fit the template `piCheck` expects — the inner K-ascriptions produced during re-abstraction claim types that don't equal the outer S-node's derived expected type via `fastEq`. The new design never bracket-abstracts during checking. It checks the explicit-binder form, where `var(3)`, `var(4)`, `var(5)` are directly visible with their declared types, and their application structure is exactly `app(var(3), app(var(4), var(5)))`.

**erase** then bracket-abstracts all six binders, producing a closed SKI tree (the actual `compose` combinator). This tree never participates in type-checking, only in runtime evaluation.

## What this design replaces

Relative to current `src/elaborate.ts` and `types.disp`:

| Current | Replaced by |
|---|---|
| `sexprToExpr` threading types as metadata | bidirectional `check`/`infer` producing tagged trees |
| `typedCompileLam` with Pi-peeling + OPAQUE fallback | `check` with expected types flowing through nested lambdas naturally |
| `piCheck` reconstructing binder structure from SKI | `normalize` + `fastEq` on explicit-binder forms |
| `sOrAsc` disambiguating ascription / S-node / AppAnn by shape | single tag dispatch in `normalize` |
| `sCodomain` combinator simulating codomain application | `subst` substituting a value for a variable in a body |
| `kCheck` constructor-class representative enumeration | direct `check` of the body under an extended ctx with the binder as pending |
| OPAQUE sentinel for unknown types | metavariables with identity |
| Allowlist as a trust escape hatch | metas solved by pattern unification; trust mode stays for bootstrap axioms only |
| `piCheck` growing with each new wire format | one `normalize` clause per new kind tag |

The existing `types.disp` base predicates (Bool, Nat, Tree, triage, ite, fix, natElim, boolElim) are preserved unchanged. The existing `apply` runtime is unchanged. The existing hash-consing invariant is preserved in both universes.

## Implementation plan

Following DEVELOPMENT_PHILOSOPHY's "tree-program is the spec, host is an optimization" rule:

1. **Write `normalize` as a tree program** in `types.disp` (or a new `elab.disp`). Small, recursive, one clause per kind. Target: testable on hand-built elaboration trees.
2. **Write the TypeScript mirror** of `normalize` in `src/elaborate.ts`. Same algorithm, faster via host recursion. Used for the actual elaborator. Cross-validate against the tree-program version on every example.
3. **Write `unify`** as a tree program + TS mirror. Calls `normalize` and `fastEq`.
4. **Write bidirectional `check`/`infer`** in TypeScript, consuming `SExpr` and producing tagged trees. The semantics are tree-program-portable but the initial implementation is host-side for iteration speed. The tree-program port is a later milestone.
5. **Write `erase`** in TypeScript — uses existing bracket abstraction.
6. **Port the test suite**: each currently-failing case in `NATIVE_TYPE_THEORY_ISSUES.md` becomes a test for the new pipeline.
7. **Add synthesis hook**: expose the elaborated-tree grammar as a sampling target for neural search. Every partial elaborated tree with metas is a well-formed synthesis state.

Steps 1 and 3 are *preconditions*, not afterthoughts. They are what makes this design tree-native.

## Open questions

1. **η-equality**. Current spec does α- and β- but not η (`λx. f x ≡ f`). If we want η, either bake it into `normalize` (η-expand functions on the way out) or into `unify` (when one side is a lam and the other isn't, η-expand before comparing). Type-directed, so requires propagating expected types through normalize. Deferred until concrete examples require it.
2. **Definitional unfolding (δ)**. Top-level `let`-definitions: when do they unfold during `normalize`? Options: always unfold (simple, can explode); never unfold (fast, can fail conversions); heuristic unfold (e.g., unfold head-only). Conventional choice: unfold on demand during unify, keep folded for display.
3. **Irrelevance and universes**. We're Type-in-Type; no universe hierarchy. If/when we add one, `pi(A, body)` gains a universe-level field.
4. **Inductive types**. Not yet addressed. Each inductive type adds a kind (constructor) and changes `normalize`'s dispatch (ι-reduction on recursors).
5. **Tag shape collision audit**. The specific `TAG_ROOT` and kind shapes chosen must be audited against base-type value spaces. `Tree` accepts everything, so strictly speaking no tag shape is "impossible" as a base value — but since `base(v)` wraps base values, the grammar prevents confusion within elaboration-universe trees.
6. **Bind-trees as an alternative to de Bruijn levels**. `dither-spec/disp/bind-trees.md` describes a tree-based binding scheme that avoids integer encodings. This design uses Nat-encoded levels for directness; bind-trees is a plausible future substitution if their sharing properties prove advantageous.

## Success criteria

This design is on track if:

- Every currently-failing case in `NATIVE_TYPE_THEORY_ISSUES.md` passes with the new pipeline.
- `types.disp` can be re-expressed (or left as-is) without the `sOrAsc`/`sCodomain`/`kCheck` combinator complexity, because `normalize` + `fastEq` handles what those were simulating.
- Neural synthesis can be pointed at the elaborated-tree grammar as a sampling target, with every sample structurally well-typed by construction.
- The tree-program `normalize` and `unify` specs run (slowly) on the test suite, matching the TypeScript implementation bit-for-bit.
- The kernel trust boundary shrinks: the allowlist mechanism is needed only for bootstrap axioms and `fix`-produced recursive definitions, not for every non-trivial typed expression.

This design is off track if:

- `normalize` grows additional cross-cutting clauses as new features are added. One clause per kind should be the invariant.
- Host-language features appear in `check`/`infer` semantics that have no tree encoding. Exceptions for control flow, mutation, and host closures are red flags per `DEVELOPMENT_PHILOSOPHY.md`.
- Metas proliferate uncontrollably (should be bounded by the number of unresolved holes + implicit-argument insertions).
- The elaboration-universe tree grammar requires ad-hoc disambiguation of shapes (cautionary tale: `sOrAsc`).
