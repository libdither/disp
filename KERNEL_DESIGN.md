# Tree-Calculus Implementation Notes

Practical implementation notes for the **current** kernel in
`lib/kernel.disp`. These describe what the codebase looks like today,
not the unified design target in [`TYPE_THEORY.typ`](TYPE_THEORY.typ);
the implementation has not yet migrated to that design.

`TYPE_THEORY.typ` owns the semantic story: types as predicates, the
H-rule, soundness, universes, type formers, and eliminators.
This file owns the engineering story: how the current `lib/kernel.disp`
is shaped, where `wait` is required, how strict evaluation interacts
with records, and which parser/bracket-abstraction details are
load-bearing. As the migration proceeds, sections here will be revised
or removed.

Reference implementation:

- `lib/prelude.disp` for tree-level combinators (`wait`, `fix`, `rec`,
  `recq`, branching helpers, pairs, nat helpers).
- `lib/kernel.disp` for the recursive-record kernel, neutral handler,
  type constructors, eliminators, conversion, and arithmetic.

## Kernel Shape

The kernel is a recursive record:

```disp
kernel : {hyp_reduce, guard, eq, eq_J, unguard, checked_apply,
          predicate_frame, eliminator_frame, bind_hyp} := recq {
  hyp_reduce       := q_hyp_reduce_fn;
  guard            := q_guard_fn;
  eq               := q_eq_fn;
  eq_J             := q_eq_J_fn;
  unguard          := q_unguard_fn;
  checked_apply    := q_checked_apply_fn;
  predicate_frame  := q_predicate_frame_fn;
  eliminator_frame := q_eliminator_frame_fn;
  bind_hyp         := q_bind_hyp_fn
}

kernel_ref : {hyp_reduce, guard, eq, eq_J, unguard, checked_apply,
              predicate_frame, eliminator_frame, bind_hyp} :=
  {q} -> wait kernel q
```

The `eq` and `eq_J` slots are legacy carry-overs from the pre-migration
kernel; they remain in the record while the Eq library type still uses
the older per-type handler path. The seven kernel primitives proper are
`hyp_reduce`, `guard`, `unguard`, `checked_apply`, `predicate_frame`,
`eliminator_frame`, `bind_hyp`.

`kernel` is the actual recursive record. `kernel_ref` is its lazy proxy:
projecting from `kernel_ref` returns delayed field selection, e.g.
`kernel_ref.pi = wait kernel pi_selector`.

Each component selected through `recq` receives `(ks, raw, query)`:

- `ks`: lazy self proxy, so `ks.field` is cheap and produces delayed
  self-selection.
- `raw`: actual recursive record. Use it only when handler identity
  matters.
- `query`: the selector that picked the current component. The H-rule
  uses it to reconstruct the current checker type.

The only common reason to use `raw` is neutral identity. Neutrals store
the actual `hyp_reduce` handler signature, so `Hyp`, `StuckElim`, and
`q_is_neutral` must use `kernel.hyp_reduce` / `raw.hyp_reduce`, not the
lazy proxy.

## wait and fix

Tree calculus is strict. Ordinary self-application diverges. Recursion
uses `wait`:

```disp
wait = {a, b, c} -> t (t a) (t t c) b
```

`wait(a)(b)` is inert. Applying a third argument gives:

```disp
wait(a)(b)(c) = a(b)(c)
```

The fixed-point combinator uses this delayed partial application:

```disp
m = {x} -> x x
fix = {f} -> wait m ({x} -> f (wait m x))
```

`fix(f)(arg)` unfolds one step on demand:

```disp
fix(f)(arg) = f(fix(f))(arg)
```

Use `wait` whenever an eager partial application would unfold a
recursive value before its real demand argument arrives.

## Signatures

Types and neutrals are wait-based values. For any wait-based value:

```disp
wait(checker)(metadata)
```

the first projection is the checker signature:

```disp
checker_sig checker = pair_fst (wait checker t)
has_sig checker v   = tree_eq (pair_fst v) (checker_sig checker)
```

Signatures are used for recognition:

- `is_neutral(v)` checks `has_sig kernel.hyp_reduce v`.
- `is_pi(v)` checks `has_sig kernel_ref.pi v`.
- `is_universe(v)` checks `has_sig kernel_ref.type v`.
- `is_eq(v)` checks `has_sig kernel_ref.eq v`.

There are no dedicated `sig_*` kernel fields anymore. They were an
artifact of the older selector-query kernel.

## Metadata

For a wait-based type:

```disp
type_meta = {T} -> pair_snd (pair_snd T)
```

The current metadata layouts are:

- `Nat`: `t`
- `Bool`: `t`
- `Eq A x y`: `make_eq_meta A x y`
- `Type rank`: `rank`
- `Pi domain codFn`: `make_pi_meta domain codFn`

Type-former tags are deliberately not used. Tags are forgeable tree
payloads; checker identity is not.

## Neutrals and hyp_reduce

Neutrals are wait-based values whose handler is `hyp_reduce`.
Every neutral metadata tree is an annotated payload:

```disp
make_neutral_meta current_type payload = t current_type payload
neutral_meta_type meta                 = pair_fst meta
neutral_meta_payload meta              = pair_snd meta
```

The annotation is the current type, which makes type extraction O(1):

```disp
neutral_type = {v} -> neutral_meta_type (type_meta v)
```

Construction:

```disp
Hyp ty id               = wait kernel.hyp_reduce (make_neutral_meta ty id)
StuckElim result target = wait kernel.hyp_reduce (make_neutral_meta result target)
```

The second metadata field is the identity payload. Hypotheses store an
id there. Neutral applications extend the payload as a spine:

```disp
extend_neutral_meta old_meta result_type arg =
  make_neutral_meta result_type (t old_meta arg)
```

The payload is not needed for type extraction, but it is needed for
identity: `f 0` and `f 1` must remain different neutrals even when both
have type `Nat`.

Applying a neutral triggers `hyp_reduce(meta)(arg)`. It checks whether
the neutral's current type has the Pi checker signature, and returns a
`CheckedResult`:

```disp
if has_sig ks.pi (neutral_meta_type meta) then
  result_type = codFn(arg)
  Ok (wait self (extend_neutral_meta meta result_type arg))
else
  Fail
```

If the stored type is non-Pi, applying the neutral has no semantic
meaning (you can't apply a Nat-typed value to anything), so the handler
returns `Fail` directly. There is no `InvalidType` sentinel: a deferred
failure stored in metadata buys nothing — the failure surfaces sooner
via `Fail`, with localised debugging.

The dispatcher invokes `q_hyp_reduce_fn` without wrapping (it returns
`CheckedResult` directly, like the type-checker handlers).

## H-Rule Implementation

Every checker first handles neutrals. The shared helper is:

```disp
q_h_rule_fn = {ks, raw, query, self, meta, v} ->
  tree_eq (wait (ks query) meta) (neutral_type v)
```

The caller already checked that `v` is neutral. The helper:

1. Reconstructs the current type as `wait (ks query) meta`.
2. Reads the neutral's stored type via `neutral_type(v)`.
3. Accepts exactly when those trees are hash-cons identical.

`ks query` is already delayed because `ks` is the `recq` proxy.

## Raw Apply Handles Everything

No special `val_apply` or `type_apply` is needed:

- Applying a type runs its checker.
- Applying a neutral runs `hyp_reduce`.
- Applying a plain function uses ordinary tree-calculus reduction.

For Pi checking, the candidate function is applied to a fresh neutral
hypothesis. The result is handled directly:

- If the result is neutral, compare its stored type with the expected
  codomain.
- If the result is concrete, apply the expected type predicate to it and
  normalize the result with `tree_eq(..., TT)`.

## Deferred Branching

`select(then, else, cond)` is eager in both branches. Non-selected
recursive branches can still unfold.

Use `select_lazy` when the branches are closed thunks:

```disp
select_lazy = {then_thunk, else_thunk, cond} ->
  t (t then_thunk ({_} -> else_thunk)) t cond t
```

Use select-then-apply when the branches share free variables. Bracket
abstraction over shared variables can defeat `select_lazy`, because the
compiled S-combinator form builds both branch closures before the
condition dispatches.

Broken:

```disp
{x} -> select_lazy ({_} -> expensive x) ({_} -> cheap x) (cond x)
```

Preferred:

```disp
expensive_fn = {x} -> expensive x
cheap_fn     = {x} -> cheap x
{x} -> (select expensive_fn cheap_fn (cond x)) x
```

The selected branch is chosen before the shared argument is applied.

## Bracket Abstraction

The parser lowers binders through SKI-style bracket abstraction.
The essential optimizations are:

- Eta reduction: `[x](f x)` where `x` is not free in `f` becomes `f`.
- K-composition: `S(K p)(K q)` becomes `K(p q)`.
- Compile-time partial evaluation: `cirToTree` applies closed trees
  during compilation.

Binder parameters must shadow parser scope entries. Otherwise a binder
parameter named like an existing `let` would compile to the let-bound
tree instead of a lambda variable. This masking is in `exprToCir`'s
`binder` case.

## Recursive Records

Plain `rec`:

```disp
rec = {components} -> fix ({self, sel} -> components sel self)
```

This is fine for small self-referential records:

```disp
let k : {a, b} = rec {
  a := {ks, x} -> x;
  b := {ks : {a, b}, x} -> ks.a x
}
```

For dense mutually recursive records such as the kernel, plain `rec`
passes the actual fixed point as `ks`. Then `ks.field` compiles to eager
application of the fixed point to a selector, which can cascade through
other fields at compile time.

Use `recq` for query-style recursive records:

```disp
recq = {components} ->
  fix ({self, query} -> components query ({q} -> wait self q) self query)
```

This keeps ordinary `ks.field` syntax in component bodies while
preserving delayed self-field selection.

## Eliminators (kernel-routed wait-forms)

Raw `triage` on a neutral interprets the neutral's wait-encoded
structure as data. Eliminators handle this by routing through kernel
handlers — their bodies run in raw mode where reflection is safe.

The user-facing eliminator is a wait-form:

```disp
bool_rec := wait kernel_ref.bool_rec init_meta
```

Each application is recognised by the dispatcher (signature
`kernel.bool_rec`) and routed through `q_bool_rec_fn`, which is an
arity-tracked accumulator handler:

- Initial meta: `(remaining-count, accumulated-args)`.
- Each application decrements count, appends arg.
- When count hits 1, the final arg is consumed and the body executes
  in raw mode — performing tree-shape dispatch (LEAF / fork(t,n) for
  Nat, etc.) and minting `StuckElim` for hypothesis targets.

`StuckElim result_type target` = `wait kernel.hyp_reduce
(make_neutral_meta result_type target)` — a neutral whose stored type
is the eliminator's computed result type. Used internally by the
handler bodies; user code should not mint this directly (the walker's
stem-rule constructor check rejects user-side neutral fork
construction).

The DAE library in `lib/dae.disp` provides identity-applied
eliminators (`bool_rec_dae motive ct cf target = target motive ct cf`)
that work for Scott-encoded values without kernel routing — for those,
hypothesis-application fires `hyp_reduce` naturally through the
per-value Pi template.

## Performance Notes

The current design keeps all kernel tests comfortably within the
10M-step evaluation budget. Recent `lib/kernel.test.disp` stats after
the recursive-record cleanup:

```text
tests=142, passed=142
steps=15767, calls=995, maxStack=249
```

Important performance properties:

- Neutral recognition is one signature check.
- Neutral type extraction is O(1).
- Raw apply handles types, neutrals, and functions uniformly.
- `wait` prevents module-level recursive field selection and fixpoint
  unfolding from happening too early.
- Select-then-apply avoids bracket-abstraction eagerness in shared-arg
  branching code.

## What Belongs Here

Keep semantic rules and soundness arguments in `TYPE_THEORY.typ`.
Keep implementation hazards and current tree-calculus idioms here.

Examples of things that belong here:

- A `wait` placement rule.
- Why `kernel.hyp_reduce` uses the raw record while type constructors
  use `kernel_ref`.
- How parser record metadata affects projection syntax.
- Bracket-abstraction or strictness pitfalls.

Examples of things that should live in `TYPE_THEORY.typ`:

- The meaning of Pi, Eq, Type, and the H-rule.
- The soundness argument against forged tags.
- The high-level neutral/type-checking model.
