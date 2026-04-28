# Tree-Calculus Implementation Notes

Practical idioms for implementing the type theory from
[TYPE_THEORY.typ](TYPE_THEORY.typ) as tree-calculus programs.
Reference implementation: `lib/*.disp`.

## Tag encoding

Tags are used only for neutrals: VHyp, VStuck, VStuckElim.
Tagged trees: `tagged(kind, payload) = fork(fork(TAG_ROOT, kind), payload)`.
TAG_ROOT is a fixed canonical tree distinguishable from data.

Recognition: triage twice (outer fork, inner fork), then `fast_eq` on the inner-left
against TAG_ROOT. Kind extraction: inner-right. Payload: outer-right.

Types do NOT use tags. There is no VLam tag. Types use the wait-based
encoding described below.

## wait and fix

Tree calculus is strict. The Z combinator diverges. Recursion uses `wait`:

```
wait = {a b c} -> △(△ a)(△ △ c) b
```

`wait(a)(b)` is a one-argument function. Applying to `c` gives `a(b)(c)`.
The key: `a(b)` is NOT evaluated until `c` arrives.

```
fix f = wait m ({x} -> f (wait m x))   where m = {x} -> x x
```

`fix(f)` is a function. `fix(f)(arg) = f(fix(f))(arg)`. Each call
unfolds one step, demand-driven.

## Wait-based type encoding

Types are `wait(checker)(metadata)`. Applying a type to a value runs the
checker: `apply(T, v) = checker(metadata)(v)`, which returns TT or FF.
Type checking is raw tree-calculus reduction — no tag dispatch needed.

Tree structure of `wait(checker)(fork(TAG, payload))`:

```
fork(stem(X), fork(LEAF, fork(TAG, payload)))
```

Key accessors:

- **Signature**: `pair_fst(T)` = `stem(X)` — constant for all types sharing
  the same checker function. Two types with the same signature use the same
  checking logic (e.g. all Pi types share `pi_checker`).
- **Metadata**: `pair_snd(pair_snd(T))` = `fork(TAG, payload)` — the type's
  identity. TAG distinguishes type formers (PI_TAG, UNIV_TAG, EQ_TAG);
  payload carries type-specific data (domain, codomain function, rank, etc).
- **Recognition**: `fast_eq(pair_fst(type_meta(T)), PI_TAG)` — check the
  metadata tag. No VLam inspection; works uniformly on any wait-based type.
- **H-rule**: The checker uses `fix` with `wait(self)(meta)` to reconstruct
  its own type for comparison. When a neutral value is applied,
  `ton_check(fast_eq(wait(self)(meta)))` walks the neutral's spine to verify
  it has the right type. This is the H-rule: `napply(T, hyp) = TT` iff the
  hypothesis was introduced at type T.

Example — Nat:

```
nat_checker = fix {self, meta, v} ->
    if is_neutral v then ton_check (fast_eq (wait self meta)) v
    else ... check zero/succ structure ...

Nat = wait nat_checker (fork(NAT_TAG, LEAF))
```

`Nat(Zero)` reduces to `nat_checker(metadata)(Zero)` → TT.
`Nat(hyp)` reduces to `nat_checker(metadata)(hyp)` → ton_check spine walk.

## Fix inside checker

Recursive type predicates (Nat, Type n, Pi) use `fix` for the checker
function itself. The checker is wrapped via `wait(checker)(metadata)`:

```
Nat = let checker = fix {self, meta, n} -> ...check logic...
      wait checker (fork(NAT_TAG, LEAF))
```

NOT fix-outside with a separate wrapper:

```
// WRONG: there is no mkVLam to wrap with
Nat = fix {self} -> mkVLam(LEAF, {n} -> ...uses self...)
```

`fix(f)` returns a `wait`-encoded partial application. When used as the
checker argument to `wait`, this is exactly what we want — `wait` defers
the fix-unfolding until the value argument arrives. The checker
reconstructs its own type via `wait(self)(meta)` inside the H-rule branch.

## Deferred branching (ited)

`ite2(then, else, cond)` evaluates BOTH branches eagerly. In strict
tree calculus, non-taken branches containing recursive calls diverge.

Fix: `ited(then_thunk, else_thunk, cond)` where branches are
`{_} -> value` thunks. The chosen thunk is applied to `leaf` after
dispatch. Non-chosen thunk is never forced.

```
ited = {t e c} -> fork(fork(t, K(e)), leaf) c leaf
```

All conditionals in NbE tree programs use `ited`, not `ite2`, with
one critical caveat: see *Select-then-apply* below.

## Select-then-apply (bracket-abstraction laziness fix)

`ited` defers branch evaluation at the *tree level*, but bracket
abstraction over shared free variables defeats this. When
`[x](ited thunk_A thunk_B cond)` is compiled and all three depend on
`x`, the S-combinator rule `[x](f g) = S([x]f)([x]g)` evaluates both
`[x]thunk_A` and `[x]thunk_B` *before* `ited` dispatches on `cond`.

This caused checkers to diverge: `ton_check` was evaluated even when
the value was not neutral.

**Fix:** compile each branch as a *closed function*, select via `ite2`
(both branches are constants, so eager selection is free), then apply
shared arguments *after* selection:

```
// BROKEN: bracket abstraction over x evaluates expensive_fn(x)
{x} -> ited ({_} -> expensive x) ({_} -> cheap x) (cond x)

// FIXED: branches are closed, selected before x is applied
expensive_fn = {x} -> expensive x
cheap_fn     = {x} -> cheap x
{x} -> (ite2 expensive_fn cheap_fn (cond x)) x
```

K-composition (`S(Kp)(Kq) → K(pq)`) collapses the constant branches
at compile time. The runtime evaluates only `cond(x)`, selects one
function, and applies `x` to it.

This pattern is used in `pi_checker` (avoids ton_check on non-neutrals),
`type_of_neutral` (avoids recursive spine inference on VHyp inputs),
`Nat` checker (avoids recursive calls in the Zero case), and `Type n`
checker (avoids universe-rank checks on non-universe inputs).

## Bracket abstraction optimizations

Standard `[x]c = Kc`, `[x]x = I`, `[x](f g) = S([x]f)([x]g)` produces
exponentially large trees. Essential optimizations:

- **η-reduction**: `[x](f x)` where x ∉ f → `f` (not `S(Kf)(I)`)
- **K-composition**: `S(Kp)(Kq)` → `K(pq)` (enables compile-time eval)
- **Compile-time partial evaluation**: `cirToTree` applies trees during
  compilation, producing compact normal forms

Turner's B/C combinators add per-use overhead in tree calculus (their
tree encodings are 20-30 nodes) and are a net negative for functions
with 2-3 variables. They help for 4+ variables.

## Deferred partial application

Applying a `fix`-based function to its first argument triggers the
self-application mechanism, which is expensive (thousands of reduction
steps for large bodies). Avoid module-level partial application.

Instead, use `wait` to defer: `wait(fix_fn)(first_arg)` is cheap
(~12 steps). The fix unfolding happens only when the LAST argument
arrives. Chain `wait` for multiple pre-applied arguments.

## val_apply (neutral-aware application)

`val_apply(f, x)` replaces the old napply/tag_dispatch for applying
functions to arguments in term bodies:

- If `f` is neutral → produce `mkVStuck(f, x)` (stuck application)
- Otherwise → raw `f(x)` (tree-calculus reduction)

This is used whenever a term-level function is applied to an argument
and the function might be a neutral (hypothesis variable, stuck term).
It does NOT handle type-level application — see *type_apply* below.

## type_apply (type-level application with H-rule)

`type_apply(T, v)` applies a type to a value. Two cases:

- **T is a wait-based type** (not neutral): raw `T(v)` works. The wait
  encoding triggers `checker(metadata)(v)`, which includes its own H-rule
  logic via `fix` + `ton_check`.
- **T is neutral** (abstract type variable, e.g. hypothesis `A : Type 0`):
  raw apply would produce garbage (the neutral is a tagged tree, not a
  checker). Instead, `ton_check(fast_eq(T))(v)` walks the neutral's spine
  to determine if `v` has the right type.

```
type_apply = {T, v} -> ite2 ta_neutral_fn ta_raw_fn (is_neutral T) T v
```

This arises in Pi checking: `pi_check_fn` evaluates the codomain function
on a fresh hypothesis, producing a type that might be neutral (if the
codomain is an abstract type variable). `type_apply` handles both cases.

## Typed eliminators (neutral-aware recursors)

Raw `triage` on a neutral (VHyp/VStuck/VStuckElim) misinterprets the
tagged fork structure as data — the triage fork-case destructures the
tag encoding instead of the value. This produces garbage, not a stuck
term.

**Fix:** typed eliminators guard with `is_neutral` before dispatching.
When the target is neutral, they produce `VStuckElim(motive, target)`
instead of triaging:

```
bool_rec = {motive, t_case, f_case, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ite2 t_case f_case target

nat_rec = fix {self, motive, base, step, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ... pattern match on zero/succ ...
```

The **motive** is a raw function from the scrutinee to the result type,
supplied at each elimination site. `type_of_neutral` handles
`VStuckElim` by applying the motive: `type = motive(target)`.

This mirrors CIC-style recursors (Lean's `Nat.rec`, Agda's pattern
matching): every eliminator carries a motive, stuck eliminators freeze
as neutral terms. Our eliminators are ordinary tree programs, not
built into the runtime.

Any user-defined type can follow this pattern:
1. Define the type predicate (wait-based checker with metadata)
2. Define a typed eliminator that guards on `is_neutral`
3. Store the motive in `VStuckElim` for type inference

## Pi body normalization

Pi's checker returns `fast_eq(type_apply(codFn(hyp), f(hyp)), TT)` —
anything not TT becomes FF. `type_apply` is used instead of raw apply
because `codFn(hyp)` might produce a neutral type (when the codomain is
an abstract type variable). This handles:

- Ill-typed returns against abstract hypothesis types (produces VStuck
  instead of FF → normalized to FF)
- Self-application rejection (VStuck chain → FF)

Soundness: `fast_eq(X, TT)` = TT iff X is literally TT (same hash-consed
node as LEAF). Can never manufacture a false TT. The only path to false
acceptance is unguarded triage on neutrals (Problem B), which typed
eliminators prevent.

## Binder parameter shadowing

The parser's bracket abstraction resolves scope variables to tree
literals during Expr → CIR conversion. Binder parameters must shadow
scope entries — otherwise `{n, m} -> ...m...` resolves `m` to a scope
tree instead of the lambda variable.

Fix: `exprToCir` masks binder param names from the scope lookup before
compiling the body. This is checked in `src/parse.ts` (the `binder`
case in `exprToCir`).

## Performance status

With select-then-apply and fix-inside-checker, all NbE operations
(val_apply, type_apply, ton_check, conv, Pi/Type/Nat/Bool checking)
execute within the 10M-step budget. Typical costs:

- `Nat(Succ(Succ(Zero)))`: ~300 steps
- `Pi(Nat, id, 0)(id)`: ~500 steps
- `Type_0(Nat)`: ~200 steps
- `Type_1(Type_0)`: ~300 steps
- Polymorphic identity check: ~1000 steps

Pi checking now uses `type_apply` for the codomain check, adding a
small constant overhead when the codomain is a concrete type (raw apply
path) and enabling correct handling when it is an abstract type variable
(ton_check path).

The previous performance wall (tens of millions of steps) was caused
by bracket-abstraction eagerness, not by inherent tree-calculus cost.
The select-then-apply pattern resolved it.

The lambada project's compilation pipeline (B/C combinators,
self-hosted optimization) would further reduce tree sizes and step
counts but is not required for correctness.
