# Tree-Calculus Implementation Notes

Practical idioms for implementing the type theory from
[TYPE_THEORY.typ](TYPE_THEORY.typ) as tree-calculus programs.
Reference implementation: `lib/*.disp`.

## Smart accum (type-tracking neutrals)

Neutrals use a **smart `accum`** handler that tracks types through
accumulation. Every neutral stores its type at a fixed position in
metadata, enabling O(1) type extraction.

```
accum = fix {self, meta, v} ->
  if pair_fst(pair_fst(meta)) has the Pi checker signature then
    result_type = codFn(v)       // Pi codomain applied to argument
    wait self (fork(result_type, fork(meta, v)))
  else
    wait self (fork(FF, fork(meta, v)))  // unknown type
```

All neutral forms share `accum` as their handler:

- **VHyp**: `wait(accum)(fork(type, fork(HYP_TAG, id)))` — type is
  stored at `pair_fst(metadata)`.
- **VStuck**: `wait(accum)(fork(result_type, fork(old_meta, arg)))` —
  smart accum computes result_type from the Pi codomain at each step.
- **VStuckElim**: `wait(accum)(fork(result_type, fork(ELIM_TAG, target)))` —
  a stuck eliminator with its result type pre-computed.

Applying a neutral to a value triggers smart accum: if the neutral's
type is Pi, the result type is `codFn(v)`; otherwise FF (unknown).
The current kernel checks Pi-ness by the Pi checker signature, not by
the forgeable `PI_TAG` metadata payload.

**Recognition**: `is_neutral = fast_eq(pair_fst(x), NEUTRAL_SIG)` — a
single O(1) check. NEUTRAL_SIG is `stem(accum)`, the constant signature
shared by all accum-based neutrals.

**ton_check** is O(1): just `check_fn(pair_fst(type_meta(v)))` —
extracts the stored type directly. No spine walking needed.

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
- **Metadata**: `pair_snd(pair_snd(T))` = `fork(TAG, payload)`. Tags describe
  payload shape; they are not trusted as public proof that a value is a real
  type former.
- **Recognition**: by checker signature for Pi/Universe/Eq, and by canonical
  identity for registered base types such as Nat and Bool. Raw metadata tags
  are forgeable, so public predicates and Type formation do not use tag-only
  recognition.
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
`Nat(hyp)` reduces to `nat_checker(metadata)(hyp)` → O(1) `ton_check`
type extraction.

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
`type_of_neutral` (avoids recursive spine walk on VHyp inputs),
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

## Raw apply handles everything

With the accum-based neutral design, there is no need for `val_apply` or
`type_apply`. Raw `apply` handles all three cases uniformly:

- **On types** (wait-based): `apply(T, v)` triggers the checker, which
  includes its own H-rule logic via `fix` + `ton_check`.
- **On neutrals** (accum-based): `apply(neutral, v)` triggers
  `accum(meta)(v)`, producing a new neutral with the argument accumulated
  into the spine.
- **On raw functions**: normal tree-calculus reduction.

For Pi body normalization, the checker branches on `is_neutral(result)`
after evaluating the codomain on a hypothesis:
- If the result is neutral → `ton_check` walks it
- If concrete → raw `apply` checks the result

This replaces the old `type_apply` indirection with a direct branch.

## Typed eliminators (neutral-aware recursors)

Raw `triage` on a neutral (VHyp/VStuck/VStuckElim) misinterprets the
accum-based wait structure as data — the triage fork-case destructures
the neutral encoding instead of the value. This produces garbage, not a
stuck term.

**Fix:** typed eliminators guard with `is_neutral` before dispatching.
When the target is neutral, they produce `VStuckElim(result_type, target)`
instead of triaging:

```
bool_rec = {motive, t_case, f_case, target} ->
    if is_neutral target then mkVStuckElim (motive target) target
    else ite2 t_case f_case target

nat_rec = fix {self, motive, base, step, target} ->
    if is_neutral target then mkVStuckElim (motive target) target
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

Pi's checker evaluates `codFn(hyp)` and branches on `is_neutral(result)`:
if neutral, `ton_check` walks the spine; if concrete, raw `apply(result, f(hyp))`
checks the body. The result is normalized to TT/FF via `fast_eq(X, TT)`.
This handles:

- Ill-typed returns against abstract hypothesis types (produces stuck
  accumulation instead of FF → normalized to FF)
- Self-application rejection (stuck chain → FF)

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

The accum-based neutral design (option C) is 37% faster than the
previous tagged design (108ms vs 212ms for the full test suite). All
NbE operations (ton_check, conv, Pi/Type/Nat/Bool checking) execute
within the 10M-step budget.

Key performance wins from eliminating tags:
- No tag construction/dispatch overhead for neutral creation and recognition
- `is_neutral` is a single `fast_eq` on the signature (O(1) via hash-consing)
- Raw `apply` on neutrals triggers `accum` directly — no val_apply indirection
- Pi codomain check branches on `is_neutral(result)` — no type_apply indirection

The previous performance wall (tens of millions of steps) was caused
by bracket-abstraction eagerness, not by inherent tree-calculus cost.
The select-then-apply pattern resolved it.

The lambada project's compilation pipeline (B/C combinators,
self-hosted optimization) would further reduce tree sizes and step
counts but is not required for correctness.
