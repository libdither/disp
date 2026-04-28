# Tree-Calculus Implementation Notes

Practical idioms for implementing the type theory from
[TYPE_THEORY.typ](TYPE_THEORY.typ) as tree-calculus programs.
Reference implementation: `test/nbe_tree.test.ts`.

## Tag encoding

Vals are tagged trees: `tagged(kind, payload) = fork(fork(TAG_ROOT, kind), payload)`.
TAG_ROOT is a fixed canonical tree distinguishable from data.

Recognition: triage twice (outer fork, inner fork), then `fast_eq` on the inner-left
against TAG_ROOT. Kind extraction: inner-right. Payload: outer-right.

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

This caused napply to diverge: `type_of_neutral(x)` was evaluated
even when `x` was not neutral.

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

This pattern is used in `napply_core` (avoids calling
`type_of_neutral` on non-neutrals), `type_of_neutral` (avoids
recursive spine inference on VHyp inputs), `Nat` (avoids recursive
`napply` calls in the Zero case), and `Type n` (avoids universe-rank
checks on non-universe inputs).

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

## Fix outside VLam

Recursive predicates (Nat, Type n) use `fix` for the body function
and wrap the result in `mkVLam` externally:

```
Nat = let body = fix {self, n} -> ...check logic...
      mkVLam LEAF body
```

NOT inside fix:

```
// WRONG: fix produces wait-encoded tree, not a VLam
Nat = fix {self} -> mkVLam(LEAF, {n} -> ...uses self...)
```

`fix(f)` returns a `wait`-encoded partial application, not a
fully-evaluated VLam. Tag dispatch (`is_vlam`, `vlam_body`) would
not recognize it. The fix-outside pattern ensures the result *is* a
tagged VLam.

## Performance status

With select-then-apply and fix-outside-VLam, all NbE operations
(napply, type_of_neutral, conv, Pi/Type/Nat/Bool checking) execute
within the 10M-step budget. Typical costs:

- `napply(Nat, Succ(Succ(Zero)))`: ~300 steps
- `napply(Nat->Nat, id)`: ~500 steps
- `napply(Type 0, Nat)`: ~200 steps
- `napply(Type 1, Type 0)`: ~300 steps
- Polymorphic identity check: ~1000 steps

The previous performance wall (tens of millions of steps) was caused
by bracket-abstraction eagerness, not by inherent tree-calculus cost.
The select-then-apply pattern resolved it.

The lambada project's compilation pipeline (B/C combinators,
self-hosted optimization) would further reduce tree sizes and step
counts but is not required for correctness.
