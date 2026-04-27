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

All conditionals in NbE tree programs use `ited`, not `ite2`.

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

## Performance wall

Tree programs compiled via bracket abstraction execute slowly on the
unoptimized tree-calculus runtime. A single `napply` call on a 3000-node
tree takes millions of reduction steps.

The lambada project's compilation pipeline (self-hosted, with
optimized combinator selection) is the reference for practical
tree-calculus program compilation. Native host implementations
(TypeScript functions mirroring the tree-program semantics) serve as
the runtime optimization per the development philosophy: "Host
implementations are optimizations."
