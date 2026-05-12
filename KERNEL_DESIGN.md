# Tree-Calculus Implementation Notes

Practical notes for the current implementation. `TYPE_THEORY.typ`
owns the semantic story; this file records the engineering details
that are easy to forget when editing the kernel, parser, or runtime.

## Kernel Shape

The kernel record lives in `lib/kernel/handlers.disp` and has seven
primitive handlers:

```disp
kernel : {hyp_reduce, guard, unguard, checked_apply,
          predicate_frame, eliminator_frame, bind_hyp} := recq {
  hyp_reduce       := q_hyp_reduce_fn;
  guard            := q_guard_fn;
  unguard          := q_unguard_fn;
  checked_apply    := q_checked_apply_fn;
  predicate_frame  := q_predicate_frame_fn;
  eliminator_frame := q_eliminator_frame_fn;
  bind_hyp         := q_bind_hyp_fn
}
```

`kernel` is the actual recursive record. `kernel_ref` is the lazy
proxy `{q} -> wait kernel q`. Hypotheses use the eager
`kernel.hyp_reduce` signature; public type constructors use lazy
`kernel_ref.*` forms so their signatures match the exported native
dispatcher anchors.

## Library Types

Pi, Type, Bool, Nat, Eq, and Ord are ordinary library definitions in
`lib/types/`.

- Predicate types use `guard (predicate_frame_form meta)`.
- Eliminators use `eliminator_frame_form init_meta`.
- Function-typed hypotheses extend through `Extend`.
- Type-like predicate hypotheses can return values directly through
  `Return`.

Eq is no longer a dedicated kernel handler. It is a predicate_frame
type whose recognizer accepts `refl` exactly when the two endpoints
are structurally equal; `eq_J` is an eliminator_frame wrapper.

## Signatures

Wait-based values have a stable head signature:

```disp
checker_sig := {checker} -> pair_fst (wait checker t)
has_sig := {checker, v} -> tree_eq (pair_fst v) (checker_sig checker)
```

Neutral recognition is deliberately stricter than `has_sig`: it also
requires fork shape. Without that guard, a stem containing the
hypothesis signature would be misclassified because `pair_fst` on a
stem returns its child.

## Neutrals

`Hyp ty id` and `StuckElim result_type target` are wait-encoded
values routed through `kernel.hyp_reduce`. Neutral metadata stores the
current type and payload:

```disp
make_neutral_meta := {current_type, payload} -> t current_type payload
neutral_meta_type := {meta} -> pair_fst meta
```

Applying a neutral reads the stored type's `codomain_fn`. `Extend`
appends to the neutral spine with a new stored type; `Return` yields a
value directly. Invalid or non-applicable cases extend with
`InvalidType` so later checks fail deterministically.

## Walker and Native Fast Paths

`checked_apply` is the security perimeter. The in-language definition
has a stable unique shape, and `src/tree.ts` intercepts it to run a
native implementation of the same dispatcher/walker discipline.

Native fast paths must be bit-identical to the object-language
semantics:

- `tree_eq` short-circuits to hash-cons identity and returns the exact
  Scott `TT`/`FF` trees.
- `checked_apply` routes registered kernel signatures in raw mode and
  otherwise runs the parametric walker.
- `I_canonical` is the only walker carve-out; `I x` is accepted even
  when `x` is a hypothesis.

## Strictness Pitfalls

Tree calculus is strict. Use `wait` when a partial application should
remain inert, especially around recursive records and fixed points.

`select_lazy` can still interact badly with bracket abstraction when a
branch closes over recursive self references. For recursive branch
bodies, prefer `match`; the compiler wraps each arm over its free
variables and avoids eager K-body evaluation during `cirToTree`.

## Records and Exports

Runtime records are Church-encoded values. Field names and field trees
are compile-time metadata used for projection and `open`.

Files with any `name := expr` fields export only those fields. A
legacy fieldless-file mode still re-exports top-level lets and opened
names for compatibility shims such as `lib/kernel/prelude.disp`.
Duplicate exported fields are rejected by the parser.
