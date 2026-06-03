# Tree-Calculus Implementation Notes

Practical notes for the current implementation. `TYPE_THEORY.typ`
owns the semantic story; this file records the engineering details
that are easy to forget when editing the kernel, parser, or runtime.

## Kernel Shape

The kernel surface is two Σ-ops plus a dispatcher, all in
`lib/kernel/core.disp`:

- `hyp_reduce` — push a frame onto a neutral. It reads the stored
  type's `respond` meta-field, which returns an `Action`: `Extend
  new_stored_type` (grow the spine) or `Return value`.
- `bind_hyp` — mint a fresh hypothesis, run a body over it, then
  `occurs`-scan the result so the hypothesis cannot escape.
- `param_apply` — the dispatcher: the in-language parametric walker
  (`walk`) with reader carve-outs plus Σ routing on the two op
  signatures.

Both Σ-ops are plain `fix`-forms. There is no kernel record and no
self-proxy handler protocol — the legacy seven-primitive `recq`
kernel was retired in the two-Σ-op cutover.

`eliminator_frame` is no longer primitive: it is the library `elim`
over `hyp_reduce` + a type's `respond`. Pi, Type, Bool, Nat, Eq, Ord,
Sigma, Refinement, Intersection, Coproduct, and Record are ordinary
library wait-forms in `core.disp`.

## Library Types

A type is `wait (make_recognizer body) meta`:

- `pair_fst T` is the recognizer **signature** (constant per
  recognizer body); type-former recognition is signature comparison.
- `type_meta T = pair_snd (pair_snd T)` is a MetaShape record — a §2.6
  headered record `{ recognizer_params, functor, respond,
  behavioral_specs }` read **by name** through the cut, not by
  position.

`make_recognizer` wraps the predicate body in the universal
recognizer-side H-rule (a neutral of the stored type is accepted in
O(1)). `make_rec_recognizer` threads the full recognizer back into the
body so recursive recognizers (Nat, Ord) re-apply the H-rule at every
structural level — a neutral child of a concrete constructor (e.g.
`succ hyp`) is recognised, not triaged.

`respond` is constitutive. Inert types use `inert_respond` (every
frame → `Extend InvalidType`); inductive types use `inductive_respond`
or a gated coherence respond (`nat_respond` / `bool_respond`) that
checks the eliminator's cases inhabit the motive.

## Signatures

Wait-based values have a stable head signature:

```disp
checker_sig := {checker} -> pair_fst (wait checker t)
```

Neutral recognition reads that signature, but is deliberately fork-
guarded where it must not misfire: a partial `wait hyp_reduce …`
recipe sitting inside type metadata is a *stem* that shares the bare
signature (`pair_fst` on a stem returns its child). `is_neutral` is the
bare check; `is_hyp_fork` is the fork-guarded check used by the support
scan and the walker.

## Neutrals

`mint_hyp ty id = wait hyp_reduce (make_neutral_meta ty id)`; `Hyp` and
`StuckElim` are aliases (a hypothesis vs. a spine-extended/stuck form —
same constructor). Neutral metadata is `pair(stored_type, payload)`:

```disp
make_neutral_meta := {current_type, payload} -> t current_type payload
neutral_meta_type := {meta} -> pair_fst meta
```

Applying a neutral routes to `hyp_reduce`, which consults the stored
type's `respond`. `Extend` appends to the spine with a new stored type;
`Return` yields a value directly. Rejection extends with `InvalidType`,
which is itself inert — an absorbing dead state, so any later check on
the stored type fails deterministically.

## Walker and Native Fast Paths

`param_apply` runs the walker **in-language**; there is no native
dispatcher fast-path (the legacy native walker was removed in the
cutover, and re-introducing one would require restoring an equivalence
test). The only live native fast-path is `tree_eq`, which short-
circuits to a hash-cons identity check and returns the exact Scott
`TT`/`FF` trees — bit-identical to the in-language reference, which is
the spec.

The walker's reader carve-outs (run raw instead of being reduced) are
`ROOT_SIG` (`pair_fst`), `STORED_TYPE` (`neutral_type`), `I_canonical`
(identity), and `tree_eq` (a two-stage partial, neutral-safe). Σ routing
runs a registered kernel-op's wait-form raw. Everything else is reduced
structurally, and the two parametricity guards fire there: forging a
neutral-rooted fork (stem-forge) and triaging on a neutral both return
`Fail`. These are pinned by `lib/tests/soundness.test.disp`.

## Strictness Pitfalls

Tree calculus is strict. Use `wait` when a partial application should
remain inert, especially around fixed points.

`select_lazy` interacts badly with bracket abstraction when a branch
closes over a recursive self-reference: the closed combinator's
compile-time reduction can fire the recursion. For recursive branch
bodies, prefer `match` — the compiler wraps each arm over its free
variables, side-stepping the eager K-body evaluation. (See CLAUDE.md's
"Compiler workarounds" for the multi-line-arm and self-recursion
caveats.)

## Records and Exports

Field names and field trees are compile-time metadata used for
projection and `open`. The value-level §2.6 records the kernel uses for
metadata are `prod`/`annihilate` forms read by name through the cut
(`field` / `proj`).

Files with any `name := expr` field export only those fields. A legacy
fieldless-file mode still re-exports top-level `let`s and opened names,
used by shims such as `lib/kernel/prelude.disp`. Duplicate exported
fields are rejected by the parser.
