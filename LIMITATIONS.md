# Disp — known type-system limitations

A running ledger of expressiveness and ergonomics gaps in the current
type system, separate from the soundness/forgery question. Each entry
states the problem, why it bites in practice, and what fixing it
roughly entails.

## Conversion is hash-cons equality only

`conv := {a, b} -> fast_eq a b` (lib/kernel.disp:13). The system equates
two terms iff their compiled trees are hash-cons-identical. This buys
α/β-equivalence on concrete normal forms but **nothing through neutrals
or under binders**.

Practical consequences:

- `Eq Nat (add 2 3) 5` proves by `refl` because both sides reduce to
  the same canonical numeral.
- `Eq Nat (add n 0) n` for neutral `n` does **not** prove by `refl`.
  `add` triages on its first argument; with `n` neutral, the triage
  hits `hyp_reduce` and produces a fresh neutral whose stored type
  is `Nat` but whose identity differs from `n`. `fast_eq` returns FF.
- `(λx. f x) = f` (η for functions) does not hold. Bracket abstraction
  produces different trees depending on K/η optimizations and shared
  variables.
- `pair_fst (pair x y) = x` doesn't hold definitionally for neutral
  `x` either; you need a structural rewrite.

`conv_structural` (lib/kernel.disp:354) extends this slightly with
Pi domain/codomain decomposition and universe-rank comparison, but
still does no β-reduction, no δ-unfolding, no η.

For arithmetic over concrete data this is fine. For symbolic
reasoning under binders — the bread and butter of theorem proving —
it forces every nontrivial equality to go through `nat_rec` /
`bool_rec` / `eq_J` chains with `eq_cong` at every recursion step.
Proof terms grow quadratically in data complexity.

**Fix:** make `conv` perform weak-head normalization, plus η for
functions, plus δ for trusted definitions, with caching to avoid
re-running unbounded reductions. WHNF on tree calculus is well-defined
(`apply` to fixed point); the engineering challenge is the cache.

## Inductive types are hand-rolled

Each new inductive — Bool, Nat, Eq, future Sigma/List/Vec — requires
four hand-written pieces:

1. The predicate (the type checker) plus its `is_X` recognizer.
2. The constructors (typically tagged forks).
3. The recursor with `is_neutral` guard and `StuckElim` fallback.
4. Wiring into `q_type_fn`'s `select_chain` so universe checking
   recognizes the type former.

There is no schema for declaring inductives, no positivity check, no
auto-generation of recursors. Adding mutual or nested inductives —
say `Tree A` with leaf/node constructors that recursively contain
`Tree A` — requires extending all four pieces by hand.

**Fix:** either a W-type primitive (single inductive, all others
derived) or a CIC-style schema language for inductives. W-types are
simpler in the kernel but have notoriously poor definitional equality.
A schema language lifts the kernel's complexity but stays close to
how Coq/Agda/Lean do it.

## No metavariables, no unification

The elaborator throws on `_` (src/compile.ts:150). Every implicit
argument must be made explicit:

- `poly_id Nat 4` — the user must write `Nat`.
- `bool_rec ({_} -> Nat) 1 0 b` — the user must write the motive
  every time, even when context determines it.
- `nat_rec ({_} -> Bool) TT ({n, prev} -> prev) 5` — same story for
  Nat eliminations.

Real implicit-argument inference requires higher-order unification.
Miller's pattern fragment is decidable and covers most practical
cases (inferring types from their use, motive inference from result
types). Adding pattern unification would be the single biggest
ergonomic improvement.

## No universe polymorphism

`{A : Type 0} -> A -> A` works for `A = Nat`, `A = Bool`. It does
**not** work for `A = Type 0`, because `Type 0 : Type 1`. The user
must write a separate `id1 : {A : Type 1} -> A -> A`, `id2`, etc.

A proper universe-polymorphism story (level variables, level
constraints, kernel-level constraint solving) doesn't exist yet.
Without it, generic libraries are duplicated per universe level.

## No Sigma / dependent pairs

Not implemented. Without Sigma:

- No existentials.
- No dependent records beyond the Pi-encoded ones (the Church
  encoding has limited use under dependent types).
- No iso types or HoTT-flavored constructs.

Adding Sigma is on the roadmap and is a clean exercise in the
existing kernel framework: predicate, constructors, projector
eliminators, universe-checking entry. Maybe a few hundred lines of
disp source.

## Termination is by budget, not by structure

`fix` admits arbitrary recursive definitions, including non-terminating
ones. A type check that times out is rejected; a check that completes
within budget is accepted. Soundness is therefore contingent on
budget being finite and on `apply` not silently looping past it.

A "well-typed" proof in Disp today means "well-typed within N steps."
This works because the budget is large enough for current proofs.
For larger developments, the budget either grows (slowing every check)
or starts rejecting valid proofs.

**Fix:** a structural recursion checker that proves recursive `fix`
calls decrease on a sub-structure of the input. Coq's `Fixpoint`
discipline is the standard reference. Without this, soundness is a
performance-tuned property rather than a structural one.

## The elaborator is host-side

`src/compile.ts` is ~1000 lines of TypeScript implementing
bidirectional checking, hypothesis introduction (`makeHyp`), tree-level
abstraction (`abstractTree` for codomain functions), kernel-helper
signature derivation (`makeKernelHelpers`), trust-table management,
import resolution, and let-desugaring.

Per `DEVELOPMENT_PHILOSOPHY.md`, all of this is supposed to have a
tree-program counterpart. Right now it has only a partial design
(`COMPILATION.typ`) and an empty landing zone.

The longer this gap stays open, the more host idioms accrete:
`Map<string, Tree>`, host exceptions for control flow, mutable scope
contexts, host recursion. None has obvious tree-language analogs.
Every accretion is a future port-back-to-disp obligation.

Concretely, the elaborator port involves:

- AST encoded as tagged forks.
- Trusted/scope context as a tree (the ctx-tree memory mentions a
  partial design).
- Each `case` switch reimplemented as a triage chain.
- Bracket abstraction in tree-program form.

Order of magnitude: ~10× the kernel's own port. The single largest
soundness-relevant chunk of host code that hasn't been ported.

## Performance scaling with dependent types

Each Pi check evaluates the candidate body against a hypothesis. Each
Type-n check on a Pi recurses into both domain and codomain. With
dependent types, evaluation is potentially exponential in nesting
depth — a proof of `(A : Type 0) -> (B : A -> Type 0) -> ...` requires
repeated codomain instantiations.

Hash-consing collapses identical subtrees and `FAST_EQ` is O(1), which
mitigates significantly. But common subexpressions still get
re-evaluated when reductions land on the same shape via different
paths. There is no WHNF caching, no shared-redex optimization, no
spine-folding.

For a Coq-scale development, this becomes prohibitive. For the
current scale (kernel + small examples) it's fine.

## No proof irrelevance, no propositional truncation

`Eq A x y` carries one proof, `refl`, but the kernel treats Eq proofs
as ordinary values. Two distinct paths to the same equality produce
two distinct trees that aren't equated definitionally. Proof
irrelevance — "any two proofs of the same equality are
indistinguishable" — would require either:

- A separate sort of "propositions" that the kernel collapses
  internally.
- A user-space convention plus elaborator support.

Without it, equalities-of-equalities (HoTT-style) are uniform with
ordinary types, which is either a feature or a bug depending on
your perspective. For non-HoTT proof theory, proof irrelevance
simplifies a lot.

## Ergonomics of typed elimination

Even after motive inference lands, the lambda-form of typed
eliminators is verbose:

```
let bool_to_nat : Bool -> Nat = {b} -> bool_rec ({_} -> Nat) 1 0 b
```

vs. what users actually want:

```
let bool_to_nat : Bool -> Nat = {b} -> if b then 1 else 0
```

A surface-level `if`/`match` desugaring that elaborates to typed
eliminators with motives inferred from context would be a major
ergonomics win. The kernel doesn't need to change; the parser and
elaborator need a desugaring pass.

## No definitional extension

There is no way for users to add a primitive type with its own
reduction rules. Every type-former goes through the existing
`wait checker meta` shape, and the kernel's `q_type_fn` `select_chain`
must be extended for each new type. There is no "interface" for
declaring inductives, coinductives, quotients, or modal operators.

A more principled foundation would have a meta-theoretic framework
(à la CIC or MLTT-with-modalities) where new type-formers are declared
via schemata and the kernel auto-generates checkers and recognition.
This is a substantial design project and is the sort of thing
DEVELOPMENT_PHILOSOPHY.md flags as needing tree-language form before
host implementation.
