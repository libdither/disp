# Soundness Fix Plan: Guarded Checked Evaluation

## Goal

Close the neutral-forging hole while keeping semantic authority in the
object language/kernel, not in the TypeScript elaborator.

The intended external boundary is:

```disp
test (Type n) ParsedType = TT
test ParsedType ParsedTerm = TT
```

The elaborator should only parse terms/types into trees, insert these
checks, and know enough universe syntax to choose the requested `Type n`.
It should not maintain a trusted table of predicates.

## Current Hole

The current kernel treats any tree with the `hyp_reduce` signature as a
neutral. An adversary can construct such a tree directly:

```disp
let fake_proof : Eq Nat 0 1 =
  StuckElim (Eq Nat 0 1) t
```

or hide construction inside a function checked by `Pi`:

```disp
let bad : Nat -> Eq Nat 0 1 =
  {_} -> StuckElim (Eq Nat 0 1) t
```

The existing `Pi` checker applies the candidate function to a freshly
created hypothesis and then trusts the returned neutral's stored type.
That is sound only if the returned neutral was created by the checker,
not by the checked term.

## Core Invariant

Use a guarded checked evaluator with this invariant:

```text
At the public entry point, the checked term contains no neutral roots.
During checked evaluation, untrusted tree-calculus code may transport,
drop, duplicate, and embed existing neutrals, but it may not inspect
protected neutrals and may not construct a new neutral root.
Only certified kernel handlers may mint new neutral roots.
```

This avoids a growing `allowed_neutrals` set. Once the public entry scan
has rejected all pre-existing neutrals, every later neutral must have
come from a certified handler, provided raw evaluation is prevented from
building a fresh neutral root.

## Important Design Constraints

This design is viable, but only if these constraints are implemented
precisely.

1. The entry guard is not enough by itself.
   A term can assemble `wait kernel.hyp_reduce meta` dynamically. The
   checked evaluator must reject any raw constructor step that creates a
   neutral root.

2. K must be allowed to transport neutrals.
   Bracket abstraction compiles `{x} -> {y} -> x` to K-like code after
   the outer argument is supplied. Rejecting K returning a neutral would
   reject ordinary valid dependent functions.

3. K must not be allowed to introduce forged neutrals.
   This is handled by the entry scan plus the fresh-neutral-construction
   ban. If `K payload` returns a neutral, that neutral must already have
   existed in the checked evaluator state.

4. Exact `I` needs special handling.
   The compiler emits `I = fork(fork(t,t),t)` for `{x} -> x`. Native
   tree calculus implements this via triage, so a blanket "triage on a
   neutral fails" rule would reject identity. `checked_apply` should
   recognize exact `I` and return the argument directly as parametric
   transport.

5. Internal checks must not re-run the public closed scan.
   Public guard entry rejects closed forged neutrals. Once a handler
   mints a hypothesis, recursive internal checks must run under the
   checked-evaluator invariant, where certified neutrals are allowed to
   be present.

6. Neutral type variables must be usable as types.
   In `{A : Type 0} -> A -> A`, `A` is a certified neutral whose stored
   type is `Type 0`. Constructors such as `Pi`/`Arrow` must accept such
   certified neutral type predicates internally, even though a closed
   forged neutral type is rejected at public entry.

7. Recursors and eliminators must become certified handlers.
   Current `bool_rec`, `nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, and
   `eq_cong` are raw functions that construct `StuckElim`. Under the
   checked evaluator that construction must fail unless the eliminator is
   a certified handler that validates its motive/branches before minting
   the stuck neutral.

## Tree Predicates

Add trusted kernel helpers for recognizing neutral roots and scanning
trees. These helpers are part of the checked evaluator, so they may
inspect raw tree structure.

```disp
neutral_root raw x =
  has_sig raw.hyp_reduce x

contains_neutral raw x =
  if neutral_root raw x:
    TT
  else if is_fork x:
    or (contains_neutral raw (pair_fst x))
       (contains_neutral raw (pair_snd x))
  else if is_stem x:
    contains_neutral raw (stem_child x)
  else:
    FF

scan_no_neutral raw x =
  not (contains_neutral raw x)
```

`scan_no_neutral` rejects actual completed neutral values. It does not
need to reject the `hyp_reduce` handler capability merely appearing as a
subtree. Dynamic construction of a completed neutral is blocked by
`checked_apply`.

Implementation note: source-level `is_fork`/`pair_fst` use triage.
The checked evaluator itself can use trusted structural inspection. In a
native optimization this should be direct tree inspection; in the object
language spec it can be encoded as a certified scanner.

## Checked Apply

Add a kernel-level checked evaluator:

```disp
checked_apply : Tree -> Tree -> CheckedResult
CheckedResult = Ok Tree | Fail
```

There is no `allowed_neutrals` argument. The invariant is carried by the
fact that checked evaluation starts from a neutral-free tree and only
certified handlers can mint neutrals.

### Public Entry

Public guarded predicates enter checked evaluation through:

```disp
checked_check core v =
  if scan_no_neutral kernel v:
    checked_apply core v
  else:
    Fail
```

Internal recursive calls use `checked_apply` directly, not
`checked_check`, because they may legitimately operate on certified
hypotheses created by `Pi`, `Type`, or eliminators.

### Certified Dispatch

Before ordinary raw tree application, `checked_apply f x` should detect
certified wait-based handlers:

```disp
if has_sig kernel.guard f:        checked_guard_apply f x
if has_sig kernel.pi f:           checked_pi_apply f x
if has_sig kernel.nat f:          checked_nat_apply f x
if has_sig kernel.bool f:         checked_bool_apply f x
if has_sig kernel.eq f:           checked_eq_apply f x
if has_sig kernel.core_type f:    checked_core_type_apply f x
if has_sig kernel.guarded_type f: checked_guarded_type_apply f x
if has_sig kernel.hyp_reduce f:   checked_hyp_reduce_apply f x
if has_sig kernel.bool_rec f:     checked_bool_rec_apply f x
if has_sig kernel.nat_rec f:      checked_nat_rec_apply f x
if has_sig kernel.eq_J f:         checked_eq_J_apply f x
```

Certified handlers may inspect neutral metadata and may mint new neutral
roots. Raw user code may not.

### Raw Rules

The checked raw evaluator mirrors `src/tree.ts` with extra checks:

```text
checked_apply I x:
  Ok x

checked_apply leaf x:
  r = stem(x)
  fail if neutral_root r
  Ok r

checked_apply stem(a) x:
  r = fork(a, x)
  fail if neutral_root r
  Ok r

checked_apply fork(leaf, payload) x:
  Ok payload

checked_apply fork(stem(c), b) x:
  run the same S-rule evaluation as native apply, using checked_apply
  for every subapplication

checked_apply fork(fork(tc, td), b) x:
  raw triage case
```

For the raw triage case:

```text
if neutral_root x:
  Fail
else if x is leaf:
  Ok tc
else if x is stem(c):
  checked_apply td c
else if x is fork(l, r):
  checked_apply (checked_apply b l) r
```

The exact stack/continuation implementation should match native apply so
that evaluation order and memoization stay predictable.

### Fast Equality

`fast_eq` is an observer. It must fail if either operand contains a
neutral anywhere, not only at the root.

```text
checked_fast_eq a b:
  if contains_neutral a or contains_neutral b:
    Fail
  else:
    Ok (raw_fast_eq a b)
```

Certified handlers may still use internal equality for their own
metadata checks, H-rules, and signature dispatch. The ban is for
untrusted `fast_eq` executed by checked user code.

### Fresh Neutral Construction Check

The fresh-construction ban applies to raw constructor results, not to
all returned values. K returning an existing neutral is allowed.

```text
raw constructor step produced r:
  if neutral_root r:
    Fail
  else:
    Ok r
```

This is the point that replaces `allowed_neutrals`. A neutral returned
by K/I/S transport was already present in the checked state. A neutral
created by raw `wait kernel.hyp_reduce meta` must pass through a raw
constructor step whose result is a neutral root, so it fails.

## Guard API

Add `guard` as a kernel component.

```disp
guard core =
  wait kernel_ref.guard core

hasguard T =
  has_sig kernel_ref.guard T

unguard_public T =
  select_lazy
    ({_} -> type_meta T)
    ({_} -> InvalidType)
    (hasguard T)

unguard_checked T =
  select_lazy
    ({_} -> type_meta T)
    ({_} ->
      select_lazy
        ({_} -> T)
        ({_} -> InvalidType)
        (is_neutral T))
    (hasguard T)
```

`unguard_public` is for closed source-level boundaries. It accepts only
top-level guarded public types.

`unguard_checked` is for certified kernel handlers running under the
checked-evaluator invariant. It accepts either a guarded type or a
certified neutral type variable. A closed forged neutral cannot reach
this path because public guard entry rejects pre-existing neutrals and
raw checked evaluation cannot mint them.

The guard handler:

```disp
checked_guard_apply guard_value v =
  let core = type_meta guard_value
  checked_check core v
```

So public raw-looking application still has the desired behavior:

```disp
Nat v
```

means "scan `v` for closed forged neutrals, then checked-evaluate the
core Nat predicate on `v`."

## Type Constructors

Keep the current mutually recursive kernel predicates as core
predicates, but export guarded public constructors.

```disp
Nat =
  guard (wait kernel_ref.nat t)

Bool =
  guard (wait kernel_ref.bool t)

Eq = {A, x, y} ->
  guard
    (wait kernel_ref.eq
      (make_eq_meta (unguard_checked A) x y))

Pi = {A, B} ->
  guard
    (wait kernel_ref.pi
      (make_pi_meta
        (unguard_checked A)
        ({x} -> unguard_checked (B x))))

Arrow = {A, B} ->
  Pi A ({_} -> B)
```

When these constructors are used in closed parsed types,
`unguard_checked` behaves like `unguard_public` because there are no
certified neutral type variables yet. When used under `Type`/`Pi`
checking, it also allows the checker-created neutral type variables that
dependent types need.

## Universes

Split the universe checker conceptually into:

```disp
core_type n
guarded_type n
```

`core_type n` validates raw/core type predicates:

- core `Nat` and `Bool` live in `Type 0`;
- core `Eq A x y` lives in the same universe as `A`;
- core `Pi A B` lives in the max universe of domain/codomain according
  to the existing hierarchy rule;
- core `guarded_type m` lives in `core_type (succ m)`;
- certified neutral type variables are accepted by the H-rule.

`guarded_type n` validates public source-level types:

```disp
checked_guarded_type_apply (guarded_type n) T =
  if neutral_root T:
    checked_h_rule (guarded_type n) T
  else:
    and (hasguard T)
        (checked_apply (core_type n) (unguard_public T))
```

Public universes are guarded predicates over guarded public types:

```disp
Type n =
  guard (wait kernel_ref.guarded_type n)
```

This gives:

```disp
Type 0 Nat = TT
Type 0 (Pi Nat ({_} -> Nat)) = TT
Type 1 (Type 0) = TT
Type 0 (Type 0) = FF
```

Internal recursive universe checks must use `checked_apply`, not public
`Type n`, so they can validate codomains containing certified neutral
type variables.

## Pi Handler

Rewrite `q_pi_fn` so all adversarial applications run through
`checked_apply`.

Current unsafe shape:

```disp
hyp = q_make_hyp raw domain meta
result = v hyp
expected = codFn hyp
```

Checked shape:

```disp
checked_pi_apply pi_type v =
  if neutral_root v:
    checked_h_rule pi_type v
  else:
    let hyp = cert_make_hyp domain meta
    let result = checked_apply v hyp
    let expected_type = checked_apply codFn hyp
    if result failed or expected_type failed:
      FF
    else if neutral_root result:
      internal_fast_eq expected_type (neutral_type result)
    else:
      checked_apply expected_type result == Ok TT
```

`cert_make_hyp` is trusted kernel code. It may construct a neutral root.

## Hyp Reduce Handler

`hyp_reduce` remains the certified interpreter for applying a neutral to
an argument.

```disp
checked_hyp_reduce_apply neutral arg =
  let current_type = neutral_type neutral
  if current_type is Pi:
    let codFn = pi_cod_fn current_type
    let result_type = checked_apply codFn arg
    cert_make_neutral result_type (extend_payload neutral arg)
  else:
    cert_make_neutral InvalidType (extend_payload neutral arg)
```

This handler is trusted. It may inspect neutral metadata and may mint the
new stuck application neutral. If computing the codomain fails, return
`Fail` or mint an `InvalidType` neutral consistently with the current
kernel policy; prefer `Fail` unless existing tests require `InvalidType`.

## Nat, Bool, Eq Handlers

The concrete cases are mostly unchanged, but all recursive or dependent
checks must use checked evaluation.

- If the candidate is neutral, use the H-rule internally.
- For concrete Nat successor checks, recurse via the certified Nat
  handler.
- For Eq proof checking, `refl` still requires internal equality of lhs
  and rhs. That equality is certified handler logic, not user `fast_eq`.
- If Eq metadata contains terms that must be checked against `A`, use
  `checked_apply A x`.

## Certified Eliminators

Replace raw eliminator definitions with wait-based certified handlers.

Example public shape:

```disp
bool_rec = {motive, t_case, f_case, target} ->
  wait kernel_ref.bool_rec
    (make_bool_rec_meta motive t_case f_case target)
```

Handler obligations:

```disp
checked_bool_rec_apply rec_value _ =
  read motive, t_case, f_case, target from metadata
  checked_apply Bool target
  checked_apply (motive TT) t_case
  checked_apply (motive FF) f_case
  if target == TT: Ok t_case
  if target == FF: Ok f_case
  if neutral_root target:
    let result_type = checked_apply motive target
    checked_apply (Type rank) result_type
    cert_make_neutral result_type target
  else:
    Fail
```

`nat_rec` similarly validates:

- target checks as Nat;
- base checks against `motive 0`;
- step checks against the dependent step type;
- neutral target produces a certified stuck eliminator only after motive
  and branch validation.

`eq_J`, `eq_subst`, `eq_sym`, and `eq_cong` must validate their motives,
proofs, and branches before minting stuck neutral results.

Do not keep these as raw lambdas containing `StuckElim`; checked raw
evaluation must reject that pattern.

## Public Neutral Constructors

`Hyp` and `StuckElim` should not be part of the normal public trusted
API after this fix.

Options:

1. Remove them from public exports.
2. Keep them only under names such as `unsafe_Hyp`/`unsafe_StuckElim`
   for kernel tests, and ensure public guarded checks reject terms that
   use them.

The kernel itself should construct neutrals only through certified
helpers such as `cert_make_hyp` and `cert_make_stuck`.

## Kernel Edit Checklist

1. Add result encodings:
   `Ok`, `Fail`, `is_ok`, `ok_value`, and boolean helpers for checked
   handler results.

2. Add trusted structural helpers:
   `neutral_root`, `contains_neutral`, `scan_no_neutral`, and direct
   root constructors/checks for wait-based values.

3. Add `checked_apply` as a kernel component or mutually recursive
   helper. It must implement exact `I`, K, S, triage, fast_eq, and
   certified handler dispatch.

4. Add `guard`, `hasguard`, `unguard_public`, and `unguard_checked`.

5. Split current `type` into `core_type` and `guarded_type`, or keep
   the current field name for `core_type` and add a separate
   `guarded_type` field. Public `Type n` should be guarded
   `guarded_type n`.

6. Rewrite public constructors:
   `Nat`, `Bool`, `Eq`, `Pi`, `Arrow`, `Type`.

7. Rewrite `q_pi_fn`, `q_type_fn`, `q_hyp_reduce_fn`, `q_nat_fn`,
   `q_bool_fn`, and `q_eq_fn` to use checked application at every
   adversarial boundary.

8. Replace raw eliminators with certified wait handlers:
   `bool_rec`, `nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`.

9. Deprecate or rename public `Hyp` and `StuckElim`.

10. Update the elaborator so typed annotations are checked by public
    `Type n`, and terms are checked by public guarded predicates. The
    elaborator should no longer need predicate trust beyond loading the
    single trusted kernel `Type`.

## Required Tests

Reject forged closed neutrals:

```disp
let fake_proof : Eq Nat 0 1 =
  StuckElim (Eq Nat 0 1) t
```

Reject dynamic neutral construction hidden behind K:

```disp
let bad : Nat -> Eq Nat 0 1 =
  {_} -> StuckElim (Eq Nat 0 1) t
```

Reject direct neutral observation:

```disp
let BadFam = {n} -> select Nat (Eq Nat 0 1) (is_neutral n)
let bad : Pi Nat BadFam = {n} -> 0
```

Reject structural neutral observation:

```disp
let BadFam = {n} -> select Nat (Eq Nat 0 1) (is_fork n)
let bad : Pi Nat BadFam = {n} -> 0
```

Reject equality on neutral-containing trees:

```disp
let BadFam = {n} -> select (Eq Nat 0 1) Nat (fast_eq n 0)
let bad : Pi Nat BadFam = {n} -> 0

let BadFam2 = {n} -> select (Eq Nat 0 1) Nat (fast_eq (t n 0) (t 0 0))
let bad2 : Pi Nat BadFam2 = {n} -> 0
```

Reject unsafe eliminator motives/branches:

```disp
let bad_bool : Bool -> Eq Nat 0 1 =
  {b} -> bool_rec ({_} -> Eq Nat 0 1) 0 0 b
```

Accept ordinary transport through SKI:

```disp
let id : Nat -> Nat = {x} -> x
let const : Nat -> Bool -> Nat = {x} -> {_} -> x
let succ_fn : Nat -> Nat = {x} -> succ x
```

Accept dependent and polymorphic identity:

```disp
let dep_refl : {n : Nat} -> Eq Nat n n = {n} -> refl
let poly_id : {A : Type 0} -> A -> A = {A} -> {x} -> x
let type_id : Type 0 -> Type 0 = {A} -> A
```

Accept certified eliminators:

```disp
let bool_to_nat : Bool -> Nat =
  {b} -> bool_rec ({_} -> Nat) 1 0 b

let nat_to_bool : Nat -> Bool =
  {n} -> nat_rec ({_} -> Bool) TT ({_k, _prev} -> FF) n
```

Universe tests:

```disp
test (Type 0) Nat = TT
test (Type 0) Bool = TT
test (Type 0) (Pi Nat ({_} -> Nat)) = TT
test (Type 1) (Type 0) = TT
test (Type 0) (Type 0) = FF
test (Type 1) (Pi (Type 0) ({A} -> Arrow A A)) = TT
```

## Performance Plan

Avoid a linear `allowed_neutrals` set.

- `neutral_root` should be O(1) using hash-consed signature checks.
- `contains_neutral` should memoize per tree id.
- `scan_no_neutral` should be one public-entry scan of the candidate
  term/type tree.
- `fast_eq` only needs a deep neutral scan when user code invokes it.
- Raw constructor checks are O(1) root checks.
- Certified handlers may use internal equality and metadata reads
  directly.

The expected overhead is proportional to public-entry tree size plus
user attempts to observe neutral-containing values, not proportional to
the number of hypotheses created during checking.

## Residual Risks

The design should close the known holes if implemented as above. The
main risks are implementation mistakes:

- accidentally using raw application where `checked_apply` is required;
- treating exact `I` as ordinary triage and rejecting identity;
- letting public `guard` run closed scans during internal recursive
  checks, which breaks certified neutral type variables;
- leaving raw `StuckElim`-based recursors in place;
- allowing user `fast_eq` on trees that contain neutrals below the root;
- making `unguard_checked` available as a public escape hatch without
  the public entry scan.

These should be covered by the required tests before considering the fix
complete.
