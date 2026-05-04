# Soundness Fix Plan: Guarded Public Types

## Goal

Fix the public neutral-forging hole without giving the TypeScript
elaborator semantic authority.

The intended boundary is:

```disp
test (Type n) ParsedType = TT
test ParsedType ParsedTerm = TT
```

`Type n` should reject unguarded/core predicates in source-level type
positions. A public type should be a canonical guard around a core type
predicate. The guard scans candidate terms before delegating to the core
predicate, so forged neutrals cannot reach the H-rule.

## Core Idea

Split every type into two layers conceptually:

```disp
kernel_ref.nat ...           // core predicate
Nat                           // public predicate = guard core

kernel_ref.pi ...            // core Pi predicate
Pi                            // public Pi constructor
```

The core layer should be the existing mutually-recursive kernel layer.
In other words, the current `q_nat_fn`, `q_bool_fn`, `q_pi_fn`,
`q_eq_fn`, `q_hyp_reduce_fn`, and universe checker remain tied together
inside one `kernel` fixed point. The guarded public constructors can be
ordinary exported definitions built from those kernel components:

```disp
Nat = guard (wait kernel_ref.nat t)

Pi = {A, B} ->
  guard
    (wait kernel_ref.pi
      (make_pi_meta
        (unguard A)
        ({x} -> unguard (B x))))
```

So `CoreNat`/`CoreBool` do not need exported names. The core predicates
are the current kernel-built trees, available internally as inline
`wait kernel_ref.* ...` expressions or by `unguard`ing a public type.
The new work is the guard component, scanner, public constructors, and a
public guarded universe.

The public constructor guards the result and unguards inputs before
composition:

```disp
Nat = guard (wait kernel_ref.nat t)
Bool = guard (wait kernel_ref.bool t)

Eq = {A, x, y} ->
  guard (wait kernel_ref.eq
    (make_eq_meta (unguard A) x y))

Pi = {A, B} ->
  guard (wait kernel_ref.pi
    (make_pi_meta
      (unguard A)
      ({x} -> unguard (B x))))
```

Core predicates keep the current neutral-aware behavior. They still use
the H-rule and still accept legitimate hypotheses created internally by
the core Pi checker.

Public guarded predicates reject adversarial closed values before core
checking:

```disp
guard core =
  wait kernel_ref.guard core

q_guard_fn core v =
  select_lazy
    ({_} -> core v)
    ({_} -> FF)
    (scan_closed v)
```

## Guard API

Add a `guard` component to the kernel fixed point. If universes are
split as described below, the final record shape is closer to:

```disp
kernel : {
  hyp_reduce,
  pi,
  nat,
  bool,
  eq,
  core_type,
  guarded_type,
  guard
}
```

Public helpers:

```disp
guard : (Tree -> Bool) -> (Tree -> Bool)
guard core = wait kernel_ref.guard core

hasguard : Tree -> Bool
hasguard T = has_sig kernel_ref.guard T

unguard : Tree -> Tree
unguard T =
  select_lazy
    ({_} -> type_meta T)
    ({_} -> InvalidType)
    (hasguard T)
```

Use the strict `unguard`: unguarded source predicates become
`InvalidType`, not themselves. This is what makes:

```disp
let RawNatAlias = unguard Nat
let x : RawNatAlias = forged_neutral
```

fail once annotations are checked by public `Type n`.

Useful laws:

```disp
hasguard (guard T) = TT
unguard (guard T) = T
guard (unguard (guard T)) = guard T
guard (guard T) = guard T
```

The last law holds by defining `guard` to return its input unchanged
when `hasguard` is already true, or by accepting structural equality
only up to a `guard` normalization helper.

## Recursive Scanner

The scanner should be a raw-tree traversal with two modes:

```disp
scan_closed x = scan_core empty_allowed x
scan_allowed allowed x = scan_core allowed x
```

`scan_closed` is what public guarded predicates use for ordinary source
terms. `scan_allowed` is used internally when validating type
expressions under binders, where a freshly-created hypothesis is
legitimate.

Pseudocode:

```disp
scan_core allowed x =
  if known_good x:
    TT
  else if is_neutral x:
    neutral_allowed allowed x
  else if is_neutral_capability x:
    FF
  else if is_fork x:
    and (scan_core allowed (pair_fst x))
        (scan_core allowed (pair_snd x))
  else:
    TT
```

Important ordering:

1. Check `known_good` first, so certified kernel recursors can contain
   internal neutral constructors without being rejected.
2. Check actual neutral values next.
3. Check forbidden neutral-constructor capabilities before descending.
4. Recurse through ordinary tree structure.

`neutral_allowed` should not trust `neutral_type`. That is the forged
field. Initially it can be identity-based:

```disp
neutral_allowed allowed x =
  allowed_contains allowed (neutral_payload_identity x)
```

For a base public guard, `allowed = empty`, so every neutral value is
rejected.

For internal type validation under `Pi`, extend the allowlist with the
fresh hypothesis identity:

```disp
h = Hyp (unguard A) pi_meta
allowed' = allow allowed h
```

## Neutral Capability Detection

Detecting neutral values alone is not enough. A checked function can
contain code that constructs a forged neutral later:

```disp
{_} -> StuckElim (unguard (Eq Nat 0 1)) t
```

At scan time this function is not itself a neutral. If the scanner only
rejects already-constructed `wait(hyp_reduce)(meta)` values, the
function passes the public guard, then the core Pi checker applies it to a
hypothesis and receives the forged neutral.

The minimal capability to reject is not a long blacklist. It is the
canonical neutral handler/capability:

```disp
kernel.hyp_reduce
```

More precisely, `scan_core` rejects:

1. any actual neutral value whose signature is `hyp_reduce`;
2. any ordinary subtree that is the exact `hyp_reduce` handler, unless
   that occurrence is inside a `known_good` certified subtree.

This catches `Hyp` and `StuckElim` because their compiled trees contain
the `hyp_reduce` handler. Metadata helpers such as `make_neutral_meta`
are not dangerous by themselves; they only become a neutral-construction
capability when paired with `hyp_reduce`.

The cleanest long-term API is still to stop exporting raw neutral
constructors and raw kernel internals. The scanner is the raw-tree
backstop: even if a user copies the exact handler tree into a term, the
public guard rejects it unless it appears inside a certified known-good
constant.

## Known-Good Subtree Skips

A recursive scan of an ordinary function value will inspect the
function's code tree. Trusted recursors such as `bool_rec`, `nat_rec`,
`eq_J`, `eq_subst`, `eq_sym`, and `eq_cong` contain `StuckElim`
internally, so a naive scan would reject every function that uses them.

Use a kernel-maintained known-good registry:

```disp
known_good x =
  fast_eq x bool_rec ||
  fast_eq x nat_rec ||
  fast_eq x eq_J ||
  fast_eq x eq_subst ||
  fast_eq x eq_sym ||
  fast_eq x eq_cong ||
  ...
```

This is a content-addressed whitelist: only exact canonical subtrees are
skipped. User code that directly calls or reconstructs `StuckElim` is
not skipped.

However, known-good must not mean "blindly safe." Each known-good
constant needs a contract:

1. It may construct neutrals only when given an allowed neutral
   scrutinee/proof.
2. It must compute the stored neutral result type from its typed
   eliminator rule, not from an arbitrary user assertion.
3. It must validate user-supplied motives and branches before returning
   a stuck neutral.

This turns the skip into a small certified-code mechanism, not a general
trust escape hatch.

## Recursors Must Become Typed

There is a second hole related to neutral-aware eliminators. Today this
program is accepted:

```disp
let bogus : Bool -> Eq Nat 0 1 =
  {b} -> bool_rec ({_} -> Eq Nat 0 1) 0 0 b
```

When checked under `b : Bool`, `bool_rec` sees a neutral target and
returns `StuckElim (Eq Nat 0 1) b`. The enclosing `Pi` checker sees a
neutral with the expected stored type and accepts, without checking the
concrete `TT`/`FF` branches.

The guard scanner alone does not fix this if `bool_rec` is placed in
`known_good`. The recursor itself must enforce its typing rule.

Plan for typed `bool_rec`:

```disp
bool_rec = {rank, motive, t_case, f_case, target} -> {
  let T = motive TT
  let F = motive FF

  if and (Type rank T)
     (and (T t_case)
     (and (Type rank F)
          (F f_case)))
  then
    select_lazy
      ({_} -> StuckElim (unguard (motive target)) target)
      ({_} -> select t_case f_case target)
      (is_neutral target)
  else
    InvalidValue
}
```

Plan for typed `nat_rec`:

```disp
nat_rec = {rank, motive, base, step, target} -> {
  let StepType =
    Pi Nat ({n} ->
      Pi (motive n) ({_prev} ->
        motive (succ n)))

  if and (Type rank (motive 0))
     (and ((motive 0) base)
          (StepType step))
  then
    ... current neutral-aware nat recursion ...
  else
    InvalidValue
}
```

The concrete API may use a different rank-passing shape, but the
recursor must have enough universe information to validate its motive
outputs.

If validation fails, return a value that the expected type rejects,
preferably a neutral carrying `InvalidType` or a named invalid sentinel.

Once recursors self-check their branch contracts, adding their exact
trees to `known_good` is sound: scanner skip avoids inspecting their
internal `StuckElim`, while the recursor runtime prevents forged
stuck-results from being used as proof of impossible branch behavior.

## Dependent Type Functions

Dependent codomains compose guarded public types with core checking:

```disp
Pi A B =
  guard (wait kernel_ref.pi
    (make_pi_meta
      (unguard A)
      ({x} -> unguard (B x))))
```

Here `B` is expected to map an inhabitant of `A` to a guarded public
type.

Type validation must be context-sensitive. For a dependent Pi:

```disp
Type n (Pi A B)
```

should:

1. Check `A` as a guarded public type.
2. Create a legitimate internal hypothesis `h : unguard A`.
3. Extend the scanner allowlist with `h`.
4. Check `B h` as a guarded public type under that allowlist.
5. Store `unguard (B h)` as the core codomain result.

Pseudocode:

```disp
GuardedTypeWith allowed n (Pi A B) =
  and (GuardedTypeWith allowed n A)
    ({ let h = Hyp (unguard A) pi_meta;
       GuardedTypeWith (allow allowed h) n (B h) })
```

This is not a new hole by itself. It becomes a hole only if codomain
functions can branch on `h` using raw tree destructors or unsafe
recursors and return a type that is valid for the neutral case but wrong
for concrete cases.

The elegant rule is:

```text
Dependent type functions may branch on typed values only through
certified typed eliminators.
```

That means:

- raw `triage` over a variable from the typing context is not part of
  the safe dependent type language;
- certified recursors are known-good scanner skips;
- certified recursors validate motives and branches before producing
  stuck eliminators.

If the long-term elaborator really only parses terms/types into trees
and invokes `Type`, this discipline must live in the object language:
either in typed recursors as above, or in an explicit elaborated AST
whose constructors encode eliminator typing rules.

## Universe Structure

Keep two universe notions during migration:

```disp
kernel_ref.core_type n
  // current Type n behavior over unguarded/core predicates

GuardedTypeWith allowed n
  // public/source-facing validator for guarded predicates
```

Implementation-wise, this likely wants two universe-related kernel
components:

```disp
kernel : {
  hyp_reduce,
  pi,
  nat,
  bool,
  eq,
  core_type,
  guarded_type,
  guard
}

Type n = guard (wait kernel_ref.guarded_type n)
```

`core_type` is the current universe checker generalized over core
predicates. `guarded_type` is the public/source-facing checker: it
requires a top-level guard, unwraps with `unguard`, and delegates to
`core_type`.

Final public `Type` should be guarded:

```disp
Type n = guard (wait kernel_ref.guarded_type n)
```

Then:

```disp
hasguard (Type n) = TT
(Type n) T = TT
```

means `T` is a guarded public type of level `n`.

Universe containment remains strict:

```disp
Type m : Type n    iff    m < n
```

Operationally, `GuardedTypeWith allowed n T` should:

```disp
GuardedTypeWith allowed n T =
  and (scan_core allowed T)
    (and (hasguard T)
         (core_type_with allowed n (unguard T)))
```

`core_type_with` is the current universe checker generalized with an
allowlist for legitimate internal hypotheses in type metadata.

For universe hierarchy, `core_type` must recognize the core of public
universes:

```disp
core_type_with allowed n (wait kernel_ref.guarded_type m) =
  nat_lt m n
```

Then:

```disp
unguard (Type m) = wait kernel_ref.guarded_type m
(Type n) (Type m) = TT iff m < n
```

This keeps the public universe guarded while still giving the core
universe checker a canonical object to recognize.

Recognition examples:

```disp
GuardedTypeWith allowed 0 Nat = TT
GuardedTypeWith allowed 0 (unguard Nat) = FF

GuardedTypeWith allowed 0 (Eq Nat 0 0) = TT
GuardedTypeWith allowed 0 (Eq (unguard Nat) 0 0) = FF

GuardedTypeWith allowed 1 (Type 0) = TT
GuardedTypeWith allowed 0 (Type 0) = FF
```

## Kernel Edit Plan

1. Extend the kernel record with `guard` and probably split `type` into
   `core_type` and `guarded_type`.
2. Keep the existing kernel fields as the anonymous/core layer. Do not
   export `CoreNat`, `CoreBool`, etc. unless temporary debug aliases are
   useful during migration.
3. Add `guard`, `hasguard`, `unguard`.
4. Add `scan_core`, `scan_closed`, and known-good registry helpers.
5. Redefine public `Nat`, `Bool`, `Eq`, `Pi`, and `Arrow` as guarded
   wrappers around core constructors.
6. Introduce `GuardedTypeWith` and transitional `GuardedType`.
7. Redefine public `Type` as a guarded universe once tests pass.
8. Replace neutral-aware recursors with typed/certified versions, or add
   typed versions first and migrate library code.
9. Remove direct public use of `Hyp` and `StuckElim` from normal APIs.
   Keep test-only or internal names only if scanner rejects their use in
   checked source terms.

## Test Plan

### Guard Shape

```disp
test hasguard Nat = TT
test hasguard Bool = TT
test hasguard (Eq Nat 0 0) = TT
test hasguard (Pi Nat ({_} -> Nat)) = TT

test hasguard (unguard Nat) = FF
test hasguard (unguard Bool) = FF

test unguard (unguard Nat) = InvalidType
```

### Public Type Validation

```disp
test (Type 0) Nat = TT
test (Type 0) Bool = TT
test (Type 0) (Eq Nat 0 0) = TT
test (Type 0) (Pi Nat ({_} -> Nat)) = TT

test (Type 0) (unguard Nat) = FF
test (Type 0) (Eq (unguard Nat) 0 0) = FF
test (Type 1) (Type 0) = TT
test (Type 0) (Type 0) = FF
```

### Forged Neutrals

```disp
let forged_nat = Hyp (unguard Nat) 0
test (unguard Nat) forged_nat = TT  // internal H-rule still works
test Nat forged_nat = FF            // public guard rejects

let forged_eq = StuckElim (unguard (Eq Nat 0 1)) t
test (unguard (Eq Nat 0 1)) forged_eq = TT
test (Eq Nat 0 1) forged_eq = FF
```

Embedded forged neutrals should also be rejected:

```disp
let pair_with_forged = t forged_nat 0
test (SomePairType Nat Nat) pair_with_forged = FF
```

Use an existing or small test pair predicate once pair/record types are
ready.

### Pi Still Works

```disp
test (Pi Nat ({_} -> Nat)) ({x} -> x) = TT
test (Pi Nat ({_} -> Bool)) ({x} -> x) = FF

test (Pi (Pi Nat ({_} -> Nat)) ({_} -> Nat))
  ({f} -> f 0) = TT
```

### Dependent Types

```disp
test (Type 0) (Pi Nat ({n} -> Eq Nat n n)) = TT

test (Pi Nat ({n} -> Eq Nat n n))
  ({n} -> refl) = TT

test (Pi (Type 0) ({A} -> Pi A ({_} -> A)))
  ({A} -> {x} -> x) = TT
```

Codomain functions containing non-allowed forged neutrals must fail:

```disp
let bad_cod = {n} -> Eq Nat (StuckElim (unguard Nat) t) n
test (Type 0) (Pi Nat bad_cod) = FF
```

### Recursor Branch Soundness

This currently accepts and must reject after typed recursors:

```disp
test (Pi Bool ({_} -> Eq Nat 0 1))
  ({b} -> bool_rec 0 ({_} -> Eq Nat 0 1) 0 0 b) = FF
```

Valid recursor use must still pass:

```disp
test (Pi Bool ({_} -> Nat))
  ({b} -> bool_rec 0 ({_} -> Nat) 1 0 b) = TT

test (Pi Nat ({_} -> Bool))
  ({n} -> nat_rec 0 ({_} -> Bool) TT ({_n, _prev} -> FF) n) = TT
```

### Elaborator-Level

Add or unskip tests equivalent to:

```disp
let fake_proof : Eq Nat 0 1 =
  StuckElim (unguard (Eq Nat 0 1)) t
```

Expected: reject.

Still accept:

```disp
let id : Nat -> Nat = {x} -> x
let dep_refl : {n : Nat} -> Eq Nat n n = {n} -> refl
let poly_id : {A : Type 0} -> A -> A = {A} -> {x} -> x
```

## Migration Strategy

1. Add guard helpers while preserving old public names.
2. Add tests for `guard`, `hasguard`, and `unguard`, but do not flip
   `Type` yet.
3. Flip `Nat`, `Bool`, `Eq`, `Pi`, `Arrow` to guarded public
   constructors.
4. Update tests that intentionally exercise internals to use
   `unguard Nat`, `unguard (Pi ...)`, or temporary debug aliases.
5. Add typed recursor APIs and migrate library code.
6. Flip public `Type` to guarded universe.
7. Remove or sharply restrict `trust`; the elaborator should only need a
   canonical root `Type` during bootstrap.

## Non-Negotiable Invariants

1. Public type positions accept only guarded types.
2. Public guarded predicates scan candidate values before core checking.
3. Core predicates remain neutral-aware for internal Pi/NbE behavior.
4. `unguard` is available to canonical type constructors, but unguarded
   predicates are rejected by public `Type`.
5. Known-good scanner skips are exact-subtree skips with contracts, not
   ambient trust.
6. Recursors that can produce stuck neutrals must validate their motives
   and branches.
7. Dependent codomains are checked under an allowlist containing only the
   legitimate hypotheses introduced by the checker.
