# Soundness Fix Plan: Guarded Checked Evaluation

## Goal

Close the neutral-forging hole while keeping the type system in tree
calculus. The TS evaluator stays as-is (a fast `apply` plus the
`tree_eq` fast path); all soundness machinery is TC code that the
runtime executes via the existing evaluator.

The intended external boundary stays:

```disp
test (Type n) ParsedType = TT
test ParsedType ParsedTerm = TT
```

The elaborator parses, picks the requested universe, and inserts these
checks. It owns no trusted predicate table.

## Current Hole

The current kernel treats any tree with the `hyp_reduce` checker
signature as a neutral. Three attack vectors verified empirically
(Hyp/StuckElim are public; both are `wait kernel.hyp_reduce meta`):

```disp
// Direct forgery
let fake = StuckElim (Eq Nat 0 1) t
test Eq Nat 0 1 fake = TT          // accepted

// Hidden via K
let bad = {_} -> StuckElim (Eq Nat 0 1) t
test Pi Nat ({_} -> Eq Nat 0 1) bad = TT   // accepted

// Reflective family
let BadFam = {n} -> select Nat (Eq Nat 0 1) (is_neutral n)
let bad = {n} -> 0
test Pi Nat BadFam bad = TT        // accepted because BadFam(hyp) = Nat
test bad 0 = 0                     // but bad 0 has declared type Eq Nat 0 1
```

Three failure modes:

1. **Forged neutral**: untrusted code constructs a tree with the
   `hyp_reduce` signature and arbitrary stored type.
2. **Forged neutral via raw value transport**: K and friends carry an
   arbitrary value into a Pi-checked context where the kernel inspects
   its stored type.
3. **Reflective family**: a type family that reduces differently on a
   neutral hypothesis than on a concrete value typechecks the
   hypothesis case but lies at use site.

All three reduce to: **untrusted code is given the ability to (a)
construct or (b) observe values whose `hyp_reduce` signature decides
kernel behavior.**

## Core Invariant

```text
At the public entry point, the checked term contains no neutral roots.
During checked evaluation, untrusted tree-calculus code may transport,
drop, duplicate, and embed existing neutrals, but it may not inspect
protected neutrals and may not construct a new neutral root.
Only certified kernel handlers may mint new neutral roots.
```

This avoids a growing `allowed_neutrals` set. Once the entry scan
rejects all pre-existing neutrals, every later neutral must come from
a certified handler — provided the checked evaluator prevents raw
evaluation from minting fresh neutral roots, and prevents user code
from observing neutrals' tree structure.

## `cert_make_neutral`

The certified-minting helper is private to the kernel module:

```disp
let cert_make_neutral = {ty, payload} ->
  wait kernel_ref.hyp_reduce (make_neutral_meta ty payload)
```

This is precisely the construction that `checked_apply` rejects when
evaluated from user code (the inner stem/fork-rule step produces a
neutral root, which trips the constructor check). It works for the
kernel because handlers are invoked via raw `apply`, not
`checked_apply` — the rejection only fires inside `checked_apply`.

The two use sites have aliases for readability:

- `cert_make_hyp domain identity = cert_make_neutral domain identity`
  for fresh hypotheses minted by Pi/Type checking.
- `cert_make_stuck result_type target = cert_make_neutral result_type target`
  for stuck eliminator results.

There is no allowlist entry for `cert_make_*`; they are not callable
from user code at all. They are not bound to any public name, and
any user expression that tries to construct `wait kernel.hyp_reduce ...`
would have to evaluate through `checked_apply` and be rejected at
the stem-rule constructor check.

The trust boundary is: anything inside a kernel handler body runs
via raw `apply` (fast, can mint neutrals); anything inside user code
runs via `checked_apply` (slow, cannot mint neutrals). The dispatch
table is what decides which side of the boundary a wait-based value
runs on.

## Tightening `is_neutral`

Today `is_neutral v = fast_eq (pair_fst v) sig`. Because `pair_fst`
is implemented via `triage`, `pair_fst (stem c) = c`, so `stem(sig)`
is also a neutral by the source predicate. Adversaries can mint such
a stem via the leaf rule (`apply(LEAF, sig) = stem(sig)`), giving a
second class of forgery.

Tighten the source predicate to require fork shape:

```disp
is_neutral := {x} -> and (is_fork x) (fast_eq (pair_fst x) sig)
```

Certified handlers always mint via `wait kernel.hyp_reduce meta`,
which reduces to a fork — legitimate neutrals are unaffected. The
trusted `neutral_root` helper now becomes a clean two-line
structural test that obviously matches the source. The leaf-rule
constructor check in `checked_apply` becomes redundant; only the
stem rule can produce a forged neutral, and only when its operand is
the canonical signature.

## `tree_eq` (rename from `fast_eq`)

Current `FAST_EQ` in `src/tree.ts` is a runtime primitive: a chosen
tree-shape sentinel whose TC reduction is *not* equality. The runtime
intercepts any apply whose `f.left.id` is the marker and substitutes
`treeEqual` (hash-cons identity).

Replace this with a real TC equality function whose reduction
computes structural equality, plus a runtime optimization that
recognizes its canonical compiled form. Behavior must be identical
with or without the optimization.

Source definition (recursive triage-based):

```disp
tree_eq := fix ({self, a, b} ->
  triage
    // a is leaf: equal iff b is leaf
    (triage TT ({_} -> FF) ({_, _} -> FF) b)
    // a is stem(ax): equal iff b is stem(bx) with ax == bx
    ({ax} -> triage FF (self ax) ({_, _} -> FF) b)
    // a is fork(ax, ay): equal iff b is fork(bx, by) with both equal
    ({ax, ay} -> triage FF ({_} -> FF) ({bx, by} -> and (self ax bx) (self ay by)) b)
    a)
```

Runtime optimization:

- Compile the canonical `tree_eq` once during prelude load. Capture
  the tree id and bake it into `src/tree.ts` as a constant.
- In `apply`, when both arguments to `tree_eq` have been supplied,
  short-circuit to `treeEqual` (hash-cons identity) returning `TT` or
  `FF`.
- The marker is the TC term itself, not an arbitrary sentinel.
  Removing the optimization yields the same answers, just slower.

**Regression test**: a unit test asserts that compiling the source
definition produces a tree whose id matches the constant baked into
`src/tree.ts`. Any change to the canonical definition that shifts
the id is caught at test time, and the constant gets re-baked.

Rename `fast_eq` → `tree_eq` everywhere in `.disp` and `.ts` sources.

In checked evaluation, `tree_eq` does not need a special handler. The
TC reduction of the recursive definition naturally triages on its
arguments; under `checked_apply`, triage on a neutral fails, so
`tree_eq a b` where either side contains a neutral root fails at
the moment the recursion descends to that neutral. Slow but correct.

For performance, `checked_apply` may optionally short-circuit when
both arguments to a `tree_eq` call are observed to contain no
neutrals (one entry-style scan), but this is an optimization, not a
correctness requirement.

## Tree Predicates

Trusted kernel helpers for recognizing neutral roots and scanning
trees. Once `is_neutral` is tightened to require fork shape, these
become natural structural tests:

```disp
neutral_root x = and (is_fork x) (tree_eq (pair_fst x) sig)

contains_neutral := fix ({self, x} ->
  select_lazy
    ({_} -> TT)
    ({_} -> select_lazy
      ({_} -> or (self (pair_fst x)) (self (pair_snd x)))
      ({_} -> select_lazy
        ({_} -> self (stem_child x))
        ({_} -> FF)
        (is_stem x))
      (is_fork x))
    (neutral_root x))

scan_no_neutral x = not (contains_neutral x)
```

`scan_no_neutral` rejects actual completed neutral values. It does
not need to reject the `hyp_reduce` handler capability merely
appearing as a subtree — dynamic construction is blocked elsewhere.

These helpers are themselves TC. They do triage on raw trees, which
under `checked_apply` would fail on neutrals — but they run at the
trusted layer (above `checked_apply`), via raw `apply`, so triage on
neutrals proceeds normally for them.

## Checked Apply

`checked_apply` is the TC interpreter that runs untrusted user code
under the soundness invariant. It cannot be a thin wrapper around
raw `apply`: a wrapper only sees the final result of an apply chain,
which lets reflective attacks (`pair_fst hyp_neutral` returning the
signature) slip through unobserved. `checked_apply` must walk every
apply step explicitly.

Result encoding:

```disp
CheckedResult = Ok Tree | Fail
Ok := {v} -> t v        // pair(LEAF, v)
Fail := t (t t)         // pair(stem-leaf, leaf) -- distinguishable from Ok
```

### Public Entry

Public guarded predicates enter through:

```disp
checked_check core v =
  select_lazy
    ({_} -> checked_apply core v)
    ({_} -> Fail)
    (scan_no_neutral v)
```

Internal recursive calls use `checked_apply` directly, not
`checked_check`, because they may legitimately operate on certified
hypotheses created by Pi/Type/eliminators.

### Certified Dispatch

Before raw tree application, `checked_apply f x` checks whether `f`
is a wait-based value with a known kernel handler signature. The
list of recognized signatures is **the security perimeter** — see
[Dispatch Table](#dispatch-table-as-security-perimeter).

```disp
checked_apply f x =
  select_chain
    (case (has_sig kernel.guard f)        (checked_guard_apply f x)
    (case (has_sig kernel.pi f)           (checked_pi_apply f x)
    (case (has_sig kernel.nat f)          (checked_nat_apply f x)
    (case (has_sig kernel.bool f)         (checked_bool_apply f x)
    (case (has_sig kernel.eq f)           (checked_eq_apply f x)
    (case (has_sig kernel.core_type f)    (checked_core_type_apply f x)
    (case (has_sig kernel.guarded_type f) (checked_guarded_type_apply f x)
    (case (has_sig kernel.hyp_reduce f)   (checked_hyp_reduce_apply f x)
    (case (has_sig kernel.bool_rec f)     (checked_bool_rec_apply f x)
    (case (has_sig kernel.nat_rec f)      (checked_nat_rec_apply f x)
    (case (has_sig kernel.eq_J f)         (checked_eq_J_apply f x)
    t)))))))))))
    checked_raw_apply
    f x
```

Certified handlers may inspect neutral metadata, may use raw
`tree_eq` internally, and may mint new neutral roots via
`cert_make_hyp` / `cert_make_stuck`. They are responsible for
validating all user inputs through `checked_apply` before minting.

### Raw Rules

The raw evaluator mirrors `apply`'s 5 reduction rules with
constructor checks at every fork-producing step:

```text
checked_raw_apply I x:                    // exact-I shortcut
  Ok x

checked_raw_apply leaf x:
  Ok (stem x)                             // post-tightening, no stem
                                          // is a neutral; no check needed

checked_raw_apply (stem a) x:
  let r = fork a x
  if neutral_root r: Fail
  else: Ok r

checked_raw_apply (fork leaf payload) x:  // K
  Ok payload

checked_raw_apply (fork (stem c) b) x:    // S
  let cx = checked_apply c x
  let bx = checked_apply b x
  if cx or bx is Fail: Fail
  else: checked_apply cx bx

checked_raw_apply (fork (fork tc td) b) x: // triage
  if neutral_root x: Fail                  // user code may not observe
  else if x is leaf: Ok tc
  else if x is stem c: checked_apply td c
  else if x is fork l r:
    let bl = checked_apply b l
    if bl is Fail: Fail
    else: checked_apply bl r
```

The triage-on-neutral ban is what closes the reflective family
attack: `is_neutral n` evaluates `pair_fst n` which triages on `n`;
if `n` is the Pi-check hypothesis, that triage fails, so the codomain
fails to evaluate, so the Pi check rejects the whole binding.

Note that exact `I = fork(fork(LEAF, LEAF), LEAF)` is itself a triage
shape; without the carve-out, polymorphic identity (`{A : Type 0} ->
A -> A`) on a hypothesis would be rejected. `checked_apply`
recognizes `I` via `tree_eq f I_canonical`, where `I_canonical` is the
TC constant `fork(fork(LEAF, LEAF), LEAF)`. The runtime's `tree_eq`
fast path makes this O(1).

(The runtime evaluator `apply` does *not* need to special-case `I`.
TC reduction of `apply(I, x)` via the triage rule already returns
`x` unchanged. The carve-out is purely a `checked_apply` concern,
because `checked_apply`'s "triage on neutral fails" rule would
otherwise reject identity on a hypothesis.)

### `tree_eq` in Checked Mode

`checked_apply` does not special-case `tree_eq` for correctness. When
user code calls `tree_eq a b`, the TC reduction of the recursive
definition runs naturally. If either argument contains a neutral
anywhere in its structure, the recursion eventually descends to a
triage on that neutral, which fails under `checked_apply`'s
triage-on-neutral rule. The user gets `Fail` exactly when they
should.

`checked_apply` may *optionally* short-circuit `tree_eq` calls (one
deep `contains_neutral` scan up front, then the runtime fast path on
neutral-free trees). This is a future optimization, not part of
correctness. The simpler "let the recursion handle it" path lands
first.

Certified handlers use raw `tree_eq` (via the runtime fast path)
directly. They run via raw `apply`, not `checked_apply`, so the
triage-on-neutral rule does not apply to them. They are responsible
for not invoking `tree_eq` on data sourced from untrusted user
code without first validating it.

## Dispatch Table as Security Perimeter

The dispatch list in `checked_apply` is the only place where
untrusted code is granted the ability to mint neutrals (indirectly,
by invoking a certified handler). Every entry is a load-bearing
contract with two parts:

**(a) The handler's identity is established by its checker
signature.** Adding an entry without a checker (or with a checker
shared with another handler) lets adversaries forge handler
identity.

**(b) The handler validates all user inputs via `checked_apply`
before minting any neutral.** Validation must cover every value the
handler will copy into the resulting neutral's metadata.

If you add a handler and forget to register it in the dispatch
table, the failure is *loud*: the handler's body runs through
`checked_apply` instead of as trusted code, its inner
`cert_make_neutral` is rejected as a raw constructor producing a
neutral root, and the handler stops working. Better: failure mode is
"handler appears broken", not "soundness silently lost".

If you add a handler and register it in the dispatch table without
satisfying (b), the failure is *silent*: forgery via that handler
becomes possible. Reviews of changes to the dispatch table are
load-bearing.

Every entry in the dispatch table must be accompanied by a comment
listing exactly which user inputs it validates and where. New
handlers must add corresponding adversarial tests under
`lib/kernel.test.disp`.

## Guard API

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

`unguard_public` is for closed source-level boundaries: only
top-level guarded public types are accepted.

`unguard_checked` is for certified kernel handlers running under the
checked-evaluator invariant: either a guarded type or a certified
neutral type variable. A closed forged neutral cannot reach this
path because public guard entry rejects pre-existing neutrals and
raw checked evaluation cannot mint new ones.

The guard handler:

```disp
checked_guard_apply guard_value v =
  let core = type_meta guard_value
  checked_check core v
```

So public raw-looking application still has the desired behavior:
`Nat v` means "scan `v` for closed forged neutrals, then
checked-evaluate the core Nat predicate on `v`."

## Type Constructors

Keep the current mutually recursive kernel predicates as core
predicates, but export guarded public constructors:

```disp
Nat = guard (wait kernel_ref.nat t)
Bool = guard (wait kernel_ref.bool t)
Eq = {A, x, y} -> guard (wait kernel_ref.eq
  (make_eq_meta (unguard_checked A) x y))
Pi = {A, B} -> guard (wait kernel_ref.pi
  (make_pi_meta (unguard_checked A) ({x} -> unguard_checked (B x))))
Arrow = {A, B} -> Pi A ({_} -> B)
```

When these constructors are used in closed parsed types,
`unguard_checked` behaves like `unguard_public`. When used under
Type/Pi checking with hypothesis-introduced type variables,
`unguard_checked` also accepts those certified neutrals.

## Universes

Split the universe checker into:

```disp
core_type n     // raw/core type predicates and certified neutral type vars
guarded_type n  // public source-level types (must be guarded)
```

`core_type n` validates raw type predicates: core Nat and Bool live
in `Type 0`; core Eq A x y lives in the same universe as A; core Pi
A B lives in max(level A, level B); core `guarded_type m` lives in
`core_type (succ m)`; certified neutral type variables are accepted
via the H-rule.

```disp
checked_guarded_type_apply (guarded_type n) T =
  if neutral_root T:
    checked_h_rule (guarded_type n) T
  else:
    and (hasguard T)
        (checked_apply (core_type n) (unguard_public T))

Type n = guard (wait kernel_ref.guarded_type n)
```

Internal recursive universe checks must use `checked_apply`, not
public `Type n`, so they can validate codomains containing certified
neutral type variables.

## Pi Handler

Rewrite so all adversarial applications run through `checked_apply`.

```disp
checked_pi_apply pi_type v =
  if neutral_root v:
    checked_h_rule pi_type v
  else:
    let domain = pi_meta_domain (type_meta pi_type)
    let codFn = pi_meta_cod_fn (type_meta pi_type)
    let hyp = cert_make_hyp domain (fresh_id pi_type)
    let result_r = checked_apply v hyp
    let expected_r = checked_apply codFn hyp
    if either is Fail: FF
    else if neutral_root (ok_value result_r):
      tree_eq (ok_value expected_r) (neutral_type (ok_value result_r))
    else:
      tree_eq (checked_apply (ok_value expected_r) (ok_value result_r)) (Ok TT)
```

`cert_make_hyp` is trusted code that may construct a neutral root
directly via raw `apply`.

## Hyp Reduce Handler

The certified interpreter for applying a neutral to an argument:

```disp
checked_hyp_reduce_apply neutral arg =
  let current_type = neutral_type neutral
  if is_pi current_type:
    let codFn = pi_cod_fn current_type
    let result_type_r = checked_apply codFn arg
    if result_type_r is Fail: Fail
    else: Ok (cert_make_neutral
      (ok_value result_type_r)
      (extend_payload (type_meta neutral) arg))
  else:
    Ok (cert_make_neutral InvalidType
      (extend_payload (type_meta neutral) arg))
```

## Nat, Bool, Eq Handlers

Concrete cases unchanged; recursive/dependent checks all use
`checked_apply`:

- Neutral candidate → H-rule.
- Concrete Nat successor → recurse via certified Nat handler.
- Eq with `refl` → check lhs equal rhs via *internal* `tree_eq` (not
  user-mode).
- Eq metadata containing terms requiring check against A → use
  `checked_apply A x`.

## Certified Eliminators

Replace raw eliminator definitions with wait-based certified
handlers:

```disp
bool_rec = {motive, t_case, f_case, target} ->
  wait kernel_ref.bool_rec
    (make_bool_rec_meta motive t_case f_case target)
```

Handler obligations (illustrative):

```disp
checked_bool_rec_apply rec_value _ =
  let m = read_meta rec_value
  let motive = m.motive
  let t_case = m.t_case
  let f_case = m.f_case
  let target = m.target
  // (1) target is a Bool
  must_ok (checked_apply Bool target)
  // (2) branches inhabit motive at TT and FF
  let tT = checked_apply motive TT
  let fT = checked_apply motive FF
  must_ok (checked_apply (ok_value tT) t_case)
  must_ok (checked_apply (ok_value fT) f_case)
  // (3) dispatch
  if tree_eq target TT: Ok t_case
  else if tree_eq target FF: Ok f_case
  else if neutral_root target:
    let result_type_r = checked_apply motive target
    must_ok (checked_apply (Type rank) (ok_value result_type_r))
    Ok (cert_make_neutral (ok_value result_type_r) target)
  else: Fail
```

`nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong` follow the same
pattern: validate target, validate branches against the motive at the
relevant points, validate result type, then mint or dispatch.

The current raw versions (lib/kernel.disp:283-347) construct
`StuckElim` directly and must be deleted. Under `checked_apply`, those
raw lambdas would be rejected because the inner `wait kernel.hyp_reduce
...` constructor step produces a neutral root and there is no
certified-handler dispatch covering them.

**Optional helper: `make_recursor`.** All the eliminator handlers
share a common shape (validate target's type, validate branches
against motive applied at each constructor case, dispatch on target
with neutral fallback that returns a stuck eliminator). Once two or
three are written, factor out a `make_recursor` combinator that
takes the target's type checker, a list of constructor case
descriptors, and the eliminator's wait-handler signature, and
generates the certified handler. This collapses near-duplicate
validators into one parametric one and removes a class of "I forgot
to validate motive at this branch" bugs. Not blocking the initial
implementation; clean up once the pattern is stable.

## Trust Boundary: Raw Apply vs Checked Apply

The kernel uses two evaluation paths and it's important to keep
straight which is which:

- **Raw `apply`** (the TS runtime). Fast. Used to invoke certified
  handler bodies and any helpers the kernel constructs and consumes
  itself. Trusts its inputs implicitly; will happily mint a neutral
  if the term it evaluates ends up shaped like one.
- **`checked_apply`** (TC interpreter). Slow. Used to evaluate user
  terms. Distrusts its inputs; rejects fresh neutral construction,
  triage on neutrals, and `tree_eq` on neutral-containing trees.

**Rule of thumb for handler implementers:**

- Reading metadata that the kernel itself just constructed
  (`pi_meta_domain`, `pi_meta_cod_fn`, `eq_meta_lhs`, etc.) →
  raw `apply`. The metadata's structure is known and trusted.
- Reading or evaluating any value that came from user code
  (the `v` argument to a checker, the `motive` of an eliminator,
  the `target`, etc.) → `checked_apply`. Even if you "know" what
  shape it should have, the user might have supplied something else.
- Constructing a new neutral via `cert_make_*` → raw `apply`. By
  construction, the result is a kernel-minted neutral.
- Comparing two trees via `tree_eq` *only when both are
  kernel-constructed metadata* → raw `apply`. If either side
  originates from user code, you must either route through
  `checked_apply` or apply a `contains_neutral` scan first.

Add a comment at every raw-apply call site inside a kernel handler
explaining why raw is correct there. Comments like "// raw: reading
kernel-built Pi metadata" or "// raw: target was just minted by
cert_make_neutral above". This makes review tractable: any raw call
without such a comment is a bug.

## Public Neutral Constructors

`Hyp` and `StuckElim` should not survive in the public API after this
fix.

Options:

1. Remove them from public exports.
2. Rename to `unsafe_Hyp`/`unsafe_StuckElim` for kernel-internal
   tests, and ensure public guarded checks reject any term that
   contains them (entry scan handles this — they produce neutral
   roots).

The kernel itself constructs neutrals only through `cert_make_hyp`
and `cert_make_stuck` (raw helpers used only inside certified
handlers).

## Kernel Edit Checklist

1. **Result encoding**: `Ok`, `Fail`, `is_ok`, `ok_value`, `must_ok`.
2. **Tighten `is_neutral`** to require fork shape. Add unit tests for
   `is_neutral (stem sig) = FF`.
3. **Rename `fast_eq` → `tree_eq`** across all `.disp` and `.ts`
   sources. Add the canonical TC `tree_eq` definition. Verify the
   runtime optimization in `src/tree.ts` recognizes the canonical
   compiled tree id (replacing `FAST_EQ_MARKER`).
4. **Trusted helpers**: `neutral_root`, `contains_neutral`,
   `scan_no_neutral`.
5. **`checked_apply`** as a TC interpreter with all rules above.
   Recognize the canonical `I` via `tree_eq f I_canonical` (the
   runtime fast path keeps this O(1)). `tree_eq` itself does *not*
   need explicit recognition in `checked_apply` — its TC reduction
   handles correctness via the triage-on-neutral rule.
6. **Guard API**: `guard`, `hasguard`, `unguard_public`,
   `unguard_checked`.
7. **Split `type` into `core_type` and `guarded_type`**.
8. **Rewrite public constructors**: `Nat`, `Bool`, `Eq`, `Pi`,
   `Arrow`, `Type`.
9. **Rewrite handlers**: `q_pi_fn`, `q_type_fn`,
   `q_hyp_reduce_fn`, `q_nat_fn`, `q_bool_fn`, `q_eq_fn` to use
   checked application at every adversarial boundary. Move to the
   certified-handler wait shape.
10. **Replace raw eliminators** with certified wait handlers:
    `bool_rec`, `nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`.
11. **Remove `Hyp` / `StuckElim`** from the public API (or rename to
    `unsafe_*`).
12. **Update the elaborator** so typed annotations are checked by
    public `Type n`, terms by public guarded predicates. The
    elaborator owns no trusted predicate table.
13. **Rewrite kernel tests using raw `Hyp` / `StuckElim`.** The
    existing tests at `lib/kernel.test.disp:80-97` (and similar)
    construct stuck eliminators on raw hypotheses such as
    `Hyp Bool 0`. Once `Hyp` is removed (or renamed `unsafe_*`), the
    legitimate way to obtain a hypothesis is through a Pi-context.
    Rewrite these tests to exercise the certified eliminators on
    hypotheses minted by `Pi`/`Type` checking. Expect a noisy test
    migration; track separately so soundness-fix diffs aren't
    dominated by test churn.

## Required Tests

All under `lib/kernel.test.disp` (or a dedicated
`lib/soundness.test.disp`).

**Reject forged closed neutrals:**

```disp
let fake_proof : Eq Nat 0 1 = StuckElim (Eq Nat 0 1) t
// expected: kernel rejects via entry scan
```

**Reject K-hidden forgery:**

```disp
let bad : Nat -> Eq Nat 0 1 = {_} -> StuckElim (Eq Nat 0 1) t
// expected: rejected (the literal StuckElim subterm is a neutral root
// detected by entry scan)
```

**Reject reflective `is_neutral`:**

```disp
let BadFam = {n} -> select Nat (Eq Nat 0 1) (is_neutral n)
let bad : Pi Nat BadFam = {n} -> 0
// expected: Pi check evaluates BadFam(hyp); `is_neutral hyp` triages
// on hyp; checked_apply rejects triage-on-neutral; Pi rejects.
```

**Reject reflective `is_fork`:**

```disp
let BadFam = {n} -> select Nat (Eq Nat 0 1) (is_fork n)
let bad : Pi Nat BadFam = {n} -> 0
// expected: same mechanism (is_fork is a triage on n).
```

**Reject `tree_eq` on neutral-containing trees:**

```disp
let BadFam = {n} -> select (Eq Nat 0 1) Nat (tree_eq n 0)
let BadFam2 = {n} -> select (Eq Nat 0 1) Nat (tree_eq (t n 0) (t 0 0))
// expected: tree_eq's recursion descends to a triage on the neutral
// hypothesis n, which fails under checked_apply's triage-on-neutral
// rule. BadFam evaluation under hyp fails; Pi rejects.
```

**Reject unsafe eliminator motives/branches:**

```disp
let bad_bool : Bool -> Eq Nat 0 1 = {b} ->
  bool_rec ({_} -> Eq Nat 0 1) 0 0 b
// expected: bool_rec validation requires t_case : motive(TT) = Eq Nat 0 1
// and f_case : motive(FF) = Eq Nat 0 1. No closed inhabitants exist;
// validation fails.
```

**Tightened `is_neutral` rejects stems** (kernel-internal regression
test for the tightening; uses internal helpers to construct the edge
case, since `cert_make_*` is private):

```disp
// Internal test only — exercises the tightened predicate against a
// crafted stem-shape with the canonical signature as its child.
test is_neutral (t hyp_reduce_sig) = FF   // post-tightening: stems aren't neutrals
test is_neutral (cert_make_neutral t t) = TT  // legitimate fork-shape neutral
```

**Accept SKI transport:**

```disp
let id : Nat -> Nat = {x} -> x
let const : Nat -> Bool -> Nat = {x} -> {_} -> x
let succ_fn : Nat -> Nat = {x} -> succ x
```

**Accept dependent and polymorphic identity:**

```disp
let dep_refl : {n : Nat} -> Eq Nat n n = {n} -> refl
let poly_id : {A : Type 0} -> A -> A = {A} -> {x} -> x
let type_id : Type 0 -> Type 0 = {A} -> A
```

**Accept certified eliminators:**

```disp
let bool_to_nat : Bool -> Nat = {b} -> bool_rec ({_} -> Nat) 1 0 b
let nat_to_bool : Nat -> Bool = {n} ->
  nat_rec ({_} -> Bool) TT ({_k, _prev} -> FF) n
```

**Universes:**

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

- `neutral_root` is O(1): tightened to a structural fork-with-correct-left
  test, equivalent to a hash-cons identity check on `pair_fst`.
- `contains_neutral` should memoize per tree id (hash-cons gives a
  natural key).
- `scan_no_neutral` is one entry scan of the candidate tree.
- `tree_eq` from user code descends recursively until it hits a
  neutral and fails via the triage-on-neutral rule; from kernel
  internals it uses the runtime fast path (O(1) hash-cons).
- Certified handlers may use raw `tree_eq` and raw apply on metadata
  they own.
- `checked_apply` walks each native apply step explicitly, so
  user-code evaluation pays a constant-factor overhead. Expected 10-50×
  on the user-code-only path. Kernel internals (certified handlers)
  stay at native speed.

The expected total overhead is proportional to public-entry tree size
plus the size of evaluation steps in user code, not proportional to
the number of hypotheses created during checking.

## Residual Risks

The design closes the known holes if implemented as above. The main
risks are implementation mistakes:

- accidentally using raw `apply` where `checked_apply` is required;
- treating exact `I` as ordinary triage and rejecting identity;
- letting public `guard` run closed scans during internal recursive
  checks, which breaks certified neutral type variables;
- leaving raw `StuckElim`-based recursors in place;
- bypassing the triage-on-neutral rule when adding the optional
  `tree_eq` fast path under `checked_apply` — the fast path must run
  a deep `contains_neutral` scan first, otherwise user code can
  observe neutral structure via hash-cons identity comparison;
- making `unguard_checked` available as a public escape hatch
  without the public entry scan;
- adding a handler to the dispatch table without satisfying the
  validation contract (silent forgery — see [Dispatch
  Table](#dispatch-table-as-security-perimeter)).

These should be covered by the [required tests](#required-tests)
before considering the fix complete.

## Kernel Record Shape

The closed kernel record grows from
`{hyp_reduce, pi, nat, bool, eq, type}` to:

```
{ hyp_reduce            // unchanged
  guard                 // new — public-entry wrapper
  pi                    // unchanged identity, body rewritten
  nat, bool, eq         // unchanged identity, body rewritten
  core_type             // formerly `type`
  guarded_type          // new — public universe
  bool_rec, nat_rec, eq_J  // new — certified eliminators (formerly raw)
}
```

`eq_subst`, `eq_sym`, `eq_cong` either become additional certified
handlers in the kernel or are derived combinators built on top of
`eq_J`. The latter is preferable if it simplifies the perimeter.

## Out of Scope

This plan keeps the kernel record closed:
`{hyp_reduce, pi, nat, bool, eq, type, ...}`. Lifting it to a
parametric kernel where users can register new type handlers is a
separate refactor — see [`KERNEL_EXTENSIBILITY_PLAN.md`](KERNEL_EXTENSIBILITY_PLAN.md).
The order is: land soundness on the closed kernel first (with the
dispatch-table contract written down), then extend.
