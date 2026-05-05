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

The certified-minting helper is private to the kernel module. It
uses `raw.hyp_reduce` (not `kernel_ref.hyp_reduce`) so that the
resulting wait-value's signature matches what `is_neutral` /
`q_is_neutral` test against — see [Bootstrap and Signature
Conventions](#bootstrap-and-signature-conventions).

Every handler that mints neutrals has access to `raw` (one of the
three recq-delivered args). Each handler body defines a local alias
that closes over `raw`, keeping call sites clean:

```disp
// At the top of every handler that mints:
let cert_make_neutral = {ty, payload} ->
  wait raw.hyp_reduce (make_neutral_meta ty payload)
let cert_make_hyp     = {domain, identity}     -> cert_make_neutral domain identity
let cert_make_stuck   = {result_type, target}  -> cert_make_neutral result_type target
```

(Why `raw` and not `ks`: `ks.hyp_reduce = wait self hyp_reduce_query`
has a different stem id from `raw.hyp_reduce`, so `q_is_neutral`
would not recognize the result. This matches today's `q_make_hyp`
in `lib/kernel.disp:67` which already uses `raw.hyp_reduce`.)

This is precisely the construction that `checked_apply` rejects when
evaluated from user code (the inner stem/fork-rule step produces a
neutral root, which trips the constructor check). It works for the
kernel because handlers are invoked via raw `apply`, not
`checked_apply` — the rejection only fires inside `checked_apply`.

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

## Tightening `is_neutral` *(landed)*

Background: `is_neutral v = tree_eq (pair_fst v) sig`. `pair_fst` is
implemented via `triage`, so `pair_fst (stem c) = c` — `stem(sig)`
passed the signature check, giving a stem-shaped class of forgery on
top of the fork-shaped one.

Both `q_is_neutral` (kernel-internal) and the public `is_neutral` now
require fork shape:

```disp
is_neutral := {x} -> and (is_fork x) (has_sig kernel.hyp_reduce x)
```

Certified handlers always mint via `wait kernel.hyp_reduce meta`,
which reduces to a fork, so legitimate neutrals are unaffected. The
trusted `neutral_root` helper (used inside `scan_no_neutral` and the
`checked_raw_apply` constructor checks below) becomes a clean
two-line structural test. Only the stem rule of `checked_raw_apply`
can produce a forged neutral, and only when its operand is the
canonical signature.

Regression: `lib/kernel.test.disp` asserts `is_neutral (t hyp_reduce_sig) = FF`
(stem of the canonical signature is rejected post-tightening).

## `tree_eq` (rename from `fast_eq`) *(partially landed)*

**Landed:** rename `fast_eq` → `tree_eq` across `.disp` and `.ts`,
and `FAST_EQ` / `FAST_EQ_MARKER` → `TREE_EQ` / `TREE_EQ_MARKER` in
`src/tree.ts`. Canonical recursive TC definition `tree_eq_spec`
added to `lib/prelude.disp`. Cross-check regression test in
`lib/tree_eq.test.disp` asserts the host-optimized `tree_eq` and the
canonical `tree_eq_spec` agree on a panel of inputs covering leaves,
stems, forks, and Nat-shaped trees.

**Not landed:** the runtime's host-optimized `tree_eq` is still
keyed off the static `TREE_EQ_MARKER` shape (a chosen sentinel),
not the runtime-captured id of the canonical `tree_eq_spec` tree.
This is intentional — the literal `fork(fork(tree_eq_id, _), _)`
recognition pattern described in earlier drafts of this plan does
not naturally fire on real `tree_eq_spec` calls (the spec form's
partial application reduces to triage-shaped intermediates, never
to a fork whose left is the captured tree id). Designing a
runtime-captured-id mechanism that satisfies "removing the
optimization yields the same answers" without observable side
effects (e.g., user code observing the synthetic partial-application
marker via `pair_fst`) is left as a follow-up.

The current path is sound and matches the plan's semantic goal:
the canonical TC `tree_eq_spec` is the source of truth; the host
`tree_eq` is an optimization with cross-checked equivalence. Only
the *recognition mechanism* used by the host fast path remains a
sentinel rather than a captured id.

Source definition (in `lib/prelude.disp`):

```disp
tree_eq_spec := fix ({self, a, b} ->
  triage
    (triage TT ({_} -> FF) ({_l, _r} -> FF) b)
    ({ax} -> triage FF (self ax) ({_l, _r} -> FF) b)
    ({ax, ay} -> triage FF ({_} -> FF) ({bx, by} -> and (self ax bx) (self ay by)) b)
    a)
```

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
neutral_root := {x} -> and (is_fork x) (tree_eq (pair_fst x) sig)

contains_neutral := fix ({self, x} ->
  match (neutral_root x) {
    TT => TT
    FF => match (is_fork x) {
      TT => or (self (pair_fst x)) (self (pair_snd x))
      FF => match (is_stem x) {
        TT => self (stem_child x)
        FF => FF
      }
    }
  })

scan_no_neutral := {x} -> not (contains_neutral x)
```

`scan_no_neutral` rejects actual completed neutral values. It does
not need to reject the `hyp_reduce` handler capability merely
appearing as a subtree — dynamic construction is blocked elsewhere.

These helpers are themselves TC. They do triage on raw trees, which
under `checked_apply` would fail on neutrals — but they run at the
trusted layer (above `checked_apply`), via raw `apply`, so triage on
neutrals proceeds normally for them.

## Laziness Discipline: `match` and Closed Branches

Tree calculus is strict. The naive way to write conditional branches
in a checker — `select_lazy ({_} -> heavy_call) ({_} -> Fail) cond` —
**does not actually delay `heavy_call`** when the thunk's body has
free variables from the enclosing scope. Bracket abstraction over
those free vars produces an `S (K K) ...` chain whose runtime S-rule
reduction eagerly evaluates the body before `select_lazy` ever
triages on `cond`. The thunk is a thunk only when its body is closed.

The kernel must therefore use **select-then-apply with closed
branches**: each branch is a function whose parameter list saturates
its body's free variables, so the branch compiles to a closed tree.
`select` picks one branch, *then* the trailing arguments are applied
to whichever was picked — the unselected branch's body never runs.

Writing this by hand is verbose, so the plan uses a `match` keyword
that desugars to closed-branch select-then-apply with automatic
free-variable capture. Surface form:

```disp
match cond {
  TT => then_body
  FF => else_body
}
```

Desugars to (with `fvs` = union of free vars in either branch, in
stable insertion order):

```disp
select
  ({fvs...} -> then_body)
  ({fvs...} -> else_body)
  cond
  fvs...
```

The compiler computes `fvs` from each arm's compiled Cir (a name is
"free" iff it would compile to a `var` rather than a closed `lit`).
Already-closed top-level names (e.g., `Fail`, `Nat`, prelude defs)
don't appear in `fvs`. Each arm becomes a closed function value;
`select` picks one; the trailing apply feeds shared args to the
chosen one only.

**Every conditional in the kernel below uses `match`, not
`select_lazy` with thunks.** When you read `match cond { TT => a; FF
=> b }`, mentally substitute the closed-branch desugaring — that's
the actual compiled shape, and that's why short-circuiting works.

`select_lazy` is still safe in two narrow cases: both branches are
closed expressions (no outer free vars), or both branches are O(1)
operations cheap enough that eager evaluation is acceptable
(structural `tree_eq`, constant returns). Anything that recurses or
calls `ks.X` must use `match`.

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
Ok := {v} -> t t v      // pair(LEAF, v) = K v = fork(LEAF, v)
Fail := t (t t)         // stem(K) -- not a fork-with-leaf-left, distinguishable from Ok

is_ok    := {r} -> tree_eq (pair_fst r) t        // r matches Ok _
ok_value := {r} -> pair_snd r                    // valid only when is_ok r

// Why `t t v` and not `t v`: pair(l, r) is fork(l, r), so Ok must produce
// a fork. `t v = stem(v)` is not a fork. `t t v = K v = fork(LEAF, v)` is.
// With this encoding, pair_fst (Ok v) follows the fork branch of pair_fst's
// triage and returns LEAF = t, so is_ok (Ok v) = TT regardless of v.
```

Two distinct "must succeed" helpers. `Ok TT` and `Ok FF` are *both*
successful evaluations, so the difference matters:

```disp
// Sub-evaluation must not Fail; the Ok payload is threaded into k
// and may legitimately be TT or FF or any tree. k must be a closed
// function (top-level let or {fvs...} -> ... whose params cover its body).
must_ok_any := {r, k} ->
  match (is_ok r) {
    TT => k (ok_value r)
    FF => Fail
  }

// Predicate evaluation must succeed AND return TT. Used for "this
// value really is a Bool", "this branch really inhabits motive(TT)",
// etc. — anywhere a successful FF would mean "no, validation failed".
must_ok_tt := {r, k} ->
  match (is_ok r) {
    TT => match (tree_eq (ok_value r) TT) {
      TT => k t
      FF => Fail
    }
    FF => Fail
  }
```

Both helpers `match` instead of `select_lazy`. The `match` arms have
free vars (`r`, `k`) — capture handles them; the desugarer lifts
both arms into closed functions of `(r, k)` and applies `r k` only
to the chosen one. So when `r` is `Fail`, the success arm's `k (...)`
is never invoked. **The CPS chain genuinely short-circuits.**

Handler bodies use `must_ok_tt` for predicate validation (the common
case) and `must_ok_any` when threading a sub-evaluation result into
the next step. Mixing these up is a soundness bug: using
`must_ok_any` where `must_ok_tt` is needed would accept `Ok FF`
as "checked successfully" and then mint a neutral whose stored type
came from a value that failed validation.

The continuation `k` passed to `must_ok_*` must itself be a closed
function. In handler bodies the natural form is an inline lambda
whose parameter list covers its body; the desugarer treats it as
closed at compile time. (A `{_} -> body` continuation with outer
free vars is the broken pattern from the introduction — don't write
that.)

### Public Entry

Public guarded predicates enter through:

```disp
checked_check := {core, v} ->
  match (scan_no_neutral v) {
    TT => checked_apply core v
    FF => Fail
  }
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
q_checked_apply_fn = {ks, raw, query} -> fix ({self, f, x} ->
  // Each branch is `{self, f, x} -> body` — closed function whose
  // params cover its body. select_chain picks one, applies self/f/x.
  // Neutrals: use raw.hyp_reduce because Hyp/StuckElim store the
  // raw handler signature (see "Bootstrap and Signature Conventions").
  // Type-formers and eliminators: use ks.X (lazy proxy) because
  // their public constructors build wait-values via kernel_ref.X,
  // which has the same hash-cons identity as ks.X.
  select_chain
    (case (has_sig raw.hyp_reduce f)  ({self, f, x} -> f x))
    (case (has_sig ks.guard f)        ({self, f, x} -> f x))
    (case (has_sig ks.pi f)           ({self, f, x} -> f x))
    (case (has_sig ks.nat f)          ({self, f, x} -> f x))
    (case (has_sig ks.bool f)         ({self, f, x} -> f x))
    (case (has_sig ks.eq f)           ({self, f, x} -> f x))
    (case (has_sig ks.core_type f)    ({self, f, x} -> f x))
    (case (has_sig ks.guarded_type f) ({self, f, x} -> f x))
    (case (has_sig ks.bool_rec f)     ({self, f, x} -> f x))
    (case (has_sig ks.nat_rec f)      ({self, f, x} -> f x))
    (case (has_sig ks.eq_J f)         ({self, f, x} -> f x))
    // Default: walk apply rules step-by-step.
    ({self, f, x} -> checked_raw_apply self f x)
    self f x)
```

`f x` inside a recognized branch is a single raw tree-calculus
application; the runtime evaluates it via the regular `apply`
function. This fires the wait-value's checker (`q_pi_fn` etc.) as
trusted code. The handler body may itself call back into
`checked_apply` (via `ks.checked_apply`) when it needs to validate
user-supplied subterms — that is the recursion the dispatcher
participates in.

Certified handlers may inspect neutral metadata, may use raw
`tree_eq` internally, and may mint new neutral roots via
`cert_make_hyp` / `cert_make_stuck`. They are responsible for
validating all user inputs through `checked_apply` before minting.

**Handler invocation runs via raw `apply`.** When the dispatcher
selects a handler and writes `f x`, that is raw tree-calculus
application, *not* a recursive entry into `checked_apply`. Handler
bodies are trusted code despite being selected from inside the
checked interpreter. The "rejection of fresh neutral roots" rule
only fires when reduction happens inside `checked_raw_apply`'s rules
below — handler bodies bypass that ruleset entirely.

### Shared H-rule helper

Every type checker handles neutrals identically: reconstruct the
checker's own type via the recq query, hash-cons-compare it to the
neutral's stored type. This is the existing `q_h_rule_fn` from the
current kernel, ported as:

```disp
checked_h_rule := {self, meta, v} ->
  // Both arguments are kernel-internal:
  // - `wait (ks query) meta` is the checker reconstructing its own type.
  // - `neutral_type v` reads from a CERTIFIED neutral (we got here by
  //   the dispatcher recognizing kernel.hyp_reduce on v).
  // Raw tree_eq is correct: no user-controlled tree on either side.
  match (tree_eq (wait (ks query) meta) (neutral_type v)) {
    TT => Ok TT
    FF => Ok FF
  }
```

The wrapper returns `Ok TT`/`Ok FF` (predicate result) rather than
`Fail`, because failing the H-rule is a legitimate "no" answer, not
an evaluation failure.

### Handler Return Conventions

Two return shapes coexist in the kernel; the convention picks one
per handler family based on what the public boundary expects:

| Handler family             | Returns           | Why                                         |
|----------------------------|-------------------|---------------------------------------------|
| `q_guard_fn`               | bare `TT` / `FF`  | Public predicate boundary; `Nat 1 = TT`     |
| All other type checkers    | `CheckedResult`   | Composes inside `checked_apply` recursion   |
| `q_hyp_reduce_fn`          | `CheckedResult`   | Returns `Ok (cert_make_neutral …)`          |
| Eliminator handlers        | `CheckedResult`   | Wrapper unwraps to bare value (or sentinel) |

**Why `q_guard_fn` is the unwrap point.** The user-facing tests
(`Nat 1 = TT`) compare against bare `TT`. Public type constructors
are guarded wait-values (`Nat = wait kernel_ref.guard core_Nat`), so
any user-mode predicate call dispatches through `q_guard_fn`. Making
that one handler bare-valued lets every existing predicate test pass
unchanged. Internal handlers compose through `checked_apply` and
return `CheckedResult` because their callers (other handlers) want
the explicit success/failure distinction.

**Eliminator wrapper pattern.** The user-facing eliminator names
(`bool_rec`, `nat_rec`, `eq_J`, …) are thin wrappers around their
wait-handlers. The wait-value is inert until applied to one extra
argument; the wrapper supplies a trigger token (`t` is fine) and
unwraps the result:

```disp
bool_rec := {motive, t_case, f_case, target} ->
  unwrap_value
    ((wait kernel_ref.bool_rec
        (make_bool_rec_meta motive t_case f_case target)) t)

unwrap_value := {r} ->
  match (is_ok r) {
    TT => ok_value r           // success: return the value the handler computed
    FF => elim_fail            // failure: return the sentinel
  }
```

`elim_fail` is a kernel-private sentinel. It is `t` for now; any
downstream use of an `elim_fail` value will likely cause downstream
type checks to fail loudly, which is the desired behavior. (A more
informative sentinel — e.g., a wait-value carrying a reason — is a
future ergonomics improvement, not a correctness one.)

The handler ignores the trigger argument:

```disp
q_bool_rec_fn = {ks, raw, query} -> {meta, _trigger} ->
  // ... CheckedResult body, see Certified Eliminators ...
```

Wrappers run as ordinary user code. Under raw `apply` (a top-level
`bool_rec` call from a test), the wait-value's checker fires
directly. Under `checked_apply` (a `bool_rec` call inside a Pi-checked
body), the dispatcher recognizes `kernel.bool_rec`'s signature and
routes the same way. Both paths produce `CheckedResult`, and both
paths feed the same `unwrap_value` step. The user sees a bare value.

**Type-reflection helpers handle the guard transparently.** Helpers
like `is_pi`, `pi_dom`, `pi_cod_fn`, `is_universe`, `is_eq` are
public API and currently work on raw wait-values. After the split,
public types are guarded, so these helpers must look through guard:

```disp
unguard_or_self := {T} ->
  match (hasguard T) {
    TT => type_meta T
    FF => T
  }

is_pi       := {T} -> has_sig kernel_ref.pi (unguard_or_self T)
pi_dom      := {T} -> guard (pi_meta_domain (type_meta (unguard_or_self T)))
pi_cod_fn   := {T, x} -> guard ((pi_meta_cod_fn (type_meta (unguard_or_self T))) x)
is_universe := {T} -> has_sig kernel_ref.guarded_type (unguard_or_self T)
is_eq       := {T} -> has_sig kernel_ref.eq (unguard_or_self T)
```

`pi_dom` re-wraps the core domain in `guard` because public callers
expect `pi_dom (Pi Nat (...)) = Nat`, and `Nat` is the guarded form.
Hash-consing makes `guard core_Nat` and `Nat` identical, so the
existing test `pi_dom (Pi Nat ({_} -> Nat)) = Nat` keeps passing.

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

**Hard correctness requirement on the optimization, if added:**

1. The `contains_neutral` scan must run on **both** arguments before
   any fast-path call. A neutral in only one side still has to fail.
2. A neutral-containing argument must produce **`Fail`**, not
   `Ok FF`. Returning `Ok FF` would let user code distinguish
   "neutrals are unequal" from "neutrals can't be compared", which
   is itself a reflective channel — `select X Y (tree_eq n m)` could
   then branch on neutral structure even though no triage ever
   touches the neutral.
3. The fast path is only valid when both arguments pass the scan.
   The natural place to plug it in is at the top of
   `checked_apply f x` when `f = fork(fork(tree_eq_marker, a), nothing)`
   and `x` is the second argument: scan `a` and `x`; on success, call
   the runtime `treeEqual`; on either-side neutral, return `Fail`.

Promoting this paragraph from a residual risk to an inline contract
because the optimization is cheap to mis-implement and silently
unsoundifying when it goes wrong.

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
guard := {core} -> wait kernel_ref.guard core

hasguard := {T} -> has_sig kernel_ref.guard T

unguard_public := {T} ->
  match (hasguard T) {
    TT => type_meta T
    FF => InvalidType
  }

unguard_checked := {T} ->
  match (hasguard T) {
    TT => type_meta T
    FF => match (is_neutral T) {
      TT => T
      FF => InvalidType
    }
  }
```

`unguard_public` is for closed source-level boundaries: only
top-level guarded public types are accepted.

`unguard_checked` is for certified kernel handlers running under the
checked-evaluator invariant: either a guarded type or a certified
neutral type variable. A closed forged neutral cannot reach this
path because public guard entry rejects pre-existing neutrals and
raw checked evaluation cannot mint new ones.

The guard handler is the public unwrap point — the only handler
that returns bare `TT`/`FF` rather than `CheckedResult` (see
[Handler Return Conventions](#handler-return-conventions)):

```disp
q_guard_fn = {ks, raw, query} -> {self, core, v} ->
  // PUBLIC PREDICATE — returns BARE TT/FF.
  match (scan_no_neutral v) {
    TT => {
      // v passed entry scan: run the core predicate via checked_apply
      // and unwrap CheckedResult to bare TT/FF.
      let r = ks.checked_apply core v
      match (is_ok r) {
        TT => match (tree_eq (ok_value r) TT) {
          TT => TT
          FF => FF
        }
        FF => FF
      }
    }
    FF => FF
  }
```

So public raw-looking application still has the desired behavior:
`Nat v` means "scan `v` for closed forged neutrals, then
checked-evaluate the core Nat predicate on `v`, then unwrap to
bare TT/FF."

## Type Constructors

Keep the current mutually recursive kernel predicates as core
predicates, but export guarded public constructors:

```disp
// Cores — wait-values without guard. Used internally; never exposed
// as a "type" in user-facing code.
core_Nat   := wait kernel_ref.nat t
core_Bool  := wait kernel_ref.bool t
core_Eq    := {A, x, y} -> wait kernel_ref.eq (make_eq_meta A x y)
core_Pi    := {A, B} -> wait kernel_ref.pi (make_pi_meta A B)

// Public guarded constructors. unguard_checked accepts both guarded
// public types and certified neutral type variables, which lets
// closed parsed types and hypothesis-introduced types both work.
Nat   := guard core_Nat
Bool  := guard core_Bool
Eq    := {A, x, y} -> guard (core_Eq (unguard_checked A) x y)
Pi    := {A, B} -> guard (core_Pi (unguard_checked A) ({x} -> unguard_checked (B x)))
Arrow := {A, B} -> Pi A ({_} -> B)
```

Pi metadata stores the **core** domain and a codomain function
returning **cores**. This is what makes internal recursive type
checking work: when the Pi handler does
`ks.checked_apply expected result`, `expected` is a core type, so
dispatch goes directly to `q_eq_fn` / `q_nat_fn` / etc. — never
through `q_guard_fn`, whose entry scan would reject embedded
certified neutrals.

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
via a stored-type rank check.

```disp
q_core_type_fn = {ks, raw, query} -> fix ({self, rank, ty} ->
  match (neutral_root ty) {
    TT => {
      // ty is a certified neutral type variable.
      // Stored type must be `wait ks.core_type k` for some k ≤ rank.
      let stored = neutral_meta_type (type_meta ty)
      match (has_sig ks.core_type stored) {
        TT => match (nat_le (type_meta stored) rank) {
          TT => Ok TT
          FF => Ok FF
        }
        FF => Ok FF
      }
    }
    FF =>
      // Concrete dispatch on ty's signature. select_chain stays here;
      // each branch is `{ks, raw, query, self, rank, ty} -> body` — the
      // closed select-then-apply form (see q_pi_fn in current kernel).
      select_chain
        (case (has_sig ks.guarded_type ty)
          ({ks, raw, query, self, rank, ty} -> Ok (nat_lt (type_meta ty) rank)))
        (case (has_sig ks.core_type ty)
          ({ks, raw, query, self, rank, ty} -> Ok (nat_lt (type_meta ty) rank)))
        (case (has_sig ks.pi ty)
          ({ks, raw, query, self, rank, ty} -> {
            let tm = type_meta ty
            let dom = pi_meta_domain tm
            let codFn = pi_meta_cod_fn tm
            must_ok_tt (wait self rank dom) ({u} -> {
              let hyp = cert_make_hyp dom tm   // raw: mint
              must_ok_tt (wait self rank (codFn hyp)) ({u2} -> Ok TT)})}))
        (case (has_sig ks.eq ty)
          ({ks, raw, query, self, rank, ty} -> {
            let tm = type_meta ty
            let A = eq_meta_type tm
            must_ok_tt (wait self rank A) ({u} ->
            must_ok_tt (ks.checked_apply A (eq_meta_lhs tm)) ({u2} ->
            must_ok_tt (ks.checked_apply A (eq_meta_rhs tm)) ({u3} -> Ok TT)))}))
        (case (is_registered_base ks ty)
          ({ks, raw, query, self, rank, ty} -> Ok TT))
        ({ks, raw, query, self, rank, ty} -> Ok FF)
        ks raw query self rank ty
  })

is_registered_base := {ks, ty} ->
  select TT (tree_eq ty (wait ks.bool t)) (tree_eq ty (wait ks.nat t))
```

Note that `select_chain`'s case branches use **named-parameter
closed functions** (`{ks, raw, query, self, rank, ty} -> body`)
rather than `{_} -> body` thunks. This is the same pattern
`select_chain` already requires in the current `q_type_fn` (see
`lib/kernel.disp:198-205`): each case is a function whose
parameter list saturates its body, and the trailing
`ks raw query self rank ty` applies shared args to the chosen
branch.

```disp
q_guarded_type_fn = {ks, raw, query} -> {self, rank, T} ->
  match (neutral_root T) {
    TT => checked_h_rule self rank T
    FF =>
      // T must be guarded AND its core must be in core_type rank.
      match (hasguard T) {
        TT => {
          let core = type_meta T   // raw: kernel-recognized guard wait-value
          ks.checked_apply (wait ks.core_type rank) core
        }
        FF => Ok FF
      }
  }

Type n := guard (wait kernel_ref.guarded_type n)
```

Internal recursive universe checks must use `ks.checked_apply` on
core types, not the public `Type n`, so they can validate codomains
containing certified neutral type variables. The Pi case in
`q_core_type_fn` mints a fresh `cert_make_hyp` and recurses on the
codomain at that hypothesis — exactly the same pattern as the
existing `q_type_fn`.

## Pi Handler

Rewrite so all adversarial applications run through `checked_apply`.

`fresh_id pi_type` is the identity payload for the hypothesis. Today's
kernel uses the Pi metadata itself as identity (`lib/kernel.disp:105`:
`q_make_hyp raw (pi_meta_domain meta) meta`); hash-consing makes that
metadata stable per Pi binding. The plan uses the same definition,
just renamed:

```disp
fresh_id pi_type = type_meta pi_type
```

No counter is needed and no counter would be safe — the H-rule
reconstructs the checker's own type via `wait (ks query) meta`, so
`meta` is exactly the value that must remain hash-cons-stable across
the binding.

```disp
checked_pi_apply := {pi_type, v} ->
  match (neutral_root v) {
    TT => checked_h_rule pi_type v
    FF => {
      let domain = pi_meta_domain (type_meta pi_type)
      let codFn  = pi_meta_cod_fn (type_meta pi_type)
      let hyp    = cert_make_hyp domain (fresh_id pi_type)  // raw: kernel-minted neutral
      let result_r   = checked_apply v hyp
      let expected_r = checked_apply codFn hyp
      // Each continuation's parameter list saturates its body's free vars,
      // so each is a closed function (not a {_}-thunk capturing outer scope).
      must_ok_any result_r ({result} ->
        must_ok_any expected_r ({expected} ->
          match (neutral_root result) {
            // result is a certified neutral: compare its stored type
            // (kernel-internal data) against the codomain (also kernel-
            // built at this point). raw tree_eq is correct.
            TT => match (tree_eq expected (neutral_type result)) {
              TT => Ok TT
              FF => Ok FF
            }
            // result is concrete: run the codomain checker on it.
            FF => checked_apply expected result
          }))
    }
  }
```

The continuations `({result} -> ...)` and `({expected} -> ...)` use
their parameters in their bodies, so they're closed functions of
those parameters from the desugarer's view — `match` inside them
sees `result` / `expected` as already-bound names and only captures
what's still free. This is the well-formed CPS pattern:
continuations always have a real parameter (not `_`), and `match`
handles the conditional branching.

`cert_make_hyp` is trusted code that constructs a neutral root via
raw `apply` (it expands to `wait kernel.hyp_reduce ...`, which the
runtime reduces to a fork — uninterceptable from inside
`checked_apply` because handler bodies don't dispatch through it).

Note that the inner `checked_apply expected result` returns
`CheckedResult`, which is exactly what the outer `must_ok_any`
chain expects — the predicate's `Ok TT` / `Ok FF` propagates
naturally.

## Hyp Reduce Handler

The certified interpreter for applying a neutral to an argument:

```disp
q_hyp_reduce_fn = {ks, raw, query} -> fix ({self, meta, arg} -> {
  let current_type = neutral_meta_type meta   // raw: kernel-built meta
  match (has_sig ks.pi current_type) {
    TT => {
      // current_type has Pi signature: extend with codFn(arg).
      let codFn = pi_meta_cod_fn (type_meta current_type)
      must_ok_any (ks.checked_apply codFn arg) ({result_type} ->
        Ok (cert_make_neutral                  // raw: mint
          result_type
          (neutral_app_payload meta arg)))
    }
    FF =>
      // current_type isn't Pi: result has InvalidType.
      Ok (cert_make_neutral InvalidType
        (neutral_app_payload meta arg))
  }
})
```

`current_type` is the neutral's stored type, supplied by
`cert_make_hyp` (originally a public Pi or core type) or computed by
a previous step (kernel-controlled). It never originates from raw
user code, so reading it via raw helpers is safe.

`neutral_app_payload meta arg = t meta arg` is the existing helper
from `lib/kernel.disp:34`. The payload preserves identity across the
spine: `f 0` and `f 1` remain different neutrals even when both have
type `Nat`.

## Nat, Bool, Eq Handlers

All three follow the same shape: H-rule on neutral candidate;
concrete dispatch otherwise; recursive sub-checks via `ks.checked_apply`.

```disp
q_nat_fn = {ks, raw, query} -> fix ({self, meta, n} ->
  match (neutral_root n) {
    TT => checked_h_rule self meta n
    FF =>
      // Concrete dispatch: leaf=zero | fork(t, n')=succ | else=Ok FF
      match (tree_eq n t) {
        TT => Ok TT                                       // n = leaf (zero)
        FF => match (is_fork n) {
          TT => match (tree_eq (pair_fst n) t) {
            TT => wait self meta (pair_snd n)             // recurse on predecessor
            FF => Ok FF
          }
          FF => Ok FF
        }
      }
  })
```

```disp
q_bool_fn = {ks, raw, query} -> {self, meta, b} ->
  match (neutral_root b) {
    TT => checked_h_rule self meta b
    FF => match (tree_eq b TT) {
      TT => Ok TT                                         // b = TT (leaf)
      FF => match (tree_eq b FF) {
        TT => Ok TT                                       // b = FF (stem of leaf)
        FF => Ok FF
      }
    }
  }
```

```disp
q_eq_fn = {ks, raw, query} -> {self, meta, p} ->
  match (neutral_root p) {
    TT => checked_h_rule self meta p
    FF =>
      // p must be refl (leaf). Check stored lhs == rhs structurally.
      match (tree_eq p t) {
        TT => match (tree_eq (eq_meta_lhs meta) (eq_meta_rhs meta)) {
          TT => Ok TT
          FF => Ok FF
        }
        FF => Ok FF
      }
  }
```

The `tree_eq (eq_meta_lhs meta) (eq_meta_rhs meta)` call merits a
note. The Eq metadata stores `lhs` and `rhs` as user-supplied terms.
At the public boundary, the guard's `scan_no_neutral` step has
already verified those terms contain no forged neutrals. During
internal Pi/Type checking, they may legitimately contain certified
neutrals (e.g., `Pi Nat ({n} -> Eq Nat n n)`). Raw `tree_eq` (the
runtime hash-cons fast path) is correct for both: structurally
identical trees with embedded certified neutrals share hash-cons
identity exactly when their neutrals do, which is exactly when they
are the same dependent term.

## Certified Eliminators

### Why eliminators take an explicit `rank` argument

In Coq/Lean/Agda, `Bool.rec` is a primitive constant of the
metalanguage with a fixed typing rule:

```
Bool.rec : ∀ {u} (motive : Bool → Sort u),
           motive true → motive false → ∀ (b : Bool), motive b
```

When the kernel sees `Bool.rec @M @t @f @b`, it knows `Bool.rec`'s
type and validates each argument bidirectionally — `M` against
`Bool → Sort u`, `t` against `M true`, etc. The motive's type comes
from the eliminator's signature (metalanguage data), not from the
motive value itself.

Disp's kernel is a tree program. There's no metalanguage that "knows"
`bool_rec` is an eliminator with a special typing rule. A user-facing
`bool_rec` is just a tree function; the kernel sees it as data. When
a Pi check evaluates `bool_rec motive t f h` (with `h` a hypothesis),
the body runs as plain tree-calculus reduction — the kernel never
inspects `motive` against an expected `Bool → Type n`, because there
is no inference machinery to do so.

This means the motive's type **must** be made available at call time,
or the kernel cannot validate it. The simplest sound design: have the
user pass `rank` (a Nat) explicitly, and let the kernel construct
the motive's expected type internally:

```disp
motiveT_internal = core_Pi core_Bool ({_} -> wait ks.core_type rank)
```

The kernel then validates `motive : motiveT_internal` via the standard
Pi check. No user-supplied `motiveT` is ever trusted — `rank` is the
only piece of "type-shape" data the user provides, and it's a Nat
that's trivially validated.

This is the only way disp can recover the soundness property that
Coq/Lean/Agda get from bidirectional inference at the metalanguage
level. The price is one extra runtime argument per eliminator call.

### Metadata layout (5-tuple for `bool_rec`)

```disp
make_bool_rec_meta := {rank, motive, t_case, f_case, target} ->
  t rank (t motive (t t_case (t f_case target)))

bool_rec_meta_rank   := {m} -> pair_fst m
bool_rec_meta_motive := {m} -> pair_fst (pair_snd m)
bool_rec_meta_t_case := {m} -> pair_fst (pair_snd (pair_snd m))
bool_rec_meta_f_case := {m} -> pair_fst (pair_snd (pair_snd (pair_snd m)))
bool_rec_meta_target := {m} -> pair_snd (pair_snd (pair_snd (pair_snd m)))
```

`nat_rec` and `eq_J` follow the same nesting pattern with their own
field counts (`rank, motive, base, step, target` for `nat_rec`;
`rank, A, x, motive, base, y, p` for `eq_J`).

### User-facing wrapper

```disp
bool_rec := {rank, motive, t_case, f_case, target} ->
  unwrap_value
    ((wait kernel_ref.bool_rec
        (make_bool_rec_meta rank motive t_case f_case target)) t)
```

The trailing `t` is the trigger arg; the handler ignores it and
dispatches on `meta`. The `unwrap_value` wrapper turns the
handler's `CheckedResult` into a bare value (or `elim_fail`) for
user-facing use.

### Handler

Returns `CheckedResult`; `must_ok_tt` for predicate validation,
`must_ok_any` to thread a sub-evaluation result:

```disp
q_bool_rec_fn = {ks, raw, query} -> {meta, _trigger} -> {
  let rank   = bool_rec_meta_rank meta     // raw: kernel-built meta
  let motive = bool_rec_meta_motive meta
  let t_case = bool_rec_meta_t_case meta
  let f_case = bool_rec_meta_f_case meta
  let target = bool_rec_meta_target meta

  // CPS continuations all take a real parameter (named or unit-via-`{u} ->`)
  // — never `{_} -> body_using_outer_vars`. That keeps each continuation a
  // non-K abstraction whose body fires only when the continuation is invoked.

  // (1) rank is a Nat
  must_ok_tt (ks.checked_apply core_Nat rank) ({u0} -> {

  // (2) Construct motive's expected type from rank.
  //     This is kernel-trusted construction — uses cores throughout so that
  //     motive's body may legitimately contain certified neutral type vars
  //     when bool_rec is invoked under a Pi check. Routing motive through
  //     a guarded Pi here would trigger q_guard_fn's entry scan and reject
  //     valid hypothesis-typed motives.
  let core_motiveT = core_Pi core_Bool ({_} -> wait ks.core_type rank)

  // (3) target is a Bool
  must_ok_tt (ks.checked_apply core_Bool target) ({u1} ->

  // (4) motive : motiveT_internal
  must_ok_tt (ks.checked_apply core_motiveT motive) ({u2} ->

  // (5) branches inhabit motive at TT and FF
  must_ok_any (ks.checked_apply motive TT) ({tT_type} ->
  must_ok_any (ks.checked_apply motive FF) ({fT_type} ->
  must_ok_tt  (ks.checked_apply tT_type t_case) ({u3} ->
  must_ok_tt  (ks.checked_apply fT_type f_case) ({u4} ->

  // (6) dispatch on a target that has now been validated as a Bool
  //     (so it is TT, FF, or a certified neutral with stored type Bool)
  match (tree_eq target TT) {
    TT => Ok t_case
    FF => match (tree_eq target FF) {
      TT => Ok f_case
      FF =>
        // certified-neutral case
        must_ok_any (ks.checked_apply motive target) ({raw_result_type} -> {
          // motive : Bool -> Type rank produces a GUARDED type (every
          // user-accessible type constructor wraps its result in `guard`).
          // Stuck-elim neutrals must store CORE forms because the kernel's
          // H-rule reconstructs cores via `wait (ks query) meta`; a guarded
          // stored type would never hash-cons-match the H-rule's core
          // reconstruction.
          //
          // unguard_checked also accepts certified neutral type variables
          // for the case where motive's result is itself a stuck neutral
          // type (e.g., a motive that branches on target via dep elim).
          let result_type = unguard_checked raw_result_type
          must_ok_tt
            (ks.checked_apply (wait ks.core_type rank) result_type)
            ({u5} -> Ok (cert_make_neutral result_type target))})
    }
  }
  )))))))})
}
```

### Five things to call out

1. **`rank` is the only "type-shape" data the user supplies.** Step
   (1) validates it as a Nat. The kernel then *constructs* the
   motive's expected type from `rank` (step 2) — there is no
   user-supplied `motiveT` to verify. This dodges a regress: every
   piece of trusted type structure either grounds out in primitive
   kernel checks (Nat, Bool, Type, Pi) or is kernel-built.
2. **`unguard_checked` on the motive result is load-bearing.**
   Without it, every legitimate stuck eliminator fails downstream
   H-rule checks: the kernel reconstructs core forms (`wait (ks query) meta`),
   but the stored type from a user motive is guarded. The unguard
   step is the eliminator-side counterpart to "Pi metadata stores
   cores" — both keep internal kernel data uniformly in core form.
3. `must_ok_tt` vs `must_ok_any` — every predicate check uses
   `must_ok_tt` (an `Ok FF` is a validation failure that must
   propagate as `Fail`); every "compute a sub-result and use it" uses
   `must_ok_any`. Mixing these up silently accepts `Ok FF` from a
   predicate, defeating validation.
4. `wait ks.core_type rank`, **not** `Type rank`. Public `Type rank`
   is guarded; calling it on a `result_type` that contains a
   certified neutral type variable would be rejected by the entry
   scan inside `q_guard_fn`. Internal universe checks must use
   `core_type` directly. (See [Universes](#universes).)
5. Both `tree_eq target TT` and `tree_eq target FF` happen *after*
   the Bool validation in step (3). Comparing `target` to a known
   constant via raw `tree_eq` is safe here because step (3) ruled
   out the case where `target` would carry an unobservable neutral.

### Other eliminators

`nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong` follow the same
pattern:

1. Validate `rank` as a Nat (step 1 above).
2. Build the motive's expected core type internally from `rank` and
   the eliminator's target type (e.g., `core_Pi core_Nat ({_} -> wait ks.core_type rank)`
   for `nat_rec`).
3. Validate `target` against its expected type.
4. Validate `motive` against the core motive type.
5. Validate each branch against `motive` applied at the relevant
   constructor case (e.g., `motive 0` for `nat_rec`'s base case;
   `motive (succ pred)` under a hypothesis `pred : Nat` for the
   step case).
6. `unguard_checked` the motive's result before storing in the
   stuck neutral.
7. Validate the (now core) result type via `wait ks.core_type rank`.
8. Mint or dispatch.

For `nat_rec`'s step case the validation creates a `pred : Nat`
hypothesis, then validates `step : Pi Nat ({pred} -> Pi (motive pred) ({ih} -> motive (succ pred)))`
or the equivalent core form. This propagates the same "construct the
expected type from `rank`" discipline.

### Removing the raw versions

The current raw versions in `lib/kernel.disp:283-347` construct
`StuckElim` directly and must be deleted as part of the eliminator
rewrite. Under `checked_apply`, those raw lambdas would be rejected
because the inner `wait kernel.hyp_reduce ...` constructor step
produces a neutral root and there is no certified-handler dispatch
covering them.

### Optional helper: `make_recursor`

All eliminator handlers share a common shape (validate `rank`,
construct motiveT from rank, validate target/motive/branches, then
mint or dispatch). Once two or three are written, factor out a
combinator that takes the target's type, a list of constructor case
descriptors (each describing the constructor's argument types and
the recursive-call structure for the step branch), and the
eliminator's wait-handler signature, and generates the certified
handler. This collapses near-duplicate validators into one parametric
one and removes a class of "I forgot to validate motive at this
branch" bugs. Not blocking the initial implementation; clean up once
the pattern is stable.

Note: this combinator stops short of *user-extensibility* —
exposing it publicly would let user code construct eliminator
handlers, which requires a controlled `cert_make_neutral` capability
that the closed-kernel soundness fix does not provide. User-extensible
eliminators are explicitly out of scope (see
[`KERNEL_EXTENSIBILITY_PLAN.md`](KERNEL_EXTENSIBILITY_PLAN.md)).

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

Items marked **(landed)** are already in `main` from earlier passes
in this plan's lifecycle. Items marked **(partial)** have some work
landed; the remainder is described in their sections.

0. **(landed) `match` syntax in the parser/compiler.** Keyword
   `match`, punct `=>`, AST node `{ tag: "match"; cond; thenBody;
   elseBody }`. Desugarer in `compile.ts` lifts each arm to a closed
   function (parameter list = union of free vars across arms,
   computed at Cir level), emits select-then-apply. Verified by
   `lib/match.test.disp` (short-circuit semantics, including the
   `loops y` divergence test that rejects naive thunk encodings).
1. **(landed) `is_neutral` tightened** to require fork shape. Both
   `q_is_neutral` (kernel-internal) and the public `is_neutral` now
   conjoin `is_fork`. Regression test in `lib/kernel.test.disp`:
   `is_neutral (t hyp_reduce_sig) = FF`.
2. **(partial) `tree_eq` rename + canonical `tree_eq_spec`.**
   Cosmetic rename across `.disp`/`.ts` complete. `tree_eq_spec`
   added to `lib/prelude.disp` as the canonical recursive form.
   Cross-check in `lib/tree_eq.test.disp` asserts agreement with
   the host-optimized `tree_eq`. **Remaining**: replace the
   `TREE_EQ_MARKER` sentinel with a runtime-captured id of the
   canonical tree (see [`tree_eq` section](#tree_eq-rename-from-fast_eq-landed)
   for the open design question — the literal `fork(fork(tree_eq_id, _), _)`
   recognition pattern doesn't naturally fire on real spec calls).
3. **Result encoding**: `Ok`, `Fail`, `is_ok`, `ok_value`,
   `must_ok_any` (sub-evaluation must not fail; payload may be any
   tree), `must_ok_tt` (predicate evaluation must succeed AND return
   `TT`). See [Checked Apply](#checked-apply) for definitions; do not
   collapse into a single `must_ok` helper. Both helpers use `match`
   internally; CPS continuations passed to them must take a real
   parameter (not `_`) so they're non-K abstractions.
4. **Trusted helpers**: `neutral_root`, `contains_neutral`,
   `scan_no_neutral`.
5. **`checked_apply`** as a TC interpreter with all rules above,
   added to the kernel `recq` record alongside the existing
   handlers (see [Kernel Record Shape](#kernel-record-shape)).
   Define `cert_make_neutral` / `cert_make_hyp` / `cert_make_stuck`
   as private kernel helpers (not bound to any user-reachable name)
   and `checked_h_rule` / `fresh_id` per their definitions above.
   Recognize the canonical `I` via `tree_eq f I_canonical` (the
   runtime fast path keeps this O(1)). `tree_eq` itself does *not*
   need explicit recognition in `checked_apply` — its TC reduction
   handles correctness via the triage-on-neutral rule. **Spec
   cleanup needed**: pseudocode like `let cx = checked_apply c x` in
   the S/triage rules elides the `ok_value` unwrap; `cx` must be
   `ok_value`-extracted before being fed back as a tree value.
6. **Guard API**: `guard`, `hasguard`, `unguard_public`,
   `unguard_checked`.
7. **Split `type` into `core_type` and `guarded_type`**.
8. **Rewrite public constructors**: `core_X` cores plus guarded
   `Nat`, `Bool`, `Eq`, `Pi`, `Arrow`, `Type`. Update reflection
   helpers (`is_pi`, `pi_dom`, `pi_cod_fn`, `is_universe`, `is_eq`)
   to look through guard via `unguard_or_self` so existing
   reflection tests keep passing.
9. **Rewrite handlers**: `q_guard_fn`, `q_pi_fn`, `q_hyp_reduce_fn`,
   `q_nat_fn`, `q_bool_fn`, `q_eq_fn`, `q_core_type_fn`,
   `q_guarded_type_fn`, `q_checked_apply_fn` to use checked
   application at every adversarial boundary. Concrete sketches in
   the "Hyp Reduce / Nat, Bool, Eq / Universes / Pi Handler /
   Certified Dispatch" sections. `q_guard_fn` is the only handler
   that returns bare `TT`/`FF`; all others return `CheckedResult`.
10. **Replace raw eliminators** with certified wait handlers:
    `bool_rec`, `nat_rec`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`.
    Each takes an explicit `rank : Nat` first argument so the kernel
    can construct the motive's expected type internally (see
    [Certified Eliminators](#certified-eliminators) for the
    rationale). User-facing wrapper supplies a trigger arg (`t`)
    and unwraps the resulting `CheckedResult` via `unwrap_value`.
    Add `make_X_meta` / `X_meta_field` helpers per the eliminator
    section.
11. **Remove `Hyp` / `StuckElim`** from the public API (or rename to
    `unsafe_*`).
12. **Update the elaborator** so typed annotations are checked by
    public `Type n`, terms by public guarded predicates. The
    elaborator owns no trusted predicate table. The elaborator must
    also synthesize the explicit `rank` argument when emitting calls
    to typed eliminators (a one-line addition, since the elaborator
    already tracks universe levels).
13. **Rewrite kernel tests using raw `Hyp` / `StuckElim`.** The
    existing tests at `lib/kernel.test.disp:80-97` (and similar)
    construct stuck eliminators on raw hypotheses such as
    `Hyp Bool 0`. Once `Hyp` is removed (or renamed `unsafe_*`), the
    legitimate way to obtain a hypothesis is through a Pi-context.
    Rewrite these tests to exercise the certified eliminators on
    hypotheses minted by `Pi`/`Type` checking. Note: existing test
    calls like `bool_rec ({_} -> Nat) 1 0 TT` need an extra `rank`
    argument: `bool_rec 0 ({_} -> Nat) 1 0 TT`. Expect a noisy test
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
  bool_rec 0 ({_} -> Eq Nat 0 1) 0 0 b
// expected: bool_rec validation requires t_case : motive(TT) = Eq Nat 0 1
// and f_case : motive(FF) = Eq Nat 0 1. No closed inhabitants exist;
// validation fails.
```

**Reject malformed motive (not Bool → Type rank):**

```disp
// motive returns Nat values, not types. The expected motive type
// constructed from rank=0 is core_Pi core_Bool ({_} -> wait ks.core_type 0),
// and ks.checked_apply on that against a Nat-returning function fails.
let bad_motive : Bool -> Nat = {b} -> bool_rec 0 ({_} -> 5) 1 0 b
// expected: bool_rec rejects — motive validation step (4) fails.
```

**Reject mis-ranked motive:**

```disp
// motive returns Type 1 values, but rank=0 is requested. The motive
// validation step ks.checked_apply (core_Pi core_Bool ({_} -> wait ks.core_type 0)) motive
// fails because Type 1 doesn't lie in core_type 0.
let bad_rank : Bool -> Type 1 = {b} -> bool_rec 0 ({_} -> Type 1) Nat Bool b
// expected: bool_rec rejects.
```

**Tightened `is_neutral` rejects stems** *(landed —
`lib/kernel.test.disp` regression test):*

```disp
test is_neutral (t hyp_reduce_sig) = FF   // post-tightening: stems aren't neutrals
test is_neutral (cert_make_neutral t t) = TT  // legitimate fork-shape neutral (kernel-internal)
```

**`tree_eq` cross-check** *(landed — `lib/tree_eq.test.disp`):*

```disp
let agrees = {a, b} -> tree_eq (tree_eq a b) (tree_eq_spec a b)
test agrees t t = TT
test agrees (t t t) (t (t t) t) = TT
// ... full panel covering leaves, stems, forks, Nat-shaped trees.
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

**Accept certified eliminators** (note the explicit `rank` arg):

```disp
let bool_to_nat : Bool -> Nat = {b} -> bool_rec 0 ({_} -> Nat) 1 0 b
let nat_to_bool : Nat -> Bool = {n} ->
  nat_rec 0 ({_} -> Bool) TT ({_k, _prev} -> FF) n
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

The 10-50× estimate assumes user code dominated by ordinary apply
steps. The picture is worse for code dominated by `tree_eq` calls:
without the fast-path optimization, every user-mode `tree_eq a b`
runs the full TC recursion, which is O(min(|a|, |b|)) tree steps
per call — versus O(1) hash-cons identity in the runtime. Once
`Eq`-heavy proofs land that compare large terms (e.g., normalized
`add` results), the optional fast path moves from "future
optimization" to "probably necessary" in practice. Implementers
should plan to enable it shortly after the simple path lands,
subject to the correctness contract spelled out in
[`tree_eq` in Checked Mode](#tree_eq-in-checked-mode).

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

## Bootstrap and Signature Conventions

### Record shape

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
  checked_apply         // new — the TC interpreter itself
}
```

### Bootstrap

```disp
kernel := recq {
  hyp_reduce    := q_hyp_reduce_fn;
  guard         := q_guard_fn;
  pi            := q_pi_fn;
  nat           := q_nat_fn;
  bool          := q_bool_fn;
  eq            := q_eq_fn;
  core_type     := q_core_type_fn;
  guarded_type  := q_guarded_type_fn;
  bool_rec      := q_bool_rec_fn;
  nat_rec       := q_nat_rec_fn;
  eq_J          := q_eq_J_fn;
  checked_apply := q_checked_apply_fn
}

kernel_ref := {q} -> wait kernel q
checked_apply := kernel.checked_apply       // public re-export
```

`recq` makes ordering inside the record body irrelevant. Handler
bodies receive `(ks, raw, query)` and reference other fields via
`ks.field`, which evaluates to `wait self field_query` — inert until
applied. The cycle is resolved on demand:

- `q_checked_apply_fn` references `ks.pi`, `ks.nat`, `ks.bool`, … to
  compute their signatures during dispatch.
- `q_pi_fn`, `q_nat_fn`, … reference `ks.checked_apply` to validate
  user-supplied subterms.
- `q_hyp_reduce_fn` references `ks.pi` to test whether the neutral's
  stored type is a Pi.

None of these references force evaluation at recq-build time; each
fires only when a real argument arrives.

### `raw` vs `ks.field` for signatures

Two different signature shapes exist in the kernel. Picking the
wrong one for a dispatch case silently mismatches every value of
that family.

| Constructor                      | Signature used by checker      | Use which inside handlers |
|----------------------------------|--------------------------------|---------------------------|
| `Hyp ty id` / `StuckElim r tg`   | `kernel.hyp_reduce` (raw)      | `raw.hyp_reduce`          |
| `Pi A B` / `core_Pi A B`         | `kernel_ref.pi` (proxy)        | `ks.pi`                   |
| `Nat`, `Bool`, `Eq`, `Type n`    | `kernel_ref.X` (proxy)         | `ks.X`                    |
| eliminator wait-values           | `kernel_ref.X_rec` (proxy)     | `ks.X_rec`                |

Hypotheses must store the `raw` (actual recq-resolved) handler
identity because `checked_hyp_reduce_apply` IS the runtime that fires
when neutrals are applied — there is no proxy indirection at that
layer. Type formers and eliminators all use the proxy form; the
public constructors uniformly write `wait kernel_ref.X meta`, so
inside handlers the matching projection is `ks.X` (which has the
same hash-cons identity).

This is exactly the convention the existing kernel already uses
(`q_is_neutral` uses `raw`; `is_pi`/`is_universe`/`is_eq` use
`kernel_ref`) — the new dispatch table just makes the same two
flavors explicit.

### Worked dispatch example

`(Pi Nat ({_} -> Nat)) ({x} -> x)`:

1. Raw apply on the outer Pi value. `Pi Nat (…)` is `guard (wait
   kernel_ref.pi (make_pi_meta core_Nat (…)))`. Outermost is `guard`
   wait-value; raw apply fires `q_guard_fn` with `(core, v)`, where
   `core = wait kernel_ref.pi …` and `v = ({x} -> x)`.
2. `q_guard_fn` runs `scan_no_neutral v` (passes — no embedded
   neutrals), then `ks.checked_apply core v`.
3. `q_checked_apply_fn` checks signatures. `pair_fst core =
   stem(wait kernel pi_query)`. `pair_fst (wait ks.pi t) =
   stem(wait self pi_query)` = same id (kernel ≡ self under recq).
   `has_sig ks.pi core = TT`. Dispatch fires: just `core v` via raw
   apply.
4. `core v` reduces via raw apply: `(wait kernel_ref.pi meta) v` =
   `kernel_ref.pi meta v`. recq projection invokes `q_pi_fn` with
   recq's wrap args, then applies `(meta, v)`. Handler runs.
5. Handler mints `cert_make_hyp core_Nat meta`, validates body,
   returns `Ok TT`.
6. Back in `q_guard_fn`, `is_ok r = TT` and `ok_value r = TT`, so
   the unwrap returns bare `TT`. Test sees `TT`.

Every cross-field call is `f x` (raw apply); `recq` guarantees
field selection produces hash-cons-stable trees so signatures match.

### Eliminators as standalone derivations

`eq_subst`, `eq_sym`, `eq_cong` either become additional certified
handlers in the kernel or are derived combinators built on top of
`eq_J`. The latter is preferable if it simplifies the perimeter:
fewer dispatch-table entries means a smaller security review surface.

## Out of Scope

This plan keeps the kernel record closed:
`{hyp_reduce, pi, nat, bool, eq, type, ...}`. Lifting it to a
parametric kernel where users can register new type handlers is a
separate refactor — see [`KERNEL_EXTENSIBILITY_PLAN.md`](KERNEL_EXTENSIBILITY_PLAN.md).
The order is: land soundness on the closed kernel first (with the
dispatch-table contract written down), then extend.
