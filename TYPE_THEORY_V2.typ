#set document(title: "Disp Type Theory")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#let note(body) = block(
  breakable: false,
  above: 0.8em,
  below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  body,
)

#align(center, text(22pt, weight: "bold")[Disp Type Theory])
#v(0.3em)
#align(center)[
  Authoritative reference for the Disp object-language semantics.\
  Companion: #raw("TYPE_THEORY_INTRO.typ") (motivational walkthrough).\
  Implementation: #raw("lib/kernel.disp"). Idioms: #raw("KERNEL_DESIGN.md").
]
#v(1em)

#note[
  *Status.* This document specifies the data-as-eliminator design:
  inductive types are encoded such that values are their own
  eliminators, and the existing Pi-application infrastructure
  (`hyp_reduce`) handles all stuck-on-hypothesis behaviour
  uniformly. Some sections describe behaviour not yet in `main`;
  the implementation tracking is in #raw("IMPLEMENTATION_PLAN.md").
  Where this document and the reference implementation disagree,
  this document is authoritative and the implementation is a bug.
]

= Overview

Disp is a dependently-typed language built on tree calculus. There
is no built-in type system, no built-in checker, no built-in
notion of "well-formed term." All of those are tree programs that
the runtime executes via a single primitive operation, `apply`.

The language commits to five interlocking ideas:

+ *Types as predicates.* A type is a function from values to
  `TT`/`FF`. Type checking is function application:
  `T(v) = TT` means "$v$ has type $T$."

+ *Wait-encoded types.* A type is structurally
  `wait(checker)(metadata)`. The checker's hash-cons identity is
  the type-former tag; the metadata carries parameters.

+ *Hypotheses carry their types.* Open terms (variables under a
  binder) are represented as opaque "neutral" trees that store
  their type internally. Type-checking under a binder reduces to
  hash-cons-identity comparison of stored types against expected
  types --- the *H-rule*.

+ *Parametric checked evaluation.* User-supplied terms are
  evaluated under a non-standard reduction discipline that bans
  reflection on hypotheses (`triage` on a neutral fails) and
  forging of hypothesis tokens (`stem` rule rejects construction
  of fork-shaped values with the kernel's hypothesis signature).

+ *Data is its own eliminator.* Every inductive type (Bool, Nat,
  Eq, Ord) is encoded such that constructor values *are*
  functions that dispatch on their case-handlers. Eliminators
  (`bool_rec`, `nat_rec`, `eq_J`, `ord_rec`) are identity-applied
  to their target. Hypothesis-handling falls out for free:
  applying a hypothesis-typed value to case-handlers fires
  `hyp_reduce`, which extends the spine and updates the stored
  type. There are no special "stuck eliminator" sentinels.

The first three give a usable type system. The fourth closes
soundness holes that ordinary tree-calculus reduction would leave
open. The fifth is what makes the kernel small: every "stuck on
a hypothesis" case --- function application, eliminator on a
hypothesis target, ordinal operation on a polymorphic rank --- is
the same mechanism (Pi-application against a neutral, mediated by
`hyp_reduce`).

== Soundness, not completeness <soundness-not-completeness>

The kernel guarantees *soundness*: if a test `T(v) = TT` passes,
then `v` genuinely inhabits `T` per the semantics. The kernel
does NOT guarantee *completeness*: many true type-membership
facts may not be derivable as `TT`, because tree-calculus
predicates are arbitrary computation and many meta-properties
(rank computation for polymorphic types, supremum reasoning,
function equivalence) are uncomputable in general.

The programmer takes responsibility for *definability*. If you
write a type whose membership test:

- *Diverges* on some input: the runtime exhausts apply budget
  and fails. Test reports `FF` (or runtime error).
- *Returns stuck* (because some operation depends on a
  hypothesis): the test fails the `= TT` assertion at the public
  boundary. Instantiate and re-test.
- *Returns FF* concretely: the test fails normally.

In every case the failure mode is "this test does not pass," not
"this unsound term type-checked." Soundness is preserved across
all of these.

This stance simplifies a lot:
- *Polymorphic-rank types* have stuck `pi_rank`. That is
  correct: the kernel cannot decide "supremum over Ord of
  $alpha + 1$ is $omega$," and it does not need to. Polymorphic
  library code is testable at concrete instantiations.
- *Subject reduction* for closed terms follows from tree
  calculus confluence. No elaborate machinery needed.
- *Bounded-level systems* (TTBFL-style) and *normal-function
  recognition* (sup-reasoning heuristics) are not needed. Stuck
  propagation covers what they would.
- *User-defined predicates* can be arbitrarily complex. The
  kernel makes no demand that they be decidable; only that they
  return `TT`/`FF`/`Fail`/stuck-Bool correctly.

The contract is: *if your type's predicate eventually says `TT`,
the value really does inhabit it*. What the predicate does
internally --- iterate, defer, or refuse to terminate --- is the
programmer's problem.

= Framework

== Two reduction modes

The runtime evaluator runs a single `apply` operation. Each
application chooses one of two reduction modes:

- *Raw mode.* Standard tree-calculus reduction. Triage is full
  reflection (it can match on any tree, including neutrals). Used
  for trusted code: the kernel's own handler bodies, kernel-built
  metadata operations, ordinary user code outside any type-check
  context.

- *Parametric mode* (also called *checked* or *walker* mode).
  Tree-calculus reduction with two restrictions:
  + *Triage on a neutral fails.* If the value being triaged on is
    a kernel-minted hypothesis, the application fails (returns
    `Fail` rather than reducing).
  + *Fork-formation rejecting neutral roots.* If the `stem` rule
    would produce a fork whose left component is the canonical
    hypothesis-handler signature, the application fails.

Parametric mode encodes operationally what parametricity says
declaratively: hypotheses are opaque tokens for universal
quantification, and user code (a) cannot inspect their structure
and (b) cannot synthesize new ones indistinguishable from
kernel-minted ones.

The dispatcher (`q_checked_apply_fn`) selects between the two modes
per application by inspecting the function being applied:
kernel-recognized signatures route through raw apply; everything
else runs the parametric rules.

#note[
  Rule (b) is enforced at *one* point: the `stem` rule's
  constructor check. The S rule and triage-fork rule do not
  perform direct constructor checks --- they delegate to recursive
  `checked_apply` calls, where the inner step that ultimately
  constructs the fork hits the stem-rule check. So rule (b) is
  one explicit check, not three.
]

=== Soundness carve-out vs performance carve-out <soundness-carve-out>

A *soundness carve-out* is a parametric-mode rule that exists
specifically to make the type system *expressible*. The only one
in the framework is the *I-shortcut*: $"apply"(I, x) -> x$,
special-cased because $I = "fork(fork(LEAF, LEAF), LEAF)"$ is
structurally a triage shape and a strict triage-on-neutral rule
would reject identity on a hypothesis. Polymorphic identity is a
legitimate program, so the walker carves out exactly this case.

A *performance carve-out* is a runtime fast path that produces
*observably identical* results to the in-language reduction.
`tree_eq` uses one (the host captures its compiled tree id and
short-circuits via hash-cons identity). A future "native walker"
in `tree.ts` would be another.

== Trusted primitives

A *trusted primitive* is a four-tuple `(signature, checker,
eliminator, identity)` registered with the framework:

- *signature* --- a hash-cons-stable tree id used by the
  dispatcher to recognize "this is one of mine, run in raw mode."
- *checker* --- the kernel handler that validates a value as
  belonging to this type. Runs in raw mode after dispatch.
- *eliminator* --- where applicable, the user-callable interface
  for case-analysis. For data-as-eliminator types (Bool, Nat, Eq,
  Ord), eliminators are library-level identity-applied-to-target
  functions; the kernel does not register them. For Pi, the
  eliminator is `apply` itself.
- *identity* --- the kernel-private mint capability for
  constructing canonical instances or hypothesis tokens.

The registry is closed at boot time. User-extensible registration
is out of scope for this version of the kernel; see
#raw("KERNEL_EXTENSIBILITY_PLAN.md").

#figure(
  table(
    columns: 4,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Primitive*], [*Signature*], [*Checker*], [*Eliminator*],
    [Pi],            [`kernel.pi`],          [`q_pi_fn`],            [`apply` (built-in)],
    [Type (core)],   [`kernel.core_type`],   [`q_core_type_fn`],     [`apply` (predicate role)],
    [Type (guarded)],[`kernel.guarded_type`],[`q_guarded_type_fn`],  [`apply` (predicate role)],
    [Hypothesis],    [`kernel.hyp_reduce`],  [`q_hyp_reduce_fn`],    [(none --- kernel-only)],
    [Guard layer],   [`kernel.guard`],       [`q_guard_fn`],         [(boundary, not user-side)],
    [Ord comparisons],[`kernel.ord_lt`, `kernel.ord_le`, `kernel.ord_max`], [primitives], [(none --- direct invocation)],
  ),
  caption: [Trusted primitives in the kernel registry. Note: Bool,
  Nat, Eq, Ord, OrdLt do NOT appear here as separate kernel
  primitives. They are library-level types defined via data-as-
  eliminator (see §4). Their per-value templates are literal Pi
  expressions (recq encoding), so the kernel recognises them via
  the existing Pi machinery without separate registration.],
)

The dispatcher is a fold over this registry: for each entry, check
`has_sig entry.sig f`; on match, route through raw apply; otherwise
apply the parametric rules.

=== Reflection via `has_sig` <reflection-apis>

Reflection on type values is done via `has_sig`:

```disp
has_sig := {checker, v} -> tree_eq (pair_fst v) (checker_sig checker)
```

`has_sig` is *inherently parametricity-safe*: `pair_fst` is
implemented via `triage`, so applying `has_sig sig v` to a
hypothesis-typed `v` runs `triage` on the hypothesis, which
the walker rejects (the triage-on-neutral rule). The walker's
existing parametric mode catches the reflection attempt.

User-facing reflection helpers are thin wrappers:

```disp
is_pi  := {v} -> has_sig kernel_ref.pi v
is_eq  := {v} -> has_sig kernel_ref.eq v
is_universe := {v} -> has_sig kernel_ref.guarded_type v
// ... etc.
```

For closed inputs, all of these work in any mode (raw triage on
closed values is fine). For hypothesis inputs, all of them fail
under the walker --- the correct parametric behaviour:
polymorphic code cannot inspect the type of a type variable.

#note[
  *No `wait_rec` needed under data-as-eliminator.* Earlier
  designs used a kernel-registered `wait_rec` as the privileged
  reflection eliminator (registry-fold over type-formers). Under
  the data-as-eliminator framework, library types' per-value
  templates ARE Pi expressions (the recq encoding ensures this).
  Reflection via `has_sig` works correctly in both modes:
  closed-type recognition succeeds; hypothesis-type recognition
  is rejected by the walker. No additional kernel machinery
  required.

  *Verified in `lib/scott_soundness.test.disp`*: the walker
  rejects `is_neutral`, `is_fork`, `is_stem`, `is_leaf`,
  `pair_fst`, and `tree_eq` on hypothesis inputs. Reflection
  helpers built on `has_sig` inherit this safety.
]

== The public boundary

Public types are *guarded* wait-values: `wait kernel.guard core`
where `core` is a kernel-internal predicate. `Type k`, `Pi`,
`Ord`, `OrdLt` are guarded; user-defined types built via
data-as-eliminator (Bool, Nat, Eq) wrap their bodies in `guard`
to be eligible at the public boundary.

`q_guard_fn` is the *only* checker that returns bare `TT`/`FF`.
It performs two operations:

+ Run `scan_no_neutral` on the value. Pre-existing neutral roots
  in the input are rejected.
+ Run `checked_apply core v` to validate the value against the
  underlying core predicate. The result is a `CheckedResult`;
  `q_guard_fn` unwraps `Ok TT` to bare `TT`, everything else to
  bare `FF`.

The entry scan is the *static* analog of the parametric-mode
fork-formation rule: values entering at the public boundary cannot
contain pre-existing neutrals that the kernel didn't mint.

= CheckedResult <checkedresult>

The walker and certified handlers communicate via the
`CheckedResult` discriminated tree:

```disp
CheckedResult = Ok tree | Fail
Ok   = {v} -> t t v        // pair(LEAF, v) = fork(LEAF, v)
Fail = t (t t)             // stem(K)
```

`is_ok r` distinguishes the two; `ok_value r` extracts the payload
when `is_ok r = TT`.

The `tree` payload of `Ok` may itself be:
- `TT` --- predicate succeeded.
- `FF` --- predicate concretely rejected.
- A *stuck-bool* (`cert_make_stuck Bool stuck_meta`) --- the
  predicate's outcome is deferred because some operation involved
  a hypothesis. Used for polymorphic universe ranks; see §5.
- An arbitrary tree value (e.g., a new neutral from
  `q_hyp_reduce_fn`).

== Composition helpers

CPS continuations chain CheckedResult-producing operations:

```disp
must_ok_any  := {r, k} -> if is_ok r then k (ok_value r) else Fail
must_ok_tt   := {r, k} -> if is_ok r ∧ ok_value r = TT then k t else Fail
must_ok_concrete_tt := {r, k_concrete, k_stuck} ->
  if is_ok r then
    if ok_value r = TT then k_concrete t
    else if is_neutral (ok_value r) then k_stuck (ok_value r)
    else Fail
  else Fail
```

Continuations passed to these helpers must be closed functions.
A `{_} -> body_using_outer_vars` continuation is *not* a thunk in
tree calculus --- bracket abstraction over outer free vars produces
an `S(K K) ...` chain that evaluates eagerly. Use `{x} -> ...`
where `x` actually appears in the body, or use `match` (which
desugars to closed-arm select-then-apply with proper free-variable
capture).

== Handler return convention

| Handler family | Returns | Dispatcher branch |
|---|---|---|
| `q_guard_fn` (public boundary) | bare `TT`/`FF` | `Ok (f x)` |
| Type-checker handlers (`q_pi_fn`, `q_core_type_fn`, `q_guarded_type_fn`) | `CheckedResult` | `f x` (no wrap) |
| `q_hyp_reduce_fn` | `CheckedResult` (`Ok new_neutral` if Pi-typed; `Fail` otherwise) | `f x` (no wrap) |
| Comparison handlers (`q_ord_lt_fn`, `q_ord_le_fn`, `q_ord_max_fn`) | `CheckedResult` | `f x` (no wrap) |
| Walker default | `CheckedResult` | (recursive call) |

Every handler returns `CheckedResult` so that stuck-bools, `Fail`,
and `Ok` propagate uniformly. The only exception is `q_guard_fn`,
which terminates the public boundary with bare `TT`/`FF`.

`q_hyp_reduce_fn` returns `Fail` on non-Pi-typed neutral
application (e.g., applying a Nat-typed neutral to anything is
semantically meaningless). There is no `InvalidType` sentinel; the
failure surfaces directly via `Fail`.

= Data as eliminator <data-as-eliminator>

The framework's defining commitment: *every inductive type is
encoded such that values are their own eliminators*. This is
standard Scott encoding from lambda calculus, adapted to Disp's
tree calculus + recursive-record (`recq`) machinery so that the
recursive type's outer form remains a literal Pi --- the kernel
recognises it without modification.

== The encoding template

For an inductive type T with constructors $c_1, ..., c_n$, the
type, constructors, and eliminator are encoded by a single
`recq` package:

```disp
let T_pkg : {T_template, T_self_ref} = recq {
  T_template := {ks, raw, query} -> {target} ->
    Pi (Pi ks.T_self_ref ({_} -> Type k)) ({m} ->
    Pi ((m c_1) | uncurried over c_1's args) ({_} ->
    ...
    Pi ((m c_n) | uncurried over c_n's args) ({_} ->
      m target))) ;
  T_self_ref := {ks, raw, query} ->
    wait ({_} -> ks.T_template t) t
}
let T_template = T_pkg.T_template
```

`T_template target` is the *per-value* eliminator type at a
specific target value: a literal Pi expression whose outer form
is `kernel.pi`. The recursive self-reference inside the motive's
domain (`ks.T_self_ref`) is a `wait`-form that lazily resolves
to `ks.T_template t` --- breaking the recursion through `recq`
without affecting the outer Pi signature.

Constructors are Scott-style closures:

```disp
c_i a_1 ... a_k := {motive, case_1, ..., case_n} ->
  case_i a_1 ... a_k
```

The eliminator is identity-applied to the target:

```disp
T_rec := {motive, case_1, ..., case_n, target} ->
  target motive case_1 ... case_n
```

Hypothesis minting instantiates the per-value template at the
hypothesis identity:

```disp
cert_make_T_hyp := {h_id} -> Hyp (T_template h_id) h_id
```

Each Bool/Nat/Eq/... hypothesis stores its own per-value Pi
type (with the hypothesis identity occupying the `target`
position). Different hypotheses have different stored types ---
this is what gives "the type of v depends on v" precision.

#note[
  *Why `recq` and not just `wait`.* A naive
  `T ≡ wait body t` puts the wait-form's signature (not
  `kernel.pi`) on T's outer form. The kernel's `has_sig
  kernel.pi T` would return `FF`, and `q_hyp_reduce_fn` would
  reject any application of a T-typed hypothesis. The `recq`
  trick keeps the outer form as a literal Pi; the recursive
  reference lives one level down inside the Pi's metadata,
  where it doesn't affect signature recognition. Verified
  experimentally in `lib/scott_recq_bool.disp` and
  `lib/scott_per_value.disp`.
]

== The hyp_reduce property

This encoding gives uniform stuck-handling for free.

For closed `target : T`, applying `T_rec ... target` reduces to
the appropriate case via the value's own dispatch. Concrete
result.

For *hypothesis* `target` of type T (minted via
`cert_make_T_hyp h_id`, with stored type `T_template h_id` ---
a literal per-value Pi): applying `target motive c_1 ... c_n`
is a sequence of Pi-applications against a neutral. Each
application fires `q_hyp_reduce_fn`, which extends the spine and
updates the stored type per Pi-typing's standard codomain
computation. After all case args are absorbed, the final stored
type is `m target` (with `m` substituted by the user's motive
and `target` by the hypothesis identity).

The result is a neutral with stored type `motive h_id` and a
fully extended spine. *This is exactly what
`cert_make_stuck (motive target) target` would have produced
under the previous design.* The eliminator did not need to check
`is_neutral target`; the deferral happened automatically through
the kernel's existing Pi-application machinery.

#note[
  *No per-eliminator stuck check.* In the previous design, every
  eliminator had to `select` on `is_neutral target` and dispatch
  to `cert_make_stuck` for the neutral case. With data-as-
  eliminator, this is unnecessary: the eliminator's body is
  literally `target motive ...`, and `apply` against a neutral
  fires `hyp_reduce` automatically.

  This is why the kernel record is so much smaller than the
  previous design: `q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`,
  `q_ord_rec_fn`, and `cert_make_stuck` for eliminators all
  disappear. The single `hyp_reduce` mechanism handles every
  stuck-on-hypothesis case.
]

#note[
  *Soundness: structural-inspection motives are rejected.*
  A motive that introspects its target via raw triage (e.g.,
  `{b} -> select Nat Bool (is_neutral b)`) is a parametricity
  violation. Under the walker, applying such a motive to a
  hypothesis target fires `is_neutral` on the hypothesis, which
  triages on it, which the walker rejects (the
  triage-on-neutral rule). The motive cannot be used.

  Motives written in data-as-eliminator style (applying target
  to inner case handlers) compose correctly through `hyp_reduce`.
  Type-builder motives (assembling types from arguments without
  introspection) work as expected.

  *Verified in `lib/scott_soundness.test.disp`* (16 tests):
  every reflective attack pattern translated to DAE encoding is
  correctly rejected by the walker.
]

== Soundness <data-as-eliminator-soundness>

Data-as-eliminator preserves all soundness properties of the
previous design. The walker's parametric-mode rules (no triage on
neutral, no neutral-rooted fork formation) are unchanged, and
the encoding doesn't introduce new attack vectors:

- Constructors (`TT`, `FF`, `zero`, `succ`, `refl`, `0_ord`,
  `omega_plus`) are Scott-style closures. Their compiled tree
  shapes are not neutral-rooted forks. User code cannot
  construct a value with `kernel.hyp_reduce` as `pair_fst` via
  these constructors.
- Hypothesis values (per-value `Hyp` instances) have stored type
  `T_template h_id`, a literal Pi expression. Applying the
  hypothesis fires `q_hyp_reduce_fn` exactly like applying any
  Pi-typed neutral. Spine extension and stored-type tracking
  work identically.
- Reflective attacks (`is_neutral`, `is_fork`, `is_stem`,
  `is_leaf`, `pair_fst`, `tree_eq`, `has_sig`) on hypothesis
  inputs are rejected by the walker via the triage-on-neutral
  rule. The DAE encoding doesn't bypass this.
- Adversarial constructors (e.g.,
  `fake_TT = {motive, ct, cf} -> select ct cf (is_neutral motive)`)
  are rejected: applying them with a hypothesis-typed motive
  triggers `is_neutral` on a neutral, which the walker rejects.

#note[
  *Verified in `lib/scott_soundness.test.disp` (20 tests).*
  Each soundness property above has a corresponding test that
  routes through `checked_apply_walker` (the actual walker) and
  asserts rejection. The DAE architecture preserves the soundness
  guarantees of the previous design.
]

== Constructor canonicality and shape overlap

Constructors of inductive types compile to specific tree shapes
via Disp's deterministic bracket abstraction. Hash-cons identity
on these shapes matches definitional equality.

#note[
  *Important: constructor shapes can overlap across types.* For
  example, the Bool constructor `TT` and the Nat constructor
  `0_nat` both compile to the K combinator (`stem(LEAF)` after
  η-reduction): both are "the first projection of two arguments."
  Tree-equality between them is `TT`.

  This is *not* a soundness issue. Types in Disp are
  predicates, not unique tags. A value of shape K inhabits both
  Bool and Nat (both predicates accept K-shape). The TYPE,
  threaded by the elaborator and checked by the kernel, is what
  determines interpretation.

  This shape overlap exists in the previous tree-shape encoding
  too (Disp's prior `TT = LEAF, zero = LEAF` had the same property).
  The data-as-eliminator architecture inherits the property; it
  does not introduce a new problem.
]

== Worked example: Bool

```disp
let bool_pkg : {bool_template, bool_self_ref} = recq {
  bool_template := {ks, raw, query} -> {target} ->
    Pi (Pi ks.bool_self_ref ({_} -> Type 0)) ({m} ->
    Pi (m TT) ({_} ->
    Pi (m FF) ({_} ->
      m target))) ;
  bool_self_ref := {ks, raw, query} ->
    wait ({_} -> ks.bool_template t) t
}
let Bool_template = bool_pkg.bool_template

TT := {motive, case_t, case_f} -> case_t
FF := {motive, case_t, case_f} -> case_f
bool_rec := {motive, case_t, case_f, target} -> target motive case_t case_f

cert_make_bool_hyp := {h_id} -> Hyp (Bool_template h_id) h_id
```

The "type Bool" used in source-level annotations
(`{x : Bool} -> ...`) is the family `Bool_template`. When the
parser encounters `Bool`, it desugars to a closure that the
elaborator can instantiate per-value at hypothesis-mint sites.

The Bool predicate (`Bool v = TT iff v inhabits Bool`) is the
recursive Pi-checking of v against `Bool_template v` --- standard
Pi-checking applied via the kernel's existing `q_pi_fn`.

== Worked example: Nat

```disp
let nat_pkg : {nat_template, nat_self_ref} = recq {
  nat_template := {ks, raw, query} -> {target} ->
    Pi (Pi ks.nat_self_ref ({_} -> Type 0)) ({m} ->
    Pi (m zero) ({_} ->
    Pi (Pi ks.nat_self_ref ({k} ->
        Pi (m k) ({_} -> m (succ k)))) ({_} ->
      m target))) ;
  nat_self_ref := {ks, raw, query} ->
    wait ({_} -> ks.nat_template t) t
}
let Nat_template = nat_pkg.nat_template

zero := {motive, case_z, case_s} -> case_z
succ := {n} -> {motive, case_z, case_s} -> case_s n (n motive case_z case_s)
nat_rec := {motive, case_z, case_s, target} ->
  target motive case_z case_s

cert_make_nat_hyp := {h_id} -> Hyp (Nat_template h_id) h_id
```

Note `succ n`'s body recurses on `n` via `n motive case_z case_s`.
This makes succ's eliminator behaviour Mendler-flavoured: the
recursive sub-result is computed inline and passed alongside the
predecessor.

For arithmetic via `fix`:
```disp
add := fix ({self, a, b} -> nat_rec ({_} -> Nat_template t)
                                    b
                                    ({pred, _} -> succ (self pred b))
                                    a)
```

#note[
  *Compile-time canonicality verified.* Experiments
  (`lib/scott_experiment.disp`) confirm that arithmetic on
  data-as-eliminator Nats produces hash-cons-identical trees
  to direct constructor expressions: `add 2 3 = 5`,
  `mul 2 4 = 8`, `pred (add 2 3) = 4`. Disp's compile-time
  partial evaluation eagerly normalises apply chains, so closed
  expressions reduce to canonical form.
]

== Worked example: Eq

```disp
let eq_pkg : {eq_template, eq_self_ref} = recq {
  eq_template := {ks, raw, query} -> {A, x, y, target} ->
    Pi (Pi ks.eq_self_ref ({_} -> Type 0)) ({m} ->
    Pi (m refl) ({_} ->
      m target)) ;
  eq_self_ref := {ks, raw, query} ->
    wait ({_} -> ks.eq_template t t t t) t
}
let Eq_template = eq_pkg.eq_template

refl := {motive, case_refl} -> case_refl
eq_J := {motive, case_refl, x, y, proof} -> proof motive case_refl
eq_subst := {P, x, y, proof, px} -> proof ({z, _} -> P z) px
eq_sym := {x, y, proof} -> proof ({z, _} -> z) refl
eq_cong := {f, x, y, proof} -> proof ({z, _} -> z) refl

cert_make_eq_hyp := {A, x, y, h_id} -> Hyp (Eq_template A x y h_id) h_id
```

#note[
  *Hash-cons-identity equality is automatic.* For
  `refl : Eq A x y` to typecheck, the body
  `{motive, case_refl} -> case_refl` must produce a value of
  type `m refl` (the supplied case) which is also expected to
  have type `m target`. The Pi-checker requires
  `m refl = m target` definitionally (hash-cons identity).
  Substituting `case_refl` for `target` in the per-value
  template requires `target` to hash-cons-equal `refl`.

  Translating to the user-facing type `Eq A x y`: refl
  typechecks at `Eq A x y` iff `y` hash-cons-equals `refl` AND
  `x` hash-cons-equals `refl`'s implicit value. Effectively,
  `Eq A x y` is inhabited by refl iff `x = y` as trees.

  Same semantics as the previous design's explicit `q_eq_fn`
  check --- but with no `q_eq_fn` in the kernel. The semantics
  drops out of the encoding. Verified in `lib/scott_eq.disp`:
  11 tests covering refl, J, transport, symmetry, congruence
  all pass.
]

== Worked example: Ord

```disp
let ord_pkg : {ord_template, ord_self_ref} = recq {
  ord_template := {ks, raw, query} -> {target} ->
    Pi (Pi ks.ord_self_ref ({_} -> Type 0)) ({m} ->
    Pi (m 0_ord) ({_} ->
    Pi (Pi ks.ord_self_ref ({alpha} ->
        Pi ks.ord_self_ref ({beta} ->
        Pi (m alpha) ({_} ->
        Pi (m beta) ({_} ->
          m (omega_plus alpha beta)))))) ({_} ->
      m target))) ;
  ord_self_ref := {ks, raw, query} ->
    wait ({_} -> ks.ord_template t) t
}
let Ord_template = ord_pkg.ord_template

0_ord := {motive, case_z, case_op} -> case_z
omega_plus := {alpha, beta} -> {motive, case_z, case_op} ->
  case_op alpha beta
          (alpha motive case_z case_op)
          (beta motive case_z case_op)
ord_rec := {motive, case_z, case_op, target} ->
  target motive case_z case_op
```

The CNF invariant (`leading_exp β ≤ α`) is a library-level
predicate over `omega_plus α β` constructions; it's checked by
the public `Ord` wrapper, not enforced at construction.

=== Three forms of Ord values <ord-forms>

Anywhere `k : Ord` appears, `k` may be in any of three forms:

+ *Closed CNF Scott closure*: `0_ord`, `omega`,
  `omega_plus 1 0_ord`. A closed data-as-eliminator value (per
  the encoding above). All arguments to `omega_plus` are
  themselves closed Ord values. Operations evaluate concretely
  by applying the closure to case handlers.

+ *Scott closure containing a hypothesis*: e.g.,
  `succ_ord r_hyp` where `r_hyp` is a hypothesis-typed Ord.
  Per the data-as-eliminator encoding, this expands to a
  closure that, when applied to case handlers, eventually
  applies the hypothesis itself. That application fires
  `hyp_reduce`, producing a stuck result. The closure is not
  itself a neutral (its outer signature is the Scott closure
  shape, not `kernel.hyp_reduce`), but it propagates stuck on
  use.

+ *Hypothesis Ord*: `Hyp (Ord_template h_id) h_id`, minted by
  `cert_make_ord_hyp`. Has stored type `Ord_template h_id`
  (a per-value Pi). The Ord predicate accepts it via the H-rule.

All three forms are valid `Ord` values: `Ord k = TT` for forms 1
and 3 (form 3 via the H-rule), or `Ord k = stuck_bool` for
form 2 (the closure's stuck propagation reaches the predicate
check). Comparisons (`ord_lt`, `ord_le`, `ord_max`) on a
non-closed form propagate stuck.

== Library type summary

The standard library types defined via the encoding above:

#figure(
  table(
    columns: 4,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Type*], [*Constructors*], [*Eliminator*], [*Encoding section*],
    [`Bool`],     [`TT`, `FF`],                  [`bool_rec`], [§4.5],
    [`Nat`],      [`zero` (= `0`), `succ`],      [`nat_rec`],  [§4.6],
    [`Eq A x y`], [`refl`],                      [`eq_J`],     [§4.7],
    [`Ord`],      [`0_ord`, `omega_plus`],       [`ord_rec`],  [§4.8],
    [`OrdLt k`],  [(refinement of `Ord`)],       [(via `ord_rec`)], [§4.8],
  ),
  caption: [Standard library inductive types under data-as-eliminator.],
)

Standard derived helpers built on these (`add`, `mul`, `pred`,
`is_zero` for Nat; `eq_subst`, `eq_sym`, `eq_cong` for Eq;
`succ_ord`, `omega`, `omega_squared` for Ord) are conventional
library code in `lib/`. They are not kernel-relevant and are
documented in their respective module headers.

= Universes and Ord

== Why Ord, not Nat

Universe ranks in Disp are values of an inductive ordinal type `Ord`,
not `Nat`. The reason is *polymorphism with subject reduction*: a
function like `{r : Ord} → Type r → Type r` has a hypothesis-typed
rank `r` whose universe comparisons must defer until `r` is
instantiated. In Disp's parametric mode, raw `nat_lt` on a
hypothesis Nat fails (correctly --- it would otherwise be a
reflective channel). Universe-checking needs to defer rather than
fail. So we use a separate type `Ord` whose comparison primitives
(`ord_lt`, `ord_le`, `ord_max`) are kernel handlers that mint
*stuck-bool* results when arguments contain hypotheses, rather
than failing outright.

`Ord` is structurally a *Cantor normal form* (CNF) tree --- a
sum-of-omega-powers expression. Every ordinal below $epsilon.alt_0$
has exactly one CNF tree (modulo data-as-eliminator's
encoding-specific shapes), so hash-cons identity on Ord values
matches ordinal equality. Constructors:

- `0_ord : Ord` (encoded as the data-as-eliminator zero case)
- `omega_plus : Ord → Ord → Ord` --- $omega^alpha + beta$ with
  invariant `leading_exp β ≤ α`.

Closed below-ω ordinals (`0_ord`, `succ_ord 0_ord`, etc.) share
shape signatures with finite Nat values --- both compile through
the same Scott pattern. Source-level `Type 0`, `Type 1` work via
identifier resolution to the canonical Ord constructors.

== Where Ord lives

`Ord` lives in `Type 0`. CNF Ord is an inductive type whose
constructors take only `Ord` recursively, so predicativity holds
at `Type 0`. The CNF invariant (`leading_exp β ≤ α`) is checked
by `Ord`'s predicate; it does not introduce a universe shift.

== Polymorphic-universe functions <poly-universe>

Polymorphic-rank functions come in two flavors: *unbounded* and
*bounded*. The kernel handles them differently --- unbounded gets
stuck `pi_rank`; bounded gets closed `pi_rank` derivable from the
bound.

=== Unbounded polymorphism: stuck `pi_rank`

A function `{r : Ord} → Type r → Type r` mathematically lives in
$"Type" omega$. But the kernel does not compute closed $omega$
for `pi_rank` of such a type --- it produces a stuck Ord, because
recognising "supremum over Ord of $alpha + 1$ is $omega$" is a
meta-theoretic identity not derivable from structural recursion.

This is consistent with @soundness-not-completeness: the kernel
provides correct stuck propagation as the floor, and lets users
opt into bounded levels (below) when they want closed answers.

=== Bounded polymorphism: closed `pi_rank` via `OrdLt k`

A function `{r : OrdLt omega} → Type r → Type r` is the bounded
form: the polymorphic rank `r` is constrained to be strictly less
than $omega$. Here `pi_rank` *can* compute closed $omega$ via
the kernel's bound-consulting machinery (see §5.5 below).

Tests at the polymorphic level work for bounded forms:
`(Type omega) ({r} -> ...) = TT` passes if the polymorphic
function genuinely inhabits `Type omega`.

=== Closed (non-polymorphic) ranks

Closed ranks are computed exactly with no special handling. For
instance:
- `pi_rank (Pi Nat ({_} -> Nat))` = `ord_max 0_ord 0_ord` = `0_ord`.
- `pi_rank (Pi (Type 0) ({_} -> Type 0))` = `ord_max 1 1` = `1`.
- `pi_rank (Pi (Type 0) ({A} -> A))` = `ord_max 1 0` = `1`.

The "rank from type" rule is implicit here: a value `v` whose
type is `Type k` has rank-as-a-type equal to `k`. For hypothesis
`v` with stored type `Type r_hyp`, the rank-as-a-type is `r_hyp`
(closed if `r_hyp` is closed, stuck if `r_hyp` is itself a
hypothesis).

== Stuck comparisons

`ord_lt`, `ord_le`, `ord_max` are kernel-registered primitives.
Their handlers receive two `Ord` values. The handlers run in
raw mode, so they can apply the values to internal probe
handlers to extract the Scott-encoded structure
(`apply value motive zero_case op_case` runs the value's own
dispatch). Three cases:

+ Both arguments are closed Scott closures (no hypothesis
  reachable): apply with probe case handlers that compute the
  comparison directly. Return `Ok TT` / `Ok FF` / `Ok ord_value`.
+ One or both arguments are hypothesis-typed Ord values
  (per-value Hyp), and the comparison is determinable from
  stored bounds (e.g., `r_hyp : OrdLt omega` ⊢
  `ord_lt r_hyp omega = TT`). The handler reads the bound from
  the hypothesis's metadata and consults the bound-consulting
  identity table. Returns the concrete result.
+ Otherwise (hypothesis without sufficient bound, OR a Scott
  closure containing a hypothesis whose probe-extraction
  triggers stuck propagation): mint a stuck result. For
  `ord_lt`/`ord_le`: `Ok (cert_make_stuck Bool stuck_meta)`.
  For `ord_max`: `Ok (cert_make_stuck Ord stuck_meta)`.

The `stuck_meta` encoding tags the operation: `(LtTag, a, b)`,
`(LeTag, a, b)`, `(MaxTag, a, b)`. This ensures distinct
operations on the same arguments produce distinct stuck-Bools.

#note[
  *Note on Scott-closure traversal.* A closed Ord-as-eliminator
  is structurally a function. To "extract its CNF structure,"
  the comparison handler applies it to specific probe handlers
  that mark which constructor was hit and what the args were.
  E.g., applying `omega_plus α β` to case handlers
  `(_, label_zero, label_op) → label_op α β` returns
  `(label_op_marker, α, β)`. The handler then recurses on the
  args.

  This is computationally similar to the previous design's raw-
  triage on CNF tree shape, just routed through Scott
  application. For closed input, terminates in O(CNF depth).
  For hypothesis-containing input, the inner application of the
  hypothesis fires `hyp_reduce` and the handler stuck-defers.
]

#note[
  *Bound-consulting identities are the trusted base for bounded
  polymorphism.* Each canonical limit ordinal (`omega`, `ω+ω`,
  `ω·n`, `ω²`, `ω^ω`, `ε₀`) admits a small set of structural
  identities:
  - Closure under successor: `r < λ ⊢ succ_ord r < λ`.
  - Closure under bounded `omega_plus`: `r, s < λ ⊢ omega_plus r s < λ`
    (when `λ` is closed under that op).

  Each identity is a one-line dispatch in the comparison handler.
  Each ships with its own targeted soundness test. The supported
  identity set is enumerated in one auditable comment block in
  `kernel.disp`.

  *Recognition fragility:* identities recognise canonical limit
  ordinals via `tree_eq` against canonical CNF trees. Surface-syntax
  shortcuts or derived expressions that should equal the canonical
  form must hash-cons-equal exactly; otherwise they fall through to
  stuck silently. Adding a parser-level `omega` keyword requires
  re-checking the identity table against the new canonical form.
]

== Subject reduction <subject-reduction>

The conventional MLTT subject reduction statement
("if $Gamma tack M : T$ and $M --> M'$ then $Gamma tack M' : T$")
presupposes a judgment with explicit contexts and an open-term
reduction relation. Disp has neither at the language level.
Type-checking is `apply(T, M)` reducing to `TT`; hypotheses exist
only inside the kernel (minted by `cert_make_hyp` during
under-binder analysis), not as syntactic free variables that the
programmer manipulates. The right Disp form of subject reduction is:

#note[
  *Theorem (Subject reduction, Disp form).* For closed `M` admitted
  by the public boundary, if `apply(T, M) -->* TT` under raw apply,
  then for any `M -->* M'` under raw apply, `apply(T, M') -->* TT`.
]

*Proof.* Tree calculus is confluent under raw apply. `apply(T, M)`
and `apply(T, M')` reach the same normal form; by hypothesis it is
`TT`. ∎

The walker does not enter this argument. The walker is a *static
admissibility filter on `M`*: it decides whether `M` is admitted at
the public boundary. Walker reductions are a subset of raw
reductions (the walker refuses some, changes none), so any walker-
reduction sequence is also a raw-reduction sequence. Once `M` is
admitted, all subsequent reductions are governed by raw apply,
where SR is a confluence corollary, not a separate theorem.

Disp avoids the elaborate SR machinery of comparable systems
because it has no side-conditions to maintain across substitution.
BCDE (arXiv 2212.03284) loses SR at *constraint-indexed products*
`[α < β] N type` --- formation gated on `α < β` being loop-free,
which substitution can violate. TTBFL (arXiv 2502.20485) recovers
SR by baking bounds into level types (`r : Ord< k`) and adding
non--syntax-directed `Trans`/`Cumul` rules. Disp has neither
construct: typing is `apply(T, M)` and there are no rules gated on
predicates over the typing context.

#note[
  *Why "under-binder SR" is not a meaningful question in Disp.*
  Hypotheses are kernel-internal artifacts produced during
  predicate evaluation, not syntactic objects in the surface
  language. There is no programmer-visible judgment of the form
  "$Gamma tack M : T$" with $Gamma$ ranging over hypothesis sets;
  there is only `apply(T, M)` on closed `M`. The conventional
  open-term SR question therefore does not apply.

  This is a structural simplification, not a limitation: anything
  that would be expressed via open-term SR in MLTT is expressed in
  Disp by checking closed instantiations.
]

#note[
  *No data-driven level computation.* Disp does not provide
  `lub : List (Σℓ. Type ℓ) → Ord` --- a function that takes a
  runtime list of types-at-various-levels and computes their
  supremum. This is the only restriction relative to TTBFL, and
  it is consistent with the soundness-not-completeness stance.
  If a use case demands this, monomorphize at elaboration time.
]

= Dispatcher and walker

== The dispatcher

`q_checked_apply_fn` (the dispatcher, exported as `checked_apply`)
routes every application through one of two paths:

+ *Signature-recognized*: if `f`'s signature matches a registered
  kernel handler, the dispatcher invokes the handler. The handler
  returns `CheckedResult` (after the InvalidType cleanup, even
  `q_hyp_reduce_fn` returns `CheckedResult`).
+ *Default*: otherwise, the dispatcher walks the apply structure
  step-by-step under parametric mode. This is `checked_raw_apply`
  --- the *walker*.

```disp
q_checked_apply_fn = {ks, raw, query} -> fix ({self, f, x} ->
  select_chain
    (case (has_sig raw.hyp_reduce f)  ({_, f, x} -> f x))
    (case (has_sig ks.guard f)        ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.pi f)           ({_, f, x} -> f x))
    (case (has_sig ks.core_type f)    ({_, f, x} -> f x))
    (case (has_sig ks.guarded_type f) ({_, f, x} -> f x))
    (case (has_sig ks.ord f)          ({_, f, x} -> f x))
    (case (has_sig ks.ord_lt_type f)  ({_, f, x} -> f x))
    (case (has_sig ks.ord_lt f)       ({_, f, x} -> f x))
    (case (has_sig ks.ord_le f)       ({_, f, x} -> f x))
    (case (has_sig ks.ord_max f)      ({_, f, x} -> f x))
    // Default: walker
    ({self, f, x} -> checked_raw_apply self f x)
    self f x)
```

The dispatch list is short: only kernel-primitive type-formers
(Pi, Type, Ord, OrdLt) and ordinal comparisons. Notable absences:

- *No per-inductive-type checkers.* Bool, Nat, Eq, Ord-as-data
  have no `q_bool_fn`, `q_nat_fn`, `q_eq_fn`. Their predicates
  reduce to Pi-checking the value against the per-value
  `T_template target` Pi-chain --- handled by `q_pi_fn`.
- *No per-eliminator dispatch cases.* `bool_rec`, `nat_rec`,
  `eq_J`, `ord_rec` are library-level identity-
  applied functions. When applied to a closed target, normal
  reduction handles it. When applied to a hypothesis target,
  the target's `apply` fires `q_hyp_reduce_fn` directly.
- *No `cert_make_stuck` or `elim_fail`.* Eliminator stuck-on-
  hypothesis is the standard Pi-spine extension via
  `q_hyp_reduce_fn`. No special handler needed.

The kernel record shrinks substantially compared to designs with
explicit per-type handlers.

== The walker (`checked_raw_apply`)

The walker mirrors the five raw apply rules, with two parametric
restrictions inserted: triage on a neutral fails; stem-rule
fork-formation rejects neutral roots.

```text
checked_raw_apply self I x:                       // I-shortcut
  Ok x

checked_raw_apply self LEAF x:
  Ok (stem x)                                     // stems are
                                                  // never neutrals

checked_raw_apply self (stem a) x:                // stem rule
  let r = fork a x
  if neutral_root r: Fail                         // (b)
  else: Ok r

checked_raw_apply self (fork LEAF payload) x:    // K rule
  Ok payload

checked_raw_apply self (fork (stem c) b) x:      // S rule
  must_ok_any (self c x) ({cx} ->
    must_ok_any (self b x) ({bx} ->
      self cx bx))

checked_raw_apply self (fork (fork tc td) b) x:  // triage rule
  if neutral_root x: Fail                        // (a)
  else if x is LEAF: Ok tc
  else if x is stem c: self td c
  else if x is fork l r:
    must_ok_any (self b l) ({bl} ->
      self bl r)
```

== Soundness theorem <soundness-theorem>

#note[
  *Theorem (Provenance soundness).* Under `checked_apply`, no
  rule of `checked_raw_apply` can produce a fork whose `pair_fst`
  matches the canonical `kernel.hyp_reduce` signature unless that
  fork was either (a) already present in the input that passed
  the public-boundary entry scan, or (b) the result of an earlier
  registered-handler call (which mints via raw `apply`, not
  `checked_raw_apply`).
]

*Proof sketch.* By structural induction on the rules of
`checked_raw_apply`.

+ *Stem rule.* Explicitly checks `neutral_root r` on the
  constructed fork; returns `Fail` if matched.

+ *S rule.* The result is the value of recursive `self` calls.
  Any fork produced inside those calls is checked by *its*
  stem/S/triage rule. By IH, no neutral-shaped fork escapes.

+ *Triage-fork rule.* Same shape; recursive calls.

+ *K rule, triage-leaf, triage-stem, I-shortcut.* Pass through
  existing values (caught by entry scan or prior `checked_apply`).

+ *Leaf rule.* Produces a stem; stems are never fork-shaped
  neutrals.

*Base case.* The public-boundary entry to `checked_apply` (via
`q_guard_fn`) runs `scan_no_neutral` on the user's value.
Internal recursive entries from registered handlers may pass
certified neutrals (built via `cert_make_*`), but those neutrals
were minted by raw `apply` from inside a handler body and did not
transit `checked_raw_apply`.

*Conclusion.* The only neutral-signed forks reachable via
`checked_apply`'s rules are (a) certified neutrals minted by
trusted handlers, or (b) any pre-existing neutral that got past
the entry scan --- which would be a bug in `scan_no_neutral`,
not in the constructor checks. ∎

#note[
  *cert_make_hyp / cert_make_stuck are kernel-private.* The
  helpers that mint neutrals are `let`-bound in `kernel.disp` and
  not exported. Even if a user could name them, calling them from
  user position would route through the dispatcher's walker
  default, where the inner `wait raw.hyp_reduce ...` construction
  hits the stem-rule's neutral-root check and fails.

  The argument is intricate enough to deserve an adversarial test:
  `apply (apply wait raw.hyp_reduce) meta` from user position must
  be rejected. See IMPLEMENTATION_PLAN.md Phase 1 tests.
]

== Soundness carve-out: I-shortcut

`I = fork(fork(LEAF, LEAF), LEAF)` is structurally a triage shape.
Without the I-shortcut, `apply(I, hyp)` would hit the triage
rule's neutral check and `Fail` --- but identity on a hypothesis
is a legitimate program. The walker carves out
`apply(I, x) = Ok x` directly, recognized via
`tree_eq f I_canonical`. The runtime `tree_eq` fast path makes
this O(1).

This is the *only* soundness carve-out in the walker.

= Type formers (kernel primitives)

The kernel registers a small set of type-formers --- those that
fundamentally cannot be defined as data-as-eliminator (Pi, Type k)
and the ordinal-comparison infrastructure
(`ord_lt`, `ord_le`, `ord_max`).

All inductive types (Bool, Nat, Eq, Ord-as-data, OrdLt) are
library-level data-as-eliminator definitions per the §4 template.
The kernel does not recognise their constructor shapes
individually; type-checking such values reduces to standard
Pi-checking against their per-value templates.

#note[
  *Pi is special.* It is the only "type former" that doesn't
  follow data-as-eliminator. Pi's eliminator IS function
  application, baked into the runtime's apply rules. Encoding Pi
  via itself would be circular and adds no value.

  *Type k is also special.* The universe checker validates
  that a value is one of the recognised type-formers (closed
  Pi, closed library-type templates, etc.) at the appropriate
  rank. This is a structural/registry check, not a recursive
  data-as-eliminator predicate.

  *Ordinal comparisons stay kernel-primitive* because they need
  to consult stored bounds on hypothesis-typed Ord values
  (bound-consulting). A library-level comparison would have to
  scan the value's Pi-template structure to recover the bound,
  which is expensive and brittle. The kernel handlers know how
  to read the bound from the hypothesis's metadata directly.
]

== Pi

*Signature*: `kernel.pi`. *Checker*: `q_pi_fn`.

Pi metadata: `make_pi_meta(domain, codFn)` where `domain` is a
core type and `codFn : domain → core_type`.

*Public constructor*:
```disp
Pi    := {A, B} -> guard (core_Pi (unguard_checked A) ({x} -> unguard_checked (B x)))
Arrow := {A, B} -> Pi A ({_} -> B)
```

*Checking* `Pi A B v`:
+ If `v` is a hypothesis: H-rule (compare stored type to
  reconstructed `wait (ks query) meta`).
+ Otherwise: mint a hypothesis `hyp = cert_make_hyp domain meta`,
  evaluate `result = ks.checked_apply v hyp` and `expected =
  ks.checked_apply codFn hyp`, then either compare H-rule (if
  result is neutral) or `ks.checked_apply expected result` (if
  result is concrete).

*Application of a Pi-typed neutral*: handled by `q_hyp_reduce_fn`.
When `(Hyp PiType id) v` is evaluated, the handler extends the
neutral's spine with `v` and computes the result type as
`pi_meta_cod_fn(PiType_meta) v`.

#note[
  *Pi is special.* It is the only "type former" that doesn't
  follow data-as-eliminator. Pi's eliminator IS function
  application, baked into the runtime's apply rules. Encoding Pi
  via itself would be circular and adds no value.
]

== Type k

*Signatures*: `kernel.core_type` and `kernel.guarded_type`.
*Checkers*: `q_core_type_fn` and `q_guarded_type_fn`.

Universe metadata: a single Ord value (the rank).

`core_type rank` accepts core types: `core_Pi`,
`wait ks.core_type k` for `k < rank`, `wait ks.guarded_type k`
for `k < rank`, and certified neutral type variables whose
stored type is a universe of rank ≤ rank.

Library-defined types (Bool, Nat, Eq, Ord, OrdLt) are
data-as-eliminator templates whose per-value Pi instances are
recognised by signature as Pi (via the recq encoding), so they
fall under the existing Pi case --- no per-library-type
recognition is needed in the universe checker.

`guarded_type rank` accepts public types: `T = guard core` where
`q_core_type_fn rank core = TT`.

*Public constructor*: `Type k = guard (wait kernel_ref.guarded_type k)`.

*Source-level conventions.* Source-level `Type 0`, `Type 1`,
`Type omega` are always written with explicit ranks. Bare
`Type` (no rank) is a parser error.

*Rank check via stuck comparison*: when checking universe
membership, `q_core_type_fn` calls `ord_le rank_target rank_param`.
If both ranks are concrete, the result is `Ok TT` / `Ok FF`. If
either is a hypothesis-typed Ord, the result is
`Ok (cert_make_stuck Bool stuck_meta)`. Stuck Bools propagate up;
at the public boundary they fail any `= TT` test, so polymorphic-rank
programs require concrete instantiation at test sites.

== Ord and OrdLt (defined in `lib/ord.disp`)

Ord and OrdLt are library-level data-as-eliminator types,
following the §4 template. They appear here because the kernel's
*comparison primitives* operate on Ord values, but the types
themselves are not kernel-primitive.

```disp
// In lib/ord.disp
let ord_pkg : {ord_template, ord_self_ref} = recq { ... }
0_ord := {motive, case_z, case_op} -> case_z
omega_plus := {α, β} -> {motive, case_z, case_op} -> case_op α β ...

let ord_lt_pkg : {ord_lt_template, ord_lt_self_ref} = recq { ... }
// OrdLt k is parameterised by the bound k in addition to target.
```

*Bound-info access for hypothesis comparisons.* When a hypothesis
is typed at `OrdLt k`, the kernel-comparison handlers
(`q_ord_lt_fn` etc.) need to recover `k` to apply
bound-consulting identities. By convention, the OrdLt template
stores `k` as the *first parameter* of the per-value template
(immediately accessible via Pi-metadata extraction). Comparison
handlers extract it without traversing the full Pi-chain.

*CNF invariant.* The `omega_plus α β` constructor's CNF invariant
(`leading_exp β ≤ α`) is enforced by a library-level Ord
predicate that runs after construction. Programs that build
malformed CNF trees fail the predicate, not the constructor.

== Hypothesis minting (`cert_make_hyp` family)

The kernel-private `cert_make_hyp` family produces neutrals
during type-checking:

```disp
cert_make_hyp        := {T, id} -> Hyp T id
cert_make_T_hyp      := {h_id} -> Hyp (T_template h_id) h_id  // per-value
```

`cert_make_hyp T id` mints a neutral with literal stored type
`T`. This is used by `q_pi_fn` when checking `Pi A B v`: it mints
`hyp = cert_make_hyp A meta` (using the Pi's metadata as the
identity) and recursively type-checks `v hyp`.

`cert_make_T_hyp h_id` is the *per-value* mint for library types:
the stored type is `T_template h_id`, a specific Pi instance with
the hypothesis identity in the target position. This produces
hypotheses whose elimination via `T_rec` correctly tracks
"motive applied to this specific hypothesis."

`Hyp T id` is the public constructor (rejected by the public
boundary's entry scan; useful only for adversarial tests). There
is no separate `StuckElim` constructor under data-as-eliminator
--- "stuck eliminator" outputs are produced by
`q_hyp_reduce_fn` extending the spine of a Hyp through the
eliminator's case args, which is just standard Pi-application.

== Comparisons (ord_lt, ord_le, ord_max)

Three kernel-registered primitives, with semantics described in
§5.5. Each takes two Ord values and returns a `CheckedResult`
with bound-consulting semantics for hypothesis inputs.

These handlers traverse Ord values via raw `apply` in raw mode
(the Ord values are data-as-eliminator closures; applying them
to internal probe handlers extracts the structure). The
bound-consulting identities are enumerated in a comment block in
`kernel.disp`. Each identity is reviewable as a structural
pattern match in the handler, and ships with its own targeted
soundness test.

= Implementation invariants

These are invariants the implementation must respect. Violating
any of them is a soundness-class bug.

== Handlers must dispatch `is_neutral` first <handler-is-neutral-first>

Type-checker handlers (`q_pi_fn`, `q_core_type_fn`,
`q_guarded_type_fn`) run in raw mode after the dispatcher routes
to them. Raw mode permits raw triage on the candidate value, which
is necessary to inspect concrete tree shapes. But raw triage on a
*neutral* interprets the neutral's wait-encoded structure as
data --- a reflective attack channel.

#note[
  *Invariant (Handler dispatch).* For every type-checker handler
  `q_X_fn`: the first operation in the handler body MUST be
  `is_neutral v`. If `v` is neutral, dispatch via the H-rule
  (`q_h_rule_fn`). Raw triage on `v`'s structure is permitted
  *only* in the non-neutral branch.
]

This invariant applies to fewer handlers than in the previous
design (Bool/Nat/Eq checkers no longer exist as kernel handlers),
but the invariant is still load-bearing for the remaining ones.

== Raw vs `ks.field` for signatures

Two different signature shapes exist; picking the wrong one
silently mismatches every value of that family.

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Constructor*], [*Signature stored in value*], [*Use which inside handlers*],
    [`Hyp ty id`],                      [`raw.hyp_reduce` (raw)],   [`raw.hyp_reduce`],
    [`Pi A B`],                         [`kernel_ref.pi` (proxy)],  [`ks.pi`],
    [`Type k`],                         [`kernel_ref.guarded_type`],[`ks.guarded_type`],
    [`OrdLt k`-bound markers],          [extracted from `Hyp` metadata], [via metadata helpers],
    [Library types (Bool, Nat, Eq, Ord)], [literal Pi (per-value template)], [`ks.pi`],
  ),
)

Hypotheses must store the `raw` (actual recq-resolved) handler
identity because `q_hyp_reduce_fn` IS the runtime that fires when
neutrals are applied. Kernel-primitive type-formers (Pi, Type)
use the proxy form. Library types' per-value templates are
literal Pi expressions (the recq encoding ensures this); they
match `ks.pi` for dispatch purposes.

== Handler return convention

The dispatcher's per-branch wrapping convention (shown in §5.1)
must match the handler's actual return shape. Mismatches double-
wrap or under-wrap, breaking `must_ok_*` chains downstream.

Every entry in the dispatch table must be accompanied by a
comment listing the handler's return convention.

== `recq` laziness for handler interdependence

Handlers reference each other via `ks.X` projections, which
resolve to `wait self X_query` --- inert until applied. Cyclic
references between handlers (e.g., `q_pi_fn` calls
`ks.checked_apply`; `q_checked_apply_fn` references `ks.pi` for
signature matching) are resolved on demand.

Implementer obligations:
- Never force `ks.X` evaluation at top level (e.g., don't write
  `let cached_pi = ks.pi` in module scope).
- Field references inside `fix` bodies are also lazy. Mutual
  recursion between handlers is resolved via the recq projection
  at apply time.

== Public/private boundary

`kernel.disp` declares exports via `:=` and private bindings via
`let`. The following must be private:

- `core_Pi` --- the unguarded kernel-primitive Pi checker.
- `cert_make_hyp` --- the neutral-minting helper. (No
  `cert_make_stuck` --- eliminator stuck-on-hypothesis is
  produced by the standard `q_hyp_reduce_fn` mechanism.)
- `q_*_fn` handler bodies.

The following must be public (`:=`):

- `Pi`, `Arrow`, `Type` --- guarded type constructors for
  kernel-primitive type formers.
- `Bool`, `Nat`, `Eq`, `Ord`, `OrdLt` --- library types defined
  in lib/. Their constructors (`TT`, `FF`, `zero`, `succ`,
  `refl`, `0_ord`, `omega_plus`) and eliminators (`bool_rec`,
  `nat_rec`, `eq_J`, `ord_rec`) are also library-level.
- `Hyp` --- public neutral constructor (callable but rejected
  at the public boundary's entry scan; useful only for
  adversarial tests).
- Reflection helpers (`is_pi`, `is_eq`, `is_universe`, etc.) ---
  thin wrappers over `has_sig`. Inherently parametricity-safe.
- `checked_apply` --- public re-export of the dispatcher.

== `match` and closed-arm desugaring

Tree calculus is strict: thunks `({_} -> body)` capturing outer
free vars do NOT delay evaluation. The `match` keyword in source
code desugars to closed-arm select-then-apply with automatic
free-variable capture. Implementations must use `match` (or the
hand-written closed-arm pattern) for any conditional whose
branches reference outer scope variables.

== Hash-cons identity is load-bearing

Type equality is `tree_eq T1 T2` via the runtime's hash-cons
identity check (O(1)). For this to give meaningful answers:
- Same type must compile to the same tree.
- Bracket abstraction must be deterministic.
- Type-former metadata must use canonical pair-encodings.
- For data-as-eliminator types, constructor expressions must
  reduce to canonical compiled forms via compile-time partial
  evaluation.

If two structurally-equal types compile to different trees,
`Eq Type X Y` would be `FF` even when `X` and `Y` are "the same
type." Compile-time reduction (cirToTree's eager apply) ensures
this for the common cases.

= Worked examples <worked-examples>

== `(Pi Nat ({_} -> Nat)) ({x} -> x) = TT`

Trace through the public test of the polymorphic identity at type
`Nat → Nat`.

+ Source `Pi Nat ({_} -> Nat)` compiles to
  `guard (wait kernel_ref.pi (make_pi_meta Nat ({_} -> Nat)))`.
+ Source `({x} -> x)` compiles to `I_canonical`.
+ Test framework evaluates `(Pi Nat (...)) ({x} -> x)`. Raw apply
  fires `q_guard_fn`.
+ Entry scan on I_canonical: no neutrals. Passes.
+ `q_guard_fn` calls `ks.checked_apply core v`. Pi-checker fires.
+ Mints `hyp = cert_make_hyp Nat meta`.
+ `result = ks.checked_apply v hyp`. v = I_canonical, dispatcher
  walker hits I-shortcut: `Ok hyp`.
+ `expected = ks.checked_apply codFn hyp`. codFn = `({_} -> Nat)`,
  walker reduces to `Nat`. Returns `Ok Nat`.
+ `result = hyp` (a neutral), `expected = Nat`. `is_neutral hyp = TT`.
  Compare `Nat` to `neutral_type hyp = Nat`. `tree_eq Nat Nat = TT`.
  Returns `Ok TT`.
+ `q_guard_fn`: returns bare `TT`. Test passes.

== Eliminator on closed Bool: `bool_rec id_motive 0 1 TT = 0`

```
bool_rec := λ motive ct cf target → target motive ct cf
TT := λ motive ct cf → ct
```

Reduce: `bool_rec (λ_→Nat) 0 1 TT = TT (λ_→Nat) 0 1`. TT is the
K-style projection: `(λ_→Nat) 0 1` ignores the third arg. Wait,
let me re-trace. TT = λ motive ct cf → ct. Apply to `(λ_→Nat) 0 1`:
substitutes motive=`(λ_→Nat)`, ct=`0`, cf=`1`. Body = `ct = 0`.

Result: `0`. ✓

This is just function application; no special "stuck check"
machinery in `bool_rec`.

== Eliminator on hypothesis Bool: stuck via hyp_reduce

Suppose we have a hypothesis `b_hyp : Bool` minted during
Pi-checking via `cert_make_bool_hyp h_id`. b_hyp's stored type
is `Bool_template h_id` --- a literal Pi expression whose
codomain references `h_id` in the target position.

Apply `bool_rec motive 0 1 b_hyp`:

```
bool_rec motive 0 1 b_hyp = b_hyp motive 0 1
```

Step-by-step under the dispatcher:

+ `apply(b_hyp, motive)`: dispatcher sees `b_hyp`'s signature
  matches `raw.hyp_reduce`, routes to `q_hyp_reduce_fn`. The
  handler reads b_hyp's stored type (`Bool_template h_id`),
  recognises it as Pi (it literally is, after the recq
  encoding's mutual recursion handles self-reference). Computes
  new stored type as the codomain of the outer Pi applied to
  `motive`. Returns a new neutral b_hyp_1.
+ `apply(b_hyp_1, 0)`: same pattern, extends spine, computes
  next codomain.
+ `apply(b_hyp_2, 1)`: extends spine again. Final stored type
  is `motive h_id` (the per-value codomain).

The result is a neutral with stored type `motive h_id` and full
spine `[motive, 0, 1]`. *Same shape as the previous design's
`cert_make_stuck (motive h_id) h_id`.* No special handling
needed --- the per-value Pi template plus standard Pi-spine
extension produce the right answer.

#note[
  *Verified experimentally.* `lib/scott_per_value.disp` (11
  tests) and `lib/scott_dependent_motive.disp` (8 tests) confirm
  this trace works under the existing kernel without any
  modifications. The recq encoding makes Bool's outer form a
  literal Pi; `q_hyp_reduce_fn` handles it via the standard
  Pi-spine extension code path.
]

== refl on `Eq Nat 5 5`: hash-cons equality automatic

```
refl := λ motive c_refl → c_refl
```

Type-check `refl : Eq Nat 5 5`:
- Eq_T's body unfolds: motive_typed_continuation expecting motive,
  c_refl, returning `motive 5 self`.
- refl as λ motive c_refl → c_refl: takes motive, returns
  identity-on-c_refl, which when applied returns c_refl of type
  motive 5 refl.
- Expected codomain: motive 5 self where self = refl.
- These are equal under hash-cons identity (motive 5 refl =
  motive 5 refl).
- Type-check passes. ✓

For `refl : Eq Nat 5 6`: same trace except expected codomain is
`motive 6 self`. Under hash-cons identity, motive 5 refl ≠
motive 6 refl (the spine extension of motive_hyp differs in its
first arg). Type-check fails. ✓

This is the same semantics as the previous design's
`tree_eq x y` check in `q_eq_fn`, but achieved through standard
Pi-checking with no kernel-level Eq handler.

== Rejecting forged neutral: `(Type 0) (Hyp Nat 0) = FF`

A user attempting to forge a Nat-typed neutral.

+ Source `Hyp Nat 0` compiles to
  `wait kernel.hyp_reduce (make_neutral_meta Nat 0)`.
+ Source `Type 0` compiles to `guard (wait kernel_ref.guarded_type 0_ord)`.
+ Test evaluates `(Type 0) (Hyp Nat 0)`. Raw apply fires
  `q_guard_fn` with `v = Hyp Nat 0`.
+ `q_guard_fn` runs `scan_no_neutral v`. `v` IS a neutral.
  `scan_no_neutral` returns `FF`. `q_guard_fn` returns bare `FF`.
+ Test sees `FF`, fails the `= TT` assertion as expected.

= Open problems

== Hypothesis identity collision risk

When `cert_make_T_hyp h_id` mints a hypothesis with stored
type `T_template h_id`, the `h_id` is required to be unique
across the kernel's lifetime to ensure type-distinctness
(two hypotheses with the same identity have hash-cons-identical
stored types and are indistinguishable).

Currently `h_id` is the Pi metadata of the binder (per the
existing `q_pi_fn` convention). This is unique per binder by
construction. As long as binder construction is deterministic
and metadata is canonical, no collisions. Worth a sanity test
in IMPLEMENTATION_PLAN's Phase 1.

== HoTT extensions

The current kernel uses MLTT-style narrow `Eq`: only `refl`
inhabits `Eq A x y`, and refl-evidence requires hash-cons
identity of `x` and `y`.

This is sufficient for current goals. It is *not* a stepping
stone to HoTT, and the spec should not pretend otherwise.
Migration to HoTT would require:

- A new `Eq` checker that recognizes equivalence-shaped proofs
  (univalence) and path-constructor inhabitants (HITs).
- Abandoning hash-cons identity as the basis for definitional
  equality on type-valued terms.
- Generalizing `eq_J` to dispatch on the proof's structure.

Migration is not additive. If HoTT becomes a goal, plan it as a
separate kernel design.

== Universe inference at the elaborator

Current source surface requires explicit universe ranks. A future
elaborator could synthesize ranks at use sites via constraint
solving. This is Coq-style implicit universe polymorphism.
Out of scope for the initial kernel; orthogonal to soundness.

== Higher universes (beyond $epsilon.alt_0$)

CNF Ord can name ordinals up to $epsilon.alt_0$. Higher ordinals
require richer constructors --- e.g., Veblen functions. Each
extension is mechanical; defer until a use case demands.

== Bound-consulting identity table extension

The kernel ships with bound-consulting identities for canonical
limit ordinals up to some chosen ceiling. Users wanting bounded
levels at custom limits provide additional comparison-handler
identities. Each new identity ships with its own targeted
soundness test.

== Native walker for performance

The in-language dispatcher walks each apply step under parametric
mode at ~70× the cost of raw apply. A native implementation in
`tree.ts` would cut this to ~1-2×. Add when profiling demands.

Note: with data-as-eliminator, more work is routed through
Pi-application, so the native-walker need may be more pressing
than under the previous design. Measure during Phase 1.

== Kernel extensibility

The current kernel has a *closed* registry. User-extensible
registration is deferred to `KERNEL_EXTENSIBILITY_PLAN.md`.

Data-as-eliminator simplifies extensibility: adding a new inductive
type means writing a wait-body definition and constructors in
user code, no kernel changes required. Only types whose checkers
fundamentally cannot be data-as-eliminator (like the ordinal
comparisons or universe stratification) need kernel registration.
