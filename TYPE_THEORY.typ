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
  *Status.* This document specifies the post-soundness-fix design,
  including parametric checked evaluation, certified eliminators, and
  ordinal-ranked universes. Some sections describe behaviour not yet
  in `main`; the implementation tracking is in
  #raw("SOUNDNESS_FIX_PLAN.md"). Where this document and the
  reference implementation disagree, this document is authoritative
  and the implementation is a bug.
]

= Overview

Disp is a dependently-typed language built on tree calculus. There
is no built-in type system, no built-in checker, no built-in
notion of "well-formed term." All of those are tree programs that
the runtime executes via a single primitive operation, `apply`.

The language commits to four interlocking ideas:

+ *Types as predicates.* A type is a function from values to
  `TT`/`FF`. Type checking is function application:
  `T(v) = TT` means "$v$ has type $T$."

+ *Wait-encoded types.* A type is structurally
  `wait(checker)(metadata)`. The checker's hash-cons identity is
  the type-former tag; the metadata carries parameters.

+ *Hypothesis carry their types.* Open terms (variables under a
  binder) are represented as opaque "neutral" trees that store
  their type internally. Type-checking under a binder reduces to
  hash-cons-identity comparison of stored types against expected
  types --- the *H-rule*.

+ *Parametric checked evaluation.* User-supplied terms are
  evaluated under a non-standard reduction discipline that bans
  reflection on hypotheses (`triage` on a neutral fails) and
  forging of hypothesis tokens (`stem` rule rejects construction
  of fork-shaped values with the kernel's hypothesis signature).

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
- *Subject reduction* holds because reductions on stuck values
  follow the same rules as reductions on closed values. Stuck
  propagation is referentially transparent.
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

The first three give us a usable type system. The fourth gives us
soundness: ordinary tree-calculus reduction has no notion of
"don't peek inside hypothesis," so a checker that runs user code
via raw `apply` admits forgery. Checked evaluation is the rule
set that closes those holes while keeping closed-program semantics
identical to raw evaluation.

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
  one explicit check, not three; the structural-induction
  argument in @soundness-theorem makes this precise.
]

=== Soundness carve-out vs performance carve-out <soundness-carve-out>

Two different kinds of "exception to the standard rules" exist in
the framework. They have different correctness contracts and
should not be conflated.

A *soundness carve-out* is a parametric-mode rule that exists
specifically to make the type system *expressible*. Removing it
would break legitimate programs. The only soundness carve-out
currently is the *I-shortcut*: $apply(I, x) -> x$, special-cased
because $I = "fork(fork(LEAF, LEAF), LEAF)"$ is structurally a
triage shape and a strict triage-on-neutral rule would reject
identity on a hypothesis. Polymorphic identity is a legitimate
program, so the walker carves out exactly this case.

A *performance carve-out* is a runtime fast path that produces
*observably identical* results to the in-language reduction.
`tree_eq` uses one (the host captures its compiled tree id and
short-circuits via hash-cons identity). A future "native walker"
in `tree.ts` would be another. Performance carve-outs are
optional --- removing them yields the same answers, just slower.

The contract for soundness carve-outs is "the language admits
this program"; the contract for performance carve-outs is "this
optimization observably equals the spec." A soundness carve-out
is part of the language definition; a performance carve-out is
not. Performance carve-outs should be added reactively (when
profiling shows a specific shape is the bottleneck), not
preemptively.

== Trusted primitives

A *trusted primitive* is a four-tuple `(signature, checker,
eliminator, identity)` registered with the framework:

- *signature* --- a hash-cons-stable tree id used by the
  dispatcher to recognize "this is one of mine, run in raw mode."
- *checker* --- the kernel handler that validates a value as
  belonging to this type. Runs in raw mode after dispatch.
- *eliminator* --- the user-callable face that lets parametric-mode
  code case-analyze on concrete instances and defers via
  `cert_make_stuck` on neutrals.
- *identity* --- the kernel-private mint capability for
  constructing canonical instances (for type-formers) or stuck
  results (for eliminators).

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
    [Pi],         [`kernel.pi`],            [`q_pi_fn`],            [Pi-application + `q_hyp_reduce_fn`],
    [Nat],        [`kernel.nat`],           [`q_nat_fn`],           [`nat_rec`],
    [Bool],       [`kernel.bool`],          [`q_bool_fn`],          [`bool_rec`],
    [Eq],         [`kernel.eq`],            [`q_eq_fn`],            [`eq_J`],
    [Ord],        [`kernel.ord`],           [`q_ord_fn`],           [`ord_rec`],
    [`OrdLt k`],  [`kernel.ord_lt_type`],   [`q_ord_lt_type_fn`],   [`ord_rec` (via Ord)],
    [Universe (core)],   [`kernel.core_type`],     [`q_core_type_fn`],     [`wait_rec`],
    [Universe (guarded)],[`kernel.guarded_type`],  [`q_guarded_type_fn`],  [`wait_rec`],
    [Hypothesis], [`kernel.hyp_reduce` (raw)], [`q_hyp_reduce_fn`], [(none --- kernel-only)],
    [Guard layer],[`kernel.guard`],         [`q_guard_fn`],         [(boundary, not user-side)],
    [Elim sentinel],[`kernel.elim_fail`],   [(no-op; always FF)],   [(none)],
    [Ord comparisons],[`kernel.ord_lt`, `kernel.ord_le`, `kernel.ord_max`], [primitives], [(none --- direct invocation)],
  ),
  caption: [Trusted primitives in the kernel registry.],
)

The dispatcher is a fold over this registry: for each entry, check
`has_sig entry.sig f`; on match, route through raw apply; otherwise
apply the parametric rules. `q_core_type_fn`'s signature dispatch
is the same fold with per-entry validators instead of raw apply.
`wait_rec` is the same fold exposed to user position with
neutral-deferral. The dispatcher, the universe checker, and
`wait_rec` are three views of one structural object: a pattern
match over the registry's signatures.

=== Reflection helpers vs kernel primitives

The kernel record above contains only what's *load-bearing for
type-checking*: the dispatcher, the type-former checkers, the
hypothesis machinery, the public boundary, and the eliminator
handlers. Reflection helpers --- `pi_rank`, `pi_dom`, `pi_cod_fn`,
`universe_rank`, `is_pi`, `is_universe`, `is_eq`, `hasguard` ---
are *user-callable tree programs* defined in `kernel.disp` on top
of `wait_rec`. They are not kernel record fields. The kernel
itself never invokes them: type-checking dispatches on signatures
directly via `has_sig`, and rank checks happen via `ord_le`
during recursive predicate evaluation, not via explicit rank
computation.

Helpers ship in `kernel.disp` for convenience: most user code that
inspects types wants `is_pi T` rather than rolling its own
`wait_rec` invocation. But helpers are equivalent in trust to any
other user code --- they run under the walker (parametric mode)
when invoked from user position, and through the
parametricity-respecting `wait_rec` interface when crossing the
neutral barrier.

== The public boundary

Public types are *guarded* wait-values: `wait kernel.guard core`
where `core` is a kernel-internal predicate (e.g.,
`wait kernel.nat t`). `Nat`, `Bool`, `Eq`, `Pi`, `Type n` are all
guarded.

`q_guard_fn` is the *only* checker that returns bare `TT`/`FF` (so
that `Nat 5 = TT` works as a literal test). It performs two
operations:

+ Run `scan_no_neutral` on the value. Pre-existing neutral roots
  in the input are rejected.
+ Run `checked_apply core v` to validate the value against the
  underlying core predicate. The result is a `CheckedResult` (see
  @checkedresult); `q_guard_fn` unwraps `Ok TT` to bare `TT`,
  everything else (`Ok FF`, `Ok stuck-bool`, `Fail`) to bare `FF`.

The entry scan is the *static* analog of the parametric-mode
fork-formation rule: values entering at the public boundary cannot
contain pre-existing neutrals that the kernel didn't mint.
Together, the static scan and the dynamic walker cover both
timescales of forgery: pre-built attacks and runtime construction.

Cores are kernel-private (declared with `let` in `kernel.disp`).
User code cannot name them directly --- the public surface is
guarded predicates only. This is load-bearing: a publicly-named
core would let user code bypass `q_guard_fn` and feed forged
neutrals directly to the bare-checker handler.

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
  a hypothesis. Used for polymorphic universe ranks; see
  @universes.
- An arbitrary tree value (e.g., a new neutral from
  `q_hyp_reduce_fn`).

This single sum is enough to express every outcome the framework
needs: `Fail` is the parametric-mode rejection ("user code did
something disallowed"); `Ok TT` / `Ok FF` are concrete predicate
answers; `Ok stuck-bool` is a deferred predicate answer.

== Composition helpers

CPS continuations chain CheckedResult-producing operations:

```disp
must_ok_any  := {r, k} -> if is_ok r then k (ok_value r) else Fail
must_ok_tt   := {r, k} -> if is_ok r ∧ ok_value r = TT then k t else Fail
must_ok_concrete_tt := {r, k_concrete, k_stuck} ->
  if is_ok r then
    if ok_value r = TT then k_concrete t
    else if is_neutral (ok_value r) then k_stuck (ok_value r)
    else Fail   // concrete FF
  else Fail
```

`must_ok_any` threads any `Ok` payload into the continuation.
`must_ok_tt` requires the payload to be exactly `TT`. The
*concrete* variant distinguishes three cases: concrete `TT`,
stuck (a neutral payload), or anything else (concrete `FF` or
`Fail`).

Continuations passed to these helpers must be closed functions
(parameter list saturates the body's free variables). A
`{_} -> body_using_outer_vars` continuation is *not* a thunk in
tree calculus --- bracket abstraction over outer free vars produces
an `S(K K) ...` chain that evaluates eagerly. Use `{x} -> ...`
where `x` actually appears in the body, or use `match` (which
desugars to closed-arm select-then-apply with proper free-variable
capture).

== Handler return convention

| Handler family | Returns | Dispatcher branch |
|---|---|---|
| `q_guard_fn` (public boundary) | bare `TT`/`FF` | `Ok (f x)` |
| All type-checker handlers (`q_pi_fn`, `q_nat_fn`, `q_bool_fn`, `q_eq_fn`, `q_core_type_fn`, `q_guarded_type_fn`, `q_ord_fn`) | `CheckedResult` | `f x` (no wrap) |
| `q_hyp_reduce_fn` | bare new neutral | `Ok (f x)` |
| Eliminator handlers (`q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`, `q_ord_rec_fn`, `q_wait_rec_fn`) | `CheckedResult` | `f x` (no wrap) |
| Walker default | `CheckedResult` | (recursive call) |

Every type-checker handler returns CheckedResult internally so
that stuck-bools and Fail propagate uniformly through must_ok_*
chains. The bare-returning handlers are exactly two:
`q_guard_fn` (public boundary) and `q_hyp_reduce_fn` (wrapped by
the dispatcher into `Ok new_neutral`).

Mixing this convention up is an unsoundness-class bug: wrapping a
handler that already returns CheckedResult double-wraps to
`Ok (Ok TT)`, and downstream `must_ok_tt` sees `is_ok = TT` and
`ok_value = Ok TT ≠ TT` --- it Fails when it shouldn't. The
dispatch table comment in `kernel.disp` is the canonical place to
document each handler's convention.

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
has exactly one CNF tree, so hash-cons identity on Ord values
matches ordinal equality. Constructors:

- `0_ord : Ord`
- `omega_plus : Ord -> Ord -> Ord` --- $omega^alpha + beta$ with
  invariant `leading_exp β ≤ α`.

Examples:

- $0 = "0_ord"$
- $1 = omega^0 + 0 = "omega_plus 0_ord 0_ord"$
- $omega = omega^1 + 0$
- $omega + 1 = omega^1 + omega^0$
- $omega dot 3 = omega^1 + omega^1 + omega^1 = omega + omega + omega$
- $omega^2 = omega^("omega_plus 0_ord 0_ord") + 0$

Closed below-ω ordinals (`0_ord`, `succ_ord 0_ord`, etc., where
`succ_ord α = omega_plus 0_ord α`) share tree shapes with Nat
values: source-level `Type 0`, `Type 1` compile unchanged. The
`Ord` checker validates the CNF invariant; not every tree shape
is a valid Ord.

#note[
  *Why CNF, not Brouwer trees.* The Brouwer-tree variant of Ord
  uses `lim_ord : (Nat → Ord) → Ord` as a constructor. It is
  predicatively well-formed (Kraus--Forsberg--Xu MFCS 2021,
  `Brw : U₀`) but non-canonical: many trees represent the same
  ordinal (`lim_ord (λn. n)` and `lim_ord (λn. 2n)` are both
  $omega$), so hash-cons identity is finer than ordinal equality.
  CNF gives canonical encodings, which matters because the kernel
  uses hash-cons identity for universe checks. Two structurally
  distinct Pi types with the "same" ordinal rank under Brouwer
  trees would be at different universes per the kernel; CNF
  eliminates this anomaly.

  The cost: CNF has no general `lim_ord f` for arbitrary `f` ---
  computing the supremum of an arbitrary function is not
  algorithmic. This restricts expressible ordinals to those
  reachable via `0_ord`, `succ_ord`, `omega_plus`, `+`, and `max`.
  This is sufficient for universe ranks of polymorphic functions,
  which only ever produce specific ordinals like
  $omega$, $omega + 1$, $omega^2$ from the syntactic structure of
  the function type (see @poly-universe).
]

== Where Ord lives

`Ord` lives in `Type 0`. CNF Ord is an inductive type whose
constructors (`0_ord`, `omega_plus`) take only `Ord` recursively,
so predicativity holds at `Type 0`. The CNF invariant
(`leading_exp β ≤ α`) is checked by `Ord`'s predicate; it does
not introduce a universe shift.

== Polymorphic-universe functions <poly-universe>

Polymorphic-rank functions come in two flavors: *unbounded* and
*bounded*. The kernel handles them differently --- unbounded gets
stuck `pi_rank`; bounded gets closed `pi_rank` derivable from the
bound.

=== Unbounded polymorphism: stuck `pi_rank`

A function `{r : Ord} → Type r → Type r` mathematically lives in
$"Type" omega$: $sup_(r:"Ord") "succ_ord" r = omega$. But the
*kernel* does not compute closed $omega$ for `pi_rank` of such a
type --- it produces a *stuck Ord*. Here's why.

`pi_rank` is structural recursion via `wait_rec`. For
`{r : Ord} → Type r`:

+ Walks the Pi: dom = `Ord`, codFn = `{r} → Type r`.
+ Computes `cod_at_canonical_hyp = codFn(r_hyp) = Type r_hyp`,
  where `r_hyp = cert_make_hyp Ord meta`.
+ Recurses: `pi_rank Ord = 0_ord` and
  `pi_rank (Type r_hyp) = succ_ord r_hyp`. The latter is a
  syntactic CNF tree containing a hypothesis (form 2 below).
+ Combines via `ord_max 0_ord (succ_ord r_hyp)` --- one closed,
  one stuck → returns *stuck Ord*.

So unbounded polymorphic-rank types have *stuck `pi_rank`* in the
kernel. Mathematically the rank is $omega$; computing $omega$
closedly would require recognising "supremum over Ord of
$alpha + 1$ is $omega$," which is a meta-theoretic identity not
derivable from structural recursion.

This is consistent with @soundness-not-completeness: the kernel
provides correct stuck propagation as the floor, and lets users
opt into bounded levels (below) when they want closed answers.

=== Bounded polymorphism: closed `pi_rank` via `OrdLt k`

A function `{r : OrdLt omega} → Type r → Type r` is the bounded
form: the polymorphic rank `r` is constrained to be strictly less
than $omega$. Here `pi_rank` *can* compute closed $omega$:

+ Walks the Pi: dom = `OrdLt omega`, codFn = `{r} → Type r`.
+ Computes `cod_at_canonical_hyp = codFn(r_hyp)` where `r_hyp`
  has stored type `OrdLt omega`. The kernel knows
  `ord_lt r_hyp omega = TT` from the stored bound.
+ Recurses: `pi_rank Ord = 0_ord` (Ord lives in Type 0;
  `OrdLt k` also lives in Type 0 since it's just a predicate
  refining Ord), and `pi_rank (Type r_hyp) = succ_ord r_hyp`.
+ Combines via `ord_max 0_ord (succ_ord r_hyp)`. The kernel's
  augmented comparison consults `r_hyp`'s stored bound: from
  `r_hyp < omega` and `omega = omega_plus 1 0_ord` (a closed
  limit ordinal), it derives `succ_ord r_hyp < omega`
  structurally, hence `ord_max 0_ord (succ_ord r_hyp)` <
  `omega`. The whole expression is bounded by $omega$.
+ Conclusion: `pi_rank` returns `omega` (the bound), closed.

Tests at the polymorphic level work for bounded forms:
`(Type omega) ({r} -> ...) = TT` passes if the polymorphic
function genuinely inhabits `Type omega`.

#note[
  *How the kernel derives "succ_ord r_hyp < omega" from "r_hyp <
  omega":* `ord_lt`'s handler, when invoked on a hypothesis-
  typed Ord with a bound stored type (`OrdLt k'`), consults the
  bound. If the comparison's right side is a closed limit
  ordinal $omega = "omega_plus" 1 "0_ord"$, and the bound
  asserts `r_hyp < omega`, then `succ_ord r_hyp` (which equals
  `omega_plus 0_ord r_hyp`, a CNF expression with leading
  exponent 0) is structurally $< omega$ regardless of `r_hyp`'s
  specific finite value. This is one specific kernel-built
  identity, not a general supremum-reasoning engine.

  Other limit ordinals ($omega + omega$, $omega^2$, etc.) admit
  similar identities. The kernel's bound-consulting logic
  enumerates them as needed; users who want bounded levels at
  custom ordinals provide the comparison primitive's identity
  rules.
]

=== Closed (non-polymorphic) ranks

Closed ranks are computed exactly with no special handling. For
instance:
- `pi_rank (Pi Nat ({_} -> Nat))` = `ord_max 0_ord 0_ord` = `0_ord`.
- `pi_rank (Pi (Type 0) ({_} -> Type 0))` = `ord_max 1 1` = `1`.
- `pi_rank (Pi (Type 0) ({A} -> A))` = `ord_max 1 0` = `1`.

These are the cases where universe polymorphism via `Type k` for
specific closed `k` works seamlessly.

== Stuck comparisons

`ord_lt`, `ord_le`, `ord_max` are kernel-registered primitives.
Their handlers receive two `Ord` arguments (validated as Ord by
the dispatcher routing). Four cases, in order:

+ Both arguments concrete (closed CNF): compute concretely,
  return `Ok TT` / `Ok FF` / `Ok ord_value`.
+ One argument is a hypothesis with `OrdLt k` stored type and
  the comparison is derivable from the bound: return concrete
  result. For example, `ord_lt r_hyp omega` where
  `r_hyp : OrdLt omega` returns `Ok TT` (the bound directly
  asserts this). `ord_le (succ_ord r_hyp) omega` for the same
  bound also returns `Ok TT` via the kernel-built identity
  "$x < omega ==> "succ_ord" x ≤ omega$" for canonical limit
  $omega$.
+ Either argument is a hypothesis (raw neutral) and the bound
  doesn't determine the answer: mint a stuck result. For
  `ord_lt`/`ord_le`: return
  `Ok (cert_make_stuck Bool stuck_meta)`. For `ord_max`: return
  `Ok (cert_make_stuck Ord stuck_meta)`.
+ One argument is itself a stuck-Ord (previously-deferred
  result): recurse into the stuck structure if possible,
  otherwise propagate stuck.

The `stuck_meta` encoding tags the operation: `(LtTag, a, b)`,
`(LeTag, a, b)`, `(MaxTag, a, b)`. This ensures distinct
operations on the same arguments produce distinct stuck-Bools.
Without tagging, `Eq Bool (ord_lt 5 r) (ord_le 4 r)` would
hash-cons-equal under the runtime `tree_eq` fast path even when
they're semantically distinct.

#note[
  *Bound-consulting is what gives bounded levels their power.*
  Without case 2, the kernel would treat hypothesis-typed Ord
  values uniformly (always stuck on comparison), and bounded
  levels would buy nothing over unbounded. The bound-consulting
  case is where the comparison handler reads the hypothesis's
  stored type, recognises an `OrdLt k`-shape stored type, and
  uses the bound to derive concrete answers.

  The kernel ships with bound-consulting identities for canonical
  limit ordinals ($omega$, $omega + 1$, $omega dot n$, $omega^2$,
  $omega^omega$, etc., up to $epsilon.alt_0$). Adding more
  identities is mechanical (one new structural rule per limit
  ordinal). Identities for non-canonical bounds (e.g., user-
  defined ordinal expressions) fall through to stuck.
]

== Subject reduction <subject-reduction>

Disp's universe polymorphism uses unbounded `r : Ord` with
stuck-comparison semantics. Subject reduction follows because:

+ Levels are ordinary terms in `Ord`, evaluated under the same
  rules as any other value.
+ Stuck `Ord` values (form 2 or 3 of @ord-forms) propagate
  through reductions and comparisons consistently with closed
  values --- they are referentially transparent.
+ Universe checks `Type k` are `apply` operations, not
  side-conditions; substitution cannot turn a "valid" check
  into an "invalid" one because there are no rules gated on
  side-conditions.

Compare to BCDE (arXiv 2212.03284), whose SR breaks at
*constraint-indexed products* `[α < β] N type` --- formation
gated on `α < β` being loop-free, which substitution can violate.
Disp has no such construct. Compare to TTBFL (arXiv 2502.20485),
which fixes SR by baking bounds into level types
(`r : Ord< k`); their `Trans` and `Cumul` rules are
non--syntax-directed, conflicting with Disp's
"type-checking is one apply" invariant. Disp avoids both
problems by accepting stuck propagation rather than chasing
closed-form completeness (per @soundness-not-completeness).

#note[
  *No data-driven level computation.* Disp does not provide
  `lub : List (Σℓ. Type ℓ) → Ord` --- a function that takes a
  runtime list of types-at-various-levels and computes their
  supremum. This is the only restriction relative to TTBFL, and
  it is consistent with the soundness-not-completeness stance:
  the kernel does not promise to compute new levels from
  runtime data. If a use case demands this, monomorphize at
  elaboration time.
]

= Dispatcher and walker

== The dispatcher

`q_checked_apply_fn` (the dispatcher, exported as `checked_apply`)
routes every application through one of two paths:

+ *Signature-recognized*: if `f`'s signature matches a registered
  kernel handler, the dispatcher invokes the handler via raw
  apply: `f x` runs in raw mode, the handler trusts `f` and
  validates `x` internally.
+ *Default*: otherwise, the dispatcher walks the apply structure
  step-by-step under parametric mode. This is `checked_raw_apply`
  --- the *walker*.

Both paths return `CheckedResult`. The signature-recognized path
either wraps the bare handler result in `Ok` (for handlers that
return bare values --- type checkers, `q_hyp_reduce_fn`) or
returns the handler's `CheckedResult` directly (for handlers that
return `CheckedResult` --- eliminators).

```disp
q_checked_apply_fn = {ks, raw, query} -> fix ({self, f, x} ->
  select_chain
    (case (has_sig raw.hyp_reduce f)  ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.guard f)        ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.pi f)           ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.nat f)          ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.bool f)         ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.eq f)           ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.ord f)          ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.ord_lt_type f)  ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.core_type f)    ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.guarded_type f) ({_, f, x} -> Ok (f x)))
    (case (has_sig ks.bool_rec f)     ({_, f, x} -> f x))
    (case (has_sig ks.nat_rec f)      ({_, f, x} -> f x))
    (case (has_sig ks.eq_J f)         ({_, f, x} -> f x))
    (case (has_sig ks.ord_rec f)      ({_, f, x} -> f x))
    (case (has_sig ks.wait_rec f)     ({_, f, x} -> f x))
    (case (has_sig ks.ord_lt f)       ({_, f, x} -> f x))
    (case (has_sig ks.ord_le f)       ({_, f, x} -> f x))
    (case (has_sig ks.ord_max f)      ({_, f, x} -> f x))
    (case (has_sig ks.elim_fail f)    ({_, f, x} -> Ok FF))
    // Default: walker
    ({self, f, x} -> checked_raw_apply self f x)
    self f x)
```

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

The two parametric extensions are at the stem rule (rule (b),
fork-formation rejecting neutral roots) and the triage rule (rule
(a), triage on neutral fails). The S rule and triage-fork case do
not perform direct constructor checks: they recurse via `self`,
and the inner step that ultimately constructs a fork hits the
stem-rule check.

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

Fork-producing rules:

+ *Stem rule.* Explicitly checks `neutral_root r` on the
  constructed fork; returns `Fail` if matched. Direct check.

+ *S rule.* The result is the value of a recursive `self c x`,
  `self b x`, then `self cx bx`. Any fork produced inside those
  recursive calls is constructed (and therefore checked) by
  *its* stem/S/triage rule. By induction, no neutral-shaped fork
  escapes.

+ *Triage-fork rule.* Same shape: result is recursive
  `self b l`, then `self bl r`.

Fork-passing rules:

4. *K rule.* The payload is either an original input (caught by
   the public-boundary entry scan) or the result of a prior
   `checked_apply` call (already checked). Never a freshly-
   constructed neutral.

5. *Triage-leaf, triage-stem.* Same provenance argument.

6. *I-shortcut.* Returns its argument unchanged. Same as K.

Non-fork-producing rules:

7. *Leaf rule.* Produces a stem. Stems are never fork-shaped
   neutrals (post-`is_neutral` tightening).

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
  helpers that mint neutrals (`cert_make_hyp ty id` and
  `cert_make_stuck T payload`) are `let`-bound in `kernel.disp`
  and not exported. Even if a user could name them, calling them
  from user position would route through the dispatcher's walker
  default, where the inner `wait raw.hyp_reduce ...`
  construction hits the stem-rule's neutral-root check and
  fails. So the soundness theorem is robust against accidental
  exposure: privacy is the convention, but the walker is the
  enforcement.
]

== Soundness carve-out: I-shortcut

`I = fork(fork(LEAF, LEAF), LEAF)` is structurally a triage
shape. Without the I-shortcut, `apply(I, hyp)` would hit the
triage rule's neutral check and `Fail` --- but identity on a
hypothesis is a legitimate program. The walker carves out
`apply(I, x) = Ok x` directly, recognized via
`tree_eq f I_canonical`. The runtime `tree_eq` fast path makes
this O(1).

This is the *only* soundness carve-out in the walker. Other
desirable shortcuts (K, S, triage on closed values) are not
carve-outs --- they fall out from the rules without modification.

== Performance carve-outs

Performance carve-outs are runtime fast paths in `tree.ts` that
produce *observably identical* results to the in-language
walker. None are required for correctness; they exist when
profiling shows specific bottlenecks.

The current candidate is a *native walker* in `tree.ts`,
mirroring the host `tree_eq` fast path: the runtime captures the
dispatcher's compiled tree id at boot and routes calls through
native dispatch logic. Cuts the per-step constant from the
in-language walker's ~70× to ~1-2× over raw apply. Add when
profiling demands it.

= Type formers

== Pi

*Signature*: `kernel.pi`. *Checker*: `q_pi_fn`.

Pi metadata: `make_pi_meta(domain, codFn)` where `domain` is a
core type and `codFn : domain → core_type`. The public
constructor `Pi A B` unguards both arguments via
`unguard_checked` so metadata stores cores.

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

*Application of a Pi-typed neutral*: handled by `q_hyp_reduce_fn`
(see below). When `(Hyp PiType id) v` is evaluated, the handler
extends the neutral's spine with `v` and computes the result type
as `pi_meta_cod_fn(PiType_meta) v`.

== Nat

*Signature*: `kernel.nat`. *Checker*: `q_nat_fn`.

Nat values: `0_nat = LEAF` and `succ_nat n = fork(LEAF, n)`. The
checker is a recursive predicate testing for these shapes. The
H-rule fires on hypothesis-typed values.

*Public constructor*: `Nat = guard core_Nat`. No metadata.

*Eliminator*: `nat_rec rank motive base step target`. See @elims.

== Bool

*Signature*: `kernel.bool`. *Checker*: `q_bool_fn`.

Bool values: `TT = LEAF` and `FF = stem(LEAF)`. Two-way check.
H-rule on hypothesis-typed values.

*Public constructor*: `Bool = guard core_Bool`. No metadata.

*Eliminator*: `bool_rec rank motive t_case f_case target`. See @elims.

== Eq

*Signature*: `kernel.eq`. *Checker*: `q_eq_fn`.

Eq metadata: `make_eq_meta(A, x, y)` where `A` is the type, `x`
and `y` are values. The checker validates that the proof argument
is `refl = LEAF` and that `tree_eq x y = TT` (under the H-rule:
hash-cons identity matches propositional equality for closed
values).

*Public constructor*: `Eq A x y = guard (core_Eq (unguard_checked A) x y)`.
*Constructor*: `refl = LEAF`.

*Tree-equality under raw vs walker.* `q_eq_fn`'s body calls
`tree_eq` on the metadata's `lhs` and `rhs`. Because the handler
runs in raw mode (after dispatcher routing), this `tree_eq` uses
the host runtime's hash-cons fast path --- O(1) identity
comparison. User-position `tree_eq` calls run under the walker,
which descends recursively and fails on hypothesis-containing
inputs (this is what closes the reflective `tree_eq` attack).
The same source-level `tree_eq` definition behaves differently
in the two modes; this is by design and is the correct
soundness-vs-performance split.

*Eliminator*: `eq_J rank A_rank A x motive base y p`. Path
induction. See @elims.

== Ord

*Signature*: `kernel.ord`. *Checker*: `q_ord_fn`.

Ord values are CNF trees. The checker validates:
- `0_ord = LEAF`, or
- `omega_plus α β` shape with the leading-exponent invariant.

*Public constructor*: `Ord = guard core_Ord`. No metadata.

*Three forms of Ord values* <ord-forms>. Anywhere `k : Ord`
appears (e.g., `Type k`, `succ_ord k`, an `ord_lt` argument),
`k` may be in any of three forms:

+ *Closed CNF*: `0_ord`, `omega`, `omega_plus 1 0_ord`. A closed
  tree with no hypothesis anywhere. Operations on closed Ords
  evaluate concretely.

+ *Syntactic CNF with embedded hypothesis*: e.g.,
  `succ_ord r_hyp = omega_plus 0_ord r_hyp` where `r_hyp` is a
  hypothesis. The outer `pair_fst` is the CNF constructor's tree,
  not `kernel.hyp_reduce` --- so this is *not itself a neutral*,
  but it contains one. The Ord checker on this value defers
  (returns `Ok stuck_bool`) because it cannot fully verify the
  CNF invariant without knowing the hypothesis's structure.

+ *Stuck Ord*: `cert_make_stuck Ord meta`. A wait-rooted
  neutral with stored type `Ord`, produced when an operation
  like `ord_max` cannot decide. The Ord checker accepts it via
  the H-rule (signature match against canonical Ord checker).

All three forms are valid `Ord` values in the sense that
`Ord k = TT` (for forms 1 and 3) or `Ord k = stuck_bool` (for
form 2). Comparisons (`ord_lt`, `ord_le`, `ord_max`) on a
non-closed form propagate stuck.

*Below-ω convenience*: source-level Nat literals (`0`, `1`, `2`)
compile to canonical Nat trees, which are also valid Ord trees
(they happen to be CNF for finite ordinals). So `Type 0`, `Type 1`
work as expected.

*Above-ω naming*: Disp does not provide source-level syntax for
limit ordinals beyond Nat literals. Users who need `ω`, `ω+1`,
`ω^2`, etc. import library-defined helpers in `lib/ord.disp`:

```disp
omega       := omega_plus 1 0_ord                  // ω
omega_plus_one := omega_plus 1 (omega_plus 0_ord 0_ord)  // ω+1
omega_squared  := omega_plus omega 0_ord                 // ω^2
```

If frequent use justifies it, the parser can later add
syntactic shortcuts (`Type ω`, `Type ω+1`). For now, library
functions suffice.

*Eliminator*: `ord_rec rank motive zero_case omega_plus_case target`.
Two cases (matching CNF's two constructors). See @elims.

*Bootstrap*: `q_ord_fn` (the Ord checker) and `q_ord_rec_fn`
(the Ord eliminator) are independent kernel record fields. The
Ord checker validates CNF structure by raw triage on the value's
tree shape, not via `ord_rec`. The Ord eliminator dispatches on
the value via raw triage in its own handler body. Neither
references the other; either can be defined first in
`kernel.disp` (subject to `recq` lazy resolution of cross-field
references).

== OrdLt k (bounded ordinals)

*Signature*: `kernel.ord_lt_type`. *Checker*: `q_ord_lt_type_fn`.

`OrdLt k` is the type of ordinals strictly less than `k`. It is
a refinement predicate over `Ord`:

```
q_ord_lt_type_fn meta v = and (Ord v) (ord_lt v k)
```

where `meta = k` (the bound). The checker validates that `v` is
an Ord *and* `v < k`. Both checks may produce stuck results;
under stuck propagation, the conjunction is stuck.

*Public constructor*: `OrdLt = {k} -> guard (wait kernel_ref.ord_lt_type k)`.

*Universe*: `OrdLt k` lives in `Type 0` for any `k` (it is just a
refinement of `Ord`, no rank shift).

*Eliminator*: none directly. To case-split on an `OrdLt k`
value, treat it as an `Ord` (the underlying tree is identical)
and use `ord_rec`.

*Use as Pi domain*: when `Pi (OrdLt k) codFn` is checked, the
hypothesis has stored type `OrdLt k`. The kernel's comparison
primitives (`ord_lt`, `ord_le`) consult this stored bound when
running on the hypothesis, deriving concrete answers in many
cases. See @poly-universe for the polymorphic-rank application.

*Convention*: `Ord` and `OrdLt` are not interchangeable as type
constructors. A function `{r : Ord} -> ...` quantifies over all
ordinals (unbounded, stuck `pi_rank`). A function
`{r : OrdLt omega} -> ...` quantifies over countable ordinals
below $omega$ (i.e., natural numbers, in CNF terms), with
closed `pi_rank` derivable. Users pick the form that matches
their needs.

== Universe (Type k)

*Signatures*: `kernel.core_type` and `kernel.guarded_type`.
*Checkers*: `q_core_type_fn` and `q_guarded_type_fn`.

Universe metadata: a single Ord value (the rank).

`core_type rank` accepts core types (kernel-internal): `core_Nat`,
`core_Bool`, `core_Eq`, `core_Pi`, `core_Ord`, `wait
ks.core_type k` for `k < rank`, `wait ks.guarded_type k` for
`k < rank`, and certified neutral type variables whose stored
type is a universe of rank ≤ rank.

`guarded_type rank` accepts public types: `T = guard core` where
`q_core_type_fn rank core = TT`.

*Public constructor*: `Type k = guard (wait kernel_ref.guarded_type k)`.
The argument `k` must be an Ord; the elaborator typically
threads `0_ord`, `succ_ord 0_ord`, etc., from source-level
numeric literals.

*Source-level conventions.* Source-level `Type 0`, `Type 1`,
`Type omega` are always written with explicit ranks. Bare
`Type` (no rank) is a parser error, not a default to `Type 0`.
Rationale: making the rank explicit avoids ambiguity now and
leaves room for a future universe-inference elaborator to
synthesize ranks at use sites (à la Coq's implicit universe
polymorphism), which would conflict with a `Type = Type 0`
default. When such an elaborator lands, source-level `Type`
without a rank can become an inference variable.

*Rank check via stuck comparison*: when checking universe
membership, `q_core_type_fn` calls `ord_le rank_target rank_param`.
If both ranks are concrete, the result is `Ok TT` / `Ok FF`. If
either is a hypothesis-typed Ord, the result is
`Ok (cert_make_stuck Bool stuck_meta)`. Stuck Bools propagate up
through universe checks; at the public boundary they fail any
`= TT` test (since they're not literally `TT`), so polymorphic-rank
programs require concrete instantiation at test sites.

= Eliminators <elims>

All eliminators take an explicit `rank : Ord` argument so the
kernel can construct the motive's expected type internally
without trusting user-supplied type metadata. Each eliminator is
a kernel record entry with a wait-value-bundled wrapper for user
position.

#note[
  *Motive parameter's universe.* For an eliminator over a target
  type `A : Type A_rank` with motive returning `Type rank`, the
  motive parameter itself has type `A → Type rank`, which lives
  in `Type (max A_rank (succ_ord rank))`. This is one universe
  above what the motive *produces*. The kernel constructs this
  expected type internally from `A_rank` and `rank` (both
  user-supplied) and validates the user's motive against it.
  Users do not write the motive's type; they only write the
  motive value and the two ranks.

  For `bool_rec` and `nat_rec`, `A_rank = 0_ord` (Bool and Nat
  both live in Type 0), so the motive's type is just
  `Type (succ_ord rank)`. Only `eq_J` exposes `A_rank` as an
  explicit argument because Eq's `A` parameter can be at any rank.
]

== bool_rec

```disp
bool_rec : (rank : Ord) →
           (motive : Bool → Type rank) →
           (t_case : motive TT) →
           (f_case : motive FF) →
           (target : Bool) →
           motive target
```

Handler: validates `rank : Ord`, `target : Bool`,
`motive : Bool → Type rank`, `t_case : motive TT`,
`f_case : motive FF`. Dispatches on target structurally; on
hypothesis target, mints `cert_make_stuck (motive target) target`.

== nat_rec

```disp
nat_rec : (rank : Ord) →
          (motive : Nat → Type rank) →
          (base : motive 0) →
          (step : (k : Nat) → motive k → motive (succ k)) →
          (target : Nat) →
          motive target
```

Same shape; dispatches on `0` / `succ k` / hypothesis.

== eq_J

```disp
eq_J : (rank : Ord) →
       (A_rank : Ord) →
       (A : Type A_rank) →
       (x : A) →
       (motive : (y : A) → Eq A x y → Type rank) →
       (base : motive x refl) →
       (y : A) →
       (p : Eq A x y) →
       motive y p
```

Two ranks: `A_rank` is the universe of the equated type; `rank`
is the universe of motive's codomain. They are independent.
`Eq A x y` itself lives in `Type A_rank` (Eq inherits its
universe from `A`). The motive's parameter type
`(y : A) → Eq A x y → Type rank` lives in
`Type (max A_rank (succ_ord rank))`.

#note[
  *Equality at different type levels.* Disp's narrow Eq
  (`refl`-only, hash-cons identity) supports equalities at any
  universe rank uniformly:

  - `Eq Nat 0 0` (A_rank = 0): finite arithmetic.
  - `Eq (Type 0) Nat Bool` (A_rank = 1): equality between types.
    Inhabited iff the trees are identical --- so structurally
    distinct types are propositionally distinct.
  - `Eq (Type 1) (Type 0) (Type 0)` (A_rank = 2): equality of
    universes.

  This is forward-compatible with HoTT: a future extension that
  inhabits `Eq (Type k) X Y` with equivalences (univalence) does
  not change `eq_J`'s signature, only its eliminator behavior at
  type-valued targets. Until then, type-level equality is
  structural.
]

Path induction. On `p = refl`, `y = x`, returns `base`. On
hypothesis `p`, mints `cert_make_stuck (motive y p) p`.

== ord_rec

```disp
ord_rec : (rank : Ord) →
          (motive : Ord → Type rank) →
          (zero_case : motive 0_ord) →
          (omega_plus_case : (α : Ord) → motive α →
                            (β : Ord) → motive β →
                            motive (omega_plus α β)) →
          (target : Ord) →
          motive target
```

Two cases matching CNF's constructors. The
`omega_plus_case` receives both `α` and `β` plus their inductive
hypotheses. On hypothesis target, mints
`cert_make_stuck (motive target) target`.

#note[
  Programs that pattern-match on specific ordinal shapes (e.g.,
  detect "is this $omega$?") write them in terms of `ord_rec` plus
  comparisons. The kernel does not provide a per-shape predicate
  for each ordinal; users derive one if needed.
]

== wait_rec

`wait_rec` is the registry-fold reflection eliminator: case-split
on a value's type-former signature.

```disp
wait_rec : (motive : Type 0 → Type rank) →
           (on_pi : (dom : Type 0) →
                    (codFn : dom → Type 0) →
                    (cod_at_hyp : Type 0) →
                    motive (Pi dom codFn)) →
           (on_nat : motive Nat) →
           (on_bool : motive Bool) →
           (on_eq : (A : Type 0) → (x : A) → (y : A) →
                    motive (Eq A x y)) →
           (on_ord : motive Ord) →
           (on_core_univ : (k : Ord) → motive (wait ks.core_type k)) →
           (on_guarded_univ : (k : Ord) → motive (wait ks.guarded_type k)) →
           (on_guard : (core : Type 0) → motive (guard core)) →
           (on_default : (T : Type 0) → motive T) →
           (target : Type 0) →
           motive target
```

The `on_pi` branch exposes both `codFn` (for direct application
to user-supplied values) and `cod_at_canonical_hyp` (for
universe-rank computation that needs to evaluate the codomain at
a hypothesis). The kernel handler computes
`cod_at_canonical_hyp = codFn (cert_make_hyp dom meta)` once per
invocation, regardless of whether the user touches it.

On hypothesis target, mints `cert_make_stuck (motive target) target`.

User-facing reflection helpers (`pi_rank`, `pi_dom`, `pi_cod_fn`,
`is_pi`, `is_universe`, `is_eq`, `hasguard`, `unguard_or_self`,
`unguard_checked`, `universe_rank`) are tree programs defined on
top of `wait_rec`. They live in `kernel.disp` for convenience but
are not kernel record entries.

= Implementation invariants

These are invariants the implementation must respect. Violating
any of them is a soundness-class bug.

== Raw vs `ks.field` for signatures

Two different signature shapes exist in the kernel; picking the
wrong one for a dispatch case silently mismatches every value of
that family.

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Constructor*], [*Signature stored in value*], [*Use which inside handlers*],
    [`Hyp ty id` / `StuckElim r tg`],   [`raw.hyp_reduce` (raw)],   [`raw.hyp_reduce`],
    [`Pi A B` / `core_Pi A B`],         [`kernel_ref.pi` (proxy)],  [`ks.pi`],
    [`Nat`, `Bool`, `Eq`, `Ord`, `Type k`], [`kernel_ref.X` (proxy)], [`ks.X`],
    [Eliminator wait-values],           [`kernel_ref.X_rec` (proxy)], [`ks.X_rec`],
    [`elim_fail` sentinel],             [`kernel_ref.elim_fail`],   [`ks.elim_fail`],
  ),
)

Hypotheses must store the `raw` (actual recq-resolved) handler
identity because `q_hyp_reduce_fn` IS the runtime that fires
when neutrals are applied --- there is no proxy indirection at
that layer. Type formers and eliminators all use the proxy form;
the public constructors uniformly write `wait kernel_ref.X meta`,
so inside handlers the matching projection is `ks.X` (which has
the same hash-cons identity).

#note[
  *Implementer warning.* This table must be reproduced as a
  comment block at the top of `q_checked_apply_fn` in code.
  Writing `has_sig ks.hyp_reduce f` instead of
  `has_sig raw.hyp_reduce f` silently breaks neutral
  recognition: every neutral falls through to the parametric
  default and reflective attacks are no longer caught at the
  dispatch level. Failure mode is silent unsoundness.
]

== Handler return convention

The dispatcher's per-branch wrapping convention (which is shown
explicitly in the `select_chain` body in §5.1) must match the
handler's actual return shape. Mismatches double-wrap or
under-wrap, breaking `must_ok_*` chains downstream.

Every entry in the dispatch table must be accompanied by a
comment listing the handler's return convention (bare or
CheckedResult) and a brief note on what the handler validates.

== `recq` laziness for handler interdependence

Handlers reference each other via `ks.X` projections, which
resolve to `wait self X_query` --- inert until applied. This
means cyclic references between handlers (e.g., `q_pi_fn` calls
`ks.checked_apply`; `q_checked_apply_fn` references `ks.pi` for
signature matching) are resolved on demand, not at recq-build
time.

Implementer obligations:
- Never force `ks.X` evaluation at top level (e.g., don't write
  `let cached_pi = ks.pi` in module scope --- it's already lazy
  and forcing it serves no purpose).
- Field references inside `fix` bodies are also lazy. Mutual
  recursion between handlers is resolved via the recq projection
  at apply time.

== Public/private boundary

`kernel.disp` files declare exports via `:=` and private
bindings via `let`. The following must be private (declared with
`let`):

- `core_Nat`, `core_Bool`, `core_Eq`, `core_Pi`, `core_Ord`,
  `core_Type` --- the unguarded type checkers.
- `cert_make_hyp`, `cert_make_stuck`, `cert_make_neutral` ---
  the neutral-minting helpers.
- `q_*_fn` handler bodies (the kernel record's checker
  implementations).

The following must be public (`:=`):

- `Nat`, `Bool`, `Eq`, `Pi`, `Arrow`, `Ord`, `OrdLt`, `Type`,
  `omega` and related ordinal helpers --- guarded type
  constructors.
- `bool_rec`, `nat_rec`, `eq_J`, `ord_rec`, `wait_rec` --- user
  eliminator wrappers.
- `Hyp`, `StuckElim` --- public neutral constructors (callable
  but rejected at the public boundary's entry scan, per §2.3).
- Reflection helpers (`is_pi`, `pi_dom`, etc.).
- `checked_apply` --- public re-export of the dispatcher.

== `match` and closed-arm desugaring

Tree calculus is strict: thunks `({_} -> body)` capturing outer
free vars do NOT delay evaluation. The `match` keyword in source
code desugars to closed-arm select-then-apply with automatic
free-variable capture. Implementations must use `match` (or the
hand-written closed-arm pattern) for any conditional whose
branches reference outer scope variables.

The `must_ok_*` helpers internally use `match`, so CPS chains
that look like `must_ok_any r ({result} -> ...)` are
short-circuiting *only* if the continuation `{result} -> ...`
has a real parameter (not `_`) that appears in the body.
Continuations with `_` parameters and outer free vars do NOT
short-circuit.

== Hash-cons identity is load-bearing

Type equality is `tree_eq T1 T2` via the runtime's hash-cons
identity check (O(1)). For this to give meaningful answers:
- Same type must compile to the same tree.
- Bracket abstraction must be deterministic.
- Type-former metadata must use canonical pair-encodings.

If two structurally-equal types compile to different trees,
`Eq Type X Y` would be `FF` even when `X` and `Y` are "the same
type." This is the structural side of the soundness fix:
hash-cons identity defines propositional type equality.

= Worked examples <worked-examples>

== `(Pi Nat ({_} -> Nat)) ({x} -> x) = TT`

Trace through the public test of the polymorphic identity at type
`Nat → Nat`.

+ Source `Pi Nat ({_} -> Nat)` compiles to
  `guard (wait kernel_ref.pi (make_pi_meta core_Nat ({_} -> core_Nat)))`.
+ Source `({x} -> x)` compiles to `I_canonical`.
+ Test framework evaluates `(Pi Nat (...)) ({x} -> x)`. Under raw
  apply, this is direct application of the guard wait-value to
  the identity.
+ Raw apply fires `q_guard_fn` with `(core, v)` where
  `core = wait kernel_ref.pi ...` and `v = I_canonical`.
+ `q_guard_fn` runs `scan_no_neutral I_canonical`. I_canonical is
  `fork(fork(LEAF, LEAF), LEAF)` --- no neutrals. Passes.
+ `q_guard_fn` calls `ks.checked_apply core v`, dispatching:
  - `pair_fst core` = `pair_fst (wait kernel_ref.pi meta)` = the
    Pi signature. Matches `has_sig ks.pi`.
  - Routes to `Ok (core v)` --- raw apply on `core v`.
+ `core v` reduces to `q_pi_fn (kernel.pi_query) (meta, v)`.
+ Handler body: `q_pi_fn` recognizes `q_is_neutral raw v = FF`
  (v is concrete), so runs the check_fn branch.
+ Check_fn: extracts `domain = core_Nat`, `codFn = ({_} -> core_Nat)`,
  mints `hyp = cert_make_hyp core_Nat meta`.
+ `result_r = ks.checked_apply v hyp` = `ks.checked_apply I_canonical hyp`.
  Dispatcher: `I_canonical`'s signature is just `fork(fork(LEAF,
  LEAF), LEAF)` --- no kernel signature matches. Routes to walker
  default. Walker hits I-shortcut: `Ok hyp`.
+ `expected_r = ks.checked_apply codFn hyp` = `ks.checked_apply ({_} -> core_Nat) hyp`.
  Dispatcher: codFn is a K-form returning core_Nat. Walker reduces
  to `core_Nat`. Returns `Ok core_Nat`.
+ Both `Ok`-extracted: `result = hyp` (a neutral), `expected = core_Nat`.
+ `q_is_neutral raw result = TT`, so compare `expected` with
  `neutral_type result` via `tree_eq`. `neutral_type hyp = core_Nat`
  (the stored type at mint time). `tree_eq core_Nat core_Nat = TT`.
  Returns `Ok TT`.
+ Back in `q_guard_fn`: `is_ok r = TT`, `ok_value r = TT`. Returns
  bare `TT`.
+ Test framework sees `TT`, asserts equality, passes.

#note[
  *What's load-bearing in this trace.* The I-shortcut in the
  walker is essential --- without it, applying `I` to a hypothesis
  would hit the triage rule's neutral check and fail. The
  `tree_eq core_Nat core_Nat` step relies on the kernel-built
  `core_Nat` being hash-cons-equal to itself (deterministic
  compilation + canonical metadata). The `q_is_neutral raw`
  check uses `raw.hyp_reduce` (the actual handler tree), not
  `ks.hyp_reduce` (the proxy) --- mismatching here would silently
  treat all neutrals as non-neutrals.
]

== Rejecting forged neutral: `(Type 0) (Hyp Nat 0) = FF`

A user attempting to forge a Nat-typed neutral.

+ Source `Hyp Nat 0` compiles to
  `wait kernel.hyp_reduce (make_neutral_meta Nat 0)`.
+ Source `Type 0` compiles to `guard (wait kernel_ref.guarded_type 0_ord)`.
+ Test evaluates `(Type 0) (Hyp Nat 0)`. Raw apply fires
  `q_guard_fn` with `(core, v)` where `v = Hyp Nat 0`.
+ `q_guard_fn` runs `scan_no_neutral v`. `v` IS a neutral
  (`pair_fst v = kernel.hyp_reduce`). `scan_no_neutral` returns
  `FF`. `q_guard_fn` returns bare `FF`.
+ Test sees `FF`, fails the `= TT` assertion as expected.

#note[
  The entry scan at `q_guard_fn` is what catches this attack.
  Without the scan, the dispatcher would route `core v` (where
  `v` is a forged neutral) through the universe checker, which
  would run the H-rule and check stored Nat against expected
  Type --- but stored is "Nat" while expected is "Type 0", so it
  would actually reject anyway. The double defense (entry scan
  AND H-rule) gives both a fast-fail at the boundary and a deep
  check inside.
]

== Rejecting reflective family: `(Pi Nat BadFam) ({n} -> 0) = FF`

The `is_neutral` reflection attack.

+ `BadFam = {n} -> select Nat (Eq Nat 0 1) (is_neutral n)`.
+ `Pi Nat BadFam` is constructed normally.
+ Test evaluates the Pi-check on the function `({n} -> 0)`.
+ `q_guard_fn` runs entry scan; passes.
+ Routes through dispatcher; `q_pi_fn` runs.
+ Mints `hyp_n = cert_make_hyp core_Nat meta`.
+ `result_r = ks.checked_apply ({n} -> 0) hyp_n`. Walker mode:
  the function reduces, hyp_n is a value of Nat type. Walker
  reduces `({n} -> 0) hyp_n` to `0` (K-rule). Returns `Ok 0`.
+ `expected_r = ks.checked_apply BadFam hyp_n`. Walker mode:
  BadFam reduces to `select Nat (Eq Nat 0 1) (is_neutral hyp_n)`.
  Walker dispatches step-by-step. Eventually hits
  `is_neutral hyp_n`, which is `(triage on hyp_n's tree
  structure)`. Walker's triage-on-neutral rule fires:
  `Fail`. `expected_r = Fail`.
+ `q_pi_fn` sees `Fail` from `expected_r`, returns `FF`.
+ `q_guard_fn` returns bare `FF`. Test fails.

#note[
  This is the reflective attack closed by the walker. Under raw
  apply (the previous sound-but-incorrect kernel), `is_neutral
  hyp_n` would return `TT` and `BadFam hyp_n` would reduce to
  `Eq Nat 0 1`, matching the stored Pi codomain --- the Pi check
  would pass! Then `bad 0` would type-check as `Eq Nat 0 1`,
  which is unsound. The walker's triage-on-neutral rule
  intercepts the reflective `is_neutral` call and refuses the
  observation, propagating Fail upward.
]

= Open problems

These are extensions and unresolved questions for future work.
None blocks the closed-kernel implementation specified above.

== HoTT extensions

The current kernel uses MLTT-style narrow `Eq`: only `refl`
inhabits `Eq A x y`, and refl-evidence requires hash-cons
identity of `x` and `y`. For `Eq (Type k) X Y`, this is
*structural type equality* --- two distinct trees are
propositionally distinct even when mathematically equivalent.

A future HoTT evolution would:
- Inhabit `Eq (Type k) X Y` with *equivalences* (univalence).
- Add path constructors to inductive types (HITs), allowing
  e.g. ordinal bisimilarity to be propositionally provable.
- Generalize `eq_J` to handle path-constructor cases.

The current `eq_J` signature (with explicit `A_rank`) is already
forward-compatible: HoTT extensions would change the
*eliminator's behavior* at type-valued targets, not the
signature.

The kernel infrastructure that supports HoTT later: nothing in
the current design precludes adding path constructors to
inductive types. The wait-encoded checker for `Eq` would gain
additional cases for non-refl inhabitants, dispatched on the
proof's tree shape. Migration would be additive.

== Universe inference at the elaborator

Current source surface requires explicit universe ranks:
`Type 0`, `Type omega`, etc. Bare `Type` is a parser error.

A future elaborator could synthesize ranks at use sites:
- Source `Type` becomes a fresh universe variable.
- Constraints accumulate as the term is checked
  (`Type ≥ Type rank_of_X` for each use).
- The elaborator solves for the minimum consistent rank.

This is Coq-style implicit universe polymorphism. It would
remove the source-level rank annotations users currently must
write but adds elaborator complexity. Out of scope for the
initial kernel; orthogonal to soundness.

== Higher universes (beyond $epsilon.alt_0$)

Disp's CNF Ord can name ordinals up to $epsilon.alt_0$ via
`0_ord`, `omega_plus`, and the algebraic operations. Higher
ordinals (Bachmann--Howard, $Gamma_0$, etc.) require richer
constructors --- e.g., `phi : Ord -> Ord -> Ord` (Veblen
function) or ordinal-indexed limits.

Disp's universe hierarchy is bounded by Ord's expressiveness.
Bumping past $epsilon.alt_0$ would require:
+ Adding new Ord constructors.
+ Updating `q_ord_fn` to validate the new shapes.
+ Updating `ord_lt`, `ord_le`, `ord_max` to compare.
+ Updating `ord_rec` to dispatch on the new constructors.

Each is mechanical. Defer until a use case demands.

== Bound-consulting identities for non-canonical ordinals

The kernel's `ord_lt`/`ord_le` handlers consult `OrdLt k` bounds
on hypothesis-typed Ord values to derive concrete answers, but
only for canonical limit ordinals where the structural identity
is known ($omega$, $omega + 1$, $omega dot n$, $omega^2$, etc.).
Comparisons against non-canonical bounds (e.g., user-defined
ordinal expressions) fall through to stuck.

Future work:
+ Catalog the canonical limit ordinals up to $epsilon.alt_0$ and
  enumerate their bound-consulting identities in a single
  kernel-internal table.
+ Allow user-defined identities (with kernel-verified
  monotonicity proofs) so non-canonical bounds can also produce
  closed answers.

This extension is purely additive --- existing programs are
unaffected; new programs gain power.

== Data-driven level computation (TTBFL `lub`)

If a future use case demands `lub : List (Σℓ. Type ℓ) → Ord` ---
a runtime function that computes a level from a data structure of
types-at-various-levels --- the kernel must adopt TTBFL's full
bounded-level machinery (Trans and Cumul rules,
non--syntax-directed universe checking). The current `OrdLt k`
predicate is sufficient for *static* bounded polymorphism but
does not provide *dynamic* level computation from data.

This is a substantial extension. Avoid until the use case
forces it.

== Native walker for performance

The in-language dispatcher walks each apply step under
parametric mode at ~70× the cost of raw apply. A native
implementation in `tree.ts` mirroring the host `tree_eq`
fast-path would cut this to ~1-2×.

Native walker is a *performance carve-out* (per @soundness-carve-out),
not a soundness one: it must produce observably identical
results to the in-language walker. Add when profiling demands.

== Kernel extensibility

The current kernel has a *closed* registry of trusted
primitives. User code cannot register new type-formers,
eliminators, or comparison primitives.

Lifting this to an open registry --- where user-defined types
can carry checkers and eliminators that the kernel routes to
correctly --- is a separate refactor. See
`KERNEL_EXTENSIBILITY_PLAN.md`. Soundness for an open registry
requires carefully bounding what user-registered handlers can
do (specifically: when can they mint neutrals?).

== Polymorphic-rank function tests

Polymorphic library code has stuck `pi_rank` and cannot be
publicly tested at the polymorphic level (per @poly-universe).
Users instantiate at concrete ranks for tests. This is
consistent with @soundness-not-completeness but means the test
surface is "closed instantiations" rather than "polymorphic
schemas."

If usability demands polymorphic-level testing, options:
+ Publish a polymorphic-rank test convention (e.g.,
  "polymorphic library functions test against
  $omega$, $omega + 1$, $omega^2$ in turn").
+ Add a `test_polymorphic` predicate that quantifies over
  representative ranks and verifies stuck propagation. This
  would be implemented as user code, not a kernel primitive.
+ Adopt TTBFL bounded levels (above) so polymorphic-rank types
  have closed bounds.

For now: closed-instantiation tests are sufficient.

== Match desugaring efficiency

The `match` desugarer captures union-of-arms free variables and
re-captures at every nesting level. Deeply nested handlers
(e.g., the eliminator handlers' must_ok_* CPS chains) compile
to N closures of size O(N × |fvs|), potentially producing
surprisingly large compiled trees.

A future improvement: bracket-abstraction-aware desugaring that
doesn't re-capture already-bound names. This is a compile-time
optimization with no soundness implications.
