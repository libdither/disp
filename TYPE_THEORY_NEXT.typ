#set document(title: "Disp Type Theory — Unified Spec (TYPE_THEORY_NEXT)")
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

#let openq(body) = block(
  breakable: false,
  above: 0.6em,
  below: 0.6em,
  stroke: (left: 2pt + rgb("#cc6600")),
  inset: (left: 1em, y: 0.3em),
  text(weight: "bold")[Open question: ] + body,
)

#align(center, text(22pt, weight: "bold")[Disp Type Theory])
#v(0.3em)
#align(center, text(13pt)[Unified Specification (NEXT)])
#v(0.5em)
#align(center)[
  Type theory of disp via manifest contracts over a tree-calculus substrate,
  with categorical foundations and cubical extensions integrated.
]
#v(1em)

#note[
  *Status (2026-05-20).* Active spec. Replaces the prior `TYPE_THEORY.typ`
  (seven-primitive kernel design) and consolidates
  `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ` and `CUBICAL_PROPOSAL.typ`
  into a single document.

  Major shifts from predecessors:
  - The kernel surface drops from 7 to 6 primitives (removed `guard`/`unguard`
    per Interp B; added `checked`).
  - Type-checking is framed as manifest contracts over the `CheckerResult`
    monad. The elaborator is purely a wrap-only pass; no bidirectional
    inference.
  - Library type-formers are explicit `TypeFormer` records with named fields,
    not positional metadata tuples.
  - Cubical operations (`transp`, `hcomp`, `comp`, `Glue`) live in the
    `Functor` field of each type-former's `TypeFormer` record.
  - `strip` is a tree-level function gated by `validate`'s certificate
    for proof-carrying-code-style erasure.

  Open items are flagged inline as `Open question:` notes. The spec is
  designed to be iterated on section by section.
]

= Overview <sec:overview>

== The framing in one paragraph

Disp is a dependently-typed language whose type system is implemented as
*manifest contracts* over a tree-calculus substrate. Every typed function
value carries a runtime input-checker (a "contract"); every type is a
predicate that values are checked against. The elaborator's only job is
to *wrap function values with their declared types* — no bidirectional
inference, no infer/check ping-pong. Type-checking happens at one
top-level call per declaration; the reduction triggers all internal
contract checks, and failures propagate uniformly via the `CheckerResult`
monad. After elaboration succeeds, a *strip pass* elides validated
contracts to give a runtime tree with no per-call checking overhead.

The kernel implements a closed indexed algebraic effect with six
operations. The dispatcher routes by structural signature on
hash-consed trees, so dispatch is O(1) via tree-id comparison. Library
types are expressed as `TypeFormer` records bundling the standard
categorical structures (subobject classifier, eval morphism, ∞-functor
action). Cubical operations sit naturally in the functor field. The
metacircular discipline holds: the type system is defined in disp
source; the host (TypeScript runtime in `src/tree.ts`) only optimizes.

== Reading guide

This document is organized so each section is independently readable
and revisable:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Section*], [*Covers*],
    [§2 Substrate], [Tree calculus, apply, hash-cons identity],
    [§3 The Result monad], [`Result E A`, `CheckerError` variants, `CheckerResult`, Kleisli composition, bind variants],
    [§4 The parametric walker and `Tree_p`], [Walker as Kleisli-lifted binary apply, `Tree_p` as greatest fixed point, soundness discipline],
    [§5 The closed indexed effect], [Algebraic-effects framing for the kernel],
    [§6 The six primitives], [Operational semantics of each kernel handler],
    [§7 Boundary operations], [`param_lift`, `param_apply`, `typecheck`],
    [§8 Checked values], [`checked`, `typed_lambda`, `validate`],
    [§9 Wrap-only elaboration], [What the elaborator does (and doesn't)],
    [§10 Strip and erasure], [`strip` as a tree function; PCC story],
    [§11 TypeFormer records], [Categorical foundations; bootstrap],
    [§12 Library types], [Each library type under the framework],
    [§13 Cubical extensions], [`I`, `Path`, `comp`, `Glue`, `ua`],
    [§14 Soundness theorem], [Formal statement and proof sketch],
    [§15 Future effects], [Multi-effect disp via Koka-style rows],
    [§16 Disp-specific], [What disp contributes beyond standard machinery],
    [§17 Related work], [Literature context],
    [§18 References], [Citations],
  ),
  caption: [Section map.],
)

Read §1–§7 in order for the framework. §8–§12 build the type system on
top. §13 covers cubical. §14 is the formal payoff. §15–§17 are
forward-looking and contextual.

== Prerequisites

Familiarity with dependent type theory and basic category theory helps
but is not required. The algebraic-effects framing in §5 is the most
mathematically dense; readers can skim it and rely on the operational
semantics in §6 if preferred.

= Substrate: tree calculus <sec:substrate>

== Trees and apply

A *tree* is one of:
- `LEAF` (the unique atom).
- `stem(c)` for a tree `c` (one-argument constructor).
- `fork(l, r)` for trees `l`, `r` (two-argument constructor).

In disp source `t` constructs trees: `t` is `LEAF`, `t x` is `stem(x)`,
and `t x y` is `fork(x, y)`.

The reduction operation `apply : Tree × Tree → Tree` is defined by five
rules:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Rule*], [*Reduction*],
    [Leaf], [`apply(LEAF, x) = stem(x)`],
    [Stem], [`apply(stem(c), x) = fork(c, x)`],
    [K], [`apply(fork(LEAF, b), x) = b`],
    [S], [`apply(fork(stem(c), b), x) = apply(apply(c, x), apply(b, x))`],
    [Triage], [`apply(fork(fork(tc, td), b), x) = ` (depends on `x`'s shape)],
  ),
  caption: [The five reduction rules.],
)

The triage rule splits on `x`'s shape:
- `x = LEAF`: returns `tc`.
- `x = stem(c)`: returns `apply(td, c)`.
- `x = fork(l, r)`: returns `apply(apply(b, l), r)`.

These five rules implement combinatory completeness: tree calculus is
Turing-complete and equi-expressive with the untyped lambda calculus.

== Hash-cons identity

Trees are *hash-consed*: any two structurally-equal trees share the same
host-side identity. The host (`src/tree.ts`) maintains a table mapping
`(constructor, child ids)` to existing tree objects, so `fork(a, b)`
returns an existing node if one exists.

The crucial consequence: `tree_eq a b` reduces to a pointer comparison
on `a.id` and `b.id`. Equality is O(1). Every operation that compares
trees — type-form recognition, H-rule application, walker neutral-checks —
exploits this.

#note[
  *Why this substrate?* Compared to lambda calculus: no bound variables
  (so no α-equivalence, no capture-avoiding substitution), hash-cons
  gives free O(1) equality, reduction is local pattern matching. Compared
  to Turing machines: structured composition, trivial self-representation.
  These properties are what enable disp's metacircular discipline.

  `apply` is evaluation, not arrow composition, so the substrate carries
  no inherited categorical structure (no canonical identity-with-respect-to-
  composition, no associative ∘). Each `TypeFormer` (§sec:typeformer)
  constructs its own functorial/morphism structure on its typed subset.
]

== Native runtime

The host runtime in `src/tree.ts` implements hash-consing and `apply`.
Performance-critical operations (`tree_eq`, the dispatcher, the
parametric walker) have native fast-paths that produce bit-identical
results to the in-language reference implementations. The in-language
reference is the spec; the host is the optimization.

= The `CheckerResult` monad <sec:result-monad>

== The general `Result` shape

`Result` is the standard error-or-value sum, parameterized over the
error type:

```
Result E A := Ok A | Err E
```

This is the same shape as Rust's `Result<T, E>`, OCaml's `('a, 'e) result`,
Haskell's `Either E A`, and Lean's `Except E A`. For each fixed `E`,
`Result E` is a monad: `η = Ok`, `μ` collapses nested `Ok (Ok x) → Ok x`
and propagates `Err e` outward.

== `CheckerError` — the kernel's failure vocabulary

The kernel's failures are not undifferentiated — they carry distinct
meanings that downstream code needs to react to differently. The
failure type is a tagged enum:

```disp
// All errors carry a source span for diagnostics.
CheckerError := tagged_enum {
  Parametricity : { kind : ParamKind, where : Tree_p, span : Span }
  Escape        : { hyp : Tree_p, body_result : Tree_p, span : Span }
  NotApplicable : { type : Tree_p, span : Span }
  TypeMismatch  : { expected : Type, actual : Tree_p, span : Span }
  Malformed     : { handler : Symbol, meta : Tree_p, span : Span }
}

ParamKind := StemForge | TriageReflect
```

Each variant maps to a distinct kernel failure path:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Variant*], [*Raised by*],
    [`Parametricity StemForge`],
      [walker, when applying a stem would produce a fork whose
       `pair_fst` is a pinned kernel signature (would forge a neutral)],
    [`Parametricity TriageReflect`],
      [walker, when triage would split on a kernel-minted neutral
       (would reflect on a hypothesis)],
    [`Escape`],
      [`bind_hyp`, when the body's result exposes the minted
       hypothesis via a non-neutral path],
    [`NotApplicable`],
      [`checked`, when the stored type is not function-shaped;
       `hyp_reduce`, when the stored type lacks `predicate_frame`
       signature],
    [`TypeMismatch`],
      [contract boundaries (`checked` argument check, any typed-
       function application), when a recognizer returns `Ok FF` on
       a value where TT was contractually required],
    [`Malformed`],
      [any handler, when its meta doesn't fit the expected shape
       (currently silent — see §5 future-work)],
  ),
  caption: [`CheckerError` variants and their kernel-handler origins.],
)

#note[
  *Verdict vs error.* A recognizer's `Ok FF` is *data*, not an
  error — it means "this value is not an inhabitant of the queried
  type," which is a legitimate answer to a query. Errors flow
  through `Err` only when something is *broken*: parametricity
  violated, hypothesis escaped, contract-mandated TT received FF,
  meta malformed. Query-style callers (`typecheck`, `validate`) see
  `Ok TT` / `Ok FF` and pattern-match. Contract-style callers
  (`checked` application) raise `TypeMismatch` because their callers
  promised the value would fit.
]

== `CheckerResult` and the monad structure

```
CheckerResult A := Result CheckerError A
```

The unit `η : A → CheckerResult A` is `Ok`. The multiplication
`μ : CheckerResult (CheckerResult A) → CheckerResult A` collapses
nested wrappings (with the inner `Err` taking precedence over an
outer `Ok` shell):

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Input*], [*Output*],
    [`Ok (Ok v)`], [`Ok v`],
    [`Ok (Err e)`], [`Err e`],
    [`Err e`], [`Err e`],
  ),
  caption: [Multiplication μ for `CheckerResult`.],
)

Monad laws (unit-left, unit-right, associativity) check trivially.

== Tree encoding (bootstrap layer)

At the bootstrap layer, `Ok` and `Err` are concrete tree shapes
chosen for O(1) discrimination:

```disp
Ok  := {v} -> t t v            // Ok v  = fork(LEAF, v)
Err := {e} -> t (t t) e         // Err e = fork(stem(LEAF), e)
```

Because `pair_fst (Ok v) = LEAF` and `pair_fst (Err e) = stem(LEAF)`,
`is_ok r = tree_eq (pair_fst r) LEAF` remains a single hash-cons
comparison. The error payload `e` is itself a tree encoding a
`CheckerError` variant — at bootstrap this is hand-encoded (a tag
tree plus payload subtrees); once §11's library types are in scope,
the same encoding is type-ascribed to `CheckerError`.

== Kleisli composition

The Kleisli category `Kl(CheckerResult)` has trees as objects;
morphisms `A → B` are functions `A → CheckerResult B`. Composition is

$ g compose_K f = mu compose T(g) compose f $

In disp source this is `{x} -> bind (f x) g`. The parametric walker
of §4 is a Kleisli arrow `Tree × Tree → CheckerResult(Tree)` — the
Kleisli lift of binary `apply`. Restricting it to `Tree_p × Tree_p
→ CheckerResult(Tree_p)` (the subset of trees on which it never
produces an `Err Parametricity`, defined in §4) gives the carrier
on which the operation is closed under `Ok`.

== General `Result E` operations

The standard `Result` API — these work for any error type `E` and
know nothing about `CheckerError`. Five primitives cover the entire
space of error-handling patterns:

```disp
// Monadic bind. Propagates Ok payload into continuation; passes Err
// through unchanged. Equivalent to >>= / flatMap.
bind := {r, k} ->
  match (is_ok r) {
    TT => k (ok_value r)
    FF => r
  }

// Functor map. Transforms the Ok payload; leaves Err alone.
map := {r, f} ->
  match (is_ok r) {
    TT => Ok (f (ok_value r))
    FF => r
  }

// Error transform. Leaves Ok alone; transforms the Err payload.
// Useful for re-tagging errors as they cross layer boundaries.
map_err := {r, f} ->
  match (is_ok r) {
    TT => r
    FF => Err (f (err_value r))
  }

// Error handler. Ok passes through; Err is handed to the handler,
// which can itself produce Ok (recovery) or another Err (rethrow).
// Equivalent to orElse / try-catch / Lean's Except.tryCatch.
catch := {r, handler} ->
  match (is_ok r) {
    TT => r
    FF => handler (err_value r)
  }

// Assertion sugar. Lifts a bool into Result Unit, supplying the
// error for the false case. Useful for inline pre-conditions inside
// a bind chain.
guard := {cond, err} ->
  match cond {
    TT => Ok t                    // t = unit at this layer
    FF => Err err
  }
```

`map` and `map_err` together form the bifunctor structure; `catch`
is monadic recovery on the error side; `guard` is the standard
boolean-to-Result lift. These five are the entire vocabulary —
every higher-level error-handling pattern is a composition.

== Using `CheckerResult` in practice

There are no `CheckerResult`-specific helper combinators. The
general `Result E` operations above are the entire vocabulary; the
kernel just composes them with appropriate `CheckerError` variants.
The two call-site patterns worth understanding:

*Query patterns.* When the caller is asking the recognizer for a
verdict — "is `v` an inhabitant of `T`?" — the answer is data, not
an error. `Ok TT` and `Ok FF` are both successful runs; `Err _`
signals something is broken (parametricity, escape, malformed
input). Callers pattern-match on the verdict:

```disp
bind (typecheck T v) ({verdict} ->
  match verdict {
    TT => /* it's an inhabitant; proceed */
    FF => /* it's not; do whatever the query API demands */
  })
```

*Contract patterns.* When the caller has a contractual guarantee
that the value fits — applying a `f : Pi A B` to an arg, where the
type system says the arg must be in `A` — receiving `Ok FF` from
the recognizer means the program is broken, not that "we got a
no." Here the receiving handler raises `TypeMismatch`:

```disp
bind (param_apply A arg) ({verdict} ->
  match verdict {
    TT => /* arg fits A; proceed */
    FF => Err (TypeMismatch { expected = A, actual = arg, span })
  })
```

This is the soundness win versus the old undifferentiated
`must_ok_or_ff`: that helper blindly folded *any* failure into
`FF`, which would silently mask a parametricity violation inside a
recognizer body as "this isn't an inhabitant." In the new design,
every recognizer is required to *be* a recognizer (returning a
verdict, never absorbing soundness errors), and the lifting of
`FF → Err` happens only where it is contractually justified
(`checked` and similar typed boundaries).

#openq[
  The eliminator handler currently uses an `Err`-fallback pattern
  (`catch r (λ_. Ok wait_form)`) to stage partial application —
  treating "not enough args yet" as an error to recover from. This
  is misusing `Err` as control flow: partial application isn't a
  failure, it's a different success state. Refactor candidate
  (codebase work): redesign the eliminator's partial-app staging so
  it returns `Ok (wait_for_more)` directly, with no `Err`
  round-trip. See §6.3 and §15.
]

== Why a monad

Failures propagate uniformly via `bind`. No manual short-circuiting
in handler code; Kleisli composition does it automatically. Tagged
errors plus `catch` mean each layer decides what to recover from and
what to re-raise, instead of pattern-matching on opaque `Fail`
trees. The general/specialized split keeps the kernel's failure
vocabulary self-documenting: `grep catch` finds every error
suppression; `grep 'Err ('` finds every error raise.

= The parametric walker and `Tree_p` <sec:tree-p>

== Motivating problem

Disp's type system relies on *hypotheses* — fresh tree values minted by
the kernel that represent "an unknown value of type `A`." Hypotheses
have a pinned signature (`pair_fst h = kernel.hyp_reduce` for a kernel-
minted neutral `h`). For the type system to be sound, user code must not
be able to introspect hypotheses (otherwise it could behave differently
on hypothesis inputs versus concrete inputs, breaking parametricity).

The fix: define the *parametric walker* — a Kleisli-lifted version
of `apply` that performs the same reduction but rejects two
introspection patterns. `Tree_p` is then the largest subset of
trees on which the walker, applied to pairs from `Tree_p × Tree_p`,
never trips a rejection.

== The walker as a Kleisli-lifted binary operation

The walker is the Kleisli lift of binary `apply`:

$ w : "Tree" times "Tree" -> "CheckerResult"("Tree") $

— a Kleisli arrow in `Kl(CheckerResult)` with the same arity as
the substrate operation it lifts. Its three clauses follow
`apply`'s rules, with two `Err Parametricity` cases and one
carve-out:

+ *Stem-rule rejection.* When `w(stem(a), x)` would reduce to
  `fork(a, x)`: if this fork's `pair_fst` would be a pinned kernel
  signature (specifically `hyp_reduce`'s signature), return
  `Err (Parametricity { kind = StemForge, where = fork(a, x), span })`.
  This prevents forgery of kernel-minted neutrals at user level.

+ *Triage-rule rejection.* When `w(f, x)` would fire the triage
  rule on `x` (because `f` is a fork-fork-fork shape), first check
  whether `x` is a kernel-minted neutral
  (`pair_fst(x) = kernel.hyp_reduce`). If so, return
  `Err (Parametricity { kind = TriageReflect, where = x, span })`.
  This prevents reflection on hypotheses via triage.

+ *I-shortcut (carve-out).* `w(I_canonical, x) = Ok x`
  unconditionally, even when `x` is a hypothesis. Required so the
  polymorphic identity function passes Pi-checks against hypothesis-
  typed arguments. It is the only soundness carve-out.

All other applications follow `apply`'s rules and return `Ok <result>`.

== `Tree_p` as a greatest fixed point

`Tree_p` is defined as the *largest* subset `S ⊆ Tree` such that
the walker restricted to `S × S` is closed under `Ok` — i.e., the
operation `S × S → CheckerResult(S)` never produces an
`Err Parametricity`:

$ "Tree"_p = "greatest" S subset.eq "Tree" "such that" forall f\,x in S, w(f, x) in {"Ok"(r) : r in S} $

(modulo divergence — non-terminating reductions don't violate
membership). The walker is the only source of `Err Parametricity`,
and parametricity is the only kind of error it produces; other
`CheckerError` variants are raised by kernel handlers further out.

#note[
  Tree_p is a property of the walker, not of `CheckerResult`. The
  monad supplies the failure container; the *content* of "what counts
  as parametric" lives in the walker's rejection clauses. The
  definition is *semantic* and undecidable in general — you cannot
  tell by inspection alone whether an arbitrary tree is in `Tree_p`.
  §4.5 gives a syntactic discipline that approximates membership
  conservatively.
]

== The walker restricted to `Tree_p`

Once `Tree_p` is in hand, the walker restricts to a closed binary
operation on it:

$ w_p : "Tree"_p times "Tree"_p -> "CheckerResult"("Tree"_p) $

That is the type the kernel actually relies on — every operation
handler in §5 consumes and produces values in `Tree_p`.

== Soundness rules for users

To keep user-written trees in `Tree_p`, follow five rules. These form
a *decidable static discipline* that conservatively approximates
membership in the greatest fixed point above:

+ *Don't triage on hypothesis-typed values.* If `x` might be a
  hypothesis (e.g., bound by `bind_hyp`), don't write
  `triage l s f x`. Use kernel-mediated checks (`is_neutral`,
  `tree_eq` against a known closed value, `has_sig` against a
  registered signature) instead.

+ *Don't construct forks with kernel signatures as `pair_fst`.* The
  stem rule rejects this. Use kernel constructors (`Hyp`, `StuckElim`,
  etc.) to mint neutrals.

+ *Define new type-formers via kernel constructors.* Use
  `predicate_frame_form` or `eliminator_frame_form`, not direct wait-
  form construction.

+ *Type-check at the boundary.* Use `typecheck T v`, not raw `T v`,
  to verify membership. The boundary version sanitizes input and
  routes through the dispatcher.

+ *Kernel handler bodies have privileged access.* Code that runs
  inside a kernel handler (recognizers, codomain functions) executes
  "raw" — outside the walker. Library authors writing these are part
  of the trusted base.

`Tree_p` is the largest carrier in `Kl(CheckerResult)` on which the
walker — the Kleisli lift of the substrate's apply operation —
restricts to a closed binary operation. Composition in
`Kl(CheckerResult)` is the standard `g ∘_K f = μ ∘ T(g) ∘ f` of
§3.5; the walker is the operation those Kleisli arrows compose with
when reducing `apply` chains.

= The closed indexed effect <sec:closed-effect>

== Setup

Disp's kernel is best described as a *closed indexed algebraic effect*
over the `CheckerResult` monad. This section makes the framing precise.
Readers preferring operational explanations can skip to §6.

== The signature

Let Σ be the signature of operation symbols

```
Σ = { hyp_reduce, predicate_frame, eliminator_frame,
      bind_hyp, param_apply, checked }
```

Each operation is a *curried function* taking its structured meta
record first, then an argument from `Tree_p`, and producing a result
in the `CheckerResult` monad. The meta record is what gets baked
into a wait-form (`wait kernel.op meta`, see §5.4); the second
application supplies the argument and triggers dispatch.

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Operation*], [*Type*],
    [`hyp_reduce`],
      [`{stored_type : Type, spine : List Tree_p} -> Tree_p -> CheckerResult(Tree_p)`],
    [`predicate_frame`],
      [`{recognizer : P -> A -> CheckerResult Bool, params : P, codomain_fn : CodFn} -> Tree_p -> CheckerResult(Bool)`],
    [`eliminator_frame`],
      [`{dispatcher : Motive -> Cases -> A -> R, motive : Motive, cases : Cases} -> Tree_p -> CheckerResult(Tree_p)`],
    [`bind_hyp`],
      [`{domain : Type, body : domain -> R} -> Tree_p -> CheckerResult(Tree_p)`],
    [`param_apply`],
      [`Tree_p -> Tree_p -> CheckerResult(Tree_p)` (no meta)],
    [`checked`],
      [`{T : Type, v : T} -> Tree_p -> CheckerResult(Tree_p)`],
  ),
  caption: [Per-operation types. All but `param_apply` take a meta
    record. Type variables: `A` = recognized/eliminated type, `P` =
    closed params, `R` = result type, `Motive`/`Cases` =
    dispatcher-specific. `CodFn = (P -> A -> Type) ∪ Sentinel` for
    non-applicable types.],
)

#note[
  *On the record types in the table.* The library types these records
  reference — `Type`, `Bool`, `List`, `Pi` (as `->`) — are defined
  later in the spec (`Type` in §11, `Bool`/`Nat`/`List` in §12). The
  kernel itself is bootstrapped from tree-calculus primitives, so at
  the bootstrap layer these records are physically encoded as nested
  pairs of trees: e.g. `bind_hyp`'s meta is `pair domain body`,
  `eliminator_frame`'s is `pair dispatcher (pair motive cases)`, etc.
  Multi-arg operations like `bind_hyp` and `eliminator_frame` collect
  their fields across multiple partial-app steps using an internal
  arity counter in the wait-form payload; the user sees a uniform
  one-arg-per-step dispatch shape while the handler assembles the
  full record before firing. Early versions of the kernel cannot
  reference the library record types because those types don't yet
  exist.

  Once §11's library types are in scope, the record forms above
  become real ascriptions and the kernel handlers are *typeable by
  the type checker they implement* — the closure of the metacircular
  discipline. See §11 (typed surface) and §15 (self-checking
  roadmap).
]

== Σ-algebras and the kernel

A *Σ-algebra* A over `T` assigns to each operation symbol `op ∈ Σ` a Kleisli
arrow `⟦op⟧^A` of the appropriate arity.

Disp ships exactly one Σ-algebra: the *kernel*.

```disp
kernel := rec {
  hyp_reduce       := q_hyp_reduce_fn;
  predicate_frame  := q_predicate_frame_fn;
  eliminator_frame := q_eliminator_frame_fn;
  bind_hyp         := q_bind_hyp_fn;
  param_apply      := q_param_apply_fn;
  checked          := q_checked_fn
}
```

Each `q_*_fn` is a Kleisli arrow implementing its operation's semantics
(full definitions in §6).

This is *closed*: there is no user-installable alternative Σ-algebra.
The kernel is the handler.

== Operation invocation via wait-forms

A wait-form `wait kernel.op meta` is a *delayed operation invocation*
— a tree whose `pair_fst` is the operation's signature (a constant
tree per operation) and whose `pair_snd` is the structured meta. When
`param_apply` encounters such a wait-form applied to an argument, it
routes via signature matching to the appropriate handler clause:

```
param_apply f x:
  if pair_fst(f) matches kernel.hyp_reduce signature  → invoke hyp_reduce^A
  if pair_fst(f) matches kernel.predicate_frame sig    → invoke predicate_frame^A
  ...
  else (no match)                                       → walker default
```

The dispatcher is the *interpretation function*: given a tree
`f` representing a potential operation invocation, decide which
operation (if any) is being invoked and apply the corresponding
Kleisli arrow.

The default for non-operation invocations is the parametric walker
(§4) — itself a Kleisli arrow that just applies tree-calculus rules
with parametricity rejections.

=== Why wait-forms, not plain partial application

Semantically, meta is just an additional argument to the handler —
`wait kernel.op meta arg` is `handler(meta, arg)` with extra
ceremony. The wait-form encoding buys three things that direct
partial application does not:

+ *O(1) signature dispatch.* `pair_fst(wait k m) = k` is a constant
  tree per operation, so the dispatcher matches by hash-cons equality
  on a single id. A directly partially-applied function would need
  some other recognition mechanism (deeper structural inspection or
  a tag argument) — both worse.

+ *Meta inspection without forcing the handler.* Library type-formers
  need this: `Bool = wait q_predicate_frame_fn bool_meta` exposes
  `bool_meta` (recognizer, classifier-into-Ω, codomain-fn) via
  `pair_snd Bool` without ever applying `Bool` to anything.

+ *Stability under bracket abstraction.* A plain partial application
  compiles, via bracket abstraction, into a tree where the S-combinator
  distributes meta into the handler body wherever it's used —
  scattering it past the dispatcher's reach. Wait-form keeps
  `(signature, meta)` as a single top-level fork after compilation,
  regardless of how the handler body uses meta internally.

The wait-form is essentially a *reified curried-application stack*
that the dispatcher pattern-matches on. The Lisp analogue is the
difference between `(lambda (arg) (op meta arg))` (meta closed over,
invisible) and `(quasiquote (op ,meta ,arg))` (meta in the spine,
visible). Wait-forms are the second flavor, native to TC.

== Derived terms

Library operations are *derived terms* built by composition of Σ
generators. Examples:

- `typecheck` is the Kleisli composition of `param_lift` and `param_apply T` (precomposed with the user-supplied type).
- `typed_lambda A B f` is a specific use of `checked` at function-type metadata.
- `validate` composes `typecheck` with certificate construction.
- Library types (`Pi`, `Sigma`, `Bool`, etc.) are derived terms built from `predicate_frame_form`, `eliminator_frame_form`, and `bind_hyp`.

The library is "freely generated" from Σ in the algebraic-theory sense,
modulo composition equations in `Kl(T)`.

== Caveats vs Plotkin-Pretnar algebraic effects

Disp's system is structurally similar to but technically narrower than
Plotkin-Pretnar algebraic effects:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Property*], [*Disp*], [*Plotkin-Pretnar*],
    [Handler set], [Closed (just the kernel)], [Open (user-defined)],
    [Dispatch], [Structural signature on wait-forms], [Lexical handler scope],
    [Algebraicity], [Operations not algebraic in general], [Operations algebraic (`bind (op c_i) k = op (bind c_i k)`)],
    [Multi-shot continuations], [No (tree calculus is pure)], [Yes],
    [Effect rows], [Implicit (one effect)], [Explicit row polymorphism],
  ),
  caption: [Disp vs Plotkin-Pretnar.],
)

Disp inherits the *vocabulary* (monad, Kleisli, operations, handlers)
but not the *power* (extensibility, equational reasoning). It is a
deliberately constrained subset. We use "algebraic effect" as informal
shorthand because the structural similarities to Koka, Effekt, and Eff
are real and the framing is illuminating. The more precise name is *a
closed Kleisli arrow algebra with signature-based dispatch over the
exception monad*.

#note[
  This framing matters for two reasons: (1) it gives disp a recognizable
  position in the PL literature, making cross-system communication
  easier; (2) it suggests a roadmap (§15) — relaxing "closed" to
  "open" yields user-installable effects, the natural extension.
]

= The six kernel primitives <sec:primitives>

This section gives operational semantics for each kernel primitive.
Each subsection covers: signature, semantics in disp source (or
pseudocode), soundness obligation, and composition properties.

== `hyp_reduce`

*Signature.* `hyp_reduce : {stored_type : Type, spine : List Tree_p} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* When a hypothesis `h` is applied to an argument `v`, the
dispatcher routes to `hyp_reduce`. The handler reads `h`'s stored type's
codomain function and uses the `Action` protocol (`Extend new_type | Return value`)
to decide whether to extend the spine or return a value.

*Disp source:*

```disp
let q_hyp_reduce_fn = {ks, raw, query} ->
  fix ({self, meta, v} -> {
    let stored_inner = neutral_meta_type meta
    let cod_fn       = pair_snd (pair_snd (type_meta stored_inner))
    let invalid_result = wait self (extend_neutral_meta meta InvalidType v)
    let dispatch_action = {action} ->
      match (is_extend action) {
        TT => wait self (extend_neutral_meta meta (pair_snd action) v)
        FF => match (is_return action) {
          TT => pair_snd action
          FF => invalid_result
        }
      }
    match (has_sig ks.predicate_frame stored_inner) {
      FF => invalid_result
      TT => match (tree_eq cod_fn t) {
        TT => invalid_result
        FF => dispatch_action (cod_fn ks raw meta v)
      }
    }
  })
```

*Soundness obligation.* The codomain function `cod_fn` runs raw (outside
the walker). Library authors writing `cod_fn`s must respect parametricity:
no triage on the input `v` unless `v` is known concrete.

== `predicate_frame`

*Signature.* `predicate_frame : {recognizer : P -> A -> CheckerResult Bool, params : P, codomain_fn : CodFn} -> Tree_p -> CheckerResult(Bool)`.

*Role.* Recognizes whether a value inhabits a type. For hypothesis
inputs, fires the H-rule (compare stored type to expected). For
concrete inputs, runs the recognizer through `ks.param_apply` so it
runs under the walker.

*Disp source:*

```disp
let q_predicate_frame_fn = {ks, raw, query} ->
  fix ({self, meta, v} -> {
    let check_fn = {ks, raw, query, self, meta, v} -> {
      let recognizer = pair_fst meta
      let params     = pair_fst (pair_snd meta)
      // The recognizer field is `P -> A -> CheckerResult Bool`. Running
      // it under the walker yields `CheckerResult (CheckerResult Bool)`:
      // the outer layer is from the walker (Err on parametricity trip),
      // the inner layer is the recognizer's own verdict-or-error. We
      // bind to flatten — any Err on either layer propagates uniformly.
      bind (ks.param_apply (recognizer params) v) ({inner} -> inner)
    }
    select q_h_rule_fn check_fn (q_is_neutral raw v)
      ks raw query self meta v
  })

let q_h_rule_fn = {ks, raw, query, self, meta, v} ->
  tree_eq (wait (ks query) meta) (neutral_meta_type (type_meta v))
```

*Soundness obligation.* The recognizer (in `pair_fst meta`) must be a
parametric function. Library authors construct recognizers using
`is_concrete_fork`, `tree_eq` against closed values, and similar
walker-safe predicates.

== `eliminator_frame`

*Signature.* `eliminator_frame : {dispatcher : Motive -> Cases -> A -> R, motive : Motive, cases : Cases} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* Case dispatch on values of inductive types. Mints `StuckElim`
on hypothesis targets so eliminations on unknown values stay opaque.

*Disp source:*

```disp
let q_eliminator_frame_fn = {ks, raw, query} ->
  {meta, x} -> {
    let count      = pair_fst meta
    let dispatcher = pair_fst (pair_snd meta)
    let acc        = pair_snd (pair_snd meta)
    let final_fn = {ks, raw, query, meta, x, count, dispatcher, acc} -> {
      let cases  = pair_snd acc
      let motive = pair_snd (pair_fst acc)
      let target = x
      select_lazy
        ({_} -> q_make_hyp raw (motive target) target)
        ({_} -> must_ok_or_self (ks.param_apply (dispatcher motive cases) target))
        (q_is_neutral raw target)
    }
    let partial_fn = {ks, raw, query, meta, x, count, dispatcher, acc} ->
      wait (ks query) (t (pair_snd count) (t dispatcher (t acc x)))
    select final_fn partial_fn
      (tree_eq count (t t t))
      ks raw query meta x count dispatcher acc
  }
```

The arity-tracking via `count` allows partial applications to remain
in wait-form, accumulating arguments until the final one fires the
dispatch.

== `bind_hyp`

*Signature.* `bind_hyp : {domain : Type, body : domain -> R} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* Introduces fresh hypotheses. Critically: when the domain type
is *applicable* (a function type), the minted hypothesis is wrapped
with `checked` so applications of function-typed hypotheses go through
input-checking.

*Disp source:*

```disp
let q_bind_hyp_fn = {ks, raw, query} ->
  {meta, x} -> {
    let count = pair_fst meta
    let acc   = pair_snd meta
    let final_fn = {ks, raw, query, meta, x, count, acc} -> {
      let domain = pair_snd acc
      let body   = x
      let hyp_id = t domain body
      let raw_hyp = q_make_hyp raw domain hyp_id
      // Wrap applicable-typed hypotheses with `checked`:
      let final_hyp = match (is_applicable_type domain) {
        TT => wait kernel.checked (pair (pi_dom domain) raw_hyp)
        FF => raw_hyp
      }
      let result_r = ks.param_apply body final_hyp
      match (is_ok result_r) {
        TT => {
          let result = ok_value result_r
          match (q_contains_via_open_path raw result raw_hyp) {
            TT => Err (Escape { hyp = raw_hyp, body_result = result, span })
            FF => Ok result
          }
        }
        FF => result_r        // re-raise upstream error unchanged
      }
    }
    let partial_fn = {ks, raw, query, meta, x, count, acc} ->
      wait (ks query) (t (pair_snd count) (t acc x))
    select final_fn partial_fn
      (tree_eq count (t t t))
      ks raw query meta x count acc
  }
```

The wrapping condition `is_applicable_type domain` is satisfied for
`Pi`-shaped types (and any future applicable type-former). For
non-applicable types like `Bool` or `Nat`, the hypothesis is bare.

*Escape check.* If the minted hypothesis is reachable in the body's
result via a non-neutral path, return
`Err (Escape { hyp, body_result, span })`. This prevents the
hypothesis from being smuggled into a context where it could be
exploited via predicate_frame's H-rule.

== `param_apply`

*Signature.* `param_apply : Tree_p -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* The dispatcher itself. Given two trees `f` and `x`, decide
whether `f` is an operation invocation (wait-form with a kernel
signature) and route accordingly; otherwise apply via the parametric
walker.

*Disp source* (the in-language reference is `checked_apply_walker` in
`lib/kernel/walker.disp`; the kernel handler `q_param_apply_fn` is a
small stub that the host's native fast-path intercepts):

```disp
// In-language reference (the spec):
param_apply := fix ({self, f, x} -> {
  match (tree_eq f I_canonical) {
    TT => Ok x                            // I-shortcut
    FF => triage
      (Ok (t x))                          // leaf rule
      ({a} -> stem_step a x)              // stem rule (with neutral-root check)
      ({a, b} -> fork_step self a b x)    // fork rule (K/S/triage with restrictions)
      f
  }
})
```

See §4 for the stem and fork step helpers with the parametricity
rejections inlined.

*Native fast-path.* The host runtime intercepts `apply(param_apply, ...)`
based on the kernel handler's tree id, runs a TypeScript implementation
of the same dispatch + walker discipline, and produces bit-identical
results. The in-language reference is the spec; the host is the
optimization.

== `checked`

*Signature.* `checked : {T : Type, v : T} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* Input-checked function application AND certificate tagging
(unified). The handler dispatches on whether the stored type is
applicable (function-like) or not:

- Applicable: check argument against domain, then apply.
- Non-applicable: applying makes no sense; return
  `Err (NotApplicable { type = T, span })`.

*Disp source:*

```disp
let q_checked_fn = {ks, raw, query} ->
  fix ({self, meta, arg} -> {
    let T = pair_fst meta
    let v = pair_snd meta
    match (is_applicable_type T) {
      TT => {
        let A = pi_dom T
        // Contract boundary: arg MUST satisfy A. Verdict-as-data
        // would be wrong here — the caller promised the type fits.
        bind (ks.param_apply A arg) ({verdict} ->
          match verdict {
            TT => ks.param_apply v arg
            FF => Err (TypeMismatch { expected = A, actual = arg, span })
          })
      }
      FF => Err (NotApplicable { type = T, span })
    }
  })
```

*Library constructors over `checked`:*

```disp
// Direct constructor — anyone can build a checked value.
// No typecheck performed; validation happens at outer let-binding.
checked := {T, v} -> wait kernel.checked (pair T v)

// Ergonomic alias for function-typed checked values:
typed_lambda := {A, B, f} -> checked (Pi A B) f

// Typecheck-gated constructor producing a certificate. Returns
// `Ok None` when the verdict is FF (legitimate query answer:
// no cert is issued). Returns `Ok (Some cert)` on TT. Soundness
// errors propagate through `Err`.
validate : Type -> Tree_p -> CheckerResult (Maybe Cert)

validate := {T, v} ->
  bind (typecheck T v) ({verdict} ->
    match verdict {
      TT => Ok (Some (checked T v))
      FF => Ok None
    })
```

*Soundness obligation.* `is_applicable_type` is a library predicate
that reads `T`'s metadata to decide. Under categorical foundations (§11),
it checks `T`'s `TypeFormer` for the `Applicable` field.

#openq[How best should `is_applicable_type` be defined? Options: (1)
check `T`'s recognizer matches `pi_pf_recognizer`; (2) read a flag from
`T`'s TypeFormer record; (3) check whether `T`'s cod_fn is non-sentinel.
Option (2) is most general but requires the categorical-foundations
TypeFormer to be in place. For now (1) suffices since only Pi is
applicable.]

= Boundary operations <sec:boundary>

== `param_lift`

The boundary sanitizer. Scans a value for embedded kernel-minted
neutrals and lifts into the Result monad on success.

```disp
param_lift := {v} ->
  match (scan_no_neutral v) {
    TT => Ok v
    FF => Err (Malformed { handler = "param_lift", meta = v, span })
  }

// scan_no_neutral: TT iff v contains no neutral-rooted subterm.
scan_no_neutral := {v} -> not (contains_neutral v)
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
```

`param_lift` is the *boundary lift* — the gate where untrusted user
values enter the Kleisli world. Inside `param_lift`'s output, you have
sanitary trees; outside, you have raw trees that might harbor forged
neutrals.

== `typecheck`

The user-facing query: "is `v` an inhabitant of `T`?" Composes
`param_lift` with `param_apply` and returns the verdict as data.

```disp
typecheck : Type -> Tree_p -> CheckerResult Bool

typecheck := {T, v} ->
  bind (param_lift v) ({sanitized_v} ->
    param_apply T sanitized_v)
```

`Ok TT` means inhabitant, `Ok FF` means not, `Err _` means
something soundness-level went wrong during the check (parametricity
violation in the recognizer, malformed input, etc.). The verdict is
data; the error channel carries only kernel-correctness failures.

== Example traces

*Trace 1: `typecheck Bool TT`.*
+ `param_lift TT` → `Ok TT` (TT contains no neutrals).
+ `param_apply Bool TT` → routes via predicate_frame signature → invokes Bool's recognizer → `TT` is a canonical Bool shape → returns `Ok TT`.
+ `typecheck` returns `Ok TT` — the verdict.

*Trace 2: `typecheck Bool (is_zero TT)`.*

Here `is_zero` is a `checked (Pi Nat Bool) is_zero_raw` value.

+ Compile-time `applyTree (is_zero, TT)`:
  - Dispatcher routes `apply(checked-wait-form, TT)` to the `checked` handler.
  - Handler checks `param_apply Nat TT`. Nat's recognizer rejects TT structurally → `Ok FF`.
  - `checked` is a contract boundary: `Nat` was promised, `Ok FF` means broken promise → raises `Err (TypeMismatch { expected = Nat, actual = TT, span })`.
  - So `is_zero TT` reduces to that `Err`.

+ Now `typecheck Bool (Err ...)`:
  - `param_lift` is called on an `Err` value. The wrapping monad propagates: `bind (Err e) k = Err e`.
  - Returns the original `Err TypeMismatch`, payload pointing at the inner mismatch.

The error originates inside the reduction (at the `checked` handler's
domain check, a contract boundary) and propagates outward unchanged.
The user gets the inner failure's full payload — the expected type,
the actual value, and the span — not an opaque outer "typecheck
failed."

Contrast: a *query* like `typecheck Bool zero` returns `Ok FF`, not
an error — `zero` simply isn't a Bool, which is a legitimate answer
to a query. No `Err` is involved.

= Checked values <sec:checked-values>

== Manifest contracts

Every typed function value in disp is *contract-wrapped*: it carries
its declared domain type, and applying it triggers a runtime input-
check. The wrapping is via the `checked` primitive (§6.6).

This is the manifest contracts discipline from the Findler-Felleisen
contract semantics, refined by Greenberg-Pierce-Weirich to identify
contracts with refinement types. See §17 for full citations.

== The three library constructors

All three are library functions over the single kernel primitive
`checked`:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Constructor*], [*Definition*], [*When used*],
    [`checked T v`], [`wait kernel.checked (pair T v)`], [Direct construction; no validation],
    [`typed_lambda A B f`], [`checked (Pi A B) f`], [Function values at construction time],
    [`validate T v`], [`typecheck T v >>= λverdict. Ok (if verdict then Some (checked T v) else None)`], [Querying whether a cert can be issued],
  ),
  caption: [Constructors for typed values.],
)

`typed_lambda` is purely ergonomic. `validate` is the proof-carrying
form — it doesn't just wrap, it ensures the wrap is sound by running
`typecheck` first.

== Soundness via structural recognizer checks

Forging a `checked` value (constructing `checked Bool zero` without
having actually validated zero against Bool) is *possible* at
construction — the constructor doesn't validate. But the lie is caught
when the value is used in a type-check.

Example trace: user writes `let bogus = checked Bool zero` (false claim).
- `bogus` evaluates to `wait kernel.checked (pair Bool zero)`.
- Later, the user calls `validate Bool bogus`:
  - Internally `typecheck Bool bogus` runs:
    - `param_lift bogus` → `Ok bogus`.
    - `param_apply Bool bogus` — Bool's recognizer triages on `bogus`.
      The wait-form is a fork with `kernel.checked` as `pair_fst`, not
      a canonical Bool shape. Returns `Ok FF`.
  - `validate` sees `Ok FF` from the verdict and returns `Ok None`
    — no certificate issued.

So forged certificates fail at first attempted validation: the user
gets `Ok None` (verdict-as-data) telling them the claim was bogus,
not a thrown error. Structural recognizer checks (each type knows
its own canonical shapes) catch the lie.

If `bogus` instead reaches a *contract boundary* — say, the user
applies a `f : Pi Bool C` to `bogus` — the `checked` handler raises
`Err (TypeMismatch { expected = Bool, actual = bogus, span })`,
because at that boundary the type was promised.

== Composition

Applying a `checked` value to an argument fires the handler:

```
(checked (Pi A B) f) x
→ ks.param_apply (kernel.checked) (pair (Pi A B) f) x
→ handler runs
→ bind (param_apply A x) ({verdict} -> match verdict {
    TT => param_apply f x
    FF => Err (TypeMismatch { expected = A, actual = x, span })
  })
→ on success:         Ok (f x)
→ on domain mismatch: Err (TypeMismatch { expected = A, actual = x, span })
```

Chained applications work the same way; each layer's handler fires
independently. For curried functions returning functions, the result
of one application may be another `checked` value, which is then
applied normally.

= Wrap-only elaboration <sec:elaboration>

== What the elaborator does

The elaborator's algorithm is minimal:

+ Parse syntax to AST.
+ Resolve variable references against scope entries (which carry their
  declared types).
+ At each lambda binder `{x : A} -> body`, wrap with
  `checked (Pi A B)` where `B` comes from the expected type at this
  position.
+ Bracket-abstract the binder (standard combinator translation).
+ At each let-binding `let name : T = body`, emit `name := body_tree`
  and run `typecheck T body_tree` once.

Type info flows *only top-down at binders*. There is no bottom-up type
synthesis, no unification, no `infer`/`check` ping-pong.

== What the elaborator does NOT need

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Not needed*], [*Why*],
    [`infer` function], [Type info is given by annotations, not synthesized],
    [Bidirectional `check`/`infer` ping-pong], [One top-level `typecheck` call per binding suffices],
    [Unification], [No inference means no constraint solving],
    [Constraint generation/solving], [Same],
    [Coercion insertion], [Subtyping is not supported],
    [Higher-order pattern unification], [Same as above],
  ),
  caption: [Things a standard elaborator does that disp does not.],
)

This minimalism is the *wrap-only* property. It's strictly less than
Lean/Coq elaboration (which does extensive type-class search, tactic
execution, unification).

== Restrictions on the source language

The wrap-only design constrains what the source language can express:

+ *Top-level `let`-bindings require `:` annotations.* `let foo = ...`
  without an annotation is rejected — there's no place for the type
  to come from.
+ *Local lambdas in untyped contexts are rejected.* Every lambda's
  domain must be inferrable from its enclosing context.
+ *No Hindley-Milner-style type variables.* Polymorphism is via
  explicit `Pi Type ({A} -> ...)` annotations.

In exchange: the elaborator is small, simple, and trivially audited.

== Compared to standard dependent-type elaboration

Lean and Coq use heavy elaboration — type-class search, implicit
argument inference, unification, tactic execution. The de Bruijn
criterion (de Moura et al. 2015) says the *kernel* (the type-checker
proper) is the trusted base; elaboration is auxiliary.

Disp pushes this minimalism further. Where Lean's elaborator does
substantial work to fill in elided information, disp requires the user
to provide it explicitly. The trade-off: more annotations in source,
but a much smaller TCB and no possibility of elaborator bugs producing
ill-typed kernel terms.

== Concrete sketch

For `let foo : Pi Nat ({_} -> Bool) = {x} -> is_zero x`:

+ Outer expected type: `Pi Nat ({_} -> Bool)`.
+ Extract domain `Nat` for the binder `{x}`.
+ Body `is_zero x` compiles to `apply(is_zero_compiled, x)`.
+ Wrap the binder: `checked (Pi Nat ({_} -> Bool)) ({x} -> apply(is_zero_compiled, x))`.
+ Bracket-abstract `{x}` over the body.
+ At the let-binding: `typecheck (Pi Nat ({_} -> Bool)) foo_tree`.

The typecheck reduces `foo_tree` via `param_apply`, fires the outer
`checked` handler against a Nat-hypothesis, runs the body, checks
result against `Bool`. Success → `Ok foo_tree`. Failure → an `Err`
variant of `CheckerError` carrying the failing type, value, and span.

= Strip and erasure <sec:strip>

== Motivation

Per-call `checked` handler invocations are not free: each application
of a typed function pays the cost of running the input-check (one
`param_apply` of the domain against the argument). For hot paths and
release builds, we want to elide these checks once the program has
been validated.

This is the *strip* pass: walk the validated tree and replace each
`checked` wait-form with its inner raw function. Result: a tree
equivalent to what raw tree-calculus apply would compute, without
type-checking overhead.

== Definition

```disp
strip := fix ({self, t} ->
  match (has_sig kernel.checked t) {
    TT => self (pair_snd (type_meta t))   // unwrap, recurse into inner
    FF => triage
      t                                   // leaf: unchanged
      ({c} -> t (self c))                 // stem: recurse
      ({l, r} -> t (self l) (self r))     // fork: recurse on both children
      t
  })
```

Strip is a tree-level function. It walks the entire tree, identifies
`kernel.checked` wait-forms by signature, and replaces each with its
underlying value (then recursively strips inside).

== Soundness

Strip is *not* unconditionally sound. Applying strip to an arbitrary
tree may remove checks that were necessary for soundness — e.g., if a
`checked Bool zero` value was waiting to be caught by an outer
`typecheck Bool`, stripping it would expose the lie.

Strip is sound *exactly when* applied to a tree that has been validated.
The standard usage pattern: pair strip with `validate`.

```disp
// Safe usage: validate first, strip the certified result.
strip_validated := {T, v} ->
  bind (validate T v) ({cert} ->
    Ok (strip (pair_snd (type_meta cert))))
```

A `Validated T v` certificate (the result of `validate`) attests that
`typecheck T v = Ok v` was true at construction time. Strip on the
inner v of a certificate is sound: every internal `checked` wait-form's
check has already been validated at certificate-construction time.

== Connection to proof-carrying code

This is Necula's proof-carrying code pattern (Necula 1997). The
elaborator/host plays the *certifier* role: it validates the program
once by running `typecheck`. The certificate (the `Validated`
wait-form) attests that validation happened. The strip pass plays the
*compiler optimization* role: once validated, runtime needn't re-check.

See §17 for the literature.

== Caching

Hash-cons gives free caching of `strip`'s output: `strip t` for the
same `t` produces the same tree id. Repeated stripping of the same
tree is O(1) (a single hash-cons lookup).

For repeated `typecheck T v` of the same `(T, v)`, a host-side
evaluation memo on `applyTree` provides the same O(1) speedup. No
specialized "validated tree" cache is needed; the generic
infrastructure suffices.

= TypeFormer records <sec:typeformer>

This section formalizes how library type-formers are structured under
the categorical-foundations reformulation. Replaces the prior
`CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ`.

== The categorical observation

Every library type-former in disp plays three categorical roles:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Role*], [*What it does*], [*Categorical structure*],
    [Recognizer],
      [Decides "is this tree an inhabitant?"],
      [Characteristic morphism `Tree → Ω` into subobject classifier (`Ω = Bool`)],
    [Eval / `∈`],
      [Behavior under application],
      [Eval morphism of an LCCC (for Pi-like) or `∈` (for predicate-like)],
    [Functor],
      [Transports values along type-paths; fills cubes],
      [Functor's morphism action between ∞-groupoids of types],
  ),
  caption: [Disp slots → standard categorical structures.],
)

These are not invented categorical roles. They are the *standard*
encoding of "membership," "application," and "functoriality" in topos
theory and ∞-category theory.

== The `TypeFormer` record

A type-former is a record with three fields, the categorical structures
named explicitly:

```disp
// Record types in disp use `{field : Type, ...}`. Record values use
// `{field := expr, ...}`. TypeFormer's three fields are the
// categorical structures, named explicitly:

TypeFormer := {
  classifier : SubobjectClassifier,
  applicable : Optional Applicable,
  functor    : Functor
}

SubobjectClassifier := {
  params                  : Type,
  characteristic_morphism : params -> Tree -> CheckerResult Bool
}

Applicable := {
  eval_morphism : EvalSignature
  // EvalSignature is the Kleisli arrow shape for application;
  // for Pi, it's the codomain_fn handler signature.
}

Functor := {
  morphism_action : InfFunctorActionSignature,
  identity_law    : Path morphism_action refl_action,
  composition_law : Path (compose m_action m_action) (compose m_action compose)
}
```

Every library type-former (`Bool`, `Nat`, `Pi`, `Sigma`, etc.) is
constructed by filling these fields.

== The smart constructor

```disp
make_type_former :
  (params : Type) ->
  (recognizer : params -> Tree -> CheckerResult Bool) ->
  (applicable : Optional EvalSignature) ->
  (functor_action : InfFunctorActionSignature) ->
  (identity_law : ...) ->
  (composition_law : ...) ->
  Type
```

Each argument is Pi-checked against its declared type. The smart
constructor produces a `predicate_frame_form` wait-form whose
metadata is a valid `TypeFormer` record.

== The bootstrap

The chicken-and-egg: `Type`'s recognizer is "the candidate's metadata
is a valid `TypeFormer` record." `TypeFormer` is itself a `Type`. So
checking `TypeFormer : Type` requires that `TypeFormer`'s metadata is
a valid `TypeFormer` record. Circular.

We break the circularity with a *primordial* `TypeFormer` instance
hand-constructed in disp source.

=== Step 1: kernel handlers exist

The six kernel primitives (`hyp_reduce`, `predicate_frame`, ...) are
defined in `lib/kernel/handlers.disp`. They use only tree-calculus
primitives plus prelude combinators (TT/FF, triage, select, pair,
wait/fix). No types involved yet — these are just trees.

The kernel record `kernel := rec { ... }` assembles them. The host
runtime registers their tree-id signatures (`kernel_hyp_reduce_sig`,
etc.).

=== Step 2: the primordial `TypeFormer`

The primordial is the first `TypeFormer` value, constructed by hand to
satisfy its own contract:

```disp
let typeformer_recognizer = {params, v} ->
  // Check v is a record matching the TypeFormer shape:
  //   pair classifier (pair (Optional applicable) functor)
  // where each field has the appropriate sub-shape. All sub-checks
  // are pure structural predicates on the tree (no kernel-mediated
  // calls), so the whole conjunction lifts to Ok in one step.
  Ok (and (is_fork v)
       (and (is_subobject_classifier (pair_fst v))
         (and (is_optional_applicable (pair_fst (pair_snd v)))
              (is_functor (pair_snd (pair_snd v))))))

// The primordial TypeFormer record itself:
let primordial_TypeFormer = pair
  primordial_classifier_field
  (pair primordial_applicable_field primordial_functor_field)

// Type = predicate_frame_form wrapping the primordial:
Type := predicate_frame_form (t typeformer_recognizer
                                (t primordial_TypeFormer t))
```

By construction, applying `Type` to `primordial_TypeFormer` reduces
through `typeformer_recognizer`, which verifies the structural shape
and returns `TT`. Self-consistency by construction.

The verification that `primordial_TypeFormer` satisfies its own contract
is a *manual proof obligation* — the implementer of this primordial
tree value must verify by inspection that the contract holds. After
that, all subsequent type-formers go through `make_type_former`, which
mechanically enforces the contract.

#note[
  *The trust seed.* The primordial `TypeFormer` is the load-bearing
  hand-crafted piece. Once it's correct, everything else bootstraps
  mechanically. This is metacircular in the same sense as Lisp's
  `eval`: the first instance is hand-written; subsequent ones use the
  established machinery.
]

=== Step 3: library types

With the primordial in place, library types are constructed via
`make_type_former`:

```disp
Bool := make_type_former
  Unit                  // no params
  bool_recognizer
  none                  // not applicable
  trivial_functor       // discrete: refl-only morphism action
  refl_identity_law
  refl_composition_law

Nat := make_type_former
  Unit
  nat_recognizer
  none
  trivial_functor
  refl_identity_law
  refl_composition_law

Pi := make_type_former
  (Sigma Type ({A} -> A -> Type))   // params = (A, B)
  pi_recognizer
  (some pi_eval_signature)
  pi_functor_action                  // non-trivial; supports transp
  pi_identity_law
  pi_composition_law
```

Each is validated against `Type` (which uses the primordial) at
construction.

=== Step 4: Russell-paradox safety

`Type : Type` holds in disp because `apply(Type, Type) = TT`. The
recognizer applied to itself just checks structural shape and accepts.

Russell-paradox attempts (encoding "the type of all types not containing
themselves") *diverge* during reduction rather than producing a logical
contradiction. The walker's parametricity discipline prevents the
introspective construction needed to encode the paradox; attempts hit
budget exhaustion before yielding a value.

This is "type-in-type via productivity": the universe is its own
inhabitant, but the type system's reduction discipline rules out
non-terminating witnesses to contradictions.

== Validation layers

Five levels of validation, from cheap to expensive:

+ *Structural validation.* `Type`'s recognizer checks the metadata is a
  `TypeFormer`-shaped tree. Cheap. Catches "this isn't a type-former at
  all."
+ *Component types.* Each field is Pi-checked against its declared type.
  The recognizer must have shape `params → Tree → CheckerResult Bool`; the
  `morphism_action` must have the right comp signature. Catches arity
  mismatches, return-type errors, missing fields.
+ *Behavioral testing.* For closed instances, run the operations on
  canonical inputs and check the categorical laws. Not part of `Type`'s
  recognizer; runs at library test time. Catches semantic errors that
  type-level validation misses.
+ *Propositional law witnesses.* `Functor.identity_law` and
  `composition_law` are `Path`-valued proofs. The kernel verifies the
  proofs *typecheck*; doesn't verify semantic correctness (undecidable).
+ *Full semantic verification.* Undecidable in general. Not pursued.

== Categorical hierarchy via record extension

Optional refinement (future direction): layer the records so operations
demand the level of structure they need.

```disp
BareType         := { classifier : SubobjectClassifier }
ApplicableType   := { base : BareType, applicable : Applicable }
FunctorialType   := { base : BareType, functor : Functor }
FullTypeFormer   := { base       : BareType,
                      applicable : Optional Applicable,
                      functor    : Functor }
```

`comp`/`transp` demand `FunctorialType`. Basic membership demands only
`BareType`. Trying to `transp` along a `BareType` is a structural
error.

#openq[Should we adopt this layered hierarchy now or wait until
operations require it? Adopting now adds verbosity but cleanly
separates capabilities.]

= Library types under Design Y <sec:library-types>

Each library type-former, recast under Design Y + categorical
foundations. The pattern: declare the `TypeFormer` fields; wrap any
function-typed metadata fields with `checked`; produce the type via
`make_type_former`.

== `Bool`

```disp
let bool_recognizer = {_, v} ->
  Ok (or (tree_eq v TT) (tree_eq v FF))

Bool := make_type_former
  Unit                     // no params
  bool_recognizer
  none                     // not applicable
  trivial_functor          // discrete
  refl_identity_law
  refl_composition_law
```

Recognizer is a closed parametric function. The structural check is
pure, so the body wraps in `Ok` directly. Discrete: transport is
identity, both laws are `refl`.

== `Nat`

```disp
let nat_recognizer = {_, v} ->
  // v is Nat iff v is zero (= LEAF) or v = fork(LEAF, n) where n is Nat.
  // Pure structural check on the tree; lifts to Ok in one step.
  Ok (fix ({self, x} ->
        triage TT                          // leaf: zero
          ({_} -> FF)                      // stem: not a Nat
          ({l, r} ->                       // fork: zero-like + recurse
            and (tree_eq l t) (self r))
          x) v)

Nat := make_type_former
  Unit
  nat_recognizer
  none
  trivial_functor          // discrete: transport identity
  refl_identity_law
  refl_composition_law
```

== `Pi`

The pivotal one — under Design Y, Pi's recognizer must require its
candidate be a `checked` wait-form.

```disp
let pi_recognizer = {params, v} -> {
  let A = pair_fst params
  let B = pair_snd params
  // Step 1 (pure structural): v must be a `checked` wait-form.
  match (has_sig kernel.checked v) {
    FF => Ok FF
    TT => {
      // Step 2 (pure structural): v's stored domain matches A.
      let v_A = pair_fst (type_meta v)
      match (tree_eq v_A A) {
        FF => Ok FF
        TT =>
          // Step 3 (kernel-mediated): bind a fresh A-hypothesis, apply
          // v to it, then check the result against B hyp. Every step
          // returns CheckerResult; `bind` propagates Err uniformly so
          // parametricity / escape / malformed errors surface to the
          // caller instead of being absorbed into a FF verdict.
          bind_hyp A ({hyp} ->
            bind (param_apply v hyp) ({result} ->
              param_apply (B hyp) result))
      }
    }
  }
}

let pi_eval_signature = ...  // standard codomain_fn signature
let pi_functor_action = ...  // non-trivial: supports transp via §13

Pi := make_type_former
  (Sigma Type ({A} -> A -> Type))    // params = (A, B)
  pi_recognizer
  (some pi_eval_signature)
  pi_functor_action
  pi_identity_law
  pi_composition_law
```

The three-step check (signature, domain match, bind-hyp+body) is what
makes Pi soundness-preserving under Design Y. Raw function values that
aren't `checked`-wrapped fail at step 1.

== `Sigma`, `Eq`, `Ord`, `Refinement`, `Record`, `Unit`, `String`

These follow the same pattern. Each defines a recognizer, builds a
`TypeFormer` via `make_type_former`. The recognizers are the same as
in the prior `lib/types/*.disp` files, but now wrapped with the
categorical-foundations record structure.

(Full definitions deferred to implementation; the framework is
established.)

== `Type` itself

`Type` is built via the primordial bootstrap described in §11.4. After
bootstrap:

```disp
Type := predicate_frame_form (t typeformer_recognizer
                                (t primordial_TypeFormer t))
```

`typecheck Type Type = Ok TT` by the primordial's self-consistency.

= Cubical extensions <sec:cubical>

This section folds in the cubical proposal. Cubical operations live in
the `Functor.morphism_action` field of each `TypeFormer`. No new kernel
primitives.

== Motivation

Disp's existing equality (`Eq A x y` with `refl`) is propositional —
proofs of equalities aren't computational. For representation-
independence and definitional iso-roundtrip, we need *paths* — functions
from an abstract "interval" type `I` to a type, whose endpoints are the
two values being identified.

The payoff: `transport_back p (transport p x)` reduces to `x` structurally
(via the per-type `transp_fn` rules), and conversion via `tree_eq` returns
`TT`. This is representation-independence as a computational fact rather
than a manually-discharged proof obligation.

== The interval `I`

`I` is a library predicate_frame type whose elements are formulas in
the free De Morgan algebra:

```disp
// In disp source, the stem(x) constructor is written `t x`.
// Tags are distinct stems-of-leaf chains.
let tag_zero = t t                     // stem(LEAF):           0
let tag_one  = t (t t)                 // stem(stem(LEAF)):     1
let tag_and  = t (t (t t))             // stem^3(LEAF):         ∧
let tag_or   = t (t (t (t t)))         // stem^4(LEAF):         ∨
let tag_inv  = t (t (t (t (t t))))     // stem^5(LEAF):         ¬

let I_zero = pair tag_zero t
let I_one  = pair tag_one  t
let I_and  = {a, b} -> pair tag_and (pair a b)
let I_or   = {a, b} -> pair tag_or  (pair a b)
let I_inv  = {a}    -> pair tag_inv a

let I_recognizer = {_, v} -> ...
  // v is in I iff it's I_zero, I_one, a tagged operation, or a neutral
  // of stored type I. Library smart constructors normalize formulas to
  // DNF so De-Morgan-equivalent formulas hash-cons to identical trees.

I := make_type_former
  Unit
  I_recognizer
  none
  trivial_functor       // I doesn't transport
  refl_identity_law
  refl_composition_law
```

A library `I_normalize` reduces formulas to canonical (DNF) form;
two I-formulas are De-Morgan-equal iff their normalized trees are
hash-cons-identical.

== `Path` and `PathP` as `Pi I` aliases

`Path A x y` and `PathP A x y` don't need separate type-formers. They
are aliases over `Pi I`:

```disp
Path  := {A, _, _} -> Pi I ({_} -> A)
PathP := {A, _, _} -> Pi I A
```

Endpoint arguments are documentation; the body recovers them as
`p I_zero` and `p I_one`. The walker's discipline (no triage on `i`)
ensures path bodies behave correctly.

Core operations are one-liners:

```disp
refl   := {A, x, i} -> x
cong   := {A, B, f, p, i} -> f (p i)
sym    := {A, p, i} -> p (I_inv i)
funext := {A, B, h, i, a} -> h a i
```

== `transp` and per-type `morphism_action`

The `Functor.morphism_action` field of each `TypeFormer` is the
*comp_fn* — the function that computes transport along a type-path
for values of this type-former.

`transp` is a library function that dispatches on the target type's
`Functor.morphism_action`:

```disp
transp := fix ({self, P, x} -> {
  let T0 = apply P I_zero
  let T1 = apply P I_one
  match (tree_eq T0 T1) {
    TT => x                                    // fast path: constant family
    FF => {
      let action = functor_morphism_action_of T0
      match (tree_eq action t) {
        TT => StuckElim T1 (pair P x)          // sentinel: no transport rule
        FF => apply action (pair self (pair P x))
      }
    }
  }
})
```

Per-type rules are supplied as the `functor.morphism_action` field
when each type-former is constructed via `make_type_former`. Sketches
of the per-type clauses (each is the `morphism_action` argument when
constructing the type):

```disp
// Discrete types (Bool, Nat, False): transport is identity.
let bool_morphism_action  = {self, P, x} -> x
let nat_morphism_action   = {self, P, x} -> x

// Pair: component-wise recursion.
let pair_morphism_action  = {self, P, x} -> ...

// Sigma: dependent second component via a-trajectory.
let sigma_morphism_action = {self, P, x} -> ...

// Pi: contravariant in A, covariant in B; threads a-trajectory through B.
let pi_morphism_action    = {self, P, f} -> ...

// Eq: refl at the new endpoints.
let eq_morphism_action    = {self, P, p} -> ...
```

Each is passed to `make_type_former` when constructing the corresponding
type. The pattern: recurse component-wise where possible; stuck-mint
via `StuckElim` for non-structural cases. Full per-type rules carry
over from the prior `CUBICAL_PROPOSAL.typ` §5.2.

== `Partial` and cofibrations

`IsOne phi : Type` is the proposition "phi reduces to I_one." `Partial
phi A := IsOne phi -> A`. Walker-safe smart constructors for face
systems.

```disp
IsOne := make_type_former
  I                       // params = i : I
  isone_recognizer
  none
  trivial_functor
  refl_identity_law
  refl_composition_law

Partial := {phi, A} -> Pi (IsOne phi) ({_} -> A)
```

(Full design from `CUBICAL_PROPOSAL.typ` §11 carries over; the
recognizer enforces "phi = I_one or phi has the canonical disjunction
shape.")

== `hcomp` and the unified `comp`

`hcomp` (homogeneous composition) and `transp` (heterogeneous
transport along a type-path) unify into `comp`:

```disp
comp := fix ({self, P, phi, u, u0} -> {
  let T = apply P I_zero
  let cfn = functor_morphism_action_of T
  match (tree_eq cfn t) {
    TT => StuckElim (apply P I_one) ...      // stuck
    FF => apply cfn ...
  }
})
```

Each `Functor.morphism_action` for a type-former handles both the
homogeneous and heterogeneous cases via its argument structure.

== `Glue` and univalence

`Glue B [phi ↦ (T, e)]` is a library type that "glues" a base type B
with partial type information (T, e) at the face phi. Its non-trivial
`Functor.morphism_action` implements equivalence-mediated transport.

```disp
Glue := make_type_former
  glue_params_shape       // (B, T, e) — base, partial type, equivalence
  glue_recognizer
  none
  glue_functor_action     // applies the equivalence on transport
  glue_identity_law
  glue_composition_law

// ua constructs a Path Type A B from an equivalence e : A ≃ B.
// Notation `[(i = I_one) ↦ (A, e)]` is mathematical shorthand for the
// partial element supplying (T, e) at the i=I_one face; the actual
// disp source materializes this as a face-system tree.
ua := {A, B, e} -> {i} ->
  Glue B (make_face_system i I_one A e)
```

Transport along `ua e` reduces via `Glue.functor.morphism_action`,
which applies the equivalence `e`. This makes univalence a definable
library theorem (not an axiom).

== HIT eliminator machinery

Higher inductive types have constructors that include path-equalities.
Their eliminators must respect those equalities. Full HIT support
requires additional library scaffolding beyond what's in this section.

#openq[HITs are sketched briefly here for completeness; full
implementation is deferred to a follow-up document on HIT mechanisms.
The framework handles HITs in principle (constructor paths live in the
eliminator's metadata), but the operational details need their own
treatment.]

== What this delivers

+ *Definitional iso-roundtrip* for ua-mediated equivalences.
+ *Structural transport on type-formers* via the per-type-former rule
  in `Functor.morphism_action`.
+ *Univalence as a definable theorem* via `Glue` + `ua`.
+ *Representation independence in practice* — functions over one
  representation work on equivalent representations via transport.
+ *No kernel growth.* The six primitives remain six.

== Limitations

+ *Endpoint-only path evaluation.* `transp` evaluates `P` at `I_zero`
  and `I_one`, then dispatches on the shared head former. Path bodies
  whose intermediate behavior matters are not captured.
+ *Walker constraints on path bodies.* Path bodies cannot triage on
  their `i`-argument. Bodies must use `i` only through I-operations
  or by passing it to I-consuming type formers.
+ *Conversion modulo De Morgan.* `tree_eq` compares structurally;
  library smart constructors normalize I-arguments so hash-cons
  captures De-Morgan equivalence.

= Soundness theorem <sec:soundness>

== Statement

#note[
  *Theorem (Soundness).* Let `T` be a tree such that
  `param_apply Type T = Ok TT` (T is a valid type). Let `v` be a tree
  such that `typecheck T v = Ok v`. Then for any context in which `v`
  is used — including reductions under hypotheses of arbitrary types —
  no application within `v` produces a value whose type contradicts the
  type structure established by `T`.

  In particular:
  - If `T = Pi A B`, then `v` is a `checked` wait-form whose stored
    domain matches `A`, and `v` applied to any value of type `A`
    produces a value of type `B(arg)`.
  - If `T = Bool`/`Nat`/etc., then `v` is structurally an inhabitant
    of `T`'s canonical shapes.
  - Internal applications inside `v` either type-check correctly or
    reduce to an `Err CheckerError` during evaluation.
]

== Proof sketch

By induction on the structure of `T`.

*Base cases* (Bool, Nat, Unit, etc., discrete types). `T`'s
recognizer is a closed parametric function that checks `v`'s structural
shape. If `typecheck T v = Ok v`, then `v` has the right shape. No
applications are involved; soundness is immediate.

*Inductive case: Pi types.* For `T = Pi A B`, the recognizer (§12.3)
requires three properties:
+ `v` is a `kernel.checked` wait-form.
+ `v`'s stored domain matches `A` exactly (hash-cons identity).
+ `bind_hyp A (\hyp -> (B hyp) (v hyp))` returns TT.

The bind_hyp body is verified under a fresh `A`-hypothesis. Since
`bind_hyp` wraps applicable-typed hypotheses with `checked` (§6.4),
applications of `hyp` inside `v`'s body go through `kernel.checked`
input-checking. The walker's parametricity discipline (§4) prevents
introspection.

By induction on `B`'s structure, the body check `B(hyp)(v hyp) = Ok TT`
holds for the specific hyp. By parametricity (the walker rejects
hypothesis introspection), the property generalizes to all values of
type `A`: applying `v` to any A-value yields a B-value.

*Inductive case: Sigma, Refinement, Eq, etc.* Similar structural
recursion using each type-former's recognizer. The recognizer checks
each component; soundness follows by induction on components.

*The trust boundary* is precisely:
- The six kernel handlers, implemented in disp source.
- The host runtime's signature-pinning of the kernel operations.
- The parametric walker (in-language reference + native fast-path).
- The hand-crafted primordial `TypeFormer` (§11.4).

What is *not* in the trusted base:
- The elaborator (it just emits wrapped trees; the kernel re-validates).
- Library type-former definitions (validated against `Type`'s recognizer).
- User code (validated at every application via `checked`).

== What this gives compared to the prior TYPE_THEORY.typ

Today's disp has a similar soundness story but the TCB is larger: the
elaborator's stepwise infer/check is implicitly trusted. Under Design Y,
the elaborator just wraps; the kernel validates each binding via
`typecheck`. The TCB shrinks to "the six kernel handlers + the walker +
the primordial."

#openq[The proof sketch is informal. A formal proof (Coq/Lean
mechanization, or a careful pencil-and-paper version with PER-model
semantics) would solidify it. Deferred.]

= Future: user-installable effects <sec:future>

Speculative section on extending disp toward Koka-style effects.

== The current state

Disp has one effect: the type-checking effect, with monad
`CheckerResult`. The kernel is the unique handler. Operations are
dispatched by structural signature.

== The forward direction

Adding user-installable effects amounts to relaxing "closed" to "open"
in §5.4. Each new effect is a monad (perhaps composed with
`CheckerResult` via monad transformers); operations are dispatched by
context-aware signature matching; handlers are installable via a
`with handler { ... }`-style construct.

Concretely, primitive trees with pinned signatures become host-
intercepted effect operations. `param_apply` gains an effect-context
parameter; operations dispatch based on what handlers are installed in
the current context.

== Three-stage migration

+ *Stage 1* (this document): document and use the algebraic-effects
  framing. Internal vocabulary aligned.
+ *Stage 2*: factor the kernel handler into smaller reusable Kleisli
  arrows. Identify which operations are "kernel-essential" vs
  "extensible."
+ *Stage 3*: add user-installable handlers and effect-row tracking
  (Koka-style). Type system extends to track effect rows.

The mechanism for dispatch (signature matching) is already in place
from Stage 0; only the "open" extension requires new infrastructure.

== Effect rows in disp

A future disp type might look like:

```
print : String → <console> ()
random : Nat → <random> Nat
```

The `<...>` row specifies which effects can be invoked. Pure code has
empty row. Handler installation transforms rows (discharging the
handled effect).

Type-checking enforces the row: invoking a primitive whose signature
isn't in the active row is a type error.

This is speculative; not committed to. The framing supports it; the
implementation work is its own project.

= What's genuinely disp-specific <sec:disp-specific>

A short section identifying what disp contributes beyond standard
machinery from category theory, contract theory, and dependent type
theory.

== Tree calculus as the substrate

The choice of combinator-based, hash-consable, bound-variable-free
representation. Other type theories make different substrate choices
(CIC, Agda core, MLTT terms); disp's choice gives O(1) structural
equality and trivial self-representation. This enables the metacircular
discipline.

== Structural parametricity

The walker's two restrictions (no triage on neutrals, no neutral-rooted
fork construction) are a *local, structural* enforcement of
parametricity. Standard parametric type theories require global
type-level reasoning (e.g., relational interpretation of types). Disp
gets the parametricity property from local pattern-matching on tree
structure.

== Wrap-only elaboration

Standard contract-compilation does syntactic inference of contract-
eligible positions; disp's elaborator goes further by doing zero
bottom-up type computation. Type info flows only top-down at binders.
The elaborator is purely a wrapping pass.

This is more minimal than Lean/Coq elaboration (which does substantial
inference) and even more than standard Findler-Felleisen contract
compilation (which inserts contracts at typed-untyped boundaries).
Disp's wrap-only design is novel in this combination.

== Unified `checked` primitive

Manifest-contract systems typically distinguish runtime checks from
validation certificates. Disp unifies them via dispatch on the stored
type's applicability. One primitive `checked` plays both roles. The
unification is what allows `typed_lambda` and `validate` to be library
aliases over a single kernel operation.

== Metacircular discipline

The kernel is its own type-checker; the type system is defined in
disp source; the host implements optimizations but not semantics. The
primordial `TypeFormer` is the only hand-crafted seed; everything else
bootstraps mechanically.

Standard dependently-typed languages have substantial host-language
infrastructure (Coq in OCaml + Coq itself; Lean in C++ + Lean itself).
Disp pushes the host's role to "tree-calculus runtime with hash-cons
and a native fast-path" — everything type-theoretic lives in `lib/`.

= Related work <sec:related-work>

Disp's design draws from multiple established research traditions. The
literature provides precise vocabulary for each design move.

== Manifest contracts and contract compilation

The wrapping semantics for typed function values follows
Findler & Felleisen (2002), "Contracts for Higher-Order Functions"
(ICFP). The identification of contracts with refinement types is
Greenberg, Pierce & Weirich (2010), "Contracts Made Manifest" (POPL),
which introduces the manifest calculus λH with refinement types and
casts.

Polymorphic extensions: Belo, Greenberg, Igarashi, Pierce (2011),
"Polymorphic Contracts" (ESOP), and Sekiyama, Igarashi, Greenberg
(2017), "Polymorphic Manifest Contracts, Revised and Resolved" (TOPLAS).

The contract-as-projection formal semantics: Findler & Blume (2006),
"Contracts as Pairs of Projections" (FLOPS).

== Hybrid type checking

Disp's unification of static and dynamic checking via the same
contract mechanism follows Flanagan (2006), "Hybrid Type Checking"
(POPL). Static checking is contract evaluation at elaboration time;
dynamic checking is contract evaluation at runtime; strip is the
erasure pass.

== Proof-carrying code

The strip-after-validation pattern is Necula (1997), "Proof-Carrying
Code" (POPL). The certifier validates once; the consumer runs without
re-checking; the proof obligation is discharged offline.

== Erasure in dependent type theory

The strip pass's formal soundness is type-theoretic erasure. Mishra-
Linger & Sheard (2008), "Erasure and Polymorphism in Pure Type Systems"
(FoSSaCS), gives the phase-distinction theorem. Tejiščák (2020), "A
Dependently Typed Calculus with Pattern Matching and Erasure
Inference" (ICFP), gives the inference algorithm.

Earlier: Pfenning (2001), "Intensionality, Extensionality, and Proof
Irrelevance" (LICS), and the Coq/Lean Prop sort.

== Elaboration in dependent type theory

The elaborate-then-check discipline is de Moura, Avigad, Kong & Roux
(2015), "Elaboration in Dependent Type Theory" (ITP); Sozeau et al.
(2020), "Coq Coq Correct! Verification of Type Checking and Erasure
for Coq, in Coq" (POPL). Disp's wrap-only elaboration is at the
minimal end of this design space.

== Algebraic effects

The closed-handler framing in §5 follows Plotkin & Power (2002),
"Notions of computation determine monads," and Plotkin & Pretnar
(2013), "Handlers of algebraic effects" (LMCS). Practical
implementations: Bauer & Pretnar (Eff), Leijen (Koka), Brachthäuser
et al. (Effekt).

Compilation: Leijen (2017), "Type Directed Compilation of Row-Typed
Algebraic Effects" (POPL); Xie, Brachthäuser, Hillerström, Schuster,
Leijen (2020), "Effect Handlers, Evidently" (ICFP).

== Cubical type theory

The cubical framework follows Cohen, Coquand, Huber, Mörtberg (CCHM
2015), "Cubical Type Theory: A Constructive Interpretation of the
Univalence Axiom." The HoTT book (Univalent Foundations Program 2013).
The Glue type and ua construction are from CCHM.

== Categorical foundations

Standard category-theoretic notions: Mac Lane, "Categories for the
Working Mathematician" (1998); Awodey, "Category Theory" (2010);
Borceux, "Handbook of Categorical Algebra." Topos theory: Mac Lane &
Moerdijk, "Sheaves in Geometry and Logic" (1992).

== The disp-specific paragraph

Putting it together as it might appear in a paper:

#note[
  Disp's typed values are *manifest contracts* in the sense of Greenberg,
  Pierce & Weirich (2010), implemented via the function-proxy wrapping
  semantics of Findler & Felleisen (2002). The unification of static
  and dynamic type-checking through the same contract mechanism is
  Flanagan's *hybrid type checking* (2006). Elaboration is a *contract-
  compilation by wrapping* (Findler & Blume 2006), kept minimal so the
  kernel is the sole trusted checker, following the de Bruijn criterion
  as articulated by de Moura et al. (2015). The strip pass is
  type-theoretic erasure (Mishra-Linger & Sheard 2008; Tejiščák 2020);
  its soundness is the proof-carrying-code pattern (Necula 1997).
  The kernel handler architecture is structurally analogous to a closed
  Plotkin-Pretnar algebraic effect system. Cubical operations follow
  the CCHM framework. The categorical foundations are standard topos
  theory.
]

= References <sec:references>

== Papers

- Findler & Felleisen (2002). "Contracts for Higher-Order Functions." ICFP. DOI 10.1145/581478.581484.
- Necula (1997). "Proof-Carrying Code." POPL. DOI 10.1145/263699.263712.
- Plotkin & Power (2002). "Notions of computation determine monads." FoSSaCS.
- Plotkin & Pretnar (2013). "Handlers of algebraic effects." LMCS 9(4).
- Flanagan (2006). "Hybrid Type Checking." POPL. DOI 10.1145/1111037.1111059.
- Findler & Blume (2006). "Contracts as Pairs of Projections." FLOPS.
- Pfenning (2001). "Intensionality, Extensionality, and Proof Irrelevance in Modal Type Theory." LICS.
- Greenberg, Pierce & Weirich (2010). "Contracts Made Manifest." POPL. DOI 10.1145/1706299.1706341.
- Mishra-Linger & Sheard (2008). "Erasure and Polymorphism in Pure Type Systems." FoSSaCS. DOI 10.1007/978-3-540-78499-9_25.
- Belo, Greenberg, Igarashi & Pierce (2011). "Polymorphic Contracts." ESOP.
- de Moura, Avigad, Kong & Roux (2015). "Elaboration in Dependent Type Theory." ITP. arXiv 1505.04324.
- Cohen, Coquand, Huber & Mörtberg (2015). "Cubical Type Theory."
- Leijen (2014). "Koka: Programming with Row-Polymorphic Effect Types." MSFP. arXiv 1406.2061.
- Leijen (2017). "Type Directed Compilation of Row-Typed Algebraic Effects." POPL.
- Sekiyama, Igarashi & Greenberg (2017). "Polymorphic Manifest Contracts, Revised and Resolved." TOPLAS.
- Tejiščák (2020). "A Dependently Typed Calculus with Pattern Matching and Erasure Inference." ICFP. DOI 10.1145/3408973.
- Xie, Brachthäuser, Hillerström, Schuster & Leijen (2020). "Effect Handlers, Evidently." ICFP.
- Sozeau et al. (2020). "Coq Coq Correct! Verification of Type Checking and Erasure for Coq, in Coq." POPL.
- Univalent Foundations Program (2013). "Homotopy Type Theory: Univalent Foundations of Mathematics." HoTT book.

== Books

- Mac Lane (1971/1998). "Categories for the Working Mathematician."
- Mac Lane & Moerdijk (1992). "Sheaves in Geometry and Logic: A First Introduction to Topos Theory."
- Awodey (2010). "Category Theory."
- Borceux (1994). "Handbook of Categorical Algebra."

== Disp-internal documents

- `GOALS.md` — north star and metacircular discipline.
- `KERNEL_DESIGN.md` — host-side implementation idioms.
- `SYNTAX.typ` — surface grammar.
- `COMPILATION.typ` — parse / elaborate / emit pipeline.
- `RECORDS_PROPOSAL.md` — records and projection (uses this framework).
- `INTERACTIVE_WALKTHROUGH.html` — pedagogical introduction.

#v(2em)

#note[
  *Document conventions.* Disp source code in this document follows the
  syntax established in `SYNTAX.typ`. Cross-references use `§N.M` for
  sections and code locations like `file:line` for source links.
  Open questions are marked `Open question:`. The document is designed
  for section-by-section iteration; revisions should preserve section
  numbering until a major restructuring.
]
