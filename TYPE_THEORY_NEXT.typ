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
  - The kernel surface drops from 7 to 5 primitives (removed `guard`,
    `unguard`, `predicate_frame`; added `checked`; predicate_frame
    relocated to library as `type_recognizer` since its handler body
    is walker-safe).
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
    [§6 The five primitives], [Operational semantics of each kernel handler],
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

== Pairs and projections

`pair`, `pair_fst`, and `pair_snd` are library functions over the
substrate, not kernel primitives. The spec uses them pervasively for
extracting fields from records, wait-forms, and metadata trees.

```disp
// pair a b = fork(a, b). In disp source, `t` writes LEAF and tree
// construction is left-associative application, so `t a b` is
// fork(a, b).
pair := {a, b} -> t a b

// Projections via triage. The leaf and stem branches are degenerate
// (pairs are forks); they exist only so the function is total.
pair_fst := {p} -> triage t ({x} -> x) ({l, r} -> l) p
pair_snd := {p} -> triage t ({x} -> x) ({l, r} -> r) p
```

On a regular pair `pair a b = fork(a, b)`, the projections behave as
expected: `pair_fst (pair a b) = a` and `pair_snd (pair a b) = b`.

These same projections are also applied to *wait-forms* — partial
applications of the `wait` combinator — whose tree shape is *not* a
plain `fork(a, b)`. The relationship between projection behavior on
those trees and the kernel's signature-based dispatch is the subject
of §5.4.

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
       `hyp_reduce`, when the stored type isn't a recognized
       type wait-form],
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

This split is what keeps soundness errors visible. Folding any
failure into `FF` — the obvious shortcut — would silently mask a
parametricity violation inside a recognizer body as "this isn't an
inhabitant." Here, every recognizer is required to *be* a recognizer
(returning a verdict, never absorbing soundness errors), and the
lifting of `FF → Err` happens only where it is contractually
justified (`checked` and similar typed boundaries).

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

+ *Define new type-formers as wait-forms.* Use the substrate's `wait`
  combinator with a library recognizer (§11). For inductive types
  that need stuck-elimination on neutrals, use `eliminator_frame_form`.

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
Σ = { hyp_reduce, eliminator_frame, bind_hyp, param_apply, checked }
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
    record. Type variables: `A` = type, `P` = closed params, `R` =
    result type, `Motive`/`Cases` = dispatcher-specific.],
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

`wait` is the library combinator (`lib/prelude.disp`):

```disp
wait := {a, b, c} -> t (t a) (t t c) b
```

so `wait a b c = a b c` but `wait a b` is a *stuck partial
application* — bracket abstraction freezes it as a specific tree
shape, not as `fork(a, b)`. A "wait-form" is `wait kernel.op meta`
in this two-arg partial state: a delayed operation invocation.

The wait-form's tree shape is not a plain pair, but it has two
properties that the dispatcher and library code rely on:

*Signature stability.* `pair_fst (wait k m)` is a derived constant
that depends only on `k`, not on `m`. The library defines

```disp
checker_sig := {checker} -> pair_fst (wait checker t)
has_sig    := {checker, v} -> tree_eq (pair_fst v) (checker_sig checker)
```

so `pair_fst (wait k m) = checker_sig(k)` for every `m`. Hash-cons
identity (§2.2) makes the comparison O(1); the host runtime
registers each kernel operation's `checker_sig` once at startup.
(So the casual phrasing "pair_fst of a wait-form is the operation's
signature" really means "the signature-constant derived from the
operation via the wait-encoding," not the operation function
itself.)

*Meta accessibility.* `pair_snd (wait k m)` is a tree structurally
containing `m`, recoverable by the handler. Library types use this
to inspect metadata without firing the handler — e.g. extracting a
`TypeFormer` record from a wait-form's payload before applying it
to anything.

With these two properties in place, the dispatcher is a single
privilege check:

```
param_apply f x:
  if pair_fst(f) is in kernel_sigs  → run raw (just `f x`)
  else                              → walker step (§4)
```

*The dispatcher does not route to handlers.* Once privilege is
granted, the wait-form's bracket-abstracted reduction
(`wait k m x` → `k m x`) invokes the embedded handler automatically
— no mapping table required. The dispatcher's only choice is between
two execution modes: raw apply for trusted reductions, walker apply
for everything else. See §6.5 for the in-language reference and how
`kernel_sigs` is derived from the kernel record itself.

=== Why wait-forms, not plain partial application

Semantically, meta is just an additional argument to the handler —
`wait kernel.op meta arg` is `handler(meta, arg)` with extra
ceremony. The wait-form encoding buys three things that direct
partial application does not:

+ *O(1) privilege check.* The signature constant
  `checker_sig(k) = pair_fst(wait k t)` is hash-cons-stable: every
  wait-form sharing the same operation produces the same pair_fst
  regardless of metadata. The dispatcher decides "is this a trusted
  kernel op?" via a single id-comparison membership check (§6.5).
  Routing to the specific handler isn't needed — the wait-form
  reduces directly to its embedded handler under raw apply.
  A directly partially-applied function would lack a stable
  signature to gate privilege on.

+ *Meta inspection without forcing the handler.* Library type-formers
  need this: `Bool = wait bool_recognizer bool_meta` exposes `bool_meta`
  (the type's metadata record) via `pair_snd Bool` without ever
  applying `Bool` to anything.

+ *Stability under bracket abstraction.* A plain partial application
  compiles, via bracket abstraction, into a tree where the S-combinator
  distributes meta into the handler body wherever it's used —
  scattering it past the dispatcher's reach. The wait combinator's
  specific shape (`t (t a) (t t c) b`) keeps the signature derivable
  from pair_fst and the meta reachable from pair_snd, regardless of
  how the handler body uses meta internally.

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
- Library types (`Pi`, `Sigma`, `Bool`, etc.) are derived terms — wait-forms over library recognizers. Inductive types use `eliminator_frame_form` for case dispatch.

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

= The five kernel primitives <sec:primitives>

This section gives operational semantics for each kernel primitive.
Each subsection covers: signature, semantics in disp source (or
pseudocode), soundness obligation, and composition properties.

The kernel surface is deliberately small: only operations that
require privileged construction (minting kernel-rooted forms the
walker would otherwise reject) live here. Type recognition, typed
function application, and most type-system machinery live in the
library — see §11 for the framing.

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
    match (is_type stored_inner) {
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
no triage on the input `v` unless `v` is known concrete. `is_type` is a
library structural check (it tests whether `stored_inner` is a wait-form
with the standard type-metadata layout — see §11).

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

*Role.* The dispatcher. One privilege check decides between trusted
raw execution (for kernel-operation invocations) and unprivileged
walker reduction (for everything else). The dispatcher does not route
to specific handlers — wait-form reduction does that automatically
(§5.4).

*`kernel_sigs` is derived from the kernel record.* Adding a handler
to `kernel` registers its signature automatically; there is no
hardcoded list to keep in sync.

```disp
// Extract a record's field chain via Option B's identity inspector.
record_chain := {r} -> r ({x} -> x)

// Walk a Sigma chain into a list.
chain_to_list := fix ({self, c} ->
  match (tree_eq c unit_witness) {
    TT => nil
    FF => cons (pair_fst c) (self (pair_snd c))
  })

kernel_sigs := list_map
  ({h} -> checker_sig h)
  (chain_to_list (record_chain kernel))

is_kernel_sig := {sig} ->
  list_any ({s} -> tree_eq s sig) kernel_sigs
```

*The dispatcher.*

```disp
param_apply := fix ({self, f, x} ->
  match (and (is_wait_form f) (is_kernel_sig (pair_fst f))) {
    TT => f x                              // raw apply: handler embedded in f
    FF => walker_step self f x             // walker (§4)
  })
```

The two arms have different semantics:

- *Raw arm.* `f x` is plain substrate apply (§2.1) — no walker
  enforcement, no further dispatch at this level. The wait-form's
  bracket-abstracted shape reduces `wait k m x` to `k m x`, invoking
  the handler whose privilege we just granted. The handler's body,
  running raw, can do operations the walker would reject — minting
  neutrals (`bind_hyp`), extending spines (`hyp_reduce`), constructing
  kernel-signature-rooted wait-forms for H-rule reconstruction.

- *Walker arm.* `walker_step` is the parametricity-enforcing
  reduction of §4. Its internal sub-applies re-enter
  `self.param_apply` rather than calling substrate apply directly,
  so dispatch happens at every layer of nested reduction. Any
  sub-tree that happens to be a kernel wait-form gets routed back
  to the raw arm.

*Native fast-path.* The host runtime intercepts `apply(param_apply, ...)`
based on the compiled tree id and runs a TypeScript implementation of
the same logic. The in-language version is the spec; the native is
the optimization, producing bit-identical results.

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

= Elaboration and tests <sec:elaboration>

The elaborator is a *purely syntactic transformation*. It parses,
resolves references, and emits trees and tests. It performs no
type-checking judgments itself — those happen via library validators
invoked through tests.

== What the elaborator does

The elaboration steps:

+ Parse syntax to AST.
+ Resolve variable references against scope entries (which carry their
  declared types).
+ *Lambda wrapping.* At each lambda binder `{x : A} -> body` whose
  surrounding type is known to be `Pi A B`, wrap the compiled body
  with `checked (Pi A B)` (§8). This ensures typed function values
  carry their declared types as runtime contracts.
+ Bracket-abstract the binder (standard combinator translation).
+ *Test emission.* At each `let name : T = body`, emit two operations:

```
let name = body_tree
test typecheck T name = TT
```

The first is the binding (no validation). The second is the test
that the elaborator runs immediately.

Type info flows *only top-down at binders*. There is no bottom-up type
synthesis, no unification, no `infer`/`check` ping-pong.

== The `test` keyword and `=`

`=` is *infix `tree_eq`*. So `expr1 = expr2` is `tree_eq expr1 expr2`,
returning a Bool tree (TT or FF). It is *not* assignment — disp has
no mutable state.

`test` is an elaborator keyword. `test expr` reduces `expr` (via the
substrate's `apply` and the kernel dispatcher) and asserts the result
equals `TT`. If `expr` reduces to anything else (FF, an Err value, a
stuck form), the elaborator throws an error reporting the failing
expression and its actual reduction.

```disp
test (add 2 3) = 5                // arithmetic check
test typecheck Type Bool = TT      // type validity check
test param_apply Bool TT = Ok TT   // recognition test
```

Common test idioms are sugar:

- `test expr` (no `=`) defaults to `test expr = TT`. Useful when
  `expr` already returns a Bool.
- `test typecheck T v` is the standard "type-check test."

The `: T` annotation is sugar for this last form. `let X : T = body`
desugars to `let X = body; test typecheck T X`.

== Tests run at elaboration time

Disp currently has no notion of compiled output or runtime tests.
Tests run during elaboration (the only time things "run" in the
current spec). A failing test stops elaboration with an error.

Once disp grows machine-code emission, a separate testing-effect
framework can stage test-runs as runtime operations. For now, "test"
is synonymous with "elaboration-time assertion."

== Test catalog

Tests defined alongside library types form the catalog of properties
the standard library is expected to satisfy. The catalog is browsable
in source and runnable as a whole by re-elaborating the library.

```disp
let Bool = wait bool_recognizer bool_meta
test typecheck Type Bool             // Bool is a type
test typecheck StrictType Bool       // Bool passes deep validation too
test bool_recognizer unit TT = Ok TT // recognizer accepts TT
test bool_recognizer unit FF = Ok TT // recognizer accepts FF
test bool_recognizer unit zero = Ok FF  // recognizer rejects non-bool
```

The first two are "type validity" tests; the last three are "behavioral"
tests. Both run identically (just elaboration-time `test` expressions);
they differ only in what they assert about.

See the appendix (§A.1) for the full catalog of tests the standard
library ships with.

== What the elaborator does NOT need

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Not needed*], [*Why*],
    [`infer` function], [Type info is given by annotations, not synthesized],
    [Bidirectional `check`/`infer` ping-pong], [One `test typecheck T body` per annotated binding],
    [Unification], [No inference means no constraint solving],
    [Constraint generation/solving], [Same],
    [Coercion insertion], [Subtyping is not supported],
    [Higher-order pattern unification], [Same as above],
    [A type-checker proper], [`typecheck` is a library validator; the elaborator just emits a test],
  ),
  caption: [Things a standard elaborator does that disp does not.],
)

The elaborator's output is *trees + tests*. The tests are run as
part of elaboration; the trees are the artifact.

== Restrictions on the source language

The wrap-only design constrains what the source language can express:

+ *Top-level `let`-bindings can omit `:` annotations.* Without `: T`,
  no test is emitted; the user can write one explicitly if validation
  is wanted.
+ *Local lambdas in untyped contexts are accepted but unwrapped.* They
  compile to bare functions; no `checked` wrapping fires. No tests
  validate them either; bugs manifest at use.
+ *No Hindley-Milner-style type variables.* Polymorphism is via
  explicit `Pi Type ({A} -> ...)` annotations.

In exchange: the elaborator is small, simple, and trivially audited.
The type system's rigor comes from library validators, not from
elaborator complexity.

== Compared to standard dependent-type elaboration

Lean and Coq use heavy elaboration — type-class search, implicit
argument inference, unification, tactic execution. The de Bruijn
criterion (de Moura et al. 2015) says the *kernel* (the type-checker
proper) is the trusted base; elaboration is auxiliary.

Disp pushes this minimalism further. The elaborator does no type-
checking judgments at all — it just transforms syntax and emits
tests. The "type system" is the set of library validators and the
tests that exercise them. The kernel only provides the dispatcher
and a small set of privileged constructors.

This is structurally analogous to property-based testing frameworks
(QuickCheck, Hypothesis) lifted to the type-system level: the system
is defined by what tests pass, not by what the elaborator decides.

== Concrete sketch

For `let foo : Pi Nat ({_} -> Bool) = {x} -> is_zero x`:

+ Outer expected type: `Pi Nat ({_} -> Bool)`.
+ Extract domain `Nat` for the binder `{x}`.
+ Body `is_zero x` compiles to `apply(is_zero_compiled, x)`.
+ Wrap the binder: `checked (Pi Nat ({_} -> Bool)) ({x} -> apply(is_zero_compiled, x))`.
+ Bracket-abstract `{x}` over the body.
+ Emit: `let foo = wrapped_tree` plus `test typecheck (Pi Nat ({_} -> Bool)) foo = TT`.

The test reduces `typecheck (Pi Nat ({_} -> Bool)) foo` via the
kernel dispatcher. Pi's recognizer fires the outer `checked` handler
against a Nat-hypothesis, runs the body, checks result against `Bool`.
Result `Ok TT` → test passes. `Ok FF` or `Err _` → elaborator throws
with the failure context.

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

= Types and validators <sec:typeformer>

This section presents the type system as a library construction.
Types are wait-forms; validators (including `Type` itself) are
wait-forms whose recognizers judge whether other wait-forms count
as types; validation is a `test` declaration (§9).

== The categorical observation

Every library type plays three categorical roles:

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

== Types are wait-forms

A type is a wait-form `wait recognizer meta`. Applying the type to a
value reduces (via wait's bracket-abstracted shape) to
`recognizer meta value`, which returns a `CheckerResult Bool`. That
reduction IS the type's runtime behavior.

```disp
Bool := wait bool_recognizer bool_meta
Nat  := wait nat_recognizer nat_meta
Pi   := {A, B} -> wait pi_recognizer (pi_meta_for A B)
Type := wait type_recognizer type_self_meta
```

The substrate's `wait` combinator (§5.4) is the universal type
constructor. There is no specialized constructor (no
`make_type_former`, no `predicate_frame_form` wrapper). Types are
just wait-forms; different recognizers and metas give different
types.

Different types have *different signatures* — `pair_fst Bool` and
`pair_fst Nat` are different trees, because their recognizers are
different. There is no uniform "type signature" recognized by the
dispatcher; types are recognized BY `Type` (the structural
validator), not by a kernel-side sig table.

== The metadata convention (MetaShape)

A type's metadata follows a conventional record layout that
downstream library code projects from. The standard fields:

```disp
MetaShape := Refinement Record [
  ("recognizer_params", Tree),        // closed args to the recognizer
  ("functor", Tree),                  // morphism action for transport (§13)
  ("applicable", Optional Applicable), // optional codomain_fn for function-shaped types
  ("behavioral_specs", Optional (List Path))  // optional Path-typed behavioral proofs
]
```

`MetaShape` is the library refinement type capturing this convention.
The structural shape (a Record with these fields) is enforced;
deeper validation of each field's contents is the validator's
responsibility.

The convention is *extensible*. Adding a new conventional field
(e.g., for a new modality or effect system) extends MetaShape's
expected layout. Existing types whose meta lacks the new field still
pass structural checks; library code that needs the new field handles
its absence explicitly.

(Records, Refinement, and the projection mechanism are library
constructions detailed in §12. Their behavior in turn relies on
Sigma's projection codomain_fn, also §12. The dependency cycle
between Type / MetaShape / Pi / Sigma is broken by deferring tests
— see §11.4.)

== Validators as library entities

A *validator* is a type whose recognizer judges whether candidate
trees are types-of-some-kind. The standard library ships three:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Validator*], [*Rigor*], [*What it checks*],
    [`Type`],
      [Structural],
      [`v` is a wait-form whose meta fits MetaShape's layout. H-rule on neutrals.],
    [`StrictType`],
      [Deep],
      [Same as `Type`, plus typechecks the recognizer against `RecognizerShape` and the meta against `MetaShape` field-by-field.],
    [`BehavioralType`],
      [Behavioral],
      [Same as `StrictType`, plus runs each Path-typed `behavioral_specs` entry.],
  ),
  caption: [Standard validators.],
)

Each validator is just a wait-form. Adding a new validator does not
modify the kernel; it defines a new wait-form with a new recognizer.

The relationship between a type and the validators it satisfies is
expressed via tests:

```disp
test typecheck Type Bool             // Bool is structurally a type
test typecheck StrictType Bool       // Bool also passes deep validation
test typecheck BehavioralType Bool   // Bool's recognizer behaves per its specs
```

Users can define their own validators (e.g., a "linear types"
validator that also checks the metadata carries usage annotations).
Tests assert which validators accept which types. The "type system"
is the union of all validators and tests; users compose them as
needed.

== Constructing `Type`

Two pieces, both pure tree construction:

*1. The recognizer.* A library function performing the structural
shape check:

```disp
let type_recognizer = {meta, v} ->
  let self_type = wait type_recognizer meta in
  match (safe_is_neutral v) {
    TT => Ok (tree_eq self_type (neutral_stored_type v))  // H-rule
    FF => bind (safe_is_fork v) ({is_pair} ->
          match is_pair {
            FF => Ok FF
            TT => Ok (has_metashape_layout (safe_pair_snd v))
          })
  }
```

The `safe_*` helpers (defined in §12) route through `eliminator_frame`
so the body works on hypothesis arguments as well as concrete values.
`has_metashape_layout` is a structural check on the record's field
presence.

*2. Type's metadata.* A record literal following the MetaShape
convention:

```disp
let type_self_meta = {
  recognizer_params := unit_witness,     // Type takes no params
  functor := trivial_functor,
  applicable := none,
  behavioral_specs := none
}
```

*3. Type itself.*

```disp
let Type = wait type_recognizer type_self_meta
```

Three definitions. No tests fired at construction; Type is just a
value.

Validation is a separate concern, expressed as tests:

```disp
test typecheck Type Type         // Type is a type (lax)
test typecheck StrictType Type   // Type also passes deep validation
```

The first runs `type_recognizer type_self_meta Type`. Type's structure
(wait-form with MetaShape-conforming metadata) satisfies the
structural check, returns `Ok TT`. The test passes by construction.

== `Type : Type`

The test `test typecheck Type Type` is expected to pass under all
standard validators. Under `Type` (structural), it passes trivially
by construction. Under `StrictType` (deep), it passes via recursive
validation that bottoms out by hash-cons memoization. Under
`BehavioralType`, it passes if Type's optional behavioral_specs (if
any) typecheck.

These tests are *runnable mechanical assertions*. Their passing or
failing is observable.

The deeper question — does `typecheck Type Type = Ok TT` imply that
disp's type system is foundationally consistent (no Girard / Hurkens
encoding of inconsistency)? — remains open. Our conjecture is yes,
on the strength of the walker's parametricity discipline.

The argument (informal):

*1. Polymorphic types like ⊥ := `Pi Type ({A} -> A)` have no
inhabitants by case analysis.* To inhabit ⊥, one needs a `checked`
wait-form whose body, given a fresh Type-hypothesis `A`, produces a
value of type `A`. Pi's recognizer's body-check runs the body under
`bind_hyp`. The body's options:
  - introspect `A` to construct a value — rejected by walker
    TriageReflect (§4.2);
  - return some closed term — rejected by `A`'s recognizer (closed
    terms aren't inhabitants of an unknown type);
  - return `A` via I-shortcut — rejected because `A : Type` is not
    generally in `A`.
None satisfy the codomain check.

*2. The argument lifts to Hurkens.* Hurkens-style normalizing
constructions reduce to the ⊥-inhabitation problem at some
intermediate position; (1) blocks it.

*3. Self-application terminates.* The structural check is
finite-depth.

#openq[
  The argument is informal and depends on three unproven
  properties:

  - *No formal parametricity theorem.* Reynolds-style parametricity
    for disp's walker discipline hasn't been mechanized.
  - *I-shortcut soundness.* The walker's I-shortcut (§4.2) might
    enable more than polymorphic identity. Precise characterization
    is open.
  - *No semantic model.* A logical-relations or PER model of disp
    that interprets `Type` would settle the question.

  Fallback if the conjecture fails: ranked universes (`Type 0 :
  Type 1 : Type 2 : ...`) with cumulative structure. Kernel
  primitives don't change — only Type's metadata carries a level
  index.

  Treating Type:Type as "informally sound until proven otherwise"
  is fine for prototype work but should not be a long-term
  position. Either mechanize the conjecture or commit to ranked
  universes before the system claims foundational status.
]

== Validation levels via validators

Where the previous spec listed five fixed "validation layers," this
spec exposes the spectrum as a *choice of validator*. Each validator
provides a specific rigor level:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Validator*], [*What it catches*],
    [`Type`],
      [Wrong wait-form shape, missing meta fields. Cheap.],
    [`StrictType`],
      [Above plus: recognizer not Pi-typed, meta fields wrong-typed.],
    [`BehavioralType`],
      [Above plus: documented Path-typed properties of the type's recognizer.],
  ),
  caption: [Validators and what they catch.],
)

Users opt into the rigor they want via the tests they write. Multiple
validators can be applied to the same type; they're not mutually
exclusive. A "fully validated" type passes tests against all three.

Test-based behavioral specs are the closest disp gets to *full semantic
verification* (which remains undecidable in general). For specific
finite properties, behavioral_specs Paths give mechanical proof.

= Library types <sec:library-types>

Each library type-former, under the categorical foundations of §11.
The pattern: write a `TypeFormer` record literal; wrap any function-
typed metadata fields with `checked`; wrap the record with
`predicate_frame_form` to produce the type.

== `Bool`

```disp
let bool_recognizer = {_, v} ->
  Ok (or (tree_eq v TT) (tree_eq v FF))

Bool := predicate_frame_form {
  classifier := { params := Unit, characteristic_morphism := bool_recognizer },
  applicable := none,
  functor    := trivial_functor
}
```

Recognizer is a closed parametric function. The structural check is
pure, so the body wraps in `Ok` directly. Discrete: transport is
identity, both laws are `refl` (bundled in `trivial_functor`).

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

Nat := predicate_frame_form {
  classifier := { params := Unit, characteristic_morphism := nat_recognizer },
  applicable := none,
  functor    := trivial_functor          // discrete: transport identity
}
```

== `Pi`

The pivotal one — Pi's recognizer requires its candidate be a
`checked` wait-form. Raw function values do not inhabit Pi types;
they must be wrapped first (via `typed_lambda` or `checked` directly).

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
let pi_functor          = ...  // non-trivial: supports transp via §13

Pi := predicate_frame_form {
  classifier := {
    params                  := Sigma Type ({A} -> A -> Type),   // (A, B)
    characteristic_morphism := pi_recognizer
  },
  applicable := some { eval_morphism := pi_eval_signature },
  functor    := pi_functor
}
```

The three-step check (signature, domain match, bind-hyp+body) is what
makes Pi soundness-preserving. Raw function values fail at step 1;
domain-mismatched `checked` values fail at step 2; body-type mismatches
fail at step 3 with a TypeMismatch or Escape error from the kernel
operations the body invokes.

== `Sigma`, `Eq`, `Ord`, `Refinement`, `Record`, `Unit`, `String`

These follow the same pattern. Each defines a recognizer, then writes
a `TypeFormer` record literal wrapped by `predicate_frame_form`. The
recognizer bodies are structural shape-checks on canonical inhabitants;
specifics live alongside each type in `lib/types/`.

(Full definitions deferred to implementation; the framework is
established.)

== `Type` itself

`Type` is the wait-form constructed in §11.4:

```disp
Type := predicate_frame_form type_metadata
```

where `type_metadata` is the TypeFormer record literal whose
`classifier.characteristic_morphism` is `typeformer_recognizer`.
`typecheck Type Type = Ok TT` because Type's tree shape satisfies
`typeformer_recognizer`'s structural pattern. The Type:Type concern
and its (conjectural) resolution are discussed in §11.5.

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

I := predicate_frame_form {
  classifier := { params := Unit, characteristic_morphism := I_recognizer },
  applicable := none,
  functor    := trivial_functor       // I doesn't transport
}
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
of each type-former's record literal. Sketches of the per-type clauses
(each becomes the `morphism_action` inside the `functor` field):

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

Each is dropped into the corresponding type's `functor.morphism_action`
field. The pattern: recurse component-wise where possible; stuck-mint
via `StuckElim` for non-structural cases. The full per-type rules
follow the standard CCHM treatment (Cohen-Coquand-Huber-Mörtberg
2015); only the dispatch mechanism (signature on the TypeFormer's
functor field) is disp-specific.

== `Partial` and cofibrations

`IsOne phi : Type` is the proposition "phi reduces to I_one." `Partial
phi A := IsOne phi -> A`. Walker-safe smart constructors for face
systems.

```disp
IsOne := predicate_frame_form {
  classifier := { params := I, characteristic_morphism := isone_recognizer },
  applicable := none,
  functor    := trivial_functor
}

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
Glue := predicate_frame_form {
  classifier := {
    params                  := glue_params_shape,   // (B, T, e)
    characteristic_morphism := glue_recognizer
  },
  applicable := none,
  functor    := glue_functor       // applies the equivalence on transport
}

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
+ *No kernel growth.* The five primitives remain five.

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
- `Type`'s own metadata record (§11.4) — a concrete tree that must
  satisfy `typeformer_recognizer`'s structural pattern.

What is *not* in the trusted base:
- The elaborator (it just emits wrapped trees; the kernel re-validates).
- Library type-former definitions (validated against `Type`'s recognizer).
- User code (validated at every application via `checked`).

== TCB shape

The wrap-only elaboration discipline keeps the trusted computing base
small. The elaborator emits wait-form trees but does not itself decide
type-checking outcomes; the kernel re-validates each binding via
`typecheck` as a one-shot reduction call. The TCB is therefore exactly
"the six kernel handlers + the walker + Type's metadata record."

Designs with bidirectional infer/check elaboration must additionally
trust the elaborator, since elaboration-time type decisions are not
re-validated by the kernel. Disp's wrap-only design avoids that
extension.

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
disp source; the host implements optimizations but not semantics.
`Type` is constructed directly from a TypeFormer record literal whose
recognizer happens to accept Type's own tree shape — self-consistency
by construction, not a special trust seed.

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
