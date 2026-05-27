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
  *Status (2026-05-25).* Active spec. Replaces the prior `TYPE_THEORY.typ`
  (seven-primitive kernel design) and consolidates
  `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ` and `CUBICAL_PROPOSAL.typ`
  into a single document.

  Major shifts from predecessors:
  - The kernel ships *4 Σ-operations + 1 parameterized dispatcher*.
    Σ-ops: `hyp_reduce`, `bind_hyp`, `eliminator_frame`, `postulate`.
    Dispatcher: `safe_apply Σ` — parameterized over a *dispatch
    environment* Σ (a list of handler trees). `param_apply := safe_apply
    default_dispatch`.
  - Kernel and host primitives are *values, not registrations*.
    `default_dispatch = kernel_handlers ++ host_provided`; both are
    ordinary top-level bindings. The host exposes `host_provided : Σ`
    as a value at module-load time; the disp side concatenates
    explicitly. No mutable global registry anywhere in the disp
    semantics.
  - Effects are unified with type-system primitives. Neutrals, stuck
    eliminations, and host IO are all dispatched through the same
    `safe_apply Σ` mechanism — they differ only in which handler is
    looked up.
  - Type-checking is framed as manifest contracts over the `CheckerResult`
    monad. The elaborator is purely a wrap-only pass; no bidirectional
    inference.
  - Library types carry MetaShape-conforming meta records with named
    fields (`recognizer_params`, `functor`, `applicable`,
    `behavioral_specs`), not positional metadata tuples.
  - Cubical operations (`transp`, `hcomp`, `comp`, `Glue`) live in the
    `functor` meta-field of each library type.
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
*wait-form* whose recognizer judges inhabitants. The elaborator's only
job is to transform syntax into trees and emit tests — no bidirectional
inference, no judgments. Type validation is a `test` declaration that
runs a library validator at elaboration time. Failures throw with the
failing component identified. After elaboration succeeds, a *strip pass*
elides validated contracts to give a runtime tree with no per-call
checking overhead.

The kernel is a *tree-calculus interpreter parameterized over a dispatch
environment* Σ — a list value mapping handler signatures to handler
trees. Four kernel Σ-operations ship in `kernel_handlers`: `hyp_reduce`,
`bind_hyp`, `eliminator_frame`, and `postulate`. The host exposes any
real-world primitives (IO, syscalls) as `host_provided : Σ`. The
default environment `default_dispatch := kernel_handlers ++
host_provided` is what `param_apply` evaluates against; stricter callers
(foundational tests, sandboxes) construct narrower environments. The
dispatcher routes by structural signature on hash-consed trees, so
dispatch is O(1) via tree-id comparison. Types, validators, recognizers,
and the MetaShape convention all live in the library — not the kernel.
Cubical operations sit naturally in each type's `functor` metadata
field. The metacircular discipline holds: the type system is defined in
disp source as library code; the host (TypeScript runtime in
`src/tree.ts`) only optimizes.

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
    [§2 Substrate], [Tree calculus, apply, hash-cons identity, glossary of ambient types],
    [§3 The `CheckerResult` monad], [`Result E A`, `CheckerError` variants, Kleisli composition, the *verdict-vs-error principle*],
    [§4 The parametric walker and `Tree_p`], [Walker as Kleisli-lifted binary apply, `Tree_p` as greatest fixed point, soundness discipline],
    [§5 The dispatcher and dispatch environments], [Σ-algebra framing: handlers as values, environments as list-passing, openness via concatenation],
    [§6 Stuck forms and neutrals], [Stuck forms from any pinned handler, the generalized H-rule, cascading-failure story],
    [§7 The kernel primitives], [Operational semantics of `hyp_reduce`, `bind_hyp`, `eliminator_frame`, `postulate`, `safe_apply`],
    [§8 Boundary operations and checked values], [`param_lift`, `typecheck`, `checked`, `typed_lambda`, `validate`],
    [§9 Elaboration and tests], [Syntactic transformation; tests as first-class; `: T` as test sugar],
    [§10 Strip and erasure], [`strip` as a tree function; PCC story],
    [§11 Types and validators], [Types-as-wait-forms; MetaShape; validators-as-values],
    [§12 Library types], [Each library type under the framework, including `Type` itself],
    [§13 Cubical extensions], [`I`, `Path`, `comp`, `Glue`, `ua`],
    [§14 Soundness via tests], [Four categories of runnable assertions; foundational conjecture stays open; environment probes via effectful tests],
    [§15 Effects], [Effects as dispatch-environment entries; effect interfaces as typed records; capability passing],
    [§16 Disp-specific], [What disp contributes beyond standard machinery],
    [§17 Related work], [Literature context],
    [Appendix A], [Open questions and conjectures],
    [Appendix B], [Where the tests live (pointer to inline tests)],
    [References], [Citations],
  ),
  caption: [Section map.],
)

Read §1–§7 in order for the framework. §8–§12 build the type system on
top. §13 covers cubical. §14 is the formal payoff. §15 covers effects
(host primitives, postulates, capability passing). §16 highlights
what's genuinely novel; §17 places the design in the literature.

== Prerequisites

Familiarity with dependent type theory and basic category theory helps
but is not required. The algebraic-effects framing in §5 is the most
mathematically dense; readers can skim it and rely on the operational
semantics in §7 if preferred.

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
  composition, no associative ∘). Each library type (§11) constructs its
  own functorial/morphism structure on its typed subset via its
  metadata's `functor` field.
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

== Ambient types and notation <sec:glossary>

The spec uses several ambient names that are conventional library
constructions over the substrate, not new primitives. Listed here once
to avoid re-introducing them inline:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Name*], [*Meaning*],
    [`Tree`],         [Any tree in the substrate (§2.1).],
    [`Tree_p`],       [Trees on which the parametric walker is closed under `Ok` (§4).],
    [`Bool`],         [`TT` / `FF` Scott encoding (`lib/prelude.disp`); see §12.],
    [`List X`],       [Standard cons/nil list of `X`-trees; iterated `pair`s.],
    [`Optional X`],   [`Some x` / `None`; sentinel-tagged.],
    [`Span`],         [Source-span record (file, start, end) attached to error variants for diagnostics; opaque tree at the substrate level.],
    [`Symbol`],       [A fixed tree value identifying a handler / constant — distinct from any user-constructed tree.],
    [`Functor`],      [Synonym for `Tree_p`; conventionally a morphism-action function consumed by `transp` (§13). Sentinel `trivial_functor` = "identity / no transport rule."],
    [`Applicable`],   [`Type -> Tree_p -> Action`. The codomain_fn role: given a stored type and an applied argument, decide what `hyp_reduce` does next.],
    [`Action`],       [`Extend Type | Return Tree_p | Invalid`. The protocol `hyp_reduce` consumes from a type's codomain_fn (§7).],
    [`Path`],         [`Pi I` alias from §13; appears in `behavioral_specs`.],
  ),
  caption: [Glossary of ambient names used in signatures.],
)

#note[
  *Layering convention.* Wherever a signature in this spec uses a
  field-typed record (e.g.
  `{stored_type : Type, spine : List Tree_p}`), the typed names refer
  to *post-§11 ascriptions* — what the library type checker assigns
  to the field once the relevant types are in scope. The same field
  at the *bootstrap layer* is a tree subterm in conventional pair-
  encoded position: `pair stored_type (pair spine ...)`. Records are
  read positionally before library types exist and ascribed by name
  once they do. We use the typed presentation throughout for
  readability; the implementation walks the pair structure either
  way.
]

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
       (currently silent in some paths — see §15 for the planned
       diagnostic story)],
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

== Why a monad

Failures propagate uniformly via `bind`. No manual short-circuiting
in handler code; Kleisli composition does it automatically. Tagged
errors plus `catch` mean each layer decides what to recover from and
what to re-raise, instead of pattern-matching on opaque `Fail`
trees. The general/specialized split keeps the kernel's failure
vocabulary self-documenting: `grep catch` finds every error
suppression; `grep 'Err ('` finds every error raise.

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

We elevate this split to a *named design principle*, referenced
elsewhere in the spec:

#note[
  *The verdict-vs-error principle.* `Ok TT` / `Ok FF` are *verdicts*
  — legitimate data answers to "is `v` an inhabitant of `T`?". `Err _`
  is reserved for *soundness-level breakage* — parametricity violation,
  escape, malformed meta, broken-contract `TypeMismatch`. Recognizer
  bodies must never fold the latter into the former. Contract
  boundaries (and only contract boundaries) lift verdict-`FF` into
  `Err TypeMismatch` because at those sites the call was promised
  to fit.
]

#note[
  *Same monad for effectful computations.* `CheckerResult` is also the
  return type of every handler in a dispatch environment (§5, §7),
  including host primitives. There is no separate `IO` monad at the
  kernel level — effectful computations and type-checking computations
  share the same failure vocabulary, and `bind` / `catch` work the
  same way for both. Library types like `IO X` are constructed atop
  this (or independently of it) as MetaShape-conforming wait-forms;
  they don't change the kernel's monad.
]

= The parametric walker and `Tree_p` <sec:tree-p>

== Motivating problem

Disp's type system relies on *hypotheses* — fresh tree values minted by
the kernel that represent "an unknown value of type `A`." Hypotheses
have a pinned signature (`pair_fst h = checker_sig hyp_reduce` for a
kernel-minted neutral `h`). More generally, every handler in the
dispatch environment Σ (§5, §7) has a sig that user code must not be
able to forge — forging would let user-side trees masquerade as
privileged operations (kernel-minted neutrals, host IO calls, etc.),
breaking the soundness of dispatch.

The fix: define the *parametric walker* — a Kleisli-lifted version
of `apply`, parameterized over Σ, that performs the same reduction
but rejects two introspection patterns. `Tree_p(Σ)` is then the
largest subset of trees on which the walker, applied to pairs from
`Tree_p(Σ) × Tree_p(Σ)`, never trips a rejection.

The walker, the dispatcher, and the stem-forge check all consult the
*same* Σ — they are aspects of one mechanism, not independent layers.

== The walker as a Kleisli-lifted binary operation

The walker is the Kleisli lift of binary `apply`, *parameterized over Σ*:

$ w_Sigma : "Tree" times "Tree" -> "CheckerResult"("Tree") $

— a Kleisli arrow in `Kl(CheckerResult)` with the same arity as the
substrate operation it lifts, indexed by the dispatch environment in
effect. Its three clauses follow `apply`'s rules, with two
`Err Parametricity` cases and one carve-out:

+ *Stem-rule rejection (parameterized).* When `w_Σ(stem(a), x)` would
  reduce to `fork(a, x)`: if there exists a handler `h ∈ Σ` such that
  `tree_eq (checker_sig h) a`, return
  `Err (Parametricity { kind = StemForge, where = fork(a, x), span })`.
  This prevents forgery of any privileged tree shape — kernel-minted
  neutrals, stuck eliminations, host-effect wait-forms, or anything
  else the current Σ pins.

+ *Triage-rule rejection.* When `w_Σ(f, x)` would fire the triage
  rule on `x` (because `f` is a fork-fork-fork shape), first check
  whether `x` is a kernel-minted neutral
  (`pair_fst(x) = checker_sig hyp_reduce`). If so, return
  `Err (Parametricity { kind = TriageReflect, where = x, span })`.
  This prevents reflection on hypotheses via triage. The check uses
  the kernel-internal `hyp_reduce` sig directly because only
  `hyp_reduce`-rooted trees are introspection targets (other pinned
  sigs name handlers, not introspectable values).

+ *I-shortcut (carve-out).* `w_Σ(I_canonical, x) = Ok x`
  unconditionally, even when `x` is a hypothesis. Required so the
  polymorphic identity function passes Pi-checks against hypothesis-
  typed arguments. It is the only soundness carve-out. Σ-independent.

All other applications follow `apply`'s rules and return `Ok <result>`.

#note[
  *Why parameterize over Σ?* Different callers want different trust
  boundaries. The default environment pins kernel handlers plus host
  primitives, forbidding their forgery. A foundational test running
  under `kernel_handlers` alone permits constructing tree shapes that
  the host would otherwise pin (because the host's primitives aren't
  in scope). A sandboxed elaborator running untrusted code might use
  a *larger* Σ that pins user-supplied mock handlers as well. The
  walker's behavior tracks the environment in which evaluation
  happens.
]

== Why only `I` needs a carve-out

Every substrate reduction rule either inspects its argument
(triage — rejected when the argument is a hypothesis), wraps it
(leaf/stem — risks forging a kernel sig at the resulting fork's
`pair_fst`), or discards it (the K rule). None of these returns the
argument unchanged. Yet the polymorphic identity `{x} -> x` must exist
as a typeable closed term — it's what makes
`Pi Type ({A} -> Pi A ({_} -> A))` inhabitable. The elaborator
compiles `{x} -> x` to a canonical tree `I` (typically `S K K` after
bracket abstraction); without intervention, `apply(I, hyp)` would
route through stem/S/K rules, hit the stem-forge rejection on an
intermediate fork, and reject the application. The I-shortcut
intercepts: `w(I, x) = Ok x` directly, by *hash-cons-identity check*
on `I`. It activates *only* for that canonical tree — η-equivalent
identities (e.g., `triage t t t` applied to canonical inputs)
don't trigger it and don't pass hypotheses through. The carve-out
is structural ("this exact tree, by pointer-identity") rather than
behavioral ("any function that happens to be the identity"), so the
soundness obligation reduces to: the elaborator must produce
canonical `I` for `{x} -> x` and only for that. The literature
framing — `I` as the identity element of the application monoid, or
equivalently the trivial polymorphic transformation — places this in
standard Yoneda/Church territory; we don't lean on that framing
operationally, but it explains why no other carve-out is needed: no
other tree represents a parametric operation that introspects
nothing.

== `Tree_p(Σ)` as a greatest fixed point

`Tree_p(Σ)` is defined as the *largest* subset `S ⊆ Tree` such that
the walker (in environment Σ) restricted to `S × S` is closed under
`Ok` — i.e., the operation `S × S → CheckerResult(S)` never produces
an `Err Parametricity`:

$ "Tree"_p (Sigma) = "greatest" S subset.eq "Tree" "such that" forall f\,x in S, w_Sigma (f, x) in {"Ok"(r) : r in S} $

(modulo divergence — non-terminating reductions don't violate
membership). The walker is the only source of `Err Parametricity`,
and parametricity is the only kind of error it produces; other
`CheckerError` variants are raised by handlers further out.

*Monotonicity in Σ.* If `Σ ⊆ Σ'`, then `Tree_p(Σ') ⊆ Tree_p(Σ)`: a
larger environment is *more* restrictive (more sigs to forbid forging),
so fewer trees survive. Programs walker-safe under a larger Σ are
walker-safe under any smaller Σ, but not conversely. This is the
soundness story for environment substitution.

#note[
  Tree_p(Σ) is a property of the walker, not of `CheckerResult`. The
  monad supplies the failure container; the *content* of "what counts
  as parametric" lives in the walker's rejection clauses. The
  definition is *semantic* and undecidable in general — you cannot
  tell by inspection alone whether an arbitrary tree is in `Tree_p`.
  §4.5 gives a syntactic discipline that approximates membership
  conservatively.

  Throughout the rest of this spec we write `Tree_p` to mean
  `Tree_p(default_dispatch)` unless an alternative environment is
  specified. The §11 type system, in particular, is defined relative
  to the default environment.
]

== The walker restricted to `Tree_p(Σ)`

Once `Tree_p(Σ)` is in hand, the walker restricts to a closed binary
operation on it:

$ w_Sigma : "Tree"_p (Sigma) times "Tree"_p (Sigma) -> "CheckerResult"("Tree"_p (Sigma)) $

That is the type the kernel actually relies on — every handler in §5,
in environment Σ, consumes and produces values in `Tree_p(Σ)`.

== Soundness rules for users

To keep user-written trees in `Tree_p(Σ)`, follow six rules. These
form a *decidable static discipline* that conservatively approximates
membership in the greatest fixed point above:

+ *Don't triage on hypothesis-typed values.* If `x` might be a
  hypothesis (e.g., bound by `bind_hyp`), don't write
  `triage l s f x`. Use kernel-mediated checks (`is_neutral`,
  `tree_eq` against a known closed value, `has_sig` against a
  registered signature) instead.

+ *Don't construct forks rooted at any handler sig in the current Σ.*
  The stem rule rejects this. Use kernel constructors (`bind_hyp`,
  `eliminator_frame`, `postulate`) for the privileged tree shapes that
  Σ pins.

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

+ *Stricter contexts use smaller Σ.* Programs that want to operate
  outside the host's effect surface should run under `safe_apply
  kernel_handlers`. The walker becomes more permissive (fewer
  pinned sigs to forbid), but dispatch covers only the kernel
  handlers — host primitives that would have been intercepted are
  now reduced as ordinary trees and will fail when their handler is
  absent.

`Tree_p(Σ)` is the largest carrier in `Kl(CheckerResult)` on which the
walker — the Kleisli lift of the substrate's apply operation —
restricts to a closed binary operation. Composition in
`Kl(CheckerResult)` is the standard `g ∘_K f = μ ∘ T(g) ∘ f` of
§3.5; the walker is the operation those Kleisli arrows compose with
when reducing `apply` chains.

= The dispatcher and dispatch environments <sec:dispatcher>

== Setup

In operational terms: the kernel takes a list of *handler trees*
(call it Σ), takes two trees (`f` and `x`), and reduces `f x`. If
`pair_fst f` matches the sig of some handler in Σ, the handler runs;
otherwise the substrate's `apply` rules fire, subject to the
parametricity walker. That's it.

The §5 material below makes this precise in algebraic-effects
vocabulary (Σ as a signature, handlers as a Σ-algebra, the dispatcher
as the algebra interpreter). The framing is useful for connecting to
the PL literature but is not needed to use the system. Readers
preferring operational explanations can skip to §7.

== The signature and the dispatcher

The kernel surface has two distinct roles. The *dispatch environment*
Σ is a list of handler trees whose signatures the dispatcher is
trusted to invoke; the *dispatcher* `safe_apply Σ` is the interpreter —
it routes incoming applications either to a handler in Σ or to the
parametric walker.

```
Σ : List Tree   -- the dispatch environment
dispatcher = safe_apply : List Tree → Tree → Tree → CheckerResult Tree
param_apply := safe_apply default_dispatch    -- the default-instance name
```

Each handler in Σ is a *curried function* taking its structured meta
record first, then an argument from `Tree_p(Σ)`, and producing a
result in the `CheckerResult` monad. The meta record is what gets
baked into a wait-form (`wait handler meta`, see §5.4); the second
application supplies the argument and triggers dispatch via
`safe_apply`.

The four kernel-shipped Σ-operations are:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Symbol*], [*Type*],
    [`hyp_reduce` (Σ-op)],
      [`{stored_type : Type, spine : List Tree_p} -> Tree_p -> CheckerResult(Tree_p)`],
    [`bind_hyp` (Σ-op)],
      [`{domain : Type, body : domain -> R} -> Tree_p -> CheckerResult(Tree_p)`],
    [`eliminator_frame` (Σ-op)],
      [`{dispatcher : Motive -> Cases -> A -> R, motive : Motive, cases : Cases} -> Tree_p -> CheckerResult(Tree_p)`],
    [`postulate` (Σ-op)],
      [`{sig : Tree} -> Tree_p -> CheckerResult(Tree_p)` (mints a wait-form rooted at any handler sig in the current Σ; the bridge from user code to non-self-dispatching handlers)],
    [`safe_apply` (dispatcher)],
      [`List Tree -> Tree_p -> Tree_p -> CheckerResult(Tree_p)` (Σ-parameterized substrate-apply with a privilege check)],
  ),
  caption: [The kernel surface: four Σ-operations plus the
    parameterized dispatcher. Type variables: `A` = type, `R` =
    result type, `Motive` / `Cases` = dispatcher-specific.],
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

== Dispatch environments

A *dispatch environment* Σ is a list of handler trees. Each handler is
a closed disp tree implementing a Kleisli arrow of the appropriate
arity; its signature is derived from the wait-encoding as
`checker_sig h := pair_fst (wait h t)` (the same library helper used
throughout §5.4 and §12). The dispatcher's privilege check asks "is
`pair_fst f` equal to `checker_sig h` for some `h ∈ Σ`?", which reduces
to scanning Σ with O(1) per-element comparisons (hash-cons identity,
§2.2). For typical Σ-sizes (≤ 30) the linear scan is fine; the host
fast-path may use a hash table indexed by sig.

The kernel ships its four Σ-operation handlers as a top-level binding:

```disp
let kernel_handlers : List Tree := [
  q_hyp_reduce_fn ;
  q_bind_hyp_fn ;
  q_eliminator_frame_fn ;
  q_postulate_fn
]
```

The host exposes its primitives (IO, syscalls, …) as a value at
module-load time:

```disp
let host_provided : List Tree := /* exposed by the host runtime */
```

The default dispatch environment used by `param_apply`, by every test
declaration, and by `typecheck` is their concatenation:

```disp
let default_dispatch : List Tree := concat kernel_handlers host_provided
let param_apply := safe_apply default_dispatch
```

There is no mutable registry. Every (P, H) — every dispatch environment —
is an explicit value. Stricter callers construct alternative
environments by restricting or extending this list:

```disp
let test_env : List Tree := concat kernel_handlers [my_mock_console]
let test_apply := safe_apply test_env
```

Each `q_*_fn` is a Kleisli arrow implementing its operation's semantics
(full definitions in §7).

#note[
  *Sigs are derivable from handlers.* A handler is a tree; its sig is
  not separately stored — `checker_sig h` is computed from `h` via the
  wait-encoding when the dispatcher needs to compare. So the entries
  in Σ are just handler trees; there is no parallel "sig list" to
  keep in sync. The wait-form encoding's signature-stability property
  (§5.4) makes the derived sig hash-cons-stable.
]

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
type's MetaShape-conforming meta record from a wait-form's payload
before applying it to anything.

With these two properties in place, the dispatcher is a single
privilege check parameterized by Σ:

```
safe_apply Σ f x:
  if ∃ h ∈ Σ. tree_eq (pair_fst f) (checker_sig h)  → run raw (just `f x`)
  else                                               → walker step (§4)
```

*The dispatcher does not route to handlers via a separate table.* Once
privilege is granted, the wait-form's bracket-abstracted reduction
(`wait k m x` → `k m x`) invokes the embedded handler automatically
— no mapping required. The dispatcher's only choice is between two
execution modes: raw apply for trusted reductions, walker apply for
everything else. The Σ argument carries the *trust set* (which sigs
are privileged); the wait-form encoding carries *which specific
handler each privileged sig dispatches to* (because the handler is
the `k` inside `wait k m`). See §7's `safe_apply` entry for the
in-language reference.

=== Why wait-forms, not plain partial application

Semantically, meta is just an additional argument to the handler —
`wait kernel.op meta arg` is `handler(meta, arg)` with extra
ceremony. The wait-form encoding buys three things that direct
partial application does not:

+ *O(1) privilege check.* The signature constant
  `checker_sig(k) = pair_fst(wait k t)` is hash-cons-stable: every
  wait-form sharing the same operation produces the same pair_fst
  regardless of metadata. The dispatcher decides "is this a trusted
  kernel op?" via a single id-comparison membership check (§7,
  `param_apply`).
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
    [Handler set], [Σ-parameterized; default carries kernel + host], [Open (user-defined)],
    [Dispatch], [Structural signature on wait-forms], [Lexical handler scope],
    [Algebraicity], [Operations not algebraic in general], [Operations algebraic (`bind (op c_i) k = op (bind c_i k)`)],
    [Multi-shot continuations], [No (tree calculus is pure); explicit CPS in interface for nondet/backtracking], [Yes],
    [Effect rows], [Capability passing via dependent records (§15)], [Explicit row polymorphism],
  ),
  caption: [Disp vs Plotkin-Pretnar.],
)

Disp inherits the *vocabulary* (monad, Kleisli, operations, handlers)
and now also part of the *power* (handler-set extension via
Σ-parameterization). It still differs from Plotkin-Pretnar in two ways:
(a) the substrate is pure, so multi-shot continuations require explicit
CPS in the effect interface rather than implicit capture; (b) effect
typing is capability passing (Effekt-style, §15), not row polymorphism
(Koka-style). We use "algebraic effect" as informal shorthand because
the structural similarities to Koka, Effekt, and Eff are real and the
framing is illuminating.

#note[
  *Position vs. predecessors.* Disp's original framing called the
  Σ-algebra "closed" (one fixed kernel handler). This spec opens it
  along the Σ-parameter axis: the kernel still ships a default
  Σ-algebra, but every caller can substitute its own. This is a
  cleaner factoring than Koka/Eff's *scoped handler installation*:
  there is no implicit dynamic handler stack, only explicit
  environment substitution.
]

== What "effect" means here

A *handler* in Σ is anything the dispatcher routes to: a tree whose
`apply` semantics carry observable consequences. The kernel-shipped
handlers in `kernel_handlers` provide *type-system effects*:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Handler*], [*Effect on the type-checking environment*],
    [`bind_hyp`], [Mint a fresh symbol for an unknown of given type.],
    [`hyp_reduce`], [Apply a symbol to an argument; extend its observed spine.],
    [`eliminator_frame`], [Case-dispatch a value; if symbolic, defer.],
    [`postulate`], [Bridge from user code to a non-self-dispatching handler.],
  ),
  caption: [Kernel handlers as type-system effects.],
)

The host-provided handlers in `host_provided` provide *real-world
effects* — IO, syscalls, time-of-day, anything the host TS layer
implements. Both categories populate the *same* Σ; both go through the
*same* `safe_apply` dispatcher; both are subject to the *same* walker
discipline (no forging, no triage on resulting stuck forms). The
kernel/host distinction is an implementation detail, not a semantic
boundary.

Operationally, the most consequential difference between the two is
*which slot of the wait-form holds what*. For kernel Σ-ops the
handler logic is bracket-abstracted into the wait-form's body and runs
via substrate reduction. For host primitives the handler is a TS
function in the native fast-path table, and the wait-form's body is
inert (waiting to be intercepted). To a disp-source reader these look
identical: both are wait-forms in Σ whose application has a defined
semantics.

This unification — *"effects" mean "Σ entries"* — is what lets the
parametricity story scale uniformly: any pinned sig is unforgeable by
the walker, regardless of whether its handler manipulates type-system
state or filesystem state. The §4 stem-forge rule and the §6 H-rule
both consult Σ; both apply equally to neutrals and to IO calls.

= Stuck forms and neutrals <sec:stuck-forms>

Before defining each kernel primitive's operational semantics, we
introduce the *stuck forms* the kernel exists to construct and
manipulate. The Σ-operations of §5 each play one role in this story —
they're the only constructors of stuck forms, the only consumers of
them, and the only sources of the H-rule short-circuit they need to
travel safely through library code.

== What are stuck forms?

A *stuck form* is a tree of a specific kernel-pinned shape
representing "a computation whose value is unknown until later." Disp
has three kinds, each from a different Σ-operation:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Kind*], [*Constructor*], [*Represents*],
    [Hypothesis],
      [`bind_hyp` (mints fresh)],
      ["An unknown value of type T."],
    [Spine-extended neutral],
      [`hyp_reduce` (extends on application)],
      ["An unknown value, after being applied to known args."],
    [Stuck elimination],
      [`eliminator_frame` (mints on neutral target)],
      ["Eliminating an unknown value with known cases."],
  ),
  caption: [The three kinds of stuck forms.],
)

All three are *handler-rooted*: their `pair_fst` is the sig of some
handler in Σ (`hyp_reduce`'s for hypotheses and spine extensions;
`eliminator_frame`'s for stuck eliminations). The walker (§4) rejects
user-side construction of such trees via its stem-forge rule. Only
the kernel can mint them, via the three privileged Σ-operations.

#note[
  *Stuck forms generalize to any handler.* Today only the three
  kernel Σ-ops listed above produce stuck forms — host primitives
  in the default environment either reduce on concrete input or fail
  on neutral input. In principle a custom handler can symbolic-defer
  on neutral input and contribute a fourth stuck-form kind; the H-rule
  generalization in §11 handles such forms uniformly via the
  `safe_is_stuck` helper. The three-kind taxonomy above is descriptive
  of the default environment, not exhaustive of what Σ can contain.
]

== How stuck forms propagate

Stuck forms thread through computation; subsequent operations on
them extend the stuck-ness rather than reducing to concrete values.

*Application to a stuck form*: `apply(hyp_X, v)` routes through
`hyp_reduce`. The handler consults `X`'s stored type's `codomain_fn`
to decide what type the result should have, then extends the spine
with the new argument. The result is a *bigger* stuck form
representing "hyp_X applied to v."

For Pi-typed hyp, codomain_fn returns `Extend B(v)`: result is a
neutral of type B(v). For Type-typed hyp, codomain_fn fires the
*predicate-side H-rule*: it returns `Return TT` iff `v` is itself a
kernel-minted neutral whose stored type hash-cons-equals the
applied hypothesis, and `Return FF` otherwise — a concrete Bool
verdict, decided structurally by `tree_eq` (see §12.10 for the
`type_predicate_h_rule` definition; the rationale for this being
predicate-side rather than `Extend`-based is in §6.4). For
non-applicable types (Bool, Nat), codomain_fn returns `Invalid` and
hyp_reduce produces an `invalid_result` form.

*Elimination of a stuck form*: `eliminator_frame_form { dispatcher,
motive, cases } target` checks if `target` is a neutral. If yes,
mints a `StuckElim` form representing "the dispatch that would have
happened if target were concrete." If no, runs the dispatcher
normally.

*Triage on a stuck form*: walker-rejected (TriageReflect, §4.2).
User code that tries to inspect a stuck form's structure via raw
triage will be caught by the walker. Library helpers (`safe_*`)
that route through `eliminator_frame` give well-defined
"stuck Bool" / "stuck Tree" / etc. results instead.

*Hash-cons equality with stuck forms*: works (`tree_eq` is just
pointer comparison). Two stuck forms constructed via the same
operation with the same metadata hash-cons to the same tree id, so
`tree_eq` returns TT for them. Different operations or different
metadata → different tree ids → FF.

== The H-rule and stuck forms

When a type-recognizer is applied to a hypothesis — or, more generally,
to any handler-minted stuck form whose stored type is recoverable —
naive recognition fails: the stuck form isn't structurally a canonical
inhabitant of the type. Without intervention, recognizers reject
their own hypothesis values, polymorphic Pi-checks cascade-fail, and
effect-using functions with intermediate stuck values fail too.

The *H-rule* solves this. When applying `T v` and `v` is a handler-
minted stuck form whose stored type equals `T`, return `Ok TT`
directly (without running T's structural check).

The H-rule has *two operational sides* corresponding to two distinct
application paths through `param_apply`. They sound similar but route
through entirely different machinery.

=== Recognizer-side H-rule

*Scenario.* `param_apply T v` where `T` is a *library recognizer* —
a `make_recognizer`-wrapped wait-form, e.g. `Bool`, `Nat`, `Pi A B`.

*Path.* The call routes through `param_apply`'s walker arm; the
wait-form reduces to `recognizer_wrap_fn body meta v`.

*Mechanism.* The wrapper checks `safe_is_stuck v`. If true, it
short-circuits to `Ok (tree_eq self_type (stuck_stored_type v))` —
a concrete `Ok TT` or `Ok FF`, computed by hash-cons identity. If
false, the per-type recognizer body runs on the concrete `v`.

*Scope.* Uniform — every `make_recognizer`-wrapped recognizer gets
this for free. Per-type bodies see only concrete `v` values; the
wrapper handles all stuck cases.

*Source.* §12, `make_recognizer`.

=== Predicate-side H-rule

*Scenario.* `param_apply hyp_T v` where `hyp_T` is itself a
*Type-typed hypothesis* being applied as a predicate — not a library
recognizer.

*Path.* `pair_fst hyp_T = checker_sig hyp_reduce`, so the call
routes through `param_apply`'s *raw arm* (privileged dispatch).
`recognizer_wrap_fn` is never reached.

*Mechanism.* `hyp_reduce`'s handler consults the stored type's
codomain function. For `hyp_T : Type` specifically, `Type`'s
codomain function is `type_predicate_h_rule`: it returns
`Return (tree_eq (stuck_stored_type v) hyp_T)` iff `v` is a
kernel-minted stuck form, else `Return FF`. Again a concrete Bool
verdict.

*Scope.* `Type`-specific. Only `Type`-typed hypotheses are
operationally predicates — Pi-hyps extend spines via `hyp_reduce`'s
default behavior, Sigma-hyps project, non-applicable hyps don't
apply at all. So only `Type` needs a predicate-side codomain function
implementing the H-rule.

*Source.* §12.10, `type_predicate_h_rule`.

=== Why both sides exist

The recognizer-side handles the common case: any library type
recognizer can short-circuit on its own hypotheses uniformly. The
predicate-side handles the polymorphic-quantification case: when a
function is polymorphic over `Type`, Pi-recognition mints a
`Type`-hyp and applies the body to it; the body's `A`-typed values
must be recognized as inhabitants of that same hyp. Without the
predicate-side H-rule, polymorphic `Pi Type ({A} -> Pi A ({_} -> A))`
fails because applying `hyp_A` to candidate values produces non-`Ok
TT` results.

Without either side, polymorphism breaks. With both, polymorphic
Pi-types work: `Pi Type ({A} -> Pi A ({_} -> A))` (the polymorphic
identity) inhabits successfully because A-hypotheses applied to it
produce A-hypotheses, which the codomain's recognizer accepts via
H-rule.

== When stuck forms cause cascading failure

A recognizer body that doesn't short-circuit on hypothesis arguments
(via H-rule) propagates stuck-ness through its operations:

```
match (safe_is_fork hyp) { TT => ...; FF => ... }
  -- safe_is_fork hyp → StuckElim form (stuck Bool)
  -- match on stuck Bool → stuck application
  -- subsequent operations → bigger stuck term
  -- recognizer's overall result → Ok stuck_term (not Ok TT)
```

The test framework asserts `Ok TT`. A stuck CheckerResult doesn't
equal `Ok TT`. The test fails.

This is why `make_recognizer`'s H-rule is mandatory for any
recognizer that might be applied to a hypothesis — which is
*every* recognizer in practice, since `bind_hyp` can create
hypotheses of any type.

== Stuck forms in tests

A test `test typecheck T v = TT` requires the computation to reduce
to literal `TT`. Stuck forms (which represent unresolved computation)
don't reduce to TT — they're not TT, they're symbolic terms
representing "what TT would be once we know the unknowns."

For tests over hypothesis-laden computations (like strict validation
of Pi-typed recognizers via Pi's body-check), the *H-rule short-
circuits* before stuck forms can propagate. With H-rule, the body
returns a concrete `Ok TT` or `Ok FF` via the tree_eq check. Without
it, the body returns stuck and the test fails.

The `make_recognizer` discipline is therefore load-bearing for
strict validation: without it, no recognizer passes its strict
validation tests, because the body-check propagates stuck forms.

== Stuck forms after strip

The `strip` pass (§10) removes `checked` wait-forms from a validated
tree. It does NOT remove kernel-rooted stuck forms — those represent
genuine unknown values, not type-checking artifacts. After strip,
validated programs that compute over hypothesis-typed values (e.g.,
polymorphic library functions instantiated lazily) still contain
stuck forms in their reduction paths until the actual values are
supplied at runtime.

This is the proof-carrying-code pattern (§17 Necula 1997) for
disp's setting: certificates assert "the stuck-form propagation is
sound by construction"; strip elides the certificate but leaves the
stuck-form machinery intact.

= The kernel primitives <sec:primitives>

This section gives operational semantics for the four Σ-operations
of §5 plus the parameterized dispatcher `safe_apply`. Each subsection
covers: signature, role (which stuck form it constructs, extends, or
mediates, per §6), disp source, and soundness obligations.

The kernel surface is deliberately small: only operations that
require privileged construction — minting handler-rooted stuck forms
the walker would otherwise reject — live here. Type recognition, typed
function application, and most type-system machinery live in the
library (see §11). (`checked` was previously kernel-resident; its
handler body is walker-safe, so it has been relocated to the library —
see §12 for the library `checked_apply`.)

#note[
  *Handler bodies and the current dispatch environment.* Each
  Σ-operation handler is invoked from inside some `safe_apply Σ`
  call. The dispatcher constructs the internal kernel record `ks`
  per-invocation with `ks.param_apply` bound to `safe_apply Σ` — the
  *current* environment, not the default. So the per-handler source
  code below using `ks.param_apply (...)` always means "dispatch the
  sub-call against the same Σ I was invoked under." Under `test_pure`,
  handlers' sub-calls run under `kernel_handlers`; under
  `default_dispatch`, they run under the full environment. The
  threading is automatic; handler-body authors don't write Σ
  explicitly.

  This means handlers compose: `bind_hyp`'s body-application uses
  whatever Σ the outer call provided. A `test_pure` test traversing
  a `bind_hyp` will see all body reductions also run under
  `kernel_handlers`.
]

== `hyp_reduce`

*Signature.* `hyp_reduce : {stored_type : Type, spine : List Tree_p} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* When a hypothesis `h` is applied to an argument `v`, the
dispatcher routes to `hyp_reduce`. The handler reads `h`'s stored type's
codomain function and uses the `Action` protocol (`Extend new_type | Return value`)
to decide whether to extend the spine or return a value.

*Gloss.* "Apply a neutral to an argument. Look up what the stored
type says should happen — extend the spine with the new arg (and a
new stored type), or short-circuit with a concrete return value, or
mark the call invalid. The codomain function is the per-type policy."

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

*Gloss.* "Dispatch on a target. If the target is concrete (an
ordinary constructor of the inductive type), run the cases-dispatcher
to pick the right branch. If the target is neutral (a hypothesis or
another stuck form), mint a fresh `StuckElim` form that records
'this dispatch would have happened, but the target is symbolic.'
The stored type of the `StuckElim` is `motive(target)`."

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

*Gloss.* "Mint a fresh kernel-pinned neutral tagged with `domain` as
its stored type, then evaluate `body` on it. If `domain` is a function
type, wrap the neutral with `checked` so applications respect the
domain contract. After the body returns, run the escape check (see
'Open vs closed paths' below) — if the hyp leaked via an open path,
raise `Err Escape`; otherwise return the body's result."

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
        TT => wait checked_apply (pair (pi_dom domain) raw_hyp)
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
result via an *open path*, return
`Err (Escape { hyp, body_result, span })`. This prevents the
hypothesis from being smuggled into a context where it could be
exploited via `type_recognizer`'s H-rule.

*Open vs closed paths — precise definition.* A path from `result` to
`raw_hyp` is *closed* iff every step in the path traverses a
distinguished "opaque slot" of a wait-form rooted at a *stuck-form-
producing kernel Σ-op*. The opaque slots are:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Kernel Σ-op*], [*Opaque slot(s)*],
    [`hyp_reduce`], [the `spine` field of pair_snd — applied arguments are inside],
    [`eliminator_frame`], [the `target` field of pair_snd — the value being eliminated],
  ),
  caption: [Opaque slots that constitute closed paths.],
)

Other slots of these same wait-forms (motive, cases, stored_type) and
*all* slots of any other wait-form (postulate-built host-effect
wait-forms, `checked_apply` wait-forms, library type wait-forms) are
*open*. Any path passing through an open slot is an open path; the
hyp escapes.

The rationale: the walker treats the opaque slots above as "values
the kernel knows are stuck symbolic data" — triage on them is
rejected, structural reflection on them is rejected. A hyp inside an
opaque slot stays inside the parametricity bubble. Any other slot is
either user-constructed (open by definition) or accessible to user
code via raw inspection (open in effect).

Worked examples:

```disp
// Closed — passes escape check, returns stuck Nat at top level.
bind_hyp Nat ({x} -> Ok (nat_rec ({_} -> Bool) cases x))
//                       ↑ target slot of eliminator_frame: opaque

// Closed — spine extension.
bind_hyp Nat ({x} -> Ok (apply x 0))
//                       ↑ spine slot of hyp_reduce: opaque

// Open — direct return.
bind_hyp Nat ({x} -> Ok x)                              // ✗ Err Escape

// Open — wrapped in user-constructed pair.
bind_hyp Nat ({x} -> Ok (pair x 0))                     // ✗ Err Escape

// Open — wrapped in a checked contract.
bind_hyp Nat ({x} -> Ok (checked Nat x))                // ✗ Err Escape

// Open — passed into a host-effect payload.
bind_hyp Nat ({x} -> Ok (host_write_stdout (nat_to_string x)))
//                                          ↑ open path through WRITE_STDOUT_SIG
//                                            wait-form's pair_snd  ✗ Err Escape
```

*Soundness story.* The escape check blocks direct hyp leakage. The
*closed-path* leak case is real — user code can return a stuck form
whose `stored_type` is whatever the user chose for the motive. The
resulting top-level value passes `typecheck T = Ok TT` for that T
(via H-rule). Soundness depends on the property that stuck forms
*never reduce to concrete values* of their declared type — the same
property underwriting the Type:Type conjecture (§11.6). See §14.5
for the soundness argument.

== `postulate`

*Signature.* `postulate : {sig : Tree} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* The kernel-mediated constructor for wait-forms rooted at any
handler in the current Σ. User code can't directly construct such
trees — the walker rejects forgery for any sig pinned by Σ (§4). The
`postulate` handler runs in the privileged arm; it is exempt from
walker enforcement and can mint the otherwise-forbidden fork shape.
Each library postulate `host_op : T := postulate sig` produces a
`checked T (call_via_postulate sig)` value; the `checked` wrap (§8,
§12) enforces input validation, and `postulate`'s handler validates
that the payload contains no neutrals before constructing.

*Gloss.* "Build a wait-form rooted at `sig`, paired with the given
payload, on behalf of user code. Refuse if the payload contains
neutrals (would smuggle a hyp to a host handler). This is the only
path by which user code produces a pinned-sig fork shape, because
the walker rejects all other paths."

The disp library is responsible for the *type ascription* T. The host
provides only the handler tree (the entry in `host_provided : Σ`);
disp source code asserts what type that handler implements. Multiple
postulates can target the same sig with different (typically
incrementally stricter) type ascriptions; users pick the one matching
their proof obligations at the call site.

*Disp source (sketch):*

```disp
let q_postulate_fn = {ks, raw, query} ->
  {meta, payload} -> {
    let sig = pair_snd meta             // the target handler sig
    // Sanitize: payload must not contain forged neutrals or
    // hypothesis leakage. The host primitive must not see them.
    match (scan_no_neutral payload) {
      FF => Err (Malformed { handler = "postulate", meta = payload, span })
      TT => Ok (wait_with_sig sig payload)
    }
  }

// wait_with_sig is host-mediated: it constructs the fork shape
// `pair_fst = sig, pair_snd = payload` using the same encoding the
// wait-form uses, bypassing the walker (we're in the privileged arm).
```

*Soundness obligation.* The host's handler for `sig`, when invoked,
behaves consistently with whatever type ascription the disp library
assigned to it. Type-ascription correctness is a *library-level*
soundness concern; the kernel only ensures that user code reaches the
host handler through the postulate route.

== `safe_apply`

*Signature.* `safe_apply : List Tree -> Tree_p -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* The parameterized dispatcher. Takes a dispatch environment Σ
as its first argument. Each call decides between trusted raw execution
(for handler invocations whose sig is in Σ) and unprivileged walker

*Gloss.* "Look at `f x`. If `f`'s sig is in Σ, run the handler raw
(it's privileged). Otherwise, run the walker — apply substrate
reduction rules, but reject any rule that would forge a pinned sig
or triage on a neutral. Either way, sub-evaluations recurse with the
same Σ."
reduction (for everything else). The dispatcher does not route to
specific handlers — wait-form reduction does that automatically
(§5.4).

*`param_apply` is the default-instance name.* The kernel ships
`default_dispatch : List Tree` (the concatenation of `kernel_handlers`
and `host_provided`); `param_apply := safe_apply default_dispatch` is
the version used by `typecheck`, by tests, and by every implicit
reduction in this spec. Stricter callers (foundational tests,
sandboxes) use `safe_apply Σ_strict` with a narrower Σ.

```disp
// Membership check on Σ. Linear scan; each comparison is O(1) hash-cons id.
is_pinned_sig := {Σ, sig} ->
  list_any ({h} -> tree_eq sig (checker_sig h)) Σ
```

*The dispatcher.*

```disp
safe_apply := fix ({self, Σ, f, x} ->
  match (and (is_wait_form f) (is_pinned_sig Σ (pair_fst f))) {
    TT => f x                                // raw apply: handler embedded in f
    FF => walker_step Σ (self Σ) f x         // walker (§4), parameterized by Σ
  })
```

The two arms have different semantics:

- *Raw arm.* `f x` is plain substrate apply (§2.1) — no walker
  enforcement, no further dispatch at this level. The wait-form's
  bracket-abstracted shape reduces `wait k m x` to `k m x`, invoking
  the handler whose privilege we just granted. The handler's body,
  running raw, can do operations the walker would reject — minting
  neutrals (`bind_hyp`), extending spines (`hyp_reduce`), constructing
  handler-sig-rooted wait-forms for H-rule reconstruction or via
  `postulate`.

- *Walker arm.* `walker_step` is the parametricity-enforcing
  reduction of §4, also parameterized by Σ. Its internal sub-applies
  re-enter `self Σ` (the same `safe_apply` with the same environment)
  rather than calling substrate apply directly, so dispatch happens
  at every layer of nested reduction. Any sub-tree that happens to be
  a Σ-pinned wait-form gets routed back to the raw arm.

*Default dispatch environment.* The kernel's standard top-level
bindings:

```disp
let kernel_handlers : List Tree := [
  q_hyp_reduce_fn ;
  q_bind_hyp_fn ;
  q_eliminator_frame_fn ;
  q_postulate_fn
]

let host_provided : List Tree := /* exposed by the host runtime */

let default_dispatch : List Tree := concat kernel_handlers host_provided

let param_apply := safe_apply default_dispatch
```

The host's contribution `host_provided` is a *value*, not a
registration: at startup the host constructs a tree-encoded list of
handler trees (each one with a native fast-path interceptor) and
binds it to `host_provided` in the disp top-level scope. The disp
side concatenates explicitly. No mutable state, no scope-tracking
primitives — just list values passed to `safe_apply`.

*Native fast-path.* The host runtime intercepts `apply(safe_apply, …)`
(or its specialization `apply(param_apply, …)`) based on the compiled
tree id and runs a TypeScript implementation of the same logic. The
in-language version is the spec; the native is the optimization,
producing bit-identical results. For host-provided handler trees, the
fast-path resolves the handler invocation directly to the
corresponding TS function (a second fast-path tier).

= Boundary operations and checked values <sec:boundary>

The boundary between untrusted user trees and the Kleisli world of
`CheckerResult` is two operations wide: `param_lift` sanitizes
incoming values, and `checked` wraps function values so applications
go through input-checking. `typecheck` and `validate` build on these
to turn the boundary into the user-facing query and certificate-
issuance APIs respectively. This section covers all four.

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

*Pure variant.* For foundational tests, untrusted-code elaboration, and
any context where host primitives must not influence the verdict, use
`typecheck_pure`:

```disp
typecheck_pure : Type -> Tree_p -> CheckerResult Bool

typecheck_pure := {T, v} ->
  bind (param_lift v) ({sanitized_v} ->
    safe_apply kernel_handlers T sanitized_v)
```

Identical behavior except: any wait-form rooted at a host primitive
inside `T` or `v` will be walker-rejected (the host's sigs aren't in
`kernel_handlers`, so the walker now considers them unpinned and may
attempt to reduce them, exposing whatever the host stubbed in their
place). Use `typecheck_pure` when the answer must be independent of
the host environment.

== Example traces

*Trace 1: `typecheck Bool TT`.*
+ `param_lift TT` → `Ok TT` (TT contains no neutrals).
+ `param_apply Bool TT` reduces `wait bool_recognizer bool_meta TT` → `bool_recognizer bool_meta TT` → `TT` is a canonical Bool shape → returns `Ok TT`.
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

== Checked values: manifest contracts <sec:checked-values>

Every typed function value in disp is *contract-wrapped*: it carries
its declared domain type, and applying it triggers a runtime input-
check. The wrapping is via the library `checked` construction, whose
full definition lives in §12; this subsection covers how checked
values participate in the boundary.

This is the manifest contracts discipline from the Findler-Felleisen
contract semantics, refined by Greenberg-Pierce-Weirich to identify
contracts with refinement types. See §17 for full citations.

== The three library constructors

`checked`, `typed_lambda`, and `validate` are library functions
built on the wait-form encoding of §5.4:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Constructor*], [*Definition*], [*When used*],
    [`checked T v`], [`wait checked_apply (pair T v)`], [Direct construction; no validation],
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
- `bogus` evaluates to `wait checked_apply (pair Bool zero)`.
- Later, the user calls `validate Bool bogus`:
  - Internally `typecheck Bool bogus` runs:
    - `param_lift bogus` → `Ok bogus`.
    - `param_apply Bool bogus` — Bool's recognizer triages on `bogus`.
      The wait-form is a fork with `checked_apply` as `pair_fst`, not
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
→ ks.param_apply (checked_apply) (pair (Pi A B) f) x
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

`test` is an elaborator keyword. `test expr` reduces `expr` under
`param_apply` (= `safe_apply default_dispatch`) and asserts the result
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

== Tests are evaluated under the default dispatch environment

Because `test expr` reduces `expr` under `param_apply`, tests *may
fire host primitives* — IO operations, syscalls, time-of-day reads,
anything `host_provided` exposes. This is by design: tests are a
useful place to make empirical observations about the surrounding
environment.

```disp
test (file_exists "lib/prelude.disp") = TT
test (length (read_env_var "PATH")) > 0
```

Pure-test discipline is opt-in via a sugar that substitutes a stricter
dispatch environment:

```disp
test_pure := {expr} -> safe_apply kernel_handlers expr

// Usage:
test (test_pure (add 2 3)) = 5
test (test_pure (typecheck Type Bool)) = Ok TT
```

Foundational and conversion tests (anything underwriting the type
system's soundness story) should run under `test_pure`. Behavioral
tests of effectful library code use the default. Users elaborating
untrusted code through `disp` should run elaboration under
`safe_apply kernel_handlers` (the CLI's `--pure-elaboration` flag
substitutes this).

#note[
  *Two failure modes for a `test` declaration.* Under the default
  environment, a `test` can fail either because the asserted equality
  doesn't hold, or because reduction reached a stuck form rooted at a
  Σ-pinned sig the test framework doesn't know how to advance (a
  symbolic value with no concrete answer). The latter is a soundness-
  relevant signal — the test caught the system in an underspecified
  state — and is reported distinctly from value-mismatch failures.
]

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
  match (has_sig checked_apply t) {
    TT => self (pair_snd (type_meta t))   // unwrap, recurse into inner
    FF => triage
      t                                   // leaf: unchanged
      ({c} -> t (self c))                 // stem: recurse
      ({l, r} -> t (self l) (self r))     // fork: recurse on both children
      t
  })
```

Strip is a tree-level function. It walks the entire tree, identifies
`checked_apply` wait-forms by signature, and replaces each with its
underlying value (then recursively strips inside).

*Strip leaves handler-rooted wait-forms intact.* Kernel-rooted stuck
forms (hypotheses, stuck eliminations) and host-effect wait-forms
both survive strip — only `checked_apply` wait-forms are unwrapped.
Strip's role is to elide validated contracts, not to remove effects.
A stripped program retains all of its effect machinery and executes
identically under `safe_apply default_dispatch`, modulo the per-call
contract overhead.

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
// Helper synonyms used in MetaShape:
//
//   Functor    := Tree_p                 // a morphism-action function;
//                                        //   sentinel `trivial_functor` means
//                                        //   "identity / no transport rule"
//   Applicable := Type -> Tree_p -> Action
//                                        // codomain_fn for function-shaped
//                                        //   types; sentinel `none` means
//                                        //   non-applicable
//   Action     := Extend Type | Return Tree_p | Invalid
//                                        // the protocol consumed by hyp_reduce

MetaShape := Refinement Record [
  ("recognizer_params", Tree),                  // closed args to the recognizer
  ("functor", Functor),                         // morphism action for transport (§13)
  ("applicable", Optional Applicable),          // codomain_fn for function-shaped types
  ("behavioral_specs", Optional (List Path))    // optional Path-typed proofs
]
```

`MetaShape` is the library refinement type capturing this convention.
The structural shape (a Record with these fields) is enforced; deeper
validation of each field's contents is the validator's responsibility.
At the bootstrap layer, where library record types don't yet exist,
each field is just a tree subterm of the meta payload — the synonyms
above name conventions, not enforced contracts.

The convention is *extensible*. Adding a new conventional field
(e.g., for a new modality or effect system) extends MetaShape's
expected layout. Existing types whose meta lacks the new field still
pass structural checks; library code that needs the new field handles
its absence explicitly.

(Records, Refinement, and the projection mechanism are library
constructions detailed in §12. Their behavior in turn relies on
Sigma's projection codomain_fn, also §12. The dependency cycle
between Type / MetaShape / Pi / Sigma is broken by deferring tests
— see §11, validators-as-library-entities.)

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

== `Type`, previewed

The next subsection discusses `Type`'s self-consistency, but `Type`'s
concrete construction lives in §12 alongside Bool/Nat/Pi/etc. For the
§11.6 discussion, take `Type` as the wait-form

```disp
Type := wait type_recognizer type_self_meta
```

where `type_recognizer` is a `make_recognizer`-wrapped function (§12,
`make_recognizer`) that accepts a candidate `v` iff:

+ `v` is a wait-form (`safe_is_fork v`),
+ `v`'s `pair_fst` was itself produced via `make_recognizer` (i.e.
  carries `checker_sig recognizer_wrap_fn`), and
+ `v`'s `pair_snd` conforms to the MetaShape layout from §11.3.

And `type_self_meta` is a MetaShape-conforming record with
`recognizer_params := unit_witness`, `functor := trivial_functor`,
`applicable := some type_predicate_h_rule` (the predicate-side
H-rule, §6.4 and §12.10), `behavioral_specs := none`.

The §11.6 argument depends on this contract *plus* the predicate-side
H-rule: `Type` is a wait-form with a structural recognizer, a
MetaShape-conforming meta, and a codomain_fn that returns
`Return (tree_eq (neutral_stored_type v) self_as_hyp)` for neutral
`v` and `Return FF` otherwise. The actual recognizer body and
H-rule are §12.10's responsibility; readers can verify they satisfy
this contract there.

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
inhabitants.* The core property: Pi's body check requires the
body's result, applied to the codomain recognizer, to reduce to
*literal `Ok TT`*. Two failure modes block ⊥-inhabitation:

  + Stuck symbolic results — trees produced by `hyp_reduce`,
    `eliminator_frame`, or composition through them — are not the
    tree `Ok TT`.
  + Concrete `Ok FF` from a decidable codomain_fn (notably `Type`'s
    predicate-side H-rule) is also not `Ok TT`.

To inhabit ⊥, the body (under a fresh Type-hypothesis `hyp_A`) must
produce a value `v` whose check against `hyp_A` reduces to `Ok TT`.
`hyp_A` is a kernel-minted neutral with stored type `Type`, so
applying `hyp_A` to `v` routes through `hyp_reduce`, which consults
`Type`'s codomain_fn (`type_predicate_h_rule`, §12.10). That
codomain_fn returns `Return (tree_eq (neutral_stored_type v) self_as_hyp)`
— a concrete Bool. The verdict is `TT` iff `v` is itself a
kernel-minted neutral whose stored type hash-cons-equals `hyp_A`.

The three classic candidate bodies all fail this way:
  - *Introspect A*: rejected directly by walker TriageReflect (§4.2)
    before reduction proceeds.
  - *Return a closed term*: closed values fail `is_neutral`, so
    `type_predicate_h_rule` returns `Ok FF`.
  - *Return `hyp_A` itself via I-shortcut*: `hyp_A` is a neutral, but
    its stored type is `Type`, not `hyp_A`. `tree_eq Type hyp_A = FF`
    (distinct hash-cons identities), so `Ok FF`.

The deeper invariant: the only way to obtain a `v` with
`neutral_stored_type v = hyp_A` is to receive one via `bind_hyp` at
domain `hyp_A`. The body of `⊥ = Pi Type ({A} -> A)` takes only
`A = hyp_A` as parameter — it has no `A`-typed parameter, and
`bind_hyp` is kernel-privileged (the walker's stem-forge rule
blocks user-side construction of `kernel.hyp_reduce`-rooted
wait-forms). Therefore no body-constructible `v` satisfies the
H-rule's `tree_eq` check.

This "decidable-FF except for matching-identity hypotheses" framing
handles the Type:Type edge case (`A = Type` is in `Type`) directly:
even when `v = hyp_A` (the I-shortcut path), the stored type is
`Type ≠ hyp_A`, giving `Ok FF`.

*2. The argument lifts to Hurkens.* Hurkens-style normalizing
constructions reduce to the ⊥-inhabitation problem at some
intermediate position; (1) blocks it. Specifically, every
intermediate Pi-typed sub-expression in Hurkens' encoding has a
body-check whose final step is `param_apply (B hyp_x) result`,
which propagates the `Ok FF`-or-stuck verdict inward.

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

Each library type, under the framing of §11. The pattern: define a
recognizer (a closed function `meta -> v -> CheckerResult Bool`),
define a meta record following the MetaShape convention (§11.2),
construct the type as `wait recognizer meta`, and assert validity
via tests.

This section opens with the library infrastructure that supports
strict / behavioral validation (Sigma's projection codomain_fn,
Record, Refinement, MetaShape, RecognizerShape, the `safe_*` helpers),
then walks the individual type-formers (Bool, Nat, Pi, …).

== `Sigma` and projection-as-application

`Sigma A B` is the dependent product. Its values are pairs `pair a b`
where `a : A` and `b : B(a)`. Inspection is via `pair_fst` / `pair_snd`.

For *hypothesis* values of Sigma type, projection works through a
codomain_fn that treats projection as application. Applying a Sigma
hypothesis to a position walker triggers `hyp_reduce`, which dispatches
via `sigma_cod_fn`:

```disp
sigma_cod_fn := {meta, walker_arg} ->
  let A = meta.recognizer_params.first in
  let B = meta.recognizer_params.second in
  match (tree_eq walker_arg walker_pair_fst) {
    TT => Extend A
    FF => match (tree_eq walker_arg walker_pair_snd) {
      TT => Extend (B (apply self_hyp walker_pair_fst))
      FF => Invalid
    }
  }
```

Applying `hyp_AB : Sigma A B` to `walker_pair_fst` extends the spine
with a fresh A-typed projection (representing "the first component
of this hypothesis"). Applying to `walker_pair_snd` extends with B
instantiated at that prior projection — the dependency is preserved.

Sigma's meta:

```disp
sigma_meta_for := {A, B} -> {
  recognizer_params := pair A B,
  functor := sigma_functor,
  applicable := some sigma_cod_fn,
  behavioral_specs := none
}

Sigma := {A, B} -> wait sigma_recognizer (sigma_meta_for A B)

test typecheck Type Sigma            // Sigma is structurally a type
test typecheck StrictType Sigma      // Sigma passes deep validation
```

Records, Refinement-of-Record, and any type built on Sigma chains
inherit projection support through this mechanism.

== `Record`

Records are iterated Sigma chains with named fields. `{x : A, y : B}`
compiles to `Sigma A (\x. Sigma B (\y. Unit))`. The `Record`
type-former takes a field list and produces the appropriate
wait-form:

```disp
Record := {fields} -> wait record_recognizer (record_meta_for fields)
```

Codomain_fn delegates directly to the underlying Sigma chain (option
(a) from the design notes): field-name verification is *parse-time*
only (`r.x` desugars to `r W_x` at elaboration), so codomain_fn just
needs to handle position walkers. Sigma's codomain_fn does this.

Tests:

```disp
test typecheck Type Record
test typecheck StrictType Record
```

== `Refinement` (passthrough codomain_fn)

`Refinement A P` values are inspected as values of A with an attached
P-proof. For hypothesis projection, Refinement defers to A's
codomain_fn:

```disp
refinement_cod_fn := {meta, walker_arg} ->
  let A = meta.recognizer_params.base in
  let A_cod_fn = applicable_eval_of A in
  match (is_some A_cod_fn) {
    TT => apply A_cod_fn (A, walker_arg)
    FF => Invalid
  }
```

A `Refinement Record [...]` hypothesis projects through this delegation
to Sigma's codomain_fn — supporting the same projections as the
underlying Record. This is what lets `MetaShape` (a Refinement of
Record) be used as a Pi domain whose body projects fields.

== `MetaShape` and `RecognizerShape`

The standard metadata layout (§11.2), as a Refinement of Record:

```disp
let MetaShape = Refinement
  (Record [
    ("recognizer_params", Tree),
    ("functor", Tree),
    ("applicable", Tree),
    ("behavioral_specs", Tree)
  ])
  ({_} -> Ok TT)
```

The Refinement's predicate is trivial — the Record-membership check
already enforces field presence. Field-value types are loosely `Tree`
(rather than specifically typed): MetaShape's structural check doesn't
deeply validate each field. The deeper validation (e.g., the
`recognizer` field is a Pi-typed function) is performed by validators
like `StrictType`.

The shape every type-former's recognizer must inhabit:

```disp
let RecognizerShape = Pi MetaShape ({_} ->
                       Pi Tree ({_} -> CheckerResultBool))
```

A function from MetaShape to Tree to CheckerResultBool. `StrictType`'s
validator typechecks each type's recognizer against this shape, which
requires the recognizer to be a `checked`-wrapped function (per Pi's
recognition rules — see the Pi entry later in §12).

Tests:

```disp
test typecheck Type MetaShape
test typecheck Type RecognizerShape
test typecheck StrictType MetaShape
test typecheck StrictType RecognizerShape
```

== Typed application: `checked` and `checked_apply`

`checked` is the library construction for typed function values — a
manifest contract pairing a function with its declared type. When
applied to an argument, the wait-form's reduction fires
`checked_apply`, which checks the argument against the type's domain
before invoking the function:

```disp
let checked_apply = {meta, arg} ->
  let T = pair_fst meta
  let v = pair_snd meta
  match (is_applicable_type T) {
    TT => bind (param_apply (pi_dom T) arg) ({verdict} ->
          match verdict {
            TT => param_apply v arg
            FF => Err (TypeMismatch { expected = pi_dom T, actual = arg })
          })
    FF => Err (NotApplicable { type = T })
  }

let checked = {T, v} -> wait checked_apply (pair T v)

// Ergonomic alias for function-typed values:
let typed_lambda = {A, B, f} -> checked (Pi A B) f

// Typecheck-gated constructor (returns a certificate or None).
let validate = {T, v} ->
  bind (typecheck T v) ({verdict} ->
    match verdict {
      TT => Ok (Some (checked T v))
      FF => Ok None
    })
```

*Walker-safe body.* `checked_apply`'s operations — pair projections,
`is_applicable_type`, `param_apply` — are all walker-safe (no
stem-forge of pinned sigs, no triage on neutrals). The function lives
in the library; no kernel privilege required.

*Dispatcher behavior.* The wait-form `wait checked_apply (pair T v)`
has the sig `checker_sig checked_apply` — a library sig, not in
`default_dispatch`. So the dispatcher does not route it specially;
walker reduction handles `(checked T v) arg` by reducing via wait's
bracket-abstracted shape to `checked_apply (pair T v) arg`. Internal
`param_apply` calls then route through the dispatcher normally.

*`is_applicable_type`*:

```disp
is_applicable_type := {T} ->
  match (safe_is_fork T) {
    FF => FF
    TT => not (is_none (meta_get (safe_pair_snd T) "applicable"))
  }
```

Returns TT iff T's metadata has a non-`none` `applicable` field —
i.e., T is function-shaped.

== `safe_*` helpers (hypothesis-safe structural inspection)

Inspection of a value's tree shape (`is_fork`, `pair_fst`, `has_sig`)
via raw triage is walker-unsafe — triage on a kernel-minted neutral
is rejected (§4.2). The `safe_*` helpers route through
`eliminator_frame_form`, which kernel-mints `StuckElim` for neutrals:

```disp
safe_is_fork := {v} -> eliminator_frame_form {
  dispatcher := tree_shape_dispatcher,
  motive := const Bool,
  cases := {leaf := FF, stem := {_} -> FF, fork := {_, _} -> TT}
} v

safe_pair_fst := {v} -> eliminator_frame_form {
  dispatcher := tree_shape_dispatcher,
  motive := const Tree,
  cases := {leaf := error_form, stem := {c} -> c, fork := {l, _} -> l}
} v

safe_has_sig := {checker, v} ->
  bind (safe_pair_fst v) ({sig} -> Ok (tree_eq sig (checker_sig checker)))

safe_is_neutral := {v} -> safe_has_sig kernel.hyp_reduce v

// The kernel-controlled list of stuck-form-producing Σ-ops. This is a
// fixed property of the kernel, not a per-call parameter — the set of
// "things that can produce stuck symbolic values" is what the kernel
// ships, independent of any dispatch environment Σ. Currently two
// entries; an async-pending Σ-op would add a third (see §15 open
// questions on async).
stuck_form_producers := [hyp_reduce ; eliminator_frame]

// Generalized stuck-form check: v is a wait-form rooted at one of the
// stuck-form-producing kernel Σ-ops. Σ-independent.
safe_is_stuck := {v} ->
  bind (safe_pair_fst v) ({sig} ->
    Ok (list_any ({h} -> tree_eq sig (checker_sig h)) stuck_form_producers))

// Stored-type extraction. Returns the type associated with a stuck v.
// Dispatches by pair_fst on the kernel-controlled producers above.
stuck_stored_type := {v} ->
  let sig = pair_fst v in
  let payload = pair_snd v in
  match (tree_eq sig (checker_sig hyp_reduce)) {
    TT => neutral_meta_type payload      // explicit domain in meta
    FF => match (tree_eq sig (checker_sig eliminator_frame)) {
      TT => apply (stuck_elim_motive payload) (stuck_elim_target payload)
                                          // motive applied to target
      FF => /* unreachable if safe_is_stuck v = TT */
            error "stuck_stored_type on non-stuck input"
    }
  }
```

For concrete values, these reduce to the raw operation's result; for
hypotheses or other handler-rooted stuck forms, they reduce to a
`StuckElim` form representing "this structural property of an unknown
value."

*Where they're used*: primarily *inside* `make_recognizer`'s wrapper.
The H-rule check `safe_is_stuck v` runs on the recognizer's v argument
before delegating; for any handler-rooted stuck v whose stored type
matches the recognizer's self-type, the H-rule branch fires and the
recognizer short-circuits with a concrete `Ok TT`.

*Where they're NOT typically used*: per-type recognizer bodies. Those
bodies run only on concrete v values (the H-rule has already
short-circuited the hypothesis case), so they can use raw operations
freely. This is the architectural benefit of `make_recognizer`: it
isolates the hypothesis-handling discipline to one library function,
keeping per-type recognizer bodies clean.

The `safe_*` helpers are also used by library functions that
genuinely need to inspect tree structure on potentially-hypothesis
arguments — for example, `is_applicable_type` (defined later in §12)
and `is_neutral`. These are kernel-mediated checks that work uniformly.

== `make_recognizer`: the universal H-rule wrapper

Every type's recognizer needs the *H-rule*: when applied to a
hypothesis whose stored type equals the type being checked, return
`Ok TT` directly. Without this, recognizers reject their own
hypothesis values, and Pi-checks of polymorphic functions cascade-fail
(see §6 "Stuck forms and neutrals" for the full story).

The H-rule is universal — every recognizer needs the same wrapping
logic. The library provides `make_recognizer` so per-type authors
write only the concrete-case body:

```disp
let recognizer_wrap_fn = fix ({wrap, body, meta, v} ->
  let self_type = wait (wait recognizer_wrap_fn body) meta in
  match (safe_is_stuck v) {                              // Σ-independent
    TT => Ok (tree_eq self_type (stuck_stored_type v))   // H-rule
    FF => body meta v                                    // concrete dispatch
  })

let make_recognizer = {body} -> wait recognizer_wrap_fn body
```

The generalized check `safe_is_stuck` (§12.6) accepts any wait-form
rooted at a kernel-shipped stuck-form-producer — currently `hyp_reduce`
(raw hypotheses and spine extensions) and `eliminator_frame` (stuck
eliminations). The set is kernel-controlled, not user-extensible, and
independent of the surrounding dispatch environment Σ. This closes a
soundness gap that matters for effect-using polymorphic functions: a
Pi-body containing `eliminator_frame`-built intermediate values (e.g.
`nat_rec`) otherwise produces stuck-typed intermediates that the older
`safe_is_neutral` check missed, cascading into spurious body-check
failures.

The wrapped recognizer is a wait-form `wait recognizer_wrap_fn body`.
Its sig is `checker_sig recognizer_wrap_fn` — a fixed library
signature shared by all `make_recognizer`-built recognizers.

`Type`'s recognizer can structurally verify that a candidate
recognizer was built via `make_recognizer` by checking
`has_sig recognizer_wrap_fn`. This makes the H-rule discipline
*enforceable*: a recognizer that bypasses `make_recognizer` is
structurally distinguishable and is rejected by `Type`.

Library convention: every type-former author writes:

```disp
let my_recognizer = make_recognizer ({meta, v} -> /* concrete-only body */)
```

The body sees only concrete v values; the wrapper handles hypothesis
cases. Bodies can use raw `triage`, `pair_fst`, etc. — they don't
need `safe_*` helpers, because they never run with hypothesis args.

== Library function `type_recognizer`

`Type`'s recognizer (formerly the kernel's `predicate_frame` handler,
relocated to library):

```disp
let type_recognizer = make_recognizer ({meta, v} ->
  bind (safe_is_fork v) ({is_pair} ->
    match is_pair {
      FF => Ok FF
      TT => bind (safe_pair_fst v) ({rec_field} ->
            // Structural check 1: recognizer field is make_recognizer-formed.
            // Catches recognizers that bypassed make_recognizer; they'd be
            // missing H-rule and would silently misbehave on hypothesis args.
            bind (safe_has_sig recognizer_wrap_fn rec_field) ({is_wrapped} ->
            match is_wrapped {
              FF => Ok FF
              TT => bind (safe_pair_snd v) ({meta_field} ->
                    // Structural check 2: meta has MetaShape layout.
                    Ok (has_metashape_layout meta_field))
            }))
    }))
```

`type_recognizer` itself is a `make_recognizer`-built recognizer, so
H-rule fires on hypothesis arguments uniformly. The body's structural
checks run only on concrete v values (using raw `safe_*` for the
top-level fork shape, since this section needs them anyway for the
recognizer's own H-rule infrastructure).

Note: the recognizer-field structural check (`has_sig
recognizer_wrap_fn`) is what enforces the make_recognizer discipline.
Without this check, recognizers built without H-rule would pass
`Type` but break at first hypothesis-argument use. With it, the
failure surfaces at type-construction time.

== `Bool`

```disp
let bool_recognizer = make_recognizer ({_, v} ->
  Ok (or (tree_eq v TT) (tree_eq v FF)))

let bool_meta = {
  recognizer_params := unit_witness,
  functor := trivial_functor,        // discrete: identity transport
  applicable := none,
  behavioral_specs := none
}

let Bool = wait bool_recognizer bool_meta

test typecheck Type Bool
test typecheck StrictType Bool
test bool_recognizer bool_meta TT = Ok TT
test bool_recognizer bool_meta FF = Ok TT
test bool_recognizer bool_meta zero = Ok FF
// H-rule test: hypothesis of Bool is recognized as Bool inhabitant.
test bool_recognizer bool_meta (mint_hyp_form Bool) = Ok TT
```

Body is concrete-only (no H-rule logic — `make_recognizer` provides
it). Discrete: transport is identity. The behavioral tests assert
the recognizer accepts canonical booleans, rejects non-booleans,
and (via H-rule) accepts hypothesis arguments.

== `Nat`

```disp
let nat_recognizer = make_recognizer ({_, v} ->
  // v is Nat iff v is zero (= LEAF) or v = fork(LEAF, n) where n is Nat.
  Ok (fix ({self, x} ->
        triage TT                          // leaf: zero
          ({_} -> FF)                      // stem: not a Nat
          ({l, r} ->                       // fork: zero-like + recurse
            and (tree_eq l t) (self r))
          x) v))

let nat_meta = {
  recognizer_params := unit_witness,
  functor := trivial_functor,
  applicable := none,
  behavioral_specs := none
}

let Nat = wait nat_recognizer nat_meta

test typecheck Type Nat
test typecheck StrictType Nat
test nat_recognizer nat_meta zero = Ok TT
test nat_recognizer nat_meta (succ zero) = Ok TT
test nat_recognizer nat_meta TT = Ok FF
test nat_recognizer nat_meta (mint_hyp_form Nat) = Ok TT   // H-rule
```

== `Pi`

The pivotal one — Pi's recognizer requires its candidate to be a
`checked` wait-form. Raw function values do not inhabit Pi types;
they must be wrapped first (via `typed_lambda` or `checked` directly).

```disp
let pi_recognizer = make_recognizer ({meta, v} ->
  let A = meta.recognizer_params.first
  let B = meta.recognizer_params.second
  // Step 1 (pure structural): v must be a `checked` wait-form.
  // Raw has_sig is fine — v is concrete here (make_recognizer's
  // H-rule already handled the hypothesis case).
  match (has_sig checked_apply v) {
    FF => Ok FF
    TT =>
      // Step 2 (pure structural): v's stored domain matches A.
      let v_A = pair_fst (pair_snd v) in
      match (tree_eq v_A A) {
        FF => Ok FF
        TT =>
          // Step 3 (kernel-mediated): bind a fresh A-hypothesis, apply
          // v to it, then check the result against B hyp.
          bind_hyp A ({hyp} ->
            bind (param_apply v hyp) ({result} ->
              param_apply (B hyp) result))
      }
  })

let pi_meta_for = {A, B} -> {
  recognizer_params := pair A B,
  functor := pi_functor,                          // non-trivial; supports transp (§13)
  applicable := some pi_eval_signature,
  behavioral_specs := none
}

let Pi = {A, B} -> wait pi_recognizer (pi_meta_for A B)

test typecheck Type Pi
test typecheck StrictType Pi
test typecheck (Pi Nat ({_} -> Bool)) is_zero    // is_zero has this Pi type
```

The three-step check (signature, domain match, bind-hyp+body) makes
Pi soundness-preserving. Raw function values fail at step 1;
domain-mismatched `checked` values fail at step 2; body-type mismatches
fail at step 3 with a TypeMismatch or Escape error from the kernel
operations the body invokes.

The recognizer uses `safe_*` helpers (§12.6) for structural inspection,
so it works on hypothesis arguments during strict validation of types
that quantify over functions.

== `Unit`

Single-inhabitant type. The recognizer accepts exactly `unit_witness`
(which is the substrate `LEAF`, i.e. `t`).

```disp
let unit_recognizer = make_recognizer ({_, v} ->
  Ok (tree_eq v unit_witness))

let unit_meta = {
  recognizer_params := unit_witness,
  functor           := trivial_functor,
  applicable        := none,
  behavioral_specs  := none
}

let Unit = wait unit_recognizer unit_meta

test typecheck Type Unit
test unit_recognizer unit_meta unit_witness = Ok TT
test unit_recognizer unit_meta TT            = Ok FF
```

== `Eq`

Propositional equality. `Eq A x y` is inhabited by `refl` exactly when
`tree_eq x y = TT`. Cubical `Path` (§13) is the computational
counterpart; `Eq` is the discrete one.

```disp
let eq_recognizer = make_recognizer ({meta, v} ->
  let x = pair_fst (pair_snd meta)
  let y = pair_snd (pair_snd meta)
  // v is a refl iff v = refl AND x and y are hash-cons-equal
  Ok (and (tree_eq v refl_canonical) (tree_eq x y)))

let eq_meta_for = {A, x, y} -> {
  recognizer_params := pair A (pair x y),
  functor           := eq_functor,         // refl at the new endpoints (§13)
  applicable        := none,
  behavioral_specs  := none
}

let Eq = {A, x, y} -> wait eq_recognizer (eq_meta_for A x y)

test typecheck Type Eq
test typecheck (Eq Nat zero zero) refl
test eq_recognizer (eq_meta_for Nat zero zero) refl = Ok TT
test eq_recognizer (eq_meta_for Nat zero (succ zero)) refl = Ok FF
```

The cross-type-parameter check (`tree_eq x y`) is the load-bearing
piece: refl inhabits `Eq A x x` for any `A` and any `x`, but never
`Eq A x y` when `x ≠ y` (in hash-cons identity).

== `Ord`

Countable ordinals: `zero_ord`, `succ_ord o`, `omega_plus o`. Used as
a recursion measure in proofs and well-founded induction.

```disp
let ord_recognizer = make_recognizer ({_, v} ->
  // v is in Ord iff v is zero_ord, succ_ord o' for o' in Ord, or
  // omega_plus o' for o' in Ord. Structural recursion bottoms out.
  Ok (fix ({self, x} -> /* tagged-shape recursion */) v))

let ord_meta = {
  recognizer_params := unit_witness,
  functor           := trivial_functor,    // discrete
  applicable        := none,
  behavioral_specs  := none
}

let Ord = wait ord_recognizer ord_meta

test typecheck Type Ord
test typecheck StrictType Ord
test ord_recognizer ord_meta zero_ord                       = Ok TT
test ord_recognizer ord_meta (succ_ord zero_ord)            = Ok TT
test ord_recognizer ord_meta (omega_plus zero_ord)          = Ok TT
test ord_recognizer ord_meta TT                              = Ok FF
```

== `String`

Sequences of characters. `String` is library-defined (a list of
character-tagged trees); shown here for completeness.

```disp
let string_recognizer = make_recognizer ({_, v} ->
  // v is a String iff v is a list whose elements are valid char tags
  Ok (and (is_list v) (list_all is_char_tag v)))

let string_meta = {
  recognizer_params := unit_witness,
  functor           := trivial_functor,
  applicable        := none,
  behavioral_specs  := none
}

let String = wait string_recognizer string_meta

test typecheck Type String
test string_recognizer string_meta empty_string            = Ok TT
test string_recognizer string_meta (cons (char "a") empty_string) = Ok TT
```

(Full definitions live in `lib/types/`; the spec mandates the
recognizer / meta shape above and the tests below.)

== `Type` itself

`Type` is constructed identically to every other library type — a
wait-form pairing a recognizer with a MetaShape-conforming meta
record. The recognizer (§12, library function `type_recognizer`)
checks that a candidate value is a wait-form whose recognizer was
built via `make_recognizer` and whose meta has the MetaShape layout.
The meta carries no parameters (Type takes none), a trivial functor,
and the *predicate-side H-rule* in its `applicable` slot.

```disp
// Predicate-side H-rule (see §6.4). Fires when a Type-typed
// hypothesis is applied as a predicate to a candidate value `v`:
// returns Ok TT iff v is itself a kernel-minted neutral whose
// stored type hash-cons-equals the applied hypothesis. This is the
// dual of `make_recognizer`'s recognizer-side H-rule — needed
// because Type-hypotheses, when applied, route through
// `hyp_reduce`'s raw arm and never reach the `make_recognizer`
// wrapper. Body runs raw per §7.1's codomain_fn discipline; raw
// `is_neutral` / `neutral_stored_type` are walker-safe because the
// codomain_fn is invoked from inside `hyp_reduce`'s privileged
// handler.
let type_predicate_h_rule = {ks, raw, neutral_meta, v} -> {
  let self_as_hyp = wait kernel.hyp_reduce neutral_meta in
  match (is_neutral v) {
    TT => Return (tree_eq (neutral_stored_type v) self_as_hyp)
    FF => Return FF
  }
}

let type_self_meta = {
  recognizer_params := unit_witness,           // Type takes no params
  functor           := trivial_functor,
  applicable        := some type_predicate_h_rule,
  behavioral_specs  := none
}

let Type = wait type_recognizer type_self_meta

test typecheck Type Type         // Type is a type (lax)
test typecheck StrictType Type   // Type also passes deep validation

// Predicate-side H-rule tests (the load-bearing case for polymorphism).
let A_hyp  = Hyp Type 0
let B_hyp  = Hyp Type 1
let x_of_A = Hyp A_hyp t

test A_hyp x_of_A = TT     // matching-identity hypothesis: H-rule fires
test B_hyp x_of_A = FF     // distinct Type-hyp: tree_eq FF
test A_hyp 0      = FF     // closed value: is_neutral FF, then Return FF
test A_hyp A_hyp  = FF     // I-shortcut: stored_type=Type ≠ A_hyp
```

Four definitions. No tests fired at construction; `Type` is just a
value. The structural tests run `type_recognizer type_self_meta Type`;
Type's structure (wait-form with MetaShape-conforming metadata,
applicable populated) satisfies the structural check, returns
`Ok TT`, and the tests pass by construction. The H-rule tests
exercise the predicate-side path: applying a Type-hypothesis
through `hyp_reduce` and observing the codomain_fn's verdict.

The Type:Type concern and its (conjectural) resolution are discussed
in §11. The tests themselves run mechanically; their passing is an
empirical observation, while their implications for foundational
consistency remain open.

== Effect interfaces and handlers

An *effect interface* is a typed record of operation signatures. An
*effect handler* is a value of an interface type. Both are ordinary
library constructions — no kernel changes, no extensions to the
dispatch environment, no new validators.

```disp
// Interface: just a record type.
let Console : Type := record {
  print    : Pi String ({_} -> Unit),
  readline : Pi Unit   ({_} -> String)
}

// Native handler: bottoms out at host postulates (§7.5, §15).
let stdoutConsole : Console := {
  print    := host_write_stdout,
  readline := host_read_stdin
}

// Constructed handler: built on top of another.
let prefixedConsole : Console := {
  print    := {msg} -> stdoutConsole.print (concat "[log] " msg),
  readline := stdoutConsole.readline
}

// Mock handler for testing: no IO.
let mockConsole : Console := {
  print    := {_} -> unit_witness,
  readline := {_} -> "stub input"
}

test typecheck Type Console
test typecheck Console stdoutConsole
test typecheck Console prefixedConsole
test typecheck Console mockConsole
```

Functions that use an effect accept a handler value as a Pi-bound
*capability* argument:

```disp
let greet : Pi Console ({_} -> Pi String ({_} -> Unit)) :=
  typed_lambda Console ({_} -> Pi String ({_} -> Unit)) ({c, name} ->
    c.print (concat "hello, " name))

// Usage:
//   greet stdoutConsole  "world"   -- real IO
//   greet prefixedConsole "world"  -- logged IO
//   greet mockConsole     "world"  -- no IO; for tests
```

*Row polymorphism via dependent records.* A function using two
effects accepts a product:

```disp
let backup : Pi (record { c : Console, f : FileIO }) ({_} -> Unit) :=
  typed_lambda _ ({_} -> Unit) ({caps} -> ...)
```

A row-polymorphic function uses Pi over a refinement that requires
specific capabilities:

```disp
let with_console : Pi (R : Type) ({_} ->
  Pi (extends R Console) ({caps} -> Unit)) := ...
```

`extends R Console` is a `Refinement Record [..]` (§12.4) requiring
`R` to contain at least Console's fields. Row union is record
concatenation; row subset is field-projection. All of this falls out
of the record/refinement machinery already in §12 — no effect-row-
specific type machinery needed.

See §15 for the broader effects story: host primitives, postulates,
construction patterns, and the relationship between interfaces and
the dispatch environment.

= Cubical extensions <sec:cubical>

This section folds in the cubical proposal. Cubical operations live
in each library type's `meta.functor` field (the *morphism-action*
function, per MetaShape's §11 convention) — no new kernel primitives,
no new type-former framework. Throughout this section, "type's functor"
is shorthand for the `functor` field of that type's metadata record;
"per-type morphism action" is the function stored there.

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

`I` is a library type whose elements are formulas in
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

let I_meta = {
  recognizer_params := unit_witness,
  functor := trivial_functor,         // I doesn't transport
  applicable := none,
  behavioral_specs := none
}

let I = wait I_recognizer I_meta

test typecheck Type I
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

== `Partial` and cofibrations

We need `Partial` before introducing `comp` because `comp`'s third
argument is a `Partial`-valued partial element.

`IsOne phi : Type` is the proposition "phi reduces to I_one." `Partial
phi A := IsOne phi -> A`. Walker-safe smart constructors for face
systems.

```disp
let IsOne = {i} -> wait isone_recognizer {
  recognizer_params := i,
  functor := trivial_functor,
  applicable := none,
  behavioral_specs := none
}

test typecheck Type IsOne

Partial := {phi, A} -> Pi (IsOne phi) ({_} -> A)

// The empty partial element (used when phi = I_zero):
let empty_partial = {A} -> {_proof_false} -> /* unreachable */
```

(Full design from `CUBICAL_PROPOSAL.typ` §11 carries over; the
recognizer enforces "phi = I_one or phi has the canonical disjunction
shape.")

== Unified `comp`, with `transp` as sugar

The cubical composition operator `comp` and the special case `transp`
share a single calling convention. Every library type's `meta.functor`
field is a *morphism-action* function of the form:

```
functor : Tree_p
//   (self  : the self-reference for fixed-point recursion)
//   (P     : I -> Type, the type-path)
//   (phi   : I, the cofibration)
//   (u     : Partial phi (Pi I ({i} -> P i)))
//   (u0    : P I_zero)
//   -> P I_one
```

Every per-type sketch in this section uses *exactly* this 5-tuple. Per-
type functors that ignore the cofibration / partial-element side (the
"pure transp" case: discrete types, etc.) drop them with `_`.

```disp
// The unified composition operator. Looks up the type's functor and
// hands it the full 5-tuple.
comp := fix ({self, P, phi, u, u0} -> {
  let T0 = apply P I_zero
  let action = meta_get (type_meta T0) "functor"
  match (tree_eq action trivial_functor) {
    TT => StuckElim (apply P I_one) (pair P (pair phi (pair u u0)))
    FF => apply action self P phi u u0
  }
})

// `transp P x` is `comp P I_zero (empty_partial T_path) x` — the
// degenerate case with no cofibration.
transp := {P, x} -> comp P I_zero (empty_partial (apply P I_zero)) x

// `hcomp` is `comp` at a constant family. The first arg is just a
// type rather than a type-path.
hcomp := {A, phi, u, u0} -> comp ({_} -> A) phi u u0
```

The fast path for constant families (when `apply P I_zero` and
`apply P I_one` hash-cons-equal) lives inside each per-type functor —
the functor is free to short-circuit with `u0` when the source and
target types match and `phi = I_zero`.

Per-type morphism actions (always 5-arg, conforming to the calling
convention above):

```disp
// Discrete types (Bool, Nat, False): transport is identity, partial
// elements have no semantic content because there's no path-structure.
let bool_functor  = {_, _, _, _, x} -> x
let nat_functor   = {_, _, _, _, x} -> x

// Pair: component-wise recursion. Splits u/u0 into component
// partials/endpoints and recurses on each side.
let pair_functor  = {self, P, phi, u, u0} ->
  pair (self (P_fst P) phi (u_fst u) (pair_fst u0))
       (self (P_snd P) phi (u_snd u) (pair_snd u0))

// Sigma: dependent second component via the first component's
// trajectory. The B(a) family is reconstructed at each interval point
// by transporting a along P_fst.
let sigma_functor = {self, P, phi, u, u0} -> /* CCHM Sigma rule */ ...

// Pi: contravariant in A, covariant in B; threads the a-trajectory
// through B's family. Body application happens at the source endpoint;
// result is transported forward.
let pi_functor    = {self, P, phi, u, u0} -> /* CCHM Pi rule */ ...

// Eq: refl at the new endpoints. The path-of-refls case is structural.
let eq_functor    = {self, P, phi, u, u0} -> /* CCHM Eq rule */ ...
```

Each value above is dropped directly into the corresponding type's
`functor` meta field. The full per-type rules follow the standard
CCHM treatment (Cohen-Coquand-Huber-Mörtberg 2015); only the dispatch
mechanism (lookup on `meta.functor` of a hash-consed type) is
disp-specific. Crucially, the 5-tuple shape is the *contract every
library type-former must honor* to participate in cubical
machinery — types whose functors take fewer arguments are
non-conforming and will type-error when wired into `comp`.

== `Glue` and univalence

`Glue B [phi ↦ (T, e)]` is a library type that "glues" a base type B
with partial type information (T, e) at the face phi. Its non-trivial
`functor` field implements equivalence-mediated transport.

```disp
let Glue = {B, T, e} -> wait glue_recognizer {
  recognizer_params := glue_params_for B T e,
  functor := glue_functor,             // applies the equivalence on transport
  applicable := none,
  behavioral_specs := none
}

test typecheck Type Glue

// ua constructs a Path Type A B from an equivalence e : A ≃ B.
// Notation `[(i = I_one) ↦ (A, e)]` is mathematical shorthand for the
// partial element supplying (T, e) at the i=I_one face; the actual
// disp source materializes this as a face-system tree.
ua := {A, B, e} -> {i} ->
  Glue B (make_face_system i I_one A e)
```

Transport along `ua e` reduces via `Glue`'s `meta.functor`, which
applies the equivalence `e`. This makes univalence a definable library
theorem (not an axiom).

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
+ *Structural transport on type-formers* via each type's
  `meta.functor` field.
+ *Univalence as a definable theorem* via `Glue` + `ua`.
+ *Representation independence in practice* — functions over one
  representation work on equivalent representations via transport.
+ *No kernel growth.* The four Σ-operations plus the dispatcher are
  the total kernel surface; cubical adds none.

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

= Soundness via tests <sec:soundness>

Disp's soundness is asserted as a *test suite* that the standard
library is expected to pass. There is no "soundness theorem" in the
traditional sense — soundness is a runnable assertion, not a
metatheoretic proof. The conjectural-consistency story (§11.5)
underwrites the foundational interpretation; the tests verify the
operational story.

== Four categories of tests

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Category*], [*What it asserts*],
    [Kernel tests],
      [The four kernel handlers behave per their specs (§7).
       Test that `bind_hyp` mints a neutral, `hyp_reduce` extends
       spines, `eliminator_frame` mints StuckElim on neutrals, etc.
       Run under `test_pure` — the foundational layer must not
       depend on host primitives.],
    [Type-system tests],
      [Each library type passes the expected validators. Test that
       `Type Bool`, `StrictType Bool`, `Type Pi`, etc. all reduce
       to `Ok TT`. Run under `test_pure`.],
    [Behavioral tests],
      [Optional Path-typed proofs in `behavioral_specs`. Test that
       each type's recognizer behaves as documented on canonical
       inhabitants and counter-examples. Run under `test_pure`
       unless the spec genuinely depends on a host primitive.],
    [Environment probes],
      [*Empirical* observations about the surrounding host
       environment — file existence, env vars, time-of-day,
       network reachability. Run under the default dispatch
       environment (host primitives active). Useful for integration
       checks; *not foundational*. May produce different results
       in different environments.],
  ),
  caption: [Test categories asserting soundness.],
)

The first three are runnable as pure tests and underwrite the
foundational story. Environment probes are an *operational* category —
they fail when the host environment doesn't match expectations, not
when the type system is unsound. Mark them with `test_observe` for
clarity (a sugar that runs under the default dispatch environment
and reports failures distinctly from foundational failures).

== Sample tests

```disp
// Kernel tests
test bind_hyp_mints_kernel_signature_neutral
test hyp_reduce_extends_spine_correctly
test eliminator_frame_mints_stuck_on_neutral
test postulate_constructs_pinned_wait_form
test safe_apply_routes_default_dispatch_handlers_raw

// Type-system tests (lax)
test typecheck Type Bool = TT
test typecheck Type Nat = TT
test typecheck Type Pi = TT
test typecheck Type Sigma = TT
test typecheck Type Eq = TT
test typecheck Type Type = TT

// Type-system tests (strict)
test typecheck StrictType Bool = TT
test typecheck StrictType Pi = TT
test typecheck StrictType Type = TT

// Behavioral tests (sample)
test bool_recognizer unit_witness TT = Ok TT
test bool_recognizer unit_witness FF = Ok TT
test nat_recognizer unit_witness zero = Ok TT
test pi_recognizer (pi_meta_for Nat ({_} -> Bool)) is_zero = Ok TT
```

The standard library ships these tests inline with the relevant
definitions. The full catalog is browsable as source and runnable as
a whole by re-elaborating the library.

== The trust boundary

What's in the trusted base:

- The four kernel Σ-operations (`hyp_reduce`, `bind_hyp`,
  `eliminator_frame`, `postulate`) implemented in disp source.
- The dispatcher `safe_apply` (in-language reference + native fast-path).
- The host runtime's signature-pinning and native fast-paths for the
  above plus any host-provided handlers in `host_provided`.
- The parametric walker (in-language reference + native fast-path).
- Library validators (`Type`, `StrictType`, `BehavioralType`) and
  their recognizer functions.
- Library `safe_*` helpers (hypothesis-safe structural inspection).
- The `MetaShape` convention.
- Each host primitive's *type ascription* — its `postulate` claim about
  what the underlying host handler implements (library-level trust;
  see §15).

What's *not* in the trusted base:

- The elaborator (purely syntactic; emits trees and tests).
- Type-former definitions (validated by the standard validators
  through tests).
- User code (validated at every typed function application via
  `checked`).

Compared to the previous spec's trust boundary: the library validators
and `safe_*` helpers move into the TCB (they were implicit in the
kernel's `predicate_frame` handler). The kernel surface shrinks
correspondingly. The net trust footprint is roughly the same; the
distribution shifts toward library code that's auditable in `.disp`
source.

== Foundational status

Test-based soundness is *operational*: the tests pass on concrete
inputs. This is necessary but not sufficient for foundational
consistency (Type:Type, etc.) — see §11.5 for the open conjecture.

*Foundational tests must run under `test_pure`.* The conjectural-
consistency argument depends on Σ being `kernel_handlers` only; if
host primitives are in scope during the test, their stubbed return
values can mask soundness issues. The standard library marks every
foundational test (kernel-handler behavior, Type:Type, conversion
properties) with `test_pure`; CI verifies this discipline.

Two distinct claims to keep separate:

+ *The standard library passes its standard tests.* Operational;
  empirically verified at every elaboration. Failure indicates an
  implementation bug or definitional inconsistency.

+ *The disp type system is foundationally consistent (no inhabitant
  of ⊥).* Theoretical; relies on the walker's parametricity
  discipline being strong enough to block Hurkens-style encodings.
  Currently a conjecture; resolution requires either a formal
  parametricity theorem or commitment to ranked universes.

The test suite addresses (1); (2) is open work flagged in §11.5.

*`bind_hyp` shares (2)'s soundness obligation.* The escape check
(§7.3) blocks *direct* hyp leakage via open paths, but stuck forms
produced inside `bind_hyp`'s body — whose target/spine slots
contain `raw_hyp` — pass the escape check and reach the top level.
Such stuck forms have user-chosen stored types and trigger the
H-rule, so they typecheck as inhabitants of those types. The system
remains sound only because:

+ Stuck forms never reduce to concrete values of their declared
  type — they propagate as stuck forms through any further
  computation.
+ Tests demand concrete reductions; a stuck-form reaching a test
  comparison fails the test (stuck ≠ literal expected value).
+ The H-rule produces concrete *verdicts* (`Ok TT`/`Ok FF`) about
  stuck-form inhabitation, but never concrete *inhabitants* of
  uninhabited types.

This is the same property the Type:Type conjecture (§11.6) relies on.
`bind_hyp`'s safety and Type:Type's consistency stand or fall
together. A formal parametricity theorem for the walker's
discipline would settle both; until then both are "empirically
sound with a documented conjecture."

#openq[A formal proof of (2) — Coq/Lean mechanization, semantic
model, or careful pencil-and-paper PER-model argument — would
solidify the foundational story. Until then, the system is
"empirically sound" with a documented conjecture about consistency.]

== Memo policy requirement

Strict validation of self-referential types (`Type`, `RecognizerShape`,
`MetaShape`, `Pi`, `Sigma` — each of which references the others
through their recognizers and metas) requires memoization that
handles *in-progress* recursive queries. Without this, validation
loops.

A concrete example: `test typecheck StrictType Type` triggers
recursive validation through `RecognizerShape` (= `Pi MetaShape (Pi
Tree CheckerResultBool)`). The Pi-body-check for `type_recognizer`
fires `bind_hyp MetaShape`, then applies the body to a fresh
metashape-hypothesis. Inside, body operations may recursively call
`typecheck RecognizerShape type_recognizer` again — the *same*
validation that's currently in progress.

The kernel's memoization table must record entries as "in progress"
when validation starts, return the optimistic answer (`Ok TT`) for
recursive queries during the in-progress window, and finalize when
the outer validation completes:

```
typecheck T v:
  case memo[T, v] of
    Some (InProgress) => Ok TT       // optimistic; outer validation
                                     // will catch real failures
    Some (Final r)    => r           // memoized result
    None              =>
      memo[T, v] := InProgress
      let r = ... actual validation ...
      memo[T, v] := Final r
      r
```

This is a *fixed-point computation*: the recursive structure asserts
"if my sub-references are well-typed, I'm well-typed." The in-
progress optimistic answer + final verification together implement
the fixed point.

*This policy is load-bearing for strict validation to terminate on
self-referential types.* Without it, `test typecheck StrictType Type`
loops. The spec mandates this memo discipline; implementations must
provide it for strict validators to work.

The lax `Type` validator doesn't need fixed-point memoization (its
structural check doesn't recurse into the candidate's components),
so `test typecheck Type Type` works under any memo policy. The
fixed-point requirement is specifically for the deep validators.

#openq[Empirical verification of fix-based self-reference hash-cons
stability (RECORDS_PROPOSAL.md §9 step 2) is also load-bearing for
H-rule reconstruction. If `wait self.handler meta` (constructed
inside a handler body) doesn't hash-cons-equal `wait kernel.handler
meta` (constructed externally), tree_eq comparisons in the H-rule
fail and validation behaves unpredictably. This needs an explicit
test in the implementation.]

= Effects <sec:effects>

Disp's effect story rests on three observations:

+ *Effects are dispatch-environment entries.* The kernel ships
  handlers for its type-system effects (neutral minting, spine
  extension, case dispatch, postulate construction). The host exposes
  additional handlers for IO primitives. Both populate the same Σ
  list-value. `safe_apply Σ` evaluates trees against any chosen
  environment.

+ *Effect interfaces are typed records; handlers are values.* No
  special syntax, no scoped runtime registry, no row-type system.
  An interface is a record type; a handler is a record value; calling
  effects is record projection plus apply. Capability passing
  (Effekt-style) is how functions declare effect requirements.

+ *Strengthening proceeds via library postulates.* The host registers
  raw handlers (sig + TS body, no types). The disp library ascribes
  types via `postulate`. As library matures, stricter postulates
  supersede looser ones; the host is unaffected. Trusted-base
  discipline is library-level, not kernel-level.

== Host primitives

The host exposes a small fixed set of low-level operations as a
*value* — `host_provided : List Tree` — bound at module-load time.
Each entry is a handler tree whose `apply` semantics are intercepted
by the native fast-path and routed to the corresponding TypeScript
function.

```typescript
// src/host_primitives.ts (host setup, not a registration call from disp)

// Each host primitive is a TS function; the host builds anchor trees
// for each and wires them into the native fast-path table.
const handlers = {
  WRITE_STDOUT_SIG: (meta, finalArg) => {
    const msg = treeToString(pair_fst(meta));
    const cont = pair_snd(meta);
    process.stdout.write(msg);
    return applyTree(cont, UNIT);
  },
  READ_STDIN_SIG:  (meta, finalArg) => readLineSync(),
  GET_TIME_SIG:    (meta, finalArg) => Date.now(),
  // ... ~10-30 total. Frozen at startup.
};

// The host then constructs `host_provided` as a disp tree value:
//   host_provided := [write_stdout_anchor, read_stdin_anchor, ...]
// and exposes it in the disp top-level scope.
```

There is no `register_effect_handler` *call* exposed to disp source.
The host's contribution is a single value, concatenated into
`default_dispatch` explicitly by `lib/prelude.disp`:

```disp
let default_dispatch : List Tree := concat kernel_handlers host_provided
```

Different builds (production, test, minimal sandbox) can expose
different `host_provided` lists — the disp library sees only the
value.

== Postulates: typed ascriptions for host primitives

The host hands disp raw handler trees. The disp library separately
ascribes types using `postulate` (§7.5):

```disp
// In lib/host/console.disp:
let host_write_stdout : Pi String ({_} -> Unit) :=
  postulate WRITE_STDOUT_SIG

let host_read_stdin : Pi Unit ({_} -> String) :=
  postulate READ_STDIN_SIG
```

The host doesn't know about `Pi String _`; the disp library asserts
this is the right type. As library matures and refinement proofs
accumulate, stricter ascriptions can be written without changing the
host:

```disp
// lib/host/console_v2.disp — stricter type, same underlying sig.
let host_write_stdout_strict : Pi (Refinement String is_valid_utf8) ({_} -> Unit) :=
  postulate WRITE_STDOUT_SIG
```

Both postulates coexist. Call sites pick the one matching their
proof obligations. Older code using the looser ascription continues
to work.

*Postulate migration discipline.* When a stricter postulate
supersedes a looser one targeting the same sig:

+ Keep both bindings in the library (e.g. `host_write_stdout` and
  `host_write_stdout_strict`).
+ Provide a `Refinement`-to-base coercion if the stricter type is a
  refinement of the looser one — call sites that lift to the strict
  version supply the necessary proof at the call site.
+ Lint: a future `--check-postulate-currency` flag flags use of the
  loose binding when a strict one targeting the same sig is in
  scope. Currently advisory.
+ Eventually the loose binding can be removed once all call sites
  have migrated; the host's underlying handler is unchanged.

This is the same pattern as Lean 4's `Decidable` migration story or
Coq's typeclass refinement: weaker ascriptions remain available for
legacy code; stricter ones land alongside; deprecation is gradual.

#note[
  *Trusted base.* Each postulate is a *trust claim*: "the handler
  registered for `WRITE_STDOUT_SIG` behaves consistently with this
  declared type." A wrong ascription is a soundness bug, but it's
  library-level — fixable without touching the kernel or the host.
  The kernel ensures user code can only invoke host handlers via the
  postulate route; it cannot ensure the postulate's type matches the
  handler's behavior. Discipline: only the standard library's
  postulates are "blessed"; user code adding its own postulates is
  use-at-your-own-risk.
]

== Effect interfaces and handlers

§12 (Effect interfaces and handlers) covers the typed-records side.
Briefly:

- An *interface* is a record type, e.g. `Console := record { print : ..., readline : ... }`.
- A *handler* is a value of an interface type. Native handlers are
  built from postulates; constructed handlers are built from other
  handlers; mock handlers stub everything.
- A function that uses an effect *accepts a handler as a Pi-bound
  argument* (capability-passing à la Effekt). Row polymorphism is
  product types of interfaces (§12).

```disp
let greet : Pi Console ({_} -> Pi String ({_} -> Unit)) :=
  typed_lambda Console _ ({c, name} ->
    c.print (concat "hello, " name))

// Usage: greet stdoutConsole "world"  (real IO)
//        greet mockConsole "world"     (no IO; for tests)
```

== Test handlers and mock environments

Tests can install mock handlers in two ways:

*(a) Pass a mock interface value.* Simplest. Works for any effect
expressed as a typed interface:

```disp
test (greet mockConsole "world") = unit_witness
```

*(b) Substitute a stricter dispatch environment.* Useful when the
test code under examination calls host primitives directly (e.g.,
the postulate values themselves). Build a custom Σ with overrides:

```disp
let mock_env : List Tree := concat kernel_handlers [my_mock_print_handler]
let test_apply := safe_apply mock_env

test (test_apply (greet stdoutConsole "world")) = unit_witness
```

Both compose. Most tests use (a) for clarity; (b) is the escape
hatch for testing primitive postulates.

== Pure regions

For foundational reasoning, sandboxed elaboration of untrusted code,
or any context where host primitives must not influence behavior,
evaluate under `safe_apply kernel_handlers`:

```disp
test_pure := {expr} -> safe_apply kernel_handlers expr
```

Inside `test_pure`, host sigs are *not* pinned, so the walker
considers their wait-forms as ordinary trees and attempts to reduce
them. Since host handlers don't reduce as ordinary trees (their
implementation is the host's TS function, accessible only when the
sig is pinned), the reduction stalls or fails. This is the desired
behavior: pure regions can't accidentally fire host effects.

== What this gives, what it gives up

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Capability*], [*Mechanism*],
    [O(1) effect dispatch], [Hash-cons sig comparison],
    [Multi-shot continuations], [Explicit CPS in interface for nondet/backtracking],
    [Effect typing], [Capability passing through typed-record arguments],
    [Row polymorphism], [Pi over `extends R Console` Refinement-of-Record],
    [Test mocking], [Pass a different handler value or substitute Σ],
    [Pure regions], [`safe_apply kernel_handlers`],
    [Native fast-path], [Host fast-path per sig; transparent to disp source],
    [Foundational soundness], [Postulate type-ascriptions are library trust claims; kernel discipline unchanged],
  ),
  caption: [What disp's effects machinery delivers.],
)

What it doesn't:

- *Transparent multi-shot continuations* (Koka-style). The substrate
  is pure; reified continuations are explicit in the effect interface.
  See the explicit CPS example below.
- *Stateful handlers without explicit state-threading.* No mutable
  cells in disp; stateful effects need either a State capability
  backed by a host primitive or pure state-threading. Best practice
  to be determined.
- *Effect rows in Koka's sense.* Capability passing gives row
  polymorphism at the type level (via record extension) but doesn't
  yet provide elaborator inference of effect rows. Future work.

== Explicit CPS in interfaces

When an effect needs control over its continuation — nondeterminism,
backtracking, async, generators — the continuation appears as an
explicit argument to the operation:

```disp
let Nondet : Type := record {
  // For any element type A and result type B, choose takes a list of
  // candidates and a continuation `k : A -> B`. The handler decides
  // how many candidates to try and how to combine the per-call
  // results.
  choose : Pi (A : Type) ({_} ->
           Pi (B : Type) ({_} ->
           Pi (List A) ({_} ->
           Pi (Pi A ({_} -> B)) ({_} ->
           List B))))
}

// Try-all handler: invoke k on every candidate, collect results.
let try_all : Nondet := {
  choose := {A, B, candidates, k} -> list_map k candidates
}

// First-success handler: invoke k on the first candidate, return its
// result (a single B, not a List B — this handler has a different
// interface, but the same operation shape).
```

The continuation `k` is a tree value. The handler can:
- Call `k` once → linear evaluation.
- Call `k` zero times → exit early (effect-style throw).
- Call `k` many times → multi-shot, generators, nondet.

Disp's substrate is pure, so multi-shot is free (continuations don't
hold mutable state; calling them repeatedly with different arguments
just performs more computation). The cost vs Koka: the user writes
`k` explicitly in the interface and at every operation call.

== Async via stuck forms (sketch)

Async I/O is where stuck forms (§6) become operationally interesting.
A host primitive like:

```disp
let host_read_async : Pi Fd ({_} -> Promise Bytes) := postulate READ_ASYNC_SIG
```

isn't synchronous — the host handler can't block to produce the
result. Instead, it returns a *fresh stuck form* representing "the
result, when it arrives." A future kernel Σ-op `async_pending` (not
yet specified) would mint these stuck forms; the host's event loop
substitutes concrete bytes when epoll signals completion.

This requires `safe_is_stuck` to recognize a third stuck-form
producer; the `stuck_form_producers` list (§12.6) would grow from
`[hyp_reduce, eliminator_frame]` to `[hyp_reduce, eliminator_frame,
async_pending]`. The H-rule then fires for `Promise X`-typed stuck
forms uniformly; polymorphic code over promises works the same way
as polymorphic code over neutrals.

Disp doesn't ship async yet. The point: stuck forms are a general
mechanism for "values whose concrete identity isn't yet known," not
just for type-checking hypotheses. Async slots in by adding one
producer to the kernel list — same machinery, new motivation.

#openq[
  *Higher-order effects (Wu–Schrijvers–Hinze 2014).* Effects whose
  operations take computation arguments (e.g. `catch handler body`)
  need more machinery than capability passing. The `handle`-via-
  `eliminator_frame` reduction (§6) covers algebraic operations
  cleanly; higher-order operations are a future addition.
]

#openq[
  *Stateful effects without host refs.* Disp has no mutable cells.
  Buffered loggers, accumulators, and similar stateful handlers
  currently require explicit state-threading. Either accept the
  verbosity, add a host primitive `host_alloc_ref`, or reify
  continuations and use them to thread state. The trade-offs need a
  design document.
]

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

The walker's two restrictions (no triage on neutrals, no pinned-sig-
rooted fork construction) are a *local, structural* enforcement of
parametricity. Standard parametric type theories require global
type-level reasoning (e.g., relational interpretation of types). Disp
gets the parametricity property from local pattern-matching on tree
structure.

== Dispatch-environment parameterization

No other dependently-typed language treats the privileged-handler set
as a first-class runtime parameter of the reducer. In Koka and Eff,
handler scope is built into the language's evaluation semantics. In
OCaml 5, the effect set is fixed at compile time. In Lean 4, IO
primitives are baked into the metatheory.

Disp passes Σ as a value to `safe_apply`, so strict callers
(foundational tests, untrusted-code sandboxes, verified-extraction
targets) can construct stricter dispatch environments without
changing the kernel or recompiling the language. The same mechanism
that pins neutrals against forgery also pins host primitives against
forgery; one parameter, one rule, one walker discipline. This
unification of "type-system effects" and "real-world effects" under
a common Σ-parameterization is, to our knowledge, novel.

== Elaboration as pure syntax + tests

Standard contract-compilation does syntactic inference of contract-
eligible positions; disp's elaborator goes further by doing zero
type-checking judgments. It transforms syntax and emits trees + tests
(§9). The "type system" is a set of library validators exercised by
those tests, not something the elaborator decides.

This is more minimal than Lean/Coq elaboration (substantial
inference and type-checking) and more minimal than standard
Findler-Felleisen contract compilation (inserts contracts at typed-
untyped boundaries). The validator-as-value framing is novel.

== Unified `checked` for typed application

Manifest-contract systems typically distinguish runtime checks from
validation certificates. Disp unifies them: `checked T v` is a
wait-form that both certifies "v is claimed to inhabit T" and runs
the input-check at every application. `typed_lambda` and `validate`
are library aliases over `checked`. (`checked` is now a library
function since its handler body is walker-safe — see §12.)

== Metacircular discipline

The kernel is a small set of privileged constructors and a dispatcher;
the type system is defined entirely in disp source as library
validators and tests; the host implements optimizations but not
semantics. `Type` is constructed directly as a wait-form whose
recognizer happens to accept Type's own tree shape — self-consistency
by construction, validated by an explicit test rather than enforced
by a special trust seed.

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

The Σ-algebra framing in §5 follows Plotkin & Power (2002),
"Notions of computation determine monads," and Plotkin & Pretnar
(2013), "Handlers of algebraic effects" (LMCS). Practical
implementations: Bauer & Pretnar (Eff), Leijen (Koka), Brachthäuser
et al. (Effekt).

*Disp's capability-passing model* is Effekt's: Brachthäuser, Schuster,
Ostermann (2020), "Effects as Capabilities: Effect Handlers and
Lightweight Effect Polymorphism" (OOPSLA, DOI 10.1145/3428194), with
the earlier "Effekt: Capability-Passing Style for Type- and
Effect-Safe, Extensible Effect Handlers in Scala" (JFP 2020). The key
move — handlers as second-class values whose lexical scope is the
handler scope — fits disp's no-implicit-state aesthetic better than
Koka's dynamic handler stack. Disp differs from Effekt in two ways:
(a) handlers are first-class values (records), so capabilities can
escape lexical scope and be stored in data structures (the
soundness consequence is that effects must be invoked via a
capability in scope, not implicitly); (b) the dispatch environment Σ
is itself a value passed to `safe_apply`, giving an additional layer
of control not present in Effekt.

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
  The kernel handler architecture is a Σ-parameterized Plotkin-Pretnar
  algebraic effect system; capability-passing for typed effects is
  Effekt's (Brachthäuser et al. 2020). Cubical operations follow the
  CCHM framework. The categorical foundations are standard topos
  theory.
]

= Appendix A: open questions and conjectures <sec:open-questions>

Inline `Open question:` blocks throughout the spec, collected here
for at-a-glance review. Each entry links back to where the question
is raised in detail.

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Where*], [*Question*],
    [§11 (Type:Type)],
      [Is `typecheck Type Type = Ok TT` enough to imply foundational
       consistency, or is a Hurkens-style encoding lurking? Argument
       is informal; needs a parametricity theorem, an I-shortcut
       characterization, or a semantic model. Fallback: ranked
       universes.],
    [§13 (HITs)],
      [Higher inductive types are sketched; constructor-path
       eliminators need their own follow-up document.],
    [§14 (formal soundness)],
      [No mechanized soundness proof exists; "empirically sound + a
       documented conjecture about consistency" is the current
       position.],
    [§14 (memo-stability test)],
      [Empirical verification needed that `wait self.handler meta`
       inside a handler body hash-cons-equals
       `wait kernel.handler meta` constructed externally. If they
       don't, H-rule tree_eq comparisons misbehave.],
    [§15 (higher-order effects)],
      [Effects with operations that take computation arguments
       (e.g. `catch handler body`) require Wu-Schrijvers-Hinze
       treatment beyond what capability passing covers.
       Operationally workable via tag-based encoding; static
       checking is open.],
    [§15 (stateful effects without refs)],
      [Buffered loggers, accumulators, and similar stateful
       handlers need explicit state-threading or a host `alloc_ref`
       primitive. Best practice undetermined.],
    [§15 (postulate migration)],
      [When a stricter postulate supersedes a looser one, library
       call sites need refactoring. A
       `--check-postulate-currency` lint that flags use of
       loose-when-strict-exists would help. Not yet implemented.],
  ),
  caption: [Open questions inventory.],
)

The spec is *not* blocked on any of these — the system is operational
without them — but each represents an honest gap worth tracking. The
foundational ones (Type:Type, formal soundness) are the load-bearing
items; the others are scoped.

*Resolved since previous iteration:*
- User-installable effects: §15 specifies the design (dispatch-
  environment entries; capability passing; postulates).
- Scoped vs. algebraic effects: lexical scope of capabilities is
  handler scope; no implicit dynamic stack needed.
- Closed vs. open Σ-algebra: Σ is a value passed to `safe_apply`,
  not a fixed compile-time set.

= Appendix B: where the tests live <sec:test-catalog>

The standard library's tests are defined *inline* with the entities
they exercise: kernel-primitive behavioral tests next to each
primitive's spec (§7), type-system tests next to each library type
(§12), cubical tests next to the cubical operations (§13). The
combined set is the spec's test suite; re-elaborating `lib/` runs all
of them, and a failing test halts elaboration at the failing
component.

The on-disk source of truth is the recursive set
`lib/tests/**/*.test.disp`, runnable as a whole via `npm test` (see
`test/disp.test.ts`). Effect-handler tests (postulate type-checks,
capability-passing application tests, mock-environment tests) land in
`lib/tests/effects/`.

This appendix exists to point at that organization; it does not
maintain a parallel test catalog. Any duplication would rot — the
inline tests are canonical.

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
- Brachthäuser, Schuster, Ostermann (2020). "Effects as Capabilities: Effect Handlers and Lightweight Effect Polymorphism." OOPSLA. DOI 10.1145/3428194.
- Brachthäuser, Schuster, Ostermann (2020). "Effekt: Capability-Passing Style for Type- and Effect-Safe, Extensible Effect Handlers in Scala." JFP 30.
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
