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
  - The kernel ships *3 Σ-operations + 1 parameterized dispatcher*.
    Σ-ops: `hyp_reduce`, `bind_hyp`, `postulate`. Dispatcher:
    `safe_apply Σ` — parameterized over a dispatch environment Σ.
    `param_apply := safe_apply default_dispatch`. `eliminator_frame`,
    in earlier drafts a fourth Σ-op, folds into `hyp_reduce` + a
    library `elim` (§12).
  - The walker consults pinned-sig sets derived from Σ: `seal(Σ)`
    (trusted-token producers — unforgeable *and* uninspectable) and
    `funnel(Σ)` (host sigs forced through `postulate`'s sanitizer —
    unforgeable only), with `forge(Σ) = seal(Σ) ∪ funnel(Σ)`. The
    pure kernel operations `bind_hyp` / `postulate` are in neither:
    the dispatcher routes pinned sigs to the *registered* handler
    (not a wait-form's embedded one), so forging their invocation is
    harmless and library recognizers may build those invocations
    under the walker.
  - Kernel and host primitives are *values, not registrations*.
    `default_dispatch = kernel_handlers ++ host_provided`. No mutable
    global registry.
  - Effects are unified with type-system primitives. Neutrals, stuck
    eliminations, and host IO are all dispatched through the same
    `safe_apply Σ` mechanism.
  - Type-checking is framed as manifest contracts over the
    `CheckerResult` monad. The elaborator is purely a wrap-only pass.
  - Library types carry MetaShape-conforming meta records with named
    fields (`recognizer_params`, `functor`, `respond`,
    `behavioral_specs`). `respond` is the universal "respond to an
    elimination frame" function, generalizing the earlier `applicable`.
  - Cubical operations (`transp`, `hcomp`, `comp`, `Glue`) live in the
    `functor` meta-field of each library type. Stuck `comp` reuses
    the one `hyp_reduce`-rooted neutral constructor.
  - The `bind_hyp` escape check is a single dependency scan `occurs`
    (§8.1) that descends *through* seals to find a hypothesis hidden in
    a derived neutral's spine; `fresh_for` and `is_closed` are its two
    policy uses (`bind_hyp` escape; `param_lift` / `postulate`
    closedness). Searching for one specific hypothesis distinguishes a
    legal in-scope reference from an extruded one, so the scan need not
    treat seals as opaque — the prior opaque scan let derived neutrals
    carry a hypothesis out of scope.
  - `strip` is a tree-level function gated by `validate`'s certificate.

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
trees. Three kernel Σ-operations ship in `kernel_handlers`:
`hyp_reduce`, `bind_hyp`, and `postulate`. The host exposes any
real-world primitives (IO, syscalls) as `host_provided : Σ`. The
default environment `default_dispatch := concat kernel_handlers
host_provided` is what `param_apply` evaluates against; stricter callers
(foundational tests, sandboxes) construct narrower environments. The
dispatcher routes by structural signature on hash-consed trees, so
dispatch is O(1) via tree-id comparison. Types, validators, recognizers,
case eliminators, and the MetaShape convention all live in the
library. Cubical operations sit in each type's `functor` metadata field
and reuse the kernel's one stuck-form constructor for their stuck
cases. The metacircular discipline holds: the type system is defined
in disp source as library code; the host only optimizes.

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
    [§2 Substrate], [Tree calculus, apply, hash-cons identity, glossary; record/array/coproduct sugar (§2)],
    [§3 The `CheckerResult` monad], [`Result E A`, `CheckerError` variants, Kleisli composition, the *verdict-vs-error principle*],
    [§4 The parametric walker and `Tree_p`], [Walker as Kleisli-lifted binary apply, `Tree_p` as greatest fixed point, soundness discipline],
    [§5 The dispatcher and dispatch environments], [Σ-algebra framing: handlers as values, environments as list-passing, openness via concatenation],
    [§6 Stuck forms and neutrals], [Stuck forms from any pinned handler, the generalized H-rule, cascading-failure story],
    [§7 The kernel primitives], [Operational semantics of `hyp_reduce`, `bind_hyp`, `postulate`, `safe_apply`],
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
    [`Functor`],      [Synonym for `Tree_p`; conventionally a morphism-action function consumed by `transp` (§13). Sentinel `trivial_functor` = "trivial Kan structure": `comp` returns `u0` for it (identity transport; discrete `hcomp` is the cap). Non-discrete types carry a real morphism action.],
    [`Respond`],      [`NeutralMeta -> Frame -> Action`. The universal "respond to an elimination frame" function carried by each type's meta. Generalizes the earlier `Applicable`.],
    [`Frame`],        [`Tree_p`. The thing applied to a neutral — an argument (Π), a projection selector (Σ), a case-pair (inductive), a dimension (Path), a candidate value (Type). Untagged; the stored type interprets it.],
    [`Action`],       [`Extend Type | Return Tree_p | Invalid`. The protocol `hyp_reduce` consumes from a type's `respond` (§7).],
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
  encoded position: `pair stored_type (pair spine ...)`. This *scaffolding*
  (the wait-form shape, neutral spines) is read positionally and ascribed
  by name once the relevant types exist; genuine `{…}` records instead
  carry their field names in a header (§2.6) and are read by name directly.
  We use the typed presentation throughout for readability; the
  implementation walks the structure either way.
]

== Records, arrays, and coproducts <sec:sugar>

Three notations recur throughout this document — record values and types in
braces, array literals in brackets, coproduct (variant) types with bars. All
three are surface sugar over the pairs of §2; their desugaring is fixed
here once so later sections use them without restating the encoding. (`≡`
below reads "desugars to.")

=== Arrays

```disp
[]              ≡  nil
[a, b, c]       ≡  cons a (cons b (cons c nil))
```

An array literal is a `List` (§2 glossary): `nil` is empty, `cons h t` prepends. At
the substrate these are the iterated-pair encoding (`nil = LEAF`,
`cons = pair`), so a list is a right-nested fork chain ending in `LEAF`.
Lists carry no names; position is the only index.

=== Record values

```disp
{ a := x, b := y, c := z }   ≡  pair [a, b, c] (pair x (pair y z))
                                       ^^^^^^^^^  ^^^^^^^^^^^^^^^^^
                                       names hdr   payload (a Σ-chain)
```

A record value carries its ordered field-name list in a header node
(`pair_fst`), with the field values as a right-nested payload tuple
(`pair_snd`). The header is one hash-consed node: records with the same
names in the same order share it, and `tree_eq` on two headers is O(1)
(§2.2). Names live in the *value*, not only in the type — so a recognizer
can decide field membership from the value alone, and a concrete record's
projection resolves against its own header at runtime (the type still
governs the *result type*; see §12).

This is the product dual of the coproduct encoding (`pair tag payload`,
below): a coproduct value tags *which one* it is; a record value names
*all the fields* it has. Both put a discriminating descriptor in
`pair_fst` and the data in `pair_snd`.

Projection is a `checked` dependent application, not a positional shortcut:

```disp
r.a   ≡   proj r "a"
```

`proj` reads the header (`names_of r = pair_fst r`), validates that `"a"`
is a present field (`ValidField (names_of r) "a"` — a Refinement decided
by kernel reduction, so a bad name fails as `Ok FF`, never a host error),
and returns the value at that field's position in the payload. For a
*literal* name the call `proj r "a"` is a subterm closed in `r`, so it
β-reduces — by ordinary `apply`, with no certificate — to the direct path
`pair_fst (pair_snd r)`; hash-cons shares that normal form
across every projection of the same field. §12 gives the dependent type of
`proj`, the neutral case, and how `strip` relates to it.

#note[
  *One record discipline, metadata included.* Every `{…}` record carries a
  header — metadata records too. A type's MetaShape meta
  (`{recognizer_params, functor, respond, …}`, §11.2) is a headered record,
  which is exactly why the kernel reads it by name: `meta_get m "respond"`
  *is* the projection `m.respond`, resolving the name against the value's
  own header with no field list threaded in. What stays positional is not
  metadata but the substrate *scaffolding* that is not a record at all — the
  `wait`-form tree shape and a neutral's spine, reached by fixed projections
  (`type_meta`, `neutral_meta_type`; the iterated-pair layout of §2's
  layering note). The header sits *inside* that scaffolding, on the meta
  record it carries. Coproducts split the same way: the interpreter's own
  enums hand-build minimal tags, while library and user coproducts tag by
  name.
]

=== Record types

```disp
{ a : A, b : B, c : C }   ≡  Record [(a, A), (b, B), (c, C)]
```

A record type is the `Record` former (§12) over the array of name/type
entries. `Record` recognizes a value by checking its name header against
the declared names (O(1)) and its payload against the underlying `Sigma`
chain — so a later field's type may depend on earlier fields
(`{ n : Nat, v : Vec n }`), and a value whose header names don't match is
rejected, distinguishing `{a:Nat,b:Nat}` from `{p:Nat,q:Nat}` at the value
level. Before §11 a record-type annotation is read positionally and
validated only once `Record` exists.

=== Coproducts

```disp
C := V1 T1 | V2 T2 | ... | Vn Tn
  ≡  C := Coproduct [(V1, T1), ..., (Vn, Tn)]
```

A coproduct is the `Coproduct` former (§12) over its constructors. A
constructor application `Vi e` builds `pair tag_i e` — a distinct tag tree in
`pair_fst`, the payload in `pair_snd` (a nullary constructor carries the unit
payload). Elimination dispatches on the tag:

```disp
match v {
  V1 x => b1
  ...
  Vn x => bn
}
```

compares `pair_fst v` against each `tag_i` and runs the matching branch on
`pair_snd v`. This one rule subsumes `select` (the two-constructor Bool
case), the per-type recursors of §12, and raw `triage` decomposition.

#note[
  *Tags and O(1) discrimination.* A coproduct's tag sits in `pair_fst`, so
  `match` discriminates by one `tree_eq` (O(1), §2.2). Library and user
  coproducts tag by interned constructor name. The kernel enums the
  interpreter itself returns — `Result` (`Ok` / `Err`, §3.4) and `Action`
  (`Extend` / `Return` / `Invalid`, §7) — pick *minimal* tag trees (`LEAF`,
  `stem LEAF`, …) so the comparison is against a boot-time constant; that is
  the only reason their trees are written by hand instead of produced by
  `Coproduct`. Same desugaring, different tag policy.

  `Bool` is the coproduct `True Unit | False Unit`; it keeps its Scott
  encoding (§12) because it is the substrate's branching primitive and must
  apply directly as `select`. That encoding optimizes the general
  eliminator — it is not a separate mechanism.
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
// All errors carry a source span for diagnostics. `span` is the ambient
// source span of the expression currently being reduced; the elaborator
// threads it through the reduction context, so the handler bodies in this
// spec write `span` without binding it explicitly.
// A coproduct (§2): each variant is a constructor carrying a record payload.
CheckerError :=
    Parametricity { kind : ParamKind, where : Tree_p, span : Span }
  | Escape        { hyp : Tree_p, body_result : Tree_p, span : Span }
  | NotApplicable { type : Tree_p, span : Span }
  | TypeMismatch  { expected : Type, actual : Tree_p, span : Span }
  | Malformed     { handler : Symbol, meta : Tree_p, span : Span }

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

#note[
  *Most of this vocabulary is meant to disappear.* Of the five variants, only
  `Parametricity` and `Escape` report a genuine soundness event — user code
  tried to forge or reflect on a hypothesis, or let one escape its scope. The
  other three describe a type that was *built* wrong, not a value that fails
  to inhabit it: `Malformed` (meta off-shape), `NotApplicable` (stored type
  not a function / not a type), and `TypeMismatch` (a contract promised `TT`
  and got `FF`). Those are discharged earlier — by validating every type
  annotation against `Type` at elaboration, and by the validate-then-strip
  discipline (§10) under which a checked contract can only fire on the
  un-validated path. On well-formed, validated input the checker never raises
  them, so the steady-state recognizer is effectively `Tree -> Tree -> Bool`:
  a verdict, with `Parametricity` / `Escape` the only residual error channel.

  *Folding `Err` to `FF` is conservative only for monotone recognizers.* When
  a recognizer runs a sub-check under the walker and folds its `Parametricity`
  failure to `Ok FF` (the kernel does this — §6), it *rejects* the value,
  which is sound. This stays sound exactly while recognizers are *monotone* in
  their sub-verdicts: a sub-check turning `FF` may only weaken the answer,
  never strengthen it. A recognizer that negated a sub-verdict
  (`{v} -> not (sub v)`) would turn a forgery's `FF` into `TT` and accept it.
  The library recognizers do not negate (Pi, Sigma, Refinement are
  conjunctive; `Not` is the type `Arrow A False`, not a verdict negation), so
  the fold is sound today; a recognizer-monotonicity check belongs in §14.
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

The walker `w_Σ : Tree × Tree → CheckerResult(Tree)` is the Kleisli
lift of binary `apply`, parameterized over *two pinned-sig sets*
derived from Σ:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Set*], [*Meaning*],
    [`seal(Σ)`],
      [Sigs whose handlers mint *trusted tokens* — values whose
       provenance other code believes (the H-rule, `is_neutral`, the
       escape scan). `hyp_reduce` in the default environment;
       extensible to any future stuck-form producer (e.g.
       `async_pending`, §15). These need *both* protections:
       unforgeable as values (construction) and opaque to inspection
       (triage).],
    [`funnel(Σ)`],
      [Host / effect sigs whose invocation must be routed through
       `postulate`'s sanitizer. These need *construction* protection
       only — so user code can't fabricate a host call that bypasses
       the no-neutral scan — but not inspection protection.],
    [`forge(Σ) := seal(Σ) ∪ funnel(Σ)`],
      [The construction-protected set the stem-forge clause consults.
       *Excludes* the pure kernel operations `bind_hyp` and
       `postulate` (see note): library code legitimately constructs
       their invocation wait-forms under the walker.],
  ),
  caption: [The pinned-sig sets the walker consults.],
)

Three clauses, following `apply`'s rules:

+ *Stem-forge.* `w_Σ(stem(a), x)` would reduce to `fork(a, x)`; if
  `tree_eq a (checker_sig h)` for some `h ∈ forge(Σ)`, return `Err
  (Parametricity { kind = StemForge, where = fork(a, x), span })`.
  Members of `forge(Σ)` can't be fabricated.

+ *Triage-on-seal.* `w_Σ(f, x)` would fire the triage rule on `x`; if
  `pair_fst(x) = checker_sig h` for some `h ∈ seal(Σ)`, return `Err
  (Parametricity { kind = TriageReflect, where = x, span })`. Seals
  can't be opened.

+ *I-shortcut.* `w_Σ(I_canonical, x) = Ok x` unconditionally, even
  when `x` is a hypothesis. Required so polymorphic identity passes
  Pi-checks against hypothesis-typed arguments. The sole soundness
  carve-out, Σ-independent.

All other applications follow `apply`'s rules and return `Ok <result>`.

#note[
  *Tokens vs. operations.* An earlier framing protected *every*
  handler sig against construction, justified as "forging a wait-form
  invokes privileged code with chosen inputs." That rationale
  presupposes the dispatcher trusting a wait-form's *embedded*
  handler; once the dispatcher routes pinned sigs to the *registered*
  handler instead (§5.4, §7.4), forging a wait-form can only
  re-invoke the genuine handler — which is built to accept arbitrary
  inputs (`bind_hyp` escape-checks, `postulate` no-neutral-scans). So
  construction protection is *not* needed for the kernel operations
  `bind_hyp` / `postulate`, and is in fact *harmful* there: library
  recognizers run under the walker (§6.3.1) and must construct
  `wait kernel.bind_hyp …` invocations to mint their hypotheses (Pi's
  body check, §12). What genuinely needs unforgeability is the *value*
  a stuck-form producer mints (a neutral is a token the H-rule
  trusts) and the *funnel* into a sanitizer (host calls). Hence
  `forge(Σ) = seal(Σ) ∪ funnel(Σ)`, excluding the operations.
  Inspection protection is narrower still: only `seal(Σ)`, whose
  minted tokens are introspection targets.
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

+ *Don't construct forks rooted at a `forge(Σ)` sig.* The stem rule
  rejects this — you can't fabricate a trusted token (`seal(Σ)`) or a
  host call (`funnel(Σ)`). Mint tokens via `bind_hyp` / `hyp_reduce`
  and build host calls via `postulate`. (Invocations of `bind_hyp` /
  `postulate` themselves are *not* forge-protected and may be
  constructed directly — the dispatcher routes them to the registered
  handler regardless, §5.4.)

+ *Define new type-formers as wait-forms.* Use the substrate's `wait`
  combinator with a library recognizer (§11). For inductive types
  that need stuck-elimination on neutrals, supply a `respond` field
  whose case-frame action returns `Extend (motive (reconstruct_self
  meta))`, then expose case dispatch via the library `elim` (§12).

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

Each handler in Σ is a *curried function* the dispatcher invokes as
`handler Σ meta arg`: the current dispatch environment Σ first (threaded
so the handler's own sub-evaluation stays in this environment), then its
structured meta record, then an argument from `Tree_p(Σ)`, producing a
result in the `CheckerResult` monad. The per-operation signatures in the
table below elide the leading Σ; handlers that sub-evaluate nothing
(`hyp_reduce`, `postulate`) ignore it. The meta record is what gets
baked into a wait-form (`wait handler meta`, see §5.4); applying the
wait-form to an argument triggers dispatch via `safe_apply`, which
supplies Σ.

The three kernel-shipped Σ-operations are:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Symbol*], [*Type*],
    [`hyp_reduce` (Σ-op)],
      [`NeutralMeta → Frame → CheckerResult(Tree_p)` — the universal
       "push a frame onto a neutral" engine; consults the stored
       type's `respond` field],
    [`bind_hyp` (Σ-op)],
      [`(T : Type) → ((h : T) → CheckerResult (Pub h R)) → CheckerResult R`
       — mints a fresh seal, runs the body, and propagates its
       `CheckerResult` after checking the result is fresh for the seal
       (§7.2)],
    [`postulate` (Σ-op)],
      [`{sig : Tree} → Tree_p → CheckerResult(Tree_p)` — mints a
       wait-form rooted at a handler sig in the current Σ; the bridge
       from user code to non-self-dispatching handlers],
    [`safe_apply` (dispatcher)],
      [`List Tree → Tree_p → Tree_p → CheckerResult(Tree_p)` —
       Σ-parameterized substrate-apply with a privilege check],
  ),
  caption: [The kernel surface: three Σ-operations plus the
    parameterized dispatcher.],
)

#note[
  *Bootstrap.* The records above are typed once §11's library types
  are in scope. At the bootstrap layer they are physically nested
  pairs: `bind_hyp`'s meta is `pair domain body`, etc. Multi-arg
  operations collect their fields across partial-app steps using an
  internal arity counter in the wait-form payload.
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

The kernel ships its three Σ-operation handlers as a top-level binding.
The named-record view `kernel` and the list `kernel_handlers` hold the
*same* handler trees — so a `kernel.hyp_reduce`-rooted neutral routes to,
and is recognized against, exactly the `q_hyp_reduce_fn` entry in Σ
(handlers reference one another through `kernel.…`, tied as a mutually
recursive record):

```disp
let kernel : { hyp_reduce, bind_hyp, postulate } := {
  hyp_reduce := q_hyp_reduce_fn,
  bind_hyp   := q_bind_hyp_fn,
  postulate  := q_postulate_fn
}
let kernel_handlers : List Tree := [
  kernel.hyp_reduce,           // = q_hyp_reduce_fn
  kernel.bind_hyp,
  kernel.postulate
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
  if ∃! h ∈ Σ. tree_eq (pair_fst f) (checker_sig h)  → h Σ ⟨wait_meta f⟩ x
                                                          // registered handler (not f's embedded one),
                                                          // given Σ + recovered meta + arg; its
                                                          // CheckerResult is the result (no extra Ok)
  else                                                 → walker step (§4)
```

*The dispatcher routes to the registered handler.* When `pair_fst f`
matches the sig of the (unique) member `h ∈ Σ`, the dispatcher
recovers the wait-form's meta `m = wait_meta f` (the *meta
accessibility* property below) and evaluates `h Σ m x` for *that
registered handler* — it does *not* trust the handler `f` happens to
embed, and it never raw-reduces `f` itself. Σ is threaded so the
handler's own sub-evaluation stays in the current environment. For a
genuine wait-form `f = wait h m` the registered handler is the embedded
one, so the call is the intended `h Σ m x`; for a forgery carrying the
right sig but a different embedded handler, the registered `h` runs
anyway, so a forged routing cannot execute attacker-chosen code. This closes the
trust-set-vs-routing-table gap (see `SCOPE_VERIFICATION_INVESTIGATION.md`):
privilege is granted to a *sig*, and the sig names a *registered
handler*, not whatever tree presents that sig.

So Σ is a genuine routing table, not merely a trust set. Soundness
needs only that *registered* handlers have distinct sigs (`checker_sig`
injective on Σ — a finite, checkable property asserted when Σ is
built), not the far stronger "no user-reachable tree shares a trusted
sig." See §7's `safe_apply` entry for the in-language reference.

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
  The sig also *keys the routing table*: the dispatcher looks it up in
  Σ and runs that registered handler on the wait-form's recovered meta
  (§5.4, §7.4), rather than trusting the handler `f` embeds. A directly
  partially-applied function would lack a stable signature to gate
  privilege on or to key the table by.

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
- Library types (`Pi`, `Sigma`, `Bool`, etc.) are derived terms — wait-forms over library recognizers. Inductive types supply a `respond` field whose case-frame action mints stuck eliminations through `hyp_reduce`; the library `elim` (§12) handles the concrete/neutral gate.

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
    [`bind_hyp`], [Mint a fresh seal for an unknown of given type.],
    [`hyp_reduce`], [Push a frame onto a neutral; consult the stored type's `respond`.],
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

A *stuck form* is a `hyp_reduce`-rooted tree of shape
`wait kernel.hyp_reduce (make_neutral_meta T payload)` representing
"a computation whose value is unknown until later." Two operational
roles use the same constructor:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Role*], [*Origin*],
    [Hypothesis], [`bind_hyp` (payload = `(domain, body)`, so a
       hypothesis is determined by its type and the body it is bound
       in — fresh per distinct binder, deterministic under hash-cons)],
    [Spine-extended neutral / stuck elimination],
      [`hyp_reduce` (extends an existing neutral with a frame;
       payload = `(old_meta, frame)` where the frame may be an
       applied arg, a projection selector, or a case-frame
       `(motive, cases)`)],
  ),
  caption: [Origins of stuck forms — one constructor.],
)

All stuck forms are *handler-rooted*: `pair_fst = checker_sig
hyp_reduce`. The walker rejects user-side construction via its
stem-forge rule; only the kernel mints them. `StuckElim` and `Hyp`
are aliases at the tree level — and, like `bind_hyp`, they are
*kernel-privileged* constructors (trusted base), not walker-buildable
user code. Tests that need a bare hypothesis to probe a recognizer or
`respond` (e.g. `bool_recognizer bool_meta (mint_hyp_form Bool)`, §12)
use these privileged minters and call the recognizer *directly* — they
do not go through `typecheck`, whose `param_lift` would reject any
public neutral (§8.3). `mint_hyp_form` is the test-only spelling of
`Hyp`.

#note[
  *Generalization.* In principle a custom handler can symbolic-defer
  on neutral input and contribute another stuck-form kind; the H-rule
  generalization in §11 handles such forms uniformly. The single-
  constructor view above is descriptive of the default environment.
]

== How stuck forms propagate

Stuck forms thread through computation; subsequent operations on
them extend the stuck-ness rather than reducing to concrete values.

*Pushing a frame*: `apply(neutral, frame)` routes through
`hyp_reduce`. The handler consults the stored type's `respond` field
and either extends the spine (`Extend T'` — new stored type `T'`,
frame appended) or yields a value (`Return v` — short-circuit).
Per-type behavior:

- *Π-typed neutral, frame = argument*: `Extend (B frame)`.
- *Σ-typed neutral, frame = projection selector*: `Extend A` or
  `Extend (B (apply self walker_pair_fst))`.
- *Inductive (Bool/Nat/…) neutral, frame = `(pair motive cases)`*:
  `Extend (motive (reconstruct_self meta))`; the library `elim` (§12)
  drives the concrete-case side.
- *Type-typed neutral, frame = candidate value*: `Return (and
  (is_neutral frame) (tree_eq (stuck_stored_type frame)
  (reconstruct_self meta)))` — the predicate-side H-rule (§12.17).
- *Non-applicable type (e.g. raw `I`)*: `respond = none`; the
  neutral is inert and `hyp_reduce` records `InvalidType`.

*Library elimination* (`elim`, §12) gates on `is_neutral target`:
the concrete branch runs the dispatcher; the neutral branch routes
the case-frame through `hyp_reduce` as above.

*Triage on a stuck form*: walker-rejected (`TriageReflect`, §4.2).
Library `safe_*` helpers go through `elim` to give well-defined
stuck-Bool / stuck-Tree results.

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

*Source.* §12.17, `type_predicate_h_rule`.

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

Operational semantics for the three Σ-operations of §5 plus the
parameterized dispatcher `safe_apply`. The kernel surface is small by
design: only operations requiring privileged construction live here.
Type recognition, case eliminators, typed function application, and
most type-system machinery live in the library (§12).

#note[
  Each handler is invoked from inside some `safe_apply Σ` call as
  `handler Σ meta arg` (§5.4). The dispatcher passes the current Σ
  explicitly, so a handler's own sub-evaluation (`safe_apply Σ …`)
  stays in the same environment. Under `test_pure` that Σ is
  `kernel_handlers`; under `default_dispatch` it's the full
  environment. Sibling Σ-ops are referenced by their canonical
  `kernel.…` names (the mutually recursive `kernel` record, §5.3), so
  every `kernel.hyp_reduce`-rooted neutral is the one canonical tree. The
  earlier `(ks, raw, query)` self-proxy is gone: explicit Σ-passing
  supplies what `ks.param_apply` did, and the `kernel.…` names supply
  what `raw.field` did.
]

== `hyp_reduce`

*Signature.* `hyp_reduce : NeutralMeta → Frame → CheckerResult(Tree_p)`.

*Role.* The universal "push a frame onto a neutral" engine. When a
neutral is applied to a frame, the dispatcher routes here; the handler
consults the stored type's `respond` field (§11.2) and either extends
the spine (`Extend`) or yields a value (`Return`).

*Disp source:*

```disp
let q_hyp_reduce_fn = {Σ, meta, frame} -> {     // Σ unused: hyp_reduce sub-evaluates nothing
  let stored  = neutral_meta_type meta
  let respond = meta_get (type_meta stored) "respond"
  // A spine extension roots at the *registered* handler `kernel.hyp_reduce`
  // — the very tree held in Σ, so the result both routes (dispatch) and
  // answers `is_neutral` — and embeds the PREDECESSOR NEUTRAL
  // `reconstruct_self meta` (= `wait kernel.hyp_reduce meta`, §12.2), NOT its
  // bare metadata. The embedding is load-bearing for the escape scan: §8.1's
  // `support` inserts metadata only at neutral roots, so a hypothesis hidden
  // in a spine is reached only if the predecessor it sits under is itself a
  // recognizable neutral. (No `fix`/`self` is needed: extensions are rooted by
  // *name* at the one canonical `kernel.hyp_reduce`, which is also what makes
  // the §14 hash-cons question moot — there is a single root tree.)
  // Returns are `Ok`-wrapped: like every dispatch target, `hyp_reduce`
  // hands back a `CheckerResult` (§7.5 wrapping invariant), so the
  // dispatcher passes it through un-nested rather than re-wrapping.
  let invalid = Ok (wait kernel.hyp_reduce (extend_neutral_meta (reconstruct_self meta) InvalidType frame))
  match (is_some respond) {
    FF => invalid
    TT => (
      let action = (unwrap respond) meta frame
      match (is_extend action) {
        TT => Ok (wait kernel.hyp_reduce (extend_neutral_meta (reconstruct_self meta) (pair_snd action) frame))
        FF => match (is_return action) {
          TT => Ok (pair_snd action)
          FF => invalid
        }
      }
    )
  }
}
```

*Soundness obligation.* `respond` runs raw (outside the walker). Each
type former's `Return v` channel must be fed only by public-derived
data — the local DCC `[BindM]` discipline. `Extend` and `Invalid` are
unconditionally safe.

== `bind_hyp`

*Signature.* `bind_hyp : (T : Type) → ((h : T) → CheckerResult (Pub h R)) → CheckerResult R`.
The body returns a `CheckerResult`; `bind_hyp` is a Kleisli `bind` that
runs the body, escape-checks the exposed `Pub h R` payload, and re-wraps
the fresh result as a `CheckerResult R` — see the wrapping note in the
source below.

*Role.* Mint a fresh hypothesis of type `T`, run the body, and ensure
the result does not depend on the hypothesis — so it cannot leak out of
the body, where it would have no binder.

*Disp source:*

```disp
let q_bind_hyp_fn = {Σ, domain, body} -> {
  let h = wait kernel.hyp_reduce (make_neutral_meta domain (t domain body))
  let h_use = match (is_pi domain) {
    TT => checked domain h        // wrap with the *full* function type, so applying
    FF => h                       // h_use checks its arg against `pi_dom domain` (§8, §12)
  }
  // safe_apply Σ runs the body under the *current* environment, so a
  // body-check fired under test_pure (Σ = kernel_handlers) stays
  // host-independent (§14). Per the §7.5 wrapping invariant, safe_apply
  // returns the body's CheckerResult un-nested, so `bind` exposes the
  // body's own payload (a `Pub h R` value); we escape-check it and
  // re-wrap with `Ok` to hand back a `CheckerResult R`.
  bind (safe_apply Σ body h_use) ({result} -> (
    match (occurs h result) {
      TT => Err (Escape { hyp = h, body_result = result, span })
      FF => Ok result
    }
  ))
}
```

*Escape rule.* The result must not depend on `h`. `occurs` (§8.1)
searches the whole result — descending *through* neutrals — for `h`'s
metadata, which is what every value derived from `h` carries in its
spine. A hypothesis hidden inside a stuck elimination is found and
rejected, not waved through:

```disp
bind_hyp Nat ({x} -> Ok 0)                       // ✓ result doesn't depend on x
bind_hyp Nat ({x} -> Ok (apply x 0))             // ✗ apply x 0 is a stuck elim carrying x
bind_hyp Nat ({x} -> Ok x)                       // ✗ x at public position
bind_hyp Nat ({x} -> Ok (pair x 0))              // ✗ x in user-constructed pair
bind_hyp Nat ({x} -> Ok (checked Nat x))         // ✗ x in checked wrapper
bind_hyp Nat ({x} -> Ok (host_write_stdout (to_string x)))   // ✗ x in host payload
```

*The `Pub` modality.* `Pub h R := Refinement R ({v} -> fresh_for h v)`
— the sealing modality as an ordinary library refinement (§12). The
runtime scan is the certificate for the typed claim; under the
per-type-former `respond` discipline (§11.2) it is sound by
construction and erasable at strip time.

#note[
  *Dependent hypotheses scan correctly.* When one hypothesis's type is
  another — `bind_hyp Type ({A} -> bind_hyp A ({x} -> …))` — `x`'s
  metadata contains `A`, so `x` depends on both. Releasing `x` (inner)
  searches for `x`: a result that mentions `A` but not `x` passes,
  since `A` is still in scope. Releasing `A` (outer) searches for `A`:
  any value of type `A` that tried to escape carries `A` in its
  metadata and is caught. Searching for one specific hypothesis, while
  descending through all structure, is what distinguishes a reference
  to a hypothesis still in scope from one being smuggled out — so the
  scan can go through seals without ever flagging a legal in-scope
  reference.
]

== Eliminators are library code, not a primitive

Case dispatch lives in the library — the generic `elim` of §12. `elim`
gates on its target: a concrete value runs the cases directly; a neutral
target is handed to `hyp_reduce`, which consults the inductive type's
`respond`. For a case-frame `(pair motive cases)` that `respond` returns
`Extend (motive (reconstruct_self meta))`, minting the stuck elimination.
So the neutral path reuses the single `hyp_reduce` primitive and the
concrete path is ordinary library code — no dedicated eliminator primitive
is needed. `StuckElim` and `Hyp` are tree-level aliases: both are
`wait kernel.hyp_reduce (make_neutral_meta T payload)`.

== `postulate`

*Signature.* `postulate : {sig : Tree} -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* The kernel-mediated constructor for wait-forms rooted at a
*funnel sig* — a host / effect handler in `funnel(Σ)` (§4.2). User
code can't construct such trees directly: the walker's stem-forge
clause rejects fabricating a `forge(Σ)` root, so every host call is
forced through this one sanitizing route. `postulate` runs in the
privileged arm and may mint the otherwise-forbidden fork shape — but
*only for `funnel(Σ)` targets*. Targeting a `seal(Σ)` sig (which would
forge a trusted token, e.g. a neutral) is refused: `postulate` is a
bridge to effects, never a back door to the seal set.
Each library postulate `host_op : T := postulate sig` produces a
`checked T (call_via_postulate sig)` value; the `checked` wrap (§8,
§12) enforces input validation, and `postulate`'s handler validates
that the payload contains no neutrals before constructing.

*Gloss.* "Build a wait-form rooted at `sig`, paired with the given
payload, on behalf of user code. Refuse if `sig` is a seal sig (no
forging tokens) or if the payload contains neutrals (would smuggle a
hyp to a host handler). This is the only path by which user code
produces a `funnel(Σ)`-rooted fork shape, because the walker rejects
all other paths to it. (The kernel operations `bind_hyp` / `postulate`
themselves are *not* construction-protected — their invocation
wait-forms are built by ordinary library code under the walker and
routed to the registered handler, §5.4.)"

The disp library is responsible for the *type ascription* T. The host
provides only the handler tree (the entry in `host_provided : Σ`);
disp source code asserts what type that handler implements. Multiple
postulates can target the same sig with different (typically
incrementally stricter) type ascriptions; users pick the one matching
their proof obligations at the call site.

*Disp source (sketch):*

```disp
let q_postulate_fn = {Σ, meta, payload} -> {    // Σ unused
    let sig = pair_snd meta             // the target handler sig
    // Reject seal-sig targets: postulate bridges to effects, never
    // forges a trusted token (e.g. a hyp_reduce-rooted neutral).
    match (is_seal_sig sig) {
      TT => Err (Malformed { handler = "postulate", meta = sig, span })
      FF =>
        // Sanitize: payload must not contain forged neutrals or
        // hypothesis leakage. The host primitive must not see them.
        match (scan_no_neutral payload) {
          FF => Err (Malformed { handler = "postulate", meta = payload, span })
          TT => Ok (wait_with_sig sig payload)
        }
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
reduction (for everything else). On the raw arm it *routes to the
registered handler* for the matched sig — the member of Σ, not the
handler the wait-form happens to embed (§5.4) — so privilege is granted
to a sig and the sig names a registered handler.

*Gloss.* "Look at `f x`. If `f`'s sig is in Σ, run the registered
handler raw (it's privileged). Otherwise, run the walker — apply
substrate reduction rules, but reject any rule that would forge a
pinned sig or triage on a neutral. Either way, sub-evaluations recurse
with the same Σ."

*`param_apply` is the default-instance name.* The kernel ships
`default_dispatch : List Tree` (the concatenation of `kernel_handlers`
and `host_provided`); `param_apply := safe_apply default_dispatch` is
the version used by `typecheck`, by tests, and by every implicit
reduction in this spec. Stricter callers (foundational tests,
sandboxes) use `safe_apply Σ_strict` with a narrower Σ.

```disp
// Routing lookup on Σ. Linear scan; each comparison is O(1) hash-cons
// id. Returns the registered handler for `sig`, or None if unpinned.
// `wait_meta` is the meta-recovery of §5.4 (the "meta accessibility"
// property): for a genuine `wait k m` it returns `m`.
lookup_handler := {Σ, sig} ->
  list_find ({h} -> tree_eq sig (checker_sig h)) Σ
```

*The dispatcher.*

```disp
safe_apply := fix ({self, Σ, f, x} ->
  match (and (is_wait_form f) (is_some (lookup_handler Σ (pair_fst f)))) {
    TT => (
      let h = unwrap (lookup_handler Σ (pair_fst f))
      h Σ (wait_meta f) x)                     // registered handler: Σ, recovered meta, arg —
                                               // already returns a CheckerResult, passed through un-nested (§7.5)
    FF => walker_step Σ (self Σ) f x          // walker (§4), parameterized by Σ
  })
```

The two arms have different semantics:

- *Routed arm.* The dispatcher runs the *registered* handler `h ∈ Σ`,
  supplying the current environment Σ, the wait-form's recovered meta,
  and the argument: `h Σ (wait_meta f) x`, in raw (non-walker)
  reduction. Threading Σ keeps the handler's sub-evaluation in the
  *same* environment (so a body-check under `test_pure` stays
  host-independent, §14). The dispatcher never raw-reduces `f` itself:
  for a genuine `f = wait h m` the registered handler equals the
  embedded one, and for a forgery presenting the sig with a different
  embedded handler the *registered* `h` runs regardless (§5.4). The
  handler's body, running raw, can do operations the walker would
  reject — minting neutrals (`bind_hyp`), extending spines
  (`hyp_reduce`), constructing funnel-sig-rooted wait-forms via
  `postulate`. Because the routed handler is the *registered* one,
  forging the *invocation* of a non-token operation (`bind_hyp`,
  `postulate`) is harmless: it can only re-enter the genuine,
  self-defending handler.

- *Walker arm.* `walker_step` is the parametricity-enforcing
  reduction of §4, also parameterized by Σ. Its internal sub-applies
  re-enter `self Σ` (the same `safe_apply` with the same environment)
  rather than calling substrate apply directly, so dispatch happens
  at every layer of nested reduction. Any sub-tree that happens to be
  a Σ-pinned wait-form gets routed back to the raw arm.

#note[
  *The wrapping invariant.* `safe_apply Σ f x` evaluates the application
  `f x` to normal form and returns that normal form *directly*; the only
  `CheckerError` it introduces itself is `Err Parametricity`, when a
  reduction step trips a walker rejection. It never adds an `Ok` of its
  own. Two consequences:
  - A *pure* reduction returns its bare result tree —
    `safe_apply … (add 2 3) → 5`, not `Ok 5` (so arithmetic tests like
    `test (add 2 3) = 5` hold).
  - A reduction whose head is a `CheckerResult`-producer — a library
    recognizer, a Σ-handler, or `checked_apply` — returns that producer's
    `CheckerResult` as the normal form: `param_apply Bool TT → Ok TT`,
    *not* `Ok (Ok TT)`.

  So `safe_apply` results are *never* nested (`Ok (Ok _)` / `Ok (Err _)`
  do not arise): every dispatch target already returns a `CheckerResult`,
  and the dispatcher returns it verbatim (the routed arm is `h Σ
  (wait_meta f) x`, with no wrapping `Ok`). This is why the three
  handlers share one return convention — `hyp_reduce`'s spine extensions
  are `Ok`-wrapped (§7.1) just as `bind_hyp` / `postulate` already wrap
  their verdicts and errors — and why `bind_hyp` re-wraps its
  escape-checked result as `Ok result` (§7.2) rather than relying on a
  second `Ok` from the dispatcher.
]

*Default dispatch environment.* The kernel's standard top-level
bindings:

```disp
let kernel_handlers : List Tree := [
  q_hyp_reduce_fn,
  q_bind_hyp_fn,
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
`CheckerResult` rests on one scan (`occurs`) and one wrap
(`checked`). `param_lift`, `typecheck`, and `validate` build on these
to turn the boundary into the user-facing query and certificate-
issuance APIs.

== `support`, `occurs`, and `fresh_for` — the dependency set

A `bind_hyp` body must not let its hypothesis leak out. The check is a
single question: does the result *depend on* the hypothesis? A value
depends on a hypothesis when the hypothesis appears anywhere in its
tree — and that includes inside the metadata of *other* neutrals. When
a hypothesis is eliminated (a function-hypothesis applied, a pair-
hypothesis projected), `hyp_reduce` builds a stuck elimination whose
spine embeds the *predecessor neutral* it came from (the same way an
eliminator stuck-form embeds its target). So a value can carry a
hypothesis without *being* it — the hypothesis sits inside a derived
neutral — but every hypothesis it depends on is still reachable as a
neutral somewhere in its tree.

The *support* of a value is the set of those hypotheses, collected by
walking the value, recognizing each neutral by its root signature, and
descending through its metadata:

```disp
// support v — the hypotheses v depends on, as their metadata. At each
// neutral, collect its metadata (type_meta) and descend into it; because
// a stuck elimination embeds the predecessor neutral it was built from
// (see hyp_reduce), the descent reaches the whole dependency chain,
// including hypotheses nested inside other neutrals' spines.
support := fix ({self, v} ->
  match (is_neutral v) {
    TT => set_insert (type_meta v) (self (type_meta v))
    FF => triage set_empty ({c} -> self c)
                 ({l, r} -> set_union (self l) (self r)) v
  })

// Membership compares metadata (type_meta), which is recovered identically
// however a neutral was reconstructed — so the check never depends on
// handler-tree identity, only on the metadata threaded verbatim.
occurs    := {h, v} -> set_member (type_meta h) (support v)
fresh_for := {h, v} -> not (occurs h v)
is_closed := {v}    -> set_is_empty (support v)
```

Two policies, one set:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Call site*], [*Policy*],
    [`bind_hyp` escape],
      [`fresh_for h result` — this hypothesis can't cross the
       kernel→user return],
    [`param_lift` / `postulate`],
      [`is_closed v` — no hypothesis crosses to the user or the host],
  ),
  caption: [Two boundary checks, one set.],
)

The walk does *not* stop at neutrals. That is the whole correction: a
stuck elimination is itself a neutral, so a scan that halted at neutral
roots would stop *before* reaching the hypothesis embedded in its
spine — precisely the values that carry a hypothesis out of scope. This
is verified in the implementation: `bind_hyp (Pi Nat ({_} -> Bool))
({h} -> h 0)` returns `Fail`, because `h 0` is a stuck elimination
whose spine holds `h` (`lib/tests/bind_hyp.test.disp`); the prior
seal-stopping scan returned the leaked neutral instead.

`support v` is a pure function of `v`'s structure, so the runtime
memoizes it per tree id (the same hash-cons-keyed `apply` memo that
makes `tree_eq` O(1)); equal subtrees share their support and the
boundary checks read the cache rather than re-walking. Membership on
metadata — not on the neutral itself — is what lets the embedded
predecessor (`wait self meta`, built inside the handler) match the
hypothesis it stands for (`wait kernel.hyp_reduce meta`, minted by
`bind_hyp`) without depending on those two wait-forms being identical.
(They are in fact identical here — `self ≡ kernel.hyp_reduce`, the one
canonical root — but the check does not rely on that.)
Because the set is keyed on what a value *depends on* rather than on a
binder stack, it also survives stuck-form producers whose lifetimes are
not stack-disciplined — async results, multi-shot continuations — which
a de Bruijn level counter would not.

== `param_lift`

The boundary sanitizer, in terms of `is_closed`:

```disp
param_lift := {v} ->
  match (is_closed v) {
    TT => Ok v
    FF => Err (Malformed { handler = "param_lift", meta = v, span })
  }
```

Inside `param_lift`'s output, you have sanitary trees; outside, you
have raw trees that might carry a neutral at a public position —
whether a stray kernel-minted token or (in a misconfigured Σ) a
forged one. Either is rejected at this boundary.

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

#note[
  *`typecheck` sanitizes; `param_apply` recognizes.* `param_lift`'s
  scan errors on *any* neutral in `v`'s public skeleton, so the
  recognizer-side H-rule (§6.3.1, §12 `make_recognizer`) — which fires
  precisely when a recognizer is applied to a neutral — is *never
  reached through `typecheck`*. That is intentional, not a dead path:
  `typecheck` is the sanitizing entry for *closed* user queries, where
  a public neutral can only be adversarial or confused. The H-rule is
  reached through `param_apply` (the internal recognition primitive),
  which the Pi/Sigma body-checks invoke *without* the sanitizing scan:
  e.g. Pi's `param_apply (B hyp) result` (§12) hands the recognizer a
  result that legitimately contains the freshly-minted `hyp`. The
  recognizer's H-rule fires there. Tests that exercise the H-rule
  directly (`bool_recognizer bool_meta (mint_hyp_form Bool) = Ok TT`,
  §12) likewise call the recognizer through `param_apply`, not
  `typecheck`. A caller that genuinely needs to ask "does this
  neutral-bearing `v` inhabit `T`?" uses `param_apply T v` directly,
  accepting that it has stepped inside the trusted recognition layer
  rather than the sanitizing boundary.
]

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
    [`checked T v`], [`wait checked_apply {ascribed_type:=T, value:=v}`], [Direct construction; no validation],
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
- `bogus` evaluates to `wait checked_apply {ascribed_type:=Bool, value:=zero}`.
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
→ checked_apply {ascribed_type:=Pi A B, value:=f} x   // wait-form reduction: checked_apply's
                                           // sig is a library sig, not in Σ, so no
                                           // special routing — wait reduces directly
→ handler runs
→ bind (param_apply A x) ({verdict} -> match verdict {
    TT => Ok (param_apply f x)   // checked_apply wraps; `param_apply f x` is bare (§7.5)
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

`test` is an elaborator keyword, not a function. `test expr`
*evaluates* `expr` — the substrate's `apply` reduction, with the
dispatcher `safe_apply default_dispatch` standing in for `apply` at
every application step — and asserts the normal form equals `TT`.
Evaluation just *is* reduction: there is no separate "un-reduced
expression" object, so `test` takes an expression to reduce, not a
tree handed to a partially-applied dispatcher. (Tree calculus is
Turing-complete, so reduction need not halt in general; tests are
expected on terminating expressions.) If `expr` reduces to anything
else (FF, an `Err` value, a stuck form), the elaborator throws,
reporting the failing expression and its actual reduction.

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

Pure-test discipline is opt-in via `test_pure`, a sibling elaborator
directive that evaluates its expression with `safe_apply
kernel_handlers` (rather than `default_dispatch`) standing in for
`apply` — the same reduction, under a stricter environment. Like
`test`, it is a directive that takes an expression to reduce, *not* a
function applied to a tree:

```disp
// The elaborator switches the reduction environment for the body:
test_pure (add 2 3) = 5
test_pure (typecheck Type Bool) = Ok TT
```

Foundational and conversion tests (anything underwriting the type
system's soundness story) should use `test_pure`. Behavioral
tests of effectful library code use the default `test`. Users elaborating
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
    TT => self ((type_meta t).value)      // unwrap (read the `value` field), recurse into inner
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
// Safe usage: validate first, strip the certified result. `validate`
// yields `Ok (Some cert)` / `Ok None`, so match the Optional and strip
// the `checked` wait-form `cert` it carries.
strip_validated := {T, v} ->
  bind (validate T v) ({opt} ->
    match opt {
      Some cert => Ok (strip ((type_meta cert).value))
      None      => Err (TypeMismatch { expected = T, actual = v, span })
    })
```

The certificate `cert = checked T v` (carried by `validate`'s `Some`)
attests that `typecheck T v = Ok TT` was true at construction time.
Strip on the inner `v` of a certificate is sound: every internal
`checked` wait-form's check has already been validated at
certificate-construction time.

== Connection to proof-carrying code

This is Necula's proof-carrying code pattern (Necula 1997). The
elaborator/host plays the *certifier* role: it validates the program
once by running `typecheck`. The certificate (the `checked T v`
wait-form returned in `validate`'s `Some`) attests that validation
happened. The strip pass plays the
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

```disp
// Helper synonyms:
//
//   Functor := Tree_p                  // Kan-method for transport (§13)
//   Respond := NeutralMeta -> Frame -> Action
//                                       // response to an elimination
//                                       //   frame applied to a neutral
//                                       //   of this type
//   Action  := Extend Type | Return Tree_p | Invalid
//   Frame   := Tree_p                   // untagged; the stored type
//                                       //   interprets it

MetaShape := Refinement
  (Record [
    ("recognizer_params", Tree),
    ("functor",  Functor),
    ("respond",  Optional Respond),
    ("behavioral_specs", Optional (List Path))
  ])
  ({_} -> Ok TT)   // structural membership suffices; the per-field types
                   //   shown are the *intended* ascriptions. §12.5 realizes
                   //   MetaShape with these fields loosened to `Tree`, with
                   //   deep field-typing deferred to `StrictType`.
```

A MetaShape value is an ordinary headered record (§2.6), so `meta_get m
"respond"` is the projection `m.respond` — the name resolves against the
value's own header, no field list needed.

`respond` is the universal "respond to an elimination frame" function.
The stored type determines how the frame is interpreted; frames are
untagged because one type accepts one frame kind:

- *Π*: frame is an argument `a`; `Extend (B a)`.
- *Σ*: frame is a projection selector (`walker_pair_fst` /
  `walker_pair_snd`); `Extend A` or `Extend (B (apply self
  walker_pair_fst))`.
- *Inductive*: frame is `(pair motive cases)`; `Extend (motive
  (reconstruct_self meta))`.
- *Path*: frame is a dimension `i`; `Extend (P i)`.
- *Type*: frame is a candidate value `v`; `Return (and (is_neutral v)
  (tree_eq (stuck_stored_type v) (reconstruct_self meta)))` — the
  predicate-side H-rule.

A type with `respond = none` is inert under elimination (e.g., `I`,
`IsOne`). The convention is extensible: existing types whose meta
lacks a new field still pass structural checks.

#note[
  *Naming.* The earlier `applicable` field is renamed `respond` to
  reflect that it handles every elimination form, not only function
  application. The narrower `is_pi` / `pi_dom` (used for `checked`
  wrapping) is a separate convention.
]

#note[
  *Two ways to read `respond`.* Operationally it is the per-type *handler
  clause* for one operation, "eliminate": a neutral is a stuck operation, its
  spine is the captured continuation, and `respond` says how that operation
  reacts to a frame — stay stuck at a new type (`Extend`), resolve to a value
  (`Return`), or reject (`Invalid`). This is the same shape as a `postulate`
  handler interpreting a host effect (§7, §15), which is why neutrals, stuck
  eliminations, and IO share one dispatcher. Formally it is the structure map
  of a coalgebra on `Tree_p` (itself a greatest fixed point, §4): a typed
  transition system whose states are neutrals carrying their stored type,
  whose labels are frames, and whose steps are `Extend` (advance to a new
  typed state), `Return` (halt with a value), and `Invalid` (dead state). The
  §8.1 escape scan is reachability in that system. Both readings agree; the
  handler reading keeps `respond` beside the effect story, the coalgebra
  reading pins down what "responds to every elimination form" means.
]

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
`respond := some type_predicate_h_rule` (the predicate-side
H-rule, §6.3 and §12.17), `behavioral_specs := none`.

The §11.6 argument depends on this contract *plus* the predicate-side
H-rule: `Type` is a wait-form with a structural recognizer, a
MetaShape-conforming meta, and a respond that returns
`Return (tree_eq (stuck_stored_type v) self_as_hyp)` for neutral
`v` and `Return FF` otherwise. The actual recognizer body and
H-rule are §12.17's responsibility; readers can verify they satisfy
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

  + Stuck symbolic results — trees produced by `hyp_reduce` (or
    composition through it) — are not the tree `Ok TT`.
  + Concrete `Ok FF` from a decidable respond (notably `Type`'s
    predicate-side H-rule) is also not `Ok TT`.

To inhabit ⊥, the body (under a fresh Type-hypothesis `hyp_A`) must
produce a value `v` whose check against `hyp_A` reduces to `Ok TT`.
`hyp_A` is a kernel-minted neutral with stored type `Type`, so
applying `hyp_A` to `v` routes through `hyp_reduce`, which consults
`Type`'s respond (`type_predicate_h_rule`, §12.17). That
respond returns `Return (tree_eq (stuck_stored_type v) self_as_hyp)`
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
`stuck_stored_type v = hyp_A` is to receive one via `bind_hyp` at
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
strict / behavioral validation (Sigma's projection respond,
Record, Refinement, MetaShape, RecognizerShape, the `safe_*` helpers),
then walks the individual type-formers (Bool, Nat, Pi, …).

== `Sigma` and projection-as-application

`Sigma A B` is the dependent product. Its values are pairs `pair a b`
where `a : A` and `b : B(a)`. Inspection is via `pair_fst` / `pair_snd`.

For *hypothesis* values of Sigma type, projection works through a
respond that treats projection as application. Applying a Sigma
hypothesis to a position walker triggers `hyp_reduce`, which dispatches
via `sigma_respond`:

```disp
sigma_respond := {meta, walker_arg} ->
  let A = meta.recognizer_params.dom in
  let B = meta.recognizer_params.fam in
  match (tree_eq walker_arg walker_pair_fst) {
    TT => Extend A
    FF => match (tree_eq walker_arg walker_pair_snd) {
      TT => Extend (B (apply (reconstruct_self meta) walker_pair_fst))
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
  recognizer_params := { dom := A, fam := B },
  functor := sigma_functor,
  respond := some sigma_respond,
  behavioral_specs := none
}

Sigma := {A, B} -> wait sigma_recognizer (sigma_meta_for A B)

test typecheck Type Sigma            // Sigma is structurally a type
test typecheck StrictType Sigma      // Sigma passes deep validation
```

Records (whose payload is a Sigma chain), Refinement-of-Record, and any
type built on Sigma chains inherit neutral-projection support through
this mechanism.

== The library eliminator

Case dispatch on inductive values is library code, not a kernel
primitive. The two cases are gated by `is_neutral`:

```disp
elim := {dispatcher, motive, cases, target} ->
  select_lazy
    ({_} -> param_apply target (pair motive cases))  // neutral
    ({_} -> dispatcher motive cases target)           // concrete
    (is_neutral target)

reconstruct_self := {meta} -> wait kernel.hyp_reduce meta
```

The neutral branch routes the case-frame `(pair motive cases)` through
`hyp_reduce`; the inductive type's `respond` extends the spine with
stored type `motive target`:

```disp
bool_respond := {meta, frame} ->
  Extend ((pair_fst frame) (reconstruct_self meta))

nat_respond := {meta, frame} ->
  Extend ((pair_fst frame) (reconstruct_self meta))
```

The concrete branch's `dispatcher` is walker-safe: for Scott-encoded
types it's plain application (no triage); for tagged-sum types it
triages on `target`, which the gate guarantees is concrete (triage on
concrete values is walker-safe).

`StuckElim` and `Hyp` are aliases at the tree level — both are
`wait kernel.hyp_reduce (make_neutral_meta T payload)`. The
distinction is purely about how the payload was constructed:
`bind_hyp` for hypotheses, `hyp_reduce` for spine-extended forms
(including stuck eliminations).

== `Record`

A record is a *name-header over a Sigma chain* (§2). The value is
`pair names payload`: `names` is the ordered field-name list in one
hash-consed header node, `payload` is the dependent Sigma chain holding
the field values. `{x : A, y : B}`'s payload type is
`Sigma A ({x} -> Sigma B ({y} -> Unit))`, so field dependencies
(`{n : Nat, v : Vec n}`) fall out of the chain.

```disp
Record := {fields} -> wait record_recognizer (record_meta_for fields)

// Recognize: the value's header must match the declared names (O(1)
// tree_eq on the hash-consed header), then the payload must inhabit
// the underlying Sigma chain.
record_recognizer := {fields, v} ->
  and (tree_eq (pair_fst v) (names_of_fields fields))
      (sigma_chain_recognizer (chain_for fields) (pair_snd v))
```

Because the names are in the *value*, two records with identical field
types but different names (`{a:Nat,b:Nat}` vs `{p:Nat,q:Nat}`) have
distinct headers and are told apart at the value boundary, not merely as
distinct types.

*Exact match vs. row subtyping.* `record_recognizer`'s `tree_eq` on the
header is an *exact* O(1) match: `Record F` recognizes only records whose
names are exactly `F`, in order. Width subtyping — accepting "any record
that has at least these fields," the basis of the row-polymorphism story
(§15, `extends R Console`) — is a *different* recognizer: a `Refinement`
whose predicate checks the value's header is a *superset* of the required
names (O(n) in the required count, not a single `tree_eq`). Closed records
keep the O(1) exact check; open/extensible records opt into the subset
check. They are distinct types, not one recognizer serving both.

=== Projection is a `checked` dependent application

`r.x` is not a positional shortcut the elaborator computes; it is the
application of a dependent function whose refined domain validates the
field name and whose codomain depends on it:

```disp
proj : Pi (Record F) ({r} ->
       Pi (ValidField F) ({n} ->     // F = r's type's field list, from the binder
          field_type F n))            // result type depends on the validated n

proj     := {r, n} -> path_at (index_of (names_of r) n) (pair_snd r)
names_of := {r} -> pair_fst r        // runtime read of the value header (concrete r)
```

`r.x` desugars to `proj r "x"`. This *is* the `checked` pattern (§8):
the validated input is the name `n` (`ValidField` is a Refinement decided
by kernel reduction, so `r.bogus` fails as a verdict `Ok FF`, never a host
throw); the trusted output type is `field_type F n`. The type depends on
`F` — the field list carried by `r`'s *type* `Record F`, from the binder —
not on `pair_fst r`; reading the value header is a runtime step (below),
never a type-level dependency on a hypothesis, and the recognizer
guarantees `pair_fst r = names_of_fields F`, so the two agree. Projection
reuses the kernel's Pi/`checked` machinery instead of re-implementing a
resolution pass.

*Concrete records resolve by reduction, trust-free.* For a literal name,
`proj r "x"` is a subterm closed in `r`: `names_of r`, `index_of`, and
`path_at` all reduce by ordinary `apply`, so the call β-reduces to the
direct path `pair_fst (pair_snd^idx (pair_snd r))`. This is unconditional
reduction — sound on all inputs, no certificate — and hash-cons shares the
normal form across every projection of that field. The elaborator decides
*nothing*: it emits `proj r "x"` and the kernel reduces — anything that
would otherwise be computed from the type is written as a closed term and
left to reduction.

*Neutral records resolve through `respond`.* When `r` is a hypothesis,
`names_of r = pair_fst r` would triage on a neutral, which the walker
forbids. There projection is an elimination, routed through the record
type's `respond` — which delegates to `sigma_respond` (§12.1), reading the
field list from the hypothesis's *stored type* (supplied by the binder,
not inferred). So names are read from the value header on concrete records
and from the type on neutral ones; neither path trusts an elaborator
computation.

=== Strip and the fast path

The reduction above needs no `strip` — it is plain β. `strip` (§10,
certificate-gated) enters only where a check is genuinely elided:

- *Dynamic names.* `proj_dyn r nameExpr` with a non-literal name keeps
  `ValidField` as a real runtime contract; `strip` removes it given a
  validation certificate — the literal `checked` story.
- *Header drop (optional).* A record proven to have no surviving dynamic
  or reflective projection may have its header elided (reclaiming the one
  node), shifting payload offsets by one. This is a representation
  lowering, not β, so it is certificate-gated and opt-in.

Reduction stays the unconditional optimizer (collapsing literal projection
to a direct path); `strip` stays the certified eliminator (removing checks,
and the optional header, whose redundancy a certificate vouches for).

Tests:

```disp
test typecheck Type Record
test typecheck StrictType Record
```

== `Refinement` (passthrough `respond`)

`Refinement A P` values are inspected as values of A with an attached
P-proof. For hypothesis frames, Refinement defers to A's `respond`:

```disp
refinement_respond := {meta, frame} ->
  let A = meta.recognizer_params.base in
  let A_respond = respond_of A in
  match (is_some A_respond) {
    TT => apply A_respond (A, frame)
    FF => Invalid
  }
```

A `Refinement Record [...]` hypothesis projects through this
delegation to Sigma's `respond` — supporting the same projections as
the underlying Record. This is what lets `MetaShape` (a Refinement of
Record) be used as a Pi domain whose body projects fields.

== `MetaShape` and `RecognizerShape`

The standard metadata layout (§11.2), as a Refinement of Record:

```disp
let MetaShape = Refinement
  (Record [
    ("recognizer_params", Tree),
    ("functor", Tree),
    ("respond", Tree),
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
// `meta` is the headered record `{ ascribed_type, value }` carried in the
// wait-form's payload (recovered by the wait reduction), read by name — the
// same one-record discipline type metas follow (§2.6, §11.2).
let checked_apply = {meta, arg} ->
  let T = meta.ascribed_type
  let v = meta.value
  match (is_pi T) {
    TT => bind (param_apply (pi_dom T) arg) ({verdict} ->
          match verdict {
            TT => Ok (param_apply v arg)   // `v` is the raw wrapped fn, so
                                           // `param_apply v arg` is a bare reduct;
                                           // checked_apply is the CheckerResult-
                                           // producer here, so it wraps (§7.5, §8.9)
            FF => Err (TypeMismatch { expected = pi_dom T, actual = arg, span })
          })
    FF => Err (NotApplicable { type = T, span })
  }

let checked = {T, v} -> wait checked_apply { ascribed_type := T, value := v }

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

*Why `is_pi`, not `is_applicable_type`.* `checked_apply` implements
exactly one elimination frame — *function application* — and then
calls the Π-specific `pi_dom`. It must therefore gate on `is_pi`, not
on the broad `is_applicable_type` (= "responds to *some* frame"),
which is also true of Σ (projection), inductives (case-frame), `Path`
(dimension) and `Type` (candidate). Gating on `is_applicable_type`
would let `checked (Sigma A B) f` past the guard and then misapply
`pi_dom` to a non-Π type. The division of labor is clean: *concrete
typed function values* are applied through `checked_apply` (Π frame
only); *all other elimination frames on neutrals* are handled by
`hyp_reduce` + the stored type's `respond` (§7.1, §11.2). A concrete
Σ value is projected with `pair_fst`/`pair_snd` directly and never
reaches `checked_apply`; `Path = Pi I` is a Π alias, so `is_pi`
accepts it. Applying a non-Π checked value (`checked Nat x`, §7.2)
still yields `Err NotApplicable`, exactly as before — only the
applicable-but-not-Π bug changes.

*Walker-safe body.* `checked_apply`'s operations — pair projections,
`is_pi`, `pi_dom`, `param_apply` — are all walker-safe (no
stem-forge of pinned sigs, no triage on neutrals). The function lives
in the library; no kernel privilege required.

*Dispatcher behavior.* The wait-form
`wait checked_apply { ascribed_type := T, value := v }` has the sig
`checker_sig checked_apply` — a library sig, not in `default_dispatch`.
So the dispatcher does not route it specially; walker reduction handles
`(checked T v) arg` by reducing via wait's bracket-abstracted shape to
`checked_apply { ascribed_type := T, value := v } arg`. Internal
`param_apply` calls then route through the dispatcher normally.

*`is_applicable_type`*:

```disp
is_applicable_type := {T} ->
  match (safe_is_fork T) {
    FF => FF
    TT => not (is_none (meta_get (safe_pair_snd T) "respond"))
  }
```

Returns TT iff T's metadata has a non-`none` `respond` field —
i.e., T accepts an elimination frame. (Sharper Π-specific checks
use `is_pi` / `pi_dom`.)

== `safe_*` helpers (hypothesis-safe structural inspection)

Inspection of a value's tree shape (`is_fork`, `pair_fst`, `has_sig`)
via raw triage is walker-unsafe — triage on a kernel-minted neutral
is rejected (§4.2). The `safe_*` helpers route through the library
`elim` (§12), which gates on `is_neutral` and pushes a case-frame
into `hyp_reduce` for stuck targets:

```disp
safe_is_fork := {v} -> elim
  tree_shape_dispatcher
  (const Bool)
  {leaf := FF, stem := {_} -> FF, fork := {_, _} -> TT}
  v

safe_pair_fst := {v} -> elim
  tree_shape_dispatcher
  (const Tree)
  {leaf := error_form, stem := {c} -> c, fork := {l, _} -> l}
  v

safe_has_sig := {checker, v} ->
  bind (safe_pair_fst v) ({sig} -> Ok (tree_eq sig (checker_sig checker)))

safe_is_neutral := {v} -> safe_has_sig kernel.hyp_reduce v

// All stuck forms are hyp_reduce-rooted (§6.1), so the generalized
// stuck-form check is a single sig comparison.
safe_is_stuck := {v} ->
  bind (safe_pair_fst v) ({sig} ->
    Ok (tree_eq sig (checker_sig kernel.hyp_reduce)))

// Stored-type extraction. All stuck forms carry the stored type in
// the first slot of their payload, regardless of whether the payload
// describes a hypothesis or a spine-extended elimination.
stuck_stored_type := {v} -> neutral_meta_type (pair_snd v)
```

For concrete values, these reduce to the raw operation's result; for
hypotheses or other stuck forms, they reduce to a `StuckElim`
(produced by `hyp_reduce` + the inductive type's `respond`)
representing "this structural property of an unknown value."

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

The generalized check `safe_is_stuck` (§12.7) accepts any wait-form
rooted at `hyp_reduce` — all stuck forms (raw hypotheses, spine
extensions, stuck eliminations) share this one constructor. The
check is Σ-independent. This closes a soundness gap that matters for
effect-using polymorphic functions: a Pi-body containing stuck-
elimination intermediate values (e.g.
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
  respond := none,
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
  respond := none,
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
  let A = meta.recognizer_params.dom
  let B = meta.recognizer_params.cod
  // Step 1 (pure structural): v must be a `checked` wait-form.
  // Raw has_sig is fine — v is concrete here (make_recognizer's
  // H-rule already handled the hypothesis case).
  match (has_sig checked_apply v) {
    FF => Ok FF
    TT =>
      // Step 2 (pure structural): v's stored domain matches A. Recover v's
      // meta record via wait_meta (§5.4); its `ascribed_type` field is the
      // full stored type `Pi A' B'` — then compare its *domain* to A.
      let v_dom = pi_dom (wait_meta v).ascribed_type in
      match (tree_eq v_dom A) {
        FF => Ok FF
        TT =>
          // Step 3 (kernel-mediated): bind a fresh A-hypothesis, apply
          // v to it, then check the result against B hyp.
          bind_hyp A ({hyp} ->
            bind (param_apply v hyp) ({result} ->
              param_apply (B hyp) result))
      }
  })

// Respond for a Π-typed neutral: the frame is an argument `a`, and the
// spine extends at the codomain instantiated there (§6.2, §11.2). Mirrors
// `sigma_respond`'s meta convention (`meta.recognizer_params.*`).
pi_respond := {meta, frame} -> Extend ((meta.recognizer_params.cod) frame)

let pi_meta_for = {A, B} -> {
  recognizer_params := { dom := A, cod := B },
  functor := pi_functor,                          // non-trivial; supports transp (§13)
  respond := some pi_respond,
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

The recognizer uses `safe_*` helpers (§12.7) for structural inspection,
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
  respond           := none,
  behavioral_specs  := none
}

let Unit = wait unit_recognizer unit_meta

test typecheck Type Unit
test unit_recognizer unit_meta unit_witness = Ok TT
test unit_recognizer unit_meta TT            = Ok FF
```

== `Eq`

Propositional equality. `Eq A x y` is inhabited by `refl_eq` (the
canonical discrete witness) exactly when `tree_eq x y = TT`. Cubical
`Path` (§13) is the computational counterpart, with its own `refl`
(§13.3); `Eq` is the discrete one.

```disp
let eq_recognizer = make_recognizer ({meta, v} ->
  let x = pair_fst (pair_snd meta)
  let y = pair_snd (pair_snd meta)
  // v inhabits Eq iff v = refl_eq AND x and y are hash-cons-equal
  Ok (and (tree_eq v refl_eq) (tree_eq x y)))

let eq_meta_for = {A, x, y} -> {
  recognizer_params := pair A (pair x y),
  functor           := eq_functor,         // refl at the new endpoints (§13)
  respond           := none,
  behavioral_specs  := none
}

let Eq = {A, x, y} -> wait eq_recognizer (eq_meta_for A x y)

test typecheck Type Eq
test typecheck (Eq Nat zero zero) refl_eq
test eq_recognizer (eq_meta_for Nat zero zero) refl_eq = Ok TT
test eq_recognizer (eq_meta_for Nat zero (succ zero)) refl_eq = Ok FF
```

The cross-type-parameter check (`tree_eq x y`) is the load-bearing
piece: `refl_eq` inhabits `Eq A x x` for any `A` and any `x`, but never
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
  respond           := none,
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
  respond           := none,
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
and the *predicate-side H-rule* in its `respond` slot.

```disp
// Predicate-side H-rule (see §6.3). Fires when a Type-typed
// hypothesis is applied as a predicate to a candidate value `v`:
// returns Ok TT iff v is itself a kernel-minted neutral whose
// stored type hash-cons-equals the applied hypothesis. This is the
// dual of `make_recognizer`'s recognizer-side H-rule — needed
// because Type-hypotheses, when applied, route through
// `hyp_reduce`'s raw arm and never reach the `make_recognizer`
// wrapper. Body runs raw per §7.1's respond discipline; raw
// `is_neutral` / `stuck_stored_type` are walker-safe because the
// respond is invoked from inside `hyp_reduce`'s privileged
// handler.
let type_predicate_h_rule = {neutral_meta, v} -> {     // a 2-arg `respond`, like bool_respond
  let self_as_hyp = wait kernel.hyp_reduce neutral_meta in
  match (is_neutral v) {
    TT => Return (tree_eq (stuck_stored_type v) self_as_hyp)
    FF => Return FF
  }
}

let type_self_meta = {
  recognizer_params := unit_witness,           // Type takes no params
  functor           := trivial_functor,
  respond           := some type_predicate_h_rule,
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
`respond` populated) satisfies the structural check, returns
`Ok TT`, and the tests pass by construction. The H-rule tests
exercise the predicate-side path: applying a Type-hypothesis
through `hyp_reduce` and observing the respond's verdict.

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

Disp's existing equality (`Eq A x y` with `refl_eq`) is propositional —
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
  respond := none,
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
  respond := none,
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
// The unified composition operator. Look up the source type's functor
// and hand it the full 5-tuple. A `trivial_functor` (the discrete
// marker) means *trivial Kan structure*: comp returns u0 directly.
comp := fix ({self, P, phi, u, u0} -> {
  let T0 = apply P I_zero
  let action = meta_get (type_meta T0) "functor"
  match (tree_eq action trivial_functor) {
    TT => u0                              // discrete: trivial Kan structure
    FF => apply action self P phi u u0    // real functor handles family + cofibration
  }
})

// `transp P x` is `comp P I_zero (empty_partial T_path) x` — the
// degenerate case with no cofibration.
transp := {P, x} -> comp P I_zero (empty_partial (apply P I_zero)) x

// `hcomp` is `comp` at a constant family. The first arg is just a
// type rather than a type-path.
hcomp := {A, phi, u, u0} -> comp ({_} -> A) phi u u0
```

`trivial_functor` now means *trivial Kan structure*, and `comp`
returns `u0` for it directly — correct for *both* `transp` (identity
transport) and `hcomp` (a discrete type composes to its cap `u0`,
since its only paths are reflexivity). So `Bool`, `Nat`, etc. keep
`functor := trivial_functor` and need no transport rule of their own.
(This replaces the earlier `trivial_functor → StuckElim`, which wrongly
made `transp Bool x` stuck instead of `x`.) A genuine per-type functor
is consulted for every non-discrete type — including `hcomp` over
higher types, whose homogeneous composite is *not* `u0` — and is free
to short-circuit with `u0` itself when its own source and target
hash-cons-equal.

#openq[
  *Non-discrete dispatch is endpoint-bound (the `ua`/`Glue` gap).*
  For a non-discrete type `comp` dispatches on the functor of the
  *source endpoint* `T0 = P I_zero`. That is correct for families
  built from a single covariant former, but it cannot deliver
  univalence transport: for `transp (ua A B e) x` the equivalence `e`
  lives in the `Glue` at the `i = I_one` face, which `T0`'s functor
  never sees. Fixing this needs `comp` to dispatch on the head former
  of the *family under a fresh interval hypothesis*
  (`bind_hyp I ({i} -> head_former_of (P i))`) rather than on an
  endpoint — the standard "recurse on the type family" principle,
  recast into disp's neutral machinery. Deferred to a dedicated
  cubical pass; until then, `ua`-mediated transport is not
  operational and the §13.8 "univalence as a definable theorem"
  claim is aspirational.
]

Per-type morphism actions (always 5-arg, conforming to the calling
convention above):

```disp
// Discrete types (Bool, Nat, False) need no functor: their
// `functor := trivial_functor` makes `comp` return `u0` directly
// (trivial Kan structure — identity transport, and hcomp = cap).

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
  respond := none,
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

+ *Structural transport on type-formers* via each type's
  `meta.functor` field — for discrete types and single-former
  covariant families. (Identity transport / cap-composition on
  discrete types is delivered by `comp`'s `trivial_functor` branch.)
+ *Univalence as a definable theorem* via `Glue` + `ua` — *once the
  non-constant dispatch is fixed* (the endpoint-bound dispatch openq
  in §13.5). The framework supports it; the current `comp` does not
  yet route `ua`-paths to `glue_functor`.
+ *Definitional iso-roundtrip* for ua-mediated equivalences —
  contingent on the same fix.
+ *Representation independence in practice* — functions over one
  representation work on equivalent representations via transport.
+ *No kernel growth.* The three Σ-operations plus the dispatcher are
  the total kernel surface; cubical adds none.

== Limitations

+ *Endpoint-only path evaluation.* `comp` dispatches on the functor of
  the source type `T0 = P I_zero` (discrete `trivial_functor` types
  return `u0` directly). Families whose transport rule lives at the
  *other* endpoint — notably `ua`/`Glue`, whose equivalence sits at
  the `i = I_one` face — are not yet handled (the §13.5 openq). Path
  bodies whose intermediate behavior matters are likewise not
  captured.
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
metatheoretic proof. The conjectural-consistency story (§11.6)
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
      [The three kernel handlers behave per their specs (§7).
       Test that `bind_hyp` mints a neutral, `hyp_reduce` extends
       spines and mints StuckElim via inductive types' `respond`,
       `postulate` builds pinned wait-forms. Run under `test_pure` —
       the foundational layer must not depend on host primitives.],
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
test hyp_reduce_mints_stuck_elim_via_nat_respond
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

- The three kernel Σ-operations (`hyp_reduce`, `bind_hyp`,
  `postulate`) implemented in disp source.
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
consistency (Type:Type, etc.) — see §11.6 for the open conjecture.

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

The test suite addresses (1); (2) is open work flagged in §11.6.

*`bind_hyp` and Type:Type share a proof recipe.* The escape check
(§7.2) blocks hypothesis leakage *structurally*: `occurs` descends
through seals, so a stuck elimination whose spine carries `h` does not
pass — it is exactly the value the scan is built to catch (§8.1). What
the scan cannot see is a *laundered* leak: a handler's `respond` that
reads a neutral's metadata and `Return`s a fresh value rebuilt from it,
carrying no neutral. That path is constrained separately, by the
`respond` discipline (§7.1: a `Return v` must be built only from public
inputs). The two together — structural scan plus `Return` discipline —
keep a hypothesis-typed neutral from being extruded and then accepted
by the H-rule as an inhabitant of its stored type.

This is the dynamic-sealing parametricity preservation theorem of
Matthews & Ahmed (*Theorems for Low, Low Prices!*, ESOP 2008),
proved by step-indexed logical relations. The disp setting is a
strict simplification: the kernel is the *sole* keyholder (user code
has no `unseal`), so the relational interpretation is single-party.

#openq[
  A step-indexed Kripke logical relation indexed by the world of
  allocated seals (the live `Tree_p(Σ)` neutrals) is the indicated
  technique; Ahmed–Dreyer–Rossberg's *State-Dependent Representation
  Independence* supplies the closest off-the-shelf machinery.
  Mechanizing this would settle both `bind_hyp`'s soundness obligation
  and the Type:Type conjecture (§11.6).
]

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
evaluate with `safe_apply kernel_handlers` in place of `param_apply` —
exactly what the `test_pure` directive (§9) does:

```disp
// `test_pure expr` reduces `expr` under `safe_apply kernel_handlers`
// rather than the default environment.
test_pure (typecheck Type Bool) = Ok TT
```

Under `safe_apply kernel_handlers`, host sigs are *not* pinned, so the walker
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

This would extend the seal set (§4.2) with `async_pending`'s sig —
`safe_is_stuck` becomes `pair_fst v ∈ {hyp_reduce, async_pending}`.
The H-rule then fires for `Promise X`-typed stuck forms uniformly;
polymorphic code over promises works the same way as polymorphic
code over neutrals.

Disp doesn't ship async yet. The point: stuck forms are a general
mechanism for "values whose concrete identity isn't yet known," not
just for type-checking hypotheses. Async slots in by adding one
producer to the kernel list — same machinery, new motivation.

#openq[
  *Higher-order effects (Wu–Schrijvers–Hinze 2014).* Effects whose
  operations take computation arguments (e.g. `catch handler body`)
  need more machinery than capability passing. The library `elim`
  (§12) + `hyp_reduce` mechanism covers algebraic operations cleanly;
  higher-order operations are a future addition.
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

= Appendix C: design direction — descriptor-tagged values <sec:descriptor-values>

#note[
  *Status.* Design direction, *not* adopted in the spec body. This appendix
  maps a unification the current encodings already gesture at — records,
  coproducts, and functions as one primitive — so a future pass can adopt it
  deliberately rather than rediscover it. Nothing in §1–§16 depends on it.
]

== The shape every value already shares

Every value in the substrate is `fork(descriptor, payload)`. The three
surface encodings differ *only* in what sits in the descriptor slot
(`pair_fst`):

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Encoding*], [*descriptor (`pair_fst`)*], [*how it activates*],
    [wait-form], [a sig `checker_sig k` (fingerprint of `k`)],
      [reduction: `wait k m x → k m x`],
    [record], [a field-name header `[a, b, …]`], [projection (read header → index)],
    [coproduct], [a constructor tag], [`match` (compare tag)],
    [neutral], [the `hyp_reduce` sig], [routing: dispatcher → `hyp_reduce`],
  ),
  caption: [One shape; the descriptor decides the capability.],
)

The generalization: *`pair_fst` is a descriptor, and which operations a value
supports is decided by which tables its descriptor appears in* — registered in
Σ ⇒ applicable; a name-list ⇒ projectable; a constructor tag ⇒ matchable. A
single value may be several at once. Record / wait-form / coproduct stop being
three encodings and become three *capabilities* a descriptor may grant.

Note a wait-form's `k` is *any* tree: the dispatch sig is an emergent O(1)
fingerprint of `k`, not a prerequisite. Most wait-forms (types, `checked`) are
*not* in Σ and activate purely by reduction; Σ-membership is the extra layer
the privileged few (`hyp_reduce`, host primitives) opt into. The wait
combinator's real contribution is *inspectability* — a stable fingerprint in
`pair_fst` plus a meta recoverable from `pair_snd` — i.e. an *inspectable
closure*.

== Records and coproducts as nested wait-forms

A record must bind `(proj, schema, payload)` and then wait for a `name`; a
coproduct must bind `(sel, tag, payload)` and then wait for the `cases`.
Freezing two-of-three is two nested wait-forms:

```disp
{a:=x, b:=y}  ≡  wait (wait proj [a,b]) (pair x y)     // apply to a NAME
Vi e          ≡  wait (wait sel  "Vi" ) e              // apply to a CASE-RECORD

// reductions:
{a:=x,b:=y} n  →  proj [a,b] (pair x y) n   →  the value at field n
(Vi e) cases   →  sel "Vi" e cases          →  cases "Vi" e   →  hi e
```

*The inner wait-form collapses.* A wait wrapper earns its keep only when its
frozen arg must be *recovered* — but the inner schema/tag is unreachable
through the outer wait anyway (the outer exposes only the *fingerprint* of its
`k`, never `k`). The inner layer's remaining jobs — *work*, and give the outer
record a *per-schema fingerprint* in `pair_fst` (`checker_sig` of the inner,
which bakes in the schema) — are both met by a bare bracket-abstracted
combinator. So collapse the inner wait into a specialized *selector-reception
function*:

```disp
{a:=x, b:=y}  ≡  wait proj_ab payload      // proj_ab : payload → name  → value
Vi e          ≡  wait sel_Vi  payload      // sel_Vi  : payload → cases → result
```

The slot logic that forces this: data to be *recovered* (payload) goes in the
outer meta; a descriptor to be *fingerprinted* (schema / tag) fuses into `k`;
behavior (`proj` / `sel`) fuses into `k`. Field/variant *enumeration* — names
as readable data, not just a capability — costs one extra stash of the schema
in the outer meta (`wait proj_ab (pair [a,b] payload)`); that is the only knob.

Literal projection still constant-folds (`proj_ab payload "a"` β-reduces to the
value), and same-schema descriptors stay hash-cons-shared, so the §2.6 name
optimizations survive; what changes is only whether names are *enumerable data*
or an *encapsulated capability*.

== Strip in this design

The §2.6 / §10 projection optimization has two parts, and both carry over.

*Part 1 — literal projection β-reduces with no runtime name lookup.* `r.a ≡ r
"a"` on a concrete `wait proj_ab payload` reduces by plain `apply`:
`r "a" → proj_ab payload "a" → pair_fst payload` — the wait resumes, then
`proj_ab` constant-folds the literal name against its *baked-in* schema. No
certificate, no runtime compare. This is in fact *cleaner* than the data view:
the schema lives in the always-concrete `proj_ab`, never in the value header, so
the "`pair_fst r` would triage on a neutral header" case that sends the data
view through `respond` (§12) never arises.

*Part 2 — one type-directed pass.* The data-view "header drop" and the `checked`
unwrap are two cases of a single erasure: *drop the part of each descriptor the
*type* already fixes, keep the part only the *value* knows, and lower every
eliminator to positional / raw form.* A wait-wrapped value `wait k payload`
splits its descriptor this way:

- *type-determined* — a contract (`checked`'s `ascribed_type`) or a name-schema
  (record fields, coproduct variants). The type fixes it, so it is redundant at
  runtime: names resolve to positions, a validated contract drops.
- *value-determined* — the payload, plus *which variant* for a sum. Only the
  value knows it, so it is retained.

#figure(
  table(
    columns: 4,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*kind*], [*type-determined → stripped*], [*value-determined → kept*], [*eliminator: before → after*],
    [`checked`], [the ascription `T`], [inner value `f`],
      [`cv x` → raw `f x` (no rewrite)],
    [record], [field names → positions], [field tuple],
      [`r."a"` → `pair_fst (pair_snd^idx payload)`],
    [coproduct], [variant names → positions], [variant *index* + payload],
      [`match`-by-name → dispatch-by-index],
  ),
  caption: [Strip = drop the type-determined descriptor, lower the eliminator.],
)

In the coproduct row the variant *names* strip to positions — *both* the value's
tag (name → positional index) and the cases-record (a product, so its header
strips like any record → a positional branch tuple) — so `match v cases` lowers
to "index the positional branches by the value's positional index." Names thus
disappear for products *and* sums alike.

`checked` is the *degenerate* case: its descriptor is entirely type-determined (a
pure contract), so it strips completely and its eliminator (apply) needs no
rewrite — raw application of the inner value is already correct. That is exactly
what §10 does today: recognize `checked` wait-forms by sig, replace with
`.value`. The general pass keeps that move and adds two strippable wrapper kinds:
recognize a record-projector or coproduct-selector sig, replace with the
value-determined core, and resolve the type-determined names → positions at the
eliminator sites. Resolving names → positions needs the binder types, so the pass
is *type-directed* — the standard erasure framing (§17), of which today's purely
structural `checked`-strip is the no-rewrite corner.

*What survives.* The only residue anywhere is a sum's variant *index* — the
genuinely dynamic "which one." It is no longer a name but a minimal positional
tag (≈ `log₂` variants of structure; in the Scott encoding not even a data field,
just which branch the value picks; a single-variant sum strips even that). So the
product/sum difference at strip time is *not* "whether the descriptor strips" — it
always does — but only the *residue*: a product leaves nothing, a sum leaves one
minimal dynamic index. That residue is the operational content of polarity (CBPV
/ focusing, §17): a product's content is fixed by its type (negative, fully
static), a sum's is chosen by its value (positive, one dynamic index survives).

== The ADT / object duality

The two presentations are the two faces of Cook's ADT-vs-object duality
(Reynolds 1975; Cook 2009):

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [], [*data view* (ADT) — eliminate by *reading*], [*behavior view* (object) — eliminate by *applying*],
    [*product*], [`pair names payload`; project via header], [`wait proj_ab payload`; apply to a name],
    [*coproduct*], [`pair tag payload`; `match` on tag], [`wait sel_Vi payload` (Scott); apply to cases],
  ),
  caption: [Same value, two faces; the wait-form holds both.],
)

The selecting name comes from *outside* for a product (the consumer chooses a
component) and from the *value's own baked-in tag* for a coproduct (the value
chooses a handler) — the essence of ⊓ vs ⊔. Coproduct elimination bottoms out
in record projection (`cases "Vi"`), and the data view additionally gives
enumeration while the behavior view additionally gives a direct branch-jump (no
tag scan) — which is exactly why §2.6 notes the Scott encoding "optimizes the
general eliminator," with `Bool`'s `{m,ct,cf}->ct` / `{m,ct,cf}->cf` the
hand-tuned curried special case.

Two threads meet here from opposite directions: pushing *functions* toward the
data view gives "a function is an inspectable `{ascribed_type, value}` record"
(the Design A meta is already this); pushing *records* toward the behavior view
gives "a record is a function from name to value." The wait-form — an
inspectable closure with a data side (recoverable meta + fingerprint) and a
behavior side (resume on apply) — is the hinge that makes a value presentable in
either view, or both. For a metacircular, self-optimizing language this is the
point: slide freely between *code I run* and *structure I inspect and rewrite*.

== Design B: dispatchable records (functions-are-records)

A value can be *both* projectable-by-name and applicable iff its descriptor is
both a field-name header and a Σ-registered key. The hard constraint:
applying a plain record `fork([names], payload)` fires *triage* on the argument
(its `pair_fst` is itself a fork), so *records cannot be made callable by
reduction* — callability must go through the dispatcher (Route 2). Register the
header `[ascribed_type, value]` → `checked_apply`, and `checked T f` *is* the
record `{ascribed_type := T, value := f}`: `.ascribed_type` projects, `v x`
dispatches. Then `pi_dom T` collapses to a literal `T.recognizer_params.dom`
field chain with no recovery hop.

When behavior is *fixed* the header alone selects the handler (`checked` →
`checked_apply`). When it is *per-instance* — each type needs its own recognizer
— the behavior moves to a *field* read by a *generic* handler (header →
`run_recognizer`, which runs `self.recognizer`): the vtable / typeclass-
dictionary pattern. The end state is "functions, types, and neutrals are all
dispatchable records, eliminated by dispatch-on-header then field reads."

#openq[
  *What adopting Design B would cost.* (1) Callable ⇒ Route 2: `checked` (and,
  fully, types) move from walker-reduction into the dispatcher's registered
  table, so `checked_apply` joins the trusted-routed set and more applications
  are routed. (2) Lose §5.3's "sig derivable from handler" — header keys are not
  computed from the handler, so Σ entries become explicit `(key, handler)`
  pairs. (3) Lose §11.4's "different types have different `pair_fst`" — types
  share the MetaShape header and differ only in field contents (`tree_eq` still
  separates them by fields). (4) Forgery is *already* handled: the walker's
  stem-forge rule guards `pair_fst` construction against `forge(Σ)` regardless
  of whether the forbidden tag is a wait-sig or a header, so seals stay
  unforgeable and `checked_apply` (not in `forge(Σ)`) stays freely
  constructible, lies caught at use (§8.7). Adoption should be a deliberate pass
  that resolves these together, not a piecemeal edit.
]

== The cut: products and coproducts annihilate

The previous subsections present records, coproducts, and functions as one
shape. This one collapses their *elimination* to a single operation — the
*cut* — by making the product the callable side and the coproduct the data
side. Four definitions:

```disp
annihilate := {P, c} -> (proj P (pair_fst c)) (pair_snd c)  // project P by c's tag, feed c's payload
prod := {P}        -> wait annihilate P     // a product: a CALLABLE wrapper; field-data stays in wait_meta
inj  := {tag, pay} -> pair tag pay          // a coproduct value: tag + payload (plain inspectable data)
acc  := {name}     -> inj name unit         // an accessor: a nullary coproduct (unit payload)
```

`prod` makes a record a `wait`-form, so applying it to a coproduct is *raw
substrate application* — no surface `pair_fst`/`pair_snd`. The load-bearing
insight (§13's `Bool`, the curry iso) is that *a record is a product whose
fields ignore the payload* (`const`), while a `match` is a product whose fields
*use* it:

```disp
// RECORD = product of `const` fields (ignore the accessor's unit payload)
r := prod { x := const 5, y := const 7 }
r (acc 'x)  ⇝  annihilate {x:=const 5,…} (inj 'x unit)
            ⇝  (proj {x:=const 5,…} 'x) unit  ⇝  (const 5) unit  ⇝  5      // r.x  ≡  r (acc 'x)

// MATCH = product of real handlers (use the scrutinee's payload)
cases := prod { None := const 0, Some := {k} -> add k 1 }
cases (inj 'Some 5)  ⇝  (proj {…} 'Some) 5  ⇝  ({k} -> add k 1) 5  ⇝  6   // match v {…}  ≡  cases v
```

`r (acc 'x)` and `cases v` are the *identical* call — `prod`-thing applied to
`inj`-thing. The only differences are `const` vs real handlers, and a `unit` vs
a data payload. So a record and a `match` are one construct, dialed between
"ignore the payload" and "use it."

This is the polarized / CBPV / sequent picture exactly: *coproducts are
positive values, products are negative consumers (continuations that say what to
do per tag), and `P C` is the cut.* `match`, `.`, and `checked_apply` are all
this one operation; `checked_apply` is its *intensional* case (the function is a
product over an arbitrary domain, so the component is computed, not stored —
§C below).

#note[
  *Cost ledger.* (1) Records gain a `wait` wrapper and fields a `const` wrap —
  pre-strip overhead that *strip* removes: a literal accessor `r.x` resolves to a
  static position and `annihilate`/`prod`/`const`/`proj` β-collapse to the direct
  path `pair_fst (pair_snd^idx …)`. So the cut is the *semantic* model; strip
  lowers it to the positional data form. (2) Inspectability is preserved — the
  field-data is the product's `wait_meta`; the coproduct is plain data. (3)
  Neutral `c` routes through `respond` (a stuck cut), as records/coproducts
  already do. (4) Hygiene wart: `const x = fork(LEAF, x)` collides with `Ok x` and
  `succ x` (§3.4) — the `fork(LEAF,_)` shape is already overloaded; making it
  canonical for fields wants a dedicated tag.
]

== Σ and Π: coproduct types, record types, and where Pi lands

The cut forces the type story to close up, and it closes cleanly: every former
here is `Σ` or `Π`, at a *finite* (extensional, data) or *arbitrary*
(intensional, code) domain.

*Coproduct types are finite-tag `Sigma`.* A coproduct value `pair tag payload`
inhabits `Coproduct [(V1,S1), …, (Vn,Sn)]` iff its tag is one of the `Vi` and
its payload inhabits the matching `Si` — i.e. it is the `Sigma` (§12.1) whose
first component ranges over the closed tag set:

```disp
let coproduct_recognizer = make_recognizer ({meta, v} ->
  let variants = meta.recognizer_params.variants        // [(V1,S1), …]
  bind (lookup_arm variants (pair_fst v)) ({arm} ->      // arm whose tag = v's tag
    match (is_some arm) {
      TT => param_apply (unwrap arm) (pair_snd v)         // payload must inhabit S_tag
      FF => Ok FF                                         // unknown tag → not an inhabitant
    }))

let Coproduct = {variants} -> wait coproduct_recognizer (coproduct_meta_for variants)
//   Coproduct [(Vi, Si)]  ≅  Sigma (Tags [V1..Vn]) ({t} -> arm_type variants t)
```

*Record types are finite-index `Pi`.* `Record [(Vi, Ti)] ≅ Π (i : Tags). T_i` —
a dependent function from the index to the field type. So field access is just
`Pi`-application: `r (acc a) = r a` selects the `a`-th component.

*Typing the cut — the two sides are curry-duals over the shared index.* `P C`
typechecks exactly when the product `P` inhabits the handler-record derived from
the coproduct `C`:

#figure(
  table(
    columns: 1, stroke: none, inset: 5pt, align: center,
    [`C : Coproduct [(Vi, Si)]     P : Record [(Vi, Si -> R)]`],
    [#line(length: 70%, stroke: 0.5pt)],
    [`P C : R`           #h(2em) (cut = `Σ`-elimination)],
  ),
  caption: [The product and coproduct must be `Π`/`Σ` duals over the same index.],
)

The handler-record `Record [(Vi, Si -> R)]` *is* `Handlers (Coproduct [(Vi,Si)])
R` — the curry iso `(⊕ Si) → R ≅ ∏ (Si → R)`. Compatibility is therefore an
ordinary `Record` recognition: `P` must have exactly the fields `{Vi}` (exact
header ⇒ *exhaustiveness*), each typed `Si -> R`. The dependent form `P :
Record [(Vi, Si -> R_i)]` gives `P C : R_(tag C)`; *field access is this with
`Si = Unit`, `R_i = T_i`*, so `r (acc a) : T_a` — and since the accessor `a` is
a literal, `T_a` is static.

*So does `Pi` merge? Conceptually yes; operationally it is the intensional
fragment.* Laying the four formers on the `Σ`/`Π` × finite/arbitrary grid:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray, align: left, inset: 6pt,
    [], [*finite domain — extensional (data, stored)*], [*arbitrary domain — intensional (code, computed)*],
    [*`Π` (product)*], [`Record` — graph stored as a `Σ`-tuple, accessed by lookup], [`Pi` — graph computed; `f x` = `checked_apply`],
    [*`Σ` (sum)*], [`Coproduct` — finite tag + payload], [`Sigma` — arbitrary tag + dependent payload],
  ),
  caption: [Every former is `Σ` or `Π`; the only axis left is extensional vs intensional.],
)

A record *is* a finite `Pi`; a coproduct *is* a finite `Sigma`. The cut couples a
`Σ`-value (coproduct) with a `Π`-consumer (record/cases) over a shared index, and
it is exactly `Σ`-elimination; `Pi`-application (field access, function call) is
`Π`-elimination. The one thing that does *not* collapse is the *finite ↔
arbitrary / extensional ↔ intensional* axis: a finite product's graph can be
stored as a `Σ`-tuple and looked up (hash-cons-comparable); an arbitrary `Pi`'s
graph must be computed and is not a finite datum. That is the data/codata
(positive/negative) boundary — the same residue every layer of this appendix
bottoms out at, and the reason `Pi` shares records' *shape* and *type-former*
(`Π`) without being swallowed by the finite-cut machinery.

The picture, whole: *`Σ` and `Π`, finite or arbitrary; one introduction per
polarity (inject / tabulate) and one elimination — the cut, `Σ`-value against
`Π`-consumer — with `match`, `.`, and function application its finite-extensional,
finite-extensional, and arbitrary-intensional instances.*

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
