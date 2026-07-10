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
  *Status.* Active spec. Replaces the prior seven-primitive `TYPE_THEORY.typ`
  and consolidates the category-theory and cubical proposals into one document.
  The framing, the kernel surface, and every former are developed in full below
  (§1 onward); this note is only a dated changelog. Open items are flagged
  inline as `Open question:` notes.

  *2026-06-01 — two-Σ-op kernel.* The kernel drops to *two* Σ-operations
  (`hyp_reduce`, `bind_hyp`) plus the two-argument dispatcher `param_apply` over
  a *fixed* Σ. `postulate` is removed and effects are re-cast as a pure library
  construction (`Eff R X` free monad + handlers + an outermost driver, §15); the
  `funnel` sig-set dissolves (`forge = seal`); `eliminator_frame` folds into
  `hyp_reduce` + a library `elim`; and `respond` becomes a constitutive
  (non-optional) field with a two-tag `Action`.

  *2026-06-13 — reconciled with the implementation.* `Action`'s second arm is
  `Reduce Tree` (this spec writes `Return v` for the verdict case `Reduce (Ok
  v)` — notation only, not a third arm; the library spells it out), both arms
  *bare* — `hyp_reduce` returns the reduct unwrapped, so
  `Ok <neutral>` never arises (the §7.5 "Option A" invariant). `respond` is
  `recognizer_params → self → frame → Action`, typed `RespondShape` (§12.6). The
  dependent record former `Telescope` (§12.7) subsumes `Sigma`/`Record`, so
  Σ-values are §2.6 records projected by name (the `walker_pair_*` selectors are
  gone; derived fields make `Reduce` carry a computed projection). The kernel's
  structural types (`Tree`, `Frame`, `NeutralMeta`, `Action`, `RespondShape`,
  `MetaShape`) are first-class, so the kernel typechecks its own internals;
  `bind_hyp` runs inside the walker and modules auto-`verify` at elaboration.
]

= Overview <sec:overview>

== The framing in one paragraph

Disp is a dependently-typed language whose type system is implemented as
*manifest contracts* over a tree-calculus substrate. Every typed function
value carries a runtime input-checker (a "contract"); every type is a predicate on programs implemented as a *wait-form* whose recognizer field judges inhabitants. The elaborator's only
job is to transform syntax into trees and emit tests — no bidirectional
inference, no judgments. Type validation is a `test` declaration that
runs a library validator at elaboration time. Failures throw with the
failing component identified. After elaboration succeeds, a *strip pass*
elides validated contracts to give a runtime tree with no per-call
checking overhead.

The kernel is a *tree-calculus interpreter over a fixed dispatch set* Σ
— the two Σ-operations `hyp_reduce` and `bind_hyp`, the only operations
needing privileged construction. `param_apply f x` routes by structural
signature on hash-consed trees (O(1) tree-id comparison): if `f`'s sig is
a kernel-op sig the handler runs raw, otherwise the parametric walker (§4)
reduces it.

Real-world effects (IO, syscalls) are *not* kernel operations
and not entries in Σ — they are library values in the `Eff` free monad
(§15), performed by an outermost driver at the program boundary; the
substrate stays pure (which hash-consing requires). Types, validators,
recognizers, case eliminators, and the MetaShape convention all live in the
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
    [§3 The `CheckerResult` monad],
    [`Result E A`, `CheckerError` variants, Kleisli composition, the *verdict-vs-error principle*],

    [§4 The parametric walker and `Tree_p`],
    [Walker as Kleisli-lifted binary apply, `Tree_p` as greatest fixed point, soundness discipline],

    [§5 The dispatcher],
    [`param_apply` over the fixed two-op Σ; routing to the registered handler; wait-forms; why the dispatcher is *not* the effect system],

    [§6 Stuck forms and neutrals],
    [Stuck forms from any pinned handler, the generalized H-rule, cascading-failure story],

    [§7 The kernel primitives], [Operational semantics of `hyp_reduce`, `bind_hyp`, `param_apply`],
    [§8 Boundary operations and checked values], [`param_lift`, `typecheck`, `checked`, `strip_validated`],
    [§9 Elaboration and tests], [Syntactic transformation; tests as first-class; `: T` as test sugar],
    [§10 Strip and erasure], [`strip` as a tree function; PCC story],
    [§11 Types and validators], [Types-as-wait-forms; MetaShape; validators-as-values],
    [§12 Library types], [Each library type under the framework, including `Type` itself],
    [§13 Cubical extensions], [`I`, `Path`, `comp`, `Glue`, `ua`],
    [§14 Soundness via tests],
    [Four categories of runnable assertions; foundational conjecture stays open; environment probes via effectful tests],

    [§15 Effects], [Effects as a library free monad `Eff R X`; operations, handlers, effect rows; the outermost driver],
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
(the `Eff` free monad, handlers, effect rows, the driver). §16 highlights
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
host-side identity. The host (`src/core/tree.ts`) maintains a table mapping
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

The host runtime in `src/core/tree.ts` implements hash-consing and `apply`.
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
    [`Tree`], [Any tree in the substrate (§2.1).],
    [`Tree_p`], [Trees on which the parametric walker is closed under `Ok` (§4).],
    [`Bool`], [`true = △` / `false = △ △` — raw shapes branded by the Bool view (`lib/prelude.disp`, §2.7); see §12.],
    [`List X`], [Standard cons/nil list of `X`-trees; iterated `pair`s.],
    [`Optional X`], [`Some x` / `None`; sentinel-tagged.],
    [`Span`],
    [Source-span record (file, start, end) attached to error variants for diagnostics; opaque tree at the substrate level.],

    [`Symbol`], [A fixed tree value identifying a handler / constant — distinct from any user-constructed tree.],
    [`Functor`],
    [Synonym for `Tree_p`; conventionally a morphism-action function consumed by `transp` (§13). Discrete types carry the sentinel `trivial_functor` (identity transport); non-discrete types carry a real morphism action. Details in §13.],

    [`Respond`],
    [`recognizer_params -> self -> frame -> Action` (typed `RespondShape`, §12.6). The universal "respond to an elimination frame" function carried by each type's meta — given the type-former parameters, the reconstructed self-neutral, and the frame. Generalizes the earlier `Applicable`; the older `NeutralMeta -> Frame -> Action` is the same function with `meta` unpacked at the call site.],

    [`Frame`],
    [`Tree_p`. The thing applied to a neutral — an argument (Π), a projection-by-name selector (`acc`; Σ/Record/Telescope), a case-pair (inductive), a dimension (Path), a candidate value (Type). Untagged; the stored type interprets it.],

    [`Action`],
    [`Extend Type | Reduce Tree`. The protocol `hyp_reduce` consumes from a type's `respond` (§7): `Extend T'` keeps the elimination stuck at a new stored type; `Reduce v` resolves it to a value (both *bare* — §7.5). A frame a type does not accept is rejected as `Extend InvalidType` (§12.3) — the dead-state type — so there is no third "reject" tag. `Return v := Reduce (Ok v)` is this spec's *notation* for the verdict (H-rule) case, not a third arm (the library writes `Reduce (Ok v)` out).],

    [`Spec`],
    [A runnable behavioral property attached to a type's meta (the `behavioral_specs` field, §11.2). Layer-neutral name so the core metadata convention does not depend on the cubical extension; realized concretely as `Path` once §13 is in scope.],

    [`Path`], [`Pi I` alias from §13; the concrete realization of `Spec` used in `behavioral_specs`.],
    [`ROOT_SIG`],
    [Canonical reader tree (= the blessed `pair_fst`): `ROOT_SIG x` is the handler signature rooting `x`. Walker-resolved on seals (§4.2); a fixed projection onto the public descriptor.],

    [`STORED_TYPE`],
    [Canonical reader tree: `STORED_TYPE x` is the stored-type slot of a neutral's meta (`neutral_meta_type (pair_snd x)`), projected atomically so the meta's payload is never surfaced. Walker-resolved on seals (§4.2).],
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
  carry their field names in their field table's header (§2.6) and are
  read by name through the cut.
  We use the typed presentation throughout for readability; the
  implementation walks the structure either way.
]

== Records, arrays, and coproducts <sec:sugar>

Three notations recur throughout this document — record values and types in
braces, array literals in brackets, coproduct (variant) types with bars. All
three are surface sugar over the pairs of §2; their desugaring is fixed
here once so later sections use them without restating the encoding. (`≡`
below reads "desugars to.")

The one idea to carry out: records, coproducts, and functions are not three
encodings but three uses of *one shape*, driven by *one* eliminator. Every value
is `fork(descriptor, payload)` — the descriptor (`pair_fst`) fixes which
capabilities the value has — and a single operation, the *cut*, eliminates them
all; projection, `match`, and function application are that one operation at
different arities. We build to it in order: arrays first (the trivial case, no
cut), then the shared shape and its two halves, the cut that joins them, the one
knob that splits records from matches, the cut's type, and finally each construct
as an instance. §12 gives the types (`Record`, `Coproduct`, `Sigma`, `Pi`); §10
the erasure that lowers the shape back to positional data.

=== Arrays

```disp
[]              ≡  nil                            // nil  : List X
[a, b, c]       ≡  cons a (cons b (cons c nil))   // cons : X -> List X -> List X
```

An array literal is a `List` (§2 glossary): `nil` is empty, `cons h t` prepends. At
the substrate these are the iterated-pair encoding (`nil = LEAF`,
`cons = pair`), so a list is a right-nested fork chain ending in `LEAF`.
Lists carry no names; position is the only index.

=== The one shape and the cut

Every value is `fork(descriptor, payload)`, used two ways: a *variant* is the
data half (a tag plus a payload — a coproduct value), a *product* the consumer
half (a callable table of fields). Their types index a finite name set by two
per-field families — `A n` and `B n`, each field's payload domain and result —
from which `Field n = A n -> B n`, a `FieldTable` is a name header plus one field
per name, a `Variant` is `(tag : Name, pay : A tag)`, and a `Product` is
`(c : Variant) -> B (tag c)`. (Precise dependent forms in §12.4.) Five combinators
build and join the halves:

```disp
inj : (tag : Name) -> A tag -> Variant :=
  {tag, pay} -> pair tag pay
```
Builds a *variant* — the tag in `pair_fst`, the payload in `pair_snd`, told apart
by one O(1) `tree_eq` (§2.2). A constructor application is exactly this injection:
`V e ≡ inj V e`.

```disp
acc : (n : Name) -> Variant :=
  {n} -> inj n unit
```
The nullary variant (payload `unit`); the accessor a projection cuts against:
`r.a ≡ r (acc a)`.

```disp
proj : (P : FieldTable) -> (n : Name) -> Field n :=
  {P, n} -> path_at (index_of (pair_fst P) n) (pair_snd P)
```
Selects a field by name: reads the table's name header for the field's position,
then indexes the payload. Internal to the cut — no surface syntax of its own.

```disp
cut : FieldTable -> (c : Variant) -> B (tag c) :=
  {P, c} -> (proj P (pair_fst c)) (pair_snd c)
```
The *cut body*: select `P`'s field named by `c`'s tag, then feed that field `c`'s
payload.

```disp
prod : FieldTable -> Product :=
  {P} -> wait cut P
```
Makes a field table *callable* — the product behind both `{ a := x }` and
`match`. `prod P` is a wait-form (§5.4), so applying it is raw substrate
reduction, with no triage on the argument.

The *cut* applies a product to a variant:

```disp
(prod P) c   →   (proj P (pair_fst c)) (pair_snd c)   :   B (tag c)
```

It typechecks exactly when `tag c ∈ names P` and `pay c : A (tag c)`, and its
result type `B (tag c)` depends on the tag (a `Σ`-elimination). If the tag names
no field, the elimination has no inhabitant and fails as a verdict (`Ok false`),
never a host crash.

The two faces differ by one knob — *do the fields read the payload?* A
*record*'s fields are `const`-wrapped and ignore it (`A n = Unit`), so
`r.a → x`; a *match*'s are raw handlers that use it (`B n = R`), so
`match v → handler pay`. §12.4 gives the dependent rule under
`Record`/`Coproduct`; the `Σ`/`Π` grid later in §12 unifies both polarities.

#note[
  *The `fork(LEAF, _)` shape is shared — by design.* A `const`-wrapped record
  field (`const x = fork(LEAF, x)`) has the same shape as `Ok x` (§3.4) and
  `succ x` (§12) — the substrate's `fork(LEAF, _)` node is reused across all
  three, *intentionally*: reusing one minimal node keeps hash-cons sharing
  maximal and the eliminators uniform. A field thunk is therefore told apart from
  those by *context* (its position in a product's table), not by its root. Where
  canonical fields must be self-describing, a dedicated tag distinguishes them;
  the cut itself never needs to, since it reaches fields by name through `proj`.

  *Consequence: recognizers are sound only on type-respecting inputs.* Because
  the shapes collide, a recognizer is not injective on *arbitrary* trees:
  `nat_recognizer` accepts `Ok zero` (it *is* `succ zero` structurally), and
  `is_ok (succ n) = true`. This is harmless because the type discipline never feeds
  a `Result` where a `Nat` is expected — every value reaches a recognizer through
  a typed position. The structural-recognition soundness claim (§8.7, §14) is
  therefore scoped to inputs that already respect the ambient typing, not to all
  trees; a recognizer is a decision procedure for its type *among well-typed
  candidates*, not a universal tree classifier.
]

=== Record values

A record is a product whose fields *ignore* the payload they are fed — each
field is wrapped in `const` (`const x = fork(LEAF, x)`, so `const x y = x`):

```disp
{ a := x, b := y, c := z }   ≡  prod (pair [a, b, c] (pair (const x) (pair (const y) (const z))))
//                                          ^ name header     ^ const-wrapped field thunks (a Σ-chain)
```

The field table — the name header plus the field thunks — is the product's
`wait_meta`. The header is one hash-consed node: records with the same names in
the same order share it, and `tree_eq` on two headers is O(1) (§2.2). Names live
in the *value* (inside its field table), not only in the type, so a recognizer
can decide field membership from the value alone (§12). A coproduct value tags
*which one* it is in `pair_fst`; a record's field table names *all the fields* in
its header; the `prod` wrapper makes that table callable so the cut drives both.

Projection is the cut against a nullary accessor — not a positional shortcut:

```disp
r.a   ≡   r (acc a)   →   cut F (inj a unit)
                       →   (proj F a) unit   →   (const x) unit   →   x
```

where `F` is `r`'s field table. The field name is validated by the cut's type
(§12): an out-of-range name has no field, so the elimination has no inhabitant
and fails as a verdict, never a host error. For a *literal* name the whole chain
is a subterm closed in `r`, so it β-reduces — by ordinary `apply`, with no
certificate — and `const x` constant-folds to `x`; hash-cons shares that normal
form across every projection of the same field. `strip` (§10) removes the
`prod`/`const` scaffolding on a validated program, leaving the bare positional
path `pair_fst (pair_snd^idx payload)`. §12 gives the dependent type of
projection and the neutral case.

#note[
  *One record discipline, metadata included.* Every `{…}` record is a product —
  metadata records too. A type's MetaShape meta
  (`{recognizer_params, functor, respond, …}`, §11.2) is such a record, which is
  exactly why the kernel reads it by name: `meta_get m "respond"` *is* the
  projection `m.respond`, i.e. the cut `m (acc respond)` against `m`'s own field
  table, with no field list threaded in. What stays positional is not metadata
  but the substrate *scaffolding* that is not a record at all — the `wait`-form
  tree shape and a neutral's spine, reached by fixed projections (`type_meta`,
  `neutral_meta_type`; the iterated-pair layout of §2's layering note). The
  field table sits *inside* that scaffolding, as the meta product's `wait_meta`.
  Coproducts split the same way: the interpreter's own enums hand-build minimal
  tags, while library and user coproducts tag by name.
]

=== Record types

```disp
{ a : A, b : B, c : C }   ≡  Record [(a, A), (b, B), (c, C)]
```

A record type is the `Record` former (§12) over the array of name/type
entries — a finite-index `Pi` (a dependent function from field name to field
type). `Record` recognizes a value by checking the name header of its field
table against the declared names (O(1)) and the table's payload against the
underlying `Sigma` chain — so a later field's type may depend on earlier fields
(`{ n : Nat, v : Vec n }`), and a value whose names don't match is rejected,
distinguishing `{a:Nat,b:Nat}` from `{p:Nat,q:Nat}` at the value level. Before
§11 a record-type annotation is read positionally and validated only once
`Record` exists.

=== Coproducts

```disp
C := V1 T1 | V2 T2 | ... | Vn Tn
  ≡  C := Coproduct [(V1, T1), ..., (Vn, Tn)]
```

A coproduct is the `Coproduct` former (§12) over its constructors — a
finite-tag `Sigma`. A constructor application `Vi e` is the injection
`inj Vi e = pair Vi e`: a distinct tag tree in `pair_fst`, the payload in
`pair_snd` (a nullary constructor carries the unit payload). Constructors are
*declared* with the arity-indexed `injN` family — the §2.6 "data declaration"
until surface syntax grows one — which fix the tag and pack an N-field payload
as right-nested pairs (exactly the shape a match arm's binders destructure):

```disp
inj0 tag       := inj tag t                       // nullary: unit payload
inj1 tag a     := inj tag a
inj2 tag a b   := inj tag (pair a b)
inj3 tag a b c := inj tag (pair a (pair b c))
// e.g.  Ok := inj1 "Ok";  Err := inj0 "Err";  Extend := inj1 "Extend"
```

Elimination is the
cut against a product of handlers — a *match* is a product whose fields *use*
the payload:

```disp
match v {
  V1 x => b1
  ...
  Vn x => bn
}
  ≡  cases v        // cases = prod (pair [V1, ..., Vn] (pair ({x} -> b1) ... ({x} -> bn)))
```

The case product `cases` is built like a record, but its fields are *raw
handlers* (not `const`-wrapped), so the cut `cases v → (proj cases (pair_fst v))
(pair_snd v)` selects the handler named by `v`'s tag and *feeds it* `v`'s payload
— one `tree_eq` to discriminate (§2.2), then a branch jump. This one rule
subsumes `select` (the two-constructor Bool case), the per-type recursors of
§12, and raw `triage` decomposition. A record's field access `r (acc a)` is the
*same* call with `const` fields and a `unit` payload — the only difference is the
`const`, i.e. whether the field reads the payload.

#note[
  *Tags and O(1) discrimination.* A coproduct's tag sits in `pair_fst`, so the
  cut discriminates by one `tree_eq` (O(1), §2.2). Library and user coproducts
  tag by interned constructor name. The kernel enums the interpreter itself
  returns — `Result` (`Ok` / `Err`, §3.4) and `Action` (`Extend` / `Reduce`,
  §7) — are declared the same way, by hand via `injN` with interned-name tags
  (`inj1 "Ok"`, `inj1 "Extend"`, …) rather than produced by `Coproduct`: only
  because they are needed during bootstrap, before `Coproduct` itself exists.
  Same cut, same tag policy.

  `Bool` is the nullary coproduct `< true, false >`, shape-encoded on the raw
  trees `true = △`, `false = △ △` (§2.7) and branded by the view iso in its
  meta — exactly `Nat`'s mechanism. It is the substrate's branching primitive:
  `select`/`cond` build the triage table and apply it to the bool, the
  consumer-drives direction §2.7 fixes. (Scott-encoded until 2026-07-07 — the
  cut internalized, a value carrying its own handler-selection; §2.7 records
  why that pole was retired.)
]

== Polarity: who drives an elimination <sec:polarity>

§2.6 ends on a convention it does not justify: the table is the callable side
(`prod P = wait cut P`) and data stays bare. The cut as a specification is
symmetric (a variant meets a field table; the tag selects a field), but
application is not: one operand must be the function. This subsection fixes the
direction as a principle rather than a habit, because the same choice recurs at
every level of the system. In its general form it is the choice between *data
with an attached eliminator* (a value that dispatches when applied) and *raw
data with an external eliminator* (an inert tree that its consumer inspects).

=== One negative former

Everything in the kernel that is defined by how it *responds* is a single
construction, a handler wait-adjoined to state:

```disp
prod P   =  wait cut         (names, fields)           // records and match tables
T        =  wait recognizer  meta                      // types (§11)
h        =  wait hyp_reduce  {stored_type, payload}    // neutrals
cell     =  wait op          meta                      // telescope cells (§12)
```

A wait-form is a coalgebra: state in the meta, behaviour in the handler. These
are the *negative* values, the ones defined by observation. Everything defined
by how it is *built* stays a raw tree: `inj`-tagged variants, `zero = △` and
`succ n = fork(△, n)`, `cons` chains, interned strings. Even codata keeps its
state raw: a `Stream` value is `inj "gen" (pair step seed)`, a defunctionalized
generator that `out` observes one step at a time, not a self-dispatching
object. These are the *positive* values.

=== The shift happens at abstraction, not construction

The fragments meet in the neutral. A hypothesis of a positive type has no
structure to inspect, so the kernel hands out a variable wrapped in its type's
behaviour: `wait hyp_reduce {stored_type := Nat, ...}` is Nat's `respond`
attached to a variable. Concrete positives are inspected (recognizers, the cut
reading a tag, `triage`); abstract positives respond (a frame routed through
`hyp_reduce` to the respond). The direction rule of the whole system is then
one sentence:

*A value drives its own elimination exactly when it is a wait-form (every
neutral, every product); otherwise its consumer drives, exactly as the
substrate's triage rule does.*

Both poles of the classical duality are therefore present, assigned by polarity
rather than chosen globally. Eliminating a concrete positive is consumer-driven
(`cases v`; `triage l s f x`). Eliminating anything negative or abstract is
scrutinee-driven (`r (acc a)`; `h {motive, cases}`). And the abstract direction
is not a preference: a hypothesis in argument position is invisible to the
machinery, so only a hypothesis in function position can route its frame to a
respond. The one genuine decision the kernel ever made here is *when* the shift
into the negative fragment happens — at abstraction time, not at construction
time — so that concrete data stays raw.

=== The substrate picked a side, and its sibling picked the other

The concrete direction is not a kernel invention either. Rule 3 of the
substrate (§2.1) is triage: a table in function position inspects its
argument's shape. Tree calculus has a sibling formulation — the fork rule of
Jay's book (*Reflective Programs in Tree Calculus*) —

```
△(△ w x) y z  ⟶  z w x
```

in which the function's own fork children are handed *to the argument*: data in
function position feeds itself to whatever consumer arrives, the visitor
pattern as a reduction rule. Both formulations are universal; they differ in
which pole is primitive. A language whose types are predicates that walk
candidate values wants intensional inspection to be the one-rule operation, so
disp sits on the triage formulation, and the §2.6 cut is that rule lifted from
shapes to names.

=== Reflection makes the choice semantic

In an ordinary language the two poles are observationally equivalent:
defunctionalizing a Scott-encoded sum into tags-plus-dispatch, or
refunctionalizing back, preserves behaviour, so the choice is style and
performance. Disp's kernel is an observer that can tell the poles apart: types
are recognizers that walk raw structure, conversion is `tree_eq`, and the
elaborator reads record headers off compiled trees. Under that much reflection
the encoding of data is semantics, and three costs pin the positive fragment to
raw trees:

- every self-dispatching datum would carry its wrapper (the unbounded runtime
  population pays, and nested data pays per level), while wrapping the table
  side is paid once per hash-consed table;
- a raw tree could no longer inhabit an inductive type, so the shape encodings
  and their view isos (§12) lose their subject: `zero = △` stops being true;
- the substrate's own values become second-class citizens of the language
  built on them, forfeiting the floor (§2.1) and the Tree-as-inductive rung
  (§11.2).

#note[
  *The duality has a literature.* ADTs versus objects: Reynolds, "User-defined
  types and procedural data structures as complementary approaches to data
  abstraction" (1975); Cook, "On understanding data abstraction, revisited"
  (2009). The transform between the poles: defunctionalization (Reynolds 1972)
  and refunctionalization (Danvy and collaborators); a callable sum
  (`wait cut_flip c`) would be the refunctionalized form of §2.6's
  tags-plus-cut. Polarity and shifts: Levy's call-by-push-value (value types
  against computation types; the wait-form is the shift into the negative
  fragment, made visible as a dozen substrate nodes); focusing (Andreoli;
  Zeilberger). Who drives as a critical pair: Curien and Herbelin, "The duality
  of computation" (the symmetric cut of λ̄μμ̃ reduces either way, and choosing
  the winner is choosing call-by-value against call-by-name); "Grokking the
  Sequent Calculus" (ICFP 2024) is a readable entry. Codata by observations:
  copatterns (Abel, Pientka, Thibodeau, Setzer, POPL 2013); a type's `respond`
  is a copattern table over frames. Interconversion as a whole-program
  transform: Rendel, Trieflinger, Ostermann (ICFP 2015); Downen, Sullivan,
  Ariola, Peyton Jones, "Codata in action" (ESOP 2019).
]

One value class used to sit on the wrong side of the line: Scott-encoded
`true`/`false` were data that drives (`cond c a b` applied the bool to its
branches), the one refunctionalized citizen in the kernel — tolerable because
both variants were nullary and fixed-shape, so recognition was two exact
`tree_eq`s with no payload to walk. Re-encoded 2026-07-07 to the raw shapes
`true = △`, `false = △ △`, with branching by triage (`select`/`cond` build the
case table and apply it to the bool): a shape-encoded coproduct exactly like
`Nat` and `Ord`, branded by the view iso in its meta. No exceptions remain.
(One consequence to know: `true = △ = zero = nil = refl` — the smallest
constants of different types share one tree, per the §2.6 collision doctrine.)

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
    [`hyp_reduce`, when the stored type isn't a recognized
      type wait-form],

    [`TypeMismatch`],
    [contract boundaries (`checked` argument check, any typed-
      function application), when a recognizer returns `Ok false` on
      a value where true was contractually required],

    [`Malformed`],
    [any handler, when its meta doesn't fit the expected shape
      (currently silent in some paths — see §15 for the planned
      diagnostic story)],
  ),
  caption: [`CheckerError` variants and their kernel-handler origins.],
)

#note[
  *Verdict vs error.* A recognizer's `Ok false` is *data*, not an
  error — it means "this value is not an inhabitant of the queried
  type," which is a legitimate answer to a query. Errors flow
  through `Err` only when something is *broken*: parametricity
  violated, hypothesis escaped, contract-mandated true received false,
  meta malformed. Query-style callers (`typecheck`, `strip_validated`)
  see `Ok true` / `Ok false` and pattern-match. Contract-style callers
  (`checked` application) raise `TypeMismatch` because their callers
  promised the value would fit.
]

#note[
  *Most of this vocabulary is meant to disappear.* Of the five variants, only
  `Parametricity` and `Escape` report a genuine soundness event — user code
  tried to forge or reflect on a hypothesis, or let one escape its scope. The
  other three describe a type that was *built* wrong, not a value that fails
  to inhabit it: `Malformed` (meta off-shape), `NotApplicable` (stored type
  not a function / not a type), and `TypeMismatch` (a contract promised `true`
  and got `false`). Those are discharged earlier — by validating every type
  annotation against `Type` at elaboration, and by the validate-then-strip
  discipline (§10) under which a checked contract can only fire on the
  un-validated path. On well-formed, validated input the checker never raises
  them, so the steady-state recognizer is effectively `Tree -> Tree -> Bool`:
  a verdict, with `Parametricity` / `Escape` the only residual error channel.

  *Folding `Err` to `false` is conservative only for monotone recognizers.* When
  a recognizer runs a sub-check under the walker and folds its `Parametricity`
  failure to `Ok false` (the kernel does this — §6), it *rejects* the value,
  which is sound. This stays sound exactly while recognizers are *monotone* in
  their sub-verdicts: a sub-check turning `false` may only weaken the answer,
  never strengthen it. A recognizer that negated a sub-verdict
  (`{v} -> not (sub v)`) would turn a forgery's `false` into `true` and accept it.
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
    true => k (ok_value r)
    false => r
  }

// Functor map. Transforms the Ok payload; leaves Err alone.
map := {r, f} ->
  match (is_ok r) {
    true => Ok (f (ok_value r))
    false => r
  }

// Error transform. Leaves Ok alone; transforms the Err payload.
// Useful for re-tagging errors as they cross layer boundaries.
map_err := {r, f} ->
  match (is_ok r) {
    true => r
    false => Err (f (err_value r))
  }

// Error handler. Ok passes through; Err is handed to the handler,
// which can itself produce Ok (recovery) or another Err (rethrow).
// Equivalent to orElse / try-catch / Lean's Except.tryCatch.
catch := {r, handler} ->
  match (is_ok r) {
    true => r
    false => handler (err_value r)
  }

// Assertion sugar. Lifts a bool into Result Unit, supplying the
// error for the false case. Useful for inline pre-conditions inside
// a bind chain.
guard := {cond, err} ->
  match cond {
    true => Ok t                    // t = unit at this layer
    false => Err err
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
what to re-raise, instead of pattern-matching on opaque `Err`
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
an error. `Ok true` and `Ok false` are both successful runs; `Err _`
signals something is broken (parametricity, escape, malformed
input). Callers pattern-match on the verdict:

```disp
bind (typecheck T v) ({verdict} ->
  match verdict {
    true => /* it's an inhabitant; proceed */
    false => /* it's not; do whatever the query API demands */
  })
```

*Contract patterns.* When the caller has a contractual guarantee
that the value fits — applying a `f : Pi A B` to an arg, where the
type system says the arg must be in `A` — receiving `Ok false` from
the recognizer means the program is broken, not that "we got a
no." Here the receiving handler raises `TypeMismatch`:

```disp
bind (param_apply A arg) ({verdict} ->
  match verdict {
    true => /* arg fits A; proceed */
    false => Err (TypeMismatch { expected = A, actual = arg, span })
  })
```

This split is what keeps soundness errors visible. Folding any
failure into `false` — the obvious shortcut — would silently mask a
parametricity violation inside a recognizer body as "this isn't an
inhabitant." Here, every recognizer is required to *be* a recognizer
(returning a verdict, never absorbing soundness errors), and the
lifting of `false → Err` happens only where it is contractually
justified (`checked` and similar typed boundaries).

We elevate this split to a *named design principle*, referenced
elsewhere in the spec:

#note[
  *The verdict-vs-error principle.* `Ok true` / `Ok false` are *verdicts*
  — legitimate data answers to "is `v` an inhabitant of `T`?". `Err _`
  is reserved for *soundness-level breakage* — parametricity violation,
  escape, malformed meta, broken-contract `TypeMismatch`. Recognizer
  bodies must never fold the latter into the former. Contract
  boundaries (and only contract boundaries) lift verdict-`false` into
  `Err TypeMismatch` because at those sites the call was promised
  to fit.
]

#note[
  *`CheckerResult` is the kernel's monad; `Eff` is a separate library
  monad.* `CheckerResult` is the return type of the two kernel handlers
  (§5, §7) and of every recognizer — the *checking* monad. Effects are a
  distinct library monad, the free monad `Eff R X` (§15), whose `bind`
  sequences effect *values*; it does not flow through the kernel and is
  not `CheckerResult`. The two only meet at the boundary: the effect
  *driver* (§15.6) and the `is_closed` sanitizer it runs are the points
  where a checked tree crosses into effect execution. So there is no
  unified "one monad for everything" — checking and effecting are
  deliberately separate, which is what keeps the substrate (and hence
  the checker) pure.
]

= The parametric walker and `Tree_p` <sec:tree-p>

== Motivating problem

Disp's type system relies on *hypotheses* — fresh tree values minted by
the kernel that represent "an unknown value of type `A`." Hypotheses
have a pinned signature (`pair_fst h = checker_sig hyp_reduce` for a
kernel-minted neutral `h`). The seal-producing kernel operation
(`hyp_reduce`, §5, §7) has a sig that user code must not be able to
forge — forging would let user-side trees masquerade as kernel-minted
neutrals (a token the H-rule trusts), breaking the soundness of
dispatch.

The fix: define the *parametric walker* — a Kleisli-lifted version
of `apply`, over the fixed Σ, that performs the same reduction
but rejects two introspection patterns. The walker, the dispatcher, and
the stem-forge check all consult the *same* Σ — they are aspects of one
mechanism, not independent layers. `Tree_p` is then the largest subset of
trees on which the walker, applied to pairs from `Tree_p × Tree_p`, never
trips a rejection.

#note[
  We keep the notation `w_Σ` / `Tree_p(Σ)` below to name the dependence on Σ;
  since Σ is a fixed constant (§5), they denote one walker and one set.
]

== The walker as a Kleisli-lifted binary operation

The walker `w_Σ : Tree × Tree → CheckerResult(Tree)` is the Kleisli
lift of binary `apply`, consulting *one* pinned-sig set derived from Σ —
`seal(Σ)`, the trusted-token producers. (Earlier drafts carried a second
set `funnel(Σ)` for host-effect sigs forced through `postulate`'s
sanitizer; with effects now a pure library construction (§15) there are no
host sigs to protect, so `funnel(Σ) = ∅` and `forge(Σ) = seal(Σ)`.)

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
      escape scan). Just `hyp_reduce` today; extensible to any future
      stuck-form producer (e.g. `async_pending`, §15). A seal needs
      *both* protections: unforgeable as a value (construction) and
      opaque to *general* inspection (triage) — only the §4.2 canonical
      readers may expose its public descriptor (root sig, stored type),
      never its payload.],

    [`forge(Σ) := seal(Σ)`],
    [The construction-protected set the stem-forge clause consults. With
      `funnel(Σ) = ∅` (effects are library, §15) it *coincides with*
      `seal(Σ)`. It *excludes* the other kernel operation `bind_hyp`
      (see note): library code legitimately constructs `bind_hyp`
      invocation wait-forms under the walker, since the dispatcher routes
      them to the registered handler regardless (§5.4).],
  ),
  caption: [The pinned-sig set the walker consults.],
)

The walker reduces `apply(f, x)` by the *same move* the §5 dispatcher uses:
*match the operator `f`, short-circuit to a fixed behavior when it matches a
known pattern, else fall through to substrate reduction.* The two differ only in
*what part of `f` is matched* and *what the matched behavior may do* — the
unified picture is §4.3. The walker's own layer matches the *whole* `f` by
hash-cons identity against a small fixed set of *canonical reader trees*, each
resolving to a fixed result (its body is never run):

+ *I-shortcut.* `w_Σ(I, x) = Ok x`. Polymorphic identity — returns its argument,
  constructs nothing. (Running `I`'s body `S K K x` would create intermediate
  forks that trip Stem-forge below; resolving to `x` directly sidesteps that.)

+ *Root-sig read.* `w_Σ(ROOT_SIG, x) = Ok (pair_fst x)` — the signature rooting
  `x` (`ROOT_SIG` *is* the canonical `pair_fst` tree). On a seal this returns the
  *public* sig, identical for every same-typed seal; a fixed projection.

+ *Stored-type read.* `w_Σ(STORED_TYPE, x) = Ok (neutral_meta_type (pair_snd x))`
  — the stored-type slot of `x`'s meta. *Atomic*: it must project straight to the
  type without ever surfacing the intermediate meta, whose payload is the
  protected content (§4.3); `pair_snd` itself is therefore *not* a reader.
  Uniform across same-typed seals.

These three are *observe-only* — each is a total function of `x`'s public
descriptor (root sig, stored type), so it returns identical results on any two
seals of the same type and leaks nothing seal-distinguishing (the litmus test,
§4.3). They short-circuit *before* the guard clauses below, so they never trip
them — exactly as `I` always has. `is_neutral`, `has_sig`, and
`neutral_type` (§12.8) are ordinary library code over these readers plus the
O(1) `tree_eq`; *value*-decomposition of a neutral (e.g. "is the unknown Nat a
fork?") is a different operation — it *applies* the neutral, routing to
`hyp_reduce` and staying symbolic (§6, §12.3) — and is not a reader.

When `f` is not a canonical reader, the walker follows `apply`'s rules, rejecting
two introspection patterns:

+ *Stem-forge.* `w_Σ(stem(a), x)` would reduce to `fork(a, x)`; if
  `tree_eq a (checker_sig h)` for some `h ∈ forge(Σ)`, return `Err
  (Parametricity { kind = StemForge, where = fork(a, x), span })`. Members of
  `forge(Σ)` can't be fabricated — only the raw construct-layer (handlers, §5)
  may build them.

+ *Triage-on-seal.* `w_Σ(f, x)` would fire the triage rule on `x`; if
  `pair_fst(x) = checker_sig h` for some `h ∈ seal(Σ)`, return `Err
  (Parametricity { kind = TriageReflect, where = x, span })`. A seal can't be
  opened by *general* triage; the only sanctioned reads are the canonical readers
  above, which expose only the public descriptor.

All other applications follow `apply`'s rules and return `Ok <result>`.

#note[
  *Tokens vs. operations.* An earlier framing protected *every*
  handler sig against construction, justified as "forging a wait-form
  invokes privileged code with chosen inputs." That rationale
  presupposes the dispatcher trusting a wait-form's *embedded*
  handler; once the dispatcher routes pinned sigs to the *registered*
  handler instead (§5.4, §7.5), forging a wait-form can only
  re-invoke the genuine handler — which is built to accept arbitrary
  inputs (`bind_hyp` escape-checks). So construction protection is
  *not* needed for the kernel operation `bind_hyp`, and is in fact
  *harmful* there: library recognizers run under the walker (§6.3.1)
  and must construct `wait bind_hyp …` invocations to mint their
  hypotheses (Pi's body check, §12). What genuinely needs
  unforgeability is the *value* a stuck-form producer mints (a neutral
  is a token the H-rule trusts). Hence `forge(Σ) = seal(Σ)`, excluding
  `bind_hyp`. Inspection protection is the same set: only `seal(Σ)`,
  whose minted tokens are introspection targets. (The earlier `funnel`
  set — host-effect sigs forced through `postulate` — is gone: effects
  are a library free monad now (§15), so there are no host sigs in Σ to
  protect.)
]

== One LHS-dispatch, two layers <sec:two-layer>

The walker's reader layer (§4.2) and the §5 dispatcher's handler layer are the
*same mechanism* at two match granularities. Both reduce `apply(f, x)` by
pattern-matching the operator `f` and short-circuiting to a registered behavior
when it matches; on no match, ordinary substrate reduction proceeds. They differ
on exactly two correlated points:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [], [*Handler layer (§5)*], [*Reader layer (§4.2)*],
    [*Matches*],
    [a *subterm* of `f` — its signature `pair_fst f` — against Σ's pinned sigs],
    [the *whole* `f`, by hash-cons identity, against the canonical reader trees],

    [*Behavior may*],
    [*construct* seal-rooted trees, branch, recurse, do IO — so it runs _raw_ (body executes outside the guards)],
    [only *observe* (a fixed projection) — so it resolves to a _denotation_ (body never runs)],

    [*Membership*],
    [environment-relative: Σ is a value, varied per call],
    [substrate-fixed: always present, part of the reduction contract],

    [*Soundness*], [trusted *by audit* (TCB; §14)], [safe *by construction* (the litmus test below)],
  ),
  caption: [Two layers of one LHS-dispatch.],
)

Both are implemented the identical way — *pattern-match the LHS to short-circuit
`apply(lhs, rhs)`*; the lone essential difference is that the handler layer
matches a *subterm* of `f` (its sig) while the reader layer matches the *whole
term* `f`. Everything else in the table follows from one root question: *does
the matched behavior construct seal-rooted trees, or only observe them?* A
constructor must run raw (it would otherwise trip Stem-forge) and is an arbitrary
program (so it cannot be a denotation), and arbitrary programs arrive as wait-form
*families* sharing one sig (so they are keyed by that sig and need the
route-to-registered discipline of §5.4). An observer is a fixed projection (so it
resolves to a denotation, needs no raw region, and is one *canonical* tree, keyed
by identity, self-routing). `I` is the degenerate
observer — it observes *nothing* and returns its argument; `ROOT_SIG` /
`STORED_TYPE` observe the public descriptor.

#note[
  *The litmus test for the reader layer.* A canonical reader is sound iff it is a
  *total function of the seal's public descriptor* — its root signature and its
  stored type — and therefore returns *identical results on any two seals sharing
  that descriptor*. `I` (returns its argument), `ROOT_SIG` (returns the public
  sig), and `STORED_TYPE` (returns the guarded type) all pass: none can
  distinguish two same-typed hypotheses. A would-be reader exposing a seal's
  *payload* (its identity or bound body) would fail — two same-typed hypotheses
  differ there, so `pair_snd` is excluded and `STORED_TYPE` must be atomic. This
  is strictly *more conservative* than a carve-out the system already grants:
  `tree_eq` on neutrals (used by the H-rule and the escape scan, §6, §8.1) can
  already tell two same-typed seals apart, which the descriptor reads cannot — so
  the reader layer widens no attack surface beyond `tree_eq`'s.
]

== Why `I` is the only *passthrough* carve-out

(`I` is the only carve-out that *passes an argument through* a reduction; the
other two reader-layer trees, `ROOT_SIG` / `STORED_TYPE`, are *observations* — a
distinct kind, covered in §4.2–§4.3. This section is about why no reduction rule
beyond `I` needs the passthrough treatment.)

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
operationally, but it explains why no other *passthrough* carve-out is
needed: no other tree represents a parametric operation that introspects
nothing. (The reader layer adds two *observation* carve-outs alongside
`I` — `ROOT_SIG` / `STORED_TYPE`, §4.2 — but those return public
descriptor facts rather than passing the argument through, and are
justified by the §4.3 litmus test, not by this passthrough argument.)

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

*Σ is fixed, so `Tree_p` is a single set.* Earlier drafts varied Σ per
caller and noted a monotonicity law (`Σ ⊆ Σ'` ⇒ `Tree_p(Σ') ⊆ Tree_p(Σ)`)
to justify environment substitution. With Σ now the fixed two-op kernel
constant (§5) there is nothing to range over: one Σ, one `Tree_p`. (Should
a future seal-producer be added — e.g. `async_pending`, §15 — it would
enlarge the protected set and shrink `Tree_p` accordingly; that is the only
residue of the old monotonicity story, and it is a kernel-design change,
not a per-call parameter.)

#note[
  Tree_p(Σ) is a property of the walker, not of `CheckerResult`. The
  monad supplies the failure container; the *content* of "what counts
  as parametric" lives in the walker's rejection clauses. The
  definition is *semantic* and undecidable in general — you cannot
  tell by inspection alone whether an arbitrary tree is in `Tree_p`.
  §4.5 gives a syntactic discipline that approximates membership
  conservatively.

  Σ is now a fixed constant (the two kernel ops, §5), so there is a
  single `Tree_p` — we write `Tree_p` for `Tree_p(Σ)` throughout, with
  no alternative environment to range over. The §11 type system is
  defined relative to it.
]

== The walker restricted to `Tree_p(Σ)`

Once `Tree_p(Σ)` is in hand, the walker restricts to a closed binary
operation on it:

$ w_Sigma : "Tree"_p (Sigma) times "Tree"_p (Sigma) -> "CheckerResult"("Tree"_p (Sigma)) $

That is the type the kernel actually relies on — every handler in §5,
in environment Σ, consumes and produces values in `Tree_p(Σ)`.

== Soundness rules for users

To keep user-written trees in `Tree_p(Σ)`, follow five rules. These
form a *decidable static discipline* that conservatively approximates
membership in the greatest fixed point above:

+ *Don't triage on hypothesis-typed values.* If `x` might be a
  hypothesis (e.g., bound by `bind_hyp`), don't write
  `triage l s f x`. Use kernel-mediated checks (`is_neutral`,
  `tree_eq` against a known closed value, `has_sig` against a
  registered signature) instead.

+ *Don't construct forks rooted at a `forge(Σ)` sig.* The stem rule
  rejects this — you can't fabricate a trusted token (`seal(Σ)` =
  `hyp_reduce`). Mint tokens via `bind_hyp` / `hyp_reduce`.
  (Invocations of `bind_hyp` itself are *not* forge-protected and may be
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

+ *Handler bodies run raw; recognizer bodies run under the walker with
  reader access.* Two different privilege levels, easy to conflate.
  *Handler* bodies (`respond` functions and codomain functions invoked
  *inside* `hyp_reduce`, and the Σ-op handlers themselves) execute "raw"
  — outside the walker — so they may construct seal-rooted trees and use
  raw projection; their authors are part of the trusted base.
  *Recognizer* bodies, by contrast, run *under the walker* (their sig is
  not in Σ, so `param_apply` reaches them through the walker arm, §6.3.1)
  — they do *not* get a raw region. What lets them inspect a
  hypothesis argument is the §4.2 *reader layer*: `ROOT_SIG` /
  `STORED_TYPE` (and the `make_recognizer` H-rule built on them) give the
  public descriptor concretely, while any *other* reflective move (raw
  `pair_snd`, `triage` on the neutral) is still rejected. So a buggy
  recognizer cannot leak by accident — the walker stays live around it;
  only the blessed reads pass.

(The prior spec had a sixth rule, "stricter contexts use smaller Σ,"
for running outside the host's effect surface. It is gone: Σ is fixed
and host effects are no longer dispatcher entries (§15), so there is no
narrower environment to drop into — effectful code is simply
interpreted by a different *handler*, in-language, with no change to Σ
or the walker.)

`Tree_p(Σ)` is the largest carrier in `Kl(CheckerResult)` on which the
walker — the Kleisli lift of the substrate's apply operation —
restricts to a closed binary operation. Composition in
`Kl(CheckerResult)` is the standard `g ∘_K f = μ ∘ T(g) ∘ f` of
§3.5; the walker is the operation those Kleisli arrows compose with
when reducing `apply` chains.

= The dispatcher <sec:dispatcher>

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

The kernel surface has two distinct roles. The *dispatch set* Σ is the
fixed list of the two kernel-operation handler trees whose signatures the
dispatcher is trusted to invoke; the *dispatcher* `param_apply` is the
interpreter — it routes an incoming application either to the matching
kernel handler in Σ or to the parametric walker.

```
Σ : List Tree   -- the fixed dispatch set: [hyp_reduce, bind_hyp]
param_apply : Tree → Tree → CheckerResult Tree    -- the dispatcher
```

Each handler is a *curried function* the dispatcher invokes as
`handler meta arg`: its structured meta record, then an argument from
`Tree_p`, producing a result in the `CheckerResult` monad. (Earlier
drafts threaded the dispatch environment Σ as a leading argument so a
handler's sub-evaluation stayed in a chosen environment; with Σ fixed to
the two kernel ops, there is nothing to vary and nothing to thread.) The
meta record is what gets baked into a wait-form (`wait handler meta`, see
§5.4); applying the wait-form to an argument triggers dispatch via
`param_apply`.

The two kernel-shipped Σ-operations are:

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

    [`param_apply` (dispatcher)],
    [`Tree_p → Tree_p → CheckerResult(Tree_p)` — substrate-apply over
      the fixed kernel Σ, with a privilege check],
  ),
  caption: [The kernel surface: two Σ-operations plus the dispatcher.],
)

#note[
  *Bootstrap.* The records above are typed once §11's library types
  are in scope. At the bootstrap layer they are physically nested
  pairs: `bind_hyp`'s meta is `pair domain body`, etc. Multi-arg
  operations collect their fields across partial-app steps using an
  internal arity counter in the wait-form payload.
]

== The fixed dispatch set

The dispatch set Σ is a *fixed* list of the two kernel-operation
handler trees. Each handler is a closed disp tree implementing a Kleisli
arrow of the appropriate arity; its signature is derived from the
wait-encoding as `checker_sig h := pair_fst (wait h t)` (the same library
helper used throughout §5.4 and §12). The dispatcher's privilege check
asks "is `pair_fst f` equal to `checker_sig h` for some `h ∈ Σ`?", which
reduces to at most two O(1) comparisons (hash-cons identity, §2.2).

The dispatch set `Σ` holds the two handler trees. Each is a plain
`fix`-form (the earlier mutually-recursive `kernel` record and the
`(ks, raw, query)` self-proxy are retired — nothing in the two-op kernel
needed sibling references): `hyp_reduce` recurses through its own
fixpoint `self`, so a `hyp_reduce`-rooted neutral routes to, and is
recognized against, the one canonical fixpoint tree. `bind_hyp` is
realized *inside* the walker (§7.2) — its op-tag is intercepted and the
body runs under the walker's own `self` — so Σ's `bind_hyp` entry is the
bare op-tag whose sig the walker routes on:

```disp
hyp_reduce := fix ({self, meta, frame} -> /* §7.1 */)
bind_hyp   := {A} -> wait bind_hyp_marker A      // op-tag; the walker intercepts (§7.2)
let Σ : List Tree := [ hyp_reduce, bind_hyp ]    // the whole dispatch set
let param_apply := /* the §7.5 fix-form over Σ */
```

There is no host environment and no mutable registry. Real-world effects
are *not* in Σ — they are library `Eff` values performed by the driver
(§15), so Σ never grows beyond the two kernel ops. (Earlier drafts
concatenated a caller-varied `host_provided` into a `default_dispatch`
and let callers substitute narrower environments; that openness existed
only to vary host availability, which is now a driver concern, so Σ is a
single fixed constant.) Each handler is a Kleisli arrow implementing its
operation's semantics (full definitions in §7).

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
privilege check against the fixed Σ:

```
param_apply f x:
  if ∃! h ∈ Σ. tree_eq (pair_fst f) (checker_sig h)  → h ⟨wait_meta f⟩ x
                                                          // registered handler (not f's embedded one),
                                                          // given recovered meta + arg; its
                                                          // CheckerResult is the result (no extra Ok)
  else                                                 → walker step (§4)
```

*The dispatcher routes to the registered handler.* When `pair_fst f`
matches the sig of the (unique) member `h ∈ Σ`, the dispatcher
recovers the wait-form's meta `m = wait_meta f` (the *meta
accessibility* property below) and evaluates `h m x` for *that
registered handler* — it does *not* trust the handler `f` happens to
embed, and it never raw-reduces `f` itself. For a genuine wait-form
`f = wait h m` the registered handler is the embedded one, so the call is
the intended `h m x`; for a forgery carrying the right sig but a different
embedded handler, the registered `h` runs anyway, so a forged routing
cannot execute attacker-chosen code. This closes the
trust-set-vs-routing-table gap (see `archive/SCOPE_VERIFICATION_INVESTIGATION.md`):
privilege is granted to a *sig*, and the sig names a *registered
handler*, not whatever tree presents that sig.

So Σ is a genuine routing table, not merely a trust set. Soundness
needs only that the two *registered* handlers have distinct sigs
(`checker_sig` injective on Σ — trivially true for the fixed pair), not
the far stronger "no user-reachable tree shares a trusted sig." See §7's
`param_apply` entry for the in-language reference.

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
  (§5.4, §7.5), rather than trusting the handler `f` embeds. A directly
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
- `checked A f` wraps a function with its input (domain) type `A`, contract-checking each argument against `A` before applying `f` (§8, §12.16).
- Library types (`Pi`, `Sigma`, `Bool`, etc.) are derived terms — wait-forms over library recognizers. Inductive types supply a `respond` field whose case-frame action mints stuck eliminations through `hyp_reduce`; the library `elim` (§12) handles the concrete/neutral gate.

The library is "freely generated" from Σ in the algebraic-theory sense,
modulo composition equations in `Kl(T)`.

== What the dispatcher is — and what it is *not*

The dispatcher routes exactly two operations — the kernel's
*type-system* operations, which change the checking state rather than
the world:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Handler*], [*Effect on the type-checking state*],
    [`bind_hyp`], [Mint a fresh seal for an unknown of given type.],
    [`hyp_reduce`], [Push a frame onto a neutral; consult the stored type's `respond`.],
  ),
  caption: [The two kernel handlers as type-system operations.],
)

*The dispatcher is not the effect system.* Real-world effects — IO,
syscalls, time-of-day — are *not* dispatcher entries and are *not* in Σ.
They are values in the library free monad `Eff R X` (§15), interpreted by
library handlers, and performed only by an outermost *driver* at the
program boundary. This is a deliberate change from the prior spec, which
unified "effects" with "Σ entries" and routed host IO through the same
dispatcher; the substrate's purity (which hash-consing requires) forces
effects to be values, so they cannot be side-effecting dispatch targets.

The genuine algebraic-effects story — operations, handlers, multi-shot
continuations, effect rows, and the comparison to Plotkin-Pretnar /
Koka / Eff — therefore lives in §15, at the *library* layer, not here. The
kernel dispatcher is just the privilege gate that lets `hyp_reduce` and
`bind_hyp` mint seal-rooted trees the walker would otherwise forbid; it
inherits the algebraic-effects *vocabulary* (monad, Kleisli) but its only
"effects" are the two type-system operations above.

= Stuck forms and neutrals <sec:stuck-forms>

Before defining each kernel primitive's operational semantics, we
introduce the *stuck forms* the kernel exists to construct and
manipulate. The Σ-operations of §5 each play one role in this story —
they're the only constructors of stuck forms, the only consumers of
them, and the only sources of the H-rule short-circuit they need to
travel safely through library code.

== What are stuck forms?

A *stuck form* is a `hyp_reduce`-rooted tree of shape
`wait hyp_reduce (make_neutral_meta T payload)` representing
"a computation whose value is unknown until later." Two operational
roles use the same constructor:

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Role*], [*Origin*],
    [make_hypothesis],
    [`bind_hyp` (payload = `(domain, body)`, so a
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
stem-forge rule; only the kernel mints them via `bind_hyp`. They are
*kernel-privileged* constructors (trusted base), not walker-buildable
user code. Tests that need a bare hypothesis to probe a recognizer or
`respond` (e.g. `bool_recognizer bool_meta (make_hyp_form Bool)`, §12)
use these privileged minters and call the recognizer *directly* — they
do not go through `typecheck`, whose `param_lift` would reject any
public neutral (§8.3). `make_hyp_form` is the test-only spelling of
`make_hyp`.

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
frame appended) or resolves to a value (`Reduce v`). Both arms hand
back the result *bare* (§7.5). Per-type behavior:

- *Π-typed neutral, frame = argument*: `Extend (B frame)`.
- *Σ/Record/Telescope-typed neutral, frame = `acc name`*: walk to the
  named field (§12.7). An *opaque* field of declared type `T`:
  `Extend T` (with priors fed their own projection-neutrals, so a
  dependent `snd : B fst` lands `Extend (B (self.fst))`). A *derived*
  field `name := recipe`: `Reduce recipe` — transparency is the
  `Reduce` arm.
- *Inductive (Bool/Nat/…) neutral, frame = `(pair motive cases)`*:
  `Extend (motive (reconstruct_self meta))`; the library `elim` (§12)
  drives the concrete-case side.
- *Type-typed neutral, frame = candidate value*: `Return (and
  (is_neutral frame) (tree_eq (neutral_type frame) self))` — the
  predicate-side H-rule (§12.18), `Return` being the `Reduce (Ok ·)`
  verdict alias.
- *Non-applicable type (e.g. raw `I`)*: `respond = inert_respond`
  (§12.3), which returns `Extend InvalidType` for every frame; the
  neutral becomes an `InvalidType`-typed dead-state stuck form.

*Library elimination* (`elim`, §12) gates on `is_neutral target`:
the concrete branch runs the dispatcher; the neutral branch routes
the case-frame through `hyp_reduce` as above.

*Triage on a stuck form*: walker-rejected (`TriageReflect`, §4.2).
Library `safe_*` helpers go through `elim` to give well-defined
stuck-Bool / stuck-Tree results.

*Hash-cons equality with stuck forms*: works (`tree_eq` is just
pointer comparison). Two stuck forms constructed via the same
operation with the same metadata hash-cons to the same tree id, so
`tree_eq` returns true for them. Different operations or different
metadata → different tree ids → false.

== The H-rule and stuck forms

When a type-recognizer is applied to a hypothesis — or, more generally,
to any handler-minted stuck form whose stored type is recoverable —
naive recognition fails: the stuck form isn't structurally a canonical
inhabitant of the type. Without intervention, recognizers reject
their own hypothesis values, polymorphic Pi-checks cascade-fail, and
effect-using functions with intermediate stuck values fail too.

The *H-rule* solves this. When applying `T v` and `v` is a handler-
minted stuck form whose stored type equals `T`, return `Ok true`
directly (without running T's structural check).

The H-rule has *two operational sides* corresponding to two distinct
application paths through `param_apply`. They sound similar but route
through entirely different machinery.

=== Recognizer-side H-rule

*Scenario.* `param_apply T v` where `T` is a *library recognizer* —
a `make_recognizer`-wrapped wait-form, e.g. `Bool`, `Nat`, `Pi A B`.

*Path.* The call routes through `param_apply`'s walker arm; the
wait-form reduces to `recognizer_wrap_fn body meta v`. The wrapper runs
*under the walker* — it gets no raw region — so its inspection of `v`
goes through the §4.2 reader layer, not raw triage.

*Mechanism.* The wrapper checks `is_neutral v` and reads
`neutral_type v` — both reader-based (`ROOT_SIG` / `STORED_TYPE`,
§4.2/§12.8), so they return a *concrete* descriptor even when `v` is a
seal. If `v` is stuck it short-circuits to
`Ok (tree_eq self_type (neutral_type v))` — a concrete `Ok true` or
`Ok false`, by hash-cons identity. If false, the per-type recognizer body
runs on the concrete `v`. (The reads are concrete here — where the
old `elim`-routed `safe_*` would have returned a symbolic `make_hyp` —
which is the bug this resolves.)

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
`respond` field. For `hyp_T : Type` specifically, `Type`'s
`respond` is `type_predicate_h_rule`: it returns
`Return (tree_eq (neutral_type v) hyp_T)` iff `v` is a
kernel-minted stuck form, else `Return false`. Again a concrete Bool
verdict.

*Scope.* `Type`-specific. Only `Type`-typed hypotheses are
operationally predicates — Pi-hyps extend spines via `hyp_reduce`'s
default behavior, Sigma-hyps project, non-applicable hyps don't
apply at all. So only `Type` needs a predicate-side `respond`
implementing the H-rule.

*Source.* §12.18, `type_predicate_h_rule`.

=== Why both sides exist

The recognizer-side handles the common case: any library type
recognizer can short-circuit on its own hypotheses uniformly. The
predicate-side handles the polymorphic-quantification case: when a
function is polymorphic over `Type`, Pi-recognition mints a
`Type`-hyp and applies the body to it; the body's `A`-typed values
must be recognized as inhabitants of that same hyp. Without the
predicate-side H-rule, polymorphic `Pi Type ({A} -> Pi A ({_} -> A))`
fails because applying `hyp_A` to candidate values produces non-`Ok
true` results.

Without either side, polymorphism breaks. With both, polymorphic
Pi-types work: `Pi Type ({A} -> Pi A ({_} -> A))` (the polymorphic
identity) inhabits successfully because A-hypotheses applied to it
produce A-hypotheses, which the codomain's recognizer accepts via
H-rule.

== When stuck forms cause cascading failure

A recognizer body that doesn't short-circuit on hypothesis arguments
(via H-rule) propagates stuck-ness through its operations:

```
match (safe_is_fork hyp) { true => ...; false => ... }
  -- safe_is_fork hyp → make_hyp form (stuck Bool)
  -- match on stuck Bool → stuck application
  -- subsequent operations → bigger stuck term
  -- recognizer's overall result → Ok stuck_term (not Ok true)
```

The test framework asserts `Ok true`. A stuck CheckerResult doesn't
equal `Ok true`. The test fails.

This is why `make_recognizer`'s H-rule is mandatory for any
recognizer that might be applied to a hypothesis — which is
*every* recognizer in practice, since `bind_hyp` can create
hypotheses of any type.

== Stuck forms in tests

A test `test typecheck T v = true` requires the computation to reduce
to literal `true`. Stuck forms (which represent unresolved computation)
don't reduce to true — they're not true, they're symbolic terms
representing "what true would be once we know the unknowns."

For tests over hypothesis-laden computations (like strict validation
of Pi-typed recognizers via Pi's body-check), the *H-rule short-
circuits* before stuck forms can propagate. With H-rule, the body
returns a concrete `Ok true` or `Ok false` via the tree_eq check. Without
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

Operational semantics for the two Σ-operations of §5 plus the
dispatcher `param_apply`. The kernel surface is small by
design: only operations requiring privileged construction live here.
Type recognition, case eliminators, typed function application,
effects (§15), and most type-system machinery live in the library (§12).

#note[
  Each handler is invoked from inside a `param_apply` call as
  `handler meta arg` (§5.4). With Σ fixed (the two kernel ops), a
  handler's own sub-evaluation just calls `param_apply` again — there is
  no environment to thread, and type-checking is host-independent
  regardless, since effects are inert library values (§15). `hyp_reduce`
  recurses through its own fixpoint `self` (§5.3, §7.1), so every
  `hyp_reduce`-rooted neutral is the one canonical tree. The earlier
  `(ks, raw, query)` self-proxy and the mutually-recursive `kernel`
  record are gone: direct `param_apply` calls supply what `ks.param_apply`
  did, and the fixpoint `self` supplies what the `kernel.…` names did.
]

== `hyp_reduce`

*Signature.* `hyp_reduce : NeutralMeta → Frame → Tree_p`.

*Role.* The universal "push a frame onto a neutral" engine. When a
neutral is applied to a frame, the dispatcher routes here; the handler
consults the stored type's `respond` field (§11.2) and either extends
the spine (`Extend`) or resolves to a value (`Reduce`).

*Disp source:*

```disp
hyp_reduce := fix ({self, meta, frame} -> {     // hyp_reduce sub-evaluates nothing
  let tmeta := type_meta meta.stored_type   // `respond` is constitutive — every type has one (§11.2)
  let me    := wait self meta               // the reconstructed self-neutral (= reconstruct_self meta)
  match (tmeta.respond tmeta.recognizer_params me frame) {
    Extend new_type => wait self (extend_neutral_meta me new_type frame)  // stuck: extend the spine (bare)
    Reduce v        => v                                                  // computes: the value (bare)
  }
})
```

The `match` on the two-tag `Action` is the whole engine. `Extend T'` keeps the
elimination stuck at a new stored type; `Reduce v` resolves it to a value. A
frame the type rejects comes back as `Extend InvalidType` (§12.3), handled by
the ordinary `Extend` branch — so there is no third case and no hardcoded
`invalid`.

A spine extension roots at `self`: by the fixpoint this is the one canonical
`hyp_reduce` tree, so the result both routes under dispatch and answers
`is_neutral`. It embeds the *predecessor neutral* `me`, not its bare metadata —
load-bearing for the escape scan, since §8.1's `support` inserts metadata only
at neutral roots, so a hypothesis hidden in a spine is reached only when the
predecessor it sits under is itself a recognizable neutral. (Rooting at the
single fixpoint tree also moots the §14 hash-cons question.)

Both arms return *bare* — no `Ok` wrap (the §7.5 wrapping invariant, "Option
A"). `Reduce v` hands back `v` directly; a verdict arrives pre-wrapped as
`Reduce (Ok b)` via the `Return` alias (§3, §12.18). So an inhabited check
surfaces `Ok b`, a rejection surfaces a bare `InvalidType`-typed neutral,
`Ok <neutral>` never arises, and `param_apply` / `verify` treat a non-`Ok`
result as a hard failure rather than a quietly-wrapped success.

*Soundness obligation.* `respond` runs raw (outside the walker). Each
type former's `Reduce v` channel must be fed only by public-derived
data — the local DCC `[BindM]` discipline. `Extend` (including the
rejection case `Extend InvalidType`) is unconditionally safe: it only
ever names a type, never surfaces payload.

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
let bind_hyp := {domain, body} -> {
  let h := wait hyp_reduce (make_neutral_meta domain (t domain body))
  let h_use := match (is_pi domain) {
    true => checked (pi_dom domain) h   // wrap with the domain's *input* type, so applying
    false => h                           // h_use checks its arg against `pi_dom domain` (§8, §12)
  }
  // `param_apply` runs the body. Type-checking never performs an effect
  // (effects are inert library values, §15), so a body-check is inherently
  // host-independent — there is no environment to thread. Per the §7.5
  // wrapping invariant, param_apply returns the body's CheckerResult
  // un-nested, so `bind` exposes the body's own payload (a `Pub h R` value);
  // we escape-check it and re-wrap with `Ok` to hand back a `CheckerResult R`.
  bind (param_apply body h_use) ({result} -> (
    match (occurs h result) {
      true => Err (Escape { hyp = h, body_result = result, span })
      false => Ok result
    }
  ))
}
```

#note[
  *Implementation (landed): `bind_hyp` is merged into the walker.* This source
  runs the body via `param_apply` — i.e. UNDER the walker. The kernel realizes
  that by making `bind_hyp` a bare op-tag (`wait bind_hyp_marker A`) that
  `param_walker` intercepts (`is_bind_hyp`, alongside the `hyp_reduce` routing);
  the interception (`w_bind_hyp`) mints the hyp, walks `body h` via the walker's
  own `self`, and escape-checks. Running the body through `self` keeps
  `bind_hyp`/`param_apply` mutually recursive without a forward-reference cycle and
  is *re-entrant*: the walker's sanctioned reads of a neutral (`is_neutral`,
  `neutral_type` — H-rule) pass, while raw reflection in the body
  (`triage`/`pair_snd`/`type_meta` on the hyp) is rejected exactly as at top level.
  This closes the metadata-extraction leak — `occurs` is no longer the sole guard
  inside a raw body (there is no raw body). Recognizer bodies are written with
  *raw applications* (`(B hyp) (v hyp)` in Pi, `(P hyp) v` in Intersection); the
  enclosing walk polices each user sub-term, so the explicit per-sub-term
  `param_apply` calls are unnecessary. An under-walker site applies the tag
  directly; a RAW context (a `respond`, a test) routes it in with
  `param_apply (bind_hyp A) body`. `bind_hyp_marker` fails closed (`Err`): a tag
  reduced outside the walker is a bug, so a type whose *recognizer* mints a hyp
  (Pi, Intersection) applied RAW (`T v`, bypassing `param_apply`) fails fast rather
  than silently checking with the guards off. (Sigma's recognizer checks a concrete
  pair — no hyp — so it does not fail fast raw; its family is policed in its respond.)

  *Verification goes through the walker.* A module's typed exports are checked by
  `verify mod := param_apply mod.typ mod.record` — NOT raw `mod.typ mod.record`
  juxtaposition, which bypasses the walker entirely (a non-parametric export would
  slip through). The elaborator auto-runs `verify` on each loaded module's typed
  exports at elaboration time, so a value that does not inhabit its declared type
  is a compile error.
]

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
is needed.

== `postulate` is gone — effects are a library construction

Earlier drafts shipped a third Σ-operation, `postulate`, as the
kernel-mediated constructor for host-rooted wait-forms — the bridge by
which user code reached a host effect handler, sanitizing the payload
(no neutrals) on the way. That entire mechanism is *removed*. Effects are
now a pure library free monad interpreted by an outermost driver (§15):

- An effect operation is an ordinary `Eff R X` *value* (a free-monad `Op`
  node, §15.2), built by library `declare_op` — not a privileged
  construction, so no kernel primitive mints it.
- The host never appears in the dispatch environment Σ. There is no
  `host_provided`, no funnel sig (§4.2), no host-rooted wait-form.
- The no-neutral sanitizer that `postulate` performed relocates to the
  *driver*, as the `is_closed` check before each host call (§15.6) —
  the only sound place for it, since op arguments are legitimately
  symbolic during type-checking.

So the kernel ships exactly *two* Σ-operations, `hyp_reduce` and
`bind_hyp`, both privileged because both *mint a seal-rooted tree*
(§4.2) that the walker forbids library code from constructing. Nothing
else needs privileged construction.

== `param_apply`

*Signature.* `param_apply : Tree_p -> Tree_p -> CheckerResult(Tree_p)`.

*Role.* The dispatcher. Each call decides between trusted raw execution
(for an invocation of a kernel Σ-operation) and unprivileged walker
reduction (everything else). On the raw arm it *routes to the registered
handler* for the matched sig — the kernel handler the sig names, not the
handler the wait-form happens to embed (§5.4) — so privilege is granted to
a sig and the sig names a fixed kernel handler.

*Σ is now a fixed kernel constant.* With effects moved to the library
(§15), nothing host-specific populates the dispatch set, so the earlier
`safe_apply Σ` parameterization collapses back to the plain two-argument
`param_apply`. The dispatch set Σ is the *fixed* list of the two
Σ-operations; there is no `host_provided`, no `default_dispatch`, no
caller-varied environment. (`Σ` remains the name of this set throughout —
the walker's `seal(Σ)` / `forge(Σ)` / `Tree_p(Σ)` of §4 are now just
`seal`, `forge`, `Tree_p` over this one fixed Σ.)

*Gloss.* "Look at `f x`. If `f`'s sig is a kernel-op sig, run that kernel
handler raw (it's privileged). Otherwise, run the walker — apply substrate
reduction rules, but reject any rule that would forge a pinned sig or
triage on a neutral."

```disp
// The fixed kernel dispatch set: the two Σ-operations. No host entries.
let Σ : List Tree := [ hyp_reduce, bind_hyp ]

// Routing lookup. At most two O(1) hash-cons comparisons. Returns the
// kernel handler for `sig`, or None if `sig` is not a kernel-op sig.
// `wait_meta` is the meta-recovery of §5.4: for a genuine `wait k m` → `m`.
lookup_handler := {sig} -> list_find ({h} -> tree_eq sig (checker_sig h)) Σ
```

*The dispatcher.*

```disp
param_apply := fix ({self, f, x} ->
  match (and (is_wait_form f) (is_some (lookup_handler (pair_fst f)))) {
    true => (
      let h := unwrap (lookup_handler (pair_fst f))
      h (wait_meta f) x)                       // registered kernel handler: recovered meta, arg —
                                               // already returns a CheckerResult, passed through un-nested (§7.5)
    false => walker_step self f x                 // walker (§4)
  })
```

The two arms have different semantics:

- *Routed arm.* The dispatcher runs the *registered* kernel handler `h`
  (its sig is one of the two in Σ), supplying the wait-form's recovered
  meta and the argument: `h (wait_meta f) x`, in raw (non-walker)
  reduction. The dispatcher never raw-reduces `f` itself: for a genuine
  `f = wait h m` the registered handler equals the embedded one, and for a
  forgery presenting the sig with a different embedded handler the
  *registered* `h` runs regardless (§5.4). The handler's body, running
  raw, can do operations the walker would reject — minting neutrals
  (`bind_hyp`), extending spines (`hyp_reduce`). Because the routed handler
  is the *registered* one, forging the *invocation* of `bind_hyp` is
  harmless: it can only re-enter the genuine, self-defending handler.

- *Walker arm.* `walker_step` is the parametricity-enforcing reduction of
  §4. Its internal sub-applies re-enter `self` (the same `param_apply`)
  rather than calling substrate apply directly, so dispatch happens at
  every layer of nested reduction. Any sub-tree that is a kernel-op
  wait-form gets routed back to the raw arm.

#note[
  *The wrapping invariant.* `param_apply f x` evaluates the application
  `f x` to normal form and returns that normal form *directly*; the only
  `CheckerError` it introduces itself is `Err Parametricity`, when a
  reduction step trips a walker rejection. It never adds an `Ok` of its
  own. Two consequences:
  - A *pure* reduction returns its bare result tree —
    `param_apply (add 2 3) → 5`, not `Ok 5` (so arithmetic tests like
    `test (add 2 3) = 5` hold).
  - A reduction whose head is a `CheckerResult`-producer — a library
    recognizer, a kernel handler, or `checked_apply` — returns that
    producer's `CheckerResult` as the normal form: `param_apply Bool true →
    Ok true`, *not* `Ok (Ok true)`.

  So `param_apply` results are *never* nested (`Ok (Ok _)` / `Ok (Err _)`
  do not arise): every dispatch target already returns a `CheckerResult`,
  and the dispatcher returns it verbatim (the routed arm is `h (wait_meta
  f) x`, with no wrapping `Ok`). This is why the two handlers share one
  return convention — `hyp_reduce`'s spine extensions are `Ok`-wrapped
  (§7.1) just as `bind_hyp` wraps its verdicts and errors — and why
  `bind_hyp` re-wraps its escape-checked result as `Ok result` (§7.2)
  rather than relying on a second `Ok` from the dispatcher.
]

*Native fast-path (optional, currently off).* `param_apply` runs
*in-language* — the dispatcher of this section is what executes. A host
fast-path that intercepts `apply(param_apply, …)` by compiled tree id and
runs a TypeScript reimplementation is *permitted* as an optimization, but
is not wired in the current runtime (the only live native fast-path is
`tree_eq`, §2.2). Re-enabling a native dispatcher requires a restored
bit-identical equivalence test against this in-language reference, which
remains the spec.

= Boundary operations and checked values <sec:boundary>

The boundary between untrusted user trees and the Kleisli world of
`CheckerResult` rests on one scan (`occurs`) and one wrap
(`checked`). `param_lift` and `typecheck` build on these to turn the
boundary into the user-facing query API; `strip_validated` (§10) pairs
the verdict with erasure.

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
    true => set_insert (type_meta v) (self (type_meta v))
    false => triage set_empty ({c} -> self c)
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

    [`param_lift` / the effect driver (§15.6)],
    [`is_closed v` — no hypothesis crosses to the user (`typecheck`) or to a host call (the driver, before `perform_host`)],
  ),
  caption: [Two boundary checks, one set.],
)

The walk does *not* stop at neutrals. That is the whole correction: a
stuck elimination is itself a neutral, so a scan that halted at neutral
roots would stop *before* reaching the hypothesis embedded in its
spine — precisely the values that carry a hypothesis out of scope. This
is verified in the implementation: `bind_hyp (Pi Nat ({_} -> Bool))
({h} -> h 0)` returns `Err`, because `h 0` is a stuck elimination
whose spine holds `h` (`lib/tests/bind_hyp.test.disp`); the prior
seal-stopping scan returned the leaked neutral instead.

`support v` is a pure function of `v`'s structure, so the runtime
memoizes it per tree id (the same hash-cons-keyed `apply` memo that
makes `tree_eq` O(1)); equal subtrees share their support and the
boundary checks read the cache rather than re-walking. Membership on
metadata — not on the neutral itself — is what lets the embedded
predecessor (`wait self meta`, built inside the handler) match the
hypothesis it stands for (`wait hyp_reduce meta`, minted by
`bind_hyp`) without depending on those two wait-forms being identical.
(They are in fact identical here — `self ≡ hyp_reduce`, the one
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
    true => Ok v
    false => Err (Malformed { handler = "param_lift", meta = v, span })
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

`Ok true` means inhabitant, `Ok false` means not, `Err _` means
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
  directly (`bool_recognizer bool_meta (make_hyp_form Bool) = Ok true`,
  §12) likewise call the recognizer through `param_apply`, not
  `typecheck`. A caller that genuinely needs to ask "does this
  neutral-bearing `v` inhabit `T`?" uses `param_apply T v` directly,
  accepting that it has stepped inside the trusted recognition layer
  rather than the sanitizing boundary.
]

*No "pure variant" is needed.* Earlier drafts shipped a `typecheck_pure`
that ran under `safe_apply kernel_handlers` so host primitives could not
influence the verdict. With effects moved to the library (§15), host
primitives are inert `Eff` values that are never *performed* during
checking — `param_apply` carries no host environment and `typecheck` is
*inherently* host-independent. The foundational tests that used
`typecheck_pure` now just use `typecheck`; there is one checking mode.

== Example traces

*Trace 1: `typecheck Bool true`.*
+ `param_lift true` → `Ok true` (true contains no neutrals).
+ `param_apply Bool true` reduces `wait bool_recognizer bool_meta true` → `bool_recognizer bool_meta true` → `true` is a canonical Bool shape → returns `Ok true`.
+ `typecheck` returns `Ok true` — the verdict.

*Trace 2: `typecheck Bool (is_zero true)`.*

Here `is_zero` is a `checked Nat is_zero_raw` value (domain `Nat`).

+ Compile-time `applyTree (is_zero, true)`:
  - Dispatcher routes `apply(checked-wait-form, true)` to the `checked` handler.
  - Handler checks `param_apply Nat true`. Nat's recognizer rejects true structurally → `Ok false`.
  - `checked` is a contract boundary: `Nat` was promised, `Ok false` means broken promise → raises `Err (TypeMismatch { expected = Nat, actual = true, span })`.
  - So `is_zero true` reduces to that `Err`.

+ Now `typecheck Bool (Err ...)`:
  - `param_lift` is called on an `Err` value. The wrapping monad propagates: `bind (Err e) k = Err e`.
  - Returns the original `Err TypeMismatch`, payload pointing at the inner mismatch.

The error originates inside the reduction (at the `checked` handler's
domain check, a contract boundary) and propagates outward unchanged.
The user gets the inner failure's full payload — the expected type,
the actual value, and the span — not an opaque outer "typecheck
failed."

Contrast: a *query* like `typecheck Bool zero` returns `Ok false`, not
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

== The library constructor

There is one constructor. `checked` pairs a function with its declared
*input* (domain) type, building the wait-form of §5.4. Its full
definition is in §12.16.

```disp
checked A f  ≡  wait checked_apply { dom := A, fn := f }
```

Applying it checks the argument against `A`, then runs `f` (§8.10).
There is no separate `typed_lambda` — a lambda is wrapped directly with
its domain — and no certificate wrapper. Whether `f` actually maps `A`
into a particular codomain is *not* asserted at construction; that is
decided when the value is recognized against a `Pi A B` (§12.9).
Validation is therefore a *verdict*, not a wrapped certificate:
`strip_validated` (§10) runs `typecheck` and, on `Ok true`, erases the
now-redundant `checked` input-guards.

== Soundness: forged function contracts are caught by recognition

`checked A f` does not assert at construction that `f` maps `A` into
any particular codomain — the constructor only records the domain and
defers the rest. A *false* contract — an `f` whose body does not land
in the claimed codomain — is caught the moment the value is recognized
against a `Pi A B`.

Example: user writes `let bogus := checked Nat ({n} -> true)` and asserts
`bogus : Pi Nat ({_} -> Nat)` (a lie — the body returns a `Bool`).
- `typecheck (Pi Nat ({_} -> Nat)) bogus` runs Pi's recognizer (§12.9):
  - `bogus` is a `checked` wait-form whose stored domain is `Nat` —
    the structural checks pass.
  - The body-check binds a fresh `Nat`-hypothesis, applies `bogus` to
    it (yielding `true`), and checks `true` against the codomain `Nat`.
    `Nat`'s recognizer rejects `true` → `Ok false`.
- So the verdict is `Ok false`: the false contract is not an inhabitant.

The user gets `Ok false` (verdict-as-data), not a thrown error — the body
simply does not satisfy the codomain.

If `bogus` instead reaches a *contract boundary* — a `checked`-wrapped
function applied to it where its domain was promised — that function's
input-check raises `Err (TypeMismatch { … })`, because at that boundary
the type was promised rather than queried (§3, verdict-vs-error).

== Composition

Applying a `checked` value to an argument fires `checked_apply`:

```
(checked A f) x
→ checked_apply { dom := A, fn := f } x   // wait-form reduction: checked_apply's
                                          // sig is a library sig, not in Σ, so no
                                          // special routing — wait reduces directly
→ bind (param_apply A x) ({verdict} -> match verdict {
    true => match (is_neutral f) {
            true => param_apply f x        // f is a bind_hyp-wrapped neutral: its
                                         //   application is already a CheckerResult
                                         //   (§7.5) — pass through, do not re-wrap
            false => Ok (param_apply f x)   // f is a raw function: its application is a
          }                              //   bare reduct, so wrap with Ok
    false => Err (TypeMismatch { expected = A, actual = x, span })
  })
→ on success:         Ok (f x)
→ on domain mismatch: Err (TypeMismatch { expected = A, actual = x, span })
```

Chained applications work the same way. For curried functions returning
functions, the result of one application may be another `checked` value,
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
  with `checked A` (§8) — the function carries its *domain* as a
  runtime input-contract.

  *Record-field lambdas wrap the same way.* The surrounding type that
  fixes a binder's `Pi` need not come from an outer `Pi` annotation — it
  can come from a *record field's* declared type. When a record literal
  `{ f := e, … }` is elaborated against a known record type
  `Record [(f, Tf), …]` (e.g. an effect-handler value at its interface
  type, §12/§15), each field's expected type `Tf` flows down to its
  initializer: if `Tf` is a `Pi` and `e` is a lambda, `e` is wrapped as
  `checked A e` (with `A` = `Tf`'s domain), exactly as a top-level
  `Pi`-typed binder would be.
  This is *required*, not cosmetic — `Pi`'s recognizer (§12) accepts only
  `checked` wait-forms, so without the wrap a bare field lambda like
  `mockConsole.print := {_} -> io_pure unit` would fail
  `typecheck Console mockConsole`. The rule recurses through nested
  records, so a field that is itself a record propagates field types one
  level further. (A field whose initializer is already a `checked` value
  needs no wrap.)
+ Bracket-abstract the binder (standard combinator translation).
+ *Test emission.* At each `let name : T = body`, emit two operations:

```
let name := body_tree
test typecheck T name = true
```

The first is the binding (no validation). The second is the test
that the elaborator runs immediately.

Type info flows *only top-down at binders*. There is no bottom-up type
synthesis, no unification, no `infer`/`check` ping-pong.

== The `test` keyword and `=`

`=` is *infix `tree_eq`*. So `expr1 = expr2` is `tree_eq expr1 expr2`,
returning a Bool tree (true or false). It is *not* assignment — disp has
no mutable state.

`test` is an elaborator keyword, not a function. `test expr`
*evaluates* `expr` — the substrate's `apply` reduction, with the
dispatcher `param_apply` standing in for `apply` at
every application step — and asserts the normal form equals `true`.
Evaluation just *is* reduction: there is no separate "un-reduced
expression" object, so `test` takes an expression to reduce, not a
tree handed to a partially-applied dispatcher. (Tree calculus is
Turing-complete, so reduction need not halt in general; tests are
expected on terminating expressions.) If `expr` reduces to anything
else (false, an `Err` value, a stuck form), the elaborator throws,
reporting the failing expression and its actual reduction.

```disp
test (add 2 3) = 5                // arithmetic check
test typecheck Type Bool = true      // type validity check
test param_apply Bool true = Ok true   // recognition test
```

Common test idioms are sugar:

- `test expr` (no `=`) defaults to `test expr = true`. Useful when
  `expr` already returns a Bool.
- `test typecheck T v` is the standard "type-check test."

The `: T` annotation is sugar for this last form. `let X : T = body`
desugars to `let X := body; test typecheck T X`.

== Tests reduce under `param_apply`; effects need a handler

`test expr` reduces `expr` under `param_apply` (the fixed-Σ dispatcher,
§7.5). Because effects are inert library values (§15), a test does *not*
fire host primitives by merely mentioning one: `read_env_var "PATH"` is
an `Eff String` *value*, not a performed read. To observe effectful
behavior in a test, interpret it with a handler — typically a mock — and
assert on the pure result:

```disp
test (handle mock_fs  (file_exists "lib/prelude.disp")) = true
test (length (handle mock_env (read_env_var "PATH")))   > 0
```

Because checking and reduction never perform an effect, there is *no*
separate "pure test" mode: the prior spec's `test_pure` (which ran under
`safe_apply kernel_handlers` to exclude host influence) collapses into
plain `test`. Foundational, conversion, and behavioral tests all use the
one `test` directive; effectful tests differ only in supplying a handler
rather than the real driver.

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
let Bool := wait bool_recognizer bool_meta
test typecheck Type Bool             // Bool is a type
test typecheck StrictType Bool       // Bool passes deep validation too
test bool_recognizer unit true = Ok true // recognizer accepts true
test bool_recognizer unit false = Ok true // recognizer accepts false
test bool_recognizer unit zero = Ok false  // recognizer rejects non-bool
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
+ Wrap the binder: `checked Nat ({x} -> apply(is_zero_compiled, x))`.
+ Bracket-abstract `{x}` over the body.
+ Emit: `let foo := wrapped_tree` plus `test typecheck (Pi Nat ({_} -> Bool)) foo = true`.

The test reduces `typecheck (Pi Nat ({_} -> Bool)) foo` via the
kernel dispatcher. Pi's recognizer fires the outer `checked` handler
against a Nat-hypothesis, runs the body, checks result against `Bool`.
Result `Ok true` → test passes. `Ok false` or `Err _` → elaborator throws
with the failure context.

= Strip and erasure <sec:strip>

== Motivation

Carrying type information at runtime is not free. A `checked` function pays an
input-check on every application (one `param_apply` of the domain against the
argument); a record carries a name header it resolves against; a coproduct
carries named tags. For hot paths and release builds we want to drop everything
the *type* already fixes, once the program has been validated.

This is the *strip* pass — one type-directed erasure with a single rule: *drop
the part of each value's descriptor the type already determines, keep the part
only the value knows, and lower every eliminator to its positional form.* The
result is a tree equivalent to what raw tree-calculus apply would compute,
without type-checking or name-resolution overhead.

== Definition

A wait-wrapped value `wait k payload` splits its descriptor in two:

- *type-determined* — a validated contract (`checked`'s domain `A`) or a
  name schema (record field names, coproduct variant names). The type fixes it,
  so it is redundant at runtime: names resolve to positions, a validated
  contract drops.
- *value-determined* — the payload, plus *which variant* a sum carries. Only the
  value knows it, so it is retained.

Strip drops the first and lowers the eliminator accordingly:

#figure(
  table(
    columns: 4,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*kind*], [*type-determined → stripped*], [*value-determined → kept*], [*eliminator: before → after*],
    [`checked`], [the domain `A`], [the function `fn`], [`cv x` → raw `fn x` (no rewrite)],
    [record], [field names → positions], [field tuple], [`r.a` → `pair_fst (pair_snd^idx payload)`],
    [coproduct], [variant names → positions], [variant *index* + payload], [`match`-by-name → dispatch-by-index],
  ),
  caption: [Strip = drop the type-determined descriptor, lower the eliminator.],
)

The `checked` row is the *degenerate, no-rewrite corner*: its descriptor is
entirely type-determined (a pure contract), so it strips completely and apply of
the inner value is already correct. That corner needs no binder types, so it is
a plain structural walk — recognize `checked_apply` wait-forms by signature,
replace each with its `value`, recurse:

```disp
strip_checked := fix ({self, t} ->
  match (has_sig checked_apply t) {
    true => self ((wait_meta t).fn)         // unwrap (read the `fn` field), recurse into inner
    false => triage
      t                                   // leaf: unchanged
      ({c} -> t (self c))                 // stem: recurse
      ({l, r} -> t (self l) (self r))     // fork: recurse on both children
      t
  })
```

The full pass `strip` extends that corner with the record and coproduct rows.
Disp has no typing derivation (elaboration is wrap-only, §9), so `strip` is a
tree-to-tree function that reads each value's schema from the value *itself* — a
record's name header, a coproduct's tag — together with the type ascriptions
embedded in the `checked` wait-forms it walks through. It drops every
type-determined descriptor, lowers each eliminator, and leaves seal-rooted
wait-forms (neutrals) untouched — effect values are ordinary `Eff`
coproduct data (§15) and are simply recursed through:

```disp
strip := fix ({self, t} ->
  match (is_neutral t) {                   // seal-rooted (a neutral): a genuine
    true => t                                //   unknown — never a contract
    false => match (has_sig checked_apply t) {
      true => self ((wait_meta t).fn)        // checked: drop domain guard, recurse into fn
      false => match (is_record_product t) {  // record VALUE (const-field product, §2.6)
        true => strip_record self (wait_meta t)
        false => triage                       // concrete data / code: structural recurse
          t
          ({c}    -> t (self c))
          ({l, r} -> t (self l) (self r))
          t
      }}})

// A record's field table is `pair names payload`, where `payload` is a Σ-chain
// of `const`-wrapped thunks (§2.6). Drop the name header, unwrap each `const`
// (its inner is `pair_snd`), strip it, and re-emit the bare positional Σ-tuple —
// position is now the only index, so a literal `r.a` (already β-lowered to
// `pair_fst (pair_snd^idx payload)`, §12.4) indexes straight into it.
strip_record := {self, F} ->
  map_chain ({field} -> self (pair_snd field)) (pair_snd F)
```

The `is_record_product` guard separates a record *value* (whose fields are
`const`-wrapped, ignoring the payload, §2.6) from a `match`/`cases` *product*
(whose fields are real handlers): only the former lowers to a positional tuple
here. The latter is the one genuinely *type-directed* step, and the only place
`strip` must consult a type rather than the value:

#note[
  *Coproduct lowering needs the variant order — the residual type-directed step.*
  A coproduct value `inj Vi pay` carries its own tag `Vi` but not the *order* of
  the full variant set, and a `match` requires the value and the branch table to
  agree on an index. That order is fixed by the coproduct's *type*. So at a
  `match` site, `strip` reads the variant list from the scrutinee's ascribed
  `Coproduct [(Vi, Si)]`, rewrites each value's tag `Vi` to
  `index_of [V1..Vn] Vi`, and lowers the `cases` product to a positional branch
  tuple keyed by that same index — consistent because both sides use the one
  variant order. A single-variant sum needs no index (one branch); `Bool`'s raw
  shapes are already the bare index (`△` / `△ △`, §2.7) — no tag to rewrite. Everywhere else — `checked`, record — the value is self-describing and
  `strip` needs no type. This is the no-typing-derivation analogue of
  type-directed erasure (§17): the schema is threaded from the enclosing `checked`
  ascription and the value's own headers, not from a typing tree.
]

*Strip leaves handler-rooted wait-forms intact.* Kernel-rooted stuck forms
(hypotheses, stuck eliminations) and host-effect wait-forms survive strip — only
type-determined descriptors are erased. Strip's role is to elide what the type
fixes, not to remove effects. A stripped program retains all of its effect
machinery and executes identically under `param_apply`, modulo
the elided contract and name-resolution overhead.

*What survives.* For a product, nothing — its content is fixed by its type
(every field present, in order), so it lowers to a bare positional tuple. For a
sum, one residue: the variant *index*, the genuinely dynamic "which one." It is
no longer a name but a minimal positional tag (≈ `log₂` variants of structure;
for `Bool`'s raw shapes not even a data field — the shape itself is the index
(§2.7); a single-variant sum strips even that). That residue is the operational content
of *polarity* (§17): a product is negative and fully static, a sum positive with
one dynamic index.

== Soundness

Strip is *not* unconditionally sound. Applying strip to an arbitrary
tree may remove checks that were necessary for soundness — e.g., if a
`checked A f` whose body lies about its codomain was waiting to be
caught by an outer `typecheck (Pi A B)`, stripping its input-guards
would erase the only thing that would have flagged the lie at runtime.

Strip is sound *exactly when* applied to a tree that has been validated.
The standard usage pattern: pair strip with `typecheck`.

```disp
// Safe usage: validate first (typecheck = Ok true), then strip. There is
// no certificate wrapper — the verdict *is* the certificate, so on
// success we strip `v` directly.
strip_validated := {T, v} ->
  bind (typecheck T v) ({verdict} ->
    match verdict {
      true => Ok (strip v)
      false => Err (TypeMismatch { expected = T, actual = v, span })
    })
```

`typecheck T v = Ok true` is itself the certificate: it attests that every
`checked` input-guard inside `v` was exercised against a fresh
hypothesis and held. Stripping those guards from `v` is therefore sound
— the contract they enforce has already been discharged.

== Connection to proof-carrying code

This is Necula's proof-carrying code pattern (Necula 1997). The
elaborator/host plays the *certifier* role: it validates the program
once by running `typecheck`. The certificate is the *verdict*
`typecheck T v = Ok true` — the fact that validation succeeded, not a
wrapper around `v`. The strip pass plays the *compiler optimization*
role: once validated, runtime needn't re-check.

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

    [Eval / `∈`], [Behavior under application], [Eval morphism of an LCCC (for Pi-like) or `∈` (for predicate-like)],
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
//   Respond := recognizer_params -> self -> frame -> Action
//                                       // response to an elimination frame
//                                       //   applied to a neutral of this type;
//                                       //   `self` is the reconstructed neutral,
//                                       //   `recognizer_params` the former's params.
//                                       //   Typed `RespondShape` (§12.6).
//   Action  := Extend Type | Reduce Tree     // reject = Extend InvalidType (§12.3);
//                                       //   Return v := Reduce (Ok v) is verdict notation
//   Frame   := Tree_p                   // untagged; the stored type
//                                       //   interprets it

MetaShape := Record [
    ("recognizer_params", Tree),   // intended: type-former parameters (per type)
    ("functor",           Tree),   // intended: Functor (Kan transport, §13)
    ("respond",           Tree),   // intended: RespondShape (elimination handler; constitutive, never none)
    ("behavioral_specs",  Tree)    // intended: Optional (List Spec) (runnable specs;
                                   //           Spec is realized as Path, §13)
  ]
  // Field types are loosened to `Tree` so structural membership (fields
  // present, in order) suffices; the *intended* ascriptions in the comments
  // — above all `respond : RespondShape` — are enforced deeply by `StrictType`,
  // not by `MetaShape` itself. `MetaShape` is now a plain `Record` instance
  // (§12.7), no longer a `Refinement` wrapper; §12.6 gives the kernel-internal
  // definition this references rather than restates.
```

A MetaShape value is an ordinary headered record (§2.6), so `meta_get m
"respond"` is the projection `m.respond` — the name resolves against the
value's own header, no field list needed.

`respond` is the universal "respond to an elimination frame" function,
called by `hyp_reduce` as `respond params self frame` (the type-former
parameters, the reconstructed self-neutral, and the frame). The stored
type determines how the frame is interpreted; frames are untagged because
one type accepts one frame kind:

- *Π*: frame is an argument `a`; `Extend (params.cod a)`.
- *Σ / Record / Telescope*: frame is a field accessor `acc name`; the
  telescope `respond` (§12.7) walks to that field — opaque field of type
  `T` ↦ `Extend T` (a dependent `snd : B fst` lands `Extend (B self.fst)`
  by feeding priors their projection-neutrals); derived field
  `name := recipe` ↦ `Reduce recipe`.
- *Inductive*: frame is `(pair motive cases)`; `Extend (motive self)`,
  policed through the walker (`inductive_respond`, §12.2/§12.3).
- *Path*: frame is a dimension `i`; `Extend (P i)`.
- *Type*: frame is a candidate value `v`; `Return (and (is_neutral v)
  (tree_eq (neutral_type v) self))` — the predicate-side H-rule (`Return`
  being the `Reduce (Ok ·)` verdict alias).

An *inert* type carries `respond = inert_respond` (§12.3), which returns
`Extend InvalidType` for every frame (e.g., `I`, `IsOne`, `String`,
`Glue`). `respond` is never `none`: it is a *constitutive half* of a
type, not optional metadata — see the note below. The convention is
extensible in the other fields: existing types whose meta lacks a
*new* field still pass structural checks.

#note[
  *A type is intro + elim + computation.* A type's recognizer says what
  *inhabits* it (its introduction side); its `respond` says how a *neutral*
  of the type *eliminates* (its elimination side). A type missing `respond`
  is only half-defined — you could ask "is `x` a `T`?" but not "what happens
  when a `T` is used." Dependent type theory needs both, so `respond` is
  mandatory, and the three validators (§11.4) are exactly three tiers of "how
  much of intro + elim + computation is present and checked":
  - `Type` (structural): both halves *present and shaped* — recognizer is
    `make_recognizer`-formed, meta fits MetaShape (`respond` included).
  - `StrictType` (deep): both halves *well-typed* — recognizer inhabits
    `RecognizerShape`, `respond` inhabits `RespondShape` (§12.6).
  - `BehavioralType` (behavioral): both halves *coherent* — the type's
    `behavioral_specs` Paths, which encode the per-former computation rules
    (the bullets above, e.g. Π's `neutral_type (n a) ≡ B a`), all run to
    `Ok true` (§11.4, §12.3).

  *Derived, not hand-written.* A user does not author a `respond`: each type
  *former* (Π/Σ/Coproduct/Record/Refinement/inductive) ships one
  library-certified `respond` plus its coherence Paths, proven once; a type
  built from a former *inherits* it (this is why every inductive shares
  `inductive_respond`, §12.3). So a user type structurally cannot carry a
  *wrong* `respond` — it never wrote one. A genuinely *new* former is the only
  site a fresh `respond` appears, and `BehavioralType` gates it.
]

#note[
  *Naming.* The earlier `applicable` field is renamed `respond` to
  reflect that it handles every elimination form, not only function
  application.
]

#note[
  *Two ways to read `respond`.* Operationally it is the per-type *handler
  clause* for one operation, "eliminate": a neutral is a stuck operation, its
  spine is the captured continuation, and `respond` says how that operation
  reacts to a frame — stay stuck at a new type (`Extend`), resolve to a value
  (`Reduce`), or reject by advancing to the dead state (`Extend InvalidType`).
  This is the same shape as an `Eff` handler's per-operation clause
  interpreting an effect (§15) — "react to a frame" and "interpret an
  operation" are one move, which is why the eliminator and the effect handler
  are the same fold. Formally it is the structure map
  of a coalgebra on `Tree_p` (a greatest fixed point, §4): a typed
  transition system whose states are neutrals carrying their stored type,
  whose labels are frames, and whose steps are `Extend` (advance to a new
  typed state) and `Reduce` (halt with a value). `Extend InvalidType` is the
  absorbing dead state — `InvalidType` itself responds `Extend (neutral_type
  self)`, staying at `InvalidType`. The
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
    [`Type`], [Structural], [`v` is a wait-form whose meta fits MetaShape's layout. H-rule on neutrals.],
    [`StrictType`],
    [Deep],
    [Same as `Type`, plus typechecks the recognizer against `RecognizerShape`, the `respond` against `RespondShape` (§12.6), and the meta against `MetaShape` field-by-field.],

    [`BehavioralType`],
    [Behavioral],
    [Same as `StrictType`, plus runs each Path-typed `behavioral_specs` entry — including the *respond-coherence* Paths (the per-former computation rules, §12.3): a type whose `respond` returns the wrong eliminated type fails here.],
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
`respond := type_predicate_h_rule` (the predicate-side
H-rule, §6.3 and §12.18), `behavioral_specs := none`.

The §11.6 argument depends on this contract *plus* the predicate-side
H-rule: `Type` is a wait-form with a structural recognizer, a
MetaShape-conforming meta, and a respond that returns
`Return (tree_eq (neutral_type v) self_as_hyp)` for neutral
`v` and `Return false` otherwise. The actual recognizer body and
H-rule are §12.18's responsibility; readers can verify they satisfy
this contract there.

== `Type : Type`

The test `test typecheck Type Type` is expected to pass under all
standard validators:

- under `Type` (structural), trivially by construction;
- under `StrictType` (deep), via recursive validation that bottoms out
  by hash-cons memoization;
- under `BehavioralType`, if Type's optional `behavioral_specs` (if any)
  typecheck.

These tests are *runnable mechanical assertions*. Their passing or
failing is observable.

The deeper question — does `typecheck Type Type = Ok true` imply that
disp's type system is foundationally consistent (no Girard / Hurkens
encoding of inconsistency)? — remains open. Our conjecture is yes,
on the strength of the walker's parametricity discipline.

The argument (informal):

*1. Polymorphic types like ⊥ := `Pi Type ({A} -> A)` have no
inhabitants.* The core property: Pi's body check requires the
body's result, applied to the codomain recognizer, to reduce to
*literal `Ok true`*. Two failure modes block ⊥-inhabitation:

+ Stuck symbolic results — trees produced by `hyp_reduce` (or
  composition through it) — are not the tree `Ok true`.
+ Concrete `Ok false` from a decidable respond (notably `Type`'s
  predicate-side H-rule) is also not `Ok true`.

To inhabit ⊥, the body (under a fresh Type-hypothesis `hyp_A`) must
produce a value `v` whose check against `hyp_A` reduces to `Ok true`.
`hyp_A` is a kernel-minted neutral with stored type `Type`, so
applying `hyp_A` to `v` routes through `hyp_reduce`, which consults
`Type`'s respond (`type_predicate_h_rule`, §12.18). That
respond returns `Return (tree_eq (neutral_type v) self_as_hyp)`
— a concrete Bool. The verdict is `true` iff `v` is itself a
kernel-minted neutral whose stored type hash-cons-equals `hyp_A`.

The three classic candidate bodies all fail this way:
- *Introspect A*: rejected directly by walker TriageReflect (§4.2)
  before reduction proceeds.
- *Return a closed term*: closed values fail `is_neutral`, so
  `type_predicate_h_rule` returns `Ok false`.
- *Return `hyp_A` itself via I-shortcut*: `hyp_A` is a neutral, but
  its stored type is `Type`, not `hyp_A`. `tree_eq Type hyp_A = false`
  (distinct hash-cons identities), so `Ok false`.

The deeper invariant: the only way to obtain a `v` with
`neutral_type v = hyp_A` is to receive one via `bind_hyp` at
domain `hyp_A`. The body of `⊥ = Pi Type ({A} -> A)` takes only
`A = hyp_A` as parameter — it has no `A`-typed parameter, and
`bind_hyp` is kernel-privileged (the walker's stem-forge rule
blocks user-side construction of `hyp_reduce`-rooted
wait-forms). Therefore no body-constructible `v` satisfies the
H-rule's `tree_eq` check.

This "decidable-false except for matching-identity hypotheses" framing
handles the Type:Type edge case (`A = Type` is in `Type`) directly:
even when `v = hyp_A` (the I-shortcut path), the stored type is
`Type ≠ hyp_A`, giving `Ok false`.

*2. The argument lifts to Hurkens.* Hurkens-style normalizing
constructions reduce to the ⊥-inhabitation problem at some
intermediate position; (1) blocks it. Specifically, every
intermediate Pi-typed sub-expression in Hurkens' encoding has a
body-check whose final step is `param_apply (B hyp_x) result`,
which propagates the `Ok false`-or-stuck verdict inward.

*3. Self-application terminates.* The structural check is
finite-depth.

#openq[
  The argument is informal and depends on three unproven
  properties:

  - *No formal parametricity theorem.* Reynolds-style parametricity
    for disp's walker discipline hasn't been mechanized.
  - *Reader-layer soundness.* The walker's identity-keyed reader layer
    (§4.2–§4.3) — `I`, plus the descriptor reads `ROOT_SIG` /
    `STORED_TYPE` — might enable more than intended. The litmus test
    (§4.3) argues each reader is a total function of the public
    descriptor, and that the descriptor reads are dominated by the
    already-granted `tree_eq`-on-neutrals carve-out; but a precise
    characterization (and `tree_eq`-on-neutrals itself) is open.
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
    [`Type`], [Wrong wait-form shape, missing meta fields (`respond` included). Cheap.],
    [`StrictType`], [Above plus: recognizer not Pi-typed, `respond` not `RespondShape`-typed, meta fields wrong-typed.],
    [`BehavioralType`],
    [Above plus: documented Path-typed properties of the recognizer *and* the respond-coherence laws (right eliminated type per frame).],
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

Each library type follows the framing of §11. The pattern: define a
recognizer (a closed function `meta -> v -> CheckerResult Bool`),
define a meta record following the MetaShape convention (§11.2),
construct the type as `wait recognizer meta`, and assert validity
via tests. This section opens with the library infrastructure that
supports strict / behavioral validation (the `Telescope`/`Sigma`
projection respond, `Record`, `Refinement`, and the kernel's own
structural types incl. `MetaShape` / `RespondShape`), then walks the
individual type-formers (`Bool`, `Nat`, `Pi`, …).

== `Telescope`, `Sigma`, and projection-by-name

The dependent n-ary record former subsumes `Sigma` and `Record` (§12.4) as
instances. A *telescope* is a chain of *cells*, threaded so later cells' types
see the earlier fields' values:

```disp
t cell0 (λx0. t cell1 (λx1. … t cellN (λxN. t)))   // tail = t (nil)
```

Each **cell is a wait-form** `wait op meta` — inspectable (`pair_fst` = the op's
signature, `type_meta` = its data) AND runnable (applying it runs the op). The op
is the cell's *observation*, and `meta` carries its `name`/`ty`/`recipe`. The four
ops: `mint` (a fresh ∀-bound hyp — a function argument), `proj name` (observe a
record field by honest `lookup_field`), `apply` (observe `v` applied to the prior —
a function codomain), `deriv name recipe` (a *derived* field pinned to `recipe`).
A tail `λx. rest` that ignores `x` makes the cell non-dependent — that degenerate
case is `Record`. Well-foundedness is structural: a tail sees only priors. New
observation modes are new ops, no walker edit (the kernel's "types are open
wait-forms" discipline, at the cell level).

Values of a telescope type are §2.6 *records* (`{ fst := a; snd := b }`),
recognized and projected *by name* through the cut — there are no positional
`pair_fst`/`pair_snd` projection selectors. ONE walker `at` serves both faces:
recognition (`at true`, concrete `v`) and projection-response (`at false`, a neutral).
`at` applies each cell op, which returns a **Step** — pure data — and interprets
it with the recursion and `bind_hyp` *inline*:

```disp
// Step: SMint ty | SThread x | SReject | SDone action
// an op:  op meta mode source prior frame -> Step
//   proj  CHECK:  lookup_field, check ty -> SThread x | SReject ;  STUCK: name-match -> SDone (Extend ty) | reflect -> SThread
//   apply CHECK:  check ty (source prior) -> SThread (source prior) | SReject ;  STUCK: SDone (Extend ty)
//   mint:  SMint ty                       (at mints (CHECK) or lands the codomain (STUCK))
//   deriv CHECK:  lookup_field, pin tree_eq -> SThread recipe | SReject ;  STUCK: name-match -> SDone (Reduce recipe) | SThread recipe
at := fix ({self, mode, tele, source, frame, prior} ->
  if (is_fork tele) then (match ((pair_fst tele) mode source prior frame) {
    SMint ty  => (if mode then (bind_hyp ty ({h} -> self true ((pair_snd tele) h) source frame h))
                          else (match (param_walker (pair_snd tele) frame) { Ok rest => self false rest source frame frame; Err _ => Extend InvalidType }))
    SThread x => (if mode then (self true ((pair_snd tele) x) source frame x)
                          else (match (param_walker (pair_snd tele) x) { Ok rest => self false rest source frame frame; Err _ => Extend InvalidType }))
    SReject _ => Ok false
    SDone a   => a
  }) else (if mode then (Ok true) else (Extend InvalidType)))

Telescope : Tree -> Type := {tele} -> wait (make_recognizer
  {meta, v} -> at true (meta.recognizer_params) v t t       // guard-free (see below)
) (make_meta tele ({params, self, frame} -> at false params self frame t))
```

This is the recursion-schemes split: the op is the per-cell *algebra* (and, being a
wait-form, the open extension point); `at` is the fixed recursion harness. `bind_hyp`
lives in `at`, not the op, *by necessity* — a continuation passed *through* a function
to `bind_hyp` miscompiles under nested binders (the hyp leaks and trips its
occurs-check), so the op returns a `Step` for `at` to interpret. `at true` runs under
the ambient walker (the mint `bind_hyp` and `source prior` are policed); `at false` runs
off-walker (driven by `hyp_reduce`), so it instantiates each tail explicitly under
`param_walker` (a tail that raw-triages a neutral prior routes to the dead state). At
a field: opaque ↦ `Extend ty`, derived ↦ `Reduce recipe` — transparency is the
`Reduce` arm of `Action`.

*No record guard; the empty telescope is `⊤`.* A type is a predicate over `Tree`,
and a telescope type is the *meet* of its cells' obligation-predicates (each cell
carves out the values passing that observation). A non-empty record telescope
rejects a non-record on its own — the `at true` walk's first `proj` `lookup_field` returns
`Err`. So the old `is_record_product v` guard was redundant *except* at the empty
telescope, where it conflated `Telescope t` with "the type of all records." Without
it, `Telescope t` is the empty meet `⋂∅ = ⊤` — the all-trees predicate, definitionally
`Tree` (§"Inert types"). `{}` is thus the *terminal* negative type / nullary product,
distinct from `Unit` (one inhabitant) and `False` (none); "the type of all records"
is a separate predicate (`is_record_product`) if wanted.

*One negative-product former.* `Pi`, `Record`, and `Sigma` are instances of this
single `Telescope` — the kernel shares one walker `at` (both faces). A cell's *op*
is `mint` (a fresh ∀-bound hyp — a function argument), `proj name` (observe a record
field by honest `lookup_field`), `apply` (observe `v` applied to the prior — a
function codomain), or `deriv name recipe` (a derived/δ field). Then `Pi A B =
Telescope [mint x:A ; apply out:(B x)]`; `Record`/`Sigma` are `proj` chains. So `Π`,
`Σ`, records, and `⊤` are one *negative* former (a finite dependent limit), differing
only in cell op — the dual of the *positive* `Coproduct` (a sum / colimit), which is
therefore **not** a telescope (§12.2).

The builder `mk T given` reads the telescope off the *type* `T`'s meta, fills
derived fields from their recipes and opaque ones from `given`, and emits the
full §2.6 record (which the recognizer then re-pins); call sites write
`mk T { … }`.

*`Sigma A B` is the two-entry telescope* `{ fst : A, snd : B fst }` — both
opaque, the second's type depending on the first:

```disp
Sigma : {A : Type} -> (A -> Type) -> Type := {A, B} -> Telescope (
  t (proj_cell "fst" A) ({a} ->
  t (proj_cell "snd" (B a)) ({_} -> t)))

test typecheck Type Sigma            // Sigma is structurally a type
```

A Sigma value is the record `{ fst := a; snd := b }`; a Sigma-*hypothesis*
projects via `acc "fst"` / `acc "snd"` through `at false`, so reading `hyp_AB.snd`
lands `Extend (B hyp_AB.fst)` — the dependency preserved by the first field's
projection-neutral being fed into the tail. The old bespoke `sigma_respond` and
the `walker_pair_fst/snd` selectors are gone.

`Sigma` is the `Σ` corner at an *arbitrary* first-component domain. Restricting
a finite *tag* domain instead gives `Coproduct` (below); the two share the
injection `pair tag payload` and differ only in whether the discriminant ranges
over a value or a fixed name list.

== `Coproduct` and `Tags`

A coproduct value `pair tag payload` (§2.6) inhabits `Coproduct [(V1,S1), …,
(Vn,Sn)]` iff its tag is one of the `Vi` and its payload inhabits the matching
`Si`. It is therefore the finite-tag `Sigma`: the `Σ` whose first component
ranges over the closed tag set `Tags [V1..Vn]`.

```disp
// Tags [V1..Vn]: the finite type of the declared constructor tags (discrete).
let tags_recognizer := make_recognizer ({meta, v} ->
  Ok (list_mem v meta.recognizer_params.names))   // v is one of the declared tags

let tags_meta_for := {names} -> {
  recognizer_params := { names := names },
  functor := trivial_functor, respond := inert_respond, behavioral_specs := none
}
let Tags := {names} -> wait tags_recognizer (tags_meta_for names)

// Coproduct [(Vi,Si)]: tag ∈ {Vi}, payload inhabits S_tag.
let coproduct_recognizer := make_recognizer ({meta, v} ->
  let variants := meta.recognizer_params.variants         // [(V1,S1), …]
  bind (lookup_arm variants (pair_fst v)) ({arm} ->       // arm whose tag = v's tag
    match (is_some arm) {
      true => param_apply (unwrap arm) (pair_snd v)          // payload must inhabit S_tag
      false => Ok false                                          // unknown tag → not an inhabitant
    }))

// Elimination is the cut: a neutral coproduct routes a case-product frame
// through `hyp_reduce`, exactly like a (non-recursive) inductive type — so it
// carries the shared `inductive_respond` (§12.3).
let coproduct_meta_for := {variants} -> {
  recognizer_params := { variants := variants },
  functor := coproduct_functor,
  respond := inductive_respond,
  behavioral_specs := none
}
let Coproduct := {variants} -> wait coproduct_recognizer (coproduct_meta_for variants)
//   Coproduct [(Vi, Si)]  ≅  Sigma (Tags [V1..Vn]) ({t} -> arm_type variants t)

test typecheck Type Coproduct
test typecheck StrictType Coproduct
test coproduct_recognizer (coproduct_meta_for [(V1, Unit)]) (inj V1 unit) = Ok true
```

A concrete coproduct is eliminated by the cut against a product of handlers
(`match v {…}`, §2.6); a neutral one routes that case-product through `respond`,
the same path the inductive eliminator below uses. This is the definition §2.6's
coproduct sugar refers to.

== The library eliminator

Case dispatch on inductive values is library code, not a kernel primitive. It is
the cut (§2.6) made neutral-aware: on a concrete target it runs the cut against
the case-product directly; on a neutral target it routes the case-frame through
`hyp_reduce`. The two cases are gated by `is_neutral`:

```disp
elim := {dispatcher, motive, cases, target} ->
  select_lazy
    ({_} -> target { motive := motive; cases := cases })  // neutral: apply to the frame record
    ({_} -> dispatcher motive cases target)               // concrete: run the dispatcher
    (is_neutral target)

reconstruct_self := {meta} -> wait hyp_reduce meta
```

The neutral branch applies the neutral `target` *directly* to the case-frame
record `{ motive; cases }` — bare juxtaposition, not `param_apply`: `hyp_reduce`'s
`Extend` is already bare (Option A, §7.5), and the juxtaposition lets an enclosing
walk route the elimination once. The inductive type's `respond` extends the spine
with stored type `motive self`. Every inductive shares this one respond: the
action is the same — recursive or non-recursive (`Bool`, `Nat`, `Unit`, `Ord`,
and every `Coproduct`) — because a neutral target cannot be unfolded, so the
response only records that eliminating it yields `motive self`:

```disp
// 3-arg respond (§7.1, §11.2); frame is the record { motive; cases }. The
// motive is routed through `param_walker` against `self` rather than applied
// raw — so a motive that raw-reflects on the neutral routes to the dead state
// (the per-sub-term policing of §12.18) instead of leaking metadata.
inductive_respond := {params, self, frame} ->
  match (param_walker frame.motive self) { Ok ty => Extend ty; Err _ => Extend InvalidType }
```

Each inductive type's meta therefore carries `respond := inductive_respond`
(installed in the `Bool`, `Nat`, `Unit`, and `Ord` metas below, and in
`Coproduct`, §12.2). `respond` is independent of `functor`: a *discrete* type
(trivial transport, §13) still responds to case-elimination.

#note[
  *The case-coherence gate (the motive must not lie).* `inductive_respond` reads
  the result type from `frame.motive` and _ignores_ `frame.cases`. On its own
  that is unsound: nothing forces the cases to
  *inhabit* the motive, so a recursor with a deliberately-wrong motive types as
  anything. Concretely, with motive `{_} -> Nat` but `Bool`-producing cases,
  `nat_rec ({_}->Nat) true ({p,ih}->true)` elaborates to a stuck neutral *typed* `Nat`,
  so `{y} -> nat_rec ({_}->Nat) true ({p,ih}->true) y` is accepted at `Nat -> Nat` even
  though it returns `true ∉ Nat` on every input (witnessed in
  `lib/tests/kernel.test.disp`).

  The dependent eliminator typing rule closes it: at a *use*, the cases must
  inhabit the motive — `base : motive zero`, `step : Π n. motive n -> motive (succ
  n)`. Crucially the gate lives _in the type's `respond`, not in `nat_rec`_ — so
  it is *unbypassable*. Every elimination of a `Nat`-neutral applies the neutral to
  the case-frame (`target (pair motive cases)`), which routes through `hyp_reduce →
  Nat`'s respond; a hand-rolled recursor, or a raw `n frame`, hits the same gate.
  And a recursor cannot avoid that route: the *only other* way to act on a neutral
  is to `triage` it, which the walker rejects (§4) — so the respond is the sole
  chokepoint. The respond reads its own type `T` off the neutral and runs a per-type
  `coherence` checker; failure routes to the dead state `InvalidType` (§12.3), so
  the eliminated neutral is typed `InvalidType` and rejected downstream:

  ```disp
  // 4-arg: the per-type coherence checker `coh` is partially applied in each
  // type's meta. `frame` is the record { motive; cases }; `self` is the
  // reconstructed neutral. On the coherent branch the motive is *policed*
  // through `param_walker self` (§12.18), exactly as in `inductive_respond`.
  gated_inductive_respond := {coh, params, self, frame} -> {
    let r := coh (neutral_type self) frame.motive frame.cases    // T read off the neutral
    match r {
      Err _ => Extend InvalidType                               // a case triaged a hyp, etc.
      Ok v  => if v
               then (match (param_walker frame.motive self) { Ok ty => Extend ty
                                                             ; Err _ => Extend InvalidType })   // coherent
               else (Extend InvalidType) } }                    // cases do not inhabit the motive

  nat_coherence := {T, motive, cases} -> {                      // base : motive zero ;
    // step : Π n. motive n -> motive (succ n). The step gate runs in a RAW
    // context, so its outer binder routes into the walker via `param_apply
    // (bind_hyp T)` (§7.2); `cases.base`/`cases.step` read by name.
    let step_coherent := param_apply (bind_hyp T) ({n} ->
      bind_hyp (motive n) ({ih} -> (motive (succ n)) ((cases.step n) ih)))
    bind ((motive zero) cases.base) ({okb} -> if okb then step_coherent else (Ok false)) }

  // Nat's meta carries `respond := {params,self,frame} -> gated_inductive_respond
  // nat_coherence params self frame`; Bool's a two-line `bool_coherence`.
  ```

  Concrete targets never reach the respond (`elim` runs the dispatcher on them), so
  ordinary computation is value-transparent — the gate fires only on the neutral
  targets that *checking* produces. Two clarifications:

  *Distinct from §12.3's respond-coherence Paths.* Those certify a *former's* respond
  returns the right *type per frame* (per-former, trivially true here); the gate
  certifies, *per use*, that the supplied cases inhabit the supplied motive. Both
  are needed.

  *Enforcement is Pi-free.* The step gate *mint-applies* `step` against the motive
  via `bind_hyp` + recogniser application, and reads `T` off the neutral, so it
  names neither `Π` nor its own type. A type *former* (`Nat`, `Bool`, …) thus
  depends only on `Type` (for an optional `Type (motive n)` motive-check), with no
  self-reference and no `Π` — the eliminator's *nominal* `Π`-type is an ascription,
  not load-bearing for checking. (`Π` and `Type` need no such gate: `Π`'s
  eliminator is application, whose coherence is `pi_body`'s body-check, and `Type`
  has no inductive eliminator — they are the foundation the data types layer on.)
]

=== Inert types, `InvalidType`, and the dead state

`respond` is constitutive (§11.2): every type has one, so there is no `none`
case. A type that *responds to no frame* — `I`, `IsOne`, `String`, `Glue`,
`Tags`, `False`, `Tree`, `Refinement` — carries the canonical `inert_respond`,
which rejects every frame to the dead state. Rejection is not a separate
`Action` tag; it is `Extend` at the distinguished type `InvalidType`, which is
itself inert, so the dead state is *absorbing* (any further frame stays at
`InvalidType`):

```disp
// inert_respond: ignore all three args (§7.1), reject to the dead state. It is
// parametric (touches none of params/self/frame), so it inhabits RespondShape
// (§12.6) — the basis for the kernel self-typing `inert_respond : RespondShape`.
inert_respond : RespondShape := {params, self, frame} -> Extend InvalidType

// The dead-state type: uninhabited (recognizer is `Ok false`), and inert under
// elimination. A neutral stored at InvalidType is "an elimination that did not
// apply." Its OWN respond is `Extend (neutral_type self)` — NOT `inert_respond`:
// `self` is an InvalidType-typed neutral, so `neutral_type self` is InvalidType,
// keeping the dead state absorbing WITHOUT a self-reference to the name
// `InvalidType` under definition (no `fix`, no forward reference).
InvalidType := wait (make_recognizer ({m, v} -> Ok false))
                    (make_meta unit_witness ({params, self, frame} -> Extend (neutral_type self)))

test typecheck Type InvalidType
test param_apply InvalidType zero = Ok false       // genuinely empty
```

This is what folds the former three-tag `Action` (`Extend | Return | Invalid`)
into two tags: `Invalid` was definitionally `Extend InvalidType` (the `hyp_reduce`
rejection branch built exactly that tree, §7.1), so naming it as a third case
bought nothing. A `respond` rejects by returning `Extend InvalidType`; the
ordinary `Extend` branch of `hyp_reduce` handles it.

=== Respond-coherence as runnable Paths

A type former's `respond` is *certified*, not trusted on sight. Each former
ships, in its `behavioral_specs` (§11.2), the *coherence law* relating `respond`
to the type's intended elimination — the per-former bullets of §11.2 turned into
runnable `Path`s, which `BehavioralType` (§11.4) runs:

```disp
// A coherence law is a Path (§13) populating the former's behavioral_specs list.
// Π-coherence: applying a Π-typed neutral to an argument lands at the codomain
// instantiated there. (The H-rule concretizes the hypothesis applications, so
// this reduces to a tree_eq and the Path is genuinely runnable.)
pi_respond_coherence := {A, B} ->
  bind_hyp (Pi A B) ({n} -> bind_hyp A ({a} ->
    Path Type (neutral_type (n a)) (B a)))

// Inductive-coherence: eliminating a neutral lands at `motive self`.
// `elim_for T` is the type's own case dispatcher (§12.3).
inductive_respond_coherence := {T, motive, cases} ->
  bind_hyp T ({n} ->
    Path Type (neutral_type (elim (elim_for T) motive cases n)) (motive n))
```

Two enforcement gates, with different strengths:

- *`StrictType` is the unconditional gate.* It checks `respond : RespondShape`
  (§12.6) for *every* type, with no escape hatch. So a "valid (strict) type
  without a well-typed `respond`" is *impossible* — a missing or ill-typed
  `respond` fails `StrictType` outright. This already secures the goal
  "not a type unless it has a well-typed `respond`."
- *`BehavioralType` adds coherence.* It looks the type's former up in the
  former→coherence-law association (each former registers its law, as
  `pi_respond_coherence` / `inductive_respond_coherence` above) and *runs* that
  law. A former with no registered law cannot be behaviorally certified — its
  types pass `BehavioralType` only *vacuously*, which a reviewer reads as
  "coherence unproven," not "coherence holds." A registered-but-false law fails
  outright. So `BehavioralType` is exactly as strong as the laws formers declare.

Because formers are certified once and user types *inherit* a former's `respond`
(and its law), a user type cannot carry a wrong `respond`; only a *new* former
introduces a fresh `respond`, and it is the new former's obligation to register
a coherence law that `BehavioralType` can run.

#note[
  *What "proven respond" does and does not cover.* The coherence Paths above
  prove `respond` returns the *right eliminated type* per frame — a finite,
  decidable property, fully runnable. They do *not* discharge the §7.1
  soundness obligation that `respond`'s `Return v` channel only ever surfaces
  *public-derived* data (the DCC `[BindM]` non-interference discipline): that is
  a parametricity property, not a finite equation, so it cannot be a runnable
  `Path` and remains in the trusted base — the same open frontier as the
  Type:Type conjecture (§11.6, Appendix A). `Extend`-only responds (every
  former except `Type`'s predicate-side H-rule) trivially satisfy the obligation
  — `Extend` names a type and surfaces no payload — so the residue is confined
  to the handful of `Return`-using responds.
]

The concrete branch's `dispatcher` is walker-safe: it triages on
`target` — directly for tagged sums, through the functor view for
shape-encoded types (`Bool`/`Nat`/`Ord`, §2.7) — and the gate guarantees
`target` is concrete (triage on concrete values is walker-safe).

The
distinction is purely about how the payload was constructed:
`bind_hyp` for hypotheses, `hyp_reduce` for spine-extended forms
(including stuck eliminations).

== `Record`

A record is the *non-dependent* telescope (§12.1): a constant-tail (K-stem)
telescope where every tail ignores its bound field, so later entries' types do
*not* depend on earlier values. It shares Telescope's single `at` walk (both
faces). The flat field list `[(Vi, Ti)]` lifts to the telescope *lazily* — at
recognition/projection, via `fields_to_tele`, not at formation — so `Record`
stays parametric in its field-list params and `Record : Tree -> Type`
self-verifies:

```disp
// Lift a flat (name, type) list to a constant-tail telescope of proj_cells: each
// tail is `t t (rest)` (the K-stem — ignores the bound field).
fields_to_tele := fix ({self, fields} ->
  if (is_fork fields) then {
    let arm := pair_fst fields
    t (proj_cell (pair_fst arm) (pair_snd arm)) (t t (self (pair_snd fields)))
  } else t)

Record : Tree -> Type := {fields} -> wait (make_recognizer    // guard-free, like Telescope
  {meta, v} -> at true (fields_to_tele meta.recognizer_params) v t t
) (make_meta fields ({params, self, frame} -> at false (fields_to_tele params) self frame t))
```

The value is `prod F`, where the field table `F = pair names payload`: `names`
is the ordered field-name list in one hash-consed header node, `payload` the
`const`-wrapped field values. The recognizer runs `at true` over the lifted
telescope, which projects and checks each field by name. (The old
`record_recognizer` / `sigma_chain_recognizer` were the degenerate Sigma-chain
case and are gone.)

Because the names are in the *value* (its field table), two records with
identical field types but different names (`{a:Nat,b:Nat}` vs `{p:Nat,q:Nat}`)
have distinct headers and are told apart at the value boundary, not merely as
distinct types.

*Structural check and row subtyping.* the `at true` walk traverses the declared fields,
projecting each by name (`field v name`, the §2.6 cut, O(1) per field) and
checking it — a *structural* "`v` has these fields, well-typed" check, the
natural basis of the row-polymorphism story (§15, `extends R Console`): it does
not reject a *wider* `v` carrying extra fields. An *exact* recognizer — "names
are exactly these, in order" — is a stricter variant: a `Refinement` whose
predicate additionally compares `v`'s field-table header to the declared names
(one O(1) `tree_eq` on the hash-consed header). Closed records opt into the
exact check; open/extensible records keep the structural one. They are distinct
types, not one recognizer serving both.

=== Record as finite `Pi`; typing the cut

`Record [(Vi, Ti)] ≅ Π (i : Tags). T_i` — a dependent function from the field
index to the field type. Field access is `Π`-application: `r (acc a)` selects the
`a`-th component. Dually, `Coproduct [(Vi, Si)]` (§12.2) is the finite-tag `Σ`.
The cut couples them: `P C` typechecks exactly when the product `P` inhabits the
handler-record the coproduct `C` demands.

#figure(
  table(
    columns: 1,
    stroke: none,
    inset: 5pt,
    align: center,
    [`C : Coproduct [(Vi, Si)]     P : Record [(Vi, Si -> R)]`],
    [#line(length: 70%, stroke: 0.5pt)],
    [`P C : R`           #h(2em) (the cut = `Σ`-elimination)],
  ),
  caption: [Product and coproduct must be `Π`/`Σ` duals over the same index.],
)

The handler-record `Record [(Vi, Si -> R)]` *is* `Handlers (Coproduct [(Vi,Si)])
R` — the curry iso `(⊕ Si) → R ≅ ∏ (Si → R)`. Compatibility is therefore an
ordinary `Record` recognition: `P` must have exactly the fields `{Vi}` (the exact
header match above ⇒ *exhaustiveness*), each typed `Si -> R`. The dependent form
`P : Record [(Vi, Si -> R_i)]` gives `P C : R_(tag C)`. *Field access is this with
`Si = Unit` and `R_i = T_i`*: `r (acc a) : T_a`, and since the accessor `a` is a
literal, `T_a` is static. `match`, projection, and function application are thus
one typed operation.

=== Projection is the cut

`r.x` is not a positional shortcut the elaborator computes; it is the cut of the
record against the literal accessor `acc x` — `Π`-application at one index, whose
codomain depends on the name:

```disp
r.x   ≡   r (acc x)               // r (acc x) : field_type F x
//   F = r's type's field list, from the binder; field_type F x is the type at name x
```

The result type `field_type F x` depends on `F` — the field list carried by `r`'s
*type* `Record F`, from the binder — not on the value's header; the recognizer's
the `at true` walk projects `r`'s fields by the names in `F`, so the two agree. An
out-of-range name has no field in `F`, so the cut has no inhabitant and fails
as a verdict `Ok false`, never a host throw. Projection reuses the cut's `Π`/`Σ`
machinery instead of a bespoke resolution pass.

*Concrete records resolve by reduction, trust-free.* For a literal name, `r (acc
x)` is a subterm closed in `r`: the cut, `proj`, and the `const` wrap all reduce
by ordinary `apply`, so the call β-reduces to the direct path
`pair_fst (pair_snd^idx payload)`. This is unconditional reduction — sound on all
inputs, no certificate — and hash-cons shares the normal form across every
projection of that field. The elaborator decides *nothing*: it emits `r (acc x)`
and the kernel reduces — anything that would otherwise be computed from the type
is written as a closed term and left to reduction.

*Neutral records resolve through `respond`.* When `r` is a hypothesis, reading
its field table would triage on a neutral, which the walker forbids. There
projection is an elimination, routed through the record type's `respond` — which
delegates to the `at false` walk (§12.1) over the lifted telescope `fields_to_tele
F`, reading the field list from the hypothesis's *stored type* (supplied by the
binder, not inferred). So names are read from the value's field table on
concrete records and from the type on neutral ones; neither path trusts an
elaborator computation.

=== Strip and the fast path

The reduction above needs no `strip` — it is plain β. `strip` (§10,
type-directed and certificate-gated) enters only where a check or scaffolding is
genuinely elided:

- *Dynamic names.* `r (acc nameExpr)` with a non-literal name cannot β-collapse;
  the cut keeps a runtime field lookup, and the name's validity is a real
  contract. `strip` removes it given a validation certificate — the literal
  `checked` story (§10).
- *Scaffolding drop.* On a validated record with no surviving dynamic or
  reflective projection, `strip` removes the `prod`/`const` wrappers and the name
  header, lowering the value to a bare positional tuple and every literal
  projection to `pair_fst (pair_snd^idx payload)`. This is the record row of the
  §10 erasure table — a representation lowering, certificate-gated and opt-in.

Reduction stays the unconditional optimizer (collapsing literal projection to a
direct path); `strip` stays the certified eliminator (removing checks and the
`prod`/`const`/header scaffolding, whose redundancy a certificate vouches for).

Tests:

```disp
test typecheck Type Record
test typecheck StrictType Record
```

== `Refinement`

`Refinement A P` values are inspected as values of A with an attached P-proof.
The recognizer checks the candidate inhabits `A` and then that the bare-Bool
predicate `P` holds; its `respond` is currently the canonical `inert_respond`
(a Refinement-neutral does not project — it rejects every frame to the dead
state):

```disp
Refinement : {A : Type} -> (A -> Bool) -> Type := {A, P} -> wait (make_recognizer
  {meta, v} -> {
    let A := meta.recognizer_params.base
    let P := meta.recognizer_params.pred
    bind (A v) ({va} -> if va then (Ok (P v)) else (Ok false))   // v : A, then P v (a bare Bool)
  }
) (make_meta { base := A; pred := P } inert_respond)
```

#openq[
  *Passthrough `respond` (target).* A more useful Refinement-neutral would
  *delegate* to `A`'s `respond` — retargeting the neutral to `A` so a
  `Refinement Record […]` hypothesis projects fields exactly as the underlying
  `Record` does. The current kernel keeps Refinement *inert* (above); the
  passthrough variant is a future refinement, not yet landed. (Where the
  earlier draft used "`MetaShape` is a Refinement of Record whose body projects
  fields," `MetaShape` is now a plain `Record`, §12.6, so it projects directly
  and does not need the passthrough.)
]

== The kernel's structural types (`Tree`, `Frame`, `NeutralMeta`, `Action`, `MetaShape`, `RespondShape`)

The kernel typechecks its own internals: the structural types its primitives
range over are *first-class library types*, built from the formers of this
section (§12.1–§12.7) and exported, so the kernel's own helpers self-verify
through `param_apply` at elaboration time (e.g. `make_meta : Tree -> Tree ->
MetaShape` and `inert_respond : RespondShape`).

```disp
// "Any tree" — the de-facto top type; recognizer is `Ok true`, respond inert.
Tree        : Type := wait (make_recognizer ({m, v} -> Ok true)) (make_meta unit_witness inert_respond)
Frame       : Type := Tree                                   // a frame; the stored type interprets it
NeutralMeta : Type := Record [(stored_type, Type), (payload, Tree)]   // a neutral's metadata (§6.1)

// The two-tag elimination protocol (§7, §12.3) as a library coproduct.
Action      : Type := Coproduct [(Extend, Type), (Reduce, Tree)]      // Return v := Reduce (Ok v) (notation)

// The metadata record every type carries (§11.2): a plain `Record`, fields
// loosened to `Tree` so structural membership suffices.
MetaShape   : Type := Record [(recognizer_params, Tree), (functor, Tree),
                              (respond, Tree), (behavioral_specs, Tree)]

// A respond `params -> self -> frame -> Action` (params/self loosened to Tree).
RespondShape : Type := Arrow Tree (Arrow Tree (Arrow Frame Action))
```

A `respond` inhabits `RespondShape` iff it does not raw-triage its arguments —
which holds for the *parametric* responds (`inert_respond`,
`inductive_respond`, and the Pi/Telescope family responds that only `Extend
(params.cod frame)` / walk to a field). `make_meta` and `inert_respond` self-type
against these, checked by the auto-`verify` of each kernel fragment.

```disp
test verify (use "lib/kernel/cut.disp")      // make_meta : Tree -> Tree -> MetaShape
test verify (use "lib/kernel/engine.disp")   // inert_respond : RespondShape
test typecheck Type MetaShape
test typecheck Type RespondShape
```

#openq[
  *`StrictType`, `RecognizerShape`, and the inspecting-respond wall (target).*
  The deep validators `StrictType` / `BehavioralType` and the companion
  `RecognizerShape` (the shape `Pi MetaShape ({_} -> Pi Tree ({_} ->
  CheckerResultBool))` every recognizer should inhabit) remain a *target*, not
  yet landed. The obstacle is concrete: only *parametric* responds inhabit
  `RespondShape`. The *inspecting* responds — Nat/Bool/Pi/Sigma/Eq/Coproduct/
  Record, which read their meta-hyp to dispatch — raw-read a hypothesis and are
  walker-rejected, so they cannot be typed `RespondShape` as written. Closing
  this needs either `NeutralMeta` reified as a concrete record with sanctioned
  projections (responds rewritten to use only those) or a privileged typing
  rule for responds. (Step 1 — a real empty `InvalidType`, §12.3 — has landed;
  the per-former `Frame` types of Steps 2–4 are deferred. Pinned in
  `metashape.test.disp`.)
]

== Typed application: `checked` and `checked_apply`

`checked` is the library construction for typed function values — a
manifest contract pairing a function with its declared *input* (domain)
type. When applied to an argument, the wait-form's reduction fires
`checked_apply`, which checks the argument against that domain before
invoking the function.

Function application is the *intensional* cut (§2.6). A function is a `Π`-product
over an *arbitrary* domain, so its graph is not a finite stored table: the
component for an argument is *computed*, not selected by name. `checked_apply` is
that computed cut — the same `Σ`-value-against-`Π`-consumer elimination as
`match` and projection, specialized to a domain too large to tabulate. The
contract check is the price of the arbitrary domain: there is no finite tag set
to guarantee a hit, so the argument is validated against the stored domain `A`
before the function runs.

```disp
// `meta` is the headered record `{ dom, fn }` carried in the wait-form's
// payload (recovered by the wait reduction), read by name — the same
// one-record discipline type metas follow (§2.6, §11.2). `dom` is the
// function's input (domain) type; `fn` is the underlying function.
let checked_apply := {meta, arg} ->
  let A  := meta.dom
  let fn := meta.fn
  bind (param_apply A arg) ({verdict} ->
    match verdict {
      // Domain holds. Apply `fn`, which is *always* one of two things:
      // a raw function (its application is a bare reduct, so wrap with
      // Ok) or a `bind_hyp`-wrapped neutral hypothesis (its application
      // routes through `hyp_reduce` and is *already* a CheckerResult, so
      // pass it through un-nested). The `is_neutral fn` split is what
      // keeps the result from double-wrapping into `Ok (Ok …)` (§7.5).
      true => match (is_neutral fn) {
              true => param_apply fn arg
              false => Ok (param_apply fn arg)
            }
      // Domain fails. A contract boundary promised the arg would fit, so
      // this is breakage, not a verdict (§3, verdict-vs-error).
      false => Err (TypeMismatch { expected = A, actual = arg, span })
    })

let checked := {A, f} -> wait checked_apply { dom := A, fn := f }
```

`checked` stores only the *domain*: that is all the input-check needs,
and it is what `Pi`'s recognizer reads to confirm a candidate's input
type (§12.9). The codomain is the recognizer's own (the expected `B`),
re-checked against the body and never read off the value, so a `checked`
value carries no codomain. There is no `is_pi` / `NotApplicable` gate:
`checked` only ever wraps a function, so the stored `dom` is the
operative type and the question "is the ascription a Π?" does not arise.
(`typed_lambda` and a certificate-issuing `validate` are gone — §8: a
lambda is wrapped directly with its domain, and validation is a verdict,
not a wrapper.)

*Walker-safe body.* `checked_apply`'s operations — pair projections,
`is_neutral`, `param_apply` — are all walker-safe (no stem-forge of
pinned sigs, no triage on neutrals). The function lives in the library;
no kernel privilege required.

*Dispatcher behavior.* The wait-form
`wait checked_apply { dom := A, fn := f }` has the sig
`checker_sig checked_apply` — a library sig, not in `Σ`.
So the dispatcher does not route it specially; walker reduction handles
`(checked A f) arg` by reducing via wait's bracket-abstracted shape to
`checked_apply { dom := A, fn := f } arg`. Internal
`param_apply` calls then route through the dispatcher normally.

*`is_applicable_type`* (a general reflection helper, no longer consulted
by `checked_apply`):

```disp
is_applicable_type := {T} ->
  match (safe_is_fork T) {
    false => false
    true => not (tree_eq (meta_get (safe_pair_snd T) "respond") inert_respond)
  }
```

Returns true iff T's `respond` is not `inert_respond` (§12.3) — i.e., T
accepts at least one elimination frame. (Every type now *has* a respond,
so the test is "non-inert," not "present"; sharper Π-specific checks use
`is_pi` / `pi_dom`.)

== `safe_*` helpers: wrapper reads vs value decomposition

Two needs — *wrapper reads* and *value decomposition* — are easy to conflate
here, and §4.2's reader layer is exactly what keeps them apart. Raw `triage` /
`pair_snd` on a seal is walker-rejected (TriageReflect, §4.2) for *both*, so
neither can use raw projection — but they diverge on what a *correct* answer is:

*Wrapper reads* — "what roots this value, and what type does it store?" — have a
concrete answer even on a seal (the public descriptor), so they go through the
canonical readers `ROOT_SIG` (= the blessed `pair_fst` tree) and `STORED_TYPE`
(§4.2), which short-circuit before the guard and return that descriptor:

```disp
// Wrapper reads (concrete on seals; built on the §4.2 readers + O(1) tree_eq).
//
// A root-sig read is *always* walker-safe — ROOT_SIG (§4.2) short-circuits
// before the guards — so "what roots this value?" needs no `safe_`-prefixed
// variant. "Neutral" and "stuck form" are the *same* predicate: all stuck forms
// are hyp_reduce-rooted (§6.1), one O(1) tree_eq, Σ-independent. It is total and
// infallible, so it returns a bare Bool (no CheckerResult wrapping):
is_neutral := {v} -> tree_eq (ROOT_SIG v) (checker_sig hyp_reduce)
//
// (The former `safe_is_neutral` / `safe_is_stuck` are gone — they were this same
//  comparison with inconsistent wrapping, `Ok Bool` vs bare `Bool`. The `safe_`
//  prefix is now reserved for *value-decomposition that must stay symbolic on a
//  neutral* — `safe_is_fork` / `safe_pair_fst` below — never for a root read.)

// The Ok-wrapped root-sig read, for recognizer bodies that thread CheckerResult.
safe_has_sig := {checker, v} -> Ok (tree_eq (ROOT_SIG v) (checker_sig checker))

// Stored type of any neutral. Uses STORED_TYPE — *not* raw
// `neutral_meta_type (pair_snd v)` — because `pair_snd` is not a reader (it
// would surface the meta's protected payload, §4.2/§4.3); STORED_TYPE projects
// straight to the type slot.
neutral_type := {v} -> STORED_TYPE v
```

*Value decomposition* — "is the value this seal *represents* a leaf / stem /
fork?" — has no concrete answer for an unknown, so it must stay *symbolic*. These
route through the library `elim` (§12.3), which gates on `is_neutral` and, for a
stuck target, pushes a case-frame into `hyp_reduce`, yielding a `make_hyp`
("this structural property of an unknown value"). They are *not* readers:

```disp
safe_is_fork := {v} -> elim
  tree_shape_dispatcher
  (const Bool)
  {leaf := false, stem := {_} -> false, fork := {_, _} -> true}
  v

safe_pair_fst := {v} -> elim                       // decompose the *represented value*
  tree_shape_dispatcher
  (const Tree)
  {leaf := error_form, stem := {c} -> c, fork := {l, _} -> l}
  v
```

The distinction resolves a concrete bug: a recognizer running under the
walker needs *wrapper* facts (its argument's root sig and stored type), which the
old `elim`-routed `safe_*` returned as a `make_hyp` — symbolic where the H-rule
needs a concrete `tree_eq`. The readers fix that: wrapper reads are concrete on
seals; only genuine value-decomposition stays symbolic. For concrete values both
columns reduce to the raw operation's result; the difference shows only on seals.

*Where they're used*: primarily *inside* `make_recognizer`'s wrapper.
The H-rule check `is_neutral v` runs on the recognizer's v argument
before delegating; for any handler-rooted stuck v whose stored type
matches the recognizer's self-type, the H-rule branch fires and the
recognizer short-circuits with a concrete `Ok true`.

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
`Ok true` directly. Without this, recognizers reject their own
hypothesis values, and Pi-checks of polymorphic functions cascade-fail
(see §6 "Stuck forms and neutrals" for the full story).

The H-rule is universal — every recognizer needs the same wrapping
logic. The library provides `make_recognizer` so per-type authors
write only the concrete-case body:

```disp
let recognizer_wrap_fn := fix ({wrap, body, meta, v} ->
  // Reconstruct this type via the `fix` self-param `wrap` (= recognizer_wrap_fn),
  // not the external let-name: `wrap` is in scope here without a forward
  // reference, and the two are the same tree, so `self_type` hash-cons-equals
  // any externally-built `wait (make_recognizer body) meta` (cf. §14 openq).
  let self_type := wait (wait wrap body) meta in
  match (is_neutral v) {                                 // Σ-independent
    true => Ok (tree_eq self_type (neutral_type v))   // H-rule
    false => body meta v                                    // concrete dispatch
  })

let make_recognizer := {body} -> wait recognizer_wrap_fn body
```

The check `is_neutral` (§12.8) accepts any wait-form rooted at
`hyp_reduce` — all stuck forms (raw hypotheses, spine extensions, stuck
eliminations) share this one constructor (§6.1), so the single root
comparison covers them all. The check is Σ-independent. This is what
lets effect-using polymorphic functions check: a Pi-body containing
stuck-elimination intermediate values (e.g. `nat_rec`) is recognized as
a neutral and short-circuited via the H-rule, rather than propagating
stuck-typed intermediates into spurious body-check failures.

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
let my_recognizer := make_recognizer ({meta, v} -> /* concrete-only body */)
```

The body sees only concrete v values; the wrapper handles hypothesis
cases. Bodies can use raw `triage`, `pair_fst`, etc. — they don't
need `safe_*` helpers, because they never run with hypothesis args.

#note[
  *Recursive recognizers must re-apply the H-rule at every level.* The wrapper
  above runs the H-rule only at the *top*. That is enough for a non-recursive
  body, but a *recursive* body that descends into the value's children hits a
  subtlety: a constructor over a neutral — `succ hyp = fork(leaf, hyp)` — has a
  *concrete* root (so the top-level H-rule does not fire) but a *neutral child*.
  A body that recurses into that child with raw `triage` triages a neutral, which
  the walker rejects (`Parametricity`), so `succ hyp` fails to recognise as a
  `Nat` and `succ y : Nat -> Nat` is spuriously rejected.

  The fix is *open recursion*: `make_rec_recognizer` threads the reconstructed
  recogniser `self` (the same `wait (wait wrap body) meta` the H-rule already
  builds) into the body, and the body recurses through `self` rather than a
  private raw predicate. A neutral child then re-enters the H-rule (recognised by
  stored type); a concrete child recurses structurally. No new memo-stability
  obligation arises — `self` is the reconstruction the top-level H-rule already
  depends on (§14).

  ```disp
  let recursive_recognizer_wrap := fix ({wrap, body, meta, v} ->
    let self := wait (wait wrap body) meta in
    match (is_neutral v) {
      true => Ok (tree_eq self (neutral_type v))   // H-rule, unchanged
      false => body self meta v                          // pass `self`: recurse via the H-rule
    })
  let make_rec_recognizer := {body} -> wait recursive_recognizer_wrap body

  // Nat's body recurses through `self`, so `succ hyp` is a Nat (the neutral
  // predecessor is recognised by the H-rule, not triaged):
  let nat_body := {self, m, v} ->
    triage (Ok true) ({_} -> Ok false)
      ({l, r} -> match (tree_eq l t) { false => Ok false; true => self r }) v
  let Nat := wait (make_rec_recognizer nat_body) nat_meta
  ```

  `Nat` and `Ord` (the recognisers with structural recursion into children) use
  `make_rec_recognizer`; the rest stay on `make_recognizer`. Recognisers that
  recurse by delegating child-checks to *field/component types* (`Record`,
  `Sigma`, `Coproduct`) are already H-rule-aware via that delegation and need no
  change. *The principled end-state* is to generate recognisers from inductive
  *declarations*, wiring each recursive occurrence through the full recogniser by
  construction; `make_rec_recognizer` is the runtime residue of exactly that knot.
]

== Library function `type_recognizer`

`Type`'s recognizer (formerly the kernel's `predicate_frame` handler,
relocated to library):

```disp
let type_recognizer := make_recognizer ({meta, v} ->
  bind (safe_is_fork v) ({is_pair} ->
    match is_pair {
      false => Ok false
      true => bind (safe_pair_fst v) ({rec_field} ->
            // Structural check 1: recognizer field is make_recognizer-formed.
            // Catches recognizers that bypassed make_recognizer; they'd be
            // missing H-rule and would silently misbehave on hypothesis args.
            bind (safe_has_sig recognizer_wrap_fn rec_field) ({is_wrapped} ->
            match is_wrapped {
              false => Ok false
              true => bind (safe_pair_snd v) ({meta_field} ->
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
let bool_recognizer := make_recognizer ({_, v} ->
  Ok (or (tree_eq v true) (tree_eq v false)))

let bool_meta := {
  recognizer_params := unit_witness,
  functor := trivial_functor,            // discrete: identity transport
  respond := inductive_respond,          // case-elimination on a neutral Bool (§12.3)
  behavioral_specs := none
}

let Bool := wait bool_recognizer bool_meta

test typecheck Type Bool
test typecheck StrictType Bool
test bool_recognizer bool_meta true = Ok true
test bool_recognizer bool_meta false = Ok true
test bool_recognizer bool_meta zero = Ok false
// H-rule test: hypothesis of Bool is recognized as Bool inhabitant.
test bool_recognizer bool_meta (make_hyp_form Bool) = Ok true
```

Body is concrete-only (no H-rule logic — `make_recognizer` provides
it). Discrete: transport is identity. The behavioral tests assert
the recognizer accepts canonical booleans, rejects non-booleans,
and (via H-rule) accepts hypothesis arguments.

== `Nat`

```disp
let nat_recognizer := make_recognizer ({_, v} ->
  // v is Nat iff v is zero (= LEAF) or v = fork(LEAF, n) where n is Nat.
  Ok (fix ({self, x} ->
        triage true                          // leaf: zero
          ({_} -> false)                      // stem: not a Nat
          ({l, r} ->                       // fork: zero-like + recurse
            and (tree_eq l t) (self r))
          x) v))

let nat_meta := {
  recognizer_params := unit_witness,
  functor := trivial_functor,
  respond := inductive_respond,          // case-elimination on a neutral Nat (§12.3)
  behavioral_specs := none
}

let Nat := wait nat_recognizer nat_meta

test typecheck Type Nat
test typecheck StrictType Nat
test nat_recognizer nat_meta zero = Ok true
test nat_recognizer nat_meta (succ zero) = Ok true
test nat_recognizer nat_meta true = Ok false
test nat_recognizer nat_meta (make_hyp_form Nat) = Ok true   // H-rule
```

== `Pi`

The pivotal one. The recognizer mints a fresh `A`-hypothesis and applies the
candidate to it, checking the result against the codomain — *raw application
under the walker*: the recognizer body is written `(B hyp) (v hyp)`, and the
enclosing walk (`bind_hyp` is merged into the walker, §7.2) polices every use
of `hyp`, so a `v` or `B` that raw-reflects on the hypothesis is rejected. No
`checked`-wrapping or `pi_dom` extraction is involved (that earlier
manifest-contract machinery is not in the minimal kernel — see the note):

```disp
Pi : {A : Type} -> (A -> Type) -> Type := {A, B} -> wait (make_recognizer
  {meta, v} -> {
    let A := meta.recognizer_params.dom
    let B := meta.recognizer_params.cod
    bind_hyp A ({hyp} -> (B hyp) (v hyp))   // walk polices hyp; typecheck (v hyp) against (B hyp)
  }
) (make_meta { dom := A; cod := B }
   // Respond for a Π-typed neutral: the frame is the argument, the spine
   // extends at the codomain instantiated there (§6.2, §11.2). `params` is
   // passed directly by `hyp_reduce` (the 3-arg respond, §7.1), so `cod` is a
   // plain field read — no `type_meta (neutral_type ·)` round-trip.
   ({params, self, frame} -> Extend (params.cod frame)))

Arrow : Type -> Type -> Type := {A, B} -> Pi A ({_} -> B)
```

#note[
  *No `checked` wrapper in the minimal kernel.* The `checked` / `checked_apply`
  manifest-contract construction (§12.16) and the `is_pi` / `pi_dom` helpers it
  used are *not* part of the current two-Σ-op kernel: a Pi-typed value need not
  be a `checked` wait-form, and the recognizer applies the *raw* function under
  the walker rather than gating on a stored domain. Recognition soundness comes
  from the walker policing each user sub-term (§7.2), not from a contract
  boundary. `checked` remains describable as a library ergonomics layer over
  this, but the kernel does not require it.
]

```disp
test typecheck Type Pi
test typecheck (Pi Nat ({_} -> Bool)) is_zero    // is_zero has this Pi type
```

The recognizer is soundness-preserving by the walker: minting the
hypothesis (`bind_hyp A`) and applying the raw body under the walk means a
body-type mismatch surfaces as a `TypeMismatch` and a hypothesis that escapes
its binder as an `Escape` (§7.2, §8.1) — the kernel operations the walk routes
through raise both. A `v` that raw-reflects on `hyp` (triage/`pair_snd` on the
neutral) is rejected by the walker, so functions that quantify over functions
validate without a `checked` wrapper.

== `Unit`

Single-inhabitant type. The recognizer accepts exactly `unit_witness`
(which is the substrate `LEAF`, i.e. `t`).

```disp
let unit_recognizer := make_recognizer ({_, v} ->
  Ok (tree_eq v unit_witness))

let unit_meta := {
  recognizer_params := unit_witness,
  functor           := trivial_functor,
  respond           := some inductive_respond,   // case-elimination on a neutral Unit (§12.3)
  behavioral_specs  := none
}

let Unit := wait unit_recognizer unit_meta

test typecheck Type Unit
test unit_recognizer unit_meta unit_witness = Ok true
test unit_recognizer unit_meta true            = Ok false
```

== `Eq`

Propositional equality. `Eq A x y` is inhabited by `refl_eq` (the
canonical discrete witness) exactly when `tree_eq x y = true`. Cubical
`Path` (§13) is the computational counterpart, with its own `refl`
(§13.3); `Eq` is the discrete one.

```disp
// `recognizer_params` is the record { type; lhs; rhs }; the canonical proof is
// the bare value `refl := t` (proofs erase to one value).
Eq : {A : Type} -> A -> A -> Type := {A, x, y} -> wait (make_recognizer
  {meta, v} -> {
    let p := meta.recognizer_params                       // { type; lhs; rhs }
    Ok (and (tree_eq v refl) (tree_eq p.lhs p.rhs))      // v ≡ refl AND endpoints hash-cons-equal
  }
) (make_meta { type := A; lhs := x; rhs := y }
   // J on a neutral equality: eliminating a stuck `p : Eq A x y` lands the
   // motive at the RHS endpoint — `motive rhs`, the J rule `C : (y:A) ⊢ C y` —
   // policed through the walker (§12.18), not applied raw.
   ({params, self, frame} ->
     match (param_walker frame.motive params.rhs) { Ok ty => Extend ty; Err _ => Extend InvalidType }))

test typecheck Type Eq
test typecheck (Eq Nat zero zero) refl
test param_apply (Eq Nat zero zero) refl = Ok true
test param_apply (Eq Nat zero (succ zero)) refl = Ok false
```

The cross-endpoint check (`tree_eq p.lhs p.rhs`) is the load-bearing
piece: `refl` inhabits `Eq A x x` for any `A` and any `x`, but never
`Eq A x y` when `x ≠ y` (in hash-cons identity).

== `Ord`

Countable ordinals: `zero_ord`, `succ_ord o`, `omega_plus o`. Used as
a recursion measure in proofs and well-founded induction.

```disp
let ord_recognizer := make_recognizer ({_, v} ->
  // v is in Ord iff v is zero_ord, succ_ord o' for o' in Ord, or
  // omega_plus o' for o' in Ord. Structural recursion bottoms out.
  Ok (fix ({self, x} -> /* tagged-shape recursion */) v))

let ord_meta := {
  recognizer_params := unit_witness,
  functor           := trivial_functor,        // discrete transport
  respond           := some inductive_respond,  // ord_rec on a neutral Ord (§12.3)
  behavioral_specs  := none
}

let Ord := wait ord_recognizer ord_meta

test typecheck Type Ord
test typecheck StrictType Ord
test ord_recognizer ord_meta zero_ord                       = Ok true
test ord_recognizer ord_meta (succ_ord zero_ord)            = Ok true
test ord_recognizer ord_meta (omega_plus zero_ord)          = Ok true
test ord_recognizer ord_meta true                              = Ok false
```

== `String`

Sequences of characters. `String` is library-defined (a list of
character-tagged trees); shown here for completeness.

```disp
let string_recognizer := make_recognizer ({_, v} ->
  // v is a String iff v is a list whose elements are valid char tags
  Ok (and (is_list v) (list_all is_char_tag v)))

let string_meta := {
  recognizer_params := unit_witness,
  functor           := trivial_functor,
  respond           := inert_respond,    // String is inert under elimination (§12.3)
  behavioral_specs  := none
}

let String := wait string_recognizer string_meta

test typecheck Type String
test string_recognizer string_meta empty_string            = Ok true
test string_recognizer string_meta (cons (char "a") empty_string) = Ok true
```

(Full definitions live across the kernel fragments in `lib/kernel/` —
String in `base.disp`; the spec mandates the recognizer / meta shape
above and the tests below.)

== `Type` itself

`Type` is constructed identically to every other library type — a
wait-form pairing a recognizer with a MetaShape-conforming meta
record. The recognizer (§12, library function `type_recognizer`)
checks that a candidate value is a wait-form whose recognizer was
built via `make_recognizer` and whose meta has the MetaShape layout.
The meta carries no parameters (Type takes none), a trivial functor,
and the *predicate-side H-rule* in its `respond` slot.

```disp
// Predicate-side H-rule (see §6.3). Fires when a Type-typed hypothesis is
// applied as a predicate to a candidate `frame`: a verdict `Ok true` iff `frame`
// is itself a kernel-minted neutral whose stored type hash-cons-equals the
// applied hypothesis `self`. The dual of `make_recognizer`'s recognizer-side
// H-rule — needed because Type-hypotheses, when applied, route through
// `hyp_reduce`'s raw arm and never reach the `make_recognizer` wrapper. The
// 3-arg respond (§7.1) gets `self` directly; raw `is_neutral` / `neutral_type`
// are walker-safe because the respond runs inside `hyp_reduce`'s handler.
// `Return v` abbreviates the verdict `Reduce (Ok v)` (spec notation; the library spells it out).
type_predicate_h_rule := {params, self, frame} ->
  if (is_neutral frame)
    then (Return (tree_eq (neutral_type frame) self))
    else (Return false)

Type := wait (make_recognizer
  {m, v} -> if (is_fork v) then (Ok (has_metashape_layout (type_meta v))) else (Ok false)
) (make_meta unit_witness type_predicate_h_rule)   // Type takes no params; Type : Type (§ Type:Type)

test typecheck Type Type         // Type is a type (lax)

// Predicate-side H-rule tests (the load-bearing case for polymorphism).
let A_hyp  := make_hyp Type 0
let B_hyp  := make_hyp Type 1
let x_of_A := make_hyp A_hyp t

// Applying a Type-hyp routes through hyp_reduce's `Reduce` arm. The respond
// returns `Return b = Reduce (Ok b)`, and `Reduce` hands back its payload BARE
// (§7.1/§7.5), so the result is `Ok true` / `Ok false` — already a verdict, with no
// extra wrap by `hyp_reduce`.
test A_hyp x_of_A = Ok true     // matching-identity hypothesis: H-rule fires
test B_hyp x_of_A = Ok false     // distinct Type-hyp: tree_eq false
test A_hyp 0      = Ok false     // closed value: is_neutral false, then Return false
test A_hyp A_hyp  = Ok false     // routes through hyp_reduce (A_hyp is the head, not I):
                              //   is_neutral A_hyp = true, but stored_type = Type ≠ A_hyp
```

Four definitions. No tests fired at construction; `Type` is just a
value. The structural tests run `type_recognizer type_self_meta Type`;
Type's structure (wait-form with MetaShape-conforming metadata,
`respond` populated) satisfies the structural check, returns
`Ok true`, and the tests pass by construction. The H-rule tests
exercise the predicate-side path: applying a Type-hypothesis
through `hyp_reduce` and observing the respond's verdict.

The Type:Type concern and its (conjectural) resolution are discussed
in §11. The tests themselves run mechanically; their passing is an
empirical observation, while their implications for foundational
consistency remain open.

== `Σ` and `Π`: the four formers, one cut

Records, coproducts, functions, and dependent pairs are not four mechanisms but
the four cells of a single grid — `Σ` or `Π`, at a *finite* (extensional, data)
or *arbitrary* (intensional, code) domain:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [], [*finite domain — extensional (data, stored)*], [*arbitrary domain — intensional (code, computed)*],
    [*`Π` (product)*],
    [`Record` — graph stored as a `Σ`-tuple, accessed by lookup],
    [`Pi` — graph computed; `f x` = `checked_apply`],

    [*`Σ` (sum)*], [`Coproduct` — finite tag + payload], [`Sigma` — arbitrary tag + dependent payload],
  ),
  caption: [Every former is `Σ` or `Π`; the remaining axis is extensional vs intensional.],
)

A record *is* a finite `Pi`; a coproduct *is* a finite `Sigma`. There is one
introduction per polarity — inject a `Σ`-value (`inj`) or tabulate a `Π`-graph
(`prod`) — and one elimination, the *cut*: a `Σ`-value applied against a
`Π`-consumer over a shared index. `match` and projection (`.`) are its
finite-extensional instances, function application its arbitrary-intensional one;
the neutral-aware `elim` (§12.3) is the cut lifted over hypotheses.

The one axis that does *not* collapse is finite ↔ arbitrary. A finite product's
graph is a `Σ`-tuple that can be stored and hash-cons-compared; an arbitrary
`Pi`'s graph must be computed and is not a finite datum. That is the data/codata
(positive/negative) boundary — the same residue `strip` (§10) bottoms out at, and
the reason `Pi` shares records' shape and type-former (`Π`) without being a
finite table. `Σ` and `Π`, finite or arbitrary, eliminated by the cut: that is
the whole of the value layer.

== The primitive floor

The grid above places every value former on the `Σ`/`Π` axes, but two of its
corners are also where the grid *bottoms out*. `Bool`, `Nat`, and `Unit` — with
the interpreter's own `Result` (§3.4) and `Action` (§7) — are conceptually `Σ`/`Π`
(`Bool` is `True | False`, `Nat` is `Zero | Succ`, `Unit` is a single-inhabitant
type — the canonical empty *tuple* value `t t t`, *not* the empty record *type*
`{}`, which is `⊤` = `Tree`, §12.7),
but they are *not* `Coproduct`/`Record` instances. They are the bootstrap floor
the formers are built on, and the dependency runs one way only:

- `coproduct_recognizer` (§12.2) branches with `match` / `is_some` / `Ok false` —
  that is `Bool`.
- the cut's accessor `acc name = inj name unit` (§2.6) needs the `Unit` value.
- `proj`'s `index_of` / `path_at` count positions — that is `Nat`.

Defining the primitives *through* the formers would therefore be circular. `Bool`
is the sharpest case: it *is* the substrate's branch (`select` / `triage`), so it
stays primitive — hand-built raw shapes `true = △`, `false = △ △` (§2.7;
Scott-encoded until 2026-07-07), branded `Bool` by the view iso in its meta
exactly like `Nat`'s zero/succ. A bare `Coproduct [(true, Unit), (false, Unit)]`
recognizer would still reject the actual values — a raw shape carries no tag in
`pair_fst` to match against — which is exactly what the view bridges. The
hand-built minimal-tag enums `Result` and `Action` are primitive for the same
reason (§2.6).

What the floor *shares* with the derived formers is the elimination, not the
encoding. A neutral `Bool` and a neutral `Coproduct [(true, Unit), (false, Unit)]`
both respond through `inductive_respond` (§12.3), and the concrete branch of
`elim` abstracts the encoding gap — the functor view decodes a shape-encoded
value to its tagged form in front of the cut. So the unification is real but lives at the level of *the cut's
semantics and the `Σ`/`Π` grid*, not at the level of definitions: minimizing
primitives (the metacircular goal) leaves an irreducible floor — the tree-calculus
substrate plus this handful of encodings — rather than eliminating it.

== Capability records (a dependency-injection pattern over `Eff`)

The canonical effect system is the `Eff R X` free monad of §15. *In
addition*, a function may bundle a set of operations into a record passed
as an explicit argument — a *capability*, in the Effekt style — which is
ordinary `Record`/`Pi`/`Refinement` machinery (§12.1–§12.5), no effect-
specific types. This is dependency injection, not a second effect system:
a capability's operation fields *return `Eff` values* (they describe
effects), and the result is still run by a handler / the driver (§15).

```disp
// A capability is just a record type; its fields produce Eff values.
let Console : Type := record {
  print    : Pi String ({_} -> Eff {IO} Unit),
  readline : Pi Unit   ({_} -> Eff {IO} String)
}

// A "handler" here is a record value supplying those operations. A mock
// supplies pure Eff values, so test code needs no driver at all.
let realConsole : Console := { print := write stdout, readline := {_} -> read stdin }
let mockConsole : Console := { print := {_} -> io_pure unit, readline := {_} -> io_pure "stub" }

// A function using the capability takes it as a Pi-bound argument:
let greet : Pi Console ({_} -> Pi String ({_} -> Eff {IO} Unit)) :=
  checked Console ({c, name} -> c.print (concat "hello, " name))

test typecheck Type Console
test typecheck Console mockConsole
```

Bundling several capabilities is a product (`record { c : Console, f :
FileIO }`); "row-polymorphic over capabilities" is `Pi` over an
`extends R Console` refinement (§12.5) — all from the record/refinement
machinery, distinct from the §15 *effect row* `R`, which tracks the
operations an `Eff` value may perform. Capability-passing answers "where
do the operations come from"; the effect row answers "which effects may
fire" — they compose, and neither requires kernel support. See §15 for
the effect system proper.

= Cubical extensions <sec:cubical>

*(Status note, 2026-07-05: the set-level fragment of this section's
motivation landed as library code — `lib/std/oeq.disp`: a type's
equivalence is a setoid derived from its cells by `setoid_of`
(pointwise at Pi = funext by definition, componentwise at non-dependent
records, `Eq` at data), overridable through `behavioral_specs` =
quotients, with `LinkedPi` as the relational binder. Deliberately NO
elimination of the equivalence into arbitrary motives — funext-with-J
is inconsistent against `tree_eq` — so the coercion half of this
section (per-type morphism actions computing transport) remains the
planned route for dependent families.)*

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
`true`. This is representation-independence as a computational fact rather
than a manually-discharged proof obligation.

== The interval `I`

`I` is a library type whose elements are formulas in
the free De Morgan algebra:

```disp
// In disp source, the stem(x) constructor is written `t x`.
// Tags are distinct stems-of-leaf chains.
let tag_zero := t t                     // stem(LEAF):           0
let tag_one  := t (t t)                 // stem(stem(LEAF)):     1
let tag_and  := t (t (t t))             // stem^3(LEAF):         ∧
let tag_or   := t (t (t (t t)))         // stem^4(LEAF):         ∨
let tag_inv  := t (t (t (t (t t))))     // stem^5(LEAF):         ¬

let I_zero := pair tag_zero t
let I_one  := pair tag_one  t
let I_and  := {a, b} -> pair tag_and (pair a b)
let I_or   := {a, b} -> pair tag_or  (pair a b)
let I_inv  := {a}    -> pair tag_inv a

let I_recognizer := {_, v} -> ...
  // v is in I iff it's I_zero, I_one, a tagged operation, or a neutral
  // of stored type I. Library smart constructors normalize formulas to
  // DNF so De-Morgan-equivalent formulas hash-cons to identical trees.

let I_meta := {
  recognizer_params := unit_witness,
  functor := trivial_functor,         // I doesn't transport
  respond := inert_respond,           // the interval is inert under elimination (§12.3)
  behavioral_specs := none
}

let I := wait I_recognizer I_meta

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
let IsOne := {i} -> wait isone_recognizer {
  recognizer_params := i,
  functor := trivial_functor,
  respond := inert_respond,
  behavioral_specs := none
}

test typecheck Type IsOne

Partial := {phi, A} -> Pi (IsOne phi) ({_} -> A)

// The empty partial element (used when phi = I_zero):
let empty_partial := {A} -> {_proof_false} -> /* unreachable */
```

(The full `Partial` / face-system design is the standard CCHM one
(§13.5; CCHM, §17); the recognizer enforces "phi = I_one or phi has the
canonical disjunction shape.")

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
  let T0 := apply P I_zero
  let action := meta_get (type_meta T0) "functor"
  match (tree_eq action trivial_functor) {
    true => u0                              // discrete: trivial Kan structure
    false => apply action self P phi u u0    // real functor handles family + cofibration
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
(This replaces the earlier `trivial_functor → make_hyp`, which wrongly
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
let pair_functor  := {self, P, phi, u, u0} ->
  pair (self (P_fst P) phi (u_fst u) (pair_fst u0))
       (self (P_snd P) phi (u_snd u) (pair_snd u0))

// Sigma: dependent second component via the first component's
// trajectory. The B(a) family is reconstructed at each interval point
// by transporting a along P_fst.
let sigma_functor := {self, P, phi, u, u0} -> /* CCHM Sigma rule */ ...

// Pi: contravariant in A, covariant in B; threads the a-trajectory
// through B's family. Body application happens at the source endpoint;
// result is transported forward.
let pi_functor    := {self, P, phi, u, u0} -> /* CCHM Pi rule */ ...

// Eq: refl at the new endpoints. The path-of-refls case is structural.
let eq_functor    := {self, P, phi, u, u0} -> /* CCHM Eq rule */ ...
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
let Glue := {B, T, e} -> wait glue_recognizer {
  recognizer_params := glue_params_for B T e,
  functor := glue_functor,             // applies the equivalence on transport
  respond := inert_respond,            // Glue is eliminated via unglue, not a respond frame
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
+ *No kernel growth.* The two Σ-operations plus the dispatcher are
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
    [The two kernel handlers behave per their specs (§7).
      Test that `bind_hyp` mints a neutral and `hyp_reduce` extends
      spines / mints make_hyp via inductive types' `respond`. There is
      no host dependence to exclude — checking never performs an effect
      (§15), so these are ordinary `test`s.],

    [Type-system tests],
    [Each library type passes the expected validators. Test that
      `Type Bool`, `StrictType Bool`, `Type Pi`, etc. all reduce
      to `Ok true`.],

    [Behavioral tests],
    [Optional Path-typed proofs in `behavioral_specs` (including the
      respond-coherence laws, §12.3). Test that each type's recognizer
      and `respond` behave as documented on canonical inhabitants and
      counter-examples.],

    [Environment probes],
    [*Empirical* observations about the surrounding host
      environment — file existence, env vars, time-of-day,
      network reachability. These run the relevant `Eff` value through
      the *real driver* (§15), so they are meaningful only at the
      CLI/integration layer, *not* in unit tests; *not foundational*;
      may produce different results in different environments.],
  ),
  caption: [Test categories asserting soundness.],
)

The first three never perform an effect and underwrite the foundational
story directly. Environment probes are an *operational* category — they
fail when the host environment doesn't match expectations, not when the
type system is unsound — and are the only category that engages the real
driver; mark them with `test_observe` for clarity (a sugar that runs the
`Eff` value through the driver and reports failures distinctly from
foundational failures).

== Sample tests

```disp
// Kernel tests
test bind_hyp_mints_kernel_signature_neutral
test hyp_reduce_extends_spine_correctly
test hyp_reduce_mints_stuck_elim_via_inductive_respond
test param_apply_routes_kernel_handlers_raw

// Type-system tests (lax)
test typecheck Type Bool = true
test typecheck Type Nat = true
test typecheck Type Pi = true
test typecheck Type Sigma = true
test typecheck Type Eq = true
test typecheck Type Type = true

// Type-system tests (strict)
test typecheck StrictType Bool = true
test typecheck StrictType Pi = true
test typecheck StrictType Type = true

// Behavioral tests (sample)
test bool_recognizer unit_witness true = Ok true
test bool_recognizer unit_witness false = Ok true
test nat_recognizer unit_witness zero = Ok true
test pi_recognizer (pi_meta_for Nat ({_} -> Bool)) is_zero = Ok true
```

The standard library ships these tests inline with the relevant
definitions. The full catalog is browsable as source and runnable as
a whole by re-elaborating the library.

== The trust boundary

What's in the trusted base:

- The two kernel Σ-operations (`hyp_reduce`, `bind_hyp`) implemented in
  disp source.
- The dispatcher `param_apply` (in-language reference + native fast-path).
- The host runtime's signature-pinning and native fast-paths for the
  above.
- The parametric walker (in-language reference + native fast-path).
- Library validators (`Type`, `StrictType`, `BehavioralType`) and
  their recognizer functions.
- Library `safe_*` helpers (hypothesis-safe structural inspection).
- The `MetaShape` convention.
- The *effect driver* (§15) and each host operation's *type ascription*
  in the host-op map — the library claim about what the underlying host
  function implements (library-level trust; see §15).

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

*Foundational tests are automatically host-independent.* The
conjectural-consistency argument needs Σ to be the kernel ops only — and
that now holds *by construction*: Σ is the fixed two-op constant, host
primitives are inert `Eff` values never performed during checking (§15),
and `param_apply` carries no host environment. The earlier requirement
to mark foundational tests `test_pure` is obsolete; every `test` already
runs in the one host-independent mode.

Two distinct claims to keep separate:

+ *The standard library passes its standard tests.* Operational;
  empirically verified at every elaboration. Errure indicates an
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
when validation starts, return the optimistic answer (`Ok true`) for
recursive queries during the in-progress window, and finalize when
the outer validation completes:

```
typecheck T v:
  case memo[T, v] of
    Some (InProgress) => Ok true       // optimistic; outer validation
                                     // will catch real failures
    Some (Final r)    => r           // memoized result
    None              =>
      memo[T, v] := InProgress
      let r := ... actual validation ...
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

#openq[
  *Is the optimistic in-progress answer sound, not just terminating?*
  The memo returns `Ok true` for a recursive query that is still
  in-progress, justified as "the outer validation will catch real
  failures." That guarantees *termination*, but its *soundness*
  rests on an unstated assumption: that the recursion is *productive*
  — every cycle must pass through enough non-recursive structural
  obligation (is it a wait-form? does the meta carry the required
  fields?) that no ill-formed self-referential type can validate
  *only* on the strength of the optimistic answer it hands itself.
  If a type passed its structural checks but its deep field-types
  were wrong in a way observable *only* through the recursive query,
  the optimism could accept it (the coinductive "guarded vs. unsound
  circular reasoning" hazard). The §11/§12 self-referential types
  (`Type`, `Pi`, `Sigma`, `MetaShape`, `RecognizerShape`) appear
  productive — their structural layer is checked eagerly and only the
  field-by-field deep typing recurses — but this has not been proven.
  A guardedness/productivity criterion on `StrictType`'s recursive
  structure would settle it; until then the optimism is a
  *conjectured-sound* termination device, not a proven-sound one.
]

#openq[Empirical verification of fix-based self-reference hash-cons
  stability (`archive/RECORDS_PROPOSAL.md` §9 step 2) is also load-bearing for
  H-rule reconstruction. If `wait self.handler meta` (constructed
  inside a handler body) doesn't hash-cons-equal `wait kernel.handler
meta` (constructed externally), tree_eq comparisons in the H-rule
  fail and validation behaves unpredictably. This needs an explicit
  test in the implementation.]

= Effects <sec:effects>

*(Status note, 2026-07-05: the elaborator's declaration layer is
designed as this section's first consumer — a module is a value of the
free monad over the elaboration signature (`Declare`/`Test`/`Import`),
interpreted by the driver, with per-name *guards* as deep handlers over
`Declare` (the declaration protocol: SYNTAX.typ § record members, `cut.disp`). Once `Eff`
exists as a library type, `mod : Eff ElabSig Unit` becomes an ordinary
checkable annotation and guard-respect a property of module values.)*

Disp's effect system is *entirely a library construction* — a free monad
over a signature of operations, interpreted by handlers, with one impure
*driver* at the program boundary. The kernel is not involved: there is no
effect Σ-entry, no `postulate`, no host-rooted wait-form. This section
specifies it.

== Why effects are values

The substrate must be pure: hash-consing shares structurally-equal trees,
and the `apply`-memo that makes `tree_eq` O(1) (§2.2) is only sound if
reduction has no side effects. A side-effecting `apply` would make two
equal subtrees — one node — perform their effect a number of times that
depends on sharing, which is incoherent. So an effect *cannot happen
during reduction*. It follows that an effect operation must be a *value*
— a description — and real IO happens only at the impure boundary, when
the outermost *driver* walks that value. This is forced, not chosen, and
it is why the design is a free monad plus a driver rather than a
dispatch-environment of side-effecting handlers (the framing of the prior
spec).

== `Eff R X`: the free monad

An effectful computation is a value of `Eff R X` — "a computation that may
perform the effects in row `R`, ultimately yielding an `X`." It is an
ordinary library `Coproduct` (§12.2), hence recursive like `List`, and it
inherits `inductive_respond` (§12.3) so an `Eff`-typed *hypothesis* is
handled by the kernel's neutral machinery automatically — the only point
at which effects touch the kernel at all.

```disp
// A self-describing operation occurrence: its label and its dependent
// param / result types, carried inline (so `op.result` is a projection,
// not a global-table lookup — §15.3).
OpOcc := { label : OpLabel, param : Type, result : Type }

// The free monad. R (the effect row, §15.5) is a type-level parameter;
// the value shape is just Pure | Op.
Eff := {R, X} -> Coproduct [
  (Pure, X),                                          // done — payload is the result
  (Op,   Sigma OpOcc ({op} ->                         // perform op `op` …
           Sigma (op.param) ({arg} ->                 //   on `arg : op.param` …
             Pi (op.result) ({_} -> Eff R X))))       //   then continue: k : op.result -> Eff R X
]

io_pure := {x}           -> inj Pure x                          // = pair Pure x   (§2.6)
io_op   := {op, arg, k}  -> inj Op (pair op (pair arg k))       // = pair Op (op,arg,k)
```

The continuation `k` is an ordinary function-tree awaiting the op's result
(§15.3 makes the result type dependent: `op.result`). `bind` is the
standard free-monad graft — walk to the `Pure` leaf, attach `f`:

```disp
bind := fix ({self, m, f} ->
  match m {                                       // the cut on m's tag (§2.6)
    Pure x => f x
    Op p   => io_op (p.op) (p.arg) ({r} -> self (p.k r) f)
  })
```

== Operations and user-definable effects

Every operation — host or user — is built by one generator that makes it
self-describing (this is the dependent-result resolution: `op.result` is
read off the occurrence, no global `OpResult` table):

```disp
// `declare_op` is what an `effect` declaration expands to, per operation.
declare_op := {label, ParamTy, ResultTy} ->
  {arg} -> io_op { label := label, param := ParamTy, result := ResultTy }
                 arg
                 ({r} -> io_pure r)        // trivial "return the result" continuation
```

The built-in `IO` effect's operations are nothing special — just the
operations of one effect whose handler happens to be the driver:

```disp
read  := declare_op IO.read  Fd                 String     // read  : Fd -> Eff {IO} String
write := {fd, s} ->
  (declare_op IO.write (pair Fd String) Unit) (pair fd s)  // write : … -> Eff {IO} Unit
```

A *user effect declaration* desugars to a label, a `declare_op` per
operation, and a row entry — all library sugar, no privilege:

```disp
// surface:                          elaborates to:
effect State S {                     State    := effect_label "State"
  get : Unit -> S                    get      := declare_op (op State "get") Unit S
  put : S    -> Unit                 put := {s} -> (declare_op (op State "put") S Unit) s
}
```

== Handlers

A handler interprets a free-monad value as a *deep fold*, with the
continuation reified as `resume`. On an operation it covers, it runs the
clause; on one it does not, it *forwards* the op and stays installed on
the tail:

```disp
handle := {h} -> fix ({self, m} ->
  match m {
    Pure x => h.return x
    Op p   => match (h.covers (p.op)) {
      true => (h.on (p.op)) (p.arg) ({r} -> self (p.k r))   // interpret; resume = run the tail under h
      false => io_op (p.op) (p.arg) ({r} -> self (p.k r))    // forward; keep h installed on the tail
    }
  })
```

`resume = {r} -> self (p.k r)` *is* the continuation. The clause decides
how often to call it; because the substrate is pure (§15.1), calling it
0/1/many times is free:

```disp
exn_handler := { covers := is_Exn,                       // throw aborts: resume unused
                 return := {x} -> Ok x,
                 on := {_op} -> {e, resume} -> Err e }

nondet_hdlr := { covers := is_Nondet,                    // choose: resume per candidate (multi-shot)
                 return := {x} -> [x],
                 on := {_op} -> {cands, resume} -> concat_map resume cands }

state_hdlr  := { covers := is_State,                     // parameter-passing: clauses return S -> …
                 return := {x} -> {s} -> io_pure (pair x s),
                 on := {op} -> select_op op {
                   get := {_, resume} -> {s} -> (resume s)    s,
                   put := {_, resume} -> {s'} -> (resume unit) s' } }
```

== The effect row and effect-safety

The row `R` in `Eff R X` is a *set of effect labels* carried in the type's
`recognizer_params`. Effect-safety is ordinary recognizer + `Pi` checking
— no new machinery. This is the research literature's *graded monad* `T_e`
with `e` ranging over the idempotent powerset-of-labels monoid (union, ∅):
Koka-style row effects, realized as a parameterized library inductive plus
dependent `Pi`-types.

=== Representation

An *effect label* is an interned effect name — a deterministic tree per
name (so `effect_label "State"` is always the same tree), totally ordered
by `label_compare` = the name order (a *structural*, hence
run-deterministic, comparison; *not* hash-cons id, which is not stable
across runs). A *row* value is then:

- a *name-sorted, duplicate-free list* of effect labels (closed row), or
- that list improper-tailed by a single *row variable* — a `Row`-typed
  hypothesis (`bind_hyp Row`) — standing for "the rest of the effects"
  (open row, for polymorphism).

`Row : Type` recognizes exactly these. Canonicity (sorted + deduped) makes
two closed rows the *same tree* iff they are the same set, so type
conversion `Eff R X ≡ Eff R' X` is the kernel's O(1) `tree_eq` — `{A,B}`
and `{B,A}` are one tree. The row operations are library functions that
*reduce to canonical form on concrete inputs* and *stay stuck*
(hash-cons-consistently) on a neutral tail:

```disp
row_nil    := nil                                   // {} — the empty row
row_singleton := {E}    -> [E]
row_cons   := {E, R}    -> sorted_insert_dedup E R  // canonical on concrete R; stuck if R is a row-var
row_union  := {R, S}    -> sorted_merge_dedup R S   // ditto
row_remove := {E, R}    -> filter ({l} -> not (label_eq l E)) R
label_in_row := {e, R}  -> list_mem e R             // (over the concrete prefix; see below)
```

=== Weakening is free — there is no subtyping judgment

The `Eff` recognizer checks one thing: every `Op`'s effect-label lies in
`R` (`Pure` is unconstrained). In other words, it decides `support(v) ⊆ R`
— the set of effects `v` may perform is *contained in* the permitted row:

```disp
eff_recognizer := make_recognizer ({meta, v} ->
  let R := meta.recognizer_params.row in
  let X := meta.recognizer_params.result in
  match v {                                   // (concrete v; H-rule handles neutral v)
    Pure x => param_apply X x                              // payload inhabits X; no row constraint
    Op p   => and (label_in_row (effect_of p.op) R)        // this op's effect ∈ R
                  (param_apply (Pi (p.op.result) ({_} -> Eff R X)) (p.k))  // k continues in Eff R X
  })
```

Because membership is a *containment* check, *effect weakening falls out
definitionally* — no subtyping rule, no coercion. `m : Eff S X` inhabits
`Eff R X` for every `R ⊇ S` (its ops are still all in `R`), and `pure : Eff
{} X` inhabits *every* row. Where Koka needs row subsumption/unification,
disp gets it because *a type is a predicate that checks `⊆`, not an index
that must match `=`*. (Handling an effect a computation never uses is the
same fact: `m : Eff ρ X` inhabits `Eff (row_cons E ρ) X`.)

=== Polymorphism is `Pi Row`; the row arithmetic rides the combinator types

A row-polymorphic combinator quantifies over a row with an ordinary
`Pi Row`, and the bound `ρ` is a `Row`-hypothesis like any other:

```
op_E    : …                                              -> Eff (row_singleton E) _
bind    : Pi Row ρ. Pi Row σ. Eff ρ A -> (A -> Eff σ B)  -> Eff (row_union ρ σ) B
handle_E: Pi Row ρ.            Eff (row_cons E ρ) X -> Handler E -> Eff ρ Y
main    :                                                   Eff (row_singleton IO) Unit
```

`bind` joins rows (`∪`, the graded μ); `handle_E` discharges `E` (removes
it). These typecheck *generically* with no new mechanism: checking such a
type applies the combinator to a *neutral* `Eff`-value, and `bind`/`handle`
are `elim`-gated (§12.3) — on a concrete target they run the fold, on a
neutral target they produce a respond-mediated stuck value whose stored
type is the codomain row (`Eff (row_union ρ σ) B`, `Eff ρ Y`), which the
recognizer accepts by the H-rule (O(1) `tree_eq` against that stored type).
A row variable only ever appears as the tail of a stuck `row_*` expression
during such a body-check; on a concrete instantiation (`ρ := {IO}`) the row
op reduces to a canonical closed row. A computation reaching `main` must
have row `⊆ {IO}` — only the driver-handled effect may remain — or it is a
type error (a missing handler).

#note[
  *Limitation: open-row equality is syntactic.* `tree_eq` compares a stuck
  `row_union ρ S` by its tree, not semantically up-to-reordering, so the
  convergent case is a *single* trailing row variable with concrete labels
  added in canonical order — which covers every standard handler /
  row-polymorphism pattern. Unioning two *distinct* row variables yields a
  stuck `row_union ρ σ` that will not match a reassociated `row_union σ ρ`;
  and full *inference* of rows (rather than ascription) stays open
  (Appendix A). This is the deliberate cost of "conversion is `tree_eq`"
  instead of a row-unification engine — and the right trade for disp:
  closed rows stay O(1), and the one-variable case (the common one) is
  exact.
]

== The driver: the one impure handler

The built-in `IO` effect is discharged by the *driver* — the outermost
handler, and the only impure code in the system, run at `main`:

```
run(m):                                   // run : Eff {IO} X -> X, native, at the boundary
  while tag(m) == Op && is_IO(m.op):
    arg = m.arg
    require (is_closed arg)               // §8.1 sanitizer — see below
    result = perform_host(m.op, arg)      // the syscall; the host-op map
    m = apply(m.k, result)                // resume the continuation with the concrete result
  return payload(m)                       // Pure x → x  (all IO discharged)
```

`perform_host` is the host-op map (`OpLabel → TS function`) — an
*interpretation* table consulted only here, never a protection registry,
never seen by the walker. An unknown label is an error (no handler).
*Async* needs no new mechanism: the driver simply *defers* `apply(m.k,
result)` until the bytes arrive — `m.k` is the suspended continuation —
so an event-loop driver schedules resumptions without any kernel `Promise`
primitive.

#note[
  *The sanitizer, and where `postulate`'s scan went.* The `is_closed`
  check (§8.1) is the residue of the prior spec's `postulate`
  no-neutral scan. It belongs *here*, at the driver, not at the
  smart constructors or in a type: during type-checking an op's `arg`
  is legitimately symbolic (checking `{s : String} -> write fd s`
  builds `write fd s` with `s` a hypothesis), and the `String`
  recognizer *accepts* a `String`-typed neutral via the H-rule — so
  neither construction nor the type can forbid a neutral arg. Only at
  run time is the program closed (`main` has no free hypotheses), so
  the driver is the unique sound place to require that the host never
  receives a symbolic tree. On a validated closed program it never
  fires; it is a cheap (memoized) defensive backstop.
]

== Worked example: a user effect over IO

```disp
prog : Eff {State Nat, IO} Unit
prog := bind get               ({n} ->
        bind (write stdout n)  ({_} ->
        put (succ n)))
// value: Op get () (\n -> Op write (stdout,n) (\_ -> Op put (n+1) (\_ -> Pure ())))

main : Eff {IO} Unit
main := map ({_} -> unit) (handle state_hdlr prog 0)
// handle state_hdlr interprets get/put (threading state), FORWARDS write;
// residual row {State Nat, IO} − {State Nat} = {IO}; the driver then runs write.
```

`handle state_hdlr` folds `prog`: `get`/`put` are interpreted (state
threaded as a parameter), `write` is forwarded unchanged into the residual
`Eff {IO}` value, which the driver performs. Multi-shot, abort, and state
all come from the *same* fold; the only difference is how each clause uses
`resume`.

== `catch` is a handler, not an operation

Scoped operations whose semantics are fixed at the use site — `catch`,
`local` (Reader), `once`, `bracket` — are *handlers*, not higher-order
operations. `catch` is a locally-installed `Exn` handler whose recovery
clause drops the continuation:

```disp
// catch : Eff (R ∪ {Exn E}) A -> (E -> Eff R A) -> Eff R A
catch := {body, recover} -> handle {
  covers := is_Exn,
  return := {x} -> io_pure x,                 // body finished normally: keep its value
  on     := {_op} -> {e, resume} -> recover e // a throw inside body: run recover, DROP resume
} body
```

The scoping is correct because `handle h body` folds *only* `body` — the
delimited region — while throws *after* `catch` live in the outer
continuation `handle` never touches. It composes with other effects: in
`catch (do { put 2; throw e; put 3 }) (\_ -> get)`, the local `Exn`
handler *forwards* `put 2` (so the outer `State` handler sees it), the
`throw` triggers recovery (dropping `put 3`), and `State` threads straight
through the catch boundary. This is the standard algebraic-effects
treatment, and disp gets delimited control for free: `handle` is `reset`,
`resume` is the captured delimited continuation, so no `shift`/`reset`
primitive is needed.

#openq[
  *Higher-order operations that must be reinterpreted by outer handlers*
  (the Wu–Schrijvers–Hinze 2014 "effect handlers in scope" problem) are
  the residue the handler-encoding does *not* cover: a scoped operation
  whose meaning is determined by an *outer* handler it is forwarded
  through (a transactional or logging handler that must observe the
  scope). The flat free monad fails here because a handler's fold walks
  the `k`-spine but never recurses into a computation sitting in argument
  position. The minimal fix is a second constructor
  `Scoped { op, body : Eff R B, k }` whose sub-computations the fold
  enters on forward (`body := self body`), plus a `weave`/`thread`
  function on handlers to push handler *state* through a forwarded
  sub-computation. This is additive (one inductive arm, one fold case),
  reuses `inductive_respond`, and leaves the driver untouched (IO
  operations are all algebraic). It is deferred as a v2 extension; the
  clean modern treatment is Bach Poulsen & van der Rest, *Hefty
  Algebras* (POPL 2023), which elaborates higher-order syntax into
  algebraic operations before handling.
]

== Pure regions and testing

Because effects are *values* and the type checker never performs them, all
type-checking is inherently host-independent — there is no `test_pure`
versus default distinction to maintain (the prior spec's `safe_apply
kernel_handlers` "pure region" is now the *only* mode, since `param_apply`
no longer carries a host environment). To run effectful code without real
IO, supply a *test handler* instead of the driver: `handle mock_console
prog` interprets `IO` operations in-language and returns a pure value, so
`test (handle mock_console prog) = …` needs no host at all.

== What this gives, what it gives up

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Capability*], [*Mechanism*],
    [Custom user-definable effects], [`effect` sugar → `declare_op`; a label + a row entry],
    [Effect typing / safety], [The row `R` in `recognizer_params`, checked by the `Eff` recognizer + `Pi` types],
    [Row composition], [`bind` unions rows (`∪`); `handle_E` discharges (`−`)],
    [Multi-shot continuations], [`resume` is a pure tree — call 0/1/many times],
    [`catch` / `local` / scoped recovery], [Locally-installed handlers (`handle` = `reset`)],
    [Test mocking], [Supply a test handler in place of the driver],
    [Async], [Driver defers `apply k`; no kernel primitive],
    [No kernel surface], [`Eff` is a library `Coproduct`; only the driver is impure],
  ),
  caption: [What disp's effects machinery delivers.],
)

What it gives up / leaves open:

- *Higher-order operations reinterpretable by outer handlers* — the
  `Scoped` + weave extension above (deferred).
- *Tail-resumption efficiency.* A handler that calls `resume` once in
  tail position naively rebuilds the continuation each step (O(n²) over
  n operations); production systems special-case tail-resumptive
  handlers. The free-monad encoding has this cost and the known fix.
- *Elaborator inference of rows.* Rows are tracked and checked, but
  inferring them (rather than ascribing) is future work.

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

== Effects as library values, forced by the substrate

In Koka and Eff, handlers are built into the evaluation semantics; in
OCaml 5 effects are an unchecked runtime primitive; in Lean 4 IO is
baked into the metatheory. Disp's effect system (§15) is *entirely
library code* — a free monad `Eff R X`, deep handlers, and row tracking
via ordinary recognizers and `Pi`-types — with the kernel doing nothing
effect-specific. What forces this factoring is the substrate itself:
hash-consing requires `apply` to be pure (a side-effecting `apply` would
break `tree_eq`-is-O(1)), so an effect *cannot* be a reduction step and
*must* be a value performed at the boundary by an outermost driver. The
kernel stays a pure type-checker; the only privileged constructs are the
two type-system operations that mint neutrals. That a dependently-typed
language can host a full row-typed algebraic-effect system with *zero*
effect surface in its kernel — because purity leaves no other option — is,
to our knowledge, novel.

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

== Input-checked functions

`checked A f` is a function paired with its declared *input* (domain)
type: applying it checks the argument against `A` before running `f`.
This is the Findler-Felleisen higher-order contract, and it is the only
typed-value wrapper disp needs — there is no separate certificate
construct. "This value was validated against `T`" is recorded as the
*verdict* `typecheck T v = Ok true`, not as a wrapper around `v`; `strip`
(§10) then erases the `checked` input-guards a validated program no
longer needs. (`checked` is a library function — its body is walker-safe,
see §12.16.)

== One cut for projection, match, and application

Most languages give records, sums, and functions three distinct eliminators
(field access, `case`, application). Disp has one. Every value is
`fork(descriptor, payload)`; products and coproducts share the `prod`/`inj`
encoding of §2.6, and a single operation — the *cut*, a `Σ`-value applied against
a `Π`-consumer over a shared index — drives all three eliminations. Field access,
`match`, and function application differ only along the finite↔arbitrary and
const-field↔real-handler axes (§12). This collapses the value layer to two
introductions (inject, tabulate) and one elimination, and it makes a value freely
presentable as data to inspect or as code to run — the property a metacircular,
self-optimizing language most needs. The polarized reading (products negative,
coproducts positive, the cut their interaction) is standard; carrying it all the
way down to one substrate combinator is disp's.

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

*Disp's effect system* (§15) is a *free monad over an operation
signature, interpreted by deep handlers* — the Plotkin-Pretnar / Eff /
Koka lineage — realized entirely in the library, with the substrate's
purity forcing effects to be values performed by an outermost driver.
Row-typed effect tracking follows Koka (Leijen); the deep-handler fold
follows Eff / Frank. The *capability-passing* layer (§12.7) — bundling
operations into a record passed as an argument — is Effekt's:
Brachthäuser, Schuster, Ostermann (2020), "Effects as Capabilities"
(OOPSLA, DOI 10.1145/3428194); but in disp it is an optional
dependency-injection pattern over the free monad (a capability's fields
*return* `Eff` values), not the effect system itself. Disp differs from
all of these in not threading effects through the kernel at all: the
dispatcher routes only the two type-system operations, and effects never
appear in the dispatch set Σ.

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

== Polarity, focusing, and data abstraction

The one-cut picture (§2.6, §12) is the polarized / call-by-push-value reading of
data. Coproducts are positive (value-determined, one dynamic index survives
erasure), products negative (type-determined, fully static), and the cut is the
sequent-calculus cut — `Σ`-value against `Π`-consumer. This is Levy's
call-by-push-value (2001) and the focusing tradition of Andreoli (1992),
Zeilberger (2008), and Munch-Maccagnoni (2013); disp's `strip` residue (§10) is
the operational content of that polarity. The two faces of a record — readable
data and callable behavior — are Cook's ADT-vs-object duality (Cook 2009),
itself a sharpening of Reynolds's "user-defined types and procedural data
structures" (1975). Disp's contribution is to realize both faces in *one*
substrate value (the `prod`/`inj` shape), sliding between them by which
capability the descriptor grants.

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
  Effects are a library free monad with deep handlers in the
  Plotkin-Pretnar / Eff / Koka lineage (the kernel is not involved),
  with capability records (Effekt, Brachthäuser et al. 2020) as an
  optional dependency-injection layer. Cubical operations follow the
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
    [Is `typecheck Type Type = Ok true` enough to imply foundational
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

    [§14 (memo optimism soundness)],
    [The in-progress memo returns optimistic `Ok true`, which
      guarantees termination but not soundness. Needs a
      guardedness/productivity criterion on `StrictType`'s recursion
      to rule out an ill-formed self-referential type validating
      only on the strength of its own optimistic answer.],

    [§15 (higher-order operations)],
    [Operations taking *computation* arguments that must be reinterpreted
      by an outer handler (`catch` as a forwardable op, `fork`) need the
      `Scoped` constructor + handler weaving (Wu-Schrijvers-Hinze; the
      hefty-algebra treatment). The flat free monad handles algebraic ops
      and use-site-scoped `catch`-as-handler; the rest is the deferred v2
      extension noted in §15.],

    [§15 (tail-resumption efficiency)],
    [A handler calling `resume` once in tail position naively rebuilds the
      continuation each step (O(n²)); production systems special-case
      tail-resumptive handlers. The free-monad encoding has this cost and
      the known fix; not yet specified.],

    [§15 (effect-row inference)],
    [Effect rows are tracked and *checked* (the `Eff` recognizer + `Pi`
      types) but must be *ascribed*; inferring them is future work.],
  ),
  caption: [Open questions inventory.],
)

The spec is *not* blocked on any of these — the system is operational
without them — but each represents an honest gap worth tracking. The
foundational ones (Type:Type, formal soundness) are the load-bearing
items; the others are scoped.

*Resolved since previous iteration:*
- User-installable effects: §15 specifies the design — a library free
  monad `Eff R X`, user `effect` declarations desugaring to `declare_op`,
  deep handlers, and an outermost driver. No kernel surface.
- Effects vs. the kernel: effects left the dispatch set entirely. Σ is
  the *fixed* two-op kernel constant (`postulate` and `host_provided` are
  gone); the substrate's purity forces effects to be values, not dispatch
  targets.
- Scoped recovery (`catch`, `local`): a locally-installed handler
  (`handle` = `reset`), not a primitive; composes with other effects.

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
`test/disp.test.ts`). Effect tests (`Eff` recognizer + row checks,
handler folds, `catch`-as-handler scoping, mock-handler runs) land in
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
- Reynolds (1975). "User-Defined Types and Procedural Data Structures as Complementary Approaches to Data Abstraction." In _New Directions in Algorithmic Languages_.
- Cook (2009). "On Understanding Data Abstraction, Revisited." OOPSLA. DOI 10.1145/1640089.1640133.
- Andreoli (1992). "Logic Programming with Focusing Proofs in Linear Logic." Journal of Logic and Computation 2(3).
- Levy (2001). "Call-by-Push-Value: A Functional/Imperative Synthesis." PhD thesis; Springer monograph (2004).
- Zeilberger (2008). "On the Unity of Duality." Annals of Pure and Applied Logic 153(1).
- Munch-Maccagnoni (2013). "Syntax and Models of a Non-Associative Composition of Programs and Proofs." PhD thesis, Université Paris Diderot.
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
- `archive/RECORDS_PROPOSAL.md` — records and projection (archived; record
  *theory* now in §2.6/§12, the file retains the forward-looking design for
  recursive records and the encoding migration).
#v(2em)

#note[
  *Document conventions.* Disp source code in this document follows the
  syntax established in `SYNTAX.typ`. Cross-references use `§N.M` for
  sections and code locations like `file:line` for source links.
  Open questions are marked `Open question:`. The document is designed
  for section-by-section iteration; revisions should preserve section
  numbering until a major restructuring.
]
