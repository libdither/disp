#set document(title: "Disp Categorical Foundations Proposal")
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

#align(center, text(22pt, weight: "bold")[Disp Categorical Foundations])
#v(0.3em)
#align(center)[
  A proposal to redesign type-former metadata as inhabitants of
  standard categorical structures, replacing positional tuple
  conventions with typed records grounded in topos theory.
]
#v(1em)

#note[
  *Status.* Design proposal. Argues that Disp's current type-former
  metadata layout (positional tuple of recognizer / codomain_fn /
  comp_fn) should be reformulated as a typed record bundling standard
  categorical structures: characteristic morphism into a subobject
  classifier (recognizer), eval morphism of a locally cartesian closed
  category (codomain_fn), and functorial action of an ∞-functor
  (comp_fn).

  This is independent of `CUBICAL_PROPOSAL.typ` but composes with it:
  the cubical proposal's `comp_fn` slot becomes the morphism-action
  field of a standard `Functor` record. Doing both together avoids
  refactoring metadata twice.

  Not a kernel change. The seven primitives remain seven. The proposal
  reshapes how library type-formers are *expressed*, not what the
  kernel *does*.
]

= Motivation

== The current state

Library type-formers in Disp are encoded as guarded `predicate_frame`
wait-forms with a positional-tuple metadata:

```
T = guard (wait kernel_ref.predicate_frame
  (pair recognizer_sig (pair params codomain_fn)))
```

The metadata layout is *convention*: slot 1 is the signature, slot 2
is the parameters, slot 3 is the codomain function. Library authors
fill slots positionally; the kernel reads positionally.

This works, but has three problems:

+ *No structural validation.* `Type`'s recognizer only checks "is this
  a guarded predicate_frame wait-form?" — nothing about the metadata's
  shape, the recognizer's arity, the codomain_fn's signature. Library
  authors can put garbage in any slot and `Type` accepts the result.

+ *Implicit categorical content.* The slot meanings encode
  well-known categorical structures — characteristic morphisms into a
  subobject classifier, eval morphisms of a closed category, functor
  actions — but these are nowhere named. Anyone reading the code has
  to *reverse-engineer* the categorical picture from the conventions.

+ *Friction adding new slots.* As cubical and future extensions add
  metadata (comp_fn, higher coherences, etc.), the positional layout
  has to grow. Each addition is a sweep across all library types and
  all tests asserting against metadata shapes.

== The categorical observation

Every slot in the metadata corresponds to a standard categorical
structure:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Slot*], [*What it does*], [*Standard categorical structure*],
    [`recognizer`],
      [Decides "does this tree inhabit T?"],
      [Characteristic morphism `Tree → Ω` into a subobject classifier
       (`Ω = Bool`); type T is the subobject classified],
    [`codomain_fn`],
      [Computes T's behavior under application],
      [Eval morphism of an LCCC (for Pi-like T) or subobject classifier
       `∈` (for predicate-like T)],
    [`comp_fn`],
      [Transports values along type-paths and fills cubes],
      [Functor's morphism action between ∞-groupoids],
  ),
  caption: [Disp slots map to standard categorical structures.],
)

These are not invented categorical roles — they're the *standard*
encoding of "membership," "application," and "functoriality" in
topos theory and ∞-category theory.

The walker's parametricity restriction is also categorical: it makes
Disp's setting a *parametric topos* (PER model, reflexive graph model,
or similar). Standard topos theory applies, with the parametricity
constraint refining *which* characteristic morphisms count as valid
types.

== The proposal

Reformulate type-former metadata as a typed record whose fields are
inhabitants of standard categorical structures (defined as library
types):

```
TypeFormer = Sigma SubobjectClassifier
             (\{sc\} -> Sigma (Optional Applicable)
                       (\{_\} -> Functor))
```

`Type`'s recognizer becomes a type-membership check: "is this a
guarded value whose contents inhabit `TypeFormer`?" Each library
type-former declares its fields, which are individually Pi-checked
against the standard structures.

This gives:

- *Structural validation by construction*: the type system enforces
  the metadata shape.
- *Categorical clarity*: the structure has explicit names tied to
  established mathematics.
- *Graceful extensibility*: adding new categorical operations means
  extending one of the standard structures, not adding ad-hoc slots.
- *Self-documenting*: anyone reading `TypeFormer`'s definition sees
  what categorical contract a type-former must satisfy.

= The categorical picture

This section makes precise the correspondence between Disp's slots
and standard categorical concepts. Each subsection covers one slot.

== Recognizer ≅ characteristic morphism in a parametric topos

In topos theory, a category has a *subobject classifier* `Ω` such
that every subobject of an object `X` corresponds bijectively to a
morphism `X → Ω`. The morphism is the *characteristic morphism* of
the subobject: `χ_S(x) = ⊤` iff `x ∈ S`.

In Disp:

- `Tree` is the universal object (every value is a tree).
- `Bool` is the subobject classifier `Ω`.
- A *type* is a subobject of `Tree`.
- The *recognizer* is the characteristic morphism `Tree → Bool`
  identifying which trees inhabit the type.

Parametric refinement: the walker restricts allowable recognizers to
those respecting parametricity (no triage on neutrals). This places
Disp's setting in a *parametric topos* (e.g., a PER model or
reflexive-graph model of type theory). Standard topos structure
applies, with this constraint on morphisms.

```
SubobjectClassifier : Type
SubobjectClassifier := record \{
  params                   : Type
  characteristic_morphism  : params -> Tree -> Bool
  -- Parametricity is enforced operationally by the walker;
  -- categorically, this is the constraint that places us in
  -- a parametric topos.
\}
```

#note[
  *Why this isn't Disp-specific.* The characteristic-morphism encoding
  of subobjects is the *standard* topos-theoretic foundation. Set
  theory's category `Set` has `Ω = \{⊤, ⊥\}` and subsets are functions
  to `Ω`. Disp's category of trees has `Ω = Bool` and types are
  parametric functions to `Bool`. Same construction, different base
  category.
]

== Codomain_fn ≅ eval morphism / subobject classifier ∈

Some types are *applicable*: applying a value of type T to an
argument yields something. Two distinct categorical patterns:

*Pi-like (LCCC eval):* for `Pi A B`, applying `f : Pi A B` to `a : A`
yields a value in `B(a)`. Categorically, this is the eval morphism of
a (locally) cartesian closed category:

```
eval : (A ⇒ B) × A -> B
```

generalized to dependent codomains via the LCCC structure (eval as a
pullback along the indexing morphism).

*Predicate-like (topos ∈):* for `Type`, applying `T : Type` to a value
`v` yields a Bool ("is `v` a member of T?"). This is the subobject
classifier's membership relation:

```
∈ : Sub(X) × X -> Ω
```

The Action protocol (`Extend new_type` for Pi-like, `Return value`
for predicate-like) is Disp's encoding mechanism for which eval is
operative; the *categorical content* is the same universal eval
morphism in both cases.

```
Applicable : Type
Applicable := record \{
  eval_morphism : EvalSignature   -- the categorical eval map
  -- Encoding via Action: tagged result distinguishing
  -- "spine extension" (LCCC pullback) from "direct value"
  -- (subobject classifier evaluation)
\}
```

Not all types are applicable. `Bool`, `Nat`, `False` aren't function
types and don't classify subobjects. For these, the `Applicable`
field is `None` (or sentinel `t` in tree-form).

== Comp_fn ≅ functor's morphism action

A type-former like `Pair : Type × Type → Type` is a *functor* between
∞-groupoids of types: its action on objects is the type-former
itself; its action on morphisms (paths in `Type`) is structural
recursion on those paths.

In ∞-categorical setting, paths between types are equivalences (via
univalence). A type-former that respects equivalences is functorial:
it sends equivalent types to equivalent types via the corresponding
structural action.

```
Functor : Type
Functor := record \{
  obj_action       : params -> Type            -- the type-former
  morphism_action  : ∞-FunctorActionSignature  -- comp_fn role
  identity_law     : preserves refl-paths       -- propositional
  composition_law  : preserves path composition -- propositional
\}
```

The cubical-specific generalization: `morphism_action` becomes
`comp_fn` (handling both type-path transport and side-wall
composition simultaneously via cubical `comp`). For full
∞-functoriality, higher coherences are needed; cubical packages them
into the `comp_fn`'s behavior on nested cubes.

== The unified picture

Putting it together:

```
TypeFormer : Type
TypeFormer := Sigma SubobjectClassifier
              (\{sc\} -> Sigma (Optional Applicable)
                        (\{_\} -> Functor))
```

Read: a type-former is
+ A *characteristic morphism* (recognizer) classifying its inhabitants
  in `Tree`.
+ *Optionally* an applicable structure (eval / classifier) for types
  that participate in application.
+ A *functor structure* (morphism action) preserving type-paths in
  the ∞-groupoid of types.

All three are standard categorical structures expressed as library
types. The kernel reads them via typed projections.

#note[
  *What's genuinely Disp-specific.* The only choice in this design
  that doesn't come from standard category theory is *"everything is a
  tree"* — the commitment to `Tree` as the universal object and to
  hash-cons identity as the equational theory. Once that choice is
  made, the categorical structures slot in without modification. Other
  type theories make different representation choices (Coq's
  CIC-terms, Agda's core-Agda terms); the categorical content is
  invariant across them.
]

= Library design

== `SubobjectClassifier`

```
SubobjectClassifier : Type
SubobjectClassifier := \{P : Type\} -> record \{
  params                   : Type            -- type-former parameter shape
  params_value             : params           -- the actual parameters
  characteristic_morphism  : params -> Tree -> Bool
  -- Operationally: how the type recognizes its inhabitants.
  -- The walker enforces parametricity automatically.
\}
```

For `Bool`, `params = Unit`. For `Pair`, `params = Type × Type`. For
`Pi`, `params = Sigma Type ({A} -> A -> Type)`. The
`characteristic_morphism` is a parametric function `Tree -> Bool` —
i.e., the recognizer.

== `Applicable`

```
Applicable : Type
Applicable := record \{
  eval_morphism : EvalSignature
\}

EvalSignature : Type
EvalSignature :=
  -- The eval map's signature, parameterized over which kind
  -- of eval is operative (LCCC pullback or subobject ∈):
  Pi KernelKs (\{ks\} ->
  Pi KernelRecord (\{raw\} ->
  Pi NeutralMeta (\{nm\} ->
  Pi Tree (\{arg\} ->
  Action))))
```

Where `Action` is the existing tagged sum `Extend new_type | Return value`.
The categorical interpretation:

- `Extend new_type`: result of an LCCC eval, where the codomain
  depends on the argument; the new type is the pulled-back codomain.
- `Return value`: result of a subobject-classifier evaluation; the
  value is the classifier's direct output.

The Action protocol is Disp's *encoding* of the eval-morphism's
output. Library code that consumes Applicable can dispatch on the
Action without knowing the categorical specialization.

== `Functor`

```
Functor : Type
Functor := record \{
  morphism_action : ∞-FunctorActionSignature
  identity_law    : Path morphism_action (refl-action)  -- propositional
  composition_law : Path (morphism_action ∘ morphism_action)
                         (morphism_action ∘ ∘)            -- propositional
\}

∞-FunctorActionSignature : Type
∞-FunctorActionSignature :=
  -- The cubical comp signature:
  Pi (Pi I (\{_\} -> Type)) (\{A\} ->
  Pi I (\{phi\} ->
  Pi (Pi I (\{i\} -> Partial phi (apply A i))) (\{u\} ->
  Pi (apply A I_zero) (\{u0\} ->
  apply A I_one))))
```

The `identity_law` and `composition_law` fields are *propositional
witnesses*. They're `Path`-valued proofs that the morphism action
satisfies the functorial laws on canonical inputs. The kernel doesn't
verify their semantic correctness (undecidable in general), but their
presence is a structural commitment: library authors who skip them
can't construct a valid `Functor`.

For types where ∞-functoriality is partial or trivial (e.g., discrete
types like `Bool`, `Nat`), the laws are easy to provide (identity
proofs are `refl`). For types with non-trivial functoriality (`Pi`,
`Glue`), the proofs are substantive.

== `TypeFormer`

```
TypeFormer : Type
TypeFormer := record \{
  classifier : SubobjectClassifier
  applicable : Optional Applicable
  functor    : Functor
\}
```

This is the unified record. Every library type-former is built by
filling these three fields with the right contents.

== Smart constructor

To prevent direct metadata construction, provide a smart constructor:

```
make_type_former :
  (params : Type) ->
  (recognizer : params -> Tree -> Bool) ->
  (applicable : Optional EvalSignature) ->
  (functor : ∞-FunctorActionSignature) ->
  (identity_law : ...) ->
  (composition_law : ...) ->
  Type
```

Each argument is Pi-checked against its declared type. The output is
a `guard`-wrapped value whose metadata is a `TypeFormer` record.

Library type definitions become:

```
Bool := make_type_former
  Unit
  bool_recognizer
  none                     -- not applicable
  bool_comp_fn             -- trivial (discrete)
  bool_identity_law        -- refl
  bool_composition_law     -- refl
```

vs. the current:

```
Bool := guard (wait kernel_ref.predicate_frame
  (pair bool_recognizer_sig (pair t t)))
```

The new form is verbose but explicit: every contract is named, every
field is typechecked, missing pieces are rejected by the elaborator.

== Categorical hierarchy via record extension

If types vary in how much categorical structure they support, we can
layer the records:

```
BareType : Type
BareType := record \{ classifier : SubobjectClassifier \}

ApplicableType : Type
ApplicableType := record \{
  base       : BareType
  applicable : Applicable
\}

FunctorialType : Type
FunctorialType := record \{
  base    : BareType
  functor : Functor
\}

FullTypeFormer : Type
FullTypeFormer := record \{
  base       : BareType
  applicable : Optional Applicable
  functor    : Functor
\}
```

This layering lets the kernel demand different levels of structure
for different operations:

- Basic membership check requires only `BareType`.
- Application via `hyp_reduce` requires `ApplicableType`.
- Cubical `comp`/`transp` requires `FunctorialType`.
- HIT eliminators require fuller higher-coherence structure.

Trying to `transp` along a `BareType` is a type error: the operation
demands `FunctorialType` and a `BareType` doesn't satisfy it.

= Integration with current Disp

== `Type`'s recognizer

Currently:

```
type_recognizer := \{_, v\} ->
  match (has_sig kernel_ref.guard v) \{
    TT => has_sig kernel_ref.predicate_frame (type_meta v)
    FF => FF
  \}
```

Under this proposal:

```
type_recognizer_v2 := \{_, v\} ->
  match (has_sig kernel_ref.guard v) \{
    TT => \{
      let inner = unguard_checked v
      match (has_sig kernel_ref.predicate_frame inner) \{
        TT => \{
          let tfm_record = type_meta inner
          TypeFormer tfm_record   -- type-membership check
        \}
        FF => FF
      \}
    \}
    FF => FF
  \}
```

The recognizer now requires the metadata to inhabit `TypeFormer`. This
is itself a Pi-check using the existing kernel machinery — no new
mechanism.

== Migration path for existing types

The unified seven-primitive kernel (per `CLAUDE.md`'s landed status)
already has Bool, Nat, Pi, Eq, Ord, Type as predicate_frame-based
library types. Migrating them to use `TypeFormer`:

+ Define `SubobjectClassifier`, `Applicable`, `Functor`, `TypeFormer`
  in `lib/categorical/`.
+ Define `make_type_former` smart constructor.
+ Rewrite each type's definition in `lib/types/X.disp` to use
  `make_type_former`, providing the required fields.
+ Update `type_recognizer` in `lib/kernel/handlers.disp`.
+ Sweep tests that assert against specific metadata shapes.

The actual metadata-tree shape changes (record vs. tuple), so any
host-side fast-path code in `src/tree.ts` that projects metadata
needs corresponding updates.

== Bootstrap

`Type` itself must be a `TypeFormer`. The recursion is:

- `Type` is defined via `make_type_former`.
- `make_type_former`'s return type is `Type`.
- `TypeFormer` is itself a library type (record), so its recognizer
  is some specific `Tree → Bool` function.
- `TypeFormer`'s metadata must itself be a `TypeFormer` record.

This is the universe-self-typing recursion already present in Disp
(`Type : Type`, allowed because Russell paradoxes diverge rather than
yield contradictions; see `TYPE_THEORY.typ` §5.5).

The bootstrap: the kernel ships with a *primordial* `TypeFormer`
record built into its initialization. Subsequent library type-formers
are validated against the primordial. The kernel's `type_recognizer`
uses the primordial to check itself.

#note[
  *Primordial vs. user `TypeFormer`s.* The primordial `TypeFormer`
  shipped in the kernel may have slightly less structure than what
  user code defines (e.g., it might use sentinel laws rather than full
  propositional witnesses). User type-formers can require richer
  structure as needed. As long as user `TypeFormer`s extend (rather
  than restrict) the primordial, validation works in both directions.
]

= Integration with `CUBICAL_PROPOSAL.typ`

The cubical proposal adds a `comp_fn` slot to type-former metadata.
Under the categorical reformulation:

- `comp_fn` becomes the `morphism_action` field of the `Functor`
  record.
- The Partial / Glue / ua machinery from §5-§7 of the cubical
  proposal sits on top of `TypeFormer` records.
- `Glue` is a library type-former whose `Functor.morphism_action`
  encodes the equivalence-mediated transport.

Concretely, the cubical proposal's Layer 2 (metadata refactor) and
this proposal's record-based reformulation should happen *together*:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Step*], [*Cubical proposal alone*], [*Combined with categorical*],
    [Metadata layout],
      [Positional 4-tuple: `(sig, params, cod, comp)`],
      [`TypeFormer` record with named fields],
    [Type validation],
      [Structural shape check],
      [Pi-check against `TypeFormer`],
    [Library type definition],
      [`guard (wait predicate_frame (...))`],
      [`make_type_former params recognizer ...`],
    [Adding higher coherences],
      [New positional slot, sweep],
      [Extend `Functor` record, sweep],
  ),
  caption: [Doing both proposals together avoids refactoring twice.],
)

The categorical reformulation is *purely structural* — it doesn't
change what the kernel does, only how library types are expressed.
The cubical proposal is *operationally* additive (new dispatch
machinery for comp). They compose cleanly when done together; doing
them sequentially means refactoring the metadata twice.

== Recommendation

Pursue this proposal as a prerequisite to (or simultaneous with)
the cubical single-pass implementation:

+ *If you haven't started cubical yet*: define the categorical
  foundations first, then build cubical on top. The cubical proposal's
  `comp_fn` slot is replaced by the `Functor.morphism_action` field;
  everything else fits naturally.

+ *If you're partway through cubical*: pause and define the
  categorical foundations. Refactor what's been done so far. The
  alternative — completing cubical with positional tuples and then
  refactoring to records — is more total work.

+ *If you're not doing cubical*: this proposal is still worthwhile
  for the validation and clarity benefits. The cubical-specific pieces
  (Functor with `comp_fn`-shaped morphism_action) can be deferred.

= Validation layers

The categorical reformulation enables validation at five distinct
levels. From cheapest to most thorough:

== Level 1 — Structural validation

`Type`'s recognizer checks that the metadata is a `TypeFormer`
record-shaped tree (specific nested-pair structure). Cheap; catches
"this isn't a type at all."

== Level 2 — Component types

Each field of the `TypeFormer` record is Pi-checked against its
declared type. The `characteristic_morphism` must have shape
`params → Tree → Bool`. The `morphism_action` must have the comp
signature. Etc.

This catches:
- Arity mismatches.
- Return-type errors.
- Dependent-shape errors.
- Missing slots (the record literally lacks the field).

Cheap-to-moderate (just the existing Pi-checking machinery).

== Level 3 — Behavioral testing

For closed instances, run the operations on canonical inputs and
check the categorical laws. E.g.:

```
test_functor_identity := \{T, sample\} ->
  tree_eq (comp (\{_\} -> T) I_zero empty sample) sample
```

This isn't part of `Type`'s recognizer (too expensive); it's run as
part of the library type's test suite. Catches semantic errors that
type-level validation misses.

== Level 4 — Propositional law witnesses

The `Functor.identity_law` and `composition_law` fields are
propositional `Path`-valued proofs. Library authors must construct
them. The kernel verifies the proofs *typecheck* (i.e., have the
right shape) but doesn't verify they're semantically correct
(undecidable).

Forces library authors to think about the laws. Forging a wrong proof
is hard — you'd have to lie at the type level, which is harder than
just writing a wrong morphism_action.

== Level 5 — Full semantic verification

Decidable for very restricted cases (e.g., finite types) but
*undecidable in general*. No type system can give you this for
arbitrary higher-order functions. We don't pursue it.

== What we can and can't catch

#figure(
  table(
    columns: 2,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Error class*], [*Caught by*],
    [Wrong tree shape], [Level 1 (structural)],
    [Wrong field arity], [Level 2 (component types)],
    [Wrong return type], [Level 2],
    [Type-incorrect law witness], [Level 2 (proof's type fails)],
    [Wrong but type-correct law witness], [Level 4 (hard to forge)],
    [Function not actually functorial], [Level 3 (testing) for canonical inputs],
    [Function not functorial on adversarial inputs], [Undecidable; not caught],
    [Semantic correctness in general], [Level 5; undecidable],
  ),
  caption: [Validation coverage by error class.],
)

This is *as much validation as is decidable*. The undecidable
remainder (full semantic correctness) is intrinsic to the
expressiveness of dependent type theory; no foundation can give it.

= Implementation plan

Roughly 7 layers, dependency-ordered:

== Layer 0 — Categorical structure definitions

```
lib/categorical/subobject_classifier.disp
lib/categorical/applicable.disp
lib/categorical/functor.disp
lib/categorical/typeformer.disp
```

Define `SubobjectClassifier`, `Applicable`, `Functor`, `TypeFormer`
as library records (Sigma types). ~200 lines.

== Layer 1 — Smart constructors

```
lib/categorical/constructors.disp     // make_type_former
```

The smart constructor that bundles the fields into a `TypeFormer`
record, performing Pi-checks on the way in. ~100 lines.

== Layer 2 — Primordial bootstrap

```
lib/kernel/handlers.disp              // primordial TypeFormer for Type
```

The kernel's initialization includes a primordial `TypeFormer`
instance for `Type` itself, breaking the recursion. ~50 lines of
kernel-side changes.

== Layer 3 — `Type`'s recognizer update

```
lib/kernel/handlers.disp              // type_recognizer_v2
```

Replace the structural check with type-membership against
`TypeFormer`. ~30 lines.

== Layer 4 — Migrate existing types

```
lib/types/bool.disp    // rewrite using make_type_former
lib/types/nat.disp
lib/types/pi.disp
lib/types/eq.disp
lib/types/ord.disp
lib/types/type.disp
```

Each existing library type-former rewritten to use the smart
constructor. Per-file, ~50 lines of changes. ~6 files = ~300 lines.

== Layer 5 — Update host fast-path

```
src/tree.ts                           // metadata projection updates
```

Any TypeScript code that projects metadata positionally (e.g., for
performance fast-paths) needs to follow the new record structure. ~50
lines of host-side changes.

== Layer 6 — Test sweep

Tests that assert against specific metadata shapes need updating.
Add tests for:
- Primordial `TypeFormer` bootstrap.
- Each type's `TypeFormer` field contents.
- Smart constructor rejection of malformed inputs.
- `Type`'s recognizer correctly accepting valid types and rejecting
  invalid ones.

~150 new tests, plus updating ~80 existing tests.

== Scope summary

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Layer*], [*New lines*], [*Disrupted lines*],
    [0. Categorical structures], [~200], [0],
    [1. Smart constructors], [~100], [0],
    [2. Primordial bootstrap], [~50], [~10 (handlers)],
    [3. Type recognizer update], [~30], [~10 (handlers)],
    [4. Migrate existing types], [~300], [~100 (existing types)],
    [5. Host fast-path], [~50], [~30 (tree.ts)],
    [6. Test sweep], [~600], [~150 (existing tests)],
    [*Total*], [*~1330*], [*~300*],
  ),
  caption: [Estimated implementation scope.],
)

About one week of focused work, assuming the categorical structure
definitions are right. Less than the cubical proposal because no new
operational machinery is added — just structural reformulation.

= Benefits and costs

== Benefits

+ *Validation by construction.* Library authors can't accidentally
  produce malformed types; the smart constructor and `TypeFormer`
  Pi-check ensure structural correctness.

+ *Self-documenting design.* The categorical contract is *literal*,
  named, and standard. Anyone with category-theory background can
  read `TypeFormer` and understand what a type-former must satisfy.

+ *Cross-system communication.* Categorical vocabulary is shared
  across type theory frameworks. Disp's design becomes easier to
  explain to people from Agda / Coq / Lean / category theory
  backgrounds.

+ *Graceful extensibility.* Future categorical operations (higher
  coherences, modal structure, etc.) extend the relevant categorical
  structure rather than adding ad-hoc slots.

+ *Composability with cubical.* The cubical proposal's `comp_fn` slot
  becomes the `Functor.morphism_action` field, fitting naturally.

+ *Better error messages.* "Missing `Functor.identity_law`" beats
  "Tuple offset 4 is wrong shape." Errors point at named contracts.

== Costs

+ *Library verbosity.* `make_type_former Bool unit bool_recognizer
  none bool_comp_fn bool_id_law bool_comp_law` is longer than `guard
  (wait predicate_frame (pair sig (pair t t)))`. More to write per
  type.

+ *Migration effort.* Every existing library type-former must be
  rewritten. ~300 disrupted lines.

+ *Bootstrap care.* The universe-self-typing recursion needs careful
  handling. Primordial `TypeFormer` must be set up correctly in the
  kernel.

+ *Performance overhead.* Negligible in principle (record projection
  is the same cost as positional access), but worth measuring.

+ *Learning curve for non-category-theorists.* Library authors need
  to understand the categorical structures to fill in the fields
  correctly. We can mitigate via good documentation and template
  examples for each `TypeFormer` field.

== Trade-off summary

Net positive for any user who values:
- Soundness of library type-formers (catches more errors).
- Clarity of design (categorical structure is explicit).
- Future extensibility (standard categorical extensions).

Net negative for users who only want:
- Minimal boilerplate when defining new types.
- No need to understand the categorical contract.

For Disp's stated goals — self-hosting, neural-guided synthesis,
optimizer that must reason about types — the benefits clearly
outweigh the costs.

= Open questions

== Universe stratification

`TypeFormer` is itself a library type, so `Type TypeFormer = TT`.
But `TypeFormer` describes what `Type` *is*. Is this circular?

Operationally, no — the recursion is handled by the primordial
bootstrap. But there's a subtle question about consistency: if we add
even more categorical structure to `TypeFormer` (higher coherences),
do we hit a universe paradox?

In Disp's current design, paradoxes diverge rather than yielding
contradictions. So this is "safe" in the technical sense. But it'd be
worth investigating whether stratifying `TypeFormer` (e.g., having
`TypeFormer₀ : TypeFormer₁ : TypeFormer₂ : ...` matching universe
ranks) is *necessary* for cleanliness, even if not for soundness.

Currently this is "open" — we'd need to encode richer cases and see
if the divergence-as-failure story holds up under more structure.

== Performance

Record projection vs. positional access. In tree calculus, both are
chains of `pair_fst`/`pair_snd`. Same cost in principle.

But: hash-cons identity of large `TypeFormer` records is more
structural work than positional tuples. Two equivalent type-formers
might hash-cons differently if their record-field ordering or
intermediate structure differs.

Mitigation: define `TypeFormer` with a *canonical* nesting order, so
that equivalent records produce identical trees. Currently this is
an "open" question pending implementation experience.

== Naming conventions

`SubobjectClassifier`, `Applicable`, `Functor` — these are
mathematically standard but verbose. Alternatives:

- Shorter: `Recognizer`, `Eval`, `Action` (loses categorical
  connection).
- Disp-flavored: `TypePredicate`, `TypeEval`, `TypeFunctorial`
  (longer but Disp-aware).
- Mixed: `Recognizer` (short), `Applicable` (medium), `Functor`
  (standard).

Recommendation: use the categorical names. They're standard, they
connect Disp to existing literature, and verbosity is fine for
foundational library types that aren't written often.

== Extension to higher coherences

For ∞-functoriality, the `Functor` record might need additional
fields for higher coherences (2-cells, 3-cells, etc.). The CCHM
cubical answer is "comp handles all of them implicitly through cube
arithmetic." But explicit higher-cell fields might be needed for
some constructions.

If/when this is needed, extending the `Functor` record with
additional fields is a one-time refactor — and each existing type
needs to opt in (e.g., by providing `refl`-witnesses for the new
coherences). Open: should we plan for this now or extend reactively?

= Summary

Disp's current type-former metadata is a positional tuple with
conventional slot meanings. Each slot corresponds to a standard
categorical structure: characteristic morphism into the subobject
classifier (recognizer), eval morphism of an LCCC / topos
classification (codomain_fn), and ∞-functor morphism action
(comp_fn).

This proposal makes the categorical content explicit by reformulating
the metadata as a typed record (`TypeFormer`) bundling three
inhabitable structures (`SubobjectClassifier`, `Applicable`, `Functor`).
`Type`'s recognizer becomes a type-membership check rather than a
structural shape check. Library type-formers are constructed via a
smart constructor with Pi-checked components.

Benefits: validation by construction, self-documenting design,
graceful extensibility, alignment with established categorical
literature, easier cross-system communication.

Costs: library verbosity, migration effort on existing types,
careful bootstrap handling, possible (negligible) performance overhead.

The seven kernel primitives remain seven. The kernel doesn't grow;
the library expresses what it's been implicitly doing all along, in
standard mathematical vocabulary.

This proposal composes naturally with `CUBICAL_PROPOSAL.typ`: the
cubical `comp_fn` slot becomes the `Functor.morphism_action` field.
Doing both proposals together avoids refactoring metadata twice.

The only genuinely Disp-specific decision in the entire categorical
picture is *"everything is a tree"* — the commitment to `Tree` as
the universal object. Everything else slots into well-established
categorical frameworks (topos theory, LCCC, ∞-category theory).
