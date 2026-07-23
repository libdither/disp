# Barry Jay's Combinatory Types, and What They Would Mean for disp

A deep dive into Barry Jay's recent type-system work (types are programs that reduce; type
inference is evaluation, not unification), and a point-by-point mapping onto disp's kernel
(types are recognizers; checking is evaluation of predicates). Written 2026-07.

## This folder

- `simple-types-for-polymorphic-functions-2604.12194.pdf` — Jay & Bader, arXiv:2604.12194
  (April 2026). The main subject of this document. The combinatory type system.
- `simple-types-for-polymorphic-functions.txt` — same paper as plain text, for grepping.
- `reflective-programs-in-tree-calculus-tree_book.pdf` — Jay's tree calculus book (2021+,
  appendix with Jose Vergara). Background for the substrate the types are meant to move to.
- `ocaml-implementation/` — Bader's OCaml type inference for the paper
  (github.com/olydis/combinatory-types, archived on Zenodo as 10.5281/zenodo.15338381).
  `lib/infer.ml` is the algorithm of Section 10 of the paper; `lib/types.ml` is type
  application; `lib/iota.dag` is an example program.

Not downloaded (paywalled): Jay, "Typed Program Analysis without Encodings", PEPM'25,
DOI 10.1145/3704253.3706138. Its Rocq formalization is public:
github.com/barry-jay-personal/typed_tree_calculus. The Rocq proofs for the 2026 paper are at
github.com/barry-jay-personal/combinatory-types.

## 1. Lineage: how Jay got here

Jay has spent decades on one idea: computation should be able to inspect its own programs
(intensionality), and the right substrate makes program analysis ordinary programming.

- **Pattern calculus** (book, Springer 2009; language: bondi). Patterns are first-class;
  matching is function application; data structures are functions. The seed of "structure is
  something you compute with, not just match against."
- **SF-calculus / factorisation calculus** (with Given-Wilson, 2011). A combinatory calculus
  that can factorise a value into operator and argument — i.e. decompose programs — and so
  can decide *equality of closed normal forms*. Jay argues ("Confusion in the Church-Turing
  Thesis", 2014, with Vergara) this makes it strictly more than lambda-style models, where
  equality is not definable.
- **Tree calculus** (2021, the tree book in this folder). The minimal version: one operator,
  five reduction rules, Turing-complete, reflective, programs are normal forms.
- **Typed tree calculus** (PEPM'25). A Curry-style type assignment for tree calculus with
  "combinations of quantified function types not found in HM", plus subtyping; it can type a
  self-interpreter. Still has quantifiers. Rocq-checked.
- **Combinatory types** (2026 paper). Deletes the quantifiers. Each combinator has at most
  one type; polymorphism is not declared but *computed on application*. Type inference =
  evaluation. This is the system this document is about, and Jay explicitly frames it as a
  stepping stone: internalise the types into tree calculus, then "blend them in a system of
  dependent types" (§12.4) — which is disp's neighborhood.

## 2. The substrate in five minutes (tree calculus)

Expressions: `E ::= △ | E E` — one operator and application. Values are unlabeled binary
trees: `△` is a *leaf*, `△a` a *stem*, `△ab` a *fork*. Reduction (the "triage calculus"
refinement, suggested by Johannes Bader):

```
△△ y z        ⟶ y                 (K)
△(△x) y z     ⟶ x z (y z)         (S)
△(△wx) y △    ⟶ w                 (triage: leaf case)
△(△wx) y (△u)   ⟶ x u             (triage: stem case)
△(△wx) y (△uv)  ⟶ y u v           (triage: fork case)
```

Rules 1–2 give K and S (Turing-complete); rules 3a–c give *triage*: case analysis on the
shape of a value, which is what lambda calculus cannot do. Confluent, non-overlapping,
only observes values. Programs = normal forms, and every computable function has one
(possible in combinatory calculi, not in λ-calculus — recursive functions can be normal
forms via fixpoint constructions). Consequence Jay leans on hard: **program analysis needs
no encoding of programs as syntax trees** — programs already are trees, and a self-
interpreter is an ordinary term.

disp's substrate is the same idea: `Leaf | Stem | Fork` in `src/core/tree.ts`, everything
always in normal form ("application is an operation"), intensional. The two calculi are
close cousins; that is why the mapping later in this document is mostly smooth.

## 3. The combinatory type system (the 2026 paper)

### 3.1 Terms

`M, N ::= S | K | M N`, with `S M N P ⟶ M P (N P)` and `K M N ⟶ M`. Lambda is sugar via
bracket/star abstraction; abstractions are always normal forms. Two gadgets matter:

- `wait{M,N} = S(S(KM)(KN))I` — keeps M and N apart until applied (`wait{M,N} P ⟶ M N P`).
  Used to build normal-form fixpoints: `Z{f}` waits for an argument, so it is normal if f is.
- `tagged{f,t} = S(S(KK)f)(tag (K t))` with `tag = S(S(KK)(KK))` — wraps f so that
  `tagged{f,t} u ⟶ f u` (functionality preserved) but the term is structurally marked by t.
  Theorem `tagged_not_star`: a tagged term is never a λ-abstraction. This separation is what
  lets the type system tell constructors apart from functions — the basis of nominal typing.

### 3.2 Types and type application

Core ("combinatory") types:

```
T, U, V ::= S0 | S1 U | S2 U V | K0 | K1 U
```

A type is the *shape of a value's normal form*: `|S| = S0`, `|S p| = S1 |p|`,
`|S p q| = S2 |p| |q|`, `|K| = K0`, `|K p| = K1 |p|`. So `SKK : S2 K0 K0`. No type
variables, no quantifiers, no function types.

There is essentially one typing rule. Application is typed by **type application**, a
partial function that shadows the term reduction rules:

```
S0 (U)        = S1 U
S1 U (V)      = S2 U V
S2 T1 T2 (U)  = T1(U) (T2(U))      (shadows  S M N P ⟶ M P (N P))
K0 (U)        = K1 U
K1 U (V)      = U                  (shadows  K M N ⟶ M)

M : T    N : U
─────────────────   T(U) = V
     M N : V
```

The recursive call in the S2 rule terminates iff the corresponding term reduction does —
type application is exactly as terminating as the program. Proven in Rocq: reduction
preserves typing; every combinator has at most one type; there is an effective type
inference algorithm.

### 3.3 Worked examples (verified by hand against the rules above)

**A type has many terms.** `K(SKK) S : S2 K0 K0`:

```
K0 (I0) = K1 I0            where I0 = S2 K0 K0
K1 I0 (S0) = I0
```

Same type as `SKK`, correctly, since `K(SKK) S ⟶ SKK`. Membership is "evaluates to this
shape": one type per *value*, many terms per type. The type is a prediction of the result's
shape; checking is checking the prediction.

**Polymorphism without ∀.** The identity `I = SKK` has the single type `I0`. Apply it to an
argument of any type T — evaluate, don't unify:

```
I0 (T) = S2 K0 K0 (T) = K0(T) (K0(T)) = K1 T (K1 T) = T
```

`I` behaves as `∀T. T → T` with no quantifier anywhere: polymorphism is latent in the
type's structure and revealed by application. Where System F specialises a term by applying
it to a type, Jay specialises a *type* by applying it to a type.

**Types run pipelines.** Composition `S (K g) f` (f then g) has type `S2 (K1 G) F`:

```
S2 (K1 G) F (U) = (K1 G)(U) · (F(U)) = G (F(U))
```

The result type is "run U through F, then through G" — the type-level computation is the
term-level computation one level up.

**What the core rejects: divergence.** Ω = `(SII)(SII)`, with `SII : S2 I0 I0`:

```
S2 I0 I0 (S2 I0 I0) = I0(X) (I0(X))   where X = S2 I0 I0
I0 (X) = X                            ...so the application reduces to X(X) = itself — loop
```

Type application diverges exactly where the term does. So even the bare core is a
classifier: a **termination analysis plus exact result-shape analysis**. Programs that loop
are ill-typed. The paper's inference algorithm loops here too (§10).

**Self-application works, though.** `let f = I in f f` is `(SII) I`, and
`S2 I0 I0 (I0) = I0(I0)(I0(I0)) = I0`. This is the example that needs `let` and type-scheme
instantiation in Hindley-Milner; here it is one evaluation. The paper stresses the system
types "structures containing polymorphic data that are beyond the traditional algorithm".

### 3.4 The hybrid system (how quantifiers were eliminated)

Section 3 of the paper first adds combinatory types as *subtypes* of HM monotypes, with
structural subtyping rules that mirror reduction:

```
K0 < U → K1 U                    K1 U < V → U
S0 < U → S1 U                    S1 U1 < U2 → S2 U1 U2
S2 (U→V→W) (U→V) < U → W         (mirrors S-reduction)
```

Then it shows the subtyping can be *replaced* by type applications (`S0 (K0) = S1 K0`
instead of `S0 < K0 → S1 K0`), leaving the pure system of §3.2. Conjecture 3.1 (stated,
unproven): every HM-typable program has a principal combinatory type. The trajectory
matters for disp: Jay started from a conventional assignment system and *dissolved* the
machinery (variables, quantifiers, subtyping, unification) into evaluation.

### 3.5 Abstract types: the accept/reject layer

The core cannot say "this type has several *different values*" — Bool vs Nat, the actual
classificatory job of a type system. That is a separate, deliberate layer. **A type
declaration carves a named subset out of the structural soup** by adding type-application
rules. Abstract types:

```
T ::= ... | Abs0{T} | Abs1{T} U | Abs2{T} U V      (T in braces is a label)
```

Two mechanisms make it work:

1. **Tagging.** Constructors are ordinary combinators wrapped as `tagged{f,t}` — same
   functionality, structurally marked. The tag is what makes types nominal: two isomorphic
   encodings with different tags are different types.
2. **The S1 restriction.** `S1 U (V) = S2 U V` now fires only *if `S2 U V` is not a tagged
   type*. So a tagged term's structural type is never produced; it has a type only when a
   declaration's rule fires. This keeps type application functional (one type per term) when
   abstract types are added: each new type is new rules, no changes to old ones.

The asymmetry after this layer: **many terms (and values) per type; still one type per
term.** The first direction is the classification you want from a type system; the second is
what keeps inference = plain evaluation.

The paper's abstract types, cumulatively:

- **Products.** `U∗V = Abs2{S1 S0} U V`, with `pair`, `fst`, `snd`. Elimination:
  `(U∗V)(T) = T(U)(V)` — note how this mirrors `pair u v K ⟶ u` at the type level.
- **Booleans.** `Bool = Abs0{S1 K0}` with `tt`, `ff`, `cond`. The raw structural type of
  `cond` is a huge shape-descriptor ("exactly describes the structure of cond but obscures
  its functionality"), but the machine proves the human rule as a *theorem about
  evaluation*: `|cond|(Bool)(U)(U) = U` (theorem `app_ty_cond`), giving the derived rule
  "b : Bool, u,v : U ⊢ cond b u v : U" (`derive_cond`). **Familiar typing rules come back as
  theorems about where the evaluation lands, not as primitives.**
- **Sums.** `U+V`, with a catch: `inl u` cannot have a unique type (`sum U V` for any V), so
  constructors take *dummy values* to pin down the other side. Requires U, V inhabited.
- **Function types.** `U→V = Funty U V = Abs2{K1 K0} U V` is *just another abstract type* —
  "their purpose is to hide implementation details, just as Bool hides the structure of tt."
  Introduction: `mk_fun (pair t d)` where dummy `d : U` and `T(U) = V`; elimination:
  `(U→V)(U) = V`. `lam x t d` combines star-abstraction with `mk_fun` (derived rule
  `derive_lam`). Warning the paper repeats: tagging into a function type *hides*
  polymorphism (`cond_mono : Bool→U→U→U` is monomorphic where raw `cond` was polymorphic).
  Abstraction and polymorphism are in tension — opacity is a choice, per declaration.
- **Recursion.** `Z{f} x ⟶ f (Z{f}) x` with `Z{f}` a normal form (via `wait2` and double
  tagging). `Rec{F} = Abs1{K0} F`, with a *conditional* elimination rule:
  `Rec{F} (V∗U) ⇒ V if F(V∗U→V)(V∗U) = V`. The paper does not give Rec a declaration —
  conditional rules don't fit the declaration machinery cleanly yet.
- **Nat.** `Nat = zero | successor of Nat`. Nats are encoded as their own fold: `n` applied
  to `pair base step` computes the fold. Elimination typing:
  `Nat (V1∗V2) = V1 if V2(Nat) = V1`. Example: `isZero = λn. n (pair tt (K ff))` — argument
  type `Bool ∗ K1 Bool`, side condition `K1 Bool (Nat) = Bool ✓`, so `isZero n : Bool`.
  Primitive recursion (`primrec`) and minimisation (`minrec`) are defined via Z with
  derived typing rules — the system is Turing-complete.
- **Lists.** `List{U} = nil of U | cons of U∗List{U}` (nil takes a dummy, like sums).
  Conditional elimination `(List{U})(V∗T) = V if T(U∗List{U}) = V`; `fold_left` via Z, with
  derived rule `derive_fold_left`.

The summary system (Figure 5) collects all of this into one type-application `match` with
~20 branches — that one function *is* the whole type system, plus the three derivation
rules (S, K, application-via-type-application, plus variable lookup for open terms).

### 3.6 Checking and errors, concretely

A program is rejected when type application is **undefined** (no rule fires) or
**diverges**. The paper's running error example is `successor tt`: applying the successor
rule's shape to `Bool` matches nothing, inference fails fast. Positive case:
`zero : Nat`, `successor zero : Nat` (rule fires), etc. So the abstract layer does the
ordinary accept/reject job — with the criterion defined *nominally* (declarations and tags)
on top of the exact structural computation.

### 3.7 Type inference (§10)

Since types are unique when they exist, inference is a recursive evaluator:

```ocaml
let rec infer gamma m =
  match m with
  | Ref x -> get x gamma
  | Sop -> Some Sty | Kop -> Some Kty
  | App (m1, m2) ->
      match (infer gamma m1, infer gamma m2) with
      | Some ty, Some vty -> infer_app ty vty   (* evaluate the type *)
      | _, _ -> None
```

`infer_app` (in `ocaml-implementation/lib/infer.ml`) implements the type-application rules
recursively. Theorem 10.1 (Rocq): `Γ ⊢ M : T ⟺ infer Γ M = T` — sound *and* complete; it
finds the type whenever one exists. Termination: fails quickly on common type errors, loops
only on pathological self-application (`(SII)(SII)`); Z-recursion is handled by special
cases. The Rocq version bounds recursion depth; the OCaml version counts `infer_app` calls
as a budget. Measured: calls-to-term-size ratio below 2 for most, below 3 for all tested
examples — effectively linear for realistic programs. Known worst case: long chains
`SSSS…` force repeated rewriting of the whole type, O(k²).

### 3.8 Inhabitation ("find a term that fits this type")

Not treated in the paper (the word "inhabited" appears only as the dummy-value side
condition for sums and lists). The landscape, from the mechanics:

- **Core layer: trivial.** Types and values are in bijection — `S2 K0 K0` decodes to exactly
  `SKK`. Inhabitation is a decoder, not a search; the "proposition" already contains its
  proof. Curry-Howard degenerates.
- **Abstract layer: genuine synthesis, semi-decidable.** "Find a term of type `Nat→Nat`"
  means finding a combinator + dummy whose tag checks out — a search problem. Two unusual
  conveniences: checking a candidate is effective (type application is a function;
  uniqueness means no guessing which type to check against), and enumeration has no
  redundancy (programs are normal forms of an intensional calculus — no two candidates are
  the same term modulo reduction). But undecidable in general: the system types μ-recursive
  functions, so inhabitation is Rice-flavored hard. Semi-decision procedure: enumerate
  normal forms by size, type-check each.
- The interesting zone is exactly where the two layers meet: abstract types as *specs*,
  structural computation as the *search space*.

### 3.9 Comparison to the usual suspects

| | HM | System F | Combinatory types |
|---|---|---|---|
| type variables / quantifiers | yes / ∀ at top | yes / ∀ anywhere | none |
| principal types | yes (schemes) | no | yes — the unique type |
| inference | unification (Algorithm W) | undecidable | evaluation of type application |
| polymorphism | declared by ∀, instantiated by substitution | declared by ∀, applied explicitly | latent in structure, revealed by application |
| data types | ad hoc constants + declarations | Church encodings | declarations = new type-application rules over tagged SK-terms |
| term syntax changes for typing | yes (let, annotations) | yes (Λ, type application) | none — terms stay SK |
| beyond-HM polymorphism | no | yes (no inference) | yes (with inference) — e.g. `(SII) I` |

The philosophical claim (§13): combinatory logic is a better base for static analysis than
λ-calculus because all programs are normal forms — "there is no need to encode programs as
syntax trees before analysing them" — and expressiveness grows by *declarations* (new
type-application rules), never by changing the term language.

## 4. The PEPM'25 typed tree calculus (brief)

"Typed Program Analysis without Encodings" (not in this folder; ACM paywall, Rocq at
github.com/barry-jay-personal/typed_tree_calculus) types tree calculus itself, still with
quantified function types ("combinations of quantified function types not found in HM") and
a subtyping relation, Curry-style. Headline result: it can type a **self-interpreter** —
typed program analysis internalised as a first-class program, no encodings. The Rocq file
list is a decent table of contents: `types.v`, `subtypes.v`, `derive.v`,
`typed_lambda/recursion/triage/evaluator.v`, `classify*.v` (case analysis on subtyping and
derivations), `reduction_preserves_typing.v`, plus `rewriting_theorems.v` for the
breadth-first strategy. The 2026 combinatory-types paper is the sequel that deletes the
quantifiers; §12.4 says the endgame is to port combinatory types to tree calculus,
internalise the types *as terms*, "and then blend them in a system of dependent types."

## 5. The disp mirror, point by point

Both systems make the same root move — collapse typing into ordinary evaluation in one
untyped universe of normal-form trees; no separate type syntax, no unification, no
metavariables — and then fork on **which side runs**:

| | Jay (combinatory types) | disp (kernel, post `param_apply`) |
|---|---|---|
| a type is… | a **transfer function**: argument-type → result-type | a **recognizer/predicate**: value → Bool (`make_type` bundles recognizer + gate + members + elim) |
| the app rule | `M:T, N:U ⊢ MN : T(U)` — evaluate the **type** | `Pi A B` mints a fresh hyp `h:A`, runs `f h`, checks against `B h` — evaluate the **term** (on a neutral); recognizer judges |
| typing a value | unique (at most one type) | membership (a value passes many types: Nat, refinements, Eq-families…) |
| consequence | **inference** falls out: evaluation replaces unification | **checking** only; inference impossible in principle (no unique answer) |
| open functions | never evaluated; no hypotheses anywhere | neutral evaluation is the core mechanism: `bind_hyp`, `hyp_reduce`, `respond` tables, parametricity policing in the walker |
| polymorphism | latent in structure, computed on application | erased dependent intersections (`Intersection`, Cedille lineage); enforced parametricity |
| conversion | syntactic identity of types | `tree_eq` — O(1) hash-cons identity (everything always normal) |
| termination | type application loops iff the term loops | user recognizers are arbitrary computation; budgets make it explicit (`Err` not loop) |
| where correctness lives | **external**: Rocq theorems about the calculus | **internal**: kernel written in disp; `param_apply`'s honesty claim is itself a machine-checked Pi-type statement; `Type : Type` deliberate |
| recursion | special-case `Rec{F}` with conditional elimination | ordinary dependent eliminators (motives), registered per-type |
| nominal/abstract types | `tagged{f,t}` + declarations | type formers are library code; records are headered values projected by name |

The deepest single difference: **Jay's system is a function; disp's is a relation.**
`T(U)=V` computes *the* answer; `param_apply T v` tests *a* membership. Everything else —
inference vs checking, uniqueness vs multiplicity, no-hyps vs hyps — follows from that.

Two structural echoes worth naming:

- **The app rule is the whole system, in both.** Jay: one rule, `T(U)=V`. disp: with
  `param_apply` absorbed into the Pi former, the application rule *is* `Pi`'s recognizer
  (mint, run, check). The difference: Jay's rule is still meta-level (OCaml/Rocq);
  disp's runs in-language.
- **Tagging ≈ headers.** Jay's `tagged{f,t}` (nominal marker, functionality preserved) is
  the same device as disp's headered records and `make_type` bundles: intensionality
  exploited to give nominal identity to structural values. Both calculi can do this only
  because they are intensional — λ-calculus cannot.

Also note what Jay's "polymorphism" is *not*: it is not parametricity. A combinator's type
makes no uniformity promise — it just computes. disp's `Intersection` plus the walker's
policing actually enforces parametric behavior. Jay gets genericity for free but no free
theorems.

## 6. What this could mean for disp

### 6.1 A direct embedding is nearly free

SK embeds in disp's tree substrate immediately (same Leaf/Stem/Fork trees). A combinatory
type is a tree; `infer_app` is a ~30-line disp function on trees; checking a program is
`tree_eq` against the computed type. This would be a small, self-contained **litmus test
of the kernel and elaborator**: a second, structurally different checker (transfer-function
style rather than recognizer style) to sanity-check against `param_apply` on shared
examples. It also exercises exactly the machinery Jay-style typing would need in tree
calculus anyway (his §12.4 internalisation plan).

### 6.2 An "inference island" inside disp

disp cannot infer, for a principled reason: predicates have no unique answer. But one could
carve a fragment where types *are* unique shape-descriptors (combinatory types, possibly
plus declared nominal abstract types), and recover **inference-by-evaluation exactly there**
— e.g. auto-infer types for first-order structural glue code, while recognizer-checking
handles the rich layers. Jay's work is the first existence proof that this trade is
available with the quantifier machinery fully removed: uniqueness, not unification, is the
real prerequisite for inference. disp gave up uniqueness globally; nothing stops a local
subsystem from reinstating it. This is probably the most actionable idea in the whole
comparison.

### 6.3 A cheaper checking tier

disp's Pi checking pays for hypotheses, `respond` tables, and parametricity policing on
every check. For non-dependent, structurally-typed code, Jay shows a lighter path: compute
the result type by evaluation, compare with `tree_eq`. No minted neutrals, no policing —
because you never evaluate the *term*, only the *type*. If profiling ever shows hyp
machinery dominating check time, a Jay-style fast path for the structural fragment is the
obvious optimization tier (and fits the repo's "nice code first, `.opt.disp` later" rule:
recognizers stay the spec, type application becomes the proven-faster path where it
applies).

### 6.4 Dependent types: Jay's roadmap ends where disp lives

Recap of the earlier analysis, kept because it drives the above:

- The core system is already *degenerately dependent*: the argument's type is its full
  value-structure, so result types already depend on argument *values* (in disguise).
  `Vec' = Abs1{vec_tag} |n|` with the index riding as a shape is sketched by the machinery.
- What's missing for real dependent types: opacity that *retains* a value index (abstract
  types currently erase values wholesale); propositions *about* values, not just shapes
  (i.e. recognizers); and an internalised judgment (`:` as a computed relation, not a
  meta-level derivation).
- The cost is known: **uniqueness dies** (a `Vec Nat 2` is also a `List Nat`), and
  uniqueness is the load-bearing assumption of inference-by-evaluation. Take the step and
  you land where disp sits: types are predicates, checking is evaluation, hypotheses do the
  work of binders, inference is gone. Jay's §12.4 endgame and disp's kernel look like the
  same point in the design space reached from opposite directions.

### 6.5 What each side teaches the other

disp → Jay: neutral evaluation + `respond` is a working answer to "how do you check open
functions"; the parametricity-policing walker is a working answer to "how do you enforce
uniformity"; budgets make the divergence-is-the-type-error tradeoff explicit
(`Err` instead of loop). Also: the observer-restriction problem (programs that can detect
they're being probed with a minted neutral — `ACTIVE_BUGS.md` item 5, studied in miniature
in `lib/standalone_kernel.disp`) is a real hazard Jay will meet the moment types inspect
*terms* during checking rather than just other types.

Jay → disp: uniqueness as the precise price of inference (§6.2); derived rules as theorems
about evaluation (`app_ty_cond`) as a pattern for stating recognizer-level lemmas; abstract
interpretation as the organising metaphor for layered type systems (his §12.3: combinatory
types as concrete domain, abstract types as abstract domains — maps cleanly onto disp's
`Type`/`StrictType`/`BehavioralType` tiers in `lib/kernel/universe.disp`); and the
demonstration that a serious type system can be *one partial function* — a useful
austerity benchmark for kernel complexity.

## 7. Sources

- Jay & Bader, *Simple Types for Polymorphic Functions*, arXiv:2604.12194 (2026).
  PDF + text in this folder. Rocq: github.com/barry-jay-personal/combinatory-types.
  OCaml: github.com/olydis/combinatory-types (in `ocaml-implementation/`).
- Jay, *Typed Program Analysis without Encodings*, PEPM'25, DOI 10.1145/3704253.3706138.
  Rocq: github.com/barry-jay-personal/typed_tree_calculus.
- Jay, *Reflective Programs in Tree Calculus* (2021+, appendix with Vergara). In this
  folder. Site: treecalcul.us (spec, demos, visualizations).
- Jay, *Pattern Calculus: Computing with Functions and Structures*, Springer 2009.
  Language: bondi (github.com/Barry-Jay/bondi).
- Jay & Vergara, *Confusion in the Church-Turing Thesis*, arXiv:1410.7103 (2014).
- disp side: `lib/kernel/engine.disp` (walker, `param_apply`), `lib/kernel/cells.disp`
  (Pi/telescope recognizers), `lib/kernel/universe.disp` (Type tiers),
  `lib/standalone_kernel.disp` (Pi-checking experiments), `ACTIVE_BUGS.md`,
  `KERNEL_DESIGN.md`, `TYPE_THEORY.typ`.
