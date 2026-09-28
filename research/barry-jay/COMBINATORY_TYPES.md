# Barry Jay's Combinatory Types, and What They Would Mean for disp

A reading of Jay & Bader's *Simple Types for Polymorphic Functions*: inference evaluates
a type-application operation over structural and abstract types. The comparison with disp
distinguishes unique typing, principal types, and deterministic inference. Written 2026-07;
comparison and technical corrections revised 2026-09-19 against the paper and current code.

## This folder

- `simple-types-for-polymorphic-functions-2604.12194.pdf` — Jay & Bader, arXiv:2604.12194
  (April 2026). The main subject of this document. The combinatory type system.
- `simple-types-for-polymorphic-functions.txt` — same paper as plain text, for grepping.
- `reflective-programs-in-tree-calculus-tree_book.pdf` — Jay's tree calculus book (2021+,
  appendix with Jose Vergara). Background for the substrate the types are meant to move to.
- `ocaml-implementation/` — Bader's OCaml type inference for the paper
  (github.com/olydis/combinatory-types, archived on Zenodo as 10.5281/zenodo.15338381).
  `lib/infer.ml` implements both inference and type application; `lib/types.ml` defines
  type representations and structural helpers; `lib/iota.dag` is an example program.

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
bracket/star abstraction. Bracket abstraction produces normal forms; the optimized star
abstraction can retain closed subterms, so its normality requires those to be normal.
Two gadgets matter:

- `wait{M,N} = S(S(KM)(KN))I` — keeps M and N apart until applied (`wait{M,N} P ⟶ M N P`).
  Used to build normal-form fixpoints: `Z{f}` waits for an argument, so it is normal if f is.
- `tagged{f,t} = S(S(KK)f)(tag (K t))` with `tag = S(S(KK)(KK))` — wraps f so that
  `tagged{f,t} u ⟶ f u` (functionality preserved) but the term is structurally marked by t.
  Theorem `tagged_not_star`: a tagged term is never the output of the specified
  star-abstraction algorithm. This separates constructor representations from those
  abstraction outputs; it does not mean constructors lack functional behavior.

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

The S2 rule can recurse indefinitely. In the bare system, applying exact structural types
tracks computation on their corresponding values. This is not a characterization of
whether an arbitrary term has a normal form under some reduction strategy: inference
also requires types for both subterms of every application. The paper proves reduction
preservation, uniqueness in a fixed context, and inference correctness for its rules.

### 3.3 Worked examples (verified by hand against the rules above)

**A type has many terms.** `K(SKK) S : S2 K0 K0`:

```
K0 (I0) = K1 I0            where I0 = S2 K0 K0
K1 I0 (S0) = I0
```

Same type as `SKK`. In the bare system, a type determines one closed normal-form value,
but multiple typable terms can reduce to that value. The converse does not follow:
reducing to a typable value need not make the original term typable.

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

The paper's inference algorithm loops on this example too (§10); it does not return a
rejection. Moreover, $K I \Omega$ reduces to $I$ by discarding its last argument, but
inference still tries to type $\Omega$. Thus having a normal form is insufficient for
typability. The abstract system below also types potentially nonterminating recursive
computations, so the bare example is not a termination guarantee for the full system.

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
- **Recursion.** The precise reduction is $Z\{f\}\,x \longrightarrow
  f\,(\operatorname{tagged}\{Z\{f\},Kx\})\,x$. The recursive-call argument is retagged
  with an arrow interface; replacing it by the unwrapped recursive function loses a
  typing-relevant step. `Z{f}` is a normal form when f is. `Rec{F} = Abs1{K0} F`, with a
  *conditional* elimination rule:
  `Rec{F} (V∗U) ⇒ V if F(V∗U→V)(V∗U) = V`. The paper does not give Rec a declaration —
  conditional rules don't fit the declaration machinery cleanly yet.
- **Nat.** `Nat = zero | successor of Nat`. Applying a numeral to a pair of handlers
  performs case analysis: the successor handler receives the predecessor, not an already
  folded result. Recursion is supplied separately. Elimination typing:
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

A missing type-application rule gives a finite rejection; divergence gives no answer.
A bounded implementation can stop either computation, but budget exhaustion does not
establish untypability. The paper's running error example is `successor tt`: applying the
successor rule's shape to `Bool` matches nothing, inference fails fast. Positive case:
`zero : Nat`, `successor zero : Nat` (rule fires), etc. So the abstract layer does the
ordinary accept/reject job — with the criterion defined *nominally* (declarations and tags)
on top of the exact structural computation.

### 3.7 Type inference (§10)

Inference looks up variables in a fixed context, assigns the primitive types to S and K,
and recursively infers both operands of an application before calling type application.
The implementation is in [infer.ml](ocaml-implementation/lib/infer.ml); its constructor
recognition helpers are in [types.ml](ocaml-implementation/lib/types.ml). It analyzes term
structure rather than executing the term on a hypothetical argument.

Theorem 10.1 identifies successful inference with derivable typing. The unbounded procedure
finds a type whenever one exists; on an untypable term it may fail or run indefinitely.
The Rocq implementation bounds recursion depth; OCaml's `infer_safe` budgets type
applications, while `infer_measure` runs without that cutoff. A fixed cutoff makes the
implementation terminate but may reject a typable input. These are separate guarantees.
[Jay & Bader, §10](https://arxiv.org/html/2604.12194v1#S10.p1.1).

The reported calls-to-size ratios are small for the ordinary examples, including the toy
compiler, but this is experimental evidence, not a general linear bound. Table 1 includes
ratios 6.36 and 73.76 for repeated-S terms; the paper explains their quadratic growth.
Uniqueness prevents searching among competing types; it does not bound the cost of
computing the unique answer.

### 3.8 Inhabitation (finding a term of a specified type)

In the bare closed system every structural type decodes to its one normal-form value.
Abstract types instead group different values and can express useful synthesis targets.
The paper does not establish a general inhabitation algorithm or an undecidability theorem
for the full system. Turing-completeness alone would not prove inhabitation undecidable.

One possible semidecision procedure is to enumerate finite typing derivations, or dovetail
candidate terms and increasing inference budgets. Running unbounded inference on each
candidate in sequence is insufficient: one divergent attempt could prevent reaching a
later solution. Restricting a search to distinct normal forms removes reduction-equivalent
candidates, but not different programs implementing the same function. These are
observations about possible search procedures, not results proved in the paper.

### 3.9 Unique types are not the same as principal types

| Question | Hindley–Milner | System F | Jay & Bader's final system |
|---|---|---|---|
| What is typed? | Expressions with implicit polymorphic instantiation | Distinguish explicitly typed terms from their erased forms | SK terms, with variable types supplied by a context |
| Is there only one valid type? | No; one principal scheme covers its instances | Explicitly typed terms have unique types in the standard presentation; erased terms need not | At most one type in a fixed context |
| How is type information found? | Algorithm W computes a principal scheme | Checking explicit terms and reconstructing omitted types are different problems; unrestricted reconstruction is undecidable | Infer operand types, then evaluate type application; this may diverge |
| Where does polymorphism live? | Quantified schemes and instantiation | Type abstraction and explicit type application | Detailed type structure whose application can work for many argument types |
| What information can be hidden? | Declared interfaces and data types | Quantified interfaces and encodings | Tagged abstract types with their own application rules |

A principal scheme is not the only valid type: every other valid scheme is an instance
of it in the Damas–Milner result. This directly refutes the claim that multiple typings
make inference impossible. [Damas & Milner, p. 2](https://steshaw.org/hm/milner-damas.pdf#page=2).

Likewise, selecting a type during synthesis need not exclude checking the expression at
other types. Bidirectional systems separate these judgments; intersections make their
relationship particularly clear. [Dunfield & Krishnaswami, §4.6.1](https://www.cl.cam.ac.uk/~nk480/bidir-survey.pdf#page=15).

The self-application example in §3.3 demonstrates avoiding HM's let-generalization
machinery, not a computation that HM cannot express. The paper also demonstrates storing
polymorphic programs together; its general correspondence with the hybrid HM system
remains Conjecture 3.1. Avoid treating this as a proved blanket inclusion of one language
in another.

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

## 5. Comparison with disp

### 5.1 Structural descriptions and predicate membership

Jay's types describe programs using a prescribed type grammar and type-application
operation. In the bare system the description is exact; abstract types intentionally hide
structure. The application rule computes a result description from the two operand
descriptions. The final system maintains at most one type for a term in a fixed context.
[Jay & Bader, introduction](https://arxiv.org/html/2604.12194v1#S1.p1.m7).

Disp instead lets a supplied recognizer judge a value. Several recognizers can accept the
same value without competing to be its exclusive type. That is a useful way to express
refinements and multiple interfaces. It is not an obstacle to implementing an algorithm
that selects a useful description as well.

The systems do not already share a single in-language implementation of typing. Jay's
paper distinguishes term syntax from type syntax; its checker is implemented externally
in Rocq and OCaml. Internalizing types and analysis in tree calculus is future work.
Disp's recognizers and checking machinery are already disp programs. Nor does bare SK
provide disp's general operations for inspecting trees: structural analysis by the
external checker is different from reflection available to the checked program.
[Jay & Bader, §12.4](https://arxiv.org/html/2604.12194v1#S12.SS4.p1.1.2).

### 5.2 What the current code actually provides

| Aspect | Jay & Bader | Current disp |
|---|---|---|
| Type representation | Structural forms plus labeled abstract forms | Callable recognizers, optionally carrying checking and observation metadata |
| Application analysis | Apply the inferred function type to the inferred argument type | A chosen function recognizer checks behavior through a chosen walker |
| Open terms | Variables obtain types from a context | Guarded checking uses fresh typed placeholders and controls their observations |
| Multiple memberships | Excluded in the final typing judgment | Intentional: refinements, singleton types, and other predicates may overlap |
| Information hiding | Tagged terms receive an abstract type in place of their structural type | Recognizers and declared interfaces can expose selected properties without exclusive membership |
| General inference | Partial inference procedure for the specified rules | No general reconstruction procedure in the current elaborator; synthesis of selected interfaces remains possible |
| Guarantees | Paper proves preservation, uniqueness, and inference correctness for its system | Guarantees depend on the selected recognizer/walker; library checks are not by themselves a metatheoretic soundness proof |

The implementation references are [kernel.disp](../../lib/kernel/kernel.disp) and
[types.disp](../../lib/kernel/types.disp):

- `Pred` wraps a recognizer; `Refine` combines base membership with another predicate.
  `Point` recognizes exactly one tree. Thus the concrete value `3` belongs to `Nat`,
  `Tree`, and `Point 3`. This is ordinary overlapping membership.
- `Pi` takes a checking table as well as a domain and codomain family. `Fn` supplies
  `DefaultWalker`, whose default is `Guard`. `Sampled` uses supplied examples instead;
  acceptance under that table must not be described as a universal proof.
- `Isect` implements a dependent intersection through the same table-based machinery.
  Any uniformity claim must account for the actual walker's observation restrictions;
  the presence of an intersection constructor alone is not a parametricity theorem.
- `ShallowType` recognizes the type wrapper. `Space` additionally requires structure that
  the checker can use. `Coherent` checks several agreements using finite probes and
  other checks; its name does not establish arbitrary semantic coherence.
- `Named` gives a recognizer a distinct name while delegating membership to its underlying
  type. It does not force its values to lose membership in differently named types.
  Equating this with Jay's exclusive tagged-constructor typing would be misleading.
- `Quotient` carries a normalizer and `canon` consults the selected type. Therefore even
  when raw trees have structural identity, the meaning of a typed equality can depend
  on the chosen type. Structural identity is not a decision procedure for equivalence
  of arbitrary predicates.

The [elaborator](../../src/elab/driver.ts) records declared type information and delegates
module checking to the in-language `check_module` hook (`Record` in the current kernel).
That is an implementation choice, not a proof that disp cannot support inference.

### 5.3 What uniqueness buys, and what it costs

Three properties must stay separate:

1. **Unique typing:** all derivations for the same term in the same context give the same
   type, allowing whatever equality the system specifies.
2. **Principal typing:** there is a best type or scheme representing the other valid
   typings through subtyping or instantiation.
3. **Deterministic synthesis:** an algorithm chooses one type. Its answer need not be the
   only valid type, or even a principal one.

Jay supplies the first property together with syntax-directed inference rules. Abstract
constructor cases reserve particular structural shapes, so inference need not choose
between retaining an exact structural type and assigning an abstract one.
[Jay & Bader, §5](https://arxiv.org/html/2604.12194v1#S5.p5.1).

This simplicity has a concrete price. Where a result type would otherwise be ambiguous,
these encodings pass dummy inhabitants: the unused side of a sum, the element type of an
empty list, or a function's domain. Consequently those constructions require inhabitants
that ordinary explicit type arguments would not require. This obstructs their usual use
with empty types. It does not prove that the framework cannot be extended with empty
types, and the dummy-value convention is not a necessary consequence of unique typing
in other systems. [Jay & Bader, §5.4](https://arxiv.org/html/2604.12194v1#S5.SS4.p1.m12).

Uniqueness is attractive because it removes choices from composing interfaces. In other
systems it may hold only for fully annotated internal terms; different annotations can
erase to the same runtime program. Principal schemes and bidirectional checking provide
other ways to control choices. None requires that a runtime value satisfy only one
property.

Dependent types do not themselves force non-unique typing either. A result type may
depend on an argument value while still being uniquely determined by the annotated term
and its context. Moving Jay's system toward dependent types therefore does not entail
adopting disp's overlapping predicates or giving up all inference.

Disp can even construct an exact singleton description for an already evaluated value
using `Point`. Under the set interpretation, any accepting predicate contains that
singleton. But discovering that containment still requires establishing the predicate
on the value. Exact descriptions do not automatically yield small, reusable interfaces,
efficient implication checking, or an inference procedure for unevaluated programs.

### 5.4 Predicates are flexible; arbitrary reasoning remains hard

Allowing a predicate to define membership does not make it a total decision procedure.
Nor does it automatically give the predicate an induction principle, an eliminator, or a
sound rule for reasoning about unknown inputs. Those need separate justification.

For disp the central questions are whether a check terminates, whether its abstract
observations justify the claimed behavior for actual inputs, and what evidence permits
one interface to be used as another. Arbitrary predicate implication and equivalence
cannot be decided in general. A budget limits work but cannot turn exhaustion into a
proof of membership or nonmembership.

Jay's unique typing does not solve these questions in general. Its own inference can
fail to terminate, while its abstract recursive types permit nonterminating programs.
Its formal results apply to the particular rules proved in the paper; disp does not
inherit those results by adopting structural descriptions or tagging.

## 6. What to borrow, and where to require a definite choice

### 6.1 An optional analysis that returns a type

Disp can retain overlapping membership while adding an analysis that returns one useful
interface, perhaps using declared interfaces, known constructors, and proved application
rules. Bidirectional checking is another option: synthesize where sufficient information
is available, and use an expected type elsewhere. This need not be complete for arbitrary
user predicates, and a principal result should be claimed only if proved.

A structural analysis inspired by Jay is one candidate. It would need a defined fragment,
a mapping between its descriptions and disp recognizers, and a soundness argument for
its application rules. SK and disp's tree calculus have different primitive operations;
an embedding of SK alone would not analyze arbitrary disp code.

This is a possible engineering direction, not an implemented feature or a demonstrated
speedup. Exact descriptions can grow large and their computation can diverge. Any proposed
fast path must be checked against the existing membership specification and profiled on
representative workloads before calling it cheaper.

### 6.2 Keep representation and behavior choices explicit

Multiple memberships are harmless when they merely record additional facts. A definite
choice matters when selecting an interface changes what the program does:

- **Implicit operations or conversions:** choose an implementation explicitly, define an
  unambiguous resolution rule, or prove that alternative choices agree observationally.
- **Typed equality and normalization:** choose which type's equality is intended. The
  existing `Quotient`/`canon` design already makes a relevant choice explicit.
- **Specialized compilation and external interfaces:** select a concrete representation
  and calling convention at each boundary, with evidence connecting it to the properties
  used by the program.

These require coherence of the selected behavior, not exclusive membership of the value.
Even deterministic inference alone is insufficient if harmless changes to annotations
can select incompatible behavior silently.

### 6.3 Derive convenient rules from the underlying computation

A particularly reusable idea is the paper's treatment of conditionals: a familiar typing
rule is established by proving how the conditional's type application behaves. Disp can
similarly justify convenient checking or inference rules against its existing recognizers
and walker semantics. The proof obligation must cover the chosen checker; a few accepted
examples, especially under `Sampled`, are not enough.

Abstract interpretation is a useful way to understand the trade between exact structure
and compact interfaces. However, the paper does not construct a lattice of all its types
or establish a widening operator in the technical sense. Its recursive abstract type
hides repeated unfolding; calling that a proved widening algorithm overstates the result.

### 6.4 Limits of the current proposal

The hybrid-system correspondence remains Conjecture 3.1. Declaration translation has
not been automated, and the paper explicitly leaves the declaration of its recursive
abstract type unresolved because of its conditional elimination rule. Extending the
system needs justification that the new rules preserve the required properties; the
existing theorem is not a theorem about every possible extension.
[Jay & Bader, §6.2](https://arxiv.org/html/2604.12194v1#S6.SS2.p3.m1).

The portable design lesson is to separate the properties a value satisfies from the
interface a particular analysis or operation chooses. Disp can adopt a deterministic
choice locally without replacing predicate membership globally.

## 7. Sources and scope

The main source is Jay & Bader, *Simple Types for Polymorphic Functions*, arXiv:2604.12194v1
(April 2026), available as the [local text](simple-types-for-polymorphic-functions.txt)
and [PDF](simple-types-for-polymorphic-functions-2604.12194.pdf). Sections 3, 5, and 6
were checked against the full text and the bundled OCaml implementation. Online passage
links above were fetch-verified with `scripts/verify-source.sh`.

The inference distinction also uses [Damas & Milner, p. 2](https://steshaw.org/hm/milner-damas.pdf#page=2)
and [Dunfield & Krishnaswami, §4.6.1](https://www.cl.cam.ac.uk/~nk480/bidir-survey.pdf#page=15).
The current disp comparison is grounded in the linked source files, not the older kernel
layout described in the original July notes. The lineage and PEPM'25 overview are
background; they are not a fresh verification of those earlier papers.
