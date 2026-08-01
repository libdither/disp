# Equality: where it stands and where it goes

The question this plan answers: what would it take for disp to relate two programs, or
two data representations, as equal, well enough to license an optimizer's rewrites and
rich enough to eventually express spaces of isomorphisms rather than just pointwise
agreement.

The investigation that preceded this plan is `archive/OEQ_INVESTIGATION.md`. Its findings
still hold and are summarized where they bear on a step; it is archived because it
describes the live kernel's `oeq` layer, while the work below happens in the standalone
kernel, which has since grown the machinery the investigation said was missing.

## The constraint that shapes everything

In any layer where values are trees, structural comparison appears in motives, and
equality supports transport, propositional equality is forced to be structural identity.

The proof is two lines. Assume function extensionality gives `p : Eq _ id shift`, where
`shift` agrees with `id` on every concrete input but is a different tree. Transport `p`
along the motive `P f := Eq Bool (tree_eq f id) true`. The input is the canonical proof,
since `tree_eq id id` computes to true. The output inhabits `Eq Bool false true`, which
is empty. The same argument at the type level refutes univalence.

So this is not a design failure to be engineered around. Every extensionality principle,
including plain function extensionality, is inconsistent with unrestricted structural
comparison. The investigation reached the same place from the contextual-equivalence
side: unrestricted contextual equivalence collapses to structural identity, so the
equality an optimizer needs must be observer-restricted.

The consequence for planning: extensional equality cannot be added to the fragment we
have. It has to live in a fragment where structural inspection is unavailable, and the
value of steps 1 and 2 below is partly that they are useful on their own and partly that
they are the parts that do not require that fragment to exist yet.

## Where the identity of a type lives

A type here is a function in two different senses, and the distinction decides what
extensionality buys.

The membership face answers which trees inhabit the type. Extensional equality of that
face identifies types with the same inhabitants. This is not what an optimizer wants:
unary and binary naturals have different inhabitants, so it identifies nothing useful.

The interface face is the observation program the type declares (its telescope, its
observation rows, its gate). Extensional equality of that face says two types are the
same when they answer the same observations, and equivalence between representations
becomes an invertible translation between observation vocabularies. A translation between
effect vocabularies is a handler, so representation-equivalence is a pair of mutually
inverse handlers, which is a bisimulation.

These two faces are already separate in the kernel and the separation is load-bearing.
The plan's destination is that identity in the extensional fragment is interface
identity, with the membership face demoted to a realizability relation recording which
trees implement the interface.

Stated as one sentence: equality is agreement on declared observations. Function
extensionality, quotients, set-level higher inductive types, and representation
equivalence are all instances, which is why they are not four separate features below.

## The ceiling, stated honestly

The inspection-free fragment, with types carrying declared equalities, is types as
partial equivalence relations over tree calculus viewed as a partial combinatory algebra.
That is the realizability setting, and it is a genuine topos. It hands over function
extensionality, propositional extensionality, quotients, and an impredicative universe of
propositions as properties of the model rather than axioms needing justification.

It is also 0-truncated. A topos validates uniqueness of identity proofs, which
contradicts univalence, so this destination is a plateau below homotopy type theory
rather than a step toward it. Realizability models of univalence exist (cubical
assemblies) but require cubical machinery rather than falling out of the relational
structure.

Whether that ceiling matters depends on a question worth settling before spending the
budget: an optimizer's transport is a program rewrite, not a term-level transport, and
the evidence it needs is a simulation or logical relation. Representation independence at
set level, supported by parametricity and the structure identity principle, is very
likely sufficient. Full univalence is a stronger tool than the licensing problem
requires.

## Step 0: hardening

Not part of the ladder, but sequenced first. Three trusted declarations were added in one
week (the judge-aware membership face, the elimination landing slot, the endpoint slot)
and the probe family did not grow. Every step below rests on declarations being honest,
and step 2 adds a fourth.

1. Propagate the judge-aware membership face to the remaining formers. Lists, records,
   effects, refinements and named types still apply component types raw, so the
   element-position behavior is closed for declared sums and open everywhere else.
2. Derive both membership faces from one declaration inside the formers. Declared sums
   already do this; nothing else does. Prefer this to writing a probe, since it removes
   the failure mode rather than detecting it.
3. Grow the gate certification into a coherence suite: endpoints that are concrete and
   unequal must recognize nothing; the two membership faces must agree on a battery of
   concrete values; every declared member must be recognized.
4. Extend the sum sugar to multi-slot variants, so declarations with several payload
   positions stop being written as explicit variant lists.

## Step 1: congruence closure

Equality reachability currently matches whole endpoints. It becomes a congruence closure
over the subterm universe of the ledger equations plus the query: collect subterms, merge
classes for each known equation, propagate the rule that structurally aligned terms with
merged components merge, iterate to a fixpoint. Termination is structural, so the
existing fuel-by-length pattern carries over, and the surface of `eq_holds` does not
change.

The soundness line: building up is always sound, so from `a = b` derive `succ a = succ b`.
Tearing down is sound only for constructor-headed terms, where trees are free, and never
for stuck applications of abstract functions. The distinction is easy to enforce because a
stuck application is a hypothesis with an observation history, not a fork.

What it unlocks is equations between constructor spines: from a proof that two values are
equal, an equation between any shared constructor context around them follows. That part
is done and pinned on both walking tiers.

### The barrier this step uncovered

The intended flagship, a proof by induction that adding zero is the identity, does not
reach congruence at all. It is blocked earlier, by type formation.

A dependent codomain or motive that computes on its bound variable is built by ordinary
evaluation, and ordinary evaluation mishandles a hypothesis in two ways. Applying one
saturates the hypothesis marker and yields its stored payload record, so `f n` and
`f (succ m)` evaluate to the same tree and a codomain mentioning them collapses to
something trivially true. Eliminating one destructures the hypothesis encoding, so
`add k zero` becomes garbage rather than a stuck term. Both are measured, and both
predate this plan: they are the documented raw-tier behaviors, one recorded as
"raw application saturates the marker, inert" and the other as "the raw tier stays
encoding-transparent by design".

Individually they were harmless, because every motive used so far was constant or a bare
constructor application. Together they mean two things. Every proof that needs computation
in a motive is unreachable, which is most of what an equality layer is for. And worse, a
type whose formation touches a hypothesis silently becomes a different type, so a
certificate can look like it proves something it does not.

Two fixes are on the table and both are architectural.

The first makes evaluation itself hypothesis-aware: the eliminator marker and the
hypothesis marker produce stuck values instead of garbage and payload-saturation. This is
the normalization-by-evaluation architecture, where the evaluator produces neutrals and the
checker never has to intervene, and it would likely subsume the per-tier machinery that
exists to do this today (the guard tier's shim and the abstract interpreter's chain state).
The risks are that it changes pinned raw-tier semantics, and that marker saturation is
load-bearing elsewhere: the effect former's gate relies on applying a hypothesis
continuation being inert.

The second walks type formation: telescope continuations are instantiated through the
tier's own application rather than raw, which is one new operation in the checking
vocabulary. It is more local for the checking side, but it does not reach the shared
respond face, which also applies motives raw and is deliberately tier-agnostic, and it puts
type-forming code under interpretation, which is the cost the live kernel's token
mechanism exists to bound.

Choosing between them is the next decision, and it should be made deliberately rather than
discovered, because it determines whether the equality layer is built on an evaluator that
respects hypotheses or on a checker that compensates for one that does not.

## Step 2: quotients and set-level higher inductive types

A higher inductive type's inhabitants are still trees; its path constructors are equations
imposed on them. At set level that is a quotient inductive type: a declaration plus
equations.

Path constructors are parameterized, so the equations are not ground pairs. Two
implementations follow. The general one stores equations as telescope-and-body
declarations and instantiates them by matching against terms in the universe, which is a
real solver. The cheap one declares a canonical-form function instead and defines equality
at the type as structural equality of normal forms.

Build the cheap one first. It composes directly with three-valued comparison, since
uniformity generalizes from "over the member list" to "over the declared equality", which
makes comparison quotient-respecting with no ban and no new mechanism. It also matches how
the kernel already works: declare, then probe. Eliminator coherence becomes the
requirement that the eliminator factors through the canonical form, which is a probe of
the kind step 0 item 3 establishes.

The risk is that a wrong normalizer silently identifies distinct things, which is why the
coherence suite comes first.

## Step 3: reflection as a tracked capability

Reflection policy currently lives in three hooks of the guard-family walker and is selected
from outside by which table a check runs at. The target is that the policy belongs to the
function type, so a term either lives in the fragment where structural inspection is
available or in the fragment where it is not.

Mechanically the walker's inspection carve-outs stop answering inline and perform an
operation; the tables become handlers for it, and the existing tiers are already three
distinct policies for exactly this (refuse, answer honestly, answer uniformly); the
function formers gain a row so annotations can name their fragment.

The kernel can enforce the discipline but cannot prove the payoff about itself. That
inspection-free code respects every declared equality is a meta-theorem, proved externally
by step-indexed logical relations; the project's sealing note already identifies the
framework (dependency-category noninterference) and the reference. What the kernel
delivers is enforcement, which is the difference between hoping nothing inspects and making
inspection unavailable, and that difference is what makes the extensional principles safe
to add rather than contradictory.

This formalizes a convention the kernel already follows informally, where machinery sits
above the trust line and certified surface below it. Machinery is exactly the code that
must keep inspecting. The migration cost is real: every current annotation implicitly
means "checked at the guard tier", and afterwards it would mean "inspection-free".

## Step 4: equality from declared interfaces

Define relatedness at a type by running that type's observation program on both values and
comparing answers. For function types the declared observation is application at a fresh
point, so agreement on all applications is the definition and function extensionality
becomes a consequence rather than an axiom. Products go componentwise, sums require the
same tag and related payloads, quotients use the declared relation.

Equality is then derived: the equality type accepts its canonical proof exactly when the
values are related. Type equivalence is a pair of interface translations agreeing on
observations, which is to say a handler pair.

Most of the machinery exists. Relatedness on functions quantifies over arguments, which is
the mint operation, so the relation is itself an effect program run at a table, the same
shape as the judge-aware membership face. This step is largely a new judge route and a
redefinition, and it is close to worthless before step 3, because in a fragment that can
inspect trees, observationally equal values remain distinguishable.

## What this does not include

Univalence and proof-relevant higher structure. Reaching them means adding an interval and
composition structure on top of step 4, and the honest position is that the licensing
problem this plan serves does not need them. Revisit only if a goal appears that genuinely
requires paths between paths.
