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
   concrete values; every declared member must be recognized, and no value on the battery
   may be recognized without being declared. That last clause is the only guard on
   exhaustiveness, which two consumers rely on and which a finite battery cannot witness in
   general; it catches truncation, which is what a partial list looks like in practice.
   A normalizer must be idempotent and preserve membership.
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

### Measured, 2026-08-01

Three experiments settled most of this.

Poisoning raw application of a hypothesis leaves all pins passing, so nothing depends on
that application being inert. The recorded worry about the effect former's gate does not
bite, because that gate's motives are constant. The premise blocking the evaluator fix is
false.

Routing telescope binder instantiation through the tier's own application, the cheap form
of walking type formation, fails twice: the abstract interpreter receives a raw
continuation where it expects a value in its own domain, and the kernel's own annotated
exports stop verifying because the walk refuses legitimate row-construction code. The
cheap form of that fix is dead and the expensive form inherits the interpretation cost.

The deferred, lazy form of the evaluator fix works. A hypothesis stops storing its type
and becomes a record of what has been observed of it; applying one records the observation
and the type is derived on demand by replaying that record through the respond face, which
is the same replay the provenance audit already performs. This dissolves the definition
order problem, because the marker needs nothing and only the reader needs the respond face,
which can be tied mutually. Measured: applying a hypothesis now yields a genuine
hypothesis, its derived type is correct, and two applications with different arguments are
finally distinct rather than collapsing to the same tree. Every guard-tier check probed
still behaves correctly.

### Landed (steps 1 and 2 complete)

The lazy form landed, along with the resolution of the conflict described below. Both
tiers now trust a hypothesis by provenance rather than by representation. The abstract
interpreter cannot simulate a marker that does work over an abstract argument, so instead
of simulating it, it recognizes it, the same way it already refuses to simulate equality,
the neutrality reader, projections and eliminator heads. An in-band mark is re-lifted into
its own domain exactly when the mark roots in the session ledger, which is the guard tier's
audit rule. That closes the forgery hole the out-of-band representation was protecting
against, by provenance instead of by construction, and forged and replayed marks stay
refused.

Applying a hypothesis therefore records rather than collapses, codomains mentioning applied
hypotheses are the types that were written, and real function congruence works while its
false form refuses.

The eliminator marker then got the same treatment: on an abstract scrutinee it collects the
case arms to the eliminator's arity and applies the scrutinee to an elimination frame,
needing no access to the respond face because the application records and the type is
derived later. Eliminating an abstract natural now yields a properly typed stuck term
rather than garbage.

With that, the flagship landed on both tiers: a machine-checked proof by induction that
adding zero is the identity. All three pieces of the arc are load-bearing in that one
result. Lazy elimination keeps the motive's endpoint stuck, so the step obligation is the
equation that was written; the step's own reduction turns it into a shared constructor
context around the induction hypothesis, which congruence closes; and the hypothesis in the
ledger discharges the remainder. It is not vacuous: the false statement with the same proof
shape, the attempt without induction, and a junk step are all refused. Cold time fell rather
than rose, because the eliminator no longer walks a hypothesis encoding before failing.

Step 2 followed on the same foundation. A type declares a normalizer and equations over it
compare normal forms, with the endpoints canonicalized once at formation so that the judge,
the ledger and congruence need no quotient awareness of their own. Abstract endpoints are
left alone, since a normalizer that inspects its argument would destructure a hypothesis,
which is the same discipline motives follow. The coherence suite gained the matching
obligation, that a normalizer be idempotent and preserve membership.

What steps 1 and 2 did not include: parameterized path constructors, whose general form
needs matching against the term universe and is a solver, and induction over a quotient,
since the wrapper carries no gate. Both are separable rungs.

### The two-representation hazard, closed

Applying or eliminating a hypothesis used to produce two different trees depending on the
path: eagerly under the walk, lazily raw. Both were correct and both answered the same type,
but this kernel compares by tree identity nearly everywhere, so nothing failed only because
every exercised path happened to compare same-route terms.

The fix was deletion, as predicted. The guard tier's application helper and its collecting
shim were doing work the marker already does, and both are gone; the projection branch of the
respond face likewise applies the mark to its frame instead of minting its own. One thing the
helper added turned out to be real and was kept: an observation the type does not declare is
not a stuck term but an error, and a lazy record has no way to represent "no type", so the
walk still refuses at the point the observation is made. It just demands the type through the
single derivation path rather than calling the respond face itself.

### The conflict this resolved

It collides with the abstract interpreter's architecture, in one specific shape: applying
a function-hypothesis to a value bound inside an eliminator's arm. Applying one to an
ordinary hypothesis is fine, and not applying it is fine.

The cause is that when the interpreter rolls an elimination it reads its arguments back to
raw trees, which converts its own out-of-band hypotheses into in-band marks. Those marks
then sit inside otherwise concrete values. With lazy hypotheses such a mark becomes active
when applied, and the interpreter's verifier cannot accept it, because it only recognizes
marks it minted itself. Not being able to tell a legitimately derived mark from a forged
one is precisely what the out-of-band design existed to avoid needing, so this is a real
architectural interaction rather than an oversight.

Four ways out, in rough order of principle. Teach the interpreter to keep eliminator
arguments as values rather than reading them back, preserving out-of-band purity at the
cost of surgery in its chain handling. Give it the same provenance audit the guard tier
uses, which converges the two tiers on one verification story and is the most principled
and the most work. Have it re-lift marks to its own representation on application, which is
small but reopens the replay hole that the equality ban era closed unless paired with an
audit. Or let the tiers diverge on the marker, which forks the hypothesis representation
and should be rejected.

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

## Step 4: relatedness from declared interfaces

Written before step 3, and now known to be built before it too. An earlier draft said step
3's whole job is to protect this relation. Measurement reversed that: the relation enforces
step 3's forbidden set by itself, so step 3 is a way of stating and exploiting a guarantee
the relation already produces, not a precondition for it. See "What the measurements
changed" below.

### The relation, corrected

An earlier draft of this section said relatedness at a function type is agreement on all
applications. That is the pointwise form and it is wrong. The correct form relates inputs
too:

    R(A → B)(f, g)  =  for all a0, a1.  R(A)(a0, a1)  implies  R(B)(f a0, g a1)

The pointwise version does not force a function to respect its domain's equality, and the
archived investigation measured the consequence: a type quotiented by "is it a leaf"
admitted a licensed replacement of a stem by a fork, which a well-typed observer could
distinguish. Standard setoid and partial-equivalence-relation systems all use the
cross-related form for exactly this reason.

Membership is then self-relatedness: `v : A` means `R(A)(v, v)`. That is worth taking
seriously rather than treating as a curiosity, because it collapses the membership face and
the relation face into one, and it is what makes respect automatic: an inhabitant of a
function type is by definition something that maps related inputs to related outputs.

### How the relation reaches a type

By a `rel` slot receiving the tier's relatedness judge, mirroring the judge-aware membership
face exactly:

    rel := {jrel, T, x, y} -> ...

It has to be a dictionary rather than a plain predicate for the same two reasons membership
did. Only the tier knows how to relate hypotheses, and only the tier holds the ledger.

### Where the two related hypotheses come from

The natural construction reuses what the equality bridge already built. At a function type,
mint two hypotheses and record their relatedness as a ledger assumption. The ledger already
holds equations and discharges them, and congruence already propagates them, so a
relatedness assumption is an ordinary ledger entry rather than new machinery. This is the
main reason to expect step 4 to be smaller than it looks.

### Termination

Relatedness follows the type's declared observation program, so it terminates wherever
membership already does. Codata and other infinite structures need a guard; version one
should refuse them explicitly rather than loop, and say so in the refusal.

### What subsumes what

Three existing slots become presentations of this one relation. The canonical form from step
2 is the decidable presentation, `R(x, y) := norm x = norm y`. The judge-aware membership
face is the diagonal. The endpoint slot is a stored relatedness claim. The endpoint of the
design is a single relation slot with those as special cases, and the migration is mechanical
once `rel` exists, but it should be done deliberately and not as a side effect.

### Equivalence of representations

`Equiv A B` is two translations plus round-trip obligations stated up to the two relations,
not up to structural identity. Since a translation between observation vocabularies is a
handler, this is the handler pair the plan's opening section describes. Its obligations are
that each translation respects the relations, and that both composites are related to the
identity. That is the structure identity principle in the form this kernel can state.

### Cost and risk

Relating at a function type needs two mints per binder rather than one, so checking cost
grows in the binder count. The bigger risk is that self-relatedness as membership is a
genuine reinterpretation of every existing type, so it should arrive as an additional slot
that coexists with the membership face, with the unification done later and separately.

### What the measurements changed, 2026-08-01

Three things, all pinned in `kernel.test.disp`.

**The relation cannot be a type; it has to be a judgment.** The obvious spelling of respect
is a proposition, `Fn A (a -> Fn A (b -> Fn (Eq A a b) (_ -> Eq B (f a) (f b))))`, and it
does not work. Codomains are instantiated by raw application, and raw structural comparison
answers natively rather than going stuck, so for an inspecting `f` both endpoints reduce to
the same concrete value and the obligation becomes `Eq Nat 1 1`. It is vacuous for exactly
the class of function it exists to catch. This is the same defect as the dependent-codomain
item below, and it is the measured reason the relation needs the tier's dictionary rather
than an argument from taste.

The judgment form works and is small: mint the pair jointly with its relatedness as a ledger
assumption, apply both candidates through the tier's own walk, then ask the tier's equality.
It decides the quotient case in both directions.

**Step 3 falls out of step 4 rather than preceding it.** The judgment refuses a structural
comparator on its own, with no annotation and no forbidden-set check: comparing a hypothesis
produces a stuck comparison whose operand is that hypothesis, and two such comparisons over
two distinct related hypotheses are not relatable by congruence. So the relation already
declines every function that inspects. It is conservative, since it also declines comparators
that would be harmless at a non-quotient type, but conservative in the direction step 3 wants.

**The blocker is that the ledger stores untyped equations.** An assumption recorded at a
quotient gets reused at the carrier, so the identity certifies as a parity-respecting map
into the naturals even though 2 and 0 are parity-equal and distinct as naturals. Relatedness
varies with the type and the ledger does not record which type an equation was assumed at.
So the concrete prerequisite for step 4 is to type the ledger entries and make reachability
refuse an equation whose type is coarser than the goal's. That is a change to `eq_pairs`,
`eq_reach` and `eq_holds`, not a new former, and it should be done first.

Two intermediate spellings were tried and both fail, which is worth recording so they are
not retried. Minting all three binders abstractly leaves the obligation vacuous, per the
first point. Minting them concretely lands on pairs where the equation type is empty, and
the tier mints there anyway because it has no ex falso rule for a concretely refuted
equation, so honest functions get refused. Mixing one concrete point with one abstract needs
real computation with the equation, which congruence closure cannot do.

## Step 3: reflection as a tracked capability

### The reframe, twice

An earlier draft said the goal is to move the reflection policy from the checking site into
the type. That is already half true and the draft was aimed at the wrong gap: the tier is
already a parameter of the type, since `Pi Guard A B` names its policy and the general
function former is that partially applied. Three things are actually missing.

The second reframe is from measurement. Step 3 was written as insurance for a future
licensing consumer, with the quotient hole as its justification. It is not insurance. The
first entry in the forbidden set below is already load-bearing for type formation today,
with a live exploit, and that is the part of this step that has to happen regardless of
whether anything ever consumes a respect guarantee. Meanwhile the enforcement half of the
step turns out to be produced by step 4's relation for free, so what remains genuinely
distinct to step 3 is stating the guarantee, composing it, and marking machinery exempt.

First, the guarantee is unstated and unexploited. Nothing anywhere says that an inhabitant
of the strict form respects declared equalities, so no consumer can rely on it.

Second, there is no composition rule. If a strict function is applied to the result of a
permissive one, or a permissive function appears inside a strict one's body, nothing says
what the composite is.

Third, the exemption for kernel machinery is a convention about definition order rather than
anything checkable. Machinery must keep inspecting; that is what makes it machinery.

### The forbidden set, precisely

The guarantee is exactly the absence of these, so the list has to be exact:

- structural comparison with a hypothesis operand, beyond the uniform fragment that the
  three-valued rule already decides
- the neutrality reader applied to a hypothesis
- raw shape inspection of a hypothesis, which is the one non-parametric substrate rule
- forging a hypothesis, which the stem-forge check catches
- in the abstract tier, recognizing a mark, which is now ledger-gated

Deliberately not on the list, because they route rather than inspect: applying a hypothesis,
projecting one, and eliminating one. The existing tiers differ only on the first two entries,
which is why they are three hooks today rather than five.

### A bit or a grade

Version one wants a bit. But the rule dispatch that would carry it is the same site where
discarding and duplicating an argument could be counted, and those are the two rules a
substructural discipline cares about. So the annotation should be designed as a monoid from
the start even if version one only ever uses two elements, because that is the difference
between this step paying for linear types later and blocking them.

### What the kernel can and cannot establish

Enforcement is in-kernel and is the deliverable. The payoff, that inspection-free code
respects every declared equality, is a meta-theorem proved externally by step-indexed
logical relations; the project's sealing note already carries the framework and the
references.

There is a partial in-kernel check available in this kernel's own idiom, and part of it is
now built: behaviorally probe on a battery, the trials-style analogue of the gate
certification. It is a lie detector rather than a proof and is labelled as one.

### The two holes this closes, both measured

**A dependent codomain does not mean what it says, and this one is live.** Codomains and
motives are instantiated by raw application, and the raw tier's structural comparison is the
native primitive, so it answers `false` on a hypothesis rather than going three-valued the
way the walk's does; the neutrality reader answers honestly for the same reason. A codomain
that inspects its bound variable is therefore formed at one branch for the certificate and
the other branch everywhere else. Measured: a codomain reading `Nat` at the hypothesis and
`False` at zero certifies the identity function, whose value at zero does not inhabit the
type that codomain declares there. The neutrality spelling certifies everything at all.

This is the residue of the barrier step 1 hit. Lazy hypotheses made raw application and raw
elimination go stuck, which is why an honest dependent codomain works today; inspection is
the piece that was never reached, and it is the first two entries of the forbidden set
above. So those entries are not insurance for a future consumer. They are what makes a
dependent function type mean its own statement.

Half of it is closed, cheaply, by noticing where the asymmetry actually lives: the
eliminator route was already honest, because a gate instantiates its motive at concrete
constructor points, while a Pi codomain is only ever instantiated at the abstract point. So
give Pi the same treatment. `TwoFace` conjoins a concrete face onto the abstract one,
minting the probe battery filtered by the domain alongside the hypothesis. It only ever adds
obligations, so it can refuse but never admit more, and it catches both lying codomains
while the existing suites pass unchanged under it. It is opt-in, so no existing surface
changes meaning; making it the default is the migration below. It is a finite battery, so it
remains a detector, and the real closure is still the forbidden set.

**A function type over a quotient does not enforce respect.** A function that compares its
argument structurally certifies at `Fn Quotient B` while distinguishing two values the
quotient identifies. Nothing consumes that as respect evidence today, so it is latent rather
than a live unsoundness, but it is exactly what a licensing consumer would misread. Step 4's
relation decides it correctly in both directions once the ledger is typed; this step does
not have to close it separately.

### Migration

Every current annotation means "checked at the guard tier" and would come to mean
"inspection-free", so the whole annotated surface changes meaning at once. Machinery needs
an explicit permissive marking rather than an implicit one.

The concrete face has the same shape of migration in miniature, and it is available now as a
rehearsal: making `TwoFace` the default table changes what every annotated export in the
kernel is asserting, from the abstract face alone to both faces. The cost is the binder-count
blowup, since the battery multiplies per binder, so it wants the filter to be sharper than
"every probe the domain recognizes" before it becomes the default.

### Order of work

1. Type the ledger entries, so an equation carries the type it was assumed at and
   reachability refuses one whose type is coarser than the goal's. Everything below needs it
   and nothing else is blocked on anything.
2. Build the relatedness judgment on the typed ledger. This decides quotient respect and, as
   a side effect, declines every inspecting function.
3. Then state the guarantee, the composition rule, and the machinery exemption, which is what
   is genuinely left of this step once the relation is doing the enforcing.

## What this does not include

Univalence and proof-relevant higher structure. Reaching them means adding an interval and
composition structure on top of step 4, and the honest position is that the licensing
problem this plan serves does not need them. Revisit only if a goal appears that genuinely
requires paths between paths.
