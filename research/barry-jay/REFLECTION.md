# Reflection after a full read: Jay & Bader, *Simple Types for Polymorphic Functions*

Companion to `COMBINATORY_TYPES.md`, which is the reference write-up of the system and its
mapping onto disp. This file is narrower: it records what a complete read of the paper
changed, answers the questions I had after a partial read, and states the critiques the
reference doc does not make. Sections marked (merge) are worth folding into
`COMBINATORY_TYPES.md` later; the rest is scaffolding that can be deleted.

## 1. The reframe: this is abstract interpretation, and Jay says so

The single sentence that reorganises the whole paper is §12.3 plus the aside in §1: "the
layers of abstract types could be used to support different layers of abstract
interpretation."

Read that way, everything lines up:

- **Combinatory types are the concrete domain.** The abstraction map is the identity.
  `|p|` is a bijection on normal forms, so a core type is the value's exact shape. Zero
  information lost, and correspondingly zero termination guarantee: type application
  diverges exactly when the term does (`(SII)(SII)`). Calling this a "type system" is
  generous; it is a partial evaluator that happens to be written over shapes.
- **Abstract types are where abstraction actually happens.** `Bool = Abs0{S1 K0}` is the
  first genuinely lossy step in the paper: two distinct values collapse to one type.
  Everything a type system is normally for (accepting `successor zero`, rejecting
  `successor tt`) lives in this layer, not the core.
- **`Rec{F}` is the widening operator.** §1: "By abstracting away internal structure,
  recursive functions can preserve their type across iterations." Without an abstract
  recursion type the type would grow without bound at each unfolding. That is precisely
  what widening is for.
- **The precision/tractability trade is named out loud.** §6.1: "Although function types
  hide many details they also risk eliminating polymorphism." `cond` has a huge structural
  type that is fully polymorphic; `cond_mono : Bool → U → U → U` is small and monomorphic.
  Abstraction loses information. That is the point, not a wart.

So the honest reading of the contribution is: *a lattice of abstract domains over
normal-form SK terms, where the concrete domain is exact and each declaration adds a
coarser domain with its own transfer functions.* Types are the demo application.

## 2. Answers to the questions I had

**Is this a type system or an abstract interpreter?** The latter, layered so that the top
layers behave like the former. See §1 above.

**What is the "summary system" of §9?** Not what I guessed. It is a summary in the mundane
sense: Figure 5 collects every type-application branch introduced across §§5 through 8 into
one omnibus `match`, "for the sake of completeness." There is no separate summarising or
collapsing mechanism. My guess that it was the size-control device was wrong.

Where does size control actually come from, then? From the abstract types, which do triple
duty: nominal identity, termination, and compression. In the core layer types are literally
the same size as terms (§10: "term and type have the same size" for `SSSS…`), and
`successor1000 zero` has size 38020 in Table 1. `Bool` is one node regardless of how big
`tt`'s SK expansion is.

**Is the subtyping relation `<` decidable?** The question dissolves. Subtyping exists only
in §3's hybrid system, which is a stepping stone the paper discards: "by replacing subtyping
with type applications such as `S0(K0) = S1 K0` it will be enough to support the combinatory
types alone." The final system (Figure 5) has no subtyping at all. Worth noting anyway that
Figure 1 includes transitivity with an undetermined middle type (`U < W if U < V and V < W`),
the usual undecidability smell, which may be part of why they moved on, though the paper's
stated reason is just simplification.

**How does tagging capture intensionality?** `tagged{f,t} = S(S(KK)f)(tag(Kt))` preserves
functionality (Theorem 2.1: `tagged{f,t} u ⟶ f u`) while changing the shape, and shape is
the type. Theorem 2.2 (`tagged_not_star`) says a tagged term is never a star-abstraction;
that separation is what keeps type application single-valued. The S1 restriction enforces
it: `S1 U (V) = S2 U V` fires only if `S2 U V` is not a tagged type, so tagged terms fall
through to declaration-specific rules or have no type at all.

Why he wants this: §5.3, "System F types are not intensional, in that all two-values types
are identified." He wants nominal distinctions between isomorphic encodings, obtained
structurally rather than by fiat. Only an intensional calculus can do this; λ-calculus
cannot tell `λxy.x` used as `tt` from `λxy.x` used as `K`.

**Recursion.** `Z{f} x ⟶ f (Z{f}) x` built from `wait2` and double tagging so that `Z{f}`
is a normal form. `Rec{F} = Abs1{K0} F`, elimination conditional on
`F (V∗U → V)(V∗U) = V`. The `V∗U` rather than `U` is another dummy-value hack, "to ensure
that type application is functional." The paper admits `Rec` gets no declaration at all
because "it is not clear how to represent a conditional elimination rule", so the
declaration language is incomplete and `Rec` is hand-rolled. The "special cases for Z" in
the inference algorithm are about not descending into the `wait2`/tag machinery
structurally; the side condition itself is determined, not searched, since `V1` comes from
the argument type.

**SK versus tree calculus.** Confirmed stepping stone. §12.4: the plan is to port this to
tree calculus, then "internalise the types and type-level computations as terms and
intensional programs, and then blend them in a system of dependent types." His roadmap
terminates roughly where disp already stands.

## 3. Three critiques the reference doc does not make (merge)

**The dummy-value tax is systemic.** Dummies appear in sums (`inl` needs a `dV`), function
types (`lam x t d`), list `nil`, and `Rec`'s `V∗U`. The pattern is exact: wherever the
result type is not determined by the argument's shape, a *runtime value* is inserted to
carry the missing type information. This is System F's type application re-encoded as term
application, with an inhabitant standing in for the type. Three consequences the paper
underplays: types must be inhabited (it calls this "not a practical difficulty" but it
"complicates the theory"), programs carry runtime junk that a compiler would have to erase,
and there can be no empty types. That last one is a hard wall for anyone wanting to read
this as a logic. Jay is not doing logic, so it costs him nothing, but it is the single
biggest reason the system cannot be lifted into a proof assistant as-is.

**"No changes to the term language" is true at the BNF and misleading in practice.** The
grammar stays `S | K | MN`, yes. But constructors must be tagged, λ must be star-abstraction
with no η-contraction (§2.2: with η, "all constructors c would also be abstractions"), and
dummies must be threaded through. The term language is unchanged; the *programs* are
typing-shaped. This matters because the headline pitch in §12.3 is "no need to encode
programs as syntax trees before analysing them", yet their own most realistic experiment
undercuts it: the toy compiler is "completely oblivious to our data types and tagging
approach", so "infer_app wrangles nothing but combinatory types." Oblivious code typechecks
but gains nothing from the abstract layer, meaning it gets no abstraction at all. To
benefit, you write in their idiom. That is an encoding, just relocated from the analysis to
the source program.

**Uniqueness is a maintained invariant, not a structural theorem.** Theorem 9.1 (at most
one type) is proven in Rocq for the system as it stands. But all the interesting content
comes from declarations that add branches to a partial function, and every branch is a
chance to make it non-functional. The guards are ad hoc and hand-placed: the S1 restriction,
`tagged_not_star`, dummy values, `V∗U` in `Rec`. There is no general criterion for "when is
a new declaration safe", which is exactly what you would want before believing this scales.
§12.2 (typing modules) would stress it immediately.

Minor: §10 says the calls-to-size ratio stays "below 3 for all tested examples", but their
own Table 1 lists `S^10` at 6.36 and `S^100` at 73.76. They caveat these as unrealistic in
the next sentence, so it is loose phrasing rather than a wrong claim, but the reference
doc's repetition of "below 3 for all tested examples" inherits the looseness.

## 4. What actually motivates the paper (correction, merge)

I had previously read the emphasis on decidable inference as being about annotation burden.
That is wrong. §1 is explicit that inference is a demonstration: "To illustrate the
possibilities, we develop an effective type inference algorithm." The goal stated in §12.3
and §13 is *static program analysis without encodings*. Because every computable function
has a normal form in combinatory logic, programs already are the trees an analysis wants to
walk, so analyses need no separate syntax representation and no separation of syntax from
semantics. Types are the first and simplest such analysis, and effective inference is the
evidence that the framework is tractable rather than the goal.

This also explains why he tolerates a system whose concrete domain diverges on `(SII)(SII)`.
An abstract interpreter is allowed to be a partial function; a type checker is not supposed
to be. He is building the former.

## 5. What this changes for disp

Mostly it sharpens what `COMBINATORY_TYPES.md` §6 already says, rather than replacing it.

- The "inference island" idea (§6.2 there) survives the full read and gets a sharper
  statement: what you would import is not a type system but an *exact shape analysis* over
  the structural fragment, with declared abstract types as the coarsening layer. Uniqueness
  is the price of inference, and abstraction is the price of tractability. Those are two
  separate trades, and the paper pays both.
- The dummy-value tax is the reason a wholesale import is unattractive. disp has empty types
  and cares about propositions; Jay's encoding of "which type did you mean" as "hand me an
  inhabitant" is incompatible with that at the root, not at the margin.
- The abstract-interpretation framing is a better organising metaphor for disp's own type
  tiers than the one currently in `COMBINATORY_TYPES.md` §6.5, which mentions it only in
  passing. Concrete domain = exact structure, abstract domains = declared recognizers with
  their own transfer rules, widening = whatever keeps recursive types from growing.
- The genuinely portable technique is small and specific: **derived rules as theorems about
  where evaluation lands.** `|cond|(Bool)(U)(U) = U` is proven by computation, and the
  familiar typing rule for `cond` falls out as a corollary. That pattern (state the ordinary
  rule, prove it as a fact about the evaluator rather than assume it as a primitive) is
  directly reusable for recognizer-level lemmas and does not require adopting anything else
  from the paper.

## 6. Still open

- Conjecture 3.1 (every HM-typable program has a principal combinatory type) is unproven:
  "the case analyses for the proof have been written out, but we have yet to find the right
  induction principle to combine them." This is the paper's main claimed relationship to HM
  and it is a conjecture.
- No general safety criterion for declarations (see §3 above).
- The PEPM'25 typed tree calculus is paywalled and not in this folder; it is the version
  with quantifiers that this paper claims to improve on, so the comparison is taken on
  trust.
