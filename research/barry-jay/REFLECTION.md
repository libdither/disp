# Reading notes: what the comparison must keep separate

Companion to [COMBINATORY_TYPES.md](COMBINATORY_TYPES.md), revised 2026-09-19 after
reading the full Jay–Bader paper and its bundled OCaml implementation. The comparison
there is the main account. These notes retain the qualifications that are easy to lose
when summarizing it; the previous version contained stronger claims than the sources
justify.

## 1. Exact description, abstraction, and reflection

The bare closed types correspond one-to-one with normal-form SK values. Type application
computes on these descriptions. Abstract types then hide structure behind declared
constructor and elimination rules. This is a useful connection to abstract interpretation,
and the authors suggest it, but the paper does not supply a lattice of all these domains
or prove that its recursive type implements a widening operator. Those were analogies in
the earlier notes, not established results.

Being an abstract interpreter and being a type system are compatible descriptions. It
was misleading to insist that the paper is only the former. Its typing judgment has
proved properties, including preservation and uniqueness, even though inference is partial.

Structural analysis is performed by an external checker in this paper. General inspection
of program structure from inside the language belongs to the tree-calculus work and the
proposed extension. SK tags distinguish terms for the checker; they do not give arbitrary
SK programs a tree-inspection primitive. Internalizing types and type-level computation
is explicitly future work. [Jay & Bader, §12.4](https://arxiv.org/html/2604.12194v1#S12.SS4.p1.1.2).

## 2. Uniqueness is a design choice, not the price of inference

In a fixed context the final system assigns at most one type. Maintaining that property
explains the reserved shapes for tagged constructors and the extra information carried
by some constructor arguments. It helps make inference syntax-directed.

It does not establish that all systems with inference need exclusive typing. Hindley–Milner
infers a principal scheme representing many typings. Bidirectional systems can synthesize
an interface while permitting checks against other types. Nor does uniqueness establish
termination: the paper's type-application computation can diverge.
[Damas & Milner, p. 2](https://steshaw.org/hm/milner-damas.pdf#page=2);
[Dunfield & Krishnaswami, §4.6.1](https://www.cl.cam.ac.uk/~nk480/bidir-survey.pdf#page=15).

Disp can retain overlapping predicates while selecting an interface for inference,
normalization, implicit operations, or compiled representation. The additional obligation
is that behavior-changing choices be explicit or coherent, not that values have only one
valid membership.

## 3. Inhabitants carry information, with limited consequences

The sum encoding requires values from both summands even when only one is selected.
The empty-list constructor needs an inhabitant of its element type, and arrow introduction
needs one of its domain. This follows from the paper's choice to transmit the missing
type information through ordinary terms. It is a real restriction on these interfaces.

It does not prove that every type in the system is inhabited or that an extension cannot
have empty types. The earlier claim that there can be no empty types was too strong.
Likewise, a dummy is part of the term representation, but its precise runtime cost or
safe erasure depends on evaluation and compilation; the paper does not establish a
universal overhead bound or an erasure theorem.

## 4. Termination, reduction, and inference are separate

The bare example of divergent self-application demonstrates divergent inference. It is
not a decision procedure for whether arbitrary terms have normal forms. Inference requires
types for both operands even when reduction can discard an operand. Reduction preserves
an existing typing; the reverse implication need not hold.

The full abstract system types recursive computations that need not terminate. Their
recursive-call arguments are retagged with an arrow interface: omitting that wrapper from
the reduction formula hides an essential typing step. Naturals and lists expose one
constructor layer; recursion and folding are defined separately.

An inference cutoff ensures the attempt stops but may reject a typable term. Similarly,
a candidate-by-candidate inhabitation search must dovetail its attempts or use increasing
budgets, otherwise a divergent check can block it forever. Turing-completeness alone does
not prove that the inhabitation problem is undecidable.

The performance table supports small inference costs on the ordinary examples, not a
general linear bound. Its repeated-S examples explicitly exceed the prose claim that
all tested ratios stay below three. [Jay & Bader, §10](https://arxiv.org/html/2604.12194v1#S10.p1.1).

## 5. What remains unresolved

The hybrid-system correspondence is still Conjecture 3.1. The introduction's broad
comparison with HM should be read alongside that qualification.

Declarations describe how new abstract types should acquire application rules, but their
translation is not automated. The recursive type is supplied by explicit rules rather
than a declaration, and the paper identifies its conditional elimination rule as an
unresolved issue. The proved properties of the presented system must be re-established
or covered by an extension theorem when rules change.
[Jay & Bader, §6.2](https://arxiv.org/html/2604.12194v1#S6.SS2.p3.m1).

For disp, a structural inference layer remains a possible design, not a proved speedup or
a required repair. The most directly reusable method is proving convenient analysis rules
against the underlying computation. The main comparison now links the current kernel
and distinguishes guarded checking from finite sampling; the former July file paths and
blanket claims of machine-checked soundness should not be carried forward.
