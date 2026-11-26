# Jay, "Typing Rules Become Programs in Tree Calculus" (Nov 2026)

A reading of Jay's November 2026 draft and its Rocq development, compared against the
April paper covered in [COMBINATORY_TYPES.md](COMBINATORY_TYPES.md), with a novelty check.
Written 2026-11-05, the day after the repo went up.

## This folder

- `typing-rules-as-programs/` — clone of
  [github.com/barry-jay-personal/typing-rules-as-programs](https://github.com/barry-jay-personal/typing-rules-as-programs)
  at `757afd5` (2 commits, 2026-11-05). ~33k lines of Rocq in 28 files plus
  `paper/typing-rules-become-programs.pdf` (28 pages, "Draft of November 5, 2026", single
  author, not peer reviewed). Still has its own `.git`.
- `typing-rules-become-programs.txt` — the paper as plain text, for grepping.

## 1. What the paper does

The April paper (Jay & Bader, SK combinators) typed programs with an external function:
a separate type grammar (`S0 | S1 U | S2 U V | K0 | K1 U | Abs…`) and a type-application
operation written in Rocq and OCaml. Its §12.4 named the next step: move to tree calculus
and make the types themselves terms. This paper is that step.

- **Types are trees.** There is no type grammar. Any value can serve as a type; type
  application on a bad one just fails.
- **The checker is a program of the calculus.** `type_app` (written `F # U`) and `infer`
  are tree-calculus values, built in Rocq with star abstraction from pattern-matching
  clauses (paper Figure 5). `t : T` means `infer @ t ⟹ Sm T`; failure is `Lf`.
- **Expressions vs values.** Evaluation is a big-step, eager relation from expression trees
  (`Sm v` = "value v", `Fk e1 e2` = "apply") to value trees. So a pending computation is
  itself a tree that can be typed, inspected and patched.
- **Concrete clauses mirror evaluation.** For any label-free ("concrete") program `c`,
  `infer @ c ⟹ Sm c`: the program is its own type and its only value (Theorems 6–7).
  Polymorphism shows up as type application: `I # U ⟹ Sm U` for every `U`.
- **Abstract types come from an "ontology".** A parameter program maps a nametag to an
  *abstractor*. When type application would build a tagged type, it asks the ontology to
  swap the carrier for an eliminator, which forgets how the values were built. A triage
  type (a program that inspects its argument) applied to a tagged type fails, so nothing
  relies on forgotten structure.
- **Soundness is relative to the ontology.** Theorem 3 / 5: if every abstractor is sound
  (by typing, or the weaker "by values"), evaluation preserves typing. There is no
  separate progress theorem, because every application of two values evaluates; the
  only stuck expression is a bare leaf, which typed expressions never contain.
- **Type equality is a program.** `Bool # (A ∗ B) ⟹ Sm A` only if `A` equals `B`, checked by
  the tree program `equal`. SK cannot define this; triage can.
- **Breadth.** One sound ontology (`paper_ontology`) covers booleans, numbers, options,
  sums, lists, length-indexed arrays and matrices (inner dimensions checked, Theorem 25 is
  a refusal), type-directed conditionals (`IsNamed[n]`), generic queries
  (`Query[B]`, e.g. `size : Query[Nat]` reading any value including functions) and
  generic updates acting on data, programs and paused evaluations, plus typed tooling:
  an optimiser, patching, and evaluation snapshots.
- **Self-typing.** `infer : infer` (Theorem 12) when the ontology is concrete.

57 numbered theorems, each a named Rocq Theorem. The README claims no `Admitted`, no axioms,
and `Print Assumptions` closed for each
([README, Checking](https://github.com/barry-jay-personal/typing-rules-as-programs#checking)).

## 2. Changes from the April paper

| | April (Jay & Bader, arXiv 2604.12194) | November (Jay) |
|---|---|---|
| Calculus | SK, rewriting | Tree calculus, eager big-step over expression trees |
| Types | Separate grammar | Any tree; concrete programs are their own types |
| Type application | External (Rocq + OCaml), ~20 fixed branches | A tree program; abstract branches delegated to the ontology |
| Abstract types | Fixed list of rules, tag + "S1 restriction" | Ontology parameter; tagging via composition with a label |
| Soundness | Preservation, uniqueness for the fixed rules | Preservation for any sound ontology, per-abstractor obligations |
| Equality of types | Meta-level | `equal`, a program |
| Function types | Abstract type, central | "An afterthought": one family among many, used for recursion |
| New families | — | Arrays, matrices, conditionals, queries, updates, tooling |
| Inference | Semi-decision (may diverge) | Same; `ω @ ω` has no type because typing diverges |

Dropped or unaddressed: Bader is not a coauthor; the April Conjecture 3.1 (every HM-typable
program has a principal combinatory type) is not revisited, and there is still no
theorem relating what this system types to HM or System F.

## 3. Things that are weaker than they first look

- **Self-typing is near-trivial.** `infer_self_typed` is a one-line corollary of
  `concrete_is_self_typed`: inference returns any label-free tree unchanged. A concrete type
  says nothing the program doesn't. All the classifying power is in the ontology.
- **Typing `infer`'s *applications* fails.** The paper says so itself (Appendix A):
  `infer`'s type refuses abstract argument types, and a type like `Bool` has no type
  (`Bool_untypable`). So a tool that consumes inference results is untyped.
- **Soundness of each abstract type is a Rocq proof, not a program.** Typing runs inside
  the calculus; knowing the ontology is sound still happens outside it. The "one level"
  story stops at the metatheory.
- **"Preservation is soundness" has little bite.** Since only a bare leaf gets stuck,
  "well-typed programs don't go wrong" is nearly free; the content is entirely in what
  each abstractor promises.
- **Inference is evaluation**, so it diverges where evaluation does (Theorems 13, 16, 30).
  Termination conditions are future work.
- **The ontology used is "sound by values" only**, so the stronger Theorem 3 does not apply
  to it, and some evaluation snapshots are untyped (§12.4).

## 4. Novelty, and the "reinvents PLT Redex" tweet

Context: Geoffrey Huntley (AI-coding commentator) posted the repo; Kiran (a PL / formal
verification researcher) quote-tweeted "typing rules become programs → look inside →
slop drop → reinvents plt redex a decades old idea"
([tweet](https://x.com/kirancodes/status/2118366511614374373)).

**Executable typing rules are old, and that part of the charge is fair against the
title.** PLT Redex lets you write reduction relations and typing judgments as runnable
relations in Racket (`define-judgment-form`;
[Redex tutorial, Typing](https://docs.racket-lang.org/redex/tutorial.html#(part._.Typing))).
Logic-programming checkers, Twelf, Ott and the K framework do similar things.

**But Redex is not what the paper claims.** In Redex the checker is Racket code over quoted
S-expressions: a judgment form "defines form-id as a relation on terms via a set of
inference rules"
([reference](https://docs.racket-lang.org/redex/Other_Relations.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._define-judgment-form%29%29)),
and the `term` form that builds those terms "behaves similarly to quasiquote"
([reference](https://docs.racket-lang.org/redex/Terms.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._term%29%29)).
Object language and meta language differ. Jay's claim is internalisation: the checker is a
term *of the same calculus* that reads unquoted programs, and is applied to itself
([paper p.2](typing-rules-as-programs/paper/typing-rules-become-programs.pdf#page=2)).
Redex has no analogue of `infer @ infer`. Caveat: this holds for programs (values) only.
Pending expressions are typed by `infer_expression` over the `Sm`/`Fk` encoding, which is
itself a light quotation. Highlighted captures of each passage are in `evidence/`.

**The closest prior work to the central intuition is uncited**, and happens to be a Redex
paper: Kuan, MacQueen and Findler, "A Rewriting Semantics for Type Inference" (ESOP 2007)
set out to treat "type checking in a manner like the operational semantics of execution",
where "each term is incrementally rewritten to its type"
([Kuan et al., p.1](https://users.cs.northwestern.edu/~robby/pubs/papers/esop2007-kmf.pdf#page=1)).
That is the same idea as "concrete clauses mirror evaluation". Cousot's "Types as Abstract
Interpretations" (cited by Jay) gives the general frame: "types are abstract
interpretations"
([Cousot 1997, p.1](https://www.di.ens.fr/~cousot/publications.www/Cousot-POPL97-p316-331-1997.pdf#page=1)).
Typed self-application without encodings is Jay's own line, from
[Jay & Palsberg, ICFP 2011](https://web.cs.ucla.edu/~palsberg/paper/icfp11.pdf#page=1)
through the tree-calculus book and PEPM'25.

**What is actually new** (narrow, but real):

1. A type checker that is a program of an intensional calculus, run on unquoted programs,
   with a machine-checked preservation theorem parametric in a user-supplied set of
   abstract types.
2. Types as plain trees with no type grammar, and polymorphism by type application, now
   internal rather than external (carried over from April).
3. Mechanised breadth: generic queries and updates typed so they act on data, on programs,
   and on paused evaluations.

Verdict: incremental on Jay's own April paper (it executes April's stated future work),
not a reinvention of Redex. The tweet misses the internalisation point, but the paper
does overreach in framing ("typing rules become programs" is a phrase with a long
history) and skips the most relevant related work.

**On "slop".** Two commits, 33k Rocq lines, and the blog post announcing it says "Maybe LLMs
can help it happen"
([blog](https://github.com/barry-jay-personal/blog/blob/main/2026-11-07-big-step.md)).
Whether an LLM helped write it can't be settled from the artefacts and doesn't bear on
correctness: the proofs either check or they don't (§5).

## 5. Build check

Built 2026-11-05 on Rocq 9.1.1 + stdlib 9.0.0 from nixpkgs (the repo asks for 9.3). The
only change needed was swapping 9.3's `Abbreviation` keyword for `Notation` (same meaning
here). Result: all 28 files compile, 8m50s wall at `-j8`, peak 2.7 GB (`divergence.v`).
`Assumptions.v` prints "Closed under the global context" for all 66 Rocq Theorems: no
axioms, no `Admitted`. The `Hypothesis` lines in `recursion.v` and `dither_minimisation.v`
are section premises ("the ontology supports booleans"), which become ordinary
assumptions of the exported theorems. Slowest files: `customisation.v` (159s),
`divergence.v` (80s), `queries.v` (72s).

So the proofs are real. What they prove is the question for §3.

## 6. What it means for disp

Jay is converging on disp's stance: types are trees, the checker lives in the language,
and soundness is relative to the declared types. The differences that remain:

- **One type per program vs overlapping recognizers.** `infer` is a function, so each
  program has at most one type. disp lets many recognizers accept one value
  (`Point 3`, `Nat`, `Tree`).
- **Ontology ≈ disp's type library**, but Jay's abstractors are opaque by default
  (triage on an abstract type fails), while disp's `Named` delegates membership.
- **Symbolic running.** Jay types a program by running it on types; disp's confirmer
  (`lib/confirm.disp`) checks claims by running programs on trees with holes. Both are
  in-language abstract interpretation. Jay's `IsNamed` and refusal-at-triage rules are a
  worked example of what "forgetting" looks like when the abstract values are trees.
- **Self-typing as a cheap anchor.** "Every label-free program is its own type" is the
  analogue of `Point x` for every tree `x`; disp already has it, and the paper shows it
  buys little on its own.
