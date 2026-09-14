# Stellogen — Boris Eng (solo)

**Repo:** https://github.com/engboris/stellogen (134★, 13 forks, GPL-3.0, OCaml; created 2023-05-03,
pushed 2026-08-07; 666 commits, 633 by Eng; commits per year 14 / 86 / 454 / 112 for 2023–2026;
three beta releases in 2024, none since). **Clone inspected:** yes — `README.md`, `BASICS.md`,
`KERNEL.md`, `examples/`, `ai/research/*.md`, `.claude/skills/write-stellogen/`. **Built:** yes,
`nix build .#stellogen-minimal`; stock examples (`naive_nat`, `proofnets/mll`, `relational/arithmetic`)
run; two probes of our own below.
**Relevance:** the only running implementation of Girard's transcendental syntax, the line
`research/sequent.typ` names (via ludics) as the closest ancestor of disp's behavioral types. Its
typing story is disp's bet stated verbatim — types are user-space test suites over an untyped
substrate, the kernel judges nothing — and its author wrote the 2023 thesis that formalises the
usine/usage split `CHECK.disp` and `IDEAS.disp` are re-deriving.

## What it is

A language whose one mechanism is first-order unification, in both roles. The **object kernel** is
stellar resolution exactly as in Eng's thesis: terms; *rays* (terms with a `+`/`-` polarity on the
head symbol); *stars* (blocks of rays, variables local to the star); *constellations* (multisets of
stars). Execution is one rule run to saturation: a reactive ray fuses with any dual unifiable ray,
the substitution propagates to the neighbours, the two stars merge; several partners means the
step branches. Two additions beyond the thesis: a star marked `*` is a *catalyst* (copied, never
consumed, dropped from the result) and a variable written `!X` is a *ground guard* (its ray may not
fuse until that position is variable-free). No numbers, booleans, functions, or recursion are
primitive; a function is a star `[(-in X) (+out (s X))]`, a relation runs backwards for free.

The **meta kernel** is glue that deliberately cannot compute: `def`/`#`, `exec`, `show`, two
assertions (`==` syntactic equality of results, `~=` polarity-blind unifiability), one iterator
(`forall` over a "galaxy" of tests, each in its own interaction space), `use`, fixed-arity `macro`,
`spec`. `KERNEL.md` is normative and states the sorting rule: anything that computes lives in the
object language; a meta form must justify itself as assemble/run/compare. About 3,200 lines of OCaml.

**Typing is user space.** A type is a galaxy of test constellations and the assertion is a prelude
macro:

```stellogen
(spec nat { [(-nat 0) ok] [(-nat (s N)) (+nat N)] })
(macro (:: Tested Test) §(forall Test T (== (exec #Tested *#T) ok)))
```

`KERNEL.md` Part IV calls this "the judgment contract": every checking macro must bottom out in the
two trusted observations, and "a practice's `::` macro is that practice declaring its orthogonality
relation." The `§` marker splits every file into a check phase (`sgen check`) and a run phase
(`sgen run`); `eval` runs both in order. Code is a first-order term by construction, and Part III
freezes that encoding as a contract so *shape checkers* can unify against reified code.

The flagship is `examples/proofnets/mll.sg`: MLL proof structures as constellations, cut
elimination as plain `exec`, and the Danos–Regnier switchings as test galaxies that accept a proof
of `(A ⊗ B) ⊸ (C ⊗ D)` and reject a non-proof. `linear_lambda.sg` types linear λ-terms through
the same proof-net encoding — the only function types in the repo.

**Direction (July 2026 design docs, AI-authored like ours):** abandon the general-purpose
ambitions; be "the assembly language of logic", "Coq with the logic evicted from the kernel into
user space". A *system* is a triple (notation macros, checker constellation, paper theorem that
everything the checker accepts has property P). First planned systems: an acyclicity checker whose
guarantee is termination (thesis Ch. 9), then MLL correctness. The meta-kernel has a written
admission rule ("admitted only if inexpressible by the strata below plus macros; observation
additions cost the most") and a reflection plan (`quote` of results, `eval` withheld until a
tactic client exists). Two recorded boundary results: synchronous circuits cannot be encoded
faithfully (unification fires on half-arrived inputs), and there is no namespacing because symbols
are global addresses.

## Why disp should care

### 1. Same bet, different half of reflection

Both refuse a typed term language, make the checker library code, and police a tiny kernel with a
written admission rule. Stellogen's *inspect* side is cheap and principled — every expression already
is a term and unification is the pattern matcher, so a shape checker is an ordinary constellation.
Its *run* side is absent: term-to-code `eval` was removed for kernel purity. disp's tree calculus
has both halves natively; Stellogen shows what the inspect half looks like when it is the whole
story.

### 2. The function-type gap is disp's gap, reproduced in one afternoon

We repeated the `CHECK.disp` experiment. Usage by sample works and catches a bad function:

```stellogen
(def suc [(-in X) (+out (s X))])
(def natout *{ [(-out 0) ok] [(-out (s N)) (+out N)] })
(show (exec #suc [(+in 0)] #natout))          ; ok
(show (exec [(-in X) (+out foo)] [(+in 0)] #natout))  ; (+out foo)
```

Feeding a *symbolic* input (a bare variable) makes the Nat checker branch forever — timed out at
20 s. With a ground guard on the checker's recursive ray (`(+out !N)`) the run stops and leaves the
residue `(+out !N)`: that is `CHECK.disp`'s `stuck` verdict, except Stellogen puts it in the
substrate as a modality while disp hand-codes it in a symbolic evaluator. Neither system has a
finite certificate for a recursive function over Nat; Stellogen's only adequate function types are
the linear fragment Eng's thesis proves (Theorem 70.5).

### 3. Two design moves worth copying

- **Explicit check-phase marking.** disp's "root-file annotations are never verified" rule
  (`AGENTS.md`) is a footgun; Stellogen marks the phase per expression with `§` and reports
  cross-phase references as errors.
- **The guarantee slot.** A Stellogen *system* names the adequacy theorem as a shippable artifact
  next to the checker. disp's `.opt.disp` overlays carry licenses for rewrites but have no slot for
  "why this shape check implies the behavior" — precisely what `check_by_induction`'s
  decreasing-measure debt is missing.

## Scorecard

| Axis | Stellogen | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✅ 83% (inert terms) | Code is inert first-order terms matched by unification; no `eval`, no self-evaluator. | 1 · ½(no eval back) · 1 — code is a first-order term that shape checkers unify against; no eval back to code; types are library constellations |
| G2 Specification | ✗ 12% (library types) | "Type system as library code" is the entire project; no dependent types, function types only for the linear λ fragment. Equality: `==` is syntactic, order- and variable-name-sensitive by specification; no behavioral equality. | ½(value tests) · 0 · 0 · 0 — types are user-space test galaxies over values; no dependent types, no proofs; `==` is syntactic |
| G3 Trust | ◐ 50% (paper theorems) | Two-part kernel with a normative spec and two trusted observations; trust rests on per-system paper theorems, no evidence objects. | 1 · 0 · ½(paper theorems) — ~3,200 lines with a normative spec and two trusted observations; checkers are user space; trust rests on paper theorems, no evidence objects |
| G4 Execution | ✗ 0% (none) | None; docs admit concrete execution "has a horrible complexity" (repeated graph isomorphism). | 0 · 0 · 0 · 0 — execution has "a horrible complexity" |
| G5 Search | ✗ 0% (none) | None, and none intended. | 0 · 0 · 0 — none intended |

## Where disp differs

disp is applicative, deterministic, eager and total-by-fuel; Stellogen is relational, asynchronous,
branching and possibly divergent. disp can run reified code; Stellogen cannot. disp wants dependent
types, a cost model and an optimizer; Stellogen wants a logic workbench and a tool paper. Eng's own
thesis (§88.4) says transcendental syntax "still cannot give a transcendental status to type
judgement appearing in (Martin-Löf) type theory" — disp starts where that sentence stops.

Two cautions. Activity fell from 454 commits (2025) to 112 (2026 to date) with no release since
December 2024: solo-researcher graveyard risk. And the "Ragot 2025" citation the docs lean on
(described there as a linear-time simulation of interaction nets) is *Linear Realisability over
Nets: Multiplicatives* (CSL 2025, Ragot–Seiller–Tortora de Falco), an orthogonality-based
realisability model for MLL; the interaction-net simulation claim is not in the published abstract
and is cited here as Stellogen's claim.

## Verdict

**Ancestor, not competitor: the living implementation of the ludics/transcendental-syntax line
disp's behavioral types descend from, with the usine/usage split made explicit and the same
function-type gap unsolved. Watch the shape-system (acyclic ⇒ terminating) and `logics/` work.**

**Distance from disp's goals: overlaps on the G2 "types as library code" thesis and the G3 kernel
discipline, near zero on equality, G4 and G5; load-bearing reconnaissance on exactly the question `CHECK.disp`
is asking.**
