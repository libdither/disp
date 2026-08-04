# F* / Low* / Pulse / KaRaMeL — Microsoft Research, Inria, Project Everest

**Repos:** https://github.com/FStarLang/FStar (3,081★, release v2026.08.02),
`FStarLang/pulse` (separation-logic DSL), `FStarLang/karamel` (extraction to C),
`hacl-star/hacl-star` (verified crypto, 1,833★)
**Activity:** FStar pushed 2026-08-03, 100+ commits in 3 months, 136 contributors.

## Why this file matters most

FOUNDATIONS §6 and Part V both name F*/HACL*/EverCrypt as **"the closest shipping
relative"** to disp, and state disp's delta in one sentence: *F\* still requires a
human to write the code and the proofs; disp wants the optimizer to synthesize
both.* So this is the baseline disp must beat, and the clearest statement of what
"already works" looks like.

## What it is

A dependently-typed language with **SMT-backed** verification (Z3 discharges proof
obligations automatically), refinement types, and an effect system. **Low\*** is the
C-like subset; **KaRaMeL** extracts Low* to readable, idiomatic C. **Pulse** is the
newer concurrent-separation-logic DSL (PulseCore, PLDI 2025, impredicative CSL).

The output ships: HACL*/EverCrypt verified crypto runs in Firefox, the Linux
kernel, WireGuard, mbedTLS, and Python. This is dependent-spec → verified fast C,
in production, at scale, for a decade.

## The parts that matter to disp

**The pipeline shape disp wants.** Write a high-level functional spec in F*, write
a low-level implementation in Low*, prove the implementation refines the spec, and
extract to C with performance comparable to hand-written assembly-adjacent code.
disp's GOALS is this pipeline with the middle step automated.

**SMT as the automation layer.** F* proved that "dependent types + an SMT solver
doing the boring parts" is the ergonomically viable point in the design space —
the reason F* is usable where Coq/Agda are not. disp's kernel checks by *running*
(types are predicates, checking is application); F* checks by *encoding to SMT*.
These are different bets about where automation comes from, and F*'s has a
15-year track record.

**Meta-F\*** — tactics and metaprogramming *in F\* itself*, reflecting F* syntax,
allowing user-written proof automation. This is A1-flavored, again via
quotation/deep embedding rather than native intensionality.

**The ICSE 2025 dataset** (940 KLOC of F* for neural synthesis of SMT-assisted
proof-oriented programming) is the incumbents building the AI on-ramp — direct
evidence that A6 is being attacked from the F* side too.

## Scorecard

| Axis | F*/Low*/Pulse | Note |
|---|---|---|
| A1 Reflection | ◐ | Meta-F* reflects F* syntax for tactics; deep embedding, not programs-as-data. |
| A2 Spec power | **✅** | Full dependent types + refinements + effects + separation logic (Pulse). Production-proven expressiveness. |
| A3 Kernel | ◐ | Trusted base is much larger than LCF-ideal: the F* typechecker *and* Z3 *and* KaRaMeL extraction are trusted. Bigger TCB than disp targets. |
| A4 Equality | ◐ | SMT-decided equalities within its fragment — genuinely automates many rewrites, but it is "retreat to a decidable fragment" (FOUNDATIONS [P1]), not a general licensing relation. |
| A5 Perf | **✅** | Low*→C→native; HACL* is competitive with hand-optimized C. The proof that verified code can be fast. |
| A6 Search | ✗ | Humans write the code and the proofs. This is precisely disp's identified delta. |

## What disp could steal

- **The Low*/KaRaMeL discipline**: define a *subset* of the verified language that
  maps 1:1 onto C, and verify at the high level while extracting at the low level.
  disp's "outsource execution to an external faster language" primitive
  (GOALS bullet 2) is a more ambitious version of the same idea, and KaRaMeL is
  the worked precedent for the boring-but-critical part (what subset extracts
  cleanly, and how you know the extraction is faithful).
- **SMT as a *component*, not a philosophy.** disp's kernel checks by running, but
  nothing stops an untrusted SMT-backed tactic from *proposing* a rewrite that the
  ~30-line trusted checker then re-validates (§12). F* shows how much mileage SMT
  gives; disp's architecture lets it stay untrusted.

## Where disp differs

F* is the *ergonomic* answer and disp is the *automated* one. Concretely:

- F*'s TCB includes Z3 — an enormous, actively-fuzzed C++ program. disp's design
  refuses that: clever things are untrusted and re-checked.
- F* has no cost model and no optimizer. Performance comes from a human writing
  Low* carefully; nothing searches for a faster equivalent program.
- F*'s equality is SMT-fragment equality. disp needs a *composable, type-indexed*
  relation (`~_T`) computed by the walker, precisely so an optimizer (not a human)
  can chain rewrites.

## Verdict

**The bar.** Any claim disp makes about "dependent spec → verified fast code"
must be stated as a delta against HACL*, because HACL* already ships that in
Firefox. disp's honest one-line pitch is: *F\* with the human removed from the
implementation step, on a substrate where the checker is itself optimizable.*
Everything in disp's Part IV is what it costs to earn that sentence.

**Distance from disp's goals: closest shipping system on A2+A5; zero on A6, which
is the entire point of disp.**
