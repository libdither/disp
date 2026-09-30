# Dedukti / Lambdapi

**Repos:** https://github.com/Deducteam/Dedukti (238★, created 2017-11-16, pushed 2026-07-01;
10 commits in 2025, 3 in 2026) · https://github.com/Deducteam/lambdapi (401★, created
2017-09-10, pushed 2026-09-30; 131 commits in 2026, 1,247 of them all-time by Frédéric Blanqui)
**Written in:** OCaml. Dates and commit counts verified via the GitHub API on 2026-09-30.
**Clones inspected:** yes. Dedukti `README.md`, `kernel/`, `commands/dkmeta.ml`,
`libraries/theories/`, `libraries/paradoxes/`; Lambdapi `README.md`, `doc/*.rst`, `src/`.
**Relevance:** the oldest working version of "the type theory is a library file over one
small checker", and the project whose whole job is re-checking other provers' proofs.

## What it is

Both check one calculus: dependent function types plus *user-written rewrite rules*. Two
terms count as equal when the rules (and β) rewrite them to the same thing. There is
nothing else built in: no inductive types, no universes, no equality type.

- **Dedukti** is the bare checker. Its kernel is 4,389 lines (`kernel/*.ml`), now in
  maintenance.
- **Lambdapi** is the proof assistant on the same calculus: tactics, implicit arguments,
  unification, an LSP server, exports to Rocq and Lean. 22,488 lines of OCaml, 5,403 of
  them in `src/core`. This is where the work happens now.
- **Kontroli** is a third, independent checker in Rust, [faster than Dedukti on all five
  datasets it was measured on](https://arxiv.org/pdf/2102.08766#page=1).

A theory is a file of constants and rules. `libraries/theories/` ships the calculus of
inductive constructions (`cic.dk`, 64 lines), observational type theory (`ott.dk`, 323
lines, after "Observational Equality, Now!"), a Cedille encoding (`cedille.dk`), simple
type theory and first-order logic. The pattern is always the same: a type of codes, a
decoding function, and rewrite rules that say what each code decodes to.

The intended use is as a hub. Other provers export their proofs, and Dedukti re-checks
them against the encoded theory: [Zenon, iProver, FoCaLiZe, HOL Light and
Matita](https://arxiv.org/pdf/2311.07185#page=1) in the original paper, and the Lambdapi
[README](https://github.com/Deducteam/lambdapi) lists current users translating
HOL Light to Rocq, SMT (Alethe) proofs, TPTP prover output, Event-B, PVS, Metamath and K.

## Why disp should care

### 1. Types as library code, already at scale

disp's G2 asks for the whole type system to be library code over a tiny kernel.
Dedukti is that, with a decade of libraries behind it. Two of the three equality answers
FOUNDATIONS §7 cites (observational, Cedille) sit in its repo as theory files, next to a
directory of paradoxes (`libraries/paradoxes/`: Russell, Girard, Hurkens, Yablo) that
show which theory files are inconsistent. A user theory is only as sound as its rules;
the kernel promises that a proof follows from them and nothing more.

### 2. Rewrite rules are the closest thing anywhere to `.opt.disp` overlays, without the license

A Dedukti rule changes what the checker treats as equal, permanently. What gets checked
before a rule is accepted:

- it preserves types: Lambdapi [will not accept a rule if it does not pass this
  test](https://lambdapi.readthedocs.io/en/latest/commands.html#id7);
- local confluence against the rules already present, as a warning only;
- nothing else. Confluence and termination are assumed, and [the verification is left to
  the user, who can call external
  provers](https://lambdapi.readthedocs.io/en/latest/commands.html#id7). Dedukti's README
  says [termination is not checked
  (yet?)](https://github.com/Deducteam/Dedukti#a-small-example).

So a rule is a definition, not a proved replacement. Lambdapi's docs advertise that you
can [turn a proved equation into a rewriting
rule](https://lambdapi.readthedocs.io/en/latest/about.html#what-is-lambdapi), but nothing
ties the rule to the proof; the user does both by hand. disp's overlay needs exactly the
missing link: the rule installs only when the equation's witness checks.

### 3. Independent re-checking is the product

Three checkers for one calculus (Dedukti, Lambdapi, Kontroli), and the calculus exists to
re-check proofs that some larger system already accepted. This is G3's third clause at
full strength. disp has five evaluators that agree, and nothing independent that
re-checks a kernel verdict.

### 4. Matching on unevaluated calls, no quotation

A rule like `plus (plus x y) z --> plus x (plus y z)` matches a call that has not run,
including calls to defined functions, with higher-order (Miller) patterns under binders.
That is a narrow form of programs inspecting programs with no quoting step. It stops
there: a rule cannot take apart an arbitrary function, and `dk meta` adds experimental
quoting encodings (`--quoting lf | prod | ltyped`) for the cases where it must.

## Scorecard

| Axis | Dedukti / Lambdapi | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ◐ 33% (rewrite patterns) | Rules match on unevaluated calls with no quotation layer, but only through patterns; arbitrary terms need `dk meta`'s quoting. The checker is OCaml. | ½(rewrite patterns) · ½(patterns only) · 0 — rewrite rules match unevaluated calls, defined symbols included, with no quotation layer; arbitrary terms need `dk meta`'s quoting; the checker is OCaml |
| G2 Specification | ◐ 65% (rewriting, library) | Dependent types with every theory a library file; large translated proof libraries. Equality: conversion modulo user rules, a decidable fragment when the rules are confluent and terminating, which the system assumes. | 1 · 1 · 0 · ½(user rewrite rules) — dependent types with every theory a library file and large translated proof libraries; equality is conversion modulo user rules, decidable only when they are confluent and terminating, which is assumed |
| G3 Trust | ◐ 72% (re-checker) | A 4,389-line kernel, proof terms, three independent checkers, and re-checking other provers as the purpose. Rules are accepted on a type-preservation check alone. | ½(mid-size kernel) · 1 · 1 · ½(type-preservation only) — a 4,389-line kernel; proof terms; Lambdapi and Kontroli re-check the same calculus, and re-checking other provers is the purpose; a rewrite rule is accepted on type preservation, confluence and termination assumed |
| G4 Execution | ✗ 0% (none) | A proof checker: no compiled output, no cost model. | 0 · 0 · 0 — a proof checker: no compiled output, no cost model |
| G5 Search | ◐ 40% (ATP export) | Automated provers (Zenon Modulo, LEO-III, SMT via Carcara) emit proofs the kernel checks. Lambdapi's own `why3` tactic records the goal as an axiom. | ½(ATP proof export) · ½(checker only) · 0 — Zenon Modulo, LEO-III and SMT solvers via Carcara emit proofs the kernel checks; the `why3` tactic admits its goal as an axiom; no cost |

## What disp could steal

- **The rule-acceptance checklist, as the floor for overlays.** Left-linearity (`--ll`),
  a typable left-hand side (`--type-lhs`), type preservation, and joinability of critical
  pairs against existing rules. disp's license should demand all of these and then the
  equivalence witness on top.
- **Exporting rules to the competition formats.** Lambdapi writes its rules as HRS and
  XTC files so the confluence and termination competition provers can check them. If
  overlays were emitted in those formats, disp would get third-party checkers for free.
- **`ott.dk` as a reading.** 323 lines stating observational equality as rewrite rules
  on a universe of codes. The shortest complete statement of that equality I found, and
  the same pole `narya.md` covers.
- **A disp theory file.** Encoding disp's checker as a λΠ-modulo theory would put disp
  proofs in front of three existing checkers and the Rocq/Lean exporters. Untested; the
  obstacle is that disp's equality is pointer identity, which rules do not express.

## Where disp differs

Dedukti extends equality by orienting equations left to right and trusting the result
to be confluent and terminating. disp keeps equality at pointer identity and wants each
replacement to carry its own evidence. Dedukti has no notion of cost, so a rule is never
"the faster version" of anything; it is just what the symbol means. And the two layers
stay apart: theories are data for an OCaml checker, never programs that run the checker.

## Verdict

**The proof that "types are a library over a small checker" scales, and the clearest
picture of what an unlicensed rewrite overlay looks like after ten years of use.**
Lambdapi is active daily; Dedukti itself is finished software.

**Distance from disp's goals: ahead on re-checking and library breadth, level on the
library-types architecture, absent on execution, cost, and self-application.**
