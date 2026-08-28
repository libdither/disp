# Telomare (Stand-In Language) — Sam Griffin (sfultong) and hhefesto (two people)

**Repo:** https://github.com/Stand-In-Language/stand-in-language (10★, pushed 2026-08-13; the
author's own https://github.com/sfultong/stand-in-language has 33★ and 50+ experiment branches).
History since 2016-04; 54–125 commits every year since 2021; hhefesto's PRs merge every few weeks.
**Written in:** Haskell, ~12.6k lines including tests, a 1k-line LSP, and docs. Nix + cabal. Apache-2.0.
**Clone inspected:** yes — `README.md`, `CHANGELOG.md`, `src/Telomare/{IR/Base, Size, Size/IR,
Machine, TypeCheck, Eval/Meter, Eval/Reference, Certificate, Levels, Fast, Desugar}.hs`,
`Prelude.tel`, `test/programs/limits/`, `test/NatUDTTests.hs`, and the three 2016–2018 essays
the README links. Not built: sizing tic-tac-toe takes ~70 s and a UDT test compile 5–6 min.

## What it is

A ten-year project whose founding essay ("A Better Model of Computation", 2016) argues for
giving up Turing completeness: split the Turing machine into "a terminating pure function
taking a state and returning a state, and a simple fixed-point combinator for feeding the pure
function into itself," so that "we know exactly how long a computer program will take to
execute down to the nanosecond, given specific inputs." The README's goal has not moved since:
a total language that "eventually will have powerful static checking and an optimizing backend."

The core is eight instructions over binary trees — `Zero, Pair, Env, SetEnv, Defer, Gate, Left,
Right` — plus `Abort`. Data is nested pairs ending in `Zero`; a closure is an ordinary
`Pair (Defer code) env` ("What is the difference between a lambda and a closure with an empty
environment? I decided there wasn't any compelling difference, and removed lambdas from the
grammar" — 2018). Recursion is not a primitive: the surface form `{ test, step, base }` compiles
into a loop whose iteration count the compiler *infers*. IO keeps the 2016 shape — `main :
input -> (output, state)`, iterated by the runtime — the same one-impure-driver-at-the-boundary
shape as disp's §8.

The pipeline is parse → expand → desugar → resolve → typecheck (unification over
`zero`/`pair`/`arrow` plus an `Any`, essentially STLC) → **size** → evaluate. Sizing is the
distinguishing stage: it runs the program over a *symbolic* input (a tree of indexed unknowns,
bounded by whatever the program's refinement annotations promise), and when a `Gate` inspects
an unknown it *superposes* both branches (`EitherPF`, `Telomare.Machine.superStep`) instead of
picking one. Each recursion site is unrolled abstractly until its test stops on every path; the
worst-case count is baked in as a Church tower; a program with no finite count does not compile,
and the error separates "budget too small — raise it" from "nothing bounds this input — add a
refinement." Sizing is deterministic, so `--compile` caches it in a `.telc` artifact and a run
skips every pass.

Three reports fall out. `--certificate`: the per-site bound (`Prelude:30:18 (#0)  <= 11`).
`--meter`: one run's steps and nodes built — deliberately no memory figure, because the
shared-environment evaluator would count a run that fits in a few GB as 1.2 TB. A structural
"levels" pass: which bindings are used *below* the recursion level they were bound at
(duplication pressure), read off the source in milliseconds. A second interpreter (the meter)
and a third (`--fast`: fuel-capped, no sizing, "plays tictactoe byte-identically in 2.74 s
against 66.80 s") are held to the reference by tests — a small version of disp's §10 oracle.

Types: `x : validator = e`, where `validator` is an ordinary function returning `0` (pass) or
`(1, message)` (abort). The compiler runs validators against an opaque input and rejects any
whose abort does not depend on the input (`StaticCheckError`); the rest run at runtime; and the
sizing pass mines them for input bounds. User-defined types are values branded with a hash of
their defining term (`HashF`) and checked by a generated runtime validator.

## Why it matters to disp

Three overlaps, one of them a built thing disp lacks, and one negative result.

**1. The cost-bound / cost-incurred split, built.** FOUNDATIONS §9 wants cost as a graded
coeffect: the *bound* a term demands and the *spend* a run incurs, linked by a soundness
theorem. Telomare has the crudest possible form of this and it works: `--certificate` is the
bound, inferred over every input and baked into the executable; `--meter` is the spend. disp
measures spend (`ApplyStats.steps`) and has no static bound; its totality story (`wf_fix` /
`Total`, `lib/prelude.disp:123`) is unbuilt. Telomare's bound is an iteration count, not a
hardware cost — but it is a machine-inferred, input-universal, static number, and nothing else
in this survey produces one.

**2. Types as predicates, at the runtime pole.** A Telomare validator is exactly disp's "a type
is a program you run on a value": `assert (not ($33 left x)) "input too large"`. The difference
is what happens under a binder. disp mints a hypothesis and watches the body — a checking
discipline that yields a verdict for all inputs. Telomare feeds the validator a barrier and only
rejects aborts that survive partial evaluation unconditionally; everything conditional is left
to runtime. That is the NuPRL-tradition idea (§2) with no logic on top: refinement as assertion,
not proof.

**3. Superposition, aimed at analysis instead of search.** `Telomare.Size` is a superposition
evaluator: an unknown at a gate becomes `EitherPF n doLeft doRight`, both branches proceed
sharing the environment, results merge by shallow equality. That is the mechanism of disp's §13
optimizer (`sup_λ`, `research/OPTIMIZER.typ` §8) and HVM4's `SUP` nodes, used to *prove a bound* rather
than to *enumerate candidates*. Same tool, opposite direction: disp collapses a superposition to
find the cheapest candidate; Telomare collapses one to find the deepest recursion.

**The negative result, for Q2.** The author's repo keeps the attempts, dated by their last
commits: `llvm-hhefesto` (2020, "jit works, but without proper garbage collection"),
`hvm`/`hvm2` (Dec 2022–Jan 2023, "more direct hvm rules, but they don't work"), `sbv3`
(SMT-driven sizing, 2024, "evalB seems to work, but too slow"), a dozen `sizing_wip*` branches
(2024, "sizing by evaluation, doesn't work because insufficiently lazy"); the 2026 cleanup
deleted "the commented-out HVM/LLVM/Chez backends." `Telomare.Levels` records why the net
backend died: `whoWon.board` in tic-tac-toe is used two recursion levels below where it is
bound, "which is the binding that made an interaction-net backend for this language
intractable." That is disp's measured no-memo finding — 4,000–67,000× work inflation, the
distributes-over-duplication conjecture holding only for affine code — reached independently,
on a *total* language, without types in the way. The live thread is the `affine` branch
("switch to 0cfa affine tagging", committed 2026-08-26), the continuation of what PR #146
called "the EAL types exploration": an affine-typing analysis sitting on exactly the
affine-only boundary disp measured.

## Scorecard

| Axis | Telomare | Note |
|---|---|---|
| A1 Reflection | ✗ | Closures are pairs, so a program can project a closure's *environment*; its code is an opaque `Defer`. No self-interpreter, no checker-in-the-language: every analysis is Haskell. |
| A2 Spec power | ◐ | Static layer is STLC-shaped (no polymorphism, an `Any` escape). Refinements are predicates, checked statically only when the failure is unconditional. No dependent types, no proofs. |
| A3 Kernel | ✗ | Trusted base is the whole compiler. `--certificate` is a *report*, not a re-checkable certificate; `.telc` artifacts carry counts nothing independent verifies. |
| A4 Equality | ✗ | Structural only (`shallowEq1` for merging superpositions). No equivalence story. |
| A5 Perf + cost | ◐ | Perf: Haskell tree-walker; sizing tic-tac-toe ≈ 70 s, a UDT test compile 5–6 min. Cost: a static, inferred, input-universal per-site bound (ahead of disp) plus a measured step/node meter (level with disp's `ApplyStats`). No hardware model. |
| A6 Search | ✗ | No synthesis. The superposition evaluator exists but is pointed at bounds, not candidates. |

## What disp could steal

- **Inferred bounds as the first static cost signal.** Before a graded ledger exists, "per
  recursion site, the worst-case iteration count over all inputs, found by symbolic superposed
  evaluation" is a concrete artifact. disp's `nat_rec`/`fix` sites are the analogue, and the
  promise machinery already does the symbolic part.
- **Two failure kinds, named.** "Budget exhausted" is a statement about the search; "nothing
  bounds this input" is a statement about the program. disp's `Ok false` and the optimizer's
  future "no certified improvement found" both want this split (cf. `vow.md`).
- **Refuse the dishonest metric.** The meter reports no memory figure and says why. disp's own
  proxies (interaction count, `steps`) deserve the same audit before they steer an optimizer (Q4).
- **Compile-once artifacts.** A deterministic analysis over symbolic input runs once and is
  cached; a run skips every pass. disp's per-file `clearCaches` and 8 GB test heaps (Q6) are
  the problem this solves.
- **A "levels" pass.** Duplication pressure — how many recursion levels below its binding a
  value is used — in milliseconds, from the source. A cheap static predictor of exactly the
  sharing loss disp measured on `rust-ic-net`.

## Where disp differs

disp is a superset of Telomare on every axis but one mechanism. The eight-instruction core
embeds trivially in tree calculus (pairs are forks, `Zero` is the leaf, `Gate` is triage,
closures are already pairs); disp adds general recursion, reflection, a kernel, dependent types,
an equality ledger, and five evaluators. Telomare's stated goals — totality, resource effects
"measured from the bottom up," an eventual optimizing backend — are a proper subset of disp's:
A5's cost-model half plus a weak A2, with A1, A4, A6 and self-application absent from the
essays entirely.

The one thing Telomare has that disp does not is the one thing it built: a totality checker
that *infers* bounds. disp chose a Turing-complete substrate and deferred totality; Telomare
made totality the substrate and paid with a sizing pass that costs minutes and rejects total
programs that outrun its unrolling budget — **[P1]** in miniature: the fully general analysis
is the slow one.

## Verdict

**A ten-year, two-person existence proof that "total by inferred bounds + a static resource
certificate" can be built on an eight-instruction pair calculus — and a measure of what it
costs.** Nothing else in this survey emits a static, input-universal cost bound; nothing else
here took a decade to reach tic-tac-toe. The interaction-net backend it abandoned, and the
reason given, is independent corroboration of disp's Q2 finding.

**Distance from disp's goals: ahead on the static half of A5 only; a runtime-checked shadow of
A2; absent on A1, A3, A4, A6.**
