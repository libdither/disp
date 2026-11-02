# The comparison axes

Every file in this directory scores a project against the same five axes, lifted out of
`GOALS.md`, which says what disp must achieve. How disp chose to build it (`FOUNDATIONS.md`)
is not graded: those bets sit in an unscored table in [`_SCORES.md`](_SCORES.md#approach-unscored)
and in the how-tags, so a project built nothing like disp can still score full marks. A project
can also be excellent and score near-zero: these are the things this goal needs, not a general
"is this a good language" scale.

The five follow the loop in `GOALS.md`: a language whose programs are data, specs a checker can
judge, a verdict the search can't game, programs that run fast with their cost coming back, and a
search that finds them.

| # | Axis | What the goal requires | Where it comes from |
|---|------|--------------------|---------------------|
| **G1** | **Substrate** | Programs are data, and the type checker is written in the language as a function that returns a verdict; quotation is fine | GOALS "programs are data", "a type system written in the host calculus", "turn type checker into a function" |
| **G2** | **Specification** | A spec can say what matters: types that mention values (dependent types or stronger), proofs, and an equality for specs like "behaves like f" | GOALS "dependently typed or strictly more powerful" |
| **G3** | **Trust** | A search that maximizes the checker's verdict will find and exploit any bug in it, so the code that verdict rests on is kept small and every clever layer hands it evidence to re-check | GOALS "formally correct", as a consequence of searching against the checker; no clause of GOALS names a kernel |
| **G4** | **Execution** | The programs found run fast, by any route; the checker can reason about the target through axioms or a model; and a primitive hands work to an external faster language and returns outputs plus time and memory | GOALS bullet 2 and the hardware bullet |
| **G5** | **Search** | From spec to implementation automatically: a search over candidate programs scored by the checker times measured cost, which writes its own score functions to practice on, and is eventually aimed at its own code | GOALS "external optimizer", "self-play", the bootstrap bullets |

## Rating key

A cell reads `◐ 50% (quotation)`: the percentage of the goal's requirement met (three or four
clauses, see the grading section below), the symbol it derives, and a how-tag.

- ✅ — 80% or more: does this at the strength the goal wants
- ◐ — 25 to 79%: a weaker or narrower version
- ✗ — under 25%: does not do this, or actively went the other way (a design counts as 0)
- **bold** — in the master table: a higher percentage than disp's, so *ahead of disp* on this axis
- **(tag)** — one or two words naming *how* the level is reached, so two projects with the same symbol still read differently; the website shows it under the symbol

## The one-sentence version of disp

Write a specification as a dependent type, turn the checker into a 0/1 score, multiply by
measured cost, and search for a program that is both provably correct and fast, then turn that
search on itself.

disp's checkers run and its search is small (`cegis` over a listed pool). disp also bets on a route: no quotation layer,
so nothing has to be trusted to connect the code the optimizer reads with the code that runs. That
bet is why disp is built on tree calculus, and it hasn't been tested yet, so it isn't scored.

## Where disp could get, in theory

**This is not what disp has.** Each clause scores what an argument written down so far says the
design could reach: a test that pins it, or a design note that says how. Where the arguments stop
short, so does the score, even though the goal is full marks everywhere. Every other project is
graded on what ships, so "ahead of disp" means ahead of this row, a high bar. What disp has now is
the next table.

| Axis | disp in theory | Note | Clauses |
|---|:--:|---|---|
| G1 Substrate | ✅ 100% (native) | A program is a tree that `shape_of` takes apart, and the checker is a disp program you apply. Already true today. | 1 · 1 · 1 — pinned by tests: a program is a tree that `shape_of` takes apart; `honest` and `certify` are disp programs you apply |
| G2 Specification | ✅ 100% (claims) | A claim is a test on bounded runs under quantifiers over all trees, so dependent types, proofs (lenses) and equality at a type are all claims. | 1 · 1 · 1 — a member of a function type is a lens (`implies`, pinned); proofs are lenses composed from the five rule lenses, so the library is work, not theory; equality at a type is agreement at every challenge, certified by induction (pinned for `id`/`shift` and the two doublings) |
| G3 Trust | ✅ 90% (claim kernel) | Trust stops at five hand-proved rule lenses over a small machine, and everything else re-runs. Short of full marks: no second certificate checker is designed. | 1 · 1 · ½(one core) — trust stops at five hand-proved rule lenses over a small machine; a certificate is a tree `certify` re-runs; `honest` re-checks independently but only refutes, and no second certificate checker is designed |
| G4 Execution | ✅ 100% (designed) | A hardware model is an ordinary program, and one impure driver at the boundary hands work to a faster language and reports its cost. Both are design notes from before the claim kernel; nothing is built. | 1 · 1 · 1 — designed, not built: a hardware model is a tree program whose cost is its output (`research/OPTIMIZER.typ` §9), so claims certify code for it; the one impure driver at the program's boundary runs work in a faster language and returns outputs plus cost (`FOUNDATIONS.md` §8) |
| G5 Search | ✅ 85% (cegis, designed) | `cegis` searches candidates against a claim, scored by verdict times cost. Short of full marks: nothing designed writes new claims, and Löb's theorem caps self-application. | 1 · 1 · ½(counterexamples kept) · ½(Löbian ceiling) — `cegis` is pinned; generators derived from claims widen its pool (`research/CLAIMS_OVER_TREES.md` §4); score = verdict × cost (`research/OPTIMIZER.typ` §2); nothing designed writes new claims; a successor's soundness can't be proved from inside (`research/LOB_AND_CURRY.md` §1) |

### The arguments

Each clause, the argument for it, and how strong it is: **pinned** (a test runs it), **designed** (a
note says how, nothing runs), or **none** (the score stops there). The claim kernel is
`lib/claim.disp` with its rules in `lib/tests/claim.test.disp`; `research/CLAIMS_OVER_TREES.md` §9
describes it.

**G1 Substrate**, 100.
- (a) programs as data: a program is a tree, and `shape_of` takes any tree apart. Pinned.
- (b) checker in the language, (c) callable as a function: `honest` and `certify` are disp
  programs, applied like any other. Pinned.

**G2 Specification**, 100.
- (a) dependent types: a claim quantifies over all trees and tests bounded runs, so membership in a
  dependent type is a claim (`CLAIMS_OVER_TREES.md` §1). A member of a function type is a lens:
  `implies A B` tests a function together with the back map saying which point of the argument
  it used. Pinned.
- (b) first-class proofs: a proof is a lens built from the five rule lenses, and its law
  `lens_holds` is itself a claim, so a wrong proof is refuted with a concrete tree. Pinned for the
  rules; the library around them is work, not theory.
- (c) equality: equality at a type is agreement at every challenge, which is extensional, and
  induction certifies it (`id` and `shift`; `dbl_step` and `dbl_step_add`). Pinned on those
  examples. Two limits that don't cost the clause: a true claim with no computable witness stays
  open, and transport along an equality only works for claims that never inspect the swapped tree.

**G3 Trust**, 90.
- (a) auditable core: the trusted part is five rules (finite cover, case split, generic run,
  induction, composition), each proved by hand, over the tree machine. Löb's theorem says a
  checker's soundness can't be proved from inside it, so a hand-proved floor is the least possible
  (`research/LOB_AND_CURRY.md` §1). Designed: the rules run in tests, the move off `lib/kernel/`
  isn't finished.
- (b) evidence objects: a certificate is a finite tree that `certify` re-runs, and a refutation is
  a concrete counterexample anyone can re-run. Pinned in tests; verdicts in `lib/verdict.disp` are
  still bare strings.
- (c) independent re-checking, ½: `honest` checks by enumeration, independently of `certify`, but it
  can only refute. None for a second checker that confirms certificates.

**G4 Execution**, 100. All three rest on design notes written for the retired kernel, which still
apply because they only need claims over programs.
- (a) fast programs: the search emits assembly certified against a hardware model, and the driver
  runs it on the machine. Designed.
- (b) target in the logic: a hardware model is a pure tree program
  `run_model : Asm → Input → (Output, Cost)`, and "this assembly refines the spec" is a claim over
  it (`research/OPTIMIZER.typ` §9). Designed.
- (c) handoff primitive: effects are data, and one impure driver at the program's boundary performs
  them; that driver is where work goes to a faster language and comes back with outputs and cost
  (`FOUNDATIONS.md` §8, `research/OPTIMIZER.typ` §9). Designed. It sits outside the claim kernel,
  which is pure.

**G5 Search**, 85.
- (a) spec → implementation: `cegis` searches a pool of candidates against a claim (pinned), and a
  generator derived from the claim can supply the pool (`CLAIMS_OVER_TREES.md` §4). Designed. Whether
  the search is affordable at scale is open (`FOUNDATIONS.md`, question 2).
- (b) checker × cost: the score is the verdict times the cost, and a cost bound is a claim like any
  other (`research/OPTIMIZER.typ` §2). Designed.
- (c) self-play, ½: `cegis` keeps the counterexamples that refuted earlier candidates and tries them
  first, which are adversaries. None for a search that writes new claims to satisfy.
- (d) self-application, ½: the optimizer is a disp program, so it can be run on itself (the third
  Futamura projection, `research/OPTIMIZER.typ` §5). But each successor's soundness can't be proved
  from inside, so self-application steps down one level of trust each time
  (`research/LOB_AND_CURRY.md`).

## Where disp is today

What the repo has, as of commit `a5e051f49b1b` (2026-11-02), graded on the same clauses and rules as
every other project. When a change moves a clause, update the `disp (today)` row in
[`_SCORES.md`](_SCORES.md), bump the commit there and here, and run the script.

| Axis | disp today | Note | Clauses |
|---|:--:|---|---|
| G1 Substrate | ✅ 100% (native) | Same as in theory: a program is a tree that `shape_of` takes apart, and the checkers are disp programs you apply. | 1 · 1 · 1 — same as in theory: `shape_of` takes a program apart, and `honest` and `certify` are disp programs you apply |
| G2 Specification | ◐ 72% (claims, library) | Claims over bounded runs and lenses work, but the rule lenses and `certify` live in a test file with little library around them, and equality at a type is certified only for `id` and `shift`. | 1 · ½(no library) · ½(a few instances) — claims over bounded runs and lenses work, but the rule lenses and `certify` live in `lib/tests/claim.test.disp` with little library around them; equality is certified for `id` and `shift` only |
| G3 Trust | ◐ 50% (two checkers) | `lib/kernel/` is still the checker in use beside the claim kernel, a verdict's `"Proved"` is a bare string, and certificates exist only inside `lib/tests/claim.test.disp`. | ½(two checkers mid-move) · ½(certificates in tests only) · ½(one core) — `lib/kernel/` is still the checker in use beside the claim kernel; a verdict's `"Proved"` (`lib/verdict.disp`) is a bare string, and certificates exist only inside the test file; `honest` re-checks by enumeration but only refutes |
| G4 Execution | ✗ 15% (interpreted) | Interpreted tree-walkers, no target model, no handoff to a faster language; `--stats` reports steps beside a run. | 0 · 0 · ½(cost report) — interpreted tree-walkers, no target model, no handoff; `--stats` reports steps beside the run |
| G5 Search | ◐ 42% (cegis) | `cegis` walks a listed pool of candidates against a claim and keeps the counterexamples it finds. Cost isn't in the loop, and nothing searches over disp itself. | ½(basic search) · ½(checker only) · ½(counterexamples kept) · 0 — `cegis` walks a listed pool of candidates against a claim and keeps the counterexamples it finds; cost isn't in the loop, and nothing searches over disp itself |

## Grading: the maximum and the clauses

The three symbols are coarse. [`_SCORES.md`](_SCORES.md) grades every cell as a percentage of the
goal's requirement: the "What the goal requires" column above, split into three or four clauses.
Each clause scores 0, ½ or 1 and the axis is their weighted mean: the weights sit on a `Weights:`
line under each axis in `_SCORES.md` and beside each clause below, relative within an axis; a —
clause (does not apply) drops out and the rest renormalize. The maximum is what `GOALS.md` asks for,
not the best project in the survey, so a project can be excellent and score low.

| Axis | Clauses |
|---|---|
| **G1** | (a) **programs as data** — programs can take programs apart, quotation counts (½ a restricted part only: types, environments, runtime graphs, rewrite patterns) · weight 1 | (b) **checker in the language** — the checker is written in the language (½ type theories as library files over an outside checker, the verifier only stated in the language, or a self-hosted compiler whose verdicts come from a solver) · weight 1 | (c) **checker as a function** — the checker is callable from programs as an ordinary function (½ from metaprograms only) · weight 1 |
| **G2** | (a) **dependent types** — specifications mention values (½ contracts or refinements over runtime values) · weight 45 | (b) **first-class proofs** — propositions and proofs are objects in the language, with a library in use (½ solver-discharged obligations, or proofs without a library) · weight 35 | (c) **equality theory** — an equality you can state and check, for specs like "behaves like f": extensional, cubical, observational (½ propositional or SMT-fragment equality, tactic-driven or a decidable fragment) · weight 20 |
| **G3** | (a) **auditable core** — a trusted core small enough to audit · weight 40 | (b) **evidence objects** — it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) · weight 40 | (c) **independent re-checking** — the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only) · weight 20 |
| **G4** | (a) **fast programs** — the programs it produces run at C/Rust class by any route, handing work to an external language included (½ compiled but not systems class) · weight 40 | (b) **target in the logic** — the checker can reason about the target through a model or axioms of equivalence, counted equally (½ a fragment, a modelled VM, or assumed specs for part of the target) · weight 30 | (c) **handoff primitive** — a primitive runs work in an external faster language and returns outputs plus time and memory as values (½ a foreign call with outputs only, or a cost report beside the run) · weight 30 |
| **G5** | (a) **spec → implementation** — implementations found automatically (½ an external LLM loop, harness generation, basic proof search) · weight 35 | (b) **checker × cost** — candidates scored by the checker and by measured cost, both in the loop (½ one of the two) · weight 35 | (c) **self-play** — the search writes its own score functions to then satisfy (½ it generates adversaries or tests) · weight 15 | (d) **self-application** — the search is aimed at itself (½ a flywheel started) · weight 15 |

Rules: a design gets no credit, the how-tag carries it; disp's theoretical row is the one exception (see above). A ½ must name its route in the
cell — `½(contracts)`, `½(metaprograms)` — the specific half-credit condition it takes,
so no half score exists without a stated reason; the script rejects a bare ½. 1 and 0
stay bare. The symbol is derived: ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's theoretical row on
that axis. `_SCORES.md` is the source; `scripts/awesome-scores.py --write` recomputes the
percentages and copies them into every write-up's scorecard (score, clause values, why), the
master table, and the table above, and its report ranks each column.
