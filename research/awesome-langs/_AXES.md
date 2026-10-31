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

disp's checker runs and its search does not (G5). disp also bets on a route: no quotation layer,
so nothing has to be trusted to connect the code the optimizer reads with the code that runs. That
bet is why disp is built on tree calculus, and it hasn't been tested yet, so it isn't scored.

## Where disp stands

The same scale applied to disp itself, so the website's comparison can draw
disp beside the others. Update it when the status changes; the site rebuilds
from this table.

| Axis | disp | Note | Clauses |
|---|:--:|---|---|
| G1 Substrate | ✅ 100% (native) | A program is a tree that `shape_of` takes apart, and the checker is a disp program you apply. Lean, Rocq, Nock and Stellogen also score full marks, some through quotation. | 1 · 1 · 1 — a program is a tree that `shape_of` takes apart; the kernel is written in disp and is a tree you apply |
| G2 Specification | ◐ 72% (dependent, library) | Dependent types as library code over the kernel (Pi, telescopes, coproducts, a universe that checks itself); proofs run but the library is small. Equality: `Eq` compares canonical forms, and the one witness-licensed rewrite slice went with the retired kernel. | 1 · ½(small library) · ½(propositional) — Pi, telescopes, coproducts and proofs that run, over a small library; cost as a grade is designed; `Eq` compares canonical forms, intensional, and the decidable fragment that would license rewrites is open (Q1) |
| G3 Trust | ✅ 90% (minting kernel) | `lib/kernel/` mints hypotheses and keeps legitimacy in provenance, and the elaborator is untrusted. Nothing independent re-checks its verdicts. MM0's core is smaller and externally verified. | 1 · 1 · ½(one core) — `lib/kernel/` mints hypotheses with provenance; elaborator and library untrusted; five evaluators agree but nothing independent re-checks the kernel's verdicts; the licensed `.opt.disp` slice went with the retired kernel and nothing replaces definitions today |
| G4 Execution | ✗ 15% (interpreted) | Interpreted tree-walkers, with no compiled output and no model of a target. Nothing hands work to an external language yet (`research/OPTIMIZER.typ` designs it as the single impure driver); `--stats` reports a run's cost beside it, not as a value. | 0 · 0 · ½(cost report) — interpreted tree-walkers; `--stats` steps and `cold_equiv` are a deterministic, replayable cost model; no hardware model; `--stats` reports a run's cost beside it, and nothing hands work to an external language yet |
| G5 Search | ✗ 0% (designed) | The optimizer is designed (`research/OPTIMIZER.typ`) and unbuilt; the licensing machinery it needs was prototyped in the retired kernel and is not in `lib/kernel/`. | 0 · 0 · 0 · 0 — the optimizer is designed (`research/OPTIMIZER.typ`), not built; no search exists yet |

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

Rules: a design gets no credit, the how-tag carries it. A ½ must name its route in the
cell — `½(contracts)`, `½(metaprograms)` — the specific half-credit condition it takes,
so no half score exists without a stated reason; the script rejects a bare ½. 1 and 0
stay bare. The symbol is derived: ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's on
that axis. `_SCORES.md` is the source; `scripts/awesome-scores.py --write` recomputes the
percentages and copies them into every write-up's scorecard (score, clause values, why), the
master table, and the table above, and its report ranks each column.
