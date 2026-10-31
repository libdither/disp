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

## Where disp stands

disp's row is graded differently from everyone else's: at what the claim kernel (`lib/claim.disp`,
`research/CLAIMS_OVER_TREES.md` §9) could reach once the move off `lib/kernel/` finishes, as a best guess,
with today's level in each note. Every other project is graded on what ships, so "ahead of disp"
means ahead of disp's limit, a high bar. Update it when the status changes; the site rebuilds
from this table.

| Axis | disp | Note | Clauses |
|---|:--:|---|---|
| G1 Substrate | ✅ 100% (native) | A program is a tree that `shape_of` takes apart, and the checker is a disp program you apply. Today: 100%, the same as the limit. Lean, Rocq, Nock and Stellogen also score full marks, some through quotation. | 1 · 1 · 1 — a program is a tree that `shape_of` takes apart; `honest` and `certify` are disp programs you apply |
| G2 Specification | ✅ 100% (claims, library) | A claim is a test on bounded runs, under quantifiers over all trees, so dependent types, proofs (lenses) and equality at a type are all claims. Limit on it: a true claim with no computable witness stays open forever, and transport along an equality only works for claims that never inspect the swapped tree. Today: about 72%, the library is small and equality is pinned on a few examples. | 1 · 1 · 1 — at the claim kernel's limit: a claim tests bounded runs, so a dependent type is a claim; a proof is a lens built from the rule lenses; equality is agreement at a type, certified by induction (`id` and `shift`). A claim with no computable witness stays open |
| G3 Trust | ✅ 100% (claim kernel) | Trust stops at five hand-proved rule lenses over a small machine, and everything else re-runs. Today: about 50%, `lib/kernel/` is still the live checker, the rules sit inside a 919-line test root, and a verdict's `"Proved"` is a bare string rather than a checked certificate. | 1 · 1 · 1 — at the claim kernel's limit: trust stops at five hand-proved rules over a small machine; a certificate is a tree re-checked by running it, and a refutation re-runs concretely; the enumerating `honest` check is independent of `certify` but can only refute, and a second certificate checker is small to write |
| G4 Execution | ◐ 65% (modelled, interpreted) | Target models are ordinary programs a claim can test, so the logic reaches the machine. The claim kernel can't make anything run fast: that needs the handoff to an external language (`research/OPTIMIZER.typ`'s single impure driver), which is outside it. Today: 15%, interpreted tree-walkers and no target model. | ½(certified, not run) · 1 · ½(cost report) — at the claim kernel's limit: a machine model is one more program, so a claim can test bounded runs of a target and certify assembly; running it fast needs the handoff, which a pure claim can't perform; `--stats` reports cost beside the run |
| G5 Search | ✅ 85% (cegis, limit) | `cegis` is a real search over candidates against a claim, and cost bounds are claims too, so the kernel carries the whole score. Two limits: nothing writes new claims yet, and Löb's theorem means a successor's soundness can't be proved from inside (`research/LOB_AND_CURRY.md`). This says what the design allows, not that search scales. Today: about 42%, `cegis` over a listed pool, checker only. | 1 · 1 · ½(counterexample pool) · ½(Löbian ceiling) — at the claim kernel's limit: `cegis` searches candidates against a claim, and cost bounds are claims too; kept counterexamples are adversaries, but nothing writes new claims; a successor's soundness can't be proved from inside, so self-application steps down a level each time |

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

Rules: a design gets no credit, the how-tag carries it; disp's own row is the one exception (see above). A ½ must name its route in the
cell — `½(contracts)`, `½(metaprograms)` — the specific half-credit condition it takes,
so no half score exists without a stated reason; the script rejects a bare ½. 1 and 0
stay bare. The symbol is derived: ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's on
that axis. `_SCORES.md` is the source; `scripts/awesome-scores.py --write` recomputes the
percentages and copies them into every write-up's scorecard (score, clause values, why), the
master table, and the table above, and its report ranks each column.
