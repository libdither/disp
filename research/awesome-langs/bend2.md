# Bend 2 / BendTT / BendRT — Victor Taelin, Higher Order Co

**Repo:** https://github.com/bendlang/bend (TypeScript + C, Apache-2.0; one squashed commit "Bend 2.0.4", 2026-09-17; 20,020★ inherited from the Bend 1 repo it was renamed from; verified 2026-09-17)
**Site:** https://bend-lang.com (higherorderco.com redirects there) · papers `paper/BendTT.pdf`, `paper/BendRT.pdf` (both written by an AI from the author's code, per their disclosure) · formalization `bend2/bend.lean`
**Predecessors:** Bend 1 (on HVM2, superseded, now `HigherOrderCO/Bend1`) and the runtime line in [`hvm4.md`](hvm4.md). The README: "Bend 1 programs and HVM do not carry over."
**Clone inspected:** yes (`~/Repos/bend2`) — `guide/GUIDE.md`, both papers' Typst sources, `bend2/bend.ts` (3,734 lines, the checker), `bend2/comp.ts` (6,232, compiler + C/Metal/CUDA/JS runtimes), `bend2/bend.lean` (20,981), `bench/*/_pin_`, `gates/`, `tests/`, `evals/`, `demos/`

## What it is

Python-shaped syntax over an **affine dependent type theory** (BendTT), compiled
to one C file that runs unchanged on CPU threads, Metal and CUDA (BendRT), with a
JavaScript backend for development. The pitch is "a fast language that blocks AI
mistakes via proof": a human states claims in `LAWS.bend`, an AI writes the code
and a `PROOF.bend` that fills each law with a def of the same name, and
`bend PROOF.bend` is the merge gate. No tactics, no inference, no unification.

## The three decisions that matter to disp

**1. Consistency from affinity, not from universes.** `Type : Type` holds and
there is no positivity check, and the theory is claimed consistent anyway. The
wall is usage: a binder is affine unless marked `+`, a `+` binder forms only over
a type of kind `Data`, and no function type is ever `Data`. So no closure is ever
copied, and Girard, Hurkens and Curry all die at the usage counter (each is a test
in the repo). Termination is one structural descent test on the definition's own
case tree; mutual recursion is forbidden; `@unsafe` opts out and is reported.
Types, erased arguments and equations are checked *dead*: they may diverge or
inhabit `Empty`, and nothing dead ever counts as live evidence. Equality is J with
a hand-written motive (`%e : P`), intensional, with no extensionality principle.
The Lean mechanization has 1,113 theorems and no `sorry` or axiom — confluence,
subject reduction, progress, weak normalization, consistency — of the *calculus*;
the file's own header says it does not yet match the shipped checker, and nothing
ties the two.

Why disp cares: disp's Q2 asks whether type checking can distribute over
duplication. Bend refuses the problem. Closures are never duplicated; reuse exists
only for first-order data and is licensed by the *kind of a type*, never by a
term. disp measured its distributes-over-duplication conjecture failing for
recognizers that project twice; Bend would reject such a recognizer unless its
argument is `Data`.

**2. Speed from ownership, not from sharing.** The BendRT paper says it outright:
readers expecting interaction nets will find "there are none". Affinity settles
ownership at compile time, so a `match` frees the node it opens, reference counts
exist only where a whole-program pass found `+` sharing, there is no garbage
collector and no C stack, and tasks live in a fixed 128×128 grid of rings with no
work stealing: the *program* promises that every parallel call splits its work
evenly, and the runtime declines to repair an unequal split. The same C text is
the CPU program and the GPU kernel. Self-reported on one Apple M4 Max:

| Measure | Bend 2 | Against |
|---|---|---|
| Sequential | 0.8 to 1.5× the time of a hand-written C twin | 16 benches |
| 16 threads | 8.8 to 12.1× over one thread | |
| Integrated GPU | 52 to 67× over one thread on uniform work | loses to 16 threads on n-queens, symbolic regression |

There is **no cost model**. The tests say it verbatim: "bend has no --stats"; the
pre-release counter was dropped, and the cost and stats test directories are
counter-less ports. Performance is policed instead by wall-time pins per machine
with a 1.15× tolerance gate in CI.

**3. The checker is fast because it does nothing clever.** One bidirectional pass
with a usage counter per binder, everything annotated, nothing inferred. On
generated files (pin of 2026-09-09):

| Bench | Bend | Lean | Rocq | Agda, Isabelle |
|---|---|---|---|---|
| 12,800 definitions | 0.30 s | 36.2 s | 6.0 s | > 300 s |
| 3,200 proofs | 0.83 s | > 300 s | 9.5 s | > 300 s |

The price is written into the README: verbose code, no type classes, no macros
beyond compile-time templates, "no tactics or proof search; proving theorems takes
extra effort."

## What is not there

- **No synthesis.** SupGen/NeoGen, the enumerative program synthesizer announced
  for years, is absent from the repo. The paid proving agent, Bender, is "a thin
  harness around models from Anthropic, OpenAI and others", with SupGen promised
  for a future update. The shipped search is an LLM in a retry loop against the
  checker.
- **No reflection.** No quote, no term inspection. Templates (`~f`) are parse-time
  re-instantiation of a def's source text with a closed argument substituted.
- **No evidence objects.** `bend.ts` calls itself "the trusted kernel"; checking is
  one pass and the output is a verdict string. Nothing re-checks, and the compiler
  is "99% AI-written and has not been fully audited yet".

## Scorecard

| Axis | Bend 2 | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✗ 0% (none) | No reflection: programs are never data, templates are textual, the checker is TypeScript. | 0 · 0 · 0 — no reflection: no quote, no term inspection; templates re-parse closed source text at compile time; the checker is TypeScript |
| G2 Specification | ◐ 65% (affine dependent) | Dependent types with `Type : Type`, laws proven by defs over a small Base with the `Equal` lemmas; quantities `&0/&1/&2` are a usage grade, not cost; equality is intensional J with a written motive. | 1 · ½(small library) · ½(usage grade) · ½(propositional) — affine dependent types with Type : Type, laws proven by defs over a small Base; quantities &0/&1/&2 are a usage grade; intensional J with a written motive, no extensionality |
| G3 Trust | ◐ 30% (one-file kernel) | The kernel is one human-written file, about a thousand lines of checking logic inside 3,734; proofs are defs it checks, no certificate is minted, nothing re-checks, the Lean model lags the checker, and the compiler's passes carry no evidence. | ½(one-file kernel) · ½(proof defs) · 0 · 0 — bend.ts is "the trusted kernel", one human-written file, checking in one pass; proofs are defs the kernel checks, no certificate minted; nothing re-checks, and the Lean model admittedly lags the checker |
| G4 Execution | ◐ 40% (native, GPU) | Near-C sequential, GPU through one C file; clang, Metal and CUDA trusted and the C runtime unverified; the cost counter was dropped. | 1 · 0 · 0 — 0.8–1.5× hand-written C sequential, 9–12× on 16 threads, up to 67× on the GPU, self-reported; clang, Metal and CUDA trusted and the C runtime unverified; the pre-release interaction counter was dropped ("bend has no --stats") |
| G5 Search | ◐ 40% (LLM loop) | Human `LAWS.bend`, AI-written `PROOF.bend`, checker as the gate; Bender wraps commercial models; cost is never in the loop; no search of its own. | ½(external LLM loop) · ½(checker only) · 0 — the shipped loop is a human LAWS.bend, an AI-written PROOF.bend and the checker as the gate (Bender wraps Anthropic/OpenAI models); the checker scores, cost never does; no search of its own |

## What disp could steal

1. **A whole-repo token budget as a CI gate.** `gates/repo.ts` allow-lists every
   tracked file and caps each at a token count (`bend.ts` 41k, `comp.ts` 61k, the
   guide 12k, each test 16k); anything else in the tree fails the build. Steal item
   14 (Thermite's budget on a generated reference) applied to the entire repo, so an
   agent can hold it.
2. **An eval arena in the tree.** `evals/` holds twenty law-proving tasks in five
   difficulty tiers, exempt from the caps ("the models' arena"). Steal item 4, a
   benchmark, shipped as a directory.
3. **The ownership split.** `LAWS.bend` is human-owned and the AI does not touch it;
   `PROOF.bend` is AI-owned; the checker's verdict on the second is the gate. Steal
   item 5 (an agent-facing interface) in its simplest form.
4. **The live/dead demand split.** Statements are free: at demand 0 a type may
   recurse without a termination check and quantify over anything. disp's types are
   programs that run, so this is not free for disp, but it names the boundary.
5. **Pins.** Per-hardware pinned medians, a tolerance gate, and a checksum every
   executor must print. Wall-time rather than disp's `cold_equiv`, but the
   discipline is the same.
6. **Refolding.** When a match sticks on a variable, the evaluator answers the
   definition applied to that variable, not the exposed case tree, so goals read in
   the vocabulary of the source.
7. **Speed by refusing inference.** The checker's numbers come from having no
   unification and no metavariables. disp's own iteration-speed priority points the
   same way.

## Where disp differs

- **Types are programs in disp and syntax in Bend.** disp's checker is an ordinary
  tree you apply; Bend's is one TypeScript file, and no Bend program can see or run
  it. Bend's whole product loop lives outside the language.
- **disp kept the cost counter; Bend threw it away.** disp's `--stats` steps and
  `cold_equiv` are the one G4 asset disp has, and Bend's runtime paper argues that a
  readable cost model was *gained* by dropping nets, then ships without one.
- **Equality.** Both are intensional. Bend accepts hand-written motives and no
  extensionality as the price of a one-pass checker; disp's Q1 is whether a
  decidable fragment can license enough rewrites to avoid that price.
- **Search.** Bend's is a commercial LLM with the checker as the filter, and its
  enumerative synthesizer never shipped. disp's optimizer is designed to be a
  program in the language it optimizes.

## Verdict

**The nearest shipped instance of the human-spec / AI-implementation / checker-gate
loop, with an original consistency argument and a real mechanization, built by
giving up the two things the old survey entry praised: the interaction-net runtime
and the cost counter.** It is a data point against disp on equality (also
intensional, shipped a proof language anyway) and for disp on cost (they dropped
it, disp kept it). Caveats: one squashed commit, so no history to audit; the papers
and the Lean proofs are AI-written by their own disclosure; the benchmarks are
unreproduced; the third-party notes site bend2.dev is stale and should not be
cited.

**Distance from disp's goals: ahead on G2 and on native speed, absent on G1, nothing on cost or on search of its own.**
