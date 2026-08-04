# Dafny — AWS / Microsoft Research

**Repo:** https://github.com/dafny-lang/dafny (3,488★, 111 contributors, pushed 2026-08-03)
**Note:** commit velocity has slowed (14 commits in the last 3 months vs. 100+ for Verus/Lean/F*) — mature rather than fast-moving.

## What it is

A verification-aware imperative language: `requires`/`ensures`/`invariant`/`decreases`,
discharged by Z3, compiling to C#/Java/Go/JS/Python. A decade of production use at
AWS (authorization, storage, cryptographic protocols).

## Why disp should care: Dafny is the empirical ceiling for "AI writes verified code"

This is the one axis where Dafny leads everything, and it is directly relevant to
disp's A6 proposer question:

| Benchmark result | Language | Source |
|---|---|---|
| **82.2%** vericoding success (Claude Opus 4.1) | **Dafny** | arXiv:2509.22908, Sep 2025 |
| 44.2% (GPT-5 best) | Verus | same |
| 26.8% (GPT-5 best) | Lean | same |
| **92.7%** DafnyBench (AxDafny, agentic) | Dafny | arXiv:2606.32007 |
| 86% DafnyBench (DafnyPro, Claude Sonnet 3.5) | Dafny | arXiv:2601.05385 |
| Pure Dafny verification: **68% → 96% in one year** | Dafny | vericoding paper |

Models are dramatically better at Dafny than at Verus or Lean. The vericoding
authors attribute the Lean gap to LLMs being trained on *math* theorem proving
rather than *program* verification, and the Verus gap to ghost-vs-exec type
mapping and machine-level obligations (overflow, etc.).

**The lesson for disp is uncomfortable and worth stating plainly:** the languages
models write best are the *simplest, most conventional, most SMT-shaped* ones —
not the most expressive. Dafny wins because its spec language is small, its
failure modes are legible, and there are years of public examples. disp's tree
calculus, types-as-predicates, and hypothesis-minting kernel are the opposite of
all three properties: novel, unusual, and with essentially zero training data.

If disp's optimizer ever wants a neural proposer, this is a real cost — and it is
an argument for the *enumerative/superposition* proposer (HVM4-style) over the
neural one, since search doesn't care how unusual your syntax is. FOUNDATIONS §15
treats the neural proposer as the newly-available component that makes the endgame
thinkable; Dafny's numbers suggest that component arrives with a strong bias
toward conventional languages.

## Other things worth noting

- **AWS production use** demonstrates the "spec is the durable artifact" workflow
  at organizational scale — the Cedar authorization language is specified in Dafny
  and Lean and re-verified continuously.
- **Compilation is to managed backends** — so Dafny is a verified-logic tier, not a
  systems-performance endpoint. It fails A5 by construction.
- The **midspiral/LemmaScript** experiment (annotated TypeScript → Dafny *or* Lean)
  is a live indie attempt to use Dafny as a verification IR behind a familiar
  surface — the "verification-aware intermediate language" framing (arXiv:2501.06283).

## Scorecard

| Axis | Dafny | Note |
|---|---|---|
| A1 Reflection | ✗ | None. |
| A2 Spec power | ◐ | First-order + quantifiers; no dependent types. Deliberately SMT-shaped. |
| A3 Kernel | ✗ | TCB = Dafny + Boogie + Z3. Large. |
| A4 Equality | ◐ | SMT-fragment only. |
| A5 Perf | ✗ | Managed backends. Not a systems language. |
| A6 Search | **✅** | The best *LLM-writability* of any verification language, by a wide margin. |

## What disp could steal

- **The benchmark culture.** DafnyBench made progress measurable, and measurable
  progress attracted the work that took it from 68% to 96% in a year. disp has no
  benchmark for its optimizer. Building a small one — N specs, a scoring harness,
  a public number — is probably worth more than any single feature.
- **Legible failure modes as a design goal.** Dafny's advantage is partly that when
  verification fails, the message is comprehensible and localized. disp's `Ok false`
  is not.

## Where disp differs

Everything except the goal. Dafny is the "retreat to a decidable SMT-shaped
fragment" strategy FOUNDATIONS calls out under [P1], executed extremely well, with
no reflection, no kernel discipline, no performance story, and no self-application.

## Verdict

**Not a design competitor — a *calibration instrument*.** Dafny tells disp what
the AI-writability ceiling looks like and what it costs (conventionality). Its
numbers are the strongest available argument for disp preferring a search-based
proposer over a neural one.

**Distance from disp's goals: opposite on A1–A5; the reference point for A6's
neural half.**
