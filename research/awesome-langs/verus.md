# Verus — Microsoft Research / secure-foundations

**Repo:** https://github.com/verus-lang/verus (2,806★, 95 contributors, rolling release 2026-08-03)
**Ecosystem (all verified active):** `verified-memory-allocator`, `verified-ironkv`,
`verified-node-replication`, `secure-foundations/vest` (verified parsers),
`microsoft/verismo` (verified AMD SEV-SNP security module), `anvil-verifier/anvil`
(verified Kubernetes controllers), `verus-analyzer`, `verusfmt`.

## What it is

Verified Rust. You write ordinary Rust plus `requires`/`ensures`/`invariant`
clauses and ghost code; an SMT solver (Z3) discharges the obligations; the result
compiles with rustc and runs at native Rust speed. Specifications are written in
Rust syntax, so there is **no extraction gap** — the verified artifact *is* the
shipped binary.

## Why it's the current best answer for "AI iterates on a spec"

This is the ecosystem where disp's A6 has actually been demonstrated end-to-end:

- **AlphaVerus** (CMU, ICML 2025) — bootstraps verified code generation with
  zero human-labeled data: translate from a higher-resource language, refine with
  "Treefinement" tree search against verifier feedback, filter spec-gaming with a
  critique phase.
- **AutoVerus** (SOSP 2025) — correct proofs for **>90% of VerusBench**, half in
  <30s or ≤3 LLM calls. Microsoft ships `verus-proof-synthesis` (113★).
- **Inductive Deductive Synthesis** (Berkeley + Google, arXiv:2605.23109, May 2026)
  — the single most disp-relevant result in this survey: joint code+proof synthesis
  with **performance benchmarks inside the verification loop**, autonomously
  producing verified distributed key-value stores for **7/7 consistency specs**
  (~6.8h, ~$106/spec) where Claude Code and Codex each managed 2/7 — and producing
  implementations **up to 3× faster than published human-verified systems**.

That last result is disp's stated goal — spec in, correct *and fast* implementation
out, machine-generated — achieved for a real distributed-systems workload, using
an LLM as the proposer and Verus as the 0/1 oracle. It is the strongest evidence
in existence that the FOUNDATIONS §15 paradigm works on systems code.

## Scorecard

| Axis | Verus | Note |
|---|---|---|
| A1 Reflection | ✗ | No programs-as-data. Ghost code is erased, not inspectable. The checker is not a Verus program. |
| A2 Spec power | ◐ | Rich first-order + quantifiers + linear ghost state; **not** dependent types. Deliberately SMT-shaped ([P1] retreat). |
| A3 Kernel | ✗ | TCB = Verus + Z3 + rustc. No LCF discipline, no certificates. |
| A4 Equality | ◐ | SMT-fragment equality; no rewrite-licensing relation, no optimizer. |
| A5 Perf | **✅** | *It is Rust.* Native, zero-overhead, real kernels and allocators verified and shipped. Best A5 in the survey. |
| A6 Search | **✅** | Best-demonstrated spec→implementation loop for systems code, with cost in the loop (IDS). |

## What disp could steal

- **The IDS loop design** (arXiv:2605.23109) is close to a blueprint for disp's
  external optimizer: a partial-proof oracle, a programmatic interface, executable
  extraction, and *performance measurement inside the scoring function*. Its
  authors note the backend requirements are met by both Verus and Lean — disp
  should check itself against that same requirements list.
- **VerusBench-style evaluation.** disp has no benchmark for "did the optimizer
  find something." Verus's benchmark culture is why its agentic results are
  legible.
- **Spec-gaming defenses.** AlphaVerus's critique phase exists because models
  learn to write vacuous specs that pass. disp's checker-as-0/1-score has exactly
  this failure mode and no defense designed yet.

## Where disp differs

Verus is the **anti-disp on A1–A4** and the **proof of concept for A6**. It
succeeds by giving up everything disp considers foundational: no reflection, no
dependent types, no small kernel, no equality theory — just Rust plus an SMT
solver plus an enormous TCB. And it works, today, on real kernels.

The uncomfortable question this poses for disp: *if the goal is "spec in, fast
verified code out," and Verus+LLM already does it for distributed KV stores, what
does the tree-calculus substrate buy?* disp's answer must be some combination of:
(a) dependent specs express things SMT-shaped specs cannot; (b) the optimizer can
be aimed at itself, which Verus's can never be (its "optimizer" is a frontier LLM
in someone else's datacenter); (c) enumerative/superposition search is cheaper
than $106/spec of LLM inference. All three are plausible; none is demonstrated.

## Verdict

**The system to beat on results, and the system to learn from on evaluation.**
Verus + agentic scaffolding is the current state of the art for disp's actual
goal, arrived at from the opposite architectural direction.

**Distance from disp's goals: furthest on substrate philosophy, closest on
delivered outcomes.**
