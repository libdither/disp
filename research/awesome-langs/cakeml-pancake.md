# CakeML + Pancake — Trustworthy Systems (UNSW), Chalmers, Cambridge

**Repos:** https://github.com/CakeML/cakeml (1,177★, v3400 released 2026-06-19, pushed 2026-08-03)
Pancake lives in the CakeML repo. https://trustworthy.systems/projects/pancake/
**Paper:** "Verifying Device Drivers with Pancake" (arXiv:2501.08249, revised May 2025)

## What they are

**CakeML** — a verified implementation of ML: the compiler is *proved correct in
HOL4*, and the proof extends down to the generated machine code. Famously
**bootstrapped**: the verified compiler compiled itself, so the binary you run
carries the correctness theorem.

**Pancake** — a new imperative, C-like systems language (2024–2026) for **formally
verified device drivers**, designed from scratch for verifiability. Semantics in
HOL4; user-level verification goes through **Viper** (SMT-backed separation logic);
compilation reuses CakeML's **verified backend**, so semantics are preserved to the
binary. A verified Ethernet NIC driver for LionsOS/seL4 Microkit took a first-year
PhD student ~3 person-months. New features landed February 2026 (function inlining).

## Why disp should care: this is A5 taken seriously

Everything else in this survey that claims systems performance either (a) trusts a
compiler (Verus trusts rustc, F* trusts KaRaMeL + the C compiler, ATS trusts the C
compiler) or (b) has no native backend at all. CakeML/Pancake is the only stack
where **the compiler itself is verified end to end**, so the guarantee that reaches
the running binary is not "we verified the source and hope the toolchain is
honest."

disp's GOALS bullet about hardware — "deterministic models of the base hardware
encoded into the type system to generate programs that perfectly exploit available
resources" — has two prerequisites, and this project supplies the first:

1. **A verified path from source semantics to machine code.** CakeML has it.
   (MM0/MMC has the other version of it; see `metamath-zero.md`.)
2. **A cost model faithful to real hardware.** Neither CakeML nor MM0 has this;
   it's disp's Q4 and nobody has answered it.

Pancake also demonstrates the *staging* strategy disp needs for A2: rather than
requiring dependent types for everything, it uses a separation-logic frontend
(Viper) for the properties drivers actually need, and gets tractable proof effort
(3 months for a real NIC driver by a beginner) as the payoff.

## Scorecard

| Axis | CakeML/Pancake | Note |
|---|---|---|
| A1 Reflection | ✗ | Conventional verified compiler; no programs-as-data. |
| A2 Spec power | ◐ | HOL4 for the compiler proof (very strong); Viper/separation logic for user code (decidable fragment). Not dependent types in the user language. |
| A3 Kernel | ✅ | HOL4's LCF kernel — the original LCF architecture disp's §4 descends from. |
| A4 Equality | ◐ | Compiler correctness *is* a semantics-preservation relation — the most industrial-strength "these two programs are equivalent" machinery in existence, but proved per-compiler-pass by humans, not searched. |
| A5 Perf | **✅** | Verified native code. Pancake drivers are performant and run in production-ish settings (LionsOS/seL4). |
| A6 Search | ✗ | None. All proofs and code are human-written. |

## What disp could steal

- **Compiler passes as the model for `~_T`-licensed rewrites.** CompCert and CakeML
  prove *exactly* the thing disp's optimizer needs to prove for each rewrite:
  source and target behave the same. Their per-pass simulation proofs are a library
  of worked equivalence arguments, and their structure (simulation relations,
  forward/backward preservation) is the shape disp's licensing relation should
  take. This is the most under-exploited resource for disp's Q1.
- **Pancake's scoping decision**: pick one high-value domain (device drivers),
  restrict the language to what that domain needs, and use a decidable frontend.
  This is the same lesson as CryptOpt's — disp's first real win should be
  domain-scoped.
- **Bootstrapping-with-a-theorem** as the template for disp's §14 self-application
  milestone: the meaningful version of "the optimizer optimized itself" is "the
  artifact carries a proof that it is the compilation of its own source."

## Where disp differs

These are *human-driven* verification projects with no automation ambitions. The
proofs are large, expensive, and written by experts (seL4-adjacent culture). disp's
entire premise is that this labor is what should be automated. CakeML/Pancake is
therefore both the quality bar for A5 and the cost demonstration that motivates
disp's A6.

## Verdict

**The gold standard for "verified all the way to the binary," and the best
available library of worked program-equivalence proofs — which is disp's Q1
problem in a different notation.**

**Distance from disp's goals: ahead on A5's verification depth, absent on A1 and
A6, and deliberately human-powered.**
