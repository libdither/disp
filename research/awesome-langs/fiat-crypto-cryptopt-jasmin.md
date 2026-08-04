# fiat-crypto + CryptOpt + Jasmin/EasyCrypt — the working superoptimizers

**Repos:**
- https://github.com/mit-plv/fiat-crypto (836★, pushed 2026-08-03) — Adam Chlipala's group, MIT
- https://github.com/0xADE1A1DE/CryptOpt (70★, **last pushed 2024-06-26** — unmaintained)
- https://github.com/jasmin-lang/jasmin (362★, pushed 2026-08-03) + EasyCrypt — Formosa Crypto
- https://github.com/mit-plv/bedrock2 (335★, pushed 2026-08-03) — verified low-level language in Rocq

## Why these get one file

FOUNDATIONS §12–13 names equality saturation, translation validation, and
superoptimization as disp's lineage, and notes the graveyard reason:
superoptimization *"has never scaled past tiny straight-line kernels."*

This cluster is the **exception that proves the rule** — the only place where
"spec in, search runs, proof certifies, output is the fastest code in the world"
actually ships. It is the working precedent for disp's A6, and its limits are
disp's warning label.

## The pipeline that works

**fiat-crypto**: you write a high-level specification of modular arithmetic in
Rocq/Coq; the framework *generates* field-arithmetic implementations
correct-by-construction, with the correctness proof produced alongside the code.
Output is deployed in **Google BoringSSL** — i.e. in Chrome and Android TLS.

**CryptOpt**: takes fiat-crypto's output and runs **randomized search over x86-64
assembly** with **on-CPU benchmarking as the fitness function**, then connects the
result back with a **formally verified (Coq) equivalence checker**. It beats GCC
and Clang on field arithmetic. PLDI 2023 Distinguished Paper.

That is disp's architecture, exactly: *untrusted clever search + trusted
re-checker + real measured cost as the objective*. It exists, it won an award,
and its output runs on billions of devices.

**Jasmin/EasyCrypt**: assembly-level language with a **formally verified compiler**
(in Rocq) plus functional-correctness and constant-time proofs in EasyCrypt.
libjade is a fully verified crypto library; this is the pipeline behind verified
post-quantum ML-KEM. Here the "optimizer" is a human writing Jasmin — the verified
compiler guarantees the assembly matches.

**bedrock2**: a C-like language with a verified compiler and program logic
*entirely inside Rocq*, targeting verified firmware down to RISC-V binaries.

## What this cluster proves, and what it doesn't

**Proves:** disp's §12 architecture is sound and practical. An arbitrarily clever,
completely untrusted search procedure whose every output is re-validated by a small
trusted checker produces code that is simultaneously *provably correct* and
*faster than the best compilers*. The cost model can be real measured wall-clock
(CryptOpt benchmarks on the actual CPU), which is disp's Q4 answered affirmatively
— **in a narrow domain**.

**Doesn't prove:** any of it generalizes. Every success here is **straight-line
field arithmetic** — no loops, no branches, no data structures, no allocation. The
search space is small enough for randomized mutation to work. Nobody has scaled
this to a program with control flow, which is disp's Q3 and the wall FOUNDATIONS
§13 names.

And a live data point on durability: **CryptOpt has been unmaintained since
June 2024**. The one project that most exactly matches disp's endgame architecture
stopped. That is worth understanding before disp builds the same thing at larger
scope.

## Scorecard

| Axis | This cluster | Note |
|---|---|---|
| A1 Reflection | ◐ | fiat-crypto/bedrock2 live inside Rocq, so program synthesis is Gallina metaprogramming — reflective in the deep-embedding sense. |
| A2 Spec power | ✅ | Full CIC (Rocq). Specs are dependent types. |
| A3 Kernel | ✅ | Rocq's kernel; CryptOpt's equivalence checker is verified *in* Rocq. Textbook A3. |
| A4 Equality | ◐ | A verified **equivalence checker** for straight-line assembly — a real, decidable, machine-checked licensing relation, but only for a tiny fragment. |
| A5 Perf | **✅** | Beats GCC/Clang. Measured on-CPU. Shipping in BoringSSL. The best A5 result in this survey. |
| A6 Search | **✅** | Randomized search with a real cost objective and a proof-carrying output. Narrow, but complete. |

## What disp could steal

- **CryptOpt's structure, verbatim, as disp's first A6 milestone.** disp's
  make-or-break Q1 asks for *one* certified `~_T`-licensed rewrite re-checked by
  the trusted kernel. CryptOpt is the template: pick a domain small enough that
  search terminates, make the equivalence checker verified and cheap, use measured
  cost as the objective. disp should aim its first optimizer result at a
  CryptOpt-shaped problem rather than at general programs.
- **On-CPU benchmarking as the fitness function** — the direct answer to disp's Q4
  (proxy vs. hardware fidelity): don't model the hardware, *run on it*, and let the
  cost signal be measured rather than estimated. disp's GOALS bullet 2 (a primitive
  that returns time/memory alongside results) is precisely the plumbing needed to
  do this, and it is currently unbuilt.
- **The verified-equivalence-checker-as-a-component pattern**: the checker doesn't
  need to be general, only *sound and fast on the fragment the optimizer emits
  rewrites in*. That reframing may make disp's Q1 much more tractable than "find a
  decidable fragment of `~_T` rich enough for everything."

## Where disp differs

disp wants this for **general programs with control flow**, specified by
**dependent types** rather than a reference implementation, on a **reflective
substrate where the optimizer can be aimed at itself**. Each of those three
extensions is a research program, and the cluster's history says the third is the
least studied and the first is where everyone has stalled.

## Verdict

**The existence proof for disp's endgame architecture, at 1/1000th of disp's
intended scope — and a caution that this exact combination has already been built,
celebrated, and abandoned once.** The right lesson is scoping: disp's first
demonstrable win should look like CryptOpt, not like a general optimizer.

**Distance from disp's goals: same architecture, narrow domain, no reflection, no
self-application.**
