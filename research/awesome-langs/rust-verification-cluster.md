# The Rust verification cluster — Creusot, Flux, Kani, Aeneas, hax, RefinedRust

All verified active on GitHub as of 2026-08-03.

| Project | ★ | Pushed | Approach |
|---|---|---|---|
| [Kani](https://github.com/model-checking/kani) | 3,275 | 2026-08-03 | Bounded model checking (CBMC) for Rust; AWS |
| [Creusot](https://github.com/creusot-rs/creusot) | 1,817 | 2026-08-03 | Deductive verification via Why3 + SMT; prophecy-based mutable borrows |
| [Flux](https://github.com/flux-rs/flux) | 900 | 2026-08-03 | Refinement types on Rust, liquid-style inference |
| [Aeneas](https://github.com/AeneasVerif/aeneas) | 888 | 2026-08-03 | Rust → pure functional model in **Lean**/F*/Rocq via Charon |
| [hax](https://github.com/cryspen/hax) | 463 | 2026-08-03 | Rust → **F\***/Rocq/ProVerif; powers libcrux (ships in Firefox) |
| [RefinedRust](https://gitlab.mpi-sws.org/lgaeher/refinedrust-dev) | — | 2026-08-03 | Refinement types + **Iris** separation logic, foundational proofs in Rocq; used by IBM's ACE-RISCV |

## Why they get one file

Individually these are tools, not language designs. Collectively they are the most
important *strategic* fact in this survey: **the industry's answer to "verified
systems code" is not a new language — it is Rust plus a verification layer.**

Six independent, well-funded, actively-developed attempts, all converging on the
same architecture: keep the fast mainstream language, bolt on specs, discharge them
either by SMT (Creusot, Flux, Kani) or by translating to a proof assistant
(Aeneas → Lean, hax → F*, RefinedRust → Rocq/Iris).

## What this means for disp

**1. The two-layer architecture won.** disp's FOUNDATIONS argues for a unified
substrate where the type system, the checker, and the optimizer are all programs in
one calculus. The market has instead settled on *performance substrate + separate
proof layer*, and it works well enough to ship verified crypto in Firefox
(hax/libcrux) and verified confidential-computing monitors (RefinedRust/ACE-RISCV).

disp's counterargument is real and should be stated explicitly whenever this comes
up: the two-layer approach can never satisfy disp's §3 requirement, because **the
optimizer is written in a different language from the programs it optimizes** and
therefore can never be aimed at itself. Every project in this table is structurally
incapable of A6's self-application. That is disp's actual differentiator, more than
any individual mechanism.

**2. The Rust→Lean pipeline is the practical fallback.** If disp's Q1 fails, the
honest alternative for "fast verified code" is: write Rust, translate with
Aeneas/Charon, prove in Lean, close obligations with commercial AI provers. That
pipeline is in *production* (Runtime Verification + Ethereum Foundation zkEVM,
arXiv:2605.30106, May 2026), and its published verdict on AI provers is worth
quoting for calibration: they are a "productivity multiplier" — good at structural
lemmas and linear arithmetic, **weak at domain-specific algebra and loop-invariant
discovery**, and **spec design stays human**.

That last finding matters for disp's A6. The hard part is not proof-closing (AI is
getting good at it); it is *invariant discovery* and *spec design* — which is
exactly what disp's optimizer would have to do to synthesize implementations, not
just prove them.

**3. Kani's stdlib challenge is a data point on scale.** 16 months in: 725 manual
harnesses, 16,748 auto-generated ones, 989 functions verified against contracts —
but contract coverage of the unsafe core is still ~4%, and momentum plateaued
around October 2025 (arXiv:2606.17374). Verifying an *existing* large codebase is
brutally hard even with major funding. Relevant to any disp claim about
retrofitting.

## Scorecard (cluster-level)

| Axis | Cluster | Note |
|---|---|---|
| A1 Reflection | ✗ | None. Structurally impossible in the two-layer design. |
| A2 Spec power | ◐–✅ | Ranges from refinement types (Flux) to full Iris separation logic (RefinedRust) and Lean (Aeneas). |
| A3 Kernel | ◐ | Aeneas/hax/RefinedRust inherit real kernels (Lean/F*/Rocq). Kani/Creusot/Flux trust SMT. |
| A4 Equality | ◐ | Aeneas's functional translation of borrows is a genuine semantic-equivalence achievement, but per-tool and human-directed. |
| A5 Perf | **✅** | It is Rust. Nothing to prove. |
| A6 Search | ◐ | Kani's Autoharness auto-generates harnesses; otherwise human-driven with AI assistance. |

## What disp could steal

- **Aeneas's functional translation of borrows.** Turning imperative
  pointer-manipulating code into a pure functional model *automatically* is a real
  A4-adjacent result: it is a machine-produced behavioral-equivalence argument. If
  disp ever needs to reason about mutable low-level code, this is the technique.
- **The production verdict on AI provers** (structural lemmas yes, invariant
  discovery no) as the realistic prior for disp's proposer.
- **Autoharness** as a model for machine-generated verification obligations.

## Verdict

**The competition, and the fallback.** disp is not competing with these on A5 or
A2 — it is competing on the claim that a unified reflective substrate enables
self-application that none of them can ever do. That claim is disp's whole reason
to exist, and this cluster is the reason it has to be stated sharply.

**Distance from disp's goals: maximal on A1 and A6, minimal on A5.**
