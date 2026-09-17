# Thermite — dollspace-gay + Maxine Levesque (two people; heavily agent-authored)

**Repo:** https://github.com/Corvidae-Coding-Projects/Thermite (54★, created 2026-06-04, pushed 2026-08-08)
**Active branch:** https://github.com/maxinelevesque/Thermite3-staging (0★, default branch `staging`, pushed 2026-09-13) — a declared staging fork for "Thermite 3", pending upstream review of RFC-6 (#128) and RFC-7 (#129); work merges upstream by PR
**Authorship verified:** dollspace-gay 527 commits, maxine-at-forecast 171 upstream; maxinelevesque 200 more on the fork; the history is `codex/` agent branches end to end
**Clone inspected:** yes (staging fork) — `README.md`, `thermite-design.md`, `RATIONALE.md`, `goal.md`, `lean/` (96 files, ~26,500 lines), `conformance/`, the RFC issue bodies

## What it is

A contract-mandatory verification language lowered to [Verus](verus.md)-annotated
Rust, designed explicitly so that **agents, not humans, are the authors**. The
design doc's thesis is the cleanest statement of the agent-economics argument in
this survey:

> AI agents invert the economics… Thermite is the arbitrage: burn the cheap
> resource (tokens) to buy the expensive one (trust). It is unpleasant for humans
> to write, by design; humans are not the intended author.

Every function carries `requires` / `ensures` / an effect row (`!`); a CLI
(Forge) checks them through a five-level assurance ladder — L4 admitted decidable
routes with **checked certificate reconstruction** (bit-vector and finite-EPR
clauses replayed through CaDiCaL + drat-trim into axiom-probed Lean), L3
all-input proof via Verus/Z3 or Lean, L2 bounded Kani/CBMC with the bound
recorded, L1 always-active runtime checks, L0 trust-by-fiat through a greppable
`#[slag]` annotation. The verdict lands in a machine-readable assurance manifest
with a **per-clause engine and trust profile**; declared effects also compile to
a seccomp filter that confines the built executable. Timeouts and unsupported
shapes are named outcomes — "never laundered into a proof."

Three things make it more than another contracts-on-Rust project:

- **The Lean spine is real.** ~26,500 lines, sorry-free by enforced policy, with
  a CI axiom probe that fails unless every proof rests on exactly
  `{propext, Classical.choice, Quot.sound}`. It carries a denotational model,
  soundness proofs for the lowering fragment, and the certificate reconstruction.
- **The trust base is enumerated, not minimized.** `make audit` re-derives the
  whole chain: the axiom probe, translation-validation batteries on the lowering,
  correspondence drift checks, independent Verus replay, and a complete inventory
  of every fiat-trusted `#[slag]` block — Leroy's "verification never eliminates
  the trusted base, it reduces it to an enumerable set," made per-function.
- **The whole language fits in a skill, enforced in CI.** The canonical reference
  `THERMITE.skill.md` is *generated from the compiler's own exhaustive match
  arms* and registries, and CI holds it under a 6,000-token budget: "any feature
  that doesn't fit doesn't ship."

The staging fork is where the language moves now. RFC-6 renames every clause
keyword to a full third-person verb (`inv` → `keeps`, `dec` → `measures`) on the
argument that abbreviations *misdirect* language models — "in a language where a
misread clause yields a vacuous proof rather than a compile error, that is a
safety property" — with the migration built as a spike and the migrated corpus
re-certified identically before asking for review. RFC-7 proposes a whole
surface generation on one organizing rule (every clause is a verb whose subject
is the item), and its appendix honestly reports probes that contradict the
docs, including a soundness-adjacent one: an effect row naming a nonexistent
region still certifies at L3.

## Why it matters to disp: two of the steal-list items, already built

Thermite is [Vow](vow.md)'s agent-first thesis executed with real verification
machinery, and it is the existence proof for two recommendations this survey
already makes:

- **Steal item #5 (structured, machine-readable checker output):** per-obligation
  JSON with concrete counterexamples, plus a fallback ladder where every degrade
  is a named, recorded outcome rather than a silent downgrade.
- **Steal item #7 (a CI-enforced axiom/trust ledger):** the assurance manifest +
  `make audit` + the `#[slag]` inventory is exactly that, and it is the project's
  central design idea rather than an afterthought.

The genuinely new idea is the **token-budgeted generated reference**: the
language surface is defined to be teachable to an LLM in ≤ 6,000 tokens, the
reference is derived from the compiler so it cannot drift, and the budget is a
CI gate that vetoes features. That is a language-design constraint no other
project in this survey has, and it is directly relevant to a language (disp)
that expects agents to write most of its code.

The development process is also part of the artifact: design docs are the
authority chain (`thermite-design.md` → per-component REQ/AC contracts → impl →
conformance corpus), gates enforce doc-drift and requirement-registry
consistency, and golden certificates are hand-authored, never regenerated from
the toolchain's own output. It is the most disciplined agent-run repository
inspected for this survey.

## Scorecard

| Axis | Thermite | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✗ 0% (none) | No reflection at all: programs are never data, Forge is an external CLI, and the lowering target is Rust source. | 0 · 0 · 0 — no reflection; Forge is an external tool and programs are never data |
| G2 Specification | ◐ 40% (contracts, ladder) | First-order contracts + loop invariants + effect rows, discharged along the L4–L0 ladder. `measures` is a termination bound, not cost; the Lean spine proves the *toolchain's* metatheory, not user propositions. | ½(contracts) · ½(solver-discharged) · 0 · ½(SMT-fragment) — contracts and invariants, solver-discharged along a recorded ladder; termination measures but no cost or resource types; SMT-fragment equality |
| G3 Trust | ◐ 38% (trust ledger) | No small core — L3 trusts Verus+Z3+rustc — but the L4 fragment replays solver certificates through axiom-probed Lean, and the audit chain (translation validation, independent replay, the `#[slag]` inventory) makes the trusted base enumerable per clause; no rewrite carries a license. | ½(L4 via Lean) · ½(fragment evidence) · ½(audit replay) · 0 — L3 trusts Verus+Z3+rustc while L4 replays certificates through axiom-probed Lean; the manifest is a report, the Lean proofs are evidence for a fragment; audit re-derives the chain with independent replay but SMT verdicts carry nothing |
| G4 Execution | ◐ 55% (via rustc) | Native through rustc/LLVM; the lowering carries translation-validation batteries rather than a verified compiler; no cost accounting. | 1 · ½(validated fragment) · 0 — native via rustc; translation-validated lowering for a fragment; no cost model |
| G5 Search | ◐ 40% (agent loop) | `forge goal` / `forge fill` + the generated skill are a purpose-built external-LLM loop with the checker (and mutant-kill scoring) in it; no cost objective, not aimed at itself. | ½(agent loop) · ½(checker only) · 0 — a purpose-built external-agent loop with the checker and mutation scoring in it; no cost objective |

## What disp could steal

- **The CI-enforced token budget on a generated reference.** disp's eventual
  agent-facing surface could hold itself to the same discipline: derive the
  reference from the elaborator's actual vocabulary, and let a token budget veto
  surface growth. The derivation-from-the-compiler half is what makes the budget
  honest — a hand-written doc under a budget just compresses prose.
- **Per-clause trust profiles in the verdict.** disp's checker says accepted or
  not; Thermite's manifest says *which engine, at which level, trusting what*
  for every clause. When disp grows multiple discharge routes (kernel replay,
  decidable fragments, `.opt.disp` licenses), a verdict that names its route per
  obligation is the difference between an auditable claim and a green light.
- **Named degrade outcomes.** A timeout that is a first-class outcome, distinct
  from refutation and from acceptance, is the honest version of what any
  budgeted checker has to do.
- **RFC-6's keyword argument** — full verbs over abbreviations because
  *pretraining overlap is load-bearing* for agent-written languages — is worth
  weighing against disp's own terse surface.

## Where disp differs

The same structural divide as Vow, executed better: Thermite bought
agent-operability by shrinking the language to what Verus/Z3 can discharge, and
its optimizer-shaped hole is filled by a frontier model reading JSON. There is
no substrate — programs are never data, so nothing like disp's self-applied
search is even expressible. And where disp wants one tiny kernel whose verdicts
are cheap to believe, Thermite accepts a large heterogeneous trusted base and
spends its effort making that base *legible* — an enumerated ledger where disp
wants a two-op core. Both are trust stories; only one of them can ever aim a
verified rewrite at its own implementation.

## Verdict

**Vow's thesis with Verus-grade machinery and the survey's trust-ledger and
structured-output recommendations already shipped; three months old, two
people, and the most disciplined agent-run repo inspected here.** Its value to
disp is the interface-and-audit layer — the budgeted generated skill, per-clause
trust profiles, named degrades — not the foundations.

**Distance from disp's goals: shares the agent thesis and G4's substrate-by-rust;
nothing on G1; ledger-not-kernel on G3; G5 outsourced to an LLM with the checker
in the loop.**
