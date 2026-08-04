# Vow — Paulo Matos (solo; veteran Igalia compiler engineer)

**Repo:** https://github.com/vow-lang/vow (6★, 956 files, created 2026-02-25, pushed 2026-08-01)
**Companion:** `pmatos/forseti` — "formal verifier inside the agent coding loop… Q.E.D. is the new LGTM"
**Authorship verified:** Paulo Matos wrote 87 of the last 100 commits (rest: dependabot/semantic-release).
**Clone inspected:** yes — `README.md`, `docs/vow_design.md`, `stdlib/`, `compiler/`

## What it is

An **agent-first** systems language: Cranelift native codegen, linear types,
self-hosted with a byte-identical bootstrap. Every function carries machine-checked
contracts (preconditions, postconditions, loop invariants) discharged at compile
time by **ESBMC** bounded model checking.

The design document is unusually explicit about the thesis:

> Agents are good at generating and transforming code. Humans need a stronger basis
> for trust than style, tests, or review alone. Formal contracts and mechanical
> verification are the scalable trust mechanism for agent-produced software.

And the tagline: *"The syntax is not for you… Yours is only the product."*

## Why it matters to disp: the loop is the product

Vow is the only project in this survey that treats **the agent-facing interface as
part of the language design**, and it is worth reading `docs/vow_design.md` §2 for
how far that goes:

- The compiler emits **structured JSON** designed for machine consumption, enabling
  an explicit **CEGIS** loop: *write code → compile → verify → read counterexamples
  → fix → iterate*.
- Design goals are stated as agent-operability properties: an agent must be able to
  "infer what the code means from local structure, predict compiler and verifier
  behavior, produce code in a single preferred form, repair failures using
  structured feedback."
- **"Single canonical way"** is a governing principle — one representation per
  concept, because it "reduces diff noise, simplifies synthesis and repair, and
  makes the output of multiple agents easier to compare mechanically."
- Deliberate non-goals: human ergonomics, macros, user-defined generics,
  extensibility. *"Agent-generated duplication is often a cheaper cost than
  expanding the language and verifier surface."*
- The self-hosting sufficiency test is explicitly an agent-synthesis test: the
  validation target is **"a Vow compiler, written in Vow, largely produced and
  maintained by agents, and verified by the same contract system."**

That last line is a self-application loop of the same *shape* as disp's §14 — the
system's own implementation is the thing the automated producer must generate and
the system's own checker must certify — with an LLM in the proposer slot instead
of a search procedure.

## Scorecard

| Axis | Vow | Note |
|---|---|---|
| A1 Reflection | ✗ | Deliberately not: macros and metaprogramming are *excluded* to keep the verifier tractable. Directly opposed to disp's A1. |
| A2 Spec power | ◐ | Contracts + loop invariants. Not dependent types; **bounded** model checking, so guarantees are bounded-depth, not universal. |
| A3 Kernel | ✗ | TCB = vowc + ESBMC. No certificates, no LCF split. |
| A4 Equality | ✗ | Not addressed. |
| A5 Perf | ✅ | Cranelift native codegen, linear types, self-hosted with byte-identical bootstrap. Real systems performance. |
| A6 Search | ◐ | CEGIS with an external LLM as proposer. No internal search, no cost objective, no self-optimization. |

## What disp could steal

- **Structured machine-readable verifier output as a first-class artifact.** disp's
  checker returns `Ok true`/`Ok false`/`Err`. For any proposer — neural or
  enumerative — the *counterexample* and the *reason* are the training signal.
  Vow's JSON-for-agents is the cheapest idea here and disp has no equivalent.
- **"Single canonical way" as a synthesis-friendliness principle.** disp already
  has this property structurally (deterministic elaboration → same type, same
  tree, O(1) conversion) but has never stated it as a *search* advantage. It is
  one: a canonical form collapses the candidate space the optimizer must explore.
- **The self-hosting-as-sufficiency-test framing** for staging disp's own bootstrap.

## Where disp differs

Vow bought agent-operability by **removing power**: no macros, no generics, no
reflection, bounded (not unbounded) verification. disp is betting the opposite —
that maximum reflective power plus a tiny kernel is what makes automation possible,
because the optimizer needs to manipulate programs as data.

The other gap is the proposer. Vow's loop depends on a frontier LLM being smart
enough; disp's design wants enumerative/superposition search steered by
reverse-mode credit assignment, which is cheaper per candidate and can be aimed at
itself. Vow can never optimize its own compiler *by search* — it can only ask a
model to try again.

## Verdict

**The best-executed "language designed for the agent loop" in existence, by a
credible compiler engineer, and almost entirely disjoint from disp on substrate.**
Its value to disp is the *interface* layer — what the verifier hands back to
whatever is proposing — which disp has not designed at all.

**Distance from disp's goals: shares the motivation and A5; opposite on A1;
weaker A2/A3; A6 outsourced to an LLM.**
