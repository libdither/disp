# Andromeda 2

**Repo:** https://github.com/Andromedans/andromeda (317★, created 2012-11-05, pushed
2026-06-13; commits per year 395 / 132 / 15 / 0 / 0 / 8 / 28 / 14 for 2019–2026; 1,555
all-time by Andrej Bauer)
**Written in:** OCaml. Dates and commit counts verified via the GitHub API on 2026-09-30.
**Clone inspected:** yes. `README.md`, `book/src/` (introduction, language, type-theory),
`lib/nucleus/`, `lib/equality/`, `stdlib/eq.m31`, `theories/`.
**Relevance:** the LCF design (a small core is the only thing that can make a theorem)
carried over to dependent types, plus an equality checker the user extends with rules.

## What it is

A proof checker where the user writes the type theory. The inference rules are
declarations, and the system computes judgements that follow from them.

- **The nucleus.** [Judgements are opaque except within a tiny, trusted
  nucleus](https://arxiv.org/pdf/1802.06217#page=1). The docs say [4,000
  lines](https://www.andromeda-prover.org/introduction.html#introduction); `lib/nucleus/`
  is 4,189 today. It guarantees one thing: a judgement it hands out is derivable from
  the user's rules. Whether those rules make sense is the user's problem.
- **The meta-language (AML).** An ML with algebraic effects. Programs build judgements by
  asking the nucleus, and take them apart by pattern matching. When a step needs an
  equality the nucleus does not have, the interpreter raises an operation and a handler
  supplies the evidence; [operations are like resumable
  exceptions](https://www.andromeda-prover.org/language.html#operations).
- **No contexts.** Free variables ("atoms") carry their own types, made by `fresh` and
  discharged by `abstract`. [The context-free type theory is implemented in the
  nucleus](https://arxiv.org/pdf/2112.00539#page=1).
- **Goals are values.** A [boundary is the shape of a judgement still to be
  built](https://www.andromeda-prover.org/type-theory.html#boundaries): a type, a term
  of a given type, or an equation with its evidence missing.
- **Equality reflection is one rule.** `theories/equality_reflection.m31` is six lines:
  from a proof of `Id A a b`, conclude `a ≡ b`. Extensional type theory is a user theory
  like any other.

Status: the design work ended around 2021. 2022 and 2023 had no commits; 2025 was a
build cleanup; June 2026 rewrote the documentation as a book and added a category-theory
example. Alive, not moving.

## Why disp should care

### 1. The same trust shape, from the other direction

disp's two-op core mints hypotheses that user code cannot forge; Andromeda's nucleus
mints judgements the meta-language cannot forge. `fresh`/`abstract` is the same
generative move as `bind_hyp`. Andromeda gets there with an abstract OCaml type and a
separate meta-language; disp has one language and needs the walker to police
reflection. Andromeda is the evidence that the LCF shape survives dependent types, at
about 4,000 lines.

### 2. The equality checker is a worked answer to a Q1-shaped problem

With user-defined theories there is no fixed notion of normal form, so the checker is
built from whatever equations the user registers (`eq.add_rule Π_β`). [The user need
only provide the equality rules, which the algorithm classifies as computation or
extensionality rules](https://arxiv.org/pdf/2103.07397#page=1). Two phases: a
type-directed pass applies extensionality rules, then normalization applies computation
rules, with "principal arguments" fixing which positions get normalized. It is proved
sound. It lives outside the nucleus, so a bug can only make it fail to find a proof.

disp's Q1 asks whether a decidable fragment can license enough rewrites. This is a
concrete procedure for that fragment: accept an equation only if it has one of two
recognizable shapes, reject the rest, and let the core re-check every step.

### 3. Handlers as the channel between core and helper

The core never searches. When it is stuck, it asks a question as an effect, and library
code answers with a judgement the core then checks. That is a cleaner interface than a
checker returning pass or fail, and it is what the survey's "structured checker output"
steal item wants.

## Scorecard

| Axis | Andromeda 2 | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ◐ 33% (meta-language) | AML programs match on and build object-theory judgements and call the nucleus; AML code itself is never data. | ½(object judgements) · 0 · ½(metaprograms) — AML programs match on and build object-theory judgements by calling the nucleus; AML code itself is never data |
| G2 Specification | ◐ 70% (user theories) | Any finitary dependent type theory, the rules written by the user; only example theories, no library. Equality: reflection as a one-rule theory, with a sound user-extensible checker. | 1 · ½(no library) · 0 · 1 — user-defined dependent theories with example theories only; equality reflection is a one-rule theory and a sound extensible checker handles user equality rules |
| G3 Trust | ◐ 70% (LCF nucleus) | A 4,189-line nucleus is the only maker of judgements; the equality checker and everything else is untrusted. No second checker. | ½(mid-size nucleus) · 1 · ½(one core) · — — a 4,189-line nucleus is the only maker of judgements; the equality checker and AML are untrusted; no second checker |
| G4 Execution | ✗ 0% (none) | An interpreter for a proof checker. | 0 · 0 · 0 — an interpreter for a proof checker |
| G5 Search | ✗ 0% (none) | Handlers direct hand-written proof procedures; no synthesis. | 0 · 0 · 0 — handlers direct hand-written proof procedures; no synthesis |

## What disp could steal

- **The two rule shapes.** A computation rule rewrites a recognizable head; an
  extensionality rule equates two things by comparing their parts. Classifying each
  candidate rewrite this way, and refusing what fits neither, is a ready-made admission
  test for licensed rewrites.
- **Boundaries.** A goal as a first-class value, the judgement minus its head. A failed
  check could return the boundary it could not fill, which gives a proposer something
  to aim at.
- **Ask-by-effect.** The core raising "I need `A ≡ B`" and a library handler answering
  is a small, explicit protocol for the untrusted layer.

## Where disp differs

Andromeda is two languages: AML programs and object-theory judgements. Its reflection
runs one way, meta over object. It promises derivability from the user's rules and
nothing about the rules themselves. It has no execution story and no search. disp wants
one language where the checker is an ordinary program, a cost account, and an optimizer.

## Verdict

**A finished research prototype whose equality-checking paper and nucleus design are
worth more to disp than the running system.** Read the papers; do not track the repo.

**Distance from disp's goals: level on the trust shape, ahead on having an equality
procedure, absent on execution, search and library.**
