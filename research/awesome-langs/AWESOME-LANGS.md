# AWESOME-LANGS

Languages and systems surveyed against **disp**'s goals, August 2026.
Scored on the six axes defined in [`_AXES.md`](_AXES.md); one file per project with
the full argument. All activity dates verified via the GitHub API on 2026-08-03
(Mojo: 2026-08-18, the day its compiler went open source; Narya: 2026-08-19; Telomare: 2026-08-26; Stellogen: 2026-09-04; Nock/Hoon: 2026-09-12).

**disp in one sentence:** write a spec as a dependent type, turn the checker into a
0/1 score, multiply by a hardware-faithful cost score, search a reflective low-level
calculus for a program that is both provably correct and fast — then turn that
search on itself.

| Axis | Short name |
|---|---|
| **G1** | Substrate: reflection, programs-as-data |
| **G2** | Specification: dependent types, proofs, resources, equality |
| **G3** | Trust: tiny kernel, everything clever untrusted |
| **G4** | Execution: native speed, path to the machine, cost accounting, licensed rewrites |
| **G5** | Search: spec → implementation, and self-application |

---

## The master table

Cells are `symbol percent`: the share of disp's own requirement met, per [`_AXES.md`](_AXES.md) clauses; ✅ from 80%, ◐ from 25%, ✗ below · **bold = ahead of disp on that axis** · the write-ups carry the how-tags and clause values

| Project | G1 | G2 | G3 | G4 | G5 | Closest to disp on |
|---|:--:|:--:|:--:|:--:|:--:|---|
| [**HVM4 / Bend2 / SupGen**](hvm4-bend-supgen.md) | ◐ 33% | ✗ 0% | ✗ 0% | **◐ 50%** | **◐ 50%** | the *search substrate* — superposition search in the runtime |
| [**Metamath Zero**](metamath-zero.md) | ◐ 33%? | ◐ 25% | **✅ 100%** | **◐ 50%** | ✗ 0% | the *kernel* + hardware model + self-verification |
| [**Verus**](verus.md) | ✗ 0% | ◐ 38% | ✗ 0% | ◐ 25% | **◐ 67%** | *delivered outcomes* — verified fast systems code from specs |
| [**F\* / Low\* / Pulse**](fstar-lowstar-pulse.md) | ◐ 50% | **◐ 62%** | ✗ 17% | ◐ 25% | ✗ 0% | the *shipping baseline*: dependent spec → verified fast C |
| [**Lean 4**](lean4.md) | ◐ 50% | **◐ 62%** | ✅ 83% | ✗ 12% | **◐ 33%** | *spec power* + where the AI provers live |
| [**Nova**](nova.md) | ✗ 0% | **◐ 62%** | ◐ 67%? | ✗ 0% | ✗ 0% | the *equality* answer + certificate/kernel split |
| [**Blight**](blight.md) | ✗ 0%? | **◐ 75%** | **✅ 100%** | ✗ 0% | ✗ 0% | the *kernel architecture*, independently reinvented |
| [**Soma**](soma.md) | ✗ 0% | ◐ 38% | ✗ 0% | ◐ 25% | ✗ 0% | same four ingredients, assembled for speed |
| [**fiat-crypto + CryptOpt + Jasmin**](fiat-crypto-cryptopt-jasmin.md) | ◐ 50% | **◐ 62%** | ✅ 83% | **◐ 75%** | **◐ 67%** | the *whole endgame*, at 1/1000th scope |
| [**Velvet / Loom / WybeCoder**](velvet-loom-wybecoder.md) | ◐ 50% | **◐ 62%** | ✅ 83% | ✗ 0% | **◐ 50%** | the *agentic proof loop*, with numbers |
| [**Agda / Cubical**](agda-cubical.md) | ◐ 50% | **◐ 75%** | ✗ 17% | ✗ 0% | ✗ 0% | cubical equality; reflection-by-quotation contrast |
| [**Narya**](narya.md) | ✗ 0% | ◐ 50% | ✗ 17% | ✗ 0% | ✗ 0% | the *interval-free* equality answer, shaped like disp's per-type metadata |
| [**Idris 2 / QTT**](idris2-qtt.md) | ◐ 50% | **◐ 75%** | ✗ 17% | ✗ 12% | ✗ 0% | the *grading* formalism disp's cost ledger uses |
| [**CakeML + Pancake**](cakeml-pancake.md) | ✗ 0% | ◐ 38% | **✅ 100%** | **◐ 62%** | ✗ 0% | verified *all the way to the binary* |
| [**ATS3 / Xanadu**](ats-xanadu.md) | ✗ 0% | ◐ 50% | ✗ 17% | ◐ 25% | ✗ 0% | the 25-year prior attempt at G2+G4 |
| [**Vow**](vow.md) | ✗ 0% | ◐ 25% | ✗ 0% | ◐ 25% | **◐ 33%** | the *agent-facing interface* |
| [**Dafny**](dafny.md) | ✗ 0% | ◐ 38% | ✗ 0% | ✗ 0% | **◐ 33%** | calibration: what AI writes best |
| [**NanoLang**](nanolang.md) | ✗ 0% | ✗ 0% | ✗ 17% | ◐ 38% | **✗ 17%** | mechanized metatheory, solo |
| [**Rust cluster**](rust-verification-cluster.md) | ✗ 0% | **◐ 62%** | ◐ 50% | ◐ 25% | **◐ 33%** | the competition, and the fallback |
| [**Mojo**](mojo.md) | ◐ 33% | ✗ 12% | ✗ 0% | ◐ 38% | **◐ 33%**ᶠ | one language at every stage, at industrial scale |
| [**LogosLang**](logoslang.md) | ◐ 50% | ✗ 0%ᵈ | ✗ 0% | ◐ 25% | ✗ 0% | nearest indie statement of disp's thesis |
| [**Acorn**](acorn.md) | ✗ 0% | ◐ 25% | ✗ 0%? | ✗ 0% | **◐ 50%** | *local* neural proposer in the loop |
| [**Salt**](salt.md) | ✗ 0% | ◐ 25% | ✗ 0% | ✗ 12%ᶜ | ✗ 0% | proof-coverage as a continuous signal |
| [**Telomare** (Stand-In Language)](telomare.md) | ✗ 17% | ◐ 25% | ✗ 0% | ◐ 25%ᵗ | ✗ 0% | the *static cost bound*: totality by inferred iteration counts, `--certificate`/`--meter` |
| [**Stellogen**](stellogen.md) | ✅ 83% | ✗ 12% | ◐ 50% | ✗ 0% | ✗ 0% | *types as user-space test suites*; the usine/usage split `CHECK.disp` re-derives, with the same function-type gap |
| [**Nock / Hoon**](nock-hoon.md) | ✅ 83% | ✗ 0% | ◐ 50% | **◐ 50%** | ✗ 0% | the *substrate*: a frozen reflective combinator VM whose jets are `.opt.disp` overlays without licenses |
| [**Indie AI-first cluster**](indie-ai-first-cluster.md) | ✗ 0% | ◐ 25% | ✗ 0% | ◐ 25% | **◐ 33%** | LSTS shares the library-not-kernel thesis |
| [**Adjacent substrates**](adjacent-substrates.md) | — | — | — | **✅ 100%**ᵉ | — | egg/e-graphs ≈ disp's `~_T` machinery |
| [**Graveyard**](graveyard.md) | — | — | — | — | — | what happens when this is attempted |

ᵈ designed, not built · ᶜ claimed, unverified provenance · ᵉ egg specifically · ᶠ cost-only parameter sweeps, not program search · ᵗ static iteration bounds, no native speed

---

## How close is anyone to disp's actual goal?

**Nobody occupies disp's square.** Every project here is missing the same pair: **G1 + G5
together**, a reflective substrate and a search. That
combination — a reflective substrate where the optimizer is a program in the
language it optimizes — is what makes self-application possible, and it is
structurally unavailable to the entire two-layer world (Rust+SMT, Lean+extraction,
C+separation logic), because there the optimizer is written in a *different*
language from the programs it transforms.

That is disp's real differentiator. Not dependent types (Lean, F*, Agda, Idris,
ATS have more), not performance (Verus, Rust cluster, CakeML have more), not the
kernel (MM0 has a better one). **It is that disp's optimizer can be aimed at
itself, and no one else's can.**

### Closest by axis

- **G1 (substrate):** disp and Nock are the only two with native intensionality and
  no quotation layer; everyone else quotes (Lean `Expr`, Agda `Term`, Meta-F*) or, like
  Mojo, stages the same language without ever making terms data. Nock has run an
  operating system on it for a decade and never built a checker on it. Stellogen
  inspects natively but cannot run what it inspects.
- **G2 (specification):** disp trails. Lean, Agda, F*, Rocq have more type theory and
  far more library; on the equality clause Nova (extensional), Agda and Blight
  (cubical) and Narya (observational) all have answers where disp has a slice, which
  is disp's own Q1 confirmed as the weakest point.
- **G3 (trust):** MM0 ahead; Blight and CakeML match it with an independent second
  checker; Nova, Lean and Rocq match the architecture.
- **G4 (execution):** Verus/Rust ≈ Mojo ≈ ATS ≈ Soma on raw speed; CakeML, MM0 and the
  Jasmin/bedrock2 line have the verified path to the machine disp lacks; disp,
  HVM4 and Telomare are the ones that account for cost deterministically; only
  CryptOpt installs a rewrite on a checked license, Nock and Mojo assert theirs.
- **G5 (search):** HVM4 (enumerative), Verus+IDS (neural, with cost in the loop),
  and CryptOpt (randomized, certified) are all ahead of disp, which has this
  designed but unbuilt. Nobody has aimed a search at itself.

### The three results that most change the picture

1. **Inductive Deductive Synthesis** (Berkeley+Google, May 2026): autonomously
   produced verified distributed KV stores for **7/7 consistency specs**, some
   **3× faster than published human-verified systems**, with performance
   benchmarks *inside* the verification loop. disp's stated endgame, achieved for
   a real workload — using Verus + an LLM, not a reflective calculus. → `verus.md`
2. **WybeCoder** (Meta, 2026): Lean/Velvet proof success went from **4.9%
   single-shot to 74.1% agentic** on the same benchmark. The gain came from search
   structure (subgoal decomposition, conflict-driven revision), not model scale.
   → `velvet-loom-wybecoder.md`
3. **CryptOpt is unmaintained since June 2024.** The one project whose architecture
   matched disp's endgame exactly — untrusted randomized search + verified
   equivalence checker + measured on-CPU cost — stopped. → `fiat-crypto-cryptopt-jasmin.md`

---

## What to steal, ranked by value per unit of effort

1. **Evaluate egg/e-graphs before building more bespoke rewrite machinery** for Q1.
   ROVER-style certificate emission (replayed by a kernel) is disp's §12
   architecture, already working. → `adjacent-substrates.md`
2. **Soma's flat/heap tiering.** Duplication of flat values is a free register copy;
   only heap types pay for SUP/ERA. Directly attacks disp's measured no-memo tax.
   → `soma.md`
3. **HVM4's label algebra, as shipped code.** disp's open Q2 is HVM4's feature set;
   `src/hvm.c`'s DUP/SUP rules and the collapser's priority queue are readable
   today. → `hvm4-bend-supgen.md`
4. **A benchmark.** DafnyBench took Dafny from 68%→96% in a year by making progress
   measurable. disp's optimizer claim is currently unfalsifiable. → `dafny.md`,
   `velvet-loom-wybecoder.md`
5. **Structured, machine-readable checker output** (Vow's JSON-for-agents) plus a
   **fallback ladder** (Velvet: SMT → tactics → property testing). `Ok false` gives
   a proposer nothing. → `vow.md`, `velvet-loom-wybecoder.md`
6. **Proof-coverage as a continuous build metric** with graceful degradation to
   runtime checks. A second smooth signal beside cost. → `salt.md`
7. **A CI-enforced `TRUST_ASSUMPTIONS.md` / axiom ledger.** → `indie-ai-first-cluster.md`
   (Verity), `metamath-zero.md` (thinking-sand)
8. **Source-directed `.compile()`** as the reflection escape hatch — full
   reflectivity by default, native speed on demand. → `logoslang.md`
9. **Scope the first real win like CryptOpt or Pancake did:** one narrow domain
   where search terminates and the equivalence checker can be small and verified.
   → `fiat-crypto-cryptopt-jasmin.md`, `cakeml-pancake.md`
10. **Mechanized metatheory for the two-op kernel** as the external anchor Q5 needs.
    NanoLang shows a solo dev can produce an `Admitted`-free development.
    → `nanolang.md`
11. **Inferred iteration bounds as a static cost certificate**, with the two failure kinds
    named (budget exhausted vs. input nothing bounds) and the analysis cached in a
    compile-once artifact. → `telomare.md`
12. **A spec-level conformance vector file for the substrate** (Nock's `norm/tests.json`: 63
    vectors including crash cases). disp's five evaluators agree with each other, but no
    third party can run a vector file against a new backend. → `nock-hoon.md`
13. **The jet failure catalogue as a checklist for `.opt.disp` overlays:** registration by
    side effect, matching on intensional identity, silent loss under hint drift, mismatches
    visible only in traces, no differential check in production. → `nock-hoon.md`

---

## The uncomfortable finding

Two of them, stated plainly:

**disp's weakest clauses are the ones it identified as its spine.** FOUNDATIONS Part V
says equality (the intensional/extensional gap) is make-or-break Q1. This survey confirms
disp holds half an equality theory and half a rewrite license, while two small projects
(Nova, Blight) already have working equality theories by choosing different foundations,
and egg has a tooled, certificate-emitting rewrite engine for the first-order fragment.

**The thing disp wants to automate is the thing AI provers are worst at.** The
production verdict from the Rust→Lean pipeline (Runtime Verification + Ethereum
Foundation, May 2026): AI provers are a productivity multiplier for structural
lemmas and linear arithmetic, but **weak at domain-specific algebra and
loop-invariant discovery, and spec design stays human**. Proof-*closing* is getting
solved; invariant *discovery* and implementation *synthesis* are not. disp's G5
needs the second category.

Both point the same way: disp's differentiator (G1+G5 self-application) is real and
unoccupied, but it is gated behind equality, where disp is behind everyone and where the
available answers come from theories disp deliberately did not choose.
