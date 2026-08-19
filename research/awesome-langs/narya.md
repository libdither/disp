# Narya

**Repo:** https://github.com/gwaithimirdain/narya (292★, pushed 2026-08-19)
**Relevance:** the reference implementation of the *observational* pole of the
three equality answers FOUNDATIONS §7 cites (Cedille φ / observational / cubical).
The survey already covers the cubical pole (`agda-cubical.md`); this is the third
pole, and the one whose shape matches disp's per-type metadata architecture.

## What it is

Mike Shulman's experimental proof assistant for higher observational type theory
(HOTT, "third-generation" HoTT): identity types compute on each type former, with
no cubical interval, and univalence is a theorem rather than an axiom. Essentially
solo (2,031 of ~2,092 commits), OCaml, GPL-3, near-daily commits since August
2023. Real tooling: ProofGeneral holes and case-splitting, user-definable mixfix
notation, separate compilation, a static binary. No tactics. Self-described as
"very much a work in progress" with breaking changes expected.

The theory in five lines:

- `Id A x y` for every type, computing per former: Id of a pair type is
  componentwise; Id of a function type is "related inputs to related outputs", so
  funext is definitional and arrives in logical-relation form; Id of the universe
  is the type of correspondences/bisimulations between types.
- The universe is *coinductive*: its destructors are `El` and `Id`, there is no
  typecase, and that absence is what justifies univalence. A built-in `glue` turns
  any bisimulation into an `Id Type A B`; univalence follows by corecursion.
- A `-parametric` mode drops transport and turns Id into internal parametricity of
  arbitrary arity (unary = displayed type theory, nullary = nominal type theory),
  either internal (anti-classical) or external (classical-compatible), organized
  through a built-in multimodal (MTT) framework.
- Semantics: presheaves on the semicartesian (BCH) cube category, meaning faces
  and degeneracies but no diagonals.
- The honesty ledger, stated in its own docs: `Type : Type` as a placeholder
  (levels planned via mugen); no termination/positivity/productivity checking, so
  inconsistent as a logic today; transport computes only on function types, simple
  records, codata, and glue so far; the NbE algorithm has no correctness proof;
  one supporting publication (POPL 2024, self-described as "a baby version").

## Why disp should care: four structural echoes

### 1. Both define the universe by observation, not inspection

Narya's universe is a coinductive object with no eliminator, and univalence
depends on that absence. disp's types are recognizer plus observation rows, its
universes carry no eliminator either, and the recorded hierarchy door (the `Space`
work) is exactly the moment a universe gains one. Same design instinct, reached
from opposite directions; Shulman's newest talk is literally titled "Coinductive
Universes and Higher Observational Type Theory."

### 2. Both take presheaves over a deliberately thin site

disp's ledger-worlds admit weakenings only, which is why intensional neutrals can
exist at all. Narya's cube category is semicartesian: faces and degeneracies, no
diagonals. Each system refuses exactly the extra morphisms that would collapse its
good properties.

### 3. Narya's Id at functions is `~_T` made into the identity type

`Id (A → B) f g` says related inputs map to related outputs: the "different
program, same behavior" statement A4 needs, with transport as the "carry every
proof across the rewrite" mechanism the optimizer story wants. Narya chose this
parametric form over naive pointwise equality on principle, the same shape as
disp's walker-computed logical relation.

### 4. Per-former equality fits disp better than an interval does

`agda-cubical.md` asks whether path types can enter selectively as cells so only
transport-using code pays. Narya answers differently: skip the interval entirely
and let each type former carry its own Id computation. disp already stores
per-type cubical operations in the `functor` metadata field (TYPE_THEORY §13);
observational Id is per-type metadata *by construction*. If disp integrates a real
equality theory, this shape is arguably the better match than CCHM cubical.

Plus one instructive opposition: hash-consing makes branding impossible in disp
(two identical trees are one object; the `Space` work re-confirmed shape-audit is
the only membership pattern), while Narya buys abstraction with generativity
everywhere: nominal record types, and even two textually identical `match`
expressions are unequal until they reduce. Narya gets sealing for free and pays
normalization; disp gets O(1) conversion for free and had to invent the audit
discipline.

## Scorecard

| Axis | Narya | Note |
|---|---|---|
| A1 Reflection | ✗ | No programs-as-data at all; even Agda's quotation API has no analogue. |
| A2 Spec power | **✅** | Full HoTT + internal parametricity + modalities; beyond disp, and beyond Cubical Agda on the parametricity axis. |
| A3 Kernel | ✗ | The typechecker is the trust base: tens of thousands of OCaml lines, NbE unproven (mitigated by an intrinsically-scoped GADT core). |
| A4 Equality | **◐** | The interval-free observational design FOUNDATIONS cites; univalence-as-theorem landed, but transport computes on only some formers so far. |
| A5 Perf | ✗ | No cost model, no performance story; correctness-first by design. |
| A6 Search | ✗ | No synthesis; no tactics even. |

## What disp could steal

- **The definitional laws for `ap`** (preserves identity, composition, refl) as
  the spec sheet for what a licensing-grade equality must satisfy: the equations
  a certified rewrite pipeline needs to hold of `~_T`.
- **The coinductive bisimulation universe.** "Equality of types = a bisimulation
  coalgebra" transplants directly: disp's two-face structure (rows + recognizer)
  is already coalgebra-shaped, and `isBisim`'s destructor list (transport both
  ways, lifting both ways, a coherence field) is a concrete candidate for what a
  `Space`-level equality row would carry.
- **The internal/external/arity flag matrix** as a map of the sealing design
  space: disp's "sealing preserves parametricity" conjecture lives somewhere on
  Narya's internal-vs-external axis, and Narya documents which modal mode theories
  are consistent with which arities.
- **"Definitionally isomorphic, pretend they are the same."** Narya deliberately
  refuses on-the-nose computation of Id to a different type, settling for a
  definitional isomorphism with shared field names. That weaker contract may be
  exactly the right strength for rewrite licensing, and it is much cheaper.

## Where disp differs

Narya is a foundations vehicle for higher-dimensional mathematics: no reflection,
no kernel/library split, no cost model, no synthesis, and none are wanted. It is
"do the equality theory properly, ignore everything else", an even purer version
of the Agda pole. The goal overlap is a single axis; on that axis it is the most
disp-shaped living system.

## Verdict

**The missing third pole of FOUNDATIONS §7's equality answers, and the one whose
per-former, interval-free shape matches disp's per-type metadata architecture.
Watch it: partially computing today, moving daily.**

**Distance from disp's goals: ahead on A2 and on A4 design, absent everywhere
else; orthogonal project, load-bearing reconnaissance.**
