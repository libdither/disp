# Development Philosophy

How we're allowed to build this system.

Companion to `GOALS.md` (why) and `TREE_NATIVE_TYPE_THEORY.md` (what). Every design decision should be compatible with this document; if it isn't, one of them changes, and changing this one is the harder path.

## The core commitment

**The object language is the specification. Host implementations are optimizations.**

Every component that participates in deciding well-typedness — the type checker, the elaborator's semantics, conversion, the compile-to-runtime pass, the data structures those components read — must have a declared encoding as a tree-calculus program. The TypeScript implementation exists to make iteration fast. It is never the ground truth.

This is load-bearing. If we drift from it, the downstream goal — neural-guided synthesis producing tree programs in a system whose checker is itself a tree program — becomes unachievable. The checker's status as a first-class inhabitant of the object language is the whole point; a checker that only exists as a host-language function is a different project.

## Why this matters: the creep trap

Lean, Coq, and Agda have never bootstrapped their checkers into themselves. Not because they couldn't in principle, but because their checkers accumulated dependencies on host-language features — mutable hash tables, structured exceptions, lazy evaluation, tactic DSLs, pattern-matching compilation, GADTs — with no object-language counterparts. By the time self-hosting was attempted seriously, the checker was too entangled with the host to port without rewriting.

MetaCoq (Sozeau et al., 2015–ongoing) has been retroactively formalizing Coq's type theory in Coq for a decade. Idris 2 was rebuilt from scratch specifically to achieve self-hosting. These are the exceptions that prove the rule: metacircular discipline is nearly impossible to apply in retrospect.

The pattern is: choose velocity (host conveniences) over discipline; accumulate dependencies invisibly; discover years later that self-hosting would require rewriting everything.

Our vulnerability is the TypeScript elaborator. Without discipline, the end state is a fast, working checker that cannot exist as a tree program — defeating the premise.

## The four rules

### 1. The kernel lives as a tree program from day one

`piCheck` and its supporting predicates are tree programs in `types.disp`. They are the specification of well-typedness. A host-language "faster checker" may exist alongside, but it must produce identical accept/reject decisions on every input. The tree-program version is canonical; when they disagree, the host is wrong.

### 2. Host implementations mirror object-language semantics

The TypeScript elaborator is written as if it were pseudocode for a tree program. Data structures used in checker-relevant logic (environments, meta tables, spine representations, neutral terms, closures) must have declared tree encodings, even before those encodings are implemented as actual tree programs. The host version may use `Map` because `Map` is faster than a tree-encoded association list — not because association lists don't exist. If the tree encoding doesn't exist, neither should the host version.

### 3. No host features without declared tree analogs

Before using any host-language capability in code that participates in checking, elaboration, or conversion, declare how it would be expressed in the tree calculus. If no such expression is known:
- work out the encoding first, or
- change the design to avoid needing the feature.

"We'll figure that out later" is the sound of metatheory creep starting.

### 4. Continuously cross-validate

Maintain the ability to run the tree-program checker on the examples the host checker handles. When they disagree, that disagreement is a bug *in the discipline* — something in the host version has diverged from the object-language spec. Fix it before shipping further work.

This cross-validation is cheap while the system is small and becomes prohibitively expensive if deferred.

## What's object-level, what's meta-level

Not everything needs to live in the tree calculus. The boundary is well-typedness: everything that contributes to deciding "is this program well-typed?" is object-level; everything that supports the development experience is meta-level.

**Object-level — must have tree encodings:**
- Type predicates (Tree, Bool, Nat, Pi, Sigma, Eq, ...)
- The checker (`piCheck` and successors)
- Conversion / definitional equality (including NbE if present)
- The elaborator's *semantics* (even when implemented in TS)
- The data the checker reads (annotated trees, environments, metas)
- The compile/erase pass from elaborated form to runtime tree

**Meta-level — host-only is acceptable:**
- Surface syntax parsing (human-facing, not part of the trusted base)
- Error messages and diagnostics
- REPL interaction, tooling, IDE integration
- Performance instrumentation and caching
- Test harness and CI
- Build system

When in doubt, ask: would a neural synthesizer operating inside the object language need to understand this component? If yes, it's object-level.

## Red flags

Signals that discipline is slipping. Any one of these warrants stopping and reassessing:

- **"We'll figure out the tree encoding later."** Figure it out now, or redesign.
- **Host data structures in checker code** (`Map`, `Set`, `WeakMap`) without a declared tree-analog primitive.
- **Host exceptions for control flow** in elaboration or checking paths. Object-language code has no `try`/`catch`; if it can't be expressed without exceptions, it can't be a tree program.
- **Ad-hoc wire-format disambiguation.** If the checker has to case-analyze on tree *structure* to figure out "what kind of thing is this," tags are missing. Cautionary tale: the `sOrAsc` clause in the current `types.disp` disambiguates S-node vs. ascription vs. AppAnn by triaging on sub-tree shapes — exactly what explicit tags would avoid.
- **Kernel grows as fast as the elaborator.** The kernel should stabilize and stay small; complexity belongs in the elaborator, which produces inputs the kernel can already check. If `piCheck` is growing continuously, the elaborator is pushing work onto it.
- **Host-only optimizations with no tree-level counterpart.** Memoization, hash-consing, sharing are fine if tree-program versions exist or are planned. Otherwise they are creep wearing the disguise of performance.
- **Surface features without object-language semantics.** If someone proposes a new surface construct and no one can sketch how it elaborates into the current tree encoding, don't add it.
- **Two forms of the same concept with no declared canonical one.** When elaborated and runtime representations both carry the same information in different shapes, one must be the source of truth and the other a derived form — not peers.

## Evaluating a new feature

One question: does this feature's behavior depend on any host-language capability that has no declared tree-calculus counterpart?

- **No** → proceed.
- **Yes** → choose one of:
  1. Work out the tree encoding, add the primitive (or derivable construction) to the object language, then implement.
  2. Redesign the feature so it doesn't require the host capability.
  3. Reject the feature.

"Implement it in the host now, port later" is not an option. That's how the big systems got stuck.

## Prior art

This discipline has a name and a history: **metacircular** or **reflective** implementation, in the LCF (Logic of Computable Functions) tradition. HOL-Light's kernel is ~400 lines of OCaml, deliberately tiny to remain auditable and portable. Twelf and the logical-framework tradition take reflection as primary. MetaCoq formalized Coq's type theory inside Coq. Idris 2 achieves self-hosting through deliberate rebuild. Nuprl has had reflective encodings since the 1980s.

What none of these systems had, and what we do, is **tree calculus as the object language**. Tree calculus is smaller than any lambda-calculus-based system: three constructors, five reduction rules, Turing-complete. This is the best possible starting position for metacircular discipline. We should not squander it by recreating the problems the LCF tradition solved.

## Amending this document

This philosophy is a floor, not a ceiling. Tightening it — stricter discipline, smaller kernel, more things object-level — is fine and doesn't require discussion. Loosening it — adding to the meta-level list, relaxing a rule, permitting a host feature without a tree counterpart — is a significant design event. Any such change should be accompanied by a concrete argument for why the specific relaxation is necessary, what it costs, and why no tighter alternative works.

Changes to this doc should be rare. Changes that expand what counts as meta-level should be rarest of all.
