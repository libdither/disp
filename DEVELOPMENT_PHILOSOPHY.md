# Development Philosophy

How we're allowed to build this. Companion to [`GOALS.md`](GOALS.md) (why) and [`TYPE_THEORY.typ`](TYPE_THEORY.typ) (what). When a decision conflicts with this doc, the doc is the harder thing to change.

## The commitment

**The object language is the specification. Host implementations are optimizations.**

Every component that decides well-typedness — the checker, the elaborator's semantics, conversion, the data those components read — must have a declared tree-calculus encoding. The TypeScript implementation makes iteration fast; it is never the ground truth. A checker that exists only as host code is a different project.

This is load-bearing. Lean, Coq, and Agda never bootstrapped their checkers into themselves; their checkers accumulated host dependencies (mutable maps, exceptions, GADTs, tactic DSLs) with no object-language counterparts, and by the time self-hosting was attempted, porting required rewriting. MetaCoq has been formalizing Coq inside Coq for a decade; Idris 2 was rebuilt from scratch. Metacircular discipline is nearly impossible to apply in retrospect. Without it here, the end state is a working checker that cannot exist as a tree program — defeating the premise of neural-guided synthesis over a self-describing checker.

## Rules

 1. **Kernel lives as a tree program from day one.** The seven primitives in `TYPE_THEORY.typ` are the spec of well-typedness. A host fast-path may run alongside; it must produce identical decisions on every input. When they disagree, the host is wrong.
 2. **Host code mirrors object-language semantics.** Write the TS elaborator as if it were pseudocode for a tree program. Checker-relevant data (environments, metas, spines, neutrals, closures) must have declared tree encodings even before those encodings are implemented. A `Map` may stand in for an association list because it's faster — not because association lists don't exist.
 3. **No host features without declared tree analogs.** Before using a host capability in checking, elaboration, or conversion: work out the encoding, or change the design. "Figure it out later" is the sound of metatheory creep starting.
 4. **Cross-validate continuously.** Keep the ability to run the tree-program checker on the host checker's inputs. Disagreement is a bug *in the discipline*; fix it before shipping further work.

## Object-level vs meta-level

The boundary is well-typedness. If a component contributes to deciding "is this well-typed?", it's object-level.

**Object-level — must have tree encodings:** type predicates and the seven kernel primitives; conversion and definitional equality (NbE if present); elaborator semantics (even when implemented in TS); annotated trees, environments, metas, and the elaborated→runtime erasure.

**Meta-level — host-only is fine:** surface parsing, error messages, REPL, IDE integration; caching, instrumentation, test harness, build system.

When in doubt: would a synthesizer operating inside the object language need to reason about this component? If yes, object-level.

## Red flags

Any of these warrants stopping and reassessing:

 - **"Figure out the encoding later."** Now, or redesign.
 - **Host data structures in checker code** (`Map`, `Set`, `WeakMap`) without a declared tree analog.
 - **Host exceptions for control flow** in elaboration. Tree programs have no `try`/`catch`.
 - **Tree-shape disambiguation** in place of explicit tags. (Cautionary: the historical `sOrAsc` clause triaged sub-tree shapes to distinguish S-node / ascription / AppAnn.)
 - **Kernel growing as fast as the elaborator.** Complexity belongs above the kernel.
 - **Host-only optimizations with no planned tree counterpart** — creep wearing a performance disguise.
 - **Surface features no one can sketch elaborating** into the current tree encoding.
 - **Two forms of the same concept** with no declared canonical one.

## New features

One question: does this depend on a host capability with no declared tree counterpart?

 - **No** → proceed.
 - **Yes** → encode it, redesign without it, or reject. "Implement now, port later" is not an option.

## Prior art and amendment

This is **metacircular** / **reflective** implementation in the LCF tradition: HOL-Light's ~400-line OCaml kernel, Twelf, MetaCoq, Idris 2, Nuprl. What none of them had, and we do, is **tree calculus as the object language** — three constructors, five rules, Turing-complete. The best possible starting position. Don't squander it.

This doc is a floor, not a ceiling. Tightening needs no discussion. Loosening is a design event: require a concrete argument for why the specific relaxation is necessary, what it costs, and why no tighter alternative works.
