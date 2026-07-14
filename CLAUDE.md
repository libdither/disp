# Disp — Claude Code context

Dependently-typed language built on tree calculus. Types are predicates; the type checker is a tree program; the object language is meant to be fully self-hosting.

## Doc map

Read the relevant doc before design changes. File headers in `lib/` and `src/` are authoritative for per-file design; this map stays terse on purpose — the code is the status (see § Implementation status).

- [`GOALS.md`](GOALS.md) — the north star (neural-guided synthesis, self-improving optimizer) and the metacircular discipline used to get there.
- [`TYPE_THEORY.typ`](TYPE_THEORY.typ) — **authoritative** type-theory spec (target). Spec invariants worth internalizing before touching the kernel:
  - Two Σ-ops (`hyp_reduce`, `bind_hyp`) plus the dispatcher `param_apply` over a *fixed* Σ — a routing table, not just a trust set: pinned sigs run the *registered* handler, ignoring the value's embedded one.
  - `respond` is a constitutive (non-optional) meta field; `Action` is two-tag (`Extend type | Reduce value` — a neutral elimination stays stuck or computes; the verdict case is spelled `Reduce (Ok v)`, no alias).
  - Effects are entirely library (§15): a free monad `Eff R X` over an op signature, deep handlers, one impure driver at the program boundary. The substrate's purity forces effects to be values, never dispatch targets. `postulate` is removed.
  - Walker protection is one set: `seal(Σ) = forge(Σ)`; `bind_hyp` is in neither.
  - §2.7 polarity: the wait-form is the one negative former (records, types, cells, neutrals); positive data stays raw (inj-pairs / shape encodings + view isos); the shift into the negative fragment happens at abstraction, not construction; a value drives its own elimination iff it is a wait-form.
  - §2.6 collision doctrine: `true = △ = zero = nil = refl` — one tree for the smallest constants. In tests pick `false` / `succ zero` as cross-type wrong-values, never true-vs-Nat.
- [`SYNTAX.typ`](SYNTAX.typ) — surface grammar and AST. Authoritative for the parser, including declarations-as-requests and the named-argument calling convention.
- [`KERNEL_DESIGN.md`](KERNEL_DESIGN.md) — tree-calculus implementation idioms for the two-Σ-op kernel: wait/fix, signatures, neutrals, telescopes, the `tree_eq` native fast-path, bracket-abstraction caveats.
- [`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) — design rationale for the telescope (the one negative former). Read before reworking `tele_walk` or its cell ops.
- [`MODULES.md`](MODULES.md) — the module system: hermetic scoping, givens/fills, the functor face, guards and license replay. Authoritative for modules.
- [`EVALUATOR.md`](EVALUATOR.md) — reduction-backend map: the `Session` ABI, the five backends, the differential-oracle discipline. Detail in `EVALUATOR_PLAN.md`; net calculus in `research/interaction-combinator/tc-net.typ`. Read before touching `src/eval/` or a backend crate.
- [`OPTIMIZER.typ`](OPTIMIZER.typ) — unified design for the verified self-improving optimizer (the GOALS.md endgame). Not built; the substrate (`rust-ic-net`) mostly exists.
- [`FOUNDATIONS.md`](FOUNDATIONS.md) — lineage and precedent for every design piece, plus the falsifiable risk assessment. The general-reader companion.
- Design notes (each carries its own status banner): [`TOWER.md`](TOWER.md) (structural checking), [`REFLECT.md`](REFLECT.md) (reflection as an effect), [`LOCAL_SYNTH.md`](LOCAL_SYNTH.md) (synthesis), [`ELABORATOR_PLAN.md`](ELABORATOR_PLAN.md) (self-hosting elaborator — stage 0 `lib/elab/bracket.disp` LIVE as the cir.ts oracle + the playground's compiler-on-screen seed; stages 1–3 deleted 2026-07-01, recover via git).
- [`ACTIVE_BUGS.md`](ACTIVE_BUGS.md) — subject-reduction gap ledger; machine pins in `lib/tests/probe_*_sr.test.disp`.
- [`READING_QUESTIONS.md`](READING_QUESTIONS.md) — open-questions scratchpad.
- `archive/` — superseded docs (`STRICTTYPE.md`, `TYPE_NORMALIZATION.md`, older proposals). Removed docs live in git history (`git log --diff-filter=D -- <path>`): the legacy 7-primitive spec (`archive/TYPE_THEORY_LEGACY.typ`), `COMPILATION.typ` (pipeline spec; the pipeline's source of truth is `src/compile.ts` + `src/elab/`), `OPTIMIZER_DESIGN.md`.
- `research/` — `interaction-combinator/` (tc-net.typ, RUST_IC_NET_DESIGN.md, SPATIAL_IC.md, EMBEDDING_THEOREM.md), `effects-and-coeffects.typ`, `sequent.typ` (the sequent-calculus reading of the kernel); `one-offs/` holds unreferenced one-shot surveys.

## Core discipline

**The object language is the specification. Host implementations are optimizations.**

Every component participating in checking, elaboration, or conversion must have a declared tree-calculus encoding. The TypeScript runtime (`src/core/tree.ts`) is the only host code; everything else — type predicates, the parametric walker, the elaborator — are tree programs. A native fast-path in `src/core/tree.ts` must mirror in-language code semantically (bit-identical results) and be validated against the in-language reference. Currently the only live native fast-path is `tree_eq`; the dispatcher (`param_apply`) runs in-language, and re-introducing a native dispatcher requires restoring an equivalence test.

## Code layout

### Source
- `src/core/tree.ts` — tree calculus runtime: hash-consed trees, eager iterative `apply`, `tree_eq` native fast-path.
- `src/parse.ts` — tokenizer / parser. Implements the `SYNTAX.typ` grammar.
- `src/compile.ts` — the elaborator's public surface, a re-export barrel over `src/elab/`:
  - `state.ts` — session/budget/caches. `APPLY_BUDGET` (400M) is a divergence bound calibrated to the heaviest cold-memo elaborator call; the rust backends floor per-call at 4G.
  - `cir.ts` — CIR + bracket abstraction (η-reduction, K-composition) + `cirToAstTree` (a Cir as the coproduct value the in-language spec consumes). Part of definitional equality; the in-language oracle is `lib/elab/bracket.disp` — change only in lockstep with it (pins: `test/bracket.test.ts`, `lib/tests/bracket.test.disp`).
  - `sugar.ts` — surface rewrites: the S2 `select_lazy`→`if` auto-rewrite, named/default/reorderable args, `binderToPi`.
  - `literals.ts` — Nat/String/accessor encoding + record-header decoding.
  - `expr.ts` — `exprToCir` (the one fold over the surface AST) + `compileExpr`/`compileType`.
  - `driver.ts` — `parseProgram`: scope stack, hermetic module cache, givens/fills, guards/licenses, deferred auto-verification, sum-literal constructor auto-declaration (skip-if-bound; pinned in `sum_ctors.test.disp`).
- `src/run.ts` — file runner; a file exporting `main : Eff io_row X` runs under the driver.
- `src/driver.ts` — the impure effects driver: kernel-checks `main` against its Eff type (root-file annotations never self-verify), walks Pure/Op enforcing the row per op, performs via the host op map (`lib/std/io.disp`), resumes with `apply(k, r)`. Examples in `examples/`; host tests in `test/driver.test.ts`.

### Library (`lib/*.disp`) — file headers carry the design; one line each here
- `prelude.disp` — fragment -1, given-bearing: raw combinators, `true`/`false` as raw shapes `△`/`△ △`, triage/select/pair/wait/fix/tree_eq/nat basics. Files below the kernel open it `open use raw "../prelude.disp" {}`; never open it bare; files on the kernel barrel don't need it (the barrel re-exports).
- `kernel/` — the two-Σ-op kernel in seven hermetic fragments, assembled in value-dependency order by the barrel (annotations may forward-ref any fragment via givens; values may not):
  - `prelude.disp` — barrel + bootstrap: raw-loads all fragments, re-imports them checked with explicit fill records; the last open activates and verifies the raw prelude's annotations (the bootstrap fixpoint; open-dedupe raw ≡ checked per name).
  - `cut.disp` — the §2.6 value cut: `prod`/`cut`/`proj`/`inj`/`make_record`/`field`, the list toolkit (the one home for cons-list folds), `Action`/`CheckerResult`, `make_meta`, and the declaration protocol (`Bind`/`Install`/`Both`, the `base`/`let`/`sig`/`guard` decorators, `default_guard` — normative for the elaborator's declaration fast path).
  - `engine.disp` — Σ-ops `hyp_reduce` + `bind_hyp` (merged into the walker: mint, body-walk under `self`, occurs check), the dispatcher `param_walker`/`param_apply` (reader carve-outs + Σ routing + the §7A policed token), `typecheck`/`verify`, `elim`, the respond combinators. Every export is ANNOTATED or SEALED(<why>) — see the header ledger.
  - `cells.disp` — wait-form cells + the one mode-polymorphic walker `tele_walk` (recognize/respond) speaking the `Step` vocabulary; cell constructors incl. meet cells (`qid`/`refine`/`imeet`/`eqends`) and recursion cells; `Pi`/`Sigma`/`Record`/`⊤` are telescope instances (KERNEL_DESIGN § Telescopes).
  - `base.disp` — Unit, String, False/Not, Eq (+ J/subst/sym/cong/trans), Refinement, Intersection.
  - `positive.disp` — `Coproduct` (per-variant positional telescopes) with the η-readback coherence gate in the respond; recursion/corecursion are cells (`Rec`/`RecUnder`/`RecAt`; codata = `std/stream.disp`); Bool/Nat/Ord.
  - `generic.disp` — cell-derived generics: `fold_value`/`rec_value` (+ `nat_rec`/`bool_rec`/`ord_rec` as instances), `children`, `fmap`/`bimap` + record-side optics.
  - `universe.disp` — `Type = BehavioralType` (StrictType + GoodRespond merged; membership routes through `param_apply` so the §7A token flattens nested checks), the kernel's structural types (`Tree`/`Frame`/`MetaShape`/`Module`/`Action`/`Step`/`RespondShape`/…), Tree as floor AND inductive (`tree_rec`; frame-shape dispatch over-rejects but never unsounds), `Neutral`/`Spine` (an extended payload is never closed — membership goes through `param_apply`, not `typecheck`), `GuardAction`/`Request`, `meta_of : Type -> MetaShape`.
- `std/` — stdlib on the kernel barrel: `nat/`, `list`, `set`, `fin`, `option`, `result`, `either`, `pair`, `build`, `hbin`, `stream`; `effect.disp` (§15 effects: free monad + deep handlers + rows; an effect IS a record type — `op`/`op0`/`ops`, `requests`/`handler_sig`, `x <- e` block binds); `kernel_spec.disp` (spec twins of the sealed kernel algorithms; its header is the SEALED ledger); `demand.disp` (the demand face of telescopes: `field_deps`/`field_pi`/`sub_record` + the module face); `relation.disp` (explicit heterogeneous relations, pointwise and binary Pi lifts, PER/equivalence/preorder law bundles, morphisms, and the relation-generic `license_guard`/`freeze` policies).
- `tests/` — all `*.test.disp`; the runner globs recursively.
- `elab/` — `bracket.disp` only: the in-language bracket-abstraction spec (the `cir.ts` oracle; the playground's visualize-the-selection seeds `bracket_compile <encoded Cir>` from it to run the compiler on screen). The rest of the self-hosted elaborator (ast/compile, stages 1–3) was removed 2026-07-01; recover via `git log --diff-filter=D -- lib/elab`.

### Host tests
- `test/disp.test.ts` — vitest harness; recursively globs `lib/tests/**/*.test.disp`.
- `test/parser.test.ts`, `test/tree.test.ts` — parser and tree-runtime units; `test/driver.test.ts` — effects driver.

## Implementation status

The codebase implements `TYPE_THEORY.typ`'s two-Σ-op kernel; Pi/Bool/Nat/Eq/Type/Ord/Sigma/Refinement/Intersection/Coproduct/Record are ordinary library types across the kernel fragments. **The code is the status, not this file** — for what's landed read `git log`, `npm test`, and the kernel/std sources; for what's not, the tracker is `TYPE_THEORY.typ` (by § number) plus the characterization tests in `lib/tests/soundness.test.disp`. Known-deferred at a glance: full route-to-registered-handler (§5.4 — the §7A policed token is one registered route), the structural `respond : RespondShape` ascription in MetaShape (§11–12 — behavioral checking landed as `Type := BehavioralType` instead; pins in `metashape.test.disp`), cubical `Path`/`comp`/`transp`/`Glue` (§13), `strip`/erasure (§10), `wf_fix`/`Total`, richer `CheckerError` vocabulary + source-span diagnostics (currently one `Err`).

## Compiler workarounds

Issues affecting kernel-level code with recursion or multi-line conditional dispatch:

- **Match arm bodies are single-line.** A multi-line match arm body like `false => triage \n (arg1) \n (arg2)` parses only the bare `triage` because the arm body uses `lineExpr`. Wrap multi-line content in parens to make it one atom: `false => (triage \n (arg1) \n (arg2))`. Same workaround for multi-line `let` bodies.
- **`select_lazy` + self-recursion blows the compile-time budget.** A thunk `{_} -> self meta (pair_snd x)` compiles to `K body`. After outer bracket abstraction, `cirToTree`'s reduction of the closed combinator eagerly evaluates internal apps via `applyTree`. For self-referential expressions, this fires fix-unfolding at compile time even though runtime semantics would short-circuit via `select_lazy`'s lazy thunk dispatch. **Workaround**: use `if`/`match` for bodies containing recursive calls — both close their branches/arms over the free-var union, side-stepping the eager K-body evaluation. The elaborator auto-rewrites the standard shape `select_lazy ({_} -> A) ({_} -> B) cond` to `if` (S2, `sugar.ts`; pinned in `match.test.disp`), so only non-standard thunk shapes need the manual rewrite.
- **The η/saturation rule for match arms.** Every call in a match-arm body must carry at least one ARM-BOUND variable in its application spine. A call saturated by free-var-row variables alone (e.g. `self env e` where all three are bound outside the match) η-exposes under bracket abstraction (`[e](self env e) → self env`), and the cut's free-var distribution then EVALUATES it at runtime — for every arm, selected or not; if the call is recursive, that's an infinite regress. Calls ending in an arm-bound var stay partial during distribution (the standard `self name f` shape is safe), and `if`-guarded bodies are safe (cond selects before the branch row applies). When an arm needs the scrutinee itself, REBUILD it from the arm binders — hash-consing makes the rebuilt application identical to the scrutinee.
- **The `bind_hyp` continuation must be an INLINE lambda in the recursive walker.** A continuation `{h} -> self … h` that is passed THROUGH a function (as a `let go` reused at the `bind_hyp` site, OR as a parameter to a helper that calls `bind_hyp ty go`) miscompiles under *nested* binders: the fresh hyp leaks into the result and trips `bind_hyp`'s occurs-check, so a function whose body uses an OUTER argument (`{a}->{b}-> t a b`) wrongly rejects while `{a}->{b}->b` passes. Inline the lambda directly in the `bind_hyp` call; applied uses of the same continuation (`go x`) are fine — only the *unapplied pass to `bind_hyp`* breaks. This is why `tele_walk` handles `mint` inline and the per-cell ops return a *Step* (data) for `tele_walk` to interpret.

(Retired 2026-06-12: the closed-prefix-redex hazard — the match desugar now closes the whole cut over the arms' free vars, so the old `wait self name x` workaround is no longer needed in match arms; the η/saturation rule above is its surviving runtime cousin.)

## Key tree-calculus idioms

- **`wait` for deferred application.** `wait a b c = a(b)(c)` but `wait(a)(b)` doesn't evaluate `a(b)`. Essential for `fix` and partial application. The kernel's Σ-ops are plain `fix`-forms.
- **Wait-based types.** `wait(checker)(metadata)`. Signature = `pair_fst(T)` (constant per checker); metadata = `type_meta(T)`, a §2.6 record whose fields (`respond`, `recognizer_params`, …) are read by name via the cut. Type-former recognition is signature comparison.
- **H-rule via `make_recognizer`.** The universal recognizer-side H-rule reconstructs the type via `wait (wait wrap body) meta` and short-circuits `Ok true` when the candidate is a neutral of that stored type; `type_predicate_h_rule` is the predicate-side dual.
- **Raw recognizer application bypasses the walker.** Verification must go through `param_apply`/`verify`; a type whose recognizer mints a hyp (Pi/Intersection) applied raw fails fast. On neutrals, the sanctioned projection is `neutral_type` only — reading a neutral's payload is the extraction leak.
- **Hash-consing is load-bearing.** Conversion is `tree_eq`, an O(1) hash-cons identity check. Deterministic elaboration ensures same type → same tree.
- **`is_neutral`** is an O(1) signature check (`pair_fst v` against `hyp_sig`); `is_hyp_fork` adds the fork-shape guard where partial wait-forms could share the bare signature.

## Testing

`npm test` runs vitest. `lib/tests/**/*.test.disp` is the primary object-language suite. `test/parser.test.ts` and `test/tree.test.ts` cover the host infrastructure.

## Operating notes

- **Git workflow — work on `main` directly.** Do all work on `main` and push to `origin/main`; do NOT create feature/topic branches unless the user explicitly asks. This overrides the default "branch first" caution for this repo. (Isolated agent worktrees for parallel subagents remain fine.)
- Type checking is raw `apply(T, v) = true`; types are wait-based raw functions.
- Prefer editing existing files over creating new ones.
- Binder parameter names shadow scope variables during compilation; collisions are safe but avoid them for clarity.
- **Export model:** in files with any `name := expr` field, only `:=` fields export; a file with no own exports re-exports its opens (the barrel rule). Prefer `name := expr` for new code.
- **Declarations are guard-mediated** (SYNTAX.typ's declaration-request note + `cut.disp`): `guard <policy> NAME : T := v` owns a name; rebinding an owned name carries credentials (`{ new := …; proof := … }` for `license_guard`, with an explicit relation contract). Unguarded duplicates error in the driver; guard-free code takes a fast path byte-identical to pre-guard behavior. Opens replay licenses: colliding open fields consult the incumbent guard, and successful rebinds carry driver-stamped certs downstream (MODULES.md § slice 3; worked example `std/nat.opt.disp`).
- **Modules are hermetic with explicit dependencies** (MODULES.md). A used file sees only its own defs + opens + givens; `given X : T (:= default)?` declares a dependency (canonical spelling: the `open given { … }` header block); fills are explicit `use "f" { x := v }`. Bare `use` of a given-bearing module errors when raw; bare CHECKED `use` yields the functor tuple (readback lambda + Pi-into-Record typ; replay is tree-identical to direct instantiation). Root files cannot declare givens. Pins: `given.test.disp`, `test/modules.test.ts`.
- **`let` and `test` are not keywords.** `let x := e` is a decorated declaration whose head is the library value `let` (`cut.disp`); legacy `let x = e` is removed. Tests are the equation item `lhs = rhs` (compound lhs required); `test` is the prelude identity, peeled while pristine. Braced `let` remains the lexical binding form. Keywords: `use open match if then else`.
