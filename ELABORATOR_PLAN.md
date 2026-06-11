# Elaborator-in-disp: the self-hosting plan

**Status: ACTIVE — Stage 0 landed (2026-06-11, commit 4ae9e9f5); Stages 1–5
open.** Companion: `EVALUATOR_PLAN.md` (evaluator backends; touches the same
`src/` layering — see §7). Read `CLAUDE.md` § Compiler workarounds before
writing any multi-param fix in elab code (the closed-prefix-redex hazard was
discovered doing Stage 0).

## 1. Principle

The core discipline (CLAUDE.md): *the object language is the specification;
host implementations are optimizations.* The elaborator makes **zero typing
judgments** (check/infer was deleted 2026-06-02; `param_apply` is the sole
checker, module auto-verify is a hard error). What remains in `src/` divides
into three categories:

1. **Semantic weight** — code that *defines* meaning, above all definitional
   equality. Must be re-specified in-language, host demoted to a validated
   fast path (the `tree_eq` discipline).
2. **Spec'd syntax maps** — deterministic desugars. Wrong = wrong program,
   never wrong judgment. In-language eventually, lower risk.
3. **The impure driver** — file IO, module cache, test runner. Stays host
   forever (TYPE_THEORY §15 sanctions exactly one impure boundary).

Per-stage validation pattern (established in Stage 0): an in-language spec in
`lib/elab/`, a `.disp` golden suite where each test is `in-language(encoded
term) = host(same surface term)` (the test harness's hash-cons comparison IS
the bit-identity check), plus a randomized host-vs-spec vitest.

## 2. Current state

| Component | Lines | Category | State |
|---|---|---|---|
| `src/tree.ts` substrate + evaluator | 582 | sanctioned host (see EVALUATOR_PLAN) | — |
| driver (`run.ts` + use-resolution/cache/IO in `compile.ts`) | ~300 | impure driver | stays host |
| **bracket abstraction** (`abstractName`/`eliminateLams`/`cirToTree`) | ~90 | semantic weight #1 | **DONE: `lib/elab/bracket.disp`**, validated by `lib/tests/bracket.test.disp` (16 golden) + `test/bracket.test.ts` (300 randomized) |
| type-position desugars (`binderToPi` ~`compile.ts:664`; recType→Telescope fold ~`compile.ts:317`) | ~50 | semantic weight #2 | Stage 1 |
| value desugars (match→cut, if→cond, literals, recValue sequential scope, puns) | ~250 | syntax maps | Stage 2 |
| scope resolution / Expr→Cir (`exprToCir`) | ~150 | syntax maps | Stage 3 |
| parser (`parse.ts`) | 942 | syntax maps (pure) | Stage 4 |
| `use`/verify orchestration (`resolveUse`) | ~80 | the ONE host-trusted soundness decision | Stage 5 |

Notes that constrain everything:
- **Determinism is load-bearing**: conversion is `tree_eq` on elaborated trees,
  so same spelling → same tree is part of the type theory. Any in-language
  stage must be deterministic and bit-match the host.
- The host evaluates closed redexes at compile time (`cirToTree` →
  `applyTree`); the in-language `cir_to_tree` gets this *for free* — its
  general-application arm is juxtaposition (the substrate IS the evaluator).
  Expect the same effect at every later stage: host `applyTree` call-sites
  cost nothing to re-specify.

## 3. Stage 1 — type-position desugars (next)

These produce TYPE trees, so they feed conversion identity directly.

1. **AST encoding** (`lib/elab/ast.disp`): the Expr subset needed here — a
   §2.6 coproduct mirroring `parse.ts` Expr:
   `EVar name | ELeaf | EApp (f,x) | EBinder (params, body) | ERecType fields
   | EAnn (e,ty) | …` (add nodes per stage; keep tags = host AST tag names).
   Param = `{ name; ty }` records with stem-option for null; recType field =
   `{ name; ty; value }` with stem-options. Strings are already trees.
2. **`binder_to_pi`** mirroring `compile.ts binderToPi`: multi-param
   right-association, anonymous params, recursive domain/codomain desugar,
   `Pi` lookup by name → take the Pi TREE as a parameter (in-language code
   has no scope lookup yet; thread an env record or pass formers explicitly —
   decide at implementation; passing `{ Pi := Pi; Telescope := Telescope; … }`
   keeps Stage 1 scope-free).
3. **`rectype_to_telescope`** mirroring the fold at `compile.ts:317-349`:
   entry records `{name; ty; def}`, prior-name lams, `Tree` default for
   untyped fields, `t recipe` def for derived members.
4. Validation: golden tests `tele_desugar (encoded {a : Nat, b := double a})
   = { a : Nat, b := double a }` (host compiles RHS); randomized vitest
   generating recType/binder ASTs.

Watch for: every fold here is a multi-param fix over an AST — apply the
`wait self name x` rule from CLAUDE.md § Compiler workarounds, or put the
arm-derived argument first.

## 4. Stage 2 — value desugars

In-language versions of: `match` → §2.6 cut (`prod (pair tags handlers)`,
wildcard appended, multi-binder pair destructuring), `if` → `cond` with
branch closures over free vars (needs in-language free-var collection — reuse
`contains_free` generalized to Expr), numeric literals → zero/succ, string
literals → codepoint lists, array literals → cons chains, recValue →
`mk_record` with sequential field scope + puns (mirror `compile.ts` recValue
case: core-with-vars, `(λname. rest) value` wrapping, prior-name shadowing).

**Recommended first move (host-side, small):** make the host match-desugar
close arms over their free vars the way `if` already does, THEN mirror it.
This retires the closed-prefix-redex workaround class entirely (arm bodies
would no longer embed `self name` as a closed subterm) and simplifies both
sides. It changes compiled trees → conversion-identity migration: do it as
its own commit with the full suite green.

## 5. Stage 3 — scope resolution (Expr → Cir)

`exprToCir`'s remaining job once desugars are in-language: name → lit/var
resolution against an environment, binder shadowing, lam introduction. The
environment is a §2.6 record (name → tree). The irreducibly POSITIONAL
type/value mode decision (annotation = universe `Type` ⇒ body compiles in
type mode; see memory `project_elaborator_simplification`) lives here — it is
a *mode bit threaded by the caller*, not inference.

After Stage 3, `compile_expr := cir_to_tree ∘ eliminate_lams ∘ expr_to_cir`
is the full in-language pipeline from AST to tree; the host `compileExpr`
becomes a pure fast path, validated end-to-end (random ASTs, both pipelines).

## 6. Stage 4 — parser

`parse.ts` (942 lines) re-specified as an in-language tokenizer + recursive-
descent/combinator parser over codepoint lists (strings are already trees).
Pure syntax, zero typing content — biggest chunk, least semantic risk, do
last. Validation: host parses source S → AST; in-language parses the SAME
source (as a string tree) → encoded AST; compare encodings. Perf will be bad;
irrelevant (it's the spec, not the path).

## 7. Stage 5 — the driver boundary (+ EVALUATOR_PLAN interplay)

What must remain host: file IO, module cache, CLI. What should become
in-language *semantics* with a host oracle: `use` — module = `{record, typ}`
+ the **verification obligation** (`verify mod = param_apply mod.typ
mod.record`). Today the *decision* to verify is trusted host code
(`resolveUse`, ~`compile.ts:746`); the end state makes `use`'s meaning (load →
assemble → verify-or-fail) an in-language function over a host-provided
`path → file-content` oracle, so skipping verification is impossible without
visibly forking the spec. Sequence this AFTER EVALUATOR_PLAN's substrate/
evaluator split if that lands first — both refactor the same `src` seams, and
the evaluator abstraction determines what "the host runs a tree program"
means for out-of-process backends.

## 8. Hazards & conventions (learned in Stage 0)

- **Closed-prefix redexes** in multi-param fixes (CLAUDE.md § Compiler
  workarounds): `wait self name x` or arm-derived-arg-first. Bites EVERY
  AST fold in this plan until the §4 match-desugar fix lands.
- **Don't annotate inspecting functions** with `Tree -> …`: verification
  mints hyps the folds triage (`mk`, `bracket_compile` precedent) — leave
  unannotated until StrictType-era classifier types exist.
- Keep host AST tag spellings as coproduct tags ("App", "Lam", …) so match
  arms read like the host switch and diffs stay reviewable.
- Equivalence harness shape: ONE `parseProgram` for all host-side terms
  (loading the kernel per term is the slow part); seeded PRNG for
  reproducibility (`test/bracket.test.ts` is the template).

## 9. What stays OUT of the elaborator (the trust gradient)

Anything creating equations or accepting/rejecting values is kernel;
anything saving keystrokes is elaborator. Planned conveniences that are
deliberately untrusted because the kernel re-checks: literal-site auto-fill
of derived fields (insert `mk T {…}` when the expected type is known —
needs bidirectional expected-type threading, deferred), and the eventual
`Imp` metavariable/unification solver (solutions are values the recognizers
re-pin). Neither may ever carry semantic weight.
