# Elaborator-in-disp: the self-hosting plan

**Status: Stage 0 LIVE, Stages 1–3 SHELVED. `lib/elab/bracket.disp` (Stage 0,
revived 2026-07-13) is the in-language spec of `src/elab/cir.ts`, validated by
`lib/tests/bracket.test.disp` (16 golden bit-identities) + `test/bracket.test.ts`
(300 randomized), and has a live consumer: the playground's
visualize-the-selection seeds `bracket_compile <encoded Cir>` (encoder
`cirToAstTree` in cir.ts, worker method `bracketSeed`) so that REDUCING the
seed performs bracket abstraction on screen. Stages 1–3 (`ast.disp`,
`compile.disp` + their tests) remain deleted as unused scaffolding
(2026-07-01), to be redone once the language is more mature; recover via
`git log --diff-filter=D -- lib/elab`. Stages 4 (parser) and 5 (driver
boundary) never started. This document remains the roadmap. Historical: Stages
0–3 first landed 2026-06-11/12 (Stage 0: commit 4ae9e9f5) — the full AST→tree
pipeline was in-language with host `compileExpr` the validated fast path.** Companion: `EVALUATOR_PLAN.md` (evaluator backends;
touches the same `src/` layering — see §7). Related: Appendix A (sharing-
aware bracket abstraction, formerly `ABSTRACTION_SHARING.md`) — the
S-duplication blow-up is the remaining member of the compile-time blow-up
family; its option (b) is a conversion-identity migration whose cost grows
with every stage landed here (hand-factoring is the accepted idiom
meanwhile).

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
| **bracket abstraction** (`abstractName`/`eliminateLams`/`cirToTree`) | ~90 | semantic weight #1 | **LIVE: `lib/elab/bracket.disp`**, validated by `lib/tests/bracket.test.disp` (16 golden) + `test/bracket.test.ts` (300 randomized); consumed by the playground's compiler-on-screen seed |
| type-position desugars (`binderToPi`; recType→Telescope fold — both `src/elab/sugar.ts`/`expr.ts`) | ~50 | semantic weight #2 | Stage 1 landed then DELETED 2026-07-01 (`lib/elab/ast.disp`, 12 golden + 130 randomized; recover via git) |
| value desugars + scope resolution (match→cut, if→cond, literals, recValue sequential scope; `exprToCir`) | ~400 | syntax maps | Stages 2–3 landed then DELETED 2026-07-01 (`lib/elab/compile.disp`, 27 golden + 180 randomized; recover via git) |
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

## 3. Stage 1 — type-position desugars (DONE 2026-06-12)

These produce TYPE trees, so they feed conversion identity directly.
Landed as `lib/elab/ast.disp`, validated by `lib/tests/ast.test.disp`
(12 golden ASTs) + `test/desugar.test.ts` (130 randomized terms, host
type-mode pipeline vs spec-desugar-then-value-compile, bit-identical).

Implementation decisions (where the plan said "decide at implementation"):
- The AST stays SYMBOLIC — names are strings, `Pi`/`Telescope`/`mk_record`/
  `list_const`/`Tree` appear as `var "Pi"` etc. in the desugared OUTPUT, so
  Stage 1 needs no former-threading at all; resolution is Stage 3's job
  (mirroring the host, where binderToPi emits `{tag:"var",name:"Pi"}`).
- Param = `pair name_opt ty_opt`, recType field = `pair name (pair ty_opt
  val_opt)` (plain pairs + stem-options, not §2.6 records — trivially
  encodable from the host side, matching bracket.disp's Cir payloads).
- Both desugars are per-NODE (non-binder/non-recType nodes pass through;
  nested recTypes stay for the recursive elaboration), exactly like the host.
- Host throws (missing domain annotation) fall to the leaf, like bracket.disp.
- Surface caveat the randomizer honours: a recType's FIRST field must be
  `name : T` — a leading `:=` classifies the braces as a record VALUE.

## 4. Stages 2+3 — value desugars + scope resolution (DONE 2026-06-12)

Landed TOGETHER as `lib/elab/compile.disp`, because they don't factor: the
match/if/recValue closures depend on what resolves (free vars are collected
on the COMPILED Cir — a name that resolved to a lit isn't free), so the
Stage-2 desugars are arms of the Stage-3 scope fold, exactly like the host's
single `exprToCir`. Validated by `lib/tests/compile.test.disp` (27 goldens)
+ `test/compile.test.ts` (180 randomized value/type ASTs, end-to-end tree
equivalence — no decode step; the spec emits final trees).

What's in it: `env_bind`/`env_shadow`/`env_lookup` (assoc list, `t v` bound /
`t` shadowed-or-unbound = the host's undefined), `cir_fvs` (ordered free-var
collection), `expr_mentions` (the host's conservative AST check),
`try_select_lazy` (the select_lazy → if rewrite — part of definitional
equality, so mirrored), per-tag desugars `w_num`/`w_if`/`w_match`/
`w_recvalue` (hand-factored per Appendix A option a), and the fold
`expr_to_cir`, a match over the AST coproduct mirroring the host switch.
Encoding tags are CAPITALIZED constructor-style (`Var`/`App`/`If`/`Match`…,
the bracket.disp Cir precedent) — the host's lowercase `if`/`match` are
keywords and can't be surface match patterns. Constructors are declared via
the cut's `inj0`–`inj3` declarators (`e_app := inj2 "App"`). Composition:
`compile_expr := bracket_compile ∘ expr_to_cir env`, and the positional mode
bit is the entry point: `compile_type env e = compile_expr env (binder_to_pi
e)` with the host's Pi-in-scope fallback.

Hazard found writing the fold (now CLAUDE.md § Compiler workarounds, the
**η/saturation rule**): a match-arm call saturated by fv-row vars alone
(`self env e`) η-exposes under bracket abstraction and the cut's fv
distribution evaluates it eagerly for every arm — selected or not — so a
recursive one regresses forever. Arms must thread an arm-bound var in every
call spine; when the scrutinee itself is needed, rebuild it from the binders
(`e_rectype fields` is hash-cons-identical to `e`). This predates the new
desugar (the old shape distributed at the enclosing-binder level with the
same η-exposure); it simply had no in-language match code dense enough to
trip it before.

**Prerequisite host-side match fix: DONE 2026-06-12.** The parser now emits a
`match` Expr node; the desugar lives in compile.ts and closes the WHOLE cut
over the arms' free-var union: `(λfvs… . prod (pair names handlers) c) fvs…`.
The fvs close *around* the cut rather than being appended to the selected
handler's result — kernel idioms rely on a mis-tagged cut (a respond
returning `Err` into `hyp_reduce`'s `Extend|Reduce` match) staying INERT, and
extra args applied to that junk re-enter recursion (found the hard way:
`wrapping.test.disp` diverged under the appended-row variant). With no free
vars the output is bit-identical to the old desugar, so only matches under
binders migrated.

Deliberately out of the in-language spec (deferred with rationale):
- `use` + module records/metadata + the projection collapse — Stage 5's
  driver boundary (the collapse is metadata-only: for plain records both
  sides emit the runtime cut, and eager closed-redex evaluation converges).
- recValue MEMBERS (`let`/`test`/`open` inside braces) and `trailing` —
  driver territory (sinks, open-splicing).
- Array literals and field puns — parser-level desugars (no Expr node
  survives to compile), so they're Stage 4.
- The host's `_anon${i}`/`__m${k}`/`__p` fresh-name SPELLINGS — binder names
  erase under bracket abstraction, so only binding structure + fresh_for
  capture-safety must match (anon params bind the leaf as their name).
- Host THROW paths (missing kernel formers, holes, unbound vars at
  cirToTree) fall to the leaf, as in bracket.disp.

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

- **Closed-prefix redexes: RETIRED** (the §4 match-desugar fix landed
  2026-06-12) — match arms close over their free vars, so `self name x` in an
  arm stays open under bracket abstraction; no `wait` needed. The remaining
  blow-up-family member is the S-duplication size blow-up
  (Appendix A): multi-occurrence helpers under nested binders
  still need hand-factoring (option a) — expect it in the bigger AST folds.
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

---

## Appendix A — Sharing-aware bracket abstraction (the compile-time blow-up fix)

**Status:** design note (2026-06-05; merged in from `ABSTRACTION_SHARING.md`
2026-06-12). Describes the compiler-level fix for the bracket-abstraction term
blow-up that currently forces hand-factoring in the kernel (e.g. `w_s` /
`w_tfk` in `lib/kernel/core.disp`). Option (a) — keep factoring, documented
inline at the `w_s` definition — has landed; this appendix is the spec for
option (b), the general fix that removes the need to factor.

Audience: anyone touching `src/compile.ts` bracket abstraction (`abstractName`,
`eliminateLams`, `cirToTree`), or hitting "Evaluation budget exhausted" while
compiling `.disp` code.

> **Constraint added 2026-06-11:** bracket abstraction now has an in-language
> specification (`lib/elab/bracket.disp`, validated bit-identically by
> `lib/tests/bracket.test.disp` + `test/bracket.test.ts`). Abstraction defines
> which tree a binder becomes — i.e. definitional equality — so option (b), or
> ANY change to `abstractName`/`eliminateLams`/`cirToTree`, must land in
> LOCKSTEP with the in-language spec and its equivalence tests, and it changes
> every compiled tree (a conversion-identity migration, not a pure optimization).

### A.1 Symptom

Inlining a multi-occurrence helper into a function with several binders makes the
file fail to compile. Concretely, replacing the factored call in `w_fork`

```disp
({c} -> w_s self c b x)
```

with `w_s`'s body inlined

```disp
({c} -> bind (self c x) ({cx} -> bind (self b x) ({bx} -> self cx bx)))
```

and loading the kernel produces:

```
error: Evaluation budget exhausted (0 steps)
```

(With no host memory cap the same blow-up manifests as a heap OOM — that is what
the original `// TODO: ... causs OOM` comment recorded.) The factored version
compiles in ~1.6 s; the inlined version never finishes.

This is the same failure family as the documented `select_lazy` + self-recursion
workaround in `CLAUDE.md`, but here it is triggered by plain variable duplication
under binders — no `fix` unfolding is required.

### A.2 Root cause

Two mechanisms in `src/compile.ts` compound.

#### A.2.1 Bracket abstraction duplicates (`abstractName`, ~L75)

The abstractor uses the **S/K/I** basis only. For an application whose bound
variable occurs in *both* sub-terms it emits

```
[x](f a)  →  S ([x]f) ([x]a)          // abstractName, the cap(cap(S, af), ax) case
```

`S` distributes the argument to *both* branches, so the abstraction is performed
twice — once per branch. There are two η/K peephole optimizations already present
(`S (K p) I → p`, and `S (K p) (K q) → K (p q)`), but neither helps when the
variable genuinely occurs on both sides.

`eliminateLams` abstracts binders innermost-first, one variable at a time. So a
body under *k* nested binders, where each binder's variable recurs, is walked by
*k* successive S-distributing passes. A variable occurring *n* times can have its
abstraction duplicated at every enclosing binder → the combinator term grows
super-linearly, toward `O(2^k)` in the adversarial case.

`w_s`'s body references `self` ×3, `x` ×2, `b` ×1 across the nested lambdas
`{cx}` / `{bx}`. Inlined under `w_fork`'s `{self,a,b,x}` plus the stem-branch
`{c}`, those occurrences are S-duplicated through ~6 nested abstractions.

#### A.2.2 `cirToTree` eagerly normalizes the result (~L126)

After abstraction, `cirToTree` lowers `Cir → Tree`. Structural S/K/I shapes are
mapped directly, but any *residual* application falls through to

```ts
return applyTree(cirToTree(e.f), cirToTree(e.x), APPLY_BUDGET)   // APPLY_BUDGET = 10_000_000
```

i.e. the blown-up closed combinator is **fully reduced at compile time**. The
reduction of the duplicated term allocates a flood of intermediate hash-consed
nodes and burns through the 10 M step budget — the observed error.

#### A.2.3 Why factoring fixes it

As a named `let`, `w_s` is bracket-abstracted over just `{self,c,b,x}` and
reduced **once** by `cirToTree` into a single hash-consed `Tree`, stored as a
`lit`. `w_fork`'s branch then references that literal:

```
app⁴(lit_ws, var self, var c, var b, var x)
```

Each variable now occurs exactly once in the spine, so abstracting
`{self,a,b,x,c}` over it is linear and there is no body to re-reduce. The factor
is, in effect, manual common-subexpression elimination / let-floating that the
abstractor does not perform itself.

### A.3 The fix (option b)

Goal: make the compiler avoid the blow-up so authors need not hand-factor, while
preserving the invariants the rest of the system relies on (see §A.6).

Three candidate approaches, in increasing order of preference.

#### A.3.1 Richer combinator basis (B, C, S′, …) — partial

Curry/Turner bracket abstraction with **B** and **C** avoids S when the variable
occurs in only *one* branch:

```
[x](f a),  x∉a   →  C ([x]f) a
[x](f a),  x∉f   →  B f ([x]a)
[x](f a),  x∈f,a →  S ([x]f) ([x]a)   // still duplicates
```

This removes the duplication for one-sided occurrences (common) but does **not**
fix the genuine multi-occurrence case (`S`), which is exactly `w_s`. Worth doing
as a strict improvement to term size in general, but insufficient alone. Note: B
and C must be added to the `Cir` IR and lowered in `cirToTree` to their tree
encodings (and validated bit-identical against their λ-definitions, per the
metacircular discipline).

#### A.3.2 Stop eager normalization in `cirToTree` — risky

Make `cirToTree` build application *nodes* lazily (or reduce only to WHNF) instead
of fully normalizing at L126. This directly removes the second mechanism — the
abstraction can stay large but is never eagerly evaluated.

The hazard: conversion in this language is `tree_eq`, an O(1) hash-cons identity
check that **assumes deterministic elaboration produces a canonical (normalized)
tree** — same type ⇒ same tree id (see `KERNEL_DESIGN.md`, "Hash-consing is
load-bearing"). If two definitionally-equal terms lower to different un-normalized
trees, `tree_eq` stops recognizing them as equal and type checking breaks. Any
move here must guarantee the lowered tree is still the unique normal form (e.g.
normalize lazily but memoize to the same canonical node). High blast radius;
treat as a last resort.

#### A.3.3 Automatic sharing / CSE on the `Cir` (recommended)

Do explicitly, in the compiler, what the manual factor does by hand: detect
shared sub-terms and compile each **once**, then reference the result.

Two layers, either or both:

1. **Hash-cons the `Cir` IR.** Today `cap` (L37) builds fresh `app` nodes with no
   sharing; `containsFree` and `abstractName` re-traverse structurally. Intern
   `Cir` nodes by structure (as `Tree` is interned in `tree.ts`) so identical
   sub-terms share one node and memoized passes (`containsFree`, the abstraction
   itself) are computed once per distinct sub-term. This bounds the *work*, though
   not necessarily the *result size*.

2. **Let-float multiply-occurring closed sub-terms.** Before abstraction, find
   maximal sub-terms that (a) occur more than once and (b) become closed after the
   current binders are accounted for, lower each to a `lit` once via `cirToTree`,
   and replace occurrences with that literal. This is precisely the `w_s` factor,
   applied automatically. Because a `lit` is opaque to `abstractName`
   (`containsFree` returns false → `K`-wrapped), it is never duplicated or
   re-reduced. This directly kills both mechanisms for the shared sub-term.

A principled variant of (2) is **Kiselyov-style abstraction with occurrence
tracking** ("λ to SKI, semantically"): tag each sub-term with whether the
abstracted variable is used in neither / left / right / both, emitting K / C / B /
S accordingly, and combine with maximal sharing. This subsumes §A.3.1 and gives
the sharing of §A.3.3 in one pass.

**Recommendation:** implement §A.3.3 — Cir-level hash-consing plus let-floating
of shared closed sub-terms — optionally on top of a B/C basis (§A.3.1). Avoid
§A.3.2 unless the canonical-normal-form guarantee can be kept.

### A.4 Implementation sketch (§A.3.3)

All in `src/compile.ts`:

1. **Intern `Cir`.** Replace `cap` with a hash-consing constructor (a `Map`
   keyed on child ids, like `tree.ts`'s `forkCache`). Give each `Cir` node a
   stable id. `lit`/`var`/`S`/`K`/`I` are interned by content.
2. **Occurrence map.** During/after `exprToCir`, compute for each interned node
   its occurrence count in the surrounding term (cheap with interning).
3. **Let-float pass.** Walk the body; for each maximal sub-term that is closed in
   the current binder scope and occurs ≥2 times, lower once via
   `cirToTree(eliminateLams(...))`, cache as a `lit`, and substitute. Run before
   `eliminateLams`/`abstractName` so the abstractor sees `lit`s, not duplicates.
4. **(Optional) B/C basis.** Extend the `Cir` union with `B`/`C`, add the
   one-sided rules to `abstractName`, and lower `B`/`C` in `cirToTree`.
5. Leave `APPLY_BUDGET` and the `cirToTree` eager reduction as-is — once the term
   no longer blows up, the eager reduction is cheap again.

### A.5 Validation

- **Regression that would currently fail:** add a kernel form (or a dedicated
  fixture) with `w_s` *inlined* into `w_fork`; assert the file compiles within
  budget and the kernel tests stay green. This locks the fix and prevents
  regressions back to mandatory hand-factoring.
- **Bit-identical lowering:** the transformed pipeline must produce the *same*
  `Tree` ids as the factored source for every existing binding — run the full
  suite and assert no test regresses (conversion depends on tree identity,
  §A.3.2). A focused check: compile the kernel both ways and compare the
  resulting tree ids for shared definitions.
- **Complexity microbenchmark:** a synthetic family `f_k` with a body of *k*
  nested binders each reusing all outer variables; assert compile time / node
  count grows ~linearly, not exponentially.
- **B/C encodings (if added):** unit-test that the `B`/`C` tree encodings reduce
  identically to their λ-definitions (`tree.test.ts`).

### A.6 Risks & interactions

- **Hash-cons identity / conversion.** The non-negotiable invariant: lowering must
  still yield the canonical normal-form tree per definition (`tree_eq` is the
  conversion check). §A.3.3 preserves this (it only changes *when/where* a shared
  sub-term is reduced, not the final tree); §A.3.2 endangers it.
- **Deterministic elaboration.** Sharing decisions must be deterministic
  (occurrence order, interning order) so repeated compiles produce identical
  trees — required for the native `tree_eq` fast-path registration
  (`compile.ts` `setTreeEqId`) and for reproducible builds.
- **Scope of change.** Self-contained to `src/compile.ts`; no `.disp` or
  `src/tree.ts` changes needed. The kernel can drop its manual factors once the
  regression in §A.5 passes, but that cleanup is optional and separable.
