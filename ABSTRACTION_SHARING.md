# Sharing-aware bracket abstraction (the compile-time blow-up fix)

**Status:** design note (2026-06-05). Describes the compiler-level fix for the
bracket-abstraction term blow-up that currently forces hand-factoring in the
kernel (e.g. `w_s` / `w_tfk` in `lib/kernel/core.disp`). Option (a) — keep
factoring, documented inline at the `w_s` definition — has landed; this doc is
the spec for option (b), the general fix that removes the need to factor.

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

---

## 1. Symptom

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

## 2. Root cause

Two mechanisms in `src/compile.ts` compound.

### 2.1 Bracket abstraction duplicates (`abstractName`, ~L75)

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

### 2.2 `cirToTree` eagerly normalizes the result (~L126)

After abstraction, `cirToTree` lowers `Cir → Tree`. Structural S/K/I shapes are
mapped directly, but any *residual* application falls through to

```ts
return applyTree(cirToTree(e.f), cirToTree(e.x), APPLY_BUDGET)   // APPLY_BUDGET = 10_000_000
```

i.e. the blown-up closed combinator is **fully reduced at compile time**. The
reduction of the duplicated term allocates a flood of intermediate hash-consed
nodes and burns through the 10 M step budget — the observed error.

### 2.3 Why factoring fixes it

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

## 3. The fix (option b)

Goal: make the compiler avoid the blow-up so authors need not hand-factor, while
preserving the invariants the rest of the system relies on (see §5).

Three candidate approaches, in increasing order of preference.

### 3.1 Richer combinator basis (B, C, S′, …) — partial

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

### 3.2 Stop eager normalization in `cirToTree` — risky

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

### 3.3 Automatic sharing / CSE on the `Cir` (recommended)

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
S accordingly, and combine with maximal sharing. This subsumes §3.1 and gives the
sharing of §3.3 in one pass.

**Recommendation:** implement §3.3 — Cir-level hash-consing plus let-floating of
shared closed sub-terms — optionally on top of a B/C basis (§3.1). Avoid §3.2
unless the canonical-normal-form guarantee can be kept.

## 4. Implementation sketch (§3.3)

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

## 5. Validation

- **Regression that would currently fail:** add a kernel form (or a dedicated
  fixture) with `w_s` *inlined* into `w_fork`; assert the file compiles within
  budget and the kernel tests stay green. This locks the fix and prevents
  regressions back to mandatory hand-factoring.
- **Bit-identical lowering:** the transformed pipeline must produce the *same*
  `Tree` ids as the factored source for every existing binding — run the full
  suite and assert no test regresses (conversion depends on tree identity, §3.2).
  A focused check: compile the kernel both ways and compare the resulting tree ids
  for shared definitions.
- **Complexity microbenchmark:** a synthetic family `f_k` with a body of *k*
  nested binders each reusing all outer variables; assert compile time / node
  count grows ~linearly, not exponentially.
- **B/C encodings (if added):** unit-test that the `B`/`C` tree encodings reduce
  identically to their λ-definitions (`tree.test.ts`).

## 6. Risks & interactions

- **Hash-cons identity / conversion.** The non-negotiable invariant: lowering must
  still yield the canonical normal-form tree per definition (`tree_eq` is the
  conversion check). §3.3 preserves this (it only changes *when/where* a shared
  sub-term is reduced, not the final tree); §3.2 endangers it.
- **Deterministic elaboration.** Sharing decisions must be deterministic
  (occurrence order, interning order) so repeated compiles produce identical
  trees — required for the native `tree_eq` fast-path registration
  (`compile.ts` `setTreeEqId`) and for reproducible builds.
- **Scope of change.** Self-contained to `src/compile.ts`; no `.disp` or
  `src/tree.ts` changes needed. The kernel can drop its manual factors once the
  regression in §5 passes, but that cleanup is optional and separable.
