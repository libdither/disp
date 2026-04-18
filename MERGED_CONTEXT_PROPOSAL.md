# Merged-context bind-tree proposal

Status: **design**, not yet implemented. Written 2026-04-14 after discovering
bind-tree staleness during under-binder normalization (concretely: `succ zero`
and `one` fail to hash-cons-equate via `normalize`, even though they're
semantically identical Church numerals).

The user asked to implement this direct-to-tree-calculus, first using it for
PiCheck (pred_of_lvl), then seeing if it handles `Eq Nat (succ zero) one`.
Philosophy-compatible: no "host-side first, port later" — the whole thing
lives in `examples/predicates.disp` / `examples/elab.disp`.

## The problem this solves

Our tagged form is `mkLam(lamA, lamB, lamBody)` where `lamB` is a bind-tree
marking where the binder's V occurs in `lamBody`. `splice(body, lamB, arg)`
performs beta by walking `body` and `lamB` in parallel, replacing V at
BE-marked positions with `arg`.

**Staleness bug:** when `normalize` reduces inside `lamBody` (e.g., beta of a
sub-application), the reduced body has different V-positions than the
original, but `lamB` was computed pre-reduction and is never updated.
Subsequent operations (`splice` for nested beta, `fast_eq` for conversion)
see an inconsistent `(body, lamB)` pair.

Symptoms:
- `normalize(succ zero)` and `one` are semantically equal Church numerals but
  hash-cons to different trees (stale `lamB` fields).
- `Eq Nat (succ zero) one` cannot be proved via the inline Leibniz witness.
- Longer-term: any def-equality that depends on under-binder reduction.

The earlier tokenize-reabstract attempt (commit d1fbaa8… experimental, then
reverted) worked algorithmically but blew the 10M-step budget on trivial
typed defs because bracket-abstracting nested let-lambdas over `tok`,
`tokenized`, `normalized` produced exponentially large SKI trees.

## Core idea

Carry a **context tree** alongside the term during normalization. The context
is structurally isomorphic to the term, with **binder-IDs at V-leaves** that
say which outer binder each V refers to. Beta-reduction updates term and
context *in parallel* with two `splice` calls using the same bind-tree.
Bind-trees for each binder are extracted from the context only when that
binder's Lam is reconstructed on the way out.

In other words: instead of per-Lam bind-trees stored separately on each Lam,
we materialize a single "tag-per-position" structure for the current scope,
thread it through normalize, and project individual bind-trees back out at
Lam-exit boundaries.

## Context tree representation

Context tree `ctx` is isomorphic to the term it describes:

| term shape         | ctx shape                           |
|--------------------|-------------------------------------|
| `V`                | `BE_tag(id)` or `BN`                |
| atom/leaf/`H`      | `BN`                                |
| `mkApp(f, x)`      | `mkBApp(ctx_f, ctx_x)`              |
| `mkLam(A, B, body)`| `mkBLam(ctx_A, ctx_body)`           |
| `mkPi(A, B, cod)`  | `mkBPi(ctx_A, ctx_cod)`             |

Key change from today's bind-trees: **leaves hold a binder-ID instead of BE**.
A position marked for binder `id` means "the V here is bound by outer Lam
`id`." BN means "this V is free, or this position isn't a V."

### Encoding binder-IDs

Use **fork-shaped depth markers** just like `pred_of_lvl`'s current level
chain:
- `norm_depth_start = t t t` = `fork(leaf, leaf)`.
- `norm_depth_next = \d. t d t` = `fork(d, leaf)`.

A binder at depth `k` has ID = `k`-fold `lvl_next` applied to start. Binder-IDs
are hash-cons-identified trees, so `fast_eq` is O(1) for "is this V tagged by
binder k?"

### Encoding leaves

A leaf in ctx is either:
- `BN = t t = stem(leaf)` — no binder, or non-V position.
- **Tagged**: `stem(stem(id))` or similar — a stem-wrapping of the binder-ID
  to keep it disjoint from raw IDs (which might be needed as sub-contexts
  somewhere).

Actual encoding TBD during implementation — must be pairwise distinguishable
from BApp/BLam/BPi shapes. One safe choice:
- BN (as today): `t t` = `stem(leaf)`.
- Tagged-BE with binder-ID `id`: `t (t id)` = `stem(stem(id))`. First-level
  `stem` distinguishes from BN (which is `stem(leaf)` — leaf has no further
  stem wrap).

Then `untag : ctx_leaf → id` is `payload(payload(ctx_leaf))`, and `is_tagged`
is a structural test.

## Operations

Three new primitives alongside existing `splice`:

### `ctx_enter_binder : ctx_sub → lamB → id → ctx_extended`

When descending into a Lam during normalize:
- Input: `ctx_sub` (outer ctx restricted to this Lam's body — i.e., the
  subtree of incoming ctx at the lamBody position), `lamB` (this Lam's
  bind-tree for its own binder's V-positions), `id` (fresh binder-ID for
  the new depth).
- Output: `ctx_extended` — a context for this Lam's body with positions
  marked by `lamB` now tagged with `id`, and other positions preserving
  `ctx_sub`'s tags.

Walks `ctx_sub` and `lamB` in parallel:
- Where `lamB` says BE (V-for-this-binder position): replace `ctx_sub[here]`
  with `tagged(id)`. (The V at this position in the term is this binder's V,
  not any outer binder's.)
- Where `lamB` says BN: keep `ctx_sub[here]` unchanged.
- Where `lamB` has BApp/BLam/BPi: recurse.

### `ctx_exit_binder : ctx' → id → (bindB_new, ctx_cleaned)`

When exiting a Lam after normalizing its body:
- Input: `ctx'` (final context of normalized body, with tags for this binder
  AND outer binders), `id` (this binder's ID).
- Output: `bindB_new` (standard BE/BN-only bind-tree for this binder's
  positions in the normalized body), `ctx_cleaned` (ctx' with this binder's
  tags replaced by BN, preserving outer binder tags).

Walks `ctx'`:
- Leaves: if `tagged(id')` with `id' == id`: emit BE in bindB, BN in
  ctx_cleaned. If `tagged(id')` with `id' != id`: emit BN in bindB, preserve
  tag in ctx_cleaned. If BN: emit BN in both.
- Non-leaves: recurse, assembling BApp/BLam/BPi for both outputs.

Returns a pair `(bindB_new, ctx_cleaned)` — encode as `fork(bindB_new, ctx_cleaned)`.

### `splice_ctx : ctx_body → lamB → ctx_arg → ctx_body'`

Parallel to `splice` for the context side of beta reduction. Same structural
walk as term-side `splice`:
- `lamB` says BE: put `ctx_arg` at this position.
- `lamB` says BN: keep `ctx_body[here]`.
- `lamB` says BApp/BLam/BPi: recurse on sub-ctx and sub-bindB.

Critically, this is the **same algorithm** as `splice` — just operating on
the context space instead of the term space. We can likely share code.

## Normalize with context

Pseudocode (tree-calc port follows same shape):

```
normalize(term, ctx, depth) → (term', ctx'):
  case term of
    App(f, x):
      # decompose ctx: expect BApp or BN
      (ctx_f, ctx_x) = decompose_BApp(ctx)
      (f', ctx_f')   = normalize(f, ctx_f, depth)
      (x', ctx_x')   = normalize(x, ctx_x, depth)
      if is_lam(f'):
        # beta: parallel splice on term and context
        new_body = splice(lamBody(f'), lamB(f'), x')
        new_ctx  = splice_ctx(ctx_for_lamBody(ctx_f'), lamB(f'), ctx_x')
        return normalize(new_body, new_ctx, depth)
      else:
        return (mkApp(f', x'), mkBApp(ctx_f', ctx_x'))

    Lam(A, lamB, body):
      id                 = depth
      ctx_body_in        = decompose_BLam(ctx)  # sub-ctx for lamBody
      ctx_body_extended  = ctx_enter_binder(ctx_body_in, lamB, id)
      (body', ctx_body') = normalize(body, ctx_body_extended, lvl_next(depth))
      (new_lamB, ctx_body_cleaned) = ctx_exit_binder(ctx_body', id)
      # lamA is passed through unchanged; ctx_for_lamA unchanged.
      ctx_A = decompose_BLam_A(ctx)
      return (mkLam(A, new_lamB, body'),
              mkBLam(ctx_A, ctx_body_cleaned))

    Pi(A, piB, cod):
      # symmetric to Lam — Pis have binders too, same tokenize/extract pattern.
      ...

    H | atom | V:
      return (term, ctx)   # ctx at this position just passes through
```

### Initial context

For a top-level `normalize(term)` call: `ctx = BN_everywhere(term)` — a
ctx-tree structurally matching term but with BN at every leaf (no outer
binders in scope). Construct via a single walk:

```
empty_ctx(term):
  case is_app(term):  mkBApp(empty_ctx(appF), empty_ctx(appX))
  case is_lam(term):  mkBLam(empty_ctx(lamA), empty_ctx(lamBody))
  case is_pi(term):   mkBPi(empty_ctx(piA), empty_ctx(piCodom))
  else:               BN
```

Hmm — this is O(term size) startup per normalize call. Might be avoidable by
letting ctx be "virtually BN" when not yet split (lazy), but that adds
complexity. Start with eager; optimize later.

### Handling internal Lams during normalize

When walking into a nested Lam inside the body: extend ctx with *another*
binder-ID (the next depth), then recurse. On exit, extract that binder's
bindB. Outer binders' tags stay in ctx throughout.

Beta can fire anywhere — when it does, `splice_ctx` propagates all the outer
binders' tags correctly because the "arg" being substituted brings its own
sub-context (which has those tags recorded at the relevant V positions).

## Using it in PiCheck (pred_of_lvl)

PiCheck currently mints H tokens via `mkH(piA u, depth)` and splices them
into codom and body. With merged-context:

```
# At Lam-vs-Pi descent:
# Old: h = mkH(piA u, depth); rec(splice codom piB h, splice lamBody lamB h, state_next)
# New: keep the original V's in place; thread a ctx instead.
```

But PiCheck needs to *compare* terms (via try_unify). Currently it compares
normalized forms — `fast_eq normalize(a) normalize(b)`. With merged-context,
normalize returns `(a', ctx_a)` and `(b', ctx_b)`. We'd want to compare
under a shared "scope alignment": two V's at the same position in a and b
are equal iff their tags (in ctx_a and ctx_b) agree.

Equality up to binder alignment:

```
ctx_eq(a, ctx_a, b, ctx_b):
  # V-aware structural equality. Terms are equal when:
  #  - both are V and their ctx tags match (same binder-ID), OR
  #  - both are non-V and structurally equal with recursively aligned contexts.
  fast_eq(a, b) && fast_eq(ctx_a, ctx_b)
```

Actually: if we've already normalized (term, ctx) pairs coming from the same
outer scope, `ctx_a == ctx_b` via fast_eq is a proxy for "the V's line up
the same way." So comparison becomes:

```
fast_eq a b  AND  fast_eq ctx_a ctx_b
```

Both are O(1) hash-cons. Clean.

**Migration plan for pred_of_lvl:**

1. Thread `ctx` through pred_of_lvl alongside `state`. state already carries
   depth; extend it to also carry current-scope ctx for the term being
   checked and the type being checked.
2. Replace mkH+splice with ctx_enter_binder at each Lam-vs-Pi descent.
3. Replace `fast_eq (normalize a) (normalize b)` with ctx-aware comparison.
4. All existing tests must continue to pass — the state/ctx change is
   invisible to test outcomes, only to the kernel's internal representation.

The migration is bigger than a normalize rewrite because PiCheck's state
shape changes. But it's cleaner because the scope-management becomes uniform
(no mix of mkH tokens and bind-trees).

## Implementation steps

1. **Spec the ctx encoding.** Pick the leaf-tagging convention (tagged-BE
   vs BN vs bind-tree-combinator tags). Decide whether `BN_ctx = BN_bindtree
   = t t` (reuse) or need distinct constants.
2. **Write `ctx_enter_binder`, `ctx_exit_binder`, `splice_ctx`, `empty_ctx`**
   in `predicates.disp`. Each is a `fix`-recursive tree walk — mirroring
   existing `splice` / `compute_bind` / `replace_marker`.
3. **Write merged-context `normalize`**. Returns pairs `fork(term, ctx)`.
   Aim for flat structure (minimal nested `let`) to control SKI bloat.
4. **Rewire pred_of_lvl**. Thread ctx; replace mkH+splice with ctx_enter; use
   ctx-aware comparison in try_unify.
5. **Port to `elab.disp`.** Same kernel changes.
6. **Test**: first ensure all 111 existing tests still pass. Then add
   `Eq Nat (succ zero) one` and verify it typechecks via the new machinery.

## Risks and open questions

- **SKI cost.** The nested-let issue from the tokenize-reabstract attempt
  may recur if the ctx-threaded normalize has similar lambda nesting.
  Mitigation: use `fork(term, ctx)` pair-passing instead of `((\a b. ...) a b)`
  let-sharing; rely on hash-cons for pair sharing.
- **Pis have binders too.** `piB` for `(x : A) -> R` marks x's positions in
  R. The new normalize must handle Pi the same way as Lam — enter binder,
  extract on exit. This is a change from current normalize (which returns
  Pi unchanged). Likely needed for soundness under dependent types anyway.
- **Meta solutions carry terms without ctx.** When a meta gets solved and
  later substituted via `substituteMetas`, the substituted term doesn't
  come with a context. Either: (a) metas also track ctx; (b) we normalize
  only fully-contextualized terms (metas are substituted before normalize);
  (c) accept that meta-substituted terms need re-contextualizing. Choice
  depends on when meta substitution happens relative to normalize.
- **Bind-tree cache vs derived.** If ctx makes per-Lam bindB redundant for
  internal normalize work, do we still need to store bindB on Lam trees?
  Probably yes — bindB is needed when the Lam is passed around outside of
  normalization (e.g., to `erase`, to host `substituteMetas`). Keep both;
  bindB is derived from ctx at Lam-exit points.

## Success criteria

The proposal is validated when:

1. All 111 existing tests still pass after the migration.
2. `def succ_zero_is_one : Eq Nat (succ zero) one = \(P : Nat -> Type). \(p : P (succ zero)). p` typechecks at parse time.
3. `fast_eq (normalize (succ zero)) (normalize one) = TT` as a disp-level
   test — demonstrates that under-binder reduction produces hash-cons-equal
   trees.
4. No blown budget: each typed def check stays well under 10M SKI steps.

## Starting point after /compact

Files to work in:
- `examples/predicates.disp` — add ctx helpers, new normalize, new pred_of_lvl.
- `examples/elab.disp` — mirror the kernel changes, add the success-criterion tests.

Reference commits:
- `3c1564eb7085` (Church Bool/Nat/def-eq-reflection) — current HEAD baseline.
- `267f0f3b6b6b` (Pi-as-Type + Church Eq) — previous milestone.
- My experimental tokenize-reabstract approach is NOT committed — was reverted.

Key data structures to preserve (don't change):
- `TAG_ROOT`, KV/KH/KA/KL/KP/KM kind tags.
- `mkLam(A, B, body)` layout (A B body via fork-fork-fork accessors).
- `splice` (term-side), `compute_bind`, `replace_marker` — all stay; ctx
  helpers are additions.

Things to change:
- `normalize` signature: was `term → term`, becomes `(term, ctx) → (term, ctx)`
  internally. Public `normalize` can wrap: `\term. fork_left(normalize_ctx
  term (empty_ctx term))`.
- `pred_of_lvl` body: replace token-based descent with ctx-threaded descent.
- `state` shape: possibly carries ctx alongside depth and metas.

Key insight to carry forward:
**Substitution on (term, ctx) pairs is two `splice` calls with the same
bind-tree. Entering/exiting a binder is a context re-tagging walk. That's
all the machinery we need.**
