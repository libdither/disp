# ctxtree: tagged lambdas + parallel context-tree

## Purpose

Finish what `lib/predicates.disp` started: the bind-tree NbE design with
per-Lam canonical hypothesis tokens, plus the parallel ctx-tree to fix the
stale-bind-tree bug that blocks `Eq Nat (succ zero) one`.

This is the **metacircular-friendly** design: one universe, terms and
values are the same type (tagged forks), `quote` is the identity
function, `fast_eq` on normal forms gives O(1) conversion.

The only missing piece from the current repo is `ctx_exit_binder`:
extracting a fresh bind-tree from the post-reduction ctx at Lam ascent.
Design covered in `MERGED_CONTEXT_PROPOSAL.md` §3; this doc is the
concise spec for the `impl.disp` port.

## What's already built (inherited from `lib/predicates.disp`)

The current file already has:
- Tagged-form encoding: `mkLam`, `mkPi`, `mkApp`, `mkH`, `mkV`, `mkMeta`.
- Bind-trees: `BE`, `BN`, `BApp`, `BLam`, `BPi` with `splice` / `compute_bind` / `replace_marker`.
- ctx-trees: `CtxV`, `CtxApp`, `CtxLam`, `CtxPi`, `ctx_none` with `ctx_enter_binder`, `ctx_lam_split_*`, `ctx_app_split_*`, `ctx_pi_split_*`.
- `normalize_pair`: ctx-aware β-reduction on (term, ctx) pairs.
- `infer`, `pred_of_lvl`, `try_unify_pair`, `check_atom_or_h_pair`.
- Elaboration state: `depth + metas` with `mk_res`, `state_next_depth`, etc.

**This design doc assumes that infrastructure and adds only what's
missing.** `impl.disp` will `use "../predicates.disp"` for the
infrastructure, then add the new primitives on top.

## The missing piece: `ctx_exit_binder`

Signature:
```
ctx_exit_binder : Ctx → binderId → (bindTree, Ctx)
```

Given the ctx-tree of a Lam's (possibly-reduced) body and the binder-ID
for *this* Lam, walk ctx producing:
- a fresh bind-tree with `BE` at positions tagged with `binderId`, `BN`
  elsewhere (mirroring ctx's structure);
- a cleaned ctx with this binder's tags replaced by `BN` (so outer
  binders' tags remain intact).

Encode the pair as `fork(bindTree, ctx_cleaned)`.

### Algorithm

Recurse on ctx, structural case analysis:

```
ctx_exit_binder ctx id :=
  case ctx of
    CtxNone        -> pair BN CtxNone
    CtxV id' ty tc -> if fast_eq id id' 
                      then pair BE (CtxV _ ty tc)     -- strip? see below
                      else pair BN ctx                -- keep outer tag
    CtxApp c_f c_x -> 
      let (b_f, c_f') = ctx_exit_binder c_f id in
      let (b_x, c_x') = ctx_exit_binder c_x id in
      pair (mkBApp b_f b_x) (mkCtxApp c_f' c_x')
    CtxLam c_A c_b -> 
      let (b_A, c_A') = ctx_exit_binder c_A id in
      let (b_b, c_b') = ctx_exit_binder c_b id in
      pair (mkBLam b_A b_b) (mkCtxLam c_A' c_b')
    CtxPi c_A c_c  -> 
      let (b_A, c_A') = ctx_exit_binder c_A id in
      let (b_c, c_c') = ctx_exit_binder c_c id in
      pair (mkBPi b_A b_c) (mkCtxPi c_A' c_c')
```

Open question in the clean/strip case: when we emit BE for a matching
CtxV, do we replace that CtxV with CtxNone in the cleaned ctx, or leave
it? The cleaned ctx is what the outer scope sees — from the outer
scope's perspective, this position is a V that belongs to the outer
binder's children, not to us. CtxNone is safest. Implementation: see
if tests pass with CtxNone; fall back to preserving if not.

### Collapse-BN optimization (optional)

When all children of a compound node return BN, emit BN for that node
entirely. This keeps bind-trees sparse:

```
if b_f == BN && b_x == BN then pair BN ctx_unchanged
```

Matches what `compute_bind` already does in the host-side elaborator.

## Updated `normalize_pair`

The current Lam case of `normalize_pair` reuses the pre-reduction `lamB`:

```
mkPair (mkLam (lamA u) (lamB u) (pair_t nb_pair))              -- STALE
       (mkCtxLam (ctx_lam_split_a ctx) (pair_c nb_pair))
```

After Phase 5: replace with ctx_exit_binder on the reduced body's ctx:

```
def normalize_pair_fixed = fix (\rec term ctx.
  ...
  ; Lam case:
  (bool_then
    (\u.
      ; recurse into body with extended ctx for this binder
      ((\nb_pair.
        ((\bid.
          ((\exit_pair.
            mkPair
              (mkLam (lamA u) (pair_t exit_pair) (pair_t nb_pair))
              (mkCtxLam (ctx_lam_split_a ctx) (pair_c exit_pair)))
           (ctx_exit_binder (pair_c nb_pair) bid)))
         (next_bid_for_ctx ctx)))       ; see below
       (rec (lamBody u) (ctx_enter_body_binder ctx u))))
    ...
    (is_lam term))
  ...)
```

Two supporting primitives:
- `next_bid_for_ctx` — mint a fresh binder-ID for this Lam (must be
  distinct from outer binder-IDs already in ctx). Reuse `state_depth`-
  style fork-chain markers.
- `ctx_enter_body_binder` — like `ctx_enter_binder` but threaded into
  normalize_pair's descent. Takes the outer ctx, the Lam's current
  lamB (used as guide for the *initial* tag positions — though these
  may shift during reduction), and produces the extended ctx the body
  should be normalized under.

**Key invariant**: every Lam is ctx-entered on the way in and
ctx-exited on the way out. The bind-tree stored on the resulting Lam
is always fresh, reflecting the *post-reduction* body's V-positions.

## Pi codom reduction

The current `normalize_pair` doesn't descend into Pi codomains. For a
complete fix, Pi descent also needs ctx_enter_binder + recurse + ctx_exit_binder:

```
(bool_then
  (\u.
    ((\nc_pair.
      ((\bid.
        ((\exit_pair.
          mkPair
            (mkPi (piA u) (pair_t exit_pair) (pair_t nc_pair))
            (mkCtxPi (ctx_pi_split_a ctx) (pair_c exit_pair)))
         (ctx_exit_binder (pair_c nc_pair) bid)))
       (next_bid_for_ctx ctx)))
     (rec (piCodom u) (ctx_enter_pi_binder ctx u))))
  (\u. mkPair term ctx)
  (is_pi term))
```

Without Pi descent, `Eq Nat (succ zero) one` still fails even with
Lam descent working — because the Eq type's codomain contains the
`succ zero` term that needs reducing.

## Updated `pred_of_lvl` (public entry)

No structural change needed — the Lam-vs-Pi case already threads ctx
via ctx_enter_binder. The fix lives entirely in `normalize_pair`'s
Lam/Pi-exit behavior. The public API stays identical:

```
def check = \term ty. res_bool 
  (pred_of_lvl ty ctx_none term ctx_none state_init)
```

## Shared export contract

Same as `debruijn/DESIGN.md`:

```
def check_id_nat               : TT/FF
def check_const                : TT/FF
def check_id_poly              : TT/FF
def check_apply_id             : TT/FF
def check_refl_zero            : TT/FF
def check_refl_succ_zero_one   : TT/FF     -- the stale-bindtree litmus
def reject_kstar_shadowed_dep  : TT/FF
def backend_name               : Atom
```

These are built by constructing the corresponding tagged terms and
types (using the existing elaborator via `elab NAME = ...`) and running
`check term ty`. The boolean result is exported.

For `check_refl_succ_zero_one` specifically: this is the test that's
**expected to transition from FF → TT** once ctx_exit_binder lands.
Before Phase 5, the current `lib/predicates.disp` already returns FF
here; after, it should return TT.

## What this design gets right

- **One universe.** Terms, types, values are all tagged trees. No
  separate Val domain, no opaque closures. Hash-cons identity
  discriminates everything.
- **Read-only terms.** `cand` is never mutated; ctx-trees carry the
  binding info alongside without touching the term.
- **O(1) conversion via `fast_eq`.** After normalization, hash-cons
  identity suffices. No readback walk needed.
- **Reuses existing infrastructure.** ~800 lines of `predicates.disp`
  stay unchanged; Phase 5 adds maybe 100 lines of `ctx_exit_binder`
  and updates to `normalize_pair` (~50 lines).

## What this design gets wrong

- **Complexity budget.** Bind-trees are load-bearing and easy to
  mis-compute. The ctx-tree discipline has to be maintained at every
  Lam entry/exit, every beta, every Pi entry/exit. Any drop in
  discipline causes silent staleness.
- **Bracket-abstraction target unclear.** After `erase`, tagged forms
  disappear — but ctx-trees presume tagged forms to parallel. The
  SKI-typecheck path (if ever built) needs a different story for
  neutrals. See `piPred` open question in `BIND_TREE_NBE_IDEA.md §7.5`.
- **No η.** Two η-equivalent terms with different bind-trees hash-cons
  to different trees. Adding η-conv requires a separate normalization
  pass at comparison time.

## Known open questions

1. **Binder-ID freshness during normalize.** `pred_of_lvl` already
   mints fresh IDs from `state.depth`. `normalize_pair` has no state
   argument; it needs its own freshness source. Can we thread the
   depth counter through normalize too, or derive fresh IDs from
   ctx's structure? (Latter is cleaner but more fiddly to get right.)
2. **Meta-substitution interaction.** When a meta is solved with a
   term, the substituted term doesn't carry a ctx. Does that break
   anything? Current `decodeMetaSolutions` happens host-side after
   check completes, so probably fine — but worth a test.
3. **SKI cost of the fix.** Prior tokenize-reabstract attempt blew the
   10M-step budget due to nested-let-lambda bracket abstraction. The
   pair-threaded design avoids deep let nesting, but should be
   verified on the suite.

## Implementation order

1. `use "../predicates.disp"` at the top of `impl.disp`.
2. Write `ctx_exit_binder` (structural tree walk, ~30 lines).
3. Write `next_bid_for_ctx` / refactor ctx-entry helpers to thread a
   fresh-id source.
4. Rewrite `normalize_pair`'s Lam branch to call `ctx_exit_binder` on
   exit. Add Pi branch too.
5. Re-run existing predicates.disp / elab.disp tests; ensure no
   regressions.
6. Export the shared contract (`check_*` names) by building tagged
   terms via `elab` and running check on them.
7. New test: `check_refl_succ_zero_one` should return TT.
