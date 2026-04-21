# ctxtree: tagged trees + parallel context

## Purpose

The **canonical-representation backend**. ctxtree's Val shape is
exactly the one committed to in `TYPE_THEORY.typ` §4: a `(term, ctx)`
pair where term is a tagged tree and ctx is a parallel context-tree
tracking binder information. All spec primitives have direct,
O(1)-friendly implementations; in particular **`conv = fast_eq` after
normalization**, which is the design's defining performance property.

Reading order assumes familiarity with `TYPE_THEORY.typ` (the spec)
and `KERNEL_DESIGN.md` (the pre-sweep prototype notes, kept for
reference on tagged-form encodings).

## Data representations

### Term (tagged tree)

Terms are hash-consed tagged forks. Each kind occupies a stem-chain
leaf in the tag namespace:

```
kind         payload shape                                 semantic role
────────────────────────────────────────────────────────────────────────
KV           t                                             bound variable reference
KH           fork(stored_type, identity)                   free hypothesis
KA           fork(fn, arg)                                 application
KL           fork(fork(param_type, bindtree), body)        lambda
KP           fork(fork(dom, bindtree), cod)                Pi
KU           encoded_nat                                   universe value (Type k)
KM           marker                                        metavariable
```

A term is `tagged k payload = t (t TAG_ROOT k) payload`. Kind is
extracted via `kind t = first (first t)` (or a helper); payload via
`payload t = second t`. Hash-cons identity makes these O(1).

### Ctx (parallel tree)

Ctx mirrors the term's skeleton, carrying binder info at V-leaf
positions:

```
CtxNone                                         — no binder info here
CtxV(binder_id, type_term, type_ctx)            — at a V-leaf position
CtxApp(ctx_fn, ctx_arg)
CtxLam(ctx_param_type, ctx_body)
CtxPi(ctx_dom, ctx_cod)
```

CtxV stores the binder that this V-leaf references: a binder-id
(distinguishing nested binders), the type the binder introduced, and
the ctx in which that type resolves (types can contain their own free
V-leaves referencing outer binders).

### Val = (term, ctx)

A Val is `fork(term, ctx)`. Closed Vals (most types, fully-reduced
data) have `CtxNone` for the ctx component; Values introduced during
binder descent carry non-trivial ctx.

This is the canonical Val shape committed to in the spec. Other
backends may represent Vals differently internally; ctxtree's
representation IS the canonical one.

### BindTree

Bind-trees guide splicing when lambdas beta-reduce. Same shape as in
the prototype:

```
BE    — substitute value here
BN    — preserve original term here
KBApp(b_fn, b_arg)
KBLam(b_param_type, b_body)
KBPi(b_dom, b_cod)
```

## Primitive implementations

All implementations operate on `(term, ctx)` pairs. `pair_t` and
`pair_c` project the components.

### apply(f, v) → Value

Head β-reduction. Build a KA node and normalize; reduces immediately
if `f` is a lambda.

```
apply (f_t, f_c) (v_t, v_c) =
  normalize_pair (mkApp f_t v_t) (mkCtxApp f_c v_c)

normalize_pair term ctx =
  case term of
    KA (f, x) -> let (f', f_c') = normalize_pair f (ctx_app_fn ctx)
                     (x', x_c') = normalize_pair x (ctx_app_arg ctx)
                 in if is_lam f'
                    then splice+ctx_enter on body with x'
                    else (mkApp f' x', mkCtxApp f_c' x_c')
    ... other cases
```

Splice updates both term and ctx in parallel (`splice` on term,
`splice_ctx` on ctx).

**H-hypothesis rule.** When `apply` reduces to `apply P h` where `h`
has `is_H h = TT` (a KH-tagged term with CtxV ctx), and
`conv P (type_of_H h) = TT`, short-circuit to `TT`. Detected before
reducing `P`'s body.

### conv(v1, v2) → Bool

```
conv (t1, c1) (t2, c2) = and (fast_eq t1 t2) (fast_eq c1 c2)
```

Two O(1) hash-cons comparisons. This is the design's defining
property: after normalization, semantic equality coincides with
structural tree identity. No recursive descent needed.

### fresh_hyp(A) → Value

A fresh hypothesis is a V-leaf with a matching CtxV entry. The
evaluator tracks binder depth; the returned Val represents "the
parameter bound at the innermost enclosing descent":

```
fresh_hyp A_val =
  let depth = evaluator.current_binder_depth
      (A_t, A_c) = A_val
  in (mkV, CtxV(depth, A_t, A_c))
```

The ctx_enter_binder operation extends ctx at bindtree-marked
positions; `fresh_hyp` is the caller-facing view of that operation.

### is_H(v), type_of_H(v), identity_of_H(v)

```
is_H (t, c) = and (fast_eq (kind t) KV) (is_ctxv c)
              or  (fast_eq (kind t) KH)
              
type_of_H (mkV, CtxV(_, A_t, A_c)) = (A_t, A_c)
type_of_H (KH t, CtxNone)          = (first (payload t), CtxNone)

identity_of_H (mkV, CtxV(bid, _, _)) = bid
identity_of_H (KH t, _)              = second (payload t)
```

Two cases because hypotheses appear in two encodings: as V-leaves with
CtxV entries (introduced during descent) or as pre-built KH tokens
(hand-rolled or serialized).

### is_pi(v), pi_dom(v), pi_cod(v)

```
is_pi (t, c) = fast_eq (kind t) KP

pi_dom (t, c) =
  let payload_fork = payload t     -- fork(fork(dom, bindtree), cod)
      dom_t        = first (first payload_fork)
  in (dom_t, ctx_pi_dom c)

pi_cod (t, c) = ({f} -> (second (payload t), ctx_pi_cod c))
                                   -- codomain as a function awaiting arg
```

`pi_cod` returns a function because the codomain is a B that must be
applied to a fresh hypothesis during checking.

### is_universe(v), universe_rank(v)

```
is_universe (t, c) = fast_eq (kind t) KU

universe_rank (t, c) = payload t    -- encoded nat
```

Universes are KU-tagged with an encoded-nat payload. Two universe
Vals at the same rank hash-cons to the same tree because the payload
is canonical.

### is_registered_base_type(v)

Threaded registry: each Type predicate closes over a canonical list of
registered rank-0 types (a right-folded fork-list of registered
Vals). Membership check:

```
is_registered_base_type v =
  right_fold (\entry acc -> or (conv v entry) acc) FF registry
```

Because conv = fast_eq for ctxtree, this is O(n) fast_eq calls where
n is the registry size. For modest registries this is acceptable.

## The canonical-form discipline

ctxtree's O(1) conv depends on a sharp discipline: **every
normalize_pair call produces terms that hash-cons correctly**.
Specifically:

1. Every β-reduction produces a canonical normal form.
2. Every binder descent uses `ctx_enter_binder` + eventual
   `ctx_exit_binder` so the bindtree on a Lam reflects the
   post-reduction body's V-positions.
3. Hypothesis identities (binder-ids in CtxV, level markers in KH)
   are derived from binder depth canonically — same depth, same
   identity.

Violating any of these breaks fast_eq on open terms, which cascades
into conv failure.

### The `ctx_exit_binder` gap

The prototype in `lib/predicates.disp` reuses the pre-reduction
bindtree when exiting a Lam. This is stale if β-reduction changed the
body's shape. The missing piece:

```
ctx_exit_binder : Ctx → binderId → (bindTree, Ctx)
```

Walks the ctx of a post-reduction body, emits a fresh bindtree with
BE at positions matching the given binderId and BN elsewhere, plus a
cleaned ctx with that binder's tags replaced by BN.

Algorithm (per earlier design notes):

```
ctx_exit_binder ctx id :=
  case ctx of
    CtxNone        -> pair BN CtxNone
    CtxV id' ty tc -> if fast_eq id id' 
                      then pair BE (CtxV _ ty tc)
                      else pair BN ctx
    CtxApp c_f c_x -> 
      let (b_f, c_f') = ctx_exit_binder c_f id
          (b_x, c_x') = ctx_exit_binder c_x id
      in pair (mkBApp b_f b_x) (mkCtxApp c_f' c_x')
    CtxLam / CtxPi -> analogous
```

With collapse-BN optimization: emit BN for compound nodes whose
children all returned BN.

This is the work item for bringing ctxtree up to full spec
compliance. Without it, certain reductions under binders (notably
`Eq Nat (succ zero) one`) fail even though they should succeed.

## Spec conformance checklist

| spec requirement | ctxtree implementation |
|---|---|
| `Val = (term, ctx)` canonical | direct — no wrapping |
| `apply` eager β, deterministic | `normalize_pair` with splice |
| `conv` = semantic equality | `fast_eq on both components`, O(1) |
| `fresh_hyp A` opaque, distinct | V-leaf + CtxV with binder-id |
| `is_H` bare-hypothesis only | KV+CtxV or KH tag check |
| H-hypothesis rule in `apply` | `conv`-check against stored type |
| universe canonical per rank | KU tag + encoded-nat payload |
| Pi canonical per (A, B) | KP tag + fork payload |
| registry closed-over | per-Type predicate argument |

## What this design gets right

- **One universe.** Terms, types, values all in the tagged-tree
  universe. No separate Val ADT.
- **Read-only terms.** Candidate terms never mutate; ctx-trees carry
  binder info alongside.
- **O(1) conv via fast_eq.** After normalization, hash-cons identity
  suffices for semantic equality. No readback step.
- **Matches spec Val shape directly.** No translation needed between
  ctxtree's internal representation and the spec's canonical `(term,
  ctx)` form.

## What this design gets wrong

- **Complexity budget.** Bind-trees are load-bearing and easy to
  mis-compute. The ctx-tree discipline has to be maintained at every
  Lam entry/exit, every β, every Pi entry/exit. Any drop in
  discipline silently breaks fast_eq on open terms, which cascades to
  conv failure.
- **No η.** Two η-equivalent terms with different bind-trees hash-cons
  differently. Adding η-conv requires a separate normalization pass
  at comparison time — absorbed into `conv`, but then `conv != fast_eq`
  for η-cases.
- **SKI erasure separate.** The tagged-form encoding must be erased to
  raw SKI for runtime execution; this is a distinct step specified in
  `COMPILATION.typ`, not free.

## Known open questions

1. **Binder-ID freshness during normalize.** `normalize_pair` needs a
   fresh-id source for intermediate binders. Thread the depth counter
   through, or derive fresh IDs from ctx's structure?
2. **Meta-substitution interaction.** When a meta is solved with a
   term, the substituted term doesn't carry a ctx. Current handling
   assumes decodeMetaSolutions happens host-side after check; verify
   this holds under the new spec.
3. **SKI cost of `ctx_exit_binder`.** Prior tokenize-reabstract attempts
   blew the 10M-step budget. The pair-threaded design should avoid
   this, but verify on the shared test suite.

## Implementation order

1. `use "../predicates.disp"` at the top of `impl.disp`.
2. Write `ctx_exit_binder` (structural tree walk, ~30 lines).
3. Update `normalize_pair`'s Lam and Pi branches to call
   `ctx_exit_binder` on exit. Thread fresh-id source.
4. Wire the spec primitives (`is_H`, `type_of_H`, `is_pi`, etc.) to
   their ctxtree implementations. Most are one-line helpers on top of
   existing tagged-form operations.
5. Implement `is_universe` / `universe_rank` via KU tag recognition.
6. Implement registry closure: each `Type` predicate captures its
   current registry as a closed-over value.
7. Export the shared contract (`check_*` names).
8. Run the test suite; `check_refl_succ_zero_one` should flip to TT.

## Relationship to the prototype (`lib/predicates.disp`)

The prototype predates the TYPE_THEORY sweep and uses pre-sweep names
(`pred_of_lvl`, `try_unify_pair`, etc.). `impl.disp` builds on the
prototype but adapts the exposed API to match the spec's primitive
names (`apply`, `conv`, `fresh_hyp`, …). The prototype's
infrastructure (tagged-form encoding, bind-trees, ctx-trees,
normalize_pair) is preserved essentially unchanged; the sweep is
mostly a naming and API alignment, plus the `ctx_exit_binder` gap.
