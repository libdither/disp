# debruijn: index-based variables + threaded context

## Purpose

**Reference oracle** against the canonical spec. debruijn occupies
the same structural slot as ctxtree — `Val = (term, ctx)` — but with
variable references encoded as de Bruijn indices in the term rather
than as tag-carrying V-leaves with ctx-side resolution. Two
independently-implemented backends converging on the same Val
semantics is the cross-validation that catches bugs in either.

This is a revision from the pre-sweep DESIGN.md, which specified a
Kovács-style closure-based NbE. Closure-based NbE has two universes
(terms vs. Values) and loses `conv = fast_eq` because closures with
structurally-different envs can denote the same function. The revised
design keeps the tree-calculus commitment and achieves
**`conv = fast_eq` after normalization**, matching the spec.

## Data representations

### Term (tagged tree with indices)

Terms are hash-consed tagged forks. The distinguishing feature vs.
ctxtree: variables are de Bruijn indices encoded as Nats, not
tag-distinguished V-leaves.

```
kind         payload                                       semantic role
────────────────────────────────────────────────────────────────────────
KIdx         encoded_nat                                   variable (index)
KHyp         fork(stored_type, identity)                   free hypothesis
KApp         fork(fn, arg)                                 application
KLam         body                                          lambda
KPi          fork(dom, cod)                                Pi type
KUniv        encoded_nat                                   universe value (Type k)
KMeta        marker                                        metavariable
```

Note: KLam's payload is just the body (no bindtree). debruijn reduces
by substitution — when a Lam is applied, we substitute the argument
for the bound index and shift other indices accordingly. No bindtree
is needed because indices self-identify.

Index convention: innermost binder is index 0, next-outer is 1, etc.
When crossing a binder outward (during readback), increment. When
crossing inward (during substitution), decrement or replace.

### Ctx (indexed list)

Ctx is a snoc-list indexed by depth: entry 0 is the innermost binder's
type, entry 1 is the next-outer, and so on:

```
CtxEmpty
CtxSnoc(outer_ctx, type_val)    — push a new binder on the inside
```

During checking, as we descend into a Lam, we push its parameter type.
When we look up an index, we walk the list until we reach that depth.

Encoded as a right-folded fork: `CtxSnoc o t = fork o t`, empty = leaf.

### Val = (term, ctx)

Same canonical shape as ctxtree:

```
Val = fork(term, ctx)
```

Closed Vals: `ctx = leaf` (CtxEmpty). Open Vals (under binders): ctx
has one entry per enclosing binder.

### The alignment with ctxtree

This representation is structurally close to ctxtree's, with the
differences:

| aspect | ctxtree | debruijn |
|---|---|---|
| variable encoding | `KV` tag, resolves via `CtxV` | `KIdx` tag carrying a Nat |
| ctx shape | tree (parallel to term) | list (indexed by depth) |
| binder identity | `binder_id` in `CtxV` | depth in the list |
| beta reduction | splice via bindtree | index-substitution |

Both achieve `conv = fast_eq` because both ensure canonical
normalization and consistent encoding across β-reduction.

## Primitive implementations

### apply(f, v) → Value

```
apply (f_t, f_c) (v_t, v_c) =
  normalize (mkApp f_t v_t) (merge_app_ctx f_c v_c)

normalize term ctx =
  case term of
    KApp (f, x) ->
      let (f', f_c') = normalize f ctx
          (x', x_c') = normalize x ctx
      in if is_klam f'
         then let body = klam_body f'
                  body' = subst_index_0 body x'      -- β, shifts other indices
              in normalize body' ctx
         else (mkApp f' x', merge_app_ctx f_c' x_c')
    KLam body -> (KLam (normalize body (push_fresh_hyp ctx)), ctx)
    ... other cases
```

`subst_index_0` replaces index 0 with the argument and decrements
higher indices. Standard de Bruijn β.

**H-hypothesis rule.** If `apply P h` where `h` is `KHyp` or an index
pointing at a ctx entry, and `conv P h's_stored_type = TT`, short-
circuit to `TT`.

### conv(v1, v2) → Bool

```
conv (t1, c1) (t2, c2) = and (fast_eq t1 t2) (fast_eq c1 c2)
```

O(1) via hash-cons identity after normalization. Same as ctxtree.

### fresh_hyp(A) → Value

Two encodings are possible; use KHyp (explicit hypothesis token) to
simplify reflection:

```
fresh_hyp A_val =
  let depth = evaluator.current_binder_depth
      (A_t, A_c) = A_val
      identity = encode_nat depth
  in (mkKHyp (fork A_t identity), A_c)
```

Using KHyp tokens (rather than ctx-resolved indices) makes `is_H`,
`type_of_H`, `identity_of_H` one-line reflections. The alternative —
relying purely on indices — would require ctx lookups for every
hypothesis reflection.

### is_H(v), type_of_H(v), identity_of_H(v)

```
is_H (t, c) = fast_eq (kind t) KHyp

type_of_H (t, c) = (first (payload t), c)
  -- stored type is first component of payload fork

identity_of_H (t, c) = second (payload t)
  -- identity (encoded nat depth) is second component
```

### is_pi(v), pi_dom(v), pi_cod(v)

```
is_pi (t, c) = fast_eq (kind t) KPi

pi_dom (t, c) = (first (payload t), c)

pi_cod (t, c) = ({x} -> apply-cod-to-x)
  -- where cod = second (payload t); returns a function awaiting
  -- the fresh hypothesis the checker will supply
```

### is_universe(v), universe_rank(v)

```
is_universe (t, c) = fast_eq (kind t) KUniv

universe_rank (t, c) = payload t
  -- the encoded Nat rank
```

### is_registered_base_type(v)

Same as ctxtree: threaded registry (closed-over by each Type
predicate), membership check via O(n) conv (= fast_eq in debruijn).

## Canonical-form discipline

Same three disciplines as ctxtree, adapted for index-based
representation:

1. Every β-reduction produces a canonical normal form — index
   substitution is deterministic.
2. Hypothesis identity is derived from binder depth canonically —
   same depth, same identity. No fresh counter (global mutable state
   would break cross-derivation identity).
3. Canonical form is maintained across all index operations. Two
   alpha-equivalent terms produce the same tree because indices
   provide canonical naming.

## Readback is unnecessary

Because Val = (term, ctx) and terms are already the normal form, the
classical "quote" step (Value → Term) is the identity. No readback.
This is the same structural win ctxtree gets.

## Spec conformance checklist

| spec requirement | debruijn implementation |
|---|---|
| `Val = (term, ctx)` canonical | direct |
| `apply` eager β, deterministic | index-substitution β |
| `conv` = semantic equality | `fast_eq` on both, O(1) |
| `fresh_hyp A` opaque, distinct | KHyp with depth-encoded identity |
| `is_H` bare-hypothesis only | KHyp tag check |
| H-hypothesis rule in `apply` | `conv`-check against stored type |
| universe canonical per rank | KUniv tag + encoded-nat payload |
| Pi canonical per (A, B) | KPi tag + fork payload |
| registry closed-over | per-Type predicate argument |

## What this design gets right

- **Indices are self-contained.** A term carries all the information
  about its free variables; no ctx lookup is needed to understand a
  term's shape. Simplifies many operations.
- **No bind-trees.** β-reduction via index substitution is simpler
  than splice-with-bindtree.
- **O(1) conv via fast_eq.** Matches ctxtree's performance property
  for the same structural reasons.
- **Oracle role intact.** Different variable encoding than ctxtree
  means two genuinely-independent implementations converge on Val
  semantics. Disagreements flag bugs.

## What this design gets wrong

- **Index shifting on substitution.** β-reduction walks the body
  decrementing indices. This is proportional to body size per β,
  whereas ctxtree's splice is proportional to BE positions in the
  bindtree. For lambda-heavy code, ctxtree is potentially faster.
- **Implicit depth tracking.** The evaluator must track current depth
  to assign correct indices to new hypotheses. State discipline is
  non-trivial.
- **Same SKI-erasure complexity as ctxtree.** Terms are tagged; they
  need erasing for runtime.

## Relationship to the pre-sweep design

The pre-sweep DESIGN.md specified a Kovács-style NbE:

- Separate Term and Val universes (violates one-universe goal).
- Closures as env + body pairs (loses `conv = fast_eq`).
- Neutrals as distinct ADT cases.

The revised design collapses to one universe (tagged trees), uses
index substitution instead of closures, and recovers `conv = fast_eq`.
The oracle role — "textbook dependent-type-checker, known-correct" —
is preserved by keeping the implementation style readably structural,
just without closures.

If a closure-based reference is still desired for comparison with
published NbE implementations, it can live as a separate
`lib/debruijn_closure/DESIGN.md` (not proposed here; raise if needed).

## Known open questions

1. **Depth tracking in fresh_hyp.** The evaluator maintains
   current_binder_depth. Is this threaded through operations explicitly
   (one more argument everywhere), or carried in host state? Host
   state is simpler but less purely tree-calculus.
2. **Index shifting performance.** For deeply-bound terms with many
   β-reductions, index shifting is O(body size * reductions).
   Benchmark vs. ctxtree splice; if significantly slower, consider
   lazy shifting.
3. **Cross-oracle validation.** Spec primitives should behave
   identically to ctxtree. Shared test suite needs extension to
   compare Vals across backends via `conv` (not backend-specific
   equality).

## Implementation order

1. Tagged-tree data: `mkIdx`, `mkKHyp`, `mkApp`, `mkLam`, `mkPi`,
   `mkUniv`, `mkMeta`.
2. Index substitution: `subst_index_0`, `shift_indices`.
3. `normalize` — β-reduction by substitution.
4. Spec primitives (`apply`, `conv`, `fresh_hyp`, reflection).
5. `is_type`'s four cases wired to primitives.
6. Registry closure and `is_registered_base_type`.
7. Shared export contract (`check_*` test judgments).
8. Oracle cross-validation: run same tests as ctxtree, verify
   agreement via conv.
