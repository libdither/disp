# Kernel Encoding — Design Notes

Captured algorithmic and implementation insights from an earlier
tree-calculus kernel prototype (removed during the type-theory cleanup
sweep). Nothing here is prescriptive for the next implementation — the
canonical spec is in [TYPE_THEORY.typ](TYPE_THEORY.typ). What follows
is reference: concrete shapes that worked, pitfalls that cost
implementation time, and tricks worth reaching for when re-encoding
the theory in tree calculus.

**Audience:** someone implementing the theory from `TYPE_THEORY.typ`
in tree calculus or a host language. If you're only using disp as a
language, this is not for you.

## 1. Tagged-form encoding

Elaborated terms were tagged forks at the root. A tag is a stem chain
of leaves (KV = `t`, KH = `t t`, KA = `t (t t)`, …); a term is
`tagged k payload = t (t TAG_ROOT k) payload`. Projections:

- `kind tg` extracts the tag
- `payload tg` extracts the body

Kinds used by the prototype:
- **KV** — bound variable reference. Canonical form: `tagged KV t`.
- **KH** — free hypothesis. Payload is `fork(type, level_marker)`.
- **KA** — application. Payload is `fork(fn, arg)`.
- **KL** — lambda. Payload is `fork(fork(param_type, bindtree), body)`.
- **KP** — Pi. Payload is `fork(fork(domain, bindtree), codomain)`.
- **KM** — metavariable. Payload is an opaque marker tree.

Stem-chain kinds make pattern-matching cheap: `is_kind k tg` is
`fast_eq k (kind tg)`, O(1). A parallel-namespace design is worth
preserving: if you introduce a sibling tagged form (e.g. the context
tree in §5), choose *fork*-rooted kinds so the two namespaces cannot
collide under hash-consing.

## 2. Bind trees

Lambdas and Pis carried a *bind tree* alongside the body:

```
mkLam param_type bindtree body
mkPi  domain     bindtree codomain
```

A bind tree is a structurally-minified copy of its body where:
- `BE` (`t`) marks a V-leaf position (where substitution targets land)
- `BN` (`t t`) marks any other leaf position (preserved as-is)
- `KBA` / `KBL` / `KBP` (fork tags) mirror app/lam/pi structure

The body holds actual subterms; the bind tree is a parallel skeleton
that tells `splice` *where* to substitute. Trades memory (two parallel
trees per binder) for substitution speed (no need to walk the body
searching for V's).

**Bind trees are load-bearing data, not decoration.** `splice` trusts
them. If a beta-reduction changes the body's shape, the outer bind
tree becomes stale, and subsequent splices silently corrupt the term.
A kernel rewrite that uses bind trees must rebuild them post-β (the
gap the prototype called `ctx_exit_binder` and never closed).

An implementation that uses plain walking-substitution (no bind trees)
doesn't hit this pitfall — at the cost of O(body size) per
substitution.

## 3. Splice

Concrete substitution: `splice term bindtree value` walks `bindtree`
in lockstep with `term`, replacing content at `BE` positions with
`value`. The bindtree-guided version skips walking `term`'s
non-substitution positions.

Beta reduction is one splice call:

```
beta_redex app_term =
  splice (lamBody (appF app_term))
         (lamB   (appF app_term))
         (appX   app_term)
```

## 4. Normalize

Weak-head first with subparts re-check:

```
normalize term =
  if term is App f x:
    let nf = normalize f
    let nx = normalize x
    if nf is Lam:
      normalize (beta_redex (mkApp nf nx))   -- re-normalize after β
    else:
      mkApp nf nx                            -- no β; freeze
  if term is Lam A B body:
    mkLam A B (normalize body)
  else:
    term
```

The re-normalize after β is what lets `Eq A x x` reduce all the way
even though the inner `Eq A` only becomes a lambda after its own β.
Without it, you get stuck App chains and false "not reducible"
rejections.

## 5. Context trees

An NbE strategy for dependent-type checking without mutating the
candidate term during descent. Instead of splicing `mkH A lvl` into a
body on binder entry, thread a *parallel context tree* alongside the
body that carries the type info.

Structure mirrors the term's skeleton:
- `CtxV(bid, type_term, type_ctx)` at V-leaf positions — the binder's
  id, the binder's stored type, and the ctx in which that type
  resolves.
- `CtxApp`, `CtxLam`, `CtxPi` at structural nodes, parallel to KA /
  KL / KP in the term.
- `CtxNone` elsewhere.

Two helpers:
- `ctx_enter_binder ctx bindtree bid type type_ctx` — overlays
  `CtxV` at each `BE` in bindtree; leaves other positions as in `ctx`.
  Replaces the splice-plus-mkH pattern for entering a binder.
- `splice_ctx ctx bindtree value_ctx` — parallel to `splice` but
  produces ctx-tree constructors at fork positions.

Why bother?
- Term stays read-only during descent. `fast_eq` on the candidate
  is stable across recursion; no re-hashing.
- Pattern transfers to systems without explicit binders (raw SKI)
  where there's no "planting site" for `mkH` tokens.
- The V branch of the check function becomes reachable — it's
  unreachable under splice-mkH because mkH eliminates V's during
  descent. Litmus test: `{x : A} -> {y : A} -> x` should reject
  being swapped to `{x : A} -> {y : A} -> y`; if V's were spliced
  away, both check as the same candidate against the same ctx.

## 6. (term, ctx) pairs for type equality

During checking, a "type" is naturally `(term, ctx)` — the tree and
the ctx in which its free V's resolve. Equality becomes `fast_eq` on
both components, O(1), no flattening to mkH-form needed.

Helpers: `mkPair`, `pair_t`, `pair_c`. `resolve_V_pair term ctx`
does one-step peel: if `ctx` is `CtxV`, return the binder's stored
`(type, type_ctx)` pair; else return `(term, ctx)`. Not recursive —
one peel is enough for structural dispatch because stored types
carry their own ctx.

## 7. Meta / hole substrate

Metas are tagged `KM marker`, where `marker` is an opaque tree minted
fresh per `_` site by the elaborator. Two metas are equal iff their
markers are hash-cons-identical.

Meta entries: `(marker, Unsolved type | Solved value)`. The meta list
is a right-folded structural list (leaf = nil). The elaboration state
carries depth plus metas; updates are structural (hash-cons sharing
keeps the cost proportional to the spine of the change, not to the
whole state).

`try_unify_pair a_t a_c b_t b_c state` handles meta-aware equality:
- If either side is a meta, solve it against the other.
- If both are metas, create an alias edge (`a := mkMeta b`). Let the
  host resolve alias chains at decode time — cheaper than threading
  chain-following through every kernel call. (Tree-side chain
  resolution can be reintroduced later if self-hosted checking
  demands it.)
- Otherwise fall back to `fast_eq` on normalized terms.

## 8. Check dispatch shape

Candidate-first dispatch, because a Lam against a non-Pi expectation
is FF regardless:

```
check ty tyCtx cand candCtx state =
  let eff = resolve_V_pair cand candCtx
  case eff.term:
    Pi _ _ _         → Pi-as-Type check (any Pi inhabits Type)
    Lam A B body     → require ty is Pi; compare domains;
                       recurse on bodies under extended ctx
                       (ctx_enter_binder on both sides)
    V                → ctx_lookup + try_unify with ty
    App f x          → infer f's type; recurse x against piA
    H / atom / M     → direct fast_eq; fall back to H-unwrap /
                       try_unify
```

Returns `(bool, state')`. The state threads meta solutions.

**`Type` as a reserved H token.** The prototype used
`Type = mkH Type_sentinel t` where `Type_sentinel` was a
distinguishable tree. The Pi-as-Type rule recognized it by hash-cons
identity against the normalized expected type. Under the new theory
(TYPE_THEORY.typ §5) `Type` becomes a user-visible predicate, but
keeping it as a well-known hash-cons-identifiable token remains a
reasonable implementation choice.

## 9. General tree-calculus kernel pitfalls

Independent of the specific encoding above — these apply to any
tree-calculus kernel written against the Tree calculus runtime.

**Recursive-call arg order under `wait` / `fix`.** `wait` defers
evaluation of one of its arguments; place inner-binder-derived data
as that argument. Outer-binder-derived arguments fire eagerly and
cause compile-time divergence. Symptom: `fix (λrec. body)` hangs the
compiler or produces an exponential tree.

**Strict-branch deferral.** `triage` evaluates every branch eagerly;
the dispatch arrow doesn't short-circuit. Wrap each case as
`λu. body` and apply after dispatch to prevent eager recursion in
non-taken branches. Symptom: pattern-matching over a recursive data
type diverges before the right branch is picked.

**H-comparison: direct `fast_eq` first.** Two H-tokens with the same
stored type and level marker are hash-cons-equal. A direct `fast_eq
a b` on two H-tokens is cheaper than extracting both types via
`type_of_H` and comparing. Only fall back to the extract-compare
path when the direct check fails (e.g., one side is an H but the
other is a structural type like `{x : A} -> B`).

## What this document is not

- A specification of the next kernel. That's `TYPE_THEORY.typ`.
- A complete implementation guide. Many details — exact tag shapes,
  state layout, meta-entry list ordering — are open; choose what
  fits your chosen internal value representation.
- Prescriptive. The prototype made specific choices; a fresh
  implementation might choose differently and still satisfy the
  theory.
