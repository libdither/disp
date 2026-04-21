# semantic: raw SKI terms + typed-neutral Val domain

## Purpose

The **terms-at-check-time-equal-terms-at-run-time** backend. Unlike
ctxtree and debruijn, semantic does not tag terms — source terms are
raw bracket-abstracted SKI trees, the same form that executes at
runtime. No erase step; no compilation from check-form to run-form.

The cost: the spec's canonical `Val = (term, ctx)` shape does not
apply. semantic uses an overlay Val ADT distinct from terms, with
self-typed neutrals carrying their inferred types. **`conv != fast_eq`
in general** because of the triage-on-neutral problem (§*The triage-on-
neutral problem* below). conv is implemented via structural recursion
with fresh-neutral introduction at binders.

Classification: **experimental backend**. Practical for studying the
"raw SKI at both check and run" property; not a primary target for
the kernel until triage-on-neutral is resolved.

## Data representations

### Term (raw tree)

Terms are raw bracket-abstracted SKI trees. No tags, no wrapping.

```
Term = Leaf | Stem(Term) | Fork(Term, Term)
```

Parser output is already in this form after bracket abstraction.
Surface `{x} -> x` compiles to `I = Fork(Fork(Leaf, Leaf), Leaf)`;
`{f, x} -> f x` compiles to some S/K composition; etc.

### Val (overlay ADT, tree-encoded)

Vals are distinct from terms. They're encoded as tagged trees (like
ctxtree's terms) but with a different tag family dedicated to the
value domain:

```
kind         payload                                       semantic role
────────────────────────────────────────────────────────────────────────
VLeaf        —                                             literal leaf value
VStem        Val                                           one-argument tree value
VFork        fork(Val, Val)                                two-argument tree value
VNeutral     Ne                                            stuck term (see below)
VPi          fork(dom_val, cod_val)                        Pi type
VType        encoded_nat                                   universe value (Type k)
VAtom        marker                                        base type constant
```

### Ne (neutral, self-typed)

```
Ne kind     payload                                         semantic role
────────────────────────────────────────────────────────────────────────
NVar         fork(unique_id, type_val)                     free var + its type
NApp         fork(head_ne, arg_val)                        neutral application
NApp3        fork(fork(stuck_head_ne, y_val), z_val)       stuck fork-in-head
NTriage      fork(fork(fork(w_val, x_val), y_val), scrut)  stuck triage on neutral
```

Key: `NVar` carries its inferred type directly. When you need "what's
this neutral's type," you ask the neutral itself. No external context
lookup. This is the mechanism that eliminates the context from
semantic's design.

### Why Val ≠ canonical (term, ctx)

Unlike ctxtree and debruijn, semantic's Val is not `(term, ctx)`. The
reasons:

1. Terms are raw SKI, which can't distinguish type forms (Pi, Universe)
   from data forms (Nat, Bool) structurally — both are just trees.
2. Overlay Vals introduce the structure the checker needs
   (Pi/Type/Atom/Neutral) on top of raw terms.
3. Self-typed neutrals eliminate the need for ctx.

This means semantic does not directly conform to the spec's canonical
Val shape. Backend implementations must expose operations whose
observable behavior matches; internal representation differs.

## Primitive implementations

### apply(f, v) → Value

The five tree-calc reduction rules lifted to Val, plus three stuck
points:

```
apply VLeaf v               = VStem(v)                    -- leaf-rule
apply (VStem a) v           = VFork(a, v)                 -- stem-rule
apply (VFork a b) v         = apply3(a, b, v)             -- fork-rule
apply (VNeutral n) v        = VNeutral(NApp(n, v))        -- STUCK (head)
apply VPi _                 = error                       -- typing violation
apply VType _               = error
apply VAtom _               = error

apply3 VLeaf y z            = y                           -- K-rule
apply3 (VStem c) y z        = apply(apply(c, z), apply(y, z))  -- S-rule
apply3 (VFork w x) y z      = triage(w, x, y, z)          -- F-rule
apply3 (VNeutral n) y z     = VNeutral(NApp3(n, y, z))    -- STUCK (apply3)

triage w x y VLeaf          = w                           -- rule 3a
triage w x y (VStem u)      = apply(x, u)                 -- rule 3b
triage w x y (VFork u v)    = apply(apply(y, u), v)       -- rule 3c
triage w x y (VNeutral n)   = VNeutral(NTriage(w, x, y, n))  -- STUCK (triage)
```

**H-hypothesis rule.** When `apply P v` where v is `VNeutral(NVar(id, T))`
and `conv(P, T) = TT`, short-circuit to `TT`.

### conv(v1, v2) → Bool

Structural recursive comparison. At binders (VPi), introduce a fresh
neutral and compare codomains. At leaves, compare atomic values.

```
conv VLeaf VLeaf                   = TT
conv (VStem a) (VStem b)           = conv(a, b)
conv (VFork a1 b1) (VFork a2 b2)   = and(conv(a1, a2), conv(b1, b2))
conv VType VType                   = TT
conv (VAtom m1) (VAtom m2)         = fast_eq(m1, m2)
conv (VPi d1 c1) (VPi d2 c2)       =
  if not(conv(d1, d2)) then FF
  else let x = fresh_hyp(d1)
       in conv(apply(c1, x), apply(c2, x))
conv (VNeutral n1) (VNeutral n2)   = conv_ne(n1, n2)
conv _ _                           = FF

conv_ne (NVar i1 _) (NVar i2 _)    = fast_eq(i1, i2)
conv_ne (NApp n1 v1) (NApp n2 v2)  = and(conv_ne(n1, n2), conv(v1, v2))
conv_ne (NApp3 ...) (NApp3 ...)    = all-three conv + conv_ne
conv_ne (NTriage w1 x1 y1 n1) (NTriage w2 x2 y2 n2) = 
  all-four conv/conv_ne
conv_ne _ _                        = FF
```

**Cost**: O(size) in worst case vs. ctxtree/debruijn's O(1). `fast_eq`
is used only at the atomic leaves (neutral IDs, VAtom markers).

### fresh_hyp(A) → Value

```
fresh_hyp A = VNeutral(NVar(fresh_id(), A))
```

Globally-unique `fresh_id()` — each call produces a distinct neutral.
No depth tracking; the neutral's unique_id is its identity.

### is_H(v), type_of_H(v), identity_of_H(v)

```
is_H (VNeutral(NVar _ _)) = TT
is_H _                    = FF

type_of_H (VNeutral(NVar _ type_val)) = type_val
identity_of_H (VNeutral(NVar id _))   = id
```

Bare-hypothesis only; NApp/NApp3/NTriage are different-category
neutrals (spine or stuck).

### is_pi(v), pi_dom(v), pi_cod(v)

```
is_pi (VPi _ _) = TT
is_pi _         = FF

pi_dom (VPi d _) = d
pi_cod (VPi _ c) = c      -- c is already a function (Val → Val)
```

### is_universe(v), universe_rank(v)

```
is_universe (VType _) = TT
is_universe _         = FF

universe_rank (VType k) = k
```

### is_registered_base_type(v)

Registry closed over by Type predicate; membership check via conv.
O(n * size) in worst case because conv is structural.

## The triage-on-neutral problem

This is semantic's defining limitation.

Consider the simplest typecheck: `I : Pi A (λ_. A)` where A is a
hypothesis. `I` as a bracket-abstracted term is
`Fork(Fork(Leaf, Leaf), Leaf)`. Applying `I` to a neutral `n`:

```
apply I n
= apply (VFork (VFork VLeaf VLeaf) VLeaf) n
= apply3 (VFork VLeaf VLeaf) VLeaf n
= triage VLeaf VLeaf VLeaf n              -- since VFork dispatches to triage
= VNeutral(NTriage(VLeaf, VLeaf, VLeaf, n))    -- STUCK
```

But extensionally, `I n = n` for any concrete `n`. The Val
`NTriage(VLeaf, VLeaf, VLeaf, n)` is structurally different from `n`
itself. So:

```
conv n (apply I n) = conv n NTriage(...) = FF   -- WRONG; should be TT
```

This breaks every check that has I applied to a neutral — i.e., every
polymorphic-identity check, and much more.

### Candidate fixes

1. **I-pattern recognition.** Special-case in `apply`:
   `apply (VFork (VFork VLeaf VLeaf) VLeaf) v = v`. Handles exact I.
   Doesn't generalize.
2. **η for triage-over-equal-branches.** Recognize
   `triage(w, x, y, n)` patterns that extensionally simplify. Hard to
   get right in general.
3. **Alternative bracket abstraction.** Use an abstraction scheme
   (Kiselyov's tA/tE) that produces neutral-transparent SKI when
   possible. Changes what the parser emits.
4. **Abandon the "terms = SKI" property.** Use tagged terms (become
   ctxtree or debruijn). Defeats the purpose.

None of these make `conv = fast_eq` achievable in general. semantic's
fundamental tradeoff: raw SKI as terms costs structural Val
comparison.

For a working impl, (1) handles `id` at rank 0 and sibling cases; (2)
is open research. (3) is the most promising long-term but requires
redesigning the parser's abstraction step.

## Spec conformance checklist

| spec requirement | semantic implementation |
|---|---|
| `Val = (term, ctx)` canonical | **NO** — overlay ADT instead |
| `apply` eager β, deterministic | yes, with triage-on-neutral caveat |
| `conv` = semantic equality | yes, via structural recursion |
| `fresh_hyp A` opaque, distinct | yes, via fresh_id() |
| `is_H` bare-hypothesis only | yes, NVar-only check |
| H-hypothesis rule in `apply` | yes, conv-check against neutral's type |
| universe canonical per rank | yes, VType k |
| Pi canonical per (A, B) | yes, VPi (d, c) |
| registry closed-over | yes |
| `conv = fast_eq` | **NO** — structural recursion required |

semantic is **non-conformant** to the canonical `Val = (term, ctx)`
shape. It conforms behaviorally (operations produce the semantically-
correct answers) but not structurally. Backends that need structural
Val exchange (e.g., cross-backend serialization) cannot use
semantic's Vals directly.

## What this design gets right

- **Terms are raw SKI.** Same representation check-time and run-time.
  No erase step needed.
- **No context threading.** Neutrals carry their types; the context
  is implicit in live neutrals.
- **Compiled substitution.** Substituting an arg into a body is just
  `apply(t, arg)` — the tree calc's own reduction rules do the work.
- **Conversion under binders is one line.** Mint fresh neutral,
  apply both sides, compare.

## What this design gets wrong

- **Triage-on-neutral.** The defining flaw. I applied to a neutral
  produces a stuck form that isn't structurally equal to the
  neutral. Workarounds are ad-hoc.
- **Bare values have no type.** `infer_v VLeaf` is undefined — a
  leaf could inhabit any type. Inference must always be type-driven
  (check mode) for non-neutral atomic values.
- **Two universes.** Term and Val are structurally distinct. Mitigation:
  both are trees; the "two universes" are two tag families, not two
  fundamentally different things.
- **No η.** Two η-equivalent functions produce different Vals.
- **`conv != fast_eq`.** Structural recursion is required. Precludes
  the O(1) conv property the other backends achieve.

## Known open questions

1. **Triage-on-neutral.** Real blocker for polymorphic checks. Pick
   I-pattern recognition for MVP; invest in approach (3) if semantic
   becomes a primary target.
2. **Type representation at term level.** Surface `(A:Type) → A → A`
   bracket-abstracts to what? Likely need Pi-shaped atom that
   `eval_term` recognizes. Partially breaks the "raw SKI" claim.
3. **`infer_v` for atomic values.** VLeaf has no inherent type. Must
   always be type-driven; infer_v only handles neutrals and type
   formers.
4. **Base-type predicates on neutrals.** `Nat (neutral)` wants to
   return a stuck term, but the canonical Nat predicate would pattern-
   match on the neutral's structure and return FF. Resolve via H-rule
   when neutral's type matches predicate.

## Implementation order

1. Val/Ne constructors as tagged trees (distinct tag family from
   ctxtree's).
2. `apply`, `apply3`, `triage` as mutually-recursive tree programs.
3. `eval_term` — trivial structural lift from raw term to Val.
4. `type_of_ne` — recursive on neutral structure.
5. `conv` + `conv_ne` — structural recursive comparison.
6. Spec primitives wired to ADT cases.
7. I-pattern recognition in `apply` to sidestep triage-on-neutral for
   the simplest case.
8. Export shared contract; document which tests pass, which fail.

## Relationship to the canonical spec

semantic is the **outlier** backend: it does not conform to
`Val = (term, ctx)`, and it does not achieve `conv = fast_eq`. This
is acknowledged and accepted because semantic's different properties
(raw-SKI terms, no erase step, self-typed neutrals) are valuable for
studying alternative NbE strategies.

Spec tests that depend on canonical-form conformance (cross-backend
Val comparison, structural oracle cross-checking) may need semantic-
specific adapters. Tests that only check spec behavior (Pi-checking,
universe-typing, cumulativity) should pass unchanged, modulo the
triage-on-neutral caveat.
