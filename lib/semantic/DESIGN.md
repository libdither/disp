# semantic: Val-domain NbE on raw bracket-abstracted terms

## Purpose

Dybjer/Filinski-style NbE adapted to tree calculus. **No tagged lambdas**
on the term side — terms are the raw bracket-abstracted SKI trees that
the runtime already produces. Binders are absent from terms entirely.
Instead, a parallel Val domain carries type information and neutral
(stuck) placeholders that make symbolic reasoning possible.

This is the design that most cleanly meets the project's runtime goal
(terms at check-time = terms at run-time), at the cost of a second
universe (the Val domain). Unlike `debruijn`, the Val domain here
mirrors tree calculus structure (Leaf/Stem/Fork + Neutral) rather than
lambda calculus structure (VLam/VPi/VNeutral), making `apply_sem`
literally the tree-calc `apply` rules + three stuck points.

## Data

### Terms

```
Term = raw tree (the output of bracket-abstracting the surface lambda form)
```

No wrapper, no tags, no bind-trees. `(λx. x)` bracket-abstracts to `I`;
`(λf x. f x)` bracket-abstracts to some S/K form; `succ zero` is just a
fork tree after bracket abstraction on both sides then application.

The parser already produces these. Nothing new needed on the term side.

### Values

```
Val ::= vLeaf
      | vStem(Val)
      | vFork(Val, Val)
      | vNeutral(Ne)
      | vPi(dom_val, codom_val)             -- codom_val is a tree-level closure
      | vType
      | vAtom(marker)                        -- Nat, Bool, etc.
```

The first three constructors mirror tree calc (leaf/stem/fork). Eval
of a Term is a direct Term→Val translation for these constructors.

`vPi` and `vType` are added for DTT. `vPi`'s codom is itself a Val
(specifically, a closure-like Val that can be `do_app`'d to get the
codomain at a specific argument).

### Neutrals

```
Ne ::= nVar(unique_id, type_val)               -- free var with its type
     | nApp(head_ne, arg_val)                   -- neutral applied to a value
     | nTriage(w_val, x_val, y_val, scrut_ne)  -- stuck triage on a neutral
     | nApp3(stuck_head_ne, y_val, z_val)       -- stuck fork in head position
```

Key: `nVar` **carries its type**. This is the trick that eliminates the
context. When you need "what type does this variable have?", you ask
the neutral directly. No external lookup.

`nTriage` arises when a triage fires on a neutral scrutinee (the three
branch cases all get recorded; which one fires is undetermined).

`nApp3` arises when a fork's left child is itself neutral, so the
apply3 dispatch (K/S/triage) can't select a rule.

## Operations

### Semantic application: `do_app(Val, Val) -> Val`

The five tree-calc reduction rules + three stuck points:

```
do_app vLeaf v               = vStem v                          -- leaf-rule
do_app (vStem a) v           = vFork a v                         -- stem-rule
do_app (vFork a b) v         = do_app3 a b v                    -- fork-rule
do_app (vNeutral n) v        = vNeutral (nApp n v)              -- STUCK (head)
do_app vPi _ _               = error "applied Pi"               -- typing violation
do_app vType _               = error "applied Type"
do_app vAtom _               = error "applied atom"

do_app3 vLeaf y z            = y                                -- K-rule
do_app3 (vStem c) y z        = do_app (do_app c z) (do_app y z) -- S-rule
do_app3 (vFork w x) y z      = triage w x y z                   -- F-rule
do_app3 (vNeutral n) y z     = vNeutral (nApp3 n y z)           -- STUCK (apply3)

triage w x y vLeaf           = w                                -- rule 3a
triage w x y (vStem u)       = do_app x u                       -- rule 3b
triage w x y (vFork u v)     = do_app (do_app y u) v            -- rule 3c
triage w x y (vNeutral n)    = vNeutral (nTriage w x y n)       -- STUCK (triage)
```

### Eval: `eval_term(Term) -> Val`

Just lift the tree structurally:
```
eval_term leaf       = vLeaf
eval_term (stem a)   = vStem (eval_term a)
eval_term (fork a b) = vFork (eval_term a) (eval_term b)
```

No reduction happens during eval. Computation only fires via `do_app`.

### Type of a neutral: `type_of_ne(Ne) -> Val`

```
type_of_ne (nVar _ ty)         = ty
type_of_ne (nApp n v)          = case type_of_ne n of
  vPi _ codom_val -> do_app codom_val v
  _               -> error "applied neutral of non-Pi type"
type_of_ne (nApp3 _ _ _)       = error "stuck apply3 has no simple type"
type_of_ne (nTriage _ _ _ _)   = error "stuck triage has no simple type"
```

The last two cases are **real gaps** in the design. In a well-typed
program, nApp3 and nTriage arise only in very specific places (roughly:
applications of dependent types that themselves case-analyze their
argument). Handling them requires either:
- a type annotation on the neutral at stuck sites, or
- a rule that triage-on-neutral in well-typed position has a knowable
  type from context.

See "Known open questions" below.

### Conversion: `conv(Val, Val) -> Bool`

Structural on Vals. At vPi, introduce a fresh neutral and compare
codomains.

```
conv vLeaf vLeaf               = TT
conv (vStem a) (vStem b)       = conv a b
conv (vFork a1 b1) (vFork a2 b2) = conv a1 a2 && conv b1 b2
conv vType vType               = TT
conv (vAtom m1) (vAtom m2)     = fast_eq m1 m2
conv (vPi d1 c1) (vPi d2 c2)   =
  conv d1 d2 &&
  let x = vNeutral (nVar (fresh ()) d1)
  in conv (do_app c1 x) (do_app c2 x)
conv (vNeutral n1) (vNeutral n2) = conv_ne n1 n2
conv _ _                       = FF

conv_ne (nVar i1 _) (nVar i2 _)     = fast_eq (church i1) (church i2)
conv_ne (nApp n1 v1) (nApp n2 v2)   = conv_ne n1 n2 && conv v1 v2
conv_ne (nApp3 h1 y1 z1) (nApp3 h2 y2 z2) = 
  conv_ne h1 h2 && conv y1 y2 && conv z1 z2
conv_ne (nTriage w1 x1 y1 n1) (nTriage w2 x2 y2 n2) = 
  conv w1 w2 && conv x1 x2 && conv y1 y2 && conv_ne n1 n2
conv_ne _ _                         = FF
```

### Check: `check(Term, Val) -> Bool`

Expected type drives the descent. For vPi, apply both sides to a fresh
neutral; recurse. For non-Pi, infer the term's type and compare via conv.

```
check t ty = check_v (eval_term t) ty

check_v v (vPi dom codom_clos) =
  let x = vNeutral (nVar (fresh ()) dom) in
  let v_applied  = do_app v x in
  let codom_at_x = do_app codom_clos x in
  check_v v_applied codom_at_x

check_v v vType               = is_type_val v    -- v should be vPi, vAtom, vType
check_v v (vAtom m)           = ...              -- base-type case; see below
check_v v (vNeutral _)        = ...              -- shouldn't happen in well-typed
check_v v ty                  = 
  case infer_v v of
    Some ty' -> conv ty ty'
    None     -> FF
```

`infer_v` synthesizes types for Val-level terms:

```
infer_v (vNeutral n)          = type_of_ne n
infer_v vLeaf                 = ???    -- what's the type of leaf?
infer_v (vStem a)             = ???    
infer_v (vFork a b)           = ???
infer_v vType                 = vType
infer_v (vAtom m)             = lookup_atom_type m
infer_v (vPi dom cod)         = vType   -- if well-formed
```

The `???` cases are where tree-calc clashes with DTT: a bare fork has
no natural type. Values at base types only make sense when interpreted
*through* a type predicate. See "Known open questions".

## The triage-on-neutral problem

This is the deepest issue with the design. Consider the simplest test:

```
check id_term (vPi nat_ty (vPi_close A_neut A_neut))
```

where `id_term` bracket-abstracts to `I = fork(fork(leaf, leaf), leaf)`.

At runtime, `apply(I, x) = x` for any concrete tree `x`. But in the
semantic domain, `do_app (vFork (vFork vLeaf vLeaf) vLeaf) (vNeutral n)`
reaches the triage rule with scrutinee = vNeutral, and produces
`vNeutral (nTriage vLeaf vLeaf vLeaf n)`.

This is **not structurally equal** to `vNeutral n`, even though
extensionally (on any concrete input) it would produce the same value.
Conversion check on the body of `I : A → A` compares `x_neut` against
`nTriage vLeaf vLeaf vLeaf x_neut` — fails.

**Candidate fixes:**

1. **η-rule for triage.** Recognize the specific pattern
   `triage(leaf, leaf, leaf, _)` as extensionally identity, and
   simplify at semantic-apply time:
   ```
   do_app (vFork (vFork vLeaf vLeaf) vLeaf) v = v    -- I applied to v = v
   ```
   Requires pattern-matching on the whole I tree shape. Ad-hoc but
   effective for the identity case.

2. **Generalized η for triage-over-equal-branches.** Recognize
   `triage(w, w, w, n) ≡ w ⊕ (something)` where `w ≡ x ≡ y`. This
   is the pattern that identity-in-triage-form has, but it generalizes.
   Hard to get right (extensional equality isn't syntactic).

3. **Bracket-abstract with a smarter algorithm.** Use an abstraction
   scheme that produces neutrally-transparent SKI trees when possible.
   Kiselyov's `tA` / `tE` schemes optimize specific patterns. For
   example, `λx. x` in some schemes produces `I = tI` as a distinct
   primitive that `do_app` handles specially. This changes the term
   rep but preserves the benefit.

4. **Abandon the goal.** Use a lambda-calc semantic domain (VLam/VPi)
   as in `debruijn`, losing the "terms are raw SKI" property. This is
   basically admitting the semantic design only works in lambda
   calculus.

For the initial `impl.disp`, implement approach (1) (I-pattern
recognition) and see how far it gets. Document failures.

## Shared export contract

Same as the other two:

```
def check_id_nat               : TT/FF
def check_const                : TT/FF
def check_id_poly              : TT/FF
def check_apply_id             : TT/FF
def check_refl_zero            : TT/FF
def check_refl_succ_zero_one   : TT/FF
def reject_kstar_shadowed_dep  : TT/FF
def backend_name               : Atom
```

For `check_id_nat`: this is where the triage-on-neutral problem bites
first. Without the η-rule, this test **fails** even though the program
is well-typed. Document in `impl.disp`: which tests pass, which fail,
and why.

## What this design gets right

- **Terms are raw bracket-abstracted SKI.** Same representation at
  check-time and run-time. No erase step, no piPred compilation.
- **No context threading.** Neutrals carry their types; the context
  is implicit in live neutrals' type fields.
- **Conversion under binders is one line.** Mint fresh neutral, apply
  both sides, compare. Classical NbE elegance.
- **Compiled substitution.** Substituting an arg into a body is
  literally `do_app t arg` — the tree calc's own reduction rules do
  the substitution work.

## What this design gets wrong

- **Triage-on-neutral.** Stuck neutrals leak structural complexity
  through operations that should be transparent. Identity isn't
  identity in the semantic domain. Workarounds are ad-hoc.
- **Bare values have no type.** `infer_v vLeaf` is undefined — a leaf
  is an inhabitant of whichever type you want it to inhabit. Need
  type-directed checking (downward), infer only for neutrals.
- **Two universes.** Term and Val are distinct. Violates one-universe
  philosophy. (Mitigation: both are trees; the "two universes" are
  two tag-families, not two fundamentally different things.)
- **η not free.** Two η-equivalent functions produce different Vals.

## Known open questions

1. **Triage-on-neutral**: real blocker. Pick approach (1) for MVP,
   explore (2)-(4) if that's insufficient.
2. **Base-type predicates vs vAtom**: `Nat` as a predicate `λn. triage TT self (K(K FF))` breaks when applied to a neutral. Use `vAtom "Nat"` as an opaque marker at check-time, and only invoke the predicate for closed values. Runtime still uses the predicate.
3. **Atomic value typing**: `vLeaf` has no inherent type. Inference
   must always be *type-driven* (check mode) for non-neutral
   atomic values. `infer_v` only handles neutrals and type formers.
4. **Representing Pi at the term level**: surface `(A:Type) → A → A`
   bracket-abstracts to what? We likely need to keep Pi tagged at
   some level (either on the type side only, or as a marked subtree
   of the raw term). Investigate: can we use the `tPi`-like tag as
   a special atom that `eval_term` recognizes? If so, terms aren't
   fully "raw" — they have Pi-shaped atoms. That's probably fine.

## Implementation order

1. Val/Ne constructors as tagged trees. Pick kind codes distinct from
   those already used in `predicates.disp` (KV/KH/KA/KL/KP/KM).
2. `do_app`, `do_app3`, `triage` as mutually recursive tree programs
   (one `fix` over a triple).
3. `eval_term` — trivial structural lift.
4. `type_of_ne` — recursive on Ne.
5. `conv` + `conv_ne` — recursive pair.
6. `check_v` + `infer_v`.
7. `check = \t ty. check_v (eval_term t) ty`.
8. I-pattern recognition in `do_app` to sidestep triage-on-neutral.
9. Export the shared contract.
10. Run the suite; document which tests pass (likely: poly id,
    const, apply, refl_zero; likely-fails without η extensions:
    anything relying on under-binder triage simplification).
