# debruijn: array-context closure NbE

## Purpose

**Reference oracle.** This is the textbook dependent-type-checker design —
the one everybody knows is correct and fast enough. Purpose within this
project: serve as the "known-good" implementation every other backend is
compared against. If `semantic` or `ctxtree` disagrees with `debruijn` on
any suite entry, the disagreement is a bug in the disagreeing backend.

Not a candidate for the final kernel. Two universes (term syntax + value
domain with closures) — violates the philosophical one-universe goal. But
that's fine for an oracle; correctness over purity.

Design based on Kovács's elaboration-zoo / smalltt. Adapted to disp by
encoding the ML-style algebraic data types as tagged trees and the
host-language closures as bracket-abstracted tree functions.

## Data

### Terms (syntactic; de Bruijn indices)

Terms carry indices, not names. Index `n` refers to the nth binder out
from the current position (innermost = 0).

```
Term ::= tVar(idx)
       | tLam(body)                 no domain annotation (inferred from Pi)
       | tPi(dom, cod)
       | tApp(f, x)
       | tType
       | tAtom(marker)              named atomic constants (Nat, Bool, ...)
```

Encoding: tagged forks, one kind per constructor. Index `idx` is a
church-encoded Nat (or fork-chain depth marker; pick one).

### Values (semantic; de Bruijn levels)

Values use *levels* for free variables (level = "k-th binder from
outside," stable across descent). A bound variable at index `i` in a
context of depth `d` has level `d - i - 1`.

```
Val  ::= vLam(clos)
       | vPi(dom_val, cod_clos)
       | vType
       | vAtom(marker)
       | vNeutral(ne)

Ne   ::= nVar(level)
       | nApp(head_ne, arg_val)

Clos ::= mkClos(env, body)          env = snoc-list of Val
```

`Clos` is a closure: environment + term body. Apply a closure to a Val
by extending the environment and calling `eval`.

### Context

```
Ctx ::= mkCtx(types, env, level)
  types : snoc-list of Val   -- bound-vars' types (lvl -> type)
  env   : snoc-list of Val   -- bound-vars' values (lvl -> neutral/solved)
  level : Nat                -- current depth = length of types/env
```

Contexts are "thick" — every binder contributes both a type and a value
(the neutral it was introduced with). Lookup by level is one list walk;
encode the lists as snoc-forks for O(1) push and O(level) lookup.

## Operations

### Eval: `eval(env, term) -> Val`

Standard closure NbE. Reference a Var by indexing into env. Lam becomes a
closure. App forces head and does `do_app`. Pi/Type eval to themselves.

```
eval env (tVar i)         = env[i]
eval env (tLam body)      = vLam (mkClos env body)
eval env (tPi dom cod)    = vPi (eval env dom) (mkClos env cod)
eval env (tApp f x)       = do_app (eval env f) (eval env x)
eval env tType            = vType
eval env (tAtom m)        = vAtom m

do_app (vLam c)     v = inst c v
do_app (vNeutral n) v = vNeutral (nApp n v)
do_app _            _ = error "applied non-function"

inst (mkClos env body) v = eval (env ++ [v]) body
```

### Quote: `quote(level, val) -> Term`

Read back a Val to a Term, introducing indices at binder crossings.
`level` is the current context depth (used to convert neutral levels
back to indices).

```
quote lvl (vLam c)         = tLam (quote (lvl+1) (inst c (vNeutral (nVar lvl))))
quote lvl (vPi d c)        = tPi (quote lvl d)
                                  (quote (lvl+1) (inst c (vNeutral (nVar lvl))))
quote lvl vType            = tType
quote lvl (vAtom m)        = tAtom m
quote lvl (vNeutral n)     = quote_ne lvl n

quote_ne lvl (nVar k)      = tVar (lvl - k - 1)
quote_ne lvl (nApp n v)    = tApp (quote_ne lvl n) (quote lvl v)
```

### Conversion: `conv(level, val, val) -> Bool`

Compare two Vals structurally. At binders, instantiate both with the same
fresh neutral and recurse. At neutrals, compare heads (same level) then
args pointwise.

```
conv lvl (vLam c1) (vLam c2) =
  let v = vNeutral (nVar lvl)
  in conv (lvl+1) (inst c1 v) (inst c2 v)

conv lvl (vPi d1 c1) (vPi d2 c2) =
  conv lvl d1 d2 &&
  let v = vNeutral (nVar lvl)
  in conv (lvl+1) (inst c1 v) (inst c2 v)

conv lvl vType vType       = TT
conv lvl (vAtom m1) (vAtom m2) = fast_eq m1 m2
conv lvl (vNeutral n1) (vNeutral n2) = conv_ne lvl n1 n2
conv _ _ _                 = FF

conv_ne lvl (nVar k1) (nVar k2) = fast_eq (church k1) (church k2)
conv_ne lvl (nApp h1 v1) (nApp h2 v2) = conv_ne lvl h1 h2 && conv lvl v1 v2
conv_ne _ _ _              = FF
```

### Check and Infer

Bidirectional, on Terms. Ctx provides types for Vars; types of binders
pushed in during descent.

```
check ctx (tLam body) (vPi dom cod) =
  let v = vNeutral (nVar (level ctx))
      ctx' = extend ctx dom v
  in check ctx' body (inst cod v)

check ctx t ty =
  let ty' = infer ctx t
  in conv (level ctx) ty ty'

infer ctx (tVar i)         = types ctx !! i
infer ctx (tApp f x)       = case infer ctx f of
  vPi dom cod -> 
    if check ctx x dom 
    then inst cod (eval (env ctx) x)
    else error
  _ -> error
infer ctx (tPi dom cod)    = 
  if check ctx dom vType && check (extend ctx (eval (env ctx) dom) fresh) cod vType
  then vType
  else error
infer ctx tType            = vType
infer ctx (tAtom m)        = lookup_atom_type m     -- Nat:Type, etc.
```

## Tree encoding sketch

- Indices/levels: fork-chain depth markers (same as current `lvl_start`/`lvl_next`).
- Snoc-lists (env, types): right-folded forks, leaf = nil.
- Closures: `mkClos(env, body) = fork(env, body)` tagged with a closure kind.
- Neutrals: tagged forks with a kind per constructor.
- `do_app` and `eval` are `fix`-recursive tree programs that dispatch on
  kind via `fast_eq (kind v) K_LAM`, etc.
- Closure application `inst` is eval in extended env — two tree programs
  referencing each other; tie the knot with `fix` over a pair.

Expected size: ~400-600 lines of disp. Bigger than bindtree's 750-line
`predicates.disp` in line count, but simpler per-function because no
bind-trees and no ctx-trees — just lists and closures.

## Shared export contract

Every backend's `impl.disp` must define these names (see `lib/suite/main.disp`):

```
def check_id_nat               : TT/FF   -- (λx. x) : Nat → Nat
def check_const                : TT/FF   -- (λx y. x) : A → B → A  (some A, B)
def check_id_poly              : TT/FF   -- (λA x. x) : (A:Type) → A → A
def check_apply_id             : TT/FF   -- (id zero) : Nat
def check_refl_zero            : TT/FF   -- refl : Eq Nat zero zero
def check_refl_succ_zero_one   : TT/FF   -- refl : Eq Nat (succ zero) one
                                         -- (hard: requires def-eq under binders)

def reject_kstar_shadowed_dep  : TT/FF   -- must be FF:
                                         --   (λx y. y) : (x:A) → (y:A) → x

def backend_name               : Atom    -- a unique marker identifying this backend
```

The suite then runs `test check_id_nat = TT` etc. for each backend.

## What this design gets right

- **Separation of syntax/values is crisp.** Makes each operation small
  and easy to review.
- **Closures hide the substitution.** `inst` does one env extension; no
  substitution walk.
- **Decidability is well-understood.** Termination arguments from
  standard DTT literature apply directly.
- **Readback is explicit.** `quote` generates fresh-level neutrals at
  binder crossings; no magic.

## What this design gets wrong (on purpose)

- **Two universes.** Values and Terms are distinct types. Violates the
  one-universe philosophy.
- **Closures are opaque.** Cannot inspect a closure without running it.
  For reflective programming (a project goal), this is a liability.
- **Indices require substitution logic.** Incrementing indices when
  crossing binders is a source of bugs. Classical NbE solves this via
  the index/level split, but it adds complexity.

These are acceptable costs for the oracle role.

## Known open questions

1. **Atomic type dispatch**: `tAtom Nat` should infer to `vType`, but
   `tAtom zero` should infer to `vAtom Nat`. Hard-code a table, or
   carry types on atoms? (Probably the latter, for uniformity.)
2. **Equality type (`Eq`)**: needs to be built-in (with a `refl`
   introduction rule and a dependent pattern-matching eliminator) or
   Church-encoded (with all the attendant conversion issues under
   binders that motivate the suite's hard tests). Pick one and
   document in `impl.disp`.
3. **Size**: will this fit within the 10M-step budget? Closure NbE
   allocates heavily on binder crossings; with budget-conscious
   encoding, probably yes, but benchmark-confirmed only.

## Implementation order

1. Data: term constructors, value constructors, closures, snoc-lists.
2. `eval` + `do_app` + `inst` (mutually recursive, `fix` over a pair).
3. `quote` + `quote_ne`.
4. `conv` + `conv_ne`.
5. `infer` + `check` (bidirectional).
6. Top-level export: the `check_*` test judgments, each constructed by
   building the corresponding Term, Val, running check, and exporting
   the resulting bool.
