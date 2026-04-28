#set document(title: "Disp Type Theory")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#let rulebox(title, body) = block(
  breakable: false,
  above: 1em, below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  {
    text(weight: "bold")[#title]
    v(-0.2em)
    body
  }
)

#align(center, text(22pt, weight: "bold")[Disp Type Theory])
#v(0.3em)
#align(center)[
  The semantics the object language commits to.\
  Companion to #raw("SYNTAX.typ") and #raw("COMPILATION.typ").\
  Reference implementation: #raw("test/disp.disp") (152 tests, tree-calculus programs\
  running through the parser/driver).
]
#v(1em)

= Types as predicates

A *type* is a predicate: applied to a candidate value, it returns
`TT` (accepted) or `FF` (rejected). Checking `v : T` is:

```
  napply(T, v) = TT
```

`napply` is a tree-calculus program --- a single raw tree that the
unmodified runtime executes. It is the *bootstrapping layer* between
the raw tree-calculus runtime and the type system. Types and values
are represented as *Vals* (tagged trees) that `napply` knows how to
evaluate. The raw runtime just runs `napply`; all type-system
intelligence lives inside it.

Why not plain `apply(T, v)`? Because types carry metadata (Pi stores
its domain and codomain; universes store their rank) that
`type_of_neutral` needs for spine inference. This metadata is encoded
as tags inside Vals. Raw `apply` would dispatch on the tag structure
instead of running the predicate body. `napply` handles both: it
recognizes tagged forms and dispatches correctly, and falls through
to raw tree-calculus rules for untagged data.

= `napply` --- the bootstrapping evaluator

`napply` is the single entry point for type checking. It is a
tree-calculus program wired via `wait`-based partial application:

```disp
// Branch: x is neutral → H-rule via ton_check(conv(f), x)
napply_neutral = {conv_fn, ton_check, f, x} ->
    if ton_check (conv_fn f) x then TT
    else dispatch f x

// Branch: x is not neutral → tag dispatch only
napply_raw = {conv_fn, ton_check, f, x} -> dispatch f x

// Core: select branch based on is_neutral(x), then apply shared args
napply_core = {conv_fn, ton_check, f, x} ->
    (if is_neutral x then napply_neutral else napply_raw)
      conv_fn ton_check f x

// Wire dependencies via wait (deferred partial application)
napply = wait (wait (wait napply_core conv) ton_check)
```

`napply_core` is *not* recursive --- it does not reference `self`.
Recursion happens via the raw tree-calculus fallthrough in `dispatch`
and via `type_of_neutral` (which is `fix`-based). Removing `fix` from
napply itself avoids unnecessary self-application overhead.

The wiring via `wait` curries `conv` and `type_of_neutral` as the
first two arguments. At call time, `napply(f, x)` expands to
`napply_core(conv, type_of_neutral, f, x)`.

```disp
dispatch = {f, x} ->
    if is_vhyp f    then VStuck f x
    else if is_vstuck f then VStuck f x
    else if is_vlam f   then vlam_body(f) x
    else f x                                  // raw tree-calculus rules
```

== H-rule (universal, before dispatch)

Before any tag dispatch, `napply` checks: if `x` is a neutral (VHyp
or VStuck) and `conv(f, type_of_neutral(x)) = TT`, return `TT`
directly. This is the mechanism that makes types-as-predicates work
for symbolic values --- including hypotheses used as types.

The H-rule is *universal* --- it fires for every predicate, including
hypotheses used as types. `napply` has no knowledge of specific type
formers.

== Select-then-apply compilation pattern

The branching in `napply_core` uses a pattern critical to correct
tree-calculus compilation. Standard bracket abstraction of
`ited(thunk_A, thunk_B, cond)` where all three share a free variable
produces S-combinators that evaluate *both* thunk bodies before
`ited` can dispatch. This is because `[x](f g) = S([x]f)([x]g)`
evaluates both sides when applied.

The fix: compile each branch as a *closed function* (no free variables
shared with the condition), select via `ite2` (eager but the branches
are constants), then apply shared arguments *after* selection:

```disp
// WRONG: ited forces both branches during bracket abstraction over x
{x} -> ited (thunk expensive_with_x) (thunk cheap_with_x) (cond x)

// RIGHT: select closed function, then apply x
expensive_fn = {x} -> expensive_with_x
cheap_fn     = {x} -> cheap_with_x
{x} -> (if cond x then expensive_fn else cheap_fn) x
```

Since `expensive_fn` and `cheap_fn` are compiled separately (closed
over no shared variables), bracket abstraction does not touch them.
K-composition (`S(Kp)(Kq) → K(pq)`) collapses the constant branches
at compile time.

This pattern is used in `napply_core`, `type_of_neutral`, `Nat`, and
`Type n`. See #raw("KERNEL_DESIGN.md") for the underlying
`ited`/`ite2` encoding.

= Vals

Vals are tagged trees that `napply` operates on.

== Val kinds

#table(
  columns: (auto, 1fr, auto),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Meaning*][*Tag*],
  [`VLam(meta, body)`],
    [Function. `meta` carries type-former info (Pi, universe). `body`
     is a tree-calculus function.],
    [`KV_LAM`],
  [`VHyp(type, id)`],
    [Hypothesis --- an opaque symbolic inhabitant of `type`,
     introduced under a binder. Distinct per binder depth.],
    [`KV_HYP`],
  [`VStuck(head, arg)`],
    [Stuck application --- a hypothesis (or deeper stuck) applied to
     an argument that could not reduce.],
    [`KV_STUCK`],
  [`VStuckElim(motive, target)`],
    [Stuck eliminator --- a case split on a neutral `target` that could
     not dispatch. `motive` determines the return type.],
    [`KV_ELIM`],
  [data: leaf, stem, fork],
    [Untagged tree data. `Zero = leaf`, `Succ n = fork(leaf, n)`.],
    [none],
)

Tags use a stem-chain encoding inside a double-fork:
`tagged(kind, payload) = fork(fork(TAG_ROOT, kind), payload)` where
`TAG_ROOT` is a fixed canonical tree.

Val constructors are tree programs:

```disp
mkVLam  = {meta, body} -> fork (fork TAG_ROOT KV_LAM) (fork meta body)
mkVHyp  = {type, id}   -> fork (fork TAG_ROOT KV_HYP) (fork type id)
mkVStuck = {head, arg}  -> fork (fork TAG_ROOT KV_STUCK) (fork head arg)
```

== Metadata

VLam's `meta` field distinguishes type formers. Reflection functions
(`is_pi`, `pi_dom`, `pi_cod`, `is_universe`, `universe_rank`) inspect
`meta` --- they are library functions, not part of `napply`.

Pi metadata: `fork(PI_TAG, fork(domain, codomain_fn))`. \
Universe metadata: `fork(UNIV_TAG, rank)`. \
Plain lambda / data predicate: `LEAF` (no metadata).

= The evaluator: `napply`

`napply(f, x)` is the complete evaluator. It handles all Val kinds and
the five tree-calculus reduction rules. It is a tree-calculus program.

== H-rule (universal, before dispatch)

Before any tag dispatch, `napply` checks: if `x` is a neutral (VHyp
or VStuck) and `conv(f, type_of_neutral(x)) = TT`, return `TT`
directly. This is the mechanism that makes types-as-predicates work
for symbolic values.

The H-rule is *universal* --- it fires for every predicate, including
hypotheses used as types. `napply` has no knowledge of specific type
formers.

= Auxiliary operations

== `conv(a, b) → Bool`

Structural semantic equality. Two levels:

*Fast path:* `fast_eq` via hash-consing (O(1)). Sufficient when the
elaborator constructs types deterministically (same source → same tree).

*Structural path:* recursive comparison that introduces fresh
hypotheses at Pi binders:

```disp
conv = fix {self, a, b} ->
    if fast_eq a b then TT
    else if is_pi a && is_pi b then
      and (self (pi_dom a) (pi_dom b))
          (let h = fresh_hyp (pi_dom a) depth
           self (napply (pi_cod a) h) (napply (pi_cod b) h))
    else if is_universe a && is_universe b then
      fast_eq (universe_rank a) (universe_rank b)
    else FF
```

Both versions are implemented as tree programs. The fast path is
the default; the structural path is used when types may have been
constructed by different code paths.

== `fresh_hyp(A, depth) → Val`

Creates a `VHyp` with stored type `A` and identity `depth`:

```disp
fresh_hyp = {A, depth} -> mkVHyp A depth
```

This is a pure tree-construction operation --- no mutable state.
Identity comes from *binder depth*, threaded by the elaborator (or
by type constructors that create binders, like Pi). Satisfies:

- *Opacity:* only the H-rule inspects the stored type.
- *Distinctness:* hypotheses at different depths are `conv`-unequal.

The elaborator increments depth at each binder; `fresh_hyp` is
called with the current depth. No global counter is needed.

== `ton_check(check_fn, v) → Bool`

CPS-style spine inference: instead of returning the inferred type (or
an error sentinel), `ton_check` takes a *check function* and calls it
with the inferred type on success, or returns `FF` on failure.

```disp
// Continuation template for VStuck spine:
// extracts pi_cod from head_type, instantiates at arg, passes to check_fn
ton_spine_cont = {check_fn, napply_fn, arg, head_type} ->
    if is_pi head_type then
      check_fn (napply_fn (pi_cod head_type) arg)
    else FF

ton_check = fix {self, napply_fn, check_fn, v} ->
    if is_vhyp v then
      check_fn (vhyp_type v)                       // success: call check_fn
    else if is_vstuck v then
      self napply_fn                                // recurse on head
        (ton_spine_cont check_fn napply_fn (vstuck_arg v))  // continuation
        (vstuck_head v)
    else if is_velim v then
      check_fn (napply_fn (velim_motive v) (velim_target v))  // motive(target)
    else FF                                         // not neutral: fail
```

At each VStuck level, the recursion passes a *continuation* that
processes the head's inferred type: if it's Pi, extract the codomain,
instantiate with `napply_simple`, and pass to the outer `check_fn`. If
not Pi, return `FF`. The recursion bottoms out at VHyp, which calls
`check_fn` directly on the stored type.

Uses `napply_simple` (tag dispatch without H-rule) to instantiate
codomains. This avoids circular dependency: `ton_check` feeds into
`napply`'s H-rule, so it cannot call the full `napply`.

Wired via `wait`: `ton_check = wait(ton_check_core, napply_simple)`.

=== No error sentinel needed

Unlike a design that returns the inferred type (requiring an
`ERROR_VAL` sentinel for failure), the CPS approach short-circuits: if
spine inference fails at any level, `FF` propagates directly.
`check_fn` is simply never called. No sentinel value, no risk of
accidental collision with a real type.

=== H-rule integration

The H-rule in `napply` becomes:

```disp
ton_check (conv f) x
```

Where `conv(f) = fast_eq(f)` is the partially-applied equality check.
If `type_of_neutral` succeeds and the inferred type matches `f`, the
check function returns `TT`. Otherwise `FF`.

= Type constructors

Type constructors are VLam values whose bodies implement the checking
logic. `napply` treats them as ordinary lambdas.

== `Pi` --- dependent function type

`Pi(A, B)` produces a VLam with `PI_TAG` metadata carrying domain `A`
and codomain function `B`:

```disp
// Template for Pi body: partial application builds the closure.
// Normalized: anything not TT becomes FF. Handles the case where
// napply(cod, result) produces a stuck term (e.g. when the codomain
// is an abstract hypothesis type and the term has the wrong type).
pi_body_template = {napply_ref, cod_at_hyp, hyp, f} ->
    fast_eq (napply_ref cod_at_hyp (napply_ref f hyp)) TT

Pi = {domain, codFn, depth} ->
    let hyp = fresh_hyp domain depth
    let cod_at_hyp = napply codFn hyp
    let meta = fork PI_TAG (fork domain codFn)
    let body = pi_body_template napply cod_at_hyp hyp
    mkVLam meta body
```

The body is constructed by *partial application* of `pi_body_template`
to three arguments (`napply`, `cod_at_hyp`, `hyp`), leaving `f` as
the free parameter. Tree-calculus partial application produces the
correctly bracket-abstracted closure.

`{x : A} -> B` in surface syntax elaborates to `Pi A ({x} -> B) depth`.
Non-dependent `A -> B` is:

```disp
Arrow = {A, B, depth} -> Pi A (mkVLam LEAF (K B)) depth
```

Where `K B` is the constant codomain function `{_} -> B`.

== `Type n` --- universe family

`Type n` is a VLam with `UNIV_TAG` metadata. The body handles four
cases, using select-then-apply for each dispatch level:

```disp
Type = {rank} ->
    // Universe rank check as continuation:
    univ_check = {rank, inferred_type} ->
        if is_universe inferred_type then le (universe_rank inferred_type) rank
        else FF
    let body = fix {self, t} ->
      if is_neutral t then
        // Case 1: cumulative neutral (via CPS ton_check)
        ton_check (univ_check rank) t
      else if is_universe t then
        // Case 2: universe below rank
        lt (universe_rank t) rank
      else if is_pi t then
        // Case 3: Pi with components at rank
        and (self (pi_dom t))
            (let h = fresh_hyp (pi_dom t) (stem (stem rank))
             self (napply (pi_cod t) h))
      else if is_registered t then TT   // Case 4: registered base type
      else FF
    mkVLam (fork UNIV_TAG rank) (body rank)
```

Case 1 (cumulative neutral) is handled in the body, not by the
H-rule, because cumulativity requires `rank ≤ n` rather than exact
type match. This is the only predicate with body-level neutral logic.

Cumulativity falls out: every case uses `<` or `≤`, monotone in `n`.

The `depth` for the Pi case uses `stem(stem(rank))` to ensure
distinctness from hypotheses created by Pi construction.

== Data types

Data predicates are VLam with no metadata (`meta = LEAF`). The body
is a `fix`-based recursive function wrapped in `mkVLam` externally
(not inside `fix`) so the result is a properly-tagged VLam:

```disp
Nat = let body = fix {self, n} ->
        if n == leaf then TT                       // Zero
        else if is_tree_fork n then
          if fork_left n == leaf then              // Succ encoding
            napply self (fork_right n)             // recurse
          else FF
        else FF
      mkVLam LEAF body

Bool = mkVLam LEAF {b} ->
    if b == TT then TT
    else if b == FF then TT
    else FF
```

The H-rule in `napply` intercepts neutrals before the body runs, so
data predicates never encounter hypotheses directly.

The pattern *fix outside VLam* is load-bearing: `fix(f)` produces a
`wait`-encoded partial, not a VLam. If the VLam construction were
inside `fix`, tag dispatch would not recognize the result as VLam.

== `Eq` --- propositional equality

`Eq A x y` is a VLam with `EQ_TAG` metadata carrying `(A, x, y)`. The
sole constructor is `refl = LEAF` (the simplest possible value).

```disp
mkEq = {A, x, y} ->
    mkVLam (fork EQ_TAG (fork A (fork x y)))
           ({p} -> if is_neutral p then TT         // H-rule handles it
                   else if p == LEAF then conv x y  // refl: check x ≡ y
                   else FF)                         // reject non-refl data
```

When `p = refl`: the predicate checks `conv(x, y)`. Hash-consing makes
this O(1). When `p` is neutral: the H-rule fires (the stored type of
the hypothesis is `Eq A x y`, which matches the predicate).

=== J eliminator (typed, neutral-aware)

```disp
// J : A → x → motive → base → y → p → motive(y, p)
//   motive : (y : A) → Eq A x y → Type
//   base   : motive(x, refl)
eq_J = {A, x, motive, base, y, p} ->
    if is_neutral p then mkVStuckElim (napply (napply motive y) p) p
    else base                    // p = refl → return base
```

When `p` is neutral, J freezes as `VStuckElim(motive(y, p), p)`. The
motive applied to `y` and `p` gives the return type. `type_of_neutral`
uses this to infer types through J.

Derived operations:
- `eq_subst A P x y p px = eq_J A x ({y,_}->P y) px y p`
- `eq_sym A x y p = eq_J A x ({y,_}->Eq A y x) refl y p`
- `eq_cong A B f x y p = eq_J A x ({y,_}->Eq B (f x) (f y)) refl y p`

=== Typed eliminators --- the general pattern

Raw `triage` on a neutral value produces garbage (the fork structure of
the tagged hypothesis is misinterpreted as data). *Typed eliminators*
solve this:

```disp
// Pattern: check is_neutral BEFORE dispatching, freeze with motive when stuck.
bool_rec = {motive, t_case, f_case, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ite2 t_case f_case target                   // concrete: dispatch

nat_rec = fix {self, motive, base, step, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ... pattern match on zero/succ ...           // concrete: recurse
```

The *motive* is a function from the scrutinee's type to the result type,
supplied at each elimination site (by the user or elaborator). For
non-dependent cases, the motive is constant (`{_}->Nat`).

`VStuckElim(motive, target)` is a neutral form. `type_of_neutral`
handles it by applying the motive: `type = napply(motive, target)`.

This mirrors how Lean/Agda handle stuck case splits: every eliminator
carries a motive, and stuck eliminators freeze as neutral terms whose
types are determined by the motive. The key difference: our eliminators
are ordinary tree programs, not built into the runtime. Any user-defined
type can follow the same pattern.

= Worked examples

== `3 : Nat`

```
napply(Nat, Succ(Succ(Succ(Zero))))
  Nat is VLam → dispatch calls Nat's body with Succ(Succ(Succ(Zero)))
  is_tree_fork? yes. fork_left == leaf? yes → Succ case
  → napply(Nat, Succ(Succ(Zero)))    // recursive via fix
  → napply(Nat, Succ(Zero))
  → napply(Nat, Zero)
  Zero == leaf → TT ✓
```

No `type_of_neutral` or H-rule involved --- this is pure data
checking via Nat's body.

== `{x : Nat} -> x` checked against `Nat -> Nat`

```
napply(Pi(Nat, {_}->Nat, depth=0), {x}->x)
  {x}->x is VLam, not neutral → dispatch calls Pi's body
  Pi body:
    h = fresh_hyp(Nat, 0) = VHyp(Nat, 0)
    result = napply({x}->x, h)
      {x}->x is VLam → body(h) = h
    cod = napply({_}->Nat, h)
      {_}->Nat is VLam → body(h) = Nat
    napply(Nat, h)
      H-rule: is_neutral(h)? yes (VHyp)
      ton_check(conv(Nat), h):
        h is VHyp → conv(Nat)(Nat) = fast_eq → TT ✓
```

The H-rule fires because `h` is a hypothesis whose stored type matches
the predicate. `type_of_neutral` on a bare VHyp is a single-step
extraction.

== `{f:Nat->Nat, x:Nat} -> f x` --- `type_of_neutral` spine inference

Checked against `Pi(Nat->Nat, {_} -> Pi(Nat, {_} -> Nat))`:

```
napply(outer_Pi, term)
  Pi body:
    h_f = fresh_hyp(Nat->Nat, 0) = VHyp(Nat->Nat, 0)
    result = napply(term, h_f)
      term is VLam → body(h_f) = VLam(_, {x} -> napply(h_f, x))
    cod = napply({_}->Pi(Nat,{_}->Nat), h_f) = Pi(Nat, {_}->Nat)
    napply(Pi(Nat, {_}->Nat), inner_term)
      inner_term is VLam, not neutral → Pi body:
        h_x = fresh_hyp(Nat, 1) = VHyp(Nat, 1)
        result = napply(inner_term, h_x)
          inner body(h_x) = napply(h_f, h_x)
            h_f is VHyp (neutral) → dispatch: VStuck(h_f, h_x)
        cod = napply({_}->Nat, h_x) = Nat
        napply(Nat, VStuck(h_f, h_x))
          H-rule: is_neutral? yes (VStuck)
          ton_check(conv(Nat), VStuck(h_f, h_x)):     ← CPS spine inference
            check_fn = conv(Nat) = fast_eq(Nat)
            v = VStuck(h_f, h_x), is_vstuck → recurse on head
            continuation = ton_spine_cont(conv(Nat), napply_simple, h_x)
            ton_check(napply_simple, continuation, h_f):
              h_f is VHyp → continuation(Nat->Nat)     ← base: stored type
              ton_spine_cont:
                is_pi(Nat->Nat)? yes
                pi_cod(Nat->Nat) = {_}->Nat
                napply_simple({_}->Nat, h_x) = Nat     ← instantiate codomain
                check_fn(Nat) = conv(Nat)(Nat) = TT ✓
```

This is the core `type_of_neutral` use case: walking the stuck spine
`VStuck(h_f, h_x)` to discover that `h_f(h_x)` has type `Nat`.
The recursion bottoms out at `h_f` (a bare VHyp), extracts its Pi type,
then uses `napply_simple` (tag dispatch, no H-rule) to instantiate
the codomain at the argument `h_x`.

Note: `type_of_neutral` uses `napply_simple`, not `napply`. This
avoids circularity (type_of_neutral is called *by* napply's H-rule).

== `Nat : Type 0` --- universe checking

```
napply(Type 0, Nat)
  Nat is VLam, not neutral → dispatch calls Type 0's body
  Type body (rank=0):
    is_neutral(Nat)? no
    is_universe(Nat)? no (meta=LEAF, not UNIV_TAG)
    is_pi(Nat)? no (meta=LEAF, not PI_TAG)
    is_registered(Nat)? conv(Nat, Nat) = TT → yes ✓
```

== `Type 0 : Type 1` --- universe stratification

```
napply(Type 1, Type 0)
  Type 0 is VLam, not neutral → dispatch calls Type 1's body
  Type body (rank=1):
    is_neutral(Type 0)? no
    is_universe(Type 0)? yes (meta = fork(UNIV_TAG, 0))
    universe_rank(Type 0) = 0
    lt(0, 1) = TT ✓
```

== `Nat -> Nat : Type 0` --- Pi in universe

```
napply(Type 0, Nat->Nat)
  Nat->Nat is VLam with PI_TAG, not neutral → dispatch calls Type 0's body
  Type body (rank=0):
    is_neutral? no. is_universe? no.
    is_pi(Nat->Nat)? yes
    check domain: napply(Type 0, Nat)
      → is_registered(Nat) → TT ✓
    check codomain:
      h = fresh_hyp(Nat, stem(stem(0)))     // depth distinct from Pi's own hyp
      napply({_}->Nat, h) = Nat             // instantiate codomain
      napply(Type 0, Nat)
        → is_registered(Nat) → TT ✓
```

== Polymorphic identity

`{A:Type 0, x:A} -> x` checked against `Pi(Type 0, {A}->Pi(A, {_}->A))`:

```
napply(outer_Pi, poly_id)
  Pi body:
    h_A = fresh_hyp(Type 0, 0)          // hypothesis: A : Type 0
    cod = napply({A}->Pi(A,{_}->A), h_A)
        = Pi(h_A, {_}->h_A)             // dependent: codomain uses h_A
    result = napply(poly_id, h_A)
           = VLam(_, I)                  // {x} -> x
    napply(Pi(h_A, {_}->h_A), VLam(_, I))
      Pi body:
        h_x = fresh_hyp(h_A, 1)         // hypothesis: x : A (abstract type!)
        result = napply(VLam(_,I), h_x)
               = I(h_x) = h_x           // identity returns h_x
        cod = napply({_}->h_A, h_x)
            = h_A                        // codomain is h_A itself
        napply(h_A, h_x)                 // check: h_x : h_A
          H-rule: is_neutral(h_x)? yes
          ton_check(conv(h_A), h_x):
            h_x is VHyp → conv(h_A)(h_A) = fast_eq → TT ✓
```

The H-rule fires on a *hypothesis used as a type*. `h_A` is both a
term (the codomain type) and the predicate being applied. Since
`type_of_neutral(h_x) = h_A` and `conv(h_A, h_A) = TT`, the check
succeeds. This is how abstract type variables work.

== Rejection: `{A:Type 0, x:A} -> x x` --- `type_of_neutral` error path

```
napply(Pi(Type 0, {A}->Pi(A,{_}->A)), bad_term)
  Pi body:
    h_A = fresh_hyp(Type 0, 0)
    h_x = fresh_hyp(h_A, 1)
    result = napply(bad_term, h_A)       // produces {x}->napply(x,x)
    ...eventually reaches:
    napply({x}->napply(x,x), h_x)
      = napply(h_x, h_x)                // apply h_x to itself
      h_x is VHyp (neutral) → dispatch: VStuck(h_x, h_x)
    napply(h_A, VStuck(h_x, h_x))
      H-rule: is_neutral? yes
      ton_check(conv(h_A), VStuck(h_x, h_x)):
        check_fn = conv(h_A)
        v = VStuck(h_x, h_x), is_vstuck → recurse on head h_x
        ton_check(napply_simple, continuation, h_x):
          h_x is VHyp → continuation(h_A)
          continuation: is_pi(h_A)? h_A is VHyp, not Pi → FF
        → FF propagates directly            ← no sentinel needed
      H-rule does not fire.
    dispatch: h_A is VHyp → VStuck(h_A, VStuck(h_x, h_x))
    Pi body normalization: fast_eq(VStuck, TT) = FF → rejected ✓
```

`type_of_neutral` fails because `h_x : h_A` where `h_A` is an opaque
type variable --- it could be any type, and there is no evidence that
it is a function type. The CPS `ton_check` returns `FF` because `h_A`
is not Pi-typed. The stuck result is caught by Pi body normalization
(`fast_eq(result, TT)`), converting it to an explicit `FF`.

= Soundness

Soundness means: if `napply(T, v) = TT`, then `v` semantically
inhabits the type denoted by `T`.

The argument rests on four invariants:

+ *Freshness.* `fresh_hyp` produces opaque, distinct hypotheses.
  Opacity ensures parametricity (one check = universal
  quantification). Distinctness ensures multi-parameter types don't
  conflate their variables.

+ *Semantic equality.* `conv` agrees with tree-calculus identity on
  closed normal forms. Two evaluations that produce the same value
  produce `conv`-equal Vals (via hash-consing).

+ *Predicate faithfulness.* Each type constructor's predicate returns
  `TT` iff the value inhabits the intended type. `Pi`, `Eq`, `Type n`
  are all defined to match their standard semantics.

+ *Universe well-foundedness.* The `Type n` hierarchy is strictly
  well-founded: no cycles, no self-containing universes. Girard's
  paradox is blocked by stratification.

Budget exhaustion (the evaluation limit) is a *completeness*
concession, not an unsoundness. A check that runs out of budget is
rejected, never falsely accepted.

= Elaboration boundary

The elaborator works in the Val domain. It produces `(term_val,
type_val)` and checks via `napply(type_val, term_val) = TT`. After
checking, the term is *quoted* (bracket-abstracted) to a raw
tree-calculus term for runtime execution. Types are erased.

The parser and surface syntax are I/O layers, not part of the type
system. The elaborator is the boundary: it consumes AST, produces Vals
using the NbE API, and emits raw trees.

The elaborator threads *binder depth* as a parameter. Each Pi binder
increments depth; `fresh_hyp` receives the current depth. No mutable
state crosses the elaboration boundary.

= Implementation status

== What is implemented as tree programs (152 tests)

All of the following run as `.disp` source through the parser/driver,
validated by `test/disp.disp`:

- `napply` with H-rule, tag dispatch, raw tree-calculus fallthrough
- `napply_simple` (tag dispatch without H-rule, used by `type_of_neutral`)
- `ton_check` (CPS spine inference, `fix`-recursive, handles VStuckElim)
- `conv` (fast\_eq) and `conv_structural` (recursive Pi/Universe comparison)
- `fresh_hyp` / `mkVHyp`, `mkVLam`, `mkVStuck`, `mkVStuckElim` (Val constructors)
- `Pi` construction (`mkPi_prog`, `mkArrow_prog`) via `pi_body_template`
- `Type n` universe predicate (all four cases, `fix`-recursive)
- `Nat` predicate (`fix`-recursive, pattern matching)
- `Bool` predicate
- `Eq` type (`mkEq`, `refl`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`)
- Typed eliminators: `bool_rec`, `nat_rec` (neutral-aware, motive-carrying)
- `nat_le`, `nat_lt` (Nat comparison for universe ranks)
- `add` (recursive via select-then-apply + `fix`)
- Eq proofs on concrete arithmetic (commutativity of add)
- Tag infrastructure: `is_tagged`, `is_vhyp`/`is_vlam`/`is_vstuck`/`is_velim`,
  `is_neutral`, `vlam_body`/`vlam_meta`, metadata reflection
- `wait`, `fix`, `ite2`, `ited`
- Integration: polymorphic identity, polymorphic const (+ rejection),
  self-application rejection, flip, composition, Church pairs,
  higher-order functions with spine inference

== What remains TypeScript-only

- The elaborator (AST walking, depth threading, motive inference, quoting)
- Error messages and diagnostics
- The parser / bracket abstraction / driver

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`Val`],         [A tagged tree in the NbE domain. One of: VLam, VHyp, VStuck, or untagged data.],
  [`napply(f, x)`],[The evaluator. H-rule + tag dispatch + tree-calculus rules.],
  [`conv(a, b)`],  [Semantic equality on Vals. Fast path: `fast_eq`. Structural: recursive with Pi/Universe handling.],
  [`fresh_hyp(A, depth)`],[Create a hypothesis: `mkVHyp(A, depth)`. Pure data construction.],
  [`ton_check(check_fn, v)`],[CPS spine inference: calls `check_fn(inferred_type)` on success, returns `FF` on failure. Replaces `type_of_neutral`.],
  [`is_pi`, `pi_dom`, `pi_cod`],[Reflection on VLam metadata. Library functions.],
  [`is_universe`, `universe_rank`],[Reflection on universe metadata.],
  [`is_neutral`],  [`TT` iff `v` is VHyp, VStuck, or VStuckElim.],
  [`VStuckElim(motive, target)`],[Stuck eliminator. Produced by typed eliminators when the scrutinee is neutral. `type_of_neutral` applies motive to target.],
  [`TT` / `FF`],   [Encoded booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`H-rule`],      [In `napply`: if `x` is neutral and `conv(f, type_of_neutral(x)) = TT`, return `TT`. Universal, not type-specific.],
  [`wait`],        [Deferred application: `wait a b c = a(b)(c)` but `wait(a)(b)` does not evaluate `a(b)`. See #raw("KERNEL_DESIGN.md").],
  [`fix`],         [Fixed-point via `wait`. Demand-driven recursion. See #raw("KERNEL_DESIGN.md").],
  [`TAG_ROOT`],    [Canonical tree prefix distinguishing tagged Vals from data.],
  [`PI_TAG` / `UNIV_TAG` / `EQ_TAG`],[Metadata tags in VLam's `meta` field.],
  [*typed eliminator*],[A neutral-aware recursor (e.g.~`bool_rec`, `nat_rec`, `eq_J`). Checks `is_neutral` before triaging; produces `VStuckElim` when stuck. Carries a motive for type inference.],
  [*select-then-apply*],[Compilation pattern: closed branch functions selected by `ite2` before shared args applied. See §2.2.],
  [*quote*],       [Convert a closed Val to a raw tree-calculus term (bracket abstraction). One-way, lossy (metadata stripped).],
  [*registry*],    [Ambient set of rank-0 base types recognized by `Type n`.],
)

= Open problems

== `apply(T, v) = TT` without `napply`

The current design requires `napply(T, v) = TT` because types are
VLam (tagged) and raw `apply` dispatches on the tag structure rather
than running the predicate body. A future design might compile type
predicates to raw bracket-abstracted combinators (with `napply` logic
inlined into each predicate's body), eliminating the `napply` entry
point. This would require either:
- inlining the H-rule and tag dispatch into every predicate, or
- a compilation strategy that makes VLam reduce correctly under raw
  `apply` (e.g., encoding VLam as a combinator whose tree-calc
  reduction matches `napply`'s dispatch).

The tests in #raw("test/disp.disp") validate that the raw/tagged
boundary is transparent to `napply`. The step from `napply(T, v)` to `apply(T, v)` is an
encoding question, not a design question.

== B/C combinators

The lambada project's bracket abstraction uses Turner's B and C
combinators in addition to S, K, I:
- `B f g x = f (g x)` --- composition, avoids evaluating `f` with `x`
- `C f g x = f x g` --- flip, avoids evaluating `g` with `x`

These reduce the work done by the S combinator when only one side of
an application depends on the abstracted variable. The current
implementation uses only S/K/I with η-reduction and K-composition,
which achieves correct results but may produce larger intermediate
trees. Adding B/C would be a compilation improvement, not a semantic
change.

== Elaborator as tree program

The elaborator (AST → Val, depth threading, `napply` calls, quoting)
is the remaining frontier for self-hosting. It threads binder depth as
a Nat parameter through recursive calls --- standard functional
programming with no mutation. The core loop is: pattern-match on AST
nodes, construct Vals, call `napply`, recur. All the NbE operations it
calls are already tree programs.
