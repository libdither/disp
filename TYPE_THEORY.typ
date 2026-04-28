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
  Reference implementation: #raw("lib/*.disp") (136 tests across 3 files,\
  tree-calculus programs running through the parser/driver).
]
#v(1em)

= Types as predicates

A *type* is a predicate: applied to a candidate value, it returns
`TT` (accepted) or `FF` (rejected). Checking `v : T` is:

```
  apply(T, v) = TT
```

`apply` is the raw tree-calculus evaluator. Types are tree-calculus
programs that, when applied to a value via the unmodified runtime,
produce `TT` or `FF`. No special evaluator sits between the runtime
and the type system --- types _are_ functions.

Each type is constructed as `wait(checker)(metadata)`, where:
- `checker` is a `fix`-recursive function implementing the predicate
  logic with the H-rule inlined.
- `metadata` carries type-former info (domain/codomain for Pi,
  rank for universes, etc.) tagged with a type-former identifier.
- `wait` holds `checker` and `metadata` in an inert tree structure
  that, when applied to `v`, evaluates `checker(metadata)(v)`.

The `wait` encoding is critical: it places `metadata` in a
K-position (extractable via triage) while ensuring that `apply(T, v)`
dispatches correctly to the checker. Tree calculus's programs-are-data
property means the metadata is inspectable --- there is no
lambda-calculus-style opacity.

= Wait-based type encoding

== Tree structure

`wait(f)(x)` produces a normal-form tree:

```
  fork(stem(X), fork(LEAF, x))
```

where `X` depends on `f` but not on `x`. When applied to `v`:

```
  apply(wait(f)(x), v) = f(x)(v)
```

For a type `T = wait(checker)(metadata)`:

```
  apply(T, v) = checker(metadata)(v)
```

The tree structure exposes two extractable components:

- *Signature:* `pair_fst(T) = stem(X)` --- determined entirely by
  `checker`. All types sharing the same checker have the same
  signature. (Not currently used for recognition; metadata tags are
  used instead.)

- *Metadata:* `pair_snd(pair_snd(T)) = metadata` --- the
  type-former-specific payload, always `fork(TAG, payload)`.

== Metadata tags

Each type former tags its metadata with an identifier:

Pi metadata: `fork(PI_TAG, fork(domain, fork(depth, codFn)))`. \
Universe metadata: `fork(UNIV_TAG, rank)`. \
Eq metadata: `fork(EQ_TAG, fork(A, fork(x, y)))`. \
Data types (Nat, Bool): `LEAF` (no metadata needed).

Recognition inspects the metadata tag:

```disp
type_meta = {T} -> pair_snd (pair_snd T)
is_pi = {v} -> fast_eq (pair_fst (type_meta v)) PI_TAG
pi_dom = {v} -> pair_fst (pair_snd (type_meta v))
pi_cod_fn = {v} -> pair_snd (pair_snd (pair_snd (type_meta v)))
```

This avoids circular dependency: `ton_check` uses `is_pi`/`pi_cod_fn`
to walk spines, and these depend only on metadata structure, not on
any checker or signature.

== Checker template

Every type former's checker follows this pattern:

```disp
some_checker = fix {self, meta, v} ->
    if is_neutral(v) then
      ton_check (fast_eq (wait self meta)) v    // H-rule
    else
      <type-specific predicate logic>
```

The H-rule is inlined: the checker reconstructs its own type via
`wait(self)(meta)` and compares it against the inferred type of the
neutral. `fix` provides the self-reference; `wait(self)(meta)`
produces the same hash-consed tree as the original type (since `self`
inside fix equals the fully recursive checker).

The `is_neutral` / else branches use select-then-apply (closed
branch functions selected by `ite2`, shared args applied after) to
avoid bracket-abstraction forcing both branches. See §2.2 of the
previous version and #raw("KERNEL_DESIGN.md").

= Neutrals

Neutrals are tagged trees representing symbolic values that cannot
reduce further. They are the _only_ tagged values in the system ---
types and functions are untagged.

== Neutral kinds

#table(
  columns: (auto, 1fr, auto),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Meaning*][*Tag*],
  [`VHyp(type, id)`],
    [Hypothesis --- an opaque symbolic inhabitant of `type`,
     introduced under a binder. Distinct per binder depth.],
    [`KV_HYP`],
  [`VStuck(head, arg)`],
    [Stuck application --- a neutral applied to an argument that
     could not reduce.],
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

Neutral constructors are tree programs:

```disp
mkVHyp  = {type, id}   -> fork (fork TAG_ROOT KV_HYP) (fork type id)
mkVStuck = {head, arg}  -> fork (fork TAG_ROOT KV_STUCK) (fork head arg)
mkVStuckElim = {motive, target} -> fork (fork TAG_ROOT KV_ELIM) (fork motive target)
```

= Application operations

Two application primitives handle the cases where raw `apply` is
insufficient.

== `val_apply(f, x)` --- neutral-aware function application

When `f` is a neutral (VHyp, VStuck, VStuckElim), raw `apply` would
trigger triage on the tag structure, producing garbage. `val_apply`
intercepts this:

```disp
val_apply = {f, x} ->
    if is_neutral f then mkVStuck f x
    else f x                              // raw tree-calculus apply
```

Used inside term bodies (composition, application through hypotheses)
where the function might be a symbolic variable.

== `type_apply(T, v)` --- type checking with abstract types

When `T` is a wait-based type, raw `apply(T, v)` works --- the
checker handles the H-rule internally. But when `T` is a neutral
(a hypothesis used as a type, e.g. `A : Type 0`), raw `apply`
produces garbage.

```disp
type_apply = {T, v} ->
    if is_neutral T then ton_check (fast_eq T) v
    else T v                              // raw apply: wait-based type
```

The neutral case uses `ton_check` to infer `v`'s type and compare
it against `T` via `fast_eq`. This is the H-rule, applied externally
rather than being inlined into a checker (since neutrals have no
checker).

`type_apply` is used inside the Pi checker for the codomain check,
because the codomain type might be an abstract type variable.

= Auxiliary operations

== `conv(a, b) → Bool`

Structural semantic equality. Two levels:

*Fast path:* `fast_eq` via hash-consing (O(1)). Sufficient when the
elaborator constructs types deterministically (same source → same tree).

*Structural path:* recursive comparison that introduces fresh
hypotheses at Pi binders:

```disp
conv_structural = fix {self, a, b} ->
    if fast_eq a b then TT
    else if is_pi a && is_pi b then
      and (self (pi_dom a) (pi_dom b))
          (self (pi_cod_fn a hyp) (pi_cod_fn b hyp))
              // where hyp = fresh_hyp(pi_dom(a), depth)
    else if is_universe a && is_universe b then
      fast_eq (universe_rank a) (universe_rank b)
    else FF
```

Both versions are implemented as tree programs. The fast path is
the default; the structural path is used when types may have been
constructed by different code paths.

== `fresh_hyp(A, depth) → Neutral`

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
ton_spine_cont = {check_fn, arg, head_type} ->
    if is_pi head_type then
      check_fn (pi_cod_fn(head_type)(arg))   // raw apply: codFn is a raw function
    else FF

ton_check = fix {self, check_fn, v} ->
    if is_vhyp v then
      check_fn (vhyp_type v)                 // base case: stored type
    else if is_vstuck v then
      self                                    // recurse on head
        (ton_spine_cont check_fn (vstuck_arg v))
        (vstuck_head v)
    else if is_velim v then
      check_fn (velim_motive(v)(velim_target(v)))  // motive applied to target
    else FF
```

At each VStuck level, the recursion passes a *continuation* that
processes the head's inferred type: if it's Pi, extract the codomain
function, apply it to the argument via raw `apply`, and pass to the
outer `check_fn`. If not Pi, return `FF`. The recursion bottoms out
at VHyp, which calls `check_fn` directly on the stored type.

Codomain instantiation uses raw `apply` --- `pi_cod_fn(T)` extracts
a raw function from Pi's metadata, and applying it to the argument
just performs tree-calculus substitution. No special evaluator needed.

=== No error sentinel needed

Unlike a design that returns the inferred type (requiring an
`ERROR_VAL` sentinel for failure), the CPS approach short-circuits: if
spine inference fails at any level, `FF` propagates directly.
`check_fn` is simply never called. No sentinel value, no risk of
accidental collision with a real type.

=== H-rule integration

The H-rule inside each checker becomes:

```disp
ton_check (fast_eq (wait self meta)) v
```

Where `wait(self)(meta)` reconstructs the checker's own type, and
`fast_eq` is the partially-applied equality check. If
`type_of_neutral` succeeds and the inferred type matches, the check
function returns `TT`. Otherwise `FF`.

For `type_apply` (external H-rule for neutrals-as-types):

```disp
ton_check (fast_eq T) v
```

Same mechanism, but `T` is provided from outside rather than
reconstructed via `wait(self)(meta)`.

= Type constructors

Type constructors are `wait(checker)(metadata)` values. The checker
implements the predicate logic with the H-rule inlined via `fix`.
Raw `apply(T, v)` dispatches directly to the checker.

== `Pi` --- dependent function type

`Pi(A, B, depth)` constructs a type that checks functions. The
metadata carries domain `A`, binder depth, and codomain function `B`:

```disp
// Checker: shared by ALL Pi types. H-rule + codomain check.
pi_checker = fix {self, meta, v} ->
    if is_neutral v then
      ton_check (fast_eq (wait self meta)) v       // H-rule
    else
      // Extract from meta = fork(PI_TAG, fork(dom, fork(depth, codFn)))
      let dom = pi_dom_of(meta)
      let depth = pi_depth_of(meta)
      let codFn = pi_cod_fn_of(meta)
      let hyp = fresh_hyp(dom, depth)
      let cod_at_hyp = codFn(hyp)              // raw apply: codFn is a raw function
      let result = v(hyp)                      // raw apply: v is non-neutral
      fast_eq(type_apply(cod_at_hyp, result), TT)

Pi = {domain, codFn, depth} ->
    wait pi_checker (fork PI_TAG (fork domain (fork depth codFn)))
```

The codomain check uses `type_apply` rather than raw `apply` because
`cod_at_hyp` might be a neutral (an abstract type variable). When
the codomain is a concrete wait-based type, `type_apply` falls
through to raw `apply`. When it's a hypothesis, the external H-rule
fires via `ton_check`.

`{x : A} -> B` in surface syntax elaborates to `Pi A ({x} -> B) depth`.
Non-dependent `A -> B` is:

```disp
Arrow = {A, B, depth} -> Pi A ({_} -> B) depth
```

Where `{_} -> B` is a raw bracket-abstracted constant function (K(B)).

== `Type n` --- universe family

`Type n` checks whether a value is a valid type at universe level `n`.
The checker handles four cases:

```disp
type_checker = fix {self, meta, v} ->
    let rank = univ_rank_of(meta)       // meta = fork(UNIV_TAG, rank)
    if is_neutral v then
      // Case 1: cumulative neutral (≤, not exact match)
      ton_check (univ_check rank) v
    else if is_universe v then
      // Case 2: universe below rank (strict <)
      lt (universe_rank v) rank
    else if is_pi v then
      // Case 3: Pi with components at rank
      and (wait self meta (pi_dom v))
          (wait self meta (pi_cod_fn(v)(fresh_hyp(pi_dom(v), stem(stem(rank))))))
    else if is_registered v then TT      // Case 4: registered base type
    else FF

Type = {rank} -> wait type_checker (fork UNIV_TAG rank)
```

Case 1 (cumulative neutral) uses `≤` rather than exact type match.
This is the only predicate with body-level neutral logic --- it
cannot rely solely on the H-rule because cumulativity requires a
range check, not equality.

The Pi case recursively checks domain and codomain using
`wait(self)(meta)` to reconstruct the Type predicate. The `depth`
for the codomain hyp uses `stem(stem(rank))` to ensure distinctness.

Cumulativity falls out: every case uses `<` or `≤`, monotone in `n`.

== Data types

Data predicates use `wait(checker)(LEAF)` --- no metadata needed.
The checker is a `fix`-recursive function with the H-rule inlined:

```disp
nat_checker = fix {self, meta, n} ->
    if is_neutral n then
      ton_check (fast_eq (wait self meta)) n     // H-rule
    else if n == leaf then TT                     // Zero
    else if is_tree_fork n then
      if fork_left(n) == leaf then               // Succ encoding
        wait self meta (fork_right n)            // recurse
      else FF
    else FF

Nat = wait nat_checker LEAF

bool_checker = fix {self, meta, b} ->
    if is_neutral b then
      ton_check (fast_eq (wait self meta)) b     // H-rule
    else if b == TT then TT
    else if b == FF then TT
    else FF

Bool = wait bool_checker LEAF
```

The H-rule intercepts neutrals before the body runs, so data
checkers never encounter hypotheses directly.

== `Eq` --- propositional equality

`Eq A x y` checks that a proof witness is `refl` (when `x ≡ y`) or
a neutral (when the proof is symbolic).

```disp
eq_checker = fix {self, meta, p} ->
    // meta = fork(EQ_TAG, fork(A, fork(x, y)))
    if is_neutral p then
      ton_check (fast_eq (wait self meta)) p     // H-rule
    else if p == LEAF then conv x y              // refl: check x ≡ y
    else FF

mkEq = {A, x, y} -> wait eq_checker (fork EQ_TAG (fork A (fork x y)))
```

When `p = refl`: the checker checks `conv(x, y)`. Hash-consing makes
this O(1). When `p` is neutral: the H-rule fires (the stored type of
the hypothesis is `Eq A x y`, which matches the predicate).

=== J eliminator (typed, neutral-aware)

```disp
eq_J = {A, x, motive, base, y, p} ->
    if is_neutral p then mkVStuckElim (motive(y)(p)) p
    else base                        // p = refl → return base
```

When `p` is neutral, J freezes as `VStuckElim(motive(y, p), p)`. The
motive applied to `y` and `p` gives the return type. `ton_check`
handles `VStuckElim` by applying the motive to the target.

Derived operations:
- `eq_subst A P x y p px = eq_J A x ({y,_}->P y) px y p`
- `eq_sym A x y p = eq_J A x ({y,_}->Eq A y x) refl y p`
- `eq_cong A B f x y p = eq_J A x ({y,_}->Eq B (f x) (f y)) refl y p`

=== Typed eliminators --- the general pattern

Raw `triage` on a neutral value produces garbage (the fork structure of
the tagged hypothesis is misinterpreted as data). *Typed eliminators*
solve this:

```disp
bool_rec = {motive, t_case, f_case, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ite2 t_case f_case target

nat_rec = fix {self, motive, base, step, target} ->
    if is_neutral target then mkVStuckElim motive target
    else ... pattern match on zero/succ ...
```

The *motive* is a raw function from the scrutinee to the result type,
supplied at each elimination site (by the user or elaborator). For
non-dependent cases, the motive is constant (`{_}->Nat`).

`VStuckElim(motive, target)` is a neutral form. `ton_check`
handles it by applying the motive: `type = motive(target)`.

This mirrors how Lean/Agda handle stuck case splits: every eliminator
carries a motive, and stuck eliminators freeze as neutral terms whose
types are determined by the motive. The key difference: our eliminators
are ordinary tree programs, not built into the runtime. Any user-defined
type can follow the same pattern.

= Worked examples

== `3 : Nat`

```
apply(Nat, Succ(Succ(Succ(Zero))))
  Nat = wait(nat_checker)(LEAF)
  → nat_checker(LEAF)(Succ(Succ(Succ(Zero))))
  is_neutral? no
  is_tree_fork? yes. fork_left == leaf? yes → Succ case
  → wait(nat_checker)(LEAF)(Succ(Succ(Zero)))      // recurse via wait(self)(meta)
  → wait(nat_checker)(LEAF)(Succ(Zero))
  → wait(nat_checker)(LEAF)(Zero)
  Zero == leaf → TT ✓
```

No H-rule involved --- this is pure data checking via nat_checker's
body.

== `{x : Nat} -> x` checked against `Nat -> Nat`

```
apply(Arrow(Nat, Nat, 0), {x}->x)
  Arrow(Nat, Nat, 0) = wait(pi_checker)(fork(PI_TAG, fork(Nat, fork(0, K(Nat)))))
  → pi_checker(meta)({x}->x)
  is_neutral({x}->x)? no (it's a raw combinator)
  Pi body:
    dom = Nat, depth = 0, codFn = K(Nat)
    hyp = mkVHyp(Nat, 0) = VHyp(Nat, 0)
    cod_at_hyp = K(Nat)(hyp) = Nat          // raw apply on constant function
    result = ({x}->x)(hyp) = hyp            // raw apply: substitution
    type_apply(Nat, hyp):
      Nat is not neutral → apply(Nat, hyp)
      → nat_checker(LEAF)(hyp)
      is_neutral(hyp)? yes (VHyp)
      H-rule: ton_check(fast_eq(wait(nat_checker)(LEAF)), hyp)
        hyp is VHyp → fast_eq(Nat)(Nat) = TT ✓
```

The H-rule fires because `hyp` is a hypothesis whose stored type (Nat)
matches the predicate. Raw `apply(Nat, hyp)` dispatches to
nat_checker, which handles the neutral internally.

== `{f:Nat->Nat, x:Nat} -> f x` --- spine inference

Checked against `Pi(Nat->Nat, {_} -> Pi(Nat, {_} -> Nat))`:

```
apply(outer_Pi, term)
  → pi_checker(outer_meta)(term)
  term is not neutral → Pi body:
    h_f = VHyp(Nat->Nat, 0)
    result = term(h_f) = {x} -> val_apply(h_f, x)
    cod = K(Pi(Nat,{_}->Nat))(h_f) = Pi(Nat, {_}->Nat)
    type_apply(Pi(Nat,{_}->Nat), result):
      Pi is not neutral → apply(Pi(Nat,{_}->Nat), result)
      → pi_checker(inner_meta)(result)
      result is not neutral → Pi body:
        h_x = VHyp(Nat, 1)
        result2 = val_apply(h_f, h_x)
          h_f is neutral → VStuck(h_f, h_x)
        cod2 = K(Nat)(h_x) = Nat
        type_apply(Nat, VStuck(h_f, h_x)):
          Nat is not neutral → apply(Nat, VStuck(h_f, h_x))
          → nat_checker(LEAF)(VStuck(h_f, h_x))
          is_neutral? yes
          H-rule: ton_check(fast_eq(Nat), VStuck(h_f, h_x)):
            VStuck → recurse on head h_f
            continuation = ton_spine_cont(fast_eq(Nat), h_x)
            ton_check(continuation, h_f):
              h_f is VHyp → continuation(Nat->Nat)
              is_pi(Nat->Nat)? yes
              pi_cod_fn(Nat->Nat)(h_x) = K(Nat)(h_x) = Nat
              fast_eq(Nat)(Nat) = TT ✓
```

This is the core spine inference use case: walking the stuck spine
`VStuck(h_f, h_x)` to discover that `h_f(h_x)` has type `Nat`.
Codomain instantiation uses raw `apply` on the codFn (a raw function
extracted from Pi's metadata).

== `Nat : Type 0`

```
apply(Type 0, Nat)
  → type_checker(fork(UNIV_TAG, 0))(Nat)
  rank = 0
  is_neutral(Nat)? no (wait-based type)
  is_universe(Nat)? type_meta(Nat) = LEAF, pair_fst(LEAF) ≠ UNIV_TAG → no
  is_pi(Nat)? pair_fst(LEAF) ≠ PI_TAG → no
  is_registered(Nat)? fast_eq(Nat, Nat) = TT → yes ✓
```

== `Type 0 : Type 1`

```
apply(Type 1, Type 0)
  → type_checker(fork(UNIV_TAG, 1))(Type 0)
  rank = 1
  is_neutral(Type 0)? no
  is_universe(Type 0)? type_meta(Type 0) = fork(UNIV_TAG, 0),
    pair_fst = UNIV_TAG → yes
  universe_rank(Type 0) = 0
  lt(0, 1) = TT ✓
```

== Polymorphic identity

`{A:Type 0, x:A} -> x` checked against `Pi(Type 0, {A}->Pi(A, {_}->A))`:

```
apply(outer_Pi, poly_id)
  → pi_checker(outer_meta)({A}->{x}->x)
  not neutral → Pi body:
    h_A = VHyp(Type 0, 0)           // hypothesis: A : Type 0
    codFn = {A}->Pi(A,{_}->A)
    cod = codFn(h_A) = Pi(h_A, {_}->h_A)   // dependent: uses h_A
    result = ({A}->{x}->x)(h_A) = {x}->x   // raw apply
    type_apply(Pi(h_A, {_}->h_A), {x}->x):
      Pi is not neutral → apply(Pi(h_A,{_}->h_A), {x}->x)
      → pi_checker(inner_meta)({x}->x)
      not neutral → Pi body:
        h_x = VHyp(h_A, 1)          // hypothesis: x : A (abstract!)
        codFn2 = {_}->h_A
        cod2 = codFn2(h_x) = h_A    // codomain is h_A itself
        result2 = ({x}->x)(h_x) = h_x
        type_apply(h_A, h_x):
          h_A IS neutral (VHyp) →
          ton_check(fast_eq(h_A), h_x):
            h_x is VHyp → fast_eq(h_A)(h_A) = TT ✓
```

The critical step: `type_apply(h_A, h_x)` recognizes that `h_A` is a
neutral (hypothesis used as a type) and routes through `ton_check`
for the external H-rule. `h_A` is both the codomain type and the
predicate --- since `vhyp_type(h_x) = h_A` and `fast_eq(h_A, h_A)`
succeeds, the check passes. This is how abstract type variables work.

== Rejection: `{A:Type 0, x:A} -> x x`

```
apply(Pi(Type 0, {A}->Pi(A,{_}->A)), bad_term)
  → pi_checker(outer_meta)(bad_term)
  ...eventually reaches:
  type_apply(h_A, VStuck(h_x, h_x)):
    h_A is neutral →
    ton_check(fast_eq(h_A), VStuck(h_x, h_x)):
      VStuck → recurse on head h_x
      ton_check(continuation, h_x):
        h_x is VHyp → continuation(h_A)
        is_pi(h_A)? h_A is VHyp, metadata inspection:
          type_meta(h_A) → tag_payload of VHyp → fork(Type0, 0)
          pair_fst(fork(Type0, 0)) = Type0 ≠ PI_TAG → not Pi → FF
      → FF propagates ✓
  fast_eq(FF, TT) = FF → rejected ✓
```

`ton_check` fails because `h_x : h_A` where `h_A` is an opaque
type variable --- there is no evidence it is a function type.

= Soundness

Soundness means: if `apply(T, v) = TT`, then `v` semantically
inhabits the type denoted by `T`.

The argument rests on four invariants:

+ *Freshness.* `fresh_hyp` produces opaque, distinct hypotheses.
  Opacity ensures parametricity (one check = universal
  quantification). Distinctness ensures multi-parameter types don't
  conflate their variables.

+ *Semantic equality.* `conv` agrees with tree-calculus identity on
  closed normal forms. Two evaluations that produce the same value
  produce `conv`-equal trees (via hash-consing).

+ *Predicate faithfulness.* Each type constructor's checker returns
  `TT` iff the value inhabits the intended type. `Pi`, `Eq`, `Type n`
  are all defined to match their standard semantics.

+ *Universe well-foundedness.* The `Type n` hierarchy is strictly
  well-founded: no cycles, no self-containing universes. Girard's
  paradox is blocked by stratification.

Budget exhaustion (the evaluation limit) is a *completeness*
concession, not an unsoundness. A check that runs out of budget is
rejected, never falsely accepted.

= Elaboration boundary

The elaborator works in the type domain. It produces `(term, type)`
and checks via `apply(type, term) = TT`. After checking, the term is
*quoted* (bracket-abstracted) to a raw tree-calculus term for runtime
execution. Types are erased.

The parser and surface syntax are I/O layers, not part of the type
system. The elaborator is the boundary: it consumes AST, constructs
types using the wait-based constructors, checks terms via raw `apply`,
and emits raw trees.

The elaborator threads *binder depth* as a parameter. Each Pi binder
increments depth; `fresh_hyp` receives the current depth. No mutable
state crosses the elaboration boundary.

= Implementation status

== What is implemented as tree programs (136 tests)

All of the following run as `.disp` source through the parser/driver,
validated by `lib/*.test.disp`:

- Wait-based type checkers: Pi, Nat, Bool, Eq, Type n --- each with
  H-rule inlined via `fix`
- `val_apply` (neutral-aware function application)
- `type_apply` (type checking with abstract type support)
- `ton_check` (CPS spine inference, `fix`-recursive, handles VStuckElim)
- `conv` (fast\_eq) and `conv_structural` (recursive Pi/Universe comparison)
- `fresh_hyp` / `mkVHyp`, `mkVStuck`, `mkVStuckElim` (neutral constructors)
- `Pi` construction (`mkPi`, `mkArrow`) via `wait(pi_checker)(metadata)`
- `Type n` universe predicate (all four cases, `fix`-recursive)
- `Nat` predicate (`fix`-recursive, pattern matching)
- `Bool` predicate
- `Eq` type (`mkEq`, `refl`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`)
- Typed eliminators: `bool_rec`, `nat_rec` (neutral-aware, motive-carrying)
- `nat_le`, `nat_lt` (Nat comparison for universe ranks)
- `add` (recursive via select-then-apply + `fix`)
- Eq proofs on concrete arithmetic (commutativity of add)
- Tag infrastructure: `is_tagged`, `is_vhyp`/`is_vstuck`/`is_velim`,
  `is_neutral`, metadata reflection (`is_pi`, `pi_dom`, `pi_cod_fn`,
  `is_universe`, `universe_rank`, `is_eq`)
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
  [`Neutral`],     [A tagged tree (VHyp, VStuck, or VStuckElim). The only tagged values in the system.],
  [`wait(checker)(metadata)`],[The encoding for types. `apply(T, v)` evaluates `checker(metadata)(v)`.],
  [`conv(a, b)`],  [Semantic equality. Fast path: `fast_eq`. Structural: recursive with Pi/Universe handling.],
  [`fresh_hyp(A, depth)`],[Create a hypothesis: `mkVHyp(A, depth)`. Pure data construction.],
  [`ton_check(check_fn, v)`],[CPS spine inference: calls `check_fn(inferred_type)` on success, returns `FF` on failure.],
  [`val_apply(f, x)`],[Neutral-aware function application. `mkVStuck` if `f` is neutral, raw `apply` otherwise.],
  [`type_apply(T, v)`],[Type checking with abstract type support. `ton_check` if `T` is neutral, raw `apply` otherwise.],
  [`is_pi`, `pi_dom`, `pi_cod_fn`],[Metadata reflection on wait-based Pi types. Library functions.],
  [`is_universe`, `universe_rank`],[Metadata reflection on wait-based Universe types.],
  [`is_neutral`],  [`TT` iff `v` is VHyp, VStuck, or VStuckElim.],
  [`VStuckElim(motive, target)`],[Stuck eliminator. Produced by typed eliminators when the scrutinee is neutral.],
  [`TT` / `FF`],   [Encoded booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`H-rule`],      [If `v` is neutral and `conv(T, type_of_neutral(v)) = TT`, accept. Inlined into each checker via `fix` + `ton_check(fast_eq(wait(self)(meta)), v)`.],
  [`type_meta(T)`],[Extract metadata from a wait-based type: `pair_snd(pair_snd(T))`.],
  [`wait`],        [Deferred application: `wait a b c = a(b)(c)` but `wait(a)(b)` does not evaluate `a(b)`. See #raw("KERNEL_DESIGN.md").],
  [`fix`],         [Fixed-point via `wait`. Demand-driven recursion. See #raw("KERNEL_DESIGN.md").],
  [`TAG_ROOT`],    [Canonical tree prefix distinguishing tagged neutrals from data.],
  [`PI_TAG` / `UNIV_TAG` / `EQ_TAG`],[Metadata tags inside wait-based types' metadata field.],
  [*typed eliminator*],[A neutral-aware recursor (e.g.~`bool_rec`, `nat_rec`, `eq_J`). Checks `is_neutral` before triaging; produces `VStuckElim` when stuck. Carries a motive for type inference.],
  [*select-then-apply*],[Compilation pattern: closed branch functions selected by `ite2` before shared args applied. See #raw("KERNEL_DESIGN.md").],
  [*quote*],       [Convert a term to a raw tree-calculus term (bracket abstraction). One-way, lossy (metadata stripped).],
  [*registry*],    [Ambient set of rank-0 base types recognized by `Type n`.],
)

= Open problems

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

The elaborator (AST → types, depth threading, `apply` calls, quoting)
is the remaining frontier for self-hosting. It threads binder depth as
a Nat parameter through recursive calls --- standard functional
programming with no mutation. The core loop is: pattern-match on AST
nodes, construct types, check terms via `apply`, recur. All the NbE
operations it calls are already tree programs.

== Eliminating `type_apply`

`type_apply` exists because neutrals used as types (hypothesis type
variables) cannot handle raw `apply` --- they have no checker with an
inlined H-rule. A future design might wrap hypothesis types in
wait-based checkers at creation time (e.g., `fresh_hyp` returns a
wait-based type that implements the H-rule for that hypothesis). This
would make `type_apply` unnecessary and fully unify type checking as
raw `apply(T, v) = TT` for all `T`.
