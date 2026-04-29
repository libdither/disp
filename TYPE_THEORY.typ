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
  Reference implementation: #raw("lib/*.disp") (123 tests across 3 files,\
  tree-calculus programs running through the parser/driver).
]
#v(1em)

= Types as predicates

A *type* is a function: applied to a candidate value, it returns
`TT` (accepted) or `FF` (rejected). Checking `v : T` is:

```
  apply(T, v) = TT
```

`apply` is the unmodified tree-calculus evaluator. There is no
special type-checking interpreter sitting between the runtime and the
type system --- types _are_ tree-calculus programs.

This is possible because tree calculus has a property that lambda
calculus lacks: *programs are data*. Every tree --- including a
compiled function --- is fully inspectable via triage. A type can
be simultaneously a runnable predicate and a data structure whose
components (domain, codomain, universe rank) are extractable.
The `wait` encoding exploits this.

= The `wait` encoding

The primitive `wait(f)(x)` constructs a tree that holds `f` and `x`
inert until a third argument arrives:

```
  wait(f)(x)(v) = f(x)(v)
```

The normal form of `wait(f)(x)` is:

```
  fork(stem(X), fork(LEAF, x))
```

where `X` depends on `f` but not on `x`. Two facts follow:

+ `pair_fst(wait(f)(x)) = stem(X)` --- constant for a given `f`.
  All values built with the same `f` share this *signature*.

+ `pair_snd(pair_snd(wait(f)(x))) = x` --- the metadata, directly
  extractable via triage.

A type `T = wait(checker)(metadata)` is therefore both:

- A *runnable predicate*: `apply(T, v) = checker(metadata)(v)`.
- An *inspectable record*: `type_meta(T)` extracts the metadata.

The checker is a fixed function shared across all instances of a type
former (all Pi types share `pi_checker`; all Nat values are checked by
`nat_checker`). The metadata varies per instance (Pi carries its
domain and codomain; a universe carries its rank).

== Metadata tags

Each type former identifies its metadata with a tag as the first
element: `metadata = fork(TAG, payload)`. Recognition is a single
`fast_eq`:

```disp
type_meta = {T} -> pair_snd (pair_snd T)
is_pi     = {T} -> fast_eq (pair_fst (type_meta T)) PI_TAG
pi_dom    = {T} -> pair_fst (pair_snd (type_meta T))
pi_cod_fn = {T} -> pair_snd (pair_snd (pair_snd (type_meta T)))
```

Pi metadata: `fork(PI_TAG, fork(domain, fork(depth, codFn)))`. \
Universe metadata: `fork(UNIV_TAG, rank)`. \
Eq metadata: `fork(EQ_TAG, fork(A, fork(x, y)))`. \
Data types (Nat, Bool): `LEAF` (no payload needed).

= Neutrals

A hypothesis introduced under a binder --- `h : A` where `A` is
abstract --- cannot be checked against a predicate by running the
predicate body. It is a *neutral*: a symbolic value awaiting more
information.

Neutrals and types share the same `wait` structure. Where a type uses
`wait(checker)(metadata)`, a neutral uses `wait(accum)(spine)`:

```disp
accum = fix {self, meta, v} -> wait self (fork meta v)
```

`accum` is the universal neutral handler. Applied to any argument, it
wraps the argument into the spine and returns a new `wait(accum)(...)`.
Nothing reduces; the application is simply *recorded*.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Form*][*Metadata*],
  [Hypothesis `VHyp(A, id)`],
    [`fork(HYP_TAG, fork(A, id))` --- base case],
  [Stuck application `f(x)` where `f` neutral],
    [`fork(f_meta, x)` --- left-leaning spine],
  [Stuck eliminator `StuckElim(motive, target)`],
    [`fork(ELIM_TAG, fork(motive, target))` --- leaf case for eliminators],
)

Because types and neutrals are both `wait`-based, `is_neutral` is a
single signature check:

```disp
NEUTRAL_SIG = pair_fst (wait accum LEAF)
is_neutral  = {x} -> fast_eq (pair_fst x) NEUTRAL_SIG
```

All neutrals share `accum`, so they share its signature. Types use
different checkers, producing different signatures. The check is O(1).

== Why neutrals accumulate

When `f` is a hypothesis and `x` is a value, raw `apply(f, x)` runs
`accum(f_meta)(x)`, which returns `wait(accum)(fork(f_meta, x))` ---
a new neutral recording the application. This is correct:
`f(x)` cannot reduce because `f` is symbolic, so the result is stuck.

This means ordinary tree-calculus `apply` handles both type checking
and symbolic evaluation. No separate `val_apply` operation is needed.

= The H-rule

When a neutral `v` is checked against a type `T`, the predicate body
cannot inspect `v` (it is opaque). Instead, the checker *infers*
`v`'s type from its spine and compares against `T`:

```
  If v is neutral and type_of_neutral(v) = T, accept.
```

Each checker inlines this logic via `fix`:

```disp
some_checker = fix {self, meta, v} ->
    if is_neutral v then
      ton_check (fast_eq (wait self meta)) v
    else
      <body: check concrete v against metadata>
```

`wait(self)(meta)` reconstructs the checker's own type. `ton_check`
infers `v`'s type and passes it to `fast_eq(T)` as a check function.
If the types match, `TT`; otherwise `FF`.

The H-rule is *universal*: every type former uses the same pattern.
It fires for every predicate, including abstract hypothesis types.
The checker has no knowledge of other type formers --- it only needs
`ton_check` and `fast_eq`.

== The Pi checker's codomain check

When the Pi checker verifies `f : Pi(A, B)`, it creates a hypothesis
`h = mkVHyp(A, depth)`, computes `result = f(h)`, and checks
`result` against the codomain `B(h)`.

If `result` is neutral (e.g.~the identity function returns `h`),
the checker cannot apply the codomain type to it --- that would
run the codomain's checker on a neutral, which works for wait-based
types (they inline the H-rule) but not for hypothesis types (a neutral
`B(h)` would just accumulate).

The Pi checker handles this by branching:

```disp
// result is neutral → use ton_check to infer its type, compare against cod
// result is concrete → apply codomain type via raw apply
if is_neutral(result) then ton_check(fast_eq(cod_at_hyp), result)
else fast_eq(cod_at_hyp(result), TT)
```

This correctly handles the case where the codomain is an abstract type
variable (a hypothesis used as a type). `ton_check` infers the
neutral's type from its spine and compares against the codomain,
without ever applying the codomain to the result.

= `ton_check` --- CPS spine inference

`ton_check(check_fn, v)` walks the accumulated metadata spine of a
neutral `v`, infers its type, and passes it to `check_fn`. If
inference fails, it returns `FF` directly --- no error sentinel.

```disp
ton_check_meta = fix {self, check_fn, meta} ->
    if pair_fst(meta) == HYP_TAG then
      // Base: hypothesis. Stored type is pair_fst(pair_snd(meta)).
      check_fn(stored_type)
    else if pair_fst(meta) == ELIM_TAG then
      // Stuck eliminator. Type = motive(target).
      check_fn(motive(target))
    else
      // Spine: meta = fork(inner_meta, arg).
      // Recurse on head with continuation for codomain instantiation.
      self (ton_spine_cont check_fn arg) inner_meta
```

At each spine level, the recursion passes a *continuation*: if the
head's type is Pi, extract the codomain function, apply it to the
argument via raw `apply`, and pass to the outer `check_fn`. If the
head's type is not Pi, return `FF`. The recursion bottoms out at
a VHyp, which calls `check_fn` directly on the stored type.

```disp
ton_spine_cont = {check_fn, arg, head_type} ->
    if is_pi(head_type) then check_fn(pi_cod_fn(head_type)(arg))
    else FF
```

Codomain instantiation uses raw `apply` --- `pi_cod_fn(T)` extracts a
raw function from Pi's metadata, and applying it to the argument is
ordinary tree-calculus reduction. No special evaluator needed.

The CPS design avoids the need for an error sentinel. If spine
inference fails at any level, `FF` propagates directly; `check_fn` is
never called.

== H-rule integration

The H-rule inside each checker becomes:

```disp
ton_check (fast_eq (wait self meta)) v
```

`wait(self)(meta)` reconstructs the checker's own type. `fast_eq`
partially applied to this type produces the check function. If the
neutral's inferred type matches, `TT`; otherwise `FF`.

= Auxiliary operations

== `conv(a, b) → Bool`

Semantic equality. Fast path: `fast_eq` via hash-consing (O(1)).
Structural path: recursive comparison that introduces fresh
hypotheses at Pi binders:

```disp
conv_structural = fix {self, a, b} ->
    if fast_eq a b then TT
    else if is_pi a && is_pi b then
      and (self (pi_dom a) (pi_dom b))
          (self (pi_cod_fn(a)(hyp)) (pi_cod_fn(b)(hyp)))
    else if is_universe a && is_universe b then
      fast_eq (universe_rank a) (universe_rank b)
    else FF
```

Both are tree programs. The fast path is the default; the structural
path handles types constructed by different code paths.

== `fresh_hyp(A, depth) → Neutral`

Creates a `VHyp` with stored type `A` and identity `depth`:

```disp
fresh_hyp = {A, depth} -> wait accum (fork HYP_TAG (fork A depth))
```

Pure tree construction. Identity comes from *binder depth*, threaded
by the elaborator. Satisfies:

- *Opacity:* only the H-rule inspects the stored type.
- *Distinctness:* different depths produce `conv`-unequal hypotheses.

= Type constructors

Every type constructor is `wait(checker)(metadata)`. The checker
follows the template: H-rule for neutrals, predicate body for
concrete values.

== `Pi` --- dependent function type

```disp
pi_checker = fix {self, meta, v} ->
    if is_neutral v then ton_check(fast_eq(wait self meta), v)
    else
      let hyp = fresh_hyp(pi_dom_of(meta), depth_of(meta))
      let cod = pi_cod_fn_of(meta)(hyp)     // raw apply: codFn is a raw function
      let result = v(hyp)                    // raw apply: v is non-neutral
      if is_neutral result then ton_check(fast_eq(cod), result)
      else fast_eq(cod(result), TT)          // raw apply: cod is wait-based

mkPi = {domain, codFn, depth} ->
    wait pi_checker (fork PI_TAG (fork domain (fork depth codFn)))

mkArrow = {A, B, depth} -> mkPi A ({_} -> B) depth
```

`{x : A} -> B` elaborates to `mkPi A ({x} -> B) depth`.
Non-dependent `A -> B` passes a constant codomain function `{_} -> B`.

== `Type n` --- universe family

```disp
type_checker = fix {self, meta, v} ->
    let rank = rank_of(meta)                // meta = fork(UNIV_TAG, rank)
    if is_neutral v then
      ton_check (univ_check rank) v         // cumulative: ≤, not exact
    else if is_universe v then
      lt (universe_rank v) rank             // strict: Type m : Type n iff m < n
    else if is_pi v then
      and (wait self meta (pi_dom v))
          (wait self meta (pi_cod_fn(v)(fresh_hyp(pi_dom(v), stem(stem(rank))))))
    else if is_registered v then TT
    else FF

mkType = {rank} -> wait type_checker (fork UNIV_TAG rank)
```

Case 1 (neutral) uses `≤` for cumulativity. This is the only
predicate that handles neutrals in the body rather than the H-rule
alone, because cumulativity requires a range check, not exact type
match.

The Pi case recursively checks domain and codomain. The hypothesis
for the codomain uses `stem(stem(rank))` as depth to ensure
distinctness from hypotheses created by Pi construction.

== Data types

Data predicates use `wait(checker)(LEAF)` --- no metadata needed.

```disp
nat_checker = fix {self, meta, n} ->
    if is_neutral n then ton_check(fast_eq(wait self meta), n)
    else if n == leaf then TT
    else if is_tree_fork n then
      if fork_left(n) == leaf then wait self meta (fork_right(n))
      else FF
    else FF

Nat = wait nat_checker LEAF

bool_checker = fix {self, meta, b} ->
    if is_neutral b then ton_check(fast_eq(wait self meta), b)
    else ite2 TT (fast_eq b FF) (fast_eq b TT)

Bool = wait bool_checker LEAF
```

== `Eq` --- propositional equality

```disp
eq_checker = fix {self, meta, p} ->
    if is_neutral p then ton_check(fast_eq(wait self meta), p)
    else if p == LEAF then conv(x_of(meta), y_of(meta))
    else FF

mkEq = {A, x, y} -> wait eq_checker (fork EQ_TAG (fork A (fork x y)))
```

`refl = LEAF`. When `p = refl`, the checker verifies `conv(x, y)`.
Hash-consing makes this O(1).

=== J eliminator (typed, neutral-aware)

```disp
eq_J = {A, x, motive, base, y, p} ->
    if is_neutral p then mkVStuckElim(motive(y)(p), p)
    else base
```

=== Typed eliminators --- the general pattern

Raw `triage` on a neutral produces garbage (the spine structure is
misinterpreted as data). Typed eliminators guard with `is_neutral`
before dispatching:

```disp
bool_rec = {motive, t_case, f_case, target} ->
    if is_neutral target then mkVStuckElim(motive, target)
    else ite2 t_case f_case target
```

The *motive* is a raw function from the scrutinee to the result
type. `StuckElim(motive, target)` is a neutral; `ton_check` handles
it by applying the motive to the target.

Any user-defined type can follow this pattern: define a checker,
define a typed eliminator that guards on `is_neutral` and stores the
motive. No runtime support needed.

= Worked examples

== `3 : Nat`

```
apply(Nat, Succ(Succ(Succ(Zero))))
  Nat = wait(nat_checker)(LEAF)
  → nat_checker(LEAF)(Succ²(Zero))
  not neutral → is_tree_fork? yes. fork_left == leaf? yes → Succ
  → wait(nat_checker)(LEAF)(Succ(Zero))    // recurse via wait(self)(meta)
  → wait(nat_checker)(LEAF)(Zero)
  Zero == leaf → TT ✓
```

== `{x : Nat} -> x` checked against `Nat -> Nat`

```
apply(Arrow(Nat, Nat), {x}->x)
  Arrow = wait(pi_checker)(meta) where meta encodes domain=Nat, codFn=K(Nat)
  → pi_checker(meta)({x}->x)
  {x}->x is not neutral → Pi body:
    hyp = mkVHyp(Nat, 0) — a neutral (wait(accum)(fork(HYP_TAG, fork(Nat, 0))))
    cod = K(Nat)(hyp) = Nat           // raw apply on constant function
    result = ({x}->x)(hyp) = hyp      // raw apply: substitution
    is_neutral(result)? yes →
    ton_check(fast_eq(Nat), hyp):
      hyp meta = fork(HYP_TAG, fork(Nat, 0))
      pair_fst = HYP_TAG → base case
      stored type = Nat
      fast_eq(Nat)(Nat) = TT ✓
```

== `{f:Nat->Nat, x:Nat} -> f x` --- spine inference

Checked against `Pi(Nat->Nat, {_}->Pi(Nat, {_}->Nat))`:

```
apply(outer_Pi, term)
  → pi_checker(outer_meta)(term)
  not neutral → Pi body:
    h_f = mkVHyp(Nat->Nat, 0)
    result = term(h_f) = {x}->f x = h_f (via eta-reduction)
    cod = K(Pi(Nat,{_}->Nat))(h_f) = Pi(Nat, {_}->Nat)
    is_neutral(h_f)? yes →
    ton_check(fast_eq(Pi(Nat,{_}->Nat)), h_f):
      h_f meta = fork(HYP_TAG, fork(Nat->Nat, 0))
      stored type = Nat->Nat
      fast_eq(Pi(Nat,{_}->Nat))(Nat->Nat)
      = fast_eq(Arrow(Nat,Nat,depth1))(Arrow(Nat,Nat,depth2))
      These are equal iff the depths match. The elaborator ensures
      consistent depth assignment. ✓
```

Note: in untyped compilation, `{x}->f x` eta-reduces to `f`. The
elaborator would prevent this by threading type information through
compilation. For the test suite, types are constructed with matching
depths.

== Polymorphic identity

`{A:Type 0, x:A} -> x` checked against `Pi(Type 0, {A}->Pi(A, {_}->A))`:

```
apply(outer_Pi, poly_id)
  → pi_checker(outer_meta)({A}->{x}->x)
  not neutral → Pi body:
    h_A = mkVHyp(Type 0, 0)
    cod = ({A}->Pi(A,{_}->A))(h_A) = Pi(h_A, {_}->h_A)
    result = ({A}->{x}->x)(h_A) = {x}->x
    is_neutral({x}->x)? no →
    fast_eq(Pi(h_A,{_}->h_A)({x}->x), TT):
      → pi_checker(inner_meta)({x}->x)
      not neutral → Pi body:
        h_x = mkVHyp(h_A, 1)
        cod2 = ({_}->h_A)(h_x) = h_A       // codomain is h_A itself
        result2 = ({x}->x)(h_x) = h_x
        is_neutral(h_x)? yes →
        ton_check(fast_eq(h_A), h_x):
          h_x meta = fork(HYP_TAG, fork(h_A, 1))
          stored type = h_A
          fast_eq(h_A)(h_A) = TT ✓
```

The critical step: `h_A` is both the codomain and the type that
`h_x` is checked against. `ton_check` extracts `h_x`'s stored type
(which is `h_A`) and `fast_eq` succeeds. This is how abstract type
variables work --- the H-rule compares the hypothesis-as-type against
the stored type of the value, using nothing more than hash-consed
identity.

== Rejection: `{A:Type 0, x:A} -> x x`

```
apply(Pi(Type 0, {A}->Pi(A,{_}->A)), bad_term)
  ...eventually checks: result = h_x(h_x)
  h_x is neutral → apply(h_x, h_x) = accum(h_x_meta)(h_x)
    = wait(accum)(fork(h_x_meta, h_x))     // accumulated spine
  ton_check(fast_eq(h_A), stuck):
    meta = fork(h_x_meta, h_x) — spine case
    recurse on h_x_meta with continuation:
      h_x_meta = fork(HYP_TAG, fork(h_A, 1)) — base case
      stored type = h_A
      continuation: is_pi(h_A)? h_A is a neutral, type_meta = fork(Type0, 0)
        pair_fst(type_meta(h_A)) = Type0 ≠ PI_TAG → not Pi → FF
  → FF ✓
```

Self-application is rejected because `h_A` is an opaque type
variable with no evidence that it is a function type.

= Soundness

Soundness means: if `apply(T, v) = TT`, then `v` semantically
inhabits the type denoted by `T`.

The argument rests on four invariants:

+ *Freshness.* `fresh_hyp` produces opaque, distinct hypotheses.
  Opacity ensures parametricity (one check = universal
  quantification). Distinctness ensures multi-parameter types don't
  conflate their variables.

+ *Semantic equality.* `conv` agrees with tree-calculus identity on
  closed normal forms. Hash-consing guarantees that two evaluations
  producing the same value yield `conv`-equal trees.

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

The elaborator produces `(term, type)` and checks via
`apply(type, term) = TT`. After checking, the term is *quoted*
(bracket-abstracted) to a raw tree-calculus term for runtime
execution. Types are erased.

The parser and surface syntax are I/O layers. The elaborator is the
boundary: it consumes AST, constructs types via the `wait`-based
constructors, checks terms via raw `apply`, and emits raw trees.

The elaborator threads *binder depth* as a parameter. Each Pi binder
increments depth; `fresh_hyp` receives the current depth. No mutable
state crosses the elaboration boundary.

= Implementation status

== What is implemented as tree programs (123 tests)

All of the following run as `.disp` source through the parser/driver,
validated by `lib/*.test.disp`:

- Wait-based type checkers: Pi, Nat, Bool, Eq, Type n --- each with
  H-rule inlined via `fix`, all using `wait(checker)(metadata)`
- Wait-based neutrals via `accum` handler (VHyp, stuck spines,
  StuckElim) --- no tag infrastructure needed
- `ton_check` (CPS spine inference, walks accumulated metadata)
- `conv` (fast\_eq) and `conv_structural` (recursive Pi/Universe)
- `fresh_hyp`, `mkVStuckElim` (neutral constructors)
- `mkPi`, `mkArrow` (Pi construction)
- `Type n` universe predicate (all four cases, `fix`-recursive)
- `Nat`, `Bool` predicates
- `Eq` type (`mkEq`, `refl`, `eq_J`, `eq_subst`, `eq_sym`, `eq_cong`)
- Typed eliminators: `bool_rec`, `nat_rec` (neutral-aware)
- `nat_le`, `nat_lt`, `add` (arithmetic)
- Eq proofs on concrete arithmetic (commutativity of add)
- Integration: polymorphic identity, polymorphic const (+ rejection),
  self-application rejection, flip, composition, Church pairs,
  higher-order functions, deep spine inference

== What remains TypeScript-only

- The elaborator (AST walking, depth threading, motive inference,
  quoting)
- Error messages and diagnostics
- The parser / bracket abstraction / driver

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`wait(checker)(metadata)`],[The encoding for types. `apply(T, v)` evaluates `checker(metadata)(v)`.],
  [`wait(accum)(spine)`],[The encoding for neutrals. `apply(n, v)` accumulates `v` onto the spine.],
  [`accum`],[The universal neutral handler: `fix {self, meta, v} -> wait self (fork meta v)`.],
  [`is_neutral`],[Signature check: `fast_eq(pair_fst(x), NEUTRAL_SIG)`. O(1).],
  [`type_meta(T)`],[Extract metadata: `pair_snd(pair_snd(T))`.],
  [`ton_check(check_fn, v)`],[CPS spine inference: walks accumulated metadata, calls `check_fn(inferred_type)` on success, returns `FF` on failure.],
  [`conv(a, b)`],[Semantic equality. Fast path: `fast_eq` (O(1) via hash-consing).],
  [`fresh_hyp(A, depth)`],[Create hypothesis: `wait accum (fork HYP_TAG (fork A depth))`.],
  [`is_pi`, `pi_dom`, `pi_cod_fn`],[Metadata reflection on Pi types.],
  [`is_universe`, `universe_rank`],[Metadata reflection on Universe types.],
  [`H-rule`],[If `v` is neutral and `type_of_neutral(v) = T`, accept. Inlined into each checker via `fix` + `ton_check(fast_eq(wait(self)(meta)), v)`.],
  [`TT` / `FF`],[Encoded booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`StuckElim(motive, target)`],[Stuck eliminator. Produced by typed eliminators when the scrutinee is neutral.],
  [`wait`],[Deferred application: `wait a b c = a(b)(c)` but `wait(a)(b)` does not evaluate `a(b)`.],
  [`fix`],[Fixed-point via `wait`. `fix(f)(arg) = f(fix(f))(arg)`. Demand-driven.],
  [`PI_TAG` / `UNIV_TAG` / `EQ_TAG`],[Metadata tags inside type metadata.],
  [`HYP_TAG` / `ELIM_TAG`],[Metadata tags inside neutral metadata.],
  [*typed eliminator*],[A neutral-aware recursor (e.g.~`bool_rec`, `nat_rec`, `eq_J`). Checks `is_neutral` before dispatching; produces `StuckElim` when stuck.],
  [*select-then-apply*],[Compilation pattern for strict branching. See #raw("KERNEL_DESIGN.md").],
)

= Open problems

== B/C combinators

Turner's B and C combinators reduce the work done by S when only one
side of an application depends on the abstracted variable. The current
implementation uses S/K/I with η-reduction and K-composition. Adding
B/C would reduce tree sizes for functions with 4+ variables.

== Elaborator as tree program

The elaborator (AST → types, depth threading, `apply` calls, quoting)
is the remaining frontier for self-hosting. It threads binder depth as
a Nat parameter --- standard functional programming with no mutation.
All NbE operations it calls are already tree programs.

== Eta-reduction and typed compilation

Bracket abstraction eta-reduces `{x} -> f x` to `f`. In untyped
compilation this is always correct, but in a typed setting it can
collapse terms in ways that change their type-checking behavior (e.g.,
a two-argument function collapses to a one-argument identity). The
elaborator must thread type information through compilation to prevent
eta-reductions that would change semantics.
