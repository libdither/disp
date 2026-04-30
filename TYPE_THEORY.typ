#set document(title: "Disp Type Theory")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#let note(body) = block(
  breakable: false,
  above: 0.8em, below: 0.8em,
  stroke: (left: 2pt + rgb("#cccccc")),
  inset: (left: 1em, y: 0.3em),
  body,
)

#align(center, text(22pt, weight: "bold")[Disp Type Theory])
#v(0.3em)
#align(center)[
  The semantics of the Disp object language.\
  Reference implementation: #raw("lib/kernel.disp") (146 tests).\
  Tree-calculus idioms: #raw("KERNEL_DESIGN.md").
]
#v(1em)

= Why types as predicates?

Disp is built on tree calculus: a minimal foundation where every
program and every piece of data is a binary tree, and the only
operation is `apply`. There is no built-in notion of "type." If we
want dependent types, we need to build them ourselves --- as tree
programs.

This constraint leads to a striking design: *types are predicates*.
A type is an ordinary function that takes a value and returns `TT`
(accepted) or `FF` (rejected). Type checking is function application:

```
  T(v) = TT      means "v has type T"
```

No external type checker. No meta-level judgment. Just one tree
applied to another. The type system is a library, not a built-in.

This buys us three things:

+ *Self-hosting.* The type checker is itself a tree program. It can
  type-check itself, optimize itself, and be reasoned about in the
  object language.

+ *Simplicity.* There is one operation (`apply`) and one representation
  (trees). Types, terms, and the checker share the same substrate.

+ *Extensibility.* Adding a new type means writing a new predicate
  function. No built-in case analysis needs updating.

The rest of this document explains how to make this idea work ---
starting from familiar concepts, building up to the full design.

= Arriving at the design

== Starting point: bidirectional type checking

In a standard dependently-typed language, the type checker has two
modes:

- *Check:* given a term `e` and a type `T`, verify `e : T`.
- *Infer:* given a term `e`, compute its type.

Both modes operate on *values* --- terms evaluated to head-normal
form via NbE (normalization by evaluation). The check mode for a
specific type `T` is a function:

```
  check_T(v) → TT or FF
```

It takes a value and returns whether that value inhabits `T`.

== The flip: types _are_ the check function

Here is the key observation. `check_T` is a function from values to
booleans. What if `T` _itself_ were that function?

Instead of having a separate checker that knows about `Nat` and
dispatches:

```
  check(v, Nat) = ... match on Nat, call the Nat-checking logic ...
```

...make `Nat` itself the checking logic:

```
  Nat(zero)     = TT      // zero is a natural
  Nat(succ(n))  = Nat(n)  // succ of a nat is a nat
  Nat(anything) = FF      // everything else is rejected
```

For concrete data, this is straightforward. `Nat` is a recursive
function that pattern-matches on the value. `Bool` checks for `TT`
or `FF`. These are ordinary programs.

== The problem: what about symbolic values?

In NbE-based type checking, you frequently need to check terms _under
binders_. To check `{x : Nat} -> x` against `Nat -> Nat`, a standard
checker:

+ Creates a fresh hypothesis $h$ of type `Nat` (a symbolic value).
+ Evaluates the body with $h$ substituted for `x`, producing $h$.
+ Checks that $h$ has type `Nat`.

Step 3 is the problem. $h$ is an opaque symbol --- it is not `zero`
or `succ(n)`. The `Nat` predicate cannot pattern-match it. It would
return `FF`, wrongly rejecting the identity function.

This is the fundamental tension. Types-as-predicates works for concrete
values but breaks on symbolic ones. Standard NbE solves this by
keeping type information in the _checker_, not in the values. But in
our design, the type IS the checker. Where does the symbolic type
information go?

== The H-rule: neutrals carry their types

The solution is the *H-rule* (hypothesis rule): make every symbolic
value carry the type it was introduced at.

When we create a hypothesis $h$ of type `Nat`, we store `Nat` inside
$h$. When a type predicate encounters a symbolic value, it does not
try to pattern-match --- instead, it reads the stored type and checks:
"does this symbolic value's type match me?"

```
  Nat(h):
    is h a neutral?  →  does h's stored type equal Nat?
                         yes → TT (accept)
                         no  → FF (reject)
    is h zero?       →  TT
    is h succ(n)?    →  Nat(n)
    otherwise        →  FF
```

The H-rule is the "infer" direction of bidirectional checking, embedded
into every predicate. Each predicate handles both concrete values
(by pattern matching) and symbolic values (by reading the stored type).

This is all that's needed for simple types. For `Pi`, things get more
interesting.

= Pi --- NbE inside a predicate

== What Pi must check

`Pi(A, B)` is the dependent function type $Pi (x : A) . B (x)$.
It must verify: "does this function take `A`-inputs to `B`-outputs?"

In standard bidirectional type checking, the rule is:

```
  Γ ⊢ f : Π(x : A). B(x)
  ─────────────────────────────────────
  Let h = fresh hypothesis of type A
  Check: f(h) : B(h)     in context Γ, h : A
```

As a predicate, `Pi(A, B)` must do exactly this:

```
  Pi(A, B)(f):
    h  = new hypothesis with stored type A
    result = f(h)                      // apply candidate to hypothesis
    expected = B(h)                    // compute expected return type
    check result against expected
```

*This is NbE.* The Pi predicate creates a symbolic value, evaluates
the function on it, and checks the result. The NbE evaluation is
happening _inside_ the predicate --- it is not external infrastructure.

This is the central insight of the design: *the Pi type bootstraps NbE
within itself*. Every function-type check is a miniature evaluation
session.

== Two cases for the result

When `f(h)` returns, the result is either concrete or neutral:

*Concrete* (e.g., `{x} -> zero` applied to $h$ returns `zero`):\
Apply the expected type directly: `B(h)(zero)`. If that returns `TT`,
accept; otherwise reject.

*Neutral* (e.g., `{x} -> x` applied to $h$ returns $h$ itself):\
The result is a symbolic value. We could apply `B(h)` to it (the
H-rule inside `B(h)` would fire), but there is a more direct path:
read the result's stored type and compare it against the expected type:

```
  infer(fast_eq(expected_type), result)
```

If the result's stored type matches the expected type, accept.

The `infer` operation is a CPS-style extraction: `infer(check_fn, v)`
reads `v`'s stored type and passes it to `check_fn`. If `v` is not
a neutral, it returns `FF`.

= Stuck applications and smart accum

== The spine problem

Consider checking `{f, x} -> f(x)` against
`(Nat → Nat) → Nat → Nat`. The Pi checker creates two hypotheses:

```
  h_f : Nat → Nat
  h_x : Nat
```

Then evaluates the body: `h_f(h_x)`. Both are neutrals --- `h_f` is
a hypothesis and `h_x` is a hypothesis. The application is _stuck_:
`h_f(h_x)` cannot reduce. It becomes a stuck application, itself a
neutral.

What type should `h_f(h_x)` have? Since `h_f : Nat → Nat` and
`h_x : Nat`, the result should have type `Nat`. But who computes this?

== The accumulator

Every neutral has a handler that fires when the neutral is applied.
In Disp, all neutrals share a single handler called the *accumulator*
(`accum`). When a neutral is applied to a value, the accumulator:

+ Reads the neutral's stored type.
+ Checks if the type is a `Pi` type (by comparing signatures).
+ If yes: computes `codomain_fn(argument)` as the result type.
+ If no: stores `FF` (unknown type --- any subsequent check will fail).
+ Produces a new neutral with the computed result type.

```
  accum(meta, v):
    my_type = stored type of this neutral
    if my_type is Pi:
      result_type = codFn(v)           // instantiate codomain at arg
    else:
      result_type = FF                 // not a function type
    → new neutral with stored type = result_type
```

This is *smart accumulation*: the handler tracks types through
applications. After `h_f(h_x)`, the resulting neutral has stored type
`({_} -> Nat)(h_x) = Nat`. No spine walking, no post-hoc inference.
The type is computed on the spot.

== O(1) type inference

Because smart accum stores the type at each step, `infer` is trivial:

```
  infer(check_fn, v):
    if is_neutral(v): check_fn(pair_fst(metadata(v)))
    else:             FF
```

Just extract `pair_fst` of the metadata. This is O(1) regardless of
how many applications produced the neutral. There is no spine-walking
`type_of_neutral` function.

= Encoding types as trees

== `wait` --- deferred evaluation

Tree calculus is strict: every application is evaluated immediately.
The standard fixed-point combinator $Y f = f (Y (f))$ diverges because
$Y (f)$ tries to evaluate itself eagerly.

The solution is `wait`:

```
  wait(a)(b)    is inert (just tree data)
  wait(a)(b)(c) = a(b)(c)   (evaluates a(b) only when c arrives)
```

`wait` defers evaluation until a third argument arrives. This is the
key building block for recursion (`fix`) and for type encoding.

== Types are `wait(checker)(metadata)`

A type is built as:

```
  wait(checker)(metadata)
```

This is an inert tree. When applied to a value `v`:

```
  wait(checker)(metadata)(v)  =  checker(metadata)(v)
```

The checker function receives its own metadata (domain, codomain,
rank, etc.) and the candidate value, then returns `TT` or `FF`.

== `fix` --- recursion via `wait`

Recursive checkers (Nat, Type n, Pi) use `fix`:

```
  fix(f) = wait m ({x} -> f(wait m x))     where m = {x} -> x x
  fix(f)(arg) = f(fix(f))(arg)              // unfolds one step
```

`fix(f)` is itself a `wait`-encoded value. When used as a checker's
first argument to `wait`, this gives exactly the right structure:
`wait(fix(checker_body))(metadata)` is a type whose checker can call
itself recursively.

== Signatures

Every `wait(checker)(metadata)` tree has a constant prefix:

```
  pair_fst(wait(checker)(metadata)) = stem(checker)
```

This is the *signature* --- it identifies which checker produced the
type. All `Nat` values share one signature. All `Pi` types share
another. Neutrals (which are `wait(accum)(...)`) share the accum
signature.

Recognition uses `fast_eq` on signatures:

```
  is_neutral(x) = fast_eq(pair_fst(x), accum_signature)
  is_pi(x)      = fast_eq(pair_fst(x), pi_signature)
```

Each check is O(1) via hash-consing.

= The unified kernel

== Why a single mutual fixed point?

Each checker needs to recognize other checkers' signatures:
- Pi's checker calls `is_neutral` (needs accum's signature)
- Type n's checker calls `is_pi`, `is_eq` (needs their signatures)
- All checkers call `infer` (needs accum's signature)

Signatures are derived from the checkers themselves. If the Pi
checker were defined separately, its signature would be a function
of its own tree. To compare against it, other checkers would need
to import it. This creates a web of dependencies that ultimately
requires mutual recursion.

The kernel puts everything into a single `fix`:

```disp
  kernel = fix ({self, query} ->
    query
      q_accum_fn q_pi_fn q_nat_fn q_bool_fn q_eq_fn
      q_type_fn q_infer_fn
      q_sig_accum_fn q_sig_pi_fn q_sig_eq_fn q_sig_type_fn
      self query)
```

Components reference each other through `self` (the kernel itself).
`query` is a Church-encoded selector: `Q_NAT` picks the Nat checker,
`Q_PI` picks Pi, etc. Selection is O(1) --- no conditional chain.

== Constructing types

Types are built with *double wait*:

```disp
  Nat  = wait(wait(kernel)(Q_NAT))(leaf)
  Bool = wait(wait(kernel)(Q_BOOL))(leaf)
  Pi(A, B, d) = wait(wait(kernel)(Q_PI))(fork(A, fork(d, B)))
```

`wait(kernel)(Q_NAT)` is inert --- it does not evaluate `kernel(Q_NAT)`
eagerly. The full evaluation happens only when the type is applied to
a value: `Nat(v) = kernel(Q_NAT)(leaf)(v) = nat_checker(leaf)(v)`.

This deferred construction is essential: it means defining `Nat` at
module level costs ~2 reduction steps, not thousands.

== The H-rule, concretely

Inside the kernel, the H-rule is a shared function used by all four
type checkers (Pi, Nat, Bool, Eq):

```disp
  q_h_rule_fn = {ks, query, self, meta, v} ->
    ks Q_INFER (fast_eq (wait (wait ks query) meta)) v
```

Breaking it down:
- `wait(wait(ks)(query))(meta)` reconstructs _this checker's own
  type_ from the kernel and its metadata.
- `fast_eq(that_type)` is a partial application: a function that
  checks if its argument equals this type.
- `ks Q_INFER` dispatches to the `infer` component, which extracts
  the neutral's stored type and passes it to `fast_eq`.

If the neutral's type matches, `fast_eq` returns `TT`. Otherwise `FF`.

= Soundness

The design avoids several pitfalls common to self-hosted type systems.

== Why not tags?

An earlier design used tags: types were `fork(fork(TAG, kind), payload)`,
and the checker dispatched on `kind`. The problem: tags are just tree
patterns. Anyone can construct a tree with the right tag shape:

```disp
  fake_nat = fork(fork(TAG_ROOT, KV_LAM), fork(LEAF, {_ } -> TT))
```

This "fake Nat" accepts everything. If the checker trusts the tag,
`fake_nat(anything) = TT` and soundness is broken.

== Canonical identity

The current design uses *canonical identity* instead of tags.
`Nat` is the tree `wait(wait(kernel)(Q_NAT))(leaf)` --- a specific,
unique tree node. The kernel is the single source of truth:

```disp
  is_nat_type(x) = fast_eq(x, Nat)     // tree identity, O(1)
```

An adversarial value `wait({_,_} -> TT)(leaf)` has a different
checker function, therefore a different `stem(checker)` prefix,
therefore a different tree. `fast_eq` rejects it. The identity of a
type is determined by its checker function, which is unforgeable.

Type-former recognition (e.g., `is_pi`) uses *signature* checks:
`fast_eq(pair_fst(x), pi_signature)`. The Pi signature is derived
from the kernel's own Pi checker. A fake Pi with a different checker
produces a different signature.

== Neutral opacity

Neutrals carry their types, but they cannot forge them. A hypothesis
is created by `Hyp(type, id)`, which sets the stored type at creation
time. Smart accum propagates types faithfully --- it reads the existing
type and computes the new one from the Pi codomain. There is no way
for a neutral to claim a type it was not given.

== Universe well-foundedness

`Type n` uses strict `<` for containment: `Type m : Type n` iff
`m < n`. No `Type : Type`, no cycles.

Budget exhaustion (the evaluation step limit) is a completeness
concession, not an unsoundness. A check that exceeds the budget is
rejected, never falsely accepted.

= Reference: type formers

#note[
  The pseudocode below uses simplified notation. The actual
  implementation in #raw("lib/kernel.disp") uses select-then-apply
  patterns (see #raw("KERNEL_DESIGN.md")) for correct compilation
  under bracket abstraction.
]

== Nat

Zero = `leaf`. Succ(n) = `fork(leaf, n)`. Metadata = `leaf`.

```
  Nat = wait(nat_checker)(leaf)
  nat_checker(meta, n):
    if is_neutral(n): H-rule
    if n = leaf: TT                           // zero
    if is_fork(n) and not is_neutral(n):      // exclude neutrals
      if pair_fst(n) = leaf:
        nat_checker(meta, pair_snd(n))        // succ → recurse
      else: FF
    else: FF
```

== Bool

`TT = leaf`, `FF = stem(leaf)`. Metadata = `leaf`.

```
  Bool = wait(bool_checker)(leaf)
  bool_checker(meta, b):
    if is_neutral(b): H-rule
    if b = leaf: TT       // TT
    if b = FF: TT         // FF
    else: FF
```

== Pi

Metadata = `fork(domain, fork(depth, codFn))`.

```
  Pi(A, B, d) = wait(pi_checker)(fork(A, fork(d, B)))
  pi_checker(meta, v):          // meta = fork(domain, fork(depth, codFn))
    if is_neutral(v): H-rule
    let hyp = Hyp(domain, depth)
    let result = v(hyp)
    let expected = codFn(hyp)
    if is_neutral(result):
      infer(fast_eq(expected), result)
    else:
      fast_eq(expected(result), TT)
```

Arrow sugar: `Arrow(A, B, d) = Pi(A, {_} -> B, d)`.

== Eq

Metadata = `fork(A, fork(x, y))`. Sole constructor: `refl = leaf`.

```
  Eq(A, x, y) = wait(eq_checker)(fork(A, fork(x, y)))
  eq_checker(meta, p):
    if is_neutral(p): H-rule
    if p = leaf: fast_eq(x, y)       // refl: check x ≡ y
    else: FF
```

== Type n (universes)

Metadata = `rank` (a natural number).

```
  Type(n) = wait(type_checker)(n)
  type_checker(meta, x):
    if is_neutral(x): infer(univ_check(n), x)   // cumulative
    if is_universe(x): nat_lt(rank(x), n)        // Type m, m < n
    if is_pi(x):
      and(self(n, pi_dom(x)),
          self(n, codFn(Hyp(pi_dom(x), ...))))
    if is_eq(x):
      and(self(n, eq_A(x)),
          and(eq_A(x)(eq_x(x)), eq_A(x)(eq_y(x))))
    if is_registered(x): TT                      // Nat, Bool
    else: FF
```

= Typed eliminators

Raw `triage` (tree calculus pattern matching) on a neutral interprets
the neutral's internal `wait`-structure as data, producing garbage.
Typed eliminators check `is_neutral` first and freeze as a neutral
term when stuck:

```disp
  bool_rec(motive, t_case, f_case, target):
    if is_neutral(target):
      StuckElim(motive(target), target)
    else:
      if target = TT: t_case
      else: f_case

  nat_rec(motive, base, step, target):
    if is_neutral(target):
      StuckElim(motive(target), target)
    else:
      ... zero/succ pattern match with recursion ...
```

The *motive* maps the scrutinee to the result type. `StuckElim` stores
`motive(target)` as the result type, so `infer` can extract it later.

Eq operations (`eq_J`, `eq_subst`, `eq_sym`, `eq_cong`) follow the
same pattern: concrete proof (`refl`) dispatches immediately; neutral
proof freezes as `StuckElim`.

= Worked examples

== `3 : Nat`

```
  Nat(succ(succ(succ(zero))))
    nat_checker(leaf, fork(leaf, fork(leaf, fork(leaf, leaf))))
    is_neutral? no. n = leaf? no.
    is_fork and pair_fst = leaf? yes → recurse on pair_snd
    ... recurse on 2, then 1, then 0 ...
    n = leaf → TT ✓
```

Pure data checking. No hypotheses, no H-rule.

== `{x} -> x` checked against `Nat -> Nat`

```
  (Arrow Nat Nat d)({x} -> x)
    pi_checker(fork(Nat, fork(d, {_}->Nat)), {x}->x)
    is_neutral? no → concrete check
    h = Hyp(Nat, d)             // fresh hypothesis
    result = ({x}->x)(h) = h   // apply identity to h
    expected = ({_}->Nat)(h) = Nat
    is_neutral(h)? yes
      infer(fast_eq(Nat), h)
        neutral_type(h) = Nat   // stored when h was created
        fast_eq(Nat, Nat) = TT ✓
```

The H-rule fires because `h` was introduced at type `Nat`, and the
expected codomain is `Nat`.

== Self-application rejection: `{x} -> x x`

Checked against `(A : Type 0) → A → A`:

```
  Pi checks A : Type 0:
    h_A = Hyp(Type0, d0)
  Pi checks x : h_A:
    h_x = Hyp(h_A, d1)               // type is a hypothesis!
  Body: h_x(h_x)
    smart accum fires on h_x(h_x):
      h_x has type h_A
      is h_A a Pi type? no (it's a neutral, not a Pi)
      → result_type = FF
    → stuck neutral with stored type FF
  Pi checks result:
    expected = h_A
    infer(fast_eq(h_A), result)
      neutral_type(result) = FF
      fast_eq(h_A, FF) = FF           // h_A ≠ FF
    → rejected ✓
```

Self-application is blocked because `h_A` is an opaque hypothesis,
not a Pi type. Smart accum stores `FF`, and the codomain check fails.
No special-case logic needed.

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`T(v) = TT`],   [Type checking. Apply type to value; raw tree-calculus application.],
  [`wait(a)(b)`],   [Inert tree. `wait(a)(b)(c) = a(b)(c)`. See #raw("KERNEL_DESIGN.md").],
  [`fix(f)`],       [Fixed-point via `wait`. `fix(f)(x) = f(fix(f))(x)`.],
  [`Hyp(type, id)`],[Create a neutral hypothesis carrying the given type.],
  [`StuckElim(type, target)`],[Stuck eliminator, produced by typed eliminators on neutral scrutinees.],
  [`infer(f, v)`],  [Extract neutral's stored type, pass to `f`. Returns `FF` if `v` is not neutral.],
  [`is_neutral`],   [Signature check: does `v` share the accumulator's signature? O(1).],
  [`smart accum`],  [The neutral handler. Propagates types through application via codomain instantiation.],
  [`H-rule`],       [Each checker's first branch: if `v` is neutral and its stored type matches this type, accept.],
  [`signature`],    [`pair_fst` of a `wait`-encoded value. Constant per checker; used for recognition.],
  [`kernel`],       [Single mutual `fix` containing all checkers, accum, infer, and signature queries.],
  [*typed eliminator*],[Neutral-aware recursor. Checks `is_neutral` before dispatching; freezes as `StuckElim` when stuck.],
  [`TT` / `FF`],    [Booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`refl`],         [Sole constructor of `Eq A x y`. Equals `leaf`.],
  [*select-then-apply*],[Compilation pattern for deferred branching. See #raw("KERNEL_DESIGN.md").],
)

= Open problems

== B/C combinators

The bracket abstraction uses S, K, I with η-reduction and
K-composition. Turner's B and C combinators would reduce tree sizes
for functions with 4+ variables. This is a compilation improvement,
not a semantic change.

== Elaborator

The elaborator (AST → typed terms, depth threading, motive inference)
is the remaining frontier. All kernel operations are tree programs;
the elaborator would orchestrate them, threading binder depth as a
Nat parameter.
