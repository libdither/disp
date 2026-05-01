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
  above: 0.8em,
  below: 0.8em,
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

This constraint produces a natural design: *types are predicates*.
A type is an ordinary function that takes a value and returns `TT`
(accepted) or `FF` (rejected). Type checking is function application:

```
  T(v) = TT      means "v has type T"
```

There is no external type checker, and no meta-level judgment system. The system is simply one tree-calculus tree applied to another, stored in a library file.

This buys us three things:

+ *Self-hosting.* The type checker is itself a tree program. It can
  type-check itself, optimize itself, and be reasoned about in the
  object language.

+ *Simplicity.* There is one operation (`apply`) and one representation
  (trees). Types, terms, and the checker share the same substrate.

+ *Extensibility.* Adding a new type means writing a new predicate
  function. No built-in case analysis needs updating.

The rest of this document works through the details.

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

`check_T` is a function from values to booleans. What if `T` _itself_
were that function?

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

That's the tension. Types-as-predicates works for concrete values but
breaks on symbolic ones. Standard NbE solves this by keeping type
information in the _checker_, not in the values. But in our design,
the type IS the checker. Where does the symbolic type information go?

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

The H-rule is the neutral case of every predicate. Each predicate
handles both concrete values (by pattern matching) and symbolic values
(by reading the stored type).

This handles simple types. Pi is harder.

= Pi --- NbE inside a predicate

== What Pi must check

`Pi(A, B)` is the dependent function type $Pi (x : A) . B (x)$.
It must verify: "does this function take each `A`-input `x` to a
`B(x)`-output?"

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

*This is NbE.* The evaluation happens inside the predicate --- not as
external infrastructure. The point: *the Pi type bootstraps NbE within
itself*.

== Dependent codomains are programs

`B` is not metadata to be inspected by a meta-level checker. It is an
ordinary object-language function from values to types. Computing
`B(x)` is just tree-calculus application.

If `x` is concrete, `B(x)` runs like any other program: it may build a
type containing `x`, branch on `x`, or call eliminators. If `x` is a
hypothesis, the same application still runs, but operations that try to
use the hypothesis meet the neutral machinery described below.

So dependent type checking does not need a separate substitution
engine for codomains. The Pi checker creates `h : A`, applies the
candidate function to `h`, and also applies the codomain function to
`h`.

== What happens when you apply a hypothesis?

The Pi checker creates hypotheses and applies the candidate function
to them. For simple cases like `{x} -> x`, the result is $h$ itself
--- a hypothesis carrying its type from creation. The H-rule handles
it.

But consider checking `{f, x} -> f(x)` against
`(Nat → Nat) → Nat → Nat`. The Pi checker creates:

```
  h_f : Nat → Nat
  h_x : Nat
```

The body evaluates to `h_f(h_x)`. Both are hypotheses. `h_f` is not
a real function --- it is a symbolic placeholder. So what happens when
you apply it?

In a standard type checker, nothing --- the application is stuck,
irreducible because the head is a variable. A separate `infer`
judgment would later walk the spine, look up `h_f`'s type in a
context, and compute that the result has type `Nat`.

But tree calculus has no stuck terms. Every application reduces.
Something _will_ run when `h_f` is applied. The question is: what?

== Two interpreters, one `apply`

The answer is that `apply` in Disp runs one of two interpreters,
depending on what is being applied:

- *Concrete function applied to anything:* the *value interpreter*
  runs. This is normal tree calculus reduction --- the program
  executing.

- *Hypothesis applied to anything:* the *type interpreter* runs.
  Instead of computing a value, it computes the result _type_.

Both use the same `apply`. The handler stored inside the value
determines which interpreter fires.

#note[
  *How this works concretely:* a hypothesis is
  `wait(hyp_reduce)(metadata)`. When applied to a value `v`, the
  `wait` rule reduces it to `hyp_reduce(metadata)(v)` --- the type
  interpreter fires via normal tree calculus reduction. No special
  mechanism, just `wait`.
]

The type interpreter --- `hyp_reduce` --- reads the hypothesis's
stored type, checks if it is a Pi, and computes the result type from
the codomain:

```
  hyp_reduce(meta, v):
    my_type = neutral_meta_type(meta)  // first metadata field
    if my_type is Pi:
      result_type = codFn(v)           // instantiate codomain at arg
    else:
      result_type = InvalidType        // not a function type
    → wait(hyp_reduce)(extend_neutral_meta(meta, result_type, v))
```

That first metadata field is the invariant shared by all neutral
constructors. `Hyp(A, id)` stores `A` there at creation time.
Neutral application preserves the invariant by constructing the next
neutral with the newly computed `result_type` in the same position.
The payload of that next neutral is `fork(old_meta, v)`: an application
spine node that preserves identity. It is not used for O(1) type
extraction, but it prevents different stuck applications with the same
type, such as `f(0)` and `f(1)`, from collapsing to the same tree.

The result is another `wait(hyp_reduce)(...)` --- another hypothesis
--- whose handler is the same `hyp_reduce`. Apply it again, the type
interpreter fires again, tracking the next type. Each application
reduces fully, but the result is always symbolic.

Walking through `h_f(h_x)`:
+ `hyp_reduce` reads `h_f`'s stored type: `Nat → Nat` (a Pi type).
+ Extracts the codomain function: `{_} -> Nat`.
+ Instantiates: `({_} -> Nat)(h_x) = Nat`.
+ Produces a new hypothesis with stored type `Nat`.

When the Pi checker later inspects the result, the type is already
there --- `neutral_type(result) = Nat`, O(1). No spine walking, no
context lookup. The same application that would have been "stuck" in
a traditional system ran the type interpreter instead, and the type
was computed as a side effect of evaluation.

This is the Disp equivalent of bidirectional inference: instead of a
context mapping variables to types and a recursive `infer` judgment,
types are embedded in values and maintained by `hyp_reduce` at each
application.

#note[
  In #raw("lib/kernel.disp"), the type interpreter is
  `q_hyp_reduce_fn`, exposed through the kernel record's `hyp_reduce`
  field. Values produced by either interpreter are called *neutrals* ---
  they share `hyp_reduce`'s signature, which is how `is_neutral`
  recognizes them (see @encoding).
]

== Running codomains on neutral arguments

Now we can say more precisely what happens inside `B(h)` when `h` is a
hypothesis. `B` still reduces to a single value. The question is what
kind of value it computes.

+ If `B` merely embeds `h` in a type, such as `Eq(Nat, h, zero)`, then
  the result is concrete type structure whose metadata contains the
  neutral `h`.
+ If `B` applies `h` as a function, then `hyp_reduce` fires. If `h`'s
  stored type is Pi, the application returns a new neutral whose stored
  type is the Pi codomain instantiated at the argument.

For example:

```
  B = {f} -> Eq Nat (f zero) zero
```

If `f` is a neutral of type `Nat -> Nat`, then `f zero` invokes
`hyp_reduce`, computes result type `Nat`, and returns a neutral carrying
that type. So `B(f)` reduces to a concrete `Eq` type whose left-hand
side is a neutral Nat.

There is one more case: `B` may need to eliminate a neutral argument,
for example by computing a return type with `nat_rec`. That is handled
by typed eliminators, introduced later. The short version is that raw
tree pattern matching on a neutral is invalid; typed eliminators detect
the neutral and return a typed neutral result instead.

== Checking the result

With every neutral knowing its type --- whether from creation or from
the type interpreter --- the Pi checker has two cases:

*Concrete* (e.g., `{x} -> zero` applied to $h$ returns `zero`):\
Apply the expected type as a predicate: `B(h)(zero)`. If `TT`, accept.

*Neutral* (produced by either interpreter --- doesn't matter which):\
Read the neutral's stored type via `neutral_type`, compare against the
expected type:

```
  fast_eq(neutral_type(result), expected_type)
```

The Pi checker doesn't care how the neutral got its type (creation or
hypothesis reduction). It just reads it and compares.

#note[
  If `expected` is itself a neutral type, the neutral-result case still
  works by equality of stored types. The concrete-result case is more
  limited: applying an unknown neutral type to a concrete value is just
  another neutral application, not an inspection procedure for that
  unknown type. In practice, codomains that get stuck are most useful
  when the checked result is also neutral.
]

#note[
  The kernel bundles `is_neutral` + type extraction into a shared
  helper called `infer(check_fn, v)`, which passes the stored type
  to `check_fn` (or returns `FF` if `v` is not neutral). This is a
  code-sharing convenience, not a separate concept --- it avoids
  repeating the neutral-detection branch at each call site.
]

= Encoding types as trees <encoding>

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
another. Neutrals (which are `wait(hyp_reduce)(...)`) share the
`hyp_reduce` signature.

Recognition uses `fast_eq` on signatures:

```
  is_neutral(x) = fast_eq(pair_fst(x), hyp_reduce_signature)
  is_pi(x)      = fast_eq(pair_fst(x), pi_signature)
```

Each check is O(1) via hash-consing.

== Neutrals as trees

A neutral is `wait(hyp_reduce)(metadata)`, using the same `wait`
encoding as types. The metadata stores the neutral's type and its
identity (hypothesis id, eliminator payload, or application spine).

The implementation names this layout instead of scattering raw
`fork` projections through the kernel:

```disp
  make_neutral_meta(current_type, payload) = fork(current_type, payload)
  neutral_meta_type(meta)                  = pair_fst(meta)
  neutral_meta_payload(meta)               = pair_snd(meta)

  extend_neutral_meta(old_meta, result_type, arg) =
    make_neutral_meta(result_type, fork(old_meta, arg))
```

This is a tree today, but conceptually it is an annotated application
spine. The annotation caches the current type. The payload preserves
identity: a hypothesis stores its id, and an application stores the
previous metadata plus the new argument.

Applying a neutral to a value `v` triggers the `wait` rule:

```
  wait(hyp_reduce)(metadata)(v)  =  hyp_reduce(metadata)(v)
```

This _does_ reduce --- tree calculus has no irreducible terms.
`hyp_reduce` fires, computes the result type (see hypothesis
reduction above), and wraps the result back into
`wait(hyp_reduce)(new_metadata)` --- another neutral. `hyp_reduce` is
a *trampoline*: every application of a neutral fully reduces, but the
result is always another neutral. Concrete data never comes out.

This is why `is_neutral` works: all neutrals, whether hypotheses or
applied neutrals, share `hyp_reduce`'s signature. And it is why typed
eliminators must check `is_neutral` before pattern-matching --- raw
`triage` on a `wait(hyp_reduce)(...)` tree would interpret the
neutral's internal structure as data, producing garbage.

= The bootstrapping problem

We now have all the pieces: types are `wait(checker)(metadata)`,
signatures identify checkers, `hyp_reduce` tracks types through
applications. But there is a circular dependency.

The Pi checker needs to call `is_neutral`, which compares against
`hyp_reduce`'s signature. The Type checker needs `is_pi` and `is_eq`,
which compare against _their_ signatures. `hyp_reduce` itself needs
`is_pi` to decide whether a hypothesis's type is a function type.

Signatures are derived from the checkers: `pair_fst(wait(checker)(meta))
= stem(checker)`. So to know the Pi signature, you need the Pi checker.
To write the Pi checker, you need the `hyp_reduce` signature. To write
`hyp_reduce`, you need the Pi signature. Everything depends on
everything else.

== The kernel: one mutual fixed point

The solution is to put everything into a single `fix`:

```disp
  kernel : {hyp_reduce, pi, nat, bool, eq, type} = recq {
    hyp_reduce := q_hyp_reduce_fn
    pi := q_pi_fn
    nat := q_nat_fn
    bool := q_bool_fn
    eq := q_eq_fn
    type := q_type_fn
  }
```

The record is still Church-encoded: field projection supplies a
selector to the record. `recq` ties the recursive knot and passes each
component three arguments:

- `ks`, a lazy self proxy where `ks(selector) = wait(kernel)(selector)`.
  This lets components write ordinary projections like `ks.pi` without
  forcing recursive field selection too early.
- `raw`, the actual recursive record, used where handler identity
  matters (notably `hyp_reduce`).
- `query`, the selector that picked the current component.

Selection is O(1) and remains tree-level; `recq` is a tree-combinator,
not a parser macro.

== Constructing types

Types are built with *double wait*:

```disp
  kernel_ref = {q} -> wait kernel q

  Nat  = wait(kernel_ref.nat)(leaf)
  Bool = wait(kernel_ref.bool)(leaf)
  Pi(A, B) = wait(kernel_ref.pi)(fork(A, B))
```

`kernel_ref.nat` is `wait(kernel)(nat_selector)`, so it is inert ---
it does not evaluate `kernel(nat_selector)` eagerly. The full evaluation
happens only when the type is applied to a value:
`Nat(v) = kernel(nat_selector)(leaf)(v) = nat_checker(leaf)(v)`.

This deferred construction is essential: it means defining `Nat` at
module level costs ~2 reduction steps, not thousands.

== The H-rule, concretely

Inside the kernel, the H-rule is a shared function used by all four
type checkers (Pi, Nat, Bool, Eq):

```disp
  q_h_rule_fn = {ks, raw, query, self, meta, v} ->
    fast_eq (wait (ks query) meta) (neutral_type v)
```

The caller already confirmed `v` is neutral, so the H-rule just:
- Reconstructs _this checker's own type_ via
  `wait(ks(query))(meta)`.
- Reads `v`'s stored type via `neutral_type(v)`.
- Compares. If they match, `fast_eq` returns `TT`. Otherwise `FF`.

= Soundness

Bundling everything into one fixed point isn't just a convenience ---
it is what makes the type system sound. The kernel is the single
source of truth for all checker functions, and a type's identity is
determined by its checker. This makes types unforgeable.

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
`Nat` is the tree `wait(kernel_ref.nat)(leaf)` --- a specific,
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
time. Hypothesis reduction propagates types faithfully --- it reads the existing
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

Metadata = `make_pi_meta(domain, codFn)`, represented as
`fork(domain, codFn)`.

```
  Pi(A, B) = wait(pi_checker)(make_pi_meta(A, B))
  pi_checker(meta, v):
    if is_neutral(v): H-rule
    let hyp = Hyp(pi_meta_domain(meta), meta)   // meta itself is identity
    let result = v(hyp)
    let expected = pi_meta_cod_fn(meta)(hyp)
    if is_neutral(result):
      fast_eq(neutral_type(result), expected)
    else:
      fast_eq(expected(result), TT)    // apply type as predicate
```

The hypothesis identity is the Pi type's own metadata. Nested Pi checks
always produce distinct hypotheses because the inner Pi (produced by
evaluating the outer's codomain function) has structurally different
metadata. This eliminates the need for an external depth counter while
preserving soundness --- two hypotheses collide only if they come from
the same Pi type, which cannot happen in nested checking.

Arrow sugar: `Arrow(A, B) = Pi(A, {_} -> B)`.

== Eq

Metadata = `make_eq_meta(A, x, y)`, represented today as
`fork(A, fork(x, y))`. Sole constructor: `refl = leaf`.

```
  Eq(A, x, y) = wait(eq_checker)(make_eq_meta(A, x, y))
  eq_checker(meta, p):
    if is_neutral(p): H-rule
    if p = leaf: fast_eq(eq_meta_lhs(meta), eq_meta_rhs(meta))
    else: FF
```

== Type n (universes)

Metadata = `rank` (a natural number).

```
  Type(n) = wait(type_checker)(n)
  type_checker(meta, x):
    if is_neutral(x): univ_check(n, neutral_type(x))  // cumulative
    if is_universe(x): nat_lt(rank(x), n)        // Type m, m < n
    if is_pi(x):
      and(self(n, pi_dom(x)),
          self(n, codFn(Hyp(pi_dom(x), pi_meta(x)))))
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
`motive(target)` as the result type, so `neutral_type` can read it.

This is where dependent codomains that branch on neutral arguments get
their type. If `B = {n} -> nat_rec(motive, base, step, n)` and `n` is a
hypothesis, `B(n)` cannot choose the zero or successor branch. Instead,
`nat_rec` returns `StuckElim(motive(n), n)`: a neutral value whose
stored type is the motive instantiated at the neutral scrutinee. If the
motive returns a universe, that stuck value is itself a neutral type.

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
  (Arrow Nat Nat)({x} -> x)
    pi_checker(fork(Nat, {_}->Nat), {x}->x)
    is_neutral? no → concrete check
    h = Hyp(Nat, meta)          // meta = fork(Nat, {_}->Nat)
    result = ({x}->x)(h) = h   // apply identity to h
    expected = ({_}->Nat)(h) = Nat
    is_neutral(h)? yes
      neutral_type(h) = Nat     // stored when h was created
      fast_eq(Nat, Nat) = TT ✓
```

The H-rule fires because `h` was introduced at type `Nat`, and the
expected codomain is `Nat`.

== `{f, x} -> f(x)` checked against `(Nat → Nat) → Nat → Nat`

This example shows hypothesis reduction computing a result type.

```
  Outer Pi: domain = Nat → Nat, codomain = {_} -> Nat → Nat
    meta_outer = fork(Nat→Nat, {_}->Nat→Nat)
    h_f = Hyp(Nat→Nat, meta_outer)   // stored type: Nat → Nat
    result = ({f,x}->f(x))(h_f)
           = {x} -> h_f(x)           // a lambda — concrete
    expected = Nat → Nat
    is_neutral? no → check {x}->h_f(x) against Nat → Nat

  Inner Pi: domain = Nat, codomain = {_} -> Nat
    meta_inner = fork(Nat, {_}->Nat)  // different from meta_outer!
    h_x = Hyp(Nat, meta_inner)       // stored type: Nat
    result = ({x}->h_f(x))(h_x)
           = h_f(h_x)                // applying neutral to neutral!
    hyp_reduce fires:
      h_f has type Nat → Nat (a Pi type)
      codomain function = {_} -> Nat
      result_type = ({_}->Nat)(h_x) = Nat
    → new neutral with stored type Nat
    expected = ({_}->Nat)(h_x) = Nat
    is_neutral? yes
      neutral_type(result) = Nat
      fast_eq(Nat, Nat) = TT ✓
```

The type was computed at application time by `hyp_reduce`, not
inferred after the fact. Note that `meta_outer ≠ meta_inner` because
the codomain functions differ --- this guarantees `h_f ≠ h_x` even
though both have domain `Nat`.

== Self-application rejection: `{x} -> x x`

Checked against `(A : Type 0) → A → A`:

```
  Pi checks A : Type 0:
    meta_A = fork(Type0, {A}->Arrow(A,A))
    h_A = Hyp(Type0, meta_A)
  Pi checks x : h_A:
    meta_x = fork(h_A, {_}->h_A)     // codFn applied to h_A → h_A
    h_x = Hyp(h_A, meta_x)           // type is a hypothesis!
  Body: h_x(h_x)
    hyp_reduce fires on h_x(h_x):
      h_x has type h_A
      is h_A a Pi type? no (it's a neutral, not a Pi)
      → result_type = InvalidType
    → neutral with stored type InvalidType
  Pi checks result:
    expected = h_A
    neutral_type(result) = InvalidType
    fast_eq(h_A, InvalidType) = FF    // h_A ≠ InvalidType
    → rejected ✓
```

Self-application is blocked because `h_A` is an opaque hypothesis,
not a Pi type. Hypothesis reduction stores `InvalidType`, and the
codomain check fails. No special-case logic needed.

= Glossary

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Term*][*Meaning*],
  [`T(v) = TT`], [Type checking. Apply type to value; raw tree-calculus application.],
  [`wait(a)(b)`], [Inert tree. `wait(a)(b)(c) = a(b)(c)`. See #raw("KERNEL_DESIGN.md").],
  [`fix(f)`], [Fixed-point via `wait`. `fix(f)(x) = f(fix(f))(x)`.],
  [`Hyp(type, id)`], [Create a neutral hypothesis carrying the given type.],
  [`StuckElim(type, target)`], [Stuck eliminator, produced by typed eliminators on neutral scrutinees.],
  [`neutral_type(v)`], [Read a neutral's stored type. `neutral_meta_type(type_meta(v))`. O(1).],
  [`InvalidType`], [Sentinel stored as the result type of invalid neutral application. Rejected by `Type n`; currently represented by `FF`.],
  [`is_neutral`], [Signature check: does `v` share `hyp_reduce`'s signature? O(1).],
  [*hypothesis reduction*],
  [A second interpreter embedded in reduction. When a neutral is applied, `hyp_reduce` runs instead of normal evaluation, computing the result type from the codomain. Disp's equivalent of bidirectional inference, distributed across evaluation.],

  [`infer(f, v)`],
  [Kernel helper. Bundles `is_neutral` check + `neutral_type` extraction + callback. Convenience, not a primitive.],

  [`H-rule`], [Each checker's first branch: if `v` is neutral and its stored type matches this type, accept.],
  [`signature`], [`pair_fst` of a `wait`-encoded value. Constant per checker; used for recognition.],
  [`kernel`], [Single mutual `fix` containing all checkers and `hyp_reduce`.],
  [*typed eliminator*],
  [Neutral-aware recursor. Checks `is_neutral` before dispatching; freezes as `StuckElim` when stuck.],

  [`TT` / `FF`], [Booleans. `TT = leaf`, `FF = stem(leaf)`.],
  [`refl`], [Sole constructor of `Eq A x y`. Equals `leaf`.],
  [*select-then-apply*], [Compilation pattern for deferred branching. See #raw("KERNEL_DESIGN.md").],
)

= Open problems

== B/C combinators

The bracket abstraction uses S, K, I with η-reduction and
K-composition. Turner's B and C combinators would reduce tree sizes
for functions with 4+ variables. This is a compilation improvement,
not a semantic change.

== Elaborator

The elaborator (AST → typed terms, motive inference) is the remaining
frontier. All kernel operations are tree programs; the elaborator
would orchestrate them. Since Pi types derive hypothesis identity from
their own metadata (no external depth counter needed), the elaborator
need not thread binder-depth state --- it only needs to compile type
expressions into kernel predicates and invoke them.
