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
  Authoritative reference for the Disp object-language semantics.\
  Implementation status: #raw("CLAUDE.md").
]
#v(1em)

#note[
  *Status.* Spec. Captures the kernel design where:

  - Inductive types are library-defined via a generic `predicate_frame`
    primitive that wraps user-supplied recognizers.
  - Quantifier types (Pi, Forall, etc.) are library-defined using
    `bind_hyp` for fresh-hypothesis minting during type-checking.
  - Inductive eliminators are library-defined using `eliminator_frame`,
    which mints stuck-typed neutrals for hypothesis targets.
  - The kernel has seven primitive handlers; everything else is library
    code.
  - Logical consistency at the public boundary follows from the
    definition of `False` (the predicate `\{_\} -> FF`), not from
    universe stratification.

  bind_hyp's body may return values of any type; soundness is
  preserved by an escape check that rejects results where the minted
  hypothesis is reachable via a non-neutral path. See §4.6 for the
  bind_hyp specification and §9 for the soundness argument.

  The current implementation does NOT yet match this spec (it has
  per-type kernel handlers per the previous data-as-eliminator
  design). The migration plan is in §10. Where the implementation
  and this document disagree, the document is authoritative and
  the implementation is in transition.
]

= Overview

Disp is a dependently-typed language built on tree calculus. Types are
predicates over trees. The kernel is a small set of primitive handlers
that mediate parametric-mode evaluation, hypothesis minting, and the
public boundary between trusted and untrusted code. All inductive
types, quantifier types, and universe machinery live in the library.

Five commitments govern the design:

+ *Types are predicates.* A type is a tree-calculus function from
  trees to `Bool`. Type checking is function application:
  `T(v) = TT` means "v inhabits T."

+ *Hypotheses carry their types.* During checking under a binder, a
  fresh kernel-minted neutral represents "any value of T." Its stored
  type is T. The H-rule compares stored types via hash-cons identity.

+ *Parametric checked evaluation.* User code runs under a non-standard
  reduction discipline (the walker) that forbids reflection on
  hypotheses. Triage on a hypothesis fails; fork-formation with the
  kernel's hypothesis signature fails.

+ *Generic library type-formers.* Inductive types declare a recognizer
  (`Tree -> Bool` predicate body) and optional metadata (rank function,
  codomain function). The kernel's `predicate_frame` wraps the
  recognizer with H-rule handling for hypothesis inputs. The kernel
  doesn't grow with library types.

+ *Soundness via three runtime mechanisms, plus False's definition.*
  Public-boundary scan, stem-rule fork rejection, and walker triage
  rejection enforce that hypotheses don't escape into closed proofs.
  False's predicate `\{_\} -> FF` ensures it has no closed inhabitants.
  Together these give logical consistency at the public boundary.

== Soundness, not completeness

The kernel guarantees: *if `T(v) = TT` evaluates at the public boundary,
then v inhabits T per T's predicate*. The kernel does not guarantee:

- Termination of user predicates (apply budget catches divergence;
  divergence does not synthesize TT).
- Semantic correctness of user-supplied recognizers, rank functions,
  or case dispatchers (the kernel runs them as-is).
- Detection of paradoxical type definitions (Russell, Girard, etc.).

Predicates that fail to terminate within budget produce no answer
(effectively FF at the public boundary). This is sufficient to prevent
contradictions: a paradoxical predicate that diverges does not
synthesize TT for anything; in particular, not for `False`.

Tooling is responsible for surfacing budget exhaustion as a diagnostic
to library authors.

= Framework

== Two reduction modes

The runtime's `apply` operates in one of two modes per application,
selected by the dispatcher:

- *Raw mode.* Standard tree-calculus reduction. Triage may match any
  tree, including kernel-minted neutrals. Used for trusted code: kernel
  handler bodies, internal metadata operations.

- *Parametric mode* (the *walker*). Tree-calculus reduction with two
  restrictions:
  + *Triage on a neutral fails.* If the value being triaged is a
    kernel-minted hypothesis, the application returns `Fail`.
  + *Fork-formation rejecting neutral roots.* If the stem rule would
    produce a fork whose left component matches the canonical
    hypothesis-handler signature, the application returns `Fail`.

Parametric mode encodes operational parametricity: hypotheses are
opaque tokens for universal quantification, and user code cannot
inspect their structure or synthesize new hypotheses.

== The dispatcher

`checked_apply` is the kernel's dispatcher. For each application, it
checks the function's signature against a fixed set of registered
handler signatures:

```
hyp_reduce, guard, unguard, predicate_frame, bind_hyp, eliminator_frame
```

If matched, the application is routed to the corresponding kernel
handler in raw mode. If not matched, the parametric walker runs. The
walker invokes recursive `checked_apply` calls, ensuring user code
inherits parametric mode throughout its evaluation.

== Trusted primitives

The kernel registry consists of seven handlers, fixed at boot time:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Primitive*], [*Role*], [*Mode*],
    [`hyp_reduce`],     [`apply` on a kernel-minted neutral],            [raw],
    [`guard`],          [public-boundary scan + predicate evaluation],   [raw],
    [`unguard`],        [walker-safe peeling of guard layer],            [raw],
    [`checked_apply`],  [dispatcher (signatures + walker default)],      [raw],
    [`predicate_frame`],[wraps recognizer with H-rule for hypotheses],   [raw],
    [`bind_hyp`],       [mints fresh hypothesis for predicate checking], [raw],
    [`eliminator_frame`],[wraps case dispatcher with stuck-mint],        [raw],
  ),
  caption: [Kernel primitive handlers. The kernel does not grow with library type-formers; these seven mediate all type-system operations.],
)

#note[
  *Pi is library, not kernel.* The current implementation has a kernel
  `q_pi_fn`. Under the unified design, `Pi A B` is a library wait-form
  using `predicate_frame` + `bind_hyp`. The kernel is unaware of Pi
  specifically; it sees a generic predicate_frame application with
  a Pi-shaped recognizer in metadata.
]

= Library type-former protocol

== Metadata convention

Every library type-former T has the structural form:

```
T_params := guard (wait kernel_ref.predicate_frame T_meta)
```

with `T_meta` a four-tuple:

```
T_meta := pair recognizer_sig
              (pair params
                    (pair codomain_fn
                          rank_fn))
```

Slots:
- *recognizer_sig.* Hash-cons-stable signature of the type-former's
  recognizer function. Used by `Type k`'s recognizer (and any other
  consumer that dispatches on type-former identity).
- *params.* Type-former-specific data. For Pi: `pair A B`. For Eq:
  `make_eq_meta A x y`. For parameterless types (Bool, Nat, Type k):
  the constituent fields directly.
- *codomain_fn.* For Pi-like applicable types, a function
  `arg -> new_stored_type`. For non-applicable types (Bool, Nat, Eq,
  Type, False, etc.), the LEAF sentinel `t`.
- *rank_fn.* A function returning the type-former's universe rank as
  Ord. Used by `Type k`'s recognizer for rank-based universe checks.

== Hash-cons stability

Every component of `T_meta` must be a deterministic function of T's
parameters. No fresh IDs, no time-varying state. This ensures
`predicate_frame`'s H-rule reconstruction (`wait (ks query) meta`)
produces hash-cons-identical trees to the original type T.

== Recognizer protocol

A type-former T's recognizer is a tree-calculus function:

```
T_recognizer : params -> value -> Bool
```

Invoked by `predicate_frame` for non-hypothesis values. Runs under
parametric mode (via `ks.checked_apply` from inside predicate_frame).

The recognizer may:
- Triage on the value's outer shape (the value is closed at the
  recognizer's input — predicate_frame routed neutrals to H-rule
  before calling).
- Extract subterms via `pair_fst`, `pair_snd`, etc.
- Recurse via type predicates on subterms (these go through
  predicate_frame's H-rule for hypothesis subterms).
- Compose via `and`, `or`, `select`, `match`.

The recognizer must NOT:
- Triage on hypothesis-rooted subterms directly (walker rejects).
- Construct forks with the kernel's hypothesis signature (stem-rule
  rejects).

The recognizer's result is a Bool (TT, FF, or stuck-Bool).

= Per-primitive specifications

== `hyp_reduce`

When `apply(neutral, x)` evaluates, the dispatcher routes to
`hyp_reduce`. The handler reads the neutral's stored type metadata,
extracts the codomain function from the type's `codomain_fn` slot, and
either extends the spine or returns `InvalidType`:

```
q_hyp_reduce_fn := \{ks, raw, query\} -> fix (\{self, meta, v\} -> \{
  let stored = q_unguard_or_self ks (neutral_meta_type meta)
  let cod_fn = pair_fst (pair_snd (pair_snd (type_meta stored)))
  match (tree_eq cod_fn t) \{
    TT => wait self (extend_neutral_meta meta InvalidType v)
    FF => wait self (extend_neutral_meta meta
                       (wait kernel_ref.guard (cod_fn v)) v)
  \}
\})
```

The codomain_fn slot convention is part of the library type-former
protocol (§3.1).

== `guard`

Public-boundary entry scan plus predicate evaluation. Returns bare
TT/FF.

```
q_guard_fn := \{ks, raw, query\} -> \{core, v\} ->
  match (q_scan_no_neutral raw v) \{
    TT => match (tree_eq (core v) TT) \{ TT => TT; FF => FF \}
    FF => FF
  \}
```

The scan rejects pre-existing neutrals; the inner check evaluates
`core` (the type's predicate function) on `v` and folds to bare TT/FF.

== `unguard`

Walker-safe peeling of the guard layer. Routed through the dispatcher
so the body's triage on T runs in raw mode rather than under the
walker (which would reject triage on a hypothesis-typed T).

== `checked_apply`

Dispatcher. Routes by signature recognition; falls through to the
parametric walker for unrecognized signatures.

== `predicate_frame`

Wraps a user-supplied recognizer with H-rule for hypothesis inputs:

```
q_predicate_frame_fn := \{ks, raw, query\} -> \{meta, v\} -> \{
  match (q_is_neutral raw v) \{
    TT =>
      // H-rule: reconstruct self-as-type, compare to v's stored type
      let self_type = wait (ks query) meta
      tree_eq (q_unguard_or_self ks (neutral_meta_type (type_meta v)))
              self_type
    FF =>
      // Run recognizer under parametric mode
      let recognizer = pair_fst meta
      let params = pair_fst (pair_snd meta)
      let result_r = ks.checked_apply (recognizer params) v
      must_ok_or_ff result_r (\{result\} -> result)
  \}
\}
```

The H-rule reconstruction relies on hash-cons stability of metadata
(§3.2).

== `bind_hyp`

Mints a fresh kernel-minted neutral for type-checking. The body may
return any value; the kernel performs an *escape check* on the
result: if the minted hypothesis is reachable in the result via a
non-neutral path, the bind_hyp call returns `Fail`. This prevents
the hypothesis from escaping into a context where it could be
exploited via predicate_frame's H-rule.

```
q_bind_hyp_fn := \{ks, raw, query\} -> \{meta, x\} -> \{
  // Standard 2-ary arity-tracked accumulator: first arg is domain,
  // second arg is body. On second application, execute.
  let count = pair_fst meta
  let acc = pair_snd meta
  let final_fn = ... -> \{
    let domain = pair_snd acc
    let body = x
    let hyp_id = t domain body
    let hyp = q_make_hyp raw domain hyp_id
    let result_r = ks.checked_apply body hyp
    must_ok_or_ff result_r (\{result\} ->
      match (q_contains_via_open_path raw result hyp) \{
        TT => Fail              // hyp escaped through a non-neutral path
        FF => result             // hyp didn't escape — return body's value
      \})
  \}
  let partial_fn = ... // accumulator for partial application
  select final_fn partial_fn (tree_eq count (t t t))
    raw query ks meta x count acc
\}
```

The escape check `q_contains_via_open_path` walks the result tree and
returns TT if `hyp` is reachable at any position whose ancestors are
*not* themselves kernel-minted neutrals. Concretely:

```
q_contains_via_open_path := fix (\{self, raw, result, hyp\} ->
  match (tree_eq result hyp) \{
    TT => TT                                      // direct match
    FF => match (q_is_neutral raw result) \{
      TT => FF                                    // walled off inside a neutral
      FF => match (is_fork result) \{
        TT => or (self raw (pair_fst result) hyp)
                  (self raw (pair_snd result) hyp)
        FF => match (is_stem result) \{
          TT => self raw (stem_child result) hyp
          FF => FF                                 // leaf, no match
        \}
      \}
    \}
  \})
```

The check stops at neutrals: if the result is a kernel-minted
neutral (e.g., a stuck-Bool, a stuck-Ord, or another hypothesis's
spine extension with our hyp buried in its metadata), the hypothesis
is walled off — user code under parametric mode cannot extract it
because triage on neutrals is rejected by the walker.

#note[
  *Why the escape check is sound.* The H-rule lie occurs when a
  user obtains a hypothesis `h` whose stored type is some
  uninhabited `T`, then applies `T` (the predicate) to `h` —
  predicate_frame's H-rule fires, says "stored = T, T = T, TT,"
  but T has no actual inhabitants.

  For this lie to propagate beyond bind_hyp, the user must be able
  to *extract* `h` from bind_hyp's return value and apply `T` to
  it. The escape check forbids exactly this: if `h` is reachable
  in the return tree via paths that user code (under parametric
  mode) can traverse, bind_hyp returns Fail instead.

  Paths user code can traverse: `pair_fst`, `pair_snd`, `triage`'s
  on_fork branch — all on non-neutral fork-shaped values. Walker
  rejects these on neutral-rooted values, so paths through neutrals
  are walled off. The escape check matches this exactly: walk
  result via projections, but stop at neutrals.

  Body returns containing `hyp` only inside neutral wrappers
  (stuck-Bool from a kernel comparison, stuck-Ord from a polymorphic
  rank computation, an extended Pi-spine from `apply hyp x`) are
  accepted, because the hyp is not user-extractable from those
  positions.
]

#note[
  *What this enables.* bind_hyp's body can return any value of any
  type, including `Bool`, `Ord`, `Nat`, etc. Pi's `rank_fn` can
  compute polymorphic codomain rank by `bind_hyp A (\{h\} -> rank_of (B h))`
  — the result is an Ord (possibly stuck-Ord containing `h` inside
  a neutral wrapper), which the escape check accepts.

  The earlier "body returns Bool" restriction was a workable
  approximation but was both too strict (rejected legitimate
  Ord-returning rank computations) and too loose (relied on caller
  patterns rather than enforcing safety at the kernel level). The
  escape check is the more precise property.
]

== `eliminator_frame`

Wraps a case dispatcher with stuck-mint for hypothesis targets. This
is the kernel primitive that *does* allow a hypothesis to escape with
a derived stored type — but only inside a controlled eliminator
pattern.

```
q_eliminator_frame_fn := \{ks, raw, query\} -> \{meta, x\} -> \{
  // Arity-tracked accumulator over (motive, cases..., target).
  let count = pair_fst meta
  let acc = pair_snd meta
  let final_fn = \{...\} -> \{
    let dispatcher_sig = pair_snd acc
    let dispatcher = recover_dispatcher dispatcher_sig
    let motive = ... // extract from acc
    let cases = ...  // extract from acc
    let target = x
    select_lazy
      (\{_\} -> q_make_hyp raw (motive target) target)
      (\{_\} -> ks.checked_apply
                 (dispatcher motive cases) target)
      (q_is_neutral raw target)
  \}
  let partial_fn = ... // accumulator for partial application
  ...
\}
```

The dispatcher's invocation goes through `ks.checked_apply` (parametric
mode) so user-supplied case bodies cannot raw-triage on
hypothesis-typed subterms of the closed target.

#note[
  *Library invariant for inductive type authors.* When writing a
  `T_dispatcher`, you may extract subterms from the closed target and
  pass them to user-supplied case handlers. Those case handlers will
  run under parametric mode. Use type predicates and function
  applications on subterms — *do not* raw-triage on them. Subterms
  that are hypothesis-typed are handled correctly by predicates
  (via H-rule); raw triage on them would be rejected by the walker.

  This is a soundness obligation that didn't exist in the previous
  per-type kernel-handler design (where eliminator handlers ran fully
  raw and could triage freely).
]

= Worked library types

== Pi

```
let pi_recognizer = \{params, v\} -> \{
  let A = pair_fst params
  let B = pair_snd params
  bind_hyp A (\{hyp\} ->
    let result = v hyp
    let expected_core = unguard_checked (B hyp)
    expected_core result)   // returns Bool from expected's predicate
\}
let pi_recognizer_sig = checker_sig pi_recognizer

Pi := \{A, B\} -> guard (wait kernel_ref.predicate_frame
  (pair pi_recognizer_sig
        (pair (pair A B)
              (pair (\{arg\} -> B arg)            // codomain_fn = B
                    (pi_rank_fn (pair A B))))))   // rank_fn (see §9)
```

The codomain_fn slot lets `hyp_reduce` extend a Pi-typed hypothesis's
spine when it's applied: `apply(Hyp (Pi A B) id, x)` reads `B` from
metadata and computes the new stored type as `B x`.

The rank_fn computes Pi's universe rank, recursing through the
codomain via `bind_hyp` (now sound thanks to the escape check):

```
let pi_rank_fn = \{params\} ->
  let A = pair_fst params
  let B = pair_snd params
  ord_max (rank_of A)
          (bind_hyp A (\{h\} -> rank_of (B h)))
```

== Bool

```
let bool_recognizer = \{_, v\} ->
  or (tree_eq v TT) (tree_eq v FF)
let bool_dispatcher = \{motive, cases, target\} ->
  let ct = pair_fst cases
  let cf = pair_snd cases
  select ct cf target

Bool := guard (wait kernel_ref.predicate_frame
  (pair bool_recognizer_sig
        (pair t                             // no params
              (pair t                       // codomain_fn = sentinel
                    (\{_\} -> zero_ord))))) // rank_fn = const 0

bool_rec := wait kernel_ref.eliminator_frame
  (init_meta_arity_4 bool_dispatcher_sig)
```

Bool's case handlers (`ct`, `cf`) are returned as values, not invoked.
The dispatcher just selects between them. No invocation, no
parametric-mode concerns inside the dispatcher.

== Nat

```
let nat_recognizer = fix (\{self, _, v\} ->
  triage TT
    (\{_\} -> FF)
    (\{l, r\} -> and (is_leaf l) (Nat r))
    v)

let nat_dispatcher = fix (\{self, motive, cases, target\} -> \{
  let base = pair_fst cases
  let step = pair_snd cases
  match (tree_eq target t) \{
    TT => base
    FF =>
      let k = pair_snd target
      let ih = self motive cases k
      step k ih   // user-supplied step body, runs under walker
  \}
\})

Nat := guard (wait kernel_ref.predicate_frame
  (pair nat_recognizer_sig
        (pair t (pair t (\{_\} -> zero_ord)))))

nat_rec := wait kernel_ref.eliminator_frame
  (init_meta_arity_4 nat_dispatcher_sig)
```

Per the library invariant from §4.7: `step k ih` runs under parametric
mode. User-supplied step bodies must use `k` and `ih` via type
predicates or function applications — not raw triage.

== Eq

```
let eq_recognizer = \{params, v\} -> \{
  let A = eq_meta_type params
  let x = eq_meta_lhs params
  let y = eq_meta_rhs params
  and (tree_eq v refl) (tree_eq x y)
\}

Eq := \{A, x, y\} -> guard (wait kernel_ref.predicate_frame
  (pair eq_recognizer_sig
        (pair (make_eq_meta A x y)
              (pair t (\{_\} -> zero_ord)))))
```

== Type k (universe)

```
let type_k_recognizer = fix (\{self, k, v\} -> \{
  // Peel guard, dispatch on inner predicate_frame's recognizer_sig
  match (has_sig kernel_ref.guard v) \{
    TT => \{
      let inner = type_meta v
      match (has_sig kernel_ref.predicate_frame inner) \{
        TT => \{
          let inner_meta = type_meta inner
          let inner_rank_fn = pair_snd (pair_snd (pair_snd inner_meta))
          let inner_params = pair_fst (pair_snd inner_meta)
          let inner_rank = inner_rank_fn inner_params
          ord_lt inner_rank k
        \}
        FF => FF
      \}
    \}
    FF => FF
  \}
\})

Type := \{k\} -> guard (wait kernel_ref.predicate_frame
  (pair type_k_recognizer_sig
        (pair k
              (pair t
                    (\{_\} -> succ_ord k)))))
```

By reading rank_fn from the metadata, `Type k` doesn't need to enumerate
known type-formers. New library type-formers participate by declaring
their own rank_fn — no edit to `Type k`'s recognizer required.

== False

```
let false_recognizer = \{_, _\} -> FF

False := guard (wait kernel_ref.predicate_frame
  (pair false_recognizer_sig
        (pair t (pair t (\{_\} -> zero_ord)))))
```

`False v = FF` for all closed `v`. For hypothesis `v` with stored type
False, predicate_frame's H-rule returns TT (correctly representing a
hypothesis assumption). At the public boundary, hypothesis-tainted
values fail `scan_no_neutral`, so no closed proof of False can be
synthesized.

== Sigma, Refinement, Forall

```
let sigma_recognizer = \{params, v\} -> \{
  let A = pair_fst params
  let B = pair_snd params
  let a = pair_fst v
  let b = pair_snd v
  and (A a) ((B a) b)
\}

let refinement_recognizer = \{params, v\} -> \{
  let A = pair_fst params
  let P = pair_snd params
  and (A v) (P v)
\}

let forall_recognizer = \{params, v\} -> \{
  let A = pair_fst params
  let P = pair_snd params
  bind_hyp A (\{hyp\} ->
    (P hyp) v)   // proposition P at hyp: does v inhabit?
\}
```

Each gets the standard 4-tuple metadata wrapping. Sigma and Refinement
don't need bind_hyp (closed dispatch). Forall does (it quantifies over
A).

Forall's body returns `(P hyp) v` — a Bool resulting from applying
P-at-hyp's predicate to v. The escape check examines this Bool:
- If concrete (TT or FF), no hyp present, accept.
- If stuck-Bool (kernel-minted neutral whose metadata may reference
  hyp), the stuck wrapper walls off hyp, accept.
- If somehow the user's P or v made hyp reachable in a non-neutral
  position of the result, reject.

In practice, `(P hyp) v` produces Bool values that don't expose hyp
in user-extractable positions; legitimate Forall recognizers pass
the escape check.

= Soundness arguments

== Operational soundness

If `T(v) = TT` evaluates at the public boundary, then v inhabits T.
This rests on:

+ *Public-boundary scan* (`q_guard_fn`'s `q_scan_no_neutral`): rejects
  any pre-existing kernel-minted neutral in the user's value.
+ *Stem-rule constructor check* (in walker and apply): rejects
  fork-formation that would synthesize a forged hypothesis-shaped
  value.
+ *Walker triage rejection*: triage on a hypothesis-rooted value fails
  under parametric mode, preventing user code from inspecting a
  hypothesis's structure.

These mechanisms apply uniformly to user-defined recognizers,
dispatchers, and rank functions. All user code under
`predicate_frame` or `eliminator_frame` runs through `ks.checked_apply`,
which routes through the walker.

== Logical consistency at the public boundary

Logical consistency means: no closed value submitted at the public
boundary inhabits `False`.

This follows from three facts:

+ False's predicate is `\{_, _\} -> FF`. For any closed v, `False v = FF`.
+ Hypothesis-tainted values cannot pass the public-boundary scan.
+ bind_hyp's escape check prevents user code from constructing
  type-system-accepted values that quietly carry hypotheses past
  type-checking.

Therefore no value, however constructed, satisfies False at the public
boundary.

#note[
  *The escape check resolves the weirder attack.* Consider
  `weirder := \{x\} -> bind_hyp False (\{h\} -> h)` —
  a function that ignores its argument and tries to return a fresh
  False hypothesis.

  Inner bind_hyp's body returns `h` directly. The escape check on
  bind_hyp's result detects that the minted hyp `h` is reachable
  via a non-neutral path (in fact, `h` IS the result, modulo
  identity). The check returns TT (h escapes), so bind_hyp
  returns Fail.

  weirder thus evaluates to Fail at the body. Pi-check
  `Pi Bool (\{_\} -> False) weirder`: the recognizer's
  `expected_core (v hyp_b)` becomes `False Fail = FF`, so Pi-check
  rejects weirder. Soundness preserved.

  Without the escape check, weirder would be accepted at type-check
  via predicate_frame's H-rule firing on the escaped h_false. The
  type-system would claim weirder inhabits Pi Bool (\{_\} -> False),
  but the public-boundary scan on weirder(TT) would produce FF.
  That mismatch is what the escape check eliminates.
]

== Russell-style paradoxes

Russell-style self-referential predicates like `R := \{T\} -> not (T T)`
don't yield contradictions in Disp; they diverge.

`R R` evaluates `not (R R)`, which evaluates `not (not (R R))`, which
loops. The apply budget exhausts; the test fails. No TT is synthesized,
in particular not for `False`.

This is fundamentally different from classical type theories where
self-typing universes can be exploited (Girard's paradox) to derive
proofs of False. Disp's "type checking" is concrete tree-calculus
computation that either terminates with TT, terminates with FF, or
fails to terminate. Failure-to-terminate is observably distinct from
TT and is treated as failure at the public boundary.

= Resolved: bind_hyp with arbitrary return types via the escape check

An earlier draft of this spec restricted bind_hyp's body to return
`Bool`, motivated by the fact that the H-rule for `predicate_frame`
can lie when applied to a hypothesis-typed value of an uninhabited
type (the weirder attack). That restriction was both too strict
(rejected legitimate Ord-returning rank computations) and too loose
(was enforced only by caller convention, not at the kernel level).

The escape check (§4.6) resolves both problems by directly catching
the soundness-violating pattern: a hypothesis reachable via a
non-neutral path in bind_hyp's return value. Body return type is
unrestricted; the kernel rejects the precise constructions that
would propagate the H-rule lie.

== Pi's rank_fn (now expressible)

Pi's rank computation can use bind_hyp directly:

```
let pi_rank_fn = \{params\} ->
  let A = pair_fst params
  let B = pair_snd params
  ord_max (rank_of A)
          (bind_hyp A (\{h\} -> rank_of (B h)))
```

Tracing each case:

- *Closed A and closed B with closed codomain rank.* `B h` evaluates
  with hyp h substituted. For a constant codomain `B = \{_\} -> Type 0`,
  `B h = Type 0`, `rank_of (Type 0) = 1` (a closed Ord). bind_hyp's
  result = 1, which doesn't contain h. Escape check passes.
- *Closed A, polymorphic-rank codomain.* `B h` produces a type
  containing h. `rank_of (B h)` produces a stuck-Ord whose metadata
  references h, but the stuck-Ord itself is a kernel-minted neutral.
  Escape check stops at the neutral wrapper — h is walled off.
  bind_hyp accepts; the stuck-Ord propagates through `ord_max` as
  expected.
- *Polymorphic A.* Similar — h has stored type referencing some outer
  hypothesis-typed thing. The escape check still works structurally:
  it looks for THIS bind_hyp's hypothesis identity, which is fresh
  per call site.

Pi's complete library definition:

```
Pi := \{A, B\} -> guard (wait kernel_ref.predicate_frame
  (pair pi_recognizer_sig
        (pair (pair A B)
              (pair (\{arg\} -> B arg)            // codomain_fn
                    (pi_rank_fn (pair A B))))))   // rank_fn
```

== Why this is sound

The escape check ensures that:

+ Hypotheses minted by bind_hyp can be used inside body's evaluation
  (passed to type predicates, applied as functions, used in Ord
  computations) but cannot escape into bind_hyp's caller as
  user-extractable values.
+ Hypotheses can be embedded inside kernel-minted neutrals (stuck-Bool,
  stuck-Ord, extended Pi-spines from `apply hyp x`) without escape
  detection, because user code cannot extract them from those
  positions under parametric mode.
+ The H-rule lie (predicate_frame returning TT for an uninhabited
  type's hypothesis) cannot propagate beyond bind_hyp, because the
  bind_hyp'd hypothesis is exactly what the lie operates on, and
  the escape check rejects its propagation through user-extractable
  paths.

These three properties together close the soundness gap that the
Bool-restriction was an approximation of.

= Open issue: `Type k` and the rank_fn slot

`Type k`'s recognizer reads `rank_fn` from the type-former's metadata
and applies it to the params. This relies on every library
type-former conforming to the 4-tuple metadata convention.

If a future library type-former forgets to include `rank_fn`, or
includes one that lies (e.g., always returns 0), the consequences
are:

- *Forgotten `rank_fn`.* Reading the absent slot returns
  `t` (LEAF, the implicit default). `t` applied to params is — well,
  not well-defined. Either way, `ord_lt result k` produces some
  Bool/stuck. Worst case, the type is treated as "rank 0" or
  "indeterminate rank." Soundness preserved (no false TT for closed
  proofs of uninhabited types).
- *Lying `rank_fn`.* Universe-polymorphic code that quantifies over
  the lied-about rank may misbehave. But closed values still need
  their predicates to return TT; False's predicate doesn't lie.
  Soundness preserved.

The kernel does not enforce rank_fn correctness. Library authors are
responsible. Tooling can lint for missing rank_fn slots.

= Migration plan (informational)

The current implementation has per-type kernel handlers
(`q_pi_fn`, `q_bool_fn`, `q_nat_fn`, `q_eq_fn`,
`q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`) and corresponding
recq fields. Migrating to the unified design is a substantial refactor:

+ Add `q_predicate_frame_fn`, `q_bind_hyp_fn`, `q_eliminator_frame_fn`
  to the kernel record.
+ Drop the per-type handlers (`q_bool_fn`, `q_nat_fn`, `q_eq_fn`,
  `q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`,
  `q_pi_fn`).
+ Update `q_core_type_fn` / `q_guarded_type_fn` to use the rank_fn
  slot, OR move them to library code entirely.
+ Reimplement Pi, Bool, Nat, Eq, Type k, Ord as library types using
  predicate_frame/eliminator_frame.
+ Update test sites (currently ~120 disp tests) for new tree shapes
  where they assert against specific representations.
+ Implement the bind_hyp escape check (`q_contains_via_open_path`)
  per §4.6.

The migration is not undertaken in this document; this spec describes
the target end-state.
