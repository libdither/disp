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
    definition of `False` (the predicate `{_} -> FF`), not from
    universe stratification.

  bind_hyp's body may return values of any type; soundness is
  preserved by an escape check that rejects results where the minted
  hypothesis is reachable via a non-neutral path. See §4.6 for the
  bind_hyp specification and §9 for the soundness argument.

  `hyp_reduce` consults the stored type's `codomain_fn` slot, which
  returns a tagged `Action`: `Extend new_stored_type` (for
  function-typed hypotheses) or `Return value` (for predicate-typed
  hypotheses). This unifies function-application and
  predicate-application through one mechanism without making any
  type-former a kernel special case.

  There is a single library `Type` (no `Type k` stratification).
  Type-polymorphic Pi-checking works because Type's
  codomain_fn does H-rule directly via `Return`. Russell-style
  paradoxes (including the universe-self-typing `Type : Type`)
  diverge per the soundness story; they don't synthesize proofs of
  False.

  Termination guarantees on user-defined types are not provided by
  ranks; users opt in via the `Total T` library construction (§5)
  by supplying a measure and a decreasing-recursion witness.

  The current implementation now follows this kernel shape for the
  core library types: Pi, Bool, Nat, Eq, Ord, and Type are defined in
  `lib/types/*.disp` using the generic primitives. Some sections below
  still mark future work where the theory asks for stronger checks
  than the implementation performs today, especially Type metadata
  validation and categorical/lawful metadata functions.
]

= Overview

Disp is a dependently-typed language built on tree calculus. Types are
predicates over trees. The kernel is a small set of primitive handlers
that mediate parametric-mode evaluation, hypothesis minting, and the
public boundary between kernel-privileged and user code. All inductive
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
  (`Tree -> Bool` predicate body) and optional metadata (codomain
  function). The kernel's `predicate_frame` wraps the recognizer with
  H-rule handling for hypothesis inputs. The kernel doesn't grow with
  library types.

+ *Soundness via three runtime mechanisms, plus False's definition.*
  Public-boundary scan, stem-rule fork rejection, and walker triage
  rejection enforce that hypotheses don't escape into closed proofs.
  False's predicate `{_} -> FF` ensures it has no closed inhabitants.
  Together these give logical consistency at the public boundary.

== Soundness, not completeness

The kernel guarantees: *if `T(v) = TT` evaluates at the public boundary,
then v inhabits T per T's predicate*. The kernel does not guarantee:

- Termination of user predicates (apply budget catches divergence;
  divergence does not synthesize TT).
- Semantic correctness of user-supplied recognizers, codomain_fns,
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
  tree, including kernel-minted neutrals. Used for kernel-privileged code: kernel
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

#note[
  *I_canonical carve-out.* The walker recognizes the canonical
  polymorphic identity tree `I_canonical := {z} -> z` and returns
  `Ok x` for `I_canonical x`, including when `x` is a hypothesis.
  This is the only intentional walker carve-out: it preserves the
  legitimate identity function without allowing user code to inspect
  or rebuild hypothesis structure.
]

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
  *Pi is library, not kernel.* `Pi A B` is a library wait-form using
  `predicate_frame` + `bind_hyp`. The kernel is unaware of Pi
  specifically; it sees a generic predicate_frame application with
  a Pi-shaped recognizer in metadata.
]

= Library type-former protocol

== Metadata convention

Every library type-former T has the structural form:

```
T_params := guard (wait kernel_ref.predicate_frame T_meta)
```

with `T_meta` a 3-tuple:

```
T_meta := pair recognizer_sig
              (pair params codomain_fn)
```

Slots:
- *recognizer_sig.* Hash-cons-stable signature of the type-former's
  recognizer function. Stored so other library code can dispatch by
  type-former identity if needed.
- *params.* Type-former-specific data. For Pi: `pair A B`. For Eq:
  `make_eq_meta A x y`. For parameterless types (Bool, Nat, Type,
  False): the constituent fields directly or the LEAF sentinel `t`.
- *codomain_fn.* Either the LEAF sentinel `t` (T is non-applicable
  as a function), or a function `meta -> arg -> Action`. See §3.4.

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

== Action protocol (codomain_fn return values)

The `codomain_fn` slot, when not the LEAF sentinel, is a function
`(ks, raw, neutral_meta, arg) -> Action`, where `Action` is a tagged
value:

```
Extend := {new_stored_type} -> pair extend_tag new_stored_type
Return := {value}            -> pair return_tag value

is_extend := {action} -> tree_eq (pair_fst action) extend_tag
is_return := {action} -> tree_eq (pair_fst action) return_tag
```

`extend_tag` and `return_tag` are distinct constant trees fixed at
boot.

`Extend new_stored_type` says: "this application is function
application; extend the hypothesis's spine with `new_stored_type`."
Used by Pi-like types whose codomain is itself a type.

`Return value` says: "this application is predicate application;
the result is `value` directly." Used by Type-like predicate
types whose codomain_fn does the H-rule check inline.

`hyp_reduce` (§4.1) dispatches on the Action tag.

The four arguments:
- `ks` — kernel-handler proxy (`{q} -> wait raw q`), the same
  value `hyp_reduce` itself receives. Used for routing to other
  kernel handlers (e.g., `q_unguard_or_self ks ...`) and for
  reconstructing the codomain_fn's own type-form via H-rule.
- `raw` — the kernel record. Threaded for symmetry with kernel
  handlers; reflective primitives that take it (e.g.,
  `q_is_neutral`) use it to look up handler signatures.
- `neutral_meta` — the *hypothesis's* metadata (not the type's).
  See §3.5 for layouts. To recover the stored type's metadata
  (`T_meta`), do `type_meta (q_unguard_or_self ks (neutral_meta_type
  neutral_meta))`.
- `arg` — the argument the hypothesis is being applied to.

#note[
  *Codomain_fn runs raw.* It's invoked from inside `hyp_reduce`,
  which is itself a kernel handler running raw mode. So codomain_fn
  can use reflection (triage on neutrals, read stored types from
  hypothesis metadata) — operations the walker would reject in
  user contexts.

  This is the privilege boundary: user code (recognizers,
  case dispatchers) runs under the walker; library-privileged code
  (codomain_fns) runs raw via hyp_reduce. The privilege is delegated
  through the metadata layout. Library authors choose what
  codomain_fn to put in their type's metadata; the kernel runs
  whatever they declare in raw mode.

  This is what lets `Type`'s codomain_fn perform the H-rule
  directly without needing a separate kernel primitive — see §5.
]

== Metadata layouts

The kernel uses several distinct metadata layers. Each is a
deterministic tree carried inside a `wait kernel_ref.X meta`
wait-form. This subsection summarises the slot conventions; the
accessor primitives are defined in §4.8.

*Type metadata (`T_meta`).* Carried by `wait kernel_ref.predicate_frame
T_meta`. Layout:

```
T_meta := pair recognizer_sig (pair params codomain_fn)
```

Slots are described in §3.1. Used by `q_predicate_frame_fn` and
(via codomain_fn extraction) by `q_hyp_reduce_fn`. Hash-cons-stable
per §3.2.

*Neutral metadata (`neutral_meta`).* Carried by
`wait kernel.hyp_reduce neutral_meta`. Layout:

```
neutral_meta := pair stored_type (pair id spine)
```

- `stored_type` — the hypothesis's stored type (a guarded
  predicate_frame wait-form).
- `id` — hypothesis identifier; for `bind_hyp`-minted hypotheses
  this is `t domain body` (deterministic per call site, see §4.6).
- `spine` — left-leaning record of arguments accumulated by past
  `Extend` actions (§4.1). Initially `t` (empty); each function
  application of the hypothesis appends one element.

*Eliminator-frame metadata.* Carried by
`wait kernel_ref.eliminator_frame meta`. Layout:

```
elim_meta := pair count (pair dispatcher acc)
```

- `count` — remaining arity, decremented per partial application;
  the final application is recognised when `count = succ t` (§4.7).
- `dispatcher` — closed case-dispatcher tree, fixed at construction.
- `acc` — left-leaning pair of accumulated args (`t` = empty).

*Bind-hyp metadata.* Carried by `wait kernel_ref.bind_hyp meta`.
Layout: `pair count acc`, a 2-ary partial-application accumulator.
On the first application, `acc` stores the domain; on the second,
the body is invoked (§4.6).

*Stuck-elim metadata.* Eliminator_frame mints a stuck-typed
neutral whose `id` is the original target hypothesis (§4.7).
Reuses the neutral_meta layout above.

== Kernel reference convention

The kernel record itself has two reference forms in the
implementation:

- `kernel` — the recq-fixed-point record. `kernel.X` is an *eager*
  projection that yields the handler closure directly.
- `kernel_ref` — a lazy proxy defined as `{q} -> wait kernel q`.
  `kernel_ref.X` yields `wait kernel selector_X` — a wait-form
  delaying the projection.

These produce *different* trees: `wait kernel.X meta` and
`wait kernel_ref.X meta` are not hash-cons-identical.

*Canonical form for hypothesis construction:* hypothesis trees must
use `wait kernel.X meta` (the eager form), because that is the form
`Hyp` / `q_make_hyp` produce. Any codomain_fn that reconstructs
`self_as_hyp` for H-rule comparison must likewise use
`wait kernel.hyp_reduce neutral_meta` — *not* `wait kernel_ref.hyp_reduce
neutral_meta` — or the reconstruction will not hash-cons-match the
actual hypothesis tree.

*Canonical form for type construction:* library type-formers should
use `wait kernel_ref.X meta` (the lazy proxy form) so that their
recognizer signatures match the dispatcher's registered anchors,
which are themselves built with `wait kernel_ref.X t` (see §4.8 /
the implementation's `kernel_*_sig` exports).

Concretely: when `T = guard (wait kernel_ref.predicate_frame
T_meta)`, the dispatcher's signature lookup matches because
`pair_fst T = pair_fst (wait kernel_ref.predicate_frame t) =
kernel_predicate_frame_sig`. Switching the type's construction to
`wait kernel.predicate_frame T_meta` would yield a different
signature shape and break the dispatcher.

= Per-primitive specifications

== `hyp_reduce`

When `apply(neutral, x)` evaluates, the dispatcher routes to
`hyp_reduce`. The handler reads the neutral's stored type metadata,
extracts the codomain_fn (§3.4), and dispatches on the Action it
returns:

```
q_hyp_reduce_fn := {ks, raw, query} -> fix ({self, meta, v} -> {
  let stored = q_unguard_or_self ks (neutral_meta_type meta)
  let cod_fn = pair_snd (pair_snd (type_meta stored))
  match (tree_eq cod_fn t) {
    TT =>
      // Sentinel: type is non-applicable. Extend with InvalidType.
      wait self (extend_neutral_meta meta InvalidType v)
    FF => {
      let action = cod_fn ks raw meta v
      match (is_extend action) {
        TT =>
          let new_stored = pair_snd action
          wait self (extend_neutral_meta meta
                       (wait kernel_ref.guard new_stored) v)
        FF => match (is_return action) {
          TT => pair_snd action   // return value directly
          FF =>
            // Malformed action: defensive InvalidType.
            wait self (extend_neutral_meta meta InvalidType v)
        }
      }
    }
  }
})
```

The two Action cases serve different type-former classes:
- *Extend* extends the hypothesis's spine, used by Pi (where applying
  a Pi-typed hypothesis to an argument produces a new neutral whose
  stored type is the codomain at that argument).
- *Return* yields the value directly with no spine extension, used by
  Type (where applying a Type-typed hypothesis is a
  predicate-application whose result is the H-rule answer, not a new
  neutral).

The codomain_fn convention is part of the library type-former
protocol (§3.1, §3.4).

== `guard`

Public-boundary entry scan plus predicate evaluation. Returns bare
TT/FF.

```
q_guard_fn := {ks, raw, query} -> {core, v} ->
  match (q_scan_no_neutral raw v) {
    TT => match (tree_eq (core v) TT) { TT => TT; FF => FF }
    FF => FF
  }
```

The scan rejects pre-existing neutrals; the inner check evaluates
`core` (the type's predicate function) on `v` and folds to bare TT/FF.

== `unguard`

Walker-safe peeling of the guard layer. Routed through the dispatcher
so the body's triage on T runs in raw mode rather than under the
walker (which would reject triage on a hypothesis-typed T).

*Pass-through on hypotheses.* If `v` is a kernel-minted hypothesis,
`unguard` returns `v` unchanged. This is what lets type-polymorphic
Pi-checks (§5.1) write `unguard_checked (B hyp)` even when `B hyp`
reduces to a hypothesis: `unguard_checked` peels nothing, returns
the hypothesis directly, and subsequent application
(`expected_core result`) routes through `hyp_reduce` as expected.

For closed non-guarded, non-hypothesis inputs, the current
implementation returns `InvalidType` (`FF`). Library code that wants
ordinary identity-on-non-guards uses `q_unguard_or_self ks v` (§4.8)
or the public `unguard_or_self` helper instead of the bare routed
`unguard`.

== `checked_apply`

Dispatcher. Routes by signature recognition; falls through to the
parametric walker for unrecognized signatures.

== `predicate_frame`

Wraps a user-supplied recognizer with H-rule for hypothesis inputs:

```
q_predicate_frame_fn := {ks, raw, query} -> {meta, v} -> {
  match (q_is_neutral raw v) {
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
      must_ok_or_ff result_r ({result} -> result)
  }
}
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
q_bind_hyp_fn := {ks, raw, query} -> {meta, x} -> {
  // Standard 2-ary arity-tracked accumulator: first arg is domain,
  // second arg is body. On second application, execute.
  let count = pair_fst meta
  let acc = pair_snd meta
  let final_fn = ... -> {
    let domain = pair_snd acc
    let body = x
    let hyp_id = t domain body
    let hyp = q_make_hyp raw domain hyp_id
    let result_r = ks.checked_apply body hyp
    must_ok_or_ff result_r ({result} ->
      match (q_contains_via_open_path raw result hyp) {
        TT => Fail              // hyp escaped through a non-neutral path
        FF => result             // hyp didn't escape — return body's value
      })
  }
  let partial_fn = ... // accumulator for partial application
  select final_fn partial_fn (tree_eq count (t t t))
    raw query ks meta x count acc
}
```

The escape check `q_contains_via_open_path` walks the result tree and
returns TT if `hyp` is reachable at any position whose ancestors are
*not* themselves kernel-minted neutrals. Concretely:

```
q_contains_via_open_path := fix ({self, raw, result, hyp} ->
  match (tree_eq result hyp) {
    TT => TT                                      // direct match
    FF => match (q_is_neutral raw result) {
      TT => FF                                    // walled off inside a neutral
      FF => match (is_fork result) {
        TT => or (self raw (pair_fst result) hyp)
                  (self raw (pair_snd result) hyp)
        FF => match (is_stem result) {
          TT => self raw (stem_child result) hyp
          FF => FF                                 // leaf, no match
        }
      }
    }
  })
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
  type, including `Bool`, `Ord`, `Nat`, etc. A library `Total` proof
  for an inductive type T can use `bind_hyp T ({h} -> measure h)`
  to express a measure of T's elements; the result is an Ord that
  may contain `h` inside a kernel-minted neutral (e.g., a stuck-Ord
  produced by ord_lt on hypothesis-typed Ord arguments), which the
  escape check accepts.

  The earlier "body returns Bool" restriction was a workable
  approximation but was both too strict (rejected legitimate
  non-Bool returns) and too loose (relied on caller patterns rather
  than enforcing safety at the kernel level). The escape check is
  the more precise property.
]

== `eliminator_frame`

Wraps a case dispatcher with stuck-mint for hypothesis targets. This
is the kernel primitive that *does* allow a hypothesis to escape with
a derived stored type — but only inside a controlled eliminator
pattern.

Each library eliminator is constructed as
`wait kernel_ref.eliminator_frame (init_meta_arity_N dispatcher)`,
where `dispatcher` is the type-former's case-dispatching tree
(a closed function `motive -> cases -> target -> result`). The
dispatcher is stored directly in the metadata; no kernel-side
registry or signature lookup is required.

Meta layout: `pair count (pair dispatcher acc)`:
- `count` — remaining arity, decremented per application; the final
  application is recognised when `count = succ t`.
- `dispatcher` — fixed slot, written at construction, never mutated.
- `acc` — left-leaning pair tree of accumulated args; nil = `t`.

```
q_eliminator_frame_fn := {ks, raw, query} -> {meta, x} -> {
  let count      = pair_fst meta
  let dispatcher = pair_fst (pair_snd meta)
  let acc        = pair_snd (pair_snd meta)
  let final_fn = {...} -> {
    let motive = ... // extract from acc
    let cases  = ... // extract from acc
    let target = x
    select_lazy
      ({_} -> q_make_hyp raw (motive target) target)
      ({_} -> ks.checked_apply
                 (dispatcher motive cases) target)
      (q_is_neutral raw target)
  }
  let partial_fn = {...} ->
    wait (ks query)
      (pair (pred count) (pair dispatcher (pair acc x)))
  select final_fn partial_fn (tree_eq count (t t t))
    raw query ks meta x count dispatcher acc
}

init_meta_arity_N := {dispatcher} ->
  pair (succ^N t) (pair dispatcher t)
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

#note[
  *Dispatcher correctness is library responsibility.* The kernel does
  not validate that the dispatcher returns a value matching the motive
  for the supplied constructor. A miswritten dispatcher (e.g., one
  that always returns `base` regardless of `target`) yields a
  type-mismatched value but does not break soundness — the public
  boundary's predicate evaluation runs concretely on the result and
  returns FF if the type doesn't match. No kernel-side registry of
  blessed dispatchers is needed; storing the dispatcher directly in
  metadata is sufficient. The parametric-mode invocation prevents the
  dispatcher from synthesising hypothesis-tainted values that would
  pass the public-boundary scan; everything else is the library
  author's obligation.
]

== Reflective and structural primitives

The kernel handlers and library codomain_fns reference a number of
fixed helper primitives. These are *not* kernel handlers (they have
no signature dispatch); they are ordinary closed tree-calculus
functions that perform fixed reflective or structural operations.
The implementation may provide them as native fast-paths in the host
runtime, mirroring an in-language reference body.

*Signature recognition.*

```
has_sig := {sig, v} ->
  // tree_eq sig (extract head signature from v)
  // For a wait-form (wait f x), the head signature is f.
  // For other shapes, signature extraction yields t (no match).
  ...

is_neutral := {v} -> and (is_fork v) (has_sig kernel.hyp_reduce v)
is_guard   := {v} -> has_sig kernel_ref.guard v

q_is_neutral := {raw, v} -> and (is_fork v) (has_sig raw.hyp_reduce v)
  // Uses the raw kernel record because Hyp / q_make_hyp store the eager
  // hyp_reduce handler signature, not the lazy kernel_ref proxy.
```

*Tree-shape predicates.* These observe the outer constructor of `v`
without applying it. Under the parametric walker, all four return
`Fail` when applied to a kernel-minted neutral; under raw mode they
report the actual shape.

```
is_leaf  := {v} -> tree_eq v t
is_stem  := {v} -> "v has the form (t a)"
is_fork  := {v} -> "v has the form (t a b)"
stem_child := {stem} -> pair_snd stem   // for is_stem v = TT
```

*Metadata accessors.*

```
type_meta := {v} ->
  // Peel guard + wait wrappers to recover the metadata payload.
  // For T = guard (wait pf T_meta):     returns T_meta.
  // For h = wait hyp_reduce nm:         returns nm.
  // For (wait elim_frame em):           returns em.
  // Otherwise:                          returns t.
  ...

neutral_meta_type := pair_fst                   // first slot of nm
neutral_meta_id   := {nm} -> pair_fst (pair_snd nm)
neutral_meta_spine := {nm} -> pair_snd (pair_snd nm)

extend_neutral_meta := {nm, new_stored, arg} ->
  pair new_stored
       (pair (neutral_meta_id nm)
             (pair (neutral_meta_spine nm) arg))
```

*Hypothesis construction.*

```
q_make_hyp := {raw, stored_type, id} ->
  wait raw.hyp_reduce
    (pair stored_type (pair id t))   // empty spine

q_unguard_or_self := {ks, v} ->
  match (is_guard v) {
    TT => unguard_checked v       // dispatcher route to kernel.unguard
    FF => v                        // pass-through (§4.3)
  }

cert_make_stuck := {stored_type, stuck_id} ->
  wait kernel.hyp_reduce (pair stored_type (pair stuck_id t))
  // Mints a stuck-typed neutral. `stuck_id` should be a deterministic
  // function of the stuck origin (e.g., (neutral_meta, v)) so that
  // independent stucks hash-cons-distinguish.
```

*Public-boundary scan.* Walks the input tree, returns FF if any
kernel-minted neutral is found; otherwise TT.

```
q_scan_no_neutral := fix ({self, raw, v} ->
  match (is_neutral v) {
    TT => FF
    FF => match (is_fork v) {
      TT => and (self raw (pair_fst v)) (self raw (pair_snd v))
      FF => match (is_stem v) {
        TT => self raw (stem_child v)
        FF => TT       // leaf
      }
    }
  })
```

*Eq metadata helpers.*

```
make_eq_meta := {A, x, y} -> pair A (pair x y)
eq_meta_type := pair_fst
eq_meta_lhs  := {m} -> pair_fst (pair_snd m)
eq_meta_rhs  := {m} -> pair_snd (pair_snd m)
```

*Result handling.* Walker invocations return `Ok value` or `Fail`;
`must_ok_or_ff` folds Fail into FF for predicate composition.

```
must_ok_or_ff := {result, k} ->
  match (is_fail result) {
    TT => FF
    FF => k (ok_val result)
  }
```

#note[
  *These primitives are not kernel-extension points.* Library code
  uses them; library authors may compose new helpers from them.
  Adding new primitives to this list is not the same as adding a
  kernel handler — the seven-handler registry (§3.3) is closed.
  Helpers are pure tree functions; soundness rests on the kernel
  handlers and the walker, not on the helper definitions.
]

= Worked library types

== Pi

```
let pi_recognizer = {params, v} -> {
  let A = pair_fst params
  let B = pair_snd params
  bind_hyp A ({hyp} ->
    let result = v hyp
    let expected_core = unguard_checked (B hyp)
    expected_core result)   // returns Bool from expected's predicate
}
let pi_recognizer_sig = checker_sig pi_recognizer

let pi_codomain_fn = {ks, raw, neutral_meta, arg} -> {
  // Recover T_meta from the hypothesis's neutral_meta:
  //   neutral_meta_type    : neutral_meta -> guarded T
  //   q_unguard_or_self ks : guarded T    -> wait pf T_meta (or self if not guarded)
  //   type_meta            : wait pf T_meta -> T_meta
  let stored_type = q_unguard_or_self ks (neutral_meta_type neutral_meta)
  let T_meta = type_meta stored_type
  let params = pair_fst (pair_snd T_meta)
  let B = pair_snd params
  Extend (B arg)
}

Pi := {A, B} -> guard (wait kernel_ref.predicate_frame
  (pair pi_recognizer_sig
        (pair (pair A B) pi_codomain_fn)))
```

The codomain_fn returns `Extend (B arg)`, telling `hyp_reduce` to
extend a Pi-typed hypothesis's spine with `B arg` as the new stored
type.

For *closed* Pi-checks, `expected_core result` invokes the codomain's
predicate routinely.

#note[
  *Pre-unguarding convention.* The recognizer's `unguard_checked (B
  hyp)` is correct *only* if `B hyp` returns a guarded type — i.e.,
  if `B` is the user-supplied codomain function whose image is
  `guard(...)`-wrapped per the public type API.

  Implementations that store a *pre-unguarded* B in the meta (e.g.,
  `params := pair (unguard_checked A) ({x} -> unguard_checked (B
  x))`) must *drop* the recognizer's outer `unguard_checked` to
  avoid double-unguarding (which `q_unguard_fn` rejects as
  `InvalidType`).

  Either convention is internally consistent; what matters is that
  the recognizer's wrapping matches the metadata's storage choice.
  The spec convention is "store guarded B; recognizer unguards once."
]

For *type-polymorphic* Pi-checks (e.g.,
`Pi Type ({A} -> Pi A ({_} -> A)) poly_id`):

+ Outer pi_recognizer mints `A_hyp : Type`.
+ Inner pi_recognizer mints `x_hyp : A_hyp`.
+ Inner check `expected_core x_hyp` becomes `apply A_hyp x_hyp`.
+ Routes via hyp_reduce → Type's codomain_fn → H-rule. Returns
  `Return TT` because `stored(x_hyp) = A_hyp = self_as_hyp`.
+ hyp_reduce returns TT directly (Action `Return TT`).
+ Inner pi_recognizer's body sees TT, propagates upward.
+ Outer Pi-check accepts.

So type-polymorphic theorems are provable as closed terms,
without Pi being a kernel primitive. The H-rule lives in Type's
codomain_fn (§5), where the `Return` Action lets it bypass spine
extension.

== Bool

```
let bool_recognizer = {_, v} ->
  or (tree_eq v TT) (tree_eq v FF)
let bool_dispatcher = {motive, cases, target} ->
  let ct = pair_fst cases
  let cf = pair_snd cases
  select ct cf target

Bool := guard (wait kernel_ref.predicate_frame
  (pair bool_recognizer_sig
        (pair t t)))   // no params; codomain_fn = sentinel

bool_rec := wait kernel_ref.eliminator_frame
  (init_meta_arity_4 bool_dispatcher)
```

Bool's case handlers (`ct`, `cf`) are returned as values, not invoked.
The dispatcher just selects between them. No invocation, no
parametric-mode concerns inside the dispatcher.

== Nat

```
let nat_recognizer = fix ({self, _, v} ->
  triage TT
    ({_} -> FF)
    ({l, r} -> and (is_leaf l) (Nat r))
    v)

let nat_dispatcher = fix ({self, motive, cases, target} -> {
  let base = pair_fst cases
  let step = pair_snd cases
  match (tree_eq target t) {
    TT => base
    FF =>
      let k = pair_snd target
      let ih = self motive cases k
      step k ih   // user-supplied step body, runs under walker
  }
})

Nat := guard (wait kernel_ref.predicate_frame
  (pair nat_recognizer_sig (pair t t)))

nat_rec := wait kernel_ref.eliminator_frame
  (init_meta_arity_4 nat_dispatcher)
```

Per the library invariant from §4.7: `step k ih` runs under parametric
mode. User-supplied step bodies must use `k` and `ih` via type
predicates or function applications — not raw triage.

== Eq

```
let eq_recognizer = {params, v} -> {
  let A = eq_meta_type params
  let x = eq_meta_lhs params
  let y = eq_meta_rhs params
  and (tree_eq v refl) (tree_eq x y)
}

Eq := {A, x, y} -> guard (wait kernel_ref.predicate_frame
  (pair eq_recognizer_sig
        (pair (make_eq_meta A x y) t)))
```

The equality eliminator is an ordinary `eliminator_frame` wrapper, not
a kernel primitive. The user-facing `eq_J A x motive base y proof`
packages `motive` and `base` into the generic
`motive -> cases -> target` shape: closed `refl` proofs dispatch to
`base`, while neutral proofs mint a stuck neutral with stored type
`motive y proof`.

== Type

```
let type_recognizer = {_, v} ->
  // v is a "type" iff it's structurally a guarded predicate_frame wait-form
  match (has_sig kernel_ref.guard v) {
    TT => has_sig kernel_ref.predicate_frame (type_meta v)
    FF => FF
  }

let type_codomain_fn = {ks, raw, neutral_meta, v} -> {
  // Use `kernel.hyp_reduce` (eager) — matches Hyp / q_make_hyp's
  // construction. Using kernel_ref.hyp_reduce would produce a
  // different tree shape that fails to hash-cons-match the actual
  // hypothesis tree under tree_eq. See §3.5 "Kernel reference
  // convention".
  let self_as_hyp = wait kernel.hyp_reduce neutral_meta
  let stored_v = q_unguard_or_self ks (neutral_meta_type (type_meta v))
  Return (tree_eq stored_v self_as_hyp)
}

Type := guard (wait kernel_ref.predicate_frame
  (pair type_recognizer_sig
        (pair t type_codomain_fn)))
```

`Type v = TT` iff v is structurally a guarded predicate_frame
wait-form. All library type-formers (Pi, Bool, Nat, Eq, False,
Sigma, Type itself) qualify.

For a hypothesis `Hyp Type id` (a "type variable"):
- `predicate_frame`'s H-rule on its own treats this hyp as inhabiting
  `Type` (stored type matches T).
- Applying the hypothesis to a value goes through hyp_reduce, which
  invokes `type_codomain_fn`. For hypothesis arguments matching this
  type-variable's identity, it returns TT (H-rule). For non-matching
  hypotheses and for closed values, it returns FF in the current
  implementation, because there is no stored-type evidence that the
  value inhabits the unknown type.

#note[
  *Type : Type.* Type is itself a guarded predicate_frame
  wait-form, so `Type Type = TT`. Russell-style paradoxes
  (e.g., `R := {T} -> not (T T)`; `R R`) exist as well-formed
  expressions but diverge when applied — the apply budget catches
  them as failure. Disp's "soundness via divergence-as-failure"
  stance applies: divergence is observably distinct from TT, so
  paradoxes don't synthesize proofs of False. Stratification is not
  needed for soundness in Disp.
]

#note[
  *Closed-value case returns FF today.* Example: if `A_hyp = Hyp Type h0`,
  then `A_hyp 5 = FF`. The closed value `5` has no neutral metadata
  whose stored type can be compared to `A_hyp`, so Type's codomain_fn
  cannot justify membership in the unknown type. This is conservative:
  legitimate polymorphic functions are checked with hypotheses, and
  concrete uses instantiate the type variable before checking closed
  values.
]

#note[
  *Future Type rigor.* `Type` currently recognizes guarded
  predicate_frame values structurally. It does not yet validate that
  a type's recognizer, params, and codomain_fn obey the intended laws
  or that metadata functions correspond to lawful categorical
  constructions. That stronger validation belongs in future tooling
  and/or a richer `Type` checker; for now users are expected to use
  types whose metadata they understand.
]

== False

```
let false_recognizer = {_, _} -> FF

False := guard (wait kernel_ref.predicate_frame
  (pair false_recognizer_sig (pair t t)))

Not := {P} -> Pi P ({_} -> False)
```

`False v = FF` for all closed `v`. For hypothesis `v` with stored type
False, predicate_frame's H-rule returns TT (correctly representing a
hypothesis assumption). At the public boundary, hypothesis-tainted
values fail `scan_no_neutral`, so no closed proof of False can be
synthesized.

== Sigma, Refinement, Forall

```
let sigma_recognizer = {params, v} -> {
  let A = pair_fst params
  let B = pair_snd params
  let a = pair_fst v
  let b = pair_snd v
  and (A a) ((B a) b)
}

let refinement_recognizer = {params, v} -> {
  let A = pair_fst params
  let P = pair_snd params
  and (A v) (P v)
}

let forall_recognizer = {params, v} -> {
  let A = pair_fst params
  let P = pair_snd params
  bind_hyp A ({hyp} ->
    (P hyp) v)   // proposition P at hyp: does v inhabit?
}
```

Each gets the standard 3-tuple metadata wrapping with sentinel
codomain_fn. Sigma and Refinement don't need bind_hyp (closed
dispatch). Forall does (it quantifies over A).

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

== `wf_fix` and `Total`

`wf_fix` is a library combinator for measure-bounded recursion. It
wraps the recursive parameter so it can only be invoked on
strictly-smaller-measure inputs:

```
// wf_fix : (T -> M) -> ((T -> Bool) -> (T -> Bool)) -> (T -> Bool)
//
// `measure` maps inputs to a well-founded type M (Nat, Ord, ...).
// `body` takes `rec` and `v`; rec invocations on inputs whose
// measure is not strictly less than measure(v) return Fail.

wf_fix := {measure, body} ->
  fix ({self, v} ->
    let bounded_self = {v_inner} ->
      match (lt_in_M (measure v_inner) (measure v)) {
        TT => self v_inner
        FF => Fail
      }
    body bounded_self v)
```

`lt_in_M` is the strict-less-than for the measure type. For Nat
measures: `nat_lt`. For Ord measures: `ord_lt`. Library helpers per
type.

Predicates defined via `wf_fix` are total by construction: recursion
is bounded by a strictly-decreasing measure into a well-founded type.

Two flavors of `Total` for asserting predicate totality:

```
// Structural: Total T iff T's recognizer was built via wf_fix.
// `wf_fix_sig` is the library-level signature of the wf_fix
// combinator (a fixed hash-cons-stable tree, not a kernel ref).
let total_recognizer = {_, T} ->
  let inner = type_meta T
  let recognizer = pair_fst (type_meta inner)
  has_sig wf_fix_sig recognizer

Total := guard (wait kernel_ref.predicate_frame
  (pair total_recognizer_sig (pair t t)))
```

```
// Constructive: TotalWith T inhabited by (measure, decreasing_proof).
TotalWith := {T} ->
  Sigma (Tree -> Ord) ({measure} ->
    Pi Tree ({v} ->
      // proof that recursive calls of T's body on input v
      // produce values v' with ord_lt (measure v') (measure v)
      ...))
```

`Total` is the "accept the syntactic discipline" variant; `TotalWith`
is "show me an explicit termination proof." Library authors pick
based on need.

Termination guarantees in Disp are opt-in. Users who care about
"my type-checker won't loop" supply Total proofs and pass them
through the type system. Users who don't care write whatever
predicates they want; divergence-as-failure handles soundness.

== `Ord` (library inductive)

`Ord` is an ordinary library inductive defined via `predicate_frame`
+ `eliminator_frame`. It is *not* a kernel concern under this
design — there is no universe-rank machinery that uses Ord. Library
code that wants ordinal arithmetic, well-founded measures, or
stuck-comparing rank arithmetic pulls Ord in.

```
Ord := guard (wait kernel_ref.predicate_frame
  (pair ord_recognizer_sig (pair t t)))
```

`ord_lt`, `ord_le`, `ord_max` are library functions defined via
`eliminator_frame`. They handle stuck-Ord values via the
eliminator_frame's stuck-mint behavior (when the target is
hypothesis-typed, the eliminator mints a stuck of the motive's
codomain type).

Used by `TotalWith` proofs over inductives whose measures aren't
expressible as Nat (e.g., when measures involve hypothesis-typed
values that need stuck propagation).

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

These mechanisms apply uniformly to user-defined recognizers and
case dispatchers. All user code under `predicate_frame` or
`eliminator_frame` runs through `ks.checked_apply`, which routes
through the walker.

Codomain_fns run in raw mode (invoked from inside hyp_reduce). They
are library-privileged code, but their privilege is delegated through
the metadata layout — library authors choose what to put in
codomain_fn slots; the kernel runs whatever they declare in raw
mode. Type's codomain_fn relies on this for its H-rule
implementation (§5.5 / §8).

== Logical consistency at the public boundary

Logical consistency means: no closed value submitted at the public
boundary inhabits `False`.

This follows from three facts:

+ False's predicate is `{_, _} -> FF`. For any closed v, `False v = FF`.
+ Hypothesis-tainted values cannot pass the public-boundary scan.
+ bind_hyp's escape check prevents user code from constructing
  type-system-accepted values that quietly carry hypotheses past
  type-checking.

Therefore no value, however constructed, satisfies False at the public
boundary.

#note[
  *The escape check resolves the weirder attack.* Consider
  `weirder := {x} -> bind_hyp False ({h} -> h)` —
  a function that ignores its argument and tries to return a fresh
  False hypothesis.

  Inner bind_hyp's body returns `h` directly. The escape check on
  bind_hyp's result detects that the minted hyp `h` is reachable
  via a non-neutral path (in fact, `h` IS the result, modulo
  identity). The check returns TT (h escapes), so bind_hyp
  returns Fail.

  weirder thus evaluates to Fail at the body. Pi-check
  `Pi Bool ({_} -> False) weirder`: the recognizer's
  `expected_core (v hyp_b)` becomes `False Fail = FF`, so Pi-check
  rejects weirder. Soundness preserved.

  Without the escape check, weirder would be accepted at type-check
  via predicate_frame's H-rule firing on the escaped h_false. The
  type-system would claim weirder inhabits `Pi Bool ({_} -> False)`,
  but the public-boundary scan on weirder(TT) would produce FF.
  That mismatch is what the escape check eliminates.
]

== Russell-style paradoxes

Russell-style self-referential predicates like `R := {T} -> not (T T)`
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

= Type-polymorphic Pi via Type's codomain_fn

The polymorphic identity check `Pi Type ({A} -> Pi A ({_} -> A))
poly_id` succeeds at the public boundary because Type's
codomain_fn (§5) does H-rule directly via the `Return` Action,
bypassing the spine-extension default that Pi-typed hypotheses use.

Specifically, the inner check `apply A_hyp x_hyp` invokes hyp_reduce.
hyp_reduce reads Type's codomain_fn, evaluates it on
`(meta, x_hyp)`. The codomain_fn does
`tree_eq stored(x_hyp) self_as_hyp` (the H-rule), gets TT, returns
`Return TT`. hyp_reduce returns TT directly (Action `Return TT`).

The Pi-recognizer thus sees TT as the membership-check result and
propagates it upward. The polymorphic Pi-check accepts. Pi remains a
library type, not a kernel primitive — the H-rule capability lives
in Type's codomain_fn, made accessible to kernel-routed
hyp_reduce by the Action protocol.

The codomain_fn's reflective operations (`q_is_neutral`,
`neutral_meta_type`) are safe because hyp_reduce runs in raw mode;
they're privileged through the kernel's dispatch routing, not
through any user reflection.

= Implementation Status and Future Work

The implementation has landed the central migration described by this
document:

+ The kernel record has the seven primitive handlers.
+ `hyp_reduce` dispatches on `Extend` / `Return` actions.
+ Pi, Bool, Nat, Eq, Type, and Ord are library types built from
  `predicate_frame`, `bind_hyp`, and `eliminator_frame`.
+ Ranked universes have been replaced by canonical `Type`.
+ Eq no longer has a dedicated kernel handler.

Known future work:

+ Make `Type` more rigorous about validating type metadata and the
  laws expected of recognizers, codomain_fns, and eliminator
  dispatchers.
+ Implement `wf_fix`, `Total`, and `TotalWith` library constructions.
+ Improve diagnostics for divergence and failed type validation.
+ Continue reducing host-only elaborator behavior as the
  self-hosting story matures.
