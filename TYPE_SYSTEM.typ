#set document(title: "Disp Type System — Evaluator-Map Type Theory")
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
  inset: (left: 1em, y: 0.35em),
  body,
)

#let invariant(title, body) = block(
  breakable: false,
  above: 0.8em,
  below: 0.8em,
  stroke: (left: 2.5pt + rgb("#0b63b0")),
  inset: (left: 1em, y: 0.35em),
  text(weight: "bold")[#title: ] + body,
)

#align(center, text(22pt, weight: "bold")[Disp Type System])
#v(0.3em)
#align(center, text(13pt)[Evaluator-Map Type Theory])
#v(0.5em)
#align(center)[
  A compositional type system over Disp's binary-tree calculus where every type is a
  `(check, assume)` pair and every assumption is a map `Eval -> Eval`.
]
#v(1em)

#note[
  *Status.* Specification of Disp's promoted evaluator-map kernel implemented in
  #raw("lib/kernel/kernel.disp") and #raw("lib/kernel/surface.disp") (opened via
  #raw("lib/kernel/prelude.disp")), validated across #raw("lib/kernel/kernel.test.disp"),
  #raw("lib/kernel/pts.test.disp"), #raw("lib/kernel/guards.test.disp"), the checked
  standard library (#raw("lib/kernel/std_*.disp")), verified rewrites
  (#raw("lib/kernel/verified_opt.disp") / #raw("lib/kernel/verified_opt.opt.disp")), and
  #raw("test/guards.test.ts"). Supersedes #raw("archive/live-kernel/TYPE_THEORY.typ"),
  #raw("archive/provenance-kernel/"), and #raw("archive/type-system-goals/"). Remaining
  milestones (extensible `meta` state for linearity/effects/cost, HoTT paths, stratified
  universes, and stream-type soundness) are tracked in #raw("FINAL_TYPE_SYSTEM_NEXT_STEPS.md").
]

= Overview & Design Principles

Disp programs are pure binary trees evaluated by a deterministic, Turing-complete tree
calculus (`lib/prelude.disp`, `lib/machine.disp`). Following `TYPE_SYSTEM_GOALS.md`, the
type system adheres to five principles:

1. *Meaning is operational refutation.* A type's semantic meaning is defined by running a
   candidate program on concrete trees looking for a counterexample (`pair("Fault", ...)`).
   Members are programs that never fault.
2. *Checking is sound abstract evaluation.* A type checker runs the *same* program inside
   the *same* evaluator upgraded with sound shortcuts over symbolic unknowns. Every
   acceptance (`yes`) must never be refutable by any bounded concrete run.
3. *Types are evaluator maps, not syntax ASTs.* There is no closed grammar of types, no
   central `switch` over type codes, and no custom runtime opcodes (`Seal`, `Brand`, `Key`,
   etc.). Every type is a pair `pair(check, assume)` where `assume : Eval -> Eval` installs
   the approved eliminators for an unknown of that type.
4. *One former serves both checking and refuting.* Type formers (`data_t`, `pi_t`,
   `sigma_t`, `eq_t`, `forall_k`, `quot_pi`, `bounded_pi`, `opaque_with_elim`) are written
   once with zero `if (is_refute)` branching. Checking and refuting are simply two base
   evaluators implementing the same 7-field `Kit`.
5. *First-class types are token-free.* Internal symbolic handle tags (`tok_sig`) stay
   strictly private to the base evaluators, so type values (`Nat`, `NN`, `IdT`) contain
   zero internal handles and can be passed as ordinary runtime values without triggering
   anti-forging guards.

= Substrate & Private Handle Discipline

All Disp values are built from the leaf `t`, stems `t(x)`, and forks `t(x, y)` (written
`pair(x, y)`). Execution is modeled explicitly in `lib/machine.disp` via small-step
machine states `machine_run(f, x, stack)` and `machine_done(v)` with continuation frames
`fr_to`, `fr_s`, and `fr_res`, accelerated natively by `m_advance`.

== Unforgeable Handles

Inside the base engine only, symbolic values are tagged forks headed by a single private
tag `tok_sig := "EvalHandle"`:

- `tok_unk(label) := pair(tok_sig, pair("U", label))` — an unknown value (function
  argument, split child, polymorphic instantiation, abstract type parameter, etc.).
- `tok_call(f, x) := pair(tok_sig, pair("C", pair(f, x)))` — a neutral call (a recursive
  function, callback application, or stuck equality test `pair("Eq", a)` on `x`).
- `mk_teq(a) := pair(tok_sig, pair("TeqP", a))` — a partially applied `tree_eq(a)`.

#invariant("Anti-Forging Guard")[
  Every entrypoint (`why`, `refute_at`, `refute_with`) rejects any candidate program `v`
  (or lemma list) for which `has_tok(v)` is `true`. During execution, `settle_m` checks
  every stem application `t(stem_child(f), arg)`: if `stem_child(f) == tok_sig`, execution
  aborts immediately with `"Builds a token"`. Because no type former ever references
  `tok_sig`, `has_tok(T) == false` holds for every constructed type `T`.
]

= The Layered Evaluator Architecture

An evaluator `Eval` separates *composable semantic rules* (stacked by `assume` maps),
*engine capabilities* (`Kit`), and *threaded per-run state* (`meta`):

```disp
record ev :: {
  step    : Tree,  // (self, fuel, next, f, x, stack, k) -> Res
  lazy    : Tree,  // (self, fuel, next, v, b, x, rest, k) -> Res
  resolve : Tree,  // (v) -> Tree
  eq      : Tree,  // (self, a, b) -> "True" | "False" | "Stuck" | Err | Refuse
  has_tag : Tree,  // (self, tag, v) -> Bool
  kit     : Kit,   // 7 base-engine capabilities
  meta    : Tree   // (depth, (sure, (cap, unbox)))
} -> Eval
```

== Why `meta` Is Separate from Stacked Rules

Most assumptions are *monotonic rule extensions*: once installed on `E`, `E.step`,
`E.resolve`, `E.eq`, and `E.has_tag` delegate downward via open recursion (`self` always
points to the outermost evaluator). However, state that *changes on every step* (such as
remaining split `depth`, whether we are inside a trusted type family `sure`, a countdown
step `cap`, or active type-unboxing bindings `unbox`) cannot be decremented by rebuilding
an inner layer without discarding newer layers stacked above it. Keeping `meta` on the
outermost `Eval` lets `run_ev` update `cap` or `depth` in place while preserving every
installed assumption layer.

== The Minimal 7-Field `Kit`

Type formers and `Eval -> Eval` combinators interact with the base machine exclusively
through `E.kit`:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Field*], [*Signature / Purpose*], [*Mode-Specific?*],
    [`run`], [`(E, fuel, next, m, k) -> Res` (accepts `machine_run` or `m_ret`)], [Yes (`refute_run` adds native `m_advance` fast-path)],
    [`quantify`], [`(E, fuel, next, A, rec_T, f, body, k) -> Res`], [Yes (`check`: 1 symbolic unknown + `with_rec`; `refute`: enumerate `tried(n)`)],
    [`fresh`], [`(label) -> handle` (`"Call"` mints `tok_call`, else `tok_unk`)], [Shared (hides `tok_sig`)],
    [`deep_res`], [`(E, v) -> Tree` (recursively resolves handles & `tok_call`)], [Shared (hides `tok_sig`)],
    [`head_tok`], [`(v) -> "U" | "T" | "N"` (O(1) head handle classifier)], [Shared (hides `tok_sig`)],
    [`has_tok`], [`(v) -> Bool` (checks if any handle occurs inside `v`)], [Shared (hides `tok_sig`)],
    [`cases`], [`(pred, u) -> Maybe(List((shape, subpreds)))` (constructor derivation)], [Shared (hides `tok_sig`)],
  ),
  caption: [The 7-field `Kit` interface. Only 2 fields (`run`, `quantify`) differ between checking and refuting.],
)

== Core Execution Loop (`run_ev`)

`run_ev` is a 20-line continuation-passing loop:
1. If `m0` is `m_ret(v, stack)`, deliver `v` onto `stack` via `deliver_m`; otherwise
   settle free leaf/stem builds via `settle_m` (checking the anti-forging guard).
2. If finished, invoke continuation `k(E, fuel, next, result)`.
3. If `Forge`, return `"Builds a token"`. If `fuel == 0`, return `"Out of fuel"`.
4. If `Lazy` (an S-combinator's first result is an unknown token that may ignore its
   argument if K-shaped), delegate to `E.lazy` (allowing `with_data` to split before
   forcing).
5. Otherwise, if `ev_cap(E)` is `pair("Cap", n)`, abort with `pair("Fault", "slow")` at
   `n == 0` or decrement `n` in `meta`, and dispatch to `E.step`.

In `refute_kit`, `refute_run` accelerates any top-level concrete call (`m_stack.is_nil`
and neither resolved function nor argument has tokens) directly through Rust-native
`m_advance`, giving a 40--60x speedup on higher-order refutation sweeps.

== Result Outcomes: `yes`, `"Fault"`, and Non-Fault Refusals

A crucial distinction makes checking conservative without causing false refutations:
- `done(fuel) := pair("Yes", fuel)` — branch succeeded.
- `pair("Fault", witness)` — a definite type violation (returned as a counterexample by
  `refute_at` and rejected by `why`).
- Plain string messages (`"Out of fuel"`, `"Out of depth"`, `"Builds a token"`,
  `"Inspects abstract type"`, `"Compares abstract type"`, `"Non-decreasing recursive call"`)
  — rejected by `why`/`accepts`, treated as *inconclusive* (`nothing`) by `refute_at` so
  a valid member is never falsely refuted.

= Types as `(check, assume)` Pairs

Every type `T` is a plain pair `pair(check, assume)`:
- `T.fst` (`check`): `(E, fuel, next, v, k) -> Res` — verifies `v : T` in `E`, calling
  `k(E', fuel', next', v')` on success.
- `T.snd` (`assume`): `(u, backing, E) -> Eval` — installs the approved eliminators for
  handle `u : T` on top of `E`.

The `backing` parameter unifies checking and refuting without mode flags:
- `backing == "None"` (*symbolic mode*): `u` stands for an arbitrary member of `T`.
  `assume` installs symbolic elimination layers (case splitting, abstract barriers,
  equality rules, callback contracts).
- `backing == pair("Some", c)` (*concrete refuting mode*): `u` wraps a concrete candidate
  tree `c` drawn from `tried(n)`. First-order data formers check `c`'s membership with
  fueled `try_call` (`m_advance` with a step cap so diverging predicates never hang) and
  either substitute `u -> c` or `prune_ev(E)`; higher-order formers (`pi_t`, `sigma_t`,
  `ListT`) wrap `c` in a contract layer that validates inputs and prunes bad outputs on
  demand.

= Core `Eval -> Eval` Assumption Layers

Every assumption layer is a standalone, token-free `Eval -> Eval` combinator:

- *`with_subst(u, val, E)`* — extends `E.resolve` to map `u -> E.resolve(val)`.
- *`with_fact(cf, cx, ans, E)`* — intercepts `cf(cx)` and delivers `ans` (used by
  `refine_t` and `if`-branch path sensitivity in `base_fork_step`).
- *`with_eq(a, b, E)`* — extends `E.eq` with symmetric, acyclic downward transitivity for
  `a == b`; when either resolved side is a bare unknown `"U"` passing the occurs check,
  also installs `with_subst` so downstream evaluation rewrites it automatically. Together
  with `base_eq`'s structural and `tok_call` congruence rules (short-circuiting identical
  function heads `fa == fb`), this provides reflexivity, symmetry, transitivity, and
  congruence for both concrete recursive functions and abstract callbacks.
- *`with_data(pred, u, E)`* — intercepts `pred(u)` to return `true`, and intercepts any
  non-identity `triage` (or lazy S-frame) touching `u`: calls `E.kit.cases(pred, u)` to
  derive constructor shapes by symbolically running `pred` on a fresh probe tree,
  decrements `depth`, and forks evaluation across every constructor branch via `each`,
  installing `with_subst(u, shape)` and recursive `with_data` on every sub-unknown.
- *`with_abstract(tag, u, E)`* — seals `u` behind a parametricity wall: applying `u` or
  triaging `u` produces `pair("Fault", ...)`, comparing `u` via `==` returns `Err`, and
  `E.has_tag(tag, u)` answers `true` when tags match under `E.eq`.
- *`with_abs_type(u, real, E)`* — seals a universally quantified *type parameter* `X` in
  `forall_k`: blocks applying, triaging, and comparing `X` in user code (via non-fault
  refusals so valid members are never falsely refuted), while registering `u -> real` in
  `ev_unbox` so polymorphic instantiation (`forall_k.assume`) unboxes `X` inside type
  expressions.
- *`with_elim(elim_fn, elim_ty, R, u, E)`* — approves calling `elim_fn(u)` on an opaque
  handle only after verifying `elim_fn : elim_ty` (with step cap paused), then returning a
  fresh unknown assumed at `R`.
- *`with_sigma_sym` / `with_list_sym`* — lazily split unknown pairs `(a, b)` (evaluating
  dependent family `fam(a)` under `sure = true`) and container lists `[] | hd :: tl`
  (works on lists of functions like `ListT(NN)`).
- *`with_quot_sym(A, norm, q, E)`* — approves *only* `norm(q)` (returning a fresh `r : A`)
  and faults on raw triage or direct `==` on `q`, enforcing representation independence
  modulo `norm`.
- *`with_rec(subj, T_thunk, E)`* — arms a `fix` function after unfolding its first call,
  answering recursive calls via an unknown of `T_thunk` (outside `sure` type-family
  contexts). See @sec:termination for the structural decrease guard.
- *`with_lemma(lem_fn, T, E)`* — installed by `why` only after every lemma passes
  `E_check_base`; intercepts `lem_fn` on token-bearing arguments using a canonical lemma
  handle `tok_unk(pair("lem", lem_fn))`.

= Composable Type Formers & Recreated Type Theories

All type formers compose purely from their sub-types' `(check, assume)` pairs:

#figure(
  table(
    columns: 3,
    stroke: 0.4pt + gray,
    align: left,
    inset: 6pt,
    [*Former*], [*Semantics (`check` / `assume`)*], [*Type Theory / Capability*],
    [`AnyT`, `EmptyT`, `UnitT`], [Trivial accept/reject; `EmptyT.assume` prunes branch], [Top, Bottom, Unit],
    [`data_t(p)`], [Check `p(v) == true`; assume via `with_data(p)` / fueled `try_call`], [Inductive base types (`Nat`, `Bit`, `data_desc`)],
    [`pi_t(A, fam)`, `fn_t(A, B)`], [Quantify `arg : A`, run `f(arg) : fam(arg)`; assume checks domain, mints `tok_call(g, rx)` (or pauses `cap` and runs concrete `c(rx)`)], [STLC, LF, dependent functions, higher-order callbacks],
    [`fn_lazy_t(A, B)`], [Eagerly checks K-shaped constants at `B` without needing `A`], [Lazy / skip-preserving functions],
    [`sigma_t(A, fam)`], [Checks fork `(a, b)` with `a : A`, `b : fam(a)`; splits pair lazily], [Dependent pairs, existential specs],
    [`refine_t(A, p)`], [Checks `A` then `p(v) == true`; assumes `A` + `with_fact(p, u, true)`], [Refinement / Liquid-style types],
    [`the_t(A, target)`], [Checks `A` and `E.eq(v, target) == "True"`; assumes `with_eq`], [Singleton types],
    [`eq_t(a, b)`], [Inhabited by `t` when `E.eq(a, b) == "True"`; assumes `with_eq(a, b)`], [Propositional equality, MLTT `transport`],
    [`ListT(A)`], [Container list checking elementwise `A`; splits `[]` / `hd :: tl`], [Higher-order containers (`ListT(NN)`)],
    [`abs_t(tag)`, `forall_k(k, F)`], [Mints `k`-ary abstract type operator `X_real` behind opaque `X_unk`], [System F (`k=0`), System F-$omega$ / CoC (`k >= 1`)],
    [`quot_t`, `quot_pi`, `quot_fn`], [Check: `with_quot_sym` barrier; Refute: `f(q) == f(norm(q))` if idempotent else searches `q2` with `norm(q2) == norm(q)`], [Quotients modulo arbitrary (even non-idempotent) normalizers],
    [`opaque_with_elim(A, fn, R)`], [Verifies `fn : fn_t(A, R)` before approving `fn(u)` on opaque `A`], [Abstract data types with verified eliminators],
    [`bounded_pi(A, fam, bound)`], [Disables `with_rec`; runs `f(arg)` under `Cap(lim)` with oracle-step accounting], [Step-bounded totality & complexity bounds],
  ),
  caption: [Complete set of composable type formers in `lib/kernel/kernel.disp`.],
)

From these formers alone, `lib/kernel/pts.test.disp` reconstructs without extra primitives:
- *Lambda Cube:* STLC (`compose`, rejecting `{x} => x(x)`), System F (`church_nat`,
  `church_plus`, `church_bool`), type operators (`twice_op`), LF (`pi_t` over families),
  System F-$omega$ (`pair_op`, `poly_op`), $lambda P 2$ (`eq_base`, `trans_base`),
  $lambda P underline(omega)$, and the Calculus of Constructions (impredicative Leibniz
  symmetry `sym_any` and its negative converse).
- *System T:* `nat_iter`, `plus_t`, `times_t`, and higher-order Ackermann `ack_t`.
- *MLTT:* `nat_ind`, `transport_nat`, inductive proofs (`plus_zero_direct`, `append_empty`,
  `flip_twice`), existential specs (`double_proof`), and generic algebraic data types via
  an 18-line `data_desc` compiler (`BoolD`, `ListD`, `TreeD`, `len_d`, `append_d`, `size_d`).

= Termination & Totality <sec:termination>

Under partial `pi_t`, `with_rec` is sound for *partial correctness* (any terminating concrete
run finishes its recursive call in strictly fewer steps), so an infinite loop `spin` passes
`pi_t(Nat, {_n} => EmptyT)`. For total type theories (MLTT / CoC proofs), `pi_t` must also
reject non-terminating recursion while still accepting structurally recursive definitions.

== Lexicographic Strict-Subtree Termination

Because `with_data`, `with_list_sym`, and `with_sigma_sym` rewrite split unknowns via
`with_subst`, `deep_resolve(self, orig)` already holds the exact constructor tree into
which an initial argument `orig` has been refined (e.g. `pair(t, n')` for `Nat`,
`pair(hd, tl)` for `ListT`, `pair(1, [l, r])` for `TreeD`).

Define `strict_subterm(x, y)` to hold iff `x` appears strictly inside `y` by descending
*only* through real constructors (`stem` and non-token `fork`), *never* into `tok_sig`
nodes (`tok_unk` / `tok_call`).

#invariant("Subtree Well-Foundedness")[
  Because `strict_subterm` never descends through `tok_sig`, if `strict_subterm(x, y)` holds
  symbolically then for *every* concrete substitution $sigma$, $sigma(x)$ is a strict subtree
  of $sigma(y)$, so $"size"(sigma(x)) < "size"(sigma(y))$ in $NN$. Hence no equality
  assumption such as `eq_t(n, dbl_n(n))` can spoof a decrease, and infinite descending chains
  are impossible.
]

When checking a `fix` function under total `pi_t`, the evaluator tracks the active
quantified argument list `(orig_1, orig_2, ...)` and guards recursive calls
lexicographically:
- If `strict_subterm(deep_resolve(self, x_1), deep_resolve(self, orig_1))`, approve the
  recursive call immediately.
- If `deep_resolve(self, x_1) == deep_resolve(self, orig_1)` and further arguments remain,
  defer the check to `x_2` against `orig_2` (and so on), approving direct lexicographic
  recursion such as Ackermann `ack(m, n)` and curried folds such as `lib/list.disp`'s
  `reduce(step, base, xs)`.
- Otherwise try right-to-left (RTL) lexicographic decrease (still rejecting oscillating
  or swapping recursion), or reject with `"Non-decreasing recursive call"` (a non-fault
  refusal, so `refute` is unaffected).

Explicitly partial functions remain available via `partial_pi` / `partial_fn` (which also
supports Scott induction for first-class recursive self-reference such as `c_fold`, while
rejecting `TotalRecArmed` leaks into total `Fn`), and fixed cost bounds remain available
via `bounded_pi` / `bounded_t`.

= Surface Syntax Integration, Certificates & Verified Rewrites

Disp's module driver (`src/elab/driver.ts`, `src/elab/vocab.ts`) decouples surface syntax
from any particular kernel through a small scope protocol implemented in
`lib/kernel/surface.disp` (re-exported by `lib/kernel/prelude.disp`):

- *Binder desugaring (`Pi`):* `{x : A} -> B` desugars to `Pi(A, {x} => B)` and `A -> B`
  to `Pi(A, {_} => B)`. Currying `Pi` on `A` lets closed domains fold at compile time,
  and routing `Pi(Type, fam)` to `forall_t(fam)` (and `Pi(TypeFam, fam)` to
  `forall_fam_t(fam)`) compiles `{A : Type} -> A -> A` directly to evaluator-map types.
- *Deferred module verification (`check_module`) & cross-module certificates:* Whenever a
  non-`raw` `use`d module has `check_module`, `make_record`, `list_const`, and `true` in
  scope, the driver invokes `check_module([(name, typ), ...], make_record(names, vals))`.
  `check_certified` checks annotated exports in file order, promoting verified functions
  as lemmas for subsequent entries, and `certify` / `cert_pick` export unforgeable
  cross-module certificates so downstream checked modules (`lib/kernel/std_*.disp`) reuse
  proved lemmas without re-checking.
- *Declaration guard protocol & verified `.opt.disp` rewrites:* `lib/kernel/surface.disp`
  provides the declaration protocol (`default_guard`, `freeze`, `base`, `let`, `sig`,
  `guard`, `given`) and relation-generic rewrite guards (`rel_guard`, `equiv_guard`,
  `fn_equiv_guard`, `fn2_equiv_guard`). A guarded export can only be rebound by supplying
  `{ new := fast; proof := pf }` (and optional certified lemmas `certs`) that passes
  `check_certified` against `rel(old, new)`. `lib/kernel/verified_opt.disp` and
  `lib/kernel/verified_opt.opt.disp` demonstrate verified in-place upgrades (`add_nat`,
  `dbl`, `rev` → `rev_fast`), pinned by `lib/kernel/guards.test.disp` and
  `test/guards.test.ts`.

= Soundness, Step-Indexed Simulation, and Stream Types

Soundness states that checking never contradicts refutation:
$
  "why"([], T, v) = "Yes" quad ==> quad forall n in NN, "refute_at"(T, v, n) = "Nothing".
$

== Per-Layer Covering Laws

Because every type builds on the same base machine via stacked `Eval -> Eval` layers,
soundness factors modularly into one local *covering law* per layer, proved by outer
induction on the concrete refuter's step budget $k$ (a step-indexed logical relation,
needed because impredicative `forall_k` instantiates types with larger types) and inner
induction on machine steps:

1. *Splitting (`with_data`, `with_list_sym`, `with_sigma_sym`):* every concrete member $c$
   of the type matches at least one derived constructor branch (`derive_cases` completeness).
2. *Shortcuts (`with_fact`, `with_elim`, `with_lemma`, `with_rec`):* the abstract answer
   covers the concrete call's outcome (for `with_rec`: any terminating concrete run executes
   the recursive call in strictly fewer steps, covered by the outer induction on $k$; under
   totality, strict subtree decrease additionally guarantees termination).
3. *Equalities (`with_eq`, `base_eq`):* symbolic `"True"` implies concrete tree equality
   under every valid filling $sigma$.
4. *Abstraction walls (`with_abstract`, `with_abs_type`, `with_quot_sym`):* blocked
   operations prevent the run from observing representation differences across valid fillings
   (parametricity for `forall_k`, representation independence modulo `norm` for `quot_t`).
5. *Oracle-step accounting (`bounded_pi` + `pi_t.assume`):* pausing `Cap` inside concrete
   callback invocations ensures concrete caller steps match symbolic caller steps 1-for-1.

Every bug found during adversarial stress testing (inspecting `forall_k`'s type argument,
unfueled host predicates in `refute`, non-idempotent quotient normalizers, order-2
`bounded_t` callback steps, and unverified `opaque_with_elim` specs) corresponded directly
to a missing invariant in one of these five laws.

== Relationship to Stream Types

Disp's stream layer (`lib/stream.disp`, `lib/claim.disp`, `archive/STREAM_TYPES_PLAN.md`,
`research/STREAM_TYPES_ONTOLOGY.md`) is the dual half of this architecture:

- *Honest refutation as an `Items` stream:* Replacing `first_found(tried(n), ...)` in
  `refute_kit.quantify` with a coalgebraic `Items(Tree)` counterexample stream separates
  `exhausted` (truly `"Proved"` on finite domains such as `Bit -> Bit`) from `spent`
  (`"Open"` — ran out of budget) from a yielded counterexample (`"Refuted"`).
- *`Eval` fills `archive/STREAM_TYPES_PLAN.md` Part 3:* The layered `Eval` machine is the
  modular symbolic machine over `lib/machine.disp` that Part 3 left open, and
  `sound_refuter` is an instance of `lens_holds` (`lib/claim.disp`).
- *What streams prove vs. what requires induction:* Base dispatch rules (`K`, `S`, `triage`,
  `tree_eq`) over finite rule shapes can be *proved by exhaustion* inside disp via `over`.
  Because symbolic checking branches (`with_data`) and takes multi-step shortcuts
  (`with_rec`, `pi_t`), lifting per-step soundness to all infinite trees and arbitrary run
  lengths requires a *branching, multi-step, step-indexed simulation* rather than 1-to-1
  lockstep trace equality.

= Open Extensions

The four remaining architectural milestones toward a universal type-theory generator are
detailed in `FINAL_TYPE_SYSTEM_NEXT_STEPS.md`: (1) extensible `meta` state for
linear/affine/effect/cost systems, (2) computational path witnesses for HoTT,
(3) stratified universes `Type_i` and Goal-4 callable types, and (4) `lib/stream.disp`
refuter integration, mechanized soundness, and retiring `lib/theories/checked.disp`.
