# Kernel Extensibility Plan: Parametric Handler List

## Goal

Lift the kernel from a closed record
`{hyp_reduce, pi, nat, bool, eq, type}` to a parametric kernel where
the handler list is a value users can extend. Adding a new type
(`List`, `Vec`, `Sigma`, ...) becomes "register a handler", not "edit
`lib/kernel.disp`".

This plan presupposes that
[`SOUNDNESS_FIX_PLAN.md`](SOUNDNESS_FIX_PLAN.md) has landed and that
the dispatch-table-as-security-perimeter contract is in place. The
extensibility refactor is about lifting that contract from a fixed
list to a parameterized one without weakening it.

## Why parametric

The closed record has three problems:

1. **Adding a type requires editing the kernel.** `Vec`, `List`,
   `Sigma`, `W`-types, propositional truncation, etc. are all things
   users may want, and each currently means a kernel patch.
2. **The security perimeter is implicit.** It lives as conditional
   branches in `q_type_fn`, `checked_apply`'s dispatch, and the
   eliminator handlers. There's no single value you can inspect to
   answer "what types does this kernel know about?"
3. **The kernel is not first-class.** The project's stated goal is
   self-hosting and neural-guided synthesis. A non-first-class kernel
   can't be passed to a synthesis search, swapped for a stricter
   variant, or composed.

Parametric kernel makes the handler list a tree-calculus value: a
record of `Handler` records, each describing one type former. The
kernel is a function from a handler list to a kernel record;
`checked_apply`'s dispatch enumerates the list rather than testing
hard-coded signatures.

## Handler Interface

A handler is a record describing one type former. Every entry in the
default handler list satisfies this contract:

```disp
Handler = {
  // The checker signature this handler claims. Used for is_my_type
  // dispatch and as the security-perimeter identity.
  checker_sig    : Tree;

  // The wait-based core checker (raw, fast). Invoked by certified
  // type-checking code; mints neutrals via cert_make_* helpers.
  core_checker   : Tree;

  // checked_apply hook. Invoked when checked_apply sees a wait-based
  // value with this handler's checker_sig. Validates user inputs via
  // checked_apply before minting any neutral. The CONTRACT from
  // SOUNDNESS_FIX_PLAN.md applies here verbatim.
  checked_apply  : {f, x} -> CheckedResult;

  // Universe rule: given the handler's metadata, return the universe
  // this type former lives in (a Nat). For type-formers like Pi this
  // computes max(level domain, level codomain); for Nat/Bool, returns
  // 0; for `Type n`, returns succ n.
  universe_level : {meta} -> Nat;

  // Eliminator hooks (optional). If this type former has eliminators
  // (bool_rec, nat_rec, eq_J, ...), they're sub-handlers registered
  // in the same handler-record.
  eliminators    : RecValue;
}
```

The core checker mints neutrals via `cert_make_*` helpers exposed by
the kernel. Those helpers are themselves part of the trusted runtime
and not user-overridable — that boundary is the *real* security
perimeter, which the parametric kernel does not weaken.

## Kernel as a Function

The closed kernel today:

```disp
kernel : {hyp_reduce, pi, nat, bool, eq, type} := recq { ... }
```

becomes:

```disp
make_kernel : Handlers -> Kernel
make_kernel = {handlers} -> recq {
  // Always-present primitives:
  hyp_reduce := q_hyp_reduce_fn;
  guard      := q_guard_fn;

  // Per-handler dispatchers:
  type       := q_type_fn handlers;     // walks `handlers`
  checked    := q_checked_apply handlers;

  // Per-handler entries derived from the list:
  __handlers := handlers
}
```

`Handlers` is a recValue: a Church-encoded record of `name :=
Handler` fields. The default kernel registers
`{nat, bool, eq, pi, ...}`.

A user extends the kernel:

```disp
my_kernel := make_kernel
  (extend_handlers default_handlers
    list := list_handler;
    vec  := vec_handler)
```

`extend_handlers` is record-update: add or replace a handler.

## Dispatch as List Walk

`checked_apply` today is a `select_chain` of hard-coded signature
checks. Under the parametric kernel:

```disp
q_checked_apply handlers := fix ({self, f, x} -> {
  // Walk handlers; each handler records its checker_sig.
  // First match wins.
  let matched = handlers_find_by_sig handlers (pair_fst f)
  select_lazy
    ({_} -> matched.checked_apply f x)
    ({_} -> checked_raw_apply self f x)
    (handlers_has_match matched)
})
```

Same structure as today's hard-coded `select_chain`, just walking
data instead of branching on hard-coded constants. The
security-perimeter contract is preserved: every entry must validate
inputs before minting neutrals. The list walk just changes *where*
the contract is checked from "in source" to "in the registered
handler value at this id".

## Type Constructors

Today every `Nat`, `Bool`, etc. references a global `kernel_ref`. In
the parametric world, type constructors take the kernel they belong
to:

```disp
default_Nat  : Type 0 = make_Nat default_kernel
make_Nat k = guard (wait k.nat.core_checker t)
```

Most user code uses `default_kernel`'s pre-applied constructors:

```disp
Nat  = make_Nat default_kernel
Bool = make_Bool default_kernel
Type = make_Type default_kernel
```

The default constructors stay as the convenient names users import.
Power users who construct alternative kernels use `make_X
their_kernel` to instantiate constructors against their kernel.

## Identity of `Nat` Across Kernels

Hash-consing means that `make_Nat k1` and `make_Nat k2` produce
*different* tree ids if `k1.nat.core_checker != k2.nat.core_checker`.
Two libraries built against different kernels stop interoperating at
the type level: `tree_eq Nat_in_k1 Nat_in_k2 = FF`.

This is the right behavior. Two kernels with different `Nat` checkers
(e.g., one accepts only structural nats, another accepts unary
encodings) are genuinely different type theories. Mixing them is a
soundness hazard.

The expected workflow:

1. Most code uses `default_kernel` and its built-in constructors.
   `Nat` is one canonical tree id, hash-cons-stable across the project.
2. Library authors who extend the kernel publish *their* kernel
   alongside their constructors. Downstream users build against that
   kernel.
3. Composing libraries that use different kernels requires an
   explicit `kernel_compose` operation that merges handler lists, with
   conflicts resolved (or rejected) explicitly.

## Bootstrap

Today `kernel := recq { ... handlers ... }` builds the kernel as a
closed top-level value. `kernel_ref := {q} -> wait kernel q` is the
lazy proxy used by handlers to refer back to the kernel.

Under parametric:

1. `default_handlers` is built first as a closed `recq` over handler
   records — handlers can reference each other lazily because they're
   all defined in the same `recq`.
2. `default_kernel := make_kernel default_handlers` builds the kernel
   record around `default_handlers`. The kernel's `__handlers` field
   stores the handler list.
3. Type constructors `Nat`, `Bool`, etc. are
   `make_X default_kernel`.
4. `kernel_ref` is replaced by `default_kernel_ref := {q} -> wait
   default_kernel q`. Handler bodies that need self-reference take
   the kernel as a parameter:
   `make_pi_handler kernel = ...`.

The mutual recursion `Pi -> Type -> Pi` (Pi codomain types are
checked against `Type`, which dispatches to handlers including Pi)
goes through `recq` as today.

The trick is that the **handler record is fixed once built**. Adding
a handler means building a *new* kernel:

```disp
let extended = make_kernel (extend_handlers default_handlers list := list_handler)
```

Hash-consing guarantees that `default_kernel`'s handlers are stable;
extending produces a new, distinct kernel.

## Interaction with the Security Perimeter

The dispatch-table-as-security-perimeter contract from
`SOUNDNESS_FIX_PLAN.md` becomes a contract on the **handler list**,
not on a hard-coded list of cases. Every handler record commits to:

- A unique `checker_sig` identifying it.
- A `checked_apply` hook that validates all user inputs via
  `checked_apply` before minting neutrals.
- An `eliminators` record where every eliminator handler satisfies
  the same contract.

The first review point shifts from "diff in `checked_apply`'s
`select_chain`" to "diff in the handler list registered with
`make_kernel`". Tooling should verify:

- No two handlers share a `checker_sig`.
- Every handler has a `checked_apply` (i.e., the list is closed under
  the contract).
- Every eliminator's motive/branches go through `checked_apply`.

A linter rule (or even a kernel-level handler-shape check) can
enforce some of this mechanically: e.g., `make_kernel` verifies that
all `checker_sig` values in the handler list are distinct.

## Migration

Phased rollout:

**Phase 1 — Internal refactor (no surface change).**
Rewrite the existing closed kernel as `make_kernel default_handlers`
where `default_handlers = recq { nat := nat_handler; ... }`. Public
constructors `Nat`, `Bool`, etc. point at
`make_X default_kernel`. All existing tests pass unchanged.

**Phase 2 — Expose `extend_handlers`.**
Add `extend_handlers` and `make_kernel` as public. Document the
handler interface contract. Add unit tests for a trivial handler
(e.g., `Unit` type).

**Phase 3 — First non-trivial extension.**
Implement `List A` or `Sigma A B` as an out-of-tree handler in
`lib/list.disp` / `lib/sigma.disp`. Validate that the dispatch-table
contract is enforceable in user-space.

**Phase 4 — Document and stabilize.**
Move the handler-extension docs into `TYPE_THEORY.typ` /
`KERNEL_DESIGN.md` as first-class. The contract becomes a
public-API stability concern.

## Open Questions

1. **Eliminator extensibility.** Today `bool_rec`, `nat_rec`, `eq_J`
   are kernel-level constants. Under the parametric kernel they
   become fields of their type former's handler. What's the user-side
   syntax for invoking them? A projection like `Bool.rec motive ...`,
   or globally-imported sugar that picks the relevant handler from
   the ambient kernel?

2. **Conversion across handler-list extensions.** If kernel `k2`
   extends `k1`, can a value typechecked under `k1` be used in `k2`
   freely? Likely yes, since `k2`'s handler list is a superset and
   `k1`'s checkers are unchanged. But formal proof of conservativity
   is desirable.

3. **Universe polymorphism.** Today `Type` takes a rank. With
   per-handler universe-level functions, do we want `Type`-of-handlers
   (a universe of type formers)? This is well past the immediate
   scope but worth flagging.

4. **Performance of dispatch by list walk.** Today `checked_apply`'s
   dispatch is a flat `select_chain` over O(10) constants. With
   parametric handlers it's a list walk over user-supplied entries —
   linear in the number of registered types. With ~10s of types
   that's fine; with 100s, indexing by signature (a hash-cons-based
   record of `signature → handler`) becomes worth it.

5. **Kernel composition.** Merging two extension lists (`kernel_compose
   k1 k2`) needs a policy for conflicting handlers. Reject? Prefer
   left? User-supplied tiebreaker? Probably reject by default and
   provide an explicit override mechanism.

## Out of Scope

- Anything that weakens the
  [security perimeter](SOUNDNESS_FIX_PLAN.md#dispatch-table-as-security-perimeter).
  The handler list IS the perimeter under this design; broadening
  what counts as a handler must not bypass the contract.
- Runtime-mutable kernels. Each `make_kernel` call returns a fresh
  hash-consed kernel value; "extending" produces a new kernel. This
  is intentional — it preserves the invariant that any term
  typechecked against a kernel is typechecked against a fixed
  handler list.
- Cross-language kernel sharing (e.g., serializing a kernel for use
  in another runtime). Possible later, not in scope here.
