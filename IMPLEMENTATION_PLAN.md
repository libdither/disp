# Kernel Implementation Plan

Track the rewrite of `lib/kernel.disp` to match the design in
[`TYPE_THEORY.typ`](TYPE_THEORY.typ). The current `lib/kernel.disp`
is partway-soundness-fixed; this plan replaces it cleanly rather
than incrementally patching.

## Scope

**Files rewritten:**
- `lib/kernel.disp` — full rewrite per TYPE_THEORY.typ §2-§7.

**Files deleted:**
- `lib/checked.disp`, `lib/checked.test.disp` — session-1 walker
  superseded by the new dispatcher's walker default.

**Files updated:**
- `lib/kernel.test.disp` — adapt to new APIs (eliminator signatures
  take an `OrdLt`-bounded rank arg; `OrdLt k` type-former added; the
  `q_type_fn` references become `q_core_type_fn` / `q_guarded_type_fn`).
- `lib/soundness.test.disp` — expand with adversarial tests for each
  parametric rule (stem rule, S rule, triage-fork rule).
- `src/compile.ts` — update `makeKernelHelpers` for new metadata
  layouts and add `OrdLt` reflection.

**Files unaffected:**
- `lib/prelude.disp` (already provides `select_chain`, `case`,
  `match`, `tree_eq`, etc.).
- `src/tree.ts` (runtime stays as-is per design; native walker
  optional, deferred).
- All non-kernel lib files (`fin.disp`, `nat.disp`, `list.disp`, etc.).

## File structure within `kernel.disp`

The file is structured top-down: imports, helpers, handlers, kernel
record, public API. recq makes cross-handler references lazy, so
definition order within sections doesn't strictly matter, but the
sections themselves should follow this order:

```
1.  Imports                       open use "lib/prelude.disp"
2.  CheckedResult primitives      Ok, Fail, is_ok, ok_value,
                                  must_ok_any, must_ok_tt,
                                  must_ok_concrete_tt
3.  Trusted helpers (private)     q_is_neutral, cert_make_hyp,
                                  cert_make_stuck, q_contains_neutral,
                                  q_scan_no_neutral, q_unguard_or_self,
                                  metadata accessors
4.  Handler bodies (private)      q_h_rule_fn (shared), q_hyp_reduce_fn,
                                  q_guard_fn, q_pi_fn, q_nat_fn,
                                  q_bool_fn, q_eq_fn, q_ord_fn,
                                  q_ord_lt_type_fn, q_core_type_fn,
                                  q_guarded_type_fn, q_ord_lt_fn,
                                  q_ord_le_fn, q_ord_max_fn,
                                  q_bool_rec_fn, q_nat_rec_fn,
                                  q_eq_J_fn, q_ord_rec_fn, q_wait_rec_fn,
                                  q_elim_fail_fn, q_checked_apply_fn
5.  Kernel record                 kernel := recq { ... 22 fields ... }
                                  kernel_ref := {q} -> wait kernel q
6.  Cores (private)               core_Nat, core_Bool, core_Eq,
                                  core_Pi, core_Ord, core_OrdLt
7.  Public type constructors      Nat, Bool, Eq, Pi, Arrow, Ord,
                                  OrdLt, Type, Hyp, StuckElim
8.  Reflection helpers            hasguard, unguard_or_self,
                                  unguard_checked, is_pi, is_universe,
                                  is_eq, pi_dom, pi_cod_fn,
                                  universe_rank, pi_rank
9.  Eliminator wrappers           bool_rec, nat_rec, eq_J, ord_rec,
                                  wait_rec, eq_subst, eq_sym, eq_cong
10. Public re-exports             checked_apply
11. Ordinal library               omega, omega_succ, succ_ord
```

Section numbers correspond to definitions, not to phases.

## Phases

Each phase ends with a green test suite. Within a phase, multiple
commits are fine as long as the suite stays green at each one.

### Phase 1 — Foundation

Implement the dispatcher and walker with the minimum kernel
infrastructure needed to mint test neutrals. This lets the
soundness theorem be validated against the full adversarial
suite (including triage-on-neutral and reflective attacks).

Items:
- §2 CheckedResult primitives + helpers.
- §3 Trusted helpers + metadata accessors, including
  `cert_make_hyp` (one-line: `wait kernel.hyp_reduce
  (make_neutral_meta type id)`).
- §4 Minimal `q_hyp_reduce_fn` (raw runtime; for Phase 1 it
  doesn't need full type-tracking — a placeholder return is
  fine, since Phase 1 tests don't apply minted neutrals).
- §4 `q_checked_apply_fn` with the hyp_reduce dispatch case
  registered (so neutrals are recognised), no other type-former
  cases yet.
- Walker rules (`checked_raw_apply`).
- §10 Public re-export of `checked_apply`.

Tests (new, in `lib/soundness.test.disp`):
- SKI transport on closed values: `(K x y) = x`, etc.
- I-shortcut fires on the canonical I tree but NOT on near-miss
  triage-shaped values (e.g., `{x, y} -> x`, which contains I as
  a subterm but isn't I).
- Forged neutral via stem rule rejected:
  `checked_apply (stem kernel.hyp_reduce) some_meta = Fail`.
- Forged neutral via S rule rejected (transitively).
- Forged neutral via triage-fork rule rejected (transitively).
- Triage on neutral fails (using a `cert_make_hyp`-minted
  hypothesis as target).
- Forgery-via-`wait`: `apply (apply wait raw.hyp_reduce) meta`
  from user position is rejected at some step (covers the
  intricate "forgery-via-wait is impossible" argument in
  TYPE_THEORY.typ §5.3).

Phase exit criterion: walker is correct on the four parametric
rules; full neutral-aware adversarial suite passes.

### Phase 2 — Boundary + classic checkers

Add the type-checker handlers and re-validate end-to-end tests.

Items:
- §4 Handlers: `q_guard_fn`, full `q_hyp_reduce_fn` (now
  returning `CheckedResult`: `Ok new_neutral` for Pi-typed,
  `Fail` for non-Pi-typed neutral application — see
  TYPE_THEORY.typ §3.4), `q_h_rule_fn`, `q_pi_fn`, `q_nat_fn`,
  `q_bool_fn`, `q_eq_fn`.
- §4 Update `q_checked_apply_fn` dispatch list to include these.
  Per TYPE_THEORY.typ §7, the handler-dispatch invariant
  applies: every type-checker handler must `is_neutral`-dispatch
  to H-rule before raw-triaging on its argument.
- §5 Kernel record gets these fields.
- §6 Cores: `core_Nat`, `core_Bool`, `core_Eq`, `core_Pi`.
- §7 Public constructors: `Nat`, `Bool`, `Eq`, `Pi`, `Arrow`,
  `Hyp`, `StuckElim`.

Tests:
- Port existing `lib/kernel.test.disp` cases that don't depend on
  universes, ordinals, or eliminators. Most should pass unchanged.
- Soundness regression: `(Type 0) (Hyp Nat 0) = FF` (entry scan
  rejects forged neutral). Won't have `Type 0` until Phase 3, but
  the closely-related `(Nat) (Hyp Nat 0) = FF` works here.

Phase exit criterion: classic typing tests green (Pi/Nat/Bool/Eq
universes-aside).

### Phase 3 — Ordinals + bounded levels

Add Ord, OrdLt, universe checkers, and the comparison primitives.

Items:
- §4 Handlers: `q_ord_fn`, `q_ord_lt_type_fn`, `q_ord_lt_fn`,
  `q_ord_le_fn`, `q_ord_max_fn`, `q_core_type_fn`,
  `q_guarded_type_fn`.
- §4 Update dispatch list.
- §6 Cores: `core_Ord`, `core_OrdLt`.
- §7 Public constructors: `Ord`, `OrdLt`, `Type`.
- §11 Ordinal library: `omega := omega_plus 1 0_ord`,
  `succ_ord := {a} -> omega_plus 0_ord a`. The CNF invariant
  requires `leading_exp a ≤ 0_ord`; for closed `a` this is
  decidable (limits to finite below-ω values), and for
  hypothesis `a` it requires bound-consulting (e.g., `a : OrdLt
  omega` forces leading_exp ≤ 0). Document both cases.

Tests:
- Closed universe tests: `(Type 0) Nat = TT`, `(Type 1) (Type 0) = TT`,
  `(Type 0) (Type 0) = FF`.
- OrdLt test: `(OrdLt omega) 5 = TT`, `(OrdLt 3) 5 = FF`,
  `(OrdLt omega) omega = FF`.
- Bounded-polymorphism test: trace `pi_rank` for
  `Pi (OrdLt omega) ({r} -> Type r)`, expect closed `omega`.
- Stuck propagation tests: ord_lt with hypothesis arg returns
  Ok stuck-bool.

Phase exit criterion: closed-rank universes + bounded-rank
polymorphism work; tests that depend on universe checks pass.

### Phase 4 — Eliminators + reflection

Add eliminator handlers, user-facing wrappers, reflection helpers.

Items:
- §4 Handlers: `q_bool_rec_fn`, `q_nat_rec_fn`, `q_eq_J_fn`,
  `q_ord_rec_fn`, `q_wait_rec_fn`, `q_elim_fail_fn`.
- §4 Update dispatch list.
- §5 Kernel record extended with `elim_fail` signature.
- §8 Reflection helpers (defined via `wait_rec`; auto-unguard at
  the wait_rec handler — no `on_guard` branch). Per
  TYPE_THEORY.typ §6.5, simple recognition (`is_pi`, `is_nat`,
  etc.) is wait_rec-based for parametricity-respecting behaviour
  in checked code. `has_sig` remains available internally for
  handler bodies and elaborator code that knows inputs are
  closed.
- §9 Eliminator wrappers.
- Update `src/compile.ts` `makeKernelHelpers` for the new metadata
  layouts.

Tests:
- Port existing eliminator tests with new signatures
  (`bool_rec` now takes `rank : Ord` first arg).
- Adversarial: malformed motive rejected; mis-ranked motive
  rejected.
- Reflection: `is_pi (Pi Nat ({_} -> Nat)) = TT`,
  `pi_dom (Pi Nat ...) = Nat`, etc.
- Polymorphic-bounded eliminator: `bool_rec rank motive ...` where
  rank is bounded.

Phase exit criterion: full kernel suite green; old kernel.disp can
be deleted; soundness attacks (1-5 from earlier work) all rejected.

## Open implementation questions, resolved

These were unresolved at planning time. Decisions:

### Q1: OrdLt's metadata storage

Just `k` (the bound). The handler computes `Ord v ∧ ord_lt v k`
each time. No derived form, no caching beyond apply-memo's natural
hash-cons caching.

```disp
core_OrdLt   := {k} -> wait kernel_ref.ord_lt_type k
OrdLt        := {k} -> guard (core_OrdLt k)

q_ord_lt_type_fn = {ks, raw, query} -> {self, k, v} ->
  must_ok_concrete_tt (ks.checked_apply core_Ord v)
    ({_} -> ks.checked_apply (wait ks.ord_lt v) k)
    ({stuck} -> Ok stuck)
```

### Q2: Bound-consulting identity table

Phase 3 ships only the **ω identity**:

```
For r_hyp with stored type OrdLt omega:
  ord_lt r_hyp omega → Ok TT                    (direct from bound)
  ord_le (succ_ord r_hyp) omega → Ok TT         (limit identity)
  ord_le r_hyp omega → Ok TT                    (weakening)
```

Recognising `omega` is a tree_eq against the canonical
`omega_plus 1 0_ord`. Other limit ordinals (`omega+1`, `omega·2`,
`omega^2`) follow the same shape but are NOT shipped in Phase 3.
Add reactively as use cases demand. Each new identity is one new
case in `q_ord_lt_fn`/`q_ord_le_fn` matching against the
canonical tree of that limit ordinal.

Ship a small comment block in `kernel.disp` enumerating which
limit ordinals are bound-conslulting-supported, so the table is
auditable from one location.

### Q3: wait_rec's `cod_at_canonical_hyp`

Use the Pi metadata as the hypothesis identity, matching current
kernel behavior:

```disp
let hyp = q_make_hyp raw dom meta   // identity = Pi metadata
let cod_at_hyp = (pi_meta_cod_fn meta) hyp
```

Hash-cons stability of `meta` per Pi binding ensures distinct Pi
types produce distinct hypotheses. Same as today's `q_make_hyp`
in the existing kernel.

### Q4: Test-file reorganization

After Phase 4 lands, consolidate:

- `lib/checked.test.disp` → DELETE (superseded).
- `lib/soundness.test.disp` → EXPAND with adversarial tests for
  each parametric rule.
- `lib/kernel.test.disp` → UPDATE for new APIs, keep as the
  "happy path" tests.

Single soundness file is easier to audit than the current split.

**Sequencing.** Inventory and port FIRST, delete SECOND. Read
every test in `lib/checked.test.disp`, port the equivalent
(adapted to the new APIs) into `lib/soundness.test.disp`
during the corresponding phase, *then* delete. "Tests green"
only catches regressions in things the new suite tests; anything
in the old suite without a counterpart silently disappears if
deletion happens before the inventory pass.

### Q5: `src/compile.ts` updates

`makeKernelHelpers` (~line 401) needs:

- New `isOrd`, `isOrdLt`, `ordLtBound` predicates.
- Updated `isPi`, `piDomain`, `piCodFn` (no behavior change, but
  may need to look through guard layer differently if metadata
  layout shifts).
- New `isUniverse` returning the rank as an Ord (was Nat).
- Updated `samplePi` reference to use the new public `Pi` shape.

The trust-table interaction stays the same: kernel exports public
constructors, elaborator reads them via `trust open`.

**API break: public types are guarded.** All public type values
now have `pair_fst = kernel.guard`. Signature-recognition for the
underlying type-former requires `unguard_or_self` first. Any
test/elaborator code currently doing `has_sig kernel.nat Nat`
directly will break silently (returns FF instead of TT); update
to `has_sig kernel.nat (unguard_or_self Nat)`. Audit
`makeKernelHelpers` for direct sig-checks against public types.

## What's NOT changing

Calling out to prevent surprise:

- `src/tree.ts` runtime — no changes. Hash-cons, apply, tree_eq
  fast path all stay.
- `lib/prelude.disp` — no changes. `select_chain`, `match`,
  `tree_eq`, etc., are reused as-is.
- recq mechanism — same as today; kernel record is built via
  `recq { ... }` with lazy field references.
- The parser — no new syntax. `Type 0`, `Type omega` work via
  existing identifier resolution.

## Risk areas

### Walker performance

Doc estimates ~70× over raw apply for in-language walker. Full
suite under walker may be slow. If unacceptable in Phase 4, native
walker in `tree.ts` becomes Phase 5 (mirror the `tree_eq` fast
path: capture dispatcher tree id at boot, route in TS).

Threshold: if `npm test` exceeds ~30s after Phase 4, write the
native walker.

### Bracket-abstraction blowup in deep CPS chains

Eliminator handlers (`q_bool_rec_fn` etc.) have nested `must_ok_*`
chains; these may compile to surprisingly large trees due to
match-desugar re-capture. Watch compile time during Phase 4. If a
single eliminator handler takes >5s to compile, refactor to extract
helpers to top level (proven approach from earlier sessions).

### OrdLt bound-consulting correctness

Each ω-identity in `q_ord_lt_fn` must be carefully verified. Easy
to write a wrong identity that admits unsound comparisons. Phase 3
must include a soundness test specifically for each shipped
identity:

- "Any concrete `r < omega` makes `ord_lt r omega = TT`."
- "Any concrete `r ≥ omega` makes `ord_lt r omega = FF`."
- "Hypothesis `r : OrdLt omega` makes `ord_lt r omega = TT`
  (via bound)."
- "Hypothesis `r : Ord` (no bound) makes `ord_lt r omega = stuck`."

## Where I'd start

Phase 1. The dispatcher and walker are the load-bearing parts.
Validating them against constructor-check tests directly proves
the soundness theorem holds in code, and the rest of the phases
are mechanical from there.

## After implementation

Once Phase 4 lands:
1. Delete `lib/checked.disp` and `lib/checked.test.disp`.
2. Update `CLAUDE.md` to reflect the new design.
3. Decide whether to revisit `KERNEL_EXTENSIBILITY_PLAN.md` (open
   registry) or move on to elaborator improvements.
4. Profile and decide whether to write the native walker
   (deferred from Phase 4 unless tests reveal it's needed).
