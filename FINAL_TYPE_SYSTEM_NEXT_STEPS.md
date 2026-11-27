# Final Type System: Next Steps

**Status:** The evaluator-map type system is now Disp's promoted core kernel at [`lib/kernel/`](lib/kernel/):
- [`lib/kernel/kernel.disp`](lib/kernel/kernel.disp) — core `(check, assume)` evaluator-map engine on a 7-field `Kit` with zero custom runtime opcodes, structural & lexicographic termination (LTR + RTL fallback), Scott induction for `PartialFn`, and unforgeable cross-module certificates (`certify`, `check_certified`).
- [`lib/kernel/surface.disp`](lib/kernel/surface.disp) & [`lib/kernel/prelude.disp`](lib/kernel/prelude.disp) — surface `:` vocabulary (`Pi`, `Fn`, `PartialPi`, `Sigma`, `Eq`, `Refine`, `Quotient`, `Bounded`, `Opaque`, `Forall`, `Implicit`, `Data`), `check_module`, declaration guard protocol (`default_guard`, `freeze`, `let`, `sig`, `guard`, `given`), and verified rewrite guards (`rel_guard`, `equiv_guard`, `fn_equiv_guard`, `fn2_equiv_guard`).
- Full stdlib re-typechecking in `lib/kernel/std_*.disp` (`std_prelude`, `std_list`, `std_verdict`, `std_trees`, `std_machine`, `std_growth`, `std_stream`, `std_stream_extra`, `std_claim`), verified `.opt.disp` rewrites (`lib/kernel/verified_opt.disp`, `lib/kernel/verified_opt.opt.disp`), and test suites (`lib/kernel/{kernel,pts,std,std_machine,std_stream,std_stream_extra,std_claim,guards}.test.disp` + `test/guards.test.ts`).
- Earlier kernels and prototypes are retired under [`archive/live-kernel/`](archive/live-kernel/), [`archive/provenance-kernel/`](archive/provenance-kernel/), and [`archive/type-system-goals/`](archive/type-system-goals/).

See [`TYPE_SYSTEM.typ`](TYPE_SYSTEM.typ) for the full specification. Below are the remaining milestones toward a universal type-theory generator, in priority order.

---

## 1. Extensible `meta` state for resources, linearity, and effects

**Why it matters:** Today `Eval` layers describe *what values are* (`step`, `lazy`, `resolve`, `eq`, `has_tag`), while changing per-run state lives in a fixed tuple `meta = (depth, sure, cap, unbox)`. Substructural type theories (linear, affine, graded/usage-counted, Rust-style ownership/borrowing) and algebraic effect / coeffect systems need *mutable-along-a-run* state (usage ledgers, borrow regions, effect logs) that updates on every step without discarding stacked assumption layers.

**Design:**
- Generalize `meta` from a fixed tuple into an extensible keyed state map (or slot list) threaded through `run_ev`, where each layer can read/update its own state key on step transitions and branch merges.
- Validate by recreating [`lib/theories/affine.test.disp`](lib/theories/affine.test.disp), [`lib/theories/graded.test.disp`](lib/theories/graded.test.disp), and [`lib/theories/cost.test.disp`](lib/theories/cost.test.disp) as plain `(check, assume)` formers on `lib/kernel/` with zero changes to the core `run_ev` loop.

---

## 2. Higher-dimensional equality (HoTT / computational paths)

**Why it matters:** Today `eq_t(a, b)` is proof-irrelevant: every inhabitant is just the leaf `t`, and transport is the identity `{_e, d} => d`. To reach [`GOALS.md`](GOALS.md)'s target ("dependently typed or strictly more powerful, i.e. HoTT"), equality proofs must carry computational content (distinct paths between the same endpoints, groupoid laws, function extensionality, and univalence / transport along equivalences).

**Design:**
- Allow `eq_t` (or a sibling `path_t(A, a, b)`) to carry real witness terms instead of forcing `UnitT` (`t`).
- Upgrade `with_eq` / `transport` so rewriting along a path witness records or applies its computational action (equivalence forward/backward maps) rather than silently aliasing terms.
- Add higher inductive type (HIT) descriptions where a data former carries both point constructors and path constructors (generalizing `quot_t` from set-level quotients to higher quotients).

---

## 3. Universes (`Type_i`) and Goal-4 self-contained types

**Why it matters:** Two separate gaps sit here:
1. **First-class universe checking:** Today types are 100% token-free values (`has_tok(Nat) == false`), so functions can take and return types freely (`{id} => id(Nat, 3)`), and `surface.disp` uses a shallow `Type` fork check. However, there is no stratified universe hierarchy `Type_0 : Type_1 : ...` verifying *"is this value a well-formed `(check, assume)` type?"* without Girard-style paradoxes.
2. **Goal 4 of [`TYPE_SYSTEM_GOALS.md`](TYPE_SYSTEM_GOALS.md):** Right now a type is `pair(check, assume)` driven externally by `accepts(T, v)` or `refute_at(T, v, n)`, rather than a standalone callable program `T(v)`.

**Design:**
- Define stratified `Type_0`, `Type_1`, ... formers whose `check` validates well-formedness of `(check, assume)` pairs at the lower level and whose `assume` installs `with_abs_type` cleanly (unifying `{A : Type} -> ...` with ordinary `Pi`).
- Explore wrapping each `(check, assume)` pair behind a callable interface (or runtime cut) so `T(v)` answers directly while `T` still exposes its `assume` map to enclosing type formers.

---

## 4. Stream-type integration, mechanized soundness, and retiring `lib/theories/checked.disp`

**Why it matters:** Right now soundness is validated by the kernel/PTS/stdlib test suites plus `sound_refuter` sweeps, not mechanically proved — and `refute_at` collapses "exhausted every input" and "ran out of fuel" into the same `nothing` answer. Meanwhile, the older lens/`judge` theory ladder in `lib/theories/` (including [`lib/theories/checked.disp`](lib/theories/checked.disp) and `test/checked.test.ts`) stays active in CI until its substructural/cost tests migrate to `lib/kernel/`.

**Design:**
- **Connect `refute_kit.quantify` to [`lib/stream.disp`](lib/stream.disp) and [`lib/claim.disp`](lib/claim.disp):** emit an `Items(Tree)` counterexample stream carrying explicit `exhausted` (`"Proved"` on finite domains like `Bit -> Bit`) vs. `spent` (`"Open"`) vs. yielded counterexample (`"Refuted"`).
- **Prove base dispatch rules exhaustively inside disp:** encode K/S/triage/`tree_eq` substitution agreement over finite rule shapes using `over` in [`lib/claim.disp`](lib/claim.disp) so their verdicts are real `"Proved"` by exhaustion.
- **Formalize the step-indexed simulation:** prove each core `Eval -> Eval` layer (`with_subst`, `with_fact`, `with_eq`, `with_data`, `with_abstract`, `with_abs_type`, `with_elim`, `with_rec`, `with_quot_sym`, `bounded_pi`) preserves the branching, multi-step covering relation against concrete `m_advance` runs by outer induction on step budget `k` (initially on paper / external proof assistant, and ultimately bootstrapped inside disp).
- **Retire `lib/theories/checked.disp`:** once Milestone 1 covers the remaining substructural/cost suites on `lib/kernel/` and the website/interactive docs point to the promoted kernel, archive `lib/theories/checked.disp` cleanly.
