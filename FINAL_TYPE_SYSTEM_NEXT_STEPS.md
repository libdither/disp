# Final Type System: Next Steps

Status: The evaluator-map type system ([eval_types.disp](research/type-system-goals/eval_types.disp)) recreates the full Lambda Cube (STLC through Calculus of Constructions), System T (including Ackermann), Martin-Löf Type Theory (induction, equality, data descriptions), refinement types, quotients, verified custom eliminators, and oracle-step bounded functions on a single 7-field `Kit` with zero custom runtime opcodes (210/210 tests passing across [eval_types.test.disp](research/type-system-goals/eval_types.test.disp) and [pts_eval.test.disp](research/type-system-goals/pts_eval.test.disp)).

This document tracks the remaining big pieces needed to turn it from a working prototype into Disp's core kernel and a universal type-theory generator, in priority order. See [TYPE_SYSTEM.typ](TYPE_SYSTEM.typ) for the full specification of the design.

---

## 1. Structural & lexicographic termination (totality)

**Why it matters:** Right now `pi_t` is *partial* ("if the call finishes, the result has type `B`"). That lets an infinite loop `spin := fix({r, n} => r(n))` pass `pi_t(Nat, {_n} => EmptyT)`. `bounded_pi` closes the loophole for a fixed step budget (like 50), but cannot prove *"terminates for every `n`, regardless of how large `n` is"* — which mathematical proofs in MLTT/CoC require.

**Design:**
- Track the active quantified arguments `(orig_1, orig_2, ...)` in `Eval` when `with_rec` arms a `fix` function.
- On a recursive call `f(x_1, x_2, ...)`, compare `deep_resolve(self, x_i)` against `deep_resolve(self, orig_i)` lexicographically using **strict structural subtree** (`strict_subterm`), descending *only* through concrete tree constructors (`stem` and non-token `fork`) and never inside internal `tok_call`/`tok_unk` handles (so `eq_t(n, dbl_n(n))` cannot spoof a decrease).
- Because `with_data`, `with_list_sym`, and `with_sigma_sym` already record every constructor split via `with_subst`, `deep_resolve(self, orig_i)` automatically exposes all strict subterms (`nat_pred(n)`, `xs.tail`, binary tree children `l`/`r`, etc.) for any data type.
- Keep `pi_t` / `fn_t` total by default, and expose `partial_pi` / `partial_fn` explicitly for general recursion where divergence is intended.

**Immediate next step after this lands:** Promote `eval_types.disp` to the core kernel, hook into `:` annotations via `Pi` / `check_module`, and begin re-typechecking `lib/` (see §6 below).

---

## 2. Extensible `meta` state for resources, linearity, and effects

**Why it matters:** Today `Eval` layers describe *what values are* (`step`, `lazy`, `resolve`, `eq`, `has_tag`), while changing per-run state lives in a fixed 4-tuple `meta = (depth, sure, cap, unbox)`. Substructural type theories (linear, affine, graded/usage-counted, Rust-style ownership/borrowing) and algebraic effect / coeffect systems need *mutable-along-a-run* state (usage ledgers, borrow regions, effect logs) that updates on every step without discarding stacked assumption layers.

**Design:**
- Generalize `meta` from a fixed tuple into an extensible keyed state map (or slot list) threaded through `run_ev`, where each layer can read/update its own state key on step transitions and branch merges.
- Validate by recreating `lib/theories/affine.test.disp`, `lib/theories/graded.test.disp`, and `lib/theories/cost.test.disp` as plain `(check, assume)` formers with zero changes to the core `run_ev` loop.

---

## 3. Higher-dimensional equality (HoTT / computational paths)

**Why it matters:** Today `eq_t(a, b)` is proof-irrelevant: every inhabitant is just the leaf `t`, and transport is the identity `{_e, d} => d`. To reach [GOALS.md](GOALS.md)'s target ("dependently typed or strictly more powerful, i.e. HoTT"), equality proofs must carry computational content (distinct paths between the same endpoints, groupoid laws, function extensionality, and univalence / transport along equivalences).

**Design:**
- Allow `eq_t` (or a sibling `path_t(A, a, b)`) to carry real witness terms instead of forcing `UnitT` (`t`).
- Upgrade `with_eq` / `transport` so rewriting along a path witness records or applies its computational action (equivalence forward/backward maps) rather than silently aliasing terms.
- Add higher inductive type (HIT) descriptions where a data former carries both point constructors and path constructors (generalizing `quot_t` from set-level quotients to higher quotients).

---

## 4. Universes (`Type_i`) and Goal-4 self-contained types

**Why it matters:** Two separate gaps sit here:
1. **First-class universe checking:** Today types are 100% token-free values (`has_tok(Nat) == false`), so functions can take and return types freely (`{id} => id(Nat, 3)`). However, there is no `TypeT` former that verifies *"is this value a well-formed `(check, assume)` type?"* or enforces a stratified universe hierarchy `Type_0 : Type_1 : ...` (needed to avoid Girard-style paradoxes once `Type` is itself a checkable type).
2. **Goal 4 of [TYPE_SYSTEM_GOALS.md](TYPE_SYSTEM_GOALS.md):** Right now a type is `pair(check, assume)` driven externally by `accepts(T, v)` or `refute_at(T, v, n)`, rather than a standalone callable program `T(v)`.

**Design:**
- Define stratified `Type_0`, `Type_1`, ... formers whose `check` validates well-formedness of `(check, assume)` pairs at the lower level and whose `assume` installs `with_abs_type` cleanly (unifying `{A : Type} -> ...` with ordinary `Pi`).
- Explore wrapping each `(check, assume)` pair behind a callable interface (or runtime cut) so `T(v)` answers directly while `T` still exposes its `assume` map to enclosing type formers.

---

## 5. Stream-type integration & mechanized soundness proof

**Why it matters:** Right now soundness is validated by 210 unit/regression tests plus `sound_refuter` sweeps, not mechanically proved — and `refute_at` collapses "exhausted every input" and "ran out of fuel" into the same `nothing` answer.

**Design:**
- **Connect `refute_kit.quantify` to [lib/stream.disp](lib/stream.disp) and [lib/claim.disp](lib/claim.disp):** emit an `Items(Tree)` counterexample stream carrying explicit `exhausted` (`"Proved"` on finite domains like `Bit -> Bit`) vs. `spent` (`"Open"`) vs. yielded counterexample (`"Refuted"`).
- **Prove base dispatch rules exhaustively inside disp:** encode K/S/triage/`tree_eq` substitution agreement over finite rule shapes using `over` in [lib/claim.disp](lib/claim.disp) so their verdicts are real `"Proved"` by exhaustion.
- **Formalize the step-indexed simulation:** prove each of the ~10 core `Eval -> Eval` layers (`with_subst`, `with_fact`, `with_eq`, `with_data`, `with_abstract`, `with_abs_type`, `with_elim`, `with_rec`, `with_quot_sym`, `bounded_pi`) preserves the branching, multi-step covering relation against concrete `m_advance` runs by outer induction on step budget `k` (initially on paper / external proof assistant, and ultimately bootstrapped inside disp once §1 totality is solid).

---

## 6. Kernel promotion, `:` syntax hook, and stdlib re-typechecking

Once §1 (structural termination) passes all existing suites plus termination tests:
1. **Promote to core kernel:** create a clean kernel entry module (e.g. `lib/kernel/eval_kernel.disp` or replacing `lib/kernel/` once stdlib + `vitest` are green) exporting the surface vocabulary expected by [src/elab/driver.ts](src/elab/driver.ts) and [src/elab/vocab.ts](src/elab/vocab.ts):
   - Type formers & aliases: `Pi`, `Fn`, `PartialPi`, `Sigma`, `Pair`, `Eq`, `The`, `Refine`, `Quotient`, `Bounded`, `Opaque`, `Forall`, `ForallFam`, `Nat`, `Bool`, `Bit`, `Tree`, `Unit`, `Empty`, `Type`, `List`.
   - Smart `Pi(A, fam)` desugar: when `A == Type`, route to `forall_t(fam)` (and `TypeFam` to `forall_fam_t(fam)`) so `{A : Type} -> A -> A` works natively with `:` syntax!
   - Module check hook: `make_record`, `list_const`, and `check_module` (checking annotated exports in file order, promoting each verified function into `lemmas` for subsequent definitions, returning `true` on full pass or `[(name, reason), ...]` on failure so [src/elab/driver.ts](src/elab/driver.ts)'s `worldReport` prints exact failure reasons).
2. **Re-typecheck the standard library incrementally:**
   - Start with `lib/prelude.disp` (typed combinators, booleans, arithmetic) and `lib/list.disp` (polymorphic `reduce`, `map`, `append`, `concat`, `reverse`, `length`, `filter`, `all`, `any`, `member`, `dedup`, `cross`, `zip`), then `lib/trees.disp`, `lib/verdict.disp`, `lib/machine.disp`, `lib/stream.disp`, and `lib/claim.disp`.
   - Profile every checked module with `--stats` and track `cold_equiv` so checking speed stays fast enough for interactive editing.
3. **Retire legacy prototypes & redundant docs:** once the new kernel passes the stdlib and theory suites, supersede `research/type-system-goals/sealed.disp` and older `lib/theories/checked.disp` / `lib/kernel/` pieces cleanly.
