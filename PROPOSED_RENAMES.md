# Proposed renames

Names that were used in `INTERACTIVE_WALKTHROUGH.html` for pedagogical clarity, listed against the current codebase spelling. Each row is a candidate to apply to the codebase as a whole — keep, discard, or rename further during review.

The doc-side rename is already in place; the question is whether to propagate to `src/` and `lib/`.

## Kernel handler fields and helpers

| Codebase | Doc | Rationale |
|---|---|---|
| `recq` | `lazy_self_record` | `recq` is opaque on first sight; the long name is one word longer but tells you immediately why it exists (self-referential, lazy). |
| `q_hyp_reduce_fn` | `hyp_reduce_fn` | The `q_` prefix and `_fn` suffix on every handler form a wall of noise. Either prefix or suffix would do; both is redundant. |
| `q_guard_fn` | `guard_fn` | (same) |
| `q_unguard_fn` | `unguard_fn` | (same) |
| `q_checked_apply_fn` | `checked_apply_fn` | (same) |
| `q_predicate_frame_fn` | `predicate_frame_fn` | (same) |
| `q_eliminator_frame_fn` | `eliminator_frame_fn` | (same) |
| `q_bind_hyp_fn` | `bind_hyp_fn` | (same) |
| `q_h_rule_fn` | `h_rule_fn` | (same) |
| `q_is_neutral` | `is_neutral` | The `q_` prefix on predicates does not communicate anything to a reader. |
| `q_scan_no_neutral` | `scan_no_neutral` | (same) |
| `q_unguard_or_self` | `unguard_or_self` | (same) |
| `must_ok_or_ff` | `expect_ok` | `must_ok_or_ff` reads as four ideas; `expect_ok` is one. The "or_ff" return mode could live in a docstring or be inferred from context. |

## Pair / tuple helpers

| Codebase | Doc | Rationale |
|---|---|---|
| `pair_fst` | `fst` | Standard short names. `pair_` prefix is implicit from the call site. |
| `pair_snd` | `snd` | (same) |

## Predicate-frame type definitions

The codebase tags every recognizer with `_pf_` (predicate-frame). The doc drops this because (a) every recognizer in `lib/types/` is predicate-frame, (b) readers do not need to disambiguate against a hypothetical non-pf variant.

| Codebase | Doc | Rationale |
|---|---|---|
| `bool_pf_recognizer` | `bool_recognizer` | |
| `nat_pf_recognizer` | `nat_recognizer` | |
| `pi_pf_recognizer` | `pi_recognizer` | |
| `eq_pf_recognizer` | `eq_recognizer` | |
| `type_pf_recognizer` | `type_recognizer` | |
| `ord_pf_recognizer` (not in doc) | `ord_recognizer` | (if applying consistently) |
| `pi_pf_codomain_fn` | `pi_codomain_fn` | |
| `Pi_pf` (the unwrapped predicate-frame) | `Pi` | The codebase distinguishes `Pi_pf` from `Pi`; the doc treats `Pi` as the user-facing name. If the unwrapped form needs a name in the codebase, consider `Pi_inner` or `Pi_unguarded`. |
| `type_pf_metadata_well_formed` | `type_metadata_well_formed` | |

## Metadata accessors

| Codebase | Doc | Rationale |
|---|---|---|
| `neutral_meta_type` | `stored_type` | Reads naturally at call sites — `stored_type(hyp)` instead of `neutral_meta_type(hyp)`. |
| `eq_meta_lhs` | `lhs` (or destructure) | If a tuple of `(A, x, y)`, prefer destructuring; if accessor needed, `lhs(eq_meta)`. |
| `eq_meta_rhs` | `rhs` (or destructure) | (same) |
| `make_eq_meta` | `eq_meta` | The `make_` prefix is implicit at constructor sites. |

## Open questions for review

- **`pf_` discriminator.** If `pi_pf_recognizer` vs `pi_recognizer` matters because there are multiple non-pf recognizers planned, keep the suffix. Otherwise, drop everywhere.
- **`q_` prefix.** If `q_` once meant something (query? quoted?), document it; if not, retire.
- **`Pi_pf` vs `Pi`.** Decide whether the wrapped/unwrapped distinction surfaces in user-facing code, or only inside kernel handlers.
- **Two-arg vs three-arg `select`.** The doc moves the unused motive out of the basic Scott encoding; if the codebase always passes the motive, the conventions stay separate. Worth deciding whether to drop the motive parameter from `select` at the source level.

## Names *not* changed (intentional)

- `predicate_frame_form` — reads as English ("the predicate-frame form of ..."), so the suffix earns its place.
- `eliminator_frame_form` — same.
- `tree_eq`, `is_leaf`, `is_fork` — already short.
- `guard`, `unguard`, `bind_hyp`, `checked_apply`, `hyp_reduce`, `predicate_frame`, `eliminator_frame` — these are the seven primitives; the names are load-bearing.
- `kernel_ref` — the proxy is named after its role.
