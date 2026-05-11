# Reorganization proposal

The codebase has reached the point where `lib/kernel.disp` is 1034 lines and ~14 `.test.disp` files share the directory with ~9 source `.disp` files. Adding a standard library on top of this would compound the mess. Sketch below.

## Current state

```
lib/
├── prelude.disp           143 lines — primitives
├── kernel.disp           1034 lines — kernel handlers + all public types
├── dae.disp               107 lines — legacy DAE encoding (still passes its tests)
├── ord.disp               143 lines — Ord (predicate_frame, post-migration)
├── nat.disp                51 lines — pred / is_zero / double / proofs
├── math.disp               18 lines — add
├── list.disp               66 lines
├── fin.disp                33 lines
├── set.disp                34 lines
└── *.test.disp           ~14 files, intermixed with sources
```

`kernel.disp` internally has six clearly distinct sections that could be separate files:

| Section | Lines | Content |
|---|---|---|
| utils | 1–110 | type_meta, has_sig, pair helpers, CheckedResult, Action, neutral metadata helpers |
| handler-helpers | 110–200 | q_is_neutral / q_make_hyp / q_unguard_or_self / q_scan_no_neutral / q_h_rule_fn / q_guard_fn |
| primitives | 200–460 | q_hyp_reduce_fn / q_predicate_frame_fn / q_eliminator_frame_fn / q_bind_hyp_fn / q_unguard_fn / q_checked_apply_fn / q_eq_fn / q_eq_J_fn / q_contains_via_open_path |
| record | 460–520 | kernel record assembly, kernel_ref, Hyp/StuckElim, guard/unguard_checked, predicate_frame_form / eliminator_frame_form |
| types | 520–820 | Bool / Nat / Eq / Pi / Type / Arrow predicate_frame definitions, bool_rec / nat_rec / eq_J / bind_hyp wait-forms, is_pi / pi_dom / pi_cod_fn / is_universe |
| walker + anchors | 820–1034 | checked_apply_walker, native dispatcher signature anchors |

## Proposed structure

```
lib/
├── prelude.disp                          # Stays — primitives only
│
├── kernel/                               # Trusted host-mediated code
│   ├── utils.disp                        # type_meta, has_sig, CheckedResult, Action
│   ├── helpers.disp                      # q_is_neutral, q_make_hyp, q_h_rule_fn, etc.
│   ├── primitives.disp                   # All q_*_fn handlers (the 7 primitives)
│   ├── record.disp                       # recq kernel + kernel_ref + Hyp/StuckElim/guard
│   ├── walker.disp                       # checked_apply_walker (standalone)
│   └── anchors.disp                      # Native dispatcher signature exports
│
├── types/                                # Library type definitions
│   ├── bool.disp                         # Bool (predicate_frame)
│   ├── nat.disp                          # Nat predicate (NOT pred/is_zero — that's std/)
│   ├── pi.disp                           # Pi + Arrow + reflection (is_pi, pi_dom, ...)
│   ├── type.disp                         # Type (universe) + is_universe
│   ├── eq.disp                           # Eq (legacy until #2 resolved)
│   ├── ord.disp                          # Ord (predicate_frame)
│   └── eliminators.disp                  # bool_rec / nat_rec / eq_J wait-forms +
│                                         #   the bind_hyp wait-form
│
├── std/                                  # Standard library (uses types/)
│   ├── nat/
│   │   ├── arith.disp                    # add (current math.disp)
│   │   ├── ops.disp                      # pred, is_zero, double (current nat.disp)
│   │   └── proofs.disp                   # equality lemmas
│   ├── bool/
│   │   └── ops.disp                      # bool_not, bool_and, bool_or
│   ├── list.disp
│   ├── set.disp
│   ├── fin.disp
│   ├── option.disp                       # NEW: Maybe/Option
│   ├── either.disp                       # NEW
│   └── vec.disp                          # NEW: length-indexed lists
│
├── legacy/                               # Pre-migration code, retained
│   └── dae.disp                          # DAE-encoded types (Bool_template etc.)
│
└── tests/                                # All .test.disp files moved here
    ├── kernel/
    │   ├── walker.test.disp
    │   ├── soundness.test.disp
    │   ├── tree_eq.test.disp
    │   ├── predicate_frame.test.disp     # tests for the primitive
    │   ├── eliminator_frame.test.disp
    │   ├── bind_hyp.test.disp
    │   ├── rec.test.disp
    │   └── match.test.disp
    ├── types/
    │   ├── bool.test.disp
    │   ├── pi.test.disp
    │   ├── type.test.disp
    │   ├── ord.test.disp
    │   └── eq.test.disp                  # carved out of kernel.test.disp
    ├── std/
    │   ├── nat.test.disp
    │   ├── list.test.disp
    │   ├── set.test.disp
    │   └── fin.test.disp
    ├── legacy/
    │   └── dae.test.disp
    ├── elab.test.disp                    # cross-cutting elaborator tests
    └── prelude.test.disp
```

## Migration tactics

### Phase 1 — tests folder (low risk)

Just move every `lib/*.test.disp` into `lib/tests/`. Update `test/disp.test.ts` to glob `lib/tests/**/*.test.disp`. Update each test file's `open use "foo.disp"` to `open use "../foo.disp"` (path resolution is relative to the file). About one regex replacement per test file. Half an hour of work; ~125 tests should keep passing.

### Phase 2 — kernel split (medium risk)

Break `lib/kernel.disp` into the kernel/*.disp files listed above. The wrinkle is that the recq record's `kernel : {hyp_reduce, ..., bind_hyp} := recq { ... }` is one binding; it has to live in one file (or be assembled from a single record literal). Likely structure:

- `kernel/utils.disp` — utilities, exported as `:=` for reuse.
- `kernel/helpers.disp` — `open use "utils.disp"`, defines q_is_neutral etc.
- `kernel/primitives.disp` — `open use "helpers.disp"`, defines all q_*_fn handlers.
- `kernel/record.disp` — `open use "primitives.disp"`, assembles `kernel := recq {...}` and the kernel_ref proxy, exports `Hyp` / `StuckElim` / `guard` / `predicate_frame_form` / etc.

`open use` chaining works in disp (each file imports the previous). The challenge is the recq record's type annotation `{hyp_reduce,guard,eq,eq_J,unguard,checked_apply,predicate_frame,eliminator_frame,bind_hyp}` — duplicated wherever a handler body references `ks`. With the kernel split across files, every primitive-defining file needs that annotation. Two options:

- Keep the annotation everywhere as it is today (~15 sites).
- Add a `ksType` alias in `utils.disp` if disp supports record-type aliases. Worth checking the parser.

### Phase 3 — types split (low risk)

Once the kernel is broken up, peel off `Bool`/`Nat`/`Pi`/`Type`/`Ord` definitions into `types/*.disp`. Each becomes a small file (~30–50 lines) that imports `kernel/record.disp` (for `predicate_frame_form` etc.) and defines the type + its eliminator + its reflection helpers.

### Phase 4 — std layout (incremental)

Already-organized: `nat.disp` → `std/nat/ops.disp`, `math.disp` → `std/nat/arith.disp`, etc. Then start adding new standard library content (`option.disp`, `either.disp`, `vec.disp`, ...) as you go. Each can be tested in isolation via `tests/std/`.

## Things to think about

**Single-namespace `open use`.** Today many files do `open use "kernel.disp"` and pull every name into scope. With a finer-grained kernel, you'd either:
- Maintain a top-level `kernel.disp` that re-opens the sub-files (back-compat for existing callers).
- Migrate callers to import only what they need (`open use "kernel/record.disp"` for `Hyp`/`guard`; `open use "types/pi.disp"` for `Pi`).

The first is non-disruptive; the second is cleaner long-term.

**`trust open use`** semantics. Currently `trust open use "kernel.disp"` enables the elaborator's typed-binding path by populating the `trusted` table with `Pi`, `Type`, `Hyp`. If those moves to `types/pi.disp` / `types/type.disp` / `kernel/record.disp`, callers would need to `trust open use` all three. A compatibility shim file (the unified `kernel.disp` mentioned above) sidesteps this.

**Where does `predicate_frame_form` live?** It's a *user-facing* construction helper, used by `lib/ord.disp` to define a library type. Probably `kernel/record.disp` (with the other public construction helpers).

**Legacy/dae.disp**. Currently 107 lines of pre-migration code that still passes its tests. Two options: keep in `lib/legacy/` as a reference artifact, or delete now that nothing else uses it. I'd lean toward keep — it's a useful comparison point and the tests document the old design.

**No changes to `src/`.** The TS host code (parser, elaborator, tree runtime) doesn't need reorganization; it's already well-factored at ~750 lines per file.

## Rough order of operations

1. ✅ Done: migrations + dead-code cleanup. Tests at 125 passing, kernel at ~960 LOC (was 1090+).
2. Phase 1: tests folder. Half an hour.
3. Phase 2: kernel split. Probably a day. Will surface any disp-language issues with multi-file imports.
4. Phase 3: types split. Few hours after phase 2.
5. Phase 4: opportunistic std/ growth as new types get added.

Phase 1 has minimal downside and immediately makes the directory legible. Phase 2 is the most invasive and worth doing only if the kernel keeps growing (Eq migration, future primitives). Phase 3 is small and can be done with phase 2.
