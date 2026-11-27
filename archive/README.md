# Archive

Retired kernels, superseded type-system prototypes, design proposals, and UI studies kept for reference.

## Directories

- [`live-kernel/`](live-kernel/) — The earlier two-op trusted-core kernel (`kernel/`, `std/`, `tests/`, `TYPE_THEORY.typ`, `KERNEL_DESIGN.md`, `NEGATIVE_TYPES.md`, `BUGS.md`). Kept loadable because host driver tests (`test/guards.test.ts`, `test/modules.test.ts`), website playground examples, and CI still exercise its protocol.
- [`provenance-kernel/`](provenance-kernel/) — The provenance-ledger kernel that preceded the evaluator-map kernel (`kernel.disp`, `types.disp`, `decision.disp`, `prelude.disp`, and its test pins). Superseded by [`lib/kernel/`](../lib/kernel/).
- [`type-system-goals/`](type-system-goals/) — Five iterative prototypes built against [`TYPE_SYSTEM_GOALS.md`](../TYPE_SYSTEM_GOALS.md) (`type_system.disp`, `type_kit.disp`, `supply.test.disp`, `wrapper.test.disp`, `sealed.disp`) leading up to the promoted evaluator-map kernel in [`lib/kernel/`](../lib/kernel/).
- [`playground-dial/`](playground-dial/) — Detail-dial design study for the tree viewer.
- [`why-page/`](why-page/) — Unpublished website Why page prototype.
- [`one-offs/`](one-offs/) — Standalone background surveys and reading notes.

## Superseded plans, proposals, and ledgers

- [`ACTIVE_BUGS.md`](ACTIVE_BUGS.md) — Bug ledger for the archived provenance kernel.
- [`ANNOTATION_MIGRATION_PLAN.md`](ANNOTATION_MIGRATION_PLAN.md) — Annotation migration plan for the retired kernels.
- [`EVALUATOR_WALKTHROUGH.html`](EVALUATOR_WALKTHROUGH.html) — Earlier interactive evaluator walkthrough.
- [`JUDGE_FROM_LENSES.md`](JUDGE_FROM_LENSES.md) — Superseded proposal for the lens-based `judge` loop (replaced by goal-driven search in [`research/CASE_TREE_SEARCH.md`](../research/CASE_TREE_SEARCH.md)).
- [`LOCAL_SYNTH.md`](LOCAL_SYNTH.md) — Earlier local synthesis notes.
- [`OEQ_INVESTIGATION.md`](OEQ_INVESTIGATION.md) / [`OEQ_PLAN.md`](OEQ_PLAN.md) — Observational equality investigation and roadmap for the retired kernels.
- [`RECORDS_PROPOSAL.md`](RECORDS_PROPOSAL.md), [`REFLECT.md`](REFLECT.md), [`SCOPE_VERIFICATION_INVESTIGATION.md`](SCOPE_VERIFICATION_INVESTIGATION.md), [`STRICTTYPE.md`](STRICTTYPE.md), [`TOWER.md`](TOWER.md), [`TYPE_NORMALIZATION.md`](TYPE_NORMALIZATION.md), [`UNIFIED_FORMER_PROPOSAL.md`](UNIFIED_FORMER_PROPOSAL.md) — Landed or superseded design proposals from earlier kernel iterations.
- [`STREAM_TYPES_PLAN.md`](STREAM_TYPES_PLAN.md) — Stream types roadmap: Part 1 landed (`lib/stream.disp`, `lib/verdict.disp`), Part 2 was superseded by `lib/claim.disp`, and Part 3's symbolic machine landed in `lib/kernel/kernel.disp` and `lib/confirm.disp`.
