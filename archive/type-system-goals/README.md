# Type system goals: archived prototypes

Superseded prototypes built against [`TYPE_SYSTEM_GOALS.md`](../../TYPE_SYSTEM_GOALS.md), oldest first.
The sixth attempt (`eval_types.disp`) was promoted to the core kernel at [`lib/kernel/`](../../lib/kernel/)
(specified in [`TYPE_SYSTEM.typ`](../../TYPE_SYSTEM.typ)), so its pre-promotion draft copies were removed
(recoverable in git history) and the five earlier attempts are kept here for reference.

None of these files is in `lib/`, so `npm test` does not run them. Run one directly with
`npx tsx src/run.ts archive/type-system-goals/<file>` (memory-capped).

1. `type_system.disp` + `type_system.test.disp` (41 tests). A type is a pair of programs, a refuter and a checker, built only by formers. Superseded by 2.
2. `type_kit.disp` + `type_kit.test.disp` (99 tests). A type is `pair(former, parts)`; a former fills slots (`refute`, `members`, `intro`, `elim`, `wf`); the evaluator runs on typed unknowns under a list of rules. Sound across three review rounds, but types are records rather than standalone programs.
3. `supply.test.disp` (14 tests). A type is two plain programs: a test that can say no, and a supply that hands over members. A function type swaps them for its input. Meaning only, no checker; lax on callbacks.
4. `wrapper.test.disp` (11 tests). A type is one wrapper taking two failure handlers (the value's fault, its user's fault); the function type swaps them. Meaning only, no checker.
5. `sealed.disp` + `sealed.test.disp` + `pts_sealed.test.disp`. Sealed polarized wrappers with a single bare-unknown evaluator. Superseded by the evaluator-map architecture promoted to [`lib/kernel/kernel.disp`](../../lib/kernel/kernel.disp).

Attempts 3 and 4 use `type_kit.disp` only for its search helpers (`tried`, `within`, `first_found`).
