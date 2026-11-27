# Type system goals: four attempts

Prototypes written against [`TYPE_SYSTEM_GOALS.md`](../../TYPE_SYSTEM_GOALS.md), oldest first. None is in `lib/`, so
`npm test` does not run them. Run one with `npx tsx src/run.ts research/type-system-goals/<file>` (memory-capped).

1. `type_system.disp` + test (41 tests, ~18 s). A type is a pair of programs, a refuter and a checker, built only by
   formers. Superseded by 2.
2. `type_kit.disp` + test (99 tests, ~9 s). A type is `pair(former, parts)`; a former fills slots (`refute`,
   `members`, `intro`, `elim`, `wf`); the evaluator runs on typed unknowns under a list of rules. Sound in three
   review rounds, but it rebuilds ordinary type theory: types are records, not programs.
3. `supply.test.disp` (14 tests). A type is two plain programs: a test that can say no, and a supply that hands over
   members. A function type swaps them for its input. Meaning only, no checker. Lax: a bad argument to a callback hangs.
4. `wrapper.test.disp` (11 tests, ~5 s). A type is one wrapper taking two failure handlers (the value's fault, its
   user's fault); the function type swaps them. The meaning is that the wrapped program never reaches the fault point.
   Meaning only: the checker (the same wrappers run on bare unknowns) is not written.

3 and 4 use `type_kit.disp` only for its search helpers (`tried`, `within`, `first_found`).
