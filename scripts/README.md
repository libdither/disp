# scripts/

One line per script. Every disp run here is memory-capped and timed; see `CLAUDE.md` for why.

- `probe.sh` — typecheck a disp snippet the way the kernel would: wraps it in a `use`d module, runs it under a memory-capped systemd scope, and classifies the outcome (ACCEPTED / REJECTED / ERROR / TIMEOUT / OOM-KILLED); `-r` for raw reduction probes, `-p` to print bindings instead of testing.
- `experiment.sh` — try a kernel edit without landing it: patches a definition in place, runs the kernel suites capped and timed, reports `cold_equiv`, and restores the file on exit; `-c` forces a cold run.
- `annotation_census.py` — bucket every top-level definition in `.disp` files by annotation tier (none, membership, sampled, guard-tier weak/precise, dependent), to track annotation waves.
- `scratch_lint.py` — flag `open use` paths in root-level scratch `.disp` files that do not resolve, so a stale probe fails at grep speed.
- `rss_run.py` — run a command and report its peak RSS and wall time on stderr; the portable stand-in for GNU time that the two shell scripts use.

## Elsewhere

- `npm test` — the vitest harness over the `.disp` suites (`test:watch` for the watcher); `npm run disp <file>` runs one file through `src/run.ts`.
- `npm run bench`, `bench:eval`, `bench:evaluators`, `bench:ic-net` — evaluator step and timing benchmarks under `bench/`.
- `research/interaction-combinator/check.sh` — the one gate for cascade-engine work: crate tests, model-cost census against its baseline, then bundle regen and validation.
- `research/interaction-combinator/regen.sh` — regenerate and validate the lattice bundle that `lattice_player.html` replays, after any engine change.
