# scripts/

One line per script. Every disp run here is memory-capped and timed; see `CLAUDE.md` for why.

- `probe.sh` — typecheck a disp snippet the way the kernel would: wraps it in a `use`d module, runs it under a memory-capped systemd scope, and classifies the outcome (ACCEPTED / REJECTED / ERROR / TIMEOUT / OOM-KILLED); `-r` for raw reduction probes, `-p` to print bindings instead of testing.
- `experiment.sh` — try a kernel edit without landing it: patches a definition in place, runs the kernel suites capped and timed, reports `cold_equiv`, and restores the file on exit; `-c` forces a cold run.
- `doc-links.py` — paths named in tracked docs and in source comments that no longer exist (each resolved from the repo root, the file's directories, or as any tracked file's tail; lines naming a commit are history and skipped), and PDFs older than their `.typ`. Many hits are other projects' paths in research notes; read it as a list, not a gate. `python scripts/doc-links.py evaluators/rust-ic-strands` checks one subtree.
- `scratch_lint.py` — flag `open use` paths in root-level scratch `.disp` files that do not resolve, so a stale probe fails at grep speed.
- `rss_run.py` — run a command and report its peak RSS and wall time on stderr; the portable stand-in for GNU time that the two shell scripts use.
- `survey-probe.sh` — first-pass evidence for an awesome-langs candidate: GitHub API metadata (API-verified dates for the write-up header), contributors, recent commits, a clone kept under /tmp with full commit history, exact commits per year, file counts, and a proof-debt word scan (`sorry`/`admit`/`axiom` in proof files) that says "not scanned" when the repo has no such files; scoring stays manual.
- `verify-source.sh` — fetch-verify a research citation (URL, DOI, or arXiv id): browser-agent fetch with cookie hops and a TLS-insecure retry, Semantic Scholar / Crossref metadata with an open-access fallback when the publisher blocks, a Wayback Machine fallback for dead links, PDF/HTML/README text extraction (OCR for scanned pages), and with `-q '<quote>'` the `#page=N` or `#anchor` fragment link for the quoted passage (ligatures, curly quotes, dashes, and line-break hyphens are folded on both sides); `-g '<regex>'` lists matching passages from the cached text without refetching; `-Q FILE` checks every `quote<TAB>source` line at once (one ok/MISS line each); pages are decoded by the charset they declare, and a fragment prefers a stable id (`Abs1`) over a generated one; downloads, text, and OCR cache under `~/.cache/verify-source/`.
- `awesome-scores.py` — recompute `research/awesome-langs/_SCORES.md` (clause values → percentages as the weighted mean of each axis's `Weights:` line, `--write` to rewrite the column) and report each axis ranked, every derived symbol that differs from the master table, the "ahead of disp" changes, and the cells still marked open.

## Elsewhere

- `npm test` — the vitest harness over the `.disp` suites (`test:watch` for the watcher); `npm run disp <file>` runs one file through `src/run.ts`.
- `npm run bench`, `bench:eval`, `bench:evaluators`, `bench:ic-net` — evaluator step and timing benchmarks under `bench/`.
