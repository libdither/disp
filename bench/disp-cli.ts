// disp batch CLI for the benchmark harness (bench/bench-evaluators.ts). Folds
// ternary terms via a disp Session backend and self-reports startup-free reduction
// time on stderr, EXACTLY mirroring lambada's cli.cjs — so every contestant is
// measured the same way: a cold subprocess, JIT cold, `reduce_ms` on stderr,
// normal form on stdout. (Run via tsx; tsx startup is excluded from reduce_ms.)
//
//   tsx bench/disp-cli.ts -e <eager|naive> -ternary t0 t1 ...

import { eagerBackend } from "../src/eval/eager.js"
import { naiveBackend } from "../src/eval/naive.js"
import { sessionBatchRunner } from "../src/eval/batch.js"

const argv = process.argv.slice(2)
let backendName = "eager"
let budget = 0
const terms: string[] = []
for (let i = 0; i < argv.length; i++) {
  const a = argv[i]
  if (a === "-e") backendName = argv[++i]
  else if (a === "-budget") budget = parseInt(argv[++i], 10)
  else if (a === "-ternary") continue
  else terms.push(a)
}
if (terms.length === 0) { console.error("no ternary terms given"); process.exit(2) }

const backend = backendName === "naive" ? naiveBackend : eagerBackend
// Defaults match the harness; -budget overrides. Loose enough to admit the totals
// at benchmark sizes; exhaustion (DNF) is the divergence safety net.
if (!budget) budget = backendName === "naive" ? 400_000_000 : 5_000_000_000

const s = backend.createSession()
const t0 = performance.now()
const nf = sessionBatchRunner(backend.name, s).fold(terms, { remaining: budget })
const ms = performance.now() - t0
s.dispose()
process.stdout.write(nf + "\n")
process.stderr.write(`reduce_ms=${ms.toFixed(3)} evaluator=disp-${backendName}\n`)
