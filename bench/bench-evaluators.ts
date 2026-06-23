// Reproducible cross-evaluator benchmark (EVALUATOR_PLAN.md §6 / Phase 5, scoped
// to evaluator throughput). Runs the five lambada benchmark programs across every
// contestant and reports best-of-N reduction time:
//
//   contestants = disp-eager, disp-naive (via bench/disp-cli.ts)
//               + all 11 lambada-llc/tree-calculus evaluators (via cli.cjs)
//   programs    = size / recursive-fib / silly-exp / exercise-rules / merge-sort
//                 (committed fixture, args generated at configurable sizes)
//
// Methodology (governs fairness — every contestant is measured IDENTICALLY):
//  - Each run is a COLD subprocess (fresh V8, JIT cold) that self-reports
//    `reduce_ms` on stderr — the fold timed inside the process, so interpreter
//    startup / tsx / bundle load are excluded. disp and lambada are therefore
//    apples-to-apples (no warm-JIT or warm-arena advantage for the in-process side).
//  - Children are spawned through `ulimit -s unlimited` (as lambada's own
//    run-one.sh does), so the recursion-based evaluators get a fair stack.
//  - Best (min) of N runs.
//  - Every run's normal form is validated against disp-eager (the conformance-
//    validated oracle, computed in-process); a mismatch is a FAIL, never a silent
//    fast number.
//  - Safety nets: disp is step-budget-bounded (DNF on exhaustion); every child is
//    wall-timeout-bounded (TIMEOUT); host-stack overflow in a recursion-based
//    evaluator is reported as STACK (a real property, not a crash/wrong answer).
//
// Reproducible: pinned evaluators (evaluators/lambada/build.sh), pinned programs
// (fixture), deterministic args, dated log under bench/logs/. Run:
//   npm run bench:evaluators            # default sizes
//   BENCH_FIB=24 BENCH_N=7 npm run bench:evaluators

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { spawnSync } from "node:child_process"
import { cpus } from "node:os"
import { eagerBackend } from "../src/eval/eager.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import { lambadaArtifactPath, lambadaAvailable, LAMBADA_EVALUATORS } from "../src/eval/lambada.js"

const HERE = dirname(fileURLToPath(import.meta.url))
const programs: Record<string, string> =
  JSON.parse(readFileSync(join(HERE, "programs", "lambada-benchmarks.json"), "utf8")).programs

// nat/list → ternary, mirroring lambada benchmark/run.sh (see eval-lambada.test.ts).
const encodeNat = (n: number): string => {
  const bits: number[] = []
  for (let m = n; m > 0; m >>= 1) bits.push(m & 1)
  let r = "0"
  for (let i = bits.length - 1; i >= 0; i--) r = (bits[i] === 1 ? "210" : "20") + r
  return r
}
const makeNatList = (start: number, stop: number, step: number): string => {
  let r = "0"
  for (let k = stop; k !== start - step; k -= step) r = "2" + encodeNat(k) + r
  return r
}

const envInt = (k: string, d: number): number => { const v = process.env[k]; return v ? parseInt(v, 10) : d }
const N = envInt("BENCH_N", 5)
const TIMEOUT_MS = envInt("BENCH_TIMEOUT_MS", 20_000)
const sizes = {
  fib: envInt("BENCH_FIB", 22),
  silly: envInt("BENCH_SILLY", 12),
  exercise: envInt("BENCH_EXERCISE", 20_000),
  merge: envInt("BENCH_MERGE", 60),
}
const EAGER_BUDGET = envInt("BENCH_EAGER_BUDGET", 5_000_000_000)
const NAIVE_BUDGET = envInt("BENCH_NAIVE_BUDGET", 400_000_000)

const benchmarks: Array<{ name: string; terms: string[] }> = [
  { name: "size", terms: [programs["size"], programs["size"]] },
  { name: `fib(${sizes.fib})`, terms: [programs["recursive-fib"], encodeNat(sizes.fib)] },
  { name: `silly-exp(${sizes.silly})`, terms: [programs["silly-exp"], encodeNat(sizes.silly)] },
  { name: `exercise(${sizes.exercise})`, terms: [programs["exercise-rules"], encodeNat(sizes.exercise)] },
  { name: `merge-sort(${sizes.merge})`, terms: [programs["merge-sort"], makeNatList(sizes.merge, 1, -1)] },
]

type Outcome =
  | { ok: true; nf: string; reduceMs: number }
  | { ok: false; status: "DNF" | "TIMEOUT" | "FAIL" | "STACK" | "ERR"; note?: string }

const CLI = lambadaArtifactPath()
const TSX = join(HERE, "..", "node_modules", ".bin", "tsx")
const DISP_CLI = join(HERE, "disp-cli.ts")

// One cold-subprocess fold. `cmd` is the executable + args up to (not including)
// `-ternary`. Spawned through a shell that raises the stack limit (lambada's
// run-one.sh convention) so recursion-based evaluators get a fair stack; `exec
// "$@"` passes terms as exact argv. reduce_ms is read from the child's stderr.
function runOnce(cmd: string[], terms: string[]): Outcome {
  const res = spawnSync("bash", ["-c", 'ulimit -s unlimited 2>/dev/null; exec "$@"', "bash", ...cmd, "-ternary", ...terms], {
    encoding: "utf8", timeout: TIMEOUT_MS, maxBuffer: 256 * 1024 * 1024,
  })
  if (res.error) {
    const timedOut = res.signal === "SIGTERM" || /ETIMEDOUT/.test(res.error.message)
    return { ok: false, status: timedOut ? "TIMEOUT" : "ERR", note: res.error.message.slice(0, 60) }
  }
  if (res.status !== 0) {
    const err = (res.stderr ?? "").trim()
    if (/budget exhausted/i.test(err)) return { ok: false, status: "DNF", note: err.slice(0, 60) }
    // Recursion-based evaluators reduce on the host call stack ⇒ deep workloads
    // overflow it (a real design property, distinct from a wrong answer/crash).
    if (/Maximum call stack|RangeError/.test(err)) return { ok: false, status: "STACK", note: err.slice(0, 60) }
    return { ok: false, status: "ERR", note: err.slice(0, 60) }
  }
  const m = /reduce_ms=([\d.]+)/.exec(res.stderr ?? "")
  return { ok: true, nf: res.stdout.trim(), reduceMs: m ? parseFloat(m[1]) : NaN }
}

// Best-of-N for one contestant on one benchmark, validating every run vs `expected`.
function measure(cmd: string[], terms: string[], expected: string): Outcome {
  let best: number | null = null
  let lastBad: Outcome | null = null
  for (let i = 0; i < N; i++) {
    const o = runOnce(cmd, terms)
    if (!o.ok) { lastBad = o; if (o.status === "TIMEOUT" || o.status === "DNF" || o.status === "STACK") break; continue }
    if (o.nf !== expected) return { ok: false, status: "FAIL", note: `nf≠oracle (${o.nf.length}≠${expected.length} chars)` }
    if (best === null || o.reduceMs < best) best = o.reduceMs
  }
  if (best === null) return lastBad ?? { ok: false, status: "ERR", note: "no runs" }
  return { ok: true, nf: expected, reduceMs: best }
}

// Oracle: disp-eager NF, computed in-process (correctness reference, not timed).
function oracleNF(terms: string[]): string | null {
  const s = eagerBackend.createSession()
  try { return sessionBatchRunner("eager", s).fold(terms, { remaining: EAGER_BUDGET }) }
  catch { return null }
  finally { s.dispose() }
}

const fmtMs = (ms: number): string =>
  ms < 10 ? `${ms.toFixed(2)}ms` : ms < 10_000 ? `${ms.toFixed(1)}ms` : `${(ms / 1000).toFixed(1)}s`

function main(): void {
  if (!lambadaAvailable()) {
    console.error(`lambada artifact missing: ${CLI}\nrun: bash evaluators/lambada/build.sh`)
    process.exit(1)
  }
  if (!existsSync(TSX)) { console.error(`tsx not found at ${TSX} (run npm install)`); process.exit(1) }

  const header = [
    `disp cross-evaluator benchmark`,
    `cpu:    ${cpus()[0]?.model ?? "unknown"} (${cpus().length} cores)`,
    `node:   ${process.version}`,
    `date:   ${new Date().toISOString()}`,
    `runs:   best of ${N}   timeout: ${TIMEOUT_MS}ms   sizes: ${JSON.stringify(sizes)}`,
    `metric: reduction time (cold subprocess, startup-free reduce_ms — disp & lambada measured identically)`,
    ``,
  ]

  const oracle = new Map<string, string>()
  for (const b of benchmarks) {
    const nf = oracleNF(b.terms)
    if (nf !== null) oracle.set(b.name, nf)
    else header.splice(header.length - 1, 0, `! oracle DNF for ${b.name} — column skipped`)
  }

  const contestants: Array<{ label: string; cmd: string[] }> = [
    { label: "disp-eager", cmd: [TSX, DISP_CLI, "-e", "eager", "-budget", String(EAGER_BUDGET)] },
    { label: "disp-naive", cmd: [TSX, DISP_CLI, "-e", "naive", "-budget", String(NAIVE_BUDGET)] },
    ...LAMBADA_EVALUATORS.map((ev) => ({ label: `lambada-${ev}`, cmd: ["node", CLI, "-e", ev] })),
  ]

  const cols = benchmarks.filter((b) => oracle.has(b.name))
  const grid: Record<string, Record<string, Outcome>> = {}
  for (const c of contestants) {
    grid[c.label] = {}
    for (const b of cols) grid[c.label][b.name] = measure(c.cmd, b.terms, oracle.get(b.name)!)
  }

  const labelW = Math.max(...contestants.map((c) => c.label.length), 10)
  const cell = (o: Outcome): string => (o.ok ? fmtMs(o.reduceMs) : o.status)
  const colW = cols.map((b) => Math.max(b.name.length, ...contestants.map((c) => cell(grid[c.label][b.name]).length), 8))
  const pad = (s: string, w: number) => s.padStart(w)
  const lines = [...header]
  lines.push([pad("evaluator", labelW), ...cols.map((b, i) => pad(b.name, colW[i]))].join("  "))
  lines.push([pad("─".repeat(labelW), labelW), ...cols.map((_, i) => pad("─".repeat(colW[i]), colW[i]))].join("  "))
  for (const c of contestants)
    lines.push([pad(c.label, labelW), ...cols.map((b, i) => pad(cell(grid[c.label][b.name]), colW[i]))].join("  "))
  lines.push("")
  lines.push("legend: STACK = host call-stack overflow (recursion-based evaluator on a deep workload)")
  lines.push("        DNF = disp step-budget exhausted · TIMEOUT = wall-timeout · FAIL = NF ≠ disp-eager oracle")

  const out = lines.join("\n")
  console.log(out)
  mkdirSync(join(HERE, "logs"), { recursive: true })
  const logPath = join(HERE, "logs", `${new Date().toISOString().replace(/[:.]/g, "-")}.log`)
  writeFileSync(logPath, out + "\n")
  console.log(`\nwrote ${logPath}`)
}

main()
