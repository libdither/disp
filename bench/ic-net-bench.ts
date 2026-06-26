// rust-ic-net vs rust-eager — the M2d head-to-head (RUST_IC_NET_DESIGN.md §8/§10 "can
// the materialized net compete?"). It isolates the two variables that a naive comparison
// conflates: execution environment (wasm-VM in-process vs native subprocess) and ic-net's
// parallel scaling. So each reducer is measured BOTH ways, on the same workloads, and
// every normal form is validated against the disp-eager oracle.
//
//   tsx bench/ic-net-bench.ts                 # default sizes
//   BENCH_FIB=12 BENCH_REPS=5 tsx bench/ic-net-bench.ts
//
// Contestants:
//   rust-eager-wasm   in-process WebAssembly (what disp actually uses; the default backend)
//   rust-eager-native native subprocess (eager-cli) — the fair native baseline
//   ic-net-wasm       in-process WebAssembly (sequential; the only ic-net path wasm can host)
//   ic-net-native-Nt  native subprocess (ic-net-cli) at N threads — the parallel path
//
// reduce_ms excludes process/session setup on both sides (in-process: timed around fold();
// native: the CLI's own t0 is after arena alloc). In-process numbers enjoy a warm JIT, so
// they flatter the wasm backends — read the comparison as orders of magnitude, not %.

import { spawnSync } from "node:child_process"
import { readFileSync, existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import { eagerBackend } from "../src/eval/eager.js"
import { rustEagerBackend, rustEagerAvailable } from "../src/eval/rust-eager.js"
import { icNetBackend, icNetAvailable } from "../src/eval/ic-net.js"
import { sessionBatchRunner } from "../src/eval/batch.js"
import type { EvalBackend } from "../src/eval/types.js"

const HERE = dirname(fileURLToPath(import.meta.url))
const ROOT = join(HERE, "..")
const EAGER_CLI = join(ROOT, "evaluators", "rust-eager", "crate", "target", "release", "eager-cli")
const ICNET_CLI = join(ROOT, "evaluators", "rust-ic-net", "crate", "target", "release", "ic-net-cli")

const REPS = Number(process.env.BENCH_REPS ?? 3)
const BUDGET = 8_000_000_000
const FIB = Number(process.env.BENCH_FIB ?? 10)
const EXP = Number(process.env.BENCH_EXP ?? 4)

const programs: Record<string, string> = JSON.parse(
  readFileSync(join(ROOT, "bench", "programs", "lambada-benchmarks.json"), "utf8"),
).programs

// nat → ternary (LSB-first) + nat list, copied from test/eval-ic-net.test.ts.
function encodeNat(n: number): string {
  const bits: number[] = []
  for (let m = n; m > 0; m >>= 1) bits.push(m & 1)
  let r = "0"
  for (let i = bits.length - 1; i >= 0; i--) r = (bits[i] === 1 ? "210" : "20") + r
  return r
}
function makeNatList(start: number, stop: number, step: number): string {
  let r = "0"
  for (let k = stop; k !== start - step; k -= step) r = "2" + encodeNat(k) + r
  return r
}

interface Case { name: string; terms: string[] }
const cases: Case[] = [
  { name: `fib(${FIB})`, terms: [programs["recursive-fib"], encodeNat(FIB)] },
  { name: `exp(${EXP})`, terms: [programs["silly-exp"], encodeNat(EXP)] },
  { name: "exercise(30)", terms: [programs["exercise-rules"], encodeNat(30)] },
  { name: "merge-sort", terms: [programs["merge-sort"], makeNatList(6, 1, -1)] },
  { name: "size", terms: [programs["size"], programs["size"]] },
]

type Outcome = { ms: number; ok: boolean; dnf: boolean }
const FAIL: Outcome = { ms: NaN, ok: false, dnf: false }
const DNF: Outcome = { ms: NaN, ok: true, dnf: true }
const best = (a: Outcome, b: Outcome): Outcome =>
  !a.ok || a.dnf ? b : !b.ok || b.dnf ? a : a.ms <= b.ms ? a : b

// ── oracle (disp-eager, in-process) ──
function oracleNF(terms: string[]): string | null {
  const s = eagerBackend.createSession()
  try {
    return sessionBatchRunner("eager", s).fold(terms, { remaining: 5_000_000_000 })
  } catch {
    return null
  } finally {
    s.dispose()
  }
}

// ── in-process measurement (one fresh session per run; fold() timed) ──
function measureInProc(backend: EvalBackend, terms: string[], oracle: string): Outcome {
  let acc = FAIL
  for (let r = 0; r < REPS; r++) {
    const s = backend.createSession()
    try {
      const run = sessionBatchRunner(backend.name, s as never)
      const t0 = performance.now()
      const nf = run.fold(terms, { remaining: BUDGET })
      const ms = performance.now() - t0
      acc = best(acc, nf === oracle ? { ms, ok: true, dnf: false } : FAIL)
    } catch {
      acc = best(acc, DNF)
    } finally {
      s.dispose()
    }
  }
  return acc
}

// ── native subprocess measurement (parse reduce_ms from stderr) ──
function measureNative(cli: string, args: string[], oracle: string): Outcome {
  let acc = FAIL
  for (let r = 0; r < REPS; r++) {
    const p = spawnSync(cli, args, { encoding: "utf8", maxBuffer: 1 << 30 })
    if (p.status !== 0 || !p.stdout) {
      acc = best(acc, DNF)
      continue
    }
    const nf = p.stdout.trim()
    const m = /reduce_ms=([0-9.]+)/.exec(p.stderr ?? "")
    if (!m) {
      acc = best(acc, FAIL)
      continue
    }
    acc = best(acc, nf === oracle ? { ms: Number(m[1]), ok: true, dnf: false } : FAIL)
  }
  return acc
}

// ── render ──
const fmt = (o: Outcome): string => (!o.ok ? "FAIL" : o.dnf ? "DNF" : o.ms < 1 ? o.ms.toFixed(3) : o.ms.toFixed(1))
function table(title: string, rows: string[], cols: string[], grid: Record<string, Record<string, Outcome>>): void {
  const labelW = Math.max(...rows.map((r) => r.length), 10)
  const colW = cols.map((c) => Math.max(c.length, ...rows.map((r) => fmt(grid[r][c]).length), 6))
  const pad = (s: string, w: number) => s.padStart(w)
  console.log(`\n${title}  (best of ${REPS}, reduce_ms; FAIL = NF≠oracle, DNF = budget)`)
  console.log([pad("contestant", labelW), ...cols.map((c, i) => pad(c, colW[i]))].join("  "))
  for (const r of rows) console.log([pad(r, labelW), ...cols.map((c, i) => pad(fmt(grid[r][c]), colW[i]))].join("  "))
}

// ── run ──
console.log(`rust-ic-net vs rust-eager — M2d head-to-head (reps=${REPS}, budget=${BUDGET})`)
if (!rustEagerAvailable()) console.log("! rust-eager wasm artifact missing — in-process row skipped")
if (!icNetAvailable()) console.log("! ic-net wasm artifact missing — in-process row skipped")
const haveEagerCli = existsSync(EAGER_CLI)
const haveIcNetCli = existsSync(ICNET_CLI)
if (!haveEagerCli) console.log(`! ${EAGER_CLI} missing — build: (cd evaluators/rust-eager/crate && cargo build --release --bin eager-cli)`)
if (!haveIcNetCli) console.log(`! ${ICNET_CLI} missing — build: (cd evaluators/rust-ic-net/crate && cargo build --release --bin ic-net-cli)`)

// Program grid: the 2×2 (env × evaluator) + ic-net thread sweep on real workloads.
const progGrid: Record<string, Record<string, Outcome>> = {}
type Runner = { name: string; run: (c: Case, oracle: string) => Outcome }
const runners: Runner[] = []
if (rustEagerAvailable()) runners.push({ name: "rust-eager-wasm", run: (c, o) => measureInProc(rustEagerBackend, c.terms, o) })
if (haveEagerCli) runners.push({ name: "rust-eager-native", run: (c, o) => measureNative(EAGER_CLI, ["-budget", String(BUDGET), "-ternary", ...c.terms], o) })
if (icNetAvailable()) runners.push({ name: "ic-net-wasm", run: (c, o) => measureInProc(icNetBackend, c.terms, o) })
for (const t of [1, 4]) {
  if (haveIcNetCli) runners.push({ name: `ic-net-native-${t}t`, run: (c, o) => measureNative(ICNET_CLI, ["-threads", String(t), "-budget", String(BUDGET), "-ternary", ...c.terms], o) })
}

const oracles = new Map<string, string>()
for (const c of cases) {
  const o = oracleNF(c.terms)
  if (o === null) {
    console.log(`! oracle DNF for ${c.name} — column dropped`)
    continue
  }
  oracles.set(c.name, o)
}
const liveCases = cases.filter((c) => oracles.has(c.name))
for (const rn of runners) {
  progGrid[rn.name] = {}
  for (const c of liveCases) progGrid[rn.name][c.name] = rn.run(c, oracles.get(c.name)!)
}
table("PROGRAMS", runners.map((r) => r.name), liveCases.map((c) => c.name), progGrid)

// Wide grid: 2^depth independent not^chain chains — the embarrassingly-parallel workload.
// In-process wasm can't host the parallel path; this is native subprocess only.
if (haveEagerCli && haveIcNetCli) {
  const wides: Array<{ name: string; d: number; ch: number }> = [
    { name: "d10×c8", d: 10, ch: 8 },
    { name: "d12×c8", d: 12, ch: 8 },
    { name: "d13×c8", d: 13, ch: 8 },
  ]
  const wideRows = ["rust-eager-native", "ic-net-native-1t", "ic-net-native-2t", "ic-net-native-4t", "ic-net-native-8t"]
  const wideGrid: Record<string, Record<string, Outcome>> = {}
  for (const r of wideRows) wideGrid[r] = {}
  for (const w of wides) {
    const oracle = spawnSync(EAGER_CLI, ["-wide", String(w.d), String(w.ch)], { encoding: "utf8", maxBuffer: 1 << 30 }).stdout.trim()
    wideGrid["rust-eager-native"][w.name] = measureNative(EAGER_CLI, ["-wide", String(w.d), String(w.ch)], oracle)
    for (const t of [1, 2, 4, 8]) {
      wideGrid[`ic-net-native-${t}t`][w.name] = measureNative(ICNET_CLI, ["-threads", String(t), "-wide", String(w.d), String(w.ch)], oracle)
    }
  }
  table("WIDE (native subprocess only — 2^d independent chains)", wideRows, wides.map((w) => w.name), wideGrid)
}
