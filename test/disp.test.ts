// Disp language tests: runs all *.test.disp files through the parser/driver.

import { afterAll, describe, it, expect } from "vitest"
import { existsSync, mkdirSync, readdirSync, readFileSync } from "node:fs"
import { createHash } from "node:crypto"
import { join } from "node:path"
import { runFile } from "../src/run.js"
import { collectSessionRoots } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const testsDir = join(import.meta.dirname, "..", "lib", "tests")

// One session shared across ALL test files: the kernel (`use`d by every file)
// is interned + apply-memoized ONCE for the whole suite instead of re-elaborated
// cold per file — the hash-cons arena and apply memo are session state, so a
// shared session turns 36× redundant kernel reduction into 1× + memo hits. Scope
// is per-runFile-call (a fresh Map stack), so there is no cross-file name bleed;
// only the immutable hash-cons arena / memo / global verifiedModules are shared,
// all of which are sound to share (identical trees → identical ids by design).
// Opt out with DISP_INDEPENDENT_SESSIONS=1 for an isolated arena per file (bounds
// peak memory at one file's worth, at the cost of re-elaborating the kernel each).
const SHARE_SESSIONS = process.env.DISP_INDEPENDENT_SESSIONS !== "1"
// Backend override for the shared session: DISP_EVALUATOR=<name> (e.g. rust-eager-native)
// runs the whole suite on that backend without changing the global default.
const backendName = process.env.DISP_EVALUATOR ?? defaultBackendName
const sharedSession = SHARE_SESSIONS
  ? (getBackend(backendName).createSession() as unknown as Session<Tree>)
  : undefined

// Per-file scoped reclamation (rust-eager native + the eager TS backend): open a scope
// before each file, close it after keeping only the module cache's cross-file roots — so
// the file's elaboration + auto-verify garbage is freed instead of leaking into the
// grow-only arena. DISP_NO_SCOPE=1 falls back to the prior memo-shed-only behavior (for
// A/B measurement). The scope methods are absent on wasm, which keeps using clearCaches.
const USE_SCOPE = process.env.DISP_NO_SCOPE !== "1"
const scoped = sharedSession as unknown as
  | { beginScope?(): void; endScope?(keep: Tree[]): void; clearCaches?(): void; stats?(): { nodes?: number; free?: number } }
  | undefined
let peakNodes = 0
let finalNodes = 0
let finalFree = 0

// Recursively find all .test.disp files under lib/tests/.
function findTestFiles(dir: string, rel = ""): string[] {
  const entries = readdirSync(dir, { withFileTypes: true })
  const out: string[] = []
  for (const e of entries) {
    const p = rel ? `${rel}/${e.name}` : e.name
    if (e.isDirectory()) out.push(...findTestFiles(join(dir, e.name), p))
    else if (e.name.endsWith(".test.disp")) out.push(p)
  }
  return out
}
// DISP_TEST_SHARD=i/n (1-based): run the i-th round-robin slice of the sorted
// file list. The suite is serial on one shared session, so CI splits it across
// jobs; round-robin spreads the heavy alphabetical head instead of loading it
// all onto shard 1. Each shard pays its own kernel elaboration once.
const shardSpec = process.env.DISP_TEST_SHARD
const shardMatch = shardSpec?.match(/^(\d+)\/(\d+)$/)
if (shardSpec && !shardMatch) throw new Error(`DISP_TEST_SHARD must be i/n (got '${shardSpec}')`)
const [shardIdx, shardCount] = shardMatch ? [Number(shardMatch[1]), Number(shardMatch[2])] : [1, 1]
if (shardIdx < 1 || shardIdx > shardCount) throw new Error(`DISP_TEST_SHARD out of range: ${shardSpec}`)
const testFiles = findTestFiles(testsDir).sort().filter((_, i) => i % shardCount === shardIdx - 1)

// --- Persistent reduction cache (rust-eager-native snapshots). ---
// Every test always RUNS; what persists across runs is the evaluator's arena +
// apply memo. Memo entries are calculus-level facts (`apply(f,x) = r` under the
// fixed tree-calculus rules), so kernel/library/elaborator changes never
// invalidate them — they just stop being hit. Only an evaluator change matters,
// which the stamp encodes (a hash of the addon binary): a stale or foreign
// snapshot is refused and the run proceeds cold. Per-shard snapshot files (the
// shards run disjoint suites); DISP_MEMO_CACHE=0 disables.
const MEMO_ON = process.env.DISP_MEMO_CACHE !== "0"
const cacheDir = join(import.meta.dirname, "..", ".disp-test-cache")
const memoPath = join(cacheDir, `memo-${shardIdx}of${shardCount}.bin`)
const memoStamp = (() => {
  if (!MEMO_ON || !sharedSession?.loadSnapshot) return null
  const artifact = join(import.meta.dirname, "..", "evaluators", "rust-eager", "artifacts", "rust_eager.node")
  try { return `v1:${createHash("sha256").update(readFileSync(artifact)).digest("hex")}` } catch { return null }
})()
if (memoStamp && existsSync(memoPath)) {
  const t0 = Date.now()
  const ok = sharedSession!.loadSnapshot!(memoPath, memoStamp)
  console.log(`[disp] reduction cache ${ok ? `warm (${Date.now() - t0}ms load)` : "stale — running cold"}: ${memoPath}`)
}

describe("disp", () => {
  // Files share one session by default (see sharedSession above) so the kernel is
  // elaborated once for the whole suite. Set DISP_INDEPENDENT_SESSIONS=1 to give
  // each file a fresh session (the prior behavior — clean per-file memo/stats).
  for (const file of testFiles) {
    it(file, () => {
      // Session 3: defense-in-depth temporarily disabled — the elaborator's assertTypeCheck
      // path applies the public guarded type to its tree, which now hits q_guard_fn's entry
      // scan; intermediate elaborator trees may contain certified-neutral hypotheses the scan
      // rejects. Re-enable once session 5 routes the elaborator through kernel-internal
      // checking that bypasses the public boundary.
      const t0 = Date.now()
      if (USE_SCOPE) scoped?.beginScope?.()
      const r = runFile(join(testsDir, file), sharedSession ? { session: sharedSession } : {})
      const peak = scoped?.stats?.().nodes ?? 0
      if (peak > peakNodes) peakNodes = peak
      // Reclaim per-file garbage at the file boundary. endScope (rust-eager native) frees
      // everything allocated this file EXCEPT the module cache's exported handles (the
      // cross-file roots) — the kernel and any `use`d modules stay; the auto-verify scratch
      // and this file's elaboration go. Backends without scoped reclamation (wasm/eager) fall
      // back to the memo shed (the prior behavior). Both are pure-cache / live-tree-safe.
      if (USE_SCOPE && scoped?.endScope) scoped.endScope(collectSessionRoots(sharedSession as Session<Tree>))
      else scoped?.clearCaches?.()
      finalNodes = scoped?.stats?.().nodes ?? finalNodes
      finalFree = scoped?.stats?.().free ?? finalFree
      // Live progress: vitest streams stdout as it arrives but prints its own
      // per-FILE line only when this whole suite file ends — without this, a
      // long run is indistinguishable from a hang.
      console.log(`[disp] ${((Date.now() - t0) / 1000).toFixed(1)}s ${file} (${r.passed}/${r.tests})${r.failed.length > 0 ? " FAILED" : ""}`)
      if (r.failed.length > 0) {
        // Each failure names its source line (stamped on the equation item by the
        // tokenizer's line tracking); file:line is clickable in most terminals.
        const msgs = r.failed.map(f =>
          `[${f.line != null ? `${join(testsDir, file)}:${f.line}` : `test ${f.i}`}] ${f.msg}`).join("\n")
        throw new Error(`${file}:\n${msgs}`)
      }
      expect(r.passed).toBe(r.tests)
      expect(r.passed).toBeGreaterThan(0)
    })
  }

  // Report the arena footprint: with scoping, per-file peak stays bounded (kernel + one
  // file); without it, the grow-only arena accumulates the whole suite. Then persist the
  // reduction cache (frozen ∪ live union) — a quiet point: every scope is closed, so the
  // memo is already pruned to the durable module-level facts.
  afterAll(() => {
    if (scoped?.stats) {
      const s = scoped.stats()
      const len = s.nodes ?? finalNodes
      const free = s.free ?? finalFree
      console.log(`[scope] peakLen=${peakNodes} finalLen=${len} finalLive=${len - free} finalHoles=${free} scoping=${USE_SCOPE} backend=${backendName}`)
    }
    if (memoStamp && sharedSession?.saveSnapshot) {
      mkdirSync(cacheDir, { recursive: true })
      const t0 = Date.now()
      const ok = sharedSession.saveSnapshot(memoPath, memoStamp)
      if (ok) console.log(`[disp] reduction cache saved (${Date.now() - t0}ms): ${memoPath}`)
    }
  })
})
