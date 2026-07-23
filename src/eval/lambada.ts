// Lambada batch-tier peers (EVALUATOR.md).
//
// BatchRunners over the bundled lambda-llc/tree-calculus reducers — INDEPENDENT
// implementations of the same 5-rule calculus, and disp's first external
// differential oracle (all prior conformance was eager-vs-naive, same author/codec
// lineage). One project ships ELEVEN evaluators (eager/lazy × value-rep ×
// app-rep × memory strategy); build.sh bundles a selectable CLI (cli.cjs) and we
// register each as its own peer.
//
// Out-of-process and one-shot by design: a per-operation subprocess Session is
// too chatty for elaboration. Each fold spawns `node cli.cjs -e <name> -ternary t0 t1 …`,
// which folds the terms by application from identity (≡ from t0, the same fold
// sessionBatchRunner does in-process) and prints the ternary normal form. Ternary
// is byte-identical to disp's (preorder 0/1/2, no separators), so no codec shim.
// Used for benchmarks + differential conformance only, never for elaboration.

import { spawnSync } from "node:child_process"
import { existsSync } from "node:fs"
import { fileURLToPath } from "node:url"
import { dirname, join } from "node:path"
import type { BatchRunner } from "./batch.js"
import type { Budget } from "./types.js"

// repo-root/evaluators/lambada/artifacts/cli.cjs — produced by build.sh, gitignored.
// (.cjs, not .js: the bundle is CommonJS and disp's package.json is "type":"module".)
const ARTIFACT = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "evaluators", "lambada", "artifacts", "cli.cjs")

// The 11 evaluators cli.cjs exposes (kept in sync with evaluators/lambada/cli.mts
// REGISTRY). Named lambada-<evaluator> as peers.
export const LAMBADA_EVALUATORS = [
  "eager-func", "eager-node-app", "eager-stacks", "eager-value-adt",
  "eager-value-memory", "eager-value-memoizing", "eager-value-memoizing-alt",
  "lazy-func", "lazy-stacks", "lazy-value-adt", "lazy-value-memory",
] as const
export type LambadaEvaluator = (typeof LAMBADA_EVALUATORS)[number]

export function lambadaArtifactPath(): string { return ARTIFACT }
export function lambadaAvailable(): boolean { return existsSync(ARTIFACT) }

// A BatchRunner over one lambada evaluator. `timeoutMs` is the ONLY budget that
// crosses the boundary (the peer has its own internal limits; the step/interaction
// unit is non-portable, so fold()'s Budget arg is
// accepted for interface conformance but not forwarded.
export function lambadaRunner(evaluator: LambadaEvaluator | string = "lazy-stacks", timeoutMs = 60_000): BatchRunner {
  return {
    name: `lambada-${evaluator}`,
    fold(terms: string[], _budget?: Budget): string {
      if (terms.length === 0) throw new Error("lambada fold: no terms")
      if (!existsSync(ARTIFACT))
        throw new Error(`lambada artifact missing: ${ARTIFACT} (run evaluators/lambada/build.sh)`)
      const res = spawnSync("node", [ARTIFACT, "-e", evaluator, "-ternary", ...terms], {
        encoding: "utf8",
        timeout: timeoutMs,
        maxBuffer: 256 * 1024 * 1024,   // big normal forms (e.g. merge-sort lists) expand under ternary
      })
      if (res.error) throw new Error(`lambada(${evaluator}) spawn failed: ${(res.error as Error).message}`)
      if (res.status !== 0)
        throw new Error(`lambada(${evaluator}) exited ${res.status} (signal ${res.signal}): ${(res.stderr ?? "").slice(0, 300)}`)
      return res.stdout.trim()
    },
  }
}
