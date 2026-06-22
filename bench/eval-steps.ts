// Evaluation-cost benchmark: reports reduction *steps* (deterministic,
// machine-independent) for a corpus of type-checking-heavy .disp files.
//
// Unlike `vitest bench` (wall-clock), this measures apply-rule counts, so it
// is reproducible across machines and ideal for A/B-ing kernel encoding
// changes (e.g. tagged-union vs §2.6-coproduct CheckerResult/Action).
//
// Run:  npx tsx bench/eval-steps.ts
// Each file is run cold (apply cache + stats reset first), so the numbers are
// the full from-scratch reduction cost.

import { runFile } from "../src/run.js"
import { clearApplyCache, getApplyStats, resetApplyStats, resetCacheStats } from "../src/core/tree.js"

const CORPUS = [
  "lib/tests/types.test.disp",   // every type former, polymorphic Pi, neutrals
  "lib/tests/kernel.test.disp",  // param_apply / hyp_reduce / bind_hyp / checked
  "lib/tests/match.test.disp",   // coproduct match / cut
  "lib/tests/nat.test.disp",
  "lib/tests/list.test.disp",
]

function measure(path: string) {
  clearApplyCache(); resetApplyStats(); resetCacheStats()
  const r = runFile(path)
  const s = getApplyStats()
  if (r.failed.length) throw new Error(`${path}: ${r.failed.length} test(s) failed`)
  return { path, tests: r.tests, ...s }
}

const rows = CORPUS.map(measure)
const total = rows.reduce((a, r) => ({
  steps: a.steps + r.steps, treeEqRules: a.treeEqRules + r.treeEqRules,
  sRules: a.sRules + r.sRules, uniqueNodes: a.uniqueNodes + r.uniqueNodes,
}), { steps: 0, treeEqRules: 0, sRules: 0, uniqueNodes: 0 })

const pad = (s: string | number, n: number) => String(s).padStart(n)
console.log(`${pad("steps", 10)} ${pad("S", 9)} ${pad("treeEq", 8)} ${pad("nodes", 9)}  file`)
for (const r of rows) {
  console.log(`${pad(r.steps, 10)} ${pad(r.sRules, 9)} ${pad(r.treeEqRules, 8)} ${pad(r.uniqueNodes, 9)}  ${r.path}`)
}
console.log(`${pad(total.steps, 10)} ${pad(total.sRules, 9)} ${pad(total.treeEqRules, 8)} ${pad(total.uniqueNodes, 9)}  TOTAL`)
