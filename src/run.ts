// Driver: load a .disp file, parse + compile, run tests.
// Tests pass iff lhs.id === rhs.id (hash-cons identity).

import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { parseProgram, type ParseItemStats } from "./parse.js"
import {
  treeEqual, prettyTree,
  clearApplyCache, getApplyStats, resetApplyStats, resetCacheStats,
} from "./tree.js"

interface RunResult { defs: number; tests: number; passed: number; failed: { i: number; msg: string }[] }
interface RunOptions { onParseItem?: (item: ParseItemStats) => void }

export function runFile(path: string, options: RunOptions = {}): RunResult {
  const abs = resolve(path)
  const src = readFileSync(abs, "utf-8")
  const decls = parseProgram(src, abs, { onItem: options.onParseItem })
  const result: RunResult = { defs: 0, tests: 0, passed: 0, failed: [] }
  let testIdx = 0
  for (const d of decls) {
    if (d.kind === "Def") {
      result.defs++
    } else {
      result.tests++; testIdx++
      if (treeEqual(d.lhs, d.rhs)) {
        result.passed++
      } else {
        result.failed.push({
          i: testIdx,
          msg: `mismatch:\n    lhs = ${prettyTree(d.lhs)}\n    rhs = ${prettyTree(d.rhs)}`,
        })
      }
    }
  }
  return result
}

if (process.argv[1] && process.argv[1].endsWith("run.ts")) {
  const args = process.argv.slice(2)
  const showStats = args.includes("--stats")
  const showStatsDetail = args.includes("--stats-detail")
  const showStatsAll = args.includes("--stats-all")
  const file = args.find(arg => !arg.startsWith("--"))
  if (!file) { console.error("usage: tsx src/run.ts [--stats] [--stats-detail] [--stats-all] <file.disp>"); process.exit(1) }
  try {
    if (showStats) {
      clearApplyCache()
      resetApplyStats()
      resetCacheStats()
    }
    const itemStats: ParseItemStats[] = []
    const r = runFile(file, { onParseItem: showStatsDetail ? item => itemStats.push(item) : undefined })
    console.log(`defs: ${r.defs}, tests: ${r.tests}, passed: ${r.passed}, failed: ${r.failed.length}`)
    if (showStats) {
      const s = getApplyStats()
      console.log(
        `stats: steps=${s.steps}, calls=${s.calls}, maxStack=${s.maxStack}, ` +
        `uniqueNodes=${s.uniqueNodes}, memoEntries=${s.cacheEntries}`,
      )
      console.log(
        `rules: K=${s.kRules}, S=${s.sRules}, triage=${s.triageLeafRules + s.triageStemRules + s.triageForkRules}, ` +
        `fastEq=${s.fastEqRules}, leaf=${s.leafRules}, stem=${s.stemRules}`,
      )
      console.log(`memo: hits=${s.memoHits}, misses=${s.memoMisses}, writes=${s.memoWrites}`)
    }
    if (showStatsDetail) {
      let prev = {
        steps: 0, calls: 0, uniqueNodes: 0, memoWrites: 0,
        kRules: 0, sRules: 0, triageLeafRules: 0, triageStemRules: 0, triageForkRules: 0, fastEqRules: 0,
      }
      const deltas: string[] = []
      for (const item of itemStats) {
        const s = item.stats
        const triage = (s.triageLeafRules + s.triageStemRules + s.triageForkRules) -
          (prev.triageLeafRules + prev.triageStemRules + prev.triageForkRules)
        const label = item.kind === "let" ? `let ${item.name}` :
          item.kind === "test" ? `test ${item.testIndex}` : "open"
        const source = item.sourcePath ? item.sourcePath.replace(process.cwd() + "/", "") : "<anonymous>"
        deltas.push(
          `item: ${source} ${label} steps=${s.steps - prev.steps}, calls=${s.calls - prev.calls}, ` +
          `uniqueNodes=${s.uniqueNodes - prev.uniqueNodes}, memoWrites=${s.memoWrites - prev.memoWrites}, ` +
          `S=${s.sRules - prev.sRules}, K=${s.kRules - prev.kRules}, triage=${triage}, fastEq=${s.fastEqRules - prev.fastEqRules}`,
        )
        prev = s
      }
      const selected = showStatsAll
        ? deltas
        : [...deltas].sort((a, b) => {
          const aSteps = Number(a.match(/steps=(-?\d+)/)?.[1] ?? 0)
          const bSteps = Number(b.match(/steps=(-?\d+)/)?.[1] ?? 0)
          return bSteps - aSteps
        }).slice(0, 20)
      console.log(showStatsAll ? "items:" : "top-items:")
      for (const line of selected) console.log(line)
    }
    for (const f of r.failed) console.error(`  [${f.i}] ${f.msg}`)
    process.exit(r.failed.length > 0 ? 1 : 0)
  } catch (e) {
    console.error(`error: ${(e as Error).message}`)
    process.exit(1)
  }
}
