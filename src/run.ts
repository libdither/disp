// Driver: load a .disp file, parse + compile, run tests.
// Tests pass iff lhs.id === rhs.id (hash-cons identity).

import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { parseProgram, type ParseItemStats } from "./compile.js"
import { prettyTree, type Tree } from "./eval/eager.js"
import type { Session, EvalStats } from "./eval/types.js"
import { getBackend, defaultBackendName } from "./eval/registry.js"
import { emitBlob } from "./format/export.js"

interface RunResult { defs: number; tests: number; passed: number; failed: { i: number; msg: string }[] }
interface RunOptions { onParseItem?: (item: ParseItemStats) => void; session?: Session<Tree> }

export function runFile(path: string, options: RunOptions = {}): RunResult {
  // One fresh session per file: isolates the apply memo / stats (replacing the
  // old clearApplyCache/reset sprinkles). Callers may pass their own (any backend,
  // e.g. --evaluator) to read its stats afterward. The default tracks the registry
  // (rust-eager when built, else eager) so the test harness gets rust-eager's OOM headroom.
  const session = options.session ?? (getBackend(defaultBackendName).createSession() as unknown as Session<Tree>)
  const abs = resolve(path)
  const src = readFileSync(abs, "utf-8")
  const decls = parseProgram(src, abs, { onItem: options.onParseItem, session })
  // Name registry for prettyTree: map each definition's hash-consed tree id to its
  // name (first definition wins on id collisions — many atoms share one leaf id).
  // Includes opened/imported names, which land in `decls` as Defs in legacy mode.
  const names = new Map<number, string>()
  for (const d of decls) {
    if (d.kind === "Def" && !names.has(d.tree.id)) names.set(d.tree.id, d.name)
  }
  // Failure printer. prettyTree expects a JS Tree (eager backend); on a
  // handle backend (e.g. rust-eager-native, where handles are numbers) it
  // garbage-decodes — every side printed as the first def's name. Route by
  // representation: JS trees keep the named pretty; handles decode
  // structurally via the Session ABI's classify (depth-capped).
  const prettyViaClassify = (h: Tree, depth: number): string => {
    if (!session.classify) return `<handle ${String(h)}>`
    if (depth <= 0) return "…"
    const c = session.classify(h)
    switch (c.tag) {
      case "leaf": return "t"
      case "stem": return `t(${prettyViaClassify(c.child, depth - 1)})`
      case "fork": return `t(${prettyViaClassify(c.left, depth - 1)})(${prettyViaClassify(c.right, depth - 1)})`
    }
  }
  const pretty = (h: Tree): string =>
    (h !== null && typeof h === "object" && "id" in (h as object)) ? prettyTree(h, names) : prettyViaClassify(h, 12)
  const result: RunResult = { defs: 0, tests: 0, passed: 0, failed: [] }
  let testIdx = 0
  for (const d of decls) {
    if (d.kind === "Def") {
      result.defs++
    } else {
      result.tests++; testIdx++
      if (session.equal!(d.lhs, d.rhs)) {
        result.passed++
      } else {
        result.failed.push({
          i: testIdx,
          msg: `mismatch:\n    lhs = ${pretty(d.lhs)}\n    rhs = ${pretty(d.rhs)}`,
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
  const valOf = (name: string): string | undefined => {
    const f = args.find(a => a === `--${name}` || a.startsWith(`--${name}=`))
    return f && f.includes("=") ? f.split("=")[1] : undefined
  }
  const backendName = valOf("evaluator") ?? defaultBackendName
  const emitName = valOf("emit")
  const file = args.find((arg: string) => !arg.startsWith("--"))
  if (!file) { console.error("usage: tsx src/run.ts [--evaluator=<name>] [--emit=<binding>] [--stats] [--stats-detail] [--stats-all] <file.disp>"); process.exit(1) }
  try {
    const session = getBackend(backendName).createSession() as unknown as Session<Tree>
    // --emit=<binding>: compile the file and print the binding's self-contained
    // ternary blob (defs inline by construction — EVALUATOR_PLAN §3.1/§4), then exit.
    if (emitName) {
      const abs = resolve(file)
      const decls = parseProgram(readFileSync(abs, "utf-8"), abs, { session })
      const def = decls.find(d => d.kind === "Def" && d.name === emitName)
      if (!def || def.kind !== "Def") { console.error(`emit: binding '${emitName}' not found in ${file}`); process.exit(1) }
      console.log(emitBlob(session, def.tree))
      process.exit(0)
    }
    const itemStats: ParseItemStats[] = []
    const r = runFile(file, { session, onParseItem: showStatsDetail ? item => itemStats.push(item) : undefined })
    console.log(`defs: ${r.defs}, tests: ${r.tests}, passed: ${r.passed}, failed: ${r.failed.length} [${backendName}]`)
    if (showStats) {
      const s = (session.stats?.() ?? { steps: 0 }) as EvalStats
      console.log(
        `stats: steps=${s.steps}, calls=${s.calls}, maxStack=${s.maxStack}, ` +
        `uniqueNodes=${s.uniqueNodes}, memoEntries=${s.cacheEntries}`,
      )
      console.log(
        `rules: K=${s.kRules}, S=${s.sRules}, triage=${s.triageLeafRules + s.triageStemRules + s.triageForkRules}, ` +
        `treeEq=${s.treeEqRules}, leaf=${s.leafRules}, stem=${s.stemRules}`,
      )
      console.log(`memo: hits=${s.memoHits}, misses=${s.memoMisses}, writes=${s.memoWrites}`)
    }
    if (showStatsDetail) {
      let prev: EvalStats = {
        steps: 0, calls: 0, uniqueNodes: 0, memoWrites: 0,
        kRules: 0, sRules: 0, triageLeafRules: 0, triageStemRules: 0, triageForkRules: 0, treeEqRules: 0,
      }
      const deltas: string[] = []
      for (const item of itemStats) {
        const s = item.stats
        const triage = (s.triageLeafRules + s.triageStemRules + s.triageForkRules) -
          (prev.triageLeafRules + prev.triageStemRules + prev.triageForkRules)
        const label = item.kind === "let" ? `let ${item.name}` :
          item.kind === "test" ? `test ${item.testIndex}` :
          item.kind === "given" ? `given ${item.name}` :
          item.kind === "field" ? `field ${item.name}` : "open"
        const source = item.sourcePath ? item.sourcePath.replace(process.cwd() + "/", "") : "<anonymous>"
        deltas.push(
          `item: ${source} ${label} steps=${s.steps - prev.steps}, calls=${s.calls - prev.calls}, ` +
          `uniqueNodes=${s.uniqueNodes - prev.uniqueNodes}, memoWrites=${s.memoWrites - prev.memoWrites}, ` +
          `S=${s.sRules - prev.sRules}, K=${s.kRules - prev.kRules}, triage=${triage}, treeEq=${s.treeEqRules - prev.treeEqRules}`,
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
