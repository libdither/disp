// Driver: load a .disp file, parse + compile, run tests.
// Tests pass iff lhs.id === rhs.id (hash-cons identity).

import { existsSync, readFileSync, readdirSync, statSync } from "node:fs"
import { createHash } from "node:crypto"
import { dirname, join, resolve } from "node:path"
import { fileURLToPath } from "node:url"
import { parseProgram, type Decl, type ParseItemStats } from "./compile.js"
import { prettyTree, type Tree } from "./eval/eager.js"
import type { Session, EvalStats } from "./eval/types.js"
import { getBackend, defaultBackendName } from "./eval/registry.js"
import { emitBlob } from "./format/export.js"
import { printValue, nameKey } from "./format/print.js"
import { driveMain, findMain } from "./driver.js"

interface RunResult { defs: number; tests: number; passed: number; failed: { i: number; msg: string; line?: number }[] }
interface RunOptions { onParseItem?: (item: ParseItemStats) => void; session?: Session<Tree>; onDecls?: (decls: Decl[]) => void; exposeLocals?: boolean }

export function runFile(path: string, options: RunOptions = {}): RunResult {
  // One fresh session per file: isolates the apply memo / stats (replacing the
  // old clearApplyCache/reset sprinkles). Callers may pass their own (any backend,
  // e.g. --evaluator) to read its stats afterward. The default tracks the registry
  // (rust-eager when built, else eager) so the test harness gets rust-eager's OOM headroom.
  const session = options.session ?? (getBackend(defaultBackendName).createSession() as unknown as Session<Tree>)
  const abs = resolve(path)
  const src = readFileSync(abs, "utf-8")
  const decls = parseProgram(src, abs, { onItem: options.onParseItem, session, exposeLocals: options.exposeLocals })
  options.onDecls?.(decls)
  // Name registry: map each definition's tree (hash-cons id / handle) to its
  // name (first definition wins on collisions — many atoms share one leaf id).
  // Includes opened/imported names, which land in `decls` as Defs in legacy mode.
  const names = new Map<unknown, string>()
  for (const d of decls) {
    if (d.kind === "Def" && !names.has(nameKey(d.tree))) names.set(nameKey(d.tree), d.name)
  }
  // Failure printer. prettyTree expects a JS Tree (eager backend); handle
  // backends (e.g. rust-eager-native) decode via the Session ABI's classify
  // with nat/string/record/name sugar (format/print.ts).
  const pretty = (h: Tree): string =>
    (h !== null && typeof h === "object" && "id" in (h as object))
      ? prettyTree(h, names as Map<number, string>)
      : printValue(session, h, { names })
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
          line: d.line,
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
  const printSpec = valOf("print")
  // Block-internal probing (Def__local bindings): always on under --print, and
  // opt-in via --locals for plain runs of probe files that reference internals.
  const exposeLocals = printSpec != null || args.includes("--locals")
  const file = args.find((arg: string) => !arg.startsWith("--"))
  if (!file) { console.error("usage: tsx src/run.ts [--evaluator=<name>] [--emit=<binding>] [--print=<name,glob*,Def__local>] [--locals] [--stats] [--stats-detail] [--stats-all] <file.disp>"); process.exit(1) }
  try {
    const session = getBackend(backendName).createSession() as unknown as Session<Tree>
    // Warm start from the test harness's persistent reduction cache when present
    // (memo entries are calculus-level facts; the stamp pins the evaluator build).
    // Read-only here — probes load warmth, only the harness writes it. Any shard's
    // snapshot helps (each contains the kernel's facts); newest wins.
    if (process.env.DISP_MEMO_CACHE !== "0" && session.loadSnapshot) {
      const root = join(dirname(fileURLToPath(import.meta.url)), "..")
      const cacheDir = join(root, ".disp-test-cache")
      const artifact = join(root, "evaluators", "rust-eager", "artifacts", "rust_eager.node")
      const memoBin = existsSync(cacheDir)
        ? readdirSync(cacheDir).filter(f => /^memo-\d+of\d+\.bin$/.test(f))
            .map(f => join(cacheDir, f))
            .sort((a, b) => statSync(b).mtimeMs - statSync(a).mtimeMs)[0]
        : undefined
      if (memoBin && existsSync(artifact)) {
        const stamp = `v1:${createHash("sha256").update(readFileSync(artifact)).digest("hex")}`
        if (session.loadSnapshot(memoBin, stamp)) console.error(`[memo] warm: ${memoBin}`)
      }
    }
    // --emit=<binding>: compile the file and print the binding's self-contained
    // ternary blob (definitions inline by construction; see EVALUATOR.md), then exit.
    if (emitName) {
      const abs = resolve(file)
      const decls = parseProgram(readFileSync(abs, "utf-8"), abs, { session })
      const def = decls.find(d => d.kind === "Def" && d.name === emitName)
      if (!def || def.kind !== "Def") { console.error(`emit: binding '${emitName}' not found in ${file}`); process.exit(1) }
      console.log(emitBlob(session, def.tree))
      process.exit(0)
    }
    // --print=<specs>: decoded human output for bindings — comma-separated names,
    // `*` globs, and `Def__local` block-internal probes (any `__` in the spec
    // turns on exposeLocals; see ParseProgramOptions).
    if (printSpec) {
      const abs = resolve(file)
      // Bindings come from the item stream (imported defs and private lets
      // included — decls carries only the root file's exports), then decls-only
      // extras (the exposeLocals pseudo-defs). Same order feeds the name map.
      const defs: { name: string; tree: Tree }[] = []
      const onParseItem = (item: ParseItemStats) => {
        if ((item.kind === "field" || item.kind === "let") && item.name && item.tree !== undefined)
          defs.push({ name: item.name, tree: item.tree })
      }
      const decls = parseProgram(readFileSync(abs, "utf-8"), abs, { session, exposeLocals, onItem: onParseItem })
      for (const d of decls) {
        if (d.kind === "Def" && !defs.some(x => x.name === d.name)) defs.push({ name: d.name, tree: d.tree })
      }
      const names = new Map<unknown, string>()
      for (const d of defs) {
        if (!names.has(nameKey(d.tree))) names.set(nameKey(d.tree), d.name)
      }
      let missing = false
      const seen = new Set<string>()
      for (const spec of printSpec.split(",").map(s => s.trim()).filter(Boolean)) {
        const re = new RegExp(`^${spec.split("*").map(p => p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join(".*")}$`)
        const matches = defs.filter(d => re.test(d.name) && !seen.has(d.name))
        if (matches.length === 0 && !spec.includes("*")) { console.error(`print: binding '${spec}' not found in ${file}`); missing = true }
        for (const m of matches) {
          seen.add(m.name)
          console.log(`${m.name} = ${printValue(session, m.tree, { names, skipName: m.name })}`)
        }
      }
      process.exit(missing ? 1 : 0)
    }
    const itemStats: ParseItemStats[] = []
    let decls: Decl[] = []
    const r = runFile(file, { session, exposeLocals, onDecls: ds => { decls = ds }, onParseItem: showStatsDetail ? item => itemStats.push(item) : undefined })
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
    for (const f of r.failed) console.error(`  [${f.line != null ? `${file}:${f.line}` : `test ${f.i}`}] ${f.msg}`)
    // The impure driver: a file exporting `main : Eff io_row X` runs after
    // its tests pass (the annotation itself was already kernel-verified at
    // load; the driver enforces the operational half — see src/driver.ts).
    if (r.failed.length === 0 && findMain(decls)) {
      driveMain(session, decls).then(
        () => process.exit(0),
        e => { console.error(`driver error: ${(e as Error).message}`); process.exit(1) },
      )
    } else {
      process.exit(r.failed.length > 0 ? 1 : 0)
    }
  } catch (e) {
    console.error(`error: ${(e as Error).message}`)
    process.exit(1)
  }
}
