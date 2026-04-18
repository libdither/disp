// Driver: load a .disp file, parse + compile, run tests.
// Tests pass iff lhs.id === rhs.id (hash-cons identity).

import { readFileSync } from "node:fs"
import { resolve } from "node:path"
import { parseProgram } from "./parse.js"
import { treeEqual, prettyTree } from "./tree.js"

interface RunResult { defs: number; tests: number; passed: number; failed: { i: number; msg: string }[] }

export function runFile(path: string): RunResult {
  const abs = resolve(path)
  const src = readFileSync(abs, "utf-8")
  const decls = parseProgram(src, abs)
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
  const file = process.argv[2]
  if (!file) { console.error("usage: tsx src/run.ts <file.disp>"); process.exit(1) }
  try {
    const r = runFile(file)
    console.log(`defs: ${r.defs}, tests: ${r.tests}, passed: ${r.passed}, failed: ${r.failed.length}`)
    for (const f of r.failed) console.error(`  [${f.i}] ${f.msg}`)
    process.exit(r.failed.length > 0 ? 1 : 0)
  } catch (e) {
    console.error(`error: ${(e as Error).message}`)
    process.exit(1)
  }
}
