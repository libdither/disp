// Validate every guided playground lesson against the real compiler:
//   - solution MUST pass all tests (and module typecheck if verifyTypes = true)
//   - starter MUST NOT already satisfy the solution (so the exercise is genuine)
//   - breakIt (when present) MUST fail in the expected way
import { readFileSync, writeFileSync, rmSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { parseProgram } from '../../src/compile.ts'
import { RustEagerBrowserSession } from '../src/lib/disp/rust-eager-browser.ts'
import type { Session } from '../../src/eval/types.ts'
import type { Tree } from '../../src/eval/eager.ts'
import { lessons } from '../src/lib/playground/curriculum.ts'

const repo = fileURLToPath(new URL('../..', import.meta.url))
const only = process.argv[2]
const list = lessons.filter((l) => !only || l.id.includes(only) || l.track === only)

const wasm = readFileSync(new URL('../static/rust_eager.wasm', import.meta.url))
const session = new RustEagerBrowserSession(wasm) as unknown as Session<Tree>

function runSnippet(name: string, src: string, verifyTypes: boolean): { ok: boolean; testsPass: boolean; typeErr: boolean; error?: string; ms: number } {
  const t0 = performance.now()
  const modFile = `${repo}/lib/tests/__site_example_curr_${name}.disp`
  writeFileSync(modFile, src)
  try {
    const decls = parseProgram(src, modFile, { session })
    let testsPass = true
    for (const d of decls) {
      if (d.kind === 'Test' && !session.equal!(d.lhs, d.rhs)) testsPass = false
    }
    if (verifyTypes) {
      try {
        parseProgram(`open use "./__site_example_curr_${name}.disp"\n`, `${repo}/lib/tests/playground.disp`, { session })
      } catch (e) {
        const msg = e instanceof Error ? e.message : String(e)
        return { ok: false, testsPass, typeErr: msg.includes('type check failed'), error: msg, ms: performance.now() - t0 }
      }
    }
    return { ok: testsPass, testsPass, typeErr: false, ms: performance.now() - t0 }
  } catch (e) {
    const msg = e instanceof Error ? e.message : String(e)
    return { ok: false, testsPass: false, typeErr: msg.includes('type check failed'), error: msg, ms: performance.now() - t0 }
  } finally {
    rmSync(modFile, { force: true })
  }
}

let failed = 0
for (const l of list) {
  const sol = runSnippet(`${l.id}_sol`, l.solution, l.verifyTypes)
  const st = runSnippet(`${l.id}_starter`, l.starter, l.verifyTypes)
  let ok = true
  const notes: string[] = []
  if (!sol.ok) {
    ok = false
    notes.push(`SOLUTION FAILED: ${sol.error ?? 'test mismatch'}`)
  }
  if (st.ok) {
    ok = false
    notes.push('STARTER ALREADY PASSES (exercise is a no-op!)')
  }
  if (l.breakIt) {
    const br = runSnippet(`${l.id}_break`, l.breakIt.snippet, l.verifyTypes)
    if (br.ok) {
      ok = false
      notes.push('BREAK-IT UNEXPECTEDLY PASSED')
    } else if (l.breakIt.expectKind === 'type-error' && !br.typeErr) {
      ok = false
      notes.push(`BREAK-IT expected type-error, got: ${br.error ?? 'test fail'}`)
    }
  }
  if (!ok) failed++
  console.log(`${ok ? 'PASS' : 'FAIL'}  ${l.id.padEnd(18)} sol=${sol.ms.toFixed(0)}ms  starter=${st.ok ? 'pass' : 'fail'}${l.breakIt ? '  break=ok' : ''}  ${notes.join(' | ')}`)
}

session.dispose?.()
process.exit(failed ? 1 : 0)
