// Validate every playground example against the real compiler. The Learn
// page's samples are regions of lib/tests/by_example.test.disp, which the test
// suite already runs.
import { writeFileSync, rmSync, readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { spawnSync } from 'node:child_process'
import { manifest } from '../src/lib/disp/examples.ts'

const repo = fileURLToPath(new URL('../..', import.meta.url))
let failed = 0

const only = process.argv[2] // optional substring filter

// Example sources come from the real files (under tsx the Vite glob in
// examples.ts is inert) — the manifest is the single list of what ships.
const cases: { id: string; source: string }[] = [
  ...manifest.map((m) => ({ id: m.id, source: readFileSync(`${repo}/examples/${m.file}`, 'utf-8') }))
].filter((c) => !only || c.id.includes(only))

for (const ex of cases) {
  const tmp = `${repo}/lib/tests/__site_example_${ex.id}.disp`
  writeFileSync(tmp, ex.source)
  const r = spawnSync('npx', ['tsx', 'src/run.ts', tmp], {
    cwd: repo,
    encoding: 'utf-8',
    timeout: 300_000
  })
  rmSync(tmp, { force: true })
  const out = (r.stdout + r.stderr).trim().split('\n').filter(l => !l.startsWith('>')).join(' | ')
  const ok = r.status === 0
  if (!ok) failed++
  console.log(`${ok ? 'PASS' : 'FAIL'}  ${ex.id.padEnd(10)} ${out.slice(0, 300)}`)
}

process.exit(failed ? 1 : 0)
