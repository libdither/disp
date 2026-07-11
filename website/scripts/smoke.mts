// Node-side smoke test of the browser runner: real fs (so `use` resolves
// against the actual repo lib/), the vendored rust-eager WASM bytes, and the
// exact DispRunner the worker uses. Validates the full pipeline before any
// browser is involved.
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { DispRunner } from '../src/lib/disp/runner.ts'

const wasmPath = fileURLToPath(new URL('../static/rust_eager.wasm', import.meta.url))
const bytes = readFileSync(wasmPath)
const runner = new DispRunner(bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength))

const PLAYGROUND = fileURLToPath(new URL('../../lib/tests/__playground_smoke.disp', import.meta.url))

let items = 0
const onItem = () => { items++ }

console.log('--- run 1: kernel load + user code ---')
const t0 = Date.now()
const out1 = runner.run(
  `open use "../kernel/prelude.disp"
open use "../std/nat.disp"
quadruple : Nat -> Nat := {n} -> double (double n)
test quadruple 3 = 12
test param_apply Type Nat = Ok true
`,
  PLAYGROUND,
  true,
  onItem
)
console.log(JSON.stringify({ ...out1, defs: out1.defs.map(d => d.name) }, null, 1))
console.log(`items streamed: ${items}, wall: ${Date.now() - t0}ms`)

console.log('--- run 2: warm rerun (module cache) ---')
const t1 = Date.now()
const out2 = runner.run(
  `open use "../kernel/prelude.disp"
n_eq_n : {n : Nat} -> Eq Nat n n := {n} -> refl
test param_apply (Pi Nat ({n} -> Eq Nat n n)) n_eq_n = Ok true
`,
  PLAYGROUND,
  false,
  onItem
)
console.log(JSON.stringify({ ok: out2.ok, tests: out2.tests, error: out2.error, ms: Date.now() - t1 }))

console.log('--- run 3: eval expression ---')
const out3 = runner.evalExpr(
  `open use "../kernel/prelude.disp"\nopen use "../std/nat.disp"`,
  'double (double 3)',
  onItem,
  PLAYGROUND
)
console.log(JSON.stringify({ ok: out3.ok, value: out3.value, hint: out3.valueHint, error: out3.error }))

console.log('--- run 4: failing test pretty ---')
const out4 = runner.run(
  `open use "../kernel/prelude.disp"\ntest succ zero = zero\n`,
  PLAYGROUND,
  false,
  onItem
)
console.log(JSON.stringify({ ok: out4.ok, tests: out4.tests }))

console.log('--- run 5: parse error surface ---')
const out5 = runner.run(`let x = 5\n`, PLAYGROUND, false, onItem)
console.log(JSON.stringify({ ok: out5.ok, error: out5.error?.slice(0, 200), line: out5.errorLine }))

console.log(`final mem: ${(runner.memoryBytes() / 1e6).toFixed(1)}MB`)
