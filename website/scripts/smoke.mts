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
let dozen := double 6
test quadruple 3 = 12
test param_apply Type Nat = Ok true
`,
  PLAYGROUND,
  true,
  onItem
)
// defs must report BOTH the exported field and the private let, each with its
// source line and (wantDefPretty) a value — the playground's inline outputs
console.log(JSON.stringify({
  ...out1,
  defs: out1.defs.map(d => ({ name: d.name, line: d.line, endLine: d.endLine, pretty: d.pretty?.slice(0, 48) }))
}, null, 1))
console.log(`items streamed: ${items}, wall: ${Date.now() - t0}ms`)
if (!out1.defs.some(d => d.name === 'dozen' && d.line === 4 && d.pretty === '12')) {
  console.error('FAIL: private let missing from defs (want dozen @ line 4 = 12)')
  process.exit(1)
}
if (!out1.defs.some(d => d.name === 'quadruple' && d.line === 3)) {
  console.error('FAIL: exported def missing line attribution')
  process.exit(1)
}
// scope names alias inside values: quadruple = the composition glue around
// the NAME double (module exports registered from the module cache; the
// embedded occurrences are hash-cons-identical to the scope binding)
const quad = out1.defs.find(d => d.name === 'quadruple')
if (!quad?.pretty?.includes('double')) {
  console.error(`FAIL: quadruple should render via the scope name 'double', got: ${quad?.pretty}`)
  process.exit(1)
}

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
  PLAYGROUND,
  true
)
console.log(JSON.stringify({ ok: out3.ok, value: out3.value, hint: out3.valueHint, node: out3.valueNode, error: out3.error }))
// structured value + click-to-unfold round trip: the eval value decodes as
// the nat 12; re-rendering its handle rawRoot exposes the numeral's fork
// (leaf successor-spine head) whose tail re-decodes as 11
if (out3.valueNode?.k !== 'nat' || out3.valueNode.n !== 12) {
  console.error('FAIL: eval valueNode should decode as nat 12')
  process.exit(1)
}
const raw12 = runner.renderValue(out3.valueNode.h, 50, true)
if (raw12.k !== 'fork' || raw12.c[0].k !== 'leaf' || raw12.c[1].k !== 'nat' || raw12.c[1].n !== 11) {
  console.error(`FAIL: rawRoot unfold of 12 should be fork(leaf, 11), got ${JSON.stringify(raw12)}`)
  process.exit(1)
}

console.log('--- run 4: failing test pretty ---')
const out4 = runner.run(
  `open use "../kernel/prelude.disp"\ntest succ zero = zero\n`,
  PLAYGROUND,
  false,
  onItem
)
console.log(JSON.stringify({ ok: out4.ok, tests: out4.tests }))
if (out4.tests[0]?.lhsNode?.k !== 'nat' || out4.tests[0]?.rhsNode?.k !== 'leaf') {
  console.error('FAIL: failing test should carry structured got/want (nat 1 vs leaf)')
  process.exit(1)
}

console.log('--- run 5: parse error surface ---')
const out5 = runner.run(`let x = 5\n`, PLAYGROUND, false, onItem)
console.log(JSON.stringify({ ok: out5.ok, error: out5.error?.slice(0, 200), line: out5.errorLine }))

console.log(`final mem: ${(runner.memoryBytes() / 1e6).toFixed(1)}MB`)
