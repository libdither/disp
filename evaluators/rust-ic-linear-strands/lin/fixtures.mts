// The lattice test's cases (crate/tests/linear.rs): each preset and a few library programs, as the net the
// compiler builds, with the input and the interpreter's answer. `npx tsx evaluators/rust-ic-linear-strands/lin/fixtures.mts`
import { writeFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { show, run, regs } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { PRESETS } from '../../../website/src/lib/evaluators/twoway/library.ts'
import { load, serialize } from './compile.ts'

const cases: [string, string, string][] = PRESETS.filter((p) => !p.prog.includes('eval') && p.name !== 'addw').map((p) => [p.name, p.prog, p.input])
cases.push(['notw 4', 'notw', '0b1011'], ['mirror small', 'mirror', '(△, (△△, △))'], ['addw 2', 'addw', '<0b10, 0b11>'], ['dup', 'unit ; swap ; dup', '((△, △△), △)'])
const lines = cases.map(([name, prog, input]) => {
  const p = parseProg(prog)
  const x = parseVal(input)
  const want = run(p, x)
  if (!want) throw new Error(`${name} gets stuck`)
  return [name, serialize(load(p, x).net), show(x), show(want)].join('\t')
})
writeFileSync(join(dirname(fileURLToPath(import.meta.url)), '../crate/tests/lin-fixtures.txt'), lines.join('\n') + '\n')
console.log(`${lines.length} cases`)
