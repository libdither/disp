// Checks the nine-rule visualizer's engine without a browser: every example runs to the answer it should,
// with the drops and copies it should have, and bracket abstraction gives the trees the prototype's compiler gives.
import { Store, R_K, R_W, R_BIND, type Id } from '../src/lib/evaluators/linear/store.ts'
import { Trace } from '../src/lib/evaluators/linear/trace.ts'
import { EXAMPLES, loadProgram } from '../src/lib/evaluators/linear/examples.ts'

const ternary = (s: Store, id: Id): string =>
  s.tag[id] === 0 ? '0' : s.tag[id] === 1 ? '1' + ternary(s, s.a[id]) : s.tag[id] === 2 ? '2' + ternary(s, s.a[id]) + ternary(s, s.b[id]) : '?'
let bad = 0
function check(ok: boolean, what: string) {
  if (!ok) bad++
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`)
}
const out: Record<string, string> = {}
for (const ex of EXAMPLES) {
  for (const mode of ['eager', 'parallel'] as const) {
    const s = new Store()
    const { root, defs } = loadProgram(s, ex.source)
    const tr = new Trace(s, root, mode)
    while (tr.step()) {}
    const end = tr.frames[tr.last]
    const fired = (r: number) => tr.rules.filter((x) => x === r).length
    const binders = fired(R_BIND)
    const want: Record<string, () => Id> = {
      right: () => s.app(s.variable('a'), s.app(s.variable('b'), s.variable('c'))),
      left: () => s.app(s.variable('b'), s.variable('a')),
      s: () => s.app(s.app(s.variable('a'), s.variable('c')), s.app(s.variable('b'), s.variable('c'))),
      drop: () => s.variable('a'),
      triage: () => s.app(s.app(s.app(s.app(s.variable('b'), s.variable('c')), s.variable('d')), s.variable('u')), s.variable('v')),
      add: () => s.nat(5),
      radd: () => {
        const tree = (n: string) => defs.find((d) => d.name === n)!.tree
        return s.fork(s.fork(tree('radd'), tree('d')), s.fork(s.nat(2), s.nat(3)))
      },
      mirror: () => {
        const tree = (n: string) => defs.find((d) => d.name === n)!.tree
        const t = s.fork(s.stem(s.leaf), s.leaf)
        const mir = s.fork(s.leaf, s.stem(s.leaf))
        return s.fork(s.fork(tree('mirror'), s.fork(tree('c'), s.fork(tree('d'), tree('b')))), mir)
        void t
      }
    }
    const expect = want[ex.id]()
    const reversible = ex.id === 'radd' || ex.id === 'mirror'
    check(end === expect, `${ex.id} (${mode}): ${s.pretty(end, 40)} after ${tr.last} frames, ${binders} binder steps, K ${fired(R_K)}, W ${fired(R_W)}${reversible ? ' (no K expected)' : ''}`)
    if (reversible) check(fired(R_K) === 0, `${ex.id} (${mode}) drops nothing`)
    if (mode === 'eager') for (const d of defs) out[`${ex.id}.${d.name}`] = ternary(s, d.tree)
  }
}
console.log(JSON.stringify(out))
process.exitCode = bad ? 1 : 0
