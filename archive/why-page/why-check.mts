// Checks everything the /why/ page says disp does: runs each program on the browser's engine (the
// WASM evaluator and the real elaborator, reading the repo's lib/) and compares verdicts and recorded
// step counts with lib/why/programs.ts. `npm run why-check [-- print]` prints the numbers to record.
import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { DispRunner } from '../src/lib/disp/runner.ts'
import * as W from '../src/lib/why/programs.ts'
import { SPECS, search, NO_IDEA, proposerFrom, ruledOut, size } from '../src/lib/why/search.ts'

const bytes = readFileSync(new URL('../static/rust_eager.wasm', import.meta.url))
const fresh = () => new DispRunner(bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength))
const PATH = fileURLToPath(new URL('../../lib/tests/__why_check.disp', import.meta.url))
const print = process.argv.includes('print')
let failures = 0
const fail = (m: string) => { failures++; console.log('  FAIL', m) }
const timed = (name: string, f: () => void) => {
  const t0 = performance.now()
  f()
  console.log(`${name.padEnd(30)} ${(performance.now() - t0).toFixed(0).padStart(7)} ms`)
}

let r = fresh()
const run = (src: string) => r.run(src, PATH, false, () => {})
const passes = (name: string, src: string) => {
  const o = run(src)
  if (o.error) fail(`${name}: ${o.error}`)
  for (const t of o.tests) if (!t.pass) fail(`${name}: test on line ${t.line} gave ${t.lhs}, wanted ${t.rhs}`)
  if (o.tests.length === 0) fail(`${name}: no tests ran`)
}
const value = (ctx: string, expr: string) => {
  const o = r.evalExpr(ctx, expr, () => {}, PATH)
  if (o.error) fail(`${expr}: ${o.error}`)
  return o.valueHint ?? o.value ?? ''
}

timed('scene 3 gate', () => {
  const ctx = W.CONTEXT + W.DOUBLING
  for (const c of W.CANDIDATES) {
    const v = value(ctx, `by_search doubles (${c.code}) ${W.GATE_BUDGET}`)
    const caught = v.includes('Refuted')
    const wantCaught = c.fails && c.id !== 'cheat'
    if (caught !== wantCaught) fail(`gate on ${c.id}: ${v}`)
  }
  passes('cheat is wrong', ctx + `test by_search doubles cheat 2000 = refuted 4\n`)
})
timed('scene 4 shape_of', () => passes('shape_of', W.CONTEXT + W.SHAPE_OF))
timed('scene 5 belt', () => {
  passes('belt code', W.CONTEXT + W.BELT_CODE)
  const ctx = W.CONTEXT + W.BELT_DEFS
  const o = run(ctx + `rows := Trees.as_items.bounded(${W.BELT_LENGTH}).s_take(${W.BELT_LENGTH}).s_items\nverd := rows.map(nat_fn sneaky)\n`)
  if (o.error) fail(o.error)
  type R = 0 | [R] | [R, R]
  const list = (x: R) => { const a: R[] = []; while (x !== 0) { a.push(x[0]); x = (x as [R, R])[1] } return a }
  const rows = list(r.rawTree({ name: 'rows' }, 100000) as R)
  const verd = list(r.rawTree({ name: 'verd' }, 100000) as R)
  if (rows.length !== W.BELT_LENGTH) fail(`belt holds ${rows.length} trees`)
  const firstFail = verd.findIndex((v) => v !== 0)
  // the page reads a budget as "how many trees the belt has shown": a budget of k must see the first k trees
  const see = (k: number) => value(ctx, `by_search nat_fn sneaky ${k}`).includes('Refuted')
  if (see(firstFail) || !see(firstFail + 1)) fail(`a budget does not count trees: first failure at ${firstFail}`)
  console.log(`  sneaky fails at tree ${firstFail} of the belt`)
})
timed('scene 6 covers', () => passes('covers', W.CONTEXT + W.COVER_CODE))
timed('scene 6 induction', () => passes('induction', W.CONFIRMER_CONTEXT + W.INDUCTION_CODE))
timed('scene 7 same', () => {
  r = fresh()
  const ctx = W.CONTEXT + W.SAME_DEFS
  if (value(ctx, 'same_tree') === 'false' || !run(ctx + 'test same_tree = true\ntest diff_tree = false\n').ok) fail('tree_eq on the doubling programs')
})

timed('recorded step counts', () => {
  const ctx = W.CONTEXT + W.DOUBLING
  const cost = (code: string, n: number) => {
    r = fresh()
    r.evalExpr(ctx, `${n}`, () => {}, PATH)
    const base = r.evalExpr(ctx, `${n}`, () => {}, PATH).steps
    const o = r.evalExpr(ctx, `(${code}) ${n}`, () => {}, PATH)
    if (o.error) fail(o.error)
    return o.steps - base
  }
  const gate = W.CANDIDATES.map((c) => [c.id, cost(c.code, W.GATE_N)] as const)
  const table = [10, 30, 100, 300, 600].map((n) => ({ n, by_twos: cost('by_twos', n), by_add2: cost('by_add2', n), by_self: cost('by_self', n) }))
  if (print) console.log(JSON.stringify({ gate, table }))
  for (const [id, steps] of gate) if (W.CANDIDATES.find((c) => c.id === id)!.steps !== steps) fail(`steps of ${id}: ${steps}`)
  if (JSON.stringify(table) !== JSON.stringify(W.DOUBLING_STEPS)) fail('doubling table drifted')
})

timed('scene 8 toy search', () => {
  for (const s of SPECS) {
    const blind = search(s, NO_IDEA)
    if (!blind.found) { fail(`${s.name}: nothing found`); continue }
    const good = search(s, proposerFrom(blind.found.prog, 0.9))
    const liar = search(s, proposerFrom(blind.found.prog, 0.02))
    if (!good.found || !liar.found) fail(`${s.name}: a proposer lost the answer`)
    else if (!(good.nodes.length < blind.nodes.length && blind.nodes.length < liar.nodes.length)) fail(`${s.name}: order of proposers`)
    console.log(`  ${s.name}: blind ${blind.nodes.length}, good ${good.nodes.length}, liar ${liar.nodes.length}, size ${size(blind.found.prog)}, ruled out ${(ruledOut(blind.nodes, 14) * 100).toFixed(1)}% up to 14`)
  }
})

console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
