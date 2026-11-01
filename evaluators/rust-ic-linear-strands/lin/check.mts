// Checks the linear net (rules.ts, compile.ts) on the abstract net: it gives what core.ts's interpreter
// gives (or gets stuck where it does), and flipped after any round it runs back to the net exactly as
// loaded. `npx tsx evaluators/rust-ic-linear-strands/lin/check.mts [quick]`.
import { L, S, F, eq, show, run, regs, registersOf, TooLong, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal, progText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { LIBRARY, PRESETS, libTree } from '../../../website/src/lib/evaluators/twoway/library.ts'
import { load, result } from './compile.ts'
import { RULES, ARITY } from './rules.ts'

const quick = process.argv[2] === 'quick'
let failures = 0
const fail = (msg: string) => {
  failures++
  if (failures < 30) console.log('  FAIL', msg)
}
const timed = (name: string, f: () => string | void) => {
  const t0 = performance.now()
  const note = f()
  console.log(`${name.padEnd(40)} ${(performance.now() - t0).toFixed(0).padStart(6)} ms  ${note ?? ''}`)
}

const memo = new Map<number, V[]>()
function trees(n: number): V[] {
  if (memo.has(n)) return memo.get(n)!
  const out: V[] = []
  if (n === 1) out.push(L())
  if (n >= 2) for (const c of trees(n - 1)) out.push(S(c))
  for (let i = 1; i <= n - 2; i++) for (const l of trees(i)) for (const r of trees(n - 1 - i)) out.push(F(l, r))
  memo.set(n, out)
  return out
}
const upTo = (n: number) => Array.from({ length: n }, (_, i) => trees(i + 1)).flat()
const hasEval = (p: V) => progText(p, false, true).includes('eval')

let flips = 0
function check(p: V, x: V, what: string, sample: 'all' | 'some' = 'all'): { meetings: number; rounds: number; agents: number } | null {
  let want: V | null
  try {
    want = run(p, x)
  } catch (e) {
    if (e instanceof TooLong) return null
    throw e
  }
  const { net: start } = load(p, x)
  const loaded = start.canon()
  const net = start.clone()
  if (!net.runAll(3_000_000)) return null
  const got = result(net)
  if (!want) {
    if (got) fail(`${what} on ${show(x)}: the interpreter gets stuck, the net gives ${show(got.val)}`)
    return null
  }
  if (!got || !got.fwd || !eq(got.val, want)) {
    fail(`${what} on ${show(x)}: want ${show(want)}, got ${got ? show(got.val) : `nothing (${net.stuck})`}`)
    return null
  }
  const total = net.rounds
  const ks = sample === 'all' || total < 40 ? Array.from({ length: total + 1 }, (_, k) => k) : [0, 1, 2, 3, total >> 2, total >> 1, total - 1, total]
  for (const k of ks) {
    const n = start.clone()
    for (let i = 0; i < k; i++) n.round()
    n.flip()
    n.runAll(3_000_000)
    const back = result(n)
    if (!back || back.fwd || !eq(back.val, x)) {
      fail(`${what} on ${show(x)}: flipped after round ${k}, it does not come back (${back ? show(back.val) : n.stuck})`)
      break
    }
    n.flip()
    if (n.canon() !== loaded) {
      fail(`${what} on ${show(x)}: flipped after round ${k} and back at the start, the net is not as loaded`)
      break
    }
    flips++
  }
  return { meetings: net.meetings, rounds: net.rounds, agents: start.ag.size }
}

console.log(`${RULES.length} rules, ${Object.keys(ARITY).length} kinds of agent`)

timed('presets', () => {
  const out: string[] = []
  for (const pr of PRESETS) {
    if (pr.prog.includes('eval')) continue
    const r = check(parseProg(pr.prog), parseVal(pr.input), pr.name, pr.name === 'addw' ? 'some' : 'all')
    if (r) out.push(`\n    ${pr.name.padEnd(10)} ${String(r.meetings).padStart(6)} meetings in ${String(r.rounds).padStart(5)} rounds, ${r.agents} agents loaded`)
  }
  return out.join('')
})

timed('every tree as a program, on small trees', () => {
  const progs = upTo(quick ? 6 : 7).filter((p) => !hasEval(p))
  const xs = upTo(quick ? 4 : 5)
  let n = 0
  for (const p of progs) for (const x of xs) n += check(p, x, progText(p, false)) ? 1 : 0
  return `${progs.length} × ${xs.length}: ${n} checked`
})

let seed = 4242
const rand = () => ((seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff)
function randomProg(depth: number, inFix: boolean): V {
  const atoms = ['not', 'swap', 'cross', 'sink', 'rot', 'stem', 'unit', ...(inFix ? ['self', 'self'] : [])]
  const r = rand()
  if (depth <= 0 || r < 0.3) return r < 0.05 ? L() : parseProg(atoms[Math.floor(rand() * atoms.length)])
  const sub = (fix = inFix) => randomProg(depth - 1, fix)
  const k = Math.floor(rand() * 6)
  if (k === 0) return F(L(), sub())
  if (k === 1) return F(S(sub()), sub())
  if (k === 2) return F(F(sub(), sub()), sub())
  if (k === 3) return S(F(F(sub(), sub()), sub()))
  if (k === 4) return S(F(L(), sub(true)))
  return F(S(sub()), sub())
}
timed('random programs on random trees', () => {
  const n = quick ? 400 : 2000
  let ok = 0
  const xs = upTo(6)
  for (let i = 0; i < n; i++) ok += check(randomProg(4, false), xs[Math.floor(rand() * xs.length)], 'random', i < 300 ? 'all' : 'some') ? 1 : 0
  return `${n}: ${ok} checked`
})

timed('the library on small trees', () => {
  const xs = upTo(quick ? 4 : 5)
  let n = 0
  for (const d of LIBRARY) {
    if (d.part) continue
    for (const x of xs) n += check(libTree.get(d.name)!, x, d.name, d.name === 'addw' || d.name === 'carries' ? 'some' : 'all') ? 1 : 0
  }
  return `${n} runs`
})

timed('addw, every sum of 2-bit words', () => {
  for (let a = 0; a < 4; a++)
    for (let b = 0; b < 4; b++) {
      const bin = (v: number) => v.toString(2).padStart(2, '0')
      check(libTree.get('addw')!, regs([bin(a), bin(b)]), `addw ${a} + ${b}`, 'some')
    }
})

console.log(`flips run back to the start: ${flips}`)
console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
