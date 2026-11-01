// Checks the compiled two-way net (src/lib/evaluators/twoway-compiled): it gives what the whole-run interpreter
// gives (or gets stuck where it does), and flipped after any round it runs back to the net exactly as
// loaded, every copy of a fix's body rolled back up. Then it counts meetings and rounds against the
// walker net (src/lib/evaluators/twoway-inet) and the counter machine. `npx tsx scripts/twoway-compiled-check.mts [quick]`.
import { L, S, F, eq, show, run, regs, registersOf, TooLong, type V } from '../src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal, progText } from '../src/lib/evaluators/twoway/text.ts'
import { LIBRARY, PRESETS, libTree } from '../src/lib/evaluators/twoway/library.ts'
import { measure } from '../src/lib/evaluators/twoway/machine.ts'
import { load as loadWalker, runAll as runWalker } from '../src/lib/evaluators/twoway-inet/net.ts'
import { Net, load, idOf, type Agent } from '../src/lib/evaluators/twoway-compiled/net.ts'

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

/** The net as text from the top, ids left out. */
function canon(net: Net): string {
  const top = [...net.ag.values()].find((a) => a.tag === 'Top')!
  const out: string[] = []
  const walk = (a: Agent, from: number) => {
    out.push(`${a.tag}${a.p}${a.e}${a.f}${a.s}${a.n}(`)
    a.ports.forEach((r, i) => {
      if (i === from) return
      if (r < 0) out.push('-')
      else walk(net.ag.get(idOf(r))!, r % 64)
    })
    out.push(')')
  }
  walk(top, -1)
  return out.join('') + `#${net.ag.size}`
}
function clone(net: Net): Net {
  const c = new Net(net.prog)
  for (const [id, a] of net.ag) c.ag.set(id, { ...a, ports: a.ports.slice() })
  c.next = net.next
  return c
}

let flipsChecked = 0
/** Runs p on x; checks the answer, and (unless `light`) that a flip after each round runs back to the start. */
function check(p: V, x: V, what: string, flips: 'all' | 'some' | 'none' = 'all'): { meetings: number; rounds: number } | null {
  let want: V | null
  try {
    want = run(p, x)
  } catch (e) {
    if (e instanceof TooLong) return null
    throw e
  }
  const start = load(p, x)
  const loaded = canon(start)
  const net = clone(start)
  if (!net.runAll(2_000_000)) return null
  const got = net.result()
  if (!want) {
    if (got) fail(`${what} on ${show(x)}: the interpreter gets stuck, the net gives ${show(got.val)}`)
    return null
  }
  if (!got || !got.fwd || !eq(got.val, want)) {
    fail(`${what} on ${show(x)}: want ${show(want)}, got ${got ? show(got.val) : `nothing (${net.stuck})`}`)
    return null
  }
  const total = net.rounds
  if (flips !== 'none') {
    const ks = flips === 'all' || total < 40 ? Array.from({ length: total + 1 }, (_, k) => k) : [0, 1, 2, 3, total >> 2, total >> 1, total - 1, total]
    for (const k of ks) {
      const n = clone(start)
      for (let i = 0; i < k; i++) n.round()
      n.flip()
      n.runAll(2_000_000)
      const back = n.result()
      if (!back || back.fwd || !eq(back.val, x)) {
        fail(`${what} on ${show(x)}: flipped after round ${k}, it does not come back (${back ? show(back.val) : n.stuck})`)
        break
      }
      n.flip()
      if (canon(n) !== loaded) {
        fail(`${what} on ${show(x)}: flipped after round ${k} and back at the start, the net is not as loaded`)
        break
      }
      flipsChecked++
    }
  }
  return { meetings: net.meetings, rounds: net.rounds }
}

timed('presets', () => {
  const out: string[] = []
  for (const pr of PRESETS) {
    if (pr.prog.includes('eval')) continue
    const p = parseProg(pr.prog)
    const x = parseVal(pr.input)
    const r = check(p, x, pr.name, pr.name === 'addw' ? 'some' : 'all')
    if (!r) continue
    const m = measure(p, x)
    let walker = '(skipped)'
    if (pr.name !== 'addw') {
      const w = loadWalker(p, x)
      w.history = false
      const wr = runWalker(w, 3_000_000)
      walker = `${String(w.pos).padStart(7)} in ${String(wr).padStart(5)}`
    }
    out.push(`\n    ${pr.name.padEnd(10)} compiled ${String(r.meetings).padStart(6)} meetings in ${String(r.rounds).padStart(4)} rounds · walker net ${walker} · counter machine ${m.rounds} rounds`)
  }
  return out.join('')
})

timed('every tree as a program, on small trees', () => {
  const progs = upTo(quick ? 6 : 7)
  const xs = upTo(quick ? 4 : 5)
  let n = 0
  let skipped = 0
  for (const p of progs)
    for (const x of xs) {
      if (progText(p, false).includes('eval')) {
        skipped++
        continue
      }
      n += check(p, x, progText(p, false)) ? 1 : 0
    }
  return `${progs.length} × ${xs.length}: ${n} checked, ${skipped} with eval skipped`
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

timed('addw, every sum of 2-bit and 4-bit words', () => {
  let meetings = 0
  let rounds = 0
  for (const w of quick ? [2] : [2, 4])
    for (let a = 0; a < 1 << w; a++)
      for (let b = 0; b < 1 << w; b++) {
        const bin = (v: number) => v.toString(2).padStart(w, '0')
        const net = load(libTree.get('addw')!, regs([bin(a), bin(b)]))
        net.runAll()
        const r = net.result()
        const regsOut = r && registersOf(r.val)
        if (!regsOut || regsOut[0] !== bin(a) || regsOut[1] !== bin((a + b) % (1 << w))) fail(`addw ${w} bits: ${a} + ${b} gave ${regsOut} (${net.stuck})`)
        if (w === 4) {
          meetings = Math.max(meetings, net.meetings)
          rounds = Math.max(rounds, net.rounds)
        }
      }
  return quick ? '' : `4 bits: at most ${meetings} meetings in ${rounds} rounds`
})

console.log(`flips run back to the start: ${flipsChecked}`)
console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
