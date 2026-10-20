// Checks the two-way programs engine (src/lib/twoway): the step machine agrees with the whole-run
// interpreter, every move it makes is undone by flip, fire, flip, flip is its own inverse, a finished run
// flipped and run again comes back to exactly its start, text reads back to the same tree, and the
// library does what it says. `npm run twoway-check [-- quick]`.
import { L, S, F, eq, show, run, regs, registersOf, nat, flipProg, TooLong, type V } from '../src/lib/twoway/core.ts'
import { parseProg, parseVal, progText, valText } from '../src/lib/twoway/text.ts'
import { LIBRARY, PRESETS, libTree } from '../src/lib/twoway/library.ts'
import { start, fire, flip, unfire, movable, fireAll, same, done, measure, type Node, type Dot } from '../src/lib/twoway/machine.ts'

const quick = process.argv[2] === 'quick'
let failures = 0
const fail = (msg: string) => {
  failures++
  if (failures < 30) console.log('  FAIL', msg)
}
const timed = (name: string, f: () => string | void) => {
  const t0 = performance.now()
  const note = f()
  console.log(`${name.padEnd(34)} ${(performance.now() - t0).toFixed(0).padStart(6)} ms  ${note ?? ''}`)
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

let seed = 12345
const rand = () => ((seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff)
const bits = (n: number) => Array.from({ length: n }, () => (rand() < 0.5 ? '1' : '0')).join('')

/** Runs the machine a round at a time, checking at each node that fires that flip, fire, flip undoes the
 * move and that flip is its own inverse; then flips the finished run, runs it again, and flips it back,
 * which must give exactly the start. Returns the result, or null where it got stuck. */
function roundTrip(p: V, x: V, maxRounds = 20_000): { y: V | null; rounds: number; long?: boolean } {
  const s0 = start(p, x)
  const forward = (from: Node): { t: Node | null; r: number; long: boolean } => {
    let t = from
    let r = 0
    while (!done(t)) {
      if (r > maxRounds) return { t: null, r, long: true }
      const ms = movable(t)
      if (!ms.length) return { t: null, r, long: false }
      for (const m of ms) {
        const b = unfire(fire(m)!)
        if (!b || !same(b, m)) fail(`flip, fire, flip does not undo a move at ${show(p)} on ${show(x)}`)
        if (!same(flip(flip(m)), m)) fail(`flip is not its own inverse at ${show(p)} on ${show(x)}`)
      }
      t = fireAll(t, new Set(ms.map((m) => m.id)))
      r++
    }
    return { t, r, long: false }
  }
  const a = forward(s0)
  if (!a.t) return { y: null, rounds: a.r, long: a.long }
  if (done(s0)) return { y: (a.t as Dot).x, rounds: 0 } // a held program has already run
  const b = forward(flip(a.t))
  if (!b.t || !same(flip(b.t), s0)) fail(`flipped and run again, ${show(p)} on ${show(x)} does not come back to its start`)
  return { y: (a.t as Dot).x, rounds: a.r }
}

function agree(p: V, x: V, what: string) {
  let want: V | null
  try {
    want = run(p, x, true, 200_000)
  } catch (e) {
    if (e instanceof TooLong) return 'long'
    throw e
  }
  const got = roundTrip(p, x)
  if (got.long) return 'long'
  if (want === null ? got.y !== null : got.y === null || !eq(got.y, want))
    fail(`${what} on ${show(x)}: whole run ${want ? show(want) : 'stuck'}, steps ${got.y ? show(got.y) : 'stuck'}`)
  if (want) {
    const back = run(p, want, false, 200_000)
    if (!back || !eq(back, x)) fail(`${what} backward on its result does not give ${show(x)}`)
  }
  return want ? 'ok' : 'stuck'
}

timed('text reads back to the same tree', () => {
  for (const d of LIBRARY) {
    const t = libTree.get(d.name)!
    for (const names of [true, false]) {
      const back = parseProg(progText(t, names, true))
      if (!eq(back, t)) fail(`${d.name}: ${progText(t, names, true)}`)
    }
  }
  let n = 0
  for (const t of upTo(quick ? 8 : 10)) {
    n++
    const s = progText(t, false)
    let back: V
    try {
      back = parseProg(s)
    } catch (e) {
      fail(`${show(t)} → ${s}: ${(e as Error).message}`)
      continue
    }
    if (!eq(back, t)) fail(`${show(t)} → ${s} → ${show(back)}`)
  }
  for (const s of ['△', '△△', '(△, △△)', 'T', '3', '0b1011', '<0b10, 0b01>', '[notw]']) {
    const v = parseVal(s)
    const t = valText(v, s.startsWith('<') ? 'regs' : s.startsWith('0b') ? 'bits' : s === '3' ? 'nat' : s === 'T' ? 'bool' : 'tree')
    if (!eq(parseVal(t), v)) fail(`value ${s} → ${t}`)
  }
  return `${n} trees as programs`
})

timed('flip is its own inverse on every tree', () => {
  let n = 0
  for (const t of upTo(quick ? 9 : 11)) {
    n++
    if (!eq(flipProg(flipProg(t)), t)) fail(`flip twice changes ${show(t)}`)
  }
  return `${n} trees`
})

timed('every tree as a program, on small trees', () => {
  const progs = upTo(quick ? 6 : 7)
  const xs = upTo(quick ? 4 : 5)
  const tally = { ok: 0, stuck: 0, long: 0 }
  for (const p of progs) for (const x of xs) tally[agree(p, x, progText(p, false)) as keyof typeof tally]++
  return `${progs.length} × ${xs.length}: ${tally.ok} ran, ${tally.stuck} stuck, ${tally.long} too long`
})

/** A random program from the forms, with self only inside a fix. */
function randomProg(depth: number, inFix: boolean): V {
  const atoms = ['not', 'swap', 'rot', 'stem', 'unit', 'eval', ...(inFix ? ['self', 'self'] : [])]
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
  const tally = { ok: 0, stuck: 0, long: 0 }
  const n = quick ? 1500 : 6000
  for (let i = 0; i < n; i++) {
    const p = randomProg(4, false)
    const xs = upTo(6)
    const x = xs[Math.floor(rand() * xs.length)]
    tally[agree(p, x, progText(p, false)) as keyof typeof tally]++
  }
  return `${n}: ${tally.ok} ran, ${tally.stuck} stuck, ${tally.long} too long`
})

timed('the library on small trees', () => {
  const xs = upTo(quick ? 6 : 8)
  let n = 0
  for (const d of LIBRARY) {
    if (d.part) continue
    const p = libTree.get(d.name)!
    for (const x of xs) {
      agree(p, x, d.name)
      n++
    }
  }
  return `${n} runs`
})

const B = (v: number) => (v ? S(L()) : L())
const want = (name: string, x: V, y: V) => {
  const got = run(libTree.get(name)!, x)
  if (!got || !eq(got, y)) fail(`${name} ${show(x)} gave ${got ? show(got) : 'stuck'}, not ${show(y)}`)
}
timed('what the library says it does', () => {
  for (const a of [0, 1]) want('not', B(a), B(1 - a))
  for (const a of [0, 1]) for (const b of [0, 1]) want('cnot', F(B(a), B(b)), F(B(a), B(a ^ b)))
  for (const a of [0, 1]) for (const b of [0, 1]) for (const c of [0, 1]) want('toffoli', F(F(B(a), B(b)), B(c)), F(F(B(a), B(b)), B(c ^ (a & b))))
  for (let a = 0; a < 6; a++) for (let b = 0; b < 6; b++) want('add', F(nat(a), nat(b)), F(nat(a), nat(a + b)))
  for (const x of upTo(6)) {
    want('copy', x, F(x, x))
    want('dup', F(x, L()), F(x, x))
  }
  const mirror = (v: V): V => (v.tag === 'fork' ? F(mirror(v.r), mirror(v.l)) : v.tag === 'stem' ? S(mirror(v.c)) : v)
  for (const x of upTo(7)) want('mirror', x, mirror(x))
  for (const n of [1, 2, 4, 8, 16]) {
    const a = bits(n)
    const b = bits(n)
    want('notw', regs([a]), regs([[...a].map((c) => (c === '1' ? '0' : '1')).join('')]))
    want('cnotw', regs([a, b]), regs([a, [...b].map((c, i) => String(+c ^ +a[i])).join('')]))
  }
})

timed('addw: every sum of 1, 2 and 4 bits', () => {
  let n = 0
  for (const w of [1, 2, 4])
    for (let a = 0; a < 1 << w; a++)
      for (let b = 0; b < 1 << w; b++) {
        const bin = (v: number) => v.toString(2).padStart(w, '0')
        const x = regs([bin(a), bin(b)])
        const y = run(libTree.get('addw')!, x)
        const r = y && registersOf(y)
        if (!r || r[0] !== bin(a) || r[1] !== bin((a + b) % (1 << w))) fail(`addw ${w} bits: ${a} + ${b} gave ${r}`)
        const back = y && run(libTree.get('addw')!, y, false)
        if (!back || !eq(back, x)) fail(`addw ${w} bits: ${a} + ${b} does not go back`)
        n++
      }
  return `${n} sums`
})

timed('addw through the machine, every 2-bit sum', () => {
  for (let a = 0; a < 4; a++)
    for (let b = 0; b < 4; b++) {
      const bin = (v: number) => v.toString(2).padStart(2, '0')
      const r = roundTrip(libTree.get('addw')!, regs([bin(a), bin(b)]))
      const got = r.y && registersOf(r.y)
      if (!got || got[1] !== bin((a + b) % 4)) fail(`machine addw ${a} + ${b} gave ${got}`)
    }
})

timed('presets, forward and back', () => {
  const out: string[] = []
  for (const pr of PRESETS) {
    const p = parseProg(pr.prog)
    const x = parseVal(pr.input)
    const r = roundTrip(p, x)
    if (!r.y) fail(`preset ${pr.name} is stuck`)
    else out.push(`${pr.name} ${r.rounds}`)
  }
  return out.join(', ')
})

timed('rounds as words grow', () => {
  const rows: string[] = []
  for (const name of ['notw', 'cnotw', 'mirror', 'addw']) {
    const cells: string[] = []
    for (const n of quick ? [4, 8, 16] : [4, 8, 16, 32, 64]) {
      const x = name === 'notw' || name === 'mirror' ? regs([bits(n)]) : regs([bits(n), bits(n)])
      const m = measure(libTree.get(name)!, x)
      const ok = m.y && eq(m.y, run(libTree.get(name)!, x)!)
      cells.push(`${n}:${m.rounds}r/${m.moves}m${ok ? '' : ' WRONG'}`)
      if (!ok) fail(`${name} ${n} bits through the machine`)
    }
    rows.push(`\n    ${name.padEnd(7)} ${cells.join('  ')}`)
  }
  return rows.join('')
})

console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
