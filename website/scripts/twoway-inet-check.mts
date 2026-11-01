// Checks the two-way programs' interaction net (src/lib/evaluators/twoway-inet): it gives what the whole-run
// interpreter gives (or gets stuck where it does), every step of the walk is undone by flip, fire, flip, a
// finished run flipped and run again comes back to its start, and undoing the history brings back the
// net exactly as loaded. `npx tsx scripts/twoway-inet-check.mts [quick]`.
import { L, S, F, eq, show, run, regs, registersOf, TooLong, type V } from '../src/lib/evaluators/twoway/core.ts'
import { parseProg, parseVal, progText } from '../src/lib/evaluators/twoway/text.ts'
import { LIBRARY, PRESETS, libTree } from '../src/lib/evaluators/twoway/library.ts'
import { measure } from '../src/lib/evaluators/twoway/machine.ts'
import { Net, load, runAll, finished, idOf, portOf, R, type Agent } from '../src/lib/evaluators/twoway-inet/net.ts'

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

const snapshot = (net: Net) => new Map(net.agents)
function sameAgents(a: Map<number, Agent>, b: Map<number, Agent>): boolean {
  if (a.size !== b.size) return false
  for (const [id, x] of a) {
    const y = b.get(id)
    if (!y || !sameAgent(x, y)) return false
  }
  return true
}
const sameAgent = (x: Agent, y: Agent) =>
  x.tag === y.tag && x.e === y.e && x.k === y.k && x.h === y.h && x.ports.length === y.ports.length && x.ports.every((p, i) => p === y.ports[i])

const WALK = new Set(['id', '† (reading)', '† (leaving)', 'fix (reading)', 'fix (leaving)', 'stem: an instruction', '; (into the first part)',
  '; (to the second part first)', '; (out)', 'counting stems', 'fork: reading its left', 'fix or map: reading its left', 'stem (back up)',
  '; (first part done)', '; (done)', 'counting stems back', 'fork (back up)', '† (into its body)', '; (into the second part)',
  'fix or map (back up)', '† (out of its body)', '; (second part done)', '; (back to the first part)', 'swap', 'move done (back to the program)'])
for (const n of ['swap', 'cross', 'sink', 'stem', 'unit', 'eval']) WALK.add(`${n} (turning to the value)`)

/** The walker or mover an event left, turned around in place (ports 0 and 1 trade). */
function turnAround(net: Net, id: number) {
  const ag = net.agents.get(id)!
  const tag = ({ Wd: 'Wu', Wu: 'Wd', Mv: 'Mvd', Mvd: 'Mv' } as Record<string, Agent['tag']>)[ag.tag]
  const c: Agent = { ...ag, tag, e: 1 - ag.e, ports: [ag.ports[1], ag.ports[0], ...ag.ports.slice(2)] }
  net.agents.set(id, c)
  for (const i of [0, 1]) {
    const p = c.ports[i]
    if (p < 0) continue
    const o = net.agents.get(idOf(p))!
    const oc = { ...o, ports: o.ports.slice() }
    oc.ports[portOf(p)] = R(id, i)
    net.agents.set(o.id, oc)
  }
}
/** For each step of the walk in the history: flip, fire, flip on what it made gives back exactly what it used. */
function checkWalkUndo(net: Net, what: string): number {
  let n = 0
  const end = net.pos
  const walker = (as: (Agent | undefined)[]) => as.find((a) => a && ['Wd', 'Wu', 'Mv', 'Mvd'].includes(a.tag))
  for (let i = end - 1; i >= 0; i--) {
    const ev = net.events[i]
    const w = WALK.has(ev.rule) && walker(ev.after)
    if (w) {
      const t = new Net()
      t.agents = new Map(net.agents)
      t.next = net.next
      turnAround(t, w.id)
      const b = t.agents.get(w.id)!
      const p = t.partner(b)
      if (!p || !t.fire(b, p)) fail(`${what}: ${ev.rule} at event ${i}: flipped, it cannot fire`)
      else {
        const back = walker(t.events[0].after)
        if (back) turnAround(t, back.id)
        const ok = ev.ids.every((id, j) => {
          const want = ev.before[j]
          const got = t.agents.get(id)
          return want ? !!got && sameAgent(want, got) && want.m === got.m && want.keep.join() === got.keep.join() : !got
        })
        if (!ok) fail(`${what}: ${ev.rule} at event ${i}: flip, fire, flip does not undo it`)
        n++
      }
    }
    net.undo()
  }
  net.seek(end)
  walkChecked += n
  return n
}
let walkChecked = 0

function want(p: V, x: V): V | null | 'long' {
  try {
    return run(p, x, true, 100_000)
  } catch (e) {
    if (e instanceof TooLong) return 'long'
    throw e
  }
}

/** Runs p on x on the net; checks it against the interpreter, the round trip and the history. */
function check(p: V, x: V, what: string, deep = false): { events: number; rounds: number } | null {
  const w = want(p, x)
  if (w === 'long') return null
  const net = load(p, x)
  const start = snapshot(net)
  const rounds = runAll(net, 300_000)
  if (rounds < 0) return null
  const fin = finished(net)
  if (w === null) {
    if (fin) fail(`${what} on ${show(x)}: the interpreter is stuck, the net gives ${show(fin.val)}`)
    return { events: net.pos, rounds }
  }
  if (!fin) return fail(`${what} on ${show(x)}: the net is stuck, the interpreter gives ${show(w)}`), null
  if (!eq(fin.val, w)) return fail(`${what} on ${show(x)}: the net gives ${show(fin.val)}, the interpreter ${show(w)}`), null
  if (!eq(fin.prog, p)) fail(`${what} on ${show(x)}: the program came back as ${show(fin.prog)}`)
  const events = net.pos
  if (deep && events < 20_000) checkWalkUndo(net, what)
  // flipped, it runs back to the start; flipped again it is the start
  net.flip()
  if (runAll(net, 2 * events + 1000) < 0) return fail(`${what} on ${show(x)}: running back does not end`), null
  const back = finished(net)
  if (!back || !eq(back.val, x) || !eq(back.prog, p) || back.fwd) fail(`${what} on ${show(x)}: run back, it ends with ${back ? show(back.val) : 'stuck'}`)
  net.flip()
  const top = [...net.agents.values()].find((a) => a.tag === 'Wd')
  if (!top || net.agents.size !== start.size) fail(`${what} on ${show(x)}: flipped twice, the net is not as loaded (${net.agents.size} agents, ${start.size} loaded)`)
  // the history undone brings back the net as loaded
  net.seek(0)
  if (!sameAgents(snapshot(net), start)) fail(`${what} on ${show(x)}: undoing every event does not give the net as loaded`)
  return { events, rounds }
}

timed('presets', () => {
  const out: string[] = []
  for (const pr of PRESETS) {
    if (pr.name === 'addw') continue
    const p = parseProg(pr.prog)
    const x = parseVal(pr.input)
    const r = check(p, x, pr.name, true)
    const m = measure(p, x)
    if (r) out.push(`\n    ${pr.name.padEnd(14)} ${String(r.events).padStart(7)} events in ${String(r.rounds).padStart(5)} rounds (the counter machine: ${m.rounds} rounds)`)
  }
  return out.join('')
})

timed('every tree as a program, on small trees', () => {
  const progs = upTo(quick ? 6 : 7)
  const xs = upTo(quick ? 4 : 5)
  let n = 0
  for (const p of progs) for (const x of xs) n += check(p, x, progText(p, false)) ? 1 : 0
  return `${progs.length} × ${xs.length}: ${n} checked`
})

let seed = 4242
const rand = () => ((seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff)
function randomProg(depth: number, inFix: boolean): V {
  const atoms = ['not', 'swap', 'cross', 'sink', 'rot', 'stem', 'unit', 'eval', ...(inFix ? ['self', 'self'] : [])]
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
  for (let i = 0; i < n; i++) ok += check(randomProg(4, false), xs[Math.floor(rand() * xs.length)], 'random', i < 200) ? 1 : 0
  return `${n}: ${ok} checked`
})

timed('the library on small trees', () => {
  const xs = upTo(quick ? 4 : 5)
  let n = 0
  // addw (and carries, a piece of it) take millions of interactions: addw is checked on its own below
  for (const d of LIBRARY) {
    if (d.part || d.name === 'addw' || d.name === 'carries') continue
    for (const x of xs) n += check(libTree.get(d.name)!, x, d.name) ? 1 : 0
  }
  return `${n} runs`
})

timed('addw without history, every 1-bit sum', () => {
  let n = 0
  for (const w of [1])
    for (let a = 0; a < 1 << w; a++)
      for (let b = 0; b < 1 << w; b++) {
        const bin = (v: number) => v.toString(2).padStart(w, '0')
        const x = regs([bin(a), bin(b)])
        const net = load(libTree.get('addw')!, x)
        net.history = false
        runAll(net, 5_000_000)
        const fin = finished(net)
        const r = fin && registersOf(fin.val)
        if (!r || r[0] !== bin(a) || r[1] !== bin((a + b) % (1 << w))) fail(`addw ${w} bits: ${a} + ${b} gave ${r}`)
        n += net.pos
      }
  return `${n} events`
})

console.log(`walk steps undone by flip, fire, flip: ${walkChecked}`)
console.log(failures ? `${failures} failures` : 'all good')
process.exit(failures ? 1 : 0)
