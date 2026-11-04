// The playground inspector's reduction trace, checked against the teaching
// reducer in treecalc.ts: both implement the five rules, so they must agree.
import { Store, FORK } from '../src/lib/playground/store.ts'
import { Trace } from '../src/lib/playground/trace.ts'
import { layoutScene, diffTrees, keyPath, K_APP } from '../src/lib/playground/layout.ts'
import { bracketReplay } from '../src/lib/disp/bracket.ts'
import type { Cir } from '../../src/elab/cir.ts'
import { Text } from '@codemirror/state'
import { indexBuffer, paramHints } from '../src/lib/playground/editor-ext.ts'
import { DEFS, normalize, nat, type T } from '../src/lib/treecalc/treecalc.ts'

let failed = 0
const check = (what: string, ok: boolean) => {
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${what}`)
  if (!ok) failed++
}

const s = new Store()
const load = (t: T): number =>
  t.tag === 'leaf'
    ? s.leaf
    : t.tag === 'stem'
      ? s.stem(load(t.c))
      : t.tag === 'fork'
        ? s.fork(load(t.l), load(t.r))
        : t.tag === 'apply'
          ? s.app(load(t.f), load(t.x))
          : s.variable(t.name)
const ap = (...xs: number[]) => xs.reduce((f, x) => s.app(f, x))
const run = (root: number, mode: 'eager' | 'parallel' = 'eager') => {
  const tr = new Trace(s, root, mode)
  while (tr.step());
  return tr
}

const add = load(DEFS.add)
s.names.set(add, 'add')
const K = load(DEFS.K)
const not = load(DEFS.not)
s.names.set(not, 'not')

{
  const called = s.app(s.fork(s.leaf, s.fork(s.leaf, s.leaf)), s.variable('x'))
  check('a numeral being called reads as code when read as S and K', s.pretty(called, 20, true) === 'K(1)(x)' && s.pretty(called, 20, false) === '2(x)')
  const scene = layoutScene(s, [called], { open: new Set(), closed: new Set() }, { sk: true })
  check('...and is drawn as code too', scene.nodes.some((n) => n.comb === 'K'))
  const head = scene.nodes.findIndex((n) => n.comb === 'K')
  const band = scene.nodes.filter((n) => n.group === head).map((n) => n.key)
  check('a K band holds its own fork and leaf, not what it keeps', band.join() === 'r00,r000')
}
check('K x y = x', run(ap(K, s.variable('x'), s.variable('y'))).frames.at(-1) === s.variable('x'))
check('not true = false', run(ap(not, load(DEFS.true))).frames.at(-1) === load(DEFS.false))

const sum = run(ap(add, load(nat(2)), load(nat(3))))
check('add 2 3 = 5', sum.frames.at(-1) === load(nat(5)))
check('add 2 3 ended in normal form', sum.end === 'normal')
check('parallel agrees', run(ap(add, load(nat(2)), load(nat(3))), 'parallel').frames.at(-1) === load(nat(5)))
const expect = load(normalize({ tag: 'apply', f: { tag: 'apply', f: DEFS.add, x: nat(4) }, x: nat(1) }, 200000))
check('agrees with the teaching reducer', run(ap(add, load(nat(4)), load(nat(1)))).frames.at(-1) === expect)

const adds = sum.calls.filter((c) => c.name === 'add')
check(`recursion shows up as calls (${adds.length} of add)`, adds.length === 3)
check('every call closed', sum.calls.every((c) => c.end >= c.start && c.result >= 0))
check('nested calls go deeper', adds[1].depth > adds[0].depth)
check('outer call returns the sum', adds[0].result === load(nat(5)))

const stuck = run(ap(s.variable('f'), load(nat(1))))
check('a free head is stuck, not an error', stuck.end === 'stuck')

const big = new Trace(s, ap(add, load(nat(2)), load(nat(3))))
big.step()
const reached = big.fireAt(0, [])
check('firing a chosen application reduces inside it first', reached >= 1 && big.frames[reached] !== big.frames[0])

let tree = s.leaf
for (let i = 0; i < 5000; i++) tree = s.fork(tree, s.leaf)
const t0 = performance.now()
const scene = layoutScene(s, [tree], { open: new Set(), closed: new Set() }, { maxVisible: 1800 })
const ms = performance.now() - t0
check(`layout respects its budget (${scene.nodes.length} nodes, ${ms.toFixed(1)}ms)`, scene.nodes.length <= 1810 && scene.cut > 0)
check('layout is fast', ms < 30)

const d = diffTrees(s, s.fork(s.leaf, load(nat(3))), s.fork(s.stem(s.leaf), load(nat(3))))
check('diff finds the mismatched child', d.diff.has('r00') && d.diff.has('r10') && d.same.has(load(nat(3))))
check('fork tag is stable', s.tag[s.fork(s.leaf, s.leaf)] === FORK)

// ---- bracket abstraction -----------------------------------------------------
const v = (name: string): Cir => ({ tag: 'var', name })
const lam = (x: string, body: Cir): Cir => ({ tag: 'lam', x, body })
const capp = (f: Cir, ...xs: Cir[]): Cir => xs.reduce<Cir>((g, x) => ({ tag: 'app', f: g, x }), f)
const toStore = (e: Cir): number => {
  switch (e.tag) {
    case 'var':
      return s.variable(e.name)
    case 'I':
      return s.fork(s.fork(s.leaf, s.leaf), s.leaf)
    case 'K':
      return s.stem(s.leaf)
    case 'S':
      return s.fork(s.stem(s.fork(s.leaf, s.leaf)), s.leaf)
    case 'app':
      if (e.f.tag === 'app' && e.f.f.tag === 'S') return s.fork(s.stem(toStore(e.f.x)), toStore(e.x))
      if (e.f.tag === 'K') return s.fork(s.leaf, toStore(e.x))
      if (e.f.tag === 'S') return s.stem(s.stem(toStore(e.x)))
      return s.app(toStore(e.f), toStore(e.x))
    default:
      throw new Error('unexpected ' + e.tag)
  }
}
const lastFrame = (e: Cir) => {
  const r = bracketReplay(e)
  return { ...r, final: r.frames[r.frames.length - 1].term }
}
const swapSrc = lam('f', lam('x', lam('y', capp(v('f'), v('y'), v('x')))))
const swap = lastFrame(swapSrc)
check('the replay lands on what the compiler produces', !swap.mismatch)
const swapped = run(ap(toStore(swap.final), s.variable('g'), s.variable('p'), s.variable('q')))
check('a replayed function behaves like its source', swapped.frames[swapped.last] === ap(s.variable('g'), s.variable('q'), s.variable('p')))

const twice = lastFrame(lam('f', lam('x', capp(v('f'), capp(v('f'), v('x'))))))
const twiced = run(ap(toStore(twice.final), s.variable('g'), s.variable('p')))
check('nested uses of a binder route correctly', twiced.frames[twiced.last] === ap(s.variable('g'), ap(s.variable('g'), s.variable('p'))))

const identity = run(ap(toStore(lastFrame(lam('x', v('x'))).final), s.variable('p')))
check('I passes a symbol through', identity.frames[identity.last] === s.variable('p'))

let seed = 7
const rnd = (n: number) => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) % n
const NAMES = ['a', 'b', 'c', 'free']
const randomTerm = (depth: number): Cir => {
  const r = depth <= 0 ? rnd(2) : rnd(5)
  if (r <= 1) return v(NAMES[rnd(NAMES.length)])
  if (r === 2) return lam(NAMES[rnd(3)], randomTerm(depth - 1))
  return capp(randomTerm(depth - 1), randomTerm(depth - 1))
}
let bad = 0
for (let i = 0; i < 2000; i++) if (bracketReplay(randomTerm(7)).mismatch) bad++
check(`the replay matches the compiler on 2000 random terms (${bad} differ)`, bad === 0)

// binders as steps: the store's own rewrites land where the compiler's replay does, and running alongside changes nothing
/// a replay frame as the worker draws it: binders kept, K(x) held, I(x) and K(x, y) already applied
const held = (e: Cir): number => {
  switch (e.tag) {
    case 'lit':
      return e.t as unknown as number
    case 'var':
      return s.variable(e.name)
    case 'I':
      return s.i
    case 'K':
      return s.stem(s.leaf)
    case 'S':
      return s.fork(s.stem(s.fork(s.leaf, s.leaf)), s.leaf)
    case 'lam':
      return s.lam(e.x, held(e.body))
    case 'app':
      if (e.f.tag === 'app' && e.f.f.tag === 'S') return s.fork(s.stem(held(e.f.x)), held(e.x))
      if (e.f.tag === 'app' && e.f.f.tag === 'K') return held(e.f.x)
      if (e.f.tag === 'K') return s.heldK(held(e.x))
      if (e.f.tag === 'I') return held(e.x)
      if (e.f.tag === 'S') return s.stem(s.stem(held(e.x)))
      return s.app(held(e.f), held(e.x))
  }
}
const LITS = [s.i, s.stem(s.leaf), load(nat(2)), K, s.fork(s.stem(s.fork(s.leaf, s.leaf)), s.leaf)]
const randomLitTerm = (depth: number): Cir => {
  const r = depth <= 0 ? rnd(3) : rnd(6)
  if (r === 0) return { tag: 'lit', t: LITS[rnd(LITS.length)] as never }
  if (r <= 2) return v(NAMES[rnd(NAMES.length)])
  if (r === 3) return lam(NAMES[rnd(3)], randomLitTerm(depth - 1))
  return capp(randomLitTerm(depth - 1), randomLitTerm(depth - 1))
}
let off = 0
let runOff = 0
let tried = 0
for (let i = 0; i < 2000; i++) {
  const e = i % 2 ? randomTerm(7) : randomLitTerm(7)
  const r = bracketReplay(e)
  if (r.mismatch || r.skipped) continue
  tried++
  const start = held(r.frames[0].term)
  const want = held(r.frames[r.frames.length - 1].term)
  if (s.encode(s.folded(s.removeBinders(start))) !== s.encode(want)) {
    off++
    continue
  }
  if (i % 8) continue
  const syms = [s.variable('p'), s.variable('q')]
  const both = new Trace(s, ap(start, ...syms), i % 16 ? 'eager' : 'parallel')
  const after = new Trace(s, ap(want, ...syms))
  both.advance(400)
  after.advance(400)
  if (both.end && after.end && both.end !== 'limit' && s.encode(both.frames[both.last]) !== s.encode(after.frames[after.last])) runOff++
}
check(`binders as steps land on the replay's result (${off} of ${tried} random terms differ)`, off === 0)
check(`running binders alongside the run ends where removing them first does (${runOff} differ)`, runOff === 0)
const swapRun = new Trace(s, ap(held(swapSrc), s.variable('g'), s.variable('p'), s.variable('q')))
swapRun.advance(1000)
check('a function with its binders still on runs to the same symbols', swapRun.frames[swapRun.last] === ap(s.variable('g'), s.variable('q'), s.variable('p')))
const binderSteps = swapRun.rules.filter((r) => r === 6).length
check(`swap takes as many binder steps as the replay has rewrites (${binderSteps} vs ${swap.frames.length - 1})`, binderSteps === swap.frames.length - 1)

let huge: Cir = v('a')
for (let i = 0; i < 400; i++) huge = capp(huge, v('b'))
const t1 = performance.now()
const long = bracketReplay(lam('a', lam('b', huge)))
check(`a very long replay stops recording but still finishes (${(performance.now() - t1).toFixed(0)}ms)`, long.skipped > 0 && !long.mismatch && long.frames.length <= 1202 && long.frames[long.frames.length - 1].rule === 'skip')

// reading as S and K must keep every held application's real path, or clicking fires the wrong one
{
  const tr = run(ap(toStore(swap.final), s.variable('g'), s.variable('p'), s.variable('q')))
  let wrong = 0
  let combs = 0
  let apps = 0
  for (const frame of tr.frames) {
    const sc = layoutScene(s, [frame], { open: new Set(), closed: new Set() }, { sk: true, labels: false })
    for (const n of sc.nodes) {
      if (n.comb) combs++
      if (n.kind !== K_APP) continue
      apps++
      if (tr.subterm(frame, keyPath(n.key)) !== n.id) wrong++
    }
  }
  check(`read as S and K, applications keep their paths (${apps} applications, ${combs} combinators)`, wrong === 0 && apps > 0 && combs > 0)
}
const heldK = s.heldK(load(nat(2)))
check('a held K stays K, and encodes to the numeral it really is', s.readComb(heldK)?.op === 'K' && s.encode(heldK) === load(nat(3)))
check('S, K and I read as themselves', s.readComb(toStore({ tag: 'S' }))?.op === 'S' && s.readComb(toStore({ tag: 'K' }))?.op === 'K' && s.readComb(toStore({ tag: 'I' }))?.op === 'I')

{
  const P: Record<string, string[]> = { honest: ['source', 'target', 'fuel'] }
  const look = (n: string, recv?: string) => (recv && !/^[A-Z]/.test(recv) ? P[n]?.slice(1) : P[n])
  const names = (t: string) => paramHints(t, look).map((h) => `${h.name}@${t.slice(h.pos, h.pos + 3)}`).join(' ')
  check('hints name each comma-separated argument', names('x := honest(Trees.as_items, g(1, 2), fuel)') === 'source@Tre target@g(1')
  check('hints carry on through f(a)(b), and a string is an argument', names('y := honest(a)(b, "s, t")') === 'source@a)( target@b,  fuel@"s,')
  const nested = paramHints('w := honest(honest(a, b), c)', look).map((h) => h.pos)
  check('nested calls come out in position order', nested.length === 4 && nested.every((p, i) => i === 0 || p > nested[i - 1]))
  check('a brace call fills its named parameters and the chain goes on', names('v := honest{target := t}(a)(b)') === 'source@a)( fuel@b)')
  check('x.f(a) hints from the second parameter; comments get none', names('z := q.honest(b, c) // honest(a)') === 'target@b,  fuel@c) ')
}
{
  const idx = indexBuffer(Text.of(['record Sample :: {claim : Claim,', '  challenges : List(Pair(A, B))} -> Type', 'f := {s} => s.claim']))
  const [ctor, ...fields] = idx.defs
  check('a record declares its constructor and one accessor per field', ctor.name === 'Sample' && ctor.params?.join() === 'claim,challenges' && fields.map((f) => `${f.name}:${f.of}`).join() === 'claim:Type,challenges:Type,f:undefined')
  const knob = indexBuffer(Text.of(['record knob :: {level : Nat, step : Nat := 1, raw := t} -> Knob'])).defs
  check('record fields may have defaults', knob.map((d) => d.name).join() === 'knob,level,step,raw' && knob[2].sig === 'Knob -> Nat' && /1 unless given/.test(knob[2].doc ?? '') && knob[3].sig === 'Knob -> Tree' && knob[1].of === 'Knob')
  check('...across lines, with nested commas in a field type', fields[1].sig === 'Type -> List(Pair(A, B))' && ctor.endLine === 2)
}

process.exit(failed ? 1 : 0)
