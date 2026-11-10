// How folding choices read on real runs, measured without a browser: each policy lays out every frame the viewer
// rests on, and the survey counts what it draws (nodes, folds, folds named only by a size), how often the step about
// to fire acts on something folded, and how often a subtree opens or closes between frames.
//   npx tsx scripts/fold-survey.mts
import { Store } from '../src/lib/playground/store.ts'
import { Trace } from '../src/lib/playground/trace.ts'
import { layoutScene, K_POD, type LayoutOptions } from '../src/lib/playground/layout.ts'
import { bracketReplay } from '../src/lib/disp/bracket.ts'
import type { Cir } from '../../src/elab/cir.ts'
import { DEFS, nat, type T } from '../src/lib/treecalc/treecalc.ts'

const s = new Store()
const load = (t: T): number =>
  t.tag === 'leaf' ? s.leaf : t.tag === 'stem' ? s.stem(load(t.c)) : t.tag === 'fork' ? s.fork(load(t.l), load(t.r)) : t.tag === 'apply' ? s.app(load(t.f), load(t.x)) : s.variable(t.name)
const named = (name: string, id: number) => (s.names.set(id, name), id)
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
const compile = (name: string, e: Cir) => {
  const r = bracketReplay(e)
  return named(name, toStore(r.frames[r.frames.length - 1].term))
}
const ap = (...xs: number[]) => xs.reduce((f, x) => s.app(f, x))
const add = named('add', load(DEFS.add))
const not = named('not', load(DEFS.not))
const and = named('and', load(DEFS.and))
const tru = load(DEFS.true)
const fls = load(DEFS.false)
const twice = compile('twice', lam('f', lam('x', capp(v('f'), capp(v('f'), v('x'))))))
const compose = compile('compose', lam('f', lam('g', lam('x', capp(v('f'), capp(v('g'), v('x')))))))
const swap = compile('swap', lam('f', lam('x', lam('y', capp(v('f'), v('y'), v('x'))))))
const n = (k: number) => load(nat(k))

const RUNS: [string, number][] = [
  ['twice(add(1), 2)', ap(twice, ap(add, n(1)), n(2))],
  ['add(3, 4)', ap(add, n(3), n(4))],
  ['not(and(true, false))', ap(not, ap(and, tru, fls))],
  ['compose(not, not, true)', ap(compose, not, not, tru)],
  ['swap(add, 2, 5)', ap(swap, add, n(2), n(5))],
  ['twice(twice(not), true)', ap(twice, ap(twice, not), tru)],
]

type Policy = { name: string; opts: (tr: Trace, f: number) => LayoutOptions; stay?: boolean }
const base = (tr: Trace, f: number): LayoutOptions => ({ nextPath: tr.nextPath(tr.frames[f]), sk: true, foldData: 40 })
const window = (tr: Trace, f: number) => tr.readsFrom(Math.max(0, f - 4), Math.min(f, 4) + 6)
const POLICIES: Policy[] = [
  { name: 'before', opts: base },
  { name: 'reads ahead', opts: (tr, f) => ({ ...base(tr, f), reads: tr.readsFrom(f, 6) }) },
  { name: 'reads -4..+6', opts: (tr, f) => ({ ...base(tr, f), reads: window(tr, f) }) },
  { name: 'stays open', opts: (tr, f) => ({ ...base(tr, f), reads: tr.readsFrom(f, 6) }), stay: true },
  { name: 'repeats 12', opts: (tr, f) => ({ ...base(tr, f), reads: window(tr, f), repeats: 12 }) },
  // what the viewer does: repeats fold through a whole run whose biggest tree passes 120 nodes, never in a small one
  { name: 'chosen', opts: (tr, f) => ({ ...base(tr, f), reads: tr.readsFrom(f, 6), repeats: tr.sizes.some((n) => n > 120) ? 24 : undefined }) },
]

const stopsOf = (tr: Trace) => {
  const out = [0]
  for (let f = 0; f < tr.last; ) {
    f++
    while (f < tr.last && tr.folds(f)) f++
    out.push(f)
  }
  return out
}

const pad = (x: string | number, w: number) => String(x).padStart(w)
console.log(`${'run'.padEnd(26)}${'policy'.padEnd(16)}${pad('steps', 6)}${pad('nodes', 7)}${pad('pods', 6)}${pad('by size', 8)}${pad('acts on fold', 13)}${pad('opens', 7)}${pad('closes', 8)}`)
for (const [label, root] of RUNS) {
  const tr = new Trace(s, root)
  tr.advance(2000)
  const stops = stopsOf(tr).slice(0, 120)
  for (const p of POLICIES) {
    let nodes = 0
    let pods = 0
    let bySize = 0
    let actsOnFold = 0
    let opens = 0
    let closes = 0
    let prev = new Map<number, boolean>()
    for (const f of stops) {
      const stay = p.stay ? new Set([...prev].flatMap(([id, pod]) => (pod ? [] : [id]))) : undefined
      const sc = layoutScene(s, [tr.frames[f]], { open: new Set(), closed: new Set() }, { ...p.opts(tr, f), stay })
      nodes += sc.nodes.length
      const podNodes = sc.nodes.filter((x) => x.kind === K_POD)
      pods += podNodes.length
      bySize += podNodes.filter((x) => x.text?.startsWith('…')).length
      // the step about to fire acts on a fold: the next site, or what it calls, is folded
      const next = tr.nextPath(tr.frames[f])
      if (next) {
        const key = 'r0' + next.join('')
        if (podNodes.some((x) => key.startsWith(x.key) || x.key === key + '0')) actsOnFold++
      }
      // a subtree that is in both frames, folded in one and open in the other
      const now = new Map<number, boolean>()
      // a subtree counts as open when any copy of it is drawn open
      for (const x of sc.nodes) if (s.size[x.id] > 3) now.set(x.id, x.kind === K_POD && now.get(x.id) !== false)
      for (const [id, pod] of now)
        if (prev.has(id) && prev.get(id) !== pod) {
          if (pod) closes++
          else opens++
        }
      prev = now
    }
    const k = stops.length
    console.log(`${label.padEnd(26)}${p.name.padEnd(16)}${pad(k - 1, 6)}${pad((nodes / k).toFixed(0), 7)}${pad((pods / k).toFixed(1), 6)}${pad((bySize / k).toFixed(1), 8)}${pad(actsOnFold, 13)}${pad(opens, 7)}${pad(closes, 8)}`)
  }
}
