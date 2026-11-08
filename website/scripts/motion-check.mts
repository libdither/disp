// The tree viewer's motion, checked without a browser: real Build and Try it reels
// are laid out and stepped through motion.ts the way TreeCanvas draws them, and
// each step is sampled for the rules the motion keeps.
//   npm run motion-check                        every reel, with a summary per rule
//   npm run motion-check -- --svg <reel> <step> [file]   draw that step's paths to an SVG
import { writeFileSync } from 'node:fs'
import { Store } from '../src/lib/playground/store.ts'
import { Trace } from '../src/lib/playground/trace.ts'
import { layoutScene, type Scene } from '../src/lib/playground/layout.ts'
import { stepGuide } from '../src/lib/playground/guides.ts'
import { orient, planTween, nodesAt, ghostAt, reachCam, reachOf, pinAt, camAt, knockedOff, knockTime, fallAt, fallLife, LANDS, GRAVITY, type Fall, type View, type Cam } from '../src/lib/playground/motion.ts'
import { bracketReplay } from '../src/lib/disp/bracket.ts'
import type { Cir } from '../../src/elab/cir.ts'

const s = new Store()
const v = (name: string): Cir => ({ tag: 'var', name })
const lam = (x: string, body: Cir): Cir => ({ tag: 'lam', x, body })
const t: Cir = { tag: 'lit', t: s.leaf as never }
const ap = (f: Cir, ...xs: Cir[]): Cir => xs.reduce<Cir>((g, x) => ({ tag: 'app', f: g, x }), f)
const fn = (params: string[], body: Cir) => params.reduceRight((b, p) => lam(p, b), body)
/// a replay frame as the worker hands it over: binders kept, K(x) held, I(x) and K(x, y) already applied
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
    default:
      throw new Error('unexpected ' + e.tag)
  }
}

const DEFS: [string, string[], Cir][] = [
  ['swap', ['f', 'x', 'y'], ap(v('f'), v('y'), v('x'))],
  ['tri', ['on_leaf', 'on_stem', 'on_fork'], ap(t, ap(t, v('on_leaf'), v('on_stem')), v('on_fork'))],
  ['twice', ['f', 'x'], ap(v('f'), ap(v('f'), v('x')))],
  ['id', ['x'], v('x')],
  ['const', ['x', 'y'], v('x')],
  ['compose', ['f', 'g', 'x'], ap(v('f'), ap(v('g'), v('x')))],
]

// and some random ones, the same every run
let seed = 11
const rnd = (n: number) => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) % n
const randomBody = (params: string[], depth: number): Cir => {
  const r = depth <= 0 ? rnd(4) : rnd(7)
  if (r === 0) return t
  if (r <= 3) return v(params[rnd(params.length)])
  return ap(randomBody(params, depth - 1), randomBody(params, depth - 1))
}
for (let i = 0; i < 40; i++) {
  const params = ['a', 'b', 'c'].slice(0, 1 + rnd(3))
  DEFS.push([`random${i}`, params, randomBody(params, 3)])
}

interface Reel {
  name: string
  trace: Trace
}
const reels: Reel[] = []
for (const [name, params, body] of DEFS) {
  const r = bracketReplay(fn(params, body))
  if (r.mismatch || r.skipped) continue
  const start = held(r.frames[0].term)
  const done = held(r.frames[r.frames.length - 1].term)
  const build = new Trace(s, start)
  build.advance(2000)
  reels.push({ name: `${name}/build`, trace: build })
  const tryIt = new Trace(s, params.reduce((f, p) => s.app(f, s.variable(p)), s.folded(done)))
  tryIt.advance(2000)
  reels.push({ name: `${name}/try`, trace: tryIt })
}

/// the frames the reel rests on with building skipped, as the viewer steps
const stopsOf = (tr: Trace) => {
  const out = [0]
  for (let f = 0; f < tr.last; ) {
    f++
    while (f < tr.last && tr.folds(f)) f++
    out.push(f)
  }
  return out
}
const VIEW: View = { w: 630, h: 670, up: true }
const layout = (tr: Trace, f: number) => layoutScene(s, [tr.frames[f]], { open: new Set(), closed: new Set() }, { nextPath: tr.nextPath(tr.frames[f]), sk: true })

const SAMPLES = 40
/// how far a node may slip backwards before it counts as turning back, in screen pixels
const SLIP = 0.75

interface StepPaths {
  nodes: { x: number[]; y: number[]; fresh: boolean; key: string }[]
  ghosts: { x: number[]; y: number[]; hit: boolean; key: string }[]
  cams: Cam[]
  /// what a binder knocks off, from the knock until it is gone: world positions, and the trunk's place
  falls: { key: string; f: Fall; x: number[]; y: number[]; trunk: number }[]
}
/// one step sampled the way TreeCanvas draws it, in screen coordinates
function sampleStep(tr: Trace, stops: number[], i: number, reach: Scene['reach']): StepPaths {
  const a = stops[i]
  const b = stops[i + 1]
  const prev = orient({ ...layout(tr, a), reach }, true)
  const next = orient({ ...layout(tr, b), guide: stepGuide(s, tr, a), reach }, true)
  const settled = reachCam(VIEW, prev)!
  const tw = planTween(prev, next, Float64Array.from(prev.nodes, (n) => n.x), Float64Array.from(prev.nodes, (n) => n.y), true)
  const target = reachCam(VIEW, next)!
  const pin = pinAt(target, settled, next)!
  const curX = Float64Array.from(tw.fromX)
  const curY = Float64Array.from(tw.fromY)
  const out: StepPaths = {
    nodes: next.nodes.map((n, j) => ({ x: [], y: [], fresh: !!tw.fresh[j], key: n.key })),
    ghosts: tw.ghosts.map((g) => ({ x: [], y: [], hit: g.hit >= 0, key: g.n.key })),
    cams: [],
    falls: [],
  }
  {
    const kx = Float64Array.from(tw.fromX)
    const ky = Float64Array.from(tw.fromY)
    nodesAt(tw, next, knockTime(tw), kx, ky)
    // measured from the trunk, which ends the step where the next scene has it
    const r = next.nodes[tw.root]
    for (const { ghost, fall } of knockedOff(tw, kx, ky, true)) {
      const life = fallLife(fall)
      const at = Array.from({ length: 81 }, (_, k) => fallAt(fall, (k / 80) * life))
      out.falls.push({ key: tw.ghosts[ghost].n.key, f: { ...fall, ground: fall.ground + r.y }, x: at.map((a) => a.x + r.x), y: at.map((a) => a.y + r.y), trunk: r.x })
    }
  }
  for (let k = 0; k <= SAMPLES; k++) {
    const tt = k / SAMPLES
    nodesAt(tw, next, tt, curX, curY)
    const cam = camAt(pin, tt, curX, curY)
    out.cams.push(cam)
    next.nodes.forEach((_, j) => {
      out.nodes[j].x.push(cam.x + curX[j] * cam.k)
      out.nodes[j].y.push(cam.y + curY[j] * cam.k)
    })
    tw.ghosts.forEach((_, j) => {
      const g = ghostAt(tw, j, tt, curX, curY, true)
      out.ghosts[j].x.push(cam.x + g.x * cam.k)
      out.ghosts[j].y.push(cam.y + g.y * cam.k)
    })
  }
  return out
}

/// how far a path goes back against the way it mostly moves
const backtrack = (xs: number[]) => {
  const net = xs[xs.length - 1] - xs[0]
  const dir = Math.sign(net)
  let worst = 0
  let best = xs[0]
  for (const x of xs) {
    if (dir >= 0) {
      best = Math.max(best, x)
      worst = Math.max(worst, best - x)
    } else {
      best = Math.min(best, x)
      worst = Math.max(worst, x - best)
    }
  }
  // a path that goes out and comes back to where it started turns back by how far it went
  if (dir === 0) worst = Math.max(...xs.map((x) => Math.abs(x - xs[0])))
  return worst
}

if (process.argv[2] === '--svg') {
  const [, , , name, stepArg, file] = process.argv
  const reel = reels.find((r) => r.name === name)
  if (!reel) throw new Error(`no reel ${name}; reels: ${reels.map((r) => r.name).join(' ')}`)
  const stops = stopsOf(reel.trace)
  const reach = reachOf(reel.trace.frames.map((f, k) => layout(reel.trace, k)))
  const p = sampleStep(reel.trace, stops, Number(stepArg), reach)
  const line = (xs: number[], ys: number[]) => xs.map((x, k) => `${x.toFixed(1)},${ys[k].toFixed(1)}`).join(' ')
  const ground = p.cams[SAMPLES].y + 26 * p.cams[SAMPLES].k
  const svg = [
    `<svg xmlns="http://www.w3.org/2000/svg" width="${VIEW.w}" height="${VIEW.h}" viewBox="0 0 ${VIEW.w} ${VIEW.h}" font-family="monospace" font-size="9">`,
    `<rect width="100%" height="100%" fill="#f4f7ee"/>`,
    `<line x1="0" x2="${VIEW.w}" y1="${ground}" y2="${ground}" stroke="#9c9" stroke-dasharray="4 3"/>`,
    ...p.ghosts.map((g) => `<polyline points="${line(g.x, g.y)}" fill="none" stroke="${g.hit ? '#e03131' : '#aaa'}" stroke-dasharray="3 2"/>`),
    ...p.falls.map((f) => {
      const c = p.cams[SAMPLES]
      return `<polyline points="${line(f.x.map((x) => c.x + x * c.k), f.y.map((y) => c.y + y * c.k))}" fill="none" stroke="#e03131"/>` + f.x.map((x, k) => (k % 4 ? '' : `<circle cx="${c.x + x * c.k}" cy="${c.y + f.y[k] * c.k}" r="1.6" fill="#e03131"/>`)).join('')
    }),
    ...p.nodes.map((n) => `<polyline points="${line(n.x, n.y)}" fill="none" stroke="${n.fresh ? '#1971c2' : '#555'}"/>`),
    ...p.nodes.map((n) => `<circle cx="${n.x[SAMPLES]}" cy="${n.y[SAMPLES]}" r="2.5" fill="${n.fresh ? '#1971c2' : '#555'}"/><circle cx="${n.x[0]}" cy="${n.y[0]}" r="1.5" fill="#bbb"/>`),
    `<text x="8" y="14">${name} step ${stepArg}: grey old, blue new, dashed ghosts, red the knocked-off fall (a dot every 1/20 of its life), dots start small and end big</text>`,
    `</svg>`,
  ].join('\n')
  const out = file ?? `/tmp/motion-${name.replace('/', '-')}-${stepArg}.svg`
  writeFileSync(out, svg)
  console.log(out)
  process.exit(0)
}

let failed = 0
const rules = { turnsBack: 0, trunkMoves: 0, zoomChanges: 0, offView: 0, appleMisses: 0 }
for (const reel of reels) {
  const tr = reel.trace
  const stops = stopsOf(tr)
  const reach = reachOf(tr.frames.map((_, k) => layout(tr, k)))
  const problems: string[] = []
  let trunk: { x: number; y: number; k: number } | null = null
  for (let i = 0; i + 1 < stops.length; i++) {
    const p = sampleStep(tr, stops, i, reach)
    const root = p.nodes.findIndex((n) => n.key === 'r0')
    for (const [k, cam] of p.cams.entries()) {
      const at = { x: p.nodes[root].x[k], y: p.nodes[root].y[k], k: cam.k }
      trunk ??= at
      if (Math.abs(at.x - trunk.x) > 0.5 || Math.abs(at.y - trunk.y) > 0.5) {
        rules.trunkMoves++
        problems.push(`step ${i}: the trunk moves to (${at.x.toFixed(1)}, ${at.y.toFixed(1)}) from (${trunk.x.toFixed(1)}, ${trunk.y.toFixed(1)})`)
        break
      }
      if (Math.abs(cam.k - trunk.k) > 1e-6) {
        rules.zoomChanges++
        problems.push(`step ${i}: the zoom goes from ${trunk.k.toFixed(3)} to ${cam.k.toFixed(3)}`)
        break
      }
    }
    for (const n of p.nodes) {
      const back = Math.max(backtrack(n.x), backtrack(n.y))
      if (back > SLIP) {
        rules.turnsBack++
        problems.push(`step ${i}: ${n.key} turns back ${back.toFixed(1)}px`)
      }
      const out = n.x.some((x) => x < 0 || x > VIEW.w) || n.y.some((y) => y < 0 || y > VIEW.h)
      if (out) {
        rules.offView++
        problems.push(`step ${i}: ${n.key} leaves the view`)
      }
    }
    const top = Math.min(...p.falls.map((f) => f.key.length))
    for (const fl of p.falls) {
      if (fl.key.length !== top) continue
      const cam = p.cams[SAMPLES]
      const last = fl.x.length - 1
      // falling only ever goes down to the ground, comes to rest on it under the tree, takes a believable time, and stays in view
      const firstLanding = (-fl.f.vy + Math.sqrt(fl.f.vy ** 2 + 2 * GRAVITY * (fl.f.ground - fl.f.y))) / GRAVITY
      const why = [
        fl.y.some((y) => y > fl.f.ground + 0.01) && 'goes below the ground',
        Math.abs(fl.y[last] - fl.f.ground) > 0.01 && 'does not end on the ground',
        Math.abs(fl.x[last] - fl.trunk) > LANDS && `lands ${Math.abs(fl.x[last] - fl.trunk).toFixed(0)} from the trunk`,
        (firstLanding < 0.3 || firstLanding > 1.1) && `takes ${firstLanding.toFixed(2)}s to land`,
        fl.x.some((x) => cam.x + x * cam.k < 0 || cam.x + x * cam.k > VIEW.w) && 'leaves the view',
      ].filter(Boolean)
      if (why.length) {
        rules.appleMisses++
        problems.push(`step ${i}: knocked-off ${fl.key} ${why.join(', ')}`)
      }
    }
  }
  console.log(`${problems.length ? 'FAIL' : 'ok  '} ${reel.name}: ${stops.length - 1} steps${problems.length ? '\n       ' + problems.slice(0, 6).join('\n       ') + (problems.length > 6 ? `\n       ...and ${problems.length - 6} more` : '') : ''}`)
  if (problems.length) failed++
}
console.log(`\nturns back ${rules.turnsBack} · trunk moves ${rules.trunkMoves} · zoom changes ${rules.zoomChanges} · leaves the view ${rules.offView} · knocked-off variable falls wrong ${rules.appleMisses}`)
process.exit(failed ? 1 : 0)
