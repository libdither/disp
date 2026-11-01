// The frame's viewer: runs the design as Bend built it (out/view.mjs, from view.bend) on a program and a
// tree, draws the lattice tick by tick, and shows LAWS.bend with the last check beside it. It reloads
// itself whenever check.sh finishes. Bundled into ../frame.js by bundle.mts.
import { define, parseProg, parseVal, valText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { F, L, S, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { ADDB, ADDT, GLOSS, readRegister, readWords, register, words } from './binary.ts'

define('addb', parseProg(ADDB))
define('addt', parseProg(ADDT))

const $ = <T extends HTMLElement = HTMLElement>(id: string) => document.getElementById(id) as T
const cv = $<HTMLCanvasElement>('cv')
const ctx = cv.getContext('2d')!

// ---- Bend values: {$: constructor, ...fields}, Nat a number, Bool a boolean ----
type B = { $: string; [field: string]: any }
type Grid = B[][]
interface View {
  packets(p: B): B; symbols(ps: B): B; lay(v: B, s: B): B; tick(g: B): B; still(g: B): boolean
  reading(g: B, v: B, s: B): B; answer(p: B, v: B): B; reference(p: B, v: B): B
}
const toB = (v: V): B => (v.tag === 'leaf' ? { $: 'L' } : v.tag === 'stem' ? { $: 'S', c: toB(v.c) } : { $: 'F', l: toB(v.l), r: toB(v.r) })
const fromB = (b: B): V => (b.$ === 'L' ? L() : b.$ === 'S' ? S(fromB(b.c)) : F(fromB(b.l), fromB(b.r)))
function list(b: B): B[] {
  const out: B[] = []
  for (; b.$ === 'Con'; b = b.tail) out.push(b.head)
  return out
}
const maybe = (b: B): B | null => (b.$ === 'Some' ? b.value : null)
const fields = (b: B) => Object.keys(b).filter((k) => k !== '$')
const isB = (x: unknown): x is B => typeof x === 'object' && x !== null && '$' in x
function text(x: unknown): string {
  if (!isB(x)) return String(x)
  const ks = fields(x)
  return ks.length ? `${x.$}{${ks.map((k) => `${k}: ${text(x[k])}`).join(', ')}}` : x.$
}
function same(a: unknown, b: unknown): boolean {
  if (!isB(a) || !isB(b)) return a === b
  return a.$ === b.$ && fields(a).every((k) => same(a[k], b[k]))
}
const bits = (x: number, n: number) => x.toString(2).padStart(n, '0')
/** a tree as text, and the two numbers in it if it holds binary add's register pair or tree add's words */
function treeText(b: B | null): string {
  if (!b) return 'none'
  const v = fromB(b), r = readRegister(v) ?? readWords(v)
  return valText(v, 'tree') + (r ? ` (a = ${r.a} = ${bits(r.a, r.n)}, b = ${r.b} = ${bits(r.b, r.n)} in ${r.n} bits)` : '')
}
/** a tree: one in the reference's syntax; bits(a, b, n), the list binary add reads; or words(a, b, n),
 * the words tree add reads */
function treeOf(src: string): V {
  const m = /^\s*(bits|words)\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)\s*$/.exec(src)
  if (!m) return parseVal(src)
  const [a, b, n] = [m[2], m[3], m[4]].map(Number)
  if (n < 1 || n > 16 || a >= 2 ** n || b >= 2 ** n) throw new Error(`${m[1]}(a, b, n): n from 1 to 16, a and b under 2^n`)
  if (m[1] === 'bits') return register(a, b, n)
  if (n & (n - 1)) throw new Error('words(a, b, n): n is 1, 2, 4, 8 or 16')
  return words(a, b, n)
}

// a packet's symbols as text: ↓ ↙ ↘ steps into a stem's part, left, right; T(…) a tri, which runs … on
// its node when the left part is a stem (a 1), T*(…) a loop, μ(…) a fix the fork runs
const STEP: Record<string, string> = { M0: '↓', M1: '↙', M2: '↘', X: 'swap', Sp: 'S+', Sm: 'S−', Flip: '¬', Chk: '?', Me: 'self', Each: '∀' }
function symText(s: B): string {
  const seq = (b: B) => list(b).map(symText).join(' ')
  if (s.$ === 'Fix') return `μ(${seq(s.body)})`
  if (s.$ === 'Tri') {
    const a = seq(s.a), k = seq(s.k), j = seq(s.j), star = s.rec ? '*' : ''
    return !a && !k ? `T${star}(${j})` : `T${star}(${[a && `at the bottom: ${a}`, j && `going down: ${j}`, k && `coming back: ${k}`].filter(Boolean).join('; ')})`
  }
  return STEP[s.$] ?? s.$
}

// ---- the run ----
interface Run {
  vb: B; syms: B; packets: string[][] | null; n: number; reg: boolean; raw: B[]; frames: Grid[]; still: boolean; done: number
  answer: B | null; reference: B | null; matter: string[]; cells: number[]; cols: number; rows: number
}
const CAP = 5000
let view: View | null = null
let run: Run | null = null
let t = 0

function compute(p: V, v: V): Run {
  const pb = toB(p), vb = toB(v), w = view!
  const ps = maybe(w.packets(pb))
  const syms = ps ? w.symbols(ps) : { $: 'Nil' }
  let g = w.lay(vb, syms)
  const raw = [g]
  let done = 0
  while (!w.still(g) && raw.length <= CAP) raw.push((g = w.tick(g)))
  const still = w.still(g)
  done = raw.length - 1
  // a still grid may go on packing its chains (slack sinking past stems); show that too
  for (let k = 0; still && k < CAP; k++) {
    const h = w.tick(g)
    if (same(h, g)) break
    raw.push((g = h))
  }
  // a design may wrap its matter (loopnet: Thing{p}); the page draws what is inside
  const unwrap = (c: B): B => (c.$ === 'Thing' ? c.p : c)
  const frames = raw.map((g) => list(g).map((row) => list(row).map(unwrap)))
  const counts = frames.map((f) => {
    const n = new Map<string, number>()
    for (const row of f) for (const c of row) if (c.$ !== 'Void') n.set(c.$, (n.get(c.$) ?? 0) + 1)
    return n
  })
  return {
    vb, syms, done, packets: ps && list(ps).map((q) => list(q).map(symText)), n: list(syms).length, raw, frames, reg: !!readRegister(v),
    still, answer: ps && maybe(w.answer(pb, vb)), reference: maybe(w.reference(pb, vb)),
    matter: counts.map((n) => [...n].map(([k, m]) => `${m} ${k}`).join(' · ')),
    cells: counts.map((n) => [...n.values()].reduce((a, b) => a + b, 0)),
    cols: frames[0][0]?.length ?? 0, rows: frames[0].length
  }
}

// ---- inputs ----
const full = (d: number): string => (d === 0 ? '△' : `(${full(d - 1)}, ${full(d - 1)})`)
// a full tree with its first leaf marked (a stem), so a mirror shows
const marked = (d: number): string => (d === 0 ? '△△' : `(${marked(d - 1)}, ${full(d - 1)})`)
const mirror = (d: number): string => (d === 0 ? 'id' : `map(id, ${mirror(d - 1)}, ${mirror(d - 1)}) ; swap`)
const PRESETS: [string, string, string?][] = [
  ['first(swap) ; swap', '((△, △△), △△△)'],
  ['swap', '(△, △△)'],
  ['map(swap, swap, swap)', '((△, △△), (△△, △))'],
  ['second(first(swap)) ; swap', '((△, △△), ((△, △△), △))'],
  ['(first(swap) ; swap)†', '((△, △△), △△△)'],
  ['map(map(swap, id, id), swap, second(swap))', '△((△, △△), (△, (△△, △)))'],
  ['par(swap, first(swap)) ; swap', '((△, △△), ((△, △△), △))'],
  ['map(swap, swap, swap)', '△(△, △△)'],
  ...[2, 3, 4, 5].map((d): [string, string, string] => [mirror(d), marked(d), `mirror, unrolled, on ${2 ** d} leaves`]),
  ['mirror', marked(3), 'mirror, the library\'s: one symbol, copied at each fork'],
  ['mirror', marked(5), 'mirror, the library\'s, on 32 leaves'],
  ['add', '(△△△△, △△△)', 'add: (3, 2) → (3, 5)'],
  ['add', '(△△△△△△△△△, △△)', 'add: (8, 1), its right chain growing into empty cells'],
  ['fix(tri(id, self ; first(swap), id))', '(△△(△, △△), △△)', 'a loop that waits on R (its burst goes left)'],
  ['add†', '(△△△, △△△△△△)', 'add†: (2, 5) → (2, 3)'],
  ['add†', '(△△△△, △△)', 'add†: (3, 1) is stuck'],
  ['map(id, add, add)', '((△△, △△△), (△△△, △△))', 'map(id, add, add): two loops at once'],
  ['second(stem) ; add', '(△△△, (△, △))', 'second(stem) ; add'],
  ['fix(tri(id, second(stem) ; self, id))', '(△△△, △)', 'a loop with its burst before self'],
  ['swap ; stem', '(△, △△)'],
  ['add ; swap', '(△△, △△)', 'add ; swap: the swap waits in the fork\'s queue until add is done'],
  ['not', '△', 'not on a bit: 0 (△) becomes 1 (△△)'],
  ['tri(id, second(not), id)', '(△△, △)', 'flips the right bit when the left one is 1 (cnot)'],
  ['tri(id, second(tri(id, second(not), id)), id)', '(△△, (△△, △))', 'flips the last bit when both before it are 1 (Toffoli)'],
  ['fix(first(not) ; second(self))', '(△, (△△, (△, (△△, △))))', 'a fix a fork runs: not on every bit down a list'],
  ['addb', 'bits(5, 3, 4)', 'binary add: 5 + 3 in 4 bits'],
  ['addb', 'bits(15, 1, 4)', 'binary add: 15 + 1, the carry rippling all the way up (4 bits wrap to 0)'],
  ['addb†', 'bits(5, 8, 4)', 'binary add run backward: 8 − 5'],
  ['addt', 'words(5, 3, 4)', 'tree add: 5 + 3 on 4-bit words, outside the design (needs tri\'s fork case)'],
  ['cross', '((△, △△), △)', 'cross: outside the design']
]

function rerun(keep = false) {
  $('err').textContent = ''
  if (!view) return
  let p: V, v: V
  try {
    p = parseProg($<HTMLInputElement>('prog').value)
    v = treeOf($<HTMLInputElement>('tree').value)
  } catch (e) {
    $('err').textContent = (e as Error).message
    return
  }
  const q = new URLSearchParams({ p: $<HTMLInputElement>('prog').value, v: $<HTMLInputElement>('tree').value })
  history.replaceState(null, '', '?' + q)
  const fresh = !run || !keep
  run = compute(p, v)
  t = keep ? Math.min(t, run.frames.length - 1) : 0
  $<HTMLInputElement>('time').max = String(run.frames.length - 1)
  summary()
  if (fresh) fit()
  show()
}

function summary() {
  const r = run!
  const agree = r.packets === null ? null : r.answer === null || r.reference === null ? r.answer === r.reference : same(r.answer, r.reference)
  const v = $('verdict')
  v.className = agree === null ? '' : agree ? 'ok' : 'bad'
  v.innerHTML =
    r.packets === null
      ? `outside the design: it has no packets for this program; the reference gives <b>${treeText(r.reference)}</b>`
      : agree && !r.answer
        ? `✓ stuck, as the reference is`
        : agree
        ? `✓ the design gives the reference's answer, <b>${treeText(r.answer)}</b>`
        : `✗ the design gives <b>${treeText(r.answer)}</b> but the reference <b>${treeText(r.reference)}</b>`
  const kept = r.matter.every((m) => m === r.matter[0])
  $('stats').innerHTML =
    `${r.packets?.length ?? 0} packets, ${r.n} symbols · grid ${r.cols}×${r.rows} · ` +
    (r.still ? `done after <b>${r.done}</b> ticks${r.frames.length - 1 > r.done ? `, packed after ${r.frames.length - 1}` : ''}` : `<span class="bad">not still after ${CAP} ticks</span>`) +
    ` · ${r.cells[0]} cells${r.cells.at(-1)! > r.cells[0] ? `, grew to ${r.cells.at(-1)}` : ''}${r.cells.every((c, i) => i === 0 || c >= r.cells[i - 1]) ? '' : ' <span class="bad">(a tick removed one)</span>'}` +
    (kept ? ` · matter ${r.matter[0]}, the same every tick` : '')
  $('packets').textContent = r.packets ? 'packets: ' + (r.packets.map((q) => q.join(' ')).join('  |  ') || 'none') : ''
  const name = $<HTMLInputElement>('prog').value.trim()
  $('doc').textContent = GLOSS[name.replace(/†$/, '')] ? (name.endsWith('†') ? 'run backward: ' : '') + GLOSS[name.replace(/†$/, '')] : ''
  for (const id of ['verdict', 'stats', 'packets', 'doc']) $(id).title = $(id).textContent!
}

function show() {
  const r = run!
  $<HTMLInputElement>('time').value = String(t)
  $('tick').textContent = `tick ${t} / ${r.frames.length - 1}`
  $('now').innerHTML = `the grid reads <b>${treeText(maybe(view!.reading(r.raw[t], r.vb, r.syms)))}</b> at tick ${t} · ${r.matter[t]}`
  $('now').title = $('now').textContent!
  draw()
}

// ---- drawing ----
const DIRS: Record<string, [number, number]> = { Up: [0, -1], Rt: [1, 0], Dn: [0, 1], Lf: [-1, 0] }
const isDir = (x: unknown): x is B => isB(x) && x.$ in DIRS
const SYM: Record<string, string> = {
  X: '#ffd866', M0: '#a9b6c8', M1: '#7aa2f7', M2: '#ffb454', Sp: '#3fdc8f', Sm: '#ff6b6b', Tri: '#b39cff', Walk: '#b39cff',
  N: '#56c8ff', R: '#56c8ff', H: '#56c8ff', E: '#56c8ff', Each: '#ff9ee6', Fix: '#ff9ee6', Me: '#ff9ee6', Flip: '#ff9e64', Chk: '#ff9e64'
}
const LABEL: Record<string, string> = { Sp: 'S+', Sm: 'S−', Tri: 'T', Walk: 'W', Each: '∀', Fix: 'μ', Flip: '¬', Chk: '?' }
const PART: Record<string, string> = { l: '#7aa2f7', r: '#ffb454', kid: '#a9b6c8' }
const FILL: Record<string, string> = { Leaf: '#3fdc8f', Stem: '#a9b6c8', Fork: '#e2e8f0', Grow: '#3fdc8f' }
const LATCH: Record<string, string> = { Pass: '#3fdc8f', Drop: '#ff6b6b', Held: '#b39cff' }
const hue = (s: string) => `hsl(${[...s].reduce((h, c) => h * 31 + c.charCodeAt(0), 7) % 360} 60% 65%)`
let z = 20, ox = 0, oy = 0

// fits the tree and the last few symbols waiting above it; the rest of the column runs off the top
function fit() {
  const r = run!
  const top = $('hud').offsetHeight + 20, bottom = $('dock').offsetHeight + 10, right = $('side').offsetWidth + 24
  const w = innerWidth - right - 24, h = innerHeight - top - bottom
  const rows = r.rows - r.n + Math.min(r.n, 6)
  z = Math.max(2, Math.min(36, Math.min(w / r.cols, h / rows)))
  ox = 12 + (w - z * r.cols) / 2
  oy = top + h - z * r.rows - Math.max(0, (h - z * rows) / 2)
}

function line(x0: number, y0: number, d: B, color: string, width: number, arrow = false) {
  const [dx, dy] = DIRS[d.$]
  ctx.strokeStyle = color
  ctx.lineWidth = width
  ctx.beginPath()
  ctx.moveTo(x0, y0)
  ctx.lineTo(x0 + dx * z, y0 + dy * z)
  ctx.stroke()
  if (arrow && z >= 8) {
    const mx = x0 + (dx * z) / 2, my = y0 + (dy * z) / 2, a = z * 0.16
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.moveTo(mx + dx * a, my + dy * a)
    ctx.lineTo(mx - dx * a - dy * a, my - dy * a + dx * a)
    ctx.lineTo(mx - dx * a + dy * a, my - dy * a - dx * a)
    ctx.fill()
  }
}

function badge(x: number, y: number, s: string, up = false) {
  ctx.fillStyle = SYM[s] ?? hue(s)
  if (up) {
    // going up: an outlined badge with a point on top
    ctx.strokeStyle = ctx.fillStyle
    ctx.lineWidth = Math.max(1, z * 0.07)
    const w = z * 0.5, h = z * 0.34
    ctx.beginPath()
    ctx.moveTo(x - w / 2, y + h / 2)
    ctx.lineTo(x + w / 2, y + h / 2)
    ctx.lineTo(x + w / 2, y - h / 4)
    ctx.lineTo(x, y - h * 0.8)
    ctx.lineTo(x - w / 2, y - h / 4)
    ctx.closePath()
    ctx.fillStyle = '#0b0e14'
    ctx.fill()
    ctx.stroke()
    if (z >= 14) {
      ctx.fillStyle = ctx.strokeStyle
      ctx.font = `bold ${Math.round(h * 0.7)}px ui-monospace, monospace`
      ctx.textAlign = 'center'
      ctx.textBaseline = 'middle'
      ctx.fillText(LABEL[s] ?? s, x, y + h / 8)
    }
    return
  }
  if (z < 12) {
    ctx.beginPath()
    ctx.arc(x, y, Math.max(1.5, z * 0.22), 0, 7)
    ctx.fill()
    return
  }
  const w = z * 0.62, h = z * 0.42
  ctx.beginPath()
  ctx.roundRect(x - w / 2, y - h / 2, w, h, h / 3)
  ctx.fill()
  ctx.fillStyle = '#0b0e14'
  ctx.font = `bold ${Math.round(h * 0.75)}px ui-monospace, monospace`
  ctx.textAlign = 'center'
  ctx.textBaseline = 'middle'
  ctx.fillText(LABEL[s] ?? s, x, y + 0.5)
}

function draw() {
  const dpr = devicePixelRatio || 1
  if (cv.width !== innerWidth * dpr || cv.height !== innerHeight * dpr) {
    cv.width = innerWidth * dpr
    cv.height = innerHeight * dpr
  }
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.fillStyle = '#0b0e14'
  ctx.fillRect(0, 0, innerWidth, innerHeight)
  if (!run) return
  const g = run.frames[t], prev = t > 0 ? run.frames[t - 1] : null
  const at = (x: number, y: number) => [ox + (x + 0.5) * z, oy + (y + 0.5) * z]
  ctx.strokeStyle = '#1b2436'
  ctx.lineWidth = 1
  ctx.strokeRect(ox, oy, run.cols * z, run.rows * z)
  g.forEach((row, y) => row.forEach((c, x) => {
    if (prev && !same(prev[y][x], c)) {
      ctx.fillStyle = 'rgba(255,216,102,.10)'
      ctx.fillRect(ox + x * z, oy + y * z, z, z)
    }
  }))
  // wires and a node's parts, then what is moving
  g.forEach((row, y) => row.forEach((c, x) => {
    const [cx, cy] = at(x, y)
    for (const k of fields(c)) {
      const d = c[k]
      if (!isDir(d)) continue
      if (k === 'to') {
        if (c.$ === 'Wire') line(cx, cy, d, '#3a4a63', Math.max(1, z * 0.12), true)
      } else line(cx, cy, d, PART[k] ?? '#5b6a80', Math.max(1, z * 0.14))
    }
  }))
  g.forEach((row, y) => row.forEach((c, x) => {
    if (c.$ === 'Void' || c.$ === 'Wire') return
    const [cx, cy] = at(x, y)
    if (c.$ === 'Jam') {
      ctx.strokeStyle = '#ff6b6b'
      ctx.lineWidth = Math.max(1.5, z * 0.12)
      ctx.beginPath()
      ctx.moveTo(cx - z * 0.3, cy - z * 0.3)
      ctx.lineTo(cx + z * 0.3, cy + z * 0.3)
      ctx.moveTo(cx + z * 0.3, cy - z * 0.3)
      ctx.lineTo(cx - z * 0.3, cy + z * 0.3)
      ctx.stroke()
      return
    }
    const sending = isB(c.sym) && c.sym.$ !== 'Q' && isDir(c.to)
    if (sending) for (const d of c.wide ? [c.l, c.r] : [c.to]) line(cx, cy, d, SYM[c.sym.$] ?? hue(c.sym.$), Math.max(1.5, z * 0.16), true)
    ctx.fillStyle = FILL[c.$] ?? hue(c.$)
    ctx.beginPath()
    ctx.arc(cx, cy, z * (c.$ === 'Leaf' ? 0.22 : 0.32), 0, 7)
    ctx.fill()
    const latch = isB(c.hold) && c.hold.$ === 'Held' ? 'Held' : c.busy ? 'Held' : isB(c.latch) ? c.latch.$ : 'Idle'
    const waiting = isB(c.todo) ? list(c.todo).length : 0
    if (waiting && z >= 10) {
      ctx.fillStyle = '#ffd866'
      ctx.font = `bold ${Math.round(z * 0.32)}px ui-monospace, monospace`
      ctx.textAlign = 'center'
      ctx.textBaseline = 'middle'
      ctx.fillText(String(waiting), cx + z * 0.36, cy + z * 0.4)
    }
    if (latch !== 'Idle') {
      ctx.strokeStyle = LATCH[latch] ?? hue(latch)
      ctx.lineWidth = Math.max(1.5, z * 0.12)
      ctx.beginPath()
      ctx.arc(cx, cy, z * 0.44, 0, 7)
      ctx.stroke()
    }
  }))
  if (run.reg) labels(g, at)
  g.forEach((row, y) => row.forEach((c, x) => {
    const [cx, cy] = at(x, y)
    const wire = c.$ === 'Wire'
    if (isB(c.sym) && c.sym.$ !== 'Q') badge(cx + (wire ? 0 : z * 0.38), cy - (wire ? 0 : z * 0.38), c.sym.$)
    if (isB(c.up) && c.up.$ !== 'Q') badge(cx - (wire ? 0 : z * 0.38), cy + (wire ? z * 0.05 : z * 0.38), c.up.$, true)
  }))
}

// binary add's bits named on the grid: position i down the list is (cᵢ, aᵢ, bᵢ), lowest bit first
const SUB = '₀₁₂₃₄₅₆₇₈₉'
const ROLE: Record<string, string> = { a: '#7dcfff', b: '#e0af68', c: '#8595ad' }
function labels(g: Grid, at: (x: number, y: number) => number[]) {
  type At = { x: number; y: number; c: B }
  const node = (x: number, y: number): At | null => {
    for (let k = 0; k < 4 * g.length * (g[0]?.length ?? 0); k++) {
      const c = g[y]?.[x]
      if (!c || c.$ === 'Void' || c.$ === 'Jam') return null
      if (c.$ !== 'Wire') return { x, y, c }
      const [dx, dy] = DIRS[c.to.$]
      x += dx
      y += dy
    }
    return null
  }
  const down = (p: At | null, side: 'l' | 'r') => {
    if (p?.c.$ !== 'Fork') return null
    const [dx, dy] = DIRS[p.c[side].$]
    return node(p.x + dx, p.y + dy)
  }
  if (z < 7) return
  ctx.font = `bold ${Math.round(Math.max(10, z * 0.55))}px ui-monospace, monospace`
  ctx.textAlign = 'right'
  ctx.textBaseline = 'middle'
  let p = node((g[0].length - 1) / 2, 0)
  for (let i = 0; p?.c.$ === 'Fork' && i < 64; i++) {
    const r1 = down(p, 'r'), r2 = down(r1, 'r')
    for (const [q, role] of [[down(p, 'l'), 'c'], [down(r1, 'l'), 'a'], [down(r2, 'l'), 'b']] as const) {
      if (!q) return
      const [cx, cy] = at(q.x, q.y)
      ctx.fillStyle = ROLE[role]
      ctx.fillText(role + [...String(i)].map((d) => SUB[+d]).join(''), cx - z * 0.42, cy - z * 0.3)
    }
    p = down(r2, 'r')
  }
}

// ---- the side panel: laws, the last check, the score ----
type Item = { kind: 'section' | 'note'; text: string } | { kind: 'law'; name: string; doc: string; small: boolean; body: string }
function parseLaws(src: string): Item[] {
  const items: Item[] = []
  let doc: string[] = []
  let first = true
  const flush = () => {
    if (doc.length && !first) {
      if (/^-+$/.test(doc.at(-1)!)) items.push({ kind: 'section', text: doc[0] })
      else items.push({ kind: 'note', text: doc.join(' ') })
    }
    if (doc.length) first = false
    doc = []
  }
  const lines = src.split('\n')
  for (let i = 0; i < lines.length; i++) {
    const ln = lines[i]
    if (ln.startsWith('#')) {
      doc.push(ln.replace(/^# ?/, ''))
      continue
    }
    const m = ln.match(/^law (\w+):/)
    if (m) {
      const body: string[] = []
      while (i + 1 < lines.length && /^\s+\S/.test(lines[i + 1])) body.push(lines[++i].trim())
      const d = doc.join(' ')
      items.push({ kind: 'law', name: m[1], small: /^LAW \(small\)/.test(d), doc: d.replace(/^LAW( \(small\))?:\s*/, ''), body: body.join('\n') })
      doc = []
      first = false
      continue
    }
    if (!ln.trim()) flush()
  }
  flush()
  return items
}

const esc = (s: string) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]!)
async function fetchText(path: string): Promise<{ text: string; at: string } | null> {
  const r = await fetch(path, { cache: 'no-store' })
  if (!r.ok) return null
  const at = r.headers.get('Last-Modified')
  return { text: await r.text(), at: at ? new Date(at).toLocaleTimeString() : '' }
}

async function side() {
  const [laws, proofs, built, score] = await Promise.all(['frame/LAWS.bend', 'frame/out/proofs.txt', 'frame/out/view.txt', 'frame/out/score.txt'].map(fetchText))
  const all = proofs ? /ALL PROOFS CHECK/.test(proofs.text) : null
  const failed = proofs?.text.match(/Location: Laws\.(\w+)/)?.[1]
  const error = proofs && !all ? proofs.text.replace(/\n?Use --verdict.*/, '').trim() : ''
  let html = `<div class="hd">Laws <span>LAWS.bend, proved by PROOF.bend</span></div>`
  html += `<p class="${all ? 'g' : all === false ? 'bad' : ''}">${all === null ? 'not checked yet: run frame/check.sh' : all ? `every law holds (checked ${proofs!.at})` : `a proof fails (checked ${proofs!.at})`}</p>`
  for (const it of laws ? parseLaws(laws.text) : []) {
    if (it.kind === 'section') html += `<div class="sec">${esc(it.text)}</div>`
    else if (it.kind === 'note') html += `<p class="note">${esc(it.text)}</p>`
    else {
      const mark = all ? '✓' : failed === it.name ? '✗' : '?'
      const tip = all ? (it.small ? 'holds on every case listed' : 'proved') : failed === it.name ? 'fails' : all === false ? 'unknown: Bend reports only the first failing proof' : 'not checked'
      html += `<div class="law ${failed === it.name ? 'fail' : all ? 'ok' : ''}"><div class="top"><b>${it.name}</b>${it.small ? '<span class="tag">small</span>' : ''}<span class="mark" title="${tip}">${mark}</span></div><div>${esc(it.doc)}</div><pre>${esc(it.body)}</pre>${failed === it.name ? `<pre class="err">${esc(error)}</pre>` : ''}</div>`
    }
  }
  if (all === false && !failed) html += `<pre class="err">${esc(error)}</pre>`
  if (built && built.text.trim()) html += `<div class="sec">Build</div><p class="bad">view.bend did not build (${built.at}); the page shows the last build that did</p><pre class="err">${esc(built.text.trim())}</pre>`
  if (score) html += `<div class="sec">Score <span>score.bend, ${score.at}</span></div><pre>${esc(score.text.trim())}</pre>`
  $('laws').innerHTML = html
}

// ---- reloading on each check ----
let stamp = ''
const modified = async (path: string) => (await fetch(path, { method: 'HEAD', cache: 'no-store' })).headers.get('Last-Modified')
const ui = modified('frame.js')
async function poll() {
  try {
    const r = await fetch('frame/out/stamp', { cache: 'no-store' })
    const s = r.ok ? (await r.text()).trim() : ''
    if (!s) $('err').textContent = 'no build yet: run research/signal-fabric/frame/check.sh'
    else if (s !== stamp) {
      const first = !stamp
      if (!first && (await modified('frame.js')) !== (await ui)) return location.reload()
      stamp = s
      view = (await import(/* @vite-ignore */ new URL(`frame/out/view.mjs?v=${encodeURIComponent(s)}`, location.href).href)).default as View
      await side()
      rerun(!first)
      $('built').textContent = `built ${new Date(s).toLocaleTimeString()}`
      if (!first) flash()
    }
  } catch (e) {
    $('err').textContent = String((e as Error).message ?? e)
  }
  setTimeout(poll, 1500)
}
function flash() {
  const b = $('built')
  b.classList.add('new')
  setTimeout(() => b.classList.remove('new'), 1500)
}

// ---- controls ----
let playing = false, last = 0
function play(on = !playing) {
  playing = on && !!run
  $('play').textContent = playing ? '❚❚' : '▶'
  if (playing && t >= run!.frames.length - 1) t = 0
  last = performance.now()
  if (playing) requestAnimationFrame(frame)
}
function frame(now: number) {
  if (!playing || !run) return
  const rate = Number($<HTMLSelectElement>('speed').value)
  if (now - last >= 1000 / rate) {
    last = now
    if (t >= run.frames.length - 1) return play(false)
    t++
    show()
  }
  requestAnimationFrame(frame)
}
const go = (to: number) => {
  if (!run) return
  t = Math.max(0, Math.min(run.frames.length - 1, to))
  show()
}

const sel = $<HTMLSelectElement>('preset')
PRESETS.forEach(([p, v, name], i) => sel.add(new Option(name ?? `${p}  on  ${v}`, String(i))))
sel.onchange = () => {
  const [p, v] = PRESETS[Number(sel.value)]
  $<HTMLInputElement>('prog').value = p
  $<HTMLInputElement>('tree').value = v
  rerun()
}
const params = new URLSearchParams(location.search)
$<HTMLInputElement>('prog').value = params.get('p') ?? PRESETS[0][0]
$<HTMLInputElement>('tree').value = params.get('v') ?? PRESETS[0][1]
for (const id of ['prog', 'tree']) $(id).addEventListener('keydown', (e) => e.key === 'Enter' && rerun())
$('run').onclick = () => rerun()
$('play').onclick = () => play()
$('back').onclick = () => go(t - 1)
$('fwd').onclick = () => go(t + 1)
$('start').onclick = () => go(0)
$('end').onclick = () => go(Infinity)
$('time').oninput = () => go(Number($<HTMLInputElement>('time').value))
addEventListener('keydown', (e) => {
  if ((e.target as HTMLElement).tagName === 'INPUT' && (e.target as HTMLInputElement).type === 'text') return
  if (e.key === ' ') (play(), e.preventDefault())
  else if (e.key === 'ArrowLeft') go(t - 1)
  else if (e.key === 'ArrowRight') go(t + 1)
})
addEventListener('resize', () => draw())

// pan, zoom, and a cell's value on hover
let drag: [number, number] | null = null
cv.onmousedown = (e) => (drag = [e.clientX - ox, e.clientY - oy])
addEventListener('mouseup', () => (drag = null))
cv.ondblclick = () => run && (fit(), draw())
cv.onwheel = (e) => {
  e.preventDefault()
  const k = Math.exp(-e.deltaY * 0.0015), nz = Math.max(1, Math.min(80, z * k))
  ox = e.clientX - ((e.clientX - ox) * nz) / z
  oy = e.clientY - ((e.clientY - oy) * nz) / z
  z = nz
  draw()
}
cv.onmousemove = (e) => {
  const tip = $('tip')
  if (drag) {
    ox = e.clientX - drag[0]
    oy = e.clientY - drag[1]
    tip.style.display = 'none'
    return draw()
  }
  const x = Math.floor((e.clientX - ox) / z), y = Math.floor((e.clientY - oy) / z)
  const c = run?.frames[t][y]?.[x]
  if (!c || c.$ === 'Void') return void (tip.style.display = 'none')
  tip.textContent = `(${x}, ${y})  ${text(c)}`
  tip.style.display = 'block'
  tip.style.left = `${Math.min(e.clientX + 14, innerWidth - tip.offsetWidth - 8)}px`
  tip.style.top = `${e.clientY + 14}px`
}
cv.onmouseleave = () => ($('tip').style.display = 'none')

poll()
