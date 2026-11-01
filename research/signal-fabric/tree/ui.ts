// The player: runs the chosen program on the signal-tree machine (a first run that learns rules, then runs
// on other bits that use them), and replays a run's signals and rewrites tick by tick on a canvas, with
// the rules beside it lighting up as they fire. Bundled into ../tree.js by bundle.mts.
import { parseProg, parseVal, valText } from '../../../website/src/lib/evaluators/twoway/text.ts'
import { run as interpret, eq, registersOf, type V } from '../../../website/src/lib/evaluators/twoway/core.ts'
import '../../../website/src/lib/evaluators/twoway/library.ts'
import { Book, CHEAP, HONEST, LANES, MOVES, Net, RULE_MAX, route, type Fire, type Layout, type Learn, type Link, type Node, type Op, type Pat, type Physics, type Rule, type Signal } from './engine.ts'
import { EXAMPLES, otherBits } from './examples.ts'

const $ = <T extends HTMLElement = HTMLElement>(id: string) => document.getElementById(id) as T
const cv = $<HTMLCanvasElement>('cv')
const ctx = cv.getContext('2d')!
const S = 16
const PANEL = 350

// ---- runs ----
type Kind = 'first' | 'again' | 'other' | 'flip'
interface Run {
  kind: Kind; net: Net; from: number; to: number; input: V; want: V | null; got: V
  rewrites: number; hits: number; hops: number; steps: number; peak: number; learned: number
  /** With wires of cells, the most wires through one cell; with sizes, the largest signal in symbols. */
  lanes: number; size: number
  fires: Fire[]; learns: Learn[]; signals: Signal[]
  /** Rules in the book before the run began. */
  known: number
}
let book = new Book()
let runs: Run[] = []
let cur = 0
let prog: V | null = null
let src = ''
let input: V | null = null
let learn = true
let relax = true
const params = new URLSearchParams(location.search)
let layout: Layout = params.get('layout') === 'packed' ? 'packed' : 'rows'
/** The physics a run may use, by what the selector says. */
const PHYSICS: Record<string, { phys: Physics; label: string }> = {
  honest: { phys: HONEST, label: 'honest: rules ride with signals' },
  cell: { phys: { ...HONEST, rules: 'cell' }, label: 'honest: rules stay in their cell' },
  shared: { phys: { ...HONEST, rules: 'shared' }, label: 'honest but one shared rule book (not local)' },
  cheap: { phys: CHEAP, label: 'cheap: every shortcut (the first version)' }
}
let physName = params.get('phys') && params.get('phys')! in PHYSICS ? params.get('phys')! : 'honest'
let T = 0
let playing = true
let last = performance.now()
const run = () => runs[cur]

function peakOf(signals: Signal[]) {
  const ev: [number, number][] = []
  for (const s of signals) { ev.push([s.t0, 1]); ev.push([s.t1, -1]) }
  ev.sort((a, b) => a[0] - b[0] || a[1] - b[1])
  let n = 0, peak = 0
  for (const [, d] of ev) { n += d; peak = Math.max(peak, n) }
  return peak
}
/** Runs the program on a new net from x and adds the run; on another net the rules learned so far apply. */
function start(x: V, kind: Kind) {
  const net = Net.of(x, layout, PHYSICS[physName].phys)
  net.relax = relax
  net.book = learn ? book : null
  const known = book.all.length
  const want = interpret(prog!, x)
  net.run(prog!)
  runs.push({
    kind, net, from: 0, to: net.now, input: x, want, got: net.value(), rewrites: net.rewrites, hits: net.hits, hops: net.hops, steps: net.steps,
    peak: peakOf(net.signals), learned: book.all.length - known, fires: net.fires, learns: net.learns, signals: net.signals, known,
    lanes: net.peakLanes, size: net.peakSize
  })
}
function flip() {
  const r = run()
  if (!r || !prog) return
  const net = r.net
  const at = runs.findIndex((q) => q.net === net && q.from === r.to)
  if (at >= 0) return select(at)
  if (net.now !== r.to) return
  const before = { f: net.fires.length, l: net.learns.length, s: net.signals.length, rw: net.rewrites, h: net.hits, hops: net.hops, st: net.steps, known: book.all.length }
  try {
    net.run(prog, r.kind === 'flip')
  } catch (e) { $('err').textContent = (e as Error).message; return }
  const signals = net.signals.slice(before.s)
  runs.push({
    kind: 'flip', net, from: r.to, to: net.now, input: r.got, want: r.input, got: net.value(), rewrites: net.rewrites - before.rw, hits: net.hits - before.h,
    hops: net.hops - before.hops, steps: net.steps - before.st, peak: peakOf(signals), learned: book.all.length - before.known,
    fires: net.fires.slice(before.f), learns: net.learns.slice(before.l), signals, known: before.known,
    lanes: net.peakLanes, size: net.peakSize
  })
  select(runs.length - 1)
}
function more(kind: 'again' | 'other') {
  if (!prog || !input) return
  try {
    start(kind === 'other' ? otherBits(input) : input, kind)
  } catch (e) { $('err').textContent = (e as Error).message; return }
  select(runs.length - 1)
}
/** A new program or input, or a setting changed: forget the rules, and do a first run and one on other bits. */
function build() {
  src = $<HTMLInputElement>('prog').value.trim()
  $('err').textContent = ''
  book = new Book()
  runs = []
  try {
    prog = parseProg(src)
    input = parseVal($<HTMLInputElement>('inp').value.trim())
    start(input, 'first')
    if (learn) start(otherBits(input), 'other')
  } catch (e) {
    $('err').textContent = (e as Error).message
    if (!runs.length) { prog = null; return }
  }
  select(0)
}
function select(i: number) {
  cur = i
  const r = run()
  reset()
  T = r.from
  playing = true
  $<HTMLInputElement>('speed').value = String(Math.log10(Math.max(6, (r.to - r.from) / 22)))
  tally = null
  fit()
  timeline()
  runList()
  learnedRows(true)
}

// ---- replaying the ops up to a tick ----
let shown = { net: null as Net | null, nodes: new Map<number, Node>(), links: new Map<number, Link>(), i: 0, t: -1 }
function reset() { shown = { net: run()?.net ?? null, nodes: new Map(), links: new Map(), i: 0, t: -1 } }
function apply(o: Op) {
  const { nodes, links } = shown
  switch (o.k) {
    case 'node': nodes.set(o.n.id, { ...o.n, kids: [] }); break
    case 'drop': nodes.delete(o.id); break
    case 'kind': nodes.get(o.id)!.kind = o.kind; break
    case 'kids': nodes.get(o.id)!.kids = o.kids; break
    case 'at': { const n = nodes.get(o.id)!; n.x = o.x; n.y = o.y; break }
    case 'link': links.set(o.l.id, { ...o.l }); break
    case 'unlink': links.delete(o.id); break
    case 'path': { const l = links.get(o.id); if (l) l.path = o.cells; break }
  }
}
function seek(t: number) {
  const net = run()?.net
  if (!net) return
  if (t < shown.t || shown.net !== net) reset()
  const ops = net.ops
  while (shown.i < ops.length && ops[shown.i].t <= t) apply(ops[shown.i++])
  shown.t = t
}
function cells(l: Link): [number, number][] {
  if (l.path) return l.path
  const a = shown.nodes.get(l.from), b = shown.nodes.get(l.to)
  return a && b ? route(a.x, a.y, b.x, b.y) : []
}
/** The value at the shown tick, read off the replayed net. */
function valueNow(): V | null {
  const net = run()?.net
  if (!net) return null
  const read = (id: number): V | null => {
    const n = shown.nodes.get(id)
    if (!n) return null
    const kid = (i: number) => { const l = shown.links.get(n.kids[i]); return l ? read(l.to) : null }
    if (n.kind === 'L') return { tag: 'leaf', id: 0 } as V
    if (n.kind === 'S') { const c = kid(0); return c && ({ tag: 'stem', c, id: 0 } as V) }
    const a = kid(0), b = kid(1)
    return a && b && ({ tag: 'fork', l: a, r: b, id: 0 } as V)
  }
  const top = shown.nodes.get(net.root)?.kids[0]
  const l = top !== undefined ? shown.links.get(top) : undefined
  return l ? read(l.to) : null
}
const show = (v: V | null) => {
  if (!v) return '…'
  const regs = input && registersOf(input)
  const t = valText(v, regs && registersOf(v)?.length === regs.length ? 'regs' : 'bool')
  return t.length > 140 ? t.slice(0, 137) + '…' : t
}

// ---- rules, as pictures ----
const HOLE = ['#ff7eb6', '#4dd0e1', '#b5e853', '#ffb86c', '#c3a6ff', '#7ee0b0', '#f78c6c', '#9ecbff']
const holeColor = (h: number) => HOLE[h % HOLE.length]
const letter = (h: number) => String.fromCharCode(97 + (h % 26))
const OWN = '#ffb454'
const LEARNED = '#3fdc8f'
const NEW = '#ffd866'
/** A side of a rule as text: △ a leaf, △x a stem, (x, y) a fork, letters the holes. */
function patText(p: Pat): string {
  if ('hole' in p) return letter(p.hole)
  if (p.kind === 'L') return '△'
  if (p.kind === 'S') { const c = patText(p.kids[0]); return '△' + (c.length > 1 && !c.startsWith('(') && !c.startsWith('△') ? `(${c})` : c) }
  return `(${patText(p.kids[0])}, ${patText(p.kids[1])})`
}
const ruleText = (r: Rule, arrow = '→') => `${patText(r.lhs)} ${arrow} ${patText(r.rhs)}`
/** A side of a rule as a little tree: nodes drawn as on the lattice, holes as lettered dots, new nodes ringed. */
function side(p: Pat, dx: number, out: string[], right: boolean): { w: number; h: number } {
  const U = 11, VY = 12
  let leaves = 0, depth = 0
  type P = { x: number; y: number; p: Pat; kids: P[] }
  const place = (q: Pat, d: number): P => {
    depth = Math.max(depth, d)
    if ('hole' in q || !q.kids.length) return { x: leaves++ * U, y: d * VY, p: q, kids: [] }
    const kids = q.kids.map((k) => place(k, d + 1))
    return { x: kids.reduce((s, k) => s + k.x, 0) / kids.length, y: d * VY, p: q, kids }
  }
  const root = place(p, 0)
  const ox = dx + 7, oy = 7
  const edges = (n: P) => { for (const k of n.kids) { out.push(`<line x1="${ox + n.x}" y1="${oy + n.y}" x2="${ox + k.x}" y2="${oy + k.y}"/>`); edges(k) } }
  const nodes = (n: P) => {
    const x = ox + n.x, y = oy + n.y, q = n.p
    if ('hole' in q) out.push(`<circle cx="${x}" cy="${y}" r="4.8" fill="${holeColor(q.hole)}"/><text x="${x}" y="${y + 2.6}">${letter(q.hole)}</text>`)
    else {
      const fill = q.kind === 'L' ? '#5f86b8' : q.kind === 'S' ? '#8a97aa' : '#a9b4c4'
      out.push(`<path d="M${x},${y - 5} L${x - 4.6},${y + 3.6} L${x + 4.6},${y + 3.6} Z" fill="${fill}"${q.i === undefined && right ? ` stroke="${NEW}" stroke-width="1.3"` : ''}/>`)
      for (const h of q.kind === 'F' ? [-1.5, 1.5] : q.kind === 'S' ? [0] : []) out.push(`<circle cx="${x + h}" cy="${y + 1.4}" r="0.95" fill="#0b0e14"/>`)
    }
    n.kids.forEach(nodes)
  }
  edges(root)
  nodes(root)
  return { w: Math.max(0, leaves - 1) * U + 14, h: depth * VY + 13 }
}
const PICS = new WeakMap<Rule, string>()
/** A rule as a picture: its left side, an arrow, its right side. */
function pic(r: Rule, arrow = '→', max = 300): string {
  const k = PICS.get(r)
  if (k && arrow === '→' && max === 190) return k
  const out: string[] = []
  const l = side(r.lhs, 0, out, false)
  const ax = l.w + 4
  const rr = side(r.rhs, ax + 18, out, true)
  const w = ax + 18 + rr.w, h = Math.max(l.h, rr.h)
  out.push(`<text class="ar" x="${ax + 8}" y="${Math.min(h / 2, 14) + 3}">${arrow}</text>`)
  const s = `<svg width="${Math.min(w, max)}" height="${(h * Math.min(w, max)) / w}" viewBox="0 0 ${w} ${h}">${out.join('')}</svg>`
  if (arrow === '→' && max === 190) PICS.set(r, s)
  return s
}

// ---- the panel: what the rules are, and which fire now ----
interface Row { key: string; title: string; text: string; long: string; rules: Rule[]; el?: HTMLElement }
const OWN_ROWS: Row[] = [
  { key: 'swap', title: 'swap', text: 'two wires trade ports', long: "a fork's two wires trade ports", rules: [MOVES.swap] },
  { key: 'cross', title: 'cross', text: 'two wires trade forks', long: 'two wires trade the forks they hang from', rules: [MOVES.cross] },
  { key: 'sink', title: 'sink', text: 'a stem moves down a fork, or up', long: 'a stem moves between a fork and its left part', rules: [MOVES.sink, MOVES.rise] },
  { key: 'stem', title: 'stem', text: 'a stem put on, or (stem†) taken off', long: 'a stem is put on a part, or (stem†) taken off', rules: [MOVES.stem, MOVES['stem†']] },
  { key: 'unit', title: 'unit', text: '(△, ·) put on, or (unit†) taken off', long: 'a fork with a leaf on its left is put over a part, or (unit†) taken off', rules: [MOVES.unit, MOVES['unit†']] },
  { key: 'triS', title: 'tri on a stem', text: 'stem off, its middle part here, stem on', long: "tri found a stem on its left part: the stem comes off, tri's middle program runs here, the stem goes back", rules: [MOVES.lift, MOVES.lower] },
  { key: 'triF', title: 'tri on a fork', text: 'rotate, its last part here, rotate back', long: "tri found a fork on its left part: a rotation, tri's last program runs here, the rotation back", rules: [MOVES.rotate, MOVES.unrotate] }
]
const ownRow = new Map<Rule, Row>()
for (const r of OWN_ROWS) for (const q of r.rules) ownRow.set(q, r)
const rowOf = (r: Rule) => (r.learned ? r.name : ownRow.get(r)!.key)
const SIG_ROWS = [
  { key: 'task', color: '#7aa2f7', title: 'piece', text: 'goes a cell a tick; <b>map</b> sends one down each wire at once', long: 'a piece of the program travels down a wire, a cell a tick; map sends one down each wire at once, so many run at the same time' },
  { key: 'probe', color: '#8595ad', title: 'read', text: 'to a node below and back, to see its shape', long: 'a signal to a node below and back, to see its shape: tri reading its left part, a move reading two levels, or a node checking which learned rule fits' },
  { key: 'done', color: '#3fbf7f', title: 'done', text: 'back up when a piece is finished', long: 'a finished piece signals back up; the node that sent it waits for this' }
]
function panel() {
  $('sigrows').innerHTML = SIG_ROWS.map((r) => `<div class="rule sig" id="sig-${r.key}" title="${r.long}"><div class="top"><span class="dot" style="background:${r.color}"></span><b>${r.title}</b><span class="txt">${r.text}</span><span class="n"></span></div></div>`).join('')
  $('ownrows').innerHTML = OWN_ROWS.map((r) => `<div class="rule own" data-row="${r.key}" title="${r.long}"><div class="top"><b>${r.title}</b><span class="n"></span></div><div class="pic">${pic(r.rules[0], '⇄', 140)}</div><div class="txt">${r.text}</div></div>`).join('')
  for (const r of OWN_ROWS) hookRow((r.el = document.querySelector(`[data-row="${r.key}"]`) as HTMLElement))
  $('cap').textContent = String(RULE_MAX)
}

/** Counts up to the shown tick of the run shown: rules fired, rules learned, signals sent. */
interface Tally { t: number; fi: number; li: number; count: Map<string, number>; lastFire: Map<string, number>; lastRule: Map<string, Rule>; born: Map<string, number>; rules: Map<string, number>; sent: Record<string, number> }
let tally: Tally | null = null
function tallyTo(t: number) {
  const r = run()
  if (!tally || t < tally.t) {
    tally = { t: -1, fi: 0, li: 0, count: new Map(), lastFire: new Map(), lastRule: new Map(), born: new Map(), rules: new Map(), sent: { task: 0, probe: 0, done: 0 } }
    for (const q of book.all.slice(0, r.known)) { tally.rules.set(q.name, (tally.rules.get(q.name) ?? 0) + 1); tally.lastRule.set(q.name, tally.lastRule.get(q.name) ?? q); tally.born.set(q.name, -Infinity) }
  }
  const y = tally
  while (y.fi < r.fires.length && r.fires[y.fi].t <= t) {
    const f = r.fires[y.fi++], k = rowOf(f.rule)
    y.count.set(k, (y.count.get(k) ?? 0) + 1)
    y.lastFire.set(k, f.t)
    if (f.rule.learned) y.lastRule.set(k, f.rule)
  }
  while (y.li < r.learns.length && r.learns[y.li].t <= t) {
    const l = r.learns[y.li++], k = l.rule.name
    y.rules.set(k, (y.rules.get(k) ?? 0) + 1)
    y.lastRule.set(k, l.rule)
    y.born.set(k, l.t)
  }
  y.sent = { task: 0, probe: 0, done: 0 }
  for (const s of r.signals) if (s.t0 <= t && (s.label !== '' || s.kind !== 'probe')) y.sent[s.kind]++
  y.t = t
}
let orderAt = 0
let order: string[] = []
let showAll = false
/** The learned rules, by the piece they were learned for: the latest active first. */
function learnedRows(force = false) {
  const y = tally
  if (!y) return
  const now = performance.now()
  const names = [...y.rules.keys()]
  if (force || now - orderAt > 400 || names.length !== order.length) {
    orderAt = now
    const act = (k: string) => Math.max(y.lastFire.get(k) ?? -Infinity, y.born.get(k) ?? -Infinity)
    order = names.sort((a, b) => act(b) - act(a) || a.localeCompare(b))
    const list = showAll ? order : order.slice(0, 12)
    $('learnrows').innerHTML = list.map((k) => {
      const r = y.lastRule.get(k)!
      return `<div class="rule learned" data-row="${esc(k)}" title="${esc(rulesOf(k))}"><div class="top"><b>${esc(k)}</b><span class="n"></span></div><div class="body"><div class="pic">${pic(r, '→', 190)}</div><div class="txt"></div></div></div>`
    }).join('') + (order.length > 12 ? `<div class="more" id="more">${showAll ? 'show the latest 12' : `and ${order.length - 12} more`}</div>` : '')
    const m = document.getElementById('more')
    if (m) m.onclick = () => { showAll = !showAll; learnedRows(true) }
    for (const el of document.querySelectorAll<HTMLElement>('#learnrows .rule')) hookRow(el)
  }
  $('lcount').textContent = names.length ? `${plural([...y.rules.values()].reduce((s, n) => s + n, 0), 'rule')} for ${plural(names.length, 'piece')}` : learn ? 'none yet' : 'off'
  for (const el of document.querySelectorAll<HTMLElement>('#learnrows .rule')) {
    const k = el.dataset.row!
    const r = y.lastRule.get(k)!
    const n = y.count.get(k) ?? 0
    el.querySelector('.n')!.textContent = n ? `×${n}` : ''
    const fired = T - (y.lastFire.get(k) ?? -Infinity)
    const born = T - (y.born.get(k) ?? -Infinity)
    el.classList.toggle('lit', fired < 3)
    el.classList.toggle('new', born < 8)
    const pc = el.querySelector('.pic')!
    if (pc.getAttribute('data-r') !== String(book.all.indexOf(r))) { pc.innerHTML = pic(r, '→', 190); pc.setAttribute('data-r', String(book.all.indexOf(r))) }
    el.querySelector('.txt')!.textContent = `${born < 8 ? 'just learned · ' : ''}${plural(y.rules.get(k)!, 'rule')}; this one stands for ${r.moves} moves`
  }
}
const rulesOf = (k: string) => book.all.filter((r) => r.name === k).map((r) => `${ruleText(r)}   (${r.moves} moves)`).join('\n')
function panelTick() {
  const y = tally!
  for (const r of OWN_ROWS) {
    const n = y.count.get(r.key) ?? 0
    r.el!.querySelector('.n')!.textContent = n ? `×${n}` : ''
    r.el!.classList.toggle('lit', T - (y.lastFire.get(r.key) ?? -Infinity) < 2)
  }
  const flying = { task: 0, probe: 0, done: 0 }
  for (const s of run().signals) if (T >= s.t0 && T < s.t1) flying[s.kind]++
  for (const r of SIG_ROWS) {
    const el = $(`sig-${r.key}`)
    el.querySelector('.n')!.textContent = y.sent[r.key] ? `×${y.sent[r.key]}` : ''
    el.classList.toggle('lit', flying[r.key as keyof typeof flying] > 0)
  }
}
let hover: string | null = null
function hookRow(el: HTMLElement) {
  el.onmouseenter = () => (hover = el.dataset.row ?? null)
  el.onmouseleave = () => (hover = null)
  el.onclick = () => {
    // to this rule's next firing in the run
    const k = el.dataset.row
    const fs = run().fires.filter((f) => rowOf(f.rule) === k)
    const next = fs.find((f) => f.t > Math.floor(T)) ?? fs[0]
    if (next) { T = next.t + 0.5; playing = false }
  }
}

// ---- view ----
let scale = 1, ox = 0, oy = 0
function fit() {
  const net = run()?.net
  if (!net) return
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
  for (const o of net.ops) {
    const p = o.k === 'node' ? o.n : o.k === 'at' ? o : null
    if (p) { x0 = Math.min(x0, p.x); y0 = Math.min(y0, p.y); x1 = Math.max(x1, p.x); y1 = Math.max(y1, p.y) }
  }
  const W = cv.clientWidth - PANEL, H = cv.clientHeight
  const top = 150, bottom = 175
  const w = (x1 - x0 + 3) * S, h = (y1 - y0 + 3) * S
  scale = Math.min((W - 30) / w, (H - top - bottom) / h, 3)
  ox = 15 + (W - 30 - w * scale) / 2 - (x0 - 1.5) * S * scale
  oy = top + (H - top - bottom - h * scale) / 2 - (y0 - 1.5) * S * scale
}

// ---- drawing ----
const COLOR = { spread: '#7aa2f7', tri: '#b39cff', move: '#ffd866', done: '#3fbf7f', probe: '#8595ad' }
const signalColor = (kind: string, label: string) =>
  kind === 'done' ? COLOR.done : kind === 'probe' ? COLOR.probe : ['map', 'fix', 'self', 'seq', 'dag'].includes(label) ? COLOR.spread : label === 'tri' ? COLOR.tri : COLOR.move
/** The first index in a list sorted by t whose t is past `t`. */
function after<E extends { t: number }>(xs: E[], t: number) {
  let lo = 0, hi = xs.length
  while (lo < hi) { const m = (lo + hi) >> 1; if (xs[m].t <= t) lo = m + 1; else hi = m }
  return lo
}
function draw() {
  const now = performance.now()
  const dt = (now - last) / 1000
  last = now
  const r = run()
  if (playing && r) {
    T = Math.min(r.to, T + dt * 10 ** +$<HTMLInputElement>('speed').value)
    if (T >= r.to) { playing = false; nudge() }
  }
  const dpr = window.devicePixelRatio || 1
  const W = cv.clientWidth, H = cv.clientHeight
  if (cv.width !== W * dpr || cv.height !== H * dpr) { cv.width = W * dpr; cv.height = H * dpr }
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  ctx.fillStyle = '#0b0e14'
  ctx.fillRect(0, 0, W, H)
  if (r) {
    seek(Math.floor(T))
    tallyTo(Math.floor(T))
    ctx.setTransform(dpr * scale, 0, 0, dpr * scale, dpr * ox, dpr * oy)
    const c = (x: number) => (x + 0.5) * S
    // cells: a faint dot each, over the region in use
    ctx.fillStyle = '#151d2b'
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
    for (const n of shown.nodes.values()) { x0 = Math.min(x0, n.x); y0 = Math.min(y0, n.y); x1 = Math.max(x1, n.x); y1 = Math.max(y1, n.y) }
    for (let y = y0 - 1; y <= y1 + 1; y++) for (let x = x0 - 1; x <= x1 + 1; x++) ctx.fillRect(c(x) - 1, c(y) - 1, 2, 2)
    // wires
    ctx.lineCap = 'round'
    ctx.lineJoin = 'round'
    ctx.strokeStyle = '#34465f'
    ctx.lineWidth = 1.6
    for (const l of shown.links.values()) {
      const p = cells(l)
      if (p.length < 2) continue
      ctx.beginPath()
      ctx.moveTo(c(p[0][0]), c(p[0][1]))
      for (const [x, y] of p.slice(1)) ctx.lineTo(c(x), c(y))
      ctx.stroke()
    }
    // a hovered rule: everywhere it fires in this run
    if (hover) {
      ctx.strokeStyle = '#e6edf3'
      ctx.lineWidth = 1
      for (const f of r.fires) if (rowOf(f.rule) === hover) { ctx.beginPath(); ctx.rect(c(f.x) - S * 0.55, c(f.y) - S * 0.55, S * 1.1, S * 1.1); ctx.stroke() }
    }
    // rewrites in the last few ticks: a ring, in amber for the machine's own rules and green for learned ones
    const t = Math.floor(T)
    const recent = r.fires.slice(after(r.fires, T - 3), after(r.fires, T))
    for (const f of recent) {
      const age = T - f.t
      ctx.strokeStyle = (f.rule.learned ? LEARNED : OWN) + alpha(1 - age / 3)
      ctx.lineWidth = f.rule.learned ? 2.2 : 1.5
      ctx.beginPath(); ctx.arc(c(f.x), c(f.y), S * (0.5 + age * 0.1), 0, 7); ctx.stroke()
    }
    // nodes; a stem on a leaf is a T bit, drawn gold, and a lone leaf blue
    const stemAbove = new Set<number>()
    for (const l of shown.links.values()) if (shown.nodes.get(l.from)?.kind === 'S' && shown.nodes.get(l.to)?.kind === 'L') stemAbove.add(l.to)
    for (const n of shown.nodes.values()) {
      const x = c(n.x), y = c(n.y)
      if (n.kind === 'R') {
        ctx.strokeStyle = '#c8d3e0'; ctx.lineWidth = 1.5
        ctx.beginPath(); ctx.arc(x, y, S * 0.32, 0, 7); ctx.stroke()
        ctx.fillStyle = '#c8d3e0'; ctx.beginPath(); ctx.arc(x, y, S * 0.1, 0, 7); ctx.fill()
        continue
      }
      const onLeaf = n.kind === 'S' && shown.nodes.get(shown.links.get(n.kids[0])?.to ?? -1)?.kind === 'L'
      const underStem = n.kind === 'L' && stemAbove.has(n.id)
      ctx.fillStyle = onLeaf || underStem ? '#d9b44a' : n.kind === 'L' ? '#5f86b8' : n.kind === 'S' ? '#8a97aa' : '#a9b4c4'
      ctx.beginPath()
      ctx.moveTo(x, y - S * 0.36); ctx.lineTo(x - S * 0.34, y + S * 0.28); ctx.lineTo(x + S * 0.34, y + S * 0.28); ctx.closePath()
      ctx.fill()
      ctx.fillStyle = '#0b0e14'
      const holes = n.kind === 'F' ? [-0.13, 0.13] : n.kind === 'S' ? [0] : []
      for (const h of holes) { ctx.beginPath(); ctx.arc(x + h * S, y + S * 0.12, S * 0.075, 0, 7); ctx.fill() }
    }
    // the parts a recent rule passed along, lettered as in its picture, where they are now
    const big = scale * S > 9
    if (big) {
      ctx.font = `bold ${S * 0.38}px ui-monospace, monospace`
      ctx.textAlign = 'center'
      for (const f of recent.slice(-24)) {
        if (T - f.t > 2.5) continue
        f.holes.forEach((id, h) => {
          const n = shown.nodes.get(id)
          if (!n) return
          const x = c(n.x) + S * 0.36, y = c(n.y) - S * 0.36
          ctx.fillStyle = holeColor(h)
          ctx.beginPath(); ctx.arc(x, y, S * 0.24, 0, 7); ctx.fill()
          ctx.fillStyle = '#0b0e14'
          ctx.fillText(letter(h), x, y + S * 0.13)
        })
      }
    }
    // signals in flight
    let flying = 0
    ctx.font = `${S * 0.42}px ui-monospace, monospace`
    ctx.textAlign = 'left'
    for (const s of r.signals) {
      if (T < s.t0 || T >= s.t1) continue
      flying++
      const f = ((T - s.t0) / (s.t1 - s.t0)) * (s.cells.length - 1)
      const i = Math.min(s.cells.length - 2, Math.floor(f)), q = f - i
      const [ax, ay] = s.cells[i], [bx, by] = s.cells[i + 1]
      const x = c(ax + (bx - ax) * q), y = c(ay + (by - ay) * q)
      const col = signalColor(s.kind, s.label)
      const g = ctx.createRadialGradient(x, y, 0, x, y, S * 0.6)
      g.addColorStop(0, col + 'aa'); g.addColorStop(1, col + '00')
      ctx.fillStyle = g
      ctx.fillRect(x - S, y - S, 2 * S, 2 * S)
      ctx.fillStyle = col
      ctx.beginPath(); ctx.arc(x, y, S * (s.kind === 'task' ? 0.2 : 0.13), 0, 7); ctx.fill()
      if (s.kind === 'task' && scale * S > 13) { ctx.fillStyle = col; ctx.fillText(s.label, x + S * 0.3, y - S * 0.25) }
    }
    // names of the rules firing now, and of rules just learned, rising from where they were learned
    if (big) {
      ctx.textAlign = 'center'
      ctx.font = `${S * 0.44}px ui-monospace, monospace`
      for (const f of recent.slice(-16)) {
        if (T - f.t > 2) continue
        ctx.fillStyle = f.rule.learned ? LEARNED : OWN
        ctx.fillText(f.rule.name, c(f.x), c(f.y) - S * 0.75)
      }
      for (const l of r.learns.slice(after(r.learns, T - 8), after(r.learns, T))) {
        const age = T - l.t
        ctx.fillStyle = NEW + alpha(1 - age / 8)
        ctx.fillText(`+ rule: ${l.rule.name}`, c(l.x), c(l.y) - S * (0.9 + age * 0.12))
      }
    }
    ctx.textAlign = 'left'
    hud(flying, t)
    learnedRows()
  }
  requestAnimationFrame(draw)
}
const alpha = (a: number) => Math.round(Math.max(0, Math.min(1, a)) * 255).toString(16).padStart(2, '0')

// ---- readouts ----
let hudKey = ''
function hud(flying: number, t: number) {
  const r = run()
  const key = `${t}|${flying}|${cur}|${runs.length}`
  if (key === hudKey) return
  hudKey = key
  const done = t >= r.to
  $('title').innerHTML = `${esc(src)}${r.kind === 'flip' ? '†' : ''} <span class="dim">on</span> ${esc(show(r.input))} <span class="dim">→</span> ${esc(done ? show(r.got) : '…')}`
  const vd = $('verdict')
  if (done) {
    const ok = !!r.want && eq(r.want, r.got)
    vd.className = ok ? 'ok' : 'bad'
    vd.textContent = r.kind !== 'flip' ? (ok ? '✓ the two-way interpreter gives the same' : `✗ the interpreter gives ${show(r.want)}`) : ok ? '✓ flipped back: the input again' : '✗ flipping back did not give the input'
  } else { vd.className = ''; vd.textContent = '' }
  const y = tally!
  const own = OWN_ROWS.reduce((s, q) => s + (y.count.get(q.key) ?? 0), 0)
  const learned = [...y.count].filter(([k]) => !OWN_ROWS.some((q) => q.key === k)).reduce((s, [, n]) => s + n, 0)
  $('now').innerHTML = `tick <b>${t - r.from}</b> of ${r.to - r.from} · <b>${flying}</b> signal${flying === 1 ? '' : 's'} in flight · <span class="o">${own} own rules</span> and <span class="g">${learned} learned rules</span> fired · now: ${esc(show(valueNow()))}`
  story(t)
  panelTick()
  cursor()
}
const KIND: Record<Kind, string> = { first: 'first run, learning', again: 'the same bits again', other: 'other bits', flip: 'flipped back' }
function runList() {
  const most = Math.max(...runs.map((q) => q.to - q.from))
  $('runs').innerHTML = runs.map((q, i) => {
    const w = Math.max(2, ((q.to - q.from) / most) * 150)
    return `<div class="run${i === cur ? ' cur' : ''}" data-i="${i}"><span class="bar" style="width:${w}px"></span><b>${(q.to - q.from).toLocaleString('en')}</b> ticks · ${i + 1}. ${q.kind === 'first' && !q.net.book ? 'first run' : KIND[q.kind]}: <span class="o">${q.rewrites.toLocaleString('en')}</span> + <span class="g">${q.hits.toLocaleString('en')}</span> rewrites${q.learned ? `, <span class="y">${plural(q.learned, 'rule')} learned</span>` : ''}${q.size ? ` · largest signal ${plural(q.size, 'symbol')}` : ''}${q.lanes ? ` · <span class="${q.lanes > LANES ? 'bad' : ''}" title="the most wires through one cell; a node's step keeps to ${LANES}, but a rewrite's splices do not">${q.lanes} wires in a cell</span>` : ''}</div>`
  }).join('')
  for (const el of document.querySelectorAll<HTMLElement>('#runs .run')) el.onclick = () => select(+el.dataset.i!)
}
/** A line saying what just happened: a rule learned, a learned rule firing, or one of the machine's own. */
function story(t: number) {
  const r = run()
  const l = r.learns[after(r.learns, t) - 1]
  const fs = r.fires.slice(after(r.fires, t - 3), after(r.fires, t))
  const lf = fs.filter((f) => f.rule.learned).pop()
  const of = fs.pop()
  const el = $('story')
  if (l && t - l.t < 6) el.innerHTML = `<span class="y">learned a rule for <b>${esc(l.rule.name)}</b></span>: the nodes it read, ${esc(patText(l.rule.lhs))}, become ${esc(patText(l.rule.rhs))}. It took ${l.rule.moves} moves and ${l.rule.ticks} ticks; next time it fits, one rewrite`
  else if (lf) el.innerHTML = `<span class="g"><b>${esc(lf.rule.name)}</b> by a learned rule</span>: ${esc(ruleText(lf.rule))}, one rewrite for ${lf.rule.moves} moves`
  else if (of) el.innerHTML = `<span class="o"><b>${esc(of.rule.name)}</b></span>: ${esc(ruleText(of.rule))}`
  else if (t >= r.to) el.innerHTML = r.kind === 'first' && learn ? `done. <b>other bits ▶▶</b> runs it again with the rules it learned` : 'done'
  else el.innerHTML = '&nbsp;'
}
function nudge() { $('other').classList.toggle('pulse', run()?.kind === 'first' && learn) }
const plural = (n: number, w: string) => `${n.toLocaleString('en')} ${w}${n === 1 ? '' : 's'}`
const esc = (s: string) => s.replace(/[&<>"]/g, (ch) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[ch]!)

// the timeline: per tick, the machine's own rules (amber, up) and learned ones (green), signals in flight
// (blue line), and rules learned (gold marks)
let tlImg: ImageData | null = null
function timeline() {
  const c = $<HTMLCanvasElement>('tl')
  const dpr = window.devicePixelRatio || 1
  c.width = c.clientWidth * dpr
  c.height = c.clientHeight * dpr
  const g = c.getContext('2d')!
  g.setTransform(dpr, 0, 0, dpr, 0, 0)
  const W = c.clientWidth, H = c.clientHeight
  g.clearRect(0, 0, W, H)
  const r = run()
  if (!r) return
  const n = Math.max(1, Math.floor(W)), span = Math.max(1, r.to - r.from)
  const col = (t: number) => Math.min(n - 1, Math.floor(((t - r.from) / span) * n))
  const own = new Float32Array(n), lrn = new Float32Array(n), fly = new Float32Array(n)
  for (const f of r.fires) (f.rule.learned ? lrn : own)[col(f.t)]++
  for (const s of r.signals) {
    const a = col(s.t0), b = col(s.t1)
    for (let i = a; i <= b; i++) fly[i] += Math.min(1, (s.t1 - s.t0) / (span / n))
  }
  const top = Math.max(1, ...own.map((v, i) => v + lrn[i])), ftop = Math.max(1, ...fly)
  const base = H - 4
  for (let i = 0; i < n; i++) {
    const ho = (own[i] / top) * (H - 10), hl = (lrn[i] / top) * (H - 10)
    g.fillStyle = OWN; g.fillRect(i, base - ho, 1, ho)
    g.fillStyle = LEARNED; g.fillRect(i, base - ho - hl, 1, hl)
  }
  g.strokeStyle = '#7aa2f7aa'; g.lineWidth = 1
  g.beginPath()
  for (let i = 0; i < n; i++) { const y = base - (fly[i] / ftop) * (H - 10); if (i) g.lineTo(i, y); else g.moveTo(i, y) }
  g.stroke()
  g.fillStyle = NEW
  for (const l of r.learns) g.fillRect(col(l.t), 0, 1, 4)
  g.fillStyle = '#5b6a80'; g.fillRect(0, base, W, 1)
  tlImg = g.getImageData(0, 0, c.width, c.height)
  $('tlnote').innerHTML = `per tick: <span class="o">own rules</span>, <span class="g">learned rules</span>, <span class="b">signals in flight</span> (up to ${Math.round(ftop)}), <span class="y">rules learned</span>`
}
function cursor() {
  const c = $<HTMLCanvasElement>('tl')
  const g = c.getContext('2d')!
  const r = run()
  if (tlImg) g.putImageData(tlImg, 0, 0)
  const dpr = window.devicePixelRatio || 1
  g.setTransform(dpr, 0, 0, dpr, 0, 0)
  g.fillStyle = '#e6edf3'
  g.fillRect(((T - r.from) / Math.max(1, r.to - r.from)) * c.clientWidth, 0, 1.5, c.clientHeight)
  const tm = $<HTMLInputElement>('time')
  tm.min = String(r.from)
  tm.max = String(r.to)
  if (document.activeElement !== tm) tm.value = String(Math.floor(T))
}

// ---- controls ----
const exSel = $<HTMLSelectElement>('ex')
for (const e of EXAMPLES) exSel.add(new Option(e.name, e.name))
function pick(name: string) {
  const e = EXAMPLES.find((x) => x.name === name)!
  $<HTMLInputElement>('prog').value = e.prog
  $<HTMLInputElement>('inp').value = e.input
  exSel.title = e.tip
  build()
}
exSel.onchange = () => pick(exSel.value)
for (const id of ['prog', 'inp']) $(id).addEventListener('keydown', (e) => { if ((e as KeyboardEvent).key === 'Enter') build() })
const playBtn = $('play')
playBtn.onclick = () => { if (T >= run().to) T = run().from; playing = !playing }
$('flip').onclick = flip
$('other').onclick = () => { $('other').classList.remove('pulse'); more('other') }
$('again').onclick = () => more('again')
$('forget').onclick = build
const toggle = (id: string, on: boolean, label: string) => { $(id).classList.toggle('on', on); $(id).textContent = `${label} ${on ? '✓' : '✗'}` }
$('learn').onclick = () => { learn = !learn; toggle('learn', learn, 'learn rules'); build() }
$('climb').onclick = () => { relax = !relax; toggle('climb', relax, 'matter moves'); build() }
const layoutLabel = () => { $('layout').textContent = layout === 'rows' ? 'layout: rows' : 'layout: packed'; $('layout').classList.toggle('on', layout === 'packed') }
$('layout').onclick = () => { layout = layout === 'rows' ? 'packed' : 'rows'; layoutLabel(); build() }
layoutLabel()
const physSel = $<HTMLSelectElement>('phys')
for (const [k, v] of Object.entries(PHYSICS)) physSel.add(new Option(v.label, k))
physSel.value = physName
physSel.onchange = () => { physName = physSel.value; build() }
$<HTMLInputElement>('time').oninput = (e) => { T = +(e.target as HTMLInputElement).value; playing = false }
const tl = $<HTMLCanvasElement>('tl')
const scrub = (e: MouseEvent) => { const r = run(); T = r.from + (e.offsetX / tl.clientWidth) * (r.to - r.from); playing = false }
tl.addEventListener('mousedown', (e) => { scrub(e); tl.onmousemove = (m) => { if (m.buttons) scrub(m) } })
const speedLabel = () => ($('speedv').textContent = `${Math.round(10 ** +$<HTMLInputElement>('speed').value)} ticks/s`)
$('speed').oninput = speedLabel
window.addEventListener('keydown', (e) => {
  if ((e.target as HTMLElement).tagName === 'INPUT' && (e.target as HTMLInputElement).type !== 'range') return
  if (e.key === ' ') { e.preventDefault(); playBtn.click() }
  else if (e.key === 'f' || e.key === 'F') flip()
  else if (e.key === 'n' || e.key === 'N') $('other').click()
  else if (e.key === 'ArrowRight') { T = Math.min(run().to, Math.floor(T) + 1); playing = false }
  else if (e.key === 'ArrowLeft') { T = Math.max(run().from, Math.floor(T) - 1); playing = false }
})
setInterval(() => { playBtn.textContent = playing ? '⏸' : '▶'; speedLabel() }, 200)

let drag: { x: number; y: number; ox: number; oy: number } | null = null
cv.addEventListener('mousedown', (e) => { drag = { x: e.clientX, y: e.clientY, ox, oy } })
window.addEventListener('mouseup', () => (drag = null))
cv.addEventListener('mousemove', (e) => {
  if (drag) { ox = drag.ox + e.clientX - drag.x; oy = drag.oy + e.clientY - drag.y }
  tip(e)
})
cv.addEventListener('wheel', (e) => {
  e.preventDefault()
  const f = Math.exp(-e.deltaY * 0.0015)
  ox = e.offsetX - (e.offsetX - ox) * f
  oy = e.offsetY - (e.offsetY - oy) * f
  scale *= f
}, { passive: false })
window.addEventListener('resize', () => { fit(); timeline() })
function tip(e: MouseEvent) {
  const el = $('tip')
  const wx = (e.offsetX - ox) / scale / S - 0.5, wy = (e.offsetY - oy) / scale / S - 0.5
  const here = [...shown.nodes.values()].filter((n) => Math.abs(n.x - wx) < 0.5 && Math.abs(n.y - wy) < 0.5)
  if (!here.length || drag) { el.style.display = 'none'; return }
  const name = { L: 'leaf △', S: 'stem', F: 'fork', R: 'root: where the program comes in' }
  el.textContent = here.map((n) => `${name[n.kind]} #${n.id} at (${n.x}, ${n.y})${n.kids.length ? `, wires ${n.kids.map((k) => { const l = shown.links.get(k); return l ? cells(l).length - 1 : '?' }).join(' and ')} cells long` : ''}`).join('\n')
  el.style.display = 'block'
  el.style.left = `${e.clientX + 14}px`
  el.style.top = `${e.clientY + 14}px`
}

panel()
const first = params.get('ex') ?? 'addw 4'
exSel.value = first
pick(first)
requestAnimationFrame(draw)
